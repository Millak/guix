;;; GNU Guix --- Functional package management for GNU
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; This manifest returns build artfacts for all supported systems.  This can be
;;; controlled by SUPPORTED_SYSTEMS environment variable.  For the list of
;;; artifacts produced, see artifacts-for-system and the `<thing>-for-system?`
;;; procedures.  NOTE: the --system argument does not change the system for which
;;; the resulting package is built. They return different definitions of the
;;; images. To change the system, pass different SUPPORTED_SYSTEMS.

(use-modules (gnu compression)
             (gnu image)
             (gnu packages graphviz)
             (gnu packages imagemagick)
             (gnu packages package-management)
             (gnu packages perl)
             (gnu services)
             (gnu system image)
             (gnu system install)
             (gnu system)
             (guix build-system gnu)
             (guix build-system trivial)
             (guix channels)
             (guix gexp)
             (guix git)
             (guix grafts)
             (guix memoization)
             (guix monads)
             (guix packages)
             (guix profiles)
             (guix records)
             (guix scripts pack)
             (guix store)
             (guix ui)
             (guix utils)
             (ice-9 format)
             (ice-9 match)
             (srfi srfi-9)
             (srfi srfi-26)
             (srfi srfi-35))

;; For easier testing, use (snapshot) guix package from (gnu packages
;; package-management). Otherwise, the package is updated to current commit and
;; might not be substitutable, leading to longer build times.
(define %use-snapshot-package?
  (string=? (or (getenv "GUIX_USE_SNAPSHOT_PACKAGE") "no") "yes"))

(define (%guix-version)
  ;; NOTE: while package-version guix is not correct in general,
  ;; it is correct for the release itself. At that time, the
  ;; guix package is updated to vX.Y.Z and it's the version
  ;; we want to use.
  (package-version guix))

(define (%vm-image-path)
  (search-path %load-path "gnu/system/examples/vm-image.tmpl"))

(define (%vm-image-efi-path)
  (search-path %load-path "gnu/system/examples/vm-image-efi.tmpl"))

;; monadic record and gexp-compiler
;; taken from Inria
;; https://gitlab.inria.fr/numpex-pc5/wp3/guix-images/-/blob/17bf4585abc2d637faa5d339436e778b7c9fb1ce/modules/guix-hpc/packs.scm

;; XXX: The <monadic> hack below will hopefully become unnecessary once the
;; (guix scripts pack) interface switches to declarative style--i.e.,
;; file-like objects.

(define-record-type <monadic>
  (monadic->declarative mvalue)
  monadic?
  (mvalue monadic-value))

(define-gexp-compiler (monadic-compiler (monadic <monadic>) system target)
  (monadic-value monadic))

;; The tarball should be the same for every system.
;; Still, we need to decide what system to build it
;; for, so use the one that CI has most resources for.
(define (source-tarball-for-system? system)
  (member system
        '("x86_64-linux")))

(define (iso-for-system? system)
  (member system
        '("x86_64-linux" "i686-linux" "aarch64-linux")))

(define (qcow2-for-system? system)
  (member system
        '("x86_64-linux" "aarch64-linux")))

(define* (qcow2-gpt-for-system? system)
  (string=? system "aarch64-linux"))

(define (copy-/etc/config.scm config)
  "Copy the configuration.scm of the operating system to /etc/config.scm, for
user's convenience. The file has to be writable, not a link to the store, so
etc-service-type can't be used here. CONFIG is a pair of strings, (FROM . TO).
The config will be copied from FROM to TO."
  (match config
    ((from . to)
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (when (not (file-exists? #$to))
             (copy-file #$from #$to)
             (make-file-writable #$to)))))
    (_ (raise
        (formatted-message-string
         (G_ "unexpected config parameter, should be pair of strings: ~a"
             config))))))

(define copy-/etc/config.scm-service-type
  (service-type (name 'copy-/etc/config.scm)
                (description
                 "Copy the system configuration file to /etc/config.scm.")
                (extensions (list (service-extension activation-service-type
                                                     copy-/etc/config.scm)))
                (default-value (cons "/run/current-system/configuration.scm"
                                     "/etc/config.scm"))))

(define (operating-system-with-/etc/config.scm os)
  "Copy the system configuration file to writable /etc/config.scm on first boot."
  (operating-system
    (inherit os)
    (services (cons (service copy-/etc/config.scm-service-type)
                    (operating-system-user-services os)))))

(define (simple-provenance-entry config-file)
  "Return system entries describing the operating system config, provided
through CONFIG-FILE."
  (mbegin %store-monad
    (return `(("configuration.scm"
               ,(local-file (assume-valid-file-name config-file)
                            "configuration.scm"))))))

;; This is mostly taken from provenance-service-type from (gnu services),
;; but it provides only configuration.scm, not channels.scm. This is
;; to get the same derivations for both Cuirass and local builds.
;; In the future, provenance-service-type could be adapted to support
;; this use case as well.
(define simple-provenance-service-type
  (service-type (name 'provenance)
                (extensions
                 (list (service-extension system-service-type
                                          simple-provenance-entry)))
                (default-value #f)                ;the OS config file
                (description
                 "Store configuration.scm of the system in the system
itself.")))

(define* (operating-system-with-simple-provenance
          os
          #:optional
          (config-file
           (operating-system-configuration-file
            os)))
  "Return a variant of OS that stores its CONFIG-FILE.  This is similar to
`operating-system-with-provenance`, but it does copy only the
configuration.scm."
  (operating-system
    (inherit os)
    (services (cons (service simple-provenance-service-type config-file)
                    (operating-system-user-services os)))))

(define (guix-package-commit guix)
  ;; Extract the commit of the GUIX package.
  (match (package-source guix)
    ((? channel? source)
     (channel-commit source))
    (_
     (apply (lambda* (#:key commit #:allow-other-keys) commit)
            (package-arguments guix)))))

;; NOTE: Normally, we would use (current-guix), along with url
;; overriden to the upstream repository to not leak our local checkout.
;; But currently, the (current-guix) derivation has to be computed through
;; QEMU for systems other than your host system. This takes a lot of time,
;; it takes at least half an hour to get the derivations.
(define (guix-package/with-commit guix commit)
  "Use the guix from (gnu packages package-management),
but override its commit to the specified version. Make sure
to also override the channel commit to have the correct
provenance."
  (let ((scm-version (car (string-split (package-version guix) #\-))))
    (package
      (inherit guix)
      (version (string-append scm-version "." (string-take commit 7)))
      (source (git-checkout
                (url (channel-url %default-guix-channel))
                (commit commit)))
      (arguments
       (substitute-keyword-arguments (package-arguments guix)
         ((#:configure-flags flags '())
          #~(cons*
             (string-append "--with-channel-commit=" #$commit)
             (filter (lambda (flag)
                       (not (string-prefix? "--with-channel-commit=" flag)))
                     #$flags))))))))

(define guix-for-images
  (mlambda (system)
    (cond
     ;; For testing purposes, use the guix package directly.
     (%use-snapshot-package? guix)
     ;; Normally, update the guix package to current commit.
     (else
      (guix-package/with-commit guix (guix-package-commit (current-guix)))))))

(define %binary-tarball-compression "xz")

;; Like guix pack -C xz -s --localstatedir --profile-name=current-guix guix
(define* (binary-tarball-for-system system #:key (extra-packages '()))
  (let* ((base-name (string-append "guix-binary-" (%guix-version) "." system))
         (manifest (packages->manifest (cons* guix extra-packages)))
         (profile (profile (content manifest)))
         (inputs `(("profile" ,profile)))
         (compression %binary-tarball-compression))
    (manifest-entry
      (name (string-append base-name ".tar." compression))
      (version (%guix-version))
      (item (monadic->declarative
             (self-contained-tarball
              base-name profile
              #:profile-name "current-guix"
              #:compressor (lookup-compressor compression)
              #:localstatedir? #t))))))

;; Like guix system image -t iso9660 \
;; --label="GUIX_$${system}_$(VERSION)" gnu/system/install.scm
(define* (iso-for-system system)
  (let* ((name (string-append
                "guix-system-install-" (%guix-version) "." system ".iso"))
         (base-os (make-installation-os
                   #:grub-displayed-version (%guix-version)
                   #:efi-only? (string=? system "aarch64-linux")))
         (base-image (os->image base-os #:type iso-image-type))
         (label (string-append "GUIX_" system "_"
                               (if (> (string-length (%guix-version)) 7)
                                   (string-take (%guix-version) 7)
                                   (%guix-version)))))
    (manifest-entry
     (name name)
     (version (%guix-version))
     (item (system-image
            (image-with-label
             (image
               (inherit base-image)
               (name (string->symbol name)))
             label))))))

;; Like guix system image -t qcow2 gnu/system/examples/vm-image.tmpl
(define* (qcow2-for-system system)
  (let* ((name (string-append
                "guix-system-vm-image-" (%guix-version) "." system ".qcow2"))
         (base-os-path
          (if (qcow2-gpt-for-system? system)
              (%vm-image-efi-path)
              (%vm-image-path)))
         (target-image-type
          (if (qcow2-gpt-for-system? system)
              qcow2-gpt-image-type
              qcow2-image-type))
         (base-os
          (operating-system-with-/etc/config.scm
           (operating-system-with-simple-provenance
            (load base-os-path) base-os-path)))
         (base-image (os->image base-os #:type target-image-type)))
    (manifest-entry
     (name name)
     (version (%guix-version))
     (item (system-image
             (image
               (inherit base-image)
               (volatile-root? #f)
               (name (string->symbol name))))))))

(define* (guix-source-tarball)
  (let ((guix (package
                (inherit guix)
                (native-inputs
                 (modify-inputs (package-native-inputs guix)
                   ;; graphviz-minimal -> graphviz
                   (replace "graphviz" graphviz)
                   (append imagemagick)
                   (append perl))))))
    (manifest-entry
      (name (string-append "guix-" (%guix-version) ".tar.gz"))
      (version (package-version guix))
      (item (dist-package
             guix
             ;; Guix is built from git source, not from tarball.
             ;; So it's fine to use its source directly.
             (package-source guix))))))

(define* (manifest-entry-with-parameters system entry
                                         #:key
                                         (guix-for-images-proc guix-for-images))
  (manifest-entry
    (inherit entry)
    (item
     (with-parameters
         ((%current-system system)
          (%current-target-system #f)
          (current-guix-package (guix-for-images-proc system)))
       (manifest-entry-item entry)))))

(define* (manifest-with-parameters system manifest
                                   #:key
                                   (guix-for-images-proc guix-for-images))
  "Returns entries in the manifest accompanied with %current-system,
%current-target-sytem and current-guix-package parameters."
  (make-manifest
   (map (cut manifest-entry-with-parameters system <>
             #:guix-for-images-proc guix-for-images-proc)
        (manifest-entries manifest))))

(define (artifacts-for-system/nonparameterized system)
  "Get all artifacts for given system.  This will always include the
guix-binary tarball and optionally iso and/or qcow2 images."
  (manifest
   (append
    (list
     (binary-tarball-for-system system))
    ;; TODO: After source tarball generation is ready, uncomment.
    ;; (if (source-tarball-for-system? system)
    ;;     (list (guix-source-tarball))
    ;;     '())
    (if (iso-for-system? system)
        (list (iso-for-system system))
        '())
    (if (qcow2-for-system? system)
        (list (qcow2-for-system system))
        '()))))

(define* (artifacts-for-system system
                               #:key
                               (guix-for-images-proc guix-for-images))
  "Collects all artifacts for a system.  Gives them the proper %current-system
and %current-target-system parameters, so the --system passed on CLI is
irrelevant."
  ;; NOTE: parameterizing current system, because the tarball seems to somehow
  ;; depend on it early on. I haven't investigated it, but seems like a bug. Could
  ;; it be the gexp->derivation + monadic->declarative, not passing down the
  ;; system?  Symptom: guix build --system=x86_64 -m artifacts-manifest.scm and
  ;; guix build --system=i686-linux -m artifacts-manifest.scm gives out different
  ;; results without the parameterization.
  (parameterize
      ((%current-system system)
       (%current-target-system #f)
       (current-guix-package (guix-for-images-proc system)))
    (manifest-with-parameters
     system
     (artifacts-for-system/nonparameterized system)
     #:guix-for-images-proc guix-for-images-proc)))

(define (manifest->union manifest)
  "Makes a union that will be a folder with all the entries symlinked.  This
is different from a profile as it expects the entries are just simple files
and symlinks them by their manifest-entry-name."
  (let ((entries (manifest-entries manifest)))
    (computed-file
     "artifacts-union"
     (with-imported-modules '((guix build union)
                              (guix build utils))
       #~(begin
           (use-modules (guix build utils))

           (mkdir-p #$output)

           (for-each
            (lambda* (entry)
              (symlink (cdr entry)
                       (string-append #$output "/" (car entry))))
            (list #$@(map (lambda (entry)
                            #~(cons
                               #$(manifest-entry-name entry)
                               #$(manifest-entry-item entry)))
                          entries))))))))

(define %supported-systems
  (or (and
       (getenv "SUPPORTED_SYSTEMS")
       (string-split (getenv "SUPPORTED_SYSTEMS") #\ ))
      '("x86_64-linux" "i686-linux"
        "armhf-linux" "aarch64-linux"
        "powerpc64le-linux" "riscv64-linux")))

(define supported-systems-union-manifest
  (concatenate-manifests
   (map artifacts-for-system
        %supported-systems)))

(when %use-snapshot-package?
  (warning (G_ "building images using the 'guix' package (snapshot)~%")))
(info (G_ "producing artifacts for the following systems: ~a~%")
          %supported-systems)
supported-systems-union-manifest
