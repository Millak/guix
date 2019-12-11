;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system bootstrap)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module ((guix packages) #:select (default-guile))
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module ((guix utils)
                #:select (version-major+minor substitute-keyword-arguments))
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; This file provides tooling to build an operating system image that builds
;;; a set of derivations straight from the initrd.  This allows us to perform
;;; builds in an environment where the trusted computing base (TCB) has been
;;; stripped from guix-daemon, shepherd, and other things.
;;;
;;; Run "guix system vm gnu/system/bootstrap.scm" to get a VM that runs this
;;; OS (pass "-m 5000" or so so it has enough memory), or use "guix system
;;; disk-image", write it to a USB stick, and get it running on the bare
;;; metal!
;;;
;;; Code:

(define* (hash-script obj #:key (guile (default-guile)))
  "Return a derivation that computes the SHA256 hash of OBJ, using Guile and
only pure Guile code."
  (define hashing
    (package
      (inherit guile-hashing)
      (arguments
       `(#:guile ,guile
         ,@(package-arguments guile-hashing)))
      (native-inputs `(("guile" ,guile)))))

  (define build
    ;; Compute and display the SHA256 of OBJ.  Do that in pure Scheme: it's
    ;; slower, but removes the need for a full-blown C compiler and GNU
    ;; userland to get libgcrypt, etc.
    (with-extensions (list hashing)
      (with-imported-modules (source-module-closure
                              '((guix serialization)))
        #~(begin
            (use-modules (hashing sha-2)
                         (guix serialization)
                         (rnrs io ports)
                         (rnrs bytevectors)
                         (ice-9 match))

            (define (port-sha256 port)
              ;; Return the SHA256 of the data read from PORT.
              (define bv (make-bytevector 65536))
              (define hash (make-sha-256))

              (let loop ()
                (match (get-bytevector-n! port bv 0
                                          (bytevector-length bv))
                  ((? eof-object?)
                   (sha-256-finish! hash)
                   hash)
                  (n
                   (sha-256-update! hash bv 0 n)
                   (loop)))))

            (define (file-sha256 file)
              ;; Return the SHA256 of FILE.
              (call-with-input-file file port-sha256))

            ;; Serialize OBJ as a nar.  XXX: We should avoid writing to disk
            ;; as this might be a tmpfs.
            (call-with-output-file "nar"
              (lambda (port)
                (write-file #$obj port)))

            ;; Compute, display, and store the hash of OBJ.
            (let ((hash (file-sha256 "nar")))
              (call-with-output-file #$output
                (lambda (result)
                  (for-each (lambda (port)
                              (format port "~a\t~a~%"
                                      (sha-256->string hash)
                                      #$obj))
                            (list (current-output-port)
                                  result)))))))))

  (computed-file "build-result-hashes" build
                 #:guile guile
                 #:options
                 `(#:effective-version
                   ,(version-major+minor (package-version guile)))))

(define* (build-script obj #:key (guile (default-guile)))
  "Return a build script that builds OBJ, an arbitrary lowerable object such
as a package, and all its dependencies.  The script essentially unrolls the
build loop normally performed by 'guix-daemon'."
  (define select?
    ;; Select every module but (guix config) and non-Guix modules.
    (match-lambda
      (('guix 'config) #f)
      (('guix _ ...)   #t)
      (_               #f)))

  (define fake-gcrypt-hash
    ;; Fake (gcrypt hash) module: since (gcrypt hash) is pulled in and not
    ;; actually used, plus GUILE may be a statically-linked Guile not capable
    ;; of loading libgcrypt, it's OK to just provide a phony module.
    (scheme-file "hash.scm"
                 #~(define-module (gcrypt hash)
                     #:export (sha1 sha256))))

  (define emit-script
    (with-imported-modules `(((guix config) => ,(make-config.scm))
                             ((gcrypt hash) => ,fake-gcrypt-hash)

                             ,@(source-module-closure
                                `((guix derivations))
                                #:select? select?))
      #~(begin
          (use-modules (guix derivations)
                       (srfi srfi-1)
                       (ice-9 match)
                       (ice-9 pretty-print))

          (define drv
            ;; Load the derivation for OBJ.
            (read-derivation-from-file #$(raw-derivation-file obj)))

          (define (derivation->script drv)
            ;; Return a snippet that "manually" builds DRV.
            `(begin
               ;; XXX: Drop part of DRV's file name to not cause the
               ;; daemon to detect the reference and go wrong ("path `%1%'
               ;; is not valid").
               (format #t "~%~%build-started ...~a~%~%"
                       ,(string-drop (basename
                                      (derivation-file-name
                                       drv))
                                     10))

               ;; XXX: Use the same directory name as the daemon?
               (mkdir-p "/tmp/guix-build")
               (chdir "/tmp/guix-build")
               (environ ',(map (match-lambda
                                 ((key . value)
                                  (string-append key "=" value)))
                               (derivation-builder-environment-vars drv)))
               (let ((result (system* ,(derivation-builder drv)
                                      ,@(derivation-builder-arguments
                                         drv))))
                 (chdir "/")
                 (delete-file-recursively "/tmp/guix-build")
                 (zero? result))))

          (define graph
            ;; Closure of the derivation for OBJ.  This does _not_ contain
            ;; fixed-output derivations, but it contains sources.
            (filter-map (lambda (file)
                          (and (string-suffix? ".drv" file)
                               (let* ((drv (read-derivation-from-file file))
                                      (out (derivation->output-path drv)))
                                 ;; GUILE itself is already in the initrd
                                 ;; because it's executing this program.
                                 ;; Thus, don't try to "build" it again.
                                 (and (not (string=? out #$guile))
                                      drv))))
                        (call-with-input-file #$(raw-derivation-closure obj)
                          read)))

          ;; Emit a script that builds OBJ and all its
          ;; dependencies sequentially.
          (call-with-output-file #$output
            (lambda (port)
              (format port "#!~a/bin/guile --no-auto-compile~%!#~%" #$guile)
              (pretty-print '(begin
                               (use-modules (srfi srfi-1)
                                            (ice-9 rdelim))

                               ;; Ensure the script refers to all the
                               ;; sources of OBJ.
                               (define these-are-the-sources-we-need
                                 '#$(object-sources obj))
                               (primitive-load
                                #$(local-file "../../guix/build/utils.scm")))
                            port)
              (newline port)
              (pretty-print `(and ,@(map derivation->script graph)
                                  (begin
                                    (format #t "~%Congratulations!~%")
                                    (sleep 3600)))
                            port)
              (chmod port #o555))))))

  (computed-file "build.scm" emit-script
                 #:guile guile))

(define (bootstrapping-os obj)
  "Return an operating system that starts building OBJ and all its
dependencies, from scratch, as it boots."
  (operating-system
    (host-name "komputilo")
    (timezone "Africa/Casablanca")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sdX")))
    ;; TODO: Use a minimal Linux-libre kernel.
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))

    ;; Network access and all that are not needed.
    (firmware '())

    (users (cons (user-account
                  (name "vagneke")
                  (comment "The Bootstrapper")
                  (group "users"))
                 %base-user-accounts))

    ;; Use a special initrd that builds it all!  The initrd contains the
    ;; script returned by 'build-script' and all its dependencies, which
    ;; includes all the source code (tarballs) necessary to build them.
    (initrd (lambda (fs . rest)
              (expression->initrd
               (let ((obj (hash-script obj #:guile %bootstrap-guile+guild)))
                 #~(execl #$(build-script obj #:guile %bootstrap-guile+guild)
                          "build"))
               #:guile %bootstrap-guile+guild)))))

;; This operating system builds MES-BOOT from scratch.  That currently
;; requires ~5 GiB of RAM.  TODO: Should we mount a root file system on a hard
;; disk or...?
(bootstrapping-os (@@ (gnu packages commencement) mes-boot))
