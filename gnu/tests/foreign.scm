;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu tests foreign)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:autoload   (guix store) (%store-prefix %store-monad %graft?)
  #:use-module (gnu compression)
  #:use-module (gnu tests)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages make-bootstrap)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu system vm)
  #:use-module ((guix scripts pack) #:prefix pack:)
  #:use-module (srfi srfi-9)
  #:export (%test-debian-install))

(define marionette-systemd-service
  ;; Definition of the marionette service for systemd.
  (plain-file "marionette.service" "
[Unit]
Description=Guix marionette service

[Install]
WantedBy=multi-user.target

[Service]
ExecStart=/opt/guix/bin/guile --no-auto-compile \\
  /opt/guix/share/guix/marionette-repl.scm\n"))

(define* (qcow-image-with-marionette image
                                     #:key
                                     (name "image-with-marionette.qcow2")
                                     (device "/dev/vdb1"))
  "Instrument IMAGE, returning a new image that contains a statically-linked
Guile under /opt/guix and a marionette systemd service.  The relevant file
system is expected to be on DEVICE."
  (define vm
    (virtual-machine
     (marionette-operating-system %simple-os)))

  (define build
    (with-imported-modules (source-module-closure
                            '((guix build utils)
                              (gnu build marionette)))
      #~(begin
          (use-modules (guix build utils)
                       (gnu build marionette))

          (define target-image
            #$output)

          (invoke #+(file-append qemu "/bin/qemu-img")
                  "create" "-b" #$image
                  "-F" "qcow2" "-f" "qcow2" target-image
                  "10G")

          ;; Run a VM that will mount IMAGE and populate it.  This is somewhat
          ;; more convenient to set up than 'guestfish' from libguestfs.
          (let ((marionette
                 (make-marionette
                  (list #$vm "-drive"
                        (string-append "file=" target-image
                                       ",format=qcow2,if=virtio,"
                                       "cache=writeback,werror=report,readonly=off")))))

            (marionette-eval '(system* "mount" #$device "/mnt")
                             marionette)
            (marionette-eval '(system* "ls" "-la" "/mnt")
                             marionette)
            (marionette-eval '(begin
                                (use-modules (guix build utils))
                                (mkdir-p "/mnt/opt/guix")
                                (copy-recursively #$%guile-static-initrd
                                                  "/mnt/opt/guix"
                                                  #:log (%make-void-port "w")
                                                  #:keep-mtime? #t))
                             marionette)
            (marionette-eval '(system* "/mnt/opt/guix/bin/guile" "--version")
                             marionette)
            (unless (= 42 (status:exit-val
                           (marionette-eval '(system* "/mnt/opt/guix/bin/guile"
                                                      "-c" "(exit 42)")
                                            marionette)))
              (error "statically-linked Guile is broken"))

            ;; Install the marionette systemd service and activate it.
            (marionette-eval '(begin
                                (mkdir-p "/mnt/opt/guix/share/guix")
                                (copy-file #$(marionette-program)
                                           "/mnt/opt/guix/share/guix/marionette-repl.scm")

                                (mkdir-p "/mnt/etc/systemd/system")
                                (copy-file #$marionette-systemd-service
                                           "/mnt/etc/systemd/system/marionette.service")

                                ;; Activate the service, as per 'systemctl
                                ;; enable marionette.service'.
                                (symlink
                                 "/etc/systemd/system/marionette.service"
                                 "/mnt/etc/systemd/system/multi-user.target.wants/marionette.service"))
                             marionette)

            (unless (zero? (marionette-eval '(system* "umount" "/mnt")
                                            marionette))
              (error "failed to unmount device"))))))

  (computed-file name build))

(define (manifest-entry-without-grafts entry)
  "Return ENTRY with grafts disabled on its contents."
  (manifest-entry
    (inherit entry)
    (item (with-parameters ((%graft? #f))
            (manifest-entry-item entry)))))

(define %installation-tarball-manifest
  ;; Manifest of the Guix installation tarball.
  (concatenate-manifests
   (list (packages->manifest (list guix))

         ;; Include the dependencies of 'hello' in addition to 'guix' so that
         ;; we can test 'guix build hello'.
         (map-manifest-entries
          manifest-entry-without-grafts
          (package->development-manifest hello))

         ;; Add the source of 'hello'.
         (manifest
          (list (manifest-entry
                  (name "hello-source")
                  (version (package-version hello))
                  (item (let ((file (origin-actual-file-name
                                     (package-source hello))))
                          (computed-file
                           "hello-source"
                           #~(begin
                               ;; Put the tarball in a subdirectory since
                               ;; profile union crashes otherwise.
                               (mkdir #$output)
                               (mkdir (in-vicinity #$output "src"))
                               (symlink #$(package-source hello)
                                        (in-vicinity #$output
                                                     (string-append "src/"
                                                                    #$file))))))))))

         ;; Include 'guile-final', which is needed when building derivations
         ;; such as that of 'hello' but missing from the development manifest.
         ;; Add '%bootstrap-guile', used by 'guix install --bootstrap'.
         (map-manifest-entries
          manifest-entry-without-grafts
          (packages->manifest (list (canonical-package guile-3.0)
                                    %bootstrap-guile))))))

(define %guix-install-script
  ;; The 'guix-install.sh' script.
  ;;
  ;; To test local changes, replace the expression below with:
  ;;
  ;;   (local-file "../../etc/guix-install.sh")
  ;;
  ;; This cannot be done unconditionally since that file does not exists in
  ;; inferiors.
  (file-append (package-source guix) "/etc/guix-install.sh"))

(define (run-foreign-install-test image name)
  "Run an installation of Guix in IMAGE, the QCOW2 image of a systemd-based
GNU/Linux distro, and check that the installation is functional."
  (define instrumented-image
    (qcow-image-with-marionette image
                                #:name (string-append name ".qcow2")))

  (define (test tarball)
    (with-imported-modules (source-module-closure
                            '((gnu build marionette)
                              (gnu system file-systems)))
      #~(begin
          (use-modules (gnu build marionette)
                       (gnu system file-systems)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette
             (list (string-append #$qemu-minimal "/bin/" (qemu-command))
                   #$@(common-qemu-options instrumented-image
                                           (list (%store-prefix))
                                           #:image-format "qcow2"
                                           #:rw-image? #t)
                   "-m" "512"
                   "-snapshot")))

          (test-runner-current (system-test-runner #$output))
          (test-begin "foreign-install")

          (test-equal "marionette works"
            "Linux"
            (marionette-eval '(utsname:sysname (uname))
                             marionette))

          (test-assert "/etc/os-release"
            (marionette-eval '(begin
                                (use-modules (ice-9 textual-ports))
                                (call-with-input-file "/etc/os-release"
                                  get-string-all))
                             marionette))

          (test-equal "mount host file store"
            0
            (marionette-eval
             '(begin
                (mkdir "/host")
                (system* "mount" "-t" "9p"
                         "-o" "trans=virtio,cache=loose,ro"
                         #$(file-system->mount-tag (%store-prefix))
                         "/host"))
             marionette))

          (test-assert "screenshot before"
            (marionette-control (string-append "screendump " #$output
                                               "/before-install.ppm")
                                marionette))

          (test-assert "install fake dependencies"
            ;; The installation script insists on checking for the
            ;; availability of 'wget' and 'gpg' but does not actually use them
            ;; when 'GUIX_BINARY_FILE_NAME' is set.  Provide fake binaries.
            (marionette-eval '(begin
                                (symlink "/bin/true" "/bin/wget")
                                (symlink "/bin/true" "/bin/gpg")
                                #t)
                             marionette))

          (test-assert "run install script"
            (marionette-eval '(system
                               (string-append
                                "yes '' | GUIX_BINARY_FILE_NAME="
                                (in-vicinity "/host"
                                             (basename #$tarball))
                                " sh "
                                (in-vicinity
                                 "/host"
                                 (string-drop #$%guix-install-script
                                              #$(string-length
                                                 (%store-prefix))))))
                             marionette))

          (test-equal "hello not already built"
            #f
            ;; Check that the next test will really build 'hello'.
            (marionette-eval '(file-exists?
                               #$(with-parameters ((%graft? #f))
                                   hello))
                             marionette))

          (test-equal "guix build hello"
            0
            ;; Check that guix-daemon is up and running and that the build
            ;; environment is properly set up (build users, etc.).
            (marionette-eval '(system* "guix" "build" "hello" "--no-grafts")
                             marionette))

          (test-assert "hello indeed built"
            (marionette-eval '(file-exists?
                               #$(with-parameters ((%graft? #f))
                                   hello))
                             marionette))

          (test-equal "guix install hello"
            0
            ;; Check that ~/.guix-profile & co. are properly created.
            (marionette-eval '(let ((pw (getpwuid (getuid))))
                                (setenv "USER" (passwd:name pw))
                                (setenv "HOME" (pk 'home (passwd:dir pw)))
                                (system* "guix" "install" "hello"
                                         "--no-grafts" "--bootstrap"))
                             marionette))

          (test-equal "user profile created"
            0
            (marionette-eval '(system "ls -lad ~/.guix-profile")
                             marionette))

          (test-equal "hello"
            0
            (marionette-eval '(system "~/.guix-profile/bin/hello")
                             marionette))

          (test-equal "create user account"
            0
            (marionette-eval '(system* "useradd" "-d" "/home/user" "-m"
                                       "user")
                             marionette))

          (test-equal "guix install hello, unprivileged user"
            0
            ;; Check that 'guix' is in $PATH for new users and that
            ;; ~user/.guix-profile also gets created.
            (marionette-eval '(system "su - user -c \
'guix install hello --no-grafts --bootstrap'")
                             marionette))

          (test-equal "user hello"
            0
            (marionette-eval '(system "~user/.guix-profile/bin/hello")
                             marionette))

          (test-equal "unprivileged user profile created"
            0
            (marionette-eval '(system "ls -lad ~user/.guix-profile")
                             marionette))

          (test-equal "store is read-only"
            EROFS
            (marionette-eval '(catch 'system-error
                                (lambda ()
                                  (mkdir (in-vicinity #$(%store-prefix)
                                                      "whatever"))
                                  0)
                                (lambda args
                                  (system-error-errno args)))
                             marionette))

          (test-assert "screenshot after"
            (marionette-control (string-append "screendump " #$output
                                               "/after-install.ppm")
                                marionette))

          (test-end))))

  (mlet* %store-monad ((profile (profile-derivation
                                 %installation-tarball-manifest))
                       (tarball (pack:self-contained-tarball
                                 "guix-binary" profile
                                 #:compressor (lookup-compressor "zstd")
                                 #:profile-name "current-guix"
                                 #:localstatedir? #t)))
    (gexp->derivation name (test tarball))))

(define debian-12-qcow2
  ;; Image taken from <https://www.debian.org/distrib/>.
  ;; XXX: Those images are periodically removed from debian.org.
  (origin
    (uri
     "https://cloud.debian.org/images/cloud/bookworm/20250210-2019/debian-12-nocloud-amd64-20250210-2019.qcow2")
    (method url-fetch)
    (sha256
     (base32
      "06vlcq2dzgczlyp9lfkkdf3dgvfjp22lh5xz0mnl0bdgzq61sykb"))))

(define %test-debian-install
  (system-test
   (name "debian-install")
   (description
    "Test installation of Guix on Debian using the @file{guix-install.sh}
script.")
   (value (run-foreign-install-test debian-12-qcow2 name))))
