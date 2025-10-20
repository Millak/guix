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
  #:use-module ((gnu tests base)
                #:select (%hello-dependencies-manifest
                          guix-daemon-test-cases))
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages make-bootstrap)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu system vm)
  #:use-module ((guix scripts pack) #:prefix pack:)
  #:use-module (srfi srfi-9)
  #:export (%test-debian-install
            %test-archlinux-install))

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
                  "-F" "qcow2" "-f" "qcow2" target-image)

          ;; Run a VM that will mount IMAGE and populate it.  This is somewhat
          ;; more convenient to set up than 'guestfish' from libguestfs.
          (let ((marionette
                 (make-marionette
                  (list #$vm "-drive"
                        (string-append "file=" target-image
                                       ",format=qcow2,if=virtio,"
                                       "cache=writeback,werror=report,readonly=off")))))

            (unless (zero? (marionette-eval '(system* "mount" #$device "/mnt")
                                            marionette))
              (error "failed to mount foreign distro image" #$device))

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
         %hello-dependencies-manifest)))

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

(define* (run-foreign-install-test image name
                                   #:key (device "/dev/vdb1"))
  "Run an installation of Guix in IMAGE, the QCOW2 image of a systemd-based
GNU/Linux distro, and check that the installation is functional.  The root
partition of IMAGE is expected to be on DEVICE."
  (define instrumented-image
    (qcow-image-with-marionette image
                                #:name (string-append name ".qcow2")
                                #:device device))

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
            ;; when 'GUIX_BINARY_FILE_NAME' is set.  Provide fake binaries
            ;; that always succeed.
            (marionette-eval '(begin
                                (false-if-exception
                                 (delete-file "/bin/wget"))
                                (symlink "/bin/true" "/bin/wget")
                                (false-if-exception
                                 (delete-file "/bin/gpg"))
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

          (test-equal "create user account"
            0
            (marionette-eval '(system* "useradd" "-d" "/home/user" "-m"
                                       "user")
                             marionette))

          #$(guix-daemon-test-cases #~marionette)

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

(define debian-13-qcow2
  ;; Image taken from <https://www.debian.org/distrib/>.
  ;; XXX: Those images are periodically removed from debian.org.
  (origin
    (uri
     "https://cloud.debian.org/images/cloud/trixie/latest/debian-13-nocloud-amd64.qcow2")
    (method url-fetch)
    (sha256
     (base32
      "0g7kcvz2yzr0xchlv5kc8d2rd2lzk4akh02i43i92cmys7q3r05c"))))

;; This test starts failing when derivations in repo for GNU Hello and its dependencies
;; differs from versions in current Guix package. The simple way to fix it is to update
;; Guix package version.
(define %test-debian-install
  (system-test
   (name "debian-install")
   (description
    "Test installation of Guix on Debian using the @file{guix-install.sh}
script.")
   (value (run-foreign-install-test debian-13-qcow2 name))))

(define archlinux-qcow2
  ;; Images generated by <https://gitlab.archlinux.org/archlinux/arch-boxes>;
  ;; using default mirror mentioned in README there.
  ;; XXX: These images are removed every ~90 days
  (origin
    (uri "https://geo.mirror.pkgbuild.com/images/v20250615.366044/Arch-Linux-x86_64-basic.qcow2")
    (method url-fetch)
    (sha256
     (base32
      "11m945cv5hgfa7zgkvd7fqgqfp3vdq3c4bdh3x0ilza36w5xcn1b"))))

(define %test-archlinux-install
  (system-test
   (name "archlinux-install")
   (description
    "Test installation of Guix on Arch Linux using the @file{guix-install.sh}
script.")
   (value (run-foreign-install-test archlinux-qcow2 name
                                    #:device "/dev/vdb3"))))
