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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages make-bootstrap)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu system vm)
  #:use-module ((guix scripts pack) #:prefix pack:)
  #:use-module (srfi srfi-9)
  #:export (%test-debian-install
            %test-archlinux-install
            %test-ubuntu-install
            %test-fedora-install))

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
                                     (device "/dev/vdb1")
                                     (resize-image #f)
                                     (resize-proc #~(const #f)))
  "Instrument IMAGE, returning a new image that contains a statically-linked
Guile under /opt/guix and a marionette systemd service.  The relevant file
system is expected to be on DEVICE.  When RESIZE-IMAGE is not #f, it is
supplied as an argument to qemu-img resize as new size of the image, eg.
\"+1G\" to add 1 GiB to the partition and its file system.  RESIZE-PROC is a
gexp evaluating to a two-argument procedure.  The two arguments are device and
marionette.  This procedure will be called from within a VM and it should
resize the partition and file system, if appropriate."
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

          (when #$resize-image
            (invoke (string-append #+qemu "/bin/qemu-img")
                    "resize" target-image #$resize-image))

          ;; Run a VM that will mount IMAGE and populate it.  This is somewhat
          ;; more convenient to set up than 'guestfish' from libguestfs.
          (let ((marionette
                 (make-marionette
                  (list #$vm "-drive"
                        (string-append "file=" target-image
                                       ",format=qcow2,if=virtio,"
                                       "cache=writeback,werror=report,readonly=off")))))

            (#$resize-proc #$device marionette)

            (unless (zero? (marionette-eval '(system* "mount" #$device "/mnt")
                                            marionette))
              (error "failed to mount foreign distro image" #$device))

            ;; Make newly copied files relabeled for SELinux-enabled distributions
            (marionette-eval '(system* "touch" "/mnt/.autorelabel") marionette)

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

(define resize-ext4-partition
;;    Gexp evaluating to a two-argument procedure, taking DEVICE and
;; MARIONETTE.  It will grow the given device and its file system to 100 %
;; of the empty space on the image.
  #~(lambda (device marionette)
      (unless (zero? (marionette-eval
                      `(system*
                        #$(file-append cloud-utils "/bin/growpart")
                        (string-take ,device (- (string-length ,device) 1))
                        (string-take-right ,device 1))
                      marionette))
        (error "failed to grow the partition"))

      ;; ;; resize2fs will refuse operation when e2fsck is not ran.
      (unless (zero? (marionette-eval
                      `(system* #$(file-append e2fsprogs "/sbin/e2fsck")
                                "-fy" ,device)
                      marionette))
        (error "failed to repair the file system"))

      (unless (zero? (marionette-eval
                      `(system* #$(file-append e2fsprogs "/sbin/resize2fs")
                                ,device)
                      marionette))
        (error "failed to grow the file system"))))

(define resize-lvm-xfs-partition
  ;;  Gexp evaluating to a two-argument procedure, taking DEVICE and
  ;; MARIONETTE.  It will grow the given device and its file system to 100 %
  ;; of the empty space on the image.
  #~(lambda (device marionette)

      (unless (zero? (marionette-eval `(begin
                                         (system* #$(file-append lvm2 "/sbin/pvscan"))
                                         (system* #$(file-append lvm2 "/sbin/vgscan"))
                                         (system* #$(file-append lvm2 "/sbin/vgchange") "-a" "y")
                                         (system* #$(file-append lvm2 "/sbin/lvscan"))
                                         (system* #$(file-append lvm2 "/sbin/lvextend") "-l" "+100%FREE" ,device))
                                      marionette))
        `(error "failed to extend logical volume" ,device))

      (unless (zero? (marionette-eval `(system* "mount" ,device "/mnt")
                                      marionette))
        `(error "failed to mount foreign distro image" ,device))

      (unless (zero? (marionette-eval
                      `(system* #$(file-append xfsprogs "/sbin/xfs_growfs")
                                ,device)
                      marionette))
        (error "failed to grow the file system"))

      (unless (zero? (marionette-eval '(system* "umount" "/mnt")
                                      marionette))
        (error "failed to unmount device"))))

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
                                   #:key
                                   (device "/dev/vdb1")
                                   (deb-files '())
                                   (resize-image #f)
                                   (resize-proc #~(const #f)))
  "Run an installation of Guix in IMAGE, the QCOW2 image of a systemd-based
GNU/Linux distro, and check that the installation is functional.  The root
partition of IMAGE is expected to be on DEVICE.  Prior to that, install all of
DEB-FILES with 'dpkg -i'.  When RESIZE-IMAGE is not #f, it is supplied as an
argument to qemu-img resize as new size of the image, eg.  \"+1G\" to add 1
GiB to the partition and its file system.  RESIZE-PROC is a gexp evaluating to
a two-argument procedure.  The two arguments are device and marionette.  This
procedure will be called from within a VM and it should resize the partition
and file system, if appropriate."
  (define instrumented-image
    (qcow-image-with-marionette image
                                #:name (string-append name ".qcow2")
                                #:device device
                                #:resize-image resize-image
                                #:resize-proc resize-proc))

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
                   "-m" "1024"
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

          (test-assert "install extra .deb packages"
            (marionette-eval
             '(and #$@(map (lambda (deb)
                             #~(system* "dpkg" "-i"
                                        (in-vicinity "/host" (basename #$deb))))
                           deb-files))
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

          ;; NOTE: There was a bug where the service couldn't be restarted,
          ;; because the store would be captured as read-only when creating
          ;; the private mount namespace.
          ;; See <https://codeberg.org/guix/guix/issues/4744>.
          ;; So we try restarting it here.
          (test-equal "restart guix-daemon.service"
            0
            (marionette-eval '(system* "systemctl" "restart"
                                       "guix-daemon.service")
                             marionette))

          (test-equal "guix build hello after guix-daemon.service restart"
            0
            (marionette-eval '(system* "guix" "build"
                                       "hello" "--no-grafts"
                                       "--check")
                             marionette))

          (test-equal "guix gc after guix-daemon.service"
            0
            (marionette-eval '(system* "guix" "gc")
                             marionette))

          ;; NOTE: Since the fix of the bug meant stopping gnu-store.mount
          ;; during startup of guix-daemon service, check that it's started.
          ;; Check only after some time, because the service might not be
          ;; up yet right after the restart.
          (test-equal "gnu-store.mount is active after guix-daemon restart"
            0
            (marionette-eval '(system* "systemctl" "is-active"
                                       "gnu-store.mount")
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

(define debian-13-qcow2
  ;; Image taken from <https://www.debian.org/distrib/>.
  ;; XXX: Those images are periodically removed from debian.org.
  (let ((image-date "20251006-2257"))
    (origin
      (uri (string-append "https://cloud.debian.org/images/cloud/trixie/"
                          image-date "/debian-13-nocloud-amd64-"
                          image-date ".qcow2"))
      (method url-fetch)
      (file-name "debian-13-nocloud-amd64.qcow2")
      (sha256
       (base32
        "0g7kcvz2yzr0xchlv5kc8d2rd2lzk4akh02i43i92cmys7q3r05c")))))

(define debian-uidmap-deb-file
  ;; This package provides 'newgidmap' & co., used by the unprivileged daemon.
  (origin
    (uri
     "mirror://debian/pool/main/s/shadow/uidmap_4.13+dfsg1-1+deb12u1_amd64.deb")
    (method url-fetch)
    (sha256
     (base32
      "0iqhljzmnni3k3jc1xb0mrb7cqywkzrmdc2322kd8b1wpw45zv8l"))))

;; This test starts failing when derivations in repo for GNU Hello and its dependencies
;; differs from versions in current Guix package. The simple way to fix it is to update
;; Guix package version.
(define %test-debian-install
  (system-test
   (name "debian-install")
   (description
    "Test installation of Guix on Debian using the @file{guix-install.sh}
script.")
   (value (run-foreign-install-test debian-13-qcow2 name
                                    #:deb-files (list debian-uidmap-deb-file)))))

(define archlinux-qcow2
  ;; Images generated by <https://gitlab.archlinux.org/archlinux/arch-boxes>;
  ;; using default mirror mentioned in README there.
  ;; XXX: These images are removed every ~90 days
  (origin
    (uri "https://geo.mirror.pkgbuild.com/images/v20251015.435734/Arch-Linux-x86_64-basic.qcow2")
    (method url-fetch)
    (sha256
     (base32
      "0mlvfsw9ak9b6cl5h0mhs90vqkqd3ha3lx4rv9xg2b510q6656li"))))

;; This test starts failing when derivations in repo for GNU Hello and its dependencies
;; differs from versions in current Guix package. The simple way to fix it is to update
;; Guix package version.
(define %test-archlinux-install
  (system-test
   (name "archlinux-install")
   (description
    "Test installation of Guix on Arch Linux using the @file{guix-install.sh}
script.")
   (value (run-foreign-install-test archlinux-qcow2 name
                                    #:device "/dev/vdb3"))))

(define ubuntu-qcow2
  (let ((filename "noble-server-cloudimg-amd64.img"))
    (origin
      (uri (string-append "https://cloud-images.ubuntu.com/noble/20251213/" filename))
      (method url-fetch)
      (sha256
       (base32
        "07zbqlxg8524p7igwrpwrf5kwc6qam78f7023ihfy38qx3zr0prb")))))

(define ubuntu-libsuid4-deb-file
  ;; This package is a dependency of uidmap that's missing in the image.
  (origin
    (uri
     "https://archive.ubuntu.com/ubuntu/pool/main/s/shadow/libsubid4_4.13+dfsg1-4ubuntu3_amd64.deb")
    (method url-fetch)
    (sha256
     (base32
      "1g2rypacgc0gp55wy3lxsa454m072c21cgzdzaf8zlpg4dv9k08l"))))

(define ubuntu-uidmap-deb-file
  ;; This package provides 'newgidmap' & co., used by the unprivileged daemon.
  (origin
    (uri
     "https://archive.ubuntu.com/ubuntu/pool/main/s/shadow/uidmap_4.13+dfsg1-4ubuntu3_amd64.deb")
    (method url-fetch)
    (sha256
     (base32
      "1y8sw1shsxaan2icl6ajr2mwm6n69mvqzxmjl98sj4i82hdlm6nw"))))

(define %test-ubuntu-install
  (system-test
   (name "ubuntu-install")
   (description
    "Test installation of Guix on Ubuntu using the @file{guix-install.sh}
script.")
   (value (run-foreign-install-test ubuntu-qcow2 name
                                    #:deb-files (list ubuntu-libsuid4-deb-file
                                                      ubuntu-uidmap-deb-file)
                                    #:resize-image "+1G"
                                    #:resize-proc resize-ext4-partition))))
(define fedora-qcow2
  (origin
    (uri "https://download.fedoraproject.org/pub/fedora/linux/releases/43/Server/x86_64/images/Fedora-Server-Guest-Generic-43-1.6.x86_64.qcow2")
    (method url-fetch)
    (sha256
     (base32
      "0j142kbrfcv9fwds4bw57dqhnjvpzd6d6m15ar3sgf1yw9dq0pac"))))

;; This test starts failing when derivations in repo for GNU Hello and its dependencies
;; differs from versions in current Guix package. The simple way to fix it is to update
;; Guix package version.
(define %test-fedora-install
  (system-test
   (name "fedora-install")
   (description
    "Test installation of Guix on Fedora Linux using the @file{guix-install.sh}
script.")
   (value (run-foreign-install-test fedora-qcow2 name
                                    #:device "/dev/systemVG/LVRoot"
                                    #:resize-image "+1G"
                                    #:resize-proc resize-lvm-xfs-partition))))
