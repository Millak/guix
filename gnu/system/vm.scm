;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu system vm)
  #:use-module (guix config)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module ((guix self) #:select (make-config.scm))

  #:use-module ((gnu build marionette)
                #:select (qemu-command))
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages linux)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu system image)
  #:use-module (gnu system linux-container)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu bootloader)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system uuid)

  #:use-module ((srfi srfi-1) #:hide (partition))
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)

  #:export (virtualized-operating-system
            system-qemu-image/shared-store-script

            linux-image-startup-command

            virtual-machine
            virtual-machine?
            virtual-machine-operating-system
            virtual-machine-qemu
            virtual-machine-cpu-count
            virtual-machine-volatile?
            virtual-machine-graphic?
            virtual-machine-memory-size
            virtual-machine-disk-image-size
            virtual-machine-port-forwardings
            virtual-machine-date

            file-system->mount-tag
            common-qemu-options))


;;; Commentary:
;;;
;;; Tools to evaluate build expressions within virtual machines.
;;;
;;; Code:

;; By default, the msize value is 8 KiB, which according to QEMU is
;; insufficient and would degrade performance.  The msize value should roughly
;; match the bandwidth of the system's IO (see:
;; https://wiki.qemu.org/Documentation/9psetup#msize).  Use 100 MiB as a
;; conservative default.
(define %default-msize-value (* 100 (expt 2 20))) ;100 MiB

;;;
;;; VMs that share file systems with the host.
;;;

(define (file-system->mount-tag fs)
  "Return a 9p mount tag for host file system FS."
  ;; QEMU mount tags must be ASCII, at most 31-byte long, cannot contain
  ;; slashes, and cannot start with '_'.  Compute an identifier that
  ;; corresponds to the rules.
  (string-append "TAG"
                 (string-drop (bytevector->base32-string
                               (sha1 (string->utf8 fs)))
                              4)))

(define (mapping->file-system mapping)
  "Return a 9p file system that realizes MAPPING."
  (match mapping
    (($ <file-system-mapping> source target writable?)
     (file-system
       (mount-point target)
       (device (file-system->mount-tag source))
       (type "9p")
       (flags (if writable? '() '(read-only)))

       ;; The 9p documentation says that cache=loose is "intended for
       ;; exclusive, read-only mounts", without additional details.  It's
       ;; faster than the default cache=none, especially when copying and
       ;; registering store items.  Thus, use cache=loose, except for writable
       ;; mounts, to ensure consistency.
       (options (string-append "trans=virtio"
                               (if writable? "" ",cache=loose")
                               ",msize=" (number->string %default-msize-value)))
       (check? #f)
       (create-mount-point? #t)))))

(define* (virtualized-operating-system os
                                       #:optional (mappings '())
                                       #:key (full-boot? #f) volatile?
                                       (system (%current-system))
                                       (target (%current-target-system)))
  "Return an operating system based on OS suitable for use in a virtualized
environment with the store shared with the host.  MAPPINGS is a list of
<file-system-mapping> to realize in the virtualized OS."
  (define user-file-systems
    ;; Remove file systems that conflict with those added below, or that are
    ;; normally bound to real devices.
    (remove (lambda (fs)
              (let ((target (file-system-mount-point fs))
                    (source (file-system-device fs)))
                (or (string=? target (%store-prefix))
                    (string=? target "/")
                    (and (string? source)
                         (string-prefix? "/dev/" source))

                    ;; Labels and UUIDs are necessarily invalid in the VM.
                    (and (file-system-mount? fs)
                         (or (file-system-label? source)
                             (uuid? source))))))
            (operating-system-file-systems os)))

  (define virtual-file-systems
    (cons (file-system
            (mount-point "/")
            (device "/dev/vda1")
            (type "ext4"))

          (append (map mapping->file-system mappings)
                  user-file-systems)))

  (operating-system
    (inherit os)
    ;; XXX: Until we run QEMU with UEFI support (with the OVMF firmware),
    ;; force the traditional i386/BIOS method.
    ;; See <https://bugs.gnu.org/28768>.
    (bootloader (bootloader-configuration
                 (inherit (operating-system-bootloader os))
                 (bootloader
                  (if (target-riscv64? (or target system))
                      u-boot-qemu-riscv64-bootloader
                      grub-bootloader))
                 (targets '("/dev/vda"))))

    (initrd (lambda (file-systems . rest)
              (apply (operating-system-initrd os)
                     file-systems
                     #:volatile-root? volatile?
                     rest)))
    ;; The (QEMU-only) "cirrus" graphics driver is still expected by some
    ;; VPS with old QEMU versions.  See <https://bugs.gnu.org/36069>.
    ;;
    ;; XXX In 6.14, the kernel renamed the "cirrus" module to "cirrus-qemu", so
    ;; we account for that here. The renaming was done in this commit:
    ;; https://git.kernel.org/pub/scm/linux/kernel/git/sashal/linux-stable.git/commit/?id=5c3c99868aa2e0b68ac69f8050a6b9c994e73397
    (initrd-modules (let* ((modules (operating-system-initrd-modules os))
                           (kernel-version
                             (package-version (operating-system-kernel os)))
                           (cirrus-module
                             (if (string< kernel-version "6.14")
                               "cirrus"
                               "cirrus-qemu")))
                      (if (member cirrus-module modules)
                          modules
                          (cons cirrus-module modules))))

    ;; Disable swap.
    (swap-devices '())

    ;; XXX: When FULL-BOOT? is true, do not add a 9p mount for /gnu/store
    ;; since that would lead the bootloader config to look for the kernel and
    ;; initrd in it.
    (file-systems (if full-boot?
                      virtual-file-systems
                      (cons
                       (file-system
                         (inherit (mapping->file-system %store-mapping))
                         (needed-for-boot? #t))
                       virtual-file-systems)))))

(define* (common-qemu-options image shared-fs
                              #:key
                              (image-format "raw")
                              rw-image?
                              (target (%current-target-system)))
  "Return the a string-value gexp with the common QEMU options to boot IMAGE,
with '-virtfs' options for the host file systems listed in SHARED-FS."

  (define (virtfs-option fs)
    (list "-virtfs"
          #~(format #f "local,path=~a,security_model=none,mount_tag=~a"
                    #$fs #$(file-system->mount-tag fs))))

  #~(;; Only enable kvm if we see /dev/kvm exists.
     ;; This allows users without hardware virtualization to still use these
     ;; commands.
     #$@(if (and (not target) (file-exists? "/dev/kvm"))
            '("-enable-kvm")
            '())

     "-object" "rng-random,filename=/dev/urandom,id=guix-vm-rng"
     "-device" "virtio-rng-pci,rng=guix-vm-rng"

     #$@(append-map virtfs-option shared-fs)
     "-drive"
     #$(if rw-image?
           #~(format #f "file=~a,format=qcow2,if=virtio" #$image)
           #~(format #f "file=~a,format=~a,if=virtio,cache=writeback,werror=report,readonly=on"
                     #$image #$image-format))))

(define* (system-qemu-image/shared-store-script os
                                                #:key
                                                (system (%current-system))
                                                (target (%current-target-system))
                                                (qemu qemu)
                                                (graphic? #t)
                                                (volatile? #t)
                                                (memory-size 512)
                                                (mappings '())
                                                full-boot?
                                                (disk-image-size
                                                 (* (if full-boot? 500 70)
                                                    (expt 2 20)))
                                                (options '()))
  "Return a derivation that builds a script to run a virtual machine image of
OS that shares its store with the host.  The virtual machine runs with
MEMORY-SIZE MiB of memory.

MAPPINGS is a list of <file-system-mapping> specifying mapping of host file
systems into the guest.

When FULL-BOOT? is true, the returned script runs everything starting from the
bootloader; otherwise it directly starts the operating system kernel.  When
VOLATILE? is true, an overlay is created on top of a read-only
storage. Otherwise the storage is made persistent.  The DISK-IMAGE-SIZE
parameter specifies the size in bytes of the root disk image; it is mostly
useful when FULL-BOOT?  is true."
  (mlet* %store-monad ((os ->  (virtualized-operating-system
                                os mappings
                                #:full-boot? full-boot?
                                #:volatile? volatile?
                                #:system system
                                #:target target))
                       (base-image -> (system-image
                                       (image
                                        (inherit
                                         (raw-with-offset-disk-image))
                                        (operating-system os)
                                        (size disk-image-size)
                                        (shared-store?
                                         (and (not full-boot?) volatile?))
                                        (volatile-root? volatile?)))))
    (define kernel-arguments
      #~(list #$@(if graphic? #~() #~("console=ttyS0"))
              #$@(operating-system-kernel-arguments os "/dev/vda1")))

    (define rw-image
      #~(format #f "/tmp/guix-image-~a" (basename #$base-image)))

    (define qemu-exec
      #~(list #+(with-parameters ((%current-system %system)
                                  (%current-target-system #f))
                  ;; Override %CURRENT-SYSTEM to always use a native emulator.
                  (file-append qemu "/bin/"
                               (qemu-command (or target system))))
              ;; Tells qemu to use the terminal it was started in for IO.
              #$@(if graphic? '() #~("-nographic"))
              #$@(if full-boot?
                     #~()
                     #~("-kernel" #$(operating-system-kernel-file os)
                        "-initrd" #$(file-append os "/initrd")
                        (format #f "-append ~s"
                                (string-join #$kernel-arguments " "))))
              ;; Default qemu-riscv64 have not PCI, virt have it, so we set it.
              #$@(if (target-riscv64? (or target system))
                     #~("-M" "virt")
                     #~())
              #$@(common-qemu-options (if volatile? base-image rw-image)
                                      (map file-system-mapping-source
                                           (cons %store-mapping mappings))
                                      #:rw-image? (not volatile?)
                                      #:target target)
              "-m " (number->string #$memory-size)
              #$@options))

    (define copy-image
      ;; Script that "copies" BASE-IMAGE to /tmp.  Make a copy-on-write image,
      ;; which is much cheaper than actually copying it.
      (program-file "copy-image"
                    (with-imported-modules '((guix build utils))
                      #~(begin
                          (use-modules (guix build utils))
                          (unless (file-exists? #$rw-image)
                            (invoke #+(file-append qemu "/bin/qemu-img")
                                    "create" "-b" #$base-image
                                    "-F" "raw" "-f" "qcow2" #$rw-image))))))

    (define builder
      #~(call-with-output-file #$output
          (lambda (port)
            (format port "#!~a~%"
                    #+(file-append bash "/bin/sh"))
            #$@(if volatile?
                   #~()
                   #~((format port "~a~%" #+copy-image)))
            (format port "exec ~a \"$@\"~%"
                    (string-join #$qemu-exec " "))
            (chmod port #o555))))

    (gexp->derivation "run-vm.sh" builder)))

(define* (linux-image-startup-command image
                                      #:key
                                      (system (%current-system))
                                      (target #f)
                                      (qemu qemu-minimal)
                                      (graphic? #f)
                                      (cpu "max")
                                      (cpu-count 1)
                                      (memory-size 1024)
                                      (port-forwardings '())
                                      (date #f))
  "Return a list-valued gexp representing the command to start QEMU to run
IMAGE, assuming it uses the Linux kernel, and not sharing the store with the
host."
  (define os
    ;; Note: 'image-operating-system' would return the wrong OS, before
    ;; its root partition has been assigned a UUID.
    (operating-system-for-image image))

  (define kernel-arguments
    #~(list #$@(if graphic? #~() #~("console=ttyS0"))
            #$@(operating-system-kernel-arguments os "/dev/vda1")))

  #~`(#+(file-append qemu "/bin/"
                     (qemu-command (or target system)))
      ,@(if (access? "/dev/kvm" (logior R_OK W_OK))
            '("-enable-kvm")
            '())

      "-cpu" #$cpu
      #$@(if (> cpu-count 1)
             #~("-smp" #$(string-append "cpus=" (number->string cpu-count)))
             #~())
      "-m" #$(number->string memory-size)
      "-nic" #$(string-append
                "user,model=virtio-net-pci,"
                (port-forwardings->qemu-options port-forwardings))
      "-kernel" #$(operating-system-kernel-file os)
      "-initrd" #$(file-append os "/initrd")
      "-append" ,(string-join #$kernel-arguments)
      "-serial" "stdio"

      #$@(if date
             #~("-rtc"
                #$(string-append "base=" (date->string date "~5")))
             #~())

      "-object" "rng-random,filename=/dev/urandom,id=guix-vm-rng"
      "-device" "virtio-rng-pci,rng=guix-vm-rng"

      "-drive"
      ,(string-append "file=" #$(system-image image)
                      ",format=qcow2,if=virtio,"
                      "cache=writeback,werror=report,readonly=off")
      "-snapshot"
      "-no-reboot"))


;;;
;;; High-level abstraction.
;;;

(define-record-type* <virtual-machine> %virtual-machine
  make-virtual-machine
  virtual-machine?
  (operating-system virtual-machine-operating-system) ;<operating-system>
  (qemu             virtual-machine-qemu              ;<package>
                    (default qemu-minimal))
  (cpu-count        virtual-machine-cpu-count     ;integer
                    (default 1))
  (volatile?        virtual-machine-volatile?    ;Boolean
                    (default #t))
  (graphic?         virtual-machine-graphic?      ;Boolean
                    (default #f))
  (memory-size      virtual-machine-memory-size   ;integer (MiB)
                    (default 256))
  (disk-image-size  virtual-machine-disk-image-size   ;integer (bytes)
                    (default 'guess))
  (port-forwardings virtual-machine-port-forwardings ;list of integer pairs
                    (default '()))
  (date             virtual-machine-date          ;SRFI-19 date | #f
                    (default #f)))

(define-syntax virtual-machine
  (syntax-rules (operating-system)
    "Declare a virtual machine running the specified OS, with the given
options."
    ((_ (operating-system os))
     ;; Also accept the long form (virtual-machine (operating-system os)), for
     ;; correctness.
     (%virtual-machine (operating-system os)))
    ((_ os)                             ;shortcut
     (%virtual-machine (operating-system os)))
    ((_ fields ...)
     (%virtual-machine fields ...))))

(define (port-forwardings->qemu-options forwardings)
  "Return the QEMU option for the given port FORWARDINGS as a string, where
FORWARDINGS is a list of host-port/guest-port pairs."
  (string-join
   (map (match-lambda
          ((host-port . guest-port)
           (string-append "hostfwd=tcp::"
                          (number->string host-port)
                          "-:" (number->string guest-port))))
        forwardings)
   ","))

(define-gexp-compiler (virtual-machine-compiler (vm <virtual-machine>)
                                                system target)
  (match vm
    (($ <virtual-machine> os qemu cpus volatile? graphic? memory-size
                          disk-image-size forwardings date)
     (let ((options
            (append (if (null? forwardings)
                        '()
                        `("-nic" ,(string-append
                                   "user,model=virtio-net-pci,"
                                   (port-forwardings->qemu-options
                                    forwardings))))
                    (if (> cpus 1)
                        `("-smp" ,(string-append "cpus="
                                                 (number->string cpus)))
                        '())
                    (if date
                        `("-rtc"
                          ,(string-append
                            "base=" (date->string date "~5")))
                        '()))))
       (system-qemu-image/shared-store-script os
                                              #:system system
                                              #:target target
                                              #:qemu qemu
                                              #:graphic? graphic?
                                              #:volatile? volatile?
                                              #:memory-size memory-size
                                              #:disk-image-size
                                              disk-image-size
                                              #:options options)))))

;;; vm.scm ends here
