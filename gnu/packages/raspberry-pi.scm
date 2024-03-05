;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2021 Stefan <stefan-guix@vodafonemail.de>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages raspberry-pi)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (make-raspi-bcm28-dtbs
            raspi-config-file
            raspi-custom-txt))

(define-public bcm2835
  (package
    (name "bcm2835")
    (version "1.64")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.airspayce.com/mikem/bcm2835/bcm2835-"
                    version ".tar.gz"))
              (sha256
               (base32
                "06s81540iz4vsh0cm6jwah2x0hih79v42pfa4pgr8kcbv56158h6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))    ; Would need to be root
    ;; doc/html docs would not be installed anyway.
    ;(native-inputs
    ; `(("doxygen" ,doxygen)))
    (synopsis "C library for Broadcom BCM 2835 as used in Raspberry Pi")
    (description "This package provides a C library for Broadcom BCM 2835 as
used in the Raspberry Pi")
    (home-page "https://www.airspayce.com/mikem/bcm2835/")
    (supported-systems '("armhf-linux" "aarch64-linux"))
    (license license:gpl3)))

(define-public raspi-gpio
  (let ((commit "6d0769ac04760b6e9f33b4aa1f11c682237bf368")
        (revision "1"))
    (package
      (name "raspi-gpio")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/RPi-Distro/raspi-gpio")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1fia1ma586hwhpda0jz86j6i55andq0wncbhzhzvhf7yc773cpi4"))))
      (build-system gnu-build-system)
      (synopsis "State dumper for BCM270x GPIOs")
      (description "Tool to help debug / hack at the BCM283x GPIO. You can dump
  the state of a GPIO or (all GPIOs). You can change a GPIO mode and pulls (and
  level if set as an output).  Beware this tool writes directly to the BCM283x
  GPIO reisters, ignoring anything else that may be using them (like Linux
  drivers).")
      (home-page "https://github.com/RPi-Distro/raspi-gpio")
      (supported-systems '("armhf-linux" "aarch64-linux"))
      (license license:bsd-3))))

(define %rpi-open-firmware-version "0.1")
(define %rpi-open-firmware-origin
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/librerpi/rpi-open-firmware")
         (commit "6be45466e0be437a1b0b3512a86f3d9627217006")))
   (file-name "rpi-open-firmware-checkout")
   (sha256
    (base32 "1wyxvv62i3rjicg4hd94pzbgpadinnrgs27sk39md706mm0qixbh"))))

(define-public raspi-arm-chainloader
  (package
    (name "raspi-arm-chainloader")
    (version %rpi-open-firmware-version)
    (source %rpi-open-firmware-origin)
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                   ; No tests exist
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'setenv
           (lambda _
             (setenv "CC" "arm-none-eabi-gcc")
             (setenv "CXX" "arm-none-eabi-g++")
             (setenv "AS" "arm-none-eabi-as")
             (setenv "OBJCOPY" "arm-none-eabi-objcopy")
             (setenv "BAREMETAL" "1")
             #t))
         (add-after 'setenv 'build-tlsf
           (lambda _
             (with-directory-excursion "tlsf"
               ;; Note: Adding "-I../common -I../notc/include".
               (invoke "make"
                       "CFLAGS=-mtune=arm1176jzf-s -march=armv6zk -mfpu=vfp -mfloat-abi=softfp -I../common -I../notc/include"))))
         (add-after 'build-tlsf 'build-common
           (lambda _
             (with-directory-excursion "common"
               (invoke "make"
                       ;; Note: Adding "-I.. -I../notc/include".
                       "ARMCFLAGS=-mtune=arm1176jzf-s -march=armv6zk -marm -I.. -I../notc/include"))))
         (add-after 'build-common 'build-notc
           (lambda _
             (with-directory-excursion "notc"
               (invoke "make"))))
         (add-after 'build-notc 'chdir
           (lambda _
             (chdir "arm_chainloader")
             (substitute* "Makefile"
              (("-I[.][.]/")
               "-I../common -I../common/include -I../notc/include -I../")
              (("-ltlsf")
               "-L../common -L../notc -L../tlsf -ltlsf"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libexec (string-append out "/libexec")))
               (mkdir-p libexec)
               (install-file "build/arm_chainloader.elf" libexec)
               (install-file "build/arm_chainloader.map" libexec)
               (install-file "build/arm_chainloader.bin" libexec)
               #t))))))
    (native-inputs
     `(("binutils" ,(cross-binutils "arm-none-eabi"))
       ("gcc" ,(make-gcc-arm-none-eabi-6))))
    (inputs
     `())
    (synopsis "Raspberry Pi ARM bootloader")
    (description "This package provides a bootloader for the ARM part of a
Raspberry Pi.  Note: It does not work on Raspberry Pi 1.")
    (home-page "https://github.com/librerpi/rpi-open-firmware/")
    (license license:gpl2+)))

(define-public raspi-arm64-chainloader
  (package
    (inherit raspi-arm-chainloader)
    (name "raspi-arm64-chainloader")
    ;; These native-inputs especially don't contain a libc.
    (native-inputs
     `(("bash" ,bash)
       ("binutils" ,binutils)
       ("coreutils" ,coreutils)
       ("file" ,file)
       ("ld-wrapper" ,ld-wrapper)
       ("make" ,gnu-make)
       ("gcc" ,gcc-6)
       ("locales" ,(libc-utf8-locales-for-target))))
    (inputs
     `())
    (arguments
     `(#:implicit-inputs? #f
       ,@(substitute-keyword-arguments (package-arguments raspi-arm-chainloader)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'setenv
               (lambda _
                 (setenv "AS" "as") ; TODO: as-for-target
                 (setenv "OBJCOPY" "objcopy")
                 (setenv "CC" ,(cc-for-target))
                 (setenv "CXX" ,(cc-for-target))
                 (setenv "BAREMETAL" "1")
                 #t))
             (add-after 'setenv 'build-tlsf
               (lambda _
                 (with-directory-excursion "tlsf"
                   (invoke "make"
                           "CFLAGS=-I../common -I../notc/include"))))
             (replace 'build-common
               (lambda _
                 (with-directory-excursion "common"
                   ;; Autodetection uses the CC filename for detecting the architecture.
                   ;; Since we are not using a cross-compiler, we side-step that.
                   (invoke "make"
                           "CFLAGS=-Ilib -I. -Iinclude -ffunction-sections -Wall -g -nostdlib -nostartfiles -ffreestanding -DBAREMETAL"))))
             (replace 'build-notc
               (lambda _
                 (with-directory-excursion "notc"
                   ;; Autodetection uses the CC filename for detecting the architecture.
                   ;; Since we are not using a cross-compiler, we side-step that.
                   (invoke "make"
                           "CFLAGS=-Iinclude -g"))))
             (replace 'chdir
               (lambda _
                 (chdir "arm64")
                 (substitute* "Makefile"
                  (("CFLAGS =")
                   "CFLAGS = -I../common -I../common/include -I../notc/include -I.. -DBAREMETAL")
                  (("-lcommon")
                   "-L../common -L../notc -lcommon"))
                 #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libexec (string-append out "/libexec")))
               (mkdir-p libexec)
               (install-file "arm64.elf" libexec)
               (install-file "arm64.map" libexec)
               (install-file "arm64.bin" libexec)
               #t))))))))
    (supported-systems '("aarch64-linux"))))

(define (raspi-config-file name content)
  "Make a configuration file like config.txt for the Raspberry Pi firmware.
CONTENT can be a list of strings, which are concatenated with a newline
character.  Alternatively CONTENT can be a string with the full file content."
  (plain-file
   name
   (if (list? content)
       (string-join content "\n" 'suffix)
       content)))

(define-public %raspi-config-txt
  ;; A config.txt file to start the ARM cores up in 64-bit mode if necessary
  ;; and to include a dtb.txt, bootloader.txt, and a custom.txt, each with
  ;; separated configurations for the Raspberry Pi firmware.
  (raspi-config-file
   "config.txt"
   `("# See https://www.raspberrypi.org/documentation/configuration/config-txt/README.md for details."
     ""
     ,(string-append "arm_64bit=" (if (target-aarch64?) "1" "0"))
     "include dtb.txt"
     "include bootloader.txt"
     "include custom.txt")))

(define-public %raspi-bcm27-dtb-txt
  ;; A dtb.txt file to be included by the config.txt to ensure that the
  ;; downstream device tree files bcm27*.dtb will be used.
  (raspi-config-file
   "dtb.txt"
   "upstream_kernel=0"))

(define-public %raspi-bcm28-dtb-txt
  ;; A dtb.txt file to be included by the config.txt to ensure that the
  ;; upstream device tree files bcm28*.dtb will be used.
  ;; This also implies the use of the dtoverlay=upstream.
  (raspi-config-file
   "dtb.txt"
   "upstream_kernel=1"))

(define-public %raspi-u-boot-bootloader-txt
  ;; A bootloader.txt file to be included by the config.txt to load the
  ;; U-Boot bootloader.
  (raspi-config-file
   "bootloader.txt"
   '("dtoverlay=upstream"
     "enable_uart=1"
     "kernel=u-boot.bin")))

(define (raspi-custom-txt content)
  "Make a custom.txt file for the Raspberry Pi firmware.
CONTENT can be a list of strings, which are concatenated with a newline
character.  Alternatively CONTENT can be a string with the full file content."
  (raspi-config-file "custom.txt" content))

(define (make-raspi-bcm28-dtbs linux)
  "Make a package with the device-tree files for Raspberry Pi models from the
kernel LINUX."
  (package
    (inherit linux)
    (name "raspi-bcm28-dtbs")
    (source #f)
    (build-system copy-build-system)
    (arguments
     #~(list
        #:phases #~(modify-phases %standard-phases (delete 'unpack))
        #:install-plan
        (list (list (search-input-directory %build-inputs
                                            "lib/dtbs/broadcom/")
                    "." #:include-regexp '("/bcm....-rpi.*\\.dtb")))))
    (inputs (list linux))
    (synopsis "Device-tree files for a Raspberry Pi")
    (description
     (format #f "The device-tree files for Raspberry Pi models from ~a."
             (package-name linux)))))

(define-public grub-efi-bootloader-chain-raspi-64
  ;; A bootloader capable to boot a Raspberry Pi over network via TFTP or from
  ;; a local storage like a micro SD card.  It neither installs firmware nor
  ;; device-tree files for the Raspberry Pi.  It just assumes them to be
  ;; existing in boot/efi in the same way that some UEFI firmware with ACPI
  ;; data is usually assumed to be existing on PCs.  It creates firmware
  ;; configuration files and a bootloader-chain with U-Boot to provide an EFI
  ;; API for the final GRUB bootloader.  It also serves as a blue-print to
  ;; create an a custom bootloader-chain with firmware and device-tree
  ;; packages or files.
  (efi-bootloader-chain grub-efi-netboot-removable-bootloader
                        #:packages (list u-boot-rpi-arm64-efi-bin)
                        #:files (list %raspi-config-txt
                                      %raspi-bcm27-dtb-txt
                                      %raspi-u-boot-bootloader-txt)))

(define (make-raspi-defconfig arch defconfig sha256-as-base32)
  "Make for the architecture ARCH a file-like object from the DEFCONFIG file
with the hash SHA256-AS-BASE32.  This object can be used as the #:defconfig
argument of the function (modify-linux)."
  (make-defconfig
   (string-append
    ;; This is from commit 7838840 on branch rpi-5.18.y,
    ;; see https://github.com/raspberrypi/linux/tree/rpi-5.18.y/
    ;; and https://github.com/raspberrypi/linux/commit/7838840b5606a2051b31da4c598466df7b1c3005
    "https://raw.githubusercontent.com/raspberrypi/linux/7838840b5606a2051b31da4c598466df7b1c3005/arch/"
    arch "/configs/" defconfig)
   sha256-as-base32))

(define-public %bcm2709-defconfig
  (make-raspi-defconfig
   "arm" "bcm2709_defconfig"
   "1hcxmsr131f92ay3bfglrggds8ajy904yj3vw7c42i4c66256a79"))

(define-public %bcm2711-defconfig
  (make-raspi-defconfig
   "arm" "bcm2711_defconfig"
   "1n7g5yq0hdp8lh0x6bfxph2ff8yn8zisdj3qg0gbn83j4v8i1zbd"))

(define-public %bcm2711-defconfig-64
  (make-raspi-defconfig
   "arm64" "bcm2711_defconfig"
   "0k9q7qvw826v2hrp49xnxnw93pnnkicwx869chvlf7i57461n4i7"))

(define-public %bcmrpi3-defconfig
  (make-raspi-defconfig
   "arm64" "bcmrpi3_defconfig"
   "1bfnl4p0ddx3200dg91kmh2pln36w95y05x1asc312kixv0jgd81"))

(define-public raspberrypi-userland
  ;; There are no release nor tag; use the latest commit.
  (let ((revision "0")
        (commit "54fd97ae4066a10b6b02089bc769ceed328737e0"))
    (package
      (name "raspberrypi-userland")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/raspberrypi/userland")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01853x2kx36vcm1wd0p20v72kw2p4xhnzp36jivh06mhma9b3h2v"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:configure-flags #~(list (string-append "-DVMCS_INSTALL_PREFIX="
                                                 #$output))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-paths
              (lambda _
                (substitute* "interface/khronos/ext/egl_khr_image_client.c"
                  (("/opt/vc/lib/libvcsm.so")
                   (string-append #$output "/lib/libvcsm.so")))))
            (add-after 'unpack 'disable-hello_pi
              (lambda _
                ;; Do not build hello_pi, which installs 32 MiB of binaries
                ;; and source files to src/.
                (substitute* "host_applications/linux/CMakeLists.txt"
                  ((".*add_subdirectory\\(apps/hello_pi).*")
                   ""))
                (substitute* "makefiles/cmake/vmcs.cmake"
                  (("install.*host_applications/linux/apps/hello_pi" all)
                   (string-append "# " all))
                  ((".*DESTINATION \\$\\{VMCS_INSTALL_PREFIX}/src)" all)
                   (string-append "# " all))))))))
      (native-inputs (list pkg-config))
      (home-page "https://github.com/raspberrypi/userland/")
      (supported-systems (list "armhf-linux" "aarch64-linux"))
      (synopsis "Raspberry Pi GPU-related libraries")
      (description "This package package contains libraries to interface to
EGL, mmal, GLESv2, vcos, openmaxil, vchiq_arm, bcm_host, VFC and OpenVG.  It
also provides the @command{dtmerge}, @command{dtoverlay}, @command{dtparam},
@command{raspivid} and @command{tvservice} commands, among others.")
      (license license:bsd-3))))

(define-public rpi-fbcp
  ;; There are no release nor tag; use the latest commit.
  (let ((revision "0")
        (commit "af8d32246c23cb23e4030e6588668a14341f5ddc"))
    (package
      (name "rpi-fbcp")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tasanakorn/rpi-fbcp")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10wym2jckicxm5iwqgby6gbhkznyi1q8x41v0qahzv71x85xpsl5"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:phases
        #~(modify-phases %standard-phases
            (replace 'install
              ;; There is no installation target.
              (lambda _
                (install-file "fbcp" (string-append #$output "/bin")))))))
      (inputs (list raspberrypi-userland))
      (home-page "https://github.com/tasanakorn/rpi-fbcp")
      (synopsis "Mirror primary to secondary frame buffer on Raspberry Pi")
      (description "The @command{fbcp} command provided by this package can be
used to copy the primary frame buffer to the secondary frame buffer of a
Raspberry Pi.  It can for example mirror the primary HDMI output to a
secondary LCD display connected to the Raspberry Pi board.")
      (license license:expat))))

(define-public rpi-imager
  (package
    (name "rpi-imager")
    (version "1.8.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/raspberrypi/rpi-imager")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (srfi srfi-26)))
              (snippet '(begin
                          ;; Remove all but the following bundled libraries,
                          ;; which are not yet packaged in Guix.
                          (define keep '("." ".."
                                         "drivelist"
                                         "mountutils"
                                         "sha256crypt"))
                          (with-directory-excursion "src/dependencies"
                            (for-each delete-file-recursively
                                      (scandir "." (negate
                                                    (cut member <> keep)))))))
              (sha256
               (base32
                "1jr4w9h0bvqpy4r1g22n7b07zpplmc318v4lcfvh70c0rhl2vfi6"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:configure-flags #~(list "-DENABLE_TELEMETRY=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "src")))
          (add-after 'chdir 'customize-os-list.json
            ;; The default operating system JSON list contains non-FSDG
            ;; systems.
            (lambda _
              (let* ((datadir (string-append #$output
                                             "/share/rpi-imager"))
                     (os-list.json (string-append datadir "/os-list.json")))
                (mkdir-p datadir)
                #$(with-extensions (list guile-json-4)
                    #~(begin
                        (use-modules (json))
                        (call-with-output-file os-list.json
                          ;; TODO: Register FSDG and RPi compatible OS
                          ;; images here.
                          (lambda (port)
                            (scm->json '() port)))))
                (substitute* "config.h"
                  (("#define OSLIST_URL.*")
                   (string-append "#define OSLIST_URL \"file:///"
                                  os-list.json "\"\n"))))))
          (add-after 'chdir 'patch-cmake
            (lambda _
              (substitute* "CMakeLists.txt"
                ;; lsblk expects to have access to /sys/dev/block,
                ;; which doesn't exist in the build container;
                ;; prevent the check to fail the build.
                (("ret EQUAL \"1\"")
                 "FALSE")))))))
    (inputs
     (list gnutls
           curl
           libarchive
           qtdeclarative-5
           qtquickcontrols2-5
           qtsvg-5
           qttools-5
           util-linux))
    (home-page "https://github.com/raspberrypi/rpi-imager/")
    (synopsis "Raspberry Pi Imaging Utility")
    (description "rpi-imager is graphical utility to easily provision and
flash a memory card with an operating system image suitable for the Raspberry
Pi single board computer.")
    (license license:asl2.0)))

(define-public waveshare-dtoverlays
  (let ((commit "6ea99d4afb4776fdb008708f3f30df1de6fc24e3")
        (revision "0"))
    (package
      (name "waveshare-dtoverlays")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/swkim01/waveshare-dtoverlays")
                      (commit commit)))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                ;; Delete pre-compiled device tree overlay binary files.
                (snippet '(for-each delete-file (find-files "." "\\.dtbo$")))
                (sha256
                 (base32
                  "1c30wnlinicwlivlri25wns6x8nx7asf5fh2zqxkzr9h1jsxbzwz"))))
      (build-system gnu-build-system)
      (arguments (list #:tests? #f      ;no test suite
                       #:make-flags #~(list (string-append "PREFIX="
                                                           #$output))
                       #:phases #~(modify-phases %standard-phases
                                    (delete 'configure))))
      (native-inputs (list dtc))
      (home-page "https://github.com/swkim01/waveshare-dtoverlays/")
      (synopsis "Device tree overlays for WaveShare SpotPear TFT LCDs")
      (description "This package contains device tree overlay binaries to
support the WaveShare SpotPear @acronym{TFT, Thin-Film Transistor}
@acronym{LCDs, Liquid Crystal Display} on the Raspberry Pi.")
      (license license:gpl3+))))
