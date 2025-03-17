;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2016, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2018, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022, 2023, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages firmware)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avr)
  #:use-module (gnu packages avr-xyz)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mingw)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)

  #:export (make-ergodox-firmware
            make-qmk-firmware))

(define-public ath9k-htc-ar7010-firmware
  (package
    (name "ath9k-htc-ar7010-firmware")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/qca/open-ath9k-htc-firmware")
                    (commit version)))
              (modules '((guix build utils)))
              (snippet
                ;; Delete binary blobs.
                #~(for-each delete-file (find-files "." "\\.(a|o)$")))
              (sha256
               (base32
                "16jbj8avg5jkgvq5lxm0hdxxn4c3zn7fx8b4nxllvr024apk9w23"))
              (file-name (git-file-name "open-ath9k-htc-firmware" version))
              (patches (search-patches "ath9k-htc-firmware-gcc-compat.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list #:target "xtensa-ath9k-elf"
           #:tests? #f
           #:configure-flags
           #~'("-DCMAKE_C_FLAGS=-Wno-error=implicit-function-declaration"
               "-DTARGET_MAGPIE=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'change-directory
                 (lambda _
                   (chdir "target_firmware")))
               (replace 'install
                 (lambda _
                   (let ((fw-dir (string-append #$output "/lib/firmware")))
                     (for-each (lambda (file)
                                 (install-file file fw-dir))
                               (find-files "." "\\.fw$"))))))))
    (native-inputs (list perl))
    (home-page "https://wireless.wiki.kernel.org/en/users/Drivers/ath9k_htc")
    (synopsis "Firmware for the Atheros AR7010 USB 802.11n NICs")
    (description
     "This is the firmware for the Qualcomm Atheros AR7010 802.11n USB NICs
(aka Wi-Fi USB dongle).  It is used by the ath9k driver of Linux-libre.")
    (license (license:non-copyleft "http://directory.fsf.org/wiki/License:ClearBSD"))))

(define-public ath9k-htc-ar9271-firmware
  (package
    (inherit ath9k-htc-ar7010-firmware)
    (name "ath9k-htc-ar9271-firmware")
    (arguments
     (substitute-keyword-arguments
       (package-arguments ath9k-htc-ar7010-firmware)
       ((#:configure-flags flags)
        #~'("-DCMAKE_C_FLAGS=-Wno-error=implicit-function-declaration"
            "-DTARGET_K2=ON"))))
    (synopsis "Firmware for the Atheros AR9271 USB 802.11n NICs")
    (description
     "This is the firmware for the Qualcomm Atheros AR9271 802.11n USB NICs
(aka Wi-Fi USB dongle).  It is used by the ath9k driver of Linux-libre.")))

(define-public b43-tools
  (let ((commit "27892ef741e7f1d08cb939744f8b8f5dac7b04ae")
        (revision "1"))
    (package
      (name "b43-tools")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.bues.ch/git/b43-tools.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1wgmj4d65izbhprwb5bcwimc2ryv19b9066lqzy4sa5m6wncm9cn"))))
      (build-system gnu-build-system)
      (native-inputs
       (list flex bison))
      (arguments
       `(#:modules ((srfi srfi-1)
                    (guix build gnu-build-system)
                    (guix build utils))
         #:tests? #f                    ; no tests
         #:make-flags `(,(string-append "PREFIX=" (assoc-ref %outputs "out"))
                        ,(string-append "CC=" ,(cc-for-target)))
         #:phases
         (let ((subdirs '("assembler" "disassembler")))
           (modify-phases %standard-phases
             (delete 'configure)        ; no configure script
             (add-before 'build 'patch-/bin/true
               (lambda _
                 (substitute* (find-files "." "Makefile")
                   (("/bin/true") ":"))))
             (replace 'build
               (lambda* (#:key (make-flags '()) #:allow-other-keys)
                 (for-each (lambda (dir)
                             (apply invoke "make" "-C" dir make-flags))
                           subdirs)))
             (replace 'install
               (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (mkdir-p (string-append out "/bin"))
                   (for-each (lambda (dir)
                               (apply invoke "make" "-C" dir "install" make-flags))
                             subdirs))))))))
      (home-page
       "https://bues.ch/cms/hacking/misc.html#linux_b43_driver_firmware_tools")
      (synopsis "Collection of tools for the b43 wireless driver")
      (description
       "The b43 firmware tools is a collection of firmware extractor,
assembler, disassembler, and debugging tools for the Linux kernel b43 wireless
driver.")
      (license license:gpl2))))

(define-public fwupd
  (package
    (name "fwupd")
    (version "1.8.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fwupd/fwupd")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "179yc0nbbyrdya5q16ncf7lkslrhr3i90rgb9vdmv751ikilkby6"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--wrap-mode=nofallback"
                                "-Dsystemd=false"
                                (string-append "-Defi_os_dir="
                                               #$gnu-efi "/lib")
                                "-Defi_binary=false"
                                (string-append "-Dudevdir="
                                               #$output "/lib/udev")
                                "--localstatedir=/var"
                                (string-append "--libexecdir="
                                               #$output "/libexec")
                                "-Dsupported_build=true"
                                ;; Disable LVFS, because it contains
                                ;; nonfree firmwares.
                                "-Dlvfs=disabled")
      #:glib-or-gtk? #t               ;To wrap binaries and/or compile schemas
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-source-writable
            (lambda _
              (for-each make-file-writable
                        (find-files "."))
              (substitute* "src/fu-self-test.c"
                (("/bin/sh")
                 (which "sh")))))
          ;; These two files are zipped by Python, so need a newer timestamp.
          (add-after 'unpack 'newer-timestamps-for-python-zip
            (lambda _
              (let ((circa-1980 (* 10 366 24 60 60)))
                (for-each (lambda (file)
                            (make-file-writable file)
                            (utime file circa-1980 circa-1980))
                          '("./libfwupdplugin/tests/colorhug/firmware.bin"
                            "./libfwupdplugin/tests/colorhug/firmware.bin.asc")))))
          (add-before 'build 'setup-home
            (lambda _
              (setenv "HOME" "/tmp")))
          (add-before 'install 'no-polkit-magic
            (lambda _
              (setenv "PKEXEC_UID" "something")))
          (add-after 'install 'ensure-all-remotes-are-disabled
            ;; Because no remote currently promises to offer only free
            ;; software firmwares, disable them to prevent a user to
            ;; unknowingly install proprietary firmware updates.
            (lambda _
              (substitute* (find-files (string-append #$output "/etc")
                                       "\\.conf$")
                (("Enabled=true")
                 "Enabled=false")))))))
    (native-inputs (list gobject-introspection
                         python-pygobject
                         python-pillow
                         python-pycairo
                         python
                         pkg-config
                         vala
                         gtk-doc
                         which
                         umockdev
                         `(,glib "bin")
                         help2man
                         gettext-minimal))
    (inputs (append
             (list bash-completion
                   libgudev
                   libxmlb
                   sqlite
                   polkit
                   eudev
                   libelf
                   tpm2-tss
                   cairo
                   efivar
                   pango
                   protobuf-c
                   mingw-w64-tools
                   gnu-efi)
             (if (supported-package? libsmbios
                                     (or (and=> (%current-target-system)
                                                platform-target->system)
                                         (%current-system)))
                 (list libsmbios)
                 '())))
    ;; In Requires of fwupd*.pc.
    (propagated-inputs (list curl
                             gcab
                             glib
                             gnutls
                             gusb
                             json-glib
                             libarchive
                             libjcat))
    (home-page "https://fwupd.org/")
    (synopsis "Daemon to allow session software to update firmware")
    (description "This package aims to make updating firmware on GNU/Linux
automatic, safe and reliable.  It is used by tools such as GNOME Software.")
    (license license:lgpl2.1+)))

(define-public openfwwf-firmware
  (package
    (name "openfwwf-firmware")
    (version "5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://netweb.ing.unibs.it/~openfwwf/firmware/"
                           "openfwwf-" version ".tar.gz"))
       (sha256
        (base32
         "1p60gdi7w88s7qw82d3g9v7mk887mhvidf4l5q5hh09j10h37q4x"))))
    (build-system gnu-build-system)
    (native-inputs
     (list b43-tools))
    (arguments
     `(#:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")
                                         "/lib/firmware/b43-open"))
       #:target #f                      ; Package produces firmware.
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "http://netweb.ing.unibs.it/~openfwwf/")
    (synopsis "Firmware for BCM43xx devices")
    (description
     "This is firmware from Open FirmWare for WiFi networks (OpenFWWF) for the
Broadcom/AirForce chipset BCM43xx with Wireless-Core Revision 5.  It is used
by the b43-open driver of Linux-libre.")
    (license license:gpl2)))

(define-public eg25-manager
  (package
    (name "eg25-manager")
    (version "0.4.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/mobian1/devices/eg25-manager")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a591dhr43mhwh09n2vlfpw6aajl6d1vkwniikjvwfjrmp01v6yq"))))
    (build-system meson-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'patch-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "udev/80-modem-eg25.rules"
                 (("/bin/grep") (search-input-file inputs "/bin/grep"))))))))
    (native-inputs (list curl
                         `(,glib "bin") pkg-config))
    (inputs (list grep libgpiod libgudev libusb))
    (synopsis "Manager daemon for the Quectel EG25 mobile broadband modem")
    (description
     "This package provides a manager daemon for the Quectel EG25 mobile
broadband modem as found, for example, on PinePhone.")
    (home-page "https://gitlab.com/mobian1/devices/eg25-manager")
    (license license:gpl3+)))

(define-public fcode-utils
  (package
    (name "fcode-utils")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openbios/fcode-utils")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yyqmiqvlf644jrv8x39aqdqywdnm80k62d2assgcammwbc7krya"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "tests"
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "STRIP=" #$(strip-for-target))
              (string-append "DESTDIR=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))        ; No configure script.
    (native-inputs
     (list tcsh))
    (home-page "https://www.openfirmware.info/FCODE_suite")
    (synopsis "Utilities to process FCODE, OpenFirmware's byte code")
    (description "This is the OpenBIOS FCODE suite.  It contains a set of
utilities used to process FCODE, OpenFirmware's byte code, consisting of:
@enumerate
@item toke - A tokenizer
@item detok - A detokenizer
@item romheaders - A PCI rom header utility
@item localvalues - A portable implementation of Forth local values
@end enumerate")
    (license (list license:gpl2
                   ;; localvalues implementation and some documentation.
                   license:cpl1.0))))

(define* (make-openbios-package name arch)
  (let ((target (cond
                 ((string-suffix? "ppc" arch)
                  "powerpc-linux-gnu")
                 ((string-suffix? "amd64" arch)
                  "x86_64-linux-gnu")
                 ((string-suffix? "x86" arch)
                  "i686-linux-gnu")
                 (else (string-append arch "-linux-gnu"))))
        ;; 1.1 was released in May 2013.
        (commit "af97fd7af5e7c18f591a7b987291d3db4ffb28b5")
        (revision "1"))
  (package
    (name name)
    (version (git-version "1.1" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openbios/openbios")
                    (commit commit)))
              (file-name (git-file-name "openbios" version))
              (sha256
               (base32
                "1xp1b6xgx40i0j3a5y3id0d1p8vdvapai8szganxg3zrvj53fh0n"))
              (patches (search-patches "openbios-aarch64-riscv64-support.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no tests
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'build-reproducibly
                 (lambda _
                   (substitute* "Makefile.target"
                     (("TZ=UTC date \\+")
                      "TZ=UTC date --date=@1 +"))))
               (replace 'configure
                 (lambda* (#:key (configure-flags #~'()) #:allow-other-keys)
                   (apply invoke "./config/scripts/switch-arch" #$arch
                          configure-flags)))
               (replace 'install
                 (lambda _
                   (let ((build-target
                          (if (string-contains #$arch "-")
                              (car (reverse (string-split #$arch #\-)))
                              #$arch)))
                     (for-each (lambda (elf)
                                 (install-file elf
                                               (string-append #$output
                                                              "/share/firmware")))
                               (find-files (string-append "obj-" build-target)
                                           "\\.elf$"))))))))
    (native-inputs
     (append (if (string-prefix? (%current-system) target)
                 (list gcc-10)
                 (list (cross-gcc target #:xgcc gcc-10) (cross-binutils target)))
             (list fcode-utils libxslt which)))
    (home-page "https://openfirmware.info/Welcome_to_OpenBIOS")
    (synopsis "Open Firmware implementation")
    (description
     "OpenBIOS is an implementation of the IEEE 1275-1994 \"Open Firmware\"
specification.  It can be used as a system firmware, as a boot loader, or
provide OpenFirmware functionality on top of an already running system.")
    ;; Some files are GPLv2 only.
    (license license:gpl2))))

(define-public openbios-qemu-ppc
  (let ((base (make-openbios-package "openbios-qemu-ppc" "qemu-ppc")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ;; No need to cross-compile, package produces reproducible firmware.
         ((#:target _ #f) #f)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'install 'rename-executable
                (lambda _
                  (with-directory-excursion #$output
                    (rename-file "share/firmware" "share/qemu")
                    (rename-file "share/qemu/openbios-qemu.elf"
                                 "share/qemu/openbios-ppc")))))))))))

(define* (make-opensbi-package platform name #:optional (arch "riscv64"))
  (package
    (name name)
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/riscv-software-src/opensbi")
             (commit (string-append "v" version))))
       (file-name (git-file-name "opensbi" version))
       (sha256
        (base32 "0mfjb9jzrmc6chsr16bjrfann67qjxiigz8q42ndf9lrp6nyigd9"))))
    (build-system gnu-build-system)
    (native-inputs
     (append
       (if (and (not (string-prefix? "riscv64" (%current-system)))
                (string-prefix? "riscv64" arch))
         (list (cross-gcc "riscv64-linux-gnu")
               (cross-binutils "riscv64-linux-gnu"))
         '())
       (list python)))
    (arguments
     `(#:tests? #f ; no check target
       #:target #f ; Package produces firmware.
       #:make-flags (list (string-append "PLATFORM=" ,platform)
                          ,@(if (and (not (string-prefix? "riscv64"
                                                          (%current-system)))
                                     (string-prefix? "riscv64" arch))
                                `("CROSS_COMPILE=riscv64-linux-gnu-")
                                `("CC=gcc"))
                          "FW_PAYLOAD=n"
                          "V=1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bin (find-files "." "fw_.*\\.(elf|bin)$")))
               (for-each
                 (lambda (file)
                   (install-file file out))
                 bin)))))))
    (home-page "https://github.com/riscv-software-src/opensbi")
    (synopsis "RISC-V @acronym{SBI, Supervisor Binary Interface} implementation")
    (description
     "OpenSBI is the reference implementation of the RISC-V @acronym{SBI,
Supervisory Binary Interface} specifications for platform-specific firmwares
executing in M-mode.")
    (license (list license:bsd-2
                   ;; lib/utils/libfdt/* is dual licensed under bsd-2 and gpl2+.
                   license:gpl2+
                   ;; platform/ariane-fpga/* is gpl2.
                   license:gpl2))))

(define-public opensbi-generic
  (make-opensbi-package "generic" "opensbi-generic"))

(define-public opensbi-qemu
  (package
    (inherit opensbi-generic)
    (name "opensbi-qemu")
    (native-inputs '())
    (inputs (list opensbi-generic))
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules ((guix build utils)))
               (let ((opensbi-riscv64 (search-input-file %build-inputs
                                                         "fw_dynamic.bin"))
                     (out (string-append #$output "/share/qemu")))
                 (mkdir-p out)
                 (symlink opensbi-riscv64
                          (string-append
                           out "/opensbi-riscv64-generic-fw_dynamic.bin"))))))
    (synopsis "OpenSBI firmware files for QEMU")
    (description
     "This package contains OpenSBI firmware files for use with QEMU.")))

(define-public opensbi-for-visionfive2
  (package
    (inherit opensbi-generic)
    (name "opensbi-for-visionfive2")
    (arguments
     (substitute-keyword-arguments
         (package-arguments opensbi-generic)
       ((#:make-flags flags)
        ;; visionfive2's u-boot need opensbi with the following flags.
        ;; see https://docs.u-boot.org/en/latest/board/starfive/visionfive2.html
        #~(cons* "FW_TEXT_START=0x40000000"
                 "FW_OPTIONS=0"
                 #$flags))))))

(define-public seabios
  (package
    (name "seabios")
    (version "1.16.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://review.coreboot.org/seabios.git")
             (commit (string-append "rel-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mal2zqn4ppxdjxddrxcphm6z9n8n4rw97xl2hldd7spw57nwq97"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Delete IASL-generated files.
            (for-each delete-file (find-files "." "\\.hex$"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list acpica python-wrapper))
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:target #f                       ; Package produces firmware.
      #:make-flags
      ;; If EXTRAVERSION is not set the build system will embed the current
      ;; date in binaries.  Use this opportunity to personalize as recommended
      ;; by Build_overview.md.
      #~'("EXTRAVERSION=/GNU Guix"
          "V=1")                        ;build verbosely
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              ;; Ensure this file is present in case we're building from a git
              ;; checkout instead of release tarball.
              (call-with-output-file ".version"
                (lambda (port)
                  (format port #$(package-version this-package))))
              ;; If we use (cc-for-target) then we have the system prefix
              ;; twice or we might have the wrong prefix.
              (setenv "CC" "gcc")))
          (add-before 'build 'build-description-tables
            (lambda _
              ;; Regenerate the ACPI description tables.
              (invoke "make" "iasl")
              ;; Clear temporary files added by the iasl target.
              (invoke "make" "clean")))
          (replace 'install
            (lambda _
              (install-file "out/bios.bin"
                            (string-append #$output "/share/firmware")))))))
    (home-page "https://www.seabios.org/SeaBIOS")
    (synopsis "BIOS implementation")
    (description "SeaBIOS is an implementation of a 16bit x86 BIOS.  SeaBIOS
can run in an emulator or it can run natively on X86 hardware with the use of
coreboot.")
    (supported-systems '("i686-linux" "x86_64-linux"))
    ;; Dual licensed.
    (license (list license:gpl3+ license:lgpl3+
                   ;; src/fw/acpi-dsdt.dsl is lgpl2
                   license:lgpl2.1
                   ;; src/fw/lzmadecode.c and src/fw/lzmadecode.h are lgpl3+ and
                   ;; cpl with a linking exception.
                   license:cpl1.0))))

(define-public seabios-qemu
  (package/inherit seabios
    (name "seabios-qemu")
    (native-inputs
     (if (member (%current-system) '("i686-linux" "x86_64-linux"))
         (package-native-inputs seabios)
         (modify-inputs (package-native-inputs seabios)
           (prepend (cross-gcc "i686-linux-gnu")
                    (cross-binutils "i686-linux-gnu")))))
    (supported-systems %supported-systems)
    (arguments
     (substitute-keyword-arguments (package-arguments seabios)
       ((#:modules modules %default-gnu-modules)
        `((ice-9 match)
          (ice-9 threads)
          ,@modules))
       ((#:phases phases)
        #~(modify-phases #$phases
            #$@(match (%current-system)
                 ((or "i686-linux" "x86_64-linux")
                  #~())
                 (_
                  #~((add-after 'configure 'configure-cross
                       (lambda _
                         (substitute* "Makefile"
                           (("CROSS_PREFIX=")
                            "CROSS_PREFIX=i686-linux-gnu-")))))))
            (replace 'build
              (lambda* (#:key (make-flags #~'()) #:allow-other-keys)
                ;; Note: These BIOS configurations are taken from QEMUs roms/
                ;; directory.
                (let ((biosen
                       '( ;; The standard BIOS using default options.
                         ("bios-256k" . ("QEMU=y" "ROM_SIZE=256" "ATA_DMA=n"))
                         ;; A minimal BIOS for old QEMU machine types.
                         ("bios-128k"
                          . ("QEMU=y" "ROM_SIZE=128" "ATA_DMA=n" "BOOTSPLASH=n"
                             "XEN=n" "USB_OHCI=n" "USB_XHCI=n" "USB_UAS=n"
                             "SDCARD=n" "TCGBIOS=n" "MPT_SCSI=n" "ESP_SCSI=n"
                             "MEGASAS=n" "PVSCSI=n" "NVME=n" "USE_SMM=n"
                             "VGAHOOKS=n" "HOST_BIOS_GEOMETRY=n" "ACPI_PARSE=n"))
                         ;; Minimal BIOS for the "microvm" machine type.
                         ("bios-microvm"
                          . ("QEMU=y" "ROM_SIZE=128" "XEN=n" "BOOTSPLASH=n"
                             "ATA=n" "AHCI=n" "SDCARD=n" "PVSCSI=n" "ESP_SCSI=n"
                             "LSI_SCSI=n" "MEGASAS=n" "MPT_SCSI=n" "FLOPPY=n"
                             "FLASH_FLOPPY=n" "NVME=n" "PS2PORT=n" "USB=n"
                             "LPT=n" "RTC_TIMER=n" "USE_SMM=n" "PMTIMER=n"
                             "TCGBIOS=n" "HARDWARE_IRQ=n" "ACPI_PARSE=y"))))
                      (vgabiosen
                       '(("ati"    . ("VGA_ATI=y" "VGA_PCI=y"))
                         ("bochs-display" . ("DISPLAY_BOCHS=y" "VGA_PCI=y"))
                         ("cirrus" . ("VGA_CIRRUS=y" "VGA_PCI=y"))
                         ("stdvga" . ("VGA_BOCHS=y" "VGA_PCI=y"))
                         ("virtio" . ("VGA_BOCHS=y" "VGA_BOCHS_VIRTIO=y"
                                      "VGA_PCI=y"))
                         ("vmware" . ("VGA_BOCHS=y" "VGA_BOCHS_VMWARE=y"
                                      "VGA_PCI=y"))
                         ("qxl"    . ("VGA_BOCHS=y" "VGA_BOCHS_QXL=y"
                                      "VGA_PCI=y"))
                         ("isavga" . ("VGA_BOCHS=y" "VGA_PCI=n"))
                         ("ramfb"  . ("VGA_RAMFB=y" "VGA_PCI=n")))))
                  (mkdir "out")
                  (n-par-for-each
                   (parallel-job-count)
                   (match-lambda
                     ((target . config)
                      (let* ((dot-config (string-append (getcwd) "/" target
                                                        "/.config"))
                             (flags (append
                                     make-flags
                                     (list (string-append "KCONFIG_CONFIG="
                                                          dot-config)
                                           (string-append "OUT=" target "/")))))
                        (mkdir target)
                        (call-with-output-file dot-config
                          (lambda (port)
                            (for-each (lambda (entry)
                                        (format port "CONFIG_~a~%" entry))
                                      config)))
                        (apply invoke "make" "oldnoconfig" flags)
                        (apply invoke "make" flags)
                        (link (string-append target "/"
                                             (if (string-prefix? "vgabios" target)
                                                 "vgabios.bin" "bios.bin"))
                              (string-append "out/" target ".bin")))))
                   (append biosen
                           (map (lambda (pair)
                                  `(,(string-append "vgabios-" (car pair))
                                    .
                                    ,(cons "BUILD_VGABIOS=y" (cdr pair))))
                                vgabiosen))))))
            (replace 'install
              (lambda _
                (let ((firmware (string-append #$output "/share/qemu")))
                  (for-each (lambda (bios)
                              (install-file bios firmware))
                            (find-files "out" "\\.bin$"))
                  (with-directory-excursion firmware
                    ;; Compatibility symlinks for QEMU.
                    (symlink "bios-128k.bin" "bios.bin")
                    (symlink "vgabios-isavga.bin" "vgabios.bin")))))))))))

(define-public sgabios
  ;; There are no tags in the repository.
  (let ((commit "72f39d48bedf044e202fd51fecf3e2218fc2ae66")
        (revision "0"))
    (package
      (name "sgabios")
      (version (git-version "0.0" revision commit))
      (home-page "https://gitlab.com/qemu-project/sgabios")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ybl021i0xaz18wzq4q13ifypy5b3dj8m11c8m0qdiq00g06vm0i"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags
             #~'(#$@(if (member (%current-system) '("i686-linux" "x86_64-linux"))
                        #~("CC=gcc")
                        #~("CC=i686-linux-gnu-gcc"
                           "LD=i686-linux-gnu-ld"
                           "OBJCOPY=i686-linux-gnu-objcopy"))
                     "HOSTCC=gcc")
             #:parallel-build? #f
             #:tests? #f   ;no tests
             #:target #f   ; Package produces firmware.
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'build-reproducibly
                   (lambda _
                     (substitute* "Makefile"
                       (("BUILD_DATE = .*")
                        "BUILD_DATE = \\\"Jan 1 1970\\\"\n")
                       (("BUILD_SHORT_DATE = .*")
                        "BUILD_SHORT_DATE = \\\"1/1/70\\\"\n"))))
                 (delete 'configure)
                 (replace 'install
                   (lambda _
                     (install-file "sgabios.bin"
                                   (string-append #$output "/share/qemu")))))))
      (native-inputs
       (if (member (%current-system) '("i686-linux" "x86_64-linux"))
           '()
           (list (cross-gcc "i686-linux-gnu")
                 (cross-binutils "i686-linux-gnu"))))
      (synopsis "Serial graphics adapter BIOS")
      (description
       "SGABIOS provides a means for legacy PC software to communicate with an
attached serial console as if a VGA card is attached.  It is designed to be
inserted into a BIOS as an option ROM to provide over a serial port the display
and input capabilities normally handled by a VGA adapter and a keyboard, and
additionally provide hooks for logging displayed characters for later collection
after an operating system boots.")
      (license license:asl2.0))))

(define-public edk2-tools
  (package
    (name "edk2-tools")
    (version "202402")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tianocore/edk2")
                    (commit (string-append "edk2-stable" version))
                    ;; EDK2 makes extensive use of submodules.
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y7jfpijgi099znhzjklnsczn0k0vm1d1qznq9x2a2sa0glydsin"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "BUILD_CC=" #$(cc-for-target))
                   (string-append "CC=" #$(cc-for-target)))
           #:test-target "Tests"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'change-directory
                 (lambda _
                   (chdir "BaseTools")))
               (replace 'build
                 (lambda* (#:key (make-flags #~'()) #:allow-other-keys)
                   ;; The default build target also runs tests.
                   (apply invoke "make" "-C" "Source/C" make-flags)))
               (delete 'configure)
               (replace 'install
                 (lambda _
                   (mkdir #$output)
                   (copy-recursively "Source/C/bin"
                                     (string-append #$output "/bin")))))))
    (native-inputs
     (list python-wrapper))
    (inputs
     (list `(,util-linux "lib")))       ;for libuuid
    (home-page
     "https://github.com/tianocore/tianocore.github.io/wiki/EDK-II-Tools-List")
    (synopsis "EFI development tools")
    (description
     "This package contains tools for processing UEFI firmware content.
Executables included are:

@itemize
@item @code{EfiRom}: Build Option ROM images.
@item @code{GenFfs}: Generate FFS files.
@item @code{GenFv}: Generate a PI firmware volume image.
@item @code{GenFw}: Get image data from PE32 files.
@item @code{GenSec}: Generate EFI_SECTION type files.
@item @code{VfrCompile}: Parse preprocessed UEFI and Framework VFR files.
@item @code{VolInfo}: Display the contents of a firmware volume.
@end itemize")
    ;; See BaseTools/Source/C/GNUmakefile
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux"
                         "aarch64-linux" "riscv64-linux"))
    (license license:bsd-2)))

(define* (make-ovmf-firmware arch)
  (let ((toolchain "GCC")
        (arch-string (match arch
                           ("x86_64" "X64")
                           ("i686" "IA32")
                           ("aarch64" "AARCH64")
                           ("armhf" "ARM")
                           ("riscv64" "RISCV64")
                           ("loongarch64" "LOONGARCH64")
                           (_ "NONE"))))
    (package
      (inherit edk2-tools)
      (name (string-append "ovmf-" arch))
      (arguments
       (list
        #:tests? #f                     ; No check target.
        #:target #f                     ; Package produces firmware.
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 match))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-source
              (lambda _
                (substitute* "edksetup.sh"
                  (("^return \\$\\?")
                   "exit $?"))))
            (add-before 'configure 'set-env
              (lambda _
                (unless (string-prefix? #$arch #$(%current-system))
                  (setenv (string-append #$toolchain "_X64_PREFIX")
                          "x86_64-linux-gnu-")
                  (setenv (string-append #$toolchain "_IA32_PREFIX")
                          "i686-linux-gnu-")
                  (setenv (string-append #$toolchain "_AARCH64_PREFIX")
                          "aarch64-linux-gnu-")
                  (setenv (string-append #$toolchain "_ARM_PREFIX")
                          "arm-linux-gnueabihf-")
                  (setenv (string-append #$toolchain "_RISCV64_PREFIX")
                          "riscv64-linux-gnu-")
                  (setenv (string-append #$toolchain "_LOONGARCH64_PREFIX")
                          "loongarch64-linux-gnu-"))))
            (replace 'configure
              (lambda _
                (let* ((cwd (getcwd))
                       (tools (string-append cwd "/BaseTools"))
                       (bin (string-append tools "/BinWrappers/PosixLike")))
                  (setenv "WORKSPACE" cwd)
                  (setenv "EDK_TOOLS_PATH" tools)
                  (setenv "PYTHON3_ENABLE" "TRUE")
                  (setenv "PYTHON_COMMAND" "python3")
                  (setenv "PATH" (string-append (getenv "PATH") ":" bin))
                  (invoke "bash" "edksetup.sh")
                  (substitute* "Conf/target.txt"
                    (("^TARGET[ ]*=.*$") "TARGET = RELEASE\n")
                    (("^TOOL_CHAIN_TAG[ ]*=.*$")
                     (string-append "TOOL_CHAIN_TAG = " #$toolchain "\n"))
                    (("^TARGET_ARCH[ ]*=.*$")
                     (string-append "TARGET_ARCH = " #$arch-string
                                    "\n"))
                    (("^MAX_CONCURRENT_THREAD_NUMBER[ ]*=.*$")
                     (format #f "MAX_CONCURRENT_THREAD_NUMBER = ~a~%"
                             (number->string (parallel-job-count)))))
                  ;; Build build support.
                  (setenv "CC" "gcc")
                  (invoke "make" "-C" tools))))
            (replace 'build
              (lambda _
                (invoke "build" "-a" #$arch-string "-t" #$toolchain "-p"
                        (match #$arch
                               ("x86_64"
                                "OvmfPkg/OvmfPkgX64.dsc")
                               ("i686"
                                "OvmfPkg/OvmfPkgIa32.dsc")
                               ((or "aarch64" "armhf")
                                "ArmVirtPkg/ArmVirtQemu.dsc")
                               ("riscv64"
                                "OvmfPkg/RiscVVirt/RiscVVirtQemu.dsc")
                               (_ #t)))))
            (add-before 'install 'install-efi-shell
              (lambda _
                (let ((fmw (string-append #$output "/share/firmware")))
                  (mkdir-p fmw)
                  (for-each
                    (lambda (file)
                      (copy-file file
                                 (string-append fmw "/Shell_"
                                                (string-downcase #$arch-string)
                                                ".efi")))
                    (find-files "Build" "Shell\\.efi"))))))))
      (native-inputs
       (append
         (list acpica
               nasm
               perl
               python-3
               (list util-linux "lib"))
         (if (not (string-prefix? arch (%current-system)))
             (if (string=? arch "armhf")
                 (list (cross-gcc "arm-linux-gnueabihf")
                       (cross-binutils "arm-linux-gnueabihf"))
                 (list (cross-gcc (string-append arch "-linux-gnu"))
                       (cross-binutils (string-append arch "-linux-gnu"))))
             '())))
      (synopsis "UEFI firmware for QEMU")
      (description "OVMF is an EDK II based project to enable UEFI support for
Virtual Machines.  OVMF contains a sample UEFI firmware for QEMU and KVM.")
      (license (list license:expat
                     license:bsd-2 license:bsd-3 license:bsd-4)))))

(define (ovmf-aux-file name)
  "Return as a gexp the auxiliary OVMF file corresponding to NAME."
  (local-file (search-auxiliary-file (string-append "ovmf/" name))))

(define-public ovmf-x86-64
  (let ((base (make-ovmf-firmware "x86_64")))
    (package
      (inherit base)
      (name "ovmf-x86-64")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'install
                (lambda _
                  (let ((fmw (string-append #$output "/share/firmware")))
                    (mkdir-p fmw)
                    (for-each
                     (lambda (file)
                       (copy-file
                        (string-append "Build/OvmfX64/RELEASE_GCC"
                                       "/FV/" file ".fd")
                        (string-append fmw "/" (string-downcase file) "_x64.bin")))
                     (list "OVMF"
                           "OVMF_CODE"
                           "OVMF_VARS")))))
              (add-after 'install 'install-qemu-firmware-metadata
                (lambda _
                  ;; The QEMU firmware metadata files are taken from the
                  ;; Fedora project (see:
                  ;; https://src.fedoraproject.org/rpms/edk2/tree/rawhide).
                  (let ((51-edk2-ovmf-2m-raw-x64-nosb.json-source
                         #$(ovmf-aux-file "51-edk2-ovmf-2m-raw-x64-nosb.json"))
                        (51-edk2-ovmf-2m-raw-x64-nosb.json-dest
                         (string-append #$output "/share/qemu/firmware/"
                                        "51-edk2-ovmf-2m-raw-x64-nosb.json")))
                    (mkdir-p (dirname 51-edk2-ovmf-2m-raw-x64-nosb.json-dest))
                    (copy-file 51-edk2-ovmf-2m-raw-x64-nosb.json-source
                               51-edk2-ovmf-2m-raw-x64-nosb.json-dest)
                    (substitute* 51-edk2-ovmf-2m-raw-x64-nosb.json-dest
                      (("/usr/share/edk2/ovmf/OVMF_(CODE|VARS).fd" _ kind)
                       (string-append
                        #$output "/share/firmware/ovmf_"
                        (string-downcase kind) "_x64.bin")))))))))))))

(define-public ovmf-i686
  (let ((base (make-ovmf-firmware "i686")))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           #~(modify-phases #$phases
               (replace 'install
                 (lambda _
                   (let ((fmw (string-append #$output "/share/firmware")))
                     (mkdir-p fmw)
                     (for-each
                       (lambda (file)
                         (copy-file
                           (string-append "Build/OvmfIa32/RELEASE_GCC"
                                          "/FV/" file ".fd")
                           (string-append fmw "/" (string-downcase file) "_ia32.bin")))
                       (list "OVMF"
                             "OVMF_CODE"
                             "OVMF_VARS"))))))))))))

(define-public ovmf-aarch64
  (let ((base (make-ovmf-firmware "aarch64")))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           #~(modify-phases #$phases
               (replace 'install
                 (lambda _
                   (let ((fmw (string-append #$output "/share/firmware")))
                    (mkdir-p fmw)
                    (copy-file (string-append "Build/ArmVirtQemu-AARCH64/"
                                              "RELEASE_GCC/FV/QEMU_EFI.fd")
                               (string-append fmw "/ovmf_aarch64.bin"))))))))))))

(define-public ovmf-arm
  (let ((base (make-ovmf-firmware "armhf")))
    (package
      (inherit base)
      (name "ovmf-arm")
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           #~(modify-phases #$phases
               (replace 'install
                 (lambda _
                   (let ((fmw (string-append #$output "/share/firmware")))
                    (mkdir-p fmw)
                    (copy-file (string-append "Build/ArmVirtQemu-ARM/"
                                              "RELEASE_GCC/FV/QEMU_EFI.fd")
                               (string-append fmw "/ovmf_arm.bin"))))))))))))

(define-public ovmf-riscv64
  (let ((base (make-ovmf-firmware "riscv64")))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           #~(modify-phases #$phases
               (replace 'install
                 (lambda _
                   (let ((fmw (string-append #$output "/share/firmware")))
                    (mkdir-p fmw)
                    (with-directory-excursion "Build/RiscVVirtQemu/RELEASE_GCC/FV"
                      (install-file "RISCV_VIRT_CODE.fd" fmw)
                      (install-file "RISCV_VIRT_VARS.fd" fmw))))))))))))

(define-public ovmf
  (deprecated-package "ovmf" ovmf-x86-64))

(define* (make-arm-trusted-firmware platform #:key
                                    (triplet "aarch64-linux-gnu")
                                    (make-flags '("DEBUG=1")))
  (define (native-build?)
    "Return #t if the host and target platforms differ."
    (or (not triplet)
        ;;%current-system is a *triplet*, unlike its name would suggest.
        (string=? (%current-system) (gnu-triplet->nix-system triplet))))
  (package
    (name (downstream-package-name "arm-trusted-firmware-" platform))
    (version "2.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://git.trustedfirmware.org"
                                  "/TF-A/trusted-firmware-a.git/"))
              (commit (string-append "lts-v" version))))
       (file-name (git-file-name "arm-trusted-firmware" version))
       (sha256
        (base32 "1vngwbjghgsh5i02zq66nmbxxr2d4p93rirsvh5jrhbcdn0v5xf8"))
       (patches (search-patches "8mq-enable-imx_hab_handler.patch"
                                "8mq-move-stack-to-ocram_s.patch"))
       (modules '((guix build utils)))
       ;; Remove binary blobs: they don't reference a source or license.
       (snippet #~(for-each delete-file (find-files "." "\\.bin$")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:target (and (not (native-build?)) triplet)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cross-build
            ;; Fix ATF commit ffb7742125def3e0acca4c7e4d3215af5ce25a31
            (lambda _
              (unless #$(native-build?)
                (substitute* "plat/rockchip/rk3399/drivers/m0/Makefile"
                  (("-oc") "-oc-default"))
                (substitute* "make_helpers/build_macros.mk"
                  (("-oc") "-oc-default")
                  (("-od") "-od-default")))))
          (delete 'configure)          ;no configure script
          (replace 'install
            (lambda _
              (for-each (lambda (file)
                          (install-file file #$output))
                        (find-files "." "\\.(bin|elf)$")))))
      #:make-flags
      #~(list (string-append "PLAT=" #$platform)
              #$@(if (not (native-build?))
                     (list (string-append "CROSS_COMPILE=" triplet "-"))
                     '("CC=gcc"))
              #$@make-flags)
      #:tests? #f))                   ;no test suite
    (native-inputs (list python))
    (home-page "https://www.trustedfirmware.org/")
    (synopsis "Secure world software for ARMv7-A and ARMv8-A")
    (description
     "ARM Trusted Firmware provides a reference implementation of secure
world software for ARMv7-A and ARMv8-A, including a Secure Monitor
executing at @dfn{Exception Level 3} (EL3).  It implements various ARM
interface standards, such as:
@enumerate
@item The Power State Coordination Interface (PSCI)
@item Trusted Board Boot Requirements (TBBR, ARM DEN0006C-1)
@item SMC Calling Convention
@item System Control and Management Interface
@item Software Delegated Exception Interface (SDEI)
@end enumerate")
    (license (list license:bsd-3
                   license:bsd-2)))) ; libfdt

(define-public arm-trusted-firmware-sun50i-a64
  (make-arm-trusted-firmware "sun50i_a64"))

(define-public arm-trusted-firmware-sun50i-h616
  (make-arm-trusted-firmware "sun50i_h616"))

(define-public arm-trusted-firmware-rk3328
  (make-arm-trusted-firmware "rk3328"))

(define-public arm-trusted-firmware-rk3399
  (let ((base (make-arm-trusted-firmware "rk3399")))
    (package
      (inherit base)
      (native-inputs (modify-inputs (package-native-inputs base)
                       (prepend (cross-gcc "arm-none-eabi")
                                (cross-binutils "arm-none-eabi")))))))

(define-public arm-trusted-firmware-rk3588
  (make-arm-trusted-firmware "rk3588"))

(define-public arm-trusted-firmware-imx8mq
  ;; Remove debug symbols because of limited OCRAM.
  (make-arm-trusted-firmware "imx8mq" #:make-flags '()))

(define make-crust-firmware
  (mlambda (platform)
    (package
      (name (string-append "crust-"
                           (string-replace-substring platform "_" "-")
                           "-firmware"))
      (version "0.6")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; There are only GitHub generated release snapshots.
               (url "https://github.com/crust-firmware/crust")
               (commit (string-append "v" version))))
         (file-name (git-file-name "crust" version))
         (sha256
          (base32
           "1blq6bi2rmg4qqwwr07pamv28b50mwcsybhpn9bws8vbzxa43afd"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:target "or1k-elf"
        #:tests? #f                       ;no test suite
        #:make-flags #~'("CROSS_COMPILE=or1k-elf-"
                         "V=1"
                         "HOSTAR=ar"
                         "HOSTCC=gcc"
                         "LEX=flex")
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (add-before 'build 'defconfig
              (lambda* (#:key make-flags #:allow-other-keys)
                (let ((config-name (string-append #$platform "_defconfig")))
                  (apply invoke "make" (cons config-name make-flags)))))
            (replace 'install
              (lambda _
                (for-each (lambda (file)
                            (install-file file (string-append #$output
                                                              "/libexec")))
                          (find-files "." "(scp\\.bin|\\.config)$")))))))
      (native-inputs (list bison flex))
      (home-page "https://github.com/crust-firmware/crust")
      (synopsis "Firmware for Allwinner sunxi SoCs")
      (description "Crust improves battery life and thermal performance by
implementing a deep sleep state.  During deep sleep, the CPU cores, the DRAM
controller, and most onboard peripherals are powered down, reducing power
consumption by 80% or more compared to an idle device.  On boards without a
PMIC, Crust is also responsible for orderly power-off and power-on of the
device.  For this to work, Crust runs outside the main CPU and DRAM, on a
dedicated always-on microprocessor called a System Control Processor (SCP).
Crust is designed to run on a specific SCP implementation, Allwinner's
AR100.")
      ;; Most files are dual-licensed "BSD-3 OR GPL2", a few are GPL2 only.
      (license (list license:bsd-3 license:gpl2)))))

(define make-crust-tools
  (mlambda (platform firmware)
    (package
      (inherit firmware)
      (name (string-append "crust-"
                           (string-replace-substring platform "_" "-")
                           "-tools"))
      (arguments
       (list #:make-flags
             #~(list "V=1"
                     "LEX=flex"
                     (string-append "HOSTAR=" #$(ar-for-target))
                     (string-append "HOSTCC=" #$(cc-for-target)))
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'do-not-build-tests
                   (lambda _
                     ;; Attempting to build the tools test binary on a
                     ;; non-aarch64 architecture fails with: "No cache
                     ;; cleaning implementation available for this
                     ;; architecture".  Avoid building it (see:
                     ;; https://github.com/crust-firmware/crust/issues/182).
                     (substitute* "tools/Makefile"
                       (("tools-y \\+= test") ""))))
                 (replace 'configure
                   (lambda* (#:key inputs native-inputs #:allow-other-keys)
                     (copy-file (search-input-file inputs "/libexec/.config")
                                ".config")))
                 (replace 'build
                   (lambda* (#:key make-flags parallel-build?
                             #:allow-other-keys)
                     (apply invoke "make" "tools"
                            `(,@(if parallel-build?
                                    `("-j"
                                      ,(number->string (parallel-job-count)))
                                     '())
                              ,@make-flags))))
                 (replace 'install
                   (lambda _
                     (install-file "build/tools/load"
                                   (string-append #$output "/bin")))))))
      (synopsis "Firmware for Allwinner sunxi SoCs (tools)")
      (inputs (list firmware)))))

(define make-crust-package
  (mlambda (platform)
    (let* ((firmware (make-crust-firmware platform))
           (tools (make-crust-tools platform firmware)))
      (package
        (inherit firmware)
        (name (string-append "crust-"
                             (string-replace-substring platform "_" "-")))
        (source #f)
        (build-system trivial-build-system)
        (arguments
         (list #:modules '((guix build union))
               #:builder
               #~(begin
                   (use-modules (ice-9 match)
                                (guix build union))

                   (match %build-inputs
                     (((names . directory) ...)
                      (union-build #$output directory))))))
        (native-inputs '())
        (inputs (list firmware tools))))))

(define-public crust-pinebook
  (make-crust-package "pinebook"))

(define-public crust-pine64-plus
  (make-crust-package "pine64_plus"))


;;;
;;; ErgoDox firmware.
;;;

(define* (make-ergodox-firmware/implementation layout #:key override.c
                                               override.h)
  "Return an ergodox-firmware package for LAYOUT, optionally using OVERRIDE.C,
a C source file-like object to override LAYOUT which may be accompanied by
OVERRIDE.H, to also override the corresponding layout include file."
  (let ((revision "0")
        (commit "89b7e2bfdafb2a87e0248846d5c95cc5e9a27858"))
    (package
      (name (string-append "ergodox-firmware-" layout))
      (version (git-version "1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/benblazak/ergodox-firmware")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1z28frxyb21nz90frycrpsbxjp09374wawayvjphnwc8njlvkkpy"))
                (patches
                 (search-patches "ergodox-firmware-fix-json-target.patch"
                                 "ergodox-firmware-fix-numpad.patch"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                   ;no test suite
        #:target "avr"
        #:make-flags
        #~(list (string-append "LAYOUT=" #$layout)
                ;; Simplify the output directory name.
                "ROOT=output")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'copy-override-files
              (lambda _
                (when #$override.c
                  (copy-file #$override.c
                             (format #f "src/keyboard/ergodox/layout/~a.c"
                                     #$layout)))
                (when #$override.h
                  (copy-file #$override.h
                             (format #f "src/keyboard/ergodox/layout/~a.h"
                                     #$layout)))))
            ;; The Makefile-based build system lacks configure
            ;; and install targets.
            (delete 'configure)
            (replace 'install
              (lambda _
                (with-directory-excursion "output"
                  (install-file "firmware.hex" #$output)
                  (install-file "firmware.eep" #$output)
                  (install-file "firmware--layout.html" #$output)))))))
      (native-inputs (list python))
      (home-page "https://www.ergodox.io")
      (synopsis "Firmware for the ErgoDox keyboard")
      (description (format #f "This package contains the original firmware for
the ErgoDox keyboard, built using the ~a layout (as defined in the
@file{src/keyboard/ergodox/layout/~@*~a.c} source file).  It contains the
@file{firmware.hex} and the @file{firmware.eep} files, which can be loaded to
a target using the @code{teensy-loader-cli} package as well as a
@file{firmware--layout.html} file, useful to easily visualize the
corresponding layout." layout))
      (license license:expat))))

(define make-ergodox-firmware
  (memoize make-ergodox-firmware/implementation))

(define-public ergodox-firmware-colemak-jc-mod
  (make-ergodox-firmware "colemak-jc-mod"))

(define-public ergodox-firmware-colemak-symbol-mod
  (make-ergodox-firmware "colemak-symbol-mod"))

(define-public ergodox-firmware-dvorak-kinesis-mod
  (make-ergodox-firmware "dvorak-kinesis-mod"))

(define-public ergodox-firmware-qwerty-kinesis-mod
  (make-ergodox-firmware "qwerty-kinesis-mod"))

(define-public ergodox-firmware-workman-p-kinesis-mod
  (make-ergodox-firmware "workman-p-kinesis-mod"))


;;;
;;; QMK Firmware.
;;;

(define-public qmk
  (package
    (name "qmk")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "qmk" version))
              (sha256
               (base32
                "0l4b5bi6x1i9bh3ik73yisn1pq1bhgiksiddvg3cxy7jmgdnqhyw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:modules '((guix build pyproject-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'wrap 'wrap-path
            ;; Wrap all the tools needed for the 'setup' and 'compile' actions
            ;; (tested with the 'ergodox_ez' keyboard).
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/qmk")
                `("PATH" prefix
                  ,(map (compose dirname
                                 (cut search-input-file inputs <>))
                        '("bin/avrdude"
                          "bin/awk"
                          "bin/cmp"
                          "bin/dfu-programmer"
                          "bin/dfu-util"
                          "bin/git"
                          "bin/grep"
                          "bin/make"
                          "bin/python3"
                          "bin/sh"
                          ;; TODO: Remove after git is wrapped with these.
                          "bin/basename"
                          "bin/sed"
                          "bin/uname")))))))))
    ;; The inputs are not propagated since qmk is to be used strictly as a
    ;; command.
    (inputs
     ;; The 'qmk setup' command advises to use GCC at version 8, and there are
     ;; compilation errors in some firmware otherwise.
     (list avrdude
           bash-minimal
           dfu-programmer
           dfu-util
           diffutils
           git-minimal                  ;for the clone action
           gawk
           gnu-make
           grep
           python
           python-dotty-dict
           python-hid
           python-hjson
           python-jsonschema
           python-milc
           python-pillow
           python-pygments
           python-pyserial
           python-pyusb
           ;; These are added to workaround faults in our git package (see
           ;; bug#65924).
           coreutils-minimal
           sed
           util-linux))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://qmk.fm")
    (synopsis "Command line utility to manage QMK keyboard firmwares")
    (description "The QMK CLI provides a @acronym{CLI, command line interface}
based program to help users work with the QMK firmware, which can be used for
multiple custom keyboards such as Planck, ErgoDox, Corne and others.

This @acronym{CLI} program is mainly used for building the QMK firmware, but
also has some other convenience utilities.  It is highly recommended to
install the udev rules provided by the @code{qmk-udev-rules} package to avoid
having to run @command{qmk} as root when flashing the firmware.")
    (license license:expat)))

(define* (make-qmk-firmware/implementation keyboard keymap
                                           #:key (description "")
                                           keymap-json
                                           keymap-source-directory
                                           keyboard-source-directory)
  "Return a package to build the QMK firmware for KEYBOARD with KEYMAP.
Keyboard should be the name of a sub-directory under the @file{keyboards} directory.
For custom keymaps, KEYMAP-JSON, a file-like object of a JSON representation of
KEYMAP as generated by the @url{https://config.qmk.fm/, QMK Configurator} tool or
KEYMAP-SOURCE-DIRECTORY, a file-like object directory containing the keymap source
files files such as @file{keymap.c}, can be provided.  For keyboards not available in
upstream repository, provide a file-like object directory containing the whole
keyboard definition in KEYBOARD-SOURCE-DIRECTORY."
  (package
    (name (string-append "qmk-firmware-"
                         (regexp-substitute/global #f "[_/]" keyboard
                                                   'pre "-" 'post)
                         "-"
                         (string-replace-substring keymap "_" "-")))
    ;; Note: When updating this package, make sure to also update the commit
    ;; used for the LUFA submodule in the 'copy-lufa-source' phase below.
    ;; Note: If you update this you WILL lose support for ergodox.
    (version "0.22.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/qmk/qmk_firmware")
                    (commit version)))
              (file-name (git-file-name "qmk-firmware" version))
              (sha256
               (base32
                "0s1lcnv7cddpn768p7mrc5bkxhx0ba5p77ya007dnkbk36c33d0w"))
              (patches
               (search-patches "qmk-firmware-fix-hacker-dvorak.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-26))
      #:target "avr"
      ;; XXX: Running a test target like "test:$keyboard" doesn't seem to run
      ;; anything and causes the .hex file to be regenerated; leave the tests
      ;; out for now.
      #:tests? #f
      #:make-flags
      #~(list #$(format #f "~a:~a" keyboard keymap)
              (string-append "SHELL=" (search-input-file
                                       %build-inputs "bin/sh")))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              ;; Do not attempt to retrieve information from git during the
              ;; build.
              (setenv "SKIP_GIT" "1")))
          (add-after 'unpack 'copy-lufa-source
            ;; QMK carries a custom fork of LUFA as a git submodule; make sure
            ;; the same commit is used (see:
            ;; https://github.com/qmk/qmk_firmware/tree/master/lib).
            (lambda _
              (copy-recursively
               #$(let ((commit "549b97320d515bfca2f95c145a67bd13be968faa"))
                   (origin
                     (inherit (package-source lufa))
                     (uri (git-reference
                           (url "https://github.com/qmk/lufa")
                           (commit commit)))
                     (file-name (git-file-name "lufa" commit))
                     (sha256
                      (base32
                       "1rmhm4rxvq8skxqn6vc4n4ly1ak6whj7c386zbsci4pxx548n9h4"))))
               "lib/lufa")))
          #$@(if keyboard-source-directory
                 #~((add-after 'unpack 'copy-keyboard-source-directory
                      (lambda _
                        (let ((keyboard-dir #$(string-append "keyboards/" keyboard)))
                          (false-if-exception (delete-file-recursively
                                               keyboard-dir))
                          (copy-recursively #$keyboard-source-directory
                                            keyboard-dir)))))
                 #~())
          #$@(if keymap-source-directory
                 #~((add-after 'unpack 'copy-keymap-source-directory
                      (lambda _
                        (let ((keymap-dir #$(string-append "keyboards/" keyboard
                                                           "/keymaps/" keymap)))
                          (false-if-exception (delete-file-recursively
                                               keymap-dir))
                          (copy-recursively #$keymap-source-directory
                                            keymap-dir)))))
                 #~())
          #$@(if keymap-json
                 #~((replace 'build
                      (lambda _
                        (invoke "qmk" "compile" #$keymap-json))))
                 #~())
          (replace 'install
            (lambda _
              (match (scandir "." (lambda (f)
                                    (false-if-exception
                                     (member (string-take-right f 4)
                                             '(".bin" ".hex" ".uf2")))))
                (()
                 (error "no built binary file found"))
                ((hex ..1)
                 (for-each (cut install-file <> #$output) hex))))))))
    ;; Some of the build tools are required to be on the PATH, as the make
    ;; files do not always operate through 'qmk'; all of qmk's inputs must
    ;; thus be made available.
    (native-inputs (modify-inputs (package-inputs qmk)
                     (append qmk)))
    (home-page "https://qmk.fm/")
    (synopsis "Keyboard firmware for Atmel AVR and Arm USB families")
    (description
     (format #f "QMK (Quantum Mechanical Keyboard Firmware) is a keyboard
firmware based on the @url{https://github.com/tmk/tmk_keyboard, tmk_keyboard
firmware} with some useful features for Atmel AVR and ARM controllers, and
more specifically, the @url{https://olkb.com/, OLKB product line}, the
@url{https://ergodox-ez.com/, ErgoDox EZ keyboard}, and the
@url{https://clueboard.co/, Clueboard product line}.~@[~%~%~a~]" description))
    (license license:gpl2+)))

(define make-qmk-firmware (memoize make-qmk-firmware/implementation))

(define-public qmk-firmware-ergodox-ez-default
  (make-qmk-firmware
   "ergodox_ez" "default" #:description
   "This is the default keymap used on the ErgoDox EZ keyboard.  It includes
the novel MEH and Hyper keys pioneered by the ErgoDox EZ, easy to reach
Control/Shift modifiers keys, and two-functions hold keys to access layer 1.
Layer 1 contains function keys, symbols, a numpad as well as brightness keys.
Layer 2 contains multimedia and mouse keys.  See the
@file{keyboards/ergodox_ez/keymaps/default/keymap.c} source file for the
keymap definition, or the
@url{https://configure.ergodox-ez.com/ergodox-ez/layouts/JwwW/latest/0,
ErgoDox EZ Configurator} page."))

(define-public qmk-firmware-ergodox-ez-dvorak-42-key
  (make-qmk-firmware "ergodox_ez" "dvorak_42_key" #:description "\
This is a Dvorak-based layout for the ErgoDox EZ.  Its basic key layout is
similar to the Atreus @samp{dvorak_42_key} layout; in fact this layout was
created for seamless switching between the ErgoDox EZ and Atreus keyboards.
On the base layer, the keys that don't exist on the Atreus are mapped to MEH
shortcuts and can be interpreted by your window managher.  This layout only
makes use of the 42 keys that the Atreus also has for the main functionality.
See the @file{keyboards/atreus/keymaps/dvorak_42_key/keymap.c} source file for
the keymap definition."))

(define-public qmk-firmware-ergodox-ez-hacker-dvorak
  (make-qmk-firmware "ergodox_ez" "hacker_dvorak" #:description "\
This is a Dvorak layout for the ErgoDox EZ.  It is inspired by the
@url{https://www.kaufmann.no/roland/dvorak, Programmer Dvorak}.  The operating
system keyboard layout should be set to US for this keymap to function
normally.  It defines 10 layers:
@enumerate
@item Dvorak
@item Querty
@item Gaming
@item Arrows
@item Mouse
@item Numpad
@item Hyper Fn
@item Media Fn
@item Meh Fn
@item Meh Fn +
@end enumerate
The complete keymap can be inspected at the ErgoDox EZ Configurator web site:
@url{https://configure.ergodox-ez.com/ergodox-ez/layouts/Wadz/latest/0}."))

(define-public qmk-firmware-ergodox-ez-dvorak
  (make-qmk-firmware
   "ergodox_ez" "dvorak" #:description
   "This is a rather plain Dvorak layout for the ErgoDox EZ, containing
function and symbols on layer 1 and media and and mouse keys on layer 2.  See
the @file{layouts/community/ergodox/dvorak/keymap.c} source file for the
keymap definition."))

(define-public qmk-firmware-ergodox-ez-dvorak-emacs
  (make-qmk-firmware
   "ergodox_ez" "dvorak_emacs" #:description
   "This is a Dvorak-based keymap optimized for Emacs usage, with the
frequently used Control and Meta (Alt) keys mapped to the thumb buttons.  It
contains a single extra layer that includes function and multimedia keys.  A
graphical representation of the keymap is available in the
@file{layouts/community/ergodox/dvorak_emacs/readme.md} source file."))

(define-public qmk-firmware-ergodox-ez-dvorak-emacs-software
  (make-qmk-firmware
   "ergodox_ez" "dvorak_emacs_software" #:description
   "This is the same layout as that of the
@code{qmk-firmware-ergodox-ez-dvorak-emacs}, but designed to be used with a
Dvorak-configured operating system (instead of a US QWERTY one, which is the
default assumption for QMK firmware keymaps)."))

(define-public qmk-udev-rules
  (package
    (inherit qmk-firmware-ergodox-ez-default)
    (name "qmk-udev-rules")
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("./util/udev" "lib/udev/rules.d"
                         #:include-regexp ("rules$")))))
    (native-inputs '())
    (inputs '())
    (propagated-inputs '())
    (synopsis "Udev rules for QMK Firmware")
    (description
     "This package provides a set of udev rules to specify the proper
privileges for flashing QMK compatible devices without needing root.  The
rules require the group @code{plugdev} to be added to each user that needs
this.")))

(define-public senoko-chibios
  (package
    (name "senoko-chibios")
    (version "2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/xobs/senoko-chibios-3.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qdpxzqdh0l65rzfbrm1lqzpik3nyg8wa7k0b8b6apj2w6vsp5pv"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; no tests
           #:make-flags #~(list "USE_VERBOSE_COMPILE=yes")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'patch
                 (lambda* (#:key outputs #:allow-other-keys)
                   (chdir "senoko")
                   (substitute* "Makefile"
                    ;; We don't have those dependencies since we delete .git
                    ;; after checkout.
                    ((" [$][(]CHIBIOS[)]/.git/HEAD [$][(]CHIBIOS[)]/.git/index")
                     "")
                    (("[$][(]shell git rev-parse HEAD[)]")
                     ;; Uniquely identify the version.
                     (assoc-ref outputs "out")))))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((destination (string-append (assoc-ref outputs "out")
                                      "/lib/firmware")))
                     (install-file "build/senoko.elf" destination)
                     (install-file "build/senoko.hex" destination)))))))
    (synopsis "Firmware for Novena battery or passthrough board")
    (description "This package provides the firmware for the Novena battery
or passthrough board.")
    (supported-systems '("armhf-linux")) ; actually cortex-m3
    (home-page "https://github.com/xobs/senoko-chibios-3/")
    (license license:gpl3+)))

(define (make-qmk-newlib-nano-arm-none-eabi)
  (let ((base (make-newlib-nano-arm-none-eabi)))
    (package
      (inherit base)
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "xgcc" (make-gcc-arm-none-eabi-12.3.rel1)))))))

(define* (make-qmk-firmware-keychron keyboard keymap
                                     #:key (description "")
                                     keymap-json
                                     keymap-source-directory
                                     keyboard-source-directory)
  (let ((base (make-qmk-firmware keyboard keymap
                                 #:description description
                                 #:keymap-json keymap-json
                                 #:keymap-source-directory keymap-source-directory
                                 #:keyboard-source-directory keyboard-source-directory)))
    (package
      (inherit base)
      (name (string-append "qmk-firmware-"
                           (regexp-substitute/global #f "[_/]" keyboard
                                                     'pre "-" 'post)
                           "-"
                           (string-replace-substring keymap "_" "-")))
      ;; Note: When updating this package, make sure to also update the commit
      ;; used for the LUFA submodule in the 'copy-lufa-source' phase below.
      (version "0.28.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/qmk/qmk_firmware")
                      (commit version)))
                (file-name (git-file-name "qmk-firmware" version))
                (sha256
                 (base32
                  "1skj7iq6dad48xhy2ljjmwpbhhdv3gk7cmi28lh3xfsnxphm4v8r"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'copy-lufa-source
                ;; QMK carries a custom fork of LUFA as a git submodule; make sure
                ;; the same commit is used (see:
                ;; https://github.com/qmk/qmk_firmware/tree/master/lib).
                (lambda _
                  (copy-recursively
                   #$(let ((commit "549b97320d515bfca2f95c145a67bd13be968faa"))
                       (origin
                         (inherit (package-source lufa))
                         (uri (git-reference
                               (url "https://github.com/qmk/lufa")
                               (commit commit)))
                         (file-name (git-file-name "lufa" commit))
                         (sha256
                          (base32
                           "1rmhm4rxvq8skxqn6vc4n4ly1ak6whj7c386zbsci4pxx548n9h4"))))
                   "lib/lufa")))
              (add-after 'unpack 'setenv
                (lambda _
                  ;; Because newlib-nano is also compiled without FPU.
                  (setenv "USE_FPU" "no")))
              (add-after 'unpack 'copy-chibios-source
                (lambda _
                  ;; Newest version.
                  (copy-recursively
                   #$(origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/qmk/ChibiOS")
                             (commit "2365f844292513ea0ee9eea6ab778d56f9ccd3b9")))
                                        ;(file-name (git-file-name name version))
                       (sha256
                        (base32
                         "1ivkrx4nv0sa4nfryzbq5h40vs7vik8p0dp9czjh2srql520y7yq")))
                   "lib/chibios")))
              (add-after 'unpack 'copy-printf-source
                ;; Newest version.
                (lambda _
                  (copy-recursively
                   #$(origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/qmk/printf.git")
                             (commit "c2e3b4e10d281e7f0f694d3ecbd9f320977288cc")))
                                        ;(file-name (git-file-name name version))
                       (sha256
                        (base32
                         "0r501hkk0idwfm6qs09g1wb808ga452gz39dw32x13rmg3a901s6")))
                   "lib/printf")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend
          (package
            (inherit (make-qmk-newlib-nano-arm-none-eabi))
            (native-search-paths
             (list (search-path-specification
                    (variable "CROSS_C_INCLUDE_PATH")
                    (files '("arm-none-eabi/include")))
                   (search-path-specification
                    (variable "CROSS_CPLUS_INCLUDE_PATH")
                    (files '("arm-none-eabi/include"
                             "arm-none-eabi/include/c++"
                             "arm-none-eabi/include/c++/arm-none-eabi")))
                   (search-path-specification
                    (variable "CROSS_LIBRARY_PATH")
                    (files '("arm-none-eabi/lib"))))))
          (make-gcc-arm-none-eabi-12.3.rel1)
          (cross-binutils "arm-none-eabi")))))))

(define-public qmk-firmware-keychron-c1-pro-ansi-rgb-default
  (make-qmk-firmware-keychron "keychron/c1_pro/ansi/rgb" "default"
   #:description
   "This package provides the firmware for the Keychron C1 Pro ANSI RGB with default keymap."))

(define-public qmk-firmware-keychron-c1-pro-ansi-white-default
  (make-qmk-firmware-keychron "keychron/c1_pro/ansi/white" "default"
   #:description
   "This package provides the firmware for the Keychron C1 Pro ANSI white with default keymap."))

(define-public qmk-firmware-keychron-c2-pro-ansi-rgb-default
  (make-qmk-firmware-keychron "keychron/c2_pro/ansi/rgb" "default"
   #:description
   "This package provides the firmware for the Keychron C2 Pro ANSI RGB with default keymap."))

(define-public qmk-firmware-keychron-c2-pro-ansi-white-default
  (make-qmk-firmware-keychron "keychron/c2_pro/ansi/white" "default"
   #:description
   "This package provides the firmware for the Keychron C2 Pro ANSI white with default keymap."))

(define-public qmk-firmware-keychron-c3-pro-ansi-red-default
  (make-qmk-firmware-keychron "keychron/c3_pro/ansi/red" "default"
   #:description
   "This package provides the firmware for the Keychron C3 Pro ANSI Red with default keymap."))

(define-public qmk-firmware-keychron-c3-pro-ansi-rgb-default
  (make-qmk-firmware-keychron "keychron/c3_pro/ansi/rgb" "default"
   #:description
   "This package provides the firmware for the Keychron C3 Pro ANSI RGB with default keymap."))

(define-public qmk-firmware-keychron-q0-base-default
  (make-qmk-firmware-keychron "keychron/q0/base" "default"
   #:description
   "This package provides the firmware for the Keychron Q0 Base with default keymap."))

(define-public qmk-firmware-keychron-q0-plus-default
  (make-qmk-firmware-keychron "keychron/q0/plus" "default"
   #:description
   "This package provides the firmware for the Keychron Q0 Plus with default keymap."))

(define-public qmk-firmware-keychron-q10-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q10/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q10 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q10-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q10/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q10 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q11-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q11/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q11 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q11-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q11/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q11 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q12-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q12/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q12 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q12-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q12/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q12 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q1v2-ansi-default
  (make-qmk-firmware-keychron "keychron/q1v2/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron Q1v2 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q1v2-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q1v2/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q1v2 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q1v2-iso-default
  (make-qmk-firmware-keychron "keychron/q1v2/iso" "default"
   #:description
   "This package provides the firmware for the Keychron Q1v2 ISO with default keymap."))

(define-public qmk-firmware-keychron-q1v2-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q1v2/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q1v2 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q1v2-jis-default
  (make-qmk-firmware-keychron "keychron/q1v2/jis" "default"
   #:description
   "This package provides the firmware for the Keychron Q1v2 JIS with default keymap."))

(define-public qmk-firmware-keychron-q1v2-jis-encoder-default
  (make-qmk-firmware-keychron "keychron/q1v2/jis_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q1v2 JIS with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q2-ansi-default
  (make-qmk-firmware-keychron "keychron/q2/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron Q2 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q2-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q2/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q2 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q2-iso-default
  (make-qmk-firmware-keychron "keychron/q2/iso" "default"
   #:description
   "This package provides the firmware for the Keychron Q2 ISO with default keymap."))

(define-public qmk-firmware-keychron-q2-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q2/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q2 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q2-jis-default
  (make-qmk-firmware-keychron "keychron/q2/jis" "default"
   #:description
   "This package provides the firmware for the Keychron Q2 JIS with default keymap."))

(define-public qmk-firmware-keychron-q2-jis-encoder-default
  (make-qmk-firmware-keychron "keychron/q2/jis_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q2 JIS with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q3-ansi-default
  (make-qmk-firmware-keychron "keychron/q3/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron Q3 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q3-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q3/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q3 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q3-iso-default
  (make-qmk-firmware-keychron "keychron/q3/iso" "default"
   #:description
   "This package provides the firmware for the Keychron Q3 ISO with default keymap."))

(define-public qmk-firmware-keychron-q3-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q3/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q3 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q3-jis-default
  (make-qmk-firmware-keychron "keychron/q3/jis" "default"
   #:description
   "This package provides the firmware for the Keychron Q3 JIS with default keymap."))

(define-public qmk-firmware-keychron-q3-jis-encoder-default
  (make-qmk-firmware-keychron "keychron/q3/jis_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q3 JIS with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q4-ansi-v2-default
  (make-qmk-firmware-keychron "keychron/q4/ansi/v2" "default"
   #:description
   "This package provides the firmware for the Keychron Q4 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q4-iso-default
  (make-qmk-firmware-keychron "keychron/q4/iso" "default"
   #:description
   "This package provides the firmware for the Keychron Q4 ISO with default keymap."))

(define-public qmk-firmware-keychron-q5-ansi-default
  (make-qmk-firmware-keychron "keychron/q5/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron Q5 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q5-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q5/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q5 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q5-iso-default
  (make-qmk-firmware-keychron "keychron/q5/iso" "default"
   #:description
   "This package provides the firmware for the Keychron Q5 ISO with default keymap."))

(define-public qmk-firmware-keychron-q5-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q5/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q5 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q60-ansi-default
  (make-qmk-firmware-keychron "keychron/q60/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron Q60 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q65-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q65/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q65 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q6-ansi-default
  (make-qmk-firmware-keychron "keychron/q6/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron Q6 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q6-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q6/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q6 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q6-iso-default
  (make-qmk-firmware-keychron "keychron/q6/iso" "default"
   #:description
   "This package provides the firmware for the Keychron Q6 ISO with default keymap."))

(define-public qmk-firmware-keychron-q6-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q6/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q6 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q7-ansi-default
  (make-qmk-firmware-keychron "keychron/q7/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron Q7 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q7-iso-default
  (make-qmk-firmware-keychron "keychron/q7/iso" "default"
   #:description
   "This package provides the firmware for the Keychron Q7 ISO with default keymap."))

(define-public qmk-firmware-keychron-q8-ansi-default
  (make-qmk-firmware-keychron "keychron/q8/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron Q8 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q8-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q8/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q8 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q8-iso-default
  (make-qmk-firmware-keychron "keychron/q8/iso" "default"
   #:description
   "This package provides the firmware for the Keychron Q8 ISO with default keymap."))

(define-public qmk-firmware-keychron-q8-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q8/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q8 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q9-ansi-default
  (make-qmk-firmware-keychron "keychron/q9/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron Q9 ANSI with default keymap."))

(define-public qmk-firmware-keychron-q9-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q9/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q9 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q9-iso-default
  (make-qmk-firmware-keychron "keychron/q9/iso" "default"
   #:description
   "This package provides the firmware for the Keychron Q9 ISO with default keymap."))

(define-public qmk-firmware-keychron-q9-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/q9/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q9 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-q9-plus-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/q9_plus/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron Q9 Plus ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-s1-ansi-rgb-default
  (make-qmk-firmware-keychron "keychron/s1/ansi/rgb" "default"
   #:description
   "This package provides the firmware for the Keychron S1 ANSI RGB with default keymap."))

(define-public qmk-firmware-keychron-s1-ansi-white-default
  (make-qmk-firmware-keychron "keychron/s1/ansi/white" "default"
   #:description
   "This package provides the firmware for the Keychron S1 ANSI white with default keymap."))

(define-public qmk-firmware-keychron-v10-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/v10/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V10 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v10-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/v10/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V10 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v1-ansi-default
  (make-qmk-firmware-keychron "keychron/v1/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron V1 ANSI with default keymap."))

(define-public qmk-firmware-keychron-v1-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/v1/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V1 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v1-iso-default
  (make-qmk-firmware-keychron "keychron/v1/iso" "default"
   #:description
   "This package provides the firmware for the Keychron V1 ISO with default keymap."))

(define-public qmk-firmware-keychron-v1-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/v1/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V1 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v1-jis-default
  (make-qmk-firmware-keychron "keychron/v1/jis" "default"
   #:description
   "This package provides the firmware for the Keychron V1 JIS with default keymap."))

(define-public qmk-firmware-keychron-v1-jis-encoder-default
  (make-qmk-firmware-keychron "keychron/v1/jis_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V1 JIS with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v2-ansi-default
  (make-qmk-firmware-keychron "keychron/v2/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron V2 ANSI with default keymap."))

(define-public qmk-firmware-keychron-v2-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/v2/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V2-ansi-encoder with default keymap."))

(define-public qmk-firmware-keychron-v2-iso-default
  (make-qmk-firmware-keychron "keychron/v2/iso" "default"
   #:description
   "This package provides the firmware for the Keychron V2 ISO with default keymap."))

(define-public qmk-firmware-keychron-v2-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/v2/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V2 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v2-jis-default
  (make-qmk-firmware-keychron "keychron/v2/jis" "default"
   #:description
   "This package provides the firmware for the Keychron V2 JIS with default keymap."))

(define-public qmk-firmware-keychron-v2-jis-encoder-default
  (make-qmk-firmware-keychron "keychron/v2/jis_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V2 JIS with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v3-ansi-default
  (make-qmk-firmware-keychron "keychron/v3/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron V3 ANSI with default keymap."))

(define-public qmk-firmware-keychron-v3-iso-default
  (make-qmk-firmware-keychron "keychron/v3/iso" "default"
   #:description
   "This package provides the firmware for the Keychron V3 ISO with default keymap."))

(define-public qmk-firmware-keychron-v3-jis-default
  (make-qmk-firmware-keychron "keychron/v3/jis" "default"
                              #:description
                              "This package provides the firmware for the Keychron V3 JIS with default keymap."))

(define-public qmk-firmware-keychron-v3-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/v3/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V3 ANSI with default keymap and with an encoder."))

(define-public qmk-firmware-keychron-v3-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/v3/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V3 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v3-jis-encoder-default
  (make-qmk-firmware-keychron "keychron/v3/jis_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V3 JIS with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v4-ansi-default
  (make-qmk-firmware-keychron "keychron/v4/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron V4 ANSI with default keymap."))

(define-public qmk-firmware-keychron-v4-iso-default
  (make-qmk-firmware-keychron "keychron/v4/iso" "default"
   #:description
   "This package provides the firmware for the Keychron V4 ISO with default keymap."))

(define-public qmk-firmware-keychron-v5-ansi-default
  (make-qmk-firmware-keychron "keychron/v5/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron V5 ANSI with default keymap."))

(define-public qmk-firmware-keychron-v5-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/v5/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V5 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v5-iso-default
  (make-qmk-firmware-keychron "keychron/v5/iso" "default"
   #:description
   "This package provides the firmware for the Keychron V5 ISO with default keymap."))

(define-public qmk-firmware-keychron-v5-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/v5/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V5 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v6-ansi-default
  (make-qmk-firmware-keychron "keychron/v6/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron V6 ANSI with default keymap."))

(define-public qmk-firmware-keychron-v6-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/v6/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V6 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v6-iso-default
  (make-qmk-firmware-keychron "keychron/v6/iso" "default"
   #:description
   "This package provides the firmware for the Keychron V6 ISO with default keymap."))

(define-public qmk-firmware-keychron-v6-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/v6/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V6 ISO with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v7-ansi-default
  (make-qmk-firmware-keychron "keychron/v7/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron V7 ANSI with default keymap."))

(define-public qmk-firmware-keychron-v7-iso-default
  (make-qmk-firmware-keychron "keychron/v7/iso" "default"
   #:description
   "This package provides the firmware for the Keychron V7 ISO with default keymap."))

(define-public qmk-firmware-keychron-v8-ansi-default
  (make-qmk-firmware-keychron "keychron/v8/ansi" "default"
   #:description
   "This package provides the firmware for the Keychron V8 ANSI with default keymap."))

(define-public qmk-firmware-keychron-v8-ansi-encoder-default
  (make-qmk-firmware-keychron "keychron/v8/ansi_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V8 ANSI with default keymap and with encoder."))

(define-public qmk-firmware-keychron-v8-iso-default
  (make-qmk-firmware-keychron "keychron/v8/iso" "default"
   #:description
   "This package provides the firmware for the Keychron V8 ISO with default keymap."))

(define-public qmk-firmware-keychron-v8-iso-encoder-default
  (make-qmk-firmware-keychron "keychron/v8/iso_encoder" "default"
   #:description
   "This package provides the firmware for the Keychron V8 ISO with default keymap and with encoder."))

(define-public firmware-senoko
  (package
    (name "firmware-senoko")
    (version "2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/novena-next/firmware-senoko.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10d6bgqajl0w17xydjv2x22n5m9d1xsjb09r430nakc1v6gn52d5"))))
    (build-system copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch
                 (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "update-senoko"
                      (("^fw=/lib/firmware/senoko.hex")
                       (string-append "fw="
                                      (assoc-ref inputs "senoko-chibios")
                                      "/lib/firmware/senoko.hex")))
                     (patch-shebang "update-senoko"))))
           #:install-plan
           ''(("update-senoko" "bin/")
              ("update-senoko.1" "share/man/man1/"))))
    (inputs
     (list senoko-chibios))
    (synopsis "Firmware flasher for Novena battery or passthrough board")
    (description "This package provides a way to update the Novena battery or
passthrough board firmware on a Novena.")
    (supported-systems '("armhf-linux"))
    (home-page "https://github.com/novena-next/firmware-senoko")
    (license license:bsd-3)))
