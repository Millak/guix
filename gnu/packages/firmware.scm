;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2018, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
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
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
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
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match))

(define-public ath9k-htc-firmware
  (package
    (name "ath9k-htc-firmware")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/qca/open-ath9k-htc-firmware")
                    (commit version)))
              (sha256
               (base32
                "16jbj8avg5jkgvq5lxm0hdxxn4c3zn7fx8b4nxllvr024apk9w23"))
              (file-name (git-file-name name version))
              (patches (search-patches "ath9k-htc-firmware-objcopy.patch"
                                       "ath9k-htc-firmware-gcc-compat.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (chdir "target_firmware")

             ;; 'configure' is a simple script that runs 'cmake' with
             ;; the right flags.
             (substitute* "configure"
               (("^TOOLCHAIN=.*$")
                (string-append "TOOLCHAIN="
                               (assoc-ref (or native-inputs inputs) "cross-gcc")
                               "\n")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (fw-dir (string-append out "/lib/firmware")))
               (for-each (lambda (file)
                           (install-file file fw-dir))
                         (find-files "." "\\.fw$"))
              #t))))
       #:tests? #f))

    ;; The firmware is cross-compiled using a "bare bones" compiler (no libc.)
    ;; Use our own tool chain for that.
    (native-inputs `(("cross-gcc" ,(cross-gcc
                                    "xtensa-elf"
                                    #:xbinutils (cross-binutils
                                                 "xtensa-elf"
                                                 #:binutils binutils-2.33)))
                     ("cross-binutils" ,(cross-binutils
                                         "xtensa-elf"
                                         #:binutils binutils-2.33))
                     ("cmake" ,cmake-minimal)
                     ("perl" ,perl)))
    (home-page "https://wireless.wiki.kernel.org/en/users/Drivers/ath9k_htc")
    (synopsis "Firmware for the Atheros AR7010 and AR9271 USB 802.11n NICs")
    (description
     "This is the firmware for the Qualcomm Atheros AR7010 and AR9271 USB
802.11n NICs (aka Wi-Fi USB dongles).  It is used by the ath9k driver of
Linux-libre.")
    (license (license:non-copyleft "http://directory.fsf.org/wiki/License:ClearBSD"))))

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
         #:phases
         (let ((subdirs '("assembler" "disassembler")))
           (modify-phases %standard-phases
             (delete 'configure)        ; no configure script
             (add-before 'build 'patch-/bin/true
               (lambda _
                 (substitute* (find-files "." "Makefile")
                   (("/bin/true") ":"))
                 #t))
             (replace 'build
               (lambda _
                 (for-each (lambda (dir)
                             (invoke "make" "-C" dir "CC=gcc"))
                           subdirs)
                 #t))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (mkdir-p (string-append out "/bin"))
                   (for-each (lambda (dir)
                               (invoke "make" "-C" dir
                                       (string-append "PREFIX=" out)
                                       "install"))
                             subdirs)
                   #t)))))))
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
    (inputs (list bash-completion
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
                  libsmbios
                  gnu-efi))
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
              (string-append "DESTDIR=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))        ; No configure script.
    (native-inputs
     (list tcsh))
    (home-page "https://www.openfirmware.info/FCODE_suite")
    (synopsis "Utilities to process FCODE, OpenFirmware's byte code")
    (description "This is the OpenBIOS FCODE suite.  It contains a set of
utilites used to process FCODE, OpenFirmware's byte code, consisting of:
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
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/riscv-software-src/opensbi")
             (commit (string-append "v" version))))
       (file-name (git-file-name "opensbi" version))
       (sha256
        (base32 "01pr7fyg3gcb5pj6d48w2an3m4mfjs9b398x31drqxwqcaz0zn94"))))
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
    (synopsis "x86 BIOS implementation")
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
       ((#:modules modules %gnu-build-system-modules)
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
and input capabilites normally handled by a VGA adapter and a keyboard, and
additionally provide hooks for logging displayed characters for later collection
after an operating system boots.")
      (license license:asl2.0))))

(define-public edk2-tools
  (package
    (name "edk2-tools")
    (version "202211")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tianocore/edk2")
                    (commit (string-append "edk2-stable" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1264542mm0mffjcmw5sw34h94n405swz5z56rw1ragp3j62144iy"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "BUILD_CC=" #$(cc-for-target)))
           #:test-target "Tests"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'change-directory
                 (lambda _
                   (chdir "BaseTools")))
               (add-after 'change-directory 'disable-some-tools
                 (lambda _
                   ;; Disable building brotli and xz, since we package them
                   ;; separately, and it would require fetching submodules.
                   (substitute* "Source/C/GNUmakefile"
                     (("^[[:blank:]]+BrotliCompress[[:blank:]]+\\\\")
                      "\\")
                     (("^[[:blank:]]+LzmaCompress[[:blank:]]+\\\\")
                      "\\"))))
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
    (license license:bsd-2)))

(define-public ovmf
  (let ((commit "13a50a6fe1dcfa6600c38456ee24e0f9ecf51b5f")
        (revision "1"))
    (package
      (name "ovmf")
      (version (git-version "20170116" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; OVMF is part of the edk2 source tree.
                      (url "https://github.com/tianocore/edk2")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1gy2332kdqk8bjzpcsripx10896rbvgl0ic7r344kmpiwdgm948b"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ; No check target.
        #:phases
        #~(modify-phases %standard-phases
            ;; Hide the default GCC from CPLUS_INCLUDE_PATH to prevent it from
            ;; shadowing the version of GCC provided in native-inputs.
            (add-after 'set-paths 'hide-implicit-gcc
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((gcc (assoc-ref inputs "gcc")))
                  (setenv "CPLUS_INCLUDE_PATH"
                          (string-join
                           (delete (string-append gcc "/include/c++")
                                   (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                 #\:))
                           ":")))))
            (add-after 'unpack 'patch-source
              (lambda _
                (substitute* "edksetup.sh"
                  (("^return \\$\\?")
                   "exit $?"))))
            (replace 'configure
              (lambda _
                (let* ((cwd (getcwd))
                       (tools (string-append cwd "/BaseTools"))
                       (bin (string-append tools "/BinWrappers/PosixLike")))
                  (setenv "WORKSPACE" cwd)
                  (setenv "EDK_TOOLS_PATH" tools)
                  (setenv "PATH" (string-append (getenv "PATH") ":" bin))
                  (invoke "bash" "edksetup.sh")
                  (substitute* "Conf/target.txt"
                    (("^TARGET[ ]*=.*$") "TARGET = RELEASE\n")
                    (("^MAX_CONCURRENT_THREAD_NUMBER[ ]*=.*$")
                     (format #f "MAX_CONCURRENT_THREAD_NUMBER = ~a~%"
                             (number->string (parallel-job-count)))))
                  ;; Build build support.
                  (setenv "BUILD_CC" "gcc")
                  (invoke "make" "-C" tools))))
            (replace 'build
              (lambda _
                (invoke "build" "-a" "IA32" "-t" "GCC49"
                        "-p" "OvmfPkg/OvmfPkgIa32.dsc")))
            #$@(if (string=? "x86_64-linux" (%current-system))
                   #~((add-after 'build 'build-x64
                        (lambda _
                          (invoke "build" "-a" "X64" "-t" "GCC49"
                                  "-p" "OvmfPkg/OvmfPkgX64.dsc"))))
                   #~())
            (replace 'install
              (lambda _
                (let ((fmw (string-append #$output "/share/firmware")))
                  (mkdir-p fmw)
                  (copy-file "Build/OvmfIa32/RELEASE_GCC49/FV/OVMF.fd"
                             (string-append fmw "/ovmf_ia32.bin"))
                  #$@(if (string=? "x86_64-linux" (%current-system))
                         '((copy-file "Build/OvmfX64/RELEASE_GCC49/FV/OVMF.fd"
                                      (string-append fmw "/ovmf_x64.bin")))
                         '())))))))
      (native-inputs
       `(("acpica" ,acpica)
         ("gcc@5" ,gcc-5)
         ("nasm" ,nasm)
         ("python-2" ,python-2)
         ("util-linux" ,util-linux "lib")))
      (supported-systems '("x86_64-linux" "i686-linux"))
      (home-page "https://www.tianocore.org")
      (synopsis "UEFI firmware for QEMU")
      (description "OVMF is an EDK II based project to enable UEFI support for
Virtual Machines.  OVMF contains a sample UEFI firmware for QEMU and KVM.")
      (license (list license:expat
                     license:bsd-2 license:bsd-3 license:bsd-4)))))

(define-public ovmf-aarch64
  (package
    (inherit ovmf)
    (name "ovmf-aarch64")
    (native-inputs
     (append (package-native-inputs ovmf)
             (if (not (string-prefix? "aarch64" (%current-system)))
                 `(("cross-gcc" ,(cross-gcc "aarch64-linux-gnu"))
                   ("cross-binutils" ,(cross-binutils "aarch64-linux-gnu")))
                 '())))
    (arguments
     (substitute-keyword-arguments (package-arguments ovmf)
       ((#:phases phases)
        #~(modify-phases #$phases
            #$@(if (string-prefix? "aarch64" (%current-system))
                   '()
                   '((add-before 'configure 'set-env
                       (lambda _
                         (setenv "GCC49_AARCH64_PREFIX" "aarch64-linux-gnu-")))))
            (replace 'build
              (lambda _
                (invoke "build" "-a" "AARCH64" "-t" "GCC49"
                        "-p" "ArmVirtPkg/ArmVirtQemu.dsc")))
            (delete 'build-x64)
            (replace 'install
              (lambda _
                (let ((fmw (string-append #$output "/share/firmware")))
                  (mkdir-p fmw)
                  (copy-file "Build/ArmVirtQemu-AARCH64/RELEASE_GCC49/FV/QEMU_EFI.fd"
                             (string-append fmw "/ovmf_aarch64.bin")))))))))
    (supported-systems %supported-systems)))

(define-public ovmf-arm
  (package
    (inherit ovmf)
    (name "ovmf-arm")
    (native-inputs
     (append (package-native-inputs ovmf)
             (if (not (string-prefix? "armhf" (%current-system)))
                 `(("cross-gcc" ,(cross-gcc "arm-linux-gnueabihf"))
                   ("cross-binutils" ,(cross-binutils "arm-linux-gnueabihf")))
                 '())))
    (arguments
     (substitute-keyword-arguments (package-arguments ovmf)
       ((#:phases phases)
        #~(modify-phases #$phases
            #$@(if (string-prefix? "armhf" (%current-system))
                   '()
                   '((add-before 'configure 'set-env
                       (lambda _
                         (setenv "GCC49_ARM_PREFIX" "arm-linux-gnueabihf-")))))
            (replace 'build
              (lambda _
                (invoke "build" "-a" "ARM" "-t" "GCC49"
                        "-p" "ArmVirtPkg/ArmVirtQemu.dsc")))
            (delete 'build-x64)
            (replace 'install
              (lambda _
                (let ((fmw (string-append #$output "/share/firmware")))
                  (mkdir-p fmw)
                  (copy-file "Build/ArmVirtQemu-ARM/RELEASE_GCC49/FV/QEMU_EFI.fd"
                             (string-append fmw "/ovmf_arm.bin")))))))))
    (supported-systems %supported-systems)))

(define* (make-arm-trusted-firmware platform
                                    #:key (triplet "aarch64-linux-gnu"))
  (let ((native-build? (lambda ()
                         ;; Note: %current-system is a *triplet*, unlike its
                         ;; name would suggest.
                         (or (not triplet) ;disable cross-compilation
                             (string=? (%current-system)
                                       (gnu-triplet->nix-system triplet))))))
    (package
      (name (string-append "arm-trusted-firmware-" platform))
      (version "2.9")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; There are only GitHub generated release snapshots.
               (url "https://git.trustedfirmware.org/TF-A/trusted-firmware-a.git/")
               (commit (string-append "v" version))))
         (file-name (git-file-name "arm-trusted-firmware" version))
         (sha256
          (base32
           "16fjbn1zck0d8b554h8lk1svqqn0zlawvrlkjxry9l71s9h4vd0p"))
         (snippet
          #~(begin
              (use-modules (guix build utils))
              ;; Remove binary blobs which do not contain source or proper
              ;; license.
              (for-each (lambda (file)
                          (delete-file file))
                        (find-files "." "\\.bin$"))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:target (and (not (native-build?)) triplet)
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)         ;no configure script
            (replace 'install
              (lambda _
                (for-each (lambda (file)
                            (install-file file #$output))
                          (find-files "." "\\.(bin|elf)$")))))
        #:make-flags #~(list (string-append "PLAT=" #$platform)
                             #$@(if (not (native-build?))
                                    (list (string-append "CROSS_COMPILE=" triplet "-"))
                                    '())
                             "DEBUG=1")
        #:tests? #f))                   ;no test suite
      (home-page "https://www.trustedfirmware.org/")
      (synopsis "Implementation of \"secure world software\"")
      (description
       "ARM Trusted Firmware provides a reference implementation of secure world
software for ARMv7A and ARMv8-A, including a Secure Monitor executing at
@dfn{Exception Level 3} (EL3).  It implements various ARM interface standards,
such as:
@enumerate
@item The Power State Coordination Interface (PSCI)
@item Trusted Board Boot Requirements (TBBR, ARM DEN0006C-1)
@item SMC Calling Convention
@item System Control and Management Interface
@item Software Delegated Exception Interface (SDEI)
@end enumerate\n")
      (license (list license:bsd-3
                     license:bsd-2))))) ; libfdt

(define-public arm-trusted-firmware-sun50i-a64
  (let ((base (make-arm-trusted-firmware "sun50i_a64")))
    (package
      (inherit base)
      (name "arm-trusted-firmware-sun50i-a64"))))

(define-public arm-trusted-firmware-rk3328
  (make-arm-trusted-firmware "rk3328"))

(define-public arm-trusted-firmware-rk3399
  (let ((base (make-arm-trusted-firmware "rk3399")))
    (package
      (inherit base)
      (name "arm-trusted-firmware-rk3399")
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend
             (cross-gcc "arm-none-eabi")
             (cross-binutils "arm-none-eabi")))))))

(define-public arm-trusted-firmware-imx8mq
  (let ((base (make-arm-trusted-firmware "imx8mq")))
    (package
      (inherit base)
      ;; Newer versions do not build and are essentially not supported
      ;; upstream.
      ;; XXX: explore using NXP maintained branch
      ;; https://github.com/nxp-imx/imx-atf
      (version "2.8")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; There are only GitHub generated release snapshots.
               (url "https://git.trustedfirmware.org/TF-A/trusted-firmware-a.git/")
               (commit (string-append "v" version))))
         (file-name (git-file-name "arm-trusted-firmware" version))
         (sha256
          (base32
           "0grq3fgxi9xhcljnhwlxjvdghyz15gaq50raw41xy4lm8rkmnzp3"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:make-flags flags ''())
          ;; Adding debug symbols causes the size to exceed limits.
          #~(delete "DEBUG=1" #$flags)))))))

(define (make-crust-package platform)
  (package
    (name (string-append "crust-"
                         (string-replace-substring platform "_" "-")))
    (version "0.5")
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
         "0xgbbhifg3miwd3yp6jq9kp7nqgz5gzy00w95vba45j8jk5vjvvz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:make-flags
      (let ((triplet-without-vendor
             (and (%current-target-system)
                  (match (string-split (nix-system->gnu-triplet
                                        (%current-target-system)) #\-)
                    ((arch vendor os ..1)
                     (string-join `(,arch ,@os) "-"))))))
        #~(list "CROSS_COMPILE=or1k-elf-"
                "V=1"
                #$@(if triplet-without-vendor
                       ;; We are cross-compiling the tools, intended to be
                       ;; executable for the target system.
                       (list (string-append "HOSTAR=" triplet-without-vendor
                                            "-ar")
                             (string-append "HOSTCC=" triplet-without-vendor
                                            "-gcc"))
                       ;; Not cross-compiling.
                       (list "HOSTAR=ar"
                             "HOSTCC=gcc"))
                "LEX=flex"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-build-tests
            (lambda _
              ;; Attempting to build the tools test binary on a non-aarch64
              ;; architecture fails with: "No cache cleaning implementation
              ;; available for this architecture".  Avoid building it (see:
              ;; https://github.com/crust-firmware/crust/issues/182).
              (substitute* "tools/Makefile"
                (("tools-y \\+= test") ""))))
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
                        (find-files "." "(scp\\.bin|\\.config)$"))
              (install-file "build/tools/load"
                            (string-append #$output "/bin")))))))
    ;; The firmware is cross-compiled using a "bare bones" compiler (no libc).
    ;; Use our own tool chain for that.
    (native-inputs
     (list bison
           (cross-gcc "or1k-elf")
           (cross-binutils "or1k-elf")
           flex))
    (home-page "https://github.com/crust-firmware/crust")
    (synopsis "System control processor firmware for Allwinner sunxi boards")
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
    (license (list license:bsd-3 license:gpl2))))

(define-public crust-pinebook
  (make-crust-package "pinebook"))

(define-public crust-pine64-plus
  (make-crust-package "pine64_plus"))

(define-public qmk
  (package
    (name "qmk")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "qmk" version))
              (sha256
               (base32
                "1619q9v90740dbg8xpzqlhwcasz42xj737803aiip8qc3a7zhwgq"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f)) ;; No tests.
    (propagated-inputs (list python-dotty-dict python-hid python-hjson
                             python-jsonschema python-milc python-pillow
                             python-pygments python-pyserial python-pyusb))
    (home-page "https://qmk.fm")
    (synopsis "Command line utility to manage QMK keyboard firmwares")
    (description "This package provides a program to help users work with
@acronym{QMK, Quantum Mechanical Keyboard} firmwares.")
    (license license:expat)))
