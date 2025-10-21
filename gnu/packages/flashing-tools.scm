;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022 Peter Polidoro <peter@polidoro.io>
;;; Copyright © 2022 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2023 B. Wilson <x@wilsonb.com>
;;; Copyright © 2023 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024, 2025 Cayetano Santos <csantosb@inventati.org>
;;; Copyright © 2025 Joaquín Aguirrezabalaga <kinote@kinote.org>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Ian Eure <ian@retrospec.tv>
;;; Copyright © 2023 Thomas Albers Raviola <thomas@thomaslabs.org>
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

(define-module (gnu packages flashing-tools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages electronics)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-26))

(define-public flashrom
  (package
    (name "flashrom")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.flashrom.org/releases/flashrom-v"
             version ".tar.xz"))
       (sha256
        (base32
         "08s4r7abcyk849zk840l3szgdmaxj0bx1281wy0zrdgrgncb77cb"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~'("-Dprogrammer=all")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "dmi.c"
                (("(dmidecode)( 2>/dev/null)" _ command suffix)
                 (string-append
                  (search-input-file
                   inputs (in-vicinity "sbin" command))
                  suffix))))))))
    (native-inputs
     (list cmocka pkg-config))
    (inputs
     (list bash-minimal dmidecode pciutils libusb libftdi libjaylink))
    (home-page "https://flashrom.org/")
    (synopsis "Identify, read, write, erase, and verify ROM/flash chips")
    (description
     "flashrom is a utility for identifying, reading, writing,
verifying and erasing flash chips.  It is designed to flash
BIOS/EFI/coreboot/firmware/optionROM images on mainboards,
network/graphics/storage controller cards, and various other
programmer devices.")
    (license license:gpl2)))

(define-public 0xffff
  (package
    (name "0xffff")
    (version "0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pali/0xffff")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nqbrr64kjr0h3h6gzhrj1vd106nni4y9mhjdr8mh2x9lcgn4fj5"))))
    (build-system gnu-build-system)
    (inputs
     ;; Building with libusb-compat will succeed but the result will be broken.
     ;; See <https://github.com/pali/0xFFFF/issues/3>.
     (list libusb-0.1))
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)) ;no configure
      #:make-flags #~(list (string-append "CC="
                                          #$(cc-for-target)) "HOST_CC=gcc"
                           "BUILD_DATE=GNU Guix"
                           (string-append "PREFIX=" %output))
      #:tests? #f)) ;no 'check' target
    (home-page "https://github.com/pali/0xFFFF")
    (synopsis "Flash FIASCO images on Maemo devices")
    (description
     "The Open Free Fiasco Firmware Flasher (0xFFFF) is a flashing tool
for FIASCO images.  It supports generating, unpacking, editing and
flashing of FIASCO images for Maemo devices.  Use it with care.  It can
brick your device.")
    (license license:gpl3+)))

(define-public avrdude
  (package
    (name "avrdude")
    (version "8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/avrdudes/avrdude/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jfz0z8i5vib181f854zwxd8avw5fy59lh3d78igqxwm08sv8nlb"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f))                     ;no tests
    (inputs
     (list libelf libusb-compat libftdi libserialport readline))
    (native-inputs
     (list bison flex python-wrapper))
    (home-page "https://www.nongnu.org/avrdude/")
    (synopsis "AVR downloader and uploader")
    (description
     "@code{AVRDUDE} is a utility to download/upload/manipulate the ROM and
EEPROM contents of AVR microcontrollers using the @acronym{ISP, in-system
programming} technique.")
    (license license:gpl2+)))

(define-public dfu-programmer
  (package
    (name "dfu-programmer")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dfu-programmer/dfu-programmer")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1y9ayl97qdy36bmlcf5vrg85jn72pgx7blgxd1albk79r87q2632"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list libusb))
    (home-page "https://dfu-programmer.github.io/")
    (synopsis "Device firmware update programmer for Atmel chips")
    (description
     "Dfu-programmer is a multi-platform command-line programmer for
Atmel (8051, AVR, XMEGA & AVR32) chips with a USB bootloader supporting
ISP.")
    (license license:gpl2+)))

(define-public dfu-util
  (package
    (name "dfu-util")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://dfu-util.sourceforge.net/releases/dfu-util-"
                    version ".tar.gz"))
              (sha256
               (base32
                "17piiyp08pccqmbhnswv957lkypmmm92kps79hypxvw23ai3pddl"))))
    (build-system gnu-build-system)
    (inputs
     (list libusb))
    (native-inputs
     (list pkg-config))
    (synopsis "Host side of the USB Device Firmware Upgrade (DFU) protocol")
    (description
     "The DFU (Universal Serial Bus Device Firmware Upgrade) protocol is
intended to download and upload firmware to devices connected over USB.  It
ranges from small devices like micro-controller boards up to mobile phones.
With dfu-util you are able to download firmware to your device or upload
firmware from it.")
    (home-page "https://dfu-util.sourceforge.net/")
    (license license:gpl2+)))

(define-public teensy-loader-cli
  (package
    (name "teensy-loader-cli")
    (version "2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PaulStoffregen/teensy_loader_cli")
             (commit version)))
       (sha256
        (base32 "0kqjmbmns3ansmrs6pbpsqk0g4d82hxknpng6lp7375zccsq52im"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; Remove example flash files and teensy rebooter flash binaries.
           (for-each delete-file (find-files "." "\\.(elf|hex)$"))))
       (patches (search-patches "teensy-loader-cli-help.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;; Makefile has no test target
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'install
            (lambda _
              (install-file "teensy_loader_cli"
                            (string-append #$output "/bin")))))))
    (inputs
     (list libusb-compat))       ;only compatible with libusb 0.1
    (synopsis "Command line firmware uploader for Teensy development boards")
    (description
     "The Teensy loader program communicates with your Teensy board when the
HalfKay bootloader is running, so you can upload new programs and run them.

You need to add the udev rules to make the Teensy update available for
non-root users.")
    (home-page "https://www.pjrc.com/teensy/loader_cli.html")
    (license license:gpl3+)))

(define-public teensy-udev-rules
  (package
    (name "teensy-udev-rules")
    (version "20230913")      ;no version whatsoever -- use the current date
    (source (origin
              (method url-fetch)
              (uri "https://www.pjrc.com/teensy/00-teensy.rules")
              (sha256
               (base32
                "1yxczxvwi0s31g7lfa4v13yvvpv6gcsfs7r9mv6y4w9jc1inpx8p"))))
    (build-system copy-build-system)
    (arguments
     (let ((rules-file "lib/udev/rules.d/70-teensy.rules"))
       (list
        #:install-plan
        #~(list `(,#$source #$rules-file))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'patch-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* (string-append #$output "/" #$rules-file)
                  (("/bin/stty")
                   (search-input-file inputs "bin/stty")))))))))
    (inputs (list coreutils-minimal))
    (home-page "https://www.pjrc.com/teensy/loader_cli.html")
    (synopsis "udev rules for the Teensy family of micro-controllers")
    (description "This package provides a udev rules file that allows
unprivileged users communication with the Teensy family of micro-controllers.
ModemManager (part of NetworkManager) can interfere with USB Serial devices,
which includes the Teensy.  In case of problems, see the @file{.rules} file
for possible workarounds.")
    ;; FIXME: The file lacks an explicit license, so the license of
    ;; teensy-cli-loader is *assumed* to hold (see:
    ;; https://github.com/PaulStoffregen/teensy_loader_cli/issues/84).
    (license license:gpl3+)
    (supported-systems (filter (cut string-suffix? "-linux" <>)
                               %supported-systems))))

(define-public rkflashtool
  (let ((commit "6022dd724e8247ff7a0825b0eda6a07c446aacdd")
        (revision "2"))
    (package
      (name "rkflashtool")
      (version (git-version "5.2" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/linux-rockchip/rkflashtool")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0s4zir2s0c3igplj42lq7bq0f0416nf9hrprbxzm87c9mvsdhyvv"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)) ; no configure
        #:make-flags
        #~(list (string-append "PREFIX=" #$output))
        #:tests? #f)) ; no tests
      (native-inputs
       (list pkg-config))
      (inputs
       (list libusb))
      (home-page "https://github.com/linux-rockchip/rkflashtool")
      (synopsis "Tools for flashing Rockchip devices")
      (description "Allows flashing of Rockchip based embedded linux devices.
The list of currently supported devices is: RK2818, RK2918, RK2928, RK3026,
RK3036, RK3066, RK312X, RK3168, RK3188, RK3288, RK3368.")
      (license license:bsd-2))))

(define-public heimdall
  (package
    (name "heimdall")
    (version "2.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.sr.ht/~grimler/Heimdall")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08sxn48jljrcily77g8yj89rhzwk0gjrz9lkglgrwl587x6q7sf7"))))
    (build-system qt-build-system)
    (arguments
     `(#:build-type "Release"
       #:qtbase ,qtbase
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-invocations
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("heimdall-frontend/source/aboutform.cpp"
                            "heimdall-frontend/source/mainwindow.cpp")
               (("start[(]\"heimdall\"")
                (string-append "start(\"" (assoc-ref outputs "out")
                               "/bin/heimdall\"")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (lib (string-append (assoc-ref outputs "out") "/lib")))
               (install-file "bin/heimdall" bin)
               (install-file "bin/heimdall-frontend" bin)
               (install-file "libpit/libpit.a" lib)
               #t))))))
    (inputs
     (list libusb qtwayland zlib))
    (native-inputs (list pkg-config))
    (home-page "https://glassechidna.com.au/heimdall/")
    (synopsis "Flash firmware onto Samsung mobile devices")
    (description "@command{heimdall} is a tool suite used to flash firmware (aka
ROMs) onto Samsung mobile devices.  Heimdall connects to a mobile device over
USB and interacts with low-level software running on the device, known as Loke.
Loke and Heimdall communicate via the custom Samsung-developed protocol typically
referred to as the \"Odin 3 protocol\".")
    (license license:expat)))

(define-public ifdtool
  (package
    (name "ifdtool")
    (version "25.09")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://review.coreboot.org/coreboot")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1a1n64dwr5fzdnaj45bjci85ap5yra5gwz4x056zn6481xwvbsmv"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              "INSTALL=install"
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "util/ifdtool")))
          (delete 'configure))           ; no configure script
      #:tests? #f))                    ; no test suite
    (home-page "https://doc.coreboot.org/util/ifdtool/")
    (synopsis "Intel Firmware Descriptor dumper")
    (description "This package provides @command{ifdtool}, a program to
dump Intel Firmware Descriptor data of an image file.")
    (license license:gpl2)))

(define-public intelmetool
  (package
    (name "intelmetool")
    (version "25.09")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://review.coreboot.org/coreboot")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a1n64dwr5fzdnaj45bjci85ap5yra5gwz4x056zn6481xwvbsmv"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              "INSTALL=install"
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "util/intelmetool")))
          (delete 'configure) ;no configure script
          (delete 'check))))
    (inputs (list pciutils zlib))
    (home-page
     "https://github.com/coreboot/coreboot/tree/main/util/intelmetool/")
    (synopsis "Intel Management Engine tools")
    (description "This package provides tools for working with Intel
Management Engine (ME).  You need to @code{sudo rmmod mei_me} and
@code{sudo rmmod mei} before using this tool.  Also pass
@code{iomem=relaxed} to the Linux kernel command line.")
    (license license:gpl2)

    ;; This is obviously an Intel thing, plus it requires <cpuid.h>.
    (supported-systems '("x86_64-linux" "i686-linux"))))

(define-public me-cleaner
  (package
    (name "me-cleaner")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/corna/me_cleaner")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "1bdj2clm13ir441vn7sv860xsc5gh71ja5lc2wn0gggnff0adxj4"))
              (file-name (git-file-name name version))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1")))
               (install-file "man/me_cleaner.1" man)
               #t))))))
    (home-page "https://github.com/corna/me_cleaner")
    (synopsis "Intel ME cleaner")
    (description "This package provides tools for disabling Intel
ME as far as possible (it only edits ME firmware image files).")
    (license license:gpl3+)

    ;; This is an Intel thing.
    (supported-systems '("x86_64-linux" "i686-linux"))))

(define-public minipro
  ;; When built from a Git repo, minipro expects GIT_DATE to be set to the
  ;; value of `git show -s --format=%ci'.  When updating the package, run this
  ;; in a checkout and put the value here.
  (let* ((date "2025-04-13 21:54:38 -0700"))
    (package
      (name "minipro")
      (version "0.7.3")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://gitlab.com/DavidGriffith/minipro.git")
                (commit version)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1525rn5h73xism16vmivd3cz93g8w76h24f0yvbpc35ydc3fkqf7"))))
      (native-inputs (list pkg-config which))
      (inputs (list libusb zlib))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ; no test suite
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure) ; No ./configure script
            (add-before 'build 'fix-makefile
              (lambda _
                ;; Fix some git related variables that minipro expects
                (substitute* "Makefile"
                  (("GIT_BRANCH = .*")
                   (string-append "GIT_BRANCH = \"master\"\n"))
                  (("GIT_HASH = .*")
                   (string-append "GIT_HASH = \"" #$version "\"\n"))
                  (("GIT_DATE = .*")
                   (string-append "GIT_DATE = \"" #$date "\"\n"))))))
        #:make-flags
        #~(list (string-append "VERSION=" #$version)
                (string-append "PREFIX=" #$output)
                (string-append "UDEV_DIR=" #$output "/lib/udev")
                (string-append "COMPLETIONS_DIR=" #$output
                               "/share/bash-completion/completions"))))
      (synopsis "Controls the TL866xx series of chip programmers")
      (description
       "minipro is designed to program or read the contents of
chips supported by the TL866xx series of programmers.  This includes many
microcontrollers, ROMs, EEPROMs and PLDs.

To use this program without root privileges you must install the necessary udev
rules.  This can be done by extending @code{udev-service-type} in your
@code{operating-system} configuration with this package.  E.g.:
@code{(udev-rules-service 'minipro minipro #:groups '(\"plugdev\")}.
Additionally your user must be member of the @code{plugdev} group.")
      (home-page "https://gitlab.com/DavidGriffith/minipro")
      (license license:gpl3+))))

(define-public uefitool
  (package
    (name "uefitool")
    (version "0.28.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/LongSoft/UEFITool")
                     (commit version)))
              (sha256
               (base32
                "1n2hd2dysi5bv2iyq40phh1jxc48gdwzs414vfbxvcharcwapnja"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (invoke "qmake" "-makefile")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "UEFITool" (string-append (assoc-ref outputs "out")
                                                     "/bin")))))))
    (inputs
     (list qtbase-5))
    (home-page "https://github.com/LongSoft/UEFITool/")
    (synopsis "UEFI image editor")
    (description "@code{uefitool} is a graphical image file editor for
Unifinished Extensible Firmware Interface (UEFI) images.")
    (license license:bsd-2)))

(define-public openfpgaloader
  (package
    (name "openfpgaloader")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/trabucayre/openfpgaloader")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00xr4dzd1mlc1k4rivh9ibmdlx6yizb016laad10dkhjqfz1ixhq"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs (list eudev
                  hidapi
                  libftdi
                  libgpiod
                  libusb
                  zlib))
    (arguments
     (list #:tests? #f                  ;no test suite
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-rules
                 (lambda _
                   (install-file
                    "../source/99-openfpgaloader.rules"
                    (string-append #$output "/lib/udev/rules.d/")))))))
    (synopsis "Utility for programming FPGA")
    (description "This package provides a program to transfer a bitstream
to an FPGA.  To use @code{openfpgaloader} without root privileges it is
necessary to install the necessary udev rules.  This can be done by extending
@code{udev-service-type} in the @code{operating-system} configuration file with
this package, as in:
@lisp
(udev-rules-service 'openfpgaloader openfpgaloader #:groups '(\"plugdev\")
@end lisp
Additionally, the @samp{plugdev} group should be registered in the
@code{supplementary-groups} field of your @code{user-account} declaration. Refer
to @samp{info \"(guix) Base Services\"} for examples.")
    (home-page "https://trabucayre.github.io/openFPGALoader/")
    (license license:asl2.0)))

(define-public srecord
  (package
    (name "srecord")
    (version "1.65.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/srecord/srecord/"
                           (version-major+minor version) "/"
                           "srecord-" version "-Source.tar.gz"))
       (sha256
        (base32 "0i3n6g8i28xx8761nadm6p2nf9y31bywx0isyi0h9rawy5yd1hw1"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Fix building without Git.  Upstream tries to allow it but is buggy.
           (substitute* "etc/configure.cmake"
             (("\\(GIT_SHA1\\)") "(FALSE)"))
           ;; It also tries to install the entire RUNTIME_DEPENDENCY_SET of
           ;; each executable: libm, libc, libstc++ & more!  Get the cluehammer.
           (substitute* "etc/packaging.cmake"
             ((".*# Find standard library DLL.*" match)
              "ENDFUNCTION()\n\nFUNCTION(WTF no)\n"))
           ;; Now stop it from deliberately clobbering -DCMAKE_INSTALL_PREFIX.
           (substitute* "CMakeLists.txt"
             (("set\\(CMAKE_INSTALL_PREFIX") "#"))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'make-tests-executable
            (lambda _
              (for-each
               (cut chmod <> #o755)
               ;; We're in a parallel build directory to the sources and tests.
               (find-files ".." "\\.sh$")))))))
    (inputs
     (list boost libgcrypt))
    (native-inputs
     (list doxygen
           ghostscript                ; for ps2pdf
           graphviz                   ; the build scripts call this ‘doxygen’…
           groff
           psutils
           ;; For the tests.
           diffutils
           which
           ;; XXX Work around Guix's currently-broken psutils package.  Remove
           ;; both and maybe (gnu packages perl) when core-updates is merged.
           perl
           perl-ipc-run3))
    (home-page "https://srecord.sourceforge.net/")
    (synopsis "Tools for EPROM files")
    (description "The SRecord package is a collection of powerful tools for
manipulating EPROM load files.  It reads and writes numerous EPROM file
formats, and can perform many different manipulations.")
    (license license:gpl3+)))

(define-public uuu
  (package
    (name "uuu")
    (version "1.5.233")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NXPmicro/mfgtools")
             (commit (string-append "uuu_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08pck42sywg0ibj79lhd3iv9z6bvr5g5bqvkls477x9x85nbsw67"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                      ; no tests
      #:modules '((guix build utils)
                  (ice-9 popen)
                  (srfi srfi-26)
                  (guix build cmake-build-system))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-version-gen
            (lambda _
              (call-with-output-file ".tarball-version"
                (lambda (port)
                  (display #$version port)))))
          (add-after 'install 'install-udev-rules
            (lambda _
              (let* ((uuu (string-append #$output "/bin/uuu"))
                     (pipe (open-pipe* OPEN_READ uuu "-udev"))
                     (rules
                      (string-append
                       #$output "/lib/udev/rules.d/70-uuu.rules")))
                (mkdir-p (string-append #$output "/lib/udev/rules.d"))
                (call-with-output-file rules
                  (cut dump-port pipe <>))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libusb bzip2 zlib libzip openssl tinyxml2
           `(,zstd "lib")))
    (home-page "https://github.com/NXPmicro/mfgtools")
    (synopsis "Freescale/NXP I.MX chip image deploy tools")
    (description "@code{uuu} is a command line tool, evolved out of MFGTools.
It can be used to upload images to I.MX SoC's using at least their boot ROM.")
    (license license:bsd-3)))

;; The upstream repository has been archived on 2024-08-21,
;; the replacement Keymapp does not seem to be free software.
;; Keep it as long as it builds and works and no replacement is in sight.
(define-public wally-cli
  (let ((commit "b0fafe52cc7fb9d55f2b968d4548c99917c7325c")
        (revision "0"))
    (package
      (name "wally-cli")
      (version (git-version "2.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zsa/wally-cli")
               (commit commit)))
         (sha256
          (base32 "09phq2g51x7rlalzb87aqf48p3j4s7s5jdf5vdf48l9805hi2yha"))
         (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       (list
        #:install-source? #f
        #:import-path "github.com/zsa/wally-cli"
        #:phases
        #~(modify-phases %standard-phases
            ;; XXX: Upstream Golang module name was changed from
            ;; <gopkg.in/cheggaaa/pb.v1> to <github.com/cheggaaa/pb>, adjust
            ;; references to it accordingly. Remove it in the new release of
            ;; the package.
            (add-after 'unpack 'fix-module-name
              (lambda* (#:key import-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" import-path)
                  (substitute* "main.go"
                    (("gopkg.in/cheggaaa/pb.v1") "github.com/cheggaaa/pb"))))))))
      (native-inputs
       (list go-github-com-briandowns-spinner
             go-github-com-google-gousb
             go-github-com-logrusorgru-aurora
             go-github-com-marcinbor85-gohex
             go-github-com-cheggaaa-pb
             pkg-config))
      (home-page "https://ergodox-ez.com/pages/wally")
      (synopsis "Flashing tool for ZSA keyboards")
      (description
       "This tool is for flashing custom layouts to
@url{https://ergodox-ez.com/,ZSA keyboards}.")
      (license license:expat))))

(define-public wlink
  (package
    (name "wlink")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wlink" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05l6h5d4w181sg00nq0l1808l0zc4fdda6syvgm7ba31glj7xkd4"))))
    (build-system cargo-build-system)
    (arguments `(#:install-source? #f))
    (native-inputs (list pkg-config))
    (inputs (cons* eudev libusb (cargo-inputs 'wlink)))
    (home-page "https://github.com/ch32-rs/wlink")
    (synopsis "Unofficial WCH-Link command line tool")
    (description
     "This package is an unofficial command line tool for use with WCH-Link, a
USB debbuger tool for flashing and debugging WCH RISC-V and ARM
microcontrollers.  This tool is still in development and may not be ready for
production use.")
    (license (list license:asl2.0 license:expat))))

(define-public qdl
  (let ((commit "13681fcb359c9f9c32a17a91d3dd20df2e413b6d")
        (revision "1"))
    (package
      (name "qdl")
      (version (git-version "1.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.linaro.org/landing-teams/working/qualcomm/qdl.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32 "0m6wdhfwbf7gzlckxx21bvbv33qjahpzqbg1pdqdd1lifx5f51mj"))))
      (build-system gnu-build-system)
      (native-inputs (list libxml2))
      (inputs (list eudev))
      (arguments
       (list
        #:tests? #f  ; No tests implemented
        #:make-flags
        #~(list (string-append "CC=" #$(cc-for-target))
                (string-append "prefix=" #$output))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure))))
      (home-page "https://git.linaro.org/landing-teams/working/qualcomm/qdl")
      (synopsis "Qualcomm EDL mode flashing tool")
      (description "This tool communicates with USB devices of id 05c6:9008 to
upload a flash loader and use this to flash images.")
      (license license:bsd-3))))
