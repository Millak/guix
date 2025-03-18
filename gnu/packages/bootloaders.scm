;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2019, 2021, 2023-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2020, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016-2018, 2021-2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016, 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2018, 2020-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 nee <nee@cock.li>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018, 2019, 2020 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022, 2023 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2021 Stefan <stefan-guix@vodafonemail.de>
;;; Copyright © 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023-2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (gnu packages bootloaders)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

(define unifont
  ;; GNU Unifont, <http://gnu.org/s/unifont>.
  ;; GRUB needs it for its graphical terminal, gfxterm.
  (origin
    (method url-fetch)
    (uri
     "http://unifoundry.com/pub/unifont-7.0.06/font-builds/unifont-7.0.06.bdf.gz")
    (sha256
     (base32
      "0p2vhnc18cnbmb39vq4m7hzv4mhnm2l0a2s7gx3ar277fwng3hys"))))

(define-public grub
  (package
    (name "grub")
    (version "2.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/grub/grub-" version ".tar.xz"))
              (sha256
               (base32
                "1ahgzvvvwdxx7rl08pv5dyqlgp76jxz0q2cflxvsdsn4yy8p7jgk"))
              (patches (search-patches
                        "grub-efi-fat-serial-number.patch"
                        "grub-setup-root.patch"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   ;; Add file missing from the release tarball.
                   (call-with-output-file "grub-core/extra_deps.lst"
                     (lambda (port)
                       (display "depends bli part_gpt\n" port)))

                   ;; Use exit code 77, not 99, to tell Automake that a test
                   ;; is skipped.
                   (substitute* (find-files "tests" "\\.in$")
                     (("exit 99") "exit 77"))))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           ;; Counterintuitively, this *disables* a spurious Python dependency by
           ;; calling the ‘true’ binary instead.  Python is only needed during
           ;; bootstrapping (for genptl.py), not when building from a release.
           #~(append
               (list "PYTHON=true")
                    ;; This needs to be compiled with clang for powerpc64le.
                    (if #$(and=> (%current-target-system)
                                 target-ppc64le?)
                        (list "TARGET_CC=powerpc64le-linux-gnu-clang")
                        '())
                    (if #$(and (target-ppc64le? (%current-system))
                               (not (%current-target-system)))
                        (list "CC=clang")
                        '()))

           ;; GRUB fails to load modules stripped with --strip-unneeded.
           #:strip-flags
           #~(list "--strip-debug" "--enable-deterministic-archives")

           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-stuff
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   (substitute* "grub-core/Makefile.in"
                     (("/bin/sh") (which "sh")))

                   ;; Give the absolute file name of 'mdadm', used to determine the
                   ;; root file system when it's a RAID device.  Failing to do that,
                   ;; 'grub-probe' silently fails if 'mdadm' is not in $PATH.
                   (let ((mdadm (false-if-exception
                                 (search-input-file inputs "/sbin/mdadm"))))
                     (when mdadm
                       (substitute* "grub-core/osdep/linux/getroot.c"
                         (("argv\\[0\\] = \"mdadm\"")
                          (string-append "argv[0] = \"" mdadm "\"")))))

                   ;; Make the font visible.
                   (copy-file #+unifont "unifont.bdf.gz")
                   (system* "gunzip" "unifont.bdf.gz")

                   ;; Give the absolute file name of 'ckbcomp'.
                   (substitute* "util/grub-kbdcomp.in"
                     (("^ckbcomp ")
                      (string-append
                       (search-input-file inputs "/bin/ckbcomp")
                       " ")))))
               (add-after 'unpack 'set-freetype-variables
                 ;; These variables need to be set to the native versions of the
                 ;; dependencies because they are used to build programs which are
                 ;; executed during build time.
                 (lambda* (#:key native-inputs #:allow-other-keys)
                   (when (assoc-ref native-inputs "freetype")
                     (let ((freetype (assoc-ref native-inputs "freetype")))
                       (setenv "BUILD_FREETYPE_LIBS"
                               (string-append "-L" freetype
                                              "/lib -lfreetype"))
                       (setenv "BUILD_FREETYPE_CFLAGS"
                               (string-append "-I" freetype
                                              "/include/freetype2"))))))
               #$@(if (target-hurd64?)
                      #~((add-after 'unpack 'apply-hurd64-patch
                           (lambda _
                             (let ((patch
                                    #$(local-file
                                       (search-patch "grub-hurd64.patch"))))
                               (invoke "patch" "--force" "-p1" "-i" patch)))))
                      #~())
               (add-before 'check 'disable-flaky-test
                 (lambda _
                   ;; This test is unreliable. For more information, see:
                   ;; <https://bugs.gnu.org/26936>.
                   (substitute* "Makefile.in"
                     (("grub_cmd_date grub_cmd_set_date grub_cmd_sleep")
                      "grub_cmd_date grub_cmd_sleep"))))
               #$@(if (target-ppc64le?)
                      #~((add-before 'check 'skip-tests
                           (lambda _
                             (substitute* "Makefile.in"
                               ((" grub_cmd_date ") " ")
                               ((" pseries_test ") " ")))))
                      #~())
               (add-before 'check 'disable-pixel-perfect-test
                 (lambda _
                   ;; This test compares many screenshots rendered with an older
                   ;; Unifont (9.0.06) than that packaged in Guix.
                   (substitute* "Makefile.in"
                     (("test_unset grub_func_test")
                      "test_unset")))))

           ;; Disable tests on ARM and AARCH64 platforms or when cross-compiling.
           #:tests? (not (or (any (cute string-prefix? <> (%current-system))
                                  '("arm" "aarch64"))
                             (%current-target-system)))))
    (inputs
     (append (list gettext-minimal freetype ncurses

                   ;; Console-setup's ckbcomp is invoked by grub-kbdcomp.  It
                   ;; is required for generating alternative keyboard layouts.
                   console-setup)

             ;; 64-bit PowerPC hardware boots in big-endian mode and then for
             ;; powerpc64le it switches to little-endian mode.  Therefore we
             ;; need a compiler which can generate both big-endian and
             ;; little-endian binaries for the bootloader and the utilities
             ;; and building with clang is the easiest option.
             (if (target-ppc64le?)
                 (list clang)
                 '())

             ;; Depend on LVM2 for libdevmapper, used by 'grub-probe' and
             ;; 'grub-install' to recognize mapped devices (LUKS, etc.)
             (if (member (or (%current-target-system)
                               (%current-system))
                           (package-supported-systems lvm2))
                 (list lvm2)
                 '())

             ;; Depend on mdadm, which is invoked by 'grub-probe' and
             ;; 'grub-install' to determine whether the root file system is
             ;; RAID.
             (if (member (or (%current-target-system)
                             (%current-system))
                         (package-supported-systems mdadm))
                 (list mdadm)
                 '())

             ;; Needed for ‘grub-mount’, the only reliable way to tell whether
             ;; a given file system will be readable by GRUB without
             ;; rebooting.
             (if (member (or (%current-target-system)
                             (%current-system))
                           (package-supported-systems fuse-2))
                 (list fuse-2)
                 '())))
    (native-inputs
     (append (list pkg-config
                   bison
                   flex
                   texinfo
                   help2man
                   freetype       ;native version needed for build-grub-mkfont

                   ;; Dependencies of the test suite.
                   parted
                   xorriso)

             ;; For the test suite, the "real" QEMU is needed because several
             ;; targets are used.
             (if (member (%current-system) (package-supported-systems qemu-minimal))
                 (list qemu-minimal)
                 '())

             ;; XXX: When building GRUB 2.02 on 32-bit x86, we need a binutils
             ;; capable of assembling 64-bit instructions.  However, our default
             ;; binutils on 32-bit x86 is not 64-bit capable.
             (if (string-match "^i[3456]86-" (%current-system))
                 (let ((binutils (package/inherit
                                     binutils
                                   (name "binutils-i386")
                                   (arguments
                                    (substitute-keyword-arguments (package-arguments binutils)
                                      ((#:configure-flags flags ''())
                                       #~(cons* "--enable-64-bit-bfd" #$flags)))))))
                   (list (make-ld-wrapper "ld-wrapper-i386"
                                          #:binutils binutils)
                         binutils))
                 '())))
    (home-page "https://www.gnu.org/software/grub/")
    (synopsis "GRand Unified Boot loader")
    (description
     "GRUB is a multiboot bootloader.  It is used for initially loading the
kernel of an operating system and then transferring control to it.  The kernel
then goes on to load the rest of the operating system.  As a multiboot
bootloader, GRUB handles the presence of multiple operating systems installed
on the same computer; upon booting the computer, the user is presented with a
menu to select one of the installed operating systems.")
    (license license:gpl3+)
    (properties '((cpe-name . "grub2")))))

(define-public grub-minimal
  (package
    (inherit grub)
    (name "grub-minimal")
    (inputs
     (modify-inputs (package-inputs grub)
       (delete "lvm2" "mdadm" "fuse" "console-setup")))
    (native-inputs
     (modify-inputs (package-native-inputs grub)
       (delete "help2man" "texinfo" "parted" "qemu" "qemu-minimal" "xorriso")))
    (arguments
     (substitute-keyword-arguments (package-arguments grub)
       ((#:tests? _ #t) #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'patch-stuff
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (substitute* "grub-core/Makefile.in"
                  (("/bin/sh")
                   (which "sh")))

                ;; Make the font visible.
                (copy-file #+unifont "unifont.bdf.gz")
                (system* "gunzip" "unifont.bdf.gz")))))))))

(define-public grub-coreboot
  (package
    (inherit grub)
    (name "grub-coreboot")
    (synopsis "GRand Unified Boot loader (Coreboot payload version)")
    (arguments
     (substitute-keyword-arguments (package-arguments grub)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'check 'disable-broken-tests
              (lambda _
                (setenv "DISABLE_HARD_ERRORS" "1")
                (setenv "XFAIL_TESTS"
                        (string-join
                         ;; TODO: All the tests below use grub shell
                         ;; (tests/util/grub-shell.in), and here grub-shell uses
                         ;; QEMU and a Coreboot image to run the tests. Since we
                         ;; don't have a Coreboot package in Guix yet these tests
                         ;; are disabled. See the Guix bug #64667 for more details
                         ;; (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=64667).
                         (list "pata_test"
                               "ahci_test"
                               "uhci_test"
                               "ehci_test"
                               "example_grub_script_test"
                               "ohci_test"
                               "grub_script_eval"
                               "grub_script_echo1"
                               "grub_script_test"
                               "grub_script_leading_whitespace"
                               "grub_script_echo_keywords"
                               "grub_script_vars1"
                               "grub_script_for1"
                               "grub_script_while1"
                               "grub_script_if"
                               "grub_script_comments"
                               "grub_script_functions"
                               "grub_script_continue"
                               "grub_script_break"
                               "grub_script_shift"
                               "grub_script_blockarg"
                               "grub_script_return"
                               "grub_script_setparams"
                               "grub_cmd_date"
                               "grub_cmd_set_date"
                               "grub_cmd_sleep"
                               "grub_cmd_regexp"
                               "grub_cmd_test"
                               "grub_script_not"
                               "grub_cmd_echo"
                               "grub_script_expansion"
                               "grub_script_gettext"
                               "grub_script_escape_comma"
                               "help_test"
                               "grub_script_strcmp"
                               "test_sha512sum"
                               "grub_cmd_tr"
                               "test_unset"
                               "file_filter_test")
                         " "))))))
       ((#:configure-flags flags #~'())
        #~(cons* "--with-platform=coreboot" #$flags))))))

(define-public grub-efi
  (package
    (inherit grub)
    (name "grub-efi")
    (synopsis "GRand Unified Boot loader (UEFI version)")
    (inputs
     (modify-inputs (package-inputs grub)
       (prepend efibootmgr mtools)))
    (native-inputs
     ;; The tests are skipped in this package so we remove some test dependencies.
     (modify-inputs (package-native-inputs grub)
       (delete "parted" "qemu" "qemu-minimal" "xorriso")))
    (arguments
     ;; TODO: Tests need a UEFI firmware for qemu. There is one at
     ;; https://github.com/tianocore/edk2/tree/master/OvmfPkg .
     ;; Search for 'OVMF' in "tests/util/grub-shell.in".
     (substitute-keyword-arguments (package-arguments grub)
       ((#:tests? _ #f) #f)
       ((#:configure-flags flags #~'())
        #~(cons* "--with-platform=efi"
                 #$@(if (string-prefix? "x86_64"
                                        (or (%current-target-system)
                                            (%current-system)))
                        #~("--enable-stack-protector") ;EFI-only for now
                        #~())
                 #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'patch-stuff 'use-absolute-efibootmgr-path
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "grub-core/osdep/unix/platform.c"
                  (("efibootmgr")
                   (search-input-file inputs "/sbin/efibootmgr")))))
            (add-after 'patch-stuff 'use-absolute-mtools-path
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "util/grub-mkrescue.c"
                  (("\"mformat\"")
                   (string-append "\""
                                  (search-input-file inputs "/bin/mformat")
                                  "\"")))
                (substitute* "util/grub-mkrescue.c"
                  (("\"mcopy\"")
                   (string-append "\""
                                  (search-input-file inputs "/bin/mcopy")
                                  "\"")))))))))))

(define-public grub-efi32
  (package
    (inherit grub-efi)
    (name "grub-efi32")
    (synopsis "GRand Unified Boot loader (UEFI 32bit version)")
    (arguments
     (substitute-keyword-arguments (package-arguments grub-efi)
       ((#:configure-flags flags #~'())
        #~(cons* #$@(cond ((target-x86?)
                           #~("--target=i386"))
                          ((target-aarch64?)
                           #~("--target=arm"
                              (string-append "TARGET_CC="
                                             #$(cc-for-target
                                                "arm-linux-gnueabihf"))))
                          ((target-arm?)
                           #~("--target=arm"))
                          (else #~()))
                 #$flags))))
    (native-inputs
     (if (target-aarch64?)
         (modify-inputs (package-native-inputs grub-efi)
           (prepend
            (cross-gcc "arm-linux-gnueabihf")
            (cross-binutils "arm-linux-gnueabihf")))
         (package-native-inputs grub-efi)))))

(define-public grub-emu
  (package/inherit grub
    (name "grub-emu")
    (synopsis "GRand Unified Boot loader (Emu version)")
    (arguments
     (substitute-keyword-arguments (package-arguments grub)
       ((#:configure-flags flags #~'())
        #~(cons* "--with-platform=emu" #$flags))))))

;; Because grub searches hardcoded paths it's easiest to just build grub
;; again to make it find both grub-pc and grub-efi.  There is a command
;; line argument which allows you to specify ONE platform - but
;; grub-mkrescue will use multiple platforms if they are available
;; in the installation directory (without command line argument).
(define-public grub-hybrid
  (package
    (inherit grub-efi)
    (name "grub-hybrid")
    (synopsis "GRand Unified Boot loader (hybrid version)")
    (inputs
     (modify-inputs (package-inputs grub-efi)
       (prepend grub)))
    (arguments
     (substitute-keyword-arguments (package-arguments grub-efi)
       ((#:modules modules `((guix build utils)
                             (guix build gnu-build-system)))
        `((ice-9 ftw) ,@modules))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'install-non-efi
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((input-dir (search-input-directory inputs "/lib/grub"))
                      (output-dir (string-append (assoc-ref outputs "out")
                                                 "/lib/grub")))
                  (for-each (lambda (basename)
                              (unless (or (string-prefix? "." basename)
                                          (file-exists? (string-append
                                                         output-dir "/"
                                                         basename)))
                                (symlink (string-append input-dir "/"
                                                        basename)
                                         (string-append output-dir "/"
                                                        basename))))
                            (scandir input-dir)))))))))))

(define-public (make-grub-efi-netboot name subdir)
  "Make a grub-efi-netboot package named NAME, which will be able to boot over
network via TFTP by accessing its files in the SUBDIR of a TFTP root directory.
This package is also able to boot from local storage devices.

A bootloader-installer basically needs to copy the package content into the
bootloader-target directory, which will usually be the TFTP root, as
'grub-mknetdir' will be invoked already during the package creation.

Alternatively the bootloader-target directory can be a mounted EFI System
Partition (ESP), or a similar partition with a FAT file system, for booting
from local storage devices.

The name of the GRUB EFI binary will conform to the UEFI specification for
removable media.  Depending on the system it will be e.g. bootx64.efi or
bootaa64.efi below SUBDIR.

The SUBDIR argument needs to be set to \"efi/boot\" to create a package which
conforms to the UEFI specification for removable media.

The SUBDIR argument defaults to \"efi/Guix\", as it is also the case for
'grub-efi-bootloader'."
  (package
    (name name)
    (version (package-version grub-efi))
    ;; Source is not needed, but it cannot be omitted.
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (let* ((system (string-split (nix-system->gnu-triplet
                                   (or (%current-target-system)
                                       (%current-system)))
                                  #\-))
            (arch (first system))
            (boot-efi
             (match system
               ;; These are the supportend systems and the names defined by
               ;; the UEFI standard for removable media.
               (("i686" _ ...)        "/bootia32.efi")
               (("x86_64" _ ...)      "/bootx64.efi")
               (("arm" _ ...)         "/bootarm.efi")
               (("aarch64" _ ...)     "/bootaa64.efi")
               (("riscv" _ ...)       "/bootriscv32.efi")
               (("riscv64" _ ...)     "/bootriscv64.efi")
               ;; Other systems are not supported, although defined.
               ;; (("riscv128" _ ...) "/bootriscv128.efi")
               ;; (("ia64" _ ...)     "/bootia64.efi")
               ((_ ...)               #f)))
            (core-efi (string-append
                       ;; This is the arch dependent file name of GRUB, e.g.
                       ;; i368-efi/core.efi or arm64-efi/core.efi.
                       (match arch
                         ("i686"    "i386")
                         ("aarch64" "arm64")
                         ("riscv"   "riscv32")
                         (_         arch))
                       "-efi/core.efi")))
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let* ((bootloader #$(this-package-input "grub-efi"))
                   (net-dir #$output)
                   (sub-dir (string-append net-dir "/" #$subdir "/"))
                   (boot-efi (string-append sub-dir #$boot-efi))
                   (core-efi (string-append sub-dir #$core-efi)))
              ;; Install GRUB, which refers to the grub.cfg, with support for
              ;; encrypted partitions,
              (setenv "GRUB_ENABLE_CRYPTODISK" "y")
              (invoke/quiet (string-append bootloader "/bin/grub-mknetdir")
                            (string-append "--net-directory=" net-dir)
                            (string-append "--subdir=" #$subdir)
                            ;; These modules must be pre-loaded to allow booting
                            ;; from an ESP or a similar partition with a FAT
                            ;; file system.
                            (string-append "--modules=part_msdos part_gpt fat"))
              ;; Move GRUB's core.efi to the removable media name.
              (false-if-exception (delete-file boot-efi))
              (rename-file core-efi boot-efi))))))
    (inputs (list grub-efi))
    (synopsis (package-synopsis grub-efi))
    (description (package-description grub-efi))
    (home-page (package-home-page grub-efi))
    (license (package-license grub-efi))))

(define-public syslinux
  (let ((commit "bb41e935cc83c6242de24d2271e067d76af3585c"))
    (package
      (name "syslinux")
      (version (git-version "6.04-pre" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/geneC/syslinux")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0k8dvafd6410kqxf3kyr4y8jzmpmrih6wbjqg6gklak7945yflrc"))
                (patches
                 (search-patches "syslinux-gcc10.patch"
                                 "syslinux-strip-gnu-property.patch"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("nasm" ,nasm)
         ("perl" ,perl)
         ("python-2" ,python-2)))
      (inputs
       `(("libuuid" ,util-linux "lib")
         ("mtools" ,mtools)))
      (arguments
       `(#:parallel-build? #f
         #:make-flags
         (list (string-append "BINDIR=" %output "/bin")
               (string-append "SBINDIR=" %output "/sbin")
               (string-append "LIBDIR=" %output "/lib")
               (string-append "INCDIR=" %output "/include")
               (string-append "DATADIR=" %output "/share")
               (string-append "MANDIR=" %output "/share/man")
               "PERL=perl"
               "bios")
         #:strip-flags '("--strip-debug" "--enable-deterministic-archives")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-files
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* (find-files "." "Makefile.*|ppmtolss16")
                 (("/bin/pwd") (which "pwd"))
                 (("/bin/echo") (which "echo"))
                 (("/usr/bin/perl") (which "perl")))
               (let ((mtools (assoc-ref inputs "mtools")))
                 (substitute* (find-files "." "\\.c$")
                   (("mcopy")
                    (string-append mtools "/bin/mcopy"))
                   (("mattrib")
                    (string-append mtools "/bin/mattrib"))))
               #t))
           (delete 'configure)
           (add-before 'build 'set-permissions
             (lambda _
               (invoke "chmod" "a+w" "utils/isohybrid.in")))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (setenv "CC" "gcc")
                 (substitute* "tests/unittest/include/unittest/unittest.h"
                   ;; Don't look up headers under /usr.
                   (("/usr/include/") ""))
                 (invoke "make" "unittest")))))))
      (home-page "https://www.syslinux.org")
      (synopsis "Lightweight Linux bootloader")
      (description "Syslinux is a lightweight Linux bootloader.")
      ;; The Makefile specifically targets i386 and x86_64 using nasm.
      (supported-systems '("i686-linux" "x86_64-linux"))
      (license (list license:gpl2+
                     license:bsd-3 ; gnu-efi/*
                     license:bsd-4 ; gnu-efi/inc/* gnu-efi/lib/*
                     ;; Also contains:
                     license:expat license:isc license:zlib)))))

(define-public dtc
  (package
    (name "dtc")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/utils/dtc/"
                    "dtc-" version ".tar.gz"))
              (sha256
               (base32
                "0cij9399snpn672pdbda8qbxljdkfg068kvv3g5811rz6yslx124"))
              (patches
               (search-patches "dtc-meson-cell-overflow.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:modules '((guix build meson-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'preparations
            (lambda _
              ;; The version string is usually derived via setuptools-scm, but
              ;; without the git metadata available this fails.
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)

              ;; Needed by setup.py.
              (setenv "DESTDIR" "/")

              ;; Native gcc needed by run_test.sh.
              (setenv "CC" "gcc")

              ;; /bin/fdt{get,overlay,put} need help finding libfdt.so.1.
              (setenv "LDFLAGS"
                      (string-append "-Wl,-rpath=" #$output "/lib"))))
          (add-after 'unpack 'install-doc
            (lambda _
              (with-directory-excursion "Documentation"
                (for-each (cut install-file <> (string-append
                                                #$output "/share/doc/dtc/"))
                          '("dts-format.txt"
                            "dt-object-internal.txt"
                            "manual.txt")))))
          (add-after 'unpack 'patch-pkg-config
            (lambda _
              (substitute* '("tests/run_tests.sh")
                (("pkg-config")
                 #$(pkg-config-for-target))))))))
    (native-inputs
     (append
      (list bison
            flex
            libyaml
            ninja
            pkg-config
            python
            python-setuptools-scm
            swig
            which)
      (if (member (%current-system) (package-supported-systems valgrind))
          (list valgrind)
          '())))
    (inputs
     (list python))
    (home-page "https://www.devicetree.org")
    (synopsis "Compiles device tree source files")
    (description "@command{dtc} compiles
@uref{http://elinux.org/Device_Tree_Usage, device tree source files} to device
tree binary files.  These are board description files used by Linux and BSD.")
    (license license:gpl2+)))

(define u-boot
  (package
    (name "u-boot")
    (version "2025.01")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://source.denx.de/u-boot/u-boot.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1v86bnixh8hyqbwwr5iwdnnadmg2fqxw9g526fvclsbvl8lz0v"))
       (patches (search-patches "u-boot-allow-disabling-openssl.patch"
                                "u-boot-rockchip-inno-usb.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list bison
           dtc
           gnutls
           flex
           lz4
           ncurses/tinfo
           perl
           pkg-config                   ;for 'make menuconfig'
           python
           python-pyelftools
           python-setuptools
           python-wheel
           swig
           (list util-linux "lib")))
    (home-page "https://www.denx.de/wiki/U-Boot/")
    (synopsis "ARM bootloader")
    (description "U-Boot is a bootloader used mostly for ARM boards.  It
also initializes the boards (RAM etc).")
    (license license:gpl2+)))

;;; This is very similar to the linux-libre-documentation package, since it
;;; reuses the same Makefile-based build system.
(define-public u-boot-documentation
  (package
    (inherit u-boot)
    (name "u-boot-documentation")
    (arguments
     (list
      #:make-flags #~(list "HOSTCC=gcc"
                           ;; Avoid treating Sphinx warnings as errors.
                           "SPHINXOPTS=")
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (replace 'build
                     (lambda* (#:key make-flags #:allow-other-keys)
                       (apply invoke "make" "infodocs" make-flags)))
                   (replace 'install
                     (lambda* (#:key make-flags #:allow-other-keys)
                       (let* ((info-dir (string-append #$output "/share/info"))
                              (info (string-append info-dir
                                                   "/DasUBoot.info.gz")))
                         (with-directory-excursion "doc/output"
                           (apply invoke "make" "-C" "texinfo" "install-info"
                                  (string-append "infodir=" info-dir)
                                  make-flags))))))))
    (native-inputs
     (modify-inputs (package-native-inputs u-boot)
       (append fontconfig
               python-sphinx
               python-sphinx-prompt
               texinfo
               which)))
    (synopsis "U-Boot documentation")
    (description "This package provides the documentation for U-Boot, as an
Info manual.")))

(define-public u-boot-tools
  (package
    (inherit u-boot)
    (name "u-boot-tools")
    (native-inputs
     (modify-inputs (package-native-inputs u-boot)
       (prepend python-coverage
                python-filelock
                python-pycryptodomex
                python-pytest
                python-pytest-xdist)))
    (arguments
     `(#:make-flags '("HOSTCC=gcc")
       #:test-target "tcheck"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/bin/pwd") (which "pwd"))
               (("/bin/false") (which "false")))
             (substitute* "tools/dtoc/fdt_util.py"
               (("'cc'") "'gcc'"))
             (substitute* "tools/u_boot_pylib/test_util.py"
               ;; python3-coverage is simply called coverage in guix.
               (("python3-coverage") "coverage")

               ;; Don't require 100% coverage since it's brittle and can
               ;; fail with newer versions of coverage or dependencies.
               (("raise ValueError\\('Test coverage failure'\\)")
                "print('Continuing anyway since Guix does not care :O')"))
             (substitute* "test/run"
               ;; Make it easier to find test failures.
               (("#!/bin/bash") "#!/bin/bash -x")
               ;; This test would require git.
               (("\\./tools/patman/patman") (which "true"))
               ;; FIXME: test fails, needs further investiation
               (("run_test \"binman\"") "# run_test \"binman\"")
               ;; FIXME: tests fail without kwbimage, i.e. openssl.
               (("run_test \"sandbox_noinst\"")
                "# run_test \"sandbox_noinst\"")
               (("run_test \"sandbox_vpl\"")
                "# run_test \"sandbox_vpl\"")
               ;; FIXME: code coverage not working
               (("run_test \"binman code coverage\"")
                "# run_test \"binman code coverage\"")
               ;; This test would require internet access.
               (("\\./tools/buildman/buildman") (which "true")))
             (substitute* "test/py/tests/test_sandbox_exit.py"
               (("def test_ctrl_c")
                "@pytest.mark.skip(reason='Guix has problems with SIGINT')
def test_ctrl_c"))
             ;; Test against the tools being installed rather than tools built
             ;; for "sandbox" target.
             (substitute* "test/image/test-imagetools.sh"
               (("BASEDIR=sandbox") "BASEDIR=."))
             (for-each (lambda (file)
                         (substitute* file
                           ;; Disable features that require OpenSSL due
                           ;; to GPL/Openssl license incompatibilities.
                           ;; See https://bugs.gnu.org/34717 for
                           ;; details.
                           (("CONFIG_FIT_SIGNATURE=y")
                            "CONFIG_FIT_SIGNATURE=n
CONFIG_UT_LIB_ASN1=n
CONFIG_TOOLS_LIBCRYPTO=n
CONFIG_TOOLS_KWBIMAGE=n")
                           ;; Catch instances of implied CONFIG_FIG_SIGNATURE
                           ;; with VPL targets
                           (("CONFIG_SANDBOX_VPL=y")
                            "CONFIG_SANDBOX_VPL=y
CONFIG_FIT_SIGNATURE=n
CONFIG_VPL_FIT_SIGNATURE=n
CONFIG_TOOLS_LIBCRYPTO=n
CONFIG_TOOLS_KWBIMAGE=n")
                           ;; This test requires a sound system, which is un-used
                           ;; in u-boot-tools.
                           (("CONFIG_SOUND=y") "CONFIG_SOUND=n")))
                       (find-files "configs" "sandbox_.*defconfig$|tools-only_defconfig"))))
         (replace 'configure
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "tools-only_defconfig" make-flags)))
         (replace 'build
           (lambda* (#:key inputs make-flags #:allow-other-keys)
             (apply invoke "make" "tools-all" make-flags)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (name)
                           (install-file name bin))
                         '("tools/netconsole"
                           "tools/jtagconsole"
                           "tools/gen_eth_addr"
                           "tools/gen_ethaddr_crc"
                           "tools/img2srec"
                           "tools/mkenvimage"
                           "tools/dumpimage"
                           "tools/mkimage"
                           "tools/kwboot"
                           "tools/proftool"
                           "tools/fdtgrep"
                           "tools/env/fw_printenv"
                           "tools/sunxi-spl-image-builder")))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key make-flags test-target tests? #:allow-other-keys)
             (when tests?
               (invoke "test/image/test-imagetools.sh"))))
         ;; Only run full test suite on x86_64 systems, as many tests
         ;; assume x86_64.
         ,@(if (string-match "^x86_64-linux"
                             (or (%current-target-system)
                                 (%current-system)))
               '((add-after 'check 'check-x86
                   (lambda* (#:key make-flags test-target tests? #:allow-other-keys)
                     (when tests?
                       (begin
                         (apply invoke "make" "mrproper" make-flags)
                         (setenv "SDL_VIDEODRIVER" "dummy")
                         (setenv "PAGER" "cat")
                         (apply invoke "make" test-target make-flags))))))
               '()))))
    (description (string-append
                  (package-description u-boot)
                  "  This package provides board-independent tools "
                  "of U-Boot."))))

(define-public python-u-boot-pylib
  (package
    (inherit u-boot)
    (name "python-u-boot-pylib")
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "tools/u_boot_pylib")))
          (add-after 'chdir 'list-package
            (lambda _
              (let ((port (open-file "pyproject.toml" "a")))
                (display "[tool.setuptools.packages.find]\n" port)
                (display "where = [\"..\"]\n" port)
                (display "include = [\"u_boot_pylib*\"]" port)
                (close-port port))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./u_boot_pylib")))))))
    (synopsis "U-Boot Python library")
    (description "This package provides common Python code used by some of the
commands part of the U-Boot project, such as Patman.")))

;;; This is packaged separately, as it can be used in other contexts than for
;;; U-Boot development.
(define-public patman
  (package
    (inherit u-boot)
    (name "patman")
    (build-system pyproject-build-system)
    (arguments
     ;; The test suite strongly relies on the git metadata being available (23
     ;; failed, 14 passed).
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Patman fails to run during 'sanity-check phase, as it needs to be
          ;; run within a git directory.
          (delete 'sanity-check)
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "tools/patman")))
          (add-after 'install 'wrap-script
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-script (string-append #$output "/bin/patman")
                `("PATH" ":" prefix
                  (,(string-append #$(this-package-input "git") "/bin")))
                `("GIT_EXEC_PATH" ":" prefix
                  (,(dirname (search-input-file
                              inputs "libexec/git-core/git-commit"))
                   ,(dirname (search-input-file
                              inputs
                              "libexec/git-core/git-send-email"))))))))))
    (inputs
     (list git
           `(,git "send-email")
           guile-3.0/pinned             ;for wrap-script
           python-pygit2
           python-requests
           python-u-boot-pylib))
    (synopsis "Patch automation tool")
    (description "Patman is a patch automation script which:
@itemize
@item Creates patches directly from your branch
@item Cleans them up by removing unwanted tags
@item Inserts a cover letter with change lists
@item Runs the patches through automated checks
@item Optionally emails them out to selected people.
@end itemize")))

(define*-public (make-u-boot-package board triplet
                                     #:key
                                     defconfig
                                     configs
                                     name-suffix
                                     append-description
                                     (u-boot u-boot))
  "Return a U-Boot package for BOARD cross-compiled for TRIPLET with the
optional DEFCONFIG file and optional configuration changes from CONFIGS.
TRIPLET may also be set to #f to disable cross-compilation.  NAME-SUFFIX is
appended to the package name, while APPEND-DESCRIPTION is appended to the
package description.  U-BOOT can be used when a fork or a different version of
U-Boot must be used."
  (let ((native-build? (lambda ()
                         (or (not triplet) ;disable cross-compilation
                             (string=? (%current-system)
                                       (gnu-triplet->nix-system triplet))))))
    (package
      (inherit u-boot)
      (name (string-append (downstream-package-name "u-boot-" board)
                           (or name-suffix "")))
      (description (if append-description
                       (string-append (package-description u-boot)
                                      "\n\n" append-description)
                       (package-description u-boot)))
      (build-system gnu-build-system)
      (arguments
       (substitute-keyword-arguments (package-arguments u-boot)
         ((#:target _ #f)
          (and (not (native-build?)) triplet))
         ((#:modules modules '())
          `((ice-9 ftw)
            (srfi srfi-1)
            (guix build gnu-build-system)
            (guix build kconfig)
            (guix build utils)
            ,@modules))
         ((#:imported-modules imported-modules '())
          `((guix build kconfig)
            ,@%default-gnu-imported-modules
            ,@imported-modules))
         ((#:test-target _ "test")
          "test")
         ((#:make-flags make-flags '())
          #~(list "HOSTCC=gcc"
                  "KBUILD_VERBOSE=1"
                  #$@(if (not (native-build?))
                         (list (string-append "CROSS_COMPILE=" triplet "-"))
                         '())
                  #$@make-flags))
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'configure
                (lambda* (#:key make-flags #:allow-other-keys)
                  (let* ((config-name (string-append #$board "_defconfig"))
                         (config-file (string-append "configs/" config-name))
                         (defconfig #$defconfig)
                         (configs '#$configs))
                    (when defconfig
                      ;; Replace the board-specific defconfig with the given
                      ;; one.
                      (copy-file defconfig config-file))
                    (if (file-exists? config-file)
                        (begin
                          (when configs
                            (modify-defconfig config-file configs))
                          (apply invoke "make" `(,@make-flags ,config-name))
                          (verify-config ".config" config-file))
                        (begin
                          (display "invalid board name; valid board names are:"
                                   (current-error-port))
                          (let ((suffix-len (string-length "_defconfig"))
                                (entries (scandir "configs")))
                            (for-each (lambda (file-name)
                                        (when (string-suffix? "_defconfig"
                                                              file-name)
                                          (format (current-error-port)
                                                  "- ~A\n"
                                                  (string-drop-right
                                                   file-name suffix-len))))
                                      (sort entries string-ci<)))
                          (error "invalid boardname ~s" #$board))))))
              (add-after 'configure 'disable-tools-libcrypto
                ;; Disable libcrypto due to GPL and OpenSSL license
                ;; incompatibilities
                (lambda _
                  (substitute* ".config"
                    (("CONFIG_TOOLS_LIBCRYPTO=.*$")
                     "CONFIG_TOOLS_LIBCRYPTO=n
CONFIG_TOOLS_KWBIMAGE=n"))))
              (replace 'install
                (lambda _
                  (let ((libexec (string-append #$output "/libexec"))
                        (uboot-files
                         (append
                          (remove
                           ;; Those would not be reproducible
                           ;; because of the randomness used to
                           ;; produce them.  It's expected that the
                           ;; user will use u-boot-tools to generate
                           ;; them instead.
                           (lambda (name)
                             (string-suffix?
                              "sunxi-spl-with-ecc.bin"
                              name))
                           (find-files "."
                                       ".*\\.(bin|efi|img|imx|spl|itb|dtb|rksd)$"))
                          (find-files "." "^(MLO|SPL)$"))))
                    (mkdir-p libexec)
                    (install-file ".config" libexec)
                    ;; Useful for "qemu -kernel".
                    (install-file "u-boot" libexec)
                    (for-each
                     (lambda (file)
                       (let ((target-file (string-append libexec "/" file)))
                         (mkdir-p (dirname target-file))
                         (copy-file file target-file)))
                     uboot-files)))))))))))

(define* (make-u-boot-rockchip-package board soc #:optional configs)
  "Return the U-Boot package for BOARD with AAarch64 Rockchip SOC
(System on Chip)."
  (let* ((board (string-append board "-" (symbol->string soc)))
         (base (make-u-boot-package board "aarch64-linux-gnu"
                                    #:configs configs)))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "BL31" (search-input-file inputs "/bl31.elf"))))))))
      (inputs (modify-inputs (package-inputs base)
                (append (match soc
                          ('rk3588 arm-trusted-firmware-rk3588)
                          ('rk3399 arm-trusted-firmware-rk3399)
                          ('rk3328 arm-trusted-firmware-rk3328))))))))

(define-public u-boot-am335x-boneblack
  (let ((base (make-u-boot-package
               "am335x_evm" "arm-linux-gnueabihf"
               ;; Patch out other device trees to build an image small enough
               ;; to fit within typical partitioning schemes where the first
               ;; partition begins at sector 2048.
               #:configs '("CONFIG_OF_LIST=\"am335x-evm am335x-boneblack\"")
               #:append-description
               "This U-Boot is built for the BeagleBone Black, which was
removed upstream, adjusted from the am335x_evm build with several device trees
removed so that it fits within common partitioning schemes.")))
    (package
      (inherit base)
      ;; The name is not derived from the board name on purpose, as the config
      ;; is modified per the comment above, parting from the default
      ;; am335x_evm configuration.
      (name "u-boot-am335x-boneblack"))))

(define-public u-boot-am335x-evm
  (make-u-boot-package "am335x_evm" "arm-linux-gnueabihf"))

(define*-public (make-u-boot-sunxi64-package board triplet scp-firmware
                                             #:key defconfig configs)
  (let ((base (make-u-boot-package
               board triplet #:defconfig defconfig #:configs configs)))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  (setenv "SCP" (search-input-file
                                 (or native-inputs inputs) "libexec/scp.bin"))
                  (setenv "BL31" (search-input-file inputs "bl31.bin"))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (append (force scp-firmware))))
      (inputs
       (modify-inputs (package-inputs base)
         (append arm-trusted-firmware-sun50i-a64))))))

(define-public u-boot-orangepi-zero2w
  (let ((base (make-u-boot-package
               "orangepi_zero2w" "aarch64-linux-gnu")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  (setenv "SCP" "/dev/null")
                  (setenv "BL31" (search-input-file inputs "bl31.bin"))))))))
      (inputs
       (modify-inputs (package-inputs base)
         ;; The Zero 2W uses the slightly revised Allwinner H618.
         (append arm-trusted-firmware-sun50i-h616))))))

(define-public u-boot-pine64-plus
  (make-u-boot-sunxi64-package "pine64_plus" "aarch64-linux-gnu"
                               (delay crust-pine64-plus)))

(define-public u-boot-pine64-lts
  (make-u-boot-sunxi64-package "pine64-lts" "aarch64-linux-gnu"
                               (delay crust-pine64-plus)))

(define-public u-boot-pinebook
  (make-u-boot-sunxi64-package
   "pinebook" "aarch64-linux-gnu" (delay crust-pinebook)
   ;; Fix regression with LCD video output introduced in 2020.01
   ;; https://patchwork.ozlabs.org/patch/1225130/
   #:configs '("CONFIG_VIDEO_BPP32=y")))

(define-public u-boot-bananapi-m2-ultra
  (make-u-boot-package "Bananapi_M2_Ultra" "arm-linux-gnueabihf"))

(define-public u-boot-a20-olinuxino-lime
  (make-u-boot-package "A20-OLinuXino-Lime" "arm-linux-gnueabihf"))

(define-public u-boot-a20-olinuxino-lime2
  (make-u-boot-package "A20-OLinuXino-Lime2" "arm-linux-gnueabihf"))

(define-public u-boot-a20-olinuxino-micro
  (make-u-boot-package "A20-OLinuXino_MICRO" "arm-linux-gnueabihf"))

(define-public u-boot-nintendo-nes-classic-edition
  (let ((base (make-u-boot-package "Nintendo_NES_Classic_Edition"
                                   "arm-linux-gnueabihf"
                                   #:append-description "This version is for
the Nintendo NES Classic Edition.  It is assumed that you have added a serial
port to pins PB0 and PB1 as described on
@url{https://linux-sunxi.org/Nintendo_NES_Classic_Edition}.

In order to use FEL mode on the device, hold the Reset button on the
device while it's being turned on (and a while longer).")))
    (package
      (inherit base)
      ;; Starting with 2019.01, FEL doesn't work anymore on A33.
      (version "2018.11")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://ftp.denx.de/pub/u-boot/"
                      "u-boot-" version ".tar.bz2"))
                (sha256
                 (base32
                  "0znkwljfwwn4y7j20pzz4ilqw8znphrfxns0x1lwdzh3xbr96z3k"))
                (patches (search-patches
                          "u-boot-nintendo-nes-serial.patch"))))
      (native-inputs
       `(("python" ,python-2)
         ,@(package-native-inputs base))))))

(define-public u-boot-wandboard
  (make-u-boot-package "wandboard" "arm-linux-gnueabihf"))

(define-public u-boot-mx6cuboxi
  (make-u-boot-package "mx6cuboxi" "arm-linux-gnueabihf"))

(define-public u-boot-novena
  (make-u-boot-package
   "novena" "arm-linux-gnueabihf"
   ;; Patch configuration to disable loading u-boot.img from FAT partition,
   ;; allowing it to be installed at a device offset.
   #:configs '("# CONFIG_SPL_FS_FAT is not set")
   #:append-description "This U-Boot is built for Novena.  Be advised that this
version, contrary to Novena upstream, does not load u-boot.img from the first
partition."))

(define-public u-boot-orangepi-r1-plus-lts-rk3328
  (make-u-boot-rockchip-package "orangepi-r1-plus-lts" 'rk3328))

(define-public u-boot-cubieboard
  (make-u-boot-package "Cubieboard" "arm-linux-gnueabihf"))

(define-public u-boot-cubietruck
  (make-u-boot-package "Cubietruck" "arm-linux-gnueabihf"))

(define-public u-boot-puma-rk3399
  (make-u-boot-rockchip-package "puma" 'rk3399))

(define-public u-boot-qemu-arm
  (make-u-boot-package "qemu_arm" "arm-linux-gnueabihf"
                       ;; Disable features that require OpenSSL due
                       ;; to GPL/Openssl license incompatibilities.
                       ;; See https://bugs.gnu.org/34717 for
                       ;; details.
                       #:configs '("# CONFIG_FIT_SIGNATURE is not set")))

(define-public u-boot-qemu-arm64
  (make-u-boot-package "qemu_arm64" "aarch64-linux-gnu"
                       ;; Disable features that require OpenSSL due
                       ;; to GPL/Openssl license incompatibilities.
                       ;; See https://bugs.gnu.org/34717 for
                       ;; details.
                       #:configs '("# CONFIG_FIT_SIGNATURE is not set")))

(define-public u-boot-qemu-riscv64
  (make-u-boot-package "qemu-riscv64" "riscv64-linux-gnu"))

(define-public u-boot-qemu-riscv64-smode
  (make-u-boot-package "qemu-riscv64_smode" "riscv64-linux-gnu"))

(define-public u-boot-sandbox
  (define base
    (make-u-boot-package
      "sandbox" #f             ;build for the native system
      ;; These disabled features require OpenSSL, which is
      ;; incompatible with the GPLv2-only parts of U-boot.
      #:configs (map (cut string-append "# CONFIG_" <> " is not set")
                     '("FIT_CIPHER" "MBEDTLS_LIB"))
      #:append-description
      "The sandbox configuration of U-Boot provides a @command{u-boot}
command that runs as a normal user space application.  It can be used to
test the functionality of U-Boot interactively without having to deploy
to an actual target device.  @xref{Sandbox<6>,,,u-boot, The U-Boot
Documentation} for more information (for example by running @samp{info
\"(u-boot) Sandbox<6>\"})."))
  (package
    (inherit base)
    (arguments
     (substitute-keyword-arguments (package-arguments base)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'symlink-u-boot-command
              (lambda* (#:key outputs #:allow-other-keys)
                ;; For ease of discovery.
                (mkdir (string-append #$output "/bin"))
                (symlink (search-input-file outputs "libexec/u-boot")
                         (string-append #$output "/bin/u-boot"))))))))
    ;; cert-to-efi-sig-list from efitools creates the EFI capsule ESL.
    (inputs (modify-inputs (package-inputs base)
              (append efitools sdl2)))))

(define-public u-boot-sifive-unleashed
  (let ((base (make-u-boot-package "sifive_unleashed" "riscv64-linux-gnu")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "OPENSBI" (search-input-file inputs
                                                       "fw_dynamic.bin"))))))))
      (inputs
       (modify-inputs (package-inputs base)
         (append opensbi-generic))))))

(define-public u-boot-sifive-unmatched
  (let ((base (make-u-boot-package "sifive_unmatched" "riscv64-linux-gnu")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "OPENSBI" (search-input-file inputs
                                                       "fw_dynamic.bin"))))))))
      (inputs
       (modify-inputs (package-inputs base)
         (append opensbi-generic))))))

(define-public u-boot-starfive-visionfive2
  (let ((base (make-u-boot-package "starfive_visionfive2" "riscv64-linux-gnu"
                                   ;; Allow kernel-arguments pass more content.
                                   ;; If out of range, boot will fail.
                                   #:configs '("CONFIG_SYS_CBSIZE=1024"))))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "OPENSBI" (search-input-file inputs
                                                       "fw_dynamic.bin"))))
              (add-after 'install 'install-u-boot-spl.bin.normal.out
                (lambda _
                  (install-file "spl/u-boot-spl.bin.normal.out"
                                (string-append #$output
                                               "/libexec/spl"))))))))
      (inputs
       (modify-inputs (package-inputs base)
         (append opensbi-for-visionfive2))))))

(define-public u-boot-rock64-rk3328
  (make-u-boot-rockchip-package "rock64" 'rk3328))

(define-public u-boot-firefly-rk3399
  (make-u-boot-rockchip-package "firefly" 'rk3399))

(define-public u-boot-rockpro64-rk3399
  (let ((base (make-u-boot-rockchip-package
               "rockpro64" 'rk3399
               '("CONFIG_USB=y"
                 "CONFIG_AHCI=y"
                 "CONFIG_AHCI_PCI=y"
                 "CONFIG_SATA=y"
                 "CONFIG_SATA_SIL=y"
                 "CONFIG_SCSI=y"
                 "CONFIG_SCSI_AHCI=y"
                 ;; Disable SPL FIT signatures, due to GPLv2 and
                 ;; OpenSSL license incompatibilities.
                 "# CONFIG_SPL_FIT_SIGNATURE is not set"))))
    (package
      (inherit base))))

(define-public u-boot-pinebook-pro-rk3399
  (make-u-boot-rockchip-package "pinebook-pro" 'rk3399))

(define*-public (make-u-boot-bin-package u-boot-package
                                         #:key
                                         (u-boot-bin "u-boot.bin"))
  "Return a package with a single U-BOOT-BIN file from the U-BOOT-PACKAGE.
The package name will be that of the U-BOOT package suffixed with \"-bin\"."
  (package
    (name (string-append (package-name u-boot-package) "-bin"))
    (version (package-version u-boot-package))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (mkdir #$output)
            (symlink (search-input-file %build-inputs
                                        (string-append "libexec/" #$u-boot-bin))
                     (string-append #$output "/" #$u-boot-bin))))))
    (inputs (list u-boot-package))
    (home-page (package-home-page u-boot-package))
    (synopsis (package-synopsis u-boot-package))
    (description (string-append
                  (package-description u-boot-package)
                  "\n\n"
                  (format #f
                          "This package only contains the file ~a."
                          u-boot-bin)))
    (license (package-license u-boot-package))))

(define-public %u-boot-rpi-efi-configs
  '("CONFIG_OF_EMBED"
    "CONFIG_OF_BOARD=y"))

(define %u-boot-rpi-description-32-bit
  "This is a 32-bit build of U-Boot.")

(define %u-boot-rpi-description-64-bit
  "This is a common 64-bit build of U-Boot for all 64-bit capable Raspberry Pi
variants.")

(define %u-boot-rpi-efi-description
  "It allows network booting and uses the device-tree from the firmware,
allowing the usage of overlays.  It can act as an EFI firmware for the
grub-efi-netboot-removable-bootloader.")

(define %u-boot-rpi-efi-description-32-bit
  (string-append %u-boot-rpi-efi-description "  "
                 %u-boot-rpi-description-32-bit))

(define-public u-boot-rpi-2
  (make-u-boot-package "rpi_2" "arm-linux-gnueabihf"
                       #:append-description %u-boot-rpi-description-32-bit))

(define-public u-boot-rpi-3-32b
  (make-u-boot-package "rpi_3_32b" "arm-linux-gnueabihf"
                       #:append-description %u-boot-rpi-description-32-bit))

(define-public u-boot-rpi-4-32b
  (make-u-boot-package "rpi_4_32b" "arm-linux-gnueabihf"
                       #:append-description %u-boot-rpi-description-32-bit))

(define-public u-boot-rpi-arm64
  (make-u-boot-package "rpi_arm64" "aarch64-linux-gnu"
                       #:append-description %u-boot-rpi-description-64-bit))

(define-public u-boot-rpi-2-efi
  (make-u-boot-package "rpi_2" "arm-linux-gnueabihf"
                       #:name-suffix "-efi"
                       #:configs %u-boot-rpi-efi-configs
                       #:append-description %u-boot-rpi-efi-description-32-bit))

(define-public u-boot-rpi-3-32b-efi
  (make-u-boot-package "rpi_3_32b" "arm-linux-gnueabihf"
                       #:name-suffix "-efi"
                       #:configs %u-boot-rpi-efi-configs
                       #:append-description %u-boot-rpi-efi-description-32-bit))

(define-public u-boot-rpi-4-32b-efi
  (make-u-boot-package "rpi_4_32b" "arm-linux-gnueabihf"
                       #:name-suffix "-efi"
                       #:configs %u-boot-rpi-efi-configs
                       #:append-description %u-boot-rpi-efi-description-32-bit))

(define-public u-boot-rpi-arm64-efi
  (make-u-boot-package "rpi_arm64""aarch64-linux-gnu"
                       #:name-suffix "-efi"
                       #:configs %u-boot-rpi-efi-configs
                       #:append-description (string-append
                                             %u-boot-rpi-efi-description "  "
                                             %u-boot-rpi-description-64-bit)))

(define-public u-boot-rpi-2-bin (make-u-boot-bin-package u-boot-rpi-2))

(define-public u-boot-rpi-3_32b-bin (make-u-boot-bin-package u-boot-rpi-3-32b))

(define-public u-boot-rpi-4_32b-bin (make-u-boot-bin-package u-boot-rpi-4-32b))

(define-public u-boot-rpi-arm64-bin (make-u-boot-bin-package u-boot-rpi-arm64))

(define-public u-boot-rpi-2-efi-bin (make-u-boot-bin-package u-boot-rpi-2-efi))

(define-public u-boot-rpi-3-32b-efi-bin
  (make-u-boot-bin-package u-boot-rpi-3-32b-efi))

(define-public u-boot-rpi-4-32b-efi-bin
  (make-u-boot-bin-package u-boot-rpi-4-32b-efi))

(define-public u-boot-rpi-arm64-efi-bin
  (make-u-boot-bin-package u-boot-rpi-arm64-efi))

(define u-boot-ts-mx6
  ;; There is no release; use the latest commit of the
  ;; 'imx_v2015.04_3.14.52_1.1.0_ga' branch.
  (let ((revision "0")
        (commit "08809160fbc60d6e949fa9d37d9a41aab8fef742"))
    (package
      (inherit u-boot)
      (name "u-boot-ts-mx6")
      (version (git-version "2015.04_3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/embeddedTS/u-boot-imx")
                      (commit commit)))
                (file-name (git-file-name "u-boot-imx-ts" version))
                (sha256
                 (base32
                  "01mja33351hkcs59rmfvppqlxqw4rh9gng7a7hx2cfspqwh2y6kr"))))
      (arguments
       (substitute-keyword-arguments (package-arguments u-boot)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'patch-u-boot
                (lambda _
                  (substitute* (find-files "include/configs" "^ts[0-9]{4}\\.h$")
                    ;; Default to boot a standard zImage instead of a uImage.
                    (("/boot/uImage")
                     "/boot/zImage")
                    (("uimage")
                     "zimage")
                    (("bootm \\$\\{loadaddr}")
                     "bootz ${loadaddr}")
                    ;; This reference DTB is not available in mainline.
                    (("ts7970-revf.dtb")
                     "ts7970.dtb")
                    ;; Enable support for DISTRO_DEFAULTS, which enables to
                    ;; use 'sysboot' to boot Guix System.  Also enable
                    ;; "standard" boot commands for dealing with discovery and
                    ;; booting of syslinux configurations (extlinux.conf).

                    ;; Disable the stock CONFIG_BOOTCOMMAND to avoid a
                    ;; redefinition error.
                    (("CONFIG_BOOTCOMMAND")
                     "CONFIG_BOOTCOMMAND_DISABLED")
                    (("CONFIG_BOOTDELAY")
                     "CONFIG_BOOTDELAY_DISABLED")
                    ;; Inspired by include/configs/embestmx6boards.h
                    (("#define CONFIG_EXTRA_ENV_SETTINGS.*" anchor)
                     (string-append "\
#include <config_distro_defaults.h>

#define MEM_LAYOUT_ENV_SETTINGS    \\
\t\"bootm_size=0x10000000\\0\"     \\
\t\"kernel_addr_r=0x10800000\\0\"  \\
\t\"fdt_addr_r=0x18000000\\0\"     \\
\t\"scriptaddr=0x18100000\\0\"     \\
\t\"pxefile_addr_r=0x18200000\\0\" \\
\t\"ramdisk_addr_r=0x18300000\\0\"

#define BOOT_TARGET_DEVICES(func)  \\
\tfunc(MMC, mmc, 0)                \\
\tfunc(MMC, mmc, 1)                \\
\tfunc(SATA, sata, 0)              \\
\tfunc(USB, usb, 0)                \\
\tfunc(PXE, pxe, na)               \\
\tfunc(DHCP, dhcp, na)

#include <config_distro_bootcmd.h>

" anchor

;; Sadly, the user config CONFIG_DEFAULT_FDT_FILE did not exist in that older
;; U-Boot.  A placeholder is added here, to be substituted in each TS U-Boot
;; board package.
"\
\t\"fdtfile=DEFAULT_FDT_FILE\\0\" \\
\tMEM_LAYOUT_ENV_SETTINGS \\
\tBOOTENV \\\n")))))
              (add-after 'unpack 'patch-for-reproducibility
                (lambda _
                  ;; Substitute dynamically computed timestamps with static
                  ;; ones.
                  (substitute* "Makefile"
                    (("U_BOOT_DATE \"%b %d %C%y\"")
                     "U_BOOT_DATE \"Jan 01 1969\"")
                    (("U_BOOT_TIME \"%T\"")
                     "U_BOOT_TIME \"00:00:00\""))))
              (add-before 'build 'adjust-for-current-gcc
                (lambda _
                  (let ((gcc-major-version #$(version-major
                                              (package-version gcc))))
                    (copy-file "include/linux/compiler-gcc6.h"
                               (string-append "include/linux/compiler-gcc"
                                              gcc-major-version ".h")))
                  (substitute* "arch/arm/Makefile"
                    (("march=armv5")
                     "march=armv5te"))))
              (add-after 'install 'build+install-tools
                (lambda* (#:key make-flags #:allow-other-keys)
                  (apply invoke "make" "tools-all" make-flags)
                  (install-file "tools/env/fw_printenv"
                                (string-append #$output "/bin"))
                  (symlink (string-append #$output "/bin/fw_printenv")
                           (string-append #$output "/bin/fw_setenv"))))))))
      (native-inputs
       (modify-inputs (package-native-inputs u-boot)
         (delete "dtc"))))))            ;otherwise the build fails

;;; Note: the default cross-build of this package is currently broken on
;;; master; the fix exists as commit 6454208222d6e7760daa964b590f35ea75ffe0e5
;;; ("build: gnu-build-system: Remove source from native inputs.") on
;;; core-updates.
(define-public u-boot-ts7970-q-2g-1000mhz-c
  (let ((base
         (make-u-boot-package "ts7970-q-2g-1000mhz-c" "arm-linux-gnueabihf"
                              #:u-boot u-boot-ts-mx6
                              #:append-description
                              "This U-Boot variant is for the Technologic
Systems TS-7970 revision C board, which includes a quad core Freescale i.MX6
CPU and 2 GiB of RAM clocked at 1000MHz.  The binary U-Boot image to flash is
the @file{libexec/u-boot.imx} file.  It can be used with the @file{zImage} and
the @file{imx6q-ts7970.dtb} files provided by the
@code{linux-libre-arm-generic} image.

To flash this bootloader, write it to an SD card, then using the U-Boot serial
console:
@example
mmc dev 0
load mmc 0:1 ${loadaddr} /boot/u-boot.imx
sf probe
sf erase 0 0x80000
sf write ${loadaddr} 0x400 $filesize
@end example

The factory values of U-Boot must also be reset so that it boots using a
zImage instead of the default uImage:
@example
run clearenv
reset
@end example

For more information, refer to
@url{https://docs.embeddedts.com/TS-7970#Update_U-Boot}.")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'patch-u-boot 'set-default-fdt-file
                (lambda _
                  (substitute* "include/configs/ts7970.h"
                    (("DEFAULT_FDT_FILE")
                     "imx6q-ts7970.dtb")))))))))))

(define-public vboot-utils
  (package
    (name "vboot-utils")
    (version "R63-10032.B")
    (source (origin
              ;; XXX: Snapshots are available but changes timestamps every download.
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://chromium.googlesource.com"
                                        "/chromiumos/platform/vboot_reference"))
                    (commit (string-append "release-" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0h0m3l69vp9dr6xrs1p6y7ilkq3jq8jraw2z20kqfv7lvc9l1lxj"))
              (patches
               (search-patches "vboot-utils-skip-test-workbuf.patch"
                               "vboot-utils-fix-tests-show-contents.patch"
                               "vboot-utils-fix-format-load-address.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          ;; On ARM, we must pass "HOST_ARCH=arm" so that the
                          ;; ${HOST_ARCH} and ${ARCH} variables in the makefile
                          ;; match.  Otherwise, ${HOST_ARCH} will be assigned
                          ;; "armv7l", the value of `uname -m`, and will not
                          ;; match ${ARCH}, which will make the tests require
                          ;; QEMU for testing.
                          ,@(if (string-prefix? "arm"
                                                (or (%current-target-system)
                                                    (%current-system)))
                                '("HOST_ARCH=arm")
                                '())
                          (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-hard-coded-paths
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((coreutils (assoc-ref inputs "coreutils"))
                            (diffutils (assoc-ref inputs "diffutils")))
                        (substitute* "futility/misc.c"
                          (("/bin/cp") (string-append coreutils "/bin/cp")))
                        (substitute* "tests/bitmaps/TestBmpBlock.py"
                          (("/usr/bin/cmp") (string-append diffutils "/bin/cmp")))
                        (substitute* "vboot_host.pc.in"
                          (("prefix=/usr")
                           (string-append "prefix=" (assoc-ref outputs "out"))))
                        #t)))
                  (delete 'configure)
                  (add-before 'check 'patch-tests
                    (lambda _
                      ;; These tests compare diffs against known-good values.
                      ;; Patch the paths to match those in the build container.
                      (substitute* (find-files "tests/futility/expect_output")
                        (("/mnt/host/source/src/platform/vboot_reference")
                         (string-append "/tmp/guix-build-" ,name "-" ,version
                                        ".drv-0/source")))
                      ;; Tests require write permissions to many of these files.
                      (for-each make-file-writable (find-files "tests/futility"))
                      #t))
                  (add-after 'install 'install-devkeys
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (share (string-append out "/share/vboot-utils")))
                        (copy-recursively "tests/devkeys"
                                          (string-append share "/devkeys"))
                        #t))))
       #:test-target "runtests"))
    (native-inputs
     `(("pkg-config" ,pkg-config)

       ;; For tests.
       ("diffutils" ,diffutils)
       ("python@2" ,python-2)))
    (inputs
     `(("coreutils" ,coreutils)
       ("libyaml" ,libyaml)
       ("openssl" ,openssl)
       ("openssl:static" ,openssl "static")
       ("util-linux" ,util-linux "lib")))
    (home-page
     "https://dev.chromium.org/chromium-os/chromiumos-design-docs/verified-boot")
    (synopsis "ChromiumOS verified boot utilities")
    (description
     "vboot-utils is a collection of tools to facilitate booting of
Chrome-branded devices.  This includes the @command{cgpt} partitioning
program, the @command{futility} and @command{crossystem} firmware management
tools, and more.")
    (license license:bsd-3)))

(define-public os-prober
  (package
    (name "os-prober")
    (version "1.81")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://debian/pool/main/o/os-prober/os-prober_"
                           version ".tar.xz"))
       (sha256
        (base32 "10w8jz6mqhp0skdcam9mpgv79vx1sv7lkpra3rqjg0jkhvn2in9g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 regex)         ; for string-match
                  (srfi srfi-26))       ; for cut
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (find-files ".")
               (("/usr") (assoc-ref outputs "out")))
             (substitute* (find-files "." "50mounted-tests$")
               (("mkdir") "mkdir -p"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (define (find-files-non-recursive directory)
               (find-files directory
                           (lambda (file stat)
                             (string-match (string-append "^" directory "/[^/]*$")
                                           file))
                           #:directories? #t))

             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (share (string-append out "/share")))
               (for-each (cut install-file <> bin)
                         (list "linux-boot-prober" "os-prober"))
               (install-file "newns" (string-append lib "/os-prober"))
               (install-file "common.sh" (string-append share "/os-prober"))
               (install-file "os-probes/mounted/powerpc/20macosx"
                             (string-append lib "/os-probes/mounted"))
               (for-each
                (lambda (directory)
                  (for-each
                   (lambda (file)
                     (let ((destination (string-append lib "/" directory
                                                       "/" (basename file))))
                       (mkdir-p (dirname destination))
                       (copy-recursively file destination)))
                   (append (find-files-non-recursive (string-append directory "/common"))
                           (find-files-non-recursive (string-append directory "/x86")))))
                (list "os-probes" "os-probes/mounted" "os-probes/init"
                      "linux-boot-probes" "linux-boot-probes/mounted"))))))))
    (home-page "https://joeyh.name/code/os-prober")
    (synopsis "Detect other operating systems")
    (description "os-prober probes disks on the system for other operating
systems so that they can be added to the bootloader.  It also works out how to
boot existing GNU/Linux systems and detects what distribution is installed in
order to add a suitable bootloader menu entry.")
    (license license:gpl2+)))

(define-public ipxe
  ;; XXX: 'BUILD_TIMESTAMP' is used to automatically select the newest version
  ;; of iPXE if multiple iPXE drivers are loaded concurrently in a UEFI system.
  ;;
  ;; TODO: Bump this timestamp at each modifications of the package (not only
  ;; for updates) by running: date +%s.
  (let ((timestamp "1733491642")
        (commit "24db39fb2983ca83ab5c6ee37cb57a4f7f6f94e6")
        (revision "3"))
    (package
      (name "ipxe")
      (version (git-version "1.21.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ipxe/ipxe")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0b2h4bsdgnyjna6piwfqqn985vqfjmbz80jh0n7hrnncp2v53qj6"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules `((guix build utils)
                    (guix build gnu-build-system)
                    (guix base32)
                    (ice-9 string-fun)
                    (ice-9 regex)
                    (rnrs bytevectors))
        #:imported-modules `((guix base32)
                             ,@%default-gnu-imported-modules)
        #:make-flags
        ;; XXX: 'BUILD_ID' is used to determine when another ROM in the
        ;; system contains identical code in order to save space within the
        ;; legacy BIOS option ROM area, which is extremely limited in size.
        ;; It is supposed to be collision-free across all ROMs, to do so we
        ;; use the truncated output hash of the package.
        #~(let ((build-id
                 (lambda (out)
                   (let* ((nix-store (string-append
                                      (or (getenv "NIX_STORE") "/gnu/store")
                                      "/"))
                          (filename
                           (string-replace-substring out nix-store ""))
                          (hash (match:substring (string-match "[0-9a-z]{32}"
                                                               filename)))
                          (bv (nix-base32-string->bytevector hash)))
                     (format #f "0x~x"
                             (bytevector-u32-ref bv 0 (endianness big))))))
                (syslinux #$(this-package-native-input "syslinux")))
            (list "ECHO_E_BIN_ECHO=echo"
                  "ECHO_E_BIN_ECHO_E=echo -e"

                  ;; Build reproducibly.
                  (string-append "BUILD_ID_CMD=echo -n " (build-id #$output))
                  (string-append "BUILD_TIMESTAMP=" #$timestamp)
                  "everything"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'enter-source-directory
              (lambda _ (chdir "src")))
            (add-after 'enter-source-directory 'set-version
              (lambda _
                ;; When not building from a git checkout, iPXE encodes the
                ;; version as "1.0.0+".  Use the package version instead.
                (substitute* "Makefile"
                  (("^VERSION[[:blank:]]+=.*")
                   (string-append "VERSION = " #$(package-version this-package)
                                  "-guix\n")))))
            (add-after 'enter-source-directory 'set-options
              (lambda _
                (substitute* "config/general.h"
                  (("^//(#define PING_CMD.*)" _ uncommented) uncommented)
                  (("^//(#define IMAGE_TRUST_CMD.*)" _ uncommented)
                   uncommented)
                  (("^#undef.*(DOWNLOAD_PROTO_HTTPS.*)" _ option)
                   (string-append "#define " option))
                  (("^#undef.*(DOWNLOAD_PROTO_NFS.*)" _ option)
                   (string-append "#define " option)))))
            ;; It is not entirely clear why these fail to compile.
            (add-after 'enter-source-directory 'skip-i386-tap-linux
              (lambda _
                (substitute* "Makefile"
                  (("bin-i386-linux/tap.linux") "")
                  (("bin-i386-linux/tests.linux") ""))))
            #$@(if (target-x86?)
                 #~((add-after 'enter-source-directory 'set-syslinux-path
                      ;; cdrtools' mkisofs will silently ignore a missing isolinux.bin!
                      ;; Luckily xorriso is more strict.
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "util/genfsimg"
                          (("\t/usr/lib/syslinux " all)
                           (string-append
                             "\t" #$(this-package-native-input "syslinux")
                             "/share/syslinux \\\n"
                             all))))))
                 #~())
            (delete 'configure)         ; no configure script
            (replace 'install
              (lambda _
                (let* ((ipxe (string-append #$output "/lib/ipxe"))
                       (exts-re
                        "\\.(efi|efirom|iso|kkpxe|kpxe|lkrn|mrom|pxe|rom|usb)$")
                       (dirs '("bin" "bin-i386-linux" "bin-x86_64-pcbios"
                               "bin-x86_64-efi" "bin-x86_64-linux" "bin-i386-efi"))
                       (files (apply append
                                     (map (lambda (dir)
                                            (find-files dir exts-re)) dirs))))
                  (for-each (lambda (file)
                              (let* ((subdir (dirname file))
                                     (fn (basename file))
                                     (tgtsubdir (cond
                                                 ((string=? "bin" subdir) "")
                                                 ((string-prefix? "bin-" subdir)
                                                  (string-drop subdir 4)))))
                                (install-file file
                                              (string-append ipxe "/" tgtsubdir))))
                            files))))
            (add-after 'install 'leave-source-directory
              (lambda _ (chdir ".."))))
        #:tests? #f))                  ; no test suite
      (native-inputs
       (append (if (target-x86?)
                 ;; Syslinux only supports i686 and x86_64.
                 (list syslinux)
                 '())
               (list perl xorriso)))
      (home-page "https://ipxe.org")
      (synopsis "PXE-compliant network boot firmware")
      (description "iPXE is a network boot firmware.  It provides a full PXE
implementation enhanced with additional features such as booting from: a web
server via HTTP, an iSCSI SAN, a Fibre Channel SAN via FCoE, an AoE SAN, a
wireless network, a wide-area network, an Infiniband network.  It
controls the boot process with a script.  You can use iPXE to replace the
existing PXE ROM on your network card, or you can chainload into iPXE to obtain
the features of iPXE without the hassle of reflashing.")
      (license license:gpl2+))))

(define-public ipxe-qemu
  (package/inherit ipxe
    (name "ipxe-qemu")
    (native-inputs
     ;; QEMU uses a 64-bit UEFI firmware.
     (if (target-x86-64?)
         (modify-inputs (package-native-inputs ipxe)
           (prepend edk2-tools))
         (if (target-64bit?)
           (modify-inputs (package-native-inputs ipxe)
             (prepend edk2-tools
                      (cross-gcc "x86_64-linux-gnu")
                      (cross-binutils "x86_64-linux-gnu")))
           ;; Our default 32-bit binutils is not 64-bit capable.
           (let ((binutils-64-bit-bfd
                   (package/inherit
                     binutils
                     (name "binutils-64-bit-bfd")
                     (arguments
                       (substitute-keyword-arguments (package-arguments binutils)
                        ((#:configure-flags flags ''())
                         #~(cons* "--enable-64-bit-bfd" #$flags)))))))
             (modify-inputs (package-native-inputs ipxe)
               (prepend edk2-tools
                        (make-ld-wrapper "ld-wrapper-64-bit-bfd"
                                         #:binutils binutils)
                        binutils-64-bit-bfd
                        (cross-gcc "x86_64-linux-gnu")
                        (cross-binutils "x86_64-linux-gnu")))))))
    (arguments
     (let ((roms
            ;; Alist of ROM -> (VID . DID) entries.  This list and below
            ;; build steps are taken from QEMUs roms/Makefile.
            '(("e1000"       . ("8086" . "100e"))
              ("e1000e"      . ("8086" . "10d3"))
              ("eepro100"    . ("8086" . "1209"))
              ("ne2k_pci"    . ("1050" . "0940"))
              ("pcnet"       . ("1022" . "2000"))
              ("rtl8139"     . ("10ec" . "8139"))
              ("virtio"      . ("1af4" . "1000"))
              ("vmxnet3"     . ("15ad" . "07b0")))))
       (substitute-keyword-arguments (package-arguments ipxe)
         ((#:modules modules)
          `((ice-9 match) ,@modules))
         ((#:make-flags flags)
          #~(append (delete "everything" #$flags)
                    '("CONFIG=qemu"
                      #$@(if (target-x86-64?)
                             '()
                             '("CROSS_COMPILE=x86_64-linux-gnu-")))
                    (map (match-lambda
                           ((_ . (vid . did))
                            (string-append "bin/" vid did ".rom")))
                         '#$roms)
                    (map (match-lambda
                           ((_ . (vid . did))
                            (string-append "bin-x86_64-efi/"
                                           vid did ".efidrv")))
                         '#$roms)))
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'install
                (lambda _
                  (let ((firmware (string-append #$output "/share/qemu")))
                    (mkdir-p firmware)
                    (for-each
                     (match-lambda
                       ((name . (vid . did))
                        (let ((rom (string-append "bin/" vid did ".rom")))
                          (copy-file rom
                                     (string-append firmware
                                                    "/pxe-" name ".rom"))
                          (invoke "EfiRom"
                                  "-b" rom
                                  "-l" "0x02"
                                  "-f" (string-append "0x" vid)
                                  "-i" (string-append "0x" did)
                                  "-ec" (string-append "bin-x86_64-efi/"
                                                       vid did ".efidrv")
                                  "-o" (string-append firmware
                                                      "/efi-" name ".rom")))))
                     '#$roms)))))))))))
