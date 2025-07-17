;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Mark Wielaard <mark@klomp.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Daniel Maksymow <daniel.maksymow@tuta.io>
;;; Copyright © 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages elf)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl2+ gpl3+ lgpl3+ lgpl2.0+ lgpl2.1 gpl2 bsd-2))
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public chrpath
  (package
    (name "chrpath")
    (version "0.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/pere/chrpath")
                    (commit (string-append "release_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n7fp6xm660m8inaadlblh7zr8izyc3x39bfdqi6rj1kn0vmzra6"))))
    (build-system gnu-build-system)
    (native-inputs (list automake autoconf))
    (home-page "https://codeberg.org/pere/chrpath")
    (synopsis "Utility for editing the RPATH or RUNPATH of ELF binaries")
    (description "@code{chrpath} allows listing, changing or removing the
dynamic library load path (RPATH and RUNPATH) of compiled programs and
libraries.")
    (license gpl2+)))

(define-public elfutils
  (package
    (name "elfutils")
    (version "0.192")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceware.org/elfutils/ftp/"
                                  version "/elfutils-" version ".tar.bz2"))
              (sha256
               (base32
                "1d0nnkm59pwi9hrr28w0ifb6smldrjk6rn33kcgs3ar4msz9jq31"))
              (patches (search-patches "elfutils-tests-ptrace.patch"))))
    (build-system gnu-build-system)

    ;; Separate programs because that's usually not what elfutils users want,
    ;; and because they duplicate what Binutils provides (but are named
    ;; differently, using the eu- prefix and can be installed in parallel).
    (outputs '("out"                           ; libelf.so, elfutils/*.h, etc.
               "bin"))                         ; eu-nm, eu-objdump, etc.

    (arguments
     ;; Programs don't have libelf.so in their RUNPATH and libraries don't
     ;; know where to find each other.
     `(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib")
                               "--disable-static"
                               ;; TODO: Enable the debuginfo server.  It
                               ;; increases the closure size significantly
                               ;; and presents bootstrapping problems, so
                               ;; we disable it for now.  See
                               ;; https://issues.guix.gnu.org/38803 and
                               ;; https://sourceware.org/bugzilla/show_bug.cgi?id=25509
                               ;; for more information.
                               "--disable-libdebuginfod"
                               "--disable-debuginfod")

       ,@(if (target-hurd64?)
             '(#:make-flags '("core-file_no_Werror=yes"))
             '())

       ;; Disable tests on MIPS and PowerPC (without changing
       ;; the arguments list on other systems).
       ,@(if (any (cute string-prefix? <> (or (%current-target-system)
                                              (%current-system)))
                  '("mips" "powerpc"))
             '(#:tests? #f)
             '())

       #:phases
       ,#~(modify-phases %standard-phases
            ;; No reason has been found for this test to reliably fail on aarch64-linux.
            (add-after 'unpack 'disable-failing-aarch64-tests
              (lambda _
                (substitute* "tests/Makefile.in"
                  (("run-backtrace-native.sh") ""))
                #t))
            #$@(if (target-riscv64?)
                   #~((add-after 'unpack 'disable-failing-riscv64-test
                        (lambda _
                          (substitute* "tests/Makefile.in"
                            ;; dwfl_thread_getframes: No DWARF information found
                            (("run-backtrace-dwarf.sh") "")
                            ;; These tests have several errors:
                            ;; unknown program header entry type 0x70000003
                            ;; '.riscv.attributes' has unsupported type 1879048195
                            (("run-reverse-sections-self.sh") "")
                            (("run-strip-strmerge.sh") "")
                            (("run-elflint-self.sh") "")))))
                   #~())
            #$@(if (system-hurd?)
                   #~((add-after 'unpack 'skip-tests
                        (lambda _
                          (substitute* '("tests/elfstrtab.c"
                                         "tests/emptyfile.c")
                            (("elf_version \\(EV_CURRENT\\);" all)
                             "exit (77);"))
                          (substitute* '("tests/run-all-dwarf-ranges.sh"
                                         "tests/run-allfcts-multi.sh"
                                         "tests/run-attr-integrate-skel.sh"
                                         "tests/run-bug1-test.sh"
                                         "tests/run-copyadd-sections.sh"
                                         "tests/run-deleted.sh"
                                         "tests/run-get-units-split.sh"
                                         "tests/run-native-test.sh"
                                         "tests/run-readelf-loc.sh"
                                         "tests/run-readelf-ranges.sh"
                                         "tests/run-unit-info.sh"
                                         "tests/run-varlocs.sh")
                            (("^#!.*" all)
                             (string-append all "exit 77;\n"))))))
                   #~()))))

    (native-inputs (list m4))
    (inputs (list xz zlib))
    (home-page "https://sourceware.org/elfutils/")
    (synopsis "Collection of utilities and libraries to handle ELF files and
DWARF data")
    (description
     "Elfutils is a collection of utilities and libraries to read, create and
modify Executable and Linkable Format (@dfn{ELF}) binary files, find and
handle Debugging With Arbitrary Record Formats (@dfn{DWARF}) debug data,
symbols, thread state and stacktraces for processes and core files on
GNU/Linux.  Elfutils includes @file{libelf} for manipulating ELF files,
@file{libdw} for inspecting DWARF data and process state and utilities like
@command{eu-stack} (to show backtraces), @command{eu-nm} (for listing symbols
from object files), @command{eu-size} (for listing the section sizes of an
object or archive file), @command{eu-strip} (for discarding symbols),
@command{eu-readelf} (to see the raw ELF file structures),
@command{eu-elflint} (to check for well-formed ELF files),
@command{eu-elfcompress} (to compress or decompress ELF sections), and more.")

    ;; Libraries are dual-licensed LGPLv3.0+ | GPLv2, and programs are GPLv3+.
    (license lgpl3+)))

(define-public libabigail
  (package
    (name "libabigail")
    (home-page "https://sourceware.org/libabigail/")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceware.org/pub/libabigail/"
                                  "libabigail-" version ".tar.gz"))
              (sha256
               (base32
                "1nkg7fsqvdr453hrskscy6xqz6fv45mylpgv1357dw3blnbsw11p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--enable-bash-completion"
                           "--enable-manual")
       #:make-flags '("V=1")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-source
                    (lambda _
                      (substitute* "build-aux/ltmain.sh"
                        ;; Don't add -specs=/usr/lib/rpm/redhat/redhat-hardened-ld
                        ;; to the GCC command line.
                        (("compiler_flags=\"-specs=.*")
                         "compiler_flags=\n"))
                      #t))
                  (add-after 'build 'build-documentation
                    (lambda _
                      (invoke "make" "-C" "doc/manuals" "html-doc" "man" "info")))
                  (add-before 'check 'set-test-environment
                    (lambda _
                      (setenv "XDG_CACHE_HOME" "/tmp")
                      #t))
                  (add-after 'install 'install-documentation
                    (lambda _
                      (invoke "make" "-C" "doc/manuals"
                              "install-man-and-info-doc")))
                  (add-after 'install-documentation 'install-bash-completion
                    (lambda* (#:key outputs #:allow-other-keys)
                      (for-each (lambda (file)
                                  (install-file
                                   file (string-append (assoc-ref outputs "out")
                                                       "/share/bash-completion"
                                                       "/completions")))
                                (find-files "bash-completion" ".*abi.*"))
                      #t)))))
    (native-inputs
     (list pkg-config texinfo python-sphinx python))
    (propagated-inputs
     (list elfutils ;libabigail.la says -lelf
           libxml2))           ;in Requires.private of libabigail.pc
    (synopsis "Analyze application binary interfaces (ABIs)")
    (description
     "@dfn{ABIGAIL} stands for the Application Binary Interface Generic
Analysis and Instrumentation Library.  It is a framework which aims at
helping developers and software distributors to spot ABI-related issues
like interface incompatibility in ELF shared libraries by performing a
static analysis of the ELF binaries at hand.")
    (license lgpl3+)))

(define-public libelf
  (package
    (name "libelf")
    (version "0.8.13")
    (source
     (origin
       (method url-fetch)
       (uri (list
             ;; As of May 2019, the original URL at mr511.de redirects to a
             ;; domain that doesn't resolve.  Use these two mirrors instead.
             (string-append "https://fossies.org/linux/misc/old/"
                            "libelf-" version ".tar.gz")
             (string-append "https://ftp.osuosl.org/pub/blfs/conglomeration/"
                            "libelf/libelf-" version ".tar.gz")))
       (sha256
        (base32
         "0vf7s9dwk2xkmhb79aigqm0x0yfbw1j0b9ksm51207qwr179n6jr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This old 'configure' script doesn't support cross-compilation
         ;; well.  I.e., it fails to find the cross-compiler.  Also,
         ;; the old `configure' script doesn't support variables passed as
         ;; arguments.  A third problem is that config.sub is too old to
         ;; recognise aarch64 and powerpc64le.
         ;;
         ;; Solve this by regenerating the configure script and letting
         ;; autoreconf update 'config.sub'.  While 'config.sub' is updated
         ;; anyway, update 'config.guess' as well.
         (add-before 'bootstrap 'delete-configure
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (delete-file "configure")
             (delete-file "config.sub")
             (delete-file "config.guess")
             (for-each (lambda (file)
                         (install-file
                          (string-append
                           (assoc-ref (or native-inputs inputs) "automake")
                           "/share/automake-"
                           ,(version-major+minor (package-version automake))
                           "/" file) "."))
                       '("config.sub" "config.guess")))))))
    (native-inputs
     (list autoconf
           ;; For up-to-date 'config.guess' and 'config.sub'
           automake))
    (home-page (string-append "https://web.archive.org/web/20181111033959/"
                              "http://www.mr511.de/software/english.html"))
    (synopsis "ELF object file access library")
    (description "Libelf is a C library to access ELF object files.")
    (license lgpl2.0+)))

(define-public patchelf
  (package
    (name "patchelf")
    (version "0.18.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/NixOS/patchelf/releases/download/"
                   version
                   "/patchelf-" version ".tar.bz2"))
             (sha256
              (base32
               "02s7ap86rx6yagfh9xwp96sgsj0p6hp99vhiq9wn4mxshakv4lhr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           ;; Our GCC code ensures that RUNPATH is never empty, it includes
           ;; at least glibc/lib and gcc:lib/lib.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "tests/no-rpath.sh"
               ;; Disable checking for an empty runpath:
               (("^if test.*") "")
               ;; Find libgcc_s.so, which is necessary for the test:
               (("/xxxxxxxxxxxxxxx") (string-append (assoc-ref inputs "gcc:lib")
                                                    "/lib")))
             (substitute* "tests/replace-needed.sh"
               ;; This test assumes that only libc will be linked alongside
               ;; libfoo, but we also link libgcc_s.
               (("grep -v 'foo\\\\.so'") "grep -E 'libc.*\\.so'"))
             (substitute* "tests/set-empty-rpath.sh"
               ;; Binaries with empty RPATHs cannot run on Guix, because
               ;; we still need to find libgcc_s (see above).
               (("^\"\\$\\{SCRATCH\\}\"\\/simple.$") ""))
             ;; Skip this test for now.
             (substitute* "tests/Makefile.in"
               ((".*shared-rpath\\.sh \\.*") "")))))))
    (native-inputs
     `(("gcc:lib" ,gcc "lib")))
    (home-page "https://nixos.org/patchelf.html")
    (synopsis "Modify the dynamic linker and RPATH of ELF executables")
    (description
     "PatchELF allows the ELF \"interpreter\" and RPATH of an ELF binary to be
changed.")
    (license gpl3+)))

;; Newer patchelf may break binaries.  e.g. When setting RUNPATH for a Go
;; program.
;; See also: https://github.com/NixOS/patchelf/issues/482
(define-public patchelf-0.16
  (package
    (inherit patchelf)
    (name "patchelf")
    (version "0.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/NixOS/patchelf/releases/download/"
                    version
                    "/patchelf-" version ".tar.bz2"))
              (sha256
               (base32
                "0zdby3gpmm8q4735pviaq92zj41i2rdnhwhyrsb3sinc9hzmz4db"))))
    (arguments
     (substitute-keyword-arguments (package-arguments patchelf)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'fix-tests 'skip-empty-rpath-test
              (lambda _
                (substitute* "tests/set-empty-rpath.sh"
                  (("^\\$\\{SCRATCH\\}\\/simple.$") ""))))))))))

(define-public libdwarf
  (package
    (name "libdwarf")
    (version "0.11.0")
    (source (origin
              (method git-fetch)
              ;; The archive at
              ;; https://www.prevanders.net/libdwarf-0.5.0.tar.xz
              ;; has a bad date header (3600).
              (uri (git-reference
                    (url "https://github.com/davea42/libdwarf-code")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j4r6558rsgx7fhwa46mqix4jlxyf6m4h8i2nsxcq8j30siq5b85"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-shared")))
    (native-inputs (list autoconf automake libtool pkg-config python))
    (inputs (list elfutils))
    (home-page "https://www.prevanders.net/dwarf.html")
    (synopsis "Handle DWARF debugging information")
    (description "@code{libdwarf} is a library that handles the DWARF
debugging information format.")
    ;; See https://www.prevanders.net/dwarflicense.html:
    (license (list lgpl2.1 gpl2 bsd-2))))
