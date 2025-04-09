;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
;;; Copyright © 2019, 2022, 2024 Andy Tai <atai@atai.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2021, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2023 Simon South <simon@simonsouth.net>
;;; Copyright © 2023 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages assembly)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix build utils) #:select (parallel-job-count))
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages xml)
  #:use-module ((guix utils)
                #:select (%current-system cc-for-target)))

(define-public asl
  (let ((build "267"))
    (package
      (name "asl")
      (version (string-append "1.42-beta-" build))
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "http://john.ccac.rwth-aachen.de:8000/ftp/as/source/c_version/"
               "asl-current-142-bld" build ".tar.bz2"))
         (sha256
          (base32 "13j2ccfgji4jiqbbqmcchhcps11ypz8aq8fq9vd83ngbhavh6c9s"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags #~(list "V=1")     ; ensures output during "check" phase
        #:phases
        #~(modify-phases %standard-phases
            (delete 'bootstrap)
            (replace 'configure
              (lambda* (#:key target #:allow-other-keys)
                (copy-file "Makefile.def-samples/Makefile.def-unknown-linux"
                           "Makefile.def")

                ;; Use the cross-compilation tools when cross-compiling.
                (when #$(%current-target-system)
                  (substitute* "Makefile.def"
                    (("^(TARG_(CC|LD) = ).*" all prefix)
                     (string-append prefix target "-gcc\n"))))

                ;; Set the output directories appropriately.
                (substitute* "Makefile.def"
                  (("^(DOCDIR = ).*" all prefix)
                   (string-append prefix #$output:doc "/share/doc/" #$name))
                  (("/usr/local")
                   #$output))))
            (add-after 'check 'build-doc
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (invoke "make"
                        "-j" (if parallel-build?
                                 (number->string (parallel-job-count))
                                 "1")
                        "docs"))))
        #:test-target "test"))
      (native-inputs
       (list (texlive-updmap.cfg (list texlive-german texlive-hyperref))))
      (outputs '("out" "doc"))
      (home-page "http://john.ccac.rwth-aachen.de:8000/as/")
      (synopsis
       "AS macro cross-assembler for microprocessors and microcontrollers")
      (description
       "AS is a portable macro cross-assembler targeting a wide range of
microprocessors and microcontrollers, including devices from Intel, Motorola,
MOS Technology, Hitachi, Fujitsu, NEC, Texas Instruments, Zilog and many other
manufacturers.")
      (license (list license:gpl2 license:gpl3)))))

(define-public asm6f
  (package
    (name "asm6f")
    (version "1.6_freem02")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/freem/asm6f")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vz2mbnnm71sns0f08qjlg5rsw2kykg2v6bah073hfi6zzkqw52p"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no substantial test suite
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ;no configure phase
          (replace 'install
            (lambda _
              (install-file "asm6f" (string-append #$output "/bin"))))
          (add-after 'install 'install-readme
            (lambda _
              (install-file "readme.txt"
                            (string-append #$output "/share/doc/asm6f/")))))))
    (home-page "https://github.com/freem/asm6f")
    (synopsis "ASM6 6502 assembler fork for NES/Famicom")
    (description "ASM6f is a fork of ASM6, primarily targeted at NES/Famicom
development.
@itemize
@item Support for some illegal/undocumented opcodes
@item FCEUX-compatible @file{.nl} output files
@item Output of Lua-compatible symbol files
@item New @code{IGNORENL} and @code{ENDINL} directives
@item Support for iNES original and 2.0 header insertion
@item Output of @file{.cdl} files, for use with FCEUX/Mesen
@item Output of Mesen-compatible symbol files (both old and new formats)
@item Generic +/- labels do not break @@local scope
@item Support for @code{a:} prefix to force absolute addressing for
zero-page addresses.
@end itemize")
    ;; The license text reads: "This is free software.  You may use, modify,
    ;; and / or redistribute any part of this software in any fashion."
    (license (license:non-copyleft "LICENSE.txt"))))

(define-public nasm
  (package
    (name "nasm")
    (version "2.15.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.nasm.us/pub/nasm/releasebuilds/"
                                  version "/nasm-" version ".tar.xz"))
              (sha256
               (base32
                "0gqand86b0r86k3h46dh560lykxmxqqywz5m55kgjfq7q4lngbrw"))))
    (build-system gnu-build-system)
    (native-inputs (list perl ;for doc and test target
                         texinfo))
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-build-ps-pdf-outputs
           (lambda _
             (substitute* "doc/Makefile.in"
               (("html nasmdoc.txt nasmdoc.pdf \\$\\(XZFILES\\)")
                "html nasmdoc.txt")
               (("\\$\\(INSTALL_DATA\\) nasmdoc.pdf")
                "$(INSTALL_DATA)"))))
         (add-after 'install 'install-info
           (lambda _
             (invoke "make" "install_doc"))))))
    (home-page "https://www.nasm.us/")
    (synopsis "80x86 and x86-64 assembler")
    (description
     "NASM, the Netwide Assembler, is an 80x86 and x86-64 assembler designed
for portability and modularity.  It supports a range of object file formats,
including Linux and *BSD a.out, ELF, COFF, Mach-O, Microsoft 16-bit OBJ,
Windows32 and Windows64.  It will also output plain binary files.  Its syntax
is designed to be simple and easy to understand, similar to Intel's but less
complex.  It supports all currently known x86 architectural extensions, and
has strong support for macros.")
    (license license:bsd-2)))

(define-public yasm
  (package
    (name "yasm")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.tortall.net/projects/yasm/releases/yasm-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0gv0slmm0qpq91za3v2v9glff3il594x5xsrbgab7xcmnh0ndkix"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f))           ; Some tests fail
                                        ; non-deterministically when run in
                                        ; parallel
    (inputs
     `(("python" ,python-wrapper)
       ("xmlto" ,xmlto)))
    (home-page "https://yasm.tortall.net/")
    (synopsis "Rewrite of the NASM assembler")
    (description
     "Yasm is a complete rewrite of the NASM assembler.

Yasm currently supports the x86 and AMD64 instruction sets, accepts NASM
and GAS assembler syntaxes, outputs binary, ELF32, ELF64, 32 and 64-bit
Mach-O, RDOFF2, COFF, Win32, and Win64 object formats, and generates source
debugging information in STABS, DWARF 2, and CodeView 8 formats.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

(define-public lightning
  (package
    (name "lightning")
    (version "2.2.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/lightning/lightning-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1qmkfg7br543kqy82hhpr1n8bsm9wrwb1z5w2whxc5xdvr185jha"))))
    (build-system gnu-build-system)
    (native-inputs (list zlib))
    (arguments
     `(#:configure-flags
       (list "--disable-static")
       ;; Some tests fail when run in parallel.
       #:parallel-tests? #f))
    (synopsis "Library for generating assembly code at runtime")
    (description
     "GNU Lightning is a library that generates assembly language code at
run-time.  Thus, it is useful in creating Just-In-Time compilers.  It
abstracts over the target CPU by exposing a standardized RISC instruction set
to the clients.")
    (home-page "https://www.gnu.org/software/lightning/")
    (license license:gpl3+)))

(define-public simde
  (package
    (name "simde")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/simd-everywhere/simde")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0giijq5n3q1nv8c5skfq2dar70rgbsm7yk0gdj22wpsa58fc624a"))))
    (build-system meson-build-system)
    ;; We really want this for the headers, and the tests require a bundled library.
    (arguments '(#:configure-flags '("-Dtests=false")))
    (synopsis "Implementations of SIMD instruction sets for foreign systems")
    (description "The SIMDe header-only library provides fast, portable
implementations of SIMD intrinsics on hardware which doesn't natively support
them, such as calling SSE functions on ARM.  There is no performance penalty if
the hardware supports the native implementation (e.g., SSE/AVX runs at full
speed on x86, NEON on ARM, etc.).")
    (home-page "https://simd-everywhere.github.io/blog/")
    (license license:expat)))

(define-public fasm
  (package
    (name "fasm")
    (version "1.73.31")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://flatassembler.net/fasm-"
                           version ".tgz"))
       (sha256
        (base32 "1qqg1czr9dr73l4gwrwim85mjs65al7vv8b292jipywimhhwnf4g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests exist
       #:strip-binaries? #f             ; fasm has no sections
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no "configure" script
         (replace 'build
           (lambda _
             (chdir "source/Linux/")
             (if (string=? ,(%current-system) "x86_64-linux")
                 ;; Use pre-compiled binaries in top-level directory to build
                 ;; fasm.
                 (invoke "../../fasm.x64" "fasm.asm")
                 (invoke "../../fasm" "fasm.asm"))))
         (replace 'install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (install-file "fasm" (string-append out "/bin"))))))))
    (supported-systems '("x86_64-linux" "i686-linux"))
    (synopsis "Assembler for x86 processors")
    (description
     "@acronym{FASM, the Flat ASseMbler} is an assembler that supports x86 and
IA-64 Intel architectures.  It does multiple passes to optimize machine code.
It has macro abilities and focuses on operating system portability.")
    (home-page "https://flatassembler.net/")
    (license license:bsd-2)))

(define-public dev86
  (package
    (name "dev86")
    (version "0.16.21")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://v3.sk/~lkundrak/dev86/Dev86src-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "154dyr2ph4n0kwi8yx0n78j128kw29rk9r9f7s2gddzrdl712jr3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; They use submakes wrong
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:system "i686-linux" ; Standalone ld86 had problems otherwise
       #:tests? #f ; No tests exist
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'install 'mkdir
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (mkdir-p (string-append out "/bin"))
              (mkdir-p (string-append out "/man/man1"))
              #t))))))
    (synopsis "Intel 8086 (primarily 16-bit) assembler, C compiler and
linker")
    (description "This package provides a Intel 8086 (primarily 16-bit)
assembler, a C compiler and a linker.  The assembler uses Intel syntax
(also Intel order of operands).")
    (home-page "https://github.com/jbruchon/dev86")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))

(define-public libjit
  (let ((commit "554c9f5c750daa6e13a6a5cd416873c81c7b8226"))
    (package
      (name "libjit")
      (version "0.1.4")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/r/libjit.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0p6wklslkkp3s4aisj3w5a53bagqn5fy4m6088ppd4fcfxgqkrcd"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf
             automake
             bison
             flex
             help2man
             gettext-minimal
             libtool
             texinfo
             pkg-config))
      (home-page "https://www.gnu.org/software/libjit/")
      (synopsis "Just-In-Time compilation library")
      (description
       "GNU libjit is a library that provides generic Just-In-Time compiler
functionality independent of any particular bytecode, language, or
runtime")
      (supported-systems '("i686-linux" "x86_64-linux" "aarch64-linux"))
      (license license:lgpl2.1+))))

(define-public rgbds
  (package
    (name "rgbds")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gbdev/rgbds")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gy75q0ikx0ki1wsrq97hxj9dw9436fcys2w91ipm90pbhk4ljva"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-pkg-config
           (lambda _
             (substitute* "Makefile"
               (("pkg-config")
                (or (which "pkg-config")
                    (string-append ,(%current-target-system)
                                   "-pkg-config"))))))
         (replace 'check
           (lambda _
             (with-directory-excursion "test/asm"
               (invoke "./test.sh"))
             (with-directory-excursion "test/link"
               (invoke "./test.sh")))))
       #:make-flags `(,(string-append "CC=" ,(cc-for-target))
                      ,(string-append "PREFIX="
                                      (assoc-ref %outputs "out")))))
    (native-inputs
     (list bison flex pkg-config util-linux))
    (inputs
     (list libpng))
    (home-page "https://github.com/gbdev/rgbds")
    (synopsis "Rednex Game Boy Development System")
    (description
     "RGBDS (Rednex Game Boy Development System) is an assembler/linker
package for the Game Boy and Game Boy Color.  It consists of:
@itemize @bullet
@item rgbasm (assembler)
@item rgblink (linker)
@item rgbfix (checksum/header fixer)
@item rgbgfx (PNG-to-Game Boy graphics converter)
@end itemize")
    (license license:expat)))

(define-public wla-dx
  (package
    (name "wla-dx")
    (version "10.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vhelin/wla-dx")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h6apmhaks4772s2cja34ck488p8yhb3nscbxjw5061ml2046zqq"))))
    (build-system cmake-build-system)
    (native-inputs (list python-sphinx)) ; to generate man pages
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'copy-tests-to-build-directory
            (lambda _
              (copy-recursively "../source/tests" "tests")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (let ((sh (which "sh")))
                (when tests?
                  (invoke sh "../source/run_tests.sh"))))))))
    (home-page "https://github.com/vhelin/wla-dx")
    (synopsis "Assemblers for various processors")
    (description "WLA DX is a set of tools to assemble assembly files to
object or library files (@code{wla-ARCH}) and link them together (@code{wlalink}).
Supported architectures are:

@itemize @bullet
@item z80
@item gb (z80-gb)
@item 6502
@item 65c02
@item 6510
@item 65816
@item 6800
@item 6801
@item 6809
@item 8008
@item 8080
@item huc6280
@item spc700
@end itemize")
    (license license:gpl2)))

(define-public xa
  (package
    (name "xa")
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.floodgap.com/retrotech/xa"
                                  "/dists/xa-" version ".tar.gz"))
              (sha256
               (base32
                "1hrspv9hxgk2nkbbh24g84hn0rglfwj8p7849zrn9qx869m2mhb3"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))          ; no "configure" script
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "LD=" #$(cc-for-target))
              (string-append "DESTDIR=" #$output)))) ; no $prefix support
    (native-inputs (list perl))
    (home-page "https://www.floodgap.com/retrotech/xa/")
    (synopsis "Two-pass portable cross-assembler")
    (description
     "xa is a high-speed, two-pass portable cross-assembler.
It understands mnemonics and generates code for NMOS 6502s (such
as 6502A, 6504, 6507, 6510, 7501, 8500, 8501, 8502 ...),
 CMOS 6502s (65C02 and Rockwell R65C02) and the 65816.")
    (license license:gpl2)))

(define-public armips
  (let ((commit "6719edebaae03330ee5441d9b28280672edf00d5")
        (revision "1"))
    (package
      (name "armips")
      (version "0.11.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Kingcom/armips")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1a85h2b3r3hy9hm07v8drvkklp4qfdq3i3zwb3cgk011s0njdfvz"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
              (substitute* "Core/Types.h"
                (("#include <string>" all)
                 (string-append all "\n"
                                "#include <string_view>")))))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DARMIPS_USE_STD_FILESYSTEM=ON")
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs #:allow-other-keys)
               (invoke "./armipstests" "../source/Tests")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "armips" (string-append (assoc-ref outputs "out")
                                                     "/bin"))
               #t)))))
      (home-page "https://github.com/Kingcom/armips")
      (synopsis "Assembler for various ARM and MIPS platforms")
      (description
       "armips is an assembler with full support for the MIPS R3000, MIPS R4000,
Allegrex and RSP instruction sets, partial support for the EmotionEngine
instruction set, as well as complete support for the ARM7 and ARM9 instruction
sets, both THUMB and ARM mode.")
      (license license:expat))))

(define-public intel-xed
  (package
    (name "intel-xed")
    (version "2025.03.02")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intelxed/xed")
             (commit (string-append "v" version))))
       (sha256 (base32 "1h1q1rq96mh0jaqsg5pwmxwrcbpb83i79kblzfqszlq2nn0ih55j"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-wrapper)
       ;; As of the time of writing this comment, mbuild does not exist in the
       ;; Python Package Index and seems to only be used by intel-xed, so we
       ;; opt to include it here instead of packaging separately.  Note also
       ;; that the git repository contains no version tags, so we directly
       ;; reference the "version" variable from setup.py instead.
       ("mbuild"
        ,(let ((name "mbuild")
               (version "2022.07.28"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/intelxed/mbuild")
                   (commit (string-append "v" version))))
             (sha256
              (base32
               "0rc9xp202yqw42bwgylwxvfvjr1crdl50qvv8vzfczyvlf4wflcx"))
             (file-name (git-file-name name version)))))))
    (outputs '("out" "lib"))
    (arguments
     `(#:phases
       ;; Upstream uses the custom Python build tool `mbuild', so we munge
       ;; gnu-build-system to fit.  The build process for this package is
       ;; documented at https://intelxed.github.io/build-manual/.
       (let* ((build-dir "build")
              (kit-dir "kit"))
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((mbuild (assoc-ref inputs "mbuild")))
                 (setenv "PYTHONPATH" mbuild)
                 (invoke "./mfile.py"
                         (string-append "--build-dir=" build-dir)
                         (string-append "--install-dir=" kit-dir)
                         "examples"
                         "doc"
                         "install"))))
           (replace 'check
             (lambda _
               ;; Skip broken test group `tests/tests-avx512pf'.
               (invoke "tests/run-cmd.py"
                       (string-append "--build-dir=" kit-dir "/bin")
                       "--tests" "tests/tests-base"
                       "--tests" "tests/tests-avx512"
                       "--tests" "tests/tests-cet"
                       "--tests" "tests/tests-via"
                       "--tests" "tests/tests-syntax"
                       "--tests" "tests/tests-xop")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (assoc-ref outputs "lib")))
                 (copy-recursively (string-append kit-dir "/bin")
                                   (string-append out "/bin"))
                 (copy-recursively (string-append kit-dir "/include")
                                   (string-append lib "/include"))
                 (copy-recursively (string-append kit-dir "/lib")
                                   (string-append lib "/lib")))))))))
    (home-page "https://intelxed.github.io/")
    (synopsis "Encoder and decoder for x86 (IA32 and Intel64) instructions")
    (description "The Intel X86 Encoder Decoder (XED) is a software library and
for encoding and decoding X86 (IA32 and Intel64) instructions.  The decoder
takes sequences of 1-15 bytes along with machine mode information and produces
a data structure describing the opcode, operands, and flags.  The encoder takes
a similar data structure and produces a sequence of 1 to 15 bytes.  Disassembly
is essentially a printing pass on the data structure.

The library and development files are under the @code{lib} output, with a
family of command line utility wrappers in the default output.  Each of the cli
tools is named like @code{xed*}.  Documentation for the cli tools is sparse, so
this is a case where ``the code is the documentation.''")
    (license license:asl2.0)))

(define-public neon2sse
  (let ((commit "097a5ecacd527d5b5c3006e360fb9cb1c1c48a1f")
        (version "0")
        (revision "1"))
    (package
      (name "neon2sse")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/intel/ARM_NEON_2_x86_SSE")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "17mf788b8asrvjl6dnyzrm5xrz20wx9j5f8n6drgc6qgwqxpx4hv"))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f)) ;no tests
      (home-page "https://github.com/intel/ARM_NEON_2_x86_SSE")
      (synopsis "Header file to simplify ARM->IA32 porting")
      (description
       "The @file{NEON_2_SSE.h} file is intended to simplify ARM-to-IA32
porting.  It makes the correspondence (or a real porting) of ARM NEON
intrinsics as defined in the @file{arm_neon.h} header and x86 SSE (up to
SSE4.2) intrinsic functions as defined in corresponding x86 compilers headers
files.")
      (license license:bsd-2))))

(define-public cpu-features
  (package
    (name "cpu-features")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/cpu_features")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0297li3408zm1dqnibaasrb51vs7n7iscnxsji3b78g0pir7jwxr"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DBUILD_TESTING=off" ;; XXX: insists on using bundled googletest
              "-DBUILD_SHARED_LIBS=ON"
              #$@(let ((target (%current-target-system)))
                   (if target
                       (cond ((string-prefix? "arm" target)
                              '("-DCMAKE_SYSTEM_PROCESSOR=arm"))
                             ((string-prefix? "aarch64" target)
                              '("-DCMAKE_SYSTEM_PROCESSOR=aarch64"))
                             ((string-prefix? "i686" target)
                              '("-DCMAKE_SYSTEM_PROCESSOR=x86"))
                             ((string-prefix? "x86_64" target)
                              '("-DCMAKE_SYSTEM_PROCESSOR=x86_64"))
                             ;; 32-bit and 64-bit
                             ((string-prefix? "powerpc" target)
                              '("-DCMAKE_SYSTEM_PROCESSOR=powerpc"))
                             ((string-prefix? "riscv64" target)
                              '("-DCMAKE_SYSTEM_PROCESSOR=riscv64"))
                             (else '()))
                       '())))))
    (home-page "https://github.com/google/cpu_features")
    (synopsis "Cross platform C99 library to get cpu features at runtime")
    (description
     "Cpu_features is a cross-platform C library to retrieve CPU features
(such as available instructions) at runtime, and supports these CPU architectures
@itemize
@item x86-64
@item AArch64
@item ARM
@item MIPS
@item POWER
@item RISC-V
@item LoongArch
@item S390x
@end itemize")
    (license license:asl2.0)))


(define-public blinkenlights
  (package
    (name "blinkenlights")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jart/blink")
                    (commit version)))
              (sha256
               (base32
                "0dgfqy5z1vbpgbf39f14ngkqmw4gi3hsyihi4sh1qcbp9gnqpg2v"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                           ;Tests require network access
           #:phases
           #~(modify-phases %standard-phases
               ;; Call ./configure without --enable-fast-install argument, which
               ;; causes the script to fail with an "unsupported option" error.
               (replace 'configure
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (invoke "./configure"
                           (string-append "CC=" #$(cc-for-target))
                           (string-append "--prefix="
                                          (assoc-ref outputs "out"))))))))
    (home-page "https://justine.lol/blinkenlights/")
    (synopsis "Emulator for x86_64-linux programs with a text user interface")
    (description
     "Blinkenlights is a command-line debugger that focuses on visualizing how
software changes memory.  It can emulate statically-linked i8086 and
x86_64-pc-linux-gnu programs.")
    (license license:isc)))
