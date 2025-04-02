;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020, 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 David Dashyan <mail@davie.li>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2022 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2023 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2023, 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2023, 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright @ 2022, Kitzman <kitzman@disroot.org>
;;; Copyright @ 2025 Dariqq <dariqq@posteo.net>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages c)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix store)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public c-intro-and-ref
  (let ((revision "1")
        (commit "47e5a234a7c036392e0f9e1e8e48ff3e6855840d"))
    (package
      (name "c-intro-and-ref")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/c-intro-and-ref.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0aza4vvlg2w0ss6n5xp741ycvg16d041c1x87yh5hpnzcb6y0ii3"))))
      (build-system copy-build-system)
      (arguments
       (list #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'build
                            (lambda* (#:key parallel-build? #:allow-other-keys)
                              (substitute* "Makefile"
                                (("makeinfo c.texi")
                                 "makeinfo --no-split c.texi"))
                              (invoke "make" "c.info" "c.html"
                                      "-j" (number->string
                                            (if parallel-build?
                                                (parallel-job-count)
                                                1))))))
             #:install-plan ''(("c.info" "share/info/")
                               ("c.html" "share/doc/"))))
      (native-inputs (list texinfo))
      (home-page "https://www.gnu.org/")
      (synopsis "GNU C Language Intro and Reference")
      (description "This manual explains the C language for use with the GNU
Compiler Collection (GCC) on the GNU/Linux system and other systems.  We refer
to this dialect as GNU C.  If you already know C, you can use this as a
reference manual.")
      (license license:fdl1.3+))))

(define-public c-rrb
  (let ((commit "d908617ff84515af90c454ff4d0f98675ae6b456")
        (revision "0"))
    (package
     (name "c-rrb")
     (version (git-version "0.1.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hypirion/c-rrb")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0zmha3xi80vgdcwzb4vwdllf97dvggjpjfgahrpsb5f5qi3yshxa"))))
     (build-system gnu-build-system)
     (inputs (list libgc))
     (native-inputs (list autoconf automake libtool))
     (home-page "https://github.com/hypirion/c-rrb")
     (synopsis "Relaxed Radix Balanced Trees")
     (description "Relaxed Radix Balanced Trees are an immutable vector-like
data structure with good performance characteristics for concatenation and
slicing.")
     (license license:boost1.0))))

(define-public cproc
  (let ((commit "70fe9ef1810cc6c05bde9eb0970363c35fa7e802")
        (revision "1"))
    (package
      (name "cproc")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~mcf/cproc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qmgzll7z7mn587azkj4cizyyd8ii6iznfxpc66ja08140sbn9yx"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags
        #~(list (string-append "CC=" #$(cc-for-target))
                (string-append "PREFIX=" #$output))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((gcc-lib (assoc-ref inputs "gcc:lib"))
                      (host-system #$(nix-system->gnu-triplet
                                      (%current-system)))
                      (target-system #$(nix-system->gnu-triplet
                                        (or (%current-target-system)
                                            (%current-system)))))
                  (invoke "./configure"
                          (string-append "--prefix=" #$output)
                          (string-append "--host=" host-system)
                          (string-append "--target=" target-system)
                          (string-append "--with-ld=" #$(ld-for-target))
                          (string-append "--with-gcc-libdir=" gcc-lib))))))))
      (inputs `(("qbe" ,qbe)
                ("gcc:lib" ,gcc "lib")))
      (supported-systems (list "x86_64-linux" "aarch64-linux"))
      (synopsis "Simple C11 compiler backed by QBE")
      (description "@code{cproc} is a C compiler using QBE as a backend,
 supporting most of C11 along with some GCC and C2x extensions.")
      (home-page "https://sr.ht/~mcf/cproc")
      (license license:expat))))

(define-public stringzilla
  (package
    (name "stringzilla")
    (version "3.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ashvardanian/StringZilla")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v1wh4lxxd1bymbiadnaj8207d85c88ic6wdxki1967f7iij8m4k"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DSTRINGZILLA_IS_MAIN_PROJECT=ON"
                   "-DSTRINGZILLA_INSTALL=ON"
                   (string-append "-DSTRINGZILLA_INCLUDE_INSTALL_DIR="
                                  #$output "/include")
                   ;; One fails, another takes very long.
                   "-DSTRINGZILLA_BUILD_BENCHMARK=OFF")
           #:phases
           #~(modify-phases %standard-phases
               ;; The install process moves a file into
               ;; "/usr/src/stringzilla/".  Fix this.
               (add-after 'unpack 'install-in-a-standard-location
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("(install\\(DIRECTORY \\./c/ DESTINATION ).*" _ prefix)
                      (string-append prefix
                                     #$output
                                     "/share/stringzilla/)\n")))))
               ;; AVX512 support can be disabled by defining SZ_USE_X86_AVX512
               ;; to 0 before including the header.
               (add-after 'unpack 'skip-failing-test
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("^ *define_launcher\\(.*_avx512 .*") "")))))))
    (home-page "https://github.com/ashvardanian/StringZilla")
    (synopsis "C/C++ header-only string library using SIMD and SWAR")
    (description
     "StringZilla is a C/C++ header-only string library which relies on SIMD
and SWAR.  It implements string search, edit distances, sorting, lazy ranges,
hashes and fingerprints.")
    (license (list license:asl2.0 license:bsd-3)))) ; readme says dual-licensed

(define-public tcc
  ;; There's currently no release fixing <https://issues.guix.gnu.org/52140>.
  (let ((revision "1")
        (commit "a83b28568596afd8792fd58d1a5bd157fc6b6634"))
    (package
      (name "tcc")                      ;aka. "tinycc"
      (version (git-version "0.9.27" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://repo.or.cz/tinycc.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01znw86fg73x3k0clafica4b6glbhz69p588kvp766i0zgvs68dh"))))
      (build-system gnu-build-system)
      (native-inputs (list perl texinfo))
      (arguments
       `(#:configure-flags (list (string-append "--elfinterp="
                                                (assoc-ref %build-inputs
                                                           "libc")
                                                ,(glibc-dynamic-linker))
                                 (string-append "--crtprefix="
                                                (assoc-ref %build-inputs
                                                           "libc") "/lib")
                                 (string-append "--sysincludepaths="
                                                (assoc-ref %build-inputs
                                                           "libc") "/include:"
                                                (assoc-ref %build-inputs
                                                           "kernel-headers")
                                                "/include:{B}/include")
                                 (string-append "--libpaths="
                                                (assoc-ref %build-inputs
                                                           "libc") "/lib")
                                 ,@(if (string-prefix? "armhf-linux"
                                                       (or (%current-target-system)
                                                           (%current-system)))
                                       `("--triplet=arm-linux-gnueabihf")
                                       '()))
         #:test-target "test"))
      (native-search-paths
       (list (search-path-specification
              (variable "CPATH")
              (files '("include")))
             (search-path-specification
              (variable "LIBRARY_PATH")
              (files '("lib" "lib64")))))
      ;; Fails to build on MIPS: "Unsupported CPU"
      (supported-systems (delete "mips64el-linux" %supported-systems))
      (synopsis "Tiny and fast C compiler")
      (description
       "TCC, also referred to as \"TinyCC\", is a small and fast C compiler
written in C.  It supports ANSI C with GNU and extensions and most of the C99
standard.")
      (home-page "http://www.tinycc.org/")
      ;; An attempt to re-licence tcc under the Expat licence is underway but not
      ;; (if ever) complete.  See the RELICENSING file for more information.
      (license license:lgpl2.1+))))

(define-public tomlc99
  (let ((revision "1")
        (commit "5221b3d3d66c25a1dc6f0372b4f824f1202fe398"))
    (package
      (name "tomlc99")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/cktan/tomlc99")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1r443cpwy7f1f8imyfykpggkihrvv9fvdlgl95liiqmzqz9snqnd"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags #~(list (string-append "CC="
                                                 #$(cc-for-target))
                                  (string-append "prefix="
                                                 #$output))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (replace 'check
                   (lambda* (#:key tests? make-flags
                             #:allow-other-keys)
                     (when tests?
                       (apply invoke
                              `("make" "-C" "unittest"
                                ,@make-flags))
                       (invoke "./unittest/t1"))))
                 ;; The Makefile checks for libtoml.pc and only installs if
                 ;; the prefix is /usr/local.
                 (add-after 'install 'install-pkg-config
                   (lambda _
                     (rename-file "libtoml.pc.sample" "libtoml.pc")
                     (substitute* "libtoml.pc"
                       (("^prefix=.*")
                        (string-append "prefix=" #$output "\n")))

                     (let ((pc (string-append #$output "/lib/pkgconfig")))
                       (mkdir-p pc)
                       (install-file "libtoml.pc" pc)))))))
      (home-page "https://github.com/cktan/tomlc99")
      (synopsis "TOML library for C")
      (description
       "This library is a C99 implementation to read
@acronym{TOML, Tom's Obvious Minimal Language} text documents.

This library is compatible with the @url{https://toml.io/en/v1.0.0,v1.0.0}
specification of the language.")
      (license license:expat))))

(define-public pcc
  (package
    (name "pcc")
    (version "20170109")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pcc.ludd.ltu.se/ftp/pub/pcc/pcc-"
                                  version ".tgz"))
              (sha256
               (base32
                "1p34w496095mi0473f815w6wbi57zxil106mg7pj6sg6gzpjcgww"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-multiple-definitions
                 (lambda _
                   ;; Certain variables are defined multiple times. This
                   ;; upsets the linker and causes a build failure.
                   (substitute* "cc/ccom/pass1.h"
                     (("FLT flt_zero;") "extern FLT flt_zero;"))
                   (substitute* (list "cc/ccom/scan.l" "cc/cxxcom/scan.l")
                     (("lineno, ") ""))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "make" "-C" "cc/cpp" "test")))))))
    (native-inputs (list bison flex))
    (synopsis "Portable C compiler")
    (description
     "PCC is a portable C compiler.  The project goal is to write a C99
compiler while still keeping it small, simple, fast and understandable.")
    (home-page "http://pcc.ludd.ltu.se")
    (supported-systems (delete "aarch64-linux" %supported-systems))
    ;; PCC incorporates code under various BSD licenses; for new code bsd-2 is
    ;; preferred.  See http://pcc.ludd.ltu.se/licenses/ for more details.
    (license (list license:bsd-2 license:bsd-3))))

(define-public qbe
  (package
    (name "qbe")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://c9x.me/qbe")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sxz5dn788n5c4v6mxa2kg3hf0a4qryg8wp0w3wx0qkzj6flj2sj"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'allow-cross-compilation
                 (lambda _
                   (substitute* "Makefile"
                     (("`uname -m`") #$(or (%current-target-system)
                                           (%current-system))))))
               (delete 'configure))))
    (supported-systems (list "x86_64-linux" "aarch64-linux" "riscv64-linux"))
    (synopsis "Simple compiler backend")
    (description
     "QBE is a small compiler backend using an SSA-based intermediate
language as input.")
    (home-page "https://c9x.me/compile/")
    (license license:expat)))

(define-public python-pcpp
  (package
    (name "python-pcpp")
    (version "1.30")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ned14/pcpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1rihvlg11nzk70kfzz4i3gi5izcy46w05ismcx04p5j1hlim0brb"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'unbundle-ply
                     (lambda _
                       (rmdir "pcpp/ply")
                       (substitute* "setup.py"
                         (("'pcpp/ply/ply'") "")))))))
    (native-inputs (list python-pytest))
    (propagated-inputs (list python-ply))
    (home-page "https://github.com/ned14/pcpp")
    (synopsis "C99 preprocessor written in Python")
    (description "This package provides a C99 preprocessor written in pure
Python.")
    (license license:bsd-3)))

(define-public aml
  (package
    (name "aml")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/any1/aml")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r4k233hh3iqc6zlhh2agmdj8q973x49pdixhz7h5hz7md38qzq5"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/any1/aml")
    (synopsis "Another/Andri's main loop")
    (description "The @code{aml} library provides a portable event loop for C,
with the following features:
@enumerate
@item file descriptor event handlers
@item timers and tickers
@item signal handlers
@item idle dispatch callbacks
@item thread pool support
@end enumerate
")
    (license license:isc)))

(define-public libbytesize
  (package
    (name "libbytesize")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/storaged-project/libbytesize/releases/"
                    "download/" version "/libbytesize-" version ".tar.gz"))
              (sha256
               (base32
                "1lfa02ac96p12xxq75ilx3qk7kym4xrlqyfh26axb7y5iazf670x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     (list gettext-minimal pkg-config python))
    (inputs
     (list mpfr pcre2))
    (home-page "https://github.com/storaged-project/libbytesize")
    (synopsis "Tiny C library for working with arbitrary big sizes in bytes")
    (description
     "The goal of this project is to provide a tiny library that would
facilitate the common operations with sizes in bytes.  Many projects need to
work with sizes in bytes (be it sizes of storage space, memory...) and all of
them need to deal with the same issues like:

@itemize
@item How to get a human-readable string for the given size?
@item How to store the given size so that no significant information is lost?
@item If we store the size in bytes, what if the given size gets over the
MAXUINT64 value?
@item How to interpret sizes entered by users according to their locale and
typing conventions?
@item How to deal with the decimal/binary units (MB versus MiB) ambiguity?
@end itemize

@code{libbytesize} offers a generally usable solution that could be used by
every project that needs to deal with sizes in bytes.  It is written in the C
language with thin bindings for other languages.")
    (license license:lgpl2.1+)))

(define-public udunits
  (package
    (name "udunits")
    ;; Four-part version numbers are development snapshots, not releases.  See
    ;; <https://github.com/Unidata/UDUNITS-2/issues/99#issuecomment-732323472>.
    (version "2.2.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.unidata.ucar.edu/pub/udunits/"
                                  "udunits-" version ".tar.gz"))
              (sha256
               (base32
                "17jpbp6f0rr132jn2gqy8ry8mv1w27v6dyhfq1igv8v1674aw2sr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (inputs
     (list expat))
    (home-page "https://www.unidata.ucar.edu/software/udunits/")
    (synopsis "C library for units of physical quantities and value-conversion utils")
    (description
     "The UDUNITS-2 package provides support for units of physical quantities.
Its three main components are:

@enumerate
@item @code{udunits2lib}, a C library for units of physical quantities;
@item @code{udunits2prog}, a utility for obtaining the definition of a unit
  and for converting numeric values between compatible units; and
@item an extensive database of units.
@end enumerate\n")
    ;; Like the BSD-3 license but with an extra anti patent clause.
    (license (license:non-copyleft "file://COPYRIGHT"))))

(define-public libfixposix
  (package
    (name "libfixposix")
    (version "0.4.3")
    (home-page "https://github.com/sionescu/libfixposix")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1x4q6yspi5g2s98vq4qszw4z3zjgk9l5zs8471w4d4cs6l97w08j"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config check))
    (native-search-paths
     (list
      (search-path-specification
       (variable "C_INCLUDE_PATH")
       (files '("include")))))
    (synopsis "Thin wrapper over POSIX syscalls")
    (description
     "The purpose of libfixposix is to offer replacements for parts of POSIX
whose behaviour is inconsistent across *NIX flavours.")
    (license license:boost1.0)))

(define-public libhx
  (package
    (name "libhx")
    (version "4.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://inai.de/files/libhx/"
                           "libHX-" version ".tar.xz"))
       (sha256
        (base32 "16rwp8b2j8l0m27rffvb7ma350r79l611sa135hzfywkdli2bqh2"))))
    (build-system gnu-build-system)
    (home-page "https://inai.de/projects/libhx/")
    (synopsis "C library with common data structures and functions")
    (description
     "This is a C library (with some C++ bindings available) that provides data
structures and functions commonly needed, such as maps, deques, linked lists,
string formatting and autoresizing, option and config file parsing, type
checking casts and more.")
    (license license:lgpl2.1+)))

(define-public libcsptr
  (package
   (name "libcsptr")
   (version "2.0.3")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Snaipe/libcsptr")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32 "056yg1irxi90ccgf646jxzngbsm946ckyzn8ykl92h6d2mnr141a"))))
   (build-system gnu-build-system)
   (native-inputs (list autoconf automake libtool))
   (home-page "https://snai.pe/c/c-smart-pointers/")
   (synopsis "Smart pointers for GNU C")
   (description "This package is an attempt at bringing smart pointers
like C++'s unique_ptr and shared_ptr to C through GCC's cleanup attribute.")
   (license license:expat)))

(define-public libwuya
  ;; This commit is the one before "wuy_pool.h" was removed from libwuya,
  ;; which libleak currently requires.
  (let ((revision "1")
        (commit "883502041044f4616cfbf75c8f2bb60059f704a9"))
    (package
      (name "libwuya")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/WuBingzheng/libwuya")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xrsqbgr13g2v0ag165ryp7xrwzv41xfygzk2a3445ca98c1qpdc"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ;no test suite
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'patch-lua-includes
                      (lambda _
                        (substitute* '("wuy_cflua.h" "wuy_cflua.c")
                          (("<lua5\\.1/") "<"))
                        #t))
                    (add-after 'unpack 'add--fPIC-to-CFLAGS
                      (lambda _
                        (substitute* "Makefile"
                          (("CFLAGS[^\n]*" all)
                           (string-append all " -fPIC")))
                        #t))
                    (add-before 'build 'set-CC
                      (lambda _
                        (setenv "CC" "gcc")
                        #t))
                    (delete 'configure) ;no configure script
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (include-dir (string-append out "/include"))
                               (headers (find-files "." "\\.h$")))
                          (for-each (lambda (h)
                                      (install-file h include-dir))
                                    headers)
                          (install-file "libwuya.a" (string-append out "/lib"))
                          #t))))))
      (inputs (list lua))
      (home-page "https://github.com/WuBingzheng/libwuya/")
      (synopsis "C library implementing various data structures")
      (description "The @code{libwuya} library implements data structures such
as dictionaries, skip lists, and memory pools.")
      ;; There is no clear information as to what license this is distributed
      ;; under, but it is included (bundled) with libleak from the same author
      ;; under the GNU GPL v2 or later license, so use this here until it is
      ;; clarified (see: https://github.com/WuBingzheng/libwuya/issues/2).
      (license license:gpl2+))))

(define-public packcc
  (package
    (name "packcc")
    (version "1.8.0")
    (home-page "https://github.com/arithy/packcc")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b25p7ri1l2l20awyknljfnj7r4rg7cf2x3bljijx5q6j8rxdcsg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'chdir
                    (lambda _
                      (chdir "build/gcc")))
                  (add-before 'check 'pre-check
                    (lambda _
                      (setenv "CC" "gcc")
                      ;; The style tests are supposed to be skipped when
                      ;; uncrustify is unavailable, but a stray version
                      ;; check prevents it from working.  This can be
                      ;; removed for future versions of PackCC.
                      (substitute* "../../tests/style.d/style.bats"
                        (("^[[:blank:]]+check_uncrustify_version")
                         ""))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (install-file "release/bin/packcc"
                                      (string-append out "/bin"))
                        (install-file "../../README.md"
                                      (string-append out "/share/doc/packcc"))))))))
    (native-inputs
     (list bats))
    (synopsis "Packrat parser generator for C")
    (description
     "PackCC is a packrat parser generator for the C programming language.
Its main features are:
@itemize
@item Generates a parser in C from a grammar described in a PEG.
@item Gives your parser great efficiency by packrat parsing.
@item Supports direct and indirect left-recursive grammar rules.
@end itemize
The grammar of your parser can be described in a @acronym{PEG, Parsing
Expression Grammar}.  The PEG is a top-down parsing language, and is similar
to the regular-expression grammar.  The PEG does not require tokenization to
be a separate step, and tokenization rules can be written in the same way as
any other grammar rules.")
    (license license:expat)))

(define-public sfsexp
  (package
    (name "sfsexp")
    (version "1.4.1")
    (home-page "https://github.com/mjsottile/sfsexp")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03srnpc7p1j7ygd0wx9gybcxhqm50kjzkybh1xs75nwz97q3y2dq"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool))
    (synopsis "Symbolic expression library for C and C++")
    (description
     "sfsexp is a C/C++ library to read, parse, modify, and create symbolic
expressions.")
    (license license:lgpl2.1+)))

(define-public c-template-sort
  ;; The latest commit is used as there is no release.
  (let ((commit "24f5b8b13810ad130109c7b56daf8e99ab0fe1b8")
        (revision "0"))
  (package
    (name "c-template-sort")
    (version (git-version "0.0.0" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/swenson/sort")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q3pgw51rjq7pb6gc7zx9i48pckyl930lcab4ngxrpa5a8flq850"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("sort.h" "include/sort.h")
          ("sort_extra.h" "include/sort_extra.h"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "test"
                       (string-append "CC=" #$(cc-for-target)))))))))
    (home-page "https://github.com/swenson/sort")
    (synopsis "C implementation of many sorting algorithms")
    (description "This package provides a header-only C library,
that implements several sorting algorithms.  It is configured using
macros and supports user-defined types.")
    (license license:expat))))

(define-public sparse
  (package
    (name "sparse")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kernel.org/software/devel/sparse/dist/"
                              "sparse-"  version ".tar.xz"))
              (sha256
               (base32
                "0z1qds52144nvsdnl82r3zs3vax618v920jmffyyssmwj54qpcka"))))
    (build-system gnu-build-system)
    (inputs (list perl))
    (arguments
     '(#:make-flags `(,(string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-cgcc
                    (lambda _
                      (substitute* "cgcc"
                        (("'cc'") (string-append "'" (which "gcc") "'"))))))))
    (synopsis "Semantic C parser for Linux development")
    (description
     "Sparse is a semantic parser for C and is required for Linux development.
It provides a compiler frontend capable of parsing most of ANSI C as well as
many GCC extensions, and a collection of sample compiler backends, including a
static analyzer also called @file{sparse}.  Sparse provides a set of
annotations designed to convey semantic information about types, such as what
address space pointers point to, or what locks a function acquires or
releases.")
    (home-page "https://sparse.wiki.kernel.org/index.php/Main_Page")
    (license license:expat)))

(define-public libestr
  (package
    (name "libestr")
    (version "0.1.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/libestr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ca4rj90c0dn7kqpbcchkflxjw88a7rxcnwbr0gply4a28i01nd8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; autogen.sh calls configure at the end of the script.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     (list autoconf automake pkg-config libtool))
    (home-page "https://github.com/rsyslog/libestr")
    (synopsis "Helper functions for handling strings")
    (description
     "This C library contains some essential string manipulation functions and
more, like escaping special characters.")
    (license license:lgpl2.1+)))

(define-public libfastjson
  (package
    (name "libfastjson")
    (version "1.2304.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/libfastjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gmyzplnb4cfbg4yai0s8yi13xbaq606d5h50zwlkky712aklwss"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool))
    (home-page "https://github.com/rsyslog/libfastjson")
    (synopsis "Fast JSON library for C")
    (description
     "libfastjson is a fork from json-c aiming to provide: a small library
with essential JSON handling functions, sufficiently good JSON support (not
100% standards compliant), and very fast processing.")
    (license license:expat)))

(define-public liblogging
  (package
    (name "liblogging")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/liblogging")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1l32m0y65svf5vxsgw935jnqs6842rcqr56dmzwqvr00yfrjhjkp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; autogen.sh calls configure at the end of the script.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     (list autoconf
           automake
           pkg-config
           libtool
           ;; For rst2man.py
           python-docutils))
    (home-page "https://github.com/rsyslog/liblogging")
    (synopsis "Easy to use and lightweight signal-safe logging library")
    (description
     "Liblogging is an easy to use library for logging.  It offers an enhanced
replacement for the syslog() call, but retains its ease of use.")
    (license license:bsd-2)))

(define-public liblognorm
  (package
    (name "liblognorm")
    (version "2.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/liblognorm.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pyy1swvq6jj12aqma42jimv71z8m66zy6ydd5v19cp2azm4krml"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:parallel-tests? #false ;not supported
      #:phases
      '(modify-phases %standard-phases
         ;; These tests fail because tmp.rulebase is never created.  This
         ;; looks rather harmless.
         (add-after 'unpack 'delete-failing-tests
           (lambda _
             (substitute* "tests/Makefile.am"
               (("string_rb_simple.sh ") "")
               (("string_rb_simple_2_lines.sh ") "")))))))
    (inputs
     (list json-c libestr libfastjson))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://www.liblognorm.com")
    (synopsis "Fast samples-based log normalization library")
    (description
     "Liblognorm normalizes event data into well-defined name-value pairs and
a set of tags describing the message.")
    ;; liblognorm is very slowly transitioning to ASL2.0
    ;; See https://github.com/rsyslog/liblognorm/issues/329
    (license license:lgpl2.1+)))

(define-public unifdef
  (package
    (name "unifdef")
    (version "2.12")
    (source (origin
              (method url-fetch)
              ;; https://dotat.at/prog/unifdef/unifdef-2.12.tar.xz
              (uri (string-append "https://dotat.at/prog/" name "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "00647bp3m9n01ck6ilw6r24fk4mivmimamvm4hxp5p6wxh10zkj3"))
              (modules '((guix build utils)))
              (snippet
               '(begin (delete-file-recursively "FreeBSD")
                       (delete-file-recursively "win32")
                       #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "prefix=" %output))
       #:tests? #f))                    ;no test suite
    (native-inputs
     (list perl))
    (home-page "https://dotat.at/prog/unifdef/")
    (synopsis "Utility to selectively processes conditional C preprocessor")
    (description "The @command{unifdef} utility selectively processes
conditional C preprocessor @code{#if} and @code{#ifdef} directives.  It
removes from a file both the directives and the additional text that they
delimit, while otherwise leaving the file alone.  It can be useful for
avoiding distractions when studying code that uses @code{#ifdef} heavily for
portability.")
    (license (list license:bsd-2        ;all files except...
                   license:bsd-3))))    ;...the unidef.1 manual page

(define-public byacc
  (package
    (name "byacc")
    (version "20240109")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://invisible-mirror.net/archives/byacc/byacc-"
                   version ".tgz"))
             (sha256
              (base32
               "0il4w1vwbglayakywyghiqhcjpg1yvv5ww2p8ylz32bi05wpg2gj"))))
    (build-system gnu-build-system)
    (home-page "https://invisible-island.net/byacc/byacc.html")
    (synopsis "Berkeley Yacc LALR parser generator")
    (description
     "Berkeley Yacc is an LALR(1) parser generator.  Yacc reads the grammar
specification from a file and generates an LALR(1) parser for it.  The parsers
consist of a set of LALR(1) parsing tables and a driver routine written in the
C programming language.")
    (license license:public-domain)))

(define-public argparse
  (package
    (name "argparse")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cofyc/argparse")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zb0b2aikk4dasjzsyiyf2xn1hbld8gf8np3843zcg9wbxydd8zd"))))
    (build-system gnu-build-system)
    (arguments
     (list #:test-target "test"
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ;no configure script
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (inc (string-append out "/include"))
                          (lib (string-append out "/lib")))
                     (install-file "argparse.h" inc)
                     (install-file "libargparse.so" lib)))))))
    (synopsis "Command line arguments parsing library")
    (home-page "https://github.com/cofyc/argparse")
    (description
     "This C library provides high-level arguments parsing solutions inspired
by Python's @code{argparse} module.")
    (license license:expat)))

(define-public aws-c-common
  (package
    (name "aws-c-common")
    ;; Update only when updating aws-crt-cpp.
    (version "0.12.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1drad31f3ynvjy2xmgnidzwxya2z47n9wa4s13cpnzf2aga0jb4v"))))
    (build-system cmake-build-system)
    (arguments
     '(#:parallel-tests? #f
       #:configure-flags
       '("-DBUILD_SHARED_LIBS=ON")))
    (supported-systems '("armhf-linux" "aarch64-linux"
                         "i686-linux" "x86_64-linux"))
    (synopsis "Amazon Web Services core C library")
    (description
     "This library provides common C99 primitives, configuration, data
 structures, and error handling for the @acronym{AWS,Amazon Web Services} SDK.")
    (home-page "https://github.com/awslabs/aws-c-common")
    (license license:asl2.0)))

(define-public aws-checksums
  (package
    (name "aws-checksums")
    ;; Update only when updating aws-crt-cpp.
    (version "0.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06mpn3g882gx1ds3962a86l2r1hq7wjkdq6lsb3ixmri7sfmi26r"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (inputs
     (list aws-c-common))
    (synopsis "Amazon Web Services checksum library")
    (description
     "This library provides cross-Platform hardware accelerated CRC32c and CRC32
with fallback to efficient C99 software implementations.")
    (home-page "https://github.com/awslabs/aws-checksums")
    (license license:asl2.0)))

(define-public aws-c-event-stream
  (package
    (name "aws-c-event-stream")
    ;; Update only when updating aws-crt-cpp.
    (version "0.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pm6ggm2yv5rqfijvi0zd7xf4a0zq0m21c36vhgda5mh5wbhzf64"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (propagated-inputs
     (list aws-c-common aws-c-io aws-checksums))
    (inputs
     (list aws-c-cal s2n))
    (synopsis "Amazon Web Services client-server message format library")
    (description
     "This library is a C99 implementation for @acronym{AWS,Amazon Web Services}
event stream encoding, a binary format for bidirectional client-server
communication.")
    (home-page "https://github.com/awslabs/aws-c-event-stream")
    (license license:asl2.0)))

(define-public aws-c-io
  (package
    (name "aws-c-io")
    ;; Update only when updating aws-crt-cpp.
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07awsk4l3aj6g9k5c3xbd2i8hr9hlcs1xh1jv295yyld46qyg4ma"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common"))
             "-DENABLE_NET_TESTS=OFF")))
    (propagated-inputs
     (list aws-c-cal aws-c-common s2n))
    (synopsis "Event driven framework for implementing application protocols")
    (description "This library provides a C99 framework for constructing
event-driven, asynchronous network application protocols.")
    (home-page "https://github.com/awslabs/aws-c-io")
    (license license:asl2.0)))

(define-public aws-c-cal
  (package
    (name "aws-c-cal")
    ;; Update only when updating aws-crt-cpp.
    (version "0.8.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10n5v7s9z2kfbsq7frkwq1l4383yrq9bc3qlsxz2hrliy230828h"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (propagated-inputs
     (list aws-c-common s2n))
    (synopsis "Amazon Web Services Crypto Abstraction Layer")
    (description "This library provides a C99 wrapper for hash, HMAC, and ECC
cryptographic primitives for the @acronym{AWS,Amazon Web Services} SDK.")
    (home-page "https://github.com/awslabs/aws-c-cal")
    (license license:asl2.0)))

(define-public aws-c-sdkutils
  (package
    (name "aws-c-sdkutils")
    ;; Update only when updating aws-crt-cpp.
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09smvqgg08kr5xri5kc94v67154pxx032vdia0agx86q1pxyfl9l"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (propagated-inputs
     (list aws-c-common))
    (synopsis "Amazon Web Service utility library")
    (description "This library provides for parsing and management of profiles
for the @acronym{AWS,Amazon Web Services} SDK.")
    (home-page "https://github.com/awslabs/aws-c-sdkutils")
    (license license:asl2.0)))

(define-public pcl
  (package
    (name "pcl")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
              "http://www.xmailserver.org/pcl-" version ".tar.gz"))
       (sha256
        (base32
         "06ly65rq4iyj2p4704i215c8y4rgspwl8sxfaifmf4ahfr30bcz7"))))
    (build-system gnu-build-system)
    (home-page "http://www.xmailserver.org/libpcl.html")
    (synopsis "Portable Coroutine Library")
    (description "The @acronym{PCL, Portable Coroutine Library} implements the
low level functionality for coroutines.")
    (license license:gpl2+)))

(define-public aws-c-http
  (package
    (name "aws-c-http")
    ;; Update only when updating aws-crt-cpp.
    (version "0.9.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0anasabsgyyn56v3w1p6sq558n646ifxgqxyy0j0601wvxxmk641"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common"))
             "-DENABLE_NET_TESTS=OFF")))
    (propagated-inputs
     (list aws-c-compression aws-c-io))
    (synopsis "Amazon Web Services HTTP library")
    (description
     "This library provides a C99 implementation of the HTTP/1.1 and HTTP/2
specifications.")
    (home-page "https://github.com/awslabs/aws-c-http")
    (license license:asl2.0)))

(define-public aws-c-compression
  (package
    (name "aws-c-compression")
    ;; Update only when updating aws-crt-cpp.
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vh1rapnk6mhcdk3clihxzli7mrik89m9py3hp3h59p937xfx6l2"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (propagated-inputs
     (list aws-c-common))
    (synopsis "Amazon Web Services compression library")
    (description
     "This library provides a C99 implementation of compression algorithms,
currently limited to Huffman encoding and decoding.")
    (home-page "https://github.com/awslabs/aws-c-compression")
    (license license:asl2.0)))

(define-public aws-c-auth
  (package
    (name "aws-c-auth")
    ;; Update only when updating aws-crt-cpp.
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pfnd65q72rg4y062ajd5imzl6v4n2cgn7nimjxkb0p6shhd8c0z"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common"))
             "-DENABLE_NET_TESTS=OFF")))
    (propagated-inputs
     (list aws-c-cal aws-c-common aws-c-http aws-c-io aws-c-sdkutils))
    (synopsis "Amazon Web Services client-side authentication library")
    (description
     "This library provides a C99 implementation for AWS client-side
authentication.")
    (home-page "https://github.com/awslabs/aws-c-auth")
    (license license:asl2.0)))

(define-public aws-c-s3
  (package
    (name "aws-c-s3")
    ;; Update only when updating aws-crt-cpp.
    (version "0.7.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zh6nf3cxglsdn00m697yl90w2rc012chd9zivd6jd4i4ck6wq7q"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common"))
             "-DENABLE_NET_TESTS=OFF")))
    (propagated-inputs
     (list aws-c-auth aws-c-http aws-checksums))
    (synopsis "Amazon Web Services client library for Amazon S3")
    (description
     "This library provides a C99 client implementation of the Simple Storage
Service (S3) protocol for object storage.")
    (home-page "https://github.com/awslabs/aws-c-s3")
    (license license:asl2.0)))

(define-public aws-c-mqtt
  (package
    (name "aws-c-mqtt")
    ;; Update only when updating aws-crt-cpp.
    (version "0.10.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sa5bxnva9qwx00a49l1pppz0gnkxbwam4g6r3gw7w6265kjrswb"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (propagated-inputs
     (list aws-c-http aws-c-io))
    (synopsis "Amazon Web Services MQTT library")
    (description
     "This library provides a C99 implementation of the Message Queuing
Telemetry Transport (MQTT) publish-subscribe messaging protocol.")
    (home-page "https://github.com/awslabs/aws-c-mqtt")
    (license license:asl2.0)))

;; Note: there is another mimalloc embedded in rust-mimalloc (version 1.6.4).
(define-public mimalloc
  (package
    (name "mimalloc")
    (version "2.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/microsoft/mimalloc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pyf05snc1ib7zjjm9kspvbmynd8lmxqw72qcpz8lcyzyywmz24i"))))
    (build-system cmake-build-system)
    (arguments
     `(#:build-type "Release"
       ,@(if (target-ppc32?)
           `(#:configure-flags '("-DMI_USE_LIBATOMIC=ON"))
           '())))
    (synopsis "General purpose memory allocator")
    (description "@code{mimalloc} is a drop-in replacement for @code{malloc}.")
    (home-page "https://microsoft.github.io/mimalloc/")
    (license license:expat)))

;;; The package is named orangeduck-mpc to differentiate it from GNU mpc.
(define-public orangeduck-mpc
  ;; The last release lacks an 'install' target.
  (let ((commit "7c910e9303833c349f7432188ff77f2745254df2")
        (revision "0"))
    (package
      (name "orangeduck-mpc")
      (version (git-version "0.9.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/orangeduck/mpc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01a4vcxdnz0fbn90c9zc3jzklyqqvp9sfjpjwpq0f5r0l2pp37ad"))
                (patches
                 (search-patches "orangeduck-mpc-fix-pkg-config.patch"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                  (string-append "PREFIX=" #$output))
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'patch-Makefile
                            (lambda _
                              (substitute* "Makefile"
                                ;; Do not attempt to alter the permissions,
                                ;; otherwise 'install' would error with
                                ;; "cannot stat [...] Permission denied"
                                ;; errors.
                                (("\\s\\-m[0-9]{3}\\s")
                                 " "))))
                          (delete 'configure))))
      (home-page "https://github.com/orangeduck/mpc")
      (synopsis "Parser Combinator library for C")
      (description "@code{mpc} is a lightweight Parser Combinator library for C.
@code{mpc} can help with tasks such as:
@itemize
@item Building a new programming language
@item Building a new data format
@item Parsing an existing programming language
@item Parsing an existing data format
@item Embedding a Domain Specific Language
@item Implementing Greenspun's Tenth Rule.
@end itemize")
      (license license:bsd-2))))

;;; Factored out of the ck package so that it can be adjusted and called on
;;; the host side easily, without impacting the package definition.
(define (gnu-triplet->ck-machine target)
  (letrec-syntax
      ((matches (syntax-rules (=>)
                  ((_ (target-prefix => machine) rest ...)
                   (if (string-prefix? target-prefix target)
                       machine
                       (matches rest ...)))
                  ((_)
                   (error "unsupported target" target)))))
    ;; This basically reproduces the logic handling the
    ;; PLATFORM variable in the configure script of ck.
    (matches ("x86_64"      => "x86_64")
             ("i586"        => "x86")
             ("i686"        => "x86")
             ("aarch64"     => "aarch64")
             ("arm"         => "arm")
             ("ppc64"       => "ppc64")
             ("ppc"         => "ppc")
             ("s390x"       => "s390x")
             ("riscv64"     => "riscv64")
             ("sparc64"     => "sparcv9"))))

(define-public ck
  (package
    (name "ck")
    (version "0.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/concurrencykit/ck")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "020yzfpvymdc8lc44znlnxmxb8mvp42g4nb4p8k814klazqvwh0x"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; ck uses a custom configure script that stumbles on
            ;; '--enable-fast-install', among other things.
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (define target-machine #$(and=> (%current-target-system)
                                              gnu-triplet->ck-machine))
              (when target-machine
                ;; The configure script doesn't currently work for
                ;; cross-compiling (see:
                ;; https://github.com/concurrencykit/ck/issues/191).
                (error "ck cannot currently be cross-compiled"))
              ;; The custom configure script doesn't make cross-compilation
              ;; adjustments itself, so manually set the archiver, compiler
              ;; and linker.
              (setenv "AR" #$(ar-for-target))
              (setenv "CC" #$(cc-for-target))
              (setenv "LD" #$(ld-for-target))
              (apply invoke "./configure"
                     `(,@(if target-machine
                             (list (string-append "--profile=" target-machine))
                             '())
                       ,(string-append "--prefix=" #$output)
                       ,(string-append "--mandir=" #$output "/share/man")
                       ,(string-append "--cores="
                                       (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1")))))))))
    (home-page "https://github.com/concurrencykit/ck")
    (synopsis "C library for concurrent systems")
    (description "Concurrency Kit (@code{ck}) provides concurrency primitives,
safe memory reclamation mechanisms and non-blocking (including lock-free) data
structures designed to aid in the research, design and implementation of high
performance concurrent systems developed in C99+.")
    (license (list license:bsd-2        ;everything except...
                   license:asl2.0))))   ;src/ck_hp.c

(define-public tinydir
  (package
    (name "tinydir")
    (version "1.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cxong/tinydir")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "143n6yabznxk032gv5g2k8glf0kzicarg9cx0714zsbfmzj8lr07"))
              (patches (search-patches "tinydir-fix-cbehave-test.patch"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "tests/cbehave"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'path-cmake
            (lambda _
              (substitute* "tests/CMakeLists.txt"
                (("^include_dir.*cbehave.*")
                 (string-append "include_directories("#$cbehave "/include)"))
                (("^add_subdir.*cbeha.*") ""))))
          (add-before 'configure 'chdir
            (lambda _
              (chdir "tests")))
          (replace 'install
            (lambda _
              (install-file "../tinydir.h"
                            (string-append #$output "/include")))))))
    (native-inputs (list cbehave))
    (home-page "https://github.com/cxong/tinydir")
    (synopsis "List directories programmatically")
    (description "@code{tinydir} is a header-only C wrapper for listing
directory contents.")
    (license license:bsd-2)))

(define-public libdispatch
  (package
    (name "libdispatch")
    (version "5.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apple/swift-corelibs-libdispatch")
             (commit (string-append "swift-" version "-RELEASE"))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02k90asz0yxrcmy67mmqqc68n8f16jf445l0n7jryb7nk30bgmm6"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; Use Clang instead of GCC.
               (add-before 'configure 'prepare-build-environment
                 (lambda _
                   (setenv "AR" "llvm-ar")
                   (setenv "NM" "llvm-nm")
                   (setenv "CC" "clang")
                   (setenv "CXX" "clang++"))))))
    (native-inputs (list clang llvm))
    (home-page "https://apple.github.io/swift-corelibs-libdispatch/")
    (synopsis "Concurrent code execution on multicore hardware")
    (description
     "Grand Central Dispatch (GCD or libdispatch) implements a concurrency model
wherein program tasks are divided into work items.  These can be run
sequentially or in parallel, with optional synchronization in between, and GCD
will take care of dispatching tasks to available cores.")
    (license license:asl2.0)))

(define-public utf8-h
  ;; The latest tag is used as there is no release.
  (let ((commit "500d4ea9f4c3449e5243c088d8af8700f7189734")
        (revision "0"))
    (package
      (name "utf8-h")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sheredom/utf8.h")
                      (commit commit)))
                (file-name (git-file-name "utf8.h" version))
                (sha256
                 (base32
                  "0x9f7ivww8c7cigf4ck0hfx2bm79qgx6q4ccwzqbzkrmcrl9shfb"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'build)
            (delete 'configure)
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (with-directory-excursion "test"
                    (invoke "cmake" ".")
                    (invoke "make")))))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (install-file "utf8.h"
                              (string-append #$output "/include/utf8")))))))
      (home-page "https://github.com/sheredom/utf8.h")
      (synopsis "Single header UTF-8 string functions for C and C++")
      (description "A simple one header solution to supporting UTF-8 strings in
C and C++.  The functions it provides are like those from the C header
string.h, but with a utf8* prefix instead of the str* prefix.")
      (license license:unlicense))))

(define-public utest-h
  ;; The latest commit is used as there is no release.
  (let ((commit   "54458e248f875f1a51f0af8bec8ca6ae7761b9d1")
        (revision "0"))
    (package
      (name "utest-h")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sheredom/utest.h")
                      (commit commit)))
                (file-name (git-file-name "utest.h" version))
                (sha256
                 (base32
                  "1ikl5jwmjdw1mblqyl2kvnqwkjgaz78c1h7mjcfmzjc0d3h8kh44"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (delete 'build)
                    (delete 'configure)
                    (replace 'check
                      (lambda* (#:key tests? #:allow-other-keys)
                        (when tests?
                          (with-directory-excursion "test"
                                                    (invoke "cmake" ".")
                                                    (invoke "make")))))
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((out (assoc-ref outputs "out")))
                          (install-file "utest.h"
                                        (string-append out "/include"))))))))
      (home-page "https://www.duskborn.com/utest_h/")
      (synopsis "Single-header unit testing framework for C and C++")
      (description
       "This package provides a header-only unit testing library for C/C++.")
      (license license:unlicense))))

(define-public nsync
  (package
    (name "nsync")
    (version "1.26.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/nsync")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qg58kkcbm4zqkql8j5yvrb7fpx09qsf7j93dwqb9s1y69l70kx4"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/google/nsync")
    (synopsis "C library for synchronization primitives")
    (description
     "nsync is a C library that exports various synchronization primitives:
@enumerate
@item locks,
@item condition variables,
@item run-once initialization,
@item waitable counter (useful for barriers),
@item waitable bit (useful for cancellation, or other conditions).
@end enumerate
")
    (license license:asl2.0)))

(define-public ispc
  (package
    (name "ispc")
    (version "1.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ispc/ispc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yhcgyzjlrgs920lm0l6kygj2skanfb6qkxbdgm69r8c2xkzkaa3"))))
    (inputs (list ncurses))
    (native-inputs (list bison clang flex m4 python))
    (build-system cmake-build-system)
    (supported-systems
     '("x86_64-linux" "i686-linux" "aarch64-linux" "armhf-linux"))
    (arguments
     `(#:tests? #f
       #:configure-flags
       `(,,(string-append "-DCMAKE_C_COMPILER=" (cc-for-target))
         ,,(string-append "-DCMAKE_CXX_COMPILER=" (cxx-for-target))
         ,(string-append "-DCLANG_EXECUTABLE="
                         (assoc-ref %build-inputs "clang")
                         "/bin/clang")
         ,(string-append "-DCLANGPP_EXECUTABLE="
                         (assoc-ref %build-inputs "clang")
                         "/bin/clang++"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-curses-requirement
           (lambda _
             (substitute* "CMakeLists.txt"
               (("\\bCURSES_CURSES_LIBRARY\\b")
                "CURSES_LIBRARY"))))
         ;; Note: This works around the following issue:
         ;; <https://github.com/ispc/ispc/issues/1865>
         ;; Because GCC in Guix does not have multilib support.
         (add-before 'configure 'patch-target-archs
           (lambda _
             (substitute* "cmake/GenerateBuiltins.cmake"
               (("\\bforeach \\(bit 32 64\\)")
                ,(if (target-64bit?)
                     "foreach (bit 64)"
                     "foreach (bit 32)"))
               (("\\bforeach \\(arch .*?\\)")
                ,(if (target-x86?)
                     "foreach (arch \"x86\")"
                     "foreach (arch \"arm\")"))
               (("\\bforeach \\(os_name \"windows\" .*?\\)")
                "foreach (os_name \"linux\")")))))))
    (synopsis "Implicit SPMD Program Compiler")
    (description
     "ISPC is a compiler for a variant of the C programming language, with
extensions for single program, multiple data programming.  Under the SPMD
model, the programmer writes a program that generally appears to be a regular
serial program, though the execution model is actually that a number of
program instances execute in parallel on the hardware.")
    (home-page "https://github.com/ispc/ispc")
    (license license:bsd-3)))

(define-public pcg-c
  (let ((commit "83252d9c23df9c82ecb42210afed61a7b42402d7")
        (revision "1"))
    (package
      (name "pcg-c")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/imneme/pcg-c")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0768h0vw75a3smk39qsz1504v04a43s5w1ili1ijbixxv8gm42nf"))
                (modules '((guix build utils)))
                ;; Autogenerated files with some tests from test-high. If
                ;; 128-bit integers are not supported, the build fails, but
                ;; this is checked when building the tests.
                (snippet #~(delete-file-recursively "sample"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:test-target "test"
        #:make-flags
        #~(list
           "CC=gcc"
           (string-append "PREFIX=" #$output))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (add-after 'unpack 'disable-sample
              (lambda _
                (substitute* "Makefile"
                  ((".*cd sample.*") ""))))
            (add-after 'unpack 'set-shared-library
              (lambda _
                (substitute* '("Makefile" "src/Makefile")
                  (("\\.a") "\\.so")
                  ((".*ar .*") "\t$(CC) $(CFLAGS) -o $@ $(LDFLAGS) -shared $^")
                  ((".*ranlib.*") "")
                  ((".*CFLAGS \\+=.*O3.*" orig)
                   (string-append orig "CFLAGS += -fPIC\n")))))
            (add-before 'install 'make-dirs
              (lambda _
                (mkdir-p (string-append #$output "/lib"))
                (mkdir-p (string-append #$output "/include")))))))
      (home-page "https://www.pcg-random.org")
      (synopsis "C implementation of the PCG random generators")
      (description "The Permuted Congruential Generator (PCG) extends the
Linear Congruential Generator (LCG) with a permutation function to increase
output randomness while retaining speed, simplicity, and conciseness.")
      (license (list license:expat license:asl2.0))))) ; dual licensed

(define-public yyjson
  (package
    (name "yyjson")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ibireme/yyjson")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kmzgs24v0rxlibg4qwlm6yplzs96pgxb1gyviijhkra9z7lx7ws"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON"
                                "-DYYJSON_BUILD_TESTS=ON")))
    (home-page "https://github.com/ibireme/yyjson")
    (synopsis "C implementation of JSON RFC 8259")
    (description
     "This package provides an implementation of JSON in ANSI C as specified
in RFC 8259.
Features:
@itemize
@item Fast: can read or write gigabytes per second JSON data on modern CPUs.
@item Portable: complies with ANSI C (C89) for cross-platform compatibility.
@item Strict: complies with
@url{https://datatracker.ietf.org/doc/html/rfc8259,RFC 8259} JSON standard,
ensuring strict number format and UTF-8 validation.
@item Extendable: offers options to allow comments, trailing commas, NaN/Inf,
 and custom memory allocator.
@item Accuracy: can accurately read and write @code{int64}, @code{uint64}, and
@code{double} numbers.
@item Flexible: supports unlimited JSON nesting levels, @code{\\u0000}
characters, and non null-terminated strings.
@item Manipulation: supports querying and modifying using JSON Pointer,
JSON Patch and JSON Merge Patch.
@item Developer-Friendly: easy integration with only one @code{.h} and one
@code{.c} file.
@end itemize")
    (license license:expat)))
