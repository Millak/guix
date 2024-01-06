;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2018 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018, 2019, 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages multiprecision)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages texinfo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public gmp
  (package
    (name "gmp")
    (version "6.3.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://gnu/gmp/gmp-"
                              version ".tar.xz"))
              (sha256
               (base32
                "1648ad1mr7c1r8lkkqshrv1jfjgfdb30plsadxhni7mq041bihm3"))
              (patches (search-patches "gmp-faulty-test.patch"))))
    (build-system gnu-build-system)
    (native-inputs (list m4))
    (outputs '("out" "debug"))
    (arguments
     (list
      #:parallel-tests? #f              ; mpz/reuse fails otherwise
      #:configure-flags
      #~(list
         ;; Build a "fat binary", with routines for several sub-architectures.
         "--enable-fat"
         "--enable-cxx"
         #$@(cond
             ((target-mingw?)
              ;; Static and shared cannot be built in one go: they produce
              ;; different headers.  We need shared.
              '("--disable-static"
                "--enable-shared"))
             ((target-x32?)
              `("ABI=x32"))
             (else '())))))
    (synopsis "Multiple-precision arithmetic library")
    (description
     "The @acronym{GMP, the GNU Multiple Precision Arithmetic} library performs
arbitrary-precision arithmetic on signed integers, rational numbers and floating
point numbers.  The precision is only limited by the available memory.
The library is highly optimized, with a design focus on execution speed.
It is aimed at use in, for example, cryptography and computational algebra.")
    (license lgpl3+)
    (home-page "https://gmplib.org/")))

(define-public gmp-6.0
  ;; We keep this one around to bootstrap GCC, to work around a compilation
  ;; issue on ARM.  See
  ;; <https://gmplib.org/list-archives/gmp-bugs/2015-December/003848.html>.
  (package
    (inherit gmp)
    (version "6.0.0a")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gmp/gmp-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0r5pp27cy7ch3dg5v0rsny8bib1zfvrza6027g2mp5f6v8pd6mli"))
              (patches (search-patches "gmp-arm-asm-nothumb.patch"
                                       "gmp-faulty-test.patch"))))))

(define-public mpfr
  (package
   (name "mpfr")
   (version "4.2.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/mpfr/mpfr-" version
                                ".tar.xz"))
            (sha256 (base32
                     "1cnb3y7y351qg6r7ynwsgaykm7l2a8zg2nlljs4rf9k778shfy17"))))
   (arguments
    (list
     #:phases (if (system-hurd?)
                  #~(modify-phases %standard-phases
                      (add-after 'unpack 'skip-tests
                        (lambda _
                          (substitute*
                              "tests/tsprintf.c"
                            (("(^| )main *\\(.*" all)
                             (string-append all "{\n  exit (77);//"))))))
                  #~%standard-phases)))
   (build-system gnu-build-system)
   (outputs '("out" "debug"))
   (propagated-inputs (list gmp))            ; <mpfr.h> refers to <gmp.h>
   (synopsis "C library for arbitrary-precision floating-point arithmetic")
   (description
    "GNU@tie{}@acronym{MPFR, Multiple Precision Floating-Point Reliably} is a C
library for performing multiple-precision, floating-point computations with
correct rounding.")
   (license lgpl3+)
   (home-page "https://www.mpfr.org/")))

(define-public mpc
  (package
   (name "mpc")
   (version "1.3.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/mpc/mpc-" version ".tar.gz"))
            (sha256
              (base32
                "1f2rqz0hdrrhx4y1i5f8pv6yv08a876k1dqcm9s2p26gyn928r5b"))))
   (build-system gnu-build-system)
   (outputs '("out" "debug"))
   (propagated-inputs (list gmp ; <mpc.h> refers to both
                            mpfr))
   (synopsis "C library for arbitrary-precision complex arithmetic")
   (description
    "GNU@tie{}@acronym{MPC, Multiple Precision Complex library} is a C library
for performing arithmetic on complex numbers.  It supports arbitrarily high
precision and correctly rounds the results.")
   (license lgpl3+)
   (home-page "https://www.multiprecision.org/mpc/")))

(define-public mpfi
  ;; The last release, 1.5.4, lacks source files such as div_ext.c and others
  ;; (see: https://gitlab.inria.fr/mpfi/mpfi/-/issues/21721).  Use the latest
  ;; commit until a new release is made.
  (let ((commit "b9825348f1e723ab0988105599b34f1f9cd5ff3e")
        (revision "1"))
    (package
      (name "mpfi")
      (version (git-version "1.5.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.inria.fr/mpfi/mpfi")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "141pa2ym1azka25bwkx2w1mf6wdcn155ncc45dr5bj5dycw2hj0m"))))
      (build-system gnu-build-system)
      (arguments
       (list #:configure-flags #~(list "--enable-static=no")
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'chdir
                            (lambda _
                              (chdir "mpfi")))
                          (add-after 'unpack 'disable-problematic-tests
                            (lambda _
                              ;; XXX: The tdiv_ext and trec_sqrt tests
                              ;; segfaults (not reported upstream; accounts on
                              ;; their gitlab apparently require to be
                              ;; manually created).
                              (setenv "XFAIL_TESTS" "tdiv_ext trec_sqrt"))))))
      (native-inputs (list automake autoconf libtool texinfo))
      (propagated-inputs
       (list gmp mpfr))                 ;<mpfi.h> refers to both
      (home-page "https://gitlab.inria.fr/mpfi/mpfi")
      (synopsis "C library for arbitrary-precision interval arithmetic")
      (description
       "@acronym{MPFI, Multiple Precision Floating-point Interval} is a portable C
library for arbitrary-precision interval arithmetic, with intervals represented
using MPFR reliable floating-point numbers.  It's based on the @acronym{GMP, GNU
Multiple Precision Arithmetic} and GNU@tie{}@acronym{MPFR, Multiple Precision
Floating-Point Reliably} libraries.

The purpose of arbitrary-precision interval arithmetic is to get results that
are both guaranteed, thanks to interval computation, and accurate, thanks to
multiple-precision arithmetic.")
      (license lgpl2.1+))))

(define-public irram
  (package
    (name "irram")
    (version "2013_01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://irram.uni-trier.de/irram-files/iRRAM_"
                           version ".tar.bz2"))
       (sha256
        (base32 "1cdmvb4hsa161rfdjqyhd9sb3fcr43p3a6nsj7cb4kn9f94qmjpj"))))
    (build-system gnu-build-system)
    (propagated-inputs (list gmp ; <mpfi.h> refers to both
                             mpfr))
    (arguments
     `(#:parallel-build? #f))
    (synopsis "C++ package for real arithmetic based on the Real-RAM concept")
    (description
     "@dfn{iRRAM} is a C++ package for error-free real arithmetic based on
the concept of a Real-RAM.  Its capabilities range from ordinary arithmetic
over trigonometric functions to linear algebra and differential
equations.  A program using iRRAM is coded in ordinary C++, but may use a
special class that behaves like real numbers without any
error.  Additionally, iRRAM uses the concept of multi-valued functions.")
    (license lgpl2.0+)
    (home-page "http://irram.uni-trier.de/")))

(define-public qd
  (package
    (name "qd")
    (version "2.3.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://crd-legacy.lbl.gov/~dhbailey/mpdist/qd-"
                           version ".tar.gz"))
       (sha256
        (base32 "0f0fwsyh23p5zfyrj6n16q7b56g213fnhdc8dbl22r3fz1rnqyx4"))))
    (build-system gnu-build-system)
    (native-inputs (list automake autoconf gfortran))
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-enable_fma" ;weird :/
              "--disable-static"
              "--enable-shared"
              #$@(if (target-aarch64?)
                     ;; XXX: The qd_test test fails numerical
                     ;; accuracy checks for 'dd_real::exp()' on
                     ;; aarch64 with GCC 5.4 at -O2.  Disabling
                     ;; expensive optimizations lets it pass.
                     '("CXXFLAGS=-O3 -fno-expensive-optimizations")
                     '("CXXFLAGS=-O3")))))
    (home-page "https://www.davidhbailey.com/dhbsoftware/")
    (synopsis "Double-double and quad-double library")
    (description "This package supports both a double-double
datatype (approx. 32 decimal digits) and a quad-double datatype (approx. 64
decimal digits).  The computational library is written in C++.  Both C++ and
Fortran-90 high-level language interfaces are provided to permit one to
convert an existing C++ or Fortran-90 program to use the library with only
minor changes to the source code.  In most cases only a few type statements
and (for Fortran-90 programs) read/write statements need to be changed.  PSLQ
and numerical quadrature programs are included.")
    (license bsd-3)))

(define-public cln
  (package
    (name "cln")
    (version "1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ginac.de/CLN/cln-"
                           version ".tar.bz2"))
       (sha256
        (base32 "0jlq9l4hphk7qqlgqj9ihjp4m3rwjbhk6q4v00lsbgbri07574pl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-static")))
    (inputs
     (list gmp))
    (home-page "https://www.ginac.de/CLN/")
    (synopsis "Library for arbitrary precision computations")
    (description "CLN is a C++ library for efficient computations with all
kinds of numbers in arbitrary precision.  It provides a rich set of number
classes and elementary, logical and transcendental functions.")
    (license gpl2+)))

(define-public tomsfastmath
  (package
    (name "tomsfastmath")
    (version "0.13.1")
    (synopsis "Large integer arithmetic library")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libtom/tomsfastmath/"
                                  "releases/download/v" version "/"
                                  "tfm-" version ".tar.xz"))
              (sha256
               (base32
                "0f0pmiaskh89sp0q933pafxb914shpaj5ad8sb5rzk1wv8d7mja7"))))
    (build-system gnu-build-system)
    (native-inputs
     (list libtool))
    (arguments
     `(#:make-flags (list "-f" "makefile.shared"
                          (string-append "LIBPATH=" %output "/lib")
                          (string-append "INCPATH=" %output "/include")
                          "GROUP=root" "USER=root"
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configuration
         (replace 'check
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make"
                    "stest" "test_standalone"
                    make-flags)
             (invoke "./stest")
             (invoke "./test")))
         (add-before 'install 'install-nogroup
           (lambda _
             ;; Let permissions inherit from the current process.
             (substitute* "makefile.shared"
               (("-g \\$\\(GROUP\\) -o \\$\\(USER\\)") ""))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((docdir (string-append (assoc-ref outputs "out")
                                          "/share/doc/tomsfastmath")))
               (install-file "doc/tfm.pdf" docdir)
               #t)))
         (add-after 'install 'install-pc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pc-dir (string-append out "/lib/pkgconfig")))
               (call-with-output-file "tomsfastmath.pc"
                 (lambda (port)
                   (format port "~
Name: TomsFastMath
Description: ~a
Version: ~a
Libs: -L~a/lib -ltfm~%"
                           ,synopsis ,version out)))
               (install-file "tomsfastmath.pc" pc-dir)
               #t))))))
    (home-page "https://www.libtom.net/TomsFastMath/")
    (description "TomsFastMath is a large integer library written in portable
ISO C.  It is a port of LibTomMath with optional support for inline assembler
multiplies.")
    (license public-domain)))

(define-public libtomcrypt
  (package
    (name "libtomcrypt")
    (version "1.18.2")
    (outputs '("out" "static"))
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/libtom/libtomcrypt"
                            "/releases/download/v" version
                            "/crypt-" version ".tar.xz"))
        (sha256
         (base32
          "113vfrgapyv72lalhd3nkw7jnks8az0gcb5wqn9hj19nhcxlrbcn"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; Patch CVE-2019-17362
            ;; https://github.com/libtom/libtomcrypt/commit/25c26a3b7a9ad8192ccc923e15cf62bf0108ef94
            (substitute* "src/pk/asn1/der/utf8/der_decode_utf8_string.c"
              (("z > 4") "z == 1 || z > 4"))
            #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure
         (add-after 'unpack 'prepare-build
           (lambda _
             ;; We want the shared library by default so force it to be the
             ;; default makefile target.
             (delete-file "makefile")
             (symlink "makefile.shared" "makefile")
             ;; We link to libtommath, so we need to add it to the pc file
             (substitute* "libtomcrypt.pc.in"
               (("-ltomcrypt") "-ltomcrypt -ltommath"))
             #t))
         (add-after 'build 'build-static
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "-f" "makefile.unix" make-flags)))
         (replace 'check
           (lambda* (#:key test-target make-flags #:allow-other-keys)
             (apply invoke "make" "-f" "makefile.unix" test-target make-flags)
             (invoke "./test")))
         (add-after 'install 'install-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (static (assoc-ref outputs "static")))
               (mkdir-p (string-append static "/lib"))
               (mkdir-p (string-append static "/include"))
               (rename-file (string-append out "/lib/libtomcrypt.a")
                            (string-append static "/lib/libtomcrypt.a"))
               (copy-recursively (string-append out "/include")
                                 (string-append static "/include"))
               #t))))
       #:test-target "test"
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "CFLAGS += -DLTM_DESC -DUSE_LTM"
             (string-append "EXTRALIBS="
                            (search-input-file %build-inputs
                                               "/lib/libtommath.so"))
             (string-append "CC=" ,(cc-for-target)))))
    (native-inputs
     (list libtool))
    (inputs
     (list libtommath))
    (home-page "https://www.libtom.net/LibTomCrypt/")
    (synopsis "Cryptographic toolkit")
    (description "LibTomCrypt is a fairly comprehensive, modular and portable
cryptographic toolkit that provides developers with a vast array of well known
published block ciphers, one-way hash functions, chaining modes, pseudo-random
number generators, public key cryptography and a plethora of other routines.")
    (properties `((lint-hidden-cve . ("CVE-2019-17362"))))
    (license unlicense)))

(define-public libtommath
  (package
    (name "libtommath")
    (version "1.3.0")
    (outputs '("out" "static"))
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/libtom/libtommath/releases/"
                            "download/v" version "/ltm-" version ".tar.xz"))
        (sha256
         (base32
          "024xzb66abhla7kjks07ga05id9lq007cq3kxc41769m6kcp4qi9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure
         (add-after 'unpack 'prepare-build
           (lambda _
             ;; We want the shared library by default so force it to be the
             ;; default makefile target.
             (delete-file "makefile")
             (symlink "makefile.shared" "makefile")))
         (add-after 'install 'remove-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/lib/libtommath.a"))))
         (replace 'check
           (lambda* (#:key tests? test-target make-flags #:allow-other-keys)
             (when tests?
               (apply invoke "make" test-target make-flags)
               (invoke "sh" "test"))))
         (add-after 'install 'install-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "-f" "makefile.unix" "install"
                     (string-append "PREFIX=" (assoc-ref outputs "static"))
                     (string-append "CC=" (which "gcc"))))))
       #:test-target "test"
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")))
    (native-inputs
     (list libtool))
    (home-page "https://www.libtom.net/LibTomMath/")
    (synopsis "Portable number theoretic multiple-precision integer library")
    (description "LibTomMath is a portable number theoretic multiple-precision
integer library written entirely in C.  It's designed to provide an API that is
simple to work with that provides fairly efficient routines that build out of
the box without configuration.")
    (properties `((upstream-name . "ltm")
                  (lint-hidden-cve . ("CVE-2023-36328"))))
    (license unlicense)))
