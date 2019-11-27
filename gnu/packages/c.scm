;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml))

(define-public tcc
  (package
    (name "tcc")                                  ;aka. "tinycc"
    (version "0.9.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/tinycc/tcc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "177bdhwzrnqgyrdv1dwvpd04fcxj68s5pm1dzwny6359ziway8yy"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)
                     ("texinfo" ,texinfo)))
    (arguments
     `(#:configure-flags (list (string-append "--elfinterp="
                                              (assoc-ref %build-inputs "libc")
                                              ,(glibc-dynamic-linker))
                               (string-append "--crtprefix="
                                              (assoc-ref %build-inputs "libc")
                                              "/lib")
                               (string-append "--sysincludepaths="
                                              (assoc-ref %build-inputs "libc")
                                              "/include:"
                                              (assoc-ref %build-inputs
                                                         "kernel-headers")
                                              "/include:{B}/include")
                               (string-append "--libpaths="
                                              (assoc-ref %build-inputs "libc")
                                              "/lib")
                               ,@(if (string-prefix? "armhf-linux"
                                                     (or (%current-target-system)
                                                         (%current-system)))
                                     `("--triplet=arm-linux-gnueabihf")
                                     '()))
       #:test-target "test"))
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
    (license license:lgpl2.1+)))

(define-public tcc-wrapper
  (package
    (inherit tcc)
    (name "tcc-wrapper")
    (build-system trivial-build-system)
    (native-inputs '())
    (inputs `(("tcc" ,tcc)
              ("guile" ,guile-2.2)))

    ;; By default TCC does not honor any search path environment variable.
    ;; This wrapper adds them.
    ;;
    ;; FIXME: TCC includes its own linker so our 'ld-wrapper' hack to set the
    ;; RUNPATH is ineffective here.  We should modify TCC itself.
    (native-search-paths
     (list (search-path-specification
            (variable "TCC_CPATH")
            (files '("include")))
           (search-path-specification
            (variable "TCC_LIBRARY_PATH")
            (files '("lib" "lib64")))))

    (arguments
     '(#:builder
       (let* ((out   (assoc-ref %outputs "out"))
              (bin   (string-append out "/bin"))
              (tcc   (assoc-ref %build-inputs "tcc"))
              (guile (assoc-ref %build-inputs "guile")))
         (mkdir out)
         (mkdir bin)
         (call-with-output-file (string-append bin "/cc")
           (lambda (port)
             (format port "#!~a/bin/guile --no-auto-compile~%!#~%" guile)
             (write
              `(begin
                 (use-modules (ice-9 match)
                              (srfi srfi-26))

                 (define (split path)
                   (string-tokenize path (char-set-complement
                                          (char-set #\:))))

                 (apply execl ,(string-append tcc "/bin/tcc")
                        ,(string-append tcc "/bin/tcc") ;argv[0]
                        (append (cdr (command-line))
                                (match (getenv "TCC_CPATH")
                                  (#f '())
                                  (str
                                   (map (cut string-append "-I" <>)
                                        (split str))))
                                (match (getenv "TCC_LIBRARY_PATH")
                                  (#f '())
                                  (str
                                   (map (cut string-append "-L" <>)
                                        (split str)))))))
              port)
             (chmod port #o777)))
         #t)))
    (synopsis "Wrapper providing the 'cc' command for TCC")))

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
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "make" "-C" "cc/cpp" "test") #t)))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (synopsis "Portable C compiler")
    (description
     "PCC is a portable C compiler.  The project goal is to write a C99
compiler while still keeping it small, simple, fast and understandable.")
    (home-page "http://pcc.ludd.ltu.se")
    (supported-systems (delete "aarch64-linux" %supported-systems))
    ;; PCC incorporates code under various BSD licenses; for new code bsd-2 is
    ;; preferred.  See http://pcc.ludd.ltu.se/licenses/ for more details.
    (license (list license:bsd-2 license:bsd-3))))

(define-public libbytesize
  (package
    (name "libbytesize")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/storaged-project/libbytesize/releases/"
                    "download/" version "/libbytesize-" version ".tar.gz"))
              (sha256
               (base32
                "1bpz9cpb8s47kqplkkiz6ryfahas2ma95g9rh2axnfjp6w1d9ixc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
     `(("mpfr" ,mpfr)
       ("pcre2" ,pcre2)))
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
    (version "2.2.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.unidata.ucar.edu/pub/udunits/"
                                  "udunits-" version ".tar.gz"))
              (sha256
               (base32
                "0v9mqw4drnkzkm57331ail6yvs9485jmi37s40lhvmf7r5lli3rn"))))
    (build-system gnu-build-system)
    (inputs
     `(("expat" ,expat)))
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
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("check" ,check)))
    (synopsis "Thin wrapper over POSIX syscalls")
    (description
     "The purpose of libfixposix is to offer replacements for parts of POSIX
whose behaviour is inconsistent across *NIX flavours.")
    (license license:boost1.0)))

(define-public libhx
  (package
    (name "libhx")
    (version "3.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/libhx/libHX/"
                           "libHX-" version ".tar.xz"))
       (sha256
        (base32
         "0i8v2464p830c15myknvvs6bhxaf663lrqgga95l94ygfynkw6x5"))))
    (build-system gnu-build-system)
    (home-page "http://libhx.sourceforge.net")
    (synopsis "C library with common data structures and functions")
    (description
     "This is a C library (with some C++ bindings available) that provides data
structures and functions commonly needed, such as maps, deques, linked lists,
string formatting and autoresizing, option and config file parsing, type
checking casts and more.")
    (license license:lgpl2.1+)))
