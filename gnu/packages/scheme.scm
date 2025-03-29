;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2022 Robby Zambito <contact@robbyzambito.me>
;;; Copyright © 2023, 2024 Andrew Whatson <whatson@tailcall.au>
;;; Copyright © 2023, 2024 Juliana Sims <juli@incana.org>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Skylar Hill <stellarskylark@posteo.net>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Adam Faiz <adam.faiz@disroot.org>
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

(define-module (gnu packages scheme)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:select (gpl2 gpl2+ lgpl2.0+ lgpl2.1 lgpl2.1+ lgpl3+ asl2.0
                          bsd-0 bsd-3 cc-by-sa4.0 non-copyleft expat
                          public-domain))
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libphidget)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define (mit-scheme-source-directory system version)
  (string-append "mit-scheme-"
                 (if (or (string-prefix? "x86_64" system)
                         (string-prefix? "i686" system))
                     ""
                     "c-")
                 version))

(define-public mit-scheme
  (package
    (name "mit-scheme")
    (version "11.2")
    (source #f)                                   ; see below
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "xzvf"
                     (assoc-ref inputs "source"))
             (chdir ,(mit-scheme-source-directory (%current-system)
                                                  version))
             ;; Delete these dangling symlinks since they break
             ;; `patch-shebangs'.
             (for-each delete-file
                       (find-files "src/compiler" "^make\\."))
             (chdir "src")
             #t))
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (setenv "CONFIG_SHELL" (which "sh"))
             (substitute* '("../tests/ffi/autogen.sh"
                            "../tests/ffi/autobuild.sh"
                            "../tests/ffi/test-ffi.sh"
                            "../tests/runtime/test-process.scm"
                            "runtime/unxprm.scm")
               (("/bin/sh") (which "sh"))
               (("\\./autogen\\.sh")
                (string-append (which "sh") " autogen.sh"))
               (("\\./configure")
                (string-append (which "sh") " configure")))
             #t))
         ;; disable array-parameter warnings that become errors while
         ;; compiling microcode target
         (add-before 'configure 'set-flags
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CFLAGS" "-Wno-array-parameter")
             (setenv "CPPFLAGS" "-Wno-array-parameter")))
         (replace 'build
           (lambda* (#:key system outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (if (or (string-prefix? "x86_64" system)
                       (string-prefix? "i686" system))
                   (invoke "make" "compile-microcode")
                   (invoke "./etc/make-liarc.sh"
                           (string-append "--prefix=" out)))
               #t)))
         (add-after 'configure 'configure-doc
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (with-directory-excursion "../doc"
               (let* ((out (assoc-ref outputs "out"))
                      (bash (assoc-ref inputs "bash"))
                      (bin/sh (string-append bash "/bin/sh")))
                 (invoke bin/sh "./configure"
                         (string-append "--prefix=" out)
                         (string-append "SHELL=" bin/sh))
                 #t))))
         (add-after 'build 'build-doc
           (lambda* _
             (with-directory-excursion "../doc"
               (invoke "make"))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (old-doc-dir (string-append out "/share/doc"))
                    (new-doc/mit-scheme-dir
                     (string-append doc "/share/doc/" ,name "-" ,version)))
               (with-directory-excursion "../doc"
                 (for-each (lambda (target)
                             (invoke "make" target))
                           '("install-info-gz" "install-man"
                             "install-html" "install-pdf")))
               (mkdir-p new-doc/mit-scheme-dir)
               (copy-recursively
                (string-append old-doc-dir "/" ,name)
                new-doc/mit-scheme-dir)
               (delete-file-recursively old-doc-dir)
               #t))))))
    (native-inputs
     `(;; Autoconf, Automake, and Libtool are necessary for the FFI tests.
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("texlive" ,(texlive-local-tree
                    (list texlive-epsf
                          texlive-texinfo)))
       ("texinfo" ,texinfo)
       ("ghostscript" ,ghostscript)
       ("m4" ,m4)))
    (inputs
     `(("libx11" ,libx11)
       ("ncurses" ,ncurses)

       ("source"

        ;; MIT/GNU Scheme is not bootstrappable, so it's recommended to
        ;; compile from the architecture-specific tarballs, which contain
        ;; pre-built binaries.  It leads to more efficient code than when
        ;; building the tarball that contains generated C code instead of
        ;; those binaries.
        ,(origin
          (method url-fetch)
          (uri (string-append "mirror://gnu/mit-scheme/stable.pkg/"
                              version "/mit-scheme-"
                              (match (%current-system)
                                ("x86_64-linux"
                                 (string-append version "-x86-64"))
                                ("aarch64-linux"
                                 (string-append version "-aarch64le"))
                                (_
                                 (string-append "c-" version)))
                              ".tar.gz"))
          (sha256
           (match (%current-system)
             ("x86_64-linux"
              (base32
               "17822hs9y07vcviv2af17p3va7qh79dird49nj50bwi9rz64ia3w"))
             ("aarch64-linux"
              (base32
               "11maixldk20wqb5js5p4imq221zz9nf27649v9pqkdf8fv7rnrs9"))
             (_
              (base32
               "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))))))

    ;; Fails to build on MIPS, see <http://bugs.gnu.org/18221>.
    ;; Also, the portable C version of MIT/GNU Scheme did not work in time for
    ;; release in version 10.1.
    (supported-systems '("x86_64-linux" "i686-linux"))

    (home-page "https://www.gnu.org/software/mit-scheme/")
    (synopsis "Scheme implementation with integrated editor and debugger")
    (description
     "GNU/MIT Scheme is an implementation of the Scheme programming
language.  It provides an interpreter, a compiler and a debugger.  It also
features an integrated Emacs-like editor and a large runtime library.")
    (license gpl2+)
    (properties '((ftp-directory . "/gnu/mit-scheme/stable.pkg")))))

(define-public bigloo
  ;; Upstream modifies source tarballs in place, making significant changes
  ;; long after the initial publication: <https://bugs.gnu.org/33525>.
  (let ((upstream-version "4.3g"))
    (package
      (name "bigloo")
      (version "4.3g")
      (source (origin
                (method url-fetch)
                (uri (string-append "ftp://ftp-sop.inria.fr/indes/fp/Bigloo/bigloo"
                                    upstream-version ".tar.gz"))
                (sha256
                 (base32
                  "07305c134v7s1nz44igwsyqpb9qqia5zyng1q2qj60sskw3nbd67"))
                ;; Remove bundled libraries.
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (for-each delete-file-recursively
                              '("gc" "gmp" "libuv" "libunistring" "pcre"))
                    #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-gmp-detection
             (lambda _
               (substitute* "configure"
                 (("gmpversion=`\\$autoconf gmp --lib=\\$gmplib`")
                  "gmpversion=`\\$autoconf gmp --lib=\"\\$gmplib\"`"))))
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)

               (substitute* "configure"
                 (("^shell=.*$")
                  (string-append "shell=" (which "bash") "\n"))
                 (("`date`") "0"))
               (substitute* "autoconf/runtest.in"
                 ((", @DATE@") ""))
               (substitute* "autoconf/osversion"
                 (("^version.*$") "version=\"\"\n"))
               (substitute* "comptime/Makefile"
                 (("\\$\\(LDCOMPLIBS\\)")
                  "$(LDCOMPLIBS) $(LDFLAGS)"))

               ;; The `configure' script doesn't understand options
               ;; of those of Autoconf.
               (let ((out (assoc-ref outputs "out")))
                 (invoke "./configure"
                         (string-append "--prefix=" out)
                                                  ; use system libraries
                         "--customgc=no"
                         "--enable-gmp"
                         "--customgmp=no"
                         "--customunistring=no"
                         "--customlibuv=no"
                         (string-append"--mv=" (which "mv"))
                         (string-append "--rm=" (which "rm"))
                         "--cflags=-fPIC"
                         (string-append "--ldflags=-Wl,-rpath="
                                        (assoc-ref outputs "out")
                                        "/lib/bigloo/" ,upstream-version)
                         (string-append "--lispdir=" out
                                        "/share/emacs/site-lisp")
                         "--sharedbde=yes"
                         "--sharedcompiler=yes"
                         "--disable-patch"))))
           (add-after 'install 'install-emacs-modes
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (dir (string-append out "/share/emacs/site-lisp")))
                 (invoke "make" "-C" "bmacs" "all" "install"
                         (string-append "EMACSBRAND=emacs25")
                         (string-append "EMACSDIR=" dir))))))))
      (inputs
       (list emacs ;UDE needs the X version of Emacs
             libgc
             libunistring
             libuv
             openssl
             sqlite
             ;; Optional APIs for which Bigloo has bindings.
             avahi
             libphidget
             pcre))
      (native-inputs
       (list pkg-config))
      (propagated-inputs
       (list gmp))                            ; bigloo.h refers to gmp.h
      (home-page "https://www-sop.inria.fr/indes/fp/Bigloo/")
      (synopsis "Efficient Scheme compiler")
      (description
       "Bigloo is a Scheme implementation devoted to one goal: enabling Scheme
based programming style where C(++) is usually required.  Bigloo attempts to
make Scheme practical by offering features usually presented by traditional
programming languages but not offered by Scheme and functional programming.
Bigloo compiles Scheme modules.  It delivers small and fast stand alone binary
executables.  Bigloo enables full connections between Scheme and C programs
and between Scheme and Java programs.")
      (license gpl2+))))

(define-public hop
  (package
    (name "hop")
    (version "3.2.0-pre1")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp-sop.inria.fr/indes/fp/Hop/hop-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0jf418d0s9imv98s6qrpjxr1mdaxr37knh5qyfl5y4a9cc41mlg5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags '("BIGLOO=bigloo")
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("tools/Makefile"
                              "test/hopjs/TEST.in")
                 (("/bin/rm") (which "rm")))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       "--hostcc=gcc"
                       (string-append "--blflags="
                                      ;; user flags completely override useful
                                      ;; default flags, so repeat them here.
                                      "-copt \\$(CPICFLAGS) "
                                      "-L \\$(BUILDLIBDIR) "
                                      "-ldopt -Wl,-rpath," out "/lib"))))))))
    (inputs (list avahi
                  bigloo
                  libgc
                  libunistring
                  libuv
                  pcre
                  sqlite
                  which))
    (home-page "http://hop.inria.fr/")
    (synopsis "Multi-tier programming language for the Web 2.0")
    (description
     "HOP is a multi-tier programming language for the Web 2.0 and the
so-called diffuse Web.  It is designed for programming interactive web
applications in many fields such as multimedia (web galleries, music players,
...), ubiquitous and house automation (SmartPhones, personal appliance),
mashups, office (web agendas, mail clients, ...), etc.")
    (license gpl2+)))

(define-public scheme48
  (package
    (name "scheme48")
    (version "1.9.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://s48.org/" version
                                 "/scheme48-" version ".tgz"))
             (sha256
              (base32
               "1x4xfm3lyz2piqcw1h01vbs1iq89zq7wrsfjgh3fxnlm1slj2jcw"))
             (patches (search-patches "scheme48-tests.patch"))))
    (build-system gnu-build-system)
    (home-page "https://s48.org/")
    (synopsis "Scheme implementation using a bytecode interpreter")
    (description
     "Scheme 48 is an implementation of Scheme based on a byte-code
interpreter and is designed to be used as a testbed for experiments in
implementation techniques and as an expository tool.")

    ;; Most files are BSD-3; see COPYING for the few exceptions.
    (license bsd-3)))

(define-public scheme48-prescheme
  (package
    (inherit scheme48)
    (name "scheme48-prescheme")
    (arguments
     (list
      #:tests? #f ; tests only cover scheme48
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (srfi srfi-1))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'patch-prescheme-version
            (lambda _
              ;; Ensure the Pre-Scheme version matches the package version
              (call-with-output-file "ps-compiler/minor-version-number"
                (lambda (port)
                  (let* ((version #$(package-version this-package))
                         (vparts (string-split version #\.))
                         (vminor (string-join (drop vparts 1) ".")))
                    (write vminor port))))))
          (add-after 'configure 'patch-prescheme-headers
            (lambda _
              ;; Rename "io.h" to play nicely with others
              (copy-file "c/io.h" "c/prescheme-io.h")
              (substitute* "c/prescheme.h"
                (("^#include \"io\\.h\"")
                 "#include \"prescheme-io.h\""))))
          (add-after 'configure 'generate-pkg-config
            (lambda _
              ;; Generate a pkg-config file
              (call-with-output-file "prescheme.pc"
                (lambda (port)
                  (let ((s48-version #$(package-version scheme48))
                        (version #$(package-version this-package)))
                    (format port (string-join
                                  '("prefix=~a"
                                    "exec_prefix=${prefix}"
                                    "libdir=${prefix}/lib/scheme48-~a"
                                    "includedir=${prefix}/include"
                                    ""
                                    "Name: Pre-Scheme (Scheme 48)"
                                    "Description: Pre-Scheme C runtime"
                                    "Version: ~a"
                                    "Libs: -L${libdir} -lprescheme"
                                    "Cflags: -I${includedir}")
                                  "\n" 'suffix)
                            #$output s48-version version))))))
          (add-after 'configure 'generate-prescheme-wrapper
            (lambda _
              ;; Generate a wrapper to load and run ps-compiler.image
              (call-with-output-file "prescheme"
                (lambda (port)
                  (let ((s48-version #$(package-version scheme48)))
                    (format port (string-join
                                  '("#!/bin/sh"
                                    "scheme48=~a/lib/scheme48-~a/scheme48vm"
                                    "prescheme=~a/lib/scheme48-~a/prescheme.image"
                                    "exec ${scheme48} -i ${prescheme} \"$@\"")
                                  "\n" 'suffix)
                            #$scheme48 s48-version #$output s48-version))))
              (chmod "prescheme" #o755)))
          (replace 'build
            (lambda _
              ;; Build a minimal static library for linking Pre-Scheme code
              (let ((lib "c/libprescheme.a")
                    (objs '("c/unix/io.o"
                            "c/unix/misc.o")))
                (apply invoke "make" objs)
                (apply invoke "ar" "rcs" lib objs))
              ;; Dump a Scheme 48 image with both the Pre-Scheme compatibility
              ;; library and compiler pre-loaded, courtesy of Taylor Campbell's
              ;; Pre-Scheme Manual:
              ;; https://groups.scheme.org/prescheme/1.3/#Invoking-the-Pre_002dScheme-compiler
              (with-directory-excursion "ps-compiler"
                (let ((version #$(package-version this-package))
                      (port (open-pipe* OPEN_WRITE "scheme48")))
                  (format port (string-join
                                '(",batch"
                                  ",config ,load ../scheme/prescheme/interface.scm"
                                  ",config ,load ../scheme/prescheme/package-defs.scm"
                                  ",exec ,load load-ps-compiler.scm"
                                  ",in prescheme-compiler prescheme-compiler"
                                  ",user (define prescheme-compiler ##)"
                                  ",dump ../prescheme.image \"(Pre-Scheme ~a)\""
                                  ",exit")
                                "\n" 'suffix)
                          version)
                  (close-pipe port)))))
          (replace 'install
            (lambda _
              (let* ((s48-version #$(package-version scheme48))
                     (bin-dir     (string-append #$output "/bin"))
                     (lib-dir     (string-append #$output "/lib/scheme48-" s48-version))
                     (pkgconf-dir (string-append #$output "/lib/pkgconfig"))
                     (share-dir   (string-append #$output "/share/scheme48-" s48-version))
                     (include-dir (string-append #$output "/include")))
                ;; Install Pre-Scheme compiler image
                (install-file "prescheme" bin-dir)
                (install-file "prescheme.image" lib-dir)
                ;; Install Pre-Scheme config, headers, and lib
                (install-file "prescheme.pc" pkgconf-dir)
                (install-file "c/prescheme.h" include-dir)
                (install-file "c/prescheme-io.h" include-dir)
                (install-file "c/libprescheme.a" lib-dir)
                ;; Install Pre-Scheme sources
                (copy-recursively "scheme/prescheme"
                                  (string-append share-dir "/prescheme"))
                (copy-recursively "ps-compiler"
                                  (string-append share-dir "/ps-compiler"))
                ;; Remove files specific to building the Scheme 48 VM
                (for-each (lambda (file)
                            (delete-file (string-append share-dir "/" file)))
                          '("ps-compiler/compile-bibop-gc-32.scm"
                            "ps-compiler/compile-bibop-gc-64.scm"
                            "ps-compiler/compile-gc.scm"
                            "ps-compiler/compile-twospace-gc-32.scm"
                            "ps-compiler/compile-twospace-gc-64.scm"
                            "ps-compiler/compile-vm-no-gc-32.scm"
                            "ps-compiler/compile-vm-no-gc-64.scm"))))))))
    (propagated-inputs (list scheme48))
    (home-page "http://s48.org/")
    (synopsis "Pre-Scheme compiler from Scheme 48")
    (description
     "Pre-Scheme is a statically compilable dialect of Scheme, used to implement the
Scheme 48 virtual machine.  Scheme 48 ships with a Pre-Scheme to C compiler written
in Scheme, and a runtime library which allows Pre-Scheme code to run as Scheme.")
    (license bsd-3)))

(define-public gambit-c
  (package
    (name "gambit-c")
    (version "4.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.gambitscheme.org/" version "/gambit-v"
             (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
             ".tgz"))
       (sha256
        (base32 "1p61z1rp0ya4i61mq3hzmr67r3xbvi9h058cf9ci2yqfbzdzi3p2"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; According to the ./configure script, this makes the build slower and
       ;; use >= 1 GB memory, but makes Gambit much faster.
       '("--enable-single-host")))
    (home-page "http://www.gambitscheme.org/")
    (synopsis "Efficient Scheme interpreter and compiler")
    (description
     "Gambit consists of two main programs: gsi, the Gambit Scheme
interpreter, and gsc, the Gambit Scheme compiler.  The interpreter contains
the complete execution and debugging environment.  The compiler is the
interpreter extended with the capability of generating executable files.  The
compiler can produce standalone executables or compiled modules which can be
loaded at run time.  Interpreted code and compiled code can be freely
mixed.")
    ;; Dual license.
    (license (list lgpl2.1+ asl2.0))))

(define-public chibi-scheme
  (package
    (name "chibi-scheme")
    (version "0.11")
    (home-page "https://github.com/ashinn/chibi-scheme")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page) (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02zq35hdbi03rmmamx6ml4ihsigdl4mmbf6d9ysazv8ciiln5v4b"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))           ; no configure script
           #:make-flags
           #~(list
              #$@(if
                  (%current-target-system)
                  #~((string-append
                      "CHIBI=" #+(this-package-native-input "chibi-scheme")
                      "/bin/chibi-scheme"))
                  #~())
              (string-append "PREFIX=" #$output)
              (string-append "CC=" #$(cc-for-target))
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
           #:test-target "test"))
    (native-inputs (if (%current-target-system)
                       (list this-package)
                       (list)))
    (synopsis "Small embeddable Scheme implementation")
    (description
     "Chibi-Scheme is a very small library with no external dependencies
intended for use as an extension and scripting language in C programs.  In
addition to support for lightweight VM-based threads, each VM itself runs in
an isolated heap allowing multiple VMs to run simultaneously in different OS
threads.")
    (license bsd-3)))

(define-public unsyntax
  (let ((commit "144772eeef4a812dd79515b67010d33ad2e7e890")
        (revision "0"))
    (package
      (name "unsyntax")
      (version (git-version "0.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/nieper/unsyntax.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ia58xdrywsm0dg19kmkghnrgw6gj2bsaypyjmbpirrila73cqk0"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags
             #~(list "gl_public_submodule_commit=") ; disable submodule checks
             #:phases
             #~(modify-phases %standard-phases
                 (add-before 'bootstrap 'prepare-bootstrap
                   (lambda _
                     ;; Unsyntax relies on bootstrap to fetch gnulib, we use
                     ;; the sources from guix's gnulib package instead.
                     (copy-recursively (getenv "GNULIB_SRCDIR") ".gnulib")
                     (setenv "GNULIB_SRCDIR" ".gnulib")
                     (patch-shebang ".gnulib/gnulib-tool")
                     (patch-shebang ".gnulib/build-aux/bootstrap")
                     (patch-shebang ".gnulib/build-aux/git-version-gen")
                     (patch-shebang ".gnulib/build-aux/prefix-gnulib-mk")
                     ;; The bootstrap_sync option updates the bootstrap script
                     ;; and runs it with CONFIG_SHELL, make sure it's correct.
                     (setenv "CONFIG_SHELL" (which "sh"))
                     ;; Tell git-version-gen the correct version number.
                     (call-with-output-file ".tarball-version"
                       (lambda (port)
                         (display #$version port)))))
                 (add-before 'configure 'patch-exec-paths
                   (lambda _
                     ;; Fix hard-coded references to chibi-scheme, using the
                     ;; configured interpreter path instead.  This avoids the need
                     ;; for chibi-scheme as a propagated input.
                     (substitute* '("src/compile-unsyntax.in"
                                    "src/expand-unsyntax.in"
                                    "src/unsyntax-scheme.in")
                       (("chibi-scheme") "'@CHIBI_SCHEME@'")))))))
      (native-inputs
       (list autoconf automake libtool git gnulib help2man perl texinfo))
      (inputs
       (list chibi-scheme))
      (home-page "https://www.unsyntax.org")
      (synopsis "Expander for R7RS programs")
      (description
       "Unsyntax is an implementation of the Scheme programming language,
specifically of its R7RS standard, and includes a number of extensions.
Unsyntax evaluates Scheme expressions and compiles and runs Scheme programs by
first expanding them into a minimal dialect of R7RS (small) without any
syntactic extensions.  The resulting expression or program is then evaluated
by an existing Scheme implementation.")
      (license expat))))

(define-public sicp
  (let ((commit "bda03f79d6e2e8899ac2b5ca6a3732210e290a79")
        (revision "3"))
    (package
      (name "sicp")
      (version (git-version "20180718" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sarabander/sicp")
                      (commit commit)))
                (sha256
                 (base32
                  "0mng7qrj2dvssyffr9ycnf4a5k0kadp4dslq7mc5bhzq1qxyjs2w"))
                (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (native-inputs (list gzip texinfo))
      (arguments
       (list #:install-plan ''(("html" "share/doc/sicp/")
                               ("sicp.info" "share/info/"))
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'remove-obsolete-commands
                            (lambda _
                              ;; Reported upstream:
                              ;; https://github.com/sarabander/sicp/issues/46.
                              (substitute* "sicp-pocket.texi"
                                (("@setshortcontentsaftertitlepage")
                                 ""))))
                          (add-before 'install 'build
                            (lambda _
                              (invoke "makeinfo" "--no-split"
                                      "--output=sicp.info"
                                      "sicp-pocket.texi"))))))
      (home-page "https://sarabander.github.io/sicp")
      (synopsis "Structure and Interpretation of Computer Programs")
      (description "Structure and Interpretation of Computer Programs (SICP) is
a textbook aiming to teach the principles of computer programming.

Using Scheme, a dialect of the Lisp programming language, the book explains
core computer science concepts such as abstraction in programming,
metalinguistic abstraction, recursion, interpreters, and modular programming.")
      (license cc-by-sa4.0))))

(define-public scheme48-rx
  (let* ((commit "dd9037f6f9ea01019390614f6b126b7dd293798d")
         (revision "2"))
    (package
      (name "scheme48-rx")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scheme/rx")
               (commit commit)))
         (sha256
          (base32
           "1bvriavxw5kf2izjbil3999vr983vkk2xplfpinafr86m40b2cci"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((share (string-append %output
                                       "/share/scheme48-"
                                       ,(package-version scheme48)
                                       "/rx")))
             (chdir (assoc-ref %build-inputs "source"))
             (mkdir-p share)
             (copy-recursively "." share)
             #t))))
      (native-inputs
       `(("source" ,source)
         ("scheme48" ,scheme48)))
      (home-page "https://github.com/scheme/rx/")
      (synopsis "SRE String pattern-matching library for scheme48")
      (description
       "String pattern-matching library for scheme48 based on the SRE
regular-expression notation.")
      (license bsd-3))))

(define-public slib
  (package
    (name "slib")
    (version "3c1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://groups.csail.mit.edu/mac/ftpdir/scm/slib-"
                                 version ".zip"))
             (sha256
              (base32
               "10f7l0fmd0xzs6kc2cwqjrx7msdn0fsd918r459xyc05wscfpy62"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There is no check target.
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-bin-share
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (delete-file-recursively
                       (string-append (assoc-ref outputs "out") "/bin"))
                      #t))
         (replace 'configure
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (invoke "./configure"
                            (string-append "--prefix="
                                           (assoc-ref outputs "out"))))))))
    (native-inputs (list unzip texinfo))
    (home-page "https://people.csail.mit.edu/jaffer/SLIB.html")
    (synopsis "Compatibility and utility library for Scheme")
    (description "SLIB is a portable Scheme library providing compatibility and
utility functions for all standard Scheme implementations.")
    (license (non-copyleft
              "http://people.csail.mit.edu/jaffer/SLIB_COPYING.txt"
              "Or see COPYING in the distribution."))))

(define-public scm
  (package
    (name "scm")
    (version "5f4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://groups.csail.mit.edu/mac/ftpdir/scm/scm-"
                    version ".zip"))
              (sha256
               (base32
                "17i6shvh2caqmksm7z130f9fz0qinaxg7xz9yadv904xh3znshnk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (invoke "./configure"
                            (string-append "--prefix="
                                           (assoc-ref outputs "out")))))
         (add-before 'build 'pre-build
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "Makefile"
                         (("ginstall-info") "install-info"))
                       #t))
         (replace 'build
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (setenv "SCHEME_LIBRARY_PATH"
                            (search-input-directory inputs "lib/slib/"))
                    (invoke "make" "scmlit" "CC=gcc")
                    (invoke "make" "all")))
         (add-after 'install 'post-install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out         (assoc-ref outputs "out"))
                             (req (string-append out "/lib/scm/require.scm")))
                        (delete-file req)
                        (format (open req (logior O_WRONLY O_CREAT))
                                "(define (library-vicinity) ~s)\n"
                                (search-input-directory inputs "lib/slib/"))

                        ;; We must generate the slibcat file.
                        (invoke (string-append out "/bin/scm")
                                "-br" "new-catalog")))))))
    (inputs (list slib))
    (native-inputs (list unzip texinfo))
    (home-page "https://people.csail.mit.edu/jaffer/SCM")
    (synopsis "Scheme implementation conforming to R5RS and IEEE P1178")
    (description "GNU SCM is an implementation of Scheme.  This
implementation includes Hobbit, a Scheme-to-C compiler, which can
generate C files whose binaries can be dynamically or statically
linked with a SCM executable.")
    (license lgpl3+)))

(define-public tinyscheme
  (package
    (name "tinyscheme")
    (version "1.42")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/" name "/"
                                  name "-" version "/" name "-" version ".zip"))
              (sha256
               (base32
                "0rik3qnxqd8wjlazx8rw996pfzkjjg60v6hcbpcqzi7rgml8q4n8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "unzip" source)
             (chdir (string-append ,name "-" ,version))
             #t))
         (add-after 'unpack 'set-scm-directory
           ;; Hard-code ‘our’ init.scm instead of looking in the current
           ;; working directory, so invoking ‘scheme’ just works.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (scm (string-append out "/share/" ,name)))
               (substitute* "scheme.c"
                 (("init.scm" all)
                  (string-append scm "/" all)))
               #t)))
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; There's no ‘install’ target.  Install files manually.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (bin     (string-append out "/bin"))
                    (doc     (string-append out "/share/doc/"
                                            ,name "-" ,version))
                    (include (string-append out "/include"))
                    (lib     (string-append out "/lib"))
                    (scm     (string-append out "/share/" ,name)))
               (install-file "scheme" bin)
               (install-file "Manual.txt" doc)
               (install-file "scheme.h" include)
               (install-file "libtinyscheme.so" lib)
               (install-file "init.scm" scm)
               #t))))
       #:tests? #f))                    ; no tests
    (home-page "https://tinyscheme.sourceforge.net/")
    (synopsis "Light-weight interpreter for the Scheme programming language")
    (description
     "TinyScheme is a light-weight Scheme interpreter that implements as large a
subset of R5RS as was possible without getting very large and complicated.

It's meant to be used as an embedded scripting interpreter for other programs.
As such, it does not offer an Integrated Development Environment (@dfn{IDE}) or
extensive toolkits, although it does sport a small (and optional) top-level
loop.

As an embedded interpreter, it allows multiple interpreter states to coexist in
the same program, without any interference between them.  Foreign functions in C
can be added and values can be defined in the Scheme environment.  Being quite a
small program, it is easy to comprehend, get to grips with, and use.")
    (license bsd-3)))                   ; there are no licence headers

(define-public tr7
  (package
    (name "tr7")
    (version "1.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/jobol/tr7")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0n77fkm5kcv2pmwbw5fl8r00aarw8da8gkd9d1ki5fn9kbl4fyk2"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (substitute* "Makefile"
                (("PREFIX = /usr/local")
                 (string-append "PREFIX=" #$output))
                (("ALL = \\$\\(LIBSTA\\) \\$\\(TR7I\\) tags")
                 "ALL = $(LIBSTA) $(TR7I)"))))
          (replace 'build
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (invoke "make")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "test"))))
          (replace 'install
            (lambda _
              (let* ((share (string-append #$output "/share"))
                     (doc (string-append #$output:doc "/share/doc/"))
                     (bin (string-append #$output "/bin"))
                     (lib (string-append #$output "/lib/"))
                     (tr7 (string-append share "/tr7"))
                     (libs (string-append tr7 "/libs")))
                (for-each mkdir-p (list tr7 libs bin lib doc))
                (copy-file "tr7i" (string-append bin "/tr7i"))
                (copy-file "libtr7.a" (string-append lib "/libtr7.a"))
                (copy-file "r7rs.pdf" (string-append doc "/r7rs.pdf"))
                (copy-recursively "tr7libs" libs)))))))
    (home-page "https://gitlab.com/jobol/tr7")
    (synopsis "Embedded R7RS small Scheme interpreter")
    (description
     "TR7 is a lightweight Scheme interpreter that implements the revision
R7RS small of scheme programming language.

It is meant to be used as an embedded scripting interpreter for other
programs.  A lot of functionality in TR7 is included conditionally, to allow
developers freedom in balancing features and footprint.")
    (license bsd-0)))

(define-public stalin
  (let ((commit "ed1c9e339c352b7a6fee40bb2a47607c3466f0be"))
    ;; FIXME: The Stalin "source" contains C code generated by itself:
    ;; 'stalin-AMD64.c', etc.
    (package
      (name "stalin")
      (version "0.11")
      (source (origin
                ;; Use Pearlmutter's upstream branch with AMD64 patches
                ;; applied. Saves us from including those 20M! patches
                ;; in Guix. For more info, see:
                ;; <ftp.ecn.purdue.edu/qobi/stalin-0.11-amd64-patches.tgz>
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/barak/stalin")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "15a5gxj9v7jqlgkg0543gdflw0rbrir7fj5zgifnb33m074wiyhn"))
                (modules '((guix build utils)))
                (snippet
                 ;; remove gc libs from build, we have them as input
                 '(begin
                    (delete-file "gc6.8.tar.gz")
                    (delete-file-recursively "benchmarks")
                    (substitute* "build"
                      ((".*gc6.8.*") "")
                      (("  cd \\.\\.") "")
                      ((".*B include/libgc.a") "")
                      ((".*make.*") ""))
                    #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "ARCH_OPTS=-freg-struct-return")
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (include-out (string-append out "/include")))
                 (invoke "./build")
                 (for-each (lambda (fname)
                             (install-file fname include-out))
                           (find-files "include"))
                 (substitute* "makefile"
                   (("\\./include") include-out))
                 (substitute* "post-make"
                   (("`pwd`") out))
                 #t)))
           (delete 'check)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "stalin.1"
                               (string-append out "/share/man/man1"))
                 (install-file "stalin"
                               (string-append out "/bin"))
                 #t))))))
      (inputs
       (list libx11))
      (propagated-inputs
       (list libgc))
      (supported-systems '("x86_64-linux"))
      (home-page "https://engineering.purdue.edu/~qobi/papers/fdlcc.pdf")
      (synopsis "Brutally efficient Scheme compiler")
      (description
       "Stalin is an aggressively optimizing whole-program compiler
for Scheme that does polyvariant interprocedural flow analysis,
flow-directed interprocedural escape analysis, flow-directed
lightweight CPS conversion, flow-directed lightweight closure
conversion, flow-directed interprocedural lifetime analysis, automatic
in-lining, unboxing, and flow-directed program-specific and
program-point-specific low-level representation selection and code
generation.")
      (license gpl2+))))

(define-public s9fes
  (package
    (name "s9fes")
    (version "20181205")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.t3x.org/s9fes/s9fes-" version ".tgz"))
       (sha256
        (base32 "0ynpl707bc9drwkdpdgvw14bz9zmwd3wffl1k02sxppjl28xm7rf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "install-all" make-flags))))
       #:tests? #f))  ; No check target.
    (inputs
     (list ncurses))
    (home-page "https://www.t3x.org/s9fes/")
    (synopsis "Interpreter for R4RS Scheme")
    (description
     "Scheme 9 from Empty Space (S9fES) is a mature, portable, and
comprehensible public-domain interpreter for R4RS Scheme offering:
@itemize
@item bignum arithmetics
@item decimal-based real number arithmetics
@item support for low-level Unix programming
@item cursor addressing with Curses
@item basic networking procedures
@item an integrated online help system
@item loads of useful library functions
@end itemize")
    (license public-domain)))

(define-public femtolisp
  (let ((commit "ec7601076a976f845bc05ad6bd3ed5b8cde58a97")
        (revision "2"))
    (package
      (name "femtolisp")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/JeffBezanson/femtolisp")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1fcyiqlqn27nd4wxi27km8mhmlzpzzsxzpwsl1bxbmhraq468njw"))))
      ;; See "utils.h" for supported systems. Upstream bug:
      ;; https://github.com/JeffBezanson/femtolisp/issues/25
      (supported-systems
       (fold delete %supported-systems
             '("armhf-linux" "mips64el-linux" "aarch64-linux")))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags '("CC=gcc" "release")
         #:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (delete 'bootstrap)
           (delete 'configure) ; No configure script
           (replace 'install ; Makefile has no 'install phase
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "flisp" bin)
                #t)))
           ;; The flisp binary is now available, run bootstrap to
           ;; generate flisp.boot and afterwards runs make test.
           (add-after 'install 'bootstrap-gen-and-test
             (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (invoke "./bootstrap.sh")
                (install-file "flisp.boot" bin)
                #t))))))
      (synopsis "Scheme-like lisp implementation")
      (description
       "@code{femtolisp} is a scheme-like lisp implementation with a
simple, elegant Scheme dialect.  It is a lisp-1 with lexical scope.
The core is 12 builtin special forms and 33 builtin functions.")
      (home-page "https://github.com/JeffBezanson/femtolisp")
      (license bsd-3))))

(define-public gauche
  (package
    (name "gauche")
    (version "0.9.15")
    (home-page "https://practical-scheme.net/gauche/index.html")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/shirok/Gauche/releases/download/release"
             (string-replace-substring version "." "_")
             "/Gauche-" version ".tgz"))
       (sha256
        (base32 "10zpbbikkcpdzk6c52wkckiyhn7nhnqjv2djdzyjr0n8qxxy4hrn"))))
    (build-system gnu-build-system)
    (inputs
     (list libatomic-ops mbedtls slib zlib))
    (native-inputs
     (list texinfo openssl ; needed for tests
           pkg-config))    ; needed to find external libatomic-ops
    (arguments
     (list #:tests? #f ; 9 ffitest failures, known issue (fixed in 0.9.16):
           ;; https://github.com/shirok/Gauche/issues/1044
           #:configure-flags
           #~(list (string-append "--with-slib=" #$(this-package-input "slib")
                                  "/lib/slib")
                   "--with-tls=mbedtls")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-/bin/sh
                 ;; Needed only for tests.
                 (lambda _
                   (substitute* '("test/www.scm"
                                  "ext/tls/test.scm"
                                  "lib/gauche/package/util.scm"
                                  "libsrc/gauche/process.scm")
                     (("/bin/sh") (which "sh")))))
               (add-after 'build 'build-doc
                 (lambda _
                   (with-directory-excursion "doc"
                     (invoke "make" "info"))))
               (add-before 'check 'patch-network-tests
                 ;; Remove net checks.
                 (lambda _
                   (delete-file "test/net.scm")
                   (invoke "touch" "test/net.scm")))
               (add-after 'install 'install-docs
                 (lambda _
                   (with-directory-excursion "doc"
                     (invoke "make" "install")))))))
    (synopsis "Scheme scripting engine")
    (description "Gauche is a R7RS Scheme scripting engine aiming at being a
handy tool that helps programmers and system administrators to write small to
large scripts quickly.  Quick startup, built-in system interface, native
multilingual support are some of the goals.  Gauche comes with a package
manager/installer @code{gauche-package} which can download, compile, install
and list gauche extension packages.")
    (license bsd-3)))

(define-public sbcl-airship-scheme
  (let ((commit "1862db81dfa67729444916c361f39f9f1c5a2ccd")
        (revision "0"))
    (package
      (name "sbcl-airship-scheme")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/mbabich/airship-scheme.git")
               (commit commit)))
         (file-name (git-file-name "cl-airship-scheme" version))
         (sha256
          (base32 "1d1kvrzlx5kcfsn3rn30ww8jihjflpgcka3n3awj2k4f0sq4mplg"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs (list sbcl-fiveam))
      (inputs
        (list sbcl-alexandria
              sbcl-float-features
              sbcl-trivial-features
              sbcl-zr-utils))
      (synopsis "R7RS Scheme implementation in Common Lisp")
      (description
       "This is a R7RS Scheme implementation designed to run within
a Common Lisp environment.")
      (home-page "https://gitlab.com/mbabich/airship-scheme")
      (license expat))))

(define-public cl-airship-scheme
  (sbcl-package->cl-source-package sbcl-airship-scheme))

(define-public gerbil
  (package
    (name "gerbil")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mighty-gerbils/gerbil/releases/download/v"
             version "/gerbil-v" version ".tar.gz"))
       (sha256
        (base32 "1dff14bzqkq6scyyhnwhc3ky96j6lr84mnghk4da0x6vifw7p0p1"))))
    (arguments
     (list
      ;; Do not build with '-march=native'.
      #:configure-flags #~(list "--enable-march=")

      #:phases #~(modify-phases %standard-phases
                   (delete 'bootstrap)
                   (add-after 'set-paths 'set-cc
                     (lambda _
                       (setenv "CC" #$(cc-for-target))))
                   (add-before 'patch-generated-file-shebangs 'fix-gxi-shebangs
                     (lambda _
                       ;; Some .ss files refer to gxi using /usr/bin/env gxi
                       ;; and 'patch-generated-file-shebangs can't fix that
                       ;; because gxi has not been compiled yet.
                       ;; We know where gxi is going to end up so we
                       ;; Doctor Who our fix here before the problem
                       ;; happens towards the end of the build.sh script.
                       (let ((abs-srcdir (getcwd)))
                         (for-each (lambda (f)
                                     (substitute* f
                                       (("#!/usr/bin/env gxi")
                                        (string-append "#!" abs-srcdir
                                                       "/../bin/gxi"))))
                                   '("./src/std/web/rack-test.ss"
                                     "./src/std/web/fastcgi-test.ss"
                                     "./src/std/build.ss"
                                     "./src/build/build-libgerbil.ss"
                                     "./src/gerbil/test/test-build-static-exe.ss"
                                     "./src/gerbil/test/test-build-optimized-static-exe.ss"
                                     "./src/gerbil/test/test-build-optimized-exe.ss"
                                     "./src/gerbil/test/test-build-exe.ss"
                                     "./src/srfi/build.ss"
                                     "./src/r7rs-large/build.ss"
                                     "./src/misc/rpc-perf/build.ss"
                                     "./src/misc/http-perf/build.ss"
                                     "./src/misc/scripts/docsnarf.ss"
                                     "./src/misc/scripts/docstub.ss"
                                     "./src/misc/scripts/docsyms.ss"
                                     "./src/tools/gxpkg.ss"
                                     "./src/tools/build.ss"
                                     "./src/lang/build.ss"
                                     "./src/tutorial/kvstore/build.ss"
                                     "./src/tutorial/httpd/build.ss"
                                     "./src/tutorial/proxy/build.ss"
                                     "./src/tutorial/ensemble/build.ss"
                                     "./src/tutorial/lang/build.ss")))))
                   (replace 'build
                     (lambda _
                       (setenv "HOME"
                               (getcwd))
                       (invoke
                        ;; The build script needs a tty or it'll crash on an ioctl
                        ;; trying to find the width of the terminal it's running on.
                        ;; Calling in script prevents that.
                        "script"
                        "-qefc"
                        "./build.sh"))))
      #:tests? #f))
    (native-inputs (list gambit-c util-linux))
    (propagated-inputs (list gambit-c openssl sqlite zlib))
    (build-system gnu-build-system)
    (synopsis "Meta-dialect of Scheme with post-modern features")
    (description
     "Gerbil is an opinionated dialect of Scheme designed for Systems
Programming, with a state of the art macro and module system on top of the Gambit
runtime.  The macro system is based on quote-syntax, and provides the full meta-syntactic
tower with a native implementation of syntax-case.  It also provides a full-blown module
system, similar to PLT Scheme's (sorry, Racket) modules.  The main difference from Racket
is that Gerbil modules are single instantiation, supporting high performance ahead of
time compilation and compiled macros.")
    (home-page "https://cons.io")
    (license `(,lgpl2.1 ,asl2.0))))

(define-public emacs-gerbil-mode
  (package
    (inherit gerbil)
    (name "emacs-gerbil-mode")
    (version "1.0")
    (build-system emacs-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'install 'change-directory
                          (lambda _
                            (chdir "etc"))))))
    (synopsis "Emacs major-mode for editing Gerbil code")
    (description
     "Gerbil mode provides font-lock, indentation, navigation, and REPL for
Gerbil code within Emacs.")))

(define-public owl
  (package
    (name "owl")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/owl-lisp/owl.git")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "0jlmpw14rg63m1q7pjmhjicaqbqgc6gnp53bph0giwg8ha8wxyqr"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))
      #:make-flags #~`(,(string-append "PREFIX=" #$output))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure) ; no configure script
                   (add-after 'patch-source-shebangs 'patch-ol-shebangs
                     (lambda _
                     (lambda _
                       (substitute* (list "bin/feather"
                                          "tests/hashbang.scm"
                                          "tests/theorem-rand.scm")
                         (("/usr") #$output))))))
      #:test-target "test"))
    (native-inputs (list which))
    (home-page "https://haltp.org/posts/owl.html")
    (synopsis "Functional Scheme dialect")
    (description
     "Owl Lisp is a simple programming language.  It is intended to provide a
portable system for writing standalone programs in a subjectively pleasant
dialect of Lisp.  It has a minimal core and runtime, purely functional
operation, and support for asynchronous evaluation.")
    (license expat)))

(define-public stklos
  (package
    (name "stklos")
    (version "2.10")
    (source (origin
              (method url-fetch)
              ;; TODO: Unbundle pcre, libgc, and libffi.
              (uri (string-append "https://stklos.net/download/stklos-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hd05r5pr3yhgq44n5sqdmvkpgnhf5fybmis2g3gwj10z52h7gvd"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list gmp libgc pcre2 libffi readline))
    (arguments
     (list
      #:modules `((ice-9 ftw)
                  ,@%default-gnu-modules)
      #:configure-flags
      #~(list (string-append "LDFLAGS=-L"
                             #$(this-package-input "readline")
                             "/lib -lreadline"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-sh-references
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bash (which "bash")))
                (substitute* "configure"
                  (("/bin/sh") bash)))))
          (add-after 'configure 'patch-rm-references
            (lambda _
              (let ((rm (which "rm")))
                (substitute* (find-files "." "^Makefile$")
                  (("/bin/rm") rm))))))))
    (properties
     '((release-monitoring-url . "https://stklos.net/download.html")))
    (home-page "https://stklos.net")
    (synopsis "R7RS Scheme with CLOS-like object system")
    (description
     "STklos is a free Scheme system mostly compliant with the languages
features defined in R7RS small.  The aim of this implementation is to be fast
as well as light.  The implementation is based on an ad-hoc Virtual
Machine.  STklos can also be compiled as a library and embedded in an
application.")
    (license gpl2+)))

(define-public r7rs-small-texinfo
  (let ((commit "38a703976ea6353e32b52a5187dbdaf77fb2f050")
        (revision "3"))
    (package
      (name "r7rs-small-texinfo")
      (version (git-version "0.1.0" revision commit))
      (home-page "https://codeberg.org/Zipheir/r7rs-small-texinfo/")
      (source
       (origin
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (method git-fetch)
         (file-name (git-file-name name version))
         (sha256
          (base32 "1fr02fyhiwd364jkfy1n5w31pq3kx1rl5w634421g05702yb47x3"))))
      (native-inputs (list bash texinfo))
      (inputs '())
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan #~'(("r7rs-small.info" "share/info/"))
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'compile-the-files
                       (lambda _
                         (let* ((source-directory-path (string-append (getcwd)
                                                        "/doc/r7rs-small"))
                                (build-script-path (string-append
                                                    source-directory-path
                                                    "/build.sh"))
                                (info-directory-path (string-append #$output
                                                      "/share/info")))
                           (chdir source-directory-path)
                           (system* "bash" build-script-path "info")
                           (mkdir-p info-directory-path)
                           (copy-file (string-append source-directory-path
                                                     "/r7rs-small.info")
                                      (string-append info-directory-path
                                                     "/r7rs-small.info"))))))))
      (synopsis
       "R7RS Small standard of the Scheme programming language in Info format")
      (description
       "Revised^7 Report of the Algorithmic Language Scheme adapted to Texinfo format.")
      (license (non-copyleft "file://COPYING")))))
