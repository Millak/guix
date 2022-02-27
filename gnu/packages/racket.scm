;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2021, 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 jgart <jgart@dismail.de>
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

(define-module (gnu packages racket)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages chez)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:))

;; Commentary:
;;
;; Anatomy of Racket:
;; ------------------
;;
;; The main Racket Git repository (<https://github.com/racket/racket>) is
;; organized broadly like this:
;;
;;     .
;;     ├── Makefile
;;     ├── pkgs/
;;     └── racket/
;;         ├── collects/
;;         └── src/
;;             ├── configure
;;             ├── Makefile.in
;;             ├── bc/
;;             ├── cs/
;;             ├── ChezScheme/
;;             └── ...
;;
;; The 'racket/src/' directory contains the source of the runtime system, core
;; compiler, and primitives for the major Racket implementations: this layer
;; is called the ``Racket VM''. It is basically a normal autotools
;; project. (Even when Racket VM implementations use components implemented in
;; Racket, they are compiled in special modes to produce VM primitives.)
;; (There are or have been experimental Racket VM implementations elsewhere,
;; e.g. <https://github.com/pycket/pycket>.)
;;
;; The 'racket/collects/' directory contains ``built in'' Racket libraries
;; that are not part of any package, including the implementation of
;; 'racket/base': in particular, it must contain enough to implement `raco pkg
;; install'. It is theoretically possible to use the Racket VM layer without
;; the main collections, but it is not stable or useful.
;;
;; The 'pkgs/' directory contains Racket packages that are especially closely
;; tied to the implementation of the Racket VM, including 'compiler-lib',
;; 'racket-doc', and 'racket-test'. Some of these packages depend on Racket
;; packages that are developed in other Git repositories, predominantly but
;; not exclusively under the 'racket' GitHub organization. Conversely, not all
;; of the packages developed in the main Git repository are part of the main
;; Racket distribution.  (Additionally, components of the Racket VM that are
;; implemented in Racket can be installed as packages, mostly for ease of
;; development.)
;;
;; The top-level 'Makefile' is more like a directory of scripts: it has
;; convienience targets for developing Racket, and it cooperates with the
;; 'distro-build' package to assemble custom Racket distributions. It is not
;; part of Racket source distributions: the root of a source distribution is
;; basically 'racket/src' with some extra package sources and configuration
;; added.
;;
;; A ''minimal Racket'' installation includes two packages: 'base', which is a
;; sort of bridge between the current ``built-in'' collections and the package
;; system's model of dependencies, and 'racket-lib', which, for installations
;; that can not rely on a system package manager, pulls in the SQLite and
;; OpenSSL shared libraries as platform-specific dependencies for use by the
;; ``built-in'' collections.
;;
;; The main Racket distribution consists of installing the 'main-distribution'
;; package and all of its dependencies.
;;
;; The default mode when building Racket (or installing it with the released
;; installers) is an ``in-place build'', which produces a self-contained,
;; relocatable, roughly FHS-like directory. (Racket also supports
;; ``Unix-style'' installations, which rearrange the parts of an in-place
;; build into Racket-specific subdirectories and generally tries to work for
;; installation into an FHS-based system.) Certain tools, e.g. 'distro-build'
;; and 'raco cross', are able to work with an in-place Racket build.
;;
;; This file defines the packages 'racket-vm-cgc', 'racket-vm-bc', and
;; 'racket-vm-cs'. All three are in-place builds of 'racket/src/' and
;; 'racket/collects/' and are installed to 'opt/racket-vm/' in the store
;; output. The function 'racket-vm-for-system' returns the recomended Racket
;; VM package for a given system.
;;
;; The file 'racket.scm' builds on these packages to define 'racket-minimal'
;; and 'racket' packages. These use Racket's support for ``layered
;; installations'', which allow an immutable base layer to be extended with
;; additional packages. They use the layer configuration directly provide
;; ready-to-install FHS-like trees, rather than relying on the built in
;; ``Unix-style install'' mechanism.
;;
;; Bootstrapping Racket:
;; ---------------------
;;
;; Here's how bootstrapping Racket works:
;;
;;   - Racket BC [CGC] can be built with only a C compiler (except for
;;     one caveat discussed below).
;;   - Racket BC [3M] needs an existing Racket to run "xform",
;;     which transforms its own C source code to add additional annotations
;;     for the precise garbage collector.
;;   - Racket CS needs (bootfiles for) Racket's fork of Chez Scheme.
;;     It also needs an existing Racket to compile Racket-implemented
;;     parts of the runtime system to R6RS libraries.
;;   - Chez Scheme also needs bootfiles for itself, but Racket can simulate
;;     enough of Chez Scheme to load Racket's fork of the Chez Scheme compiler
;;     purely from source into Racket and apply the compiler to itself,
;;     producing the needed bootfiles (albeit very slowly).
;;     Any variant of Racket since version 7.1 can run the simulation.
;;
;; So, we build CGC to build 3M to build bootfiles and CS.
;;
;; (Note: since the CGC variant is basically only for bootstrapping, we
;; often use "BC" to mean "3M", consistent with `(banner)` and the
;; suffixes used on executables when more than one variant co-exists.)
;;
;; One remaining bootstrapping limitation is that Racket's reader, module
;; system, and macro expander are implemented in Racket. For Racket CS,
;; they are compiled to R6RS libraries as discussed above. This note from the
;; README file applies to all such subsystems:
;;
;;     The Racket version must be practically the same as the current Racket
;;     verson, although it can be the Racket BC implementation (instead of
;;     the Racket CS implementation).
;;
;;     Unlike Chez Scheme boot files, the files generated in "schemified"
;;     are human-readable and -editable Scheme code. That provides a way
;;     out of bootstrapping black holes, even without BC.
;;
;; However, other Racket subsystems implemented in Racket for Racket CS
;; use older C implementations for Racket BC, whereas the reader, expander,
;; and module system were completely replaced with the Racket implementation
;; as of Racket 7.0.
;;
;; For Racket BC, the compiled "linklet" s-expressions (primitive modules)
;; are embeded in C as a static string constant. Eventually, they are further
;; compiled by the C-implemented Racket BC bytecode and JIT compilers.
;; (On platforms where Racket BC's JIT is not supported, yet another compiler
;; instead compiles the linklets to C code, but this is not a bootstrapping
;; issue.)
;;
;; Code:

(define %racket-version "8.4")
;; ^ Remember to update chez-scheme-for-racket-bootstrap-bootfiles!
(define %racket-commit
  (string-append "v" %racket-version))
(define %racket-origin
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/racket/racket")
          (commit %racket-commit)))
    (sha256
     (base32 "1vpl66gdgc8rnldmn8rmb7ar9l057jqjvgpfn29k57i3c5skr8s6"))
    (file-name (git-file-name "racket" %racket-version))
    (patches (search-patches "racket-minimal-sh-via-rktio.patch"
                             ;; Remove by Racket 8.5:
                             "racket-enable-scheme-backport.patch"))
    (modules '((guix build utils)))
    (snippet
     #~(begin
         (use-modules (guix build utils))
         ;; Unbundle Chez submodules.
         (with-directory-excursion "racket/src/ChezScheme"
           ;; TODO: consider putting this in a (guix ...) or (guix build ...)
           ;; module so it can be shared with the upstream Chez Scheme origin
           ;; without cyclic issues.
           (for-each (lambda (dir)
                       (when (directory-exists? dir)
                         (delete-file-recursively dir)))
                     '("stex"
                       "nanopass"
                       "lz4"
                       "zlib")))
         ;; Unbundle libffi.
         (delete-file-recursively "racket/src/bc/foreign/libffi")))))

(define (racket-vm-common-configure-flags)
  ;; under a lambda abstraction to avoid evaluating bash-minimal too early.
  #~`(,@(cond
         ((false-if-exception
           (search-input-file %build-inputs "/bin/libtool"))
          => (lambda (libtool)
               (list (string-append "--enable-lt=" libtool))))
         (else
          '()))
      ,@(cond
         ((false-if-exception
           (search-input-file %build-inputs "/opt/racket-vm/bin/racket"))
          => (lambda (racket)
               (list (string-append "--enable-racket=" racket))))
         (else
          '()))
      ,(string-append "CPPFLAGS=-DGUIX_RKTIO_PATCH_BIN_SH="
                      #$(file-append bash-minimal "/bin/sh"))
      "--disable-strip"
      ;; XXX: origtree layout is required by some other packages down the
      ;; bootstrap chain.  Remove these flags as soon as we can do without them.
      "--enable-origtree"
      ,(string-append "--prefix=" #$output "/opt/racket-vm")))

(define (make-unpack-nanopass+stex)
  ;; Adapted from chez-scheme.
  ;; Thunked to avoid evaluating 'chez-scheme' too early.
  ;; TODO: Refactor enough to share this directly.
  #~(begin
      (copy-recursively
       #$(match (assoc-ref (package-native-inputs chez-scheme)
                           "nanopass")
           ((src)
            src))
       "nanopass"
       #:keep-mtime? #t)
      (mkdir-p "stex")
      (with-output-to-file "stex/Mf-stex"
        (lambda ()
          ;; otherwise, it will try to download submodules
          (display "# to placate ../configure")))))


(define-public racket-vm-cgc
  ;; Eventually, it may make sense for some vm packages to not be hidden,
  ;; but this one is especially likely to remain hidden.
  (hidden-package
   (package
     (name "racket-vm-cgc")
     (version %racket-version)
     (source %racket-origin)
     (inputs (list ncurses ;; <- common to all variants (for #%terminal)
                   bash-minimal ;; <- common to all variants (for `system`)
                   libffi)) ;; <- only for BC variants
     (native-inputs (list libtool)) ;; <- only for BC variants
     (outputs '("out" "debug"))
     (build-system gnu-build-system)
     (arguments
      (list
       #:configure-flags
       #~(cons "--enable-cgcdefault"
               #$(racket-vm-common-configure-flags))
       ;; Tests are in packages like racket-test-core and
       ;; main-distribution-test that aren't part of the main
       ;; distribution.
       #:tests? #f
       ;; Upstream recommends #:out-of-source?, and it does
       ;; help with debugging, but it confuses `install-license-files`.
       #:modules '((ice-9 match)
                   (ice-9 regex)
                   (guix build gnu-build-system)
                   (guix build utils))
       #:strip-directories #~'("opt/racket-vm/bin"
                               "opt/racket-vm/lib")
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'configure 'initialize-config.rktd
             (lambda* (#:key inputs #:allow-other-keys)
               (define (write-racket-hash alist)
                 ;; inside must use dotted pair notation
                 (display "#hash(")
                 (for-each (match-lambda
                             ((k . v)
                              (format #t "(~s . ~s)" k v)))
                           alist)
                 (display ")\n"))
               (define maybe-release-catalog
                 (let ((v #$(package-version this-package)))
                   (if (string-match "^[0-9]+\\.[0-9]+($|\\.[0-8][0-9]*$)"
                                     v)
                       `(,(string-append
                           "https://download.racket-lang.org/releases/"
                           v
                           "/catalog/"))
                       '())))
               (mkdir-p "racket/etc")
               (with-output-to-file "racket/etc/config.rktd"
                 (lambda ()
                   (write-racket-hash
                    `((build-stamp . "")
                      (catalogs ,@maybe-release-catalog
                                #f)))))))
           (add-before 'configure 'chdir
             (lambda _
               (chdir "racket/src"))))))
     (home-page "https://racket-lang.org")
     (synopsis "Old Racket implementation used for bootstrapping")
     (description "This variant of the Racket BC (``before Chez'' or
``bytecode'') implementation is not recommended for general use.  It uses
CGC (a ``Conservative Garbage Collector''), which was succeeded as default in
PLT Scheme version 370 (which translates to 3.7 in the current versioning
scheme) by the 3M variant, which in turn was succeeded in version 8.0 by the
Racket CS implementation.

Racket CGC is primarily used for bootstrapping Racket BC [3M].  It may
also be used for embedding applications without the annotations needed in C
code to use the 3M garbage collector.")
     ;; https://download.racket-lang.org/license.html
     ;; The LGPL components are only used by Racket BC.
     (license (list license:lgpl3+ license:asl2.0 license:expat)))))

(define-public racket-vm-bc
  (package
    (inherit racket-vm-cgc)
    (name "racket-vm-bc")
    (native-inputs
     (modify-inputs (package-native-inputs racket-vm-cgc)
       (prepend racket-vm-cgc)))
    (arguments
     (substitute-keyword-arguments (package-arguments racket-vm-cgc)
       ((#:configure-flags _ '())
        #~(cons "--enable-bconly"
                #$(racket-vm-common-configure-flags)))))
    (synopsis "Racket BC [3M] implementation")
    (description "The Racket BC (``before Chez'' or ``bytecode'')
implementation was the default before Racket 8.0.  It uses a compiler written
in C targeting architecture-independent bytecode, plus a JIT compiler on most
platforms.  Racket BC has a different C API and supports a slightly different
set of architectures than the current default runtime system, Racket CS (based
on ``Chez Scheme'').  It is the recommended implementation for architectures
that Racket CS doesn't support.

This package is the normal implementation of Racket BC with a precise garbage
collector, 3M (``Moving Memory Manager'').")
    ;; https://download.racket-lang.org/license.html
    ;; The LGPL components are only used by Racket BC.
    (license (list license:lgpl3+ license:asl2.0 license:expat))))

(define-public racket-vm-cs
  (package
    (inherit racket-vm-bc)
    (name "racket-vm-cs")
    (inputs
     (modify-inputs (package-inputs racket-vm-cgc)
       (prepend zlib lz4)
       (delete "libffi")))
    (native-inputs
     (modify-inputs (package-native-inputs racket-vm-cgc)
       (delete "libtool")
       (prepend chez-scheme-for-racket-bootstrap-bootfiles
                racket-vm-bc)))
    (arguments
     (substitute-keyword-arguments (package-arguments racket-vm-cgc)
       ((#:phases those-phases #~%standard-phases)
        #~(modify-phases #$those-phases
            (add-after 'unpack 'unpack-nanopass+stex
              (lambda args
                (with-directory-excursion "racket/src/ChezScheme"
                  #$(make-unpack-nanopass+stex))))
            (add-after 'unpack-nanopass+stex 'unpack-bootfiles
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (with-directory-excursion "racket/src/ChezScheme"
                  (copy-recursively
                   (search-input-directory (or native-inputs inputs)
                                           "lib/chez-scheme-bootfiles")
                   "boot"))))))
       ((#:configure-flags _ '())
        #~(cons* "--enable-csonly"
                 "--enable-libz"
                 "--enable-lz4"
                 #$(racket-vm-common-configure-flags)))))
    (synopsis "Racket CS implementation")
    (description "The Racket CS implementation, which uses ``Chez Scheme'' as
its core compiler and runtime system, has been the default Racket VM
implemetation since Racket 8.0.  It performs better than the Racket BC
implementation for most programs.

Using the Racket VM packages directly is not recommended: instead, install the
@code{racket-minimal} or @code{racket} packages.")
    ;; https://download.racket-lang.org/license.html
    ;; The LGPL components are only used by Racket BC.
    (license (list license:asl2.0 license:expat))))

(define (racket-packages-origin name origin specs)
  "Extract from ORIGIN the sources for the Racket packages specified by SPECS,
a non-empty list of package specifications.  In the resulting file-like
object, each package's source will be in the directory
\"/share/racket/pkgs/PKG/\", where PKG is the Racket name for the package.
The NAME will be used in the store file name for the resulting file-like
object.

A package specification is a list of the form:

  (PKG PATH)

where PATH is the path to the package source relative to ORIGIN---possibly
\".\".  As a special case, a package specification may also be a string, which
is equivalent to:

  (PKG PKG)

Examples:

- \"expeditor\"
- (\"main-distribution\" \".\")
- (\"racket-lib\" \"pkgs/racket-lib\")"
  (computed-file
   (string-append "racket-pkg-" name "-sources")
   (with-imported-modules `((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (mkdir-p (string-append #$output "/share/racket/pkgs"))
         (chdir (string-append #$output "/share/racket/pkgs"))
         #$@(map (match-lambda
                   ((? string? name)
                    #~(copy-recursively #$(file-append origin (string-append "/" name))
                                        #$name))
                   ((name ".")
                    #~(copy-recursively #$origin #$name))
                   ((name path)
                    #~(copy-recursively #$(file-append origin (string-append "/" path))
                                        #$name)))
                 specs)))))

(define (simple-racket-origin repo hash specs)
  "Like 'racket-packages-origin', but specialized for packages hosted at
\"https://github.com/racket/REPO\" with sha256 checksum HASH.  REPO is also
used to build the name of the resulting store item."
  (racket-packages-origin
   repo
   (origin
     (method git-fetch)
     (uri (git-reference
           (url (format #f "https://github.com/racket/~a" repo))
           (commit %racket-commit)))
     (sha256 hash)
     (file-name (git-file-name (string-append "racket-" repo)
                               %racket-version)))
   specs))

(define-public racket-minimal
  (package
    (name "racket-minimal")
    (version %racket-version)
    (source #f)
    ;; For cross-compilation, Matthew Flatt recommends reusing
    ;; as much of `raco cross` as possible. So, put that off until
    ;; we have a build system for Racket packages.
    (inputs
     (list openssl
           sqlite
           racket-vm-cs ;; TODO (racket-vm-for-system)
           (racket-packages-origin
            "base" %racket-origin
            '(("base" "pkgs/base")
              ("racket-lib" "pkgs/racket-lib")))))
    (build-system gnu-build-system)
    (arguments
     ;; Here and for the `racket` package, we're using #:configure-flags
     ;; to pass flags for `configure-layer.rkt` and #:make-flags
     ;; to pass arguments for `raco pkg install`.
     (list
      #:configure-flags
      #~`("--tethered"
          "--extra-foreign-lib-search-dirs"
          ,(format
            #f "~s"
            (list #$(file-append (this-package-input "openssl") "/lib")
                  #$(file-append (this-package-input "sqlite") "/lib"))))
      #:make-flags #~`("racket-lib")
      #:tests? #f ;; packaged separately
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (guix build union)
                  (ice-9 match))
      #:imported-modules `((guix build union)
                           ,@%gnu-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (delete 'unpack)
          (replace 'configure
            (lambda* (#:key inputs configure-flags #:allow-other-keys)
              (let* ((vm-dir (search-input-directory inputs "opt/racket-vm"))
                     (racket (string-append vm-dir "/bin/racket")))
                (apply invoke
                       racket
                       #$configure-layer.rkt
                       `(,@(cond
                            ((false-if-exception
                              (search-input-file
                               inputs "etc/racket/config.rktd"))
                             => (lambda (file)
                                  `("--parent"
                                    ,(dirname (dirname (dirname file))))))
                            (else
                             '()))
                         ,@configure-flags
                         ,vm-dir
                         ,#$output))
                (invoke racket
                        "--config" (string-append #$output "/etc/racket")
                        "-l" "raco" "setup"
                        "--no-user"))))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              ;; We use "share/racket/pkgs" for sources to distinguish them
              ;; from the "lib/racket/pkgs" of a potential parent layer.
              (union-build (string-append #$output "/lib/racket/pkgs")
                           (search-path-as-list '("share/racket/pkgs")
                                                (map cdr inputs))
                           #:create-all-directories? #t)))
          (replace 'install
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (let ((racket
                     (search-input-file inputs "/opt/racket-vm/bin/racket")))
                (unless (null? make-flags)
                  (invoke racket
                          "-l-"
                          "pkg/dirs-catalog"
                          "--link"
                          "local-catalog"
                          (string-append #$output "/lib/racket/pkgs"))
                  (apply invoke
                         racket
                         "--config" (string-append #$output "/etc/racket")
                         "-l" "raco"
                         "pkg" "install"
                         "--installation"
                         "--auto"
                         "--catalog" "local-catalog"
                         make-flags))))))))
    (home-page "https://racket-lang.org")
    (synopsis "Racket without bundled packages such as DrRacket")
    (description
     "Racket is a general-purpose programming language in the Scheme family,
with a large set of libraries and a compiler based on Chez Scheme.  Racket is
also a platform for language-oriented programming, from small domain-specific
languages to complete language implementations.

The ``minimal Racket'' distribution includes just enough of Racket for you to
use @command{raco pkg} to install more.  Bundled packages, such as the
DrRacket IDE, are not included.")
    ;; https://download.racket-lang.org/license.html
    ;; The LGPL components are only used by Racket BC.
    (license (list license:asl2.0 license:expat))))

(define-public racket
  (package
    (inherit racket-minimal)
    (name "racket")
    (source #f)
    (native-inputs (list racket-minimal)) ; XXX: conservative estimate, untested
    (inputs
     (list
      cairo
      fontconfig
      glib
      glu
      gmp
      gtk+ ;; propagates gdk-pixbuf+svg
      libjpeg-turbo
      libpng
      libx11 ;; ?? wayland ??
      mesa
      mpfr
      pango
      unixodbc
      libedit ;; TODO reconsider in light of expeditor and readline-gpl
      racket-minimal ;; <-- TODO non-tethered layer
      racket-vm-cs ;; TODO (racket-vm-for-system)
      (simple-racket-origin
       "2d" (base32 "1zzcz5qyjv7syi41vb8jkxjp1rqgj61zbsdrg0nlc4qy9qsafzgr")
       '("2d" "2d-doc" "2d-lib"))
      (simple-racket-origin
       "algol60" (base32 "09kj6asypmc24n29w0izc9p0q8hpga2hpkchsypfwn5c8zpvihlx")
       '(("algol60" ".")))
      (racket-packages-origin
       "racket" %racket-origin
       '(("at-exp-lib" "pkgs/at-exp-lib")
         ("compiler" "pkgs/compiler")
         ("compiler-lib" "pkgs/compiler-lib")
         ("net" "pkgs/net")
         ("net-doc" "pkgs/net-doc")
         ("net-lib" "pkgs/net-lib")
         ("racket-doc" "pkgs/racket-doc")
         ("racket-index" "pkgs/racket-index")
         ("sandbox-lib" "pkgs/sandbox-lib")
         ("zo-lib" "pkgs/zo-lib")))
      (simple-racket-origin
       "cext-lib" (base32 "00w38jpv88fpl4pgj6ndnysvn0s21rjvj0xhznay80msan0vc341")
       '("cext-lib" "dynext-lib"))
      (simple-racket-origin
       "class-iop" (base32 "08z57q83cr7wnh6g8ah3hdhmsmf9zp1jfs7yvxv188l3hzvygy5l")
       '("class-iop-lib"))
      (simple-racket-origin
       "compatibility" (base32 "0bfqwscjpyi325br5pa6g62g9c8lq18a80zp5g3d2qzn3n3mi6x0")
       '("compatibility" "compatibility-doc" "compatibility-lib"))
      (simple-racket-origin
       "contract-profile" (base32 "1xm2z8g0dpv5d9h2sg680vx1a8ix9gbsdpxxb8qv1w7akp73paj3")
       '(("contract-profile" ".")))
      (simple-racket-origin
       "data" (base32 "10iabgrk9alaggvksnyb0hdq7f1p30pq6pq2bcakvhzpxwiv1f55")
       '("data" "data-doc" "data-enumerate-lib" "data-lib"))
      (simple-racket-origin
       "datalog" (base32 "0n5j5gnqh7g31mvgx19ggl18hirzbvq2r189lbngmnrmbc7b73fp")
       '(("datalog" ".")))
      (simple-racket-origin
       "db" (base32 "1n02ja0yj3mjjhmz0yv04yfhyvrsznbljn8bjviyfxnm4xf9rcc5")
       '("db" "db-doc" "db-lib"))
      (simple-racket-origin
       "deinprogramm" (base32 "1is6fapgv6rxfjz47nh6qf3kh7y7sjdinakaxqffi46gf1al8prd")
       '("deinprogramm" "deinprogramm-signature"))
      (simple-racket-origin
       "distributed-places" (base32 "1dajpkj9balqcpv6cdk9hwjz592h1vq8rrx5vncariiac4vbdpa0")
       '("distributed-places" "distributed-places-doc" "distributed-places-lib"))
      (simple-racket-origin
       "draw" (base32 "1xgjfbh70hqw67z88iqqajg98d04qwbzn6im2wj47rs28jxlm9ly")
       '("draw" "draw-doc" "draw-lib"))
      (simple-racket-origin
       "drracket" (base32 "0m3l4an3nq2ycd1h287s1az2v2zprjbzd8if2x7d5r71vaj4i00c")
       '("drracket"
         "drracket-plugin-lib"
         "drracket-tool"
         "drracket-tool-doc"
         "drracket-tool-lib"
         "drracket-tool-text-lib"))
      (simple-racket-origin
       "ds-store" (base32 "0ajr27kipp4dr1qlisaghsb3h7lhhjwrfw2r79b5myczsa1mp661")
       '("ds-store" "ds-store-doc" "ds-store-lib"))
      (simple-racket-origin
       "eli-tester" (base32 "0icx6wn14gjm8kdmq1jppqgq87sxkras4qb5xmdr6wigxafhjqyk")
       '(("eli-tester"  ".")))
      (simple-racket-origin
       "eopl" (base32 "1fmiixj6rxsgzwvgva8lvrvv0gl49v2405mp3s0i7ipis5c4n27s")
       '(("eopl" ".")))
      (simple-racket-origin
       "errortrace" (base32 "14m7rhaxngj36070iw15am434hm438pfgmwjfsiqhsglz4pcxhip")
       '("errortrace" "errortrace-doc" "errortrace-lib"))
      (simple-racket-origin
       "expeditor" (base32 "07djzxs6307l51mcsk3yr2g4g47ayxa3878g7sf5xhqdr4hd9vxf")
       '("expeditor" "expeditor-doc" "expeditor-lib"))
      (simple-racket-origin
       "frtime" (base32 "0ydz2yn8vvv6z7brwlswcyx0f31a6y6d443i89rysfvd2xkhpfd5")
       '(("frtime" ".")))
      (simple-racket-origin
       "future-visualizer" (base32 "1758qq769m0r14xf64sl2ix2l9z340kvapar0j7s5kdg42lmvnhm")
       '("future-visualizer"
         "future-visualizer-pict"
         "future-visualizer-typed"))
      (simple-racket-origin
       "games" (base32 "0kpn3izlx1ccd0pj0dnvmnrhny51b85xy418a7psj70lz8j8415d")
       '(("games" ".")))
      (racket-packages-origin
       "gui" (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/racket/gui")
                     (commit %racket-commit)))
               (sha256 (base32
                        "1x33jgrx3r32k7hgwr591z3xqv1m2r5nc4km2fnxv0ak2xa0j3gj"))
               (patches
                ;; remove in Racket 8.5
                ;; see https://github.com/racket/racket/issues/4133
                (search-patches "racket-gui-tethered-launcher-backport.patch"))
               (file-name (git-file-name "racket-gui" %racket-version)))
       '("gui" "gui-doc" "gui-lib" "tex-table"))
      (simple-racket-origin
       "gui-pkg-manager" (base32 "1ji9448d723nklqvycwdswj0ni28sabrncag14f9mx47did5myb5")
       '("gui-pkg-manager-lib"))
      (simple-racket-origin
       "htdp" (base32 "0r4ykybcpr10y2db9rlza9pr0xh58nd7ac389mjcxp8g386hgihl")
       '("htdp" "htdp-doc" "htdp-lib"))
      (simple-racket-origin
       "html" (base32 "18n1jnjgzfknc8nv8dppi85nb8q08gqdwkg6hfjk08x0p00anx2x")
       '("html" "html-doc" "html-lib"))
      (simple-racket-origin
       "icons" (base32 "1s5a6j11fg3fdr6b7vm2q7q178d7q8b8igy73bs211r27qrd1gg7")
       '(("icons" ".")))
      (simple-racket-origin
       "images" (base32 "0rpjxqw34bq5m08kh1ldl1mr7s9z1lyydxxcyzb292kqh9qiqvfl")
       '("images" "images-doc" "images-gui-lib" "images-lib"))
      (simple-racket-origin
       "lazy" (base32 "176ylzgbdsbmqknpihaz519afq71pyjkv1h87j5v8jfbpbddyfsf")
       '(("lazy" ".")))
      (simple-racket-origin
       "macro-debugger" (base32 "14hyrwbkffr61fk44l02xb47bhv5zccw0ymaa9kxld86hvyqhqbm")
       '("macro-debugger" "macro-debugger-text-lib"))
      (simple-racket-origin
       "main-distribution" (base32 "0m2n9s32s8a4a2gn4ywrm9l8jycdm5ayi5w9kh5wchhrrw7qzq7y")
       '(("main-distribution" ".")))
      (simple-racket-origin
       "make" (base32 "10852fj30bz5r46c3d99s37fkgy5yh44gb01j29sf3kxnhi0g2sa")
       '(("make" ".")))
      (simple-racket-origin
       "math" (base32 "02sqbnvxvmvslk33b44fx4v93zafcvhva0cx8z21jqbl5wp217ac")
       '("math" "math-doc" "math-lib"))
      (simple-racket-origin
       "mysterx" (base32 "11p9jzrafw0hizhl0cs4sxx7rv281185q8hryic2rpk0kzjdyr48")
       '(("mysterx" ".")))
      (simple-racket-origin
       "mzcom" (base32 "0rc9pfj7gwm5azghqvcibz6si1x5s2v8mr2yngk7ssq9gzfbi6a4")
       '(("mzcom" ".")))
      (simple-racket-origin
       "mzscheme" (base32 "192c52zi726h5wjamxrhivjw2waq1im0zpyxhbrkrxknm8x84bs9")
       '("mzscheme" "mzscheme-doc" "mzscheme-lib"))
      (racket-packages-origin
       "net-cookies" (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/RenaissanceBug/racket-cookies")
                             (commit %racket-commit)))
                       (sha256 (base32
                                "0k0hifxhywl5c3hjcaiizc098dpyk001d981p572gly116yvjxc1"))
                       (file-name
                        (git-file-name "RenaissanceBug-racket-cookies" %racket-version)))
       '("net-cookies" "net-cookies-doc" "net-cookies-lib"))
      (racket-packages-origin
       "optimization-coach"
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stamourv/optimization-coach")
               (commit %racket-commit)))
         (sha256 (base32
                  "0b27sw48d7rhz0hin88c7rbr9vpg1c23sn82nd4jkmq54h6gasr1"))
         (file-name
          (git-file-name "stamourv-optimization-coach" %racket-version)))
       '(("optimization-coach" ".")))
      (simple-racket-origin
       "option-contract" (base32 "026b7n5l0c3024nymshz8zp1yhn493rdzgpflzfd52hj7awafqhk")
       '("option-contract" "option-contract-doc" "option-contract-lib"))
      (simple-racket-origin
       "parser-tools" (base32 "08pvz4zramirzm3j64hbhjm0mmh5zfy37iv4s3vmq0rj49cr8fl3")
       '("parser-tools" "parser-tools-doc" "parser-tools-lib"))
      (simple-racket-origin
       "pconvert" (base32 "00czi0p399mmyrvxyrs5kniizpkqfxyz2ncxqi2jy79a7wk79pb1")
       '("pconvert-lib"))
      (simple-racket-origin
       "pict" (base32 "0g1iwdr6qh1xb0crhj96830vjjnbds409xbpqn7j5sh0ksy6vr5x")
       '("pict" "pict-doc" "pict-lib"))
      (simple-racket-origin
       "pict-snip" (base32 "081nwiy4a0n4f7xws16hqbhf0j3kz5alizndi3nnyr3chm4kng6x")
       '("pict-snip" "pict-snip-doc" "pict-snip-lib"))
      (simple-racket-origin
       "picturing-programs" (base32 "1g6xr39hx1j03gb3d4dljm3v91xcj2gfpq3dgy5xvplzr6cmmxgr")
       '(("picturing-programs" ".")))
      (simple-racket-origin
       "plai" (base32 "0i983sh0r0zm2ng4j44m5aw9669kh5fhp91bzpc9jm280rfcqvyl")
       '("plai" "plai-doc" "plai-lib"))
      (simple-racket-origin
       "planet" (base32 "0r2yqrzrmdjjyr14k6hhlzc5kzrcx3583m1s02mhrcmpfw0s85w9")
       '("planet" "planet-doc" "planet-lib"))
      (simple-racket-origin
       "plot" (base32 "07kq32si34ybcwz8idxxcrzssg8diyrp1nfgkcj0mmvr45321zm7")
       '("plot" "plot-compat" "plot-doc" "plot-gui-lib" "plot-lib"))
      (simple-racket-origin
       "preprocessor" (base32 "1p5aid58ifnjy4xl0ysh85cq39k25661v975jrpk182z3k5621mg")
       '(("preprocessor" ".")))
      (simple-racket-origin
       "profile" (base32 "179i86lyby29nywz60l4vnadi02w8b12h7501nm5h5g4pq9jjmbb")
       '("profile" "profile-doc" "profile-lib"))
      (racket-packages-origin
       "quickscript" (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/Metaxal/quickscript")
                             (commit %racket-commit)))
                       (sha256 (base32
                                "100g3yqhbjdq06b6l6d72ywsw29awgy8crqg33wj7h12xq07nzcr"))
                       (file-name (git-file-name "Metaxal-quickscript" %racket-version)))
       '(("quickscript" ".")))
      (simple-racket-origin
       "r5rs" (base32 "1g3cysj7z88r38vkzvi8g2fb2hn4yg1fdhy5smxw303jxgl3inp6")
       '("r5rs" "r5rs-doc" "r5rs-lib"))
      (simple-racket-origin
       "r6rs" (base32 "0b1ymzdp10r0flw2acbidjsh5ma1pm5hy54jss37sxf89z3xbvm4")
       '("r6rs" "r6rs-doc" "r6rs-lib"))
      (racket-packages-origin
       "racket-cheat" (origin
                        (method git-fetch)
                        (uri (git-reference
                              (url "https://github.com/jeapostrophe/racket-cheat")
                              (commit %racket-commit)))
                        (sha256 (base32
                                 "06wcj558rzkbl2bwkmikyspya9v1f4iwlzwnwxpkc33h2xapwabr"))
                        (file-name
                         (git-file-name "jeapostrophe-racket-cheat" %racket-version)))
       '(("racket-cheat" ".")))
      (simple-racket-origin
       "racklog" (base32 "1rgrvwy3kr9b9w5cghsffiv3ly00yfvvzr5xaaw83g1w7yin0mnb")
       '(("racklog" ".")))
      (simple-racket-origin
       "rackunit" (base32 "057z31rja6h3nabh5b2xgwfrzmlm6h1cv1qcgf3xfy4g2q5dqn5p")
       '("rackunit"
         "rackunit-doc"
         "rackunit-gui"
         "rackunit-lib"
         "rackunit-plugin-lib"
         "rackunit-typed"
         "schemeunit"
         "testing-util-lib"))
      (simple-racket-origin
       "readline" (base32 "13kbcn2wchv82d709mw3r8n37bk8iwq0y4kpvm9dbzx0w2pxkfwn")
       '("readline" "readline-doc" "readline-lib"))
      (simple-racket-origin
       "realm" (base32 "0hxcgla08iack54j8v40fj51811chpy66ym2zq76zb52c7kzn0hi")
       '(("realm" ".")))
      (simple-racket-origin
       "redex" (base32 "0vlgxbnbgrlihk1hh5zd6hsc4566ldi4q76f87z5vai54dxkwy2f")
       '("redex"
         "redex-benchmark"
         "redex-doc"
         "redex-examples"
         "redex-gui-lib"
         "redex-lib"
         "redex-pict-lib"))
      (simple-racket-origin
       "sasl" (base32 "0ibh4wb4gn8pggx6gkv4vk4d6rwzn5nrvjibhvkzhaynf6lhb824")
       '("sasl" "sasl-doc" "sasl-lib"))
      (simple-racket-origin
       "scheme-lib" (base32 "0pcf0y8rp4qyjhaz5ww5sr5diq0wpcdfrrnask7zapyklzx1jx8x")
       '(("scheme-lib" ".")))
      (simple-racket-origin
       "scribble" (base32 "0rgvnsykrxkah6s5fw1vyp9lxsb4z9w6hgwk5j6wbwjp2gsfczbm")
       '("scribble"
         "scribble-doc"
         "scribble-html-lib"
         "scribble-lib"
         "scribble-text-lib"))
      (simple-racket-origin
       "serialize-cstruct-lib"
       (base32 "1rq3n1fa7ldjwx3lrh9ybhig7jlsw1crpzyklbzp3xqdw6jymfnz")
       '(("serialize-cstruct-lib" ".")))
      (simple-racket-origin
       "sgl" (base32 "0nkymhdyjrwi5h199j4w5zh7y3x3ai42gsiwxzh0hy7yqrqqg9zv")
       '(("sgl" ".")))
      (simple-racket-origin
       "shell-completion" (base32 "04m144gy2mp4fiq6rcbf12wjr8mws8k9scfhg9lc38vqppp4lxsj")
       '(("shell-completion" ".")))
      (simple-racket-origin
       "simple-tree-text-markup"
       (base32 "0fyd9gfz6bnv0m1901wv5mnhc05rm8hw9i6ddrqx33hs6qsg2zqr")
       '("simple-tree-text-markup"
         "simple-tree-text-markup-doc"
         "simple-tree-text-markup-lib"))
      (simple-racket-origin
       "slatex" (base32 "0pkm2isbbdk63slrbsxcql7rr0wdrw5kapw1xq4ps5k8dhlzv8x0")
       '(("slatex" ".")))
      (simple-racket-origin
       "slideshow" (base32 "1znv1i2d0610hhy71q932xy7wka00q3q50in1xfnk8ibg7nzkagm")
       '("slideshow" "slideshow-doc" "slideshow-exe" "slideshow-lib" "slideshow-plugin"))
      (simple-racket-origin
       "snip" (base32 "01r9wc5xr3q3n4yyif6j0a37rgdzmpslxn05k13ksik73b3wj6hj")
       '("snip" "snip-lib"))
      (simple-racket-origin
       "typed-racket" (base32 "1462kj9yswsxbnw71casylzlvhd7cxrml2v9j7rcsnn9hmrqx4vv")
       '("source-syntax"
         "typed-racket"
         "typed-racket-compatibility"
         "typed-racket-doc"
         "typed-racket-lib"
         "typed-racket-more"))
      (racket-packages-origin
       "srfi" (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/racket/srfi")
                      ;; Includes an FSDG fix: return to %racket-commit in 8.5.
                      ;; See <https://github.com/racket/srfi/pull/15>.
                      (commit "7243029b135741ce08ae30f877e2f49a2a460b22")))
                (sha256 (base32
                         "0aqbcdv2dfc2xnk0h6zfi56p7bpwqji8s88qds3d03hhh9k28gvn"))
                ;; Use the relevant version for srfi-doc and srfi-lib,
                ;; since we're using a newer commit than the v8.4 tag.
                (file-name (git-file-name "racket-srfi" "1.1")))
       '("srfi" "srfi-doc" "srfi-lib" "srfi-lite-lib"))
      (simple-racket-origin
       "string-constants" (base32 "1qizjq4n0hzdgdcjjpr94464gsywpsk2g9mnvwzqr7dcqbrsfvn6")
       '("string-constants" "string-constants-doc" "string-constants-lib"))
      (simple-racket-origin
       "swindle" (base32 "164gdsphjzdl2vv7zxz7dfk9jwax8njpmim6sidm8qz8a8589y67")
       '(("swindle" ".")))
      (simple-racket-origin
       "syntax-color" (base32 "1vf2fc3qvx8a1igi7swsg8gaqhx786sa0vqxd18xhbsidfgb5ywp")
       '("syntax-color" "syntax-color-doc" "syntax-color-lib"))
      (simple-racket-origin
       "trace" (base32 "070ihla5j796hdarn5wxdwn4xj0xnkm50shgh49jy994mribvhia")
       '(("trace" ".")))
      (simple-racket-origin
       "unix-socket" (base32 "02dfwas5ynbpyz74w9kwb4wgb37y5wys7svrlmir8k0n9ph9vq0y")
       '("unix-socket" "unix-socket-doc" "unix-socket-lib"))
      (simple-racket-origin
       "web-server" (base32 "1zgb6jl7zx6258ljs8f3lvryrq5n5zpd71dqzr698m92kw3x2pkn")
       '("web-server" "web-server-doc" "web-server-lib"))
      (simple-racket-origin
       "wxme" (base32 "1qp5gr9gqsakiq3alw6m4yyv5vw4i3hp4y4nhq8vl2nkjmirvn0b")
       '("wxme" "wxme-lib"))
      (simple-racket-origin
       "xrepl" (base32 "12zjgsy5zqm3fck3ihg4a70wj56s2cnnjyb4jlfi5nnsfqyrnxg3")
       '("xrepl" "xrepl-doc" "xrepl-lib"))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments racket-minimal)
       ((#:make-flags _ '())
        #~`("main-distribution"))
       ((#:configure-flags _ '())
        #~`("--tethered"
            "--extra-foreign-lib-search-dirs"
            ,(format #f "~s"
                     '(#$@(map (lambda (name)
                                 (cond
                                  ((this-package-input name)
                                   => (cut file-append <> "/lib"))
                                  (else
                                   (raise-exception
                                    (make-exception
                                     (make-assertion-failure)
                                     (make-exception-with-message
                                      "missing input to the 'racket' package")
                                     (make-exception-with-irritants
                                      (list name)))))))
                               '("cairo"
                                 "fontconfig-minimal" ;; aka fontconfig
                                 "glib"
                                 "glu"
                                 "gmp"
                                 "gtk+"
                                 "libjpeg-turbo"
                                 "libpng"
                                 "libx11"
                                 "mesa"
                                 "mpfr"
                                 "pango"
                                 "unixodbc"
                                 "libedit"))))))))
    (synopsis "Programmable programming language in the Scheme family")
    (description
     "Racket is a general-purpose programming language in the Scheme family,
with a large set of libraries and a compiler based on Chez Scheme.  Racket is
also a platform for language-oriented programming, from small domain-specific
languages to complete language implementations.

The main Racket distribution comes with many bundled packages, including the
DrRacket IDE, libraries for GUI and web programming, and implementations of
languages such as Typed Racket, R5RS and R6RS Scheme, Algol 60, and Datalog.")))

(define configure-layer.rkt
  (scheme-file
   "configure-layer.rkt"
   `(module
     configure-layer racket/base
     (require racket/cmdline
              racket/match
              racket/file
              racket/port
              racket/list
              racket/pretty)
     (define (build-path-string . args)
       (path->string (apply build-path args)))
     (define rx:racket
       ;; Guile's reader doesn't support #rx"racket"
       (regexp "racket"))
     (define tethered? #f)
     (define parent #f)
     (define extra-foreign-lib-search-dirs '())
     (define-values [vm-dir prefix]
       (command-line
        #:once-each
        [("--tethered") "create a tethered layer"
         (set! tethered? #t)]
        [("--parent") dir "path of parent layer, if any"
         (set! parent dir)]
        [("--extra-foreign-lib-search-dirs") dir-list
         "foreign library directories, as a list of strings in `read` syntax"
         (set! extra-foreign-lib-search-dirs
               (call-with-input-string dir-list read))]
        #:args (vm-dir prefix)
        (values vm-dir prefix)))
     (let* ([config
             (for/fold
              ([config (file->value
                        (if parent
                            (build-path parent "etc/racket/config.rktd")
                            (build-path vm-dir "etc/config.rktd")))])
              ([spec
                (in-list
                 '((lib-dir lib-search-dirs "lib/racket" "lib")
                   (share-dir share-search-dirs "share/racket" "share")
                   (links-file links-search-files
                               "lib/racket/links.rktd"
                               "share/links.rktd")
                   (pkgs-dir pkgs-search-dirs "lib/racket/pkgs" "share/pkgs")
                   ;; Partial workaround for:
                   ;; https://github.com/racket/racket/issues/4133
                   #;(bin-dir bin-search-dirs "bin" "bin")
                   (bin-dir bin-search-dirs
                            "lib/racket/bogus-untethered-bin"
                            "bin")
                   (man-dir man-search-dirs "share/man" "share/man")
                   (doc-dir doc-search-dirs "share/doc/racket" "doc")
                   (include-dir include-search-dirs
                                "include/racket"
                                "include")))])
              (match-define (list main-key search-key pth vm-pth) spec)
              (hash-set*
               config
               main-key
               (build-path-string prefix pth)
               search-key
               (list* #f
                      (hash-ref config
                                main-key
                                (lambda ()
                                  (if parent
                                      (build-path-string parent pth)
                                      (build-path-string vm-dir vm-pth))))
                      (filter values (hash-ref config search-key null)))))]
            [config
             (hash-update config
                          'lib-search-dirs
                          (lambda (dirs)
                            ;; add after other layers, but before older
                            ;; foreign lib search directories
                            (define-values [rkt old-foreign-dirs]
                              (partition (lambda (pth)
                                           (or (not pth)
                                               (regexp-match? rx:racket pth)))
                                         dirs))
                            (append rkt
                                    extra-foreign-lib-search-dirs
                                    old-foreign-dirs)))]
            [config
             (hash-set* config
                        'apps-dir
                        (build-path-string prefix "share/applications")
                        'absolute-installation? #t
                        ;; Let Guix coexist with other installation
                        ;; methods without clobbering user-specific packages.
                        ;; This could be set in various places, but doing
                        ;; it here is convienient, at least until we support
                        ;; cross-compilation.
                        'installation-name
                        (string-append (version)
                                       "-guix"
                                       (match (system-type 'gc)
                                         ['cgc "-cgc"]
                                         ;; workaround Guile reader/printer:
                                         ['|3m| "-bc"]
                                         [_ ""])))]
            [config
             (cond
              [tethered?
               ;; Partial workaround for:
               ;; https://github.com/racket/racket/issues/4133
               #;(define bin-dir (hash-ref config 'bin-dir))
               (define bin-dir (build-path-string prefix "bin"))
               (hash-set* config
                          'config-tethered-apps-dir (hash-ref config 'apps-dir)
                          'config-tethered-console-bin-dir bin-dir
                          'config-tethered-gui-bin-dir bin-dir)]
              [else
               config])])
       (define new-config-pth
         (build-path prefix "etc/racket/config.rktd"))
       (make-parent-directory* new-config-pth)
       (call-with-output-file*
        new-config-pth
        (lambda (out)
          (pretty-write config out)))))))
