;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2016, 2018, 2020-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2021-2025 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module ((guix search-paths) #:select ($SSL_CERT_DIR $SSL_CERT_FILE))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
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

;; COMMENTARY:
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
;;             ├── rktboot/
;;             ├── zuo/
;;             └── ...
;;
;; The 'racket/src/' directory contains the source of the runtime system, core
;; compiler, and primitives for the major Racket implementations: this layer
;; is called the ``Racket VM''.  It is basically a normal Autoconf project,
;; except that the makefiles just run Zuo to do the real work. (Even when
;; Racket VM implementations use components implemented in Racket, they are
;; compiled in special modes to produce VM primitives.) (There are or have
;; been experimental Racket VM implementations elsewhere,
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
;; 'distro-build' package to assemble custom Racket distributions. (Again,
;; the makefile just delegates to Zuo.) It is not part of Racket source
;; distributions: the root of a source distribution is basically 'racket/src'
;; with some extra package sources and configuration added. In fact, the
;; top-level 'Makefile' and the 'distro-build' package are what create Racket
;; source distributions.
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
;; output.
;;
;; Using 'racket-vm-cs', we then define the packages 'racket-minimal' and
;; 'racket'. These use Racket's support for ``layered installations'', which
;; allow an immutable base layer to be extended with additional packages.
;; They use the layer configuration directly provide ready-to-install FHS-like
;; trees, rather than relying on the built in ``Unix-style install''
;; mechanism.
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
;;   - Racket CS needs (boot files for) the corresponding version of Chez
;;     Scheme. It also needs an existing Racket to compile Racket-implemented
;;     parts of the runtime system to R6RS libraries.
;;   - Chez Scheme also needs boot files for itself, but Racket BC can
;;     bootstrap these using the code in "racket/src/rktboot/".
;;     See the commentary in "chez.scm" for further details
;;
;; So, we build CGC to build 3M to build bootfiles and CS.
;;
;; (Note: since the CGC variant is basically only for bootstrapping, we
;; often use "BC" to mean "3M", consistent with `(banner)` and the
;; suffixes used on executables when more than one variant co-exists.)
;;
;; Since the pre-releases for Chez Scheme 10.0.0, all of Racket's changes have
;; been merged upstream, and development will be kept in sync going
;; forward. However, there is no plan to align the Chez Scheme and Racket
;; release cycles. For the near fulture, a given released version of Racket
;; will continue to depend on a specific pre-release version of Chez Scheme as
;; part of Racket CS's "ABI". See upstream discussion at
;; <https://racket.discourse.group/t/2739/3>.
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
;; as of Racket 7.0. See also <https://racket.discourse.group/t/951/4>.
;;
;; For Racket BC, the compiled "linklet" s-expressions (primitive modules)
;; are embedded in C as a static string constant. Eventually, they are further
;; compiled by the C-implemented Racket BC bytecode and JIT compilers.
;;
;; Zuo is notably *not* a problem for bootstrapping. The implementation is a
;; single hand-written C file designed to build with just `cc -o zuo zuo.c`,
;; even with very old or limited compilers. (We use the Autoconf support for
;; convienience.) As of Zuo 1.8, Zuo has tagged releases in its own repository
;; independent of the Racket release cycle.
;;
;; CODE:

(define %racket-version "8.17") ; Remember to update chez-scheme-for-racket!
(define %racket-commit
  (string-append "v" %racket-version))
(define %racket-origin
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/racket/racket")
          (commit %racket-commit)))
    (sha256
     (base32 "1zbpsa6nrp2mzflylag250dj8b937y1ivs7kgx99bix92kh8zcz2"))
    (file-name (git-file-name "racket" %racket-version))
    (patches (search-patches "racket-chez-scheme-bin-sh.patch"
                             "racket-launcher-config-dir.patch"
                             "racket-rktio-bin-sh.patch"))
    (modules '((guix build utils)))
    (snippet
     #~(begin
         (use-modules (guix build utils))
         ;; Unbundle Chez submodules and boot files.
         (with-directory-excursion "racket/src/ChezScheme"
           ;; TODO: consider putting this in a (guix ...) or (guix build ...)
           ;; module so it can be shared with the upstream Chez Scheme origin
           ;; without cyclic issues.
           (for-each (lambda (dir)
                       (when (directory-exists? dir)
                         (delete-file-recursively dir)))
                     '("boot"
                       "lz4"
                       "nanopass"
                       "stex"
                       "zlib"
                       "zuo")))
         ;; Unbundle Zuo.
         (delete-file-recursively "racket/src/zuo")
         ;; Unbundle libffi.
         (delete-file-recursively "racket/src/bc/foreign/libffi")))))


(define-public zuo
  (package
    (name "zuo")
    (version "1.12") ; defined in racket/src/zuo/zuo.c or the following
    #;(displayln (~a (hash-ref (runtime-env) 'version) "."
                     (hash-ref (runtime-env) 'minor-version)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/racket/zuo")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "053zn230v4bgfrf7clrqcp5d614fh4sv00hr5qw3vdvnl006shh5"))
              (file-name (git-file-name name version))
              (patches (search-patches "zuo-bin-sh.patch"))))
    (outputs '("out" "debug"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:out-of-source? #t))
    (home-page "https://github.com/racket/zuo")
    (synopsis "Tiny Racket for build scripts")
    (description "Zuo is a tiny Racket with primitives for dealing
with files and running processes.  It comes with a @command{make}-like
embedded DSL, which is used to build Racket itself.

Zuo is a Racket variant in the sense that program files start with
@code{#lang}, and the module path after @code{#lang} determines the parsing
and expansion of the file content.  That's how the @command{make}-like DSL is
defined, and even the base Zuo language is defined by layers of @code{#lang}s.
One of the early layers implements macros.")
    (license (list license:asl2.0 license:expat))))


(define racket-vm-common-configure-flags
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
      "CFLAGS=-g -O2 -Wno-error=implicit-function-declaration"
      "--disable-strip"
      ;; Using --enable-origtree lets us distinguish the VM from subsequent
      ;; layers and produces a build with the shape expected by tools such as
      ;; "distro-build" and "raco-cross". Removing these flags would require
      ;; changes, especially to 'configure-layer.rkt' (defined below).
      "--enable-origtree"
      ,(string-append "--prefix=" #$output "/opt/racket-vm")))

(define-public racket-vm-cgc
  ;; Eventually, it may make sense for some vm packages to not be hidden,
  ;; but this one is especially likely to remain hidden.
  (hidden-package
   (package
     (name "racket-vm-cgc")
     (version %racket-version)
     (source %racket-origin)
     (inputs (list ncurses ;; <- common to all variants (for #%terminal)
                   libffi)) ;; <- for BC and non-native CS variants
     (native-inputs (cons* zuo ;; <- for all variants
                           libtool ;; <- only for BC variants
                           (if (%current-target-system)
                               (list this-package)
                               '())))
     (outputs '("out" "debug"))
     (build-system gnu-build-system)
     (arguments
      (list
       #:configure-flags
       #~(cons "--enable-cgcdefault"
               #$racket-vm-common-configure-flags)
       #:make-flags
       #~(list (string-append "ZUO="
                              #+(this-package-native-input "zuo")
                              "/bin/zuo"))
       ;; Tests are in packages like racket-test-core and
       ;; main-distribution-test that aren't part of the main
       ;; distribution.
       #:tests? #f
       ;; Upstream recommends #:out-of-source?, and it
       ;; helps a lot with debugging.
       #:out-of-source? #t
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
               (chdir "racket/src")))
           (replace 'install-license-files
             ;; The #:out-of-source? mode for install-license-files fails
             ;; to find the srcdir: as a workaround, navigate there ourselves.
             (let ((install-license-files
                    (assoc-ref %standard-phases 'install-license-files)))
               (lambda args
                 (with-directory-excursion "../src"
                   (apply install-license-files
                          `(,@args
                            ;; if there are duplicate keywords, last is used
                            #:out-of-source? #f)))))))))
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
     (if (%current-target-system)
         (package-native-inputs racket-vm-cgc)
         (modify-inputs (package-native-inputs racket-vm-cgc)
           (prepend racket-vm-cgc))))
    (arguments
     (substitute-keyword-arguments (package-arguments racket-vm-cgc)
       ((#:configure-flags _ '())
        #~(cons "--enable-bconly"
                #$(cond
                   ((target-ppc64le?)
                    ;; Attempt to avoid a problem bootstrapping Chez Scheme:
                    ;; see <https://issues.guix.gnu.org/57050#19>
                    ;; and <https://racket.discourse.group/t/950/30>.
                    #~(map
                       (lambda (flag)
                         (if (string-prefix? "CPPFLAGS=" flag)
                             (string-append flag
                                            " -DSTACK_SAFETY_MARGIN=2000000")
                             flag))
                       #$racket-vm-common-configure-flags))
                   (else
                    racket-vm-common-configure-flags))))))
    (synopsis "Racket BC [3M] implementation")
    (description "The Racket BC (``before Chez'' or ``bytecode'')
implementation was the default before Racket 8.0.  It uses a compiler written
in C targeting architecture-independent bytecode, plus a JIT compiler on most
platforms.  Racket BC has a different C API than the current default runtime
system, Racket CS (based on ``Chez Scheme'').

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
     (let ((inputs (modify-inputs (package-inputs racket-vm-cgc)
                     (prepend zlib lz4))))
       (if (nix-system->native-chez-machine-type)
           (modify-inputs inputs
             (delete "libffi"))
           inputs)))
    (native-inputs
     (let ((native-inputs (package-native-inputs racket-vm-cgc)))
       (modify-inputs (if (%current-target-system)
                          (modify-inputs native-inputs
                            (prepend this-package)
                            (delete "racket-vm-cgc"))
                          native-inputs)
         (delete "libtool")
         (prepend chez-scheme-for-racket
                  chez-nanopass-bootstrap))))
    (arguments
     (substitute-keyword-arguments (package-arguments racket-vm-cgc)
       ((#:phases those-phases #~%standard-phases)
        #~(modify-phases #$those-phases
            (add-after 'unpack 'unpack-nanopass
              #$unpack-nanopass)))
       ((#:configure-flags _ '())
        #~(cons* "--enable-csonly"
                 "--enable-libz"
                 "--enable-lz4"
                 (string-append "--enable-scheme="
                                #+(this-package-native-input
                                   "chez-scheme-for-racket")
                                "/bin/scheme")
                 #$@(if (nix-system->native-chez-machine-type)
                        #~()
                        #~(#$(string-append "--enable-mach="
                                            (nix-system->pbarch-machine-type))
                           "--enable-pb"))
                 #$racket-vm-common-configure-flags))))
    (synopsis "Racket CS implementation")
    (description "The Racket CS implementation, which uses ``Chez Scheme'' as
its core compiler and runtime system, has been the default Racket VM
implementation since Racket 8.0.  It performs better than the Racket BC
implementation for most programs.  On systems for which Racket CS cannot
generate machine code, this package uses a variant of its ``portable
bytecode'' backend specialized for word size and endianness.

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
           racket-vm-cs
           (racket-packages-origin
            "base" %racket-origin
            '(("base" "pkgs/base")
              ("racket-lib" "pkgs/racket-lib")))))
    (native-search-paths (list $SSL_CERT_DIR $SSL_CERT_FILE))
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
                           ,@%default-gnu-imported-modules)
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
      racket-vm-cs
      (simple-racket-origin
       "2d" (base32 "0fb5v6058ls08xw3zbmqyr2ym0psm119gl9ffgmhm9w8rs9i4dq7")
       '("2d" "2d-doc" "2d-lib"))
      (simple-racket-origin
       "algol60" (base32 "03akd7xhn4l7y66qgaimvdbn6gq7ay6j03dc11mz80n06z21dfb6")
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
         ("scheme-doc" "pkgs/scheme-doc")
         ("sandbox-lib" "pkgs/sandbox-lib")
         ("sequence-tools-lib" "pkgs/sequence-tools-lib")
         ("zo-lib" "pkgs/zo-lib")))
      (simple-racket-origin
       "cext-lib" (base32 "01dirj5nq9s384xqpzh1p07la38vcycsim0k1ls04a52sgyglgwc")
       '("cext-lib" "dynext-lib"))
      (simple-racket-origin
       "class-iop" (base32 "08z57q83cr7wnh6g8ah3hdhmsmf9zp1jfs7yvxv188l3hzvygy5l")
       '("class-iop-lib"))
      (simple-racket-origin
       "compatibility" (base32 "1d1py9wizi4jnzak7b6d0p9filxv1x9mlx5gsp3yhcjajigq182p")
       '("compatibility" "compatibility-doc" "compatibility-lib"))
      (simple-racket-origin
       "contract-profile" (base32 "1xm2z8g0dpv5d9h2sg680vx1a8ix9gbsdpxxb8qv1w7akp73paj3")
       '(("contract-profile" ".")))
      (simple-racket-origin
       "data" (base32 "11ai6c9h0brbblc6xn045ajj4gyrvbjpk1pqcqm1m0dy9q2rzd4a")
       '("data" "data-doc" "data-enumerate-lib" "data-lib"))
      (simple-racket-origin
       "datalog" (base32 "0nf6cy4djpyhfvgpa6yn72apbz9s83gp0qg95pzjd0az4v6qwq1s")
       '(("datalog" ".")))
      (simple-racket-origin
       "db" (base32 "0ijiha9cmwa61srrzr0g9rvxplfih9pbmrw619vj3hgzka2z0myz")
       '("db" "db-doc" "db-lib"))
      (simple-racket-origin
       "deinprogramm" (base32 "0wwv5zphc3sm5n0vv8v1hlbkf85m1m2c5kkf13ll1awc6yv8z1kv")
       '("deinprogramm"
         "deinprogramm-doc"
         "deinprogramm-lib"
         "deinprogramm-signature"
         "deinprogramm-tool"))
      (simple-racket-origin
       "distributed-places" (base32 "1dajpkj9balqcpv6cdk9hwjz592h1vq8rrx5vncariiac4vbdpa0")
       '("distributed-places" "distributed-places-doc" "distributed-places-lib"))
      (simple-racket-origin
       "draw" (base32 "0f980ggciy09y1adm2a428p1vpgp20m7zrhqwqvbqy2n6mnvxnxd")
       '("draw" "draw-doc" "draw-lib"))
      (simple-racket-origin
       "drracket" (base32 "11shkj39bfjr8bkabdlw757f31q7znbjgph49zfm0pndq4zmgbkd")
       '("drracket"
         "drracket-core"
         "drracket-core-lib"
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
       "eopl" (base32 "0qihajdpwf2q7hbs6fsw6mmmzy57kgzgry3vyp9z8w7ay8df3w15")
       '(("eopl" ".")))
      (simple-racket-origin
       "errortrace" (base32 "0l9k2rx6z4jkid7kxhbs763s66pxbzvfrgxajykbrmqkv77lhb3w")
       '("errortrace" "errortrace-doc" "errortrace-lib"))
      (simple-racket-origin
       "expeditor" (base32 "0dyxsg727wsrni4mjaj72q7r6l8q9frc0hh3axc03y9j44andx86")
       '("expeditor" "expeditor-doc" "expeditor-lib"))
      (simple-racket-origin
       "frtime" (base32 "0ydz2yn8vvv6z7brwlswcyx0f31a6y6d443i89rysfvd2xkhpfd5")
       '(("frtime" ".")))
      (simple-racket-origin
       "future-visualizer" (base32 "0yzfs4ls5h9224wnssm21mm6dig40r9k6zh66sbi1vrlwyswpym0")
       '("future-visualizer"
         "future-visualizer-pict"
         "future-visualizer-typed"))
      (simple-racket-origin
       "games" (base32 "13z7fnbr48s98lmfxc0nbfhbqkd4hphymy2r63hqm783xzn6ylzi")
       '(("games" ".")))
      (simple-racket-origin
       "gui" (base32 "12fqk1pc9x1hmlrbzh5afcm4xmk0m5jqydixzibcp1q211san310")
       '("gui" "gui-doc" "gui-lib" "tex-table"))
      (simple-racket-origin
       "gui-pkg-manager" (base32 "024880k3wmvkqhjc954prrkavwv3bgafcw00ajz7h2llygh0aqpl")
       '("gui-pkg-manager-lib"))
      (simple-racket-origin
       "htdp" (base32 "1y1sf3k704q0p25bxgqnplzpm4hfcy8czgfcf1zakqa287w693d9")
       '("htdp" "htdp-doc" "htdp-lib"))
      (simple-racket-origin
       "html" (base32 "18n1jnjgzfknc8nv8dppi85nb8q08gqdwkg6hfjk08x0p00anx2x")
       '("html" "html-doc" "html-lib"))
      (simple-racket-origin
       "icons" (base32 "1s5a6j11fg3fdr6b7vm2q7q178d7q8b8igy73bs211r27qrd1gg7")
       '(("icons" ".")))
      (simple-racket-origin
       "images" (base32 "0ckg53nzq4zdv8qc4f5nim9wsgsnif56w1wmji5w2czfm0j7frln")
       '("images" "images-doc" "images-gui-lib" "images-lib"))
      (simple-racket-origin
       "lazy" (base32 "0rn8kd0ih7aw6fj1g7jdvi5978a6q4hyd6gbh89nb6j5ijvlgn43")
       '(("lazy" ".")))
      (simple-racket-origin
       "macro-debugger" (base32 "133b74pxavbafcsmp34kgvwmx1l6xx4bykzvgwwjg3kg2fa5bg1g")
       '("macro-debugger" "macro-debugger-text-lib"))
      (simple-racket-origin
       "main-distribution" (base32 "0m2n9s32s8a4a2gn4ywrm9l8jycdm5ayi5w9kh5wchhrrw7qzq7y")
       '(("main-distribution" ".")))
      (simple-racket-origin
       "make" (base32 "10852fj30bz5r46c3d99s37fkgy5yh44gb01j29sf3kxnhi0g2sa")
       '(("make" ".")))
      (simple-racket-origin
       "math" (base32 "1xlqbwnsbyc62dfmnn9whccq3fr3iwkvl0y4ykzrkzf6a74j75jc")
       '("math" "math-doc" "math-lib"))
      (simple-racket-origin
       "mysterx" (base32 "11p9jzrafw0hizhl0cs4sxx7rv281185q8hryic2rpk0kzjdyr48")
       '(("mysterx" ".")))
      (simple-racket-origin
       "mzcom" (base32 "0rc9pfj7gwm5azghqvcibz6si1x5s2v8mr2yngk7ssq9gzfbi6a4")
       '(("mzcom" ".")))
      (simple-racket-origin
       "mzscheme" (base32 "04kyrhznq46mklacf7pms4fv47zsimqqk3948r3w2ym9n04fhjsx")
       '("mzscheme" "mzscheme-doc" "mzscheme-lib"))
      (racket-packages-origin
       "net-cookies" (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/RenaissanceBug/racket-cookies")
                             (commit %racket-commit)))
                       (sha256 (base32
                                "1fbl717w5801bydx3nzwxddd1rh1cc0gxwpjc7ka9zh5ak09sb0w"))
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
       "option-contract" (base32 "1914dcxy6qzpg3cpnzdm9lj0kwyw8xdb85mwiamqfkbg0xlyyhpa")
       '("option-contract" "option-contract-doc" "option-contract-lib"))
      (simple-racket-origin
       "parser-tools" (base32 "0i822p7ik6q03l7mrlf0qb63xdxvx06yl5lg1g1qri5wmi5xj0pb")
       '("parser-tools" "parser-tools-doc" "parser-tools-lib"))
      (simple-racket-origin
       "pconvert" (base32 "0xw9f9692v2qp16fwmwmwmrb118kpz259vmx8cq6s9vsw0112zpq")
       '("pconvert-lib" "pconvert-doc"))
      (simple-racket-origin
       "pict" (base32 "0xy5hf12wds4gvx29pgcd9z4a60q6pfnl0qw5gphcplhg2bzm4rr")
       '("pict" "pict-doc" "pict-lib"))
      (simple-racket-origin
       "pict-snip" (base32 "081nwiy4a0n4f7xws16hqbhf0j3kz5alizndi3nnyr3chm4kng6x")
       '("pict-snip" "pict-snip-doc" "pict-snip-lib"))
      (simple-racket-origin
       "picturing-programs" (base32 "1a3d6nw9n827b4711nvgrnw7w47k4fxvk8c2a8br6a1l748kxkwy")
       '(("picturing-programs" ".")))
      (simple-racket-origin
       "plai" (base32 "1vcplmrzk2wr0n0m4fa0nq9ir986d5nk3dvcd7h1rv2jvhkcqdaq")
       '("plai" "plai-doc" "plai-lib"))
      (simple-racket-origin
       "planet" (base32 "0r2yqrzrmdjjyr14k6hhlzc5kzrcx3583m1s02mhrcmpfw0s85w9")
       '("planet" "planet-doc" "planet-lib"))
      (simple-racket-origin
       "plot" (base32 "0skmir4njxmh3yx3ng23dvwkal79dd3fd23ijk6lyk8b013w1kj2")
       '("plot" "plot-compat" "plot-doc" "plot-gui-lib" "plot-lib"))
      (simple-racket-origin
       "preprocessor" (base32 "1p5aid58ifnjy4xl0ysh85cq39k25661v975jrpk182z3k5621mg")
       '(("preprocessor" ".")))
      (simple-racket-origin
       "profile" (base32 "10lv3iz58hqk043llnbqrkmjnm1pbvr4j1j188g85zg798rqyjzs")
       '("profile" "profile-doc" "profile-lib"))
      (racket-packages-origin
       "quickscript" (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/Metaxal/quickscript")
                             (commit %racket-commit)))
                       (sha256 (base32
                                "1r7i4g39hfkn0wb34dylzcp43ys1zl7ra9wmbzly07an7891xmns"))
                       (file-name (git-file-name "Metaxal-quickscript" %racket-version)))
       '(("quickscript" ".")))
      (simple-racket-origin
       "r5rs" (base32 "06rw7aihq4wdr27nan42mfg13fs9lh8d0hf7m9g8m5vmnkd3h2pn")
       '("r5rs" "r5rs-doc" "r5rs-lib"))
      (simple-racket-origin
       "r6rs" (base32 "1ildnf12i369drf1j24rclyhhpa9p3zjrfkbkb8ggdrajbwkrvsm")
       '("r6rs" "r6rs-doc" "r6rs-lib"))
      (racket-packages-origin
       "racket-cheat" (origin
                        (method git-fetch)
                        (uri (git-reference
                              (url "https://github.com/jeapostrophe/racket-cheat")
                              (commit %racket-commit)))
                        (sha256 (base32
                                 "1q0da3hm64fj64f713yxdlg7x1hp162fnn3mc7al2hzk098w242w"))
                        (file-name
                         (git-file-name "jeapostrophe-racket-cheat" %racket-version)))
       '(("racket-cheat" ".")))
      (simple-racket-origin
       "racklog" (base32 "0fr8xij0sssfnmwn6dfdi4jj3l62f2yj3jrjljv13kaycrfls032")
       '(("racklog" ".")))
      (simple-racket-origin
       "rackunit" (base32 "0csfxzbrk7jic4hjj0ljiry88sh9vp6rh6z0600zsybhs3vgrxsj")
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
       "realm" (base32 "0rlvwyd6rpyl0zda4a5p8dp346fvqzc8555dgfnrhliymkxb6x4g")
       '(("realm" ".")))
      (simple-racket-origin
       "redex" (base32 "1m7dimbf5raxhw9d9c5iinslijzgdnhdwd46wb7w62l0vv975pgz")
       '("redex"
         "redex-benchmark"
         "redex-doc"
         "redex-examples"
         "redex-gui-lib"
         "redex-lib"
         "redex-pict-lib"))
      (simple-racket-origin
       "sasl" (base32 "1l74rkp8m2jfb82678k3lhd2y5k9l8csazwshf9m969i67gzbjh8")
       '("sasl" "sasl-doc" "sasl-lib"))
      (simple-racket-origin
       "scheme-lib" (base32 "0pcf0y8rp4qyjhaz5ww5sr5diq0wpcdfrrnask7zapyklzx1jx8x")
       '(("scheme-lib" ".")))
      (simple-racket-origin
       "scribble" (base32 "1cy763syzvhgxyrln9d8ns6m7x7sf1x430ip10skdd2id71xrzh8")
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
       "slideshow" (base32 "0zagamjigvw6z66q7v7jf2xffg34clqjf0k1f1fzhcgiy9g0skw7")
       '("slideshow" "slideshow-doc" "slideshow-exe" "slideshow-lib" "slideshow-plugin"))
      (simple-racket-origin
       "snip" (base32 "1jpy5728w9bszgsjczlzrjjxsvqlcn4ah6gd3rp0plp8zzk9mq0s")
       '("snip" "snip-lib"))
      (simple-racket-origin
       "typed-racket" (base32 "1h27qc44vzar0ngllqimza02zj7b46bwj059jcblrxvhmqb2vi3p")
       '("source-syntax"
         "typed-racket"
         "typed-racket-compatibility"
         "typed-racket-doc"
         "typed-racket-lib"
         "typed-racket-more"))
      (simple-racket-origin
       "srfi" (base32 "0z7kchsz9m6cb4qmbznzqs2px45fz1g81amg9zn7n1sa31x3jzch")
       '("srfi" "srfi-doc" "srfi-lib" "srfi-lite-lib"))
      (simple-racket-origin
       "string-constants" (base32 "0b08xjniwnirwr55jnvbc93jsqyb2smsy9h2214w01f20x51i0na")
       '("string-constants" "string-constants-doc" "string-constants-lib"))
      (simple-racket-origin
       "swindle" (base32 "1q8vdxpzczzwdw2mys2caab45yvadmqkixsr29k8pl03n8dsg8j9")
       '(("swindle" ".")))
      (simple-racket-origin
       "syntax-color" (base32 "1h1nrxs5l8kbx3qc6nf6jvfpjasn2h2rz6grskgqbxwja7wvhfca")
       '("syntax-color" "syntax-color-doc" "syntax-color-lib"))
      (simple-racket-origin
       "trace" (base32 "1zx0kfbacidg5rllbvx842x949ra22n004irs3vm1hqmwmixy3is")
       '(("trace" ".")))
      (simple-racket-origin
       "unix-socket" (base32 "02dfwas5ynbpyz74w9kwb4wgb37y5wys7svrlmir8k0n9ph9vq0y")
       '("unix-socket" "unix-socket-doc" "unix-socket-lib"))
      (simple-racket-origin
       "web-server" (base32 "0ry5vwjd06ml9hmspgrwci8c99syx7zhp9sgldmhwwfvf5m1g29d")
       '("web-server" "web-server-doc" "web-server-lib"))
      (simple-racket-origin
       "wxme" (base32 "1qp5gr9gqsakiq3alw6m4yyv5vw4i3hp4y4nhq8vl2nkjmirvn0b")
       '("wxme" "wxme-lib"))
      (simple-racket-origin
       "xrepl" (base32 "0sw531zvd8xqckmrh5da4fiq0sfjyg1nj5d508l7bl3azn35khil")
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
                                   (raise
                                    (formatted-message
                                     (G_ "missing input '~a' to the 'racket' package")
                                     name)))))
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
                        ;; it here is convenient, at least until we support
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
