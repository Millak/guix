;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017-2019, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2019–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2019, 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2019 Jesse Gildersleve <jessejohngildersleve@protonmail.com>
;;; Copyright © 2019-2024 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021, 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2021 Charles Jackson <charles.b.jackson@protonmail.com>
;;; Copyright © 2022 Joeke de Graaf <joeke@posteo.net>
;;; Copyright © 2021, 2022 jgart <jgart@dismail.de>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2023 Andrew Kravchuk <awkravchuk@gmail.com>
;;; Copyright © 2024 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2024 bigbug <bigbookofbug@proton.me>
;;; Copyright © 2024, 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Omar Bassam <omar.bassam88@gmail.com>
;;; Copyright © 2024 Suhail Singh <suhail@bayesians.ca>
;;; Copyright © 2024 David Pflug <david@pflug.io>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

;;; This file only contains Common Lisp compilers and tooling.
;;; Common Lisp libraries go to lisp-xyz.scm.
;;; Common Lisp applications should go to the most appropriate file,
;;; e.g. StumpWM is in wm.scm.

(define-module (gnu packages lisp)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system trivial)
  #:use-module ((guix search-paths) #:select ($SSL_CERT_DIR $SSL_CERT_FILE))
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages libffcall)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages notcurses)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public buildapp
  (package
    (name "buildapp")
    (version "1.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xach/buildapp")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "020ipjfqa3l8skd97cj5kq837wgpj28ygfxnkv64cnjrlbnzh161"))))
    (build-system gnu-build-system)
    (native-inputs
     (list sbcl))
    (arguments
     `(#:tests? #f
       #:make-flags
       (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'set-home
           (lambda _
             (setenv "HOME" "/tmp")
             #t))
         (add-before 'install 'create-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               #t))))))
    (home-page "https://www.xach.com/lisp/buildapp/")
    (synopsis "Makes easy to build application executables with SBCL")
    (description
     "Buildapp is an application for SBCL or CCL that configures and saves an
executable Common Lisp image.  It is similar to cl-launch and hu.dwim.build.")
    (license license:bsd-2)))

(define-public carp
  ;; Release 0.5.5 does not support GHC 9.2.
  (let ((commit "339722325ec607091f6035866ebedea2b69080fe")
        (revision "1"))
    (package
      (name "carp")
      (version (git-version "0.5.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/carp-lang/Carp")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0w0j3imi4270dsmrh96spsc9xllsk5rrh817l80q1nyay9p53xwd"))))
      (build-system haskell-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 ;; Carp looks inside the sources checkout to know where to
                 ;; find its core libraries and other files.
                 ;; Carp emits C code and tries to compile it with an external
                 ;; C compiler. On Linux it defaults to Clang.
                 (add-after 'install 'wrap-programs
                   (lambda* (#:key inputs #:allow-other-keys)
                     (define (wrap-carp-program program)
                       (wrap-program (string-append
                                      #$output "/bin/" program)
                         `("CARP_DIR" prefix
                           (#$(package-source this-package)))
                         `("PATH" prefix
                           ,(list (dirname
                                   (search-input-file inputs "bin/clang"))
                                  (dirname
                                   (search-input-file inputs "bin/ld"))))
                         `("C_INCLUDE_PATH" prefix
                           ,(list (dirname
                                   (search-input-directory
                                    inputs "include/linux"))
                                  (dirname
                                   (search-input-file
                                    inputs "include/stdlib.h"))))))

                     (for-each wrap-carp-program
                               (list "carp"
                                     "carp-header-parse")))))))
      (inputs
       (list bash-minimal
             clang
             ghc-blaze-markup
             ghc-blaze-html
             ghc-split
             ghc-ansi-terminal
             ghc-cmark
             ghc-edit-distance
             ghc-hashable
             ghc-open-browser
             ghc-optparse-applicative))
      (native-inputs
       (list ghc-hunit))
      (home-page "https://carp-lang.org/")
      (synopsis "Statically typed Lisp without a garbage collector")
      (description
       "@code{carp} is a Lisp-like programming language that compiles to
C.  It features inferred static typing, macros, automatic memory
management without a garbage collector, a REPL, and straightforward
integration with code written in C.")
      (license license:asl2.0))))

(define-public ccl
  ;; Warning: according to upstream, CCL is not bootstrappable.
  ;; See https://github.com/Clozure/ccl/issues/222 from 2019-09-02:
  ;;
  ;;     "As far as I know, there is no way to build CCL without an existing
  ;;     running CCL image. It was bootstrapped back in 1986 or so as
  ;;     Macintosh Common Lisp, by Gary Byers, I believe, who is no longer on
  ;;     the planet to tell us the story. It SHOULD be possible to port the
  ;;     CCL compiler to portable Common Lisp, so that ANY lisp could build
  ;;     it, as is the case for SBCL, but I know of no attempt to do so."
  (package
    (name "ccl")
    ;; XXX When updating this package, check whether we can simply append
    ;; VERSION to the ccl-bootstrap URL again, instead of per architecture.
    (version "1.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Clozure/ccl/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "ccl" version))
              (sha256
               (base32
                "12cb3gp43bhl7nyms3a4lix8j1s3arnp124d3ahc2lhlrjirs0mr"))))
    (build-system gnu-build-system)
    ;; CCL consists of a "lisp kernel" and "heap image", both of which are
    ;; shipped in precompiled form in source tarballs.  The former is a C
    ;; program which we can rebuild from scratch, but the latter cannot be
    ;; generated without an already working copy of CCL, and is platform
    ;; dependent, so we need to fetch the correct tarball for the platform.
    (inputs
     `(("ccl-bootstrap"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/Clozure/ccl/releases/download/v"
                 (match (%current-system)
                   ("armhf-linux" "1.13/linuxarm")
                   ;; XXX: This source only works on x86, but provide it as a
                   ;; catch-all to prevent errors when querying this package
                   ;; on unsupported platforms.
                   (_ "1.13/linuxx86"))
                 ".tar.gz"))
           (sha256
            (base32
             (match (%current-system)
               ("armhf-linux"
                "1mlj7lgn4p9bw1ki7gk00sj89cjv0cjpdmpp65brhx9q3fvkk9xf")
               (_ "04ry8zwrcjjmzbxp43nkknchabk6i6mclrzg5biwfp1h64bcnzfx"))))))))
    (native-inputs
     (list cl-asdf m4))
    (arguments
     `(#:tests? #f                      ;no 'check' target
       #:modules ((ice-9 match)
                  (srfi srfi-26)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-image
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "xzvf" (assoc-ref inputs "ccl-bootstrap"))))
         (add-after 'unpack 'replace-asdf
           ;; Use system ASDF instead of bundled one.
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((cl-asdf (assoc-ref inputs "cl-asdf"))
                    (guix-asdf (string-append
                                cl-asdf
                                "/share/common-lisp/source/asdf/asdf.lisp"))
                    (contrib-asdf "tools/asdf.lisp"))
               (delete-file contrib-asdf)
               (copy-file guix-asdf contrib-asdf))))
         (delete 'configure)
         (add-before 'build 'pre-build
           ;; Enter the source directory for the current platform's lisp
           ;; kernel, and run 'make clean' to remove the precompiled one.
           (lambda* (#:key system #:allow-other-keys)
             (substitute* "lisp-kernel/m4macros.m4"
               (("/bin/pwd") (which "pwd")))
             (chdir (string-append
                     "lisp-kernel/"
                     (match system
                       ("i686-linux" "linuxx8632")
                       ("x86_64-linux" "linuxx8664")
                       ("armhf-linux" "linuxarm")
                       (_ (string-append "unknown system: " system)))))
             (substitute* '("Makefile")
               (("/bin/rm") "rm"))
             (setenv "CC" "gcc")
             (invoke "make" "clean")))
         ;; XXX Do we need to recompile the heap image as well for Guix?
         ;; For now just use the one we already got in the tarball.
         (replace 'install
           (lambda* (#:key outputs inputs system #:allow-other-keys)
             ;; The lisp kernel built by running 'make' in lisp-kernel/$system
             ;; is put back into the original directory, so go back.  The heap
             ;; image is there as well.
             (chdir "../..")
             (let* ((out (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib/"))
                    (bindir (string-append out "/bin/"))
                    (wrapper (string-append bindir "ccl"))
                    (bash (assoc-ref inputs "bash"))
                    (kernel
                     (match system
                       ("i686-linux" "lx86cl")
                       ("x86_64-linux" "lx86cl64")
                       ("armhf-linux" "armcl")
                       ;; Unlikely to work, but try it anyway...
                       (_ system)))
                    (heap (string-append kernel ".image")))
               (install-file kernel libdir)
               (install-file heap libdir)

               (let ((dirs `("lib" "library" "examples" "tools" "objc-bridge"
                             ,@(match system
                                 ("x86_64-linux"
                                  '("x86-headers64"))
                                 ("i686-linux"
                                  '("x86-headers"))
                                 (_ '())))))
                 (for-each copy-recursively
                           dirs
                           (map (cut string-append libdir <>) dirs)))

               (mkdir-p bindir)
               (with-output-to-file wrapper
                 (lambda ()
                   (display
                    (string-append
                     "#!" bash "/bin/sh\n"
                     "export CCL_DEFAULT_DIRECTORY=" libdir "\n"
                     "exec -a \"$0\" " libdir kernel " \"$@\"\n"))))
               (chmod wrapper #o755))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))
           (search-path-specification
            (variable "XDG_CONFIG_DIRS")
            (files '("etc")))))
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))
    (home-page "https://ccl.clozure.com/")
    (synopsis "Common Lisp implementation")
    (description "Clozure CL (often called CCL for short) is a Common Lisp
implementation featuring fast compilation speed, native threads, a precise,
generational, compacting garbage collector, and a convenient foreign-function
interface.")
    (license license:asl2.0)))

(define-public clasp-cl
  (package
    (name "clasp-cl")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/clasp-developers/clasp/releases/download/"
             version "/clasp-" version ".tar.gz"))
       (sha256
        (base32 "1sf8m0w3d4kagf4chb7viqndnr152crpsy979ll61krcfxaybc4j"))))
    (build-system gnu-build-system)
    (inputs
     (list boost clang-toolchain-19 fmt `(,gcc "lib") gmp libelf libunwind))
    (native-inputs
     (list binutils-gold ninja pkg-config sbcl))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'unpack 'fix-sh-paths
           (lambda _
             (substitute* '("dependencies/quicklisp-client/asdf.lisp"
                            "src/lisp/modules/asdf/uiop/run-program.lisp"
                            "src/lisp/modules/asdf/uiop/launch-program.lisp"
                            "src/lisp/regression-tests/extensions.lisp")
               (("\"/bin/sh\"")
                (string-append "\"" (which "sh") "\"")))))
         (add-after 'unpack 'patch-koga
           (lambda* _
             (call-with-port (open-file "src/koga/units.lisp" "a")
               (lambda (p)
                 (display "(defmethod configure-unit (c (u (eql :git))))\n" p)))))
         (add-before 'configure 'set-configure-environment
           (lambda* _
             (setenv "SOURCE_DATE_EPOCH" "1")
             (setenv "ASDF_OUTPUT_TRANSLATIONS"
                     (string-append (getenv "PWD")
                                    ":"
                                    (getenv "PWD")
                                    "/__fasls"))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (clang (assoc-ref inputs "clang-toolchain"))
                    (ld-flags (string-join
                                (apply append
                                  (map (lambda (f)
                                         (list "-L" f "-rpath" f))
                                       ;; prepend self in rpaths
                                       (cons (string-append out "/lib")
                                             (string-split (getenv "LIBRARY_PATH") #\:))))
                                ",")))
               (invoke "sbcl"
                       "--script"
                       "./koga"
                       "--skip-sync"
                       "--build-mode=bytecode-faso"
                       (string-append "--cc=" clang "/bin/clang")
                       (string-append "--cxx=" clang "/bin/clang++")
                       (string-append "--ldflags=-Wl," ld-flags)
                       "--reproducible-build"
                       "--package-path=/"
                       (string-append "--bin-path=" out "/bin")
                       (string-append "--lib-path=" out "/lib/clasp")
                       (string-append "--dylib-path=" out "/lib")
                       (string-append "--pkgconfig-path=" out "/lib/pkgconfig")
                       (string-append "--share-path=" out "/share/clasp")))))
         (replace 'build
           (lambda* (#:key parallel-build? #:allow-other-keys)
             (let ((jobs (if parallel-build?
                                (number->string (parallel-job-count))
                                "1")))
               (setenv "CLASP_BUILD_JOBS" jobs)
               (invoke "ninja" "-C" "build" "-j" jobs))))
         (replace 'install
           (lambda* _
             (invoke "ninja" "-C" "build" "install")))
         (add-after 'install 'install-lib
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "build/boehmprecise/lib/libclasp.so"
                  (string-append (assoc-ref outputs "out") "/lib")))))))
    (home-page "https://clasp-developers.github.io/")
    (synopsis "Common Lisp implementation based on LLVM and C++")
    (description "Clasp is a new Common Lisp implementation that seamlessly
 interoperates with C++ libraries and programs using LLVM for compilation to
 native code.  This allows Clasp to take advantage of a vast array of
 preexisting libraries and programs, such as out of the scientific computing
 ecosystem.  Embedding them in a Common Lisp environment allows you to make use
 of rapid prototyping, incremental development, and other capabilities that
 make it a powerful language.")
    (license license:lgpl2.1+)))

(define-public cl-asdf
  (package
    (name "cl-asdf")
    (version "3.3.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://common-lisp.net/project/asdf/archives/asdf-"
                       version ".lisp"))
       (sha256
        (base32 "12dirg0qaq2rk8s11z7nnw793l94ysqna9mbqy8a0cx6bqiqykl7"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("config-patch" ,@(search-patches "cl-asdf-config-directories.patch"))
       ("patch" ,patch/pinned)))
    (arguments
     `(#:modules ((guix build utils)
                  (guix build lisp-utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (guix build lisp-utils))
         (let* ((out (string-append (assoc-ref %outputs "out")))
                (asdf-install (string-append out %source-install-prefix
                                             "/source/asdf/"))
                (src-asdf (string-append (assoc-ref %build-inputs "source")))
                (dst-asdf (string-append asdf-install "asdf.lisp"))
                (patch (string-append (assoc-ref %build-inputs "patch")
                                      "/bin/patch"))
                (config-patch (assoc-ref %build-inputs "config-patch")))
           (mkdir-p asdf-install)
           (copy-file src-asdf dst-asdf)
           (invoke patch "-p1" "-i" config-patch dst-asdf)))))
    (home-page "https://common-lisp.net/project/asdf/")
    (synopsis "Another System Definition Facility")
    (description
     "ASDF is what Common Lisp hackers use to build and load software.  It is
the successor of the Lisp DEFSYSTEM of yore.  ASDF stands for Another System
Definition Facility.")
    ;; MIT License
    (license license:expat)))

(define-public clisp
  (package
    (name "clisp")
    (version "2.49-92")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/gnu-clisp/clisp")
             (commit "clisp-2.49.92-2018-02-18")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k2dmgl0miz3767iks4p0mvp6xw0ysyxhjpklyh11j010rmh6hqb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list cl-asdf))
    (inputs (list libffcall ncurses readline libsigsegv))
    (arguments
     `(#:configure-flags '(,@(if (string-prefix? "armhf-linux"
                                                 (or (%current-system)
                                                     (%current-target-system)))
                                 '("CFLAGS=-falign-functions=4")
                                 '())
                           ,@(if (target-x86-64?)
                                 '("--enable-portability"
                                   "--with-threads=POSIX_THREADS")
                                 '())
                            "--with-dynamic-ffi"
                            "--with-dynamic-modules"
                            "--with-ffcall"
                            "--with-readline"
                            "--with-sigsegv"
                            "--with-module=asdf"
                            "--with-module=rawsock")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sh-and-pwd
           (lambda _
             ;; The package is very messy with its references to "/bin/sh" and
             ;; some other absolute paths to traditional tools.  These appear in
             ;; many places where our automatic patching misses them.  Therefore
             ;; we do the following, in this early (post-unpack) phase, to solve
             ;; the problem from its root.
             (substitute* '("src/clisp-link.in"
                            "src/unix.d"
                            "src/makemake.in")
               (("/bin/sh") (which "sh")))
             (substitute* (find-files "." "configure|Makefile")
               (("/bin/sh") "sh"))
             (substitute* '("src/clisp-link.in")
               (("/bin/pwd") "pwd"))
             #t))
         (add-after 'unpack 'replace-asdf
           ;; Use system ASDF instead of bundled one.
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((cl-asdf (assoc-ref inputs "cl-asdf"))
                    (guix-asdf (string-append
                                cl-asdf
                                "/share/common-lisp/source/asdf/asdf.lisp"))
                    (contrib-asdf "modules/asdf/asdf.lisp"))
               (delete-file contrib-asdf)
               (copy-file guix-asdf contrib-asdf)))))))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))
           (search-path-specification
            (variable "XDG_CONFIG_DIRS")
            (files '("etc")))))
    (home-page "https://clisp.sourceforge.io/")
    (synopsis "Common Lisp implementation")
    (description
     "GNU CLISP is an implementation of ANSI Common Lisp.  Common Lisp is a
high-level, object-oriented functional programming language.  CLISP includes
an interpreter, a compiler, a debugger, and much more.")
    (license license:gpl2+)))

(define-public confusion-mdl
  (let* ((commit "12a055581fc262225272df43287dae48281900f5"))
    (package
      (name "confusion-mdl")
      (version "0.2")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url (string-append "https://gitlab.com/emacsomancer/" name))
                      (commit commit)))
                (sha256
                 (base32
                  "1zi8kflzvwqg97ha1sa5xjisbjs5z1mvbpa772vfxiv5ksnpxp0d"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; there are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key (make-flags '()) #:allow-other-keys)
               (apply invoke "make" "CC=gcc" make-flags)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "mdli" bin)
                 #t))))))
      (native-inputs
       (list perl))
      (inputs
       (list libgc))
      (synopsis "Interpreter for the MIT Design Language (MDL)")
      (description "MDL (the MIT Design Language) is a descendant of Lisp.  It
was originally developed in 1971 on the PDP-10 computer under the Incompatible
Timesharing System (ITS) to provide high level language support for the
Dynamic Modeling Group at MIT's Project MAC.  Infocom built the original
PDP-10 Zork in MDL and their later ZIL (Zork Implementation Language) was
based on a subset of MDL.  Confusion is a MDL interpreter that works just well
enough to play the original mainframe Zork all the way through.")
      (home-page "http://www.russotto.net/git/mrussotto/confusion/src/master/src/README")
      (license license:gpl3+))))

(define-public ecl
  (package
    (name "ecl")
    (version "24.5.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ecl.common-lisp.dev/static/files/release/"
             name "-" version ".tgz"))
       (sha256
        (base32 "0av2prf474cg0glr37fgsfb4w0dxfg38pykb72ay9q3132xnbsp4"))))
    (build-system gnu-build-system)
    ;; src/configure uses 'which' to confirm the existence of 'gzip'.
    (native-inputs
     (list cl-asdf which texinfo))
    (inputs (list bash-minimal))
    ;; When ECL is embedded in a program that wants to use Common Lisp as an
    ;; extension language, libgmp, libatomic-ops, libgc and libffi must be
    ;; present when compiling the program because they are required by ECL's
    ;; header file.
    ;; Therefore we put these libraries in 'propagated-inputs' instead
    ;; of 'inputs'.
    (propagated-inputs
     (list gmp libatomic-ops libgc libffi))
    (arguments
     `(#:configure-flags '("--without-rt")
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: As of version 20.4.24, we pass 17995 tests and fail 7.
         ;; 2-3 tests may be due to FHS assumptions.
         (delete 'check)
         (add-after 'unpack 'replace-asdf
           ;; Use system ASDF instead of bundled one.
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((cl-asdf (assoc-ref inputs "cl-asdf"))
                    (guix-asdf (string-append
                                cl-asdf
                                "/share/common-lisp/source/asdf/asdf.lisp"))
                    (contrib-asdf "contrib/asdf/asdf.lisp"))
               (copy-file guix-asdf contrib-asdf))))
         (add-after 'install 'remove-build-stamp
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/lib/ecl-" ,version "/build-stamp"))))
         (add-after 'remove-build-stamp 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((ecl (assoc-ref outputs "out"))
                    (input-path (lambda (lib path)
                                  (string-append
                                   (assoc-ref inputs lib) path)))
                    (libraries '("gmp" "libatomic-ops" "libgc" "libffi" "libc"))
                    (binaries  '("gcc" "ld-wrapper" "binutils"))
                    (library-directories
                     (map (lambda (lib) (input-path lib "/lib"))
                          libraries)))
               (wrap-program (string-append ecl "/bin/ecl")
                 `("PATH" prefix
                   ,(map (lambda (binary)
                           (input-path binary "/bin"))
                         binaries))
                 `("CPATH" suffix
                   ,(map (lambda (lib)
                           (input-path lib "/include"))
                         `("kernel-headers" ,@libraries)))
                 `("LIBRARY_PATH" suffix ,library-directories)
                 `("LD_LIBRARY_PATH" suffix ,library-directories)))))
         (add-after 'wrap 'check (assoc-ref %standard-phases 'check))
         (add-before 'check 'fix-path-to-ecl
           (lambda _
             (substitute* "build/tests/Makefile"
               (("\\$\\{exec_prefix\\}/") "")))))))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))
           (search-path-specification
            (variable "XDG_CONFIG_DIRS")
            (files '("etc")))))
    (home-page "https://ecl.common-lisp.dev/")
    (synopsis "Embeddable Common Lisp")
    (supported-systems (remove target-hurd? %supported-systems))
    (description "ECL is an implementation of the Common Lisp language as
defined by the ANSI X3J13 specification.  Its most relevant features are: a
bytecode compiler and interpreter, being able to compile Common Lisp with any
C/C++ compiler, being able to build standalone executables and libraries, and
supporting ASDF, Sockets, Gray streams, MOP, and other useful components.")
    ;; Note that the file "Copyright" points to some files and directories
    ;; which aren't under the lgpl2.1+ and instead contain many different,
    ;; non-copyleft licenses.
    ;; See https://common-lisp.net/project/ecl/posts/ECL-license.html.
    (license license:lgpl2.1+)))

(define-public eisl
  (package
    (name "eisl")
    (version "5.38")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sasagawa888/eisl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zrlsg77azlhm01djfqxglipqnz67r4h092f86rvvvhr332y2902"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal freeglut gdbm libiconv ncurses tcl tk))
    (native-inputs
     (list cppcheck))
    (arguments
     (list #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "library/compiler.lsp"
                     (("\"cc ")
                      "\"gcc "))
                   (substitute* "library/tcltk.lsp"
                     (("include <tcl/tcl\\.h>")
                      "include <tcl.h>")
                     (("c-option \"-ltcl -ltk\" linux")
                      "c-option \"-ltcl8.6 -ltk8.6\" linux"))))
               (delete 'configure)
               (add-after 'install 'wrap
                 (lambda* (#:key inputs #:allow-other-keys)
                   (wrap-program (string-append #$output "/bin/eisl")
                     `("PATH" prefix
                       ,(map (lambda (bin)
                               (string-append (assoc-ref inputs bin) "/bin"))
                             '("binutils" "coreutils" "gcc" "ld-wrapper")))
                     `("LIBRARY_PATH" suffix
                       ,(map (lambda (lib)
                               (string-append (assoc-ref inputs lib) "/lib"))
                             '("libc")))))))))
    (home-page "https://github.com/sasagawa888/eisl")
    (synopsis "Implementation of ISLisp")
    (description "Easy ISLISP (eisl) is an implementation of ISLisp which
includes a compiler as well as an interpreter.")
    (license (list license:bsd-2 ;; documents/license.txt
                   license:expat ;; cii/LICENSE
                   license:gpl2+ ;; nana/gdb/test.c and others under nana/
                   license:bsd-3)))) ;; bench/*

(define-public gcl
  (package
    (name "gcl")
    (version "2.6.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/r/gcl.git")
             (commit (string-append "Version_"
                                    (string-map (lambda (c)
                                                  (if (char=? c #\.) #\_ c))
                                                version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b9m02rfnyflsr8n57v7llxz5m3mi7ip3ypwdww4pdhbgh0lzyg7"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:parallel-build? #f  ; The build system seems not to be thread safe.
      #:test-target "ansi-tests/test_results"
      #:configure-flags #~(list
                           "--enable-ansi" ; required by the maxima package
                           (string-append "CFLAGS=-I"
                                          #$(this-package-input "libtirpc")
                                          "/include/tirpc")
                           (string-append "LDFLAGS=-L"
                                          #$(this-package-input "libtirpc")
                                          "/lib")
                           "LIBS=-ltirpc")
      #:make-flags #~(let ((gcc (search-input-file %build-inputs "/bin/gcc")))
                       (list (string-append "GCL_CC=" gcc)
                             (string-append "CC=" gcc)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'realpath-workaround
            ;; Calls to the realpath function can set errno even if the return
            ;; value of the function indicates that there is no error, which
            ;; make massert consider that there was an error.
            (lambda _
              (substitute* "gcl/o/main.c"
                (("massert\\(realpath\\(s,o\\)\\);" all)
                 "massert((realpath(s, o) != NULL) && ((errno = 0) == 0));"))))
          (add-after 'unpack 'fix-missing-enum
            (lambda _
              ;; The 'disassembler_style' enum is not defined anywhere,
              ;; and the parameter is not used...
              (substitute* "gcl/o/main.c"
                (("my_fprintf_styled\\(void \\*v,enum disassembler_style,")
                 "my_fprintf_styled(void *v,int disassembler_style,"))))
          (add-after 'unpack 'fix-makefile
            ;; The "final" target doesn't exist.
            (lambda _
              (substitute* "gcl/makefile"
                (("\\$\\(MAKE\\) -C \\$\\(PORTDIR\\) final")
                 "$(MAKE) -C $(PORTDIR)"))))
          (add-before 'configure 'pre-conf
            (lambda* (#:key inputs #:allow-other-keys)
              (chdir "gcl")
              (substitute*
                  (append
                   '("pcl/impl/kcl/makefile.akcl"
                     "add-defs"
                     "unixport/makefile.dos"
                     "add-defs.bat"
                     "gcl-tk/makefile.prev"
                     "add-defs1")
                   (find-files "h" "\\.defs"))
                (("SHELL=/bin/bash")
                 (string-append "SHELL=" (which "bash")))
                (("SHELL=/bin/sh")
                 (string-append "SHELL=" (which "sh"))))
              (substitute* "h/linux.defs"
                (("#CC") "CC")
                (("-fwritable-strings") "")
                (("-Werror") ""))
              (substitute* "lsp/gcl_top.lsp"
                (("\"cc\"")
                 (string-append "\"" (assoc-ref %build-inputs "gcc")
                                "/bin/gcc\""))
                (("\\(or \\(get-path \\*cc\\*\\) \\*cc\\*\\)") "*cc*")
                (("\"ld\"")
                 (string-append "\"" (assoc-ref %build-inputs "binutils")
                                "/bin/ld\""))
                (("\\(or \\(get-path \\*ld\\*\\) \\*ld\\*\\)") "*ld*")
                (("\\(get-path \"objdump --source \"\\)")
                 (string-append "\"" (assoc-ref %build-inputs "binutils")
                                "/bin/objdump --source \"")))))
          (add-after 'install 'wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((gcl #$output)
                     (input-path (lambda (lib path)
                                   (string-append
                                    (assoc-ref inputs lib) path)))
                     (binaries '("binutils")))
                ;; GCC and the GNU binutils are necessary for GCL to be
                ;; able to compile Lisp functions and programs (this is
                ;; a standard feature in Common Lisp). While the
                ;; the location of GCC is specified in the make-flags,
                ;; the GNU binutils must be available in GCL's $PATH.
                (wrap-program (string-append gcl "/bin/gcl")
                  `("PATH" prefix ,(map (lambda (binary)
                                          (input-path binary "/bin"))
                                        binaries))))))
          ;; drop strip phase to make maxima build, see
          ;; https://www.ma.utexas.edu/pipermail/maxima/2008/009769.html
          (delete 'strip))))
    (inputs
     (list bash-minimal gmp libtirpc readline))
    (native-inputs
     (list m4 texinfo))
    (home-page "https://www.gnu.org/software/gcl/")
    (synopsis "Common Lisp implementation")
    (description "GCL is an implementation of the Common Lisp language.  It
features the ability to compile to native object code and to load native
object code modules directly into its lisp core.  It also features a
stratified garbage collection strategy, a source-level debugger and a built-in
interface to the Tk widget system.")
    (license license:lgpl2.0+)))

(define-public janet
  (package
    (name "janet")
    (version "1.37.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janet-lang/janet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06ym80siwvlg3fmha01vsaqilcd7fjx8wq61zql906yjiljq22rb"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list
              (string-append "DESTDIR=" #$output)
              (string-append "PREFIX=")
              (string-append "CC=" #$(cc-for-target)))
           #:test-target "test"
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))
    (home-page "https://janet-lang.org/")
    (synopsis "Functional, imperative and embeddable programming language")
    (description
     "Janet is a functional and imperative programming language.  It can be
used for rapid prototyping, dynamic systems, and other domains where dynamic
languages shine.  You can also add Janet scripting to an application by
embedding a single C file and two headers.  It can be easily ported to new
platforms.  The entire language (core library, interpreter, compiler,
assembler, PEG) is less than 1MB.")
    (license license:expat)))

(define-public jpm
  (package
    (name "jpm")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janet-lang/jpm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05rdxigmiy7vf93s16a8n2029lq33073jccz1rjl4iisxj6piw4l"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "configs/linux_config.janet"
                (("/usr/local")
                 #$output)
                (("\"cc\"")
                 (string-append "\""
                                (search-input-file inputs "/bin/gcc")
                                "\""))
                (("\"c\\+\\+\"")
                 (string-append "\""
                                (search-input-file inputs "/bin/g++")
                                "\""))
                (("\"git\"")
                 (string-append "\""
                                (search-input-file inputs "/bin/git")
                                "\""))
                (("\"curl\"")
                 (string-append "\""
                                (search-input-file inputs "/bin/curl")
                                "\"")))
              (substitute* "jpm/shutil.janet"
                (("cp")
                 (string-append (search-input-file inputs "/bin/cp"))))
              (setenv "PREFIX"
                      #$output)))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each (lambda (dir)
                          (mkdir-p (string-append #$output "/" dir)))
                        '("lib/janet/jpm" "share/man/man1"))
              (invoke "janet" "bootstrap.janet"
                      "configs/linux_config.janet")
              (let ((gcc-toolchain (assoc-ref inputs "gcc-toolchain"))
                    (core-min (assoc-ref inputs "coreutils-minimal")))
                (wrap-program (string-append #$output "/bin/jpm")
                  `("JANET_HEADERPATH" ":" prefix
                    (,(string-append #$janet "/include/janet")))
                  `("JANET_LIBPATH" ":" prefix
                    (,(string-append #$janet "/lib")))
                  `("C_INCLUDE_PATH" ":" prefix
                    (,(string-append gcc-toolchain "/include")))
                  `("CPLUS_INCLUDE_PATH" ":" prefix
                    (,(string-append gcc-toolchain "/include/c++")
                     ,(string-append gcc-toolchain "/include")))
                  `("LIBRARY_PATH" ":" prefix
                    (,(string-append gcc-toolchain "/lib")
                     ,(string-append gcc-toolchain "/lib64")))
                  `("PATH" ":" prefix
                    (,(string-append gcc-toolchain "/bin")
                     ,(string-append core-min "/bin"))))))))))
    (inputs
     (list bash-minimal
           coreutils-minimal
           curl
           gcc
           git-minimal/pinned
           ;; Lazily resolve the gcc-toolchain to avoid a circular
           ;; dependency.
           (module-ref (resolve-interface '(gnu packages commencement))
                       'gcc-toolchain)))
    (propagated-inputs (list janet))
    (native-search-paths (list $SSL_CERT_DIR $SSL_CERT_FILE))
    (home-page "https://janet-lang.org/")
    (synopsis "Janet Project Manager for the Janet programming language")
    (description
     "@code{jpm} is the Janet Project Manager tool.  It is a build
tool and its main uses are installing dependencies, compiling C/C++ to native
libraries, and other management tasks for Janet projects.")
    (license license:expat)))

(define-public lisp-repl-core-dumper
  (package
    (name "lisp-repl-core-dumper")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/ambrevar/lisp-repl-core-dumper.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04c12gi5izpkv8ha98z9qdkbmyrsq6ga060adrx53da8z31gnhk1"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("lisp-repl-core-dumper" "bin/"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'fix-utils-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((cat (search-input-file inputs "/bin/cat"))
                    (paste (search-input-file inputs "/bin/paste"))
                    (sort (search-input-file inputs "/bin/sort"))
                    (basename (search-input-file inputs "/bin/basename"))
                    (sed (search-input-file inputs "/bin/sed")))
               (substitute* "lisp-repl-core-dumper"
                 (("\\$\\(basename") (string-append "$(" basename))
                 (("\\<cat\\>") cat)
                 (("\\<paste\\>") paste)
                 (("\\<sed\\>") sed)
                 (("\\<sort\\>") sort))))))))
    (inputs
     `(("coreutils" ,coreutils-minimal)
       ("sed" ,sed)))
    (home-page "https://gitlab.com/ambrevar/lisp-repl-core-dumper")
    (synopsis "Generate REPL-optimized Lisp cores on demand")
    (description
     "This tool generates Lisp images that can embed the provided systems
and make for REPLs that start blazing fast.

@itemize
@item It’s portable and should work with any compiler.
@item It works for any REPL.
@item It allows you to include arbitrary libraries.
@end itemize\n")
    (license license:gpl3+)))

(define-public lush2
  (package
    (name "lush2")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/lush/lush2/lush-"
                           version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "src/unix.c"
             (("\\{ \"LUSH_DATE\", __DATE__ \\},") "")
             (("\\{ \"LUSH_TIME\", __TIME__ \\},") ""))
           (substitute* "src/main.c"
             (("\" \\(built \" __DATE__ \"\\)\"") ""))
           #t))
       (sha256
        (base32
         "02pkfn3nqdkm9fm44911dbcz0v3r0l53vygj8xigl6id5g3iwi4k"))))
    (build-system gnu-build-system)
    (arguments
     `(;; We have to add these LIBS so that they are found.
       #:configure-flags (list "LIBS=-lz"
                               "X_EXTRA_LIBS=-lfontconfig"
                               "--with-x")
       #:tests? #f)) ; No make check.
    (native-inputs (list intltool))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-net" ,sdl-net)
       ("sdl-ttf" ,sdl-ttf)
       ("libxft" ,libxft)
       ("fontconfig" ,fontconfig)
       ("gsl" ,gsl)
       ("openblas" ,openblas)
       ("glu" ,glu)
       ("mesa" ,mesa)
       ("mesa-utils" ,mesa-utils)
       ("binutils" ,binutils)
       ("libiberty" ,libiberty)
       ("readline" ,readline)
       ("zlib" ,zlib)
       ("gettext-minimal" ,gettext-minimal)))
    (synopsis "Lisp Universal Shell")
    (description
     "Lush is an object-oriented Lisp interpreter/compiler with features
designed to please people who want to prototype large numerical
applications.  Lush includes an extensive library of
vector/matrix/tensor manipulation, numerous numerical libraries
(including GSL, LAPACK, and BLAS), a set of graphic functions, a
simple GUI toolkit, and interfaces to various graphic and multimedia
libraries such as OpenGL, SDL, Video4Linux, and ALSA (video/audio
grabbing), and others.  Lush is an ideal frontend script language for
programming projects written in C or other languages.  Lush also has
libraries for Machine Learning, Neural Nets and statistical estimation.")
    (home-page "https://lush.sourceforge.net/")
    (license license:lgpl2.1+)))

(define-public picolisp
  (package
    (name "picolisp")
    (version "24.9")
    (supported-systems '("aarch64-linux"
                         "powerpc64le-linux"
                         "x86_64-linux"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://software-lab.de/picoLisp-" version ".tgz"))
       (sha256
        (base32 "1bsp14vjzvlav813mxzgigcdrmsxhrrhlhdzfdvdq6havacm0cwf"))))
    (build-system gnu-build-system)
    (inputs (list clang readline libffi openssl pkg-config))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'fix-paths
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (picolisp-shebang (string-append "#!" out
                                                "/bin/picolisp " out
                                                "/lib/picolisp/lib.l"))
                             (pil-shebang (string-append "#!" out "/bin/pil")))
                        (substitute* '("bin/pil" "bin/psh" "bin/pty" "bin/vip"
                                       "bin/watchdog")
                          (("#\\!bin/picolisp lib.l")
                           picolisp-shebang)
                          (("#\\!\\.\\./bin/picolisp \\.\\./lib.l")
                           picolisp-shebang)
                          (("#\\!/usr/bin/picolisp /usr/lib/picolisp/lib.l")
                           picolisp-shebang)
                          (("#\\!/usr/bin/pil")
                           pil-shebang)))))
                  (replace 'build
                    (lambda _
                      (invoke "make" "-C" "src")))
                  (add-before 'check 'set-home-for-tests
                    (lambda _
                      (setenv "HOME" "/tmp")))
                  (replace 'check
                    (lambda _
                      (invoke "./pil" "test/lib.l" "-bye" "+")))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (doc (string-append out "/share/doc"))
                             (man (string-append out "/share/man"))
                             (picolisp (string-append out "/lib/picolisp")))
                        (copy-recursively "man" man)
                        (copy-recursively "doc" doc)
                        (copy-recursively "." picolisp)
                        (for-each (lambda (name)
                                    (let ((path (string-append picolisp "/"
                                                               name)))
                                      (delete-file-recursively path)))
                                  '("doc" "COPYING"
                                    "INSTALL"
                                    "man"
                                    "pil"
                                    "README"
                                    "src"
                                    "test"))
                        (mkdir-p bin)
                        (for-each (lambda (name)
                                    (symlink name
                                             (string-append bin "/"
                                                            (basename name))))
                                  (find-files (string-append picolisp "/bin")))))))))
    (synopsis "Interpreter for the PicoLisp programming language")
    (description
     "PicoLisp is a programming language, or really a programming system,
including a built-in database engine and a GUI system.")
    (home-page "https://picolisp.com/wiki/?home")
    (license license:expat)))

(define-public roswell
  (package
   (name "roswell")
   (version "23.10.14.114")
   (home-page "https://github.com/roswell/roswell")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/roswell/roswell")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "05w5sjh1bfy2wnblc09cb9qs8h7hxkx5hcqlbgpn7md32b0m4h7g"))))
   (build-system gnu-build-system)
   (arguments
    (list #:configure-flags #~(list (string-append "--prefix=" #$output))
          #:tests? #f))
   (native-inputs
    (list autoconf automake intltool))
   (inputs
    (list curl))
   (propagated-inputs
    ;; Has to be propagated in order to be found during setup.
    (list gnu-make patchelf))
   (synopsis "Common Lisp implementation manager, launcher, and more")
   (description
    "Roswell started out as a command-line tool with the aim to make
installing and managing Common Lisp implementations really simple and easy.
Roswell has now evolved into a full-stack environment for Common Lisp
development, and has many features that makes it easy to test, share, and
distribute your Lisp applications.

Roswell is still in beta.  Despite this, the basic interfaces are stable and
not likely to change.")
   (license license:expat)))

(define-public s7-bootstrap
  ;; Need s7-bootstrap to build libc_s7.so (for the REPL) and run tests
  (let ((commit "23a64facfac06c4bc23a7de8f04691c55f88bd40") ;no releases
        (revision "0"))
    (hidden-package
     (package
       (name "s7-bootstrap")
       (version (git-version "24.1" revision commit))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://cm-gitlab.stanford.edu/bil/s7.git")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "0fiw8lb7rswglixvn271la2q2hgwszrf2ckykhr6jjxdvmb9a7d0"))
                 (patches (search-patches "s7-flint-3.patch"))))
       (build-system gnu-build-system)
       (arguments
        (list #:tests? #f ;no tests in bootstrap
              #:phases #~(modify-phases %standard-phases
                           (delete 'configure) ;no configure
                           (replace 'build
                             (lambda _
                               ;; using build commands from s7 home page
                               (display "[BUILD] repl\n")
                               (invoke #$(cc-for-target) "s7.c" "-o" "repl"
                                       "-I." "-O2" "-g"
                                       "-DWITH_MAIN"
                                       (string-append
                                        "-DS7_LOAD_PATH=\""
                                        #$output "/share/s7/scm\"")
                                       "-ldl" "-lm"
                                       "-Wl,-export-dynamic")
                               (display "[BUILD] nrepl\n")
                               (invoke #$(cc-for-target) "s7.c" "-o" "nrepl"
                                       "-I." "-O2" "-g"
                                       "-DWITH_MAIN" "-DWITH_NOTCURSES"
                                       (string-append
                                        "-DS7_LOAD_PATH=\""
                                        #$output "/share/s7/scm\"")
                                       "-ldl" "-lm" "-lnotcurses-core"
                                       "-Wl,-export-dynamic")
                               (display "[BUILD] libarb_s7.so\n")
                               (invoke #$(cc-for-target) "libarb_s7.c"
                                       "-I." "-O2" "-g"
                                       "-shared" "-o" "libarb_s7.so"
                                       "-lflint" "-lmpc" "-fPIC")
                               (display "[BUILD] libnotcurses_s7.so\n")
                               (invoke #$(cc-for-target) "notcurses_s7.c"
                                       "-I." "-O2" "-g"
                                       "-shared" "-o" "libnotcurses_s7.so"
                                       "-lnotcurses-core" "-fPIC")
                               ;; Need s7.o and ffitest for tests
                               (display "[BUILD] s7.o\n")
                               (invoke #$(cc-for-target) "-c" "s7.c" "-o"
                                       "s7.o" "-I." "-O2"  "-ldl" "-lm")
                               (display "[BUILD] ffitest\n")
                               (invoke #$(cc-for-target) "-o" "ffitest"
                                       "ffitest.c" "-g2" "s7.o" "-lm" "-I."
                                       "-ldl" "-Wl,-export-dynamic")))
                           (add-before 'check 'install-scm
                             ;; scm files need to be installed before testing
                             (lambda _
                               (for-each (lambda (x)
                                           (install-file
                                            x (string-append
                                               #$output "/share/s7/scm/")))
                                         (find-files "." "\\.scm"))))
                           (replace 'install
                             (lambda _
                               (let ((bin (string-append #$output "/bin"))
                                     (share (string-append #$output
                                                           "/share/s7/"))
                                     (doc (string-append #$output
                                                         "/share/doc/s7/"))
                                     (lib (string-append #$output "/lib"))
                                     (inc (string-append #$output "/include/")))
                                 (install-file "repl" bin)
                                 (install-file "nrepl" bin)
                                 (install-file "ffitest" bin)
                                 (install-file "libarb_s7.so" lib)
                                 (install-file "libnotcurses_s7.so" lib)
                                 (install-file "s7.c" share)
                                 (install-file "s7.h" inc)
                                 (install-file "s7.html" doc)))))))
       (inputs (list flint mpc notcurses))
       (home-page "https://ccrma.stanford.edu/software/snd/snd/s7.html")
       (synopsis "Scheme interpreter intended as an extension language")
       (description
        "s7 is a Scheme interpreter intended as an extension language for
other applications.  It exists as just two files, @code{s7.c} and @code{s7.h},
that may be copied into the source tree of another application.  There are no
libraries, no run-time init files, and no configuration scripts.  It can also
be built as a stand-alone REPL interpreter.")
       (license license:bsd-0)))))

(define-public s7
  (package
    (inherit s7-bootstrap)
    (name "s7")
    (arguments
     (substitute-keyword-arguments
       (strip-keyword-arguments
         '(#:tests?)
         (package-arguments s7-bootstrap))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch
              (lambda _
                (substitute* "s7.c"
                  (("libc_s7.so")
                   (string-append #$output "/lib/libc_s7.so")))))
            (add-after 'build 'build-full
              (lambda _
                (invoke "repl" "./libc.scm")))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "repl" "./s7test.scm"))))
            (add-after 'install 'install-full
              (lambda _
                (install-file "libc_s7.so"
                              (string-append #$output "/lib/"))
                (delete-file (string-append #$output "/bin/ffitest"))))))))
    (native-inputs (list s7-bootstrap))
    (properties (alist-delete 'hidden? (package-properties s7-bootstrap)))))

(define-public sbcl
  (package
    (name "sbcl")
    (version "2.4.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sbcl/sbcl/" version "/sbcl-"
                           version "-source.tar.bz2"))
       (sha256
        (base32 "1y72ql8dxwjkwkikyk26w4n38p6apvv2l7440kp3x4fjd5mkksyf"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Don't force ARMv5.
           (substitute* "src/runtime/Config.arm-linux"
             (("-march=armv5t") ""))))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (native-inputs
     ;; From INSTALL:
     ;;     Supported build hosts are:
     ;;       SBCL
     ;;       CMUCL
     ;;       CCL (formerly known as OpenMCL)
     ;;       ABCL (recent versions only)
     ;;       CLISP (only some versions: 2.44.1 is OK, 2.47 is not)
     ;;       XCL
     ;;
     ;; From NEWS:
     ;;     * build enhancement: new host quirks mechanism, support for building under
     ;;     ABCL and ECL (as well as CCL, CMUCL, CLISP and SBCL itself)
     ;;
     ;; CCL is not bootstrappable so it won't do.  CLISP 2.49 seems to work.
     ;; ECL too.  As of 2020-07-01, ECL was last updated in 2020 while CLISP
     ;; was last updated in 2010, and both take about the same time to build SBCL.
     ;;
     ;; For now we stick to CLISP as the default for all systems.  In any event, keep
     ;; the `match' here to make it easier to change the host compiler for various
     ;; architectures.  Consider switching to ECL if it gets faster than CLISP
     ;; (maybe post 2020 release).
     (list (match (%current-system)
             ("powerpc-linux"       ; CLISP fails to build, needs investigating.
              ecl)
             (_
              clisp))
           cl-asdf
           ed
           inetutils         ;for hostname(1)
           texinfo
           (texlive-updmap.cfg (list texlive-texinfo))
           which))
    (inputs
     (list bash-minimal
           gmp                          ; for sb-gmp
           mpfr                         ; for sb-mpfr
           (list zstd "lib")))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-build-id
           ;; One of the build scripts makes a build id using the current date.
           ;; Replace it with a reproducible id using a part of the output hash.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((hash (substring (assoc-ref outputs "out")
                                    (+ (string-length (%store-directory)) 1)
                                    (+ (string-length (%store-directory)) 9))))
               (substitute* "make-config.sh"
                 (("echo .* > output/build-id.inc")
                  (string-append "echo '\"'guix-sbcl-"
                                 hash
                                 "'\"' > output/build-id.inc"))))))
         (add-after 'unpack 'replace-asdf
           ;; SBCL developers have not committed to keeping ASDF up to date
           ;; due to breaking changes [1]. Guix can handle this situation
           ;; easily, and it behooves us to have more control over what version
           ;; of ASDF we use to build software; therefore, replace the contrib
           ;; ASDF with the version packaged into Guix.
           ;; [1] - https://bugs.launchpad.net/sbcl/+bug/1823442
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((cl-asdf (assoc-ref inputs "cl-asdf"))
                    (guix-asdf (string-append
                                cl-asdf
                                "/share/common-lisp/source/asdf/asdf.lisp"))
                    (contrib-asdf "contrib/asdf/asdf.lisp"))
               (copy-file guix-asdf contrib-asdf))))
         (add-before 'build 'patch-unix-tool-paths
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bash (assoc-ref inputs "bash"))
                   (coreutils (assoc-ref inputs "coreutils"))
                   (ed (assoc-ref inputs "ed")))
               (define (quoted-path input path)
                 (string-append "\"" input path "\""))
               ;; Patch absolute paths in string literals.  Note that this
               ;; occurs in some .sh files too (which contain Lisp code).  Use
               ;; ISO-8859-1 because some of the files are ISO-8859-1 encoded.
               (with-fluids ((%default-port-encoding #f))
                 ;; The removed file is utf-16-be encoded, which gives substitute*
                 ;; trouble. It does not contain references to the listed programs.
                 (substitute* (delete
                               "./tests/data/compile-file-pos-utf16be.lisp"
                               (find-files "." "\\.(lisp|sh)$"))
                   (("\"/bin/sh\"") (quoted-path bash "/bin/sh"))
                   (("\"/usr/bin/env\"") (quoted-path coreutils "/usr/bin/env"))
                   (("\"/bin/cat\"") (quoted-path coreutils "/bin/cat"))
                   (("\"/bin/ed\"") (quoted-path ed "/bin/ed"))
                   (("\"/bin/echo\"") (quoted-path coreutils "/bin/echo"))
                   (("\"/bin/uname\"") (quoted-path coreutils "/bin/uname"))))
               ;; This one script has a non-string occurrence of /bin/sh.
               (substitute* '("tests/foreign.test.sh")
                 ;; Leave whitespace so we don't match the shebang.
                 ((" /bin/sh ") " sh "))
               ;; This file contains a module that can create executable files
               ;; which depend on the presence of SBCL.  It generates shell
               ;; scripts doing "exec sbcl ..." to achieve this.  We patch both
               ;; the shebang and the reference to "sbcl", tying the generated
               ;; executables to the exact SBCL package that generated them.
               (substitute* '("contrib/sb-executable/sb-executable.lisp")
                 (("/bin/sh") (string-append bash "/bin/sh"))
                 (("exec sbcl") (string-append "exec " out "/bin/sbcl")))
               ;; Disable some tests that fail in our build environment.
               (substitute* '("contrib/sb-bsd-sockets/tests.lisp")
                 ;; This requires /etc/protocols.
                 (("\\(deftest get-protocol-by-name/error" all)
                  (string-append "#+nil ;disabled by Guix\n" all)))
               (substitute* '("contrib/sb-posix/posix-tests.lisp")
                 ;; These assume some users/groups which we don't have.
                 (("\\(deftest pwent\\.[12]" all)
                  (string-append "#+nil ;disabled by Guix\n" all))
                 (("\\(deftest grent\\.[12]" all)
                  (string-append "#+nil ;disabled by Guix\n" all))))))
         (add-before 'build 'fix-contrib-library-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gmp (assoc-ref inputs "gmp"))
                   (mpfr (assoc-ref inputs "mpfr")))
               (substitute* '("contrib/sb-gmp/gmp.lisp")
                 (("\"libgmp\\.so")
                  (string-append "\"" gmp "/lib/libgmp.so")))
               (substitute* '("contrib/sb-mpfr/mpfr.lisp")
                 (("\"libmpfr\\.so")
                  (string-append "\"" mpfr "/lib/libmpfr.so"))))))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (invoke "sh" "make.sh" ,@(match (%current-system)
                                        ("powerpc-linux"
                                         `("ecl"))
                                        (_
                                         `("clisp")))
                     (string-append "--prefix="
                                    (assoc-ref outputs "out"))
                     ,@(if (or (target-ppc32?)
                               (target-x86-32?))
                         ;; 3072 is too much for these architectures.
                         `("--dynamic-space-size=2048")
                         `("--dynamic-space-size=3072"))
                     "--with-sb-core-compression"
                     "--with-sb-xref-for-internals"
                     ;; SB-SIMD will only be built on x86_64 CPUs supporting
                     ;; AVX2 instructions. Some x86_64 CPUs don't, so for reproducibility
                     ;; we disable it.
                     "--without-sb-simd")))
         (add-after 'build 'build-shared-library
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (invoke "sh" "make-shared-library.sh")))
         (replace 'install
           (lambda _
             (invoke "sh" "install.sh")))
         (add-after 'build 'build-doc
           (lambda _
             (with-directory-excursion "doc/manual"
               (and  (invoke "make" "info")
                     (invoke "make" "dist")))))
         (add-after 'build 'build-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (rc (string-append out "/lib/sbcl/sbclrc"))
                    (source-dir (string-append out "/share/sbcl")))
               (for-each (lambda (p)
                           (copy-recursively p (string-append source-dir "/" p)))
                         '("src" "contrib"))
               (mkdir-p (dirname rc))
               (with-output-to-file rc
                 (lambda ()
                   (display
                    (string-append "(sb-ext:set-sbcl-source-location \""
                                   source-dir "\")") ))))))
         (add-after 'install 'remove-coreutils-references
           ;; They are only useful on non-Linux, non-SBCL.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share-dir (string-append out "/share/sbcl/")))
               (substitute* (string-append share-dir "src/code/run-program.lisp")
                 (("\\(run-program \".*uname\"")
                  "(run-program \"uname\""))
               (substitute* (string-append share-dir "contrib/asdf/asdf.lisp")
                 (("\\(\".*/usr/bin/env\"")
                  "(\"/usr/bin/env\""))
               (substitute* (string-append share-dir "contrib/asdf/uiop.lisp")
                 (("\\(\".*/usr/bin/env\"")
                  "(\"/usr/bin/env\"")))))
         (add-after 'install 'install-shared-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib-dir (string-append out "/lib")))
               (install-file "src/runtime/libsbcl.so" lib-dir))))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (old-doc-dir (string-append out "/share/doc"))
                    (new-doc/sbcl-dir (string-append doc "/share/doc/sbcl")))
               (rmdir (string-append old-doc-dir "/sbcl/html"))
               (mkdir-p new-doc/sbcl-dir)
               (copy-recursively (string-append old-doc-dir "/sbcl")
                                 new-doc/sbcl-dir)
               (delete-file-recursively old-doc-dir)))))
       ;; No 'check' target, though "make.sh" (build phase) runs tests.
       #:tests? #f))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))
           (search-path-specification
            (variable "XDG_CONFIG_DIRS")
            (files '("etc")))))
    (home-page "https://www.sbcl.org/")
    (supported-systems (remove target-hurd? %supported-systems))
    (synopsis "Common Lisp implementation")
    (description "Steel Bank Common Lisp (SBCL) is a high performance Common
Lisp compiler.  In addition to the compiler and runtime system for ANSI Common
Lisp, it provides an interactive environment including a debugger, a
statistical profiler, a code coverage tool, and many other extensions.")
    ;; Public domain in jurisdictions that allow it, bsd-2 otherwise.  MIT
    ;; loop macro has its own license.  See COPYING file for further notes.
    (license (list license:public-domain license:bsd-2
                   (license:x11-style "file://src/code/loop.lisp")))))

(define man-for-txr
  (let ((commit "dfbf19b9a96474b8c1bacac85e43605e5691ceb2")
        ;; Number of additional commits since the last tag (see the output of
        ;; "git describe --tags").
        (revision "41"))
    (package
      (name "man-for-txr")
      (version (git-version "1.6g" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://www.kylheku.com/git/man/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zy0g8fj9nsfwzvg88hyaiy94r8j14xhs8vy2ln2niqdm6x2lvy2"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests.
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-man2html-makefile
             (lambda _
               (substitute* "man2html/Makefile.in"
                 ;; It inadvertently ignores @bindir@.
                 (("^(bindir = \\$\\(DESTDIR\\)\\$\\(PREFIX\\)).*" _ prefix)
                  (string-append prefix "@bindir@\n")))
               #t))
           (add-after 'unpack 'delete-generated-files
             (lambda _
               (for-each delete-file
                         (append
                          (list "conf_script")
                          (map (lambda (d) (string-append d "/Makefile"))
                               '("." "man" "man2html" "src"))
                          (map (lambda (f) (string-append "src/" f))
                               '("makewhatis.in" "man.conf"
                                 "paths.h" "version.h"))))
               #t))
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (setenv "CC" ,(cc-for-target))
               ;; Humor the manually written configure script.
               (invoke "./configure" "+lang" "en" "+fhs"
                       (string-append "-prefix=" (assoc-ref outputs "out")))
               #t)))))
      (home-page "https://www.kylheku.com/cgit/man/")
      (synopsis "Modifications to the man utilities, specifically man2html")
      (description
       "This is a fork of the man utilities intended specifically for building
the HTML documentation of TXR.")
      (license license:gpl2))))

(define-public txr
  (package
    (name "txr")
    (version "299")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://www.kylheku.com/git/txr/")
             (commit (string-append "txr-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hlq79j3vyx5y3ffccjkbqx7mvwzbw9dcfy5jrayfzzqwnsyvr78"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "cc=" #$(cc-for-target))
                   (string-append "--prefix=" #$output))
           #:test-target "tests"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-license-installation
                 (lambda _
                   (substitute* "Makefile"
                     (("INSTALL(,.*LICENSE,.*)\\$\\(datadir\\)" _ match)
                      (string-append "INSTALL" match #$output
                                     "/share/doc/" #$name "-" #$version)))))
               (delete 'install-license-files)
               (add-after 'unpack 'fix-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "stream.c"
                     (("/bin/sh")
                      (search-input-file inputs "/bin/bash")))))
               (add-after 'unpack 'fix-tests
                 (lambda _
                   (substitute* (list "tests/017/realpath.tl"
                                      "tests/017/realpath.expected")
                     (("/usr/bin") "/"))))
               (replace 'configure
                 ;; ./configure is a hand-written script that can't handle
                 ;; standard autotools arguments like CONFIG_SHELL.
                 (lambda* (#:key configure-flags #:allow-other-keys)
                   (setenv "txr_shell" (which "bash"))
                   (apply invoke "./configure" configure-flags)))
               (add-after 'build 'build-doc
                 (lambda _
                   (setenv "GS_GENERATE_UUIDS" "0")
                   (invoke "make" "txr-manpage.html" "txr-manpage.pdf")))
               (add-after 'install 'install-doc
                 (lambda _
                   (let ((doc (string-append #$output "/share/doc/"
                                             #$name "-" #$version)))
                     (for-each (lambda (f) (install-file f doc))
                               '("txr-manpage.html" "txr-manpage.pdf")))))
               (add-after 'install 'install-vim-files
                 (lambda _
                   (let ((syntax (string-append
                                   #$output
                                   "/share/vim/vimfiles/pack/guix/start/txr/syntax")))
                     (install-file "tl.vim" syntax)
                     (install-file "txr.vim" syntax)))))))
    (native-inputs
     ;; Required to build the documentation.
     (list ghostscript
           groff
           man-for-txr))
    (inputs
     (list bash-minimal
           libffi
           libxcrypt
           zlib))
    (synopsis "General-purpose, multi-paradigm programming language")
    (description
     "TXR is a general-purpose, multi-paradigm programming language.  It
comprises two languages integrated into a single tool: a text scanning and
extraction language referred to as the TXR Pattern Language (sometimes just
\"TXR\"), and a general-purpose dialect of Lisp called TXR Lisp.  TXR can be
used for everything from \"one liner\" data transformation tasks at the
command line, to data scanning and extracting scripts, to full application
development in a wide-range of areas.")
    (home-page "https://www.nongnu.org/txr/")
    (license license:bsd-2)))
