;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2019 Jesse Gildersleve <jessejohngildersleve@protonmail.com>
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
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages libffcall)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define (asdf-substitutions lisp)
  ;; Prepend XDG_DATA_DIRS/LISP-bundle-systems to ASDF's
  ;; 'default-system-source-registry'.
  `((("\\(,dir \"systems/\"\\)\\)")
     (format #f
             "(,dir \"~a-bundle-systems\")))

      ,@(loop :for dir :in (xdg-data-dirs \"common-lisp/\")
              :collect `(:directory (,dir \"systems\"))"
             ,lisp))))

(define-public gcl
  (let ((commit "d3335e2b3deb63f930eb0328e9b05377744c9512")
        (revision "2")) ;Guix package revision
    (package
      (name "gcl")
      (version (string-append "2.6.12-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/r/gcl.git")
               (commit commit)))
         (file-name (string-append "gcl-" version "-checkout"))
         (sha256
          (base32 "05v86lhvsby05nzvcd3c4k0wljvgdgd0i6arzd2fx1yd67dl6fgj"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-build? #f  ; The build system seems not to be thread safe.
         #:test-target "ansi-tests/test_results"
         #:configure-flags '("--enable-ansi") ; required for use by the maxima package
         #:make-flags (list
                       (string-append "GCL_CC=" (assoc-ref %build-inputs "gcc")
                                      "/bin/gcc")
                       (string-append "CC=" (assoc-ref %build-inputs "gcc")
                                      "/bin/gcc"))
         #:phases
         (modify-phases %standard-phases
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
                                 "/bin/objdump --source \"")))
               #t))
           (add-after 'install 'wrap
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((gcl (assoc-ref outputs "out"))
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
                                         binaries))))
               #t))
           ;; drop strip phase to make maxima build, see
           ;; https://www.ma.utexas.edu/pipermail/maxima/2008/009769.html
           (delete 'strip))))
      (inputs
       `(("gmp" ,gmp)
         ("readline" ,readline)))
      (native-inputs
       `(("m4" ,m4)
         ("texinfo" ,texinfo)))
      (home-page "https://www.gnu.org/software/gcl/")
      (synopsis "A Common Lisp implementation")
      (description "GCL is an implementation of the Common Lisp language.  It
features the ability to compile to native object code and to load native
object code modules directly into its lisp core.  It also features a
stratified garbage collection strategy, a source-level debugger and a built-in
interface to the Tk widget system.")
      (license license:lgpl2.0+))))

(define-public ecl
  (package
    (name "ecl")
    (version "16.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://common-lisp.net/project/ecl/static/files/release/"
             name "-" version ".tgz"))
       (sha256
        (base32 "0m0j24w5d5a9dwwqyrg0d35c0nys16ijb4r0nyk87yp82v38b9bn"))
       (modules '((guix build utils)))
       (snippet
        ;; Add ecl-bundle-systems to 'default-system-source-registry'.
        `(begin
           (substitute* "contrib/asdf/asdf.lisp"
             ,@(asdf-substitutions name))
           #t))))
    (build-system gnu-build-system)
    ;; src/configure uses 'which' to confirm the existence of 'gzip'.
    (native-inputs `(("which" ,which)))
    (inputs `(("gmp" ,gmp)
              ("libatomic-ops" ,libatomic-ops)
              ("libgc" ,libgc)
              ("libffi" ,libffi)))
    (arguments
     '(#:configure-flags '("--without-rt")
       #:tests? #t
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'wrap
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
               (("\\$\\{exec_prefix\\}/") ""))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (home-page "http://ecls.sourceforge.net/")
    (synopsis "Embeddable Common Lisp")
    (description "ECL is an implementation of the Common Lisp language as
defined by the ANSI X3J13 specification.  Its most relevant features are: a
bytecode compiler and interpreter, being able to compile Common Lisp with any
C/C++ compiler, being able to build standalone executables and libraries, and
supporting ASDF, Sockets, Gray streams, MOP, and other useful components.")
    ;; Note that the file "Copyright" points to some files and directories
    ;; which aren't under the lgpl2.0+ and instead contain many different,
    ;; non-copyleft licenses.
    (license license:lgpl2.0+)))

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
        (base32 "0k2dmgl0miz3767iks4p0mvp6xw0ysyxhjpklyh11j010rmh6hqb"))
       (patches (search-patches "clisp-remove-failing-test.patch"))))
    (build-system gnu-build-system)
    (inputs `(("libffcall" ,libffcall)
              ("ncurses" ,ncurses)
              ("readline" ,readline)
              ("libsigsegv" ,libsigsegv)))
    (arguments
     `(#:configure-flags '(,@(if (string-prefix? "armhf-linux"
                                                 (or (%current-system)
                                                     (%current-target-system)))
                                 '("CFLAGS=-falign-functions=4")
                                 '())
                            "--with-dynamic-ffi"
                            "--with-dynamic-modules"
                            "--with-module=rawsock")
       #:build #f
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
             #t)))
       ;; Makefiles seem to have race conditions.
       #:parallel-build? #f))
    (home-page "https://clisp.sourceforge.io/")
    (synopsis "A Common Lisp implementation")
    (description
     "GNU CLISP is an implementation of ANSI Common Lisp.  Common Lisp is a
high-level, object-oriented functional programming language.  CLISP includes
an interpreter, a compiler, a debugger, and much more.")
    (license license:gpl2+)))

(define-public sbcl
  (package
    (name "sbcl")
    (version "1.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sbcl/sbcl/" version "/sbcl-"
                           version "-source.tar.bz2"))
       (sha256
        (base32 "0k7zjrky8r2krkd8780cph214hiihg9nh5rxn4nrhg6i6f8jymw4"))
       (modules '((guix build utils)))
       (snippet
        ;; Add sbcl-bundle-systems to 'default-system-source-registry'.
        `(begin
           (substitute* "contrib/asdf/asdf.lisp"
             ,@(asdf-substitutions name))
           #t))))
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
     ;; ECL too.  ECL builds SBCL about 20% slower than CLISP.  As of
     ;; 2019-09-05, ECL was last updated in 2016 while CLISP was last update
     ;; in 2010.
     ;;
     ;; For now we stick to CLISP for all systems.  We keep the `match' in to
     ;; make it easier to change the host compiler for various architectures.
     `(,@(match (%current-system)
           ((or "x86_64-linux" "i686-linux")
            `(("clisp" ,clisp)))
           (_
            `(("clisp" ,clisp))))
       ("which" ,which)
       ("inetutils" ,inetutils)         ;for hostname(1)
       ("ed" ,ed)
       ("texlive" ,(texlive-union (list texlive-tex-texinfo)))
       ("texinfo" ,texinfo)
       ("zlib" ,zlib)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
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
         ;; FIXME: the texlive-union insists on regenerating fonts.  It stores
         ;; them in HOME, so it needs to be writeable.
         (add-before 'build 'set-HOME
           (lambda _ (setenv "HOME" "/tmp") #t))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (invoke "sh" "make.sh" ,@(match (%current-system)
                                        ((or "x86_64-linux" "i686-linux")
                                         `("clisp"))
                                        (_
                                         `("clisp")))
                     (string-append "--prefix="
                                    (assoc-ref outputs "out"))
                     "--with-sb-core-compression"
                     "--with-sb-xref-for-internals")))
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
                                   source-dir "\")") )))
               #t)))
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
               (delete-file-recursively old-doc-dir)
               #t))))
         ;; No 'check' target, though "make.sh" (build phase) runs tests.
         #:tests? #f))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (home-page "http://www.sbcl.org/")
    (synopsis "Common Lisp implementation")
    (description "Steel Bank Common Lisp (SBCL) is a high performance Common
Lisp compiler.  In addition to the compiler and runtime system for ANSI Common
Lisp, it provides an interactive environment including a debugger, a
statistical profiler, a code coverage tool, and many other extensions.")
    ;; Public domain in jurisdictions that allow it, bsd-2 otherwise.  MIT
    ;; loop macro has its own license.  See COPYING file for further notes.
    (license (list license:public-domain license:bsd-2
                   (license:x11-style "file://src/code/loop.lisp")))))

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
    (version "1.11.5")
    (source #f)
    (build-system gnu-build-system)
    ;; CCL consists of a "lisp kernel" and "heap image", both of which are
    ;; shipped in precompiled form in source tarballs.  The former is a C
    ;; program which we can rebuild from scratch, but the latter cannot be
    ;; generated without an already working copy of CCL, and is platform
    ;; dependent, so we need to fetch the correct tarball for the platform.
    (inputs
     `(("ccl"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/Clozure/ccl/releases/download/v" version
                 "/ccl-" version "-"
                 (match (%current-system)
                   ((or "i686-linux" "x86_64-linux") "linuxx86")
                   ("armhf-linux" "linuxarm")
                   ;; Prevent errors when querying this package on unsupported
                   ;; platforms, e.g. when running "guix package --search="
                   (_ "UNSUPPORTED"))
                 ".tar.gz"))
           (sha256
            (base32
             (match (%current-system)
               ((or "i686-linux" "x86_64-linux")
                "0hs1f3z7crgzvinpj990kv9gvbsipxvcvwbmk54n51nasvc5025q")
               ("armhf-linux"
                "0p0l1dzsygb6i1xxgbipjpxkn46xhq3jm41a34ga1qqp4x8lkr62")
               (_ ""))))))))
    (native-inputs
     `(("m4" ,m4)
       ("subversion" ,subversion)))
    (arguments
     `(#:tests? #f                      ;no 'check' target
       #:modules ((srfi srfi-26)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "xzvf" (assoc-ref inputs "ccl"))
             (chdir "ccl")
             #t))
         (delete 'configure)
         (add-before 'build 'pre-build
           ;; Enter the source directory for the current platform's lisp
           ;; kernel, and run 'make clean' to remove the precompiled one.
           (lambda _
             (substitute* "lisp-kernel/m4macros.m4"
               (("/bin/pwd") (which "pwd")))
             (chdir (string-append
                     "lisp-kernel/"
                     ,(match (or (%current-target-system) (%current-system))
                        ("i686-linux"   "linuxx8632")
                        ("x86_64-linux" "linuxx8664")
                        ("armhf-linux"  "linuxarm")
                        ;; Prevent errors when querying this package
                        ;; on unsupported platforms, e.g. when running
                        ;; "guix package --search="
                        (_              "UNSUPPORTED"))))
             (substitute* '("Makefile")
               (("/bin/rm") "rm"))
             (setenv "CC" "gcc")
             (invoke "make" "clean")))
         ;; XXX Do we need to recompile the heap image as well for Guix?
         ;; For now just use the one we already got in the tarball.
         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
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
                     ,(match (or (%current-target-system) (%current-system))
                        ("i686-linux"   "lx86cl")
                        ("x86_64-linux" "lx86cl64")
                        ("armhf-linux"  "armcl")
                        ;; Prevent errors when querying this package
                        ;; on unsupported platforms, e.g. when running
                        ;; "guix package --search="
                        (_              "UNSUPPORTED")))
                    (heap (string-append kernel ".image")))
               (install-file kernel libdir)
               (install-file heap libdir)

               (let ((dirs '("lib" "library" "examples" "tools" "objc-bridge"
                             ,@(match (%current-system)
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
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))
    (home-page "https://ccl.clozure.com/")
    (synopsis "Common Lisp implementation")
    (description "Clozure CL (often called CCL for short) is a Common Lisp
implementation featuring fast compilation speed, native threads, a precise,
generational, compacting garbage collector, and a convenient foreign-function
interface.")
    ;; See file doc/LICENSE for clarifications it makes regarding how the LGPL
    ;; applies to Lisp code according to them.
    (license (list license:lgpl2.1
                   license:clarified-artistic)))) ;TRIVIAL-LDAP package

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
    (native-inputs `(("intltool" ,intltool)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-net" ,sdl-net)
       ("sdl-ttf" ,sdl-ttf)
       ("lapack" ,lapack)
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
    (home-page "http://lush.sourceforge.net/")
    (license license:lgpl2.1+)))

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
       `(("perl" ,perl)))
      (inputs
       `(("libgc" ,libgc)))
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

(define-public txr
  (package
    (name "txr")
    (version "224")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "http://www.kylheku.com/git/txr/")
             (commit (string-append "txr-" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "txr-shell.patch"))
       (sha256
        (base32
         "1036k71f6mffy9rjwzmhr5nnp1n0wzb0rqvilpzvb8jc5yxv0810"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("cc=gcc")
       #:phases (modify-phases %standard-phases
                  (add-after 'configure 'fix-tests
                    (lambda _
                      (substitute* "tests/017/realpath.tl"
                        (("/usr/bin") "/"))
                      (substitute* "tests/017/realpath.expected"
                        (("/usr/bin") "/"))
                      #t))
                  (replace 'check
                    (lambda _
                      (invoke "make" "tests"))))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("libffi" ,libffi)))
    (synopsis "General-purpose, multi-paradigm programming language")
    (description
     "TXR is a general-purpose, multi-paradigm programming language.  It
comprises two languages integrated into a single tool: a text scanning and
extraction language referred to as the TXR Pattern Language (sometimes just
\"TXR\"), and a general-purpose dialect of Lisp called TXR Lisp.  TXR can be
used for everything from \"one liner\" data transformation tasks at the
command line, to data scanning and extracting scripts, to full application
development in a wide-range of areas.")
    (home-page "https://nongnu.org/txr/")
    (license license:bsd-2)))
