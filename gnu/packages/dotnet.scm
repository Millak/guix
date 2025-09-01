;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 unmush <unmush@hashbang.sh>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2025 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu packages dotnet)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages base)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages instrumentation)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (ice-9 match))

(define-public treecc
  (package
    (name "treecc")
    (version "0.3.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah/dotgnu-pnet/treecc-" version ".tar.gz"))
              (sha256
               (base32
                "1rzgnspg2xccdq3qsx0vi3j28h4qkrzbrjnhzvnny34fjfk217ay"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/dotgnu/")
    (synopsis "Tree Compiler-Compiler")
    (description "The treecc program is designed to assist in the development
of compilers and other language-based tools.  It manages the generation of
code to handle abstract syntax trees and operations upon the trees.")
    (license license:gpl2+)))

;; Several improvements occurred past the 0.8.0 release that make it
;; easier to bootstrap mono.
(define-public pnet-git
  (let ((commit "3baf94734d8dc3fdabba68a8891e67a43ed6c4bd")
        (version "0.8.0")
        (revision "0"))
    (package
      (name "pnet-git")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/dotgnu-pnet/pnet.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0vznvrgz8l0mpib1rz5v3clr7cn570vyp80f7f1jvzivnc1imzn6"))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
                     (for-each delete-file-recursively '("libffi" "libgc"))
                     (for-each delete-file
                               (append
                                 (filter file-exists?
                                         '("compile"
                                           "configure"
                                           "config.guess"
                                           "config.sub"
                                           "depcomp"
                                           "install-sh"
                                           "ltconfig"
                                           "ltcf-c.sh"
                                           "ltmain.sh"))
                                 (find-files "." "Makefile(\\.in)?$")
                                 (find-files "." "_(grammar|scanner)\\.(c|h)$")))
                     ;; Fix to not require bundled dependencies
                     (substitute* "configure.in"
                       (("GCLIBS='.*libgc.a'") "GCLIBS='-lgc'")
                       ;; AC_SEARCH_LIBJIT checks hardcoded header locations
                       (("search_libjit=true")
                        (string-append "search_libjit=false\n"
                                       "JIT_LIBS=-ljit")))
                     (substitute* "Makefile.am"
                       (("OPT_SUBDIRS \\+= lib.*") ""))
                     (substitute* "support/hb_gc.c"
                       (("#include .*/libgc/include/gc.h.")
                        "#include <gc.h>")
                       (("#include .*/libgc/include/gc_typed.h.")
                        "#include <gc/gc_typed.h>"))
                     (substitute* (list "codegen/Makefile.am"
                                        "cscc/bf/Makefile.am"
                                        "cscc/csharp/Makefile.am"
                                        "cscc/c/Makefile.am"
                                        "cscc/java/Makefile.am")
                       ;; Generated files aren't prerequisites
                       (("TREECC_OUTPUT =.*") ""))
                     (substitute* "cscc/csharp/cs_grammar.y"
                       (("YYLEX") "yylex()"))
                     (substitute* "cscc/common/cc_main.h"
                       (("CCPreProc CCPreProcessorStream;" all)
                        (string-append "extern " all)))
                     (substitute* "csdoc/scanner.c"
                       (("int\ttoken;" all)
                        (string-append "extern " all)))
                     (substitute* "doc/cvmdoc.py"
                       (("python1.5")
                        "python"))
                     (substitute* "profiles/full"
                       ;; If this is left unmodified, it causes a segfault in
                       ;; pnetlib's tests.  Unrollers are somewhat
                       ;; architecture-specific anyway, and it will fall back
                       ;; to using GNU C's labels-as-values feature (it can be
                       ;; made to further fall back to fully
                       ;; standards-portable interpreter implementations).
                       (("IL_CONFIG_UNROLL=y")
                        "IL_CONFIG_UNROLL=n"))))
                (patches (search-patches "pnet-newer-libgc-fix.patch"
                                         "pnet-newer-texinfo-fix.patch"
                                         "pnet-fix-line-number-info.patch"
                                         "pnet-fix-off-by-one.patch"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf
             automake
             bison
             flex
             libatomic-ops
             libtool
             python-minimal-wrapper
             texinfo
             treecc))
      (inputs
       (cons* libffi
              libgc
              (if (supported-package? libjit)
                  (list libjit)
                  '())))
      (arguments
       (append (if (this-package-input "libjit")
                   (list #:configure-flags #~(list "--with-jit"))
                   '())
               (list #:make-flags
                     #~(list (string-append
                              "CFLAGS=-O2 -g -Wno-pointer-to-int-cast"
                              " -Wno-error=implicit-function-declaration"
                              " -Wno-error=incompatible-pointer-types")))))
      (native-search-paths
       (list (search-path-specification
              (variable "CSCC_LIB_PATH")
              (files (list "lib/cscc/lib")))))
      (home-page "http://www.gnu.org/software/dotgnu/html2.0/pnet.html")
      (synopsis "Compiler for the C# programming language")
      (description
       "The goal of this project is to build a suite of free software tools
to build and execute .NET applications, including a C# compiler,
assembler, disassembler, and runtime engine.")
      (license license:gpl2+))))

(define-public pnetlib-git
  (let ((version "0.8.0")
        (commit "c3c12b8b0c65f5482d03d6a4865f7670e98baf4c")
        (revision "0"))
    (package
      (name "pnetlib-git")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url
                       "https://git.savannah.gnu.org/git/dotgnu-pnet/pnetlib.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04dikki3lr3m1cacirld90rpi95656b2y2mc5rkycb7s0yfdz1nk"))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
                     (for-each delete-file
                               (append (filter file-exists?
                                               '("configure"
                                                 "config.guess"
                                                 "config.sub"
                                                 "install-sh"
                                                 "ltmain.sh"))
                                       (find-files "." "Makefile(\\.in)?$")))
                     (substitute* (find-files "tests" "^Makefile\\.am$")
                       (("TESTS_ENVIRONMENT.*")
                        (string-append
                         "LOG_COMPILER = $(SHELL)\n"
                         "AM_LOG_FLAGS = $(top_builddir)/tools/run_test.sh"
                         " $(top_builddir)")))
                     (substitute* "tools/run_test.sh.in"
                       (("en_US") "en_US.utf8"))
                     (substitute* "tools/wrapper.sh.in"
                       (("exec .LN_S clrwrap ..1.")
                        (string-append
                         "echo '#!@SHELL@' >> $1\n"
                         "echo exec $CLRWRAP"
                         " $(dirname $(dirname $1))"
                         "/lib/cscc/lib/$(basename $1).exe >> $1\n"
                         "chmod +x $1")))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags #~(list "CFLAGS=-O2 -g -Wno-pointer-to-int-cast")
        #:tests? (and (not (%current-target-system))
                      (not (target-aarch64?)))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'disable-x11-tests
              (lambda _
                (substitute* "tests/Makefile.am"
                  ;; This actually always fails, for a number of
                  ;; reasons:
                  ;; 1. We have no libx11 present, nor do we have an X display
                  ;;    present.  This will cause libXsharpSupport.so to be
                  ;;    built with only shims that fail at runtime.
                  ;; 2. No mechanism is provided for
                  ;;    tests/System.Windows.Forms/TestForms.dll to find
                  ;;    libXsharpSupport.so, which seems to sit at
                  ;;    Xsharp/.libs/libXsharpSupport.so.
                  ;; With a libjit pnet,
                  ;; System.Drawing.Toolkit.ToolkitHandler.CreateDefaultToolkit
                  ;; throws ArgumentNullException when invoking Assembly.Load,
                  ;; while a cvm pnet instead succeeds temporarily, but then
                  ;; fails when invoking
                  ;; System.Drawing.Toolkit.DrawingToolkit..ctor.  For some
                  ;; reason this results in csunit passing the former and
                  ;; failing the latter.
                  (("System\\.Windows\\.Forms") "")))))))
      (native-inputs
       (list autoconf automake libtool treecc))
      (inputs
       (list pnet-git))
      (home-page "http://www.gnu.org/software/dotgnu/html2.0/pnet.html")
      (synopsis "Libraries for the C# programming language")
      (description
       "DotGNU Portable.NET Library contains an implementation of the C# library,
for use with .NET-capable runtime engines and applications.")
      (license license:gpl2+))))

(define prepare-mono-source-0
  #~((when (file-exists? "configure")
       (delete-file "configure"))
     (when (file-exists? "libgc")
       (delete-file-recursively "libgc"))
     ;; just to be sure
     (for-each delete-file
               (find-files "." "\\.(dll|exe|DLL|EXE|so)$"))
     ;; We deleted docs/AgilityPack.dll earlier (if it existed), and it's
     ;; required for building the documentation, so skip building the
     ;; documentation.  According to docs/README, "the sources to this DLL
     ;; live in GNOME CVS module beagle/Filters/AgilityPack".
     (substitute* "Makefile.am"
       (("^(|DIST_|MOONLIGHT_|MONOTOUCH_)SUBDIRS =.*" all)
        (string-replace-substring
         (string-replace-substring
          (string-replace-substring all " docs" "")
          " $(libgc_dir)" "")
         " libgc" "")))))

;; A lot of the fixes are shared between many versions, and it doesn't hurt to
;; apply them to versions before or after they are necessary, so just include
;; them all.
(define prepare-mono-source
  #~(begin
      #$@prepare-mono-source-0
      (substitute* (filter file-exists?
                           '("configure.in"
                             "configure.ac"))
        (("int f = isinf \\(1\\);")
         "int f = isinf (1.0);"))
      ;; makedev is in <sys/sysmacros.h> now.  Include it.
      (substitute* "mono/io-layer/processes.c"
        (("#ifdef HAVE_SYS_MKDEV_H") "#if 1")
        (("sys/mkdev.h") "sys/sysmacros.h"))
      (substitute* (filter file-exists? '("mono/metadata/boehm-gc.c"))
        (("GC_set_finalizer_notify_proc")
         "GC_set_await_finalize_proc")
        (("GC_toggleref_register_callback")
         "GC_set_toggleref_func"))
      (substitute* (filter file-exists? '("mono/utils/mono-compiler.h"))
        (("static __thread gpointer x MONO_TLS_FAST")
         "static __thread gpointer x __attribute__((used))"))
      ;; Since the time the old mono versions were written at, gcc has started
      ;; removing more things it thinks are unused (for example because they
      ;; are only referenced in inline assembly of some sort).
      (substitute* (filter file-exists? '("mono/metadata/sgen-alloc.c"))
        (("static __thread char \\*\\*tlab_next_addr")
         "static __thread char **tlab_next_addr __attribute__((used))"))
      (substitute* (filter file-exists? '("mono/utils/mono-compiler.h"))
        (("#define MONO_TLS_FAST ")
         "#define MONO_TLS_FAST __attribute__((used)) "))))

(define-public mono-1.2.6
  (let ((cflags (string-append "-O2 -g -DARG_MAX=500 "
                               "-Wno-error=implicit-function-declaration "
                               "-Wno-error=incompatible-pointer-types "
                               "-Wno-error=implicit-int "
                               "-Wno-error=return-mismatch")))
    (package
      (version "1.2.6")
      (name "mono")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://download.mono-project.com/sources/mono/"
                      "mono-" version ".tar.bz2"))
                (sha256
                 (base32 "03sn7wyvrjkkkbrqajpmqifxfn83p30qprizpb3m6c5cdhwlzk14"))
                (modules '((guix build utils)
                           (ice-9 string-fun)))
                (snippet #~(begin
                             #$prepare-mono-source
                             (with-directory-excursion
                               "mcs/class/System/System.Text.RegularExpressions"
                               (delete-file "BaseMachine.cs")
                               ;; Can't patch a file with different line endings,
                               ;; so the patch creates a new one, and we overwrite
                               ;; the old one here.
                               (rename-file "BaseMachine.cs-2"
                                            "BaseMachine.cs"))))
                (patches (search-patches "mono-1.2.6-bootstrap.patch"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf
             automake
             bison
             libtool
             pnet-git
             pnetlib-git
             pkg-config))
      (inputs
       (list glib
             libgc
             libx11
             zlib))
      (arguments
       (list
        #:configure-flags #~(list "--with-gc=boehm")
        #:make-flags #~(list (string-append "EXTERNAL_MCS="
                                            #+(this-package-native-input "pnet-git")
                                            "/bin/cscc")
                             (string-append "EXTERNAL_RUNTIME="
                                            #+(this-package-native-input "pnet-git")
                                            "/bin/ilrun")
                             (string-append "CFLAGS=" #$cflags)
                             #$(string-append "CC=" (cc-for-target))
                             "V=1")
        ;; build fails nondeterministically without this
        #:parallel-build? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-includes
              (lambda _
                ;; Upstream forgot to #include that.
                (substitute* "mono/metadata/security.c"
                 (("#include <mono/metadata/image.h>")
                  "#include <mono/metadata/image.h>
#include <mono/metadata/assembly.h>"))))
            (add-after 'unpack 'set-env
              (lambda _
                ;; Configure script for sock_un.sun_path uses exit() without importing it.
                (setenv "CFLAGS" #$cflags)
                ;; All tests under mcs/class fail trying to access $HOME
                (setenv "HOME" "/tmp")
                ;; ZIP files have "DOS time" which starts in Jan 1980.
                (setenv "SOURCE_DATE_EPOCH" "315532800"))))
        ;; System.Object isn't marked as serializable because it causes issues
        ;; with compiling with pnet (circular class reference between Object and
        ;; SerializableAttribute), and this causes tests to fail.
        #:tests? #f))
      (native-search-paths
       (list (search-path-specification
              (variable "MONO_PATH")
              (files (list "lib/mono")))))
      (synopsis "Compiler and libraries for the C# programming language")
      (description "Mono is a compiler, vm, debugger and set of libraries for C#
a C-style programming language from Microsoft that is very similar to Java.")
      (home-page "https://www.mono-project.com/")
      ;; See ./LICENSE
      (license (list
                ;; most of mcs/tools, mono/man, most of mcs/class, tests by
                ;; default, mono/eglib
                license:x11
                ;; mcs/mcs, mcs/gmcs, some of mcs/tools
                license:gpl1+ ;; note: ./mcs/LICENSE.GPL specifies no version
                ;; mono/mono (the mono VM, I think they meant mono/mini)
                license:lgpl2.0+ ;; note: ./mcs/LICENSE.LGPL specifies no version
                ;; mcs/jay
                license:bsd-4)))))

(define-public mono-1.9.1
  (package
    (inherit mono-1.2.6)
    (version "1.9.1")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit "mono-1.9.1.1")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s1n3zdhc2alk9smxfdl1kjz7lz2p19gs0ks4hgr864jlmf13bws"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet prepare-mono-source)
              (patches (search-patches
                         "mono-1.9.1-fixes.patch"
                         "mono-1.9.1-add-MONO_CREATE_IMAGE_VERSION.patch"
                         "mono-1.9.1-reproducibility.patch"))))
    (native-inputs
     (modify-inputs (package-native-inputs mono-1.2.6)
       (delete "pnet-git")
       (delete "pnetlib-git")
       (prepend mono-1.2.6)
       (append which)
       ;; needed for tests
       (append perl)))
    (arguments
     (substitute-keyword-arguments (package-arguments mono-1.2.6)
       ((#:make-flags _ #f)
        #~(list (string-append "CFLAGS=-O2 -g -DARG_MAX=500 "
                               "-Wno-error=implicit-function-declaration "
                               "-Wno-error=incompatible-pointer-types "
                               "-Wno-error=implicit-int "
                               "-Wno-error=return-mismatch")
                #$(string-append "CC=" (cc-for-target))
                "NO_SIGN_ASSEMBLY=yes" ; non-reproducible otherwise.
                "V=1"))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'install 'delete-mdb
              (lambda _
                ;; Those are a source of non-reproducibility--because of the
                ;; random GUIDs.  We are also nerfing the module GUIDs anyway
                ;; so I don't think .net still knows which mdb module is for
                ;; what implementation module.
                (for-each delete-file (find-files "." "[.]mdb$"))))
            ;; Note: Would also work directly after unpack.
            (add-after 'configure 'disable-signing
              (lambda _
                ;; This would be a source of non-reproducibility and have no /keyfile.
                (substitute* "mcs/class/IBM.Data.DB2/Makefile"
                 (("^LIB_MCS_FLAGS =")
                  "LIB_MCS_FLAGS = /delaysign+ "))
                ;; This would be a source of non-reproducibility.
                (substitute* "mcs/class/FirebirdSql.Data.Firebird/Assembly/AssemblyInfo.cs"
                 (("AssemblyDelaySign[(]false[)]")
                  "AssemblyDelaySign(true)"))))
            (add-before 'configure 'set-cflags
              (lambda _
                ;; apparently can't be set via make flags in this version
                (let ((original (getenv "CFLAGS")))
                  (setenv "CFLAGS" (string-append (or original "")
                                                  (if original " " "")
                                                  "-DARG_MAX=500 "
                                                  "-Wno-error=implicit-function-declaration "
                                                  "-Wno-error=incompatible-pointer-types "
                                                  "-Wno-error=implicit-int "
                                                  "-Wno-error=return-mismatch ")))))
            (add-before 'configure 'set-create-image-version
              (lambda _
                ;; pnet produces v2.x assemblies.  Mono does this weird thing
                ;; where it always produces assemblies of the same version as
                ;; the runtime that is running it, which is based on the
                ;; version of the assembly that it loaded, which is based on
                ;; what it decided for the previous compiler... on and on all
                ;; the way back to pnet.  This breaks that chain, because
                ;; otherwise it ends up compiling the initial mcs against .NET
                ;; 2.0 libraries and then running with .NET 1.0 libraries.
                (setenv "MONO_CREATE_IMAGE_VERSION" "v1.1.4322")))
            (add-after 'unpack 'patch-test-driver-shebang
              (lambda _
                (patch-shebang "mono/tests/test-driver")))))
       ((#:tests? _ #f) #f)
       ((#:parallel-tests? _ #f) #f)))))

(define-public mono-2.4.2
  (package
    (inherit mono-1.9.1)
    (version "2.4.2.3")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit (string-append
                              "mono-" (string-replace-substring version "." "-")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mnrk17rd9c5rh30dh82a39c9ak1ns998b41ivprvy7m068skpda"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet prepare-mono-source)
              (patches (search-patches "mono-2.4.2.3-reproducibility.patch"
                                       "mono-2.4.2.3-fixes.patch"
                                       "mono-2.4.2.3-fix-parallel-builds.patch"))))
    (native-inputs (modify-inputs (package-native-inputs mono-1.9.1)
                     (replace "mono" mono-1.9.1)))
    (inputs (modify-inputs (package-inputs mono-1.9.1)
              (append gettext-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments mono-1.9.1)
       ((#:make-flags _ #f)
        #~(list (string-append "CFLAGS=-O2 -g -DARG_MAX=500 "
                               "-Wno-error=implicit-function-declaration "
                               "-Wno-error=incompatible-pointer-types "
                               "-Wno-error=implicit-int "
                               "-Wno-error=return-mismatch "
                               "-Wno-error=int-conversion ")
                #$(string-append "CC=" (cc-for-target))
                "V=1"))
       ((#:tests? _ #f)
        ;; When it tries building iltests.il in mono/mini, it gets: error
        ;; CS0006: cannot find metadata file `TestDriver.dll'.  It builds fine
        ;; outside of the build environment, but later tests fail, and I can't
        ;; be bothered to figure out what's causing ilasm to not find
        ;; TestDriver.dll.
        #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'disable-mono-mini-timestamps
              (lambda _
                ;; Note: Newer monos have mono/mini/Makefile.am.in .
                (substitute* '("mono/mini/Makefile.am")
                 (("`date`")
                  ;; This timestamp is the same as SOURCE_DATE_EPOCH.
                  "Tue Jan  1 12:00:00 AM UTC 1980"))))
            (add-before 'bootstrap 'patch-sub-autogen.sh-shebang
              (lambda _
                (patch-shebang "eglib/autogen.sh")))))))
    (license (list
              ;; most of mcs/tools, mono/man, most of mcs/class, tests by
              ;; default, mono/eglib
              ;; mcs/mcs, mcs/gmcs (dual-licensed GPL)
              ;; samples
              license:x11
              ;; mcs/mcs, mcs/gmcs (dual-licensed X11)
              ;; some of mcs/tools
              license:gpl1+ ;; note: ./mcs/LICENSE.GPL specifies no version
              ;; mono/mono (the mono VM, I think they meant mono/mini)
              license:lgpl2.0+ ;; note: ./mcs/LICENSE.LGPL specifies no version
              ;; mcs/jay
              license:bsd-4))))

(define-public mono-2.6.4
  (package
    (inherit mono-2.4.2)
    (version "2.6.4")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit (string-append "mono-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17977w45qh8jmfsl4bhi83si4fxd8s3x8b0pxnwdzjv3bqr54c85"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet prepare-mono-source)
              (patches (search-patches "mono-2.4.2.3-reproducibility.patch"
                                       "mono-2.6.4-fixes.patch"))))
    (native-inputs (modify-inputs (package-native-inputs mono-2.4.2)
                     (replace "mono" mono-2.4.2)))))

;; submodule checkouts use git://, which isn't supported by github anymore, so
;; we need to manually provide them instead of being able to use (recursive?
;; #t).  Also try not to think too hard about the fact that some of these
;; submodules in later versions contain binary compiler blobs which mono
;; maintainers presumably used when creating the bootstrap binaries they
;; published.  All fetched and updated over unauthenticated git://.

(define mono-2.11.4-external-repo-specs
  ;; format: ({reponame OR (reponame dir-name)} commit-hash origin-sha256) ...
  ;; if reponame starts with https:// it is treated as the repository url,
  ;; otherwise the name of a repository under https://github.com/mono/
  '(("aspnetwebstack"               "1836deff6a2683b8a5b7dd78f2b591a10b47573e"
     "0vqq45i8k6jylljarr09hqqiwjs8wn0lgjrl6bz72vxqpp0j344k")
    ("cecil"                        "54e0a50464edbc254b39ea3c885ee91ada730705"
     "007szbf5a14q838695lwdp7ap6rwzz3kzllgjfnibzlqipw3x2yk")
    ("entityframework"              "9baca562ee3a747a41870f45e749e4436b6aca26"
     "0l8k04bykbrbk5q2pz8hzh8xy8y4ayz7j97fw0kyk3lrai89v5da")
    ("Newtonsoft.Json"              "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")))

(define (add-external-repos specs)
  (define (reponame->url reponame)
    (if (string-prefix? "https://" reponame)
        reponame
        (string-append "https://github.com/mono/" reponame)))

  (define* (external-repo-gexp reponame commit hash
                               #:key recursive? (patches '()))
    (let ((short-commit (string-take commit 6))
          (reponame (if (pair? reponame) (car reponame)
                        reponame))
          (dir-name (if (pair? reponame) (cadr reponame)
                        reponame)))
      #~(copy-recursively #+(origin
                              (method git-fetch)
                              (uri (git-reference
                                    (url (reponame->url reponame))
                                    (commit commit)
                                    (recursive? recursive?)))
                              (file-name
                               (git-file-name dir-name
                                              short-commit))
                              (sha256 (base32 hash))
                              (patches (map search-patch patches)))
                          #$(string-append "./external/" dir-name))))

  (define (spec->gexp spec)
    (apply external-repo-gexp spec))

  #~(begin
      #+@(map spec->gexp specs)))

(define-public mono-2.11.4
  (package
    (inherit mono-2.6.4)
    (version "2.11.4")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit (string-append "mono-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y2bifi2avbjmfp80hjga2dyqip4b46zkvx6yfr9pa2hhm940rpx"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-2.11.4-external-repo-specs)
                           #$prepare-mono-source))
              (patches (search-patches "mono-2.11.4-fixes.patch"))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments mono-2.6.4)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (delete 'disable-signing)))))
    (native-inputs (modify-inputs (package-native-inputs mono-2.6.4)
                     (replace "mono" mono-2.6.4)))
    (license (list
              ;; most of mcs/tools, mono/man, most of mcs/class, tests by
              ;; default, mono/eglib, mono/metadata/sgen*,
              ;; mono/arch/*/XXX-codegen.h
              ;; mcs/mcs, mcs/gmcs (dual-licensed GPL)
              ;; samples
              license:x11
              ;; mcs/mcs, mcs/gmcs (dual-licensed X11)
              ;; some of mcs/tools
              license:gpl1+ ;; note: ./mcs/LICENSE.GPL specifies no version
              ;; mono/mono (the mono VM, I think they meant mono/mini)
              license:lgpl2.0+ ;; note: ./mcs/LICENSE.LGPL specifies no version
              ;; mcs/jay
              license:bsd-4
              ;; mcs/class/System.Core/System/TimeZoneInfo.Android.cs
              license:asl2.0))))

(define mono-3.0.12-external-repo-specs
  ;; format: ({reponame OR (reponame dir-name)} commit sha256) ...
  ;; if reponame starts with https:// it is treated as the repository url,
  ;; otherwise the name of a repository under https://github.com/mono/
  '(("aspnetwebstack"               "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    ("cecil"                        "54e0a50464edbc254b39ea3c885ee91ada730705"
     "007szbf5a14q838695lwdp7ap6rwzz3kzllgjfnibzlqipw3x2yk")
    ("entityframework"              "a5faddeca2bee08636f1b7b3af8389bd4119f4cd"
     "0b05pzf6qwdd92pbzym32nfmw8rq36820vdzakq1kykfmddjr9a7")
    (("ikvm-fork" "ikvm")           "10b8312c8024111780ee382688cd4c8754b1f1ac"
     "025wf9gjgfvrq42vgw91ahy3cmzcw094vx783dsp7gjdyd8q09nm")
    ("Lucene.Net"                   "88fb67b07621dfed054d8d75fd50672fb26349df"
     "1rfxqfz7hkp9rg5anvxlv6fna0xi0bnv1y8qbhf8x48l08yjb38k")
    ("Newtonsoft.Json"              "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    ("rx"                           "17e8477b2cb8dd018d49a567526fe99fd2897857"
     "0fyyy4jf0mma6kff6fvbvdcs5ra1bz4s063nvjjva9xlnv7sjvh4")))

(define-public mono-3.0
  (package
    (inherit mono-2.11.4)
    (version "3.0.12")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit (string-append "mono-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "110f3hcfikk6bxbrgjas5dqldci9f24gvm3vdgn4j9j7xhlcx1lj"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-3.0.12-external-repo-specs)
                           #$prepare-mono-source))))
    (arguments
     (substitute-keyword-arguments (package-arguments mono-2.11.4)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (delete 'disable-mono-mini-timestamps)))))
    (native-inputs (modify-inputs (package-native-inputs mono-2.11.4)
                     (replace "mono" mono-2.11.4)))
    (license (list
              ;; most of mcs/tools, mono/man, most of mcs/class, tests by
              ;; default, mono/eglib, mono/metadata/sgen*,
              ;; mono/arch/*/XXX-codegen.h
              ;; mcs/mcs, mcs/gmcs (dual-licensed GPL)
              ;; samples
              license:x11
              ;; mcs/mcs, mcs/gmcs (dual-licensed X11)
              ;; some of mcs/tools
              license:gpl1+ ;; note: ./mcs/LICENSE.GPL specifies no version
              ;; mono/mono (the mono VM, I think they meant mono/mini)
              ;; mono/support (note: directory doesn't exist, probably meant
              ;; ./support, but that contains a copy of zlib?)
              license:lgpl2.0+ ;; note: ./mcs/LICENSE.LGPL specifies no version
              ;; mcs/jay
              license:bsd-4
              ;; mcs/class/System.Core/System/TimeZoneInfo.Android.cs
              license:asl2.0
              ;; ./support, contains a copy of zlib
              license:zlib))))

(define mono-3.12.1-external-repo-specs
  ;; format: ({reponame OR (reponame dir-name)} commit sha256) ...
  '(("aspnetwebstack"               "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    ("cecil"                        "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("entityframework"              "a5faddeca2bee08636f1b7b3af8389bd4119f4cd"
     "0b05pzf6qwdd92pbzym32nfmw8rq36820vdzakq1kykfmddjr9a7")
    ("ikdasm"                       "7ded4decb9c39446be634d42a575fda9bc3d945c"
     "0f3mbfizxmvr5njj123w0wn7sz85v5q2mzwijjql8w1095i0916l")
    (("ikvm-fork" "ikvm")           "22534de2098acbcf208f6b06836d122dab799e4b"
     "1ivywy5sc594sl3bs9xrkna1dbhkp7v1mv79n96ydgq6zcs0698l")
    ("Lucene.Net"                   "88fb67b07621dfed054d8d75fd50672fb26349df"
     "1rfxqfz7hkp9rg5anvxlv6fna0xi0bnv1y8qbhf8x48l08yjb38k")
    ("Newtonsoft.Json"              "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    ("rx"                           "00c1aadf149334c694d2a5096983a84cf46221b8"
     "0ndam0qrnkb4gj21lapqgcy0mqw7s18viswsjyjyaaa4fgqw8kmq")))

(define-public mono-3.12.1
  (package
    (inherit mono-3.0)
    (version "3.12.1")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit (string-append "mono-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01sxrnfch61k8v7av7ccbmy3v37ky8yp8460j6ycnbyfa3305y0f"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-3.12.1-external-repo-specs)
                           #$prepare-mono-source))))
    (native-inputs (modify-inputs (package-native-inputs mono-3.0)
                     (replace "mono" mono-3.0)))
    (arguments
     (substitute-keyword-arguments (package-arguments mono-3.0)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'set-cflags
              (lambda _
                (setenv "CFLAGS"
                 (string-append "-O2 -g "
                                "-Wno-error=implicit-function-declaration "
                                "-Wno-error=incompatible-pointer-types "
                                "-Wno-error=implicit-int "
                                "-Wno-error=return-mismatch "
                                "-Wno-error=int-conversion"))))
            (add-after 'unpack 'set-TZ
              (lambda _
                ;; for some reason a default is only used if this is empty, not
                ;; if it is unset.
                (setenv "TZ" "")))))))))

(define mono-4.9.0-external-repo-specs
  ;; format: ({reponame OR (reponame dir-name)} commit sha256) ...
  '(("aspnetwebstack"               "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    ;; (("reference-assemblies" "binary-reference-assemblies")
    ;;  "6c77197318fe85dfddf75a1b344b9bf8d0007b0b"
    ;;  "11hbs952srjlsiyin76y2llm5rfjkwjc67ya1i3p0pw193zw14jk")
    ;; According to github description this is a "custom" fork of boringssl
    ("boringssl"                    "c06ac6b33d3e7442ad878488b9d1100127eff998"
     "187zpi1rvh9i6jfccwzqq337rxxi1rgny6mjq79r08dlrh0lydzc")
    ("buildtools"                   "9b6ee8686be55a983d886938165b6206cda50772"
     "0sjw3swavcmijynmaxh647qpkjsbgihdr8lhkyzf8dsprhlq4fxd")
    ("cecil"                        "2b39856e80d8513f70bc3241ed05325b0de679ae"
     "0vvax32r6bnhvrcvis83gdrdqcgyxb704hz28g9q0wnay4knqxdm")
    (("cecil" "cecil-legacy")       "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ;; ("debian-snapshot"              "9342f8f052f81deaba789f030db23a88b4369724"
    ;;  "")
    ("ikdasm"                       "e4deabf61c11999f200dcea6f6d6b42474cc2131"
     "1frbf70y7n7l72j393avdiwk6153cvfwwpighkf2m46clqmq4han")
    (("ikvm-fork" "ikvm")           "367864ef810859ae3ce652864233b35f2dd5fdbe"
     "0ig99kbma4s0mzb13nzsk1vm200ygfr11q6mzgh6jj46s2fc35px")
    ("Lucene.Net.Light"             "85978b7eb94738f516824341213d5e94060f5284"
     "0d118i52m3a0vfjhfci81a2kc4qvnj23gs02hrvdrfpd1q92fyii")
    ("Newtonsoft.Json"              "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    ("nuget-buildtasks"             "04bdab55d8de9edcf628694cfd2001561e8f8e60"
     "1nklxayxkdskg5wlfl44cndzqkl18v561rz03hwx7wbn5w89q775")
    ("nunit-lite"                   "4bc79a6da1f0ee538560b7e4d0caff46d3c86e4f"
     "085fpabjw47rn8hb5zw6wizsg2jrgdbj9rnlar9lrls40wig272q")
    ("rx"                           "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")))

(define-public mono-4.9.0
  (package
    (inherit mono-3.12.1)
    (version "4.9.0")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://gitlab.winehq.org/mono/mono.git")
                ;; some commit chosen after configure.ac was updated to make
                ;; the version >= 4.9.0
                (commit "5a3736606e6243d2c84d4df2cf35c284214b8cc4")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vqkkqkaqwbii4hdzg0vffyy31fz1kmmsa67jyqwxdsvgpjszih3"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-4.9.0-external-repo-specs)
                           #$prepare-mono-source))
              (patches (search-patches
                        ;; Saves us an extra intermediate step
                        "mono-4.9.0-fix-runtimemetadataversion.patch"))))
    (native-inputs (modify-inputs (package-native-inputs mono-3.12.1)
                     (replace "mono" mono-3.12.1)
                     (append tzdata-for-tests)))
    (arguments
     (substitute-keyword-arguments (package-arguments mono-3.12.1)
       ((#:configure-flags _ #f)
        ;; "External Boehm is no longer supported" - I VILL NOT use the
        ;; bundled software!
        #~(list "--with-sgen=yes"
                "--disable-boehm"
                "--with-csc=mcs"))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            ;; The files moved and were fixed upstream anyway.
            (delete 'fix-includes)
            ;; GCC static library linking dependency resolution got stricter--so
            ;; we have to add a dependency.
            (add-after 'unpack 'patch-sgen-linking
              (lambda _
                (substitute* "tools/monograph/Makefile.am"
                 (("/mono/metadata/libmonoruntimesgen-static[.]la")
                  (string-append "/mono/metadata/libmonoruntimesgen-static.la "
                                 "$(top_builddir)/mono/sgen/libmonosgen-static.la")))))
            (add-before 'configure 'set-TZDIR
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (search-input-directory (or native-inputs inputs)
                                        "share/zoneinfo")))
            (add-after 'unpack 'use-old-mono-libraries
              ;; At this point in history mono had not, to my knowledge,
              ;; deigned to grace us with the actual sources to the binaries
              ;; shipped in external/binary-reference-assemblies, so just copy
              ;; the libraries from an older mono for now I guess.
              (lambda _
                (substitute* "mcs/class/reference-assemblies/Makefile"
                  (("\\.\\./\\.\\./\\.\\./external/binary-reference-assemblies/v")
                   (string-append #$(this-package-native-input "mono")
                                  "/lib/mono/")))))
            (add-after 'unpack 'disable-Microsoft.Build.Tasks-tests
              (lambda _
                ;; These fail for unknown reasons
                (substitute* "mcs/class/Microsoft.Build.Tasks/Makefile"
                  (("^include ../../build/library.make" all)
                   (string-append
                    all "\nrun-test-recursive:\n\t@echo skipping tests\n")))))))))
    (license (list
              ;; most of mcs/tools, mono/man, most of mcs/class, tests by
              ;; default, mono/eglib, mono/metadata/sgen*,
              ;; mono/arch/*/XXX-codegen.h
              ;; mcs/mcs, mcs/gmcs (dual-licensed GPL)
              ;; samples
              license:x11
              ;; mcs/mcs, mcs/gmcs (dual-licensed X11)
              ;; some of mcs/tools
              license:gpl1+ ;; note: ./mcs/LICENSE.GPL specifies no version
              ;; mono/mono (the mono VM, I think they meant mono/mini)
              ;; mono/support (note: directory doesn't exist, probably meant
              ;; ./support, but that contains a copy of zlib?)
              license:lgpl2.0+ ;; note: ./mcs/LICENSE.LGPL specifies no version
              ;; mcs/jay, mono/utils/memcheck.h
              license:bsd-4
              ;; mono/utils/bsearch.c, mono/io-layer/wapi_glob.{h,c}
              license:bsd-3
              ;; mono/utils/freebsd-{dwarf,elf_common,elf64,elf32}.h
              license:bsd-2
              ;; mcs/class/System.Core/System/TimeZoneInfo.Android.cs
              ;; mcs/class/RabbitMQ.Client (dual licensed mpl1.1)
              license:asl2.0
              ;; ./support, contains a copy of zlib, incl. ./support/minizip
              license:zlib
              ;; mono/docs/HtmlAgilityPack, mcs/unit24
              license:ms-pl
              ;; mcs/class/I18N/mklist.sh, mono/benchmark/{zipmark,logic}.cs
              ;; mcs/class/{,Compat.}ICSharpCode.SharpZipLib
              license:gpl2+
              ;; mcs/class/RabbitMQ.Client (dual licensed asl2.0)
              license:mpl1.1
              ;; API Documentation
              license:cc-by4.0))))

(define mono-5.0.1-external-repo-specs
  '(("aspnetwebstack"              "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    ;; snippet in the actual package will delete all dlls and exes, so this
    ;; should be rebuilt from scratch.
    (("reference-assemblies" "binary-reference-assemblies")
     "febc100f0313f0dc9d75dd1bcea45e87134b5b55"
     "0lpj911m2lq23r22dpy4i02fy4ykf27dx8fvqpxsxknysj2jl6y4")
    ("bockbuild"                   "512ba41a94bec35ff0c395eb71a180fda23da95c"
     "16m00la8svx8v07sxy4zxbpq0cbq7d3nzy53w8kqml8b18h5dabg")
    ("boringssl"                   "c06ac6b33d3e7442ad878488b9d1100127eff998"
     "187zpi1rvh9i6jfccwzqq337rxxi1rgny6mjq79r08dlrh0lydzc")
    ("buildtools"                  "9b6ee8686be55a983d886938165b6206cda50772"
     "0sjw3swavcmijynmaxh647qpkjsbgihdr8lhkyzf8dsprhlq4fxd")
    ("cecil"                       "7801534de1bfed97c844821c3244e05fc7ffcfb8"
     "0dmfyzkm57n3lbgllx6ffz4g84x1slkib9hb4cfp3nhz852qim7b")
    (("cecil" "cecil-legacy")      "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("corefx"                      "bd96ae5f1485ae8541fe476dfd944efde76bcd9c"
     "0j51lc54dmwa4fzna2vjfj4pcd1lk1s5bp5dfix1aqcncyzivazi")
    ("corert"                      "d87c966d80c1274373ddafe3375bf1730cd430ed"
     "078v5ks7inm2g1hf96x19k42jnv1qhhh7r8jxrfc7jk4v4lgmqyf")
    ("ikdasm"                      "e4deabf61c11999f200dcea6f6d6b42474cc2131"
     "1frbf70y7n7l72j393avdiwk6153cvfwwpighkf2m46clqmq4han")
    (("ikvm-fork" "ikvm")          "367864ef810859ae3ce652864233b35f2dd5fdbe"
     "0ig99kbma4s0mzb13nzsk1vm200ygfr11q6mzgh6jj46s2fc35px")
    ("linker"                      "e4d9784ac37b9ebf4757175c92bc7a3ec9fd867a"
     "0ga7br9lqdsycz22dndkbiwbd0c60ml6nl22xlsnjr7lwdccfjvl")
    ("Lucene.Net.Light"            "85978b7eb94738f516824341213d5e94060f5284"
     "0d118i52m3a0vfjhfci81a2kc4qvnj23gs02hrvdrfpd1q92fyii")
    ("Newtonsoft.Json"             "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    (("NuGet.BuildTasks" "nuget-buildtasks")
     "8d307472ea214f2b59636431f771894dbcba7258"
     "1h1frnj0x8k7b29ic4jisch0vlpmsmghjw554pz277f2nxaidljj")
    (("NUnitLite" "nunit-lite")    "690603bea98aae69fca9a65130d88591bc6cabee"
     "1f845ysjzs3yd9gcyww66dnkx484z5fknb8l0xz74sjmxk2mngwc")
    ;; ("roslyn-binaries"          "0d4198b1299bcb019973749da4d47e90f15a1e46"
    ;;  "")
    ("rx"                          "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")))

(define-public mono-5.0.1
  (package
    (inherit mono-4.9.0)
    (version "5.0.1")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://gitlab.winehq.org/mono/mono.git")
                (commit "mono-5.0.1.1")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05z9bddljp8xwsw7qw3f7bic8i202wrc60pjb9fn4igwfz9278n5"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-5.0.1-external-repo-specs)
                           #$@prepare-mono-source-0))))
    (native-inputs (modify-inputs (package-native-inputs mono-4.9.0)
                     (replace "mono" mono-4.9.0)
                     (append cmake-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments mono-4.9.0)
       ((#:make-flags _ #f)
        ;; Build system is buggy here, it does some weird wildcard expansion
        ;; that assumes there's only at most one file in a directory
        #~(list "V=1" "SKIP_AOT=1"))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'disable-roslyn-install
              ;; For some reason there is no predefined way to persuade mono to
              ;; not install the binary blobs it assumes are there.
              (lambda _
                (substitute* "mcs/packages/Makefile"
                  (("^install-local:")
                   (string-append "install-local:
	echo \"Skipping blob install\"

unused0:")))))
            (delete 'use-old-mono-libraries)
            (add-after 'build 'build-reference-assemblies
              (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
                (let ((top (getcwd))
                      ;; parallel-build? needs to be false for mono's build
                      ;; phase, but it should work here.
                      (parallel-build? #t))
                  (with-directory-excursion "external/binary-reference-assemblies"
                    ;; No clue why all these references are missing, just
                    ;; power through I guess.
                    (substitute* (find-files "." "^Makefile$")
                      (("CSC_COMMON_ARGS := " all)
                       (string-append all "-delaysign+ "))
                      (("IBM\\.Data\\.DB2_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Mono\\.Data\\.Sqlite_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Data\\.DataSetExtensions_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Data\\.OracleClient_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.IdentityModel_REFS := " all)
                       (string-append all "System.Configuration "))
                      (("System\\.Design_REFS := " all)
                       (string-append all "Accessibility "))
                      (("System\\.Web\\.Extensions\\.Design_REFS := " all)
                       (string-append all "System.Windows.Forms System.Web "))
                      (("System\\.ServiceModel\\.Routing_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Web\\.Abstractions_REFS := " all)
                       (string-append all "System "))
                      (("System\\.Reactive\\.Windows\\.Forms_REFS := " all)
                       (string-append all "System "))
                      (("System\\.Windows\\.Forms\\.DataVisualization_REFS := " all)
                       (string-append all "Accessibility "))
                      (("Facades/System\\.ServiceModel\\.Primitives_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Dynamic\\.Runtime_REFS := " all)
                       (string-append all "System "))
                      (("Facades/System\\.Xml\\.XDocument_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Runtime\\.Serialization.Xml_REFS := " all)
                       (string-append all "System.Xml ")))
                    (apply invoke "make"
                           `(,@(if parallel-build?
                                   `("-j" ,(number->string
                                            (parallel-job-count)))
                                   '())
                             ,(string-append "CSC=MONO_PATH="
                                             top "/mcs/class/lib/build"
                                             " "
                                             top "/runtime/mono-wrapper"
                                             " "
                                             top "/mcs/class/lib/build/mcs.exe")
                             ,@make-flags))))))))))))

(define mono-5.1.0-external-repo-specs
  '(("aspnetwebstack"              "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    (("reference-assemblies" "binary-reference-assemblies")
     "febc100f0313f0dc9d75dd1bcea45e87134b5b55"
     "0lpj911m2lq23r22dpy4i02fy4ykf27dx8fvqpxsxknysj2jl6y4")
    ("bockbuild"                   "fd1d6c404d763c98b6f0e64e98ab65f92e808245"
     "0l2n9863j5y20lp3fjcpbb0a9jcfk0kqmnzlsw20qchd05rjgyb0")
    ("boringssl"                   "c06ac6b33d3e7442ad878488b9d1100127eff998"
     "187zpi1rvh9i6jfccwzqq337rxxi1rgny6mjq79r08dlrh0lydzc")
    ("buildtools"                  "b5cc6e6ab5f71f6c0be7b730058b426e92528479"
     "0ldj5l4p4q8j9dhk0nifr3m0i64csvb56wlc2xd4zy80sfgmjn06")
    ("cecil"                       "44bc86223530a07fa74ab87007cf264e53d63400"
     "0smsa8i4709y1nky3hshj7ayxhjcc17wlnfdvhfay7ly5dxml84g")
    (("cecil" "cecil-legacy")      "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("corefx"                      "63c51e726292149b4868db71baa883e5ad173766"
     "1406rbra83k6gw2dnnsfqcfwiy1h89y6lq64ma5rckmb5drb0ng9")
    ("corert"                      "31eda261991f9f6c1add1686b6d3799f835b2978"
     "0s0pd4m9070xlx238fdhqf2b3iyd2vzff3f0sxlyi8s0lhsrl8zv")
    ("ikdasm"                      "88b67c42ca8b7d58141c176b46749819bfcef166"
     "0b0b1dhg80r640n81iqawwkxi1k289n4zxjfj0ldd9rkvfxvlwaw")
    (("ikvm-fork" "ikvm")          "7c1e61bec8c069b2cc9e214c3094b147d76bbf82"
     "0vmc5r4j76hkd4zis1769ppdl1h1l7z8cld0y4p1m64n86ghkzfn")
    ("linker"                      "1bdcf6b7bfbe3b03fdaa76f6124d0d7374f08615"
     "1xx6s8dcgcz803yvqgzhcgmj16c9s8vrvvl8k4y0xma5w51kn23k")
    ("Lucene.Net.Light"            "85978b7eb94738f516824341213d5e94060f5284"
     "0d118i52m3a0vfjhfci81a2kc4qvnj23gs02hrvdrfpd1q92fyii")
    ("Newtonsoft.Json"             "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    (("NuGet.BuildTasks" "nuget-buildtasks")
     "04bdab55d8de9edcf628694cfd2001561e8f8e60"
     "1nklxayxkdskg5wlfl44cndzqkl18v561rz03hwx7wbn5w89q775")
    (("NUnitLite" "nunit-lite")    "690603bea98aae69fca9a65130d88591bc6cabee"
     "1f845ysjzs3yd9gcyww66dnkx484z5fknb8l0xz74sjmxk2mngwc")
    ;; ("roslyn-binaries"          "0d4198b1299bcb019973749da4d47e90f15a1e46"
    ;;  "")
    ("rx"                          "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")
    ;; ("xunit-binaries"           "b8e20d265b368dd6252703d5afd038d0b028e388"
    ;;  "")
    ))

(define-public mono-5.1.0
  (package
    (inherit mono-5.0.1)
    (version "5.1.0")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit "6fafd08b507c56f11a2eb6570703a39e5bdc0a81")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sxq40nay5ghhmfbdln98iri19y0h7q36r3pqnxmxnm94livx2k5"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-5.1.0-external-repo-specs)
                           #$@prepare-mono-source-0))))
    (arguments
     (substitute-keyword-arguments (package-arguments mono-5.0.1)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'build 'build-resx2sr
              (lambda* (#:key make-flags #:allow-other-keys)
                (apply invoke "make" "-C" "mcs/tools/resx2sr" make-flags)))
            (add-after 'install 'install-resx2sr
              (lambda* (#:key inputs make-flags #:allow-other-keys)
                (apply invoke "make" "-C" "mcs/tools/resx2sr" "install" make-flags)
                ;; They don't install a wrapper script--but we need it for
                ;; bootstrapping MSBuild.
                (let ((resx2sr (string-append #$output "/bin/resx2sr")))
                  (call-with-output-file resx2sr
                    (lambda (port)
                      (format port "#!~a
exec ~s ~s \"$@\"
"
                              (search-input-file inputs "/bin/bash")
                              (string-append #$output "/bin/mono")
                              (string-append #$output "/lib/mono/4.5/resx2sr.exe"))))
                  (chmod resx2sr #o755))))))))
    (native-inputs (modify-inputs (package-native-inputs mono-5.0.1)
                     (replace "mono" mono-5.0.1)))))

(define mono-5.2.0-external-repo-specs
  '(("aspnetwebstack"              "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    (("reference-assemblies" "binary-reference-assemblies")
     "142cbeb62ffabf1dd9c1414d8dd76f93bcbed0c2"
     "1wkd589hgb16m5zvmp9yb57agyyryaa1jj8vhl4w20i2hp22wad9")
    ("bockbuild"                   "45aa142fa322f5b41051e7f40008f03346a1e119"
     "1sjlgzh3hq251k729a1px707c1q2gnfayghgx1z5qyddnyaxna20")
    ("boringssl"                   "3e0770e18835714708860ba9fe1af04a932971ff"
     "139a0gl91a52k2r6na6ialzkqykaj1rk88zjrkaz3sdxx7nmmg6y")
    ("buildtools"                  "b5cc6e6ab5f71f6c0be7b730058b426e92528479"
     "0ldj5l4p4q8j9dhk0nifr3m0i64csvb56wlc2xd4zy80sfgmjn06")
    ("cecil"                       "362e2bb00fa693d04c2d140a4cd313eb82c78d95"
     "0bvaavlnldrja8ixb66bg33kz05950vm5sk4pz0k0zjgspfgpcvd")
    (("cecil" "cecil-legacy")      "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("corefx"                      "78360b22e71b70de1d8cc9588cb4ef0040449c31"
     "1wrszafyar7q1cdfba68xd6b4d54p3iim2czmxblms1yw19ycqm7")
    ("corert"                      "ed6296dfbb88d66f08601c013caee30c88c41afa"
     "179q1aiq44bzdckg1xqm6iwyx835cp6161w5vgsfrgbw0p3kidxr")
    ("ikdasm"                      "88b67c42ca8b7d58141c176b46749819bfcef166"
     "0b0b1dhg80r640n81iqawwkxi1k289n4zxjfj0ldd9rkvfxvlwaw")
    (("ikvm-fork" "ikvm")          "7c1e61bec8c069b2cc9e214c3094b147d76bbf82"
     "0vmc5r4j76hkd4zis1769ppdl1h1l7z8cld0y4p1m64n86ghkzfn")
    ("linker"                      "c7450ca2669becddffdea7dcdcc06692e57989e1"
     "0vd1vw6hqm1p127m6079p9n4xrckrf4iakvj41hnqfwws94w5mv1")
    ("Lucene.Net.Light"            "85978b7eb94738f516824341213d5e94060f5284"
     "0d118i52m3a0vfjhfci81a2kc4qvnj23gs02hrvdrfpd1q92fyii")
    ("Newtonsoft.Json"             "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    (("NuGet.BuildTasks" "nuget-buildtasks")
     "8d307472ea214f2b59636431f771894dbcba7258"
     "1h1frnj0x8k7b29ic4jisch0vlpmsmghjw554pz277f2nxaidljj")
    (("NUnitLite" "nunit-lite")    "690603bea98aae69fca9a65130d88591bc6cabee"
     "1f845ysjzs3yd9gcyww66dnkx484z5fknb8l0xz74sjmxk2mngwc")
    ;; ("roslyn-binaries"          "dcb0a0534d5104eaf945d3d1f319dc33044b7bbe"
    ;;  "")
    ("rx"                          "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")
    ;; ("xunit-binaries"           "b8e20d265b368dd6252703d5afd038d0b028e388"
    ;;  "")
    ))

(define-public mono-5.2.0
  (package
    (inherit mono-5.1.0)
    (version "5.2.0.224")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit (string-append "mono-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zsgfqyjkpix05gvgvhqyyqcwcjp5xlvcyv471q32qf307dccbfa"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-5.2.0-external-repo-specs)
                           #$@prepare-mono-source-0))))
    (native-inputs (modify-inputs (package-native-inputs mono-5.1.0)
                     (replace "mono" mono-5.1.0)))))

(define mono-5.4.0-external-repo-specs
  '(("aspnetwebstack"              "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    ("api-doc-tools"               "d03e819838c6241f92f90655cb448cc47c9e8791"
     "1riki79f3ig3cxigviss81dz601hn92a1gifglm0mzjbs76sf3fj"
     #:recursive? #t)
    ("api-snapshot"                "b09033be33ab25113743151c644c831158c54042"
     "0z67iqd1brib6ni36pklrp7rlxyhri5nk3px37fm1aacgrnsk7ck")
    (("reference-assemblies" "binary-reference-assemblies")
     "142cbeb62ffabf1dd9c1414d8dd76f93bcbed0c2"
     "1wkd589hgb16m5zvmp9yb57agyyryaa1jj8vhl4w20i2hp22wad9")
    ("bockbuild"                   "0efdb371e6d79abc54c0e3bb3689fa1646f4394e"
     "10qr1m2wa3zb2i3j16i0cq49higjm451bhlqhqd4rlisqn0w8nrv")
    ("boringssl"                   "3e0770e18835714708860ba9fe1af04a932971ff"
     "139a0gl91a52k2r6na6ialzkqykaj1rk88zjrkaz3sdxx7nmmg6y")
    ("cecil"                       "c0eb983dac62519d3ae93a689312076aacecb723"
     "02i3pwpaf6q00pklfmwxhz0lgp83854dyqnvf4c1ys07cs8y1pdk")
    (("cecil" "cecil-legacy")      "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("corefx"                      "9ad53d674e31327abcc60f35c14387700f50cc68"
     "0ap4g2fj8wsar4xvbc6dkd2l67qalxlcw5laplq3an5nvj2ld65w"
     #:patches ("corefx-mono-5.4.0-patches.patch"))
    ("corert"                      "48dba73801e804e89f00311da99d873f9c550278"
     "1zw47jf4cwqmaixylisxi73xf6cap41bwf9vlmpxanzxaqklzsvk")
    ("ikdasm"                      "1d7d43603791e0236b56d076578657bee44fef6b"
     "1kw8ykkad55qhapg6jbvqim7vainqlpz8469flm083lpz7pks3sg")
    (("ikvm-fork" "ikvm")          "847e05fced5c9a41ff0f24f1f9d40d5a8a5772c1"
     "1fl9bm3lmzf8iqv3x4iqkz9fc54mwdvrxisxg2nvwwcsi4saffpi")
    ("linker"                      "99354bf5c13b8055209cb082cddc50c8047ab088"
     "05zlajnqf83xfvn2whh9nql6j85sq12aw26sqmyqz7zcpml171mj")
    ("Newtonsoft.Json"             "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    (("NuGet.BuildTasks" "nuget-buildtasks")
     "b58ba4282377bcefd48abdc2d62ce6330e079abe"
     "1say03fnqkjsx97zacany3sa5j4mhfk827hkwp23ib02q18f7lvp")
    (("NUnitLite" "nunit-lite")    "690603bea98aae69fca9a65130d88591bc6cabee"
     "1f845ysjzs3yd9gcyww66dnkx484z5fknb8l0xz74sjmxk2mngwc")
    ;; ("roslyn-binaries"          "1904c7d0682a878e2d25b4d49f3475d12fbb9cc6"
    ;;  "")
    ("rx"                          "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")
    ;; ("xunit-binaries"           "d4433b0972f40cb3efaa3fbba52869bde5df8fa8"
    ;;  "")
    ))

(define-public mono-5.4.0
  (package
    (inherit mono-5.2.0)
    (version "5.4.0.212")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://gitlab.winehq.org/mono/mono.git")
                (commit
                 ;; 5.4.0.135 and before have a bug that makes mono not
                 ;; self-hosting (fails to compile self, example error:
                 ;; System.Data.SqlClient/SqlTransaction.cs(39,22): error
                 ;; CS0738: `System.Data.SqlClient.SqlTransaction' does not
                 ;; implement interface member
                 ;; `System.Data.IDbTransaction.Connection.get' and the best
                 ;; implementing candidate
                 ;; `System.Data.SqlClient.SqlTransaction.Connection.get'
                 ;; return type `System.Data.SqlClient.SqlConnection' does not
                 ;; match interface member return type
                 ;; `System.Data.IDbConnection'

                 ;; Note: in above example, SqlConnection implements
                 ;; IDbConnection.  My understanding is that for this to
                 ;; compile properly, we need covariant return types, which is
                 ;; a C# 9.0 feature, but somehow the same code has been
                 ;; compiled just fine by previous versions of mono, and is
                 ;; compiled fine by this version, but not specific 5.4.0.XXX
                 ;; versions.
                 "mono-5.4.0.212")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gx3fxz1wlq5fkj7iphv32vg9m78ia74m9pgn9rab4fyq2k9an2y"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-5.4.0-external-repo-specs)
                           #$@prepare-mono-source-0))
              (patches (search-patches "mono-5.4.0-patches.patch"))))
    (native-inputs (modify-inputs (package-native-inputs mono-5.2.0)
                     (replace "mono" mono-5.2.0)))))

(define mono-pre-5.8.0-external-repo-specs
  '(("api-doc-tools"               "d03e819838c6241f92f90655cb448cc47c9e8791"
     "1riki79f3ig3cxigviss81dz601hn92a1gifglm0mzjbs76sf3fj"
     #:recursive? #t)
    ("api-snapshot"                "e790a9b77031ef1d8ebf093ef88840edea11ed73"
     "1c4np2fqd9mpc1i1x8bsxnypacp58vkvgdwpnmvmlyjdvbj5ax6q")
    ("aspnetwebstack"              "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    (("reference-assemblies" "binary-reference-assemblies")
     "142cbeb62ffabf1dd9c1414d8dd76f93bcbed0c2"
     "1wkd589hgb16m5zvmp9yb57agyyryaa1jj8vhl4w20i2hp22wad9")
    ("bockbuild"                   "b445017309aac741a26d8c51bb0636234084bf23"
     "1jzhvavd1j0n7sy1waczgjv0kmrbr35gkzd76fhlmqvsy0sr9695")
    ("boringssl"                   "3e0770e18835714708860ba9fe1af04a932971ff"
     "139a0gl91a52k2r6na6ialzkqykaj1rk88zjrkaz3sdxx7nmmg6y")
    ("cecil"                       "c76ba7b410447fa37093150cb7bc772cba28a0ae"
     "0ydi7rn8ajqyvnj9agyn74llb4qgd9kgdcg3gajdfyb2klxx6za8")
    (("cecil" "cecil-legacy")      "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("corefx"                      "74ccd8aa00d7d271191ca3b9c4f818268dc36c28"
     "0nm41qdpvj62r8bxnf92m7kimjm1i544ygdqz5a7pgc6zf99as6j"
     #:patches ("corefx-mono-pre-5.8.0-patches.patch"))
    ("corert"                      "48dba73801e804e89f00311da99d873f9c550278"
     "1zw47jf4cwqmaixylisxi73xf6cap41bwf9vlmpxanzxaqklzsvk")
    ("ikdasm"                      "3aef9cdd6013fc0620a1817f0b11d8fb90ed2e0f"
     "078cai33x8c71969iwi7hmbqdfwpicpmam2ag3k2bklpva2vnszv")
    (("ikvm-fork" "ikvm")          "847e05fced5c9a41ff0f24f1f9d40d5a8a5772c1"
     "1fl9bm3lmzf8iqv3x4iqkz9fc54mwdvrxisxg2nvwwcsi4saffpi")
    ("linker"                      "21e445c26c69ac3a2e1441befa02d0bd105ff849"
     "1hx3ik0sg70ysc2y8jdjxm2ljql0069i05i8fp1lakx7s7z7bywc")
    ("Newtonsoft.Json"             "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    (("NuGet.BuildTasks" "nuget-buildtasks")
     "8d307472ea214f2b59636431f771894dbcba7258"
     "1h1frnj0x8k7b29ic4jisch0vlpmsmghjw554pz277f2nxaidljj")
    (("NUnitLite" "nunit-lite")    "690603bea98aae69fca9a65130d88591bc6cabee"
     "1f845ysjzs3yd9gcyww66dnkx484z5fknb8l0xz74sjmxk2mngwc")
    ;; ("roslyn-binaries"          "80b86f340b7f6fb7afe84443214e1cbd7ff70620"
    ;;  "")
    ("rx"                          "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")
    ;; ("xunit-binaries"           "d4433b0972f40cb3efaa3fbba52869bde5df8fa8"
    ;;  "")
    ))

(define-public mono-pre-5.8.0
  (let ((commit "d0f51b4e834042cfa593748ada942033b458cc40")
        (version "5.4.0.201")
        (revision "0"))
    (package
      (inherit mono-5.4.0)
      (version (git-version version revision commit))
      (name "mono")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.winehq.org/mono/mono.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0az5syk1nn9gd5imkbmpb13qm9q6ibr2d2ksdzpwsarkfyp4ic53"))
                (modules '((guix build utils)
                           (ice-9 string-fun)))
                (snippet #~(begin
                             #$(add-external-repos
                                mono-pre-5.8.0-external-repo-specs)
                             #$@prepare-mono-source-0))
                (patches (search-patches "mono-5.4.0-patches.patch"))))
      (native-inputs (modify-inputs (package-native-inputs mono-5.4.0)
                       (replace "mono" mono-5.4.0)))
      (arguments
       (substitute-keyword-arguments (package-arguments mono-5.4.0)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (delete 'patch-sub-autogen.sh-shebang))))))))

(define mono-5.8.0-external-repo-specs
  '(("api-doc-tools"               "d03e819838c6241f92f90655cb448cc47c9e8791"
     "1riki79f3ig3cxigviss81dz601hn92a1gifglm0mzjbs76sf3fj"
     #:recursive? #t)
    ("api-snapshot"                "6668c80a9499218c0b8cc41f48a9e242587df756"
     "0vbwbwa1hr4jlj7283w8bk3v5i8s43h8413r2pkh4hf38b2rks7d")
    ("aspnetwebstack"              "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    (("reference-assemblies" "binary-reference-assemblies")
     "e048fe4a88d237d105ae02fe0363a68296099362"
     "0i87i3x694f4g8s2flflv0ah88blxds7gbiyrwrmscqdjsifhy49")
    ("bockbuild"                   "cb4545409dafe16dfe86c7d8e6548a69c369e2a2"
     "0svdfv61d6ppwd4zgki129r9prf75fnsqihna253zfwfpzpingx7")
    ("boringssl"                   "3e0770e18835714708860ba9fe1af04a932971ff"
     "139a0gl91a52k2r6na6ialzkqykaj1rk88zjrkaz3sdxx7nmmg6y")
    ("cecil"                       "76ffcdabae660e9586273c9b40db180a0dc8d4c8"
     "0f3bsfri28pxmnb0m6074bnmmjgsr7cjixv9fhnp6aimhvy4l5p4")
    (("cecil" "cecil-legacy")      "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("corefx"                      "b965d1f8b5281712c4400ef28ed97670ffd4880d"
     "0r9hr0bs3j3agqi2pq4n1km9jfycaqvxf6756y7r5l3ykqsd6wsr")
    ("corert"                      "48dba73801e804e89f00311da99d873f9c550278"
     "1zw47jf4cwqmaixylisxi73xf6cap41bwf9vlmpxanzxaqklzsvk")
    ("ikdasm"                      "465c0815558fd43c0110f8d00fc186ac0044ac6a"
     "0xir7pcgq04hb7s8g9wsqdrypb6l29raj3iz5rcqzdm0056k75w2")
    (("ikvm-fork" "ikvm")          "847e05fced5c9a41ff0f24f1f9d40d5a8a5772c1"
     "1fl9bm3lmzf8iqv3x4iqkz9fc54mwdvrxisxg2nvwwcsi4saffpi")
    ("linker"                      "c62335c350f3902ff0459112f7efc8b926f4f15d"
     "015191sdw9i7vnhlsycv65pw8nnfpkd65k11jw1y9bikb4x3aj8x")
    ("Newtonsoft.Json"             "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    (("NuGet.BuildTasks" "nuget-buildtasks")
     "b2c30bc81b2a7733a4eeb252a55f6b4d50cfc3a1"
     "01vajrfx6y12f525xdiwfbn9qzmym2s65rbiqpy9d9xw0pnq7gbl")
    (("NUnitLite" "nunit-lite")    "764656cdafdb3acd25df8cb52a4e0ea14760fccd"
     "0pc7lk3p916is8cn4ngaqvjlmlzv3vvjpyksy4pvb3qb5iiaw0vq")
    ;; ("roslyn-binaries"          "e484c75e2edd3c3f1870a2468a71a56220cf1f7f"
    ;;  "")
    ("rx"                          "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")
    ;; ("xunit-binaries"           "d4433b0972f40cb3efaa3fbba52869bde5df8fa8"
    ;;  "")
    ))

(define-public mono-5.8.0
  (package
    (inherit mono-pre-5.8.0)
    (version "5.8.0.129")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit (string-append "mono-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0130vd33yzp4w7570qw9xjq2g7b2xmacjbpkmzrpbhy8as5hy4z6"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-5.8.0-external-repo-specs)
                           #$@prepare-mono-source-0))
              (patches (search-patches "mono-5.8.0-patches.patch"))))
    (native-inputs (modify-inputs (package-native-inputs mono-pre-5.8.0)
                     (replace "mono" mono-pre-5.8.0)))
    (arguments
     (substitute-keyword-arguments (package-arguments mono-pre-5.8.0)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'build-reference-assemblies
              ;; More references need updating this time...
              (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
                (let ((top (getcwd))
                      ;; parallel-build? needs to be false for mono's build
                      ;; phase, but it should work here.
                      (parallel-build? #t))
                  (with-directory-excursion
                      "external/binary-reference-assemblies"
                    (substitute* (find-files "." "^Makefile$")
                      (("CSC_COMMON_ARGS := " all)
                       (string-append all "-delaysign+ "))
                      (("IBM\\.Data\\.DB2_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Mono\\.Data\\.Sqlite_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Data\\.DataSetExtensions_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Data\\.OracleClient_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.IdentityModel_REFS := " all)
                       (string-append all "System.Configuration "))
                      (("System\\.Design_REFS := " all)
                       (string-append all "Accessibility "))
                      (("System\\.Web\\.Extensions\\.Design_REFS := " all)
                       (string-append all "System.Windows.Forms System.Web "))
                      (("System\\.ServiceModel\\.Routing_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Web\\.Abstractions_REFS := " all)
                       (string-append all "System "))
                      (("System\\.Reactive\\.Windows\\.Forms_REFS := " all)
                       (string-append all "System "))
                      (("System\\.Windows\\.Forms\\.DataVisualization_REFS := " all)
                       (string-append all "Accessibility "))
                      (("Facades/System\\.ServiceModel\\.Primitives_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Dynamic\\.Runtime_REFS := " all)
                       (string-append all "System "))
                      (("Facades/System\\.Xml\\.XDocument_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Runtime\\.Serialization.Xml_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Data\\.Common_REFS := " all)
                       (string-append all "System System.Xml ")))
                    (apply invoke "make"
                           `(,@(if parallel-build?
                                   `("-j" ,(number->string
                                            (parallel-job-count)))
                                   '())
                             ,(string-append "CSC=MONO_PATH="
                                             top "/mcs/class/lib/build"
                                             " "
                                             top "/runtime/mono-wrapper"
                                             " "
                                             top "/mcs/class/lib/build/mcs.exe")
                             ,@make-flags))))))))))))

(define mono-pre-5.10.0-external-repo-specs
  '(("api-doc-tools"               "d03e819838c6241f92f90655cb448cc47c9e8791"
     "1riki79f3ig3cxigviss81dz601hn92a1gifglm0mzjbs76sf3fj"
     #:recursive? #t)
    ("api-snapshot"                "627333cae84f02a36ee9ca605c96dac4557d9f35"
     "0p9c6brxiwx38yvaf55jd0l1mxfj3b5ah0xas2hv6frkz80yrqdl")
    ("aspnetwebstack"              "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    (("reference-assemblies" "binary-reference-assemblies")
     "9c5cc7f051a0bba2e41341a5baebfc4d2c2133ef"
     "14bfn1qvni8gyfxjwmvykyjjy3j5ng4fnbljdadi9dm4b9al0wg1")
    ("bockbuild"                   "29022af5d8a94651b2eece93f910559b254ec3f0"
     "0lclc1smmrj6xw32dll073mxw4ddiixv9arv02yw3w5h135ay7w4")
    ("boringssl"                   "3e0770e18835714708860ba9fe1af04a932971ff"
     "139a0gl91a52k2r6na6ialzkqykaj1rk88zjrkaz3sdxx7nmmg6y")
    ("cecil"                       "bc11f472954694ebd92ae4956f110c1036a7c2e0"
     "122nnp5pcnw18pj6amnqkqxlrmapd4vy9xs65hd0bqyqjh56bwnd")
    (("cecil" "cecil-legacy")      "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("corefx"                      "cb1b049c95227465c1791b857cb5ba86385d9f29"
     "1pr0qjlgxf63zs1g80gqd6x3qhlgb0wlcc8zm8z8am5aywrvgb53")
    ("corert"                      "48dba73801e804e89f00311da99d873f9c550278"
     "1zw47jf4cwqmaixylisxi73xf6cap41bwf9vlmpxanzxaqklzsvk")
    ("ikdasm"                      "465c0815558fd43c0110f8d00fc186ac0044ac6a"
     "0xir7pcgq04hb7s8g9wsqdrypb6l29raj3iz5rcqzdm0056k75w2")
    (("ikvm-fork" "ikvm")          "847e05fced5c9a41ff0f24f1f9d40d5a8a5772c1"
     "1fl9bm3lmzf8iqv3x4iqkz9fc54mwdvrxisxg2nvwwcsi4saffpi")
    ("linker"                      "99354bf5c13b8055209cb082cddc50c8047ab088"
     "05zlajnqf83xfvn2whh9nql6j85sq12aw26sqmyqz7zcpml171mj")
    ("Newtonsoft.Json"             "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    (("NuGet.BuildTasks" "nuget-buildtasks")
     "b58ba4282377bcefd48abdc2d62ce6330e079abe"
     "1say03fnqkjsx97zacany3sa5j4mhfk827hkwp23ib02q18f7lvp")
    (("NUnitLite" "nunit-lite")    "764656cdafdb3acd25df8cb52a4e0ea14760fccd"
     "0pc7lk3p916is8cn4ngaqvjlmlzv3vvjpyksy4pvb3qb5iiaw0vq")
    ;; ("roslyn-binaries"          "1904c7d0682a878e2d25b4d49f3475d12fbb9cc6"
    ;;  "")
    ("rx"                          "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")
    ;; ("xunit-binaries"           "d4433b0972f40cb3efaa3fbba52869bde5df8fa8"
    ;;  "")
    ))

(define-public mono-pre-5.10.0
  (let ((commit "3e9d7d6e9cf8dc33eb29c497c350a1cd7df3a057")
        (version "5.8.0.129")
        (revision "0"))
    (package
      (inherit mono-5.8.0)
      (version (git-version version revision commit))
      (name "mono")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.winehq.org/mono/mono.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0m8i0zgzh0fgb3ssy95v9czk1c0rl76q0jj7834s5fjnkdj8l4jb"))
                (modules '((guix build utils)
                           (ice-9 string-fun)))
                (snippet #~(begin
                             #$(add-external-repos
                                mono-pre-5.10.0-external-repo-specs)
                             #$@prepare-mono-source-0))
                (patches (search-patches "mono-mcs-patches-from-5.10.0.patch"))))
      (native-inputs (modify-inputs (package-native-inputs mono-5.8.0)
                       (replace "mono" mono-5.8.0))))))

(define mono-5.10.0-external-repo-specs
  '(("api-doc-tools"               "d03e819838c6241f92f90655cb448cc47c9e8791"
     "1riki79f3ig3cxigviss81dz601hn92a1gifglm0mzjbs76sf3fj"
     #:recursive? #t)
    ("api-snapshot"                "da8bb8c7b970383ce26c9b09ce3689d843a6222e"
     "00kxw09yirdh0bzkvs0v3h6bkdjv9d4g9agn3b8640awvpym3yqw")
    ("aspnetwebstack"              "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    (("reference-assemblies" "binary-reference-assemblies")
     "e048fe4a88d237d105ae02fe0363a68296099362"
     "0i87i3x694f4g8s2flflv0ah88blxds7gbiyrwrmscqdjsifhy49")
    ("bockbuild"                   "1908d43ec630544189bd11630a59ec4ef571db28"
     "1h13lgic2dwnbzc58nqhjhagn0f100nl5mhzryjdmypgrf3cr1b3")
    ("boringssl"                   "3e0770e18835714708860ba9fe1af04a932971ff"
     "139a0gl91a52k2r6na6ialzkqykaj1rk88zjrkaz3sdxx7nmmg6y")
    ("cecil"                       "dfee11e80d59e1a3d6d9c914c3f277c726bace52"
     "1y2f59v988y2llqpqi0zl9ly0lkym8zw0a4vkav7cpp6m5mkq208")
    (("cecil" "cecil-legacy")      "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("corefx"                      "e327d2855ed74dac96f684797e4820345297a690"
     "11pinnn8zwf4hi0gfj98cyqkmh7wrmd5mhcdm84gkl9s2g18iaq0")
    ("corert"                      "aa64b376c1a2238b1a768e158d1b11dac77d722a"
     "1gg4m49s0ry5yx96dwjary7r395ypzzg4ssz1ajld2x5g7ggvwgg")
    ("ikdasm"                      "465c0815558fd43c0110f8d00fc186ac0044ac6a"
     "0xir7pcgq04hb7s8g9wsqdrypb6l29raj3iz5rcqzdm0056k75w2")
    (("ikvm-fork" "ikvm")          "847e05fced5c9a41ff0f24f1f9d40d5a8a5772c1"
     "1fl9bm3lmzf8iqv3x4iqkz9fc54mwdvrxisxg2nvwwcsi4saffpi")
    ("linker"                      "84d37424cde6e66bbf997110a4dbdba7e60038e9"
     "07ffkc9ijzsdvbkrc1fn5sb25sgxyabs54kzyblwkzparwj047qr")
    ("Newtonsoft.Json"             "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    (("NuGet.BuildTasks" "nuget-buildtasks")
     "b2c30bc81b2a7733a4eeb252a55f6b4d50cfc3a1"
     "01vajrfx6y12f525xdiwfbn9qzmym2s65rbiqpy9d9xw0pnq7gbl")
    (("NUnitLite" "nunit-lite")    "70bb70b0ffd0109aadaa6e4ea178972d4fb63ea3"
     "0ln7rn1960cdwmfqcscp2d2ncpwnknhq9rf8v53ay8g2c3g6gh4q")
    ;; ("roslyn-binaries"          "00da53c4746250988a92055ef3ac653ccf84fc40"
    ;;  "")
    ("rx"                          "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")
    ;; ("xunit-binaries"           "c5a907be25c201cda38bec99f6c82548ab3d9b5a"
    ;;  "")
    ))

(define-public mono-5.10.0
  (package
    (inherit mono-pre-5.10.0)
    (version "5.10.0.179")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit (string-append "mono-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zvib164w4mzrsk06ym9my0208ccdanja2fx6x6mlyib358h3626"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-5.10.0-external-repo-specs)
                           #$@prepare-mono-source-0))
              (patches (search-patches "mono-5.10.0-later-mcs-changes.patch"))))
    (native-inputs (modify-inputs (package-native-inputs mono-pre-5.10.0)
                     (replace "mono" mono-pre-5.10.0)
                     (append python-minimal-wrapper)))
    (arguments
     (substitute-keyword-arguments (package-arguments mono-pre-5.10.0)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            ;; Build now relies on these being built before any mcs is built;
            ;; have to use the input mcs.
            (delete 'build-reference-assemblies)
            (add-before 'build 'build-reference-assemblies
              (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
                (let ((top (getcwd))
                      ;; parallel-build? needs to be false for mono's build
                      ;; phase, but it should work here.
                      (parallel-build? #t))
                  (with-directory-excursion
                      "external/binary-reference-assemblies"
                    (substitute* (find-files "." "^Makefile$")
                      (("CSC_COMMON_ARGS := " all)
                       (string-append all "-delaysign+ "))
                      (("IBM\\.Data\\.DB2_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Mono\\.Data\\.Sqlite_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Data\\.DataSetExtensions_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Data\\.OracleClient_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.IdentityModel_REFS := " all)
                       (string-append all "System.Configuration "))
                      (("System\\.Design_REFS := " all)
                       (string-append all "Accessibility "))
                      (("System\\.Web\\.Extensions\\.Design_REFS := " all)
                       (string-append all "System.Windows.Forms System.Web "))
                      (("System\\.ServiceModel\\.Routing_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Web\\.Abstractions_REFS := " all)
                       (string-append all "System "))
                      (("System\\.Reactive\\.Windows\\.Forms_REFS := " all)
                       (string-append all "System "))
                      (("System\\.Windows\\.Forms\\.DataVisualization_REFS := " all)
                       (string-append all "Accessibility "))
                      (("Facades/System\\.ServiceModel\\.Primitives_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Dynamic\\.Runtime_REFS := " all)
                       (string-append all "System "))
                      (("Facades/System\\.Xml\\.XDocument_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Runtime\\.Serialization.Xml_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Data\\.Common_REFS := " all)
                       (string-append all "System System.Xml ")))
                    (apply invoke "make"
                           `(,@(if parallel-build?
                                   `("-j" ,(number->string
                                            (parallel-job-count)))
                                   '())
                             "CSC=mcs"
                             ,@make-flags))))))))))))

(define-public libgdiplus
  (package
    (name "libgdiplus")
    (version "6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mono/libgdiplus.git")
             ;; The releases aren't tagged.
             (commit "94a49875487e296376f209fe64b921c6020f74c0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gwmhrddr8kdlfprjqcd6gqiy8p5v8sl9215dbd949j1l76szl9v"))
       (modules '((guix build utils)))
       (snippet #~(substitute* "./Makefile.am"
                    (("\\./update_submodules\\.sh")
                     ":")))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake
           autoconf
           googletest-1.8
           libtool
           pkg-config
           which))
    (inputs (list cairo
                  freetype
                  fontconfig
                  gettext-minimal
                  giflib
                  glib
                  libexif
                  libjpeg-turbo
                  libpng
                  libtiff
                  libx11))
    (synopsis "Open Source implementation of the GDI+ API")
    (description "Libgdiplus is the Mono library that provides a
GDI+-compatible API on non-Windows operating systems.  It uses Cairo to do
most of the heavy lifting.")
    (home-page "https://github.com/mono/libgdiplus")
    (license license:expat)))

(define mono-6.12.0-external-repo-specs
  '(("api-doc-tools"               "5da8127af9e68c9d58a90aa9de21f57491d81261"
     "0rq8dxmy5av36nd298k60d8s386zhqlw63yn2ywc0416xsflscg4"
     #:recursive? #t)
    ("api-snapshot"                "808e8a9e4be8d38e097d2b0919cac37bc195844a"
     "1i5migdw649bmxqii99q2hd6vka11wlcphfrm98kb7pprz4k401a")
    ("aspnetwebstack"              "e77b12e6cc5ed260a98447f609e887337e44e299"
     "0rks344qr4fmp3fs1264d2qkmm348m8d1kjd7z4l94iiirwn1fq1")
    ;; (("https://github.com/Unity-Technologies/bdwgc" "bdwgc")
    ;;  "a27eddb837d613cb4cf436405c23ce39ed16a86d"
    ;;  "")
    (("reference-assemblies" "binary-reference-assemblies")
     "e68046d5106aa0349c23f95821456955fc15b96b"
     "1mqpz274qdhl84y6x8bazrfmajcf6qagiks2g0gyg4qyqwgrp490")
    ("bockbuild"                   "3bd44f6784b85b1ece8b00b13d12cf416f5a87e7"
     "0z3d8qylfwnlklpcvsmsgy5n248gcff5vmzqjzalfj7d1h7vcjxs")
    ("boringssl"                   "296137cf989688b03ed89f72cd7bfd86d470441e"
     "11ghdayfcvysnh1617bj478hxrg7b43jpk7vgafm6jb7ykpxl8fa")
    ("cecil"                       "8021f3fbe75715a1762e725594d8c00cce3679d8"
     "0j19lwbs30y2xz8myk0fbxs4hbal1p8vqjmnkvn301v0xxacynxm")
    (("cecil" "cecil-legacy")      "33d50b874fd527118bc361d83de3d494e8bb55e1"
     "1p4hl1796ib26ykyf5snl6cj0lx0v7mjh0xqhjw6qdh753nsjyhb")
    ("corefx"                      "c4eeab9fc2faa0195a812e552cd73ee298d39386"
     "03530pf6dddqlihvb83m9z34bark8mzrffnrclq726gndfg4vqs8")
    ("corert"                      "11136ad55767485063226be08cfbd32ed574ca43"
     "1g0q83fff13237nwsfcmk7fmzwx0kv93zsqqybcigwip5x6ani8f")
    ("helix-binaries"              "64b3a67631ac8a08ff82d61087cfbfc664eb4af8"
     "1f6kkpbzj3bx9p1hb36kzjq0ppckk4rpmjnr82hyq7y18fwikfd7")
    ("ikdasm"                      "f0fd66ea063929ef5d51aafdb10832164835bb0f"
     "0313pvmmjh01h9b306jd6cd6fcbnbxaglaj81m0l0acf4yn7zb10")
    (("ikvm-fork" "ikvm")          "08266ac8c0b620cc929ffaeb1f23ac37629ce825"
     "1g0v1v8nvxkwq7w9qyqhf9kgmxq3qm6rsw4al8x0w3dmbgxjhqjv")
    ("illinker-test-assets"       "ec9eb51af2eb07dbe50a2724db826bf3bfb930a6"
     "1b4vq4jbgnl4lzffg02n5w1sppg2k6bfks0150pj403sbnml85gl")
    ("linker"                      "ed4a9413489aa29a70e41f94c3dac5621099f734"
     "16rdpch9anarnhczi441a9zna4rz93jwpb31x0dzrb4j03cxajg2")
    ;; (("https://github.com/dotnet/llvm-project" "llvm-project")
    ;;  "7dfdea1267f0a40955e02567dcbcd1bcb987e825"
    ;;  "")
    ("Newtonsoft.Json"             "471c3e0803a9f40a0acc8aeceb31de6ff93a52c4"
     "0dgngd5hqk6yhlg40kabn6qdnknm32zcx9q6bm2w31csnsk5978s")
    (("NuGet.BuildTasks" "nuget-buildtasks")
     "99558479578b1d6af0f443bb411bc3520fcbae5c"
     "1434m6z9sb7bvki9ba6iinqpmh4a4iyld76jz10qz07sycklflq3")
    (("NUnitLite" "nunit-lite")    "a977ca57572c545e108b56ef32aa3f7ff8287611"
     "02zwdfpw8pazllwbp4hkzqwfql98g4854diykqdb9wa0vrb8w4sj")
    ;; ("roslyn-binaries"             "1c6482470cd219dcc7503259a20f26a1723f20ec"
    ;;  "")
    ("rx"                          "b29a4b0fda609e0af33ff54ed13652b6ccf0e05e"
     "1n1jwhmsbkcv2d806immcpzkb72rz04xy98myw355a8w5ah25yiv")
    ;; ("xunit-binaries"              "8f6e62e1c016dfb15420852e220e07091923734a"
    ;;  "")
    ))

(define-public mono-6.12.0
  (package
    (inherit mono-5.10.0)
    (version "6.12.0.206")
    (name "mono")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.winehq.org/mono/mono.git")
                    (commit (string-append "mono-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cw9v53bgbc6v7xmp5ij76y6inb6sz1g1zx2jk825rxshq96alvk"))
              (modules '((guix build utils)
                         (ice-9 string-fun)))
              (snippet #~(begin
                           #$(add-external-repos
                              mono-6.12.0-external-repo-specs)
                           #$@prepare-mono-source-0))
              (patches (search-patches "mono-6.12.0-fix-ConditionParser.patch"
                                       "mono-6.12.0-add-runpath.patch"
                                       "mono-6.12.0-fix-AssemblyResolver.patch"))))
    (native-inputs (modify-inputs (package-native-inputs mono-5.10.0)
                     (replace "mono" mono-5.10.0)))
    (inputs (modify-inputs (package-inputs mono-5.10.0)
              (append libgdiplus unixodbc)))
    (arguments
     (substitute-keyword-arguments
       (strip-keyword-arguments (list #:parallel-build?)
         (package-arguments mono-5.10.0))
       ((#:modules modules '((guix build gnu-build-system)
                             (guix build utils)))
        `((sxml simple)
          ,@modules))
       ((#:make-flags make-flags #~'())
        #~(append #$make-flags
                  (list
                   (string-append "PLATFORM_DISABLED_TESTS="
                                  ;; segfaults (!), reason unknown
                                  "safehandle.2.exe"
                                  ;; unclear why these fail
                                  "bug-10834.exe"
                                  "bug-60848.exe"
                                  ;; these are tests of microsoft
                                  ;; telemetry.  They fail because
                                  ;; microsoft telemetry is only
                                  ;; enabled on OSX.  No idea why
                                  ;; these tests are run by default.
                                  "merp-crash-test.exe"
                                  "merp-json-valid.exe"))))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (delete 'patch-sgen-linking)
            (delete 'patch-sub-autogen.sh-shebang)
            ;; Our 5.10.0 compiler has been rather souped up.
            (add-after 'unpack 'disable-profile-version-check
              (lambda _
                (substitute* "mcs/build/common/basic-profile-check.cs"
                  (("min_mono_version = .*")
                   "min_mono_version = new Version (0, 0);\n"))))
            (add-after 'unpack 'disable-c#-8.0-tests
              ;; These aren't compilable by mcs
              (lambda _
                (substitute* "mono/mini/Makefile.am.in"
                  (("-langversion:8\\.0")
                   ""))
                (substitute* "mono/tests/Makefile.am"
                  (("	(dim-generic|dim-issue-18917|interface-2|delegate18|generic-unmanaged-constraint|async-generic-enum)\\.cs.*")
                   ""))))
            (add-after 'unpack 'disable-verification-error
              (lambda _
                ;; For some reason verification fails complaining about a bunch
                ;; of missing icalls.
                (substitute* "runtime/Makefile.am"
                  (("	    fi; done; done;")
                   "	    fi; done; done; ok=:;"))))
            ;; This requires binary blobs to be used, it doesn't provide a
            ;; clear way to regenerate them and no corresponding source is
            ;; linked, plus from what little I know of it it sounds like it's
            ;; not something we need at all?
            (add-after 'unpack 'disable-helix-client
              (lambda _
                (substitute* "mcs/tools/Makefile"
                  (("mono-helix-client")
                   ""))))
            (replace 'build-reference-assemblies
              (lambda* (#:key make-flags #:allow-other-keys)
                (let ((top (getcwd)))
                  (with-directory-excursion
                      "external/binary-reference-assemblies"
                    (substitute* (find-files "." "^Makefile$")
                      (("CSC_COMMON_ARGS := " all)
                       (string-append all "-delaysign+ "))
                      (("IBM\\.Data\\.DB2_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Mono\\.Data\\.Sqlite_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Data\\.DataSetExtensions_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Data\\.OracleClient_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.IdentityModel_REFS := " all)
                       (string-append all "System.Configuration "))
                      (("System\\.Design_REFS := " all)
                       (string-append all "Accessibility "))
                      (("System\\.Web\\.Extensions\\.Design_REFS := " all)
                       (string-append all "System.Windows.Forms System.Web "))
                      (("System\\.ServiceModel\\.Routing_REFS := " all)
                       (string-append all "System.Xml "))
                      (("System\\.Web\\.Abstractions_REFS := " all)
                       (string-append all "System "))
                      (("System\\.Reactive\\.Windows\\.Forms_REFS := " all)
                       (string-append all "System "))
                      (("System\\.Windows\\.Forms\\.DataVisualization_REFS := " all)
                       (string-append all "Accessibility "))
                      (("Facades/System\\.ServiceModel\\.Primitives_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Dynamic\\.Runtime_REFS := " all)
                       (string-append all "System "))
                      (("Facades/System\\.Xml\\.XDocument_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Runtime\\.Serialization.Xml_REFS := " all)
                       (string-append all "System.Xml "))
                      (("Facades/System\\.Data\\.Common_REFS := " all)
                       (string-append all "System System.Xml ")))
                    (substitute* "build/monodroid/Makefile"
                      (("ECMA_KEY := \\.\\./\\.\\./\\.\\./\\.\\./\\.\\./mono/")
                       ;; it should only be 4 directories up, and it's in
                       ;; mcs/, not mono/mcs/
                       "ECMA_KEY := ../../../../"))
                    (apply invoke "make" "-j" (number->string
                                               (parallel-job-count))
                           "CSC=mcs" make-flags)))))
            (add-after 'unpack 'enable-resx2sr-installation
              (lambda* (#:key make-flags #:allow-other-keys)
                (substitute* "mcs/tools/resx2sr/Makefile"
                 (("^NO_INSTALL = .*")
                  "NO_INSTALL = \n"))))
            (replace 'check
              (lambda* (#:key tests? (make-flags '()) #:allow-other-keys)
                (when tests?
                  ;; There are more tests than these, but they depend on
                  ;; external/xunit-binaries, so we limit ourselves to the
                  ;; tests that debian runs.
                  (with-directory-excursion "mono/mini"
                    (apply invoke "make" "check" make-flags))
                  (with-directory-excursion "mono/tests"
                    (apply invoke "make" "check" make-flags)))))
            (add-after 'install 'configure-external-libs
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((gac (string-append #$output
                                          "/lib/mono/gac"))
                      (libgdiplus (search-input-file inputs "/lib/libgdiplus.so.0"))
                      (libx11 (search-input-file inputs "/lib/libX11.so.6"))
                      (unixodbc (search-input-file inputs "/lib/libodbc.so.2")))
                  ;;; gamin purposefully not fixed since nowadays mono can
                  ;;; just use inotify--and does.
                  (for-each
                   (lambda (name)
                     (call-with-output-file (string-append name ".config")
                       (lambda (port)
                         (sxml->xml
                          `(configuration
                            (dllmap (@ (dll "libodbc.so.2")
                                       (target ,unixodbc))))
                          port))))
                   (find-files gac "^System[.]Data[.]dll$"))
                  (for-each
                   (lambda (name)
                     (call-with-output-file (string-append name ".config")
                       (lambda (port)
                         (sxml->xml
                          `(configuration (dllmap (@ (dll "gdiplus")
                                                     (target ,libgdiplus))))
                          port))))
                   (find-files gac "^System[.]Drawing[.]dll$"))
                  (for-each
                   (lambda (name)
                     (call-with-output-file (string-append name ".config")
                       (lambda (port)
                         (sxml->xml
                          `(configuration (dllmap (@ (dll "libX11")
                                                     (target ,libx11))))
                          port))))
                   (find-files gac "^System[.]Windows[.]Forms[.]dll$")))))))))))

(define-public mono mono-6.12.0)

(define-public mono-system-collections-immutable-bootstrap
  (hidden-package
   (package
     (name "mono-system-collections-immutable-bootstrap")
     (version
      (package-version mono))
     (source
      (package-source mono))
     (build-system gnu-build-system)
     (native-inputs
      (list mono))
     (arguments
      (list #:tests? #f ; tests would require xunit which is not in the bootstrap path.
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'prepare
                  (lambda _
                    (chdir "external/corefx/src/System.Collections.Immutable/src")
                    (substitute* "../../Common/src/System/SR.cs"
                      ;; I don't want to drag System.Security.AccessControl into the bootstrap path.
                      (("new ResourceManager[(]ResourceType[)]")
                       "new ResourceManager(\"System.Collections.Immutable\", typeof(SR).Assembly)"))))
                (delete 'configure) ; no "configure" script exists
                (replace 'build
                  (lambda* (#:key inputs #:allow-other-keys)
                    (invoke "resx2sr" "-o" "SR.cs" "-n" "System.SR"
                            "--warn-mismatch"
                            "Resources/Strings.resx")
                    (apply invoke "mcs"
                           "-target:library"
                           "-langversion:7.2"
                          ;;; mono can't do it: "-d:FEATURE_ITEMREFAPI"
                           "-out:System.Collections.Immutable.dll"
                           "../../Common/src/System/Runtime/Versioning/NonVersionableAttribute.cs"
                           "../../Common/src/System/SR.cs"
                           (find-files "." "\\.cs$"))))
                (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((lib-dir (string-append #$output "/lib/mono/4.5")))
                      (mkdir-p lib-dir)
                      (install-file "System.Collections.Immutable.dll"
                                    lib-dir)))))))
     (synopsis "System.Collections.Immutable library for bootstrapping")
     (description "This package builds the System.Collections.Immutable library from
the source code included within the Mono source tree.")
     (home-page "https://dot.net/")
     (license license:expat))))

(define-public mono-system-reflection-metadata-bootstrap
  (hidden-package
   (package
     (name "mono-system-reflection-metadata-bootstrap")
     ;; Upstream version 1.4.2; but for bootstrap packages it's more useful to have the mono version here.
     (version
      (package-version mono))
     (source
      (package-source mono))
     (build-system gnu-build-system)
     (inputs
      (list mono-system-collections-immutable-bootstrap)) ; not required: mono-system-buffers-bootstrap
     (native-inputs
      (list mono))
     (arguments
      (list #:tests? #f ; would require xunit which is not in the bootstrap path
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'chdir
                  (lambda _
                    (chdir "external/corefx/src/System.Reflection.Metadata/src")
                    (substitute* "../../Common/src/System/SR.cs"
                      ;; I don't want to drag System.Security.AccessControl into the bootstrap path.
                      (("new ResourceManager[(]ResourceType[)]")
                       "new ResourceManager(\"System.Collections.Immutable\", typeof(SR).Assembly)"))))
                (add-after 'chdir 'prepare
                  (lambda* (#:key inputs #:allow-other-keys)
                    (for-each
                     (lambda (name)
                       (if (file-exists? name)
                           (delete-file name)
                           (format #t "Warning: File ~s doesn't exist~%" name)))
                     ;; We don't need those since they would be for different .NET standards.
                     '("./System/Reflection/Internal/Utilities/CriticalDisposableObject.netstandard1.1.cs"
                       "./System/Reflection/Internal/Utilities/EncodingHelper.netcoreapp.cs"
                       "./System/Reflection/Internal/Utilities/FileStreamReadLightUp.netstandard1.1.cs"
                       "./System/Reflection/Internal/Utilities/MemoryMapLightUp.netstandard1.1.cs"))))
                (delete 'configure) ; no "configure" script exists
                (replace 'build
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (invoke "resx2sr" "-o" "SR.cs" "-n" "System.SR" "--warn-mismatch"
                            "Resources/Strings.resx")
                    (apply invoke "mcs"
                           "-target:library"
                           "-langversion:7.2"
                           "-unsafe"
                           "-out:System.Reflection.Metadata.dll"
                           (string-append "-r:"
                            (search-input-file inputs
                                               "/lib/mono/4.5/System.Collections.Immutable.dll"))
                           "../../Common/src/System/SR.cs"
                           (find-files "." "\\.cs$"))))
                (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((lib-dir (string-append #$output "/lib/mono/4.5")))
                      (install-file "System.Reflection.Metadata.dll" lib-dir)))))))
     (synopsis "System.Reflection.Metadata library for bootstrapping")
     (description "This package builds the System.Reflection.Metadata library from
the source code included within the Mono source tree.")
     (home-page "https://dot.net/")
     (license license:expat))))

;; too new version: 15.9.21.664
;; too old (no support for mono) version: 14.0
(define-public msbuild
  (package
    (name "msbuild")
    (version "15.7.179")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dotnet/msbuild")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fhd4z9575lwgy6l3wisih7g6qd6j3wb99kz246028dzm0rh3cfh"))
       (patches
        (search-patches
         "mono-msbuild-15.7.179-fix-build.patch"
         "mono-msbuild-15.7.179-fix-resources.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal
           mono mono-system-reflection-metadata-bootstrap
           mono-system-collections-immutable-bootstrap))
    (arguments
     (list #:tests? #f ; would require xunit which is not in the bootstrap path
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (define (generate-version-file filename version-str internals-list)
                     (call-with-output-file filename
                       (lambda (port)
                         (format port
                                 "[assembly: System.Reflection.AssemblyVersion(\"~a\")]~%"
                                 version-str)
                         (format port
                                 "[assembly: System.Reflection.AssemblyFileVersion(\"~a\")]~%"
                                 version-str)
                         ;; TODO: and commit id, if any.
                         (format port
                                 "[assembly: System.Reflection.AssemblyInformationalVersion(\"~a\")]~%"
                                 version-str)
                         (for-each
                          (lambda (internal-name)
                            (format port
                                    "[assembly: System.Runtime.CompilerServices.InternalsVisibleTo(\"~a\")]~%"
                                    internal-name))
                          internals-list))))
                   (let ((version #$(package-version this-package)))
                     (substitute* "src/Build/Resources/AssemblyResources.cs"
                       ;; There's a caller that has a caller that checks for null.
                       ;;
                       ;; But if this check is here, the high-level fallback will
                       ;; not work since it only falls back on null, not on
                       ;; exception.
                       ;;
                       ;; So what's this about?!  Remove it.
                       (("ErrorUtilities.VerifyThrow[(]resource != null, .*")
                        "\n"))
                     (substitute* "src/Shared/AssemblyNameExtension.cs"
                       (("\\<ISerializable\\>")
                        "System.Runtime.Serialization.ISerializable")
                       (("\\<StreamingContext\\>")
                        "System.Runtime.Serialization.StreamingContext")
                       (("\\<SerializationInfo\\>")
                        "System.Runtime.Serialization.SerializationInfo"))
                     ;; ThisAssembly would have been generated by MSBuild--which we don't have yet.
                     (substitute* '("src/Shared/CommunicationsUtilities.cs"
                                    "src/Tasks/StronglyTypedResourceBuilder.cs")
                       (("ThisAssembly[.]AssemblyInformationalVersion")
                        (string-append "\"" version "\""))
                       (("ThisAssembly[.]Version")
                        (string-append "\"" version "\"")))
                     (substitute* "src/Shared/FrameworkLocationHelper.cs"
                       ;; That is unused anyway.
                       (("^using Microsoft.Build.Evaluation;")
                        ""))
                     (substitute* '("src/Tasks/AspNetCompiler.cs"
                                    "src/Tasks/AxTlbBaseTask.cs"
                                    "src/Tasks/AxImp.cs"
                                    "src/Tasks/TlbImp.cs"
                                    "src/Tasks/Exec.cs"
                                    "src/Tasks/ResGen.cs"
                                    "src/Tasks/LC.cs"
                                    "src/Tasks/SGen.cs"
                                    "src/Tasks/WinMDExp.cs")
                       (("protected override bool ValidateParameters")
                        "protected internal override bool ValidateParameters")
                       (("override protected bool ValidateParameters") ; SGen.cs
                        "protected internal override bool ValidateParameters"))
                     (substitute* "src/Shared/Modifiers.cs"
                       (("^using Microsoft.Build.Internal;")
                        ""))
                     (substitute* "src/MSBuild/OutOfProcTaskHostNode.cs"
                       (("^using Microsoft.Build.BackEnd;")
                        "using Microsoft.Build.BackEnd;
using Microsoft.Build.BackEnd.Components.Caching;"))
                     (generate-version-file "Version-Framework.cs" version
                                            '("Microsoft.Build.Utilities.Core"
                                              "Microsoft.Build.Tasks.Core"
                                              "Microsoft.Build.Tasks"
                                              "Microsoft.Build"))
                     (generate-version-file "Version-Utilities.cs" version
                                            '("Microsoft.Build"
                                              "Microsoft.Build.Tasks.Core"
                                              "Microsoft.Build.Tasks"
                                              "MSBuild"))
                     (generate-version-file "Version.cs" version
                                            '("MSBuild"))
                     (generate-version-file "Version-exe.cs" version
                                            '()))))
               (replace 'build
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((mcs-flags '("-langversion:7.2" "-unsafe" "-d:NET472" "-d:STRONG_NAME"
                                       "-d:MONO" "-d:STANDALONEBUILD"
                                       ;; Otherwise the build would fail.
                                       "-d:FEATURE_COM_INTEROP"
                                       ;; Otherwise it would try to load shell32.
                                       "-d:FEATURE_SPECIAL_FOLDERS"
                                        ;"-d:FEATURE_BINARY_SERIALIZATION"
                                        ;"-d:FEATURE_ASSEMBLY_LOADFROM"
                                        ;"-d:FEATURE_RESX_RESOURCE_READER"
                                       "-d:FEATURE_RESGENCACHE"
                                       "-d:FEATURE_CODEDOM"
                                        ;"-d:FEATURE_SYSTEM_CONFIGURATION"
                                       "-d:FEATURE_APPDOMAIN"
                                        ;"-d:FEATURE_APM" ; ?
                                       "-d:FEATURE_TYPE_INVOKEMEMBER"
                                       "-d:FEATURE_APPDOMAIN_UNHANDLED_EXCEPTION")))
                     (mkdir "artifacts")

                     ;;; --- 1. Build Microsoft.Build.Framework.dll

                     ;;; Note: No generating SR.cs for now.

                     (invoke "resgen" "src/Shared/Resources/Strings.shared.resx"
                             "artifacts/Microsoft.Build.Framework.Strings.shared.resources")

                     (apply invoke "mcs"
                            (append mcs-flags
                                    '("-target:library" "-out:artifacts/Microsoft.Build.Framework.dll"
                                      "-resource:artifacts/Microsoft.Build.Framework.Strings.shared.resources"
                                      "-r:System.Xaml.dll"
                                      "Version-Framework.cs")
                                    (find-files "src/Framework" "\\.cs$")
                                    (list "src/Shared/Constants.cs"
                                          "src/Shared/BinaryWriterExtensions.cs")))

                     ;;; --- 2. Build Microsoft.Build.Utilities.Core.dll

                     ;; No resx2sr since src/Utilities/AssemblyResources.cs is hand-written.
                     (invoke "resgen" "src/Shared/Resources/Strings.shared.resx"
                             "artifacts/Microsoft.Build.Utilities.Core.Strings.shared.resources")
                     (invoke "resgen" "src/Utilities/Resources/Strings.resx"
                             "artifacts/Microsoft.Build.Utilities.Core.Strings.resources")
                     (apply invoke "mcs"
                            (append mcs-flags
                                    '("-target:library" "-out:artifacts/Microsoft.Build.Utilities.Core.dll"
                                      "-resource:artifacts/Microsoft.Build.Utilities.Core.Strings.shared.resources"
                                      "-resource:artifacts/Microsoft.Build.Utilities.Core.Strings.resources"
                                      "-r:System.Runtime.Serialization.dll"
                                      "-r:artifacts/Microsoft.Build.Framework.dll"
                                      "src/Utilities/AssemblyResources.cs"
                                      "Version-Utilities.cs")
                                    (map (lambda (f) (string-append "src/Utilities/" f))
                                         '("SDKManifest.cs"
                                           "ApiContract.cs"
                                           "SDKType.cs"
                                           "Logger.cs"
                                           "TrackedDependencies/FlatTrackingData.cs"
                                           "TrackedDependencies/CanonicalTrackedOutputFiles.cs"
                                           "TrackedDependencies/CanonicalTrackedInputFiles.cs"
                                           "TrackedDependencies/CanonicalTrackedFilesHelper.cs"
                                           "TrackedDependencies/FileTracker.cs"
                                           "TrackedDependencies/DependencyTableCache.cs"
                                           "AssemblyFolders/AssemblyFoldersExInfo.cs"
                                           "AssemblyFolders/AssemblyFoldersFromConfigInfo.cs"
                                           "AssemblyInfo.cs"
                                           "ProcessorArchitecture.cs"
                                           "FxCopExclusions/Microsoft.Build.Utilities.Suppressions.cs"
                                           "PlatformManifest.cs"
                                           "ExtensionSDK.cs"
                                           "CommandLineBuilder.cs"
                                           "TaskItem.cs"
                                           "ToolTask.cs"
                                           "TargetPlatformSDK.cs"
                                           "AppDomainIsolatedTask.cs"
                                           "Task.cs"
                                           "ProcessExtensions.cs"
                                           "MuxLogger.cs"))
                                    (map (lambda (f) (string-append "src/Shared/" f))
                                         '("FxCopExclusions/Microsoft.Build.Shared.Suppressions.cs"
                                           "EncodingStringWriter.cs"
                                           "EncodingUtilities.cs"
                                           "CopyOnWriteDictionary.cs"
                                           "Tracing.cs"
                                           "TaskLoggingHelper.cs"
                                           "TaskLoggingHelperExtension.cs"
                                           "EventArgsFormatting.cs"
                                           "FileDelegates.cs"  ; req by tasks
                                           "NativeMethodsShared.cs" ; again ???
                                           "MSBuildNameIgnoreCaseComparer.cs"
                                           "BuildEventFileInfo.cs"
                                           "ErrorUtilities.cs"
                                           "EscapingUtilities.cs"
                                           "FileUtilities.cs"
                                           "FileUtilities.GetFolderPath.cs"
                                           "TempFileUtilities.cs"
                                           "Modifiers.cs"
                                           "FileUtilitiesRegex.cs"
                                           "HybridDictionary.cs"
                                           "IConstrainedEqualityComparer.cs"
                                           "ResourceUtilities.cs"
                                           "StringBuilderCache.cs"
                                           "Traits.cs"
                                           "IElementLocation.cs"
                                           "INodePacket.cs"
                                           "INodePacketFactory.cs"
                                           "INodePacketHandler.cs"
                                           "INodePacketTranslatable.cs"
                                           "INodePacketTranslator.cs"
                                           "ExceptionHandling.cs"
                                           "ReadOnlyEmptyCollection.cs"
                                           "OpportunisticIntern.cs"
                                           "AssemblyUtilities.cs"
                                           "ReadOnlyEmptyDictionary.cs"
                                           "CanonicalError.cs"
                                           "VisualStudioLocationHelper.cs"
                                           "AssemblyFolders/Serialization/AssemblyFolderItem.cs"
                                           "AssemblyFolders/Serialization/AssemblyFolderCollection.cs"
                                           "BuildEnvironmentHelper.cs"
                                           "EnvironmentUtilities.cs"
                                           "VersionUtilities.cs"
                                           "InternalErrorException.cs"))))

                     ;;; --- 3. Build Microsoft.Build.Tasks.Core.dll

                     ;; No resx2sr since src/Tasks/AssemblyResources.cs is
                     ;; hand-written.
                     (invoke "resgen" "src/Tasks/Resources/Strings.resx"
                             "artifacts/Microsoft.Build.Tasks.Core.Strings.resources")
                     (apply invoke "mcs"
                            (append mcs-flags
                                    `("-d:MICROSOFT_BUILD_TASKS"
                                      "-target:library"
                                      "-out:artifacts/Microsoft.Build.Tasks.Core.dll"
                                      "-resource:artifacts/Microsoft.Build.Tasks.Core.Strings.resources,Microsoft.Build.Tasks.Core.Strings"
                                      "-r:System.Xml.Linq.dll"
                                      "-r:artifacts/Microsoft.Build.Framework.dll"
                                      ;; This should contain ToolLocationHelper--but it's impossible.
                                      "-r:artifacts/Microsoft.Build.Utilities.Core.dll"
                                      "-r:System.Windows.Forms.dll" ; ResXDataNode
                                      ,(string-append "-r:"
                                                      (search-input-file inputs
                                                                         "/lib/mono/4.5/System.Reflection.Metadata.dll"))
                                      ,(string-append "-r:"
                                                      (search-input-file inputs
                                                                         "lib/mono/4.5/System.Collections.Immutable.dll"))
                                      "Version.cs")
                                    (map (lambda (f) (string-append "src/Tasks/" f))
                                         '(;; Otherwise impossible to use since it requires weird things.
                                           "../Utilities/ToolLocationHelper.cs"
                                           "Delegate.cs"
                                           "StrongNameUtils.cs"
                                           "AssemblyRegistrationCache.cs"
                                           "StateFileBase.cs"
                                           "AppDomainIsolatedTaskExtension.cs"
                                           "SdkToolsPathUtility.cs"
                                           "StronglyTypedResourceBuilder.cs"
                                           "Al.cs" "AppConfig/AppConfig.cs"
                                           "AppConfig/AppConfigException.cs"
                                           "AppConfig/BindingRedirect.cs"
                                           "AppConfig/DependentAssembly.cs"
                                           "AppConfig/RuntimeSection.cs"
                                           "AspNetCompiler.cs"
                                           "AssignCulture.cs"
                                           "AssignLinkMetadata.cs"
                                           "AssignProjectConfiguration.cs"
                                           "AssignTargetPath.cs"
                                           "AssemblyDependency/AssemblyFoldersExResolver.cs"
                                           "AssemblyDependency/AssemblyFoldersFromConfig/AssemblyFoldersFromConfigCache.cs"
                                           "AssemblyDependency/AssemblyFoldersFromConfig/AssemblyFoldersFromConfigResolver.cs"
                                           "AssemblyDependency/AssemblyFoldersResolver.cs"
                                           "AssemblyDependency/AssemblyInformation.cs"
                                           "AssemblyDependency/AssemblyNameReference.cs"
                                           "AssemblyDependency/AssemblyNameReferenceAscendingVersionComparer.cs"
                                           "AssemblyDependency/AssemblyResolution.cs"
                                           "AssemblyDependency/AssemblyResolutionConstants.cs"
                                           "AssemblyDependency/BadImageReferenceException.cs"
                                           "AssemblyDependency/CandidateAssemblyFilesResolver.cs"
                                           "AssemblyDependency/ConflictLossReason.cs"
                                           "AssemblyDependency/CopyLocalState.cs"
                                           "AssemblyDependency/DependencyResolutionException.cs"
                                           "AssemblyDependency/DirectoryResolver.cs"
                                           "AssemblyDependency/DisposableBase.cs"
                                           "AssemblyDependency/FrameworkPathResolver.cs"
                                           "AssemblyDependency/GacResolver.cs"
                                           "AssemblyDependency/GlobalAssemblyCache.cs"
                                           "AssemblyDependency/HintPathResolver.cs"
                                           "AssemblyDependency/InstalledAssemblies.cs"
                                           "AssemblyDependency/InvalidReferenceAssemblyNameException.cs"
                                           "AssemblyDependency/NoMatchReason.cs"
                                           "AssemblyDependency/RawFilenameResolver.cs"
                                           "AssemblyDependency/Reference.cs"
                                           "AssemblyDependency/ReferenceResolutionException.cs"
                                           "AssemblyDependency/ReferenceTable.cs"
                                           "AssemblyDependency/ResolutionSearchLocation.cs"
                                           "AssemblyDependency/Resolver.cs"
                                           "AssemblyDependency/ResolveAssemblyReference.cs"
                                           "AssemblyDependency/TaskItemSpecFilenameComparer.cs"
                                           "AssemblyDependency/UnificationReason.cs"
                                           "AssemblyDependency/UnificationVersion.cs"
                                           "AssemblyDependency/UnifiedAssemblyName.cs"
                                           "AssemblyDependency/WarnOrErrorOnTargetArchitectureMismatchBehavior.cs"
                                           "AssemblyDependency/GenerateBindingRedirects.cs"
                                           "AssemblyFolder.cs" "AssemblyInfo.cs" "AssemblyRemapping.cs"
                                           "AxImp.cs"
                                           "AxTlbBaseTask.cs"
                                           "BuildCacheDisposeWrapper.cs"
                                           "CallTarget.cs"
                                           "CodeTaskFactory.cs"
                                           "CombinePath.cs"
                                           "CommandLineBuilderExtension.cs"
                                           "ComReferenceResolutionException.cs"
                                           "ComReferenceTypes.cs"
                                           "ComReferenceWrapperInfo.cs"
                                           "ConvertToAbsolutePath.cs"
                                           "Copy.cs"
                                           "CreateCSharpManifestResourceName.cs"
                                           "CreateItem.cs"
                                           "CreateManifestResourceName.cs"
                                           "CreateProperty.cs"
                                           "CreateVisualBasicManifestResourceName.cs"
                                           "CSharpParserUtilities.cs"
                                           "Culture.cs"
                                           "CultureInfoCache.cs"
                                           "Delete.cs"
                                           "Dependencies.cs"
                                           "DependencyFile.cs"
                                           "Error.cs"
                                           "ErrorFromResources.cs"
                                           "Exec.cs"
                                           "ExtractedClassName.cs"
                                           "FileIO/ReadLinesFromFile.cs"
                                           "FileIO/WriteLinesToFile.cs"
                                           "FileState.cs"
                                           "FindAppConfigFile.cs"
                                           "FindInList.cs"
                                           "FindInvalidProjectReferences.cs"
                                           "FormatUrl.cs"
                                           "FormatVersion.cs"
                                           "FxCopExclusions/Microsoft.Build.Tasks.Suppressions.cs"
                                           "GenerateResource.cs"
                                           "GetAssemblyIdentity.cs"
                                           "GetFrameworkPath.cs"
                                           "GetFrameworkSDKPath.cs"
                                           "GetInstalledSDKLocations.cs"
                                           "GetReferenceAssemblyPaths.cs"
                                           "GetSDKReferenceFiles.cs"
                                           "Hash.cs"
                                           "IAnalyzerHostObject.cs"
                                           "ICscHostObject.cs"
                                           "ICscHostObject2.cs"
                                           "ICscHostObject3.cs"
                                           "ICscHostObject4.cs"
                                           "IComReferenceResolver.cs"
                                           "IVbcHostObject.cs"
                                           "IVbcHostObject2.cs"
                                           "IVbcHostObject3.cs"
                                           "IVbcHostObject4.cs"
                                           "IVbcHostObject5.cs"
                                           "IVbcHostObjectFreeThreaded.cs"
                                           "InstalledSDKResolver.cs"
                                           "InvalidParameterValueException.cs"
                                           "LC.cs"
                                           "ListOperators/FindUnderPath.cs"
                                           "ListOperators/RemoveDuplicates.cs"
                                           "LockCheck.cs" "MakeDir.cs"
                                           "ManifestUtil/ApplicationIdentity.cs"
                                           "ManifestUtil/AssemblyIdentity.cs"
                                           "ManifestUtil/AssemblyReference.cs"
                                           "ManifestUtil/AssemblyReferenceCollection.cs"
                                           "ManifestUtil/BaseReference.cs"
                                           "ManifestUtil/CngLightup.cs"
                                           "ManifestUtil/ComImporter.cs"
                                           "ManifestUtil/CompatibleFramework.cs"
                                           "ManifestUtil/CompatibleFrameworkCollection.cs"
                                           "ManifestUtil/Constants.cs"
                                           "ManifestUtil/ConvertUtil.cs"
                                           "ManifestUtil/EmbeddedManifestReader.cs"
                                           "ManifestUtil/FileAssociation.cs"
                                           "ManifestUtil/FileAssociationCollection.cs"
                                           "ManifestUtil/FileReference.cs"
                                           "ManifestUtil/FileReferenceCollection.cs"
                                           "ManifestUtil/ManifestFormatter.cs"
                                           "ManifestUtil/MetadataReader.cs"
                                           "ManifestUtil/NativeMethods.cs"
                                           "ManifestUtil/OutputMessage.cs"
                                           "ManifestUtil/PathUtil.cs"
                                           "ManifestUtil/RSAPKCS1SHA256SignatureDescription.cs"
                                           "ManifestUtil/Util.cs"
                                           "ManifestUtil/XmlNamespaces.cs"
                                           "ManifestUtil/XmlUtil.cs"
                                           "ManifestUtil/XPaths.cs"
                                           "Message.cs"
                                           "Move.cs"
                                           "MSBuild.cs"
                                           "NativeMethods.cs"
                                           "ParserState.cs"
                                           "RCWForCurrentContext.cs"
                                           "RedistList.cs"
                                           "RegisterAssembly.cs"
                                           "RemoveDir.cs"
                                           "RequiresFramework35SP1Assembly.cs"
                                           "ResGen.cs"
                                           "ResGenDependencies.cs"
                                           "ResolveCodeAnalysisRuleSet.cs"
                                           "ResolveKeySource.cs"
                                           "ResolveManifestFiles.cs"
                                           "ResolveNonMSBuildProjectOutput.cs"
                                           "ResolveProjectBase.cs"
                                           "ResolveSDKReference.cs"
                                           "SGen.cs"
                                           "StrongNameException.cs"
                                           "System.Design.cs"
                                           "TaskExtension.cs"
                                           "Telemetry.cs"
                                           "TlbImp.cs"
                                           "ToolTaskExtension.cs"
                                           "Touch.cs"
                                           "UnregisterAssembly.cs"
                                           "VisualBasicParserUtilities.cs"
                                           "Warning.cs"
                                           "WinMDExp.cs"
                                           "WriteCodeFragment.cs"
                                           "XmlPeek.cs"
                                           "XmlPoke.cs"
                                           "XslTransformation.cs"
                                           "AssemblyDependency/AssemblyMetadata.cs"))
                                    (append
                                     (map (lambda (f) (string-append "src/Shared/LanguageParser/" f))
                                          '("CSharptokenCharReader.cs"
                                            "CSharptokenizer.cs"
                                            "tokenChar.cs"
                                            "token.cs"
                                            "VisualBasictokenCharReader.cs"
                                            "VisualBasictokenizer.cs"
                                            "CSharptokenEnumerator.cs"
                                            "StreamMappedString.cs"
                                            "tokenCharReader.cs"
                                            "tokenEnumerator.cs"
                                            "VisualBasictokenEnumerator.cs"))
                                     '("src/Shared/AssemblyNameExtension.cs"
                                       "src/Shared/Constants.cs"
                                       "src/Shared/NGen.cs"
                                       "src/Shared/PropertyParser.cs"
                                       "src/Shared/ConversionUtilities.cs"
                                       "src/Shared/MetadataConversionUtilities.cs"
                                       "src/Shared/AssemblyNameComparer.cs"
                                       "src/Shared/AssemblyNameReverseVersionComparer.cs"
                                       "src/Shared/FileMatcher.cs"
                                       "src/Shared/RegistryHelper.cs"
                                       "src/Shared/StrongNameHelpers.cs"
                                       "src/Shared/AssemblyFolders/AssemblyFoldersFromConfig.cs" ; class
                                       ;; Requires Evaluation.
                                       "src/Shared/FrameworkLocationHelper.cs"))))

                     ;;; --- 4. Build Microsoft.Build.dll (The Main Engine)

                     ;; src/Build/Resources/AssemblyResources.cs was
                     ;; hand-written to fall-back to EXE resources,
                     ;; so no resx2sr here.

                     (invoke "resgen" "src/Shared/Resources/Strings.shared.resx"
                             "artifacts/Microsoft.Build.Strings.shared.resources")
                     (invoke "resgen" "src/Build/Resources/Strings.resx"
                             "artifacts/Microsoft.Build.Strings.resources")
                     (invoke "resgen" "src/MSBuild/Resources/Strings.resx"
                             "artifacts/Microsoft.Build.Strings.commandline.resources")

                     (apply invoke "mcs"
                            (append mcs-flags
                                    `("-d:BUILD_ENGINE"
                                      "-d:FEATURE_ASSEMBLY_LOADFROM"
                                      "-d:FEATURE_SYSTEM_CONFIGURATION"
                                      "-target:library"
                                      "-out:artifacts/Microsoft.Build.dll"
                                      "-resource:artifacts/Microsoft.Build.Strings.shared.resources,Microsoft.Build.Strings.shared.resources"
                                      "-resource:artifacts/Microsoft.Build.Strings.resources,Microsoft.Build.Strings.resources"
                                      "-resource:artifacts/Microsoft.Build.Strings.commandline.resources,Microsoft.Build.Strings.commandline.resources"

                                      "-r:System.Configuration.dll"
                                      "-r:System.Threading.Tasks.Dataflow.dll"
                                      "-r:System.IO.Compression.dll"
                                      "-r:artifacts/Microsoft.Build.Framework.dll"
                                      ,(string-append "-r:"
                                        (search-input-file inputs
                                                           "/lib/mono/4.5/System.Collections.Immutable.dll"))
                                      "Version.cs")
                                    (filter (lambda (name)
                                              (not (string-contains name "/Originals/")))
                                            (find-files "src/Build" "\\.cs$"))
                                    (map (lambda (f) (string-append "src/Shared/" f))
                                         '("CollectionHelpers.cs"
                                           "Constants.cs"
                                           "EscapingUtilities.cs"
                                           "FileUtilities.cs"
                                           "FileUtilitiesRegex.cs"
                                           "TempFileUtilities.cs"
                                           "FileUtilities.GetFolderPath.cs"
                                           "InterningBinaryReader.cs"
                                           "MSBuildNameIgnoreCaseComparer.cs"
                                           "NativeMethodsShared.cs"
                                           "ResourceUtilities.cs"
                                           "StringBuilderCache.cs"
                                           "Traits.cs"
                                           "IKeyed.cs"
                                           "Pair.cs"
                                           "EscapingStringExtensions/EscapingStringExtensions.cs"
                                           "NodeShutdown.cs"
                                           "NodeEngineShutdownReason.cs"
                                           "NodePacketFactory.cs"
                                           "INodeEndpoint.cs"
                                           "NodeBuildComplete.cs"
                                           "LogMessagePacketBase.cs"
                                           "NodeEndpointOutOfProcBase.cs"
                                           "ProjectFileErrorUtilities.cs"
                                           "TaskHostConfiguration.cs"
                                           "TaskHostTaskCancelled.cs"
                                           "TaskHostTaskComplete.cs"
                                           "ToolsetElement.cs"
                                           "TaskEngineAssemblyResolver.cs"
                                           "RegisteredTaskObjectCacheBase.cs"
                                           "TypeLoader.cs"
                                           "LoadedType.cs"
                                           "AssemblyLoadInfo.cs"
                                           "ReuseableStringBuilder.cs"
                                           "TaskParameter.cs"
                                           "TaskParameterTypeVerifier.cs"
                                           "OutOfProcTaskHostTaskResult.cs"
                                           "VisualStudioConstants.cs"
                                           "CommunicationsUtilities.cs"
                                           "XMakeAttributes.cs"
                                           "XMakeElements.cs"
                                           ;; Yes, again.  It has a feature flag check.
                                           "TaskLoggingHelper.cs"
                                           "TaskLoggingHelperExtension.cs"
                                           "AssemblyNameComparer.cs"
                                           "EncodingUtilities.cs"
                                           "BuildEventFileInfo.cs"
                                           "CopyOnWriteDictionary.cs"
                                           "FileDelegates.cs"
                                           "HybridDictionary.cs"
                                           "IConstrainedEqualityComparer.cs"
                                           "IElementLocation.cs"
                                           "INodePacket.cs"
                                           "INodePacketFactory.cs"
                                           "INodePacketHandler.cs"
                                           "INodePacketTranslatable.cs"
                                           "INodePacketTranslator.cs"
                                           "NGen.cs"
                                           "OpportunisticIntern.cs"
                                           "ErrorUtilities.cs"
                                           "ExceptionHandling.cs"
                                           "AssemblyUtilities.cs"
                                           "AwaitExtensions.cs"
                                           "BuildEnvironmentHelper.cs"
                                           "ConversionUtilities.cs"
                                           "EnvironmentUtilities.cs"
                                           "EventArgsFormatting.cs"
                                           "FileMatcher.cs"
                                           "FrameworkLocationHelper.cs"
                                           "NodePacketTranslator.cs"
                                           "ProjectErrorUtilities.cs"
                                           "ProjectWriter.cs"
                                           "PropertyParser.cs"
                                           "ReadOnlyEmptyCollection.cs"
                                           "ReadOnlyEmptyDictionary.cs"
                                           "TaskLoader.cs"
                                           "ThreadPoolExtensions.cs"
                                           "Tracing.cs"
                                           "VersionUtilities.cs"
                                           "XmlUtilities.cs"
                                           "VisualStudioLocationHelper.cs"
                                           "Modifiers.cs"
                                           "ReadOnlyCollection.cs"
                                           "AssemblyNameExtension.cs"
                                           "BufferedReadStream.cs"
                                           "CanonicalError.cs"
                                           "EncodingStringWriter.cs"
                                           "InternalErrorException.cs"))))

                     ;;; --- 5. Build MSBuild.exe (the executable)

                     ;; no resx2sr since src/MSBuild/AssemblyResources.cs is hand-written.
                     (invoke "resgen" "src/MSBuild/Resources/Strings.resx"
                             "artifacts/MSBuild.Strings.resources")
                     (invoke "resgen" "src/Shared/Resources/Strings.shared.resx"
                             "artifacts/MSBuild.Strings.shared.resources")
                     (apply invoke "mcs"
                            (append mcs-flags
                                    '("-target:exe"
                                      "-out:artifacts/MSBuild.exe"
                                      ;; Add the correct logical names (RHS) for BOTH resource files.
                                      "-resource:artifacts/MSBuild.Strings.resources,MSBuild.Strings.resources"
                                      "-resource:artifacts/MSBuild.Strings.shared.resources,MSBuild.Strings.shared.resources"
                                      "-r:artifacts/Microsoft.Build.dll"
                                      "-r:artifacts/Microsoft.Build.Framework.dll"
                                      "-r:artifacts/Microsoft.Build.Tasks.Core.dll"
                                      "Version-exe.cs")
                                    (find-files "src/MSBuild" "\\.cs$")
                                    '("src/Shared/QuotingUtilities.cs"
                                      "src/Shared/ExceptionHandling.cs"))))))
               (replace 'install
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((lib-dir (string-append #$output "/lib/mono/msbuild"))
                          (bin-dir (string-append #$output "/bin")))
                     (mkdir-p lib-dir)
                     (mkdir-p bin-dir)
                     (for-each (lambda (file)
                                 (install-file file lib-dir))
                               (find-files "artifacts" "(\\.dll|\\.exe)$"))
                     (for-each (lambda (file)
                                 (install-file file lib-dir))
                               (find-files "src/Tasks" "\\.(targets|props|tasks)$"))
                     (substitute* '("src/MSBuild/app.amd64.config"
                                    "src/MSBuild/app.config")
                       (("</configuration>")
                        (string-append "<runpath path=\""
                                       (dirname
                                        (search-input-file inputs
                                                           "/lib/mono/4.5/System.Reflection.Metadata.dll"))
                                       ":"
                                       (dirname
                                        (search-input-file inputs
                                                           "/lib/mono/4.5/System.Collections.Immutable.dll"))
                                       "\"/></configuration>")))
                     (copy-file #$(if (target-x86-64? (or (%current-target-system)
                                                          (%current-system)))
                                      "src/MSBuild/app.amd64.config"
                                      "src/MSBuild/app.config")
                                (string-append lib-dir "/MSBuild.exe.config"))
                     (let* ((msbuild-exe (string-append lib-dir "/MSBuild.exe"))
                            (wrapper (string-append bin-dir "/msbuild")))
                       (call-with-output-file wrapper
                         (lambda (port)
                           (format port "#!~a
exec ~s ~s \"$@\"~%"
                                   (search-input-file inputs "/bin/bash")
                                   (search-input-file inputs "/bin/mono")
                                   msbuild-exe)))
                       (chmod wrapper #o755))))))))
    (synopsis "Microsoft Build Engine (MSBuild) for mono")
    (description "This package provides MSBuild, the build tool for .NET.")
    (home-page "https://github.com/dotnet/msbuild")
    (license license:expat)))
