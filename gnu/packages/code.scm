;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2018, 2020, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2019-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2019 Hartmut Goebel <h.goebel@goebel-consult.de>
;;; Copyright © 2020, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020, 2021, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 lu hui <luhuins@163.com>
;;; Copyright © 2021, 2022 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2023 Fries <fries1234@protonmail.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Jordan Moore <lockbox@struct.foo>
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

(define-module (gnu packages code)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

;;; Tools to deal with source code: metrics, cross-references, etc.

(define-public automatic-component-toolkit
  (package
    (name "automatic-component-toolkit")
    (version "1.6.0")
    (home-page "https://github.com/Autodesk/AutomaticComponentToolkit")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r0sbw82cf9dbcj3vgnbd4sc1lklzvijic2z5wgkvs21azcm0yzh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f              ;no tests
      #:install-source? #f
      #:import-path "github.com/Autodesk/AutomaticComponentToolkit/cmd/act"
      #:unpack-path "github.com/Autodesk/AutomaticComponentToolkit/"
      #:phases
      #~(modify-phases %standard-phases
          ;; Golang produces the final binary based on the current directory
          ;; name if -o options is not provided, utilize this assumption to
          ;; completely relay on go-build-system.
          (add-before 'build 'pretend-cmd-act
            (lambda* (#:key unpack-path #:allow-other-keys)
              (let ((act (string-append "src/" unpack-path "/cmd/act"))
                    (source (string-append "src/" unpack-path "/Source")))
                (mkdir-p act)
                (copy-recursively source act)))))))
    (synopsis "Automatically generate software components")
    (description
     "The Automatic Component Toolkit (@dfn{ACT}) is a code generator that
takes an instance of an Interface Description Language (@dfn{IDL}) file and
generates a thin C89-API, implementation stubs, and language bindings of your
desired software component.")
    (license license:bsd-2)))

(define-public cflow
  (package
    (name "cflow")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/cflow/cflow-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "11khr78090jjyqa2l26bdz0myjx6b212lz216dhjc7h0z754c4fh"))))
    (build-system gnu-build-system)

    ;; Needed to have cflow-mode.el installed.
    (native-inputs
     (list emacs-minimal))
    (arguments
     '(#:configure-flags (list (string-append "CPPFLAGS="
                                              "-D" "CFLOW_PREPROC=\\\""
                                              (assoc-ref %build-inputs "gcc")
                                              "/bin/cpp\\\""))))
    (home-page "https://www.gnu.org/software/cflow/")
    (synopsis "Create a graph of control flow within a program")
    (description
     "GNU cflow analyzes C source files and produces a graph charting the
control flow of the program.  It can output the graph in several styles and
in either the POSIX format or in an extended GNU format.  cflow also includes
a major mode for Emacs for examining the flowcharts that it produces.")
    (license license:gpl3+)))

(define-public complexity
  (package
    (name "complexity")
    (version "1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/complexity/complexity-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0lr0l9kj2w3jilz9h9y4np9pf9i9ccpy6331lanki2fnz4z8ldvd"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'set-man-page-date
                 ;; Avoid embedding the current date for reproducible builds
                 (lambda _
                   (setenv "MAN_PAGE_DATE" "2012-04-18"))))))
    (native-inputs
     (list texinfo autogen))
    (home-page "https://www.gnu.org/software/complexity/")
    (synopsis "Analyze complexity of C functions")
    (description
     "GNU complexity provides tools for finding procedures that are
convoluted, overly long or otherwise difficult to understand.  This
may help in learning or reviewing unfamiliar code or perhaps
highlighting your own code that seemed comprehensible when you wrote it.")
    (license license:gpl3+)))

(define-public global                             ; a global variable
  (package
    (name "global")
    (version "6.6.14")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/global/global-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "085kihqvl26q275dvp2a8b4xalb5mxmn31mnbgl95lmfd05zvrzn"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-ncurses="
                                  #$(this-package-input "ncurses"))
                   (string-append "--with-sqlite3="
                                  #$(this-package-input "sqlite"))
                   (string-append "--with-universal-ctags="
                                  #$(this-package-input "universal-ctags")
                                  "/bin/ctags")
                   ;; Otherwise this gets overridden in the 'configure phase.
                   (string-append "--with-python-interpreter="
                                  #$(this-package-input "python-wrapper")
                                  "/bin/python")
                   (string-append "--sysconfdir="
                                  #$output "/share/gtags")
                   "--localstatedir=/var" ; This needs to be a writable location.
                   "--disable-static")

           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-globash
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "globash/globash.in"
                     (("/bin/echo")
                      (search-input-file inputs "bin/echo")))))
               (add-after 'install 'post-install
                 (lambda _
                   ;; Install the plugin files in the right place.
                   (let* ((data (string-append #$output "/share/gtags"))
                          (vim  (string-append
                                  #$output
                                  "/share/vim/vimfiles/pack/guix/start/global/plugin"))
                          (lisp (string-append #$output "/share/emacs/site-lisp/"
                                               #$(package-name this-package) "-"
                                               #$(package-version this-package))))
                     (mkdir-p lisp)
                     (mkdir-p vim)
                     (rename-file (string-append data "/gtags.el")
                                  (string-append lisp "/gtags.el"))
                     (rename-file (string-append data "/gtags.vim")
                                  (string-append vim  "/gtags.vim"))
                     (rename-file (string-append data "/gtags-cscope.vim")
                                  (string-append vim  "/gtags-cscope.vim")))))
               (add-after 'post-install 'install-plugins
                 (lambda _
                   (with-directory-excursion "plugin-factory"
                     (invoke "make" "install"))))
               (add-before 'install 'dont-install-to-/var
                 (lambda _
                   (substitute* "gozilla/Makefile"
                     (("DESTDIR\\)\\$\\{localstatedir\\}")
                      "TMPDIR)"))))
               (add-after 'install-plugins 'wrap-program
                 (lambda _
                   (wrap-program
                       (string-append #$output
                                      "/share/gtags/script/pygments_parser.py")
                     `("GUIX_PYTHONPATH" ":" prefix
                       (,(getenv "GUIX_PYTHONPATH")))))))))
    (inputs
      (list bash-minimal                ; for wrap-program
            coreutils
            universal-ctags
            libltdl
            ncurses
            python-pygments
            python-wrapper
            sqlite))
    (home-page "https://www.gnu.org/software/global/")
    (synopsis "Cross-environment source code tag system")
    (description
     "GNU GLOBAL is a source code tagging system that functions in the same
way across a wide array of environments, such as different text editors,
shells and web browsers.  The resulting tags are useful for quickly moving
around in a large, deeply nested project.")
    (license license:gpl3+)))

(define-public sloccount
  (package
    (name "sloccount")
    (version "2.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dwheeler.com/sloccount/sloccount-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ayiwfjdh1946asah861ah9269s5xkc8p5fv1wnxs9znyaxs4zzs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'make-dotl-files-older
                              (lambda _
                                ;; Make the '.l' files as old as the '.c'
                                ;; files to avoid triggering the rule that
                                ;; requires Flex.
                                (define ref
                                  (stat "README"))

                                (for-each (lambda (file)
                                            (set-file-time file ref))
                                          (find-files "." "\\.[chl]$"))
                                #t))
                  (add-before 'install 'make-target-directories
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let ((out (assoc-ref outputs "out")))
                                  (mkdir-p (string-append out "/bin"))
                                  (mkdir-p (string-append out
                                                          "/share/man/man1"))
                                  (mkdir-p (string-append out
                                                          "/share/doc"))
                                  #t)))
                  (replace 'check
                           (lambda _
                             (setenv "HOME" (getcwd))
                             (setenv "PATH"
                                     (string-append (getcwd) ":"
                                                    (getenv "PATH")))
                             (invoke "make" "test"))))

        #:make-flags (list (string-append "PREFIX="
                                          (assoc-ref %outputs "out")))))
    (inputs (list perl))
    (home-page "https://dwheeler.com/sloccount/")
    (synopsis "Count physical source lines of code (SLOC)")
    (description
     "SLOCCount is a set of the programs for counting source lines of
code (SLOC) in large software systems.  It can automatically identify and
measure a wide range of programming languages.  It automatically estimates the
effort, time, and money it would take to develop the software, using the
COCOMO model or user-provided parameters.")
    (license license:gpl2+)))

(define-public cloc
  (package
    (name "cloc")
    (version "2.02")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AlDanial/cloc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1syh1gppjr3p2m9mq0zm3n47bhaksxcn5bx1nwqc29xn404c2fm9"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal
           coreutils
           perl
           perl-algorithm-diff
           perl-digest-md5
           perl-parallel-forkmanager
           perl-regexp-common))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)   ; nothing to configure
                  (delete 'build)       ; nothing to build
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (invoke "make" "-C" "Unix"
                                (string-append "prefix=" out)
                                (string-append "INSTALL="
                                               (assoc-ref inputs "coreutils")
                                               "/bin/install")
                                "install"))))
                  (add-after 'install 'wrap-program
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/bin/cloc")
                          `("PERL5LIB" ":" =
                            ,(string-split (getenv "PERL5LIB") #\:)))))))
       #:out-of-source? #t
       ;; Tests require some other packages.
       #:tests? #f))
    (home-page "https://github.com/AlDanial/cloc")
    (synopsis "Count source lines of code (SLOC) and other source code metrics")
    (description "cloc counts blank lines, comment lines, and physical lines
of source code in many programming languages.  Given two versions of a code
base, cloc can compute differences in blank, comment, and source lines.

cloc contains code from David Wheeler's SLOCCount.  Compared to SLOCCount,
cloc can handle a greater variety of programming languages.")
    (license license:gpl2+)))

(define-public scc
  (package
    (name "scc")
    (version "3.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/boyter/scc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fvp9ab65vwn3dn0wm8l63rrp5fz1gpymcxs8yr0h4s5zmrsg9zf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/boyter/scc"))
    (native-inputs
     (list go-github-com-dbaggerman-cuba
           go-github-com-json-iterator-go
           go-github-com-mattn-go-runewidth
           go-github-com-minio-blake2b-simd
           go-github-com-spf13-cobra
           go-golang-org-x-text
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/boyter/scc")
    (synopsis "Fast code counter written in Go")
    (description
     "@command{scc} provides a lines-of-code counter similar to tools like
@command{cloc} and @command{sloccount}.  It aims to be fast as possible while
supporting @acronym{COCOMO,Constructive Cost Model} calculation and code
complexity estimation.")
    (license license:expat)))

(define-public the-silver-searcher
  (package
    (name "the-silver-searcher")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://geoff.greer.fm/ag/releases/the_silver_searcher-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0w1icjqd8hd45rn1y6nbfznk1a6ip54whwbfbhxp7ws2hn3ilqnr"))))
    (build-system gnu-build-system)
    (arguments
     ;; Required since GCC 10, see:
     ;; https://gcc.gnu.org/gcc-10/porting_to.html.
     `(#:configure-flags (list "CFLAGS=-fcommon")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list pcre xz zlib))
    (home-page "https://geoff.greer.fm/ag/")
    (synopsis "Fast code searching tool")
    (description
     "The Silver Searcher (@command{ag}) is a tool for quickly searching large
numbers of files.  It's intended primarily for source code repositories, and
respects files like @file{.gitignore} and @file{.hgignore}.  It's also an order
of magnitude faster than its inspiration, @command{ack}, and less specialised
tools such as @command{grep}.")
    (license license:asl2.0)))

(define-public trio
  (package
    (name "trio")
    (version "1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ctrio/trio/trio-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "02pwd5m5vq7hbrffgm2na1dfc249z50yyr5jv73vdw15bd7ygl44"))))
    (build-system gnu-build-system)
    (home-page "https://daniel.haxx.se/projects/trio/")
    (synopsis "Portable and extendable printf and string functions")
    (description
     "Trio is a set of @code{printf} and string functions designed be used by
applications with a focus on portability or with the need for additional
features that are not supported by the standard @code{stdio} implementation.")
    ;; This license is very similar to the ISC license, but the wording is
    ;; slightly different.
    (license (license:non-copyleft
              "http://sourceforge.net/p/ctrio/git/ci/master/tree/README"))))

(define-public universal-ctags
  (package
    (name "universal-ctags")
    (version "6.1.20250525.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/universal-ctags/ctags")
             (commit (string-append "p" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nxhmpzkxixb303bsihd5j7n0d29ak2lgnqff920q3dm33y965sy"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove the bundled PackCC and associated build rules.
           (substitute* "Makefile.am"
             (("^PACKCC = .*")
              "PACKCC = packcc\n")
             (("\\$\\(PACKCC_FILES\\)")
              "")
             (("\\$\\(PEG_SRCS\\) \\$\\(PEG_HEADS\\): \\$\\(PACKCC\\)")
              "$(PEG_SRCS) $(PEG_HEADS):"))
           (delete-file-recursively "misc/packcc")))))
    (build-system gnu-build-system)
    (arguments
     '(;; Don't use the build-time TMPDIR (/tmp/guix-build-...) at runtime.
       #:configure-flags '("--enable-tmpdir=/tmp")
       #:test-target "units"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'make-files-writable
                    (lambda _
                      (for-each make-file-writable (find-files "."))))
                  (add-before 'bootstrap 'patch-misc
                    (lambda _
                      ;; The autogen.sh script calls out to these scripts, so
                      ;; we cannot wait for the patch-source-shebangs phase.
                      (for-each patch-shebang (find-files "misc"))))
                  (add-before 'check 'patch-tests
                    (lambda _
                      (substitute* "misc/units"
                        (("SHELL=/bin/sh")
                         (string-append "SHELL=" (which "sh"))))
                      (substitute* "Tmain/utils.sh"
                        (("/bin/echo") (which "echo"))))))))
    (native-inputs
     (list autoconf automake packcc perl pkg-config python-docutils))
    (inputs
     (list jansson libseccomp libxml2 libyaml pcre2))
    (home-page "https://ctags.io/")
    (synopsis "Generate tag files for source code")
    (description
     "Universal Ctags generates an index (or tag) file of language objects
found in source files for many popular programming languages.  This index
makes it easy for text editors and other tools to locate the indexed items.
Universal Ctags improves on traditional ctags because of its multilanguage
support, its ability for the user to define new languages searched by regular
expressions, and its ability to generate emacs-style TAGS files.")
    (license license:gpl2+)))

(define-public lcov
  (package
    (name "lcov")
    (version "1.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/linux-test-project/lcov"
                           "/releases/download/v" version
                           "/lcov-" version ".tar.gz"))
       (sha256
        (base32 "02r4wdl5d694aaxc5fgjw4nl3zmzq45khaxmsi3agj18annk2w4q"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       ;; This test suite has race conditions in its scripts which make it
       ;; unreliable when run in parallel.
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-references-to-commands
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Lift the requirement of having a shell and 'find' in PATH.
             (substitute* "bin/geninfo"
               (("qw/abs_path/")
                "qw/abs_path getcwd/"))
             (substitute* '("bin/lcov" "bin/geninfo")
               (("`pwd`")
                "getcwd()")
               (("`find ")
                (string-append "`"
                               (search-input-file inputs "/bin/find")
                               " ")))))
         (delete 'configure)            ;no configure script
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/geninfo")
                 `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB"))))))))))
    (inputs (list bash-minimal perl perl-io-compress perl-json))
    (home-page "https://ltp.sourceforge.net/coverage/lcov.php")
    (synopsis "Code coverage tool that enhances GNU gcov")
    (description "LCOV is an extension of @command{gcov}, a tool part of the
GNU@tie{}Binutils, which provides information about what parts of a program
are actually executed (i.e., \"covered\") while running a particular test
case.  The extension consists of a set of Perl scripts which build on the
textual @command{gcov} output to implement the following enhanced
functionality such as HTML output.")
    (license license:gpl2+)))

(define-public lcov-cobertura
  (package
    (name "python-lcov-cobertura")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lcov_cobertura" version))
       (sha256
        (base32
         "02ar6yjazlxq4p64cz9gag08bvakmzjrp147jara9wlnlbc96j8g"))))
    (build-system python-build-system)
    (home-page "https://eriwen.github.io/lcov-to-cobertura-xml/")
    (synopsis "LCOV to Cobertura XML converter")
    (description
     "The lcov-to-cobertura Python module converts code coverage report files
in the lcov format to the XML format of
@uref{http://cobertura.github.io/cobertura/, Cobertura}, a Java code coverage
tool.  It allows continuous integration servers like Jenkins to aggregate
results and determine build stability.")
    (license license:asl2.0)))

(define-public kcov
  (package
    (name "kcov")
    (version "42")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SimonKagstrom/kcov")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14pyahpgadh845q2p1gjw2yrlqcan4n870icrn2yqdpf33cprzgk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-/bin/bash-references
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "src" ".*\\.cc?$")
               (("/bin/(bash|sh)" shell)
                (search-input-file inputs shell))))))))
    (inputs
     (list curl elfutils libelf openssl zlib))
    (native-inputs
     (list python))
    (home-page "https://github.com/SimonKagstrom/kcov")
    (synopsis "Code coverage tester for compiled languages, Python and Bash")
    (description "Kcov is a code coverage tester for compiled languages,
Python and Bash.  It was originally a fork of Bcov, but has since evolved to
support a large feature set in addition to that of Bcov.

Kcov uses DWARF debugging information for compiled programs to make it
possible to collect coverage information without special compiler switches.")
    (license license:gpl2+)))

(define-public rtags
  (package
    (name "rtags")
    (version "2.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Andersbakken/rtags")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "rtags-separate-rct.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Part of spliting rct with rtags.
        ;; Substitute #include "rct/header.h" with #include <rct/header.h>.
        '(with-directory-excursion "src"
           (delete-file-recursively "rct")        ;remove bundled copy
           (let ((files (find-files "." ".*\\.cpp|.*\\.h")))
             (substitute* files
               (("#include ?\"rct/(.*.h)\"" all header)
                (string-append "#include <rct/" header ">")))
             #t)))
       (sha256
        (base32
         "0raqjbkl1ykga4ahgl9xw49cgh3cyqcf42z36z7d6fz1fw192kg0"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "RelWithDebInfo"
       #:configure-flags
       '("-DRTAGS_NO_ELISP_FILES=1"
         "-DBUILD_TESTING=FALSE")
       #:tests? #f))
    (native-inputs
     (list pkg-config))
    (inputs
     (list bash-completion
           clang
           llvm
           lua
           rct
           selene))
    (home-page "https://github.com/Andersbakken/rtags")
    (synopsis "Indexer for the C language family with Emacs integration")
    (description
     "RTags is a client/server application that indexes C/C++ code and keeps a
persistent file-based database of references, declarations, definitions,
symbolnames etc.  There’s also limited support for ObjC/ObjC++.  It allows you
to find symbols by name (including nested class and namespace scope).  Most
importantly we give you proper follow-symbol and find-references support.")
    (license license:gpl3+)))

(define-public colormake
  (package
    (name "colormake")
    (version "0.9.20140503")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pagekite/Colormake")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f9v5s0viq4yc9iv6701h3pv7j21zz1ckl37lpp9hsnliiizv03p"))))
    (build-system trivial-build-system)
    (native-inputs
     (list bash perl))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source") "source")
         (chdir "source")
         (patch-shebang  "colormake.pl"
                         (list (string-append (assoc-ref %build-inputs "perl")
                                              "/bin")))
         (let* ((out (assoc-ref %outputs "out"))
                (bin (string-append out "/bin"))
                (doc (string-append out "/share/doc"))
                (install-files (lambda (files directory)
                                 (for-each (lambda (file)
                                             (install-file file directory))
                                           files))))
           (substitute* "colormake"
             (("colormake\\.pl") (string-append bin "/colormake.pl"))
             (("/bin/bash")
              (search-input-file %build-inputs "/bin/sh")))
           (install-file "colormake.1" (string-append doc "/man/man1"))
           (install-files '("AUTHORS" "BUGS" "ChangeLog" "README") doc)
           (install-files '("colormake" "colormake-short" "clmake"
                            "clmake-short" "colormake.pl")
                          bin)
           #t))))
    (home-page "https://bre.klaki.net/programs/colormake/")
    (synopsis "Wrapper around @command{make} to produce colored output")
    (description "This package provides a wrapper around @command{make} to
produce colored output.")
    (license license:gpl2+)))

(define-public makefile2graph
  (package
    (name "makefile2graph")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lindenb/makefile2graph")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gjfk3d8qg3cla7qd2y7r9s03whlfwy83q8k76xfcnqrjjfavdgk"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (native-inputs
     (list graphviz))
    (home-page "https://github.com/lindenb/makefile2graph")
    (synopsis "Creates a graph of dependencies from GNU Make")
    (description
     "@code{make2graph} creates a graph of dependencies from GNU Make.  The
output is a graphviz-dot file, a Gexf-XML file or a list of the deepest
independent targets.")
    (license license:expat)))

(define-public uncrustify
  (package
    (name "uncrustify")
    (version "0.80.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/uncrustify/uncrustify/")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1n7ifr4i35ii5zaia61jdd61virbygc00anhc9iq3p5gzjlfxhsa"))))
    (build-system cmake-build-system)
    (native-inputs (list python-wrapper))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unpack-etc
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; Configuration samples are not installed by default.
                   (let* ((output (assoc-ref outputs "out"))
                          (etcdir (string-append output "/etc")))
                     (for-each (lambda (l)
                                 (install-file l etcdir))
                               (find-files "etc" "\\.cfg$"))))))))
    (home-page "https://uncrustify.sourceforge.net/")
    (synopsis "Code formatter for C and other related languages")
    (description
     "Beautify source code in many languages of the C family (C, C++, C#,
Objective@tie{}C, D, Java, Pawn, and Vala).  Features:
@itemize
@item Indent and align code.
@item Reformat comments (a little bit).
@item Fix inter-character spacing.
@item Add or remove parens / braces.
@item Supports embedded SQL @code{EXEC SQL} stuff.
@item Highly configurable - More than 600 configurable options.
@end itemize\n")
    (license license:gpl2+)))

(define-public astyle
  (package
    (name "astyle")
    (version "3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/astyle/astyle/astyle%20"
                           version "/astyle-"  version ".tar.bz2"))
       (sha256
        (base32 "0g4jyp47iz97ld9ac4wb5k59j4cs8dbw4dp8f32bwqx8pyvirz6y"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:make-flags
      #~(list (string-append "prefix=" #$output)
              "INSTALL=install"
              "release" "shared")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-makefile
            (lambda _
              (substitute* "build/gcc/Makefile"
                (("CBASEFLAGS = -Wall -Wextra -fno-rtti -fno-exceptions -std=c\\+\\+11")
                 "CBASEFLAGS = -Wall -Wextra -fno-rtti -fno-exceptions -std=c++17"))))
          (replace 'configure
            (lambda _
              (chdir "build/gcc")))
          (add-after 'install 'install-more
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Libraries and headers aren't installed by default.
              (let ((include (string-append #$output "/include"))
                    (lib     (string-append #$output "/lib")))
                (mkdir-p include)
                (copy-file "../../src/astyle.h"
                           (string-append include "/astyle.h"))
                (mkdir-p lib)
                (for-each (lambda (l)
                            (copy-file
                             l (string-append lib "/" (basename l))))
                          (find-files "bin" "^lib.*\\.so"))))))))
    (home-page "https://astyle.sourceforge.net/")
    (synopsis "Source code indenter, formatter, and beautifier")
    (description
     "Artistic Style is a source code indenter, formatter, and beautifier for
the C, C++, C++/CLI, Objective‑C, C#, and Java programming languages.")
    (license license:lgpl3+)))

(define-public indent
  (package
   (name "indent")
   (version "2.2.13")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/indent/indent-" version
                                ".tar.gz"))
            (sha256
             (base32 "15c0ayp9rib7hzvrcxm5ijs0mpagw5y8kf5w0jr9fryfqi7n6r4y"))
            ;; Remove patch when updating.
            (patches (search-patches "indent-CVE-2024-0911.patch"))))
   (build-system gnu-build-system)
   (native-inputs
    (list texinfo))
   (synopsis "Code reformatter")
   (description
    "Indent is a program that makes source code easier to read by
reformatting it in a consistent style.  It can change the style to one of
several different styles such as GNU, BSD or K&R.  It has some flexibility to
deal with incomplete or malformed syntax.  GNU indent offers several
extensions over the standard utility.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/indent/")))

(define-public amalgamate
  (let* ((commit "c91f07eea1133aa184f652b8f1398eaf03586208")
         (revision "0")
         (version (git-version "1.1.1" revision commit)))
    (package
      (name "amalgamate")
      (version version)
      (home-page "https://github.com/edlund/amalgamate")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (sha256
          (base32
           "0cllaraw8mxs8q2nr28nhgzkb417gj2wcklqg59w84f4lc78k3yb"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(substitute* "test.sh"
             (("test_command \"cc -Wall -Wextra -o source.out source.c\"" all)
              "test_command \"gcc -Wall -Wextra -o source.out source.c\"")))))
      (build-system gnu-build-system)
      (inputs (list python-wrapper))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "amalgamate.py" bin))))
           (replace 'check
             (lambda _
               (invoke "./test.sh"))))))
      (synopsis "Tool for amalgamating C source and header files")
      ;; The package is indeed a script file, and the term "amalgamate.py" is
      ;; used by upstream.
      (description "amalgamate.py aims to make it easy to use SQLite-style C
source and header amalgamation in projects.")
      (license license:bsd-3))))

(define-public cdecl
  (package
    (name "cdecl")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ibiblio.org/pub/linux/devel/lang/c/cdecl-"
                           version ".tar.gz"))
       (sha256
        (base32 "0dm98bp186r4cihli6fmcwzjaadgwl1z3b0zdxfik8h7hkqawk5p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       ,#~(list "LIBS=-lreadline"
                (string-append "BINDIR=" #$output "/bin")
                (string-append "MANDIR=" #$output "/share/man/man1"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)  ; No configure script.
         (add-after 'unpack 'fix-build
           (lambda _
             (substitute* "Makefile"
               (("lex cdlex.l")
                "flex cdlex.l"))
             (substitute* "cdecl.c"
               ;; Fix "error: conflicting types for ‘getline’".
               (("char \\* getline\\(\\)")
                "char * our_getline(void)")
               (("char \\* getline \\(\\)")
                "char * our_getline(void)")
               (("line = getline\\(\\)")
                "line = our_getline()")
               ;; Fix "error: conflicting types for ‘getopt’".
               (("int getopt\\(int,char \\*\\*,char \\*\\);")
                "")
               ;; Fix invalid use of "restrict" as a variable name.
               (("i, j, restrict")
                "i, j, restriction")
               (("restrict =")
                "restriction =")
               ;; Fix "warning: implicit declaration of function ‘add_history’".
               (("# include <readline/readline.h>" all)
                (string-append all "\n# include <readline/history.h>"))
               ;; Fix "warning: implicit declaration of function ‘dotmpfile_from_string’".
               (("void setprogname\\(char \\*\\);" all)
                (string-append all "\nint dotmpfile_from_string(char *);"))
               ;; Fix "warning: implicit declaration of function ‘completion_matches’".
               (("matches = completion_matches\\(text, command_completion\\);")
                "matches = rl_completion_matches(text, command_completion);")
               (("char \\* command_completion\\(char \\*, int\\);")
                "char * command_completion(const char *, int);")
               (("char \\* command_completion\\(char \\*text, int flag\\)")
                "char * command_completion(const char *text, int flag)")
               ;; Fix "warning: ‘CPPFunction’ is deprecated".
               (("rl_attempted_completion_function = \\(CPPFunction \\*\\)attempt_completion;")
                "rl_attempted_completion_function = (rl_completion_func_t *)attempt_completion;")
               ;; Fix "warning: ‘Function’ is deprecated".
               (("rl_completion_entry_function = \\(Function \\*\\)keyword_completion;")
                "rl_completion_entry_function = (rl_compentry_func_t *)keyword_completion;"))
             ;; Fix typo in man page.
             (substitute* "cdecl.1"
               (("<storage>\t::= auto \\| extern \\| register \\| auto")
                "<storage>\t::= auto | extern | register | static"))))
         (add-before 'install 'create-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (mkdir-p bin)
               (mkdir-p man)))))
       #:tests? #f))  ; No "check" target.
    (native-inputs (list bison flex))
    (inputs (list readline))
    (home-page "https://www.ibiblio.org/pub/linux/devel/lang/c/")
    (synopsis "Turn English phrases into C or C++ declarations and vice versa")
    (description "@code{cdecl} is a program that turns English-like phrases into C
declarations.  It can also translate C into pseudo-English.  It also handles
type casts and C++.  It has command-line editing and history with the GNU
Readline library.")
    (license license:public-domain)))

(define-public cscope
  (package
    (name "cscope")
    (version "15.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/cscope/cscope/"
                           "v" version "/cscope-" version ".tar.gz"))
       (sha256
        (base32 "0ngiv4aj3rr35k3q3wjx0y19gh7i1ydqa0cqip6sjwd8fph5ll65"))))
    (build-system gnu-build-system)
    (inputs (list ncurses))
    (arguments
     `(#:configure-flags
       ;; Specify the correct ncurses directory to prevent incorrect fallback
       ;; on SysV curses.
       (list (string-append "--with-ncurses="
                            (assoc-ref %build-inputs "ncurses")))))
    (home-page "https://cscope.sourceforge.net")
    (synopsis "Tool for browsing source code")
    (description
     "Cscope is a text screen based source browsing tool.  Although it is
primarily designed to search C code (including lex and yacc files), it can
also be used for C++ code.

Using cscope, you can easily search for where symbols are used and defined.")
    (license license:bsd-3)))

(define-public xenon
  (package
    (name "xenon")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xenon" version))
       (sha256
        (base32
         "1f4gynjzfckm3rjfywwgz1c7icfx3zjqirf16aj73xv0c9ncpffj"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f)) ;test suite not shipped with the PyPI archive
    (inputs (list python-pyyaml python-radon python-requests))
    (home-page "https://xenon.readthedocs.org/")
    (synopsis "Monitor code metrics for Python on your CI server")
    (description
     "Xenon is a monitoring tool based on Radon.  It monitors code complexity.
Ideally, @code{xenon} is run every time code is committed.  Through command
line options, various thresholds can be set for the complexity of code.  It
will fail (i.e., it will exit with a non-zero exit code) when any of these
requirements is not met.")
    (license license:expat)))

(define-public python-xenon
  (deprecated-package "python-xenon" xenon))
