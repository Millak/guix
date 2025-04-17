;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2016, 2018 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2023, 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Amirouche <amirouche@hypermove.net>
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Taylan Kammer <taylan.kammer@gmail.com>
;;; Copyright © 2020-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021, 2024 Timothy Sample <samplet@ngyro.com>
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

(define-module (gnu packages guile)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix deprecation)
  #:use-module (guix utils))

;;; Commentary:
;;;
;;; GNU Guile, and modules and extensions.
;;;
;;; Code:

(define-public guile-1.8
  (package
   (name "guile")
   (version "1.8.8")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/guile/guile-" version
                                ".tar.gz"))
            (sha256
             (base32
              "0l200a0v7h8bh0cwz6v7hc13ds39cgqsmfrks55b1rbj5vniyiy3"))
            (patches (search-patches "guile-1.8-cpp-4.5.patch"))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags '("--disable-error-on-warning"

                                    ;; Build with '-O1' to work around GC
                                    ;; crash on x86_64:
                                    ;; <https://issues.guix.gnu.org/50427>.
                                    "CFLAGS=-O1 -g -Wall")

                ;; Insert a phase before `configure' to patch things up.
                #:phases
                (modify-phases %standard-phases
                  (add-before 'configure 'patch-stuff
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Add a call to `lt_dladdsearchdir' so that
                      ;; `libguile-readline.so' & co. are in the
                      ;; loader's search path.
                      (substitute* "libguile/dynl.c"
                        (("lt_dlinit.*$" match)
                         (format #f
                                 "  ~a~%  lt_dladdsearchdir(\"~a/lib\");~%"
                                 match
                                 (assoc-ref outputs "out"))))

                      ;; The usual /bin/sh...
                      (substitute* "ice-9/popen.scm"
                        (("/bin/sh") (which "sh")))))
                  (add-after 'install 'add-libxcrypt-reference-pkgconfig
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (define out (assoc-ref outputs "out"))
                      (define libxcrypt
                        (false-if-exception
                         (dirname (search-input-file inputs "lib/libcrypt.so.1"))))
                      (when libxcrypt
                        (substitute*
                            (find-files (string-append out "/lib/pkgconfig")
                                        ".*\\.pc")
                          (("-lcrypt")
                           (string-append "-L" libxcrypt " -lcrypt")))))))

                ;; XXX: Several numerical tests and tests related to
                ;; 'inet-pton' fail on glibc 2.33/GCC 10.  Disable them.
                ;; TODO: Remove this package when its dependents no longer
                ;; need it.
                #:tests? #f))

   ;; When cross-compiling, a native version of Guile itself is needed.
   (native-inputs (if (%current-target-system)
                      `(("self" ,this-package))
                      '()))

   (inputs (list gawk libxcrypt readline))

   ;; Since `guile-1.8.pc' has "Libs: ... -lgmp -lltdl", these must be
   ;; propagated.
   (propagated-inputs (list gmp libltdl))

   (native-search-paths
    (list (search-path-specification
           (variable "GUILE_LOAD_PATH")
           (files '("share/guile/site")))))

   (synopsis "Scheme implementation intended especially for extensions")
   (description
    "Guile is the GNU Ubiquitous Intelligent Language for Extensions, the
official extension language of the GNU system.  It is an implementation of
the Scheme language which can be easily embedded in other applications to
provide a convenient means of extending the functionality of the application
without requiring the source code to be rewritten.")
   (home-page "https://www.gnu.org/software/guile/")
   (license license:lgpl2.0+)))

(define-public guile-2.0
  (package
   (name "guile")
   (version "2.0.14")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/guile/guile-" version
                                ".tar.xz"))
            (sha256
             (base32
              "10lxc6l5alf3lzbs3ihnbfy6dfcrsyf8667wa57f26vf4mk2ai78"))))
   (build-system gnu-build-system)

   ;; When cross-compiling, a native version of Guile itself is needed.
   (native-inputs
    (append (list pkg-config)
            (if (%current-target-system)
                (list this-package)
                '())))
   (inputs
    (append (list libffi libxcrypt)
            (libiconv-if-needed)

            ;; We need Bash when cross-compiling because some of the scripts
            ;; in bin/ refer to it.  Use 'bash-minimal' because we don't need
            ;; an interactive Bash with Readline and all.
            (if (target-mingw?) '() (list bash-minimal))))
   (propagated-inputs
    (list
     ;; These ones aren't normally needed here, but since `libguile-2.0.la'
     ;; reads `-lltdl -lunistring', adding them here will add the needed
     ;; `-L' flags.  As for why the `.la' file lacks the `-L' flags, see
     ;; <http://thread.gmane.org/gmane.comp.lib.gnulib.bugs/18903>.
     libunistring

     ;; Depend on LIBLTDL, not LIBTOOL.  That way, we avoid some the extra
     ;; dependencies that LIBTOOL has, which is helpful during bootstrap.
     libltdl

     ;; The headers and/or `guile-2.0.pc' refer to these packages, so they
     ;; must be propagated.
     libgc
     gmp))

   (outputs '("out" "debug"))

   (arguments
    `(#:configure-flags
      ,(if (target-x86-32?)               ;<https://issues.guix.gnu.org/49368>
           ''("--disable-static" "CFLAGS=-g -O2 -fexcess-precision=standard")
           ''("--disable-static"))                ;saves 3 MiB

      ;; Work around non-reproducible .go files as described in
      ;; <https://bugs.gnu.org/20272>, which affects 2.0, 2.2, and 3.0 so far.
      #:parallel-build? #f

      #:phases
      (modify-phases %standard-phases
        ,@(if (system-hurd?)
              '((add-after 'unpack 'disable-tests
                  (lambda _
                    ;; Hangs at: "Running 00-repl-server.test"
                    (rename-file "test-suite/tests/00-repl-server.test" "00-repl-server.test")
                    ;; Sometimes Hangs at: "Running 00-socket.test"
                    (rename-file "test-suite/tests/00-socket.test" "00-socket.test")
                    ;; FAIL: srfi-18.test: thread-sleep!: thread sleeps fractions of a second
                    (rename-file "test-suite/tests/srfi-18.test" "srfi-18.test")
                    ;; failed to remove 't-guild-compile-7215.go.tdL7yC
                    (substitute* "test-suite/standalone/Makefile.in"
                      (("test-guild-compile ") "")))))
              '())
        ,@(if (system-hurd?)
              '((add-after 'unpack 'disable-threads.tests
                  (lambda _
                    ;; Many tests hang, esp. (join-thread ..), also others.
                    (rename-file "test-suite/tests/threads.test" "threads.test"))))
              '())
        (add-before 'configure 'pre-configure
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Tell (ice-9 popen) the file name of Bash.

            ;; TODO: On the next rebuild cycle, unconditionally use
            ;; 'search-input-file' instead of 'assoc-ref'.
            (let ((bash (assoc-ref inputs "bash")))
              (substitute* "module/ice-9/popen.scm"
                ;; If bash is #f allow fallback for user to provide
                ;; "bash" in PATH.  This happens when cross-building to
                ;; MinGW for which we do not have Bash yet.
                (("/bin/sh")
                 ,(cond ((target-mingw?)
                         "bash")
                        ((%current-target-system)
                         '(search-input-file inputs "/bin/bash"))
                        (else
                         '(string-append bash "/bin/bash")))))
              #t)))
        (add-after 'install 'add-libxcrypt-reference-pkgconfig
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (define out (assoc-ref outputs "out"))
            (define libxcrypt
              (false-if-exception
               (dirname (search-input-file inputs "lib/libcrypt.so.1"))))
            (when libxcrypt
              (substitute*
                  (find-files (string-append out "/lib/pkgconfig")
                              ".*\\.pc")
                (("-lcrypt")
                 (string-append "-L" libxcrypt " -lcrypt")))))))))

   (native-search-paths
    (list (search-path-specification
           (variable "GUILE_LOAD_PATH")
           (files '("share/guile/site/2.0")))
          (search-path-specification
           (variable "GUILE_LOAD_COMPILED_PATH")
           (files '("lib/guile/2.0/site-ccache")))))

   (synopsis "Scheme implementation intended especially for extensions")
   (description
    "Guile is the GNU Ubiquitous Intelligent Language for Extensions, the
official extension language of the GNU system.  It is an implementation of
the Scheme language which can be easily embedded in other applications to
provide a convenient means of extending the functionality of the application
without requiring the source code to be rewritten.")
   (home-page "https://www.gnu.org/software/guile/")
   (license license:lgpl3+)))

(define-public guile-2.2
  (package
    (inherit guile-2.0)
    (name "guile")
    (version "2.2.7")
    (source (origin
              (method url-fetch)

              ;; Note: we are limited to one of the compression formats
              ;; supported by the bootstrap binaries, so no lzip here.
              (uri (string-append "mirror://gnu/guile/guile-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "013mydzhfswqci6xmyc1ajzd59pfbdak15i0b090nhr9bzm7dxyd"))
              (modules '((guix build utils)))
              (patches (search-patches
                        "guile-2.2-skip-oom-test.patch"
                        "guile-2.2-skip-so-test.patch"))

              ;; Remove the pre-built object files.  Instead, build everything
              ;; from source, at the expense of significantly longer build
              ;; times (almost 3 hours on a 4-core Intel i5).
              (snippet '(begin
                          (for-each delete-file
                                    (find-files "prebuilt" "\\.go$"))
                          #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments guile-2.0)
       ((#:configure-flags flags ''())
        (if (target-x86-32?)            ;<https://issues.guix.gnu.org/49368>
            `(append '("--disable-static")
                 '("CFLAGS=-g -O2 -fexcess-precision=standard"))
            flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            #$@(if (system-hurd?)
                   #~((delete 'disable-threads.tests))
                   '())))))

    (properties '((timeout . 72000)               ;20 hours
                  (max-silent-time . 36000)))     ;10 hours (needed on ARM
                                                  ;  when heavily loaded)
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_LOAD_PATH")
            (files '("share/guile/site/2.2")))
           (search-path-specification
            (variable "GUILE_LOAD_COMPILED_PATH")
            (files '("lib/guile/2.2/site-ccache")))))))

(define-public guile-2.2.4
  (package
    (inherit guile-2.2)
   (version "2.2.4")
   (source (origin
             (inherit (package-source guile-2.2))
             (uri (string-append "mirror://gnu/guile/guile-" version
                                 ".tar.xz"))
             (sha256
              (base32
               "07p3g0v2ba2vlfbfidqzlgbhnzdx46wh2rgc5gszq1mjyx5bks6r"))))))

(define-public guile-3.0
  ;; This is the latest Guile stable version.
  (package
    (inherit guile-2.2)
    (name "guile")
    (version "3.0.9")
    (source (origin
              (inherit (package-source guile-2.2))
              (uri (string-append "mirror://gnu/guile/guile-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "03bm1mnfc9kkg2ls942a0js7bxrdzmcffgrgg6anwdmjfan2a9hs"))
              (patches '())
              ;; Replace the snippet because the oom-test still
              ;; fails on some 32-bit architectures.
              (snippet '(for-each delete-file
                                  (find-files "prebuilt" "\\.go$")))))

    ;; Build with the bundled mini-GMP to avoid interference with GnuTLS' own
    ;; use of GMP via Nettle: <https://issues.guix.gnu.org/46330>.
    (propagated-inputs
     (modify-inputs (package-propagated-inputs guile-2.2)
       (delete "gmp" "libltdl")))
    (arguments
     (substitute-keyword-arguments (package-arguments guile-2.0)
       ;; Guile 3.0.9 is bit-reproducible when built in parallel, thanks to
       ;; its multi-stage build process for cross-module inlining, except when
       ;; cross-compiling.
       ((#:parallel-build? _ #f)
        (not (%current-target-system)))
       ((#:configure-flags flags #~'())
        ;; XXX: JIT-enabled Guile crashes in obscure ways on GNU/Hurd.
        #~(cons* #$@(if (target-hurd?)
                        #~("--disable-jit")
                        #~())
                 ;; -fexcess-precision=standard is required when compiling for
                 ;; i686-linux, otherwise "numbers.test" will fail
                 ;; (see <https://issues.guix.gnu.org/49368> and
                 ;; <https://issues.guix.gnu.org/49659>).
                 ;; TODO: Keep this in GUILE-2.2 and remove from here on next
                 ;; rebuild cycle.
                 #$@(if (target-x86-32?)
                        #~("CFLAGS=-g -O2 -fexcess-precision=standard")
                        #~())
                 "--enable-mini-gmp"
                 '("--disable-static")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'check 'disable-stack-overflow-test
              (lambda _
                ;; This test can invoke the "OOM killer", especially when
                ;; running on emulated hardware (QEMU).  Skip it.
                (substitute* "test-suite/standalone/test-stack-overflow"
                  (("!#")
                   "!#\n(exit 77)\n"))))

            #$@(if (target-hurd?)
                   #~((add-before 'build 'patch-posix-spawn-usage
                        (lambda _
                          ;; TODO: Move patch to 'source' on next rebuild
                          ;; cycle.
                          (define patch
                            #$(local-file
                               (search-patch "guile-hurd-posix-spawn.patch")))
                          (invoke "patch" "--force" "-p1" "-i" patch))))
                   #~())
            #$@(if (system-hurd?)
                   #~((add-after 'unpack 'disable-popen.test-no-duplicate
                        ;; This test hangs on the Hurd.
                        (lambda _
                          (substitute* "test-suite/tests/popen.test"
                            (("\\(pass-if \"no duplicate\".*" all)
                             (string-append
                              all
                              (object->string
                               '(when (string-ci= "GNU"
                                                  (vector-ref (uname) 0))
                                  (throw 'unresolved)))))))))
                   #~())
            #$@(if (target-ppc32?)
                   #~((add-after 'unpack 'adjust-bootstrap-flags
                        (lambda _
                          ;; Upstream knows about suggested solution.
                          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45214
                          ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=977223#46
                          (substitute* "stage0/Makefile.in"
                            (("^GUILE_OPTIMIZATIONS.*")
                             "GUILE_OPTIMIZATIONS = -O1 -Oresolve-primitives -Ocps\n")))))
                   #~())
            #$@(if (target-powerpc?)
                   #~((add-after 'unpack 'skip-oom-test
                        (lambda _
                          ;; This test hangs with guile-3.0.9 and libgc-8.2.2 and
                          ;; fails completely on powerpc-linux.
                          (substitute* "test-suite/standalone/test-out-of-memory"
                            (("!#") "!#\n\n(exit 77)\n")))))
                   #~())
            #$@(if (or (target-ppc32?)
                       (target-riscv64?))
                   #~((add-after 'unpack 'skip-failing-fdes-test
                        (lambda _
                          ;; ERROR: ((system-error "seek" "~A" ("Bad file descriptor") (9)))
                          (substitute* "test-suite/tests/ports.test"
                            (("fdes not closed\"" all) (string-append all "(exit 77)")))
                          #t)))
                   #~())))))

    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_LOAD_PATH")
            (files '("share/guile/site/3.0")))
           (search-path-specification
            (variable "GUILE_LOAD_COMPILED_PATH")
            (files '("lib/guile/3.0/site-ccache"
                     "share/guile/site/3.0")))))))

(define-public guile-3.0-latest guile-3.0)

;;; The symbol guile-3.0/fixed should be used when guile-3.0 needs fixes
;;; (security or else) and this deprecation could be removed.
(define-deprecated/public-alias guile-3.0/fixed guile-3.0/pinned)

(define-public guile-3.0/pinned
  ;; A package of Guile that's rarely changed.  It is the one used in the
  ;; `base' module, and thus changing it entails a full rebuild.
  (package
    (inherit guile-3.0)
    (properties '((hidden? . #t)            ;people should install 'guile-2.2'
                  (timeout . 72000)             ;20 hours
                  (max-silent-time . 36000))))) ;10 hours (needed on ARM
                                                ;  when heavily loaded)

(define-public guile-next
  (let ((version "3.0.10")
        (revision "1")
        (commit "402e0dfa33f442ad238a0f82a332efa438538840"))
    (package
      (inherit guile-3.0)
      (name "guile-next")
      (version (git-version version revision commit))
      (source (origin
                ;; The main goal here is to allow for '--with-branch'.
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/guile.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1l3xzv7kygdb7ca6nzayg9ingn7d5d9xsjlv1r7y7819axx3457f"))))
      (arguments
       (substitute-keyword-arguments (package-arguments guile-3.0)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-before 'bootstrap 'set-version
                (lambda _
                  ;; Tell 'git-version-gen' what version this is, or it will
                  ;; just pick "UNKNOWN", making it unusable as a replacement
                  ;; for 'guile-3.0'.  XXX: This is inaccurate when using
                  ;; '--with-branch' but using (package-version this-package)
                  ;; wouldn't give us a valid version string.
                  (call-with-output-file ".tarball-version"
                    (lambda (port)
                      (display #$version port)))))
              #$@(if (target-hurd?)
                     #~((delete 'patch-posix-spawn-usage))
                     #~())))))
      (native-inputs
       (modify-inputs (package-native-inputs guile-3.0)
         (prepend autoconf
                  automake
                  libtool
                  flex
                  gnu-gettext
                  texinfo
                  gperf)
         (replace "guile" this-package)))
      (synopsis "Development version of GNU Guile"))))

(define* (make-guile-readline guile #:optional (name "guile-readline"))
  (package
    (name name)
    (version (package-version guile))
    (source (package-source guile))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~'("--disable-silent-rules"
               "--enable-mini-gmp")               ;for Guile >= 3.0.6

           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'chdir
                 (lambda* (#:key outputs #:allow-other-keys)
                   (invoke "make" "-C" "libguile" "scmconfig.h")
                   (invoke "make" "-C" "lib")
                   (chdir "guile-readline")

                   (substitute* "Makefile"
                     (("../libguile/libguile-[[:graph:]]+\\.la")
                      ;; Remove dependency on libguile-X.Y.la.
                      "")
                     (("^READLINE_LIBS = (.*)$" _ libs)
                      ;; Link against the provided libguile.
                      (string-append "READLINE_LIBS = "
                                     "-lguile-$(GUILE_EFFECTIVE_VERSION) "
                                     libs "\n"))
                     (("\\$\\(top_builddir\\)/meta/build-env")
                      ;; Use the provided Guile, not the one from
                      ;; $(builddir).
                      "")

                     ;; Install modules to the 'site' directories.
                     (("^moddir = .*$")
                      "moddir = $(pkgdatadir)/site/$(GUILE_EFFECTIVE_VERSION)\n")
                     (("^ccachedir = .*$")
                      "ccachedir = $(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/site-ccache\n"))

                   ;; Load 'guile-readline.so' from the right place.
                   (substitute* "ice-9/readline.scm"
                     (("load-extension \"guile-readline\"")
                      (format #f "load-extension \
 (string-append ~s \"/lib/guile/\" (effective-version) \"/extensions/guile-readline\")"
                              (assoc-ref outputs "out"))))
                   #t)))))
    (home-page (package-home-page guile))
    (native-inputs (package-native-inputs guile))
    (propagated-inputs (package-propagated-inputs guile))
    (inputs (modify-inputs (package-inputs guile)
              (prepend guile readline)))
    (synopsis "Line editing support for GNU Guile")
    (description
     "This module provides line editing support via the Readline library for
GNU@tie{}Guile.  Use the @code{(ice-9 readline)} module and call its
@code{activate-readline} procedure to enable it.")
    (license license:gpl3+)))

(define-public guile-readline
  (make-guile-readline guile-3.0))

(define-public guile2.2-readline
  (make-guile-readline guile-2.2 "guile2.2-readline"))

(define (guile-variant-package-name prefix)
  (lambda (name)
    "Return NAME with PREFIX instead of \"guile-\", when applicable."
    (if (string-prefix? "guile-" name)
        (string-append prefix "-"
                       (string-drop name
                                    (string-length "guile-")))
        name)))

(define package-for-guile-2.0
  ;; A procedure that rewrites the dependency tree of the given package to use
  ;; GUILE-2.0 instead of GUILE-3.0.
  (package-input-rewriting `((,guile-3.0 . ,guile-2.0))
                           (guile-variant-package-name "guile2.0")
                           #:deep? #f))

(define package-for-guile-2.2
  (package-input-rewriting `((,guile-3.0 . ,guile-2.2))
                           (guile-variant-package-name "guile2.2")
                           #:deep? #f))

(define-public guile-for-guile-emacs
  (let ((commit "e62c0d1b32f625fcbaa733c32a88622846aee905")
        (revision "2"))
    (package (inherit guile-next)
      (name "guile-for-guile-emacs")
      (version (git-version "3.0.7-81" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://codeberg.org/lyrra/guile")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xfnd99iws9dwk5va8bmqpysmb8pnb1w91rw7rbfzzklyfvpibh6"))))
      (arguments
       (substitute-keyword-arguments (package-arguments guile-next)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-before 'check 'skip-failing-tests
                (lambda _
                  (delete-file "test-suite/tests/version.test"))))))))))


;;;
;;; Extensions.
;;;

(define-public guile-json-1
  (package
    (name "guile-json")
    (version "1.3.2")
    (home-page "https://github.com/aconchillo/guile-json")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0m6yzb169r6iz56k3nkncjaiijwi4p0x9ijn1p5ax3s77jklxy9k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")))   ;to prevent guild warnings
    (native-inputs (list pkg-config guile-2.2))
    (inputs (list guile-2.2))
    (synopsis "JSON module for Guile")
    (description
     "Guile-JSON supports parsing and building JSON documents according to the
specification.  These are the main features:

@itemize
@item Strictly complies to @uref{http://json.org, specification}.
@item Build JSON documents programmatically via macros.
@item Unicode support for strings.
@item Allows JSON pretty printing.
@end itemize\n")

    ;; Version 1.2.0 switched to GPLv3+ (from LGPLv3+).
    (license license:gpl3+)))

(define-public guile2.0-json
  (package-for-guile-2.0 guile-json-1))

(define-public guile-json-3
  ;; This version is incompatible with 1.x; see the 'NEWS' file.
  (package
    (inherit guile-json-1)
    (name "guile-json")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0nj0684qgh6ppkbdyxqfyjwsv2qbyairxpi8fzrhsi3xnc7jn4im"))))
    (native-inputs (list pkg-config guile-3.0))
    (inputs (list guile-3.0))))

(define-public guile-json-4
  (package
    (inherit guile-json-3)
    (name "guile-json")
    (version "4.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "127k2xc07w1gnyqs40z4865l8p3ra5xgpcn569dz04lxsa709fiq"))))))

(define-public guile2.2-json
  (package-for-guile-2.2 guile-json-4))

;; There are two guile-gdbm packages, one using the FFI and one with
;; direct C bindings, hence the verbose name.

(define-public guile-gdbm-ffi
  (package
    (name "guile-gdbm-ffi")
    (version "20120209.fa1d5b6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ijp/guile-gdbm")
                    (commit "fa1d5b6231d0e4d096687b378c025f2148c5f246")))
              (file-name (string-append name "-" version "-checkout"))
              (patches (search-patches
                        "guile-gdbm-ffi-support-gdbm-1.14.patch"))
              (sha256
               (base32
                "1j8wrsw7v9w6qkl47xz0rdikg50v16nn6kbs3lgzcymjzpa7babj"))))
    (build-system guile-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'move-examples
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Move examples where they belong.
                      (let* ((out (assoc-ref outputs "out"))
                             (doc (string-append out "/share/doc/"
                                                 (strip-store-file-name out)
                                                 "/examples")))
                        (copy-recursively "examples" doc)
                        (delete-file-recursively "examples")
                        #t)))
                  (add-after 'unpack 'set-libgdbm-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "gdbm.scm"
                        (("\\(dynamic-link \"libgdbm\"\\)")
                         (format #f "(dynamic-link \"~a/lib/libgdbm.so\")"
                                 (assoc-ref inputs "gdbm"))))
                      #t)))))
    (native-inputs (list guile-3.0))
    (inputs (list gdbm))
    (home-page "https://github.com/ijp/guile-gdbm")
    (synopsis "Guile bindings to the GDBM library via Guile's FFI")
    (description
     "Guile bindings to the GDBM key-value storage system, using
Guile's foreign function interface.")
    (license license:gpl3+)))

(define-public guile2.0-gdbm-ffi
  (package-for-guile-2.0 guile-gdbm-ffi))

(define-public guile2.2-gdbm-ffi
  (package-for-guile-2.2 guile-gdbm-ffi))

(define-public guile-sqlite3
  (package
    (name "guile-sqlite3")
    (version "0.1.3")
    (home-page "https://notabug.org/guile-sqlite3/guile-sqlite3.git")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0qqygvlpz63phdi2p5p8ncp80dci230qfa3pwds8yfxqqaablmhb"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake guile-3.0 pkg-config))
    (inputs (list guile-3.0 sqlite))
    (synopsis "Access SQLite databases from Guile")
    (description
     "This package provides Guile bindings to the SQLite database system.")
    (license license:gpl3+)))

(define-public guile2.0-sqlite3
  (package-for-guile-2.0 guile-sqlite3))

(define-public guile2.2-sqlite3
  (package-for-guile-2.2 guile-sqlite3))

(define-public guile-bytestructures
  (package
    (name "guile-bytestructures")
    (version "1.0.10")
    (home-page "https://github.com/TaylanUB/scheme-bytestructures")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14k50jln32kkxv41hvsdgjkkfj6xlv06vc1caz01qkgk1fzh72nk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")     ;to prevent guild warnings

       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-doc
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (package ,(package-full-name this-package "-"))
                             (doc (string-append out "/share/doc/" package)))
                        (install-file "README.md" doc)
                        #t))))))
    (native-inputs (list autoconf automake pkg-config guile-3.0))
    (inputs (list guile-3.0))
    (synopsis "Structured access to bytevector contents for Guile")
    (description
     "Guile bytestructures offers a system imitating the type system
of the C programming language, to be used on bytevectors.  C's type
system works on raw memory, and Guile works on bytevectors which are
an abstraction over raw memory.  It's also more powerful than the C
type system, elevating types to first-class status.")
    (license license:gpl3+)
    (properties '((upstream-name . "bytestructures")))))

(define-public guile2.0-bytestructures
  (package-for-guile-2.0 guile-bytestructures))

(define-public guile2.2-bytestructures
  (package-for-guile-2.2 guile-bytestructures))

(define-public guile-git
  (package
    (name "guile-git")
    (version "0.10.0")
    (home-page "https://gitlab.com/guile-git/guile-git.git")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01scznpg7356ygdlgn1f2892bwg6fsvp37hbmsk4gsg0g49aj4la"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")       ; to prevent guild warnings
       ;; https://gitlab.com/guile-git/guile-git/-/issues/20
       ,@(if (target-ppc32?)
           `(#:phases
             (modify-phases %standard-phases
               (add-after 'unpack 'skip-failing-test
                 (lambda _
                   (substitute* "Makefile.am"
                     ((".*tests/blob\\.scm.*") ""))))))
           '())
       ,@(if (system-hurd?)
             (list
              #:phases
              #~(modify-phases %standard-phases
                  (add-after 'unpack 'skip-tests/hurd
                    (lambda _
                      (substitute* "tests/proxy.scm"
                        (("\\(test-begin.*" all)
                         (string-append
                          all
                          "(when (string-ci= \"GNU\" (vector-ref (uname) 0))\n"
                          "  (test-skip 1))\n")))))))
             '())))
    (native-inputs
     (list pkg-config autoconf automake texinfo guile-3.0 guile-bytestructures))
    (inputs
     (list guile-3.0 libgit2-1.8))
    (propagated-inputs
     (list guile-bytestructures))
    (synopsis "Guile bindings for libgit2")
    (description
     "This package provides Guile bindings to libgit2, a library to
manipulate repositories of the Git version control system.")
    (license license:gpl3+)))

(define-public guile2.2-git
  (package-for-guile-2.2 guile-git))

(define-public guile2.0-git
  ;; Guile-Git 0.8.0 no longer supports Guile 2.0.
  (deprecated-package "guile2.0-git" guile2.2-git))

(define-public guile-zlib
  (package
    (name "guile-zlib")
    (version "0.2.2")
    (source
     (origin
       ;; XXX: Do not use "git-fetch" method here that would create and
       ;; endless inclusion loop, because this package is used as an extension
       ;; in the same method.
       (method url-fetch)
       (uri
        (string-append "https://notabug.org/guile-zlib/guile-zlib/archive/v"
                       version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        ;; content nar-sha256: 0rwjlqr1hl2vczs16xsihw8pyj6s70p1yv9ky0sawhm6g30639k9
        (base32
         "1h9q7rw8bh5mwa8qjik0pqwcr8v9hnk7xnvwcbsg6yfa695wlv3g"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake pkg-config guile-3.0))
    (inputs (list guile-3.0 zlib))
    (synopsis "Guile bindings to zlib")
    (description
     "This package provides Guile bindings for zlib, a lossless
data-compression library.  The bindings are written in pure Scheme by using
Guile's foreign function interface.")
    (home-page "https://notabug.org/guile-zlib/guile-zlib")
    (license license:gpl3+)))

(define-public guile2.2-zlib
  (package-for-guile-2.2 guile-zlib))

(define-public guile-lzlib
  (package
    (name "guile-lzlib")
    (version "0.3.0")
    (home-page "https://notabug.org/guile-lzlib/guile-lzlib")
    (source
     (origin
       ;; Note: Until "builtin:git-download" can be taken for granted, this
       ;; must be 'url-fetch', not 'git-fetch', to avoid a circular dependency
       ;; with the 'git-fetch' derivation on systems that lack
       ;; "builtin:git-download".
       (method url-fetch)
       (uri (string-append home-page "/archive/" version ".tar.gz"))
       ;; content nar-sha256: 19870njb3q5h6zy239gvra92ji077c6s8xm0hgcn42z74q5wqnk6
       (file-name (string-append "guile-lzlib-" version ".tar.gz"))
       (sha256
        (base32
         "1whgmwkr1v8m63p4aaqn8blwl9vcrswwhbfv4bm0aghl5a6rryd7"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~'("GUILE_AUTO_COMPILE=0") ;prevent guild warnings
      #:phases (if (or (%current-target-system) (target-hurd64?))
                   #~(modify-phases %standard-phases
                       (add-after 'unpack 'apply-hurd64-patch
                         (lambda _
                           (let ((patch
                                  #$(local-file
                                     (search-patch
                                      "guile-lzlib-hurd64.patch"))))
                             (invoke "patch" "--force" "-p1" "-i" patch)))))
                   #~%standard-phases)))
    (native-inputs (list autoconf automake pkg-config guile-3.0))
    (inputs (list guile-3.0 lzlib))
    (synopsis "Guile bindings to lzlib")
    (description
     "This package provides Guile bindings for lzlib, a C library for
in-memory LZMA compression and decompression.  The bindings are written in
pure Scheme by using Guile's foreign function interface.")
    (license license:gpl3+)))

(define-public guile2.2-lzlib
  (package-for-guile-2.2 guile-lzlib))

(define-public guile-zstd
  (package
    (name "guile-zstd")
    (version "0.1.1")
    (home-page "https://notabug.org/guile-zstd/guile-zstd")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c8l7829b5yx8wdc0mrhzjfwb6h9hb7cd8dfxcr71a7vlsi86310"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake pkg-config guile-3.0))
    (inputs (list `(,zstd "lib") guile-3.0))
    (synopsis "GNU Guile bindings to the zstd compression library")
    (description
     "This package provides a GNU Guile interface to the zstd (``zstandard'')
compression library.")
    (license license:gpl3+)))

(define-public guile-lzma
  (package
    (name "guile-lzma")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.ngyro.com/guile-lzma/guile-lzma-"
                           version ".tar.gz"))
       (sha256
        (base32 "0pnfzk92p9y5ymjq6rq619b9fy0dflv56jwg00wlvvbjssb6i1ib"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake guile-3.0 guile-bytestructures pkg-config))
    (inputs (list guile-3.0 xz))
    (propagated-inputs (list guile-bytestructures))
    (home-page "https://ngyro.com/software/guile-lzma.html")
    (synopsis "Guile bindings for liblzma (XZ)")
    (description "Guile-LZMA is a Guile wrapper for the liblzma (XZ)
library.  It exposes an interface similar to other Guile compression
libraries, like Guile-zlib.")
    (license license:gpl3+)))

(define-public guile-bzip2
  (package
    (name "guile-bzip2")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.ngyro.com/guile-bzip2/guile-bzip2-"
                           version ".tar.gz"))
       (sha256
        (base32 "1qnxk5fzg8m9ik1ckhjvi22kkhd810mrg8jzxiizhk920b69wbdh"))))
    (build-system gnu-build-system)
    (native-inputs (list guile-3.0 guile-bytestructures pkg-config))
    (inputs (list guile-3.0 bzip2))
    (propagated-inputs (list guile-bytestructures))
    (home-page "https://ngyro.com/software/guile-bzip2.html")
    (synopsis "Guile bindings for libbzip2")
    (description "Guile-bzip2 is a Guile wrapper for the libbzip2
library.  It exposes an interface similar to other Guile compression
libraries, like Guile-zlib.")
    (license license:gpl3+)))

;;; guile.scm ends here
