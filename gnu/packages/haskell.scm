;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Tonton <tonton@riseup.net>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages haskell)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 regex)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define-public cl-yale-haskell
  (let ((commit "85f94c72a16c5f70301dd8db04cde9de2d7dd270")
        (revision "1"))
    (package
      (name "cl-yale-haskell")
      (version (string-append "2.0.5-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/yale-haskell.git")
                      (commit commit)))
                (file-name (string-append "yale-haskell-" commit "-checkout"))
                (sha256
                 (base32
                  "0bal3m6ryrjamz5p93bhs9rp5msk8k7lpcqr44wd7xs9b9k8w74g"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no tests
         ;; Stripping binaries leads to a broken executable lisp system image.
         #:strip-binaries? #f
         #:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda _
               (setenv "PRELUDE" "./progs/prelude")
               (setenv "HASKELL_LIBRARY" "./progs/lib")
               (setenv "PRELUDEBIN" "./progs/prelude/clisp")
               (setenv "HASKELLPROG" "./bin/clisp-haskell")
               #t)))))
      (inputs
       `(("clisp" ,clisp)))
      (home-page "https://git.elephly.net/software/yale-haskell.git")
      (synopsis "Port of the Yale Haskell system to CLISP")
      (description "This package provides the Yale Haskell system running on
top of CLISP.")
      (license license:bsd-4))))

(define ghc-bootstrap-x86_64-7.8.4
  (origin
    (method url-fetch)
    (uri
     "https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz")
    (sha256
     (base32
      "13azsl53xgj20mi1hj9x0xb32vvcvs6cpmvwx6znxhas7blh0bpn"))))

(define ghc-bootstrap-i686-7.8.4
  (origin
    (method url-fetch)
    (uri
     "https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-i386-unknown-linux-deb7.tar.xz")
    (sha256
     (base32
      "0wj5s435j0zgww70bj1d3f6wvnnpzlxwvwcyh2qv4qjq5z8j64kg"))))

;; 43 tests out of 3965 fail.
;;
;; Most of them do not appear to be serious:
;;
;; - some tests generate files referring to "/bin/sh" and "/bin/ls". I've not
;;   figured out how these references are generated.
;;
;; - Some tests allocate more memory than expected (ca. 3% above upper limit)
;;
;; - Some tests try to load unavailable libriries: Control.Concurrent.STM,
;;   Data.Vector, Control.Monad.State.
;;
;; - Test posix010 tries to check the existence of a user on the system:
;;   getUserEntryForName: does not exist (no such user)
(define-public ghc-7
  (package
    (name "ghc")
    (version "7.10.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.haskell.org/ghc/dist/"
                          version "/" name "-" version "-src.tar.xz"))
      (sha256
       (base32
        "1x8m4rp2v7ydnrz6z9g8x7z3x3d3pxhv2pixy7i7hkbqbdsp7kal"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (outputs '("out" "doc"))
    (inputs
     `(("gmp" ,gmp)
       ("ncurses" ,ncurses)
       ("libffi" ,libffi)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32
             "0qp9da9ar87zbyn6wjgacd2ic1vgzbi3cklxnhsmjqyafv9qaj4b"))))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)                ; for tests (fails with python-3)
       ("ghostscript" ,ghostscript)        ; for tests
       ("patchelf" ,patchelf)
       ;; GHC is built with GHC. Therefore we need bootstrap binaries.
       ("ghc-binary"
        ,(if (string-match "x86_64" (or (%current-target-system) (%current-system)))
             ghc-bootstrap-x86_64-7.8.4
             ghc-bootstrap-i686-7.8.4))))
    (arguments
     `(#:test-target "test"
       ;; We get a smaller number of test failures by disabling parallel test
       ;; execution.
       #:parallel-tests? #f

       ;; The DSOs use $ORIGIN to refer to each other, but (guix build
       ;; gremlin) doesn't support it yet, so skip this phase.
       #:validate-runpath? #f

       ;; Don't pass --build=<triplet>, because the configure script
       ;; auto-detects slightly different triplets for --host and --target and
       ;; then complains that they don't match.
       #:build #f

       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build rpath)
                  (srfi srfi-26)
                  (srfi srfi-1))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build rpath))
       #:configure-flags
       (list
        (string-append "--with-gmp-libraries="
                       (assoc-ref %build-inputs "gmp") "/lib")
        (string-append "--with-gmp-includes="
                       (assoc-ref %build-inputs "gmp") "/include")
        "--with-system-libffi"
        (string-append "--with-ffi-libraries="
                       (assoc-ref %build-inputs "libffi") "/lib")
        (string-append "--with-ffi-includes="
                       (assoc-ref %build-inputs "libffi") "/include"))
       ;; FIXME: The user-guide needs dblatex, docbook-xsl and docbook-utils.
       ;; Currently we do not have the last one.
       ;; #:make-flags
       ;; (list "BUILD_DOCBOOK_HTML = YES")
       #:phases
       (let* ((ghc-bootstrap-path
               (string-append (getcwd) "/" ,name "-" ,version "/ghc-bin"))
              (ghc-bootstrap-prefix
               (string-append ghc-bootstrap-path "/usr" )))
         (alist-cons-after
          'unpack-bin 'unpack-testsuite-and-fix-bins
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (with-directory-excursion ".."
              (copy-file (assoc-ref inputs "ghc-testsuite")
                         "ghc-testsuite.tar.xz")
              (system* "tar" "xvf" "ghc-testsuite.tar.xz"))
            (substitute*
                (list "testsuite/timeout/Makefile"
                      "testsuite/timeout/timeout.py"
                      "testsuite/timeout/timeout.hs"
                      "testsuite/tests/rename/prog006/Setup.lhs"
                      "testsuite/tests/programs/life_space_leak/life.test"
                      "libraries/process/System/Process/Internals.hs"
                      "libraries/unix/cbits/execvpe.c")
              (("/bin/sh") (which "sh"))
              (("/bin/rm") "rm"))
            #t)
          (alist-cons-after
           'unpack 'unpack-bin
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p ghc-bootstrap-prefix)
             (with-directory-excursion ghc-bootstrap-path
               (copy-file (assoc-ref inputs "ghc-binary")
                          "ghc-bin.tar.xz")
               (zero? (system* "tar" "xvf" "ghc-bin.tar.xz"))))
           (alist-cons-before
            'install-bin 'configure-bin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((binaries
                      (list
                       "./utils/ghc-pwd/dist-install/build/tmp/ghc-pwd"
                       "./utils/hpc/dist-install/build/tmp/hpc"
                       "./utils/haddock/dist/build/tmp/haddock"
                       "./utils/hsc2hs/dist-install/build/tmp/hsc2hs"
                       "./utils/runghc/dist-install/build/tmp/runghc"
                       "./utils/ghc-cabal/dist-install/build/tmp/ghc-cabal"
                       "./utils/hp2ps/dist/build/tmp/hp2ps"
                       "./utils/ghc-pkg/dist-install/build/tmp/ghc-pkg"
                       "./utils/unlit/dist/build/tmp/unlit"
                       "./ghc/stage2/build/tmp/ghc-stage2"))
                     (gmp (assoc-ref inputs "gmp"))
                     (gmp-lib (string-append gmp "/lib"))
                     (gmp-include (string-append gmp "/include"))
                     (ncurses-lib
                      (string-append (assoc-ref inputs "ncurses") "/lib"))
                     (ld-so (string-append (assoc-ref inputs "libc")
                                           ,(glibc-dynamic-linker)))
                     (libtinfo-dir
                      (string-append ghc-bootstrap-prefix
                                     "/lib/ghc-7.8.4/terminfo-0.4.0.0")))
                (with-directory-excursion
                    (string-append ghc-bootstrap-path "/ghc-7.8.4")
                  (setenv "CONFIG_SHELL" (which "bash"))
                  (setenv "LD_LIBRARY_PATH" gmp-lib)
                  ;; The binaries have "/lib64/ld-linux-x86-64.so.2" hardcoded.
                  (for-each
                   (cut system* "patchelf" "--set-interpreter" ld-so <>)
                   binaries)
                  ;; The binaries include a reference to libtinfo.so.5 which
                  ;; is a subset of libncurses.so.5.  We create a symlink in a
                  ;; directory included in the bootstrap binaries rpath.
                  (mkdir-p libtinfo-dir)
                  (symlink
                   (string-append ncurses-lib "/libncursesw.so."
                                  ;; Extract "6.0" from "6.0-20170930" if a
                                  ;; dash-separated version tag exists.
                                  ,(let* ((v (package-version ncurses))
                                          (d (or (string-index v #\-)
                                                 (string-length v))))
                                     (version-major+minor (string-take v d))))
                   (string-append libtinfo-dir "/libtinfo.so.5"))

                  (setenv "PATH"
                          (string-append (getenv "PATH") ":"
                                         ghc-bootstrap-prefix "/bin"))
                  (system*
                   (string-append (getcwd) "/configure")
                   (string-append "--prefix=" ghc-bootstrap-prefix)
                   (string-append "--with-gmp-libraries=" gmp-lib)
                   (string-append "--with-gmp-includes=" gmp-include)))))
            (alist-cons-before
             'configure 'install-bin
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (with-directory-excursion
                   (string-append ghc-bootstrap-path "/ghc-7.8.4")
                 (zero? (system* "make" "install"))))
             %standard-phases)))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-8.0
  (package
    (name "ghc")
    (version "8.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.haskell.org/ghc/dist/"
                          version "/" name "-" version "-src.tar.xz"))
      (sha256
       (base32 "1c8qc4fhkycynk4g1f9hvk53dj6a1vvqi6bklqznns6hw59m8qhi"))
      (patches
       (search-patches
        "ghc-dont-pass-linker-flags-via-response-files.patch"
        "ghc-8.0-fall-back-to-madv_dontneed.patch"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (outputs '("out" "doc"))
    (inputs
     `(("gmp" ,gmp)
       ("ncurses" ,ncurses)
       ("libffi" ,libffi)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32 "1wjc3x68l305bl1h1ijd3yhqp2vqj83lkp3kqbr94qmmkqlms8sj"))))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)                ; for tests
       ("ghostscript" ,ghostscript)        ; for tests
       ;; GHC is built with GHC.
       ("ghc-bootstrap" ,ghc-7)))
    (arguments
     `(#:test-target "test"
       ;; We get a smaller number of test failures by disabling parallel test
       ;; execution.
       #:parallel-tests? #f

       ;; The DSOs use $ORIGIN to refer to each other, but (guix build
       ;; gremlin) doesn't support it yet, so skip this phase.
       #:validate-runpath? #f

       ;; Don't pass --build=<triplet>, because the configure script
       ;; auto-detects slightly different triplets for --host and --target and
       ;; then complains that they don't match.
       #:build #f

       #:configure-flags
       (list
        (string-append "--with-gmp-libraries="
                       (assoc-ref %build-inputs "gmp") "/lib")
        (string-append "--with-gmp-includes="
                       (assoc-ref %build-inputs "gmp") "/include")
        "--with-system-libffi"
        (string-append "--with-ffi-libraries="
                       (assoc-ref %build-inputs "libffi") "/lib")
        (string-append "--with-ffi-includes="
                       (assoc-ref %build-inputs "libffi") "/include")
        (string-append "--with-curses-libraries="
                       (assoc-ref %build-inputs "ncurses") "/lib")
        (string-append "--with-curses-includes="
                       (assoc-ref %build-inputs "ncurses") "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-testsuite
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion ".."
               (copy-file (assoc-ref inputs "ghc-testsuite")
                          "ghc-testsuite.tar.xz")
               (zero? (system* "tar" "xvf" "ghc-testsuite.tar.xz")))))
         (add-before 'build 'fix-lib-paths
           (lambda _
             (substitute*
                 (list "libraries/process/System/Process/Posix.hs"
                       "libraries/process/tests/process001.hs"
                       "libraries/process/tests/process002.hs"
                       "libraries/unix/cbits/execvpe.c")
               (("/bin/sh") (which "sh"))
               (("/bin/ls") (which "ls")))
             #t))
         (add-before 'build 'fix-environment
           (lambda _
             (unsetenv "GHC_PACKAGE_PATH")
             (setenv "CONFIG_SHELL" (which "bash"))
             #t))
         (add-before 'check 'fix-testsuite
           (lambda _
             (substitute*
                 (list "testsuite/timeout/Makefile"
                       "testsuite/timeout/timeout.py"
                       "testsuite/timeout/timeout.hs"
                       "testsuite/tests/programs/life_space_leak/life.test")
               (("/bin/sh") (which "sh"))
               (("/bin/rm") "rm"))
             #t)))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-8
  (package (inherit ghc-8.0)
    (name "ghc")
    (version "8.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/" name "-" version "-src.tar.xz"))
       (sha256
        (base32 "1mk046vb561j75saz05rghhbkps46ym5aci4264dwc2qk3dayixf"))))
    (inputs
     `(("gmp" ,gmp)
       ("ncurses" ,ncurses)
       ("libffi" ,libffi)
       ("target-binutils" ,binutils)
       ("target-gcc" ,gcc)
       ("target-ld-wrapper" ,(make-ld-wrapper "ld-wrapper"
                                              #:binutils binutils))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python)               ; for tests
       ("ghostscript" ,ghostscript)     ; for tests
       ;; GHC 8.4.3 is built with GHC 8.
       ("ghc-bootstrap" ,ghc-8.0)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32
             "1z55b1z0m3plqd2d1ks6w5wvx7igm7zsk3i4v7cms003z0as0hzz"))))))
    (arguments
     `(#:test-target "test"
       ;; We get a smaller number of test failures by disabling parallel test
       ;; execution.
       #:parallel-tests? #f

       ;; The DSOs use $ORIGIN to refer to each other, but (guix build
       ;; gremlin) doesn't support it yet, so skip this phase.
       #:validate-runpath? #f

       ;; Don't pass --build=<triplet>, because the configure script
       ;; auto-detects slightly different triplets for --host and --target and
       ;; then complains that they don't match.
       #:build #f

       #:configure-flags
       (list
        (string-append "--with-gmp-libraries="
                       (assoc-ref %build-inputs "gmp") "/lib")
        (string-append "--with-gmp-includes="
                       (assoc-ref %build-inputs "gmp") "/include")
        "--with-system-libffi"
        (string-append "--with-ffi-libraries="
                       (assoc-ref %build-inputs "libffi") "/lib")
        (string-append "--with-ffi-includes="
                       (assoc-ref %build-inputs "libffi") "/include")
        (string-append "--with-curses-libraries="
                       (assoc-ref %build-inputs "ncurses") "/lib")
        (string-append "--with-curses-includes="
                       (assoc-ref %build-inputs "ncurses") "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-testsuite
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "xvf"
                     (assoc-ref inputs "ghc-testsuite")
                     "--strip-components=1")
             #t))
         (add-after 'unpack-testsuite 'fix-shell-wrappers
           (lambda _
             (substitute* '("driver/ghci/ghc.mk"
                            "utils/mkdirhier/ghc.mk"
                            "rules/shell-wrapper.mk")
               (("echo '#!/bin/sh'")
                (format #f "echo '#!~a'" (which "sh"))))
             #t))
         ;; This is necessary because the configure system no longer uses
         ;; “AC_PATH_” but “AC_CHECK_”, setting the variables to just the
         ;; plain command names.
         (add-before 'configure 'set-target-programs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((binutils (assoc-ref inputs "target-binutils"))
                   (gcc (assoc-ref inputs "target-gcc"))
                   (ld-wrapper (assoc-ref inputs "target-ld-wrapper")))
               (setenv "CC" (string-append gcc "/bin/gcc"))
               (setenv "CXX" (string-append gcc "/bin/g++"))
               (setenv "LD" (string-append ld-wrapper "/bin/ld"))
               (setenv "NM" (string-append binutils "/bin/nm"))
               (setenv "RANLIB" (string-append binutils "/bin/ranlib"))
               (setenv "STRIP" (string-append binutils "/bin/strip"))
               ;; The 'ar' command does not follow the same pattern.
               (setenv "fp_prog_ar" (string-append binutils "/bin/ar"))
               #t)))
         (add-before 'build 'fix-references
           (lambda _
             (substitute* '("testsuite/timeout/Makefile"
                            "testsuite/timeout/timeout.py"
                            "testsuite/timeout/timeout.hs"
                            "testsuite/tests/programs/life_space_leak/life.test"
                            ;; libraries
                            "libraries/process/System/Process/Posix.hs"
                            "libraries/process/tests/process001.hs"
                            "libraries/process/tests/process002.hs"
                            "libraries/unix/cbits/execvpe.c")
               (("/bin/sh") (which "sh"))
               (("/bin/ls") (which "ls"))
               (("/bin/rm") "rm"))
             #t))
         (add-before 'build 'fix-environment
           (lambda _
             (unsetenv "GHC_PACKAGE_PATH")
             (setenv "CONFIG_SHELL" (which "bash"))
             #t)))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))))

(define-public ghc ghc-8.0)

(define-public ghc-hostname
  (package
    (name "ghc-hostname")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/hostname/"
                           "hostname-" version ".tar.gz"))
       (sha256
        (base32
         "0p6gm4328946qxc295zb6vhwhf07l1fma82vd0siylnsnsqxlhwv"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/hostname")
    (synopsis "Hostname in Haskell")
    (description "Network.HostName is a simple package providing a means to
determine the hostname.")
    (license license:bsd-3)))

(define-public ghc-convertible
  (package
    (name "ghc-convertible")
    (version "1.1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/convertible/"
                           "convertible-" version ".tar.gz"))
       (sha256
        (base32
         "0v18ap1mccnndgxmbfgyjdicg8jlss01bd5fq8a576dr0h4sgyg9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-time" ,ghc-old-time)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-text" ,ghc-text)))
    (home-page "https://hackage.haskell.org/package/convertible")
    (synopsis "Typeclasses and instances for converting between types")
    (description
     "This package provides a typeclass with a single function that is
designed to help convert between different types: numeric values, dates and
times, and the like.  The conversions perform bounds checking and return a
pure @code{Either} value.  This means that you need not remember which specific
function performs the conversion you desire.")
    (license license:bsd-3)))

(define-public ghc-double-conversion
  (package
    (name "ghc-double-conversion")
    (version "2.0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "double-conversion/double-conversion-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0sx2kc1gw72mjvd8vph8bbjw5whfxfv92rsdhjg1c0al75rf3ka4"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/bos/double-conversion")
    (synopsis "Fast conversion between double precision floating point and text")
    (description
     "This package provides a library that performs fast, accurate conversion
between double precision floating point and text.")
    (license license:bsd-3)))

(define-public ghc-libxml
  (package
    (name "ghc-libxml")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/libxml/"
                           "libxml-" version ".tar.gz"))
       (sha256
        (base32
         "01zvk86kg726lf2vnlr7dxiz7g3xwi5a4ak9gcfbwyhynkzjmsfi"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("libxml2" ,libxml2)))
    (arguments
     `(#:configure-flags
       `(,(string-append "--extra-include-dirs="
                         (assoc-ref %build-inputs "libxml2")
                         "/include/libxml2"))))
    (home-page "https://hackage.haskell.org/package/libxml")
    (synopsis "Haskell bindings to libxml2")
    (description
     "This library provides minimal Haskell binding to libxml2.")
    (license license:bsd-3)))

(define-public ghc-prelude-extras
  (package
    (name "ghc-prelude-extras")
    (version "0.4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/prelude-extras/prelude-extras-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0xzqdf3nl2h0ra4gnslm1m1nsxlsgc0hh6ky3vn578vh11zhifq9"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ekmett/prelude-extras")
    (synopsis "Higher order versions of Prelude classes")
    (description "This library provides higher order versions of
@code{Prelude} classes to ease programming with polymorphic recursion and
reduce @code{UndecidableInstances}.")
    (license license:bsd-3)))

(define-public ghc-data-default
  (package
    (name "ghc-data-default")
    (version "0.7.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/data-default/data-default-"
             version
             ".tar.gz"))
       (sha256
        (base32 "04d5n8ybmcxba9qb6h389w9zfq1lvj81b82jh6maqp6pkhkmvydh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class"
        ,ghc-data-default-class)
       ("ghc-data-default-instances-base"
        ,ghc-data-default-instances-base)
       ("ghc-data-default-instances-containers"
        ,ghc-data-default-instances-containers)
       ("ghc-data-default-instances-dlist"
        ,ghc-data-default-instances-dlist)
       ("ghc-data-default-instances-old-locale"
        ,ghc-data-default-instances-old-locale)))
    (home-page "https://hackage.haskell.org/package/data-default")
    (synopsis "Types with default values")
    (description
     "This package defines a class for types with a default value, and
provides instances for types from the base, containers, dlist and old-locale
packages.")
    (license license:bsd-3)))

(define-public ghc-data-default-class
  (package
    (name "ghc-data-default-class")
    (version "0.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/data-default-class/"
             "data-default-class-" version ".tar.gz"))
       (sha256
        (base32 "0miyjz8d4jyvqf2vp60lyfbnflx6cj2k8apmm9ly1hq0y0iv80ag"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/data-default-class")
    (synopsis "Types with default values")
    (description
     "This package defines a class for types with default values.")
    (license license:bsd-3)))

(define-public ghc-data-default-instances-base
  (package
    (name "ghc-data-default-instances-base")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "data-default-instances-base/"
             "data-default-instances-base-" version ".tar.gz"))
       (sha256
        (base32 "0ym1sw3ssdzzifxxhh76qlv8kkmb2iclc158incv1dklyr9y8kw4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)))
    (home-page "https://hackage.haskell.org/package/data-default-instances-base")
    (synopsis "Default instances for types in base")
    (description
     "This package provides default instances for types from the base
package.")
    (license license:bsd-3)))

(define-public ghc-data-default-instances-containers
  (package
    (name "ghc-data-default-instances-containers")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "data-default-instances-containers/"
             "data-default-instances-containers-" version ".tar.gz"))
       (sha256
        (base32 "06h8xka031w752a7cjlzghvr8adqbl95xj9z5zc1b62w02phfpm5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)))
    (home-page "https://hackage.haskell.org/package/data-default-instances-containers")
    (synopsis "Default instances for types in containers")
    (description "Provides default instances for types from the containers
package.")
    (license license:bsd-3)))

(define-public ghc-data-default-instances-dlist
  (package
    (name "ghc-data-default-instances-dlist")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "data-default-instances-dlist/"
             "data-default-instances-dlist-" version ".tar.gz"))
       (sha256
        (base32 "0narkdqiprhgayjiawrr4390h4rq4pl2pb6mvixbv2phrc8kfs3x"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-dlist" ,ghc-dlist)))
    (home-page "https://hackage.haskell.org/package/data-default-instances-dlist")
    (synopsis "Default instances for types in dlist")
    (description "Provides default instances for types from the dlist
package.")
    (license license:bsd-3)))

(define-public ghc-code-page
  (package
  (name "ghc-code-page")
  (version "0.1.3")
  (source
   (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/code-page/code-page-"
           version ".tar.gz"))
      (sha256
       (base32
        "1491frk4jx6dlhifky9dvcxbsbcfssrz979a5hp5zn061rh8cp76"))))
  (build-system haskell-build-system)
  (home-page "https://github.com/RyanGlScott/code-page")
  (synopsis "Windows code page library for Haskell")
  (description "A cross-platform library with functions for adjusting
code pages on Windows.  On all other operating systems, the library does
nothing.")
  (license license:bsd-3)))

(define-public ghc-haddock-library
  (package
    (name "ghc-haddock-library")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haddock-library/haddock-library-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ns4bpf6whmcfl0cm2gx2c73if416x4q3ac4l4qm8w84h0zpcr7p"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis "Library exposing some functionality of Haddock")
    (description
     "Haddock is a documentation-generation tool for Haskell libraries.  These
modules expose some functionality of it without pulling in the GHC dependency.
Please note that the API is likely to change so specify upper bounds in your
project if you can't release often.  For interacting with Haddock itself, see
the ‘haddock’ package.")
    (license license:bsd-3)))

(define-public ghc-haddock-api
  (package
    (name "ghc-haddock-api")
    ;; This is the last version to be supported by Cabal < 2.0
    (version "2.17.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haddock-api/haddock-api-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "00fn6pzgg8xjbaw12d76jdqh2dbc5xy7miyz0x6kidvvar7i35ss"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-paths" ,ghc-paths)
       ("ghc-haddock-library" ,ghc-haddock-library)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis "API for documentation-generation tool Haddock")
    (description "This package provides an API to Haddock, the
documentation-generation tool for Haskell libraries.")
    (license license:bsd-3)))

(define-public ghc-haddock-test
  (package
    (name "ghc-haddock-test")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "haddock-test/haddock-test-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1ax8fnfrwx66csj952f3virxzapipan9da7z5l1zc12nqkifbs7w"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-xml" ,ghc-xml)
       ("ghc-syb" ,ghc-syb)))
    (home-page "http://www.haskell.org/haddock/")
    (synopsis "Test utilities for Haddock")
    (description "This package provides test utilities for Haddock.")
    (license license:bsd-3)))

(define-public ghc-haddock
  (package
    (name "ghc-haddock")
    (version "2.17.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haddock/haddock-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1z3h3v7w84dzsm47iavdppc2w899mr4c1agq9fzghgz902i0a655"))))
    (build-system haskell-build-system)
    ;; FIXME: Tests fail with this error:
    ;; driver-test/Main.hs:4:1: error:
    ;; Failed to load interface for ‘ResponseFileSpec’
    (arguments `(#:tests? #f))
    (inputs `(("ghc-haddock-api" ,ghc-haddock-api)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-haddock-test" ,ghc-haddock-test)))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis
     "Documentation-generation tool for Haskell libraries")
    (description
     "Haddock is a documentation-generation tool for Haskell libraries.")
    (license license:bsd-3)))

(define-public ghc-simple-reflect
  (package
    (name "ghc-simple-reflect")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/simple-reflect/simple-reflect-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1dpcf6w3cf1sfl9bnlsx04x7aghw029glj5d05qzrsnms2rlw8iq"))))
    (build-system haskell-build-system)
    (home-page
     "https://twanvl.nl/blog/haskell/simple-reflection-of-expressions")
    (synopsis
     "Simple reflection of expressions containing variables")
    (description
     "This package allows simple reflection of expressions containing
variables.  Reflection here means that a Haskell expression is turned into a
string.  The primary aim of this package is teaching and understanding; there
are no options for manipulating the reflected expressions beyond showing
them.")
    (license license:bsd-3)))

(define-public ghc-haskell-src
  (package
    (name "ghc-haskell-src")
    (version "1.0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haskell-src/haskell-src-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "19lilhpwnjb7cks9fq1ipnc8f7dwxy0ri3dgjkdxs3i355byw99a"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-happy" ,ghc-happy)
       ("ghc-syb" ,ghc-syb)))
    (home-page
     "https://hackage.haskell.org/package/haskell-src")
    (synopsis
     "Support for manipulating Haskell source code")
    (description
     "The 'haskell-src' package provides support for manipulating Haskell
source code.  The package provides a lexer, parser and pretty-printer, and a
definition of a Haskell abstract syntax tree (AST).  Common uses of this
package are to parse or generate Haskell 98 code.")
    (license license:bsd-3)))

(define-public ghc-alex
  (package
    (name "ghc-alex")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/alex/alex-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0bi1cs9b8ir33h1fl6x2xw4ymygapqbr713ridpf7rmk2wa7jqqs"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://www.haskell.org/alex/")
    (synopsis
     "Tool for generating lexical analysers in Haskell")
    (description
     "Alex is a tool for generating lexical analysers in Haskell.  It takes a
description of tokens based on regular expressions and generates a Haskell
module containing code for scanning text efficiently.  It is similar to the
tool lex or flex for C/C++.")
    (license license:bsd-3)))

(define-public ghc-cgi
  (package
    (name "ghc-cgi")
    (version "3001.3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cgi/cgi-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1hbpplss1m4rdpm4ibip6fpimlhssqa14fl338kl2jbc463i64cj"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck")))
    (inputs
     `(("ghc-parsec" ,ghc-parsec)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-multipart" ,ghc-multipart)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-network" ,ghc-network)
       ("ghc-mtl" ,ghc-mtl)))
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page
     "https://github.com/cheecheeo/haskell-cgi")
    (synopsis "Library for writing CGI programs")
    (description
     "This is a Haskell library for writing CGI programs.")
    (license license:bsd-3)))

(define-public ghc-cmdargs
  (package
    (name "ghc-cmdargs")
    (version "0.10.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cmdargs/cmdargs-"
             version ".tar.gz"))
       (sha256
        (base32
         "1lnmcsf6p9yrwwz1zvrw5lbc32xpff7b70yz4ylawaflnlz6wrlh"))))
    (build-system haskell-build-system)
    (home-page
     "http://community.haskell.org/~ndm/cmdargs/")
    (synopsis "Command line argument processing")
    (description
     "This library provides an easy way to define command line parsers.")
    (license license:bsd-3)))

(define-public ghc-concatenative
  (package
    (name "ghc-concatenative")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/concatenative/concatenative-"
                    version ".tar.gz"))
              (sha256
               (base32
                "05xwqvcdnk8bsyj698ab9jxpa1nk23pf3m7wi9mwmw0q8n99fngd"))))
    (build-system haskell-build-system)
    (home-page
     "https://patch-tag.com/r/salazar/concatenative/snapshot/current/content/pretty")
    (synopsis "Library for postfix control flow")
    (description
     "Concatenative gives Haskell Factor-style combinators and arrows for
postfix notation.  For more information on stack based languages, see
@uref{https://concatenative.org}.")
    (license license:bsd-3)))

(define-public ghc-happy
  (package
    (name "ghc-happy")
    (version "1.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/happy/happy-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "186ky3bly0i3cc56qk3r7j7pxh2108aackq4n2lli7jmbnb3kxsd"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)))
    (home-page "https://hackage.haskell.org/package/happy")
    (synopsis "Parser generator for Haskell")
    (description "Happy is a parser generator for Haskell.  Given a grammar
specification in BNF, Happy generates Haskell code to parse the grammar.
Happy works in a similar way to the yacc tool for C.")
    (license license:bsd-3)))

(define-public ghc-haskell-lexer
  (package
    (name "ghc-haskell-lexer")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haskell-lexer/haskell-lexer-"
             version ".tar.gz"))
       (sha256
        (base32
         "0rj3r1pk88hh3sk3mj61whp8czz5kpxhbc78xlr04bxwqjrjmm6p"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/haskell-lexer")
    (synopsis "Fully compliant Haskell 98 lexer")
    (description
     "This package provides a fully compliant Haskell 98 lexer.")
    (license license:bsd-3)))

(define-public ghc-pretty-show
  (package
    (name "ghc-pretty-show")
    (version "1.6.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/pretty-show/"
                           "pretty-show-" version ".tar.gz"))
       (sha256
        (base32
         "1fblcxw4z4ry14brin1mvwccs6hqqlhi7xhwv1f23szjq25cjacn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-haskell-lexer" ,ghc-haskell-lexer)
       ("ghc-happy" ,ghc-happy)))
    (home-page "http://wiki.github.com/yav/pretty-show")
    (synopsis "Tools for working with derived `Show` instances")
    (description
     "This package provides a library and an executable for working with
derived @code{Show} instances.  By using the library, derived @code{Show}
instances can be parsed into a generic data structure.  The @code{ppsh} tool
uses the library to produce human-readable versions of @code{Show} instances,
which can be quite handy for debugging Haskell programs.  We can also render
complex generic values into an interactive Html page, for easier
examination.")
    (license license:expat)))

(define-public ghc-pretty-show-for-haskell-src-exts
  (package (inherit ghc-pretty-show)
    (name "ghc-pretty-show")
    (version "1.6.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/pretty-show/"
                           "pretty-show-" version ".tar.gz"))
       (sha256
        (base32
         "133s4l1gk46saf6ycm785rswycy8g3j0qqrv93b5wp8cp50kd0ww"))))))

(define-public ghc-haskell-src-exts
  (package
    (name "ghc-haskell-src-exts")
    (version "1.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haskell-src-exts/haskell-src-exts-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1jsjl9hja2dpcfq4mzlfpwyr6axwnwgacfb7aa070kz4lbygzaa8"))))
    (build-system haskell-build-system)
    (inputs
     `(("cpphs" ,cpphs)
       ("ghc-happy" ,ghc-happy)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-pretty-show" ,ghc-pretty-show-for-haskell-src-exts)))
    (native-inputs
     `(("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)
       ("ghc-tasty-golden" ,ghc-tasty-golden)))
    (home-page "https://github.com/haskell-suite/haskell-src-exts")
    (synopsis "Library for manipulating Haskell source")
    (description "Haskell-Source with Extensions (HSE, haskell-src-exts) is an
extension of the standard @code{haskell-src} package, and handles most
registered syntactic extensions to Haskell.  All extensions implemented in GHC
are supported.  Apart from these standard extensions, it also handles regular
patterns as per the HaRP extension as well as HSX-style embedded XML syntax.")
    (license license:bsd-3)))

(define-public ghc-haskell-src-exts-util
  (package
    (name "ghc-haskell-src-exts-util")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "haskell-src-exts-util/haskell-src-exts-util-"
                           version ".tar.gz"))
       (sha256
        (base32
         "14rhwcrdz3kfb69c64qn8kybl7wnpajrjlfz5p95ca4bva4mwclg"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default" ,ghc-data-default)
       ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-uniplate" ,ghc-uniplate)))
    (home-page "https://github.com/pepeiborra/haskell-src-exts-util")
    (synopsis "Helper functions for working with haskell-src-exts trees")
    (description
     "This package provides helper functions for working with
@code{haskell-src-exts} trees.")
    (license license:bsd-3)))

(define-public ghc-refact
  (package
    (name "ghc-refact")
    (version "0.3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "refact/refact-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0v0zxcx29b8jxs2kgy9csykqcp8kzhdvyylw2xfwmj4pfxr2kl0a"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/refact")
    (synopsis "Specify refactorings to perform with apply-refact")
    (description
     "This library provides a datatype which can be interpreted by
@code{apply-refact}.  It exists as a separate library so that applications can
specify refactorings without depending on GHC.")
    (license license:bsd-3)))

(define-public hlint
  (package
    (name "hlint")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/" name
             "/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mmfavd158l5ig5wb1b1zbhbraj8hhqvbk4mg85n6c8ndpr4grd4"))))
    (build-system haskell-build-system)
    (inputs
     `(("cpphs" ,cpphs)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-vector" ,ghc-vector)
       ("ghc-text" ,ghc-text)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-haskell-src-exts-util" ,ghc-haskell-src-exts-util)
       ("ghc-uniplate" ,ghc-uniplate)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-extra" ,ghc-extra)
       ("ghc-refact" ,ghc-refact)
       ("ghc-aeson" ,ghc-aeson)
       ("hscolour" ,hscolour)))
    (home-page "http://community.haskell.org/~ndm/hlint/")
    (synopsis "Suggest improvements for Haskell source code")
    (description "HLint reads Haskell programs and suggests changes that
hopefully make them easier to read.  HLint also makes it easy to disable
unwanted suggestions, and to add your own custom suggestions.")
    (license license:bsd-3)))

(define-public ghc-resourcet
  (package
    (name "ghc-resourcet")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/resourcet/"
                           "resourcet-" version ".tar.gz"))
       (sha256
        (base32
         "0rzjzh34s36ssign7akqjnwnjxf11c3511wk7ky0xxy0dqmc2rg7"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-mmorph" ,ghc-mmorph)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-unliftio-core" ,ghc-unliftio-core)))
    (native-inputs
     `(("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/snoyberg/conduit")
    (synopsis "Deterministic allocation and freeing of scarce resources")
    (description "ResourceT is a monad transformer which creates a region of
code where you can safely allocate resources.")
    (license license:bsd-3)))

(define-public ghc-objectname
  (package
    (name "ghc-objectname")
    (version "1.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/ObjectName/ObjectName-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "046jm94rmm46cicd31pl54vdvfjvhd9ffbfycy2lxzc0fliyznvj"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/ObjectName")
    (synopsis "Helper library for Haskell OpenGL")
    (description "This tiny package contains the class ObjectName, which
corresponds to the general notion of explicitly handled identifiers for API
objects, e.g. a texture object name in OpenGL or a buffer object name in
OpenAL.")
    (license license:bsd-3)))

(define-public ghc-sdl
  (package
    (name "ghc-sdl")
    (version "0.6.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/SDL/SDL-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1sa3zx3vrs1gbinxx33zwq0x2bsf3i964bff7419p7vzidn36k46"))))
    (build-system haskell-build-system)
    (inputs
     `(("sdl" ,sdl)))
    (home-page "https://hackage.haskell.org/package/SDL")
    (synopsis "LibSDL for Haskell")
    (description "Simple DirectMedia Layer (libSDL) is a cross-platform
multimedia library designed to provide low level access to audio, keyboard,
mouse, joystick, 3D hardware via OpenGL, and 2D video framebuffer.  It is used
by MPEG playback software, emulators, and many popular games, including the
award winning Linux port of \"Civilization: Call To Power.\"")
    (license license:bsd-3)))

(define-public ghc-sdl-mixer
  (package
    (name "ghc-sdl-mixer")
    (version "0.6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/SDL-mixer/SDL-mixer-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1fhray79d80dk2aj9mx3ks05mm48sd832g8zgxli226jx471fs8r"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags
       (let* ((sdl-mixer (assoc-ref %build-inputs "sdl-mixer"))
              (sdl-mixer-include (string-append sdl-mixer "/include/SDL")))
         (list (string-append "--extra-include-dirs=" sdl-mixer-include)))))
    (inputs
     `(("ghc-sdl" ,ghc-sdl)
       ("sdl-mixer" ,sdl-mixer)))
    (home-page "https://hackage.haskell.org/package/SDL-mixer")
    (synopsis "Haskell bindings to libSDL_mixer")
    (description "SDL_mixer is a sample multi-channel audio mixer library.  It
supports any number of simultaneously playing channels of 16 bit stereo audio,
plus a single channel of music, mixed by the popular MikMod MOD, Timidity
MIDI, Ogg Vorbis, and SMPEG MP3 libraries.")
    (license license:bsd-3)))

(define-public ghc-sdl-image
  (package
    (name "ghc-sdl-image")
    (version "0.6.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/SDL-image/SDL-image-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ybdwlqi5nqzpsbh2md5mxhwmjn910iqysf6nykwjxlmvhcjk281"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags
       (let* ((sdl-image (assoc-ref %build-inputs "sdl-image"))
              (sdl-image-include (string-append sdl-image "/include/SDL")))
         (list (string-append "--extra-include-dirs=" sdl-image-include)))))
    (inputs
     `(("ghc-sdl" ,ghc-sdl)
       ("sdl-image" ,sdl-image)))
    (home-page "https://hackage.haskell.org/package/SDL-image")
    (synopsis "Haskell bindings to libSDL_image")
    (description "SDL_image is an image file loading library.  It loads images
as SDL surfaces, and supports the following formats: BMP, GIF, JPEG, LBM, PCX,
PNG, PNM, TGA, TIFF, XCF, XPM, XV.")
    (license license:bsd-3)))

(define-public ghc-half
  (package
    (name "ghc-half")
    (version "0.2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/half/half-"
             version ".tar.gz"))
       (sha256
        (base32
         "0p4sb7vv9cljv48wlx65wgdnkryrk5d6yfh7g4yrm20w1p449hl5"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ekmett/half")
    (synopsis "Half-precision floating-point computations")
    (description "This library provides a half-precision floating-point
computation library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-fixed
  (package
    (name "ghc-fixed")
    (version "0.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/fixed/fixed-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1qhmwx8iqshns0crmr9d2f8hm65jxbcp3dvv0c39v34ra7if3a94"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ekmett/fixed")
    (synopsis "Signed 15.16 precision fixed point arithmetic")
    (description
     "This package provides functions for signed 15.16 precision fixed point
arithmetic.")
    (license license:bsd-3)))

(define-public ghc-openglraw
  (package
    (name "ghc-openglraw")
    (version "3.2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/OpenGLRaw/OpenGLRaw-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "024aln102d1mmsdalq9jd5mmwjbnrb8gxcak73lybrc7q87kswk2"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-half" ,ghc-half)
       ("ghc-fixed" ,ghc-fixed)
       ("glu" ,glu)
       ("ghc-text" ,ghc-text)))
    (home-page "https://www.haskell.org/haskellwiki/Opengl")
    (synopsis "Raw Haskell bindings for the OpenGL graphics system")
    (description "OpenGLRaw is a raw Haskell binding for the OpenGL 4.5
graphics system and lots of OpenGL extensions.  It is basically a 1:1 mapping
of OpenGL's C API, intended as a basis for a nicer interface.  OpenGLRaw
offers access to all necessary functions, tokens and types plus a general
facility for loading extension entries.  The module hierarchy closely mirrors
the naming structure of the OpenGL extensions, making it easy to find the
right module to import.  All API entries are loaded dynamically, so no special
C header files are needed for building this package.  If an API entry is not
found at runtime, a userError is thrown.")
    (license license:bsd-3)))

(define-public ghc-glut
  (package
    (name "ghc-glut")
    (version "2.7.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/GLUT/GLUT-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0sbm943bg896nf3qjmlnsg3zzrr3pl330rzh9g0hwv47nzwwn4ab"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-statevar" ,ghc-statevar)
       ("ghc-opengl" ,ghc-opengl)
       ("ghc-openglraw" ,ghc-openglraw)
       ("freeglut" ,freeglut)))
    (home-page "https://www.haskell.org/haskellwiki/Opengl")
    (synopsis "Haskell bindings for the OpenGL Utility Toolkit")
    (description "This library provides Haskell bindings for the OpenGL
Utility Toolkit, a window system-independent toolkit for writing OpenGL
programs.")
    (license license:bsd-3)))

(define-public ghc-gluraw
  (package
    (name "ghc-gluraw")
    (version "2.0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/GLURaw/GLURaw-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1phmvxr0kkxq89ykmr8rj77inj8dvcxd6h72z1ncdgdazfz3sjw8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-openglraw" ,ghc-openglraw)))
    (home-page "https://www.haskell.org/haskellwiki/Opengl")
    (synopsis "Raw Haskell bindings GLU")
    (description "GLURaw is a raw Haskell binding for the GLU 1.3 OpenGL
utility library.  It is basically a 1:1 mapping of GLU's C API, intended as a
basis for a nicer interface.")
    (license license:bsd-3)))

(define-public ghc-opengl
  (package
    (name "ghc-opengl")
    (version "3.0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/OpenGL/OpenGL-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1nm2mzrn1h2nnfs9vl5p088hqpll7rdvcsyqsn8q641im9jhqp7l"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-objectname" ,ghc-objectname)
       ("ghc-gluraw" ,ghc-gluraw)
       ("ghc-statevar" ,ghc-statevar)
       ("ghc-openglraw" ,ghc-openglraw)))
    (home-page "https://www.haskell.org/haskellwiki/Opengl")
    (synopsis "Haskell bindings for the OpenGL graphics system")
    (description "This package provides Haskell bindings for the OpenGL
graphics system (GL, version 4.5) and its accompanying utility library (GLU,
version 1.3).")
    (license license:bsd-3)))

(define-public ghc-streaming-commons
  (package
    (name "ghc-streaming-commons")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "streaming-commons/streaming-commons-"
                           version ".tar.gz"))
       (sha256
        (base32
         "13fn6qmpiggwpn8lczyydgp77cyzfypwds7wxskrwir4i5cgxlfq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-network" ,ghc-network)
       ("ghc-random" ,ghc-random)
       ("ghc-stm" ,ghc-stm)
       ("ghc-text" ,ghc-text)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/streaming-commons")
    (synopsis "Conduit and pipes needed by some streaming data libraries")
    (description "This package provides low-dependency functionality commonly
needed by various Haskell streaming data libraries, such as @code{conduit} and
@code{pipe}s.")
    (license license:expat)))

(define-public cpphs
  (package
    (name "cpphs")
    (version "1.20.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/" name "/"
             name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bh524asqhk9v1s0wvipl0hgn7l63iy3js867yv0z3h5v2kn8vg5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-polyparse" ,ghc-polyparse)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-old-time" ,ghc-old-time)))
    (home-page "http://projects.haskell.org/cpphs/")
    (synopsis "Liberalised re-implementation of cpp, the C pre-processor")
    (description "Cpphs is a re-implementation of the C pre-processor that is
both more compatible with Haskell, and itself written in Haskell so that it
can be distributed with compilers.  This version of the C pre-processor is
pretty-much feature-complete and compatible with traditional (K&R)
pre-processors.  Additional features include: a plain-text mode; an option to
unlit literate code files; and an option to turn off macro-expansion.")
    (license (list license:lgpl2.1+ license:gpl3+))))

(define-public ghc-reflection
  (package
    (name "ghc-reflection")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/reflection/reflection-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0f9w0akbm6p8h7kzgcd2f6nnpw1wy84pqn45vfz1ch5j0hn8h2d9"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-tagged" ,ghc-tagged)))
    (home-page "https://github.com/ekmett/reflection")
    (synopsis "Reify arbitrary terms into types that can be reflected back
into terms")
    (description "This package addresses the 'configuration problem' which is
propogating configurations that are available at run-time, allowing multiple
configurations to coexist without resorting to mutable global variables or
@code{System.IO.Unsafe.unsafePerformIO}.")
    (license license:bsd-3)))

(define-public ghc-old-locale
  (package
    (name "ghc-old-locale")
    (version "1.0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/old-locale/old-locale-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0l3viphiszvz5wqzg7a45zp40grwlab941q5ay29iyw8p3v8pbyv"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=base")))
    (home-page "https://hackage.haskell.org/package/old-locale")
    (synopsis "Adapt to locale conventions")
    (description
     "This package provides the ability to adapt to locale conventions such as
date and time formats.")
    (license license:bsd-3)))

(define-public ghc-old-time
  (package
    (name "ghc-old-time")
    (version "1.1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/old-time/old-time-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1h9b26s3kfh2k0ih4383w90ibji6n0iwamxp6rfp2lbq1y5ibjqw"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=base")))
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)))
    (home-page "https://hackage.haskell.org/package/old-time")
    (synopsis "Time compatibility library for Haskell")
    (description "Old-time is a package for backwards compatibility with the
old @code{time} library.  For new projects, the newer
@uref{https://hackage.haskell.org/package/time, time library} is recommended.")
    (license license:bsd-3)))

(define-public ghc-data-default-instances-old-locale
  (package
    (name "ghc-data-default-instances-old-locale")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://hackage.haskell.org/package/"
              "data-default-instances-old-locale/"
              "data-default-instances-old-locale-" version ".tar.gz"))
        (sha256
          (base32 "00h81i5phib741yj517p8mbnc48myvfj8axzsw44k34m48lv1lv0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-old-locale" ,ghc-old-locale)))
    (home-page
      "https://hackage.haskell.org/package/data-default-instances-old-locale")
    (synopsis "Default instances for types in old-locale")
    (description "Provides Default instances for types from the old-locale
  package.")
    (license license:bsd-3)))

(define-public ghc-dlist
  (package
    (name "ghc-dlist")
    (version "0.8.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/dlist/dlist-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0yirrh0s6acjy9hhvf5fqg2d6q5y6gm9xs04v6w1imndh1xqdwdc"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/spl/dlist")
    (synopsis "Difference lists")
    (description
     "Difference lists are a list-like type supporting O(1) append.  This is
particularly useful for efficient logging and pretty printing (e.g. with the
Writer monad), where list append quickly becomes too expensive.")
    (license license:bsd-3)))

(define-public ghc-extensible-exceptions
  (package
    (name "ghc-extensible-exceptions")
    (version "0.1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "extensible-exceptions/extensible-exceptions-"
                           version ".tar.gz"))
       (sha256
        (base32 "1273nqws9ij1rp1bsq5jc7k2jxpqa0svawdbim05lf302y0firbc"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/extensible-exceptions")
    (synopsis "Extensible exceptions for Haskell")
    (description
     "This package provides extensible exceptions for both new and old
versions of GHC (i.e., < 6.10).")
    (license license:bsd-3)))

(define-public cabal-install
 (package
  (name "cabal-install")
   (version "1.22.6.0")
   (source
    (origin
     (method url-fetch)
      (uri (string-append
            "https://hackage.haskell.org/package/cabal-install/cabal-install-"
            version
            ".tar.gz"))
      (sha256
       (base32 "1d5h7h2wjwc2s3dvsvzjgmmfrfl2312ym2h6kyjgm9wnaqw9w8wx"))))
   (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
   (build-system haskell-build-system)
   (inputs
    `(("ghc-http" ,ghc-http)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-network-uri" ,ghc-network-uri)
      ("ghc-network" ,ghc-network)
      ("ghc-random" ,ghc-random)
      ("ghc-stm" ,ghc-stm)
      ("ghc-zlib" ,ghc-zlib)))
   (home-page "https://www.haskell.org/cabal/")
   (synopsis "Command-line interface for Cabal and Hackage")
   (description
    "The cabal command-line program simplifies the process of managing
Haskell software by automating the fetching, configuration, compilation and
installation of Haskell libraries and programs.")
   (license license:bsd-3)))

(define-public cabal-doctest
  (package
    (name "cabal-doctest")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cabal-doctest/cabal-doctest-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0bgd4jdmzxq5y465r4sf4jv2ix73yvblnr4c9wyazazafddamjny"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/phadej/cabal-doctest")
    (synopsis "Setup.hs helper for running doctests")
    (description
     "To properly work, the @code{doctest} package needs plenty of
configuration.  This library provides the common bits for writing custom
@file{Setup.hs} files.")
    (license license:bsd-3)))

(define-public ghc-mtl
  (package
    (name "ghc-mtl")
    (version "2.2.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/mtl/mtl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa"))))
    (build-system haskell-build-system)
    ;; This is an official revision, relaxing the requirement transformers ==
    ;; 0.4 to transformers >= 0.4 && < 0.6; see
    ;; https://hackage.haskell.org/package/mtl-2.2.1/revision/1.cabal
    (arguments
     `(#:configure-flags (list "--allow-newer=transformers")))
    (home-page "https://github.com/ekmett/mtl")
    (synopsis
     "Monad classes, using functional dependencies")
    (description "Monad classes using functional dependencies, with instances
for various monad transformers, inspired by the paper 'Functional Programming
with Overloading and Higher-Order Polymorphism', by Mark P Jones, in 'Advanced
School of Functional Programming', 1995.  See
@uref{https://web.cecs.pdx.edu/~mpj/pubs/springschool.html, the paper}.")
    (license license:bsd-3)))

(define-public ghc-parsec-numbers
  (package
    (name "ghc-parsec-numbers")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "parsec-numbers/parsec-numbers-" version ".tar.gz"))
       (sha256
        (base32 "1gzy4v3r02kvdxvgg1nj83mmb6aph2v4ilf9c7y6nbvi2x49l0bp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-parsec" ,ghc-parsec)))
    (home-page "https://hackage.haskell.org/package/parsec-numbers")
    (synopsis "Utilities for parsing numbers from strings")
    (description
     "This package provides the number parsers without the need to use a large
(and unportable) token parser.")
    (license license:bsd-3)))

(define-public ghc-paths
  (package
    (name "ghc-paths")
    (version "0.1.0.9")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/ghc-paths/ghc-paths-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ibrr1dxa35xx20cpp8jzgfak1rdmy344dfwq4vlq013c6w8z9mg"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/simonmar/ghc-paths")
    (synopsis
     "Knowledge of GHC's installation directories")
    (description
     "Knowledge of GHC's installation directories.")
    (license license:bsd-3)))

(define-public ghc-utf8-string
  (package
    (name "ghc-utf8-string")
    (version "1.0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/utf8-string/utf8-string-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0h7imvxkahiy8pzr8cpsimifdfvv18lizrb33k6mnq70rcx9w2zv"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=base")))
    (home-page "https://github.com/glguy/utf8-string/")
    (synopsis "Support for reading and writing UTF8 Strings")
    (description
     "A UTF8 layer for Strings.  The utf8-string package provides operations
for encoding UTF8 strings to Word8 lists and back, and for reading and writing
UTF8 without truncation.")
    (license license:bsd-3)))

(define-public ghc-setenv
  (package
    (name "ghc-setenv")
    (version "0.1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/setenv/setenv-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0cnbgrvb9byyahb37zlqrj05rj25v190crgcw8wmlgf0mwwxyn73"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/setenv")
    (synopsis "Library for setting environment variables")
    (description "This package provides a Haskell library for setting
environment variables.")
    (license license:expat)))

(define-public ghc-setlocale
  (package
    (name "ghc-setlocale")
    (version "1.0.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/setlocale-"
                    version "/setlocale-" version ".tar.gz"))
              (sha256
               (base32
                "1rl8qb8vzv8fdbczy2dxwgn4cb68lfrjdxf2w8nn9wy1acqzcyjq"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/setlocale")
    (synopsis "Haskell bindings to setlocale")
    (description "This package provides Haskell bindings to the
@code{setlocale} C function.")
    (license license:bsd-3)))

(define-public ghc-x11
  (package
    (name "ghc-x11")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/X11/"
                           "X11-" version ".tar.gz"))
       (sha256
        (base32 "13lxq36856fzp61y4api78vssykyh8fm2aplr0nsj18ymdm1c6sl"))))
    (build-system haskell-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("libxrandr" ,libxrandr)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("ghc-data-default" ,ghc-data-default)))
    (home-page "https://github.com/haskell-pkg-janitors/X11")
    (synopsis "Bindings to the X11 graphics library")
    (description
     "This package provides Haskell bindings to the X11 graphics library.  The
bindings are a direct translation of the C bindings.")
    (license license:bsd-3)))

(define-public ghc-x11-xft
  (package
    (name "ghc-x11-xft")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/X11-xft/"
                           "X11-xft-" version ".tar.gz"))
       (sha256
        (base32 "1lgqb0s2qfwwgbvwxhjbi23rbwamzdi0l0slfr20c3jpcbp3zfjf"))))
    (inputs
     `(("ghc-x11" ,ghc-x11)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("libx11" ,libx11)
       ("libxft" ,libxft)
       ("xorgproto" ,xorgproto)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/X11-xft")
    (synopsis "Bindings to Xft")
    (description
     "Bindings to the Xft, X Free Type interface library, and some Xrender
parts.")
    (license license:lgpl2.1)))

(define-public ghc-stringbuilder
  (package
    (name "ghc-stringbuilder")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/stringbuilder/stringbuilder-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ap95xphqnrhv64c2a137wqslkdmb2jjd9ldb17gs1pw48k8hrl9"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: circular dependencies with tests
                               ; enabled
    (home-page "https://hackage.haskell.org/package/stringbuilder")
    (synopsis "Writer monad for multi-line string literals")
    (description "This package provides a writer monad for multi-line string
literals.")
    (license license:expat)))

(define-public ghc-zlib
  (package
    (name "ghc-zlib")
    (version "0.6.2")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/zlib/zlib-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1vbzf0awb6zb456xf48za1kl22018646cfzq4frvxgb9ay97vk0d"))))
    (build-system haskell-build-system)
    (arguments `(#:configure-flags (list "--allow-newer=tasty")))
    (inputs `(("zlib" ,zlib)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://hackage.haskell.org/package/zlib")
    (synopsis
     "Compression and decompression in the gzip and zlib formats")
    (description
     "This package provides a pure interface for compressing and decompressing
streams of data represented as lazy 'ByteString's.  It uses the zlib C library
so it has high performance.  It supports the 'zlib', 'gzip' and 'raw'
compression formats.  It provides a convenient high level API suitable for
most tasks and for the few cases where more control is needed it provides
access to the full zlib feature set.")
    (license license:bsd-3)))

(define-public ghc-stm
  (package
    (name "ghc-stm")
    (version "2.4.5.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/stm/stm-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "19sr11a0hqikhvf561b38phz6k3zg9s157a0f5ffvghk7wcdpmri"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/stm")
    (synopsis "Software Transactional Memory")
    (description
     "Software Transactional Memory, or STM, is an abstraction for concurrent
communication.  The main benefits of STM are composability and modularity.
That is, using STM you can write concurrent abstractions that can be easily
composed with any other abstraction built using STM, without exposing the
details of how your abstraction ensures safety.  This is typically not the
case with other forms of concurrent communication, such as locks or
@code{MVar}s.")
    (license license:bsd-3)))

(define-public ghc-parallel
  (package
    (name "ghc-parallel")
    (version "3.2.1.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/parallel/parallel-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "09l52k1gsn667lmv0pp052cbwkzkbhfivp51f2xkrlfzf6xwvqsd"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/parallel")
    (synopsis "Parallel programming library")
    (description
     "This package provides a library for parallel programming.")
    (license license:bsd-3)))

(define-public ghc-safesemaphore
  (package
    (name "ghc-safesemaphore")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "SafeSemaphore/SafeSemaphore-" version ".tar.gz"))
       (sha256
        (base32
         "0rpg9j6fy70i0b9dkrip9d6wim0nac0snp7qzbhykjkqlcvvgr91"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stm" ,ghc-stm)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/ChrisKuklewicz/SafeSemaphore")
    (synopsis "Exception safe semaphores")
    (description "This library provides exception safe semaphores that can be
used in place of @code{QSem}, @code{QSemN}, and @code{SampleVar}, all of which
are not exception safe and can be broken by @code{killThread}.")
    (license license:bsd-3)))

(define-public ghc-text
  (package
    (name "ghc-text")
    (version "1.2.2.2")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/text/text-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1y9d0zjs2ls0c574mr5xw7y3y49s62sd3wcn9lhpwz8a6q352iii"))))
    (build-system haskell-build-system)
    ;; The test dependencies depend on ghc-text: ghc-test-framework -> ghc-xml -> ghc-text
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-random" ,ghc-random)))
    (home-page "https://github.com/bos/text")
    (synopsis "Efficient packed Unicode text type library")
    (description
     "An efficient packed, immutable Unicode text type (both strict and
lazy), with a powerful loop fusion optimization framework.

The 'Text' type represents Unicode character strings, in a time and
space-efficient manner.  This package provides text processing
capabilities that are optimized for performance critical use, both
in terms of large data quantities and high speed.")
    (license license:bsd-3)))

(define-public ghc-text-binary
  (package
    (name "ghc-text-binary")
    (version "0.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "text-binary/text-binary-"
                           version ".tar.gz"))
       (sha256
        (base32
         "18gl10pwg3qwsk0za3c70j4n6a9129wwf1b7d3a461h816yv55xn"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-text" ,ghc-text)))
    (home-page "https://github.com/kawu/text-binary")
    (synopsis "Binary instances for text types")
    (description
     "This package provides a compatibility layer providing @code{Binary}
instances for strict and lazy text types for versions older than 1.2.1 of the
text package.")
    (license license:bsd-2)))

(define-public ghc-strict
  (package
    (name "ghc-strict")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/strict/strict-"
                           version ".tar.gz"))
       (sha256
        (base32 "08cjajqz9h47fkq98mlf3rc8n5ghbmnmgn8pfsl3bdldjdkmmlrc"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/strict")
    (synopsis "Strict data types and String IO")
    (description
     "This package provides strict versions of some standard Haskell data
types, such as pairs, @code{Maybe} and @code{Either}.  It also contains strict
IO operations.")
    (license license:bsd-3)))

(define-public ghc-hashable
  (package
    (name "ghc-hashable")
    (version "1.2.6.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hashable/hashable-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ymv2mcrrgbdc2w39rib171fwnhg7fgp0sy4h8amrh1vw64qgjll"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-random" ,ghc-random)))
    (native-inputs
     `(("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/tibbe/hashable")
    (synopsis "Class for types that can be converted to a hash value")
    (description
     "This package defines a class, @code{Hashable}, for types that can be
converted to a hash value.  This class exists for the benefit of hashing-based
data structures.  The package provides instances for basic types and a way to
combine hash values.")
    (license license:bsd-3)))

(define-public ghc-hashable-time
  (package
    (name "ghc-hashable-time")
    (version "0.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hashable-time/hashable-time-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0k932nyd08l3xxbh2g3n76py2f4kd9yw4s5a065vjz0xp6wjnxdm"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-hashable" ,ghc-hashable)))
    (home-page "http://hackage.haskell.org/package/hashable-time")
    (synopsis "Hashable instances for Data.Time")
    (description
     "This package provides @code{Hashable} instances for types in
@code{Data.Time}.")
    (license license:bsd-3)))

(define-public ghc-data-hash
  (package
    (name "ghc-data-hash")
    (version "0.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/data-hash"
                           "/data-hash-" version ".tar.gz"))
       (sha256
        (base32 "1ghbqvc48gf9p8wiy71hdpaj7by3b9cw6wgwi3qqz8iw054xs5wi"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hackage.haskell.org/package/data-hash")
    (synopsis "Combinators for building fast hashing functions")
    (description
     "This package provides combinators for building fast hashing functions.
It includes hashing functions for all basic Haskell98 types.")
    (license license:bsd-3)))

(define-public ghc-murmur-hash
  (package
    (name "ghc-murmur-hash")
    (version "0.1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/murmur-hash"
                           "/murmur-hash-" version ".tar.gz"))
       (sha256
        (base32 "1bb58kfnzvx3mpc0rc0dhqc1fk36nm8prd6gvf20gk6lxaadpfc9"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/nominolo/murmur-hash")
    (synopsis "MurmurHash2 implementation for Haskell")
    (description
     "This package provides an implementation of MurmurHash2, a good, fast,
general-purpose, non-cryptographic hashing function.  See
@url{https://sites.google.com/site/murmurhash/} for details.  This
implementation is pure Haskell, so it might be a bit slower than a C FFI
binding.")
    (license license:bsd-3)))

(define-public ghc-random
  (package
    (name "ghc-random")
    (version "1.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/random/random-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0nis3lbkp8vfx8pkr6v7b7kr5m334bzb0fk9vxqklnp2aw8a865p"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/random")
    (synopsis "Random number library")
    (description "This package provides a basic random number generation
library, including the ability to split random number generators.")
    (license license:bsd-3)))

(define-public ghc-primitive
  (package
    (name "ghc-primitive")
    (version "0.6.3.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/primitive/primitive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0mcmbnj08wd6zfwn7xk6zf5hy5zwbla5v78pw0dpymqg9s0gzpnd"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/haskell/primitive")
    (synopsis "Primitive memory-related operations")
    (description
     "This package provides various primitive memory-related operations.")
    (license license:bsd-3)))

(define-public ghc-tf-random
  (package
    (name "ghc-tf-random")
    (version "0.5")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tf-random/tf-random-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0445r2nns6009fmq0xbfpyv7jpzwv0snccjdg7hwj4xk4z0cwc1f"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-primitive" ,ghc-primitive)
       ("ghc-random" ,ghc-random)))
    (home-page "https://hackage.haskell.org/package/tf-random")
    (synopsis "High-quality splittable pseudorandom number generator")
    (description "This package contains an implementation of a high-quality
splittable pseudorandom number generator.  The generator is based on a
cryptographic hash function built on top of the ThreeFish block cipher.  See
the paper \"Splittable Pseudorandom Number Generators Using Cryptographic
Hashing\" by Claessen, Pałka for details and the rationale of the design.")
    (license license:bsd-3)))

(define-public ghc-transformers-base
  (package
    (name "ghc-transformers-base")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/transformers-base/transformers-base-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "11r3slgpgpra6zi2kjg3g60gvv17b1fh6qxipcpk8n86qx7lk8va"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stm" ,ghc-stm)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page
     "https://hackage.haskell.org/package/transformers-compat")
    (synopsis
     "Backported transformer library")
    (description
     "Backported versions of types that were added to transformers in
transformers 0.3 and 0.4 for users who need strict transformers 0.2 or 0.3
compatibility to run on old versions of the platform.")
    (license license:bsd-3)))

(define-public ghc-transformers-compat
  (package
    (name "ghc-transformers-compat")
    (version "0.5.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/transformers-compat"
             "/transformers-compat-" version ".tar.gz"))
       (sha256
        (base32
         "17yam0199fh9ndsn9n69jx9nvbsmymzzwbi23dck3dk4q57fz0fq"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ekmett/transformers-compat/")
    (synopsis "Small compatibility shim between transformers 0.3 and 0.4")
    (description "This package includes backported versions of types that were
added to transformers in transformers 0.3 and 0.4 for users who need strict
transformers 0.2 or 0.3 compatibility to run on old versions of the platform,
but also need those types.")
    (license license:bsd-3)))

(define-public ghc-unix-time
  (package
    (name "ghc-unix-time")
    (version "0.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unix-time/unix-time-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1qdlc9raih8s0m3x8x3n7q3ngh4faw2alv9l78sp6gnx648k0c8i"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f)) ; FIXME: Test fails with "System.Time not found".  This
                     ; is weird, that should be provided by GHC 7.10.2.
    (inputs
     `(("ghc-old-time" ,ghc-old-time)
       ("ghc-old-locale" ,ghc-old-locale)))
    (home-page "https://hackage.haskell.org/package/unix-time")
    (synopsis "Unix time parser/formatter and utilities")
    (description "This library provides fast parsing and formatting utilities
for Unix time in Haskell.")
    (license license:bsd-3)))

(define-public ghc-unix-compat
  (package
    (name "ghc-unix-compat")
    (version "0.4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unix-compat/unix-compat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "036nv05w0yjxc3rfpar60ddjrlzc40mdgr5k6ihvwlvqfmq1gw9m"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/jystic/unix-compat")
    (synopsis "Portable POSIX-compatibility layer")
    (description
     "This package provides portable implementations of parts of the unix
package.  This package re-exports the unix package when available.  When it
isn't available, portable implementations are used.")
    (license license:bsd-3)))

(define-public ghc-indents
  (package
    (name "ghc-indents")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/indents/indents-"
                    version ".tar.gz"))
              (sha256
               (base32
                "16lz21bp9j14xilnq8yym22p3saxvc9fsgfcf5awn2a6i6n527xn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-parsec" ,ghc-parsec)
       ("ghc-concatenative" ,ghc-concatenative)
       ("ghc-mtl" ,ghc-mtl)))
    (home-page "http://patch-tag.com/r/salazar/indents")
    (synopsis "Indentation sensitive parser-combinators for parsec")
    (description
     "This library provides functions for use in parsing indentation sensitive
contexts.  It parses blocks of lines all indented to the same level as well as
lines continued at an indented level below.")
    (license license:bsd-3)))

(define-public ghc-iproute
  (package
    (name "ghc-iproute")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/iproute/iproute-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1viyxq3m1aifl05w0hxwrhhhcfpmvwz4ymil2gngi4nfm0yd1f2p"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: Tests cannot find System.ByteOrder,
                               ; exported by ghc-byteorder.  Doctest issue.
    (inputs
     `(("ghc-appar" ,ghc-appar)
       ("ghc-byteorder" ,ghc-byteorder)
       ("ghc-network" ,ghc-network)
       ("ghc-safe" ,ghc-safe)))
    (home-page "https://www.mew.org/~kazu/proj/iproute/")
    (synopsis "IP routing table")
    (description "IP Routing Table is a tree of IP ranges to search one of
them on the longest match base.  It is a kind of TRIE with one way branching
removed.  Both IPv4 and IPv6 are supported.")
    (license license:bsd-3)))

(define-public ghc-iwlib
  (package
    (name "ghc-iwlib")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/iwlib/iwlib-"
                           version ".tar.gz"))
       (sha256
        (base32 "0khmfwql4vwj55idsxmhjhrbqzfir3g9wm5lmpvnf77mm95cfpdz"))))
    (build-system haskell-build-system)
    (inputs
     `(("wireless-tools" ,wireless-tools)))
    (home-page "https://github.com/jaor/iwlib")
    (synopsis "Haskell binding to the iw wireless networking library")
    (description
     "IWlib is a thin Haskell binding to the iw C library.  It provides
information about the current wireless network connections, and adapters on
supported systems.")
    (license license:bsd-3)))

(define-public ghc-regex-base
  (package
    (name "ghc-regex-base")
    (version "0.93.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-base/regex-base-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0y1j4h2pg12c853nzmczs263di7xkkmlnsq5dlp5wgbgl49mgp10"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)))
    (home-page
     "https://sourceforge.net/projects/lazy-regex")
    (synopsis "Replaces/Enhances Text.Regex")
    (description "@code{Text.Regex.Base} provides the interface API for
regex-posix, regex-pcre, regex-parsec, regex-tdfa, regex-dfa.")
    (license license:bsd-3)))

(define-public ghc-regex-posix
  (package
    (name "ghc-regex-posix")
    (version "0.95.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-posix/regex-posix-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0gkhzhj8nvfn1ija31c7xnl6p0gadwii9ihyp219ck2arlhrj0an"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)))
    (home-page "https://sourceforge.net/projects/lazy-regex")
    (synopsis "POSIX regular expressions for Haskell")
    (description "This library provides the POSIX regex backend used by the
Haskell library @code{regex-base}.")
    (license license:bsd-3)))

(define-public ghc-regex-compat
  (package
    (name "ghc-regex-compat")
    (version "0.95.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-compat/regex-compat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0fwmima3f04p9y4h3c23493n1xj629ia2dxaisqm6rynljjv2z6m"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)
       ("ghc-regex-posix" ,ghc-regex-posix)))
    (home-page "https://sourceforge.net/projects/lazy-regex")
    (synopsis "Replaces/Enhances Text.Regex")
    (description "This library provides one module layer over
@code{regex-posix} to replace @code{Text.Regex}.")
    (license license:bsd-3)))

(define-public ghc-regex-tdfa-rc
  (package
    (name "ghc-regex-tdfa-rc")
    (version "1.1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-tdfa-rc/regex-tdfa-rc-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1vi11i23gkkjg6193ak90g55akj69bhahy542frkwb68haky4pp3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-mtl" ,ghc-mtl)))
    (home-page
     "https://hackage.haskell.org/package/regex-tdfa")
    (synopsis "Tagged DFA regex engine for Haskell")
    (description "A new all-Haskell \"tagged\" DFA regex engine, inspired by
@code{libtre} (fork by Roman Cheplyaka).")
    (license license:bsd-3)))

(define-public ghc-regex-tdfa-text
  (package
    (name "ghc-regex-tdfa-text")
    (version "1.0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-tdfa-text/"
             "regex-tdfa-text-" version ".tar.gz"))
       (sha256
        (base32
         "0090g6lgbdm9lywpqm2d3724nnnh24nx3vnlqr96qc2w486pmmrq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-regex-base" ,ghc-regex-base)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)))
    (home-page
     "http://hackage.haskell.org/package/regex-tdfa-text")
    (synopsis "Text interface for regex-tdfa")
    (description
     "This provides an extra text interface for regex-tdfa.")
    (license license:bsd-3)))

(define-public ghc-regex
  (package
    (name "ghc-regex")
    (version "1.0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/regex/"
                           "regex-" version ".tar.gz"))
       (sha256
        (base32
         "1sjkpkgv4phy5b5v2lr89x4vx4dh44pj0sbvlsp6n86w9v6v4jwb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-regex-base" ,ghc-regex-base)
       ("ghc-regex-pcre-builtin" ,ghc-regex-pcre-builtin)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-regex-tdfa-text" ,ghc-regex-tdfa-text)
       ("ghc-text" ,ghc-text)
       ("ghc-time-locale-compat" ,ghc-time-locale-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (home-page "http://regex.uk")
    (synopsis "Toolkit for regex-base")
    (description
     "This package provides a regular expression toolkit for @code{regex-base}
with compile-time checking of regular expression syntax, data types for
matches and captures, a text replacement toolkit, portable options, high-level
AWK-like tools for building text processing apps, regular expression macros
with parsers and test bench, comprehensive documentation, tutorials and
copious examples.")
    (license license:bsd-3)))

(define-public ghc-parsers
  (package
    (name "ghc-parsers")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/parsers/parsers-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "07najh7f9y3ahh42z96sw4hnd0kc4x3wm0xlf739y0gh81ys5097"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: Test fails with "cannot satisfy
                               ; -package attoparsec-0.13.0.1"
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-charset" ,ghc-charset)
       ("ghc-text" ,ghc-text)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (home-page "https://github.com/ekmett/parsers/")
    (synopsis "Parsing combinators")
    (description "This library provides convenient combinators for working
with and building parsing combinator libraries.  Given a few simple instances,
you get access to a large number of canned definitions.  Instances exist for
the parsers provided by @code{parsec}, @code{attoparsec} and @code{base}'s
@code{Text.Read}.")
    (license license:bsd-3)))

(define-public ghc-trifecta
  (package
    (name "ghc-trifecta")
    (version "1.7.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/trifecta/"
                    "trifecta-" version ".tar.gz"))
              (sha256
               (base32
                "13n6a3fdxngnzsjnhfrzigv1c2g0xm6lqkjcnirpc37sd0rpby31"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-doctest" ,ghc-doctest-0.13)
       ("ghc-reducers" ,ghc-reducers)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-charset" ,ghc-charset)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-fingertree" ,ghc-fingertree)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-lens" ,ghc-lens)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-parsers" ,ghc-parsers)
       ("ghc-profunctors" ,ghc-profunctors)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)))
    (home-page "https://github.com/ekmett/trifecta/")
    (synopsis "Parser combinator library with convenient diagnostics")
    (description "Trifecta is a modern parser combinator library for Haskell,
with slicing and Clang-style colored diagnostics.")
    (license license:bsd-3)))

(define-public ghc-time-locale-compat
  (package
    (name "ghc-time-locale-compat")
    (version "0.1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "time-locale-compat/time-locale-compat-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1vdcfr2hp9qh3ag90x6ikbdf42wiqpdylnplffna54bpnilbyi4i"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-old-locale" ,ghc-old-locale)))
    (home-page "https://github.com/khibino/haskell-time-locale-compat")
    (synopsis "Compatibility of TimeLocale between old-locale and time-1.5")
    (description "This package contains a wrapped name module for
@code{TimeLocale}.")
    (license license:bsd-3)))

(define-public ghc-attoparsec
  (package
    (name "ghc-attoparsec")
    (version "0.13.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/attoparsec/attoparsec-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0j6qcwd146yzlkc9mcvzvnixsyl65n2a68l28322q5v9p4g4g4yx"))))
    (build-system haskell-build-system)
    ;; FIXME: at least on test fails with QuickCheck > 2.9.2.  Once upstream
    ;; has updated the tests to work with a later version of QuickCheck we can
    ;; re-enable them.
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-scientific" ,ghc-scientific)
       ("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-unicode" ,ghc-quickcheck-unicode)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/bos/attoparsec")
    (synopsis "Fast combinator parsing for bytestrings and text")
    (description "This library provides a fast parser combinator library,
aimed particularly at dealing efficiently with network protocols and
complicated text/binary file formats.")
    (license license:bsd-3)))

(define-public ghc-zip-archive
  (package
    (name "ghc-zip-archive")
    (version "0.3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/zip-archive/zip-archive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1iwpzjck4jg9bz1yqky051i2wljsqc14q5zbi10dydfp8ip3d0yw"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-time" ,ghc-old-time)
       ("ghc-digest" ,ghc-digest)
       ("zip" ,zip)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-text" ,ghc-text)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://hackage.haskell.org/package/zip-archive")
    (synopsis "Zip archive library for Haskell")
    (description "The zip-archive library provides functions for creating,
modifying, and extracting files from zip archives in Haskell.")
    (license license:bsd-3)))

(define-public ghc-distributive
  (package
    (name "ghc-distributive")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/distributive/distributive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0y566r97sfyvhsmd4yxiz4ns2mqgwf5bdbp56wgxl6wlkidq0wwi"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-tagged" ,ghc-tagged)
       ("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-generic-deriving" ,ghc-generic-deriving)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-doctest" ,ghc-doctest-0.12)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/ekmett/distributive/")
    (synopsis "Distributive functors for Haskell")
    (description "This package provides distributive functors for Haskell.
Dual to @code{Traversable}.")
    (license license:bsd-3)))

(define-public ghc-cereal
  (package
    (name "ghc-cereal")
    (version "0.5.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cereal/cereal-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1x4aib7nnaf4x2b9zlndq5n5zsqniw78jifkf55nhnbmvryf05n3"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-fail" ,ghc-fail)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hackage.haskell.org/package/cereal")
    (synopsis "Binary serialization library")
    (description "This package provides a binary serialization library,
similar to @code{binary}, that introduces an @code{isolate} primitive for
parser isolation, and labeled blocks for better error messages.")
    (license license:bsd-3)))

(define-public ghc-comonad
  (package
    (name "ghc-comonad")
    (version "5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/comonad/comonad-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0anb36m9z52y9xsipxzj9w1b41b2rj8r389cxd6d20dplnfv3rbq"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)))
    (inputs
     `(("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/ekmett/comonad/")
    (synopsis "Comonads for Haskell")
    (description "This library provides @code{Comonad}s for Haskell.")
    (license license:bsd-3)))

(define-public hscolour
  (package
    (name "hscolour")
    (version "1.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hscolour/hscolour-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1j3rpzjygh3igvnd1n2xn63bq68rs047cjxr2qi6xyfnivgf6vz4"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/hscolour")
    (synopsis "Script to colourise Haskell code")
    (description "HSColour is a small Haskell script to colourise Haskell
code.  It currently has six output formats: ANSI terminal codes (optionally
XTerm-256colour codes), HTML 3.2 with font tags, HTML 4.01 with CSS, HTML 4.01
with CSS and mouseover annotations, XHTML 1.0 with inline CSS styling, LaTeX,
and mIRC chat codes.")
    (license license:bsd-3)))

(define-public ghc-polyparse
  (package
    (name "ghc-polyparse")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/polyparse/polyparse-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05dya1vdvq29hkhkdlsglzhw7bdn51rvs1javs0q75nf99c66k7m"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)))
    (home-page
     "http://code.haskell.org/~malcolm/polyparse/")
    (synopsis
     "Alternative parser combinator libraries")
    (description
     "This package provides a variety of alternative parser combinator
libraries, including the original HuttonMeijer set.  The Poly sets have
features like good error reporting, arbitrary token type, running state, lazy
parsing, and so on.  Finally, Text.Parse is a proposed replacement for the
standard Read class, for better deserialisation of Haskell values from
Strings.")
    (license license:lgpl2.1)))

(define-public ghc-extra
  (package
    (name "ghc-extra")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/extra/extra-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "06ds0jlx6sljwdf63l154qbzia9mnsri79i9qm3xikky3nj9ia1m"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-clock" ,ghc-clock)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/ndmitchell/extra")
    (synopsis "Extra Haskell functions")
    (description "This library provides extra functions for the standard
Haskell libraries.  Most functions are simple additions, filling out missing
functionality.  A few functions are available in later versions of GHC, but
this package makes them available back to GHC 7.2.")
    (license license:bsd-3)))

(define-public ghc-profunctors
  (package
    (name "ghc-profunctors")
    (version "5.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/profunctors/profunctors-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0s1pwjidbn761xk43pmzyvn99hm3psdifjd78ylki7f97aiyd0g9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-tagged" ,ghc-tagged)))
    (home-page "https://github.com/ekmett/profunctors/")
    (synopsis "Profunctors for Haskell")
    (description "This library provides profunctors for Haskell.")
    (license license:bsd-3)))

(define-public ghc-reducers
  (package
    (name "ghc-reducers")
    (version "3.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/reducers/reducers-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1gbaa5x4zbvnbklcb0d4q4m8hk6w0gz4s0c4m288czi1nw43dl65"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-fingertree" ,ghc-fingertree)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-text" ,ghc-text)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page "https://github.com/ekmett/reducers/")
    (synopsis "Semigroups, specialized containers and a general map/reduce framework")
    (description "This library provides various semigroups, specialized
containers and a general map/reduce framework for Haskell.")
    (license license:bsd-3)))

(define-public ghc-appar
  (package
    (name "ghc-appar")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/appar/appar-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "09jb9ij78fdkz2qk66rw99q19qnm504dpv0yq0pjsl6xwjmndsjq"))))
    (build-system haskell-build-system)
    (home-page
     "https://hackage.haskell.org/package/appar")
    (synopsis "Simple applicative parser")
    (description "This package provides a simple applicative parser in Parsec
style.")
    (license license:bsd-3)))

(define-public ghc-safe
  (package
    (name "ghc-safe")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/safe/safe-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1jdnp5zhvalf1xy8i872n29nljfjz6lnl9ghj80ffisrnnkrwcfh"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ndmitchell/safe#readme")
    (synopsis "Library of safe (exception free) functions")
    (description "This library provides wrappers around @code{Prelude} and
@code{Data.List} functions, such as @code{head} and @code{!!}, that can throw
exceptions.")
    (license license:bsd-3)))

(define-public ghc-generic-deriving
  (package
    (name "ghc-generic-deriving")
    (version "1.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/generic-deriving/generic-deriving-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1sdh5hpcwvh3b6zvgfk3pr3hla8p88l82njnih880c0gk5zl53dk"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/generic-deriving")
    (synopsis "Generalise the deriving mechanism to arbitrary classes")
    (description "This package provides functionality for generalising the
deriving mechanism in Haskell to arbitrary classes.")
    (license license:bsd-3)))

(define-public ghc-pcre-light
  (package
    (name "ghc-pcre-light")
    (version "0.4.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/pcre-light/pcre-light-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0xcyi1fivwg7a92mch5bcqzmrfxzqj42rmb3m8kgs61x4qwpxj82"))))
    (build-system haskell-build-system)
    (inputs
     `(("pcre" ,pcre)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/Daniel-Diaz/pcre-light")
    (synopsis "Haskell library for Perl 5 compatible regular expressions")
    (description "This package provides a small, efficient, and portable regex
library for Perl 5 compatible regular expressions.  The PCRE library is a set
of functions that implement regular expression pattern matching using the same
syntax and semantics as Perl 5.")
    (license license:bsd-3)))

(define-public ghc-logict
  (package
    (name "ghc-logict")
    (version "0.6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/logict/logict-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "07hnirv6snnym2r7iijlfz00b60jpy2856zvqxh989q0in7bd0hi"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-mtl" ,ghc-mtl)))
    (home-page "http://code.haskell.org/~dolio/")
    (synopsis "Backtracking logic-programming monad")
    (description "This library provides a continuation-based, backtracking,
logic programming monad.  An adaptation of the two-continuation implementation
found in the paper \"Backtracking, Interleaving, and Terminating Monad
Transformers\" available @uref{http://okmij.org/ftp/papers/LogicT.pdf,
online}.")
    (license license:bsd-3)))

(define-public ghc-xml
  (package
    (name "ghc-xml")
    (version "1.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/xml/xml-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0g814lj7vaxvib2g3r734221k80k7ap9czv9hinifn8syals3l9j"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)))
    (home-page "http://code.galois.com")
    (synopsis "Simple XML library for Haskell")
    (description "This package provides a simple XML library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-feed
  (package
    (name "ghc-feed")
    (version "0.3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "feed/feed-" version ".tar.gz"))
       (sha256
        (base32
         "0hkrsinspg70bbm3hwqdrvivws6zya1hyk0a3awpaz82j4xnlbfc"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-time-locale-compat" ,ghc-time-locale-compat)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-xml" ,ghc-xml)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/bergmark/feed")
    (synopsis "Haskell package for handling various syndication formats")
    (description "This Haskell package includes tools for generating and
consuming feeds in both RSS (Really Simple Syndication) and Atom format.")
    (license license:bsd-3)))

(define-public ghc-exceptions
  (package
    (name "ghc-exceptions")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/exceptions/exceptions-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1gl7xzffsqmigam6zg0jsglncgzxqafld2p6kb7ccp9xirzdjsjd"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck")))
    (native-inputs
     `(("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (inputs
     `(("ghc-stm" ,ghc-stm)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/ekmett/exceptions/")
    (synopsis "Extensible optionally-pure exceptions")
    (description "This library provides extensible optionally-pure exceptions
for Haskell.")
    (license license:bsd-3)))

(define-public ghc-temporary
  (package
    (name "ghc-temporary")
    (version "1.2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/temporary/temporary-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0qk741yqnpd69sksgks2vb7zi50rglp9m498lzw4sh268a017rsi"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-exceptions" ,ghc-exceptions)))
    (home-page "https://www.github.com/batterseapower/temporary")
    (synopsis "Temporary file and directory support")
    (description "The functions for creating temporary files and directories
in the Haskelll base library are quite limited.  This library just repackages
the Cabal implementations of its own temporary file and folder functions so
that you can use them without linking against Cabal or depending on it being
installed.")
    (license license:bsd-3)))

(define-public ghc-temporary-rc
  (package
    (name "ghc-temporary-rc")
    (version "1.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/temporary-rc/temporary-rc-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1nqih0qks439k3pr5kmbbc8rjdw730slrxlflqb27fbxbzb8skqs"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-exceptions" ,ghc-exceptions)))
    (home-page
     "https://www.github.com/feuerbach/temporary")
    (synopsis
     "Portable temporary file and directory support")
    (description
     "The functions for creating temporary files and directories in the base
library are quite limited.  The unixutils package contains some good ones, but
they aren't portable to Windows.  This library just repackages the Cabal
implementations of its own temporary file and folder functions so that you can
use them without linking against Cabal or depending on it being installed.
This is a better maintained fork of the \"temporary\" package.")
    (license license:bsd-3)))

(define-public ghc-smallcheck
  (package
    (name "ghc-smallcheck")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/smallcheck/smallcheck-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "195fj7w3v03d1y1nm2ylavzrwxjcdbq0lb6zsw1dwyx5jmwfc84h"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-logict" ,ghc-logict)
       ("ghc-mtl" ,ghc-mtl)))
    (home-page
     "https://github.com/feuerbach/smallcheck")
    (synopsis "Property-based testing library")
    (description "SmallCheck is a testing library that allows to verify
properties for all test cases up to some depth.  The test cases are generated
automatically by SmallCheck.")
    (license license:bsd-3)))

(define-public ghc-silently
  (package
    (name "ghc-silently")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/silently/silently-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0f9qm3f7y0hpxn6mddhhg51mm1r134qkvd2kr8r6192ka1ijbxnf"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ;; circular dependency with nanospec
    ;; (inputs
    ;;  `(("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/hspec/silently")
    (synopsis "Prevent writing to stdout")
    (description "This package provides functions to prevent or capture
writing to stdout and other handles.")
    (license license:bsd-3)))

(define-public ghc-case-insensitive
  (package
    (name "ghc-case-insensitive")
    (version "1.2.0.7")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/case-insensitive/case-insensitive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1j6ahvrz1g5q89y2difyk838yhwjc8z67zr0v2z512qdznc3h38n"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)))
    ;; these inputs are necessary to use this library
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-hashable" ,ghc-hashable)))
    (arguments
     `(#:tests? #f)) ; FIXME: currently missing libraries used for tests.
    (home-page
     "https://github.com/basvandijk/case-insensitive")
    (synopsis "Case insensitive string comparison")
    (description
     "The module 'Data.CaseInsensitive' provides the 'CI' type constructor
which can be parameterised by a string-like type like: 'String', 'ByteString',
'Text', etc..  Comparisons of values of the resulting type will be insensitive
to cases.")
    (license license:bsd-3)))

(define-public ghc-syb
  (package
    (name "ghc-syb")
    (version "0.6")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/syb/syb-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1p3cnqjm13677r4a966zffzhi9b3a321aln8zs8ckqj0d9z1z3d3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-mtl" ,ghc-mtl)))
    (home-page
     "http://www.cs.uu.nl/wiki/GenericProgramming/SYB")
    (synopsis "Scrap Your Boilerplate")
    (description "This package contains the generics system described in the
/Scrap Your Boilerplate/ papers (see
@uref{http://www.cs.uu.nl/wiki/GenericProgramming/SYB, the website}).  It
defines the 'Data' class of types permitting folding and unfolding of
constructor applications, instances of this class for primitive types, and a
variety of traversals.")
    (license license:bsd-3)))

(define-public ghc-fgl
  (package
    (name "ghc-fgl")
    (version "5.6.0.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/fgl/fgl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1i6cp4b3w7sjk7y1dq3fh6bci2sm5h3lnbbaw9ln19nwncg2wwll"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck"
                               "--allow-newer=hspec")))
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://web.engr.oregonstate.edu/~erwig/fgl/haskell")
    (synopsis
     "Martin Erwig's Functional Graph Library")
    (description "The functional graph library, FGL, is a collection of type
and function definitions to address graph problems.  The basis of the library
is an inductive definition of graphs in the style of algebraic data types that
encourages inductive, recursive definitions of graph algorithms.")
    (license license:bsd-3)))

(define-public ghc-chasingbottoms
  (package
    (name "ghc-chasingbottoms")
    (version "1.3.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/ChasingBottoms/"
                           "ChasingBottoms-" version ".tar.gz"))
       (sha256
        (base32
         "04jwwjs22mqc4hvpp4c3gpb79inrrq5sapks5khknspv2hslm61q"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-random" ,ghc-random)
       ("ghc-syb" ,ghc-syb)))
    (home-page "https://hackage.haskell.org/package/ChasingBottoms")
    (synopsis "Testing of partial and infinite values in Haskell")
    (description
     ;; FIXME: There should be a @comma{} in the uref text, but it is not
     ;; rendered properly.
     "This is a library for testing code involving bottoms or infinite values.
For the underlying theory and a larger example involving use of QuickCheck,
see the article
@uref{http://www.cse.chalmers.se/~nad/publications/danielsson-jansson-mpc2004.html,
\"Chasing Bottoms A Case Study in Program Verification in the Presence of
Partial and Infinite Values\"}.")
    (license license:expat)))

(define-public ghc-unordered-containers
  (package
    (name "ghc-unordered-containers")
    (version "0.2.7.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unordered-containers"
             "/unordered-containers-" version ".tar.gz"))
       (sha256
        (base32
         "00npqiphivjp2d7ryqsdavfn4m5v3w1lq2azhdsrfh0wsvqpg4ig"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-chasingbottoms" ,ghc-chasingbottoms)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-hashable" ,ghc-hashable)))
    (home-page
     "https://github.com/tibbe/unordered-containers")
    (synopsis
     "Efficient hashing-based container types")
    (description
     "Efficient hashing-based container types.  The containers have been
optimized for performance critical use, both in terms of large data quantities
and high speed.")
    (license license:bsd-3)))

(define-public ghc-uniplate
  (package
    (name "ghc-uniplate")
    (version "1.6.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/uniplate/uniplate-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1dx8f9aw27fz8kw0ad1nm6355w5rdl7bjvb427v2bsgnng30pipw"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (home-page "http://community.haskell.org/~ndm/uniplate/")
    (synopsis "Simple, concise and fast generic operations")
    (description "Uniplate is a library for writing simple and concise generic
operations.  Uniplate has similar goals to the original Scrap Your Boilerplate
work, but is substantially simpler and faster.")
    (license license:bsd-3)))

(define-public ghc-base64-bytestring
  (package
    (name "ghc-base64-bytestring")
    (version "1.0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/base64-bytestring/base64-bytestring-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0l1v4ddjdsgi9nqzyzcxxj76rwar3lzx8gmwf2r54bqan3san9db"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))         ; FIXME: testing libraries are missing.
    (home-page "https://github.com/bos/base64-bytestring")
    (synopsis "Base64 encoding and decoding for ByteStrings")
    (description "This library provides fast base64 encoding and decoding for
Haskell @code{ByteString}s.")
    (license license:bsd-3)))

(define-public ghc-annotated-wl-pprint
  (package
    (name "ghc-annotated-wl-pprint")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/annotated-wl-pprint"
             "/annotated-wl-pprint-" version
             ".tar.gz"))
       (sha256
        (base32
         "061xfz6qany3wf95csl8dcik2pz22cn8iv1qchhm16isw5zjs9hc"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/david-christiansen/annotated-wl-pprint")
    (synopsis
     "The Wadler/Leijen Pretty Printer, with annotation support")
    (description "This is a modified version of wl-pprint, which was based on
Wadler's paper \"A Prettier Printer\".  This version allows the library user
to annotate the text with semantic information, which can later be rendered in
a variety of ways.")
    (license license:bsd-3)))

(define-public ghc-wl-pprint
  (package
    (name "ghc-wl-pprint")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/wl-pprint/wl-pprint-"
                    version ".tar.gz"))
              (sha256
               (base32
                "166zvk4zwn2zaa9kx66m1av38m34qp6h4i65bri2sfnxgvx0700r"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/wl-pprint")
    (synopsis "Wadler/Leijen pretty printer")
    (description
     "This is a pretty printing library based on Wadler's paper @i{A Prettier
Printer}.  This version allows the library user to declare overlapping
instances of the @code{Pretty} class.")
    (license license:bsd-3)))

(define-public ghc-ansi-wl-pprint
  (package
    (name "ghc-ansi-wl-pprint")
    (version "0.6.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "ansi-wl-pprint/ansi-wl-pprint-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0gnb4mkqryv08vncxnj0bzwcnd749613yw3cxfzw6y3nsldp4c56"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)))
    (home-page "https://github.com/ekmett/ansi-wl-pprint")
    (synopsis "Wadler/Leijen Pretty Printer for colored ANSI terminal output")
    (description "This is a pretty printing library based on Wadler's paper
\"A Prettier Printer\".  It has been enhanced with support for ANSI terminal
colored output using the ansi-terminal package.")
    (license license:bsd-3)))

(define-public ghc-split
  (package
    (name "ghc-split")
    (version "0.2.3.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/split/split-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "12660m16c6sib3laaxn6w9s453pyx1b49myrmzfij372vq5bc5bn"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/split")
    (synopsis "Combinator library for splitting lists")
    (description "This package provides a collection of Haskell functions for
splitting lists into parts, akin to the @code{split} function found in several
mainstream languages.")
    (license license:bsd-3)))

(define-public ghc-parsec
  (package
    (name "ghc-parsec")
    (version "3.1.11")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/parsec/parsec-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0vk7q9j2128q191zf1sg0ylj9s9djwayqk9747k0a5fin4f2b1vg"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-mtl" ,ghc-mtl)))
    (arguments
     `(#:tests? #f)) ; FIXME: currently missing libraries used for tests.
    (home-page
     "https://github.com/aslatter/parsec")
    (synopsis "Monadic parser combinators")
    (description "Parsec is a parser library.  It is simple, safe, well
documented, has extensive libraries, good error messages, and is fast.  It is
defined as a monad transformer that can be stacked on arbitrary monads, and it
is also parametric in the input stream type.")
    (license license:bsd-3)))

(define-public ghc-parser-combinators
  (package
    (name "ghc-parser-combinators")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "parser-combinators/parser-combinators-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1azkz0a6ikym02s8wydjcklp7rz8k512bs4s9lp9g1g03m0yj95i"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/mrkkrp/parser-combinators")
    (synopsis "Commonly useful parser combinators")
    (description
     "This is a lightweight package providing commonly useful parser
combinators.")
    (license license:bsd-3)))

(define-public ghc-megaparsec
  (package
    (name "ghc-megaparsec")
    (version "6.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "megaparsec/megaparsec-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0h9azhs0dfrc359vrbd1jljrg3yfdbwd4p62cxqkn7mnh8913jpd"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-parser-combinators" ,ghc-parser-combinators)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/mrkkrp/megaparsec")
    (synopsis "Monadic parser combinators")
    (description
     "This is an industrial-strength monadic parser combinator library.
Megaparsec is a feature-rich package that strikes a nice balance between
speed, flexibility, and quality of parse errors.")
    (license license:bsd-2)))

(define-public ghc-vector
  (package
    (name "ghc-vector")
    (version "0.12.0.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/vector/vector-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0yrx2ypiaxahvaz84af5bi855hd3107kxkbqc8km29nsp5wyw05i"))))
    (build-system haskell-build-system)
    ;; FIXME: To simplify upgrading all Haskell packages, we leave the tests
    ;; disabled for now.
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-primitive" ,ghc-primitive)
       ("ghc-random" ,ghc-random)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ;; ("ghc-hunit" ,ghc-hunit)
       ;; ("ghc-test-framework" ,ghc-test-framework)
       ;; ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ;; ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ))
    (home-page "https://github.com/haskell/vector")
    (synopsis "Efficient Arrays")
    (description "This library provides an efficient implementation of
Int-indexed arrays (both mutable and immutable), with a powerful loop
optimisation framework.")
    (license license:bsd-3)))

(define-public ghc-vector-binary-instances
  (package
    (name "ghc-vector-binary-instances")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "vector-binary-instances/vector-binary-instances-"
             version ".tar.gz"))
       (sha256
        (base32
         "1y236jb72iab9ska1mc48z6yb0xgwmj45laaqdyjxksd84z7hbrb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/bos/vector-binary-instances")
    (synopsis "Instances of Data.Binary and Data.Serialize for vector")
    (description "This library provides instances of @code{Binary} for the
types defined in the @code{vector} package, making it easy to serialize
vectors to and from disk.  We use the generic interface to vectors, so all
vector types are supported.  Specific instances are provided for unboxed,
boxed and storable vectors.")
    (license license:bsd-3)))

(define-public ghc-bloomfilter
  (package
    (name "ghc-bloomfilter")
    (version "2.0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "bloomfilter/bloomfilter-" version ".tar.gz"))
       (sha256
        (base32
         "03vrmncg1c10a2wcg5skq30m1yiknn7nwxz2gblyyfaxglshspkc"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-random" ,ghc-random)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/bos/bloomfilter")
    (synopsis "Pure and impure Bloom filter implementations")
    (description "This package provides both mutable and immutable Bloom
filter data types, along with a family of hash functions and an easy-to-use
interface.")
    (license license:bsd-3)))

(define-public ghc-network
  (package
    (name "ghc-network")
    (version "2.6.3.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/network/network-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1rl2gl37cf4k0ddsq93q15fwdz1l25nhl4w205krbh7d5dg5y12p"))))
    (build-system haskell-build-system)
    ;; The regression tests depend on an unpublished module.
    (arguments `(#:tests? #f))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/haskell/network")
    (synopsis "Low-level networking interface")
    (description
     "This package provides a low-level networking interface.")
    (license license:bsd-3)))

(define-public ghc-network-uri
  (package
    (name "ghc-network-uri")
    (version "2.6.1.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/network-uri/network-uri-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1w27zkvn39kjr9lmw9421y8w43h572ycsfafsb7kyvr3a4ihlgj2"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))  ; FIXME: currently missing libraries used for tests.
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (inputs
     `(("ghc-parsec" ,ghc-parsec)
       ("ghc-network" ,ghc-network)))
    (home-page
     "https://github.com/haskell/network-uri")
    (synopsis "Library for URI manipulation")
    (description "This package provides an URI manipulation interface.  In
'network-2.6' the 'Network.URI' module was split off from the 'network'
package into this package.")
    (license license:bsd-3)))

(define-public ghc-ansi-terminal
  (package
    (name "ghc-ansi-terminal")
    (version "0.6.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/ansi-terminal/ansi-terminal-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0hpfw0k025y681m9ml1c712skrb1p4vh7z5x1f0ci9ww7ssjrh2d"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/feuerbach/ansi-terminal")
    (synopsis "ANSI terminal support for Haskell")
    (description "This package provides ANSI terminal support for Haskell.  It
allows cursor movement, screen clearing, color output showing or hiding the
cursor, and changing the title.")
    (license license:bsd-3)))

(define-public ghc-vault
  (package
    (name "ghc-vault")
    (version "0.3.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/vault/vault-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0j7gcs440q7qlgzi2hn36crgp2c0w69k40g6vj9hxlm31zk3shqb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-hashable" ,ghc-hashable)))
    (home-page
     "https://github.com/HeinrichApfelmus/vault")
    (synopsis "Persistent store for arbitrary values")
    (description "This package provides vaults for Haskell.  A vault is a
persistent store for values of arbitrary types.  It's like having first-class
access to the storage space behind @code{IORefs}.  The data structure is
analogous to a bank vault, where you can access different bank boxes with
different keys; hence the name.  Also provided is a @code{locker} type,
representing a store for a single element.")
    (license license:bsd-3)))

(define-public ghc-edisonapi
  (package
    (name "ghc-edisonapi")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/EdisonAPI"
                           "/EdisonAPI-" version ".tar.gz"))
       (sha256
        (base32 "0vmmlsj8ggbpwx6fkf5fvb6jp0zpx6iba6b28m80lllr2p8bi8wm"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-mtl" ,ghc-mtl)))
    (home-page "http://rwd.rdockins.name/edison/home/")
    (synopsis "Library of efficient, purely-functional data structures (API)")
    (description
     "Edison is a library of purely functional data structures written by
Chris Okasaki.  It is named after Thomas Alva Edison and for the mnemonic
value EDiSon (Efficient Data Structures).  Edison provides several families of
abstractions, each with multiple implementations.  The main abstractions
provided by Edison are: Sequences such as stacks, queues, and dequeues;
Collections such as sets, bags and heaps; and Associative Collections such as
finite maps and priority queues where the priority and element are distinct.")
    (license license:expat)))

(define-public ghc-edisoncore
  (package
    (name "ghc-edisoncore")
    (version "1.3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/EdisonCore"
                           "/EdisonCore-" version ".tar.gz"))
       (sha256
        (base32 "06shxmcqxcahcn6zgl64vlqix4fnq53d97drcgsh94qp7gp201ry"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-edisonapi" ,ghc-edisonapi)))
    (home-page "http://rwd.rdockins.name/edison/home/")
    (synopsis "Library of efficient, purely-functional data structures")
    (description
     "This package provides the core Edison data structure implementations,
including multiple sequence, set, bag, and finite map concrete implementations
with various performance characteristics.")
    (license license:expat)))

(define-public ghc-mmorph
  (package
    (name "ghc-mmorph")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/mmorph/mmorph-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1i8dzrc5qi3ryc9vrrmpn3sihmramsbhhd592w4w2k5g26qr3hql"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://hackage.haskell.org/package/mmorph")
    (synopsis "Monad morphisms")
    (description
     "This library provides monad morphism utilities, most commonly used for
manipulating monad transformer stacks.")
    (license license:bsd-3)))

(define-public ghc-ifelse
  (package
    (name "ghc-ifelse")
    (version "0.85")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "IfElse/IfElse-" version ".tar.gz"))
       (sha256
        (base32
         "1kfx1bwfjczj93a8yqz1n8snqiq5655qgzwv1lrycry8wb1vzlwa"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-mtl" ,ghc-mtl)))
    (home-page "http://hackage.haskell.org/package/IfElse")
    (synopsis "Monadic control flow with anaphoric variants")
    (description "This library provides functions for control flow inside of
monads with anaphoric variants on @code{if} and @code{when} and a C-like
@code{switch} function.")
    (license license:bsd-3)))

(define-public ghc-monad-control
  (package
    (name "ghc-monad-control")
    (version "1.0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/monad-control"
             "/monad-control-" version ".tar.gz"))
       (sha256
        (base32
         "1x018gi5irznx5rgzmkr2nrgh26r8cvqwkcfc6n6y05pdjf21c6l"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stm" ,ghc-stm)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/basvandijk/monad-control")
    (synopsis "Monad transformers to lift control operations like exception
catching")
    (description "This package defines the type class @code{MonadBaseControl},
a subset of @code{MonadBase} into which generic control operations such as
@code{catch} can be lifted from @code{IO} or any other base monad.")
    (license license:bsd-3)))

(define-public ghc-fail
  (package
    (name "ghc-fail")
    (version "4.9.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/fail/fail-"
                           version ".tar.gz"))
       (sha256
        (base32 "18nlj6xvnggy61gwbyrpmvbdkq928wv0wx2zcsljb52kbhddnp3d"))))
    (build-system haskell-build-system)
    (arguments `(#:haddock? #f)) ; Package contains no documentation.
    (home-page "https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail")
    (synopsis "Forward-compatible MonadFail class")
    (description
     "This package contains the @code{Control.Monad.Fail} module providing the
@uref{https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail, MonadFail}
class that became available in
@uref{https://hackage.haskell.org/package/base-4.9.0.0, base-4.9.0.0} for
older @code{base} package versions.  This package turns into an empty package
when used with GHC versions which already provide the
@code{Control.Monad.Fail} module.")
    (license license:bsd-3)))

(define-public ghc-monadplus
  (package
    (name "ghc-monadplus")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/monadplus"
                           "/monadplus-" version ".tar.gz"))
       (sha256
        (base32 "15b5320wdpmdp5slpphnc1x4rhjch3igw245dp2jxbqyvchdavin"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/monadplus")
    (synopsis "Filtering and folding over arbitrary MonadPlus instances")
    (description
     "This package generalizes many common stream operations such as
@code{filter}, @code{catMaybes} etc, enabling filtering and folding over
arbitrary @code{MonadPlus} instances.")
    (license license:bsd-3)))

(define-public ghc-byteorder
  (package
    (name "ghc-byteorder")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/byteorder/byteorder-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "06995paxbxk8lldvarqpb3ygcjbg4v8dk4scib1rjzwlhssvn85x"))))
    (build-system haskell-build-system)
    (home-page
     "http://community.haskell.org/~aslatter/code/byteorder")
    (synopsis
     "Exposes the native endianness of the system")
    (description
     "This package is for working with the native byte-ordering of the
system.")
    (license license:bsd-3)))

(define-public ghc-base-compat
  (package
    (name "ghc-base-compat")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/base-compat/base-compat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0452l6zf6fjhy4kxqwv6i6hhg6yfx4wcg450k3axpyj30l7jnq3x"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/base-compat")
    (synopsis "Haskell compiler compatibility library")
    (description "This library provides functions available in later versions
of base to a wider range of compilers, without requiring the use of CPP
pragmas in your code.")
    (license license:bsd-3)))

(define-public ghc-blaze-builder
  (package
    (name "ghc-blaze-builder")
    (version "0.4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/blaze-builder/blaze-builder-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1m33y6p5xldni8p4fzg8fmsyqvkfmnimdamr1xjnsmgm3dkf9lws"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))          ; FIXME: Missing test libraries.
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (home-page "https://github.com/lpsmith/blaze-builder")
    (synopsis "Efficient buffered output")
    (description "This library provides an implementation of the older
@code{blaze-builder} interface in terms of the new builder that shipped with
@code{bytestring-0.10.4.0}.  This implementation is mostly intended as a
bridge to the new builder, so that code that uses the old interface can
interoperate with code that uses the new implementation.")
    (license license:bsd-3)))

(define-public ghc-blaze-markup
  (package
    (name "ghc-blaze-markup")
    (version "0.8.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "blaze-markup/blaze-markup-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0m3h3ryxj5r74mv5g5dnfq5jbbwmvkl7ray18vi20d5vd93sydj4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://jaspervdj.be/blaze")
    (synopsis "Fast markup combinator library for Haskell")
    (description "This library provides core modules of a markup combinator
library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-easy-file
  (package
    (name "ghc-easy-file")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/easy-file/easy-file-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0v75081bx4qzlqy29hh639nzlr7dncwza3qxbzm9njc4jarf31pz"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/kazu-yamamoto/easy-file")
    (synopsis "File handling library for Haskell")
    (description "This library provides file handling utilities for Haskell.")
    (license license:bsd-3)))

(define-public ghc-async
  (package
    (name "ghc-async")
    (version "2.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/async/async-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1qj4fp1ynwg0l453gmm27vgkzb5k5m2hzdlg5rdqi9kf8rqy90yd"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stm" ,ghc-stm)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/simonmar/async")
    (synopsis "Library to run IO operations asynchronously")
    (description "Async provides a library to run IO operations
asynchronously, and wait for their results.  It is a higher-level interface
over threads in Haskell, in which @code{Async a} is a concurrent thread that
will eventually deliver a value of type @code{a}.")
    (license license:bsd-3)))

(define-public ghc-fingertree
  (package
    (name "ghc-fingertree")
    (version "0.1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/fingertree/fingertree-"
             version ".tar.gz"))
       (sha256
        (base32
         "1ryjj7qrx70ckcjlr02x9zh86kfp76azbxq05r7hawqkaqg44sfs"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hackage.haskell.org/package/fingertree")
    (synopsis "Generic finger-tree structure")
    (description "This library provides finger trees, a general sequence
representation with arbitrary annotations, for use as a base for
implementations of various collection types.  It includes examples, as
described in section 4 of Ralf Hinze and Ross Paterson, \"Finger trees: a
simple general-purpose data structure\".")
    (license license:bsd-3)))

(define-public ghc-optparse-applicative
  (package
    (name "ghc-optparse-applicative")
    (version "0.14.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/optparse-applicative"
             "/optparse-applicative-" version ".tar.gz"))
       (sha256
        (base32
         "0c3z1mvynlyv1garjbdmdd3npm40dabgm75js4r07cf766c1wd71"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/pcapriotti/optparse-applicative")
    (synopsis "Utilities and combinators for parsing command line options")
    (description "This package provides utilities and combinators for parsing
command line options in Haskell.")
    (license license:bsd-3)))

(define-public ghc-base-orphans
  (package
    (name "ghc-base-orphans")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/base-orphans/base-orphans-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "03mdww5j0gwai7aqlx3m71ldmjcr99jzpkcclzjfclk6a6kjla67"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/base-orphans")
    (synopsis "Orphan instances for backwards compatibility")
    (description "This package defines orphan instances that mimic instances
available in later versions of base to a wider (older) range of compilers.")
    (license license:bsd-3)))

(define-public ghc-auto-update
  (package
    (name "ghc-auto-update")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/auto-update/auto-update-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "09dlh2alsx2mw5kvj931yhbj0aw7jmly2cm9xbscm2sf098w35jy"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "Efficiently run periodic, on-demand actions")
    (description "This library provides mechanisms to efficiently run
periodic, on-demand actions in Haskell.")
    (license license:expat)))

(define-public ghc-tagged
  (package
    (name "ghc-tagged")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tagged/tagged-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "16cdzh0bw16nvjnyyy5j9s60malhz4nnazw96vxb0xzdap4m2z74"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://hackage.haskell.org/package/tagged")
    (synopsis "Haskell phantom types to avoid passing dummy arguments")
    (description "This library provides phantom types for Haskell 98, to avoid
having to unsafely pass dummy arguments.")
    (license license:bsd-3)))

(define-public ghc-unbounded-delays
  (package
    (name "ghc-unbounded-delays")
    (version "0.1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unbounded-delays/unbounded-delays-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1f4h87503m3smhip432q027wj3zih18pmz2rnafh60589ifcl420"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/basvandijk/unbounded-delays")
    (synopsis "Unbounded thread delays and timeouts")
    (description "The @code{threadDelay} and @code{timeout} functions from the
Haskell base library use the bounded @code{Int} type for specifying the delay
or timeout period.  This package provides alternative functions which use the
unbounded @code{Integer} type.")
    (license license:bsd-3)))

(define-public ghc-clock
  (package
    (name "ghc-clock")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "clock/"
             "clock-" version ".tar.gz"))
       (sha256
        (base32 "07v91s20halsqjmziqb1sqjp2sjpckl9by7y28aaklwqi2bh2rl8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://hackage.haskell.org/package/clock")
    (synopsis "High-resolution clock for Haskell")
    (description "A package for convenient access to high-resolution clock and
timer functions of different operating systems via a unified API.")
    (license license:bsd-3)))

(define-public ghc-charset
  (package
    (name "ghc-charset")
    (version "0.3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/charset/charset-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1gn0m96qpjww8hpp2g1as5yy0wcwy4iq73h3kz6g0yxxhcl5sh9x"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (home-page "https://github.com/ekmett/charset")
    (synopsis "Fast unicode character sets for Haskell")
    (description "This package provides fast unicode character sets for
Haskell, based on complemented PATRICIA tries.")
    (license license:bsd-3)))

(define-public ghc-bytestring-builder
  (package
    (name "ghc-bytestring-builder")
    (version "0.10.8.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/bytestring-builder"
             "/bytestring-builder-" version ".tar.gz"))
       (sha256
        (base32
         "1hnvjac28y44yn78c9vdp1zvrknvlw98ky3g4n5vivr16rvh8x3d"))))
    (build-system haskell-build-system)
    (arguments `(#:haddock? #f)) ; Package contains no documentation.
    (home-page "https://hackage.haskell.org/package/bytestring-builder")
    (synopsis "The new bytestring builder, packaged outside of GHC")
    (description "This package provides the bytestring builder that is
debuting in bytestring-0.10.4.0, which should be shipping with GHC 7.8.
Compatibility package for older packages.")
    (license license:bsd-3)))

(define-public ghc-nats
  (package
    (name "ghc-nats")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/nats/nats-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1kfl2yy97nb7q0j17v96rl73xvi3z4db9bk0xychc76dax41n78k"))))
    (build-system haskell-build-system)
    (arguments `(#:haddock? #f))
    (inputs
     `(("ghc-hashable" ,ghc-hashable)))
    (home-page "https://hackage.haskell.org/package/nats")
    (synopsis "Natural numbers")
    (description "This library provides the natural numbers for Haskell.")
    (license license:bsd-3)))

(define-public ghc-void
  (package
    (name "ghc-void")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/void/void-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1x15x2axz84ndw2bf60vjqljhrb0w95lddaljsxrl0hcd29zvw69"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-hashable" ,ghc-hashable)))
    (home-page "https://github.com/ekmett/void")
    (synopsis
     "Logically uninhabited data type")
    (description
     "A Haskell 98 logically uninhabited data type, used to indicate that a
given term should not exist.")
    (license license:bsd-3)))

(define-public ghc-kan-extensions
  (package
    (name "ghc-kan-extensions")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/kan-extensions/kan-extensions-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1qm0kf4krmyjbjynn96ab0h3q117vwcia5nin7n2b8b4f3jrzph1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-adjunctions" ,ghc-adjunctions)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-free" ,ghc-free)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-tagged" ,ghc-tagged)))
    (home-page "https://github.com/ekmett/kan-extensions/")
    (synopsis "Kan extensions library")
    (description "This library provides Kan extensions, Kan lifts, various
forms of the Yoneda lemma, and (co)density (co)monads for Haskell.")
    (license license:bsd-3)))

(define-public ghc-call-stack
  (package
    (name "ghc-call-stack")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "call-stack/call-stack-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1qmihf5jafmc79sk52l6gpx75f5bnla2lp62kh3p34x3j84mwpzj"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-nanospec" ,ghc-nanospec)))
    (home-page "https://github.com/sol/call-stack#readme")
    (synopsis "Use GHC call-stacks in a backward compatible way")
    (description "This package provides a compatibility layer for using GHC
call stacks with different versions of the compiler.")
    (license license:expat)))

;; This is used as an input to ghc-hunit.  We cannot use ghc-call-stack there,
;; because it depends on ghc-nanospec, which depends on ghc-hunit.
(define-public ghc-call-stack-boot
  (package
    (inherit ghc-call-stack)
    (arguments '(#:tests? #f))
    (inputs '())))

(define-public ghc-statevar
  (package
    (name "ghc-statevar")
    (version "1.1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/StateVar/StateVar-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1dzz9l0haswgag9x56q7n57kw18v7nhmzkjyr61nz9y9npn8vmks"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stm" ,ghc-stm)))
    (home-page "https://hackage.haskell.org/package/StateVar")
    (synopsis "State variables for Haskell")
    (description "This package provides state variables, which are references
in the @code{IO} monad, like @code{IORef}s or parts of the OpenGL state.")
    (license license:bsd-3)))

(define-public ghc-lens
  (package
    (name "ghc-lens")
    (version "4.15.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/lens/lens-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1lkwlnhgpgnsz046mw4qs0fa7h4l012gilrr3nf3spllsy3pnbkl"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-doctest" ,ghc-doctest-0.13)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-free" ,ghc-free)
       ("ghc-kan-extensions" ,ghc-kan-extensions)
       ("ghc-parallel" ,ghc-parallel)
       ("ghc-reflection" ,ghc-reflection)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-vector" ,ghc-vector)
       ("ghc-call-stack" ,ghc-call-stack)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-profunctors" ,ghc-profunctors)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-text" ,ghc-text)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-void" ,ghc-void)
       ("ghc-generic-deriving" ,ghc-generic-deriving)
       ("ghc-nats" ,ghc-nats)
       ("ghc-simple-reflect" ,ghc-simple-reflect)
       ("hlint" ,hlint)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-th" ,ghc-test-framework-th)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/ekmett/lens/")
    (synopsis "Lenses, Folds and Traversals")
    (description "This library provides @code{Control.Lens}.  The combinators
in @code{Control.Lens} provide a highly generic toolbox for composing families
of getters, folds, isomorphisms, traversals, setters and lenses and their
indexed variants.")
    (license license:bsd-3)))

(define-public ghc-cheapskate
  (package
    (name "ghc-cheapskate")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cheapskate/cheapskate-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1hiqi7h76shjzs2zj0j8g6wnq2hbiq1hmfafdazr97fba2zl2432"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-text" ,ghc-text)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-xss-sanitize" ,ghc-xss-sanitize)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-syb" ,ghc-syb)
       ("ghc-uniplate" ,ghc-uniplate)))
    (home-page "https://github.com/jgm/cheapskate")
    (synopsis "Experimental markdown processor")
    (description "Cheapskate is an experimental Markdown processor in pure
Haskell.  It aims to process Markdown efficiently and in the most forgiving
possible way.  It is designed to deal with any input, including garbage, with
linear performance.  Output is sanitized by default for protection against
cross-site scripting (@dfn{XSS}) attacks.")
    (license license:bsd-3)))

(define-public ghc-bifunctors
  (package
    (name "ghc-bifunctors")
    (version "5.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/bifunctors/bifunctors-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "04fbsysm6zl8kmvqgffmrqa9bxl9dl2gibrd51asqzg737mb4ark"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-th-abstraction" ,ghc-th-abstraction)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-semigroups" ,ghc-semigroups)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/ekmett/bifunctors/")
    (synopsis "Bifunctors for Haskell")
    (description "This package provides bifunctors for Haskell.")
    (license license:bsd-3)))

(define-public ghc-semigroupoids
  (package
    (name "ghc-semigroupoids")
    (version "5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/semigroupoids/semigroupoids-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0dgqc59p4xx5cl8qkpm6sn4wd3n59rq7l6din76hf10bnklqrb0n"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: doctest packagedb propagation problem.
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-tagged" ,ghc-tagged)))
    (home-page "https://github.com/ekmett/semigroupoids")
    (synopsis "Semigroupoids operations for Haskell")
    (description "This library provides a wide array of (semi)groupoids and
operations for working with them.  A @code{Semigroupoid} is a @code{Category}
without the requirement of identity arrows for every object in the category.
A @code{Category} is any @code{Semigroupoid} for which the Yoneda lemma holds.
Finally, to work with these weaker structures it is beneficial to have
containers that can provide stronger guarantees about their contents, so
versions of @code{Traversable} and @code{Foldable} that can be folded with
just a @code{Semigroup} are added.")
    (license license:bsd-3)))

(define-public ghc-contravariant
  (package
    (name "ghc-contravariant")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/contravariant/contravariant-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "117fff8kkrvlmr8cb2jpj71z7lf2pdiyks6ilyx89mry6zqnsrp1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-void" ,ghc-void)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-statevar" ,ghc-statevar)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page
     "https://github.com/ekmett/contravariant/")
    (synopsis "Contravariant functors")
    (description "Contravariant functors for Haskell.")
    (license license:bsd-3)))

(define-public ghc-semigroups
  (package
    (name "ghc-semigroups")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/semigroups/semigroups-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1r6hsn3am3dpf4rprrj4m04d9318v9iq02bin0pl29dg4a3gzjax"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-nats" ,ghc-nats)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-text" ,ghc-text)
       ("ghc-hashable" ,ghc-hashable)))
    (home-page "https://github.com/ekmett/semigroups/")
    (synopsis "Semigroup operations for Haskell")
    (description "This package provides semigroups for Haskell.  In
mathematics, a semigroup is an algebraic structure consisting of a set
together with an associative binary operation.  A semigroup generalizes a
monoid in that there might not exist an identity element.  It
also (originally) generalized a group (a monoid with all inverses) to a type
where every element did not have to have an inverse, thus the name
semigroup.")
    (license license:bsd-3)))

(define-public ghc-free
  (package
    (name "ghc-free")
    (version "4.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/free/free-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1147s393442xf4gkpbq0rd1p286vmykgx85mxhk5d1c7wfm4bzn9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-prelude-extras" ,ghc-prelude-extras)
       ("ghc-profunctors" ,ghc-profunctors)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page "https://github.com/ekmett/free/")
    (synopsis "Unrestricted monads for Haskell")
    (description "This library provides free monads, which are useful for many
tree-like structures and domain specific languages.  If @code{f} is a
@code{Functor} then the free @code{Monad} on @code{f} is the type of trees
whose nodes are labeled with the constructors of @code{f}.  The word \"free\"
is used in the sense of \"unrestricted\" rather than \"zero-cost\": @code{Free
f} makes no constraining assumptions beyond those given by @code{f} and the
definition of @code{Monad}.")
    (license license:bsd-3)))

(define-public ghc-adjunctions
  (package
    (name "ghc-adjunctions")
    (version "4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/adjunctions/adjunctions-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1k1ykisf96i4g2zm47c45md7p42c4vsp9r73392pz1g8mx7s2j5r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-profunctors" ,ghc-profunctors)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-free" ,ghc-free)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-void" ,ghc-void)))
    (home-page "https://github.com/ekmett/adjunctions/")
    (synopsis "Adjunctions and representable functors")
    (description "This library provides adjunctions and representable functors
for Haskell.")
    (license license:bsd-3)))

(define-public ghc-equivalence
  (package
    (name "ghc-equivalence")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/equivalence"
                           "/equivalence-" version ".tar.gz"))
       (sha256
        (base32 "0a85bdyyvjqs5z4kfhhf758210k9gi9dv42ik66a3jl0z7aix8kx"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-stmonadtrans" ,ghc-stmonadtrans)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/pa-ba/equivalence")
    (synopsis "Maintaining an equivalence relation implemented as union-find")
    (description
     "This is an implementation of Tarjan's Union-Find algorithm (Robert E.@:
Tarjan.  \"Efficiency of a Good But Not Linear Set Union Algorithm\",JACM
22(2), 1975) in order to maintain an equivalence relation.  This
implementation is a port of the @code{union-find} package using the @code{ST}
monad transformer (instead of the IO monad).")
    (license license:bsd-3)))

(define-public ghc-fast-logger
  (package
    (name "ghc-fast-logger")
    (version "2.4.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/fast-logger/fast-logger-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ad2vq4nifdxshqk9yrmghqizhkgybfz134kpr6padglb2mxxrdv"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-auto-update" ,ghc-auto-update)
       ("ghc-easy-file" ,ghc-easy-file)
       ("ghc-text" ,ghc-text)
       ("ghc-unix-time" ,ghc-unix-time)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://hackage.haskell.org/package/fast-logger")
    (synopsis "Fast logging system")
    (description "This library provides a fast logging system for Haskell.")
    (license license:bsd-3)))

(define-public ghc-doctest
  (package
    (name "ghc-doctest")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/doctest/doctest-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0xv4vx1r3mk7cmiwywzrq25545cx3i7imhcx33mk47r88j5c49fj"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))          ; FIXME: missing test framework
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-paths" ,ghc-paths)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-stringbuilder" ,ghc-stringbuilder)
       ("ghc-silently" ,ghc-silently)
       ("ghc-setenv" ,ghc-setenv)))
    (home-page
     "https://github.com/sol/doctest#readme")
    (synopsis "Test interactive Haskell examples")
    (description "The doctest program checks examples in source code comments.
It is modeled after doctest for Python, see
@uref{https://docs.python.org/library/doctest.html, the Doctest website}.")
    (license license:expat)))

(define-public ghc-doctest-0.12
  (package (inherit ghc-doctest)
    (name "ghc-doctest")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/doctest/doctest-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "13h549cpgcvb7c54c7wif28g5wak84dxc3ais0hlqhzk1q6la91a"))))
    (inputs
     `(("ghc-code-page" ,ghc-code-page)
       ,@(package-inputs ghc-doctest)))))

(define-public ghc-doctest-0.13
  (package (inherit ghc-doctest-0.12)
    (name "ghc-doctest")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "doctest/doctest-" version ".tar.gz"))
       (sha256
        (base32
         "08g3nmpqbnbkxc95d65hkhfabwax10qrq51vlynr342npn40mn2b"))))))

(define-public ghc-lifted-base
  (package
    (name "ghc-lifted-base")
    (version "0.2.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/lifted-base/lifted-base-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "17yz4n7q96x4cp8vxai8csn2vmpigxvipkfh48arahf91f0xy18n"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: Missing testing libraries.
    (inputs
     `(("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/basvandijk/lifted-base")
    (synopsis "Lifted IO operations from the base library")
    (description "Lifted-base exports IO operations from the @code{base}
library lifted to any instance of @code{MonadBase} or @code{MonadBaseControl}.
Note that not all modules from @code{base} are converted yet.  The package
includes a copy of the @code{monad-peel} test suite written by Anders
Kaseorg.")
    (license license:bsd-3)))

(define-public ghc-word8
  (package
    (name "ghc-word8")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/word8/word8-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "12jx7f13d2h1djq4fh4dyrab61sm49mj1w61j3rzp2vjfm696c16"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/word8")
    (synopsis "Word8 library for Haskell")
    (description "Word8 library to be used with @code{Data.ByteString}.")
    (license license:bsd-3)))

(define-public ghc-stringsearch
  (package
    (name "ghc-stringsearch")
    (version "0.3.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/stringsearch/stringsearch-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0jpy9xjcjdbpi3wk6mg7xwd7wfi2mma70p97v1ij5i8bj9qijpr9"))))
    (build-system haskell-build-system)
    (home-page "https://bitbucket.org/dafis/stringsearch")
    (synopsis "Fast searching, splitting and replacing of ByteStrings")
    (description "This package provides several functions to quickly search
for substrings in strict or lazy @code{ByteStrings}.  It also provides
functions for breaking or splitting on substrings and replacing all
occurrences of a substring (the first in case of overlaps) with another.")
    (license license:bsd-3)))

(define-public ghc-integer-logarithms
  (package
    (name "ghc-integer-logarithms")
    (version "1.0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "integer-logarithms/integer-logarithms-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1wj8kgjg5bn2yrs4zh9qfjv85cx6w998j9pi39yrbv305944mb9j"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=tasty")))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)))
    (home-page "https://github.com/Bodigrim/integer-logarithms")
    (synopsis "Integer logarithms")
    (description
     "This package provides the following modules:
@code{Math.NumberTheory.Logarithms} and
@code{Math.NumberTheory.Powers.Integer} from the @code{arithmoi} package,
@code{GHC.Integer.Logarithms.Compat} and
@code{Math.NumberTheory.Power.Natural}, as well as some additional functions
in migrated modules.")
    (license license:expat)))

(define-public ghc-scientific
  (package
    (name "ghc-scientific")
    (version "0.3.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/scientific/scientific-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "03ql2f0ac8bsl524idy9xxa3kxisb2sj3avflzw580j5hzy0m397"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-integer-logarithms" ,ghc-integer-logarithms)
       ("ghc-text" ,ghc-text)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-primitive" ,ghc-primitive)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-ant-xml" ,ghc-tasty-ant-xml)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/basvandijk/scientific")
    (synopsis "Numbers represented using scientific notation")
    (description "This package provides @code{Data.Scientific}, which provides
the number type @code{Scientific}.  Scientific numbers are arbitrary precision
and space efficient.  They are represented using
@uref{https://en.wikipedia.org/wiki/Scientific_notation, scientific
notation}.")
    (license license:bsd-3)))

(define-public ghc-boxes
  (package
    (name "ghc-boxes")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/boxes/boxes-"
                           version ".tar.gz"))
       (sha256
        (base32 "1n7xiplzd3s1a39nizwjcgsh3wi2348mp21c3fk19v98ialfjgjf"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-split" ,ghc-split)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/boxes")
    (synopsis "2D text pretty-printing library")
    (description
     "Boxes is a pretty-printing library for laying out text in two dimensions,
using a simple box model.")
    (license license:bsd-3)))

(define-public ghc-deepseq-generics
  (package
    (name "ghc-deepseq-generics")
    (version "0.2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "deepseq-generics/deepseq-generics-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17bwghc15mc9pchfd1w46jh2p3wzc86aj6a537wqwxn08rayzcxh"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/hvr/deepseq-generics")
    (synopsis "Generic RNF implementation")
    (description
     "This package provides a @code{GHC.Generics}-based
@code{Control.DeepSeq.Generics.genericRnf} function which can be used for
providing an 'rnf' implementation.")
    (license license:bsd-3)))

(define-public ghc-string-qq
  (package
    (name "ghc-string-qq")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/string-qq/string-qq-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0662m3i5xrdrr95w829bszkhp88mj9iy1zya54vk2sl5hz9wlmwp"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/string-qq")
    (synopsis
     "QuasiQuoter for non-interpolated strings, texts and bytestrings.")
    (description
     "This package provides a quasiquoter for non-interpolated strings, texts
and bytestrings.")
    (license license:public-domain)))

(define-public ghc-pandoc-types
  (package
    (name "ghc-pandoc-types")
    (version "1.17.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "pandoc-types/pandoc-types-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1q6v2bynij724fv347mhqxdscwifzrx5jb9mq80608qf638fn717"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck")))
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-string-qq" ,ghc-string-qq)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "http://johnmacfarlane.net/pandoc")
    (synopsis "Types for representing a structured document")
    (description
     "This module defines the @code{Pandoc} data structure, which is used by
pandoc to represent structured documents.  It also provides functions for
building up, manipulating and serialising @code{Pandoc} structures.")
    (license license:bsd-3)))

(define-public ghc-pandoc-types-for-pandoc-1
  (package (inherit ghc-pandoc-types)
    (version "1.17.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "pandoc-types/pandoc-types-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1csipjdq00iiq77k2wlrg4i7afrzlh8nl585q785xzw7nn45b0n8"))))
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck"
                               "--allow-newer=HUnit")))
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-aeson" ,ghc-aeson-for-pandoc-1)
       ("ghc-string-qq" ,ghc-string-qq)))))

(define-public ghc-texmath
  (package
    (name "ghc-texmath")
    (version "0.11.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "texmath/texmath-" version ".tar.gz"))
              (sha256
               (base32
                "11dc09hfnyfsz20ch2c867w0zdgjkzq41506lm61i3dk87ngdisf"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-split" ,ghc-split)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-xml" ,ghc-xml)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-pandoc-types" ,ghc-pandoc-types)))
    (home-page "https://github.com/jgm/texmath")
    (synopsis "Conversion between formats used to represent mathematics")
    (description
     "The texmath library provides functions to read and write TeX math,
presentation MathML, and OMML (Office Math Markup Language, used in Microsoft
Office).  Support is also included for converting math formats to pandoc's
native format (allowing conversion, via pandoc, to a variety of different
markup formats).  The TeX reader supports basic LaTeX and AMS extensions, and
it can parse and apply LaTeX macros.")
    (license license:gpl2+)))

(define-public ghc-texmath-for-pandoc-1
  (package (inherit ghc-texmath)
    (version "0.9.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "texmath/texmath-" version ".tar.gz"))
              (sha256
               (base32
                "129q33m56diiv35kdwfb07838wrg0mm88kxdqxfyl1zvf9nzkqkd"))))
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-pandoc-types" ,ghc-pandoc-types-for-pandoc-1)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-split" ,ghc-split)
       ("ghc-syb" ,ghc-syb)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-xml" ,ghc-xml)))))

(define-public ghc-regex-pcre-builtin
  (package
    (name "ghc-regex-pcre-builtin")
    (version "0.94.4.8.8.35")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "regex-pcre-builtin/regex-pcre-builtin-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0y7as9wqlkykpipka2cfdhmcnin345q01pp0wsva8fwmvsavdl8b"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)))
    (home-page "https://hackage.haskell.org/package/regex-pcre")
    (synopsis "Enhancement of the builtin Text.Regex library")
    (description
     "This package is an enhancement of the @code{Text.Regex} library,
providing the PCRE backend to accompany regex-base, with bundled code from
@url{https://www.pcre.org}.")
    (license license:bsd-3)))

(define-public ghc-diff
  (package
    (name "ghc-diff")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "Diff/Diff-" version ".tar.gz"))
              (sha256
               (base32
                "0bqcdvhxx8dmqc3793m6axg813wv9ldz2j37f1wygbbrbbndmdvp"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hub.darcs.net/sterlingclover/Diff")
    (synopsis "O(ND) diff algorithm in Haskell")
    (description
     "This package provides an implementation of the standard diff algorithm,
and utilities for pretty printing.")
    (license license:bsd-3)))

(define-public ghc-highlighting-kate
  (package
    (name "ghc-highlighting-kate")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "highlighting-kate/highlighting-kate-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03c4flh4h1jd48bx0qmplax3q8w6wj1dhbh6j0xhaf5h95fbinki"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-diff" ,ghc-diff)
       ("ghc-regex-pcre-builtin" ,ghc-regex-pcre-builtin)))
    (native-inputs
     `(("ghc-parsec" ,ghc-parsec)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-mtl" ,ghc-mtl)))
    (home-page "https://github.com/jgm/highlighting-kate")
    (synopsis "Syntax highlighting library")
    (description
     "Highlighting-kate is a syntax highlighting library with support for
nearly one hundred languages.  The syntax parsers are automatically generated
from @uref{https://kate-editor.org/, Kate syntax descriptions}, so any syntax
supported by Kate can be added.  An (optional) command-line program is
provided, along with a utility for generating new parsers from Kate XML syntax
descriptions.")
    (license license:gpl2+)))

(define-public ghc-cmark
  (package
    (name "ghc-cmark")
    (version "0.5.6")
    (source (origin
              (method url-fetch)
              ;; XXX As of version 0.5.6, this package bundles libcmark 0.28.0.
              ;; See cbits/cmark_version.h.
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "cmark/cmark-" version ".tar.gz"))
              (sha256
               (base32
                "1c1j3a8b9qx5zk9myqm3gap8ymz7fipwrdmyfsq9wkkdr9x4np45"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/jgm/commonmark-hs")
    (synopsis "Fast, accurate CommonMark (Markdown) parser and renderer")
    (description
     "This package provides Haskell bindings for
@uref{https://github.com/jgm/cmark, libcmark}, the reference parser for
CommonMark, a fully specified variant of Markdown.  It includes bundled libcmark
sources, and does not require prior installation of the C library.")
    (license license:bsd-3)))

(define-public ghc-cmark-gfm
  (package
    (name "ghc-cmark-gfm")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cmark-gfm/cmark-gfm-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1fkisbrydmdx5h8yad09fzk8h1b1j53r02g7vb81izgdwj9b673k"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/kivikakk/cmark-gfm-hs")
    (synopsis
     "Fast, accurate GitHub Flavored Markdown parser and renderer")
    (description
     "This package provides Haskell bindings for libcmark-gfm, the reference
parser for GitHub Flavored Markdown, a fully specified variant of Markdown.
It includes sources for libcmark-gfm and does not require prior installation
of the C library.")
    (license license:bsd-3)))

(define-public ghc-executable-path
  (package
    (name "ghc-executable-path")
    (version "0.0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "executable-path/executable-path-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0vxwmnsvx13cawcyhbyljkds0l1vr996ijldycx7nj0asjv45iww"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/executable-path")
    (synopsis "Find out the full path of the executable")
    (description
     "The documentation of @code{System.Environment.getProgName} says that
\"However, this is hard-to-impossible to implement on some non-Unix OSes, so
instead, for maximum portability, we just return the leafname of the program
as invoked.\" This library tries to provide the missing path.")
    (license license:public-domain)))

(define-public ghc-enclosed-exceptions
  (package
    (name "ghc-enclosed-exceptions")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "enclosed-exceptions/enclosed-exceptions-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1fghjj7nkiddrf03ks8brjpr5x25yi9fs7xg6adbi4mc2gqr6vdg"))))
    (build-system haskell-build-system)
    ;; FIXME: one of the tests blocks forever:
    ;; "thread blocked indefinitely in an MVar operation"
    (arguments '(#:tests? #f))
    (inputs
     `(("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-async" ,ghc-async)
       ("ghc-transformers-base" ,ghc-transformers-base)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/jcristovao/enclosed-exceptions")
    (synopsis "Catch all exceptions from within an enclosed computation")
    (description
     "This library implements a technique to catch all exceptions raised
within an enclosed computation, while remaining responsive to (external)
asynchronous exceptions.")
    (license license:expat)))

(define-public ghc-packedstring
  (package
    (name "ghc-packedstring")
    (version "0.1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "packedstring/packedstring-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1x78pzzdlnpcmh9p37rlf8m5cxf3yqm2alf3whl4zpr9w25r0qj8"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enable-extension
          (lambda _
            ;; This package won't compile without the StandaloneDeriving
            ;; extension.
            (substitute* "packedstring.cabal"
              (("CPP") "CPP, StandaloneDeriving"))
            #t)))))
    (home-page "https://hackage.haskell.org/package/packedstring")
    (synopsis "Library for packed strings")
    (description
     "This deprecated library provides an implementation of packed strings.")
    (license license:bsd-3)))

(define-public ghc-th-abstraction
  (package
    (name "ghc-th-abstraction")
    (version "0.2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "th-abstraction/th-abstraction-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0g42h6wnj2awc5ryhbvx009wd8w75pn66bjzsq1z4s3xajd2hbp5"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/glguy/th-abstraction")
    (synopsis "Nicer interface for reified information about data types")
    (description
     "This package normalizes variations in the interface for inspecting
datatype information via Template Haskell so that packages and support a
single, easier to use informational datatype while supporting many versions of
Template Haskell.")
    (license license:isc)))

(define-public ghc-th-lift
  (package
    (name "ghc-th-lift")
    (version "0.7.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "th-lift/th-lift-" version ".tar.gz"))
              (sha256
               (base32
                "0ay10b78x3969rpqqrgzy8srkl6iby2cljbf3mm17na8x22k7y1c"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/mboes/th-lift")
    (synopsis "Derive Template Haskell's Lift class for datatypes")
    (description
     "This is a Haskell library to derive Template Haskell's Lift class for
datatypes.")
    (license license:bsd-3)))

(define-public ghc-th-lift-instances
  (package
    (name "ghc-th-lift-instances")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "th-lift-instances/th-lift-instances-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1f56cp6ckcalld5jchv0kxpjkwcsixd7smd0g7r8cg67ppx6m90x"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-th-lift" ,ghc-th-lift)
       ("ghc-vector" ,ghc-vector)
       ("ghc-text" ,ghc-text)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/bennofs/th-lift-instances/")
    (synopsis "Lift instances for template-haskell for common data types.")
    (description "Most data types in the Haskell platform do not have Lift
instances.  This package provides orphan instances for @code{containers},
@code{text}, @code{bytestring} and @code{vector}.")
    (license license:bsd-3)))

(define-public ghc-th-expand-syns
  (package
    (name "ghc-th-expand-syns")
    (version "0.4.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "th-expand-syns/th-expand-syns-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1sjy7a17zwyvlbkc8gklii67sy78wpnw35fyb00lsbnpk4cryd2r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-syb" ,ghc-syb)))
    (home-page "https://hackage.haskell.org/package/th-expand-syns")
    (synopsis "Expands type synonyms in Template Haskell ASTs")
    (description
     "This package enables users to expand type synonyms in Template Haskell
@dfn{abstract syntax trees} (ASTs).")
    (license license:bsd-3)))

(define-public ghc-th-reify-many
  (package
    (name "ghc-th-reify-many")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "th-reify-many/th-reify-many-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1b76zjxkj0v0n8zj9l0nwav2irm0c43rx6qjihfw8klmmxvx59df"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-safe" ,ghc-safe)
       ("ghc-th-expand-syns" ,ghc-th-expand-syns)))
    (home-page "https://github.com/mgsloan/th-reify-many")
    (synopsis "Recurseively reify template haskell datatype info")
    (description
     "th-reify-many provides functions for recursively reifying top level
declarations.  The main intended use case is for enumerating the names of
datatypes reachable from an initial datatype, and passing these names to some
function which generates instances.")
    (license license:bsd-3)))

(define-public ghc-th-orphans
  (package
    (name "ghc-th-orphans")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "th-orphans/th-orphans-" version ".tar.gz"))
              (sha256
               (base32
                "0102vkyzpgnp2fys8pvw4khrhrh2y1b8dp1slgvn020vg4s351mc"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-th-lift" ,ghc-th-lift)
       ("ghc-th-lift-instances" ,ghc-th-lift-instances)
       ("ghc-th-reify-many" ,ghc-th-reify-many)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-generic-deriving" ,ghc-generic-deriving)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://hackage.haskell.org/package/th-orphans")
    (synopsis "Orphan instances for TH datatypes")
    (description
     "This package provides orphan instances for Template Haskell datatypes.  In particular,
instances for @code{Ord} and @code{Lift}, as well as a few missing @code{Show}
and @code{Eq} instances.  These instances used to live in the haskell-src-meta
package, and that's where the version number started.")
    (license license:bsd-3)))

(define-public ghc-geniplate-mirror
  (package
    (name "ghc-geniplate-mirror")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package"
                           "/geniplate-mirror"
                           "/geniplate-mirror-" version ".tar.gz"))
       (sha256
        (base32 "17vjps2118s5z3k39ij00lkmkxv3mqf8h59wv6qdamlgmhyr36si"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-mtl" ,ghc-mtl)))
    (home-page "https://github.com/danr/geniplate")
    (synopsis "Use Template Haskell to generate Uniplate-like functions")
    (description
     "Use Template Haskell to generate Uniplate-like functions.  This is a
maintained mirror of the @uref{https://hackage.haskell.org/package/geniplate,
geniplate} package, written by Lennart Augustsson.")
    (license license:bsd-3)))

(define-public ghc-gitrev
  (package
    (name "ghc-gitrev")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/gitrev/gitrev-"
                           version ".tar.gz"))
       (sha256
        (base32 "0cl3lfm6k1h8fxp2vxa6ihfp4v8igkz9h35iwyq2frzm4kdn96d8"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-base-compat" ,ghc-base-compat)))
    (home-page "https://github.com/acfoltzer/gitrev")
    (synopsis "Compile git revision info into Haskell projects")
    (description
     "This package provides some handy Template Haskell splices for including
the current git hash and branch in the code of your project.  This is useful
for including in panic messages, @command{--version} output, or diagnostic
info for more informative bug reports.")
    (license license:bsd-3)))

(define-public ghc-haskell-src-meta
  (package
    (name "ghc-haskell-src-meta")
    (version "0.8.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "haskell-src-meta/haskell-src-meta-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "12rc4v5dbbbcwdp7j8isvnm9vqpazv124j5kdfwlgwgwjhxi8ysb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-syb" ,ghc-syb)
       ("ghc-th-orphans" ,ghc-th-orphans)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://hackage.haskell.org/package/haskell-src-meta")
    (synopsis "Parse source to template-haskell abstract syntax")
    (description
     "This package provides tools to parse Haskell sources to the
template-haskell abstract syntax.")
    (license license:bsd-3)))

(define-public ghc-conduit
  (package
    (name "ghc-conduit")
    (version "1.3.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "conduit/conduit-" version ".tar.gz"))
              (sha256
               (base32
                "1sangm0qqi9dzlq95746a3kl14k8b09592a423shxjf2a0b1yx5v"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-mono-traversable" ,ghc-mono-traversable)
       ("ghc-mmorph" ,ghc-mmorph)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-silently" ,ghc-silently)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unliftio-core" ,ghc-unliftio-core)
       ("ghc-vector" ,ghc-vector)
       ("ghc-void" ,ghc-void)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-safe" ,ghc-safe)
       ("ghc-split" ,ghc-split)))
    (home-page "https://github.com/snoyberg/conduit")
    (synopsis "Streaming data library ")
    (description
     "The conduit package is a solution to the streaming data problem,
allowing for production, transformation, and consumption of streams of data
in constant memory.  It is an alternative to lazy I/O which guarantees
deterministic resource handling, and fits in the same general solution
space as enumerator/iteratee and pipes.")
    (license license:expat)))

(define-public ghc-logging-facade
  (package
    (name "ghc-logging-facade")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "logging-facade/logging-facade-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "18ldv6rsff480rqpbs3iabjpvn1fhw0i2a0g80jnhxg9ajfz5yb0"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/logging-facade")
    (synopsis "Simple logging abstraction that allows multiple back-ends")
    (description
     "This package provides a simple logging abstraction that allows multiple
back-ends.")
    (license license:expat)))

(define-public ghc-mockery
  (package
    (name "ghc-mockery")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "mockery/mockery-" version ".tar.gz"))
              (sha256
               (base32
                "1m7sq2vclgir3qbpngzl3g87ks4034blwwf7p3h02c0jlcwpl5b1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-temporary" ,ghc-temporary)
       ("ghc-logging-facade" ,ghc-logging-facade)
       ("ghc-base-compat" ,ghc-base-compat)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/mockery")
    (synopsis "Support functions for automated testing")
    (description
     "The mockery package provides support functions for automated testing.")
    (license license:expat)))

(define-public ghc-yaml
  (package
    (name "ghc-yaml")
    (version "0.8.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "yaml/yaml-" version ".tar.gz"))
              (sha256
               (base32
                "0swgkzkfrwj0ac7lssn8rnrdfmh3lcsdn5fbq2iwv55di6jbc0pp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-text" ,ghc-text)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-enclosed-exceptions" ,ghc-enclosed-exceptions)
       ("ghc-base-compat" ,ghc-base-compat)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)
       ("hspec-discover" ,hspec-discover)
       ("ghc-mockery" ,ghc-mockery)))
    (home-page "https://github.com/snoyberg/yaml/")
    (synopsis "Parsing and rendering YAML documents")
    (description
     "This package provides a library to parse and render YAML documents.")
    (license license:bsd-3)))

(define-public ghc-yaml-for-pandoc-1
  (package (inherit ghc-yaml)
    (inputs
     `(("ghc-aeson" ,ghc-aeson-for-pandoc-1)
       ,@(alist-delete "ghc-aeson" (package-inputs ghc-yaml))))))

(define-public ghc-filemanip
  (package
    (name "ghc-filemanip")
    (version "0.3.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "filemanip/filemanip-" version ".tar.gz"))
              (sha256
               (base32
                "0ilqr8jv41zxcj5qyicg29m8s30b9v70x6f9h2h2rw5ap8bxldl8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-unix-compat" ,ghc-unix-compat)))
    (home-page "https://github.com/bos/filemanip")
    (synopsis "File and directory manipulation for Haskell")
    (description
     "This package provides a Haskell library for working with files and
directories.  It includes code for pattern matching, finding files, modifying
file contents, and more.")
    (license license:bsd-3)))

(define-public ghc-mmap
  (package
    (name "ghc-mmap")
    (version "0.5.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "mmap/mmap-" version ".tar.gz"))
              (sha256
               (base32
                "1y5mk3yf4b8r6rzmlx1xqn4skaigrqnv08sqq0v7r3nbw42bpz2q"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/mmap")
    (synopsis "Memory mapped files for Haskell")
    (description
     "This library provides a wrapper to @code{mmap}, allowing files or
devices to be lazily loaded into memory as strict or lazy @code{ByteStrings},
@code{ForeignPtrs} or plain @code{Ptrs}, using the virtual memory subsystem to
do on-demand loading.")
    (license license:bsd-3)))

(define-public ghc-juicypixels
  (package
    (name "ghc-juicypixels")
    (version "3.2.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "JuicyPixels/JuicyPixels-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14s57fgf6kd5n5al2kcvk1aaxbq1ph0r5h8blflrjkx83yl6r8yn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-zlib" ,ghc-zlib)
       ("ghc-vector" ,ghc-vector)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-mmap" ,ghc-mmap)))
    (home-page "https://github.com/Twinside/Juicy.Pixels")
    (synopsis "Picture loading and serialization library")
    (description
     "This library can load and store images in PNG, Bitmap, JPEG, Radiance,
TIFF and GIF formats.")
    (license license:bsd-3)))

(define-public ghc-hslua
  (package
    (name "ghc-hslua")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "hslua/hslua-" version ".tar.gz"))
              (sha256
               (base32
                "1j2zk7f7nyywg2b0n6kb2yf6ljc7cn2sk9jz0h76g3ag2b70l12n"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags '("-fsystem-lua")))
    (inputs
     `(("lua" ,lua)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-fail" ,ghc-fail)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-expected-failure" ,ghc-tasty-expected-failure)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)))
    (home-page "https://hackage.haskell.org/package/hslua")
    (synopsis "Lua language interpreter embedding in Haskell")
    (description
     "The Scripting.Lua module is a wrapper of the Lua language interpreter as
described in @url{https://www.lua.org/}.")
    (license license:expat)))

(define-public ghc-hslua-for-pandoc-1
  (package (inherit ghc-hslua)
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "hslua/hslua-" version ".tar.gz"))
              (sha256
               (base32
                "0gqp6qhp4v24kzv2j49kgk7bxqzw0w10x0zr0r2j9wkfavqb9wid"))))
    (inputs
     `(("lua" ,lua-5.1)
       ("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec-contrib" ,ghc-hspec-contrib)
       ("ghc-hunit" ,ghc-hunit)
       ("hspec-discover" ,hspec-discover)))))

(define-public ghc-hslua-module-text
  (package
    (name "ghc-hslua-module-text")
    (version "0.1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hslua-module-text/hslua-module-text-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0bcfpb1dhnxp0gr376ai4w7vczr9zrjl1r3r6w7kcxivfkwq9cxf"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hslua" ,ghc-hslua)
       ("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/hslua/hslua-module-text")
    (synopsis "Lua module for text")
    (description
     "This package provides a UTF-8 aware subset of Lua's @code{string} module
for Haskell.  The functions provided by this module are @code{upper},
@code{lower}, @code{len}, @code{reverse}, and @code{sub}.")
    (license license:expat)))

(define-public ghc-byteable
  (package
    (name "ghc-byteable")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "byteable/byteable-" version ".tar.gz"))
              (sha256
               (base32
                "1qizg0kxxjqnd3cbrjhhidk5pbbciz0pb3z5kzikjjxnnnhk8fr4"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/vincenthz/hs-byteable")
    (synopsis "Type class for sequence of bytes")
    (description
     "This package provides an abstract class to manipulate sequence of bytes.
The use case of this class is abstracting manipulation of types that are just
wrapping a bytestring with stronger and more meaniful name.")
    (license license:bsd-3)))

(define-public ghc-hourglass
  (package
    (name "ghc-hourglass")
    (version "0.2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "hourglass/hourglass-" version ".tar.gz"))
              (sha256
               (base32
                "104d1yd84hclprg740nkz60vx589mnm094zriw6zczbgg8nkclym"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-old-locale" ,ghc-old-locale)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/vincenthz/hs-hourglass")
    (synopsis "Simple time-related library for Haskell")
    (description
     "This is a simple time library providing a simple but powerful and
performant API.  The backbone of the library are the @code{Timeable} and
@code{Time} type classes.  Each @code{Timeable} instances can be converted to
a type that has a @code{Time} instances, and thus are different
representations of current time.")
    (license license:bsd-3)))

(define-public ghc-edit-distance
  (package
    (name "ghc-edit-distance")
    (version "0.2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/edit-distance"
                           "/edit-distance-" version ".tar.gz"))
       (sha256
        (base32 "0jkca97zyv23yyilp3jydcrzxqhyk27swhzh82llvban5zp8b21y"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck")))
    (inputs
     `(("ghc-random" ,ghc-random)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/phadej/edit-distance")
    (synopsis "Levenshtein and restricted Damerau-Levenshtein edit distances")
    (description
     "This package provides optimized functions to determine the edit
distances for fuzzy matching, including Levenshtein and restricted
Damerau-Levenshtein algorithms.")
    (license license:bsd-3)))

(define-public ghc-memory
  (package
    (name "ghc-memory")
    (version "0.14.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "memory/memory-" version ".tar.gz"))
              (sha256
               (base32
                "03rbszi5d4z9rlbfv8ydrl1xf84xsh8z57g07f7j9qccn9587c3v"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-basement" ,ghc-basement)
       ("ghc-foundation" ,ghc-foundation)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/vincenthz/hs-memory")
    (synopsis "Memory abstractions for Haskell")
    (description
     "This package provides memory abstractions, such as chunk of memory,
polymorphic byte array management and manipulation functions.  It contains a
polymorphic byte array abstraction and functions similar to strict ByteString,
different type of byte array abstraction, raw memory IO operations (memory
set, memory copy, ..) and more")
    (license license:bsd-3)))

(define-public ghc-socks
  (package
    (name "ghc-socks")
    (version "0.5.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "socks/socks-" version ".tar.gz"))
              (sha256
               (base32
                "0s689w1hh9g8ifl75xhzbv96ir07hwn04b4lgvbxzl8swa9ylir6"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-cereal" ,ghc-cereal)
       ("ghc-network" ,ghc-network)))
    (home-page "https://github.com/vincenthz/hs-socks")
    (synopsis "SOCKS proxy (version 5) implementation")
    (description
     "This library provides a SOCKS proxy (version 5) implementation.")
    (license license:bsd-3)))

(define-public ghc-connection
  (package
    (name "ghc-connection")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "connection/connection-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1c1prsgad669cmf6qrqlb5hmh0dnam2imijqzpwcr4ja14l6rh83"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-byteable" ,ghc-byteable)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-network" ,ghc-network)
       ("ghc-tls" ,ghc-tls)
       ("ghc-socks" ,ghc-socks)
       ("ghc-x509" ,ghc-x509)
       ("ghc-x509-store" ,ghc-x509-store)
       ("ghc-x509-system" ,ghc-x509-system)
       ("ghc-x509-validation" ,ghc-x509-validation)))
    (home-page "https://github.com/vincenthz/hs-connection")
    (synopsis "Simple and easy network connections API")
    (description
     "This package provides a simple network library for all your connection
needs.  It provides a very simple API to create sockets to a destination with
the choice of SSL/TLS, and SOCKS.")
    (license license:bsd-3)))

(define-public ghc-skylighting
  (package
    (name "ghc-skylighting")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/skylighting-"
                                  version "/skylighting-" version ".tar.gz"))
              (sha256
               (base32
                "0l5lhhqqlfaq1fs7pn3n3b25kmazk8p4ahwvhagbrhcbm5hsigdg"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-colour" ,ghc-colour)
       ("ghc-diff" ,ghc-diff)
       ("ghc-hxt" ,ghc-hxt)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-pretty-show" ,ghc-pretty-show)
       ("ghc-regex-pcre-builtin" ,ghc-regex-pcre-builtin)
       ("ghc-safe" ,ghc-safe)
       ("ghc-text" ,ghc-text)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-golden" ,ghc-tasty-golden)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://hackage.haskell.org/package/skylighting")
    (synopsis "Syntax highlighting library")
    (description "Skylighting is a syntax highlighting library with support
for over one hundred languages.  It derives its tokenizers from XML syntax
definitions used by KDE's @code{KSyntaxHighlighting} framework, so any syntax
supported by that framework can be added.  An optional command-line program is
provided.  Skylighting is intended to be the successor to highlighting-kate.")
    (license license:gpl2)))

(define-public ghc-skylighting-for-pandoc-1
  (package (inherit ghc-skylighting)
    (version "0.1.1.5")
    (source (origin
              (method git-fetch)
              ;; We take the sources from Github, because the tarball on
              ;; hackage does not include the XML files.
              (uri (git-reference
                    (url "https://github.com/jgm/skylighting.git")
                    (commit version)))
              (file-name (string-append "ghc-skylighting-" version "-checkout"))
              (sha256
               (base32
                "0z3yv8v2fqqgv6lsf0ff3ld0h2vkg97b2jiry9wn2f1rizwdqmzl"))))
    (arguments
     `(#:configure-flags '("-fbootstrap")
       #:phases
       (modify-phases %standard-phases
         ;; After building the skylighting-extract tool we use it to generate
         ;; syntax source files from the included XML files.  These are then
         ;; added to the skylighting.cabal file.
         (add-after 'build 'extract-xml
           (lambda _
             (make-file-writable "skylighting.cabal")
             (apply invoke "./dist/build/skylighting-extract/skylighting-extract"
                    (find-files "xml" "\\.xml$"))
             #t))
         ;; Reconfigure without bootstrap flag
         (add-after 'extract-xml 'configure-again
           (lambda* (#:key outputs inputs tests? #:allow-other-keys)
             ((assoc-ref %standard-phases 'configure)
              #:outputs outputs
              #:inputs inputs
              #:tests? tests?
              #:configure-flags '("-f-bootstrap"))))
         (add-after 'configure-again 'build-again
           (assoc-ref %standard-phases 'build)))))
    (inputs
     `(("ghc-aeson" ,ghc-aeson-for-pandoc-1)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-diff" ,ghc-diff)
       ("ghc-hxt" ,ghc-hxt)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-pretty-show" ,ghc-pretty-show)
       ("ghc-regex-pcre-builtin" ,ghc-regex-pcre-builtin)
       ("ghc-safe" ,ghc-safe)
       ("ghc-text" ,ghc-text)
       ("ghc-utf8-string" ,ghc-utf8-string)))))

(define-public ghc-doctemplates
  (package
    (name "ghc-doctemplates")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "doctemplates/doctemplates-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1bfs2kl5j5al2w2y4qpbn68p0xsnb65r7h51l356kpkzc326646p"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-text" ,ghc-text)
       ("ghc-vector" ,ghc-vector)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-scientific" ,ghc-scientific)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/jgm/doctemplates#readme")
    (synopsis "Pandoc-style document templates")
    (description
     "This package provides a simple text templating system used by pandoc.")
    (license license:bsd-3)))

(define-public ghc-doctemplates-for-pandoc-1
  (package (inherit ghc-doctemplates)
    (version "0.1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "doctemplates/doctemplates-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0swal6rjya1293mwvl63jch5fx9ghpsil7qs4v7rpansa0izalmp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson-for-pandoc-1)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-text" ,ghc-text)
       ("ghc-vector" ,ghc-vector)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-scientific" ,ghc-scientific)))))

(define-public ghc-pandoc
  (package
    (name "ghc-pandoc")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/pandoc/pandoc-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1dqin92w513l7whg5wdgrngnxsj5mb8gppfvn7kjgyv2pdgpy0zy"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-aeson-pretty" ,ghc-aeson-pretty)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-cmark-gfm" ,ghc-cmark-gfm)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-deepseq-generics" ,ghc-deepseq-generics)
       ("ghc-diff" ,ghc-diff)
       ("ghc-doctemplates" ,ghc-doctemplates)
       ("ghc-executable-path" ,ghc-executable-path)
       ("ghc-glob" ,ghc-glob)
       ("ghc-haddock-library" ,ghc-haddock-library)
       ("ghc-hslua" ,ghc-hslua)
       ("ghc-hslua-module-text" ,ghc-hslua-module-text)
       ("ghc-http" ,ghc-http)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-pandoc-types" ,ghc-pandoc-types)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-random" ,ghc-random)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-sha" ,ghc-sha)
       ("ghc-skylighting" ,ghc-skylighting)
       ("ghc-split" ,ghc-split)
       ("ghc-syb" ,ghc-syb)
       ("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-texmath" ,ghc-texmath)
       ("ghc-text" ,ghc-text)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-xml" ,ghc-xml)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-zip-archive" ,ghc-zip-archive)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-golden" ,ghc-tasty-golden)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://pandoc.org")
    (synopsis "Conversion between markup formats")
    (description
     "Pandoc is a Haskell library for converting from one markup format to
another, and a command-line tool that uses this library.  It can read and
write Markdown and (subsets of) other formats, such as HTML, reStructuredText,
LaTeX, DocBook, and many more.

Pandoc extends standard Markdown syntax with footnotes, embedded LaTeX,
definition lists, tables, and other features.  A compatibility mode is
provided for those who need a drop-in replacement for Markdown.pl.")
    (license license:gpl2+)))

;; This is the last version of Pandoc 1.x, which is preferred for Rmarkdown.
(define-public ghc-pandoc-1
  (package (inherit ghc-pandoc)
    (version "1.19.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/pandoc/pandoc-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0mim429mpakrcnm50csxyqk3ljcx2l26r5grk6w9isnggwgqrq5v"))))
    (arguments
     `(#:configure-flags (list "--allow-newer=skylighting")))
    (inputs
     `(("ghc-aeson" ,ghc-aeson-for-pandoc-1)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-cmark" ,ghc-cmark)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-deepseq-generics" ,ghc-deepseq-generics)
       ("ghc-diff" ,ghc-diff)
       ("ghc-doctemplates" ,ghc-doctemplates-for-pandoc-1)
       ("ghc-executable-path" ,ghc-executable-path)
       ("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
       ("ghc-filemanip" ,ghc-filemanip)
       ("ghc-haddock-library" ,ghc-haddock-library)
       ("ghc-hslua" ,ghc-hslua-for-pandoc-1)
       ("ghc-http" ,ghc-http)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-pandoc-types" ,ghc-pandoc-types-for-pandoc-1)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-random" ,ghc-random)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-sha" ,ghc-sha)
       ("ghc-skylighting" ,ghc-skylighting-for-pandoc-1)
       ("ghc-syb" ,ghc-syb)
       ("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-texmath" ,ghc-texmath-for-pandoc-1)
       ("ghc-text" ,ghc-text)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-xml" ,ghc-xml)
       ("ghc-yaml" ,ghc-yaml-for-pandoc-1)
       ("ghc-zip-archive" ,ghc-zip-archive)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))))

(define-public ghc-hs-bibutils
  (package
    (name "ghc-hs-bibutils")
    (version "6.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hs-bibutils/hs-bibutils-"
             version ".tar.gz"))
       (sha256
        (base32
         "0c56sjgg82kjcg5rrplnyn7vf02ccsj3kkcbgc87zxlv0j880rjb"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-syb" ,ghc-syb)))
    (home-page "https://hackage.haskell.org/package/hs-bibutils")
    (synopsis "Haskell bindings to bibutils")
    (description
     "This package provides Haskell bindings to @code{bibutils}, a library
that interconverts between various bibliography formats using a common
MODS-format XML intermediate.")
    (license license:gpl2+)))

(define-public ghc-rfc5051
  (package
    (name "ghc-rfc5051")
    (version "0.1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/rfc5051/"
                           "rfc5051-" version ".tar.gz"))
       (sha256
        (base32
         "0av4c3qvwbkbzrjrrg601ay9pds7wscqqp2lc2z78mv2lllap3g3"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/rfc5051")
    (synopsis "Simple unicode collation as per RFC5051")
    (description
     "This library implements @code{unicode-casemap}, the simple, non
locale-sensitive unicode collation algorithm described in RFC 5051.  Proper
unicode collation can be done using @code{text-icu}, but that is a big
dependency that depends on a large C library, and @code{rfc5051} might be
better for some purposes.")
    (license license:bsd-3)))

(define-public ghc-typed-process
  (package
    (name "ghc-typed-process")
    (version "0.2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "typed-process/typed-process-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0c6gvgvjyncbni9a5bvpbglknd4yclr3d3hfg9bhgahmkj40dva2"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-stm" ,ghc-stm)))
    (native-inputs
     `(("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://haskell-lang.org/library/typed-process")
    (synopsis "Run external processes with strong typing of streams")
    (description
     "This library provides the ability to launch and interact with external
processes.  It wraps around the @code{process} library, and intends to improve
upon it.")
    (license license:expat)))

(define-public ghc-conduit-extra
  (package
    (name "ghc-conduit-extra")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "conduit-extra/conduit-extra-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1bi2b6kdzy5f9glq46jzsk02has95jkxqz0cchpbmnakzhjwjh9c"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-text" ,ghc-text)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-typed-process" ,ghc-typed-process)
       ("ghc-async" ,ghc-async)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-network" ,ghc-network)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-stm" ,ghc-stm)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/snoyberg/conduit")
    (synopsis "Conduit adapters for common libraries")
    (description
     "The @code{conduit} package itself maintains relative small dependencies.
The purpose of this package is to collect commonly used utility functions
wrapping other library dependencies, without depending on heavier-weight
dependencies.  The basic idea is that this package should only depend on
@code{haskell-platform} packages and @code{conduit}.")
    (license license:expat)))

(define-public ghc-xml-types
  (package
    (name "ghc-xml-types")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/xml-types/"
                           "xml-types-" version ".tar.gz"))
       (sha256
        (base32
         "1jgqxsa9p2q3h6nymbfmvhldqrqlwrhrzmwadlyc0li50x0d8dwr"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-text" ,ghc-text)))
    (home-page "https://john-millikin.com/software/haskell-xml/")
    (synopsis "Basic types for representing XML")
    (description "This package provides basic types for representing XML
documents.")
    (license license:expat)))

(define-public ghc-xml-conduit
  (package
    (name "ghc-xml-conduit")
    (version "1.7.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/xml-conduit/"
                           "xml-conduit-" version ".tar.gz"))
       (sha256
        (base32
         "0n4k0rq9j5cc9kdvj9xbx8gmiqlyk5x6pw8yxzw5wfsw7qkych2s"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-text" ,ghc-text)
       ("ghc-xml-types" ,ghc-xml-types)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/snoyberg/xml")
    (synopsis "Utilities for dealing with XML with the conduit package")
    (description
     "This package provides pure-Haskell utilities for dealing with XML with
the @code{conduit} package.")
    (license license:expat)))

(define-public ghc-pandoc-citeproc
  (package
    (name "ghc-pandoc-citeproc")
    (version "0.12.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "pandoc-citeproc/pandoc-citeproc-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1l58nbflcnlznc93qimkk7ghk2gv8kipf45zf88piqa2zys41yyx"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tests need to be run after installation.
         (delete 'check)
         (add-after 'install 'post-install-check
           (assoc-ref %standard-phases 'check)))))
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-pandoc-types" ,ghc-pandoc-types)
       ("ghc-pandoc" ,ghc-pandoc)
       ("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-text" ,ghc-text)
       ("ghc-vector" ,ghc-vector)
       ("ghc-xml-conduit" ,ghc-xml-conduit)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-setenv" ,ghc-setenv)
       ("ghc-split" ,ghc-split)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-hs-bibutils" ,ghc-hs-bibutils)
       ("ghc-rfc5051" ,ghc-rfc5051)
       ("ghc-syb" ,ghc-syb)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-aeson-pretty" ,ghc-aeson-pretty)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/jgm/pandoc-citeproc")
    (synopsis "Library for using pandoc with citeproc")
    (description
     "The @code{pandoc-citeproc} library exports functions for using the
citeproc system with pandoc.  It relies on @code{citeproc-hs}, a library for
rendering bibliographic reference citations into a variety of styles using a
macro language called @dfn{Citation Style Language} (CSL).  This package also
contains an executable @code{pandoc-citeproc}, which works as a pandoc filter,
and also has a mode for converting bibliographic databases a YAML format
suitable for inclusion in pandoc YAML metadata.")
    (license license:bsd-3)))

(define-public ghc-pandoc-citeproc-with-pandoc-1
  (let ((for-pandoc-1
         (package-input-rewriting
          `((,ghc-aeson        . ,ghc-aeson-for-pandoc-1)
            (,ghc-yaml         . ,ghc-yaml-for-pandoc-1)
            (,ghc-texmath      . ,ghc-texmath-for-pandoc-1)
            (,ghc-pandoc-types . ,ghc-pandoc-types-for-pandoc-1)
            (,ghc-hslua        . ,ghc-hslua-for-pandoc-1)
            (,ghc-skylighting  . ,ghc-skylighting-for-pandoc-1)
            (,ghc-doctemplates . ,ghc-doctemplates-for-pandoc-1)
            (,ghc-pandoc       . ,ghc-pandoc-1)))))
    (for-pandoc-1 ghc-pandoc-citeproc)))

(define-public ghc-union-find
  (package
    (name "ghc-union-find")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/union-find/union-find-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1v7hj42j9w6jlzi56jg8rh4p58gfs1c5dx30wd1qqvn0p0mnihp6"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/nominolo/union-find")
    (synopsis "Efficient union and equivalence testing of sets")
    (description
     "The Union/Find algorithm implements these operations in (effectively)
constant-time:
@enumerate
@item Check whether two elements are in the same equivalence class.
@item Create a union of two equivalence classes.
@item Look up the descriptor of the equivalence class.
@end enumerate\n")
    (license license:bsd-3)))

(define-public ghc-base16-bytestring
  (package
    (name "ghc-base16-bytestring")
    (version "0.1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/base16-bytestring/"
             "base16-bytestring-" version ".tar.gz"))
       (sha256
        (base32
         "0jf40m3yijqw6wd1rwwvviww46fasphaay9m9rgqyhf5aahnbzjs"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/bos/base16-bytestring")
    (synopsis "Fast base16 (hex) encoding and decoding for ByteStrings")
    (description
     "This package provides a Haskell library for working with base16-encoded
data quickly and efficiently, using the ByteString type.")
    (license license:bsd-3)))

(define-public ghc-data-ordlist
  (package
    (name "ghc-data-ordlist")
    (version "0.4.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/data-ordlist/data-ordlist-"
             version ".tar.gz"))
       (sha256
        (base32
         "03a9ix1fcx08viwv2jg5ndw1qbkydyyrmjvqr9wasmcik9x1wv3g"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/data-ordlist")
    (synopsis "Set and bag operations on ordered lists")
    (description
     "This module provides set and multiset operations on ordered lists.")
    (license license:bsd-3)))

(define-public ghc-regex-applicative
  (package
    (name "ghc-regex-applicative")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-applicative/"
             "regex-applicative-" version ".tar.gz"))
       (sha256
        (base32
         "1riv7jqf26lbv4rm54sd6mrx8xdh4dvh4xbzymzdfdw13k6a4nb6"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/feuerbach/regex-applicative")
    (synopsis "Regex-based parsing with applicative interface")
    (description
     "@code{regex-applicative} is a Haskell library for parsing using
regular expressions.  Parsers can be built using Applicative interface.")
    (license license:expat)))

(define-public ghc-regex-tdfa
  (package
    (name "ghc-regex-tdfa")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-tdfa/regex-tdfa-"
             version ".tar.gz"))
       (sha256
        (base32
         "0f8x8wyr6m21g8dnxvnvalz5bsq37l125l6qhs0fscbvprsxc4nb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-regex-base" ,ghc-regex-base)))
    (home-page "https://github.com/ChrisKuklewicz/regex-tdfa")
    (synopsis "POSIX extended regular expressions in Haskell.")
    (description
     "Regex-tdfa is a pure Haskell regular expression library implementing POSIX
extended regular expressions.  It is a \"tagged\" DFA regex engine. It is
inspired by libtre.")
    (license license:bsd-3)))

(define-public ghc-regex-compat-tdfa
  (package
    (name "ghc-regex-compat-tdfa")
    (version "0.95.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-compat-tdfa/regex-compat-tdfa-"
             version ".tar.gz"))
       (sha256
        (base32
         "1p90fn90yhp7fvljjdqjp41cszidcfz4pw7fwvzyx4739b98x8sg"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)))
    (home-page "https://hub.darcs.net/shelarcy/regex-compat-tdfa")
    (synopsis "Unicode Support version of Text.Regex, using regex-tdfa")
    (description
     "One module layer over @code{regex-tdfa} to replace @code{Text.Regex}.
@code{regex-compat} can't use Unicode characters correctly because of using regex-posix.
This is not good for Unicode users.  This modified regex-compat uses regex-tdfa to solve
this problem.")
    (license license:bsd-3)))

(define-public ghc-sandi
  (package
    (name "ghc-sandi")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/sandi/sandi-"
             version ".tar.gz"))
       (sha256
        (base32
         "0dvkpk91n9kz2ha04rvp231ra9sgd1ilyc1qkzf9l03iir7zrh9b"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stringsearch" ,ghc-stringsearch)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-th" ,ghc-tasty-th)))
    (home-page "https://hackage.haskell.org/package/sandi")
    (synopsis "Data encoding library")
    (description "Reasonably fast data encoding library.")
    (license license:bsd-3)))

(define-public ghc-bytestring-handle
  (package
    (name "ghc-bytestring-handle")
    (version "0.1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/bytestring-handle/bytestring-handle-"
             version ".tar.gz"))
       (sha256
        (base32
         "18f17aja1ivhr3zyg2cccn2m03hdn5jf5410dndkhf12gvgiqs7y"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hub.darcs.net/ganesh/bytestring-handle")
    (synopsis "ByteString-backed Handles")
    (description "ByteString-backed Handles") ; There is no description
    (license license:bsd-3)))

(define-public ghc-tar
  (package
    (name "ghc-tar")
    (version "0.5.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tar/tar-"
             version ".tar.gz"))
       (sha256
        (base32
         "18qq94j9bm91iswnxq2dm5dws5c7wm4k01q2rpf8py35cf3svnfq"))))
    (build-system haskell-build-system)
    ;; FIXME: 2/24 tests fail.
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-bytestring-handle" ,ghc-bytestring-handle)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://hackage.haskell.org/package/tar")
    (synopsis "Reading, writing and manipulating \".tar\" archive files")
    (description
     "This library is for working with \\\"@.tar@\\\" archive files.
It can read and write a range of common variations of the tar archive format
including V7, POSIX USTAR and GNU formats.  It provides support for packing and
unpacking portable archives.  This makes it suitable for distribution but not
backup because details like file ownership and exact permissions are not
preserved.  It also provides features for random access to archive content using
an index.")
    (license license:bsd-3)))

(define-public ghc-stmonadtrans
  (package
    (name "ghc-stmonadtrans")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/STMonadTrans"
                           "/STMonadTrans-" version ".tar.gz"))
       (sha256
        (base32 "1nr26fnmi5fdjc6d00w13kjhmfyvb5b837d0006w4dj0yxndaksp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)))
    (home-page "https://hackage.haskell.org/package/STMonadTrans")
    (synopsis "Monad transformer version of the ST monad")
    (description
     "This package provides a monad transformer version of the @code{ST} monad
for strict state threads.")
    (license license:bsd-3)))

(define-public ghc-findbin
  (package
    (name "ghc-findbin")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/FindBin/FindBin-"
             version ".tar.gz"))
       (sha256
        (base32
         "197xvn05yysmibm1p5wzxfa256lvpbknr5d1l2ws6g40w1kpk717"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/audreyt/findbin")
    (synopsis "Get the absolute path of the running program")
    (description
     "This module locates the full directory of the running program, to allow
the use of paths relative to it.  FindBin supports invocation of Haskell
programs via \"ghci\", via \"runhaskell/runghc\", as well as compiled as
an executable.")
    (license license:bsd-3)))

(define-public ghc-patience
  (package
    (name "ghc-patience")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/patience/patience-"
             version ".tar.gz"))
       (sha256
        (base32
         "0qyv20gqy9pb1acy700ahv70lc6vprcwb26cc7fcpcs4scsc7irm"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/patience")
    (synopsis "Patience diff and longest increasing subsequence")
    (description
     "This library implements the 'patience diff' algorithm, as well as the
patience algorithm for the longest increasing subsequence problem.
Patience diff computes the difference between two lists, for example the lines
of two versions of a source file.  It provides a good balance between
performance, nice output for humans, and simplicity of implementation.")
    (license license:bsd-3)))

(define-public ghc-monads-tf
  (package
    (name "ghc-monads-tf")
    (version "0.1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/monads-tf/monads-tf-"
             version ".tar.gz"))
       (sha256
        (base32
         "1wdhskwa6dw8qljbvwpyxj8ca6y95q2np7z4y4q6bpf4anmd5794"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/monads-tf")
    (synopsis "Monad classes, using type families")
    (description
     "Monad classes using type families, with instances for various monad transformers,
inspired by the paper 'Functional Programming with Overloading and Higher-Order
Polymorphism', by Mark P Jones.  This package is almost a compatible replacement for
the @code{mtl-tf} package.")
    (license license:bsd-3)))

(define-public ghc-colour
(package
  (name "ghc-colour")
  (version "2.3.3")
  (source
   (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/colour/colour-"
           version ".tar.gz"))
     (sha256
      (base32
       "1qmn1778xzg07jg9nx4k1spdz2llivpblf6wwrps1qpqjhsac5cd"))))
  (build-system haskell-build-system)
  (home-page "https://www.haskell.org/haskellwiki/Colour")
  (synopsis "Model for human colour perception")
  (description
   "This package provides a data type for colours and transparency.
Colours can be blended and composed.  Various colour spaces are
supported.  A module of colour names (\"Data.Colour.Names\") is provided.")
  (license license:expat)))

(define-public ghc-wl-pprint-text
  (package
    (name "ghc-wl-pprint-text")
    (version "1.2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wl-pprint-text/wl-pprint-text-"
             version ".tar.gz"))
       (sha256
        (base32
         "0g3w92rad6x5appfb22rbzcas2ix2h0hy91sdxhq8a4a5cnlrpa0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-text" ,ghc-text)))
    (home-page "https://hackage.haskell.org/package/wl-pprint-text")
    (synopsis "Wadler/Leijen Pretty Printer for Text values")
    (description
     "A clone of wl-pprint for use with the text library.")
    (license license:bsd-3)))

(define-public ghc-fgl-arbitrary
  (package
    (name "ghc-fgl-arbitrary")
    (version "0.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/fgl-arbitrary/fgl-arbitrary-"
             version ".tar.gz"))
       (sha256
        (base32
         "0ln1szgfy8fa78l3issq4fx3aqnnd54w3cb4wssrfi48vd5rkfjm"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck"
                               "--allow-newer=hspec")))
    (inputs
     `(("ghc-fgl" ,ghc-fgl)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://hackage.haskell.org/package/fgl-arbitrary")
    (synopsis "QuickCheck support for fgl")
    (description
     "Provides Arbitrary instances for fgl graphs to avoid adding a
QuickCheck dependency for fgl whilst still making the instances
available to others.  Also available are non-fgl-specific functions
for generating graph-like data structures.")
    (license license:bsd-3)))

(define-public ghc-graphviz
  (package
    (name "ghc-graphviz")
    (version "2999.20.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "graphviz/graphviz-" version ".tar.gz"))
              (sha256
               (base32
                "0kj7ap0gnliviq2p8lscw1m06capnsa90vpvcys24nqy5nw2wrp7"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck")))
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-colour" ,ghc-colour)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-fgl" ,ghc-fgl)
       ("ghc-fgl-arbitrary" ,ghc-fgl-arbitrary)
       ("ghc-polyparse" ,ghc-polyparse)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-text" ,ghc-text)
       ("ghc-wl-pprint-text" ,ghc-wl-pprint-text)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("graphviz" ,graphviz)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/graphviz")
    (synopsis "Bindings to Graphviz for graph visualisation")
    (description
     "This library provides bindings for the Dot language used by
the @uref{https://graphviz.org/, Graphviz} suite of programs for
visualising graphs, as well as functions to call those programs.
Main features of the graphviz library include:

@enumerate
@item Almost complete coverage of all Graphviz attributes and syntax
@item Support for specifying clusters
@item The ability to use a custom node type
@item Functions for running a Graphviz layout tool with all specified output types
@item Generate and parse Dot code with two options: strict and liberal
@item Functions to convert FGL graphs and other graph-like data structures
@item Round-trip support for passing an FGL graph through Graphviz to augment node
and edge labels with positional information, etc.
@end enumerate\n")
    (license license:bsd-3)))

(define-public ghc-constraints
  (package
    (name "ghc-constraints")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/constraints/constraints-"
             version ".tar.gz"))
       (sha256
        (base32
         "120mmv9rwbahslisc1z8zx9lw7v6hl5fzid4l0hiy5as6ijqgl2c"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hashable" ,ghc-hashable)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/ekmett/constraints/")
    (synopsis "Constraint manipulation")
    (description
     "GHC 7.4 gave us the ability to talk about @code{ConstraintKinds}.
They stopped crashing the compiler in GHC 7.6.  This package provides
a vocabulary for working with them.")
    (license license:bsd-3)))

(define-public ghc-lifted-async
  (package
    (name "ghc-lifted-async")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/lifted-async/lifted-async-"
             version ".tar.gz"))
       (sha256
        (base32
         "00fnwfcnc6niq9jbbb9rap9rkdgv5qhcglwanzc5fi8834j94c1r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-constraints" ,ghc-constraints)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-th" ,ghc-tasty-th)))
    (home-page "https://github.com/maoe/lifted-async")
    (synopsis "Run lifted IO operations asynchronously and wait for their results")
    (description
     "This package provides IO operations from @code{async} package lifted to any
instance of @code{MonadBase} or @code{MonadBaseControl}.")
    (license license:bsd-3)))

;; Ghc-shelly depends on ghc-system-filepath and ghc-system-fileio, who in turn depend on
;; ghc-chell and ghc-chell-quickcheck for the test phase. Ghc-chell depends on ghc-options
;; which depends on ghc-chell and ghc-chell-quickcheck.
;; Therefore we bootstrap it with tests disabled.
(define ghc-system-filepath-bootstrap
  (package
    (name "ghc-system-filepath-bootstrap")
    (version "0.4.13.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/system-filepath/system-filepath-"
             version ".tar.gz"))
       (sha256
        (base32
         "1yy5zsmmimhg6iaw9fmpwrxvxrgi5s6bfyqfihdsnx4bjvn7sp9l"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "High-level, byte-based file and directory path manipulations")
    (description
     "Provides a FilePath datatype and utility functions for operating on it.
Unlike the filepath package, this package does not simply reuse String,
increasing type safety.")
    (license license:expat)))

;; See ghc-system-filepath-bootstrap. In addition this package depends on
;; ghc-system-filepath.
(define ghc-system-fileio-bootstrap
  (package
    (name "ghc-system-fileio-bootstrap")
    (version "0.3.16.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/system-fileio/system-fileio-"
             version ".tar.gz"))
       (sha256
        (base32
         "1484hcl27s2qcby8ws5djj11q9bz68bspcifz9h5gii2ndy70x9i"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("ghc-system-filepath-bootstrap" ,ghc-system-filepath-bootstrap)
       ("ghc-text" ,ghc-text)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "Consistent file system interaction across GHC versions")
    (description
     "This is a small wrapper around the directory, unix, and Win32 packages,
for use with system-filepath.  It provides a consistent API to the various
versions of these packages distributed with different versions of GHC.
In particular, this library supports working with POSIX files that have paths
which can't be decoded in the current locale encoding.")
    (license license:expat)))

(define-public ghc-shelly
  (package
    (name "ghc-shelly")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/shelly/shelly-"
             version ".tar.gz"))
       (sha256
        (base32
         "023fbvbqs5gdwm30j5517gbdcc7fvz0md70dgwgpypkskj3i926y"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-system-filepath-bootstrap" ,ghc-system-filepath-bootstrap)
       ("ghc-system-fileio-bootstrap" ,ghc-system-fileio-bootstrap)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-lifted-async" ,ghc-lifted-async)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-enclosed-exceptions" ,ghc-enclosed-exceptions)
       ("ghc-text" ,ghc-text)
       ("ghc-async" ,ghc-async)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec-contrib" ,ghc-hspec-contrib)))
    (home-page "https://github.com/yesodweb/Shelly.hs")
    (synopsis "Shell-like (systems) programming in Haskell")
    (description
     "Shelly provides convenient systems programming in Haskell, similar in
spirit to POSIX shells.  Shelly is originally forked  from the Shellish package.")
    (license license:bsd-3)))

;; See ghc-system-filepath-bootstrap, chell and chell-quickcheck are required for tests.
(define ghc-options-bootstrap
  (package
    (name "ghc-options-bootstrap")
    (version "1.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/options/options-"
             version ".tar.gz"))
       (sha256
        (base32
         "0qjs0v1ny52w51n5582d4z8wy9h6n0zw1xb5dh686ff5wadflgi8"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("ghc-monads-tf" ,ghc-monads-tf)))
    (home-page "https://john-millikin.com/software/haskell-options/")
    (synopsis "Powerful and easy-to-use command-line option parser")
    (description
     "The @code{options} package lets library and application developers
easily work with command-line options.")
    (license license:expat)))

(define-public ghc-chell
  (package
    (name "ghc-chell")
    (version "0.4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/chell/chell-"
             version ".tar.gz"))
       (sha256
        (base32
         "0lb95abzxl4a87nfqxsxpb3a39pd52cci43hcvj8615hyhqvs2jz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-options-bootstrap" ,ghc-options-bootstrap)
       ("ghc-patience" ,ghc-patience)
       ("ghc-random" ,ghc-random)
       ("ghc-text" ,ghc-text)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)))
    (home-page "https://john-millikin.com/software/chell/")
    (synopsis "Simple and intuitive library for automated testing")
    (description
     "Chell is a simple and intuitive library for automated testing.
It natively supports assertion-based testing, and can use companion
libraries such as @code{chell-quickcheck} to support more complex
testing strategies.")
    (license license:expat)))

(define ghc-chell-quickcheck-bootstrap
  (package
    (name "ghc-chell-quickcheck-bootstrap")
    (version "0.2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/chell-quickcheck/"
             "chell-quickcheck-" version ".tar.gz"))
       (sha256
        (base32
         "1iicsys9igx7m7n4l2b8djardmjy2ah5ibzp7kzs758h460fq53a"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-chell" ,ghc-chell)
       ("ghc-random" ,ghc-random)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (arguments
     `(#:tests? #f))
    (home-page "https://john-millikin.com/software/chell/")
    (synopsis "QuickCheck support for the Chell testing library")
    (description "More complex tests for @code{chell}.")
    (license license:expat)))

(define-public ghc-chell-quickcheck
  (package
    (name "ghc-chell-quickcheck")
    (version "0.2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/chell-quickcheck/"
             "chell-quickcheck-" version ".tar.gz"))
       (sha256
        (base32
         "1iicsys9igx7m7n4l2b8djardmjy2ah5ibzp7kzs758h460fq53a"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-chell" ,ghc-chell)
       ("ghc-chell-quickcheck-bootstrap" ,ghc-chell-quickcheck-bootstrap)
       ("ghc-random" ,ghc-random)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://john-millikin.com/software/chell/")
    (synopsis "QuickCheck support for the Chell testing library")
    (description "More complex tests for @code{chell}.")
    (license license:expat)))

(define-public ghc-options
  (package
    (name "ghc-options")
    (version "1.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/options/options-"
             version ".tar.gz"))
       (sha256
        (base32
         "0qjs0v1ny52w51n5582d4z8wy9h6n0zw1xb5dh686ff5wadflgi8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-monads-tf" ,ghc-monads-tf)
       ("ghc-chell" ,ghc-chell)
       ("ghc-chell-quickcheck" ,ghc-chell-quickcheck)))
    (home-page "https://john-millikin.com/software/haskell-options/")
    (synopsis "Powerful and easy-to-use command-line option parser")
    (description
     "The @code{options} package lets library and application developers
easily work with command-line options.")
    (license license:expat)))

(define-public ghc-system-filepath
  (package
    (name "ghc-system-filepath")
    (version "0.4.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/system-filepath/system-filepath-"
             version ".tar.gz"))
       (sha256
        (base32
         "14yras4pz2dh55xpwmazcgxijvi8913pjgzb9iw50mjq1lycwmhn"))))
    (build-system haskell-build-system)
    ;; FIXME: One of the tests fails:
    ;; [ FAIL  ] tests.validity.posix
    ;; note: seed=7310214548328823169
    ;; *** Failed! Falsifiable (after 24 tests):
    ;; FilePath "/r2\ENQ52\t ;$/o\US=/okG\146\&6\n<u\DC3/5\182\223a\DELN\EOT#\NUL/`[m/\USEKV\ETX([)b6/\ACK\SOo\245\ETBO/f\128\STX`|\EM\"/*\EMA\USD3/\143\&4/\CAN?\SUBee\CANR/9/B0\187Kx4/Vqr\232'b:/\a\234\DLE.\"\179/\ENQ{J/|/G)@^\237/\219ml/\DC3pd\ESC"
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-chell" ,ghc-chell)
       ("ghc-chell-quickcheck" ,ghc-chell-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "High-level, byte-based file and directory path manipulations")
    (description
     "Provides a FilePath datatype and utility functions for operating on it.
Unlike the filepath package, this package does not simply reuse String,
increasing type safety.")
    (license license:expat)))

(define-public ghc-system-fileio
  (package
    (name "ghc-system-fileio")
    (version "0.3.16.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/system-fileio/system-fileio-"
             version ".tar.gz"))
       (sha256
        (base32
         "1484hcl27s2qcby8ws5djj11q9bz68bspcifz9h5gii2ndy70x9i"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-system-filepath" ,ghc-system-filepath)
       ("ghc-text" ,ghc-text)
       ("ghc-chell" ,ghc-chell)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "Consistent file system interaction across GHC versions")
    (description
     "This is a small wrapper around the directory, unix, and Win32 packages,
for use with system-filepath.  It provides a consistent API to the various
versions of these packages distributed with different versions of GHC.
In particular, this library supports working with POSIX files that have paths
which can't be decoded in the current locale encoding.")
    (license license:expat)))

(define-public ghc-storable-complex
  (package
    (name "ghc-storable-complex")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/storable-complex/storable-complex-"
             version ".tar.gz"))
       (sha256
        (base32 "01kwwkpbfjrv26vj83cd92px5qbq1bpgxj0r45534aksqhany1xb"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/cartazio/storable-complex")
    (synopsis "Haskell Storable instance for Complex")
    (description "This package provides a Haskell library including a
Storable instance for Complex which is binary compatible with C99, C++
and Fortran complex data types.")
    (license license:bsd-3)))

(define-public ghc-hmatrix
  (package
    (name "ghc-hmatrix")
    (version "0.18.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hmatrix/hmatrix-"
             version ".tar.gz"))
       (sha256
        (base32 "07zkwvg872hfk6jyn4s54ws8mvclynazaxf7fsbqi16dmf9dn61c"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-random" ,ghc-random)
       ("ghc-split" ,ghc-split)
       ("ghc-storable-complex" ,ghc-storable-complex)
       ("ghc-vector" ,ghc-vector)
       ;;("openblas" ,openblas)
       ("lapack" ,lapack)))
    ;; Guix's OpenBLAS is built with the flag "NO_LAPACK=1" which
    ;; disables inclusion of the LAPACK functions.
    ;; (arguments `(#:configure-flags '("--flags=openblas")))
    (home-page "https://github.com/albertoruiz/hmatrix")
    (synopsis "Haskell numeric linear algebra library")
    (description "The HMatrix package provices a Haskell library for
dealing with linear systems, matrix decompositions, and other
numerical computations based on BLAS and LAPACK.")
    (license license:bsd-3)))

(define-public ghc-hmatrix-gsl
  (package
    (name "ghc-hmatrix-gsl")
    (version "0.18.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hmatrix-gsl/hmatrix-gsl-"
             version ".tar.gz"))
       (sha256
        (base32 "0mflm7zg6c6a5vy092pa429rzpyv5drc1589r3x4fbmvcyqc79gx"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hmatrix" ,ghc-hmatrix)
       ("ghc-vector" ,ghc-vector)
       ("ghc-random" ,ghc-random)
       ("gsl" ,gsl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/albertoruiz/hmatrix")
    (synopsis "Haskell GSL binding")
    (description "This Haskell library provides a purely functional
interface to selected numerical computations, internally implemented
using GSL.")
    (license license:gpl3+)))

(define-public ghc-hmatrix-special
  (package
    (name "ghc-hmatrix-special")
    (version "0.4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/hmatrix-special/hmatrix-special-"
         version ".tar.gz"))
       (sha256
        (base32 "0kpcqdchi7ikzhqacy4rh4dxz3v37paxyb84wqa66sysb72wkabj"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hmatrix" ,ghc-hmatrix)
       ("ghc-hmatrix-gsl" ,ghc-hmatrix-gsl)))
    (home-page "https://github.com/albertoruiz/hmatrix")
    (synopsis "Haskell interface to GSL special functions")
    (description "This library provides an interface to GSL special
functions for Haskell.")
    (license license:gpl3+)))

(define-public ghc-hmatrix-gsl-stats
  (package
    (name "ghc-hmatrix-gsl-stats")
    (version "0.4.1.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/hmatrix-gsl-stats/hmatrix-gsl-stats-"
         version ".tar.gz"))
       (sha256
        (base32 "1gslgk58lzin43cvbpivhw7nrn9qyaa6qwhy1z9ypvyal5p8n3sa"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-storable-complex" ,ghc-storable-complex)
       ("ghc-hmatrix" ,ghc-hmatrix)
       ("gsl" ,gsl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://code.haskell.org/hmatrix-gsl-stats")
    (synopsis "GSL Statistics interface for Haskell")
    (description "This Haskell library provides a purely functional
interface for statistics based on hmatrix and GSL.")
    (license license:bsd-3)))

(define-public ghc-easyplot
  (package
    (name "ghc-easyplot")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/easyplot/easyplot-"
             version ".tar.gz"))
       (sha256
        (base32 "18kndgvdj2apjpfga6fp7m16y1gx8zrwp3c5vfj03sx4v6jvciqk"))))
    (build-system haskell-build-system)
    (propagated-inputs `(("gnuplot" ,gnuplot)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-setup-suffix
                    (lambda _ (rename-file "Setup.lhs" "Setup.hs") #t)))))
    (home-page "https://hub.darcs.net/scravy/easyplot")
    (synopsis "Haskell plotting library based on gnuplot")
    (description "This package provides a plotting library for
Haskell, using gnuplot for rendering.")
    (license license:expat)))

(define-public ghc-hashtables
  (package
    (name "ghc-hashtables")
    (version "1.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hashtables/hashtables-"
             version ".tar.gz"))
       (sha256
        (base32 "1b6w9xznk42732vpd8ili60k12yq190xnajgga0iwbdpyg424lgg"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=vector")))
    (inputs
     `(("ghc-hashable" ,ghc-hashable)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/gregorycollins/hashtables")
    (synopsis "Haskell Mutable hash tables in the ST monad")
    (description "This package provides a Haskell library including a
couple of different implementations of mutable hash tables in the ST
monad, as well as a typeclass abstracting their common operations, and
a set of wrappers to use the hash tables in the IO monad.")
    (license license:bsd-3)))

(define-public ghc-data-accessor
  (package
    (name "ghc-data-accessor")
    (version "0.2.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/data-accessor/data-accessor-"
             version ".tar.gz"))
       (sha256
        (base32 "1vf2g1gac3rm32g97rl0fll51m88q7ry4m6khnl5j47qsmx24r9l"))))
    (build-system haskell-build-system)
    (home-page "https://www.haskell.org/haskellwiki/Record_access")
    (synopsis
     "Haskell utilities for accessing and manipulating fields of records")
    (description "This package provides Haskell modules for accessing and
manipulating fields of records.")
    (license license:bsd-3)))

(define-public ghc-data-accessor-transformers
  (package
    (name "ghc-data-accessor-transformers")
    (version "0.2.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/data-accessor-transformers/"
             "data-accessor-transformers-" version ".tar.gz"))
       (sha256
        (base32 "0yp030vafbpddl27m606aibbbr5ar5j5bsv4bksscz3cq4yq5j10"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-data-accessor" ,ghc-data-accessor)))
    (home-page "https://www.haskell.org/haskellwiki/Record_access")
    (synopsis "Use Accessor to access state in transformers State monad")
    (description "This package provides Haskell modules to allow use of
Accessor to access state in transformers State monad.")
    (license license:bsd-3)))

(define-public ghc-utility-ht
  (package
    (name "ghc-utility-ht")
    (version "0.0.12")
    (home-page "https://hackage.haskell.org/package/utility-ht")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/utility-ht-" version ".tar.gz"))
       (sha256
        (base32 "1vq5bd51rl9l5lgfmaqxgiggddk38hzgngcj7qgrqnalcd1myi54"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-quickcheck" ,ghc-quickcheck)))
    (synopsis "Haskell helper functions for Lists, Maybes, Tuples, Functions")
    (description "This package includes Hakell modules providing various
helper functions for Lists, Maybes, Tuples, Functions.")
    (license license:bsd-3)))

(define-public ghc-gnuplot
  (package
    (name "ghc-gnuplot")
    (version "0.5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/gnuplot/gnuplot-"
             version ".tar.gz"))
       (sha256
        (base32 "1xz8prw9xjk0rsyrkp9bsmxykzrbhpv9qhhkdapy75mdbmgwjm7s"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-temporary" ,ghc-temporary)
       ("ghc-utility-ht" ,ghc-utility-ht)
       ("ghc-data-accessor-transformers" ,ghc-data-accessor-transformers)
       ("ghc-data-accessor" ,ghc-data-accessor)
       ("gnuplot" ,gnuplot)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-path-to-gnuplot
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gnuplot (assoc-ref inputs "gnuplot")))
               (substitute* "os/generic/Graphics/Gnuplot/Private/OS.hs"
                 (("(gnuplotName = ).*$" all cmd)
                  (string-append cmd "\"" gnuplot "/bin/gnuplot\"")))))))))
    (home-page "https://www.haskell.org/haskellwiki/Gnuplot")
    (synopsis "2D and 3D plots using gnuplot")
    (description "This package provides a Haskell module for creating 2D and
3D plots using gnuplot.")
    (license license:bsd-3)))

(define-public ghc-hinotify
  (package
    (name "ghc-hinotify")
    (version "0.3.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/hinotify/"
                    "hinotify-" version ".tar.gz"))
              (sha256
               (base32
                "03c1f4d7x805zdiq2w26kl09xrfjw19saycdkhnixzv2qcr6xm1p"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/kolmodin/hinotify.git")
    (synopsis "Haskell binding to inotify")
    (description "This library provides a wrapper to the Linux kernel's inotify
feature, allowing applications to subscribe to notifications when a file is
accessed or modified.")
    (license license:bsd-3)))

(define-public ghc-fsnotify
  (package
    (name "ghc-fsnotify")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/fsnotify/"
                    "fsnotify-" version ".tar.gz"))
              (sha256
               (base32
                "0asl313a52qx2w6dw25g845683xsl840bwjh118nkwi5v1xipkzb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-async" ,ghc-async)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-hinotify" ,ghc-hinotify)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-temporary-rc" ,ghc-temporary-rc)))
    (home-page "https://github.com/haskell-fswatch/hfsnotify")
    (synopsis "Cross platform library for file change notification.")
    (description "Cross platform library for file creation, modification, and
deletion notification. This library builds upon existing libraries for platform
specific Windows, Mac, and Linux file system event notification.")
    (license license:bsd-3)))

(define-public ghc-ieee754
  (package
    (name "ghc-ieee754")
    (version "0.7.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/ieee754/"
                    "ieee754-" version ".tar.gz"))
              (sha256
               (base32
                "1zvfnnd5nm5kgr60214cdyks0kqdqyzpwk5sdh0s60yr8b7fyjny"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/patperry/hs-ieee754")
    (synopsis "Utilities for dealing with IEEE floating point numbers")
    (description "Utilities for dealing with IEEE floating point numbers,
ported from the Tango math library; approximate and exact equality comparisons
for general types.")
    (license license:bsd-3)))

(define-public ghc-terminal-size
  (package
    (name "ghc-terminal-size")
    (version "0.3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/terminal-size/"
                    "terminal-size-" version ".tar.gz"))
              (sha256
               (base32
                "0n4nvj3dbj9gxfnprgish45asn9z4dipv9j98s8i7g2n8yb3xhmm"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/terminal-size")
    (synopsis "Get terminal window height and width")
    (description "Get terminal window height and width without ncurses
dependency.")
    (license license:bsd-3)))

(define-public ghc-language-c
  (package
    (name "ghc-language-c")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "language-c/language-c-" version ".tar.gz"))
       (sha256
        (base32
         "08i2bl7jmmymn2sldzlbz6ig7lx3wfwhlpadzibs3fx72z08pmc6"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-syb" ,ghc-syb)))
    (native-inputs
     `(("ghc-happy" ,ghc-happy)
       ("ghc-alex" ,ghc-alex)))
    (home-page "https://visq.github.io/language-c/")
    (synopsis "Analysis and generation of C code")
    (description
     "Language C is a Haskell library for the analysis and generation of C code.
It features a complete, well-tested parser and pretty printer for all of C99
and a large set of GNU extensions.")
    (license license:bsd-3)))

(define-public ghc-markdown-unlit
  (package
    (name "ghc-markdown-unlit")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://hackage/package/markdown-unlit/"
                    "markdown-unlit-" version ".tar.gz"))
              (sha256
               (base32
                "1kj2bffl7ndd8ygwwa3r1mbpwbxbfhyfgnbla8k8g9i6ffp0qrbw"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-silently" ,ghc-silently)
       ("ghc-stringbuilder" ,ghc-stringbuilder)
       ("ghc-temporary" ,ghc-temporary)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/sol/markdown-unlit#readme")
    (synopsis "Literate Haskell support for Markdown")
    (description "This package allows you to have a README.md that at the
same time is a literate Haskell program.")
    (license license:expat)))

(define-public corrode
  (let ((commit "b6699fb2fa552a07c6091276285a44133e5c9789"))
    (package
      (name "corrode")
      (version (string-append "0.0.1-" (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jameysharp/corrode.git")
               (commit "b6699fb2fa552a07c6091276285a44133e5c9789")))
         (file-name
          (string-append name "-" version "-checkout"))
         (sha256
          (base32 "02v0yyj6sk4gpg2222wzsdqjxn8w66scbnf6b20x0kbmc69qcz4r"))))
      (build-system haskell-build-system)
      (inputs
       `(("ghc-language-c" ,ghc-language-c)
         ("ghc-markdown-unlit" ,ghc-markdown-unlit)))
      (home-page "https://github.com/jameysharp/corrode")
      (synopsis "Automatic semantics-preserving translation from C to Rust")
      (description
       "This program reads a C source file and prints an equivalent module in
Rust syntax.  It is intended to be useful for two different purposes:

@enumerate
@item Partial automation for migrating legacy code that was implemented in C.
@item A new, complementary approach to static analysis for C programs.
@end enumerate\n")
      (license license:gpl2+))))

(define-public ghc-wave
  (package
    (name "ghc-wave")
    (version "0.1.4")
    (source (origin
      (method url-fetch)
      (uri (string-append
             "https://hackage.haskell.org/package/wave/wave-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1g5nmqfk6p25v9ismwz4i66ay91bd1qh39xwj0hm4z6a5mw8frk8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-cereal" ,ghc-cereal)
       ("ghc-data-default-class"
        ,ghc-data-default-class)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-temporary" ,ghc-temporary)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/mrkkrp/wave")
    (synopsis "Work with WAVE and RF64 files in Haskell")
    (description "This package allows you to work with WAVE and RF64
files in Haskell.")
    (license license:bsd-3)))

(define-public ghc-hslogger
  (package
    (name "ghc-hslogger")
    (version "1.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hslogger-" version "/" "hslogger-"
                           version ".tar.gz"))
       (sha256 (base32
                "0as5gvlh6pi2gflakp695qnlizyyp059dqrhvjl4gjxalja6xjnp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-network" ,ghc-network)
       ("ghc-old-locale" ,ghc-old-locale)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://software.complete.org/hslogger")
    (synopsis "Logging framework for Haskell, similar to Python's logging module")
    (description "Hslogger lets each log message have a priority and source be
associated with it.  The programmer can then define global handlers that route
or filter messages based on the priority and source.  It also has a syslog
handler built in.")
    (license license:bsd-3)))

(define-public ghc-unexceptionalio
  (package
    (name "ghc-unexceptionalio")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "unexceptionalio-" version "/" "unexceptionalio-"
                           version ".tar.gz"))
       (sha256 (base32 "1f7h7zwky4scdcyjspg4ksfh7x6yra0wjybxq70p7vcwpgk2nzlj"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/singpolyma/unexceptionalio")
    (synopsis "IO without any non-error, synchronous exceptions")
    (description "When you've caught all the exceptions that can be
handled safely, this is what you're left with.")
    (license license:isc)))

(define-public ghc-json
  (package
    (name "ghc-json")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/json/"
                           "json-" version ".tar.gz"))
       (sha256
        (base32
         "18l5027vc68hnnxrxlnyl59vkkg95a92m1zzms0dqiby2r6pxdcn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-text" ,ghc-text)
       ("ghc-parsec" ,ghc-parsec)))
    (home-page "https://hackage.haskell.org/package/json")
    (synopsis "Serializes Haskell data to and from JSON")
    (description "This package provides a parser and pretty printer for
converting between Haskell values and JSON.
JSON (JavaScript Object Notation) is a lightweight data-interchange format.")
    (license license:bsd-3)))

(define-public ghc-esqueleto
  (package
    (name "ghc-esqueleto")
    (version "2.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "esqueleto/esqueleto-" version ".tar.gz"))
       (sha256
        (base32
         "10n49rzqmblky7pwjnysalyy6nacmxfms8dqbsdv6hlyzr8pb69x"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-persistent" ,ghc-persistent)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-text" ,ghc-text)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-persistent-sqlite" ,ghc-persistent-sqlite)
       ("ghc-persistent-template" ,ghc-persistent-template)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/bitemyapp/esqueleto")
    (synopsis "Type-safe embedded domain specific language for SQL queries")
    (description "This library provides a type-safe embedded domain specific
language (EDSL) for SQL queries that works with SQL backends as provided by
@code{ghc-persistent}.  Its language closely resembles SQL, so you don't have
to learn new concepts, just new syntax, and it's fairly easy to predict the
generated SQL and optimize it for your backend.")
    (license license:bsd-3)))

(define-public shellcheck
  (package
    (name "shellcheck")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/koalaman/shellcheck/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "1qkd69lc34n3l23ss9rq1azvx49bfq4hi4bmaj76rgxybscxhg0w"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-json" ,ghc-json)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)))
    (home-page "https://github.com/koalaman/shellcheck")
    (synopsis "Static analysis for shell scripts")
    (description "@code{shellcheck} provides static analysis for
@command{bash} and @command{sh} shell scripts.
It gives warnings and suggestions in order to:

@enumerate
@item Point out and clarify typical beginner's syntax issues that cause
a shell to give cryptic error messages.
@item Point out and clarify typical intermediate level semantic problems
that cause a shell to behave strangely and counter-intuitively.
@item Point out subtle caveats, corner cases and pitfalls that may cause an
advanced user's otherwise working script to fail under future circumstances.
@end enumerate")
    (license license:gpl3+)))

(define-public ghc-simple-sendfile
  (package
    (name "ghc-simple-sendfile")
    (version "0.2.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "simple-sendfile-" version "/"
                           "simple-sendfile-" version ".tar.gz"))
       (sha256
        (base32
         "0k99j9xfcf83c55jmn202hdinhjaa4yn3dal4rvjk2w2rlhqirha"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-network" ,ghc-network)
       ("ghc-resourcet" ,ghc-resourcet)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/kazu-yamamoto/simple-sendfile")
    (synopsis "Cross platform library for the sendfile system call")
    (description "This library tries to call minimum system calls which
are the bottleneck of web servers.")
    (license license:bsd-3)))

(define-public ghc-hex
  (package
    (name "ghc-hex")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hex-" version "/"
                           "hex-" version ".tar.gz"))
       (sha256
        (base32
         "1v31xiaivrrn0q2jz8919wvkjplv1kxna5ajhsj701fqxm1i5vhj"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/hex")
    (synopsis "Convert strings into hexadecimal and back")
    (description "This package provides conversion functions between
bytestrings and their hexademical representation.")
    (license license:bsd-3)))

(define-public ghc-psqueues
  (package
    (name "ghc-psqueues")
    (version "0.2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "psqueues-" version "/"
                           "psqueues-" version ".tar.gz"))
       (sha256
        (base32
         "0n39s1i88j6s7vvsdhpbhcr3gpbwlzabwcc3nbd7nqb4kb4i0sls"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck")))
    (inputs
     `(("ghc-hashable" ,ghc-hashable)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/jaspervdj/psqueues")
    (synopsis "Pure priority search queues")
    (description "The psqueues package provides
@uref{https://en.wikipedia.org/wiki/Priority_queue, Priority Search Queues} in
three different flavors:

@itemize
@item @code{OrdPSQ k p v}, which uses the @code{Ord k} instance to provide
fast insertion, deletion and lookup.  This implementation is based on Ralf
Hinze's @uref{http://citeseer.ist.psu.edu/hinze01simple.html, A Simple
Implementation Technique for Priority Search Queues}.

Hence, it is similar to the @uref{https://hackage.haskell.org/package/PSQueue,
PSQueue} library, although it is considerably faster and provides a slightly
different API.

@item @code{IntPSQ p v} is a far more efficient implementation.  It fixes the
key type to @code{Int} and uses a
@code{https://en.wikipedia.org/wiki/Radix_tree, radix tree} (like @code{IntMap})
with an additional min-heap property.

@item @code{HashPSQ k p v} is a fairly straightforward extension
of @code{IntPSQ}: it simply uses the keys' hashes as indices in the
@code{IntPSQ}.  If there are any hash collisions, it uses an
@code{OrdPSQ} to resolve those.  The performance of this implementation
is comparable to that of @code{IntPSQ}, but it is more widely
applicable since the keys are not restricted to @code{Int},
but rather to any @code{Hashable} datatype.
@end itemize

Each of the three implementations provides the same API, so they can
be used interchangeably.

Typical applications of Priority Search Queues include:

@itemize
@item Caches, and more specifically LRU Caches;
@item Schedulers;
@item Pathfinding algorithms, such as Dijkstra's and A*.
@end itemize")
    (license license:bsd-3)))

(define-public ghc-glob
  (package
    (name "ghc-glob")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "Glob-" version "/"
                           "Glob-" version ".tar.gz"))
       (sha256
        (base32
         "0rzmsknl02p332dxmm36fyrz3dpma7bchn0ymyjipxvqil20pjw0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-dlist" ,ghc-dlist)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "http://iki.fi/matti.niemenmaa/glob/")
    (synopsis "Haskell library matching glob patterns against file paths")
    (description "This package provides a Haskell library for @dfn{globbing}:
matching patterns against file paths.")
    (license license:bsd-3)))

(define-public ghc-errors
  (package
    (name "ghc-errors")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "errors-" version "/"
                           "errors-" version ".tar.gz"))
       (sha256
        (base32
         "13sflhglcm5skwrxb48fw96skdcx7ydiy4zg22200733pxhjncpn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-text" ,ghc-text)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-unexceptionalio" ,ghc-unexceptionalio)
       ("ghc-safe" ,ghc-safe)))
    (home-page "https://github.com/gabriel439/haskell-errors-library")
    (synopsis "Error handling library for Haskell")
    (description "This library encourages an error-handling style that
directly uses the type system, rather than out-of-band exceptions.")
    (license license:bsd-3)))

(define-public ghc-vector-th-unbox
  (package
    (name "ghc-vector-th-unbox")
    (version "0.2.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "vector-th-unbox-" version "/"
                           "vector-th-unbox-" version ".tar.gz"))
       (sha256
        (base32
         "0d82x55f5vvr1jvaia382m23rs690lg55pvavv8f4ph0y6kd91xy"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-data-default" ,ghc-data-default)))
    (home-page "https://github.com/liyang/vector-th-unbox")
    (synopsis "Deriver for Data.Vector.Unboxed using Template Haskell")
    (description "This Haskell library provides a Template Haskell
deriver for unboxed vectors, given a pair of coercion functions to
and from some existing type with an Unbox instance.")
    (license license:bsd-3)))

(define-public ghc-erf
  (package
    (name "ghc-erf")
    (version "2.0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "erf-" version "/"
                           "erf-" version ".tar.gz"))
       (sha256
        (base32
         "0dxk2r32ajmmc05vaxcp0yw6vgv4lkbmh8jcshncn98xgsfbgw14"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/erf")
    (synopsis "The error function, erf, and related functions for Haskell")
    (description "This Haskell library provides a type class for the
error function, erf, and related functions.  Instances for Float and
Double.")
    (license license:bsd-3)))

(define-public ghc-math-functions
  (package
    (name "ghc-math-functions")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "math-functions-" version "/"
                           "math-functions-" version ".tar.gz"))
       (sha256
        (base32
         "1sv5vabsx332v1lpb6v3jv4zrzvpx1n7yprzd8wlcda5vsc5a6zp"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))  ; FIXME: 1 test fails.
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-vector-th-unbox" ,ghc-vector-th-unbox)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-erf" ,ghc-erf)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/bos/math-functions")
    (synopsis "Special functions and Chebyshev polynomials for Haskell")
    (description "This Haskell library provides implementations of
special mathematical functions and Chebyshev polynomials.  These
functions are often useful in statistical and numerical computing.")
    (license license:bsd-3)))

(define-public ghc-mwc-random
  (package
    (name "ghc-mwc-random")
    (version "0.13.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "mwc-random-" version "/"
                           "mwc-random-" version ".tar.gz"))
       (sha256
        (base32
         "05j7yh0hh9nxic3dijmzv44kc6gzclvamdph7sq7w19wq57k6pq6"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-primitive" ,ghc-primitive)
       ("ghc-vector" ,ghc-vector)
       ("ghc-math-functions" ,ghc-math-functions)))
    (arguments
     `(#:tests? #f)) ; FIXME: Test-Suite `spec` fails.
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/bos/mwc-random")
    (synopsis "Random number generation library for Haskell")
    (description "This Haskell package contains code for generating
high quality random numbers that follow either a uniform or normal
distribution.  The generated numbers are suitable for use in
statistical applications.

The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)
multiply-with-carry generator, which has a period of 2^{8222} and
fares well in tests of randomness.  It is also extremely fast,
between 2 and 3 times faster than the Mersenne Twister.")
    (license license:bsd-3)))

(define-public ghc-vector-algorithms
  (package
    (name "ghc-vector-algorithms")
    (version "0.7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "vector-algorithms-" version "/"
                           "vector-algorithms-" version ".tar.gz"))
       (sha256
        (base32
         "0w4hf598lpxfg58rnimcqxrbnpqq2jmpjx82qa5md3q6r90hlipd"))))
    (build-system haskell-build-system)
    ;; The limits have been adjusted in a revision of the cabal file.
    (arguments
     '(#:configure-flags (list "--allow-newer=vector")
       #:phases
       (modify-phases %standard-phases
         ;; The tests cannot be built due to type errors.
         (add-after 'unpack 'do-not-build-quickcheck-tests
           (lambda _
             (substitute* "vector-algorithms.cabal"
               (("\\!flag\\(properties\\)") "True"))
             #t)))))
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-mwc-random" ,ghc-mwc-random)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/bos/math-functions")
    (synopsis "Algorithms for vector arrays in Haskell")
    (description "This Haskell library algorithms for vector arrays.")
    (license license:bsd-3)))

(define-public ghc-language-haskell-extract
  (package
    (name "ghc-language-haskell-extract")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "language-haskell-extract-" version "/"
                           "language-haskell-extract-" version ".tar.gz"))
       (sha256
        (base32
         "1nxcs7g8a1sp91bzpy4cj6s31k5pvc3gvig04cbrggv5cvjidnhl"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-posix" ,ghc-regex-posix)))
    (home-page "https://github.com/finnsson/template-helper")
    (synopsis "Haskell module to automatically extract functions from
the local code")
    (description "This package contains helper functions on top of
Template Haskell.

For example, @code{functionExtractor} extracts all functions after a
regexp-pattern, which can be useful if you wish to extract all functions
beginning with @code{test} (for a test framework) or all functions beginning
with @code{wc} (for a web service).")
    (license license:bsd-3)))

(define-public ghc-abstract-par
  (package
    (name "ghc-abstract-par")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "abstract-par-" version "/"
                           "abstract-par-" version ".tar.gz"))
       (sha256
        (base32
         "0q6qsniw4wks2pw6wzncb1p1j3k6al5njnvm2v5n494hplwqg2i4"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/simonmar/monad-par")
    (synopsis "Abstract parallelization interface for Haskell")
    (description "This Haskell package is an abstract interface
only.  It provides a number of type clasess, but not an
implementation.  The type classes separate different levels
of @code{Par} functionality.  See the @code{Control.Monad.Par.Class}
module for more details.")
    (license license:bsd-3)))

(define-public ghc-monad-par-extras
  (package
    (name "ghc-monad-par-extras")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "monad-par-extras-" version "/"
                           "monad-par-extras-" version ".tar.gz"))
       (sha256
        (base32
         "0bl4bd6jzdc5zm20q1g67ppkfh6j6yn8fwj6msjayj621cck67p2"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-abstract-par" ,ghc-abstract-par)
              ("ghc-cereal" ,ghc-cereal)
              ("ghc-random" ,ghc-random)
              ("ghc-mtl" ,ghc-mtl)))
    (home-page "https://github.com/simonmar/monad-par")
    (synopsis "Combinators and extra features for Par monads for Haskell")
    (description "This Haskell package provides additional data structures,
and other added capabilities layered on top of the @code{Par} monad.")
    (license license:bsd-3)))

(define-public ghc-abstract-deque
  (package
    (name "ghc-abstract-deque")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "abstract-deque-" version "/"
                           "abstract-deque-" version ".tar.gz"))
       (sha256
        (base32
         "18jwswjxwzc9bjiy4ds6hw2a74ki797jmfcifxd2ga4kh7ri1ah9"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-random" ,ghc-random)))
    (home-page "https://github.com/rrnewton/haskell-lockfree/wiki")
    (synopsis "Abstract, parameterized interface to mutable Deques for Haskell")
    (description "This Haskell package provides an abstract interface to
highly-parameterizable queues/deques.

Background: There exists a feature space for queues that extends between:

@itemize
@item Simple, single-ended, non-concurrent, bounded queues

@item Double-ended, thread-safe, growable queues with important points
in between (such as the queues used for work stealing).
@end itemize

This package includes an interface for Deques that allows the programmer
to use a single API for all of the above, while using the type system to
select an efficient implementation given the requirements (using type families).

This package also includes a simple reference implementation based on
@code{IORef} and @code{Data.Sequence}.")
    (license license:bsd-3)))

(define-public ghc-monad-par
  (package
    (name "ghc-monad-par")
    (version "0.3.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "monad-par-" version "/"
                           "monad-par-" version ".tar.gz"))
       (sha256
        (base32
         "0ldrzqy24fsszvn2a2nr77m2ih7xm0h9bgkjyv1l274aj18xyk7q"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-abstract-par" ,ghc-abstract-par)
              ("ghc-abstract-deque" ,ghc-abstract-deque)
              ("ghc-monad-par-extras" ,ghc-monad-par-extras)
              ("ghc-mwc-random" ,ghc-mwc-random)
              ("ghc-parallel" ,ghc-parallel)
              ("ghc-mtl" ,ghc-mtl)))
    (native-inputs `(("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-hunit" ,ghc-hunit)
                     ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
                     ("ghc-test-framework-quickcheck2"
                      ,ghc-test-framework-quickcheck2)
                     ("ghc-test-framework" ,ghc-test-framework)
                     ("ghc-test-framework-th" ,ghc-test-framework-th)))
    (home-page "https://github.com/simonmar/monad-par")
    (synopsis "Haskell library for parallel programming based on a monad")
    (description "The @code{Par} monad offers an API for parallel
programming.  The library works for parallelising both pure and @code{IO}
computations, although only the pure version is deterministic.  The default
implementation provides a work-stealing scheduler and supports forking tasks
that are much lighter weight than IO-threads.")
    (license license:bsd-3)))

(define-public ghc-statistics
  (package
    (name "ghc-statistics")
    (version "0.14.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "statistics-" version "/"
                           "statistics-" version ".tar.gz"))
       (sha256
        (base32
         "0y27gafkib0x0fn39qfn2rkgsfrm09ng35sbb5dwr7rclhnxz59l"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-erf" ,ghc-erf)
       ("ghc-math-functions" ,ghc-math-functions)
       ("ghc-monad-par" ,ghc-monad-par)
       ("ghc-mwc-random" ,ghc-mwc-random)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-vector" ,ghc-vector)
       ("ghc-vector-algorithms" ,ghc-vector-algorithms)
       ("ghc-vector-th-unbox" ,ghc-vector-th-unbox)
       ("ghc-vector-binary-instances" ,ghc-vector-binary-instances)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-ieee754" ,ghc-ieee754)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (arguments
     `(#:tests? #f)) ; FIXME: Test-Suite `spec` fails.
    (home-page "https://github.com/bos/mwc-random")
    (synopsis "Haskell library of statistical types, data, and functions")
    (description "This library provides a number of common functions
and types useful in statistics.  We focus on high performance, numerical
robustness, and use of good algorithms.  Where possible, we provide references
to the statistical literature.

The library's facilities can be divided into four broad categories:

@itemize
@item Working with widely used discrete and continuous probability
distributions.  (There are dozens of exotic distributions in use; we focus
on the most common.)

@item Computing with sample data: quantile estimation, kernel density
estimation, histograms, bootstrap methods, significance testing,
and regression and autocorrelation analysis.

@item Random variate generation under several different distributions.

@item Common statistical tests for significant differences between samples.
@end itemize")
    (license license:bsd-2)))

(define-public ghc-chunked-data
  (package
    (name "ghc-chunked-data")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "chunked-data-" version "/"
                           "chunked-data-" version ".tar.gz"))
       (sha256
        (base32
         "0bszq6fijnr4pmadzz89smj7kfmzx0ca3wd9ga8gv0in9jk9vgp1"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-vector" ,ghc-vector)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-text" ,ghc-text)))
    (home-page "https://github.com/snoyberg/mono-traversable")
    (synopsis "Typeclasses for dealing with various chunked data
representations for Haskell")
    (description "This Haskell package was originally present in
classy-prelude.")
    (license license:expat)))

(define-public ghc-base-prelude
  (package
    (name "ghc-base-prelude")
    (version "1.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "base-prelude-" version "/"
                           "base-prelude-" version ".tar.gz"))
       (sha256
        (base32
         "17hivs7lmsglagdlzxd9q9zsddmgqin2788mpq911zwnb57lj6l1"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/nikita-volkov/base-prelude")
    (synopsis "The most complete prelude formed solely from the Haskell's base
package")
    (description "This Haskell package aims to reexport all the non-conflicting
and most general definitions from the \"base\" package.

This includes APIs for applicatives, arrows, monoids, foldables, traversables,
exceptions, generics, ST, MVars and STM.

This package will never have any dependencies other than \"base\".

Versioning policy:

The versioning policy of this package deviates from PVP in the sense
that its exports in part are transitively determined by the version of \"base\".
Therefore it's recommended for the users of @code{ghc-base-prelude} to specify
the bounds of \"base\" as well.")
    (license license:expat)))

(define-public ghc-tuple-th
  (package
    (name "ghc-tuple-th")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "tuple-th-" version "/"
                           "tuple-th-" version ".tar.gz"))
       (sha256
        (base32
         "1mrl4vvxmby7sf1paf7hklzidnr6wq55822i73smqyz0xpf3gsjn"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/DanielSchuessler/tuple-th")
    (synopsis "Generate utility functions for tuples of statically known size
for Haskell")
    (description "This Haskell package contains Template Haskell functions for
generating functions similar to those in @code{Data.List} for tuples of
statically known size.")
    (license license:bsd-3)))

(define-public ghc-contravariant-extras
  (package
    (name "ghc-contravariant-extras")
    (version "0.3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "contravariant-extras-" version "/"
                           "contravariant-extras-" version ".tar.gz"))
       (sha256
        (base32
         "1mbrgjybdx8fjdck4ldwi8955w4qnmm0ql56zix7dyn0s7s9spgk"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-tuple-th" ,ghc-tuple-th)
              ("ghc-contravariant" ,ghc-contravariant)
              ("ghc-base-prelude" ,ghc-base-prelude)))
    (home-page "https://github.com/nikita-volkov/contravariant-extras")
    (synopsis "Extras for the @code{ghc-contravariant} Haskell package")
    (description "This Haskell package provides extras for the
@code{ghc-contravariant} package.")
    (license license:expat)))

(define-public ghc-monadrandom
  (package
    (name "ghc-monadrandom")
    (version "0.4.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "MonadRandom-" version "/"
                           "MonadRandom-" version ".tar.gz"))
       (sha256
        (base32
         "1h1nhswrcmhy3mq6vd530p0df51fcnnf4csbwnljar7cf0mb2h6y"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-transformers-compat" ,ghc-transformers-compat)
              ("ghc-mtl" ,ghc-mtl)
              ("ghc-primitive" ,ghc-primitive)
              ("ghc-fail" ,ghc-fail)
              ("ghc-random" ,ghc-random)))
    (home-page "https://github.com/byorgey/MonadRandom")
    (synopsis "Random-number generation monad for Haskell")
    (description "This Haskell package provides support for computations
which consume random values.")
    (license license:bsd-3)))

(define-public ghc-either
  (package
    (name "ghc-either")
    (version "4.4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "either-" version "/"
                           "either-" version ".tar.gz"))
       (sha256
        (base32
         "1lrlwqqnm6ibfcydlv5qvvssw7bm0c6yypy0rayjzv1znq7wp1xh"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-bifunctors" ,ghc-bifunctors)
              ("ghc-exceptions" ,ghc-exceptions)
              ("ghc-free" ,ghc-free)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-manodrandom" ,ghc-monadrandom)
              ("ghc-mtl" ,ghc-mtl)
              ("ghc-mmorph" ,ghc-mmorph)
              ("ghc-profunctors" ,ghc-profunctors)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-semigroupoids" ,ghc-semigroupoids)
              ("ghc-transformers-base" ,ghc-transformers-base)))
    (home-page "https://github.com/ekmett/either")
    (synopsis "Provides an either monad transformer for Haskell")
    (description "This Haskell package provides an either monad transformer.")
    (license license:bsd-3)))

(define-public ghc-pretty-hex
  (package
    (name "ghc-pretty-hex")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "pretty-hex-" version "/"
                           "pretty-hex-" version ".tar.gz"))
       (sha256
        (base32
         "0ylwkvvjvmpprha9nx83xb8gkhyanhk5fffc0r7lb96n4ch5z6pz"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/GaloisInc/hexdump")
    (synopsis "Haskell library for hex dumps of ByteStrings")
    (description "This Haskell library generates pretty hex dumps of
ByteStrings in the style of other common *nix hex dump tools.")
    (license license:bsd-3)))

(define-public ghc-network-info
  (package
    (name "ghc-network-info")
    (version "0.2.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "network-info-" version "/"
                           "network-info-" version ".tar.gz"))
       (sha256
        (base32
         "0xndvg776241fgjmynxfpy81f1csjmh8dg33yf0c8m71ychz3pzc"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/jystic/network-info")
    (synopsis "Access the local computer's basic network configuration")
    (description "This Haskell library provides simple read-only access to the
local computer's networking configuration.  It is currently capable of
getting a list of all the network interfaces and their respective
IPv4, IPv6 and MAC addresses.")
    (license license:bsd-3)))

(define-public ghc-uuid-types
  (package
    (name "ghc-uuid-types")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "uuid-types-" version "/"
                           "uuid-types-" version ".tar.gz"))
       (sha256
        (base32
         "1zdka5jnm1h6k36w3nr647yf3b5lqb336g3fkprhd6san9x52xlj"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck"
                               "--allow-newer=HUnit")))
    (inputs `(("ghc-hashable" ,ghc-hashable)
              ("ghc-random" ,ghc-random)
              ("ghc-text" ,ghc-text)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)
                     ("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)
                     ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/hvr/uuid")
    (synopsis "Haskell type definitions for UUIDs")
    (description "This Haskell library contains type definitions for
@dfn{Universally Unique Identifiers} or
@uref{https://en.wikipedia.org/wiki/UUID, UUIDs}, and basic conversion
functions.")
    (license license:bsd-3)))

(define-public ghc-uuid
  (package
    (name "ghc-uuid")
    (version "1.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "uuid-" version "/"
                           "uuid-" version ".tar.gz"))
       (sha256
        (base32
         "09xhk42yhxvqmka0iqrv3338asncz8cap3j0ic0ps896f2581b6z"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck"
                               "--allow-newer=HUnit")))
    (inputs `(("ghc-cryptohash-sha1" ,ghc-cryptohash-sha1)
              ("ghc-cryptohash-md5" ,ghc-cryptohash-md5)
              ("ghc-entropy" ,ghc-entropy)
              ("ghc-network-info" ,ghc-network-info)
              ("ghc-random" ,ghc-random)
              ("ghc-text" ,ghc-text)
              ("ghc-uuid-types" ,ghc-uuid-types)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)
                     ("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)
                     ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/hvr/uuid")
    (synopsis "Haskell library to create, compare, parse, and print UUIDs")
    (description "This Haskell library provides utilities creating, comparing,
parsing and printing @dfn{Universally Unique Identifiers} or UUIDs.")
    (license license:bsd-3)))

(define-public ghc-rebase
  (package
    (name "ghc-rebase")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "rebase-" version "/"
                           "rebase-" version ".tar.gz"))
       (sha256
        (base32
         "1qkhnpcc4g2vd6jmbf3b6psqkan6hyawqzrwzqdd931hsb02l6ia"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-stm" ,ghc-stm)
              ("ghc-hashable" ,ghc-hashable)
              ("ghc-vector" ,ghc-vector)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-text" ,ghc-text)
              ("ghc-scientific" ,ghc-scientific)
              ("ghc-uuid" ,ghc-uuid)
              ("ghc-dlist" ,ghc-dlist)
              ("ghc-void" ,ghc-void)
              ("ghc-bifunctors" ,ghc-bifunctors)
              ("ghc-profunctors" ,ghc-profunctors)
              ("ghc-contravariant" ,ghc-contravariant)
              ("ghc-contravariant-extras" ,ghc-contravariant-extras)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-mtl" ,ghc-mtl)
              ("ghc-either" ,ghc-either)
              ("ghc-fail" ,ghc-fail)
              ("ghc-base-prelude" ,ghc-base-prelude)))
    (home-page "https://github.com/nikita-volkov/rebase")
    (synopsis "Progressive alternative to the base package
for Haskell")
    (description "This Haskell package is intended for those who are
tired of keeping long lists of dependencies to the same essential libraries
in each package as well as the endless imports of the same APIs all over again.

It also supports the modern tendencies in the language.

To solve those problems this package does the following:

@itemize
@item Reexport the original APIs under the @code{Rebase} namespace.

@item Export all the possible non-conflicting symbols from the
@code{Rebase.Prelude} module.

@item Give priority to the modern practices in the conflicting cases.
@end itemize

The policy behind the package is only to reexport the non-ambiguous and
non-controversial APIs, which the community has obviously settled on.
The package is intended to rapidly evolve with the contribution from
the community, with the missing features being added with pull-requests.")
    (license license:expat)))

(define-public ghc-vector-builder
  (package
    (name "ghc-vector-builder")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "vector-builder-" version "/"
                           "vector-builder-" version ".tar.gz"))
       (sha256
        (base32
         "1l6sfgd2s107zkp1qd1w6jdjcbznp31769qf99pxar087f697wvp"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-vector" ,ghc-vector)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-base-prelude" ,ghc-base-prelude)))
    (native-inputs `(("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)
                     ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
                     ("ghc-hunit" ,ghc-hunit)
                     ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
                     ("ghc-rebase" ,ghc-rebase)))
    (home-page "https://github.com/nikita-volkov/vector-builder")
    (synopsis "Vector builder for Haskell")
    (description "This Haskell package provides an API for constructing vectors.
It provides the composable @code{Builder} abstraction, which has instances of the
@code{Monoid} and @code{Semigroup} classes.

You would first use the @code{Builder} abstraction to specify the structure of
the vector; then you can execute the builder to actually produce the
vector. ")
    (license license:expat)))

(define-public ghc-foldl
  (package
    (name "ghc-foldl")
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "foldl-" version "/"
                           "foldl-" version ".tar.gz"))
       (sha256
        (base32
         "10qsp7dj2xsq4q2xm6x6b12y5pq32qf7my41hnkmdwwbccvhdxb2"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-mwc-randam" ,ghc-mwc-random)
              ("ghc-primitive" ,ghc-primitive)
              ("ghc-text" ,ghc-text)
              ("ghc-vector" ,ghc-vector)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-hashable" ,ghc-hashable)
              ("ghc-contravariant" ,ghc-contravariant)
              ("ghc-profunctors" ,ghc-profunctors)
              ("ghc-comonad" ,ghc-comonad)
              ("ghc-vector-builder" ,ghc-vector-builder)))
    (home-page "https://github.com/Gabriel439/Haskell-Foldl-Library")
    (synopsis "Composable, streaming, and efficient left folds for Haskell")
    (description "This Haskell library provides strict left folds that stream
in constant memory, and you can combine folds using @code{Applicative} style
to derive new folds.  Derived folds still traverse the container just once
and are often as efficient as hand-written folds.")
    (license license:bsd-3)))

(define-public ghc-mono-traversable
  (package
    (name "ghc-mono-traversable")
    (version "1.0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "mono-traversable-" version "/"
                           "mono-traversable-" version ".tar.gz"))
       (sha256
        (base32
         "0180ks0dyvpk1r20w5jw2w2n79mjnk69n9vhspaxzlyxqgim5psa"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-hashable" ,ghc-hashable)
              ("ghc-text" ,ghc-text)
              ("ghc-vector" ,ghc-vector)
              ("ghc-vector-algorithms" ,ghc-vector-algorithms)
              ("ghc-split" ,ghc-split)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-hunit" ,ghc-hunit)
                     ("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-semigroups" ,ghc-semigroups)
                     ("ghc-foldl" ,ghc-foldl)))
    (home-page "https://github.com/snoyberg/mono-traversable")
    (synopsis "Haskell classes for mapping, folding, and traversing monomorphic
containers")
    (description "This Haskell package provides Monomorphic variants of the
Functor, Foldable, and Traversable typeclasses.  If you understand Haskell's
basic typeclasses, you understand mono-traversable.  In addition to what
you are used to, it adds on an IsSequence typeclass and has code for marking
data structures as non-empty.")
    (license license:expat)))

(define-public ghc-conduit-combinators
  (package
    (name "ghc-conduit-combinators")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "conduit-combinators-" version "/"
                           "conduit-combinators-" version ".tar.gz"))
       (sha256
        (base32
         "1lz70vwp4y4lpsivxl0cshq7aq3968rh48r6rjvpyaj2l0bdj5wp"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-conduit" ,ghc-conduit)
              ("ghc-conduit-extra" ,ghc-conduit-extra)
              ("ghc-transformers-base" ,ghc-transformers-base)
              ("ghc-primitive" ,ghc-primitive)
              ("ghc-vector" ,ghc-vector)
              ("ghc-text" ,ghc-text)
              ("ghc-void" ,ghc-void)
              ("ghc-mwc-random" ,ghc-mwc-random)
              ("ghc-unix-compat" ,ghc-unix-compat)
              ("ghc-base16-bytestring" ,ghc-base16-bytestring)
              ("ghc-base64-bytestring" ,ghc-base64-bytestring)
              ("ghc-resourcet" ,ghc-resourcet)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-chunked-data" ,ghc-chunked-data)
              ("ghc-mono-traversable" ,ghc-mono-traversable)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-silently" ,ghc-silently)
                     ("ghc-mtl" ,ghc-mtl)
                     ("ghc-safe" ,ghc-safe)
                     ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/snoyberg/mono-traversable")
    (synopsis "Commonly used conduit functions, for both chunked and
unchunked data")
    (description "This Haskell package provides a replacement for Data.Conduit.List,
as well as a convenient Conduit module.")
    (license license:expat)))

(define-public ghc-aws
  (package
    (name "ghc-aws")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "aws-" version "/aws-" version ".tar.gz"))
       (sha256 (base32
                "0h7473wkvc5xjzx5fd5k5fp70rjq5gqmn1cpy95mswvvfsq3irxj"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; Tests require AWS credentials.
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-byteable" ,ghc-byteable)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-cereal" ,ghc-cereal)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-http-conduit" ,ghc-http-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-network" ,ghc-network)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-safe" ,ghc-safe)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-text" ,ghc-text)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector" ,ghc-vector)
       ("ghc-xml-conduit" ,ghc-xml-conduit)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-errors" ,ghc-errors)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-conduit-combinators" ,ghc-conduit-combinators)))
    (home-page "https://github.com/aristidb/aws")
    (synopsis "Amazon Web Services for Haskell")
    (description "This package attempts to provide support for using
Amazon Web Services like S3 (storage), SQS (queuing) and others to
Haskell programmers.  The ultimate goal is to support all Amazon
Web Services.")
    (license license:bsd-3)))

(define-public ghc-basement
  (package
    (name "ghc-basement")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "basement/basement-" version ".tar.gz"))
       (sha256
        (base32
         "0w2g4k9bs2ph00p0fgrmcrng8ypdz6xis0r10g380nzkg2rvj0dm"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/haskell-foundation/foundation")
    (synopsis "Basic primitives for Foundation starter pack")
    (description
     "This package contains basic primitives for the Foundation set of
packages.")
    (license license:bsd-3)))

(define-public ghc-foundation
  (package
    (name "ghc-foundation")
    (version "0.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "foundation/foundation-" version ".tar.gz"))
       (sha256
        (base32
         "0bg4g0xf4pb2vmahnfp8c4f0a3v0av73lb5g8bwnp170khxfcsms"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-basement" ,ghc-basement)))
    (home-page "https://github.com/haskell-foundation/foundation")
    (synopsis "Alternative prelude with batteries and no dependencies")
    (description
     "This package provides a custom prelude with no dependencies apart from
the base package.

Foundation has the following goals:

@enumerate
@item provide a base like sets of modules that provide a consistent set of
   features and bugfixes across multiple versions of GHC (unlike base).
@item provide a better and more efficient prelude than base's prelude.
@item be self-sufficient: no external dependencies apart from base;
@item provide better data-types: packed unicode string by default, arrays;
@item Numerical classes that better represent mathematical things (no more
   all-in-one @code{Num});
@item I/O system with less lazy IO.
@end enumerate\n")
    (license license:bsd-3)))

(define-public ghc-stm-chans
  (package
    (name "ghc-stm-chans")
    (version "3.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "stm-chans-" version "/"
                           "stm-chans-" version ".tar.gz"))
       (sha256
        (base32
         "0f27sp09yha43xk9q55sc185jyjs5h7gq2dhsyx6bm9kz9dzqi13"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-stm" ,ghc-stm)))
    (home-page "https://hackage.haskell.org/package/stm-chans")
    (synopsis "Additional types of channels for ghc-stm")
    (description "This Haskell package offers a collection of channel types,
similar to @code{Control.Concurrent.STM.@{TChan,TQueue@}} but with additional
features.")
    (license license:bsd-3)))

(define-public ghc-monad-loops
  (package
    (name "ghc-monad-loops")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "monad-loops-" version "/"
                           "monad-loops-" version ".tar.gz"))
       (sha256
        (base32
         "062c2sn3hc8h50p1mhqkpyv6x8dydz2zh3ridvlfjq9nqimszaky"))))
    (build-system haskell-build-system)
    (native-inputs `(("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/mokus0/monad-loops")
    (synopsis "Monadic loops for Haskell")
    (description "This Haskell package provides some useful control
operators for looping.")
    (license license:public-domain)))

(define-public ghc-monad-logger
  (package
    (name "ghc-monad-logger")
    (version "0.3.25.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "monad-logger-" version "/"
                           "monad-logger-" version ".tar.gz"))
       (sha256
        (base32
         "0yv4fsi566zrn30j2g5l901lyqgmflhvzy4hji7ikcbh5d45m920"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-transformers-compat" ,ghc-transformers-compat)
              ("ghc-text" ,ghc-text)
              ("ghc-stm" ,ghc-stm)
              ("ghc-stm-chans" ,ghc-stm-chans)
              ("ghc-lifted-base" ,ghc-lifted-base)
              ("ghc-resourcet" ,ghc-resourcet)
              ("ghc-conduit" ,ghc-conduit)
              ("ghc-conduit-extra" ,ghc-conduit-extra)
              ("ghc-fast-logger" ,ghc-fast-logger)
              ("ghc-transformers-base" ,ghc-transformers-base)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-monad-loops" ,ghc-monad-loops)
              ("ghc-mtl" ,ghc-mtl)
              ("ghc-blaze-builder" ,ghc-blaze-builder)
              ("ghc-exceptions" ,ghc-exceptions)))
    (home-page "https://github.com/kazu-yamamoto/logger")
    (synopsis "Provides a class of monads which can log messages for Haskell")
    (description "This Haskell package uses a monad transformer approach
for logging.

This package provides Template Haskell functions for determining source
code locations of messages.")
    (license license:expat)))

(define-public ghc-shakespeare
  (package
    (name "ghc-shakespeare")
    (version "2.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "shakespeare-" version "/"
                           "shakespeare-" version ".tar.gz"))
       (sha256
        (base32
         "0j5zx8ka7d7scvb9shm7k3376qzl3k4kpim9aqqfs6n86901zpl4"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-parsec" ,ghc-parsec)
              ("ghc-text" ,ghc-text)
              ("ghc-aeson" ,ghc-aeson)
              ("ghc-blaze-markup" ,ghc-blaze-markup)
              ("ghc-blaze-html" ,ghc-blaze-html)
              ("ghc-exceptions" ,ghc-exceptions)
              ("ghc-vector" ,ghc-vector)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-scientific" ,ghc-scientific)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-hunit" ,ghc-hunit)
                     ("hspec-discover" ,hspec-discover)))
    (home-page "https://www.yesodweb.com/book/shakespearean-templates")
    (synopsis "Family of type-safe template languages for Haskell")
    (description "This Haskell package provides a family of type-safe
templates with simple variable interpolation.  Shakespeare templates can
be used inline with a quasi-quoter or in an external file and it
interpolates variables according to the type being inserted.")
    (license license:expat)))

(define-public ghc-securemem
  (package
    (name "ghc-securemem")
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "securemem-" version "/"
                           "securemem-" version ".tar.gz"))
       (sha256
        (base32
         "0dkhhjxa7njc3qbgvd5a23rkvr39vj2kn2a9nk6yjg7a8b2hvdpy"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-byteable" ,ghc-byteable)
              ("ghc-memory" ,ghc-memory)))
    (home-page "https://github.com/vincenthz/hs-securemem")
    (synopsis "Auto-scrubbing and const-time-eq memory chunk abstraction for
Haskell")
    (description "SecureMem is similar to ByteString, except that it provides
a memory chunk that will be auto-scrubbed after it run out of scope.")
    (license license:bsd-3)))

(define-public ghc-resource-pool
  (package
    (name "ghc-resource-pool")
    (version "0.2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "resource-pool-" version "/"
                           "resource-pool-" version ".tar.gz"))
       (sha256
        (base32
         "04mw8b9djb14zp4rdi6h7mc3zizh597ffiinfbr4m0m8psifw9w6"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-hashable" ,ghc-hashable)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-transformers-base" ,ghc-transformers-base)
              ("ghc-stm" ,ghc-stm)
              ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/bos/pool")
    (synopsis "Striped resource pooling implementation in Haskell")
    (description "This Haskell package provides striped pooling abstraction
for managing flexibly-sized collections of resources such as database
connections.")
    (license license:bsd-3)))

(define-public ghc-attoparsec-iso8601
  (package
    (name "ghc-attoparsec-iso8601")
    (version "1.0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "attoparsec-iso8601-" version "/"
                           "attoparsec-iso8601-" version ".tar.gz"))
       (sha256
        (base32
         "12l55b76bhya9q89mfmqmy6sl5v39b6gzrw5rf3f70vkb23nsv5a"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-attoparsec" ,ghc-attoparsec)
              ("ghc-base-compat" ,ghc-base-compat)
              ("ghc-text" ,ghc-text)))
    (home-page "https://github.com/bos/aeson")
    (synopsis "Parse ISO 8601 dates")
    (description "Haskell library for parsing of ISO 8601 dates, originally
from aeson.")
    (license license:bsd-3)))

(define-public ghc-generics-sop
  (package
    (name "ghc-generics-sop")
    (version "0.3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "generics-sop-" version "/"
                           "generics-sop-" version ".tar.gz"))
       (sha256
        (base32
         "1bazlhgmxcwv7vd44jhdx74cnhmaz6yy47jxfycapjj4mjrnp0x7"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/well-typed/generics-sop")
    (synopsis "Generic Programming using True Sums of Products for Haskell")
    (description "This Haskell package supports the definition of generic
functions.  Datatypes are viewed in a uniform, structured way: the choice
between constructors is represented using an n-ary sum, and the arguments of
each constructor are represented using an n-ary product.")
    (license license:bsd-3)))

(define-public ghc-uri-bytestring
  (package
    (name "ghc-uri-bytestring")
    (version "0.3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "uri-bytestring-" version "/"
                           "uri-bytestring-" version ".tar.gz"))
       (sha256
        (base32
         "04qjv1sgyrdg538290p9hqnvyxnahvr5cjwl8vm1rn9j0fv3ymq9"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-attoparsec" ,ghc-attoparsec)
              ("ghc-fail" ,ghc-fail)
              ("ghc-blaze-builder" ,ghc-blaze-builder)
              ("ghc-th-lift-instances" ,ghc-th-lift-instances)))
    (native-inputs `(("ghc-attoparsec" ,ghc-attoparsec)
                     ("ghc-hunit" ,ghc-hunit)
                     ("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)
                     ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
                     ("ghc-base-compat" ,ghc-base-compat)
                     ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
                     ("ghc-semigroups" ,ghc-semigroups)
                     ("ghc-generics-sop" ,ghc-generics-sop)))
    (home-page "https://github.com/Soostone/uri-bytestring")
    (synopsis "Haskell URI parsing as ByteStrings")
    (description "This Haskell package aims to be an RFC3986 compliant URI
parser that uses ByteStrings for parsing and representing the URI data.")
    (license license:bsd-3)))

(define-public ghc-http-api-data
  (package
    (name "ghc-http-api-data")
    (version "0.3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-api-data-" version "/"
                           "http-api-data-" version ".tar.gz"))
       (sha256
        (base32
         "1zbmf0kkfsw7pfznisi205gh7jd284gfarxsyiavd2iw26akwqwc"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))  ;  FIXME: Tests require QuickCheck >= 2.9
    (inputs `(("ghc-attoparsec" ,ghc-attoparsec)
              ("ghc-attoparsec-iso8601" ,ghc-attoparsec-iso8601)
              ("ghc-hashable" ,ghc-hashable)
              ("ghc-http-types" ,ghc-http-types)
              ("ghc-text" ,ghc-text)
              ("ghc-time-locale-compat" ,ghc-time-locale-compat)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-uri-bytestring" ,ghc-uri-bytestring)
              ("ghc-uuid-types" ,ghc-uuid-types)))
    (home-page "https://github.com/fizruk/http-api-data")
    (synopsis "Convert to/from HTTP API data like URL pieces, headers and
query parameters")
    (description "This Haskell package defines typeclasses used for converting
Haskell data types to and from HTTP API data.")
    (license license:bsd-3)))

(define-public ghc-persistent
  (package
    (name "ghc-persistent")
    (version "2.7.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "persistent-" version "/"
                           "persistent-" version ".tar.gz"))
       (sha256
        (base32
         "1jbvavdvr9qz5ld7vf6l1jgiadhmxx6zc4vqsdk9ivfq6d5wlg1p"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-old-locale" ,ghc-old-locale)
              ("ghc-text" ,ghc-text)
              ("ghc-conduit" ,ghc-conduit)
              ("ghc-resourcet" ,ghc-resourcet)
              ("ghc-exceptions" ,ghc-exceptions)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-lifted-base" ,ghc-lifted-base)
              ("ghc-resource-pool" ,ghc-resource-pool)
              ("ghc-path-pieces" ,ghc-path-pieces)
              ("ghc-http-api-data" ,ghc-http-api-data)
              ("ghc-aeson" ,ghc-aeson)
              ("ghc-monad-logger" ,ghc-monad-logger)
              ("ghc-transformers-base" ,ghc-transformers-base)
              ("ghc-base64-bytestring" ,ghc-base64-bytestring)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-vector" ,ghc-vector)
              ("ghc-attoparsec" ,ghc-attoparsec)
              ("ghc-haskell-src-meta" ,ghc-haskell-src-meta)
              ("ghc-blaze-html" ,ghc-blaze-html)
              ("ghc-blaze-markup" ,ghc-blaze-markup)
              ("ghc-silently" ,ghc-silently)
              ("ghc-mtl" ,ghc-mtl)
              ("ghc-fast-logger" ,ghc-fast-logger)
              ("ghc-scientific" ,ghc-scientific)
              ("ghc-tagged" ,ghc-tagged)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://www.yesodweb.com/book/persistent")
    (synopsis "Type-safe, multi-backend data serialization for Haskell")
    (description "This Haskell package allows Haskell programs to access data
storage sytems like PostgreSQL, SQLite, MySQL and MongoDB in a type-safe
way.")
    (license license:expat)))

(define-public ghc-aeson-compat
  (package
    (name "ghc-aeson-compat")
    (version "0.3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "aeson-compat-" version "/"
                           "aeson-compat-" version ".tar.gz"))
       (sha256
        (base32
         "1jya3lm9imclhb8qqihv39hhb62vvs3qpws7pc5fc23vwg0hsx2r"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))  ;  FIXME: Tests require QuickCheck >= 2.10
    (inputs `(("ghc-base-compat" ,ghc-base-compat)
              ("ghc-aeson" ,ghc-aeson)
              ("ghc-attoparsec" ,ghc-attoparsec)
              ("ghc-attoparsec" ,ghc-attoparsec-iso8601)
              ("ghc-exceptions" ,ghc-exceptions)
              ("ghc-hashable" ,ghc-hashable)
              ("ghc-scientific" ,ghc-scientific)
              ("ghc-text" ,ghc-text)
              ("ghc-time-locale-compat" ,ghc-time-locale-compat)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-vector" ,ghc-vector)
              ("ghc-tagged" ,ghc-tagged)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-nats" ,ghc-nats)))
    (home-page "https://github.com/phadej/aeson-compat")
    (synopsis "Compatibility layer for ghc-aeson")
    (description "This Haskell package provides compatibility layer for
ghc-aeson.")
    (license license:bsd-3)))

(define-public ghc-persistent-template
  (package
    (name "ghc-persistent-template")
    (version "2.5.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "persistent-template-" version "/"
                           "persistent-template-" version ".tar.gz"))
       (sha256
        (base32
         "0449piw3n02q7dag7k1pakfmzmf3ms4wk1qmnagczpm1ckajinwd"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-persistent" ,ghc-persistent)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-text" ,ghc-text)
              ("ghc-aeson" ,ghc-aeson)
              ("ghc-aeson-compat" ,ghc-aeson-compat)
              ("ghc-monad-logger" ,ghc-monad-logger)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-tagged" ,ghc-tagged)
              ("ghc-path-pieces" ,ghc-path-pieces)
              ("ghc-http-api-data" ,ghc-http-api-data)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://www.yesodweb.com/book/persistent")
    (synopsis "Type-safe, non-relational, multi-backend persistence")
    (description "This Haskell package provides interfaces and helper
functions for the ghc-persistent package.")
    (license license:expat)))

(define-public ghc-unliftio-core
  (package
    (name "ghc-unliftio-core")
    (version "0.1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "unliftio-core-" version "/"
                           "unliftio-core-" version ".tar.gz"))
       (sha256
        (base32
         "1193fplsjm1lcr05xwvkj1rsyzx74i755f6kw3ikmxbsv0bv0l3m"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme")
    (synopsis "The MonadUnliftIO typeclass for unlifting monads to IO")
    (description "This Haskell package provides the core @code{MonadUnliftIO}
typeclass, instances for base and transformers, and basic utility
functions.")
    (license license:expat)))

(define-public ghc-microlens
  (package
    (name "ghc-microlens")
    (version "0.4.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "microlens-" version "/"
                           "microlens-" version ".tar.gz"))
       (sha256
        (base32
         "17qx2mbqdrlnkc3gxq8njbp7qw8nh51drmz6fc8khgj9bls5ni2k"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/aelve/microlens")
    (synopsis "Provides a tiny lens Haskell library with no dependencies")
    (description "This Haskell package provides a lens library, just like
@code{ghc-lens}, but smaller.  It provides essential lenses and
traversals (like @code{_1} and @code{_Just}), as well as ones which are simply
nice to have (like @code{each}, @code{at}, and @code{ix}), and some
combinators (like @code{failing} and @code{singular}), but everything else is
stripped.  As the result, this package has no dependencies.")
    (license license:bsd-3)))

(define-public ghc-microlens-th
  (package
    (name "ghc-microlens-th")
    (version "0.4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "microlens-th-" version "/"
                           "microlens-th-" version ".tar.gz"))
       (sha256
        (base32
         "15a12cqxlgbcn1n73zwrxnp2vfm8b0ma0a0sdd8zmjbs8zy3np4f"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-microlens" ,ghc-microlens)))
    (home-page
     "https://github.com/aelve/microlens")
    (synopsis "Automatic generation of record lenses for
@code{ghc-microlens}")
    (description "This Haskell package lets you automatically generate lenses
for data types; code was extracted from the lens package, and therefore
generated lenses are fully compatible with ones generated by lens (and can be
used both from lens and microlens).")
    (license license:bsd-3)))

(define-public ghc-unliftio
  (package
    (name "ghc-unliftio")
    (version "0.2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unliftio/unliftio-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0vpncmwaq5zb6bziqfns4qdgxmq8ky0rlxna2yngxp170s5zxx9z"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: hspec-discover not in PATH
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-stm" ,ghc-stm)
       ("ghc-unliftio-core" ,ghc-unliftio-core)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/fpco/unliftio")
    (synopsis "Provides MonadUnliftIO typecplass for unlifting monads to
IO (batteries included)")
    (description "This Haskell package provides the core @code{MonadUnliftIO}
typeclass, a number of common instances, and a collection of common functions
working with it.")
    (license license:expat)))

(define-public ghc-persistent-sqlite
  (package
    (name "ghc-persistent-sqlite")
    (version "2.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "persistent-sqlite-" version "/"
                           "persistent-sqlite-" version ".tar.gz"))
       (sha256
        (base32
         "16mc2ra0hbyyc8ckjlxxc11bpskdymbr8c3g6ih6wzik639xprbm"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-persistent" ,ghc-persistent)
              ("ghc-unliftio-core" ,ghc-unliftio-core)
              ("ghc-aeson" ,ghc-aeson)
              ("ghc-conduit" ,ghc-conduit)
              ("ghc-monad-logger" ,ghc-monad-logger)
              ("ghc-microlens-th" ,ghc-microlens-th)
              ("ghc-resourcet" ,ghc-resourcet)
              ("ghc-old-locale" ,ghc-old-locale)
              ("ghc-resource-pool" ,ghc-resource-pool)
              ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-persistent-template" ,ghc-persistent-template)
                     ("ghc-temporary" ,ghc-temporary)
                     ("ghc-text" ,ghc-text)))
    (home-page
     "https://www.yesodweb.com/book/persistent")
    (synopsis "Backend for the persistent library using sqlite3")
    (description "This Haskell package includes a thin sqlite3 wrapper based
on the direct-sqlite package, as well as the entire C library, so there are no
system dependencies.")
    (license license:expat)))

(define-public ghc-email-validate
  (package
    (name "ghc-email-validate")
    (version "2.3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "email-validate/email-validate-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0qvxysiap3r4mi3xff5nsk9qv6diqxfgwj186bypbamzvzlz0lav"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-doctest" ,ghc-doctest)))
    (home-page
     "https://github.com/Porges/email-validate-hs")
    (synopsis "Email address validator for Haskell")
    (description
     "This Haskell package provides a validator that can validate an email
address string against RFC 5322.")
    (license license:bsd-3)))

(define-public ghc-semigroupoids-5.2.2
  (package
    (inherit ghc-semigroupoids)
    (name "ghc-semigroupoids")
    (version "5.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "semigroupoids-" version "/semigroupoids-"
                                   version ".tar.gz"))
              (sha256
               (base32
                "17i96y4iqj8clcs090lf6k0ij3j16nj14vsfwz0mm9nd6i4gbpp4"))))
    (inputs `(("ghc-unordered-containers" ,ghc-unordered-containers)
              ,@(package-inputs ghc-semigroupoids)))))

(define-public ghc-bytes
  (package
   (name "ghc-bytes")
   (version "0.15.4")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append "https://hackage.haskell.org/package/bytes-"
                     version "/bytes-"
                     version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "121x3iqlm8pghw8cd9g30cnqbl7jrdpfjxdanmqdqllajw6xivrm"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-cereal" ,ghc-cereal)
             ("cabal-doctest" ,cabal-doctest)
             ("ghc-doctest" ,ghc-doctest-0.13)
             ("ghc-mtl" ,ghc-mtl)
             ("ghc-scientific" ,ghc-scientific)
             ("ghc-text" ,ghc-text)
             ("ghc-transformers-compat" ,ghc-transformers-compat)
             ("ghc-unordered-containers" ,ghc-unordered-containers)
             ("ghc-void" ,ghc-void)
             ("ghc-vector" ,ghc-vector)))
   (synopsis "Serialization between @code{binary} and @code{cereal}")
   (description "This package provides a simple compatibility shim that lets
you work with both @code{binary} and @code{cereal} with one chunk of
serialization code.")
   (home-page "https://hackage.haskell.org/package/bytes")
   (license license:bsd-3)))

(define-public ghc-disk-free-space
  (package
    (name "ghc-disk-free-space")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "disk-free-space/disk-free-space-"
                           version ".tar.gz"))
       (sha256
        (base32
         "07rqj8k1vh3cykq9yidpjxhgh1f7vgmjs6y1nv5kq2217ff4yypi"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/redneb/disk-free-space")
    (synopsis "Retrieve information about disk space usage")
    (description "A cross-platform library for retrieving information about
disk space usage.")
    (license license:bsd-3)))

(define-public ghc-xdg-basedir
  (package
    (name "ghc-xdg-basedir")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/xdg-basedir/"
             "xdg-basedir-" version ".tar.gz"))
       (sha256
        (base32
         "0azlzaxp2dn4l1nr7shsxah2magk1szf6fx0mv75az00qsjw6qg4"))))
    (build-system haskell-build-system)
    (home-page "http://github.com/willdonnelly/xdg-basedir")
    (synopsis "XDG Base Directory library for Haskell")
    (description "This package provides a library implementing the XDG Base Directory spec.")
    (license license:bsd-3)))

(define-public ghc-errorcall-eq-instance
  (package
    (name "ghc-errorcall-eq-instance")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "errorcall-eq-instance/errorcall-eq-instance-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0hqw82m8bbrxy5vgdwb83bhzdx070ibqrm9rshyja7cb808ahijm"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "http://hackage.haskell.org/package/errorcall-eq-instance")
    (synopsis "Orphan Eq instance for ErrorCall")
    (description
     "Prior to @code{base-4.7.0.0} there was no @code{Eq} instance for @code{ErrorCall}.
This package provides an orphan instance.")
    (license license:expat)))

(define-public ghc-missingh
  (package
    (name "ghc-missingh")
    (version "1.4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/MissingH/"
                           "MissingH-" version ".tar.gz"))
       (sha256
        (base32
         "0wcvgrmav480w7nf4bl14yi0jq2yzanysxwzwas9hpb28vyjlgr8"))))
    (build-system haskell-build-system)
    ;; Tests require the unmaintained testpack package, which depends on the
    ;; outdated QuickCheck version 2.7, which can no longer be built with
    ;; recent versions of GHC and Haskell libraries.
    (arguments '(#:tests? #f))
    (inputs
     `(("ghc-network" ,ghc-network)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-regex-compat" ,ghc-regex-compat)
       ("ghc-hslogger" ,ghc-hslogger)
       ("ghc-random" ,ghc-random)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-old-locale" ,ghc-old-locale)))
    (native-inputs
     `(("ghc-errorcall-eq-instance" ,ghc-errorcall-eq-instance)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "http://software.complete.org/missingh")
    (synopsis "Large utility library")
    (description
     "MissingH is a library of all sorts of utility functions for Haskell
programmers.  It is written in pure Haskell and thus should be extremely
portable and easy to use.")
    (license license:bsd-3)))

(define-public ghc-intervalmap
  (package
    (name "ghc-intervalmap")
    (version "0.6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/IntervalMap/"
                           "IntervalMap-" version ".tar.gz"))
       (sha256
        (base32
         "06hin9wf1by8aqa7820fsi2339bh82184frkwz3jsb9sqa0hszcg"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "http://www.chr-breitkopf.de/comp/IntervalMap")
    (synopsis "Containers for intervals, with efficient search")
    (description
     "This package provides ordered containers of intervals, with efficient
search for all keys containing a point or overlapping an interval.  See the
example code on the home page for a quick introduction.")
    (license license:bsd-3)))

(define-public ghc-operational
  (package
    (name "ghc-operational")
    (version "0.2.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/operational/"
                           "operational-" version ".tar.gz"))
       (sha256
        (base32
         "1x2abg2q9d26h1vzj40r6k7k3gqgappbs4g9d853vvg77837km4i"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-random" ,ghc-random)))
    (home-page "http://wiki.haskell.org/Operational")
    (synopsis "Implementation of difficult monads made easy with operational semantics")
    (description
     "This library makes it easy to implement monads with tricky control
flow. This is useful for: writing web applications in a sequential style,
programming games with a uniform interface for human and AI players and easy
replay capababilities, implementing fast parser monads, designing monadic
DSLs, etc.")
    (license license:bsd-3)))

(define-public ghc-gtk2hs-buildtools
  (package
    (name "ghc-gtk2hs-buildtools")
    (version "0.13.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "gtk2hs-buildtools/gtk2hs-buildtools-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0yg6xmylgpylmnh5g33qwwn5x9bqckdvvv4czqzd9vrr12lnnghg"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-random" ,ghc-random)
       ("ghc-hashtables" ,ghc-hashtables)))
    (native-inputs
     `(("ghc-alex" ,ghc-alex)
       ("ghc-happy" ,ghc-happy)))
    (home-page "http://projects.haskell.org/gtk2hs/")
    (synopsis "Tools to build the Gtk2Hs suite of user interface libraries")
    (description
     "This package provides a set of helper programs necessary to build the
Gtk2Hs suite of libraries. These tools include a modified c2hs binding tool
that is used to generate FFI declarations, a tool to build a type hierarchy
that mirrors the C type hierarchy of GObjects found in glib, and a generator
for signal declarations that are used to call back from C to Haskell. These
tools are not needed to actually run Gtk2Hs programs.")
    (license license:gpl2)))

(define-public ghc-chart
  (package
    (name "ghc-chart")
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/Chart/"
                           "Chart-" version ".tar.gz"))
       (sha256
        (base32
         "13s64fhb2pmkdmx5bkgbgcn25qjihs364fvr47a1dw25f804kiy1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-lens" ,ghc-lens)
       ("ghc-colour" ,ghc-colour)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-operational" ,ghc-operational)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/timbod7/haskell-chart/wiki")
    (synopsis "Library for generating 2D charts and plots")
    (description
     "This package provides a library for generating 2D charts and plots, with
backends provided by the @code{Cairo} and @code{Diagrams} libraries.")
    (license license:bsd-3)))

(define-public ghc-wcwidth
  (package
    (name "ghc-wcwidth")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/wcwidth/wcwidth-"
             version ".tar.gz"))
       (sha256
        (base32
         "1n1fq7v64b59ajf5g50iqj9sa34wm7s2j3viay0kxpmvlcv8gipz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-setlocale" ,ghc-setlocale)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-attoparsec" ,ghc-attoparsec)))
    (home-page "https://github.com/solidsnack/wcwidth/")
    (synopsis "Haskell bindings to wcwidth")
    (description "This package provides Haskell bindings to your system's
native wcwidth and a command line tool to examine the widths assigned by it.
The command line tool can compile a width table to Haskell code that assigns
widths to the Char type.")
    (license license:bsd-3)))

(define-public ghc-aula
(package
  (name "ghc-aula")
  (version "0.38.0")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://github.com/liqd/aula/archive/v"
             version
             ".tar.gz"))
      (sha256
        (base32 "0gpx7xmdq3h7qlqdadinil6hjsb3l49lgbl6s24sjxllb2lqn023"))))
  (build-system haskell-build-system)
  (inputs
    `(("ghc-juicypixels" ,ghc-juicypixels)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-acid-state" ,ghc-acid-state)
      ("ghc-aeson" ,ghc-aeson)
      ("ghc-aeson-pretty" ,ghc-aeson-pretty)
      ("ghc-aeson-qq" ,ghc-aeson-qq)
      ("ghc-basic-sop" ,ghc-basic-sop)
      ("ghc-case-insensitive" ,ghc-case-insensitive)
      ("ghc-cassava" ,ghc-cassava)
      ("ghc-cookie" ,ghc-cookie)
      ("ghc-cryptonite" ,ghc-cryptonite)
      ("ghc-css-syntax" ,ghc-css-syntax)
      ("ghc-digestive-functors"
       ,ghc-digestive-functors)
      ("ghc-digestive-functors-lucid"
       ,ghc-digestive-functors-lucid)
      ("ghc-ekg" ,ghc-ekg)
      ("ghc-ekg-core" ,ghc-ekg-core)
      ("ghc-elocrypt" ,ghc-elocrypt)
      ("ghc-email-validate" ,ghc-email-validate)
      ("ghc-functor-infix" ,ghc-functor-infix)
      ("ghc-generic-aeson" ,ghc-generic-aeson)
      ("ghc-generics-sop" ,ghc-generics-sop)
      ("ghc-http-client" ,ghc-http-client)
      ("ghc-http-media" ,ghc-http-media)
      ("ghc-http-types" ,ghc-http-types)
      ("ghc-lens" ,ghc-lens)
      ("ghc-lucid" ,ghc-lucid)
      ("ghc-mime-mail" ,ghc-mime-mail)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-parsec" ,ghc-parsec)
      ("ghc-pretty-show" ,ghc-pretty-show)
      ("ghc-quickcheck-instances"
       ,ghc-quickcheck-instances)
      ("ghc-random" ,ghc-random)
      ("ghc-safecopy" ,ghc-safecopy)
      ("ghc-scrypt" ,ghc-scrypt)
      ("ghc-servant" ,ghc-servant)
      ("ghc-servant-lucid" ,ghc-servant-lucid)
      ("ghc-servant-server" ,ghc-servant-server)
      ("ghc-stm" ,ghc-stm)
      ("ghc-string-conversions"
       ,ghc-string-conversions)
      ("ghc-tagsoup" ,ghc-tagsoup)
      ("ghc-text" ,ghc-text)
      ("ghc-text-icu" ,ghc-text-icu)
      ("ghc-thentos-cookie-session"
       ,ghc-thentos-cookie-session)
      ("ghc-unicode-properties"
       ,ghc-unicode-properties)
      ("ghc-unicode-transforms"
       ,ghc-unicode-transforms)
      ("ghc-vector" ,ghc-vector)
      ("ghc-wai" ,ghc-wai)
      ("ghc-wai-app-static" ,ghc-wai-app-static)
      ("ghc-wai-middleware-metrics"
       ,ghc-wai-middleware-metrics)
      ("ghc-warp" ,ghc-warp)
      ("ghc-xlsx" ,ghc-xlsx)
      ("ghc-xlsx-templater" ,ghc-xlsx-templater)
      ("ghc-yaml" ,ghc-yaml)
      ("ghc-zip-archive" ,ghc-zip-archive)
      ("ghc-extra" ,ghc-extra)
      ("ghc-fsnotify" ,ghc-fsnotify)
      ("ghc-hspec" ,ghc-hspec)
      ("ghc-aeson-extra" ,ghc-aeson-extra)
      ("ghc-unordered-containers"
       ,ghc-unordered-containers)))
  (native-inputs
    `(("ghc-hunit" ,ghc-hunit)
      ("ghc-juicypixels" ,ghc-juicypixels)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-acid-state" ,ghc-acid-state)
      ("ghc-aeson" ,ghc-aeson)
      ("ghc-aeson-pretty" ,ghc-aeson-pretty)
      ("ghc-aeson-qq" ,ghc-aeson-qq)
      ("ghc-basic-sop" ,ghc-basic-sop)
      ("ghc-case-insensitive" ,ghc-case-insensitive)
      ("ghc-cassava" ,ghc-cassava)
      ("ghc-cookie" ,ghc-cookie)
      ("ghc-cryptonite" ,ghc-cryptonite)
      ("ghc-css-syntax" ,ghc-css-syntax)
      ("ghc-digestive-functors"
       ,ghc-digestive-functors)
      ("ghc-digestive-functors-lucid"
       ,ghc-digestive-functors-lucid)
      ("ghc-ekg" ,ghc-ekg)
      ("ghc-ekg-core" ,ghc-ekg-core)
      ("ghc-elocrypt" ,ghc-elocrypt)
      ("ghc-email-validate" ,ghc-email-validate)
      ("ghc-free" ,ghc-free)
      ("ghc-functor-infix" ,ghc-functor-infix)
      ("ghc-generic-aeson" ,ghc-generic-aeson)
      ("ghc-generics-sop" ,ghc-generics-sop)
      ("ghc-hspec" ,ghc-hspec)
      ("ghc-hspec-core" ,ghc-hspec-core)
      ("ghc-hspec-wai" ,ghc-hspec-wai)
      ("ghc-http-client" ,ghc-http-client)
      ("ghc-http-media" ,ghc-http-media)
      ("ghc-http-types" ,ghc-http-types)
      ("ghc-lens" ,ghc-lens)
      ("ghc-lucid" ,ghc-lucid)
      ("ghc-mime-mail" ,ghc-mime-mail)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-parsec" ,ghc-parsec)
      ("ghc-pretty-show" ,ghc-pretty-show)
      ("ghc-quickcheck-instances"
       ,ghc-quickcheck-instances)
      ("ghc-random" ,ghc-random)
      ("ghc-safecopy" ,ghc-safecopy)
      ("ghc-scrypt" ,ghc-scrypt)
      ("ghc-servant" ,ghc-servant)
      ("ghc-servant-lucid" ,ghc-servant-lucid)
      ("ghc-servant-mock" ,ghc-servant-mock)
      ("ghc-servant-server" ,ghc-servant-server)
      ("ghc-stm" ,ghc-stm)
      ("ghc-string-conversions"
       ,ghc-string-conversions)
      ("ghc-tagsoup" ,ghc-tagsoup)
      ("ghc-temporary" ,ghc-temporary)
      ("ghc-text" ,ghc-text)
      ("ghc-text-icu" ,ghc-text-icu)
      ("ghc-thentos-cookie-session"
       ,ghc-thentos-cookie-session)
      ("ghc-unicode-properties"
       ,ghc-unicode-properties)
      ("ghc-unicode-transforms"
       ,ghc-unicode-transforms)
      ("ghc-unordered-containers"
       ,ghc-unordered-containers)
      ("ghc-vector" ,ghc-vector)
      ("ghc-wai" ,ghc-wai)
      ("ghc-wai-app-static" ,ghc-wai-app-static)
      ("ghc-wai-extra" ,ghc-wai-extra)
      ("ghc-wai-middleware-metrics"
       ,ghc-wai-middleware-metrics)
      ("ghc-warp" ,ghc-warp)
      ("ghc-webdriver" ,ghc-webdriver)
      ("ghc-wreq" ,ghc-wreq)
      ("ghc-xlsx" ,ghc-xlsx)
      ("ghc-xlsx-templater" ,ghc-xlsx-templater)
      ("ghc-yaml" ,ghc-yaml)
      ("ghc-zip-archive" ,ghc-zip-archive)))
  (home-page "https://github.com/liqd/aula#readme")
  (synopsis "bla")
  (description "bla")
  (license "agpl")))

;;; haskell.scm ends here
