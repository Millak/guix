;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2018-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2017 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2015, 2017, 2018, 2020, 2021, 2023, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2019, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Josh Marshall <joshua.r.marshall.1991@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022, 2023 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2022-2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Luis Felipe López Acevedo <luis.felipe.la@protonmail.com>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2023 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 Reza Housseini <reza@housseini.me>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Navid Afkhami <navid.afkhami@mdc-berlin.de>
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

(define-module (gnu packages check)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix deprecation)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public pict
  (package
    (name "pict")
    (version "3.7.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Microsoft/pict")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1f3xpcdwihlxd8lj5clzfiz4rybhzdib95nrsnjfl009gh6gbwh0"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'install
            (lambda _
              (install-file "pict" (string-append #$output "/bin"))
              (install-file "doc/pict.md"
                            (string-append #$output
                                           "/share/doc/pict-" #$version)))))))
    (native-inputs (list perl))
    (home-page "https://www.pairwise.org/")
    (synopsis "Pairwise Independent Combinatorial Tool")
    (description "PICT is a pairwise testing tool that generates test cases
and test configurations.  With PICT, you can generate tests that are more
effective than manually generated tests and in a fraction of the time required
by hands-on test case design.  PICT runs as a command line tool.  It takes a
model file detailing the parameters of the interface as an input and generates
a compact set of parameter value choices that represent the test cases you
should use to get comprehensive combinatorial coverage of your parameters.")
    (license license:expat)))

(define-public pedansee
  (package
    (name "pedansee")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.flyn.org/projects/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0lsg791x6n95pxg6vif8qfc46nqcamhjq3g0dl5xqf6imy7n3acd"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list clang pkg-config python-wrapper))
    (inputs
     (list glib))
    (synopsis "Code checker for C")
    (description "Pedansee checks C source files for compliance with a particular
programming style.  The style is currently defined by the pedansee source code
in the form of functions which walk each source file’s syntax tree.  You can
modify some aspects of this style through the use of regular expressions.")
    (home-page "https://www.flyn.org/projects/pedansee/")
    (license license:gpl3+)))

(define-public mutest
  (package
    (name "mutest")
    (version "0.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/ebassi/mutest")
         (commit "e6246c9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gdqwq6fvk06wld4rhnw5752hahrvhd69zrci045x25rwx90x26q"))))
    (build-system meson-build-system)
    (synopsis "Small C testing library")
    (description "Mutest aims to be a small unit testing library for C projects,
with an API heavily modelled on high level Behavior-Driver Development frameworks
like Jasmine or Mocha.")
    (home-page "https://ebassi.github.io/mutest/mutest.md.html")
    (license license:expat)))

(define-public check
  (package
    (name "check")
    (version "0.15.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/libcheck/check/releases/download/"
                          version "/check-" version ".tar.gz"))
      (sha256
       (base32
        "02m25y9m46pb6n46s51av62kpd936lkfv3b13kfpckgvmh5lxpm8"))
      (patches
       (list
        ;; This patch fixes some tests that would otherwise fail on
        ;; powerpc64le-linux.  Without this patch, the tests make certain
        ;; assumptions about floating point number precision that are not true
        ;; on that platform.
        ;;
        ;; TODO: Remove this patch when updating to the next check release,
        ;; since it will be included there.  See:
        ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47698
        (origin
          (method url-fetch)
          (uri
           (string-append "https://github.com/libcheck/check/commit/"
                          "4fbe702fa4f35bee8a90512f9f59d1441c4ae82e.patch"))
          (file-name (string-append name
                                    "-fix-test-precision-for-ppc.patch"))
          (sha256
           (base32
            "04qg1p9afdd6453k18qskazrvscysdcjz9j6w4i6p5x4xyma19v6")))))))
    (build-system gnu-build-system)
    (home-page "https://libcheck.github.io/check/")
    (synopsis "Unit test framework for C")
    (description
     "Check is a unit testing framework for C.  It features a simple
interface for defining unit tests, putting little in the way of the
developer.  Tests are run in a separate address space, so Check can
catch both assertion failures and code errors that cause segmentation
faults or other signals.  The output from unit tests can be used within
source code editors and IDEs.")
    (license license:lgpl2.1+)))

;; Some packages require older versions.  Removed once no longer needed.
(define-public check-0.14
  (package
    (inherit check)
    (version "0.14.0")
    (source (origin
              (inherit (package-source check))
              (method url-fetch)
              (uri (string-append "https://github.com/libcheck/check/releases"
                                  "/download/" version "/check-" version ".tar.gz"))
              (sha256
               (base32
                "02zkfiyklckmivrfvdsrlzvzphkdsgjrz3igncw05dv5pshhq3xx"))))))

(define-public check-0.12
  (package
   (inherit check)
   (version "0.12.0")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/libcheck/check/releases"
                                 "/download/" version "/check-" version ".tar.gz"))
             (sha256
              (base32
               "0d22h8xshmbpl9hba9ch3xj8vb9ybm5akpsbbh7yj07fic4h2hj6"))))))

;;; XXX: This project is abandoned upstream, and included in modern catch2
;;; releases.  It is still depended by the restinio test suite at this time,
;;; so keep it (see: https://github.com/Stiffstream/restinio/issues/181).
(define-public clara
  (package
    (name "clara")
    (version "1.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/catchorg/Clara")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08mlm9ax5d7wkmsihm1xnlgp7rfgff0bfl4ly4850xmrdaxmmkl3"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Un-bundle catch2.
                          (delete-file-recursively "third_party")
                          (substitute* "CMakeLists.txt"
                            (("include_directories\\( include third_party )")
                             "include_directories( include )"))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DCMAKE_CXX_FLAGS=-I"
                             (search-input-directory %build-inputs
                                                     "include/catch2")))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              (install-file (string-append #$source "/single_include/clara.hpp")
                            (string-append #$output "/include")))))))
    (native-inputs (list catch2))
    (home-page "https://github.com/catchorg/Clara")
    (synopsis "Simple command line parser for C++")
    (description "Clara is a simple to use, composable, command line parser
for C++ 11 and beyond implemented as a single-header library.")
    (license license:boost1.0)))

(define-public clitest
  (package
    (name "clitest")
    (version "0.4.0")
    (home-page "https://github.com/aureliojargas/clitest")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (patches (search-patches "clitest-grep-compat.patch"))
              (sha256
               (base32
                "1p745mxiq3hgi3ywfljs5sa1psi06awwjxzw0j9c2xx1b09yqv4a"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; This package is distributed as a single shell script and comes
          ;; without a proper build system.
          (delete 'configure)
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (invoke "./clitest" "test.md"))))
          (replace 'install
            (lambda _
              (install-file "clitest" (string-append #$output "/bin"))
              (install-file "README.md"
                            (string-append #$output "/share/doc/clitest-"
                                           #$(package-version this-package))))))))
    (native-inputs
     (list perl))                 ;for tests
    (inputs
     (list bash-minimal))
    (synopsis "Command line test tool")
    (description
     "@command{clitest} is a portable shell script that performs automatic
testing of Unix command lines.")
    (license license:expat)))

(define-public cunit
  (package
    (name "cunit")
    (version "2.1-3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/cunit/CUnit/"
                           version "/CUnit-" version ".tar.bz2"))
       (sha256
        (base32
         "057j82da9vv4li4z5ri3227ybd18nzyq81f6gsvhifs5z0vr3cpm"))))
    (build-system gnu-build-system)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   ;; XXX: The "bootstrap" phase detects the "bootstrap"
                   ;; script, but fails to execute it, so we bootstrap
                   ;; manually.
                   (replace 'bootstrap
                     (lambda _ (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     (list automake autoconf libtool))
    (home-page "https://cunit.sourceforge.net/")
    (synopsis "Automated testing framework for C")
    (description
     "CUnit is a lightweight system for writing, administering, and running
unit tests in C.  It provides C programmers with basic testing functionality
with a flexible variety of user interfaces.")
    (license license:gpl2+)))

(define-public cppunit
  (package
    (name "cppunit")
    (version "1.15.1")
    (source (origin
             (method url-fetch)
              (uri (string-append "http://dev-www.libreoffice.org/src/"
                                  name "-" version ".tar.gz"))
             (sha256
              (base32
               "19qpqzy66bq76wcyadmi3zahk5v1ll2kig1nvg96zx9padkcdic9"))))
    ;; Explicitly link with libdl. This is expected to be done by packages
    ;; relying on cppunit for their tests. However, not all of them do.
    ;; If we added the linker flag to such packages, we would pollute all
    ;; binaries, not only those used for testing.
    (arguments
     `(#:make-flags '("LDFLAGS=-ldl")))
    (build-system gnu-build-system)
    (home-page "https://wiki.freedesktop.org/www/Software/cppunit/")
    (synopsis "Unit testing framework for C++")
    (description "CppUnit is the C++ port of the famous JUnit framework for
unit testing.  Test output is in XML for automatic testing and GUI based for
supervised tests.")
    (license license:lgpl2.1))) ; no copyright notices. LGPL2.1 is in the tarball

(define-public shunit2
  (package
    (name "shunit2")
    (version "2.1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kward/shunit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08vs0jjl3pfh100sjlw31x4638xj7fghr0j2g1zfikba8n1f9491"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; no configure script
         (delete 'build)
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* "shunit2"
               (("#! /bin/sh") (string-append "#! " (which "sh")))
               (("/usr/bin/od") (which "od")))
             (substitute* "test_runner"
               (("/bin/sh") (which "sh"))
               (("/bin/bash") (which "bash")))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; This test is buggy in the build container.
               (delete-file "shunit2_misc_test.sh")
               (invoke "sh" "test_runner"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "shunit2"
                           (string-append (assoc-ref outputs "out")
                                          "/bin"))
             #t)))))
    (home-page "https://github.com/kward/shunit2")
    (synopsis "@code{xUnit} based unit testing for Unix shell scripts")
    (description "@code{shUnit2} was originally developed to provide a
consistent testing solution for @code{log4sh}, a shell based logging framework
similar to @code{log4j}.  It is designed to work in a similar manner to JUnit,
PyUnit and others.")
    (license license:asl2.0)))

;; When dependent packages upgraded to use newer version of catch, this one should
;; be removed.
(define-public catch-framework
  (package
    (name "catch")
    (version "1.3.5")                  ;Sub-minor is the build number
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/philsquared/Catch")
                    ;; Semi-arbitrary.
                    (commit "ae5ee2cf63d6d67bd1369b512d2a7b60b571c907")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1yfb3lxv929szqy1nw9xw3d45wzkppziqshkjxvrb1fdmf46x564"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (output (assoc-ref %outputs "out"))
                          (incdir (string-append output "/include"))
                          (docdir (string-append output "/share/doc/catch-"
                                                 ,version)))
                     (for-each mkdir-p (list incdir docdir))
                     (install-file (string-append source
                                                  "/single_include/catch.hpp")
                                   incdir)
                     (copy-recursively (string-append source "/docs")
                                       docdir)
                     #t))))
    (home-page "http://catch-lib.net/")
    (synopsis "Automated test framework for C++ and Objective-C")
    (description
     "Catch stands for C++ Automated Test Cases in Headers and is a
multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))

(define-public catch2-1
  (package
    (name "catch2")
    (version "1.12.2")
    (home-page "https://github.com/catchorg/Catch2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/catchorg/Catch2")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gdp5wm8khn02g2miz381llw3191k7309qj8s3jd6sasj01rhf23"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* '("include/internal/catch_fatal_condition.hpp"
                               "single_include/catch.hpp")
                  ;; In glibc 2.34 and later, SIGSTKSZ is no longer a
                  ;; compile-time constant.  Hard code a reasonably large
                  ;; value.
                  (("SIGSTKSZ")
                   "32768")))))
    (build-system cmake-build-system)
    (synopsis "Automated test framework for C++ and Objective-C")
    (description "Catch2 stands for C++ Automated Test Cases in Headers and is
a multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))

(define-public catch2
  (package
    (name "catch2")
    (version "2.13.8")
    (home-page "https://github.com/catchorg/Catch2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/catchorg/Catch2")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18a6d7rcb6ilhxd5dff32jkfdf2ik58pbywrv04ras70217kdq4c"))))
    (build-system cmake-build-system)
    (inputs
     (list python-wrapper))
    (synopsis "Automated test framework for C++ and Objective-C")
    (description "Catch2 stands for C++ Automated Test Cases in Headers and is
a multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))

(define-public cbehave
  (let ((commit "5deaea0eaaf52f1c5ccdac0c68c003988f348fb4")
        (revision "1"))
    (package
      (name "cbehave")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/bigwhite/cbehave")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0kicawxmxn059n3rmfc7r2q5wfjrqbr6lm8dmsi86ba76ak0f9gi"))
                (snippet
                 #~(begin
                     (for-each delete-file
                               '("aclocal.m4"
                                 "config.guess" "config.sub" "configure"
                                 "depcomp" "install-sh"
                                 "libtool" "ltmain.sh" "missing"
                                 "Makefile.in" "src/Makefile.in"
                                 "src/example/Makefile.in"))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:configure-flags #~(list "--enable-shared" "--disable-static")
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'bootstrap 'rename-configure.in
              (lambda _
                (rename-file "configure.in" "configure.ac")))
            (add-after 'rename-configure.in 'set-AM_PROG_AR
              (lambda _
                (substitute* "configure.ac"
                  (("^AC_PROG_LIBTOOL.*" orig)
                   (string-append "AM_PROG_AR\n" orig)))))
            (add-after 'set-AM_PROG_AR 'enable-tests
              (lambda _
                (let ((port (open-file "src/example/Makefile.am" "a")))
                  (display (string-append "\nTESTS = calculator_test"
                                          " text_editor_test string_test"
                                          " product_database_test mock_test\n")
                           port)
                  (close-port port))))
            (add-before 'check 'create-dummy-file
              (lambda _
                (invoke "touch" "src/example/foo.txt"))))))
      (native-inputs (list autoconf automake libtool))
      (home-page "https://github.com/bigwhite/cbehave")
      (synopsis "Behavior-driven development framework")
      (description "CBehave is a behavior-driven development implemented in C.
It allows the specification of behaviour scenarios using a given-when-then
pattern.")
      (license license:apsl2))))

(define-public catch2-3
  (package
    (name "catch2")
    (version "3.5.3")
    (home-page "https://github.com/catchorg/Catch2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/catchorg/Catch2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11yla93vm2896fzhm3fz8lk3y3iz5iy7vd6wa7wnwvhsfd2dbfq3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DCATCH_DEVELOPMENT_BUILD=ON"
              "-DCATCH_ENABLE_WERROR=OFF"
              "-DBUILD_SHARED_LIBS=ON")))
    (inputs (list python-wrapper))
    (synopsis "Automated test framework for C++ and Objective-C")
    (description "Catch2 stands for C++ Automated Test Cases in Headers and is
a multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))

(define-public cmdtest
  (package
    (name "cmdtest")
    ;; Use the latest commit (from 2019) in order to get Python 3 support.
    (version "0.32-14-gcdfe14e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.liw.fi/cmdtest/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yhcwsqcpckkq5kw3h07k0xg6infyiyzq9ni3nqphrzxis7hxjf1"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))          ;requires Python 2!
    (native-inputs
     (list python-coverage-test-runner python))
    (inputs
     (list python-cliapp python-markdown python-ttystatus))
    (home-page "https://liw.fi/cmdtest/")
    (synopsis "Black box Unix program tester")
    (description
     "@code{cmdtest} black box tests Unix command line tools.  Roughly, it is
given a command line and input files, and the expected output, and it verifies
that the command line produces the expected output.  If not, it reports a
problem, and shows the differences.")
    (license license:gpl3+)))

(define-public cmocka
  (package
    (name "cmocka")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cmocka.org/files/"
                                  (version-major+minor version) "/cmocka-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1dm8pdvkyfa8dsbz9bpq7wwgixjij4sii9bbn5sgvqjm5ljdik7h"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no test target
    (home-page "https://cmocka.org/")
    (synopsis "Unit testing framework for C")
    (description "Cmocka is a unit testing framework for C with support for
mock objects.  It only requires the standard C library, and works with
different compilers.  Cmocka supports several different message output formats
like Test Anything Protocol, Subunit, xUnit XML or the original cmockery output
format.")
    (license license:asl2.0)))

(define-public cppcheck
  (package
    (name "cppcheck")
    (version "2.10.3")
    (source (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/danmar/cppcheck")
             (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1xfxcg00rxjrb9m2k78yd3jjlldkciv67fsbmjb6n3l43hgfxb9k"))
      (patches (search-patches "cppcheck-disable-char-signedness-test.patch"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_TESTS=ON")))
    (home-page "https://cppcheck.sourceforge.io")
    (synopsis "Static C/C++ code analyzer")
    (description "Cppcheck is a static code analyzer for C and C++.  Unlike
C/C++ compilers and many other analysis tools it does not detect syntax errors
in the code.  Cppcheck primarily detects the types of bugs that the compilers
normally do not detect.  The goal is to detect only real errors in the code
(i.e. have zero false positives).")
    (license license:gpl3+)))

(define-public cukinia
  (package
    (name "cukinia")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/savoirfairelinux/cukinia")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i92b37w8kb0rzkazlnnhjjbh1l1nmk2yrjvar7rpl97i9gn212m"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; The test suite assumes the host system runs systemd, has a root user,
      ;; among other things (see:
      ;; https://github.com/savoirfairelinux/cukinia/issues/51).
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ;no configure script
          (delete 'build)               ;no build system
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./cukinia" "tests/testcases.conf"))))
          (replace 'install
            (lambda _
              (install-file "cukinia" (string-append #$output "/bin")))))))
    (home-page "https://github.com/savoirfairelinux/cukinia")
    (synopsis "Simple on-target system test framework")
    (description "Cukinia is designed to help GNU/Linux-based embedded
firmware developers run simple system-level validation tests on their
firmware.  Cukinia integrates well with embedded firmware generation
frameworks such as Buildroot and Yocto, and can be run manually or by your
favourite continuous integration framework.  Among Cukinia features are:
@itemize
@item simple to use
@item no dependencies other than BusyBox or GNU Coreutils
@item easy integration with CI/CD pipelines.
@end itemize")
    (license (list license:gpl3+ license:asl2.0)))) ;dual license

(define-public cxxtest
  (package
    (name "cxxtest")
    (version "4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/cxxtest/cxxtest/"
                                  version "/cxxtest-" version ".tar.gz"))
              (sha256
               (base32
                "1n7pbj4z9ivx005hqvivj9ddhq8awynzg6jishfbypf6j7ply58w"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-source
           (lambda _
             (chdir "python")
             #t))
         (add-after 'install 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include-dir (string-append out "/include/cxxtest")))
               (for-each (lambda (header-file)
                           (install-file header-file include-dir))
                         (find-files "../cxxtest"))
               #t)))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc-dir (string-append out "/share/doc/cxxtest")))
               (install-file "../README" doc-dir)
               (install-file "../doc/guide.txt" doc-dir)
               (copy-recursively "../sample" (string-append doc-dir "/sample"))
               #t))))))
    (propagated-inputs
     (list python-ply))
    (home-page "https://web.archive.org/web/20230604070022/http://cxxtest.com/")
    (synopsis "Unit testing framework for C++")
    (description "CxxTest is a unit testing framework for C++ that is similar
in spirit to JUnit, CppUnit, and xUnit.  CxxTest does not require precompiling
a CxxTest testing library, it employs no advanced features of C++ (e.g. RTTI)
and it supports a very flexible form of test discovery.")
    (license license:lgpl3+)))

(define-public doctest
  (package
    (name "doctest")
    (version "2.4.9")
    (home-page "https://github.com/onqtam/doctest")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pkpwwvskcr21p00zrbnxpddv34p605mls86qirqqdwggmws82ds"))))
    (build-system cmake-build-system)
    (synopsis "C++ test framework")
    (description
     "doctest is a single-header testing framework for C++11 and later.  It
has been designed to be fast, light and unintrusive.")
    (license license:expat)))

(define-public ftest
  ;; There aren't any releases and it looks more like a small side project.
  ;; It is included for completness to run tests for package utfcpp.
  (let ((commit "c4ad4af0946b73ce1a40cbc72205d15d196c7e06")
        (revision "0"))
    (package
      (name "ftest")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nemtrif/ftest")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jcd76zjhx5f2nsi80hj7gmywgpz1f7vcw8lv5yf7gx0l99dn86x"))))
      ;; No CMakeLists.txt file provided, only one to run tests
      (build-system copy-build-system)
      (arguments
       (list #:install-plan
             #~'(("ftest.h" "include/ftest/"))
             #:phases
             #~(modify-phases %standard-phases
                 (add-before 'install 'check
                   (lambda _
                     (with-directory-excursion "tests"
                       (invoke "cmake" ".")
                       (invoke "make")
                       (invoke "ctest")))))))
      (native-inputs (list cmake-minimal))
      (home-page "https://github.com/nemtrif/ftest")
      (synopsis "C++ testing framework")
      (description
       "This package provides a simple and limited unit-test framework for C++.")
      (license license:boost1.0))))

(define-public python-gixy
  ;; The 0.1.20 release is missing some important fixes.
  ;; XXX: Commit 'e9008dcbd11f43ccac109b0cf2bf98a94e76b449' breaks tests
  ;; since it improperly removes an import.
  (let ((commit "303eb6887ddecab18138b6e427b04ae77c41d2f1")
        (revision "0")
        (base-version "0.1.20"))
    (package
      (name "python-gixy")
      (version (git-version base-version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yandex/gixy")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0gymjcnvjx9snyrzdbmjnk93ibb161q72xam29vnl3yyac4r1330"))))
      (build-system pyproject-build-system)
      (native-inputs (list python-nose))
      (propagated-inputs
       (list python-cached-property python-configargparse
             python-jinja2 python-six
             ;; XXX: gixy is incompatible with pyparsing >= 3.x.
             ;; See <https://github.com/yandex/gixy/pull/132> and
             ;; <https://github.com/yandex/gixy/pull/122>.
             python-pyparsing-2.4.7))
      (home-page "https://github.com/yandex/gixy")
      (synopsis "Static NGINX configuration analyzer")
      (description "Gixy is a static analyzer whose main goal is to help
prevent common NGINX misconfigurations.  It provides the @command{gixy}
command.")
      (license license:mpl2.0))))

(define-public googletest
  (package
    (name "googletest")
    (version "1.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/googletest")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cv55x3amwrvfan9pr8dfnicwr8r6ar3yf6cg9v6nykd6m2v3qsv"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (native-inputs
     `(("python" ,python-wrapper)))
    (home-page "https://github.com/google/googletest/")
    (synopsis "Test discovery and XUnit test framework")
    (description "Google Test features an XUnit test framework, automated test
discovery, death tests, assertions, parameterized tests and XML test report
generation.")
    (license license:bsd-3)))

(define-public googletest-1.8
  (package
    (inherit googletest)
   (version "1.8.1")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/googletest")
                   (commit (string-append "release-" version))))
             (file-name (git-file-name "googletest" version))
             (sha256
              (base32
               "0270msj6n7mggh4xqqjp54kswbl7mkcc8px1p5dqdpmw5ngh9fzk"))))))

(define-public googlebenchmark
  (package
    (name "googlebenchmark")
    (version "1.8.3")
    (home-page "https://github.com/google/benchmark")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name "google-benchmark" version))
              (sha256
               (base32
                "1hf8xrdd9k57kw3mpdi68a78fd96vzdqv3179v2yy5dxx336ffw3"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON"
                               (string-append
                                "-DGOOGLETEST_PATH="
                                (assoc-ref %build-inputs "googletest")))))
    (inputs
     `(("googletest" ,(package-source googletest))))
    (synopsis "C++ library to support the benchmarking of functions")
    (description
     "The googlebenchmark C++ library support the benchmarking of functions,
similar to unit tests.")
    (license license:asl2.0)))

(define-public greatest
  (package
   (name "greatest")
   (version "1.5.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/silentbicycle/greatest")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "11rajkb5m7mlzi3i3v0i27k6rrjw3x8a7bl6fkc29igzpwfbxndy"))))
   (build-system copy-build-system)
   (arguments (list #:install-plan
                    #~'(("greatest.h" "include/"))))
   (home-page "https://github.com/silentbicycle/greatest")
   (synopsis "Single-header test system")
   (description "Greatest is a single-header test system for C, including
macros for defining tests, grouping them into suites, and providing a test
runner.  It is quite unopinionated with most of its features being optional.")
   (license license:isc)))

(define-public klee-uclibc
  (let ((commit "955d502cc1f0688e82348304b053ad787056c754"))
    (package
      (name "klee-uclibc")
      (version (git-version "20230612" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/klee/klee-uclibc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12fnr5mq80cxwvv09gi844mi31jgi8067swagxnlxlhxj4mi125j"))))
      (build-system gnu-build-system)
      (supported-systems '("x86_64-linux"))
      (arguments
       `(#:tests? #f ;upstream uClibc tests do not work in the fork
         #:strip-directories '() ;only ships a static library, so don't strip anything.
         #:phases (modify-phases %standard-phases
                    ;; Disable locales as these would have to be downloaded and
                    ;; shouldn't really be needed for symbolic execution either.
                    (add-after 'unpack 'patch-config
                      (lambda _
                        (substitute* "klee-premade-configs/x86_64/config"
                          (("UCLIBC_DOWNLOAD_PREGENERATED_LOCALE_DATA=y")
                           "UCLIBC_DOWNLOAD_PREGENERATED_LOCALE_DATA=n")
                          (("UCLIBC_PREGENERATED_LOCALE_DATA=y")
                           "UCLIBC_PREGENERATED_LOCALE_DATA=n")
                          (("UCLIBC_HAS_LOCALE=y")
                           "UCLIBC_HAS_LOCALE=n")
                          (("UCLIBC_HAS_XLOCALE=y")
                           "UCLIBC_HAS_XLOCALE=n"))))

                    ;; Upstream uses a custom non-GNU configure script written
                    ;; in Python, replace the default configure phase accordingly.
                    (replace 'configure
                      (lambda _
                        (invoke "./configure" "--make-llvm-lib"
                                "--enable-release")))

                    ;; Custom install phase to only install the libc.a file manually.
                    ;; This is the only file which is used/needed by KLEE itself.
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (install-file "lib/libc.a"
                                      (string-append (assoc-ref outputs "out")
                                                     "/lib/klee")))))))
      ;; ncurses is only needed for the `make menuconfig` interface.
      (native-inputs (list clang-13 llvm-13 python ncurses))
      (synopsis "Variant of uClibc tailored to symbolic execution")
      (description
       "Modified version of uClibc for symbolic execution of
Unix userland software.  This library can only be used in conjunction
with the @code{klee} package.")
      (home-page "https://klee-se.org/")
      (license license:lgpl2.1))))

(define-public klee
  (package
   (name "klee")
   (version "3.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/klee/klee")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1nma6dqi8chjb97llsa8mzyskgsg4dx56lm8j514j5wmr8vkafz6"))))
   (arguments
    (list
     #:strip-directories #~(list "bin") ;don't strip LLVM bitcode in /lib
     #:test-target "check"
     #:phases
     #~(modify-phases %standard-phases
                      (add-after 'unpack 'patch
                        (lambda _
                          ;; Allow specification of an absolute full path to uclibc.
                          (substitute* "CMakeLists.txt"
                            (("\\$\\{KLEE_UCLIBC_PATH\\}/lib/libc\\.a")
                             "${KLEE_UCLIBC_PATH}"))
                          ;; Make sure that we retain the value of the GUIX_PYTHONPATH
                          ;; environment variable in the test environmented created by
                          ;; python-lit.  Otherwise, the test scripts won't be able to
                          ;; find the python-tabulate dependency, causing test failures.
                          (substitute* "test/lit.cfg"
                            (("addEnv\\('PWD'\\)" env)
                             (string-append env "\n" "addEnv('GUIX_PYTHONPATH')")))))
                      (add-after 'install 'wrap-programs
                        (lambda* (#:key inputs outputs #:allow-other-keys)
                          (let* ((out (assoc-ref outputs "out"))
                                 (bin (string-append out "/bin"))
                                 (lib (string-append out "/lib")))
                            ;; Ensure that klee-stats finds its Python dependencies.
                            (wrap-program (string-append bin "/klee-stats")
                              `("GUIX_PYTHONPATH" ":" prefix
                                ,(search-path-as-string->list
                                   (getenv "GUIX_PYTHONPATH"))))
                            ;; Ensure that klee finds runtime libraries (e.g. uclibc).
                            (wrap-program (string-append bin "/klee")
                              `("KLEE_RUNTIME_LIBRARY_PATH" =
                                (,(string-append lib "/klee/runtime/"))))))))
     #:configure-flags
     #~(list "-DENABLE_UNIT_TESTS=ON"
             "-DENABLE_SYSTEM_TESTS=ON"
             (string-append "-DGTEST_SRC_DIR="
                            #+(package-source googletest))
             (string-append "-DGTEST_INCLUDE_DIR="
                            #+(package-source googletest) "/googletest/include")
             (string-append "-DLLVMCC="
                            (search-input-file %build-inputs "/bin/clang"))
             (string-append "-DLLVMCXX="
                            (search-input-file %build-inputs "/bin/clang++"))
             (string-append "-DKLEE_UCLIBC_PATH="
                            (search-input-file %build-inputs "/lib/klee/libc.a"))
             "-DENABLE_POSIX_RUNTIME=ON")))
   (native-inputs (list clang-13 llvm-13 python-lit))
   (inputs (list bash-minimal klee-uclibc gperftools sqlite z3 python python-tabulate))
   (build-system cmake-build-system)
   (supported-systems '("x86_64-linux"))
   (home-page "https://klee-se.org/")
   (synopsis "Symbolic execution engine")
   (description "KLEE is a symbolic virtual machine built on top of the LLVM
compiler infrastructure.")
   (license license:bsd-3)))

(define-public cpputest
  (package
    (name "cpputest")
    (version "4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cpputest/cpputest/releases/download/v"
                           version "/cpputest-" version ".tar.gz"))
       (sha256
        (base32
         "1xslavlb1974y5xvs8n1j9zkk05dlw8imy4saasrjlmibl895ii1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list googletest))
    (home-page "https://cpputest.github.io/")
    (synopsis "Unit testing and mocking framework for C/C++")
    (description
     "CppUTest is a C/C++ based unit xUnit test framework.  It is written in
C++ but is used in C and C++ projects and frequently used in embedded systems
but it works for any C/C++ project.")
    (license license:bsd-3)))

(define-public actionlint
  (package
    (name "actionlint")
    (version "1.6.26")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rhysd/actionlint")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j4ni2cryvqn3qim1r6q6sargh0wig6l4vjjwc40cgqvvkrdla04"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/rhysd/actionlint/cmd/actionlint"
       #:unpack-path "github.com/rhysd/actionlint"
       #:install-source? #f))
    (inputs (list go-github-com-fatih-color
                  go-github-com-mattn-go-colorable
                  go-github-com-mattn-go-runewidth
                  go-github-com-robfig-cron
                  go-golang-org-x-sync
                  go-golang-org-x-sync
                  go-gopkg-in-yaml-v3))
    (native-inputs (list go-github-com-google-go-cmp-cmp))
    (home-page "https://rhysd.github.io/actionlint/")
    (synopsis "Static checker for GitHub Actions workflow files")
    (description
     "actionlint is a static checker for GitHub Actions
workflow files.  Features include:

@itemize
@item Syntax check for workflow files to check unexpected or missing
keys following workflow syntax
@item Strong type check for @code{$@{@{ @}@}} expressions to catch
several semantic errors like access to not existing property, type
mismatches, ...
@item Actions usage check to check that inputs at @code{with:} and
outputs in @code{steps.@{id@}.outputs} are correct
@item Reusable workflow check to check inputs/outputs/secrets of
reusable workflows and workflow calls
@item shellcheck and pyflakes integrations for scripts at @code{run:}
@item Security checks; script injection by untrusted inputs,
hard-coded credentials
@item Other several useful checks; glob syntax validation,
dependencies check for @code{needs:}, runner label validation, cron
syntax validation, ...
@end itemize
")
    (license license:expat)))

(define-public python-parameterized
  (package
    (name "python-parameterized")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parameterized" version))
       (sha256
        (base32 "0p1vhfw552rgd7gb2vy4l4l4k8mnbdz7f3chgzvk0r0qsqvzzfs1"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "nosetests" "-v")
                          (format #t "test suite not run~%"))
                      #t)))))
    (native-inputs
     (list python-mock python-nose))
    (home-page "https://github.com/wolever/parameterized")
    (synopsis "Parameterized testing with any Python test framework")
    (description
     "Parameterized is a Python library that aims to fix parameterized testing
for every Python test framework.  It supports nose, py.test, and unittest.")
    (license license:bsd-2)))

(define-public python-minimock
  (package
    (name "python-minimock")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MiniMock" version))
       (sha256
        (base32
         "0k2sxb1ibnyg05iblz7zhbv825f1zk9906rab7883iqgvzmdzpsz"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/MiniMock")
    (synopsis "Simple Python library for using mock objects")
    (description "MiniMock is a simple library for building mock objects with
doctest.")
    (license license:expat)))

(define-public python-mock
  (package
    (name "python-mock")
    (version "3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mock" version))
       (sha256
        (base32
         "1hrp6j0yrx2xzylfv02qa8kph661m6yq4p0mc8fnimch9j4psrc3"))))
    (propagated-inputs
     (list python-six))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests require "pytest", which depends on this package.
     '(#:tests? #f))
    (home-page "https://github.com/testing-cabal/mock")
    (synopsis "Python mocking and patching library for testing")
    (description
     "Mock is a library for testing in Python.  It allows you to replace parts
of your system under test with mock objects and make assertions about how they
have been used.  This library is now part of Python (since Python 3.3),
available via the @code{unittest.mock} module.")
    (license license:expat)))

;;; This package is unmaintained (see the note at the top of doc/index.rst).
(define-public python-nose
  (package
    (name "python-nose")
    (version "1.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nose" version))
        (sha256
          (base32
            "164a43k7k2wsqqk1s6vavcdamvss4mz0vd6pwzv2h9n8rgwzxgzi"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'invoke-2to3
                    (lambda _
                      (invoke "2to3" "-w" "."))))))
    (home-page "https://readthedocs.org/docs/nose/")
    (synopsis "Python testing library")
    (description
     "Nose extends the unittest library to make testing easier.")
    (license license:lgpl2.0+)))

(define-public python-nose2
  (package
    (name "python-nose2")
    (version "0.14.0")
      (source
        (origin
          (method url-fetch)
          (uri (pypi-uri "nose2" version))
          (sha256
           (base32
            "1936fkrxg672bhp9i32ivna7jbydl9dpbhyn5f3059xrl1qdfa2w"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; Tests require nose2 itself.
                     (setenv "PYTHONPATH" (getcwd))
                     (invoke (string-append #$output "/bin/nose2") "-v")))))))
    (home-page "https://github.com/nose-devs/nose2")
    (synopsis "Next generation of nicer testing for Python")
    (description
     "Nose2 is the next generation of nicer testing for Python, based on the
plugins branch of unittest2.  Nose2 aims to improve on nose by providing a
better plugin api, being easier for users to configure, and simplifying internal
interfaces and processes.")
    (license license:bsd-2)))

(define-public python-unittest2
  (package
    (name "python-unittest2")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "unittest2" version))
       (patches
        (search-patches "python-unittest2-python3-compat.patch"
                        "python-unittest2-remove-argparse.patch"))
       (sha256
        (base32
         "0y855kmx7a8rnf81d3lh5lyxai1908xjp0laf4glwa4c8472m212"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "python" "-m" "unittest2" "discover" "--verbose")))))))
    (propagated-inputs
     (list python-six python-traceback2))
    (home-page "https://pypi.org/project/unittest2/")
    (synopsis "Python unit testing library")
    (description
     "Unittest2 is a replacement for the unittest module in the Python
standard library.")
    (license license:psfl)))

(define-public python-pytest
  (package
    (name "python-pytest")
    (version "7.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest" version))
       (sha256
        (base32
         "0f8c31v5r2kgjixvy267n0nhc4xsy65g3n9lz1i1377z5pn5ydjg"))
       (patches (search-patches "pytest-fix-unstrable-exception-test.patch"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pretend-version
            ;; The version string is usually derived via setuptools-scm, but
            ;; without the git metadata available, the version string is set to
            ;; '0.0.0'.
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                      #$(package-version this-package))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (setenv "TERM" "dumb")    ;attempt disabling markup tests
              (if tests?
                  (invoke "pytest" "-vv" "-k"
                          (string-append
                           ;; This test involves the /usr directory, and fails.
                           " not test_argcomplete"
                           ;; These test do not honor the isatty detection and
                           ;; fail.
                           " and not test_code_highlight"
                           " and not test_color_yes"))
                  (format #t "test suite not run~%")))))))
    (propagated-inputs
     (list python-attrs-bootstrap
           python-iniconfig
           python-packaging-bootstrap
           python-pluggy
           python-py
           python-tomli))
    (native-inputs
     ;; Tests need the "regular" bash since 'bash-final' lacks `compgen`.
     (list bash
           python-hypothesis
           python-nose
           python-pytest-bootstrap
           python-setuptools-scm
           python-xmlschema))
    (home-page "https://docs.pytest.org/en/latest/")
    (synopsis "Python testing library")
    (description
     "Pytest is a testing tool that provides auto-discovery of test modules
and functions, detailed info on failing assert statements, modular fixtures,
and many external plugins.")
    (license license:expat)))

(define-public python-pytest-8
  (package/inherit python-pytest
    (name "python-pytest")
    (version "8.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest" version))
       (sha256
        (base32 "0xvr25qvmdh6z03jpgg24adhgqkvkal2g2v8vk63j6909q8bhjyy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-append
                    "not test_code_highlight_continuation"
                    " and not test_code_highlight"
                    " and not test_code_highlight_custom_theme"
                    " and not test_code_highlight_invalid_theme"
                    " and not test_code_highlight_invalid_theme_mode"
                    " and not test_code_highlight_simple"
                    " and not test_color_yes"
                    " and not test_comparisons_handle_colors"
                    " and not test_empty_NO_COLOR_and_FORCE_COLOR_ignored"
                    " and not test_remove_dir_prefix"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-pytest)
       (append python-exceptiongroup)
       (replace "python-pluggy" python-pluggy-next)))))

(define-public python-pytest-next
  (package/inherit python-pytest
    (name "python-pytest")
    (version "7.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest" version))
       (sha256
        (base32 "02q32y67nflrmk9snmibq5kmqcbgfm29k9wm0yw0ia2vqly0m6gf"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-pytest)
       (append python-exceptiongroup)))))

(define-deprecated python-pytest-6 python-pytest)
(export python-pytest-6)

(define-deprecated python-pytest-7 python-pytest)
(export python-pytest-7)

(define-public python-pytest-bootstrap
  (package
    (inherit python-pytest)
    (name "python-pytest-bootstrap")
    (native-inputs (list python-iniconfig python-setuptools-scm
                         python-tomli))
    (arguments `(#:tests? #f))))

(define-public python-pytest-assume
  (package
    (name "python-pytest-assume")
    (version "2.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-assume" version))
       (sha256
        (base32 "0zilqsy9fcjr6l2f9qzfxpkp40h24csnjm5mifhpmzb0fr9r0glq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "pytest")))))))
    (propagated-inputs
     (list python-pytest python-six))
    (home-page "https://github.com/astraw38/pytest-assume")
    (synopsis "Pytest plugin that allows multiple failures per test")

    (description "This package provides a Pytest plugin that allows multiple
failures per test.  This is a fork from pytest-expect which includes the
following improvements:
@itemize
@item showlocals support (the Pytest option)
@item global usage support (a fixture is not required)
@item output refinements and tweaks.
@end itemize")
    (license license:expat)))

(define-public python-pytest-cov
  (package
    (name "python-pytest-cov")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-cov" version))
        (sha256
         (base32 "0w6lfv8gc1lxmnvsz7mq5z9shxac5zz6s9mwrai108kxc6qzbw77"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
          (lambda _
            ;; Options taken from tox.ini.
            ;; TODO: make "--restructuredtext" tests pass. They currently fail
            ;; with "Duplicate implicit target name".
            (invoke "python" "./setup.py" "check"
                    "--strict" "--metadata"))))))
    (propagated-inputs
     (list python-coverage python-pytest))
    (home-page "https://github.com/pytest-dev/pytest-cov")
    (synopsis "Pytest plugin for measuring coverage")
    (description
     "Pytest-cov produces coverage reports.  It supports centralised testing and
distributed testing in both @code{load} and @code{each} modes.  It also
supports coverage of subprocesses.")
  (license license:expat)))

(define-public python-pytest-relaxed
  (package
    (name "python-pytest-relaxed")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bitprophet/pytest-relaxed")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lnnkadfr390i30209gpl80nymc20pmamvxjhd11gvf4d6f54n7x"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-decorator))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/bitprophet/pytest-relaxed")
    (synopsis "Relaxed test discovery for pytest")
    (description "This package provides relaxed test discovery for pytest.")
    (license license:bsd-2)))

(define-public python-pytest-dotenv
  (package
    (name "python-pytest-dotenv")
    (version "0.5.2")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quiqua/pytest-dotenv")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bdxwaak5clhsd63b9q65nf2amqqv5hfn7dskfakyldxsqnnh0y6"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-dotenv))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/quiqua/pytest-dotenv")
    (synopsis "Automatically detect and load a .env file before running tests")
    (description
     "This Pytest plugin automatically detects and loads environment variables
from a .env file before running tests.")
    (license license:expat)))

(define-public python-pytest-examples
  (package
    (name "python-pytest-examples")
    (version "0.0.10")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pydantic/pytest-examples")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lwxgyfj6lnkhmrvb6kzfskpwfz70kxnhnjvyl3l65k568c4wb4c"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k"
              (string-append
               ;; Disable tests requiring pthon-ruff.
               "not test_ruff"
               " and not test_ruff_config"
               " and not test_ruff_offset"
               " and not test_ruff_ok"
               " and not test_ruff_error"
               " and not test_update_files"
               " and not test_cases_update[simple.md]"
               " and not test_cases_update[dataclass_indent.md]"
               " and not test_cases_update[long_python_lines.py]"
               " and not test_cases_update[simple.py]"
               " and not test_cases_update[python_class.py]"
               " and not test_cases_update[call_twice.md]"
               " and not test_insert_print[example/README.md:3-33]"
               " and not test_insert_print[example/README.md:37-40]"
               " and not test_insert_print[example/README.md:44-47]"
               " and not test_insert_print[example/README.md:49-66]"
               " and not test_python_self[example/test_example.py:28-31]"
               " and not test_python_self[example/test_example.py:40-43]"
               " and not test_python_self_change_docstyle[example/test_example.py:28-31]"
               " and not test_python_self_change_docstyle[example/test_example.py:40-43]"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                ;; XXX: Removing ruff from required packages to pass Sanity
                ;; check, add it back when it's available.
                (("'ruff>=0.0.258',") "")
                ;; black>=23
                ((">=23") ">22")))))))
    (propagated-inputs
     ;; TODO: Add python-ruff once it has been packaged.
     (list python-black python-pytest))
    (native-inputs
     (list python-hatchling))
    (home-page "https://pypi.org/project/pytest-examples/")
    (synopsis "Pytest plugin for testing examples in docstrings and markdown files")
    (description
     "Pytest-examples provides functionality for testing Python code examples
in docstrings and markdown files, with its main features being:

@itemize
@item lint code examples using ruff and black
@item run code examples
@item run code examples and check print statements are inlined correctly in
the code
@item It can also update code examples in place to format them and insert or
update print statements
@end itemize")
    (license license:expat)))

(define-public python-pytest-httpserver
  (package
    (name "python-pytest-httpserver")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest_httpserver" version))
              (sha256
               (base32
                "0vbls0j570l5my83j4jnk5blmnir44i0w511azlh41nl6k8rac5f"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-werkzeug))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-library-loading
           (lambda _
             (setenv "GUIX_PYTHONPATH" (string-append (getenv "GUIX_PYTHONPATH") ":."))))
         (replace 'check
           (lambda _
             (invoke "pytest" "tests" "-vv")
             (invoke "pytest" "tests" "-vv" "--ssl"))))))
    (home-page "https://github.com/csernazs/pytest-httpserver")
    (synopsis "HTTP server for pytest")
    (description "Pytest plugin library to test http clients without
contacting the real http server.")
    (license license:expat)))

(define-public python-pytest-nunit
  (package
    (name "python-pytest-nunit")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-nunit" version))
       (sha256
        (base32 "1gw3a33myq9yncjixs3kkcrr1xkjzvvf3xk6x955p3i79wlwkswx"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #false)) ;no tests included
    (propagated-inputs (list python-attrs python-pytest))
    (native-inputs (list python-pytest python-pytest-cov python-xmlschema))
    (home-page "https://github.com/pytest-dev/pytest-nunit")
    (synopsis "Pytest plugin for generating NUnit3 test result XML output")
    (description
     "This package provides a pytest plugin for generating NUnit3 test result
XML output")
    (license license:expat)))

(define-public python-pytest-param-files
  (package
    (name "python-pytest-param-files")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest_param_files" version))
              (sha256
               (base32
                "0gc9nsqizrjapjnbcs1bdxfcl69dpmwbpd9sssjidgcikm7k433c"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-flit-core))
    (propagated-inputs (list python-pytest))
    (home-page "https://github.com/chrisjsewell/pytest-param-files")
    (synopsis "Pytest plugin to parameterize tests from external files")
    (description "This Pytest plugin enables creating Pytest parametrize
decorators from external files.")
    (license license:expat)))

(define-public python-pytest-random-order
  (package
    (name "python-pytest-random-order")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-random-order" version))
       (sha256
        (base32 "0lpzl218l04vjy4gckrbrcacc3w9xrjnvz64bf2i132c58s5j8bb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "python" "-m" "pytest" "--random-order")))))))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/jbasko/pytest-random-order")
    (synopsis "Pytest plugin to randomize the order of tests")
    (description "@code{pytest-random-order} is a Pytest plugin that
randomizes the order of tests.  This can be useful to detect a test that
passes just because it happens to run after an unrelated test that leaves the
system in a favourable state.  The plugin allows user to control the level of
randomness they want to introduce and to disable reordering on subsets of
tests.  Tests can be rerun in a specific order by passing a seed value
reported in a previous test run.")
    (license license:expat)))

(define-public python-pytest-randomly
  (package
    (name "python-pytest-randomly")
    (version "3.11.0")
    (source (origin
              (method git-fetch)        ;no tests in pypi archive
              (uri (git-reference
                    (url "https://github.com/pytest-dev/pytest-randomly")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sjgq49g8f8973vhmzrim79b6wz29a765n99azjk1maimqh7mmik"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; The tests validating ordering fail, as well as as two
                ;; others, for unknown reasons (see:
                ;; https://github.com/pytest-dev/pytest-randomly/issues/454).
                (invoke "pytest" "-vv" "-k"
                        (string-append
                         "not reordered "
                         "and not test_it_runs_before_stepwise "
                         "and not test_entrypoint_injection"))))))))
    (native-inputs (list python-coverage
                         python-factory-boy
                         python-faker
                         python-numpy
                         python-pytest-xdist))
    (propagated-inputs (list python-importlib-metadata python-pytest))
    (home-page "https://github.com/pytest-dev/pytest-randomly")
    (synopsis "Pytest plugin to randomly order tests")
    (description "This is a Pytest plugin to randomly order tests and control
Python's @code{random.seed}.")
    (license license:expat)))

(define-public python-pytest-runner
  (package
    (name "python-pytest-runner")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-runner" version))
       (sha256
        (base32
         "11dnhxnjmh4nf1j8rnvx944ha3wg8ggrgrwdcx4c7d19xmi57n5l"))))
    (build-system python-build-system)
    (arguments
     (list
      ;; FIXME: The test suite requires 'python-flake8' and 'python-black',
      ;; but that introduces a circular dependency.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda _
              (let ((circa-1980 (* 10 366 24 60 60)))
                (setenv "SOURCE_DATE_EPOCH" (number->string circa-1980))
                (invoke "python" "-m" "build" "--wheel" "--no-isolation" "."))))
          (replace 'install
            (lambda _
              (let ((whl (car (find-files "dist" "\\.whl$"))))
                (invoke "pip" "--no-cache-dir" "--no-input"
                        "install" "--no-deps" "--prefix" #$output whl))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (invoke "pytest" "-vv")
                  (format #t "test suite not run~%")))))))
    (native-inputs
     (list python-pypa-build python-setuptools-scm python-wheel))
    (home-page "https://github.com/pytest-dev/pytest-runner")
    (synopsis "Invoke py.test as a distutils command")
    (description
     "This package provides a @command{pytest-runner} command that
@file{setup.py} files can use to run tests.")
    (license license:expat)))

(define-public python-pytest-lazy-fixture
  (package
    (name "python-pytest-lazy-fixture")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-lazy-fixture" version))
        (sha256
         (base32 "1b0hmnsxw4s2wf9pks8dg6dfy5cx3zcbzs8517lfccxsfizhqz8f"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make the installed plugin discoverable by Pytest.
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/tvorog/pytest-lazy-fixture")
    (synopsis "Use fixtures in @code{pytest.mark.parametrize}")
    (description "This plugin helps to use fixtures in
@code{pytest.mark.parametrize}.")
    (license license:expat)))

(define-public python-pytest-mock
  (package
    (name "python-pytest-mock")
    (version "3.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mock" version))
       (sha256
        (base32 "0kzdwwdjw001qzf1n4qzh7c364rvmb0cmkfqdwr2l9bwxy2v1ggv"))
       (modules '((guix build utils)))
       (snippet
        ;; Some tests do a string match on Pytest output, and fails when
        ;; warnings are present.  Adjust to cope with warnings from
        ;; third-party libraries (looking at you, pytest-asyncio).
        '(substitute* "tests/test_pytest_mock.py"
           (("1 passed in \\*")
            "1 passed*")))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Skip the assertion rewriting tests, which don't work in the
               ;; presence of read-only Python modules (a limitation of
               ;; Pytest).  Also skip the "test_standalone_mock" test, which
               ;; can only work when 'python-mock' is not available
               ;; (currently propagated by Pytest 5).
               (invoke "pytest" "--assert=plain" "-vv"
                       "-k" "not test_standalone_mock")))))))
    (native-inputs
     (list python-pytest-asyncio python-setuptools-scm))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/pytest-dev/pytest-mock/")
    (synopsis "Thin-wrapper around the mock package for easier use with py.test")
    (description
     "This plugin installs a @code{mocker} fixture which is a thin-wrapper
around the patching API provided by the @code{mock} package, but with the
benefit of not having to worry about undoing patches at the end of a test.
The mocker fixture has the same API as @code{mock.patch}, supporting the
same arguments.")
    (license license:expat)))

(define-public python-pytest-xdist
  (package
    (name "python-pytest-xdist")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-xdist" version))
       (sha256
        (base32
         "1psf5dqxvc38qzxvc305mkg5xpdmdkbkkfiyqlmdnkgh7z5dx025"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools-scm python-filelock python-pytest))
    (propagated-inputs (list python-execnet python-pytest-forked))
    (home-page "https://github.com/pytest-dev/pytest-xdist")
    (synopsis
     "Plugin for py.test with distributed testing and loop-on-failing modes")
    (description
     "The pytest-xdist plugin extends py.test with some unique test execution
modes: parallelization, running tests in boxed subprocesses, the ability
to run tests repeatedly when failed, and the ability to run tests on multiple
Python interpreters or platforms.  It uses rsync to copy the existing
program code to a remote location, executes there, and then syncs the
result back.")
    (license license:expat)))

(define-public python-pytest-timeout
  (package
    (name "python-pytest-timeout")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-timeout" version))
       (sha256
        (base32
         "1nf339zg6qam3681f72j9c8fbqk8qcilna92psmzh4n60isa0z60"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Make the installed plugin discoverable by Pytest.
                      (add-installed-pythonpath inputs outputs)
                      (invoke "pytest" "-vv"))))))
    (propagated-inputs
     (list python-pytest python-pytest-cov))
    (native-inputs
     (list python-pexpect))
    (home-page "https://github.com/pytest-dev/pytest-timeout")
    (synopsis "Plugin for py.test to abort hanging tests")
    (description
     "This package provides a py.test plugin that aborts hanging tests after a
timeout has been exceeded.")
    (license license:expat)))

(define-public python-pytest-forked
  (package
    (name "python-pytest-forked")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)               ;for tests
       (uri (git-reference
             (url "https://github.com/pytest-dev/pytest-forked")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1y93q914gwf0nshql1qix6sj826q163b04vw17zmwhsnbv00c2d3"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'pretend-version
                 ;; The version string is usually derived via setuptools-scm,
                 ;; but without the git metadata available, the version string
                 ;; is set to '0.0.0'.
                 (lambda _
                   (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     ;; XXX: The bootstrap variant of Pytest is used to ensure the
     ;; 'hypothesis' plugin is not in the environment (due to
     ;; <http://issues.guix.gnu.org/25235>), which would cause the test suite
     ;; to fail (see: https://github.com/pytest-dev/pytest-forked/issues/54).
     (list python-pytest-bootstrap python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-forked")
    (synopsis "Pytest plugin to run tests in isolated forked subprocesses")
    (description "This package provides a Pytest plugin which enables running
each test in a subprocess and will report if a test crashed the process.  It
can be useful to isolate tests against undesirable global environment
side-effects (such as setting environment variables).")
    (license license:expat)))

(define-public python-scripttest
  (package
    (name "python-scripttest")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scripttest" version))
       (sha256
        (base32
         "0f4w84k8ck82syys7yg9maz93mqzc8p5ymis941x034v44jzq74m"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (home-page (string-append "https://web.archive.org/web/20161029233413/"
                              "http://pythonpaste.org/scripttest/"))
    (synopsis "Python library to test command-line scripts")
    (description "Scripttest is a Python helper library for testing
interactive command-line applications.  With it you can run a script in a
subprocess and see the output as well as any file modifications.")
    (license license:expat)))

(define-public python-testtools-bootstrap
  (package
    (name "python-testtools-bootstrap")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testtools" version))
       (sha256
        (base32
         "02mkphygx8897617m8qnmj0alksyvvfcjmazzfxyrlzjq0a5xdi8"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-fixtures" ,python-fixtures-bootstrap)
       ("python-pbr" ,python-pbr-minimal)))
    (home-page "https://github.com/testing-cabal/testtools")
    (synopsis
     "Extensions to the Python standard library unit testing framework")
    (description
     "This package is only for bootstrapping.  Do not use this.")
    (license license:psfl)))

(define-public python-testtools
  (package
    (inherit python-testtools-bootstrap)
    (name "python-testtools")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "testtools.run"
                       "testtools.tests.test_suite")))))))
    (propagated-inputs
     (list python-fixtures python-pbr))
    (native-inputs
     `(("python-testscenarios" ,python-testscenarios-bootstrap)))
    (description
     "Testtools extends the Python standard library unit testing framework to
provide matchers, more debugging information, and cross-Python
compatibility.")))

(define-public python-testscenarios-bootstrap
  (package
    (name "python-testscenarios-bootstrap")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testscenarios" version))
       (sha256
        (base32
         "1dm2aydqpv76vnsk1pw7k8n42hq58cfi4n1ixy7nyzpaj1mwnmy2"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "python" "-m" "testtools.run"
                              "testscenarios.test_suite"))))))
    (propagated-inputs
     `(("python-pbr" ,python-pbr-minimal)
       ("python-testtools" ,python-testtools-bootstrap)))
    (home-page "https://launchpad.net/testscenarios")
    (synopsis "Pyunit extension for dependency injection")
    (description
     "This package is only for bootstrapping.  Don't use this.")
    (license (list license:bsd-3 license:asl2.0)))) ; at the user's option

(define-public python-testscenarios
  (package
    (inherit python-testscenarios-bootstrap)
    (name "python-testscenarios")
    (propagated-inputs
     (list python-pbr python-testtools))
    (description
     "Testscenarios provides clean dependency injection for Python unittest
style tests.")))

;; Testresources requires python-pbr at runtime, but pbr needs it for its
;; own tests.  Hence this bootstrap variant.
(define-public python-testresources-bootstrap
  (package
    (name "python-testresources-bootstrap")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "testresources" version))
              (sha256
               (base32
                "05s4dsli9g17m1r3b1gvwicbbgq011hnpb2b9qnj27ja2n11k7gf"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-pbr" ,python-pbr-minimal)))
    (home-page "https://launchpad.net/testresources")
    (synopsis
     "Pyunit extension for managing test resources")
    (description
     "This package is only here for bootstrapping purposes.  Use the regular
testresources package instead.")
    (license (list license:bsd-3 license:asl2.0)))) ; at the user's option

(define-public python-testresources
  (package
    (inherit python-testresources-bootstrap)
    (name "python-testresources")
    (propagated-inputs
     (list python-pbr))
    (arguments '())
    (native-inputs
     (list python-fixtures python-testtools))
    (description
     "Testresources is an extension to Python's unittest to allow declarative
use of resources by test cases.")))

(define-public python-subunit-bootstrap
  (package
    (name "python-subunit-bootstrap")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-subunit" version))
       (sha256
        (base32
         "0j0ymmnc5nfxi1qzvy59j27viqca7l7xd0y9x29g7yr0h693j804"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-extras python-testtools-bootstrap))
    (native-inputs
     (list python-fixtures-bootstrap python-hypothesis
           python-testscenarios-bootstrap))
    (home-page "https://launchpad.net/subunit")
    (synopsis "Python implementation of the subunit protocol")
    (description
     "This package is here for bootstrapping purposes only.  Use the regular
python-subunit package instead.")
    (license (list license:bsd-3 license:asl2.0)))) ; at the user's option

(define-public python-subunit
  (package
    (inherit python-subunit-bootstrap)
    (name "python-subunit")
    (propagated-inputs
     (list python-extras python-testtools))
    (native-inputs
     (list python-fixtures python-hypothesis python-testscenarios))
    (description
     "Python-subunit is a Python implementation of the subunit test streaming
protocol.")))

;; Fixtures requires python-pbr at runtime, but pbr uses fixtures for its
;; own tests.  Hence this bootstrap variant.
(define-public python-fixtures-bootstrap
  (package
    (name "python-fixtures-bootstrap")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fixtures" version))
        (sha256
         (base32
          "1vxj29bzz3rd4pcy51d05wng9q9dh4jq6wx92yklsm7i6h1ddw7w"))
        (patches (search-patches "python-fixtures-remove-monkeypatch-test.patch"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
         (modify-phases %standard-phases
           ;; Package is not loadable on its own at this stage.
           (delete 'sanity-check))))
    (propagated-inputs
     (list python-pbr-minimal python-six python-extras))
    (home-page "https://launchpad.net/python-fixtures")
    (synopsis "Python test fixture library")
    (description
     "This package is only used for bootstrapping.  Use the regular
python-fixtures package instead.")
    (license (list license:bsd-3 license:asl2.0)))) ; at user's option

(define-public python-fixtures
  (package
    (inherit python-fixtures-bootstrap)
    (name "python-fixtures")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "testtools.run"
                       "fixtures.test_suite")))))))
    (propagated-inputs
     ;; Fixtures uses pbr at runtime to check versions, etc.
     (list python-pbr python-six python-extras))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-testtools" ,python-testtools-bootstrap)))
    (description
     "Fixtures provides a way to create reusable state, useful when writing
Python tests.")))

(define-public python-testrepository-bootstrap
  (package
    (name "python-testrepository-bootstrap")
     (version "0.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testrepository" version))
       (sha256
        (base32
         "1ssqb07c277010i6gzzkbdd46gd9mrj0bi0i8vn560n2k2y4j93m"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-fixtures" ,python-fixtures-bootstrap)
       ("python-subunit" ,python-subunit-bootstrap)
       ("python-testtools" ,python-testtools-bootstrap)))
    (native-inputs
     (list python-mimeparse))
    (home-page "https://launchpad.net/testrepository")
    (synopsis "Database for Python test results")
    (description
     "Bootstrap package for python-testrepository.  Don't use this.")
    (license (list license:bsd-3 license:asl2.0)))) ; at user's option

(define-public python-testrepository
  (package
    (inherit python-testrepository-bootstrap)
    (name "python-testrepository")
    (arguments
     ;; FIXME: Many tests are failing.
     '(#:tests? #f))
    (propagated-inputs
     (list python-fixtures python-subunit python-testtools))
    (native-inputs
     (list python-mimeparse))
    (description "Testrepository provides a database of test results which can
be used as part of a developer's workflow to check things such as what tests
have failed since the last commit or what tests are currently failing.")))

(define-public python-coverage
  (package
    (name "python-coverage")
    (version "6.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "coverage" version))
       (sha256
        (base32
         "157vndwrzyv9ypn2w3b6g8gv7vw07v994hq8nxasdb75k3ry2apc"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: 95 tests failed, 539 passed, 6 skipped, 2 errors.
     '(#:tests? #f))
    (propagated-inputs
     (list python-tomli))
    (home-page "https://coverage.readthedocs.io")
    (synopsis "Code coverage measurement for Python")
    (description
     "Coverage measures code coverage, typically during test execution.  It
uses the code analysis tools and tracing hooks provided in the Python standard
library to determine which lines are executable, and which have been
executed.")
    (license license:bsd-3)))

(define-public python-pytest-asyncio
  (package
    (name "python-pytest-asyncio")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)               ;for tests
       (uri (git-reference
             (url "https://github.com/pytest-dev/pytest-asyncio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03wljn0gdwyfr5s1795w3h2mfvvi23bn42nwjv5568rgphqyldqq"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f          ;XXX: to avoid a cycle with python-pytest-trio
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'pretend-version
                 (lambda _
                   (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                           #$(package-version this-package)))))))
    (native-inputs (list python-setuptools-scm))
    (propagated-inputs (list python-pytest))
    (home-page "https://github.com/pytest-dev/pytest-asyncio")
    (synopsis "Pytest support for asyncio")
    (description "Python asyncio code is usually written in the form of
coroutines, which makes it slightly more difficult to test using normal
testing tools.  @code{pytest-asyncio} provides useful fixtures and markers
to make testing async code easier.")
    (license license:asl2.0)))

(define-public python-cov-core
  (package
    (name "python-cov-core")
    (version "1.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cov-core" version))
        (sha256
         (base32
          "0k3np9ymh06yv1ib96sb6wfsxjkqhmik8qfsn119vnhga9ywc52a"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-coverage))
    (home-page "https://github.com/schlamar/cov-core")
    (synopsis "Coverage plugin core for pytest-cov, nose-cov and nose2-cov")
    (description
     "This is a library package for use by @code{pytest-cov}, @code{nose-cov}
and @code{nose2-cov}.  It is useful for developing coverage plugins for these
testing frameworks.")
    (license license:expat)))

(define-public python-codecov
  (package
    (name "python-codecov")
    (version "2.0.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "codecov" version))
        (sha256
         (base32
          "1217c0vqf7ii65635gvl27a5pfhv0r7zhrpdp9cx640hg73bgn4f"))))
    (build-system python-build-system)
    (native-inputs
     (list python-unittest2))
    (propagated-inputs
     (list python-coverage python-requests))
    (home-page "https://github.com/codecov/codecov-python")
    (synopsis "Upload code coverage reports to @code{codecov.io}")
    (description
     "Codecov collects code coverage reports from code written in Python, Java,
C/C++, R, and more, and uploads it to the @code{codecov.io} service.")
    (license license:asl2.0)))

(define-public python-testpath
  (package
    (name "python-testpath")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jupyter/testpath")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08r1c6bhvj8pcdvzkqv1950k36a6q3v81fd2p1yqdq3c07mcwgif"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                (("flit_core >=3.2.0,<3.3")
                 "flit_core >=3.2.0"))))
          ;; XXX: PEP 517 manual build copied from python-isort.
          (replace 'build
            (lambda _
              (invoke "python" "-m" "build" "--wheel" "--no-isolation" ".")))
          (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest"))))
          (replace 'install
            (lambda _
              (let ((whl (car (find-files "dist" "\\.whl$"))))
                (invoke "pip" "--no-cache-dir" "--no-input"
                        "install" "--no-deps" "--prefix" #$output whl)))))))
    (native-inputs
     (list python-pypa-build python-flit-core python-pytest))
    (home-page "https://github.com/jupyter/testpath")
    (synopsis "Test utilities for code working with files and commands")
    (description
     "Testpath is a collection of utilities for Python code working with files
and commands.  It contains functions to check things on the file system, and
tools for mocking system commands and recording calls to those.")
    (license license:expat)))

(define-public python-testlib
  (package
    (name "python-testlib")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testlib" version ".zip"))
       (sha256
        (base32 "1mz26cxn4x8bbgv0rn0mvj2z05y31rkc8009nvdlb3lam5b4mj3y"))))
    (build-system python-build-system)
    (native-inputs
     (list unzip))  ; for unpacking the source
    (synopsis "Python micro test suite harness")
    (description "A micro unittest suite harness for Python.")
    (home-page "https://github.com/trentm/testlib")
    (license license:expat)))

;;; The software provided by this package was integrated into pytest 2.8.
(define-public python-pytest-cache
  (package
    (name "python-pytest-cache")
    (version "1.0")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "pytest-cache" version))
             (sha256
              (base32
               "1a873fihw4rhshc722j4h6j7g3nj7xpgsna9hhg3zn6ksknnhx5y"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-apipkg python-execnet python-py python-pytest))
    (synopsis "Py.test plugin with mechanisms for caching across test runs")
    (description "The pytest-cache plugin provides tools to rerun failures from
the last py.test invocation.")
    (home-page "https://bitbucket.org/hpk42/pytest-cache/")
    (license license:expat)))

(define-public python-pytest-localserver
  (package
    (name "python-pytest-localserver")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-localserver" version))
              (sha256
               (base32
                "0fzysfzvlc2p5dh6lhs5sq3h8g4mypwvqm4w44fr6f5lbialcyz7"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "py.test" "-v"))))))
    (native-inputs
     (list python-pytest python-requests python-six))
    (propagated-inputs
     (list python-werkzeug))
    (synopsis "Py.test plugin to test server connections locally")
    (description "Pytest-localserver is a plugin for the pytest testing
framework which enables you to test server connections locally.")
    (home-page "https://pypi.org/project/pytest-localserver/")
    (license license:expat)))

(define-public python-pytest-xprocess
  (package
    (name "python-pytest-xprocess")
    (version "0.18.1")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "pytest-xprocess" version))
             (sha256
              (base32
               "0rm2rchrr63imn44xk5slwydxf8gvy579524qcxq7dc42pnk17zx"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools-scm))
    (propagated-inputs
     (list python-pytest python-psutil))
    (synopsis "Pytest plugin to manage external processes across test runs")
    (description "Pytest-xprocess is an experimental py.test plugin for managing
processes across test runs.")
    (home-page "https://github.com/pytest-dev/pytest-xprocess/")
    (license license:expat)))

(define-public python-pytest-subtesthack
  (package
    (name "python-pytest-subtesthack")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-subtesthack" version))
              (sha256
               (base32
                "15kzcr5pchf3id4ikdvlv752rc0j4d912n589l4rifp8qsj19l1x"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytest))
    (synopsis "Set-up and tear-down fixtures for unit tests")
    (description "This plugin allows you to set up and tear down fixtures within
unit test functions that use @code{py.test}. This is useful for using
@command{hypothesis} inside py.test, as @command{hypothesis} will call the test
function multiple times, without setting up or tearing down fixture state as is
normally the case.")
    (home-page "https://github.com/untitaker/pytest-subtesthack/")
    (license license:unlicense)))

(define-public python-pytest-sugar
  (package
    (name "python-pytest-sugar")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-sugar" version))
       (sha256
        (base32 "1i0hv3h49zvl62jbiyjag84carbrp3zprqzxffdr291nxavvac0n"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-packaging python-pytest python-termcolor))
    (home-page "https://pivotfinland.com/pytest-sugar/")
    (synopsis "Plugin for pytest that changes the default look and feel")
    (description
     "@code{pytest-sugar} is a plugin for py.test that changes the default
look and feel of py.test, using a progress bar and showing failures and errors
instantly.")
    (license license:bsd-3)))

(define-public python-hypothesis
  (package
    (name "python-hypothesis")
    (version "6.54.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hypothesis" version))
              (sha256
               (base32
                "1ivyrjpnahvj359pfndnk8x3h0gw37kqm02fmnzibx4mas15d44a"))))
    (build-system python-build-system)
    (arguments
     ;; XXX: Tests are not distributed with the PyPI archive.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               ;; XXX: hypothesis requires pytest at runtime, but we can
               ;; not propagate it due to a circular dependency.
               (delete 'sanity-check))))
    (propagated-inputs
     (list python-attrs-bootstrap python-exceptiongroup python-sortedcontainers))
    (synopsis "Library for property based testing")
    (description "Hypothesis is a library for testing your Python code against a
much larger range of examples than you would ever want to write by hand.  It’s
based on the Haskell library, Quickcheck, and is designed to integrate
seamlessly into your existing Python unit testing work flow.")
    (home-page "https://hypothesis.works/")
    (license license:mpl2.0)))

(define-deprecated python-hypothesis-next python-hypothesis)
(export python-hypothesis-next)

(define-public python-hypothesmith
  (package
    (name "python-hypothesmith")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hypothesmith" version))
       (sha256
        (base32
         "02j101m5grjrbvrgjap17jsxd1hgawkylmyswcn33vf42mxh9zzr"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-hypothesis python-lark-parser python-libcst-minimal))
    (home-page "https://github.com/Zac-HD/hypothesmith")
    (synopsis "Strategies for generating Python programs")
    (description
     "This package contains hypothesis strategies for generating Python
programs, something like CSmith, a random generator of C programs.")
    (license license:mpl2.0)))

(define-public python-lit
  (package
    (name "python-lit")
    (version "17.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lit" version))
        (sha256
         (base32
          "06z3p85gsy5hw3rbk0ym8aig9mvry1327gz7dfjhjigwandszafz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "lit.py" "tests")))))))
    ;; This can be built with any version of llvm.
    (native-inputs (list llvm))
    (home-page "https://llvm.org/")
    (synopsis "LLVM Software Testing Tool")
    (description "@code{lit} is a portable tool for executing LLVM and Clang
style test suites, summarizing their results, and providing indication of
failures.")
    (license license:ncsa)))

;;; This is marked as a bootstrap package because it propagates bootstrapped
;;; versions of jaraco-context and jaraco-functools.
(define-public python-pytest-enabler-bootstrap
  (hidden-package
   (package
     (name "python-pytest-enabler-bootstrap")
     (version "1.2.1")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-enabler" version))
        (sha256
         (base32 "023ymm0r2gpn5q7aikvx567s507j0zk46w41w6gxb69c688zgs73"))))
     (build-system python-build-system)
     (arguments (list #:tests? #f))
     (propagated-inputs
      (list python-jaraco-context-bootstrap
            python-jaraco-functools-bootstrap
            python-toml))
     (native-inputs (list python-setuptools-scm))
     (home-page "https://github.com/jaraco/pytest-enabler")
     (synopsis "Enable installed pytest plugins")
     (description "Enable installed pytest plugins")
     (license license:expat))))

(define-public python-pytest-enabler
  (package/inherit python-pytest-enabler-bootstrap
    (arguments
     (substitute-keyword-arguments
       (strip-keyword-arguments
         '(#:tests?)
         (package-arguments python-pytest-enabler-bootstrap))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "python" "-m" "pytest" "-vv" "tests"))))))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-pytest-enabler-bootstrap)
       (replace "python-jaraco-context-bootstrap" python-jaraco-context)
       (replace "python-jaraco-functools-bootstrap" python-jaraco-functools)))
    (native-inputs
     (modify-inputs (package-native-inputs python-pytest-enabler-bootstrap)
       (append python-pytest
               python-pytest-black
               python-pytest-checkdocs
               python-pytest-cov
               python-pytest-flake8
               python-pytest-mypy
               python-types-toml)))
    (properties (alist-delete 'hidden?
                              (package-properties
                               python-pytest-enabler-bootstrap)))))

(define-public python-pytest-freezegun
  (package
    (name "python-pytest-freezegun")
    (version "0.4.2")
    (source (origin
              ;; The test suite is not included in the PyPI archive.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ktosiek/pytest-freezegun")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10c4pbh03b4s1q8cjd75lr0fvyf9id0zmdk29566qqsmaz28npas"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv")))))))
    (propagated-inputs (list python-freezegun python-pytest))
    (native-inputs (list unzip))
    (home-page "https://github.com/ktosiek/pytest-freezegun")
    (synopsis "Pytest plugin to freeze time in test fixtures")
    (description "The @code{pytest-freezegun} plugin wraps tests and fixtures
with @code{freeze_time}, which controls (i.e., freeze) the time seen
by the test.")
    (license license:expat)))

(define-public python-pytest-mypy
  (package
    (name "python-pytest-mypy")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mypy" version))
       (sha256
        (base32 "0p5bd4r4gbwk1h7mpx1jkhdwkckapfz24bp9x5mmqb610ps3pylz"))))
    (build-system python-build-system)
    (native-inputs (list python-setuptools-scm))
    (propagated-inputs
     (list python-attrs python-filelock python-mypy python-pytest))
    (home-page "https://github.com/dbader/pytest-mypy")
    (synopsis "Mypy static type checker plugin for Pytest")
    (description "@code{pytest-mypi} is a static type checker plugin for
Pytest that runs the mypy static type checker on your source files as part of
a Pytest test execution.")
    (license license:expat)))

(define-public python-pytest-mypy-plugins
  (package
    (name "python-pytest-mypy-plugins")
    (version "1.10.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-mypy-plugins" version))
              (sha256
               (base32
                "05ng29b05gasqj195i9hyyhx5shmwypyvajb7plxwha3g36qq98z"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #false)) ;there are none
    (propagated-inputs (list python-chevron
                             python-decorator
                             python-mypy
                             python-pytest
                             python-pyyaml
                             python-regex))
    (home-page "https://github.com/TypedDjango/pytest-mypy-plugins")
    (synopsis "Pytest plugin for writing tests for mypy plugins")
    (description "This package provides a pytest plugin for writing tests for
mypy plugins.")
    (license license:expat)))

(define-public python-pytest-pep8
  (package
    (name "python-pytest-pep8")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-pep8" version))
              (sha256
               (base32
                "06032agzhw1i9d9qlhfblnl3dw5hcyxhagn7b120zhrszbjzfbh3"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Fails with recent pytest and pep8. See upstream issues #8 and #12.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dependencies
           (lambda _
             (substitute* "setup.py"
               (("'pytest-cache', ") ""))))  ; Included in recent pytest
         (replace 'check
            (lambda* (#:key tests? inputs outputs #:allow-other-keys)
              (when tests?
                (add-installed-pythonpath inputs outputs)
                (invoke "pytest" "-v")))))))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-pep8))
    (home-page "https://bitbucket.org/pytest-dev/pytest-pep8")
    (synopsis "Py.test plugin to check PEP8 requirements")
    (description "Pytest plugin for checking PEP8 compliance.")
    (license license:expat)))

(define-public python-pytest-perf
  (package
    (name "python-pytest-perf")
    (version "0.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jaraco/pytest-perf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hrccvrbccqwba04pqj749hdzn4sgldmbpg74nf3fzz7wyg6jxqk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "-k"
                          (string-append
                           ;; Do not test the myproject.toml build as it tries to pull
                           ;; dependencies from the internet.
                           "not project "
                           ;; The benchmark test attempts to install the
                           ;; package, failing to pull its dependencies from the
                           ;; network.
                           "and not BenchmarkRunner "
                           ;; The upstream_url test requires networking.
                           "and not upstream_url"))))
    (native-inputs
     (list python-pytest
           python-pytest-black
           python-pytest-checkdocs
           python-pytest-cov
           python-pytest-enabler
           python-pytest-flake8
           python-pytest-mypy))
    (propagated-inputs
     (list python-jaraco-context
           python-jaraco-functools
           python-more-itertools
           python-packaging
           python-pip-run
           python-tempora))
    (home-page "https://github.com/jaraco/pytest-perf")
    (synopsis "Pytest plugin for performance testing")
    (description "@code{pytest-perf} makes it easy to compare works by
creating two installs, the control and the experiment, and measuring the
performance of some Python code against each.  Under the hood, it uses the
@command{pip-run} command to install from the upstream main
branch (e.g. https://github.com/jaraco/pytest-perf) for the control and from
@file{.} for the experiment.  It then runs each of the experiments against
each of the environments.")
    (license license:expat)))

(define-public python-pytest-flakes
  (package
    (name "python-pytest-flakes")
    (version "4.0.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-flakes" version))
              (sha256
               (base32
                "0959qfxb4ayvfxvmpargvh4zfhwdq5l77gczhzv33bhmfblk8ccm"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; It's easier to run tests after install.
             ;; Make installed package available for running the tests
             (add-installed-pythonpath inputs outputs)
             (invoke "py.test" "-vv" "-k" "not test_syntax_error"))))))
    (native-inputs
     (list python-coverage python-pytest python-pytest-pep8))
    (propagated-inputs
     (list python-pyflakes))
    (home-page "https://github.com/fschulze/pytest-flakes")
    (synopsis "Py.test plugin to check source code with pyflakes")
    (description "Pytest plugin for checking Python source code with pyflakes.")
    (license license:expat)))

(define-public python-coverage-test-runner
  (package
    (name "python-coverage-test-runner")
    (version "1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://git.liw.fi/cgi-bin/cgit/cgit.cgi/"
             "coverage-test-runner/snapshot/coverage-test-runner-"
             version ".tar.gz"))
       (sha256
        (base32
         "1kjjb9llckycnfxag8zcvqsn4z1s3dwyw6b1n0avxydihgf30rny"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./testrun"))))))
    (propagated-inputs
     (list python-coverage))
    (home-page "https://liw.fi/coverage-test-runner/")
    (synopsis "Python module for running unit tests")
    (description "@code{CoverageTestRunner} is a python module for running
unit tests and failing them if the unit test module does not exercise all
statements in the module it tests.")
    (license license:gpl3+)))

(define-public python-pylint
  (package
    (name "python-pylint")
    (version "2.14.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PyCQA/pylint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ljfvyzr2i07pi7m19kbshlc3cfnwr53mjhcpydaa0w8bak4cc95"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; The unused but collected 'primer'-related test files require
               ;; the extraneous 'git' Python module; remove them.
               (delete-file "tests/primer/test_primer_external.py")
               (delete-file "tests/testutils/test_package_to_lint.py")
               (setenv "HOME" "/tmp")
               (invoke "pytest" "-k" "test_functional"
                       "-n" (number->string (parallel-job-count)))))))))
    (native-inputs
     (list python-pytest python-pytest-xdist))
    (propagated-inputs
     (list python-astroid
           python-dill
           python-isort
           python-mccabe
           python-platformdirs
           python-tomlkit
           python-typing-extensions))
    (home-page "https://github.com/PyCQA/pylint")
    (synopsis "Advanced Python code static checker")
    (description "Pylint is a Python source code analyzer which looks
for programming errors, helps enforcing a coding standard and sniffs
for some code smells (as defined in Martin Fowler's Refactoring book).

Pylint has many rules enabled by default, way too much to silence them
all on a minimally sized program.  It's highly configurable and handle
pragmas to control it from within your code.  Additionally, it is
possible to write plugins to add your own checks.")
    (license license:gpl2+)))

(define-public python-setuptools-lint
  (package
    (name "python-setuptools-lint")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools-lint" version))
              (sha256
               (base32
                "16a1ac5n7k7sx15cnk03gw3fmslab3a7m74dc45rgpldgiff3577"))))
    (build-system python-build-system)
    (propagated-inputs (list python-tomli python-pylint))
    (home-page "https://github.com/johnnoone/setuptools-pylint")
    (synopsis "Run pylint with @command{python setup.py lint}")
    (description "This package expose pylint as a lint command into
setup.py.")
    (license license:bsd-3)))

(define-public python-paramunittest
  (package
    (name "python-paramunittest")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ParamUnittest" version))
       (sha256
        (base32
         "0kp793hws5xv1wvycxq7jw2pwy36f35k39jg8hx5qikij5a0jid1"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/rik0/ParamUnittest")
    (synopsis
     "Simple extension to have parametrized unit tests")
    (description
     "This package creates parameterized unit-tests that work with the standard
unittest package.  A parameterized test case is automatically converted to multiple test
cases.  Since they are TestCase subclasses, they work with other test suites that
recognize TestCases.")
    (license license:bsd-2)))

(define-public python-pytest-warnings
  (package
    (name "python-pytest-warnings")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-warnings" version))
       (sha256
        (base32
         "0gf2dpahpl5igb7jh1sr9acj3z3gp7zahqdqb69nk6wx01c8kc1g"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/fschulze/pytest-warnings")
    (synopsis "Pytest plugin to list Python warnings in pytest report")
    (description
     "Python-pytest-warnings is a pytest plugin to list Python warnings in
pytest report.")
    (license license:expat)
    (properties `((superseded unquote python-pytest)))))

(define-public python-pytest-capturelog
  (package
    (name "python-pytest-capturelog")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-capturelog" version ".tar.gz"))
       (sha256
        (base32
         "038049nyjl7di59ycnxvc9nydivc5m8np3hqq84j2iirkccdbs5n"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytest))
    (home-page "https://bitbucket.org/memedough/pytest-capturelog/overview")
    (synopsis "Pytest plugin to catch log messages")
    (description
     "Python-pytest-catchlog is a pytest plugin to catch log messages.")
    (license license:expat)))

(define-public python-pytest-catchlog
  (package
    (name "python-pytest-catchlog")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-catchlog" version ".zip"))
       (sha256
        (base32
         "1w7wxh27sbqwm4jgwrjr9c2gy384aca5jzw9c0wzhl0pmk2mvqab"))))
    (build-system python-build-system)
    (native-inputs
     (list unzip))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/eisensheng/pytest-catchlog")
    (synopsis "Pytest plugin to catch log messages")
    (description
     "Python-pytest-catchlog is a pytest plugin to catch log messages.  This is
a fork of pytest-capturelog.")
    (license license:expat)))

(define-public python-nosexcover
  (package
    (name "python-nosexcover")
    (version "1.0.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nosexcover" version))
              (sha256
               (base32
                "10xqr12qv62k2flxwqhh8cr00cjhn7sfjrm6p35gd1x5bmjkr319"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-coverage python-nose))
    (home-page "https://github.com/cmheisel/nose-xcover")
    (synopsis "Extends nose.plugins.cover to add Cobertura-style XML reports")
    (description "Nose-xcover is a companion to the built-in
@code{nose.plugins.cover}.  This plugin will write out an XML coverage report
to a file named coverage.xml.

It will honor all the options you pass to the Nose coverage plugin,
especially -cover-package.")
    (license license:expat)))

(define-public python-discover
  (package
    (name "python-discover")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "discover" version))
       (sha256
        (base32
         "0y8d0zwiqar51kxj8lzmkvwc3b8kazb04gk5zcb4nzg5k68zmhq5"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/discover/")
    (synopsis
     "Python test discovery for unittest")
    (description
     "Discover provides test discovery for unittest, a feature that has been
backported from Python 2.7 for Python 2.4+.")
    (license license:bsd-3)))

(define-public behave
  (package
    (name "behave")
    ;; The 1.2.6 release from 2018 has several problems with newer Python
    ;; versions, so we package a recent snapshot.
    (version "1.2.7.dev2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/behave/behave")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sv94wagi214h0l91zn8m04f78x5wn83vqxib81hnl1qahvx9hq7"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "pytest" "-c" "/dev/null" "-vv")))))))
    (native-inputs
     (list python-mock python-nose python-pathpy python-pyhamcrest
           python-pytest))
    (propagated-inputs
     (list python-colorama
           python-cucumber-tag-expressions
           python-parse
           python-parse-type))
    (home-page "https://github.com/behave/behave")
    (synopsis "Python behavior-driven development")
    (description
     "Behave is a tool for behavior-driven development in python.
Behavior-driven development (or BDD) is an agile software development
technique that encourages collaboration between developers, QA and
non-technical or business participants in a software project.  Behave uses
tests written in a natural language style, backed up by Python code.")
    (license license:x11)))

(define-public python-behave-web-api
  (package
    (name "python-behave-web-api")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "behave-web-api" version))
       (sha256
        (base32
         "03kpq2xsy1gab3jy0dccbxlsg7vwfy4lagss0qldwmx3xz6b3i19"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dependencies
           (lambda _
             (substitute* "setup.py"
               (("'wheel'") "")                ; We don't use it.
               (("'ordereddict==1.1'") ""))    ; Python >= 2.7 has it built-in.
             #t)))))
    (propagated-inputs
     (list behave python-requests))
    (home-page "https://github.com/jefersondaniel/behave-web-api")
    (synopsis "Provides testing for JSON APIs with Behave for Python")
    (description "This package provides testing utility modules for testing
JSON APIs with Behave.")
    (license license:expat)))

(define-public python-rednose
  (package
    (name "python-rednose")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rednose" version))
        (sha256
          (base32
            "11x5nx5b4wdq04s7vj1gcdl07jvvkfb37p0r5lg773gr5rr8mj6h"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-setup.py
                    (lambda _
                      ;; Six is only required for tests and later versions
                      ;; work fine.
                      (substitute* "setup.py"
                        (("six==1.10.0") "six"))
                      #t)))))
    (propagated-inputs
     (list python-colorama python-termstyle))
    (native-inputs
     (list python-six python-nose))
    (home-page "https://github.com/JBKahn/rednose")
    (synopsis "Colored output for Python nosetests")
    (description "This package provides colored output for the
@command{nosetests} command of the Python Nose unit test framework.")
    (license license:bsd-3)))

(define-public python-nose-exclude
  (package
    (name "python-nose-exclude")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nose-exclude" version))
       (sha256
        (base32 "0123x1lyv5b2p9civcfg8vilj2ga3q7p2ks1hq25z0gb3ssai3zp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'disable-test
            (lambda _
              ;; Disable failing test: AssertionError.
              (substitute* '("test_dirs/build/test.py"
                            "test_dirs/test_not_me/test.py")
                (("def test_i_should_never_run")
                 "def off_i_should_never_run")))))))
    (propagated-inputs
     (list python-nose))
    (home-page "https://github.com/kgrandis/nose-exclude")
    (synopsis "Exclude specific directories from nosetests runs")
    (description
     "@code{nose-exclude} is a Nose plugin that allows you to easily specify
directories to be excluded from testing.")
    (license license:lgpl2.1+)))

(define-public python-nose-random
  (package
    (name "python-nose-random")
    (version "1.0.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/fzumstein/nose-random")
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "1dvip61r2frjv35mv6mmfjc07402z73pjbndfp3mhxyjn2zhksw2"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose))
    (home-page "https://github.com/fzumstein/nose-random")
    (synopsis "Nose plugin to facilitate randomized unit testing with
Python")
    (description "Python nose-random is designed to facilitate
Monte-Carlo style unit testing.  The idea is to improve testing by
running your code against a large number of randomly generated input
scenarios.")
    (license license:expat)))

(define-public python-nose-randomly
  (package
    (name "python-nose-randomly")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nose-randomly" version))
       (sha256
        (base32 "0z662rqhfk4bjmg806mn4frb8nz4gbh7mrddsrhfffp1g4yklj3y"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose python-numpy))
    (home-page "https://github.com/adamchainz/nose-randomly")
    (synopsis
     "Nose plugin to randomly order tests and control random.seed")
    (description
     "This is a @code{Nose} plugin to randomly order tests which can be quite
powerful in discovering hidden flaws in the tests themselves, while helping to
reduce inter-test dependencies.  It also helps in controlling @code{random.seed},
by resetting it to a repeatable number for each test, enabling the tests to
create data based on random numbers and yet remain repeatable.")
    (license license:bsd-3)))

(define-public python-nose-timer
  (package
    (name "python-nose-timer")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nose-timer" version))
        (sha256
          (base32 "05wzkc88vbzw62pqkvhl33211b90kns0lny70b7qw62rcg4flzk4"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-nose python-termcolor))
    (home-page "https://github.com/mahmoudimus/nose-timer")
    (synopsis "Timer plugin for nosetests")
    (description "Shows how much time was needed to run individual tests.")
    (license license:expat)))

(define-public python-freezegun
  (package
    (name "python-freezegun")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "freezegun" version))
       (sha256
        (base32 "0ijlq32qvpm5zprfzbyzawpl9qjsknlxhryr1i0q84wl0sxd28nd"))
       (modules '((guix build utils)))
       (snippet
        ;; Add an explicit case for static methods as they are callable
        ;; in Python 3.10, breaking this conditional.
        ;; XXX Taken from upstream pull request:
        ;; https://github.com/spulec/freezegun/pull/397
        '(substitute* "freezegun/api.py"
           (("if not callable\\(attr_value\\) or \
inspect\\.isclass\\(attr_value\\):")
            "if (not callable(attr_value) or inspect.isclass(attr_value)\
or isinstance(attr_value, staticmethod)):")))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-dateutil))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The tests are normally executed via `make test`, but the PyPi
         ;; package does not include the Makefile.
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (home-page "https://github.com/spulec/freezegun")
    (synopsis "Test utility for mocking the datetime module")
    (description
     "FreezeGun is a library that allows your python tests to travel through
time by mocking the datetime module.")
    (license license:asl2.0)))

(define-public python-flexmock
  (package
    (name "python-flexmock")
    (version "0.10.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "flexmock" version))
              (sha256
               (base32
                "0b6qw3grhgx58kxlkj7mdma7xdvlj02zabvcf7w2qifnfjwwwcsh"))))
    (build-system python-build-system)
    (home-page "https://flexmock.readthedocs.org")
    (synopsis "Testing library for Python")
    (description
     "flexmock is a testing library for Python that makes it easy to create
mocks, stubs and fakes.")
    (license license:bsd-3)))

(define-public python-flaky
  (package
    (name "python-flaky")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "flaky" version))
              (sha256
               (base32
                "03daz352021211kvdb056f3afrd2gsdq0rd1awgr38910xw01l9s"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: Tests require 'coveralls' and 'genty' which are not in Guix yet.
     '(#:tests? #f))
    (home-page "https://github.com/box/flaky")
    (synopsis "Automatically rerun flaky tests")
    (description
     "Flaky is a plugin for @code{nose} or @code{py.test} that automatically
reruns flaky tests.

Ideally, tests reliably pass or fail, but sometimes test fixtures must rely
on components that aren't 100% reliable.  With flaky, instead of removing
those tests or marking them to @code{@@skip}, they can be automatically
retried.")
    (license license:asl2.0)))

(define-public python-pyhamcrest
  (package
    (name "python-pyhamcrest")
    (version "2.0.2")
    (source (origin
              (method git-fetch)        ;no tests in PyPI archive
              (uri (git-reference
                    (url "https://github.com/hamcrest/PyHamcrest")
                    (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05kdzlhs2kvj82pfca13qszszcj6dyrk4b9pbr46x06sq2s4qyls"))))
    (native-inputs                      ;all native inputs are for tests
     (list python-pytest-cov python-mock python-pytest python-hypothesis))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (add-installed-pythonpath inputs outputs)
                      (invoke "pytest" "-vv"))))))
    (home-page "https://hamcrest.org/")
    (synopsis "Hamcrest matchers for Python")
    (description "PyHamcrest is a framework for writing matcher objects,
allowing you to declaratively define \"match\" rules.")
    (license license:bsd-3)))

(define-public theft
  (package
   (name "theft")
   (version "0.4.5")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/silentbicycle/theft")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1n2mkawfl2bpd4pwy3mdzxwlqjjvb5bdrr2x2gldlyqdwbk7qjhd"))
            (snippet #~(begin
                         (delete-file "vendor/greatest.h")))))
   (build-system gnu-build-system)
   (arguments (list #:make-flags #~(list "VENDOR="
                                         (string-append "CC=" #$(cc-for-target))
                                         (string-append "PREFIX=" #$output))
                    #:test-target "test"
                    #:phases
                    #~(modify-phases %standard-phases
                        (delete 'bootstrap)
                        (delete 'configure))))
   (native-inputs (list greatest))
   (home-page "https://github.com/silentbicycle/theft")
   (synopsis "Property-based testing for C")
   (description "Theft is a library for property-based testing.")
   (license license:isc)))

(define-public unittest-cpp
  (package
    (name "unittest-cpp")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/unittest-cpp/unittest-cpp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0sxb3835nly1jxn071f59fwbdzmqi74j040r81fanxyw3s1azw0i"))))
    (arguments
     `(#:tests? #f))                     ; It's run after build automatically.
    (build-system cmake-build-system)
    (home-page "https://github.com/unittest-cpp/unittest-cpp")
    (synopsis "Lightweight unit testing framework for C++")
    (description "UnitTest++ is a lightweight unit testing framework for C++.
It was designed to do test-driven development on a wide variety of platforms.
Simplicity, portability, speed, and small footprint are all very important
aspects of UnitTest++.  UnitTest++ is mostly standard C++ and makes minimal use
of advanced library and language features, which means it should be easily
portable to just about any platform.")
    (license license:expat)))

(define-public libfaketime
  (package
    (name "libfaketime")
    (version "0.9.10")
    (home-page "https://github.com/wolfcw/libfaketime")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "112l7x2gv4f47hpffpb8djfwvgrs8w5h9s266h1fshi1c916x10d"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'embed-date-reference
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/faketime.c"
                (("\"date\"")
                 (format #f "~s" (search-input-file inputs "bin/date"))))))
          (replace 'configure
            (lambda* (#:key outputs #:allow-other-keys)
              (setenv "CC" #$(cc-for-target))
              (setenv "PREFIX" #$output)

              ;; XXX: Without this flag, the CLOCK_REALTIME test hangs
              ;; indefinitely.  See README.packagers for more information.
              ;; There are specific instructions to not enable more flags
              ;; than absolutely needed.
              #$@(if (or (target-ppc64le?)
                         (target-riscv64?))
                     #~((setenv "FAKETIME_COMPILE_CFLAGS"
                                "-DFORCE_MONOTONIC_FIX -DFORCE_PTHREAD_NONVER"))
                     #~((setenv "FAKETIME_COMPILE_CFLAGS"
                                "-DFORCE_MONOTONIC_FIX")))))
          (add-before 'check 'pre-check
            (lambda _
              (substitute* "test/functests/test_exclude_mono.sh"
                (("/bin/bash") (which "bash"))))))))
    (native-inputs (list perl))         ;for tests
    (inputs (list coreutils-minimal))
    (synopsis "Fake the system time for single applications")
    (description
     "The libfaketime library allows users to modify the system time that an
application \"sees\".  It is meant to be loaded using the dynamic linker's
@code{LD_PRELOAD} environment variable.  The @command{faketime} command
provides a simple way to achieve this.")
    (license license:gpl2)))

(define-public rapidcheck
  (let ((commit "ff6af6fc683159deb51c543b065eba14dfcf329b")
        (revision "1"))
    (package
      (name "rapidcheck")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/emil-e/rapidcheck")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1s2qva1amhs887jcdj12ppxk9kkfvy25xy7vzhkwb7rljr3gj713"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
              (make-file-writable "ext/CMakeLists.txt")
              (call-with-output-file "ext/CMakeLists.txt"
                (lambda (out)
                  (display "find_package(Catch2 REQUIRED GLOBAL)\n" out)
                  (display "find_package(GTest GLOBAL)\n" out)
                  (display "find_package(Boost GLOBAL)\n" out)))
              (substitute* "extras/boost/test/CMakeLists.txt"
                (("^([ ]*)boost" all spaces)
                 (string-append spaces "Boost::boost")))
              ;; Disable tests failing on Apple M1 and Hetzner CAX41 (aarch64).
              ;; Upstream issue: https://github.com/emil-e/rapidcheck/issues/328
              (substitute* "test/gen/NumericTests.cpp"
                (("forEachType<SignedProperties.*") ""))
              (substitute* "test/shrink/ShrinkTests.cpp"
                (("forEachType<SignedIntegralProperties.*") ""))))))
      (arguments
       (list
        #:configure-flags #~(list "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
                                  "-DRC_ENABLE_BOOST=on"
                                  "-DRC_ENABLE_CATCH=on"
                                  "-DRC_ENABLE_DOCTEST=on"
                                  "-DRC_ENABLE_GTEST=on"
                                  "-DRC_ENABLE_TESTS=on")))
      (build-system cmake-build-system)
      (inputs (list boost
                    catch2
                    doctest
                    googletest))
      (native-inputs (list catch2 googletest))
      (home-page "https://github.com/emil-e/rapidcheck")
      (synopsis "Property based testing framework for C++")
      (description "Rapidcheck is a property based testing framework for C++.
It works by generating random data to try and find a case breaks your given
pre-condition.")
      (license license:bsd-2))))

(define-public umockdev
  (package
    (name "umockdev")
    (version "0.17.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/martinpitt/umockdev/"
                                  "releases/download/" version  "/"
                                  "umockdev-" version ".tar.xz"))
              (sha256
               (base32
                "1kqkraag5v1jl5qfv0mb3ckm8yq2im21mng08sbs9dh9c9pbyvkc"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'skip-test-umockdev.c
                 ;; This test depends on /sys being available, among other
                 ;; things.
                 (lambda _
                   (call-with-output-file "tests/test-umockdev.c"
                     (lambda (port)
                       (format port "int main(void) { return 0; }")))))
               ;; Avoid having to set 'LD_LIBRARY_PATH' to use umockdev
               ;; via introspection.
               (add-after 'unpack 'absolute-introspection-library
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute* "meson.build"
                     (("libumockdev.so.0" all)
                      (string-append #$output "/lib/" all)))))
               (add-after 'install 'absolute-filenames
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; 'patch-shebangs' will take care of the shebang.
                   (substitute* (string-append #$output "/bin/umockdev-wrapper")
                     (("env") (search-input-file inputs "bin/env"))
                     (("libumockdev")
                      (string-append #$output "/lib/libumockdev"))))))))
    (native-inputs
     (list gobject-introspection
           gtk-doc/stable
           pkg-config
           python
           vala
           which))
    (inputs
     (list bash-minimal                 ;for umockdev-wrapper
           coreutils-minimal            ;for bin/env
           eudev
           glib
           libgudev
           libpcap))
    (home-page "https://github.com/martinpitt/umockdev/")
    (synopsis "Mock hardware devices for creating unit tests")
    (description "umockdev mocks hardware devices for creating integration
tests for hardware related libraries and programs.  It also provides tools to
record the properties and behaviour of particular devices, and to run a
program or test suite under a test bed with the previously recorded devices
loaded.")
    (license license:lgpl2.1+)))

(define-public virtest
  ;; No releases yet, so we take the commit that "vc" expects.
  (let ((commit "f7d03ef39fceba168745bd29e1b20af6e7971e04")
        (revision "0"))
    (package
      (name "virtest")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/mattkretz/virtest")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07pjyb0mk7y2w1dg1bhl26nb7416xa1mw16ifj6mmps5y6aq054l"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'adjust-install-directory
                      (lambda _
                        ;; Vc is the only consumer of this library, and expects
                        ;; to find it in "virtest/vir/" instead of "vir/vir/".
                        (substitute* "CMakeLists.txt"
                          (("DESTINATION include/vir")
                           "DESTINATION include/virtest"))
                        #t)))))
      (synopsis "Header-only test framework")
      (description
       "@code{virtest} is a small header-only test framework for C++.  It
grew out of the @dfn{Vc} project.")
      (license license:bsd-3))))

(define-public python-pyfakefs
  (package
    (name "python-pyfakefs")
    (version "4.6.3")
    (source (origin
              (method url-fetch)
              ;; We use the PyPI URL because there is no proper release
              ;; available from GitHub.  The GitHub project only provides
              ;; autogenerated tarballs, which are known to change in place.
              (uri (pypi-uri "pyfakefs" version))
              (sha256
               (base32
                "18bcv8yalg80zgigx40fk692yr3wf9ch1hkb0cdplqspyry2mwbd"))
              (patches (search-patches
                        "python-pyfakefs-remove-bad-test.patch"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The default test suite does not run these extra tests.
         (add-after 'check 'check-pytest-plugin
           (lambda _
             (invoke
              "python" "-m" "pytest"
              "pyfakefs/pytest_tests/pytest_plugin_test.py"))))))
    (native-inputs
     (list python-pytest))
    (build-system python-build-system)
    ;; Guix lint doesn't like that this is a permanent redirect to the GitHub
    ;; page, but the pyfakefs documentation asks us to use this specific URL
    ;; when linking to the project.  Honor their request.
    (home-page "http://pyfakefs.org/")
    ;; TRANSLATORS: In the synopsis, "Mock" is a verb.
    (synopsis "Mock file system interactions in tests")
    (description
     "This package provides a Python library intended for use in automated
tests.  One difficulty when testing software is that the code under test might
need to read or write to files in the local file system.  If the file system
is not set up in just the right way, it might cause a spurious error during
the test.  The pyfakefs library provides a solution to problems like this by
mocking file system interactions.  In other words, it arranges for the code
under test to interact with a fake file system instead of the real file
system.  The code under test requires no modification to work with pyfakefs.")
    (license license:asl2.0)))

(define-public python-aiounittest
  (package
    (name "python-aiounittest")
    (version "1.4.2")
    ;; Pypi package lacks tests.
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/kwarunek/aiounittest.git")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0srahyzrk5awfh4rmppvqkblfmiavdklxl9i5mcr8gl7ahiwwl7f"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "nosetests" "-v")
                          (format #t "test suite not run~%"))
                      #t)))))
    (propagated-inputs (list python-wrapt))
    (native-inputs
     (list python-coverage python-nose))
    (home-page
     "https://github.com/kwarunek/aiounittest")
    (synopsis "Test asyncio code more easily")
    (description "Aiounittest is a library that helps write tests using
asynchronous code in Python (asyncio).")
    (license license:expat)))

(define-public python-pytest-dependency
  (package
    (name "python-pytest-dependency")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-dependency" version))
        (sha256
          (base32
            "0swl3mxca7nnjbb5grfzrm3fa2750h9vjsha0f2kyrljc6895a62"))))
    (build-system python-build-system)
    (propagated-inputs
      (list python-pytest))
    (home-page
      "https://github.com/RKrahl/pytest-dependency")
    (synopsis "Manage dependencies of tests")
    (description "This pytest plugin manages dependencies of tests.  It allows
to mark some tests as dependent from other tests.  These tests will then be
skipped if any of the dependencies did fail or has been skipped.")
    (license license:asl2.0)))

(define-public python-pytest-pudb
  ;; PyPi does not include tests
  (let ((commit "a6b3d2f4d35e558d72bccff472ecde9c9d9c69e5"))
    (package
      (name "python-pytest-pudb")
      ;; Version mentioned in setup.py version field.
      (version "0.7.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wronglink/pytest-pudb")
                      (commit commit)))
                (file-name (git-file-name name commit))
                (sha256
                 (base32
                  "1c0pypxx3y8w7s5bz9iy3w3aablnhn81rnhmb0is8hf2qpm6k3w0"))))
      (build-system python-build-system)
      (propagated-inputs (list pudb))
      (native-inputs (list python-pytest))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'check
                      (lambda* (#:key inputs outputs tests? #:allow-other-keys)
                        (when tests?
                          (add-installed-pythonpath inputs outputs)
                          (invoke "pytest" "-v")))))))
      (home-page "https://github.com/wronglink/pytest-pudb")
      (synopsis "Pytest PuDB debugger integration")
      (description
       "@code{python-pytest-pudb} provides PuDB debugger integration based
on pytest PDB integration.  For example, the software developer can
call pudb by running @code{py.test --pudb} from the command line or by
including @code{pudb.set_trace} in their test file(s).")
      (license license:expat))))

(define-public python-pytest-datadir
  (package
    (name "python-pytest-datadir")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-datadir" version))
       (sha256
        (base32
         "066bg6wlzgq2pqnjp73dfrcmk8951xw3aqcxa3p1axgqimrixbyk"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools-scm))
    (propagated-inputs
     (list python-pytest python-wheel))
    (home-page "https://github.com/gabrielcnr/pytest-datadir")
    (synopsis "Pytest plugin for manipulating test data directories and files")
    (description
     "This package provides a Pytest plugin for manipulating test data
directories and files.")
    (license license:expat)))

(define-public python-pytest-regressions
  (package
    (name "python-pytest-regressions")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-regressions" version))
       (sha256
        (base32
         "0792s1rp4hksfarnnciy0yiy2q2yqqsbin3mc9h2gxp86kdlrv5k"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytest-datadir python-pyyaml))
    (native-inputs
     (list python-matplotlib
           python-numpy
           python-pandas
           python-pillow
           python-restructuredtext-lint
           python-tox
           python-setuptools-scm
           python-pytest))
    (home-page "https://github.com/ESSS/pytest-regressions")
    (synopsis "Easy to use fixtures to write regression tests")
    (description
     "This plugin makes it simple to test general data, images, files, and numeric
tables by saving expected data in a data directory (courtesy of pytest-datadir)
that can be used to verify that future runs produce the same data.")
    (license license:expat)))

(define-public python-pytest-tornado5
  (package
    (name "python-pytest-tornado5")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-tornado5" version))
              (sha256
               (base32
                "0qb62jw2w0xr6y942yp0qxiy755bismjfpnxaxjjm05gy2pymr8d"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Tests require pytest < 6
     (list #:tests? #f))
    (propagated-inputs (list python-pytest python-tornado))
    (home-page "https://github.com/vidartf/pytest-tornado")
    (synopsis
     "Fixtures and markers to simplify testing of Tornado applications")
    (description
     "This package provides a @code{py.test} plugin supplying fixtures and
markers to simplify testing of asynchronous tornado applications.")
    (license license:asl2.0)))

(define-public guile-proba
  (package
    (name "guile-proba")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/luis-felipe/guile-proba")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17ab304ylylm9z980ij5lv188inx6331r1mn1s7qrlxly9fzx888"))))
    (build-system guile-build-system)
    (inputs (list bash-minimal guile-3.0))
    (native-inputs (list texinfo))
    (propagated-inputs (list guile-config guile-lib))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'set-paths 'add-output-to-guile-load-paths
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (guile-version (target-guile-effective-version))
                     (scm-path (string-append out
                                              "/share/guile/site/"
                                              guile-version))
                     (go-path (string-append out
                                             "/lib/guile/"
                                             guile-version
                                             "/site-ccache")))
                (setenv "GUILE_LOAD_PATH"
                        (string-append scm-path ":"
                                       (getenv "GUILE_LOAD_PATH")))
                (setenv "GUILE_LOAD_COMPILED_PATH"
                        (string-append
                         go-path ":"
                         (getenv "GUILE_LOAD_COMPILED_PATH"))))))
          (add-after 'build 'build-manual
            (lambda _
              (invoke "makeinfo" "manual/main.texi")))
          (add-after 'build 'check
            (lambda _
              (invoke "guile" "proba.scm" "run" "tests")))
          (add-after 'install 'install-wrapped-script
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin-dir (string-append out "/bin"))
                     (script (string-append bin-dir "/proba")))
                (mkdir-p bin-dir)
                (copy-file "proba.scm" script)
                (chmod script #o555)
                (wrap-program script
                  `("GUILE_LOAD_PATH" prefix (,(getenv "GUILE_LOAD_PATH")))
                  `("GUILE_LOAD_COMPILED_PATH" prefix
                    (,(getenv "GUILE_LOAD_COMPILED_PATH")))))))
          (add-after 'install 'install-manual
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (info-dir (string-append out "/share/info")))
                (mkdir-p info-dir)
                (install-file "guile-proba" info-dir)))))
      #:scheme-file-regexp
      #~(begin
          (use-modules (ice-9 regex))
          (lambda (file stat) (string-match "/proba/.*\\.scm$" file)))))
    (home-page "https://luis-felipe.gitlab.io/guile-proba/")
    (synopsis "Testing tools for GNU Guile projects with SRFI 64 test suites")
    (description
     "This software is a set of testing tools for GNU Guile projects
with SRFI 64-based test suites.  It comes with a command-line interface
to run test collections, and a library that includes a test runner and
helpers for writing tests.")
    (license license:public-domain)))

(define-public subunit
  (package
    (name "subunit")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/testing-cabal/subunit")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16n1zxwnmhb7vzixngvmm5zzk4q5jaqqjwyr6pr6w0ys60b7xja3"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf
                         automake
                         check
                         cppunit
                         libtool
                         pkg-config
                         python-fixtures
                         python-hypothesis
                         python-testscenarios))
    (inputs (list perl python))
    (propagated-inputs (list python-testtools))
    (home-page "https://github.com/testing-cabal/subunit")
    (synopsis "Test reporting and control protocol")
    (description
     "Subunit is a streaming protocol for test results.  Subunit comes with
command line filters to process a subunit stream and language bindings for
Python, C, C++ and shell.  Bindings are easy to write for other languages.")
    (license (list license:asl2.0 license:bsd-3)))) ;user can pick
