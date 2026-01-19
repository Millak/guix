;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2018-2025 Efraim Flashner <efraim@flashner.co.il>
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
;;; Copyright © 2015, 2017, 2018, 2020, 2021, 2023, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017-2018, 2020-2021, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2019, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Josh Marshall <joshua.r.marshall.1991@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020-2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022, 2023 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2022-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Luis Felipe López Acevedo <luis.felipe.la@protonmail.com>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2023 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 Reza Housseini <reza@housseini.me>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023, 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Navid Afkhami <navid.afkhami@mdc-berlin.de>
;;; Copyright © 2024, 2025 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2024 Ashvith Shetty <ashvithshetty10@gmail.com>
;;; Copyright © 2025 Jordan Moore <lockbox@struct.foo>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 nomike Postmann <nomike@nomike.com>
;;; Copyright © 2025 Josep Bigorra <jjbigorra@gmail.com>
;;; Copyright © 2025 Murilo <murilo@disroot.org>
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
  #:use-module (gnu packages nss)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages php)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages ruby)
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
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix deprecation)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public atf
  (package
    (name "atf")
    (version "0.21")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/freebsd/atf")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jwzz6g9jdi5f8v10y0wf3hq73vxyv5qqhkh832ddsj36gn8rlcz"))
              (patches (search-patches "atf-execute-with-shell.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "ATF_SHELL="
                                  #$(this-package-input "bash-minimal")
                                  "/bin/sh"))))
    (native-inputs (list autoconf automake libtool))
    (inputs (list bash-minimal))
    (home-page "https://github.com/freebsd/atf")
    (synopsis "C/C++ Automated Testing Framework libraries")
    (description "ATF, or Automated Testing Framework, is a collection of
libraries to write test programs in C, C++ and POSIX shell.

The ATF libraries offer a simple API.  The API is orthogonal through the
various bindings, allowing developers to quickly learn how to write test
programs in different languages.

ATF-based test programs offer a consistent end-user command-line interface to
allow both humans and automation to run the tests.

ATF-based test programs rely on an execution engine to be run and this
execution engine is not shipped with ATF.  Kyua is the engine of choice.")
    (license (list license:bsd-2 license:bsd-3))))

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

(define-public cargo-nextest
  (package
    (name "cargo-nextest")
    (version "0.9.97")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-nextest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j55sqr3fnhsk5b9n2jwy6g1h605qgrhwpxlsx789k8b3yhhnfyz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs
     (cons* pkg-config zlib `(,zstd "lib") (cargo-inputs 'cargo-nextest)))
    (home-page "https://github.com/nextest-rs/nextest")
    (synopsis "next-generation test runner for Rust")
    (description
     "This package provides a next-generation test runner for Rust.")
    (license (list license:asl2.0 license:expat))))

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

(define-public checkmake
  (package
    (name "checkmake")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/checkmake/checkmake")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ajrgnm5mg4b317brx53b8cpjvdw6vin1rk6yh9vrhrz014ifps2"))
       (modules '((guix build utils)))
       (snippet `(begin
                   (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/mrtazz/checkmake"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-man
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (let ((man-dir (string-append #$output "/share/man/man1")))
                  (mkdir-p man-dir)
                  (invoke "go-md2man"
                          "-in" "man/man1/checkmake.1.md"
                          "-out" (string-append man-dir "/checkmake.1")))))))))
    (native-inputs
     (list go-github-com-docopt-docopt-go
           go-github-com-go-ini-ini
           go-github-com-olekukonko-tablewriter-0.0.5
           go-github-com-stretchr-testify
           go-md2man))
    (home-page "https://github.com/checkmake/checkmake")
    (synopsis "Linter and analyzer for @file{Makefile}")
    (description
     "@samp{checkmake} is an experimental tool for linting and checking
Makefiles.  It allows for a set of configurable rules being run
against a @file{Makefile} or a set of @file{*.mk} files.")
    (license license:expat)))

(define-public mbake
  (package
    (name "mbake")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mbake" version))
       (sha256
        (base32 "1badaaw5cxbca7fqahjm6j5yk6mmcakc4772q9gdrr84jx9wjd67"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-rich
           python-typer))
    (home-page "https://github.com/EbodShojaei/bake")
    (synopsis "Makefile formatter and linter")
    (description
     "@command{mbake} is a Makefile formatter and linter,which is configurable
via a TOML file.  It intelligently deals with @code{.PHONY} declarations and
line continuations, and ensures consistent formatting and style for your
Makefile.")
    (license license:expat)))

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

(define-public snitch
  (package
    (name "snitch")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/snitch-org/snitch")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mf55yi8ahpczl9jz2is3dnghfi3g9qz5vch7mx7akqx4gfhhviz"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f
                     #:configure-flags
                     #~(list "-DBUILD_SHARED_LIBS=ON"
                             "-DSNITCH_DO_TEST=ON"
                             "-DSNITCH_USE_SYSTEM_DOCTEST=ON")))
    (native-inputs (list doctest))
    (home-page "https://github.com/snitch-org/snitch")
    (synopsis "Lightweight C++20 testing framework")
    (description "@code{snitch} aims to be a simple, cheap, non-invasive, and
user-friendly testing framework.  The design philosophy is to keep the testing
API lean, including only what is strictly necessary to present clear messages
when a test fails.")
    (license license:boost1.0)))

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
    (version "2.13.10")
    (home-page "https://github.com/catchorg/Catch2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/catchorg/Catch2")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dcwp41963vga22hm32hj609wxgfn0a6p9impcw8dxqj477gcx2y"))))
    (build-system cmake-build-system)
    (inputs
     (list python-wrapper))
    (synopsis "Automated test framework for C++ and Objective-C")
    (description "Catch2 stands for C++ Automated Test Cases in Headers and is
a multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))

(define-public cbehave
  ;; XXX: The last time updated on <2013-07-12>, the only user is tinydir
  ;; package.
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
        #:configure-flags
        #~(list "CFLAGS=-g -O2 -Wno-error=implicit-function-declaration"
                "--enable-shared"
                "--disable-static")
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

(define-public catch2-3.8
  (package
    (name "catch2")
    (version "3.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
         (url "https://github.com/catchorg/Catch2")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v1k14n02aiw4rv5sxhc5612cjhkdj59cjpm50qfxhapsdv54n3f"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DCATCH_DEVELOPMENT_BUILD=ON"
              "-DCATCH_ENABLE_WERROR=OFF"
              "-DBUILD_SHARED_LIBS=ON")))
    (inputs (list python-wrapper))
    (home-page "https://github.com/catchorg/Catch2")
    (synopsis "Automated test framework for C++ and Objective-C")
    (description "Catch2 stands for C++ Automated Test Cases in Headers and is
a multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))

(define-public catch2-3
  (package
    (inherit catch2-3.8)
    (name "catch2")
    (version "3.5.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/catchorg/Catch2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11yla93vm2896fzhm3fz8lk3y3iz5iy7vd6wa7wnwvhsfd2dbfq3"))))))

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
    (version "2.18.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/danmar/cppcheck")
             (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1551pbbvx43j5jl5jz725km7gd65fizf9y7j66anjmdmqzccpdmn"))
       (patches (search-patches "cppcheck-fix-basedir-test.patch"))))
    (build-system cmake-build-system)
    (arguments
     '(#:parallel-tests? #f
       #:configure-flags '("-DBUILD_TESTS=ON"
                           "-DUSE_BUNDLED_TINYXML2=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-testcondition-test
           (lambda _
             (substitute* "test/testcondition.cpp"
               (("test.cpp:4\\] -> \\[test.cpp:6")
                "test.cpp:4:25] -> [test.cpp:6:18")
               (("test.cpp:3\\] -> \\[test.cpp:5(.*false)"_ rest)
                (string-append "test.cpp:3:22] -> [test.cpp:5:22" rest
                               " [knownConditionTrueFalse]"))))))))
    (inputs
     (list tinyxml2))
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
    ;; 4.4 is from 2014, there is some move on master branch but still no
    ;; fresh tag, see: <https://github.com/CxxTest/cxxtest/issues/156>.
    (version "4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/cxxtest/cxxtest/" version
                           "/cxxtest-" version ".tar.gz"))
       (sha256
        (base32 "1n7pbj4z9ivx005hqvivj9ddhq8awynzg6jishfbypf6j7ply58w"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: There are SCons and CMake tests, but not pytest ones.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-source
            (lambda _
              (chdir "python")))
          (add-after 'install 'install-headers
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((include-dir (string-append #$output "/include/cxxtest")))
                (for-each (lambda (header-file)
                            (install-file header-file include-dir))
                          (find-files "../cxxtest")))))
          (add-after 'install 'install-doc
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((doc-dir (string-append #$output "/share/doc/cxxtest")))
                (install-file "../README" doc-dir)
                (install-file "../doc/guide.txt" doc-dir)
                (copy-recursively "../sample"
                                  (string-append doc-dir "/sample"))))))))
    (native-inputs (list python-setuptools))
    (propagated-inputs (list python-ply))
    (home-page
     "https://web.archive.org/web/20230604070022/http://cxxtest.com/")
    (synopsis "Unit testing framework for C++")
    (description
     "CxxTest is a unit testing framework for C++ that is similar in spirit to
JUnit, CppUnit, and xUnit.  CxxTest does not require precompiling a CxxTest
testing library, it employs no advanced features of C++ (e.g. RTTI) and it
supports a very flexible form of test discovery.")
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
  ;; It is included for completeness to run tests for package utfcpp.
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

(define-public kyua
  (package
    (name "kyua")
    (version "0.14.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/freebsd/kyua")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w238ynhnjz7p2v8fbgxv35kl1x7a4vs86227qhb4gxncr75nsbl"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* '("Makefile.am"
                             "utils/process/isolation_test.cpp"
                             "utils/stacktrace_test.cpp"
                             "integration/utils.sh"
                             "integration/cmd_test_test.sh")
                (("/bin/sh")
                 ;; The 'local-kyua' generated script in Makefile.am is used
                 ;; to run the built kyua binary for tests.
                 (which "sh")))))
          (add-after 'unpack 'fix-to_absolute-test
            ;; This test checks for the existence of /bin and /bin/ls.
            (lambda _
              (substitute* "utils/fs/path_test.cpp"
                (("chdir\\(\"/bin\")")
                 (format #f "chdir(~s)" (dirname (which "ls"))))
                (("\"/bin/ls\"")
                 (string-append "\"" (which "ls") "\"")))))
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              ;; The stacktrace tests expect core files to be dumped to the
              ;; current directory, which doesn't happen with our kernel
              ;; configuration (see:
              ;; https://github.com/freebsd/kyua/issues/214).
              (substitute* "utils/Kyuafile"
                ((".*atf_test_program.*stacktrace_test.*")
                 "")))))))
    (native-inputs (list autoconf automake gdb-minimal libtool pkg-config))
    (inputs (list atf lutok sqlite))
    (home-page "https://github.com/freebsd/kyua")
    (synopsis "Testing framework for infrastructure software")
    (description "Kyua is a testing framework for infrastructure software.
Kyua is lightweight and simple, and integrates well with various build systems
and continuous integration frameworks.  Kyua features an expressive test suite
definition language, a safe runtime engine for test suites and a powerful
report generation engine.")
    (license license:bsd-3)))

(define-public googletest
  (package
    (name "googletest")
    (version "1.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/googletest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zn701fgmbk29y45p49sajaswm01i2bv89ds2kkbiq8i0p2cr08w"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (native-inputs
     `(("python" ,python-wrapper)))
    (home-page "https://github.com/google/googletest/")
    (synopsis "Test discovery and XUnit test framework")
    (description "Google Test features an XUnit test framework, automated test
discovery, death tests, assertions, parameterized tests and XML test report
generation.")
    (license license:bsd-3)))

(define-public googletest-1.13
  (package
    (inherit googletest)
    (name "googletest")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/google/googletest")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x5lr1k4kgw3i8d7c12vp759p0w8c8r2y8lwvqswswxvwygw8lid"))))))

(define-public googletest-1.12
  (package
    (inherit googletest-1.13)
    (name "googletest")
    (version "1.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/google/googletest")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cv55x3amwrvfan9pr8dfnicwr8r6ar3yf6cg9v6nykd6m2v3qsv"))))))

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
    (version "1.9.1")
    (home-page "https://github.com/google/benchmark")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name "google-benchmark" version))
              (sha256
               (base32
                "1ky51j4hbqcaxp7i7sfiyh1435mgyijgbfgc48bbmi52vgay0477"))))
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

(define-public gotestsum
  (package
    (name "gotestsum")
    (version "1.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gotestyourself/gotestsum")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pj2d2rjab1zdzrc88ksf2v09fk9is9hg4rn6grcyaq76b8l3jcg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "gotest.tools/gotestsum"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list "TestE2E_IgnoresWarnings"
                     "TestE2E_MaxFails_EndTestRun"
                     "TestE2E_RerunFails/first_run_has_errors,_abort_rerun"
                     "TestE2E_RerunFails/reruns_continues_to_fail"
                     "TestE2E_RerunFails/reruns_until_success"
                     "TestScanTestOutput_TestTimeoutPanicRace/panic-race-2")
               "|"))
      ;; Run just unit test, integration tests from "testjson" require: run
      ;; 'go test . -update' to automatically update
      ;; testdata/summary/with-run-id to the new expected value.'
      #:test-subdirs #~(list "cmd/..." "internal/...")))
    (native-inputs
     (list go-github-com-bitfield-gotestdox
           go-github-com-dnephin-pflag
           go-github-com-fatih-color
           go-github-com-fsnotify-fsnotify
           go-github-com-google-go-cmp
           go-github-com-google-shlex
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-tools
           go-gotest-tools-v3))
    (home-page "https://github.com/gotestyourself/gotestsum")
    (synopsis "Go test runner with output optimized for humans")
    (description
     "This package provides a @code{go test} runner with output optimized for
humans, JUnit XML for CI integration, and a summary of the test results.")
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
     #:modules '((guix build cmake-build-system)
                 ((guix build gnu-build-system) #:prefix gnu:)
                 (guix build utils))
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
                      (replace 'check (assoc-ref gnu:%standard-phases 'check))
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
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/rhysd/actionlint")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xbsrcvklxn0lppikabwrizav945jk85d0mz16zc3spxc80plrvn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:build-flags
      #~(list (string-append
               "-ldflags=-X github.com/rhysd/actionlint.version=" #$version))
      #:import-path "github.com/rhysd/actionlint/cmd/actionlint"
      #:unpack-path "github.com/rhysd/actionlint"
      #:test-subdirs #~(list "../../"))) ;test from project's root
    ;; XXX: Install Man page, wrap with shellcheck and pyflakes.
    (native-inputs
     (list go-github-com-bmatcuk-doublestar-v4
           go-github-com-fatih-color
           go-github-com-google-go-cmp
           go-github-com-mattn-go-colorable
           go-github-com-mattn-go-runewidth
           go-github-com-mattn-go-shellwords
           go-github-com-robfig-cron-v3
           go-github-com-yuin-goldmark
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-gopkg-in-yaml-v3))
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
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parameterized" version))
       (sha256
        (base32 "1c89vc40zj5aj2zvbvw875wqpyf0x6xrqhm3q5jg797g5hkhbjbz"))))
    (build-system pyproject-build-system)
    ;; See: <https://github.com/wolever/parameterized/issues/181>,
    ;;      <https://github.com/wolever/parameterized/issues/167>,
    ;;      <https://github.com/wolever/parameterized/pull/162>.
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/wolever/parameterized")
    (synopsis "Parameterized testing with any Python test framework")
    (description
     "Parameterized is a Python library that aims to fix parameterized testing
for every Python test framework.  It supports nose, py.test, and unittest.")
    (license license:bsd-2)))

(define-public python-minimock
  (package
    (name "python-minimock")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lowks/minimock")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mby3y78w8zka3dwp2dnsq3a0bq9nxr5g0h538p6zbyjrqly5paj"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pynose python-setuptools))
    (home-page "https://github.com/lowks/minimock")
    (synopsis "Simple Python library for using mock objects")
    (description
     "MiniMock is a simple library for building mock objects with doctest.")
    (license license:expat)))

(define-public python-mock
  (package
    (name "python-mock")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mock" version))
       (sha256
        (base32 "0zbnp1kmf7ykc9bvlxamsp15rxsd0ar99v99lbh1hiysrkasm5jy"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-six))
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
    (version "0.15.1")
      (source
        (origin
          (method url-fetch)
          (uri (pypi-uri "nose2" version))
          (sha256
           (base32
            "1pzhqk7yzdw2s9hk7q5l9rmrz6xzpymy9gp0pwycvgpmkm8hyxrn"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; Tests require nose2 itself.
                     (setenv "PYTHONPATH" (getcwd))
                     (invoke (string-append #$output "/bin/nose2") "-v")))))))
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/nose-devs/nose2")
    (synopsis "Next generation of nicer testing for Python")
    (description
     "Nose2 is the next generation of nicer testing for Python, based on the
plugins branch of unittest2.  Nose2 aims to improve on nose by providing a
better plugin api, being easier for users to configure, and simplifying internal
interfaces and processes.")
    (license license:bsd-2)))

(define-public python-pytest
  (package
    (name "python-pytest")
    (version "8.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest" version))
       (sha256
        (base32 "0g593wjl45yck5g1xi8q31s08arxiapw67ipv6g3axs82xlzsrvw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Just 2 tests fail:
      ;;   1. access to "/usr/" is required.
      ;;   2. assertion fails to compare length of the list.
      ;;
      ;; 3780 passed, 119 skipped, 3 deselected, 11 xfailed, 1 xpassed
      #~(list "-k" "not test_remove_dir_prefix and not test_len")))
    (native-inputs
     ;; Tests need the "regular" bash since 'bash-final' lacks `compgen`.
     (list bash
           python-attrs-bootstrap
           python-hypothesis
           python-setuptools
           python-setuptools-scm
           python-xmlschema
           python-wheel))
    (propagated-inputs
     (list python-iniconfig
           python-packaging-bootstrap
           python-pluggy
           python-pygments-bootstrap))  ;it is in installation dependencies
    (home-page "https://docs.pytest.org/en/latest/")
    (synopsis "Python testing library")
    (description
     "Pytest is a testing tool that provides auto-discovery of test modules
and functions, detailed info on failing assert statements, modular fixtures,
and many external plugins.")
    (license license:expat)))

(define-deprecated python-pytest-6 python-pytest)
(export python-pytest-6)

(define-deprecated python-pytest-7 python-pytest)
(export python-pytest-7)

(define-public python-pytest-bootstrap
  (package/inherit python-pytest
    (name "python-pytest-bootstrap")
    (arguments (list #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))))

(define-public python-pytest-cov
  (package
    (name "python-pytest-cov")
    (version "6.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-cov" version))
        (sha256
         (base32 "1h5dvj4lr0qpbxqd38g66ab7q41vbx30n0kgszibi2r4raavbq7x"))))
    (build-system pyproject-build-system)
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
    (native-inputs
     (list python-setuptools
           python-wheel))
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
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bitprophet/pytest-relaxed")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j37gkgcag4rpv9g5q2n80425cs1alwx8fwncx3mv7sdgqgdgqnz"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Shaky tests with Pytest@8
     (list #:test-flags #~(list "-k" "not test_skips_pytest_fixture")))
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools))
    (propagated-inputs
     (list python-decorator))
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
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs (list python-dotenv))
    (home-page "https://github.com/quiqua/pytest-dotenv")
    (synopsis "Automatically detect and load a .env file before running tests")
    (description
     "This Pytest plugin automatically detects and loads environment variables
from a .env file before running tests.")
    (license license:expat)))

(define-public python-pytest-examples
  (package
    (name "python-pytest-examples")
    (version "0.0.13")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pydantic/pytest-examples")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11ifavc13s2b7grjv13l6am36v5kw8m9pj75j5hl4ch605ci4j27"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; TODO: Rework skipped tests when ruff is availalbe.
      ;; tests: 49 passed, 23 deselected, 1 xfailed
      #:test-flags
      #~(list "-k" (string-join
                    ;; Disable tests requiring pthon-ruff.
                    (list "not test_ruff"
                          "test_find_examples[case_python_class.py:3-10]"
                          "test_find_examples[my_file.md:3-5]"
                          "test_find_examples[my_file.md:7-9]"
                          "test_find_examples[my_file.py:5-7]"
                          "test_find_run_examples[my_file.md:1-4]"
                          "test_insert_print[example/README.md:3-33]"
                          "test_insert_print[example/README.md:37-40]"
                          "test_insert_print[example/README.md:44-47]"
                          "test_insert_print[example/README.md:49-66]"
                          "test_python_self[example/test_example.py:28-31]"
                          "test_python_self[example/test_example.py:40-43]"
                          "test_python_self_change_docstyle[example/test_example.py:28-31]"
                          "test_python_self_change_docstyle[example/test_example.py:40-43]"
                          "test_ruff_config"
                          "test_ruff_error"
                          "test_ruff_offset"
                          "test_ruff_ok"
                          ;; Black incompatability faileurs.
                          "test_black_error"
                          "test_black_error_multiline"
                          "test_cases_update[call_twice.md]"
                          "test_cases_update[dataclass_indent.md]"
                          "test_cases_update[long_python_lines.py]"
                          "test_cases_update[python_class.py]"
                          "test_cases_update[simple.md]"
                          "test_cases_update[simple.py]"
                          "test_find_run_examples[my_file.md:3-5]"
                          "test_run_example_ok_fail"
                          "test_update_files")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                ;; XXX: Removing ruff from required packages to pass Sanity
                ;; check, add it back when it's available.
                (("'ruff>=0.5.0',") "")))))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_httpserver" version))
       (sha256
        (base32 "00c7vki5qpl40qn5riyppkjnmgrvhsbg0hsdp6qm3m9fkqcv073b"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-poetry-core
           python-toml
           python-pytest))
    (propagated-inputs
     (list python-werkzeug))
    (home-page "https://github.com/csernazs/pytest-httpserver")
    (synopsis "HTTP server for pytest")
    (description
     "Pytest plugin library to test http clients without contacting the real
http server.")
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
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-setuptools
           python-wheel
           python-xmlschema))
    (propagated-inputs
     (list python-attrs
           python-pytest))
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
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-random-order" version))
       (sha256
        (base32 "104hww3b86jchk41kjhyycr541pd2dfgqkww6lx5y70z9z9xfwj4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; AttributeError: module 'py' has no attribute 'code'.
      #~(list "-k" (string-append "not test_it_works_with_actual_tests"
                                  " and not test_failed_first"
                                  " and not test_doctests"))))
    (native-inputs
     (list python-pytest-xdist
           python-setuptools
           python-py
           python-wheel))
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

(define-public python-pytest-runner
  (package
    (name "python-pytest-runner")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-runner" version))
       (sha256
        (base32
         "16zly218ij0n6fxzqsasia3vh9xkzl9w0cs9pwvqy057hnap7m3h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; FIXME: The test suite requires 'python-pytest-virtualenv',
      ;; but that introduces a circular dependency.
      #:tests? #f))
    (native-inputs
     (list python-pytest
           python-pytest-checkdocs
           python-pytest-enabler
           ;; python-pytest-virtualenv
           python-setuptools
           python-setuptools-scm
           python-types-setuptools
           python-wheel))
    (home-page "https://github.com/pytest-dev/pytest-runner")
    (synopsis "Invoke py.test as a distutils command")
    (description
     "This package provides a @command{pytest-runner} command that
@file{setup.py} files can use to run tests.")
    (license license:expat)))

(define-public python-pytest-lazy-fixtures
  (package
    (name "python-pytest-lazy-fixtures")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_lazy_fixtures" version))
       (sha256
        (base32 "1v819n9jrgf0rk2fidn6fkdzz68r4jin94lvy9fbb9gf546iymhc"))))
    (native-inputs
     (list python-poetry-core))
    (propagated-inputs
     (list python-pytest))
    (build-system pyproject-build-system)
    (home-page "https://github.com/dev-petrov/pytest-lazy-fixtures")
    (synopsis "Use fixtures in @code{pytest.mark.parametrize}")
    (description
     "This plugin helps to use fixtures in @code{pytest.mark.parametrize},
inspied by @url{https://github.com/TvoroG/pytest-lazy-fixture,
pytest-lazy-fixture}.")
    (license license:expat)))

(define-public python-pytest-mock
  (package
    (name "python-pytest-mock")
    (version "3.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mock" version))
       (sha256
        (base32 "1l0b864arbzrq13z635l1x9ial0w7pgz6svd0nyavkpy3rd2a697"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Skip the assertion rewriting tests, which don't work in the presence
      ;; of read-only Python modules (a limitation of Pytest).
      #:test-flags #~(list "--assert=plain")))
    (native-inputs
     (list python-pytest-asyncio
           python-setuptools
           python-setuptools-scm
           python-wheel))
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
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_xdist" version))
       (sha256
        (base32
         "1wd9sgcgmx5nj0n1qmfnjmahi38xkzaz54xac440bilvxhjq2mvy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Failes with execnet.gateway_base.RemoteError: Traceback (most recent
      ;; call last).
      #~(list "-k" "not test_ignore_sys_path_hook_entry")))
    (native-inputs
     (list python-filelock
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-execnet))
    (home-page "https://github.com/pytest-dev/pytest-xdist")
    (synopsis "Plugin for py.test with distributed testing and loop-on-failing modes")
    (description
     "The pytest-xdist plugin extends py.test with some unique test execution
modes: parallelization, running tests in boxed subprocesses, the ability to
run tests repeatedly when failed, and the ability to run tests on multiple
Python interpreters or platforms.  It uses rsync to copy the existing program
code to a remote location, executes there, and then syncs the result back.")
    (license license:expat)))

(define-public python-pytest-timeout
  (package
    (name "python-pytest-timeout")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-timeout" version))
       (sha256
        (base32
         "1nb5a6xf2p9zbcasy8136nbxpm1raag5n0q1rbdcnvjw28lpff8j"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pexpect
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-pytest))
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
     ;; See <https://github.com/pytest-dev/pytest-forked/issues/88>.
     (list #:tests? #f
           #:phases
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
     (list python-pytest-bootstrap python-setuptools
           python-setuptools-scm python-wheel))
    (propagated-inputs (list python-py))
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
    (version "2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pypa/scripttest")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07cyrh4yp8497radz8cx7la2p8yr78r77xm62hh77hcs1migznaf"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/pypa/scripttest")
    (synopsis "Python library to test command-line scripts")
    (description
     "Scripttest is a Python helper library for testing interactive
command-line applications.  With it you can run a script in a subprocess and
see the output as well as any file modifications.")
    (license license:expat)))

(define-public python-testtools
  (package
    (name "python-testtools")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testtools" version))
       (sha256
        (base32 "18vy77n4ab2dvgx5ni6gfp2d0haxhh3yrkm6mih8n3zsy30vprav"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'custom
      #:test-flags #~(list "-m" "testtools.run" "testtools.tests.test_suite")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; There are six failing tests:
              ;; "test_fast_keyboard_interrupt_stops_test_run"
              ;; "test_keyboard_interrupt_stops_test_run"
              ;; "test_fast_sigint_raises_no_result_error"
              ;; "test_fast_sigint_raises_no_result_error_second_time"
              ;; "test_sigint_raises_no_result_error"
              ;; "test_sigint_raises_no_result_error_second_time"
              (substitute* "testtools/tests/twistedsupport/__init__.py"
                (("test_spinner,") "")
                (("test_runtest,") "")))))))
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-twisted
           python-testscenarios-bootstrap))
    (propagated-inputs
     (list python-fixtures))
    (home-page "https://github.com/testing-cabal/testtools")
    (synopsis "Extensions to the Python standard library unit testing framework")
    (description
     "Testtools extends the Python standard library unit testing framework to
provide matchers, more debugging information, and cross-Python
compatibility.")
    (license license:expat)))

(define-public python-testtools-bootstrap
  (hidden-package
   (package/inherit python-testtools
     (arguments
      ;; To break cycle with python-fixtures.
      (list #:tests? #f))
     (propagated-inputs '()))))

;; XXX: The project is not maintained since 2015, consider to remove when
;; nothing depends on it.
(define-public python-testscenarios
  (package
    (name "python-testscenarios")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testscenarios" version))
       (sha256
        (base32 "1dm2aydqpv76vnsk1pw7k8n42hq58cfi4n1ixy7nyzpaj1mwnmy2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'custom
      #:test-flags #~(list "-m" "testtools.run" "testscenarios.test_suite")))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-pbr
           python-testtools))
    (home-page "https://launchpad.net/testscenarios")
    (synopsis "Pyunit extension for dependency injection")
    (description
     "Testscenarios provides clean dependency injection for Python unittest
style tests.")
    (license (list license:bsd-3 license:asl2.0)))) ; at the user's option

(define-public python-testscenarios-bootstrap
  (hidden-package
   (package/inherit python-testscenarios
     (arguments
      ;; To break cycle with python-testtols and python-subunit.
      (list #:tests? #f
            #:phases
            #~(modify-phases %standard-phases
                (delete 'sanity-check))))
     (propagated-inputs
      (list python-pbr)))))

(define-public python-testresources
  (package
    (name "python-testresources")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testresources" version))
       (sha256
        (base32 "1v7a3mzi461v4cw4qil094j39wzn8hk10jkm9gi9ybmb01z3vgrc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'custom
      #:test-flags #~(list "-m" "testtools.run" "discover")))
    (native-inputs
     (list python-fixtures
           python-setuptools
           python-testtools))
    (propagated-inputs
     (list python-pbr))
    (home-page "https://github.com/testing-cabal/testresources")
    (synopsis "Pyunit extension for managing test resources")
    (description
     "Testresources is an extension to Python's unittest to allow declarative
use of resources by test cases.")
    (license (list license:bsd-3 license:asl2.0)))) ; at the user's option

(define-public python-subunit
  (package
    (name "python-subunit")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-subunit" version))
       (sha256
        (base32 "0d7arni5h98x7mpgpv90bzipf6p6q9djcdwj4m2ky7da64qkcy8h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'custom
      #:test-flags #~(list "-m" "testtools.run" "subunit.test_suite")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              ;; There are two failing tests:
              ;; subunit.tests.test_test_protocol.TestExecTestCase.test_args
              ;; AssertionError: 1 != 0
              ;;
              ;; subunit.tests.test_test_protocol.TestExecTestCase.test_run
              ;; AssertionError: Lists differ: [('startTest',
              ;; <subunit.RemotedTestCase de[666 chars]r'>)] != []
              (substitute* "python/subunit/tests/test_test_protocol.py"
                (("test_args") "_off_test_args")
                (("test_run") "_off_test_run"))))
          (add-after 'disable-failing-tests 'disable-failing-console-scripts
            (lambda _
              (substitute* "setup.py"
                ;; Requries not maintained python-junitxml.
                (("'subunit2junitxml=.*subunit2junitxml:main',") "")
                ;; To avoid Gtk in inputs.
                (("'subunit-notify=.*subunit_notify:main',") "")
                (("'subunit2gtk=.*subunit2gtk:main',") "")))))))
    (native-inputs
     (list python-fixtures
           python-setuptools
           python-testscenarios))
    (propagated-inputs
     (list python-iso8601
           python-pygobject
           python-testtools))
    (home-page "https://github.com/testing-cabal/subunit")
    (synopsis "Python implementation of the subunit protocol")
    (description
     "Python-subunit is a Python implementation of the subunit test streaming
protocol.")
    (license (list license:bsd-3 license:asl2.0)))) ; at the user's option

(define-public python-fixtures
  (package
    (name "python-fixtures")
    (version "4.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fixtures" version))
       (sha256
        (base32 "02y92rnl2vyjcbc31mcpxkxjky6g9hjb2cxy5xkkl1j5n4ajniwm"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-testtools-bootstrap))
    (home-page "https://github.com/testing-cabal/fixtures")
    (synopsis "Python test fixture library")
    (description
     "Fixtures provides a way to create reusable state, useful when writing
Python tests.")
    (license (list license:bsd-3 license:asl2.0)))) ; at user's option

(define-public python-testrepository
  (package
    (name "python-testrepository")
    (version "0.0.21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testrepository" version))
       (sha256
        (base32 "1nd36zf1gz7rp5qqqvw15d8lwcdwcigs4m6ppn4cdcwhw3vzr2in"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (invoke "./testr" "init")
                (invoke "./testr" "run")))))))
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-pytz
           python-setuptools
           python-testresources
           python-testscenarios))
    (propagated-inputs
     (list python-fixtures
           python-iso8601
           python-subunit))
    (home-page "https://github.com/testing-cabal/testrepository")
    (synopsis "Database for Python test results")
    (description
     "Testrepository provides a database of test results which can be used as
part of a developer's workflow to check things such as what tests have failed
since the last commit or what tests are currently failing.")
    (license (list license:bsd-3 license:asl2.0)))) ; at user's option

(define-public python-coverage
  (package
    (name "python-coverage")
    (version "7.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "coverage" version))
       (sha256
        (base32 "0l403f6d59q8rik9vvzb6982qad0zrfj87dqydzsz8hwmh2dayqn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1303 passed, 23 skipped
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              #$@(map (lambda (file) (string-append "--ignore=tests/" file))
                      ;; XXX: The most of the tests fail.
                      (list "test_process.py"
                            ;; Unable to properly compare reports.
                            "test_report.py"
                            ;; Network connection is required
                            "test_venv.py"))
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "tests/test_python.py::"
                                                    "GetZipBytesTest::"
                                                    "test_get_encoded_zip_files"
                                                    test))
                      ;; > assert zip_data is not None
                      ;; E assert None is not None
                      (list "[utf-8]@get_zip_bytes_test"
                            "[gb2312]@get_zip_bytes_test"
                            "[hebrew]@get_zip_bytes_test"
                            "[shift_jis]@get_zip_bytes_test"
                            "[cp1252]@get_zip_bytes_test"))
              #$@(map (lambda (test) (string-append "--deselect=tests/test_"
                                                    test))
                      ;; AssertionError
                      (list "api.py::RelativePathTest::test_files_up_one_level"
                            "concurrency.py::SigtermTest::\
test_sigterm_multiprocessing_saves_data"
                            "oddball.py::DoctestTest::test_doctest"
                            "oddball.py::MockingProtectionTest::\
test_os_path_exists"
                            "plugins.py::PluginTest::\
test_local_files_are_importable"
                            "regions.py::test_real_code_regions"
                            "setup.py::SetupPyTest::test_metadata"
                            "testing.py::test_all_our_source_files"
                            "xml.py::XmlReportTest::test_no_duplicate_packages"
                            ;; FileNotFoundError
                            "setup.py::SetupPyTest::test_more_metadata"
                            ;; ModuleNotFoundError
                            "filereporter.py::FileReporterTest::test_zipfile"
                            ;; OSError
                            "annotate.py::AnnotationGoldTest::test_multi"))
              #$@(if (or (equal? (%current-system) "riscv64-linux")
                         (equal? (%current-system) "aarch64-linux"))
                     ;; Thypothesis.errors.FailedHealthCheck: Data generation
                     ;; is extremely slow.
                     (map (lambda (test) (string-append "--deselect="
                                                        "tests/test_numbits.py"
                                                        test))
                          (list "NumbitsOpTest::test_any_intersection"
                                "NumbitsOpTest::test_conversion"
                                "NumbitsOpTest::test_union"))
                     '()))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                (("-q -n auto ") ""))))
          (add-before 'check 'pre-check
            (lambda _
              ;; WARNING: The directory '/homeless-shelter/.cache/pip' or its
              ;; parent directory is not owned or is not writable by the
              ;; current user. The cache has been disabled. Check the
              ;; permissions and owner of that directory. If executing pip
              ;; with sudo, you should use sudo's -H flag.
              (setenv "HOME" "/tmp")
              ;; This would otherwise interfere with finding the installed
              ;; coverage when running tests.
              (delete-file-recursively "coverage"))))))
    (native-inputs
     (list python-pytest
           python-pytest-xdist  ;some tests need xdist_group
           python-flaky
           python-setuptools))
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
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_asyncio" version))
       (sha256
        (base32 "0gwbqikldfy5yvyzhi71h7vicni2dchj0iarsbmf2mj47z8n6m6i"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; 4 failed, 163 passed
      #~(list "-k" (string-join
                    (list "not test_strict_mode_ignores_unmarked_coroutine"
                          "test_strict_mode_ignores_unmarked_fixture"
                          "test_strict_mode_marked_test_unmarked_fixture_warning"
                          "test_strict_mode_marked_test_unmarked_autouse_fixture_warning")
                    " and not "))))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-pytest
           python-typing-extensions))
    (home-page "https://github.com/pytest-dev/pytest-asyncio")
    (synopsis "Pytest support for asyncio")
    (description "Python asyncio code is usually written in the form of
coroutines, which makes it slightly more difficult to test using normal
testing tools.  @code{pytest-asyncio} provides useful fixtures and markers
to make testing async code easier.")
    (license license:asl2.0)))

(define-public python-pytest-asyncio-0.23
  (package
    (inherit python-pytest-asyncio)
    (version "0.23.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_asyncio" version))
       (sha256
        (base32 "1lz4h8y6m6hxnsl7kqh0rjxqp5q2wc2m5gd88371rikd7ari16vm"))))))

(define-public python-pytest-asyncio-0.26
  (package
    (inherit python-pytest-asyncio)
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_asyncio" version))
       (sha256
        (base32 "0bri1scs8whdrxb3jrv0ywiz20qb0mrll3kzkvriy928frljmpy4"))))))

(define-public python-pytest-asyncio-0.21
  (package
    (inherit python-pytest-asyncio)
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_asyncio" version))
       (sha256
        (base32 "0iag2lmglfajiasxi3dr2110gq0nxc5paq6hp4kb751b4gy3hxyn"))))
    (native-inputs
     (list python-flaky
           python-pytest-trio
           python-setuptools-scm
           python-setuptools
           python-wheel))))

(define-public python-codecov
  (package
    (name "python-codecov")
    (version "2.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "codecov" version))
        (sha256
         (base32
          "131av6sdzxf3qqqb85fm3mcd771mrrv9n6lmp52szbiwcf2vcqi3"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ;no tests in PyPI, tests in git are broken
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-coverage
           python-requests))
    (home-page "https://github.com/codecov/codecov-python")
    (synopsis "Upload code coverage reports to @code{codecov.io}")
    (description
     "Codecov collects code coverage reports from code written in Python, Java,
C/C++, R, and more, and uploads it to the @code{codecov.io} service.")
    (license license:asl2.0)))

(define-public python-testpath
  (package
    (name "python-testpath")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jupyter/testpath")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pib1xsvjwwyyhv0sqzxvgg814k83dmv1ppwfkkq9llkhr8k7s9y"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-flit-core
           python-pytest))
    (home-page "https://github.com/jupyter/testpath")
    (synopsis "Test utilities for code working with files and commands")
    (description
     "Testpath is a collection of utilities for Python code working with files
and commands.  It contains functions to check things on the file system, and
tools for mocking system commands and recording calls to those.")
    (license license:expat)))

(define-public python-pytest-localserver
  (package
    (name "python-pytest-localserver")
    (version "0.9.0.post0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_localserver" version))
       (sha256
        (base32 "1w2zgpdr1wj7mxnsyqyiy3alqrdm7wngxb7ra14brll2ndps6cw0"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-requests
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-werkzeug))
    (home-page "https://pypi.org/project/pytest-localserver/")
    (synopsis "Py.test plugin to test server connections locally")
    (description
     "Pytest-localserver is a plugin for the pytest testing framework which
enables you to test server connections locally.")
    (license license:expat)))

(define-public python-pytest-xprocess
  (package
    (name "python-pytest-xprocess")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-xprocess" version))
       (sha256
        (base32 "03y61h42sc343ddhsz7glxmv9ga83k4grrayfmbbrsl6fmip1qhm"))))
    (build-system pyproject-build-system)
    (arguments
     (if (target-riscv64?)
         (list
          #:test-flags
          #~(list "-k"
                  ;; Unclear why these tests are failing on riscv64-linux.
                  (string-append "not test_startup_without_pattern"
                                 " and not test_startup_with_pattern_and_callback")))
         '()))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-pytest python-psutil))
    (home-page "https://github.com/pytest-dev/pytest-xprocess/")
    (synopsis "Pytest plugin to manage external processes across test runs")
    (description
     "Pytest-xprocess is an experimental py.test plugin for managing processes
across test runs.")
    (license license:expat)))

(define-public python-pytest-sugar
  (package
    (name "python-pytest-sugar")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-sugar" version))
       (sha256
        (base32 "02kc4y0ry4y9lp63kjq9p7yvbjijfxn1fcn6wx6c1c7mb0rfh8k4"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-packaging python-pytest python-termcolor))
    (native-inputs
     (list python-setuptools python-wheel))
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
    (version "6.135.26")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hypothesis" version))
       (sha256
        (base32 "0073lb8xp789fxs5g8dmi3pr2p8q7imfsksidy0ccfahrm30xbvk"))))
    (build-system pyproject-build-system)
    (arguments
     ;; XXX: Tests are not distributed with the PyPI archive.
     (list #:tests? #f))
    (native-inputs
     (list python-pytest-bootstrap      ;to pass sanity check
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-attrs-bootstrap
           python-exceptiongroup
           python-sortedcontainers))
    (home-page "https://hypothesis.works/")
    (synopsis "Library for property based testing")
    (description "Hypothesis is a library for testing your Python code against a
much larger range of examples than you would ever want to write by hand.  It’s
based on the Haskell library, Quickcheck, and is designed to integrate
seamlessly into your existing Python unit testing work flow.")
    (license license:mpl2.0)))

(define-deprecated/public-alias python-hypothesis-next
  python-hypothesis)                   ;may be removed after 2025-12-01

;; WARNING: This package is a dependency of mesa.
(define-public python-lit
  (package
    (name "python-lit")
    (version "18.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lit" version))
        (sha256
         (base32 "1nsf3ikvlgvqqf185yz5smkvw0268jipdvady0qfh6llhshp9ha7"))))
    (build-system pyproject-build-system)
    (arguments (list #:test-backend #~'custom
                     #:test-flags #~(list "lit.py" "tests")))
    ;; This can be built with any version of llvm.
    (native-inputs
     (list llvm
           python-setuptools))
    (home-page "https://llvm.org/")
    (synopsis "LLVM Software Testing Tool")
    (description "@code{lit} is a portable tool for executing LLVM and Clang
style test suites, summarizing their results, and providing indication of
failures.")
    (license license:ncsa)))

(define-public python-pytest-enabler
  (package
    (name "python-pytest-enabler")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_enabler" version))
       (sha256
        (base32 "0gxsr0wk8kzf8rqfnhy84yh1zi0b55j76c0m9n006d8q4bzy1v25"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-k" "not test_coverage_explicit")))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-importlib-resources
           python-jaraco-context
           python-jaraco-functools
           python-toml))
    (home-page "https://github.com/jaraco/pytest-enabler")
    (synopsis "Enable installed pytest plugins")
    (description "Enable installed pytest plugins")
    (license license:expat)))

(define-public python-pytest-freezegun
  (package
    (name "python-pytest-freezegun")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ktosiek/pytest-freezegun")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10c4pbh03b4s1q8cjd75lr0fvyf9id0zmdk29566qqsmaz28npas"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-freezegun python-pytest))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/ktosiek/pytest-freezegun")
    (synopsis "Pytest plugin to freeze time in test fixtures")
    (description
     "The @code{pytest-freezegun} plugin wraps tests and fixtures with
@code{freeze_time}, which controls (i.e., freeze) the time seen by the test.")
    (license license:expat)))

(define-public python-pytest-mypy
  (package
    (name "python-pytest-mypy")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_mypy" version))
       (sha256
        (base32 "0x60ap70ix4blavrwih9aglp3ghviv1frxccnv3cq3f8fpzwlprz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "-p" "no:mypy"
              ;; Fails to compare warnings.
              "--deselect=tests/test_pytest_mypy.py::test_mypy_encoding_warnings")
      #:phases
      (if (or (target-riscv64?)
              (target-ppc64le?))
          #~(modify-phases %standard-phases
              (add-after 'unpack 'extend-test-timeout
                (lambda _
                  ;; Some architectures need an even longer timeout.
                  (substitute* "tests/test_pytest_mypy.py"
                    (("60\\.0") "180.0")))))
          #~%standard-phases)))
    (native-inputs
     (list python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-filelock
           python-mypy
           python-pytest))
    (home-page "https://github.com/dbader/pytest-mypy")
    (synopsis "Mypy static type checker plugin for Pytest")
    (description "@code{pytest-mypi} is a static type checker plugin for
Pytest that runs the mypy static type checker on your source files as part of
a Pytest test execution.")
    (license license:expat)))

(define-public python-pytest-mypy-plugins
  (package
    (name "python-pytest-mypy-plugins")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mypy-plugins" version))
       (sha256
        (base32 "1gpynypn13032by633dr2zhng54v2gl09kwgp3ysc4wpwl09pyhl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    (list "not reveal_type_extension_is_loaded"
                          "test_pyproject_toml"
                          "test_ini_files")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests failed to run in project's root directory:
          ;; ModuleNotFoundError: No module named 'pytest_mypy_plugins.tests'
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (with-directory-excursion "/tmp"
                  (apply invoke "pytest" "-v" test-flags))))))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-decorator
           python-jinja2
           python-jsonschema
           python-mypy
           python-packaging
           python-pytest
           python-pyyaml
           python-regex
           python-tomlkit))
    (home-page "https://github.com/TypedDjango/pytest-mypy-plugins")
    (synopsis "Pytest plugin for writing tests for mypy plugins")
    (description "This package provides a pytest plugin for writing tests for
mypy plugins.")
    (license license:expat)))

(define-public python-pytest-mypy-testing
  (package
    (name "python-pytest-mypy-testing")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mypy-testing" version))
       (sha256
        (base32 "0kqb3hi4jrc4knhc9pxv44m7c1jjkkwqx0dyghq4mw6wphlsv3q8"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-flit-core))
    (propagated-inputs (list python-mypy python-pytest))
    (home-page "https://github.com/davidfritzsche/pytest-mypy-testing")
    (synopsis "Pytest plugin to check mypy output")
    (description "This package provides a pytest plugin to test that mypy
produces a given output.  As mypy can be told to display the type of an
expression this allows you to check mypys type interference.")
    (license (list license:expat license:asl2.0))))

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
           python-pytest-mypy
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-jaraco-context
           python-jaraco-functools
           python-more-itertools
           python-packaging
           python-pip
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
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k" "not test_syntax_error")))
    (native-inputs
     (list python-coverage python-pytest python-setuptools python-wheel))
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
    (version "3.3.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PyCQA/pylint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h6ynaxsgb5bidjkbrfwb05nc7dw9paxc0bjgkqaxrkpqfm39baj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "-W" "ignore::DeprecationWarning"
              "--ignore=tests/benchmark"
               ;; The unused but collected 'primer'-related test files require
               ;; the extraneous 'git' Python module; ignore them.
              "--ignore=tests/testutils/_primer/test_package_to_lint.py"
              "--ignore=tests/testutils/_primer/test_primer.py"
              "-k" (string-join
                    ;; Test failing with multiple assertion errors.
                    (list "not test_functional"
                          "test_functional_relation_extraction")
                    " and not "))
     #:phases
       #~(modify-phases %standard-phases
         (add-before 'check 'set-home
           (lambda _
               (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-pytest-timeout
           python-pytest-xdist
           python-setuptools))
    (propagated-inputs
     (list python-astroid
           python-dill
           python-isort
           python-mccabe
           python-platformdirs
           python-tomlkit))
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
    (version "1.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/behave/behave")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fxj51kailsdakqi7qbw700k258n7r3lv75mgxs45ld5xh2jfyxh"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-assertpy
           python-chardet
           python-mock
           python-path
           python-pyhamcrest
           python-pytest
           python-pytest-html
           python-setuptools))
    (propagated-inputs
     (list python-colorama
           python-cucumber-expressions
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

(define-public python-freezegun
  (package
    (name "python-freezegun")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "freezegun" version))
       (sha256
        (base32 "0pmwcjf0vxmars4kncb35pvrckp0j97lya5aqzbzy2m56d721inp"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-dateutil))
    (home-page "https://github.com/spulec/freezegun")
    (synopsis "Test utility for mocking the datetime module")
    (description
     "FreezeGun is a library that allows your python tests to travel through
time by mocking the datetime module.")
    (license license:asl2.0)))

(define-public python-flaky
  (package
    (name "python-flaky")
    (version "3.8.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "flaky" version))
              (sha256
               (base32
                "1xcrjrr63131n2ydj5hn0gagka5dpkmdlqdxrxd3spwhxj0ll827"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Check with upstream. Tests failing with AttributeError: 'str'
      ;; object has no attribute 'tb_frame', ValueError: too many values to
      ;; unpack (expected 2), AssertionError: 'Plain HelloPlain Hellońőń ȁŝćȉȉ
      ;; ŝƭȕƒƒ' != 'Plain Hellońőń ȁŝćȉȉ ŝƭȕƒƒ'.
      #:test-flags
      #~(list "-k" (string-append
                    "not test_flaky_plugin_handles_non_ascii_byte_string_in_exception"
                    " and not test_flaky_plugin_identifies_failure"
                    " and not test_write_then_read"
                    " and not  test_writelines_then_read"
                    " and not test_something_flaky"))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
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
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyhamcrest" version))
       (sha256
        (base32 "175xfp4zgjmsxac79z39in77pjg7yck1kbr25kkvf31xjb0bxb66"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-pytest-bootstrap))
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

(define-public toml-test
  (package
    ;; Upstream is informed to provide man/info for the project, see
    ;; <https://github.com/toml-lang/toml-test/issues/163>.
    (name "toml-test")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/toml-lang/toml-test")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b6lfamh673a4x509cacr6qr1xvf82562cpqn9ygrgnd81469qcc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/toml-lang/toml-test/cmd/toml-test"
      #:unpack-path "github.com/toml-lang/toml-test"))
    (native-inputs
     (list go-zgo-at-zli
           go-zgo-at-jfmt
           go-github-com-burntsushi-toml))
    (home-page "https://github.com/toml-lang/toml-test")
    (synopsis "Language agnostic test suite for TOML parsers")
    (description
     "@samp{toml-test} is a language-agnostic test suite to verify the
correctness of @url{https://toml.io,TOML} parsers and writers.  Tests are
divided into two groups: @emph{invalid} and @emph{valid}.  Decoders or
encoders that reject @emph{invalid} tests pass the tests, and decoders that
accept @emph{valid} tests and output precisely what is expected pass the
tests.  The output format is JSON.")
    (license license:expat)))

(define-public trompeloeil
  (package
    (name "trompeloeil")
    (version "49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rollbear/trompeloeil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m4bfzcj033qfk3sihbikrhk9krsdbsqk79nsambnsnqqcgc2903"))))
    (build-system cmake-build-system)
    (arguments
     (append
      (if (%current-target-system)
          (list)
          (list #:configure-flags #~(list "-DTROMPELOEIL_BUILD_TESTS=yes")))
      (list
       #:phases #~(modify-phases %standard-phases
                    (replace 'check
                      (lambda* (#:key tests? #:allow-other-keys)
                        (when tests?
                          (invoke "test/self_test"))))))))
    (native-inputs (list catch2-3))
    (home-page "https://github.com/rollbear/trompeloeil")
    (synopsis "Header only C++14 mocking framework")
    (description
     "Trompeloeil is a thread-safe header-only mocking framework for C++11/14.")
    (license license:boost1.0)))

(define-public tidyall
  (package
    (name "tidyall")
    (version "0.84")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DR/DROLSKY/Code-TidyAll-" version
             ".tar.gz"))
       (sha256
        (base32 "1xxil8yfbd4nizwaaris07sp441nhx3ixr4qj13l1x6pxphi9h5k"))))
    (build-system perl-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (delete 'remove-command-line-tool)
                   (add-after 'install 'wrap-programs
                     (lambda _
                       (wrap-program (string-append #$output "/bin/tidyall")
                         (list "PERL5LIB" ":"
                               'prefix
                               (list (getenv "PERL5LIB")
                                     (string-append #$output
                                                    "/lib/perl5/site_perl")))))))))
    (native-inputs (list perl-test-class-most
                         perl-test-differences
                         perl-test-fatal
                         perl-test-warnings
                         perl-lib-relative
                         php))
    (inputs (list bash-minimal))
    (propagated-inputs (list perl-capture-tiny
                             perl-config-ini
                             perl-timedate
                             perl-file-which
                             perl-ipc-run3
                             perl-ipc-system-simple
                             perl-list-compare
                             perl-list-someutils
                             perl-log-any
                             perl-module-runtime
                             perl-moo-2
                             perl-path-tiny
                             perl-scope-guard
                             perl-specio
                             perl-specio-library-path-tiny
                             perl-text-diff
                             perl-time-duration-parse
                             perl-try-tiny
                             perl-parallel-forkmanager
                             perl-file-pushd))
    (home-page "https://metacpan.org/release/Code-TidyAll")
    (synopsis "Engine for tidyall, your all-in-one code tidier and validator")
    (description
     "@command{tidyall} makes a lot of code tidiers and validators available
from a single unified interface.  You can run @command{tidyall} on a
single file or on an entire project hierarchy, and configure which
tidiers/validators are applied to which files.  @command{tidyall} will
back up files beforehand, and for efficiency will only consider files
that have changed since they were last processed.

Note that if you see some missing tidier or validator modules error,
you can let tidyall load them after install them.  For example, one
can run @code{guix shell perl-perl-tidy perl} in advance to load
@code{Perl::Tidy}.")
    (license license:perl-license)))

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
     (list
      #:tests? #f                       ; It's run after build automatically.
      ;; Fix 'Version:' setting in .pc file. See:
      ;; <https://github.com/unittest-cpp/unittest-cpp/pull/188>
      #:configure-flags #~(list (string-append "-DPACKAGE_VERSION=" #$version))))
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

(define-public unity-test
  (package
    (name "unity-test")
    (version "2.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ThrowTheSwitch/Unity")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1s0jj9f2zav49mn9ib90idcmb6hq93aczbqysn5hj6binjmrnjw3"))
              (patches (search-patches "unity-test-set-subdir-correctly.patch"
                                       "unity-test-support-int64.patch"))))
    (build-system meson-build-system)
    (arguments
      (list #:configure-flags
            #~(list "-Dsupport_double=true"
                    "-Dsupport_int64=true")
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'patch-source-shebangs 'patch-more-shebangs
                  (lambda _
                    (substitute* '("auto/unity_test_summary.rb")
                      (("/usr/bin/ruby")
                       (string-append #$(this-package-input "ruby")
                                      "/bin/ruby")))
                    (substitute* '("auto/stylize_as_junit.py"
                                   "auto/unity_test_summary.py")
                      (("#! python3")
                       (string-append #$(this-package-input "python")
                                      "/bin/python3")))))
                (add-after 'install 'install-extra
                  (lambda _
                    (for-each (lambda (x)
                                (install-file x
                                  (string-append #$output "/share/unity-test")))
                              (find-files "../source/auto")))))))
    (native-inputs (list pkg-config))
    (inputs (list python ruby))
    (home-page "https://github.com/ThrowTheSwitch/Unity")
    (synopsis "Simple unit testing for C")
    (description "This package provides a unit testing framework built for C,
with a focus on working with embedded toolchains.")
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

          #$@(if (target-64bit?)
                 #~()
                 #~((add-after 'unpack 'apply-32bit-patch
                      (lambda _
                        (let ((patch #$(local-file
                                  (search-patch "libfaketime-32bit.patch"))))
                          (invoke "patch" "--force" "-p1" "-i" patch))))))

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
    (native-inputs (list perl))                   ;for tests
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
         (patches (search-patches "rapidcheck-fix-libs.patch"))
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
               ;; https://github.com/martinpitt/umockdev/issues/228#issuecomment-1968397286
               (add-after 'unpack 'compat-with-meson-1.4
                 (lambda _
                   (substitute* "meson.build"
                     (("-Werror=unused-variable") "-Wno-error=unused-variable"))))
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
                           "DESTINATION include/virtest"))))
                    (add-after 'unpack 'gcc14
                      (lambda _
                        (substitute* "vir/test.h"
                          (("#include <cmath>" all)
                            (string-append all "\n#include <cstdint>"))))))))
      (synopsis "Header-only test framework")
      (description
       "@code{virtest} is a small header-only test framework for C++.  It
grew out of the @dfn{Vc} project.")
      (license license:bsd-3))))

(define-public python-pyfakefs
  ;; TODO: Move to python-build.
  (package
    (name "python-pyfakefs")
    (version "5.9.1")
    (source
     (origin
       (method git-fetch)       ;no tets in PyPI archive
       (uri (git-reference
              (url "https://github.com/pytest-dev/pyfakefs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xdq8fgbqrrnhwbgklwfngmpzgna2frr9vc318vy1rb0vhsmzvjk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'custom
      ;; Ran 2699 tests; 1177 skipped, 2 expected failures
      #:test-flags
      #~(list "-m" "pyfakefs.tests.all_tests_without_extra_packages")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-root-tests
            (lambda _
              (substitute* "pyfakefs/tests/fake_filesystem_test.py"
                (("def test_expand_root")
                 "def __off_test_expand_root")))))))
    ;; As it would be imported by many through python-importlib-metadata, keep
    ;; dependencies on bare minimal.
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/pytest-dev/pyfakefs")
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
    (version "1.5.0")
    (source
     (origin
       (method git-fetch) ;no tets in PyPI archive
       (uri (git-reference
              (url "https://github.com/kwarunek/aiounittest.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0266i5z589jh75hjzakvwcqx5shgv5zis8mr70qa209v7jjclzfd"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-wrapt))
    (home-page "https://github.com/kwarunek/aiounittest")
    (synopsis "Test asyncio code more easily")
    (description "Aiounittest is a library that helps write tests using
asynchronous code in Python (asyncio).")
    (license license:expat)))

(define-public python-pytest-datadir
  (package
    (name "python-pytest-datadir")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_datadir" version))
       (sha256
        (base32
         "0gj49053hzb3xchzrsp14hj7d4fakhxmvrv895g218yh6n525x8m"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-bootstrap
           python-setuptools
           python-setuptools-scm))
    (home-page "https://github.com/gabrielcnr/pytest-datadir")
    (synopsis "Pytest plugin for manipulating test data directories and files")
    (description
     "This package provides a Pytest plugin for manipulating test data
directories and files.")
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
    (native-inputs (list python-setuptools python-wheel))
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
          (add-after 'check 'install-wrapped-script
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
          (add-after 'build-manual 'install-manual
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
    (version "1.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/testing-cabal/subunit")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fcqxh4cwhn9yz2z1zszzvgc554gai5b7c59w45a4cg9y9p30h8d"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           check
           cppunit
           libtool
           pkg-config
           python-fixtures
           python-hypothesis
           python-testscenarios))
    (inputs
     (list perl
           python))
    (propagated-inputs
     (list python-iso8601
           python-pygobject
           python-testtools))
    (home-page "https://github.com/testing-cabal/subunit")
    (synopsis "Test reporting and control protocol")
    (description
     "Subunit is a streaming protocol for test results.  Subunit comes with
command line filters to process a subunit stream and language bindings for
Python, C, C++ and shell.  Bindings are easy to write for other languages.")
    (license (list license:asl2.0 license:bsd-3)))) ;user can pick

(define-public uitest
  ;; XXX: No releases.
  (let ((commit "d845427140cbcbce99bb6c72919199ac5f033784")
        (revision "0"))
    (package
      (name "uitest")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Grumbel/uitest")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bgpqakls8bs3n3v0igni9xgyrc4sp43b4bf85f7jnv1rxr88gf4"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "-DBUILD_TESTS=ON")))
      (native-inputs (list googletest tinycmmc))
      (home-page "https://github.com/Grumbel/uitest")
      (synopsis "Simple testing framework for interactive tests")
      (description
       "This package is a simple helper library for writing interactive tests.")
      (license license:gpl3+))))

(define-public munit
  ;; Last release in 2016, see also <https://github.com/nemequ/munit/issues/95>.
  (let ((commit "fbbdf1467eb0d04a6ee465def2e529e4c87f2118")
        (revision "1"))
    (package
      (name "munit")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nemequ/munit")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "13725v4pps2bpndniksa58nqi9gvx0f0900k0rqvp95bxw5z8vda"))))
      (build-system meson-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-install
              ;; munit is paradoxically configured to only be installed
              ;; when built as a subproject.
              ;; See <https://github.com/nemequ/munit/pull/67> for a
              ;; pull request that aims to fix this.  As we don't care about
              ;; bundling scenarios (or rather: aim to unbundle everything),
              ;; install it unconditionally.
              (lambda _
                (substitute* "meson.build"
                  (("install: meson.is_subproject\\(\\)")
                   "install: true")))))))
      (synopsis "Small unit testing framework for C")
      (description
       "µnit is a small testing framework for C with nested test suites,
parameterized tests, timing of the wall clock and CPU time, reproducible
random number generation, and more.")
      (home-page "https://nemequ.github.io/munit/")
      (license license:x11))))
