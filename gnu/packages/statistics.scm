;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Vicente Vera Parra <vicentemvp@gmail.com>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016, 2017 Raoul Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017–2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018 Alex Branham <alex.branham@gmail.com>
;;; Copyright © 2020 Tim Howes <timhowes@lavabit.com>
;;; Copyright © 2021, 2022, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021 Frank Pursel <frank.pursel@gmail.com>
;;; Copyright © 2022 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2023 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2023 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Jonas Freimuth <jonas.freimuth@posteo.de>
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

(define-module (gnu packages statistics)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))


(define-public pspp
  (package
    (name "pspp")
    (version "2.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pspp/pspp-"
                          version ".tar.gz"))
      (sha256
       (base32
        "002c08rxym056mn7a73jwjmcazqd4gh5j1cyml603y4ckvqb1nwf"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-test-suite
		 (lambda _
		   (substitute* "tests/output/tex.at"
		     (("AT_CHECK\\(\\[LC_ALL=C.UTF-8 pspp")
		      "AT_CHECK([LC_ALL=en_US.UTF-8 pspp"))))
               (add-before 'check 'prepare-tests
                 ;; Prevent irrelevant errors that cause test output mismatches:
                 ;; ‘Fontconfig error: No writable cache directories’
                 (lambda _
                   (setenv "XDG_CACHE_HOME" (getcwd)))))))
    (inputs
     (list cairo
           gettext-minimal
           gsl
           libxml2
           pango
           readline
           gtk+
           gtksourceview-3
           spread-sheet-widget
           zlib))
    (native-inputs
     (list autoconf ;for tests
           `(,glib "bin") ;for glib-genmarshal
           perl
           pkg-config
           python-3 ;for tests
           texinfo))
    (home-page "https://www.gnu.org/software/pspp/")
    (synopsis "Statistical analysis")
    (description
     "GNU PSPP is a statistical analysis program.  It can perform
descriptive statistics, T-tests, linear regression and non-parametric tests.
It features both a graphical interface as well as command-line input.  PSPP
is designed to interoperate with Gnumeric, LibreOffice and OpenOffice.  Data
can be imported from spreadsheets, text files and database sources and it can
be output in text, PostScript, PDF or HTML.")
    (license license:gpl3+)))

(define-public jags
  (package
    (name "jags")
    (version "4.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mcmc-jags/JAGS/"
                                  (version-major version) ".x/Source/"
                                  "JAGS-" version ".tar.gz"))
              (sha256
               (base32
                "0aa2w4g5057vn1qjp954s2kwxfmy1h7p5yn56fyi7sz9nmaq69gr"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--with-lapack=-lopenblas"
              "--with-blas=-lopenblas")))
    (home-page "https://mcmc-jags.sourceforge.net/")
    (native-inputs
     (list gfortran openblas))
    (synopsis "Gibbs sampler")
    (description "JAGS is Just Another Gibbs Sampler.  It is a program for
analysis of Bayesian hierarchical models using Markov Chain Monte Carlo (MCMC)
simulation not wholly unlike BUGS.  JAGS was written with three aims in mind:

@enumerate
@item To have a cross-platform engine for the BUGS language;
@item To be extensible, allowing users to write their own functions,
  distributions and samplers;
@item To be a platform for experimentation with ideas in Bayesian modelling.
@end enumerate\n")
    (license license:gpl2)))

(define-public libxls
  (package
    (name "libxls")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libxls/libxls/releases/download/"
                           "v" version "/libxls-" version ".tar.gz"))
       (sha256
        (base32 "0wg3ymr43aa1j3scyl9x83b2xgg7wilzpil0dj91a8dzji6w7b2x"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/libxls/libxls")
    (synopsis "Read binary (.xls) Excel spreadsheet files")
    (description
     "libxls is a C library to read .xls spreadsheet files in the binary OLE
BIFF8 format as created by Excel 97 and later versions.  It cannot write them.

This package also provides @command{xls2csv} to export Excel files to CSV.")
    (license license:bsd-2)))

;; Update this package together with the set of recommended packages: r-boot,
;; r-class, r-cluster, r-codetools, r-foreign, r-kernsmooth, r-lattice,
;; r-mass, r-matrix, r-mgcv, r-nlme, r-nnet, r-rpart, r-spatial, r-survival.
(define r-with-tests
  (package
    (name "r-with-tests")
    (version "4.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-major version) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1v0pp3zdyyyz7krhr9mng48fhsf5k6zxhj9yfic6p1ld7rhcsy0m"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:disallowed-references `(,tzdata-for-tests)
      #:make-flags
      #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib/R/lib")
              ;; This affects the embedded timestamp of only the core packages.
              "PKG_BUILT_STAMP=1970-01-01")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'do-not-compress-serialized-files
            (lambda _
              ;; This ensures that Guix can detect embedded store references;
              ;; see bug #28157 for details.
              (substitute* "src/library/base/makebasedb.R"
                (("compress = TRUE") "compress = FALSE"))
              (substitute* '("src/library/tools/Makefile.in"
                             "share/make/basepkg.mk"
                             "share/make/lazycomp.mk")
                (("makeLazyLoading\\(")
                 "makeLazyLoading(compress=FALSE,"))))
          (add-before 'configure 'patch-coreutils-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((uname-bin (search-input-file inputs "/bin/uname"))
                    (rm-bin (search-input-file inputs "/bin/rm")))
                (substitute* "src/scripts/R.sh.in"
                  (("uname") uname-bin))
                (substitute* "src/unix/sys-std.c"
                  (("rm -Rf ") (string-append rm-bin " -Rf ")))
                (substitute* "src/library/parallel/R/detectCores.R"
                  (("'grep")
                   (string-append "'"
                                  (search-input-file inputs "/bin/grep")))
                  (("\\| wc -l")
                   (string-append "| "
                                  (search-input-file inputs "/bin/wc")
                                  " -l"))))))
          (add-after 'unpack 'patch-tests
            (lambda _
              ;; This is needed because R is run during the check phase and
              ;; /bin/sh doesn't exist in the build container.
              (substitute* "src/unix/sys-unix.c"
                (("\"/bin/sh\"")
                 (string-append "\"" (which "sh") "\"")))
              ;; This test fails because line numbers are off by two.
              (substitute* "tests/reg-packages.R"
                (("8 <= print" m) (string-append "## " m)))))
          (add-after 'unpack 'build-reproducibly
            (lambda _
              ;; The documentation contains time stamps to demonstrate
              ;; documentation generation in different phases.
              (substitute* "src/library/tools/man/Rd2HTML.Rd"
                (("\\\\%Y-\\\\%m-\\\\%d at \\\\%H:\\\\%M:\\\\%S")
                 "(removed for reproducibility)"))

              ;; Remove timestamp from tracing environment.  This fixes
              ;; reproducibility of "methods.rd{b,x}".
              (substitute* "src/library/methods/R/trace.R"
                (("dateCreated = Sys.time\\(\\)")
                 "dateCreated = as.POSIXct(\"1970-1-1 00:00:00\", tz = \"UTC\")"))

              ;; Ensure that gzipped files are reproducible.
              (substitute* '("src/library/grDevices/Makefile.in"
                             "doc/manual/Makefile.in")
                (("R_GZIPCMD\\)" line)
                 (string-append line " -n")))

              ;; The "srcfile" procedure in "src/library/base/R/srcfile.R"
              ;; queries the mtime of a given file and records it in an object.
              ;; This is acceptable at runtime to detect stale source files,
              ;; but it destroys reproducibility at build time.

              ;; Similarly, the "srcfilecopy" procedure records the current
              ;; time.  We change both of them to respect SOURCE_DATE_EPOCH.
              (substitute* "src/library/base/R/srcfile.R"
                (("timestamp <- (timestamp.*|file.mtime.*)" _ time)
                 (string-append "timestamp <- \
as.POSIXct(if (\"\" != Sys.getenv(\"SOURCE_DATE_EPOCH\")) {\
  as.numeric(Sys.getenv(\"SOURCE_DATE_EPOCH\"))\
} else { " time "}, origin=\"1970-01-01\")\n")))

              ;; This library is installed using "install_package_description",
              ;; so we need to pass the "builtStamp" argument.
              (substitute* "src/library/tools/Makefile.in"
                (("(install_package_description\\(.*\"')\\)\"" line prefix)
                 (string-append prefix ", builtStamp='1970-01-01')\"")))

              (substitute* "src/library/Recommended/Makefile.in"
                (("INSTALL_OPTS =" m)
                 (string-append m " --built-timestamp=1970-01-01" m)))

              ;; R bundles an older version of help2man, which does not respect
              ;; SOURCE_DATE_EPOCH.  We cannot just use the latest help2man,
              ;; because that breaks a test.
              (with-fluids ((%default-port-encoding "ISO-8859-1"))
                (substitute* "tools/help2man.pl"
                  (("my \\$date = strftime \"%B %Y\", localtime" line)
                   (string-append line " 1"))))

              ;; The "References" section of this file when converted to
              ;; package.rds is sometimes stored with a newline, sometimes with
              ;; a space.  We avoid this problem by removing the line break
              ;; that is suspected to be the culprit.
              (substitute* "src/library/methods/DESCRIPTION.in"
                (("\\(2008\\)\n") "(2008) ")
                (("  ``Software") "``Software")
                (("Data Analysis:.") "Data Analysis:\n")
                (("Programming with R") "  Programming with R"))
              (substitute* "src/library/tools/DESCRIPTION.in"
                (("codetools, methods, xml2, curl, commonmark, knitr, xfun, mathjaxr")
                 "codetools, methods, xml2, curl, commonmark,
    knitr, xfun, mathjaxr"))))
          (add-before 'build 'set-locales
            (lambda _
              (setlocale LC_ALL "C")
              (setenv "LC_ALL" "C")))
          (add-before 'configure 'set-default-pager
            ;; Set default pager to "cat", because otherwise it is "false",
            ;; making "help()" print nothing at all.
            (lambda _ (setenv "PAGER" "cat")))
          (add-before 'configure 'set-timezone
            ;; Some tests require the timezone to be set.  However, the
            ;; timezone may not just be "UTC", or else a brittle regression
            ;; test in reg-tests-1d will fail.
            ;; We also need TZ during the configure step.
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "TZ" "UTC+1")
              (setenv "TZDIR"
                      (search-input-directory inputs
                                              "share/zoneinfo"))))
          (add-before 'check 'set-home
            ;; Some tests require that HOME be set.
            (lambda _ (setenv "HOME" "/tmp")))
          (add-after 'build 'make-info
            (lambda _ (invoke "make" "info")))
          (add-after 'build 'install-info
            (lambda _ (invoke "make" "install-info"))))
       #:configure-flags
       #~(list
          ;; We build the recommended packages here, because they are needed in
          ;; order to run the test suite.  We disable them in the r-minimal
          ;; package.
          "--with-cairo"
          "--with-blas=-lopenblas"
          "--with-libpng"
          "--with-jpeglib"
          "--with-libtiff"
          "--with-ICU"
          "--with-tcltk"
          (string-append "--with-tcl-config="
                         #$(this-package-input "tcl")
                         "/lib/tclConfig.sh")
          (string-append "--with-tk-config="
                         #$(this-package-input "tk")
                         "/lib/tkConfig.sh")
          "--enable-R-shlib"
          "--enable-BLAS-shlib"
          "--with-system-tre")))
    ;; R has some support for Java.  When the JDK is available at configure
    ;; time environment variables pointing to the JDK will be recorded under
    ;; $R_HOME/etc and ./tools/getsp.java will be compiled which is used by "R
    ;; CMD javareconf".  "R CMD javareconf" appears to only be used to update
    ;; the recorded environment variables in $R_HOME/etc.  Refer to
    ;; https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Java-support
    ;; for additional information.

    ;; As the JDK is a rather large input with only very limited effects on R,
    ;; we decided to drop it.
    (native-inputs
     (list bzip2
           perl
           pkg-config
           texinfo                      ; for building HTML manuals
           (texlive-updmap.cfg
            (list texlive-etoolbox
                  texlive-fancyvrb
                  texlive-inconsolata
                  texlive-upquote
                  texlive-xkeyval))
           tzdata-for-tests
           xz))
    (inputs
     (list coreutils
           curl
           openblas
           gfortran
           grep
           icu4c
           libdeflate
           libjpeg-turbo
           libpng
           libtiff
           libxt
           ;; We need not only cairo here, but pango to ensure that tests for the
           ;; "cairo" bitmapType plotting backend succeed.
           pango
           pcre2
           readline
           tcl
           tk
           which
           zlib
           ;; This avoids a reference to the ungraftable static bash.  R uses the
           ;; detected shell for the "system" procedure.
           bash-minimal))
    (native-search-paths
     (list (search-path-specification
            (variable "R_LIBS_SITE")
            (files (list "site-library/")))))
    (home-page "https://www.r-project.org/")
    (synopsis "Environment for statistical computing and graphics")
    (description
     "R is a language and environment for statistical computing and graphics.
It provides a variety of statistical techniques, such as linear and nonlinear
modeling, classical statistical tests, time-series analysis, classification
and clustering.  It also provides robust support for producing
publication-quality data plots.  A large amount of 3rd-party packages are
available, greatly increasing its breadth and scope.")
    (license license:gpl3+)))

(define-public r-minimal
  (package (inherit r-with-tests)
    (name "r-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments r-with-tests)
       ((#:tests? #f #f) #f)
       ((#:disallowed-references refs '())
        (cons perl refs))
       ((#:configure-flags flags)
        ;; Do not build the recommended packages.  The build system creates
        ;; random temporary directories and embeds their names in some
        ;; package files.  We build these packages with the r-build-system
        ;; instead.
        #~(cons "--without-recommended-packages" #$flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-extraneous-references
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (substitute* (string-append #$output "/lib/R/etc/Makeconf")
                  (("^# configure.*")
                   "# Removed to avoid extraneous references\n"))
                (substitute* (string-append #$output "/lib/R/bin/libtool")
                  (((string-append
                     "(-L)?("
                     (format #false
                             "~a/[^-]+-(~{~a~^|~})-[^/]+"
                             (%store-directory)
                             '("bzip2"
                               "file"
                               "glibc-utf8-locales"
                               "graphite2"
                               "libselinux"
                               "libsepol"
                               "perl"
                               "texinfo"
                               "texlive-bin"
                               "util-macros"
                               "xz"))
                     "|"
                     (format #false "~a/[^-]+-glibc-[^-]+-static"
                             (%store-directory))
                     ")/lib")) ""))))))))))

(define-public rmath-standalone
  (package (inherit r-minimal)
    (name "rmath-standalone")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-timezone
           ;; We need TZ during the configure step.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (search-input-directory inputs "share/zoneinfo"))))
         (add-after 'configure 'chdir
           (lambda _ (chdir "src/nmath/standalone/"))))))
    (synopsis "Standalone R math library")
    (description
     "This package provides the R math library as an independent package.")))

(define-public r
  (package (inherit r-minimal)
    (name "r")
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (begin (mkdir %output) #t)))
    (propagated-inputs
     (list r-minimal
           r-boot
           r-class
           r-cluster
           r-codetools
           r-foreign
           r-kernsmooth
           r-lattice
           r-mass
           r-matrix
           r-mgcv
           r-nlme
           r-nnet
           r-rpart
           r-spatial
           r-survival))))

(define-public r-knitr
  (package
    (name "r-knitr")
    (version "1.49")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "knitr" version))
              (sha256
               (base32
                "1lx2k2gg14pmpbh8i884mdaqx0zaf0x07x1001z3hja6cmwwali9"))))
    (properties
     ;; We can't have r-litedown among the inputs here to avoid a dependency
     ;; cycle.
     '((updater-ignored-native-inputs . ("r-litedown"))))
    (build-system r-build-system)
    ;; Tests need r-tibble, which needs r-knitr.
    (arguments (list #:tests? #false))
    (propagated-inputs
     (list r-evaluate r-highr r-xfun r-yaml))
    (home-page "https://yihui.org/knitr/")
    (synopsis "General-purpose package for dynamic report generation in R")
    (description
     "This package provides a general-purpose tool for dynamic report
generation in R using Literate Programming techniques.")
    ;; The code is released under any version of the GPL.  As it is used by
    ;; r-markdown which is available under GPLv2 only, we have chosen GPLv2+
    ;; here.
    (license license:gpl2+)))

(define-public r-knitrbootstrap
  (package
    (name "r-knitrbootstrap")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "knitrBootstrap" version))
       (sha256
        (base32
         "157mn2gm7djfiw1b55vdr2ylrmgxlpfg95bmm82ghv5g6vr26smd"))))
    (properties `((upstream-name . "knitrBootstrap")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-knitr r-markdown r-rmarkdown))
    (native-inputs (list r-knitr r-testthat))
    (home-page "https://github.com/jimhester/knitrBootstrap")
    (synopsis "Knitr bootstrap framework")
    (description
     "This package provides a framework to create Bootstrap 3 HTML reports
from knitr Rmarkdown.")
    (license license:expat)))

(define-public r-microbenchmark
  (package
    (name "r-microbenchmark")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "microbenchmark" version))
              (sha256
               (base32
                "0pvn94zfv50imz5g39wyqbqaa7fkl0h5sy98ic9as4b842lr47ix"))))
    (build-system r-build-system)
    (native-inputs (list r-runit))
    (home-page "https://cran.r-project.org/web/packages/microbenchmark/")
    (synopsis "Accurate timing functions for R")
    (description
     "This package provides infrastructure to accurately measure and compare
the execution time of R expressions.")
    (license license:bsd-2)))

(define-public r-pryr
  (package
    (name "r-pryr")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "pryr" version))
              (sha256
               (base32
                "013p2xxd51kr9ddx051cvn45mzgj44fm47nkchdb13l0885a7hb8"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-codetools r-lobstr r-rcpp r-stringr))
    (native-inputs (list r-testthat))
    (home-page "https://github.com/hadley/pryr")
    (synopsis "Tools for computing on the R language")
    (description
     "This package provides useful tools to pry back the covers of R and
understand the language at a deeper level.")
    (license license:gpl2)))

(define-public r-memoise
  (package
    (name "r-memoise")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "memoise" version))
              (sha256
               (base32
                "1srdzv2bp0splislrabmf1sfbqfi3hn189nq7kxhgjn8k3p38l7q"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cachem r-rlang))
    (native-inputs (list r-testthat))
    (home-page "https://github.com/hadley/memoise")
    (synopsis "Memoise functions for R")
    (description
     "This R package caches the results of a function so that when
you call it again with the same arguments it returns the pre-computed value.")
    (license license:expat)))

(define-public r-praise
  (package
    (name "r-praise")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "praise" version))
       (sha256
        (base32
         "1gfyypnvmih97p2r0php9qa39grzqpsdbq5g0fdsbpq5zms5w0sw"))))
    (properties
     ;; We can't have r-testthat among the inputs here to avoid a dependency
     ;; cycle.
     '((updater-ignored-native-inputs . ("r-testthat"))))
    (build-system r-build-system)
    ;; Tests require r-testthat, which depends on r-praise.
    (arguments (list #:tests? #false))
    (home-page "https://github.com/gaborcsardi/praise")
    (synopsis "Functions to praise users")
    (description
     "This package provides template functions to assist in building friendly
R packages that praise their users.")
    (license license:expat)))

(define-public r-testthat
  (package
    (name "r-testthat")
    (version "3.2.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "testthat" version))
              (sha256
               (base32
                "1by17pjy05xjc0clbp8kam2lrzy5w00fd0nqqp76l0dwbd57j2qq"))))
    (properties
     '((updater-ignored-native-inputs . ("r-testthat"))))
    (build-system r-build-system)
    ;; Some tests require r-xml2, which uses r-testthat.
    (arguments (list #:tests? #false))
    (propagated-inputs
     (list r-brio
           r-callr
           r-cli
           r-desc
           r-digest
           r-evaluate
           r-jsonlite
           r-lifecycle
           r-magrittr
           r-pkgload
           r-praise
           r-processx
           r-ps
           r-r6
           r-rlang
           r-waldo
           r-withr))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/hadley/testthat")
    (synopsis "Unit testing for R")
    (description
     "This package provides a unit testing system for R designed to be fun,
flexible and easy to set up.")
    (license license:expat)))

(define-public r-r6
  (package
    (name "r-r6")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R6" version))
              (sha256
               (base32
                "0j5z0b0myzjyyykk310xsa9n2mcm9bz8yqbq4xgz2yzdq8lvv4ld"))))
    (properties
     ;; We can't have r-testthat among the inputs here to avoid a dependency
     ;; cycle.
     '((updater-ignored-native-inputs . ("r-testthat"))))
    (build-system r-build-system)
    ;; Tests require r-testthat, which indirectly depends on this package.
    (arguments (list #:tests? #false))
    (home-page "https://github.com/wch/R6/")
    (synopsis "Classes with reference semantics in R")
    (description
     "The R6 package allows the creation of classes with reference semantics,
similar to R's built-in reference classes.  Compared to reference classes, R6
classes are simpler and lighter-weight, and they are not built on S4 classes
so they do not require the methods package.  These classes allow public and
private members, and they support inheritance, even when the classes are
defined in different packages.")
    (license license:expat)))

(define-public r-rlang
  (package
    (name "r-rlang")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "rlang" version))
              (sha256
               (base32
                "1v8wq2y3zsq47n6k74m49r1i4yrlx01cf0gmvy4xz08x9wj9pncz"))))
    (properties
     ;; We can't have r-testthat among the inputs here to avoid a dependency
     ;; cycle.
     '((updater-ignored-native-inputs . ("r-testthat"))))
    (build-system r-build-system)
    ;; Tests require r-testthat, which indirectly depends on this package.
    (arguments (list #:tests? #false))
    (home-page "http://rlang.tidyverse.org")
    (synopsis "Functions for base types, core R and Tidyverse features")
    (description "This package provides a toolbox for working with base types,
core R features like the condition system, and core @code{Tidyverse} features
like tidy evaluation.")
    (license license:gpl3)))

(define-public r-tibble
  (package
    (name "r-tibble")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tibble" version))
       (sha256
        (base32
         "0c916wl19wbhncv05hjzs2vmvvbcxlswjl6i232ygmkzal62v9v5"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-fansi
           r-lifecycle
           r-magrittr
           r-pillar
           r-pkgconfig
           r-rlang
           r-vctrs))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://github.com/hadley/tibble")
    (synopsis "Simple data frames")
    (description
     "This package provides a @code{tbl_df} class that offers better checking
and printing capabilities than traditional data frames.")
    (license license:expat)))

(define-public r-dplyr
  (package
    (name "r-dplyr")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "dplyr" version))
              (sha256
               (base32
                "1jsq8pj12bngy66xms486j8a65wxvyqs944q9rxkiaylsla08wyg"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'delete-failing-tests
           (lambda _
             ;; 2 tests fail because of unexpected but harmless warnings.
             (delete-file "tests/testthat/test-rows.R"))))))
    (propagated-inputs
     (list r-cli
           r-generics
           r-glue
           r-lifecycle
           r-magrittr
           r-pillar
           r-r6
           r-rlang
           r-tibble
           r-tidyselect
           r-vctrs))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://github.com/hadley/dplyr")
    (synopsis "Tools for working with data frames in R")
    (description
     "dplyr is the next iteration of plyr.  It is focused on tools for
working with data frames.  It has three main goals: 1) identify the most
important data manipulation tools needed for data analysis and make them easy
to use in R; 2) provide fast performance for in-memory data by writing key
pieces of code in C++; 3) use the same code interface to work with data no
matter where it is stored, whether in a data frame, a data table or
database.")
    (license license:expat)))

(define-public r-dbplyr
  (package
    (name "r-dbplyr")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dbplyr" version))
       (sha256
        (base32
         "1zxw4ignzm2fzixsf6n80f44b9q7434vamy2xj4v31wlx3dmnixv"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         ;; We need this for one failing test.
         (add-before 'check 'set-timezone
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (search-input-directory inputs
                                             "share/zoneinfo")))))))
    (propagated-inputs
     (list r-blob
           r-cli
           r-dbi
           r-dplyr
           r-glue
           r-lifecycle
           r-magrittr
           r-pillar
           r-purrr
           r-r6
           r-rlang
           r-tibble
           r-tidyr
           r-tidyselect
           r-vctrs
           r-withr))
    (native-inputs
     (list r-knitr r-rsqlite r-testthat tzdata-for-tests))
    (home-page "https://github.com/tidyverse/dbplyr")
    (synopsis "Dplyr back end for databases")
    (description
     "This package provides a dplyr back end for databases that allows you to
work with remote database tables as if they are in-memory data frames.  Basic
features works with any database that has a @code{DBI} back end; more advanced
features require SQL translation to be provided by the package author.")
    (license license:expat)))

(define-public r-locfit
  (package
    (name "r-locfit")
    (version "1.5-9.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "locfit" version))
       (sha256
        (base32
         "1fg3qha36hvmibfrin1rndarr5zk0v55nj0q5h5inpqkzdl4p5ls"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice))
    (home-page "https://cran.r-project.org/web/packages/locfit")
    (synopsis "Local regression, likelihood and density estimation")
    (description
     "This package provides functions used for local regression, likelihood
and density estimation.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-chron
  (package
    (name "r-chron")
    (version "2.3-62")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "chron" version))
              (sha256
               (base32
                "18dyy4fn48d4m7azqbqpazhvy9mwl1k494zl7gxckfrx4ivjrb87"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/chron")
    (synopsis "Chronological R objects which can handle dates and times")
    (description
     "This package provides chronological R objects which can handle dates and
times.")
    (license license:gpl2)))

(define-public r-data-table
  (package
    (name "r-data-table")
    (version "1.16.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "data.table" version))
              (sha256
               (base32
                "1pww4qq7zsvq1k9gdrx1wf0p190ic4mv7kr30bk1s4dwmsrlnxkj"))))
    (properties
     `((upstream-name . "data.table")
       (updater-extra-native-inputs . ("tzdata-for-tests"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         ;; These two phases are needed for 3 otherwise failing tests
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp")))
         (add-before 'check 'set-timezone
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (search-input-directory inputs "share/zoneinfo")))))))
    (inputs
     (list zlib))
    (native-inputs
     (list pkg-config r-knitr tzdata-for-tests))
    (home-page "https://github.com/Rdatatable/data.table/wiki")
    (synopsis "Enhanced version of data.frame R object")
    (description
     "The R package @code{data.table} is an extension of @code{data.frame}
providing functions for fast aggregation of large data (e.g. 100GB in RAM),
fast ordered joins, fast add/modify/delete of columns by group, column listing
and fast file reading.")
    (license license:gpl3+)))

(define-public python-vega-datasets
  (package
    (name "python-vega-datasets")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vega_datasets" version))
       (sha256
        (base32 "1h1zv607mars2j73v8fdwihjh479blqxyw29nhmc73lf40s9iglx"))
       (modules '((guix build utils)))
       (patches
        (search-patches "python-vega-datasets-remove-la-riots-code.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-la-riots-dataset
                 ;; Remove dataset with unclear license.
                 (lambda _
                   (delete-file "vega_datasets/_data/la-riots.csv"))))))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-pandas))
    (home-page "https://github.com/altair-viz/vega_datasets")
    (synopsis "Example datasets used by Vega-related projects")
    (description "This package provides a collection of datasets used in Vega
and Vega-Lite examples.")
    (license license:expat)))

(define-public python-altair
  (package
    (name "python-altair")
    (version "5.3.0")
    (source (origin
              (method git-fetch)        ; no tests in PyPI
              (uri (git-reference
                    (url "https://github.com/altair-viz/altair")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lx3pkphi36pljns6jjxhyn9fbrana8f1y6gcg4yca48nvwlfssl"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           ;; XXX: This test file requires hard to package python-anywidgets.
           #~(list "--ignore=tests/test_jupyter_chart.py"
                   "-k" (string-join
                         (list
                          ;; these tests open an external connection.
                          "not test_from_and_to_json_roundtrip"
                          "test_render_examples_to_chart"
                          ;; introduces a circular dependency on altair-viewer.
                          "not test_save_html"
                          ;; these tests require the vl-convert vega compiler
                          "test_vegalite_compiler"
                          "test_to_dict_with_format_vega"
                          "test_to_json_with_format_vega"
                          "test_to_url"
                          "test_renderer_with_none_embed_options"
                          "test_jupyter_renderer_mimetype")
                         " and not "))))
    (propagated-inputs (list python-jinja2
                             python-jsonschema
                             python-numpy
                             python-pandas
                             python-setuptools
                             python-toolz
                             python-typing-extensions))
    (native-inputs (list python-black
                         python-hatchling
                         python-ipython
                         python-pytest
                         python-vega-datasets))
    (home-page "https://altair-viz.github.io/")
    (synopsis "Declarative statistical visualization library for Python")
    (description
     "Vega-Altair is a declarative statistical visualization library for Python.")
    (license license:expat)))

(define-public python-george
  (package
    (name "python-george")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "george" version))
       (sha256
        (base32 "1zvbdq50ds820aj06lr2nrzwg121bkd9bg0aq83gvk7lf8yqgp4v"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list cmake-minimal
           pybind11
           python-pytest
           python-scikit-build-core
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy
           python-scipy))
    (home-page "https://george.readthedocs.io")
    (synopsis "Fast Gaussian Processes for regression")
    (description
     "George is a fast and flexible Python library for Gaussian Process (GP)
Regression, focused on efficiently evaluating the marginalized likelihood of a
dataset under a GP prior, even as this dataset gets Big.")
    (license license:expat)))

(define-public python-getdist
  (package
    (name "python-getdist")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "getdist" version))
       (sha256
        (base32 "01s1p53pqpxbi8sy2030jpjn7gsy67zb7y6p0gf57lgxvp4zx74q"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-packaging
           python-pyyaml
           python-scipy))
    (home-page "https://github.com/cmbant/getdist")
    (synopsis "GetDist Monte Carlo sample analysis, plotting and GUI")
    (description
     "GetDist is a Python package for analysing Monte Carlo samples, including
correlated samples from Markov Chain Monte Carlo (MCMC).")
    (license license:lgpl3)))

(define-public python-hdmedians
  (package
    (name "python-hdmedians")
    (version "0.14.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hdmedians" version))
              (sha256
               (base32
                "1mn2k8srnmfy451l7zvb2l4hn9701bc5awjm6q3vmqbicyqyqyml"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'build-extensions
           (lambda _
             ;; Cython extensions have to be built before running the tests.
             (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs (list python-cython python-numpy python-setuptools
                             python-wheel))
    (native-inputs (list python-nose))
    (home-page "http://github.com/daleroberts/hdmedians")
    (synopsis "High-dimensional medians")
    (description "Various definitions for a high-dimensional median exist and
this Python package provides a number of fast implementations of these
definitions.  Medians are extremely useful due to their high breakdown
point (up to 50% contamination) and have a number of nice applications in
machine learning, computer vision, and high-dimensional statistics.")
    (license license:asl2.0)))

(define-public python-nestle
  (package
    (name "python-nestle")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nestle" version))
       (sha256
        (base32 "0v94qcqwl519vqhd4wb1zhx4x4q9xhbck8g2h0v2n4mwxgz9irsx"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests in PyPI or Git
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/kbarbary/nestle")
    (synopsis "Nested sampling algorithms for evaluating Bayesian evidence")
    (description
     "This package provides an implementation of
@url{https://en.wikipedia.org/wiki/Nested_sampling_algorithm, Nested Sampling}
algorithms for evaluating Bayesian evidence.")
    (license license:expat)))

(define-public python-arviz
  (package
    (name "python-arviz")
    (version "0.21.0")
    (source (origin
              (method git-fetch)        ; PyPI misses some test files
              (uri (git-reference
                    (url "https://github.com/arviz-devs/arviz")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02bqpl61gzn65vhwspi6gx9ln2wlwh8xm418i8vhmls44rvszcxf"))))
    (build-system pyproject-build-system)
    (arguments
     ;; FIXME: matplotlib tests fail because of the "--save" test flag.
     (list #:test-flags #~'("--ignore"
                            "arviz/tests/base_tests/test_plots_matplotlib.py")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-radon
                 (lambda _
                   (delete-file
                    ;; This dataset is loaded remotely, it's not supposed to
                    ;; be copied locally.
                    "arviz/data/example_data/code/radon/radon.json")))
               (add-before 'check 'write-permission
                 (lambda _
                   ;; 3 tests require write permission.
                   (setenv "HOME" "/tmp"))))))
    (native-inputs (list python-cloudpickle python-pytest))
    (propagated-inputs (list python-dm-tree
                             python-h5netcdf
                             python-matplotlib
                             python-numpy
                             python-packaging
                             python-pandas
                             python-scipy
                             python-typing-extensions
                             python-xarray
                             python-xarray-einstats
                             python-setuptools
                             python-wheel))
    (home-page "https://github.com/arviz-devs/arviz")
    (synopsis "Exploratory analysis of Bayesian models")
    (description
     "ArviZ is a Python package for exploratory analysis of Bayesian models.
It includes functions for posterior analysis, data storage, model checking,
comparison and diagnostics.")
    (license license:asl2.0)))

(define-public python-pymc
  (package
    (name "python-pymc")
    (version "5.11.0")
    (source (origin
              (method git-fetch)        ; no tests in PyPI
              (uri (git-reference
                    (url "https://github.com/pymc-devs/pymc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x94qzq3z02fxlliz1xfdpb2pbn7nhp4skzcxz6qdavbj9xqcxys"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f ; tests are too computationally intensive
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'versioneer
                          (lambda _
                            (with-output-to-file "setup.cfg"
                (lambda ()
                  (display "\
[versioneer]
VCS = git
style = pep440
versionfile_source = pymc/_version.py
versionfile_build = pymc/_version.py
tag_prefix =
parentdir_prefix = pymc-
")))
              (invoke "versioneer" "install")
              (substitute* "setup.py"
                (("versioneer.get_version\\(\\)")
                 (string-append "\"" #$version "\"")))))
                        ;; To create the compiledir for tests.
                        (add-before 'check 'write-permissions
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME" "/tmp")))))))
    (native-inputs (list python-pytest-cov python-versioneer))
    (propagated-inputs (list python-arviz
                             python-cachetools
                             python-cloudpickle
                             python-fastprogress
                             python-numpy
                             python-pandas
                             python-pytensor
                             python-scipy
                             python-typing-extensions))
    (home-page "https://github.com/pymc-devs/pymc")
    (synopsis "Library for probabilistic programming in Python")
    (description
     "PyMC (formerly PyMC3) is a Python package for Bayesian statistical
modeling focusing on advanced Markov chain Monte Carlo (MCMC) and variational
inference (VI) algorithms.")
    (license license:asl2.0)))

(define-public python-chaospy
  (package
    (name "python-chaospy")
    (version "4.3.13")
    (source (origin ;; PyPI misses Pytest fixtures.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jonathf/chaospy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bn4jmwygs5h0dskbniivj20qblgm75pyi9hcjf47r25kawd730m"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; The deprecation warnings break the tests.
          (add-after 'unpack 'dont-treat-deprecation-warnings-as-error
            (lambda _
              (substitute* "pyproject.toml"
                (("\"error::DeprecationWarning\",") "")))))))
    (propagated-inputs (list python-importlib-metadata python-numpoly
                             python-numpy python-scipy))
    (native-inputs (list python-pytest python-scikit-learn python-setuptools
                         python-wheel))
    (home-page "https://chaospy.readthedocs.io/en/master/")
    (synopsis "Numerical tool for performing uncertainty quantification")
    (description "Chaospy is a numerical toolbox for performing uncertainty
quantification using polynomial chaos expansions, advanced Monte Carlo
methods implemented in Python.  It also include a full suite of tools for
doing low-discrepancy sampling, quadrature creation, polynomial manipulations,
and a lot more.")
    (license license:expat)))

(define-public python-patsy
  (package
    (name "python-patsy")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "patsy" version))
              (sha256
               (base32
                "17dn72519gvwifw3i8mzwlslxmxkl8ihzfrxg1iblsk70iwdwlsh"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests? (invoke "pytest" "-vv")))))))
    (propagated-inputs
     (list python-numpy python-scipy python-six))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/pydata/patsy")
    (synopsis "Describe statistical models and build design matrices")
    (description
     "Patsy is a Python package for describing statistical models and for
building design matrices.")
    ;; The majority of the code is distributed under BSD-2.  The module
    ;; patsy.compat contains code derived from the Python standard library,
    ;; and is covered by the PSFL.
    (license (list license:bsd-2 license:psfl))))

(define-public python-kalepy
  (package
    (name "python-kalepy")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kalepy" version))
       (sha256
        (base32 "1a1d98vjkjs8zwx4hdss3gv67jyf25mmsrdc5qi8hpxminkizb6w"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-matplotlib
           python-numba
           python-numpy
           python-scipy
           python-six))
    (home-page "https://github.com/lzkelley/kalepy")
    (synopsis "Kernel Density Estimation (KDE) and sampling")
    (description
     "This package performs KDE operations on multidimensional data to
calculate estimated PDFs (probability distribution functions), and resample
new data from those PDFs.")
    ;; MIT   - setup.py
    ;; GPL3+ - LICENSE
    (license (list license:gpl3+ license:expat))))

(define-public python-lifelines
  (package
    (name "python-lifelines")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lifelines" version))
       (sha256
        (base32 "065yajlfydi7x7b1sjxp9h3rqgwrd3w9ivxiyph7y5nbbwkzdxpp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; NOTE: Tests take 15-25min to complete on 16 threads and much longer
      ;; in single one, consider to try enabling --numprocesses option.
      #~(list ;; "--numprocesses" (number->string (parallel-job-count))
              ;; This accuracy test fails because 0.012 is not < 0.01.
              "-k" "not test_weibull_with_delayed_entries")))
    (native-inputs
     (list python-dill
           python-flaky
           python-joblib
           python-pytest
           ;; python-pytest-xdist
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-autograd
           python-autograd-gamma
           python-formulaic
           python-matplotlib
           python-numpy
           python-pandas
           python-scipy))
    (home-page "https://github.com/CamDavidsonPilon/lifelines")
    (synopsis
     "Survival analysis including Kaplan Meier, Nelson Aalen and regression")
    (description
     "This package enables survival analysis in Python, including Kaplan
Meier, Nelson Aalen and regression.")
    (license license:expat)))

(define-public python-mapie
  (package
    (name "python-mapie")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "MAPIE" version))
              (sha256
               (base32
                "00qhgfrix5aq7ng1xpvz2gk0d2d2maidbbd8ic9psq1vdqs6vp2a"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pandas python-pytest python-setuptools
                         python-wheel))
    (propagated-inputs (list python-numpy python-scikit-learn))
    (home-page "https://github.com/scikit-learn-contrib/MAPIE")
    (synopsis "Module for estimating prediction intervals")
    (description "MAPIE allows you to easily estimate prediction intervals
(or prediction sets) using your favourite scikit-learn-compatible model for
single-output regression or multi-class classification settings.

Prediction intervals output by MAPIE encompass both aleatoric and epistemic
uncertainties and are backed by strong theoretical guarantees thanks to
conformal prediction methods intervals.")
    (license license:bsd-3)))

(define-public python-emcee
  (package
    (name "python-emcee")
    (version "3.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "emcee" version))
       (sha256
        (base32 "0zb3ihja3hyj6zjbhkpxhyyppnv58q2jjg0yd1lwmydqdaplvbqi"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-coverage
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://emcee.readthedocs.io/en/stable/")
    (synopsis "Ensemble sampling toolkit for MCMC")
    (description
     "@code{emcee} is a Python implementation of the affine-invariant ensemble
sampler for Markov chain Monte Carlo (MCMC).")
    (license license:expat)))

(define-public python-statsmodels
  (package
    (name "python-statsmodels")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statsmodels" version))
       (sha256
        (base32 "1927ysv7m46m1x3wz05i0s3r5x0nasmidf2yy54djrp9i7bcfxb8"))
       (modules '((guix build utils)))
       (snippet
        '(for-each delete-file (find-files "." "\\.c$")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; The test suite is very large and rather brittle.  Tests often fail
      ;; because of minor changes in dependencies that upstream hasn't fixed
      ;; in a new release.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-matplotlib-backend-to-agg
            (lambda _
              ;; Set the matplotlib backend to Agg to avoid problems using the
              ;; GTK backend without a display.
              (substitute* (append (find-files "statsmodels/graphics/tests" "\\.py")
                                   '("statsmodels/tsa/vector_ar/tests/test_var.py"
                                     "statsmodels/duration/tests/test_survfunc.py"))
                (("import matplotlib\\.pyplot as plt" line)
                 (string-append "import matplotlib;matplotlib.use('Agg');"
                                line))))))))
    (propagated-inputs
     (list python-numpy python-packaging python-pandas python-patsy
           python-scipy))
    (native-inputs
     (list python-colorama
           python-cython
           python-flake8
           python-isort
           python-joblib
           python-matplotlib
           python-pytest
           python-pytest-randomly
           python-pytest-xdist
           python-setuptools-scm
           python-setuptools
           python-wheel))
    (home-page
     (string-append "https://www.statsmodels.org/v" version "/"))
    (synopsis "Statistical modeling and econometrics in Python")
    (description
     "Statsmodels is a Python package that provides a complement to scipy for
statistical computations including descriptive statistics and estimation and
inference for statistical models.")
    (license license:bsd-3)))

(define-public python-openturns
  (package
    (name "python-openturns")
    (version "1.24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openturns/openturns")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k7vgmlg5dybrbn61nzlsyx2142byi9jv357zv7mzf6b4y133k7k"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          ;; This is a Python package which is fully managed by CMake.  In
          ;; cmake-build-system the check phase runs before install, but the
          ;; Python modules required for testing are only installed in the
          ;; install phase.  Move check to after the install phase.
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ctest" "--exclude-regex"
                        ;; XXX: Cpp tests fail in 'No such file or directory',
                        ;; skip for now and only run the Python tests.
                        ;; TODO: To pass the Python tests below, Ipopt must be
                        ;; built with MUMPS support, but simply adding mumps
                        ;; to the inputs doesn't work for it to be found,
                        ;; possibly because MUMPS doesn't generate a .pc file.
                        (string-join
                         (list "^cpp"
                               "pyinstallcheck_Bonmin_std"
                               "pyinstallcheck_Bonmin_4dsoo"
                               "pyinstallcheck_Bonmin_MIT15"
                               "pyinstallcheck_Bonmin_swiler2014"
                               "pyinstallcheck_Ipopt_std"
                               "pyinstallcheck_example_plot_optimization_bonmin"
                               "pyinstallcheck_coupling_tools"
                               ;; Subprocess aborted for these tests.
                               "pyinstallcheck_Study_std"
                               "pyinstallcheck_OptimizationAlgorithm_std"
                               "pyinstallcheck_docstring_missing")
                         "|"))))))))
    (native-inputs
     (list bison
           dvisvgm
           flex
           python-numpydoc
           python-sphinx
           ;; python-sphinx-gallery ;; Currently broken
           swig))
    (inputs
     (list openblas                ; the only required dependency
           ;; The dependecies below are all optional.
           bonmin
           boost
           cbc ;; Maybe this should be propagated by Bonmin?
           ceres
           cminpack
           dlib
           hdf5
           hmat
           ipopt
           libxml2
           mpc
           mpfr
           nlopt
           pagmo
           primesieve
           python-wrapper
           spectra
           tbb))
    (propagated-inputs
     (list python-chaospy
           python-dill
           python-matplotlib
           python-numpy
           python-pandas
           python-scipy))
    (home-page "https://openturns.github.io/www/")
    (synopsis "Uncertainty treatment library")
    (description
     "OpenTURNS is a scientific C++ and Python library including an internal
data model and algorithms dedicated to the treatment of uncertainties.  The
main goal of this library is giving to specific applications all the
functionalities needed to treat uncertainties in studies.")
    (license license:lgpl3+)))

(define-public r-multitaper
  (package
    (name "r-multitaper")
    (version "1.0-17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "multitaper" version))
       (sha256
        (base32
         "1wqa2whcjxwjqg9z46i48dvrga17fdj63r05kg993r1fpriclc1l"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
    (home-page "https://github.com/wesleyburr/multitaper/")
    (synopsis "Multitaper spectral analysis tools")
    (description
     "This package implements multitaper spectral estimation
techniques using prolate spheroidal sequences (Slepians) and sine
tapers for time series analysis.  It includes an adaptive weighted
multitaper spectral estimate, a coherence estimate, Thomson's Harmonic
F-test, and complex demodulation.  The Slepians sequences are
generated efficiently using a tridiagonal matrix solution, and
jackknifed confidence intervals are available for most estimates.")
    (license license:gpl2+)))

(define-public r-rversions
  (package
    (name "r-rversions")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "rversions" version))
              (sha256
               (base32
                "0q5ip3rkhcxz7472fbqddrw3a2wm31b18w7ax0pi6wc27qiihn6y"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-curl r-xml2))
    (native-inputs (list r-testthat))
    (home-page "https://github.com/metacran/rversions")
    (synopsis "Query R versions, including 'r-release' and 'r-oldrel'")
    (description
     "This package provides functions to query the main R repository to find
the versions that @code{r-release} and @code{r-oldrel} refer to, and also all
previous R versions and their release dates.")
    (license license:expat)))

(define-public r-whisker
  (package
    (name "r-whisker")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "whisker" version))
              (sha256
               (base32
                "1g7jy6dnc5igs7yqy1n7cmy0ia8dm2yi3qj1mil2y0q88m4m2ldz"))))
    (build-system r-build-system)
    ;; Fails with: could not find function "whisker.render"
    (arguments (list #:tests? #false))
    (home-page "https://github.com/edwindj/whisker")
    (synopsis "Logicless mustache templating for R")
    (description
     "This package provides logicless templating, with a syntax that is not
limited to R.")
    (license license:gpl3+)))

(define-public r-checkmate
  (package
    (name "r-checkmate")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "checkmate" version))
       (sha256
        (base32
         "1qw81bs3lhwr1m6wlbs0l0j669051n60x4ca2821599ddhnp6mbj"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-backports))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://github.com/mllg/checkmate")
    (synopsis "Fast and versatile argument checks")
    (description
     "This package provides tests and assertions to perform frequent argument
checks.  A substantial part of the package was written in C to minimize any
worries about execution time overhead.")
    (license license:bsd-3)))

(define-public r-fail
  (package
    (name "r-fail")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fail" version))
       (sha256
        (base32
         "0vfm6kmpmgsamda5p0sl771kbnsscan31l2chzssyw93kwmams7d"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bbmisc r-checkmate))
    (native-inputs (list r-testthat))
    (home-page "https://github.com/mllg/fail")
    (synopsis "File abstraction interface layer (FAIL)")
    (description
     "This package provides a more comfortable interface to work with R data
or source files in a key-value fashion.")
    (license license:bsd-3)))

(define-public r-desc
  (package
    (name "r-desc")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "desc" version))
       (sha256
        (base32
         "09ig14bm68cps6d1mrmj6hhjr01i6gz1ri35qpkwk3yp7nkqsijl"))))
    (properties
     ;; We can't have r-testthat among the inputs here to avoid a dependency
     ;; cycle.
     '((updater-ignored-native-inputs . ("r-testthat"))))
    (build-system r-build-system)
    (arguments (list #:tests? #false))
    (propagated-inputs
     (list r-cli r-r6))
    (home-page "https://github.com/r-pkgs/desc")
    (synopsis "Manipulate DESCRIPTION Files")
    (description
     "This package provides tools to read, write, create, and manipulate
DESCRIPTION files.  It is intended for packages that create or manipulate
other packages.")
    (license license:expat)))

(define-public r-commonmark
  (package
    (name "r-commonmark")
    (version "1.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "commonmark" version))
       (sha256
        (base32
         "1sw6nizfbb88358r6mafiqiqdwd49cvf507g5cqggf72x3xxqshx"))))
    (properties
     ;; We can't have r-testthat among the inputs here to avoid a dependency
     ;; cycle.
     '((updater-ignored-native-inputs . ("r-testthat"))))
    (build-system r-build-system)
    (arguments (list #:tests? #false))
    (home-page "https://cran.r-project.org/web/packages/commonmark")
    (synopsis "CommonMark and Github Markdown Rendering in R")
    (description
     "The CommonMark specification defines a rationalized version of markdown
syntax.  This package uses the @code{cmark} reference implementation for
converting markdown text into various formats including HTML, LaTeX and groff
man.  In addition, it exposes the markdown parse tree in XML format.  The latest
version of this package also adds support for Github extensions including
tables, autolinks and strikethrough text.")
    (license license:bsd-2)))

(define-public r-roxygen2
  (package
    (name "r-roxygen2")
    (version "7.3.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "roxygen2" version))
              (sha256
               (base32
                "0701v8dglv0mdsjs1mijpylpciiijb2schplavkf5drjj6gqg25p"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-brew
           r-cli
           r-commonmark
           r-cpp11
           r-desc
           r-knitr
           r-pkgload
           r-purrr
           r-r6
           r-rlang
           r-stringi
           r-stringr
           r-withr
           r-xml2))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://github.com/klutometis/roxygen")
    (synopsis "In-source documentation system for R")
    (description
     "Roxygen2 is a Doxygen-like in-source documentation system for Rd,
collation, and NAMESPACE files.")
    (license license:gpl2+)))

(define-public r-openssl
  (package
    (name "r-openssl")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "openssl" version))
       (sha256
        (base32
         "1nw4clg9skm6fnaar72zp00wrk29bjhmh9714a9q0m9wpimcl9cr"))))
    (properties
     `((upstream-name . "openssl")
       (updater-extra-inputs . ("openssl"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-versioned-linking
           (lambda _
             (substitute* "configure"
               (("PKG_LIBS=\"\\$\\{PKG_LIBS_VERSIONED\\}\"")
                "PKG_LIBS=\"${PKG_LIBS}\"")))))))
    (inputs
     (list openssl))
    (native-inputs
     (list pkg-config r-curl r-knitr r-sodium r-testthat))
    (propagated-inputs
     (list r-askpass))
    (home-page "https://github.com/jeroenooms/openssl")
    (synopsis "Toolkit for encryption, signatures and certificates")
    (description
     "This package provides R bindings to OpenSSL libssl and libcrypto, plus
custom SSH pubkey parsers.  It supports RSA, DSA and NIST curves P-256, P-384
and P-521.  Cryptographic signatures can either be created and verified
manually or via x509 certificates.  AES block cipher is used in CBC mode for
symmetric encryption; RSA for asymmetric (public key) encryption.  High-level
envelope functions combine RSA and AES for encrypting arbitrary sized data.
Other utilities include key generators, hash functions (md5, sha1, sha256,
etc), base64 encoder, a secure random number generator, and @code{bignum} math
methods for manually performing crypto calculations on large multibyte
integers.")
    (license license:expat)))

(define-public r-httr
  (package
    (name "r-httr")
    (version "1.4.7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "httr" version))
              (sha256
               (base32
                "0fisvq3zydmqagn7lj6x98zxkcl7l95gfydl27zkigb7zg1fcm8m"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-curl r-jsonlite r-mime r-openssl r-r6))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://github.com/hadley/httr")
    (synopsis "Tools for working with URLs and HTTP")
    (description
     "The aim of httr is to provide a wrapper for RCurl customised to the
demands of modern web APIs.  It provides useful tools for working with HTTP
organised by HTTP verbs (@code{GET()}, @code{POST()}, etc).  Configuration
functions make it easy to control additional request components.")
    (license license:expat)))

(define-public r-git2r
  (package
    (name "r-git2r")
    (version "0.35.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "git2r" version))
              (sha256
               (base32
                "0i76pf7nfdf989fwdhx233kp3qqimqqjhdj4r4jx6yx1bklg06i9"))))
    (build-system r-build-system)
    (inputs
     (list libgit2 zlib))
    (native-inputs
     (list pkg-config tzdata-for-tests))
    (home-page "https://github.com/ropensci/git2r")
    (synopsis "Access Git repositories with R")
    (description
     "This package provides an R interface to the libgit2 library, which is a
pure C implementation of the Git core methods.")
    ;; GPLv2 only with linking exception.
    (license license:gpl2)))

(define-public r-rstudioapi
  (package
    (name "r-rstudioapi")
    (version "0.17.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "rstudioapi" version))
              (sha256
               (base32
                "029rbfdw9b57mcnx8yblgrwd79v47ky068mr3w9p61lnr21zjzk1"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://cran.r-project.org/web/packages/rstudioapi")
    (synopsis "Safely access the RStudio API")
    (description
     "This package provides functions to access the RStudio API and provide
informative error messages when it's not available.")
    (license license:expat)))

(define-public r-devtools
  (package
    (name "r-devtools")
    (version "2.4.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "devtools" version))
              (sha256
               (base32
                "0jwh14clyj5flygpmifk01avs7mbnj2s1686pxzfrkcshfyhw5iq"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cli
           r-desc
           r-ellipsis
           r-fs
           r-lifecycle
           r-memoise
           r-miniui
           r-pkgbuild
           r-pkgdown
           r-pkgload
           r-profvis
           r-rcmdcheck
           r-remotes
           r-rlang
           r-roxygen2
           r-rversions
           r-sessioninfo
           r-testthat
           r-urlchecker
           r-usethis
           r-withr))
    (native-inputs
     (list r-knitr r-mockery r-testthat))
    (home-page "https://github.com/hadley/devtools")
    (synopsis "Tools to make developing R packages easier")
    (description "The devtools package is a collection of package development
tools to simplify the devolpment of R packages.")
    (license license:gpl2+)))

(define-public r-withr
  (package
    (name "r-withr")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "withr" version))
              (sha256
               (base32
                "18y1jbyf6b70zm0kyjdjhdzyskhsbaww3j0kpyjcqxfjjgs0afha"))))
    (build-system r-build-system)
    ;; Tests require r-testthat, which indirectly depends on this package.
    (arguments (list #:tests? #false))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/jimhester/withr")
    (synopsis "Run code with temporarily modified global state")
    (description
     "This package provides a set of functions to run R code in an environment
in which global state has been temporarily modified.  Many of these functions
were originally a part of the r-devtools package.")
    (license license:gpl2+)))

(define-public r-hms
  (package
    (name "r-hms")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hms" version))
       (sha256
        (base32
         "10h2k5j97fggq3hc0qzxv1q9821y21m326v3x99zsvpl1b3g89p6"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lifecycle r-pkgconfig r-rlang r-vctrs))
    (native-inputs (list r-testthat))
    (home-page "https://github.com/rstats-db/hms")
    (synopsis "Pretty time of day")
    (description
     "This package implements an S3 class for storing and formatting
time-of-day values, based on the @code{difftime} class.")
    (license license:gpl3+)))

(define-public r-readr
  (package
    (name "r-readr")
    (version "2.1.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "readr" version))
              (sha256
               (base32
                "02p1jjal73j39r49ba4jlvbx8bdqmm96nsdp47igyv54w1gmm9hg"))))
    (build-system r-build-system)
    (properties
     '((updater-extra-native-inputs . ("r-stringi"))))
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'set-timezone
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Two tests would fail without this.
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (search-input-directory inputs
                                             "share/zoneinfo")))))))
    (propagated-inputs
     (list r-cli
           r-clipr
           r-cpp11
           r-crayon
           r-hms
           r-lifecycle
           r-r6
           r-rlang
           r-tibble
           r-tzdb
           r-vroom))
    (native-inputs
     (list r-knitr r-stringi r-testthat tzdata-for-tests))
    (home-page "https://github.com/hadley/readr")
    (synopsis "Read tabular data")
    (description
     "This package provides functions to read flat or tabular text files from
disk (or a connection).")
    (license license:gpl2+)))

(define-public r-plotrix
  (package
    (name "r-plotrix")
    (version "3.8-4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "plotrix" version))
              (sha256
               (base32
                "1mp8mb79prgf5fzlaix4fvslr4q67hrzxqdv3kr7mik1mf9jv8p6"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/plotrix")
    (synopsis "Various plotting functions")
    (description
     "This package provides lots of plotting, various labeling, axis and color
scaling functions for R.")
    (license license:gpl2+)))

(define-public r-gridbase
  (package
    (name "r-gridbase")
    (version "0.4-7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "gridBase" version))
              (sha256
               (base32
                "09jzw4rzwf2y5lcz7b16mb68pn0fqigv34ff7lr6w3yi9k91i1xy"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/gridBase")
    (synopsis "Integration of base and grid graphics")
    (description
     "This package provides an integration of base and grid graphics for R.")
    (license license:gpl2+)))

(define-public r-latticeextra
  (package
    (name "r-latticeextra")
    (version "0.6-30")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "latticeExtra" version))
       (sha256
        (base32
         "1vcy4xr6c53nbvnxikjnlf5kd3n5hf4d8dzj5d41hj322dlsfl65"))))
    (properties `((upstream-name . "latticeExtra")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-interp
           r-jpeg
           r-lattice
           r-mass
           r-png
           r-rcolorbrewer))
    (home-page "https://latticeextra.r-forge.r-project.org/")
    (synopsis "Extra graphical utilities based on lattice")
    (description
     "Building on the infrastructure provided by the lattice package, this
package provides several new high-level graphics functions and methods, as
well as additional utilities such as panel and axis annotation functions.")
    (license license:gpl2+)))

(define-public r-rcpparmadillo
  (package
    (name "r-rcpparmadillo")
    (version "14.2.3-1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RcppArmadillo" version))
              (sha256
               (base32
                "1pr6a84h6c415ac43fadv77k3dazi8138q92gn7mj3qw7v2wn74k"))))
    (properties `((upstream-name . "RcppArmadillo")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp))
    (home-page "https://github.com/RcppCore/RcppArmadillo")
    (synopsis "Rcpp integration for the Armadillo linear algebra library")
    (description
     "Armadillo is a templated C++ linear algebra library that aims towards a
good balance between speed and ease of use.  Integer, floating point and
complex numbers are supported, as well as a subset of trigonometric and
statistics functions.  Various matrix decompositions are provided through
optional integration with LAPACK and ATLAS libraries.  This package includes
the header files from the templated Armadillo library.")
    ;; Armadillo is licensed under the MPL 2.0, while RcppArmadillo (the Rcpp
    ;; bindings to Armadillo) is licensed under the GNU GPL version 2 or
    ;; later, as is the rest of 'Rcpp'.
    (license license:gpl2+)))

(define-public r-catools
  (package
    (name "r-catools")
    (version "1.18.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "caTools" version))
              (sha256
               (base32
                "1n380hnddj29nrzsqf462a1h3xy55qk4q45i5287ffd2lx13j6z7"))))
    (properties `((upstream-name . "caTools")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bitops))
    (home-page "https://cran.r-project.org/web/packages/caTools")
    (synopsis "Various tools including functions for moving window statistics")
    (description
     "This package contains several basic utility functions including:
moving (rolling, running) window statistic functions, read/write for GIF and
ENVI binary files, fast calculation of AUC, LogitBoost classifier, base64
encoder/decoder, round-off-error-free sum and cumsum, etc.")
    (license license:gpl3+)))

(define-public r-rprojroot
  (package
    (name "r-rprojroot")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rprojroot" version))
       (sha256
        (base32
         "16bf6ga5fgm83j3m67plw5i54az2vdbvw5m99ixaqkd24pxn7x5m"))))
    (build-system r-build-system)
    ;; Tests require r-testthat, which uses this package.
    (arguments (list #:tests? #false))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/krlmlr/rprojroot")
    (synopsis "Finding files in project subdirectories")
    (description
     "This package helps accessing files relative to a project root.  It
provides helpers for robust, reliable and flexible paths to files below a
project root.  The root of a project is defined as a directory that matches a
certain criterion, e.g., it contains a certain regular file.")
    (license license:gpl3)))

(define-public r-rmarkdown
  (package
    (name "r-rmarkdown")
    (version "2.29")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "rmarkdown" version))
        (sha256
          (base32 "0a0995dc0h4dvdkb0ivd23pnwzyrr9l59fg3ssm9r1kc662sqqk6"))))
    (properties
     `((upstream-name . "rmarkdown")
       (updater-extra-propagated-inputs . ("pandoc"))))
    (build-system r-build-system)
    (propagated-inputs
     (list pandoc
           r-bslib
           r-evaluate
           r-fontawesome
           r-htmltools
           r-jquerylib
           r-jsonlite
           r-knitr
           r-tinytex
           r-xfun
           r-yaml))
    (native-inputs
     (list esbuild r-knitr r-testthat))
    (home-page "https://rmarkdown.rstudio.com")
    (synopsis "Convert R Markdown documents into a variety of formats")
    (description
     "This package provides tools to convert R Markdown documents into a
variety of formats.")
    (license license:gpl3+)))

(define-public r-gtable
  (package
    (name "r-gtable")
    (version "0.3.6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "gtable" version))
              (sha256
               (base32
                "1vz0kpj1hh3iz27yaax4i3l9n018py455p4f5nfn92r727xaa1fk"))))
    (properties `((upstream-name . "gtable")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cli r-glue r-lifecycle r-rlang))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://cran.r-project.org/web/packages/gtable")
    (synopsis "R library to arrange grobs in tables")
    (description
     "Gtable is a collection of tools to make it easier to work with
\"tables\" of grobs.")
    (license license:gpl2+)))

(define-public r-gridextra
  (package
    (name "r-gridextra")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "gridExtra" version))
              (sha256
               (base32
                "0fwfk3cdwxxim7nd55pn7m31bcaqj48y06j7an2k1v1pybk0rdl1"))))
    (properties `((upstream-name . "gridExtra")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gtable))
    (native-inputs
     (list r-knitr r-testthat)) ;for building vignettes
    (home-page "https://github.com/baptiste/gridextra")
    (synopsis "Miscellaneous functions for \"Grid\" graphics")
    (description
     "This package provides a number of user-level functions to work with
@code{grid} graphics, notably to arrange multiple grid-based plots on a page,
and draw tables.")
    (license license:gpl2+)))

(define-public r-pkgconfig
  (package
    (name "r-pkgconfig")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "pkgconfig" version))
              (sha256
               (base32
                "0l1qph8zyi2sic3k2qcd7vlfl7rzfh1q7z7zvjkl5f7y1x2fy3rk"))))
    (build-system r-build-system)
    (native-inputs (list r-testthat))
    (home-page "https://github.com/gaborcsardi/pkgconfig")
    (synopsis "Private configuration for R packages")
    (description "This package provides the functionality to set configuration
options on a per-package basis.  Options set by a given package only apply to
that package, other packages are unaffected.")
    (license license:expat)))

(define-public r-rsqlite
  (package
    (name "r-rsqlite")
    (version "2.3.9")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RSQLite" version))
              (sha256
               (base32
                "116029x7kykxy0wg4p1ln14x6w7wa16jdsnb8wmy6c4khdzlbsym"))))
    (properties
     '((upstream-name . "RSQLite")
       ;; These are not strictly necessary for running tests and adding them
       ;; would cause dependency cycles.
       (updater-ignored-native-inputs . ("r-dbitest" "r-ndbi"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         ;; Needed for one failing test
         (add-before 'check 'set-locale
           (lambda _ (setenv "LC_ALL" "en_US.UTF-8"))))))
    (propagated-inputs
     (list r-bit64
           r-blob
           r-cpp11
           r-dbi
           r-memoise
           r-pkgconfig
           r-plogr
           r-rlang))
    (native-inputs
     (list r-callr
           r-hms
           r-knitr
           r-magrittr
           r-testthat
           r-withr))
    (home-page "https://github.com/rstats-db/RSQLite")
    (synopsis "SQLite interface for R")
    (description
     "This package embeds the SQLite database engine in R and provides an
interface compliant with the DBI package.  The source for the SQLite
engine (version 3.8.8.2) is included.")
    (license license:lgpl2.0+)))

(define-public r-rcurl
  (package
    (name "r-rcurl")
    (version "1.98-1.16")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RCurl" version))
              (sha256
               (base32
                "19qddkyasfwa61mj0r0m4wk2xhpsqxf2dikih0s1fdjr207c938s"))))
    (properties `((upstream-name . "RCurl")))
    (build-system r-build-system)
    (arguments
     (list
      ;; Tests require internet connection.
      #:tests? #false
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'respect-CURL_CA_BUNDLE
           (lambda _
             (substitute* "R/options.S"
               (("\\.els = rev\\(merge\\(list\\(\\.\\.\\.\\), \\.opts\\)\\)" m)
                (string-append "\
certs = Sys.getenv(\"CURL_CA_BUNDLE\")
if (certs != \"\") { .opts = merge.list(.opts, list(cainfo=certs)) }
" m))))))))
    (inputs
     (list curl libxml2))
    (propagated-inputs
     (list r-bitops))
    (home-page "https://www.omegahat.net/RCurl")
    (synopsis "General network client interface for R")
    (description
     "The package allows one to compose general HTTP requests and provides
convenient functions to fetch URIs, GET and POST forms, etc. and process the
results returned by the Web server.  This provides a great deal of control
over the HTTP/FTP/... connection and the form of the request while providing a
higher-level interface than is available just using R socket connections.
Additionally, the underlying implementation is robust and extensive,
supporting FTP/FTPS/TFTP (uploads and downloads), SSL/HTTPS, telnet, dict,
ldap, and also supports cookies, redirects, authentication, etc.")
    (license license:bsd-3)))

(define-public r-lambda-r
  (package
    (name "r-lambda-r")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "lambda.r" version))
              (sha256
               (base32
                "1mh1g0gsd58gng0hb29vww2yqb2jfs07kba5kxnnqck5j3izwlnj"))))
    (properties `((upstream-name . "lambda.r")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-formatr))
    (native-inputs (list r-testit))
    (home-page "https://cran.r-project.org/web/packages/lambda.r")
    (synopsis "Functional programming extension for R")
    (description
     "This package provides a language extension to efficiently write
functional programs in R.  Syntax extensions include multi-part function
definitions, pattern matching, guard statements, built-in (optional) type
safety.")
    (license license:lgpl3+)))

(define-public r-futile-options
  (package
    (name "r-futile-options")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "futile.options" version))
              (sha256
               (base32
                "0w15agpi88y3qkv6fl72zy2pzyplzgvnj41a4ixhg64mw1sck73s"))))
    (properties
     `((upstream-name . "futile.options")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/futile.options")
    (synopsis "Options management framework")
    (description
     "The futile.options subsystem provides an easy user-defined options
management system that is properly scoped.  This means that options created
via @code{futile.options} are fully self-contained and will not collide with
options defined in other packages.")
    (license license:lgpl3+)))

(define-public r-futile-logger
  (package
    (name "r-futile-logger")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "futile.logger" version))
              (sha256
               (base32
                "1r3nayk0z9n1svbf8640vw90dal5q07nkn0gv4bnva3pbzb352sy"))))
    (properties `((upstream-name . "futile.logger")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-futile-options r-lambda-r))
    (native-inputs (list r-testthat))
    (home-page "https://cran.r-project.org/web/packages/futile.logger")
    (synopsis "Logging utility for R")
    (description
     "This package provides a simple yet powerful logging utility.  Based
loosely on log4j, futile.logger takes advantage of R idioms to make logging a
convenient and easy to use replacement for @code{cat} and @code{print}
statements.")
    (license license:lgpl3+)))

(define-public r-segmented
  (package
    (name "r-segmented")
    (version "2.1-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "segmented" version))
       (sha256
        (base32
         "0h3c6gnhy7242phidzia8ly1sa8x9xyspqlbbj0g9w4vxqby4bh7"))))
    (build-system r-build-system)
    (propagated-inputs (list r-mass r-nlme))
    (home-page "https://cran.r-project.org/web/packages/segmented")
    (synopsis "Regression models with breakpoints estimation")
    (description
     "Given a regression model, segmented updates the model by adding one or
more segmented (i.e., piecewise-linear) relationships.  Several variables with
multiple breakpoints are allowed.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-snow
  (package
    (name "r-snow")
    (version "0.4-4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "snow" version))
              (sha256
               (base32
                "1j8kvf3imxijsqkdjz4i9s7qggfxqrpas46y5wz6za92y937yn44"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/snow")
    (synopsis "Support for simple parallel computing in R")
    (description
     "The snow package provides support for simple parallel computing on a
network of workstations using R.  A master R process calls @code{makeCluster}
to start a cluster of worker processes; the master process then uses functions
such as @code{clusterCall} and @code{clusterApply} to execute R code on the
worker processes and collect and return the results on the master.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-sparsem
  (package
    (name "r-sparsem")
    (version "1.84-2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "SparseM" version))
              (sha256
               (base32
                "0rc6x466jhalb6baxxhffmmcpi03nndvvighp696rrvrhsxzv015"))))
    (properties
     `((upstream-name . "SparseM")))
    (native-inputs
     (list gfortran r-knitr))
    (build-system r-build-system)
    (home-page "http://www.econ.uiuc.edu/~roger/research/sparse/sparse.html")
    (synopsis "Sparse linear algebra")
    (description
     "This package provides some basic linear algebra functionality for sparse
matrices.  It includes Cholesky decomposition and backsolving as well as
standard R subsetting and Kronecker products.")
    (license license:gpl2+)))

(define-public r-iterators
  (package
    (name "r-iterators")
    (version "1.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "iterators" version))
       (sha256
        (base32
         "0yv7rh6ghlfx727xy2aq64a7skyxp9msakaffs641q9h15d0gwyf"))))
    (build-system r-build-system)
    (native-inputs (list r-runit))
    (home-page "https://cran.r-project.org/web/packages/iterators")
    (synopsis "Iterator construct for R")
    (description
     "This package provides support for iterators, which allow a programmer to
traverse through all the elements of a vector, list, or other collection of
data.")
    (license license:asl2.0)))

(define-public r-foreach
  (package
    (name "r-foreach")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "foreach" version))
       (sha256
        (base32
         "1r5gdf9fp3rprvrhf0gzl9qmmqhgdn5gscpm5hk8zxpraf3qscsn"))))
    (build-system r-build-system)
    ;; Tests require doparallel, which we cannot add because of a dependency
    ;; cycle.
    (arguments (list #:tests? #false))
    (propagated-inputs
     (list r-codetools r-iterators))
    (native-inputs
     (list r-knitr))
    (home-page "https://cran.r-project.org/web/packages/foreach")
    (synopsis "Foreach looping construct for R")
    (description
     "This package provides support for the @code{foreach} looping construct.
@code{foreach} is an idiom that allows for iterating over elements in a
collection, without the use of an explicit loop counter.  This package in
particular is intended to be used for its return value, rather than for its
side effects.  In that sense, it is similar to the standard @code{lapply}
function, but doesn't require the evaluation of a function.  Using
@code{foreach} without side effects also facilitates executing the loop in
parallel.")
    (license license:asl2.0)))

(define-public r-doparallel
  (package
    (name "r-doparallel")
    (version "1.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "doParallel" version))
       (sha256
        (base32
         "1mxbg2qqda1775vb4s26gz27p5n91lljgjmlqxq4sras22njasmr"))))
    (properties `((upstream-name . "doParallel")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-foreach r-iterators))
    (native-inputs (list r-runit))
    (home-page "https://cran.r-project.org/web/packages/doParallel")
    (synopsis "Foreach parallel adaptor for the 'parallel' package")
    (description
     "This package provides a parallel backend for the @code{%dopar%} function
using the parallel package.")
    (license license:gpl2+)))

(define-public r-domc
  (package
    (name "r-domc")
    (version "1.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "doMC" version))
       (sha256
        (base32
         "18qrcvqwm4cclvk8spvyi281y8prwzivj52xmzk1l9a82j2ny65j"))))
    (properties `((upstream-name . "doMC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-foreach r-iterators))
    (native-inputs (list r-runit))
    (home-page "https://cran.r-project.org/web/packages/doMC")
    (synopsis "Foreach parallel adaptor for the 'parallel' package")
    (description
     "This package provides a parallel backend for the @code{%dopar%} function
using the multicore functionality of the parallel package.")
    (license license:gpl2+)))

(define-public r-dt
  (let* ((extension-origin
          (lambda (name version hash)
            (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     (format #false "https://github.com/DataTables/~a" name))
                    (commit version)))
              (file-name
               (format #false "datatables-~a-~a-checkout" name version))
              (sha256
               (base32 hash)))))
         (extensions
          '(((name . "AutoFill")
             (version . "2.6.0")
             (hash . "0dmhp7vcg9l2qwcvn8w605n9yz38b3k9j907j4i495577mk6cqb0"))
            ((name . "Buttons")
             (version . "2.4.2")
             (hash . "0zr7k5ginsfrg0nxayyd68z410ymxa6qs5n7zqjbzaydf5na2rk3"))
            ((name . "ColReorder")
             (version . "1.7.0")
             (hash . "1z152xhncmr7612wxn6p94m8330d31kdrial86zr2bj77ix571pl"))
            ((name . "DateTime")
             (version . "1.5.1")
             (hash . "11wj2pbisymp148jwg2r05ppa6c2swpip6k04q8f324ma4wy83c5"))
            ((name . "FixedHeader")
             (version . "3.4.0")
             (hash . "0vfgxmg11mbvzvi25c88a611nz53qi88j5jnmpwjag584b90a6bj"))
            ((name . "KeyTable")
             (version . "2.10.0")
             (hash . "18a0pz76532lyn9xsa079ibbfb8rrd2wbv4c8pk9npxc6j1ml0j1"))
            ((name . "Responsive")
             (version . "2.5.0")
             (hash . "04njvyab7hv61nrxs8g7nyxnxjljhnd22kmhahhdz34xf54ydy01"))
            ((name . "RowGroup")
             (version . "1.4.0")
             (hash . "0anvrw096k3pj1k7gkvyhpgchh3jzk2r6j48ywfssgqh38x41byr"))
            ((name . "RowReorder")
             (version . "1.4.1")
             (hash . "0zg7vz19hapgfivjc98fliz59zfwidx9fg42rj5zkamdn18gm8sm"))
            ((name . "Scroller")
             (version . "2.2.0")
             (hash . "1wzla8710986kax63a3r48a5j5ddkbn9fd36pdy778xb9qp766iq"))
            ((name . "Select")
             (version . "1.7.0")
             (hash . "1rb6ik3cd5zijlm95d0fl93bvan6j3bdbj83lrdzhb419qs3v3bf"))))
         (ts-extensions
          '(((name . "FixedColumns")
             (version . "4.3.0")
             (hash . "0z31qw3nz1arccg1zwkdnla98mws7c49gsyncv61c3ghh5b0qlcg"))
            ((name . "SearchBuilder")
             (version . "1.6.0")
             (hash . "10df2jrc3z2v44alwl45cp1qr96b49ai8x7kspqn6ha816lwjqrv"))
            ((name . "SearchPanes")
             (version . "2.2.0")
             (hash . "0pkcxyy357zd722bz1jnbn0dmccpwfpy6qca5cpyl97mdmmprpi9"))
            ((name . "StateRestore")
             (version . "1.3.0")
             (hash . "0zj7idfyj02hynd02w2gjrnp0m8krhzrkwn5fjadfs7gq4i21ial"))))
         (javascript-sources
          `(("https://cdn.datatables.net/1.13.6/js/jquery.dataTables.js"
             "15yjj5s1nkf9f1llmjbjgbw380gl1v35psab4qd7l18gvrspaf5k"
             "datatables")
            ("https://cdn.datatables.net/1.13.6/js/dataTables.bootstrap.js"
             "064a4vm2wd2qhf1szkppvh80fzqpyrm0xjzfcqrg1x9mr8xfs06p"
             "datatables")
            ("https://cdn.datatables.net/1.13.6/js/dataTables.bootstrap4.js"
             "0w2s6zs7bwlxdzc2z67pj49naqs2kh7xgmxmh1a4d42x3fpxacgy"
             "datatables")
            ("https://cdn.datatables.net/1.13.6/js/dataTables.bootstrap5.js"
             "1c1bnaq35w37iq214gq8rsd4rx0wp1c46054w9h21vqpf2xiy79i"
             "datatables")
            ("https://cdn.datatables.net/1.13.6/js/dataTables.bulma.js"
             "0d7ylp4qgkzkrfc5y7p0nzxxxqibynz8ijsrkvc65jbcbccxglsd"
             "datatables")
            ("https://cdn.datatables.net/1.13.6/js/dataTables.foundation.js"
             "0s6r8pc7rdic4qifwc0x3q9i737930wfqh27yycksrbv7i96w2s7"
             "datatables")
            ("https://cdn.datatables.net/1.13.6/js/dataTables.jqueryui.js"
             "09cwapax46yf2qz9w50wq9yqzysgfqfnmm9s2ixlafzhzdlx5nw5"
             "datatables")
            ("https://cdn.datatables.net/1.13.6/js/dataTables.semanticui.js"
             "09q31xdgnyc0fsi81qg6a2rfivgncm7jk7x2idm7b1pip8d58j74"
             "datatables")
            ("https://cdnjs.cloudflare.com/ajax/libs/jszip/3.10.1/jszip.js"
             "01l5lw49jz2qn6k9i63dk4llar4lvvpd6xp6i45mpwfk49fbxqg2"
             "datatables-extensions/Buttons"))))
    (package
      (name "r-dt")
      (version "0.33")
      (source (origin
                (method url-fetch)
                (uri (cran-uri "DT" version))
                (sha256
                 (base32
                  "1f17gdqjk1aj7vwjvv3363k8lnsvc6ssh4s3gy1prnz33kdxlig1"))
                (modules '((guix build utils)
                           (ice-9 match)))
                (snippet
                 `(with-directory-excursion "inst/htmlwidgets/lib"
                    (for-each (match-lambda
                                ((url hash dir)
                                 (let ((file (string-append dir "/js/" (basename url ".js") ".min.js")))
                                   (delete-file file))))
                              ',javascript-sources)
                    ;; Preserve pdfmake.js and vfs_fonts.js
                    (copy-file "datatables-extensions/Buttons/js/pdfmake.js"
                               "/tmp/pdfmake.js")
                    (copy-file "datatables-extensions/Buttons/js/vfs_fonts.js"
                               "/tmp/vfs_fonts.js")
                    (for-each (lambda (extension)
                                (let ((name (assoc-ref extension 'name)))
                                  (delete-file-recursively
                                   (string-append "datatables-extensions/" name "/js"))))
                              (append ',extensions ',ts-extensions))
                    (mkdir-p "datatables-extensions/Buttons/js/")
                    (copy-file "/tmp/pdfmake.js"
                               "datatables-extensions/Buttons/js/pdfmake.js")
                    (copy-file "/tmp/vfs_fonts.js"
                               "datatables-extensions/Buttons/js/vfs_fonts.js")
                    (delete-file "nouislider/jquery.nouislider.min.js")
                    (delete-file "selectize/selectize.min.js")
                    (with-directory-excursion "datatables-plugins/features/"
                      (for-each delete-file
                                '("scrollResize/source.min.js"
                                  "searchHighlight/source.min.js")))))))
      (properties
       `((upstream-name . "DT")))
      (build-system r-build-system)
      (arguments
       `(#:modules
         ((guix build r-build-system)
          (guix build minify-build-system)
          (guix build utils)
          (ice-9 match))
         #:imported-modules
         (,@%r-build-system-modules
          (guix build minify-build-system))
         #:phases
         (modify-phases (@ (guix build r-build-system) %standard-phases)
           (add-after 'unpack 'process-javascript
             (lambda* (#:key inputs #:allow-other-keys)
               (with-directory-excursion "inst/htmlwidgets/lib/"
                 (for-each (match-lambda
                             ((url hash dir)
                              (let* ((input (string-append "js:" (basename url)))
                                     (source (assoc-ref inputs input))
                                     (target (string-append dir "/js/"
                                                            (basename url ".js")
                                                            ".min.js")))
                                (mkdir-p dir)
                                (minify source #:target target))))
                           ',javascript-sources)

                 ;; Minify JS for each extension
                 (for-each (lambda (extension)
                             (let* ((name    (assoc-ref extension 'name))
                                    (version (assoc-ref extension 'version))
                                    (hash    (assoc-ref extension 'hash))
                                    (label   (string-append "js:" name))
                                    (dir     (string-append "datatables-extensions/"
                                                            name "/js")))
                               (mkdir-p dir)
                               (with-directory-excursion dir
                                 (for-each
                                  (lambda (file)
                                    (minify file
                                            #:target
                                            (string-append (basename file ".js") ".min.js")))
                                  (find-files (string-append (assoc-ref inputs label) "/js")
                                              "\\.js$")))))
                           ',extensions)
                 ;; Process typescript extensions
                 (for-each (lambda (extension)
                             (let* ((name    (assoc-ref extension 'name))
                                    (version (assoc-ref extension 'version))
                                    (hash    (assoc-ref extension 'hash))
                                    (label   (string-append "ts:" name))
                                    (dir     (string-append "datatables-extensions/"
                                                            name "/js")))
                               (mkdir-p dir)
                               (with-directory-excursion dir
                                 (apply invoke "esbuild"
                                        "--bundle"
                                        (string-append
                                         "--tsconfig="
                                         (string-append (assoc-ref inputs label)
                                                        "/tsconfig.json"))
                                        (string-append "--outdir=" (getcwd))
                                        (find-files
                                         (string-append (assoc-ref inputs label) "/src/")
                                         "\\.ts$"))
                                 (rename-file "index.js" (string-append "dataTables."
                                                                        (string-downcase name 0 1)
                                                                        ".js"))
                                 (for-each
                                  (lambda (file)
                                    (minify file
                                            #:target
                                            (string-append (basename file ".js") ".min.js"))
                                    (delete-file file))
                                  (find-files "." "\\.js$")))))
                           ',ts-extensions)
                 (minify (string-append (assoc-ref inputs "datatables-plugins")
                                        "/dataRender/ellipsis.js")
                         #:target "datatables-plugins/dataRender/ellipsis/source.min.js")
                 (minify (string-append (assoc-ref inputs "datatables-plugins")
                                        "/filtering/type-based/accent-neutralise.js")
                         #:target "datatables-plugins/filtering/accent-neutralise/source.min.js")
                 (minify (string-append (assoc-ref inputs "datatables-plugins")
                                        "/filtering/type-based/diacritics-neutralise.js")
                         #:target "datatables-plugins/filtering/diacritics-neutralise/source.min.js")
                 (minify (string-append (assoc-ref inputs "datatables-plugins")
                                        "/sorting/natural.js")
                         #:target "datatables-plugins/sorting/natural/source.min.js")
                 (minify (string-append (assoc-ref inputs "datatables-plugins")
                                        "/features/scrollResize/dataTables.scrollResize.js")
                         #:target "datatables-plugins/features/scrollResize/source.min.js")
                 (minify (string-append (assoc-ref inputs "datatables-plugins")
                                        "/features/searchHighlight/dataTables.searchHighlight.js")
                         #:target "datatables-plugins/features/searchHighlight/source.min.js")
                 (minify (assoc-ref inputs "js-nouislider")
                         #:target "nouislider/jquery.nouislider.min.js")

                 (let ((replace-file (lambda (old new)
                                       (format #t "replacing ~a with ~a\n" old new)
                                       (symlink new old))))
                   (replace-file "selectize/selectize.min.js"
                                 (string-append (assoc-ref inputs "js-selectize")
                                                "/share/javascript/selectize.min.js")))))))))
      (propagated-inputs
       (list r-crosstalk
             r-htmltools
             r-htmlwidgets
             r-httpuv
             r-jquerylib
             r-jsonlite
             r-magrittr
             r-promises))
      (inputs
       (list js-selectize))
      (native-inputs
       `(("r-knitr" ,r-knitr)
         ("r-testit" ,r-testit)
         ("esbuild" ,esbuild)
         ("datatables-plugins"
          ,(let ((version "1.13.6"))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/DataTables/Plugins.git")
                     (commit version)))
               (file-name (git-file-name "datatables-plugins" version))
               (sha256
                (base32
                 "02ijp9671al2fpb5sahy1z4nx1q75jp8p0i77vv87r8lqmsvsjis")))))
         ("js-nouislider"
          ,(let ((version "7.0.10"))
             (origin
               (method url-fetch)
               (uri (string-append "https://raw.githubusercontent.com/leongersen/noUiSlider/"
                                   version "/distribute/jquery.nouislider.js"))
               (sha256
                (base32
                 "1f7vsfcn7wwzngib6j0wpl0psd6qriiaa6kv728ynfn5da73zfxm")))))
         ,@(map (match-lambda
                  ((url hash dir)
                   `(,(string-append "js:" (basename url))
                     ,(origin (method url-fetch)
                              (uri url)
                              (sha256 (base32 hash))))))
                javascript-sources)
         ,@(map (lambda (extension)
                  (let ((name    (assoc-ref extension 'name))
                        (version (assoc-ref extension 'version))
                        (hash    (assoc-ref extension 'hash)))
                    `(,(string-append "js:" name)
                      ,(extension-origin name version hash))))
                extensions)
         ,@(map (lambda (extension)
                  (let ((name    (assoc-ref extension 'name))
                        (version (assoc-ref extension 'version))
                        (hash    (assoc-ref extension 'hash)))
                    `(,(string-append "ts:" name)
                      ,(extension-origin name version hash))))
                ts-extensions)))
      (home-page "https://rstudio.github.io/DT")
      (synopsis "R wrapper of the DataTables JavaScript library")
      (description
       "This package allows for data objects in R to be rendered as HTML tables
using the JavaScript library @code{DataTables} (typically via R Markdown or
Shiny).  The @code{DataTables} library has been included in this R package.")
      ;; The DT package as a whole is distributed under GPLv3.  The DT package
      ;; inludes other software components under different licenses:
      ;;
      ;;   * Expat: jquery.highlight.js, DataTables
      ;;   * WTFPL: noUiSlider
      (license (list license:gpl3
                     license:expat
                     license:wtfpl2)))))

(define-public r-irlba
  (package
    (name "r-irlba")
    (version "2.3.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "irlba" version))
       (sha256
        (base32
         "1ky5nlmyrnwz6121wwqd8p8r1ycnjkl5r290k4x2477rzs267zic"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-matrix))
    (home-page "https://cran.r-project.org/web/packages/irlba")
    (synopsis "Methods for eigendecomposition of large matrices")
    (description
     "This package provides fast and memory efficient methods for truncated
singular and eigenvalue decompositions, as well as for principal component
analysis of large sparse or dense matrices.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-glmnet
  (package
   (name "r-glmnet")
   (version "4.1-8")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "glmnet" version))
     (sha256
      (base32 "1znw1p1mi7nv39l7albcg6sfxj7a1jyjw85hb3ginv870z7fbnqx"))))
   (build-system r-build-system)
   (native-inputs
    (list gfortran r-knitr r-testthat))
   (propagated-inputs
    (list r-foreach
          r-matrix
          r-rcpp
          r-rcppeigen
          r-shape
          r-survival))
   (home-page "https://www.jstatsoft.org/article/view/v033i01")
   (synopsis "Lasso and elastic-net regularized generalized linear models")
   (description
    "The glmnet package provides efficient procedures for fitting the entire
lasso or elastic-net regularization path for linear and Poisson regression, as
well as logistic, multinomial, Cox, multiple-response Gaussian and grouped
multinomial models.  The algorithm uses cyclical coordinate descent in a
path-wise fashion.")
   (license license:gpl2+)))

(define-public r-pkgmaker
  (package
    (name "r-pkgmaker")
    (version "0.32.10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pkgmaker" version))
       (sha256
        (base32
         "0cr95vmsb4gkl917wg4wwq8jihvwasdg18qzhk2cq224lrrh8awp"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-codetools
           r-digest
           r-registry
           r-stringr
           r-withr
           r-xtable))
    (native-inputs (list r-testthat))
    (home-page "https://renozao.github.io/pkgmaker")
    (synopsis "Package development utilities")
    (description
     "This package provides some low-level utilities to use for R package
development.  It currently provides managers for multiple package specific
options and registries, vignette, unit test and bibtex related utilities.")
    (license license:gpl2+)))

 (define-public r-registry
   (package
     (name "r-registry")
     (version "0.5-1")
     (source
      (origin
        (method url-fetch)
        (uri (cran-uri "registry" version))
        (sha256
         (base32
          "1k3j6dx350awamr0dwwgkhfs46vsnj4nf08iw5byq0x7n3nkdsnz"))))
     (build-system r-build-system)
     (home-page "https://cran.r-project.org/web/packages/registry")
     (synopsis "Infrastructure for R package registries")
     (description
      "This package provides a generic infrastructure for creating and using R
package registries.")
     (license license:gpl2+)))

(define-public r-rngtools
  (package
    (name "r-rngtools")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rngtools" version))
       (sha256
        (base32
         "0kd7x214cqw7hzpmk1iqy1bn7j6x0ady0yz2hsdbclbq9k57d33z"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-digest))
    (native-inputs (list r-testthat))
    (home-page "https://renozao.github.io/rngtools")
    (synopsis "Utility functions for working with random number generators")
    (description
     "This package contains a set of functions for working with Random Number
Generators (RNGs).  In particular, it defines a generic S4 framework for
getting/setting the current RNG, or RNG data that are embedded into objects
for reproducibility.  Notably, convenient default methods greatly facilitate
the way current RNG settings can be changed.")
    (license license:gpl3+)))

(define-public r-rtsne
  (package
    (name "r-rtsne")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rtsne" version))
       (sha256
        (base32
         "02srqmfkdj7v9gyhssaqdarg4ljn2ds77w25a5w0dm66sqa6ibis"))))
    (properties
     `((upstream-name . "Rtsne")
       (updater-extra-native-inputs . ("r-irlba"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp))
    (native-inputs (list r-irlba r-testthat))
    (home-page "https://github.com/jkrijthe/Rtsne")
    (synopsis "T-distributed stochastic neighbor embedding")
    (description
     "This package provides an R wrapper around the fast T-distributed
Stochastic Neighbor Embedding using a Barnes-Hut implementation.")
    ;; The declared license for this package is BSD-3, but it also includes
    ;; code licensed under BSD-4.
    (license (list license:bsd-3 license:bsd-4))))

(define-public r-synchronicity
  (package
    (name "r-synchronicity")
    (version "1.3.10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "synchronicity" version))
       (sha256
        (base32
         "12svf4xzb9s3m1h2ddqgl6q2v57ifsj2p80j8cg2k0543a43pww2"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh r-bigmemory-sri r-rcpp r-uuid))
    (home-page "http://www.bigmemory.org")
    (synopsis "Boost mutex functionality in R")
    (description "This package provides support for synchronization
via mutexes and may eventually support interprocess communication and
message passing.")
    ;; Users can choose either LGPLv3 or ASL2.0.
    (license (list license:lgpl3 license:asl2.0))))

(define-public r-r-methodss3
  (package
    (name "r-r-methodss3")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.methodsS3" version))
              (sha256
               (base32
                "0bkwj9c2cpgb0ibk9znh8qh4k1wzp3bkhaxyhf41xjflv9hmwbc2"))))
    (properties `((upstream-name . "R.methodsS3")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/R.methodsS3")
    (synopsis "S3 methods simplified")
    (description
     "This package provides methods that simplify the setup of S3 generic
functions and S3 methods.  Major effort has been made in making definition of
methods as simple as possible with a minimum of maintenance for package
developers.  For example, generic functions are created automatically, if
missing, and naming conflict are automatically solved, if possible.  The
method @code{setMethodS3()} is a good start for those who in the future may
want to migrate to S4.")
    (license license:lgpl2.1+)))

(define-public r-r-oo
  (package
    (name "r-r-oo")
    (version "1.27.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.oo" version))
              (sha256
               (base32
                "1yb73m3fyl6sn6vf1jd64xh7np3g990v3cgyzjxjj9v7dm8824ni"))))
    (properties `((upstream-name . "R.oo")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-r-methodss3))
    (home-page "https://github.com/HenrikBengtsson/R.oo")
    (synopsis "R object-oriented programming with or without references")
    (description
     "This package provides methods and classes for object-oriented
programming in R with or without references.  Large effort has been made on
making definition of methods as simple as possible with a minimum of
maintenance for package developers.")
    (license license:lgpl2.1+)))

(define-public r-r-utils
  (package
    (name "r-r-utils")
    (version "2.12.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.utils" version))
              (sha256
               (base32
                "03640a1v8jk9k9s5xirdia9lngb53gh4p9gyj2j82cx2jmxfgmkl"))))
    (properties `((upstream-name . "R.utils")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-references
           (lambda _
             (substitute* '("R/System.R"
                            "R/GString-class.R")
               (("/usr/bin/env uname") (which "uname"))
               (("/usr/bin/env whoami") (which "whoami"))))))))
    (propagated-inputs
     (list r-r-methodss3 r-r-oo))
    (native-inputs
     (list r-digest))
    (home-page "https://github.com/HenrikBengtsson/R.utils")
    (synopsis "Various programming utilities")
    (description
     "This package provides utility functions useful when programming and
developing R packages.")
    (license license:lgpl2.1+)))

(define-public r-r-cache
  (package
    (name "r-r-cache")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.cache" version))
              (sha256
               (base32
                "14cja0d78mzipb94cwgdl00k5r7awjclzxl30c77j6jpc68l0lvq"))))
    (properties `((upstream-name . "R.cache")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-digest r-r-methodss3 r-r-oo r-r-utils))
    (home-page "https://github.com/HenrikBengtsson/R.cache")
    (synopsis "Light-weight caching of objects and results")
    (description
     "This package provides methods for caching or memoization of objects and
results.  With this package, any R object can be cached in a key-value storage
where the key can be an arbitrary set of R objects.  The cache memory is
persistent (on the file system).")
    (license license:lgpl2.1+)))

(define-public r-r-rsp
  (package
    (name "r-r-rsp")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.rsp" version))
              (sha256
               (base32
                "1frkgjc2mzvjnay8g5nky1bvxv60wvsypmmdj6mbsfjnzq7ni7qs"))))
    (properties `((upstream-name . "R.rsp")))
    (build-system r-build-system)
    (arguments
     (list
      ;; Vignettes require r-r-devices, which uses r-r-rsp.
      #:test-types '(list "tests")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp")))
         (add-after 'unpack 'delete-broken-test
           (lambda _
             ;; The multi,selfcontained.R tests fail because r-r-devices is
             ;; missing.  It depends on r-r-rsp, so we can't add it.
             (delete-file "tests/multi,selfcontained.R"))))))
    (propagated-inputs
     (list r-digest r-r-cache r-r-methodss3 r-r-oo r-r-utils))
    (home-page "https://github.com/HenrikBengtsson/R.rsp")
    (synopsis "Dynamic generation of scientific reports")
    (description
     "The RSP markup language provides a powerful markup for controlling the
content and output of LaTeX, HTML, Markdown, AsciiDoc, Sweave and knitr
documents (and more), e.g. @code{Today's date is <%=Sys.Date()%>}.  Contrary
to many other literate programming languages, with RSP it is straightforward
to loop over mixtures of code and text sections, e.g.  in month-by-month
summaries.  RSP has also several preprocessing directives for incorporating
static and dynamic contents of external files (local or online) among other
things.  RSP is ideal for self-contained scientific reports and R package
vignettes.")
    (license license:lgpl2.1+)))

(define-public r-mvtnorm
  (package
    (name "r-mvtnorm")
    (version "1.3-3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "mvtnorm" version))
              (sha256
               (base32
                "0wa1zal0q8zawqrjllx70kakn3ynpsm0imb3mmc956y89d9py3p2"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran r-numderiv))
    (home-page "https://mvtnorm.R-forge.R-project.org")
    (synopsis "Package for multivariate normal and t-distributions")
    (description "This package can compute multivariate normal and
t-probabilities, quantiles, random deviates and densities.")
    (license license:gpl2)))

(define-public r-matrixstats
  (package
    (name "r-matrixstats")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "matrixStats" version))
              (sha256
               (base32
                "05fcbpwrrs3y5ia15x88x7br7ympf5mg31qh7sj05hkg7rgnr68j"))))
    (properties `((upstream-name . "matrixStats")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list r-covr r-r-rsp)) ;used to build vignettes
    (home-page "https://github.com/HenrikBengtsson/matrixStats")
    (synopsis "Methods applying to vectors and matrix rows and columns")
    (description
     "This package provides methods operating on rows and columns of matrices,
e.g.  @code{rowMedians()}, @code{rowRanks()}, and @code{rowSds()}.  There are
also some vector-based methods, e.g. @code{binMeans()}, @code{madDiff()} and
@code{weightedMedians()}.  All methods have been optimized for speed and
memory usage.")
    (license license:artistic2.0)))

(define-public r-viridis
  (package
    (name "r-viridis")
    (version "0.6.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "viridis" version))
              (sha256
               (base32
                "0p00s24d8bsifw3r0afwrx98bqixnf5krpbw42hfwp8ipsv5qaw6"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2 r-gridextra r-viridislite))
    (native-inputs
     (list r-knitr r-testthat)) ; for vignettes
    (home-page "https://github.com/sjmgarnier/viridis")
    (synopsis "Matplotlib default color map")
    (description
     "This package is a port of the new @url{matplotlib,
http://matplotlib.org/} color maps (@code{viridis}--the default--,
@code{magma}, @code{plasma}, and @code{inferno}) to R.  These color maps are
designed in such a way that they will analytically be perfectly
perceptually-uniform, both in regular form and also when converted to
black-and-white.  They are also designed to be perceived by readers with the
most common form of color blindness.")
    (license license:x11)))

(define-public r-viridislite
  (package
    (name "r-viridislite")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "viridisLite" version))
       (sha256
        (base32
         "18g1rk24kr47jl01r70vvni2146fl9xxpjcvjp6d5k6y64fi2gw9"))))
    (properties `((upstream-name . "viridisLite")))
    (build-system r-build-system)
    (native-inputs (list r-testthat))
    (home-page "https://github.com/sjmgarnier/viridisLite")
    (synopsis "Default color maps from matplotlib")
    (description
     "This package is a port of the new @code{matplotlib} color maps
(@code{viridis}, @code{magma}, @code{plasma} and @code{inferno}) to R.
matplotlib is a popular plotting library for Python.  These color maps are
designed in such a way that they will analytically be perfectly
perceptually-uniform, both in regular form and also when converted to
black-and-white.  They are also designed to be perceived by readers with the
most common form of color blindness.  This is the @code{lite} version of the
more complete @code{viridis} package.")
    (license license:expat)))

(define-public r-tidyselect
  (package
    (name "r-tidyselect")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tidyselect" version))
       (sha256
        (base32
         "0g4h8mfm5ima0izy4h0c65q478473xsj4hskh15dzg5z1fx9g7hn"))))
    (properties
     '((updater-extra-native-inputs . ("r-stringr"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cli r-glue r-lifecycle r-rlang r-vctrs r-withr))
    (native-inputs
     (list r-knitr r-stringr r-testthat))
    (home-page "https://cran.r-project.org/web/packages/tidyselect")
    (synopsis "Select from a set of strings")
    (description
     "This package provides a backend for the selecting functions of the
tidyverse.  It makes it easy to implement select-like functions in your own
packages in a way that is consistent with other tidyverse interfaces for
selection.")
    (license license:gpl3)))

(define-public r-tidyr
  (package
    (name "r-tidyr")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tidyr" version))
       (sha256
        (base32
         "0pc3ad9k36lk3c5qbgx4blhs8aihqyysfxljyirgahsmrdhw4878"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cli
           r-cpp11
           r-dplyr
           r-glue
           r-lifecycle
           r-magrittr
           r-purrr
           r-rlang
           r-stringr
           r-tibble
           r-tidyselect
           r-vctrs))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://github.com/hadley/tidyr")
    (synopsis "Tidy data with `spread()` and `gather()` functions")
    (description
     "tidyr is a reframing of the reshape2 package designed to accompany the
tidy data framework, and to work hand-in-hand with magrittr and dplyr to build
a solid pipeline for data analysis.  It is designed specifically for tidying
data, not the general reshaping that reshape2 does, or the general aggregation
that reshape did.  In particular, built-in methods only work for data frames,
and tidyr provides no margins or aggregation.")
    (license license:expat)))

(define-public r-hexbin
  (package
    (name "r-hexbin")
    (version "1.28.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hexbin" version))
       (sha256
        (base32
         "1rnk94mjhzfmpcq77iy8qi2j8dj8bdw9vdjmwm27s94z77b8gl1f"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice))
    (native-inputs
     (list gfortran r-knitr)) ; for vignettes
    (home-page "https://github.com/edzer/hexbin")
    (synopsis "Hexagonal binning routines")
    (description
     "This package provides binning and plotting functions for hexagonal bins.
It uses and relies on grid graphics and formal (S4) classes and methods.")
    (license license:gpl2+)))

(define-public r-purrr
  (package
    (name "r-purrr")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "purrr" version))
       (sha256
        (base32
         "0m4fkd047z0p7pd0vp819h6x6n7rmrmi53kvdbjslp8wclj3f0bc"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cli r-lifecycle r-magrittr r-rlang r-vctrs))
    (native-inputs
     (list r-knitr r-lubridate r-testthat r-tibble r-tidyselect))
    (home-page "https://github.com/hadley/purrr")
    (synopsis "Functional programming tools")
    (description
     "This package completes R's functional programming tools with missing
features present in other programming languages.")
    (license license:gpl3+)))

(define-public r-plotly
  (package
    (name "r-plotly")
    (version "4.10.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "plotly" version))
              (sha256
               (base32
                "0ryqcs9y7zan36zs6n1hxxy91pajldpax8q7cwcimlsmxnvrbafg"))
              (modules '((guix build utils)))
              (snippet
               '(with-directory-excursion "inst/htmlwidgets/lib/"
                  (for-each delete-file
                            '("plotlyjs/plotly-latest.min.js"
                              "colourpicker/colourpicker.min.js"
                              "typedarray/typedarray.min.js"
                              "selectize/selectize.min.js"))))))
    (build-system r-build-system)
    (arguments
     (list
      ;; Tests require internet access.
      #:tests? #false
      #:modules '((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/htmlwidgets/lib/"
               (symlink (string-append (assoc-ref inputs "js-selectize")
                                       "/share/javascript/selectize.min.js")
                        "selectize/selectize.min.js")
               (call-with-values
                   (lambda ()
                     (unzip2
                      `((,(assoc-ref inputs "js-plotly")
                         "plotlyjs/plotly-latest.min.js")
                        (,(string-append (assoc-ref inputs "js-colourpicker")
                                         "/js/colourpicker.js")
                         "colourpicker/colourpicker.min.js")
                        (,(string-append (assoc-ref inputs "js-typedarray")
                                         "/typedarray.js")
                         "typedarray/typedarray.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #t "Processing ~a --> ~a~%"
                                       source target)
                               (invoke "esbuild" source "--minify"
                                       (string-append "--outfile=" target)))
                             sources targets)))))))))
    (propagated-inputs
     (list r-base64enc
           r-crosstalk
           r-data-table
           r-digest
           r-dplyr
           r-ggplot2
           r-htmltools
           r-htmlwidgets
           r-httr
           r-jsonlite
           r-lazyeval
           r-magrittr
           r-promises
           r-purrr
           r-rcolorbrewer
           r-rlang
           r-scales
           r-tibble
           r-tidyr
           r-vctrs
           r-viridislite))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("js-colourpicker"
        ,(let ((commit "27c2a266d51e18a9fe6d7542264152b27c7d34e0")
               (version "1.1")
               (revision "0"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/daattali/jquery-colourpicker")
                   (commit commit)))
             (file-name (git-file-name "jquery-colourpicker"
                                       (git-version version revision commit)))
             (sha256
              (base32
               "0lg8amh8xh6p246j38rqghrljd7v5z34i169ra6403z8ga33wiqb")))))
       ("js-plotly"
        ,(let ((version "2.11.1"))
           (origin
             (method url-fetch)
             (uri (string-append "https://raw.githubusercontent.com/plotly/plotly.js/v"
                                 version "/dist/plotly.js"))
             (sha256
              (base32
               "1mxd8s4v3i885w5i02cyzqsrvqfr9w0svdclvqxbd05dly4bdkbj")))))
       ("js-selectize" ,js-selectize)
       ;; This is not quite the same as the bundled minified script from 2016,
       ;; but it seems to be the original with fixes from late 2017.
       ("js-typedarray"
        ,(let ((commit "9f7d4168657e2c164d647a6959f402f2c33eb5b4")
               (version "0")
               (revision "0"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/inexorabletash/polyfill/")
                   (commit commit)))
             (file-name (git-file-name "typedarray-polyfill"
                                       (git-version version revision commit)))
             (sha256
              (base32
               "0f9np4mmyhny03n3xpwzs07rld30lnfqsnh97x1v7xm0qy0zjanf")))))))
    (home-page "https://plot.ly/r")
    (synopsis "Create interactive web graphics")
    (description
     "This package enables the translation of ggplot2 graphs to an interactive
web-based version and/or the creation of custom web-based visualizations
directly from R.  Once uploaded to a plotly account, plotly graphs (and the
data behind them) can be viewed and modified in a web browser.")
    (license license:x11)))

(define-public r-rematch
  (package
    (name "r-rematch")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rematch" version))
       (sha256
        (base32
         "09jwg3glp32q4ml4khhzi8j7bzg9zhqqdg1m6r8gibh756zzgnhm"))))
    (build-system r-build-system)
    (native-inputs (list r-testthat))
    (home-page "https://github.com/MangoTheCat/rematch")
    (synopsis "Match regular expressions with a nicer API")
    (description
     "This package provides a small wrapper on @code{regexpr} to extract the
matches and captured groups from the match of a regular expression to a
character vector.")
    (license license:expat)))

(define-public r-cellranger
  (package
    (name "r-cellranger")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cellranger" version))
       (sha256
        (base32
         "16fgi3annn34c3cxi0pxf62mmmmxi21hp0zzlv7bkfsjqy4g4f2x"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rematch r-tibble))
    (native-inputs (list r-knitr r-testthat))
    (home-page "https://github.com/rsheets/cellranger")
    (synopsis "Translate spreadsheet cell ranges to rows and columns")
    (description
     "This package provides helper functions to work with spreadsheets and the
@code{A1:D10} style of cell range specification.")
    (license license:expat)))

(define-public r-googlesheets
  (package
    (name "r-googlesheets")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "googlesheets" version))
       (sha256
        (base32 "13n6vkdcbz78sbxq5xrj0zhq0dpriz6cyq13v3raa92152l88rm5"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cellranger
           r-dplyr
           r-httr
           r-jsonlite
           r-purrr
           r-readr
           r-stringr
           r-tibble
           r-tidyr
           r-xml2))
    (native-inputs (list r-knitr r-testthat))
    (home-page "https://github.com/jennybc/googlesheets")
    (synopsis "Manage Google spreadsheets from R")
    (description "This package provides tools to interact with Google Sheets
from within R.")
    (license license:expat)))

(define-public r-spams
  (package
    (name "r-spams")
    (version "2.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.inria.fr/thoth/spams-devel")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qvj87fw4sm54c7dvhxjgmgvnyyrrz9fk6dqp3ak0gwgb42gqh60"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-generated-file-shebangs 'patch-paths
            (lambda _
              (substitute* (cons* "swig/setRelease"
                                  "swig/R/docmatlab2R"
                                  "swig/R/mkdist"
                                  (find-files "." "(^mk|\\.sh$)"))
                (("/bin/pwd") "pwd")
                (("/bin/rm") "rm"))))
          (add-after 'patch-paths 'mkdist
            (lambda _
              (chdir "swig/R")
              (setenv "TEXINPUTS" (string-append (getcwd) "/../../doc:"))
              (substitute* "mkdist"
                (("^SWIG=.*")
                 (string-append "SWIG=" (which "swig")))
                (("^../mkdoc") ""))     ;requires texlive-aeguill
              (invoke "./mkdist")
              (chdir "dist/spams-R/spams")))
          ;; Don't tune for the building machine.
          (add-after 'mkdist 'no-mtune
            (lambda _
              (substitute* "src/Makevars"
                (("-mtune=native") ""))))
          (add-after 'no-mtune 'use-c++11
            (lambda _
              (substitute* "src/Makevars"
                (("-DUSE_BLAS_LIB" m)
                 (string-append m " -std=c++11"))))))))
    (native-inputs
     (list hevea
           perl
           swig
           (texlive-updmap.cfg
            (list texlive-aeguill texlive-jknapltx))))
    (propagated-inputs
     (list r-lattice
           r-matrix))
    (home-page "https://gitlab.inria.fr/thoth/spams-devel/")
    (synopsis "Toolbox for solving sparse estimation problems")
    (description "SPAMS (SPArse Modeling Software) is an optimization toolbox
for solving various sparse estimation problems.  It includes tools for the
following problems:

@enumerate
@item Dictionary learning and matrix factorization (NMF, sparse @dfn{principle
 component analysis} (PCA), ...)
@item Solving sparse decomposition problems with LARS, coordinate descent,
 OMP, SOMP, proximal methods
@item Solving structured sparse decomposition problems (l1/l2, l1/linf, sparse
 group lasso, tree-structured regularization, structured sparsity with
 overlapping groups,...).
@end enumerate\n")
    (license license:gpl3+)))

(define-public r-hmisc
  (package
    (name "r-hmisc")
    (version "5.2-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Hmisc" version))
       (sha256
        (base32 "0z09b1nx1q1ayw9wnvvqsb8jnxrkb0hac4m1p7rdibg09ypj2ln5"))))
    (properties `((upstream-name . "Hmisc")))
    (build-system r-build-system)
    ;; Tests need r-rms, which needs this package.
    (arguments (list #:test-types '(list "vignettes")))
    (native-inputs
     (list gfortran))
    (propagated-inputs
     (list r-base64enc
           r-cluster
           r-colorspace
           r-data-table
           r-foreign
           r-formula
           r-ggplot2
           r-gridextra
           r-gtable
           r-htmltable
           r-htmltools
           r-knitr
           r-nnet
           r-rmarkdown
           r-rpart
           r-viridis))
    (home-page "http://biostat.mc.vanderbilt.edu/Hmisc")
    (synopsis "Miscellaneous data analysis and graphics functions")
    (description
     "This package contains many functions useful for data analysis,
high-level graphics, utility operations, functions for computing sample size
and power, importing and annotating datasets, imputing missing values,
advanced table making, variable clustering, character string manipulation,
conversion of R objects to LaTeX code, and recoding variables.")
    (license license:gpl2+)))

(define-public r-runit
  (package
    (name "r-runit")
    (version "0.4.33")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RUnit" version))
       (sha256
        (base32
         "0pybwvd57vf71vvlxdrynw5n6s5gnbqnwvq0qpd395ggqypwb95j"))))
    (properties `((upstream-name . "RUnit")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/RUnit")
    (synopsis "R unit test framework")
    (description
     "This package provides R functions implementing a standard unit testing
framework, with additional code inspection and report generation tools.")
    (license license:gpl2+)))

(define-public r-dynamictreecut
  (package
    (name "r-dynamictreecut")
    (version "1.63-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dynamicTreeCut" version))
       (sha256
        (base32
         "1fadbql7g5r2vvlkr89nlrjxwp4yx4xrdqmv077qvmnx9vv0f4w3"))))
    (properties `((upstream-name . "dynamicTreeCut")))
    (build-system r-build-system)
    (home-page
     "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/BranchCutting/")
    (synopsis "Detect clusters in hierarchical clustering dendrograms")
    (description
     "This package contains methods for the detection of clusters in
hierarchical clustering dendrograms.")
    (license license:gpl2+)))

(define-public r-fastcluster
  (package
    (name "r-fastcluster")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fastcluster" version))
       (sha256
        (base32
         "06cd3w62ijf0yx2yq0xgx6qw5lcrnn1033ygx6bl9dmhix2haal5"))))
    (build-system r-build-system)
    (home-page "http://danifold.net/fastcluster.html")
    (synopsis "Fast hierarchical clustering routines")
    (description
     "This package implements fast hierarchical, agglomerative clustering
routines.  Part of the functionality is designed as drop-in replacement for
existing routines: @code{linkage()} in the SciPy package
@code{scipy.cluster.hierarchy}, @code{hclust()} in R's @code{stats} package,
and the @code{flashClust} package.  It provides the same functionality with
the benefit of a much faster implementation.  Moreover, there are
memory-saving routines for clustering of vector data, which go beyond what the
existing packages provide.")
    (license license:bsd-2)))

(define-public r-sfsmisc
  (package
    (name "r-sfsmisc")
    (version "1.1-20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sfsmisc" version))
       (sha256
        (base32
         "0svpqdcwq62y5d2ywcdrqn1lpq1jvfqx9mxl0dxxa08whahhyqs4"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/sfsmisc")
    (synopsis "Utilities from \"Seminar fuer Statistik\" ETH Zurich")
    (description
     "This package provides useful utilities from Seminar fuer Statistik ETH
Zurich, including many that are related to graphics.")
    (license license:gpl2+)))

(define-public r-gtools
  (package
    (name "r-gtools")
    (version "3.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gtools" version))
       (sha256
        (base32
         "0jqpvygxgr4m1rgaj5rzd048fwxvpcr4n1vx8bfabc9d2p0vdsfy"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/gtools")
    (synopsis "Various R programming tools")
    (description
     "This package contains a collection of various functions to assist in R
programming, such as tools to assist in developing, updating, and maintaining
R and R packages, calculating the logit and inverse logit transformations,
tests for whether a value is missing, empty or contains only @code{NA} and
@code{NULL} values, and many more.")
    (license license:gpl2)))

(define-public r-gdata
  (package
    (name "r-gdata")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gdata" version))
       (sha256
        (base32
         "0qiqfrk0qi8k1f6kvfkgp43algjdgfx6w5xmqrpyzh535gfrcfnv"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gtools))
    (native-inputs (list r-runit))
    (home-page "https://cran.r-project.org/web/packages/gdata")
    (synopsis "Various R programming tools for data manipulation")
    (description
     "This package provides various R programming tools for data manipulation,
including:

@itemize
@item medical unit conversions
@item combining objects
@item character vector operations
@item factor manipulation
@item obtaining information about R objects
@item generating fixed-width format files
@item extricating components of date and time objects
@item operations on columns of data frames
@item matrix operations
@item operations on vectors and data frames
@item value of last evaluated expression
@item wrapper for @code{sample} that ensures consistent behavior for
  both scalar and vector arguments
@end itemize\n")
    (license license:gpl2+)))

(define-public r-gplots
  (package
    (name "r-gplots")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gplots" version))
       (sha256
        (base32
         "0isgbzsj0zkbzp5zvcz4nkc70rm33pcvcmfd6jiiqiw7nilirqqj"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-catools r-gtools r-kernsmooth))
    (native-inputs
     (list r-knitr))
    (home-page "https://cran.r-project.org/web/packages/gplots")
    (synopsis "Various R programming tools for plotting data")
    (description
     "This package provides various R programming tools for plotting data,
including:

@itemize
@item calculating and plotting locally smoothed summary function
@item enhanced versions of standard plots
@item manipulating colors
@item calculating and plotting two-dimensional data summaries
@item enhanced regression diagnostic plots
@item formula-enabled interface to @code{stats::lowess} function
@item displaying textual data in plots
@item balloon plots
@item plotting \"Venn\" diagrams
@item displaying Open-Office style plots
@item plotting multiple data on same region, with separate axes
@item plotting means and confidence intervals
@item spacing points in an x-y plot so they don't overlap
@end itemize\n")
    (license license:gpl2+)))

(define-public r-rocr
  (package
    (name "r-rocr")
    (version "1.0-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ROCR" version))
       (sha256
        (base32
         "0amvvrkiflmr3qygrsgrsja4gaf2v6r6h6i2bgpsm8r069vmlf2p"))))
    (properties `((upstream-name . "ROCR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gplots))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://rocr.bioinf.mpi-sb.mpg.de/")
    (synopsis "Visualizing the performance of scoring classifiers")
    (description
     "ROCR is a flexible tool for creating cutoff-parameterized 2D performance
curves by freely combining two from over 25 performance measures (new
performance measures can be added using a standard interface).  Curves from
different cross-validation or bootstrapping runs can be averaged by different
methods, and standard deviations, standard errors or box plots can be used to
visualize the variability across the runs.  The parameterization can be
visualized by printing cutoff values at the corresponding curve positions, or
by coloring the curve according to cutoff.  All components of a performance
plot can be quickly adjusted using a flexible parameter dispatching
mechanism.")
    (license license:gpl2+)))

(define-public r-vipor
  (package
    (name "r-vipor")
    (version "0.4.7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "vipor" version))
              (sha256
               (base32
                "17hb6y1i9bva0fr4k9m6wncmnzdjad1b7fhsvfhva4xavpll3bds"))))
    (build-system r-build-system)
    ;; Vignettes need r-beeswarm and r-ggbeeswarm, leading to a dependency
    ;; cycle.
    (arguments (list #:test-types '(list "tests")))
    (native-inputs (list r-testthat))
    (home-page "https://cran.r-project.org/web/packages/vipor")
    (synopsis "Plot categorical data using noise and density estimates")
    (description
     "This package provides tools to generate a violin point plot, a
combination of a violin/histogram plot and a scatter plot by offsetting points
within a category based on their density using quasirandom noise.")
    (license license:gpl2+)))

(define-public r-sourcetools
  (package
    (name "r-sourcetools")
    (version "0.1.7-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sourcetools" version))
       (sha256
        (base32
         "1l9i9ram12pjks8h2gzmj119wf4ixwyhljsfv289dn8dgbdjp0cn"))))
    (build-system r-build-system)
    (native-inputs (list r-testthat))
    (home-page "https://cran.r-project.org/web/packages/sourcetools")
    (synopsis "Tools for reading, tokenizing and parsing R code")
    (description
     "The sourcetools package provides both an R and C++ interface for the
tokenization of R code, and helpers for interacting with the tokenized
representation of R code.")
    (license license:expat)))

(define-public r-ggbeeswarm
  (package
    (name "r-ggbeeswarm")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggbeeswarm" version))
              (sha256
               (base32
                "1rcw54isai05np4fj19vcxx2vcxq7y2nm3az9m8xwbc9pdjs4z7x"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beeswarm r-cli r-ggplot2 r-lifecycle r-vipor))
    (home-page "https://github.com/eclarke/ggbeeswarm")
    (synopsis "Categorical scatter (violin point) plots")
    (description
     "This package provides two methods of plotting categorical scatter plots
such that the arrangement of points within a category reflects the density of
data at that region, and avoids over-plotting.")
    (license license:gpl2+)))

(define-public r-ggthemes
  (package
    (name "r-ggthemes")
    (version "5.1.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggthemes" version))
              (sha256
               (base32
                "03mxigi34nd05wjigm8lpcbyhp4c8jxz7zm04qs27clbzsn1jj07"))))
    (properties
     '((updater-extra-native-inputs . ("r-dplyr"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2
           r-lifecycle
           r-purrr
           r-scales
           r-stringr
           r-tibble))
    (native-inputs (list r-dplyr r-testthat))
    (home-page "https://cran.rstudio.com/web/packages/ggthemes")
    (synopsis "Extra themes, scales and geoms for @code{ggplot2}")
    (description "This package provides extra themes and scales for
@code{ggplot2} that replicate the look of plots by Edward Tufte and
Stephen Few in Fivethirtyeight, The Economist, Stata, Excel, and The
Wall Street Journal, among others.  This package also provides
@code{geoms} for Tufte's box plot and range frame.")
    (license license:gpl2)))

(define-public r-statmod
  (package
    (name "r-statmod")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "statmod" version))
              (sha256
               (base32
                "1jbf07h0mnncn2qp4wcw5bnbvsw3lizpd3cg0cpb8mcxn3wkw76n"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/statmod")
    (native-inputs
     (list gfortran))
    (synopsis "Statistical modeling")
    (description
     "This package provides a collection of algorithms and functions to aid
statistical modeling.  It includes growth curve comparisons, limiting dilution
analysis (aka ELDA), mixed linear models, heteroscedastic regression,
inverse-Gaussian probability calculations, Gauss quadrature and a secure
convergence algorithm for nonlinear models.  It also includes advanced
generalized linear model functions that implement secure convergence,
dispersion modeling and Tweedie power-law families.")
    ;; Statmod is distributed under either license
    (license (list license:gpl2 license:gpl3))))

(define-public r-rann
  (package
    (name "r-rann")
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RANN" version))
              (sha256
               (base32
                "0wlpkf6vilw871ac2bxy5nr790cfs6sax5k8m0fhasxbansxbxvf"))))
    (properties
     `((upstream-name . "RANN")))
    (build-system r-build-system)
    (native-inputs (list r-testthat))
    (home-page "https://github.com/jefferis/RANN")
    (synopsis "Fast nearest neighbour search")
    (description
     "This package finds the k nearest neighbours for every point in a given
dataset in O(N log N) time using Arya and Mount's ANN library.  Provides
approximate, exact searches, fixed radius searches, bd and kb trees.")
    (license license:gpl3+)))

(define-public r-fivethirtyeight
  (package
    (name "r-fivethirtyeight")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mran.microsoft.com/src/contrib/"
                           "fivethirtyeight_" version ".tar.gz"))
       (sha256
        (base32
         "0fcc8rq745nsghp27dk0lgih90y4zx8hrzcvsn6ih786yv7qxhvl"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-knitr
           r-rmarkdown
           r-dplyr
           r-readr
           r-ggplot2
           r-magrittr
           r-stringr))
    (home-page "https://mran.microsoft.com/package/fivethirtyeight/")
    (synopsis "Data and code behind the stories at FiveThirtyEight")
    (description "This R package provides access to the code and data sets
published by the statistics blog FiveThirtyEight.")
    (license license:expat)))

(define-public r-compquadform
  (package
    (name "r-compquadform")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "CompQuadForm" version))
       (sha256
        (base32
         "1i30hrqdk64q17vsn918c3q79brchgx2wzh1gbsgbn0dh1ncabq4"))))
    (properties `((upstream-name . "CompQuadForm")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/CompQuadForm")
    (synopsis "Distribution function of quadratic forms in normal variables")
    (description
     "This package provides functions to compute the distribution function of
quadratic forms in normal variables using Imhof's method, Davies's algorithm,
Farebrother's algorithm or Liu et al.'s algorithm.")
    (license license:gpl2+)))

(define-public r-cowplot
  (package
    (name "r-cowplot")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cowplot" version))
       (sha256
        (base32
         "0wxjynpbamyimpms7psbac7xgwswzlidczpc037q20y5yld9fml7"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2 r-gtable r-rlang r-scales))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://github.com/wilkelab/cowplot")
    (synopsis "Streamlined plot theme and plot annotations for ggplot2")
    (description
     "This package provides some helpful extensions and modifications to the
ggplot2 package to combine multiple ggplot2 plots into one and label them with
letters, as is often required for scientific publications.")
    (license license:gpl2)))

(define-public r-mixtools
  (package
    (name "r-mixtools")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mixtools" version))
       (sha256
        (base32
         "03by64xi7yka0hzc1qyz1jdqzah6isvr3cdsc7g5v6hb4f178kl5"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-kernlab
           r-mass
           r-plotly
           r-scales
           r-segmented
           r-survival))
    (home-page "https://cran.r-project.org/web/packages/mixtools")
    (synopsis "Tools for analyzing finite mixture models")
    (description
     "This package provides a collection of R functions for analyzing finite
mixture models.")
    (license license:gpl2+)))

(define-public r-lars
  (package
    (name "r-lars")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lars" version))
       (sha256
        (base32
         "17al1g0pvfz9frs2kxicqr8dyp7ciz3x87yx2l4lqd53ls6nm7n6"))))
    (build-system r-build-system)
    (inputs
     (list gfortran))
    (home-page "https://web.stanford.edu/~hastie/Papers/LARS/")
    (synopsis "Least angle regression software")
    (description
     "Least Angle Regression (\"LAR\") is a model selection algorithm; a
useful and less greedy version of traditional forward selection methods.  A
simple modification of the LAR algorithm implements Tibshirani's Lasso; the
Lasso modification of LARS calculates the entire Lasso path of coefficients
for a given problem at the cost of a single least squares fit.  Another LARS
modification efficiently implements epsilon Forward Stagewise linear
regression.")
    (license license:gpl2)))

(define-public r-fastica
  (package
    (name "r-fastica")
    (version "1.2-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fastICA" version))
       (sha256
        (base32
         "08cw1aszsi4i8asfrzyyz66wl70g50y1dndmf8j8kqvhw1r6kdxw"))))
    (properties `((upstream-name . "fastICA")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/fastICA")
    (synopsis "FastICA algorithms to perform ICA and projection pursuit")
    (description
     "This package provides an implementation of the FastICA algorithm to
perform @dfn{independent component analysis} (ICA) and projection pursuit.")
    ;; Any GPL version.
    (license license:gpl3+)))

(define-public r-randomforest
  (package
    (name "r-randomforest")
    (version "4.7-1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "randomForest" version))
       (sha256
        (base32
         "04aan0wzkfdl2nh4z8fls7qhgz80xl7px7c4282dbfj8nqkk5lxa"))))
    (properties `((upstream-name . "randomForest")))
    (build-system r-build-system)
    (home-page "https://www.stat.berkeley.edu/~breiman/RandomForests/")
    (native-inputs
     (list gfortran))
    (synopsis "Breiman and Cutler's random forests for classification and regression")
    (description
"This package provides the Breiman and Cutler's random forests algorithm, based on a
forest of trees using random inputs, for classification and regression.")
    (license license:gpl2+)))

(define-public r-diptest
  (package
    (name "r-diptest")
    (version "0.77-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "diptest" version))
       (sha256
        (base32
         "04r8c6xy5vympxaxmgnjib4pqh97f9k6b43i66qhzkl3yh0awki2"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/diptest")
    (synopsis "Hartigan's dip test statistic for unimodality")
    (description
     "This package computes Hartigan's dip test statistic for unimodality,
multimodality and provides a test with simulation based p-values, where the
original public code has been corrected.")
    (license license:gpl2+)))

(define-public r-modeltools
  (package
    (name "r-modeltools")
    (version "0.2-23")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "modeltools" version))
       (sha256
        (base32
         "1vqh69256h344sbj5affm0kmc77dakrxp6442xfdnfd0y5d8sgkb"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/modeltools")
    (synopsis "Tools and classes for statistical models")
    (description
     "This package provides a collection of tools to deal with statistical
models.  The functionality is experimental and the user interface is likely
to change in the future.")
    (license license:gpl2)))

(define-public r-flexmix
  (package
    (name "r-flexmix")
    (version "2.3-19")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flexmix" version))
       (sha256
        (base32
         "1s8jpii6ws6hra8w11fn3x84wrdrrlycnrhn5ijy6ibdpc6a9xdd"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice r-modeltools r-nnet))
    (home-page "https://cran.r-project.org/web/packages/flexmix")
    (synopsis "Flexible mixture modeling")
    (description
     "This package implements a general framework for finite mixtures of
regression models using the EM algorithm.  FlexMix provides the E-step and
all data handling, while the M-step can be supplied by the user to easily
define new models.  Existing drivers implement mixtures of standard linear
models, generalized linear models and model-based clustering.")
    (license license:gpl2+)))

(define-public r-mclust
  (package
    (name "r-mclust")
    (version "6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mclust" version))
       (sha256
        (base32
         "0xb726ssy224mzfg37nq5hffkdwi2hwjp1y9gwngk9vfbs703myx"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran r-knitr))
    (home-page "https://www.stat.washington.edu/mclust/")
    (synopsis "Gaussian mixture modelling for model-based clustering etc.")
    (description
     "This package provides Gaussian finite mixture models fitted via EM
algorithm for model-based clustering, classification, and density estimation,
including Bayesian regularization, dimension reduction for visualisation,
and resampling-based inference.")
    (license license:gpl2+)))

(define-public r-prabclus
  (package
    (name "r-prabclus")
    (version "2.3-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "prabclus" version))
       (sha256
        (base32
         "0wmcw0iz0a4q0x83v36ldv8b67czcpw3g18v68i3v9284rbj6xnd"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mass r-mclust))
    (native-inputs (list r-spatialreg r-spdep))
    (home-page "https://cran.r-project.org/web/packages/prabclus")
    (synopsis "Parametric bootstrap tests for spatial neighborhood clustering")
    (description
     "This package provides distance-based parametric bootstrap tests for
clustering with spatial neighborhood information.  It implements some distance
measures, clustering of presence-absence, abundance and multilocus genetical
data for species delimitation, nearest neighbor based noise detection.")
    (license license:gpl2+)))

(define-public r-deoptimr
  (package
    (name "r-deoptimr")
    (version "1.1-3-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DEoptimR" version))
       (sha256
        (base32
         "18kjq2gcqnicmbdpg5pkzsa4wvy20fprqdkh115k34l6pm176ssq"))))
    (properties `((upstream-name . "DEoptimR")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/DEoptimR")
    (synopsis "Differential evolution optimization in pure R")
    (description
     "This package provides a differential evolution (DE) stochastic
algorithms for global optimization of problems with and without constraints.
The aim is to curate a collection of its state-of-the-art variants that (1) do
not sacrifice simplicity of design, (2) are essentially tuning-free, and (3)
can be efficiently implemented directly in the R language.")
    (license license:gpl2+)))

(define-public r-robustbase
  (package
    (name "r-robustbase")
    (version "0.99-4-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "robustbase" version))
       (sha256
        (base32
         "14gz260amdy60shm3bfsw214471by27yac6r66fs6rjgc7kxw7j8"))))
    (build-system r-build-system)
    ;; FIXME: test failure
    ;; Error in if (grepl("^Fedora", osVersion) && !is32) identical(i.a4Out,  :
    ;; missing value where TRUE/FALSE needed
    (arguments (list #:tests? #false))
    (native-inputs
     (list gfortran))
    (propagated-inputs
     (list r-deoptimr))
    (home-page "https://robustbase.r-forge.r-project.org/")
    (synopsis "Basic robust statistics")
    (description
     "This package analyzes data with robust methods such as
regression methodology including model selections and multivariate statistics.")
    (license license:gpl2+)))

(define-public r-pcapp
  (package
    (name "r-pcapp")
    (version "2.0-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pcaPP" version))
       (sha256
        (base32
         "043w3g3f57j2d0irnrxm58mzzisg7xmnsl17v7wijsh1czcswkv7"))))
    (properties `((upstream-name . "pcaPP")))
    (build-system r-build-system)
    ;; Vignettes attempt to load the missing file "load.package.name.R".
    (arguments (list #:test-types '(list "tests")))
    (propagated-inputs
     (list r-mvtnorm))
    (home-page "https://cran.r-project.org/web/packages/pcaPP")
    (synopsis "Robust PCA by projection pursuit")
    (description
     "This package provides functions for robust @dfn{principal component
analysis} (PCA) by projection pursuit.")
    (license license:gpl3+)))

(define-public r-rrcov
  (package
    (name "r-rrcov")
    (version "1.7-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rrcov" version))
       (sha256
        (base32
         "1pj8w4w4yd730qy7bl8h3z7d5xj05avnq0fbj2gnxrz489yc18mq"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice r-mvtnorm r-pcapp r-robustbase))
    (native-inputs
     (list gfortran r-mass))
    (home-page "https://cran.r-project.org/web/packages/rrcov")
    (synopsis "Scalable robust estimators with high breakdown Point")
    (description
     "This package provides an implementation of robust location and scatter
estimation and robust multivariate analysis with high breakdown point.")
    (license license:gpl2+)))

(define-public r-fit-models
  (package
    (name "r-fit-models")
    (version "0.64")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fit.models" version))
       (sha256
        (base32
         "1nk4x2q8cv79zcls61saf627ac0fci6jcrd6lmzk61asm2zhc27p"))))
    (properties `((upstream-name . "fit.models")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice))
    (native-inputs (list r-mass))
    (home-page "https://cran.r-project.org/web/packages/fit.models")
    (synopsis "Compare fitted models")
    (description
     "The @code{fit.models} function and its associated methods (coefficients, print,
summary, plot, etc.) were originally provided in the @code{robust} package to
compare robustly and classically fitted model objects.  The aim of the
@code{fit.models} package is to separate this fitted model object comparison
functionality from the robust package and to extend it to support fitting
methods (e.g., classical, robust, Bayesian, regularized, etc.) more
generally.")
    ;; Any version of the GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-robust
  (package
    (name "r-robust")
    (version "0.7-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "robust" version))
       (sha256
        (base32
         "1k0s5i9r0lyz3qsw76dd514qclggqj9k2axmgj86df81j6f2mh53"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-fit-models r-lattice r-mass r-robustbase r-rrcov))
    (native-inputs
     (list gfortran))
    (home-page "https://cran.r-project.org/web/packages/robust")
    (synopsis "Port of the S+ \"Robust Library\"")
    (description
     "This package is a port of the S+ \"Robust Library\".  It provides
methods for robust statistics, notably for robust regression and robust
multivariate analysis.")
    (license license:gpl2)))

(define-public r-trimcluster
  (package
    (name "r-trimcluster")
    (version "0.1-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "trimcluster" version))
       (sha256
        (base32
         "12siv8yx8dcavsz8jk96lwscbj257ar8jpaxksl2zb06987g4fcj"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/trimcluster")
    (synopsis "Cluster analysis with trimming")
    (description
     "The trimmed k-means clustering method by Cuesta-Albertos, Gordaliza and
Matran (1997).  This optimizes the k-means criterion under trimming a portion
of the points.")
    ;; Any GPL version
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-fpc
  (package
    (name "r-fpc")
    (version "2.2-13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fpc" version))
       (sha256
        (base32
         "0irzsl1g71z7kl0qrklkv0rv9byv017lh5klp5hfw3xw8zw3sm2g"))))
    (build-system r-build-system)
    ;; FIXME Test fails with this message:
    ;; attempt to select less than one element in get1index <real>
    (arguments (list #:tests? #false))
    (propagated-inputs
     (list r-class
           r-cluster
           r-diptest
           r-flexmix
           r-kernlab
           r-mass
           r-mclust
           r-prabclus
           r-robustbase))
    (home-page "https://cran.r-project.org/web/packages/fpc")
    (synopsis "Flexible procedures for clustering")
    (description
     "This package provides various methods for clustering and cluster validation.
For example, it provides fixed point clustering, linear regression clustering,
clustering by merging Gaussian mixture components, as well as symmetric and
asymmetric discriminant projections for visualisation of the separation of
groupings.")
  (license license:gpl2+)))

(define-public r-vgam
  (package
    (name "r-vgam")
    (version "1.1-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "VGAM" version))
       (sha256
        (base32 "05pgznwb0bwppq9mslvg197iyp0ln0npapd4r0zc6c2k6ikq4xsa"))))
    (properties `((upstream-name . "VGAM")))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
    (home-page "https://www.stat.auckland.ac.nz/~yee/VGAM")
    (synopsis "Vector generalized linear and additive models")
    (description
    "This package is an implementation of about 6 major classes of statistical
regression models.  Currently only fixed-effects models are implemented, i.e.,
no random-effects models.  Many (150+) models and distributions are estimated
by maximum likelihood estimation (MLE) or penalized MLE, using Fisher scoring.
VGLMs can be loosely thought of as multivariate generalised linear models.")
    (license license:gpl2+)))

(define-public r-pbapply
  (package
    (name "r-pbapply")
    (version "1.7-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbapply" version))
       (sha256
        (base32
         "04xf1p7c0066cwnxfmzaikbc322bxnw022ziv8kkhzlc6268rvdf"))))
    (build-system r-build-system)
    (native-inputs (list r-future))
    (home-page "https://github.com/psolymos/pbapply")
    (synopsis "Adding progress bar to apply functions")
    (description
     "This lightweight package that adds progress bar to vectorized R
functions apply.  The implementation can easily be added to functions where
showing the progress is useful e.g. bootstrap.")
    (license license:gpl2)))

(define-public r-minqa
  (package
    (name "r-minqa")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "minqa" version))
       (sha256
        (base32
         "0rslvg4imaijzb5z6vzsx4zqhrna1jk6qkp2kxnzqy0rn2wy8har"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp))
    (native-inputs
     (list gfortran))
    (home-page "https://optimizer.r-forge.r-project.org")
    (synopsis "Derivative-free optimization algorithms by quadratic approximation")
    (description
      "This package provides a derivative-free optimization by quadratic approximation
based on an interface to Fortran implementations by M. J. D. Powell.")
    (license license:gpl2)))

(define-public r-rcppeigen
  (package
    (name "r-rcppeigen")
    (version "0.3.4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppEigen" version))
       (sha256
        (base32
         "1fs2wmsq2s6nzhkrx59li4x1zvcjhk9mhmc2pdz8pm4z2ai7pbgc"))))
    (properties `((upstream-name . "RcppEigen")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp))
    (home-page "http://eigen.tuxfamily.org")
    (synopsis "Rcpp integration for the Eigen templated linear algebra library")
    (description
      "This package provides an integration of Eigen in R using a C++ template
library for linear algebra: matrices, vectors, numerical solvers and related algorithms.
It supports dense and sparse matrices on integer, floating point and complex numbers,
decompositions of such matrices, and solutions of linear systems.")
    (license license:gpl2+)))

(define-public r-modelmetrics
  (package
    (name "r-modelmetrics")
    (version "1.2.2.2")
    (source
     (origin
       (method url-fetch)
        (uri (cran-uri "ModelMetrics" version))
        (sha256
         (base32
          "0mrlsw4c5y1vdsqynxr2dcvmhh5h37pnd71jw5a5djpbda9g21jy"))))
    (properties `((upstream-name . "ModelMetrics")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-data-table r-rcpp))
    (native-inputs (list r-testthat))
    (home-page "https://cran.r-project.org/web/packages/ModelMetrics")
    (synopsis "Rapid calculation of model metrics")
    (description
     "Written in C++ using @code{Rcpp}, this package provides a collection of
metrics for evaluating models.")
    (license license:gpl2+)))

(define-public r-matrixmodels
  (package
    (name "r-matrixmodels")
    (version "0.5-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "MatrixModels" version))
       (sha256
        (base32
         "03zjfxjk4l2dl1117slz163w0ky675d23sjfni4d7fdhqq359ny2"))))
    (properties `((upstream-name . "MatrixModels")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-matrix))
    (home-page "https://cran.r-project.org/web/packages/MatrixModels")
    (synopsis "Modelling with sparse and dense matrices")
    (description
     "This package models with sparse and dense matrix matrices,
using modular prediction and response module classes.")
    (license license:gpl2+)))

(define-public r-quantreg
  (package
    (name "r-quantreg")
    (version "6.00")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "quantreg" version))
       (sha256
        (base32 "1avx7d2zlpsjlqpqyzagazkz0xwa0varjbl3qq5mcn4kdhk40pwb"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         ;; This is needed for building vignettes
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list gfortran r-r-rsp)) ;for vignettes
    (propagated-inputs
     (list r-mass r-matrix r-matrixmodels r-sparsem r-survival))
    (home-page "https://www.r-project.org")
    (synopsis "Quantile regression")
    (description
     "This package provides an estimation and inference methods for models
of conditional quantiles: linear and nonlinear parametric and non-parametric
models for conditional quantiles of a univariate response and several methods
for handling censored survival data.  Portfolio selection methods based on
expected shortfall risk are also included.")
    (license license:gpl2+)))

(define-public r-nloptr
  (package
    (name "r-nloptr")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nloptr" version))
       (sha256
        (base32
         "1snqvav2pxjhssn1nld49mjj89cn2zv7gjn2y8ch22gbzmfzbnjc"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran pkg-config r-knitr))
    (inputs (list nlopt))
    (home-page "https://cran.r-project.org/web/packages/nloptr")
    (synopsis "R interface to NLopt")
    (description
     "This package is interface to NLopt, a library for nonlinear
optimization.  NLopt is a library for nonlinear optimization, providing a
common interface for a number of different free optimization routines
available online as well as original implementations of various other
algorithms.")
    (license license:lgpl3)))

(define-public r-lme4
  (package
    (name "r-lme4")
    (version "1.1-36")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lme4" version))
       (sha256
        (base32
         "0vyqmbz1zv3xh45ivfjl9dxpr5n8b48g1k70r58952hd994h7q5l"))))
    (build-system r-build-system)
    (properties
     '((updater-ignored-native-inputs . ("r-car" "r-gamm4"))))
    (propagated-inputs
     (list r-boot
           r-lattice
           r-mass
           r-matrix
           r-minqa
           r-nlme
           r-nloptr
           r-rcpp
           r-rcppeigen
           r-reformulas))
    (native-inputs
     (list r-devtools
           r-ggplot2
           r-knitr
           r-mgcv
           r-optimx
           r-statmod
           r-testthat
           r-tibble))
    (home-page "https://cran.r-project.org/web/packages/lme4")
    (synopsis "Linear mixed-effects models using eigen and S4")
    (description
      "This package provides fit linear and generalized linear mixed-effects
models.  The models and their components are represented using S4 classes and
methods.  The core computational algorithms are implemented using the Eigen
C++ library for numerical linear algebra and RcppEigen glue.")
    (license license:gpl2+)))

(define-public r-pbkrtest
  (package
    (name "r-pbkrtest")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbkrtest" version))
       (sha256
        (base32
         "0qxswrk54r40qmi9ky7jzv53mrlr2w2mrlf1czma597nzrb52gmh"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-broom
           r-doby
           r-dplyr
           r-lme4
           r-mass
           r-matrix
           r-numderiv))
    (native-inputs
     (list r-knitr))
    (home-page "https://people.math.aau.dk/~sorenh/software/pbkrtest/")
    (synopsis "Methods for linear mixed model comparison")
    (description
     "This package implements a parametric bootstrap test and a Kenward Roger
modification of F-tests for linear mixed effects models and a parametric
bootstrap test for generalized linear mixed models.")
    (license license:gpl2+)))

(define-public r-cardata
  (package
    (name "r-cardata")
    (version "3.0-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "carData" version))
       (sha256
        (base32 "06j52rpbqi6fj7chfjwjbwbr6slrbb7i3aygv66gnfiyndcp3rq2"))))
    (properties `((upstream-name . "carData")))
    (build-system r-build-system)
    (home-page "https://r-forge.r-project.org/projects/car/")
    (synopsis "Data Sets for the book Companion to Applied Regression")
    (description
     "This package provides datasets to accompany J. Fox and S. Weisberg, An R
Companion to Applied Regression, Third Edition, Sage.")
    (license license:gpl2+)))

(define-public r-car
  (package
    (name "r-car")
    (version "3.1-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "car" version))
       (sha256
        (base32 "1gs9prinmxn4l6w8lyig3mzm1j2zfq3yhkznm1igd5ar74jc1wjr"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-cardata
           r-formula
           r-lme4
           r-mass
           r-mgcv
           r-nlme
           r-nnet
           r-pbkrtest
           r-quantreg
           r-scales))
    (native-inputs
     (list r-knitr))
    (home-page "https://r-forge.r-project.org/projects/car/")
    (synopsis "Companion to applied regression")
    (description
      "This package provides functions and datasets from book Companion
to Applied regression, Second Edition, Sage, 2011.")
    (license license:gpl2+)))

(define-public r-rcppprogress
  (package
    (name "r-rcppprogress")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppProgress" version))
       (sha256
        (base32
         "0j2b37gwjpgmrnr00srdzm751hzlpsjb54ph63xxmcdfnwhlnqmi"))))
    (properties `((upstream-name . "RcppProgress")))
    (build-system r-build-system)
    (native-inputs (list r-testthat))
    (home-page "https://github.com/kforner/rcpp_progress")
    (synopsis "Interruptible progress bar for C++ in R packages")
    (description
     "This package displays a progress bar in the R console for long running
computations taking place in C++ code, and support for interrupting those computations
even in multithreaded code, typically using OpenMP.")
    (license license:gpl3+)))

(define-public r-tmvnsim
  (package
    (name "r-tmvnsim")
    (version "1.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tmvnsim" version))
       (sha256
        (base32
         "03xsvsg9bqvgl98ywid3h91mmlhax5s6wvmypp3hq91vmc5kvxlp"))))
    (properties `((upstream-name . "tmvnsim")))
    (build-system r-build-system)
    (native-inputs (list gfortran))
    (home-page "https://www.r-project.org")
    (synopsis "Truncated multivariate normal simulation")
    (description
     "This package implements importance sampling from the truncated
multivariate normal using the @dfn{Geweke-Hajivassiliou-Keane} (GHK)
simulator.  Unlike Gibbs sampling which can get stuck in one truncation
sub-region depending on initial values, this package allows truncation based
on disjoint regions that are created by truncation of absolute values.  The
GHK algorithm uses simple Cholesky transformation followed by recursive
simulation of univariate truncated normals hence there are also no convergence
issues.  Importance sample is returned along with sampling weights, based on
which, one can calculate integrals over truncated regions for multivariate
normals.")
    (license license:gpl2)))

(define-public r-mnormt
  (package
    (name "r-mnormt")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
        (uri (cran-uri "mnormt" version))
        (sha256
          (base32
           "020ka48jsxm9l109ksvy2l17xcpm51avm0l971dgs2mgg01sgz4m"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
    (home-page "http://azzalini.stat.unipd.it/SW/Pkg-mnormt")
    (synopsis "Multivariate normal and \"t\" distributions")
    (description
     "This package provides functions for computing the density and the
distribution function of multivariate normal and \"t\" random variables, and
for generating random vectors sampled from these distributions.  Probabilities
are computed via non-Monte Carlo methods.")
    (license license:gpl2+)))

(define-public r-numderiv
  (package
    (name "r-numderiv")
    (version "2016.8-1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "numDeriv" version))
       (sha256
        (base32
         "0idk02pqkziik932bd8k72d1q775g1is3m4bc861pcxfz6gx3i6q"))))
    (properties `((upstream-name . "numDeriv")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/numDeriv")
    (synopsis "Accurate numerical derivatives")
    (description
     "This package provides methods for calculating accurate numerical
first and second order derivatives.")
    (license license:gpl2)))

(define-public r-sn
  (package
    (name "r-sn")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sn" version))
       (sha256
        (base32
         "1g92vwbg0kqnqpsgianid2r91334p5shi39hr26v2z6wj5nvbxpr"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mnormt r-numderiv r-quantreg))
    (native-inputs (list r-r-rsp))
    (home-page "http://azzalini.stat.unipd.it/SN")
    (synopsis "The skew-normal and skew-t distributions")
    (description
     "This package provides functionalities to build and manipulate
probability distributions of the skew-normal family and some related
ones, notably the skew-t family, and provides related statistical
methods for data fitting and diagnostics, in the univariate and the
multivariate case.")
    (license license:gpl2+)))

(define-public r-tclust
  (package
    (name "r-tclust")
    (version "2.0-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tclust" version))
       (sha256
        (base32
         "1gx8avqpy5m69h5a3pxy23dwnvy5pbi2ih6sxacs4lmsahpivj0p"))))
    (build-system r-build-system)
    ;; These are all suggested packages, not build dependencies.
    (propagated-inputs
     (list r-doparallel
           r-ellipsis
           r-foreach
           r-mass
           r-rcpp
           r-rcpparmadillo))
    (native-inputs (list r-cluster r-mclust r-sn))
    (home-page "https://cran.r-project.org/web/packages/tclust")
    (synopsis "Robust trimmed clustering")
    (description
     "This package implements different robust clustering
algorithms (@code{tclust}) based on trimming and including some graphical
diagnostic tools (@code{ctlcurves} and @code{DiscrFact}).")
    (license license:gpl3)))

(define-public r-ranger
  (package
    (name "r-ranger")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ranger" version))
       (sha256
        (base32
         "0dlxl1cgxd778gzvnnvnxlwm37q825fw48g2kwyzjyliwi6dz36l"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-matrix r-rcpp r-rcppeigen))
    (native-inputs (list r-survival r-testthat))
    (home-page "https://github.com/imbs-hl/ranger")
    (synopsis "Fast implementation of random forests")
    (description
     "This package provides a fast implementation of Random Forests,
particularly suited for high dimensional data.  Ensembles of classification,
regression, survival and probability prediction trees are supported.  Data from
genome-wide association studies can be analyzed efficiently.")
    (license license:gpl3)))

(define-public r-tsne
  (package
    (name "r-tsne")
    (version "0.1-3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tsne" version))
       (sha256
        (base32
         "126q6ha25wx9mdsfngzkyp8j2fj81ri1knjdq1iyvwx3q1dwdaql"))))
    (build-system r-build-system)
    (home-page "https://github.com/jdonaldson/rtsne/")
    (synopsis "t-Distributed Stochastic Neighbor Embedding for R")
    (description
     "This package provides a pure R implementation of the t-SNE algorithm.")
    (license license:gpl2+)))

(define-public r-cairo
  (package
    (name "r-cairo")
    (version "1.6-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Cairo" version))
       (sha256
        (base32
         "0qjdsj6mpbwvnp8cimc4yrqfq3z68pfcd78ahsai52hpjdplqvvb"))))
    (properties `((upstream-name . "Cairo")))
    (build-system r-build-system)
    (inputs
     (list cairo harfbuzz icu4c libjpeg-turbo libtiff zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.rforge.net/Cairo/")
    (synopsis "R graphics device using Cairo graphics library")
    (description
     "This package provides a Cairo graphics device that can be use to
create high-quality vector (PDF, PostScript and SVG) and bitmap
output (PNG,JPEG,TIFF), and high-quality rendering in displays (X11
and Win32).  Since it uses the same back-end for all output, copying
across formats is WYSIWYG.  Files are created without the dependence
on X11 or other external programs.  This device supports alpha
channel (semi-transparent drawing) and resulting images can contain
transparent and semi-transparent regions.  It is ideal for use in
server environments (file output) and as a replacement for other
devices that don't have Cairo's capabilities such as alpha support or
anti-aliasing.  Backends are modular such that any subset of backends
is supported.")
    (license license:gpl2)))

(define-public r-lubridate
  (package
    (name "r-lubridate")
    (version "1.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lubridate" version))
       (sha256
        (base32
         "1ha68ri36gcq573j7s71m284166qd1ywran62h9d88nn4gi7wjw6"))))
    (build-system r-build-system)
    (properties
     '((updater-extra-native-inputs . ("tzdata-for-tests"))))
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         ;; We need this for one failing test.
         (add-before 'check 'set-timezone
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "LOCALTIME"
                     (search-input-file inputs
                                        "share/zoneinfo/localtime"))
             (setenv "TZ" "UTC")
             (setenv "TZDIR"
                     (search-input-directory inputs
                                             "share/zoneinfo")))))))
    (propagated-inputs
     (list r-generics r-timechange))
    (native-inputs
     (list r-knitr r-testthat r-vctrs tzdata-for-tests))
    (home-page "https://cran.r-project.org/web/packages/lubridate/")
    (synopsis "Make dealing with dates a little easier")
    (description
     "This package provides functions to work with date-times and time-spans:
fast and user friendly parsing of date-time data, extraction and updating of
components of a date-time (years, months, days, hours, minutes, and seconds),
algebraic manipulation on date-time and time-span objects.  The @code{lubridate}
package has a consistent and memorable syntax that makes working with dates
easy and fun.")
    (license license:gpl2)))

(define-public r-fdrtool
  (package
    (name "r-fdrtool")
    (version "1.2.18")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fdrtool" version))
       (sha256
        (base32
         "1vxswr56c63vvl2q6m0j0a5psdw724vspf6zxlr1k9zsdfcnd6b4"))))
    (build-system r-build-system)
    (home-page "https://strimmerlab.org/software/fdrtool/")
    (synopsis "Estimation of false discovery rates and higher criticism")
    (description
     "This package provides tools to estimate tail area-based false discovery
rates as well as local false discovery rates for a variety of null
models (p-values, z-scores, correlation coefficients, t-scores).  The
proportion of null values and the parameters of the null distribution are
adaptively estimated from the data.  In addition, the package contains
functions for non-parametric density estimation (Grenander estimator), for
monotone regression (isotonic regression and antitonic regression with
weights), for computing the @dfn{greatest convex minorant} (GCM) and the
@dfn{least concave majorant} (LCM), for the half-normal and correlation
distributions, and for computing empirical @dfn{higher criticism} (HC) scores
and the corresponding decision threshold.")
    (license license:gpl3+)))

(define-public r-forcats
  (package
    (name "r-forcats")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "forcats" version))
       (sha256
        (base32
         "0ki5c84n7xm4iw4aj629l51hm84f6p5wa3bw88d1wbnr15wibfy5"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cli
           r-glue
           r-lifecycle
           r-magrittr
           r-rlang
           r-tibble))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://forcats.tidyverse.org")
    (synopsis "Tools for working with factors")
    (description "This package provides helpers for reordering factor
levels (including moving specified levels to front, ordering by first
appearance, reversing, and randomly shuffling), and tools for modifying factor
levels (including collapsing rare levels into other, \"anonymizing\", and
manually \"recoding\").")
    (license license:gpl3)))

(define-public r-tgconfig
  (let ((commit "15cf199436ae0b2ac0006b2ca7f0aeeb5c9d4445")
        (revision "1"))
    (package
      (name "r-tgconfig")
      (version (git-version "0.1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tanaylab/tgconfig")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "159m8hhbk9ip2fdy6zsa96v0173q1awcrzdz2rr2796awfaxjgx1"))))
      (properties `((upstream-name . "tgconfig")))
      (build-system r-build-system)
      (propagated-inputs (list r-yaml))
      (native-inputs (list r-testthat))
      (home-page "https://github.com/tanaylab/tgconfig")
      (synopsis "Infrastructure for managing package parameters")
      (description
       "This is a package to provide infrastructure for managing package parameters.
Parameters are easy to get in relevant functions within a package,
and rrror is thrown if a parameter is missing.  Developers are able
to register parameters and set their default value in a config file
that is part of the package in YAML format, and users are able to
override parameters using their own YAML.  Users get an exception
when trying to override a parameter that was not registered, and
can load multiple parameters to the current environment.")
      (license license:gpl3+))))

(define-public r-tgutil
  (let ((commit "db4ff8b98082f8e4dbdeacb452641d215fd3c7ff")
        (revision "1"))
    (package
      (name "r-tgutil")
      (version (git-version "0.1.15" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tanaylab/tgutil")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00rsqs7f896piywh84jr8fkphbbx4jb7radf6znhhj6fip63yn91"))))
      (properties `((upstream-name . "tgutil")))
      (build-system r-build-system)
      (propagated-inputs (list r-broom
                               r-cowplot
                               r-data-table
                               r-dplyr
                               r-ggplot2
                               r-glue
                               r-magrittr
                               r-matrix
                               r-matrixstats
                               r-qlcmatrix
                               r-readr
                               r-rlang
                               r-scales
                               r-tibble
                               r-tidyr))
      (native-inputs (list r-testthat))
      (home-page "https://github.com/tanaylab/tgutil")
      (synopsis "Simple utility functions for Tanay lab code")
      (description
       "This package provides simple utility functions that are shared
across several packages maintained by the Tanay lab.")
      (license license:gpl3))))

(define-public r-catterplots
  (let ((commit "ae17cd5e49ddda4ecfe0eba8a4c21df8c88e72c4")
        (revision "3"))
    (package
      (name "r-catterplots")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Gibbsdavidl/CatterPlots")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qa8liylffpxgdg8xcgjar5dsvczqhn3akd4w35113hnyg1m4xyg"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-png))
      (home-page "https://github.com/Gibbsdavidl/CatterPlots")
      (synopsis "Scatter plots with cat shaped points")
      (description "Did you ever wish you could make scatter plots with cat
shaped points?  Now you can!")
      (license license:asl2.0))))

(define-public r-colorout
  (package
    (name "r-colorout")
    (version "1.2-2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jalvesaq/colorout")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rsx69wjpa73c6x2hacvvvbzdzxn7wg06gizf97kasjdlb7azmp3"))))
    (build-system r-build-system)
    (home-page "https://github.com/jalvesaq/colorout")
    (synopsis "Colorize output in the R REPL")
    (description "@code{colorout} is an R package that colorizes R output when
running in terminal emulator.

R STDOUT is parsed and numbers, negative numbers, dates in the standard
format, strings, and R constants are identified and wrapped by special ANSI
scape codes that are interpreted by terminal emulators as commands to colorize
the output.  R STDERR is also parsed to identify the expressions warning and
error and their translations to many languages.  If these expressions are
found, the output is colorized accordingly; otherwise, it is colorized as
STDERROR (blue, by default).

You can customize the colors according to your taste, guided by the color
table made by the command @code{show256Colors()}.  You can also set the colors
to any arbitrary string.  In this case, it is up to you to set valid values.")
    (license license:gpl3+)))

(define-public r-nnlm
  (let ((commit "4574bca9456fe2285b668b4c22a908cffbad10a0")
        (revision "1"))
    (package
      (name "r-nnlm")
      (version (git-version "0.4.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/linxihui/NNLM")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qzvav4ch0j1k7jcqzk9cvl8dx79fapmvdzmzzyl8smscybhfgba"))))
      (properties `((upstream-name . "NNLM")))
      (build-system r-build-system)
      (propagated-inputs (list r-rcpp r-rcpparmadillo r-rcppprogress))
      (native-inputs (list r-knitr r-testthat))
      (home-page "https://github.com/linxihui/NNLM")
      (synopsis "Fast and versatile non-negative matrix factorization")
      (description
       "This is a package for @dfn{Non-Negative Linear Models} (NNLM).  It
implements fast sequential coordinate descent algorithms for non-negative
linear regression and @dfn{non-negative matrix factorization} (NMF).  It
supports mean square error and Kullback-Leibler divergence loss.  Many other
features are also implemented, including missing value imputation, domain
knowledge integration, designable W and H matrices and multiple forms of
regularizations.")
      (license license:bsd-2))))

(define-public r-swne
  (let ((commit "05fc3ee4e09b2c34d99c69d3b97cece4c1c34143")
        (revision "1"))
    (package
      (name "r-swne")
      (version (git-version "0.6.20" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yanwu2014/swne")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0crlpg9kclbv4v8250p3086a3lk6f2hcq79psqkdylc1qnrx3kfx"))))
      (properties `((upstream-name . "swne")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-fnn
             r-ggplot2
             r-ggrepel
             r-hash
             r-ica
             r-igraph
             r-irlba
             r-jsonlite
             r-liger
             r-mass
             r-matrix
             r-mgcv
             r-nnlm ;not listed but required at install time
             r-plyr
             r-proxy
             r-rcolorbrewer
             r-rcpp
             r-rcpparmadillo
             r-rcppeigen
             r-reshape
             r-reshape2
             r-snow
             r-umap
             r-usedist))
      (home-page "https://github.com/yanwu2014/swne")
      (synopsis "Visualize high dimensional datasets")
      (description
       "@dfn{Similarity Weighted Nonnegative Embedding} (SWNE) is a method for
visualizing high dimensional datasets.  SWNE uses Nonnegative Matrix
Factorization to decompose datasets into latent factors, projects those
factors onto 2 dimensions, and embeds samples and key features in 2 dimensions
relative to the factors.  SWNE can capture both the local and global dataset
structure, and allows relevant features to be embedded directly onto the
visualization, facilitating interpretation of the data.")
      (license license:gpl2))))

(define-public r-languageserver
  (let ((commit "004da9388f9b19990f031c8dc9b527fb406378ba")
        (revision "1"))
    (package
      (name "r-languageserver")
      (version (git-version "0.3.12" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/REditorSupport/languageserver")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "05f22bjpc87fngzq4jsk2q2yb2i3ha03b377r0wx15d0b8xaa1ix"))))
      (properties `((upstream-name . "languageserver")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-callr
             r-collections
             r-fs
             r-jsonlite
             r-lintr
             r-r6
             r-roxygen2
             r-stringi
             r-styler
             r-xml2
             r-xmlparsedata))
      (native-inputs (list r-mockery r-testthat))
      (home-page "https://github.com/REditorSupport/languageserver")
      (synopsis "Language Server for R")
      (description
       "This package provides an implementation of the Language Server
Protocol for R.  The
@url{https://microsoft.github.io/language-server-protocol/,Language Server
protocol} is used by an editor client to integrate features like auto
completion.")
      (license license:expat))))

(define-public python-rpy2
  (package
    (name "python-rpy2")
    (version "3.5.17")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rpy2" version))
        (sha256
         (base32
          "10nmydlbmi0vyim7sx71isx3z2mnnfjmhf3248cicy9x1z1hizyv"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-cffi
           python-six
           python-jinja2
           python-numpy
           python-pandas
           python-pytz
           python-ipython
           python-tzlocal))
    (inputs
     (list icu4c
           libdeflate
           pcre
           python-numpy
           readline
           r-minimal
           r-survival
           r-ggplot2
           r-rsqlite
           r-dplyr
           r-dbplyr
           zlib))
    (native-inputs
     (list python-coverage
           python-ipython
           python-numpy
           python-pandas
           python-pytest
           python-pytest-cov
           python-setuptools
           python-wheel))
    (home-page "https://rpy2.github.io")
    (synopsis "Python interface to the R language")
    (description "rpy2 is a redesign and rewrite of rpy.  It is providing a
low-level interface to R from Python, a proposed high-level interface,
including wrappers to graphical libraries, as well as R-like structures and
functions.")
    ;; Any of these licenses can be picked for the R interface.  The whole
    ;; project is released under GPLv2+ according to the license declaration
    ;; in "setup.py".
    (license (list license:mpl2.0 license:gpl2+ license:lgpl2.1+))))

(define-public python-rchitect
  (package
    (name "python-rchitect")
    (version "0.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/randy3k/rchitect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ijbb0v77ir7j64r4r4kznv03wyc57rcqa9jnsc46476il79dcrk"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cffi python-packaging python-six))
    (native-inputs (list python-pytest
                         python-pytest-cov
                         python-pytest-mock
                         python-setuptools
                         python-wheel
                         ;; R dependencies needed only for testing.
                         r-minimal
                         r-reticulate))
    (home-page "https://github.com/randy3k/rchitect")
    (synopsis "Mapping R API to Python")
    (description
     "rchitect provides access to R functionality from Python.  Its
main use is as the driver for radian, the R console.")
    (license license:expat)))

(define-public python-radian
  (package
    (name "python-radian")
    (version "0.6.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/randy3k/radian")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nnwgvifhsxdac7rr9d2zspc97xx0vyzxn1v9g4bnm9061rragc3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'set-home
                     (lambda _
                       ;; During tests radian wants to write history files to
                       ;; $HOME which causes tests to fail when that does not
                       ;; exist. Test fails then look like
                       ;; "Exception: value is " with the value being empty.
                       (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list python-prompt-toolkit python-pygments
                             python-rchitect))
    (native-inputs (list python-coverage
                         python-pexpect
                         python-ptyprocess
                         python-pyte
                         python-pytest
                         python-setuptools
                         python-wheel
                         ;; Needed afaict only for
                         ;; `tests/test_reticulate.py::test_completion`.
                         python-jedi
                         ;; R dependencies needed only for testing.
                         r-askpass
                         r-minimal
                         r-reticulate
                         ;; Needed for sh tests.
                         git-minimal))
    (home-page "https://github.com/randy3k/radian")
    (synopsis "R console")
    (description
     "Radian is an alternative console for the R program with multiline
editing and rich syntax highlight.  One would consider Radian as a IPython
clone for R, though its design is more aligned to Julia.")
    (license license:expat)))

(define-public java-jdistlib
  (package
    (name "java-jdistlib")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/jdistlib/jdistlib-"
                                  version "-src.jar"))
              (sha256
               (base32
                "1pkj8aahw9ydr1isbaqrkd05nvq98ik5jwwhf3yf3rky3z869v11"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jdistlib.jar"
       #:jdk ,icedtea-8
       #:tests? #f ; no dedicated test directory
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-broken-encoding
           (lambda _
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "src/jdistlib/Beta.java"
                 (("Scheff.+-Tukey") "Scheffe-Tukey")))
             #t)))))
    (propagated-inputs
     (list java-jtransforms))
    (native-inputs
     (list java-junit))
    (home-page "https://jdistlib.sourceforge.net/")
    (synopsis "Java library of statistical distributions")
    (description "JDistlib is the Java Statistical Distribution Library, a
Java package that provides routines for various statistical distributions.")
    ;; The files that were translated from R code are under GPLv2+; some files
    ;; are under the GPLv3, which is a mistake.  The author confirmed in an
    ;; email that this whole project should be under GPLv2+.
    (license license:gpl2+)))

(define-public emacs-ess
  (package
    (name "emacs-ess")
    (version "25.01.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-ess/ESS")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0v56b47qidpyxvyk0q487qxhj9si0jkm852frl832iraks02l5h5"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Stop ESS from trying to bundle an external julia-mode.el.
            (substitute* "lisp/Makefile"
              ((" \\$\\(JULIAS)") "")
              (("\ttest.*julia-mode.*\\.el") ""))
            ;; Only build docs in info format.
            (substitute* "doc/Makefile"
              (("all  : info text")
               "all  : info")
              (("install: install-info install-other-docs")
               "install: install-info"))
            ;; Stop install-info from trying to update the info directory.
            (substitute* "doc/Makefile"
              ((".*/dir.*") ""))
            ;; Avoid generating ess-autoloads.el twice.
            (substitute* "Makefile"
              (("all: lisp doc etc autoloads")
               "all: lisp doc etc"))
            ;; Install to correct directories.
            (substitute* "Makefile"
              (("mkdir -p \\$\\(ESSDESTDIR)")
               "$(MAKE) -C lisp install; $(MAKE) -C doc install")
              (("\\$\\(INSTALL) -R \\./\\* \\$\\(ESSDESTDIR)/")
               "$(MAKE) -C etc install"))))))
    (build-system gnu-build-system)
    (arguments
     (let ((base-directory "/share/emacs/site-lisp"))
       (list
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (guix build emacs-utils))
        #:imported-modules `(,@%default-gnu-imported-modules
                             (guix build emacs-build-system)
                             (guix build emacs-utils))
        #:make-flags
        #~(list (string-append "PREFIX=" #$output)
                (string-append "ETCDIR=" #$output #$base-directory "/etc")
                (string-append "LISPDIR=" #$output #$base-directory)
                (string-append "INFODIR=" #$output "/share/info"))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (add-before 'check 'skip-failing-tests
              (lambda _
                (let-syntax
                    ((disable-tests
                      (syntax-rules ()
                        ((_ ())
                         (syntax-error "test names list must not be empty"))
                        ((_ (test-name ...))
                         (substitute* (find-files "test" "\\.el$")
                           (((string-append "^\\(ert-deftest " test-name ".*")
                             all)
                            (string-append all "(skip-unless nil)\n"))
                           ...))))
                     (disable-etests  ;different test syntax
                      (syntax-rules ()
                        ((_ ())
                         (syntax-error "test names list must not be empty"))
                        ((_ (test-name ...))
                         (for-each
                          (lambda (file)
                            (emacs-batch-edit-file file
                              '(progn
                                (dolist (test (list test-name ...))
                                        (goto-char (point-min))
                                        (let ((s (format "etest-deftest %s "
                                                         test)))
                                          (when (search-forward s nil t)
                                            (beginning-of-line)
                                            (kill-sexp))))
                                (basic-save-buffer))))
                          (find-files "test" "\\.el$"))))))
                  (disable-tests ("ess--derive-connection-path"
                                  "ess-eval-line-test"
                                  "ess-eval-region-test"
                                  "ess-mock-remote-process"
                                  "ess-r-load-ESSR-github-fetch-no"
                                  "ess-r-load-ESSR-github-fetch-yes"
                                  "ess-set-working-directory-test"
                                  "ess-test-r-startup-directory"))
                  (disable-etests ("ess-r-eval-ns-env-roxy-tracebug-test"
                                   "ess-r-eval-sink-freeze-test"
                                   ;; Looks like an off-by-one error.
                                   "ess--command-browser-unscoped-essr")))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests? (invoke "make" "test"))))))))
    (native-inputs (list perl r-roxygen2 texinfo))
    (inputs (list emacs-minimal r-minimal))
    (propagated-inputs (list emacs-julia-mode))
    (home-page "https://ess.r-project.org/")
    (synopsis "Emacs mode for statistical analysis programs")
    (description
     "Emacs Speaks Statistics (ESS) is an add-on package for GNU Emacs.  It
is designed to support editing of scripts and interaction with various
statistical analysis programs such as R, Julia, and JAGS.")
    (license license:gpl3+)))

(define-public emacs-poly-r
  (package
    (name "emacs-poly-r")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/polymode/poly-R")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a4wx73jkngw5nbq1fa4jfhba6bsmyn6vnsf887x3xhb5v3ykhsg"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-ess emacs-poly-noweb emacs-polymode-markdown
           emacs-polymode))
    (properties '((upstream-name . "poly-R")))
    (home-page "https://github.com/polymode/poly-markdown")
    (synopsis "Polymodes for the R language")
    (description
     "This package provides a number of polymodes for working with mixed R
files, including Rmarkdown files.")
    (license license:gpl3+)))

(define-public readstat
  (package
    (name "readstat")
    (version "1.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WizardMac/ReadStat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aqalr86d7h9sp9zjkydw3ap4s54qgq2ml6p8gd0gnn1jf0ljm72"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake gnu-gettext libtool))
    (inputs
     (list zlib))
    (home-page "https://github.com/WizardMac/ReadStat")
    (synopsis "Convert SAS, Stata, and SPSS files")
    (description "Command-line tool and C library for reading files from
popular stats packages like SAS, Stata and SPSS.")
    (license license:expat)))

(define-public python-pyreadstat
  (package
    (name "python-pyreadstat")
    (version "1.2.4")
    ;; No tests in the PyPI tarball.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Roche/pyreadstat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zysrzixvqw2lwwykxqg5yj8a0zyv5s2bmk22x30f4rj2hgvq1pv"))
       (patches (search-patches "python-pyreadstat-link-libiconv.patch"))))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'change-home-dir
                    (lambda _
                      ;; test_sav_expand and test_sav_write_basic_expanduser need a
                      ;; home directory with write permissions.
                      (setenv "HOME" "/tmp")))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        ;; The source also contains tests/test_version.py
                        ;; which checks the version in __init__.py against the
                        ;; one in setup.py. Since this requires texlive
                        ;; dependencies to run and is also not mentioned in
                        ;; how_to_test.md, this test is skipped.
                        (invoke "python" "tests/test_basic.py")))))))
    (build-system python-build-system)
    (propagated-inputs (list python-pandas))
    (inputs (list libiconv zlib))
    (native-inputs (list python-cython-3))
    (home-page "https://github.com/Roche/pyreadstat")
    (synopsis
     "Read and write SAS, SPSS and Stata files into/from Pandas DataFrames")
    (description
     "This Python package can be used to read and write SAS, SPSS and Stata
files into/from Pandas DataFrames.  It is a wrapper around the C library
@code{readstat}.")
    (license license:asl2.0)))

(define-public r-quantpsyc
  (package
    (name "r-quantpsyc")
    (version "1.6")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "QuantPsyc" version))
        (sha256
          (base32
            "1dbj830p5837fiwa800nzsaf19lf95vc3n7jmds2q9v06mrz6syq"))))
    (properties `((upstream-name . "QuantPsyc")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-boot
           r-dplyr
           r-mass
           r-purrr))
    (home-page "https://cran.r-project.org/web/packages/QuantPsyc/")
    (synopsis "Quantitative Psychology Tools")
    (description
      "Contains functions useful for data screening, testing moderation,
mediation and estimating power.")
    (license license:gpl2+)))

(define-public r-clubsandwich
  (package
    (name "r-clubsandwich")
    (version "0.5.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "clubSandwich" version))
       (sha256
        (base32
         "1y9iy86w2440y22279is7vgi9m9v876p5954nhq5lg9ghpzbwzdq"))))
    (properties `((upstream-name . "clubSandwich")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lifecycle r-sandwich))
    (native-inputs (list r-knitr r-testthat))
    (home-page "https://github.com/jepusto/clubSandwich")
    (synopsis "Cluster-Robust (Sandwich) Variance Estimators with Small-Sample
Corrections")
    (description
     "Provides several cluster-robust variance estimators (i.e.,
sandwich estimators) for ordinary and weighted least squares linear regression
models, including the bias-reduced linearization estimator introduced by Bell
and McCaffrey (2002)
@url{http://www.statcan.gc.ca/pub/12-001-x/2002002/article/9058-eng.pdf} and
developed further by Pustejovsky and Tipton (2017)
@url{doi:10.1080/07350015.2016.1247004}.  The package includes functions for estimating
the variance- covariance matrix and for testing single- and multiple-
contrast hypotheses based on Wald test statistics.  Tests of single regression
coefficients use Satterthwaite or saddle-point corrections.  Tests of multiple-
contrast hypotheses use an approximation to Hotelling's T-squared distribution.
Methods are provided for a variety of fitted models, including @code{lm()} and
@code{mlm} objects, @code{glm()}, ivreg (from package @code{AER}), @code{plm()}
(from package @code{plm}), @code{gls()} and @code{lme()} (from @code{nlme}),
@code{robu()} (from @code{robumeta}), and @code{rma.uni()} and @code{rma.mv()}
(from @code{metafor}).")
    (license license:gpl3)))

(define-public r-puniform
  (package
    (name "r-puniform")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "puniform" version))
       (sha256
        (base32
         "15q5wlcps7387rjz7p1f5kifg1fl5yxxy7gjx6fvspvqwjkjbs4z"))))
    (properties `((upstream-name . "puniform")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-adgoftest r-metafor r-numderiv r-rcpp r-rcpparmadillo))
    (home-page
     "https://github.com/RobbievanAert/puniform")
    (synopsis
     "Meta-Analysis Methods Correcting for Publication Bias")
    (description
     "This package provides meta-analysis methods that correct for publication
bias and outcome reporting bias.  Four methods and a visual tool are currently
included in the package.

@enumerate
@item The p-uniform method as described in van Assen, van Aert, and Wicherts
(2015) @url{doi:10.1037/met0000025} can be used for estimating the average
effect size, testing the null hypothesis of no effect, and testing for
publication bias using only the statistically significant effect sizes of
primary studies.

@item The p-uniform* method as described in van Aert and van Assen (2019)
@url{doi:10.31222/osf.io/zqjr9}.  This method is an extension of the p-uniform
method that allows for estimation of the average effect size and the
between-study variance in a meta-analysis, and uses both the statistically
significant and nonsignificant effect sizes.

@item The hybrid method as described in van Aert and van Assen (2017)
@url{doi:10.3758/s13428-017-0967-6}.  The hybrid method is a meta-analysis
method for combining an original study and replication and while taking into
account statistical significance of the  original study.  The p-uniform and
hybrid method are based on the statistical theory that the distribution of
p-values is uniform conditional on the population effect size.

@item
The fourth method in the package is the Snapshot Bayesian Hybrid Meta-Analysis
Method as described in van Aert and van Assen (2018)
@url{doi:10.1371/journal.pone.0175302}.  This method computes posterior
probabilities for four true effect sizes (no, small, medium, and large) based
on an original study and replication while taking into account publication bias
in the original study.  The method can also be used for computing the required
sample size of the replication akin to power analysis in null hypothesis
significance testing.
@end enumerate

The meta-plot is a visual tool for meta-analysis that
provides information on the primary studies in the meta-analysis, the results
of the meta-analysis, and characteristics of the research on the effect under
study (van Assen and others, 2020).

Helper functions to apply the Correcting for Outcome Reporting Bias (CORB)
method to correct for outcome reporting bias in a meta-analysis (van Aert &
Wicherts, 2020).")
    (license license:gpl2+)))

(define-public r-kknn
  (package
    (name "r-kknn")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "kknn" version))
        (sha256
          (base32
            "1nzkg3dxaiqp87p56wm895qx5xn86hv5hjr73qvl1yiaxiq0x112"))))
    (properties `((upstream-name . "kknn")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-igraph r-matrix))
    (home-page "https://github.com/KlausVigo/kknn")
    (synopsis "Weighted k-Nearest Neighbors")
    (description
      "Weighted k-Nearest Neighbors for Classification, Regression and Clustering.")
    (license license:gpl2+)))

(define-public r-logspline
  (package
    (name "r-logspline")
    (version "2.1.22")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "logspline" version))
        (sha256
          (base32
            "00pdcfj1hmafxzs7gqw3n072p6pxp9hvsdm062n0xkiby6igffkp"))))
    (properties `((upstream-name . "logspline")))
    (build-system r-build-system)
    (native-inputs (list gfortran))
    (home-page
      "https://cran.r-project.org/web/packages/logspline/")
    (synopsis
      "Routines for Logspline Density Estimation")
    (description
      "Contains routines for logspline density estimation.  The function
@code{oldlogspline()} uses the same algorithm as the logspline package version
1.0.x; i.e., the Kooperberg and Stone (1992) algorithm (with an improved
interface).  The recommended routine @code{logspline()} uses an algorithm from
@url{doi:10.1214/aos/1031594728,Stone et al (1997)}.")
    (license license:asl2.0)))

(define-public r-norm
  (package
    (name "r-norm")
    (version "1.0-11.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "norm" version))
              (sha256
               (base32
                "1g33g721c0f2b275b334ir6n0h81fh567vs9vrxk60y21z1ydzy2"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
    (home-page "https://cran.r-project.org/web/packages/norm/")
    (synopsis "Analysis of multivariate normal datasets with missing values")
    (description "Multiple imputation of multivariate continuous data under a
normal model.")
    ;; Custom license, see https://cran.r-project.org/web/packages/norm/LICENSE.
    (license (license:non-copyleft "file://LICENSE"))))

(define-public r-naniar
  (package
    (name "r-naniar")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "naniar" version))
              (sha256
               (base32
                "0i3gijr1l7hbcp75cyj26pxmm493lnvasl8aba9vv4w8lz1lck59"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cli
           r-dplyr
           r-forcats
           r-ggplot2
           r-glue
           r-lifecycle
           r-magrittr
           r-norm
           r-purrr
           r-rlang
           r-tibble
           r-tidyr
           r-upsetr
           r-vctrs
           r-viridis
           r-visdat))
    (native-inputs
     (list r-knitr r-testthat))
    (home-page "https://github.com/njtierney/naniar")
    (synopsis
     "Data structures, summaries, and visualisations for missing data")
    (description
     "Missing values are ubiquitous in data and need to be explored and
handled in the initial stages of analysis.  The package provides data structures
and functions that facilitate the plotting of missing values and examination of
imputations.  This allows missing data dependencies to be explored with minimal
deviation from the common work patterns of @code{ggplot2} and tidy data.")
    (license license:expat)))

(define-public r-glinternet
  (package
    (name "r-glinternet")
    (version "1.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "glinternet" version))
       (sha256
        (base32
         "0aphdscj0s6kqxlkgqkw2d6snrylz6hiis6307pl8ldh7q5cvm33"))))
    (build-system r-build-system)
    (home-page "http://web.stanford.edu/~hastie/Papers/glinternet_jcgs.pdf")
    (synopsis "Learning interactions via hierarchical group-lasso regularization")
    (description "Group-Lasso INTERaction-NET.  Fits linear pairwise-interaction
models that satisfy strong hierarchy: if an interaction coefficient is estimated
to be nonzero, then its two associated main effects also have nonzero estimated
coefficients.  Accommodates categorical variables (factors) with arbitrary
numbers of levels, continuous variables, and combinations thereof.  Implements
the machinery described in the paper \"Learning interactions via hierarchical
group-lasso regularization\" (JCGS 2015, Volume 24, Issue 3).
Michael Lim & Trevor Hastie (2015)")
    (license license:gpl2)))

(define-public r-datasaurus
  (package
    (name "r-datasaurus")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "datasauRus" version))
        (sha256
          (base32
            "0lkf32py342nswm8k4x1hf9j5pnbx2xipzfaj54z6d58a2nlgii1"))))
    (properties `((upstream-name . "datasauRus")))
    (build-system r-build-system)
    (native-inputs (list r-knitr r-testthat))
    (home-page
      "https://github.com/lockedata/datasauRus")
    (synopsis "Datasets from the Datasaurus Dozen")
    (description
     "The Datasaurus Dozen is a set of datasets with the same summary
statistics.  They retain the same summary statistics despite having radically
different distributions.  The datasets represent a larger and quirkier object
lesson that is typically taught via Anscombe's Quartet (available in the
'datasets' package).  Anscombe's Quartet contains four very different
distributions with the same summary statistics and as such highlights the value
of visualisation in understanding data, over and above summary statistics.  As
well as being an engaging variant on the Quartet, the data is generated in a
novel way.  The simulated annealing process used to derive datasets from the
original Datasaurus is detailed in \"Same Stats, Different Graphs: Generating
Datasets with Varied Appearance and Identical Statistics through Simulated
Annealing\" @url{doi:10.1145/3025453.3025912}.")
    (license license:expat)))

(define-public r-lmom
  (package
    (name "r-lmom")
    (version "3.2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "lmom" version))
        (sha256
          (base32 "165dqz8yv3wak77935shz8axcck6jg5h7fnyy0ih9dzin3npng6s"))))
    (properties `((upstream-name . "lmom")))
    (build-system r-build-system)
    (native-inputs (list gfortran))
    (home-page "https://cran.r-project.org/package=lmom")
    (synopsis "L-Moments for R")
    (description
      "This package provides functions related to L-moments: computation
of L-moments and trimmed L-moments of distributions and data samples;
parameter estimation; L-moment ratio diagram; plot vs.  quantiles of an
extreme-value distribution.")
    (license license:cpl1.0)))

(define-public r-gld
  (package
    (name "r-gld")
    (version "2.6.7")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "gld" version))
        (sha256
          (base32 "0w50nn1cp8mzjxp8403ymqh7f3iz0nk5wa8ms97d02lqp2jgfvyc"))))
    (properties `((upstream-name . "gld")))
    (build-system r-build-system)
    (propagated-inputs (list r-e1071 r-lmom))
    (home-page
      "https://cran.r-project.org/package=gld")
    (synopsis
      "Estimation and Use of the Generalised (Tukey) Lambda Distribution")
    (description
      "The generalised lambda distribution, or Tukey lambda distribution,
provides a wide variety of shapes with one functional form.  This package
provides random numbers, quantiles, probabilities, densities and
density quantiles for four different types of the distribution, the FKML
(Freimer et al 1988), RS (Ramberg and Schmeiser 1974), GPD (van Staden
and Loots 2009) and FM5 - see documentation for details.  It provides the
density function, distribution function, and Quantile-Quantile plots.
It implements a variety of estimation methods for the distribution,
including diagnostic plots.  Estimation methods include the starship (all
4 types), method of L-Moments for the GPD and FKML types, and a number
of methods for only the FKML type.  These include maximum likelihood,
maximum product of spacings, Titterington's method, Moments, Trimmed
L-Moments and Distributional Least Absolutes.")
    (license license:gpl2+)))

(define-public r-exact
  (package
    (name "r-exact")
    (version "3.3")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "Exact" version))
        (sha256
          (base32 "0hikz8f19blywxs66xj6zf4krwyivsy3w165qvrnb0m2l6dx1l7v"))))
    (properties `((upstream-name . "Exact")))
    (build-system r-build-system)
    (propagated-inputs (list r-rootsolve))
    (home-page "https://cran.r-project.org/package=Exact")
    (synopsis "Unconditional Exact Test")
    (description
      "Performs unconditional exact tests and power calculations for 2x2
contingency tables.  For comparing two independent proportions, performs
@url{doi:10.1038/156177a0, Barnard's test (1945)} using the original CSM
test (@url{doi:10.1093/biomet/34.1-2.123, Barnard (1947)}), using Fisher's
p-value referred to as @url{doi:10.1111/j.1467-9574.1970.tb00104.x,
Boschloo's test (1970)}, or using a Z-statistic (@url{doi:10.2307/2981892,
Suissa and Shuster (1985)}).  For comparing two binary proportions,
performs unconditional exact test using McNemar's Z-statistic
(@url{doi:10.1191/0962280203sm312ra, Berger and Sidik (2003)}), using
McNemar's Z-statistic with continuity correction, or using CSM test.
Calculates confidence intervals for the difference in proportion.")
    (license license:gpl2)))

(define-public r-desctools
  (package
    (name "r-desctools")
    (version "0.99.59")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "DescTools" version))
        (sha256
          (base32 "1xiw8741brv3771vf18clj240i8p0wn8367fv1f6vwxwxlqrclkr"))))
    (properties `((upstream-name . "DescTools")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-boot
           r-cli
           r-data-table
           r-exact
           r-expm
           r-gld
           r-haven
           r-httr
           r-mass
           r-mvtnorm
           r-rcpp
           r-readxl
           r-rstudioapi
           r-withr))
    (native-inputs (list gfortran r-r-rsp))
    (home-page "https://andrisignorell.github.io/DescTools/")
    (synopsis "Tools for Descriptive Statistics")
    (description
      "This package provides a collection of miscellaneous basic statistic
functions and convenience wrappers for efficiently describing data.
The author's intention was to create a toolbox, which facilitates the
(notoriously time consuming) first descriptive tasks in data analysis,
consisting of calculating descriptive statistics, drawing graphical
summaries and reporting the results.  The package contains furthermore
functions to produce documents using MS Word (or PowerPoint) and functions
to import data from Excel.  Many of the included functions can be found
scattered in other packages and other sources written partly by Titans
of R.  The reason for collecting them here, was primarily to have them
consolidated in ONE instead of dozens of packages (which themselves might
depend on other packages which are not needed at all), and to provide a
common and consistent interface as far as function and arguments naming,
NA handling, recycling rules etc.  are concerned.  Google style guides
were used as naming rules (in absence of convincing alternatives).
The BigCamelCase style was consequently applied to functions borrowed
from contributed R packages as well.")
    (license license:gpl2+)))

(define-public xlispstat
  (let ((commit "f1bea6053df658ee48612bf1f63c35de99e2c649")
        (revision "0"))
    (package
      (name "xlispstat")
      (version (git-version "3.52.23" revision commit))
      (source (origin
	        (method git-fetch)
	        (uri (git-reference
		      (url "https://github.com/jhbadger/xlispstat.git")
		      (commit commit)))
                (file-name (git-file-name name version))
	        (sha256
	         (base32
	          "1p0cmgy19kbkxia139cb5w9dnkp2cdqp5n3baag6cq3prn3n71mf"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-build? #f   ; Parallel builds are not supported
         #:configure-flags (list "--with-gcc")
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (with-output-to-file "exit.lsp"
                   (lambda () (display "(exit)")))
                 (invoke "./xlisp" "tests/test" "exit")))))))
      (inputs (list tcsh
                    libx11
                    libxmu
                    libxext
                    libxpm
                    libxaw
                    ncurses
                    gnuplot))
      (native-inputs (list pkg-config))
      (synopsis "Statistical analysis environment with interactive graphics")
      (description "XLISP-STAT is a statistical environment based on a Lisp
dialect called XLISP.  To facilitate statistical computations, standard
functions for addition, logarithms, etc., have been modified to operate on
lists and arrays of numbers, and a number of basic statistical functions have
been added.  Many of these functions have been written in Lisp, and additional
functions can be added easily by a user.  Several basic forms of plots,
including histograms, scatterplots, rotatable plots and scatterplot matrices
are provided.  These plots support various forms of interactive highlighting
operations and can be linked so points highlighted in one plot will be
highlighted in all linked plots.  Interactions with the plots are controlled
by the mouse, menus and dialog boxes.  An object-oriented programming system
is used to allow menus, dialogs, and the response to mouse actions to be
 customized.")
      (home-page "https://homepage.divms.uiowa.edu/~luke/xls/xlsinfo/")
      (license license:expat))))

(define-public r-rlrsim
  (package
    (name "r-rlrsim")
    (version "3.1-8")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "RLRsim" version))
        (sha256
          (base32 "06i4slc7fj8xifq20i1vqfadfw2p81b0kfzga60139hnryz1zpva"))))
    (properties `((upstream-name . "RLRsim")))
    (build-system r-build-system)
    (propagated-inputs (list r-lme4 r-mgcv r-nlme r-rcpp))
    (home-page "https://github.com/fabian-s/RLRsim")
    (synopsis
      "Exact (Restricted) Likelihood Ratio Tests for Mixed and Additive Models")
    (description
      "Rapid, simulation-based exact (restricted) likelihood ratio tests for testing
the presence of variance components/nonparametric terms for models fit with
@code{nlme::lme()}, @code{lme4::lmer()}, @code{lmeTest::lmer()},
@code{gamm4::gamm4()}, @code{mgcv::gamm()} and @code{SemiPar::spm()}.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-simr
  (package
    (name "r-simr")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "simr" version))
       (sha256
        (base32 "1rfnhyqvdazvar7r1ml71lskh1hdk3yfzv5jlcz18wzffscgkgmb"))))
    (properties `((upstream-name . "simr")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-binom
           r-car
           r-iterators
           r-lme4
           r-lmertest
           r-pbkrtest
           r-plotrix
           r-plyr
           r-rlrsim
           r-stringr))
    (native-inputs (list r-knitr r-testthat))
    (home-page "https://github.com/pitakakariki/simr")
    (synopsis
     "Power analysis for generalized linear mixed models by simulation")
    (description
     "This package lets you calculate power for generalized linear mixed
models, using simulation.  It was designed to work with models fit using the
@code{lme4} package.  The package is described in
@url{doi:10.1111/2041-210X.12504, Green and MacLeod (2016)}.")
    (license license:gpl2+)))

(define-public r-mixedpower
  ;; This commit contains fixes for R>=4.2 and contains new features. A
  ;; newer release does not exist.
  (let ((commit "b2b87068546327d8f592d141e5482e0478c1b2ee")
        (revision "2"))
    (package
      (name "r-mixedpower")
      (version (git-version "2.0" revision commit))
      (source
        (origin
          ;; Not available on CRAN.
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/DejanDraschkow/mixedpower")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32 "0pys66a6c1k2fp5qk9grhzg1q1d3q51rggcilmdssimkmcsm234z"))))
      (properties `((upstream-name . "mixedpower")))
      (build-system r-build-system)
      (propagated-inputs
        (list r-doparallel r-foreach r-ggplot2 r-lme4 r-reshape2))
      (home-page "https://github.com/DejanDraschkow/mixedpower")
      (synopsis
        "Pilotdata based simulations for estimating power in linear mixed models")
      (description
       "Mixedpower uses pilotdata and a linear mixed model fitted with lme4 to
simulate new data sets.  Power is computed separate for every effect in the
model output as the relation of significant simulations to all simulations.
More conservative simulations as a protection against a bias in the pilotdata
are available as well as methods for plotting the results.")
      (license license:gpl3))))

(define-public r-colorway
  (let ((commit "8ba8f0026aba37752c6770de45bf53b1b0f48afc")
        (revision "1"))
    (package
      (name "r-colorway")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hypercompetent/colorway")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0r6yhnzx3ll1z12pp6y8fqbrx7v18rcff2j2179bqy9ca2d2d94l"))))
      (properties `((upstream-name . "colorway")))
      (build-system r-build-system)
      (propagated-inputs (list r-dplyr r-ggplot2 r-rlang))
      (home-page "https://github.com/hypercompetent/colorway")
      (synopsis "Functions for colors in R")
      (description
       "This package provides a collection of (mostly simple) functions for
generating and manipulating colors in R.")
      (license license:gpl3))))

(define-public r-reghelper
  (package
    (name "r-reghelper")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "reghelper" version))
              (sha256
               (base32
                "1vd8kd719kyjp65zym6zx3vax1q2kbhpl6la71d5aa59s54ylri3"))))
    (properties `((upstream-name . "reghelper")))
    (build-system r-build-system)
    (propagated-inputs (list r-ggplot2 r-lme4 r-mass r-nlme r-rlang))
    (native-inputs (list r-testthat))
    (home-page "https://github.com/jeff-hughes/reghelper")
    (synopsis "Helper Functions for Regression Analysis")
    (description
     "This package provides a set of functions used to automate commonly used methods
in regression analysis.  This includes plotting interactions, and calculating
simple slopes, standardized coefficients, regions of significance (Johnson &
Neyman, 1936; cf.  Spiller et al., 2012), etc.")
    (license license:gpl3)))

(define-public r-mumin
  (package
    (name "r-mumin")
    (version "1.48.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "MuMIn" version))
              (sha256
               (base32
                "04zn63snv218c44nllfh8bqh5xg0gamd9fcpwxisvi9gwb3f20i0"))))
    (properties `((upstream-name . "MuMIn")))
    (build-system r-build-system)
    (propagated-inputs (list r-insight r-matrix r-nlme))
    (native-inputs (list r-mgcv r-survival))
    (home-page "https://cran.r-project.org/package=MuMIn")
    (synopsis "Multi-Model Inference")
    (description
     "Tools for performing model selection and model averaging.  Automated model
selection through subsetting the maximum model, with optional constraints for
model inclusion.  Model parameter and prediction averaging based on model
weights derived from information criteria (AICc and alike) or custom model
weighting schemes.")
    (license license:gpl2)))

(define-public ruby-enumerable-statistics
  (package
    (name "ruby-enumerable-statistics")
    (version "2.0.7")
    ;; Source at RubyGems.org doesn't have tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mrkn/enumerable-statistics.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a8k2dvm1v0g6hcrbnzy0f7d63hdmpmldfdkl8wr32nbl05xnifa"))
              (modules '((guix build utils)))
              (snippet `(begin
                          (substitute* "enumerable-statistics.gemspec"
                            ;; benchmark-driver gem is used for
                            ;; performance benchmarking, and isn't
                            ;; needed for tests.
                            (("spec.add_development_dependency \"benchmark-driver\"\n")
                             ""))))))
    (build-system ruby-build-system)
    (native-inputs (list bundler
                         ruby-rake
                         ruby-rake-compiler
                         ruby-rspec
                         ruby-test-unit
                         ruby-fuubar
                         ruby-yard))
    (synopsis "Library which provides statistics features for Enumerable")
    (description
     "@code{Enumerable::Statistics} provides some methods to calculate
statistical summary in arrays and enumerables.")
    (home-page "https://github.com/mrkn/enumerable-statistics")
    (license license:expat)))

(define-public python-pgmpy
  (package
    (name "python-pgmpy")
    (version "0.1.24")
    (source
     (origin
       (method git-fetch)  ;pypi package does not include test data
       (uri (git-reference
             (url "https://github.com/pgmpy/pgmpy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fvzh6v0yhgdryczamvzhfy2ymywkh0ssx4rl47xnfvi43hnij90"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-daft
                             python-joblib
                             python-networkx
                             python-numpy
                             python-opt-einsum
                             python-pandas
                             python-pyparsing
                             python-pytorch
                             python-scikit-learn
                             python-scipy
                             python-statsmodels
                             python-tqdm))
    (native-inputs (list python-mock
                         python-pytest))
    (home-page "https://github.com/pgmpy/pgmpy")
    (synopsis "Probabilistic Graphical Models library")
    (description "This package provides a library for Probabilistic
Graphical Models.  It can be used for learning (Structure and Parameter),
inference (Probabilistic and Causal), and simulations in Bayesian
Networks.")
    (license license:expat)))
