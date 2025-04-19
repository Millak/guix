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
    (version "4.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-major version) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1q9wdj225v4sxwfgpaz4crpkiiy2rr4mjkc7jfbxq78d7q8ylcrv"))))
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
    (version "5.21.0")
    (source (origin
              (method git-fetch)        ; no tests in PyPI
              (uri (git-reference
                    (url "https://github.com/pymc-devs/pymc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0azkbl0mpanza35ibdqdm21bf45n3xi26wy01lnxzxqblcjcny9l"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f ; tests are too computationally intensive
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'versioneer
                          (lambda _
                            (invoke "versioneer" "install")
                            (substitute* "setup.py"
                              (("version=versioneer.get_version\\(),")
                               (format #f "version=~s," #$version)))))
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
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "patsy" version))
              (sha256
               (base32
                "1i60b6s8zj0w2ks63ip4mr8z14p6pixp76rm9q2qr0gc3qwsk1p7"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-numpy python-scipy))
    (native-inputs
     (list python-pytest python-pytest-cov python-setuptools python-wheel))
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
    (version "2.13.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.utils" version))
              (sha256
               (base32
                "09fg7f0z0zdxjndfwmcsyi2z54bp7xxcwdx0hpzji68j9v1l685b"))))
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
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)  ;pypi package does not include test data
       (uri (git-reference
             (url "https://github.com/pgmpy/pgmpy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hg6wrg3jcac71zn4gknni1wrn38wa86ka3sgp2bndz59mx6sr2s"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           #~(list "-k" (string-append
                         "not test_pillai"
                         " and not test_estimate_with_cache_no_llm_calls"
                         " and not test_estimate_with_orientations"))))
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
                         python-pyro-ppl
                         python-pytest
                         python-setuptools
                         python-wheel
                         python-xgboost))
    (home-page "https://github.com/pgmpy/pgmpy")
    (synopsis "Probabilistic Graphical Models library")
    (description "This package provides a library for Probabilistic
Graphical Models.  It can be used for learning (Structure and Parameter),
inference (Probabilistic and Causal), and simulations in Bayesian
Networks.")
    (license license:expat)))
