;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Prafulla Giri <pratheblackdiamond@gmail.com>
;;; Copyright © 2020 Christopher Lam <christopher.lck@gmail.com>
;;; Copyright © 2023, 2024 gemmaro <gemmaro.dev@gmail.com>
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

(define-module (gnu packages gnucash)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public gnucash
  ;; TODO: Unbundle libraries such as guile-json found under the "borrowed/"
  ;; directory.
  (package
    (name "gnucash")
    (version "5.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gnucash/gnucash%20%28stable%29/"
                           version "/gnucash-" version ".tar.bz2"))
       (sha256
        (base32 "14r5nmml40icxbjfz4giis6kiplvjna17j1fd6c4b78bf3xj7j52"))))
    (outputs '("out" "doc" "debug" "python"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:test-target "check"
      #:configure-flags #~(list "-DWITH_PYTHON=ON")
      #:make-flags #~(list "GUILE_AUTO_COMPILE=0")
      #:imported-modules `(,@%gnu-build-system-modules
                           (guix build cmake-build-system)
                           (guix build glib-or-gtk-build-system))
      #:modules '((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-online-test
            (lambda _
              (call-with-output-file "libgnucash/app-utils/test/CMakeLists.txt"
                (lambda (port)
                  (display "set(CTEST_CUSTOM_TESTS_IGNORE online_wiggle test-lots)" port)))))
          (add-after 'unpack 'set-env-vars
            (lambda* (#:key inputs #:allow-other-keys)
              ;; At least one test is time-related and requires this
              ;; environment variable.
              (setenv "TZDIR" (search-input-directory inputs "share/zoneinfo"))
              (substitute* "CMakeLists.txt"
                (("set\\(SHELL /bin/bash\\)")
                 (string-append "set(SHELL " (which "bash") ")")))))
          ;; The qof test requires the en_US, en_GB, and fr_FR locales.
          (add-before 'check 'install-locales
            (lambda _
              (setenv "LOCPATH" (getcwd))
              (invoke "localedef" "-i" "en_US" "-f" "UTF-8" "./en_US.UTF-8")
              (invoke "localedef" "-i" "en_GB" "-f" "UTF-8" "./en_GB.UTF-8")
              (invoke "localedef" "-i" "fr_FR" "-f" "UTF-8" "./fr_FR.UTF-8")))
          ;; There is about 100 MiB of documentation.
          (add-after 'install 'install-docs
            (lambda _
              (mkdir-p (string-append #$output:doc "/share"))
              (symlink (string-append
                        #$(this-package-native-input "gnucash-docs")
                        "/share/help")
                       (string-append #$output:doc "/share/help"))))
          (add-after 'install 'split-python-bindings
            (lambda _
              (let ((python-bindings (string-append
                                      "lib/python"
                                      #$(version-major+minor
                                         (package-version python)))))
                (mkdir-p (string-append #$output:python "/" python-bindings))
                (copy-recursively
                 (string-append #$output "/" python-bindings)
                 (string-append #$output:python "/" python-bindings))
                (delete-file-recursively
                 (string-append #$output "/" python-bindings)))))
          (add-after 'install-docs 'wrap-programs
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (for-each
               (lambda (prog)
                 (wrap-program (search-input-file
                                outputs (string-append "bin/" prog))
                   `("GNC_DBD_DIR" =
                     (,(search-input-directory inputs "lib/dbd")))
                   `("PERL5LIB" ":" prefix
                     ,(map (lambda (o)
                             (string-append o "/lib/perl5/site_perl/"
                                            #$(package-version perl)))
                           (if (string=? prog "gnc-fq-helper")
                               (list
                                #$@(transitive-input-references
                                    'inputs
                                    (map (lambda (l)
                                           (assoc l (package-inputs this-package)))
                                         '("perl-finance-quote"
                                           "perl-date-manip"))))
                               (list
                                #$@(transitive-input-references
                                    'inputs
                                    (map (lambda (l)
                                           (assoc l (package-inputs this-package)))
                                         '("perl-finance-quote")))))))))
               '("gnucash"
                 "gnc-fq-update"))))
          (add-after 'install 'glib-or-gtk-compile-schemas
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
          (add-after 'install 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list gmp
           `(,glib "bin")               ;glib-compile-schemas, etc.
           gnucash-docs
           googletest
           intltool
           pkg-config
           swig))
    (inputs
     (list aqbanking
           bash-minimal
           boost
           glib
           gtk+
           guile-3.0
           icu4c
           libdbi
           libdbi-drivers
           libofx
           libxml2
           libxslt
           perl-date-manip
           perl-finance-quote
           perl-json
           perl-json-parse
           python
           tzdata-for-tests
           webkitgtk-with-libsoup2))
    (propagated-inputs
     ;; dconf is required at runtime according to README.dependencies.
     (list dconf))
    (home-page "https://www.gnucash.org/")
    (synopsis "Personal and small business financial accounting software")
    (description
     "GnuCash is personal and professional financial-accounting software.
It can be used to track bank accounts, stocks, income and expenses, based on
the double-entry accounting practice.  It includes support for QIF/OFX/HBCI
import and transaction matching.  It also automates several tasks, such as
financial calculations or scheduled transactions.

To make the GnuCash documentation available, its doc output must be
installed as well as Yelp, the Gnome help browser.")
    (license license:gpl3+)))

;; This package is not public, since we use it to build the "doc" output of
;; the gnucash package (see above).  It would be confusing if it were public.
(define gnucash-docs
  (let ((revision ""))               ;set to the empty string when no revision
    (package
      (name "gnucash-docs")
      (version (package-version gnucash))
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/gnucash/gnucash%20%28stable%29/"
               version "/gnucash-docs-" version revision ".tar.gz"))
         (sha256
          (base32 "0gssmbwwiafp4g9v5waz5935bkgyzbna76ryz5lhc294b3n49wxq"))))
      (build-system cmake-build-system)
      ;; These are native-inputs because they are only required for building the
      ;; documentation.
      (native-inputs
       ;; The "check" target needs docbook-xml package to validate the DocBook
       ;; XML during the tests.
       (list docbook-xml
             docbook-xsl
             libxml2
             libxslt
             scrollkeeper))
      (arguments
       `(#:tests? #f))                  ;no test target
      (home-page "https://www.gnucash.org/")
      (synopsis "Documentation for GnuCash")
      (description
       "User guide and other documentation for GnuCash in various languages.
This package exists because the GnuCash project maintains its documentation in
an entirely separate package from the actual GnuCash program.  It is intended
to be read using the GNOME Yelp program.")
      (license (list license:fdl1.1+ license:gpl3+)))))

(define-public gwenhywfar
  (let ((attachid "501")) ;; file attachid changes for each version
    (package
      (name "gwenhywfar")
      (version "5.10.2")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://www.aquamaniac.de/rdm/attachments/"
                             "download/" attachid "/gwenhywfar-" version ".tar.gz"))
         (sha256
          (base32 "1qd950kilf9bcb51b1daykipanml6bg1h3pj10950r98ah1xm9v0"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list "--disable-network-checks"
               ;; GTK+3, GTK+2 and QT4 are supported.
               "--with-guis=gtk3"
               "--enable-system-certs"
               "--with-libxml2-code=yes"
               "--disable-binreloc")))
      (inputs
       (list libgcrypt gnutls openssl gtk+ libxml2))
      (native-inputs
       (list pkg-config))
      (home-page "https://www.aquamaniac.de")
      (synopsis "Utility library for networking and security applications")
      (description
       "This package provides a helper library for networking and security
applications and libraries.  It is used by AqBanking.")
      ;; The license includes an explicit additional permission to compile and
      ;; distribute this library with the OpenSSL Toolkit.
      (license license:lgpl2.1+))))

(define-public aqbanking
  (let ((attachid "499")) ;; file attachid changes for each version
    (package
      (name "aqbanking")
      (version "6.5.4")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://www.aquamaniac.de/rdm/attachments/"
                             "download/" attachid "/aqbanking-" version ".tar.gz"))
         (sha256
          (base32 "16dc63gav0b4rmwj0clqilfpz4hlidx59x1qcr38wwghfspcw5hd"))))
      (build-system gnu-build-system)
      (arguments
       `(;; Parallel building fails because aqhbci is required before it's
         ;; built.
         #:parallel-build? #f
         #:phases
         (modify-phases %standard-phases
           ;; Embed the package version instead of the build date
           (add-after 'unpack 'use-version-instead-of-date
             (lambda _
               (substitute*
                   "src/libs/plugins/backends/aqhbci/header.xml.in"
                 (("@DATETIME@") ,version)))))))
      (propagated-inputs
       (list gwenhywfar))
      (inputs
       (list gmp xmlsec gnutls))
      (native-inputs
       (list pkg-config gettext-minimal libltdl))
      (home-page "https://www.aquamaniac.de")
      (synopsis "Interface for online banking tasks")
      (description
       "AqBanking is a modular and generic interface to online banking tasks,
financial file formats (import/export) and bank/country/currency information.
AqBanking uses backend plugins to actually perform the online tasks.  HBCI,
OFX DirectConnect, YellowNet, GeldKarte, and DTAUS discs are currently
supported.  AqBanking is used by GnuCash, KMyMoney, and QBankManager.")
      ;; AqBanking is licensed under the GPLv2 or GPLv3
      (license (list license:gpl2 license:gpl3)))))
