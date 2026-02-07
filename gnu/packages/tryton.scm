;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021,2024,2025 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2025 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages tryton)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject))

(define (guix-trytonpath-search-path version)
  "Generate a GUIX_TRYTOND_MODULES_PATH search path specification, using
VERSION.

Do not use PYTHHONPATH not avoid interfering with any different Python package
installed in the same environments.  Collecting only paths actually containing
/tryton/modules reduces the number of paths."
  (search-path-specification (variable "GUIX_TRYTOND_MODULES_PATH")
                             (files (list (string-append
                                           "lib/python"
                                           (version-major+minor version)
                                           "/site-packages/trytond/modules")))))

(define-public trytond
  (package
    (name "trytond")
    (version "7.0.45")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond" version))
       (sha256
        (base32 "1xd7xf6d6qw51w7rvcy2h4mpaj2zfwrzgr8xcarm10rp6qy9c6y7"))
       (patches (search-patches "trytond-add-guix_trytond_path.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             (string-append
              ;; "modules" is [None], but should be a list of modules.
              "not ModuleTestCase"
                            ;; fixture 'self' not found
                            " and not test_method"))
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'preparations
           (lambda _
             (setenv "DB_NAME" ":memory:")
             (setenv "DB_CACHE" "/tmp")
             (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-dateutil
           python-defusedxml
           python-genshi
           python-lxml
           python-passlib
           python-polib
           python-psycopg2
           python-relatorio
           python-sql
           python-werkzeug))
    (native-inputs
     (list python-html2text python-pillow python-pydot python-pytest
           python-setuptools tzdata-for-tests))
    (native-search-paths
     (list (guix-trytonpath-search-path (package-version python))
           ;; Required to pick up entry-points from profile for Tryton modules
           ;; which are not in named 'trytond.modules.…'
           (guix-pythonpath-search-path (package-version python))))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton Server")
    (description "Tryton is a three-tier high-level general purpose
application platform using PostgreSQL as its main database engine.  It is the
core base of a complete business solution providing modularity, scalability
and security.")
    (license license:gpl3+)))

(define-public tryton
  (package
    (name "tryton")
    (version "7.0.32")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tryton" version))
       (sha256
        (base32 "0xmxxd3apcql7lz702kziimanglik1vxs1q9pxhrfwscp302l9nw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'unittest #:test-flags #~(list "discover")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'change-home
            (lambda _
              ;; Change from /homeless-shelter to /tmp for write permission.
              (setenv "HOME" "/tmp")))
          (add-after 'install 'wrap-gi-python
            (lambda _
              (let ((gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
                (wrap-program (string-append #$output "/bin/tryton")
                  `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           python-pytest
           python-setuptools))
    (inputs (list bash-minimal))        ;for wrap-program
    (propagated-inputs
     (list (librsvg-for-system)
           gsettings-desktop-schemas
           gtk+
           python-dateutil
           python-pycairo
           python-pygobject))
    (home-page "https://www.tryton.org/")
    (synopsis "Desktop client for Tryton")
    (description
     "This package provides the Tryton GTK client.")
    (license license:gpl3+)))

(define-public python-proteus
  (package
    (name "python-proteus")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "proteus" version))
       (sha256
        (base32 "08qx8c5kknmg6jkf6k3zlv4v3dsrls2p61h899mk41afa503gskg"))))
    (build-system pyproject-build-system)
    ;; Tests require python-trytond-party which requires python-proteus.
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     (list python-dateutil python-defusedxml))
    (native-inputs (list python-setuptools))
    (home-page "http://www.tryton.org/")
    (synopsis "Library to access a Tryton server as a client")
    (description
     "This package provides a library to access Tryton server as a client.")
    (license license:lgpl3+)))

;; Suppress common useless warnings to avoid cluttering output
(define %pytest.ini "
[pytest]
filterwarnings =
  ignore:.*SQLite backend.*:UserWarning
  ignore:Can not create index with parameters:UserWarning
  ignore::DeprecationWarning
")

(define (tryton-phases module . extra-test-arguments)
  "Return the phases for building and testing a Tryton module named MODULE.
If present, pass EXTRA-TEST-ARGUMENTS to pytest as well."
  #~(modify-phases %standard-phases
      (add-after 'wrap 'prepare-check
        (lambda* (#:key tests? #:allow-other-keys)
          (when tests?
            (setenv "DB_NAME" ":memory:")
            (setenv "DB_CACHE" "/tmp")
            (setenv "HOME" "/tmp")
            ;; Fake this directory as a tryton.module.… sub-module.
            (mkdir-p "/tmp/dummy/trytond/modules")
            (symlink (getcwd) (string-append "/tmp/dummy/trytond/modules/" #$module))
            (setenv "GUIX_TRYTOND_MODULES_PATH"
                    (string-append (getenv "GUIX_TRYTOND_MODULES_PATH")
                                   ":/tmp/dummy/trytond/modules"))
            ;; Create pytest.ini in sub-dir to make that dir pytest's
            ;; "rootdir" and avoid that the module's files get scanned (which
            ;; will fail since here they are not part of a package).
            (with-output-to-file "tests/pytest.ini"
              (lambda ()
                (format #t #$%pytest.ini))))))
      (replace 'check
        (lambda* (#:key tests? #:allow-other-keys)
          (when tests?
            ;; Use pytest to allow excluding failing tests via command line
            ;; args (resp. arguments to '(tryton-arguments)')
            (invoke "pytest" "--tb=short" "-v"
                    "--config-file=tests/pytest.ini"
                    "tests"
                    #$@extra-test-arguments))))))

(define (tryton-arguments module . extra-arguments)
  "Like ’tryton-phases’, but directly return all arguments for
the build system."
  (list #:phases (apply tryton-phases module extra-arguments)))

;;;
;;;  Tryton modules - please sort alphabetically
;;;

(define %standard-trytond-native-inputs
  ;; native-inputs required for building and by most of the trytond modules
  ;; for running the test
  (list python-dateutil
        python-genshi
        python-lxml
        python-magic
        python-passlib
        python-polib
        python-proteus
        python-pytest ; see tryton-phases above
        python-setuptools ; for pyproject-build-system
        python-relatorio
        python-sql
        python-werkzeug
        python-wrapt
        tzdata-for-tests))

(define-public trytond-account
  (package
    (name "trytond-account")
    (version "7.0.22")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account" version))
       (sha256
        (base32 "10m6743h2qjdfrbzvcm11bg7b3rk4570vfg5ng0zi69yv8hd8zak"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-simpleeval trytond trytond-company trytond-currency
           trytond-party))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for accounting")
    (description
     "This package provides a Tryton module that defines the fundamentals for
most of accounting needs.")
    (license license:gpl3+)))

(define-public trytond-account-asset
  (package
    (name "trytond-account-asset")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_asset" version))
       (sha256
        (base32 "0v11scsbzgfx9k89dqb3fil9ynccvqxm0ph1jhyhls3i09d1vpga"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_asset"))
    (native-inputs
     (cons* trytond-purchase
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account trytond-account-invoice
           trytond-account-product trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-account-asset")
    (synopsis "Tryton module for assets management")
    (description "The @emph{Account Asset} Tryton module adds the depreciation
of fixed assets.")
    (license license:gpl3+)))

(define-public trytond-account-be
  (package
    (name "trytond-account-be")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_be" version))
       (sha256
        (base32 "1lx2dgvp84ib18z47fk7chxr04jlqm3jyk5pkb32q0air5d95j2f"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_be"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-account-eu))
    (home-page "https://docs.tryton.org/projects/modules-account-be")
    (synopsis "Tryton module with Belgian chart of accounts")
    (description "The @emph{Account BE} Tryton module defines the standard
chart of account for Belgium.")
    (license license:gpl3+)))

(define-public trytond-account-budget
  (package
    (name "trytond-account-budget")
    (version "7.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_account_budget" version))
              (sha256
               (base32 "0ycxy7p54gssw4mzb62gmlkhkyfa9s8bqaps30zyw0jd79pll2xy"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_budget"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs (list trytond trytond-account
                             trytond-company trytond-currency))
    (home-page "https://docs.tryton.org/projects/modules-account-budget")
    (synopsis "Tryton module that allows budgets to be setup for accounts")
    (description "The @emph{Account Budget} Tryton module provides the ability
to set budgets for accounts over a defined period of time.  These budgets can
then be used to track the total amount from relevant transactions against the
budgeted amount.")
    (license license:gpl3+)))

(define (standard-trytond-native-inputs 1stmodule . more-modules)
  (apply list 1stmodule more-modules %standard-trytond-native-inputs))

(define-public trytond-account-cash-rounding
  (package
    (name "trytond-account-cash-rounding")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_cash_rounding" version))
       (sha256
        (base32 "1ap0ndymlkazk3q0crn6ll27w8zmq8ykm9j4qvz5i051kf05k250"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_cash_rounding"))
    (native-inputs
     (cons* trytond-account-invoice
            trytond-purchase
            trytond-sale
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account trytond-currency))
    (home-page
     "https://docs.tryton.org/projects/modules-account-cash-rounding")
    (synopsis "Tryton module to round cash amount")
    (description "The @emph{Account Cash Rounding} Tryton module allows cash
amounts to be rounded using the cash rounding factor of the currency.")
    (license license:gpl3+)))

(define-public trytond-account-consolidation
  (package
    (name "trytond-account-consolidation")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_consolidation" version))
       (sha256
        (base32 "07qjw6h2cap1lg712w2yznnbp3swk1m9rspb48c13yyhm5xaf4ii"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_consolidation"))
    (native-inputs
     (cons* trytond-account-invoice
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list
      trytond
      trytond-account
      trytond-company
      trytond-currency))
    (home-page "https://docs.tryton.org/projects/modules-account-consolidation")
    (synopsis "Tryton module to consolidate accounting of many companies")
    (description "The @emph{Account Consolidation} Tryton module allows
consolidate accounting report of multiple companies.")
    (license license:gpl3+)))

(define-public trytond-account-credit-limit
  (package
    (name "trytond-account-credit-limit")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_credit_limit" version))
       (sha256
        (base32 "12p2ryn9lywnxm6839wv9s9jgx8k6cnjaffdxyv2m0vkj2hpvm7r"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_credit_limit"))
    (native-inputs
     (cons* trytond-account-dunning
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account trytond-company trytond-currency
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-credit-limit")
    (synopsis "Tryton module for account credit limit")
    (description "The @emph{Account Credit Limit} Tryton module for manages
credit limit of parties.")
    (license license:gpl3+)))

(define-public trytond-account-de-skr03
  (package
    (name "trytond-account-de-skr03")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_de_skr03" version))
       (sha256
        (base32 "0id7qvjahgnv3nn0q5sb451zfa61qd04cgpf24y9jcbscb42bs5z"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_de_skr03"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account))
    (home-page "https://docs.tryton.org/projects/modules-account-de-skr03")
    (synopsis "Tryton module with German chart of accounts SKR03")
    (description "This package provides the German SKR03 chart of accounts for
Tryton.")
    (license license:gpl3+)))

(define-public trytond-account-deposit
  (package
    (name "trytond-account-deposit")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_deposit" version))
       (sha256
        (base32 "055v9b30q1drrkzq6xdcd5v20ysa7mqqsvzibrrbjjapx9hi6vp2"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_deposit"))
    (native-inputs
     (cons* trytond-account-payment-clearing
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account trytond-account-invoice
           trytond-company trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-deposit")
    (synopsis "Tryton module for accounting deposit")
    (description "The @emph{Account Deposit} Tryton module adds support for
deposit accounting.

A deposit is an amount paid by the customer prior to the company providing it
with services or goods.  A wizard on invoice allows recalling a prior deposit of
the party.")
    (license license:gpl3+)))

(define-public trytond-account-dunning
  (package
    (name "trytond-account-dunning")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_dunning" version))
       (sha256
        (base32 "0vxz71gwp2s5cm6y8c770aaw4mwgbdf8brl75p404lp6mwh3fqay"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_dunning"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-company trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-dunning")
    (synopsis "Tryton module for account dunning")
    (description "The @emph{Account Dunning} Tryton module adds dunning for
receivable move lines.")
    (license license:gpl3+)))

(define-public trytond-account-dunning-email
  (package
    (name "trytond-account-dunning-email")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_dunning_email" version))
       (sha256
        (base32 "0ry043b8fa0sphamm7xaybf836c5q75w18158zgzg9wm2xbbqjsg"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_dunning_email"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-dunning trytond-account-dunning-letter
           trytond-party))
    (home-page
     "https://docs.tryton.org/projects/modules-account-dunning-email")
    (synopsis "Tryton module for account dunning email")
    (description "This package provides a Tryton module for sending dunning
emails.")
    (license license:gpl3+)))

(define-public trytond-account-dunning-fee
  (package
    (name "trytond-account-dunning-fee")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_dunning_fee" version))
       (sha256
        (base32 "00rvw1720rz9x2kl6kss0w85w3ch8lygp4nfljw5hqw1fyxwsq2z"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_dunning_fee"))
    (native-inputs
     (cons* trytond-account-dunning-letter
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account-dunning trytond-account-product))
    (home-page "https://docs.tryton.org/projects/modules-account-dunning-fee")
    (synopsis "Tryton module for account dunning fee")
    (description "This package provides a Tryton module for generating
accounting moves as fees when processing dunning.")
    (license license:gpl3+)))

(define-public trytond-account-dunning-letter
  (package
    (name "trytond-account-dunning-letter")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_dunning_letter" version))
       (sha256
        (base32 "0ad0jyxmqiyfc5kgxrfzipllxmcn8kzgxmnjjil7w2pv3fw5jcr8"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_dunning_letter"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-account-dunning
           trytond-company trytond-party))
    (home-page
     "https://docs.tryton.org/projects/modules-account-dunning-letter")
    (synopsis "Tryton module for account dunning letter")
    (description "This package provides a Tryton module for generating dunning
letters.")
    (license license:gpl3+)))

(define-public trytond-account-es
  (package
    (name "trytond-account-es")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_es" version))
       (sha256
        (base32 "009wx8ihx8i976hpfpqjlp0gbvfzpdgmpdigwla19416yqjkpvka"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_es"))
    (native-inputs
     (cons* trytond-account-asset
            trytond-account-payment-sepa
            trytond-sale-advance-payment
            trytond-sale-gift-card
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-phonenumbers
           trytond
           trytond-account
           trytond-account-eu
           trytond-account-invoice
           trytond-company
           trytond-currency
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-es")
    (synopsis "Tryton with Spanish chart of accounts")
    (description "This package provides the following Spanish charts of
accounts for Tryton:
@itemize
@item Plan General Contable Español 2008
@item Plan Contable para PYMES 2008
@end itemize

A wizard allows generating the following AEAT files:

@itemize
@item Modelo 111
@item Modelo 115
@item Modelo 303
@end itemize")
    (license license:gpl3+)))

(define-public trytond-account-es-sii
  (package
    (name "trytond-account-es-sii")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_es_sii" version))
       (sha256
        (base32 "1dl8wfzcqyp8zdbxzj264hmgmdy85i74fnmx88vqrrchzmk2g9a4"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_es_sii"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-requests
           python-zeep
           trytond
           trytond-account
           trytond-account-es
           trytond-account-invoice))
    (home-page "https://docs.tryton.org/projects/modules-account-es-sii")
    (synopsis "Tryton module that sends invoices to the Spanish SII webservice")
    (description "The @emph{Account Spanish SII} Tryton module allows sending
invoices to the SII portal.  This is legal requirement for some Spanish
companies.")
    (license license:gpl3+)))

(define-public trytond-account-eu
  (package
    (name "trytond-account-eu")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_eu" version))
       (sha256
        (base32 "115pvxvb4wnlsnbz8qk9z7a9kgvgzjfkxdyvlyffpllq5fwj5dym"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_eu"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-company
           trytond-currency
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-eu")
    (synopsis "Tryton module for european accounting")
    (description "This package provides a Tryton module implementing common
accounting requirements in Europe.  It includes:

@itemize
@item EC Sales List (ESL)
@end itemize")
    (license license:gpl3+)))

(define-public trytond-account-fr
  (package
    (name "trytond-account-fr")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_fr" version))
       (sha256
        (base32 "0cigsrh2d2bj9h87lxmi8hancghwhxs3mam581knygi8r4n1c7aj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases #$(tryton-phases "account_fr")
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; doctests uses '__file__' which is unset in pytest
                (invoke "python" "-m" "unittest" "discover")))))))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-party-siret))
    (home-page "https://docs.tryton.org/projects/modules-account-fr")
    (synopsis "Tryton module with French chart of accounts")
    (description "This package provides the French standard chart of account
for Tryton.")
    (license license:gpl3+)))

(define-public trytond-account-fr-chorus
  (package
    (name "trytond-account-fr-chorus")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_fr_chorus" version))
       (sha256
        (base32 "1cim1zy34inp9162lcblldwfksxhqvcmcndgvj9l75dsrv53zg9h"))))
    (build-system pyproject-build-system)
    ;; doctest requires network and an api key
    (arguments (tryton-arguments "account_fr_chorus" "-k not scenario"))
    (native-inputs
     (cons* trytond-account-fr
            trytond-edocument-uncefact
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-oauthlib
           python-requests-oauthlib
           trytond
           trytond-account
           trytond-account-invoice
           trytond-company
           trytond-party
           trytond-party-siret))
    (home-page "https://docs.tryton.org/projects/modules-account-fr-chorus")
    (synopsis "Tryton module to communicate with the French Chorus Pro
portal")
    (description "This package provides a Tryton module to send invoices
through the French Chorus Pro portal.

If the party is checked for Chorus Pro, all posted customer invoices are
queued to be sent.  A cron job will send them every 15 minutes by default,
using the credential from the accounting configuration.")
    (license license:gpl3+)))

(define-public trytond-account-invoice
  (package
    (name "trytond-account-invoice")
    (version "7.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice" version))
       (sha256
        (base32 "0s2slws07s3gfarjnc8ps65w9zgpmrr0xj51vlb9rykswynf7515"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_invoice"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-product
           trytond-company
           trytond-currency
           trytond-party
           trytond-product))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for invoicing")
    (description
     "This package provides a Tryton module that adds the invoice, payment
term.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-correction
  (package
    (name "trytond-account-invoice-correction")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_correction" version))
       (sha256
        (base32 "154nyf29lm74wlsad5byh9c96nszscsb31jc4hxi8j91n93f2l6j"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_invoice_correction"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-correction")
    (synopsis "Tryton module to correct invoice")
    (description "The @emph{Account Invoice Correction} Tryton module adds a
wizard on invoice which allows select lines for which the unit price must be
corrected.  A new invoice is created with those lines in double: once with the
original quantity, once with the inverted quantity.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-defer
  (package
    (name "trytond-account-invoice-defer")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_defer" version))
       (sha256
        (base32 "1q281cl0dhmm5d1cc4sjpx6wk97vrva35sldxwydrpw460df9lh9"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_invoice_defer"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-account-invoice
           trytond-company))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-defer")
    (synopsis "Tryton module to defer expense and revenue")
    (description "The @emph{Account Invoice Defer} Tryton module allows
deferring the expense or the revenue of an invoice line over many periods.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-history
  (package
    (name "trytond-account-invoice-history")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_history" version))
       (sha256
        (base32 "07ggrwqq6dz7c49bc20ly7nxznw3sm9q5cmskjycmfx6h72j5l8y"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_invoice_history"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-party))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-history")
    (synopsis "Tryton module to historize invoices")
    (description "The @emph{Account Invoice History} Tryton module activates
the historization of the invoice and its related fields.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-line-standalone
  (package
    (name "trytond-account-invoice-line-standalone")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_line_standalone" version))
       (sha256
        (base32 "1gihaiabl9pk9k8b9hmd0b2ih7i6dmyi2p3s1ycl395y8rc3d58d"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_invoice_line_standalone"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-line-standalone")
    (synopsis "Tryton module to have standalone invoice lines")
    (description "The @emph{Account Invoice Line Standalone} Tryton module
allows creating an invoice line not linked to an invoice.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-secondary-unit
  (package
    (name "trytond-account-invoice-secondary-unit")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_secondary_unit" version))
       (sha256
        (base32 "1qqr70xw5ybn1xywgdznqc2lqsvs592qiam0rr75zyjzv3g3bfxc"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_invoice_secondary_unit"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-product))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-secondary-unit")
    (synopsis "Tryton module to add a secondary unit on invoice line")
    (description "The @emph{Account Invoice Secondary Unit} Tryton module adds
a secondary unit of measure on invoice line.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-stock
  (package
    (name "trytond-account-invoice-stock")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_stock" version))
       (sha256
        (base32 "10nysdhm770m4s4nqn8zp8f5ajpi5aqhd3vp4lsfib940lgwrzbx"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_invoice_stock"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-product trytond-stock))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module to link stock and invoice")
    (description
     "This package provides a Tryton module that adds link between invoice
lines and stock moves.  The unit price of the stock move is updated with the
average price of the posted invoice lines that are linked to it.")
    (license license:gpl3+)))


(define-public trytond-account-invoice-watermark
  (package
    (name "trytond-account-invoice-watermark")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_watermark" version))
       (sha256
        (base32 "00b0s9mzdxafw6hfl4xg3wvb6w3k8zw98knirig4ksy04xr4w4zp"))))
    (build-system pyproject-build-system)
    ;; doctest would required libreoffice
    (arguments (tryton-arguments "account_invoice_watermark"
                "-k not scenario_account_invoice_watermark.rst"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-pypdf
           trytond
           trytond-account-invoice))
    (home-page "https://docs.tryton.org/projects/modules-account-invoice-watermark")
    (synopsis "Tryton module to add a watermark to invoices")
    (description "The @emph{Account Invoice Watermark} Tryton module adds a
\"draft\" or \"paid\" watermark to the printed invoice.")
    (license license:gpl3+)))

(define-public trytond-account-move-line-grouping
  (package
    (name "trytond-account-move-line-grouping")
    (version "7.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_account_move_line_grouping" version))
              (sha256
               (base32 "167z9djgraijy7py64ic9a4y5dp2f57j5j349lpz857qr75zxrv3"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_move_line_grouping"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs (list trytond trytond-account))
    (home-page "https://docs.tryton.org/projects/modules-account-move-line-grouping")
    (synopsis "Tryton module to display account move lines grouped")
    (description "The @emph{Account Move Line Grouping} Tryton module adds a
view that displays move lines grouped.")
    (license license:gpl3+)))

(define-public trytond-account-payment
  (package
    (name "trytond-account-payment")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment" version))
       (sha256
        (base32 "06hm5lwp3y9azzjwwrk52r28qw5hv440wn42iv5xg50jm4dzag8d"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases #$(tryton-phases "account_payment")
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; DB_CACHE and pytest don't work together here
                (invoke "python" "-m" "unittest" "discover")))))))
    (native-inputs
     (cons* trytond-account-dunning
            trytond-account-invoice
            trytond-account-statement
            trytond-account-statement-rule
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account trytond-company trytond-currency
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-payment")
    (synopsis "Tryton module for payment")
    (description "This package provides a Tryton module for generating grouped
payments for receivable or payable Account Move Lines.")
    (license license:gpl3+)))

(define-public trytond-account-payment-braintree
  (package
    (name "trytond-account-payment-braintree")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_braintree" version))
       (sha256
        (base32 "0kix59xjfdq9wa6f1aknf9rf1a8zdf5a8x1r765349w5xlla66mk"))))
    (build-system pyproject-build-system)
    ;; doctest requires network and an api key
    (arguments (tryton-arguments "account_payment_braintree"
                                 "-k not scenario"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-braintree trytond trytond-account
           trytond-account-payment trytond-party))
    (home-page
     "https://docs.tryton.org/projects/modules-account-payment-braintree")
    (synopsis "Tryton module for Braintree payment")
    (description "The @emph{Account Payment Braintree} Tryton module allows
receipt of payments using Braintree.  It uses the Drop-in UI in a checkout
form to handle the payment method nonce for card and other supported payment
methods.")
    (license license:gpl3+)))

(define-public trytond-account-payment-clearing
  (package
    (name "trytond-account-payment-clearing")
    (version "7.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_clearing" version))
       (sha256
        (base32 "0ghbz7d4l6dywnb4qclldf2ynv98hpp9451zwad83bigfyyf0h40"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_payment_clearing"))
    (native-inputs
     (cons* trytond-account-invoice
            trytond-account-statement
            trytond-account-statement-rule
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account trytond-account-payment trytond-company))
    (home-page
     "https://docs.tryton.org/projects/modules-account-payment-clearing")
    (synopsis "Tryton module for payment clearing")
    (description "The @emph{Account Payment Clearing} Tryton module allows
generating an account move when a payment succeeded between the
receivable/payable account to a clearing account defined on the payment
journal.")
    (license license:gpl3+)))

(define-public trytond-account-payment-sepa
  (package
    (name "trytond-account-payment-sepa")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_sepa" version))
       (sha256
        (base32 "0lqpwcaky6dsgghskmsz5d2qqdwx1j8z2vz6wi27rmsgz840ssvg"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_payment_sepa"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-stdnum
           trytond
           trytond-account-payment
           trytond-bank
           trytond-company
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-payment-sepa")
    (synopsis "Tryton module for SEPA payment")
    (description "The @emph{Account Payment SEPA} Tryton module allows
generating SEPA files for a Payment Group.")
    (license license:gpl3+)))

(define-public trytond-account-payment-sepa-cfonb
  (package
    (name "trytond-account-payment-sepa-cfonb")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_sepa_cfonb" version))
       (sha256
        (base32 "0ic3j1rzb8fsxxrcxzpnahjzrz7w7zsbmwjhcaivryvbgyzdzq89"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_payment_sepa_cfonb"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account-payment
           trytond-account-payment-sepa
           trytond-bank
           trytond-company
           trytond-party))
    (home-page
     "https://docs.tryton.org/projects/modules-account-payment-sepa-cfonb")
    (synopsis "Tryton module for CFONB SEPA payment")
    (description "The @emph{account payment sepa cfonb} Tryton module adds
CFONB flavors to SEPA messages.")
    (license license:gpl3+)))

(define-public trytond-account-payment-stripe
  (package
    (name "trytond-account-payment-stripe")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_stripe" version))
       (sha256
        (base32 "0ddljzik0sk5dyvhfvamxcxw1zcmzw4lwx6jdlryxhq1z82qvcyv"))))
    (build-system pyproject-build-system)
    ;; doctest requires network and an api key
    (arguments (tryton-arguments "account_payment_stripe" "-k not scenario"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-stripe-12 trytond trytond-account trytond-account-payment
           trytond-party))
    (home-page
     "https://docs.tryton.org/projects/modules-account-payment-stripe")
    (synopsis "Tryton module for Stripe payment")
    (description "The @emph{Account Payment Stripe} Tryton module for
receiving payments from Stripe.  It uses Stripe.js and Stripe Elements in a
checkout form to handle Setup Intent and Payment Intent by card.")
    (license license:gpl3+)))

(define-public trytond-account-product
  (package
    (name "trytond-account-product")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_product" version))
       (sha256
        (base32 "17hgqn52li1sdwl9ppa6h2fx1x3hvnbaqb2lvpi0r97zz92wc75x"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_product"))
    (native-inputs
     (cons* trytond-analytic-account
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account trytond-company trytond-product))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module to add accounting on product")
    (description
     "This package provides a Tryton module that adds accounting on product
and category.")
    (license license:gpl3+)))

(define-public trytond-account-receivable-rule
  (package
    (name "trytond-account-receivable-rule")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_receivable_rule" version))
       (sha256
        (base32 "1a7awr71gwndp367vjam45482555hb2fv8j3z9v2x0q5lvjv3v40"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_receivable_rule"))
    (native-inputs
     (cons* trytond-account-statement
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-company
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-receivable-rule")
    (synopsis "Tryton module to enforce receivable rules")
    (description "The @emph{Account Receivable Rule} Tryton module allows
defining rules to reconcile receivables between accounts.")
    (license license:gpl3+)))

(define-public trytond-account-rule
  (package
    (name "trytond-account-rule")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_account_rule" version))
              (sha256
               (base32 "0p9blifdzx97arzs8djpqqgg3lpyk8saxjd4q4xxzjghk22gf04c"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_rule"))
    (native-inputs
     (cons* trytond-account-invoice-stock
            trytond-account-stock-anglo-saxon
            trytond-account-stock-continental
            trytond-product
            trytond-purchase
            trytond-purchase-shipment-cost
            trytond-sale
            trytond-sale-gift-card
            trytond-stock
            trytond-stock-consignment
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-company
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-rule")
    (synopsis "Tryton module to change accounts based on rules")
    (description "The @emph{Account Rule} Tryton module allows rules which
substitute default accounts with other accounts.")
    (license license:gpl3+)))

(define-public trytond-account-statement
  (package
    (name "trytond-account-statement")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement" version))
       (sha256
        (base32 "10byw4b1h1kfdrz3fzs10pvmi9vjzwhd6ih76jczydsqz8nl8dps"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_statement"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-bank
           trytond-company
           trytond-currency
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-statement")
    (synopsis "Tryton module with account statements")
    (description "The @emph{Account Statement} Tryton module allows booking
statements.  Statement can be used for bank statement, cash daybook etc.")
    (license license:gpl3+)))

(define-public trytond-account-statement-aeb43
  (package
    (name "trytond-account-statement-aeb43")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_aeb43" version))
       (sha256
        (base32 "0vgcbh7gfgcdqc7fcc90hsib0d5sbianw53vxb61bax8dgzj4cyr"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_statement_aeb43"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-csb43-0.10 trytond trytond-account-statement trytond-bank))
    (home-page
     "https://docs.tryton.org/projects/modules-account-statement-aeb43")
    (synopsis "Tryton module to import AEB43 statements")
    (description "The @emph{Account Statement AEB43} Tryton module implements
the import of @emph{Norm 43} files as statement.  @emph{Norm 43} is a standard
defined by the Spanish banking association.")
    (license license:gpl3+)))

(define-public trytond-account-statement-coda
  (package
    (name "trytond-account-statement-coda")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_coda" version))
       (sha256
        (base32 "1w38h95v9ix4p7qmvwbfn6mi36r2x6rskj6mci8wdhkl55szyf80"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_statement_coda"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-febelfin-coda trytond trytond-account-statement
           trytond-bank))
    (home-page
     "https://docs.tryton.org/projects/modules-account-statement-coda")
    (synopsis "Tryton module to import CODA statements")
    (description "The @emph{Account Statement CODA} Tryton module implements
the import of @emph{CODA} files as statement.  @emph{CODA} is a standard
defined by Belgian \"febelfin\".")
    (license license:gpl3+)))

(define-public trytond-account-statement-mt940
  (package
    (name "trytond-account-statement-mt940")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_mt940" version))
       (sha256
        (base32 "1lkcv0h2qm2vgnjf8lyvwqcj48xgrkmsna57k5rc9hzj5pr5fnqp"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_statement_mt940"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-mt940
           trytond
           trytond-account-statement
           trytond-bank))
    (home-page "https://docs.tryton.org/projects/modules-account-statement-mt940")
    (synopsis "Tryton module to import MT940 statements")
    (description "The @emph{Account Statement MT940} Tryton module implements
the import of MT940 files as statements.")
    (license license:gpl3+)))

(define-public trytond-account-statement-ofx
  (package
    (name "trytond-account-statement-ofx")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_ofx" version))
       (sha256
        (base32 "00dg1dq71qllhnq9ngjb4br3y6qa34hi3gjx93ibwkfy0rj84hni"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_statement_ofx"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-ofxparse trytond trytond-account-statement trytond-bank
           trytond-party))
    (home-page
     "https://docs.tryton.org/projects/modules-account-statement-ofx")
    (synopsis "Tryton module to import OFX statements")
    (description "The @emph{Account Statement OFX} Tryton module implements
the import of the @emph{OFX} files as statement.")
    (license license:gpl3+)))

(define-public trytond-account-statement-rule
  (package
    (name "trytond-account-statement-rule")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_rule" version))
       (sha256
        (base32 "1yg4f29hpq28rrcgf358f5z2hcxiyawwznrxi9rl879x4jb8yg7r"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_statement_rule"))
    (native-inputs
     (cons* trytond-bank
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-account-statement
           trytond-company
           trytond-party))
    (home-page
     "https://docs.tryton.org/projects/modules-account-statement-rule")
    (synopsis "Tryton module to automate statement import with rules")
    (description "The @emph{Account Statement Rule} Tryton module allows rules
to be defined to complete statement lines from imported files.

When the @emph{Apply Rule} button is clicked on a statement, each rule is
tested in order against each origin that does not have any lines until one is
found that matches.  Then the rule found is used to create the statement lines
linked to the origin.")
    (license license:gpl3+)))

(define-public trytond-account-statement-sepa
  (package
    (name "trytond-account-statement-sepa")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_sepa" version))
       (sha256
        (base32 "1gzkljcd5gbwk2vqmzz2zzh5sy7qjbmqviilaribv9n9n6n310dv"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_statement_sepa"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account-statement
           trytond-bank))
    (home-page "https://docs.tryton.org/projects/modules-account-statement-sepa")
    (synopsis "Tryton module to import SEPA statements")
    (description "The @emph{Account Statement SEPA} Tryton module implements
the import of the CAMT.052, CAMT.053 and CAMT.054 SEPA files as statement.")
    (license license:gpl3+)))

(define-public trytond-account-stock-anglo-saxon
  (package
    (name "trytond-account-stock-anglo-saxon")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_anglo_saxon" version))
       (sha256
        (base32 "088qygd6abz63dhd241mg5px2wwzzq7bv1jj3y0prncxzzyrkl9j"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_stock_anglo_saxon"))
    (native-inputs
     (cons* trytond-purchase
            trytond-sale
            trytond-sale-supply-drop-shipment
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-account-invoice-stock
           trytond-account-product
           trytond-account-stock-continental))
    (home-page
     "https://docs.tryton.org/projects/modules-account-stock-anglo-saxon")
    (synopsis "Tryton module for anglo-saxon real-time stock valuation")
    (description "The @emph{Account Stock Anglo Saxon} Tryton module adds the
anglo-saxon accounting model for stock valuation.")
    (license license:gpl3+)))

(define-public trytond-account-stock-continental
  (package
    (name "trytond-account-stock-continental")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_continental" version))
       (sha256
        (base32 "1hwfv06mg0x1zhpqkbvywkyfdj3fn9p92yxfw6f28da4n3k6id1r"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_stock_continental"))
    (native-inputs
     (cons* trytond-account-invoice
            trytond-purchase
            trytond-sale
            trytond-sale-supply-drop-shipment
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account trytond-account-product trytond-product
           trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-account-stock-continental")
    (synopsis "Tryton module for continental real-time stock valuation")
    (description "The @emph{Account Stock Continental} Tryton module adds the
continental accounting model for stock valuation.")
    (license license:gpl3+)))

(define-public trytond-account-stock-eu
  (package
    (name "trytond-account-stock-eu")
    (version "7.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_eu" version))
       (sha256
        (base32 "15mp546dn764cbna72z9wwq7nw4w78p44hw56wlmvd5hwkaq2i9w"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_stock_eu"))
    (native-inputs
     (cons* trytond-carrier
            trytond-incoterm
            trytond-production
            trytond-purchase-shipment-cost
            trytond-stock-consignment
            trytond-stock-package-shipping
            trytond-stock-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-company
           trytond-country
           trytond-currency
           trytond-customs
           trytond-party
           trytond-product
           trytond-product-measurements
           trytond-stock
           trytond-stock-shipment-measurements))
    (home-page "https://docs.tryton.org/projects/modules-account-stock-eu")
    (synopsis "Tryton module for European stock accounting")
    (description "The @emph{Account Stock EU} Tryton module is used to
generate the Intrastat declarations every month.")
    (license license:gpl3+)))

(define-public trytond-account-stock-landed-cost
  (package
    (name "trytond-account-stock-landed-cost")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_landed_cost" version))
       (sha256
        (base32 "1wjd6c4dp266b5vcw99yas2p42mr86mdzir110w0s3ml46qqwr1j"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_stock_landed_cost"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-account-invoice
           trytond-product trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-account-stock-landed-cost")
    (synopsis "Tryton module for landed cost")
    (description "The @emph{Account Stock Landed Cost} Tryton module allows
allocating landed cost on Supplier Shipments after their reception.")
    (license license:gpl3+)))

(define-public trytond-account-stock-landed-cost-weight
  (package
    (name "trytond-account-stock-landed-cost-weight")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_landed_cost_weight" version))
       (sha256
        (base32 "0msa79mf81prf14p4hfy1b98a9w9x49j138r5bc3c99ymmc3vljk"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_stock_landed_cost_weight"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-stock-landed-cost trytond-product
           trytond-product-measurements trytond-stock-shipment-measurements))
    (home-page
     "https://docs.tryton.org/projects/modules-account-stock-landed-cost-weight")
    (synopsis "Tryton module for landed cost per weight")
    (description "The @emph{Account Stock Landed Cost Weight} Tryton module
adds an allocation method based on weight of each line.  The Weight is taken
from the Product Measurements")
    (license license:gpl3+)))

(define-public trytond-account-stock-shipment-cost
  (package
    (name "trytond-account-stock-shipment-cost")
    (version "7.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_account_stock_shipment_cost" version))
              (sha256
               (base32 "0gxjhry3hpm0j72ai22q9fgay9w5087xnrisc18ynwv4zzyr84vq"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_stock_shipment_cost"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-product
           trytond-stock
           trytond-stock-shipment-cost))
    (home-page "https://docs.tryton.org/projects/modules-account-stock-shipment-cost")
    (synopsis "Tryton module to allocate shipment cost based on invoice")
    (description "The @emph{Account Stock Shipment Cost} Tryton module
allocates shipment cost based on invoice.")
    (license license:gpl3+)))

(define-public trytond-account-stock-shipment-cost-weight
  (package
    (name "trytond-account-stock-shipment-cost-weight")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_shipment_cost_weight" version))
       (sha256
        (base32 "0nlfz0azn6y16i0qy8mwpsdz2ckh554h640warfnydmmbj1hqkni"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_stock_shipment_cost_weight"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account-stock-shipment-cost
           trytond-stock-shipment-measurements))
    (home-page
     "https://docs.tryton.org/projects/modules-account-stock-shipment-cost-weight")
    (synopsis "Tryton module to allocate shipment cost \"by weight\"")
    (description "The @emph{Account Stock Shipment Cost Weight} Tryton module
adds “by weight” as allocation method on shipment cost.")
    (license license:gpl3+)))

(define-public trytond-account-tax-cash
  (package
    (name "trytond-account-tax-cash")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_tax_cash" version))
       (sha256
        (base32 "0q2nr9fwfb0jv4yiy2xq3r3065lvsji5zj9aj9arbd9kwpzqi51p"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_tax_cash"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-account-invoice trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-account-tax-cash")
    (synopsis "Tryton module to support tax report on cash basis")
    (description "The @emph{Account Tax Cash} Tryton module allows making a tax
report on cash basis.")
    (license license:gpl3+)))

(define-public trytond-account-tax-non-deductible
  (package
    (name "trytond-account-tax-non-deductible")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_tax_non_deductible" version))
       (sha256
        (base32 "0frlix01zfwqkx33bvp7wh888gvzf00dh86027pzjbqq5k890mpr"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_tax_non_deductible"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice))
    (home-page "https://docs.tryton.org/projects/modules-account-tax-non-deductible")
    (synopsis "Tryton module to report non-deductible taxes")
    (description "The @emph{Account Tax Non Deductible} Tryton module allows
to define non-deductible taxes and reports them.")
    (license license:gpl3+)))

(define-public trytond-account-tax-rule-country
  (package
    (name "trytond-account-tax-rule-country")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_tax_rule_country" version))
       (sha256
        (base32 "0jpp11rlskpqp3dzfm8ml2kqn4855vrcrcmwk6jrjkpsh2m8f6s3"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "account_tax_rule_country"))
    (native-inputs
     (cons* trytond-account-invoice
            trytond-purchase
            trytond-sale
            trytond-stock
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account trytond-country))
    (home-page
     "https://docs.tryton.org/projects/modules-account-tax-rule-country")
    (synopsis "Tryton module to add countries on tax rules")
    (description "The @emph{Account Tax Rule Country} Tryton module extends
the tax rule to add origin and destination countries and subdivisions as
criteria.")
    (license license:gpl3+)))

(define-public trytond-analytic-account
  (package
    (name "trytond-analytic-account")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_account" version))
       (sha256
        (base32 "14qrx6nf1x939sd20hm905bfj9r1gjfpbs8cs9myjwxrqq2m191l"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "analytic_account"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-company trytond-currency
           trytond-party))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for analytic accounting")
    (description
     "This package provides a Tryton module that adds the fundamentals
required to analyse accounting using multiple different axes.")
    (license license:gpl3+)))

(define-public trytond-analytic-budget
  (package
    (name "trytond-analytic-budget")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_analytic_budget" version))
              (sha256
               (base32 "1j3lxb3nxhis7w4snds0m9yvr70mcr2mj8rbs3vvnmg99ysgrrci"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "analytic_budget"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs (list trytond trytond-account-budget
                             trytond-analytic-account
                             trytond-company))
    (home-page "https://docs.tryton.org/projects/modules-analytic-budget")
    (synopsis "Allow creating budgets for analytic accounts in Tryton")
    (description "The @emph{Analytic Budget} Tryton module provides the
ability to set budgets for analytic accounts over a defined period of time.
These budgets can then be used to track the total amount from relevant
transactions against the budgeted amount.")
    (license license:gpl3+)))

(define-public trytond-analytic-invoice
  (package
    (name "trytond-analytic-invoice")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_invoice" version))
       (sha256
        (base32 "0cj20yzaxsmc8ifzqk1d1vvs1bb8r3dlivpqkq0gdjn3mqblfplx"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "analytic_invoice"))
    (native-inputs
     (cons* trytond-account-asset
            trytond-account-invoice-defer
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-analytic-account))
    (home-page "https://docs.tryton.org/projects/modules-analytic-invoice")
    (synopsis "Tryton module to add analytic accounting on invoice")
    (description "The @emph{Analytic Invoice} Tryton module allows setting
analytic accounts on an invoice line.")
    (license license:gpl3+)))

(define-public trytond-analytic-purchase
  (package
    (name "trytond-analytic-purchase")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_purchase" version))
       (sha256
        (base32 "1vpvn3k5zfdhkbv40q91ln1i2014xfcqhi7p4rky1hh9yy2szjl2"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "analytic_purchase"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-analytic-account trytond-analytic-invoice
           trytond-purchase))
    (home-page "https://docs.tryton.org/projects/modules-analytic-purchase")
    (synopsis "Tryton module to add analytic accounting on purchase")
    (description "The @emph{Analytic Purchase} Tryton module allows setting
analytic accounts on a purchase line.")
    (license license:gpl3+)))

(define-public trytond-analytic-sale
  (package
    (name "trytond-analytic-sale")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_sale" version))
       (sha256
        (base32 "06ydgb2x3mwp3g1nnsmm1k449nnn29whl82hgvlp1n5z58lcjmvh"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "analytic_sale"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-analytic-account trytond-analytic-invoice
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-analytic-sale")
    (synopsis "Tryton module to add analytic accounting on sale")
    (description "The @emph{Analytic Sale} Tryton module allows setting
analytic accounts on a sale line.")
    (license license:gpl3+)))

(define-public trytond-attendance
  (package
    (name "trytond-attendance")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_attendance" version))
       (sha256
        (base32 "1j5pldlgdkhllsqlnpjd4l56h933pyfwllxrfr1dd47730ky7rw9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases #$(tryton-phases "attendance")
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; DB_CACHE and pytest don't work together here
                (invoke "python" "-m" "unittest" "discover")))))))
    (native-inputs
     (cons* trytond-timesheet
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-company))
    (home-page "https://docs.tryton.org/projects/modules-attendance")
    (synopsis "Tryton module for recording employee attendance")
    (description "The @emph{Attendance} Tryton module allows you to track the
entry and exit time of employees.  The module also comes with a sheet that
shows for each employee the total duration per day in the company and the
detail of the time of entrance and exit")
    (license license:gpl3+)))

(define-public trytond-authentication-saml
  (package
    (name "trytond-authentication-saml")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_authentication_saml" version))
       (sha256
        (base32 "0ln7mj4wg3dapa1sdmcm78bm2j7rsivby23xbmf1s83i3jdb4wdm"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "authentication_saml"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-pysaml2
           trytond))
    (home-page "https://docs.tryton.org/projects/modules-authentication-saml")
    (synopsis "Tryton module to authenticate users via SAML")
    (description "The @emph{Authentication SAML} Tryton module allows
delegating the user authentication to an identity provider via SAML.")
    (license license:gpl3+)))

(define-public trytond-authentication-sms
  (package
    (name "trytond-authentication-sms")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_authentication_sms" version))
       (sha256
        (base32 "0gnqnc0mcwspby663h91kf2y7pgw25zms3iqh0gs18sygx291a7y"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "authentication_sms"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond))
    (home-page "https://docs.tryton.org/projects/modules-authentication-sms")
    (synopsis "Tryton module to authenticate users via SMS")
    (description "The @emph{Authentication SMS} Tryton module allows users to
authenticate via SMS.  It adds a new authentication method sms, which can be
used in the list of authentications in the session section of the
configuration file.")
    (license license:gpl3+)))

(define-public trytond-bank
  (package
    (name "trytond-bank")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_bank" version))
       (sha256
        (base32 "0m5agk7dl2f51sg85s885b8wkfynvli9z8bra0i0f1k65q1qcpcp"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "bank"))
    (native-inputs
     (cons* python-schwifty
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-stdnum trytond trytond-currency trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-bank")
    (synopsis "Tryton module with banks")
    (description "The @emph{Bank} Tryton module defines the concept of bank
and account.")
    (license license:gpl3+)))

(define-public trytond-carrier
  (package
    (name "trytond-carrier")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_carrier" version))
       (sha256
        (base32 "0n5jpry7yjc0w1h9z4kkp498v46hbd9gn76b4gwypscd7anpgziy"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "carrier"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-country trytond-party trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-carrier")
    (synopsis "Tryton module with carriers")
    (description "The @emph{Carrier} Tryton module defines the concept
of carrier.")
    (license license:gpl3+)))

(define-public trytond-carrier-carriage
  (package
    (name "trytond-carrier-carriage")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_carrier_carriage" version))
       (sha256
        (base32 "106a7cixchss6g8j0zclhb35ir87sa5whn6l33jxgvraq7jswy9n"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "carrier_carriage"))
    (native-inputs
     (cons* trytond-account-invoice
            trytond-incoterm
            trytond-purchase-shipment-cost
            trytond-sale-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-carrier
           trytond-stock
           trytond-stock-shipment-cost))
    (home-page "https://docs.tryton.org/projects/modules-carrier-carriage")
    (synopsis "Tryton module to support multiple carriers")
    (description "The @emph{Carrier Carriage} Tryton module extends the
support of carrier by adding carriers before and after the main carrier.")
    (license license:gpl3+)))

(define-public trytond-carrier-percentage
  (package
    (name "trytond-carrier-percentage")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_carrier_percentage" version))
       (sha256
        (base32 "10gp67rdv9qlprq8381k8pxsswc8022idkim2fhxivglghic3i4r"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "carrier_percentage"))
    (native-inputs
     (cons* trytond-purchase-shipment-cost
            trytond-sale-shipment-cost
            trytond-stock-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-carrier trytond-currency))
    (home-page "https://docs.tryton.org/projects/modules-carrier-percentage")
    (synopsis "Tryton module to add cost method based on percentage")
    (description "The @emph{Carrier Percentage} Tryton module adds a cost
method \"on percentage\" on carrier.")
    (license license:gpl3+)))

(define-public trytond-carrier-subdivision
  (package
    (name "trytond-carrier-subdivision")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_carrier_subdivision" version))
       (sha256
        (base32 "0vpp0pk7mh8ir6jbqcyc9q5g872750g8ikhiichph2w9bjjkq2rx"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "carrier_subdivision"))
    (native-inputs
     (cons* trytond-carrier-carriage
            trytond-sale-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-carrier))
    (home-page "https://docs.tryton.org/projects/modules-carrier-subdivision")
    (synopsis "Tryton module that allows carriers selection to be restricted
by subdivision")
    (description "The @emph{Carrier Subdivision} Tryton module extends the
carrier selection pattern with

@itemize
@item the warehouse Subdivision,
@item the customer Subdivision,
@item a regular expression to match against warehouse postal code and
@item A regular expression to match against customer postal code.
@end itemize

These can be used to restrict the usage of a carrier to a specific subdivision
or a specific postal code.")
    (license license:gpl3+)))

(define-public trytond-carrier-weight
  (package
    (name "trytond-carrier-weight")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_carrier_weight" version))
       (sha256
        (base32 "0s6120k2glc18z6jhdm870vh8l575sq11a6sa262bmi1hqgmk2gp"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "carrier_weight"))
    (native-inputs
     (cons* trytond-purchase-shipment-cost
            trytond-sale-shipment-cost
            trytond-stock-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-carrier
           trytond-company
           trytond-currency
           trytond-product
           trytond-product-measurements))
    (home-page "https://docs.tryton.org/projects/modules-carrier-weight")
    (synopsis "Tryton module to add cost method based on weight")
    (description "The @emph{Carrier Weight} Tryton module adds a cost method
\"on weight\" on carrier.  The price is computed by finding the line for which
the weight is greater or equal but smaller than the next line.")
    (license license:gpl3+)))

(define-public trytond-commission
  (package
    (name "trytond-commission")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_commission" version))
       (sha256
        (base32 "0xvlkx45r4mrn86jib9mc98vlwx8c03c9py4dzfhpiy1lngz5hg8"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "commission"))
    (native-inputs
     (cons* trytond-account-invoice-stock
            trytond-sale
            trytond-stock
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-simpleeval
           trytond
           trytond-account
           trytond-account-invoice
           trytond-account-product
           trytond-company
           trytond-party
           trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-commission")
    (synopsis "Tryton module for commission")
    (description "The @emph{Commission} Tryton module allows manageing a
commission for sales agents.  A commission move is created when posting the
invoice, following the agent's commission plan.")
    (license license:gpl3+)))

(define-public trytond-commission-waiting
  (package
    (name "trytond-commission-waiting")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_commission_waiting" version))
       (sha256
        (base32 "0iwsrcp2gxa13mcvqag063wq3fxpq8qm2ws9qkv8v2k6ypbqvp43"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "commission_waiting"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-account-invoice
           trytond-commission))
    (home-page "https://docs.tryton.org/projects/modules-commission-waiting")
    (synopsis "Tryton module for commission waiting")
    (description "The @emph{Commission Waiting} Tryton module allows
generating an account move for each commission between the expense/revenue account
to a waiting account defined on the agent.")
    (license license:gpl3+)))

(define-public trytond-company
  (package
    (name "trytond-company")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_company" version))
       (sha256
        (base32 "19j2msxjis7sw6gvgp89gbhm44364wgl03w0s2i6kd52jy40jpdv"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "company"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-currency trytond-party))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with companies and employees")
    (description
     "This package provides a Tryton module that defines the concepts of
company and employee and extend the user model.")
    (license license:gpl3+)))

(define-public trytond-company-work-time
  (package
    (name "trytond-company-work-time")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_company_work_time" version))
       (sha256
        (base32 "1y6c3p51jp2hcvby12b3pmn0h7jmsd7qigfxznmj17k6bqmm7b6b"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "company_work_time"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-company))
    (home-page "https://docs.tryton.org/projects/modules-company-work-time")
    (synopsis "Tryton module to add work time on company")
    (description "The @emph{Company Work Time} Tryton module adds work time
management.

The Company Work Time module adds 4 new fields (Hours per Work Day, Hours per
Work Week, Hours per Work Month, Hours per Work Year) on the company form that
allows defining how many hours are spent by an employee in a day, a week, a
month and a year of work.")
    (license license:gpl3+)))

(define-public trytond-country
  (package
    (name "trytond-country")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_country" version))
       (sha256
        (base32 "1qvnbq9c0kymzgl82p56z31nvqshsqwqh746z229sbrln39i7miv"))))
    (build-system pyproject-build-system)
    ;; Doctest contains one test that requires internet access.
    (arguments (tryton-arguments "country"
                "-k not scenario_country_import.rst"))
    (native-inputs
     (cons* python-pycountry
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond))
    (home-page "http://www.tryton.org/")
    (synopsis "Tryton module with countries")
    (description
     "This package provides a Tryton module with countries.")
    (license license:gpl3+)))

(define-public trytond-currency
  (package
    (name "trytond-currency")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_currency" version))
       (sha256
        (base32 "1gw82jfvlnbyr3wkh5r8gsd7b7m3jz0szvqqd9fccm1yb229k91w"))))
    (build-system pyproject-build-system)
    ;; Quite some tests require network access.
    (arguments (tryton-arguments "currency"
                "-k not (scenario_currency_rate_update.rst or ECBtestCase)"))
    (native-inputs
     (cons* python-pycountry
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with currencies")
    (description
     "This package provides a Tryton module that defines the concepts of
currency and rate.")
    (license license:gpl3+)))

(define-public trytond-currency-ro
  (package
    (name "trytond-currency-ro")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_currency_ro" version))
              (sha256
               (base32 "1az7w8afhs5cwv4npfa2naizv7mr1mg9c4fgpdg5yp9vlkw3kssz"))))
    (build-system pyproject-build-system)
    ;; doctests require network access
    (arguments (tryton-arguments "currency_ro" "-k not scenario_currency_ro"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-requests
           trytond
           trytond-currency))
    (home-page "https://docs.tryton.org/projects/modules-currency-ro")
    (synopsis "Fetch currency rates from the Romanian National Bank")
    (description "The @emph{Currency RO} Tryton module adds the Romanian
National Bank as a source for currency exchange rates.")
    (license license:gpl3+)))

(define-public trytond-currency-rs
  (package
    (name "trytond-currency-rs")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_currency_rs" version))
              (sha256
               (base32 "1zkb1b9y3j027g46jfwpxb6nv84qds795arsfdzn82zncp9gz4lx"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "currency_rs"
                "-k not (scenario_currency_rs or test_selection_fields)"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-currency
           python-zeep))
    (home-page "https://docs.tryton.org/projects/modules-currency-rs")
    (synopsis "Fetch currency rates from the Serbian National Bank")
    (description "The @emph{Currency RS} Tryton module adds the Serbian
National Bank as a source for currency exchange rates.")
    (license license:gpl3+)))

(define-public trytond-customs
  (package
    (name "trytond-customs")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_customs" version))
       (sha256
        (base32 "0v6rzrdphys3davkc3d46mlq6hkd8xl6x444ddl23fw0b08mvrna"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "customs"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-simpleeval trytond trytond-country trytond-currency
           trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-customs")
    (synopsis "Tryton module for customs")
    (description "The @emph{Customs} Tryton module allows defining customs
duty based on the tariff code.")
    (license license:gpl3+)))

(define-public trytond-dashboard
  (package
    (name "trytond-dashboard")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_dashboard" version))
       (sha256
        (base32 "0cdxmyzs23bdksqgg87y7r4zvpzx63pvjgvhjck6hm49nyicqlmf"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "dashboard"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond))
    (home-page "https://docs.tryton.org/projects/modules-dashboard")
    (synopsis "Tryton module for dashboard")
    (description "The @emph{Dashboard} Tryton module allows users to
configure their dashboard.")
    (license license:gpl3+)))

(define-public trytond-document-incoming
  (package
    (name "trytond-document-incoming")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_document_incoming" version))
       (sha256
        (base32 "18apzwxh8qh0c9gkhjf95avm4f2v5w96rx564j5v2j53a9kp86zj"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "document_incoming"))
    (native-inputs
     (cons* trytond-inbound-email
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-pypdf
           trytond
           trytond-company))
    (home-page "https://docs.tryton.org/projects/modules-document-incoming")
    (synopsis "Tryton module to manage incoming documents")
    (description "The @emph{Document Incoming} Tryton module collects and
process incoming documents.")
    (license license:gpl3+)))

(define-public trytond-document-incoming-invoice
  (package
    (name "trytond-document-incoming-invoice")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_document_incoming_invoice" version))
       (sha256
        (base32 "0fa11cgr8hi1ssirb8c9jgn8li5rwcil7z5ck1n8wpyql5waa6qz"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "document_incoming_invoice"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account-invoice
           trytond-document-incoming
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-document-incoming-invoice")
    (synopsis "Tryton module to manage incoming invoice document")
    (description "The @emph{Document Incoming Invoice} Tryton module creates
supplier invoices from incoming documents.")
    (license license:gpl3+)))

(define-public trytond-document-incoming-ocr
  (package
    (name "trytond-document-incoming-ocr")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_document_incoming_ocr" version))
       (sha256
        (base32 "0gngpviap9yr8lv5x02k9a0s86jj8lm7395sp6kshzq0hbr7q0nk"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "document_incoming_ocr"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-document-incoming))
    (home-page "https://docs.tryton.org/projects/modules-document-incoming-ocr")
    (synopsis "Tryton module to process incoming document with OCR")
    (description "The @emph{Document Incoming OCR} Tryton module provides the
basis to interact with OCR services.")
    (license license:gpl3+)))

(define-public trytond-document-incoming-ocr-typless
  (package
    (name "trytond-document-incoming-ocr-typless")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_document_incoming_ocr_typless" version))
       (sha256
        (base32 "1d1arbkz1clih78sy06kk1qw6a84z2r1zy59mnrrg35irn55kfrl"))))
    (build-system pyproject-build-system)
    ;; doctest requires network and an api key
    (arguments (tryton-arguments "document_incoming_ocr_typless"
                "-k not scenario_document_incoming_ocr_typless.rs" ))
    (native-inputs
     (cons* trytond-document-incoming-invoice
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-requests
           trytond
           trytond-currency
           trytond-document-incoming
           trytond-document-incoming-ocr
           trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-document-incoming-ocr-typless")
    (synopsis "Tryton module that integrates Typless online OCR for incoming document")
    (description "The @emph{Document Incoming OCR Typless} Tryton module
provides integration with Typless online services.")
    (license license:gpl3+)))

(define-public trytond-edocument-uncefact
  (package
    (name "trytond-edocument-uncefact")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_edocument_uncefact" version))
       (sha256
        (base32 "1zk7nzfrsxq3h9n1dkmqpxvjmzax2p0snqhgg99q7zs9s1x9wm2k"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "edocument_uncefact"))
    (native-inputs
     (cons* trytond-account-invoice
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-edocument-unece))
    (home-page "https://docs.tryton.org/projects/modules-edocument-uncefact")
    (synopsis "Tryton module for electronic document UN/CEFACT")
    (description "The @emph{Edocument UN/CEFACT} Tryton module implements
electronic document from UN/CEFACT.  Supported formats are:

@itemize
@item Cross-Industry-Invoice (16B-CII)
@end itemize")
    (license license:gpl3+)))

(define-public trytond-edocument-unece
  (package
    (name "trytond-edocument-unece")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_edocument_unece" version))
       (sha256
        (base32 "1ncagmd0ydvxplvih1bjzmjvx2hpbxyrqfnzwwd6f9ixk9vz60wl"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "edocument_unece"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-edocument-unece")
    (synopsis "Tryton module for electronic document UNECE codes")
    (description "The @emph{Edocument UNECE} Tryton module adds several codes
from the UNECE.  Supported formats are:

@itemize
@item Recommendation N°. 20 Codes for
      Units of Measure Used in International Trade
@item 5153  Duty or tax or fee type name code
@item 5305  Duty or tax or fee category code
@end itemize")
    (license license:gpl3+)))

(define-public trytond-gis
  (package
    (name "trytond-gis")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_gis" version))
       (sha256
        (base32 "1kx62p7jl27ihh0j952f4r0bfn1a1zz15wgvd254yyhc3h57pc7b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f  ;; tests require postgis database
      #:phases (tryton-phases "gis")))
    (native-inputs
     (cons* python-psycopg2
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-geomet
           trytond))
    (home-page "https://docs.tryton.org/projects/backend-gis")
    (synopsis "Geographic Information System support from Tryton")
    (description "The @emph{Trytond GIS} Tryton module adds GIS (Geographic
information system) support to Tryton.")
    (license license:gpl3+)))

(define-public trytond-google-maps
  (package
    (name "trytond-google-maps")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_google_maps" version))
       (sha256
        (base32 "1sb1mp991szg9bfjbn2pvri52sndk33ffx19r7d2cybnkihnhydv"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "google_maps"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs (list trytond trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-google-maps")
    (synopsis "Tryton module to link addresses to Google Maps")
    (description "The @emph{Trytond Google Maps} Tryton module adds a new URL
field on the party addresses.  This link open the Google Maps page on the
default browser with the map centered on the selected address.")
    (license license:gpl3+)))

(define-public trytond-inbound-email
  (package
    (name "trytond-inbound-email")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_inbound_email" version))
       (sha256
        (base32 "0y4zjqr3wrd5pvp8gi270m6r4kfs9djz8hc2iw8mj5p8w5hw8p33"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "inbound_email"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond))
    (home-page "https://docs.tryton.org/projects/modules-inbound-email")
    (synopsis "Tryton module to manage inbound e-mail")
    (description "The @emph{Inbound Email} Tryton module allows defining rules
to apply to inbound e-mails.")
    (license license:gpl3+)))

(define-public trytond-incoterm
  (package
    (name "trytond-incoterm")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_incoterm" version))
       (sha256
        (base32 "1yla0h6d0p7xwznlvfbmy3aam5bcxp18z6190wicfb2sg6qyvqly"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "incoterm"))
    (native-inputs
     (cons* trytond-account
            trytond-account-invoice
            trytond-account-invoice-stock
            trytond-carrier
            trytond-purchase
            trytond-purchase-request-quotation
            trytond-sale
            trytond-sale-invoice-grouping
            trytond-sale-opportunity
            trytond-sale-shipment-cost
            trytond-stock
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-company trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-incoterm")
    (synopsis "Tryton module for incoterms")
    (description "The @emph{Incoterm} Tryton module is used to manage the
Incoterms on sales, purchases and shipments.  The module contains the Incoterm
versions of 2010 and 2020.")
    (license license:gpl3+)))

(define-public trytond-ldap-authentication
  (package
    (name "trytond-ldap-authentication")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_ldap_authentication" version))
       (sha256
        (base32 "0pnmv39hdw4a61ml8s2sh5c9dxxgsmnjfg27w490l0mvrnb394n4"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "ldap_authentication"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-ldap3 trytond))
    (home-page "https://docs.tryton.org/projects/modules-ldap-authentication")
    (synopsis "Tryton module to authenticate users through LDAP")
    (description "The @emph{LDAP Authentication} Tryton module allows
authenticating users via a LDAP server.")
    (license license:gpl3+)))

(define-public trytond-marketing
  (package
    (name "trytond-marketing")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_marketing" version))
       (sha256
        (base32 "1pihlkwa54wjv8hjmsjfmrajfjhdk6ja8pnqkfxqx5s26qzp5x1w"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "marketing"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond))
    (home-page "https://docs.tryton.org/projects/modules-marketing")
    (synopsis "Tryton module to group marketing features")
    (description "The @emph{Marketing} Tryton module defines the
fundamentals for marketing modules.")
    (license license:gpl3+)))

(define-public trytond-marketing-automation
  (package
    (name "trytond-marketing-automation")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_marketing_automation" version))
       (sha256
        (base32 "0cnnaijg9f7l92gir3sbr066b67zzbhqqxxrlypiyk83wwca9l76"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "marketing_automation"))
    (native-inputs
     (cons* trytond-sale
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-marketing
           trytond-party
           trytond-web-shortener))
    (home-page "https://docs.tryton.org/projects/modules-marketing-automation")
    (synopsis "Tryton module to plan, coordinate and manage marketing
campaigns")
    (description "The @emph{Marketing Automation} Tryton module allows
marketing actions to be automated.  It is based on scenarios and activities
that are executed on selected records.")
    (license license:gpl3+)))

(define-public trytond-marketing-campaign
  (package
    (name "trytond-marketing-campaign")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_marketing_campaign" version))
       (sha256
        (base32 "1sabklrxyfx9172813b5q4i74xn4j24hs5pd5gy9lcc0xlyv8bm6"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "marketing_campaign"))
    (native-inputs
     (cons* trytond-marketing-automation
            trytond-marketing-email
            trytond-sale
            trytond-sale-opportunity
            trytond-sale-point
            trytond-web-shortener
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-marketing))
    (home-page "https://docs.tryton.org/projects/modules-marketing-campaign")
    (synopsis "Tryton module to manage marketing campaign")
    (description "The @emph{Marketing Campaign} Tryton module helps collecting
data about marketing campaigns.")
    (license license:gpl3+)))

(define-public trytond-marketing-email
  (package
    (name "trytond-marketing-email")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_marketing_email" version))
       (sha256
        (base32 "00ycqv3x22hxz9kfc10w9z2fn0bicb8p63szj2k4x30val22bxb6"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "marketing_email"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-marketing trytond-party trytond-web-shortener
           trytond-web-user))
    (home-page "https://docs.tryton.org/projects/modules-marketing-email")
    (synopsis "Tryton module to manage marketing mailing lists")
    (description "This package provides a Tryton module for managing marketing
mailing lists.")
    (license license:gpl3+)))

(define-public trytond-notification-email
  (package
    (name "trytond-notification-email")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_notification_email" version))
       (sha256
        (base32 "1vyvxp612wb1v173zmd9jf24kz49aw51399gjpj6mwip06q54r6d"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "notification_email"))
    (native-inputs
     (cons* trytond-commission
            trytond-company
            trytond-party
            trytond-web-user
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond))
    (home-page "https://docs.tryton.org/projects/modules-notification-email")
    (synopsis "Tryton module for sending email notifications")
    (description "The @emph{Notification Email} Tryton module allows defining
email templates which will be sent to a list of recipients when a trigger is
fired on a record event.  Extra reports from the same record can be attached
to the email.")
    (license license:gpl3+)))

(define-public trytond-party
  (package
    (name "trytond-party")
    (version "7.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_party" version))
       (sha256
        (base32 "19mlqkykih95h28r6d9ibmdvh93hxjw4ykgr6svc204rvpjqlbmg"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "party"))
    (native-inputs
     (cons* python-phonenumbers
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-stdnum trytond trytond-country))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for parties and addresses")
    (description
     "This package provides a Tryton module for (counter)parties and
addresses.")
    (license license:gpl3+)))

(define-public trytond-party-avatar
  (package
    (name "trytond-party-avatar")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_party_avatar" version))
       (sha256
        (base32 "1dvr1k5nd4pk8j3ix1x289ln35mlb6hf2i0qn19kyj27nlmb8p99"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "party_avatar"))
    (native-inputs
     (cons* trytond-company
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-party-avatar")
    (synopsis "Tryton module that adds avatars to parties")
    (description "The @emph{Party Avatar} Tryton module adds an avatar to each
party.")
    (license license:gpl3+)))

(define-public trytond-party-relationship
  (package
    (name "trytond-party-relationship")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_party_relationship" version))
       (sha256
        (base32 "1jqxg281ndxhg64bgnzb5rl53cv7clz8q9qs3qqsncj7vawxcwx2"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "party_relationship"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-party-relationship")
    (synopsis "Party Relationship module for Tryton")
    (description "The @emph{Party Relationship} Tryton module allows defining
different types of relations between parties.")
    (license license:gpl3+)))

(define-public trytond-party-siret
  (package
    (name "trytond-party-siret")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_party_siret" version))
       (sha256
        (base32 "0bvn22hpj8a65kcvd1g8b51dxylpi6dm77fzivpklmpjqln5wgl3"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "party_siret"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-party-siret")
    (synopsis "Tryton module to add SIRET/SIREN on parties")
    (description "The @emph{Party SIRET} Tryton module adds the French company
identification numbers SIREN and SIRET on party and address.")
    (license license:gpl3+)))

(define-public trytond-product
  (package
    (name "trytond-product")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product" version))
       (sha256
        (base32 "0d03zmhay1kaaw8a9yalfsyzn54pb9yzkg1kxpywzd4ysxs295iz"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-stdnum trytond trytond-company))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with products")
    (description
     "This package provides a Tryton module that defines two concepts: Product
Template and Product.")
    (license license:gpl3+)))

(define-public trytond-product-attribute
  (package
    (name "trytond-product-attribute")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_attribute" version))
       (sha256
        (base32 "15sdcmjnkyznay4ib4ccwlhhl6aymdpd7w0f6d28dfv41y4viw1y"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_attribute"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-product-attribute")
    (synopsis "Tryton module with product attributes")
    (description "The @emph{Product Attribute} Tryton module defines the
models `Attribute` and `Attribute Set` for products.")
    (license license:gpl3+)))

(define-public trytond-product-classification
  (package
    (name "trytond-product-classification")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_classification" version))
       (sha256
        (base32 "0xd67s2d6n6a9kna5a1fvx3kcbb6wnkk4337pcfs4b7zkls4q4m8"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_classification"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product))
    (home-page
     "https://docs.tryton.org/projects/modules-product-classification")
    (synopsis "Tryton module to implement product classification")
    (description "The @emph{Product Classification} Tryton module defines the
tools for other modules to create classifications of products.  It adds a
reference field classification to the product template.")
    (license license:gpl3+)))

(define-public trytond-product-classification-taxonomic
  (package
    (name "trytond-product-classification-taxonomic")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_classification_taxonomic" version))
       (sha256
        (base32 "04rr76kwxv05z0m5dy3p5sk5cq4jf0wab8wzrwl8g62ajrs8m697"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_classification_taxonomic"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product-classification))
    (home-page
     "https://docs.tryton.org/projects/modules-product-classification-taxonomic")
    (synopsis "Tryton module to implement product classification taxonomic")
    (description "The @emph{Product Classification Taxonomic} Tryton module
adds the taxonomic classification to the products.")
    (license license:gpl3+)))

(define-public trytond-product-cost-fifo
  (package
    (name "trytond-product-cost-fifo")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_cost_fifo" version))
       (sha256
        (base32 "130vq1wqxxlcdxdlyf3apg0f2xpy4farbl49mplz6ihiwpvzh1ka"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_cost_fifo"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-product-cost-fifo")
    (synopsis "Tryton module to add FIFO cost method")
    (description "The @emph{Product Cost FIFO} Tryton module add a
first-in-first-out option in the `Cost Method` field of the product form.")
    (license license:gpl3+)))

(define-public trytond-product-cost-history
  (package
    (name "trytond-product-cost-history")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_cost_history" version))
       (sha256
        (base32 "19dmw7319q3nrn9hj1i7zzbi034y7sb8870376sln8yw5rfacqvz"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_cost_history"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-product-cost-history")
    (synopsis "Tryton module to historize product cost")
    (description "The @emph{Product Cost History} Tryton module adds a `Cost
History` relate on the product form, showing the cost price evolution of the
product.  The history is based on the cost price stored on the incoming stock
moves for goods and assets and based on the history table for service.  When a
historic cost price is needed, the value is taken from this history for goods
and assets.")
    (license license:gpl3+)))

(define-public trytond-product-cost-warehouse
  (package
    (name "trytond-product-cost-warehouse")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_cost_warehouse" version))
       (sha256
        (base32 "075qjhwpc1a9hg8hzrzbwhaa9bnyz8a9y55jgl88b013zc2aic5h"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_cost_warehouse"))
    (native-inputs
     (cons* trytond-account-invoice-stock
            trytond-account-stock-continental
            trytond-product-cost-fifo
            trytond-product-cost-history
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-company trytond-product trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-product-cost-warehouse")
    (synopsis "Tryton module to compute product cost per warehouse")
    (description "The @emph{Product Cost Warehouse} Trython module allows the
cost price of products to be calculated separately for each warehouse.")
    (license license:gpl3+)))

(define-public trytond-product-image
  (package
    (name "trytond-product-image")
    (version "7.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_product_image" version))
              (sha256
               (base32 "0x60yvgmg9wa8mqs3kalvhvgm8lwqdnzmwi960ipfims8mfsjfpj"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_image"
                "-k not (test_get_image_url or test_image)"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs (list python-pillow trytond trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-product-image")
    (synopsis "Tryton module that adds images to products")
    (description "The @emph{Product Image} Tryton module adds images to each
product and variant.")
    (license license:gpl3+)))

(define-public trytond-product-image-attribute
  (package
    (name "trytond-product-image-attribute")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_product_image_attribute" version))
              (sha256
               (base32 "0avbq2bvcynfb70148nmkm2pgzy6f40qm2hmh2ms5nsyn2r9bfz4"))))
    (build-system pyproject-build-system)
    ;; tests require network - unfortunately this disables the main test case
    (arguments (tryton-arguments "product_image_attribute"
                                 "-k not test_image_attribute"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs (list trytond
                             trytond-product
                             trytond-product-attribute
                             trytond-product-image))
    (home-page "https://docs.tryton.org/projects/modules-product-image-attribute")
    (synopsis "Tryton module to select variant images based on attributes")
    (description "The @emph{Product Image Attribute} Tryton module adds
attributes to product images.")
    (license license:gpl3+)))

(define-public trytond-product-kit
  (package
    (name "trytond-product-kit")
    (version "7.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_kit" version))
       (sha256
        (base32 "00479fq97ldrg3hkwzhwaaadd430rcs9rg4dd13lbijckzhapj86"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_kit"))
    (native-inputs
     (cons* trytond-account-invoice
            trytond-account-invoice-stock
            trytond-company
            trytond-purchase
            trytond-purchase-amendment
            trytond-sale
            trytond-sale-amendment
            trytond-stock
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-product-kit")
    (synopsis "Tryton module to manage product kits and components")
    (description "The @emph{Product Kit} Tryton Module adds kits and
components to products.  This enables a defined set of products to be sold or
purchased using a single line.")
    (license license:gpl3+)))

(define-public trytond-product-measurements
  (package
    (name "trytond-product-measurements")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_measurements" version))
       (sha256
        (base32 "133d2rsipfakjx35m685qclis5b1y288wpb03sqny41h1sd0h2qr"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_measurements"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-product-measurements")
    (synopsis "Tryton module to add measurements to product")
    (description "The @emph{Product Measurements} Tryton module adds this
following measurements to Product:")
    (license license:gpl3+)))

(define-public trytond-product-price-list
  (package
    (name "trytond-product-price-list")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_price_list" version))
       (sha256
        (base32 "1n1zbq7jwqkida4qx7pp4ybys7pskppxqd86kl1vdw5fj43nhm7n"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_price_list"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-simpleeval trytond trytond-company trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-product-price-list")
    (synopsis "Tryton module with price list")
    (description "The @emph{Product Price List} Tryton module provides formula
to compute prices per product or category.")
    (license license:gpl3+)))

(define-public trytond-product-price-list-cache
  (package
    (name "trytond-product-price-list-cache")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_price_list_cache" version))
       (sha256
        (base32 "1hrj7iz9yb2kmybjcgzzp9s29jc6884vih6fqw5xgnz78x9rbfdq"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_price_list_cache"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-product
           trytond-product-price-list))
    (home-page "https://docs.tryton.org/projects/modules-product-price-list-cache")
    (synopsis "Tryton module to cache price lists")
    (description "The @emph{Product Price List Cache} Tryton module
pre-computes and stores prices for each product and price list.")
    (license license:gpl3+)))

(define-public trytond-product-price-list-dates
  (package
    (name "trytond-product-price-list-dates")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_price_list_dates" version))
       (sha256
        (base32 "1lm2x8d3z19sp24k755ncfsvqf7s6vi670wbhvdrx2hkqy0hvpgn"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_price_list_dates"))
    (native-inputs
     (cons* trytond-product-price-list-cache
            trytond-purchase-price-list
            trytond-sale-price-list
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-product-price-list))
    (home-page
     "https://docs.tryton.org/projects/modules-product-price-list-dates")
    (synopsis "Tryton module to add dates on price list")
    (description "The @emph{Product Price List Dates} Tryton module adds start
date and end date conditions to the price list lines.")
    (license license:gpl3+)))

(define-public trytond-product-price-list-parent
  (package
    (name "trytond-product-price-list-parent")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product_price_list_parent" version))
       (sha256
        (base32 "12icwym955ip9wk667yyzarc2qjp8xf6dq12fy0blsqf17jfn91j"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "product_price_list_parent"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product-price-list))
    (home-page
     "https://docs.tryton.org/projects/modules-product-price-list-parent")
    (synopsis "Tryton module to use price from another price list")
    (description "The @emph{Product Price List Parent} Tryton module adds a
parent to the price list and the keyword `parent_unit_price` for the formula
which contains the unit price computed by the parent price list.")
    (license license:gpl3+)))

(define-public trytond-production
  (package
    (name "trytond-production")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_production" version))
       (sha256
        (base32 "1jlhipmcvr09xdjh8f6qzmfas5yjii31ymhb2cgkp52b1kp563xw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases #$(tryton-phases "production")
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; DB_CACHE and pytest don't work together here
                (invoke "python" "-m" "unittest" "discover")))))))
    (native-inputs
     (cons* trytond-stock-lot
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-company trytond-product trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-production")
    (synopsis "Tryton module for production")
    (description "The @emph{Production} Tryton module defines basics
for production management: Bill of material and production order.")
    (license license:gpl3+)))

(define-public trytond-production-outsourcing
  (package
    (name "trytond-production-outsourcing")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_production_outsourcing" version))
       (sha256
        (base32 "16f4d5jma1ygjn8rwi1pi7w6nyj2n2nw0qph7jjb5pzfdw88g3g6"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "production_outsourcing"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product trytond-production
           trytond-production-routing trytond-purchase))
    (home-page
     "https://docs.tryton.org/projects/modules-production-outsourcing")
    (synopsis "Tryton module to outsource production")
    (description "The @emph{Production Outsourcing} Tryton module allows
outsourcing production orders per routing.  When such outsourced production is
set to @code{waiting}, a purchase order is created and its cost is added to
the production.")
    (license license:gpl3+)))

(define-public trytond-production-routing
  (package
    (name "trytond-production-routing")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_production_routing" version))
       (sha256
        (base32 "0n9xay9s12lzkcra30npnsh0589zrfkhk7al5whla58i123a7vwi"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "production_routing"))
    (native-inputs
     (cons* trytond-stock-supply-production
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-production))
    (home-page "https://docs.tryton.org/projects/modules-production-routing")
    (synopsis "Tryton module for production routing")
    (description "The @emph{Production Routing} Tryton module defines the
routings for production: Routing, Step and Operation.")
    (license license:gpl3+)))

(define-public trytond-production-split
  (package
    (name "trytond-production-split")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_production_split" version))
       (sha256
        (base32 "06ic3w4zskk08q617660w1gx5l8dmf782n9kq0kg8x82m2lw0f0n"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "production_split"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-production))
    (home-page "https://docs.tryton.org/projects/modules-production-split")
    (synopsis "Tryton module to split production")
    (description "The @emph{Production Split} Tryton module adds on the
production a wizard that allows splitting it.  The production is split into
productions of Quantity.  If a count is set, it will be split only this number
of times.  On occasion there can be a production with the remaining
quantity.")
    (license license:gpl3+)))

(define-public trytond-production-work
  (package
    (name "trytond-production-work")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_production_work" version))
       (sha256
        (base32 "031673vvqmdhrfkib5nikza4a5w5hgq4x14mdzgwny3ln7vdmis9"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "production_work"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-product
           trytond-production
           trytond-production-routing
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-production-work")
    (synopsis "Tryton module for production work")
    (description "The @emph{Production Work} Tryton module allows managing a
work order for each production.  It also adds in the production cost for the
work cost.")
    (license license:gpl3+)))

(define-public trytond-production-work-timesheet
  (package
    (name "trytond-production-work-timesheet")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_production_work_timesheet" version))
       (sha256
        (base32 "0c737kxdqpjc1h9vb00sz69zh6y76d3ll71nrs2nxidqjdwyhhz5"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "production_work_timesheet"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-production-routing trytond-production-work
           trytond-timesheet))
    (home-page
     "https://docs.tryton.org/projects/modules-production-work-timesheet")
    (synopsis "Tryton module for timesheet on production work")
    (description "The @emph{Production Work Timesheet} Tryton module allows
entering a timesheet for production works.")
    (license license:gpl3+)))

(define-public trytond-project
  (package
    (name "trytond-project")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_project" version))
       (sha256
        (base32 "0y8ymmfp91z89ylwwfwkl66l227phz3c0dj1r3k8ahlv7q4rrlsg"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "project"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-company trytond-company-work-time
           trytond-party trytond-timesheet))
    (home-page "https://docs.tryton.org/projects/modules-project")
    (synopsis "Tryton module with projects")
    (description "The @emph{Project} Tryton module provides the concepts of
project and task and the basis for simple project management.")
    (license license:gpl3+)))

(define-public trytond-project-invoice
  (package
    (name "trytond-project-invoice")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_project_invoice" version))
       (sha256
        (base32 "031bjkh6dyixs4rkmdpaf28xa8cx5yr3hh51gkcd4mcnz2pbflxx"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "project_invoice"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-account-product
           trytond-currency
           trytond-product
           trytond-project
           trytond-project-revenue
           trytond-timesheet))
    (home-page "https://docs.tryton.org/projects/modules-project-invoice")
    (synopsis "Tryton module to invoice projects")
    (description "The @emph{Project Invoice} Tryton module adds invoice
methods on projects.  The methods are:
@itemize
@item Manual: Tryton doesn’t create any invoice.
@item On Effort: The invoices are created based on the Effort hours
      for all children works with 100% progress.
@item On Progress: The invoices are create proportionally to the Progress
      of the Effort hours of each children work.
@item On Timesheet: The invoices are created based on the timesheets
      encoded on all children works.
@end itemize")
    (license license:gpl3+)))

(define-public trytond-project-plan
  (package
    (name "trytond-project-plan")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_project_plan" version))
       (sha256
        (base32 "06411x4iswy09pjxxiwxrha5r272df1rkyd5w42b192vxbjfshy7"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "project_plan"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-company trytond-project trytond-timesheet))
    (home-page "https://docs.tryton.org/projects/modules-project-plan")
    (synopsis "Tryton module to add planning capabilities on projects")
    (description "The @emph{Project Plan} Tryton module adds planning features
on top of the Project module.")
    (license license:gpl3+)))

(define-public trytond-project-revenue
  (package
    (name "trytond-project-revenue")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_project_revenue" version))
       (sha256
        (base32 "1j8qdliylg1jjas51z34gvi78q9qv81ssk4blp9y4kr1svq8wpjk"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "project_revenue"))
    (native-inputs
     (cons* trytond-purchase
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-product
           trytond-project
           trytond-timesheet
           trytond-timesheet-cost))
    (home-page "https://docs.tryton.org/projects/modules-project-revenue")
    (synopsis "Tryton module to add revenue on project")
    (description "The @emph{Project Revenue} Tryton module computes revenue
and cost per task and project.  The revenue uses the list price of the
product.  If the product's unit of measure is time based, the revenue is
computed as the product of the price and the hours of effort otherwise the
price is considered as fixed.  The cost is computed by summing the cost of all
the linked time sheets and the linked purchase lines.")
    (license license:gpl3+)))

(define-public trytond-purchase
  (package
    (name "trytond-purchase")
    (version "7.0.15")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase" version))
       (sha256
        (base32 "0mc5m7navyw9h7xc8kypdmcr05ggh3f33fdxnw0r08mmw01sy1cg"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-account-invoice-stock
           trytond-account-product
           trytond-company
           trytond-currency
           trytond-party
           trytond-product
           trytond-stock))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for purchase")
    (description
     "This package provides a Tryton module that defines the Purchase model.")
    (license license:gpl3+)))

(define-public trytond-purchase-amendment
  (package
    (name "trytond-purchase-amendment")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_amendment" version))
       (sha256
        (base32 "0s8kp88s73jn9z5bnj5n91fl67hpycrzcxl6hkdx4l2vha6r2f65"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_amendment"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-purchase
           trytond-purchase-history trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-purchase-amendment")
    (synopsis "Tryton module to amend purchases")
    (description "The @emph{Purchase Amendment} Tryton module allows you to
change purchases that are being processed and keep track of the changes.  An
amendment is composed of action lines which can:

@itemize
@item recompute taxes (if the supplier tax rules or product taxes have
      changed),
@item change the payment term,
@item change the party and the address,
@item change the warehouse, or
@item change a purchase line: (product, quantity and unit of measure,
      unit price or description).
@end itemize")
    (license license:gpl3+)))

(define-public trytond-purchase-blanket-agreement
  (package
    (name "trytond-purchase-blanket-agreement")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_blanket_agreement" version))
       (sha256
        (base32 "1h96y36ik14snxw6hm6w6nsxkkn5lv4vmhmy1xfadiffafhr7503"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_blanket_agreement"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-currency
           trytond-party
           trytond-product
           trytond-purchase))
    (home-page "https://docs.tryton.org/projects/modules-purchase-blanket-agreement")
    (synopsis "Tryton module for purchase blanket agreements")
    (description "The @emph{Purchase Blanket Agreement} Tryton module manages
long-term contracts with suppliers to purchase a specific quantity of products
with multiple orders over a period.")
    (license license:gpl3+)))

(define-public trytond-purchase-history
  (package
    (name "trytond-purchase-history")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_history" version))
       (sha256
        (base32 "1vf9r2rsbxxgy9brl3458n5axdk4sc4r01xrq7h1293izs1cjw5s"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_history"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-purchase))
    (home-page "https://docs.tryton.org/projects/modules-purchase-history")
    (synopsis "Tryton module to historize purchases")
    (description "The @emph{Purchase History} Tryton module activates the
historization of the purchase and adds a revision counter which increases each
time the purchase is reset to draft.")
    (license license:gpl3+)))

(define-public trytond-purchase-invoice-line-standalone
  (package
    (name "trytond-purchase-invoice-line-standalone")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_invoice_line_standalone" version))
       (sha256
        (base32 "1djvnlqlhc3q77r07il8hx5j13qzjdnwfxfnrj3sf8nkpdbi12ac"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_invoice_line_standalone"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice-line-standalone
           trytond-party trytond-purchase))
    (home-page
     "https://docs.tryton.org/projects/modules-purchase-invoice-line-standalone")
    (synopsis "Tryton module for standalone invoice line from purchase")
    (description "The @emph{Purchase Invoice Line Standalone} Tryton module
makes purchase to generate invoice lines instead of invoices.")
    (license license:gpl3+)))

(define-public trytond-purchase-price-list
  (package
    (name "trytond-purchase-price-list")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_price_list" version))
       (sha256
        (base32 "1zpyd9vryh9lwl3n8pnfa222815n7lcmwhs82p734v5l7c9cwfwg"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_price_list"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-company
           trytond-party
           trytond-product
           trytond-product-price-list
           trytond-purchase))
    (home-page "https://docs.tryton.org/projects/modules-purchase-price-list")
    (synopsis "Tryton module to add price list on purchase")
    (description "The @emph{Purchase Price List} Tryton Module allows price
lists to be defined for suppliers.")
    (license license:gpl3+)))

(define-public trytond-purchase-product-quantity
  (package
    (name "trytond-purchase-product-quantity")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_product_quantity" version))
       (sha256
        (base32 "0q1paxi1ppdc1qim389nixl483hd8whrgv6ck554f4008v4dpj8r"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_product_quantity"))
    (native-inputs
     (cons* trytond-stock-supply
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-product
           trytond-purchase
           trytond-purchase-request))
    (home-page "https://docs.tryton.org/projects/modules-purchase-product-quantity")
    (synopsis "Tryton module to add quantity constraints on purchase lines")
    (description "The @emph{Purchase Product Quantity} Tryton module permits
to enforce the minimal and the rounding of quantity purchased per supplier
from purchase request.")
    (license license:gpl3+)))

(define-public trytond-purchase-request
  (package
    (name "trytond-purchase-request")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_request" version))
       (sha256
        (base32 "0s6i8s0s3k8wj2xkyncfnn9zd4l7d73wyz7x6glqyjx7x69qc74p"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_request"))
    (native-inputs
     (cons* trytond-stock-supply-bootstrap
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-product trytond-purchase))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for purchase requests")
    (description
     "This package provides a Tryton module that introduces the concept of
Purchase Requests which are central points to collect purchase requests
generated by other process from Tryton.")
    (license license:gpl3+)))

(define-public trytond-purchase-request-quotation
  (package
    (name "trytond-purchase-request-quotation")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_request_quotation" version))
       (sha256
        (base32 "176p3yslqhn75a5nkkgzqsbrbb247b52za7ydrhd20dylid24g6s"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_request_quotation"))
    (native-inputs
     (cons* trytond-purchase-requisition
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-currency
           trytond-party
           trytond-product
           trytond-purchase-request))
    (home-page
     "https://docs.tryton.org/projects/modules-purchase-request-quotation")
    (synopsis "Tryton module for purchase request quotation")
    (description "The @emph{Purchase Request Quotation} Tryton module allows
users to ask quotations from selected purchase requests to different
suppliers.  Each request will collect quotation information from the
supplier.")
    (license license:gpl3+)))

(define-public trytond-purchase-requisition
  (package
    (name "trytond-purchase-requisition")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_requisition" version))
       (sha256
        (base32 "000n2fhx38yxxg6czsd2ir1qlb4fv0zx720w4p6lfi8g3xgri01c"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_requisition"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-currency
           trytond-party
           trytond-product
           trytond-purchase
           trytond-purchase-request))
    (home-page "https://docs.tryton.org/projects/modules-purchase-requisition")
    (synopsis "Tryton module to enter requests for product
supply (requisition)")
    (description "The @emph{Purchase Requisition} Tryton module allows users
to create their requests for product supply (purchase requisitions).  Those
requisitions will be approved or rejected by the approval group, whoich
typically is the purchasing department.  On approval, purchase requests will
be created.")
    (license license:gpl3+)))

(define-public trytond-purchase-secondary-unit
  (package
    (name "trytond-purchase-secondary-unit")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_secondary_unit" version))
       (sha256
        (base32 "1sg7jc9aw1wa7xbhn2l4g6b8q161zf4118jfy1i6968dx6z2ask7"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_secondary_unit"))
    (native-inputs
     (cons* trytond-account-invoice-secondary-unit
            trytond-purchase-amendment
            trytond-purchase-blanket-agreement
            trytond-purchase-request
            trytond-purchase-requisition
            trytond-stock-secondary-unit
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-product
           trytond-purchase trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-purchase-secondary-unit")
    (synopsis "Tryton module to add a secondary unit on purchase line")
    (description "The @emph{Purchase Secondary Unit} Tryton module adds a
secondary unit of measure on purchase lines.

The secondary quantity and unit price are kept synchronized with the quantity
and unit price.  The secondary unit is defined on the product supplier or on
the product with its factor against the purchase unit.")
    (license license:gpl3+)))

(define-public trytond-purchase-shipment-cost
  (package
    (name "trytond-purchase-shipment-cost")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_shipment_cost" version))
       (sha256
        (base32 "1mvn0cwr5c9ndrghir7yd9djvdlk4sshnlq0qxw9wp613qlcwp6x"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "purchase_shipment_cost"))
    (native-inputs
     (cons* trytond-account-invoice-stock
            trytond-account-stock-anglo-saxon
            trytond-account-stock-continental
            trytond-purchase
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-carrier trytond-currency trytond-product
           trytond-stock trytond-stock-shipment-cost))
    (home-page
     "https://docs.tryton.org/projects/modules-purchase-shipment-cost")
    (synopsis "Tryton module for purchase shipment costs")
    (description "The @emph{Purchase Shipment Cost} Tryton module adds
shipment costs to Supplier Shipment.")
    (license license:gpl3+)))

(define-public trytond-quality
  (package
    (name "trytond-quality")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_quality" version))
       (sha256
        (base32 "04w50icp1bqn6ybmvdl0hxnmizsjjj1jnhkid51w8s0phlwdlvrc"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "quality"))
    (native-inputs
     (cons* trytond-production
            trytond-stock
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-product))
    (home-page "https://docs.tryton.org/projects/modules-quality")
    (synopsis "Tryton module for quality management")
    (description "The @emph{Quality} Tryton module enables quality to be
controlled by configuring control points and inspecting against these when
certain operations are performed.")
    (license license:gpl3+)))

(define-public trytond-sale
  (package
    (name "trytond-sale")
    (version "7.0.15")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale" version))
       (sha256
        (base32 "171ihl762iizlyvhn4zvg52waffsaxr9yh7d7gy4vsdciym6vm8k"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-account-invoice-stock
           trytond-account-product
           trytond-company
           trytond-country
           trytond-currency
           trytond-party
           trytond-product
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-sale")
    (synopsis "Tryton module for sale")
    (description "The @emph{Sale} Tryton module helps organise and manage
sales made by the company.  It adds the concept of a sale to Tryton and allows
it to be tracked through its states from draft to done.  It also oversees the
creation of customer shipments and invoices for the sales, and allows reports
to be generated that contain aggregated sales figures.")
    (license license:gpl3+)))

(define-public trytond-sale-advance-payment
  (package
    (name "trytond-sale-advance-payment")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_advance_payment" version))
       (sha256
        (base32 "08qcfa5sif1a3l02f5vr5668bzca8mndg0h917r2x43z6gd1g8z9"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_advance_payment"))
    (native-inputs
     (cons* trytond-sale-supply
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-simpleeval
           trytond
           trytond-account
           trytond-account-invoice
           trytond-company
           trytond-sale))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-advance-payment")
    (synopsis "Tryton module for sale advance payment")
    (description "The @emph{Sale Advance Payment} Tryton module adds support
for advance payment management on the sale.")
    (license license:gpl3+)))

(define-public trytond-sale-amendment
  (package
    (name "trytond-sale-amendment")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_amendment" version))
       (sha256
        (base32 "0p4v44hcby6s7l07hzdxj5z8lzb4xn4r4z5j1rlnkfvgccgmfvpc"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_amendment"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-sale
           trytond-sale-history trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-sale-amendment")
    (synopsis "Tryton module to amend sales")
    (description "The @emph{Sale Amendment} Tryton module allows you to change
sales that are being processed and keep track of the changes.  An amendment is
composed of action lines which can:")
    (license license:gpl3+)))

(define-public trytond-sale-blanket-agreement
  (package
    (name "trytond-sale-blanket-agreement")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_blanket_agreement" version))
       (sha256
        (base32 "0ai58qmyqrn89ppbrnsqclss5kf4hr5y2gpa9s0xs1cicn367w8f"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_blanket_agreement"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-currency
           trytond-party
           trytond-product
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-blanket-agreement")
    (synopsis "Tryton module for sale blanket agreements")
    (description "The @emph{Sale Blanket Agreement} Tryton module manages
long-term contracts with customers to sell a specific quantity of products
with multiple orders over a period.")
    (license license:gpl3+)))

(define-public trytond-sale-complaint
  (package
    (name "trytond-sale-complaint")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_complaint" version))
       (sha256
        (base32 "18840gqa97bmw6yksy18a65qn3lidcy2vn5h35mp1pzq6rb5rc6i"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_complaint"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-company trytond-party
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-complaint")
    (synopsis "Tryton module for sale complaints")
    (description "The @emph{Sale Complaint} Tryton module defines the
@code{Complaint} model.")
    (license license:gpl3+)))

(define-public trytond-sale-credit-limit
  (package
    (name "trytond-sale-credit-limit")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_credit_limit" version))
       (sha256
        (base32 "15lkdvhxhr3wk9s20g2ypaqmal4kc0cr40gj51jamfqj8s1yj79p"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_credit_limit"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account-credit-limit
           trytond-account-invoice
           trytond-company
           trytond-currency
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-credit-limit")
    (synopsis "Tryton module for sale credit limit")
    (description "The @emph{Sale Credit Limit} Tryton module adds confirmed
sale but not yet invoiced to the credit amount of the party and check the
credit limit of the party when confirming a sale.")
    (license license:gpl3+)))

(define-public trytond-sale-discount
  (package
    (name "trytond-sale-discount")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_discount" version))
       (sha256
        (base32 "1s2msvpgbgi7l0d1iq30y0l02bq2in2a9yr1526za952vd3clf93"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_discount"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-discount")
    (synopsis "Tryton module that manages discount on sale")
    (description "The @emph{Sale Discount} Tryton module adds discount on sale
line.")
    (license license:gpl3+)))

(define-public trytond-sale-extra
  (package
    (name "trytond-sale-extra")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_extra" version))
       (sha256
        (base32 "1vvw7h2q0c6ifbwl3z60sna022npsqby29hrxcpgd5izcphk8jja"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_extra"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-product
           trytond-product-price-list
           trytond-sale
           trytond-sale-price-list))
    (home-page "https://docs.tryton.org/projects/modules-sale-extra")
    (synopsis "Tryton module for sale extra")
    (description "The @emph{Sale Extra} Tryton module allows adding an extra line
on sale based on criteria.")
    (license license:gpl3+)))

(define-public trytond-sale-gift-card
  (package
    (name "trytond-sale-gift-card")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_gift_card" version))
       (sha256
        (base32 "1brfyrdd3j1lk8accdjv1jx6ila0rbskjjhlg0vv2jflwq81hpvn"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_gift_card"))
    (native-inputs
     (cons* trytond-sale-point
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-company
           trytond-currency
           trytond-product
           trytond-sale
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-sale-gift-card")
    (synopsis "Tryton module to manage gift cards")
    (description "The @emph{Sale Gift Card} Tryton module manages the selling
and redeeming of gift cards.")
    (license license:gpl3+)))

(define-public trytond-sale-history
  (package
    (name "trytond-sale-history")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_history" version))
       (sha256
        (base32 "1adi6hvlpmg036h29zjzj6aismcvh9dk94acmk12bg7qv4zw9imx"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_history"))
    (native-inputs
     (cons* trytond-sale-subscription
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-history")
    (synopsis "Tryton module to historize sales")
    (description "The @emph{Sale History} Tryton module activates the
historization of the sale and adds a revision counter which increases each
time the sale is reset to draft.")
    (license license:gpl3+)))

(define-public trytond-sale-invoice-date
  (package
    (name "trytond-sale-invoice-date")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_invoice_date" version))
       (sha256
        (base32 "0ssc87by1hinxckcma99hlngy1r9is84svccy1zrvqdif06pjfqp"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_invoice_date"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-party
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-invoice-date")
    (synopsis "Tryton module to compute the invoice date of sale")
    (description "The @emph{Sale Invoice Date} Tryton module fills the invoice
date of invoices created by sales.")
    (license license:gpl3+)))

(define-public trytond-sale-invoice-grouping
  (package
    (name "trytond-sale-invoice-grouping")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_invoice_grouping" version))
       (sha256
        (base32 "06awrzvq8c4v7133by95njiq1n3j85x362r4fhb6z7fyrn03kzx6"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_invoice_grouping"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-party trytond-sale))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-invoice-grouping")
    (synopsis "Tryton module to group sale invoices")
    (description "The @emph{Sale Invoice Grouping} Tryton module adds an
option to define how invoice lines generated from sales will be grouped.")
    (license license:gpl3+)))

(define-public trytond-sale-opportunity
  (package
    (name "trytond-sale-opportunity")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_opportunity" version))
       (sha256
        (base32 "1r2xa0y60yc4f13w0pm7w48jnwh60hrn2w4jld251wjf56cb1yjr"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_opportunity"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account-invoice
           trytond-company
           trytond-currency
           trytond-party
           trytond-product
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-opportunity")
    (synopsis "Tryton module with leads and opportunities")
    (description "The @emph{Sale Opportunity} Tryton module defines the
lead/opportunity model.")
    (license license:gpl3+)))

(define-public trytond-sale-payment
  (package
    (name "trytond-sale-payment")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_payment" version))
       (sha256
        (base32 "03rd22bim95z9frpmvb5vk2h2p4pybbds3jy3y8rkz3vhizv61j4"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_payment"))
    (native-inputs
     (cons* trytond-account-payment-clearing
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account-invoice
           trytond-account-payment
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-payment")
    (synopsis "Tryton module that manage payments on sale")
    (description "The @emph{Sale Payment} Tryton module extends Sale to allow
payments prior to the creation of any invoice.")
    (license license:gpl3+)))

(define-public trytond-sale-point
  (package
    (name "trytond-sale-point")
    (version "7.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_sale_point" version))
              (sha256
               (base32 "0m77c14id626b3f2gqi16rxl4g0wfywhp6iyiwm9cxsdd5qj8wb3"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_point"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-product
           trytond-company
           trytond-party
           trytond-product
           trytond-sale
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-sale-point")
    (synopsis "Tryton module for Point of Sales")
    (description "The @emph{Sale Point} Tryton module allows retail sales to
be handled and recorded.")
    (license license:gpl3+)))

(define-public trytond-sale-price-list
  (package
    (name "trytond-sale-price-list")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_price_list" version))
       (sha256
        (base32 "0r0rklqqa8aw97yk910l4vzx953a3q4hlbn53l22pq0f30j2gqlf"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_price_list"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-company trytond-party
           trytond-product-price-list trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-price-list")
    (synopsis "Tryton module to add price list on sale")
    (description "The @emph{Sale Price List} Tryton module adds support for
price list on sale.  A price list can be set per party or as default.")
    (license license:gpl3+)))

(define-public trytond-sale-product-customer
  (package
    (name "trytond-sale-product-customer")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_product_customer" version))
       (sha256
        (base32 "0zgzkif68sf9klpdp88rmns9ga5c3ir0jkg88yqa5gv3rma3sh3a"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_product_customer"))
    (native-inputs
     (cons* trytond-sale-amendment
            trytond-sale-blanket-agreement
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-product trytond-sale))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-product-customer")
    (synopsis "Tryton module to manage customer product on sale")
    (description "The @emph{Sale Product_Customer} Tryton module defines
customer's names and codes for products or variants.")
    (license license:gpl3+)))

(define-public trytond-sale-product-quantity
  (package
    (name "trytond-sale-product-quantity")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_product_quantity" version))
       (sha256
        (base32 "1k1mi0iiw1xqm3sjywzfq61whc9kwsv09248m31y9k1r0qx0zxh7"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_product_quantity"))
    (native-inputs
     (cons* trytond-sale-point
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-product
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-product-quantity")
    (synopsis "Tryton module to add quantity constraints on sale lines")
    (description "The @emph{Sale Product Quantity} Tryton module permits
enforcing the minimal and the rounding of quantity sold per product.")
    (license license:gpl3+)))

(define-public trytond-sale-product-recommendation
  (package
    (name "trytond-sale-product-recommendation")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_product_recommendation" version))
       (sha256
        (base32 "11s8i76p6743xx1m0h0hslvzw4cj190m3hdpsv6id5prgragx9sk"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_product_recommendation"))
    (native-inputs
     (cons* trytond-sale-point
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-product
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-product-recommendation")
    (synopsis "Tryton module for product recommendations")
    (description "The @emph{Sale Product Recommendation} Tryton module
provides facilities to implement recommendation of products on sale.")
    (license license:gpl3+)))

(define-public trytond-sale-product-recommendation-association-rule
  (package
    (name "trytond-sale-product-recommendation-association-rule")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_product_recommendation_association_rule" version))
       (sha256
        (base32 "0d59811an015s8lfv4i5dflkg8fbvn937y81shn6wxndrrm32z4a"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_product_recommendation_association_rule"))
    (native-inputs
     (cons* trytond-sale-point
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-efficient-apriori
           trytond trytond-product
           trytond-sale
           trytond-sale-product-recommendation))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-product-recommendation-association-rule")
    (synopsis "Tryton module to learn association rule for recommendations")
    (description "The @emph{Sale Product Recommendation Association Rule}
Tryton module implements recommendation based on association rule learning
from previous sales.")
    (license license:gpl3+)))

(define-public trytond-sale-promotion
  (package
    (name "trytond-sale-promotion")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_promotion" version))
       (sha256
        (base32 "1vc7ij8pgnfgp5w557kf1lnbir5xcq1i6zqhvv9pa0rpnrsmnh5z"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_promotion"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-simpleeval
           trytond
           trytond-company
           trytond-currency
           trytond-product
           trytond-product-price-list
           trytond-sale
           trytond-sale-price-list))
    (home-page "https://docs.tryton.org/projects/modules-sale-promotion")
    (synopsis "Tryton module for sale promotion")
    (description "The @emph{Sale Promotion} module allows applying promotions
on a sale based on criteria.")
    (license license:gpl3+)))

(define-public trytond-sale-promotion-coupon
  (package
    (name "trytond-sale-promotion-coupon")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_promotion_coupon" version))
       (sha256
        (base32 "1xywa7b3mfq6x9xzmxdxr3j5i5vy91wjxmf1f2s3q8hya92djjnr"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_promotion_coupon"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-sale
           trytond-sale-promotion))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-promotion-coupon")
    (synopsis "Tryton module for sale promotion coupon")
    (description "The @emph{Sale Promotion Coupon} Tryton module adds coupon
to the promotions.")
    (license license:gpl3+)))

(define-public trytond-sale-promotion-coupon-payment
  (package
    (name "trytond-sale-promotion-coupon-payment")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_promotion_coupon_payment" version))
       (sha256
        (base32 "051jgg0pid8rsk8987q0vfx1xiixvh65rzl9bcgdzj7kl8fxlssp"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_promotion_coupon_payment"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account-payment
           trytond-sale-promotion-coupon))
    (home-page "https://docs.tryton.org/projects/modules-sale-promotion-coupon-payment")
    (synopsis "Tryton module to link payments with coupons")
    (description "The @emph{Sale Promotion Coupon Payment} Tryton module
includes the identical parties from the payments to count usage per party.")
    (license license:gpl3+)))

(define-public trytond-sale-secondary-unit
  (package
    (name "trytond-sale-secondary-unit")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_secondary_unit" version))
       (sha256
        (base32 "1pd3a4ykjyiipacy0pksv30mb6kf7n203mp6qh8jn4c2wwvjn06g"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_secondary_unit"))
    (native-inputs
     (cons* trytond-account-invoice-secondary-unit
            trytond-sale-amendment
            trytond-sale-blanket-agreement
            trytond-sale-product-customer
            trytond-stock-secondary-unit
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-account-invoice trytond-product trytond-sale
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-sale-secondary-unit")
    (synopsis "Tryton module to add a secondary unit on sale line")
    (description "The @emph{Sale Secondary Unit} Tryton module adds a
secondary unit of measure on sale lines.  The secondary quantity and unit
price are kept synchronized with the quantity and unit price.  The secondary
unit is defined on the product with its factor against the sale unit.")
    (license license:gpl3+)))

(define-public trytond-sale-shipment-cost
  (package
    (name "trytond-sale-shipment-cost")
    (version "7.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_shipment_cost" version))
       (sha256
        (base32 "011hjaqwd3m4ncz15rs3czp8rcwcr4ak4rjzb15zznb0ghkz1kgx"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_shipment_cost"))
    (native-inputs
     (cons* trytond-account
            trytond-party
            trytond-sale-promotion
            trytond-sale-shipment-grouping
            trytond-stock
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account-invoice
           trytond-carrier
           trytond-currency
           trytond-product
           trytond-sale
           trytond-stock
           trytond-stock-shipment-cost))
    (home-page "https://docs.tryton.org/projects/modules-sale-shipment-cost")
    (synopsis "Tryton module for sale shipment cost")
    (description "The @emph{Sale Shipment Cost} Tryton module adds shipment
cost for sale.")
    (license license:gpl3+)))

(define-public trytond-sale-shipment-grouping
  (package
    (name "trytond-sale-shipment-grouping")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_shipment_grouping" version))
       (sha256
        (base32 "1hb4h00xsx9r6cri6c5ys7gx0181h8lv2q366djsxfsci76zim8g"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_shipment_grouping"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-party trytond-sale trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-shipment-grouping")
    (synopsis "Tryton module to group sale stock moves")
    (description "The @emph{Sale Shipment Grouping} module adds an option to
define how stock moves generated from sales will be grouped.")
    (license license:gpl3+)))

(define-public trytond-sale-shipment-tolerance
  (package
    (name "trytond-sale-shipment-tolerance")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_shipment_tolerance" version))
       (sha256
        (base32 "0cqia2qlf6f6yjn673amicjraggb59z0g00cbrdfpyv4g2zgv6qr"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_shipment_tolerance"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-sale trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-shipment-tolerance")
    (synopsis "Tryton module to define tolerance for sale shipment")
    (description "The @emph{Sale Shipment Tolerance} module adds under and
over shipment tolerance on the sale.  If the quantity of a sale line is under
shipped but inside the tolerance percentage, then the line will be considered
as fully shipped and no back-order will be created.  If the quantity of a sale
line is over shipped more than the tolerance percentage, then a warning is
raised.")
    (license license:gpl3+)))

(define-public trytond-sale-stock-quantity
  (package
    (name "trytond-sale-stock-quantity")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_stock_quantity" version))
       (sha256
        (base32 "0q5kffgg8cpgwyzn283aghrqbqsnjayvq70q9va2khqnjbl1h9rh"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_stock_quantity"))
    (native-inputs
     (cons* trytond-stock-supply
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-product trytond-sale trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-sale-stock-quantity")
    (synopsis "Tryton module to add stock warning on sale")
    (description "The @emph{Sale Stock Quantity} Tryton module checks the
stock quantity of the products when quoting a sale.  The check will warn the
user if the forecast quantity at the sale date (and further dates until next
supply) is lower than the quantity sold by considering other sales and the
stock forecasts.")
    (license license:gpl3+)))

(define-public trytond-sale-subscription
  (package
    (name "trytond-sale-subscription")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_subscription" version))
       (sha256
        (base32 "1jdpg6g8w0q5slsfdzz6bkacv78x4gw5nr2afi7m9givqr8vivrm"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_subscription"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-account-invoice
           trytond-company
           trytond-currency
           trytond-product
           trytond-sale))
    (home-page "https://docs.tryton.org/projects/modules-sale-subscription")
    (synopsis "Tryton module for subscription")
    (description "The @emph{Sale Subscription} module defines subscription,
services and recurrence rule models.")
    (license license:gpl3+)))

(define-public trytond-sale-subscription-asset
  (package
    (name "trytond-sale-subscription-asset")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_subscription_asset" version))
       (sha256
        (base32 "1xg9wypsmv701w7mv49mw8glgkhfbvijsx97n0fr23h7w9pm7lnr"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_subscription_asset"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-sale-subscription trytond-stock-lot))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-subscription-asset")
    (synopsis "Tryton module to handle asset in the sale subscriptions")
    (description "The @emph{Sale Subscription Asset} Tryton module adds the
notion of asset to the sale subscription module.")
    (license license:gpl3+)))

(define-public trytond-sale-supply
  (package
    (name "trytond-sale-supply")
    (version "7.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_supply" version))
       (sha256
        (base32 "0rgyrimkfali4ak7b0052rp3m8m07qk1fhd10vi98zynz5bqs73g"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_supply"))
    (native-inputs
     (cons* trytond-stock-supply
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-purchase trytond-purchase-request trytond-sale
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-sale-supply")
    (synopsis "Tryton module for sale supply")
    (description "The @emph{Sale Supply} Tryton module adds a \"supply on sale
option\" to purchasable products.  If checked, it will generate a purchase
request for each sale line of this product regardless of the stock levels.
Once the purchased products are received they are assigned on the customer
shipments.  If the purchase is cancelled the sale goes back to the default
supply method.")
    (license license:gpl3+)))

(define-public trytond-sale-supply-drop-shipment
  (package
    (name "trytond-sale-supply-drop-shipment")
    (version "7.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_supply_drop_shipment" version))
       (sha256
        (base32 "04ydxw9jqbq07z5hl4aw31riyp9pv097v50zg9i4d7j2aykkldir"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_supply_drop_shipment"))
    (native-inputs
     (cons* trytond-sale-amendment
            trytond-stock-split
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-product
           trytond-purchase
           trytond-purchase-request
           trytond-sale
           trytond-sale-supply
           trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-supply-drop-shipment")
    (synopsis "Tryton module for sale supply drop shipment")
    (description "The @emph{Sale Supply Drop Shipment} Tryton module adds a
drop shipment option on product supplier if \"supply on request\" is checked.
When checked, the purchase request and the linked purchase have the address of
customer as Delivery Address; at the confirmation of the purchase a drop
shipment is created and linked to both the purchase and the sale.")
    (license license:gpl3+)))

(define-public trytond-sale-supply-production
  (package
    (name "trytond-sale-supply-production")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_sale_supply_production" version))
       (sha256
        (base32 "062nmykl8q9cypza05rr9japx2z0h63310m78qkbwnqvmlfih8jn"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "sale_supply_production"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-production trytond-sale-supply))
    (home-page
     "https://docs.tryton.org/projects/modules-sale-supply-production")
    (synopsis "Tryton module to supply sales from production")
    (description "The @emph{Sale Supply Production} Tryton module adds a
\"supply on sale\" option to producible products.  If checked, it will
generate a production request for each sale line of this product regardless of
the stock levels.  Once the products are produced they are assigned to the
customer shipments.  If the production request is cancelled, the sale goes
back to the default supply method.")
    (license license:gpl3+)))

(define-public trytond-stock
  (package
    (name "trytond-stock")
    (version "7.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock" version))
       (sha256
        (base32 "0kbp2s6dn8dp8h8pjasqylr8adicz52z02cqc6bpxr529aj7sa55"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list python-simpleeval
           trytond
           trytond-company
           trytond-currency
           trytond-party
           trytond-product))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for stock and inventory")
    (description
     "This package provides a Tryton module that defines the fundamentals for
all stock management situations: Locations where products are stored, moves
between these locations, shipments for product arrivals and departures and
inventory to control and update stock levels.")
    (license license:gpl3+)))

(define-public trytond-stock-assign-manual
  (package
    (name "trytond-stock-assign-manual")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_assign_manual" version))
       (sha256
        (base32 "0rh6fap18m0pglc0rpvwy0px8gxkj5cy93pln9b2d98saxndiirc"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_assign_manual"))
    (native-inputs
     (cons* trytond-production
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-stock-assign-manual")
    (synopsis "Tryton module to assign manually stock move")
    (description "The @emph{Stock Assign Manual} Tryton module adds a wizard
on shipments and production that allows you to decide from which precise
location to pick products.")
    (license license:gpl3+)))

(define-public trytond-stock-consignment
  (package
    (name "trytond-stock-consignment")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_consignment" version))
       (sha256
        (base32 "0q8kszbcndm58x6yngyi1phqqzmsd9rawv6rdc00q8x4xyjsc9wr"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_consignment"))
    (native-inputs
     (cons* trytond-stock-supply
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account-invoice
           trytond-account-invoice-line-standalone
           trytond-account-invoice-stock
           trytond-product
           trytond-purchase
           trytond-sale
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-stock-consignment")
    (synopsis "Tryton module to manage consignment stock")
    (description "The @emph{Stock Consignment} Tryton module allows managing
consignment stock from supplier or at customer warehouse.")
    (license license:gpl3+)))

(define-public trytond-stock-forecast
  (package
    (name "trytond-stock-forecast")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_forecast" version))
       (sha256
        (base32 "0avj0j3aphfqr97j7yy5kx2xz1znlf2bsls7c6dxkxwmr9k8p6w0"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_forecast"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-product
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-stock-forecast")
    (synopsis "Tryton module with stock forecasts")
    (description "The @emph{Stock Forecast} Tryton module provide a simple way
to create stock moves toward customers with a date in the future.  This allows
other stock mechanisms to anticipate customer demand.")
    (license license:gpl3+)))

(define-public trytond-stock-inventory-location
  (package
    (name "trytond-stock-inventory-location")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_inventory_location" version))
       (sha256
        (base32 "1z79j28liyrf1wk34vpnqrvv0d9pil74scmghyn6s3d5a0hvaia6"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_inventory_location"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-company trytond-product trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-inventory-location")
    (synopsis "Tryton module to create inventories by locations")
    (description "The @emph{Stock Inventory Location} Tryton module adds a new
wizard \"Create Inventories\" under the \"Inventories\" sub-menu.")
    (license license:gpl3+)))

(define-public trytond-stock-location-move
  (package
    (name "trytond-stock-location-move")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_location_move" version))
       (sha256
        (base32 "0qm25pa1w7cark6bphxfqvb0rw0zrr3izn7rllzsy4drvnh7ad1k"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_location_move"))
    (native-inputs
     (cons* trytond-stock-supply
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-stock-location-move")
    (synopsis "Tryton module to move storage locations")
    (description "The @emph{Stock Location} move Tryton module allows
defining some Locations as movable
(like palette).")
    (license license:gpl3+)))

(define-public trytond-stock-location-sequence
  (package
    (name "trytond-stock-location-sequence")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_location_sequence" version))
       (sha256
        (base32 "12ahqcxxfsk5iwxyyx7fz1hplp4yjdrw11ybyd1cdvcx3zsjblag"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_location_sequence"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-location-sequence")
    (synopsis "Tryton module to add sequence on location")
    (description "The @emph{Stock Location Sequence} Tryton module adds
ordering to location.")
    (license license:gpl3+)))

(define-public trytond-stock-lot
  (package
    (name "trytond-stock-lot")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_lot" version))
       (sha256
        (base32 "1vgc9j221sp0wrs5c90pl5z5xmi6qdn2jm5s0hpaw94sv883j464"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_lot"))
    (native-inputs
     (cons* trytond-stock-split
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-product
           trytond-stock))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for lot of products")
    (description
     "This package provides a Tryton module that defines lot of products.")
    (license license:gpl3+)))

(define-public trytond-stock-lot-sled
  (package
    (name "trytond-stock-lot-sled")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_lot_sled" version))
       (sha256
        (base32 "1bvdd4jlw3rplpqpjl3k34hzlpv4mmahc1ga3qacr3n03w1040sc"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_lot_sled"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-product
           trytond-stock
           trytond-stock-lot))
    (home-page "https://docs.tryton.org/projects/modules-stock-lot-sled")
    (synopsis "Tryton module for shelf life expiration date of product lots")
    (description "The @emph{Stock Lot Sled} Tryton module adds the \"Shelf
Live Expiration Date\" anf \"Expiration Date\" on \"lot of products\".  When
the shelf life of a lot expires in less than the configured shelf life delay,
it is no more used to compute the forecast quantity of the stock.")
    (license license:gpl3+)))

(define-public trytond-stock-lot-unit
  (package
    (name "trytond-stock-lot-unit")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_lot_unit" version))
       (sha256
        (base32 "0zpyhspnfbcr45x5c5w8dgz1ssl5z72l36c3w4j05z3c0lxs50vz"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_lot_unit"))
    (native-inputs
     (cons* trytond-production
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-product trytond-stock trytond-stock-lot))
    (home-page "https://docs.tryton.org/projects/modules-stock-lot-unit")
    (synopsis "Tryton module to define unit on stock lot")
    (description "The @emph{Stock Lot Unit} Tryton module allows defining a
unit and quantity on stock lot.")
    (license license:gpl3+)))

(define-public trytond-stock-package
  (package
    (name "trytond-stock-package")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_package" version))
       (sha256
        (base32 "1ambw85d8ibi5b5pki8frc401m5xiyjikwrkqlnbi86h6r3agacc"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_package"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-product
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-stock-package")
    (synopsis "Tryton module for stock packaging")
    (description "The @emph{Stock Package} Tryton module allows storing
packaging information about customer and supplier return shipments.")
    (license license:gpl3+)))

(define-public trytond-stock-package-shipping
  (package
    (name "trytond-stock-package-shipping")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_package_shipping" version))
       (sha256
        (base32 "1md2rxdgsgblk2vsicr0xx7pjdixc69awy3wsdlq0vxmxaicahfa"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_package_shipping"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-carrier
           trytond-product
           trytond-product-measurements
           trytond-stock
           trytond-stock-package
           trytond-stock-shipment-cost
           trytond-stock-shipment-measurements))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-package-shipping")
    (synopsis "Tryton base module for interacting with shipping services")
    (description "This Tryton module is the Fundamental module required to
interact with shipping service providers.")
    (license license:gpl3+)))

(define-public trytond-stock-package-shipping-dpd
  (package
    (name "trytond-stock-package-shipping-dpd")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_package_shipping_dpd" version))
       (sha256
        (base32 "18sdjrg6p4bjlv2jahial0as3j54r9r6a915fbw6ji7bvrk9ify6"))))
    (build-system pyproject-build-system)
    ;; doctest requires network and an api key
    (arguments (tryton-arguments "stock_package_shipping_dpd"
                                 "-k not scenario_shipping_dpd"))
    (native-inputs
     (cons* trytond-sale
            trytond-sale-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-pypdf
           python-zeep
           trytond
           trytond-party
           trytond-product
           trytond-stock
           trytond-stock-package
           trytond-stock-package-shipping
           trytond-stock-shipment-measurements))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-package-shipping-dpd")
    (synopsis "DPD connector for the Tryton application platform")
    (description "The @emph{Stock Package Shipping DPD} Tryton module allows
you to generate the DPD label using the DPD webservices.  DPD has many
different web services, the module supports:")
    (license license:gpl3+)))

(define-public trytond-stock-package-shipping-mygls
  (package
    (name "trytond-stock-package-shipping-mygls")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_stock_package_shipping_mygls" version))
              (sha256
               (base32 "1qm1a9mkb8w2jaz149zk7rvv7w9s0irmv9fdnwhp3jmxlfrvh7xm"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_package_shipping_mygls"
                                 "-k not scenario_shipping_mygls"))
    (native-inputs
     (cons* trytond-sale
            trytond-sale-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs (list python-pypdf
                             trytond
                             trytond-carrier
                             trytond-stock
                             trytond-stock-package
                             trytond-stock-package-shipping
                             python-zeep))
    (home-page "https://docs.tryton.org/projects/modules-stock-package-shipping-mygls")
    (synopsis "MyGLS connector for the Tryton application platform")
    (description "The @emph{Stock Package Shipping MyGLS} Tryton module allows
package labels to be generated for shipments using MyGLS webservices.")
    (license license:gpl3+)))

(define-public trytond-stock-package-shipping-sendcloud
  (package
    (name "trytond-stock-package-shipping-sendcloud")
    (version "7.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_stock_package_shipping_sendcloud" version))
              (sha256
               (base32 "04jm3ippy3ym79c1akx9fypw7nj80drbywsa3j7aa5bj043jk269"))))
    (build-system pyproject-build-system)
    ;; doctest requires network and an api key
    (arguments (tryton-arguments "stock_package_shipping_sendcloud"
                                 "-k not scenario_shipping_sendcloud"))
    (native-inputs
     (cons* trytond-sale
            trytond-sale-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs (list python-requests
                             trytond
                             trytond-carrier
                             trytond-company
                             trytond-party
                             trytond-product
                             trytond-stock
                             trytond-stock-package
                             trytond-stock-package-shipping
                             trytond-stock-shipment-measurements))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-package-shipping-sendcloud")
    (synopsis "Sendcloud connector for the Tryton application platform")
    (description "The @emph{Stock Package Shipping Sendcloud} Tryton module
allows package labels to be generated for shipments made by any of Sendcloud’s
supported carriers.")
    (license license:gpl3+)))

(define-public trytond-stock-package-shipping-ups
  (package
    (name "trytond-stock-package-shipping-ups")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_package_shipping_ups" version))
       (sha256
        (base32 "0zpcpfdc0fp258n7kfdmdscal4d922121jxfh6lcgjs5pjfbn8r5"))))
    (build-system pyproject-build-system)
    ;; doctest requires network and an api key
    (arguments (tryton-arguments "stock_package_shipping_ups"
                                 "-k not scenario_shipping_ups"))
    (native-inputs
     (cons* trytond-sale
            trytond-sale-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-oauthlib
           python-requests
           python-requests-oauthlib
           trytond
           trytond-carrier
           trytond-party
           trytond-product
           trytond-stock
           trytond-stock-package
           trytond-stock-package-shipping
           trytond-stock-shipment-measurements))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-package-shipping-ups")
    (synopsis "UPS connector for the Tryton application plateform")
    (description "The @emph{Stock Package Shipping UPS} Tryton module allows
you to generate the UPS labels per package using the UPS webservices.")
    (license license:gpl3+)))

(define-public trytond-stock-product-location
  (package
    (name "trytond-stock-product-location")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_product_location" version))
       (sha256
        (base32 "1pd9qvfzw89c8dx75i2mn23h1gn44mxviji5msm466ig3zi21qhx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases #$(tryton-phases "stock_product_location")
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; DB_CACHE and pytest don't work together here
                (invoke "python" "-m" "unittest" "discover")))))))
    (native-inputs
     (cons* trytond-production
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-product
           trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-product-location")
    (synopsis "Tryton module to add default location on product")
    (description "The @emph{Stock Product Location} Tryton module adds on the
product form a list of preferred location by warehouse.  This list is used
when a supplier shipment is received: the auto-generated Inventory Moves will
use as default destination the preferred locations associated to the current
warehouse.")
    (license license:gpl3+)))

(define-public trytond-stock-quantity-early-planning
  (package
    (name "trytond-stock-quantity-early-planning")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_quantity_early_planning" version))
       (sha256
        (base32 "0zk6y57yykm8yb2qnbcypkfaw3a9g0isqz2v6hl21kpxnyz4sicc"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_quantity_early_planning"))
    (native-inputs
     (cons* trytond-production
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-company trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-quantity-early-planning")
    (synopsis "Tryton module to plan earlier shipments and productions")
    (description "The @emph{Stock Quantity Early Planning} Tryton module helps
reducing stock level by proposing to consume earlier.")
    (license license:gpl3+)))

(define-public trytond-stock-quantity-issue
  (package
    (name "trytond-stock-quantity-issue")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_quantity_issue" version))
       (sha256
        (base32 "19vpka6czcg4s93wx8r7j4rx4i327gr5fpiys1bwzjyqqpw08apa"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_quantity_issue"))
    (native-inputs
     (cons* trytond-production
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond trytond-company trytond-product trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-stock-quantity-issue")
    (synopsis "Tryton module to manage quantity issue with stock")
    (description "The @emph{Stock Quantity Issue} Tryton module helps to solve
stock quantity issues.")
    (license license:gpl3+)))

(define-public trytond-stock-secondary-unit
  (package
    (name "trytond-stock-secondary-unit")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_secondary_unit" version))
       (sha256
        (base32 "0xb85s763yf1icl05f09m4wizk1klby6bqj7addwnld2ycm906ga"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_secondary_unit"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-stock-secondary-unit")
    (synopsis "Tryton module to add a secondary unit on stock move")
    (description "The @emph{Stock Secondary Unit} Tryton module adds a
secondary unit of measure on the stock move.")
    (license license:gpl3+)))

(define-public trytond-stock-shipment-cost
  (package
    (name "trytond-stock-shipment-cost")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_shipment_cost" version))
       (sha256
        (base32 "04v7s9amb225r60zrhlp3kv4xykaws7gwvg1sdvxflq6zkxn0brv"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_shipment_cost"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-carrier
           trytond-product
           trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-stock-shipment-cost")
    (synopsis "Tryton module for stock shipment cost")
    (description "The @emph{Stock Shipment Cost} Tryton Module adds a shipment
cost on the outgoing moves which is calculated from the carrier purchase
price.  This cost is added to the product margin reports.")
    (license license:gpl3+)))

(define-public trytond-stock-shipment-cost-weight
  (package
    (name "trytond-stock-shipment-cost-weight")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_shipment_cost_weight" version))
       (sha256
        (base32 "0ayrwalpn2ryis94wgjvaplp9azfjdyzqw331ipvm5s3ayfcbjia"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_shipment_cost_weight"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-carrier
           trytond-stock-shipment-cost
           trytond-stock-shipment-measurements))
    (home-page "https://docs.tryton.org/projects/modules-stock-shipment-cost-weight")
    (synopsis "Tryton module to allocate shipment cost \"by weight\"")
    (description "The @emph{Stock Shipment Cost Weight} Tryton module adds “by
weight” as allocation method of shipment cost on the carrier.")
    (license license:gpl3+)))

(define-public trytond-stock-shipment-measurements
  (package
    (name "trytond-stock-shipment-measurements")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_shipment_measurements" version))
       (sha256
        (base32 "1w7xaxkqwwgxjlypk1gh1765nd0p28hqnqc4m0qab4r4gl4jgr56"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_shipment_measurements"))
    (native-inputs
     (cons* trytond-stock-package
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-product
           trytond-product-measurements
           trytond-stock))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-shipment-measurements")
    (synopsis "Tryton module to add measurements to shipment")
    (description "The @emph{Stock Shipment Measurements} Tryton module adds
weight and volume on shipments and packages.  They are computed using the
measurement and the quantity of their moves.")
    (license license:gpl3+)))

(define-public trytond-stock-split
  (package
    (name "trytond-stock-split")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_split" version))
       (sha256
        (base32 "0hvkk7n160w05xwyjlh11p41q2wg2wq7zylmh7wypbc6k7qx0m2g"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_split"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-stock))
    (home-page "https://docs.tryton.org/projects/modules-stock-split")
    (synopsis "Tryton module to split stock move")
    (description "The @emph{Stock Split} Tryton module adds on the stock move
a wizard that allows splitting them.  The move is split into moves of Quantity.
If Counts is set, it will be split only this number of times.  On occasion
there can be a move with the remaining quantity.")
    (license license:gpl3+)))

(define-public trytond-stock-supply
  (package
    (name "trytond-stock-supply")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_supply" version))
       (sha256
        (base32 "0c6df8bnhjmwg431z2lnxpy4xm1zsnr4drbj0nn99pfsrkhya5nf"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_supply"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-party
           trytond-product
           trytond-purchase
           trytond-purchase-request
           trytond-stock))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for stock supply")
    (description
     "This package provides a Tryton module that adds automatic supply
mechanisms and introduces the concepts of order point.")
    (license license:gpl3+)))

(define-public trytond-stock-supply-bootstrap
  (hidden-package
   (package/inherit trytond-stock-supply
     (name "trytond-stock-supply-bootstrap")
     (arguments
      (list
       #:tests? #f
       #:phases
       #~(modify-phases %standard-phases
           (delete 'sanity-check))))
     (propagated-inputs
      (modify-inputs (package-propagated-inputs trytond-stock-supply)
        (delete "trytond-purchase-request"))))))

(define-public trytond-stock-supply-day
  (package
    (name "trytond-stock-supply-day")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_supply_day" version))
       (sha256
        (base32 "1bh8wn40s6i6agr67zl7a7k4l537haadp8czgvdcwckz3nlh0flq"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_supply_day"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-purchase))
    (home-page "https://docs.tryton.org/projects/modules-stock-supply-day")
    (synopsis "Tryton module to add supply weekdays")
    (description "The @emph{Stock Supply Day} Tryton module adds a Week Days
list on the Product Supplier form.  This allows restricting the supply week
days for each supplier on each product.  If no days are defined for a supplier
a supplying may happens at any day of the week.")
    (license license:gpl3+)))

(define-public trytond-stock-supply-forecast
  (package
    (name "trytond-stock-supply-forecast")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_supply_forecast" version))
       (sha256
        (base32 "1wp0ajsxpnw03az5xrhd24lyh64d2w357x6456c622yi4bgg957v"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_supply_forecast"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-stock-forecast trytond-stock-supply))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-supply-forecast")
    (synopsis "Tryton module to add forecast to supply computation")
    (description "The @emph{Stock Supply Forecast} Tryton module takes
forecast into account to compute purchase requests.")
    (license license:gpl3+)))

(define-public trytond-stock-supply-production
  (package
    (name "trytond-stock-supply-production")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_supply_production" version))
       (sha256
        (base32 "0anghn1h2afv7j6wr4b4ymwwyr1kf7j98l01v857p1q6086k898m"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "stock_supply_production"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-product trytond-production trytond-stock
           trytond-stock-supply))
    (home-page
     "https://docs.tryton.org/projects/modules-stock-supply-production")
    (synopsis "Tryton module for stock supply of production")
    (description "The @emph{Stock Supply Production} module adds automatic
supply mechanisms via production request.")
    (license license:gpl3+)))

(define-public trytond-timesheet
  (package
    (name "trytond-timesheet")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_timesheet" version))
       (sha256
        (base32 "1f4js2aykh68b272mijlqlzphkim2c37qb9dnbgcydqaydvy9025"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "timesheet"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond
           trytond-company
           trytond-company-work-time))
    (home-page "https://docs.tryton.org/projects/modules-timesheet")
    (synopsis "Tryton module with timesheets")
    (description "The @emph{Timesheet} Tryton module allows tracking the time
spent by employees on various works.  This module also comes with several
reports that show the time spent by employees on works following various time
periods.")
    (license license:gpl3+)))

(define-public trytond-timesheet-cost
  (package
    (name "trytond-timesheet-cost")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_timesheet_cost" version))
       (sha256
        (base32 "0j01gmci9kzjlp0jplg3k36hzwa4cws51jxbjlni65szmm4vsvkw"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "timesheet_cost"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-company trytond-party trytond-timesheet))
    (home-page "https://docs.tryton.org/projects/modules-timesheet-cost")
    (synopsis "Tryton module to add cost on timesheet")
    (description "The @emph{Timesheet Cost} Tryton module adds cost price per
employee.")
    (license license:gpl3+)))

(define-public trytond-user-role
  (package
    (name "trytond-user-role")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_user_role" version))
       (sha256
        (base32 "08mds3hfzwfhk7cgdanhz9p943naqi355ih9w15psri4h87234i1"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "user_role"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond))
    (home-page "https://docs.tryton.org/projects/modules-user-role")
    (synopsis "Tryton module to manage roles on users")
    (description "This package provides a Tryton module for assigning roles to
user instead of groups.  A Role is defined by a set of groups.  When a role is
added to a user, it overrides the existing groups.  A role can be added to a
user for a period of time only.")
    (license license:gpl3+)))

(define-public trytond-web-shop
  (package
    (name "trytond-web-shop")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_web_shop" version))
       (sha256
        (base32 "19i8c34jcgni6q6fyr0dbfpcbcri9cw2nrwh7j609yspvi4x2was"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "web_shop"))
    (native-inputs
     (cons* trytond-account-tax-rule-country
            trytond-product-attribute
            trytond-product-image
            trytond-sale-price-list
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list trytond
           trytond-account
           trytond-company
           trytond-country
           trytond-currency
           trytond-product
           trytond-sale
           trytond-stock
           trytond-web-user))
    (home-page "https://docs.tryton.org/projects/modules-web-shop")
    (synopsis "Tryton module that provides a common base for webshops")
    (description "The @emph{Web Shop} Tryton module facilitates storing
configuration of an online web shop.")
    (license license:gpl3+)))

(define-public trytond-web-shop-shopify
  (package
    (name "trytond-web-shop-shopify")
    (version "7.0.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trytond_web_shop_shopify" version))
              (sha256
               (base32 "18v46n58di6rk1kzslqgbjqrpf4d2wqi9qcxcsq3lv1a9lf2z8b8"))))
    (build-system pyproject-build-system)
    ;; doctest requires network and an account at shopify
    (arguments
     (tryton-arguments "web_shop_shopify" "-k not scenario_web_shop_shopify"))
    (native-inputs
     (cons* trytond-account-payment-clearing
            trytond-customs
            trytond-product-image
            trytond-product-measurements
            trytond-sale-discount
            trytond-sale-secondary-unit
            trytond-sale-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-pyactiveresource
           python-shopifyapi
           trytond
           trytond-account-payment
           trytond-currency
           trytond-party
           trytond-product
           trytond-product-attribute
           trytond-sale
           trytond-sale-amendment
           trytond-sale-payment
           trytond-stock
           trytond-web-shop))
    (home-page "https://docs.tryton.org/projects/modules-web-shop-shopify")
    (synopsis "Integrate Tryton with Shopify")
    (description "The @emph{Web Shop Shopify} Tryton module provides a way to
manage @emph{Shopify} stores.  It uploads products, variants and collections
to Shopify, and downloads orders, transactions and creates fulfilments.")
    (license license:gpl3+)))

(define-public trytond-web-shop-vue-storefront
  (package
    (name "trytond-web-shop-vue-storefront")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_web_shop_vue_storefront" version))
       (sha256
        (base32 "051xsffvhigra6xf7vnl9vnsd9d393xg9b2alcfy6h5fz5f2zxcr"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "web_shop_vue_storefront"))
    (native-inputs
     (cons* trytond-carrier
            trytond-product-attribute
            trytond-product-image
            trytond-sale-promotion-coupon
            trytond-sale-shipment-cost
            %standard-trytond-native-inputs))
    (propagated-inputs
     (list python-elasticsearch
           python-stdnum
           trytond
           trytond-party
           trytond-product
           trytond-sale
           trytond-web-shop
           trytond-web-user))
    (home-page
     "https://docs.tryton.org/projects/modules-web-shop-vue-storefront")
    (synopsis "Tryton module to integrate with Vue Storefront")
    (description "This Tryton module provides the back-end to integrate with
Vue Storefront 1.x.")
    (license license:gpl3+)))

(define-public trytond-web-shop-vue-storefront-stripe
  (package
    (name "trytond-web-shop-vue-storefront-stripe")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_web_shop_vue_storefront_stripe" version))
       (sha256
        (base32 "1350gyrvx8r845cikvl0p3z9rpnkz2apcqz5blx26pzhdlvkk5x5"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "web_shop_vue_storefront_stripe"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-account-payment-stripe trytond-sale-payment
           trytond-web-shop trytond-web-shop-vue-storefront))
    (home-page
     "https://docs.tryton.org/projects/modules-web-shop-vue-storefront-stripe")
    (synopsis "Tryton module to support Stripe payment with Vue Storefront")
    (description "The @emph{Web Shop Vue Storefront Stripe} Tryton module
provides support of Stripe payment for Vue Storefront integration.")
    (license license:gpl3+)))

(define-public trytond-web-shortener
  (package
    (name "trytond-web-shortener")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_web_shortener" version))
       (sha256
        (base32 "052c4sylpqjgwgk02zy9kq1pdxbnfciga7lf8aip8sxry55n95in"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "web_shortener"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond))
    (home-page "https://docs.tryton.org/projects/modules-web-shortener")
    (synopsis "Tryton module to plug a URL to an action")
    (description "The @emph{Web Shortener} Tryton module allows URLs to be
shortened.  It also counts the number of times the URL is accessed and
optionally triggers action.")
    (license license:gpl3+)))

(define-public trytond-web-user
  (package
    (name "trytond-web-user")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_web_user" version))
       (sha256
        (base32 "1ww3074ywgvh7rl007m85lndnhywkx71gsi302nxrsvrkjnxjjyc"))))
    (build-system pyproject-build-system)
    (arguments (tryton-arguments "web_user"))
    (native-inputs %standard-trytond-native-inputs)
    (propagated-inputs
     (list trytond trytond-party))
    (home-page "https://docs.tryton.org/projects/modules-web-user")
    (synopsis "Tryton module to manage Web users")
    (description "The @emph{Web User} Tryton module provides facilities to
manage external user accessing from the web.")
    (license license:gpl3+)))
