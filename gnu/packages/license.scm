;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Evgeny Pisemsky <mail@pisemsky.site>
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

(define-module (gnu packages license)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages web)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public perl-regexp-pattern-license
  (package
    (name "perl-regexp-pattern-license")
    (version "3.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JO/JONASS/Regexp-Pattern-License-"
             "v" version ".tar.gz"))
       (sha256
        (base32 "1blkraliby1696pqici7k1pkwcrf7gbdavfxfffa2mk8lr4a6xw6"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-regexp-pattern perl-test-regexp-pattern
           perl-test-without-module perl-test2-suite perl-try-tiny))
    (home-page "https://metacpan.org/release/Regexp-Pattern-License")
    (synopsis "Regular expressions for legal licenses")
    (description "Regexp::Pattern::License provides a hash of regular
expression patterns related to legal software licenses.

Regexp::Pattern is a convention for organizing reusable regex patterns.")
    (license gpl3+)))

(define-public perl-string-copyright
  (package
    (name "perl-string-copyright")
    (version "0.003014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JO/JONASS/String-Copyright-"
             version ".tar.gz"))
       (sha256
        (base32
         "0xdm0ml65r77sk1pklnq4spbmn9qid4m44rnva8hhh00b9044k9f"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-without-module perl-test2-suite))
    (propagated-inputs
     (list perl-exporter-tiny perl-set-intspan))
    (home-page "https://metacpan.org/release/String-Copyright")
    (synopsis "Representation of text-based copyright statements")
    (description "String::Copyright Parses common styles of copyright
statements and serializes in normalized format.")
    (license gpl3+)))

(define-public perl-string-license
  (package
    (name "perl-string-license")
    (version "0.0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JO/JONASS/String-License-v"
                    version ".tar.gz"))
              (sha256
               (base32
                "1dbw8rgwdlgnlvscijpb2dnw5irfd6wvca587bprq5kk19mf7jzf"))))
    (build-system perl-build-system)
    (native-inputs (list perl-file-basedir
                         perl-regexp-pattern-license
                         perl-software-license
                         perl-test-without-module
                         perl-test2-suite
                         perl-yaml-libyaml))
    (propagated-inputs (list perl-array-intspan
                             perl-feature-compat-class
                             perl-log-any
                             perl-namespace-clean
                             perl-path-tiny
                             perl-regexp-pattern
                             perl-regexp-pattern-license))
    (home-page "https://metacpan.org/release/String-License")
    (synopsis "Detect source code license statements in a text string")
    (description "@code{String::License} identifies license statements in a
string and serializes them in a normalized format.")
    (license agpl3+)))

(define-public perl-software-license
  (package
    (name "perl-software-license")
    (version "0.103014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/L/LE/LEONT/Software-License-"
             version ".tar.gz"))
       (sha256
        (base32
         "128pbm9pf5drakm9bpkifc1zg8f005xabfwzg21nc03m5mhfligb"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-try-tiny))
    (propagated-inputs
     (list perl-data-section perl-text-template))
    (home-page "https://metacpan.org/release/Software-License")
    (synopsis "Templated software licenses")
    (description "This package provides templated software licenses.")
    (license (package-license perl))))

(define-public licensecheck
  (package
    (name "licensecheck")
    (version "3.3.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JO/JONASS/App-Licensecheck-"
                    "v" version ".tar.gz"))
              (sha256
               (base32
                "17sfw2cz5x339zq6xc2nfjps2vwpj3d307v90gva498fvnlk1y4y"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-encode-locale
           perl-regexp-pattern-license
           perl-string-copyright
           perl-test-without-module
           perl-test2-suite
           perl-test2-tools-command))
    (propagated-inputs
     (list perl-feature-compat-class
           perl-feature-compat-try
           perl-io-interactive
           perl-log-any
           perl-log-any-adapter-screen
           perl-namespace-clean
           perl-path-iterator-rule
           perl-path-tiny
           perl-pod-constants
           perl-string-copyright
           perl-string-escape
           perl-string-license))
    (inputs (list bash-minimal))        ; for wrap-program
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-licensecheck
            (lambda _
              (let ((licensecheck (string-append #$output "/bin/licensecheck"))
                    (perl5lib (string-append #$output "/lib/perl5/site_perl/"
                                             #$(package-version perl))))
                (wrap-program licensecheck
                  `("PERL5LIB" ":" prefix
                    ,(list perl5lib (getenv "PERL5LIB")))))))
          (add-after 'wrap-licensecheck 'check-wrap
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (unsetenv "PERL5LIB")
                (invoke/quiet (string-append #$output "/bin/licensecheck")
                              "--version")))))))
    (home-page "https://metacpan.org/release/App-Licensecheck")
    (synopsis "License checker for source files")
    (description "Licensecheck attempts to determine the license that applies
to each file passed to it, by searching the start of the file for text
belonging to various licenses.")
    (license agpl3+)))

(define-public reuse
  (package
    (name "reuse")
    (version "5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "reuse" version))
       (sha256
        (base32 "0p08xmpf361m81kfmkwzm898q9iaq5v6cvb0sjx1176jbnp1d047"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Change directory before running the test suite to avoid having both
     ;; the local sources on GUIX_PYTHONPATH as well as the installed
     ;; libraries confusing Pytest (ImportPathMismatchError).
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'chdir
                          (lambda _
                            (chdir "/tmp"))))
           ;; The test_simple test hangs (see:
           ;; https://github.com/fsfe/reuse-tool/issues/1119).
           #:test-flags #~(list "-k" "not test_simple")))
    (native-inputs
     (list python-freezegun
           python-poetry-core
           python-pytest
           python-wheel))
    (inputs
     (list python-attrs
           python-binaryornot
           python-boolean.py
           python-click
           python-debian
           python-jinja2
           python-license-expression
           python-tomlkit))
    (home-page "https://reuse.software/")
    (synopsis "Provide and verify copyright and licensing information")
    (description
     "The REUSE tool helps you achieve and confirm license compliance with the
@uref{https://reuse.software, REUSE specification}, a set of recommendations
for licensing Free Software projects.  REUSE makes it easy to declare the
licenses under which your works are released, especially when reusing software
from different projects released under different licenses.  It avoids reliance
on fuzzy heuristicts and allows both legal experts and computers to understand
how your project is licensed.  This allows generating a \"bill of materials\"
for software.

This tool downloads full license texts, adds copyright and license information
to file headers, and contains a linter to identify problems.  There are other
tools that have a lot more features and functionality surrounding the analysis
and inspection of copyright and licenses in software projects.  This one is
designed to be simple.")
    (license (list asl2.0 cc0 cc-by-sa4.0 gpl3+))))

(define-public licenseheaders
  (package
    (name "licenseheaders")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "licenseheaders" version))
       (sha256
        (base32 "073xcm10gyg5kcxqmbsyaz9sr0slbdwgr0r9qanch0zl8i0z9259"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Reported upstream:
         ;; <https://github.com/johann-petrak/licenseheaders/issues/47>.
         (add-after 'unpack 'patch-code
           (lambda _
             (substitute* "licenseheaders.py"
               (("\\\"filenames\\\": \\[\\\"CMakeLists.txt\\\"\\],")
                "\"filenames\": [\"CMakeLists.txt\"], \n        \"extensions\": [],"))
             #t)))))
    (propagated-inputs
     (list python-regex))
    (home-page "https://github.com/johann-petrak/licenseheaders")
    (synopsis "Add or change license headers for all files in a directory")
    (description
     "Licenseheaders is a Python 3 tool to update, change or add license
headers to all files of any of the supported types in or below some
directory.")
    (license expat)))
