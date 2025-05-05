;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021, 2023-2024 jgart <jgart@dismail.de>
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

(define-module (gnu packages spreadsheet)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages time)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xml))

(define-public sc-im
    (package
      (name "sc-im")
      (version "0.8.3")
      (home-page "https://github.com/andmarti1424/sc-im")
      (source (origin
                (method git-fetch)
                (uri
                  (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                  (base32
                    "10x50smn0xl9k6m0bgfcvpwgvivmjkysawvc4zyibc8nxlqz2na2"))))
      (build-system gnu-build-system)
      (arguments
        (list
         #:make-flags
           #~(list "-C" "src"
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "prefix=" #$output))
          #:phases
            #~(modify-phases %standard-phases
                 (delete 'configure)
                 (add-after 'unpack 'fix-bash-path-declaration-in-script
                   (lambda _
                     (substitute* "tests/run_all_tests.sh"
                       (("/bin/bash") (string-append "#!" (which "bash"))))))
                 (replace 'check
                   (lambda* (#:key tests? #:allow-other-keys)
                     (with-directory-excursion "tests"
                       (when tests?
                         (invoke "bash" "run_all_tests.sh"))))))))
      (inputs
        (list gnuplot
              libxls
              libxlsxwriter
              libxml2
              libzip
              ncurses))
      (native-inputs
        (list bash-minimal
              bison
              pkg-config
              valgrind/pinned
              which))
      (synopsis "Spreadsheet program with vim-like keybindings")
      (description
 "@code{sc-im} is a highly configurable spreadsheet program
 providing a vim-like experience.  @code{sc-im} supports @{gnuplot} interaction,
 functions for sorting and filtering, 256 color support, and much more.")
      (license bsd-4)))

(define-public visidata
  (package
    (name "visidata")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "visidata" version))
       (sha256
        (base32
         "0cdhx0n79n9z5d22nr90kkg93ndxcnyl4margs4f8l88iwaq8i4c"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'set-home-directory
                 (lambda _ (setenv "HOME" "/tmp")))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests? (invoke "pytest")))))))
    (inputs
     (list python-dateutil
           python-importlib-metadata
           python-requests
           python-lxml
           python-openpyxl
           python-xlrd))
    (native-inputs
     (list python-pytest))
    (synopsis "Terminal spreadsheet multitool for discovering and arranging data")
    (description
     "VisiData is an interactive multitool for tabular data.  It combines the
clarity of a spreadsheet, the efficiency of the terminal, and the power of
Python, into a lightweight utility which can handle millions of rows.")
    (home-page "https://www.visidata.org/")
    (license gpl3)))
