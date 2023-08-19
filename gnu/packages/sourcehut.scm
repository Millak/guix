;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages sourcehut)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages))

(define-public python-core-sr-ht
  (package
    (name "python-core-sr-ht")
    (version "0.69.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~sircmpwn/core.sr.ht")
             (commit version)
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00d633lmdiqamyv24szmd441dhkjh1n1qkvf47vn57da0z1rxp2g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false                   ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _ (setenv "PKGVER" #$version))))))
    (propagated-inputs
     (list python-alembic
           python-beautifulsoup4
           python-bleach
           python-celery
           python-cryptography
           python-flask
           python-humanize
           python-markdown
           python-mistletoe
           python-prometheus-client
           python-psycopg2
           python-pygments
           python-redis
           python-requests
           python-sqlalchemy
           python-sqlalchemy-utils))
    (home-page "https://git.sr.ht/~sircmpwn/core.sr.ht")
    (synopsis "Shared code for all sourcehut projects")
    (description
     "This package contains code shared among all sr.ht projects.")
    (license license:bsd-3)))

(define-public python-scm-sr-ht
  (package
    (name "python-scm-sr-ht")
    (version "0.22.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~sircmpwn/scm.sr.ht")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1p1nlwqfqfb89nky8sp4jh4vjnh8fm2mdx8inziqs3898qw1v7yk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false                   ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _ (setenv "PKGVER" #$version))))))
    (propagated-inputs
     (list python-core-sr-ht))
    (home-page "https://git.sr.ht/~sircmpwn/scm.sr.ht")
    (synopsis "Shared support code for sr.ht source control services")
    (description
     "This package provides shared support code for sr.ht source control
services.")
    (license license:agpl3)))
