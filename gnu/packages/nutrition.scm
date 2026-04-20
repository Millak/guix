;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages nutrition)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml))

(define-public python-scrape-schema-recipe
  (package
    (name "python-scrape-schema-recipe")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/micahcochran/scrape-schema-recipe")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yrqawnxp1k9ps0c920qjbbaxpmz3cf8bzyjs1kimvlxym78b614"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'configure-tests
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (substitute* "test_scrape.py"
                  (("DISABLE_NETWORK_TESTS = False")
                   "DISABLE_NETWORK_TESTS = True"))))))))
    (native-inputs (list python-pytest python-setuptools))
    (propagated-inputs
     (list python-extruct
           python-importlib-resources
           python-isodate
           python-requests))
    (home-page "https://github.com/micahcochran/scrape-schema-recipe")
    (synopsis "HTML Recipe format extractor")
    (description "This tool extracts cooking recipe from HTML structured data
in the @url{https://schema.org/Recipe} format.")
    (license asl2.0)))
