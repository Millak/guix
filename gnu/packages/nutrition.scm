;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml))

(define-public python-scrape-schema-recipe
  (package
    (name "python-scrape-schema-recipe")
    (version "0.2.0")
    ;; The PyPI archive lacks a VERSION file as well as the test suite.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/micahcochran/scrape-schema-recipe")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "044c6qwhb4c710ksgiw29cd0qcp84h1m4y8yr2g4c8vdlm3kkqh5"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (substitute* "test_scrape.py"
                  (("DISABLE_NETWORK_TESTS = False")
                   "DISABLE_NETWORK_TESTS = True"))
                (invoke "pytest" "-vv")))))))
    (native-inputs (list python-pytest))
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

(define-public gourmet
  ;; Use the latest commit to gain Python 3 support.
  (let ((revision "0")
        (commit "8af29c8ded24528030e5ae2ea3461f61c1e5a575"))
    (package
      (name "gourmet")
      (version (git-version "0.17.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/thinkle/gourmet")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08fbw6zp32ws6w9czwy2sqc9c9izlkglsskshj2114d0l79z4gj8"))
         (patches (search-patches "gourmet-sqlalchemy-compat.patch"))))
      (build-system python-build-system)
      (arguments
       (list
        #:modules `((guix build utils)
                    (guix build python-build-system)
                    (ice-9 ftw)
                    (srfi srfi-26))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'check 'prepare-x
              ;; Both the tests and the sanity-check phase need an X server to
              ;; succeed.
              (lambda _
                (system "Xvfb &")
                (setenv "DISPLAY" ":0")))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (setenv "HOME" "/tmp") ;needed by tests
                  (apply invoke "pytest" "-vv"
                         ;; XXX: This is needed because some tests in deeper
                         ;; directories or otherwise discovered by Pytest are
                         ;; broken.
                         (map (cut string-append "gourmet/tests/" <>)
                              (scandir "gourmet/tests"
                                       (cut string-prefix? "test_" <>)))))))
            (add-after 'install 'install-dekstop-file-and-icons
              (lambda _
                (define share (string-append #$output "/share"))
                (install-file ".flatpak/io.github.thinkle.Gourmet.desktop"
                              (string-append share "/applications"))
                (install-file ".flatpak/io.github.thinkle.Gourmet.svg"
                              (string-append share "/icons/Gourmet")))))))
      (native-inputs
       (list python-dogtail
             python-pytest
             python-selenium
             xorg-server-for-tests))
      (inputs
       (list gtk+
             python-argcomplete
             python-beautifulsoup4
             python-gst
             python-keyring
             python-lxml
             python-pillow
             python-pycairo
             python-pyenchant
             python-pygobject
             python-requests
             python-scrape-schema-recipe
             python-sqlalchemy))
      (home-page "https://thinkle.github.io/gourmet/")
      (synopsis "Recipe organizer")
      (description
       "Gourmet Recipe Manager is a recipe organizer that allows you to collect,
search, organize, and browse your recipes.  Gourmet can also generate shopping
lists and calculate nutritional information.  It imports Mealmaster,
MasterCook and KRecipe files and exports PDFs, webpages, and other formats.")
      (license gpl2+))))
