;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2026 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages genealogy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml))

(define-public gramps
  (package
    (name "gramps")
    (version "6.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gramps-project/gramps")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06kdcf6h9rqdcsx097a3n2z4q8yvln20ja7ajwrna1i133591bra"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:imported-modules (append %glib-or-gtk-build-system-modules
                                 %pyproject-build-system-modules)
      #:modules
      `((ice-9 match)
        (srfi srfi-1)
        (guix build pyproject-build-system)
        ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
        (guix build utils))
      ;; tests: 32408 passed, 44 skipped, 9 deselected, 1 warning, 65730 subtests passed
      #:test-flags
      ;; Tests failing during collection.
      #~(list "--ignore=gramps/gen/merge/test/merge_ref_test.py"
              "--ignore=po/test/po_test.py"
              "--ignore=test/LosHawlos_db_test.py"
              "--ignore=test/blob_to_json_test.py"
              ;; Assertions are not equal for these tests.
              #$@(map (lambda (ls) (string-append "--deselect=gramps/"
                                                  (string-join ls "::")))
                      '(("gen/proxy/test/proxies_test.py"
                         "LivingProxyTest" "test_person_count")
                        ("gen/proxy/test/proxies_test.py"
                         "LivingPrivateProxyTest" "test_person_count")
                        ("plugins/test/reports_test.py"
                         "TestDynamic" "test_export_gedcom")
                        ("plugins/test/reports_test.py"
                         "TestDynamic" "test_export_gpkg")
                        ("plugins/test/reports_test.py"
                         "TestDynamic" "test_indiv_complete")
                        ("plugins/test/reports_test.py"
                         "TestDynamic" "test_navwebpage")
                        ("plugins/test/reports_test.py"
                         "TestDynamic" "test_tool_check")
                        ("plugins/test/tools_test.py"
                         "ToolControl" "test_tcg_and_check_and_repair")
                        ("plugins/test/db_undo_and_signals_test.py"
                         "TestSQLite" "test_one"))))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-gexiv2-dependency
            (lambda _
              ;; Use our own version
              (substitute* (find-files "." ".*\\.py$")
                (("\"GExiv2\", \"0.10\"") "\"GExiv2\", \"0.16\""))))
          (add-after 'unpack 'swap-bsddb3-with-berkeleydb
            ;; bsddb3 was renamed upstream to berkeleydb.
            (lambda _
              (substitute* (find-files "." ".*\\.py")
                (("bsddb3") "berkeleydb"))))
          (add-before 'check 'prepare-tests
            (lambda _
              (setenv "GDK_BACKEND" "-")
              (setenv "HOME" "/tmp")
              ;; Presence of .git directory is used to determine whether this
              ;; is a final installation.  Without it, tests fail to determine
              ;; resource path.
              (mkdir ".git")))
          (add-before 'wrap 'wrap-with-GI_TYPELIB_PATH
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/gramps")
                `("GI_TYPELIB_PATH" ":" prefix
                  ,(filter-map
                    (match-lambda
                      ((output . directory)
                       (let ((girepodir (string-append
                                         directory
                                         "/lib/girepository-1.0")))
                         (and (file-exists? girepodir)
                              girepodir))))
                    inputs)))))
          (add-after 'wrap 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list gettext-minimal
           intltool
           python-jsonschema
           python-mock
           python-pytest
           python-setuptools))
    (inputs
     (list bash-minimal
           cairo
           font-gnu-freefont
           geocode-glib
           gexiv2
           ghostscript
           gobject-introspection
           gtk+
           gtkspell3
           graphviz
           (librsvg-for-system)
           osm-gps-map
           pango
           python-berkeleydb
           python-imagesize
           python-orjson
           python-pillow
           python-pycairo
           python-pycountry
           python-pygobject
           python-pyicu
           rcs
           sqlite
           xdg-utils))
    (home-page "https://gramps-project.org")
    (synopsis "Genealogical research software")
    (description
     "Gramps is a free software project and community striving to produce
a genealogy program that is both intuitive for hobbyists and feature-complete
for professional genealogists.")
    (license license:gpl2+)))
