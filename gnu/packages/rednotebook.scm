;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
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

(define-module (gnu packages rednotebook)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages webkit))

(define-public rednotebook
  (package
    (name "rednotebook")
    (version "2.41")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jendrikseipp/rednotebook")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nljj227lykl4gq1qvvv0pj00k8pbi3njm4agz7wsfcihz6dlrxi"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:imported-modules
      `((guix build glib-or-gtk-build-system)
        ,@%pyproject-build-system-modules)
      #:modules
      `((ice-9 match)
        (guix build pyproject-build-system)
        ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'sanity-check 'configure-tests
            (lambda _
              (setenv "HOME" (getcwd))))
          ;; Make sure rednotebook can find the typelibs and webkitgtk shared
          ;; libraries.
          (add-before 'wrap 'wrap-with-library-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((webkitgtk-bin (search-input-file inputs
                                                      "bin/WebKitWebDriver")))
                (wrap-program (string-append #$output "/bin/rednotebook")
                  `("GI_TYPELIB_PATH" ":" prefix
                    (,(getenv "GI_TYPELIB_PATH")))
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append (dirname (dirname webkitgtk-bin))
                                     "/lib"))))))))))
    (native-inputs (list gettext-minimal python-pytest python-setuptools))
    (inputs
     (list bash-minimal
           gtk+
           gtksourceview-3
           python-pyyaml
           python-pygobject
           webkitgtk-for-gtk3))
    ;; TODO: package the following for python3 (if possible), add them as
    ;; dependencies, and remove them from rednotebook source:
    ;; pygtkspellcheck, elib.intl, txt2tags
    ;; TODO: package and add pyenchant for python3 and add it as a dependency.
    (home-page "https://www.rednotebook.app")
    (synopsis "Daily journal with calendar, templates and keyword searching")
    (description
     "RedNotebook is a modern desktop journal.  It lets you format, tag and
search your entries.  You can also add pictures, links and customizable
templates, spell check your notes, and export to plain text, HTML, Latex or
PDF.")
    (license (list license:gpl2+     ; rednotebook, txt2tags
                   license:lgpl3+    ; elib.intl
                   license:gpl3+)))) ; pygtkspellcheck
