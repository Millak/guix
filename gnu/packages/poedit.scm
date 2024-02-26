;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages poedit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml))

(define-public poedit
  (package
    (name "poedit")
    (version "3.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vslavik/poedit")
             (commit (string-append "v" version "-oss"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "119hhhjikzdvawldav99zzxnx7rj2ri5crq8smb2lprab8fx70hm"))
       (modules '((guix build utils)))
       (snippet #~(delete-file-recursively "deps"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-boost-libdir="
                             #$(this-package-input "boost") "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-executable
            (lambda _
              (wrap-program (string-append #$output "/bin/poedit")
                `("PATH" prefix
                  ,(list (string-append #$(this-package-input "gettext")
                                        "/bin")))))))))

    (native-inputs (list autoconf automake pkg-config))
    (inputs (list bash-minimal
                  boost
                  enchant
                  gnu-gettext
                  gtk+
                  gtkspell3
                  icu4c
                  lucene++
                  nlohmann-json
                  python-minimal
                  pugixml
                  wxwidgets))
    (home-page "https://poedit.net/")
    (synopsis "Gettext catalog editing tool")
    (description
     "Poedit is a GUI frontend to the GNU gettext utilities and a catalog
editor/source code parser.  It helps with translating applications into other
languages.")
    (license license:expat)))
