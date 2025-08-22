;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages fcitx)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public presage
  (package
    (name "presage")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/presage/presage/"
                       version "/presage-" version ".tar.gz"))
       (sha256
        (base32 "0rm3b3zaf6bd7hia0lr1wyvi1rrvxkn7hg05r5r1saj0a3ingmay"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "CFLAGS=-Wno-narrowing -std=c++14"
        "CXXFLAGS=-Wno-narrowing -std=c++14")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share/presage"))
               (rename-file
                (string-append out "/share/presage/html")
                (string-append doc "/share/presage/html")))))
         (add-after 'unpack 'update-config-scripts
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (for-each (lambda (file)
                         (install-file
                          (search-input-file
                           (or native-inputs inputs)
                           (string-append "/bin/" file)) "."))
                       '("config.guess" "config.sub")))))))
    (native-inputs
     `(("config" ,config)
       ("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("help2man" ,help2man)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (inputs
     (list glib gtk+ libx11 sqlite tinyxml))
    (synopsis "Intelligent Predictive Text Entry System")
    (description "Presage generates predictions by modelling natural language as
a combination of redundant information sources.  It computes probabilities for
words which are most likely to be entered next by merging predictions generated
by the different predictive algorithms.")
    (home-page "https://presage.sourceforge.io/")
    (license gpl2+)))

