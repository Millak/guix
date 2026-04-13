;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages gkrellm)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

(define-public gkrellm
  (package
    (name "gkrellm")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gkrellm.srcbox.net/releases/gkrellm-"
                           version ".tar.bz2"))
       (sha256
        (base32 "0qvy2xmwmfy5f0g09yn9lr262shnx82ba04r5il2wj4qscg3r7h8"))))
    (build-system gnu-build-system)
    (inputs (list gtk+-2 libsm))
    (native-inputs (list gettext-minimal pkg-config))
    (arguments
     (list
      #:tests? #f ;there is no check target
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)) ;no configure script
      #:make-flags
      #~(list (string-append "INSTALLROOT=" #$output)
              (string-append "CC=" #$(cc-for-target)))))
    (home-page "https://gkrellm.srcbox.net/")
    (synopsis "System monitors")
    (description
     "GKrellM is a single process stack of system monitors which supports
applying themes to match its appearance to your window manager, Gtk, or any
other theme.")
    (license license:gpl3+)))
