;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2025 Evgeny Pisemsky <mail@pisemsky.site>
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

(define-module (gnu packages figlet)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages video))

(define-public figlet
  (package
    (name "figlet")
    (version "2.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.figlet.org/pub/figlet/program"
                           "/unix/figlet-" version ".tar.gz"))
       (sha256
        (base32 "0za1ax15x7myjl8jz271ybly8ln9kb9zhm1gf6rdlxzhs07w925z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases (delete 'configure))
       #:make-flags
       (list (string-append "prefix=" %output))))
    (home-page "http://www.figlet.org/")
    (synopsis "Make large letterforms out of ordinary screen characters")
    (description "FIGlet is a program for making large ASCII art letterforms
out of ordinary screen characters.")
    (license license:bsd-3)))

(define-public toilet
  (package
    (name "toilet")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://caca.zoy.org/raw-attachment/wiki/toilet/toilet-" version
             ".tar.gz"))
       (sha256
        (base32 "1pl118qb7g0frpgl9ps43w4sd0psjirpmq54yg1kqcclqcqbbm49"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libcaca))
    (home-page "http://caca.zoy.org/wiki/toilet")
    (synopsis "Display large colourful characters")
    (description
     "The TOIlet project attempts to create a free replacement for the
FIGlet utility.  It uses the powerful libcaca library to achieve
various text-based effects.")
    (license license:wtfpl2)))
