;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2020 Sergei Trofimovich <slyfox@inbox.ru>
;;; Copyright © 2021 Sergei Trofimovich <slyich@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2025 Alexey Abramov <levenson@mmer.org>
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

(define-module (gnu packages re2c)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public re2c
  (package
    (name "re2c")
    (version "4.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/skvadrik/" name
                                 "/releases/download/" version "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
                "07ysqgdm0h566a8lwnpdgycp93vz7zskzihsgah3bla0ycj2pp69"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? (not (or (%current-target-system)
                             ;; run_tests.py hangs
                             (system-hurd?)))
           #:phases
           (if (target-arm32?)
               #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-sources
                     (lambda _
                       (invoke "patch" "-p1" "--force" "--input"
                               #$(local-file (search-patch
                                              "re2c-Use-maximum-alignment.patch"))))))
               #~%standard-phases)))
    (native-inputs
     (list python))             ; for the test driver
    (home-page "https://re2c.org/")
    (synopsis "Lexer generator for C/C++")
    (description
     "@code{re2c} generates minimalistic hard-coded state machine (as opposed
to full-featured table-based lexers).  A flexible API allows generated code
to be wired into virtually any environment.  Instead of exposing a traditional
@code{yylex()} style API, re2c exposes its internals.  Be sure to take a look
at the examples, as they cover a lot of real-world cases and shed some light on
dark corners of the re2c API.")
    (license public-domain)))
