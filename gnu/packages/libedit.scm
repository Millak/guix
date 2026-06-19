;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2020 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2026 Edouard Klein <edk@beaver-labs.com>
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

(define-module (gnu packages libedit)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages ncurses))

(define-public isocline
  (package
    (name "isocline")
    (version "1.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/daanx/isocline")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "080ffn40ww4h68l4lwdc1k9lrivi31jdw44kv90ppvbymxfjy4zn"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./test_colors"))))
          (replace 'install
            (lambda _
              (install-file "libisocline.a"
                            (string-append #$output "/lib"))
              (install-file "../source/include/isocline.h"
                            (string-append #$output "/include")))))))
    (home-page "https://github.com/daanx/isocline")
    (synopsis "Portable alternative to GNU Readline")
    (description
     "Isocline is a small, portable line-editing library that provides
multiline editing, history, completion, Unicode support, syntax highlighting,
and other interactive input features.  It has no external dependencies and
can be used as an alternative to GNU Readline.")
    (license expat)))

(define-public libedit
  (package
    (name "libedit")
    (version "20260512-3.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://thrysoee.dk/editline"
                          "/libedit-" version ".tar.gz"))
      (sha256
       (base32 "18547v10g6g8mhkaazmplvx7gv8f3p0ppjifkz9ns4dhm1z5wba3"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses))
    (home-page "https://thrysoee.dk/editline/")
    (synopsis "NetBSD Editline library")
    (description
     "This is an autotool- and libtoolized port of the NetBSD Editline
library (libedit).  This Berkeley-style licensed command line editor library
provides generic line editing, history, and tokenization functions, similar to
those found in GNU Readline.")
    (license bsd-3)))

(define-public editline
  (package
    (name "editline")
    (version "1.17.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://ftp.troglobit.com/editline/editline-" version ".tar.xz"))
      (sha256
       (base32 "03pw3z6pz590v8xfnjx0yynnzij2sb9xwjbvqvdzsid56crkn8nz"))))
    (build-system gnu-build-system)
    (home-page "https://troglobit.com/projects/editline/")
    (synopsis "Line editing library")
    (description
     "@code{editline} is a small line editing library.  It can be linked into almost
any program to provide command line editing and history functions.  It is call
compatible with the GNU Readline library, but at a fraction of the size,
and as a result fewer features.")
    (license bsd-4)))
