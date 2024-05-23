;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020, 2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages entr)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages ncurses)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

(define-public entr
  (package
    (name "entr")
    (version "5.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://eradman.com/entrproject/code/entr-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "09p5aqbb95bysdx73n094v0b07hn3v9kqg6k7yyycnpaxzi0r30j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (setenv "CC" ,(cc-for-target))
               (setenv "PREFIX" out)
               (setenv "MANPREFIX" (string-append out "/man"))
               (invoke "./configure"))))
         (add-before 'build 'remove-fhs-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "entr.c"
               (("/bin/sh" command)
                (search-input-file inputs command))
               (("/bin/cat" command)
                (search-input-file inputs command))
               (("/usr(/bin/clear)" _ command)
                (search-input-file inputs command))))))))
    (inputs
     (list bash coreutils ncurses))
    (home-page "https://eradman.com/entrproject/")
    (synopsis "Run arbitrary commands when files change")
    (description
     "@acronym{entr, event notify test runner} reads a list of file names from
standard input and executes a given command (including arguments) whenever any
of them change.  It aims to facilitate rapid feedback and automated testing.

For example, you can use @command{entr} to automatically invoke @command{make}
each time you modify a source code file.  It supports interactive utilities and
doesn't waste resources by polling for changes.")

    ;; Per 'LICENSE', portability code under missing/ is under BSD-2.
    (license isc)))
