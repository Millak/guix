;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Denis Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2025 Zheng Junjie <z572@z572.online>
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

(define-module (gnu packages valgrind)
  #:use-module (srfi srfi-1)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages perl))

(define-public valgrind/pinned
  (package
    (name "valgrind")
    (version "3.25.1")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://sourceware.org/pub/valgrind"
                                        "/valgrind-" version ".tar.bz2")
                         (string-append "ftp://sourceware.org/pub/valgrind"
                                        "/valgrind-" version ".tar.bz2")))
              (sha256
               (base32
                "1bvilavwimza3n6v5r2ypxydk737krn3n6ywxxlc4ibvfb8bipk1"))))
    (build-system gnu-build-system)
    (outputs '("doc"                              ;16 MB
               "out"))
    (arguments
     (append
      (if (string-prefix? "powerpc" (or (%current-target-system)
                                        (%current-system)))
          (list #:make-flags #~(list "CFLAGS+=-maltivec"))
          '())
      (list #:phases
            #~(modify-phases %standard-phases
                (add-after 'install 'patch-suppression-files
                  (lambda _
                    ;; Don't assume the FHS.
                    (let* ((dir (string-append #$output "/lib/valgrind")))
                      (substitute* (find-files dir "\\.supp$")
                        (("obj:/lib") "obj:*/lib")
                        (("obj:/usr/X11R6/lib") "obj:*/lib")
                        (("obj:/usr/lib") "obj:*/lib")))))
                (add-after 'install 'install-doc
                  (lambda _
                    (let ((orig (string-append #$output "/share/doc"))
                          (dest (string-append #$output:doc "/share")))
                      (mkdir-p dest)
                      (rename-file orig dest))))))))
    (native-inputs
     (list perl))
    (home-page "https://www.valgrind.org/")
    (synopsis "Debugging and profiling tool suite")
    (description
     "Valgrind is an instrumentation framework for building dynamic analysis
tools.  There are Valgrind tools that can automatically detect many memory
management and threading bugs, and profile your programs in detail.  You can
also use Valgrind to build new tools.")
    ;; https://valgrind.org/info/platforms.html
    (supported-systems (fold delete %supported-systems
                             '("i586-gnu" "x86_64-gnu" "armhf-linux")))
    (license gpl2+)

    ;; Hide this variant so end users get the "interactive" Valgrind below.
    (properties '((hidden? . #t)))))

(define-public valgrind
  (package
    (inherit valgrind/pinned)
    (version "3.26.0")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://sourceware.org/pub/valgrind"
                                        "/valgrind-" version ".tar.bz2")
                         (string-append "ftp://sourceware.org/pub/valgrind"
                                        "/valgrind-" version ".tar.bz2")))
              (sha256
               (base32
                "1mvbc6x0w6blkv8wn5fh7pcm6bk9p4m81bxd99jg21li08bwfm4d"))))))

(define-public valgrind/interactive
  (package/inherit valgrind
    (inputs
     ;; GDB is needed to provide a sane default for `--db-command'.
     (list gdb `(,(canonical-package (libc-for-target)) "debug")))
    (properties '())))
