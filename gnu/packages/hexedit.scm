;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018, 2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
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

(define-module (gnu packages hexedit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public hexedit
  (package
    (name "hexedit")
    (version "1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pixel/hexedit")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00l8vazbjzdg09azp6w3nzq4rl7qyh06i65dh621r6zaprp0z23w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         ;; Make F1 open the man page even if man-db is not in the profile.
         (add-after 'unpack 'patch-man-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "interact.c"
               (("\"man\"")
                (string-append "\"" (assoc-ref inputs "man-db") "/bin/man\""))
               (("\"hexedit\"")
                (string-append "\"" (assoc-ref outputs "out")
                               "/share/man/man1/hexedit.1.gz\""))))))))
    (native-inputs
     (list autoconf automake))
    (inputs
     (list man-db ncurses))
    (synopsis "View and edit files or devices in hexadecimal or ASCII")
    (description "hexedit shows a file both in ASCII and in hexadecimal.  The
file can be a device as the file is read a piece at a time.  You can modify
the file and search through it.")
    (home-page "http://rigaux.org/hexedit.html")
    (license license:gpl2+)))

(define-public dhex
  (package
    (name "dhex")
    (version "0.69")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.dettus.net/dhex/dhex_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "06y4lrp29f2fh303ijk1xhspa1d4x4dm6hnyw3dd8szi3k6hnwsj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no tests provided
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ;no configure script
          (replace 'install ;multiple issues with provided 'make install'
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (man1 (string-append #$output
                                         "/share/man/man1"))
                    (man5 (string-append #$output
                                         "/share/man/man5")))
                (install-file "dhex" bin)
                (install-file "dhex.1" man1)
                (install-file "dhexrc.5" man5)
                (install-file "dhex_markers.5" man5)
                (install-file "dhex_searchlog.5" man5)))))))
    (inputs (list ncurses))
    (home-page "https://www.dettus.net/dhex/")
    (synopsis "View, edit, and diff files in hexadecimal")
    (description
     "Dhex is hex editor which includes a diff mode, which can be used to
easily and conveniently compare two binary files.  It is based on Ncurses
and is themeable.")
    (license license:gpl2+)))

(define-public ht
  (package
    (name "ht")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://sourceforge.net/projects/hte/files/ht-source/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0w2xnw3z9ws9qrdpb80q55h6ynhh3aziixcfn45x91bzrbifix9i"))))
    (build-system gnu-build-system)
    (inputs
     (list lzo ncurses))
    (synopsis "Viewer, editor, and analyzer for executable binaries")
    (description
     "ht is a terminal-based program to view, edit, and analyze any file, but
with a special focus on executable binaries.  Its goal is to combine the
low-level functionality of a debugger with the usability of an @dfn{Integrated
Development Environment} (IDE).")
    (home-page "https://hte.sourceforge.net/")
    (license license:gpl2)))

(define-public bvi
  (package
    (name "bvi")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/bvi/bvi/" version
                    "/bvi-" version ".src.tar.gz"))
              (sha256
               (base32
                "1wllrvs8r8bdm11sx01095j8cj4drjmw0dlkjfdkm5lnnk11dfjb"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))          ; no check target
    (inputs
     (list ncurses))
    (synopsis "Binary file editor")
    (description "@command{bvi} is a display-oriented editor for binary files,
based on the @command{vi} text editor.")
    (home-page "https://bvi.sourceforge.net/")
    (license license:gpl3+)))
