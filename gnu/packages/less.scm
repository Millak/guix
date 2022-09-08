;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages less)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages file)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public less
  (package
    (name "less")
    (version "590")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://gnu/less/less-"
                                 version ".tar.gz")
                  (string-append "http://www.greenwoodsoftware.com/less/less-"
                                 version ".tar.gz")))
       (patches (search-patches "less-hurd-path-max.patch"))
       (sha256
        (base32 "044fl3izmsi8n1vqzsqdp65q0qyyn5kmsg4sk7id0mxzx15zbbba"))))
    (build-system gnu-build-system)
    (inputs (list ncurses))
    (home-page "https://www.gnu.org/software/less/")
    (synopsis "Paginator for terminals")
    (description
     "GNU less is a pager, a program that allows you to view large amounts
of text in page-sized chunks.  Unlike traditional pagers, it allows both
backwards and forwards movement through the document.  It also does not have
to read the entire input file before starting, so it starts faster than most
text editors.")
    (license gpl3+))) ; some files are under GPLv2+

(define-public lesspipe
  (package
    (name "lesspipe")
    (version "2.06")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wofr06/lesspipe")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f67f45dznhnw99r4zxrzf1xvspzj2rpg2z8b905ixmzpnv86772"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                      ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key outputs #:allow-other-keys)
              ;; configure is a perl script which the standard configure phase
              ;; fails to execute
              (invoke "./configure"
                      (string-append "--prefix=" (assoc-ref outputs "out")))))
          (add-before 'install 'fix-makefile
            (lambda _
              (substitute* "Makefile"
                (("\\$\\(DESTDIR\\)/etc") "$(DESTDIR)$(PREFIX)/etc"))))
          (add-before 'install 'patch-command-paths
            ;; Depending on the content of the file to be displayed and some
            ;; settings, lesspipe tries to use a large variety of external
            ;; commands, e.g. rpm, dpkg, vimcolor.  We only link the
            ;; essential ones to avoid this package to pull in all these
            ;; dependencies which might never ever be used.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((file (search-input-file inputs "/bin/file"))
                    (tput (search-input-file inputs "/bin/tput")))
                (substitute* "sxw2txt"
                  (("^use warnings;" line)
                   (string-append
                    line "\nuse lib '" #$(this-package-input "perl-archive-zip")
                    "/lib/perl5/site_perl';")))
                (substitute* "lesscomplete"
                  (("file -") (string-append file " -")))
                (substitute* "lesspipe.sh"
                  (("tput colors")
                   (string-append tput " colors"))
                  (("file -")
                   (string-append file " -")))))))))
    (inputs
     (list file
           ncurses  ;; for tput
           perl-archive-zip))
    (native-inputs (list perl))
    (home-page "https://github.com/wofr06/lesspipe")
    (synopsis "Input filter for less")
    (description "To browse files, the excellent viewer @code{less} can be
used.  By setting the environment variable @code{LESSOPEN}, less can be
enhanced by external filters to become more powerful.  The input filter for
less described here is called @code{lesspipe.sh}.  It is able to process a
wide variety of file formats.  It enables users to inspect archives and
display their contents without having to unpack them before.  The filter is
easily extensible for new formats.")
    (license gpl2+)))
