;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages noweb)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages perl))

(define-public noweb
  (package
    (name "noweb")
    (version "2.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nrnrnr/noweb")
             (commit (string-append "v" (string-join (string-split version #\.)
                                                     "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fwngh7zl9mrjz966pskhi4zvk26j6vsm85x99df9194nv51drq8"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "BIN=" #$output "/bin")
              (string-append "LIB=" #$output "/lib")
              (string-append "MAN=" #$output "/share/man")
              (string-append "TEXINPUTS=" #$output
                             "/share/texmf/tex/latex"))
      #:modules
      '((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-source-directory
            (lambda _
              (chdir "src")))
          (add-after 'enter-source-directory 'bind-early
            (lambda _
              (substitute* (list "lib/nwmtime"
                                 "shell/htmltoc")
                (("exec perl ")
                 (string-append "exec " (which "perl") " ")))
              (substitute* "shell/noweb"
                ((" cpif ")
                 (string-append " " #$output "/bin/cpif ")))))
          (delete 'configure)           ; no configure script
          (add-before 'install 'create-latex-directory
            (lambda _
              (mkdir-p (string-append #$output "/share/texmf/tex/latex"))))
          (add-after 'install 'refer-to-inputs
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion #$output
                (for-each (lambda (program)
                            (substitute* program
                              (("nawk")
                               (search-input-file inputs "bin/awk"))))
                          (append (map (cut string-append "bin/" <>)
                                       '("noweb" "nountangle"
                                         "noroots" "noroff"
                                         "noindex"))
                                  (map (cut string-append "lib/" <>)
                                       '("btdefn" "emptydefn" "noidx"
                                         "pipedocs" "toascii" "tohtml"
                                         "toroff" "totex" "unmarkup"))))
                (substitute* "bin/cpif"
                  (("^PATH=.*$")
                   (string-append "PATH="
                                  (dirname (search-input-file
                                            inputs"bin/basename")) ":"
                                  (dirname (search-input-file
                                            inputs "bin/cmp"))
                                  "\n")))))))
      #:tests? #f))                              ; no tests
    (inputs
     (list perl))
    (home-page "https://www.cs.tufts.edu/~nr/noweb/")
    (synopsis "Literate programming tool")
    (description
     "Noweb is designed to meet the needs of literate programmers while
remaining as simple as possible.  Its primary advantages are simplicity,
extensibility, and language-independence—especially noticeable when compared
with other literate-programming tools.  noweb uses 5 control sequences to
WEB's 27.  The noweb manual is only 4 pages; an additional page explains how
to customize its LaTeX output.  noweb works “out of the box” with any
programming language, and supports TeX, LaTeX, HTML, and troff back ends.")
    (license
     (list bsd-2                        ; dual-licenced under this and…
           (fsf-free "https://www.cs.tufts.edu/~nr/noweb/#copyright")))))
