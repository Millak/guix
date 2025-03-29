;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 宋文武 <iyzsong@envs.net>
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

(define-module (gnu packages fsf)
  #:use-module (gnu packages tex)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public book-faif
  (package
    (name "book-faif")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.fsf.org/faif/faif-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "0qf14d0n6k1dn9z0fdnx9qkhn4iq685xd443w7l7w54bm931p7dw"))))
    (build-system copy-build-system)
    (native-inputs
     (list (texlive-local-tree
            (list texlive-caption
                  texlive-endnotes
                  texlive-etoolbox
                  texlive-fncychap
                  texlive-helvetic
                  texlive-times
                  texlive-ucs))))
    (arguments
     (list
      #:install-plan #~'(("faif-2.0.pdf" "share/doc/faif/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'build
            (lambda _
              (invoke "pdflatex" "faif-2.0.tex"))))))
    (home-page "https://www.fsf.org/faif")
    (synopsis "Free as in Freedom (2.0)")
    (description
     "In 2002, Sam Williams wrote Free as in Freedom, a biography of Richard
M. Stallman.  In its epilogue, Williams expressed hope that choosing to
distribute his book under the GNU Free Documentation License would enable and
encourage others to share corrections and their own perspectives through
modifications to his work.  Free as in Freedom (2.0) is Stallman's revision of
the original biography.  While preserving Williams's viewpoint, it includes
factual corrections and extensive new commentary by Stallman, as well as new
prefaces by both authors written for the occasion.  It is a rare kind of
biography, where the reader has the benefit of both the biographer's original
words and the subject's response.")
    (license license:fdl1.3+)))
