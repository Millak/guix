;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2025 aurtzy <aurtzy@gmail.com>
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

(define-module (gnu packages stb)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:select (expat public-domain)))

(define-public stb
  ;; stb is a collection of libraries developed within the same repository.
  ;; When updating this, remember to change versions below as appropriate.
  (let ((commit "2e2bef463a5b53ddf8bb788e25da6b8506314c08")
        (revision "3"))
    (package
      (name "stb")
      (home-page "https://github.com/nothings/stb")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1n3klhrf65ndlzg88z0ligk9z4jdg4p296wnqivd1259834cj8gf"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules `((ice-9 ftw)
                    (ice-9 regex)
                    (srfi srfi-26)
                    ,@%default-gnu-modules)
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (delete 'build)
            (replace 'check
              (lambda _
                #f                     ; (invoke "make" "-C" "tests" "CC=gcc")
                ))
            (replace 'install
              (lambda _
                (let* ((files-rx (make-regexp "\\.(c|h|md)$"))
                       (include-file? (cut regexp-exec files-rx <>))
                       (deprecated-output (string-append #$output "/deprecated")))
                  (for-each (cut install-file <> #$output)
                            (scandir "." include-file?))
                  (mkdir-p deprecated-output)
                  (with-directory-excursion "deprecated"
                    (for-each (cut install-file <> deprecated-output)
                              (scandir "." include-file?)))
                  #t))))))
      (synopsis "Single file libraries for C/C++")
      (description
       "This package contains a variety of small independent libraries for
the C programming language.")
      ;; The user can choose either license.
      (license (list expat public-domain)))))

(define* (make-stb-header-package name version description #:key deprecated?)
  (package
    (inherit stb)
    (name name)
    (version version)
    (source #f)
    (inputs (list stb))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((headers-dir #$(file-append (this-package-input "stb")
                                            (if deprecated? "/deprecated" "")))
                (lib (string-join (string-split #$name #\-) "_"))
                (out #$output))
            (install-file (string-append headers-dir "/" lib ".h")
                          (string-append out "/include"))
            #t))))
    (description description)))

(define-syntax define-stb-header-package
  (syntax-rules (description)
    ((_ symbol name version (description text) rest ...)
     (define-public symbol
       (make-stb-header-package name version text rest ...)))))

(define-stb-header-package stb-image
  "stb-image" "2.30"
  (description
   "stb-image is a small and self-contained library for image loading or
decoding from file or memory.  A variety of formats are supported."))

(define-stb-header-package stb-image-resize
  "stb-image-resize" "0.97"
  (description
   "stb-image-resize is a library that supports scaling and translation of
images.  This library is deprecated; @code{stb-image-resize2} should be used
instead.")
  #:deprecated? #t)

(define-stb-header-package stb-image-resize2
  "stb-image-resize2" "2.12"
  (description
   "stb-image-resize2 is a library that supports scaling and translation of
images."))

(define-stb-header-package stb-image-write
  "stb-image-write" "1.16"
  (description
   "stb-image-write is a small library for writing image files to the
C@tie{}@code{stdio} interface."))

(define-stb-header-package stb-rect-pack
  "stb-rect-pack" "1.01"
  (description
   "stb-rect-pack is a small rectangle packing library useful for, e.g., packing
rectangular textures into an atlas.  It does not do rotation."))

(define-stb-header-package stb-sprintf
  "stb-sprintf" "1.10"
  (description
   "stb-sprintf implements fast @code{sprintf}, @code{snprintf} for C/C++."))

(define-stb-header-package stb-truetype
  "stb-truetype" "1.26"
  (description
   "stb-truetype is a library for parsing, decoding, and rasterizing
characters from TrueType fonts."))
