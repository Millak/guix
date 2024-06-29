;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Jorge Acereda <jacereda@gmail.com>
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


(define-module (gnu packages decker)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages vim)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public decker
  (package
    (name "decker")
    (version "1.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JohnEarnest/Decker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hwi62g5wg7zxj98j3civz42za133jq3mz8c0mapqnkn0xnb0qa8"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags #~(list (string-append "PREFIX=" #$output)
                           (string-append "COMPILER=" #$(cc-for-target)))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)  ;no configure script
                   (replace 'build
                     (lambda* (#:key parallel-build? #:allow-other-keys)
                       (let ((job-count (if parallel-build?
                                            (parallel-job-count)
                                            1)))
                         (invoke "make" "lilt" "decker"
                                 "-j" (number->string job-count))))))))
    (native-inputs (list xxd))
    (inputs (list (sdl-union (list sdl2-image sdl2))))
    (home-page "http://beyondloom.com/decker/")
    (synopsis "Multimedia sketchpad")
    (description
     "Decker is a multimedia platform for creating and sharing interactive
documents, with sound, images, hypertext, and scripted behavior.")
    (license license:expat)))
