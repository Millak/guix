;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2024 Li-cheng (Andy) Tai <atai@atai.org>
;;
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

(define-module (gnu packages squirrel)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages sphinx)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public squirrel
  (package
    (name "squirrel")
    (version "3.2")
    (source (origin
              (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/albertodemichelis/squirrel.git")
                     (commit (string-append "v" version))))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "028v90k5bbcb8qwysgv6r0ycy6g920ns32i2sdq0i8hqib90ac5z"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DDISABLE_STATIC=ON")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((source (assoc-ref %build-inputs "source"))
                    (out (assoc-ref outputs "out"))
                    (doc-dir (string-append out "/share/doc/squirrel")))
               (for-each
                (lambda (file)
                  (install-file (string-append source "/" file) doc-dir))
                '("COPYRIGHT" "HISTORY" "README"
                  ;"doc/sqstdlib3.pdf" "doc/squirrel3.pdf"  ;; pdf not build out of git; TODO

                  )))
             #t)))))
    (native-inputs
     `(("cmake" ,cmake-minimal)
       ("python-sphinx" ,python-sphinx)))
    (home-page "https://squirrel-lang.org/")
    (synopsis "High level imperative, object-oriented programming language")
    (description
     "Squirrel is a high level imperative, object-oriented programming
language, designed to be a light-weight scripting language that fits in the
size, memory bandwidth, and real-time requirements of applications like video
games.")
    (license license:expat)))
