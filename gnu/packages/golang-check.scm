;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
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

(define-module (gnu packages golang-check)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang))

;;; Commentary:
;;;
;;; Golang packages to unit-test, mock, assert, lint processes for Golang itself.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public go-github-com-jacobsa-oglematchers
  (let ((commit "141901ea67cd4769c6800aa7bfdfc558fa22bda5")
        (revision "0"))
    (package
      (name "go-github-com-jacobsa-oglematchers")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jacobsa/oglematchers")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09ff5x6vbhd9zl1z4yzyk573ifh16rry38q1rx986kbz4hqkmniq"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/jacobsa/oglematchers"
         ;; break loop with with go-github-com-jacobsa-ogletest
         #:tests? #f))
      (home-page "https://github.com/jacobsa/oglematchers")
      (synopsis "Matchers for Go testing framework")
      (description
       "Package oglematchers provides a set of matchers useful in a testing or mocking
framework.  These matchers are inspired by and mostly compatible with Google
Test for C++ and Google JS Test.")
      (license license:asl2.0))))

(define-public go-github-com-jacobsa-oglemock
  (let ((commit "e94d794d06ffc6de42cb19d0dab3c219efdd6dcf")
        (revision "0"))
    (package
      (name "go-github-com-jacobsa-oglemock")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jacobsa/oglemock")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14yxf8ykwdwkcccksl6741xgzcf8qykyi58kp4maxpgscqhdl8rq"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/jacobsa/oglemock"
        ;; break loop with with go-github-com-jacobsa-ogletest
        #:tests? #f))
      (native-inputs
       (list go-github-com-jacobsa-oglematchers))
      (home-page "https://github.com/jacobsa/oglemock")
      (synopsis "Mocking framework for unit tests")
      (description
       "Package oglemock provides a mocking framework for unit tests.")
      (license license:asl2.0))))

(define-public go-github-com-stretchr-testify
  (package
    (name "go-github-com-stretchr-testify")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stretchr/testify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ixgjsvafr3513pz3r6pmgk074s2dxkll0dadvl25gkf30rkmh10"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/stretchr/testify"))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-pmezard-go-difflib
           go-github-com-stretchr-objx
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/stretchr/testify")
    (synopsis "Go helper library for tests and invariant checking")
    (description "This package provide many tools for testifying that your
code will behave as you intend.

Features include:
@itemize
@item Easy assertions
@item Mocking
@item HTTP response trapping
@item Testing suite interfaces and functions.
@end itemize")
    (license license:expat)))

(define-public go-gopkg-in-check-v1
  (package
    (name "go-gopkg-in-check-v1")
    (version "1.0.0-20201130134442-10cb98267c6c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-check/check")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jwxndf8rsyx0fgrp47d99rp55yzssmryb92jfj3yf7zd8rjjljn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/check.v1"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (unless
                  ;; The tests fail when run with gccgo.
                  (false-if-exception (search-input-file inputs "/bin/gccgo"))
                (apply (assoc-ref %standard-phases 'check) args)))))))
    (propagated-inputs
     (list go-github-com-kr-pretty))
    (home-page "https://gopkg.in/check.v1")
    (synopsis "Test framework for the Go language")
    (description "This package provides a test library for the Go language.")
    (license license:bsd-2)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
