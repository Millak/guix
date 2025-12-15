;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Marcel Steinbeck <msteinbeck@posteo.net>
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

(define-module (gnu packages fusion)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages java)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages node)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public fusion
  (package
    (name "fusion")
    (version "3.2.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/fusionlanguage/fut")
              (commit (string-append "fut-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "06qn098nbspz3srhb4n3fycsk75f2xala25gpyjryby3f5ijb9md"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "prefix=" #$output))
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'remove-unsupported-tests
            (lambda _
              (substitute* "Makefile"
                ;; The following targets cannot be executed in the Guix build
                ;; environment:
                ;;
                ;;   test-cs - needs `dotnet'
                ;;   test-ts - tries to download packages
                ;;   test-swift - needs Swift
                ;;
                ;; We therefore remove them from the `test' target.
                (("^(test:.*)test-cs (.*)test-ts (.*)test-swift (.*)$"
                  _ prefix a b suffix)
                 (string-append prefix a b suffix)))))
          (add-after 'install 'install-doc
            (lambda _
              (let ((doc (string-append #$output:doc "/share/doc/fusion/")))
                (mkdir-p doc)
                (copy-recursively "doc" doc)))))))
    (native-inputs
     (list clang             ;; test-cl
           dmd               ;; test-d
           glib              ;; test-c
           icu4c             ;; test-cpp
           node              ;; test-js
           `(,openjdk "jdk") ;; test-java
           perl              ;; for evaluating test results
           pkg-config        ;; test-c
           python            ;; test-py
           python-mypy))     ;; test-py
    (home-page "https://fusion-lang.org")
    (synopsis "The Fusion programming language transpiler (fut)")
    (description "Fusion is a programming language designed for implementing reusable
components (libraries) for C, C++, C#, D, Java, JavaScript, Python, Swift,
TypeScript and OpenCL C, all from single codebase.")
    (license license:gpl3+)))
