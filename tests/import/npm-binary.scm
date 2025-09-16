;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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
(define-module (test-npm-binary)
  #:use-module ((gcrypt hash)
                #:select ((sha256 . gcrypt-sha256)))
  #:use-module (guix import npm-binary)
  #:use-module (guix base32)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:export (run-test))

(define* (foo-json #:key (license "MIT"))
  "Create a JSON description of an example foo npm package, optionally using a
different @var{license}."
  (scm->json-string
   `((name . "foo")
     (dist-tags . ((latest . "1.2.3")
                   (next . "2.0.1-beta4")))
     (description . "General purpose utilities to foo your bars")
     (homepage . "https://github.com/quartz/foo")
     (repository . "quartz/foo")
     (versions
      . ((1.2.3
          . ((name . "foo")
             (description . "General purpose utilities to foo your bars")
             (version . "1.2.3")
             (author . "Jelle Licht <jlicht@fsfe.org>")
             (devDependencies . ((node-megabuilder . "^0.0.2")))
             (dependencies . ((bar . "^0.1.0")))
             (repository . ((url . "quartz/foo")))
             (homepage . "https://github.com/quartz/foo")
             (license . ,license)
             (dist
              . ((tarball
                  . "https://registry.npmjs.org/foo/-/foo-1.2.3.tgz"))))))))))

;; Dependency JSON for the bar package
(define bar-json
  "{
  \"name\": \"bar\",
  \"dist-tags\": {
    \"latest\": \"0.1.2\"
  },
  \"description\": \"Core module in FooBar\",
  \"homepage\": \"https://github.com/quartz/bar\",
  \"repository\": \"quartz/bar\",
  \"versions\": {
    \"0.1.2\": {
      \"name\": \"bar\",
      \"description\": \"Core module in FooBar\",
      \"version\": \"0.1.2\",
      \"author\": \"Jelle Licht <jlicht@fsfe.org>\",
      \"repository\": {
        \"url\": \"quartz/bar\"
      },
      \"homepage\": \"https://github.com/quartz/bar\",
      \"license\": \"MIT\",
      \"dist\": {
        \"tarball\": \"https://registry.npmjs.org/bar/-/bar-0.1.2.tgz\"
      }
    }
  }
}")

(define test-source
  "Empty file\n")

(define test-source-hash
  (bytevector->nix-base32-string
   (gcrypt-sha256 (string->bytevector test-source "utf-8"))))

(define have-guile-semver?
  (false-if-exception (resolve-interface '(semver))))

(define* (foo-sexp #:key (license 'license:expat))
  `(package
     (name "node-foo")
     (version "1.2.3")
     (source (origin
               (method url-fetch)
               (uri "https://registry.npmjs.org/foo/-/foo-1.2.3.tgz")
               (sha256
                (base32 "1n0h7zg9zzv4f7yn2gp0mq1v107im7pi6qq4k6q86rixz71ijklh"))))
     (build-system node-build-system)
     (arguments
      (list #:tests? #f
            #:phases
            (gexp (modify-phases %standard-phases
                    (delete 'build)
                    (add-after 'patch-dependencies 'delete-dev-dependencies
                      (lambda _
                        (modify-json
                         (delete-dependencies '("node-megabuilder")))))))))
     (inputs (list node-bar-0.1.2))
     (home-page "https://github.com/quartz/foo")
     (synopsis "General purpose utilities to foo your bars")
     (description "General purpose utilities to foo your bars")
     (license ,license)))

(test-begin "npm")

(unless have-guile-semver? (test-skip 1))
(test-assert "npm-binary->guix-package base case"
  (mock ((guix http-client) http-fetch
         (lambda* (url #:rest _)
           (match url
             ("https://registry.npmjs.org/foo"
              (let ((json-foo (foo-json)))
                (values (open-input-string json-foo)
                        (string-length json-foo))))
             ("https://registry.npmjs.org/bar"
              (values (open-input-string bar-json)
                      (string-length bar-json)))
             ("https://registry.npmjs.org/foo/-/foo-1.2.3.tgz"
              (values (open-input-string test-source)
                      (string-length test-source))))))
        (let ((sexp-foo (foo-sexp)))
          (match (npm-binary->guix-package "foo")
            (sexp-foo
             #t)
            (x
             (pk 'fail x #f))))))

(test-assert "npm-binary->guix-package with multiple licenses"
  (mock ((guix http-client) http-fetch
         (lambda* (url #:rest _)
           (match url
             ("https://registry.npmjs.org/foo"
              (let ((json-foo (foo-json #:license #("MIT" "Apache2.0"))))
                (values (open-input-string json-foo)
                        (string-length json-foo))))
             ("https://registry.npmjs.org/bar"
              (values (open-input-string bar-json)
                      (string-length bar-json)))
             ("https://registry.npmjs.org/foo/-/foo-1.2.3.tgz"
              (values (open-input-string test-source)
                      (string-length test-source))))))
        (let ((sexp-foo (foo-sexp
                         #:license '(list license:expat license:asl2.0))))
          (match (npm-binary->guix-package "foo")
            (sexp-foo
             #t)
            (x
             (pk 'fail x #f))))))

(test-end "npm")
