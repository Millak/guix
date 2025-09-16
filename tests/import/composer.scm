;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (test-composer)
  #:use-module (guix import composer)
  #:use-module (guix base32)
  #:use-module (gcrypt hash)
  #:use-module (guix tests http)
  #:use-module (guix grafts)
  #:use-module (srfi srfi-64)
  #:use-module (web client)
  #:use-module (ice-9 match))

;; Globally disable grafts because they can trigger early builds.
(%graft? #f)

(define test-json
  "{
  \"packages\": {
    \"foo/bar\": {
      \"0.1\": {
        \"name\": \"foo/bar\",
        \"description\": \"description\",
        \"keywords\": [\"testing\"],
        \"homepage\": \"http://example.com\",
        \"version\": \"0.1\",
        \"license\": [\"BSD-3-Clause\"],
        \"source\": {
          \"type\": \"url\",
          \"url\": \"http://example.com/Bar-0.1.tar.gz\"
        },
        \"require\": {},
        \"require-dev\": {\"phpunit/phpunit\": \"1.0.0\"}
      }
    }
  }
}")

(define test-source
  "foobar")

(test-begin "composer")

(test-assert "composer->guix-package"
  ;; Replace network resources with sample data.
  (with-http-server `((200 ,test-json)
                      (200 ,test-source))
    (parameterize ((%composer-base-url (%local-url))
                   (current-http-proxy (%local-url)))
      (match (composer->guix-package "foo/bar")
        (`(package
            (name "php-foo-bar")
            (version "0.1")
            (source (origin
                      (method url-fetch)
                      (uri "http://example.com/Bar-0.1.tar.gz")
                      (sha256
                       (base32
                        ,(? string? hash)))))
            (build-system composer-build-system)
            (native-inputs (list php-phpunit-phpunit))
            (synopsis "")
            (description "description")
            (home-page "http://example.com")
            (license license:bsd-3))
         (string=? (bytevector->nix-base32-string
                    (call-with-input-string test-source port-sha256))
                   hash))
        (x
         (pk 'fail x #f))))))

(test-end "composer")
