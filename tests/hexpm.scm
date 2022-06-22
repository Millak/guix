;;; GNU Guix --- Functional package management for GNU
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

(define-module (test-hexpm)
  #:use-module (guix import hexpm)
  #:use-module (guix base32)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match))

(define test-bla-package
  "{\"name\": \"bla\",
    \"html_url\": \"https://hex.pm/packages/bla\",
    \"docs_html_url\": null,
    \"meta\": {
        \"description\": \"A cool package\",
        \"licenses\": [\"MIT\", \"Apache-2.0\"]
    },
    \"releases\": [
        {\"url\": \"https://hex.pm/api/packages/bla/releases/1.5.0\",
         \"version\": \"1.5.0\"},
        {\"url\": \"https://hex.pm/api/packages/bla/releases/1.4.7\",
         \"version\": \"1.4.7\"}
    ]
}")

(define test-bla-release
  "{
   \"version\": \"1.5.0\",
   \"url\": \"https://hex.pm/api/packages/bla/releases/1.5.0\",
   \"requirements\": {
     \"blubb\":{\"app\": \"blubb\",
        \"optional\": false,
        \"requirement\": \"~>0.3\"
         },
     \"fasel\":{\"app\": \"fasel\",
        \"optional\": false,
        \"requirement\": \"~>1.0\"
         }
   },
   \"meta\":{ \"build_tools\":[\"mix\", \"make\", \"rebar3\"] }
 }")

(define test-blubb-package
  "{\"name\": \"blubb\",
    \"latest_stable_version\": \"0.3.1\",
    \"latest_version\": \"0.3.1\",
    \"html_url\": \"https://hex.pm/packages/blubb\",
    \"docs_html_url\": null,
    \"meta\": {
        \"description\": \"Another cool package\",
        \"licenses\": [\"MIT\"]
    },
    \"releases\": [
        {\"url\": \"https://hex.pm/api/packages/blubb/releases/0.3.1\",
         \"version\": \"0.3.1\"},
        {\"url\": \"https://hex.pm/api/packages/blubb/releases/0.3.0\",
         \"version\": \"0.3.0\"}
    ]
}")

(define test-blubb-release
  "{
   \"version\": \"0.3.1\",
   \"url\": \"https://hex.pm/api/packages/blubb/releases/0.3.1\",
   \"requirements\": {
     \"fasel\":{\"app\": \"fasel\",
        \"optional\": false,
        \"requirement\": \"~>1.0\"
         }
   },
   \"meta\": { \"build_tools\":[\"mix\"] }
 }")

(define test-fasel-package
  "{\"name\": \"fasel\",
    \"latest_stable_version\": \"1.2.1\",
    \"latest_version\": \"1.2.1\",
    \"html_url\": \"https://hex.pm/packages/fasel\",
    \"docs_html_url\": null,
    \"meta\": {
        \"description\": \"Yet another cool package\",
        \"licenses\": [\"GPL\"]
    },
    \"releases\": [
        {\"url\": \"https://hex.pm/api/packages/fasel/releases/1.2.1\",
         \"version\": \"1.2.1\"}
    ]
}")

(define test-fasel-release
  "{
   \"version\": \"1.2.1\",
   \"url\": \"https://hex.pm/api/packages/fasel/releases/1.2.1\",
   \"requirements\" :{},
   \"meta\":{ \"build_tools\":[\"make\"] }
 }")

(test-begin "hexpm")

(test-assert "hexpm->guix-package"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://hex.pm/api/packages/bla"
              (values (open-input-string test-bla-package)
                      (string-length test-bla-package)))
             ("https://hex.pm/api/packages/bla/releases/1.5.0"
              (values (open-input-string test-bla-release)
                      (string-length test-bla-release)))
             (_ (error "http-fetch got unexpected URL: " url)))))
  (mock ((guix build download) url-fetch
         (lambda* (url file-name
                       #:key
                       (mirrors '()) verify-certificate?)
           (with-output-to-file file-name
             (lambda ()
               (display
                (match url
                  ("https://repo.hex.pm/tarballs/bla-1.5.0.tar"
                   "source")
                  (_ (error "url-fetch got unexpected URL: " url))))))))
    (match (hexpm->guix-package "bla")
      (('package
         ('name "erlang-bla")
         ('version "1.5.0")
         ('source
          ('origin
            ('method 'url-fetch)
            ('uri ('hexpm-uri "bla" 'version))
            ('sha256
             ('base32
              "0zcl4dgcmqwl1g5xb901pd6dz61r1xgmac9mqlwvh022paa6gks1"))))
         ('build-system 'rebar-build-system)
         ('inputs ('list 'erlang-blubb 'erlang-fasel))
         ('synopsis "A cool package")
         ('description "This package provides a cool package")
         ('home-page "https://hex.pm/packages/bla")
         ('license ('list 'license:expat 'license:asl2.0)))
       #t)
      (x
       (pk 'fail x #f))))))

(test-assert "hexpm-recursive-import"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://hex.pm/api/packages/bla"
              (values (open-input-string test-bla-package)
                      (string-length test-bla-package)))
             ("https://hex.pm/api/packages/bla/releases/1.5.0"
              (values (open-input-string test-bla-release)
                      (string-length test-bla-release)))
             ("https://hex.pm/api/packages/blubb"
              (values (open-input-string test-blubb-package)
                      (string-length test-blubb-package)))
             ("https://hex.pm/api/packages/blubb/releases/0.3.1"
              (values (open-input-string test-blubb-release)
                      (string-length test-blubb-release)))
             ("https://hex.pm/api/packages/fasel"
              (values (open-input-string test-fasel-package)
                      (string-length test-fasel-package)))
             ("https://hex.pm/api/packages/fasel/releases/1.2.1"
              (values (open-input-string test-fasel-release)
                      (string-length test-fasel-release)))
             (_ (error "http-fetch got unexpected URL: " url)))))
  (mock ((guix build download) url-fetch
         (lambda* (url file-name
                       #:key
                       (mirrors '()) verify-certificate?)
           (with-output-to-file file-name
             (lambda ()
               (display
                (match url
                  ("https://repo.hex.pm/tarballs/bla-1.5.0.tar"
                   "bla-source")
                  ("https://repo.hex.pm/tarballs/blubb-0.3.1.tar"
                   "blubb-source")
                  ("https://repo.hex.pm/tarballs/fasel-1.2.1.tar"
                   "fasel-source")
                  (_ (error "url-fetch got unexpected URL: " url))))))))
        (match (hexpm-recursive-import "bla")
          ((('package
              ('name "erlang-blubb")
              ('version "0.3.1")
              ('source
               ('origin
                 ('method 'url-fetch)
                 ('uri ('hexpm-uri "blubb" 'version))
                 ('sha256
                  ('base32
                   "17y88b5y8ld7s1c2bcwwwa04pf1cl4402i9zk3inna221ps3ppj2"))))
              ('build-system 'mix-build-system)
              ('inputs ('list 'erlang-fasel))
              ('synopsis "Another cool package")
              ('description "Another cool package")
              ('home-page "https://hex.pm/packages/blubb")
              ('license 'license:expat))
            ('package
              ('name "erlang-fasel")
              ('version "1.2.1")
              ('source
               ('origin
                 ('method 'url-fetch)
                 ('uri ('hexpm-uri "fasel" 'version))
                 ('sha256
                  ('base32
                   "1k6d70mxwqgq78jrbr7yqnw187yki74jnagybi7nacrj4a67qjha"))))
              ('build-system 'gnu-build-system)
              ('synopsis "Yet another cool package")
              ('description "Yet another cool package")
              ('home-page "https://hex.pm/packages/fasel")
              ('license "GPL"))
            ('package
              ('name "erlang-bla")
              ('version "1.5.0")
              ('source
               ('origin
                 ('method 'url-fetch)
                 ('uri ('hexpm-uri "bla" 'version))
                 ('sha256
                  ('base32
                   "0d3gj746c4swbb1m6ycylxb239jsavvdcizag6bfbg2aqccxwij8"))))
              ('build-system 'rebar-build-system)
              ('inputs ('list 'erlang-blubb 'erlang-fasel))
              ('synopsis "A cool package")
              ('description "This package provides a cool package")
              ('home-page "https://hex.pm/packages/bla")
              ('license ('list 'license:expat 'license:asl2.0))))
           #t)
          (x
           (pk 'fail x #f))))))

(test-end "hexpm")
