;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2019, 2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2023, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2025 Herman Rimm <herman@rimm.ee>
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

(define-module (test-crate)
  #:use-module (guix import crate)
  #:use-module (guix base32)
  #:use-module (guix build-system cargo)
  #:use-module ((gcrypt hash)
                #:select ((sha256 . gcrypt-sha256)))
  #:use-module (guix packages)
  #:use-module (guix read-print)
  #:use-module (guix tests)
  #:use-module (gnu packages)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64))


;; crate versions and dependencies used here
;; foo-0.8.1
;; foo-1.0.0
;; foo-1.0.3
;; 	leaf-alice 0.7.5
;; bar-1.0.0
;;      leaf-bob   3.0.1
;;      leaf-bob   3.0.2 (dev-dependency)
;;      leaf-bob   4.0.0 (dev-dependency)
;;
;; root-1.0.0
;; root-1.0.4
;; 	intermediate-a 1.0.42
;; 	intermediate-b ^1.0.0
;; 	leaf-alice     ^0.7
;; 	leaf-bob       ^3
;; 	leaf-bob       3 (build-dependency)
;; 	intermediate-c 1 (dev-dependency)
;;
;; intermediate-a-1.0.40
;; intermediate-a-1.0.42
;; intermediate-a-1.1.0-alpha.1
;; 	intermediate-a	1.2.3
;; 	leaf-alice	0.7.5
;; 	leaf-bob	^3
;;
;; intermediate-b-1.2.3
;; 	leaf-bob	3.0.1
;;
;; intermediate-c-1.0.1
;;      leaf-alice      0.7.5 (dev-dependency)
;;
;; leaf-alice-0.7.3
;; leaf-alice-0.7.5
;;
;; leaf-bob-3.0.1
;; leaf-bob-3.0.2 (yanked)
;; leaf-bob-4.0.0 (yanked)


(define test-foo-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.3\",
    \"name\": \"foo\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\", \"test\"],
    \"categories\": [\"test\"],
    \"actual_versions\": [
      { \"id\": 234210,
        \"num\": \"0.8.1\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/foo/0.8.1/dependencies\"
        },
        \"yanked\": false
      },
      { \"id\": 234212,
        \"num\": \"1.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/foo/1.0.0/dependencies\"
        },
        \"yanked\": false
      },
      { \"id\": 234214,
        \"num\": \"1.0.3\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/foo/1.0.3/dependencies\"
        },
        \"yanked\": false
      }
    ]
  }
}")

(define test-foo-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"leaf-alice\",
       \"kind\": \"normal\",
       \"req\": \"0.7.5\"
     }
  ]
}")

(define test-bar-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.0\",
    \"name\": \"bar\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\", \"test\"],
    \"categories\": [\"test\"],
    \"actual_versions\": [
      { \"id\": 234100,
        \"num\": \"1.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/bar/1.0.0/dependencies\"
        },
        \"yanked\": false
      }
    ]
  }
}")

(define test-bar-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"normal\",
       \"req\": \"3.0.1\"
     },
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"dev\",
       \"req\": \"^3.0.2\"
     },
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"dev\",
       \"req\": \"^4.0.0\"
     }
  ]
}")

(define test-root-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.4\",
    \"name\": \"root\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\", \"test\"],
    \"categories\": [\"test\"],
    \"actual_versions\": [
      { \"id\": 234240,
        \"num\": \"1.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/root/1.0.0/dependencies\"
        },
        \"yanked\": false
      },
      { \"id\": 234242,
        \"num\": \"1.0.4\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/root/1.0.4/dependencies\"
        },
        \"yanked\": false
      }
    ]
  }
}")

(define test-root-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"intermediate-a\",
       \"kind\": \"normal\",
       \"req\": \"1.0.42\"
     },
     {
       \"crate_id\": \"intermediate-b\",
       \"kind\": \"normal\",
       \"req\": \"^1.0.0\"
     },
     {
       \"crate_id\": \"leaf-alice\",
       \"kind\": \"normal\",
       \"req\": \"^0.7\"
     },
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"normal\",
       \"req\": \"^3\"
     },
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"build\",
       \"req\": \"3\"
     },
     {
       \"crate_id\": \"intermediate-c\",
       \"kind\": \"dev\",
       \"req\": \"1\"
     }
  ]
}")

(define test-intermediate-a-crate
  "{
  \"crate\": {
    \"max_version\": \"1.1.0-alpha.1\",
    \"name\": \"intermediate-a\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\", \"test\"],
    \"categories\": [\"test\"],
    \"actual_versions\": [
      { \"id\": 234251,
        \"num\": \"1.0.40\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/intermediate-a/1.0.40/dependencies\"
        },
        \"yanked\": false
      },
      { \"id\": 234250,
        \"num\": \"1.0.42\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/intermediate-a/1.0.42/dependencies\"
        },
        \"yanked\": false
      },
      { \"id\": 234252,
        \"num\": \"1.1.0-alpha.1\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/intermediate-a/1.1.0-alpha.1/dependencies\"
        },
        \"yanked\": false
      }
    ]
  }
}")

(define test-intermediate-a-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"intermediate-b\",
       \"kind\": \"normal\",
       \"req\": \"1.2.3\"
     },
     {
       \"crate_id\": \"leaf-alice\",
       \"kind\": \"normal\",
       \"req\": \"0.7.5\"
     },
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"normal\",
       \"req\": \"^3\"
     }
  ]
}")

(define test-intermediate-b-crate
  "{
  \"crate\": {
    \"max_version\": \"1.2.3\",
    \"name\": \"intermediate-b\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\", \"test\"],
    \"categories\": [\"test\"],
    \"actual_versions\": [
      { \"id\": 234260,
        \"num\": \"1.2.3\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/intermediate-b/1.2.3/dependencies\"
        },
        \"yanked\": false
      }
    ]
  }
}")

(define test-intermediate-b-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"normal\",
       \"req\": \"3.0.1\"
     }
  ]
}")

(define test-intermediate-c-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.1\",
    \"name\": \"intermediate-c\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\", \"test\"],
    \"categories\": [\"test\"],
    \"actual_versions\": [
      { \"id\": 234290,
        \"num\": \"1.0.1\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/intermediate-c/1.0.1/dependencies\"
        },
        \"yanked\": false
      }
    ]
  }
}")

(define test-intermediate-c-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"leaf-alice\",
       \"kind\": \"dev\",
       \"req\": \"0.7.5\"
     }
  ]
}")

(define test-leaf-alice-crate
  "{
  \"crate\": {
    \"max_version\": \"0.7.5\",
    \"name\": \"leaf-alice\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\", \"test\"],
    \"categories\": [\"test\"],
    \"actual_versions\": [
      { \"id\": 234270,
        \"num\": \"0.7.3\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/leaf-alice/0.7.3/dependencies\"
        },
        \"yanked\": false
      },
      { \"id\": 234272,
        \"num\": \"0.7.5\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/leaf-alice/0.7.5/dependencies\"
        },
        \"yanked\": false
      }
    ]
  }
}")

(define test-leaf-alice-dependencies
  "{
  \"dependencies\": []
}")

(define test-leaf-bob-crate
  "{
  \"crate\": {
    \"max_version\": \"3.0.1\",
    \"name\": \"leaf-bob\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\", \"test\"],
    \"categories\": [\"test\"]
    \"actual_versions\": [
      { \"id\": 234280,
        \"num\": \"3.0.1\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/leaf-bob/3.0.1/dependencies\"
        },
        \"yanked\": false
      },
      { \"id\": 234281,
        \"num\": \"3.0.2\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/leaf-bob/3.0.2/dependencies\"
        },
        \"yanked\": true
      },
      { \"id\": 234282,
        \"num\": \"4.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/leaf-bob/4.0.0/dependencies\"
        },
        \"yanked\": true
      }
    ]
  }
}")

(define test-leaf-bob-dependencies
  "{
  \"dependencies\": []
}")


(define test-source-hash
  "")

(define have-guile-semver?
  (false-if-exception (resolve-interface '(semver))))

(define rust-leaf-bob-3
  (package
    (name "rust-leaf-bob")
    (version "3.0.1")
    (source #f)
    (build-system #f)
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define rust-leaf-bob-3.0.2-yanked
  (package
    (name "rust-leaf-bob")
    (version "3.0.2")
    (source #f)
    (properties '((crate-version-yanked? . #t)))
    (build-system #f)
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define temp-file
  (string-append "t-crate-" (number->string (getpid))))


(test-begin "crate")

(test-equal "guix-package->crate-name"
  "rustc-serialize"
  (guix-package->crate-name
   (dummy-package
    "rust-rustc-serialize"
    (source (dummy-origin
             (uri (crate-uri "rustc-serialize" "1.0")))))))

(unless have-guile-semver? (test-skip 1))
(test-assert "crate->guix-package"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://crates.io/api/v1/crates/foo"
              (open-input-string test-foo-crate))
             ("https://crates.io/api/v1/crates/foo/1.0.3/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/foo/1.0.3/dependencies"
              (open-input-string test-foo-dependencies))
             ("https://crates.io/api/v1/crates/leaf-alice"
              (open-input-string test-leaf-alice-crate))
             ("https://crates.io/api/v1/crates/leaf-alice/0.7.5/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/leaf-alice/0.7.5/dependencies"
              (open-input-string test-leaf-alice-dependencies))
             (_ (error "Unexpected URL: " url)))))

        (match (crate->guix-package "foo")
          ((define-public 'rust-foo-1
             (package (name "rust-foo")
                      (version "1.0.3")
                      (source
                       (origin
                         (method url-fetch)
                         (uri (crate-uri "foo" 'version))
                         (file-name (string-append name "-" version ".tar.gz"))
                         (sha256
                          (base32
                           (?  string? hash)))))
                      (build-system 'cargo-build-system)
                      (arguments
                       ('quasiquote
                        (#:skip-build? #t
                         #:cargo-inputs
                         (("rust-leaf-alice" ('unquote 'rust-leaf-alice-0.7))))))
                      (home-page "http://example.com")
                      (synopsis "summary")
                      (description "This package provides summary.")
                      (license (list license:expat license:asl2.0))))

           (string=? test-source-hash hash))
          (x
           (pk 'fail x #f)))))

(unless have-guile-semver? (test-skip 1))
(test-assert "crate->guix-package-marks-missing-packages"
  (mock
   ((gnu packages) find-packages-by-name
    (lambda* (name #:optional version)
      (match name
        ("rust-leaf-bob"
         (list rust-leaf-bob-3.0.2-yanked))
        (_ '()))))
   (mock
    ((guix http-client) http-fetch
     (lambda (url . rest)
       (match url
         ("https://crates.io/api/v1/crates/intermediate-b"
          (open-input-string test-intermediate-b-crate))
         ("https://crates.io/api/v1/crates/intermediate-b/1.2.3/download"
          (set! test-source-hash
                (bytevector->nix-base32-string
                 (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
          (open-input-string "empty file\n"))
         ("https://crates.io/api/v1/crates/intermediate-b/1.2.3/dependencies"
          (open-input-string test-intermediate-b-dependencies))
         ("https://crates.io/api/v1/crates/leaf-bob"
          (open-input-string test-leaf-bob-crate))
         ("https://crates.io/api/v1/crates/leaf-bob/3.0.1/download"
          (set! test-source-hash
                (bytevector->nix-base32-string
                 (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
          (open-input-string "empty file\n"))
         (_ (error "Unexpected URL: " url)))))
    (match (crate->guix-package "intermediate-b" #:mark-missing? #t)
      ((define-public 'rust-intermediate-b-1
         (package
           (name "rust-intermediate-b")
           (version "1.2.3")
           (source
            (origin
              (method url-fetch)
              (uri (crate-uri "intermediate-b" version))
              (file-name
               (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                (?  string? hash)))))
           (build-system cargo-build-system)
           (arguments
            ('quasiquote
             (#:skip-build? #t
              #:cargo-inputs
              (($ <comment> ";; rust-leaf-bob-3\n" #f)))))
           (home-page "http://example.com")
           (synopsis "summary")
           (description "This package provides summary.")
           (license (list license:expat license:asl2.0))))
       #t)
      (x
       (pk 'fail
           (pretty-print-with-comments (current-output-port) x)
           #f))))))

(unless have-guile-semver? (test-skip 1))
(test-assert "crate-recursive-import"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://crates.io/api/v1/crates/root"
              (open-input-string test-root-crate))
             ("https://crates.io/api/v1/crates/root/1.0.4/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/root/1.0.4/dependencies"
              (open-input-string test-root-dependencies))
             ("https://crates.io/api/v1/crates/intermediate-a"
              (open-input-string test-intermediate-a-crate))
             ("https://crates.io/api/v1/crates/intermediate-a/1.0.42/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/intermediate-a/1.0.42/dependencies"
              (open-input-string test-intermediate-a-dependencies))
             ("https://crates.io/api/v1/crates/intermediate-b"
              (open-input-string test-intermediate-b-crate))
             ("https://crates.io/api/v1/crates/intermediate-b/1.2.3/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/intermediate-b/1.2.3/dependencies"
              (open-input-string test-intermediate-b-dependencies))
             ("https://crates.io/api/v1/crates/intermediate-c"
              (open-input-string test-intermediate-c-crate))
             ("https://crates.io/api/v1/crates/intermediate-c/1.0.1/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/intermediate-c/1.0.1/dependencies"
              (open-input-string test-intermediate-c-dependencies))
             ("https://crates.io/api/v1/crates/leaf-alice"
              (open-input-string test-leaf-alice-crate))
             ("https://crates.io/api/v1/crates/leaf-alice/0.7.5/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/leaf-alice/0.7.5/dependencies"
              (open-input-string test-leaf-alice-dependencies))
             ("https://crates.io/api/v1/crates/leaf-bob"
              (open-input-string test-leaf-bob-crate))
             ("https://crates.io/api/v1/crates/leaf-bob/3.0.1/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/leaf-bob/3.0.1/dependencies"
              (open-input-string test-leaf-bob-dependencies))
             (_ (error "Unexpected URL: " url)))))
        (match (crate-recursive-import "root")
          ;; rust-intermediate-b has no dependency on the rust-leaf-alice
          ;; package, so this is a valid ordering
          (((define-public 'rust-intermediate-c-1
              (package
                (name "rust-intermediate-c")
                (version "1.0.1")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "intermediate-c" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments
                 ('quasiquote (#:skip-build? #t)))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-leaf-alice-0.7
              (package
                (name "rust-leaf-alice")
                (version "0.7.5")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "leaf-alice" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments ('quasiquote (#:skip-build? #t)))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-leaf-bob-3
              (package
                (name "rust-leaf-bob")
                (version "3.0.1")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "leaf-bob" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments ('quasiquote (#:skip-build? #t)))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-intermediate-b-1
              (package
                (name "rust-intermediate-b")
                (version "1.2.3")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "intermediate-b" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments
                 ('quasiquote (#:skip-build? #t
                               #:cargo-inputs
                               (("rust-leaf-bob"
                                 ('unquote rust-leaf-bob-3))))))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-intermediate-a-1
              (package
                (name "rust-intermediate-a")
                (version "1.0.42")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "intermediate-a" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments
                 ('quasiquote (#:skip-build? #t
                               #:cargo-inputs
                               (("rust-intermediate-b"
                                 ('unquote rust-intermediate-b-1))
                                ("rust-leaf-alice"
                                 ('unquote 'rust-leaf-alice-0.7))
                                ("rust-leaf-bob"
                                 ('unquote rust-leaf-bob-3))))))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-root-1
              (package
                (name "rust-root")
                (version "1.0.4")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "root" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments
                 ('quasiquote (#:cargo-inputs
                               (("rust-intermediate-a"
                                 ('unquote rust-intermediate-a-1))
                                ("rust-intermediate-b"
                                 ('unquote rust-intermediate-b-1))
                                ("rust-leaf-alice"
                                 ('unquote 'rust-leaf-alice-0.7))
                                ("rust-leaf-bob"
                                 ('unquote rust-leaf-bob-3)))
                               #:cargo-development-inputs
                               (("rust-intermediate-c"
                                 ('unquote rust-intermediate-c-1))))))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0)))))
           #t)
          (x
           (pk 'fail x #f)))
        (match (crate-recursive-import "root"
                                       #:recursive-dev-dependencies? #t)
          ;; rust-intermediate-b has no dependency on the rust-leaf-alice
          ;; package, so this is a valid ordering
          (((define-public 'rust-intermediate-c-1
              (package
                (name "rust-intermediate-c")
                (version "1.0.1")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "intermediate-c" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments
                 ('quasiquote (#:cargo-development-inputs
                               (("rust-leaf-alice"
                                 ('unquote rust-leaf-alice-0.7))))))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-leaf-alice-0.7
              (package
                (name "rust-leaf-alice")
                (version "0.7.5")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "leaf-alice" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-leaf-bob-3
              (package
                (name "rust-leaf-bob")
                (version "3.0.1")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "leaf-bob" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-intermediate-b-1
              (package
                (name "rust-intermediate-b")
                (version "1.2.3")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "intermediate-b" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments
                 ('quasiquote (#:cargo-inputs
                               (("rust-leaf-bob"
                                 ('unquote rust-leaf-bob-3))))))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-intermediate-a-1
              (package
                (name "rust-intermediate-a")
                (version "1.0.42")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "intermediate-a" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments
                 ('quasiquote (#:cargo-inputs
                               (("rust-intermediate-b"
                                 ('unquote rust-intermediate-b-1))
                                ("rust-leaf-alice"
                                 ('unquote 'rust-leaf-alice-0.7))
                                ("rust-leaf-bob"
                                 ('unquote rust-leaf-bob-3))))))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-root-1
              (package
                (name "rust-root")
                (version "1.0.4")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "root" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments
                 ('quasiquote (#:cargo-inputs
                               (("rust-intermediate-a"
                                 ('unquote rust-intermediate-a-1))
                                ("rust-intermediate-b"
                                 ('unquote rust-intermediate-b-1))
                                ("rust-leaf-alice"
                                 ('unquote 'rust-leaf-alice-0.7))
                                ("rust-leaf-bob"
                                 ('unquote rust-leaf-bob-3))
                                ("rust-leaf-bob"
                                 ('unquote rust-leaf-bob-3)))
                               #:cargo-development-inputs
                               (("rust-intermediate-c"
                                 ('unquote rust-intermediate-c-1))))))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0)))))
           #t)
          (x
           (pk 'fail x #f)))))

(test-equal "licenses: MIT OR Apache-2.0"
  '(license:expat license:asl2.0)
  (string->license "MIT OR Apache-2.0"))

(test-equal "licenses: Apache-2.0 / MIT"
  '(license:asl2.0 license:expat)
  (string->license "Apache-2.0 / MIT"))

(test-equal "licenses: Apache-2.0 WITH LLVM-exception"
  '(license:asl2.0 unknown-license!)
  (string->license "Apache-2.0 WITH LLVM-exception"))

(test-equal "licenses: MIT/Apache-2.0 AND BSD-2-Clause"
  '(license:expat license:asl2.0 license:bsd-2)
  (string->license "MIT/Apache-2.0 AND BSD-2-Clause"))

(test-equal "licenses: MIT/Apache-2.0"
  '(license:expat license:asl2.0)
  (string->license "MIT/Apache-2.0"))



(unless have-guile-semver? (test-skip 1))
(test-assert "crate-recursive-import-honors-existing-packages"
  (mock
   ((gnu packages) find-packages-by-name
    (lambda* (name #:optional version)
      (match name
        ("rust-leaf-bob"
         (list rust-leaf-bob-3 rust-leaf-bob-3.0.2-yanked))
        (_ '()))))
   (mock
    ((guix http-client) http-fetch
     (lambda (url . rest)
       (match url
         ("https://crates.io/api/v1/crates/bar"
          (open-input-string test-bar-crate))
         ("https://crates.io/api/v1/crates/bar/1.0.0/download"
          (set! test-source-hash
                (bytevector->nix-base32-string
                 (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
          (open-input-string "empty file\n"))
         ("https://crates.io/api/v1/crates/bar/1.0.0/dependencies"
          (open-input-string test-bar-dependencies))
         ("https://crates.io/api/v1/crates/leaf-bob"
          (open-input-string test-leaf-bob-crate))
         ("https://crates.io/api/v1/crates/leaf-bob/3.0.2/download"
          (set! test-source-hash
                (bytevector->nix-base32-string
                 (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
          (open-input-string "empty file\n"))
         ("https://crates.io/api/v1/crates/leaf-bob/3.0.2/dependencies"
          (open-input-string test-leaf-bob-dependencies))
         ("https://crates.io/api/v1/crates/leaf-bob/4.0.0/download"
          (set! test-source-hash
                (bytevector->nix-base32-string
                 (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
          (open-input-string "empty file\n"))
         ("https://crates.io/api/v1/crates/leaf-bob/4.0.0/dependencies"
          (open-input-string test-leaf-bob-dependencies))
         (_ (error "Unexpected URL: " url)))))
    (match (crate-recursive-import "bar"
                                   #:allow-yanked? #t)
      (((define-public 'rust-bar-1
          (package
            (name "rust-bar")
            (version "1.0.0")
            (source
             (origin
               (method url-fetch)
               (uri (crate-uri "bar" version))
               (file-name
                (string-append name "-" version ".tar.gz"))
               (sha256
                (base32
                 (?  string? hash)))))
            (build-system cargo-build-system)
            (arguments
             ('quasiquote (#:cargo-inputs
                           (("rust-leaf-bob"
                             ('unquote 'rust-leaf-bob-3)))
                           #:cargo-development-inputs
                           (("rust-leaf-bob"
                             ('unquote 'rust-leaf-bob-3.0.2-yanked))
                            ("rust-leaf-bob"
                             ('unquote 'rust-leaf-bob-4.0.0-yanked))))))
            (home-page "http://example.com")
            (synopsis "summary")
            (description "This package provides summary.")
            (license (list license:expat license:asl2.0)))))
       #t)
      (x
       (pk 'fail x #f))))))

(unless have-guile-semver? (test-skip 1))
(test-assert "crate-import-only-yanked-available"
  (mock
   ((guix http-client) http-fetch
    (lambda (url . rest)
      (match url
        ("https://crates.io/api/v1/crates/bar"
         (open-input-string test-bar-crate))
        ("https://crates.io/api/v1/crates/bar/1.0.0/download"
         (set! test-source-hash
               (bytevector->nix-base32-string
                (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
         (open-input-string "empty file\n"))
        ("https://crates.io/api/v1/crates/bar/1.0.0/dependencies"
         (open-input-string test-bar-dependencies))
        ("https://crates.io/api/v1/crates/leaf-bob"
         (open-input-string test-leaf-bob-crate))
        ("https://crates.io/api/v1/crates/leaf-bob/3.0.1/download"
         (set! test-source-hash
               (bytevector->nix-base32-string
                (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
         (open-input-string "empty file\n"))
        ("https://crates.io/api/v1/crates/leaf-bob/3.0.1/dependencies"
         (open-input-string test-leaf-bob-dependencies))
        ("https://crates.io/api/v1/crates/leaf-bob/3.0.2/download"
         (set! test-source-hash
               (bytevector->nix-base32-string
                (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
         (open-input-string "empty file\n"))
        ("https://crates.io/api/v1/crates/leaf-bob/3.0.2/dependencies"
         (open-input-string test-leaf-bob-dependencies))
        ("https://crates.io/api/v1/crates/leaf-bob/4.0.0/download"
         (set! test-source-hash
               (bytevector->nix-base32-string
                (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
         (open-input-string "empty file\n"))
        ("https://crates.io/api/v1/crates/leaf-bob/4.0.0/dependencies"
         (open-input-string test-leaf-bob-dependencies))
        (_ (error "Unexpected URL: " url)))))
        (match (crate-recursive-import "bar"
                                       #:recursive-dev-dependencies? #t
                                       #:allow-yanked? #t)
          (((define-public 'rust-leaf-bob-4.0.0-yanked
              (package
                (name "rust-leaf-bob")
                (version "4.0.0")
                ($ <comment> "; This version was yanked!\n" #t)
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "leaf-bob" version))
                   (file-name
                    (string-append name "-" version "-yanked.tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (properties ('quote (('crate-version-yanked? . #t))))
                (build-system cargo-build-system)
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-leaf-bob-3.0.2-yanked
              (package
                (name "rust-leaf-bob")
                (version "3.0.2")
                ($ <comment> "; This version was yanked!\n" #t)
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "leaf-bob" version))
                   (file-name
                    (string-append name "-" version "-yanked.tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (properties ('quote (('crate-version-yanked? . #t))))
                (build-system cargo-build-system)
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-leaf-bob-3
              (package
                (name "rust-leaf-bob")
                (version "3.0.1")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "leaf-bob" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0))))
            (define-public 'rust-bar-1
              (package
                (name "rust-bar")
                (version "1.0.0")
                (source
                 (origin
                   (method url-fetch)
                   (uri (crate-uri "bar" version))
                   (file-name
                    (string-append name "-" version ".tar.gz"))
                   (sha256
                    (base32
                     (?  string? hash)))))
                (build-system cargo-build-system)
                (arguments
                 ('quasiquote (#:cargo-inputs
                               (("rust-leaf-bob"
                                 ('unquote 'rust-leaf-bob-3)))
                               #:cargo-development-inputs
                               (("rust-leaf-bob"
                                 ('unquote 'rust-leaf-bob-3.0.2-yanked))
                                ("rust-leaf-bob"
                                 ('unquote 'rust-leaf-bob-4.0.0-yanked))))))
                (home-page "http://example.com")
                (synopsis "summary")
                (description "This package provides summary.")
                (license (list license:expat license:asl2.0)))))
            #t)
          (x
           (pk 'fail (pretty-print-with-comments (current-output-port) x) #f)))))


(test-assert "crate-lockfile-import"
  (begin
    (call-with-output-file temp-file
      (lambda (port)
        (display "\
# This file is automatically @generated by Cargo.
# It is not intended for manual editing.
version = 3

[[package]]
name = \"adler2\"
version = \"2.0.0\"
source = \"registry+https://github.com/rust-lang/crates.io-index\"
checksum = \"512761e0bb2578dd7380c6baaa0f4ce03e84f95e960231d1dec8bf4d7d6e2627\"

[[package]]
name = \"aho-corasick\"
version = \"1.1.3\"
source = \"registry+https://github.com/rust-lang/crates.io-index\"
checksum = \"8e60d3430d3a69478ad0993f19238d2df97c507009a52b3c10addcd7f6bcb916\"
dependencies = [
 \"memchr\",
]

[[package]]
name = \"smithay\"
version = \"0.4.0\"
source = \"git+https://github.com/Smithay/smithay.git?rev=\
0cd3345c59f7cb139521f267956a1a4e33248393#\
0cd3345c59f7cb139521f267956a1a4e33248393\"
dependencies = [
 \"appendlist\",
]

[[package]]
name = \"test\"
version = \"25.2.0\"\n" port)))
    (mock
     ((guix scripts download) guix-download
      (lambda _
        (format #t "~a~%~a~%"
                "/gnu/store/in056fyrz6nvy3jpxrxglgj30g0lwniv-smithay-0cd3345"
                "191h87bpzg0l1ihfb4hmx00b86pfb5mwwc6s8i49al0vigc14l37")))
     (let-values
         (((source-expressions cargo-inputs-entry)
           (cargo-lock->expressions temp-file "test")))
       (and
        (match source-expressions
          (`((define rust-adler2-2.0.0
               (crate-source
                "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))
             (define rust-aho-corasick-1.1.3
               (crate-source
                "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))
             (define rust-smithay-0.4.0.0cd3345
               ,($ <comment>
                   ";; TODO: Define standalone package if this is a workspace.\n"
                   #f)
               (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/Smithay/smithay.git")
                       (commit "0cd3345c59f7cb139521f267956a1a4e33248393")))
                 (file-name (git-file-name "rust-smithay" "0.4.0.0cd3345"))
                 (sha256
                  (base32
                   "191h87bpzg0l1ihfb4hmx00b86pfb5mwwc6s8i49al0vigc14l37")))))
           #t)
          (x
           (pk 'fail (pretty-print-with-comments (current-output-port) x) #f)))
        (match cargo-inputs-entry
          (`(test => (list rust-adler2-2.0.0
                           rust-aho-corasick-1.1.3
                           rust-smithay-0.4.0.0cd3345))
           #t)
          (x
           (pk 'fail x #f))))))))

(test-end "crate")

(false-if-exception (delete-file temp-file))
