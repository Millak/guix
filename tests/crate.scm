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

(define test-foo-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.0\",
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
        \"yanked\": true
      }
    ]
  }
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
        \"yanked\": true
      }
    ]
  }
}")

(define test-source-hash
  "")

(define have-guile-semver?
  (false-if-exception (resolve-interface '(semver))))

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

(unless have-guile-semver? (test-skip 3))
(test-assert "crate->guix-package"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://crates.io/api/v1/crates/foo"
              (open-input-string test-foo-crate))
             ("https://crates.io/api/v1/crates/foo/1.0.0/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             (_ (error "Unexpected URL: " url)))))

        (match (crate->guix-package "foo")
          (`(define-public foo
              (package (name "foo")
                       (version "1.0.0")
                       (source
                        (origin
                          (method url-fetch)
                          (uri (crate-uri "foo" version))
                          (file-name (string-append name "-" version ".tar.gz"))
                          (sha256
                           (base32
                            ,(?  string? hash)))))
                       (build-system cargo-build-system)
                       (inputs (cargo-inputs 'foo))
                       (home-page "http://example.com")
                       (synopsis "summary")
                       (description "This package provides summary.")
                       (license (list license:expat license:asl2.0))))

           (string=? test-source-hash hash))
          (x
           (pk 'fail x #f)))))

(test-assert "crate->guix-package yanked"
  (mock
   ((guix http-client) http-fetch
    (lambda (url . rest)
      (match url
        ("https://crates.io/api/v1/crates/foo"
         (open-input-string test-foo-crate))
        ("https://crates.io/api/v1/crates/foo/1.0.3/download"
         (set! test-source-hash
               (bytevector->nix-base32-string
                (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
         (open-input-string "empty file\n"))
        (_ (error "Unexpected URL: " url)))))

   (match (crate->guix-package "foo" #:version "1.0.3" #:allow-yanked? #t)
     (`(define-public foo
         (package (name "foo")
                  (version "1.0.3")
                  ,(? comment?)
                  (source
                   (origin
                     (method url-fetch)
                     (uri (crate-uri "foo" version))
                     (file-name (string-append name "-" version "-yanked.tar.gz"))
                     (sha256
                      (base32
                       ,(?  string? hash)))))
                  (properties '((crate-version-yanked? . #t)))
                  (build-system cargo-build-system)
                  (inputs (cargo-inputs 'foo))
                  (home-page "http://example.com")
                  (synopsis "summary")
                  (description "This package provides summary.")
                  (license (list license:expat license:asl2.0))))
      (string=? test-source-hash hash))
     (x
      (pk 'fail x #f)))))

(test-assert "crate->guix-package only yanked available"
  (mock
   ((guix http-client) http-fetch
    (lambda (url . rest)
      (match url
        ("https://crates.io/api/v1/crates/bar"
         (open-input-string test-foo-crate))
        ("https://crates.io/api/v1/crates/bar/1.0.0/download"
         (set! test-source-hash
               (bytevector->nix-base32-string
                (gcrypt-sha256 (string->bytevector "empty file\n" "utf-8"))))
         (open-input-string "empty file\n"))
        (_ (error "Unexpected URL: " url)))))

   (match (crate->guix-package "bar")
     (`(define-public bar
         (package (name "bar")
                  (version "1.0.0")
                  (source
                   (origin
                     (method url-fetch)
                     (uri (crate-uri "bar" version))
                     (file-name
                      (string-append name "-" version ".tar.gz"))
                     (sha256
                      (base32
                       ,(?  string? hash)))))
                  (build-system cargo-build-system)
                  (inputs (cargo-inputs 'bar))
                  (home-page "http://example.com")
                  (synopsis "summary")
                  (description "This package provides summary.")
                  (license (list license:expat license:asl2.0))))
      (string=? test-source-hash hash))
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
