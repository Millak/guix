;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
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

(define-module (test-elm)
  #:use-module (guix build-system elm)
  #:use-module (guix import elm)
  #:use-module (guix base32)
  #:use-module (guix hash)
  #:use-module (guix utils)
  #:autoload   (gcrypt hash) (hash-algorithm sha256)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "elm")

(test-group "elm->package-name and infer-elm-package-name"
  (test-group "round trip"
    ;; Cases when our heuristics can find the upstream name.
    (define-syntax-rule (test-round-trip elm guix)
      (test-group elm
        (test-equal "elm->package-name" guix
                    (elm->package-name elm))
        (test-equal "infer-elm-package-name" elm
                    (infer-elm-package-name guix))))
    (test-round-trip "elm/core" "elm-core")
    (test-round-trip "elm/html" "elm-html")
    (test-round-trip "elm-explorations/markdown" "elm-explorations-markdown")
    (test-round-trip "elm-explorations/test" "elm-explorations-test")
    (test-round-trip "elm-explorations/foo-bar" "elm-explorations-foo-bar")
    (test-round-trip "elm/explorations" "elm-explorations")
    (test-round-trip "terezka/intervals" "elm-terezka-intervals")
    (test-round-trip "justinmimbs/time-extra" "elm-justinmimbs-time-extra")
    (test-round-trip "danhandrea/elm-date-format"
                     "elm-danhandrea-elm-date-format"))
  (test-group "upstream-name needed"
    ;; Upstream names that our heuristic can't infer.  We still check that the
    ;; round-trip behavior of 'infer-elm-package-name' works as promised for
    ;; the hypothetical Elm name it doesn't infer.
    (define-syntax-rule (test-upstream-needed elm guix inferred)
      (test-group elm
        (test-equal "elm->package-name" guix
                    (elm->package-name elm))
        (test-group "infer-elm-package-name"
          (test-equal "infers other name" inferred
                      (infer-elm-package-name guix))
          (test-equal "infered name round-trips" guix
                      (elm->package-name inferred)))))
    (test-upstream-needed "elm/virtual-dom"
                          "elm-virtual-dom"
                          "virtual/dom")
    (test-upstream-needed "elm/project-metadata-utils"
                          "elm-project-metadata-utils"
                          "project/metadata-utils")
    (test-upstream-needed "explorations/foo"
                          "elm-explorations-foo"
                          "elm-explorations/foo")
    (test-upstream-needed "explorations/foo-bar"
                          "elm-explorations-foo-bar"
                          "elm-explorations/foo-bar")
    (test-upstream-needed "explorations-central/foo"
                          "elm-explorations-central-foo"
                          "elm-explorations/central-foo")
    (test-upstream-needed "explorations-central/foo-bar"
                          "elm-explorations-central-foo-bar"
                          "elm-explorations/central-foo-bar")
    (test-upstream-needed "elm-xyz/foo"
                          "elm-xyz-foo"
                          "xyz/foo")
    (test-upstream-needed "elm-xyz/foo-bar"
                          "elm-xyz-foo-bar"
                          "xyz/foo-bar")
    (test-upstream-needed "elm-explorations-xyz/foo"
                          "elm-explorations-xyz-foo"
                          "elm-explorations/xyz-foo")
    (test-upstream-needed "elm-explorations-xyz/foo-bar"
                          "elm-explorations-xyz-foo-bar"
                          "elm-explorations/xyz-foo-bar"))
  (test-group "no inferred Elm name"
    ;; Cases that 'infer-elm-package-name' should not attempt to handle,
    ;; because 'elm->package-name' would never produce such names.
    (define-syntax-rule (test-not-inferred guix)
      (test-assert guix (not (infer-elm-package-name guix))))
    (test-not-inferred "elm")
    (test-not-inferred "guile")
    (test-not-inferred "gcc-toolchain")
    (test-not-inferred "font-adobe-source-sans-pro")))

(define test-package-registry-json
  ;; we intentionally list versions in different orders here
  "{
    \"elm/core\": [\"1.0.0\", \"1.0.1\", \"1.0.2\", \"1.0.3\", \"1.0.4\"],
    \"elm-guix/demo\": [\"2.0.0\", \"3.0.0\", \"1.0.0\"]
}")

(define test-elm-core-json
  "{
    \"type\": \"package\",
    \"name\": \"elm/core\",
    \"summary\": \"Elm's standard libraries\",
    \"license\": \"BSD-3-Clause\",
    \"version\": \"1.0.4\",
    \"exposed-modules\": {
        \"Primitives\": [
            \"Basics\",
            \"String\",
            \"Char\",
            \"Bitwise\",
            \"Tuple\"
        ],
        \"Collections\": [
            \"List\",
            \"Dict\",
            \"Set\",
            \"Array\"
        ],
        \"Error Handling\": [
            \"Maybe\",
            \"Result\"
        ],
        \"Debug\": [
            \"Debug\"
        ],
        \"Effects\": [
            \"Platform.Cmd\",
            \"Platform.Sub\",
            \"Platform\",
            \"Process\",
            \"Task\"
        ]
    },
    \"elm-version\": \"0.19.0 <= v < 0.20.0\",
    \"dependencies\": {},
    \"test-dependencies\": {}
}")

(define test-elm-core-readme
  "# Core Libraries

Every Elm project needs this package!

It provides **basic functionality** like addition and subtraction as well as
**data structures** like lists, dictionaries, and sets.")

(define test-elm-guix-demo-json
  "{
    \"type\": \"package\",
    \"name\": \"elm-guix/demo\",
    \"summary\": \"A test for `(guix import elm)`\",
    \"license\": \"GPL-3.0-or-later\",
    \"version\": \"3.0.0\",
    \"exposed-modules\": [
        \"Guix.Demo\"
    ],
    \"elm-version\": \"0.19.0 <= v < 0.20.0\",
    \"dependencies\": {
        \"elm/core\": \"1.0.0 <= v < 2.0.0\"
    },
    \"test-dependencies\": {
        \"elm/json\": \"1.0.0 <= v < 2.0.0\"
    }
}")

(define test-elm-guix-demo-readme
  ;; intentionally left blank
  "")

(define (directory-sha256 directory)
  "Returns the string representing the hash of DIRECTORY as would be used in a
package definition."
  (bytevector->nix-base32-string
   (file-hash* directory
               #:algorithm (hash-algorithm sha256)
               #:recursive? #t)))

(test-group "(guix import elm)"
  (call-with-temporary-directory
   (lambda (dir)
     ;; Initialize our fake git checkouts.
     (define elm-core-dir
       (string-append dir "/test-elm-core-1.0.4"))
     (define elm-guix-demo-dir
       (string-append dir "/test-elm-guix-demo-3.0.0"))
     (for-each (match-lambda
                 ((dir json readme)
                  (mkdir dir)
                  (with-output-to-file (string-append dir "/elm.json")
                    (lambda ()
                      (display json)))
                  (with-output-to-file (string-append dir "/README.md")
                    (lambda ()
                      (display readme)))))
               `((,elm-core-dir ,test-elm-core-json ,test-elm-core-readme)
                 (,elm-guix-demo-dir
                  ,test-elm-guix-demo-json
                  ,test-elm-guix-demo-readme)))
     ;; Replace network resources with sample data.
     (parameterize ((%elm-package-registry
                     (lambda ()
                       (json-string->scm test-package-registry-json)))
                    (%current-elm-checkout
                     (lambda (name version)
                       (match (list name version)
                         (("elm/core" "1.0.4")
                          elm-core-dir)
                         (("elm-guix/demo" "3.0.0")
                          elm-guix-demo-dir)))))
       (test-assert "(elm->guix-package \"elm/core\")"
         (match (elm->guix-package "elm/core")
           (`(package
               (name "elm-core")
               (version "1.0.4")
               (source (elm-package-origin
                        "elm/core"
                        version
                        (base32 ,(? string? hash))))
               (build-system elm-build-system)
               (home-page
                "https://package.elm-lang.org/packages/elm/core/1.0.4")
               (synopsis "Elm's standard libraries")
               (description "Every Elm project needs this package!")
               (license license:bsd-3))
            (equal? (directory-sha256 elm-core-dir)
                    hash))
           (x
            (raise-exception x))))
       (test-assert "(elm-recursive-import \"elm-guix/demo\")"
         (match (elm-recursive-import "elm-guix/demo")
           (`((package
                (name "elm-guix-demo")
                (version "3.0.0")
                (source (elm-package-origin
                         "elm-guix/demo"
                         version
                         (base32 ,(? string? hash))))
                (build-system elm-build-system)
                (propagated-inputs
                 ,'`(("elm-core" ,elm-core)))
                (inputs
                 ,'`(("elm-json" ,elm-json)))
                (home-page
                 "https://package.elm-lang.org/packages/elm-guix/demo/3.0.0")
                (synopsis "A test for `(guix import elm)`")
                (description
                 "This package provides a test for `(guix import elm)`")
                (properties '((upstream-name . "elm-guix/demo")))
                (license license:gpl3+)))
            (equal? (directory-sha256 elm-guix-demo-dir)
                    hash))
           (x
            (raise-exception x))))))))

(test-end "elm")
