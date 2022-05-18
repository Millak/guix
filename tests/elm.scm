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

(test-end "elm")
