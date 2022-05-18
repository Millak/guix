;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
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

(define-module (gnu packages elm)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-web)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system elm)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;; The `elm` build usually calls out to itself via Template Haskell to compile
;; the `elm reactor` web app (which depends on additional Elm packages) and
;; embeds the static files into itself.  The reactor isn't required to compile
;; Elm applications, so we want to skip it for the bootstrap package, but we
;; also want to be able to enable it once we can build it.  We patch Elm to
;; instead look for the files on disk relative to the executable and to have
;; `elm reactor` exit with a useful error message if they aren't there.
(define %reactor-root-base
  "share/elm/reactor-")
(define-public elm
  (package
    (name "elm")
    (version "0.19.1")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name name version))
       (uri (git-reference
             (url "https://github.com/elm/compiler/")
             (commit version)))
       (sha256
        (base32 "1rdg3xp3js9xadclk3cdypkscm5wahgsfmm4ldcw3xswzhw6ri8w"))
       (patches
        (search-patches "elm-reactor-static-files.patch"
                        "elm-offline-package-registry.patch"))))
    (build-system haskell-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--ghc-option=-DGUIX_REACTOR_STATIC_REL_ROOT="
                             "\"../" #$%reactor-root-base
                             #$(package-version this-package)
                             "\""))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'update-constraints
            (lambda _
              (substitute* "elm.cabal"
                (("(ansi-terminal|containers|network|http-client|language-glsl)\\s+[^,]+" all dep)
                 dep)))))))
    (inputs
     (list ghc-ansi-terminal
           ghc-ansi-wl-pprint
           ghc-edit-distance
           ghc-filelock
           ghc-http
           ghc-http-client
           ghc-http-client-tls
           ghc-http-types
           ghc-language-glsl
           ghc-logict
           ghc-network
           ghc-raw-strings-qq
           ghc-scientific
           ghc-sha
           ghc-snap-core
           ghc-snap-server
           ghc-unordered-containers
           ghc-utf8-string
           ghc-vector
           ghc-zip-archive))
    (home-page "https://elm-lang.org")
    (synopsis "Programming language for Web applications")
    (description
     "Elm is a statically-typed, purely-functional programming language for
the browser.  The @command{elm} exectable includes commands for developers
such as @command{elm make} and @command{elm repl}.")
    (license license:bsd-3)))

;; The 'elm' package used to be called 'elm-compiler'.
(define-public elm-compiler
  (deprecated-package "elm-compiler" elm))

(define-public elm-core
  (package
    (name "elm-core")
    (version "1.0.5")
    (source
     (elm-package-origin
      "elm/core"
      version
      (base32 "0g3xbi8f9k5q45s95nx3jfvzwdf4b2n63a52wr4027d2xjx0pmvl")))
    (build-system elm-build-system)
    (inputs (list elm-json-bootstrap))
    (arguments
     (list #:implicit-elm-package-inputs? #f))
    (home-page "https://package.elm-lang.org/packages/elm/core/1.0.5")
    (synopsis "Elm's standard libraries")
    (description "Every Elm project needs this package!")
    (license license:bsd-3)))

(define-public elm-json
  (package
    (name "elm-json")
    (version "1.1.3")
    (source
     (elm-package-origin
      "elm/json"
      version
      (base32 "1hx986yqw1v2bpkrh6brszl8n8awwg1s8zi7v5qg0p1rqwvjlicz")))
    (build-system elm-build-system)
    (propagated-inputs (list elm-core))
    (arguments
     (list #:implicit-elm-package-inputs? #f))
    (home-page "https://package.elm-lang.org/packages/elm/json/1.1.3")
    (synopsis "Encode and decode JSON values in Elm")
    (description
     "This package helps you convert between Elm values and JSON values.")
    (license license:bsd-3)))

(define-public elm-json-bootstrap
  ;; elm/core doesn't depend on elm/json,
  ;; but elm-build-system's strategy for building it
  ;; (and everything else) does
  (hidden-package
   (package
     (inherit elm-json)
     (name "elm-json-bootstrap")
     (properties '((upstream-name . "elm/json")))
     (propagated-inputs '())
     (arguments
      (list #:phases
            #~(modify-phases %standard-phases
                (delete 'configure)
                (delete 'build)
                (delete 'validate-compiled))
            #:implicit-elm-package-inputs? #f)))))
