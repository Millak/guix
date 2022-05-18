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

(define-public elm-virtual-dom
  (package
    (name "elm-virtual-dom")
    (version "1.0.3")
    (source
     (elm-package-origin
      "elm/virtual-dom"
      version
      (base32 "1bjyyws7l0qvgp4ixzaimwriq86ncx5bvrzaksvjx3pv7bmkbx69")))
    (build-system elm-build-system)
    (propagated-inputs (list elm-json elm-core))
    (home-page "https://package.elm-lang.org/packages/elm/virtual-dom/1.0.2")
    (synopsis
     "Elm's low-level virtual DOM implementation")
    (description
     "This package provides a virtual DOM implementation that backs Elm's
core libraries for HTML and SVG.  You should almost certainly use those
higher-level libraries directly.")
    (properties '((upstream-name . "elm/virtual-dom")))
    (license license:bsd-3)))

(define-public elm-html
  (package
    (name "elm-html")
    (version "1.0.0")
    (source
     (elm-package-origin
      "elm/html"
      version
      (base32 "15k1679ja57vvlpinpv06znmrxy09lbhzfkzdc89i01qa8c4gb4a")))
    (build-system elm-build-system)
    (propagated-inputs
     (list elm-virtual-dom
           elm-json
           elm-core))
    (home-page "https://package.elm-lang.org/packages/elm/html/1.0.0")
    (synopsis "Fast HTML, rendered with virtual DOM diffing")
    (description "This package provides Elm's HTML rendering library.")
    (license license:bsd-3)))

(define-public elm-svg
  (package
    (name "elm-svg")
    (version "1.0.1")
    (source
     (elm-package-origin
      "elm/svg"
      version
      (base32 "1iqsc3p129j56lp1y3z3mfc6x1shvrmx3pkhri2777ylhyw90qvl")))
    (build-system elm-build-system)
    (propagated-inputs
     (list elm-html
           elm-virtual-dom
           elm-json
           elm-core))
    (home-page "https://package.elm-lang.org/packages/elm/svg/1.0.1")
    (synopsis "Fast SVG, rendered with virtual DOM diffing")
    (description
     "This package provides Elm's @acronym{SVG, Scalable Vector Graphics}
library.")
    (license license:bsd-3)))

(define-public elm-time
  (package
    (name "elm-time")
    (version "1.0.0")
    (source
     (elm-package-origin
      "elm/time"
      version
      (base32 "0wqa2vhl1zf8z0j2yd3yjwfhr0dydfns43bbzll3k4rhnjadxr1l")))
    (build-system elm-build-system)
    (propagated-inputs (list elm-core))
    (home-page "https://package.elm-lang.org/packages/elm/time/1.0.0")
    (synopsis
     "POSIX time and time zones in Elm")
    (description
     "This package provides an Elm library for working with POSIX times, time
zones, formatting, and the clock.")
    (license license:bsd-3)))

(define-public elm-url
  (package
    (name "elm-url")
    (version "1.0.0")
    (source
     (elm-package-origin
      "elm/url"
      version
      (base32 "1f2ij4i7zmijnj2i50qf19lpkr14bhms8dkq029inb5mydi9f8gs")))
    (build-system elm-build-system)
    (propagated-inputs (list elm-core))
    (home-page "https://package.elm-lang.org/packages/elm/url/1.0.0")
    (synopsis
     "Create and parse URLs in Elm")
    (description
     "This package helps you:

@enumerate
@item
build new URLs; and

@item
parse existing URLs into nice Elm data structures.
@end enumerate

Use it for HTTP and for @dfn{routing} in @acronym{SPAs, single-page apps}.")
    (license license:bsd-3)))

(define-public elm-browser
  (package
    (name "elm-browser")
    (version "1.0.2")
    (source
     (elm-package-origin
      "elm/browser"
      version
      (base32 "0863nw2hhbpm3s03lm1imi5x28wwknzrwg2p79s5mydgvdvgwjf0")))
    (build-system elm-build-system)
    (propagated-inputs
     (list elm-virtual-dom
           elm-url
           elm-time
           elm-json
           elm-html
           elm-core))
    (home-page "https://package.elm-lang.org/packages/elm/browser/1.0.2")
    (synopsis
     "Run Elm in browsers")
    (description
     "This package allows you to create Elm programs that run in browsers,
with access to browser history for @acronym{SPAs, single-page apps}.")
    (license license:bsd-3)))

(define-public elm-bytes
  (package
    (name "elm-bytes")
    (version "1.0.8")
    (source
     (elm-package-origin
      "elm/bytes"
      version
      (base32 "0n411j2cyz9m241q6vszfzpq3fraradwal5m0gigp2505mdfpz3x")))
    (build-system elm-build-system)
    (propagated-inputs (list elm-core))
    (home-page "https://package.elm-lang.org/packages/elm/bytes/1.0.8")
    (synopsis "Work with sequences of bytes in Elm")
    (description "This package provides an Elm library for working with
densely packed sequences of bytes, such as @code{ArrayBuffer}, typed arrays,
and @code{DataView}.")
    (license license:bsd-3)))

(define-public elm-file
  (package
    (name "elm-file")
    (version "1.0.5")
    (source
     (elm-package-origin
      "elm/file"
      version
      (base32 "0aimgicrdpys0v89m2wjx413561zil14cczjh6mkn9jcgckx6yng")))
    (build-system elm-build-system)
    (propagated-inputs
     (list elm-time
           elm-json
           elm-core
           elm-bytes))
    (home-page "https://package.elm-lang.org/packages/elm/file/1.0.5")
    (synopsis "Work with files in Elm")
    (description "This package enables Elm programs to select files, download
files, and work with file content.")
    (license license:bsd-3)))

(define-public elm-http
  (package
    (name "elm-http")
    (version "2.0.0")
    (source
     (elm-package-origin
      "elm/http"
      version
      (base32 "0mfbz0lkfidmq5xpv5csw8943q0yrpvj0rwd2vb0gc8rbsfc9dg8")))
    (build-system elm-build-system)
    (propagated-inputs
     (list elm-json
           elm-file
           elm-core
           elm-bytes))
    (home-page "https://package.elm-lang.org/packages/elm/http/2.0.0")
    (synopsis "Make HTTP requests in Elm")
    (description "This package enables Elm programs to make HTTP requests and
talk to servers.")
    (license license:bsd-3)))

(define-public elm-parser
  (package
    (name "elm-parser")
    (version "1.1.0")
    (source
     (elm-package-origin
      "elm/parser"
      version
      (base32 "06xx29rmagc5r45qfpvrd393lz83ylngidfp08432f1qc8y6r3lh")))
    (build-system elm-build-system)
    (propagated-inputs (list elm-core))
    (home-page "https://package.elm-lang.org/packages/elm/parser/1.1.0")
    (synopsis
     "Parsing library for Elm")
    (description
     "Regular expressions are quite confusing and difficult to use.  This
library provides a coherent alternative that handles more cases and produces
clearer code.  It is focused on simplicity and great error messages.")
    (license license:bsd-3)))

(define-public elm-project-metadata-utils
  (package
    (name "elm-project-metadata-utils")
    (version "1.0.2")
    (source
     (elm-package-origin
      "elm/project-metadata-utils"
      version
      (base32 "1wj7chfy4knwwyc3k0hy431c80hs7hc686qsr34ayn8gip73x2jj")))
    (build-system elm-build-system)
    (propagated-inputs
     (list elm-parser
           elm-json
           elm-core))
    (home-page
     "https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.2")
    (synopsis "Work with @file{elm.json} and @file{docs.json} files in Elm")
    (description
     "This package is meant for people creating Elm tooling, like editor
plugins.  If you just want to make stuff in Elm, there is nothing here for
you.")
    (properties '((upstream-name . "elm/project-metadata-utils")))
    (license license:bsd-3)))

(define-public elm-explorations-markdown
  (package
    (name "elm-explorations-markdown")
    (version "1.0.0")
    (source
     (elm-package-origin
      "elm-explorations/markdown"
      version
      (base32 "1f57ikdpbbhchcpwj32216gxjxijrc3sdpg27s1cgzia9pnkqk6p")))
    (build-system elm-build-system)
    (propagated-inputs (list elm-html elm-core))
    (home-page
     "https://package.elm-lang.org/packages/elm-explorations/markdown/1.0.0")
    (synopsis "Fast markdown parsing and rendering in Elm")
    (description
     "This package is for markdown parsing and rendering in Elm.  It is based
on the @code{marked} project, which focuses on speed.")
    (license license:bsd-3)))

