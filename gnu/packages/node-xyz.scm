;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Noisytoot <noisytoot@disroot.org>
;;; Copyright © 2021 Charles <charles.b.jackson@protonmail.com>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 Dhruvin Gandhi <contact@dhruvin.dev>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2024 Daniel Khodabakhsh <d.khodabakhsh@gmail.com>
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

(define-module (gnu packages node-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web)
  #:use-module (guix build-system node)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public node-acorn
  (package
    (name "node-acorn")
    (version "8.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/acornjs/acorn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32  "10lpqbq4wvndx13mh7yjqgpcp3ac81b9zmrn4qb1qpzgy462fa92"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:modules
       ((guix build node-build-system)
        (srfi srfi-1)
        (ice-9 match)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "acorn")))
         (add-before 'configure 'avoid-prepare-scripts
           (lambda _
             ;; We need to remove the prepare script from "package.json", as
             ;; it would try to use the build environment and would block the
             ;; automatic building by other packages making use of node-acorn.
             (modify-json (delete-fields '(("scripts" "prepare"))))))
         (replace 'build
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((esbuild (search-input-file (or native-inputs inputs)
                                        "/bin/esbuild")))
               (invoke esbuild
                     "src/index.js"
                     "--outfile=dist/acorn.js"
                     "--format=cjs"
                     "--bundle"
                     "--platform=node")
               (invoke esbuild
                     "src/index.js"
                     "--outfile=dist/acorn.mjs"
                     "--format=esm"
                     "--bundle"
                     "--platform=node")
               (invoke esbuild
                     "src/bin/acorn.js"
                     "--outfile=dist/bin.js"
                     "--format=cjs"
                     "--platform=node")))))))
    (native-inputs
     (list esbuild))
    (home-page "https://github.com/acornjs/acorn/tree/master/acorn")
    (synopsis "Javascript-based Javascript parser")
    (description "Acornjs is a Javascript parser with many options and an
architecture supporting plugins.")
    (license license:expat)))

(define-public node-addon-api
  (package
    (name "node-addon-api")
    (version "8.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nodejs/node-addon-api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1swvhdss2w636l24bnssnwb1rqh7i6lhpkk4knbrvpspqf80kagc"))))
    (inputs
     (list python node-safe-buffer))
    (build-system node-build-system)
    (arguments
     `(#:modules
       ((guix build node-build-system)
        (srfi srfi-1)
        (ice-9 match)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           `("benchmark"
                             "bindings"
                             "clang-format"
                             "eslint"
                             "eslint-config-semistandard"
                             "eslint-config-standard"
                             "eslint-plugin-import"
                             "eslint-plugin-node"
                             "eslint-plugin-promise"
                             "fs-extra"
                             "neostandard"
                             "path"
                             "pre-commit"
                             "semver"))
               ;; We can't run the js-based tests,
               ;; but we can still do the C++ parts
               (replace-fields (list (cons
                 "scripts.test" "echo stopping after pretest on Guix")))))))))
    (home-page "https://github.com/nodejs/node-addon-api")
    (synopsis "Node.js API (Node-API) header-only C++ wrappers")
    (description "This module contains header-only C++ wrapper classes which
simplify the use of the C based Node-API provided by Node.js when using C++.
It provides a C++ object model and exception handling semantics with low
overhead.

Node-API is an ABI stable C interface provided by Node.js for building native
addons.  It is intended to insulate native addons from changes in the
underlying JavaScript engine and allow modules compiled for one version to run
on later versions of Node.js without recompilation.  The @code{node-addon-api}
module, which is not part of Node.js, preserves the benefits of the Node-API
as it consists only of inline code that depends only on the stable API
provided by Node-API.

It is important to remember that @emph{other} Node.js interfaces such as
@code{libuv} (included in a project via @code{#include <uv.h>}) are not
ABI-stable across Node.js major versions.")
    (license license:expat)))

(define-public node-bindings
  (package
    (name "node-bindings")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TooTallNate/node-bindings")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "100gp6kpjvd4j1dqnp0sbjr1hqx5mz7r61q9qy527jyhk9mj47wk"))))
    (inputs
     (list node-file-uri-to-path))
    (build-system node-build-system)
    (arguments
     ;; there are no tests
     `(#:tests? #f))
    (home-page "https://github.com/TooTallNate/node-bindings")
    (synopsis "Locate native addons")
    (description "This package provides a helper module to locate native
addons in a wide array of potential locations.")
    (license license:expat)))

(define-public node-buffer-crc32
  (package
    (name "node-buffer-crc32")
    (version "0.2.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brianloveswords/buffer-crc32")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09qx2mnd898190m50mc0rhyvbm7d677sxz9bn09qmqkz6fnsddgf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json (delete-dependencies '("tap"))))))))
    (home-page "https://github.com/brianloveswords/buffer-crc32")
    (synopsis "CRC32 implementation in Javascript")
    (description
     "This package provides a CRC32 algorithm that works with binary data
and fancy character sets, signed or unsigned data and has tests, for Node.")
    (license license:expat)))

(define-public node-color-name
  (package
    (name "node-color-name")
    (version "1.1.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/colorjs/color-name")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "09rbmj16nfwcwkhrybqxyy66bkrs50vpw6hkdqqb14l3gsyxpr74"))))
    (build-system node-build-system)
    (home-page "https://github.com/colorjs/color-name")
    (synopsis "JSON with CSS color names")
    (description
     "This package provides a JSON list with color names and their values.")
    (license license:expat)))

(define-public node-crx3
  (package
    (name "node-crx3")
    (version "1.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ahwayakchih/crx3")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1snqyw8c3s9p2clhqh1172z0rs1was36sfxkk6acgpar32c2rwzw"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'replace-mri-by-minimist
                    (lambda _
                      (substitute* "package.json"
                        (("\"mri\": \"\\^1\\.1\\.6\",")
                         "\"minimist\": \"^1.2.6\","))
                      (substitute* "lib/configuration.js"
                        (("mri")
                         "minimist"))))
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json (delete-dependencies
                                    '("c8"
                                      "docdash"
                                      "eslint"
                                      "eslint-plugin-jsdoc"
                                      "jsdoc"
                                      "tap-diff"
                                      "tape"
                                      "tape-catch"))))))))
    (inputs (list node-minimist node-pbf node-yazl))
    (home-page "https://github.com/ahwayakchih/crx3")
    (synopsis "Create CRXv3 browser extensions with Javascript")
    (description
     "This package creates web extension files (CRXv3) for Chromium versions
64.0.3242 and above and all other browsers supporting the file format and API.")
    (license license:bsd-3)))

(define-public node-debug
  (package
    (name "node-debug")
    (version "4.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/debug-js/debug")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ji0dmdl2xkgxqxvd6xjy7k3mmknmhvqjgc40vyly9ka1mpf20vb"))))
    (inputs
     (list node-ms))
    (build-system node-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           `("brfs"
                             "browserify"
                             "coveralls"
                             "istanbul"
                             "karma"
                             "karma-browserify"
                             "karma-chrome-launcher"
                             "karma-mocha"
                             "mocha"
                             "mocha-lcov-reporter"
                             "xo"
                             "supports-color"))))))
       #:tests? #f))
    (home-page "https://github.com/debug-js/debug")
    (synopsis "Debugging utility for Node.js")
    (description "The @code{debug} module exposes a function, which if called
with a module name as argument provides a function that writes debug output to
@code{console.error} under that module name.  This output can be controlled in
a more fine-grained manner by binding the @env{DEBUG} variable.")
    (license license:expat)))

(define-public node-env-variable
  (package
    (name "node-env-variable")
    (version "0.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/bigpipe/env-variable")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0nnpxjxfhy4na7fixb7p3ww6ard5xgggfm83b78i333867r4gmsq"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (home-page "https://github.com/bigpipe/env-variable")
    (synopsis "Environment variables for Node with fallbacks")
    (description "This package provides environment variables with
@code{process.env}, @code{window.name}, @code{location.hash} and
@code{localStorage} fallbacks.")
    (license license:expat)))

(define-public node-far
  (package
    (name "node-far")
    (version "0.0.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/felixge/node-far")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "083rv1rszjn0i91zcpaghlid0kwhk0angmpj4hiflrlyhd6cmjzw"))))
    (build-system node-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; We skip the two tests which are supposed to fail.
             (invoke "bin/node-far" "-v" "test/" "-e" "test.*fail.js"))))))
    (inputs
     (list node-oop))
    (home-page "https://github.com/felixge/node-far")
    (synopsis "Node.js test runner")
    (description "This package provides a simple test runner that finds and runs
multiple node.js files, while providing useful information about output and exit
codes.")
    (license license:expat)))

(define-public node-file-uri-to-path
  (package
    (name "node-file-uri-to-path")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TooTallNate/file-uri-to-path")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08l779az44czm12xdhgcrnzpqw34s59hbrlfphs7g9y2k26drqav"))))
    (native-inputs
     (list esbuild))
    (build-system node-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           `("@types/mocha"
                             "@types/node"
                             "@typescript-eslint/eslint-plugin"
                             "@typescript-eslint/parser"
                             "cpy-cli"
                             "eslint"
                             "eslint-config-airbnb"
                             "eslint-config-prettier"
                             "eslint-import-resolver-typescript"
                             "eslint-plugin-import"
                             "eslint-plugin-jsx-a11y"
                             "eslint-plugin-react"
                             "mocha"
                             "rimraf"
                             "typescript")))))
         (replace 'build
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (copy-recursively "src" "dist")
             (invoke (search-input-file (or native-inputs inputs)
                                        "/bin/esbuild")
                     "dist/index.ts"
                     "--outfile=dist/src/index.js"
                     "--format=cjs"
                     "--sourcemap"
                     "--platform=node"))))
       #:tests? #f))
    (home-page "https://github.com/TooTallNate/file-uri-to-path")
    (synopsis "Convert a @code{file:} URI to a file path")
    (description "This package provides a function to convert a @code{file:}
URI to a file path.  It accepts a @code{file:} URI and returns a file path
suitable for use with the @code{fs} module functions.")
    (license license:expat)))

(define-public node-global-gradle-clean
  (package
    (name "node-global-gradle-clean")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/VarunBarad/global-gradle-clean")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fhm4jimnf7bl5drgwg39mjh8x4rns15hl0fz3bnxi9ikc6dm02y"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f)) ;No tests.
    (home-page "https://github.com/VarunBarad/global-gradle-clean")
    (synopsis "Global Gradle Clean")
    (description
     "Global Gradle Clean is a Node.js package used to clean all gradle
projects under a given directory.  It uses the gradle wrapper to execute the
clean task of each project.")
    (license license:expat)))

(define-public node-ieee754
  (package
    (name "node-ieee754")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/feross/ieee754")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19rlg59lavnwsvbblhvrqwinz2wzqlxhddqpwrc3cyqkscjgza7i"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json
                       (delete-dependencies
                        '("airtap" "standard" "tape"))))))))
    (home-page "https://github.com/feross/ieee754")
    (synopsis "Read/write IEEE754 floating point numbers in Javascript")
    (description "This package can read and write IEEE754 floating point
numbers from/to a Buffer or array-like object in Javascript.")
    (license license:bsd-3)))

(define-public node-inherits
  (package
    (name "node-inherits")
    (version "2.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/isaacs/inherits")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cpsr5yqwkxpbbbbl0rwk4mcby6zbx841k2zb4c3gb1579i5wq9p"))))
    (build-system node-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies '("tap"))))))
       ;; FIXME: Tests depend on node-tap
       #:tests? #f))
    (home-page "https://github.com/isaacs/inherits")
    (synopsis "Browser-friendly object inheritance")
    (description "This package provides an alternative implementation of
Node's @code{inherits} constructor that can be used in browsers, while
defaulting to Node's implementation otherwise.")
    (license license:isc)))

(define-public node-irc
  (package
    (name "node-irc")
    (version "0.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/martynsmith/node-irc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ln4qfx20jbwg4cp8lp0vf27m5281z2sz16d15xd6150n26cbi4x"))))
    (build-system node-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json
              (delete-dependencies
               `("ansi-color" "faucet" "jscs" "tape"))))))
       #:tests? #f))
    (inputs
     (list node-irc-colors))
    (home-page "https://github.com/martynsmith/node-irc")
    (synopsis "IRC client library for Node.js")
    (description "@code{node-irc} is an IRC client library for Node.js.
It has functions for joining, parting, talking, and many other IRC commands.")
    (license license:gpl3+)))

(define-public node-irc-colors
  (package
    (name "node-irc-colors")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fent/irc-colors.js")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q3y34rbnlc55jcakmdxkicwazyvyph9r6gaf6hi8k7wj2nfwfli"))))
    (build-system node-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies `("istanbul" "vows"))))))
       #:tests? #f))
    (home-page "https://github.com/fent/irc-colors.js")
    (synopsis "Node.js module providing color and formatting for IRC")
    (description "@code{node-irc-colors} is a Node.js module that
allows you to easily use colored output and formatting in IRC bots.
It contains functions for colours as well as more complex formatting
such as rainbows.")
    (license license:expat)))

(define-public node-long-stack-traces
  (package
    (name "node-long-stack-traces")
    (version "0.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tlrobinson/long-stack-traces")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0famwsyc6xawi30v25zi65d8fhbvlvh976bqydf1dqn5gz200cl3"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (home-page "https://github.com/tlrobinson/long-stack-traces")
    (synopsis "Long stacktraces implemented in user-land JavaScript")
    (description "This package provides long stacktraces for V8 implemented in
user-land JavaScript.")
    (license license:expat))) ; in README

(define-public node-mersenne
  (package
    (name "node-mersenne")
    (version "0.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jwatte/node-mersenne")
               ;; The actual release lacks a git tag.
               (commit "f9fa01694ee49d6ae6ff9d90cfda594bddd3ccef")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "034iaiq2pdqn342p2404cpz364g282d2hkp9375hysnh9i968wbb"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (home-page "https://www.enchantedage.com/node-mersenne")
    (synopsis "Node.js module for generating Mersenne Twister random numbers")
    (description "This package provides a node.js port of the Mersenne Twister
random number generator.")
    (license license:bsd-3)))

(define-public node-minimist
  (package
    (name "node-minimist")
    (version "1.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/substack/minimist")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mxj40mygbiy530wskc8l28wxb6fv3f8vrhpwjgprymhpgbaac7d"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json (delete-dependencies
                                    '("covert" "tap" "tape"))))))))
    (home-page "https://github.com/substack/minimist")
    (synopsis "Parse CLI arguments in Javascript")
    (description "This package can scan for CLI flags and arguments in
Javascript.")
    (license license:expat)))

(define-public node-ms
  (package
    (name "node-ms")
    (version "2.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vercel/ms")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l74kmmwffmzdz38lli0v5mdb9p9jmsjxpb48ncknqw2n74cgf08"))))
    (build-system node-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           `("eslint"
                             "expect.js"
                             "husky"
                             "lint-staged"
                             "mocha"
                             "prettier"))))))
       #:tests? #f))
    (home-page "https://github.com/vercel/ms")
    (synopsis "Convert time to milliseconds")
    (description "Use this package to easily convert various time formats to
milliseconds.  A number supplied as integer or string is returned as-is, while
a string consisting of a number and a time unit is converted to milliseconds.")
    (license license:expat)))

(define-public node-nan
  (package
    (name "node-nan")
    (version "2.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nodejs/nan")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02gqm23x26glffvyxrnk610hy3hg0kwh2v58dhnb032l0jhjzqvp"))))
    (build-system node-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           '("bindings"
                             "commander"
                             "glob"
                             "request"
                             "node-gyp" ;; would be needed for tests
                             "tap"
                             "xtend"))))))
       ;; tests need tap and other dependencies
       #:tests? #f))
    (inputs
     (list node-readable-stream))
    (home-page "https://github.com/nodejs/nan")
    (synopsis "Native Abstractions for Node.js")
    (description "Native Abstractions for Node.js (``NaN'') provides a header
file filled with macros and utilities for making add-on development for Node.js
easier across versions.  The goal is to provide all logic necessary to develop
native Node.js addons without having to inspect @code{NODE_MODULE_VERSION}.")
    (license license:expat)))

(define-public node-normalize-path
  (package
    (name "node-normalize-path")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonschlinkert/normalize-path")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l079szbgw2b9i5zx6zbwvxiivssa55a4pwfy4m7n6rdkcmsxf7f"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json (delete-dependencies
                                    '("gulp-format-md" "mocha"))))))))
    (native-inputs (list node-minimist))
    (home-page "https://github.com/jonschlinkert/normalize-path")
    (synopsis "Normalize slashes in a file path")
    (description
     "Normalize slashes in a file path to be POSIX/Unix-like forward slashes.
Can also condense repeated slashes to a single slash and remove trailing
slashes, unless disabled.")
    (license license:expat)))

(define-public node-once
  (package
    (name "node-once")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/isaacs/once")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z8dcbf28dqdcp4wb0c53wrs90a07nkrax2c9kk26dsk1dhrnxav"))))
    (build-system node-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies '("tap"))))))
       ;; FIXME: Tests depend on node-tap
       #:tests? #f))
    (inputs
     (list node-wrappy))
    (home-page "https://github.com/isaacs/once")
    (synopsis "Node.js module to call a function only once")
    (description
     "@code{once} is a Node.js module to call a function exactly one time.
Subsequent calls will either return the cached previous value or throw an error
if desired.")
    (license license:isc)))

(define-public node-oop
  ;; No releases, last commit was February 2013.
  (let ((commit "f9d87cda0958886955c14a0a716e57021ed295dc")
        (revision "1"))
    (package
      (name "node-oop")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/felixge/node-oop")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0mqrcf0xi2jbwffwkk00cljpqfsri1jk8s6kz8jny45apn7zjds1"))))
      (build-system node-build-system)
      (arguments '(#:tests? #f)) ; Tests run during build phase.
      (home-page "https://github.com/felixge/node-oop")
      (synopsis "Simple, light-weight oop module for Node")
      (description "This library tries to bring basic oop features to JavaScript
while being as light-weight and simple as possible.")
      (license license:expat))))

(define-public node-path-key
  (package
    (name "node-path-key")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sindresorhus/path-key")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09f4rf70qhq234fcc3pw9nrqic8cb75pk2b6wfnpr96v0r1h8d8g"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json
                       (delete-dependencies
                        '("@types/node" "ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/path-key")
    (synopsis "Cross-platform utility to compute the PATH environment variable key")
    (description "@code{path-key} provides an implementation to compute the
particular cross-platform spellings of the PATH environment variable key.")
    (license license:expat)))

(define-public node-pbf
  (package
    (name "node-pbf")
    (version "3.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mapbox/pbf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r8xs787ix79yr0vrwrizdml9h7cmxjrzhvnhkj784ac5f8nv5j7"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json (delete-dependencies
                                    '("benchmark"
                                      "browserify"
                                      "eslint"
                                      "eslint-config-mourner"
                                      "mkdirp"
                                      "protobufjs"
                                      "protocol-buffers"
                                      "tap"
                                      "tile-stats-runner"
                                      "uglify-js"))))))))
    (inputs (list node-ieee754 node-resolve-protobuf-schema))
    (home-page "https://github.com/mapbox/pbf")
    (synopsis "Decode and encode protocol buffers in Javascript")
    (description
     "This package is a low-level, fast and lightweight JavaScript library
for decoding and encoding protocol buffers, a compact binary format for
structured data serialization.  Works both in Node and the browser.
It supports lazy decoding and detailed customization of the reading/writing
code.")
    (license license:bsd-3)))

(define-public node-protocol-buffers-schema
  (package
    (name "node-protocol-buffers-schema")
    (version "3.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mafintosh/protocol-buffers-schema")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lnckxj14jzsnfxdd5kmlwrac43c214bv8i2g5rdldymlpxzrz1v"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json (delete-dependencies
                                    '("standard" "tape"))))))))
    (home-page "https://github.com/mafintosh/protocol-buffers-schema")
    (synopsis "Protocol buffers schema parser written in Javascript")
    (description "This package provides a protocol buffers schema parser
written in Javascript.")
    (license license:expat)))

(define-public node-readable-stream
  (package
    (name "node-readable-stream")
    (version "3.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nodejs/readable-stream")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ybl4cdgsm9c5jq3xq8s01201jk8w0yakh63hlclsfbcdfqhd9ri"))))
    (build-system node-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           `("@babel/cli"
                             "@babel/core"
                             "@babel/polyfill"
                             "@babel/preset-env"
                             "airtap"
                             "assert"
                             "bl"
                             "deep-strict-equal"
                             "events.once"
                             "glob"
                             "gunzip-maybe"
                             "hyperquest"
                             "lolex"
                             "nyc"
                             "pump"
                             "rimraf"
                             "tap"
                             "tape"
                             "tar-fs"
                             "util-promisify"))))))
       #:tests? #f))
    (inputs (list node-util-deprecate node-string-decoder node-inherits))
    (home-page "https://github.com/nodejs/readable-stream")
    (synopsis "Node.js core streams for userland")
    (description
     "@code{readable-stream} provides an implementation of Node.js core streams
that behaves the same across different versions.")
    (license license:expat)))

(define-public node-resolve-protobuf-schema
  (package
    (name "node-resolve-protobuf-schema")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mafintosh/resolve-protobuf-schema")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zxavr0b2yz9xzp6zlsg5g09i0a6zqb24j12rdvfgph6wd4mzk40"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json (delete-dependencies
                                    '("standard" "tape"))))))))
    (inputs (list node-protocol-buffers-schema))
    (home-page "https://github.com/mafintosh/resolve-protobuf-schema")
    (synopsis "Resolve protobuf imports")
    (description
     "This package can read a protobuf schema from the disk, parse it and
resolve all imports.")
    (license license:expat)))

(define-public node-safe-buffer
  (package
    (name "node-safe-buffer")
    (version "5.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/feross/safe-buffer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r26m0nl41h90ihnl2xf0cqs6z9z7jb87dl5j8yqb7887r9jlbpi"))))
    (build-system node-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies '("tape" "standard"))))))
       #:tests? #f))
    (home-page "https://github.com/feross/safe-buffer")
    (synopsis "Buffer creation with explicit semantics")
    (description "This package provides a drop-in replacement for Node.js
@code{Buffer} API, which provides newer, explicit constructors (such as
@code{Buffer.alloc(SIZE)}) in older versions.")
    (license license:expat)))

(define-public node-safe-stable-stringify
  (package
    (name "node-safe-stable-stringify")
    (version "2.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BridgeAR/safe-stable-stringify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "008adig8j13rn2a21ngnp770y4zz6yq176ix5rkskjbb8g2qwapg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json (delete-dependencies
                                    '("benchmark"
                                      "clone"
                                      "fast-json-stable-stringify"
                                      "fast-safe-stringify"
                                      "fast-stable-stringify"
                                      "faster-stable-stringify"
                                      "fastest-stable-stringify"
                                      "json-stable-stringify"
                                      "json-stringify-deterministic"
                                      "json-stringify-safe"
                                      "standard"
                                      "tap"
                                      "typescript"
                                      "@types/node"
                                      "@types/json-stable-stringify"))))))))
    (home-page "https://github.com/BridgeAR/safe-stable-stringify")
    (synopsis "Serialization of javascript objects")
    (description
     "Safe, deterministic and fast serialization alternative to JSON.stringify.
Gracefully handles circular structures and bigint instead of throwing.
Optional custom circular values, deterministic behavior or strict JSON
compatibility check.")
    (license license:expat)))

(define-public node-segfault-handler
  (package
    (name "node-segfault-handler")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ddopson/node-segfault-handler")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07nbw35wvrr18kmh8f388v4k5mpjgyy0260bx0xzjdv795i3xvfv"))))
    (native-inputs
     (list python))
    (inputs
     (list node-bindings node-nan))
    (build-system node-build-system)
    (arguments
     ;; there are no tests
     `(#:tests? #f))
    (home-page "https://github.com/ddopson/node-segfault-handler")
    (synopsis "Catches @code{SIGSEGV} and prints diagnostic information")
    (description "This package is a tool for debugging Node.js C/C++ native
code modules and getting stack traces when things go wrong.  If a
@code{SIGSEGV} signal is raised, the module will print a native stack trace to
both @file{stderr} and to a timestamped file.")
    (license license:bsd-3)))

(define-public node-semver
  (package
    (name "node-semver")
    (version "7.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/npm/node-semver")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06biknqb05r9xsmcflm3ygh50pjvdk84x6r79w43kmck4fn3qn5p"))))
    (build-system node-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies '("tap"))))))
       ;; FIXME: Tests depend on node-tap
       #:tests? #f))
    (home-page "https://github.com/npm/node-semver")
    (synopsis "Parses semantic versions strings")
    (description
     "@code{node-semver} is a JavaScript implementation of the
@uref{https://semver.org/, SemVer.org} specification.")
    (license license:isc)))

(define-public node-serialport
  (package
    (name "node-serialport")
    (version "9.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/serialport/node-serialport")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x7zm59a5ff5yygjyw15xs3r5m3rb8av1yfrh4snn44mrwq87yg8"))))
    (inputs
     (list node-serialport-bindings
           node-serialport-parser-delimiter
           node-serialport-parser-readline
           node-serialport-parser-regex
           node-serialport-parser-ready
           node-serialport-parser-inter-byte-timeout
           node-serialport-parser-cctalk
           node-serialport-parser-byte-length
           node-serialport-stream
           node-debug))
    (build-system node-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           `("@serialport/binding-mock")))))
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/serialport"))))
       #:tests? #f))
    (home-page "https://serialport.io")
    (synopsis "Node.js package to access serial ports")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  This package is the recommended entry point for most
projects.  It combines a high-level Node.js stream interface with a useful
default set of parsers and bindings.")
    (license license:expat)))

(define-public node-serialport-binding-abstract
  (package
    (inherit node-serialport)
    (name "node-serialport-binding-abstract")
    (version "9.2.3")
    (inputs
     (list node-debug))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/binding-abstract"))))
       #:tests? #f))
    (synopsis "Abstract base class for Node SerialPort bindings")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

This package provides the @code{AbstractBinding} class, the base for all Node
SerialPort bindings.  You wouldn't use this class directly, but instead extend
it to make a new binding for a different platform or underling technology.")))

(define-public node-serialport-bindings
  (package
    (inherit node-serialport)
    (name "node-serialport-bindings")
    (version "9.2.7")
    (native-inputs
     (list python))
    (inputs
     (list node-nan node-bindings node-serialport-binding-abstract
           node-serialport-parser-readline node-debug))
    (arguments
     `(#:modules
       ((guix build node-build-system)
        (srfi srfi-1)
        (ice-9 match)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/bindings")))
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies `("node-abi"))))))
       #:tests? #f))
    (synopsis "Abstract base class for Node SerialPort bindings")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

This package provides the @code{Binding} class, which uses a native addon to
talk to the underlying system.  You never have to use @code{Binding} objects
directly.  There is also a @code{MockBinding} available (but not yet packaged
for Guix) to assist with testing.")))

(define-public node-serialport-parser-byte-length
  (package
    (inherit node-serialport)
    (name "node-serialport-parser-byte-length")
    (version "9.2.4")
    (inputs `())
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/parser-byte-length"))))
       #:tests? #f))
    (synopsis "Node SerialPort parser for fixed-length buffers")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

Parsers are used to take raw binary data and transform them into usable
messages.  This package provides @code{ByteLength}, a parser that emits data
as a buffer every time a specified number of bytes are received.")))

(define-public node-serialport-parser-cctalk
  (package
    (inherit node-serialport)
    (name "node-serialport-parser-cctalk")
    (version "9.2.4")
    (inputs `())
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/parser-cctalk"))))
       #:tests? #f))
    (synopsis "Node SerialPort parser for the ccTalk protocol")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

Parsers are used to take raw binary data and transform them into usable
messages.  This package provides @code{CCTalk}, which emits packets for the
ccTalk protocol (an open standard for currency detectors) as they are
received.")))

(define-public node-serialport-parser-delimiter
  (package
    (inherit node-serialport)
    (name "node-serialport-parser-delimiter")
    (version "9.2.4")
    (inputs `())
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/parser-delimiter"))))
       #:tests? #f))
    (synopsis "Node SerialPort parser to split data on a delimiter")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

Parsers are used to take raw binary data and transform them into usable
messages.  This package provides @code{Delimiter}, a parser that emits data
each time a specified byte sequence is received.")))

(define-public node-serialport-parser-inter-byte-timeout
  (package
    (inherit node-serialport)
    (name "node-serialport-parser-inter-byte-timeout")
    (version "9.2.4")
    (inputs `())
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/parser-inter-byte-timeout"))))
       #:tests? #f))
    (synopsis "Node SerialPort parser to detect pauses in data")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

Parsers are used to take raw binary data and transform them into usable
messages.  This package provides @code{InterByteTimeout}, a parser that emits
data if there is a pause between packets for the specified amount of time.")))

(define-public node-serialport-parser-readline
  (package
    (inherit node-serialport)
    (name "node-serialport-parser-readline")
    (version "9.2.4")
    (inputs
     (list node-serialport-parser-delimiter))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/parser-readline"))))
       #:tests? #f))
    (synopsis "Node SerialPort parser to split data on newlines")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

Parsers are used to take raw binary data and transform them into usable
messages.  This package provides @code{Readline}, a parser that emits data
after a (configurable) newline delimiter is received.")))

(define-public node-serialport-parser-ready
  (package
    (inherit node-serialport)
    (name "node-serialport-parser-ready")
    (version "9.2.4")
    (inputs `())
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/parser-ready"))))
       #:tests? #f))
    (synopsis "Node SerialPort parser to wait for specified byte sequence")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

Parsers are used to take raw binary data and transform them into usable
messages.  This package provides @code{Ready}, a parser that waits for a
specified sequence of ``ready'' bytes before emitting a ready event and
emitting data events.")))

(define-public node-serialport-parser-regex
  (package
    (inherit node-serialport)
    (name "node-serialport-parser-regex")
    (version "9.2.4")
    (inputs `())
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/parser-regex"))))
       #:tests? #f))
    (synopsis "Node SerialPort parser to split data on a regular expression")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

Parsers are used to take raw binary data and transform them into usable
messages.  This package provides @code{Regex}, a parser that uses a regular
expression to split the incoming text.")))

(define-public node-serialport-stream
  (package
    (inherit node-serialport)
    (name "node-serialport-stream")
    (version "9.2.4")
    (inputs
     (list node-debug))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json
               (delete-dependencies `("@serialport/binding-mock")))))
         (add-after 'unpack 'chdir
           (lambda args
             (chdir "packages/stream"))))
       #:tests? #f))
    (synopsis "Node.js stream interface for Node SerialPort")
    (description "Node SerialPort is a modular suite of Node.js packages for
accessing serial ports.  The Guix package @code{node-serialport} provides the
recommended high-level interface.

This package provides an interface for using Node SerialPort bindings via the
Node.js Stream API.  The stream is a duplex stream, allowing for reading and
writing.  It has additional methods for managing the SerialPort
connection.")))

(define-public node-sqlite3
  (package
    (name "node-sqlite3")
    (version "5.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mapbox/node-sqlite3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sbbzzli282nxyfha10zx0k5m8hdp0sf3ipl59khjb7wm449j86h"))
       (snippet
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils))
              ;; unbundle sqlite
              (for-each delete-file-recursively
                        (find-files "deps"
                                    (lambda (pth stat)
                                      (gzip-file? pth)))))))))
    (inputs
     (list node-addon-api python sqlite))
    (build-system node-build-system)
    (arguments
     `(#:modules
       ((guix build node-build-system)
        (srfi srfi-1)
        (ice-9 match)
        (guix build utils))
       #:tests? #f ; FIXME: tests depend on node-mocha
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json
              (delete-dependencies
               `(;; Normally, this is "built" using @mapbox/node-pre-gyp,
                 ;; which publishes or downloads pre-built binaries or
                 ;; falls back to building from source.  Here, we patch out
                 ;; all of that and just build directly.  It might be
                 ;; better to patch a version of @mapbox/node-pre-gyp that
                 ;; always builds from source, as Debian does, but there
                 ;; are a number of dependencies that need to be packaged
                 ;; or removed.
                 "@mapbox/node-pre-gyp"
                 "node-pre-gyp" ;; deprecated name still used in some places
                 "aws-sdk"
                 "@mapbox/cloudfriend"
                 ;; Confusingly, this is only a dependency because of
                 ;; @mapbox/node-pre-gyp: with that removed,
                 ;; npm will use its own copy:
                 "node-gyp"
                 ;; These we'd like, we just don't have them yet:
                 "eslint"
                 "mocha")))))
         (add-before 'configure 'npm-config-sqlite
           ;; We need this step even if we do replace @mapbox/node-pre-gyp
           ;; because the package expects to build its bundled sqlite
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "npm_config_sqlite" (assoc-ref inputs "sqlite"))))
         (add-after 'install 'patch-binding-path
           ;; We replace a file that dynamic searches for the addon using
           ;; node-pre-gyp (which we don't have) with a version that
           ;; simply uses the path to the addon we built directly.
           ;; The exact path is supposed to depend on things like the
           ;; architecture and napi_build_version, so, to avoid having
           ;; hard-code the details accurately, we do this after the addon
           ;; has been built so we can just find where it ended up.
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion
                 (search-input-directory outputs
                                         "lib/node_modules/sqlite3/lib")
               (match (find-files "binding" "\\.node$")
                 ((rel-path)
                  (with-atomic-file-replacement "sqlite3-binding.js"
                    (lambda (in out)
                      (format out "var binding = require('./~a');\n" rel-path)
                      (display "module.exports = exports = binding;\n"
                               out))))))))
         (add-after 'patch-dependencies 'avoid-node-pre-gyp
           (lambda args
             ;; We need to patch .npmignore before the 'repack phase
             ;; so that the built addon is installed with in the package.
             ;; (Upstream assumes node-pre-gyp will download a pre-built
             ;; version when this package is installed.)
             (substitute* ".npmignore"
               (("lib/binding")
                "#lib/binding # <- patched for Guix"))
             (modify-json
               (lambda (pkg-meta-alist)
                 (let ((binary-alist (assoc-ref pkg-meta-alist "binary")))
                 ;; When it builds from source, node-pre-gyp supplies
                 ;; module_name and module_path based on the entries under
                 ;; "binary" from "package.json", so this package's
                 ;; "binding.gyp" doesn't define them. Thus, we also need
                 ;; to supply them. The GYP_DEFINES environment variable
                 ;; turns out to be the easiest way to make sure they are
                 ;; propagated from npm to node-gyp to gyp.
                 (setenv "GYP_DEFINES" (string-append
                   "module_name="
                   (assoc-ref binary-alist "module_name")
                   " module_path="
                   (assoc-ref binary-alist "module_path"))))
                 pkg-meta-alist)
               ;; We need to remove the install script from "package.json",
               ;; as it would try to use node-pre-gyp and would block the
               ;; automatic building performed by `npm install`.
               (delete-fields `(("scripts" "install")))))))))
    (home-page "https://github.com/mapbox/node-sqlite3")
    (synopsis "Node.js bindings for SQLite3")
    (description
     "@code{node-sqlite3} provides a set of a Node.js bindings for interacting
with SQLite3 databases.")
     (license license:bsd-3)))

(define-public node-stack-trace
  ;; There have been improvements since the last release.
  (let ((commit "4fd379ee78965ce7ce8820b436f1b1b590d5dbcf")
        (revision "1"))
    (package
      (name "node-stack-trace")
      (version (git-version "0.0.10" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/felixge/node-stack-trace")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1pk19wcpy8i95z5jr77fybd57qj7xmzmniap4dy47vjlmpkqia4i"))))
      (build-system node-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
         (add-before 'check 'skip-intentionally-failing-test
           (lambda _
             (substitute* "test/run.js"
               (("far.include") "far.exclude(/test-parse.js/)\nfar.include"))
             #t)))))
      (native-inputs
       (list node-far node-long-stack-traces))
      (home-page "https://github.com/felixge/node-stack-trace")
      (synopsis "Get v8 stack traces as an array of CallSite objects")
      (description "Get v8 stack traces as an array of CallSite objects.")
      (license license:expat))))

(define-public node-statsd-parser
  (package
    (name "node-statsd-parser")
    (version "0.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dscape/statsd-parser")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "049rnczsd6pv6bk282q4w72bhqc5cs562djgr7yncy7lk0wzq5j3"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (home-page "https://github.com/dscape/statsd-parser")
    (synopsis "Streaming parser for the statsd protocol")
    (description "This package provides a streaming parser for the statsd
protocol used in @code{node-lynx}.")
    (license license:asl2.0)))

(define-public node-string-decoder
  (package
    (name "node-string-decoder")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nodejs/string_decoder")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xxvyya9fl9rlkqwmxzqzbz4rdr3jgw4vf37hff7cgscxkhg266k"))))
    (build-system node-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json
              (delete-dependencies
               '("tap" "core-util-is" "babel-polyfill"))))))
       ;; FIXME: Tests depend on node-tap
       #:tests? #f))
    (inputs (list node-safe-buffer node-inherits))
    (home-page "https://github.com/nodejs/string_decoder")
    (synopsis "Decode buffers while preserving multi-byte sequences")
    (description "This package provides a user-land implementation of
Node-core's @code{string_decoder}, which serves to decode buffers to
strings so that the decoded string does not contain incomplete multibyte
sequences.")
    (license license:expat)))

(define-public node-tiddlywiki
  (package
    (name "node-tiddlywiki")
    (version "5.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TiddlyWiki/TiddlyWiki5")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x5rr1phzvymys8rwkxl1xv377j8c5dvl89yjf702adr2yfhq848"))))
    (build-system node-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (delete 'build)
                   (add-after 'patch-dependencies 'delete-dev-dependencies
                     (lambda _
                       (modify-json
                        (delete-dependencies '("eslint" "@eslint/js"))))))))
    (home-page "https://github.com/TiddlyWiki/TiddlyWiki5#readme")
    (synopsis "Non-linear personal web notebook")
    (description "TiddlyWiki is a unique non-linear notebook for capturing,
organising and sharing complex information.  Running TiddlyWiki on Node.js brings
several important benefits over and above the single file version:
@enumerate
@item Individual tiddlers are stored in separate files, which you can organise as you
wish.
@item The ability to build multiple wikis that blend different combinations of shared
and unique content.
@item You can edit your content on any modern browser, including smartphones and
tablets.
@end enumerate")
    (license license:bsd-3)))

(define-public node-util-deprecate
  (package
    (name "node-util-deprecate")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/TooTallNate/util-deprecate")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1rk94nl3qc7znsk8400bnga30v0m7j2mmvz9ldwjinxv1d3n11xc"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f)) ; No test suite.
    (home-page "https://github.com/TooTallNate/util-deprecate")
    (synopsis "Node.js `util.deprecate()` function with browser support")
    (description "This package provides the Node.js @code{util.deprecate()}
function with browser support.")
    (license license:expat)))

(define-public node-wrappy
  (package
    (name "node-wrappy")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/npm/wrappy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ymlc61cja6v5438vwb04gq8wg2b784lj39zf0g4i36fvgcw9783"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f ; FIXME: Tests depend on node-tap
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies '("tap"))))))))
    (home-page "https://github.com/npm/wrappy")
    (synopsis "Callback wrapping utility")
    (description "@code{wrappy} is a utility for Node.js to wrap callbacks.")
    (license license:isc)))

(define-public node-yazl
  (package
    (name "node-yazl")
    (version "2.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/thejoshwolfe/yazl")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lhwqqnvazpi4xw81ldpx0ky0h1j5rcx3br480q2bnzj21cm109n"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dependencies
                    (lambda _
                      (modify-json
                       (delete-dependencies
                        '("airtap" "bl" "istanbul" "yauzl"))))))))
    (inputs (list node-buffer-crc32))
    (home-page "https://github.com/thejoshwolfe/yazl")
    (synopsis "Yet another zip library for node")
    (description
     "This package provides a zip library for Node.  It follows the
following principles:
@enumerate
@item Don't block the JavaScript thread.  Use and provide async APIs.
@item Keep memory usage under control.  Don't attempt to buffer entire
files in RAM at once.
@item Prefer to open input files one at a time than all at once.
@end enumerate")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
