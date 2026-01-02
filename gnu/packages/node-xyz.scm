;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2021 Noisytoot <noisytoot@disroot.org>
;;; Copyright © 2021 Charles <charles.b.jackson@protonmail.com>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 Dhruvin Gandhi <contact@dhruvin.dev>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2024-2026 Daniel Khodabakhsh <d@niel.khodabakh.sh>
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
  #:use-module (gnu packages base) ; sed
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web) ; node-esbuild
  #:use-module (guix build-system node)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

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
    (version "8.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nodejs/node-addon-api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18sbs3n69p2qax88dji6704a2jlq1af74k4i61s7pja7xc8gs1vd"))))
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

(define-public node-agent-base
  (package
    (name "node-agent-base")
    (version "7.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TooTallNate/proxy-agents")
             (commit (string-append "agent-base@" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0svw5z3j2icg7lcyv6cnk1a5n11fc8kzig11j2bhiq9wnclv96r0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'change-directory
            (lambda _
              (chdir "packages/agent-base")))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies))))
          (replace 'build
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let ((esbuild (search-input-file (or native-inputs inputs) "/bin/esbuild")))
                (mkdir-p "dist")
                (for-each
                 (lambda (ts-file)
                   (let* ((base (basename ts-file ".ts"))
                          (js-file (string-append "dist/" base ".js")))
                     (invoke esbuild ts-file
                             (string-append "--outfile=" js-file)
                             "--format=cjs"
                             "--platform=node"
                             "--target=es2020")))
                 (find-files "src" "\\.ts$"))))))))
    (native-inputs (list esbuild))
    (home-page "https://github.com/TooTallNate/proxy-agents")
    (synopsis "Turn a function into an http.Agent instance")
    (description "This package provides a base class for creating Node.js
HTTP.Agent instances from a function.")
    (license license:expat)))

(define-public node-ansi-styles
  (package
    (name "node-ansi-styles")
    (version "3.2.1")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/chalk/ansi-styles")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "15b5ggrhxi2zw5qlhr2di1b7rmfyacrl4rf8j3ndf8iqkv9fijqd"))))
    (build-system node-build-system)
    (inputs (list
      node-color-convert))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'xo', 'ava', and 'tsd'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "ANSI escape codes for styling strings in the terminal")
    (description "Library of ANSI escape codes to be used to style strings in terminals.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-array-back
  (package
    (name "node-array-back")
    (version "4.0.2")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/75lb/array-back")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1xfg4yd155zhi0v2a26kvg3jghcim5xgpwsn4lhikisshmrmf93m"))
      (modules '((guix build utils)))
      (snippet #~(begin
        (delete-file-recursively "dist")))))
    (build-system node-build-system)
    (native-inputs (list esbuild))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'esm-runner'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies))))
        (replace 'build (lambda _
          (invoke
            "esbuild"
            "index.mjs"
            "--format=cjs"
            "--outfile=dist/index.js"))))))
    (synopsis "Isomorphic load-anywhere arrayify function")
    (description "Takes any input and guarantees an array back.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-balanced-match
  (package
    (name "node-balanced-match")
    (version "1.0.2")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/juliangruber/balanced-match")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256 (base32 "0977r6hv9fyv6f8wvn31vcncxwhffnn05y0h4hmpkg8p2vs9ip0b"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME Tests require 'tape'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package
          (lambda _
            (modify-json
              (delete-dev-dependencies)))))))
    (synopsis "Match balanced character pairs, like { and }")
    (description "Match balanced string pairs, like { and } or <b> and </b>. Supports\
 regular expressions as well!")
    (home-page (git-reference-url (origin-uri source)))
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

(define-public node-brace-expansion-1
  (package
    (name "node-brace-expansion")
    (version "1.1.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/juliangruber/brace-expansion")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "117k5p167k4sz739rr83cjhf7bsq0iidvm8ylvnybbj86varv9q1"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list node-balanced-match-1 node-concat-map))
    (home-page "https://github.com/juliangruber/brace-expansion")
    (synopsis "Brace expansion for JavaScript")
    (description "This package provides brace expansion as known from
sh/bash, for JavaScript.")
    (license license:expat)))

(define-public node-brace-expansion
  (package
    (name "node-brace-expansion")
    (version "2.0.1")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/juliangruber/brace-expansion")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256 (base32 "113banahmr00n1fnyswdsx2jgpfi05f4hn2gc5i8kka7hmj3b5g1"))))
    (build-system node-build-system)
    (inputs (list node-balanced-match))
    (arguments (list
      #:tests? #f ; FIXME Tests require 'tape'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package
          (lambda _
            (modify-json (delete-dev-dependencies)))))))
    (synopsis "Brace expansion as known from sh/bash, in JavaScript")
    (description "Return an array of all possible and valid expansions of str. If none\
 are found, [str] is returned.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-balanced-match-1
  (package
    (name "node-balanced-match")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/juliangruber/balanced-match")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0977r6hv9fyv6f8wvn31vcncxwhffnn05y0h4hmpkg8p2vs9ip0b"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/juliangruber/balanced-match")
    (synopsis "Match balanced string pairs in JavaScript")
    (description "This package provides a way to match balanced string pairs.")
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

(define-public node-buffer-from
  (package
    (name "node-buffer-from")
    (version "1.1.2")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/LinusU/buffer-from")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256 (base32 "15x3iix1z2ggfq3gmnjnz809k02g0zbkf391g1if8s7d3q0r0w1b"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'standard'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "A ponyfill for Buffer.from")
    (description "A ponyfill for Buffer.from, uses native implementation if available.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-bufferutil
  (package
    (name "node-bufferutil")
    (version "4.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/websockets/bufferutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00d6qsrhzv91m72xv9zx1vmlwfx22avwvmxz4yclwsj37y8imd5s"))))
    (build-system node-build-system)
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'patch-dependencies 'delete-dev-dependencies
                 (lambda _
                   (modify-json (delete-dev-dependencies))))
               (add-before 'install 'set-cc
                 (lambda _
                   (setenv "CC" #$(cc-for-target)))))))
    (inputs
     (list node-gyp-build node-addon-api))
    (native-inputs
     (list python))
    (home-page "https://github.com/websockets/bufferutil")
    (synopsis "WebSocket buffer utilities for Node.js")
    (description "This package provides an addon for Node.js for operations
on buffers for use with WebSocket implementations.  It provides efficient
buffer masking and unmasking operations.")
    (license license:expat)))

(define-public node-chalk
  (package
    (name "node-chalk")
    (version "2.4.2")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/chalk/chalk")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1isx2l8a11k5arz1p3kd7mbdb8kjr85l1axdm7yya57vwlkdkc2y"))))
    (build-system node-build-system)
    (inputs (list
      node-ansi-styles
      node-supports-color
      node-escape-string-regexp))
    (arguments (list
      #:tests? #f ; FIXME: Tests reuire 'xo', 'c8', 'ava', and 'tsd'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Terminal string styling library")
    (description "Chalk comes with an easy to use composable API where you just chain and\
 nest the styles you want.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-color-convert
  (package
    (name "node-color-convert")
    (version "1.9.3")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/Qix-/color-convert")
        (commit version)))
      (file-name (git-file-name name version))
      (sha256
        (base32 "0apgv8p1y9hs5z42wwrwrid62vfkfb89kh3a75s9lvqdbyh04390"))))
    (build-system node-build-system)
    (inputs (list
      node-color-name))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'xo' and 'tsd'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Plain color conversion functions")
    (description "Color-convert is a color conversion library for JavaScript and node. It\
 converts all ways between rgb, hsl, hsv, hwb, cmyk, ansi, ansi16, hex strings, and CSS\
 keywords (will round to closest)")
    (home-page (git-reference-url (origin-uri source)))
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

(define-public node-colors
  (package
    (name "node-colors")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Marak/colors.js")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ih98ycxjprlxn72ygqgkgcp9wkpd20apndjd11270qyyifvkr8y"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/Marak/colors.js")
    (synopsis "Get colors in your Node.js console")
    (description "This package provides a library for adding colors to
Node.js console output.")
    (license license:expat)))

(define-public node-command-line-usage
  (package
    (name "node-command-line-usage")
    (version "6.1.3")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/75lb/command-line-usage")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "073blk28qdk1bl1l4jsd4a6fmwvl2jv4fi6kfcjykc8x6hkv84cx"))))
    (build-system node-build-system)
    (inputs (list
      node-chalk
      node-typical
      node-array-back
      node-table-layout))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'test-runner'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Generates command-line usage information")
    (description "A simple, data-driven module for creating a usage guide.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-commander
  (package
    (name "node-commander")
    (version "8.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tj/commander.js")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fwvjcmhj9vqav6s26bvxq7zbzd1v2gjc1n5j2wf4lbz1qqvy138"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/tj/commander.js")
    (synopsis "Complete solution for Node.js command-line interfaces")
    (description "This package provides a way for building command-line
interfaces in Node.js.")
    (license license:expat)))

(define-public node-concat-map
  (package
    (name "node-concat-map")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ljharb/concat-map")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l30hn1w9incwahjbvv3kzw6p150vjiiji6dlxxawd3krfn7z3k5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/ljharb/concat-map")
    (synopsis "Concatenative map for JavaScript")
    (description "This package provides a concatenative map for JavaScript.")
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
    (version "4.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/debug-js/debug")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06hm12vgqxc1bvfxp2rv95yyycr7xjj5482ykq2c4fq4pkjci827"))))
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

(define-public node-deep-extend
  (package
    (name "node-deep-extend")
    (version "0.6.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/unclechu/node-deep-extend")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "0r2r0wc1c9lq8gplis17pr220c7hk18sq12cipvsa7krb2lb4cpb"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'mocha'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Recursive object extending")
    (description "Library which allows deeply merging two different objects.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-define-lazy-prop
  (package
    (name "node-define-lazy-prop")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sindresorhus/define-lazy-prop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lbd1m9n77bpzjrp2m7670hiyd73qd8w9gnc2w5lxjf6gglwz0ay"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/sindresorhus/define-lazy-prop")
    (synopsis "Define a lazily evaluated property on an object")
    (description "This package provides a way to lazily evaluate properties
on an object.")
    (license license:expat)))

(define-public node-diff
  (package
    (name "node-diff")
    (version "7.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kpdecker/jsdiff")
             (commit version)))  ; No "v" prefix for this package
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dipv7nsvvzqpiw6i0zlgs82pbdryla01qzjh1wyvxwfjr5lra89"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies))))
          (replace 'build
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              ;; Compile JavaScript modules with esbuild.
              (let ((esbuild (search-input-file (or native-inputs inputs) "/bin/esbuild")))
                (mkdir-p "lib")
                ;; Build CommonJS version
                (invoke esbuild "src/index.js"
                        "--bundle"
                        "--outfile=lib/index.js"
                        "--format=cjs"
                        "--platform=node"
                        "--target=es2020")
                ;; Build ESM version (required by package.json exports)
                (invoke esbuild "src/index.js"
                        "--bundle"
                        "--outfile=lib/index.mjs"
                        "--format=esm"
                        "--platform=node"
                        "--target=es2020")))))))
    (native-inputs (list esbuild))
    (home-page "https://github.com/kpdecker/jsdiff")
    (synopsis "JavaScript text differencing implementation")
    (description "This package provides a JavaScript library for computing
text differences, similar to Unix diff.")
    (license license:bsd-3)))

(define-public node-dprint-formatter
  (package
    (name "node-dprint-formatter")
    (version "0.4.1")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/dprint/js-formatter")
        (commit version)))
      (file-name (git-file-name name version))
      (sha256
        (base32 "0ihdspyz7brx1rny2ind715z6rlzncslsigi3yr553xmk18iswm2"))))
    (build-system node-build-system)
    (native-inputs (list esbuild))
    (arguments (list
      #:modules '(
        (guix build node-build-system)
        (guix build utils)
        (ice-9 match)
        (json))
      #:tests? #f ; FIXME: Tests require 'deno'.
      #:phases #~(modify-phases %standard-phases
        (delete 'patch-dependencies)
        (delete 'configure)
        (replace 'build (lambda _
          (define output "output")
          (mkdir output)
          (delete-file "mod_test.ts")
          (substitute* (list "mod.ts" "v3.ts" "v4.ts")
            (("\\.ts") ""))
          (install-file "LICENSE" output)
          (install-file "README.md" output)
          (chdir output)
          (call-with-output-file
            "package.json"
            (lambda (out)
              (scm->json
                (list
                  (cons "name" "@dprint/formatter")
                  (cons "version" #$version)
                  (cons "main" "./script/mod.js")
                  (cons "module" "./esm/mod.js"))
                out)))
          (for-each
            (match-lambda ((format outdir)
              (invoke
                "esbuild"
                "../*.ts"
                (string-append "--format=" format)
                "--platform=node"
                (string-append "--outdir=" outdir))))
            (list (list "cjs" "script") (list "esm" "esm"))))))))
    (synopsis "Wasm formatter for dprint plugins")
    (description "A JS formatter which uses WASM plugins for dprint.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-dotenv
  (package
    (name "node-dotenv")
    (version "16.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/motdotla/dotenv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sbfq4d3p3zjlmf8vvr6dgzgxab0w8fcms6dk6v4n4y5ca7vjv93"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/motdotla/dotenv")
    (synopsis "Loads environment variables from .env files")
    (description "This package provides a zero-dependency module that loads
environment variables from a @code{.env} file into @code{process.env}.")
    (license license:bsd-2)))

(define-public node-end-of-stream
  (package
    (name "node-end-of-stream")
    (version "1.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mafintosh/end-of-stream")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0241fxmc30hzxcidyvkd0bylsdcq20nf9yy6v5cgiwp9xq7bpc3a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (modify-json (delete-dev-dependencies)))))))
    (inputs (list node-once))
    (home-page "https://github.com/mafintosh/end-of-stream")
    (synopsis "Call a callback when a stream has finished or errored")
    (description "This package provides a way so that a callback is called
when a readable/writable/duplex stream has completed or failed.")
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

(define-public node-escape-string-regexp
  (package
    (name "node-escape-string-regexp")
    (version "1.0.5")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/sindresorhus/escape-string-regexp")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "0iv0zd2r1mr3w966n023wqghcyaw37w3wl07is45hy74rrnq6z0v"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'xo' and 'ava'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Escape RegExp special characters")
    (description "This library takes a string and escapes the regex characters such that\
 the string can be inserted into a regex expression without triggering any special\
 characters.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-escape-string-regexp-4
  (package
    (name "node-escape-string-regexp")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sindresorhus/escape-string-regexp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0axhbb68f9vn87bccy3d49bglalrb52k66hgfixcdpz8l2yi4ms2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/sindresorhus/escape-string-regexp")
    (synopsis "Escape RegExp special characters")
    (description "This package provides a way to escape special characters
in a string for use in a regular expression.")
    (license license:expat)))

(define-public node-extract-zip
  (package
    (name "node-extract-zip")
    (version "2.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/maxogden/extract-zip")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nksca10cn7cv8ykhqxy2mdimpna1kvf6pqs9pdq3p1z4s7xbm27"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (modify-json (delete-dev-dependencies)))))))
    (inputs (list node-debug node-get-stream node-yauzl))
    (home-page "https://github.com/maxogden/extract-zip")
    (synopsis "Unzip written in pure JavaScript")
    (description "This package provides a cross-platform extraction utility
for ZIP files, written entirely in JavaScript for Node.js.")
    (license license:bsd-2)))

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

(define-public node-fast-xml-parser
  (package
    (name "node-fast-xml-parser")
    (version "4.5.3")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/NaturalIntelligence/fast-xml-parser")
        (commit "f8d4d427a31a52d4df82260ac1973d94ad73335b")))
      (file-name (git-file-name name version))
      (sha256 (base32 "1wc5mgihrgync0vj79gk7g367rssqhzmzxl41kdhhcya045bis85"))
      (modules '((guix build utils)))
      (snippet #~(begin
        (delete-file-recursively "lib")))))
    (build-system node-build-system)
    (inputs (list node-strnum))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'nyc'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies))))
        (replace 'build (lambda _
          (define output "output")
          (mkdir output)
          (for-each
            (lambda (file) (install-file file output))
            (list "CHANGELOG.md" "LICENSE" "package.json" "README.md"))
          (copy-recursively "src" (string-append output "/src"))
          (chdir output))))))
    (synopsis "Validate XML, Parse XML, Build XML without C/C++ based libraries")
    (description "Validate XML, Parse XML to JS Object, or Build XML from JS Object\
 without C/C++ based libraries and no callback.
  * Validate XML data syntactically. Use detailed-xml-validator to verify business rules.
  * Parse XML to JS Objectand vice versa
  * Common JS, ESM, and browser compatible
  * Faster than any other pure JS implementation.
It can handle big files (tested up to 100mb). XML Entities, HTML entities, and DOCTYPE\
 entites are supported. Unpaired tags (Eg <br> in HTML), stop nodes (Eg <script> in HTML)\
 are supported. It can also preserve Order of tags in JS object")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-fastest-levenshtein
  (package
    (name "node-fastest-levenshtein")
    (version "1.0.16")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/ka-weihe/fastest-levenshtein")
        (commit "03d621ba324d0f665b3b7f557429ca622560d9a3")))
      (file-name (git-file-name name version))
      (sha256
        (base32 "11hpi4ifix8pwx2a2sfj3s1gcwb5mxdrqgdcdppmkl6krv3sxx02"))))
    (build-system node-build-system)
    ; Use ESBuild because this package is used to build Typescript.
    (native-inputs (list esbuild))
    (arguments (list
      #:modules '(
        (guix build node-build-system)
        (guix build utils)
        (ice-9 match))
      #:tests? #f ; FIXME: Tests require 'jest'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)
            (delete-fields (list
              "scripts.prepare"
              "scripts.build")))))
        (replace 'build (lambda _
          (for-each
            (match-lambda ((format outdir parameters)
              (apply invoke (append
                (list
                  "esbuild"
                  "mod.ts"
                  (string-append "--format=" format)
                  "--target=es2015"
                  (string-append "--outdir=" outdir))
                parameters))))
            (list
              (list "esm" "esm" (list "--sourcemap"))
              (list "cjs" "." (list "--platform=node")))))))))
    (synopsis "Fastest Levenshtein distance implementation in JS")
    (description "Fastest JS/TS implemenation of Levenshtein distance.
Measure the difference between two strings.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-fd-slicer
  (package
    (name "node-fd-slicer")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/andrewrk/node-fd-slicer")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wpazi7a1rjdd8ikp0j4w082jbbh8nv1x9kms2sas74rn8xr0ahi"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (modify-json (delete-dev-dependencies)))))))
    (inputs (list node-pend))
    (home-page "https://github.com/andrewrk/node-fd-slicer")
    (synopsis "Create multiple readable/writable streams from a file descriptor")
    (description "This package provides a way to create multiple readable/writable streams
based on a single file descriptor, maintaining safety from concurrency issues.")
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

(define-public node-glob
  (package
    (name "node-glob")
    (version "10.1.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/isaacs/node-glob")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256 (base32 "19r0q9mxfbbma6z0iapf1jnccrz0hil69kihzbjjxzy9b7cgzmsd"))))
    (build-system node-build-system)
    (inputs (list
      node-minimatch
      node-minipass-5
      node-path-scurry))
    ; Use ESBuild instead of tshy because this is used to build Typescript.
    (native-inputs (list esbuild))
    (arguments (list
      #:modules '(
        (guix build node-build-system)
        (guix build utils)
        (ice-9 match))
      #:tests? #f ; FIXME: Tests require 'c8' and 'tap'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package
          (lambda _
            (modify-json
              (delete-dev-dependencies)
              (delete-fields (list
                "scripts.prepare"))
              (delete-dependencies (list
                ; Not currently used in other packages, but if there are
                ; issues we may need to add this in.
                "fs.realpath")))))
        (replace 'build
          (lambda _
            (define output "output")
            (for-each
              (match-lambda ((format directory)
                (invoke
                "esbuild"
                "src/*.ts"
                "--platform=node"
                (string-append "--format=" format)
                "--target=es2022"
                "--sourcemap"
                (string-append "--outdir=" output "/dist/" directory))))
              (list (list "cjs" "cjs") (list "esm" "mjs")))
            (for-each
              (lambda (file) (install-file file output))
              (list "LICENSE" "README.md" "package.json"))
            (chdir output))))))
    (synopsis "Match files using the patterns the shell uses")
    (description "The most correct and second fastest glob implementation in JavaScript.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:isc)))

(define-public node-get-stream
  (package
    (name "node-get-stream")
    (version "5.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sindresorhus/get-stream")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mc7fw8qc0b1a4i2h4mz1d4hygla5grxpfh8dk2rabnw46lxm73m"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (modify-json (delete-dev-dependencies)))))))
    (inputs (list node-pump))
    (home-page "https://github.com/sindresorhus/get-stream")
    (synopsis "Get a stream as a string, buffer, or array")
    (description "This package provides a way to get a stream as a string,
buffer, or array.  Useful for getting the result of a stream.")
    (license license:expat)))

(define-public node-graceful-fs
  (package
    (name "node-graceful-fs")
    (version "4.2.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/isaacs/node-graceful-fs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18al1wpdmpi92yvifa3q1vga3szndfd84fqlakh9abn8bfsqxkp8"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/isaacs/node-graceful-fs")
    (synopsis "Drop-in replacement for fs with various improvements")
    (description "This package provides a drop-in replacement for the fs
module that makes various improvements, including queueing operations,
retrying EMFILE errors, and working around various platform quirks.")
    (license license:isc)))

(define-public node-gyp-build
  (package
    (name "node-gyp-build")
    (version "4.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prebuild/node-gyp-build")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nb5fx5x5dpz08jjs2d9xphjslcxnvzcjig802y40bvhd8c114gb"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/prebuild/node-gyp-build")
    (synopsis "Build tool for native Node.js addons")
    (description "This package provides a build tool that works with prebuild
and node-gyp to compile native Node.js addons.  It will try to load prebuilt
binaries first, falling back to building from source.")
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

(define-public node-has-flag
  (package
    (name "node-has-flag")
    (version "3.0.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/sindresorhus/has-flag")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "0wz3ihagci5nk2fawbw2pfprnqbhlvxi79ikvrn4kkhh3vfn85q2"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'xo' and 'ava'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Check if argv has a specific flag")
    (description "Check if argv has a specific flag
Correctly stops looking after an -- argument terminator.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-hereby
  (package
    (name "node-hereby")
    (version "1.11.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/jakebailey/hereby")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "0jdfrfvnz97nc2ly8ws98dwi05hcc6qs1p76xy0hjdf4r5yvirsj"))))
    (build-system node-build-system)
    (inputs (list
      node-command-line-usage
      node-fastest-levenshtein
      node-minimist
      node-picocolors
      node-pretty-ms))
    ; Use ESBuild because this is used to build Typescript.
    (native-inputs (list esbuild))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'ava'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies))))
        (replace 'build (lambda _
          (delete-file "tsconfig.json")
          (delete-file-recursively "src/__tests__")
          (invoke
            "esbuild"
            "src/**/*.ts"
            "--platform=node"
            "--format=esm"
            "--sourcemap"
            "--outdir=dist"))))))
    (synopsis "Simple task runner")
    (description "Tasks are defined in Herebyfile.mjs. Exported tasks are available to\
 run at the CLI, with support for export default.")
    (home-page (git-reference-url (origin-uri source)))
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

(define-public node-https-proxy-agent
  (package
    (name "node-https-proxy-agent")
    (version "7.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TooTallNate/proxy-agents")
             (commit (string-append "https-proxy-agent@" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c02kcbfp63r1y36hrkhzkmn3gz6ad2s577js94776vza3r7r631"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'change-directory
            (lambda _
              (chdir "packages/https-proxy-agent")))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies))))
          (replace 'build
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let ((esbuild (search-input-file (or native-inputs inputs) "/bin/esbuild")))
                (mkdir-p "dist")
                (for-each
                 (lambda (ts-file)
                   (let* ((base (basename ts-file ".ts"))
                          (js-file (string-append "dist/" base ".js")))
                     (invoke esbuild ts-file
                             (string-append "--outfile=" js-file)
                             "--format=cjs"
                             "--platform=node"
                             "--target=es2020")))
                 (find-files "src" "\\.ts$"))))))))
    (native-inputs (list esbuild))
    (inputs
     (list node-agent-base node-debug))
    (home-page "https://github.com/TooTallNate/proxy-agents")
    (synopsis "HTTPS proxy HTTP.Agent implementation")
    (description "This package provides an @code{HTTP.Agent} implementation
that connects to a specified HTTP or HTTPS proxy server, and can be used with
the built-in https module.")
    (license license:expat)))

(define-public node-jpeg-js
  (package
    (name "node-jpeg-js")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eugeneware/jpeg-js")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11lw3b6p4g7wm7k7ha3y1fnbmbr58r7fm7hkwjbp6bb5705mgyjg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/eugeneware/jpeg-js")
    (synopsis "Pure JavaScript JPEG encoder and decoder")
    (description "This package provides a pure JavaScript JPEG encoder and
decoder for Node.js.")
    (license license:bsd-3)))

(define-public node-ip-address
  (package
    (name "node-ip-address")
    (version "9.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/beaugunderson/ip-address")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nb8cyhvpbjlm46y7axhlrgfmr4kw7506fgp7482hl7kaf9ixl14"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies))))
          (replace 'build
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let ((esbuild (search-input-file (or native-inputs inputs)
                                                "/bin/esbuild")))
                (mkdir-p "dist")
                (for-each
                 (lambda (ts-file)
                   (let* ((relative (substring ts-file (string-length "src/")))
                          (js-file (string-append "dist/"
                                                  (string-take relative (string-index relative #\.))
                                                  ".js")))
                     (mkdir-p (dirname js-file))
                     (invoke esbuild ts-file
                             (string-append "--outfile=" js-file)
                             "--format=cjs"
                             "--platform=node"
                             "--target=es2020")))
                 (find-files "src" "\\.ts$"))))))))
    (native-inputs (list esbuild))
    (inputs
     (list node-jsbn node-sprintf-js))
    (home-page "https://github.com/beaugunderson/ip-address")
    (synopsis "Library for parsing and manipulating IPv6 and IPv4 addresses")
    (description "This package provides a library for parsing and
manipulating IPv6 and IPv4 addresses in JavaScript.")
    (license license:expat)))

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

(define-public node-isexe
  (package
    (name "node-isexe")
    (version "2.0.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/isaacs/isexe")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "18rh937j0m0jkzdxfdvvjv6nsdbrdqipnq7nvv1ab7b7rjyw5id3"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'tap'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Minimal module to check if a file is executable.")
    (description "Minimal module to check if a file is executable, and a normal file.
Uses fs.stat.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:isc)))

(define-public node-jsonc-parser
  (package
    (name "node-jsonc-parser")
    (version "3.3.1")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/microsoft/node-jsonc-parser")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1pf3kxbx9v4h646hqfjw8r70y8vq9zl3l393nfcqzhm8z1skb3iy"))))
    (build-system node-build-system)
    (native-inputs (list esbuild))
    (arguments (list
      #:modules '(
        (guix build node-build-system)
        (guix build utils)
        (ice-9 match))
      #:tests? #f ; FIXME: Tests require 'mocha'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (delete-file-recursively "src/test")
          (modify-json
            (delete-dev-dependencies))))
        (replace 'build (lambda _
          (define output "output")
          (define lib (string-append output "/lib"))
          (mkdir-p lib)
          (for-each
            (match-lambda ((format directory parameters)
              (apply invoke (append
                (list
                  "esbuild"
                  "src/**/*.ts"
                  "--platform=node"
                  (string-append "--format=" format)
                  "--target=es2020"
                  (string-append "--outdir=" lib "/" directory))
                parameters))))
            (list
              (list "cjs" "umd" (list "--global-name=jsoncParser"))
              (list "esm" "esm" (list))))
          (for-each
            (lambda (file) (install-file file output))
            (list
              "CHANGELOG.md"
              "LICENSE.md"
              "package.json"
              "README.md"
              "SECURITY.md"))
          (chdir output))))))
    (synopsis "Scanner and parser for JSON with comments")
    (description "JSONC is JSON with JavaScript style comments. This node module provides\
 a scanner and fault tolerant parser that can process JSONC but is also useful for\
 standard JSON.
  * the scanner tokenizes the input string into tokens and token offsets
  * the visit function implements a 'SAX' style parser with callbacks for the encountered\
 properties and values.
  * the parseTree function computes a hierarchical DOM with offsets representing the\
 encountered properties and values.
  * the parse function evaluates the JavaScript object represented by JSON string in a\
 fault tolerant fashion.
  * the getLocation API returns a location object that describes the property or value\
 located at a given offset in a JSON document.
  * the findNodeAtLocation API finds the node at a given location path in a JSON DOM.
  * the format API computes edits to format a JSON document.
  * the modify API computes edits to insert, remove or replace a property or value in a\
 JSON document.
  * the applyEdits API applies edits to a document.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-is-docker
  (package
    (name "node-is-docker")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sindresorhus/is-docker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04cs0zffssdagqrn6sd98vk03ps4hwkd3yvk5gq2xpgdn6n77qzq"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/sindresorhus/is-docker")
    (synopsis "Check if the process is running inside a Docker container")
    (description "This package provides a way to check if the process is
running inside a Docker container.")
    (license license:expat)))

(define-public node-is-wsl
  (package
    (name "node-is-wsl")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sindresorhus/is-wsl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gn5yfg4mwbphfnwqnsxwcfarb2jfgy6ib3hyp5b83wy3dqb04b5"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list node-is-docker))
    (home-page "https://github.com/sindresorhus/is-wsl")
    (synopsis "Check if the process is running inside Windows Subsystem for Linux")
    (description "This package provides a way to check if the process is
running inside Windows Subsystem for Linux.")
    (license license:expat)))

(define-public node-jsbn
  (package
    (name "node-jsbn")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andyperlitch/jsbn")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z875y5i0ck809gqnnw33yw02x5vmc9g42mfr5cjkgwinci1d9lk"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/andyperlitch/jsbn")
    (synopsis "JavaScript BigNum library")
    (description "This package is a port of Tom Wu's Big Integer library to
JavaScript.")
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

(define-public node-lru-cache
  (package
    (name "node-lru-cache")
    (version "10.4.3")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/isaacs/node-lru-cache")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256 (base32 "1zqpm7d7czbqyymzgs4qadd0hiy6n290y6wwy1gwa46mmd1bxbr3"))))
    (build-system node-build-system)
    ; Use ESBuild instead of tshy because this is used to build Typescript.
    (native-inputs (list esbuild))
    (arguments (list
      #:modules '(
        (guix build node-build-system)
        (guix build utils)
        (ice-9 match))
      #:tests? #f ; FIXME: Tests require 'tap'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'delete-dependencies
          (lambda _
            (modify-json
              (delete-dev-dependencies)
              (delete-fields (list
                "scripts.prepare")))))
        (replace 'build
          (lambda _
            (define output "output")
            (for-each
              (match-lambda ((format directory type)
                (define target-output (string-append output "/dist/" directory))
                (invoke
                  "esbuild"
                  "src/*.ts"
                  "--platform=node"
                  "--target=es2022"
                  (string-append "--format=" format)
                  "--jsx=transform"
                  "--sourcemap"
                  (string-append "--outdir=" target-output))
                (with-output-to-file
                  (string-append target-output "/package.json")
                  (lambda _
                    (display (string-append "{\"type\": \"" type "\"}"))))))
              (list
                (list "cjs" "commonjs" "commonjs")
                (list "esm" "esm" "module")))
            (for-each
              (lambda (file) (install-file file output))
              (list "LICENSE" "package.json" "README.md"))
            (chdir output))))))
    (synopsis "A fast cache that automatically deletes the least recently used items.")
    (description "A cache object that deletes the least-recently-used items.
Specify a max number of the most recently used items that you want to keep, and
this cache will keep that many of the most recently accessed items.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:isc)))

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

(define-public node-mime
  (package
    (name "node-mime")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/broofa/mime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g6k3aj71bv10cjfl6rnjks767vfsj967z8xypc0zn47v0w0k1jb"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies))))
         (add-after 'install 'remove-prepare-script
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The prepare script generates types/standard.js and types/other.js
             ;; from mime-db.  These files are already generated during the Guix
             ;; build.  Remove the script to prevent npm from trying to re-run it
             ;; when this package is used as a file: dependency.
             (modify-json #:file (string-append (assoc-ref outputs "out")
                                                "/lib/node_modules/mime/package.json")
                          (delete-fields '(("scripts" "prepare")))))))))
    (home-page "https://github.com/broofa/mime")
    (synopsis "Comprehensive MIME type mapping API")
    (description "This package provides a library for MIME type mapping based
on mime-db.")
    (license license:expat)))

(define-public node-minimatch-3
  (package
    (name "node-minimatch")
    (version "3.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/isaacs/minimatch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g6w4nn8vx38j4ka5hlphhp505dxyr6dv7g99kfzrjrlnlh7vjbr"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list node-brace-expansion-1))
    (home-page "https://github.com/isaacs/minimatch")
    (synopsis "Glob matcher for JavaScript")
    (description "This package provides a minimal matching utility for
JavaScript.")
    (license license:isc)))

(define-public node-minimatch
  (package
    (name "node-minimatch")
    (version "9.0.5")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/isaacs/minimatch")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256 (base32 "1m9lr4nalzml8l6lz09jmk3nhqm7jf6lkyybvdg0ccqx74si7d44"))))
    (build-system node-build-system)
    (inputs (list node-brace-expansion))
    ; Use ESBuild because this is used to build Typescript.
    (native-inputs (list esbuild))
    (arguments (list
      #:modules '(
        (guix build node-build-system)
        (guix build utils)
        (ice-9 match))
      #:tests? #f ; FIXME: Tests require 'tap'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package
          (lambda _
            (modify-json
              (delete-dev-dependencies)
              (delete-fields (list
                "scripts.prepare")))))
        (replace 'build
          (lambda _
            (define output "output")
            (for-each
              (match-lambda ((format directory type)
                (define target-output (string-append output "/dist/" directory))
                (invoke
                  "esbuild"
                  "src/*.ts"
                  "--platform=node"
                  "--target=es2022"
                  (string-append "--format=" format)
                  "--jsx=transform"
                  "--sourcemap"
                  (string-append "--outdir=" target-output))
                (with-output-to-file
                  (string-append target-output "/package.json")
                  (lambda _
                    (display (string-append "{\"type\": \"" type "\"}"))))))
              (list
                (list "cjs" "commonjs" "commonjs")
                (list "esm" "esm" "module")))
            (for-each
              (lambda (file) (install-file file output))
              (list "LICENSE" "package.json" "README.md"))
            (chdir output))))))
    (synopsis "A glob matcher in javascript.")
    (description "A minimal matching utility.
This is the matching library used internally by npm.
It works by converting glob expressions into JavaScript RegExp objects.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:isc)))

(define-public node-minimist
  (package
    (name "node-minimist")
    (version "1.2.8")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/minimistjs/minimist")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "027nxm7pkam89qxdybf7sd1k3h84njykhhxmzn1l6psmvrkwbqb7"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Parse CLI arguments in Javascript")
    (description "This package can scan for CLI flags and arguments in Javascript.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-minipass-5
  (package
    (name "node-minipass")
    (version "5.0.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/isaacs/minipass")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256 (base32 "1xwxk9w290d0xsi9flzlrpdr4rwbq8q47d8hrzv27pzr2gf42nsz"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'tap'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package
          (lambda _
            (modify-json
              (delete-dev-dependencies)
              (delete-fields (list
                "scripts.prepare")))))
        (replace 'build
          (lambda _
            (define output "output")
            (mkdir output)
            (invoke "node" "./scripts/transpile-to-esm.js")
            (for-each
              (lambda (file) (install-file file output))
              (list "index.js" "index.mjs" "LICENSE" "package.json" "README.md"))
            (chdir output))))))
    (synopsis "Minimal implementation of a PassThrough stream")
    (description "A very minimal implementation of a PassThrough stream
It's very fast for objects, strings, and buffers.
Supports pipe()ing (including multi-pipe() and backpressure transmission), buffering\
 data until either a data event handler or pipe() is added (so you don't lose the first\
 chunk), and most other cases where PassThrough is a good idea.
There is a read() method, but it's much more efficient to consume data from this stream\
 via 'data' events or by calling pipe() into some other stream. Calling read() requires\
 the buffer to be flattened in some cases, which requires copying memory.
If you set objectMode: true in the options, then whatever is written will be emitted.\
 Otherwise, it'll do a minimal amount of Buffer copying to ensure proper Streams\
 semantics when read(n) is called.
objectMode can only be set at instantiation. Attempting to write something other than a\
 String or Buffer without having set objectMode in the options will throw an error.
This is not a through or through2 stream. It doesn't transform the data, it just passes\
 it right through. If you want to transform the data, extend the class, and override the\
 write() method. Once you're done transforming the data however you want, call\
 super.write() with the transform output.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:isc)))

(define-public node-minipass-7
  (package
    (inherit node-minipass-5)
    (version "7.1.2")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url (git-reference-url (origin-uri (package-source node-minipass-5))))
        (commit (string-append "v" version))))
      (file-name (git-file-name (package-name node-minipass-5) version))
      (sha256 (base32 "0zqnnw9ibwm660md1pfqsiwbbpbizydiwjp4fr8niyzczxk66k1j"))))
    ; Use ESBuild because this is used to build Typescript.
    (native-inputs (list esbuild))
    (arguments (list
      #:modules '(
        (guix build node-build-system)
        (guix build utils)
        (ice-9 match))
      #:tests? #f ; FIXME: Tests require 'tap'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package
          (lambda _
            (modify-json
              (delete-dev-dependencies)
              (delete-fields (list
                "scripts.prepare")))))
        (replace 'build
          (lambda _
            (define output "output")
            (for-each
              (match-lambda ((format directory type)
                (define target-output (string-append output "/dist/" directory))
                (invoke
                  "esbuild"
                  "src/index.ts"
                  "--platform=node"
                  "--target=es2022"
                  (string-append "--format=" format)
                  "--jsx=transform"
                  "--sourcemap"
                  (string-append "--outdir=" target-output))
                (with-output-to-file
                  (string-append target-output "/package.json")
                  (lambda _ (display (string-append "{\"type\": \"" type "\"}"))))))
              (list
                (list "cjs" "commonjs" "commonjs")
                (list "esm" "esm" "module")))
            (for-each
              (lambda (file) (install-file file output))
              (list "LICENSE" "package.json" "README.md"))
            (chdir output))))))))

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

(define-public node-open
  (package
    (name "node-open")
    (version "8.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sindresorhus/open")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17h4sv0xr96hckw5z4wjsnszv9ic3kjn03yixg2ihcak535lhkra"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list node-define-lazy-prop node-is-docker node-is-wsl))
    (home-page "https://github.com/sindresorhus/open")
    (synopsis "Open URLs, files, and executables")
    (description "This package provides a way to open stuff like URLs, files,
executables.")
    (license license:expat)))

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

(define-public node-parse-ms
  (package
    (name "node-parse-ms")
    (version "3.0.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/sindresorhus/parse-ms")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1chq6a4q2ycswkpz16bgm7853g6rh3w1j5dr7061x9h6xip3mphb"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'xo', 'ava', and 'tsd'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Parse milliseconds into an object")
    (description "Usage
import parseMilliseconds from 'parse-ms';
parseMilliseconds(1337000001);
/*
{
	days: 15,
	hours: 11,
	minutes: 23,
	seconds: 20,
	milliseconds: 1,
	microseconds: 0,
	nanoseconds: 0
}
*/")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

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

(define-public node-path-scurry
  (package
    (name "node-path-scurry")
    (version "1.11.1")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/isaacs/path-scurry")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256 (base32 "1vbmzg2pwmm441d9clk69fpij33ns5mblncy0za1x5f7d04jjyi2"))))
    (build-system node-build-system)
    (inputs (list
      node-lru-cache
      node-minipass-7))
    ; Use ESBuild because this is used to build Typescript.
    (native-inputs (list esbuild))
    (arguments (list
      #:modules '(
        (guix build node-build-system)
        (guix build utils)
        (ice-9 match))
      #:tests? #f ; FIXME: Tests require 'tap'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'delete-dependencies
          (lambda _
            (modify-json
              (delete-dev-dependencies)
              (delete-fields (list "scripts.prepare")))))
        (replace 'build
          (lambda _
            (define output "output")
            (for-each
              (match-lambda ((format directory type)
                (define target-output (string-append output "/dist/" directory))
                (invoke
                  "esbuild"
                  "src/*.ts"
                  "--platform=node"
                  "--target=es2022"
                  (string-append "--format=" format)
                  "--jsx=transform"
                  "--sourcemap"
                  (string-append "--outdir=" target-output))
                (with-output-to-file
                  (string-append target-output "/package.json")
                  (lambda _ (display (string-append "{\"type\": \"" type "\"}"))))))
              (list
                (list "cjs" "commonjs" "commonjs")
                (list "esm" "esm" "module")))
            (for-each
              (lambda (file) (install-file file output))
              (list "LICENSE.md" "package.json" "README.md"))
            (chdir output))))))
    (synopsis "Yet another file traversal library.")
    (description "Extremely high performant utility for building tools that read\
 the file system, minimizing filesystem and path string munging operations to\
 the greatest degree possible..")
    (home-page (git-reference-url (origin-uri source)))
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

(define-public node-pend
  (package
    (name "node-pend")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/andrewrk/node-pend")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fy6x7safc0hwmfnic0yqqs2ak8ckmr1hk8pl30f03angy0qkwnx"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/andrewrk/node-pend")
    (synopsis "Dead-simple optimistic async helper")
    (description "This package provides an async helper for Node.js that
manages pending callbacks and signals completion when all have finished.")
    (license license:expat)))

(define-public node-picocolors
  (package
    (name "node-picocolors")
    (version "1.1.1")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/alexeyraspopov/picocolors")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "04g0rl3i08fsakadmls21nbyk5srz7qpmic6m8fjxglbf5mvnsq7"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; Test is broken, possibly by being run in guix build.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Small and fast ANSI colors terminal output formatting library")
    (description "The tiniest and the fastest library for terminal output formatting with\
 ANSI colors.
  * No dependencies.
  * 14 times smaller and 2 times faster than chalk.
  * Used by popular tools like PostCSS, SVGO, Stylelint, and Browserslist.
  * Node.js v6+ & browsers support. Support for both CJS and ESM projects.
  * TypeScript type declarations included.
  * NO_COLOR friendly.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:isc)))

(define-public node-pretty-ms
  (package
    (name "node-pretty-ms")
    (version "8.0.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/sindresorhus/pretty-ms")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "08pyb3lmkn1bn8icsi2r9jlkh24mzi39yr9k81p2hlgm46b8wiar"))))
    (build-system node-build-system)
    (inputs (list
      node-parse-ms))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'xo', 'ava', and 'tsd'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Milliseconds to humand readable string converter")
    (description "Convert milliseconds to a human readable string:\
 `1337000000` → `15d 11h 23m 20s`")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

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

(define-public node-pngjs
  (package
    (name "node-pngjs")
    (version "6.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pngjs/pngjs")
             ;; No git tag for 6.0.0; commit found via:
             ;; curl -s 'https://api.github.com/repos/pngjs/pngjs/commits?path=package.json' \
             ;;   | jq '.[] | select(.commit.message | test("v6.0.0"))'
             (commit "b68264b1a85abf6d111a7138ad4f18b117fb5f81")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d617apsrgsqkn3lwilvajjx5j49rkdyyz3i4by3qvqsznpzsgxx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies))))
         (delete 'build))))
    (home-page "https://github.com/pngjs/pngjs")
    (synopsis "PNG encoder/decoder for Node.js in pure JavaScript")
    (description "This package provides a simple PNG encoder/decoder for
Node.js with no dependencies.")
    (license license:expat)))

(define-public node-progress
  (package
    (name "node-progress")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/visionmedia/node-progress")
             ;; No git tag for 2.0.3; commit found via:
             ;; curl -s 'https://api.github.com/repos/visionmedia/node-progress/commits?path=package.json' \
             ;;   | jq '.[] | select(.commit.message == "2.0.3")'
             (commit "0790207ef077cbfb7ebde24a1dd9895ebf4643e1")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "125v9cdgrb0pa8ijs6klsckws23lws9p2rcm5mixg6p584yyzfv8"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/visionmedia/node-progress")
    (synopsis "Flexible ascii progress bar for Node.js")
    (description "This package provides a flexible ASCII progress bar for
command-line applications.")
    (license license:expat)))

(define-public node-proper-lockfile
  (package
    (name "node-proper-lockfile")
    (version "4.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/moxystudio/node-proper-lockfile")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i6ljfzxnzsmdkrpr74vy7qdn6pgllflkdr85a29sdyx55b5gfc2"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list node-graceful-fs
           node-retry
           node-signal-exit))
    (home-page "https://github.com/moxystudio/node-proper-lockfile")
    (synopsis "Inter-process and inter-machine lockfile utility")
    (description "This package provides an inter-process and inter-machine
lockfile utility that works on a local or network file system.")
    (license license:expat)))

(define-public node-proxy-from-env
  (package
    (name "node-proxy-from-env")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Rob--W/proxy-from-env")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m7rr70lb7011310rj85nyg3vwlkr1w8302nr4s0xmcxi4yv6sln"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/Rob--W/proxy-from-env")
    (synopsis "Proxy URL from environment variables")
    (description "This package provides a way to read proxy URLs from
environment variables.")
    (license license:expat)))

(define-public node-pump
  (package
    (name "node-pump")
    (version "3.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mafintosh/pump")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1iyfks1k62dplvajd4xm41b5v3dfp8b22h88z7xrwsbnwsd0vl6f"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-dependencies 'delete-dev-dependencies
                    (lambda _
                      (modify-json (delete-dev-dependencies)))))))
    (inputs (list node-end-of-stream node-once))
    (home-page "https://github.com/mafintosh/pump")
    (synopsis "Pipe streams together and close all of them if one closes")
    (description "This package allows you to pipe streams together and
destroy all of them if one of them closes.")
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

(define-public node-reduce-flatten
  (package
    (name "node-reduce-flatten")
    (version "2.0.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/75lb/reduce-flatten")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "0xw23djgi8agv06bbjc3x6wsca1xxnksbv7zd6cgxvmw15xqyb68"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'test-runner'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Flatten an array into the supplied array")
    (description "Isomorphic map-reduce function to flatten an array into the supplied\
 array.
Example
> numbers = [ 1, 2, [ 3, 4 ], 5 ]
> numbers.reduce(flatten, [])
[ 1, 2, 3, 4, 5 ]")
    (home-page (git-reference-url (origin-uri source)))
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

(define-public node-retry
  (package
    (name "node-retry")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tim-kos/node-retry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qkcbmmysm1d4g8xb425c3hcvlkkgfzv4w2jgq9isimximqqm5qg"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/tim-kos/node-retry")
    (synopsis "Retry strategies for failed operations in Node.js")
    (description "This package provides retry strategies for retrying failed
operations.")
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

(define-public node-signal-exit
  (package
    (name "node-signal-exit")
    (version "3.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tapjs/signal-exit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f2wn0p2d1w2yqd0wz0vppnr6slgk7jzf7w54cqkxdj1j8gwmh4y"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/tapjs/signal-exit")
    (synopsis "Fire an event when a process exits")
    (description "This package provides a way to fire an event when a process
exits.")
    (license license:isc)))

(define-public node-smart-buffer
  (package
    (name "node-smart-buffer")
    (version "4.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JoshGlazebrook/smart-buffer")
             ;; No git tag for 4.2.0; commit found via:
             ;; curl -s 'https://api.github.com/repos/JoshGlazebrook/smart-buffer/commits?path=package.json' \
             ;;   | jq '.[] | select(.commit.message == "4.2.0")'
             (commit "2ce01c95312343325997b19e52daa166dc227930")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "064z6b0xlc8mvnlj8fmi0zq2wk8xah96aiabarq9igsc4hijmcgm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies))))
          (replace 'build
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let ((esbuild (search-input-file (or native-inputs inputs)
                                                "/bin/esbuild")))
                (mkdir-p "build")
                (for-each
                 (lambda (ts-file)
                   (let* ((base (basename ts-file ".ts"))
                          (js-file (string-append "build/" base ".js")))
                     (invoke esbuild ts-file
                             (string-append "--outfile=" js-file)
                             "--format=cjs"
                             "--platform=node"
                             "--target=es2020")))
                 (find-files "src" "\\.ts$"))))))))
    (native-inputs (list esbuild))
    (home-page "https://github.com/JoshGlazebrook/smart-buffer")
    (synopsis "Buffer wrapper with automatic read/write offset management")
    (description "This package provides a smarter Buffer that keeps track of
its own read and write offsets.")
    (license license:expat)))

(define-public node-socks
  (package
    (name "node-socks")
    (version "2.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JoshGlazebrook/socks")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1isqyw7vlf258fmdxs7gq1kgqf4mh51ijnp6vcd445rcqbk7n43f"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies))))
          (replace 'build
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let ((esbuild (search-input-file (or native-inputs inputs)
                                                "/bin/esbuild")))
                (mkdir-p "build")
                (for-each
                 (lambda (ts-file)
                   (let* ((relative (substring ts-file (+ (string-length "src/") 0)))
                          (js-file (string-append "build/"
                                                  (string-drop-right relative 3)
                                                  ".js")))
                     (mkdir-p (dirname js-file))
                     (invoke esbuild ts-file
                             (string-append "--outfile=" js-file)
                             "--format=cjs"
                             "--platform=node"
                             "--target=es2020")))
                 (find-files "src" "\\.ts$"))))))))
    (native-inputs (list esbuild))
    (inputs
     (list node-ip-address node-smart-buffer))
    (home-page "https://github.com/JoshGlazebrook/socks")
    (synopsis "SOCKS proxy client for Node.js")
    (description "This package provides a SOCKS proxy client supporting
SOCKS4, SOCKS4a, and SOCKS5 protocols.")
    (license license:expat)))

(define-public node-socks-proxy-agent
  (package
    (name "node-socks-proxy-agent")
    (version "8.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TooTallNate/proxy-agents")
             (commit (string-append "socks-proxy-agent@" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c02kcbfp63r1y36hrkhzkmn3gz6ad2s577js94776vza3r7r631"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'change-directory
            (lambda _
              (chdir "packages/socks-proxy-agent")))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies))))
          (replace 'build
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let ((esbuild (search-input-file (or native-inputs inputs)
                                                "/bin/esbuild")))
                (mkdir-p "dist")
                (invoke esbuild "src/index.ts"
                        "--outfile=dist/index.js"
                        "--format=cjs"
                        "--platform=node"
                        "--target=es2020")))))))
    (native-inputs (list esbuild))
    (inputs
     (list node-agent-base node-debug node-socks))
    (home-page "https://github.com/TooTallNate/proxy-agents")
    (synopsis "SOCKS proxy HTTP.Agent implementation")
    (description "This package provides a SOCKS proxy @code{http.Agent}
implementation for HTTP and HTTPS.")
    (license license:expat)))

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
             (modify-json (delete-dependencies '("prebuild-install"
                                                 ;; devDependencies
                                                 "@serialport/binding-mock"
                                                 "node-abi")))))
         (add-after 'chdir 'avoid-prebuild-install
           (lambda args
             (modify-json (delete-fields '(("scripts" "install")))
                          (replace-fields '(("gypfile" . #f)))))))
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

(define-public node-source-map
  (package
    (name "node-source-map")
    (version "0.6.1")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/mozilla/source-map")
        (commit version)))
      (file-name (git-file-name name version))
      (sha256
        (base32 "0q8qaddi05387y8f58q0xlj7fjxf4mkymmir7wd9c5imqqs378md"))
      (modules '((guix build utils)))
      (snippet #~(begin
        (delete-file-recursively "dist")))))
    (build-system node-build-system)
    (native-inputs (list esbuild))
    (arguments (list
      #:modules '(
        (guix build node-build-system)
        (guix build utils)
        (ice-9 match)
        (json))
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)
            (replace-fields (list (cons "scripts.test" "node test/run-tests.js"))))))
        (replace 'build (lambda _
          (for-each
            (match-lambda ((name parameters)
              (apply invoke (append
                (list
                  "esbuild"
                  "source-map.js"
                  "--bundle"
                  "--format=iife"
                  "--global-name=sourceMap"
                  (string-append "--outfile=dist/source-map" name ".js"))
                parameters))))
            (list
              (list "" (list))
              (list ".min" (list "--sourcemap" "--minify")))))))))
    (synopsis "Generates and consumes source maps")
    (description "This is a library to generate and consume the source map format\
 @uref{https://docs.google.com/document/d/1U1RGAehQwRypUTovF1KRlpiOFze0b-_2gc6fAH0KY0k\
/edit, described here}.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:bsd-3)))

(define-public node-source-map-support
  (package
    (name "node-source-map-support")
    (version "0.5.21")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/evanw/node-source-map-support")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1r03whfzy5f76wz4cf51gllxndxv9mk75p4aip6bxffv5p7apax3"))
      (modules '((guix build utils)))
      (snippet #~(begin
        (delete-file "browser-source-map-support.js")))))
    (build-system node-build-system)
    (inputs (list
      node-buffer-from
      node-source-map))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'mocha'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)
            ; Build only builds browser-source-map-support.js which we don't currently use
            (delete-fields (list "scripts.build"))))))))
    (synopsis "Fixes stack traces for files with source maps")
    (description "This module provides source map support for stack traces in node via\
 the V8 stack trace API. It uses the source-map module to replace the paths and line\
 numbers of source-mapped files with their original paths and line numbers. The output\
 mimics node's stack trace format with the goal of making every compile-to-JS language\
 more of a first-class citizen. Source maps are completely general (not specific to any\
 one language) so you can use source maps with multiple compile-to-JS languages in the\
 same node process.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

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

(define-public node-stack-utils
  (package
    (name "node-stack-utils")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tapjs/stack-utils")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dgzj3x1c257vv3306xd1kg6kvwv3z41a0irzg1lmp0nl0pg744n"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list node-escape-string-regexp-4))
    (home-page "https://github.com/tapjs/stack-utils")
    (synopsis "Captures and cleans stack traces")
    (description "This package provides a way to capture and clean stack
traces for Node.js.")
    (license license:expat)))

(define-public node-sprintf-js
  (package
    (name "node-sprintf-js")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alexei/sprintf.js")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "106r47h0xhrqwqzdzhd5b93saldj89av0qapl1x50gs3pnkdqh8r"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove prebuilt minified JavaScript files.
           (delete-file-recursively "dist")
           ;; Remove files not needed for the package.
           (delete-file-recursively "benchmark")
           (delete-file-recursively "demo")
           (delete-file-recursively "test")))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/alexei/sprintf.js")
    (synopsis "JavaScript sprintf implementation")
    (description "This package provides a JavaScript sprintf implementation.")
    (license license:bsd-3)))

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

; Beware of this package. Version 1.x is for CJS and 2.x for ESM, but they've
; been publishing the 1.x version by hand, it's not checked into the repo.
; Currently switching from ESM to CJS in this package definition.
(define-public node-strnum
  (package
    (name "node-strnum")
    (version "1.1.2")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/NaturalIntelligence/strnum")
        (commit "6ed1b2fc39f169c35c2fc06ec89e7b7fd17fa0ce")))
      (file-name (git-file-name name version))
      (sha256
        (base32 "0ghi1j76fh6d572jwvh1zw8m9h8g480dj5snkpl7b0di30s8qbyi"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'jasmine'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)
            (delete-fields (list "type"))
            (replace-fields (list (cons "version" #$version))))))
        (replace 'build (lambda _
          (define problem-file "strnum.js")
          (substitute* problem-file (("^export default ") ""))
          (let ((problem-file-port (open-file problem-file "a")))
            (display "\nmodule.exports = toNumber;" problem-file-port)
            (close-port problem-file-port)))))))
    (synopsis "Parse String to Number based on configuration")
    (description "Javascript library that parses a string into a number.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-supports-color
  (package
    (name "node-supports-color")
    (version "5.5.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/chalk/supports-color")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1wfwzxjh4q7wv8p8wjrhlcdljdydjpcydgdysy9y161xy6r99db9"))))
    (build-system node-build-system)
    (inputs (list
      node-has-flag))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'xo' and 'ava'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Detect whether a terminal supports color")
    (description "Returns an Object with a stdout and stderr property for testing either\
 streams. Each property is an Object, or false if color is not supported.

The stdout/stderr objects specifies a level of support for color through a .level\
 property and a corresponding flag:

  * .level = 1 and .hasBasic = true: Basic color support (16 colors)
  * .level = 2 and .has256 = true: 256 color support
  * .level = 3 and .has16m = true: Truecolor support (16 million colors)")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-table-layout
  (package
    (name "node-table-layout")
    (version "1.0.2")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/75lb/table-layout")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1k21p0ia995ax1biknalivq444jw9xy31bp52a5sq3rwlnzp0vbx"))))
    (build-system node-build-system)
    (inputs (list
      node-typical
      node-array-back
      node-wordwrapjs
      node-deep-extend))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'test-runner'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Stylable text tables for handling ansi colour")
    (description "Generates plain-text tables from JSON recordset input (array of\
 objects). Useful for presenting text in column layout or data in table layout in\
 text-based user interfaces.")
    (home-page (git-reference-url (origin-uri source)))
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

(define (node-types type version commit hash inputs)
  (package
    (name (string-append "node-types-" type))
    (version version)
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/DefinitelyTyped/DefinitelyTyped")
        (commit commit)))
      (file-name (git-file-name name version))
      (sha256 (base32 hash))))
    (build-system node-build-system)
    (inputs (force inputs))
    (arguments (list
      #:tests? #f ; Static files, no tests.
      #:phases #~(modify-phases %standard-phases
        (add-after 'unpack 'setup (lambda _
          (chdir (string-append "types/" #$type))
          (modify-json
            (delete-dev-dependencies)
            (replace-fields (list
              (cons "version" #$version)))))))))
    (synopsis (string-append "TypeScript definitions for " type))
    (description (string-append "Typescript definition files (*.d.ts) for '" type "'."))
    (home-page (string-append
      (git-reference-url (origin-uri source)) "/tree/master/types/" type))
    (license license:expat)))

(define-public node-types-node
  (node-types
    "node"
    "22.14.0" ; Match with version of node used
    "1f6ca6ff73af20b951f5ea6ecbea6668eff1750f"
    "05q0cj2b35z27fv1b00kr8ja5hj2dzl4shx1mwk0jg44z1cwkp0j"
    (delay (list node-undici-types))))

(define-public node-types-source-map-support
  (node-types
    "source-map-support"
    "0.5.10"
    "05766ab10a4987e93fdee7627f9fe9e7bc6d1a65"
    "1p2kakd1mhcps5c19wl65w9gkafd13qdy9hszriak4xpad8a2nz7"
    (delay (list node-source-map))))

(define-public node-typescript
  (package
    (name "node-typescript")
    (version "5.8.3")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/microsoft/TypeScript")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1dlwf83bc9kwnj35ng6ji9vl2hx23c9fl988rqq0hy29bmxl1rhm"))))
    (build-system node-build-system)
    (native-inputs (list
      node-dprint-formatter ; 0.x
      node-esbuild ; 0.x
      node-fast-xml-parser ; 4.x
      node-glob ; 10.x
      node-hereby ; 1.x
      node-jsonc-parser
      node-minimist ; 1.x
      node-picocolors ; 1.x
      node-source-map-support ; 0.x
      node-types-node ; Match the version of node
      node-types-source-map-support
      node-which ; 3.x
      sed))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'chai', 'mocha', and 'eslint'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dependencies (list
              ; TODO @dprint/typescript needs a rust build but it's only required to
              ; format the .d.ts files, however they're already somewhat formatted.
              "@dprint/typescript"
              "@esfx/canceltoken"
              "@eslint/js"
              "@octokit/rest"
              "@types/chai"
              "@types/diff"
              "@types/minimist"
              "@types/mocha"
              "@types/ms"
              "@types/which"
              "@typescript-eslint/rule-tester"
              "@typescript-eslint/type-utils"
              "@typescript-eslint/utils"
              "azure-devops-node-api"
              "c8"
              "chai"
              "chokidar"
              "chalk"
              "diff"
              "dprint"
              "eslint"
              "eslint-formatter-autolinkable-stylish"
              "eslint-plugin-regexp"
              "globals"
              "knip"
              "mocha"
              "mocha-fivemat-progress-reporter"
              "monocart-coverage-reports"
              "ms"
              "playwright"
              "tslib"
              "typescript"
              "typescript-eslint")))

          ; Remove imports of dependencies that are not used in building.
          (substitute*
            (list
              "Herebyfile.mjs"
              "scripts/build/tests.mjs"
              "scripts/build/utils.mjs")
            (("^import .* from \"(@esfx/canceltoken|chokidar|glob)\".*") ""))

          ; Replace @esfx/canceltoken CancelError
          (substitute* "scripts/build/utils.mjs"
            (("CancelError\\(\\)")
              "(function() {const error = new Error(\"Operation was canceled\");\
error.name=\"CancelError\";return error})()"))

          ; TODO: Once @dprint/typescript is added, remove these modifications
          ; to format typescript.d.ts.
          (define dtsBundler "scripts/dtsBundler.mjs")
          (substitute* dtsBundler
            (("^import .* from \"@dprint/typescript\".*") "")
            (("^import .* from \"typescript\";")
              "import * as ts from \"../built/local/typescript.js\";")
            (("^const buffer =.*") "")
            (("^const formatter =.*") ""))
          (invoke "sed" "-i" "/^formatter/,/;/d" dtsBundler)
          (invoke "sed" "-i"
            "/^function dprint(/,/^}/c\\function dprint(contents) {return contents;}"
            dtsBundler)))
        (replace 'build (lambda _
          ; First build tsc so it can be used in the rest of the build.
          (invoke "hereby" "tsc" "--bundle" "--no-typecheck")
          (invoke "hereby" "lkg" "--bundle" "--no-typecheck" "--built")))
        (add-after 'check 'prepare-package (lambda _
          (define output "output")
          (mkdir output)
          (for-each
            (lambda (file) (install-file file output))
            (list
              "LICENSE.txt"
              "package.json"
              "README.md"
              "SECURITY.md"
              "ThirdPartyNoticeText.txt"))
          (for-each
            (lambda (dir) (copy-recursively dir (string-append output "/" dir)))
            (list "bin" "lib"))
          (chdir output))))))
    (synopsis "TypeScript language for application scale JavaScript development")
    (description "TypeScript is a language for application-scale JavaScript. TypeScript\
 adds optional types to JavaScript that support tools for large-scale JavaScript\
 applications for any browser, for any host, on any OS. TypeScript compiles to readable,\
 standards-based JavaScript.")
    (home-page "https://www.typescriptlang.org/")
    (license license:expat)))

(define-public node-typical
  (package
    (name "node-typical")
    (version "5.2.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/75lb/typical")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "072fqdw7qrgh1cnri19z3rf702hh8p6agh3npkzwrywjmlnndfdm"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'test-runner'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Type-checking library for Javascript")
    (description "Isomorphic, functional type-checking for Javascript.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:expat)))

(define-public node-undici-types
  (package
    (name "node-undici-types")
    (version "7.8.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/nodejs/undici")
        (commit (string-append "v" version))))
      (file-name (git-file-name "node-undici" version))
      (sha256
        (base32 "044qy77m15m2pbk83pyyr6ql9imgmj4p8kdkn0v9qh9dnh5w6vdg"))))
    (build-system node-build-system)
    (arguments (list
      #:tests? #f ; No tests.
      #:phases #~(modify-phases %standard-phases
        (add-after 'unpack 'setup (lambda _
          (invoke "node" "scripts/generate-undici-types-package-json.js")
          (chdir "types"))))))
    (synopsis "Stand-alone types package for Undici")
    (description "Typescript definition (d.ts) files for Undici.")
    (home-page "https://undici.nodejs.org")
    (license license:expat)))

(define-public node-utf-8-validate
  (package
    (name "node-utf-8-validate")
    (version "6.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/websockets/utf-8-validate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nca4zynp7rpjlxcwyv7fhz5zhdhhdhwwl7lqxkv2rz00rc86i22"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dev-dependencies
           (lambda _
             (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list node-gyp-build node-addon-api))
    (native-inputs
     (list python))
    (home-page "https://github.com/websockets/utf-8-validate")
    (synopsis "UTF-8 validation for Node.js")
    (description "This package provides efficient UTF-8 data validation for
use with WebSocket implementations.")
    (license license:expat)))

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

(define-public node-which
  (package
    (name "node-which")
    (version "3.0.1")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/npm/node-which")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1ra5y2l02difjpvgh6la6g7jz782misn0n7d06miqlmvd26mldvk"))))
    (build-system node-build-system)
    (inputs (list
      node-isexe))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'tap'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Find the first instance of an executable in PATH")
    (description "Like which(1) unix command. Find the first instance of an executable\
 in the PATH.")
    (home-page (git-reference-url (origin-uri source)))
    (license license:isc)))

(define-public node-wordwrapjs
  (package
    (name "node-wordwrapjs")
    (version "4.0.1")
    (source (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://github.com/75lb/wordwrapjs")
        (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
        (base32 "126rsbhn2dkxq4dy2p2ylwcjdipgk9mkqp47y5n4l5pjkhyv7lwr"))))
    (build-system node-build-system)
    (inputs (list
      node-reduce-flatten
      node-typical))
    (arguments (list
      #:tests? #f ; FIXME: Tests require 'test-runner'.
      #:phases #~(modify-phases %standard-phases
        (add-before 'patch-dependencies 'modify-package (lambda _
          (modify-json
            (delete-dev-dependencies)))))))
    (synopsis "Word-wrapping library for javascript")
    (description "Word wrapping, with a few features.
  * force-break option
  * wraps hypenated words
  * multilingual - wraps any language that uses whitespace for word separation.")
    (home-page (git-reference-url (origin-uri source)))
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

(define-public node-ws
  (package
    (name "node-ws")
    (version "8.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/websockets/ws")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03ynm4n25yjza1qjgaz8ci2rjc3ryl710q9gr99p77r6ymcxkf34"))))
    (build-system node-build-system)
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'patch-dependencies 'delete-dev-dependencies
                 (lambda _
                   (modify-json (delete-dev-dependencies))))
               (add-before 'install 'set-cc
                 (lambda _
                   (setenv "CC" #$(cc-for-target)))))))
    (inputs
     (list node-bufferutil node-utf-8-validate))
    (native-inputs
     (list python))
    (home-page "https://github.com/websockets/ws")
    (synopsis "Simple WebSocket implementation for Node.js")
    (description "This package provides a WebSocket client and server
implementation.")
    (license license:expat)))

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
