;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021, 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024, 2025 Daniel Khodabakhsh <d@niel.khodabakh.sh>
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

(define-module (gnu packages node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system node)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

;; This should be the latest version of node that still builds without
;; depending on llhttp.
(define-public node-bootstrap
  (package
    (name "node")
    (version "10.24.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.xz"))
              (sha256
               (base32
                "032801kg24j04xmf09m0vxzlcz86sv21s24lv9l4cfv08k1c4byp"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Fix process.versions.XXXX assumption of always having
                  ;; a version string of major.minor.patch and not major.minor.
                  (substitute* "test/parallel/test-process-versions.js"
                    (("\\\\d\\+\\\\.\\\\d\\+\\\\.") "(\\d+\\.)+"))
                  ;; Remove bundled software.
                  (for-each delete-file-recursively
                            '("deps/cares"
                              "deps/http_parser"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/openssl"
                              "deps/uv"
                              "deps/zlib"))
                  (substitute* "Makefile"
                    ;; Remove references to bundled software.
                    (("deps/http_parser/http_parser.gyp") "")
                    (("deps/uv/include/\\*.h") "")
                    (("deps/uv/uv.gyp") "")
                    (("deps/zlib/zlib.gyp") ""))))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--shared-cares"
                           "--shared-http-parser"
                           "--shared-libuv"
                           "--shared-nghttp2"
                           "--shared-openssl"
                           "--shared-zlib"
                           "--without-snapshot"
                           "--without-intl")
       ;; Run only the CI tests.  The default test target requires additional
       ;; add-ons from NPM that are not distributed with the source.
       #:test-target "test-ci-js"
       #:modules
       ((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-1)
        (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-hardcoded-program-references
           (lambda* (#:key inputs #:allow-other-keys)

             ;; Fix hardcoded /bin/sh references.
             (substitute*
                 (let ((common
                        '("lib/child_process.js"
                          "lib/internal/v8_prof_polyfill.js"
                          "test/parallel/test-child-process-spawnsync-shell.js"
                          "test/parallel/test-stdio-closed.js"
                          "test/sequential/test-child-process-emfile.js"))
                       ;; not in bootstap node:
                       (sigxfsz "test/parallel/test-fs-write-sigxfsz.js"))
                   (if (file-exists? sigxfsz)
                       (cons sigxfsz common)
                       common))
               (("'/bin/sh'")
                (string-append "'"  (search-input-file inputs "/bin/sh") "'")))

             ;; Fix hardcoded /usr/bin/env references.
             (substitute* '("test/parallel/test-child-process-default-options.js"
                            "test/parallel/test-child-process-env.js"
                            "test/parallel/test-child-process-exec-env.js")
               (("'/usr/bin/env'")
                (string-append "'" (assoc-ref inputs "coreutils")
                               "/bin/env'")))))
         (add-after 'patch-hardcoded-program-references
             'delete-problematic-tests
           (lambda* (#:key inputs #:allow-other-keys)

             ;; FIXME: These tests fail in the build container, but they don't
             ;; seem to be indicative of real problems in practice.
             (for-each delete-file
                       '("test/parallel/test-cluster-master-error.js"
                         "test/parallel/test-cluster-master-kill.js"
                         ;; See also <https://github.com/nodejs/node/issues/25903>.
                         "test/sequential/test-performance.js"))

             ;; These tests fail on recent versions of nghttp2
             (for-each delete-file
                       '("test/parallel/test-http2-methods.js"
                         "test/parallel/test-http2-multiplex.js"))

             ;; This requires a DNS resolver.
             (delete-file "test/parallel/test-dns.js")

             ;; This test is timing-sensitive, and fails sporadically on
             ;; slow, busy, or even very fast machines.
             (delete-file "test/parallel/test-fs-utimes.js")

             ;; FIXME: This test fails randomly:
             ;; https://github.com/nodejs/node/issues/31213
             (delete-file "test/parallel/test-net-listen-after-destroying-stdin.js")

             ;; FIXME: These tests fail on armhf-linux:
             ;; https://github.com/nodejs/node/issues/31970
             ,@(if (string-prefix? "arm" (%current-system))
                   '((for-each delete-file
                               '("test/parallel/test-zlib.js"
                                 "test/parallel/test-zlib-brotli.js"
                                 "test/parallel/test-zlib-brotli-flush.js"
                                 "test/parallel/test-zlib-brotli-from-brotli.js"
                                 "test/parallel/test-zlib-brotli-from-string.js"
                                 "test/parallel/test-zlib-convenience-methods.js"
                                 "test/parallel/test-zlib-random-byte-pipes.js"
                                 "test/parallel/test-zlib-write-after-flush.js")))
                   '())

             ;; These tests have an expiry date: they depend on the validity of
             ;; TLS certificates that are bundled with the source.  We want this
             ;; package to be reproducible forever, so remove those.
             ;; TODO: Regenerate certs instead.
             (for-each delete-file
                       '("test/parallel/test-tls-passphrase.js"
                         "test/parallel/test-tls-server-verify.js"))))
         (add-before 'configure 'set-bootstrap-host-rpath
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (let* ((inputs      (or native-inputs inputs))
                    (c-ares      (assoc-ref inputs "c-ares"))
                    (http-parser (assoc-ref inputs "http-parser"))
                    (nghttp2     (assoc-ref inputs "nghttp2"))
                    (openssl     (assoc-ref inputs "openssl"))
                    (libuv       (assoc-ref inputs "libuv"))
                    (zlib        (assoc-ref inputs "zlib")))
               (substitute* "deps/v8/gypfiles/v8.gyp"
                 (("'target_name': 'torque'," target)
                  (string-append target
                                 "'ldflags': ['-Wl,-rpath="
                                 c-ares "/lib:"
                                 http-parser "/lib:"
                                 nghttp2 "/lib:"
                                 openssl "/lib:"
                                 libuv "/lib:"
                                 zlib "/lib"
                                 "'],"))))))
         (replace 'configure
           ;; Node's configure script is actually a python script, so we can't
           ;; run it with bash.
           (lambda* (#:key outputs (configure-flags '()) native-inputs inputs
                     #:allow-other-keys)
             (let* ((prefix (assoc-ref outputs "out"))
                    (xflags ,(if (%current-target-system)
                                 `'("--cross-compiling"
                                    ,(string-append
                                      "--dest-cpu="
                                      (match (%current-target-system)
                                        ((? (cut string-prefix? "arm" <>))
                                         "arm")
                                        ((? (cut string-prefix? "aarch64" <>))
                                         "arm64")
                                        ((? (cut string-prefix? "i686" <>))
                                         "ia32")
                                        ((? (cut string-prefix? "x86_64" <>))
                                         "x64")
                                        ((? (cut string-prefix? "powerpc64" <>))
                                         "ppc64")
                                        ((? (cut string-prefix? "riscv64" <>))
                                         "riscv64")
                                        (_ "unsupported"))))
                                 ''()))
                    (flags (cons (string-append "--prefix=" prefix)
                                 (append xflags configure-flags))))
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               ;; Node's configure script expects the CC environment variable to
               ;; be set.
               (setenv "CC_host" "gcc")
               (setenv "CXX_host" "g++")
               (setenv "CC" ,(cc-for-target))
               (setenv "CXX" ,(cxx-for-target))
               (setenv "PKG_CONFIG" ,(pkg-config-for-target))
               (apply invoke
                      (let ((inpts (or native-inputs inputs)))
                        (with-exception-handler
                            (lambda (e)
                              (if (search-error? e)
                                  (search-input-file inpts "/bin/python3")
                                  (raise-exception e)))
                          (lambda ()
                            (search-input-file inpts "/bin/python"))
                          #:unwind? #t))
                      "configure"
                      flags))))
         (add-after 'patch-shebangs 'patch-nested-shebangs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Based on the implementation of patch-shebangs
             ;; from (guix build gnu-build-system).
             (let ((path (append-map (match-lambda
                                       ((_ . dir)
                                        (list (string-append dir "/bin")
                                              (string-append dir "/sbin")
                                              (string-append dir "/libexec"))))
                                     (append outputs inputs))))
               (for-each
                (lambda (file)
                  (patch-shebang file path))
                (find-files (search-input-directory outputs "lib/node_modules")
                            (lambda (file stat)
                              (executable-file? file))
                            #:stat lstat)))))
         (add-after 'install 'install-npmrc
           ;; Note: programs like node-gyp only receive these values if
           ;; they are started via `npm` or `npx`.
           ;; See: https://github.com/nodejs/node-gyp#npm-configuration
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (with-output-to-file
                   ;; Use the config file "primarily for distribution
                   ;; maintainers" rather than "{prefix}/etc/npmrc",
                   ;; especially because node-build-system uses --prefix
                   ;; to install things to their store paths:
                   (string-append out "/lib/node_modules/npm/npmrc")
                 (lambda ()
                   ;; Tell npm (mostly node-gyp) where to find our
                   ;; installed headers so it doesn't try to
                   ;; download them from the internet:
                   (format #t "nodedir=~a\n" out)))))))))
    (native-inputs
     ;; Runtime dependencies for binaries used as a bootstrap.
     (list c-ares-for-node-bootstrap
           http-parser
           libuv-for-node
           `(,nghttp2 "lib")
           openssl-1.1
           zlib
           ;; Regular build-time dependencies.
           gcc-11 ; GCC > 11 produces a broken binary
           perl
           pkg-config
           procps
           python-2
           util-linux))
    (native-search-paths
     (list (search-path-specification
            (variable "NODE_PATH")
            (files '("lib/node_modules")))))
    (inputs
     (list bash-minimal
           coreutils
           c-ares-for-node-bootstrap
           http-parser
           libuv-for-node
           `(,nghttp2 "lib")
           openssl
           python-wrapper               ;for node-gyp (supports python3)
           zlib))
    (synopsis "Evented I/O for V8 JavaScript")
    (description
     "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (supported-systems (fold delete %supported-systems
                             '("powerpc-linux" "riscv64-linux")))
    (home-page "https://nodejs.org/")
    (license license:expat)
    (properties '((max-silent-time . 7200)   ;2h, needed on ARM
                  (timeout . 21600)          ;6h
                  (cpe-name . "node.js")
                  (hidden? . #t)))))

;; Duplicate of node-semver
(define-public node-semver-bootstrap
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
     `(#:node ,node-bootstrap
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies '("tap"))))))))
    (home-page "https://github.com/npm/node-semver")
    (properties '((hidden? . #t)))
    (synopsis "Parses semantic versions strings")
    (description
     "@code{node-semver} is a JavaScript implementation of the
@uref{https://semver.org/, SemVer.org} specification.")
    (license license:isc)))

(define-public node-ms-bootstrap
  (package
    (name "node-ms")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vercel/ms.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pjxzbi4j8pinlsc7yxvfrh0b47kb2dc4lfc2rjq4wx5bdwl33fj"))))
    (build-system node-build-system)
    (arguments
     `(#:node ,node-bootstrap
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           '("eslint"
                             "expect.js"
                             "husky"
                             "lint-staged"
                             "mocha"))))))))
    (home-page "https://github.com/zeit/ms#readme")
    (properties '((hidden? . #t)))
    (synopsis "Tiny millisecond conversion utility")
    (description "Use this package to easily convert various time
formats to milliseconds.")
    (license license:expat)))

(define-public node-binary-search-bootstrap
  (package
    (name "node-binary-search")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/darkskyapp/binary-search.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1xr2msdc143cd3xwgq7n3rhzy7j8wrnaidxl0r6l6b6g3mpbpjig"))))
    (build-system node-build-system)
    (arguments
     `(#:node ,node-bootstrap
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies `("chai" "mocha"))))))))
    (home-page "https://github.com/darkskyapp/binary-search#readme")
    (properties '((hidden? . #t)))
    (synopsis "Tiny binary search function with comparators")
    (description "This package is a binary search function for Node.js.")
    (license license:cc0)))

(define-public node-debug-bootstrap
  (package
    (name "node-debug")
    (version "4.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/visionmedia/debug.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08g52r1d4yqcsfdfb7n5if33d4cghaq75gx5n9hj6m6fd8jfp2pi"))))
    (build-system node-build-system)
    (arguments
     `(#:node ,node-bootstrap
       #:tests? #f
       #:phases
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
                             "xo"))))))))
    (inputs (list node-ms-bootstrap))
    (home-page "https://github.com/visionmedia/debug#readme")
    (properties '((hidden? . #t)))
    (synopsis "Small debugging utility")
    (description "This package contains a tiny JavaScript debugging
utility modelled after Node.js core's debugging technique.  It works in
Node.js and web browsers.")
    (license license:expat)))

(define-public node-llparse-builder-bootstrap
  (package
    (name "node-llparse-builder")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/indutny/llparse-builder.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r82iiwqsb73k2fxw7842rjjiixllxpyc6yl9cq4ma6ybkf6xmzm"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; FIXME: Unneeded runtime dependency.
           ;; https://github.com/indutny/llparse-builder/pull/2
           (substitute* "package.json"
             (("\"@types/debug.*,") ""))
           ;; Fix imports for esbuild.
           ;; https://github.com/evanw/esbuild/issues/477
           (substitute* '("src/node/invoke.ts"
                          "src/node/base.ts"
                          "src/node/consume.ts"
                          "src/node/match.ts"
                          "src/node/error.ts"
                          "src/node/pause.ts"
                          "src/edge.ts"
                          "src/utils.ts"
                          "src/loop-checker/index.ts"
                          "src/loop-checker/lattice.ts"
                          "src/code/field.ts"
                          "src/span-allocator.ts")
             (("\\* as assert") "assert")
             (("\\* as debugAPI") "debugAPI"))
           #t))))
    (build-system node-build-system)
    (arguments
     `(#:node ,node-bootstrap
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda _
             (modify-json (delete-dependencies
                           `("@types/mocha"
                             "@types/node"
                             "mocha"
                             "ts-node"
                             "tslint"
                             "typescript")))))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((esbuild (search-input-file inputs "/bin/esbuild")))
               (invoke esbuild
                       "--platform=node"
                       "--outfile=lib/builder.js"
                       "--bundle"
                       "src/builder.ts")))))))
    (inputs
     (list node-binary-search-bootstrap node-debug-bootstrap))
    (native-inputs
     (list esbuild))
    (home-page "https://github.com/indutny/llparse-builder#readme")
    (properties '((hidden? . #t)))
    (synopsis "Graph builder for consumption by llparse")
    (description "This package builds graphs for consumption by llparse.")
    (license license:expat)))

(define-public node-llparse-frontend-bootstrap
  (package
    (name "node-llparse-frontend")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/indutny/llparse-frontend.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rm9g4ifyip30svm5cgnf0gx7d45jgh4mpf2hkd092xhngmfvicc"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Fix imports for esbuild.
           ;; https://github.com/evanw/esbuild/issues/477
           (substitute* '("src/frontend.ts"
                          "src/code/field-value.ts"
                          "src/container/index.ts"
                          "src/container/wrap.ts"
                          "src/node/sequence.ts"
                          "src/node/single.ts"
                          "src/node/table-lookup.ts"
                          "src/trie/index.ts")
             (("\\* as assert") "assert")
             (("\\* as debugAPI") "debugAPI"))
           #t))))
    (build-system node-build-system)
    (arguments
     `(#:node ,node-bootstrap
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           `("@types/debug"
                             "@types/mocha"
                             "@types/node"
                             "mocha"
                             "ts-node"
                             "tslint"
                             "typescript")))))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((esbuild (search-input-file inputs "/bin/esbuild")))
               (invoke esbuild
                       "--platform=node"
                       "--outfile=lib/frontend.js"
                       "--bundle"
                       "src/frontend.ts")))))))
    (inputs
     (list node-debug-bootstrap node-llparse-builder-bootstrap))
    (native-inputs
     (list esbuild))
    (home-page "https://github.com/indutny/llparse-frontend#readme")
    (properties '((hidden? . #t)))
    (synopsis "Frontend for the llparse compiler")
    (description "This package is a frontend for the llparse compiler.")
    (license license:expat)))

(define-public node-llparse-bootstrap
  (package
    (name "node-llparse")
    (version "7.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/indutny/llparse.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10da273iy2if88hp79cwms6c8qpsl1fkgzll6gmqyx5yxv5mkyp6"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Fix imports for esbuild.
           ;; https://github.com/evanw/esbuild/issues/477
           (substitute* '("src/compiler/index.ts"
                          "src/implementation/c/node/base.ts"
                          "src/implementation/c/node/table-lookup.ts"
                          "src/implementation/c/compilation.ts"
                          "src/implementation/c/helpers/match-sequence.ts"
                          "src/implementation/c/code/mul-add.ts")
             (("\\* as assert") "assert")
             (("\\* as debugAPI") "debugAPI"))
           #t))))
    (build-system node-build-system)
    (arguments
     `(#:node ,node-bootstrap
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json (delete-dependencies
                           `("@types/debug"
                             "@types/mocha"
                             "@types/node"
                             "esm"
                             "llparse-test-fixture"
                             "mocha"
                             "ts-node"
                             "tslint"
                             "typescript")))))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((esbuild (search-input-file inputs "/bin/esbuild")))
               (invoke esbuild
                       "--platform=node"
                       "--outfile=lib/api.js"
                       "--bundle"
                       "src/api.ts")))))))
    (inputs
     (list node-debug-bootstrap node-llparse-frontend-bootstrap))
    (native-inputs
     (list esbuild))
    (home-page "https://github.com/nodejs/llparse#readme")
    (properties '((hidden? . #t)))
    (synopsis "Compile incremental parsers to C code")
    (description "This package offers an API for compiling an incremental
parser definition into a C output.")
    (license license:expat)))

(define-public llhttp-bootstrap
  (package
    (name "llhttp")
    (version "9.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nodejs/llhttp.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mzg19aqb1am498gms0z75cwd5kmfg9p78b1hhxw67019nsjcbac"))
              (patches (search-patches "llhttp-ponyfill-object-fromentries.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Fix imports for esbuild.
                  ;; https://github.com/evanw/esbuild/issues/477
                  (substitute* "src/llhttp/http.ts"
                    (("\\* as assert") "assert"))
                  (substitute* "Makefile"
                    (("npx ts-node bin/generate.ts")
                     "node bin/generate.js"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "CLANG=" ,(cc-for-target))
                          (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((esbuild (search-input-file (or native-inputs inputs)
                                               "/bin/esbuild")))
               (invoke esbuild
                       "--platform=node"
                       "--target=node10"
                       "--outfile=bin/generate.js"
                       "--bundle" "bin/generate.ts"))))
         (add-before 'install 'create-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (dir)
                           (mkdir-p (string-append out dir)))
                         (list "/lib" "/include" "/src"))
               #t)))
         (add-after 'install 'install-src
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (src-dir (string-append out "/src")))
               (install-file "build/c/llhttp.c" src-dir)
               (install-file "src/native/api.c" src-dir)
               (install-file "src/native/http.c" src-dir)
               #t))))))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("node" ,node-bootstrap)
       ("node-semver" ,node-semver-bootstrap)
       ("node-llparse-bootstrap" ,node-llparse-bootstrap)))
    (home-page "https://github.com/nodejs/llhttp")
    (properties '((hidden? . #t)))
    (synopsis "Parser for HTTP messages")
    (description "This is a rewrite of
@url{https://github.com/nodejs/http-parser, http-parser} using
@url{https://github.com/nodejs/llparse, llparse} to generate the C
source files.")
    (license license:expat)))

(define-public node-lts
  (package
    (inherit node-bootstrap)
    (version "22.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "12msprh604s6qdsgwymxw4kl8ivaldbaydf4v37lbp02aznk2kkc"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; openssl.cnf is required for build.
                  (for-each delete-file-recursively
                            (find-files "deps/openssl"
                                        (lambda (file stat)
                                          (not (string-contains file "nodejs-openssl.cnf")))))
                  ;; Remove bundled software, where possible
                  (for-each delete-file-recursively
                            '("deps/brotli"
                              "deps/cares"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/ngtcp2"
                              "deps/uv"
                              "deps/zlib"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments node-bootstrap)
       ((#:configure-flags configure-flags)
        ''("--shared-cares"
           "--shared-libuv"
           "--shared-nghttp2"
           "--shared-openssl"
           "--shared-zlib"
           "--shared-brotli"
           "--with-intl=system-icu"
           "--shared-ngtcp2"
           "--shared-nghttp3"
           ;;Needed for correct snapshot checksums
           "--v8-enable-snapshot-compression"))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'set-bootstrap-host-rpath
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (let* ((inputs        (or native-inputs inputs))
                      (c-ares        (assoc-ref inputs "c-ares"))
                      (brotli        (assoc-ref inputs "brotli"))
                      (icu4c         (assoc-ref inputs "icu4c"))
                      (nghttp2       (assoc-ref inputs "nghttp2"))
                      (openssl       (assoc-ref inputs "openssl"))
                      (libuv         (assoc-ref inputs "libuv"))
                      (zlib          (assoc-ref inputs "zlib"))
                      (host-binaries '("torque"
                                       "bytecode_builtins_list_generator"
                                       "gen-regexp-special-case"
                                       "node_mksnapshot"
                                       "mksnapshot")))
                 (substitute* '("node.gyp" "tools/v8_gypfiles/v8.gyp")
                   (((string-append "'target_name': '("
                                    (string-join host-binaries "|")
                                    ")',")
                      target)
                    (string-append target
                                   "'ldflags': ['-Wl,-rpath="
                                   c-ares "/lib:"
                                   brotli "/lib:"
                                   icu4c "/lib:"
                                   nghttp2 "/lib:"
                                   openssl "/lib:"
                                   libuv "/lib:"
                                   zlib "/lib"
                                   "'],"))))))
           (add-after 'patch-hardcoded-program-references
                      'patch-additional-hardcoded-program-references
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "test/parallel/test-stdin-from-file-spawn.js"
                 (("'/bin/sh'") (string-append
                                 "'" (search-input-file inputs "/bin/sh")
                                 "'")))))
           (replace 'delete-problematic-tests
             (lambda* (#:key inputs #:allow-other-keys)
               ;; FIXME: These tests fail in the build container, but they don't
               ;; seem to be indicative of real problems in practice.
               (for-each delete-file
                         '("test/parallel/test-cluster-primary-error.js"
                           "test/parallel/test-cluster-primary-kill.js"
                           "test/parallel/test-node-run.js"))

               ;; These require a DNS resolver.
               (for-each delete-file
                         '("test/parallel/test-dns.js"
                           "test/parallel/test-dns-lookupService-promises.js"
                           "test/parallel/test-net-socket-connect-without-cb.js"
                           "test/parallel/test-tcp-wrap-listen.js"
                           "test/report/test-report-exclude-network.js"))

               ;; These tests require networking.
               (for-each delete-file
                         '("test/parallel/test-https-agent-unref-socket.js"))

               ;; This test is timing-sensitive, and fails sporadically on
               ;; slow, busy, or even very fast machines.
               (delete-file "test/parallel/test-fs-utimes.js")

               ;; FIXME: This test fails randomly:
               ;; https://github.com/nodejs/node/issues/31213
               (delete-file "test/parallel/test-net-listen-after-destroying-stdin.js")

               ;; FIXME: These tests fail on armhf-linux:
               ;; https://github.com/nodejs/node/issues/31970
               ,@(if (target-arm32?)
                     '((for-each delete-file
                                 '("test/parallel/test-zlib.js"
                                   "test/parallel/test-zlib-brotli.js"
                                   "test/parallel/test-zlib-brotli-flush.js"
                                   "test/parallel/test-zlib-brotli-from-brotli.js"
                                   "test/parallel/test-zlib-brotli-from-string.js"
                                   "test/parallel/test-zlib-convenience-methods.js"
                                   "test/parallel/test-zlib-random-byte-pipes.js"
                                   "test/parallel/test-zlib-write-after-flush.js")))
                     '())

               ;; https://github.com/nodejs/node/issues/45906
               ;; This test depends on 64-bit time_t so skipping on 32-bit systems.
               ,@(if (not (target-64bit?))
                     '((delete-file "test/parallel/test-fs-utimes-y2K38.js")

                       (delete-file "test/parallel/test-debugger-heap-profiler.js"))
                     '())

               ;; These tests have an expiry date: they depend on the validity of
               ;; TLS certificates that are bundled with the source.  We want this
               ;; package to be reproducible forever, so remove those.
               ;; TODO: Regenerate certs instead.
               (for-each delete-file
                         '("test/parallel/test-tls-passphrase.js"
                           "test/parallel/test-tls-server-verify.js"))

               ;; These tests fail when linking to upstream libuv.
               ;; https://github.com/nodejs/node/commit/3f6addd590
               (for-each delete-file
                         '("test/parallel/test-process-euid-egid.js"
                           "test/parallel/test-process-initgroups.js"
                           "test/parallel/test-process-setgroups.js"
                           "test/parallel/test-process-uid-gid.js"))))
           (add-after 'delete-problematic-tests 'patch-problematic-tests
             (lambda _
               ;; XXX: These tests seem to not work by default
               (substitute*
                   '("test/parallel/test-http2-premature-close.js"
                     "test/parallel/test-http2-invalid-last-stream-id.js")
                 (("client\\.connect\\(address\\)")
                  "client.connect(address.port)"))))
           (add-after 'delete-problematic-tests 'replace-llhttp-sources
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Replace pre-generated llhttp sources
               (let ((llhttp (assoc-ref inputs "llhttp")))
                 (copy-file (string-append llhttp "/src/llhttp.c")
                            "deps/llhttp/src/llhttp.c")
                 (copy-file (string-append llhttp "/src/api.c")
                            "deps/llhttp/src/api.c")
                 (copy-file (string-append llhttp "/src/http.c")
                            "deps/llhttp/src/http.c")
                 (copy-file (string-append llhttp "/include/llhttp.h")
                            "deps/llhttp/include/llhttp.h"))))
           ;; npm installs dependencies by copying their files over a tar
           ;; stream.  A file with more than one hardlink is marked as a
           ;; "Link".  pacote/lib/fetcher.js calls node-tar's extractor with a
           ;; filter that ignores any "Link" entries.  This means that
           ;; dependending on the number of hardlinks on files in a node-*
           ;; package *some* of its files may not be installed when generating
           ;; another package's "node_modules" directory.  The build output
           ;; would differ depending on irrelevant file system state.
           ;;
           ;; To avoid this, we patch node-tar to treat files with hardlinks
           ;; the same as any other file, so that node-tar has no choice but
           ;; to extract all of them --- independent of pacote's filter.
           ;;
           ;; Why not patch pacote's filter instead?  This has led to subtle
           ;; differences in where the files are installed, so it's easier to
           ;; just ensure that files with hardlinks are always treated as
           ;; regular files.
           ;;
           ;; Discussion:
           ;;   https://lists.gnu.org/archive/html/guix-devel/2023-07/msg00040.html
           ;; Upstream bug report:
           ;;   https://github.com/npm/pacote/issues/285
           (add-after 'install 'ignore-number-of-hardlinks
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((file (string-append (assoc-ref outputs "out")
                                          "/lib/node_modules/npm/node_modules"
                                          "/tar/lib/write-entry.js")))
                 (substitute* file
                   (("this.stat.nlink > 1") "false")))))
           (add-after 'install 'install-node-gyp-wrapper
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (dir (string-append out "/lib/node_modules/npm/bin/node-gyp-bin"))
                      (file (string-append dir "/node-gyp")))
                 (mkdir-p dir)
                 ;; See https://github.com/npm/cli/issues/6842
                 (call-with-output-file file
                   (lambda (port)
                     (format port "#!~a/bin/sh
if [ \"x$npm_config_node_gyp\" = \"x\" ]; then
  ~a/bin/node \"~a/lib/node_modules/npm/node_modules/node-gyp/bin/node-gyp.js\" \"$@\"
else
  \"$npm_config_node_gyp\" \"$@\"
fi"
                             (assoc-ref inputs "bash")
                             out
                             out)))
                 (chmod file #o555))))))))
    (native-inputs
     (list ;; Runtime dependencies for binaries used as a bootstrap.
           c-ares-for-node-lts
           brotli
           icu4c-76
           libuv-for-node-lts
           `(,nghttp2 "lib")
           openssl
           zlib
           ; ngtcp2? nghttp3?
           ;; Regular build-time dependencies.
           perl
           pkg-config
           procps
           python
           util-linux))
    (inputs
     (list bash-minimal
           coreutils
           c-ares-for-node-lts
           icu4c-76
           libuv-for-node-lts
           llhttp-bootstrap
           brotli
           ngtcp2
           nghttp3
           `(,nghttp2 "lib")
           openssl
           zlib))
    (supported-systems
      (cons "riscv64-linux" (package-supported-systems node-bootstrap)))
    (properties (alist-delete 'hidden? (package-properties node-bootstrap)))))

(define-public node node-lts)

(define-public libnode
  (package/inherit node-lts
    (name "libnode")
    (arguments
     (substitute-keyword-arguments (package-arguments node-lts)
       ((#:configure-flags flags ''())
        `(cons* "--shared" "--without-npm" ,flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (delete 'install-npmrc)
           (delete 'patch-nested-shebangs)
           (delete 'ignore-number-of-hardlinks)))))))
