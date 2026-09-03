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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix build-system gnu)
  #:use-module (guix derivations)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

;; Stripped-down build phases for JS packages.  Uses esbuild + NODE_PATH instead
;; of npm, so editing node-build-system does not rebuild node-lts.  Set the
;; ENTRYPOINT environment variable before the build phase to override the entry
;; point auto-detected from package.json's "main" field, used when "main" points
;; to compiled output but the TypeScript source is what esbuild should bundle.
(define set-node-path-phase
  #~(lambda* (#:key inputs native-inputs #:allow-other-keys)
      (setenv "NODE_PATH"
              (string-join
               (delq #f
                     (map (lambda (input)
                            (let ((dir (string-append (cdr input) "/lib/node_modules")))
                              (and (file-exists? dir) dir)))
                          (append (or native-inputs '()) inputs)))
               ":"))))

(define (bootstrap-node-phases)
  (with-extensions (list guile-json-4)
    #~(begin
        (use-modules (json))
        (modify-phases %standard-phases
          (replace 'configure #$set-node-path-phase)
          (replace 'build
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let* ((all-inputs (append (or native-inputs '()) inputs))
                     (esbuild (search-input-file all-inputs "/bin/esbuild"))
                     (pkg (call-with-input-file "package.json" json->scm))
                     (entry (or (getenv "ENTRYPOINT")
                                (assoc-ref pkg "main")
                                "index.js")))
                (invoke esbuild "--bundle" "--platform=node" "--format=cjs"
                        "--outfile=bundle.js" entry))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (pkg (call-with-input-file "package.json" json->scm))
                     (name (assoc-ref pkg "name"))
                     (dest (string-append out "/lib/node_modules/" name)))
                (mkdir-p dest)
                (call-with-output-file (string-append dest "/package.json")
                  (lambda (port)
                    (scm->json (assoc-set! pkg "main" "./index.js") port)))
                (copy-file "bundle.js" (string-append dest "/index.js")))))))))

(define-public node-semver-bootstrap
  (package
    (name "node-semver-bootstrap")
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
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases (bootstrap-node-phases)))
    (native-inputs (list esbuild))
    (home-page "https://github.com/npm/node-semver")
    (properties '((hidden? . #t)))
    (synopsis "Parses semantic versions strings")
    (description
     "@code{node-semver} is a JavaScript implementation of the
@uref{https://semver.org/, SemVer.org} specification.")
    (license license:isc)))

(define-public node-llparse-builder-bootstrap
  ;; The binary-search package is vendored in 1351b75.  It's not unvendored
  ;; here because it's small, and as a bootstrap package doesn't bring
  ;; deduplication to a profile.
  (let ((commit "b04a70c8a3344adcfb8f5132528f7ae9852af79b")
        (revision "1"))
    (package
      (name "node-llparse-builder-bootstrap")
      (version (git-version "1.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/nodejs/llparse-builder.git")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "08wsxyz7hhaf1n3jgvlnhhaaafq576wmpf4k4whihz012i4sxncg"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f
        #:phases
        #~(modify-phases #$(bootstrap-node-phases)
            (add-after 'unpack 'fix-imports-for-esbuild
              ;; https://github.com/evanw/esbuild/issues/477
              (lambda _
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
                  (("\\* as assert") "assert"))))
            (add-before 'build 'set-entrypoint
              (lambda _
                (setenv "ENTRYPOINT" "src/builder.ts"))))))
      (native-inputs
       (list esbuild))
      (home-page "https://github.com/nodejs/llparse-builder#readme")
      (properties '((hidden? . #t)))
      (synopsis "Graph builder for consumption by llparse")
      (description "This package builds graphs for consumption by llparse.")
      (license license:expat))))

(define-public node-llparse-frontend-bootstrap
  (package
    (name "node-llparse-frontend-bootstrap")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nodejs/llparse-frontend.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rm9g4ifyip30svm5cgnf0gx7d45jgh4mpf2hkd092xhngmfvicc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases #$(bootstrap-node-phases)
          (add-after 'unpack 'fix-imports-for-esbuild
            ;; https://github.com/evanw/esbuild/issues/477
            (lambda _
              (substitute* '("src/frontend.ts"
                             "src/code/field-value.ts"
                             "src/container/index.ts"
                             "src/container/wrap.ts"
                             "src/node/sequence.ts"
                             "src/node/single.ts"
                             "src/node/table-lookup.ts"
                             "src/trie/index.ts")
                (("\\* as assert") "assert"))
              ;; Drop debug dependency, see
              ;; https://github.com/nodejs/llparse-frontend/pull/8
              (substitute* "src/frontend.ts"
                (("import \\* as debugAPI from 'debug';")
                 "import { debuglog } from 'node:util';")
                (("debugAPI")
                 "debuglog"))))
          (add-before 'build 'set-entrypoint
            (lambda _
              (setenv "ENTRYPOINT" "src/frontend.ts"))))))
    (inputs
     (list node-llparse-builder-bootstrap))
    (native-inputs
     (list esbuild))
    (home-page "https://github.com/nodejs/llparse-frontend#readme")
    (properties '((hidden? . #t)))
    (synopsis "Frontend for the llparse compiler")
    (description "This package is a frontend for the llparse compiler.")
    (license license:expat)))

(define-public node-llparse-bootstrap
  (package
    (name "node-llparse-bootstrap")
    (version "7.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nodejs/llparse.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0srahcyqgnx2f2w0hwsayx7ihvqgll6a6bcwrsiyvccis4hccww2"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases #$(bootstrap-node-phases)
          (add-after 'unpack 'fix-imports-for-esbuild
            ;; https://github.com/evanw/esbuild/issues/477
            (lambda _
              (substitute* '("src/compiler/index.ts"
                             "src/implementation/c/node/base.ts"
                             "src/implementation/c/node/table-lookup.ts"
                             "src/implementation/c/compilation.ts"
                             "src/implementation/c/helpers/match-sequence.ts"
                             "src/implementation/c/code/mul-add.ts")
                (("\\* as assert") "assert"))
              ;; Drop debug dependency, see
              ;; https://github.com/nodejs/llparse/pull/87
              (substitute* "src/compiler/index.ts"
                (("import \\* as debugAPI from 'debug';")
                 "import { debuglog } from 'node:util';")
                (("debugAPI")
                 "debuglog"))))
          (add-before 'build 'set-entrypoint
            (lambda _
              (setenv "ENTRYPOINT" "src/api.ts"))))))
    (inputs
     (list node-llparse-frontend-bootstrap))
    (native-inputs
     (list esbuild))
    (home-page "https://github.com/nodejs/llparse#readme")
    (properties '((hidden? . #t)))
    (synopsis "Compile incremental parsers to C code")
    (description "This package offers an API for compiling an incremental
parser definition into a C output.")
    (license license:expat)))

(define-public node-lts
  (package
    (name "node")
    (version "24.21.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.xz"))
              (sha256
               (base32
                "1z4lxmf5zh0mjzr63kqj2ybf1d79g0gdc4xs3ps88z7xnvplvxd6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; openssl.cnf is required for build.
                  (for-each delete-file-recursively
                            (find-files "deps/openssl"
                                        (lambda (file stat)
                                          (not (string-contains file "nodejs-openssl.cnf")))))
                  ;; [temp.names] requires a 'template' when calling a template
                  ;; member via a dependent expression.  This header is compiled
                  ;; for 32-bit targets (V8_TARGET_ARCH_32_BIT), so
                  ;; non-conformance goes unnoticed elsewhere.  Coincidentally
                  ;; fixed in upstream v8 at:
                  ;; https://chromium-review.googlesource.com/c/v8/v8/+/6830052
                  (substitute*
                      "deps/v8/src/compiler/turboshaft/int64-lowering-reducer.h"
                    (("__ Tuple<Word32, Word32>")
                     "__ template Tuple<Word32, Word32>"))
                  ;; Remove bundled software, where possible
                  (for-each delete-file-recursively
                            '("deps/brotli"
                              "deps/cares"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/ngtcp2"
                              "deps/llhttp"
                              "deps/uv"
                              "deps/uvwasi"
                              "deps/zlib"
                              "deps/zstd"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--shared-cares"
                           "--shared-libuv"
                           "--shared-http-parser"
                           "--shared-http-parser-libname=llhttp"
                           "--shared-nghttp2"
                           "--shared-openssl"
                           "--shared-zlib"
                           "--shared-brotli"
                           "--with-intl=system-icu"
                           "--shared-ngtcp2"
                           "--shared-nghttp3"
                           "--shared-zstd"
                           "--shared-uvwasi"
                           "--shared"
                           ;; Needed for correct snapshot checksums
                           "--v8-enable-snapshot-compression")
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
             (substitute* '("lib/child_process.js"
                            "lib/internal/v8_prof_polyfill.js"
                            "test/parallel/test-child-process-spawnsync-shell.js"
                            "test/parallel/test-stdio-closed.js"
                            "test/sequential/test-child-process-emfile.js"
                            "test/parallel/test-fs-write-sigxfsz.js"
                            "test/parallel/test-stdin-from-file-spawn.js")
               (("'/bin/sh'")
                (string-append "'" (search-input-file inputs "/bin/sh") "'")))
             ;; Fix hardcoded /usr/bin/env references.
             (substitute* '("test/parallel/test-child-process-default-options.js"
                            "test/parallel/test-child-process-env.js"
                            "test/parallel/test-child-process-exec-env.js")
               (("'/usr/bin/env'")
                (string-append "'" (search-input-file inputs "/bin/env") "'")))))
         (add-after 'patch-hardcoded-program-references 'delete-problematic-tests
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
             ;; These tests are timing-sensitive, and fail sporadically on
             ;; slow, busy, or even very fast machines.
             (for-each delete-file
                       '("test/parallel/test-fs-utimes.js"))
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
             ,@(if (target-32bit?)
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
             ;; TODO: These tests seem to not work by default, but seem fixed now online
             (substitute*
                 '("test/parallel/test-http2-premature-close.js"
                   "test/parallel/test-http2-invalid-last-stream-id.js")
               (("client\\.connect\\(address\\)")
                "client.connect(address.port)"))))
         (add-before 'configure 'set-bootstrap-host-rpath
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
         (add-after 'patch-nested-shebangs 'do-not-capture-python
           (lambda* (#:key outputs #:allow-other-keys)
             ;; patch-shebangs embeds the Python store path into node-gyp's
             ;; Python scripts, pulling Python into node's closure.  Revert
             ;; to /usr/bin/env python3 so downstream gyp builds supply
             ;; their own Python.
             (let ((node-gyp (string-append (assoc-ref outputs "out")
                                            "/lib/node_modules/npm"
                                            "/node_modules/node-gyp")))
               (for-each
                (lambda (f)
                  (substitute* f
                    (("^#!.*/bin/python3") "#!/usr/bin/env python3")))
                (find-files node-gyp "\\.py$")))))
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
             (let ((dir (string-append (assoc-ref outputs "out")
                                       "/lib/node_modules/npm/node_modules"
                                       "/tar/dist")))
               (substitute*
                   (list (string-append dir "/esm/write-entry.js")
                         (string-append dir "/commonjs/write-entry.js"))
                 (("this.stat.nlink > 1") "false")))))
         (add-after 'install 'fix-node-gyp-reference
           ;; Note: programs like node-gyp only receive these values if
           ;; they are started via `npm` or `npx`.
           ;; See: https://github.com/nodejs/node-gyp#npm-configuration
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (spec)
                  (wrap-program (string-append out spec)
                    `("npm_package_config_node_gyp_nodedir" = (,out))))
                '("/bin/npm"
                  "/bin/npx"))))))))
    (native-inputs
     (list ;; Runtime dependencies for binaries used as a bootstrap.
      c-ares-for-node-lts
      brotli
      icu4c-78
      libuv-for-node-lts
      `(,nghttp2-for-node-lts "lib")
      openssl
      zlib
                                        ; ngtcp2? nghttp3?
      ;; Regular build-time dependencies.
      perl
      pkg-config
      procps
      python
      util-linux))
    (native-search-paths
     (list (search-path-specification
             (variable "NODE_PATH")
             (files '("lib/node_modules")))))
    (inputs
     (list bash-minimal
           coreutils
           c-ares-for-node-lts
           icu4c-78
           libuv-for-node-lts
           llhttpish
           brotli
           ngtcp2
           nghttp3
           `(,nghttp2-for-node-lts "lib")
           openssl
           zlib
           uvwasi-for-node-lts
           `(,zstd "lib")))
    (synopsis "Evented I/O for V8 JavaScript")
    (description
     "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (supported-systems (fold delete %supported-systems '("powerpc-linux")))
    (home-page "https://nodejs.org/")
    (license license:expat)
    (properties '((max-silent-time . 7200)   ;2h, needed on ARM
                  (timeout . 21600)          ;6h
                  (cpe-name . "node.js")))))

(define-public node node-lts)

(define-deprecated-package libnode node-lts)

(define-public llhttpish
  (package
    (name "llhttpish")
    (version "9.4.3")
    (source (origin
	      (method url-fetch)
              (uri (string-append "https://codeberg.org/jlicht/llhttpish/releases/download/v" version
                                  "/llhttpish-" version ".tgz"))
	      (sha256
	       (base32
                "19zw4issqjhyfvwi7q39zvjhfmx1cq1kf887zp3sv7g7agyvczr8"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:make-flags
      #~(list (string-append "CLANG=" #$(cc-for-target))
              (string-append "DESTDIR=" #$output)
              "PREFIX=")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure #$set-node-path-phase))))
    (native-inputs
     (list esbuild
           quickjs
           node-llparse-bootstrap
           node-semver-bootstrap))
    (home-page "https://codeberg.org/jlicht/llhttpish")
    (synopsis "Parser for HTTP messages")
    (description "This is a soft fork of a rewrite of
@url{https://github.com/nodejs/http-parser, http-parser} using
@url{https://github.com/nodejs/llparse, llparse} to generate the C source
files.")
    (license license:expat)))
