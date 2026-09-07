;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Pradana Aumars <paumars@courrier.dev>
;;; Copyright © 2023 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2025 Luca Di Sera <disera.luca@gmail.com>
;;; Copyright © 2026 Lîm Tsú-thuàn <inbox@dannypsnl.me>
;;; Copyright © 2026 Carlo Zancanaro <carlo@zancanaro.id.au>
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

(define-module (gnu packages lean)
  #:use-module (ice-9 match)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages c)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public lean4
  (package
    (name "lean4")
    (version "4.29.0")
    (home-page "https://lean-lang.org" )
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/leanprover/lean4.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09f347n99iicvq3gnjkxxf0ym2964c9fwalmb1dp0x4bn170xznj"))))
    (build-system cmake-build-system)
    (native-inputs
     (list git ; for the tests
           perl ; for the tests
           pkg-config
           python-wrapper
           tzdata-for-tests))
    (inputs
     (list cadical gmp libuv llvm))
    (arguments
     (list
      #:make-flags
      #~(list "SHELL=bash -euo pipefail")
      #:build-type "Release"            ; default upstream build type
      ;; XXX: Test phases currently fail on 32-bit sytems.
      ;; Tests for those architectures have been temporarily
      ;; disabled, pending further investigation.
      #:tests? (and (not (%current-target-system))
                    (let ((arch (%current-system)))
                      (not (or (string-prefix? "i686" arch)
                               (string-prefix? "armhf" arch)))))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch
            (lambda _
              (substitute* '("stage0/src/CMakeLists.txt"
                             "src/CMakeLists.txt")
               ;; Convert clang option to GCC option.
               (("--print-target-triple") "-dumpmachine")) ; -print-multiarch
              (substitute* '("src/bin/leanc.in"
                             "src/util/ffi.cpp"
                             "stage0/src/bin/leanc.in"
                             "stage0/src/util/ffi.cpp")
               ;; Prevent ld error from:
               ;; "--start-group" "-lInit" "-lleanrt" "--end-group" "-lstdc++"
               ;; "-lLake" ""
               ((" @LEAN_EXTRA_LINKER_FLAGS@")
                "@LEAN_EXTRA_LINKER_FLAGS@"))
              (substitute* "src/lean.mk.in"
               (("SHELL = /usr/bin/env bash")
                "SHELL = bash"))
              (substitute* "src/stdlib.make.in"
               (("/usr/bin/env bash")
                "bash"))
              (setenv "SHELL" "bash -euo pipefail")))
          (add-after 'configure 'pre-populate-mimalloc
            (lambda _
              ;; Lean's CMake tries to git-clone mimalloc at build
              ;; time, which fails in the sandbox. Pre-populate the
              ;; source and create stamp files so ExternalProject
              ;; skips the download.
              (let ((mimalloc-dir "../build/mimalloc/src/mimalloc")
                    (stamp-dir "../build/mimalloc/src/mimalloc-stamp"))
                (mkdir-p stamp-dir)
                (copy-recursively #$(package-source mimalloc) mimalloc-dir)
                ;; Create stamp files so ExternalProject skips all steps.
                (for-each (lambda (step)
                            (call-with-output-file
                              (string-append stamp-dir "/mimalloc-" step)
                              (lambda (port) (display "" port))))
                          '("mkdir" "download" "update" "patch"
                            "configure" "build" "install")))))
          (replace 'check
            (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "../source"
                  (invoke "ctest" "--preset" "release" "--test-dir" "../build/stage1"
                          "-E" (string-join
                                 (list "leancomptest_(doc_example|foreign)"
                                       "tests/lake/examples/(reverse-ffi|ffi)"
                                       "tests/lean/run/(timeIO|async_dns)"
                                       "tests/lean/server/"
                                       "tests/lean/interactive/")
                                 "|")
                          "-j" (if parallel-tests?
                                   (number->string (parallel-job-count))
                                   "1"))))))
          (add-before 'install 'delete-junk
            (lambda _
              ;; Package is reproducible with ".git" deleted.
              (for-each delete-file-recursively
               (find-files "../source/src/lake/tests" "^\\.git$"
                           #:directories? #t)))))))
    (native-search-paths
     ;; Lean packages are a full build tree for the package, which stores its
     ;; build results in this directory.  This fits with how Lake expects to
     ;; find things, so it all works out.  Hopefully upstream improves this
     ;; over time: https://github.com/leanprover/lean4/issues/5122
     (list (search-path-specification
             (variable "LEAN_PATH")
             (files (list ".lake/build/lib/lean")))))
    (synopsis "Theorem prover and programming language")
    (description
     "Lean is a theorem prover and programming language with a small trusted
core based on dependent typed theory, aiming to bridge the gap between
interactive and automated theorem proving.")
    (license license:asl2.0)))

(define (lean-package-name package)
  "Return the name of the Lean package that this Guix PACKAGE represents, as
specified in its lean-package-name property."
  (assoc-ref (package-properties package)
             'lean-package-name))

(define (make-package-overrides.json package)
  "Return a GEXP for a package-overrides.json file which tells Lake where to
find the dependencies of PACKAGE."
  (let* ((packages (map cadr
                        (package-propagated-inputs package)))
         (relevant-packages (filter lean-package-name packages)))
    (mixed-text-file "package-overrides.json"
                     "{\"version\":\"1.2.0\",\"packages\": ["
                     #~(string-join (list #$@(map (lambda (package
                                                            )
                                                    #~(string-append
                                                       "{\"name\":\""
                                                       #$(lean-package-name
                                                          package)
                                                       "\",\"type\":\"path\",\"dir\":\""
                                                       #$package
                                                       "\",\"inherited\":false}"))
                                                  relevant-packages)) ",")
                     "]}")))

(define-syntax-rule (package-overrides.json)
  "Returns a GEXP for a package-overrides.json file which tells Lake where to
find the dependencies of the current package."
  (make-package-overrides.json this-package))

(define-public lean4-batteries
  (package
    (name "lean4-batteries")
    (version (package-version lean4))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leanprover-community/batteries")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "013bh6anafhwgq8mav7yswqy7ym86gwvf5m6h39s45dnd25h6hmh"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (setenv "CC" "gcc")
              (invoke "lake" "build" "--packages"
                      #$(package-overrides.json) "Batteries:static")))
          (delete 'check)
          (replace 'install
            (lambda _
              (copy-recursively "."
                                #$output))))))
    (native-inputs (list lean4))
    (home-page "https://github.com/leanprover-community/batteries")
    (synopsis
     "Collection of maths/computer science data structures and tactics")
    (description
     "Batteries is the the \"batteries included\" extended library for Lean 4.
This is a collection of data structures and tactics intended for use by both
computer-science applications and mathematics applications of Lean 4.")
    (license license:asl2.0)
    (properties '((lean-package-name . "batteries")))))

(define-public lean4-cli
  (package
    (name "lean4-cli")
    (version (package-version lean4))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leanprover/lean4-cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fasl91y1dlfpgd8whfs6cbv1a8z0m2p07p47rc0p6ymqpi2a9cc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (setenv "CC" "gcc")
              (invoke "lake" "build" "Cli:static")))
          (delete 'check)
          (replace 'install
            (lambda _
              (copy-recursively "."
                                #$output))))))
    (native-inputs (list lean4))
    (home-page "https://github.com/leanprover/lean4-cli")
    (synopsis "Command line parsing for Lean 4")
    (description "Cli is a Lean 4 library for configuring Command Line
Interfaces and parsing command line arguments.  Commands are configured with a
lightweight DSL.")
    (license license:expat)
    (properties '((lean-package-name . "Cli")))))

(define-public lean4-import-graph
  (package
    (name "lean4-import-graph")
    (version (package-version lean4))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leanprover-community/import-graph")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f39gycfiwwfkk39fp1x9169f8xn7vpblcmddk29r24nmkd4x9xn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (setenv "CC" "gcc")
              (invoke "lake" "build" "--packages"
                      #$(package-overrides.json) "ImportGraph:static")))
          (delete 'check)
          (replace 'install
            (lambda _
              (copy-recursively "."
                                #$output))))))
    (native-inputs (list lean4))
    (propagated-inputs (list lean4-cli))
    (home-page "https://github.com/leanprover-community/import-graph")
    (synopsis "Create import graphs of Lake packages")
    (description
     "This package provides a tool to create import graphs of Lake packages
(Lean projects) in the Graphviz/dot format or as HTML.")
    (license license:asl2.0)
    (properties '((lean-package-name . "importGraph")))))

(define-public lean4-quote4
  (package
    (name "lean4-quote4")
    (version (package-version lean4))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leanprover-community/quote4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03cx5xpm3sr6b8hmigq14apn4ahw13n12f7m17gbl9b7zn33kmm4"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (setenv "CC" "gcc")
              (invoke "lake" "build" "--packages"
                      #$(package-overrides.json) "Qq:static")))
          (delete 'check)
          (replace 'install
            (lambda _
              (copy-recursively "."
                                #$output))))))
    (native-inputs (list lean4))
    (home-page "https://github.com/leanprover-community/quote4")
    (synopsis "Type-safe expression quotations for Lean 4")
    (description
     "This package implements type-safe expression quotations,
which are a way of constructing object-level expressions (Expr) in meta-level
code.")
    (license license:asl2.0)
    (properties '((lean-package-name . "Qq")))))

(define-public lean4-search-client
  (package
    (name "lean4-search-client")
    (version "4.32.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leanprover-community/LeanSearchClient")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wwlh8sz9bhbrydr5ifrjiph9yf9agn3s0m9ljl68jsg6vv5a52k"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (setenv "CC" "gcc")
              (invoke "lake" "build" "--packages"
                      #$(package-overrides.json) "LeanSearchClient:static")))
          (delete 'check)
          (replace 'install
            (lambda _
              (copy-recursively "."
                                #$output))))))
    (native-inputs (list lean4))
    (home-page "https://github.com/leanprover-community/LeanSearchClient")
    (synopsis "Natural language search within Lean")
    (description
     "LeanSearchClient provides syntax for search using the
leansearch API and the LeanStateSearch API from within Lean.  It allows you to
search for Lean tactics and theorems using natural language.  It also allows
searches on Loogle from within Lean.")
    (license license:asl2.0)
    (properties '((lean-package-name . "LeanSearchClient")))))

(define-public python-mathlibtools
  (package
    (name "python-mathlibtools")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/leanprover-community/mathlib-tools")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vjz3f2lvkql9dz9x1d9mq8vczqs70v2ar88g7p4wm6qq7pgx7l3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k"
              ;; These tests require network access.
              (string-join (list "not test_new"
                                 "test_add"
                                 "test_upgrade_project"
                                 "test_upgrade_mathlib"
                                 "test_get_tutorials")
                           " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-home-env
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs (list python-pytest python-setuptools))
    (inputs (list python-toml
                  python-pygithub
                  python-certifi
                  python-gitpython
                  python-requests
                  python-click
                  python-tqdm
                  python-networkx
                  python-pydot
                  python-pyyaml
                  python-atomicwrites))
    (home-page "https://github.com/leanprover-community/mathlib-tools")
    (synopsis "Development tools for Lean mathlib")
    (description
     "This package contains @command{leanproject}, a supporting tool for Lean
mathlib, a mathematical library for the Lean theorem prover.")
    (license license:asl2.0)))
