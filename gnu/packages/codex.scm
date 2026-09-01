;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Danny Milosavljevic <dannym@friendly-machines.com>
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

;;; This module is separate from (gnu packages rust-apps) to avoid a
;;; circular module dependency: (gnu packages rust-sources), which
;;; defines rust-codex packages used by codex-acp, transitively loads
;;; (gnu packages rust-apps) through its #:use-module chain.  If the
;;; codex-acp package lived in rust-apps.scm, loading rust-sources would
;;; trigger loading rust-apps before those rust-codex packages are
;;; defined, causing an unbound variable error.

(define-module (gnu packages codex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-sources)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization))

(define-public codex-acp
  (package
    (name "codex-acp")
    (version "0.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zed-industries/codex-acp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rlgkbvrbdl5jhf49sjn2mifn2rq6z0vwf0gard23y1bz6sr3wmr"))
       (patches (search-patches "codex-acp-0.11.1-remove-patch-sections.patch"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-codex-deps
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Rewrite git dependencies to use vendored sources from rust-codex
              (substitute* "Cargo.toml"
                (("git = \"https://github.com/openai/codex\", tag = \"rust-v0.117.0\"")
                 "version = \"0.117.0\""))
              ;; Disable V8 runtime in codex-code-mode (transitive dep).
              (let ((port (open-file "Cargo.toml" "a")))
                (display "
[dependencies.codex-code-mode]
version = \"0.117.0\"
default-features = false
" port)
                (close-port port))
              ;; Point codex-linux-sandbox at system bubblewrap source.
              (setenv "CODEX_BWRAP_SOURCE_DIR"
                      (string-append
                       (assoc-ref inputs "bubblewrap-source")
                       "/"))))
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "USER" "nixbld")))
          (add-before 'build 'create-node-version-file
            (lambda _
              ;; codex-core's js_repl uses include_str! to read
              ;; node-version.txt four directories up from its source;
              ;; create it where the vendored crate expects it.
              (call-with-output-file "guix-vendor/node-version.txt"
                (lambda (port)
                  (display "22.22.0" port))))))))
    (native-inputs
     `(("cmake-minimal" ,cmake-minimal)
       ("clang" ,clang)
       ("pkg-config" ,pkg-config)
       ("bubblewrap-source" ,(package-source bubblewrap))))
    (inputs (cons* libcap openssl sqlite zlib `(,zstd "lib")
                   (cargo-inputs 'codex-acp)))
    (home-page "https://github.com/zed-industries/codex-acp")
    (synopsis "ACP-compatible agent bridging Zed Codex with ACP clients")
    (description
     "This package provides an Agent Client Protocol (ACP) compatible agent
that bridges the Zed Codex runtime with ACP clients over stdio.  It
supports multiple LLM providers through configuration in
@file{~/.codex/config.toml} and integrates with MCP servers for filesystem
operations.")
    (license license:asl2.0)))
