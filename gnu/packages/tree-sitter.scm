;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Luis Henrique Gomes Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (gnu packages tree-sitter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages icu4c)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public tree-sitter
  (package
    (name "tree-sitter")
    (version "0.20.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nv2a2hr22w8ix71b6rkkxv9rfvhvwlmyql0g6lva9qzj4vy50p4"))
              (modules '((guix build utils)))
              (snippet #~(begin
                           ;; Remove bundled ICU parts
                           (delete-file-recursively "lib/src/unicode")))))
    (build-system gnu-build-system)
    (inputs (list icu4c))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))
           #:tests? #f ; there are no tests for the runtime library
           #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target)))))
    (home-page "https://tree-sitter.github.io/tree-sitter/")
    (synopsis "Incremental parsing system for programming tools")
    (description
     "Tree-sitter is a parser generator tool and an incremental parsing
library.  It can build a concrete syntax tree for a source file and efficiently
update the syntax tree as the source file is edited.

Tree-sitter aims to be:

@itemize
@item General enough to parse any programming language
@item Fast enough to parse on every keystroke in a text editor
@item Robust enough to provide useful results even in the presence of syntax errors
@item Dependency-free so that the runtime library (which is written in pure C)
can be embedded in any application
@end itemize

This package includes the @code{libtree-sitter} runtime library.")
    (license license:expat)))

(define-public tree-sitter-cli
  (package (inherit tree-sitter)
    (name "tree-sitter-cli")
    (source (origin
              (inherit (package-source tree-sitter))
              (snippet
               #~(begin
                   ;; Remove the runtime library code and dynamically link to
                   ;; it instead.
                   (delete-file-recursively "lib/src")
                   (delete-file "lib/binding_rust/build.rs")
                   (with-output-to-file "lib/binding_rust/build.rs"
                     (lambda _
                       (format #t "fn main() {~@
                              println!(\"cargo:rustc-link-lib=tree-sitter\");~@
                              }~%")))))))
    (build-system cargo-build-system)
    (inputs (list tree-sitter))
    (arguments
     (list
      ;; Running test requires downloading fixtures, see the
      ;; script/fetch-fixtures script, which fetches grammars.  Maybe it make
      ;; sence to run tests in the grammar's packages?
      #:tests? #f
      ;; We're only packaging the CLI program so we do not need to install
      ;; sources.
      #:install-source? #f
      #:cargo-inputs
      `(("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-difference" ,rust-difference-2)
        ("rust-dirs" ,rust-dirs-3)
        ("rust-html-escape" ,rust-html-escape-0.2)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-semver" ,rust-semver-1)
        ("rust-smallbitvec" ,rust-smallbitvec-2)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tiny-http" ,rust-tiny-http-0.8)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-webbrowser" ,rust-webbrowser-0.5)
        ("rust-which" ,rust-which-4))
      #:cargo-development-inputs
      `(("rust-pretty-assertions" ,rust-pretty-assertions-0.7))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-cargo-lock
            (lambda _
              (delete-file "Cargo.lock")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (install-file "target/release/tree-sitter" bin)))))))
    (description "Tree-sitter is a parser generator tool and an incremental
parsing library.  It can build a concrete syntax tree for a source file and
efficiently update the syntax tree as the source file is edited.

Tree-sitter aims to be:

@enumerate
@item General enough to parse any programming language.
@item Fast enough to parse on every keystroke in a text editor.
@item Robust enough to provide useful results even in the presence of syntax
errors.
@item Dependency-free so that the runtime library (which is written in pure C)
can be embedded in any application.
@end enumerate

This package includes the @command{tree-sitter} command-line tool.")
    (license license:expat)))

(define* (tree-sitter-grammar
          language language-for-synopsis version commit hash
          #:key
          (repository-url
           (format #f "https://github.com/tree-sitter/tree-sitter-~a" language))
          (source-directory ""))
  (let ((synopsis (string-append language-for-synopsis
                                 " grammar for tree-sitter"))
        (name (string-append "tree-sitter-grammar-" language))
        (src-dir source-directory)
        (lib (format #f "libtree-sitter-~a.so" language)))
    (package
      (name name)
      (version version)
      (home-page repository-url)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url repository-url)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32 hash))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'build
              (lambda _
                (with-directory-excursion (string-append #$src-dir "src")
                  (let* ((scanner? (or (file-exists? "scanner.c")
                                       (file-exists? "scanner.cc")))
                         (CC (if (file-exists? "scanner.cc") "g++" "gcc"))
                         (compile (lambda (f) (invoke CC "-fPIC" "-c" "-I." f)))
                         (link-args `("-fPIC" "-shared" "parser.o"
                                      ,@(if scanner? '("scanner.o") '())
                                      "-o" ,#$lib)))
                    (invoke "gcc" "-fPIC" "-c" "-I." "parser.c")
                    (for-each
                     (lambda (f) (when (file-exists? f) (compile f)))
                     '("scanner.c" "scanner.cc"))
                    (apply invoke CC link-args)))))
            (delete 'check)
            (replace 'install
              (lambda _
                (install-file (string-append #$src-dir "src/" #$lib)
                              (string-append #$output "/lib/tree-sitter")))))))
      (synopsis synopsis)
      (description (string-append synopsis "."))
      (license license:expat))))

(define-public tree-sitter-grammar-html
  (tree-sitter-grammar
   "html" "HTML"
   "0.19.0" "v0.19.0"
   "1hg7vbcy7bir6b8x11v0a4x0glvqnsqc3i2ixiarbxmycbgl3axy"))
