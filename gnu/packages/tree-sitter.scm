;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Luis Henrique Gomes Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2022, 2023 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages node)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public python-tree-sitter
  (package
    (name "python-tree-sitter")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/py-tree-sitter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rc8zqiry4n52xlf7pwx4s56ka9vwjzwgn7blwbkiscqdwvsai92"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-tree-sitter-lib-path
            (lambda _
              (let ((tree-sitter #$(this-package-input "tree-sitter")))
                (substitute* "setup.py"
                  (((string-append
                     "( *)\\[\"tree_sitter\\/core\\/lib\\/src\\/lib\\.c\", "
                     "\"tree_sitter\\/binding\\.c\"\\],") all tabs)
                   (string-append
                    tabs "[\"tree_sitter/binding.c\"],\n"
                    tabs "library_dirs=[\"" tree-sitter "/lib\"],\n"
                    tabs "libraries=[\"tree-sitter\"],"))
                  (("include_dirs=.*")
                   (string-append
                    "include_dirs=[\"" tree-sitter "/include\"],\n"))))))
          (add-before 'check 'set-test-lib-paths
            (lambda _
              (let ((py #$(this-package-native-input "tree-sitter-python"))
                    (js #$(this-package-native-input "tree-sitter-javascript")))
                (substitute* "tests/test_tree_sitter.py"
                  (("Language\\.build_library")
                   "_ =")
                  (("LIB_PATH(, \"python\")" all name)
                   (string-append
                    "\"" py "/lib/tree-sitter/libtree-sitter-python.so\"" name))
                  (("LIB_PATH(, \"javascript\")" all name)
                   (string-append
                    "\"" js "/lib/tree-sitter/libtree-sitter-javascript.so\""
                    name)))))))))
    (inputs (list tree-sitter))
    (native-inputs
     (list tree-sitter-python tree-sitter-javascript))
    (home-page "https://github.com/tree-sitter/py-tree-sitter")
    (synopsis "Python bindings to the Tree-sitter parsing library")
    (description "This package provides Python bindings to the
Tree-sitter parsing library.")
    (license license:expat)))

(define-public tree-sitter
  (package
    (name "tree-sitter")
    (version "0.20.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10w17lfn9asqrk612xivkx26lc620s3nnm30hhlyqd4bj19k7gyv"))
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
library.  It can build a concrete syntax tree for a source file and
efficiently update the syntax tree as the source file is edited.

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
  (package
    (inherit tree-sitter)
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
    (inputs
     (list tree-sitter graphviz node-lts))
    (arguments
     (list
      #:cargo-test-flags
      ''("--release" "--"
         ;; Skip tests which rely on downloading grammar fixtures.  It is
         ;; difficult to support such tests given upstream does not encode
         ;; which version of the grammars are expected.
         ;; Instead, we do run some tests for each grammar in the tree-sitter
         ;; build-system, by running `tree-sitter test'.  This isn't as
         ;; complete as running all tests from tree-sitter-cli, but it's a
         ;; good compromise compared to maintaining two different sets of
         ;; grammars (Guix packages vs test fixtures).
         "--skip=tests::corpus_test"
         "--skip=tests::highlight_test"
         "--skip=tests::node_test"
         "--skip=tests::parser_test"
         "--skip=tests::pathological_test"
         "--skip=tests::query_test"
         "--skip=tests::tags_test"
         "--skip=tests::test_highlight_test"
         "--skip=tests::test_tags_test"
         "--skip=tests::tree_test")
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
        ("rust-tiny-http" ,rust-tiny-http-0.12)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-webbrowser" ,rust-webbrowser-0.8)
        ("rust-which" ,rust-which-4))
      #:cargo-development-inputs
      `(("rust-ctor" ,rust-ctor-0.1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.7)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-unindent" ,rust-unindent-0.2))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-node
            (lambda _
              (substitute* "cli/src/generate/mod.rs"
                (("Command::new\\(\"node\"\\)")
                 (string-append
                  "Command::new(\"" #$node-lts "/bin/node\")")))))
          (add-after 'unpack 'patch-dot
            (lambda _
              (substitute* "cli/src/util.rs"
                (("Command::new\\(\"dot\"\\)")
                 (string-append
                  "Command::new(\"" #$graphviz "/bin/dot\")")))))
          (replace 'install
            (lambda _
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

(define (tree-sitter-delete-generated-files grammar-directories)
  #~(begin
      (use-modules (guix build utils))
      (delete-file "binding.gyp")
      (delete-file-recursively "bindings")
      (for-each
       (lambda (lang)
         (with-directory-excursion lang
           (delete-file "src/grammar.json")
           (delete-file "src/node-types.json")
           (delete-file "src/parser.c")
           (delete-file-recursively "src/tree_sitter")))
       '#$grammar-directories)))

(define* (tree-sitter-grammar
          name text hash version
          #:key
          (commit (string-append "v" version))
          (repository-url
           (format #f "https://github.com/tree-sitter/tree-sitter-~a" name))
          (grammar-directories '("."))
          (article "a")
          (inputs '())
          (get-cleanup-snippet tree-sitter-delete-generated-files)
          (license license:expat))
  "Returns a package for Tree-sitter grammar.  NAME will be used with
tree-sitter- prefix to generate package name and also for generating
REPOSITORY-URL value if it's not specified explicitly, TEXT is a string which
will be used in description and synopsis. GET-CLEANUP-SNIPPET is a function,
it recieves GRAMMAR-DIRECTORIES as an argument and should return a G-exp,
which will be used as a snippet in origin."
  (let* ((multiple? (> (length grammar-directories) 1))
         (grammar-names (string-append text " grammar" (if multiple? "s" "")))
         (synopsis (string-append "Tree-sitter " grammar-names))
         (description
          (string-append "This package provides "
                         (if multiple? "" article) (if multiple? "" " ")
                         grammar-names " for the Tree-sitter library."))
         (name (string-append "tree-sitter-" name)))
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
                (sha256 (base32 hash))
                (snippet
                 (get-cleanup-snippet grammar-directories))))
      (build-system tree-sitter-build-system)
      (arguments (list #:grammar-directories grammar-directories))
      (inputs inputs)
      (synopsis synopsis)
      (description description)
      (license license))))

(define-public tree-sitter-html
  (tree-sitter-grammar
   "html" "HTML"
   "1hg7vbcy7bir6b8x11v0a4x0glvqnsqc3i2ixiarbxmycbgl3axy"
   "0.19.0"))

(define-public tree-sitter-javascript
  (tree-sitter-grammar
   "javascript" "JavaScript(JSX)"
   "175yrk382n2di0c2xn4gpv8y4n83x1lg4hqn04vabf0yqynlkq67"
   "0.20.0"
   #:commit "rust-0.20.0"))

(define-public tree-sitter-typescript
  (tree-sitter-grammar
   "typescript" "TypeScript and TSX"
   "07fl9d968lal0aqj4f0n16p3n94cjkgfp54wynfr8gbdkjss5v5x"
   "0.20.1"
   #:inputs (list tree-sitter-javascript)
   #:grammar-directories '("typescript" "tsx")))

(define-public tree-sitter-bibtex
  (let ((commit "ccfd77db0ed799b6c22c214fe9d2937f47bc8b34")
        (revision "0"))
    (tree-sitter-grammar
     "bibtex" "Bibtex"
     "0m7f3dkqbmy8x1bhl11m8f4p6n76wfvh99rp46zrqv39355nw1y2"
     (git-version "0.1.0" revision commit)
     #:repository-url "https://github.com/latex-lsp/tree-sitter-bibtex"
     #:commit commit
     #:license license:expat)))

(define-public tree-sitter-css
  (tree-sitter-grammar
   "css" "CSS"
   "014jrlgi7zfza9g38hsr4vlbi8964i5p7iglaih6qmzaiml7bja2"
   "0.19.0"))

(define-public tree-sitter-c
  (tree-sitter-grammar
   "c" "C"
   "1w03r4l773ki4iq2xxsc2pqxf3pjsbybq3xq4glmnsihgylibn8v"
   "0.20.2"))

(define-public tree-sitter-cpp
  ;; There are a lot of additions, the last tag was placed more than 1 year ago
  (let ((commit "56cec4c2eb5d6af3d2942e69e35db15ae2433740")
        (revision "0"))
      (tree-sitter-grammar
       "cpp" "C++"
       "0c5iwg9j6naivvr18glfp095x32nfl9hbw0q02rhh1b59fkpjs09"
       (git-version "0.20.0" revision commit)
       #:commit commit
       #:inputs (list tree-sitter-c))))

(define-public tree-sitter-cmake
  (tree-sitter-grammar
   "cmake" "CMake"
   "1z49jdachwxwbzrrapskpi2kxq3ydihfj45ab9892gbamfij2zp5"
   "0.4.1"
   #:repository-url "https://github.com/uyha/tree-sitter-cmake"))

(define-public tree-sitter-elixir
  ;; No tags at all, version in the source code is 0.19.0
  (let ((commit "b20eaa75565243c50be5e35e253d8beb58f45d56")
        (revision "0"))
      (tree-sitter-grammar
       "elixir" "Elixir"
       "1i0c0xki3sv24649p0ws7xs2jagbwg7z7baz1960239bj94nl487"
       (git-version "0.19.0" revision commit)
       #:article "an"
       #:repository-url "https://github.com/elixir-lang/tree-sitter-elixir"
       #:commit commit
       #:license (list license:asl2.0 license:expat))))

(define-public tree-sitter-heex
  (tree-sitter-grammar
   "heex" "Heex"
   "00330rgg67fq0d9gk1yswj78d9mn1jvvjmmy1k7cxpvm5993p3sw"
   "0.6.0"
   #:repository-url "https://github.com/phoenixframework/tree-sitter-heex"))

(define-public tree-sitter-bash
  (tree-sitter-grammar
   "bash" "Bash"
   "18c030bb65r50i6z37iy7jb9z9i8i36y7b08dbc9bchdifqsijs5"
   "0.19.0"))

(define-public tree-sitter-c-sharp
  (tree-sitter-grammar
   "c-sharp" "C#"
   "054fmpf47cwh59gbg00sc0nl237ba4rnxi73miz39yqzcs87055r"
   "0.19.1"))

(define-public tree-sitter-dockerfile
  (tree-sitter-grammar
   "dockerfile" "Dockerfile"
   "0kf4c4xs5naj8lpcmr3pbdvwj526wl9p6zphxxpimbll7qv6qfnd"
   "0.1.2"
   #:repository-url "https://github.com/camdencheek/tree-sitter-dockerfile"))

(define-public tree-sitter-elm
  (tree-sitter-grammar
   "elm" "Elm"
   "0b5jpj8bnil1ylisyc4w48j8a30dyf3zylhidj73mlrb8rf7xm2s"
   "5.6.3"
   #:article "an"
   #:repository-url "https://github.com/elm-tooling/tree-sitter-elm"))

(define-public tree-sitter-gomod
  (tree-sitter-grammar
   "gomod" "Go .mod"
   "1hblbi2bs4hlil703myqhvvq2y1x41rc3w903hg2bhbazh7x8yyf"
   "1.0.0"
   #:repository-url "https://github.com/camdencheek/tree-sitter-go-mod.git"))

(define-public tree-sitter-go
  ;; There are a lot of additions, the last tag was placed more than 1 year ago
  (let ((commit "64457ea6b73ef5422ed1687178d4545c3e91334a")
        (revision "0"))
    (tree-sitter-grammar
     "go" "Go"
     "16d32m78y8jricba9xav35c9y0k2r29irj5xyqgq24323yln9jnz"
     (git-version "0.19.1" revision commit)
     #:commit commit)))

(define-public tree-sitter-haskell
  ;; There are a lot of additions, the last tag was placed more than 4 years ago
  (let ((commit "3bdba07c7a8eec23f87fa59ce9eb2ea4823348b3")
        (revision "0"))
    (tree-sitter-grammar
     "haskell" "Haskell"
     "1hg19af1n510bndf5k5iri7dzb48xb527vispv1aapki4mvr98gx"
     (git-version "0.14.0" revision commit)
     #:commit commit)))

(define-public tree-sitter-java
  (tree-sitter-grammar
   "java" "Java"
   "0440xh8x8rkbdlc1f1ail9wzl4583l29ic43x9lzl8290bm64q5l"
   "0.20.1"))

(define-public tree-sitter-json
  ;; Not tagged
  (let ((commit "5d992d9dd42d533aa25618b3a0588f4375adf9f3"))
    (tree-sitter-grammar
     "json" "JSON"
     "08kxzqyyl900al8mc0bwigxlkzsh2f14qzjyb5ki7506myxlmnql"
     "0.20.0"
     #:commit commit)))

(define-public tree-sitter-julia
  (tree-sitter-grammar
   "julia" "Julia"
   "1pbnmvhy2gq4vg1b0sjzmjm4s2gsgdjh7h01yj8qrrqbcl29c463"
   "0.19.0"))

(define-public tree-sitter-ocaml
  (tree-sitter-grammar
   "ocaml" "OCaml (.ml and .mli)"
   "021vnbpzzb4cca3ncd4qhzy583vynhndn3qhwayxrpgdl61m44i6"
   "0.20.1"
   #:grammar-directories '("ocaml" "interface")))

(define-public tree-sitter-php
  ;; There are a lot of additions, the last tag was placed more than 1 year ago
  (let ((commit "f860e598194f4a71747f91789bf536b393ad4a56")
        (revision "0"))
    (tree-sitter-grammar
     "php" "PHP"
     "02yc5b3qps8ghsmy4b5m5kldyr5pnqz9yw663v13pnz92r84k14g"
     (git-version "0.19.0" revision commit)
     #:commit commit)))

(define-public tree-sitter-python
  ;; There are a lot of additions, the last tag was placed a while ago
  (let ((commit "9e53981ec31b789ee26162ea335de71f02186003")
        (revision "0"))
    (tree-sitter-grammar
     "python" "Python"
     "1lv3pgb7h2a0f121897r0lwc228rjwb77y3a6g3ghifx1rgbwvqg"
     (git-version "0.20.0" revision commit)
     #:commit commit)))

(define-public tree-sitter-r
  ;; No tags
  (let ((commit "80efda55672d1293aa738f956c7ae384ecdc31b4")
        (revision "0"))
    (tree-sitter-grammar
     "r" "R"
     "1n7yxi2wf9xj8snw0b85a5w40vhf7x1pwirnwfk78ilr6hhz4ix9"
     (git-version "0.0.1" revision commit)
     #:commit commit)))

(define-public tree-sitter-ruby
  ;; There are a lot of additions, the last tag was placed more than 1 year ago
  (let ((commit "206c7077164372c596ffa8eaadb9435c28941364")
        (revision "0"))
    (tree-sitter-grammar
     "ruby" "Ruby"
     "1pqr24bj68lgi1w2cblr8asfby681l3032jrppq4n9x5zm23fi6n"
     (git-version "0.19.0" revision commit)
     #:commit commit)))

(define-public tree-sitter-rust
  (tree-sitter-grammar
   "rust" "Rust"
   "149jhy01mqvavwa8jlxb8bnn7sxpfq2x1w35si6zn60b7kqjlx8f"
   "0.20.3"))

(define-public tree-sitter-clojure
  (tree-sitter-grammar
   "clojure" "Clojure"
   "0bgd9g1j4ww45g0l0aa1jac49421z95cc2rhcgqmgx7nzn94rszp"
   "0.0.11"
   #:repository-url "https://github.com/sogaiu/tree-sitter-clojure"))

(define-public tree-sitter-markdown
  ;; No tags
  (let ((commit "ef3caf83663ea97ad9e88d891424fff6a20d878d")
        (revision "0"))
    (tree-sitter-grammar
     "markdown" "Markdown (CommonMark Spec v0.30)"
     "0p9mxpvkhzsxbndda36zx5ycd6g2r2qs60gpx4y56p10lhgzlyqj"
     "0.1.1"
     #:repository-url "https://github.com/MDeiml/tree-sitter-markdown"
     #:grammar-directories '("tree-sitter-markdown"
                             "tree-sitter-markdown-inline")
     #:commit commit)))

(define-public tree-sitter-markdown-gfm
  ;; Not updated for more than 1 year, can be deprecated when gfm will be
  ;; implemented in tree-sitter-markdown
  (tree-sitter-grammar
   "markdown-gfm" "Markdown (CommonMark Spec v0.29-gfm)"
   "1a2899x7i6dgbsrf13qzmh133hgfrlvmjsr3bbpffi1ixw1h7azk"
   "0.7.1"
   #:repository-url "https://github.com/ikatyang/tree-sitter-markdown"))

(define-public tree-sitter-meson
  ;; tag 1.2 is Aug 24,2022  this commit is Feb 28,2023
  (let ((commit "3d6dfbdb2432603bc84ca7dc009bb39ed9a8a7b1")
        (revision "0"))
    (tree-sitter-grammar
     "meson" "Meson"
     "1rn7r76h65d41354czyccm59d1j9nzybcrjvjh934lpr59qrw61m"
     (git-version "1.2" revision commit)
     #:repository-url "https://github.com/Decodetalkers/tree-sitter-meson"
     #:commit commit
     #:license license:expat)))

(define-public tree-sitter-org
  ;; There are a lot of additions, the last tag was placed a while ago
  (let ((commit "081179c52b3e8175af62b9b91dc099d010c38770")
        (revision "0"))
    (tree-sitter-grammar
     "org" "Org"
     "0h9krbaq9j6ijf86sg0w221s0zbpbx5f7m1l0whzjahbrqpnqgxl"
     (git-version "1.3.1" revision commit)
     #:repository-url "https://github.com/milisims/tree-sitter-org"
     #:commit commit)))

(define-public tree-sitter-scheme
  ;; There are a lot of additions, the last tag was placed a while ago
  (let ((commit "67b90a365bebf4406af4e5a546d6336de787e135")
        (revision "0"))
    (tree-sitter-grammar
     "scheme" "Scheme (R5RS, R6RS)"
     "1pvxckza1kdfwqs78ka3lbwldrwkgymb31f5x1fq5vyawg60wxk8"
     (git-version "0.2.0" revision commit)
     #:repository-url "https://github.com/6cdh/tree-sitter-scheme"
     #:commit commit)))

(define-public tree-sitter-racket
  ;; No tags
  (let ((commit "1a5df0206b25a05cb1b35a68d2105fc7493df39b")
        (revision "0"))
    (tree-sitter-grammar
     "racket" "Racket"
     "06gwn3i7swhkvbkgxjlljdjgvx8y1afafbqmpwya70r9z635593h"
     (git-version "0.1.0" revision commit)
     #:repository-url "https://github.com/6cdh/tree-sitter-racket"
     #:commit commit)))

(define-public tree-sitter-plantuml
  ;; No tags
  (let ((commit "bea443ef909484938cb0a9176ebda7b8a3d108f7")
        (revision "0"))
    (tree-sitter-grammar
     "plantuml" "PlantUML"
     "0swqq4blhlvvgrvsb0h4cjl3pnfmmdpfd5r5kg9rpdwk0sn98x3a"
     (git-version "1.0.0" revision commit)
     #:repository-url "https://github.com/Decodetalkers/tree_sitter_plantuml"
     #:commit commit
     #:get-cleanup-snippet
     (lambda _
       #~(begin
           (use-modules (guix build utils))
           (delete-file "binding.gyp")
           (delete-file-recursively "bindings"))))))
