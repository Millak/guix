;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Luis Henrique Gomes Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2022, 2023 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022, 2024 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023, 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023-2025 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.com>
;;; Copyright © 2025 Nguyễn Gia Phong <mcsinyx@disroot.org>
;;; Copyright © 2025 Evgenii Klimov <eugene.dev@lipklim.org>
;;; Copyright © 2025 Felipe Silva <git@felipeqq2.rocks>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages node)
  #:use-module (gnu packages python-build)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public tree-sitter
  (package
    (name "tree-sitter")
    (version "0.25.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cck2wa17figxww7lb508sgwy9sbyqj89vxci07hiscr5sgdx9y5"))
              (modules '((guix build utils)))
              (snippet #~(begin
                           ;; Remove bundled ICU parts
                           (delete-file-recursively "lib/src/unicode")))))
    (build-system gnu-build-system)
    (inputs (list icu4c))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               ;; The library uses -fvisibility=hidden to compile, but this
               ;; symbol is needed by the Rust bindings.
               (add-after 'unpack 'patch-_ts_dup-visibility
                 (lambda _
                   (substitute* "lib/src/tree.c"
                     (("int _ts_dup")
                      (string-append
                       "int __attribute__ ((visibility (\"default\"))) "
                       "_ts_dup"))))))
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
    (source
     (origin
       (inherit (package-source tree-sitter))
       (snippet
        #~(begin
            ;; Remove the runtime library code and dynamically link to it
            ;; instead.
            (for-each delete-file-recursively
                      (find-files
                       "lib/src"
                       (lambda (file stat)
                         ;; These files are required for building the binding.
                         (not
                          (or (string-contains file "parser.h")
                              (string-contains file "stdlib-symbols.txt"))))))
            (delete-file "lib/binding_rust/build.rs")
            (with-output-to-file "lib/binding_rust/build.rs"
              (lambda _
                (format #t "~
use std::{env, fs, path::{Path, PathBuf}};
fn main() {
    let out_dir = PathBuf::from(env::var(\"OUT_DIR\").unwrap());
    fs::copy(\"src/wasm/stdlib-symbols.txt\",
    out_dir.join(\"stdlib-symbols.txt\")).unwrap();
    println!(\"cargo:rustc-link-lib=tree-sitter\");
}~%")))))))
    (build-system cargo-build-system)
    (inputs
     (cons* tree-sitter graphviz node-lts (cargo-inputs 'tree-sitter-cli)))
    (arguments
     (list
      #:cargo-test-flags
      ''("--"
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
         "--skip=tests::tree_test"
         "--skip=tests::async_context_test"
         "--skip=tests::text_provider_test"
         "--skip=tests::detect_language"
         "--skip=tests::language_test"
         "--skip=tests::parser_hang_test")
      ;; We're only packaging the CLI program so we do not need to install
      ;; sources.
      #:install-source? #f
      #:cargo-install-paths ''("cli")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-references
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "cli/generate/src/lib.rs"
                (("(js_runtime\\.unwrap_or\\(\")node(\"\\))" _ prefix suffix)
                 (string-append
                  prefix
                  (search-input-file inputs "bin/node")
                  suffix)))
              (substitute* "cli/src/util.rs"
                (("Command::new\\(\"dot\"\\)")
                 (string-append
                  "Command::new(\""
                  (search-input-file inputs "bin/dot")
                  "\")"))))))))
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
it receives GRAMMAR-DIRECTORIES as an argument and should return a G-exp,
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
      (inputs (if (promise? inputs)
                  (force inputs)
                  inputs))
      (synopsis synopsis)
      (description description)
      (license license))))

;;; Language grammars (sort alphabetically)

(define-public tree-sitter-ada
  (let ((version "0.1.0") ; In package.json, but untagged.
        (commit "b23672d313b4c994ab96fd54f1b7ff15eac68a55")
        (revision "0"))
    (tree-sitter-grammar
     "ada" "Ada"
     "0ly6zd2hbng5gbs14yq6riqw2mbzvh0shc7lp2smzbv9ran6chjy"
     (git-version version revision commit)
     #:repository-url "https://github.com/briot/tree-sitter-ada"
     #:commit commit)))

(define-public tree-sitter-awk
  (tree-sitter-grammar
   "awk" "AWK"
   "1bglimp46cggq9kx92h943wnq2r6ymp7anwynjnl42wrdssc0drh"
   "0.7.2"
   #:repository-url "https://github.com/Beaglefoot/tree-sitter-awk"))

(define-public tree-sitter-bash
  (tree-sitter-grammar
   "bash" "Bash"
   "1smlcfkxxknhya1b1h72zj3ccg35szbg9mii2xwh7iq9acnlzpgc"
   "0.23.3"))

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

(define-public tree-sitter-blueprint
  (let ((commit "329699d55f3e3955091e13756563c3f320a561fc")
        (revision "0"))
    (tree-sitter-grammar
     "blueprint" "Blueprint"
     "0pdjka1sdw9dwilw3vjl3k3cn7pcyv7m57cmxqibashg316dygxm"
     (git-version "0.0.0" revision commit) ; Upstream has not tagged any releases
     #:repository-url "https://github.com/huanie/tree-sitter-blueprint"
     #:commit commit
     #:license license:expat)))

(define-public tree-sitter-c
  (tree-sitter-grammar
   "c" "C"
   "1vw7jd3wrb4vnigfllfmqxa8fwcpvgp1invswizz0grxv249piza"
   "0.23.5"))

(define-public tree-sitter-clojure
  (tree-sitter-grammar
   "clojure" "Clojure"
   "1j41ba48sid6blnfzn6s9vsl829qxd86lr6yyrnl95m42x8q5cx4"
   "0.0.13"
   #:repository-url "https://github.com/sogaiu/tree-sitter-clojure"
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         (use-modules (guix build utils))
         (for-each
          (lambda (lang)
            (with-directory-excursion lang
              (delete-file "src/grammar.json")
              (delete-file "src/node-types.json")
              (delete-file "src/parser.c")
              (delete-file-recursively "src/tree_sitter")))
          '#$grammar-directories)))))

(define-public tree-sitter-cmake
  (tree-sitter-grammar
   "cmake" "CMake"
   "0b30ci696iabk3hm03rm4c3qi72d7xsbipc6g0ixxxj2qihdl2fy"
   "0.7.0"
   #:repository-url "https://github.com/uyha/tree-sitter-cmake"))

(define-public tree-sitter-cpp
  (tree-sitter-grammar
   "cpp" "C++"
   "0sbvvfa718qrjmfr53p8x3q2c19i4vhw0n20106c8mrvpsxm7zml"
   "0.23.4"
   #:inputs (list tree-sitter-c)))

(define-public tree-sitter-c-sharp
  (tree-sitter-grammar
   "c-sharp" "C#"
   "0w6xdb8m38brhin0bmqsdqggdl95xqs3lbwq7azm5gg94agz9qf1"
   "0.23.1"))

(define-public tree-sitter-css
  (tree-sitter-grammar
   "css" "CSS"
   "0c5j9zyjcykmraix1agbc0gdk85zs2v379q0aykr10fi9w2r9z9c"
   "0.23.2"))

(define-public tree-sitter-devicetree
  (tree-sitter-grammar
   "devicetree" "Devicetree"
   "13rm15p9mrdklys0d720xy386pnvirxxjswg0wi1m87hs8i49qns"
   "0.11.1"
   #:repository-url "https://github.com/joelspadin/tree-sitter-devicetree"
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         (for-each delete-file '("parser.exp" "parser.lib"))
         #$(tree-sitter-delete-generated-files grammar-directories)))
   #:license license:expat))

(define-public tree-sitter-dockerfile
  (tree-sitter-grammar
   "dockerfile" "Dockerfile"
   "09iw9mqlpgsi6ak4mxrv16anvmbyap6vf61r2pi2lqdp9h1mp7g0"
   "0.2.0"
   #:repository-url "https://github.com/camdencheek/tree-sitter-dockerfile"))

(define-public tree-sitter-dot
  ;; 0.1.6 is taken from Cargo.toml. The package.json has 0.1.5.
  (let ((version "0.1.6")
        (commit "9ab85550c896d8b294d9b9ca1e30698736f08cea")
        (revision "0"))
    (tree-sitter-grammar
     "dot" "Dot"
     "013brrljrhgpnks1r0cdvj93l303kb68prm18gpl96pvhjfci063"
     (git-version version revision commit)
     #:repository-url "https://github.com/rydesun/tree-sitter-dot"
     #:commit commit)))

(define-public tree-sitter-elixir
  (tree-sitter-grammar
   "elixir" "Elixir"
   "12i0z8afdzcznn5dzrssr7b7jx4h7wss4xvbh3nz12j6makc7kzl"
   "0.3.4"
   #:article "an"
   #:repository-url "https://github.com/elixir-lang/tree-sitter-elixir"
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         ;; FIXME: Failure - row: 18, column: 4, expected highlight
         ;; 'comment.doc', actual highlights: none.
         (delete-file "test/highlight/module.ex")
         #$(tree-sitter-delete-generated-files grammar-directories)))
   #:license (list license:asl2.0 license:expat)))

(define-public tree-sitter-elm
  (let ((commit "3b373a3ccb48f31aa4b7ddc9092e2f5e24ab7aed")
        (revision "0"))
    (tree-sitter-grammar
     "elm" "Elm"
     "1cfkqi1vgxg7bkdda1vy9wqz6ap61fwjcdv9qzkcfzxaqr5z8dbw"
     (git-version "5.7.0" revision commit)
     #:article "an"
     #:repository-url "https://github.com/elm-tooling/tree-sitter-elm"
     #:commit commit
     #:get-cleanup-snippet
     (lambda (grammar-directories)
       #~(begin
           (for-each
            delete-file
            '("docs/js/tree-sitter-elm.wasm" "docs/js/tree-sitter.wasm"))
           #$(tree-sitter-delete-generated-files grammar-directories))))))

(define-public tree-sitter-embedded-template
  (tree-sitter-grammar
   "embedded-template" "embedded template (like ERB and EJS)"
   "1vq9dywd9vcy59f6i5mk5n7vwk67g8j5x77czg7avpznskgfhqhb"
   "0.23.2"
   #:article "an"))

(define-public tree-sitter-erlang
  (let ((commit "364e323b32d098ad0e7b29e7adb4005c2bb5cf34")
        (revision "0"))
  (tree-sitter-grammar
   "erlang" "Erlang"
   "17dkvygqrx4v3y568jg72q6rdli5bp55l3zfqxvq6b5ibw48kilq"
   (git-version "0.14.0" revision commit)
   #:repository-url "https://github.com/WhatsApp/tree-sitter-erlang"
   #:commit commit
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         (use-modules (guix build utils))
         ;; FIXME: Language not found.
         (delete-file-recursively "test/highlight")
         #$(tree-sitter-delete-generated-files grammar-directories))))))

(define-public tree-sitter-go
  (tree-sitter-grammar
   "go" "Go"
   "0yi8if9mqzzcs4qflflz90hhaxkzlq54wia3s0iiqzfqxk24a61g"
   "0.23.4"))

(define-public tree-sitter-gomod
  (tree-sitter-grammar
   "gomod" "Go .mod"
   "1vbg4fn54a7lbwcrvjdx3nrwgw5y925chbbb7sd6kwms1434yyhb"
   "1.1.0"
   #:repository-url "https://github.com/camdencheek/tree-sitter-go-mod"))

(define-public tree-sitter-gpr
  (let ((version "0.1.0") ; In package.json, but untagged
        (commit "cea857d3c18d1385d1f5b66cd09ea1e44173945c")
        (revision "0"))
    (tree-sitter-grammar
     "gpr" "GNAT Project"
     "0mf6ghqdyn0qbani9hg67yr6a68cbbbvhn734ndygxm30vqdz9xn"
     (git-version version revision commit)
     #:repository-url "https://github.com/brownts/tree-sitter-gpr"
     #:commit commit
     ;; binding.gyp & bindings is not part of this grammar.
     #:get-cleanup-snippet
     (lambda (grammar-directories)
       #~(begin
           (use-modules (guix build utils))
           (for-each
            (lambda (lang)
              (with-directory-excursion lang
                (delete-file "src/grammar.json")
                (delete-file "src/node-types.json")
                (delete-file "src/parser.c")
                (delete-file-recursively "src/tree_sitter")))
            '#$grammar-directories))))))

(define-public tree-sitter-haskell
  (tree-sitter-grammar
   "haskell" "Haskell"
   "08qzkvyc830k56j5lglfzmlp03ygixf9vlrpazbndqvqk20n56xa"
   "0.15.0"
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         (delete-file "test/highlight/Basic.hs") ;FIXME: Language not found.
         #$(tree-sitter-delete-generated-files grammar-directories)))))

(define-public tree-sitter-hcl
  (let ((commit "de10d494dbd6b71cdf07a678fecbf404dbfe4398")
        (revision "0"))
    (tree-sitter-grammar
     "hcl" "HCL"
     "0dpxrr7m1k4kjasgz6v8kj7ldp0k6jcxlzcrr09qv7209v24s4x1"
     (git-version "1.1.0" revision commit)
     #:article "an"
     #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-hcl"
     #:commit commit
     #:license license:asl2.0)))

(define-public tree-sitter-heex
  (tree-sitter-grammar
   "heex" "Heex"
   "0d0ljmxrvmr8k1wc0hd3qrjzwb31f1jaw6f1glamw1r948dxh9xf"
   "0.8.0"
   #:repository-url "https://github.com/phoenixframework/tree-sitter-heex"))

(define-public tree-sitter-html
  (tree-sitter-grammar
   "html" "HTML"
   "0slhrmwcw2xax4ylyaykx4libkzlaz2lis8x8jmn6b3hbdxlrpix"
   "0.23.2"))

(define-public tree-sitter-janet
  (tree-sitter-grammar
   "janet" "Janet"
   "1sg862gqxn5y86sqa4habyicsr1ax70i7w8ibnn2yyx1bzn99jqn"
   "0.1.0"
   #:repository-url "https://github.com/GrayJack/tree-sitter-janet"
   #:license license:bsd-3))

(define-public tree-sitter-java
  (tree-sitter-grammar
   "java" "Java"
   "11j4ifhl5hsmb2sa651cp5xds9cjgjynl86yivvk6bnr2ba0xw9s"
   "0.23.5"))

(define-public tree-sitter-javascript
  (tree-sitter-grammar
   "javascript" "JavaScript(JSX)"
   "03v1gpr5lnifrk4lns690fviid8p02wn7hfdwp3ynp7lh1cid63a"
   "0.23.1"))

(define-public tree-sitter-json
  (tree-sitter-grammar
   "json" "JSON"
   "0p0fiqi5imxm13s1fs6bhqw6v11n79ri1af3d072zm7jqkcl5mhc"
   "0.24.8"))

(define-public tree-sitter-julia
  (tree-sitter-grammar
   "julia" "Julia"
   "0xi04a48ly438gar25bkkvvr8by4dd013cnafbjdysqjfs04q2wg"
   "0.23.1"))

(define-public tree-sitter-kdl
  (tree-sitter-grammar
   "kdl" "KDL"
   "1015x24ffrvzb0m0wbqdzmaqavpnjw0gvcagxi9b6vj3n1ynm0ps"
   "1.1.0"
   #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-kdl"))

(define-public tree-sitter-kotlin
  (tree-sitter-grammar
   "kotlin" "Kotlin"
   "0bv21rcypi9dx87kgfr89mg8qfc7ik1g1fcb8am7ss17by8badwk"
   "0.3.8"
   #:repository-url "https://github.com/fwcd/tree-sitter-kotlin"
   #:commit "0.3.8"))

(define-public tree-sitter-latex
  (tree-sitter-grammar
   "latex" "LaTeX"
   "18dyda7299imb6i2jnjpr7z2jdrjn804c3958nkkpxzzfhbq39h7"
   "0.4.0"
   #:repository-url "https://github.com/latex-lsp/tree-sitter-latex"
   #:get-cleanup-snippet
   (lambda _
     #~(begin
         (use-modules (guix build utils))
         (delete-file "binding.gyp")
         (delete-file-recursively "bindings")))))

(define-public tree-sitter-lua
  (tree-sitter-grammar
   "lua" "Lua"
   "041anx0qirvd4il87whpic8nfdc1nk3kimxdb99m25bfdzm9rn0r"
   "0.3.0"
   #:repository-url "https://github.com/MunifTanjim/tree-sitter-lua"
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         (use-modules (guix build utils))
         (delete-file-recursively "test/highlight") ;FIXME
         #$(tree-sitter-delete-generated-files grammar-directories)))))

(define-public tree-sitter-markdown
  (tree-sitter-grammar
   "markdown" "Markdown (CommonMark Spec v0.30)"
   "0r8jlmyr1bnyqqipmpmxxw4qw81c9n0l29xdfkz2n2zmjqps5v9r"
   "0.4.1"
   #:repository-url "https://github.com/MDeiml/tree-sitter-markdown"
   #:grammar-directories '("tree-sitter-markdown"
                           "tree-sitter-markdown-inline")
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         (use-modules (guix build utils))
         ;; FIXME
         (with-directory-excursion "tree-sitter-markdown-inline/test/corpus"
           (for-each delete-file
                     '("extension_wikilink.txt" "spec.txt" "tags.txt")))
         #$(tree-sitter-delete-generated-files grammar-directories)))))

(define-public tree-sitter-markdown-gfm
  (deprecated-package "tree-sitter-markdown-gfm" tree-sitter-markdown))

(define-public tree-sitter-matlab
  (tree-sitter-grammar
   "matlab" "Matlab"
   "1kcg16aa9swk4a73klp72rib15ljz7zjlr97rr134laym4x0fnvl"
   "1.0.4"
   #:repository-url "https://github.com/acristoffers/tree-sitter-matlab"
   #:license license:expat))

(define-public tree-sitter-mermaid
  (let ((version "0.0.2") ; In package.json, but untagged
        (commit "90ae195b31933ceb9d079abfa8a3ad0a36fee4cc")
        (revision "0"))
  (tree-sitter-grammar
   "mermaid" "Mermaid"
   "1wfdsz3ikdbpym14aljgy80jhr06cgwhhg72rr8d3rsbm8z5ppaf"
   (git-version version revision commit)
   #:commit commit
   #:repository-url "https://github.com/monaqa/tree-sitter-mermaid")))

(define-public tree-sitter-meson
  (tree-sitter-grammar
   "meson" "Meson"
   "10v1d22wgkc9m33b7zfl712q7zrpn17l1xyprj4wwasqjvg29l03"
   "1.3.0"
   #:repository-url "https://github.com/Decodetalkers/tree-sitter-meson"
   #:license license:expat))

(define-public tree-sitter-nix
  (tree-sitter-grammar
   "nix" "Nix"
   "0rkvr64fvawblpg39rr0mrigf1rhsk30vng2vxviwb7fk50iww0i"
   "0.0.2"
   #:repository-url "https://github.com/nix-community/tree-sitter-nix"
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         (use-modules (guix build utils))
         ;; FIXME: Language not found.
         (delete-file-recursively "test/highlight")
         #$(tree-sitter-delete-generated-files grammar-directories)))))

(define-public tree-sitter-ocaml
  (tree-sitter-grammar
   "ocaml" "OCaml (.ml and .mli)"
   "17g2ynqhjf1nyhdidz9j9s0s12iys3b2vbywxkygwyccj7rb8zdi"
   "0.24.0"
   #:grammar-directories '("grammars/interface" "grammars/ocaml" "grammars/type")))

(define-public tree-sitter-org
  ;; There are a lot of additions, the last tag was placed a while ago
  (let ((commit "64cfbc213f5a83da17632c95382a5a0a2f3357c1")
        (revision "1"))
    (tree-sitter-grammar
     "org" "Org"
     "1l62p4a3b22pa7b5mzmy497pk5b8w01hx6zinfwpbnzg2rjdwkgz"
     (git-version "1.3.1" revision commit)
     #:repository-url "https://github.com/milisims/tree-sitter-org"
     #:commit commit)))

(define-public tree-sitter-plantuml
  ;; No tags
  (let ((commit "c7361a1d481dc1ff6700b14ea1d5efc549b72713")
        (revision "1"))
    (tree-sitter-grammar
     "plantuml" "PlantUML"
     "0apmv0dad58ixhxhzxkwlm9wgbphj7lxilbh427rpxy1y5hrml0f"
     (git-version "1.0.0" revision commit)
     #:repository-url "https://github.com/Decodetalkers/tree_sitter_plantuml"
     #:commit commit
     #:get-cleanup-snippet
     (lambda _
       #~(begin
           (use-modules (guix build utils))
           (substitute* "grammar.js"
             (("u\\{\\[0-9a-fA-F\\]\\+\\}")
              "u\\{[0-9a-fA-F]+\\}"))
           (delete-file "binding.gyp")
           (delete-file-recursively "bindings"))))))

(define-public tree-sitter-php
  (tree-sitter-grammar
   "php" "PHP"
   "05qhz14vvqgwpxgdfr1skwgrv041zwc3wxjyx6y679965nn0lrji"
   "0.23.12"
   #:grammar-directories '("php" "php_only")))

(define-public tree-sitter-prisma
  (tree-sitter-grammar
   "prisma" "Prisma"
   "144l2w7ky5imd7yvp1n6lgbyy5kq6kx4c2ja43yk15k3258pf52v"
   "1.5.1"
   #:repository-url "https://github.com/victorhqc/tree-sitter-prisma"
   #:license license:expat))

(define-public tree-sitter-python
  (tree-sitter-grammar
   "python" "Python"
   "0a108sfqcsxrp54lapk7k3kq6fmz8745z5q99wpn3i1cqpi9slzg"
   "0.23.6"))

(define-public tree-sitter-qml
  (let ((version "0.2.0") ; Tagged, but we use a more recent commit
        (commit "0889da4632bba3ec6f39ef4102625654890c15c1")
        (revision "0"))
    (tree-sitter-grammar
     "qml" "Qml"
     "1qdfwbqjnw7vcvq2dcx19c159b12kv2hvwrvbp7m9i5vl6szc22g"
     (git-version version revision commit)
     #:repository-url "https://github.com/yuja/tree-sitter-qmljs"
     #:commit commit
     #:inputs (delay (list tree-sitter-javascript tree-sitter-typescript))
     #:license license:expat)))

(define-public tree-sitter-query
  (package
    (inherit (tree-sitter-grammar "query"
     "Query"
     "0fbqwg7km4yqjq8p2fkj9hpy0sfnijnf1hsk34wsirlp3af3hc67"
     "0.7.0"
     #:repository-url
     "https://github.com/tree-sitter-grammars/tree-sitter-query"))
    (synopsis "Tree-sitter grammar for Tree-sitter's query language")
    (description "This package provides Tree-sitter's query grammar.")))

(define-public tree-sitter-r
  (tree-sitter-grammar
   "r" "R"
   "1jf14nvrfcznsnmxmlkbnn59bdykpsawxm5grph65p8vghi4fik7"
   "1.1.0"
   #:repository-url "https://github.com/r-lib/tree-sitter-r"
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         (use-modules (guix build utils))
         ;; FIXME: Language not found.
         (for-each delete-file-recursively '("test/highlight" "test/tags"))
         #$(tree-sitter-delete-generated-files grammar-directories)))))

(define-public tree-sitter-racket
  (tree-sitter-grammar
   "racket" "Racket"
   "1x9x6pn7l578xlm1cmg5w0cy2zk34qywsn0jzf1j8phwf7k66bxg"
   "0.23.0-1"
   #:repository-url "https://github.com/6cdh/tree-sitter-racket"))

(define-public tree-sitter-ron
  (let ((commit "78938553b93075e638035f624973083451b29055")
        (revision "0"))
    (tree-sitter-grammar
     "ron" "RON"
     "13f8a5sfvyrizkl0szh2yaslmvjk91s97yb2abrdh74a0pl217aa"
     (git-version "0.2.0" revision commit)
     #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-ron"
     #:commit commit
     #:license (list license:asl2.0 license:expat))))

(define-public tree-sitter-ruby
  (tree-sitter-grammar
   "ruby" "Ruby"
   "0c1vs63ydcb3q2wnif18l5irbj6chkcvd3p6dg0vyhklk5acrvca"
   "0.23.1"))

(define-public tree-sitter-rust
  (tree-sitter-grammar
   "rust" "Rust"
   "0x7xqxspdfnbgn9nvrsl2rsnrjbs01i5hy8k8p5wwk2j358hjyyb"
   "0.24.0"))

(define-public tree-sitter-scala
  (tree-sitter-grammar
   "scala" "Scala"
   "02awiraj5mmgyi31yzxyxarkkl41qksm3lm41pq9c6bmyqpir2y6"
   "0.23.4"))

(define-public tree-sitter-scheme
  (tree-sitter-grammar
   "scheme" "Scheme (R5RS, R6RS, R7RS)"
   "12p8g2mnd73lanibk16llhbx7xarlcl2ihngcibhpa4bzppcbb8l"
   "0.23.0-1"
   #:repository-url "https://github.com/6cdh/tree-sitter-scheme"))

(define-public tree-sitter-starlark
  (tree-sitter-grammar
   "starlark" "Starlark"
   "1qlvk67fqd49138nxl81l4gx833271mns36g3lm884sdmw3225w8"
   "1.3.0"
   #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-starlark"
   #:inputs (list tree-sitter-python)))

(define-public tree-sitter-sway
  (let ((commit "f9e53e922496dd47208a141fa7ac315625a1874f")
        (revision "0"))
    (tree-sitter-grammar
     "sway" "Sway"
     "1bgr3153wwzgkrnk5w9mv7kskw3ygdxaihrpiljcw0bhciqsnkk8"
     (git-version "1.0.0" revision commit)
     #:repository-url "https://github.com/FuelLabs/tree-sitter-sway"
     #:commit commit)))

(define-public tree-sitter-tlaplus
  (tree-sitter-grammar
   "tlaplus" "TLA+"
   "11073rjg9n9hnr88193mmb8ff5xlcv6isgqc0g2fbfbl0y820zlk"
   "1.5.0"
   #:repository-url "https://github.com/tlaplus-community/tree-sitter-tlaplus"
   #:commit "1.5.0"))

(define-public tree-sitter-toml
  (tree-sitter-grammar
   "toml" "TOML"
   "1j9fgy1hlsq6xqqnpjl65a2mmydlx91p8ss36k7vyf7284d6bm4v"
   "0.7.0"
   #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-toml"))

(define-public tree-sitter-typescript
  (tree-sitter-grammar
   "typescript" "TypeScript and TSX"
   "0rlhhqp9dv6y0iljb4bf90d89f07zkfnsrxjb6rvw985ibwpjkh9"
   "0.23.2"
   #:inputs (list tree-sitter-javascript)
   #:grammar-directories '("typescript" "tsx")))

(define-public tree-sitter-typst
  (tree-sitter-grammar
   "typst" "Typst"
   "198rn8wdcqq51nz6hj1zxk7mplplr2z8vc36z6h1zpjjq925794z"
   "0.11.0"
   #:repository-url "https://github.com/uben0/tree-sitter-typst"
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         ;; These tests are supposed to fail, but they don't fail the way the
         ;; suite expects anymore.
         (delete-file "test/corpus/negative.scm")
         #$(tree-sitter-delete-generated-files grammar-directories)))))

(define-public tree-sitter-ungrammar
  ;; No releases yet.
  (let ((commit "debd26fed283d80456ebafa33a06957b0c52e451")
        (revision "0"))
    (tree-sitter-grammar
     "ungrammar" "Ungrammar"
     "09bbml1v1m6a9s9y9q1p2264ghf3fhb6kca1vj3qm19yq87xrnvy"
     (git-version "0.0.2" revision commit)
     #:commit commit
     #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-ungrammar"
     #:article "an")))

(define-public tree-sitter-verilog
  (tree-sitter-grammar
   "verilog" "Verilog"
   "1mk8waij5lbj1wbayvqs0cxk003dssdic13h14gd5fi1ckfvflja"
   "1.0.3"))

(define-public tree-sitter-vhdl
  (let ((version "0.1.1") ; In package.json, but untagged
        (commit "a3b2d84990527c7f8f4ae219c332c00c33d2d8e5")
        (revision "0"))
    (tree-sitter-grammar
     "vhdl" "VHDL"
     "0gz2b0qg1jzi2q6wgj6k6g35kmni3pqglq4f5kblkxx909463n8a"
     (git-version version revision commit)
     #:repository-url "https://github.com/alemuller/tree-sitter-vhdl"
     #:commit commit
     #:get-cleanup-snippet
     (lambda _
       #~(begin
           (use-modules (guix build utils))
           (delete-file "binding.gyp")
           ;; tree-sitter-vhdl does not have bindings/ directory.
           (delete-file "src/grammar.json")
           (delete-file "src/node-types.json")
           (delete-file "src/parser.c")
           (delete-file-recursively "src/tree_sitter")
           ;; FIXME: Language not found.
           (delete-file-recursively "test/highlight")
           ;; Fix a query error in the highlight.scm query test. This would be
           ;; easier with a patch, but this works too, and we still get to use
           ;; tree-sitter-grammar. The fix is taken from here:
           ;; https://github.com/n8tlarsen/tree-sitter-vhdl/commit/dabf157c6bb7220d72d3ceba0ce1abd90bf62187
           ;; This is a documented issue that has not been resolved for nearly 2
           ;; years.
           ;; https://github.com/alemuller/tree-sitter-vhdl/issues/2
           (substitute* "queries/highlights.scm"
             (("\\(integer_decimal\n") "(integer_decimal)\n")
             (("\\(integer\\)") "")
             (("\"0\")") "\"0\"")))))))

(define-public tree-sitter-vim
  (tree-sitter-grammar "vim"
   "Vimscript"
   "0wr0sijh3vpka0gysbf0ki8zkvwfg8r5lvhi3xbwmkbyszjzgrqw"
   "0.7.0"
   #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-vim"))

(define-public tree-sitter-vimdoc
  (tree-sitter-grammar "vimdoc"
   "Vimdoc"
   "1gi16hmh4vk9hdfkg9kvwxd7m4rq8r6vymk7fgxqqrbyrks9f0mw"
   "4.0.0"
   #:repository-url
   "https://github.com/neovim/tree-sitter-vimdoc"
   #:license license:expat))

(define-public tree-sitter-yaml
  (tree-sitter-grammar
   "yaml" "YAML"
   "0z5fz9hiafzapi0ijhyz8np6rksq6c1pb16xv1vhnlfh75rg6zyv"
   "0.7.0"
   #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))

(define-public tree-sitter-zig
  (tree-sitter-grammar
   "zig" "Zig"
   "1r9p7hhnc1zagwxzdxhs4p6rnqs9naddkgbfymi6pbw6cyg2ccwl"
   "1.1.2"
   #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-zig"))

;;; Python Bindings

(define-public python-tree-sitter
  (package
    (name "python-tree-sitter")
    (version "0.21.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/tree-sitter/py-tree-sitter")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cdxl0zyldml3x5wi2nmlmhwfahwxalcr5lxyb6j6762irmm4b2c"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'unittest
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-tree-sitter-lib-path
            (lambda _
              (let ((tree-sitter #$(this-package-input "tree-sitter")))
                (substitute* "setup.py"
                  ((".*\"tree_sitter/core.*") "")
                  (("( *)sources=" all tabs)
                   (string-append
                    tabs "library_dirs=[\"" tree-sitter "/lib\"],\n"
                    tabs "libraries=[\"tree-sitter\"],\n"
                    all))
                  (("include_dirs=\\[" all)
                   (string-append all "\"" tree-sitter "/include\""))))))
          (add-before 'check 'set-test-lib-paths
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (substitute* "tests/test_tree_sitter.py"
                (("Language\\.build_library")
                 "_ =")
                (((string-append "LIB_PATH, \"("
                                 (string-join
                                  '("embedded_template"
                                    "html"
                                    "javascript"
                                    "json"
                                    "python"
                                    "rust")
                                  "|")
                                 ")\"")
                  all name)
                 (string-append
                  (format #f "~s, ~s"
                          (search-input-file
                           (or native-inputs inputs)
                           (string-append
                            "lib/tree-sitter/libtree-sitter-" name ".so"))
                          name))))))
          ;; XXX: See https://codeberg.org/guix/guix/issues/2108
          (add-before 'check 'remove-uninstalled-package
            (lambda _
              (delete-file-recursively "tree_sitter"))))))
    (inputs (list tree-sitter))
    (native-inputs
     (list tree-sitter-embedded-template
           tree-sitter-html
           tree-sitter-javascript
           tree-sitter-json
           tree-sitter-python
           tree-sitter-rust
           python-setuptools
           python-wheel))
    (home-page "https://github.com/tree-sitter/py-tree-sitter")
    (synopsis "Python bindings to the Tree-sitter parsing library")
    (description "This package provides Python bindings to the
Tree-sitter parsing library.")
    (license license:expat)))

(define* (python-tree-sitter-grammar pkg #:key tests?)
  "Returns a package for Python bindings of a Tree-sitter grammar.  PKG is a
package for a Tree-sitter grammar; its name will be used with python- prefix
to generate the package name.  When TESTS? is true, tests are enabled."
  (package
    (inherit pkg)
    (name (string-append "python-" (package-name pkg)))
    (source (origin (inherit (package-source pkg))
                    (snippet #f) (patches '())))
    (build-system pyproject-build-system)
    (arguments (list #:tests? tests?))
    (build-system pyproject-build-system)
    (native-inputs (append (if tests?
                               (list python-pytest
                                     python-tree-sitter)
                               '())
                           (list python-setuptools
                                 python-wheel)))
    (description
     (string-append (package-description pkg)
                    (P_ "\n\nThis variant provides Python bindings.")))))

(define-public python-tree-sitter-html
  (python-tree-sitter-grammar
   tree-sitter-html
   ;; TODO: Enable tests once python-tree-sitter >= 0.22 is packaged
   #:tests? #f))

(define-public python-tree-sitter-javascript
  (python-tree-sitter-grammar
   tree-sitter-javascript
   ;; TODO: Enable tests once python-tree-sitter >= 0.22 is packaged
   #:tests? #f))

(define-public python-tree-sitter-json
  (python-tree-sitter-grammar
   tree-sitter-json
   ;; TODO: Enable tests once python-tree-sitter >= 0.22 is packaged
   #:tests? #f))

(define-public python-tree-sitter-python
  (python-tree-sitter-grammar
   tree-sitter-python
   ;; TODO: Enable tests once python-tree-sitter >= 0.22 is packaged
   #:tests? #f))

(define-public python-tree-sitter-rust
  (python-tree-sitter-grammar
   tree-sitter-rust
   ;; TODO: Enable tests once python-tree-sitter >= 0.22 is packaged
   #:tests? #f))
