;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015-2022, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2016 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2016 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017, 2018, 2020, 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2017 Feng Shu <tumashu@163.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2014 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2019-2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Tom Zander <tomz@freedommail.ch>
;;; Copyright © 2020 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2020 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 aecepoglu <aecepoglu@fastmail.fm>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Calum Irwin <calumirwin1@gmail.com>
;;; Copyright © 2022 Luis Henrique Gomes Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2022 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022, 2024 jgart <jgart@dismail.de>
;;; Copyright © 2022 Andy Tai <atai@atai.org>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2023 Eidvilas Markevičius <markeviciuseidvilas@gmail.com>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 David Pflug <david@pflug.io>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024, 2025 Spencer King <spencer.king@wustl.edu>
;;; Copyright © 2024 Murilo <murilo@disroot.org>
;;; Copyright © 2025 Ashvith Shetty <ashvithshetty0010@zohomail.in>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages text-editors)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public ed
  (package
    (name "ed")
    (version "1.20.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/ed/ed-"
                                 version ".tar.lz"))
             (sha256
              (base32
               "1jmvpbs2mnrmk870js11v7g5qr3z8w0ws7sbdj3zjhd1jyr6795i"))))
    (build-system gnu-build-system)
    (native-inputs (list lzip))
    (arguments
     `(#:configure-flags (list ,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (add-before 'patch-source-shebangs 'patch-test-suite
                     (lambda _
                       (substitute* "testsuite/check.sh"
                         (("/bin/sh") (which "sh"))))))))
    (home-page "https://www.gnu.org/software/ed/")
    (synopsis "Line-oriented text editor")
    (description
     "Ed is a line-oriented text editor: rather than offering an overview of
a document, ed performs editing one line at a time.  It can be executed both
interactively and via shell scripts.  Its method of command input allows
complex tasks to be performed in an automated way.  GNU ed offers several
extensions over the standard utility.")
    (license license:gpl3+)))

(define-public micro
  (let ((commit "04c577049ca898f097cd6a2dae69af0b4d4493e1")
        (date "April 16, 2025"))
    (package
      (name "micro")
      (version "2.0.14")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zyedidia/micro")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1pcfsaq7k6q59vh3xgh8gy350apkv5rkc09d4fh15lx7m6bxbwka"))))
      (build-system go-build-system)
      (arguments
       (list
        #:install-source? #f
        #:import-path "github.com/zyedidia/micro/v2/cmd/micro"
        #:unpack-path "github.com/zyedidia/micro/v2"
        #:build-flags
        ;; Set value for variables in the `--version` flag
        #~(list (format #f "-ldflags=~a ~a ~a"
                        (string-append
                         "-X github.com/zyedidia/micro/v2/internal/util.Version="
                         #$version)
                        (string-append
                         "-X github.com/zyedidia/micro/v2/internal/util.CommitHash="
                         (string-take #$commit 7))
                        (format #f
                         "-X 'github.com/zyedidia/micro/v2/internal/util.CompileDate=~a'"
                         #$date)))
        ;; Step away from cmd/micro to test the whole project.
        #:test-subdirs
        #~(list "../../...")
        #:phases
        #~(modify-phases %standard-phases
            ;; Generate syntax files.
            (add-before 'build 'go-generate
              (lambda _
                (invoke "go" "generate" "-v" "-x"
                        "github.com/zyedidia/micro/v2/runtime"))))))
      (inputs (list go-github-com-blang-semver
                    go-github-com-dustin-go-humanize
                    go-github-com-go-errors-errors
                    go-github-com-layeh-gopher-luar
                    go-github-com-mattn-go-isatty
                    go-github-com-mitchellh-go-homedir
                    go-github-com-sergi-go-diff
                    go-github-com-stretchr-testify
                    go-github-com-yuin-gopher-lua
                    go-github-com-zyedidia-clipper
                    go-github-com-zyedidia-glob
                    go-github-com-zyedidia-go-runewidth
                    go-github-com-zyedidia-go-shellquote
                    go-github-com-zyedidia-json5
                    go-github-com-zyedidia-tcell-v2
                    go-github-com-zyedidia-terminal
                    go-golang-org-x-text
                    go-gopkg-in-yaml-v2))
      (home-page "https://github.com/zyedidia/micro")
      (synopsis "Modern and intuitive terminal-based text editor")
      (description
       "@code{micro} is a terminal-based text editor that aims to be easy to use and
intuitive, while also taking advantage of the capabilities of modern terminals.")
      ;; The project lists licenses of all used sources in LICENSE-THIRD-PARTY
      ;; file which are already included in Guix's package definition.
      (license license:expat))))

(define-public lem
  (let ((commit "96aec3ab957607e0ff35ff018d5e05ec4d52faa0")
        (revision "6"))
    (package
      (name "lem")
      (version (git-version "2.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lem-project/lem/")
               (commit commit)))
         (sha256
          (base32 "1087s9vylx319mz7ld73f4h71gh62hw2f64a94dz9m25cqhsiq6d"))
         (file-name (git-file-name name version))
         (snippet
          #~(begin
              (use-modules (guix build utils))
              (delete-file-recursively "roswell")
              ;; Delete precompiled shared object files.
              (delete-file-recursively "extensions/terminal/lib")))))
      (build-system asdf-build-system/sbcl)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-shared-object-files
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((libvterm-lib (assoc-ref inputs "libvterm"))
                       (lib-dir (string-append libvterm-lib "/lib"))
                       (shared-lib-dir (string-append (assoc-ref outputs "out")
                                                      "/lib"))
                       (shared-lib (string-append shared-lib-dir
                                                  "/terminal.so")))

                  (substitute* "extensions/terminal/ffi.lisp"
                    (("terminal.so") shared-lib)))))
            (add-after 'create-asdf-configuration 'build-program
              (lambda* (#:key outputs #:allow-other-keys)
                (build-program
                 (string-append (assoc-ref outputs "out") "/bin/lem")
                 outputs
                 #:dependencies '("lem-ncurses" "lem-sdl2")
                 #:entry-program '((lem:main) 0))))
            (add-after 'build 'build-terminal-library
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((libvterm-lib (assoc-ref inputs "libvterm"))
                       (lib-dir (string-append libvterm-lib "/lib"))
                       (shared-lib-dir (string-append (assoc-ref outputs "out")
                                                      "/lib"))
                       (shared-lib (string-append shared-lib-dir
                                                  "/terminal.so")))
                  (mkdir-p shared-lib-dir)
                  (invoke #$(cc-for-target)
                          "extensions/terminal/terminal.c"
                          "-L" lib-dir "-lvterm"
                          "-Wl,-Bdynamic"
                          "-o" shared-lib
                          "-shared"
                          "-fPIC"
                          "-lutil")))))))
      (native-inputs
       (list sbcl-cl-ansi-text
             sbcl-rove
             sbcl-trivial-package-local-nicknames))
      (inputs
       (list
        libvterm
        sbcl-alexandria
        sbcl-trivia
        sbcl-trivial-gray-streams
        sbcl-trivial-types
        sbcl-cl-ppcre
        sbcl-closer-mop
        sbcl-iterate
        sbcl-lem-mailbox
        sbcl-inquisitor
        sbcl-babel
        sbcl-bordeaux-threads
        sbcl-yason
        sbcl-log4cl
        sbcl-split-sequence
        sbcl-cl-str
        sbcl-dexador
        sbcl-3bmd
        sbcl-micros
        sbcl-lisp-preprocessor
        sbcl-trivial-ws
        sbcl-trivial-open-browser
        sbcl-sdl2
        sbcl-sdl2-ttf
        sbcl-sdl2-image
        sbcl-trivial-main-thread
        sbcl-cffi
        sbcl-cl-charms
        sbcl-cl-setlocale
        sbcl-log4cl
        sbcl-jsonrpc
        sbcl-usocket
        sbcl-quri
        sbcl-cl-change-case
        sbcl-async-process
        sbcl-cl-iconv
        sbcl-esrap
        sbcl-parse-number
        sbcl-cl-package-locks
        sbcl-slime-swank
        sbcl-trivial-utf-8))
      (home-page "http://lem-project.github.io/")
      (synopsis "Integrated IDE/editor for Common Lisp")
      (description "Lem is a Common Lisp editor/IDE with high expansibility.")
      (license license:expat))))

(define-public vis
  (package
    (name "vis")
    (version "0.8")                     ; also update the vis-test input
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~martanne/vis")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0ija192c9i13gbikm707jynf6my212i040ls0f8pgkbiyvls7xay"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-test-suite
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((vis-test (assoc-ref inputs "vis-test")))
               (copy-recursively vis-test "test")
               #t)))
         (delete 'check)                ; the tests need a wrapped vis
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lpeg (assoc-ref inputs "lua-lpeg"))
                    (lua-version ,(version-major+minor (package-version lua)))
                    (LUA_PATH (string-append lpeg "/share/lua/"
                                             lua-version "/?.lua"))
                    (LUA_CPATH (string-append lpeg "/lib/lua/"
                                              lua-version "/?.so")))
               (wrap-program (string-append out "/bin/vis")
                 `("LUA_PATH" ":" prefix (,LUA_PATH))
                 `("LUA_CPATH" ":" prefix (,LUA_CPATH)))
               #t)))
         (add-after 'wrap-binary 'check
           (assoc-ref %standard-phases 'check))
         (add-before 'check 'set-up-tests
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; DEFAULT_COMPILER is hard-coded here.
               (substitute* "test/core/ccan-config.c"
                 (("\"cc\"")
                  (format #f "\"~a\"" ,(cc-for-target))))
               ;; Use the ‘vis’ executable that we wrapped above.
               (install-file (string-append out "/bin/vis") ".")))))))
    (native-inputs
     `(("vis-test"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://git.sr.ht/~martanne/vis-test")
                 (commit "bbd2f34ff788e87a51a74069069273ad83c44f1f")))
           (sha256
            (base32 "1jsvg2lg3xqfgi79x08kx94mc34mh62ivca10vsci6fqsk68jbd0"))
           (file-name (git-file-name "vis-test" version))))))
    (inputs (list bash-minimal lua ncurses libtermkey lua-lpeg tre))
    (synopsis "Vim-like text editor")
    (description
     "Vis aims to be a modern, legacy free, simple yet efficient vim-like text
editor.  It extends vim's modal editing with built-in support for multiple
cursors/selections and combines it with sam's structural regular expression
based command language.")
    (home-page "https://github.com/martanne/vis")
    (license (list license:isc               ; Main distribution.
                   license:public-domain     ; map.[ch]
                   license:expat))))         ; lexers and libutf.[ch]

(define-public kakoune
  (package
    (name "kakoune")
    (version "2024.05.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mawww/kakoune/"
                           "releases/download/v" version "/"
                           "kakoune-" version ".tar.bz2"))
       (sha256
        (base32 "1ymr1jpdnd5wj6npzi8bgfd30d0j885sfnhl236rn7fjc4pars6s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; kakoune uses confstr with _CS_PATH to find out where to find
             ;; a posix shell, but this doesn't work in the build
             ;; environment. This substitution just replaces that result
             ;; with the "sh" path.
             (substitute* "src/shell_manager.cc"
               (("if \\(m_shell.empty\\(\\)\\)" line)
                (string-append "m_shell = \"" (which "sh")
                               "\";\n        " line)))))
         (delete 'configure))))            ; no configure script
    (native-inputs (list pkg-config))
    (synopsis "Vim-inspired code editor")
    (description
     "Kakoune is a code editor heavily inspired by Vim, as such most of its
commands are similar to Vi's ones, and it shares Vi's \"keystrokes as a text
editing language\" model.  Kakoune has a strong focus on interactivity, most
commands provide immediate and incremental results, while still being
competitive (as in keystroke count) with Vim.")
    (home-page "https://kakoune.org/")
    (license license:unlicense)))

(define-public kak-lsp
  (package
    (name "kak-lsp")
    (version "18.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kak-lsp/kak-lsp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 "1dinxd3h0dllws1v28l1igvns94j91kifv1bxjds6859q22yhhpd")))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-clap" ,rust-clap-4)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-daemonize" ,rust-daemonize-0.5)
        ("rust-diffs" ,rust-diffs-0.5)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-enum-primitive" ,rust-enum-primitive-0.1)
        ("rust-fs4" ,rust-fs4-0.8)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-indoc" ,rust-indoc-2)
        ("rust-itertools" ,rust-itertools-0.13)
        ("rust-jsonrpc-core" ,rust-jsonrpc-core-18)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-lsp-types" ,rust-lsp-types-0.95)
        ("rust-mio" ,rust-mio-1)
        ("rust-notify-debouncer-full" ,rust-notify-debouncer-full-0.3)
        ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.9)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-ropey" ,rust-ropey-1)
        ;("rust-sentry" ,rust-sentry-0.35)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-repr" ,rust-serde-repr-0.1)
        ("rust-slog" ,rust-slog-2)
        ("rust-slog-scope" ,rust-slog-scope-4)
        ("rust-sloggers" ,rust-sloggers-2)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-url" ,rust-url-2)
        ("rust-whoami" ,rust-whoami-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-optional-crash-reporting
           (lambda _
             (substitute* "Cargo.toml"
               ((".*sentry.*") "")))))))
    (home-page "https://github.com/kak-lsp/kak-lsp")
    (synopsis "Language Server Protocol (LSP) client for Kakoune")
    (description
     "kak-lsp is a Language Server Protocol client for Kakoune implemented in
Rust.")
    (license license:unlicense)))

(define-public parinfer-rust
  (package
    (name "parinfer-rust")
    (version "0.4.3")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eraserhd/parinfer-rust")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0hj5in5h7pj72m4ag80ing513fh65q8xlsf341qzm3vmxm3y3jgd"))
       (file-name (git-file-name name version))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-emacs" ,rust-emacs-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-unicode-width" ,rust-unicode-width-0.1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-plugins-and-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (vimfiles (string-append out "/share/vim/vimfiles/"
                                             "pack/guix/start/parinfer")))
               (with-directory-excursion "target/release"
                 (install-file "libparinfer_rust.so" lib))
               (substitute* "plugin/parinfer.vim"
                            (("(let s:libdir = ).*" all libdir)
                             (format #f "~a'~a'\n" libdir lib)))
               (install-file "doc/parinfer.txt"
                             (string-append vimfiles "/doc"))
               (install-file "plugin/parinfer.vim"
                             (string-append vimfiles "/plugin"))
               (install-file "rc/parinfer.kak"
                             (string-append out "/share/kak/autoload"))))))))
    (inputs
     (list clang))
    (home-page "https://github.com/justinbarclay/parinfer-rust")
    (synopsis "Infer parentheses for Clojure, Lisp and Scheme")
    (description
     "Parinfer is a plugin for Kakoune, Vim, Neovim and Emacs that infers
parentheses and indentation.  This library can be called from other editors that
can load dynamic libraries.")
    (license license:expat)))

(define-public parinfer-rust-emacs
  (package
    (name "parinfer-rust-emacs")
    (version "0.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/justinbarclay/parinfer-rust-emacs")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1gay4m6hd893p5m3fayfdqxncg8cg9kw60w5qm8z14p9nxyqb0i5"))
       (file-name (git-file-name name version))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-inputs (list rust-getopts-0.2
                           rust-libc-0.2
                           rust-emacs-0.19
                           rust-serde-1
                           rust-serde-json-1
                           rust-serde-derive-1
                           rust-stdweb-0.4
                           rust-unicode-segmentation-1
                           rust-unicode-width-0.1
                           rust-winapi-0.3)
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'install-library
                     (lambda _
                       (let ((lib (string-append #$output "/lib")))
                         (with-directory-excursion "target/release"
                           (install-file "libparinfer_rust.so" lib))))))))
    (inputs (list clang))
    (home-page "https://github.com/justinbarclay/parinfer-rust-emacs")
    (synopsis "Emacs-centric fork of parinfer-rust")
    (description
     "@command{parinfer-rust-emacs} is an Emacs-centric fork of parinfer-rust,
itself an implementation of Shaun Lebron’s Parinfer.  This builds a shared
library intended to be loaded by the @command{emacs-parinfer-rust-mode} Emacs
plugin, though a standalone binary is built also.")
    (license license:isc)))

(define-public helix
  (package
    (name "helix")
    (version "23.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/helix-editor/helix")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gl5iplj9x58pgqvb296d70xgq8fslqk8chai2arn65bcbgaw014"))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:install-source? #f
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'disable-grammar-build
             (lambda _
               (setenv "HELIX_DISABLE_AUTO_GRAMMAR_BUILD" "1")))
           (replace 'install
             (lambda _
               (let* ((bin (string-append #$output "/bin"))
                      (hx (string-append bin "/hx"))
                      (share (string-append #$output "/share/helix"))
                      (runtime (string-append share "/runtime"))
                      (applications (string-append share "/applications")))
                 (install-file "target/release/hx" bin)
                 (install-file "contrib/Helix.desktop" applications)
                 (copy-recursively "runtime" runtime)
                 (wrap-program hx
                   `("HELIX_RUNTIME" prefix
                     (,runtime)))))))
       #:cargo-inputs
       (list rust-ahash-0.8
             rust-anyhow-1
             rust-arc-swap-1
             rust-bitflags-2
             rust-cassowary-0.3
             rust-cc-1
             rust-chardetng-0.1
             rust-chrono-0.4
             rust-clipboard-win-4
             rust-content-inspector-0.2
             rust-crossterm-0.27
             rust-dunce-1
             rust-encoding-rs-0.8
             rust-etcetera-0.8
             rust-fern-0.6
             rust-futures-executor-0.3
             rust-futures-util-0.3
             rust-gix-0.55
             rust-globset-0.4
             rust-grep-regex-0.1
             rust-grep-searcher-0.1
             rust-hashbrown-0.14
             rust-ignore-0.4
             rust-imara-diff-0.1
             rust-libc-0.2
             rust-libloading-0.8
             rust-log-0.4
             rust-lsp-types-0.94
             rust-nucleo-0.2
             rust-once-cell-1
             rust-parking-lot-0.12
             rust-pulldown-cmark-0.9
             rust-regex-1
             rust-ropey-1
             rust-rustix-0.38
             rust-serde-1
             rust-serde-json-1
             rust-signal-hook-0.3
             rust-signal-hook-tokio-0.3
             rust-slotmap-1
             rust-smallvec-1
             rust-smartstring-1
             rust-tempfile-3
             rust-termini-1
             rust-textwrap-0.16
             rust-thiserror-1
             rust-threadpool-1
             rust-tokio-1
             rust-tokio-stream-0.1
             rust-toml-0.7
             rust-tree-sitter-0.20
             rust-unicode-general-category-0.6
             rust-unicode-segmentation-1
             rust-unicode-width-0.1
             rust-url-2
             rust-which-4)
       #:cargo-development-inputs
       (list rust-fern-0.6
             rust-indoc-2
             rust-quickcheck-1
             rust-smallvec-1
             rust-tempfile-3)))
    (inputs (list bash-minimal))
    (home-page "https://helix-editor.com/")
    (synopsis "Post-modern modal text editor")
    (description "A Kakoune / Neovim inspired editor, written in Rust.")
    (license (list license:mpl2.0))))

(define-public joe
  (package
    (name "joe")
    (version "4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/joe-editor/"
                           "files/JOE sources/joe-" version "/"
                           "joe-" version ".tar.gz"))
       (sha256
        (base32
         "1pmr598xxxm9j9dl93kq4dv36zyw0q2dh6d7x07hf134y9hhlnj9"))))
    (build-system gnu-build-system)
    (inputs (list ncurses))
    (home-page "https://joe-editor.sourceforge.net/")
    (synopsis "Console screen editor")
    (description
     "JOE is a powerful console screen editor with a \"mode-less\" user
interface similar to many user-friendly editors.  JOE has some of the key
bindings and many of the powerful features of GNU Emacs.")
    (license license:gpl3+)))

(define-public jupp
  (package
    (name "jupp")
    (version "3.1jupp41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.mirbsd.org/MirOS/dist/jupp/joe-"
                           version ".tgz"))
       (sha256
        (base32
         "0by8c640kxdf05xvrj8n6awg5mq847iwkhryz7zzxghryn5fmf3v"))))
    (build-system gnu-build-system)
    (inputs (list ncurses))
    (home-page "http://mirbsd.de/jupp")
    (synopsis "Portable version of JOE’s Own Editor")
    (description
    "Jupp is the portable version of JOE’s Own Editor.  It has been
enhanced by functions intended for programmers or other professional
users.  It comes with the editor flavors known from joe, specifically,
jmacs, joe, jpico, jstar, and rjoe.")
    (license license:gpl1)))

(define-public jucipp
  (package
    (name "jucipp")
    (version "1.7.2")
    (home-page "https://gitlab.com/cppit/jucipp")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))
                                  ;; Two submodules are required which are
                                  ;; developed alongside JuCi++ and difficult
                                  ;; to package separately.
                                  (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "034il3z38a7qvp95f52n9rxbqmh8fxsy416rjak3zzagvfkvzyii"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled copy of nlohmann/json.
                  (delete-file-recursively "lib/json")))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_TESTING=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-tiny-process-library
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   (with-directory-excursion "lib/tiny-process-library"
                     (substitute* '("process_unix.cpp"
                                    "tests/io_test.cpp")
                       (("/bin/sh") (search-input-file (or native-inputs inputs)
                                                       "bin/sh"))))))
               (add-after 'unpack 'disable-some-tests
                 (lambda _
                   (substitute* "tests/CMakeLists.txt"
                     ;; Disable the CMake build test, as it does not test
                     ;; functionality of the package, and requires doing
                     ;; an "in-source" build.
                     (("add_test\\(cmake_(build|file_api)_test.*\\)")
                      "")
                     ;; Disable the git test, as it requires the full checkout.
                     (("add_test\\(git_test.*\\)")
                      ""))))
               (add-before 'check 'pre-check
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   ;; Tests do not expect HOME to be empty.
                   (setenv "HOME" "/etc")

                   ;; Most tests require an X server.
                   (let ((xvfb (search-input-file (or native-inputs inputs)
                                                  "bin/Xvfb"))
                         (display ":1"))
                     (setenv "DISPLAY" display)
                     (system (string-append xvfb " " display " &")))))
               (add-after 'install 'wrap
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; The package needs GTK+ and GtkSourceView on XDG_DATA_DIRS
                   ;; for syntax highlighting to work.  shared-mime-info is
                   ;; necessary for MIME handling.
                   ;; XXX: Ideally we'd reuse glib-or-gtk-wrap here, but it
                   ;; does not pick up "share/gtksourceview-3.0".
                   (wrap-program (string-append #$output "/bin/juci")
                     `("XDG_DATA_DIRS" ":" prefix
                       (,(string-join
                          (cons (string-append #$output "/share")
                                (map (lambda (directory)
                                       (dirname (search-input-directory
                                                 inputs
                                                 (string-append "share/"
                                                                directory))))
                                     '("gtk-3.0" "gtksourceview-3.0" "mime")))
                          ":")))))))))
    (native-inputs
     (list pkg-config xorg-server-for-tests))
    (inputs
     (list aspell
           bash-minimal
           boost
           clang-11               ;XXX: must be the same version as Mesas LLVM
           gtkmm-3
           gtksourceviewmm
           nlohmann-json
           libgit2
           universal-ctags))
    (synopsis "Lightweight C++ IDE")
    (description
     "juCi++ is a small @acronym{IDE, Integrated Development Environment}
designed especially towards libclang with speed, stability, and ease of use
in mind.

It supports autocompletion, on-the-fly warnings and errors, syntax
highlighting, and integrates with Git as well as the CMake and Meson build
systems.")
    (license license:expat)))

(define-public leafpad
  (package
    (name "leafpad")
    (version "0.8.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/"
                                  "leafpad/leafpad-" version ".tar.gz"))
              (sha256
               (base32
                "0b0az2wvqgvam7w0ns1j8xp2llslm1rx6h7zcsy06a7j0yp257cm"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list gtk+-2))
    (home-page "http://tarot.freeshell.org/leafpad/")
    (synopsis "GTK+ based text editor")
    (description "Leafpad is a GTK+ text editor that emphasizes simplicity.  As
development focuses on keeping weight down to a minimum, only the most essential
features are implemented in the editor.  Leafpad is simple to use, is easily
compiled, requires few libraries, and starts up quickly.")
    (license license:gpl2+)))

(define-public l3afpad
  (let ((commit "16f22222116b78b7f6a6fd83289937cdaabed624")
        (revision "0"))
    (package
      (name "l3afpad")
      (version (git-version "0.8.18.1.11" revision commit))
      (source (origin
                (method git-fetch)
                (file-name (git-file-name name version))
                (uri (git-reference
                       (url "https://github.com/stevenhoneyman/l3afpad")
                       (commit commit)))
                (sha256
                 (base32
                  "0q55351lvvlw9bi35l49mxa43g4fv110pwprzkk9m5li77vb0bcp"))))
      (build-system glib-or-gtk-build-system)
      (arguments
        (list
         #:phases
         #~(modify-phases %standard-phases
            (add-after 'install 'install-documentation
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (doc (string-append out "/share/doc/" #$name "-"
                                           #$(package-version this-package)))
                       (man (string-append out "/share/man/man1")))
                  (install-file "l3afpad.1" man)
                  (install-file "README" doc)))))))
      (native-inputs
       (list intltool autoconf automake pkg-config))
      (inputs
       (list gtk+))
      (home-page "http://tarot.freeshell.org/leafpad/")
      (synopsis "GTK+ 3 based text editor")
      (description "L3afpad is a GTK+ 3 text editor that emphasizes simplicity.  As
  development focuses on keeping weight down to a minimum, only the most essential
  features are implemented in the editor.  L3afpad is simple to use, is easily
  compiled, requires few libraries, and starts up quickly.  L3afpad is a
  fork of Leafpad that uses GTK+ 3 instead of GTK+ 2.")
      (license license:gpl2+))))

(define-public e3
  (package
    (name "e3")
    (version "2.82")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sites.google.com/site/e3editor/Home/"
                                  "e3-" version ".tgz"))
              (sha256
               (base32
                "0919kadkas020maqq37852isnzp053q2fnws2zh3mz81d1jiviak"))
              (modules '((guix build utils)))

              ;; Remove pre-built binaries.
              (snippet '(begin
                          (delete-file-recursively "bin")
                          #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (native-inputs
     (list nasm))
    (home-page "https://sites.google.com/site/e3editor/")
    (synopsis "Tiny text editor written in assembly")
    (description
     "e3 is a micro text editor with an executable code size between 3800 and
35000 bytes.  Except for ``syntax highlighting'', the e3 binary supports all
of the basic functions one expects plus built in arithmetic calculations.
UTF-8 coding of unicode characters is supported as well.  e3 can use
Wordstar-, EMACS-, Pico, Nedit or vi-like key bindings.  e3 can be used on
16, 32, and 64-bit CPUs.")
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:gpl2+)))

(define-public mg
  (package
    (name "mg")
    (version "20240709")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hboetes/mg")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02q3976glcih0icqvfz2fxrc723si57q080ba4ali5hw4wwggnk4"))
              (modules '((guix build utils)))
              (snippet '(begin
                          (substitute* "GNUmakefile"
                            (("/usr/bin/") ""))))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list diffutils libbsd ncurses))
    (arguments
     ;; No test suite available.
     (list #:tests? #f
           #:make-flags
           #~(list (string-append "prefix=" #$output)
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "PKG_CONFIG=" #$(pkg-config-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ;no configure script
               (add-before 'build 'correct-location-of-diff
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "buffer.c"
                     (("/usr/bin/diff")
                      (search-input-file inputs "/bin/diff")))))
               (add-before 'install 'patch-tutorial-location
                 (lambda _
                   (substitute* "mg.1"
                     (("/usr") #$output))))
               (add-after 'install 'install-tutorial
                 (lambda _
                   (let ((doc (string-append #$output "/share/doc/mg")))
                     (install-file "tutorial" doc)))))))
    (home-page "https://homepage.boetes.org/software/mg/")
    (synopsis "Microscopic GNU Emacs clone")
    (description
     "Mg (@command{mg}) is a GNU Emacs style editor, with which it is
\"broadly\" compatible.  This is a portable version of the mg maintained by the
OpenBSD team.")
    (license license:public-domain)))

(define-public nano
  (package
    (name "nano")
    (version "8.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/nano/nano-" version ".tar.xz"))
      (sha256
       (base32 "13b2iqmbx5vcyd0rnvvc9l8l79q6665r4xx6gvc28mnmpci95ljs"))))
    (build-system gnu-build-system)
    (arguments
     (if (%current-target-system)
         ;; fix for crosscompiling; on GNU system strcasecmp always works
         (list #:configure-flags
               #~(list "ac_cv_func_strcasecmp=yes"
                       "gl_cv_func_strcasecmp_works=yes"))
         '()))
    (inputs
     (list gettext-minimal ncurses))
    (home-page "https://www.nano-editor.org/")
    (synopsis "Small, user-friendly console text editor")
    (description
     "GNU nano is a small and simple text editor for use in a terminal.  Besides
basic editing, it supports: undo/redo, syntax highlighting, spell checking,
justifying, auto-indentation, bracket matching, interactive search-and-replace
(with regular expressions), and the editing of multiple files.")
    (license license:gpl3+))) ; some files are under GPLv2+

(define-public qemacs
  (package
    (name "qemacs")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bellard.org/qemacs/"
                           "qemacs-" version ".tar.gz"))
       (sha256
        (base32 "156z4wpj49i6j388yjird5qvrph7hz0grb4r44l4jf3q8imadyrg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-qhtml
           ;; Build fails without first creating qHTML library.
           (lambda _ (invoke "make" "-C" "libqhtml")))
         (add-before 'install 'fix-man-pages-directory
           ;; Install in $out/share/man instead of $out/man.
           (lambda _
             (substitute* "Makefile"
               (("/man/man1" all) (string-append "/share" all)))
             #t))
         (add-before 'install 'create-directories
           ;; Ensure directories exist before installing files.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (d) (mkdir-p (string-append out d)))
                         '("/bin" "/share/man/man1" "/share/qe"))
               #t)))
         (add-after 'install 'install-extra-documentation
           ;; Install sample configuration file, Info, and HTML manual.
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let* ((share (string-append (assoc-ref outputs "out") "/share"))
                    (doc (string-append share "/doc/" ,name "-" ,version))
                    (html (string-append share "/html"))
                    (info (string-append share "/info"))
                    (makeinfo (string-append
                               (assoc-ref (or native-inputs inputs) "texinfo")
                               "/bin/makeinfo")))
               ;; First fix Texinfo documentation, create appropriate
               ;; directories, then generate Info and HTML files there.
               (substitute* "qe-doc.texi"
                 (("^M-([{}])" _ bracket) (string-append "M-@" bracket)))
               (for-each (lambda (d) (mkdir-p d)) (list html info))
               (invoke makeinfo "qe-doc.texi" "-o" info)
               (invoke makeinfo "qe-doc.texi" "--html" "--no-split" "-o" html)
               ;; Install sample configuration file.
               (install-file "config.eg" doc)
               #t))))))
    (native-inputs
     (list texinfo))
    (inputs
     (list libx11 libxext libxv))
    (home-page "https://bellard.org/qemacs/")
    (synopsis "Small but powerful text editor")
    (description "QEmacs (for Quick Emacs) is a very small but
powerful editor.  It has features that even big editors lack:

@itemize

@item Full screen editor with an Emacs look and feel with all Emacs
common features: multi-buffer, multi-window, command mode, universal
argument, keyboard macros, config file with C-like syntax, minibuffer
with completion and history.

@item Can edit files of hundreds of Megabytes without being slow by
using a highly optimized internal representation and by mmaping the
file.

@item Full Unicode support, including multi charset handling (8859-x,
UTF8, SJIS, EUC-JP, ...) and bidirectional editing respecting the
Unicode bidi algorithm.  Arabic and Indic scripts handling (in
progress).

@item WYSIWYG HTML/XML/CSS2 mode graphical editing.  Also supports
Lynx like rendering on VT100 terminals.

@item WYSIWYG DocBook mode based on XML/CSS2 renderer.

@item C mode: coloring with immediate update.  Emacs like auto-indent.

@item Shell mode: colorized VT100 emulation so that your shell work
exactly as you expect.  Compile mode with next/prev error.

@item Input methods for most languages, including Chinese (input
methods come from the Yudit editor).

@item Hexadecimal editing mode with insertion and block commands.
Unicode hexa editing is also supported.

@item Works on any VT100 terminals without termcap.  UTF8 VT100
support included with double width glyphs.

@item X11 support.  Support multiple proportional fonts at the same
time (as XEmacs).  X Input methods supported.  Xft extension supported
for anti aliased font display.

@item Small! Full version (including HTML/XML/CSS2/DocBook rendering
and all charsets): 200KB big.  Basic version (without bidir/unicode
scripts/input/X11/C/Shell/HTML/Dired): 49KB.
@end itemize")
    (license license:lgpl2.1+)))

(define-public manuskript
  (package
    (name "manuskript")
    (version "0.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olivierkes/manuskript")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w1wscq7w0xx4wkkk9rl3pc067yspbk8qnfaq3i9sxc7k6zsy77x"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda _
              (let ((share (string-append #$output "/share/manuskript")))
                ;; Install data.
                (mkdir-p share)
                (for-each
                 (lambda (d)
                   (let ((destination  (string-append share "/" d)))
                     (mkdir-p destination)
                     (copy-recursively d destination)))
                 '("bin" "i18n" "icons" "libs" "manuskript" "resources"))
                ;; Install documentation.
                (let ((doc (string-append #$output
                                          "/doc/manuskript-" #$version
                                          "/sample-projects")))
                  (mkdir-p doc)
                  (copy-recursively "sample-projects" doc))
                ;; Wrap executable in "$out/share/manuskript/bin" and
                ;; link to it from "$out/bin".
                (let ((bin (string-append #$output "/bin"))
                      (executable (string-append share "/bin/manuskript")))
                  (wrap-program executable
                    (list "GUIX_PYTHONPATH" 'prefix
                          (list (getenv "GUIX_PYTHONPATH"))))
                  (mkdir-p bin)
                  (with-directory-excursion bin
                    (symlink (string-append share "/bin/manuskript")
                             "manuskript")))
                ;; Install icons and create .desktop file.
                (let ((apps (string-append #$output "/share/applications"))
                      (icons-dir (string-append #$output "/share/pixmaps")))
                  (install-file "icons/Manuskript/manuskript.svg" icons-dir)
                  (mkdir-p apps)
                  (make-desktop-entry-file
                   (string-append apps "/manuskript.desktop")
                   #:name "Manuskript"
                   #:mime-type "application/x-manuskript-book;"
                   #:exec (string-append #$output "/bin/manuskript %f")
                   #:comment '((#f "Tool for writers")
                               ("es" "Herramienta para escritores/as"))
                   #:keywords "manuskript;office;write;edit;novel;text;msk"
                   #:terminal #f
                   #:type "Application"
                   #:icon "manuskript"
                   #:categories "Office;WordProcessor;"))))))))
    (inputs
     (list bash-minimal
           pandoc
           python-lxml
           python-markdown
           python-pyenchant
           python-pyqt
           qtsvg-5))
    (home-page "http://www.theologeek.ch/manuskript/")
    (synopsis "Tool for writers")
    (description "Manuskript provides a rich environment to help
writers create their first draft and then further refine and edit
their masterpiece.  With Manuskript you can:

@itemize
@item Grow your premise from one sentence, to a paragraph, to a full
summary,
@item Create characters,
@item Conceive plots,
@item Construct outlines (Outline mode and/or Index cards),
@item Write with focus (Distraction free mode),
@item Build worlds,
@item Track items,
@item Edit and re-organize chapters and scenes,
@item View Story line,
@item Compose with fiction or non-fiction templates and writing modes,
@item Import and export document formats such as HTML, ePub,
OpenDocument, DocX, and more.
@end itemize

Additionally Manuskript can help in many more ways with a spell
checker, markdown highlighter, frequency analyzer, and automatic save
in plain text file format.")
    (license license:gpl3+)))

(define-public editorconfig-core-c
  (package
    (name "editorconfig-core-c")
    (version "0.12.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/editorconfig/editorconfig-core-c")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05qllpls3r95nfl14gqq3cv4lisf07fgn85n52w8blc5pfl1h93g"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'insert-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tests (assoc-ref inputs "tests")))
               (copy-recursively tests "tests"))
             #t))
         (add-after 'insert-tests 'disable-failing-tests
           (lambda _
             (substitute* "tests/parser/CMakeLists.txt"
               (("# Test max property name and values")
                "# Disabled: test max property name and values\nif(FALSE)\n")
               (("# Test max section names")
                "endif()\n\n# Test max section names"))))
         (add-after 'install 'delete-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (with-directory-excursion lib
                 (delete-file "libeditorconfig_static.a"))
               #t))))))
    (native-inputs
     `(("tests"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/editorconfig/editorconfig-core-test")
                 ;; The tests submodule commit matching this package's version.
                 (commit "48610d43b7455af12195473377f93c4ceea654f5")))
           (file-name (git-file-name "editorconfig-core-test" version))
           (sha256
            (base32 "1s29p4brmcsc3xsww3gk85dg45f1kk3iykh1air3ij0hymf5dyqy"))))))
    (inputs
     (list pcre2))
    (home-page "https://editorconfig.org/")
    (synopsis "EditorConfig core library written in C")
    (description "EditorConfig makes it easy to maintain the correct coding
style when switching between different text editors and between different
projects.  The EditorConfig project maintains a file format and plugins for
various text editors which allow this file format to be read and used by those
editors.")
    (license license:bsd-2)))

(define-public texmacs
  (package
    (name "texmacs")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.texmacs.org/Download/ftp/tmftp/"
                           "source/TeXmacs-" version "-src.tar.gz"))
       (sha256
        (base32 "11l1q5lmsj9g7yil1dn7n1cgsr8iikx59kg9riahpb6xw0p959l7"))
       (patches
        (search-patches "texmacs-wayland-hidpi.patch"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config xdg-utils))       ;for xdg-icon-resource
    (inputs
     (list freetype
           guile-1.8
           libjpeg-turbo
           libxcrypt
           perl
           python-wrapper
           qtbase-5
           qtsvg-5
           qtwayland-5
           sqlite))
    (arguments
     (list
      #:tests? #f                       ; no check target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-icon-directory
            (lambda _
              (substitute* "packages/linux/icons.sh"
                (("/usr/share")
                 (string-append #$output "/share")))))
          (add-before 'configure 'gzip-flags
            (lambda _
              (substitute* "Makefile.in"
                (("^GZIP = gzip -f") "GZIP = gzip -f -n")))))))
    (synopsis "Editing platform with special features for scientists")
    (description
     "GNU TeXmacs is a text editing platform which is specialized for
scientists.  It is ideal for editing structured documents with different types
of content.  It has robust support for mathematical formulas and plots.  It
can also act as an interface to external mathematical programs such as R and
Octave.  TeXmacs is completely extensible via Guile.")
    (license license:gpl3+)
    (home-page "https://www.texmacs.org/tmweb/home/welcome.en.html")))

(define-public texmacs-guile3
  (package
    (inherit texmacs)
    (name "texmacs-guile3")
    (version "2.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/texmacs/texmacs.git")
             (commit "guile3_branch_2.1")))
       (sha256
        (base32 "0f7l1sfbii25gawqsg27m31myvixb3xdxnsg5njlrnmp8xh1rs3v"))
       (patches
        (search-patches "texmacs-wayland-hidpi.patch"))))
    (inputs
     (list freetype
           guile-3.0
           gmp
           libjpeg-turbo
           libxcrypt
           perl
           python-wrapper
           qtbase-5
           qtsvg-5
           qtwayland-5
           sqlite))))

(define-public mogan
  (package
    (inherit texmacs)
    (name "mogan")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/XmacsLabs/mogan")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04wz6xmimjv2l6baxgzm8vyq5grg102m3l4wq8i6bglv529yp4ff"))
       (patches
        (search-patches "texmacs-wayland-hidpi.patch"))))
    (build-system qt-build-system)
    (inputs
     (modify-inputs (package-inputs texmacs)
       ;; Replaced by S7 scheme
       ;; TODO: Maybe unbundle S7
       (delete "guile")
       (prepend curl)))
    (arguments
     (substitute-keyword-arguments (package-arguments texmacs)
       ((#:phases orig)
        #~(modify-phases #$orig
            ;; The non-deterministic compression issue is solved in Mogan.
            (delete 'gzip-flags)))))
    (home-page "https://github.com/XmacsLabs/mogan")
    (synopsis "Scientific structural text editor")
    (description
     "Mogan is a scientific structural text editor, a fork of GNU TeXmacs.")
    (license license:gpl3+)))

(define-public textpieces
  (package
    (name "textpieces")
    (version "3.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/liferooter/textpieces")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14zq2c7js80m4cq8wpdb3kyz5sw96l8znbz027w8s94gqhm632ff"))))
    (arguments
     '(;; The test suite fails to validate appstream file due to lack of
       ;; network access
       #:tests? #f
       #:glib-or-gtk? #t))
    (build-system meson-build-system)
    (native-inputs
     (list appstream-glib
           blueprint-compiler-0.4
           desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           `(,gtk "bin")
           pkg-config
           vala))
    (inputs
     (list gtk
           gtksourceview
           json-glib
           libadwaita
           libgee
           python
           python-pygobject
           python-pyyaml))
    (home-page "https://github.com/liferooter/textpieces")
    (synopsis "Quick text processor")
    (description
     "Text Pieces is a tool for quick text transformations such as checksums,
encoding, decoding, etc.

The basic features of Text Pieces are:
@itemize
@item Base64 encoding and decoding
@item SHA-1, SHA-2 and MD5 checksums
@item Prettify and minify JSON
@item Covert JSON to YAML and vice versa
@item Count lines, symbols and words
@item Escape and unescape string, URL and HTML
@item Remove leading and trailing whitespaces
@item Sort and reverse sort lines
@item Reverse lines and whole text
@item You can write your own scripts and create custom tools
@end itemize")
    (license license:gpl3)))

(define-public scintilla
  (package
    (name "scintilla")
    (version "5.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.scintilla.org/scintilla"
                           (string-delete #\. version) ".tgz"))
       (sha256
        (base32 "1rvp8aabk0m75fih7fvv6qhhc2ahf93vqbqa2zd0r8627v3llqzx"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list "GTK3=1"
              (string-append "CC=" #$(cc-for-target))
              "-Cgtk")
      #:tests? #f                       ;require un-packaged Pyside
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ;no configure script
          (replace 'install
            ;; Upstream provides no install script.
            (lambda _
              (let ((lib (string-append #$output "/lib"))
                    (inc (string-append #$output "/include")))
                (for-each (lambda (f) (install-file f lib))
                          (find-files "bin/" "\\.so$"))
                (for-each (lambda (f) (install-file f inc))
                          (find-files "include/" "\\.h$"))))))))
    (native-inputs (list pkg-config python-wrapper))
    (inputs (list gtk+))
    (home-page "https://www.scintilla.org/")
    (synopsis "Code editor for GTK+")
    (description "Scintilla is a source code editing component for
GTK+.  It has the usual features found in text editing components, as
well as some that are especially useful for editing and debugging
source code; these include support for syntax styling, error
indicators, code completion and call tips.  Styling choices are more
open than with many editors: Scintilla lets you use proportional
fonts, bold and italics, multiple foreground and background colours,
and multiple fonts.")
    (license license:hpnd)))

(define-public lexilla
  (package
    (name "lexilla")
    (version "5.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.scintilla.org/lexilla"
                           (string-delete #\. version) ".tgz"))
       (sha256
        (base32
         "0sc3z6y82h1vq8aaydp119kymzvrv0p1xvy56r5j996jl6zxikk4"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CXX=" #$(cxx-for-target))
                           (string-append "SCINTILLA_INCLUDE="
                                          #$(this-package-input "scintilla")
                                          "/include"))
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda args
              (with-directory-excursion "src"
                (apply (assoc-ref %standard-phases 'build) args))))
          (add-after 'build 'patch-more-shebangs
            (lambda _
              ;; Patch these bash shebangs to avoid them failing the tests.
              (substitute* '("test/examples/bash/x.bsh.folded"
                             "test/examples/bash/x.bsh.styled")
                (("/usr/bin/env bash")
                 (which "bash")))))
          (replace 'check
            (lambda args
              (with-directory-excursion "test"
                (apply (assoc-ref %standard-phases 'check) args))))
          (add-after 'unpack 'fix-deps.mak
            (lambda _
              (substitute* "src/deps.mak"
                (("../../scintilla")
                 #$(this-package-input "scintilla")))))
          (delete 'configure)           ;no configure script
          (replace 'install
            ;; Upstream provides no install script.
            (lambda _
              (let ((lib (string-append #$output "/lib"))
                    (inc (string-append #$output "/include")))
                (for-each (lambda (f) (install-file f lib))
                          (find-files "bin/" "\\.so$"))
                (for-each (lambda (f) (install-file f inc))
                          (find-files "include/" "\\.h$"))))))))
    (native-inputs (list python))
    (inputs (list scintilla))
    (home-page "https://www.scintilla.org/Lexilla.html")
    (synopsis "Language lexers for Scintilla")
    (description "Lexilla is a library of language lexers that can be
used with the Scintilla editing component.")
    (license license:hpnd)))

(define-public geany
  (package
    (name "geany")
    (version "1.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.geany.org/"
                           "geany-" version ".tar.bz2"))
       (sha256
        (base32 "0inmmb9wra2w99pfv6p64d66s2zrhafc8drhwmga7gj89mp1gzxb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           doxygen
           `(,glib "bin")
           intltool
           libtool
           pkg-config
           python-docutils)) ;for rst2html
    (inputs
     (list gtk+
           ;; FIXME: Geany bundles a 3.X release of Scintilla.  It is not
           ;; currently possible to replace it with our Scintilla package.
           ;; ("scintilla" ,scintilla)
           ))
    (arguments
     `(#:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%default-gnu-imported-modules)
       #:modules (((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (home-page "https://www.geany.org")
    (synopsis "Fast and lightweight IDE")
    (description "Geany is a small and fast Integrated Development
Environment (IDE) that only has a few dependencies on other packages and is as
independent as possible from special desktop environments like KDE or GNOME.

The basic features of Geany are:
@itemize
@item syntax highlighting
@item code completion
@item auto completion of often constructed constructs like if, for and while
@item auto completion of XML and HTML tags
@item call tips
@item folding
@item many supported filetypes like C, Java, PHP, HTML, Python, Perl, Pascal
@item symbol lists
@item embedded terminal emulation
@item extensibility through plugins
@end itemize")
    (license license:gpl2+)))

(define-public fe
  (package
    (name "fe")
    ;; Stable release is 1.9.  However, this development version
    ;; introduces support for UTF-8.
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.moria.de/~michael/fe/"
                                  "fe-" version ".tar.gz"))
              (sha256
               (base32
                "10mk5wc3dsdp46b3hkjyd740gcdv6m1gvlr3p8xjxf55b3vfs0la"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       ;; Sendmail is only used to send a crash log.  Disable the
       ;; feature since it is (1) undocumented (2) not very useful.
       #:configure-flags (list "--disable-sendmail")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (for-each (lambda (f) (install-file f doc))
                         '("fe.doc" "fe.html" "fe.ps" "feref.ps" "README"))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     (list ncurses))
    (home-page "http://www.moria.de/~michael/fe/")
    (synopsis "Small folding editor")
    (description "Fe is a small folding editor.  It folds
arbitrary text regions; it is not bound to syntactic units.

Fe has no configuration or extension language and requires no setup.
Its user interface is emacs-like and it has menus for the very most
important functions to help beginners.  Further there is a reference
card.  It offers:

@itemize
@item Regions and Emacs-like kill ring
@item Incremental search
@item Keyboard macros
@item Editing binary files
@item Multiple windows and views
@item Compose function for Latin 1 characters
@end itemize")
    (license license:gpl2+)))

(define-public ne
  (package
    (name "ne")
    (version "3.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vigna/ne")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nlbfidwsjsdvb82d8cikimd5ym1qlv45bw6bgk8h6zx012w9hwz"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl texinfo))
    (inputs
     (list ncurses))
    (arguments
     (list #:tests? #f
           #:parallel-build? #f         ; or enums.h may not yet be generated
           #:make-flags
           #~(list "STRIP=true"         ; don't
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output)
                   (string-append "LDFLAGS=-L" #$(this-package-input "ncurses")
                                  "/lib"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'patch-early-shebang
                 (lambda _
                   (substitute* "version.pl"
                     (("/usr/bin/env .*perl") (which "perl")))))
               (replace 'configure
                 (lambda _
                   (substitute* "src/makefile"
                     (("-lcurses") "-lncurses")))))))
    (home-page "https://ne.di.unimi.it/")
    (synopsis "Text editor with menu bar")
    (description "This package provides a modeless text editor with menu bar.
It supports syntax highlighting, regular expressions, configurable menus,
keybindings, autocomplete and unlimited undo.  It can pipe a marked block
of text through any command line filter.  It can also open very large binary
files.  It was originally developed on the Amiga 3000T.")
    (license license:gpl3+)))

(define-public hexer
  (package
    (name "hexer")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://devel.ringlet.net/files/editors/hexer/"
                            "hexer-" version ".tar.xz"))
        (sha256
          (base32 "157z17z8qivdin2km2wp86x1bv1nx15frrwcz11mk0l3ab74mf76"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no upstream tests
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "LTERMCAP=-lncurses")
             (string-append "LDFLAGS=-L" (assoc-ref %build-inputs "ncurses")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ;no configure script
    (inputs
     (list ncurses))
    (home-page "https://devel.ringlet.net/editors/hexer/")
    (synopsis "Multi buffer editor for binary files with vi-like interface")
    (description "Hexer is a multi-buffer editor for binary files for Unix-like
systems that displays its buffer(s) as a hex dump.  The user interface is kept
similar to vi/ex.")
    (license license:bsd-3)))

(define-public edlin
  (package
    (name "edlin")
    (version "2.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/freedos-edlin/freedos-edlin/"
                           version "/edlin-" version ".tar.bz2"))
       (sha256
        (base32 "0cdv42ffminncwj5ph9lw0j7zpbv8l35acppy90wj7x1qm4qk6x8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-read-only
           (lambda _
             ;; Remove executable bits.
             (chmod "COPYING" #o444)
             (chmod "edlin.htm" #o444)))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc-dir (string-append (assoc-ref outputs "out")
                                           "/share/doc/edlin-" ,version)))
               (mkdir-p doc-dir)
               (install-file "edlin.htm" doc-dir)))))))
    (home-page "https://sourceforge.net/projects/freedos-edlin/")
    (synopsis "The line editor of the FreeDOS operating system")
    (description "The @code{edlin} program is a small line editor, written for
FreeDOS as a functional clone of the old MS-DOS program edlin.")
    (license license:gpl2+)))

(define-public mle
  (package
    (name "mle")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/adsr/mle")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nhd00lsx9v12zdmps92magz76c2d8zzln3lxvzl4ng73gbvq3n0"))))
    (build-system gnu-build-system)
    (inputs (list lua pcre uthash))
    (arguments
     `(#:test-target "test"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-lua
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "mle.h"
                        (("<lua5.4/") "<"))
                      (substitute* "Makefile"
                        (("-llua5.4") "-llua")
                        (("/bin/sh") (which "sh")))))
                  (add-after 'unpack 'patch-test-shebangs
                    (lambda _
                      (substitute* (find-files "tests/func" "\\.sh$")
                        (("/usr/bin/env bash") (which "bash")))))
                  (delete 'configure) ;no configure script
                  (add-after 'install 'install-man-pages
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (man (string-append out
                                                 "/share/man/man1")))
                        (install-file "mle.1"
                                      (string-append man))))))
       #:make-flags (list (string-append "CC="
                                         ,(cc-for-target))
                          (string-append "prefix=" %output))))
    (home-page "https://github.com/adsr/mle")
    (synopsis "Small, flexible, terminal-based text editor")
    (description
     "mle is a small, flexible, terminal-based text editor written in C.
Notable features include: full Unicode support, syntax highlighting,
scriptable rc file, macros, search and replace (PCRE), window
splitting, multiple cursors, and integration with various shell
commands.")
    (license license:asl2.0)))

(define-public lite-xl
  (package
    (name "lite-xl")
    (version "2.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lite-xl/lite-xl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19wdq8w6ickyynx6r2wg2vf5isl2577zjizgwbzql9vhqdsi8ag3"))))
    (build-system meson-build-system)
    (arguments (list #:configure-flags #~'("-Duse_system_lua=true")))
    (inputs (list lua-5.4 pcre2 freetype sdl2))
    (native-inputs (list pkg-config))
    (home-page "https://lite-xl.com")
    (synopsis "Lightweight text editor written in Lua")
    (description
     "Lite XL is derived from lite.  It is a lightweight text editor written
mostly in Lua.  It aims to provide something practical, pretty, small and fast
easy to modify and extend, or to use without doing either.

The aim of Lite XL compared to lite is to be more user-friendly, improve the
quality of font rendering, and reduce CPU usage.")
    (license license:expat)))

(define-public jed
  (package
    (name "jed")
    (version "0.99-19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.jedsoft.org/releases/jed/jed-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0qspdc6wss43wh1a8fddvf62xyhld5p7hl75grv4d95h5z73k8wp"))
              (modules '((guix build utils)))
              (snippet #~(begin
                           ;; Delete Windows binaries.
                           (delete-file-recursively "bin/w32")

                           (substitute* "src/Makefile.in"
                             (("/bin/cp")
                              "cp"))
                           (substitute* "configure"
                             (("TERMCAP=-ltermcap")
                              "TERMCAP="))))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-slang="
                                  #$(this-package-input "slang")))
           ;; jed provides no tests
           #:tests? #f))
    (inputs (list slang))
    (home-page "https://www.jedsoft.org/jed/")
    (synopsis "Programmer's editor using S-Lang scripting for configuration")
    (description
     "Jed is a powerful programmer's editor using the S-Lang scripting language
for configuration and extensibility.  It provides emulation modes for the
key bindings of many editors (including Emacs and WordStar), and has syntax
highlighting for dozens of languages.  Jed is very small and fast.")
    (license license:gpl2+)))

(define-public xnedit
  (package
    (name "xnedit")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xnedit/" name "-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0mb30xl0j27fs37hxskap50xawv7f1pb6ymp6yh8dz8j4iwrz9lb"))))

    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "PREFIX=" #$output)
                           (string-append "CC=" #$(cc-for-target)))
      #:tests? #f                       ;no tests
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (replace 'build
                     (lambda* (#:key make-flags #:allow-other-keys)
                       (apply invoke "make" "linux" make-flags))))))
    (inputs (list motif))
    (native-inputs (list pkg-config))
    (home-page "https://sourceforge.net/projects/xnedit/")
    (synopsis "Fast and classic X11 text editor")
    (description
     "XNEdit is a fast and classic X11 text editor, based on NEdit,
with full unicode support and antialiased text rendering.")
    (license license:gpl2+)))

(define-public dte
  (package
    (name "dte")
    (version "1.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/craigbarnes/dte")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q43jvhj6r1ws58ngpcq8mzj4di7mj2a4bmigk9zy0f3riagnlv1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list util-linux))
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (home-page "https://craigbarnes.gitlab.io/dte/")
    (synopsis "Small and easy to use console text editor")
    (description
     "@command{dte} is a portable console text editor with features like multiple buffers/tabs,
syntax highlighting, customizable color scheme (including support for 24-bit true
colours), kitty keyboard protocol, editorconfig support, amongst other features.")
    (license license:gpl2)))
