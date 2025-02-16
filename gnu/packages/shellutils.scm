;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2019 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2020, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 aecepoglu <aecepoglu@fastmail.fm>
;;; Copyright © 2020 Dion Mendel <guix@dm9.info>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Alexandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2021 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2022 Jose G Perez Taveras <josegpt27@gmail.com>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2023 Camilo Q.S. (Distopico) <distopico@riseup.net>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Brian Kubisiak <brian@kubisiak.com>
;;; Copyright © 2024 Jordan Moore <lockbox@struct.foo>
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

(define-module (gnu packages shellutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-shell)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim))

(define-public ascii
  (package
    (name "ascii")
    (version "3.30")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.catb.org/~esr/ascii/"
                                  "ascii-" version ".tar.gz"))
              (sha256
               (base32
                "0hr4k4mvnq4zpjxdjkvbbyzz7c1iwxhfal1hz0mdm1qv7sbxqbzd"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-before 'install 'create-directories
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin"))
                          (man1 (string-append out "/share/man/man1")))
                     (mkdir-p bin)
                     (mkdir-p man1)))))
           #:tests? #f))
    (home-page "http://www.catb.org/~esr/ascii/")
    (synopsis "ASCII name and synonym chart")
    (description
      "The @code{ascii} utility provides easy conversion between various byte
representations and the American Standard Code for Information Interchange
(ASCII) character table.  It knows about a wide variety of hex, binary, octal,
Teletype mnemonic, ISO/ECMA code point, slang names, XML entity names, and
other representations.  Given any one on the command line, it will try to
display all others.  Called with no arguments it displays a handy small ASCII
chart.")
    (license license:bsd-2)))

(define-public boxes
  (package
    (name "boxes")
    (version "2.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ascii-boxes/boxes")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "028vg9h3vxz3icy7hmxgyqhn62953h1ls6bxwbhdwhl1lpj5py3n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "utest"
       #:make-flags (list (string-append "GLOBALCONF="
                                         (assoc-ref %outputs "out")
                                         "/etc/boxes-config"))
       #:modules
       ((ice-9 match)
        ,@%default-gnu-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (match-lambda
                           ((source target)
                            (install-file source
                                          (string-append out "/" target))))
                         '(("out/boxes"    "bin/")
                           ("doc/boxes.1"  "share/man/man1/")
                           ("boxes-config" "etc/")))))))))
    (native-inputs
     (list bison flex cmocka
           ;; For the tests.
           xxd))
    (inputs
     (list ncurses libunistring pcre2))
    (home-page "https://boxes.thomasjensen.com")
    (synopsis "Command line ASCII boxes")
    (description
     "This command-line filter program draws ASCII-art boxes around your input
text.")
    (license license:gpl2)))

(define-public zsh-autopair
  (package
    (name "zsh-autopair")
    (version "1.0")
    (home-page "https://github.com/hlissner/zsh-autopair")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hlissner/zsh-autopair.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h0vm2dgrmb8i2pvsgis3lshc5b0ad846836m62y8h3rdb3zmpy1"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("autopair.zsh"
                         "/share/zsh/plugins/zsh-autopair/zsh-autopair.zsh"))))
    (synopsis "Auto-close and delete matching delimiters in Zsh")
    (description
     "This Zsh plugin auto-closes, deletes, and skips over matching delimiters
in Zsh intelligently.")
    (license license:expat)))

(define-public zsh-autosuggestions
  (package
    (name "zsh-autosuggestions")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zsh-users/zsh-autosuggestions")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g3pij5qn2j7v7jjac2a63lxd97mcsgw6xq6k5p7835q9fjiid98"))))
    (build-system gnu-build-system)
    (native-inputs
     (list ruby
           ruby-pry
           ruby-rspec
           ruby-rspec-wait
           tmux
           zsh))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              ;; Failing tests since tmux-3.2a
              (delete-file "spec/options/buffer_max_size_spec.rb")))
          (delete 'configure)
          (replace 'check ; Tests use ruby's bundler; instead execute rspec directly.
            (lambda _
              (setenv "TMUX_TMPDIR" (getenv "TMPDIR"))
              (setenv "SHELL" (which "zsh"))
              (invoke "rspec")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (zsh-plugins
                      (string-append out "/share/zsh/plugins/zsh-autosuggestions")))
                (invoke "make" "all")
                (install-file "zsh-autosuggestions.zsh" zsh-plugins)))))))
    (home-page "https://github.com/zsh-users/zsh-autosuggestions")
    (synopsis "Fish-like autosuggestions for zsh")
    (description
     "Fish-like fast/unobtrusive autosuggestions for zsh.  It suggests commands
as you type.")
    (license license:expat)))

(define-public zsh-completions
  (package
    (name "zsh-completions")
    (version "0.34.0")
    (home-page "https://github.com/zsh-users/zsh-completions")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jjgvzj3v31yibjmq50s80s3sqi4d91yin45pvn3fpnihcrinam9"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("src/" "share/zsh/site-functions/")
                        ("README.md" "share/doc/zsh-completions/"))))
    (synopsis "Additional completion definitions for Zsh")
    (description
     "This projects aims at gathering/developing new completion scripts that
are not available in Zsh yet.  The scripts may be contributed to the Zsh
project when stable enough.")
    (license (license:non-copyleft "file://LICENSE"
              "Custom BSD-like, permissive, non-copyleft license."))))

(define-public zsh-history-substring-search
  (package
    (name "zsh-history-substring-search")
    (version "1.0.2")
    (home-page "https://github.com/zsh-users/zsh-history-substring-search")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y8va5kc2ram38hbk2cibkk64ffrabfv1sh4xm7pjspsba9n5p1y"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("zsh-history-substring-search.plugin.zsh"
                         "share/zsh/plugins/zsh-history-substring-search/")
                        ("zsh-history-substring-search.zsh"
                         "share/zsh/plugins/zsh-history-substring-search/")
                        ("README.md" "share/doc/zsh-history-substring-search/"))))
    (synopsis "ZSH port of Fish history search (up arrow)")
    (description
     "This is a clean-room implementation of the Fish shell's history search
feature, where you can type in any part of any command from history and then
press chosen keys, such as the UP and DOWN arrows, to cycle through matches.")
    (license license:bsd-3)))

(define-public zsh-syntax-highlighting
  (package
    (name "zsh-syntax-highlighting")
    (version "0.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zsh-users/zsh-syntax-highlighting")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "039g3n59drk818ylcyvkciv8k9mf739cv6v4vis1h9fv9whbcmwl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list zsh))
    (arguments
     ;; FIXME: Tests fail when running test regexp
     ;; there is no pcre module in the Guix zsh package
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("/usr/local") out)
                 (("share/\\$\\(NAME\\)") "share/zsh/plugins/$(NAME)")))))
         (add-after 'patch-paths 'make-writable
           (lambda _
             (for-each make-file-writable
                       '("docs/highlighters.md"
                         "README.md"))))
         (add-before 'build 'add-all-md
           (lambda _
             (invoke "make" "all")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "test")
               (invoke "make" "perf")))))))
    (home-page "https://github.com/zsh-users/zsh-syntax-highlighting")
    (synopsis "Fish shell-like syntax highlighting for Zsh")
    (description
     "This package provides syntax highlighting for Zsh.  It enables
highlighting of commands whilst they are typed at a Zsh prompt into an
interactive terminal.  This helps in reviewing commands before running them,
particularly in catching syntax errors.")
    (license license:bsd-3)))

(define-public grml-zsh-config
  (package
    (name "grml-zsh-config")
    (version "0.19.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://deb.grml.org/pool/main/g/grml-etc-core/grml-etc-core_"
                    version ".tar.gz"))
              (sha256
               (base32
                "1k1m2fbmvw73qy9mc6k2ygjg6zz8h7nn3d9pvj6jbjadnx4pz770"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases
            %standard-phases
          (add-before 'install 'make-doc
            (lambda _ (with-directory-excursion "doc" (invoke "make")))))
      #:install-plan
      #~'(("etc/skel/.zshrc"  "etc/skel/.zshrc")
          ("etc/zsh/keephack" "etc/zsh/keephack")
          ("etc/zsh/zshrc"    "etc/zsh/zshrc")
          ("doc/grmlzshrc.5"  "share/man/man5/grmlzshrc.5"))))
    (native-inputs (list txt2tags))
    (home-page "https://grml.org/zsh/")
    (synopsis "Grml's zsh configuration")
    (description "This package provides an interactive setup for zsh
preconfigured by the Grml project.")
    (license license:gpl2)))

(define-public sh-z
  (package
    (name "sh-z")
    (version "1.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rupa/z")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13zbgkj6y0qhvn5jpkrqbd4jjxjr789k228iwma5hjfh1nx7ghyb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests provided
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1"))
                    (bin (string-append out "/bin")))
               (install-file "z.sh" bin)
               (chmod (string-append bin "/z.sh") #o755)
               (install-file "z.1" man)
               #t))))))
    (synopsis "Jump about directories")
    (description
     "Tracks your most used directories, based on ``frecency''.  After a short
learning phase, z will take you to the most ``frecent'' directory that matches
all of the regexes given on the command line in order.")
    (home-page "https://github.com/rupa/z")
    (license license:expat)))

(define-public shfmt
  (package
    (name "shfmt")
    (version "3.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mvdan/sh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qqrggliwicmrqzwv9ivg7w1chy1b97w8p7ifpvqfsbal0qcr1xi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "mvdan.cc/sh/v3/cmd/shfmt"
      #:unpack-path "mvdan.cc/sh/v3"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda* (#:key unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (let ((fixed-version (string-append #$version " (GNU Guix)")))
                  ;; shfmt uses modules to look up the version at runtime;
                  ;; since our build system does not yet support modules,
                  ;; inject the version string here instead
                  (substitute* "cmd/shfmt/main.go"
                    (("version = \"\\(devel\\)\"")
                     (format #f "version = \"~a\"" fixed-version)))
                  (substitute* "cmd/shfmt/testdata/script/flags.txtar"
                    (("devel\\|v3") #$version))))))
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (native-inputs
     (list go-github-com-creack-pty
           go-github-com-go-quicktest-qt
           go-github-com-google-go-cmp
           go-github-com-google-renameio-v2
           go-github-com-muesli-cancelreader
           go-github-com-rogpeppe-go-internal
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-term
           go-mvdan-cc-editorconfig))
    (home-page "https://github.com/mvdan/sh")
    (synopsis "Shell formatter with bash support")
    (description
     "This package provides a shell formatter.  Supports
@url{https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html,POSIX
Shell}, @url{https://www.gnu.org/software/bash/,Bash}, and
@url{http://www.mirbsd.org/mksh.htm,mksh}.")
    (license license:bsd-3)))

(define-public starship
  (package
    (name "starship")
    (version "1.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "starship" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ikdy6jwlc36add55acxlba6f009dln9iyz368c1ndbfpgn4n42g"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-completions
            (lambda* (#:key native-inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (starship-bin
                       (if #$(%current-target-system)
                           (search-input-file native-inputs "/bin/starship")
                           (string-append out "/bin/starship")))
                     (share (string-append out "/share"))
                     (bash-completion-dir
                       (string-append out "/etc/bash-completion.d/"))
                     (zsh-completion-dir
                       (string-append share "/zsh/site-functions/"))
                     (fish-completion-dir
                       (string-append share "/fish/vendor_completions.d/"))
                     (elvish-completion-dir
                       (string-append share "/elvish/lib")))
                    ;; Make the directories
                  (mkdir-p bash-completion-dir)
                  (mkdir-p zsh-completion-dir)
                  (mkdir-p fish-completion-dir)
                  (mkdir-p elvish-completion-dir)
                  ;; Use the built starship to generate the completions.
                  (with-output-to-file
                    (string-append bash-completion-dir "/starship")
                    (lambda _ (invoke starship-bin "completions" "bash")))
                  (with-output-to-file
                    (string-append zsh-completion-dir "/_starship")
                    (lambda _(invoke starship-bin "completions" "zsh")))
                  (with-output-to-file
                    (string-append fish-completion-dir "/starship.fish")
                    (lambda _ (invoke starship-bin "completions" "fish")))
                  (with-output-to-file
                    (string-append elvish-completion-dir "/starship")
                    (lambda _ (invoke starship-bin "completions" "elvish"))))))
          ;; Some tests require a writable home directory
          (add-after 'unpack 'patch-test-shell
            (lambda* (#:key inputs #:allow-other-keys)
              ;; search through the rust files and then replace `/bin/sh'
              ;; with the path to the `/bin/sh' in the drv inputs
              (let ((rust-files (find-files "." "\\.rs$")))
                (for-each (lambda (file)
                            (substitute* file
                              (("/bin/sh")
                               (search-input-file inputs "/bin/sh"))))
                          rust-files))))
          ;; Set "HOME" to be located inside the cwd so it is writable
          ;; for tests checking for user-configs
          (add-before 'check 'set-test-env-vars
            (lambda _
              (setenv "HOME"
                      (string-append (getcwd) "/.test-home")))))
      #:cargo-inputs `(("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-complete" ,rust-clap-complete-4)
                       ("rust-deelevate" ,rust-deelevate-0.2)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gix" ,rust-gix-0.66)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-guess-host-triple" ,rust-guess-host-triple-0.1)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-notify-rust" ,rust-notify-rust-4)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-open" ,rust-open-5)
                       ("rust-os-info" ,rust-os-info-3)
                       ("rust-path-slash" ,rust-path-slash-0.2)
                       ("rust-pest" ,rust-pest-2)
                       ("rust-pest-derive" ,rust-pest-derive-2)
                       ("rust-process-control" ,rust-process-control-5)
                       ("rust-quick-xml" ,rust-quick-xml-0.36)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rust-ini" ,rust-rust-ini-0.21)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-shadow-rs" ,rust-shadow-rs-0.35)
                       ("rust-shell-words" ,rust-shell-words-1)
                       ("rust-starship-battery" ,rust-starship-battery-0.10)
                       ("rust-strsim" ,rust-strsim-0.11)
                       ("rust-systemstat" ,rust-systemstat-0.2)
                       ("rust-terminal-size" ,rust-terminal-size-0.4)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-toml-edit" ,rust-toml-edit-0.22)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.2)
                       ("rust-urlencoding" ,rust-urlencoding-2)
                       ("rust-versions" ,rust-versions-6)
                       ("rust-which" ,rust-which-6)
                       ("rust-whoami" ,rust-whoami-1)
                       ("rust-windows" ,rust-windows-0.58)
                       ("rust-winres" ,rust-winres-0.1)
                       ("rust-yaml-rust2" ,rust-yaml-rust2-0.9))
      #:cargo-development-inputs `(("rust-mockall" ,rust-mockall-0.13)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (inputs (list cmake-minimal))
    (native-inputs
     (append
       (if (%current-target-system)
           (list this-package)
           '())
       (list git-minimal)))
    (home-page "https://starship.rs")
    (synopsis
     "The minimal, blazing-fast, and infinitely customizable prompt for any shell!")
    (description
     "This package provides The minimal, blazing-fast, and infinitely customizable
prompt for any shell!

@itemize
@item Fast: it's fast - *really really* fast :rocket:
@item Customizable: configure every aspect of your prompt
@item Universal: works on any shell, on any operating system
@item Intelligent: shows relevant information at a glance
@item Feature rich: support for all your favorite tools
@item Easy: quick to install - start using it in minutes
@end itemize

Note: users must have a nerd font installed and enabled in their terminal")
    (license license:isc)))

(define-public envstore
  (package
    (name "envstore")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://finalrewind.org/projects/"
                           name "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "1x97lxad80m5blhdfanl5v2qzjwcgbij2i23701bn8mpyxsrqszi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://finalrewind.org/projects/envstore/")
    (synopsis "Save and restore environment variables")
    (description "Envstore is a program for sharing environment variables
between various shells or commands.")
    (license license:wtfpl2)))

(define-public trash-cli
  (package
    (name "trash-cli")
    (version "0.24.5.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andreafrancia/trash-cli")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mqs3y9vbph33jsaa5hc0fhk80pklmsn8ylp979k9qj63fgqrnwn"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'fix-setup.py
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((bin (string-append #$output "/bin")))
                         (mkdir-p bin)
                         (substitute* "setup.py"
                           (("add_script\\('")
                            (string-append "add_script('" bin "/"))))))
                   ;; Whenever setup.py is invoked, scripts in out/bin/ are
                   ;; replaced. Thus we cannot invoke setup.py for testing.
                   ;; Upstream also uses pytest.
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         (invoke "pytest")))))))
    (native-inputs (list python-flexmock
                         python-mock
                         python-parameterized
                         python-pytest
                         python-shtab
                         python-six))
    (inputs (list coreutils))
    (propagated-inputs (list python-psutil))
    (home-page "https://github.com/andreafrancia/trash-cli")
    (synopsis "Trash can management tool")
    (description
     "trash-cli is a command line utility for interacting with the
FreeDesktop.org trash can used by GNOME, KDE, XFCE, and other common desktop
environments.  It can move files to the trash, and remove or list files that
are already there.")
    (license license:gpl2+)))

(define-public tran
  ;; There is no new release yet, but there are some changes in master brunch,
  ;; see <https://github.com/kilobyte/tran/issues/4>.
  (let ((commit "039df9529d5dfb8283edfb3c8b3cc16c01f0bfce")
        (revision "0"))
    (package
      (name "tran")
      ;; The latest upstream version seems to be "v5".
      (version (git-version "5.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kilobyte/tran")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32 "1kzr3lfhi5f8wpwjzrzlwkxjv9rasdr9ndjdns9kd16vsh0gl2rd"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ;no tests
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure) ;no configure provided
            (add-after 'unpack 'patch
              (lambda _
                (substitute* "tran"
                  (("my \\$DATA=\"data\"")
                   (format #f "my $DATA=\"~a/share/tran/data\"" #$output)))))
            (replace 'build
              (lambda _
                (invoke "make")))
            (delete 'strip)
            (replace 'install
              (lambda _
                (install-file "tran" (string-append #$output "/bin/"))
                (install-file "tran.1" (string-append
                                        #$output "/share/man/man1/"))
                (copy-recursively "data" (string-append
                                          #$output "/share/tran/data/")))))))
      (inputs (list perl))
      (home-page "https://github.com/kilobyte/tran")
      (synopsis "Transcription between character scripts")
      (description
       "This tool can transliterate/transcribe text both ways between the
Latin script and other languages.")
      (license license:expat))))

(define-public direnv
  (package
    (name "direnv")
    (version "2.35.0")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/direnv/direnv")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0l9pziv5nxlq6dxbsqrfl6z4ibq171wfx6wmjs392cdn5w2n908b"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/direnv/direnv"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-manpages
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1")))
               (mkdir-p man)
               (with-directory-excursion "src/github.com/direnv/direnv"
                 (install-file "man/direnv.1" man)
                 (install-file "man/direnv-stdlib.1" man)
                 (install-file "man/direnv.toml.1" man)))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" "/tmp")
               (with-directory-excursion "src/github.com/direnv/direnv"
                 ;; The following file needs to be writable so it can be
                 ;; modified by the testsuite.
                 (make-file-writable "test/scenarios/base/.envrc")
                 ;; We need to manually run test because make test
                 ;; tries to use go modules
                 (invoke "go" "test" "./...")
                 ;; Clean up from the tests, especially so that the extra
                 ;; direnv executable that's generated is removed.
                 (invoke "make" "clean"))))))))
    (native-inputs
     (list go-github-com-burntsushi-toml
           go-github-com-mattn-go-isatty
           go-golang-org-x-mod
           which))
    (home-page "https://direnv.net/")
    (synopsis "Environment switcher for the shell")
    (description
     "direnv can hook into the bash, zsh, tcsh, and fish shells to load
or unload environment variables depending on the current directory.  This
allows project-specific environment variables without using @file{~/.profile}.

Before each prompt, direnv checks for the existence of a @file{.envrc} file in
the current and parent directories.  This file is then used to alter the
environment variables of the current shell.")
    (license license:expat)))

(define-public fzy
  (package
    (name "fzy")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhawthorn/fzy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gkzdvj73f71388jvym47075l9zw61v6l8wdv2lnc0mns6dxig0k"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))
    (home-page "https://github.com/jhawthorn/fzy")
    (synopsis "Fast fuzzy text selector for the terminal with an advanced
scoring algorithm")
    (description
     "Most other fuzzy matchers sort based on the length of a match.  fzy tries
to find the result the user intended.  It does this by favouring matches on
consecutive letters and starts of words.  This allows matching using acronyms
or different parts of the path.

fzy is designed to be used both as an editor plugin and on the command
line.  Rather than clearing the screen, fzy displays its interface directly
below the current cursor position, scrolling the screen if necessary.")
    (license license:expat)))

(define-public hstr
  (package
    (name "hstr")
    (version "3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dvorka/hstr")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w1xr0ddf34i46b8wwz2snf7ap6m0mv53zid3d1l5hc4m3az5qis"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'adjust-ncurses-includes
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "src/include/hstr_curses.h"
                 (("ncursesw\\/curses.h") "ncurses.h"))
               (substitute* "src/include/hstr.h"
                 (("ncursesw\\/curses.h") "ncurses.h")))
             #t)))))
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list ncurses readline))
    (synopsis "Navigate and search command history with shell history suggest box")
    (description "HSTR (HiSToRy) is a command-line utility that brings
improved Bash and Zsh command completion from the history.  It aims to make
completion easier and more efficient than with @kbd{Ctrl-R}.  It allows you to
easily view, navigate, and search your command history with suggestion boxes.
HSTR can also manage your command history (for instance you can remove
commands that are obsolete or contain a piece of sensitive information) or
bookmark your favourite commands.")
    (home-page "https://me.mindforger.com/projects/hh.html")
    (license license:asl2.0)))

(define-public shell-functools
  (package
    (name "shell-functools")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sharkdp/shell-functools")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d6zzg7cxfrzwzh1wmpj7q85kz33sak6ac59ncsm6dlbin12h0hi"))))
    (build-system python-build-system)
    (home-page "https://github.com/sharkdp/shell-functools/")
    (synopsis "Functional programming tools for the shell")
    (description "This package provides higher order functions like map,
filter, foldl, sort_by and take_while as simple command-line tools.  Following
the UNIX philosophy, these commands are designed to be composed via pipes.  A
large collection of functions such as basename, replace, contains or is_dir
are provided as arguments to these commands.")
    (license license:expat)))

(define-public rig
  (package
    (name "rig")
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/rig/rig/"
                                  version "/rig-"
                                  version ".tar.gz"))
              (sha256
                (base32
                  "1f3snysjqqlpk2kgvm5p2icrj4lsdymccmn3igkc2f60smqckgq0"))))
    (build-system gnu-build-system)
    (arguments `(#:make-flags
                 (list (string-append "CXX=" ,(cxx-for-target))
                       (string-append "PREFIX=" %output))
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure)
                   (add-after 'unpack 'fix-build
                     (lambda _
                       (substitute* "rig.cc"
                         (("^#include <string>")
                          "#include <cstring>"))
                       (substitute* "Makefile"
                         (("g\\+\\+")
                          "${CXX} -O2")
                         (("install -g 0 -m 755 -o 0 -s rig \\$\\(BINDIR\\)")
                          "install -m 755 -d $(DESTDIR)$(BINDIR)\n\t\
install -m 755 rig $(DESTDIR)$(BINDIR)/rig")
                         (("install -g 0 -m 644 -o 0 rig.6 \\$\\(MANDIR\\)/man6/rig.6")
                          "install -m 755 -d $(DESTDIR)$(MANDIR)/man6/\n\t\
install -m 644 rig.6 $(DESTDIR)$(MANDIR)/man6/rig.6")
                         (("install -g 0 -m 755 -o 0 -d \\$\\(DATADIR\\)")
                          "install -m 755 -d $(DESTDIR)$(DATADIR)")
                         (("install -g 0 -m 644 -o 0 data/\\*.idx \\$\\(DATADIR\\)")
                          "install -m 644 data/*.idx $(DESTDIR)$(DATADIR)")))))
                 #:tests? #f))
    (home-page "https://rig.sourceforge.net")
    (synopsis "Random identity generator")
    (description
      "RIG (Random Identity Generator) generates random, yet real-looking,
personal data.  It is useful if you need to feed a name to a Web site, BBS, or
real person, and are too lazy to think of one yourself.  Also, if the Web
site/BBS/person you are giving the information to tries to cross-check the
city, state, zip, or area code, it will check out.")
    (license license:gpl2+)))

(define-public conflict
  (package
    (name "conflict")
    (version "20221002")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://invisible-mirror.net/archives/conflict/conflict-"
                    version ".tgz"))
              (sha256
               (base32
                "1z6z61yiss9m45m3agqs92l569r55w9nsqaap56kh568mcy3y64c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda _
             (substitute* "run_test.sh"
               (("PATH=\".:\\$BIN:/bin\"")
                "PATH=\".:$BIN:$PATH\"")))))))
    (home-page "https://invisible-island.net/conflict/conflict.html")
    (synopsis "Displays conflicting filenames in your execution path")
    (description
     "@code{conflict} examines the user-specifiable list of programs, looking
for instances in the user's path which conflict (i.e., the name appears in
more than one point in the path).")
    (license (license:x11-style "file://COPYING"))))

(define-public renameutils
  (package
    (name "renameutils")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/renameutils/"
                           "renameutils-" version ".tar.gz"))
       (sha256
        (base32
         "18xlkr56jdyajjihcmfqlyyanzyiqqlzbhrm6695mkvw081g1lnb"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (substitute* "src/Makefile.in"
                     (("\\(\\$bindir\\)") "$(bindir)"))
                   #t))))
    (build-system gnu-build-system)
    (inputs
     (list readline))
    (home-page "https://www.nongnu.org/renameutils/")
    (synopsis "File renaming utilities")
    (description "The file renaming utilities (renameutils for short) are a
set of programs designed to make renaming of files faster and less cumbersome.
The file renaming utilities consists of five programs: @command{qmv},
@command{qcp}, @command{imv}, @command{icp}, and @command{deurlname}.")
    (license license:gpl3+)))

(define-public grc
  (package
    (name "grc")
    (version "1.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/garabik/grc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h0h88h484a9796hai0wasi1xmjxxhpyxgixn6fgdyc5h69gv8nl"))))
    (build-system gnu-build-system)
    (inputs
     (list python))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (substitute* "grc"
                (("conffilenames = \\[.*\\]")
                 (string-append
                  "conffilenames = ["
                  "os.environ.get('GUIX_ENVIRONMENT', '" #$output "') "
                  "+ '/etc/grc.conf']")))
              (substitute* "grcat"
                (("conffilepath \\+= \\['/usr/.*\\]")
                 (string-append
                  "conffilepath += ["
                  "os.environ.get('GUIX_ENVIRONMENT', '" #$output "') "
                  "+ '/share/grc/']"))))) ;; trailing slash!
          (delete 'check)
          (replace 'install
            (lambda _
              (invoke "sh" "install.sh" #$output #$output))))))
    (home-page "http://kassiopeia.juls.savba.sk/~garabik/software/grc.html")
    (synopsis "Generic colouriser for everything")
    (description "@code{grc} can be used to colourise logfiles, output of
shell commands, arbitrary text, etc.  Many shell commands are supported out of
the box.

You might want to add these lines you your @code{~/.bashrc}:
@example
GRC_ALIASES=true
source ${GUIX_ENVIRONMENT:-$HOME/.guix-profile}/etc/profile.d/grc.sh
@end example
")
    (license license:gpl2)))

(define-public liquidprompt
  (package
    (name "liquidprompt")
    (version "2.1.2")
    (home-page "https://github.com/liquidprompt/liquidprompt")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/liquidprompt/liquidprompt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ljlq97mh84d6g6r3abb254vrwrdan5v74b69fpd62d7p9ffnsgf"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("liquidpromptrc-dist" "etc/liquidpromptrc")
                         ("example.bashrc" "share/liquidprompt/examples/")
                         ("liquid.ps1" "share/liquidprompt/examples/")
                         ("liquidprompt" "share/liquidprompt/")
                         ("contrib" "share/liquidprompt/")
                         ("themes" "share/liquidprompt/")
                         ("liquidprompt.plugin.zsh"
                          "share/zsh/plugins/liquidprompt/")
                         ("docs" #$(string-append "share/doc/" name "-"
                                                  version "/")))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'fix-plugin
                     (lambda _
                       (substitute* "liquidprompt.plugin.zsh"
                         (("source(.*)$")
                          (string-append "source "
                                         #$output
                                         "/share/liquidprompt/liquidprompt")))))
                   (add-after 'fix-plugin 'fix-utils-path
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "liquidprompt"
                         (("([ (])\\\\?(tput|hostname|cksum|uname|tty|grep)([) ])"
                           all beginning command ending)
                          (string-append beginning
                                         (search-input-file
                                          inputs
                                          (string-append "bin/" command))
                                         ending))))))))
    (inputs (list ncurses
                  coreutils
                  inetutils))
    (synopsis "Full-featured prompt for Bash & Zsh")
    (description
     "Liquidprompt is an adaptive prompt for Bash and Zsh that gives
you a nicely displayed prompt with useful information when you need it. It
does this with a powerful theming engine and a large array of data sources.

In order to use liquidprompt with Zsh, you should use the following snippet
with Guix Home:

@example
(service home-zsh-service-type
         (home-zsh-configuration
           (zshrc (list ;;...
                    ;; This loads liquidprompt
                    (mixed-text-file \"liquidprompt\"
                                     \"[[ $- = *i* ]] && source \" liquidprompt \"/share/liquidprompt/liquidprompt\")
                    ;; This loads the powerline theme available in liquidprompt
                    (mixed-text-file \"powerline-theme\"
                                     \"source \" liquidprompt \"/share/liquidprompt/themes/powerline/powerline.theme\"))))))
@end example\n")
    (license license:agpl3+)))

(define-public fzf-tab
  (package
  (name "fzf-tab")
  (version "1.1.2")
  (home-page "https://github.com/Aloxaf/fzf-tab")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Aloxaf/fzf-tab")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "061jjpgghn8d5q2m2cd2qdjwbz38qrcarldj16xvxbid4c137zs2"))))
  (build-system copy-build-system)
  (arguments
   '(#:install-plan '(("lib" "/share/zsh/plugins/fzf-tab/")
                      ("modules" "/share/zsh/plugins/fzf-tab/")
                      ("fzf-tab.plugin.zsh" "/share/zsh/plugins/fzf-tab/")
                      ("fzf-tab.zsh" "/share/zsh/plugins/fzf-tab/")
                      ("README.md" "/share/doc/fzf-tab/"))))
  (synopsis "Replace the zsh default completion menu with fzf")
  (description
   "The fzf-tab package replaces the default completion menu of the zsh
shell with fzf, enabling fuzzy finding and multi-selection.")
  (license license:expat)))
