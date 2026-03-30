;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Hamzeh Nasajpour <h.nasajpour@pantherx.org>
;;; Copyright © 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 LibreMiami <packaging-guix@libremiami.org>
;;; Copyright © 2021 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021 Sebastian Gibb <mail@sebastiangibb.de>
;;; Copyright © 2022 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Pavel Shlyak <p.shlyak@pantherx.org>
;;; Copyright © 2022 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2025 Matthias Riße <matrss@0px.xyz>
;;; Copyright © 2025 jgart <jgart@dismail.de>
;;; Copyright © 2026 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages task-management)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-vcs)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt))

(define-public beads
  (package
    (name "beads")
    (version "0.17.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/steveyegge/beads")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0adg00mqgl70fxynciswkzka5hyia86h92b1pnqd8achk6c5szbr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/steveyegge/beads/cmd/bd"
      #:unpack-path "github.com/steveyegge/beads"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list git-minimal/pinned
           go-github-com-anthropics-anthropic-sdk-go
           go-github-com-fatih-color
           go-github-com-spf13-cobra
           go-github-com-spf13-viper
           go-gopkg-in-natefinch-lumberjack-v2
           go-modernc-org-sqlite
           go-rsc-io-script))
    (home-page "https://github.com/steveyegge/beads")
    (synopsis "Graph-based issue tracker for AI coding agents")
    (description
     "@command{bd} (Beads) is a lightweight memory system for coding
agents, using a graph-based issue tracker.  Four kinds of dependencies
work to chain issues together like beads, making them easy for agents
to follow for long distances and reliably perform complex task streams
in the right order.  It uses SQLite for fast local operations and
JSONL files stored in git for distributed synchronization across
machines.")
    (license license:expat)))

(define-public clikan
  (let ((commit "55ab29e68263c6fed2844aef96fbebacda3eba9b")
        (revision "1"))
    (package
      (name "clikan")
      (version
       (git-version "0.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kitplummer/clikan")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1nyx80z53xxlbhpb5k22jnv4jajxqhjm0gz7qb18w9pqqlrvkqd4"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'check 'configure-tests
              (lambda _
                (setenv "HOME" (getenv "TEMP")))))))
      (native-inputs
       (list python-pytest python-click python-setuptools))
      (inputs
       (list python-click
             python-click-default-group
             python-pyyaml
             python-rich))
      (home-page "https://github.com/kitplummer/clikan")
      (synopsis "Command-line kanban utility")
      (description
       "Clikan is a super simple command-line utility for tracking tasks
following the Japanese kanban (boarding) style.")
      (license license:expat))))

(define-public git-bug
  (package
    (name "git-bug")
    (version "0.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/git-bug/git-bug")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s3y8ll2942d6my2wz6bbipram4l6brbwwfvp2nr8cchzrb23dl8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/git-bug/git-bug"
      #:embed-files
      #~(list "prelude.graphql")
      #:test-flags
      #~(list "-skip" (string-join
                       (list
                        ;; These tests require network access.
                        "TestValidateProject"
                        "TestValidateUsername"
                        ;; This test is buggy. See
                        ;; https://github.com/git-bug/git-bug/issues/809
                        "TestGoGitRepo_Indexes")
                       "|"))))
    (native-inputs
     (list go-github-com-99designs-gqlgen
           go-github-com-99designs-keyring
           go-github-com-araddon-dateparse
           go-github-com-awesome-gocui-gocui
           go-github-com-blevesearch-bleve
           go-github-com-dustin-go-humanize
           go-github-com-fatih-color
           go-github-com-go-git-go-billy-v5
           go-github-com-go-git-go-git-v5
           go-github-com-gorilla-mux
           go-github-com-hashicorp-golang-lru-v2
           go-github-com-michaelmure-go-term-text
           go-github-com-phayes-freeport
           go-github-com-protonmail-go-crypto
           go-github-com-shurcool-githubv4
           go-github-com-skratchdot-open-golang
           go-github-com-spf13-cobra
           go-github-com-stretchr-testify
           go-github-com-vbauerster-mpb-v8
           go-github-com-vektah-gqlparser-v2
           ;; The build fails with the latest (v1.28.1)
           ;; go-gitlab-com-gitlab-org-api-client-go. See
           ;; https://github.com/git-bug/git-bug/issues/1514 So, we use the
           ;; version specified in go.mod.
           go-gitlab-com-gitlab-org-api-client-go-0.116
           go-golang-org-x-oauth2
           go-github-com-icrowley-fake
           ;; git is required only for tests.
           git-minimal/pinned))
    (home-page "https://github.com/git-bug/git-bug")
    (synopsis "Distributed, offline-first bug tracker embedded in git")
    (description
     "@code{git-bug} is a standalone, distributed, offline-first issue
management tool that embeds issues, comments, and more as objects in a git
repository.")
    (license license:gpl3+)))

(define-public annextimelog
  (package
    (name "annextimelog")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "annextimelog" version))
       (sha256
        (base32 "0mqv5w8m9g83wzlmmvrq4kaqm9sjcy3qsyp89lqm1xig2y1w82yp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "annextimelog/test.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'create-entrypoints 'wrap-program
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin")))
                (for-each (lambda (file)
                            (wrap-program file
                              `("PATH" ":" prefix
                                (,(dirname (which "git"))
                                 ,(dirname (which "git-annex"))))))
                          (list (string-append bin "/annextimelog")
                                (string-append bin "/atl")))))))))
    (native-inputs
     (list python-poetry-core
           python-pytest))
    (inputs
     (list bash-minimal
           git
           git-annex))
    (propagated-inputs
     (list python-rich
           python-tzdata))
    (home-page "https://gitlab.com/nobodyinperson/annextimelog")
    (synopsis "Git Annex-backed Time Tracking")
    (description
     "This package provides a functionality to track time spent on projects,
backed by Git Annex.")
    (license license:gpl3+)))

(define-public t-todo-manager
  ;; Last release is more than 10 years old.  Using latest commit.
  (let ((changeset "89ad444c000b")
        (revision "97"))
    (package
      (name "t-todo-manager")
      (version (git-version "1.2.0" revision changeset))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://hg.stevelosh.com/t")
               (changeset changeset)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32 "0c8zn7l0xq65wp07h7mxnb5ww56d1443l2vkjvx5sj6wpcchfn0s"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (if tests?
                    (invoke "cram" "tests")
                    (format #t "test suite not run.~%")))))))
      (native-inputs
       (list python-cram python-setuptools python-wheel))
      (home-page "https://github.com/sjl/t")
      (synopsis "Command-line todo list manager")
      (description
       "@command{t} is a command-line todo list manager for people that want
to finish tasks, not organize them.")
      (license license:expat))))

(define-public taskwarrior
  (package
    (name "taskwarrior")
    (version "3.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/GothenburgBitFactory/taskwarrior"
             "/releases/download/v" version
             "/task-" version ".tar.gz"))
       (sha256
        (base32 "05p70bfjimv87qxxxamwq18bs6n6d0mklqa5lnjy8s0jrlgpc0nk"))
       (patches (search-patches
                 "taskwarrior-link-taskchampion-cpp-with-sqlite3.patch"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "doc/ref")
                 (substitute* "doc/CMakeLists.txt"
                   ((".*task-ref.*") ""))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests implemented.
      #:imported-modules `(,@%cargo-build-system-modules
                           ,@%cmake-build-system-modules)
      #:modules '(((guix build cargo-build-system) #:prefix cargo:)
                  (guix build cmake-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-cargo-build-system
            (lambda args
              (for-each
                (lambda (phase)
                  (format #t "Running cargo phase: ~a~%" phase)
                  (apply (assoc-ref cargo:%standard-phases phase)
                         #:cargo-target #$(cargo-triplet) args))
                '(prepare-rust-crates
                  unpack-rust-crates
                  configure
                  check-for-pregenerated-files
                  patch-cargo-checksums)))))))
    (inputs
     (list gnutls
           ;; needs sqlite3_is_interrupted, not present on 3.39.3
           sqlite-next
           `(,util-linux "lib")))
    (native-inputs
     (cons* corrosion
            rust
            (cargo-inputs 'taskwarrior)))
    (home-page "https://taskwarrior.org")
    (synopsis "Command line task manager")
    (description
     "Taskwarrior is a command-line task manager following the Getting Things
Done time management method.  It supports network synchronization, filtering
and querying data, exposing task data in multiple formats to other tools.")
    (license license:expat)))

(define-public tasksh
  (package
    (name "tasksh")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://taskwarrior.org/download/tasksh-" version ".tar.gz"))
       (sha256 (base32
                "1z8zw8lld62fjafjvy248dncjk0i4fwygw0ahzjdvyyppx4zjhkf"))))
    (build-system cmake-build-system)
    (inputs
     (list readline))
    (arguments
     `(#:tests? #f ; No tests implemented.
       #:phases
       (modify-phases %standard-phases
         (delete 'install-license-files)))) ; Already installed by package
    (home-page "https://taskwarrior.org")
    (synopsis "Taskwarrior shell")
    (description
     "Tasksh is a shell for Taskwarrior, providing a more immersive
environment for list management. It has a review feature, shell command
execution, and libreadline support.")
    (license license:expat)))

(define-public timewarrior
  (package
    (name "timewarrior")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/GothenburgBitFactory/timewarrior/releases/download/v" version
             "/timew-" version ".tar.gz"))
       (patches (search-patches "timewarrior-time-sensitive-tests.patch"))
       (sha256 (base32
                "0lyaqzcg8np2fpsmih0hlkjxd3qbadc7khr24m1pq9lsdhq7xpy4"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-source-shebangs 'patch-cmake-shell
            (lambda _
              (substitute* "src/commands/CMakeLists.txt"
                (("/bin/sh") "sh"))))
          ;; Fix out of source building of manual pages
          (add-after 'patch-source-shebangs 'patch-man-cmake
            (lambda _
              (substitute* "doc/man1/CMakeLists.txt"
                (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}")
                 "${CMAKE_CURRENT_SOURCE_DIR}"))
              (substitute* "doc/man7/CMakeLists.txt"
                (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}")
                 "${CMAKE_CURRENT_SOURCE_DIR}"))))
          (replace 'check
            (lambda* (#:rest args)
              (apply (assoc-ref gnu:%standard-phases 'check)
                     #:test-target "test" args)))
          (add-after 'install 'install-completions
            (lambda _
              (let ((bash-completion-install-dir
                     (string-append #$output "/etc/bash_completion.d")))
                (mkdir-p bash-completion-install-dir)
                (copy-file
                 "../timew-1.4.3/completion/timew-completion.bash"
                 (string-append bash-completion-install-dir "/timew"))))))))
    (native-inputs
     (list ruby-asciidoctor/minimal))
    (inputs
     (list gnutls python `(,util-linux "lib")))
    (home-page "https://timewarrior.net")
    (synopsis "Command line utility to track and report time")
    (description
     "Timewarrior is a command line time tracking application, which allows
you to record time spent on activities.  You may be tracking your time for
curiosity, or because your work requires it.")
    (license license:expat)))

(define-public worklog
  (let ((commit "0f545ad6697ef4de7f68d92cd7cc5c6a4c60517b")
        (revision "1"))
    (package
      (name "worklog")
      (version (git-version "2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atsb/worklog")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "18dkmy168ks9gcnfqri1rfl0ag0dmh9d6ppfmjfcdd6g9gvi6zll"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         ,#~(list (string-append "CC=" #$(cc-for-target))
                  (string-append "BIN=" #$output "/bin")
                  (string-append "MAN=" #$output "/share/man"))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))
         #:tests? #f))  ; No "check" target.
      (inputs (list ncurses))
      (home-page "https://github.com/atsb/worklog")
      (synopsis "Program keeping track of time spent on different projects")
      (description
       "@code{worklog} is a program that helps you keep track of your time.
@code{worklog} is a simple ncurses based program that runs a clock and logs
time to a logfile.")
      (license license:public-domain))))

(define-public dstask
  (package
    (name "dstask")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/naggie/dstask")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d2mkzsiax6mi9ddicr5l6iydpcbxm4ahaw0mj3ab463w77x09gx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/naggie/dstask"
      #:build-flags
      #~(list (string-append
               "-ldflags=-X github.com/naggie/dstask.VERSION=" #$version
               " -X github.com/naggie/dstask.GIT_COMMIT=" #$version))
      #:test-subdirs #~(list "pkg/..." ".")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda arguments
              (let ((path-prefix "github.com/naggie/dstask/cmd/"))
                (for-each
                 (lambda (cmd)
                   (apply (assoc-ref %standard-phases 'build)
                          `(,@arguments #:import-path
                            ,(string-append path-prefix cmd))))
                 (list "dstask-import" "dstask")))))
          ;; TODO: Completions may be generated on the fly with "dstask
          ;; (bash|fish|zsh)-completion" but invocation of "dstask" requires
          ;; creating $HOME/.dstask git repository, find out workaround and
          ;; add fish completion as well.
          (add-after 'install 'install-shell-completions
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (let* ((out #$output)
                       (share (string-append out "/share"))
                       (zsh-completion
                        (string-append share "/zsh/site-functions/_dstask"))
                       (bash-completion
                        (string-append out "/bash-completion/completions/_dstask")))
                  (install-file ".dstask-bash-completions.sh" bash-completion)
                  (install-file ".dstask-zsh-completions.sh" zsh-completion))))))))
    (native-inputs
     (list go-github-com-burntsushi-toml
           go-github-com-gofrs-uuid
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-runewidth
           go-github-com-shurcool-githubv4
           go-github-com-sirupsen-logrus
           go-github-com-stretchr-testify
           go-golang-org-x-oauth2
           go-golang-org-x-sys
           go-gopkg-in-yaml-v2
           go-mvdan-cc-xurls-v2))
    (synopsis "CLI-based TODO manager with git-based sync + markdown notes per task")
    (description "dstask is a personal task tracker that uses git for
synchronization.  It offers a note command to attach a Markdown based note to
a task.")
    (home-page "https://github.com/naggie/dstask")
    (license license:expat)))

(define-deprecated/public-alias blanket
  (@ (gnu packages gnome-circle) blanket))

(define-public feathernotes
  (package
    (name "feathernotes")
    (version "0.10.0")
    (home-page "https://github.com/tsujan/FeatherNotes")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "122pbbxvi0mmhbz8m8far71vm72090r5cafss4hvrsjmq52a0y4k"))))
    (build-system qt-build-system)
    (arguments (list #:tests? #f))           ; no upstream tests
    (native-inputs (list pkg-config qttools-5))           ; for lrelease
    (inputs (list hunspell qtbase-5 qtsvg-5 qtwayland-5 qtx11extras))
    (synopsis "GUI hierarchical notes-manager")
    (description
     "FeatherNotes is a GUI hierarchical notes-manager for Linux.
It is independent of any desktop environment and has rich text formatting,
image embedding and inserting editable tables, spell checking, searchable
tags, drag and drop support, tray icon, node icons, hyperlinks, pdf and html
export, password protection and auto-saving.")
    (license license:gpl3+)))

(define-public pueue
  (package
    (name "pueue")
    (version "4.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nukesor/pueue")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zbplfab4glj6f9rip4immca5vabyd132c4k4gch3hrmq7w39x7b"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-install-paths ''("pueue")
      #:install-source? #f
      #:modules
      '((guix build cargo-build-system)
        (guix build utils)
        (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-completions
           (lambda* (#:key native-inputs #:allow-other-keys)
             (for-each
              (match-lambda
                ((shell . path)
                 (mkdir-p (in-vicinity #$output (dirname path)))
                 (let ((binary
                        (if #$(%current-target-system)
                            (search-input-file native-inputs "bin/pueue")
                            (in-vicinity #$output "bin/pueue"))))
                   (with-output-to-file (in-vicinity #$output path)
                     (lambda _
                       (invoke binary "completions" shell))))))
              '(("bash"    . "share/bash-completion/completions/pueue")
                ("elvish"  . "share/elvish/lib/pueue")
                ("fish"    . "share/fish/vendor_completions.d/pueue.fish")
                ("nushell" . "share/nushell/vendor/autoload/pueue")
                ("zsh"     . "share/zsh/site-functions/_pueue"))))))))
    (native-inputs
     (if (%current-target-system)
         (list this-package)
         '()))
    (inputs (cargo-inputs 'pueue))
    (home-page "https://github.com/Nukesor/pueue")
    (synopsis
     "Task management for sequential and parallel execution of long-running tasks")
    (description
     "Pueue is a tool that processes a queue of shell commands.  On top of that,
there are a lot of convenient features and abstractions like task groups, task
scheduling and background process execution.")
    (license (list license:asl2.0 license:expat))))

(define-public wtime
  (package
    (name "wtime")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/wtime/wtime/"
                           version "/wtime_"
                           (string-replace-substring version "." "_")
                           ".tar.gz"))
       (sha256
        (base32 "1rp1sxas9wjc84fvr6x94ryl3r9w7jd0x5j1hbi9q7yrgfclp830"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       ,#~(list (string-append "CC=" #$(cc-for-target))
                (string-append "PREFIX=" #$output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-man-path
           (lambda _
             (substitute* "Makefile"
               (("/man1") "/share/man/man1")))))
       #:tests? #f))  ; No "check" target.
    (home-page "https://wtime.sourceforge.net")
    (synopsis
     "Command-line utility for tracking time spent on arbitrary tasks")
    (description
     "@code{wtime} is a command-line utility for tracking time spent working
on arbitrary tasks.  All the time data is saved in files residing in the
@code{.wtimed} directory in the user's home directory.")
    (license license:x11)))

(define-public todoman
  (package
    (name "todoman")
    (version "4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "todoman" version))
       (sha256
        (base32 "1dlxmw919jvjxycf315vzs4f5q64gdjrp3988y8jkyivqywfwyqb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "--hypothesis-profile=ci"
                        "-vv" "tests" "-k"
                        (string-append
                         ;; Test expects wrong output string.
                         "not test_bad_start_date "
                         ;; Unknown failure
                         "and not test_default_command_args"))))))))
    (native-inputs
     (list python-freezegun
           python-hypothesis
           python-pytest
           python-pytest-cov
           python-pytest-runner
           python-pytz
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-atomicwrites
           python-click
           python-click-log
           python-dateutil
           python-humanize
           python-icalendar
           python-parsedatetime
           python-pyxdg
           python-urwid))
    (home-page "https://todoman.readthedocs.io/")
    (synopsis "CalDav-based todo manager")
    (description "Todoman is a simple, standards-based, cli todo (aka: task)
manager.  Todos are stored into icalendar files, which means you can sync
them via CalDAV using, for example, @code{vdirsyncer}.")
    (license license:isc)))

(define-public watson
  (package
    (name "watson")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tailordev/watson")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j0gqnxf0smjs0sy7ipryj1sk0s59wrh4qwr7h55zdr4wdhi407w"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-mock
           python-pytest
           python-pytest-datafiles
           python-pytest-mock
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-arrow
           python-click
           python-click-didyoumean
           python-colorama
           python-requests))
    (home-page "https://tailordev.github.io/Watson/")
    (synopsis "Command-line time tracker")
    (description
     "Watson is command-line interface to manage your time.  It supports
projects, tagging and reports.")
    (license license:expat)))

(define-public zk
  (package
    (name "zk")
    (version "0.15.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zk-org/zk/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g77sd8aw3mqcc4wqvrv5ir524i3x3qmpavqsl7f2hci7j4j2v2r"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove the sqlite database tests as they require a dependency on
        ;; github.com/go-testfixtures/testfixtures/v3 which itself requires
        ;; several new dependencies to be added to Guix.
        #~(begin
            (for-each delete-file
                      (find-files "internal/adapter/sqlite" ".*_test\\.go$"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/zk-org/zk"
      #:build-flags
      #~(list "-tags" "fts5"
              "-ldflags" (string-append "-X=main.Version=" #$version))
      #:test-flags
      #~(list "-vet=off" "-skip"
              ;; Test matches $HOME against the /etc/passwd entry.
              ;; Doesn't work on Guix because of HOME=/homeless-shelter.
              "TestExpandPath")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs import-path #:allow-other-keys)
              (substitute* (string-append "src/" import-path
                                          "/internal/adapter/fzf/fzf.go")
                (("\"fzf\"")
                 (format #f "~s" (search-input-file inputs "/bin/fzf")))))))))
    (native-inputs
     (list go-github-com-alecaivazis-survey-v2
           go-github-com-alecthomas-kong-for-zk
           go-github-com-aymerick-raymond
           go-github-com-bmatcuk-doublestar-v4
           go-github-com-fatih-color-for-zk
           go-github-com-go-testfixtures-testfixtures-v3
           go-github-com-google-go-cmp
           go-github-com-gosimple-slug
           go-github-com-kballard-go-shellquote
           go-github-com-lestrrat-go-strftime
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-sqlite3
           go-github-com-mvdan-xurls
           go-github-com-pelletier-go-toml
           go-github-com-pkg-errors
           go-github-com-relvacode-iso8601
           go-github-com-rvflash-elapsed
           go-github-com-schollz-progressbar-v3
           go-github-com-tj-go-naturaldate
           go-github-com-tliron-glsp
           go-github-com-tliron-kutil
           go-github-com-yuin-goldmark
           go-github-com-yuin-goldmark-meta
           go-github-com-zk-org-pretty
           go-gopkg-in-djherbis-times-v1))
    (inputs
     (list fzf))
    (home-page "https://zk-org.github.io/zk/")
    (synopsis "Plain text note-taking assistant")
    (description
     "Command-line tooling for helping you to maintain a plain text
Zettelkasten or personal wiki.  Based on the Markdown markup language.")
    (license license:gpl3)))
