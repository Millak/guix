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
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
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
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt))

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
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs outputs tests? #:allow-other-keys)
               (setenv "HOME" (getenv "TEMP"))
               (when tests?
                 (add-installed-pythonpath inputs outputs)
                 (invoke "pytest" "-vv")))))))
      (native-inputs
       `(("pytest" ,python-pytest)
         ("click" ,python-click)))
      (inputs
       `(("click" ,python-click)
         ("click-default-group" ,python-click-default-group)
         ("pyyaml" ,python-pyyaml)
         ("rich" ,python-rich)))
      (home-page "https://github.com/kitplummer/clikan")
      (synopsis "Command-line kanban utility")
      (description
       "Clikan is a super simple command-line utility for tracking tasks
following the Japanese kanban (boarding) style.")
      (license license:expat))))

(define-public annextimelog
  (package
    (name "annextimelog")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "annextimelog" version))
       (sha256
        (base32 "0m1q0pbjy7d4yvgkflg7208gmdrqn1cx346b4li0mlss1kr91hvz"))))
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
      (build-system python-build-system)
      (native-inputs
       (list python-cram))
      (synopsis "Command-line todo list manager")
      (description
       "@command{t} is a command-line todo list manager for people that want
to finish tasks, not organize them.")
      (home-page "https://stevelosh.com/projects/t/")
      (license license:expat))))

(define-public taskwarrior
  (package
    (name "taskwarrior")
    (version "2.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://taskwarrior.org/download/task-" version ".tar.gz"))
       (sha256 (base32
                "1v6gca4cfrlh7adjn95j3jg3qq81w3h68037803dc3yd03qaglxi"))))
    (build-system cmake-build-system)
    (inputs
     (list gnutls
           `(,util-linux "lib")))
    (arguments
     `(#:tests? #f ; No tests implemented.
       #:phases
       (modify-phases %standard-phases
         (delete 'install-license-files)))) ; Already installed by package
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
          (add-after 'install 'install-completions
            (lambda _
              (let ((bash-completion-install-dir
                     (string-append #$output "/etc/bash_completion.d")))
                (mkdir-p bash-completion-install-dir)
                (copy-file
                 "../timew-1.4.3/completion/timew-completion.bash"
                 (string-append bash-completion-install-dir "/timew"))))))))
    (native-inputs
     (list ruby-asciidoctor))
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
    (version "0.27")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/naggie/dstask")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01vdxm3y5fg4hqhq4k1lk0m7w70kkwlka5jhixv7a9lf1gqldskd"))))
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

(define-public blanket
  (package
    (name "blanket")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rafaelmardojai/blanket/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00i821zqfbigxmc709322r16z75qsw4rg23yhv35gza9sl65bzkg"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:tests? #f                   ;the "Validate appstream file" test fails
       #:phases
       (modify-phases %standard-phases
         (add-after 'wrap 'wrap-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out               (assoc-ref outputs "out"))
                    (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                    (gst-plugin-path   (getenv "GST_PLUGIN_SYSTEM_PATH"))
                    (python-path       (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/blanket")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                 `("GUIX_PYTHONPATH" ":" prefix (,python-path)))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           `(,gtk+ "bin")
           pkg-config))
    (inputs
     (list appstream-glib
           bash-minimal
           gsettings-desktop-schemas
           gst-plugins-bad
           gst-plugins-good             ;for ScaleTempo plugin
           gtk+
           libhandy
           python
           python-gst
           python-pygobject))
    (home-page "https://github.com/rafaelmardojai/blanket")
    (synopsis "Ambient sound and noise player")
    (description
     "Blanket provides different ambient sounds and types of noise to listen
to with the goal of improving your focus and enhancing your productivity.
You can also use it to fall asleep in a noisy environment.")
    (license license:gpl3+)))

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
    (inputs (list hunspell qtsvg-5 qtx11extras qtbase-5))
    (synopsis "GUI hierarchical notes-manager")
    (description
     "FeatherNotes is a GUI hierarchical notes-manager for Linux.
It is independent of any desktop environment and has rich text formatting,
image embedding and inserting editable tables, spell checking, searchable
tags, drag and drop support, tray icon, node icons, hyperlinks, pdf and html
export, password protection and auto-saving.")
    (license license:gpl3+)))

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
