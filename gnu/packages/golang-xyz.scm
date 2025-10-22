;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017-2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Pierre Neidhardt <ambrevar@gmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018, 2020 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019-2021 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2019-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020, 2021 raingloom <raingloom@riseup.net>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Guix Together <jgart@dismail.de>
;;; Copyright © 2021, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Ramana Radhakrishnan <ramana.radhakrishnan@arm.com>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021, 2023-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2022 (unmatched-parenthesis <paren@disroot.org>
;;; Copyright © 2022 Dhruvin Gandhi <contact@dhruvin.dev>
;;; Copyright © 2022 Dominic Martinez <dom@dominicm.dev>
;;; Copyright © 2022 JOULAUD François <Francois.JOULAUD@radiofrance.com>
;;; Copyright © 2022 Leo Nikkilä <hello@lnikki.la>
;;; Copyright © 2022 kiasoc5 <kiasoc5@disroot.org>
;;; Copyright © 2023 Benjamin <benjamin@uvy.fr>
;;; Copyright © 2023, 2024 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Fries <fries1234@protonmail.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2023 Miguel Ángel Moreno <mail@migalmoreno.com>
;;; Copyright © 2023 Nguyễn Gia Phong <mcsinyx@disroot.org>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Sergey Trofimov <sarg@sarg.org.ru>
;;; Copyright © 2023 Thomas Ieong <th.ieong@free.fr>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2023 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2023 conses <contact@conses.eu>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Brian Kubisiak <brian@kubisiak.com>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Jean Simard <woshilapin@tuziwo.info>
;;; Copyright © 2024 Jesse Eisses <jesse@eisses.email>
;;; Copyright © 2024 Luis Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2024 Roman Scherer <roman@burningswell.com>
;;; Copyright © 2024 Simen Endsjø <contact@simendsjo.me>
;;; Copyright © 2024 Spencer Peters <spencerpeters@protonmail.com>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2025 Ashvith Shetty <ashvithshetty0010@zohomail.in>
;;; Copyright © 2025 Jussi Timperi <jussi.timperi@iki.fi>
;;; Copyright © 2025 45mg <45mg.writes@gmail.com>
;;; Copyright © 2025 Daniel Ziltener <dziltener@lyrion.ch>
;;; Copyright © 2025 Formbi <formbi@protonmail.com>
;;; Copyright © 2025 David Thompson <davet@gnu.org>
;;; Copyright © 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2025 Arthur Rodrigues <arthurhdrodrigues@proton.me>
;;; Copyright © 2025 Tomás Ortín Fernández <quanrong@mailbox.org>
;;; Copyright © 2025 Allan Adair <allan@adair.no>
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

(define-module (gnu packages golang-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages specifications)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

;;; Commentary:
;;;
;;; Nomad Golang modules (libraries) are welcome here.
;;;
;;; There are two sections her: Libraries - for any source only Golang
;;; libraries which are meant to be imported by other packages; Executables -
;;; should contain inherited packages where commands need to be built.
;;;
;;; Please: Try to add new module packages in alphabetic order and avoid stand
;;; alone final executables which are not inherited from sources presented in
;;; this module.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-9fans-net-go
  ;; XXX: This variant is to keep go importer healthy with "--insert" option,
  ;; which places package in alphabetical order and names it accordingly to
  ;; import path from go.mod file.
  ;;
  ;; Use go-ninefans-net-go to include in inputs.
  (package
    (name "go-9fans-net-go")
    (version "0.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/9fans/go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cskaf3mk0l9xlz2fda5hwjc0kwcvdxwj4414znpvjzas73gr9mi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "9fans.net/go"
      #:test-flags
      #~(list "-vet=off")    ;Go@1.24 forces vet, but tests are not ready yet.
      #:test-subdirs #~(list "acme/..."
                             ;; "cmd/..." ; missing packages
                             ;;
                             ;; Tests fail with error: panic: drawfcall.New:
                             ;; exec: "devdraw": executable file not found in
                             ;; $PATH
                             ;;
                             ;; "draw/..."
                             "games/..."
                             "p9trace/..."
                             "plan9/..."
                             "plumb/...")))
    ;; TODO: Not ready packages required to build CLI from <cmd/devdraw>.
    ;; (native-inputs
    ;;  (list go-golang-org-x-exp-shiny
    ;;        go-golang-org-x-mobile))
    (propagated-inputs
     (list go-golang-org-x-exp
           go-golang-org-x-sys))
    (home-page "https://9fans.net/go")
    (synopsis "Interface for interacting with Acme windows")
    (description
     "The @code{acme} Go package provides simple interface for
interacting with Acme windows of the Plan 9 text editor.")
    (license license:expat)))

(define-public go-ariga-io-atlas
  (package
    (name "go-ariga-io-atlas")
    (version "0.36.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ariga/atlas")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x3rchcm3c0ixpgx8wvz71a9mbb4flqcsjrkqhj4aa24wik5p5h2"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - ariga.io/atlas/cmd/atlas
            ;; - ariga.io/atlas/internal/integration
            (delete-file-recursively "cmd")
            (delete-file-recursively "internal/integration")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "ariga.io/atlas"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Tests requiring atlas-cli in PATH.
                       (list "TestAtlasMigrate_Apply"
                             "TestAtlasMigrate_ApplyBroken"
                             "TestAtlasMigrate_ApplyWithRemote"
                             "TestAtlasMigrate_Lint"
                             "TestAtlasMigrate_LintWithLogin"
                             "TestAtlasMigrate_Push"
                             "TestAtlasSchema_Apply"
                             "TestAtlasSchema_Lint"
                             "TestDriver_LockAcquired"
                             "TestMaintainOriginalWorkingDir"
                             "TestMigrateHash"
                             "TestMigrateRebase"
                             "TestMigrate_Diff"
                             "TestMigrate_Status"
                             "TestNewClient"
                             "TestValidate")
                       "|"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Received unexpected error: open
          ;; /homeless-shelter/.cache/lock.lock: no such file or directory
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list go-github-com-data-dog-go-sqlmock
           go-github-com-mattn-go-sqlite3
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-bmatcuk-doublestar
           go-github-com-go-openapi-inflect
           go-github-com-hashicorp-hcl-v2
           go-github-com-zclconf-go-cty
           go-github-com-zclconf-go-cty-yaml
           go-golang-org-x-mod
           go-gopkg-in-yaml-v3))
    (home-page "https://ariga.io/atlas")
    (synopsis "Database schema as code")
    (description
     "Atlas is a language-agnostic tool for managing and migrating database
schemas using @code{DevOps} principles.")
    (license license:asl2.0)))

(define-public go-atomicgo-dev-cursor
  (package
    (name "go-atomicgo-dev-cursor")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atomicgo/cursor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ds85nyd3dnjr961x9g5kflx1qdb92vn7n6wc4jbk0fjjzbrnh5s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "atomicgo.dev/cursor"))
    (home-page "https://atomicgo.dev/cursor")
    (synopsis "Moving terminal cursor in Golang")
    (description
     "Package cursor contains cross-platform methods to move the terminal cursor in
different directions.  This package can be used to create interactive CLI tools
and games, live charts, algorithm visualizations and other updatable output of
any kind.")
    (license license:expat)))

(define-public go-atomicgo-dev-keyboard
  (package
    (name "go-atomicgo-dev-keyboard")
    (version "0.2.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atomicgo/keyboard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0axhs1ji87szirv91vvwy0l0h5f468pllp8zap2dpcy05krmi9jf"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Cycle: go-github-com-pterm-pterm -> go-github-com-marvinjwendt-testza
      ;; -> go-atomicgo-dev-keyboard -> go-github-com-pterm-pterm
      #:tests? #f
      #:import-path "atomicgo.dev/keyboard"))
    (propagated-inputs
     (list go-github-com-containerd-console))
    (home-page "https://atomicgo.dev/keyboard")
    (synopsis "Read keyboard events in CLI applications")
    (description
     "This package provides a functionality to read key presses from the keyboard,
while in a terminal application, which may be combined to check for ctrl+c,
alt+4, ctrl-shift, alt+ctrl+right, etc.  It can also be used to
simulate (mock) keypresses for CI testing.")
    (license license:expat)))

(define-public go-atomicgo-dev-schedule
  (package
    (name "go-atomicgo-dev-schedule")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atomicgo/schedule")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13zrmf9jagqjvjjckyqlvr889y2gxf22iz42l6j2zmgy9klbn6vl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "atomicgo.dev/schedule"))
    (home-page "https://atomicgo.dev/schedule")
    (synopsis "Easily schedule non-blocking tasks in Golang")
    (description
     "This package provides a simple scheduler which, can run a function at a
given time, in a given duration, or repeatedly at a given interval.")
    (license license:expat)))

(define-public go-bazil-org-fuse
  (package
    (name "go-bazil-org-fuse")
    (version "0.0.0-20200117225306-7b5117fecadc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bazil/fuse")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bw2lp1nijpqp729k808xkhwmb8nn7igsv51hvv9jw74q805qg2f"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests require root access to mount file system.
      #:tests? #f
      #:import-path "bazil.org/fuse"))
    (propagated-inputs
     (list go-github-com-tv42-httpunix go-golang-org-x-sys))
    (home-page "https://bazil.org/fuse")
    (synopsis "FUSE filesystems in Golang")
    (description
     "Package fuse enables writing FUSE file systems.  It is a from-scratch
implementation of the kernel-userspace communication protocol, and does not
use the C library from the project called FUSE.")
    (license (list license:bsd-2 license:bsd-3 license:hpnd))))

(define-public go-code-cloudfoundry-org-bytefmt
  (package
    (name "go-code-cloudfoundry-org-bytefmt")
    (version "0.0.0-20240329144308-0c372429d24b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudfoundry/bytefmt")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aqzbiy3idddyj39i7ydkjhnmpcbwr99g094kqiw72m9flrvrnxj"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "code.cloudfoundry.org/bytefmt"))
    (native-inputs
     (list go-github-com-onsi-gomega
           go-github-com-onsi-ginkgo-v2))
    (home-page "https://pkg.go.dev/code.cloudfoundry.org/bytefmt")
    (synopsis "Human readable byte formatter for Golang")
    (description
     "Package bytefmt contains helper methods and constants for converting to and from
a human-readable byte format.")
    (license license:asl2.0)))

(define-public go-codeberg-org-anaseto-gruid
  (package
    (name "go-codeberg-org-anaseto-gruid")
    (version "0.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/anaseto/gruid.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19751r5skzn43af1ccgk8km3b3kxzvqzvw0igxbirvw862g1il04"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "codeberg.org/anaseto/gruid"))
    (propagated-inputs
     (list go-golang-org-x-image))
    (home-page "https://codeberg.org/anaseto/gruid")
    (synopsis "Grid-based UI and game framework")
    (description
     "Package gruid provides a model for building grid-based applications.
The interface abstracts rendering and input for different platforms.  There
are drivers for terminal apps (gruid-tcell), native graphical apps (gruid-sdl)
and browser apps (gruid-js).")
    (license license:isc)))

(define-public go-codeberg-org-anaseto-gruid-js
  (package
    (name "go-codeberg-org-anaseto-gruid-js")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/anaseto/gruid-js.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m42d469djrix9gnhj6jzvq9kaj2av6ndzl9bf17iyagmqsp5d8x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "codeberg.org/anaseto/gruid-js"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: The final application needs to provide the same environment
          ;; variables before build to prevent issue like this: imports
          ;; syscall/js: build constraints exclude all Go files in
          ;; <...>/syscall/js.
          (add-before 'check 'pre-check
            (lambda _
              (setenv "GOOS" "js")
              (setenv "GOARCH" "wasm"))))))
    (propagated-inputs
     (list go-codeberg-org-anaseto-gruid))
    (home-page "https://codeberg.org/anaseto/gruid-js")
    (synopsis "Gruid Driver using syscall/js and wasm and HTML5 canvas")
    (description
     "Package js provides a Driver for making browser apps using wasm.  The
user needs to serve using an http server a directory containing the app.wasm
file along with an index.html file.")
    (license license:isc)))

(define-public go-codeberg-org-anaseto-gruid-tcell
  (package
    (name "go-codeberg-org-anaseto-gruid-tcell")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/anaseto/gruid-tcell.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zr5xlmnxva5n3c5fj5hxg1wcsw1pq4favfwwv5nclwgzbd0mjr5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "codeberg.org/anaseto/gruid-tcell"))
    (propagated-inputs
     (list go-codeberg-org-anaseto-gruid
           go-github-com-gdamore-tcell-v2))
    (home-page "https://codeberg.org/anaseto/gruid-tcell")
    (synopsis "Gruid Driver using the tcell library")
    (description
     "Package tcell provides a gruid Driver for making terminal apps.")
    (license license:isc)))

(define-public go-codeberg-org-emersion-go-scfg
  (package
    (name "go-codeberg-org-emersion-go-scfg")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/emersion/go-scfg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yw35rf6cxk5cwzf9y4qd0rlcryq1pxr9n34q6620i9djhgskwq2"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "codeberg.org/emersion/go-scfg"))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew))
    (home-page "https://codeberg.org/emersion/go-scfg")
    (synopsis "Go library for simple configuration file format")
    (description
     "Package go-scfg parses scfg files.")
    (license license:expat)))

(define-public go-dario-cat-mergo
  (package
    (name "go-dario-cat-mergo")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/darccio/mergo")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q61904rd858ac19vsmmhz69b1hvn0y9rjfb9d2gc4abg64dva57"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "dario.cat/mergo"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (native-inputs
     (list go-gopkg-in-yaml-v3))
    (home-page "https://github.com/darccio/mergo")
    (synopsis "Helper to merge structs and maps in Golang")
    (description
     "Helper to merge structs and maps in Golang.  Useful for configuration
default values, avoiding messy if-statements.

Mergo merges same-type structs and maps by setting default values in
zero-value fields.  Mergo won't merge unexported (private) fields.  It will do
recursively any exported one.  It also won't merge structs inside
maps (because they are not addressable using Go reflection).")
    (license license:bsd-3)))

(define-public go-git-sr-ht-emersion-go-sqlite3-fts5
  (package
    (name "go-git-sr-ht-emersion-go-sqlite3-fts5")
    (version "0.0.0-20240124102820-f3a72e8b79b1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~emersion/go-sqlite3-fts5")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1plbfb1z0y3gprddwvp4n61r0cacpp7cjn3abq00xhac5vdvig0v"))))
    (build-system go-build-system)
    ;; XXX: fts5.c, fts5.h, generate.sh, sqlite3.h and sqlite3ext.h are
    ;; obtained from
    ;; <https://www.sqlite.org/2023/sqlite-preprocessed-3440000.zip>, check if
    ;; they may be sourced from sqlite package.
    (arguments
     (list
      #:import-path "git.sr.ht/~emersion/go-sqlite3-fts5"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-flags
            (lambda _
              ;; FIXME: Find out why it's failing without these flags:
              ;; src/git.sr.ht/~emersion/go-sqlite3-fts5/internal/internal.go:13:
              ;; undefined reference to `sqlite3_auto_extension'collect2:
              ;; error: ld returned 1 exit status
              (setenv "CGO_LDFLAGS"
                      "-Wl,--unresolved-symbols=ignore-in-object-files"))))))
    (propagated-inputs
     (list go-github-com-mattn-go-sqlite3))
    (home-page "https://git.sr.ht/~emersion/go-sqlite3-fts5")
    (synopsis "FTS5 extension for go-sqlite3")
    (description
     "Standalone FTS5 extension for
@@url{https://github.com/mattn/go-sqlite3,go-sqlite3}, that provides full-text
search functionality to database applications.")
    (license license:expat)))

(define-public go-git-sr-ht-rjarry-go-opt
  (package
    (name "go-git-sr-ht-rjarry-go-opt")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~rjarry/go-opt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jcs3bn43g3wv4d5w59zazy139qfkn0903lnvndfn06s81gzqpch"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~rjarry/go-opt"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://git.sr.ht/~rjarry/go-opt")
    (synopsis "Argument parsing and completion based on struct tags")
    (description
     "@code{go-opt} is a library to parse command line arguments based on tag
annotations on struct fields.  It came as a spin-off from
@url{https://git.sr.ht/~rjarry/aerc,aerc} to deal with its internal
commands.")
    (license license:expat)))

(define-public go-git-sr-ht-rjarry-go-opt-v2
  (package
    (inherit go-git-sr-ht-rjarry-go-opt)
    (name "go-git-sr-ht-rjarry-go-opt-v2")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~rjarry/go-opt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1092926vcgkhp2yjpvlx7g7q60j0bbkkq6n4v1nv2mapxvrs7xjf"))))
    (arguments
     (list
      #:import-path "git.sr.ht/~rjarry/go-opt/v2"))))

(define-public go-git-sr-ht-rockorager-tcell-term
  (package
    (name "go-git-sr-ht-rockorager-tcell-term")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~rockorager/tcell-term")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z64yzr2l5j5r5rqi89jk4madn3ak8hw95lva5ra7gnlyhh2vs05"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~rockorager/tcell-term"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-creack-pty
           go-github-com-gdamore-tcell-v2
           go-github-com-mattn-go-runewidth))
    (home-page "https://git.sr.ht/~rockorager/tcell-term")
    (synopsis "Terminal widget for @code{tcell}")
    (description
     "This package provides a virtual terminal widget for the @code{tcell}
Go library.")
    (license license:expat)))

(define-public go-git-sr-ht-rockorager-vaxis
  (package
    (name "go-git-sr-ht-rockorager-vaxis")
    (version "0.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~rockorager/vaxis")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09n2vafkb4mfxq4fiwf7ir3557q0h91n0s7imhh0789fj19g9j30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~rockorager/vaxis"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-containerd-console
           go-github-com-creack-pty
           go-github-com-mattn-go-runewidth
           go-github-com-mattn-go-sixel
           go-github-com-rivo-uniseg
           go-golang-org-x-exp
           go-golang-org-x-image
           go-golang-org-x-sys))
    (home-page "https://git.sr.ht/~rockorager/vaxis")
    (synopsis "TUI library for Golang")
    (description
     "Package vaxis is a terminal user interface for modern terminals.  It
supports supports modern terminal features, such as styled underlines and
graphics.  A widgets package is provided with some useful widgets.")
    (license license:asl2.0)))

(define-public go-git-sr-ht-sircmpwn-getopt
  (package
    (name "go-git-sr-ht-sircmpwn-getopt")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~sircmpwn/getopt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f9rammnmhaz21qkmz7qf76r8jlzi323g05ps3j7gwrxlw7442a6"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "git.sr.ht/~sircmpwn/getopt"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://git.sr.ht/~sircmpwn/getopt")
    (synopsis "POSIX getopt for Go")
    (description
     "This package provides a POSIX-compatible implementation of
@code{getopt} for Go.")
    (license license:bsd-3)))

(define-public go-git-sr-ht-sircmpwn-go-bare
  (package
    (name "go-git-sr-ht-sircmpwn-go-bare")
    (version "0.0.0-20210406120253-ab86bc2846d9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~sircmpwn/go-bare")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zh36qppk8lscd8mysy0anm2vw5c74c10f4qvhd541wxm06di928"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~sircmpwn/go-bare"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-git-sr-ht-sircmpwn-getopt))
    (home-page "https://git.sr.ht/~sircmpwn/go-bare")
    (synopsis "Implementation of the BARE message format")
    (description
     "This package provides an implementation of the @acronym{BARE, Binary
Application Record Encoding} https://baremessages.org/ message format for
Golang.")
    (license license:asl2.0)))

(define-public go-github-com-a-h-parse
  (package
    (name "go-github-com-a-h-parse")
    (version "0.0.0-20250122154542-74294addb73e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a-h/parse")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1warii2f9mrp5m2da9pn4chzd4y3fjlc3547va8xljfh09bvray4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/a-h/parse"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (home-page "https://github.com/a-h/parse")
    (synopsis "Parsing tools for Golang")
    (description
     "This package provides a set of parsing tools for Go inspired by
@url{https://github.com/sprache/Sprache/, Sprache}.")
    (license license:expat)))

(define-public go-github-com-a8m-envsubst
  (package
    (name "go-github-com-a8m-envsubst")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/a8m/envsubst")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pkvza3dr3bs2r8y8gfbckijcpl4w3llxd7zy8hw45zznynb273q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/a8m/envsubst"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/a8m/envsubst")
    (synopsis "Environment variables substitution for Go")
    (description
     "This package provides a library for environment variables
substitution.")
    (license license:expat)))

(define-public go-github-com-abadojack-whatlanggo
  (package
    (name "go-github-com-abadojack-whatlanggo")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abadojack/whatlanggo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pidd5dqvcnqjjka12h0clj3mmq0j3bpanf9153schsx85xz7mzx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/abadojack/whatlanggo"))
    (home-page "https://github.com/abadojack/whatlanggo")
    (synopsis "Natural language detection library for Golang")
    (description
     "This package provides functionality for detecting natural languages and
scripts (writing systems).  Languages are represented by a defined list of
constants, while scripts are represented by RangeTable.")
    (license license:expat)))

(define-public go-github-com-aclements-go-perfevent
  (package
    (name "go-github-com-aclements-go-perfevent")
    (version "0.0.0-20240703205258-f34bb3e1a4e4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aclements/go-perfevent")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xlm7zi7k2ynla8z18n4zbz76n5f3iw5wz8axnn95jhdgzw07xr5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/aclements/go-perfevent"
      #:test-flags
      ;; Disable tests requiring root access and failing with error:
      ;; permission denied (consider: echo 0 | sudo tee
      ;; /proc/sys/kernel/perf_event_paranoid)
      #~(list "-skip" (string-join
                       (list "TestBasic"
                             "TestOpenGroup"
                             "TestOpenOne"
                             "TestResetRunning"
                             "TestResetStopped"
                             "TestStop"
                             "TestTotal")
                       "|"))))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/aclements/go-perfevent")
    (synopsis "Golang API for Linux's @code{perf_event_open}")
    (description
     "This package provides a simple Go API to Linux's @code{perf_event_open},
supporting event counters and a basic set of events.")
    (license license:bsd-3)))

(define-public go-github-com-adhocore-gronx
  (package
    (name "go-github-com-adhocore-gronx")
    (version "1.19.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/adhocore/gronx")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i1q3gy35h8gz0kr93419jnhfwsky0p40i1x8nfz4bpyfh2jxlvd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/adhocore/gronx"))
    (home-page "https://github.com/adhocore/gronx")
    (synopsis "Cron expression parser for Golang")
    (description
     "@code{gronx} is cron expression parser ported from
@url{https://github.com/adhocore/php-cron-expr, adhocore/cron-expr} with task
runner and daemon that supports crontab like task list file.  It may be used
programatically in Golang or as standalone binary instead of crond.")
    (license license:expat)))

(define-public go-github-com-adrg-frontmatter
  (package
    (name "go-github-com-adrg-frontmatter")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/adrg/frontmatter")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0slacbb6m6g9xg85qw3b170mimjahn9pryacm4iqk459s1qib6sq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/adrg/frontmatter"))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/adrg/frontmatter")
    (synopsis "Detecting and decoding various content front matter formats")
    (description
     "This package implements fuctionality of detecting and decoding various
content front matter formats e.g. @code{JSON}, @code{TOML} and @code{YAML}.")
    (license license:expat)))

(define-public go-github-com-adrg-strutil
  (package
    (name "go-github-com-adrg-strutil")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adrg/strutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xkjzjllv8b2m3lgn66cb09b0f5xqy2bk8ny3lkn4z0ywlchawj9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/adrg/strutil"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/adrg/strutil")
    (synopsis "Golang string utility functions")
    (description
     "Package strutil provides string metrics for calculating string
similarity as well as other string utility functions.")
    (license license:expat)))

(define-public go-github-com-adrg-xdg
  (package
    (name "go-github-com-adrg-xdg")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adrg/xdg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xbkb8wmr6phj2ppr75akc58jdzrv20gc3mkxa1mmb968isy8s6c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/adrg/xdg"
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests need HOME to be set: could not create any of the following
          ;; paths: /homeless-shelter/.local/data,
          ;; /homeless-shelter/.local/data, /usr/share
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/adrg/xdg")
    (synopsis "XDG specification implementation for Golang")
    (description
     "Package xdg provides an implementation of the @acronym{XDG, X Desktop
Group} Base Directory Specification.  The specification defines a set of
standard paths for storing application files including data and configuration
files.  For portability and flexibility reasons, applications should use the
XDG defined locations instead of hardcoding paths.  The package also includes
the locations of well known user directories.")
    (license license:expat)))

(define-public go-github-com-agext-levenshtein
  (package
    (name "go-github-com-agext-levenshtein")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/agext/levenshtein")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a26c8pp9h5w66bhd9vb6lpvmhp30mz46pnh3a8vrjx50givb2lw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/agext/levenshtein"))
    (home-page "https://github.com/agext/levenshtein")
    (synopsis "Calculating the Levenshtein distance between two strings in Go")
    (description
     "Package levenshtein implements distance and similarity metrics for
strings, based on the Levenshtein measure.")
    (license license:asl2.0)))

(define-public go-github-com-agnivade-levenshtein
  (package
    (name "go-github-com-agnivade-levenshtein")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/agnivade/levenshtein")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vg9aj9k4qv96nqqp261qrm9g7kj0axqhv3mm9qvw932l72943hn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/agnivade/levenshtein"))
    (native-inputs ; for tests only
     (list go-github-com-arbovm-levenshtein
           go-github-com-dgryski-trifles))
    (home-page "https://github.com/agnivade/levenshtein")
    (synopsis "Golang implementation to calculate Levenshtein Distance")
    (description
     "This package implements a functionality to calculate
@url{https://en.wikipedia.org/wiki/Levenshtein_distance, Levenshtein
Distance}.")
    (license license:expat)))

(define-public go-github-com-agonopol-go-stem
  (package
    (name "go-github-com-agonopol-go-stem")
    (version "0.0.0-20150630113328-985885018250")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/agonopol/go-stem")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "128qv5s2g13akbsclyi6kyvx52gx20wz81yxkd3qnlfh0f5fqjd0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/agonopol/go-stem"))
    (home-page "https://github.com/agonopol/go-stem")
    (synopsis "Word Stemming in Golang")
    (description
     "This package provides a implementation of the porter stemming algorithm:
http://tartarus.org/~martin/PorterStemmer/index.html.")
    (license license:expat)))

(define-public go-github-com-akamensky-argparse
  (package
    (name "go-github-com-akamensky-argparse")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/akamensky/argparse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m7rzrfwyrwxbbry5ppds2b3c5gdslpakvjhsh6i8mhdfhywd8wc"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Panic: unexpected call to os.Exit(0) during test.
      #:test-flags #~(list "-skip" "TestUsageString")
      #:import-path "github.com/akamensky/argparse"))
    (home-page "https://github.com/akamensky/argparse")
    (synopsis "Argparse for golang")
    (description
     "This package implements a flexible and configurable option for command
line arguments parsing.")
    (license license:expat)))

(define-public go-github-com-alecaivazis-survey-v2
  (package
    (name "go-github-com-alecaivazis-survey-v2")
    (version "2.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AlecAivazis/survey")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l3wqphqvm0qxv33pj9f1r72z5fln99vg735fcigv8k513m2aw9l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/AlecAivazis/survey/v2"))
    (native-inputs
     (list go-github-com-creack-pty
           go-github-com-hinshun-vt10x
           go-github-com-netflix-go-expect
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-kballard-go-shellquote
           go-github-com-mattn-go-isatty
           go-github-com-mgutz-ansi
           go-golang-org-x-term
           go-golang-org-x-text))
    (home-page "https://github.com/AlecAivazis/survey")
    (synopsis "Interactive and accessible terminal prompts for Go")
    (description
     "This package provides a library for building interactive and accessible
prompts on terminals supporting ANSI escape sequences.")
    (license license:expat)))

(define-public go-github-com-alecthomas-chroma
  (package
    (name "go-github-com-alecthomas-chroma")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/alecthomas/chroma")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hjzb61m5lzx95xss82wil9s8f9hbw1zb3jj73ljfwkq5lqk76zq"))
       (modules '((guix build utils)))
       ;; Delete git submodules and generated files by Hermit.
       (snippet '(delete-file-recursively "bin"))))
    (build-system go-build-system)
    ;; TODO: Build cmd/chroma and cmd/chromad commands.
    (arguments
     (list
      #:import-path "github.com/alecthomas/chroma"
      #:test-flags
      #~(list "-vet=off"  ;Go@1.24 forces vet, but tests are not ready yet.
              "-skip" "TestLexers")))
    (native-inputs
     (list go-github-com-alecthomas-kong
           go-github-com-alecthomas-kong-hcl
           go-github-com-dlclark-regexp2
           go-github-com-gorilla-csrf
           go-github-com-gorilla-handlers
           go-github-com-gorilla-mux
           go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty
           go-github-com-stretchr-testify))
    (home-page "https://github.com/alecthomas/chroma/")
    (synopsis "General purpose syntax highlighter in pure Go")
    (description
     "Chroma takes source code and other structured text and converts it into
syntax highlighted HTML, ANSI-coloured text, etc.")
    (license license:expat)))

(define-public go-github-com-alecthomas-chroma-v2
  (package
    (inherit go-github-com-alecthomas-chroma)
    (name "go-github-com-alecthomas-chroma-v2")
    (version "2.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/chroma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05w4hnfcxqdlsz7mkc0m3jbp1aj67wzyhq5jh8ldfgnyjnlafia3"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/alecthomas/chroma/v2/cmd/chroma
            ;; - github.com/alecthomas/chroma/v2/cmd/chromad
            (delete-file-recursively "cmd")))))
    (arguments
     (list
      #:import-path "github.com/alecthomas/chroma/v2"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2
           go-github-com-alecthomas-repr))
    (propagated-inputs
     (list go-github-com-dlclark-regexp2))))

(define-public go-github-com-alecthomas-colour
  (package
    (name "go-github-com-alecthomas-colour")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/colour")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10zbm12j40ppia4b5ql2blmsps5jhh5d7ffphxx843qk7wlbqnjb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/colour"))
    (native-inputs
     (list go-github-com-mattn-go-isatty))
    (home-page "https://github.com/alecthomas/colour/")
    (synopsis "Colour terminal text for Go")
    (description
     "Package colour provides Quake-style colour formatting for Unix
terminals.  The package level functions can be used to write to stdout (or
strings or other files).  If stdout is not a terminal, colour formatting will
be stripped.")
    (license license:expat)))

(define-public go-github-com-alecthomas-kingpin-v2
  (package
    (name "go-github-com-alecthomas-kingpin-v2")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/kingpin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12xl62xzwq2h71hp1i0133403zhyqwsh95sr870fx18wmpqh8shf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/kingpin/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-alecthomas-units
           go-github-com-xhit-go-str2duration-v2))
    (home-page "https://github.com/alecthomas/kingpin")
    (synopsis "Go library provides utilities for building command line interfaces")
    (description
     "Go library provides utilities for building command line interfaces.")
    (license license:expat)))

(define-public go-github-com-alecthomas-kong
  (package
    (name "go-github-com-alecthomas-kong")
    (version "1.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/kong")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rx1vab7azz9f8xm044dlvx24dqggh25fjq1sxx4mizgvr28yjgq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/kong"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2))
    (propagated-inputs
     (list go-github-com-alecthomas-repr))
    (home-page "https://github.com/alecthomas/kong")
    (synopsis "Command-line parser for Golang")
    (description
     "Package kong aims to support arbitrarily complex command-line structures
with as little developer effort as possible.")
    (license license:expat)))

(define-public go-github-com-alecthomas-kong-hcl
  (package
    (name "go-github-com-alecthomas-kong-hcl")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/kong-hcl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q383705kavn23ay4vzr662x9lsl2xc1mv5irhcy0cazjjc7jzp2"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/alecthomas/kong-hcl/v2
            (delete-file-recursively "v2")))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags #~(list "-skip" "TestHCL/FromResolver")
      #:import-path "github.com/alecthomas/kong-hcl"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-alecthomas-kong
           go-github-com-hashicorp-hcl
           go-github-com-pkg-errors))
    (home-page "https://github.com/alecthomas/kong-hcl")
    (synopsis "Kong configuration loader for HCL")
    (description
     "This package implements functionality to map HCL or JSON fragment into
Golang structs.")
    (license license:expat)))

(define-public go-github-com-alecthomas-participle-v2
  (package
    (name "go-github-com-alecthomas-participle-v2")
    (version "2.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/participle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hfgrdzj0p2knqmnspkpjb1y4bbhbxbykckpvsp3mc1n9hr6gz01"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/participle/v2"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2
           go-github-com-alecthomas-kong)) ; for CLI
    (home-page "https://github.com/alecthomas/participle")
    (synopsis "Parser library for Go")
    (description
     "This package provides a parser library for Golang which constructs
parsers from definitions in struct tags and parses directly into those
structs.  The approach is similar to how other marshallers work in Golang,
\"unmarshalling\" an instance of a grammar into a struct.")
    (license license:expat)))

(define-public go-github-com-alecthomas-repr
  (package
    (name "go-github-com-alecthomas-repr")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/repr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ikvl78dighkn87bxk6gki4wcz9f138n7kbqkagj5vbdb690yjkl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/repr"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/alecthomas/repr/")
    (synopsis "Represent Go values in an almost direct form")
    (description
     "This package attempts to represent Go values in a form that can be used
almost directly in Go source code.")
    (license license:expat)))

(define-public go-github-com-alecthomas-units
  ;; No release, see <https://github.com/alecthomas/units/issues/9>.
  (let ((commit "2efee857e7cfd4f3d0138cc3cbb1b4966962b93a")
        (revision "0"))
    (package
      (name "go-github-com-alecthomas-units")
      (version "0.0.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alecthomas/units")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1j65b91qb9sbrml9cpabfrcf07wmgzzghrl7809hjjhrmbzri5bl"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/alecthomas/units"))
      (native-inputs
       (list go-github-com-stretchr-testify))
      (home-page "https://github.com/alecthomas/units")
      (synopsis "Helpful unit multipliers and functions for Go")
      (description
       "This library provides unit multipliers and functions for Go.")
      (license license:expat))))

(define-public go-github-com-alessio-shellescape
  (package
    (name "go-github-com-alessio-shellescape")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alessio/shellescape")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14zypi8qdxl77lks5b9jshr17idrm4sri1rxgpw5q4dys1palddd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alessio/shellescape"))
    (home-page "https://github.com/alessio/shellescape")
    (synopsis "Escape arbitrary strings for use as command line arguments")
    (description
     "This package provides the @code{shellescape.Quote} to escape arbitrary
strings for a safe use as command line arguments in the most common POSIX
shells.")
    (license license:expat)))

(define-public go-github-com-alsm-ioprogress
  (package
    (name "go-github-com-alsm-ioprogress")
    (version "0.0.0-20170412085706-063c3725f436")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alsm/ioprogress")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10ym5qlq77nynmkxbk767f2hfwyxg2k7hrzph05hvgzv833dhivh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alsm/ioprogress"))
    (home-page "https://github.com/alsm/ioprogress")
    (synopsis "Textual progress bars in Go")
    (description
     "@code{ioprogress} is a Go library with implementations of
@code{io.Reader} and @code{io.Writer} that draws progress bars.  The primary
use case for these are for command-line applications but alternate progress
bar writers can be supplied for alternate environments.")
    (license license:expat)))

(define-public go-github-com-anacrolix-fuse
  (package
    (name "go-github-com-anacrolix-fuse")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             ;; It's an alternative and maintained fork
             ;; https://github.com/bazil/fuse
             ;; -> https://github.com/zegl/fuse
             ;;    ->
             (url "https://github.com/anacrolix/fuse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rlzcdgv7s3ywmhapp35qgi0p5cz8acw589n7hr918hq30dp7w9z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; tests require fuse mount
      #:import-path "github.com/anacrolix/fuse"))
    (propagated-inputs
     (list go-github-com-anacrolix-envpprof
           go-github-com-tv42-httpunix
           go-golang-org-x-sys))
    (home-page "https://github.com/anacrolix/fuse")
    (synopsis "FUSE implementation in Golang")
    (description
     "Package fuse enables writing FUSE file systems on Linux, OS X, and
@code{FreeBSD}.")
    (license license:bsd-3)))

(define-public go-github-com-anacrolix-generics
  (package
    (name "go-github-com-anacrolix-generics")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/generics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19nagwx6s5sjyphn5czlplqvzj4zxcv3jl9zskpix5yxg5l24san"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/generics"))
    (propagated-inputs (list go-golang-org-x-exp))
    (home-page "https://github.com/anacrolix/generics")
    (synopsis "Generic implementation for Golang")
    ;; FIXME: The project provides no documentatin of README. Report upstream.
    (description "This package provides generic implementations for Go.")
    (license license:mpl2.0)))

(define-public go-github-com-anacrolix-log
  (package
    (name "go-github-com-anacrolix-log")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/log")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pbvg15rrsh2gv10vsdqak8r572kkjlhil1g0gngnzbcyifqmca0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/log"))
    (native-inputs
     (list go-github-com-frankban-quicktest
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-anacrolix-generics))
    (home-page "https://github.com/anacrolix/log")
    (synopsis "Context-style logging for Golang")
    (description
     "Package log implements a std log compatible logging system that draws
some inspiration from the @url{https://docs.python.org/3/library/logging.html,
Python logging module} from Python's standard library.  It supports multiple
handlers, log levels, zero-allocation, scopes, custom formatting, and
environment and runtime configuration.")
    (license license:mpl2.0)))

(define-public go-github-com-andreasbriese-bbloom
  (package
    (name "go-github-com-andreasbriese-bbloom")
    (version "0.0.0-20190825152654-46b345b51c96")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/AndreasBriese/bbloom")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "021c0pl7r4pc9yslqhbjg9wr6dm03lnzf94a0b9c0hmg0bhhkln9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/AndreasBriese/bbloom"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/AndreasBriese/bbloom")
    (synopsis "Bitset Bloom filter for Golang")
    (description
     "This package implements a fast bloom filter with real @code{bitset} and
JSONMarshal/JSONUnmarshal to store/reload the Bloom filter.")
    ;; XXX: It may be included to guix licenses: CC0 1.0 UNIVERSAL
    ;; <http://creativecommons.org/publicdomain/zero/1.0/>
    ;; Dual licence: MIT (Expat) and CC0 1.0 UNIVERSAL.
    (license license:expat)))

(define-public go-github-com-andreaskoch-go-fswatch
  (package
    (name "go-github-com-andreaskoch-go-fswatch")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andreaskoch/go-fswatch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0caikz1bbb2g9w8hyk7qvwixsy8dvc2gism10927q2cc1100mlr2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/andreaskoch/go-fswatch"))
    (home-page "https://github.com/andreaskoch/go-fswatch")
    (synopsis "File system watch library")
    (description
     "fswatch is a go library for watching file system changes to @emph{does not}
depend on inotify.")
    (license license:bsd-3)))

(define-public go-github-com-anmitsu-go-shlex
  (package
    (name "go-github-com-anmitsu-go-shlex")
    (version "0.0.0-20200514113438-38f4b401e2be")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anmitsu/go-shlex")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17iz68yzbnr7y4s493asbagbv79qq8hvl2pkxvm6bvdkgphj8w1g"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/anmitsu/go-shlex"))
    (home-page "https://github.com/anmitsu/go-shlex")
    (synopsis "Simple shell-like lexical analyzer for Go")
    (description
     "This package provides a simple lexical analyzer to parse shell-like
commands.")
    (license license:expat)))

(define-public go-github-com-antihax-optional
  (package
    (name "go-github-com-antihax-optional")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/antihax/optional")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ix08vl49qxr58rc6201cl97g1yznhhkwvqldslawind99js4rj0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/antihax/optional"))
    (home-page "https://github.com/antihax/optional")
    (synopsis "Optional parameters for Golang")
    (description
     "This package implements optimal parameters for data-types.")
    (license license:expat)))

(define-public go-github-com-antlr4-go-antlr-v4
  (package
    (name "go-github-com-antlr4-go-antlr-v4")
    (version "4.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/antlr4-go/antlr")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m5q00fvz28dgvv3ws924p6gamxm6gzqfm12f5ryhljifg22xq3d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/antlr4-go/antlr/v4"))
    (propagated-inputs
     (list go-golang-org-x-exp))
    (home-page "https://github.com/antlr4-go/antlr")
    (synopsis "ANTLR4 Go Runtime Module")
    (description
     "This package implements the Go version of the
@url{https://github.com/antlr/antlr4, ANTLR 4} runtime - ANother Tool for
Language Recognition, a parser generator that uses a LL algorithm for
parsing.")
    (license license:bsd-3)))

(define-public go-github-com-apex-logs
  (package
    (name "go-github-com-apex-logs")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/apex/logs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dm4bqwnjlb5xgpym3qwmn3gxr05p29fjd6i8vdbc34cj6lyc35h"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Network access is requried.
      #:test-flags #~(list "-skip" "TestService_GetProjects|TestHTTP")
      #:import-path "github.com/apex/logs"))
    (native-inputs
     (list go-github-com-tj-assert))
    (home-page "https://github.com/apex/logs")
    (synopsis "Logs client for Golang")
    ;; There is no much info in project's README.
    (description "This package implements a log client.")
    (license license:expat)))

(define-public go-github-com-apparentlymart-go-dump
  (package
    (name "go-github-com-apparentlymart-go-dump")
    (version "0.0.0-20190214190832-042adf3cf4a0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-dump")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yqc50v82za54j2yy3ln7jzx983scb5gbh195wb6vmvqj18q696q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/apparentlymart/go-dump"))
    (home-page "https://github.com/apparentlymart/go-dump")
    (synopsis "Utility for formatting Go values in a pretty-printed way")
    (description
     "This package implements a functionality for generating pretty-printed
dumps of Go values.")
    (license license:mpl2.0)))

(define-public go-github-com-apparentlymart-go-textseg-autoversion
  (package
    (name "go-github-com-apparentlymart-go-textseg-autoversion")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-textseg")
             (commit (go-version->git-ref version #:subdir "autoversion"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06y73sqyihcyfigy9z0kbv4x1dd7yh4ipkxhsyshd04hwxfxx0bq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/apparentlymart/go-textseg/autoversion"
      #:unpack-path "github.com/apparentlymart/go-textseg"))
    (propagated-inputs
     (list go-github-com-apparentlymart-go-textseg-v9
           go-github-com-apparentlymart-go-textseg-v10
           go-github-com-apparentlymart-go-textseg-v11
           go-github-com-apparentlymart-go-textseg-v12
           go-github-com-apparentlymart-go-textseg-v13
           go-github-com-apparentlymart-go-textseg-v15))
    (home-page "https://github.com/apparentlymart/go-textseg")
    (synopsis "Wrap go-textesg based on the Unicode Golang version")
    (description
     "This package is a wrapper around each of the the
Unicode-version-specific textseg implementations that selects an
implementation automatically based on the Unicode version of the Go standard
library that it's being built against.")
    (license (list license:expat license:unicode license:asl2.0))))

(define-public go-github-com-apparentlymart-go-textseg-v9
  (package
    (name "go-github-com-apparentlymart-go-textseg-v9")
    (version "9.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-textseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pmnl674ksbizkdilqz1j44djws9dj23pc6bpbjma8jfjq6b2zax"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/apparentlymart/go-textseg/v9"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-submodule
            (lambda* (#:key import-path #:allow-other-keys #:rest args)
              (when (false-if-exception
                     (stat (string-append "src/" import-path "/autoversion")))
                (delete-file-recursively
                 (string-append "src/" import-path "/autoversion"))))))))
    (home-page "https://github.com/apparentlymart/go-textseg")
    (synopsis "Go implementation of Unicode Text Segmentation")
    (description
     "This package provides an implementation of the Unicode Text Segmentation
specification for Go.  Specifically, it currently includes only the grapheme
cluster segmentation algorithm.")
    ;; Project is released under Expat terms.  Some parts use Unicode and
    ;; ASL2.0 licenses.
    (license (list license:expat license:unicode license:asl2.0))))

(define-public go-github-com-apparentlymart-go-textseg-v10
  (package
    (inherit go-github-com-apparentlymart-go-textseg-v9)
    (name "go-github-com-apparentlymart-go-textseg-v10")
    (version "10.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-textseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jj3w8m2vz574s9lq0468f56kq348wm7xhvrac5yrqw1axg6hjxd"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-apparentlymart-go-textseg-v9)
       ((#:import-path _) "github.com/apparentlymart/go-textseg/v10")))))

(define-public go-github-com-apparentlymart-go-textseg-v11
  (package
    (inherit go-github-com-apparentlymart-go-textseg-v10)
    (name "go-github-com-apparentlymart-go-textseg-v11")
    (version "11.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-textseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gyhj8vqpmxcd7zjq3myj6872fqf1si09dwbv03msp7hf4g976cx"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-apparentlymart-go-textseg-v10)
       ((#:import-path _) "github.com/apparentlymart/go-textseg/v11")))))

(define-public go-github-com-apparentlymart-go-textseg-v12
  (package
    (inherit go-github-com-apparentlymart-go-textseg-v11)
    (name "go-github-com-apparentlymart-go-textseg-v12")
    (version "12.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-textseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i8p4kkgx8yhxwzqmnpx8f87s0h27n7jrdhv78ydh6618512x5v9"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-apparentlymart-go-textseg-v11)
       ((#:import-path _) "github.com/apparentlymart/go-textseg/v12")))))

(define-public go-github-com-apparentlymart-go-textseg-v13
  (package
    (inherit go-github-com-apparentlymart-go-textseg-v12)
    (name "go-github-com-apparentlymart-go-textseg-v13")
    (version "13.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-textseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gdgi0d52rq1xsdn9icc8lghn0f2q927cifmrlfxflf7bf21vism"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-apparentlymart-go-textseg-v12)
       ((#:import-path _) "github.com/apparentlymart/go-textseg/v13")))))

(define-public go-github-com-apparentlymart-go-textseg-v15
  (package
    (inherit go-github-com-apparentlymart-go-textseg-v13)
    (name "go-github-com-apparentlymart-go-textseg-v15")
    (version "15.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-textseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bb7mxkxjhsp8kfwm26n0ds1mpwxxdpy4gndvyz26bqfm6v8vsa4"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-apparentlymart-go-textseg-v13)
       ((#:import-path _) "github.com/apparentlymart/go-textseg/v15")))))

(define-public go-github-com-apparentlymart-go-textseg-v16
  (package
    (inherit go-github-com-apparentlymart-go-textseg-v15)
    (name "go-github-com-apparentlymart-go-textseg-v16")
    (version "16.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-textseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j7vm09cd36wm4z986qz5am3rk242v52amcapwbdbkbgzx2kqfkm"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-apparentlymart-go-textseg-v15)
       ((#:import-path _) "github.com/apparentlymart/go-textseg/v16")))))

(define-public go-github-com-arbovm-levenshtein
  (package
    (name "go-github-com-arbovm-levenshtein")
    (version "0.0.0-20160628152529-48b4e1c0c4d0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/arbovm/levenshtein")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nmx2iip8xpnbmy6gvqpc9ikizr33dr40xgv746h0b0by8n7rv7y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/arbovm/levenshtein"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/arbovm/levenshtein")
    (synopsis "Levenshtein Distance in Golang")
    (description
     "This package implements a functionality to calculate the
@url{http://en.wikipedia.org/wiki/Levenshtein_distance, Levenshtein
Distance}.")
    (license license:bsd-3)))

(define-public go-github-com-arceliar-phony
  (package
    (name "go-github-com-arceliar-phony")
    (version "v0.0.0-20220903101357-530938a4b13d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Arceliar/phony")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ww3issk2jg9nzijmz1xncdhd0mh553nixns34s3yjm4mb8c5s93"))))
    (arguments
     '(#:import-path "github.com/Arceliar/phony"))
    (build-system go-build-system)
    (home-page "https://github.com/Arceliar/phony")
    (synopsis "Very minimal actor model library")
    (description "Phony is a very minimal actor model library for Go,
inspired by the causal messaging system in the Pony programming language.")
    (license license:expat)))

(define-public go-github-com-armon-circbuf
  (package
    (name "go-github-com-armon-circbuf")
    (version "0.0.0-20190214190532-5111143e8da2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/armon/circbuf")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nhzs8wza5sxqjh0920jypy9irq6cspd55g8a9vgyjjfrqb5njs0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/armon/circbuf"))
    (home-page "https://github.com/armon/circbuf")
    (synopsis "Circular buffer for Golang")
    (description
     "This package provides a circular buffer object.  The buffer can be
written to infinitely, but has a fixed size, so only the last @code{size}
bytes are ever retained.")
    (license license:expat)))

(define-public go-github-com-armon-go-radix
  (package
    (name "go-github-com-armon-go-radix")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/armon/go-radix")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m1k0jz9gjfrk4m7hjm7p03qmviamfgxwm2ghakqxw3hdds8v503"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/armon/go-radix"))
    (home-page "https://github.com/armon/go-radix")
    (synopsis "Go implementation of Radix trees")
    (description "This package provides a single @code{Tree} implementation,
optimized for sparse nodes of
@url{http://en.wikipedia.org/wiki/Radix_tree,radix tree}.")
    (license license:expat)))

(define-public go-github-com-arolek-p
  (package
    (name "go-github-com-arolek-p")
    (version "0.0.0-20191103215535-df3c295ed582")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ARolek/p")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dsfrksyrnr3d6aiafnjf3nqyq5jn7s7pzd64j18kngc8l6jrxj1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/arolek/p"))
    (home-page "https://github.com/arolek/p")
    (synopsis "Pointer to the value")
    (description
     "Pacakge p takes in values and returns a pointer to the value.")
    (license license:expat)))

(define-public go-github-com-arran4-golang-ical
  (package
    (name "go-github-com-arran4-golang-ical")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arran4/golang-ical")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gcn5afds1dnq3wrl4ndi4wqqwmrnvh9pdqhyv77d3cqakn82vj3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/arran4/golang-ical"))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-stretchr-testify))
    (home-page "https://github.com/arran4/golang-ical")
    (synopsis "Handle iCalenders in Go")
    (description
     "The @code{ical} package provides an ICS/iCalender parser and serialiser
for Go.")
    (license license:asl2.0)))

(define-public go-github-com-asaskevich-govalidator
  (package
    (name "go-github-com-asaskevich-govalidator")
    (version "11.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asaskevich/govalidator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aab1pym5c6di8vidynp6ly5j4kcqv6lp2737gw0a07zng0nn8lw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/asaskevich/govalidator"
      #:test-flags
      #~(list "-vet=off")   ;Go@1.24 forces vet, but tests are not ready yet.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; XXX: Some validation are failed in the test.
                  (("TestIsExistingEmail") "OffTestIsExistingEmail"))))))))
    (home-page "https://github.com/asaskevich/govalidator")
    (synopsis "Collection of various validators for Golang")
    (description
     "This package provides validators and sanitizers for strings, structs and
collections.  It was based on
@url{https://github.com/chriso/validator.js,validator.js}.")
    (license license:expat)))

(define-public go-github-com-asdine-storm-v3
  (package
    (name "go-github-com-asdine-storm-v3")
    (version "3.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asdine/storm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07913m4nxy7cbc9q6ldqvxx60rh32b4djyvcp9bxwbb3c4al3fh4"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; It requires v4 but import it as v1.
            (substitute* (find-files "." "\\.go$")
              (("github.com/vmihailenco/msgpack")
               "github.com/vmihailenco/msgpack/v4"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/asdine/storm/v3"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-golang-protobuf
           go-github-com-sereal-sereal-go-sereal
           go-github-com-vmihailenco-msgpack-v4
           go-go-etcd-io-bbolt))
    (home-page "https://github.com/asdine/storm")
    (synopsis "BoltDB toolkit for Golang")
    (description
     "Storm is a toolkit for @url{https://github.com/coreos/bbolt, BoltDB},
providing various methods to work with it.")
    (license license:expat)))

(define-public go-github-com-atotto-clipboard
  (package
    (name "go-github-com-atotto-clipboard")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atotto/clipboard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ycd8zkgsq9iil9svhlwvhcqwcd7vik73nf8rnyfnn10gpjx97k5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/atotto/clipboard"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1"))))))
    (native-inputs
     (list xorg-server-for-tests))
    (propagated-inputs (list xclip))
    (home-page "https://github.com/atotto/clipboard")
    (synopsis "Clipboard for Golang")
    (description
     "@code{clipboard} provides copying and pasting to the clipboard for Go.")
    (license license:bsd-3)))

(define-public go-github-com-audriusbutkevicius-recli
  (package
    (name "go-github-com-audriusbutkevicius-recli")
    (version "0.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AudriusButkevicius/recli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mxrpn8p6ylf5qjzsqrk96nky5vgagjkkpd5jwpm6sa977qb0v3i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/AudriusButkevicius/recli"))
    (native-inputs
     (list go-github-com-pkg-errors
           go-github-com-urfave-cli))
    (home-page "https://github.com/AudriusButkevicius/recli")
    (synopsis "Reflection-based CLI generator")
    (description
     "For a given struct, @code{recli} builds a set of @code{urfave/cli}
commands which allows you to modify it from the command line.  It is useful
for generating command line clients for your application configuration that is
stored in a Go struct.")
    (license license:mpl2.0)))

(define-public go-github-com-avast-retry-go
  (package
    (name "go-github-com-avast-retry-go")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/avast/retry-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zkn8c2gyz8j90bf0aj6avfl3sf7j4rk5g4ak4yhglnsx72jdhbz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/avast/retry-go"
      #:test-flags #~(list "-skip" "TestMaxDelay")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/avast/retry-go")
    (synopsis "Simple golang library for retry mechanism")
    (description
     "This package is a simple Go library that provides retry functionality
for functions that may fail.  It includes various customizable retry
strategies, such as fixed delay, backoff delay, and random delay.")
    (license license:expat)))

(define-public go-github-com-avast-retry-go-v3
  (package
    (inherit go-github-com-avast-retry-go)
    (name "go-github-com-avast-retry-go-v3")
    (version "3.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/avast/retry-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01mwrzjh2y3xignkivx8kaghjs3gwb3z89zqgxjfaslslazc863b"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-avast-retry-go)
       ((#:import-path _) "github.com/avast/retry-go/v3")))))

(define-public go-github-com-avast-retry-go-v4
  (package
    (inherit go-github-com-avast-retry-go)
    (name "go-github-com-avast-retry-go-v4")
    (version "4.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/avast/retry-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09gs4wmkq7ragyf2xd0h6j8f9xqq66cwa95kwp5qdwz3wwv9xq1b"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-avast-retry-go)
       ((#:import-path _) "github.com/avast/retry-go/v4")))))

(define-public go-github-com-awesome-gocui-gocui
  (package
    (name "go-github-com-awesome-gocui-gocui")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/awesome-gocui/gocui")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "082ziwxj239nxcclv54d783933s6c5ks592mq3ilcvg1vfyfkjz8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/awesome-gocui/gocui"))
    (propagated-inputs
     (list go-github-com-gdamore-tcell-v2
           go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/awesome-gocui/gocui")
    (synopsis "Console User Interface in Golang")
    (description
     "This package implements a functionality to create console user
interfaces.")
    (license license:bsd-3)))

(define-public go-github-com-awesome-gocui-keybinding
  (let ((commit "86029037a63f3b47096fcfef02f63e5e5d6d5abd")
        (revision "1"))
    (package
      (name "go-github-com-awesome-gocui-keybinding")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/awesome-gocui/keybinding")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wa7scakwbqfzxc81wxmw1z0c9w3z92vdrxa8mha6w9ykifjdkyz"))))
      (build-system go-build-system)
      (arguments
       (list
        #:tests? #f ;broken tests
        #:import-path "github.com/awesome-gocui/keybinding"))
      (propagated-inputs (list go-github-com-awesome-gocui-gocui))
      (home-page "https://github.com/awesome-gocui/keybinding")
      (synopsis "Wrapper for parsing gocui keybindings in Golang")
      (description
       "This package provides a golang wrapper for parsing gocui keybindings.")
      (license license:expat))))

(define-public go-github-com-axiomhq-hyperloglog
  (package
    (name "go-github-com-axiomhq-hyperloglog")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/axiomhq/hyperloglog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a37q1prgpp80a93yx7zxhcam8fznzxvfvz6likfixz0ans2lbav"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/axiomhq/hyperloglog"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-kamstrup-intmap
                             go-github-com-dgryski-go-metro
                             go-github-com-davecgh-go-spew))
    (home-page "https://github.com/axiomhq/hyperloglog")
    (synopsis "Algorithm for approximating the number of distinct elements")
    (description
     "HyperLogLog is an improved version of
@url{https://en.wikipedia.org/wiki/HyperLogLog,HyperLogLog} for the
count-distinct problem, approximating the number of distinct elements in a
multiset.  This implementation offers enhanced performance, flexibility, and
simplicity while maintaining accuracy.")
    (license license:expat)))

(define-public go-github-com-aybabtme-rgbterm
  (package
    (name "go-github-com-aybabtme-rgbterm")
    (version "0.0.0-20170906152045-cc83f3b3ce59")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/aybabtme/rgbterm")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wvmxvjn64968ikvnxrflb1x8rlcwzpfl53fzbxff2axbx9lq50q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aybabtme/rgbterm"))
    (home-page "https://github.com/aybabtme/rgbterm")
    (synopsis "RGB colors support in terminal")
    (description
     "Package rgbterm colorizes bytes and strings using RGB colors, for a full
range of pretty terminal strings.")
    (license license:expat)))

(define-public go-github-com-aymanbagabas-go-osc52-v2
  (package
    (name "go-github-com-aymanbagabas-go-osc52-v2")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aymanbagabas/go-osc52")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y4y49zys7fi5wpicpdmjqnk0mb6569zg546km02yck2349jl538"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aymanbagabas/go-osc52/v2"))
    (home-page "https://github.com/aymanbagabas/go-osc52")
    (synopsis "Terminal ANSI OSC52 wrapper")
    (description
     "OSC52 is a terminal escape sequence that allows copying text to the
clipboard.")
    (license license:expat)))

(define-public go-github-com-aymanbagabas-go-udiff
  (package
    (name "go-github-com-aymanbagabas-go-udiff")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aymanbagabas/go-udiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09p17r8s5flhq6p69z08345q0y99dpb0yyashlwpgxn45xir7y6g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aymanbagabas/go-udiff"))
    (home-page "https://github.com/aymanbagabas/go-udiff")
    (synopsis "Diffing library for Golang")
    (description
     "@code{udiff} (micro-diff, or µDiff) is a library that implements the
@url{http://www.xmailserver.org/diff2.pdf, Myers' diffing algorithm}.  It aims to
provide a minimal API to compute and apply diffs with zero dependencies.  It also
supports generating diffs in the
@url{https://www.gnu.org/software/diffutils/manual/html_node/Unified-Format.html,
Unified Format}.")
    (license license:expat)))

(define-public go-github-com-beorn7-perks
  (package
    (name "go-github-com-beorn7-perks")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/beorn7/perks")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17n4yygjxa6p499dj3yaqzfww2g7528165cl13haj97hlx94dgl7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/beorn7/perks"))
    (home-page "https://github.com/beorn7/perks")
    (synopsis "Compute approximate quantiles over an unbounded data stream")
    (description
     "Perks contains the Go package @code{quantile} that computes
approximate quantiles over an unbounded data stream within low memory and CPU
bounds.")
    (license license:expat)))

(define-public go-github-com-bgentry-speakeasy
  (package
    (name "go-github-com-bgentry-speakeasy")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bgentry/speakeasy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pasgmb9gcchprc06fbn7yjgp6caz03j6pgj14mmr8bcx0zfq7ag"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bgentry/speakeasy"))
    (home-page "https://github.com/bgentry/speakeasy")
    (synopsis "Reading password input without cgo")
    (description
     "This package provides cross-platform Golang helpers for taking user
input from the terminal while not echoing the input back (similar to
@code{getpasswd}).  The package uses syscalls to avoid any dependence on cgo,
and is therefore compatible with cross-compiling.")
    (license license:expat)))

(define-public go-github-com-bitly-go-hostpool
  (package
    (name "go-github-com-bitly-go-hostpool")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bitly/go-hostpool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iibj7dwymczw7cknrh6glc6sdpp4yap2plnyr8qphynwrzlz73w"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bitly/go-hostpool"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/bitly/go-hostpool")
    (synopsis "Pool among multiple hosts from Golang")
    (description
     "This package provides a Go package to intelligently and flexibly pool among
multiple hosts from your Go application.  Host selection can operate in round
robin or epsilon greedy mode, and unresponsive hosts are avoided.")
    (license license:expat)))

(define-public go-github-com-bitly-timer-metrics
  (package
    (name "go-github-com-bitly-timer-metrics")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bitly/timer_metrics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02fhx8hx8126m2cgxw9fm8q2401r7zfann8b5zy5yyark1sgkrb4"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bitly/timer_metrics"))
    (home-page "https://github.com/bitly/timer_metrics")
    (synopsis "Capture timings and enable periodic metrics every @var{n} events")
    (description "This package provides an efficient way to capture timing
information and periodically output metrics")
    (license license:expat)))

(define-public go-github-com-bits-and-blooms-bitset
  (package
    (name "go-github-com-bits-and-blooms-bitset")
    (version "1.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bits-and-blooms/bitset")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m1rk1bf6i1jnhjxm774i3rhg8n8s88a3n5pnhg5a9gf34y7r8az"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bits-and-blooms/bitset"))
    (home-page "https://github.com/bits-and-blooms/bitset")
    (synopsis "Bitsets in Go")
    (description
     "This package provides a Go implementation of bitsets, which are a
mapping between non-negative integers and boolean values focused on efficient
space usage.")
    (license license:bsd-3)))

(define-public go-github-com-bits-and-blooms-bloom-v3
  (package
    (name "go-github-com-bits-and-blooms-bloom-v3")
    (version "3.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bits-and-blooms/bloom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "022pyzjp862ysl30aj105i2xmapn400ambjh8h1dcyjy9c0f8agn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bits-and-blooms/bloom/v3"))
    (propagated-inputs
     (list go-github-com-bits-and-blooms-bitset
           go-github-com-twmb-murmur3))
    (home-page "https://github.com/bits-and-blooms/bitset")
    (synopsis "Bloom filters in Go")
    (description
     "This package provides a Go implementation of bloom filters,
based on murmurhash.")
    (license license:bsd-2)))

(define-public go-github-com-blang-semver
  (package
    (name "go-github-com-blang-semver")
    (version "3.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blang/semver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16s66zbfkn35msmxpkiwf5dv91kzw7yzxzkcv8ma44j7lbgzx5qk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/blang/semver"))
    (home-page "https://github.com/blang/semver")
    (synopsis "Semantic versioning library written in Go")
    (description
     "Semver is a library for Semantic versioning written in Go.")
    (license license:expat)))

(define-public go-github-com-blang-semver-v4
  (package
    (inherit go-github-com-blang-semver)
    (name "go-github-com-blang-semver-v4")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blang/semver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14h9ys4n4kx9cbj42lkdf4i5k3nkll6sd62jcvl7cs565v6fiknz"))))
    (arguments
     (list
      #:import-path "github.com/blang/semver/v4"
      #:unpack-path "github.com/blang/semver"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))))

(define-public go-github-com-bmatcuk-doublestar
  (package
    (name "go-github-com-bmatcuk-doublestar")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/bmatcuk/doublestar")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bk5bixl6rqa8znxghyp6zndbccx9kdyrymjahgyp6qsrp7rk144"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bmatcuk/doublestar"
      #:test-flags
      #~(list "-vet=off" ;Go@1.24 forces vet, but tests are not ready yet.
              "-skip" "TestMatch")))
    (home-page "https://github.com/bmatcuk/doublestar/")
    (synopsis "Path pattern matching and globbing supporting doublestar")
    (description
     "@code{doublestar} is a Go implementation of path pattern
matching and globbing with support for \"doublestar\" patterns.")
    (license license:expat)))

(define-public go-github-com-bmatcuk-doublestar-v3
  (package
    (inherit go-github-com-bmatcuk-doublestar)
    (name "go-github-com-bmatcuk-doublestar-v3")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bmatcuk/doublestar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "011h07mlmscbxxjr3h30fjjb4dw3gb245nzczaq520r112xlidhj"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-bmatcuk-doublestar)
       ((#:import-path _) "github.com/bmatcuk/doublestar/v3")))))

(define-public go-github-com-bmatcuk-doublestar-v4
  (package
    (inherit go-github-com-bmatcuk-doublestar)
    (name "go-github-com-bmatcuk-doublestar-v4")
    (version "4.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bmatcuk/doublestar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jskh3dp9xmh1bf1a7dh5ykv0pk2v4pxh5bynsl33cmw61dkd6s0"))))
    (arguments
     (list
      #:import-path "github.com/bmatcuk/doublestar/v4"
      #:test-flags
      #~(list "-vet=off"))))) ;Go@1.24 forces vet, but tests are not ready yet.

(define-public go-github-com-bmizerany-perks-quantile
  (package
    (name "go-github-com-bmizerany-perks-quantile")
    (version "0.0.0-20230307044200-03f9df79da1e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bmizerany/perks")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f2a99v3618bz2mf61iwhdjm3xi1gam6v4apqgcrz71gj7ba9943"))))
    (build-system go-build-system)
    (arguments
     (list #:unpack-path "github.com/bmizerany/perks"
           #:import-path "github.com/bmizerany/perks/quantile"))
    (home-page "https://github.com/bmizerany/perks")
    (synopsis "Library for computing quantiles")
    (description
     "Perks contains the Go package @code{quantile} that computes approximate
quantiles over an unbounded data stream within low memory and CPU bounds.")
    (license license:bsd-2)))

;; XXX: This repository has been archived by the owner on Mar 9, 2019. It is
;; now read-only.
(define-public go-github-com-boltdb-bolt
  (package
    (name "go-github-com-boltdb-bolt")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/boltdb/bolt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z7j06lijfi4y30ggf2znak2zf2srv2m6c68ar712wd2ys44qb3r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ;tests are broken in upstream
      #:import-path "github.com/boltdb/bolt"))
    (home-page "https://github.com/boltdb/bolt")
    (synopsis "Embedded key/value database for Golang")
    (description
     "Bolt is a pure Go key/value store inspired by
@url{http://symas.com/mdb/, Howard Chu's LMDB project}.  The goal of the
project is to provide a simple, fast, and reliable database for projects that
don't require a full database server such as Postgres or MySQL.")
    (license license:expat)))

(define-public go-github-com-boombuler-barcode
  (package
    (name "go-github-com-boombuler-barcode")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/boombuler/barcode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wpk7lkq3m9dq7wfkziinfzli78qpiwd15cxhibnnyl9lkifgp9s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/boombuler/barcode"))
    (home-page "https://github.com/boombuler/barcode")
    (synopsis "Barcode creation library for golang")
    (description
     "This package implements a functionality to generate barcodes.

Supported Barcode Types:
@itemize
@item 2 of 5
@item Aztec Code
@item Codabar
@item Code 128
@item Code 39
@item Code 93
@item Datamatrix
@item EAN 13
@item EAN 8
@item PDF 417
@item QR Code
@end itemize")
    (license license:expat)))

(define-public go-github-com-bradfitz-gomemcache
  (package
    (name "go-github-com-bradfitz-gomemcache")
    (version "0.0.0-20230905024940-24af94b03874")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bradfitz/gomemcache")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xhf5zphhilv1jiwsdf5f4b4g2jj8q3yhn2r83f52mpi9s8jp5db"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/bradfitz/gomemcache"))
    (home-page "https://github.com/bradfitz/gomemcache")
    (synopsis "Memcache client library in Go")
    (description
     "This is a memcache client library for the Go programming language.")
    (license license:asl2.0)))

(define-public go-github-com-briandowns-spinner
  (package
    (name "go-github-com-briandowns-spinner")
    (version "1.23.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/briandowns/spinner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1icg6z10rkksbls6c50syfw63vvxbp849w4gbq3dsxlsabj32vsp"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/briandowns/spinner"))
    (propagated-inputs
     (list go-github-com-fatih-color
           go-github-com-mattn-go-isatty
           go-golang-org-x-term))
    (home-page "https://github.com/briandowns/spinner")
    (synopsis "Terminal spinner/progress indicators")
    (description
     "Package spinner is a simple package to add a spinner / progress
indicator to any terminal application.")
    (license license:asl2.0)))

(define-public go-github-com-btcsuite-btclog
  (package
    (name "go-github-com-btcsuite-btclog")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/btcsuite/btclog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aqpgb3bw1g5am2az4f1g1a54xii0axvxp0zymhyl8jdk6hhyyd8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/btcsuite/btclog"))
    (home-page "https://github.com/btcsuite/btclog")
    (synopsis "Subsystem aware logger for Go")
    (description
     "Package @command{btclog} defines a logger interface and provides a
default implementation of a subsystem-aware leveled logger implementing the
same interface.")
    (license license:isc)))

(define-public go-github-com-btcsuite-btclog-v2
  (package
    (inherit go-github-com-btcsuite-btclog)
    (name "go-github-com-btcsuite-btclog-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/btcsuite/btclog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v9zh39d35647g6m9b74l5z1ifbnk4chbglnbmj5ndzj9alzsc38"))))
    (arguments
     (list
      #:import-path "github.com/btcsuite/btclog/v2"
      #:unpack-path "github.com/btcsuite/btclog"))))

(define-public go-github-com-buildkite-shellwords
  (package
    (name "go-github-com-buildkite-shellwords")
    (version "0.0.0-20180315110454-59467a9b8e10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/buildkite/shellwords")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kx6wxbdznarmnifwzmxxcd86bgn27rwpfnw2y2gd0j8zg9g1682"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/buildkite/shellwords"))
    (home-page "https://github.com/buildkite/shellwords")
    (synopsis "Split command-line strings into words")
    (description
     "This package provides a golang library for splitting command-line
strings into words like a POSIX or Windows shell would.")
    (license license:expat)))

(define-public go-github-com-burntsushi-graphics-go
  (package
    (name "go-github-com-burntsushi-graphics-go")
    (version "0.0.0-20160129215708-b43f31a4a966")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BurntSushi/graphics-go")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1084wkrj5l6j48s9lkh28h2zgmw8kp63ra1yw1nfpkf0f6hil3hn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/BurntSushi/graphics-go"
      #:skip-build? #t))
    (home-page "https://github.com/BurntSushi/graphics-go")
    (synopsis "Graphics library for the Golang")
    (description
     "This package provides a library to works with graphics.")
    (license license:bsd-3)))

(define-public go-github-com-burntsushi-toml
  (package
    (name "go-github-com-burntsushi-toml")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/BurntSushi/toml")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "075ay86gn99wlz26x7hp40s4lpc9r026pd2r0ap0pcrvb88inzy1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/BurntSushi/toml"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/BurntSushi/toml")
    (synopsis "Toml parser and encoder for Go")
    (description
     "This package is toml parser and encoder for Go.  The interface is
similar to Go's standard library @code{json} and @code{xml} package.")
    (license license:expat)))

(define-public go-github-com-bytedance-sonic
  (package
    (name "go-github-com-bytedance-sonic")
    (version "1.13.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bytedance/sonic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sh7xmf0sivxbl344ns4i35fijcq259wcz5fn2xb5pkacxffclgg"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/bytedance/sonic/external_jsonlib_test
            ;; - github.com/bytedance/sonic/fuzz
            ;; - github.com/bytedance/sonic/generic_test
            ;; - github.com/bytedance/sonic/loader
            (for-each delete-file-recursively
                      (list "external_jsonlib_test"
                            "fuzz"
                            "generic_test"
                            "loader"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bytedance/sonic"))
    (native-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-bytedance-sonic-loader
           go-github-com-cloudwego-base64x
           go-github-com-klauspost-cpuid-v2
           go-github-com-twitchyliquid64-golang-asm
           go-golang-org-x-arch))
    (home-page "https://github.com/bytedance/sonic")
    (synopsis "JSON serializing and deserializing library")
    (description
     "This package implements a functionality to serialize/deserialize JSON by
using JIT and SIMD approaches.")
    ;; There some other licenses in "licenses" but all of them look like ASL
    ;; compatible.
    (license license:asl2.0)))

(define-public go-github-com-bytedance-sonic-loader
  (package
    (name "go-github-com-bytedance-sonic-loader")
    (version "0.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bytedance/sonic")
             (commit (go-version->git-ref version
                                          #:subdir "loader"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09a7jka6a74802i6a6lgxlc3vp0jnb69hy1l5s772260q1zgnkds"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bytedance/sonic/loader"
      #:unpack-path "github.com/bytedance/sonic"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-cloudwego-iasm))
    (home-page "https://github.com/bytedance/sonic")
    (synopsis "Function loader for Sonic Golang library")
    (description
     "This package provides functionality to load functions used in Sonic JSON
library.")
    (license license:asl2.0)))

(define-public go-github-com-c-bata-go-prompt
  (package
    (name "go-github-com-c-bata-go-prompt")
    (version "0.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/c-bata/go-prompt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16hfb5xvgixn1anbsvazs8ihcrzyww0n8fddx10yiygqhsp07avz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/c-bata/go-prompt"))
    (propagated-inputs
     (list go-github-com-mattn-go-colorable
           go-github-com-mattn-go-runewidth
           go-github-com-mattn-go-tty
           go-github-com-pkg-term
           go-golang-org-x-sys))
    (home-page "https://github.com/c-bata/go-prompt")
    (synopsis "Interactive CLI prompts toolit")
    (description
     "This package provides a library for building powerful interactive
prompts inspired by
@url{https://github.com/jonathanslenders/python-prompt-toolkit,
python-prompt-toolkit}, making it easier to build cross-platform command line
tools using Go.")
    (license license:expat)))

(define-public go-github-com-caarlos0-env
  (package
    (name "go-github-com-caarlos0-env")
    (version "11.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/caarlos0/env")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "074bagdfvsq65i0cak5l7ipci0b1j2m0j8rd54g7rznhqmxwha97"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/caarlos0/env"))
    (home-page "https://github.com/caarlos0/env")
    (synopsis "Library to parse environment variables into structs")
    (description
     "@code{env} is a simple, zero-dependencies library to parse environment
variables into structs.")
    (license license:expat)))

(define-public go-github-com-cention-sany-utf7
  (package
    (name "go-github-com-cention-sany-utf7")
    (version "0.0.0-20170124080048-26cad61bd60a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cention-sany/utf7")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jy15ryfcln1iwchrksqyrnyfy41gisymm4f9sr1d73ja029bznm"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/cention-sany/utf7"))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://github.com/cention-sany/utf7")
    (synopsis "UTF-7 for Go")
    (description
     "The utf7 package provides support for the obsolete UTF-7 text
encoding in Go.")
    (license license:bsd-3)))

(define-public go-github-com-cespare-mph
  (package
    (name "go-github-com-cespare-mph")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cespare/mph")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mvd6bkvf3i3555kqkkr3k9jd4c25scjq4xad35sxpny8f72nbg1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cespare/mph"))
    (home-page "https://github.com/cespare/mph")
    (synopsis "Minimal perfect hashing in Go")
    (description
     "@code{mph} is a Go package that implements a minimal perfect hash table
over strings.")
    (license license:expat)))

(define-public go-github-com-chai2010-webp
  (package
    (name "go-github-com-chai2010-webp")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chai2010/webp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1103iah700q9id04hz37nxbpb25qz13g1ia2r6gdffc3vh2w3riv"))
       (modules '((guix build utils)))
       ;; FIXME: The project indludes a copy of libwebp
       ;; (internal/libwebp-1.5.0) which is availalbe in Guix, find out how to
       ;; build it with it's source.
       (snippet
        #~(begin
            ;; Remove files which were auto generated by 'go generate'.
            (for-each delete-file
                      (find-files "." "^z_libwebp_src_.*\\.c$"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/chai2010/webp"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'go-generate
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "go" "generate")))))))
    (propagated-inputs
     (list go-golang-org-x-image))
    (home-page "https://github.com/chai2010/webp")
    (synopsis "WebP decoder and encoder for Golang")
    (description
     "Package webp implements a decoder and encoder for
@code{https://en.wikipedia.org/wiki/WebP, WebP} images.")
    (license license:bsd-3)))

(define-public go-github-com-charlievieth-fastwalk
  (package
    (name "go-github-com-charlievieth-fastwalk")
    (version "1.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charlievieth/fastwalk")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17zy17q31p8b93bf703rr0xqafp02bb0slkrgpxb8r0aaxz3zg4y"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; github.com/charlievieth/fastwalk/fastwalk_test.go:962:48: cannot use
      ;; math.MaxUint32 (untyped int constant 4294967295) as int value in
      ;; argument to fmt.Sprintf (overflows).
      #:tests? (target-64bit?)
      #:import-path "github.com/charlievieth/fastwalk"))
    (home-page "https://github.com/charlievieth/fastwalk")
    (synopsis "Fast directory traversal for Golang")
    (description
     "Package fastwalk provides a faster version of
@url{/path/filepath#@code{WalkDir,filepath.WalkDir}} for file system scanning
tools.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-bubbles
  (package
    (name "go-github-com-charmbracelet-bubbles")
    (version "0.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/bubbles")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qdcln01bq9lk6r33b8p5d5x850wgd8ddq57n4bg3xn76z2fd657"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/charmbracelet/bubbles"))
    (propagated-inputs
     (list go-github-com-atotto-clipboard
           go-github-com-charmbracelet-bubbletea
           go-github-com-charmbracelet-harmonica
           go-github-com-charmbracelet-lipgloss
           go-github-com-charmbracelet-x-ansi
           go-github-com-charmbracelet-x-exp-golden
           go-github-com-dustin-go-humanize
           go-github-com-lucasb-eyer-go-colorful
           go-github-com-makenowjust-heredoc
           go-github-com-mattn-go-runewidth
           go-github-com-muesli-termenv
           go-github-com-rivo-uniseg
           go-github-com-sahilm-fuzzy))
    (home-page "https://github.com/charmbracelet/bubbles")
    (synopsis "TUI components for Bubble Tea library")
    (description
     "@code{bubbles} is a library that provide components for
@@url{https://github.com/charmbracelet/bubbletea, Bubble Tea} applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-bubbletea
  (package
    (name "go-github-com-charmbracelet-bubbletea")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/bubbletea")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0g5pj40lsdkh2gwixlpg53ji7fajncj512xj0v1x3mk5grgbc2zr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/bubbletea"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          '("examples" "tutorials")))))
          (add-before 'check 'fix-tests
            (lambda _
              ;; XXX: The package requires
              ;; "go-github-com-charmbracelet-x-ansi" version 0.4.5; with the
              ;; newer version of "ansi", some "bubbletea" screen tests fail
              ;; as "ansi" 0.5.2 handles escape sequences a little bit
              ;; differently.
              (substitute* "src/github.com/charmbracelet/bubbletea/screen_test.go"
                (("x1b\\[0K")
                 "x1b[K")
                (("x1b\\[2;0H")
                 "x1b[2;H")))))))
    (propagated-inputs
     (list go-github-com-charmbracelet-lipgloss
           go-github-com-charmbracelet-x-ansi
           go-github-com-charmbracelet-x-term
           go-github-com-erikgeiser-coninput
           go-github-com-muesli-ansi
           go-github-com-muesli-cancelreader
           go-golang-org-x-sync
           go-golang-org-x-sys))
    (home-page "https://github.com/charmbracelet/bubbletea")
    (synopsis "Powerful little TUI framework")
    (description
     "Bubble Tea is a Go framework based on The Elm Architecture.  It is
well-suited for simple and complex terminal applications, either inline,
full-window, or a mix of both.")
    (license license:asl2.0)))

(define-public go-github-com-charmbracelet-colorprofile
  (package
    (name "go-github-com-charmbracelet-colorprofile")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/charmbracelet/colorprofile")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r2qhiq110hjk55y4l4bkcrmkksbr8ah4s8qp9rd96vjy6bmbr2a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/colorprofile"))
    (propagated-inputs
     (list go-github-com-charmbracelet-x-ansi
           go-github-com-charmbracelet-x-term
           go-github-com-lucasb-eyer-go-colorful
           go-github-com-xo-terminfo
           go-golang-org-x-sys))
    (home-page "https://github.com/charmbracelet/colorprofile")
    (synopsis "Magical terminal color handling")
    (description
     "Package colorprofile provides a way to downsample ANSI escape sequence
colors and styles automatically based on output, environment variables, and
Terminfo databases.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-glamour
  (package
    (name "go-github-com-charmbracelet-glamour")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/charmbracelet/glamour")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b8r13n6ia92m6s297br86s82jxzhfb8snildm4mcgx3sb4mg9l6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list ".*\\.xml")
      #:import-path "github.com/charmbracelet/glamour"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Some tests fail with comparing terminal escape
                       ;; sequence.
                       (list "TestRenderHelpers"
                             "TestRenderer/code_block"
                             "TestRenderer/table_align"
                             "TestRenderer/table_truncate"
                             "TestRendererIssues/107"
                             "TestRendererIssues/257"
                             "TestRendererIssues/316"
                             "TestRendererIssues/46_2"
                             "TestRendererIssues/48"
                             "TestRendererIssues/79"
                             "TestTermRenderer"
                             "TestTermRendererWriter"
                             "TestWithChromaFormatterCustom"
                             "TestWithChromaFormatterDefault")
                       "|"))))
    (propagated-inputs
     (list go-github-com-alecthomas-chroma-v2
           go-github-com-charmbracelet-lipgloss
           go-github-com-charmbracelet-x-ansi
           go-github-com-charmbracelet-x-exp-golden
           go-github-com-charmbracelet-x-exp-slice
           go-github-com-microcosm-cc-bluemonday
           go-github-com-muesli-reflow
           go-github-com-muesli-termenv
           go-github-com-yuin-goldmark
           go-github-com-yuin-goldmark-emoji
           go-golang-org-x-term
           go-golang-org-x-text))
    (home-page "https://github.com/charmbracelet/glamour/")
    (synopsis "Write handsome command-line tools with glamour")
    (description
     "@code{glamour} lets you render markdown documents and templates on ANSI
compatible terminals.  You can create your own stylesheet or use one of our
glamorous default themes.")
    (license license:expat)))

;; For chezmoi@2.1.0
(define-public go-github-com-charmbracelet-glamour-0.3
  (hidden-package (package (inherit go-github-com-charmbracelet-glamour)
   (name "go-github-com-charmbracelet-glamour")
   (version "0.3.0")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/charmbracelet/glamour")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0fk7wvn0yvsvhnwz6g4q4qb42r513b66131cgk8ahzs3va6flxk3"))))
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/charmbracelet/glamour"))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs
                     go-github-com-charmbracelet-glamour)
       (replace "go-github-com-alecthomas-chroma-v2"
         go-github-com-alecthomas-chroma)
       (append go-github-com-dlclark-regexp2
               go-github-com-olekukonko-tablewriter-0.0.5))))))

(define-public go-github-com-charmbracelet-harmonica
  (package
    (name "go-github-com-charmbracelet-harmonica")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/harmonica")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aasg0c0xxhwav4ivm1mqmsqab6lk407xky8c19pb85r1hdbq0n7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/harmonica"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))
    (home-page "https://github.com/charmbracelet/harmonica")
    (synopsis "Simple, physics-based animation library")
    (description
     "A simple, efficient spring animation library for smooth, natural motion.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-lipgloss
  (package
    (name "go-github-com-charmbracelet-lipgloss")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/charmbracelet/lipgloss")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iww4y37qsyzswsg0p8ws7wb2jl6i2z2n0kwayvfwwh73qjh8zmr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/lipgloss"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))
    (propagated-inputs
     (list go-github-com-aymanbagabas-go-udiff
           go-github-com-charmbracelet-x-ansi
           go-github-com-charmbracelet-x-cellbuf
           go-github-com-charmbracelet-x-exp-golden
           go-github-com-muesli-termenv
           go-github-com-rivo-uniseg))
    (home-page "https://github.com/charmbracelet/lipgloss")
    (synopsis "Style definitions for nice terminal layouts")
    (description
     "Style definitions for nice terminal layouts.  Built with TUIs in mind.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-log
  (package
    (name "go-github-com-charmbracelet-log")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/log")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dljsz5h6pw9w396sy9na99c2pvi542b3r138lka7l0ifmzpxjw9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/log"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-charmbracelet-lipgloss
           go-github-com-go-logfmt-logfmt
           go-github-com-muesli-termenv
           go-golang-org-x-exp))
    (home-page "https://github.com/charmbracelet/log")
    (synopsis "Colorful Go logging library")
    (description
     "This package provides a minimal and colorful Go logging library.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-ansi
  (package
    (name "go-github-com-charmbracelet-x-ansi")
    (version "0.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/charmbracelet/x")
              (commit (go-version->git-ref version
                                           #:subdir "ansi"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "083zj3yqb48li8w389iabi1b1zklbw7cwam2grvvglcqrrsj3bsf"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "ansi")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/ansi"
      #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs
     (list go-github-com-bits-and-blooms-bitset
           go-github-com-lucasb-eyer-go-colorful
           go-github-com-mattn-go-runewidth
           go-github-com-rivo-uniseg))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "ANSI escape sequence parser and definitions")
    (description
     "@code{ansi} defines common ANSI escape sequences based on the
@url{https://ecma-international.org/publications-and-standards/standards/ecma-48/,
ECMA-48} specs.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-cellbuf
  (package
    (name "go-github-com-charmbracelet-x-cellbuf")
    (version "0.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/charmbracelet/x")
              (commit (go-version->git-ref version
                                           #:subdir "cellbuf"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hx2dcxr40vs73xmhx0yhhafhjhns064zl9i5wskdyp47nl3z81w"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "cellbuf")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/cellbuf"
      #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs
     (list go-github-com-charmbracelet-colorprofile
           go-github-com-charmbracelet-x-ansi
           go-github-com-charmbracelet-x-term
           go-github-com-mattn-go-runewidth
           go-github-com-rivo-uniseg))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Terminal cell buffer functionality")
    (description
     "Package cellbuf provides terminal cell buffer functionality.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-exp-golden
  (package
    (name "go-github-com-charmbracelet-x-exp-golden")
    (version "0.0.0-20241121171228-5bc00623ea2f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version
                                          #:subdir "exp/golden"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "016s67690dr3w3an6m24q6f4vrmwpk0qd4akvvh1dzpfyf4khxd4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/exp/golden"
      #:unpack-path "github.com/charmbracelet/x/"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda* (#:key import-path #:allow-other-keys)
              ;; Tests need to write to that files.
              (with-directory-excursion (string-append "src/" import-path)
                (make-file-writable "testdata/TestRequireEqualUpdate.golden")
                (make-file-writable "testdata/TestRequireEqualNoUpdate.golden"))))
          (add-after 'check 'post-check
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                ;; Remove modified testdata just in case.
                (delete-file-recursively "testdata")))))))
    (propagated-inputs
     (list go-github-com-aymanbagabas-go-udiff))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Verify @code{.golden} file equality")
    (description
     "Golden files (@code{.golden}) contain the raw expected output of
tests,which can contain control codes and escape sequences.  @code{golden}
package provides an API for comparing Golden files.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-exp-slice
  (package
    (name "go-github-com-charmbracelet-x-exp-slice")
    (version "0.0.0-20250910184208-1e3a578dd00d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version
                                          #:subdir "exp/slice"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jbyg0ar8cz5f1lfnsqrzsbl51c74xf27v065nwcw78b26i7mg2g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/exp/slice"
      #:unpack-path "github.com/charmbracelet/x"))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Utility functions for working with slices in Glang")
    (description
     "Package slice provides utility functions for working with slices in Go.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-input
  (package
    (name "go-github-com-charmbracelet-x-input")
    (version "0.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/charmbracelet/x")
              (commit (go-version->git-ref version
                                           #:subdir "input"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yp3hx5b4z61yfbgphabhz16v8alfwd7nwyq40bzls4hcg91hqw5"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "ansi")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/input"
      #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs
     (list go-github-com-charmbracelet-x-ansi
           go-github-com-muesli-cancelreader
           go-github-com-rivo-uniseg
           go-github-com-xo-terminfo
           go-golang-org-x-sys))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Terminal event input handler and driver")
    (description
     "This package provides a terminal event input handler and driver.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-term
  (package
    (name "go-github-com-charmbracelet-x-term")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version
                                          #:subdir "term"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1shw55110fnn4xz80wmgr18czmiil6z1j064m90iw8c7j9llfzn5"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "term")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/term"
      #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs (list go-github-com-rivo-uniseg
                             go-golang-org-x-sys))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Terminal utilities and helpers")
    (description
     "@code{term} provides an API for working with terminals that includes:
@itemize
@item Switching a terminal to the raw mode.
@item Getting, setting and restoring the state of a terminal.
@item Getting size of a terminal.
@item Reading passwords from a terminal without a local echo.
@end itemize")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-windows
  (package
    (name "go-github-com-charmbracelet-x-windows")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version
                                          #:subdir "windows"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "011kkz7l7fqr3a4sslfipiyl6bq51md1rc7ayj73xm5ayscpm0r2"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/charmbracelet/x/windows"
           #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Windows API used at Charmbracelet")
    (description
     "This package provides the Windows API used at Charmbracelet.")
    (license license:expat)))

(define-public go-github-com-checkpoint-restore-go-criu-v6
  (package
    (name "go-github-com-checkpoint-restore-go-criu-v6")
    (version "6.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/checkpoint-restore/go-criu")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b7427rqf1il6pjbgzdm8vwcxvcf013d5sa13k7fi8pmifqb81dz"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/checkpoint-restore/go-criu/v6"
      ;; Failed to get stats.
      #:test-flags #~(list "-skip" "TestGetDumpStats|TestGetRestoreStats")))
    (propagated-inputs
     (list go-github-com-spf13-cobra
           go-golang-org-x-sys
           go-google-golang-org-protobuf))
    (home-page "https://github.com/checkpoint-restore/go-criu")
    (synopsis "Go bindings for CRIU")
    (description
     "This pacakge provides bindings for @url{https://criu.org/, CRIU}.  The
code is based on the Go-based PHaul implementation from the CRIU repository.")
    (license license:asl2.0)))

(define-public go-github-com-checkpoint-restore-go-criu-v7
  (package
    (inherit go-github-com-checkpoint-restore-go-criu-v6)
    (name "go-github-com-checkpoint-restore-go-criu-v7")
    (version "7.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/checkpoint-restore/go-criu")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07qnlmvdzm7gl3lf2kldm83n2bfqnh6qqlhi43734q0dk5bfhpym"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (delete-file-recursively "vendor")))))
    (arguments
     (list
      #:import-path "github.com/checkpoint-restore/go-criu/v7"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Error opening binary file: open
                       ;; test-imgs/pstree.img: no such file or directory.
                       (list "TestNewMemoryReader"
                             "TestGetMemPages"
                             "TestGetPsArgsAndEnvVars"
                             "TestSearchPattern"
                             ;; Failed to get stats.
                             "TestGetDumpStats"
                             "TestGetRestoreStats")
                       "|"))))))

(define-public go-github-com-cheggaaa-pb
  (package
    (name "go-github-com-cheggaaa-pb")
    (version "1.0.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cheggaaa/pb/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n8y589gf9aw53j72y4z8mzkgahbf6k8h19n2j0mllw5xpvpgijy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cheggaaa/pb"))
    (propagated-inputs
     (list go-github-com-fatih-color
           go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-runewidth
           go-github-com-vividcortex-ewma))
    (home-page "https://github.com/cheggaaa/pb/")
    (synopsis "Console progress bar for Go")
    (description
     "This package is a Go library that draws progress bars on the terminal.")
    (license license:bsd-3)))

(define-public go-github-com-cheggaaa-pb-v3
  (package
    (inherit go-github-com-cheggaaa-pb)
    (name "go-github-com-cheggaaa-pb-v3")
    (version "3.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cheggaaa/pb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zbqjc7phdsa4p66j3nrpbgrdq171nvqma99bq6d3w373lnl1q67"))))
    (arguments
     (list
      #:import-path "github.com/cheggaaa/pb/v3"
      #:unpack-path "github.com/cheggaaa/pb"))))

(define-public go-github-com-chzyer-logex
  (package
    (name "go-github-com-chzyer-logex")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chzyer/logex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c9yr3r7dl3lcs22cvmh9iknihi9568wzmdywmc2irkjdrn8bpxw"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; See <https://github.com/chzyer/logex/issues/4> and
      ;; <https://github.com/chzyer/logex/pull/7>.
      #:tests? #f
      #:import-path "github.com/chzyer/logex"))
    (home-page "https://github.com/chzyer/logex")
    (synopsis "Golang log library")
    (description
     "This package provides a Golang log library supporting tracing and log
levels that works by wrapping the standard @code{log} library.")
    (license license:expat)))

(define-public go-github-com-chzyer-readline
  (package
    (name "go-github-com-chzyer-readline")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chzyer/readline")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1msh9qcm7l1idpmfj4nradyprsr86yhk9ch42yxz7xsrybmrs0pb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/chzyer/readline"))
    (native-inputs
     (list go-github-com-chzyer-test))
    (propagated-inputs
     (list go-github-com-chzyer-logex
           go-golang-org-x-sys))
    (home-page "https://github.com/chzyer/readline")
    (synopsis "Pure Go readline library")
    (description
     "Readline is a pure Go implementation of a GNU-Readline like library.")
    (license license:expat)))

(define-public go-github-com-cilium-ebpf
  (package
    (name "go-github-com-cilium-ebpf")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cilium/ebpf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p3wssg00d0h5dn1fadl0g8iwcak0d6myyjlqwgf6rnfnlajcrgi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cilium/ebpf"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Tests fail with errors:
               ;; - neither debugfs nor tracefs are mounted
               ;; - no such file or directory
               (list "TestNewEvent"
                     "TestFSType"
                     "TestEventID"
                     "TestSanitizePath"
                     "TestGetTracefsPath"
                     ;; Tests failing on i686-linux system.
                     #$@(if (target-x86?)
                            '("TestAuxvVDSOMemoryAddress/auxv64le.bin"
                              "TestUnsafeB.*/.*_with_trailing_padding"
                              "TestUnsafeB.*/.*_with_interspersed_padding"
                              "TestUnsafeB.*/.*_between_slice_entries"
                              "TestUnsafeB.*/.*_between_array_entries")
                            '())
                     ;; Tests failing on ARM systems with error: reading vDSO
                     ;; ELF: read /proc/self/mem: input/output error.
                     #$@(if (target-arm?)
                            '("TestVDSOVersion"
                              "TestCurrentKernelVersion")
                            '()))
               "|"))
      ;; XXX: 337 tests failed and 664 passed when "..."  is preserved, run
      ;; some of available tests, figure out how to fix the rests.
      #:test-subdirs
      #~(list
         ;; Tests fail with error: detect support for
         ;; FnSkbSetTstamp for program type SchedCLS:
         ;; detect support for SchedCLS: load program:
         ;; operation not permitted
         ;; "features"

         ;; Failed to adjust rlimit, tests may fail
         ;; "link"
         ;; "perf"
         ;; "ringbuf"
         "asm"
         "internal/...")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "example_sock_elf_test.go"
                                "example_sock_extract_dist_test.go"
                                "examples"))))))))
    (propagated-inputs
     (list go-github-com-go-quicktest-qt
           go-github-com-google-go-cmp
           go-github-com-jsimonetti-rtnetlink-v2
           go-golang-org-x-exp
           go-golang-org-x-sys))
    (home-page "https://ebpf-go.dev/")
    (synopsis "Read, modify and load extended Berkeley Packet Filter programs in Golang")
    (description
     "This package provides utilities for loading, compiling, and debugging
@url{https://www.ebpf.io/,eBPF} programs.  It has minimal external
dependencies and is intended to be used in long running processes.")
    (license license:expat)))

(define-public go-github-com-clbanning-mxj-v2
  (package
    (name "go-github-com-clbanning-mxj-v2")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clbanning/mxj")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kdh9cdq0x9jk5vzn2k489w7k88rdwf1b87yzhr7jbkchh2nh608"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; XXX: find out why
      #:import-path "github.com/clbanning/mxj/v2"
      #:test-subdirs #~(list ".")))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (home-page "https://github.com/clbanning/mxj")
    (synopsis "Decode / encode XML in Golang")
    (description
     "This package implements a functionality to marshal/unmarshal XML to/from
@code{map[string]interface{}} values (and JSON); extract/modify values from
maps by key or key-path, including wildcards.")
    (license license:expat)))

(define-public go-github-com-cli-safeexec
  (package
    (name "go-github-com-cli-safeexec")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cli/safeexec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j6hspjx9kyxn98nbisawx6wvbi1d6rpzr6p2rzhllm673wibwr3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cli/safeexec"))
    (home-page "https://github.com/cli/safeexec")
    (synopsis "Safe implementation of Go's exec.Command")
    (description
     "This package provides a Go module that provides a stabler alternative to
@code{exec.LookPath()}.")
    (license license:bsd-2)))

(define-public go-github-com-client9-misspell
  (package
    (name "go-github-com-client9-misspell")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/client9/misspell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vwf33wsc4la25zk9nylpbp9px3svlmldkm0bha4hp56jws4q9cs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/client9/misspell"
      #:test-subdirs #~(list "ignore" ".")))
    (propagated-inputs (list go-github-com-gobwas-glob))
    (home-page "https://github.com/client9/misspell")
    (synopsis "Correct commonly misspelled English words in source files")
    (description
     "misspell assists with correcting commonly misspelled English words in
source files.  A neutral variety of English is used by default, but a US or UK
locale can be selected.")
    (license license:expat)))

(define-public go-github-com-cloudwego-iasm
  (package
    (name "go-github-com-cloudwego-iasm")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudwego/iasm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j9jvx6ijlr2xz3am4qrz5py68xpl8np7m7yfq9m2ilkli3ksq9x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cloudwego/iasm"))
    (native-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-klauspost-cpuid-v2
           go-github-com-knz-go-libedit
           go-nullprogram-com-x-optparse))
    (home-page "https://github.com/cloudwego/iasm")
    (synopsis "Interactive Assembler for Golang")
    (description
     "This package provides x86_64 variant of ported from a Python module
@url{https://github.com/Maratyszcza/PeachPy,PeachPy}, with some adaption to
the Go language features.")
    (license license:asl2.0)))

(define-public go-github-com-cockroachdb-crlib
  (package
    (name "go-github-com-cockroachdb-crlib")
    (version "0.0.0-20250521014800-1789bc709bcb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cockroachdb/crlib")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09757m5brzl2pgvzqwgcz5sy0wnvgl12lz991plssazmgkl98dv6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      ;; TODO: More additional packages are required to enable all tests, it's
      ;; used as source only package.
      #:test-flags
      #~(list "-skip" "TestLint")
      #:import-path "github.com/cockroachdb/crlib"))
    (home-page "https://github.com/cockroachdb/crlib")
    (synopsis "Utility library for CockroachDB")
    (description
     "This package provides general-purpose Go libraries and utilities.  It is
intended as an \"extended standard library\" and it has no external
dependencies.")
    (license license:asl2.0)))

(define-public go-github-com-cockroachdb-fifo
  (package
    (name "go-github-com-cockroachdb-fifo")
    (version "0.0.0-20240816210425-c5d0cb0b6fc0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cockroachdb/fifo")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17p2x5q7ngp0l0wswyf9816dv93ix0wljvp9cfiid9if6mr96wjp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cockroachdb/fifo"))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-golang-org-x-sync))
    (home-page "https://github.com/cockroachdb/fifo")
    (synopsis "Facilities for FIFO queueing in Golang")
    (description
     "This package provides several optimized facilities related to FIFO
queueing and rate limiting.")
    (license license:asl2.0)))

(define-public go-github-com-cockroachdb-logtags
  (package
    (name "go-github-com-cockroachdb-logtags")
    (version "0.0.0-20241215232642-bb51bb14a506")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cockroachdb/logtags")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vlbp0k365arqqxmdvdizxvmx6qpr2fqhqi5p6ini3l5zbxl5bw7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cockroachdb/logtags"))
    (home-page "https://github.com/cockroachdb/logtags")
    (synopsis "Key/Value annotations for Golang contexts")
    (description
     "This package provides a way to attach key/value annotations to a Go
@code{context.Context}.")
    (license license:asl2.0)))

(define-public go-github-com-cockroachdb-redact
  (package
    (name "go-github-com-cockroachdb-redact")
    (version "1.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cockroachdb/redact")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q6h35nrgsh2ygcvvs4ds9swwzzbh3v7414rvlpsnqq6a1kcskw0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cockroachdb/redact"))
    (home-page "https://github.com/cockroachdb/redact")
    (synopsis "Utilities to redact Golang strings for confidentiality")
    (description
     "Package redact provides facilities for separating @code{safe} and
@code{unsafe} pieces of data when logging and constructing error object.")
    (license license:asl2.0)))

(define-public go-github-com-cockroachdb-swiss
  (package
    (name "go-github-com-cockroachdb-swiss")
    (version "0.0.0-20250327203710-2932b022f6df")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cockroachdb/swiss")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v902vdngjqabqz6brkrsa26sb5x0xwa2b3986jy8ih6z7x44ib5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cockroachdb/swiss"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-aclements-go-perfevent))
    (home-page "https://github.com/cockroachdb/swiss")
    (synopsis "Golang port of Google's Swiss Table hash table")
    (description
     "This package implements Swiss Tables as described in
https://abseil.io/about/design/swisstables. It provides pseudo-randomized
iteration (iteration order will change from one iteration to the next) and
iteration stability akin to Go's builtin map if the map is mutated during
iteration.")
    (license license:asl2.0)))

(define-public go-github-com-cockroachdb-tokenbucket
  (package
    (name "go-github-com-cockroachdb-tokenbucket")
    (version "0.0.0-20250429170803-42689b6311bb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cockroachdb/tokenbucket")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "051s1y35xqnr2qxzzyqnhs4zz8knqfj6zyxgzli2c7nycbzg9nrq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cockroachdb/tokenbucket"))
    (home-page "https://github.com/cockroachdb/tokenbucket")
    (synopsis "Token bucket implementation in Golang")
    (description
     "This package provides a token bucket implementation in Golang.")
    (license license:asl2.0)))

(define-public go-github-com-code-hex-go-generics-cache
  (package
    (name "go-github-com-code-hex-go-generics-cache")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Code-Hex/go-generics-cache")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xna1fn9m35z17slf4z2f4dkc6s1hy5q41w8gf2500cl6bfid1ip"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Code-Hex/go-generics-cache"))
    (propagated-inputs (list go-golang-org-x-exp))
    (home-page "https://github.com/Code-Hex/go-generics-cache")
    (synopsis "Key:Value store/cache library written in Golang")
    (description
     "This package implements a functionality of an in-memory key:value
store/cache that is suitable for applications running on a single machine.
This in-memory cache uses @url{https://go.dev/blog/generics-proposal, Go
Generics} which is introduced in 1.18.")
    (license license:expat)))

(define-public go-github-com-containerd-btrfs-v2
  (package
    (name "go-github-com-containerd-btrfs-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/btrfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05xwni5gvg5nka1n6lbx7mah0iykz2jw7ca010r33djcn4i8r5bs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/btrfs/v2"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/containerd/btrfs")
    (synopsis "Btrfs bindings for Go")
    (description
     "Package btrfs provides bindings for working with btrfs partitions from
Go.  The Linux kernel headers are only required on compilation time, not on
run time.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-cgroups
  (package
    (name "go-github-com-containerd-cgroups")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/cgroups")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14nf5nc65vsnijaairs5v96h98y8f0sy35bpxbpmxxn4dfnz9x0y"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodule(s) with their own go.mod files and packed as
            ;; separated packages:
            ;;
            ;; - github.com/containerd/cgroups/cmd cgctl
            (delete-file-recursively "cmd")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/cgroups"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; cannot find cgroup mount destination"
                       (list "TestSystemd240"
                             ;; cannot statfs cgroup root
                             "TestCgroupType"
                             "TestCgroupv2CpuStats"
                             "TestCgroupv2MemoryStats"
                             "TestCgroupv2PSIStats"
                             "TestCgroupv2PidsStats"
                             "TestErrorsWhenUnitAlreadyExists"
                             "TestEventChanCleanupOnCgroupRemoval"
                             "TestIgnoreUnitExistsWhenPidNegativeOne"
                             "TestKill"
                             "TestMoveTo"
                             "TestSystemdCgroupCpuController"
                             "TestSystemdCgroupMemoryController"
                             "TestSystemdCgroupPSIController"
                             "TestSystemdCgroupPidsController"
                             ;; Assertion failed
                             "TestDeviceFilter_Nil"
                             "TestDeviceFilter_Privileged"
                             "TestDeviceFilter_Weird")
                       "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-go-uber-org-goleak))
    (propagated-inputs
     (list go-github-com-cilium-ebpf
           go-github-com-coreos-go-systemd-v22
           go-github-com-docker-go-units
           go-github-com-godbus-dbus-v5
           go-github-com-gogo-protobuf
           go-github-com-opencontainers-runtime-spec
           go-github-com-sirupsen-logrus
           go-golang-org-x-sys))
    (home-page "https://containerd.io/")
    (synopsis "Cgroups for Golang")
    (description
     "This package implements a functionality for creating, managing,
inspecting, and destroying cgroups.  The resources format for settings on the
cgroup uses the OCI runtime-spec found
@url{https://github.com/opencontainers/runtime-spec,here}.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-cgroups-v3
  (package
    (inherit go-github-com-containerd-cgroups)
    (name "go-github-com-containerd-cgroups-v3")
    (version "3.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/cgroups")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09fkbhkx0hmcfcym3zl0dshbhj3p692xg7d6y8pj732g64zk6v4k"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodule(s) with their own go.mod files and packed as
            ;; separated packages:
            ;;
            ;; - github.com/containerd/cgroups/cmd cgctl
            (delete-file-recursively "cmd")))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-containerd-cgroups)
       ((#:import-path _) "github.com/containerd/cgroups/v3")))
    (propagated-inputs
     (list go-github-com-cilium-ebpf
           go-github-com-containerd-log
           go-github-com-coreos-go-systemd-v22
           go-github-com-docker-go-units
           go-github-com-godbus-dbus-v5
           go-github-com-moby-sys-userns
           go-github-com-opencontainers-runtime-spec
           go-golang-org-x-sys
           go-google-golang-org-protobuf))))

(define-public go-github-com-containerd-console
  (package
    (name "go-github-com-containerd-console")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/console")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p10k6lwfxgij5a9i47dark8apffc6wn254dwj43ks8jr134854v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/console"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/containerd/console")
    (synopsis "Console package for Go")
    (description
     "This is Golang package for dealing with consoles.  It has few
dependencies and a simple API.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-containerd
  (package
    (name "go-github-com-containerd-containerd")
    (version "1.7.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/containerd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jwrdiglhqxccmmiq33li3myrb409mvskvcghli411fs9r46lxm3"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)

    ;; For the forgejo-runner build, we only need the pkg/userns module which
    ;; doesn't require the massive dependency tree for all of containerd, so
    ;; we cheat and skip the build and avoid having to package a large number
    ;; of additional dependencies.
    (arguments
     (list
      #:import-path "github.com/containerd/containerd"
      #:skip-build? #t
      #:tests? #f))
    ;; (propagated-inputs
    ;;  (list go-tags-cncf-io-container-device-interface
    ;;        go-k8s-io-utils
    ;;        go-k8s-io-klog-v2
    ;;        go-k8s-io-cri-api
    ;;        go-k8s-io-component-base
    ;;        go-k8s-io-client-go
    ;;        go-k8s-io-apiserver
    ;;        go-k8s-io-apimachinery
    ;;        go-k8s-io-api
    ;;        go-google-golang-org-protobuf
    ;;        go-google-golang-org-grpc
    ;;        go-google-golang-org-genproto-googleapis-rpc
    ;;        go-google-golang-org-genproto
    ;;        go-golang-org-x-sys
    ;;        go-golang-org-x-sync
    ;;        go-golang-org-x-net
    ;;        go-go-opentelemetry-io-otel-trace
    ;;        go-go-opentelemetry-io-otel-sdk
    ;;        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
    ;;        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
    ;;        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
    ;;        go-go-opentelemetry-io-otel
    ;;        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
    ;;        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
    ;;        go-go-etcd-io-bbolt
    ;;        go-github-com-vishvananda-netlink
    ;;        go-github-com-urfave-cli
    ;;        go-github-com-tchap-go-patricia-v2
    ;;        go-github-com-stretchr-testify
    ;;        go-github-com-sirupsen-logrus
    ;;        go-github-com-prometheus-client-golang
    ;;        go-github-com-pelletier-go-toml
    ;;        go-github-com-opencontainers-selinux
    ;;        go-github-com-opencontainers-runtime-tools
    ;;        go-github-com-opencontainers-runtime-spec
    ;;        go-github-com-opencontainers-image-spec
    ;;        go-github-com-opencontainers-go-digest
    ;;        go-github-com-moby-sys-userns
    ;;        go-github-com-moby-sys-user
    ;;        go-github-com-moby-sys-symlink
    ;;        go-github-com-moby-sys-signal
    ;;        go-github-com-moby-sys-sequential
    ;;        go-github-com-moby-sys-mountinfo
    ;;        go-github-com-moby-locker
    ;;        go-github-com-minio-sha256-simd
    ;;        go-github-com-klauspost-compress
    ;;        go-github-com-intel-goresctrl
    ;;        go-github-com-grpc-ecosystem-go-grpc-prometheus
    ;;        go-github-com-grpc-ecosystem-go-grpc-middleware
    ;;        go-github-com-google-uuid
    ;;        go-github-com-google-go-cmp
    ;;        go-github-com-fsnotify-fsnotify
    ;;        go-github-com-emicklei-go-restful-v3
    ;;        go-github-com-docker-go-units
    ;;        go-github-com-docker-go-metrics
    ;;        go-github-com-docker-go-events
    ;;        go-github-com-distribution-reference
    ;;        go-github-com-davecgh-go-spew
    ;;        go-github-com-coreos-go-systemd-v22
    ;;        go-github-com-containernetworking-plugins
    ;;        go-github-com-containernetworking-cni
    ;;        go-github-com-containerd-zfs
    ;;        go-github-com-containerd-typeurl-v2
    ;;        go-github-com-containerd-ttrpc
    ;;        go-github-com-containerd-platforms
    ;;        go-github-com-containerd-nri
    ;;        go-github-com-containerd-log
    ;;        go-github-com-containerd-imgcrypt
    ;;        go-github-com-containerd-go-runc
    ;;        go-github-com-containerd-go-cni
    ;;        go-github-com-containerd-fifo
    ;;        go-github-com-containerd-errdefs
    ;;        go-github-com-containerd-continuity
    ;;        go-github-com-containerd-containerd-api
    ;;        go-github-com-containerd-console
    ;;        go-github-com-containerd-cgroups-v3
    ;;        go-github-com-containerd-btrfs-v2
    ;;        go-github-com-containerd-aufs
    ;;        go-github-com-microsoft-hcsshim
    ;;        go-github-com-microsoft-go-winio
    ;;        go-github-com-adamkorcz-go-118-fuzz-build
    ;;        go-github-com-adalogics-go-fuzz-headers
    ;;        go-dario-cat-mergo))
    (home-page "https://github.com/containerd/containerd")
    (synopsis "Container runtime support daemon")
    (description
     "containerd is a container runtime with an emphasis on simplicity,
robustness, and portability.  It is available as a daemon, which can manage
the complete container lifecycle of its host system: image transfer and
storage, container execution and supervision, low-level storage and network
attachments, etc.")
    (license license:asl2.0)
    ;; Don't expose since it's a partial package.
    (properties '((hidden? . #t)))))

(define-public go-github-com-containerd-continuity
  (package
    (name "go-github-com-containerd-continuity")
    (version "0.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containerd/continuity")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01p5cqc0lvv6z5m0w23xq38fmc86k490wvylng5sfn90zplgjrwi"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/containerd/continuity/cmd/continuity
            (delete-file-recursively "cmd/continuity")
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/continuity"))
    (propagated-inputs
     (list go-github-com-containerd-log
           ;; go-github-com-microsoft-go-winio ; for Windows only
           go-github-com-opencontainers-go-digest
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-google-golang-org-protobuf))
    (home-page "https://github.com/containerd/continuity")
    (synopsis "Transport-agnostic, filesystem metadata manifest system")
    (description
     "This package provides a transport-agnostic, filesystem metadata manifest system.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-errdefs
  (package
    (name "go-github-com-containerd-errdefs")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/errdefs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0afaljkkd388f6igr3f2vjnd14yr8h20fcfzglw8j5q1q7a1cvik"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/containerd/errdefs/pkg
            (delete-file-recursively "pkg")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/errdefs"))
    (home-page "https://github.com/containerd/errdefs")
    (synopsis "Common definition and library of errors used by containerd")
    (description
     "Package errdefs defines the common errors used throughout containerd
packages.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-errdefs-pkg
  (package
    (name "go-github-com-containerd-errdefs-pkg")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containerd/errdefs")
              (commit (go-version->git-ref version
                                           #:subdir "pkg"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0afaljkkd388f6igr3f2vjnd14yr8h20fcfzglw8j5q1q7a1cvik"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            ;; Consider to implement it as re-usable procedure in
            ;; guix/build/utils or guix/build-system/go.
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "pkg")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/containerd/errdefs/pkg"
      #:unpack-path "github.com/containerd/errdefs"))
    (propagated-inputs
     (list go-github-com-containerd-errdefs
           go-github-com-containerd-typeurl-v2
           go-google-golang-org-genproto-googleapis-rpc
           go-google-golang-org-grpc
           go-google-golang-org-protobuf))
    (home-page "https://github.com/containerd/errdefs")
    (synopsis "Addintional error handling modules for containerd")
    (description
     "This package provides an additinal Golang modules for error handling in
containerd projects.

@itemize
@item errgrpc - provides utility functions for translating errors to and from
a gRPC context
@item errhttp - provides utility functions for translating errors to and from
a HTTP context
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-containerd-fifo
  (package
    (name "go-github-com-containerd-fifo")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/fifo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ddb1spairbsjkvxqysa7pzb5za07dvv1aay3mqr160gh2za3kd4"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/containerd/fifo"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/containerd/fifo")
    (synopsis "FIFO package for Golang")
    (description
     "This package implements a functionality of handling FIFOs in a sane
way.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-go-runc
  (package
    (name "go-github-com-containerd-go-runc")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containerd/go-runc")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03f6a44j24g64x0zwx6daqbssbka0wcvj3fkjz4rvqx5dz3n7xhf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/go-runc"
      #:test-flags #~(list "-skip" "TestRuncStarted")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs import-path #:allow-other-keys)
              (substitute* (string-append "src/" import-path "/runc_test.go")
                (("Command: \"/bin/true\",")
                 (string-append "Command: \""
                                (search-input-file inputs "/bin/true")
                                "\",\n"))
                (("Command: \"/bin/false\",")
                 (string-append "Command: \""
                                (search-input-file inputs "/bin/false")
                                "\",\n")))
              (substitute* (string-append "src/" import-path "/runc.go")
                (("return -1, err")
                 "fmt.Errorf(\"Achou\")\n  return -1, err")))))))
    (inputs
     (list coreutils))
    (propagated-inputs
     (list go-github-com-containerd-console
           go-github-com-opencontainers-runtime-spec
           go-github-com-sirupsen-logrus
           go-golang-org-x-sys))
    (home-page "https://github.com/containerd/go-runc")
    (synopsis "Runc bindings for Golang")
    (description
     "This package implements a functionality for consuming the @code{runc}
 binary in Go applications.  It tries to expose all the settings and features
of the @code{runc} CLI.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-log
  (package
    (name "go-github-com-containerd-log")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/log")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nzviiqpn0djdwql2q3m2xs1ndxyd0v2klvq6xi2r0dn4wr3mqdy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/log"))
    (propagated-inputs (list go-github-com-sirupsen-logrus))
    (home-page "https://github.com/containerd/log")
    (synopsis "Common log interface for containerd repositories and clients")
    (description
     "Package log provides types and functions related to logging, passing
loggers through a context, and attaching context to the logger.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-platforms
  (package
    (name "go-github-com-containerd-platforms")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/platforms")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03clc3b2fvlym5d2rvgima0p0br1m34p8gs7rn6y5rp7v7z601sx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/platforms"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-containerd-log
           go-github-com-opencontainers-image-spec
           go-golang-org-x-sys))
    (home-page "https://github.com/containerd/platforms")
    (synopsis "Handling container platform type")
    (description
     "Package platforms provides a toolkit for normalizing, matching and
specifying container platforms.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-plugin
  (package
    (name "go-github-com-containerd-plugin")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/plugin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gzm8h4yaparji0llqqfxl68gv56hwlybz4rgwnr54fhr029mpzp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/plugin"))
    (propagated-inputs
     (list go-github-com-opencontainers-image-spec))
    (home-page "https://github.com/containerd/plugin")
    (synopsis "Registering and managing typed plugins with dependencies")
    (description
     "This package provides a common plugin interface across containerd
repositories.")
    (license license:asl2.0)))

(define-public go-github-com-coocood-freecache
  (package
    (name "go-github-com-coocood-freecache")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coocood/freecache")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iw0s07qy8g1lncwl524c524wh56djl0vn6i3bm91cnwzav7ihjl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/coocood/freecache"))
    (propagated-inputs (list go-github-com-cespare-xxhash-v2))
    (home-page "https://github.com/coocood/freecache")
    (synopsis "Caching library for Go")
    (description
     "This library provides caching capabilities for Go with no garbage
collection overhead and high concurrent performance.  An unlimited number of
objects can be cached in memory without increased latency or degraded
throughput.")
    (license license:expat)))

(define-public go-github-com-coreos-go-semver
  (package
    (name "go-github-com-coreos-go-semver")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coreos/go-semver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vs04yykv1bwgvbyvi1m7ps83w06wzplw4giw8jac2iidx0x74v5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/coreos/go-semver"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v3))
    (home-page "https://github.com/coreos/go-semver/")
    (synopsis "Semantic versioning library")
    (description
     "@code{go-semver} is a semantic versioning library for Go.  It lets you
parse and compare two semantic version strings.")
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-systemd-v22
  (package
    (name "go-github-com-coreos-go-systemd-v22")
    (version "22.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coreos/go-systemd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vhb4cw8nw9nx8mprx829xv8w4jnwhc2lcyjljzlfafsn8nx5nyf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/coreos/go-systemd/v22"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-sdjournal-header
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "sdjournal/journal.go"
                  (("systemd/sd-journal.h") "elogind/sd-journal.h")
                  (("systemd/sd-id128.h") "elogind/sd-id128.h")))))
          ;; XXX: Activate when go-build-system supports submodules.
          (delete 'build)
          (add-before 'check 'remove-failing-test-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (list
                           ;; dial unix /var/run/dbus/system_bus_socket: connect: no such
                           ;; file or directory
                           "dbus/dbus_test.go"
                           "dbus/methods_test.go"
                           "dbus/subscription_set_test.go"
                           "dbus/subscription_test.go"
                           "import1/dbus_test.go"
                           "login1/dbus_test.go"
                           "machine1/dbus_test.go"
                           ;; journal_test.go:30: journald socket not detected
                           "journal/journal_test.go"
                           ;; exec: "systemd-run": executable file not found
                           ;; in $PATH
                           "journal/journal_unix_test.go"
                           ;; Error opening journal: unable to open a handle
                           ;; to the library
                           "sdjournal/journal_test.go"
                           ;; Error getting an existing function: unable to
                           ;; open a handle to the library
                           "sdjournal/functions_test.go")))))
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (inputs
     (list elogind))
    (propagated-inputs
     (list go-github-com-godbus-dbus-v5))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "Go bindings to systemd")
    (description
     "This package implements a various systemd bindings and provides Golang
submodules:

@itemize
@item @code{activation} - for writing and using socket activation from Go
@item @code{daemon} - for notifying systemd of service status changes
@item @code{dbus} - for starting/stopping/inspecting running services and units
@item @code{journal} - for writing to systemd's logging service, journald
@item @code{sdjournal} - for reading from journald by wrapping its C API
@item @code{login1} - for integration with the systemd logind API
@item @code{machine1} - for registering machines/containers with systemd
@item @code{unit} - for (de)serialization and comparison of unit files
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-cosiner-argv
  (package
    (name "go-github-com-cosiner-argv")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cosiner/argv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ard8655lr4rqd929pvn9phv4mbgzrl3rswcl6i7p97cls7gn2yc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cosiner/argv"))
    (home-page "https://github.com/cosiner/argv")
    (synopsis "Split command line string into arguments array")
    (description
     "Package argv parses command line string into arguments array using the
bash syntax.")
    (license license:expat)))

(define-public go-github-com-couchbase-gomemcached
  (package
    (name "go-github-com-couchbase-gomemcached")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/couchbase/gomemcached")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "132zjbr7d586gb1wqlnhg3vgyshq629z1wsskrpbmyypjfkq620c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/couchbase/gomemcached"
      #:test-flags #~(list "-skip" "TestEncodingResponse")
      #:test-subdirs #~(list ".")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pkg-errors))
    (home-page "https://github.com/couchbase/gomemcached")
    (synopsis "Memcached binary protocol toolkit for go")
    (description
     "This package provides memcache client and server functionality.")
    (license license:expat)))

(define-public go-github-com-cowsql-go-cowsql
  (package
    (name "go-github-com-cowsql-go-cowsql")
    (version "1.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cowsql/go-cowsql")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0my26gndkzzhmkp5n4x2akc6h9572y4lpn9ryw7imdhd958mpjrx"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; TODO: Package <https://github.com/cowsql/cowsql> for the full
      ;; featured binding support, using as source only as a client code.
      #:skip-build? #t
      #:test-subdirs #~(list "logging")         ;minimal tests
      #:import-path "github.com/cowsql/go-cowsql"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-renameio
           go-github-com-mattn-go-sqlite3
           go-github-com-peterh-liner
           go-github-com-pkg-errors
           go-github-com-rican7-retry
           go-github-com-spf13-cobra
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/cowsql/go-cowsql")
    (synopsis "Golang bindings for libcowsql")
    (description
     "This package provides bindings for the @url{cowsql,
https://github.com/cowsql/cowsql} C library and a pure Golang client for the
@url{cowsql wire protocol,
https://github.com/cowsql/cowsql/blob/main/doc/protocol.md}.")
    (license license:asl2.0)))

(define-public go-github-com-cpuguy83-dockercfg
  (package
    (name "go-github-com-cpuguy83-dockercfg")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cpuguy83/dockercfg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "055gxyq0wvyr9lap6rd49ijyg846mcpd1kwx9w69qj0pszvh2v96"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cpuguy83/dockercfg"))
    (home-page "https://github.com/cpuguy83/dockercfg")
    (synopsis "Library to load Docker CLI configs")
    (description
     "Go library to load docker CLI configs, auths, etc.  with minimal deps.
  So far the only deps are on the stdlib.")
    (license license:expat)))

(define-public go-github-com-cpuguy83-go-md2man-v2
  (package
    (name "go-github-com-cpuguy83-go-md2man-v2")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cpuguy83/go-md2man")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gqlkv1pv8cpvcj8g77d1hzy5bnp5a3k3xs02iahlr3a65m4azsi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/cpuguy83/go-md2man/v2"))
    (propagated-inputs
     (list go-github-com-russross-blackfriday-v2))
    (home-page "https://github.com/cpuguy83/go-md2man")
    (synopsis "Convert markdown into roff")
    (description
     "Go-md2man is a Go program that converts markdown to roff for the purpose
of building man pages.")
    (license license:expat)))

(define-public go-github-com-crackcomm-go-gitignore
  (package
    (name "go-github-com-crackcomm-go-gitignore")
    (version "0.0.0-20241020182519-7843d2ba8fdf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/crackcomm/go-gitignore")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vabnacz1bk2lvln3bjg4i6wj1lsb6pxy55xzkjp8wdhd8gmk47b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/crackcomm/go-gitignore"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/crackcomm/go-gitignore")
    (synopsis "Gitignore parser for Golang")
    (description
     "ignore is a library which returns a new ignorer object which can test
against various paths.  This is particularly useful when trying to filter
files based on a .gitignore document.")
    (license license:expat)))

(define-public go-github-com-creack-pty
  (package
    (name "go-github-com-creack-pty")
    (version "1.1.24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/creack/pty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yy4zhfb7vrrbwd13rcw0zzcq0ami3zv3hp0x7g7il6mrbadcf25"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/creack/pty"
      #:modules '((ice-9 popen)
                  (ice-9 textual-ports)
                  (guix build go-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'regenerate-types
            (lambda* (#:key import-path #:allow-other-keys)
              ;; Generated files are included (ztypes_*). We need to remake
              ;; them with Cgo.
              (with-directory-excursion (string-append "src/" import-path)
                (let* ((go-arch
                        #$(car (go-target
                                (or (%current-target-system)
                                    (nix-system->gnu-triplet (%current-system))))))
                       (file (string-append "ztypes_" go-arch ".go"))
                       (pipe (open-input-pipe "go tool cgo -godefs types.go"))
                       (text (get-string-all pipe)))
                  (close-pipe pipe)
                  (for-each delete-file
                            (find-files (getcwd) (file-name-predicate
                                                  "ztypes_[a-zA-Z0-9_]+.go")))
                  (call-with-output-file file
                    (lambda (port)
                      (display text port))))))))))
    (home-page "https://github.com/creack/pty")
    (synopsis "Pseudoterminal handling in Go")
    (description
     "The pty package provides functions for working with Unix pseudoterminals.")
    (license license:expat)))

(define-public go-github-com-creasty-defaults
  (package
    (name "go-github-com-creasty-defaults")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/creasty/defaults")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1avbm47ghqc6hiafv0c61mzrw9rajgszjyqh4yww916fqzaw8li3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/creasty/defaults"))
    (home-page "https://github.com/creasty/defaults")
    (synopsis "Initialize structs with default values")
    (description
     "This package implements functionality to initialize structs with default
values.  It supports almost all kind of types: @code{int/8/16/32/64},
@code{uint/8/16/32/64}, @code{float32/64}, @code{uintptr}, @code{bool},
@code{string}, @code{map}, @code{slice}, @code{struct},
@code{f,map[K1]map[K2]Struct}, @code{}[]map[K1]Struct[]},
@code{time.Duration}, @code{*SampleStruct}, and @code{*int}")
    (license license:expat)))

(define-public go-github-com-cskr-pubsub
  (package
    (name "go-github-com-cskr-pubsub")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cskr/pubsub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wr8cg5axrlz9xg33r9dqvkp5ix9q8h8c7qw78mj22qprwh3zj9f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cskr/pubsub"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-example
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file "example_test.go")))))))
    (home-page "https://github.com/cskr/pubsub")
    (synopsis "Simple pubsub package for go")
    (description
     "Package @code{pubsub} implements a simple multi-topic pub-sub library.")
    (license license:bsd-2)))

(define-public go-github-com-cskr-pubsub-v2
  (package
    (inherit go-github-com-cskr-pubsub)
    (name "go-github-com-cskr-pubsub-v2")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cskr/pubsub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iy85nxrfv6hp4i4mnqayjfx4hci7qyycqbaz4fx8wbd15n9ll66"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cskr/pubsub/v2"))))

(define-public go-github-com-cyberdelia-go-metrics-graphite
  (package
    ;; No release, see
    ;; <https://github.com/cyberdelia/go-metrics-graphite/issues/17>.
    (name "go-github-com-cyberdelia-go-metrics-graphite")
    (version "0.0.0-20161219230853-39f87cc3b432")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cyberdelia/go-metrics-graphite")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nnpwryw8i110laffyavvhx38gcd1jnpdir69y6fxxzpx06d094w"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-github-com-rcrowley-go-metrics))
    (arguments
     '(#:tests? #f ; Tests require network interface access
       #:import-path "github.com/cyberdelia/go-metrics-graphite"))
    (home-page "https://github.com/cyberdelia/go-metrics-graphite")
    (synopsis "Graphite client for go-metrics")
    (description
     "This package provides a reporter for the
@url{https://github.com/rcrowley/go-metrics,go-metrics} library which posts
metrics to Graphite.")
    (license license:bsd-2)))

(define-public go-github-com-cyphar-filepath-securejoin
  (package
    (name "go-github-com-cyphar-filepath-securejoin")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cyphar/filepath-securejoin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cyqbxmrn3qgq8q0v7xmm9knc8nr60s017yrhkghcwg4yqqpmr9l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags #~(list "-timeout=30m" "-shuffle=on" "-v")
      #:import-path "github.com/cyphar/filepath-securejoin"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/cyphar/filepath-securejoin")
    (synopsis "Alternative Golang @code{filepath.SecureJoin} implementation")
    (description
     "Package securejoin is an implementation of the
hopefully-soon-to-be-included @code{SecureJoin} helper that is meant to be
part of the \"path/filepath\" package.  The purpose of this project is to
provide a @code{PoC} implementation to make the @code{SecureJoin} proposal
(@url{https://github.com/golang/go/issues/20126,https://github.com/golang/go/issues/20126})
more tangible.")
    (license license:bsd-3)))

(define-public go-github-com-d4l3k-messagediff
  (package
    (name "go-github-com-d4l3k-messagediff")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/d4l3k/messagediff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "104hl8x57ciaz7mzafg1vp9qggxcyfm8hsv9bmlihbz9ml3nyr8v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/d4l3k/messagediff"))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/d4l3k/messagediff")
    (synopsis "Diff arbitrary Go structs")
    (description
     "Messagediff is a library for calculating diffs of arbitrary
structs in the Go programming language.")
    (license license:expat)))

(define-public go-github-com-d5-tengo-v2
  (package
    (name "go-github-com-d5-tengo-v2")
    (version "2.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/d5/tengo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12h7fg2hj9s64hzsv5mz0pl9q1hf1lw3b5k9fr40nfqlq1bw84da"))))
    (build-system go-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      ;; See <https://github.com/d5/tengo/issues/466>.
      #:go go-1.23
      #:import-path "github.com/d5/tengo/v2"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-doc
            (lambda* (#:key import-path outputs #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (let* ((data (string-append #$output:doc "/share"))
                       (doc (string-append data "/doc/" #$name "-" #$version)))
                  (copy-recursively "docs/" doc))))))))
    (home-page "https://github.com/d5/tengo")
    (synopsis "Script language for Go")
    (description
     "Tengo is a small, dynamic, fast, secure script language for Go.
Features:
@itemize
@item simple and highly readable syntax
@item dynamic typing with type coercion
@item higher-order functions and closures
@item immutable values
@item securely embeddable and extensible
@item compiler/runtime written in native Go (no external deps or cgo)
@item executable as a standalone language/REPL
@item use cases: rules engine, state machine, data pipeline, transpiler
@end itemize")
    (license license:expat)))

(define-public go-github-com-danieljoos-wincred
  (package
    (name "go-github-com-danieljoos-wincred")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danieljoos/wincred")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cgf74srid92gzkd094mwp0jvakgi0a22a8hpl7v9w28a9d61bf3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/danieljoos/wincred"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/danieljoos/wincred")
    (synopsis "Go interface to Windows Credentials Management")
    (description
     "Package wincred provides primitives for accessing the Windows Credentials
Management API. This includes functions for retrieval, listing and storage of
credentials as well as Go structures for convenient access to the credential
data.")
    (license license:expat)))

(define-public go-github-com-dannav-hhmmss
  (package
    (name "go-github-com-dannav-hhmmss")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dannav/hhmmss")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h2wdpd5sd2wfd5d2vyqiwlrqlxf3qwpqjy74hbcr7bhjpgv81m0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dannav/hhmmss"))
    (home-page "https://github.com/dannav/hhmmss")
    (synopsis "Parse HHMMSS strings into a Go time.Duration type")
    (description
     "Package @code{hhmmss} manages converting HH:MM:SS time strings to
@code{time.Duration} values.")
    (license license:asl2.0)))

(define-public go-github-com-danwakefield-fnmatch
  (let ((commit "cbb64ac3d964b81592e64f957ad53df015803288")
        (revision "0"))
    (package
      (name "go-github-com-danwakefield-fnmatch")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/danwakefield/fnmatch")
               (commit commit)))
         (sha256
          (base32 "0cbf511ppsa6hf59mdl7nbyn2b2n71y0bpkzbmfkdqjhanqh1lqz"))
         (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/danwakefield/fnmatch"))
      (home-page "https://github.com/danwakefield/fnmatch")
      (synopsis "Updated clone of kballards golang fnmatch gist")
      (description
       "This package provides string-matching based on BSD fnmatch.3.  It is an
updated clone of kballards golang fnmatch
gist (https://gist.github.com/kballard/272720).")
      (license license:bsd-2))))

(define-public go-github-com-darccio-mergo
  (hidden-package
   (package/inherit go-dario-cat-mergo
     (name "go-github-com-darccio-mergo")
     (arguments
      (list
       #:go go-1.23
       #:import-path "github.com/darccio/mergo"
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'fix-import-path
             (lambda* (#:key tests? import-path #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 (substitute* (find-files "." "\\.go$")
                   (("dario.cat/mergo") import-path)))))))))))

(define-public go-github-com-dave-jennifer
  (package
    (name "go-github-com-dave-jennifer")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dave/jennifer")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a0zg8cdnhyqfgrz7jbgpnnz75g5ps1c8cnmbxvfldmy973ziaml"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dave/jennifer"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/dave/jennifer")
    (synopsis "Code generator for Go")
    (description "This package provides functionality to generate Go code.")
    (license license:expat)))

(define-public go-github-com-daviddengcn-go-colortext
  (package
    (name "go-github-com-daviddengcn-go-colortext")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/daviddengcn/go-colortext")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j5ldwg3a768d3nniiglghr9axj4p87k7f7asqxa1a688xvcms48"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/daviddengcn/go-colortext"))
    (native-inputs
     (list go-github-com-golangplus-testing))
    (home-page "https://github.com/daviddengcn/go-colortext")
    (synopsis "Change the color of console text and background")
    (description
     "This is a package to change the color of the text and background in the
console, working both under Windows and other systems.  Under Windows, the
console APIs are used.  Otherwise, ANSI texts are output.")
    ;; dual-licensed
    (license (list license:bsd-3 license:expat))))

(define-public go-github-com-dbaggerman-cuba
  (package
    (name "go-github-com-dbaggerman-cuba")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dbaggerman/cuba")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sbria32fh2bzc8agnm9p5id5z15mrqj4fyxhnkq05bh2qjkrwc7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dbaggerman/cuba"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-karrick-godirwalk))
    (home-page "https://github.com/dbaggerman/cuba")
    (synopsis "Goroutine parallelism library")
    (description
     "This package provides a library for Goroutines that helps to implement
more complicated parallel cases.")
    (license license:expat)))

(define-public go-github-com-dchest-stemmer
  (package
    (name "go-github-com-dchest-stemmer")
    (version "0.0.0-20161207102402-66719a20c4b5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dchest/stemmer")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r2rz8ynmcrkwjk8sq10n6jgxmkfqf2wqy8nvgriyww41pfq5fgn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dchest/stemmer"))
    (home-page "https://github.com/dchest/stemmer")
    (synopsis "Stemmer package for Golang")
    (description
     "Stemmer package provides an interface for stemmers and includes English,
German and Dutch stemmers as sub-packages.")
    (license license:bsd-2)))

(define-public go-github-com-deckarep-golang-set
  (package
    (name "go-github-com-deckarep-golang-set")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/deckarep/golang-set")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y64c0p6a7ww5jp6adm6fm97vsni86njw8wkwxfmciy466vhl0lf"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/deckarep/golang-set"))
    (home-page "https://github.com/deckarep/golang-set")
    (synopsis "Set type for Go")
    (description
     "Set is the set collection for the Go language.")
    (license license:expat)))

(define-public go-github-com-deckarep-golang-set-v2
  (package
    (inherit go-github-com-deckarep-golang-set)
    (name "go-github-com-deckarep-golang-set-v2")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deckarep/golang-set")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yvmra0hfcdzyyw51k0gnnd13zklvsacja0qkd0j1wjhpqmmfbcy"))))
    (arguments
     (list
      #:import-path "github.com/deckarep/golang-set/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-elliotchance-orderedmap-v2))))

(define-public go-github-com-delthas-go-libnp
  (let ((commit "96674b98150ed492b535d61dde5767dfa2dd14ce")
        (revision "1"))
    (package
      (name "go-github-com-delthas-go-libnp")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/delthas/go-libnp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1l2p2mpspjaffninxvghjsfywr39cravfbzpxyiq62lfpw43zwaq"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/delthas/go-libnp"))
      (propagated-inputs
       (list go-github-com-godbus-dbus-v5))
      (home-page "https://github.com/delthas/go-libnp")
      (synopsis "Tiny library providing information about now-playing media")
      (description
       "@code{go-libnp} is a tiny cross-platform library for extracting
information about the music/image/video that is Now Playing on the system.")
      (license license:expat))))

(define-public go-github-com-delthas-go-localeinfo
  (package
    (name "go-github-com-delthas-go-localeinfo")
    (version "0.0.0-20240813094314-e5413e186769")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/delthas/go-localeinfo")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nw21gv3j7cc9x4gq4avlg4s7xzgc7gxrkmq4v451zrvx0mnv0bn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/delthas/go-localeinfo"))
    (home-page "https://github.com/delthas/go-localeinfo")
    (synopsis "Library for extracting locale information")
    (description
     "@code{go-localeinfo} extracts monetary/numeric/time
formatting information, rather than the current locale name.")
    (license license:expat)))

(define-public go-github-com-denisbrodbeck-machineid
  (package
    (name "go-github-com-denisbrodbeck-machineid")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/denisbrodbeck/machineid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "075rqb2f9hla9jwc6823jkkb3xcv6azz3phndbssssn2dps07cib"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/denisbrodbeck/machineid"
      #:test-flags
      #~(list "-vet=off" ;Go@1.24 forces vet, but tests are not ready yet.
              ;; id_test.go:8: machineid: open /etc/machine-id: no such file
              ;; or directory
              "-skip" "TestID|TestProtectedID")))
    (home-page "https://github.com/denisbrodbeck/machineid")
    (synopsis "Read the unique machine ID of most host OS's")
    (description
     "This package implements functionality for reading the unique machine
ID (@code{/etc/machine-id}) of most OSs (without admin privileges).")
    (license license:expat)))

(define-public go-github-com-dennwc-btrfs
  (package
    (name "go-github-com-dennwc-btrfs")
    (version "0.0.0-20241002142654-12ae127e0bf6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dennwc/btrfs")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0afc1clhzzhvshxp6xqs4d1hq681pzm85fqwjdgs9yh0j5bxjnhg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dennwc/btrfs"
      ;; Tests fail on 32bit
      #:tests? (target-64bit?)
      #:test-flags
      ;; Tests require "mount" and "mkfs.btrfs" in the PATH.
      #~(list "-skip"
              (string-join
               (list "TestCloneFile"
                     "TestCompression"
                     "TestIsSubvolume"
                     "TestOpen"
                     "TestResize"
                     "TestSubvolumes")
               "|"))))
    (native-inputs
     (list go-github-com-spf13-cobra)) ; for CLI
    (propagated-inputs (list go-github-com-dennwc-ioctl))
    (home-page "https://github.com/dennwc/btrfs")
    (synopsis "Btrfs library in a pure Golang")
    ;; XXX: Projects lacks README or any other documentation describing the
    ;; functionality.
    (description
     "This package implements Btrfs functionality in a pure Go.")
    (license license:asl2.0)))

(define-public go-github-com-dennwc-ioctl
  (package
    (name "go-github-com-dennwc-ioctl")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dennwc/ioctl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01b5d6fywrzs0q4q1j2c3qb15hwslb405w7bbc54w4qav0f61l55"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dennwc/ioctl"))
    (home-page "https://github.com/dennwc/ioctl")
    (synopsis "Golang implementation of ioctl")
    (description
     "This package provides @code{ioctl} wrapper to std @code{syscall} and
@code{os}.")
    (license license:expat)))

(define-public go-github-com-dennwc-varint
  (package
    (name "go-github-com-dennwc-varint")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dennwc/varint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w6fnh7i55155cv55cjdqq436zb2y08rglxvz58vv67bb4hj7dkk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dennwc/varint"
      ;; XXX: varint_test.go:94: unexpected error: -11.
      #:test-flags
      #~(list "-skip" "TestUvarint/overflow|TestUvarint/overflow_short")))
    (home-page "https://github.com/dennwc/varint")
    (synopsis "Fast varint library for Golang")
    (description
     "This package provides an optimized implementation of protobuf's varint
encoding/decoding.  It has no dependencies.")
    (license license:expat)))

(define-public go-github-com-derekparker-trie
  (package
    (name "go-github-com-derekparker-trie")
    (version "0.0.0-20230829180723-39f4de51ef7d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/derekparker/trie")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ik8xsxm7bd12lycga6d0zw561axmdwdqxi5qbf39n7mw41l9vj2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/derekparker/trie"))
    (home-page "https://github.com/derekparker/trie")
    (synopsis "Prefix/fuzzy string searching in Golang")
    (description "Implementation of an R-Way Trie data structure.")
    (license license:expat)))

(define-public go-github-com-derekparker-trie-v3
  (package
    (inherit go-github-com-derekparker-trie)
    (name "go-github-com-derekparker-trie-v3")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/derekparker/trie")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02br0cw2wh27xffs1hsbwh145d3vpaihcd7mygf36ihdhrp00pka"))))
    (arguments
     (list
      #:import-path "github.com/derekparker/trie/v3"))))

(define-public go-github-com-detailyang-go-fallocate
  (package
    (name "go-github-com-detailyang-go-fallocate")
    (version "0.0.0-20180908115635-432fa640bd2e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/detailyang/go-fallocate")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yvgkj5i43ihagdwg70vjgrhwf2mjsbyv2xwd6znsdwav6qpcdnw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/detailyang/go-fallocate"))
    (home-page "https://github.com/detailyang/go-fallocate")
    (synopsis "Syscal @code{fallocate} implementation in Golang")
    (description
     "This package implements syscal @code{fallocate} by wrapping standard Go
@code{os.SEEK_SET}.")
    (license license:expat)))

(define-public go-github-com-dgraph-io-badger
  (package
    (name "go-github-com-dgraph-io-badger")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgraph-io/badger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y37di0da6jciv1mwj0d8kv2xf56sribmwbcmd6ma65c5h6mdch9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dgraph-io/badger"
      #:test-flags
      #~(list "-skip"
              ;; Test fails with error: assertion is not equal.
              "TestBuildKeyValueSizeHistogram/All_same_size_key-values")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-failing-tests
            (lambda* (#:key unpack-path tests? #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (substitute* (find-files "." "histogram_test.go$")
                  ;; conversion from int64 to string yields a string of one
                  ;; rune, not a string of digits (did you mean
                  ;; fmt.Sprint(x)?).
                  ;; See: <https://github.com/dgraph-io/badger/issues/2103>.
                  (("\"testing\"") (string-append "\"testing\"\n\"fmt\""))
                  (("string") "fmt.Sprint"))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-andreasbriese-bbloom
           go-github-com-dgraph-io-ristretto
           go-github-com-dustin-go-humanize
           go-github-com-golang-protobuf
           go-github-com-pkg-errors
           go-github-com-spf13-cobra
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://dgraph.io/docs/badger")
    (synopsis "Key-value database in Golang")
    (description
     "BadgerDB implements an embeddable, key-value (KV) database, written in
pure Go.  It is designed to be highly performant for both reads and writes
simultaneously.  It uses @acronym{Multi-Version Concurrency Control, MVCC},
supports concurrent serializable transactions.")
    (license license:asl2.0)))

(define-public go-github-com-dgraph-io-badger-v4
  (package
    (inherit go-github-com-dgraph-io-badger)
    (name "go-github-com-dgraph-io-badger-v4")
    (version "4.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgraph-io/badger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v4c69whypm3k40hrx8bw9rjrcb0swz43v056s0fadqr04j0ncwj"))))
    (arguments
     (list
      #:go go-1.23
      #:tests? #f ; TODO: tests hang, find out why.
      #:import-path "github.com/dgraph-io/badger/v4"))
    (propagated-inputs
     (list go-github-com-cespare-xxhash-v2
           go-github-com-dgraph-io-ristretto-v2
           go-github-com-dustin-go-humanize
           go-github-com-google-flatbuffers
           go-github-com-klauspost-compress
           go-github-com-pkg-errors
           go-github-com-spf13-cobra
           go-go-opencensus-io
           go-golang-org-x-net
           go-golang-org-x-sys
           go-google-golang-org-protobuf))))

(define-public go-github-com-dgraph-io-ristretto
  (package
    (name "go-github-com-dgraph-io-ristretto")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgraph-io/ristretto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lp6plhiskdpka44qlcw1x90nknccnkj1bnmxyxhzm8knx8c5yvw"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Tests fail on 32 bit architecture:
      ;;
      ;; cannot use 12 << 30 (untyped int constant 12884901888) as int value
      ;; in assignment (overflows).
      ;;
      ;; cannot use 4340958203495 (untyped int constant) as int value in
      ;; argument to z.KeyToHash (overflows)
      #:tests? (and (target-64bit?)
                    (not (%current-target-system)))
      #:import-path "github.com/dgraph-io/ristretto"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks-and-contrib
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "benchmarks")
                (delete-file-recursively "contrib")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-cespare-xxhash-v2
           go-github-com-dgryski-go-farm
           go-github-com-dustin-go-humanize
           go-github-com-golang-glog
           go-github-com-pkg-errors
           go-golang-org-x-sys))
    (home-page "https://github.com/dgraph-io/ristretto")
    (synopsis "Memory-bound cache in Golang")
    (description
     "Ristretto is a concurrent, fixed size, in-memory cache with a dual focus
on throughput and hit ratio performance.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-dgraph-io-ristretto-v2
  (package
    (inherit go-github-com-dgraph-io-ristretto)
    (name "go-github-com-dgraph-io-ristretto-v2")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgraph-io/ristretto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q65y9psk8vnrsjlmaqhc8l3fwpsh23wrr4cjz8jfnph45hhh4jk"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-dgraph-io-ristretto)
       ((#:import-path _) "github.com/dgraph-io/ristretto/v2")))))

(define-public go-github-com-dgryski-go-linebreak
  (package
    (name "go-github-com-dgryski-go-linebreak")
    (version "0.0.0-20180812204043-d8f37254e7d3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/go-linebreak")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jv8ldb9vh5mgh66g61fh418x16s8jszrykhkbc901y1llf2mi22"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dgryski/go-linebreak"))
    (home-page "https://github.com/dgryski/go-linebreak")
    (synopsis "Wraps text at a given width")
    (description
     "Package linebreak wraps text at a given width, it's a translation of
@url{http://xxyxyz.org/line-breaking/, linear}, an implementation of
@url{https://en.wikipedia.org/wiki/SMAWK_algorithm, SMAWK algorithm}.")
    (license license:expat)))

;; The project provides no go.mod files and contains a veracity of commands
;; and libraries which might need missing dependencies, update them on demand.
(define-public go-github-com-dgryski-trifles
  (package
    (name "go-github-com-dgryski-trifles")
    (version "0.0.0-20240922021506-5ecb8eeff266")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/trifles")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "071pnsyax99ikc58b110hdvqk1v46mqk6zdd0sshrf9lmwixwpnj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/dgryski/trifles"))
    (home-page "https://github.com/dgryski/trifles")
    (synopsis "Collection of Golang utility libraries and commands")
    (description
     "This package provides a collection veriaty of utility libraries:

@itemize
@item intset - compress a stream of integers
@item lz - lempel-ziv compression
@item threadtree - a threaded binary-tree
@item numerical - numerical integration and root finding
@item maze - maze generation stuff
@item hist - simple command-line histogramming tool
@item simhash - trivial simhash implementation
@item wscat - trivial websocket netcat
@item servedir - trivial http fileserver
@item rndtxt - generate random text strings
@item mpush - push to multiple notification services (pushbullet, nma, pushover)
@item superbat - batmanjs and go-restful playground
@item msgrpc - msgpack rpc python/go interop samples
@item quantile - testing different streaming quantile estimators
@item wtflog - logging package with some renamed log levels
@item qrshow - display QR codes in a terminal
@item nlz - asm code to find number of leading zeros
@item httpecho - server to dump information about an http request
@item lzpack - trivial packed format for lz4 compression
@item grinderplot - generate a flot chart from grinder logs
@item worker - framework for spawning concurrent workers
@item gddo - search godoc.org from the command line
@item uuid - generate random UUIDs
@item entropy - reducer to compute entropy per epoch for a set of values in a category
@item inthash - integer hashing functions
@item udprelay - simple udp-to-tcp multiplexing relay
@item shufsecs - shuffle sorted epoch-data within epochs
@item strtable - dumb string->uint32 hash table for profiling vs native maps
@item cachetest - playing with different caching algorithms (clock, lru, lfu, random)
@item glj - passing data from go to lua with msgpack
@item toms - text filter for time.Duration to milliseconds
@item bluniq - bloom-filter based unique filter
@item skvdist - check distribution of shardedkv choosers
@item gcwatch - print out garbage collection stats from /debug/vars
@item toepoch - convert time fields to epochs
@item repl - framework for making dumb repls for testing
@item skvchk - tool for checking shardedkv distributions
@item interp - interpolation search
@item oma - simulation of the Dutch children's board game \"Met de bus naar Oma\"
@item rndsample - uniform random sample from stdin
@item pphrase - simple passphrase generator
@item fastrand - fast xorshift rng with bias-free [0..n)
@item range2cird - turn IP ranges into CIDR
@item gfmt - trivial filter wrapping go-linebreak
@item mtest - port of libtommath test program
@item leven - fastest levenshtein distance algorithm I could find
@item matcher - test different methods of testing string set membership
@item cstbucket - crunch carbonserver logs for time-ranges of queries
@item stablepart - stable partition a sort.Interface on a boolean predicate
@item jumpreplica - tool for playing with replica choices for jump-hash
@item sshdregex - demo using ragel for optimized regexp matching
@item hllbench - benchmark different hyperloglog implementations
@item shlines, sipsum - tools for siphashing things
@item urlq - extract query parameters from a list of URLs
@item median - compute the median of 5 numbers with a sorting network
@item hashbench - benchmark different hashing functions
@item fastpprof - how to use pprof with fasthttp
@item ewmaest - progress logging with ewma-based ETA estimation
@end itemize")
    (license license:expat)))

(define-public go-github-com-dicedb-dicedb-go
  (package
    (name "go-github-com-dicedb-dicedb-go")
    (version "1.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DiceDB/dicedb-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14n3j88asnx52hy8ykg6xx2cpshcl5w5mlp5qvcjbgrrmw29c0s9"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; dicedb-go depends on dicedb for running tests
      ;; but dicedb depends on dice-db, creating a cyclic depedency
      #:tests? #f
      #:import-path "github.com/dicedb/dicedb-go"))
    (propagated-inputs (list go-github-com-google-uuid
                             go-google-golang-org-protobuf))
    (home-page "https://github.com/dicedb/dicedb-go")
    (synopsis "SDK for @code{DiceDB}")
    (description
     "Go SDK for @url{https://github.com/dicedb/dice,@code{dicedb}}.")
    (license license:bsd-3)))

(define-public go-github-com-digitalocean-go-smbios
  (package
    (name "go-github-com-digitalocean-go-smbios")
    (version "0.0.0-20180907143718-390a4f403a8e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/digitalocean/go-smbios")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "026nw64rg2lck9hx4bixh6v9b7kz9v4dbraykqy2b655wcnlv8rv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/digitalocean/go-smbios"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (home-page "https://github.com/digitalocean/go-smbios")
    (synopsis "Detection and access to SMBIOS and DMI data and structures")
    (description
     "Package @code{smbios} provides detection and access to System Management
BIOS (@url{https://en.wikipedia.org/wiki/System_Management_BIOS, SMBIOS}) and
Desktop Management Interface (DMI) data and structures.")
    (license license:asl2.0)))

(define-public go-github-com-dimchansky-utfbom
  (package
    (name "go-github-com-dimchansky-utfbom")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dimchansky/utfbom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ll3wqvifmdanfyg6wsvz31c7n4mnczg2yxb65j35qxrnak89hn3"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/dimchansky/utfbom"))
    (home-page "https://github.com/dimchansky/utfbom")
    (synopsis "Go Unicode byte order mark detection library")
    (description
     "This package provides a library for @acronym{BOM, Unicode Byte Order
Mark} detection.")
    (license license:asl2.0)))

(define-public go-github-com-disintegration-imaging
  (package
    (name "go-github-com-disintegration-imaging")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/disintegration/imaging")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sl201nmk601h0aii4234sycn4v2b0rjxf8yhrnik4yjzd68q9x5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/disintegration/imaging"))
    (inputs
     (list go-golang-org-x-image))
    (home-page "https://github.com/disintegration/imaging")
    (synopsis "Simple image processing for Go")
    (description
     "This package provides basic image processing functions
(resize, rotate, crop, brightness/contrast adjustments, etc.).")
    (license license:expat)))

(define-public go-github-com-distribution-reference
  (package
    (name "go-github-com-distribution-reference")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/distribution/reference")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zj2lmmznlrxdrrfmdsx7fgrmi64bj1jqz6r0ar35qmkx8pjvgl2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/distribution/reference"))
    (propagated-inputs (list go-github-com-opencontainers-go-digest))
    (home-page "https://github.com/distribution/reference")
    (synopsis "Handle references to container images held in registries")
    (description
     "Package reference provides a general type to represent any way of referencing
images within the registry.  Its main purpose is to abstract tags and digests
(content-addressable hash).")
    (license license:asl2.0)))

(define-public go-github-com-djherbis-atime
  (package
    (name "go-github-com-djherbis-atime")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djherbis/atime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xsz55zpihd9wyrj6qvm3miqzb6x3mnp5apzs0dx1byndhb8adpq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/djherbis/atime"))
    (home-page "https://github.com/djherbis/atime")
    (synopsis "Access Times for files")
    (description "Package atime provides a platform-independent way to get
atimes for files.")
    (license license:expat)))

(define-public go-github-com-djherbis-times
  (package
    (name "go-github-com-djherbis-times")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djherbis/times")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a70nqkc592ipbgb3ib4yg8i2yj2hlhalpzzksdlhilm5a3689ic"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/djherbis/times"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/djherbis/times")
    (synopsis "File times - atime, mtime, ctime and btime for Golang")
    (description
     "Package @code{times} provides a platform-independent way to get atime,
mtime,ctime and btime for files.")
    (license license:expat)))

(define-public go-github-com-dlclark-regexp2
  (package
    (name "go-github-com-dlclark-regexp2")
    (version "1.11.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlclark/regexp2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i5c7ak8r4wwlyrx5f1mdipqk6p6ms1jgclb7hlb4qgy83c7xplc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dlclark/regexp2"
      #:test-flags
      ;; Time sensitive not deterministic tests.
      #~(list "-skip" "TestDeadline|TestStopTimeoutClock")))
    (home-page "https://github.com/dlclark/regexp2/")
    (synopsis "Full featured regular expressions for Go")
    (description
     "Regexp2 is a feature-rich RegExp engine for Go.")
    (license license:expat)))

(define-public go-github-com-dnephin-pflag
  (package
    (name "go-github-com-dnephin-pflag")
    (version "1.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dnephin/pflag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d3aakwpwdbq3lqpk5kdqlr0h7maqxnpsbrd2022xwd93fxyxcq0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; FIXME
      #:import-path "github.com/dnephin/pflag"))
    (home-page "https://github.com/dnephin/pflag")
    (synopsis "Drop-in replacement for Go's flag package")
    (description
     "Package pflag is a drop-in replacement for Go's flag package,
implementing POSIX/GNU-style --flags.  It is compatible with the
@url{http://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html,
GNU extensions to the POSIX recommendations for command-line options}.  This
is an actively maintained fork of @url{https://github.com/ogier/pflag}.")
    (license license:bsd-3)))

(define-public go-github-com-docker-cli
  (package
    (name "go-github-com-docker-cli")
    (version "25.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/docker/cli")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gaz2pkivky94z8148aa27kdxn548j3r96xa3a9xfqpi6b1rhy27"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/cli"
      #:embed-files #~(list ".*\\.json")
      #:skip-build? #t
      #:tests? #f))
    (propagated-inputs
     (list go-github-com-docker-docker-credential-helpers
           go-github-com-fvbommel-sortorder
           go-github-com-google-shlex
           go-github-com-mitchellh-mapstructure
           go-github-com-pkg-errors
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/docker/cli")
    (synopsis "Docker command-line interface")
    (description "This repository is the home of the Docker command-line
interface (CLI).")
    (license license:asl2.0)))

(define-public go-github-com-docker-distribution
  (package
    (name "go-github-com-docker-distribution")
    (version "2.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/distribution")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dbaxmkhg53anhkzngyzlxm2bd4dwv0sv75zip1rkm0874wjbxzb"))
       (snippet
        ;; TODO: Unbundle more.
        #~(begin (use-modules (guix build utils))
                 (for-each delete-file-recursively
                           (list "vendor/golang.org"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/distribution"
      #:test-flags #~(list "-test.short")
      #:test-subdirs
      #~(list "configuration"
              "context"
              "health"
              "manifest/..."
              "notifications/..."
              "uuid")))
    (native-inputs
     (list go-github-com-sirupsen-logrus
           go-golang-org-x-crypto
           go-golang-org-x-sys))
    (home-page "https://github.com/docker/distribution")
    (synopsis
     "This package is a Docker toolset to pack, ship, store, and deliver content")
    (description
     "Docker Distribution is a Docker toolset to pack, ship,
store, and deliver content.  It contains Docker Registry 2.0 and libraries to
interact with distribution components.")
    (license license:asl2.0)))

(define-public go-github-com-docker-docker
  (package
    (name "go-github-com-docker-docker")
    (version "25.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/moby/moby")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0852mrvs8602azqzx2zhb1xl0vs7baw8qfmkgrl625xm5hxrigvq"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/docker"
      #:skip-build? #t
      #:tests? #f))
    (propagated-inputs
     (list go-github-com-containerd-containerd
           go-github-com-containerd-log
           go-github-com-distribution-reference
           go-github-com-docker-go-connections
           go-github-com-docker-go-units
           go-github-com-gogo-protobuf
           go-github-com-klauspost-compress
           go-github-com-moby-docker-image-spec
           go-github-com-moby-sys-sequential
           go-github-com-moby-sys-user
           go-github-com-moby-sys-userns
           go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
           go-go-opentelemetry-io-otel))
    (home-page "https://github.com/docker/docker")
    (synopsis "The Moby Project")
    (description
     "Moby is an open-source project created by Docker to enable and accelerate
software containerization.")
    (license license:asl2.0)))

(define-public go-github-com-docker-docker-credential-helpers
  (package
    (name "go-github-com-docker-docker-credential-helpers")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/docker-credential-helpers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y9chbmp70sjz88j4yy8p68f8n9x2rl9r4z25kd77s31cbdkg707"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/docker-credential-helpers"
      #:skip-build? #t
      #:tests? #f))
    (propagated-inputs (list go-github-com-keybase-go-keychain
                             go-github-com-danieljoos-wincred))
    (home-page "https://github.com/docker/docker-credential-helpers")
    (synopsis
     "Keep Docker login credentials safe by storing in platform keystores")
    (description
     "docker-credential-helpers is a suite of programs to use native stores to keep
Docker credentials safe.")
    (license license:expat)))

(define-public go-github-com-docker-go-units
  (package
    (name "go-github-com-docker-go-units")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/go-units")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q92l7pyfg9rn4ljr1warnv1ri6z7svah7m3rqmvmyaw6bzdbbw8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/go-units"))
    (home-page "https://github.com/docker/go-units")
    (synopsis "Parse and print size and time units in human-readable format")
    (description
     "@code{go-units} is a library to transform human friendly measurements
into machine friendly values.")
    (license license:asl2.0)))

(define-public go-github-com-docopt-docopt-go
  (let ((commit "ee0de3bc6815ee19d4a46c7eb90f829db0e014b1")
        (revision "0"))
    (package
      (name "go-github-com-docopt-docopt-go")
      (version (git-version "0.6.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/docopt/docopt.go")
               (commit version)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hlra7rmi5pmd7d93rv56ahiy4qkgmq8a6mz0jpadvbi5qh8lq6j"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/docopt/docopt-go"))
      (home-page "https://github.com/docopt/docopt.go")
      (synopsis "Implementation of docopt in Golang")
      (description
       "This package provides command-line arguments parser based on written
help message which may simplify crating CLI applications, it's Golang
implementation of http://docopt.org/.")
      (license license:expat))))

(define-public go-github-com-dsnet-golib
  (package
    (name "go-github-com-dsnet-golib")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dsnet/golib")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f314wzr16w6ix3bs7ginjkizgyl3b1r3j2gvvqzr8dv53r4s5cq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dsnet/golib"))
    (propagated-inputs
     (list go-github-com-google-go-cmp))
    (home-page "https://github.com/dsnet/golib")
    (synopsis "Collection of helper libraries for Golang")
    (description
     "@code{golib} is a collection of unrelated libraries.
This package provides a following list of Golang models:
@table @code
@item bufpipe
Implements a buffered pipe.
@item cron
Parses and runs cron schedules.
@item hashmerge
Merges hash checksums.
@item jsoncs
Implements JSON Canonicalization Scheme (JCS) as specified in RFC 8785.
@item jsonfmt
Implements a JSON formatter.
@item memfile
Implements an in-memory emulation of @code{os.File}.
@item unitconv
Implements string conversion functionality for unit prefixes.
@end table")
    (license license:bsd-3)))

(define-public go-github-com-dustin-go-humanize
  (package
    (name "go-github-com-dustin-go-humanize")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dustin/go-humanize")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iyhd90pnmxh64nhsh6k02c1b1glpmhh4whga9jgb9g0i5hz3sya"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dustin/go-humanize"))
    (home-page "https://github.com/dustin/go-humanize")
    (synopsis "Humane unit formatter")
    (description
     "@code{go-humanize} provides formatters for units to human friendly
sizes.  It converts boring ugly numbers to human-friendly strings and back.")
    (license license:expat)))

(define-public go-github-com-dustinkirkland-golang-petname
  (package
    (name "go-github-com-dustinkirkland-golang-petname")
    (version "0.0.0-20240428194347-eebcea082ee0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dustinkirkland/golang-petname")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s43l8d8vysjaxq0xmmpzb2w1xnfc1f6j6g0xpfhyjln0r49k6gc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dustinkirkland/golang-petname"))
    (home-page "https://github.com/dustinkirkland/golang-petname")
    (synopsis "RFC 1178 pet names implementation in Golang")
    (description
     "This package provides an @url{https://tools.ietf.org/html/rfc1178,
RFC1178} implementation to generate pronounceable, sometimes even memorable,
\"pet names\", consisting of a random combination of adverbs, an adjective,
and an animal name")
    (license license:asl2.0)))

(define-public go-github-com-eapache-channels
  (package
    (name "go-github-com-eapache-channels")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eapache/channels")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "179ryd2rqsccnv5nk35f8j4nfbqr8cgb2bjm0j8isvf5nzks8s9y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/eapache/channels"))
    (propagated-inputs
     (list go-github-com-eapache-queue))
    (home-page "https://github.com/eapache/channels")
    (synopsis "Portable mmap package for Golang")
    (description
     "Package channels provides a collection of helper functions, interfaces
and implementations for working with and extending the capabilities of
golang's existing channels.  The main interface of interest is Channel, though
sub-interfaces are also provided for cases where the full Channel interface
cannot be met (for example, @code{InChannel} for write-only channels).")
    (license license:expat)))

(define-public go-github-com-eapache-queue
  (package
    (name "go-github-com-eapache-queue")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/eapache/queue")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07dp54n94gn3gsvdcki56yqh7py7wqqigxbamhxwgbr05n61fqyg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/eapache/queue"))
    (home-page "https://github.com/eapache/queue")
    (synopsis "Fast golang queue using ring-buffer")
    (description
     "Package queue provides a fast, ring-buffer queue based on the version
suggested by Dariusz Górecki.  Using this instead of other, simpler, queue
implementations
(slice+append or linked list) provides substantial memory and time benefits, and
fewer GC pauses.")
    (license license:expat)))

(define-public go-github-com-edsrzf-mmap-go
  (package
    (name "go-github-com-edsrzf-mmap-go")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edsrzf/mmap-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11xpfcacfvmrkbp0pv4j8pg2gyjnxpfp7l93j42h0svwxywhjmrc"))))
    (build-system go-build-system)
    (propagated-inputs (list go-golang-org-x-sys))
    (arguments
     (list
      #:import-path "github.com/edsrzf/mmap-go"))
    (home-page "https://github.com/edsrzf/mmap-go")
    (synopsis "Memory mapped fiels (mmap) in Golang")
    (description
     "This package implements functionality of mapping files into memory.  It
tries to provide a simple interface, but doesn't go out of its way to abstract
away every little platform detail.

This specifically means:
@itemize
@item forked processes may or may not inherit mappings
@item a file's timestamp may or may not be updated by writes through mappings
@item specifying a size larger than the file's actual size can increase the
file's size
@item if the mapped file is being modified by another process while your
program's running, don't expect consistent results between platforms
@end itemize")
    (license license:bsd-3)))

(define-public go-github-com-elastic-gosigar
  (package
    (name "go-github-com-elastic-gosigar")
    (version "0.14.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elastic/gosigar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g7si935p23brvq29y7f6rasj2cbpdhrp7z0mk21q98hsa3qs60a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/elastic/gosigar"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Test fails with error: open /etc/mtab: no such file or
               ;; directory.
               (list "TestFileSystemList"
                     ;; Test fails on ARM with error: inetdiag_test.go:61:
                     ;; protocol not supported.
                     #$@(if (target-arm?)
                            '("TestNetlinkInetDiag"
                              ;; Expect "qemu-aarch64" to match "go(.exe)?"
                              "TestProcExe")
                            '()))
               "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pkg-errors go-golang-org-x-sys))
    (home-page "https://github.com/elastic/gosigar")
    (synopsis "Gathers system and per process statistics")
    (description
     "Go sigar is a golang implementation of the
@url{https://github.com/hyperic/sigar,sigar API}.  The Go version of sigar
has a very similar interface, but is being written from scratch in pure
go/cgo, rather than cgo bindings for libsigar.  This package provides an
alternative fork of @url{https://github.com/cloudfoundry/gosigar}.")
    (license license:asl2.0)))

(define-public go-github-com-elliotchance-orderedmap
  (package
    (name "go-github-com-elliotchance-orderedmap")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elliotchance/orderedmap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hhyk96l6mfijkay9ga6jqpczpn34fbqkjrqj3v9pf5p1hzd0xdx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/elliotchance/orderedmap"
      #:unpack-path "github.com/elliotchance/orderedmap"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-submodule
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "v2"))))
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  ;; The full test suite runs flaky performance tests, so only
                  ;; run the short tests.
                  (invoke "go" "test" "-test.short" "."))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/elliotchance/orderedmap")
    (synopsis "Go ordered map library")
    (description
     "This package provides a ordered map library that maintains amortized
O(1) for @code{Set}, @code{Get}, @code{Delete} and @code{Len}.")
    (license license:expat)))

(define-public go-github-com-elliotchance-orderedmap-v2
  (package
    (inherit go-github-com-elliotchance-orderedmap)
    (name "go-github-com-elliotchance-orderedmap-v2")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elliotchance/orderedmap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11bvia6cflq46nzc2hfgikgxyck7wskyi0i7ksy9r0d41l4jh4l9"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-elliotchance-orderedmap)
       ((#:import-path _) "github.com/elliotchance/orderedmap/v2")
       ((#:phases _ '%standard-phases)
        #~(modify-phases %standard-phases
            (delete 'remove-submodule)))))))

(define-public go-github-com-emersion-go-autostart
  (package
    (name "go-github-com-emersion-go-autostart")
    (version "0.0.0-20210130080809-00ed301c8e9a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-autostart")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cqqvbzn32xv5lknfygrx01rx2sc6pi833k7008nlk9lsfgry06v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-autostart"))
    (home-page "https://github.com/emersion/go-autostart")
    (synopsis "Autostart library in Go")
    (description
     "Go-Autostart is a Go library to run a command after login.")
    (license license:expat)))

(define-public go-github-com-emersion-go-ical
  (package
    (name "go-github-com-emersion-go-ical")
    (version "0.0.0-20240127095438-fc1c9d8fb2b6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-ical")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01cn9kggkdalb6xp2nrka01gs40zs8v6h5bq8d2m8wrdcsy5b36v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-ical"))
    (propagated-inputs (list go-github-com-teambition-rrule-go))
    (home-page "https://github.com/emersion/go-ical")
    (synopsis "iCalendar library for Golang")
    (description
     "This package implements @url{https://tools.ietf.org/html/rfc5545, RFC
5545} iCalendar specification.")
    (license license:expat)))

(define-public go-github-com-emersion-go-vcard
  (package
    (name "go-github-com-emersion-go-vcard")
    (version "0.0.0-20230815062825-8fda7d206ec9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-vcard")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12n5jinj5xzdfl9jhqvjbzxvj32bw310mdw4q5rjv35pk566zixl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-vcard"))
    (home-page "https://github.com/emersion/go-vcard")
    (synopsis "Parse and format vCard in Golang")
    (description
     "This package implements functionality to parse and format vCard as
specified in @url{https://datatracker.ietf.org/doc/html/rfc6350, RFC 6350}.")
    (license license:expat)))

(define-public go-github-com-emersion-go-webdav
  (package
    (name "go-github-com-emersion-go-webdav")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-webdav")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lqll59rvdlj0s014fjdn8brb9j9h59fmly744yfz6wxwdcp23g4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-webdav"))
    (propagated-inputs
     (list go-github-com-emersion-go-ical
           go-github-com-emersion-go-vcard))
    (home-page "https://github.com/emersion/go-webdav")
    (synopsis "WebDAV, CalDAV and CardDAV implementations in Golang")
    (description
     "This package provides Golang modules implementing WebDAV
@url{https://tools.ietf.org/html/rfc4918, RFC 4918}, CalDAV
@url{https://tools.ietf.org/html/rfc4791, RFC 4791} and CardDAV
@url{https://tools.ietf.org/html/rfc6352, RFC 6352} specifications.")
    (license license:expat)))

(define-public go-github-com-emirpasic-gods
  (package
    (name "go-github-com-emirpasic-gods")
    (version "1.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emirpasic/gods")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vnnmv813m8yhykwlxpizpvpimsirbaiwa3ckxfyx3ybv1swlq44"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/emirpasic/gods"))
    (home-page "https://github.com/emirpasic/gods/")
    (synopsis "Implementation of various data structures and algorithms in Go")
    (description
     "This package provides implementation of various data structures and
algorithms in Go.")
    (license license:bsd-2)))

(define-public go-github-com-envoyproxy-protoc-gen-validate
  (package
    (name "go-github-com-envoyproxy-protoc-gen-validate")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bufbuild/protoc-gen-validate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yd77gnsn9bbiihbkdyn9klwbv314l6ar83z4kivpn9mr93xysch"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/envoyproxy/protoc-gen-validate/tests
            (delete-file-recursively "tests")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/envoyproxy/protoc-gen-validate"))
    (propagated-inputs
     (list go-github-com-iancoleman-strcase
           go-github-com-lyft-protoc-gen-star-v2-next
           go-golang-org-x-net
           go-google-golang-org-protobuf))
    (home-page "https://github.com/envoyproxy/protoc-gen-validate")
    (synopsis "Protocol Buffer Validation for Go, Java, Python, and C++")
    (description
     "PGV is a protoc plugin to generate polyglot message validators.  While
protocol buffers effectively guarantee the types of structured data, they
cannot enforce semantic rules for values.  This plugin adds support to
protoc-generated code to validate such constraints.")
    (license license:asl2.0)))

(define-public go-github-com-ergochat-readline
  (package
    (name "go-github-com-ergochat-readline")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ergochat/readline")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16zyk1dzwix5l9iph61img6qn5kryq3kb03dk2lwmrwyr1xdsip3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ergochat/readline"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (propagated-inputs (list go-golang-org-x-text go-golang-org-x-sys))
    (home-page "https://github.com/ergochat/readline")
    (synopsis "Readline implementation in pure Go")
    (description
     "This package provides a pure Go implementation of functionality
comparable to @url{https://en.wikipedia.org/wiki/GNU_Readline, GNU Readline},
i.e.  line editing and command history for simple TUI programs.")
    (license license:expat)))

(define-public go-github-com-erikgeiser-coninput
  (package
    (name "go-github-com-erikgeiser-coninput")
    (version "0.0.0-20211004153227-1c3628e74d0f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/erikgeiser/coninput")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x8yw15ngyg4vlcdv5wsgpr6w5kavjv7bmk5mpvvx848bwvslr1r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/erikgeiser/coninput"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/example")))))))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/erikgeiser/coninput")
    (synopsis "Input handling with Windows Console API")
    (description
     "Go library for input handling using Windows Console API.")
    (license license:expat)))

(define-public go-github-com-errata-ai-ini
  (package
    (name "go-github-com-errata-ai-ini")
    (version "1.63.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/errata-ai/ini")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zs9dwxh8mzxm1zfck4ghs7hma1lz5ajh98kmyh888rn3npvrnm5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/errata-ai/ini"))
    (home-page "https://github.com/errata-ai/ini")
    (synopsis "INI file read and write functionality in Golang")
    (description
     "This Package provides a functionality of INI file read and write,
implementing features:
@itemize
@item load from multiple data sources(file, @code{[]byte}, @code{io.Reader}
and @code{io.ReadCloser}) with overwrites
@item read with recursion values
@item read with parent-child sections
@item read with auto-increment key names
@item read with multiple-line values
@item read with tons of helper methods
@item read and convert values to Go types
@item read and WRITE comments of sections and keys
@item manipulate sections, keys and comments with ease
@item keep sections and keys in order as you parse and save
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-errata-ai-regexp2
  (package
    (inherit go-github-com-dlclark-regexp2)
    (name "go-github-com-errata-ai-regexp2")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/errata-ai/regexp2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p28af5c7dn4knnksl9dxjb44cicsmadzb8kwzyyf20kr7hrq53q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/errata-ai/regexp2"))
    (home-page "https://github.com/errata-ai/regexp2")
    (description
     (string-append (package-description go-github-com-dlclark-regexp2)
                    "  This package is a fork of dlclark/regexp2 providing a
more similar API to regexp."))))

(define-public go-github-com-ettle-strcase
  (package
    (name "go-github-com-ettle-strcase")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ettle/strcase")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gg3zxbbp3vfskzg2dl1s1agjn34dw14282fj28g9nrwqbbq854x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ettle/strcase"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmark
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "benchmark")))))))
    (home-page "https://github.com/ettle/strcase")
    (synopsis "String naming convention style library")
    (description
     "Package strcase is a package for converting strings into various word
cases (e.g. snake_case, camelCase, kebab-case, etc).")
    (license license:expat)))

(define-public go-github-com-expr-lang-expr
  (package
    (name "go-github-com-expr-lang-expr")
    (version "1.17.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/expr-lang/expr")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kcpgycdy9fm4g2i4mhp6hprzkg75r0lfrvc0gbwd2wiir460222"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/expr-lang/expr/repl
            ;; - github.com/expr-lang/expr/debug
            (for-each delete-file-recursively
                      (list "repl" "debug"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/expr-lang/expr"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://expr-lang.org/")
    (synopsis "Expression language and expression evaluation for Go")
    (description
     "The package @strong{Expr} provides a Go-centric expression language
designed to deliver dynamic configurations with unparalleled accuracy, safety,
and speed.")
    (license license:expat)))

(define-public go-github-com-facebookgo-atomicfile
  (package
    (name "go-github-com-facebookgo-atomicfile")
    (version "0.0.0-20151019160806-2de1f203e7d5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/facebookarchive/atomicfile")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vsx6r6y601jxvjqc8msbpr5v1037dfxxdd8h1q3s8wm6xhvj2v6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/facebookgo/atomicfile"))
    (home-page "https://github.com/facebookgo/atomicfile")
    (synopsis "Atomically written/replaced file")
    (description
     "Package atomicfile provides the ability to write a file with an eventual
rename on Close (using @code{os.Rename}).  This allows for a file to always be
in a consistent state and never represent an in-progress write.")
    ;; patents
    ;;
    ;; Additional Grant of Patent Rights Version 2
    ;; <...>
    ;; Facebook, Inc. ("Facebook") hereby grants to each recipient of the
    ;; Software ("you") a perpetual, worldwide, royalty-free, non-exclusive,
    ;; irrevocable (subject to the termination provision below) license under
    ;; any Necessary Claims, to make, have made, use, sell, offer to sell,
    ;; import, and otherwise transfer the Software.
    ;; <...>
    (license license:bsd-3)))

(define-public go-github-com-facebookgo-flagenv
  (package
    (name "go-github-com-facebookgo-flagenv")
    (version "0.0.0-20160425205200-fcd59fca7456")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/facebookarchive/flagenv")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c7fw4y4gmdrn66dx7la637lx0hvs1w27x9j12wdx037n51r078g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f   ;Tests require dated code <github.com/facebookgo/ensure>
      #:import-path "github.com/facebookgo/flagenv"))
    (home-page "https://github.com/facebookgo/flagenv")
    (synopsis "Populate flags from environment variables")
    (description
     "Package flagenv provides the ability to populate flags from environment
variables.")
    (license license:bsd-3)))

(define-public go-github-com-facette-natsort
  (package
    (name "go-github-com-facette-natsort")
    (version "0.0.0-20181210072756-2cd4dd1e2dcb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/facette/natsort")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kfas7nq7cfrbaqvpmifg2p8v8z0d2kdqjb7p9y6r0rpdzl2zy6p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/facette/natsort"))
    (home-page "https://github.com/facette/natsort")
    (synopsis "Natural strings sorting in Go")
    (description
     "This package provides an implementation of
@url{https://web.archive.org/web/20210803201519/http://davekoelle.com/alphanum.html,the
Alphanum Algorithm} developed by Dave Koelle in Go.")
    (license license:bsd-3)))

(define-public go-github-com-fatih-camelcase
  (package
    (name "go-github-com-fatih-camelcase")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fatih/camelcase")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z7rw6f5j97dkzqya257dqlxf3cm8zl508081gmnr4bsjhkwpz2l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fatih/camelcase"))
    (home-page "https://github.com/fatih/camelcase")
    (synopsis "Split a camelcase word into a slice of words in Go")
    (description
     "Package camelcase is a micro package to split the words of a camelcase type
string into a slice of words.")
    (license license:expat)))

(define-public go-github-com-fatih-color
  (package
    (name "go-github-com-fatih-color")
    (version "1.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fatih/color")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07axwr6016xwylxlsrw3cnkg1kg963zqqgf06pc3dgicfg5qrhj2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fatih/color"))
    (propagated-inputs
     (list go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty))
    (home-page "https://pkg.go.dev/github.com/fatih/color")
    (synopsis "Print colored text in Go")
    (description
     "This package provides an ANSI color package to output colorized or SGR
defined output to the standard output.")
    (license license:expat)))

(define-public go-github-com-fatih-gomodifytags
  ;; This particular commit (v1.17.1-0.20250423142747-f3939df9aa3c) provides
  ;; "modifytags" submodule which is required for gopls@0.19.1.
  (let ((commit "f3939df9aa3cc13eb51e50268af256b4f9272cdb")
        (revision "0"))
    (package
      (name "go-github-com-fatih-gomodifytags")
      (version (git-version "1.17.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/fatih/gomodifytags")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0gr9bs5f94kpnmjsg7dn6gr7yazlgllyypy2g6xvqrhgzrk2ckbr"))))
      (build-system go-build-system)
      (arguments
       (list
        #:skip-build? #t
        #:import-path "github.com/fatih/gomodifytags"))
      (native-inputs
       (list go-golang-org-x-tools))   ;for the CLI
      (propagated-inputs
       (list go-github-com-fatih-camelcase
             go-github-com-fatih-structtag))
      (home-page "https://github.com/fatih/gomodifytags")
      (synopsis "Tool to modify struct field tags in Golang")
      (description
       "This package implements a functionality to modify/update field tags in
structs making it easy to update, add or delete the tags in a struct field
with possibility to add and remove tag options.  It's intended to be used by
an editor, but also has modes to run it from the terminal.")
      (license license:bsd-3))))

(define-public go-github-com-fatih-structs
  (package
    (name "go-github-com-fatih-structs")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fatih/structs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wrhb8wp8zpzggl61lapb627lw8yv281abvr6vqakmf569nswa9q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fatih/structs"))
    (home-page "https://github.com/fatih/structs")
    (synopsis "Utilities for Go structs")
    (description "This package provides various utility functions to work
with Go structs.")
    (license license:expat)))

(define-public go-github-com-fatih-structtag
  (package
    (name "go-github-com-fatih-structtag")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fatih/structtag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09a9pycvkf384v5f47ff4q33bjbzpx6kbkn23za1gcwc96466sk3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fatih/structtag"))
    (home-page "https://github.com/fatih/structtag")
    (synopsis "Parse and modify Go struct field tags")
    (description
     "This package provides an easy way of parsing and manipulating struct tag fields.
Please vendor the library as it might change in future versions.")
    (license license:bsd-3)))

(define-public go-github-com-flopp-go-findfont
  (package
    (name "go-github-com-flopp-go-findfont")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flopp/go-findfont")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05jvs5sw6yid0qr2ld7aw0n1mjp47jxhvbg9lsdig86668i2fj2q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/flopp/go-findfont"))
    (home-page "https://github.com/flopp/go-findfont")
    (synopsis "Go font finder library")
    (description
     "This package provides a platform-agnostic Go library to locate TrueType
font files in your system's user and system font directories.")
    (license license:expat)))

;; XXX: This repository has been archived by the owner on Nov 9, 2017. It is
;; now read-only.
(define-public go-github-com-flynn-archive-go-shlex
  (let ((commit "3f9db97f856818214da2e1057f8ad84803971cff")
        (revision "0"))
    (package
      (name "go-github-com-flynn-archive-go-shlex")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/flynn-archive/go-shlex")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1j743lysygkpa2s2gii2xr32j7bxgc15zv4113b0q9jhn676ysia"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/flynn-archive/go-shlex"))
      (synopsis "Go lexer")
      (description
       "Shlex is a simple lexer for go that supports shell-style
quoting, commenting, and escaping.")
      (home-page "https://github.com/flynn-archive/go-shlex")
      (license license:asl2.0))))

;; XXX: This project isn't maintained upstream, consider to find alternative
;; fork, see <https://github.com/fogleman/gg/issues/185> and remove the
;; package when it has no users.
(define-public go-github-com-fogleman-gg
  (package
    (name "go-github-com-fogleman-gg")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fogleman/gg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nkldjghbqnzj2djfaxhiv35kk341xhcrj9m2dwq65v684iqkk8n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f      ; Issue with test flags.
      #:import-path "github.com/fogleman/gg"))
    (propagated-inputs
     (list go-github-com-golang-freetype))
    (home-page "https://github.com/fogleman/gg")
    (synopsis "2D rendering in Go")
    (description
     "@code{gg} is a library for rendering 2D graphics in pure Go.")
    (license license:expat)))

(define-public go-github-com-freddierice-go-losetup
  (package
    (name "go-github-com-freddierice-go-losetup")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/freddierice/go-losetup")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qhc81r3cxhx16c7pkgwpkhkz1xn47xjcgfwydvlgh9f04cyydpq"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/freddierice/go-losetup"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/freddierice/go-losetup")
    (synopsis "Loop devices for Golang")
    (description
     ;; XXX: Project's REAME lacks details.
     "This package implements @code{/dev/loop} device in Go.")
    (license license:expat)))

(define-public go-github-com-fsnotify-fsnotify
  (package
    (name "go-github-com-fsnotify-fsnotify")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fsnotify/fsnotify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wr3695yb7hl405h6pzkbdkkxpdbmc5kwjjwaf9almbvmpk6077r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fsnotify/fsnotify"
      #:test-flags #~(list "-skip" "TestDiffMatch/3")))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/fsnotify/fsnotify")
    (synopsis "File system notifications for Go")
    (description
     "File system notifications for Go")
    (license license:bsd-3)))

(define-public go-github-com-fvbommel-sortorder
  (package
    (name "go-github-com-fvbommel-sortorder")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fvbommel/sortorder")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01cdxp9bqg1dlsf0nkb70gssr9mh250agfzyksx875m6ys1x37g7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fvbommel/sortorder"))
    (home-page "https://github.com/fvbommel/sortorder")
    (synopsis "Sort orders and comparison functions")
    (description
     "Package sortorder implements sort orders and comparison functions.")
    (license license:expat)))

(define-public go-github-com-fxamacker-cbor-v2
  (package
    (name "go-github-com-fxamacker-cbor-v2")
    (version "2.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fxamacker/cbor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mvlgjh59s0hqdhz5j0nf8p9mlp6hwkrd8i8bkyvq2hilgplm1pw"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Check if the most of the tests may be enabled:
      ;; src/github.com/fxamacker/cbor/v2/decode_test.go:328:9: cannot convert
      ;; 1000000000000 (untyped int constant) to type uint
      #:tests? (and (target-64bit?)
                    (not (%current-target-system)))
      #:import-path "github.com/fxamacker/cbor/v2"))
    (propagated-inputs
     (list go-github-com-x448-float16))
    (home-page "https://github.com/fxamacker/cbor")
    (synopsis "CBOR Codec in Golang")
    (description
     "This package implements functionality for encoding and decoding
@acronym{Concise Binary Object
Representation,CBOR} (@url{https://www.rfc-editor.org/rfc/rfc8949.html,RFC
8949}) and CBOR Sequences, with CBOR tags, Golang struct tags (@code{toarray},
@code{keyasint}, @code{omitempty}), @code{float64/32/16}, and
@code{big.Intp}.")
    (license license:expat)))

(define-public go-github-com-gabriel-vasile-mimetype
  (package
    (name "go-github-com-gabriel-vasile-mimetype")
    (version "1.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gabriel-vasile/mimetype")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ris146v7k1x1n4vraq0xzjds0f7jw3scx9mzj8y29hql3sy4nkd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gabriel-vasile/mimetype"
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'add-supported-mimes-md
                     (lambda* (#:key import-path #:allow-other-keys)
                       ;; This file needs to be available for writing during the
                       ;; tests otherwise they will fail.
                       (let ((file (format #f "src/~a/supported_mimes.md"
                                           import-path)))
                         (invoke "touch" file)
                         (chmod file #o644)))))))
    (propagated-inputs (list go-golang-org-x-net))
    (home-page "https://github.com/gabriel-vasile/mimetype")
    (synopsis "Golang library for media type and file extension detection")
    (description
     "This package provides a Golang module that uses magic number signatures
to detect the MIME type of a file.

Main features:
@itemize
@item Fast and precise MIME type and file extension detection.
@item Supports
@url{https://github.com/gabriel-vasile/mimetype/blob/master/supported_mimes.md,
many MIME types}.
@item Allows to
@url{https://pkg.go.dev/github.com/gabriel-vasile/mimetype#example-package-Extend,
extend} with other file formats.
@item Common file formats are prioritized.
@item
@url{https://pkg.go.dev/github.com/gabriel-vasile/mimetype#example-package-TextVsBinary,
Differentiation between text and binary files}.
@item Safe for concurrent usage.
@end itemize")
    (license license:expat)))

(define-public go-github-com-gammazero-chanqueue
  (package
    (name "go-github-com-gammazero-chanqueue")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gammazero/chanqueue")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ff4k2hmg9m1k9qjdr5cv1rdhls5iajpi4c3rdqwbmqnnaz7m5fr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gammazero/chanqueue"))
    (native-inputs
     (list go-go-uber-org-goleak))
    (propagated-inputs
     (list go-github-com-gammazero-deque))
    (home-page "https://github.com/gammazero/chanqueue")
    (synopsis "Buffered channel with unlimited capacity queue")
    (description
     "Package chanqueue implements a queue that uses channels for input and
output to provide concurrent access to a re-sizable queue.  This allows the
queue to be used like a channel.  Closing the input channel closes the output
channel when all queued items are read, consistent with channel behavior.  In
other words chanqueue is a dynamically buffered channel with up to infinite
capacity.")
    (license license:expat)))

(define-public go-github-com-gammazero-deque
  (package
    (name "go-github-com-gammazero-deque")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gammazero/deque")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "017xy7cw5yzmfjixwx5bglcxhg9gyyrqjilqzvq3mrh2760idp1a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gammazero/deque"))
    (home-page "https://github.com/gammazero/deque")
    (synopsis "Fast ring-buffer double-ended queue")
    (description
     "Package deque provides a fast ring-buffer deque (double-ended queue)
implementation for Golang.

It generalizes a queue and a stack, to efficiently add and remove items at
either end with O(1) performance.  Queue (FIFO) operations are supported using
@code{PushBack} and @code{PopFront}.  Stack (LIFO) operations are supported
using @code{PushBack} and @code{PopBack}.")
    (license license:expat)))

(define-public go-github-com-gdamore-encoding
  (package
    (name "go-github-com-gdamore-encoding")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gdamore/encoding")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0506b4pnn3yk80sjrsh38z29qj41spxjl80fjw7vg3mz5kfkdxhi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gdamore/encoding"))
    (inputs
     (list go-golang-org-x-text))
    (home-page "https://github.com/gdamore/encoding")
    (synopsis "Provide encodings missing from Go")
    (description
     "This package provides useful encodings not included in the standard
@code{Text} package, including some for dealing with I/O streams from
non-UTF-friendly sources.")
    (license license:expat)))

(define-public go-github-com-gdamore-tcell
  (package
    (name "go-github-com-gdamore-tcell")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gdamore/tcell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "178h7kj4zb8lcw84nbqanapnxrgvhq4111xw4fj6m56y46anlzwg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gdamore/tcell"))
    (inputs
     (list go-github-com-gdamore-encoding
           go-github-com-mattn-go-runewidth
           go-github-com-lucasb-eyer-go-colorful
           go-golang-org-x-sys
           go-golang-org-x-text))
    (home-page "https://github.com/gdamore/tcell")
    (synopsis "Provide a cell-based view for text terminals")
    (description
     "This package includes a full parser and expander for terminfo
capability strings to avoid hard-coding escape strings for formatting.  It
also favors portability, and includes support for all POSIX systems.")
    (license license:asl2.0)))

(define-public go-github-com-gdamore-tcell-v2
  (package
    (inherit go-github-com-gdamore-tcell)
    (name "go-github-com-gdamore-tcell-v2")
    (version "2.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gdamore/tcell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lr81rqcd30djsz1xa7pqh7k3vyip2xp95g14qqr87xxvb4ggm6v"))))
    (arguments
     (list
      #:import-path "github.com/gdamore/tcell/v2"))
    (propagated-inputs
     (modify-inputs (package-inputs go-github-com-gdamore-tcell)
       (prepend go-golang-org-x-term go-golang-org-x-sys)))))

(define-public go-github-com-gdey-errors
  (package
    (name "go-github-com-gdey-errors")
    (version "0.0.0-20190426172550-8ebd5bc891fb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gdey/errors")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mg33xckg5i529jnv6jxvmp36innz0xl5gbkmnww9paak5yvfjb3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gdey/errors"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/gdey/errors")
    (synopsis "Augmentation of std @code{errors} library")
    (description
     "This package provides a small error library that augments the errors
library in go standard library.")
    (license license:expat)))

(define-public go-github-com-gedex-inflector
  (package
    (name "go-github-com-gedex-inflector")
    (version "0.0.0-20170307190818-16278e9db813")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gedex/inflector")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05hjqw1m71vww4914d9h6nqa9jw3lgjzwsy7qaffl02s2lh1amks"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gedex/inflector"))
    (home-page "https://github.com/gedex/inflector")
    (synopsis "Go library that pluralizes and singularizes English nouns")
    (description
     "Go library that pluralizes and singularizes English nouns.")
    (license license:bsd-2)))

(define-public go-github-com-ghemawat-stream
  (package
    (name "go-github-com-ghemawat-stream")
    (version "0.0.0-20171120220530-696b145b53b9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ghemawat/stream")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i0cjvhn2zfnvm9dc9nd9yyq27nmv8j2s7sa0lvcdvv2mbcdvvq8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ghemawat/stream"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/ghemawat/stream")
    (synopsis "UNIX pipe-like chained filters")
    (description
     "This Package provides filters that can be chained together in a manner
similar to Unix pipelines.")
    (license license:asl2.0)))

(define-public go-github-com-ghodss-yaml
  (package
    (name "go-github-com-ghodss-yaml")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ghodss/yaml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0skwmimpy7hlh7pva2slpcplnm912rp3igs98xnqmn859kwa5v8g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ghodss/yaml"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2))
    (home-page "https://github.com/ghodss/yaml")
    (synopsis "YAML marshaling and unmarshaling support for Go")
    (description
     "This package provides a wrapper around
@url{https://github.com/go-yaml/yaml, go-yaml} designed to enable a better way
of handling YAML when marshaling to and from structs.

It first converts YAML to JSON using go-yaml and then uses @code{json.Marshal}
and @code{json.Unmarshal} to convert to or from the struct.  This means that
it effectively reuses the JSON struct tags as well as the custom JSON methods
@code{MarshalJSON} and @code{UnmarshalJSON} unlike go-yaml.")
    (license license:expat)))

(define-public go-github-com-git-lfs-go-netrc
  (package
    (name "go-github-com-git-lfs-go-netrc")
    (version "0.0.0-20250218165306-ba0029b43d11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/git-lfs/go-netrc")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11srhd9xjrrkmqxkgzdbfahak6bi1zlm153i6cbl3z0pchrlykqr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/git-lfs/go-netrc"))
    (home-page "https://github.com/git-lfs/go-netrc")
    (synopsis "Netrc file parser for Go")
    (description
     "This package is for reading and writing netrc files.  This package can
parse netrc files, make changes to them, and then serialize them back to netrc
format, while preserving any whitespace that was present in the source file.")
    (license license:expat)))

(define-public go-github-com-gizak-termui-v3
  (package
    (name "go-github-com-gizak-termui-v3")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gizak/termui")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v3k8l5p95kb1v297ra5mw9sxdd59y82y6ibjzya5ma2pry6k5cn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gizak/termui/v3"
      #:unpack-path "github.com/gizak/termui"))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth
           go-github-com-mitchellh-go-wordwrap
           go-github-com-nsf-termbox-go))
    (home-page "https://github.com/gizak/termui")
    (synopsis "Terminal dashboard widget Go library")
    (description
     "The termui Go library draws customizable dashboard widgets in a text
terminal.  It includes several common widgets: lists, trees, tables and tabs,
but also more complex items such as (stacked) bar and pie charts, scatter
plots, gauges, and even images and a canvas for drawing `high resolution'
braille dots.

You can also easily create new custom widgets.  Widgets can be coloured and
styled and positioned absolutely or relatively.  They respond to keyboard,
mouse, and terminal resizing events.")
    (license license:expat)))

(define-public go-github-com-go-cmd-cmd
  (package
    (name "go-github-com-go-cmd-cmd")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-cmd/cmd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n465pnvphvs4jp3mn1krbxb0wcjclfcrif1c5zcir8idj18vsax"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-cmd/cmd"))
    (native-inputs
     (list go-github-com-go-test-deep))
    (home-page "https://github.com/go-cmd/cmd")
    (synopsis "Non-blocking external commands in Go with streaming output")
    (description
     "Package cmd runs external commands with concurrent access to output and
status.  It wraps the Go standard library @code{os/exec.Command} to correctly
handle reading output (STDOUT and STDERR) while a command is running and
killing a command.  All operations are safe to call from multiple
goroutines.")
    (license license:expat)))

;; For delve@1.25.1
(define-public go-github-com-go-delve-liner
  (hidden-package
   (package
     (name "go-github-com-go-delve-liner")
     (version "1.2.3-0.20231231155935-4726ab1d7f62")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-delve/liner")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0f94qx7jzign64gv865whirq9xw7rakxf3wy4y9fsn52bxx408x0"))))
     (build-system go-build-system)
     (arguments
      (list
       #:import-path "github.com/go-delve/liner"))
     (propagated-inputs
      (list go-github-com-mattn-go-runewidth
            go-golang-org-x-sys))
     (home-page "https://github.com/go-delve/liner")
     (synopsis "Command line editor Go library")
     (description
      "This package is an alternative fork of https://github.com/peterh/liner
to build @code{delve} - debugger for the Go programming language.")
     (license license:expat))))

(define-public go-github-com-go-errors-errors
  (package
    (name "go-github-com-go-errors-errors")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-errors/errors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ydwx20al9x99xnki0srb9iy96y638inw05xx5jb16dn8rz09wib"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-errors/errors"
      #:test-flags
      #~(list "-vet=off" ;Go@1.24 forces vet, but tests are not ready yet.
              ;; Stack trace does not contain source line: 'a: b(5)'.
              "-skip" "TestStackFormat")))
    (home-page "https://github.com/go-errors/errors")
    (synopsis "Errors with stacktraces for Golang")
    (description
     "Package errors provides errors that have stack-traces.  It provides the
type @code{*Error} which implements the standard golang error interface, so
you can use this library interchangeably with code that is expecting a normal
error return.")
    (license license:expat)))

(define-public go-github-com-go-git-go-billy-v5
  (package
    (name "go-github-com-go-git-go-billy-v5")
    (version "5.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-git/go-billy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "061yczvpxq5xcnjbi8ywx0qvrispi8pwk3nqky5cv1dsi6xzkchc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-git/go-billy/v5"))
    (native-inputs
     (list go-github-com-onsi-gomega
           go-github-com-stretchr-testify
           go-gopkg-in-check-v1))
    (propagated-inputs
     (list go-github-com-cyphar-filepath-securejoin
           go-golang-org-x-sys))
    (home-page "https://github.com/go-git/go-billy/")
    (synopsis "File system abstraction for Go")
    (description
     "Billy implements an interface based on the OS's standard library to
develop applications without depending on the underlying storage.  This makes
it virtually free to implement mocks and testing over file system
operations.")
    (license license:asl2.0)))

(define-public go-github-com-go-git-go-git-fixtures-v4
  (package
    (name "go-github-com-go-git-go-git-fixtures-v4")
    (version "4.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-git/go-git-fixtures")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d6qs2mzbhz95aflpjh6ijywvb4ys73jvk2v30mickax3gmm2vlw"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: The onpanic: runtime error: makeslice: cap out of range
      #:tests? (target-64bit?)
      #:test-subdirs #~(list ".")
      #:import-path "github.com/go-git/go-git-fixtures/v4"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-git-go-billy-v5
           go-gopkg-in-check-v1))
    (home-page "https://github.com/go-git/go-git-fixtures/")
    (synopsis "Fixtures used by @code{go-git}")
    (description
     "This package provides fixtures used by @code{go-git}.")
    (license license:asl2.0)))

(define-public go-github-com-go-git-go-git-fixtures-v5
  (package
    (inherit go-github-com-go-git-go-git-fixtures-v4)
    (name "go-github-com-go-git-go-git-fixtures-v5")
    (version "5.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-git/go-git-fixtures")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mdwqdmqbqmrh21n25nqrv48zkamgw89parcbyp7k2skqk1jkjpf"))))
    (arguments
     (list
      #:import-path "github.com/go-git/go-git-fixtures/v5"))))

(define-public go-github-com-go-ini-ini
  (package
    (name "go-github-com-go-ini-ini")
    (version "1.67.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ini/ini")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vpzkjmrwp7bqqsijp61293kk2vn6lcck56j8m5y6ks6cf21lpap"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/go-ini/ini"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://gopkg.in/ini.v1")
    (synopsis "INI file read and write functionality in Go")
    (description
     "This package provides INI file read and write functionality in Go.")
    (license license:asl2.0)))

(define-public go-github-com-go-kit-log
  (package
    (name "go-github-com-go-kit-log")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-kit/log")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xjv2g1cd1iaghhm1c1zw0lcz89a9zq5xradyjipvrbqxbxckqm6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-kit/log"))
    (propagated-inputs
     (list go-github-com-go-logfmt-logfmt))
    (home-page "https://github.com/go-kit/log")
    (synopsis "Minimal and extensible structured logger")
    (description
     "This package provides a minimal interface for structured logging in
services.  It may be wrapped to encode conventions, enforce type-safety,
provide leveled logging, and so on.  It can be used for both typical
application log events, and log-structured data streams.")
    (license license:expat)))

(define-public go-github-com-go-logfmt-logfmt
  (package
    (name "go-github-com-go-logfmt-logfmt")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-logfmt/logfmt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s3dz7z5a8p5ia5czihy5y2hkij7rdfyr425sw9rnxqil3d0dlj6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-logfmt/logfmt"))
    (home-page "https://github.com/go-logfmt/logfmt")
    (synopsis "Marshal and unmarshal logfmt messages")
    (description
     "Package logfmt implements utilities to marshal and unmarshal data in the
logfmt format.  The logfmt format records key/value pairs in a way that
balances readability for humans and simplicity of computer parsing.  It is
most commonly used as a more human friendly alternative to JSON for structured
logging.")
    (license license:expat)))

(define-public go-github-com-go-logr-logr
  (package
    (name "go-github-com-go-logr-logr")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-logr/logr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03agibg7i25ppjak1yv4qzvbrv871qnfkmfy17did0h9ml7xh2nx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-logr/logr"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples-and-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples"))
              (delete-file-recursively
               (string-append "src/" import-path "/funcr/example"))
              (delete-file-recursively
               (string-append "src/" import-path "/benchmark")))))))
    (home-page "https://github.com/go-logr/logr")
    (synopsis "Minimal logging API for Go")
    (description
     "Package @code{logr} defines a general-purpose logging API and abstract
interfaces to back that API.  Packages in the Go ecosystem can depend on it,
while callers can implement logging with whatever backend is appropriate.")
    (license license:asl2.0)))

(define-public go-github-com-go-logr-stdr
  (package
    (name "go-github-com-go-logr-stdr")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-logr/stdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dl2rzvjacwqlnvw7azrxqbh4jvzaq8v399f6drs146l39ss21c1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; no tests for stdr.go
      #:import-path "github.com/go-logr/stdr"))
    (propagated-inputs
     (list go-github-com-go-logr-logr))
    (home-page "https://github.com/go-logr/stdr")
    (synopsis "Minimal Go logging using logr and Go's standard library")
    (description
     "Package stdr implements github.com/go-logr/logr.Logger in terms of Go's
standard log package.")
    (license license:asl2.0)))

(define-public go-github-com-go-md2man
  (deprecated-package "go-github-com-go-md2man" go-github-com-cpuguy83-go-md2man-v2))

(define-public go-github-com-go-openapi-inflect
  (package
    (name "go-github-com-go-openapi-inflect")
    (version "0.21.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/inflect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xmayn2qbl8dy7hk60xwwgkacpzv7ssm2s6xqn84kg4bnr6bbvhv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/inflect"))
    (home-page "https://github.com/go-openapi/inflect")
    (synopsis "Pluralize words library for Golang")
    (description
     "This package provides a basic set of functions applying grammar rules to
inflect English words, modify case style (Capitalize, camelCase, snake_case,
etc.).")
    (license license:expat)))

(define-public go-github-com-go-playground-locales
  (package
    (name "go-github-com-go-playground-locales")
    (version "0.14.1")
    (home-page "https://github.com/go-playground/locales")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "117nss5gv7rfzr7z40rkpwfr273wv6ahrd3ycqdarxvaxh0ldhh4"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/go-playground/locales"))
    (propagated-inputs
     (list go-golang-org-x-text))
    (synopsis "Set of locales generated from the CLDR Unicode Project")
    (description
     "This package provides a set of locales generated from the
@uref{http://cldr.unicode.org/, Unicode CLDR Project} which can be used
independently or within an internalization (i18n) package.  Its currently
implemented features include

@itemize
@item Rules generated from the CLDR data, v31.0.3
@item Contains Cardinal, Ordinal and Range Plural Rules
@item Contains Month, Weekday and Timezone translations built in
@item Contains Date & Time formatting functions
@item Contains Number, Currency, Accounting and Percent formatting functions
@item Supports the \"Gregorian\" calendar only
@end itemize")
    (license license:expat)))

(define-public go-github-com-go-playground-universal-translator
  (package
    (name "go-github-com-go-playground-universal-translator")
    (version "0.18.1")
    (home-page "https://github.com/go-playground/universal-translator")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lgz9wrkcfx6q3x6i9fprr8rfwnk0c6x61jgzacgikbmzsl7dw6v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-playground/universal-translator"))
    (propagated-inputs
     (list go-github-com-go-playground-locales))
    (synopsis "Translator using Unicode CLDR data and pluralization rules")
    (description
     "This package offers an Internalization Translator for Go using
@uref{http://cldr.unicode.org/, Unicode CLDR Project} data and pluralization
rules.  Its currently implemented features include

@itemize
@item Rules generated from the CLDR data, v30.0.3
@item Contains Cardinal, Ordinal and Range Plural Rules
@item Contains Month, Weekday and Timezone translations built in
@item Contains Date & Time formatting functions
@item Contains Number, Currency, Accounting and Percent formatting functions
@item Supports the \"Gregorian\" calendar only
@item Support loading translations from files
@item Exporting translations to file(s), mainly for getting them
professionally translated
@end itemize")
    (license license:expat)))

(define-public go-github-com-go-spatial-proj
  (package
    (name "go-github-com-go-spatial-proj")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/go-spatial/proj")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sdjms403rr1smm63p21k95m1dfis06i52y1962jnxidcywzm6i5"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-spatial/proj"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-spatial/proj")
    (synopsis "Port of PROJ.4 projection library in Golang")
    (description
     "Proj is a selective and on-going port of the venerable
@url{http://proj4.org/, PROJ.4} project to the Go language.

This package provides the following subpackages:
@itemize
@item @code{proj} (top-level): the Conversion API
@item @code{proj/cmd/proj}: the simple proj command-line tool
@item @code{proj/core}: the Core API, representing coordinate systems and
conversion operations
@item @code{proj/gie}: a naive implementation of the PROJ.4 gie tool, plus the
full set of PROJ.4 test case files
@item @code{proj/merror}: a little error package
@item @code{proj/mlog}: a little logging package
@item @code{proj/operations}: the actual coordinate operations; these routines
tend to be closest to the original C code
@item @code{proj/support}: misc structs and functions in support of the core
package
@end itemize")
    ;; As it's a port of <http://proj4.org/> all licenses are preserved, see
    ;; LICENSE.md.
    (license (list license:expat license:asl2.0 license:x11))))

(define-public go-github-com-go-sql-driver-mysql
  (package
    (name "go-github-com-go-sql-driver-mysql")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-sql-driver/mysql")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ihdqg411gkv454fwx8w5nbndgkm5dz5phfliksxgmhggyxxm7sn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-sql-driver/mysql"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestConnectorReturnsTimeout"
                             "TestErrorInMultiResult"
                             "TestDSNReformat/user:p"
                             "FuzzFormatDSN/seed#8")
                       "|"))))
    (propagated-inputs
     (list go-filippo-io-edwards25519))
    (home-page "https://github.com/go-sql-driver/mysql")
    (synopsis "MySQL driver for golang")
    (description
     "This is a pure Go implementation of the MySQL API, compatible with
golang's database/sql package.")
    (license license:mpl2.0)))

(define-public go-github-com-go-stack-stack
  (package
    (name "go-github-com-go-stack-stack")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-stack/stack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01m6l9w84yq2yyly8bdfsgc386hla1gn9431c7vr3mfa3bchj5wb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-stack/stack"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestCallStackString"
                             "TestCallStackMarshalText"
                             "TestTrimRuntime")
                       "|"))))
    (home-page "https://github.com/go-stack/stack")
    (synopsis "Utilities to capture, manipulate, and format call stacks")
    (description
     "Package @code{stack} implements utilities to capture, manipulate,
and format call stacks.  It provides a simpler API than package
@code{runtime}.  The implementation takes care of the minutia and special
cases of interpreting the program counter (pc) values returned by
@code{runtime.Callers}.")
    (license license:expat)))

(define-public go-github-com-go-task-slim-sprig
  (let ((commit "afa1e2071829e4db655eb448d6c7c16eb0bc5766")
        (revision "0"))
    (package
      (name "go-github-com-go-task-slim-sprig")
      (version (git-version "2.20.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/go-task/slim-sprig")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1185y8qygv8gb3wpghx5d945wq68j4dbaiffq3h0dh453g4h1w7a"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/go-task/slim-sprig"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'remove-failing-tests
              (lambda* (#:key import-path #:allow-other-keys)
                (delete-file
                 (string-append "src/" import-path "/network_test.go")))))))
      (native-inputs
       (list  go-github-com-stretchr-testify))
      (home-page "https://github.com/go-task/slim-sprig")
      (synopsis "Various useful template functions for Go")
      (description
       "Sprig provides over 100 functions that extend the Go template system.
Slim-Sprig is a fork of Sprig that removes all external dependencies to make
the library more lightweight.")
      (license license:expat))))

(define-public go-github-com-go-task-slim-sprig-v3
  (package
    (inherit go-github-com-go-task-slim-sprig)
    (name "go-github-com-go-task-slim-sprig-v3")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-task/slim-sprig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h6m9n8w6yk0fp1kpk574kac6l3ibkh71myjakvns1nmqphb085w"))))
    (build-system go-build-system)
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-go-task-slim-sprig)
       ((#:import-path _) "github.com/go-task/slim-sprig/v3")))))

(define-public go-github-com-go-viper-mapstructure-v2
  (package
    (name "go-github-com-go-viper-mapstructure-v2")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-viper/mapstructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x13x2s0vkikmn5wcarxskhr6c90s64nkbsgjsh7g9sh4v31n5yw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-viper/mapstructure/v2"))
    (home-page "https://github.com/go-viper/mapstructure")
    (synopsis "Go type converters")
    (description
     "Package mapstructure exposes functionality to convert one arbitrary Go
type into another, typically to convert a @code{map[string]interface{}} into a
native Go structure.")
    (license license:expat)))

(define-public go-github-com-go-yaml-yaml
  (package
    (name "go-github-com-go-yaml-yaml")
    (version "0.0.0-20220527083530-f6f7691b1fde")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-yaml/yaml")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01b0wjb7yzv8wzzz2iim8mjpkwjnykcanrwiq06pkl89lr6gv8hn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-yaml/yaml"))
    (native-inputs (list go-gopkg-in-check-v1))
    (propagated-inputs (list go-gopkg-in-yaml-v3))
    (home-page "https://github.com/go-yaml/yaml")
    (synopsis "YAML support for the Go language")
    (description "Package yaml implements YAML support for the Go language.")
    (license license:asl2.0)))

(define-public go-github-com-gobwas-glob
  (package
    (name "go-github-com-gobwas-glob")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gobwas/glob")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jxk1x806zn5x86342s72dq2qy64ksb3zrvrlgir2avjhwb18n6z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gobwas/glob"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/gobwas/glob")
    (synopsis "Go globbing library")
    (description
     "This package provides a Go implementation of globs.")
    (license license:expat)))

(define-public go-github-com-gobwas-pool
  (package
    (name "go-github-com-gobwas-pool")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gobwas/pool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0imipsf8nslc78in78wcri2ir2zzajp2h543dp2cshrrdbwkybx7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gobwas/pool"))
    (home-page "https://github.com/gobwas/pool")
    (synopsis "Go Pooling Helpers")
    (description
     "Package pool contains helpers for pooling structures distinguishable by
size.")
    (license license:expat)))

(define-public go-github-com-goccy-go-yaml
  (package
    (name "go-github-com-goccy-go-yaml")
    (version "1.18.0")
    (home-page "https://github.com/goccy/go-yaml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0abvpywscsi503nq5a6z1jxhvvhk5gc366nk6xdlslp0gdh4sfhq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/goccy/go-yaml"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/benchmarks")))))))
    (native-inputs
     (list go-github-com-go-playground-validator-v10
           go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-fatih-color
           go-golang-org-x-xerrors))
    (synopsis "YAML support for the Go language")
    (description
     "This package provides features beyond the
@uref{https://github.com/go-yaml/yaml, defacto YAML library} including:

@itemize
@item Pretty format for error notifications
@item Support Scanner or Lexer or Parser as public API
@item Support Anchor and Alias to Marshaler
@item Allow referencing elements declared in another file via anchors
@item Extract value or AST by YAMLPath (YAMLPath is like a JSONPath)
@end itemize")
    (license license:expat)))

(define-public go-github-com-godbus-dbus
  (package
    (name "go-github-com-godbus-dbus")
    (version "0.0.0-20190726142602-4481cbc300e2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/godbus/dbus")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h0cl1r136g0kxbw3i7ggb9mhavpi1yr7d7312iwhkxm93dxkphg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/godbus/dbus"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "dbus-run-session" "--"
                          "go" "test" "./..."
                          ;; Disable tests which require a system D-Bus
                          ;; instance.
                          "-skip" "TestSystemBus|TestConnectSystemBus"))))))))
    (native-inputs
     (list dbus)) ;dbus-launch
    (home-page "https://github.com/godbus/dbus/")
    (synopsis "Native Go client bindings for the D-Bus")
    (description
     "@code{dbus} is a library that implements native Go client bindings for
the D-Bus message bus system.")
    (license license:bsd-2)))

(define-public go-github-com-godbus-dbus-v5
  (package
    (inherit go-github-com-godbus-dbus)
    (name "go-github-com-godbus-dbus-v5")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/godbus/dbus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kayd4x7idrhi06ahh5kqkgwzgh9icvv71mjar2d0jl486dfs8r5"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-godbus-dbus)
       ((#:import-path _ "github.com/godbus/dbus")
        "github.com/godbus/dbus/v5")))))


(define-public go-github-com-gofrs-flock
  (package
    (name "go-github-com-gofrs-flock")
    (version "0.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gofrs/flock/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kfnbcahr9x61k40wsrqzxxr3ybix0jqsm4ibpjgnhfgrln7ag8v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gofrs/flock"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/gofrs/flock/")
    (synopsis "Thread-safe file locking library in Go")
    (description
     "@code{flock} implements a thread-safe file lock.  It also includes a
non-blocking @code{TryLock} function to allow locking without blocking
execution.")
    (license license:bsd-3)))

(define-public go-github-com-gofrs-uuid
  (package
    (name "go-github-com-gofrs-uuid")
    (version "4.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gofrs/uuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13n5q70sdw39f2j9ya5l6n7475fv5b1vns7skv56yjh04chwl7p3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gofrs/uuid"))
    (home-page "https://github.com/gofrs/uuid")
    (synopsis "UUID package for Golang")
    (description
     "Package uuid provides implementations of the Universally Unique Identifier
(UUID), as specified in RFC-4122 and the Peabody RFC Draft (revision 03).")
    (license license:expat)))

(define-public go-github-com-gofrs-uuid-v5
  (package
    (inherit go-github-com-gofrs-uuid)
    (name "go-github-com-gofrs-uuid-v5")
    (version "5.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gofrs/uuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fkyclwji4llkpcwwvpn00fc68mawdlz8ssf2x1vrmv33gps4icl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gofrs/uuid/v5"))))

(define-public go-github-com-gogs-chardet
  (package
    (name "go-github-com-gogs-chardet")
    (version "0.0.0-20211120154057-b7413eaefb8f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gogs/chardet")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12j8q5wc9m4n51v2j2m40nahqdl9bh3hzpdp26clzq91kc2amiz0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gogs/chardet"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/gogs/chardet")
    (synopsis "Character set detection for Go")
    (description
     "The chardet package ports character set detection from ICU to Go.")
    (license license:expat)))

(define-public go-github-com-golang-freetype
  (package
    (name "go-github-com-golang-freetype")
    (version "0.0.0-20170609003504-e2365dfdc4a0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/freetype")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "194w3djc6fv1rgcjqds085b9fq074panc5vw582bcb8dbfzsrqxc"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/golang/freetype"))
    (propagated-inputs
     (list go-golang-org-x-image))
    (home-page "https://github.com/golang/freetype")
    (synopsis "Freetype font rasterizer in the Go programming language")
    (description
     "The Freetype font rasterizer in the Go programming language.")
    (license (list license:freetype
                   license:gpl2+))))

(define-public go-github-com-golangplus-bytes
  (package
    (name "go-github-com-golangplus-bytes")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golangplus/bytes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r8f9m7hm6idnbapfb01nw5h09a20b09r7sky2gkniympn98ra75"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/golangplus/bytes"))
    (native-inputs
     (list go-github-com-golangplus-testing-bootstrap))
    (home-page "https://github.com/golangplus/bytes")
    (synopsis "Extension to Golang standard @code{bytes} library.")
    (description
     "Package bytesp is a plus to the standard @code{bytes} package.")
    (license license:bsd-3)))

(define-public go-github-com-golangplus-bytes-bootstrap
  (hidden-package
   (package
     (inherit go-github-com-golangplus-bytes)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "github.com/golangplus/bytes"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-github-com-gologme-log
  (package
    (name "go-github-com-gologme-log")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gologme/log")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m7dszaisviq7fgwyxg9lwhxyzdab0w7d1zbilrnarzvyx9wh3ax"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gologme/log"))
    (home-page "https://github.com/gologme/log/")
    (synopsis "Fork of the golang built in log package to add support for levels")
    (description
     "This package is a drop in replacement for the built-in Go log package.
All the functionality of the built-in package still exists and is unchanged.
This package contains a series of small enhancements and additions.")
    (license license:bsd-3)))

(define-public go-github-com-gomarkdown-markdown
  (package
    (name "go-github-com-gomarkdown-markdown")
    (version "0.0.0-20250311123330-531bef5e742b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gomarkdown/markdown")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lw38q8izwflfkqxpr89p0sbriqgbzr240mfzhba2fkk3365y3xs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gomarkdown/markdown"
      #:test-flags #~(list "-skip" "TestRenderCodeBlock")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples"))))
          (add-after 'check 'remove-testdata
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "testdata")))))))
    (home-page "https://github.com/gomarkdown/markdown")
    (synopsis "Markdown Parser and HTML Renderer for Go")
    (description
     "Package markdown implements markdown parser and HTML renderer.")
    (license license:bsd-2)))

(define-public go-github-com-gomodule-redigo
  (package
    (name "go-github-com-gomodule-redigo")
    (version "1.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gomodule/redigo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03z02zmkl8cj73c6xrvlpj144d9ysikc25ay64dhpbzwkn16h5yv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/gomodule/redigo"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Run just unit tests which do not require Redis reducing
               ;; closure size.
               (list "TestDoCommands"
                     "TestPipelineCommands"
                     "TestBlankCommand"
                     "TestRecvBeforeSend"
                     "TestError"
                     "TestDialContext_CanceledContext"
                     "TestDialClientName"
                     "TestExecError"
                     "Test.*Pool.*"
                     "TestPushed"
                     "TestPubSubReceiveContext"
                     "TestSlowLog"
                     "TestLatency"
                     "TestLatencyHistories"
                     "TestScript"
                     "Example.*"
                     "TestConnMux"
                     "TestConnMuxClose")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (list "redis/zpop_example_test.go"
                                "redis/scan_test.go"
                                "redis/pubsub_example_test.go"
                                "redis/reply_test.go"))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/gomodule/redigo")
    (synopsis "Go client for Redis")
    (description
     "Redigo is a Go client for the Redis database.")
    (license license:asl2.0)))

(define-public go-github-com-google-cadvisor
  (let ((commit "2ed7198f77395ee9a172878a0a7ab92ab59a2cfd")
        (revision "0"))
    (package
      (name "go-github-com-google-cadvisor")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/cadvisor")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1w8p345z5j0gk3yiq5ah0znd5lfh348p2s624k5r10drz04p3f55"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/google/cadvisor"
        #:test-subdirs #~(list ".")))
      (home-page "https://github.com/google/cadvisor")
      (synopsis "Analyze resource usage of running containers")
      (description
       "The package provides @code{cadvisor}, which provides information about
the resource usage and performance characteristics of running containers.")
      (license license:asl2.0))))

(define-public go-github-com-google-cel-go
  (package
    (name "go-github-com-google-cel-go")
    (version "0.26.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/google/cel-go")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hrdr9fzac0p7jrlbchz2qvdikr1szq5rg4sdsld7n849mmz8ypf"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/google/cel-go/codelab
            ;; - github.com/google/cel-go/conformance
            ;; - github.com/google/cel-go/policy
            ;; - github.com/google/cel-go/repl/appengine
            ;; - github.com/google/cel-go/repl
            ;; - github.com/google/cel-go/tools
            (for-each delete-file-recursively
                      (list "codelab"
                            "conformance"
                            "policy"
                            "repl/appengine"
                            "repl"
                            "tools"
                            "vendor"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/google/cel-go"
      #:test-flags #~(list "-skip" "TestStringFormat")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (propagated-inputs
     (list go-cel-dev-expr
           go-github-com-antlr4-go-antlr-v4
           go-github-com-stoewer-go-strcase
           go-golang-org-x-text
           go-google-golang-org-genproto-googleapis-api
           go-google-golang-org-protobuf
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/google/cel-go")
    (synopsis "Common Expression Language")
    (description
     "The Common Expression Language (CEL) is a non-Turing complete language
designed for simplicity, speed, safety, and portability.  CEL's C-like
@url{https://github.com/google/cel-spec, syntax} looks nearly identical to
equivalent expressions in C++, Go, Java, and TypeScript.")
    (license license:asl2.0)))

(define-public go-github-com-google-gnostic-models
  (package
    (name "go-github-com-google-gnostic-models")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/google/gnostic-models")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xnqzkb25m3q425ld386mb5d34p0wfqnsjy7b5ss2r1mqbr38k6i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/google/gnostic-models"))
    (propagated-inputs
     (list go-go-yaml-in-yaml-v3
           go-google-golang-org-protobuf))
    (home-page "https://github.com/google/gnostic-models")
    (synopsis "Protocol Buffer models for Gnostic")
    (description
     "This package provides Protocol Buffer models and associated libraries
for working with API description formats supported by
@url{https://github.com/google/gnostic,gnostic}.  It exists to provide a
lightweight distribution of these models with minimal dependencies.")
    (license license:asl2.0)))

(define-public go-github-com-google-gops
  (package
    (name "go-github-com-google-gops")
    (version "0.3.28")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/gops")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dbnpnvf624mygajxn13qcdh9nx23lr70p3x2y6xaz4jnprkilqw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/google/gops"
      #:phases
      #~(modify-phases %standard-phases
          ;; mkdir /homeless-shelter: permission denied
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list go-github-com-shirou-gopsutil-v3
           go-github-com-spf13-cobra
           go-github-com-xlab-treeprint
           go-golang-org-x-sys
           go-rsc-io-goversion))
    (home-page "https://github.com/google/gops")
    (synopsis "Listing and diagnosing Go processes tool")
    (description
     "This package implements a functionalit to list currently running Go
processes.")
    (license license:bsd-3)))

(define-public go-github-com-google-goterm
  (package
    (name "go-github-com-google-goterm")
    (version "0.0.0-20200907032337-555d40f16ae2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/goterm")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0809sf02dhg2bjhsz43pmlb5d7nbsnwxls3lw01zw5p7ri9bqwfb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/goterm/term"
      #:unpack-path "github.com/google/goterm"))
    (home-page "https://github.com/google/goterm/")
    (synopsis "PTY creation and termios get/set attributes")
    (description
     "The term package implements PTY creation and termios get/set attributes.
It also contains some convenience functions for colors, SSH to and from
termios translations, readCh, reading passwords, etc.")
    (license license:bsd-3)))

;; XXX: This repository has been archived by the owner on Dec 29, 2022. It is
;; now read-only.  It's only used by kiln, consider to remove it when it does
;; no longer require it.
(define-public go-github-com-google-shlex
  (package
    (name "go-github-com-google-shlex")
    (version "0.0.0-20191202100458-e7afc7fbc510")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/shlex")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14z8hqyik910wk2qwnzgz8mjsmiamxa0pj55ahbv0jx6j3dgvzfm"))))
    (build-system go-build-system)
    (arguments (list #:import-path "github.com/google/shlex"))
    (home-page "https://github.com/google/shlex")
    (synopsis "Simple lexer for Go")
    (description
     "@code{shlex} implements a simple lexer which splits input into tokens
using shell-style rules for quoting and commenting.")
    (license license:asl2.0)))

(define-public go-github-com-google-subcommands
  (package
    (name "go-github-com-google-subcommands")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/subcommands")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00w7fx92696z5p3isvpg71b4023g8f686xnhy56k08vc2q1r2hhw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/subcommands"))
    (home-page "https://github.com/google/subcommands")
    (synopsis "Go subcommand library")
    (description
     "@code{subcommands} implements a functionality for a single command to
have many subcommands, each of which takes arguments.")
    (license license:asl2.0)))

(define-public go-github-com-gookit-color
  (package
    (name "go-github-com-gookit-color")
    (version "1.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gookit/color")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "012naz084chvdqzrrzv9pklqfh259hi2jcp2f3n39fppvjwmzgkf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gookit/color"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                ;; Error: Received unexpected
                ;; error: open README.md: permission denied.
                ;; Reported upstream, see
                ;; <https://github.com/gookit/color/pull/91>.
                (substitute* "utils_test.go"
                  (("os.O_WRONLY") "os.O_RDONLY"))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-xo-terminfo
           go-golang-org-x-sys))
    (home-page "https://github.com/gookit/color")
    (synopsis "Terminal color rendering library")
    (description
     "This package provides a command-line color library with 16/256/True
color support, universal API methods and Windows support.

Features:
@itemize
@item supports rich color output: 16-color (4-bit), 256-color (8-bit), true
color (24-bit, RGB)
@item support converts HEX HSL value to RGB color
@item generic API methods: @code{Print}, @code{Printf}, @code{Println},
@code{Sprint}, @code{Sprintf}
@item supports HTML tag-style color rendering, such as @code{<green>message</>
<fg=red;bg=blue>text</>}
@item basic colors: @code{Bold}, @code{Black}, @code{White}, @code{Gray},
@code{Red}, @code{Green}, @code{Yellow}, @code{Blue}, @code{Magenta},
@code{Cyan}
@item additional styles: @code{Info}, @code{Note}, @code{Light}, @code{Error},
@code{Danger}, @code{Notice}, @code{Success}, @code{Comment}, @code{Primary},
@code{Warning}, @code{Question}, @code{Secondary}
@item support by set @code{NO_COLOR} for disable color or use
@code{FORCE_COLOR} for force open color render
@item support RGB, 256, 16 color conversion
@end itemize")
    (license license:expat)))

(define-public go-github-com-goreleaser-fileglob
  (package
    (name "go-github-com-goreleaser-fileglob")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/goreleaser/fileglob")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c4p98prb0gf8a8789lxp1qw0v6anlqk5b1ff2r861gv2nclp8dx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/goreleaser/fileglob"))
    (native-inputs
     (list go-github-com-caarlos0-testfs
           go-github-com-matryer-is))
    (propagated-inputs
     (list go-github-com-gobwas-glob))
    (home-page "https://github.com/goreleaser/fileglob")
    (synopsis "Golang file globbing library")
    (description
     "This package provides a filesystem glob API.  It uses @code{gobwas/glob}
underneath and returns only matching files or directories, depending on the
configuration.")
    (license license:expat)))

(define-public go-github-com-gorhill-cronexpr
  (package
    (name "go-github-com-gorhill-cronexpr")
    (version "0.0.0-20180427100037-88b0669f7d75")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorhill/cronexpr")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hc7xdfclp2qgkr1581jb3ckjvl34nxmqrnraci5jzmqx5av9j1r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gorhill/cronexpr"))
    (home-page "https://github.com/gorhill/cronexpr")
    (synopsis "Cron expression parser in the Go language")
    (description
     "This package provides a cron expression parser in the Go language.
Given a cron expression and a time stamp, you can get the next time stamp
which satisfies the cron expression.")
    (license (list license:gpl3+
                   license:asl2.0))))

(define-public go-github-com-gosexy-gettext
  (package
    (name "go-github-com-gosexy-gettext")
    (version "0.0.0-20160830220431-74466a0a0c4a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gosexy/gettext")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0asphx8nd7zmp88wk6aakk5292np7yw73akvfdvlvs9q5r5ahkgi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gosexy/gettext"
      ;; Assertion errors in two tests.
      #:test-flags #~(list "-skip" "TestSpanish|TestDeutsch")))
    (native-inputs
     (list go-github-com-jessevdk-go-flags
           go-github-com-stretchr-testify
           go-gopkg-in-check-v1))
    (home-page "https://github.com/gosexy/gettext")
    (synopsis "Golang bindings for GNU's gettext")
    (description "Package gettext provides bindings for @url{GNU Gettext,
https://www.gnu.org/software/gettext/}.")
    (license license:expat)))

(define-public go-github-com-gosuri-uilive
  (package
    (name "go-github-com-gosuri-uilive")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gosuri/uilive")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pwxx0w4mv908dascnxkdjq865ks01niqy71imv4kllz0a84zkag"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gosuri/uilive"))
    (home-page "https://github.com/gosuri/uilive")
    (synopsis "Updating terminal output in realtime")
    (description
     "Package uilive provides a writer that live updates the terminal.  It
provides a buffered io.Writer that is flushed at a timed interval.")
    (license license:expat)))

(define-public go-github-com-goverter-patherr
  (package
    (name "go-github-com-goverter-patherr")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/goverter/patherr")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zjcyva959ir23rgk2rvc7ivlyan9dh2rqw7hl03h6xq935zznjq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/goverter/patherr"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/goverter/patherr")
    (synopsis "Implementation for Goverter's wrapErrorsUsing")
    (description
     "This package provides an implementation for the goverter feature
@code{wrapErrorsUsing}.")
    (license license:expat)))

(define-public go-github-com-gowebpki-jcs
  (package
    (name "go-github-com-gowebpki-jcs")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gowebpki/jcs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cgclrxdkxrbch0791m4ijs9a4g4zn355k8griavaadjy87l3qq8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gowebpki/jcs"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/gowebpki/jcs")
    (synopsis "JSON Canonicalization")
    (description
     "Package jcs transforms UTF-8 JSON data into a canonicalized version
according @@url{https://rfc-editor.org/rfc/rfc8785.html, RFC 8785}.")
    (license license:asl2.0)))

(define-public go-github-com-guptarohit-asciigraph
  (package
    (name "go-github-com-guptarohit-asciigraph")
    (version "0.7.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/guptarohit/asciigraph")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j708hj80hk1b39zbdfx6kqy75i70jhz55bml0jngqwfx698d1pv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/guptarohit/asciigraph"))
    (home-page "https://github.com/guptarohit/asciigraph")
    (synopsis "ASCII line graphs for Golang")
    (description
     "This package can generate ASCII line graphs in Golang.")
    (license license:bsd-3)))

(define-public go-github-com-h2non-filetype
  (package
    (name "go-github-com-h2non-filetype")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/h2non/filetype")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05xad4dfimlccg3hy0fpcns4fl7yj17dbpdf6ijwbp6k4ryzy9cm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/h2non/filetype"))
    (home-page "https://github.com/h2non/filetype")
    (synopsis "Infer binary file types based on the magic numbers header signature")
    (description
     "This package implements a functionality to infer file and MIME type
checking the @url{https://en.wikipedia.org/wiki/Magic_number_(programming)
,magic numbers} signature.")
    (license license:expat)))

(define-public go-github-com-h2non-parth
  (package
    (name "go-github-com-h2non-parth")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/h2non/parth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aj37m7z6wbi397g38jni54n3c7yy8nljc40ksy8am213lil5aqc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/h2non/parth"))
    (home-page "https://github.com/h2non/parth")
    (synopsis "Path parsing library for segment unmarshaling and slicing")
    (description
     "The @code{parth} Go library provides path parsing for segment
unmarshaling and slicing.  In other words, parth provides simple and flexible
access to (URL) path parameters.")
    (license license:expat)))

(define-public go-github-com-hanwen-go-fuse
  (package
    (name "go-github-com-hanwen-go-fuse")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hanwen/go-fuse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04xa8mh34639lv1b2p8dx13x742j5i493i3sk89hd3jfskzvval1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/hanwen/go-fuse"
      ;; Most of the tests require "/bin/fusermount" to be available which
      ;; is missed during packaging, limit to some unit tests only.
      #:test-subdirs #~(list "internal/..." "splice")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-example
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "example")))))))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/hanwen/go-fuse")
    (synopsis "Go bindings for FUSE filesystems")
    (description
     "This is a repository containing Go bindings for writing FUSE file systems.")
    (license license:bsd-3)))

(define-public go-github-com-hanwen-go-fuse-v2
  (package
    (inherit go-github-com-hanwen-go-fuse)
    (name "go-github-com-hanwen-go-fuse-v2")
    (version "2.5.0")  ;check go,mod in gocryptfs before upgrading
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hanwen/go-fuse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wgx8gwimgf7rp7j23cl6bxx1gzzarda2kqz91i64ydc124jsqmr"))))
    (build-system go-build-system)
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-hanwen-go-fuse)
       ((#:import-path _) "github.com/hanwen/go-fuse/v2")
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'remove-benchmark
              (lambda* (#:key import-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" import-path)
                  (delete-file-recursively "benchmark"))))
            (add-after 'unpack 'fix-paths
              (lambda* (#:key import-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" import-path)
                  (let* ((fusermount3 "/run/setuid-programs/fusermount3"))
                    (substitute* "fuse/mount_linux.go"
                      (("bin, err := fusermountBinary[(][)]")
                       (format #f "bin, err := ~s, nil" fusermount3)))))))))))
    (inputs
     (list fuse))
    (propagated-inputs
     (list go-github-com-kylelemons-godebug
           go-github-com-moby-sys-mountinfo
           go-golang-org-x-sync
           go-golang-org-x-sys))))

(define-public go-github-com-hashicorp-errwrap
  (package
    (name "go-github-com-hashicorp-errwrap")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/errwrap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p5wdz8p7dmwphmb33gwhy3iwci5k9wkfqmmfa6ay1lz0cqjwp7a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/errwrap"))
    (home-page "https://github.com/hashicorp/errwrap")
    (synopsis "Wrapping and querying errors for Golang")
    (description
     "@code{errwrap} is a package for Go that formalizes the pattern of
wrapping errors and checking if an error contains another error.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-envparse
  (package
    (name "go-github-com-hashicorp-go-envparse")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-envparse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wqb96gbfrj5986a1cg1sa0412dk7qpnh9l2w3y5q7a38v2dlh9j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-envparse"))
    (home-page "https://github.com/hashicorp/go-envparse")
    (synopsis "Environment variable parser for Golang")
    (description
     "Package envparse is a minimal environment variable parser.  It handles
empty lines, comments, single quotes, double quotes, and JSON escape
sequences.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-hclog
  (package
    (name "go-github-com-hashicorp-go-hclog")
    (version "1.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-hclog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nh70591hcxjvxdfkq2is437rvnrnf37mdgpl31qj47fdffclsjs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-hclog"))
    (propagated-inputs
     (list go-github-com-fatih-color
           go-github-com-mattn-go-isatty
           go-golang-org-x-tools))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/hashicorp/go-hclog")
    (synopsis "Key/value logging interface for Go")
    (description
     "This package provides a simple key/value logging interface for Golang
for use in development and production environments.  Unlike the standard
library @code{log} package, this package provides logging levels that provide
decreased output based upon the desired amount of output.  It also comes with
a command-line program @code{hclogvet} that can be used to check that the logging level
methods on @code{hclog.Logger} are used correctly.")
    (license license:expat)))

(define-public go-github-com-hashicorp-go-immutable-radix
  (package
    (name "go-github-com-hashicorp-go-immutable-radix")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-immutable-radix")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s7sf8y5lj8rx4gdymrz29gg6y2xwksfpgniaz32yzcmg3c817zb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-immutable-radix"))
    (propagated-inputs
     (list go-github-com-hashicorp-golang-lru
           go-github-com-hashicorp-go-uuid))
    (home-page "https://github.com/hashicorp/go-immutable-radix")
    (synopsis "Immutable radix tree implementation in Golang")
    (description
     "This package implements an immutable
@url{http://en.wikipedia.org/wiki/Radix_tree,radix tree}.  It only provides a
single @code{Tree} implementation, optimized for sparse nodes.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-immutable-radix-v2
  (package
    (inherit go-github-com-hashicorp-go-immutable-radix)
    (name "go-github-com-hashicorp-go-immutable-radix-v2")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-immutable-radix")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sik2rg6xldk87aqzx9dmpg4f2cwkk7ypdwsjqq131npx7jszxzr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-immutable-radix/v2"))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs
                     go-github-com-hashicorp-go-immutable-radix)
       (prepend go-golang-org-x-exp)
       (replace "go-github-com-hashicorp-golang-lru"
         go-github-com-hashicorp-golang-lru-v2)))))

(define-public go-github-com-hashicorp-go-memdb
  (package
    (name "go-github-com-hashicorp-go-memdb")
    (version "1.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-memdb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1faz0sr9f82zz0vgxsh131b7swi6a3yrsgbw72y45cm2k8bxviad"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-memdb"))
    (propagated-inputs
     (list go-github-com-hashicorp-go-immutable-radix))
    (home-page "https://github.com/hashicorp/go-memdb")
    (synopsis "Golang in-memory database built on immutable radix trees")
    (description
     "This package implements a simple in-memory database built on immutable
@url{https://en.wikipedia.org/wiki/Radix_tree, radix trees}.  The database
provides Atomicity, Consistency and Isolation from ACID.  Being that it is
in-memory, it does not provide durability.  The database is instantiated with
a schema that specifies the tables and indices that exist and allows
transactions to be executed.  The database provides the following:
@acronym{Multi-Version Concurrency Control, MVCC}, transaction support, rich
indexing, watches.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-msgpack-v2
  (package
    (name "go-github-com-hashicorp-go-msgpack-v2")
    (version "2.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/hashicorp/go-msgpack")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y87vqmzysbsvdi6di7dq8az4fswm8lsbxxcq54dnhrg40g4y3nd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/hashicorp/go-msgpack/v2"))
    (propagated-inputs
     (list go-golang-org-x-tools))
    (home-page "https://github.com/hashicorp/go-msgpack")
    (synopsis "MessagePack implementation in Golang")
    (description
     "This package provides a High Performance, Feature-Rich Idiomatic
codec/encoding library for msgpack, JSON.

Supported Serialization formats are:
@itemize
@item msgpack: https://github.com/msgpack/msgpack
@item json: http://json.org http://tools.ietf.org/html/rfc7159
@end itemize")
    (license license:expat)))

(define-public go-github-com-hashicorp-go-multierror
  (package
    (name "go-github-com-hashicorp-go-multierror")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-multierror")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l4s41skdpifndn9s8y6s9vzgghdzg4z8z0lld9qjr28888wzp00"))))
    (build-system go-build-system)
    (propagated-inputs (list go-github-com-hashicorp-errwrap))
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-multierror"))
    (home-page "https://github.com/hashicorp/go-multierror")
    (synopsis "Representing a errors list as a single error for Golang")
    (description
     "@code{go-multierror} is Golang module providing a mechanism for
representing a list of @code{error} values as a single @code{error}.  It is
fully compatible with the standard @code{errors} package, including
the functions @code{As}, @code{Is}, and @code{Unwrap}.  This provides a
standardized approach for introspecting on error values.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-syslog
  (package
    (name "go-github-com-hashicorp-go-syslog")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-syslog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09vccqggz212cg0jir6vv708d6mx0f9w5bxrcdah3h6chgmal6v1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-syslog"))
    (home-page "https://github.com/hashicorp/go-syslog")
    (synopsis "Golang syslog wrapper, cross-compile friendly")
    (description
     "This package is a very simple wrapper around log/syslog")
    (license license:expat)))

(define-public go-github-com-hashicorp-go-uuid
  (package
    (name "go-github-com-hashicorp-go-uuid")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-uuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wd4maaq20alxwcvfhr52rzfnwwpmc2a698ihyr0vfns2sl7gkzk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-uuid"))
    (home-page "https://github.com/hashicorp/go-uuid")
    (synopsis "Generate UUID-format strings")
    (description
     "This package generates UUID-format strings using high quality bytes.
It is not intended to be RFC compliant, merely to use a well-understood string
representation of a 128-bit value.  It can also parse UUID-format strings into
their component bytes.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-version
  (package
    (name "go-github-com-hashicorp-go-version")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/hashicorp/go-version")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04kb102igwnp03rcjjlg7w2lb2dbr7h0w751w72v6imid51kyrsj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-version"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/hashicorp/go-version")
    (synopsis "Parsing and verifying versions for Golang")
    (description
     "This package is a library for parsing versions and version constraints,
and verifying versions against a set of constraints.  It can sort a collection
of versions properly, handles prerelease/beta versions, can increment
versions.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-golang-lru
  (package
    (name "go-github-com-hashicorp-golang-lru")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/golang-lru")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13q3mdlr4hb2cxa5k4ccpz1gg4swrmkxm7h3brq3xsawidpbjbyb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/golang-lru"))
    (home-page "https://github.com/hashicorp/golang-lru")
    (synopsis "Golang LRU cache")
    (description
     "@code{lru} is a package which implements a fixed-size thread safe
@acronym{Least recently used,LRU} cache.  It is based on the cache in
Groupcache.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-golang-lru-arc-v2
  (package
    (name "go-github-com-hashicorp-golang-lru-arc-v2")
    (version "2.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/golang-lru")
             (commit (string-append "arc/v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jin9spx8mv3ynnnyplfmf7plxkym398aaqq04i7zklb716ld4gq"))
       (modules '((guix build utils)))
       (snippet
        ;; It's a helper for go-build-system to compile import-path and
        ;; unpack-path when it struggles to find module.
        #~(begin
            (mkdir "arc/v2")
            (for-each (lambda (f)
                        (rename-file f (string-append "arc/v2/" (basename f))))
                      (find-files  "./arc"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/golang-lru/arc/v2"
      #:unpack-path "github.com/hashicorp/golang-lru"))
    (propagated-inputs
     (list go-github-com-hashicorp-golang-lru-v2))
    (home-page "https://github.com/hashicorp/golang-lru")
    (synopsis "Adaptive Replacement Cache")
    (description
     "@acronym{Adaptive Replacement Cache,ARC} is an enhancement over the
standard LRU cache in that tracks both frequency and recency of use.  This
avoids a burst in access to new entries from evicting the frequently used
older entries.  It adds some additional tracking overhead to a standard LRU
cache, computationally it is roughly 2x the cost, and the extra memory
overhead is linear with the size of the cache.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-golang-lru-v2
  (package
    (inherit go-github-com-hashicorp-golang-lru)
    (name "go-github-com-hashicorp-golang-lru-v2")
    (version "2.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/golang-lru")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lb2ylv2bz6lsqhn6c2hsafjjcx0hsdbah6arhb778g3xbkpgvf3"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodule(s) with their own go.mod files and packed as
            ;; separated packages:
            ;;
            ;; - github.com/hashicorp/golang-lru/arc/v2
            (for-each delete-file-recursively
                      (list "arc"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/golang-lru/v2"))))

(define-public go-github-com-hashicorp-hcl
  (package
    (name "go-github-com-hashicorp-hcl")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/hcl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q6ml0qqs0yil76mpn4mdx4lp94id8vbv575qm60jzl1ijcl5i66"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/hcl"
      #:test-subdirs
      #~(list "json/..." "hcl/ast" "hcl/fmtcmd" "hcl/printer" "hcl/scanner"
              "hcl/strconv" "hcl/token" ".")))
    (native-inputs
     (list go-github-com-davecgh-go-spew))
    (synopsis "Go implementation of HashiCorp Configuration Language V1")
    (description
     "This package contains the main implementation of the @acronym{HCL,
HashiCorp Configuration Language}.  HCL is designed to be a language for
expressing configuration which is easy for both humans and machines to read.")
    (home-page "https://github.com/hashicorp/hcl")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-hcl-v2
  (package
    (name "go-github-com-hashicorp-hcl-v2")
    (version "2.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/hcl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y457prckv5pdglxxc61fcslmswm35c6swwgcrdvfmjgw286y56i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/hashicorp/hcl/v2"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list "TestExpressionParseAndValue/.*unk.*"
                     "TestFunctionCallExprValue/valid_call_with_unknown_arg.*"
                     "TestFunctionCallExprValue/valid_call_with_dynamic_arg")
               "|"))))
    (native-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-go-test-deep
           go-github-com-spf13-pflag
           go-github-com-zclconf-go-cty-debug))
    (propagated-inputs
     (list go-github-com-agext-levenshtein
           go-github-com-apparentlymart-go-textseg-v13
           go-github-com-mitchellh-go-wordwrap
           go-github-com-zclconf-go-cty
           go-golang-org-x-tools))
    (synopsis "Go implementation of HashiCorp Configuration Language V2")
    (description
     "This package contains the main implementation of the @acronym{HCL,
HashiCorp Configuration Language}.  HCL is designed to be a language for
expressing configuration which is easy for both humans and machines to read.")
    (home-page "https://github.com/hashicorp/hcl")
    (license license:mpl2.0)))

(define-public go-github-com-hdrhistogram-hdrhistogram-go
  (package
    (name "go-github-com-hdrhistogram-hdrhistogram-go")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HdrHistogram/hdrhistogram-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l12j3dvljp868p1d2izpiq7ysll05fchvxij8zb8r160lmqv58r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/HdrHistogram/hdrhistogram-go"))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-gonum-org-v1-gonum))
    (home-page "https://github.com/HdrHistogram/hdrhistogram-go")
    (synopsis "Gil Tene's HDR Histogram in Golang")
    (description
     "This package provides an implementation of
@url{https://github.com/HdrHistogram/HdrHistogram, Gil Tene's HDR Histogram}
data structure.  The HDR Histogram allows for fast and accurate analysis of
the extreme ranges of data with non-normal distributions, like latency.")
    (license license:expat)))

(define-public go-github-com-hebcal-gematriya
  (package
    (name "go-github-com-hebcal-gematriya")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hebcal/gematriya")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xmnb2i80dy380yv8c4pd04bbyqgbc7c40p8hz1vqj2lhbm6jabf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hebcal/gematriya"))
    (home-page "https://github.com/hebcal/gematriya")
    (synopsis "Print numbers as Hebrew letters in Go")
    (description
     "This package provides a Go library for printing numbers as Hebrew
letters.")
    (license license:bsd-2)))

(define-public go-github-com-hebcal-greg
  (package
    (name "go-github-com-hebcal-greg")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hebcal/greg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cf53ay6sj0m635k9p3f64iw03asx81aqmyrk102lzmhg838mq1a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hebcal/greg"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/hebcal/greg")
    (synopsis "Converts between Gregorian dates and Rata Die day numbers")
    (description
     "Hebcal's greg package converts between Gregorian dates and R.D. (Rata
Die) day numbers.")
    (license license:gpl2)))

(define-public go-github-com-hebcal-hdate
  (package
    (name "go-github-com-hebcal-hdate")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hebcal/hdate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i3r3ng9ygd7ly59znw948im4z4sbp23jqj3hhgxabwhh6b8mdmf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hebcal/hdate"))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs (list go-github-com-hebcal-greg))
    (home-page "https://github.com/hebcal/hdate")
    (synopsis "Converts between Hebrew and Gregorian dates")
    (description
     "Hebcal's hdate package converts between Hebrew and Gregorian dates.")
    (license license:gpl2)))

(define-public go-github-com-hebcal-hebcal-go
  (package
    (name "go-github-com-hebcal-hebcal-go")
    (version "0.9.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hebcal/hebcal-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vccmjb4g8g8x733a45g78lz25k2a0avsq4zvlp94varssk8wj1q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/hebcal/hebcal-go"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-dustin-go-humanize
           go-github-com-hebcal-gematriya
           go-github-com-hebcal-greg
           go-github-com-hebcal-hdate
           go-github-com-nathan-osman-go-sunrise))
    (home-page "https://github.com/hebcal/hebcal-go")
    (synopsis "Go library for the Hebcal perpetual Jewish calendar")
    (description
     "This package provides a library for conversion between Hebrew and
Gregorian dates, and generation of lists of Jewish holidays for a given year.
Shabbat and holiday candle lighting and havdalah times are approximated based
on location.

Torah readings, Daf Yomi, and counting of the Omer can also be specified.
Algorithms are included to calculate yahrzeits, birthdays, and
anniversaries.")
    (license license:gpl2+)))

(define-public go-github-com-hhrutter-tiff
  (package
    (name "go-github-com-hhrutter-tiff")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hhrutter/tiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09fzgvxwkd34izbfd26ln8vdbhc4j9gxpar3s7h9h125psrjvg0k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hhrutter/tiff"))
    (propagated-inputs (list go-golang-org-x-image go-github-com-hhrutter-lzw))
    (home-page "https://github.com/hhrutter/tiff")
    (synopsis "Extended version of @code{golang.org/x/image/tiff}")
    (description "This package is an enhanced version of the
@code{golang.org/x/image/tiff} library featuring:

@itemize
@item Read support for CCITT Group3/4 compressed images.
@item Read/write support for LZW compressed images.
@item Read/write support for the CMYK color model.
@end itemize")
    (license license:bsd-3)))

(define-public go-github-com-hinshun-vt10x
  (package
    (name "go-github-com-hinshun-vt10x")
    (version "0.0.0-20220301184237-5011da428d02")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hinshun/vt10x")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pzdwwbzxrsqjb8xfzmfpkyb1gbcszrrimr70cz75jjk2535r26b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hinshun/vt10x"))
    (home-page "https://github.com/hinshun/vt10x")
    (synopsis "vt10x terminal emulation backend")
    (description
     "Package terminal is a vt10x terminal emulation backend, influenced
largely by st, rxvt, xterm, and iTerm as reference.  Use it for terminal
muxing, a terminal emulation frontend, or wherever else you need terminal
emulation.")
    (license license:expat)))

(define-public go-github-com-hodgesds-perf-utils
  (package
    (name "go-github-com-hodgesds-perf-utils")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hodgesds/perf-utils")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wrma5rn0pybav8lwmv5xjwjl6slgnr8ya64f1npijam59r7afzj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hodgesds/perf-utils"
      #:test-flags
      ;; XXX: Tests requiring root access to mount file system or shaky, check
      ;; if they may be fixed.
      #~(list "-skip"
              (string-join
               (list "TestAvailableEvents"
                     "TestAvailablePMUs"
                     "TestBusCycles"
                     "TestCPURefCycles"
                     "TestDebugFSMount"
                     "TestGroupProfiler"
                     "TestLLCache"
                     "TestMSR"
                     "TestMSRPaths"
                     "TestNodeCache"
                     "TestProfiler"
                     "TestSoftwareProfiler"
                     "TestTraceFSMount"
                     #$@(if (target-arm?)
                            '("TestDataTLB"
                              "TestInstructionTLB")
                            '()))
               "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-go-uber-org-multierr))
    (home-page "https://github.com/hodgesds/perf-utils")
    (synopsis "Perf Utilities for Golang")
    (description
     "This package is a Go library for interacting with the @code{perf}
subsystem in Linux.")
    (license license:expat)))

(define-public go-github-com-huandu-xstrings
  (package
    (name "go-github-com-huandu-xstrings")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huandu/xstrings")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04z4xb22mi03a772f1si2fqwi5vn5584afzxas73jc6n6sppz0db"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/huandu/xstrings"))
    (home-page "https://github.com/huandu/xstrings/")
    (synopsis "Collection of string functions")
    (description
     "Go package xstrings is a collection of string functions,which are widely
used in other languages but absent in Go package strings.")
    (license license:expat)))

(define-public go-github-com-iancoleman-orderedmap
  (package
    (name "go-github-com-iancoleman-orderedmap")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iancoleman/orderedmap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rkahhb86ngvzjmdlrpw9rx24a0b1yshq2add1ry2ii6nkx0xbfs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/iancoleman/orderedmap"))
    (home-page "https://github.com/iancoleman/orderedmap")
    (synopsis "Ordered map in Golang")
    (description
     "This package provides a Golang data type for ordered maps where the keys
keep the order that they're added.  It can be de/serialized from/to JSON.")
    (license license:expat)))

(define-public go-github-com-iancoleman-strcase
  (package
    (name "go-github-com-iancoleman-strcase")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iancoleman/strcase")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "169fb56kiif2gq92b7hvh9xgl2n8kjmdg4gqaa1492kb97ia8lwm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/iancoleman/strcase"))
    (home-page "https://github.com/iancoleman/strcase")
    (synopsis "Converting to snake_case or CamelCase")
    (description
     "Package strcase converts strings to various cases.")
    (license license:expat)))

(define-public go-github-com-ianlancetaylor-demangle
  ;; No release, see <https://github.com/ianlancetaylor/demangle/issues/21>.
  (package
    (name "go-github-com-ianlancetaylor-demangle")
    (version "0.0.0-20230524184225-eabc099b10ab")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ianlancetaylor/demangle")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pvlg1adp50hnw8dz7il473xb197ixirg26cy5hj3ngb4qlajwvc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ianlancetaylor/demangle"))
    (home-page "https://github.com/ianlancetaylor/demangle")
    (synopsis "Symbol name demangler written in Go")
    (description
     "This package defines functions that demangle GCC/LLVM C++ and Rust
symbol names.  This package recognizes names that were mangled according to
the C++ ABI defined at https://codesourcery.com/cxx-abi/ and the
@url{https://rust-lang.github.io/rfcs/2603-rust-symbol-name-mangling-v0.html,Rust
ABI}.")
    (license license:bsd-3)))

(define-public go-github-com-imdario-mergo
  (hidden-package
   (package/inherit go-dario-cat-mergo
     (name "go-github-com-imdario-mergo")
     (arguments
      (list
       #:go go-1.23
       #:import-path "github.com/imdario/mergo"
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'fix-import-path
             (lambda* (#:key tests? import-path #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 (substitute* (find-files "." "\\.go$")
                   (("dario.cat/mergo") import-path)))))))))))

(define-public go-github-com-invopop-yaml
  (package
    (name "go-github-com-invopop-yaml")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/invopop/yaml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0isvalfivw39hjj20y28g4yws0plzqaym8asz3sr5lfwv5gnq7zg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/invopop/yaml"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v3))
    (home-page "https://github.com/invopop/yaml")
    (synopsis "YAML marshaling and unmarshaling support for Golang")
    (description
     "This package provides a wrapper around go-yaml designed to enable a
better way of handling YAML when marshaling to and from structs.")
    (license license:expat)))

(define-public go-github-com-itchyny-astgen-go
  (package
    (name "go-github-com-itchyny-astgen-go")
    (version "0.0.0-20250520171007-4331c963041e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/itchyny/astgen-go")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07hzzkczpwsnznwl46jdfqq77b4hjbcxsj1xa2qh8733yym10ap1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/itchyny/astgen-go"))
    (home-page "https://github.com/itchyny/astgen-go")
    (synopsis "AST build for Golang @code{interface{}} => @code{ast.Node}")
    (description "Build Go code from arbitrary value in Go.")
    (license license:expat)))

(define-public go-github-com-itchyny-go-flags
  (package
    (name "go-github-com-itchyny-go-flags")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/itchyny/go-flags")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qfh7gn95aldlsigk72jl87npmwvx15kb7df1100d6j0nbakd8b5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/itchyny/go-flags"
      ;; Test is time dependent and not reproducible.
      ;; -.TH TestMan 1 "1 January 1970"
      ;;  +.TH TestMan 1 "26 June 2025"
      #:test-flags #~(list "-skip" "TestMan")))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/itchyny/go-flags")
    (synopsis "Command line option parser for Golang")
    (description
     "Package flags provides an extensive command line option parser.  The
flags package is similar in functionality to the go built-in flag package but
provides more options and uses reflection to provide a convenient and succinct
way of specifying command line options.  It's an alternative fork of
https://github.com/jessevdk/go-flags.")
    (license license:bsd-3)))

(define-public go-github-com-itchyny-timefmt-go
  (package
    (name "go-github-com-itchyny-timefmt-go")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/itchyny/timefmt-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ldagirn1wh3klkk1rr96d5b5jbn24aib14x3j73x47cjfqi92wf"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/itchyny/timefmt-go"))
    (home-page "https://github.com/itchyny/timefmt-go")
    (synopsis "Efficient time formatting library (strftime, strptime) for Golang")
    (description
     "@code{timefmt-go} is a Go language package for formatting and parsing date
time strings.")
    (license license:expat)))

(define-public go-github-com-jackc-chunkreader
  (package
    (name "go-github-com-jackc-chunkreader")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/chunkreader")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zsxsd6alk51lssq1xq194sf88awj083fjiy7pk3098v2nj9m65l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/chunkreader"))
    (home-page "https://github.com/jackc/chunkreader")
    (synopsis "Wrapper for @code{io.Reader}")
    (description
     "ChunkReader is a @code{io.Reader} wrapper that minimizes IO reads and
memory allocations.  It allocates memory in chunks and will read as much as
will fit in the current buffer in a single call regardless of how large a read
is actually requested.  The memory returned via Next is owned by the caller.
This avoids the need for an additional copy.  It extracted from original
implementation in https://github.com/jackc/pgx.")
    (license license:expat)))

(define-public go-github-com-jackc-chunkreader-v2
  (package
    (inherit go-github-com-jackc-chunkreader)
    (name "go-github-com-jackc-chunkreader-v2")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/chunkreader")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fj585hp3s4cjfzncr5gmim96p0b956pqdf4nm7yan1ipfch9l1c"))))
    (arguments
     (list
      #:import-path "github.com/jackc/chunkreader/v2"))))

(define-public go-github-com-jackc-pgconn
  (package
    (name "go-github-com-jackc-pgconn")
    (version "1.14.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgconn")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rqx0y9k6g8ydcpciv3k246hfd5am4yw4jg3cbq4wlfny01ksh3y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/pgconn"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list "TestConfigCopyCanBeUsedToConnect"
                     "TestConnStress"
                     "TestFrontendFatalErrExec"
                     "TestLRUModePrepare"
                     "TestLRUContext"
                     "TestLRUStmtInvalidationIntegration"
                     "TestLRUModeDescribe"
                     "TestLRUModePrepareStress"
                     "TestLRUStmtInvalidation")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          ;; All tests in this file require PostgreSQL service running.
          (add-before 'check 'remove-failing-test-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file "pgconn_test.go")))))))
    (native-inputs
     (list go-github-com-jackc-pgmock-bootstrap
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-jackc-chunkreader-v2
           go-github-com-jackc-pgio
           go-github-com-jackc-pgpassfile
           go-github-com-jackc-pgproto3-v2
           go-github-com-jackc-pgservicefile
           go-golang-org-x-crypto
           go-golang-org-x-text))
    (home-page "https://github.com/jackc/pgconn")
    (synopsis "Low-level PostgreSQL database driver")
    (description
     "Package pgconn is a low-level PostgreSQL database driver.  It operates
at nearly the same level as the C library libpq.

It is primarily intended to serve as the foundation for higher level libraries
such as @url{https://github.com/jackc/pgx}.  Applications should handle normal
queries with a higher level library and only use pgconn directly when required
for low-level access to PostgreSQL functionality.")
    (license license:expat)))

(define-public go-github-com-jackc-pgconn-bootstrap
  (hidden-package
   (package
     (inherit go-github-com-jackc-pgconn)
     (arguments
      (list #:tests? #f
            #:import-path "github.com/jackc/pgconn"
            #:phases
            #~(modify-phases %standard-phases
                (delete 'build))))
      (native-inputs '()))))

(define-public go-github-com-jackc-pgerrcode
  (package
    (name "go-github-com-jackc-pgerrcode")
    (version "0.0.0-20240316143900-6e2875d9b438")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgerrcode")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00csrjkg8fg97q5wy4r2wqi88w7g1cj0f5xkp5442nz3agmkxgrj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/pgerrcode"))
    (home-page "https://github.com/jackc/pgerrcode")
    (synopsis "Constants for PostgreSQL error codes")
    (description
     "Package pgerrcode contains constants for @code{PostgreSQL} error
codes.")
    (license license:expat)))

(define-public go-github-com-jackc-pgio
  (package
    (name "go-github-com-jackc-pgio")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l17gpn11wf6jm5kbfmxh8j00n5zpmwck3wr91f1cv34k4chyvg1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/pgio"))
    (home-page "https://github.com/jackc/pgio")
    (synopsis "Low-level toolkit building messages in the PostgreSQL wire protocol")
    (description
     "Package pgio is a low-level toolkit building messages in the
@code{PostgreSQL} wire protocol.")
    (license license:expat)))

(define-public go-github-com-jackc-pgpassfile
  (package
    (name "go-github-com-jackc-pgpassfile")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgpassfile")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1crw06lzksgimbmr1a3sr00azg2v7l4qkvjra1cpmzzq5mncaj8z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/pgpassfile"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jackc/pgpassfile")
    (synopsis "PostgreSQL .pgpass files parser")
    (description
     "Package pgpassfile is a parser @code{PostgreSQL} .pgpass files.")
    (license license:expat)))

(define-public go-github-com-jackc-pgproto3
  (package
    (name "go-github-com-jackc-pgproto3")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgproto3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03vpkqa6j4sanmsj7q13fb6yamspszfv38sr28d40g887bcwf0j8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/pgproto3"))
    (propagated-inputs
     (list go-github-com-jackc-chunkreader
           go-github-com-jackc-pgio
           go-github-com-pkg-errors))
    (home-page "https://github.com/jackc/pgproto3")
    (synopsis "Encoder and decoder of the PostgreSQL wire protocol version 3")
    (description
     "This package provides a encoder and decoder of the @code{PostgreSQL}
wire protocol version 3.")
    (license license:expat)))

(define-public go-github-com-jackc-pgproto3-v2
  (package
    (inherit go-github-com-jackc-pgproto3)
    (name "go-github-com-jackc-pgproto3-v2")
    (version "2.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgproto3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bmj1bqnn5863178a0k8m3f9xv48zs10z96dm2rl28ybx33d2l77"))))
    (arguments
     (list
      #:import-path "github.com/jackc/pgproto3/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-jackc-chunkreader-v2
           go-github-com-jackc-pgio))))

(define-public go-github-com-jackc-pgservicefile
  (package
    (name "go-github-com-jackc-pgservicefile")
    (version "0.0.0-20240606120523-5a60cdf6a761")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgservicefile")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z8ndfdxx5r4dpjbjn9caq9w56lrzwm6nh8jwwk0gnq0n2q4cfhi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/pgservicefile"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jackc/pgservicefile")
    (synopsis "Parser PostgreSQL for service files")
    (description
     "Package pgservicefile is a parser for @code{PostgreSQL} service
files (e.g. .pg_service.conf).")
    (license license:expat)))

(define-public go-github-com-jackc-pgtype
  (package
    (name "go-github-com-jackc-pgtype")
    (version "1.14.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgtype")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04yd3b1c1qph6g1giskmm49c9hk0scagfqd08bhj1pprvp9jmn2f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; test require running PostgreSQL
      #:import-path "github.com/jackc/pgtype"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-gofrs-uuid
           go-github-com-jackc-pgconn
           go-github-com-jackc-pgio
           go-github-com-jackc-pgx-v4-bootstrap
           go-github-com-lib-pq
           go-github-com-shopspring-decimal))
    (home-page "https://github.com/jackc/pgtype")
    (synopsis "PostgreSQL types implementations in Golang")
    (description
     "This package implements Go types for over 70 PostgreSQL types.  It is
the type system underlying the https://github.com/jackc/pgx PostgreSQL driver.
These types support the binary format for enhanced performance with pgx.  They
also support the database/sql @code{Scan} and @code{Value} interfaces and can
be used with https://github.com/lib/pq.")
    (license license:expat)))

(define-public go-github-com-jackc-pgtype-bootstrap
  (hidden-package
   (package
     (inherit go-github-com-jackc-pgtype)
     (arguments
      (list #:tests? #f
            #:import-path "github.com/jackc/pgtype"
            #:phases
            #~(modify-phases %standard-phases
                (delete 'build))))
      (native-inputs '())
      (propagated-inputs '()))))

(define-public go-github-com-jackc-pgx
  (package
    (name "go-github-com-jackc-pgx")
    (version "3.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hbnh69ss0pq83n18b62znj3qi54y9kr31a3xi9h35p27nsk3izf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/pgx"
      #:test-subdirs
      #~(list ;; "pgtype/..." ; most tests require networking setup
              ;; "stdlib"
              ;; "."          ; github.com/jackc/pgx [build failed]
              "chunkreader"
              "internal/sanitize"
              "log/..."
              "pgio"
              "pgproto3")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples-and-benchmarks
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (native-inputs
     (list go-github-com-cockroachdb-apd
           go-github-com-jackc-fake))
    (propagated-inputs
     (list go-github-com-gofrs-uuid
           go-github-com-lib-pq
           go-github-com-rs-zerolog
           go-github-com-satori-go-uuid
           go-github-com-shopspring-decimal
           go-github-com-sirupsen-logrus
           go-go-uber-org-zap))
    (home-page "https://github.com/jackc/pgx")
    (synopsis "PostgreSQL driver and toolkit for Golang")
    (description
     "This package implements a pure Go driver and toolkit for PostgreSQL.  It
is different from other drivers such as
@url{http://godoc.org/github.com/lib/pq,pq} because, while it can operate as a
database/sql compatible driver, pgx is also usable directly.  It offers a
native interface similar to database/sql that offers better performance and
more features.")
    (license license:expat)))

(define-public go-github-com-jackc-pgx-v4
  (package
    (inherit go-github-com-jackc-pgx)
    (name "go-github-com-jackc-pgx-v4")
    (version "4.18.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xxvkgngl8c73zg06xdm5sqvck6yvrvjpzidpmsaah1az0lh1lay"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-jackc-pgx)
       ((#:import-path _) "github.com/jackc/pgx/v4")
       ((#:test-subdirs _) #~(list "internal/sanitize" "log/..."))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-cockroachdb-apd
           go-github-com-go-kit-log
           go-github-com-gofrs-uuid
           go-github-com-jackc-pgconn
           go-github-com-jackc-pgio
           go-github-com-jackc-pgproto3-v2
           go-github-com-jackc-pgtype-bootstrap
           go-github-com-jackc-puddle
           go-github-com-masterminds-semver-v3
           go-github-com-rs-zerolog
           go-github-com-shopspring-decimal
           go-github-com-sirupsen-logrus
           go-go-uber-org-zap
           go-gopkg-in-inconshreveable-log15-v2))))

(define-public go-github-com-jackc-pgx-v4-bootstrap
  (hidden-package
   (package
     (inherit go-github-com-jackc-pgx-v4)
     (arguments
      (list #:tests? #f
            #:import-path "github.com/jackc/pgx/v4"
            #:phases
            #~(modify-phases %standard-phases
                (delete 'build))))
      (native-inputs '())
      (propagated-inputs '()))))

(define-public go-github-com-jackc-pgx-v5
  (package
    (inherit go-github-com-jackc-pgx-v4)
    (name "go-github-com-jackc-pgx-v5")
    (version "5.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b8wgqax34q77m4pmaaqlp9dr1x9mam76jx7ah1sxdldl737rv27"))))
    (build-system go-build-system)
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-jackc-pgx-v4)
       ((#:import-path _) "github.com/jackc/pgx/v5")
       ((#:test-subdirs _)
        #~(list "internal/..."
                "log/..."
                "multitracer"))))
    (propagated-inputs
     (list go-github-com-jackc-pgpassfile
           go-github-com-jackc-pgservicefile
           go-github-com-jackc-puddle-v2
           go-golang-org-x-crypto
           go-golang-org-x-sync
           go-golang-org-x-text))))

(define-public go-github-com-jackc-puddle
  (package
    (name "go-github-com-jackc-puddle")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/puddle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0382q7xjdw5wx6174i2sf4gnc5ppgj9snvrvh3rcnwg02yd0np38"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/puddle"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jackc/puddle")
    (synopsis "Generic resource pool for Golang")
    (description
     "Puddle is a tiny generic resource pool library hat uses the standard
context library to signal cancellation of acquires.  It is designed to contain
the minimum functionality required for a resource pool.  It can be used
directly or it can be used as the base for a domain specific resource pool.
For example, a database connection pool may use puddle internally and
implement health checks and keep-alive behavior without needing to implement
any concurrent code of its own.")
    (license license:expat)))

(define-public go-github-com-jackc-puddle-v2
  (package
    (inherit go-github-com-jackc-puddle)
    (name "go-github-com-jackc-puddle-v2")
    (version "2.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/puddle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0agbk4nnja0fahi8mjp1y5ac9vjsjhldjwx9zshw0zjqhaxmsk11"))))
    (arguments
     (list
      #:import-path "github.com/jackc/puddle/v2"))
    (propagated-inputs
     (list go-golang-org-x-sync))))

(define-public go-github-com-jacobsa-fuse
  (package
    (name "go-github-com-jacobsa-fuse")
    (version "0.0.0-20241025064006-8ccd61173b05")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jacobsa/fuse")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zf9fhv5j0pffmp4c0zba58627lb6cwr2qxjsa3dwba9b8fiz7sf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; requires fusermount
      #:import-path "github.com/jacobsa/fuse"))
    (native-inputs
     (list go-github-com-detailyang-go-fallocate
           go-github-com-jacobsa-ogletest
           go-github-com-kylelemons-godebug))
    (propagated-inputs
     (list go-github-com-jacobsa-oglematchers
           go-github-com-jacobsa-syncutil
           go-github-com-jacobsa-timeutil
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/jacobsa/fuse")
    (synopsis "FUSE file system in Golang")
    (description
     "Package fuse enables writing and mounting user-space file systems.
Subpackages:
@itemize
@item @code{fuse} provides support for mounting a new file system and reading
requests from the kernel

@item @code{fuseops} enumerates the supported requests from the kernel, and
provides documentation on their semantics

@item @code{fuseutil}, in particular the @code{FileSystem} interface, provides
a convenient way to create a file system type and export it to the kernel via
@code{fuse.Mount}.
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-jacobsa-syncutil
  (package
    (name "go-github-com-jacobsa-syncutil")
    (version "0.0.0-20180201203307-228ac8e5a6c3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jacobsa/syncutil")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dylp0qaabm8djqmn0qjki3gy9wnq23cbr4k4g63zk4axqgm2ns2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jacobsa/syncutil"))
    (native-inputs
     (list go-github-com-jacobsa-oglematchers
           go-github-com-jacobsa-ogletest))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/jacobsa/syncutil")
    (synopsis "Code supplementing Go's sync package")
    (description
     "This package contains code that supplements the
@url{http://godoc.org/sync,sync} package from the Go standard library.

In particular:
@itemize
@item Bundle, which makes it easy to write code that spawns multiple
cancellation-aware workers that may fail
@item invariantMutex, which makes it possible to automatically check your
invariants at lock and unlock time
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-jacobsa-timeutil
  (package
    (name "go-github-com-jacobsa-timeutil")
    (version "0.0.0-20170205232429-577e5acbbcf6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jacobsa/timeutil")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vp6ngfyw3z3n7fzqpijqh26mnwnhapy5x4kzly1qb8gmv15a8xb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jacobsa/timeutil"))
    (native-inputs
     (list go-github-com-jacobsa-oglematchers
           go-github-com-jacobsa-ogletest))
    (home-page "https://github.com/jacobsa/timeutil")
    (synopsis "Code supplementing Go's time package")
    (description
     "This package contains code that supplements the
@url{http://godoc.org/time, time} package from the Go standard library.

In particular:
@itemize
@item a Clock interface, with a fake implementation that can be used in tests
@item implementations of
@url{https://godoc.org/github.com/jacobsa/oglematchers#Matcher,
oglematchers.Matcher} for time values
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-jaypipes-pcidb
  (package
    (name "go-github-com-jaypipes-pcidb")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jaypipes/pcidb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "092wax6i7bn2lvwbz0rbbj17ly60n4b4n5ymy92w4d0mzxrn1ac4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jaypipes/pcidb"
      ;; Expected no error creating PciInfo, but got No pci-ids DB files found
      ;; (and network fetch disabled).
      #:test-flags #~(list "-skip" "TestPCI")))
    (propagated-inputs
     (list go-github-com-mitchellh-go-homedir))
    (home-page "https://github.com/jaypipes/pcidb")
    (synopsis "Query PCI database information")
    (description
     "@code{pcidb} is a small Golang library for programmatic querying of PCI
vendor,product and class information.")
    (license (list license:asl2.0 license:asl2.0))))

(define-public go-github-com-jba-templatecheck
  (package
    (name "go-github-com-jba-templatecheck")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jba/templatecheck")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03nlpfgjiiqqhf0df6vgvxi7kn58ykh0jhnrffpa32wv2w67ndxz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jba/templatecheck"))
    (propagated-inputs
     (list go-github-com-google-safehtml))
    (home-page "https://github.com/jba/templatecheck")
    (synopsis "Checks Go templates for problems")
    (description
     "Package templatecheck checks Go templates for problems.  It can detect
many errors that are normally caught only during execution.  Use templatecheck
in tests to find template errors early, and along template execution paths
that might only rarely be reached.")
    (license license:expat)))

(define-public go-github-com-jbenet-go-context
  (package
    (name "go-github-com-jbenet-go-context")
    (version "0.0.0-20150711004518-d14ea06fba99")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbenet/go-context")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q91f5549n81w3z5927n4a1mdh220bdmgl42zi3h992dcc4ls0sl"))
       (patches (search-patches
                 "go-github-com-jbenet-go-context-fix-import-error.patch"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/jbenet/go-context"))
    (home-page "https://github.com/jbenet/go-context/")
    (synopsis "@code{jbenet's} context extensions")
    (description
     "This package provides @code{jbenet's} context extensions.")
    (license license:expat)))

(define-public go-github-com-jbenet-go-random
  (package
    (name "go-github-com-jbenet-go-random")
    (version "0.0.0-20190219211222-123a90aedc0c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbenet/go-random")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kgx19m8p76rmin8s8y6j1padciv1dx37qzy7jkh9bw49ai3haw3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jbenet/go-random"))
    (propagated-inputs
     (list go-github-com-dustin-go-humanize))
    (home-page "https://github.com/jbenet/go-random")
    (synopsis "Go library and a program that outputs randomness")
    (description
     "This is a Unix utility that outputs randomness.  It is a thin
wrapper around @code{crypto/rand}.")
    (license license:expat)))

(define-public go-github-com-jbenet-go-temp-err-catcher
  (package
    (name "go-github-com-jbenet-go-temp-err-catcher")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbenet/go-temp-err-catcher")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n482jhh6jwq43jj21xkq8grqzx78hjh7f44p0q3n01zp1dsh97r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jbenet/go-temp-err-catcher"
      #:test-flags
      ;; One test fails on ARM with error: tec_test.go:86: time difference is
      ;; greater than 0s 71.555µs.
      #~(list #$@(if (target-arm?) '("-skip" "TestDoubles") '()))))
    (home-page "https://github.com/jbenet/go-temp-err-catcher")
    (synopsis "Error handling helper library")
    (description "Package @code{temperrcatcher} provides a @code{TempErrCatcher}
object, which implements simple error-retrying functionality.")
    (license license:expat)))

(define-public go-github-com-jbenet-goprocess
  (package
    (name "go-github-com-jbenet-goprocess")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbenet/goprocess")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z4a5skx9kh2c727pc6zz0vhf9v8acd320s7z0f1kwy3y1nbdhjk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/jbenet/goprocess"
      #:test-flags
      #~(list "-short"
              ;; Tests fail on ARM with error: periodic_test.go:31: time diff
              ;; incorrect: 5ms 3.295666ms 15ms.
              #$@(if (target-arm?)
                     '("-skip" (string-join
                                (list "TestEveryGoSeqParallel"
                                      "TestEverySeq"
                                      "TestTickSeq"
                                      "TestTickSeqNoWait"
                                      "TestTickerGoParallel"
                                      "TestTickerGoSeq"
                                      "TestTickerSeq")
                                "|"))
                     '()))))
    (native-inputs
     (list go-github-com-jbenet-go-cienv))
    (home-page "https://github.com/jbenet/goprocess")
    (synopsis "Manage process life cycles in Go")
    (description
     "@code{goprocess} introduces a way to manage process lifecycles in
Go.  It is much like @code{go.net/context} (it actually uses a Context), but it is
more like a Context-WaitGroup hybrid.  @code{goprocess} is about being able to start
and stop units of work, which may receive @code{Close} signals from many clients.")
    (license license:expat)))

(define-public go-github-com-jcmturner-gofork
  (package
    (name "go-github-com-jcmturner-gofork")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jcmturner/gofork")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w1j6b671121r6md5w7hnh2d0sa332pw5q49yihw23wdfinknyin"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jcmturner/gofork"))
    (home-page "https://github.com/jcmturner/gofork")
    (synopsis "Modified Go standard library packages")
    (description
     "This repository contains modified Go standard library packages for use
as work arounds until issues are addressed in the official distribution.")
    (license license:bsd-3)))

(define-public go-github-com-jdkato-go-tree-sitter-julia
  (package
    (name "go-github-com-jdkato-go-tree-sitter-julia")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jdkato/go-tree-sitter-julia")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17xr33vvrqsl0v1xzb3ya99anhg5zf04kp6wfk1m3iqi904ff0am"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/jdkato/go-tree-sitter-julia"))
    (propagated-inputs
     (list go-github-com-smacker-go-tree-sitter))
    (home-page "https://github.com/jdkato/go-tree-sitter-julia")
    (synopsis "Julia grammar for tree sitter")
    (description
     "This package provides a Julia grammar for
@url{https://github.com/tree-sitter/tree-sitter, tree-sitter} bindings in
Golang.")
    ;; It's a component of Vale, written by the same author and does not
    ;; provide license.
    (license license:unlicense)))

(define-public go-github-com-jdkato-twine
  (package
    (name "go-github-com-jdkato-twine")
    (version "0.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jdkato/twine")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hbpxcrcsbi975lklrhzyzk0fzn79pxicvfyf2sckmd2n6jb4ayy"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Module name has been changed upstream.
            (substitute* (find-files "." "\\.go$")
              (("gopkg.in/neurosnap/sentences.v1")
               "github.com/neurosnap/sentences"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jdkato/twine"
      #:skip-build? #t
      #:test-flags #~(list "-skip" "TestGoldenRules")))
    (propagated-inputs
     (list go-github-com-montanaflynn-stats
           go-github-com-neurosnap-sentences
           go-github-com-errata-ai-regexp2))
    (home-page "https://github.com/jdkato/twine")
    (synopsis "NLP-related string utilities")
    (description
     "NLP-related string utility functions for Golang.")
    (license license:expat)))

(define-public go-github-com-jedib0t-go-pretty-v6
  (package
    (name "go-github-com-jedib0t-go-pretty-v6")
    (version "6.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jedib0t/go-pretty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "122zmbcrxvl0yvpcq56p4hhcasf7lmprmka4fa00hkpr0m0rrbxq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:skip-build? #t
      #:import-path "github.com/jedib0t/go-pretty/v6"))
    (native-inputs
     (list go-github-com-pkg-profile ; for the CLI
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-text))
    (home-page "https://github.com/jedib0t/go-pretty")
    (synopsis "Table-writer and more in Golang")
    (description
     "Utilities to prettify console output of tables, lists, progress-bars,
text, etc. with a heavy emphasis on customization.")
    (license license:expat)))

(define-public go-github-com-jessevdk-go-flags
  (package
    (name "go-github-com-jessevdk-go-flags")
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jessevdk/go-flags")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dxk8n06sh15rm7777v5jgwxz9ca1c090ni6lyjhj1d2lxfysb45"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/jessevdk/go-flags"
      #:test-flags #~(list "-skip" "TestCompletion|TestParserCompletion")))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/jessevdk/go-flags")
    (synopsis "Go library for parsing command line arguments")
    (description
     "The @code{flags} package provides a command line option parser.  The
functionality is similar to the go builtin @code{flag} package, but
@code{flags} provides more options and uses reflection to provide a succinct
way of specifying command line options.")
    (license license:bsd-3)))

(define-public go-github-com-jiangxin-multi-log
  (package
    (name "go-github-com-jiangxin-multi-log")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jiangxin/multi-log")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0im1i0dz5rcczfzxyvwqwvslv1mq6gbhlr5aw9s2fg7s29lcy179"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jiangxin/multi-log"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-sirupsen-logrus
           go-golang-org-x-crypto))
    (home-page "https://github.com/jiangxin/multi-log")
    (synopsis "Simple logging library for Go")
    (description
     "Multi-log is based on logrus, and supports concurrently logging to two
destinations: the console and a log file.")
    (license license:expat)))

(define-public go-github-com-jinzhu-copier
  (package
    (name "go-github-com-jinzhu-copier")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jinzhu/copier")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kf29cmmbic72kfrfd1xnass7l9j85impf8mqn5f3fd3ibi9bs74"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/jinzhu/copier"))
    (home-page "https://github.com/jinzhu/copier")
    (synopsis "Go copier library")
    (description
     "This package provides a library, which supports copying value from one
struct to another.")
    (license license:expat)))

(define-public go-github-com-jkeiser-iter
  (package
    (name "go-github-com-jkeiser-iter")
    (version "0.0.0-20200628201005-c8aa0ae784d1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jkeiser/iter")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vdmxbhvq8s51i93g6nympmwnww7vh0xw2xznazhf69k4ggr9v2r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jkeiser/iter"))
    (home-page "https://github.com/jkeiser/iter")
    (synopsis "Golang library for @code{iteration}")
    (description
     "This package is intended to support forward-only iteration in a variety
of use cases while avoiding the normal errors and leaks that can happen with
iterators in Go.  It provides mechanisms for map/select filtering, background
iteration through a goroutine, and error handling throughout.")
    (license license:expat)))

(define-public go-github-com-jmattheis-goverter
  (package
    (name "go-github-com-jmattheis-goverter")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jmattheis/goverter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ph8470wxpf8p2cdr5w3hkchlgpiyzljlsdna9jvhgw53sf2c32n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jmattheis/goverter"
      ;; Test requiring compplex set-up, fails during build but passed outside
      ;; build-system.
      #:test-flags #~(list "-skip" "TestScenario")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-broken-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "example/enum/transform-custom"
                                "example/wrap-errors-using"
                                "example/protobuf"))))))))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-github-com-goverter-patherr))
    (propagated-inputs
     (list go-github-com-dave-jennifer
           go-golang-org-x-tools
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/jmattheis/goverter")
    (synopsis "Type-safe Go converters by defining function signatures")
    (description
     "This package provides a functionality to generate type-safe converters
for Go.  The project is meant as alternative to
@url{https://github.com/jinzhu/copier, jinzhu/copier} that doesn't use
reflection.")
    (license license:expat)))

(define-public go-github-com-jmoiron-sqlx
  (package
    (name "go-github-com-jmoiron-sqlx")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jmoiron/sqlx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10rg9b6cl1j7jjr6z95xa1k45016mhicii3cmz0pkwrxw3dpfzfh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jmoiron/sqlx"))
    (propagated-inputs
     (list go-github-com-go-sql-driver-mysql
           go-github-com-lib-pq
           go-github-com-mattn-go-sqlite3))
    (home-page "https://github.com/jmoiron/sqlx")
    (synopsis "General purpose extensions to golang's @code{database/sql}")
    (description
     "sqlx is a library which provides a set of extensions on go's standard
@code{database/sql} library.  The sqlx versions of @code{sql.DB},
@code{sql.TX}, @code{sql.Stmt}, et al. all leave the underlying interfaces
untouched, so that their interfaces are a superset on the standard ones.  This
makes it relatively painless to integrate existing codebases using
database/sql with sqlx.")
    (license license:expat)))

(define-public go-github-com-jochenvg-go-udev
  (package
    (name "go-github-com-jochenvg-go-udev")
    (version "0.0.0-20240801134859-b65ed646224b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jochenvg/go-udev")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fx6pqhcfp1999yk31vajihi8djsz706f3ylivhgg1k2gzcbwfbq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f  ;require root access
      #:import-path "github.com/jochenvg/go-udev"))
    (inputs
     (list eudev))
    (propagated-inputs
     (list go-github-com-jkeiser-iter
           go-golang-org-x-sys))
    (home-page "https://github.com/jochenvg/go-udev")
    (synopsis "Golang bindings for @code{libudev}")
    (description
     "Package udev provides a cgo wrapper around the libudev C library.")
    (license license:asl2.0)))

(define-public go-github-com-johnkerl-lumin
  (package
    (name "go-github-com-johnkerl-lumin")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/johnkerl/lumin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1liv27pxi79q4yr1bd0wgsx31ixw53ipsgs2kp0asxj2d6z4hpiz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/johnkerl/lumin"))
    (home-page "https://github.com/johnkerl/lumin")
    (synopsis "Command-line tool to highlight matches in files")
    (description
     "@command{lumin} is a simple command-line program which highlights matches
to a specified pattern (string or regex) in the specified files.  This is like
@code{grep} with @code{--color}, except that @code{lumin} shows all lines, not
just matching lines.  This package provides a CLI tool and @code{colors}
library.")
    (license license:bsd-2)))

(define-public go-github-com-joho-godotenv
  (package
    (name "go-github-com-joho-godotenv")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joho/godotenv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03vijs05k31jdf24pzj3vlk6b5jxf894v1kvzals4wzclyq2h3ch"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/joho/godotenv"))
    (home-page "https://github.com/joho/godotenv")
    (synopsis "Golang library to load environment variables from @code{.env}")
    (description
     "This package provides a Go port of the Ruby's dotenv library
https://github.com/bkeepers/dotenv.")
    (license license:expat)))

(define-public go-github-com-jonboulle-clockwork
  (package
    (name "go-github-com-jonboulle-clockwork")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonboulle/clockwork")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j1k9chzy5pb76r4r1k0vr5fbkvna564v6dzjflhapwsr7jpjgwf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jonboulle/clockwork"))
    (home-page "https://github.com/jonboulle/clockwork")
    (synopsis "Fake clock library for Go")
    (description
     "Replace uses of the @code{time} package with the @code{clockwork.Clock}
interface instead.")
    (license license:asl2.0)))

(define-public go-github-com-jorropo-jsync
  (package
    (name "go-github-com-jorropo-jsync")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Jorropo/jsync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s1nn6wy6ymvvawyly84y8bx1vl9k46awnvd000q6ndrvgs1clxg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Jorropo/jsync"))
    (home-page "https://github.com/Jorropo/jsync")
    (synopsis "Synchronisation implementation wrapper for Go std @code{sync}")
    (description
     "jsync is a package that implements various synchronisation helpers that
are missing from @code{sync}.  It does not and will not rely on golinkname to
be portable.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-josharian-intern
  (package
    (name "go-github-com-josharian-intern")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/josharian/intern")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1za48ppvwd5vg8vv25ldmwz1biwpb3p6qhf8vazhsfdg9m07951c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/josharian/intern"))
    (home-page "https://github.com/josharian/intern")
    (synopsis "String interning for Go")
    (description
     "This library defines functions to perform string interning in Go,
storing only one copy of each unique string in memory.  All functions may be
called concurrently with themselves and each other.")
    (license license:expat)))

(define-public go-github-com-josharian-native
  (package
    (name "go-github-com-josharian-native")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/josharian/native")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wa4yzc3r06qjklqjf4n30zx9v660w8hmxkmybzwk03fmlv2rcyj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/josharian/native"))
    (home-page "https://github.com/josharian/native")
    (synopsis "Native Golang @code{encoding/binary.ByteOrder} enchantment")
    (description
     "This package provides an easy access to native byte order.")
    (license license:expat)))

(define-public go-github-com-jpillora-backoff
  (let ((commit "fab01a9d9810a410d2d95a0a697f0afb604658f9")
        (revision "1"))
    (package
      (name "go-github-com-jpillora-backoff")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jpillora/backoff")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0irpxdjvwmfd1njvws5x466ar8faiwjnnna26jnly9sw1b0h1b89"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/jpillora/backoff"))
      (home-page "https://github.com/jpillora/backoff")
      (synopsis "Simple exponential backoff counter in Go")
      (description
       "This package is a simple exponential backoff counter in Go.")
      (license license:expat))))

(define-public go-github-com-juju-ansiterm
  (package
    (name "go-github-com-juju-ansiterm")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/juju/ansiterm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05mk7mlvg11dd6b0j0wlq547ghbmx2ywwrlbcb4kddpg7qaqp1va"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/juju/ansiterm"))
    (propagated-inputs
     (list go-gopkg-in-check-v1
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-colorable
           go-github-com-lunixbochs-vtclean))
    (home-page "https://github.com/juju/ansiterm")
    (synopsis "Writer to output ANSI escape codes for color and styles")
    (description
     "The ansiterm package provides a writer to output the ANSI escape codes
for color and styles.")
    (license license:lgpl3)))

(define-public go-github-com-k-sone-critbitgo
  (package
    (name "go-github-com-k-sone-critbitgo")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/k-sone/critbitgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wl0jc42msph8i8fhdz9p9i9j9svn641nf178k2jr1fjqzqc1xrd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/k-sone/critbitgo"))
    (home-page "https://github.com/k-sone/critbitgo")
    (synopsis "Crit-bit for Golang")
    (description
     "This package provides an implementatioin of
@url{http://cr.yp.to/critbit.html, Crit-bit trees} in Golang.")
    (license license:expat)))

(define-public go-github-com-k0kubun-go-ansi
  (package
    (name "go-github-com-k0kubun-go-ansi")
    (version "0.0.0-20180517002512-3bf9e2903213")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/k0kubun/go-ansi")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "117afax4l268rbswf02icbgxncmd1pk2abkz7cv26iyszi8l26dq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/k0kubun/go-ansi"))
    (home-page "https://github.com/k0kubun/go-ansi")
    (synopsis "Windows-portable ANSI escape sequence utility for Golang")
    (description
     "This library converts ANSI escape sequences to Windows API calls on
Windows environment.  You can easily use this feature by replacing fmt with
ansi.")
    (license license:expat)))

(define-public go-github-com-k0kubun-pp
  (package
    (name "go-github-com-k0kubun-pp")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/k0kubun/pp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vpp5n3kdazk4s1ljhwbrhz3kilzvdvx5hya922bg0q9vnjqqvvc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/k0kubun/pp"))
    (propagated-inputs (list go-github-com-mattn-go-colorable
                             go-golang-org-x-text))
    (home-page "https://github.com/k0kubun/pp")
    (synopsis "Colored pretty-printer for Go")
    (description
     "This package provides a pretty-printer for Go.  The functions defined by
@code{pp} follow an API similar to @code{fmt} and its configuration can be
customized globally.")
    (license license:expat)))

(define-public go-github-com-kamstrup-intmap
  (package
    (name "go-github-com-kamstrup-intmap")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kamstrup/intmap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w9masbm9x2p4njxngr5sgqbjnwdwxv3f68yd3wgrrrjbfw5lh44"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kamstrup/intmap"))
    (home-page "https://github.com/kamstrup/intmap")
    (synopsis "Fast hashmap with integer keys for Golang")
    (description
     "@code{intmap} is a fast hashmap implementation for Golang, specialized for maps with
integer type keys.")
    (license license:bsd-2)))

;; Some packages (Yggdrasil) need it to compile it's a tiny package and it's
;; easier to bundle it than to patch it out.
(define-public go-github-com-kardianos-minwinsvc
  (package
    (name "go-github-com-kardianos-minwinsvc")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kardianos/minwinsvc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02k2vibmm65bzkdjpmllphvq88wwyz3m02lbz8bffcpxjad2453v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kardianos/minwinsvc"))
    (home-page "https://github.com/kardianos/minwinsvc/")
    (synopsis "Minimal windows only service stub for Go")
    (description
     "Go programs designed to run from most *nix style operating systems can
import this package to enable running programs as services without modifying
them.")
    (license license:zlib)))

(define-public go-github-com-kardianos-osext
  (package
    (name "go-github-com-kardianos-osext")
    (version "0.0.0-20190222173326-2bc1f35cddc0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kardianos/osext")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pvrbrvmrf4mx0fxbfaphbzgqgwn8v6lkfk2vyrs0znxrs1xyc5r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kardianos/osext"))
    (home-page "https://github.com/kardianos/osext")
    (synopsis "Find the running executable")
    (description
     "Osext provides a method for finding the current executable file that is
running.  This can be used for upgrading the current executable or finding
resources located relative to the executable file.")
    (license license:bsd-3)))

(define-public go-github-com-kardianos-service
  (package
    (name "go-github-com-kardianos-service")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kardianos/service")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wcrc632nf2l5gzwgjfpx7bh6v4la0qjmaykj58fysabb9fkk9dy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kardianos/service"
      #:test-flags #~(list "-skip" "TestPlatformName")))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/kardianos/service")
    (synopsis "Run go programs as a service")
    (description
     "This package provides a simple way to create a system service.")
    (license license:zlib)))

(define-public go-github-com-karpeleslab-reflink
  (package
    (name "go-github-com-karpeleslab-reflink")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/KarpelesLab/reflink")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16x01ff1w4xcdl41iicsrsxpk4ba6xf1g1hlq3qx1f098k4s3nci"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/KarpelesLab/reflink"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/KarpelesLab/reflink")
    (synopsis "Reflink file copy in Golang")
    (description
     "This package implements a functionality to perform efficient file copies
using reflink operations on compatible filesystems such as btrfs and xfs.")
    (license license:expat)))

(define-public go-github-com-karrick-godirwalk
  (package
    (name "go-github-com-karrick-godirwalk")
    (version "1.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/karrick/godirwalk")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jyvai5vpmx86l71hg9j6lxc2b4v32ajvcmjlz40zimfb9ip11q9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/karrick/godirwalk"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    ;; To build all examples as a test scenario.
    (native-inputs
     (list go-github-com-karrick-golf
           go-github-com-mattn-go-isatty))
    (home-page "https://github.com/karrick/godirwalk")
    (synopsis "Fast directory traversal library for Go")
    (description
     "This package provides functions to read and traverse directory trees.")
    (license license:bsd-2)))

(define-public go-github-com-karrick-golf
  (package
    (name "go-github-com-karrick-golf")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/karrick/golf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iskalk32zqiwwrfmi9lcd1s69xgn3yl056xcm5q4y5znbs96fac"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/karrick/golf"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "example")))))))
    (home-page "https://github.com/karrick/golf")
    (synopsis "Light-weight long and short command line option parser")
    (description
     "Go long flag: a light-weight long and short command line option parser.")
    (license license:bsd-3)))

(define-public go-github-com-kataras-golog
  (package
    (name "go-github-com-kataras-golog")
    (version "0.1.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kataras/golog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1szibhgxmkd52gz6i77p3fwlxqyidj7l5vf59pjl5bjy8ggv0kpv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kataras/golog"))
    (propagated-inputs
     (list go-github-com-kataras-pio))
    (home-page "https://github.com/kataras/golog")
    (synopsis "Logging foundation for Go applications")
    (description
     "GoLog is a level-based logger written in Go.")
    (license license:bsd-3)))

(define-public go-github-com-kataras-pio
  (package
    (name "go-github-com-kataras-pio")
    (version "0.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kataras/pio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z0fkhs6amnkvb02519pfxy2x2r5sfmvdzxfi31ipa17m98pk3bq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kataras/pio"))
    (home-page "https://github.com/kataras/pio")
    (synopsis "Pill for Input/Output")
    (description
     "PIO is a low-level package that provides a way to centralize different
output targets.  Supports colors and text decoration to all popular
terminals.")
    (license license:bsd-3)))

(define-public go-github-com-kballard-go-shellquote
  ;; No release, see <https://github.com/kballard/go-shellquote/issues/13>.
  (let ((commit "95032a82bc518f77982ea72343cc1ade730072f0")
        (revision "1"))
    (package
      (name "go-github-com-kballard-go-shellquote")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kballard/go-shellquote")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1rspvmnsikdq95jmx3dykxd4k1rmgl98ryjrysvl0cf18hl1vq80"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/kballard/go-shellquote"))
      (synopsis "Shell-style string joins and splits")
      (description
       "Shellquote provides utilities for joining/splitting strings using sh's
word-splitting rules.")
      (home-page "https://github.com/kballard/go-shellquote")
      (license license:expat))))

(define-public go-github-com-kevinburke-ssh-config
  (package
    (name "go-github-com-kevinburke-ssh-config")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kevinburke/ssh_config")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m8nagaylwrgy9yfqr5x6p1zdsnrfy4km7pgv58iqdmmd5l6v73a"))))
    (arguments
     (list
      #:import-path "github.com/kevinburke/ssh_config"))
    (build-system go-build-system)
    (home-page "https://github.com/kevinburke/ssh_config/")
    (synopsis "Parser for @file{ssh_config} files")
    (description
     "This is a Go parser for @file{ssh_config} files.  Importantly, this
parser attempts to preserve comments in a given file, so you can manipulate a
@file{ssh_config} file from a program.")
    (license license:expat)))

(define-public go-github-com-kevinmbeaulieu-eq-go
  (package
    (name "go-github-com-kevinmbeaulieu-eq-go")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kevinmbeaulieu/eq-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gc2r1yrjg206swlx1vrvb92s2m1y6752yhbh869h2m47q2p4y38"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/kevinmbeaulieu/eq-go"))
    (home-page "https://github.com/kevinmbeaulieu/eq-go")
    (synopsis "Compare Golang source directories")
    (description
     "This package provides a way to check whether two Go source directories
contain equivalent code.")
    (license license:asl2.0)))

(define-public go-github-com-keybase-dbus
  (package
    (name "go-github-com-keybase-dbus")
    (version "0.0.0-20220506165403-5aa21ea2c23a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/keybase/dbus")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ihicxqq685jy47dw522b5c7b8vcikay0xph6y55jcas3m3zj1lj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f; tests require running D-bus
      #:import-path "github.com/keybase/dbus"))
    (home-page "https://github.com/keybase/dbus")
    (synopsis "Native Go bindings for D-Bus")
    (description
     "Package dbus implements bindings to the D-Bus message bus system.
Features:
@itemize
@item complete native implementation of the D-Bus message protocol
@item go-like API (channels for signals / asynchronous method calls,
Goroutine-safe connections)
@item subpackages that help with the introspection / property interfaces
@end itemize")
    (license license:bsd-2)))

(define-public go-github-com-keybase-go-keychain
  (package
    (name "go-github-com-keybase-go-keychain")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/keybase/go-keychain")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gkd839h8xnfiv0g52hm4p9snrcfgrnczrqf5wxr61sgg2w8h3y1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/keybase/go-keychain"
      ;; Test suite tries to talk to dbus.
      #:tests? #f))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-keybase-dbus))
    (home-page "https://github.com/keybase/go-keychain")
    (synopsis "Go library to access the keychain")
    (description
     "This package provides a library for accessing the keychain, typically
the @code{SecretService} D-Bus interface on GNU/Linux.")
    (license license:expat)))

(define-public go-github-com-keybase-go-ps
  (package
    (name "go-github-com-keybase-go-ps")
    (version "0.0.0-20190827175125-91aafc93ba19")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/keybase/go-ps")
         (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1la7m9pd1rrij727g34k9d2iapqwrkwdkqwpkbsbcq8ig0fg634h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/keybase/go-ps"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "test\\.go")
                  (("/bin/sleep") (which "sleep")))
                (substitute* "process_openbsd.go"
                  (("^// \\+build ignore") ""))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/keybase/go-ps")
    (synopsis "Process list library for Go")
    (description
     "Go-Ps is a library for Go that implements OS-specific APIs to list and
manipulate processes in a safe way.")
    (license license:expat)))

(define-public go-github-com-kimmachinegun-automemlimit
  (package
    (name "go-github-com-kimmachinegun-automemlimit")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KimMachineGun/automemlimit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sgm46dhd5yp2fjsmjszbv8fhvw1kip2c6p4qf4856ma0znr49p6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/KimMachineGun/automemlimit"
      #:test-flags
      ;; memlimit_linux_test.go:82: SetGoMemLimit() error = failed to set
      ;; GOMEMLIMIT: process is not in cgroup, wantErr cgroups is not
      ;; supported on this system
      #~(list "-skip" "TestSetGoMemLimit/Unavailable")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (propagated-inputs
     (list go-github-com-pbnjay-memory))
    (home-page "https://github.com/KimMachineGun/automemlimit")
    (synopsis "Automatically set GOMEMLIMIT to match cgroups(7) memory limit")
    (description
     "Automatically set
@url{https://tip.golang.org/doc/gc-guide#Memory_limit,GOMEMLIMIT} to match
Linux @url{https://man7.org/linux/man-pages/man7/cgroups.7.html,cgroups(7)}
memory limit.")
    (license license:expat)))

(define-public go-github-com-kisielk-sqlstruct
  (package
    (name "go-github-com-kisielk-sqlstruct")
    (version "0.0.0-20210630145711-dae28ed37023")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kisielk/sqlstruct")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kna8qzpf1n5zsfi624xm5k3sssn5cnsw1b23w4l3qa5djy4wylk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kisielk/sqlstruct"))
    (home-page "https://github.com/kisielk/sqlstruct")
    (synopsis "Golang sturcts with std @code{database/sql}")
    (description
     "Package sqlstruct provides some convenience functions for using structs
with the Go standard library's @code{database/sql} package.")
    (license license:expat)))

(define-public go-github-com-klauspost-asmfmt
  (package
    (name "go-github-com-klauspost-asmfmt")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/klauspost/asmfmt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01qas9x9qb0s1aiq0235p8hvvqqn76ff0cs4cg71paxcy6l1a4k3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/klauspost/asmfmt"))
    (home-page "https://github.com/klauspost/asmfmt")
    (synopsis "Go Assembler Formatter")
    (description
     "This package implements functionality to format Assembler code the same
way that @code{gofmt} formats Go code.")
    (license license:expat)))

(define-public go-github-com-klauspost-cpuid
  (package
    (name "go-github-com-klauspost-cpuid")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/klauspost/cpuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s510210wdj5dkamii1qrk7v87k4qpdcrrjzflp5ha9iscw6b06l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/klauspost/cpuid"))
    (home-page "https://github.com/klauspost/cpuid")
    (synopsis "CPU feature identification for Go")
    (description
     "@code{cpuid} provides information about the CPU running the current
program.  CPU features are detected on startup, and kept for fast access
through the life of the application.  Currently x86 / x64 (AMD64) is
supported, and no external C (cgo) code is used, which should make the library
very eas to use.")
    (license license:expat)))

(define-public go-github-com-klauspost-cpuid-v2
  (package
    (inherit go-github-com-klauspost-cpuid )
    (name "go-github-com-klauspost-cpuid-v2")
    (version "2.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/klauspost/cpuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fys5v9vslar483arj7wy4id5kg1c7vqv4437kgjnwvki69j9mxf"))))
    (arguments
     (list
      #:import-path "github.com/klauspost/cpuid/v2"))))

(define-public go-github-com-klauspost-reedsolomon
  (package
    (name "go-github-com-klauspost-reedsolomon")
    (version "1.12.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/klauspost/reedsolomon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04j6arqk9pisn3yjr9k90nmfs51fh5i7firl7lzs3x98d84qq5lv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/klauspost/reedsolomon"
      #:test-flags #~(list "-short")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples-and-benchmarks
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "benchmark")
                (delete-file-recursively "examples"))))
        (add-before 'build 'go-generate
          (lambda* (#:key import-path #:allow-other-keys)
            (with-directory-excursion (string-append "src/" import-path "/_gen")
              (invoke "go" "generate" "-v" "-n")))))))
    (propagated-inputs (list go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/klauspost/reedsolomon")
    (synopsis "Reed-Solomon algorithm implementation in Golang")
    (description
     "Package reedsolomon enables
@url{https://en.wikipedia.org/wiki/Reed%E2%80%93Solomon_error_correction,
Erasure Coding}.  It's a Go port of the
@url{https://github.com/Backblaze/JavaReedSolomon, JavaReedSolomon}.

For encoding high shard counts (>256) a Leopard implementation is used.  For
most platforms this performs close to the original Leopard implementation in
terms of speed.")
    (license license:expat)))

(define-public go-github-com-kljensen-snowball
  (package
    (name "go-github-com-kljensen-snowball")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kljensen/snowball")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ldc8hw13jwffhfac3w19033rxg8042jpwnw16l5mnwc0rg8b23l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kljensen/snowball"))
    (home-page "https://github.com/kljensen/snowball")
    (synopsis "Go implementation of the Snowball stemmers")
    (description
     "This package provides a implementation of the
@url{http://snowball.tartarus.org/, Snowball stemmer} for natural language
processing.")
    (license license:expat)))

(define-public go-github-com-knz-go-libedit
  (package
    (name "go-github-com-knz-go-libedit")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/knz/go-libedit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04a5ryzldsk7agybcz4rpd7g1v5vh7smawlky58bwj0341083p44"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/knz/go-libedit"
      #:phases
      #~(modify-phases %standard-phases
          ;; These steps are taken from the project's README.
          (add-after 'unpack 'use-system-wide-libedit
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "unix/editline_unix.go"
                  ((".*#cgo linux CFLAGS.*") "")
                  (("#cgo linux CPPFLAGS.*")
                   (string-append "#cgo linux CPPFLAGS: -I"
                                  #$(this-package-input "libedit")
                                  "/include -Ishim\n"))
                  (("#cgo linux LDFLAGS.*") "#cgo linux LDFLAGS: -ledit\n"))))))))
    (inputs
     (list libedit))
    (home-page "https://github.com/knz/go-libedit")
    (synopsis "Go wrapper around @code{libedit}")
    (description
     "This package provides a wrapper around @code{libedit} for Golang.")
     (license license:asl2.0)))

(define-public go-github-com-knz-lipgloss-convert
  (package
    (name "go-github-com-knz-lipgloss-convert")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/knz/lipgloss-convert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x9p89jfafkvsz3rrk856bniwcxlj8b1bhi0ixyngh620mx4h0cq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/knz/lipgloss-convert"
      #:test-flags #~(list "-skip" "TestExport/full")))
    (propagated-inputs
     (list go-github-com-charmbracelet-lipgloss
           go-github-com-kr-pretty
           go-github-com-pmezard-go-difflib))
    (home-page "https://github.com/knz/lipgloss-convert")
    (synopsis "String conversion functions for lipgloss Styles")
    (description
     "String conversion functions for
@url{https://github.com/charmbracelet/lipgloss, lipgloss} Styles.")
    (license license:asl2.0)))

(define-public go-github-com-komkom-toml
  (package
    (name "go-github-com-komkom-toml")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/komkom/toml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rl44jkfdwzjqp31aif6ywyli6pzl2999wp3807vzxz0yd6chwfw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/komkom/toml"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pkg-errors))
    (home-page "https://github.com/komkom/toml")
    (synopsis "Instream TOML to JSON encoder")
    (description
     "This package provides a TOML parser and JSON encoder.")
    (license license:expat)))

(define-public go-github-com-kortschak-utter
  (package
    (name "go-github-com-kortschak-utter")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kortschak/utter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pycm9kkfwpxz10v6f3w0478qy66bzaginr94rnmnnz7kp40fa00"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kortschak/utter"))
    (home-page "https://github.com/kortschak/utter")
    (synopsis "Deep pretty printer")
    (description
     "This package implements a deep pretty printer for Go data structures to
aid data snapshotting.")
    (license license:isc)))

(define-public go-github-com-kpango-fastime
  (package
    (name "go-github-com-kpango-fastime")
    (version "1.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kpango/fastime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18f1p5wf0zf73ky0h2hqfa6b6zryf7pq7k2r02if673x7bjlbp9h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kpango/fastime"))
    (home-page "https://github.com/kpango/fastime")
    (synopsis "Fast time function library for Go")
    (description
     "@code{fastime} is a time function library for Go with zero memory
allocation.  @code{fastime} is returning the approximate time.")
    (license license:expat)))

(define-public go-github-com-kpango-glg
  (package
    (name "go-github-com-kpango-glg")
    (version "1.6.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kpango/glg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k6y8nvj0q8mz362490cmcx15rhcpyx4sf4rv153dgh46acd1phh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kpango/glg"))
    (propagated-inputs
     (list go-github-com-goccy-go-json
           go-github-com-kpango-fastime
           go-github-com-sirupsen-logrus
           go-go-uber-org-zap))
    (home-page "https://github.com/kpango/glg")
    (synopsis "Lock-free logging library for Go")
    (description
     "@code{glg} is simple lock-free logging library for Go.")
    (license license:expat)))

(define-public go-github-com-kr-fs
  (let ((commit "1455def202f6e05b95cc7bfc7e8ae67ae5141eba")
        (revision "0"))
    (package
      (name "go-github-com-kr-fs")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kr/fs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11zg176x9hr9q7fsk95r6q0wf214gg4czy02slax4x56n79g6a7q"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/kr/fs"
        #:test-flags #~(list "-skip" "TestBug3486")))
      (home-page "https://github.com/kr/fs")
      (synopsis "File-system-related functions for Go")
      (description
       "The fs package provides file-system-related Go functions.")
      (license license:bsd-3))))

(define-public go-github-com-kr-pretty
  (package
    (name "go-github-com-kr-pretty")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kr/pretty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19d4ycy22il43s4pnr7jv1aahp87wa1p16zpis5jdiiyfgni2l8f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kr/pretty"))
    (propagated-inputs
     (list go-github-com-kr-text go-github-com-rogpeppe-go-internal))
    (home-page "https://github.com/kr/pretty")
    (synopsis "Pretty printer for Go values")
    (description
     "This package provides a pretty printer for Go values.")
    (license license:expat)))

(define-public go-github-com-kr-text
  (package
    (name "go-github-com-kr-text")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kr/text")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hf58ypz6rxsw6nx3i856whir9lvy4sdx946wbw1nfaf2rdmr9vx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kr/text"))
    (propagated-inputs
     (list go-github-com-creack-pty))
    (home-page "https://github.com/kr/text")
    (synopsis "Text formatting in Go")
    (description "This package provides a text formatting functions in Go.")
    (license license:expat)))

(define-public go-github-com-kylebanks-depth
  (package
    (name "go-github-com-kylebanks-depth")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KyleBanks/depth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19gnz1w3ny3dawdhfnfsr17ll11isgk0jmrbfn2hsa6yqzc7jd3k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/KyleBanks/depth"
      #:test-subdirs #~(list ".")))
    (home-page "https://github.com/KyleBanks/depth")
    (synopsis "Visualize Golang Dependency Trees")
    (description
     "Package depth provides an ability to traverse and retrieve Go source
code dependencies in the form of internal and external packages.")
    (license license:expat)))

(define-public go-github-com-kylelemons-godebug
  (package
    (name "go-github-com-kylelemons-godebug")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kylelemons/godebug")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dkk3friykg8p6wgqryx6745ahhb9z1j740k7px9dac6v5xjp78c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/kylelemons/godebug"))
    (home-page "https://github.com/kylelemons/godebug")
    (synopsis "Pretty printer for Go values")
    (description
     "This package will pretty print a compact representation of a Go data
structure.  It can also produce a much more verbose, one-item-per-line
representation suitable for computing diffs.")
    (license license:asl2.0)))

(define-public go-github-com-labstack-gommon
  (package
    (name "go-github-com-labstack-gommon")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/labstack/gommon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05mafzmx050hc3js3i0h05ga81kk3hhhlv395xwzv9n38h27xpnz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/labstack/gommon"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty
           go-github-com-valyala-fasttemplate))
    (home-page "https://github.com/labstack/gommon")
    (synopsis "Common libraries for Go")
    (description
     "This package provides functionlaity for common tasks:
@itemize
@item @code{Bytes} - format/parse bytes
@item @code{Color} - style terminal text
@item @code{Log} - simple logging
@end itemize")
    (license license:expat)))

(define-public go-github-com-landlock-lsm-go-landlock
  (package
    (name "go-github-com-landlock-lsm-go-landlock")
    (version "0.0.0-20250303204525-1544bccde3a3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/landlock-lsm/go-landlock")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00qis8gg2ajyph9jyrjghm6cn0h22pwjwdp6qa22ji6jslgnm02n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/landlock-lsm/go-landlock"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-kernel-org-pub-linux-libs-security-libcap-psx))
    (home-page "https://landlock.io/")
    (synopsis "Golang library for the Linux Landlock sandboxing feature")
    (description
     "This package implements a restriction for the current processes' ability
to use files, using Linux 5.13's
@url{https://docs.kernel.org/security/landlock.html, Landlock} feature.")
    (license license:expat)))

(define-public go-github-com-lann-builder
  (package
    (name "go-github-com-lann-builder")
    (version "0.0.0-20180802200727-47ae307949d0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lann/builder")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kg9jy1rciznj627hafpq2mi7hr5d3ssgqcpwrm3bnlk9sqnydil"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lann/builder"))
    (propagated-inputs
     (list go-github-com-lann-ps))
    (home-page "https://github.com/lann/builder")
    (synopsis "Fluent immutable builders for Golang")
    (description
     "Package builder provides a method for writing fluent immutable DSL
builders.  It uses immutable persistent data structures so that each step in
the method chain can be reused.")
    (license license:expat)))

(define-public go-github-com-lann-ps
  (package
    (name "go-github-com-lann-ps")
    (version "0.0.0-20150810152359-62de8c46ede0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lann/ps")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10yhcyymypvdiiipchsp80jbglk8c4r7lq7h54v9f4mxmvz6xgf7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lann/ps"))
    (home-page "https://github.com/lann/ps")
    (synopsis "Persistent data structures for Golang")
    (description
     "This package implements a fully persistent data structures - a data
structure that always preserves the previous version of itself when it is
modified.  Such data structures are effectively immutable, as their operations
do not update the structure in-place, but instead always yield a new
structure.  It's a stable fork of https://github.com/mndrix/ps.")
    (license license:expat)))

(define-public go-github-com-layeh-gopher-luar
  (package
    (name "go-github-com-layeh-gopher-luar")
    (version "1.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/layeh/gopher-luar")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zfafqy2jwjmrr0gl3h2ivn0iixb0bvslcwcly9bcmc5yxq35m89"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "layeh.com/gopher-luar"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (propagated-inputs (list go-github-com-yuin-gopher-lua))
    (home-page "https://github.com/layeh/gopher-luar")
    (synopsis "Simplifies data passing to and from gopher-lua")
    (description
     "Package @code{gopher-luar} simplifies data passing to and from
 @url{https://github.com/yuin/gopher-lua, gopher-lua}.")
    (license license:mpl2.0)))

(define-public go-github-com-leodido-go-urn
  (package
    (name "go-github-com-leodido-go-urn")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leodido/go-urn")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bn9dj6y299jdh8szfim32yxj9zip38cqgv965dj23cixgr7baxb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/leodido/go-urn"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/leodido/go-urn")
    (synopsis "Parser for uniform resource names as seen on RFC 2141")
    (description
     "This package implements a parser for uniform resource names (URN) as
specified by @uref{https://tools.ietf.org/html/rfc2141, IETF RFC 2141}.")
    (license license:expat)))

(define-public go-github-com-leonelquinteros-gotext
  (package
    (name "go-github-com-leonelquinteros-gotext")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leonelquinteros/gotext")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "098iizlr05xj16b2mxwpa8bmcfm3fnlhm8a13kdzmw9dz11jjsm9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/leonelquinteros/gotext"
      #:test-flags #~(list "-vet=off")))
    (propagated-inputs
     (list go-golang-org-x-tools))
    (home-page "https://github.com/leonelquinteros/gotext")
    (synopsis "GNU gettext utilities in Go")
    (description
     "This package implements GNU gettext utilities in Go.  It features:

@itemize
@item Implements GNU gettext support in native Go.
@item Complete support for PO files including:
@item Support for MO files.
@item Thread-safe: This package is safe for concurrent use across multiple
goroutines.
@item It works with UTF-8 encoding as it's the default for Go language.
@item Unit tests available.
@item Language codes are automatically simplified from the form en_UK to en if
the first isn't available.
@item Ready to use inside Go templates.
@item Objects are serializable to []byte to store them in cache.
@item Support for Go Modules.
@end itemize")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-backoff-v2
  (package
    (name "go-github-com-lestrrat-go-backoff-v2")
    (version "2.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lestrrat-go/backoff")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s939szsdv0ggp69rig8dkl74s5dvwzm5cw80h0b3dvkqhikim5d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/backoff"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/bench")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-lestrrat-go-option))
    (home-page "https://github.com/lestrrat-go/backoff")
    (synopsis "Idiomatic backoff for Golang")
    (description
     "This library is an implementation of
@url{https://en.wikipedia.org/wiki/Exponential_backoff, backoff algorithm} for
retrying operations.  It respects @code{context.Context} natively, and the
critical notifications are done through channel operations, allowing you to
write code that is both more explicit and flexibile.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-blackmagic
  (package
    (name "go-github-com-lestrrat-go-blackmagic")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lestrrat-go/blackmagic")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vyij1wnsh85vqi70sq0kgwrnx4zrn4yx8nk5lqd630g1akqwr8y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/blackmagic"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/blackmagic")
    (synopsis "Reflect-based black magic for Golang")
    (description
     ;; XXX: REAMDE lacks of any description at all, code not documented
     ;; either.
     "This package implements a reflect-based black magic for Go.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-envload
  (package
    (name "go-github-com-lestrrat-go-envload")
    (version "0.0.0-20180220234015-a3eb8ddeffcc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/envload")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hlhvygfg67w8pqmjl91124zggnz6m750vjmmjlf8ys63nv3na05"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/envload"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/envload")
    (synopsis "Restore and load environment variables")
    (description
     "This package implements a Perl5 like @code{temporary} variable, for
applications requiring reloading of configuration from environment variables
or during the tests temporarily change the value of an environment variable in
Golang.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-iter
  (package
    (name "go-github-com-lestrrat-go-iter")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lestrrat-go/iter")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p5fhw5g3kh7c6hvw2mc1r4ckxb3ax262x8b736yyhpv2ynl8jyz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/lestrrat-go/iter"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/iter")
    (synopsis "Iterator for arbitrary array and map types for Golang")
    (description
     "This package provides a set of utilities to safely iterate over
arbitrary array and maps types.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-option
  (package
    (name "go-github-com-lestrrat-go-option")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lestrrat-go/option")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p9744hpdxsnimha5i0gyn7hxn2fy3dxqhlpqvj5s3pc5xv3s14h"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/lestrrat-go/option/cmd/genoptions
            (delete-file-recursively "cmd/genoptions")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/option"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-lestrrat-go-blackmagic))
    (home-page "https://github.com/lestrrat-go/option")
    (synopsis "Base option type for Go")
    (description
     "This package provides a Golang implementation of the Base object for the
\"Optional Parameters Pattern\".")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-option-v2
  (package
    (inherit go-github-com-lestrrat-go-option)
    (name "go-github-com-lestrrat-go-option-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/option")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07w19iwqna9zml1x3zsrnm8dzq07kbwd3isf31293rm5i73rkmwq"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/lestrrat-go/option/cmd/genoptions
            (delete-file-recursively "cmd/genoptions")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/option/v2"))))

(define-public go-github-com-lestrrat-go-strftime
  (package
    (name "go-github-com-lestrrat-go-strftime")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/strftime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iqzxmj3ijldjf99acy44qrrzvfxzn0vza3m0c9bw46bg8v1wsyc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/strftime"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'remove-benchmarks
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively
                        (string-append "src/" import-path "/bench")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pkg-errors
           go-github-com-lestrrat-go-envload))
    (home-page "https://github.com/lestrrat-go/strftime")
    (synopsis "Strftime for Golang")
    (description
     "This package provides a Golang library implementing the conversion of
date and time information from a given calendar time to a character string
according to a format string.  It is optimized for scenarios where the same
pattern is called repeatedly.")
    (license license:expat)))

(define-public go-github-com-lib-pq
  (package
    (name "go-github-com-lib-pq")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lib/pq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08j1smm6rassdssdks4yh9aspa1dv1g5nvwimmknspvhx8a7waqz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lib/pq"
      ;; The tests seem to fail without access to the network or a running
      ;; Postgres instance.
      #:tests? #f))
    (home-page "https://github.com/lib/pq")
    (synopsis "Golang Postgres driver for Go's database/sql")
    (description
     "This package provides a pure Go Postgres driver for Go's
database/sql package.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-buffer-pool
  (package
    (name "go-github-com-libp2p-go-buffer-pool")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-buffer-pool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0514rsnin6wjqifpg66dp5nrwh40smqlkgs3kxyz9cansi78c2n1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-buffer-pool"))
    (home-page "https://github.com/libp2p/go-buffer-pool")
    (synopsis "Variable size buffer pool for Golang")
    (description
     "This package provides a variable size buffer pool for Golang.

@code{go-buffer-pool} provides:
@itemize
@item @code{BufferPool}: A pool for re-using byte slices of varied sizes.
This pool will always return a slice with at least the size requested and a capacity
up to the next power of two.  Each size class is pooled independently which makes the
@code{BufferPool} more space efficient than a plain @code{sync.Pool} when used in
situations where data size may vary over an arbitrary range.
@item @code{Buffer}: a buffer compatible with @code{bytes.Buffer} but backed by a
@code{BufferPool}.  Unlike @code{bytes.Buffer}, @code{Buffer} will automatically
shrink on read, using the buffer pool to avoid causing too much work for the
allocator.  This is primarily useful for long lived buffers that usually sit empty.
@end itemize")
    ;; There are two license files provided by the project: LICENSE and
    ;; LICENSE-BSD.
    (license (list license:expat license:bsd-3))))

(define-public go-github-com-libp2p-go-msgio
  (package
    (name "go-github-com-libp2p-go-msgio")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-msgio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "005cdmkcgsfqlf8478wxyzmy5iixqa8fhjrbig912n8ngnqx1029"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-msgio"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-gogo-protobuf
           go-github-com-libp2p-go-buffer-pool
           go-github-com-multiformats-go-varint
           go-google-golang-org-protobuf))
    (home-page "https://github.com/libp2p/go-msgio")
    (synopsis "Read and write length-delimited slices")
    (description
     "@code{go-msgio} implements functionality to read and write
length-delimited slices.  It's helpful for building wire protocols.")
    (license license:expat)))

(define-public go-github-com-lithammer-dedent
  (package
    (name "go-github-com-lithammer-dedent")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lithammer/dedent")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zqdlz3csraphk12q91vmv5zhap3abscjn9v725d8r55qblwrvs0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lithammer/dedent"))
    (home-page "https://github.com/lithammer/dedent")
    (synopsis "Remove any common leading whitespace from multiline strings")
    (description
     "This package provides a functionality to remove common leading
whitespace from multiline strings.  Inspired by
@url{https://docs.python.org/3/library/textwrap.html#textwrap.dedent,(code
textwrap.dedent)} in Python.")
    (license license:expat)))

(define-public go-github-com-lithammer-fuzzysearch
  (package
    (name "go-github-com-lithammer-fuzzysearch")
    (version "1.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lithammer/fuzzysearch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fp00gzbrr5fnz01lmkjqcs5z24zjrsp4r13ia0x0wslp5r13hv8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lithammer/fuzzysearch"
      #:skip-build? #t))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://github.com/lithammer/fuzzysearch")
    (synopsis "Tiny and fast fuzzy search in Go")
    (description
     "A speedy fuzzy matching package for Go inspired by the JavaScript
library bevacqua/fuzzysearch.")
    (license license:expat)))

(define-public go-github-com-liyue201-gostl
  (package
    (name "go-github-com-liyue201-gostl")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/liyue201/gostl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dxzh791agir21dp1jmfa1bvqc23byz93fx3jlm94brlgm9zdkd3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/liyue201/gostl"
      #:skip-build? #t))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/liyue201/gostl")
    (synopsis "Data structure and algorithm library for Go")
    (description
     "@code{gostl} is a data structure and algorithm library for Go, designed
to provide functions similar to C++ STL.")
    (license license:expat)))

(define-public go-github-com-logrusorgru-aurora
  (package
    (name "go-github-com-logrusorgru-aurora")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/logrusorgru/aurora")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1ck2j2ff2avph07vgq0r1y7hmbqgvk339rvph45dcwgci23lb3pf"))
       (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/logrusorgru/aurora"))
    (home-page "https://github.com/logrusorgru/aurora")
    (synopsis "Ultimate ANSI colors for Golang")
    (description
     "This package provides ANSI colors for Golang.  The package supports
Printf/Sprintf etc.")
    (license license:unlicense)))

(define-public go-github-com-logrusorgru-aurora-v3
  (package
    (inherit go-github-com-logrusorgru-aurora)
    (name "go-github-com-logrusorgru-aurora-v3")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/logrusorgru/aurora")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0z7cgj8gl69271d0ag4f4yjbsvbrnfibc96cs01spqf5krv2rzjc"))
       (file-name (git-file-name name version))))
    (arguments
     (list
      #:import-path "github.com/logrusorgru/aurora/v3"))))

(define-public go-github-com-logrusorgru-aurora-v4
  (package
    (inherit go-github-com-logrusorgru-aurora)
    (name "go-github-com-logrusorgru-aurora-v4")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/logrusorgru/aurora")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0a4w4p0sl5hwa9fridk7s023sjcis8qf1k8fm3g5qar58vxzlh9w"))
       (file-name (git-file-name name version))))
    (arguments
     (list
      #:import-path "github.com/logrusorgru/aurora/v4"))
    (native-inputs
     (list go-github-com-stretchr-testify))))

(define-public go-github-com-lucasb-eyer-go-colorful
  (package
    (name "go-github-com-lucasb-eyer-go-colorful")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lucasb-eyer/go-colorful")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08c3fkf27r16izjjd4w94xd1z7w1r4mdalbl53ms2ka2j465s3qs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lucasb-eyer/go-colorful"))
    (propagated-inputs
     (list go-golang-org-x-image))
    (home-page "https://github.com/lucasb-eyer/go-colorful")
    (synopsis "Library for playing with colors in Go")
    (description
     "The colorful package provides a library for using colors in Go.
It stores colors in RGB and provides methods for converting these to various
color spaces.")
    (license license:expat)))

(define-public go-github-com-lunixbochs-vtclean
  (package
    (name "go-github-com-lunixbochs-vtclean")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lunixbochs/vtclean")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jqn33l1kzy4dk66zbvxz7rlgkgg34s9mhc8z0lrz0i88466zhd8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lunixbochs/vtclean"))
    (home-page "https://github.com/lunixbochs/vtclean")
    (synopsis "Filter out terminal escape sequences")
    (description
     "The @code{vtclean} provides the @command{vtclean} command and a library
designed to clean up raw terminal output by stripping escape sequences,
optionally preserving color.")
    (license license:expat)))

(define-public go-github-com-lxc-go-lxc
  (package
    (name "go-github-com-lxc-go-lxc")
    (version "0.0.0-20240606200241-27b3d116511f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lxc/go-lxc")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "020jqzqzcq73jbi82cdxkv5nqa36a2322y2c2hwf8fyw3f2809yd"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; The final package needs to include pkg-config and lxc, it is to
      ;; prevent importing virtualization module here.
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/lxc/go-lxc"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/lxc/go-lxc")
    (synopsis "Golang bindings for LXC")
    (description
     "Package lxc provides Go Bindings for LXC (Linux Containers) C API.")
    (license license:lgpl2.1)))

(define-public go-github-com-lyft-protoc-gen-star-v2
  (package
    (name "go-github-com-lyft-protoc-gen-star-v2")
    (version "2.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lyft/protoc-gen-star")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ay8fni2m7ic811qjisinxiw1hl7c6fflwnv3hwlghsnwimnlq9f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lyft/protoc-gen-star/v2"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list "TestGraph_Bidirectional"
                     "TestGraph_Bidirectional_Messages_Enums"
                     "TestGraph_Bidirectional_Recursive"
                     "TestGraph_Extensions"
                     "TestGraph_FDSet"
                     "TestGraph_Messages"
                     "TestGraph_Packageless"
                     "TestGraph_Services"
                     "TestGraph_SourceCodeInfo")
               "|"))
      ;; XXX: To enable all tests require more complex set up, check how to
      ;; enable most of them.
      #:test-subdirs #~(list ".")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-spf13-afero
           go-golang-org-x-tools
           go-google-golang-org-protobuf))
    (home-page "https://github.com/lyft/protoc-gen-star")
    (synopsis "Protoc plugin library for efficient proto-based code generation")
    ;; The Project lacks a good README, suggesting to read source code
    ;; "... the true documentation source is the code itself ...".
    (description
     "PG* is a protoc plugin library for efficient proto-based code
generation.")
    (license license:asl2.0)))

(define-public go-github-com-lyft-protoc-gen-star-v2-next
  ;; The latest (2.0.3) version lacks some fixes, this variant points to the
  ;; latest commit on master branch.
  (let ((commit "496ad1ac90a4573d8b89f09e6ef5f8e25dd4adb8")
        (revision "0"))
  (package
    (inherit go-github-com-lyft-protoc-gen-star-v2)
    (name "go-github-com-lyft-protoc-gen-star-v2")
    (version (git-version "2.0.3" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lyft/protoc-gen-star")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jdgzaq3r1bs12f0a0y84vnrc01m9xzvsk55cvcfspkv14pscqjy")))))))

(define-public go-github-com-magiconair-properties
  (package
    (name "go-github-com-magiconair-properties")
    (version "1.8.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/magiconair/properties")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cmbh9y51lbn2q2i2jzjfd14spwclg88hfsj4k1kkj1xc2bkwqdj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/magiconair/properties"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/magiconair/properties")
    (synopsis "Java properties scanner for Go")
    (description "Java properties scanner for Go")
    (license license:bsd-2)))

(define-public go-github-com-makenowjust-heredoc
  (package
    (name "go-github-com-makenowjust-heredoc")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/makenowjust/heredoc")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18f21zm8n2wlnkz1ylw8rcxmqxyv2rlz8749yfqggm2m0m2884pj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/MakeNowJust/heredoc"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/MakeNowJust/heredoc")
    (synopsis "Here-documents with indent")
    (description
     "This package implements a functionality of creating here-documents from
raw strings.")
    (license license:expat)))

(define-public go-github-com-makenowjust-heredoc-v2
  (package
    (inherit go-github-com-makenowjust-heredoc)
    (name "go-github-com-makenowjust-heredoc-v2")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/makenowjust/heredoc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k4ggh0dkz3919m7gy2avhj0drjxrcvm01a5dgsc3z971yk6h5xw"))))
    (arguments
     (list
      #:import-path "github.com/MakeNowJust/heredoc/v2"))))

(define-public go-github-com-marcinbor85-gohex
  ;; No release, see <https://github.com/marcinbor85/gohex/issues/5>.
  (let ((commit "baab2527a9a2a4abb3dc06baabedfa5e0268b8d8")
        (revision "0"))
    (package
      (name "go-github-com-marcinbor85-gohex")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/marcinbor85/gohex")
               (commit commit)))
         (sha256
          (base32 "06v4cc6ld6vvxd4xm9k6l49lhcd9ncq7xfx35mj5b9r96ih49fiz"))
         (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/marcinbor85/gohex"))
      (home-page "https://pkg.go.dev/github.com/marcinbor85/gohex")
      (synopsis "Parse Intel HEX files")
      (description
       "This package provides a Golang library for parsing Intel HEX files,
implementing features like:

@itemize
@item robust intelhex parsing (full test coverage)
@item support i32hex format
@item two-way converting hex<->bin
@item trivial but powerful api (only the most commonly used functions)
@item interface-based IO functions
@end itemize")
      (license license:expat))))

(define-public go-github-com-masterminds-goutils
  (package
    (name "go-github-com-masterminds-goutils")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Masterminds/goutils")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09m4mbcdlv9ng3xcrmjlxi0niavby52y9nl2jhjnbx1xxpjw0jrh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Masterminds/goutils"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/Masterminds/goutils/")
    (synopsis "Utility functions to manipulate strings")
    (description
     "GoUtils provides utility functions to manipulate strings in various
ways.  It is a Go implementation of some string manipulation libraries of Java
Apache Commons.")
    (license license:asl2.0)))

(define-public go-github-com-masterminds-semver
  (package
    (name "go-github-com-masterminds-semver")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Masterminds/semver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i169xscsxsh8lsw8bz2apnsqixld37xdnfh36i30xy5wnf0iwfx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Masterminds/semver"))
    (home-page "https://github.com/Masterminds/semver")
    (synopsis "@code{semver} helps to work with semantic versions")
    (description
     "Package semver provides the ability to work with
@url{https://semver.org, Semantic Versions} in Go.")
    (license license:expat)))

(define-public go-github-com-masterminds-semver-v3
  (package
    (name "go-github-com-masterminds-semver-v3")
    (version "3.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Masterminds/semver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h4c647dgq6k5q78j3m98ccdrzd7kbcq4ahdy25j72rbxjmci8al"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Masterminds/semver/v3"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/Masterminds/semver/")
    (synopsis "@code{semver} helps to work with semantic versions")
    (description
     "The semver package provides the ability to work with
semantic versions.  Specifically it provides the ability to:
@itemize
@item Parse semantic versions
@item Sort semantic versions
@item Check if a semantic version fits within a set of constraints
@item Optionally work with a @code{v} prefix
@end itemize")
    (license license:expat)))

(define-public go-github-com-masterminds-sprig-v3
  (package
    (name "go-github-com-masterminds-sprig-v3")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Masterminds/sprig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ljpizbfjh29xb4f40ipkrqriyixhdsfnd72y3pdzrjf2kbmgw9n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Masterminds/sprig/v3"
      #:test-flags #~(list "-skip" "TestGetHostByName")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-dario-cat-mergo
           go-github-com-google-uuid
           go-github-com-huandu-xstrings
           go-github-com-masterminds-goutils
           go-github-com-masterminds-semver-v3
           go-github-com-mitchellh-copystructure
           go-github-com-shopspring-decimal
           go-github-com-spf13-cast
           go-golang-org-x-crypto))
    (home-page "https://github.com/Masterminds/sprig/")
    (synopsis "Template functions for Go templates")
    (description
     "Sprig is a library that provides more than 100 commonly used template
functions.")
    (license license:expat)))

(define-public go-github-com-masterminds-squirrel
  (package
    (name "go-github-com-masterminds-squirrel")
    (version "1.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Masterminds/squirrel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ws7p3gchw6n81sfdhpk1pbh7gsj1fm7fbjah702d7q1gbn00vja"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Masterminds/squirrel"))
    (native-inputs
     (list go-github-com-go-sql-driver-mysql
           go-github-com-lib-pq
           go-github-com-mattn-go-sqlite3
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-lann-builder))
    (home-page "https://github.com/Masterminds/squirrel")
    (synopsis "Fluent SQL generation for golang")
    (description
     "Package squirrel provides a fluent SQL generator.")
    (license license:expat)))

(define-public go-github-com-matryer-try
  (package
    (name "go-github-com-matryer-try")
    (version "0.0.0-20161228173917-9ac251b645a2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matryer/try")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19fnqmpl3p54vmxgm1hmqvdc87brqx754wf3cdhq1bj04fcbb5h9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/matryer/try"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (substitute* (string-append "src/" import-path
                                          "/try_test.go")
                (("var value string")
                 "")
                (("value, err = SomeFunction\\(\\)")
                 "_, err = SomeFunction()")))))))
    (native-inputs
     (list go-github-com-cheekybits-is))
    (home-page "https://github.com/matryer/try")
    (synopsis "Simple idiomatic retry package for Go")
    (description "This package provides an idiomatic Go retry module.")
    (license license:expat)))

(define-public go-github-com-mattn-go-colorable
  (package
    (name "go-github-com-mattn-go-colorable")
    (version "0.1.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-colorable")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05hl2ddp67p5kj3ix4zzqqjh4fan4ban3vgw8f98simwigs3q41j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-colorable"))
    (propagated-inputs
     (list go-github-com-mattn-go-isatty))
    (home-page "https://github.com/mattn/go-colorable")
    (synopsis "Handle ANSI color escapes on Windows")
    (description
     "This package provides @code{colorable}, a module that makes it possible
to handle ANSI color escapes on Windows.")
    (license license:expat)))

(define-public go-github-com-mattn-go-isatty
  (package
    (name "go-github-com-mattn-go-isatty")
    (version "0.0.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-isatty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g63n9wpb991qnq9mn2kvd8jk1glrp6gnd851kvwz2wmzdkggiga"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-isatty"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/mattn/go-isatty")
    (synopsis "Provide @code{isatty} for Golang")
    (description
     "This package provides @code{isatty}, a Go module that can tell you
whether a file descriptor points to a terminal and the type of the terminal.")
    (license license:expat)))

(define-public go-github-com-mattn-go-pointer
  (package
    (name "go-github-com-mattn-go-pointer")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-pointer")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1px9kj2xwwi7r00qxxpidr23xi823kw0pkd6f50lib8bp60x3n7p"))
       (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mattn/go-pointer"))
    (home-page "https://github.com/mattn/go-pointer")
    (synopsis "Utility for cgo")
    (description
     "This package allows for a cgo argument to be passed a Go pointer.")
    (license license:expat)))

(define-public go-github-com-mattn-go-runewidth
  (package
    (name "go-github-com-mattn-go-runewidth")
    (version "0.0.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-runewidth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d7wbfz1kd3m0a4sx0ijrnbn4kw3bhn6myvnk76s19h8zjvafbrl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-runewidth"))
    (propagated-inputs
     (list go-github-com-rivo-uniseg))
    (home-page "https://github.com/mattn/go-runewidth")
    (synopsis "Rune width implementation for Go")
    (description
     "This package provides functions to get the fixed width of a character or
string.")
    (license license:expat)))

(define-public go-github-com-mattn-go-shellwords
  (package
    (name "go-github-com-mattn-go-shellwords")
    (version "1.0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-shellwords")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l0l5s4hlsrm4z6hygig2pp1qirk5ycrzn9z27ay3yvg9k7zafzx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/mattn/go-shellwords"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-sh-path
            (lambda* (#:key import-path #:allow-other-keys)
              (substitute* (string-append
                            "src/" import-path "/util_posix.go")
                (("/bin/sh") (which "sh"))))))))
    (home-page "https://github.com/mattn/go-shellwords")
    (synopsis "Parse lines into shell words")
    (description "This package parses text into shell arguments.  Based on
the @code{cpan} module @code{Parse::CommandLine}.")
    (license license:expat)))

;; For fzf@0.60.2
(define-public go-github-com-junegunn-go-shellwords
  (let ((commit "2aa3b3277741a6ad31883f223d770221a85e9dd0")
        (revision "0"))
    (hidden-package (package (inherit go-github-com-mattn-go-shellwords)
       (name "go-github-com-junegunn-go-shellwords")
       (version (git-version "0.0.0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/junegunn/go-shellwords")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1x51lwmkf9bbn28f5682idkph70lk6xzh0w46diq6c7a9rw27b5b"))))
       (build-system go-build-system)
       (arguments
        (substitute-keyword-arguments
            (package-arguments go-github-com-mattn-go-shellwords)
          ((#:import-path _) "github.com/junegunn/go-shellwords")))))))

(define-public go-github-com-mattn-go-sixel
  (package
    (name "go-github-com-mattn-go-sixel")
    (version "0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-sixel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0icv1mcavdw867s47kwvd16q19h2a4znph850lyq18d5z00kpjjs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-sixel"))
    (native-inputs
     (list go-github-com-burntsushi-graphics-go)) ; for the CLI command
    (propagated-inputs
     (list go-github-com-soniakeys-quant))
    (home-page "https://github.com/mattn/go-sixel")
    (synopsis "DRCS/Sixel Encoder/Decoder")
    (description
     "This package implements functionality to encode and decode
@acronym{DRCS,Dynamically Redefinable Character Sets} Sixel.")
    (license license:expat)))

(define-public go-github-com-mattn-go-sqlite3
  (package
    (name "go-github-com-mattn-go-sqlite3")
    (version "1.14.22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-sqlite3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05fcdh6likz0hkvxnrkz3r3l5gzxfjh93w5015m9hs1wi6qpdqyb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-sqlite3"))
    (home-page "https://github.com/mattn/go-sqlite3")
    (synopsis "Sqlite3 driver for Go")
    (description
     "This package provides a Sqlite3 driver for Go using
@code{database/sql}.")
    (license license:expat)))

(define-public go-github-com-mattn-go-tty
  (package
    (name "go-github-com-mattn-go-tty")
    (version "0.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-tty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09ndgwrx99jqaakmhk4v2pnai9h2mvryapc3qg6i33v2x809y6z6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-tty"))
    (propagated-inputs
     (list go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-runewidth
           go-golang-org-x-sys))
    (home-page "https://github.com/mattn/go-tty")
    (synopsis "Simple TTY utility for Golang")
    (description
     "This package provides a TTY utilities implementation for verity of
operation systems.")
    (license license:expat)))

(define-public go-github-com-mattn-go-zglob
  (package
    (name "go-github-com-mattn-go-zglob")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-zglob")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1923lvakm66mzy62jmngdvcmbmiqclinsvnghs3907rgygnx1qc1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-zglob"))
    (home-page "https://github.com/mattn/go-zglob")
    (synopsis "Glob library that descends into other directories")
    (description
     "This package provides a glob library that implements descending into
other directories.  It is optimized for filewalking.")
    (license license:expat)))

(define-public go-github-com-matttproud-golang-protobuf-extensions-v2
  (package
    (name "go-github-com-matttproud-golang-protobuf-extensions-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/matttproud/golang_protobuf_extensions")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jw4vjycwx0a82yvixmp25805krdyqd960y8lnyggllb6br0vh41"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/matttproud/golang_protobuf_extensions/v2"
      #:skip-build? #t))
    (propagated-inputs
     (list go-github-com-golang-protobuf
           go-google-golang-org-protobuf))
    (home-page "https://github.com/matttproud/golang_protobuf_extensions")
    (synopsis "Streaming Protocol Buffers in Go")
    (description
     "This package provides various Protocol Buffer extensions for the Go
language, namely support for record length-delimited message streaming.")
    (license license:asl2.0)))

(define-public go-github-com-mgutz-ansi
  (package
    (name "go-github-com-mgutz-ansi")
    (version "0.0.0-20200706080929-d51e80ef957d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mgutz/ansi")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yfj9fj1m7wfvlmf328w8awyvqsvjmyv9yfsnv9r8al9i9wrv3j5"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/mgutz/ansi"))
    (propagated-inputs
     (list go-github-com-mattn-go-isatty go-github-com-mattn-go-colorable))
    (home-page "https://github.com/mgutz/ansi")
    (synopsis "Small, fast library to create ANSI colored strings and codes")
    (description
     "This package provides @code{ansi}, a Go module that can generate ANSI
colored strings.")
    (license license:expat)))

(define-public go-github-com-michiwend-golang-pretty
  (package
    (name "go-github-com-michiwend-golang-pretty")
    (version "0.0.0-20141116172505-8ac61812ea3f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/michiwend/golang-pretty")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rjfms0csjqi91xnddzx3rcrcaikc7xc027617px3kdwdap80ir4"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f                    ; Upstream tests seem to be broken.
       #:import-path "github.com/michiwend/golang-pretty"))
    (propagated-inputs
     (list go-github-com-kr-text))
    (home-page "https://github.com/michiwend/golang-pretty")
    (synopsis "Pretty printing for Go values")
    (description
     "Package @code{pretty} provides pretty-printing for Go values.  This is
useful during debugging, to avoid wrapping long output lines in the terminal.

It provides a function, @code{Formatter}, that can be used with any function
that accepts a format string.  It also provides convenience wrappers for
functions in packages @code{fmt} and @code{log}.")
    (license license:expat)))

(define-public go-github-com-michiwend-gomusicbrainz
  (package
    (name "go-github-com-michiwend-gomusicbrainz")
    (version "0.0.0-20181012083520-6c07e13dd396")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/michiwend/gomusicbrainz")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c13hyv34l5hm7hrwrm62s2414ivx6dbbyhb7rw90bni78q441b1"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/michiwend/gomusicbrainz"))
    (propagated-inputs
     (list go-github-com-michiwend-golang-pretty))
    (home-page "https://github.com/michiwend/gomusicbrainz")
    (synopsis "MusicBrainz WS2 client library for Golang")
    (description
     "Currently GoMusicBrainz provides methods to perform search and lookup
requests.  Browse requests are not supported yet.")
    (license license:expat)))

(define-public go-github-com-miolini-datacounter
  (package
    (name "go-github-com-miolini-datacounter")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/miolini/datacounter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s1hxqy6666qd524rdp1dr3778davc8gx9brg9lkcjvr5l05s9wa"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/miolini/datacounter"))
    (home-page "https://github.com/miolini/datacounter")
    (synopsis "Counters for Go readers and writers")
    (description
     "The datacounter package provides counters for Go readers and writers.")
    (license license:expat)))

(define-public go-github-com-mitchellh-cli
  (package
    (name "go-github-com-mitchellh-cli")
    (version "1.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gp1p7pqc27ps8njv49hmzsfnbxq4wg5azmqpqdw91qxw3prs3kr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mitchellh/cli"))
    (propagated-inputs
     (list go-github-com-armon-go-radix
           go-github-com-bgentry-speakeasy
           go-github-com-fatih-color
           go-github-com-masterminds-sprig-v3
           go-github-com-mattn-go-isatty
           go-github-com-posener-complete))
    (home-page "https://github.com/mitchellh/cli")
    (synopsis "Go library for implementing command-line interfaces")
    (description
     "cli is a library for implementing command-line interfaces.
Features:
@itemize
@item easy sub-command based CLIs: @code{cli foo}, @code{cli bar}, etc.
@item support for nested subcommands such as @code{cli foo bar}
@item optional support for default subcommands so @code{cli} does something
other than error
@item support for shell autocompletion of subcommands, flags, and arguments
with callbacks in Go
@item automatic help generation for listing subcommands
@item automatic help flag recognition of @code{-h}, @code{--help}, etc.
@item automatic version flag recognition of @code{-v}, @code{--version}
@item helpers for interacting with the terminal, such as outputting
information, asking for input, etc.
@item use of Go interfaces/types makes augmenting various parts of the library
a piece of cake
@end itemize")
    (license license:mpl2.0)))

(define-public go-github-com-mitchellh-colorstring
  (package
    (name "go-github-com-mitchellh-colorstring")
    (version "0.0.0-20190213212951-d06e56a500db")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/colorstring")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d2mi5ziszfzdgaz8dg4b6sxa63nw1jnsvffacqxky6yz9m623kn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mitchellh/colorstring"))
    (home-page "https://github.com/mitchellh/colorstring")
    (synopsis "Functions to colorize strings for terminal output")
    (description
     "Colorstring provides functions for colorizing strings for terminal output.")
    (license license:expat)))

(define-public go-github-com-mitchellh-copystructure
  (package
    (name "go-github-com-mitchellh-copystructure")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/copystructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1izw243b3r03nvgnnxvk706l3s3v3q7k69kq3n4asnzjkcymq7sm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mitchellh/copystructure"))
    (propagated-inputs
     (list go-github-com-mitchellh-reflectwalk))
    (home-page "https://github.com/mitchellh/copystructure")
    (synopsis "Go library for decoding deep copying values")
    (description "@code{copystructure} is a Go library for deep copying values
in Go.

This allows you to copy Go values that may contain reference values such as
maps, slices, or pointers, and copy their data as well instead of just their
references.")
    (license license:expat)))

(define-public go-github-com-mitchellh-go-homedir
  (package
    (name "go-github-com-mitchellh-go-homedir")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/go-homedir")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ydzkipf28hwj2bfxqmwlww47khyk6d152xax4bnyh60f4lq3nx1"))))
    (build-system go-build-system)
    (arguments
     (quote (#:import-path "github.com/mitchellh/go-homedir"
             ;; TODO: Tests fail because it tries to access home.
             #:tests? #f)))
    (home-page "https://github.com/mitchellh/go-homedir")
    (synopsis "Go library for detecting and expanding the user's home directory without cgo")
    (description
     "This is a Go library for detecting the user's home directory without the
use of @command{cgo}, so the library can be used in cross-compilation
environments.

Usage is simple, just call homedir.Dir() to get the home directory for a user,
and homedir.Expand() to expand the @command{~} in a path to the home
directory.

Why not just use @command{os/user}?  The built-in @command{os/user} package
requires cgo on Darwin systems.  This means that any Go code that uses that
package cannot cross compile.  But 99% of the time the use for
@command{os/user} is just to retrieve the home directory, which we can do for
the current user without cgo.  This library does that, enabling
cross-compilation.")
    (license license:expat)))

(define-public go-github-com-mitchellh-go-ps
  (package
    (name "go-github-com-mitchellh-go-ps")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/go-ps")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ipcbz66x7q8xczi7cyfq06y7n7v0syvkp730vn9jrn7s8f5ag0z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mitchellh/go-ps"))
    (home-page "https://github.com/mitchellh/go-ps")
    (synopsis "Process List Library for Golang")
    (description
     "This package provides an API for finding and listing processes in a
platform-agnostic way.")
    (license license:expat)))

(define-public go-github-com-mitchellh-go-wordwrap
  (package
    (name "go-github-com-mitchellh-go-wordwrap")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/go-wordwrap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12imq66hgj8q9ii2xqdy8apc0icphh6yimjb0div1pvl3s9gn83y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mitchellh/go-wordwrap"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2))
    (home-page "https://github.com/mitchellh/go-wordwrap")
    (synopsis "Go library for word-wrapping strings")
    (description
     "This Go library automatically wraps words onto multiple lines.  It's
primary goal is to format command-line output, but of course word wrapping is
a generally useful thing to do.")
    (license license:expat)))

;; XXX: This package is in maintenance mode: "This repository has been
;; archived by the owner on Jul 22, 2024. It is now read-only."
(define-public go-github-com-mitchellh-mapstructure
  (package
    (name "go-github-com-mitchellh-mapstructure")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/mapstructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10f2v143lkip8h46shd99k5yavfqpgqmd7a6y42v7szc0lcn3mff"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mitchellh/mapstructure"))
    (home-page "https://github.com/mitchellh/mapstructure")
    (synopsis "Go library for decoding generic map values")
    (description "Go library for decoding generic map values")
    (license license:expat)))

(define-public go-github-com-mitchellh-reflectwalk
  (package
    (name "go-github-com-mitchellh-reflectwalk")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/reflectwalk")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nxgb4gskzv045539vb312n0a443308dvh1akz7vi6x1l0z46zsm"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/mitchellh/reflectwalk"))
    (home-page "https://github.com/mitchellh/reflectwalk/")
    (synopsis "Walk a value in Go using reflection")
    (description
     "reflectwalk is a Go library for \"walking\" a value in Go using
reflection, in the same way a directory tree can be \"walked\" on the file
system.  Walking a complex structure can allow you to do manipulations on
unknown structures such as those decoded from JSON.")
    (license license:expat)))

(define-public go-github-com-mmcdole-goxpp
  (package
    (name "go-github-com-mmcdole-goxpp")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mmcdole/goxpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g9va3vhc5s60s00kyp4agfhb6kjp8j8i41vkj3lwbz5sx947mif"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mmcdole/goxpp"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/mmcdole/goxpp")
    (synopsis "XML pull prser for Golang")
    (description
     "The @code{goxpp} library, inspired by
@url{http://www.xmlpull.org/v1/download/unpacked/doc/quick_intro.html, Java's
XML@code{PullParser}}, is a lightweight wrapper for Go's standard XML
Decoder,tailored for developers who need fine-grained control over XML
parsing.")
    (license license:expat)))

(define-public go-github-com-mmcloughlin-geohash
  (package
    (name "go-github-com-mmcloughlin-geohash")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mmcloughlin/geohash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q97mq70jp5336gmfyv2nj9xm3qgz48lkfy4cyfdsd2rpb7zms50"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mmcloughlin/geohash"))
    (home-page "https://github.com/mmcloughlin/geohash")
    (synopsis "Geohash library for Golang")
    (description
     "@code{geohash} provides encoding and decoding of string and integer geohashes.")
    (license license:expat)))

(define-public go-github-com-moby-docker-image-spec
  (package
    (name "go-github-com-moby-docker-image-spec")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/docker-image-spec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06r6z8s0rvl66n626q41hmqgnnlpsqdblj32fjq3r0qsccp8s167"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/docker-image-spec"
      #:skip-build? #t
      #:tests? #f))
    (propagated-inputs (list go-github-com-opencontainers-image-spec))
    (home-page "https://github.com/moby/docker-image-spec")
    (synopsis "Docker Image Specification v1.")
    (description
     "This directory contains documents about Docker Image Specification v1.X.")
    (license license:asl2.0)))

(define-public go-github-com-moby-locker
  (package
    (name "go-github-com-moby-locker")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/locker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07rc2c6h35f9mcy81jp382a030f6xmcifi9n5jnlayybfwxmpjir"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/locker"))
    (home-page "https://github.com/moby/locker")
    (synopsis "Golang std @code{sync.Mutex} locker alternative")
    (description
     "Package locker provides a mechanism for creating finer-grained locking
to help free up more global locks to handle other tasks.")
    (license license:asl2.0)))

(define-public go-github-com-moby-patternmatcher
  (package
    (name "go-github-com-moby-patternmatcher")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/patternmatcher")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s77wpsc6szr9qdpnpg9q65ibgjgj4b2d12hwf6wrwb39grcnbcz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/patternmatcher"))
    (home-page "https://github.com/moby/patternmatcher")
    (synopsis "File name pattern matching")
    (description
     "This Go library provides facilities for pattern matching on file
names.")
    (license license:asl2.0)))

(define-public go-github-com-moby-pubsub
  (package
    (name "go-github-com-moby-pubsub")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/moby/pubsub")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1di8wyipxjxg9v28klzjna6a9zg5n2g5wyn1qy3klp428zzknbyw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/pubsub"))
    (home-page "https://github.com/moby/pubsub")
    (synopsis "Publish–subscribe pattern in Golang")
    (description "This package implements a
@url{https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern,
publish–subscribe pattern}.")
    (license license:asl2.0)))

(define-public go-github-com-moby-spdystream
  (package
    (name "go-github-com-moby-spdystream")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/spdystream")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p5pwwspmp24ff900656fyvrgdz8xxl6y0dk9fqgcaaaylmw0v9g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/spdystream"))
    (home-page "https://github.com/moby/spdystream")
    (synopsis "Multiplexed streams for Golang")
    (description
     "This package provides a multiplexed stream library using spdy.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-atomicwriter
  (package
    (name "go-github-com-moby-sys-atomicwriter")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "atomicwriter"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0an8ypp8v9gfxbbb71mpimb1g9labl4v7lgazcphysn6c5smgmiw"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "atomicwriter")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/atomicwriter"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs (list go-github-com-moby-sys-sequential))
    (home-page "https://github.com/moby/sys")
    (synopsis "File atomic write utilities")
    (description
     "This package provides utilities to perform atomic writes to a file or
set of files.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-capability
  (package
    (name "go-github-com-moby-sys-capability")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/moby/sys")
              (commit (go-version->git-ref version
                                           #:subdir "capability"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1140wqx5mlr9adk74k6bsswqm6dhps02cwv6k8j6nssd7ln3v514"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/capability"
      #:unpack-path "github.com/moby/sys"))
    (home-page "https://github.com/moby/sys")
    (synopsis "Basic primitives to work with Linux capabilities")
    (description
     "Package capability provides utilities for manipulating POSIX
capabilities.  It's a maintained fork of
https://github.com/syndtr/gocapability.")
    (license license:bsd-2)))

(define-public go-github-com-moby-sys-mount
  (package
    (name "go-github-com-moby-sys-mount")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/moby/sys")
              (commit (go-version->git-ref version
                                           #:subdir "mount"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nh1disclgydvq7k10awzks6k8kw9cjj3q19f83ksi4b76p5l475"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "mount")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/mount"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs
     (list go-github-com-moby-sys-mountinfo
           go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis "Mount/unmount functions in Golang")
    (description
     "This package provides a set of functions to mount and unmount mounts.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-mountinfo
  (package
    (name "go-github-com-moby-sys-mountinfo")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "mountinfo"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1phx1kk9qa4jf1i1nl23d3f6k9fn2w46274cl76cqw9hjqg868"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/moby/sys/mountinfo"
      #:unpack-path "github.com/moby/sys"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestMountedBy/not_mounted_socket"
                             "TestMountedBy/socket_bind-mounted_to_itself")
                       "|"))))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis "Retrieve information about OS mounts")
    (description
     "Package mountinfo provides a set of functions to retrieve information
about OS mounts as seen by the current process is available from
@code{/proc/self/mountinfo}.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-reexec
  (package
    (name "go-github-com-moby-sys-reexec")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "reexec"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n2z0zqfdyw6rllqdljddczh758kq22k4ajrhv27shv7m3fnvm0p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/reexec"
      #:unpack-path "github.com/moby/sys"))
    (home-page "https://github.com/moby/sys")
    (synopsis "BusyBox style reexec of a binary for Golang")
    (description
     "This package facilitates the BusyBox style reexec of a binary.
Handlers can be registered with a name and the argv 0 of the exec of the
binary will be used to find and execute custom init paths.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-sequential
  (package
    (name "go-github-com-moby-sys-sequential")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "sequential"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1phx1kk9qa4jf1i1nl23d3f6k9fn2w46274cl76cqw9hjqg868"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/sequential"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis "Go bindings to the Windows sequential file interface")
    (description
     "Package sequential provides a set of functions for managing sequential files on
Windows.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-signal
  (package
    (name "go-github-com-moby-sys-signal")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "signal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1phx1kk9qa4jf1i1nl23d3f6k9fn2w46274cl76cqw9hjqg868"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/signal"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis "Helper functions for dealing with OS signals")
    (description
     "This provides helper functions for dealing with signals across various
operating systems.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-symlink
  (package
    (name "go-github-com-moby-sys-symlink")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "symlink"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1phx1kk9qa4jf1i1nl23d3f6k9fn2w46274cl76cqw9hjqg868"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/symlink"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis "Extension of Golang's @code{path/filepath} library")
    (description
     "This package implements @code{FollowSymlinkInScope} which is an
 extension of @code{path/filepath.EvalSymlinks}, as well as a Windows
long-path aware version of @code{path/filepath.EvalSymlinks} from the Go
standard library.")
    ;; The code from [path/filepath.EvalSymlinks] has been adapted in fs.go.
    ;; Read the [LICENSE.BSD] file that governs fs.go and [LICENSE.APACHE] for
    ;; fs_unix_test.go.
    (license (list license:asl2.0
                   license:bsd-3))))

(define-public go-github-com-moby-sys-user
  (package
    (name "go-github-com-moby-sys-user")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "user"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ayv2f12za923fzyf6j4d39l54xwaijbq0xfrlfdb8xsif4nlfnb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/user"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis "Unix user and group access from Go")
    (description
     "This Go library provides facilities to access @file{/etc/passwd} and
related files.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-userns
  (package
    (name "go-github-com-moby-sys-userns")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "userns"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1clr9x412gr1cq3jxf9lxblh9pkf8c42gz57wr14miy0nqsimx7j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/userns"
      #:unpack-path "github.com/moby/sys"))
    (home-page "https://github.com/moby/sys")
    (synopsis "Probe Linux user namespace")
    (description
     "Package userns provides utilities to detect whether we are currently
running in a Linux user namespace.")
    (license license:asl2.0)))

(define-public go-github-com-moby-term
  (package
    (name "go-github-com-moby-term")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/moby/term")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05g3dn1hbk9vxzp3dm752j8zn1gy61qzxm33nsj7xisa8s6v8vgw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/term"))
    (propagated-inputs
     (list ;; go-github-com-azure-go-ansiterm ; for Windows only
           go-github-com-creack-pty
           go-golang-org-x-sys))
    (home-page "https://github.com/moby/term")
    (synopsis "Utilities for dealing with terminals")
    (description
     "Package term provides structures and helper functions to work with
terminal (state, sizes).")
    (license license:asl2.0)))

(define-public go-github-com-modern-go-concurrent
  (package
    (name "go-github-com-modern-go-concurrent")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/modern-go/concurrent")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s0fxccsyb8icjmiym5k7prcqx36hvgdwl588y0491gi18k5i4zs"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/modern-go/concurrent"))
    (home-page "https://github.com/modern-go/concurrent")
    (synopsis "Concurrency utilities for Go")
    (description
     "A Go library providing various concurrency utilities including a backport
of @code{sync.Map} to Go versions below 1.9 and a cancellable Goroutine with
explicit ownership.")
    (license license:asl2.0)))

(define-public go-github-com-modern-go-reflect2
  (package
    (name "go-github-com-modern-go-reflect2")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/modern-go/reflect2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05a89f9j4nj8v1bchfkv2sy8piz746ikj831ilbp54g8dqhl8vzr"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/modern-go/reflect2"))
    (home-page "https://github.com/modern-go/reflect2")
    (synopsis "Cheaper reflect API")
    (description
     "This library provides a reflect api for Go programs
without the runtime cost of the standard library reflect.Value.")
    (license license:asl2.0)))

(define-public go-github-com-mohae-deepcopy
  (package
    (name "go-github-com-mohae-deepcopy")
    (version "0.0.0-20170308212314-bb9b5e7adda9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mohae/deepcopy")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "173j05wv4yy8jh9ccjw46xfy1knxwvv1ir6b8l6g9pc5j5damm1f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mohae/deepcopy"))
    (home-page "https://github.com/mohae/deepcopy")
    (synopsis "Copy of pointers and values for Golang")
    (description
     "@code{deepcopy} implements a functionality of deep copies of things.  A
standard @code{copy} will copy the pointers where @code{deepcopy} copies the
values pointed to.  Unexported field values are not copied.")
    (license license:expat)))

(define-public go-github-com-monochromegane-go-gitignore
  (package
    (name "go-github-com-monochromegane-go-gitignore")
    (version "0.0.0-20200626010858-205db1a8cc00")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/monochromegane/go-gitignore")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rawqsxi6n0nwl34nc4jda7drnwfli9v4zkhf60qhl6lcipj0lwg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/monochromegane/go-gitignore"))
    (home-page "https://github.com/monochromegane/go-gitignore")
    (synopsis "Gitignore matching library for Golang")
    (description
     "This package provides a fast gitignore matching library for Go.")
    (license license:expat)))

(define-public go-github-com-morikuni-aec
  (package
    (name "go-github-com-morikuni-aec")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/morikuni/aec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qaqh0lk9wrqgff0yrxnbznvmwyhdxy3g9b2hjpazp5bw4nj0dp7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/morikuni/aec"))
    (home-page "https://github.com/morikuni/aec")
    (synopsis "Go wrapper for ANSI escape code")
    (description "This package provides a wrapper for ANSI escape code.")
    (license license:expat)))

(define-public go-github-com-motemen-go-colorine
  (package
    (name "go-github-com-motemen-go-colorine")
    (version "0.0.0-20180816141035-45d19169413a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/motemen/go-colorine")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mdy6q0926s1frj027nlzlvm2qssmkpjis7ic3l2smajkzh07118"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/motemen/go-colorine"))
    (propagated-inputs
     (list go-github-com-daviddengcn-go-colortext))
    (home-page "https://github.com/motemen/go-colorine")
    (synopsis "Simple colorized console logger for golang")
    (description
     "This package provides simple colorized console logger for golang.")
    (license license:expat)))

(define-public go-github-com-motemen-go-quickfix
  (package
    (name "go-github-com-motemen-go-quickfix")
    (version "0.0.0-20250224075427-39bb724d71b7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/motemen/go-quickfix")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j14k6kfzvfn8v21gf2ssaypicrwb4pvh7yzfa5m1jcc9581j2ad"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/motemen/go-quickfix"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (propagated-inputs (list go-golang-org-x-tools))
    (home-page "https://github.com/motemen/go-quickfix")
    (synopsis "Go ASTs fixing library")
    (description
     "The @code{quickfix} Go package provides functions for fixing Go ASTs
that are well typed but @samp{go build} refuses to build.")
    (license license:expat)))

(define-public go-github-com-mreiferson-go-options
  (package
    (name "go-github-com-mreiferson-go-options")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mreiferson/go-options")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pxs9ybrh196qy14ijn4zn51h2z28lj31y6vxrz2xxhgvpmfmxyl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mreiferson/go-options"))
    (home-page "https://github.com/mreiferson/go-options")
    (synopsis "Go package to structure and resolve options")
    (description
     "The @code{options} Go package resolves configuration values set via
command line flags, config files, and default struct values.")
    (license license:expat)))

(define-public go-github-com-mreiferson-go-svc
  ;; NSQ specific fork of github.com/judwhite/go-svc, as Guix go build system
  ;; does not support go.mod with `replace' statement.
  (let ((commit "7a96e00010f68d9436e3de53a70c53f209a0c244")
        (revision "0"))
    (package
      (name "go-github-com-mreiferson-go-svc")
      (version (git-version "1.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mreiferson/go-svc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1acgb0n3svhnraqj1fz5qc5n3b4vc5ffwyk9vfi6gcfkibm0hgmd"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/judwhite/go-svc"))
      (propagated-inputs (list go-golang-org-x-sys))
      (home-page "https://github.com/mreiferson/go-svc")
      (synopsis "Go Windows Service wrapper for GNU/Linux")
      (description
       "Go Windows Service wrapper compatible with GNU/Linux.  Windows tests
@url{https://github.com/judwhite/go-svc/raw/master/svc/svc_windows_test.go,here}.")
      (license license:expat))))

(define-public go-github-com-mrunalp-fileutils
  (package
    (name "go-github-com-mrunalp-fileutils")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mrunalp/fileutils")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nanl3vypvjlmjyjmv1ngz3lxvc5l55cn9xgr6k36wck5l37jcpi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mrunalp/fileutils"
      ;; Not able to Lstat the device: lstat /dev/mem: no such file or directory.
      #:test-flags #~(list "-skip" "TestDeviceNumbers")))
    (home-page "https://github.com/mrunalp/fileutils")
    (synopsis "Collection of utilities for file manipulation in golang")
    (description
     "This package provides a collection of utilities for file manipulation in
golang.")
    (license license:asl2.0)))

(define-public go-github-com-msteinert-pam
  (package
    (name "go-github-com-msteinert-pam")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/msteinert/pam")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qnr0zxyxny85andq3cbj90clmz2609j8z9mp0zvdyxiwryfhyhj"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; To run the full suite, the tests must be run as the root user.
      #:tests? #f
      #:import-path "github.com/msteinert/pam"))
    (propagated-inputs
     (list go-golang-org-x-term
           ;; For header files, otherwise it needs to be added as an input in
           ;; final package to prevent build failure:
           ;; ../../../github.com/msteinert/pam/transaction.go:7:10: fatal
           ;; error: security/pam_appl.h: No such file or directory
           linux-pam))
    (home-page "https://github.com/msteinert/pam")
    (synopsis "Golang wrapper module for the PAM API")
    (description
     "This package provides a wrapper for the @acronym{Pluggable
Authentication Modules, PAM} application API.")
    (license license:bsd-2)))

(define-public go-github-com-msteinert-pam-v2
  (package
    (inherit go-github-com-msteinert-pam)
    (name "go-github-com-msteinert-pam-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/msteinert/pam")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h02dcx00vgcsxgl5sly82dbixk8cimjb10q5p405bf4fz8z7q6k"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-msteinert-pam)
       ((#:import-path _ "github.com/msteinert/pam")
        "github.com/msteinert/pam/v2")))))

(define-public go-github-com-mtibben-percent
  (package
    (name "go-github-com-mtibben-percent")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mtibben/percent")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iqivw8pigj259rj5yifibbvic70f9hb7k24a4sa967s4fj6agb6"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/mtibben/percent"))
    (home-page "https://github.com/mtibben/percent")
    (synopsis "Package percent escapes strings using percent-encoding")
    (description
     "Package percent escapes strings using percent-encoding.")
    (license license:expat)))

(define-public go-github-com-muesli-ansi
  (package
    (name "go-github-com-muesli-ansi")
    (version "0.0.0-20230316100256-276c6243b2f6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/muesli/ansi")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jr8kgn3vb72jmf4a8n52g876mfpbvk3310p8gsg7jkn338af4m9"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/muesli/ansi"))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth
           go-github-com-rivo-uniseg))
    (home-page "https://github.com/muesli/ansi")
    (synopsis "Raw ANSI sequence helpers")
    (description
     "ANSI sequence helpers for working with raw ANSI sequences.")
    (license license:expat)))

(define-public go-github-com-muesli-cancelreader
  (package
    (name "go-github-com-muesli-cancelreader")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/muesli/cancelreader")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0157mgpk0z45xizrgrz73swhky0d8nyk6fhwb089n1290k7yjhxq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/muesli/cancelreader"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/muesli/cancelreader")
    (synopsis "Cancelable reader for Golang")
    (description
     "This package provides a cancelable reader for Go.")
    (license license:expat)))

(define-public go-github-com-muesli-combinator
  (package
    (name "go-github-com-muesli-combinator")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/muesli/combinator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rjw1rl1v38pn6lmyaz7yhgrwkyh22d6974lj33x1c86q0xpaw4d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/muesli/combinator"))
    (home-page "https://github.com/muesli/combinator")
    (synopsis "Cancelable reader for Go")
    (description
     "@code{combinator} generates a slice of all possible value combinations
for any given struct and a set of its potential member values.  This can be
used to generate extensive test matrixes among other things.")
    (license license:expat)))

(define-public go-github-com-muesli-reflow
  (package
    (name "go-github-com-muesli-reflow")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/muesli/reflow")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09zcz2cqdwgj1ilya5pqwndryk6lansn87x63fcm8j1xn74vd2ry"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/muesli/reflow"
      #:skip-build? #t))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/muesli/reflow/")
    (synopsis "Collection of methods helping to transform blocks of text")
    (description
     "This package provides a collection of ANSI-aware methods and io.Writers
helping you to transform blocks of text.")
    (license license:expat)))

(define-public go-github-com-muesli-termenv
  (package
    (name "go-github-com-muesli-termenv")
    (version "0.15.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/muesli/termenv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19yhli6k79aqpra4djp0cl4q76mqxbc1f7in20y0dzhnjb7yz42p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/muesli/termenv"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))
    (propagated-inputs
     (list go-github-com-aymanbagabas-go-osc52-v2
           go-github-com-lucasb-eyer-go-colorful
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-runewidth
           go-golang-org-x-sys))
    (home-page "https://github.com/muesli/termenv/")
    (synopsis "Advanced styling options on the terminal")
    (description
     "termenv lets you safely use advanced styling options on the terminal.
It gathers information about the terminal environment in terms of its ANSI and
color support and offers you convenient methods to colorize and style your
output, without you having to deal with all kinds of weird ANSI escape
sequences and color conversions.")
    (license license:expat)))

(define-public go-github-com-mufti1-interconv
  (let ((commit "d7c72925c6568d60d361757bb9f2d252dcca745c")
        (revision "0"))
    (package
      (name "go-github-com-mufti1-interconv")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/mufti1/interconv")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13f5pvr74afa28pbpmgvjzjx68vv5zmrwlvxp7hr5bl5625zlxmy"))))
      (build-system go-build-system)
      (arguments
       (list
        #:skip-build? #t
        #:import-path "github.com/mufti1/interconv"))
      (home-page "https://github.com/mufti1/interconv")
      (synopsis "Data type converter")
      (description
       "InterConv converts interfaces into any data type.

Data type that can be converted:
@itemize
@item Int
@item Int8
@item Int32
@item Int16
@item Int64
@item Float32
@item Float64
@item Boolean
@item String
@item Uint
@item Uint8
@item Uint16
@item Uint32
@item Uint64
@item Uintptr
@end itemize")
      (license license:expat))))

(define-public go-github-com-muhlemmer-gu
  (package
    (name "go-github-com-muhlemmer-gu")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/muhlemmer/gu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cgcwl4kv03xxhamngjadqr1yff1x4v7v3s860mmskcq6rxds7ng"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/muhlemmer/gu"))
    (home-page "https://github.com/muhlemmer/gu")
    (synopsis "Generic utilities for Golang")
    (description
     "Package gu provides Generic Utilities for the Go programming language
with low in complexity.")
    (license license:unlicense)))

(define-public go-github-com-multiformats-go-base32
  (package
    (name "go-github-com-multiformats-go-base32")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-base32")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ala6gaa5r5mqcg6cdwfg492hpz41cjbfwyn1ljd6qvya3n0qqiv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-base32"))
    (home-page "https://github.com/multiformats/go-base32")
    (synopsis "Go @code{base32} encoding package with @code{NoPadding} option")
    (description
     "@code{base32} encoding package from Go with @code{NoPadding} option")
    (license license:bsd-3)))

(define-public go-github-com-multiformats-go-base36
  (package
    (name "go-github-com-multiformats-go-base36")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-base36")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wfhsmxkvm97pglfwgiw3ad5g9vqc9nhd61i0kyvsb9lc006g8qq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-base36"))
    (home-page "https://github.com/multiformats/go-base36")
    (synopsis "Optimized @code{base36} codec for Go")
    (description
     "Optimized codec for @code{[]byte} <=> @code{base36} string conversion.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-multiformats-go-multibase
  (package
    (name "go-github-com-multiformats-go-multibase")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-multibase")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11za5yqiq9bkxfg0lvjzgr5d0kawkf2szxj90by9vfnalihqgkrr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-multibase"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-multibase-specs
            (lambda* (#:key import-path #:allow-other-keys)
              (copy-recursively
               (string-append #$(this-package-native-input
                                 "specification-multibase")
                              "/share/multibase/")
               (string-append "src/" import-path "/spec")))))))
    (native-inputs
     (list specification-multibase))
    (propagated-inputs
     (list go-github-com-mr-tron-base58
           go-github-com-multiformats-go-base32
           go-github-com-multiformats-go-base36))
    (home-page "https://github.com/multiformats/go-multibase")
    (synopsis "Implementation of multibase parser in Go")
    (description
     "Implementation of @url{https://github.com/multiformats/multibase,
multibase} (self identifying base encodings) in Go.")
    (license license:expat)))

(define-public go-github-com-multiformats-go-multicodec
  (package
    (name "go-github-com-multiformats-go-multicodec")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-multicodec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vyc85aa9k644l9m3safiz7kk8mm84jclridsp0qnxfj2kcqgipd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-multicodec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-multibase-specs
            (lambda* (#:key import-path #:allow-other-keys)
              (copy-recursively
               (string-append #$(this-package-native-input
                                 "specification-multicodec")
                              "/share/multicodec/")
               (string-append "src/" import-path "/spec/multicodec/")))))))
    (native-inputs
     (list specification-multicodec))
    (home-page "https://github.com/multiformats/go-multicodec")
    (synopsis "Golang constants for the multicodec table")
    (description
     "Package multicodec exposes the multicodec table as Go constants.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-multiformats-go-varint
  (package
    (name "go-github-com-multiformats-go-varint")
    (version "0.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-varint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l4s0z3rc3d350zp6qximl1jjhic6l8w74wkmx244jgfzsxd93af"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-varint"))
    (home-page "https://github.com/multiformats/go-varint")
    (synopsis "Varint helpers that enforce minimal encoding")
    (description
     "This package provides a functionality for encoding and decoding unsigned
varints.")
    (license license:expat)))

(define-public go-github-com-mxk-go-flowrate
  (package
    (name "go-github-com-mxk-go-flowrate")
    (version "0.0.0-20140419014527-cca7078d478f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mxk/go-flowrate")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zqs39923ja0yypdmiqk6x8pgmfs3ms5x5sl1dqv9z6zyx2xy541"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/mxk/go-flowrate"))
    (home-page "https://github.com/mxk/go-flowrate")
    (synopsis "Limiting and monitoring data flow rate in Golang")
    (description
     "This package provides the tools for monitoring and limiting the flow
rate of an arbitrary data stream.")
    (license license:bsd-3)))

(define-public go-github-com-nakagami-firebirdsql
  (package
    (name "go-github-com-nakagami-firebirdsql")
    (version "0.9.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nakagami/firebirdsql")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "043wc8pigv0cpnzljry1vfdnwlmbfy14b5yhbkyhnndk81c4pl7s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; tests require running database service
      #:import-path "github.com/nakagami/firebirdsql"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-kardianos-osext
           go-github-com-shopspring-decimal
           go-gitlab-com-nyarla-go-crypt
           go-golang-org-x-crypto
           go-golang-org-x-text
           go-modernc-org-mathutil))
    (home-page "https://github.com/nakagami/firebirdsql")
    (synopsis "Firebird RDBMS sql driver for Golang")
    (description
     "Package firebird provides database/sql driver for
@url{https://firebirdsql.org/, Firebird} RDBMS.")
    (license license:expat)))

(define-public go-github-com-natefinch-atomic
  (package
    (name "go-github-com-natefinch-atomic")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/natefinch/atomic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y1hr9smjfwp3zgn8s9njp84x9m42x3a7f1h2q7qyd0i5hf9bcvx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/natefinch/atomic"))
    (home-page "https://github.com/natefinch/atomic")
    (synopsis "Atomic file writing in Golang")
    (description
     "This package provides functions to atomically change files, by writing
first to a temp file, and then overwriting the target file in an atomic way.")
    (license license:expat)))

(define-public go-github-com-nathan-osman-go-sunrise
  (package
    (name "go-github-com-nathan-osman-go-sunrise")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nathan-osman/go-sunrise")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "017zwzx05r5spxcs07dp6bnh7waknzsd819k7aqd8kr819v3x9in"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/nathan-osman/go-sunrise"))
    (home-page "https://github.com/nathan-osman/go-sunrise")
    (synopsis "Calculate sunrise and sunset times in Go")
    (description
     "This package provides a Go library for calculating sunrise and
sunset times from geographical coordinates and a date.")
    (license license:expat)))

(define-public go-github-com-nats-io-nats-go
  (package
    (name "go-github-com-nats-io-nats-go")
    (version "1.39.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nats-io/nats.go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "122y7n1xridlpy8048z0p7bv8192pc5yp9js1sspayi9rrn27z6i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nats-io/nats.go"
      ;; Most tests in "test" directory fail to set up, limit to project's
      ;; root tests.
      #:test-subdirs #~(list ".")))
    (native-inputs
     (list go-github-com-nats-io-jwt-v2))
    (propagated-inputs
     (list go-github-com-klauspost-compress
           go-github-com-nats-io-nkeys
           go-github-com-nats-io-nuid
           go-golang-org-x-text
           go-google-golang-org-protobuf)) ; for encoders
    (home-page "https://github.com/nats-io/nats.go")
    (synopsis "Go Client for NATS server")
    (description
     "This package provides a Go client for the NATS messaging system.")
    (license license:asl2.0)))

(define-public go-github-com-nats-io-nuid
  (package
    (name "go-github-com-nats-io-nuid")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nats-io/nuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11zbhg4kds5idsya04bwz4plj0mmiigypzppzih731ppbk2ms1zg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nats-io/nuid"))
    (home-page "https://github.com/nats-io/nuid")
    (synopsis "Go library implementing identifier generator for NATS ecosystem")
    (description
     "This package provides a unique identifier generator that is high performance,
very fast, and tries to be entropy pool friendly.")
    (license license:asl2.0)))

(define-public go-github-com-ncruces-go-strftime
  (package
    (name "go-github-com-ncruces-go-strftime")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncruces/go-strftime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rmr44m8mj5w9j1sy4c24b3n55lx2gwz1z3lb2g3p4qw87wv0j2g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncruces/go-strftime"))
    (home-page "https://github.com/ncruces/go-strftime")
    (synopsis "Implementation of strftime/strptime for Golang")
    (description
     "Package strftime provides strftime/strptime compatible time formatting
and parsing.")
    (license license:expat)))

(define-public go-github-com-netflix-go-expect
  (package
    (name "go-github-com-netflix-go-expect")
    (version "0.0.0-20220104043353-73e0943537d2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Netflix/go-expect")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zkvhnc4ii6ygvcsj54ng0kql26rnny7l3hy1w61g88mxjsww1b9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Netflix/go-expect"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-creack-pty))
    (home-page "https://github.com/Netflix/go-expect")
    (synopsis "Control applications with an Expect-like interface in Go")
    (description
     "Package expect provides an expect-like interface to automate control of
applications.  It is unlike expect in that it does not spawn or manage process
lifecycle.  This package only focuses on expecting output and sending input
through it's psuedoterminal.")
    (license license:asl2.0)))

(define-public go-github-com-neurosnap-sentences
  (package
    (name "go-github-com-neurosnap-sentences")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neurosnap/sentences")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qkq635x54mqzydxmifh2l0kicacgqcbkw4vli1cnwwcs0x902f2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/neurosnap/sentences"))
    (home-page "https://github.com/neurosnap/sentences")
    (synopsis "Multilingual command line sentence tokenizer in Golang")
    (description
     "This package provides functionality of converting a blob of text into a
list of sentences.")
    (license license:expat)))

(define-public go-github-com-nicksnyder-go-i18n-v2
  (package
    (name "go-github-com-nicksnyder-go-i18n-v2")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/nicksnyder/go-i18n")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14v4v06j30rsn2y8lss7j5qyfbr7wir7yzpjd8wqh279aq489d2j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/nicksnyder/go-i18n/v2"
      ;; One test fails with error: open active.en.toml: permission denied.
      #:test-flags #~(list "-skip" "TestMain/extract")))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-golang-org-x-text
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/nicksnyder/go-i18n")
    (synopsis "Internationalization and localization for Golang")
    (description
     "This package provides support for implementing internationalization and
localization of Go code, covering pluralized strings for all 200+ languages in
@acronym{CLDR, Unicode Common Locale Data Repository}.")
    (license license:expat)))

(define-public go-github-com-nightlyone-lockfile
  (package
    (name "go-github-com-nightlyone-lockfile")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/nightlyone/lockfile")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "092r6gv27khplc3srg2ai7214hnvpms6klnsl66x49mspq10b7l5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nightlyone/lockfile"))
    (home-page "https://github.com/nightlyone/lockfile")
    (synopsis " Handle locking via pid files ")
    (description
     "Package lockfile handles pid file based locking.  While a
@code{sync.Mutex} helps against concurrency issues within a single process,
this package is designed to help against concurrency issues between
cooperating processes or serializing multiple invocations of the same
process.")
    (license license:expat)))

(define-public go-github-com-niklasfasching-go-org
  (package
    (name "go-github-com-niklasfasching-go-org")
    (version "1.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/niklasfasching/go-org")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "154c7vx5fmww1wf5hvmijw8a0sn1inm6j54vl9qdybd427xmrhjm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list ".*\\.xml")
      #:import-path "github.com/niklasfasching/go-org"
      ;; PublicDir hashes do not match for testdata/public/about.html:
      ;; 'ce7dde7ff21d19e562c00553cfae3b2d' ->
      ;; 'ac8849c0b32ce36c5cf0843bceb3a167'
      #:test-flags #~(list "-skip" "TestBlorg")))
    (propagated-inputs
     (list go-golang-org-x-net
           go-github-com-pmezard-go-difflib
           go-github-com-alecthomas-chroma-v2))
    (home-page "https://github.com/niklasfasching/go-org")
    (synopsis "Org mode parser and render for Golang")
    (description
     "This package provides a library and CLI program to parse the
@code{org-mode} file format alongside a static site generator with HTML &
pretty printed rendering in Golang.")
    (license license:expat)))

(define-public go-github-com-nlpodyssey-spago
  (package
    (name "go-github-com-nlpodyssey-spago")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nlpodyssey/spago")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vxc7370a1ssb2p25xmrgxkg3jdrl6srsg3w8x7qiacgfdasn5cn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:skip-build? #t
      #:import-path "github.com/nlpodyssey/spago"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-flatbuffers))
    (home-page "https://github.com/nlpodyssey/spago")
    (synopsis "ML/NLP library in Golang")
    (description
     "This package provides is a Machine Learning library written in pure Go
designed to support relevant neural architectures in Natural Language
Processing.

It is self-contained, in that it uses its own lightweight computational
graph both for training and inference, easy to understand from start to
finish.

It provides:
@itemize
@item automatic differentiation via dynamic define-by-run execution
@item feed-forward layers (Linear, Highway, Convolution...)
@item recurrent layers (LSTM, GRU, BiLSTM...)
@item attention layers (Self-Attention, Multi-Head Attention...)
@item gradient descent optimizers (Adam, RAdam, RMS-Prop, AdaGrad, SGD)
@item gob compatible neural models for serialization
@end itemize")
    (license license:bsd-2)))

(define-public go-github-com-nsf-termbox-go
  (package
    (name "go-github-com-nsf-termbox-go")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nsf/termbox-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n5jwnx53nkjvq8rcqzv2scs532iq9w06pd83w6cipniccqp4m2i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nsf/termbox-go"))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/nsf/termbox-go")
    (synopsis "@code{termbox} provides a minimal API for text-based user interfaces")
    (description
     "Termbox is a library that provides a minimalistic API which allows the
programmer to write text-based user interfaces.")
    (license license:expat)))

(define-public go-github-com-nsqio-go-diskqueue
  (package
    (name "go-github-com-nsqio-go-diskqueue")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nsqio/go-diskqueue")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hp66hkmfn0nyf3c53a40f94ah11a9rj01r5zp3jph9p54j8rany"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nsqio/go-diskqueue"))
    (home-page "https://github.com/nsqio/go-diskqueue")
    (synopsis "Go package providing a file system backed FIFO queue")
    (description
     "The @code{diskqueue} Go package provides a file system backed FIFO
queue.")
    (license license:expat)))

(define-public go-github-com-nsqio-go-nsq
  (package
    (name "go-github-com-nsqio-go-nsq")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nsqio/go-nsq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h9z3z225sdgg7fl3l7x11xn5ch6lm5flgmcj046cdp453qj2qhf"))))
    (build-system go-build-system)
    (arguments
     (list #:tests? #f                  ;tests require networking
           #:import-path "github.com/nsqio/go-nsq"))
    (propagated-inputs (list go-github-com-golang-snappy))
    (home-page "https://github.com/nsqio/go-nsq")
    (synopsis "Consumer/producer library for NSQ")
    (description
     "The @code{nsq} Go module provides a high-level @code{Consumer} and
@code{Producer} types as well as low-level functions to communicate over the
NSQ protocol @url{https://nsq.io/}.")
    (license license:expat)))

(define-public go-github-com-nvveen-gotty
  (package
    (name "go-github-com-nvveen-gotty")
    (version "0.0.0-20120604004816-cd527374f1e5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Nvveen/Gotty")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ylvr1p6p036ns3g3wdz8f92f69symshkc8j54fa6gpg4hyk0k6q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f       ;no tests provided
      #:import-path "github.com/Nvveen/Gotty"))
    (home-page "https://github.com/Nvveen/Gotty")
    (synopsis "Interpretation and loading of Termcap database files")
    (description
     "This package implements a functionality for reading and parsing the
@url{https://en.wikipedia.org/wiki/Termcap, terminfo database}.")
    ;; Usage of this source code is governed by a BSD-style license that can be
    ;; found in the LICENSE file.
    (license license:bsd-0)))

(define-public go-github-com-nwidger-jsoncolor
  (package
    (name "go-github-com-nwidger-jsoncolor")
    (version "0.3.2")
    (home-page "https://github.com/nwidger/jsoncolor")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rxb6r40c9jjbvzwfnwc3la53acnizkdsgca8p512s4by180zg49"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/nwidger/jsoncolor"))
    (native-inputs
     (list go-github-com-fatih-color))
    (synopsis "Colorized JSON marshalling and encoding")
    (description
     "@code{jsoncolor} is a drop-in replacement for @code{encoding/json}'s
@code{Marshal} and @code{MarshalIndent} functions and @code{Encoder} type
which produce colorized output using github.com/fatih/color.")
    (license license:expat)))

(define-public go-github-com-nxadm-tail
  (package
    (name "go-github-com-nxadm-tail")
    (version "1.4.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/nxadm/tail")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s8lawq661g8qqf7c1cip5l60cm2138b125jgmv9h548ji9g5yqx"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nxadm/tail"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (propagated-inputs
     (list go-github-com-fsnotify-fsnotify
           go-gopkg-in-tomb-v1))
    (home-page "https://github.com/nxadm/tail")
    (synopsis "Go implementation of the functionality of @command{tail -f}")
    (description
     "This package provides a Go library for reading from continuously
updating files, like @command{tail -f}.")
    (license license:expat)))

(define-public go-github-com-oklog-run
  (package
    (name "go-github-com-oklog-run")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oklog/run")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r55p3kgdkgw55i33lqvvvl60mjp92mhd1170m980sw98z9150jk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/oklog/run"))
    (home-page "https://github.com/oklog/run")
    (synopsis "Universal mechanism to manage goroutine lifecycles")
    (description
     "@code{run.Group} is a universal mechanism to manage goroutine
lifecycles, written to manage component lifecycles in @code{func main} for OK
Log.  It's useful in any circumstance where you need to orchestrate multiple
goroutines as a unit whole.")
    (license license:asl2.0)))

(define-public go-github-com-oklog-ulid
  (package
    (name "go-github-com-oklog-ulid")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oklog/ulid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hybwyid820n80axrk863k2py93hbqlq6hxhf84ppmz0qd0ys0gq"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/oklog/ulid"))
    (native-inputs
     (list go-github-com-pborman-getopt-v2)) ; for CLI
    (home-page "https://github.com/oklog/ulid")
    (synopsis "Universally Unique Lexicographically Sortable Identifier in Golang")
    (description
     "This package implements @acronym{ULID, Universally Unique
Lexicographically Sortable Identifier} as specified in
@url{https://github.com/ulid/spec}.

Features of ULID:
@itemize
@item 128-bit compatibility with UUID
@item 1.21e+24 unique ULIDs per millisecond
@item lexicographically sortable
@item canonically encoded as a 26 character string, as opposed to the 36
character UUID
@item uses Crockford's base32 for better efficiency and readability (5 bits
per character)
@item case insensitive
@item no special characters (URL safe)
@item monotonic sort order (correctly detects and handles the same
millisecond)
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-oklog-ulid-v2
  (package
    (inherit go-github-com-oklog-ulid)
    (name "go-github-com-oklog-ulid-v2")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oklog/ulid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pxjrg48zrmzzdjpsz7b2d56x1vwix2wywgbbv3sdi5mqf0hz17y"))))
    (arguments
     (list
      #:import-path "github.com/oklog/ulid/v2"))))

(define-public go-github-com-olekukonko-errors
  (package
    (name "go-github-com-olekukonko-errors")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olekukonko/errors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12wb02aa4pmd1dl34jczcf12v0bv7kh6qfp8zqmw0hsnh6kdp3i4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/olekukonko/errors"))
    (home-page "https://github.com/olekukonko/errors")
    (synopsis "Enhanced Error Handling for Golang")
    (description
     "Package errors provides a robust error handling library with support for
error wrapping, stack traces, context storage, and retry mechanisms.  It
extends the standard library's error interface with features like HTTP-like
status codes, error categorization, and JSON serialization, while maintaining
compatibility with `errors.Is`, `errors.As`, and `errors.Unwrap`.  The package
is thread-safe and optimized with object pooling for performance.")
    (license license:expat)))

(define-public go-github-com-olekukonko-ll
  (package
    (name "go-github-com-olekukonko-ll")
    (version "0.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olekukonko/ll")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c3f0vqg5fpyqmz86xlgf8sjv9jgbxc3i9ackmdk9xnhjx9mxac9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/olekukonko/ll"))
    (home-page "https://github.com/olekukonko/ll")
    (synopsis "Structured Logging Library for Golang")
    (description
     "@code{ll} is a high-performance, production-ready logging library for Go,
designed to provide @strong{hierarchical namespaces}, @strong{structured
logging}, @strong{middleware pipelines}, @strong{conditional logging}, and
support for multiple output formats, including text, JSON, colorized logs, and
compatibility with Go’s @code{slog}.  It’s ideal for applications requiring
fine-grained log control, extensibility, and scalability.")
    (license license:expat)))

(define-public go-github-com-olekukonko-tablewriter
  (package
    (name "go-github-com-olekukonko-tablewriter")
    (version "1.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olekukonko/tablewriter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13qkanznaq2wadb24rskf0p76wkd84qbxd5lavysnq57ip4pm0fd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/olekukonko/tablewriter"
      ;; XXX: wrap_test.go:38:12: non-constant format string in call to
      ;; (*testing.common).Errorf
      #:test-flags #~(list "-vet=off")))
    (native-inputs
     (list go-github-com-olekukonko-ts)) ; for CLI <cmd/csv2table>
    (propagated-inputs
     (list go-github-com-fatih-color
           go-github-com-mattn-go-runewidth
           go-github-com-olekukonko-errors
           go-github-com-olekukonko-ll))
    (home-page "https://github.com/olekukonko/tablewriter/")
    (synopsis "Generate ASCII table")
    (description "This package generates ASCII tables.  Features:
@itemize
@item automatic Padding
@item support Multiple Lines
@item supports Alignment
@item support Custom Separators
@item automatic Alignment of numbers and percentage
@item write directly to http , file etc via @code{io.Writer}
@item read directly from CSV file
@item optional row line via @code{SetRowLine}
@item normalise table header
@item make CSV Headers optional
@item enable or disable table border
@item set custom footer support
@item optional identical cells merging
@item set custom caption
@item optional reflowing of paragrpahs in multi-line cells
@end itemize")
    (license license:expat)))

(define-public go-github-com-olekukonko-tablewriter-0.0.5
  ;; XXX: Pinned older version as upstream did not migrated to the latest one,
  ;; remove it when no longer required.
  (hidden-package (package (inherit go-github-com-olekukonko-tablewriter)
    (name "go-github-com-olekukonko-tablewriter")
    (version "0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olekukonko/tablewriter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zhnrih2px6jm8nxzkz8s7va3lj03bzwxim8wjba9zh7i78bp67z")))))))

(define-public go-github-com-olekukonko-ts
  (package
    (name "go-github-com-olekukonko-ts")
    (version "0.0.0-20171002115256-78ecb04241c0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olekukonko/ts")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k88n5rvs5k5zalbfa7c71jkjb8dhpk83s425z728qn6aq49c978"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f  ; inappropriate ioctl for device.
      #:import-path "github.com/olekukonko/ts"))
    (home-page "https://github.com/olekukonko/ts/")
    (synopsis "Simple Go application to get the size of the terminal")
    (description
     "This package provides a simple Go application to get the size of the
terminal.")
    (license license:expat)))

(define-public go-github-com-op-go-logging
  (package
    (name "go-github-com-op-go-logging")
    (version "1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/op/go-logging")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01a6lkpj5p82gplddh55az194s9y3014p4j8x4zc8yv886z9c8gn"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ; ERROR: incorrect callpath: String.rec...a.b.c.Info.
       #:import-path "github.com/op/go-logging"))
    (home-page "https://github.com/op/go-logging")
    (synopsis "Go logging library")
    (description
     "Go-Logging implements a logging infrastructure for Go.  Its
output format is customizable and supports different logging backends like
syslog, file and memory.  Multiple backends can be utilized with different log
levels per backend and logger.")
    (license license:bsd-3)))

(define-public go-github-com-opencontainers-cgroups
  (package
    (name "go-github-com-opencontainers-cgroups")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/opencontainers/cgroups")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wwfknbj5zj9y07sdbzqg919ddz39xryp3n5vn94cn2zv3c1kwvr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/opencontainers/cgroups"
      #:test-flags
      ;; Tests requiring root access to /sys/fs/cgroup.
      #~(list "-skip" (string-join
                       (list "TestParseCgroups"
                             "TestHugetlbStatsNoUsageFile"
                             "TestHugetlbStatsNoMaxUsageFile"
                             "TestHugetlbStatsBadUsageFile"
                             "TestHugetlbStatsBadMaxUsageFile"
                             "TestInvalidCgroupPath"
                             "TestTryDefaultCgroupRoot"
                             "TestNilResources")
                       "|"))))
    (propagated-inputs
     (list go-github-com-cilium-ebpf
           go-github-com-coreos-go-systemd-v22
           go-github-com-cyphar-filepath-securejoin
           go-github-com-godbus-dbus-v5
           go-github-com-moby-sys-mountinfo
           go-github-com-moby-sys-userns
           go-github-com-sirupsen-logrus
           go-golang-org-x-sys))
    (home-page "https://github.com/opencontainers/cgroups")
    (synopsis "OCI Project Template")
    (description
     "This package provides a useful boilerplate and organizational
information for all OCI projects.")
    (license license:asl2.0)))

;; For runc@1.3.0, remove when a fresh version is released.
(define-public go-github-com-opencontainers-cgroups-0.0.1
  (hidden-package
   (package
     (inherit go-github-com-opencontainers-cgroups)
     (name "go-github-com-opencontainers-cgroups")
     (version "0.0.1")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
           (url "https://github.com/opencontainers/cgroups")
           (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "173izkgmh33chamkn099gkvz914k726hbv8xhf0xl1rcrv335sa5")))))))

(define-public go-github-com-opencontainers-go-digest
  (package
    (name "go-github-com-opencontainers-go-digest")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opencontainers/go-digest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i5acjajvr6hi9zb7gxwifd8w28y884cv7cx36adj8lngj647xbi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/opencontainers/go-digest"))
    (home-page "https://github.com/opencontainers/go-digest")
    (synopsis "Common digest package used across the container ecosystem")
    (description
     "Package digest provides a generalized type to opaquely represent message
digests and their operations within the registry.  The Digest type is designed
to serve as a flexible identifier in a content-addressable system.  More
importantly, it provides tools and wrappers to work with hash.Hash-based
digests with little effort.")
    (license (list license:asl2.0 license:cc-by-sa4.0))))

(define-public go-github-com-opencontainers-image-spec
  (package
    (name "go-github-com-opencontainers-image-spec")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opencontainers/image-spec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "108jws0i4kbawcihprj3wxp3yqv7nrynkwzwmbz42sx8dmbfq0kc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/opencontainers/image-spec"))
    (propagated-inputs
     (list go-github-com-opencontainers-go-digest
           go-github-com-russross-blackfriday
           go-github-com-xeipuuv-gojsonreference
           go-github-com-xeipuuv-gojsonschema))
    (home-page "https://github.com/opencontainers/image-spec")
    (synopsis "OCI Image Format Specification")
    (description
     "The OCI Image Format project creates and maintains the software shipping
container image format spec (OCI Image Format).")
    (license license:asl2.0)))

;; For umoci@1.5.0, remove when a fresh version is released.
(define-public go-github-com-opencontainers-image-spec-1.0.2
  (hidden-package
   (package
     (inherit go-github-com-opencontainers-image-spec)
     (name "go-github-com-opencontainers-image-spec")
     (version "1.0.2")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
           (url "https://github.com/opencontainers/image-spec")
           (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wcw1z39wjx338406ga86a41f5ird0yc4ab3c70nfhkpkvjjzhkm"))))
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "github.com/opencontainers/image-spec"))
     (propagated-inputs
      (list go-github-com-pkg-errors)))))

(define-public go-github-com-opencontainers-runc
  (package
    (name "go-github-com-opencontainers-runc")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/opencontainers/runc")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0midvxwmj4fvhy5mqv616bhlx39j0gd6y890adx7dnz5in506ym1"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/opencontainers/runc"
      ;; Most tests require additinoal set up and downloading images from
      ;; Internet.
      #:test-subdirs #~(list "."
                             "libcontainer/capabilities"
                             "libcontainer/devices"
                             "libcontainer/intelrdt"
                             "libcontainer/internal/userns"
                             "libcontainer/logs"
                             "libcontainer/nsenter/test"
                             "libcontainer/specconv"
                             "libcontainer/system"
                             "libcontainer/system/kernelversion"
                             "libcontainer/utils")))
    (propagated-inputs
     (list go-github-com-checkpoint-restore-go-criu-v6
           go-github-com-containerd-console
           go-github-com-coreos-go-systemd-v22
           go-github-com-cyphar-filepath-securejoin
           go-github-com-docker-go-units
           go-github-com-godbus-dbus-v5
           go-github-com-moby-sys-capability
           go-github-com-moby-sys-mountinfo
           go-github-com-moby-sys-user
           go-github-com-moby-sys-userns
           go-github-com-mrunalp-fileutils
           go-github-com-opencontainers-cgroups
           go-github-com-opencontainers-runtime-spec
           go-github-com-opencontainers-selinux
           go-github-com-seccomp-libseccomp-golang
           go-github-com-sirupsen-logrus
           go-github-com-urfave-cli
           go-github-com-vishvananda-netlink
           go-golang-org-x-net
           go-golang-org-x-sys
           go-google-golang-org-protobuf))
    (home-page "https://github.com/opencontainers/runc")
    (synopsis "Tool for running containers according to the OCI specification")
    (description
     "@code{runc} is a CLI tool and library for spawning and running
containers on Linux according to the OCI specification.")
    (license license:asl2.0)))

;; XXX: Find a way to source from specification-runtime-spec.
(define-public go-github-com-opencontainers-runtime-spec
  (package
    (name "go-github-com-opencontainers-runtime-spec")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opencontainers/runtime-spec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bh2awpdmbjh65js45vidm3s2pf42jykgad5wb772pysd7qnphbv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/opencontainers/runtime-spec"
      #:skip-build? #t))
    (native-inputs
     (list go-github-com-xeipuuv-gojsonschema))
    (home-page "https://github.com/opencontainers/runtime-spec")
    (synopsis "OCI specs implementation in Golang")
    (description
     "This package provides a collection Golang implementation defined in
specification-runtime-spec.")
    (license license:asl2.0)))

(define-public go-github-com-opencontainers-runtime-tools
  (package
    (name "go-github-com-opencontainers-runtime-tools")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/opencontainers/runtime-tools")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pli3jb1rq9lkzzz83f7jw788vijg7x6ly3vgasdlwri7kiph1sa"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      ;; XXX: See: <https://github.com/opencontainers/runtime-tools/issues/792>.
      #:tests? #f
      #:import-path "github.com/opencontainers/runtime-tools"
      #:build-flags
      #~(list (format #f "-ldflags=-X ~s"
                      (string-append "main.version=" #$version)))
      #:test-flags
      #~(list "-vet=off"
              ;; Network is required.
              "-skip" "TestGenerateValid|TestJSONSchema")))
    (native-inputs
     (list go-github-com-mndrix-tap-go
           go-github-com-stretchr-testify
           go-github-com-urfave-cli))
    (propagated-inputs
     (list go-github-com-blang-semver
           go-github-com-hashicorp-go-multierror
           go-github-com-mrunalp-fileutils
           go-github-com-opencontainers-runtime-spec
           go-github-com-opencontainers-selinux
           go-github-com-satori-go-uuid
           go-github-com-sirupsen-logrus
           go-github-com-syndtr-gocapability
           go-github-com-xeipuuv-gojsonschema
           go-golang-org-x-sys))
    (home-page "https://github.com/opencontainers/runtime-tools")
    (synopsis "OCI Runtime Tools")
    (description
     "This package provides a collection of tools for working with the
@url{https://github.com/opencontainers/runtime-spec, OCI runtime
specification}.  To build from source code, runtime-tools requires Go 1.10.x
or above.")
    (license license:asl2.0)))

(define-public go-github-com-opencontainers-selinux
  (package
    (name "go-github-com-opencontainers-selinux")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opencontainers/selinux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19j92lj9037d94lsls97b9d6cr0y8qavi8rampyawlykcp7gknqz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/opencontainers/selinux"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/opencontainers/selinux")
    (synopsis "Common SELinux implementation")
    (description
     "Common SELinux package used across the container ecosystem.")
    (license license:asl2.0)))

(define-public go-github-com-opencontainers-umoci
  (package
    (name "go-github-com-opencontainers-umoci")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/opencontainers/umoci")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10pxiqk4194nbnvlvfvlbk31wp8k35in3g694y20f9261nn0qx6n"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/opencontainers/umoci"
      ;; convert spec to rootless: inspecting mount flags of /etc/resolv.conf:
      ;; no such file or directory
      #:test-flags
      #~(list "-skip" "TestUnpackManifestCustomLayer|TestUnpackStartFromDescriptor")
      #:test-subdirs
      ;; cmd/umoci needs older version of image-spec, excluding it.
      #~(list "." "mutate"
              "oci/cas/dir"
              "oci/casext/blobcompress"
              "oci/config/generate"
              "pkg/funchelpers"
              "pkg/hardening"
              "pkg/idtools"
              "pkg/mtreefilter"
              "pkg/pathtrie"
              "pkg/system"
              "pkg/unpriv")))
    (native-inputs
     (list go-github-com-mohae-deepcopy
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-adalogics-go-fuzz-headers
           go-github-com-apex-log
           go-github-com-blang-semver-v4
           go-github-com-cyphar-filepath-securejoin
           go-github-com-docker-go-units
           go-github-com-klauspost-compress
           go-github-com-klauspost-pgzip
           go-github-com-moby-sys-user
           go-github-com-moby-sys-userns
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-runtime-spec
           go-github-com-rootless-containers-proto-go-proto
           go-github-com-urfave-cli
           go-github-com-vbatts-go-mtree
           go-golang-org-x-sys
           go-google-golang-org-protobuf))
    (home-page "https://umo.ci/")
    (synopsis "Modifies Open Container images")
    (description
     "umoci modifies Open Container images (pronounced /uːmoˈʨi/) is a
reference implementation of the @url{OCI image specification,
https://github.com/opencontainers/image-spec} and provides users with the
ability to create, manipulate, and otherwise interact with container images.
It is designed to be as small and unopinonated as possible, so as to act as a
foundation for larger systems to be built on top of.")
    (license license:asl2.0)))

(define-public go-github-com-openhistogram-circonusllhist
  (package
    (name "go-github-com-openhistogram-circonusllhist")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openhistogram/circonusllhist")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ngngjzl3nd30csqvp0gp2qa1hsskl3rh4fgslygvf4p34s42jxk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/openhistogram/circonusllhist"))
    (home-page "https://github.com/circonus-labs/circonusllhist")
    (synopsis "OpenHistogram log-linear histograms in Golang")
    (description
     "Package circllhist provides an implementation of Circonus fixed
log-linear histogram data structure.  This allows tracking of histograms in a
composable way such that accurate error can be reasoned about.")
    (license license:bsd-3)))

(define-public go-github-com-openprinting-goipp
  (package
    (name "go-github-com-openprinting-goipp")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenPrinting/goipp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p05dk37l393byvjanvi3ipqcax320vf3qynlzazm7czzzlw448h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/OpenPrinting/goipp"))
    (home-page "https://github.com/OpenPrinting/goipp")
    (synopsis "IPP core protocol implementation")
    (description
      "The goipp package implements the IPP core protocol, as defined by
@@url{https://rfc-editor.org/rfc/rfc8010.html,RFC 8010}.")
    (license license:bsd-2)))

(define-public go-github-com-operatorfoundation-monolith-go
  (package
    (name "go-github-com-operatorfoundation-monolith-go")
    (version "1.0.10")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/OperatorFoundation/monolith-go")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zzamnrakjvz9frxscyhrvyz2ikqq2klmynn218jk5dar6mc6xyf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/OperatorFoundation/monolith-go"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestEnumeratedItems"
                             "TestOptional2"
                             "TestVariableStringsPart")
                       "|"))))
    (propagated-inputs
     (list go-github-com-deckarep-golang-set))
    (home-page "https://github.com/OperatorFoundation/monolith-go")
    (synopsis "Byte sequences library")
    (description
     "Monolith-Go is a Go library for working with byte sequences.")
    (license license:expat)))

;; To build bitmask 0.21.11, remove when it's updated.
(define-public go-github-com-operatorfoundation-monolith-go-1.0.4
  (package
    (inherit go-github-com-operatorfoundation-monolith-go)
    (name "go-github-com-operatorfoundation-monolith-go")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/OperatorFoundation/monolith-go")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "066bqlgw5h7a3kxswqlv734asb7nw2y6snsn09yqk0ixj23qw22s"))))))

(define-public go-github-com-orisano-pixelmatch
  (package
    (name "go-github-com-orisano-pixelmatch")
    (version "0.0.0-20230914042517-fa304d1dc785")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/orisano/pixelmatch")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lplxfif5mfqnd0jjph2vd25c3bpr3idfs2axh8z0ib0zdkwca32"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/orisano/pixelmatch"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/orisano/pixelmatch")
    (synopsis "Pixelmatch port to Go")
    (description
     "This package provides a port of Pixelmatch, a pixel-level image
comparison library, to Go.  Both a library and a command-line tool are
included in this package.")
    (license license:expat)))

(define-public go-github-com-ostreedev-ostree-go
  (package
    (name "go-github-com-ostreedev-ostree-go")
    (version "0.0.0-20210805093236-719684c64e4f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ostreedev/ostree-go")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "039pf8fdjlnma02m5ymj92m3j5w9pr950qkc7977lw2bjmw95g69"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; The final command needs to include libostree and pkg-config packages.
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/ostreedev/ostree-go"))
    (home-page "https://github.com/ostreedev/ostree-go")
    (synopsis "Golang bindings for @code{libostree}")
    (description
     "This packae provides bindings for
@code{https://github.com/ostreedev/ostree, OSTree}.")
    (license license:isc)))

(define-public go-github-com-otiai10-copy
  (package
    (name "go-github-com-otiai10-copy")
    (version "1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/otiai10/copy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fv4cwk4k5fsd3hq5akqxrd5qxj9qm6a2wlp6s1knblhzkm1jxzb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/otiai10/copy"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'make-test-directory-writable
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each make-file-writable (find-files "./test")))))
          (add-after 'check 'remove-test-data
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "./test")))))))
    (native-inputs
     (list go-github-com-otiai10-mint))
    (propagated-inputs
     (list go-golang-org-x-sync go-golang-org-x-sys))
    (home-page "https://github.com/otiai10/copy")
    (synopsis "Go copy directory recursively")
    (description
     "This package implements recursive copy functionality for directory.")
    (license license:expat)))

(define-public go-github-com-outcaste-io-ristretto
  (package
    (name "go-github-com-outcaste-io-ristretto")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/outcaste-io/ristretto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07ndjrblmj9b3ghn10h040zvzf2i3mlzvfziklcqdkhxbr1hf1rq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/outcaste-io/ristretto"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-cespare-xxhash-v2
           go-github-com-dgryski-go-farm
           go-github-com-dustin-go-humanize
           go-github-com-pkg-errors
           go-go-uber-org-atomic
           go-golang-org-x-sys))
    (home-page "https://github.com/outcaste-io/ristretto")
    (synopsis "Memory-bound Go cache")
    (description
     "Ristretto is a fast, fixed size, in-memory cache with a dual focus on
throughput and hit ratio performance.  It's a fork of
@code{dgraph-io/ristretto} project.")
    (license license:asl2.0)))

(define-public go-github-com-patrickmn-go-cache
  (package
    (name "go-github-com-patrickmn-go-cache")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/patrickmn/go-cache")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10020inkzrm931r4bixf8wqr9n39wcrb78vfyxmbvjavvw4zybgs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/patrickmn/go-cache"))
    (home-page "https://github.com/patrickmn/go-cache")
    (synopsis "In-memory key:value store/cache Golang library")
    (description
     "go-cache is an in-memory key:value store/cache similar to Memcached that
is suitable for applications running on a single machine.  Its major advantage
is that, being essentially a thread-safe @code{map[string]interface{}} with
expiration times, it doesn't need to serialize or transmit its contents over
the network.")
    (license license:expat)))

(define-public go-github-com-pbnjay-memory
  (let ((commit "7b4eea64cf580186c0eceb10dc94ba3a098af46c")
        (revision "2"))
    (package
      (name "go-github-com-pbnjay-memory")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pbnjay/memory")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "107w8pd1aasdrk35hh9pbdh9z11s9s79nglz6rqfnf6bhgb8b3s0"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/pbnjay/memory"))
      (home-page "https://github.com/gedex/inflector")
      (synopsis "Go library to report total system memory")
      (description
       "@code{memory} provides a single method reporting total physical system
memory accessible to the kernel.  It does not account for memory used by other
processes.")
      (license license:bsd-3))))

(define-public go-github-com-pborman-getopt
  (package
    (name "go-github-com-pborman-getopt")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pborman/getopt")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sa66n392hzqbahn47grbjyaasvpklnn4s1wkzs1kdwrfdd62kfa"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/pborman/getopt/v2
            (delete-file-recursively "v2")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pborman/getopt"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/pborman/getopt")
    (synopsis "Getopt style option parsing for Go")
    (description
     "This package provides traditional getopt processing for implementing
programs that use traditional command lines.")
    (license license:bsd-3)))

(define-public go-github-com-pborman-getopt-v2
  (package
    (inherit go-github-com-pborman-getopt)
    (name "go-github-com-pborman-getopt-v2")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pborman/getopt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sacv6g8cxfibxd3gnfjnzp7fynrnc4s2aaz5wbxivqqhvflc22l"))))
    (build-system go-build-system)
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-pborman-getopt)
       ((#:import-path _) "github.com/pborman/getopt/v2")
       ((#:unpack-path _ "") "github.com/pborman/getopt")))))

(define-public go-github-com-pborman-uuid
  (package
    (name "go-github-com-pborman-uuid")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pborman/uuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n1ng6396zgm1iggzp43h554wwp32iwr62qqfy0zl4jnk2dg41lv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pborman/uuid"))
    (propagated-inputs
     (list go-github-com-google-uuid))
    (home-page "https://github.com/pborman/uuid")
    (synopsis "Generates and inspects UUIDs")
    (description
     "The uuid package generates and inspects UUIDs based on
@url{http://tools.ietf.org/html/rfc4122, RFC 4122} and DCE 1.1: Authentication
and Security Services.  This package now leverages the github.com/google/uuid
package (which is based off an earlier version of this package).")
    (license license:bsd-3)))

(define-public go-github-com-pelletier-go-toml
  (package
    (name "go-github-com-pelletier-go-toml")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pelletier/go-toml")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wypjrr1axkrkzp4n5gvams94f2sd7dq1pdpd2i35sgpdz6r2m6g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pelletier/go-toml"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-github-com-davecgh-go-spew
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/pelletier/go-toml")
    (synopsis "Go library for the TOML configuration language")
    (description
     "Go library for the TOML configuration language")
    (license license:expat)))

(define-public go-github-com-pelletier-go-toml-v2
  (package
    (inherit go-github-com-pelletier-go-toml)
    (name "go-github-com-pelletier-go-toml-v2")
    (version "2.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pelletier/go-toml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hqxj34d49snvc2m6lxfjxks3z9sic9xbb6w49ajrqbzy953spzs"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Tests hang in CI without any error trace, causing the built to
      ;; fail, find out why.  They pass just fine during local build on AMD
      ;; Ryzen 7 3800X.
      #:tests? #f
      #:import-path "github.com/pelletier/go-toml/v2"
      #:parallel-tests? #f
      #:test-flags
      #~(list "-short"
              "-count" "1"
              "-skip" "FuzzUnmarshal") ; for benchmark
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/benchmark")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs '())))

(define-public go-github-com-petar-gollrb
  (package
    (name "go-github-com-petar-gollrb")
    (version "0.0.0-20210522233825-ae3b015fd3e9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/petar/GoLLRB")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k50v8jk7pfwpghmpyr9gk8kpcxns0d8kw113z9wjcr0x8gnyj0n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/petar/GoLLRB"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "example")))))))
    (home-page "https://github.com/petar/GoLLRB")
    (synopsis "LLRB implementation of balanced binary search trees for Golang")
    (description
     "@code{GoLLRB} is a Left-Leaning Red-Black (LLRB) implementation of 2-3
balanced binary search trees in Go Language.")
    (license license:bsd-3)))

(define-public go-github-com-peterbourgon-diskv
  (package
    (name "go-github-com-peterbourgon-diskv")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/peterbourgon/diskv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pdy8f7bkm65gx4vknwcvfa619hknflqxkdlvmf427k2mzm91gmh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/peterbourgon/diskv"))
    (propagated-inputs (list go-github-com-google-btree))
    (home-page "https://github.com/peterbourgon/diskv")
    (synopsis "Disk-backed key-value store")
    (description
     "Diskv (disk-vee) is a simple, persistent key-value store written in the
Go language.  It starts with a simple API for storing arbitrary data on a
filesystem by key, and builds several layers of performance-enhancing
abstraction on top.  The end result is a conceptually simple, but highly
performant, disk-backed storage system.")
    (license license:expat)))

(define-public go-github-com-peterbourgon-ff-v3
  (package
    (name "go-github-com-peterbourgon-ff-v3")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/peterbourgon/ff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "162qh3mp7xn4qhw7rgigwmg0r52mflwcr07fig5z3k257h0mclar"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/peterbourgon/ff/v3"))
    (propagated-inputs
     (list go-github-com-pelletier-go-toml
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/peterbourgon/ff")
    (synopsis "Flags-first Golang library for configuration")
    (description
     "Package ff is a flags-first helper package for configuring programs.

The basic idea is that @code{myprogram -h} should always show the complete
configuration \"surface area\" of a program.  Therefore, every config
parameter should be defined as a flag.  This module provides a simple and
robust way to define those flags, and to parse them from command-line
arguments, environment variables, and/or config files.")
    (license license:asl2.0)))

(define-public go-github-com-peterh-liner
  (package
    (name "go-github-com-peterh-liner")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/peterh/liner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hq0maja0ymdc0x5f78jv0hxh4i7byxb5y9p70vi9zsip9yhirqp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/peterh/liner"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/peterh/liner")
    (synopsis "Command line editor Go library")
    (description "The @code{liner} Go package implements a simple command line
editor with history, inspired by @url{https://github.com/antirez/linenoise/,
linenoise}.  Xterm as well as WIN32 terminal codes are supported.")
    (license license:expat)))

(define-public go-github-com-phayes-permbits
  (package
    (name "go-github-com-phayes-permbits")
    (version "0.0.0-20190612203442-39d7c581d2ee")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/phayes/permbits")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jixy4m65agyyly5mg4icszwzs5hjgj1x7cwvc9a3df6j5lwd41x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/phayes/permbits"))
    (home-page "https://github.com/phayes/permbits")
    (synopsis "Easy file permissions for Golang")
    (description
     "This package makes it a breeze to check and modify file permission
bits.")
    (license license:expat)))

(define-public go-github-com-philhofer-fwd
  (package
    (name "go-github-com-philhofer-fwd")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/philhofer/fwd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "083zlvna3mz37p91r4h8r7yvjqcvsgr9l5p0zidmk3ajq6gxds1p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/philhofer/fwd"))
    (home-page "https://github.com/philhofer/fwd")
    (synopsis "Buffered Reader/Writer for Goalng")
    (description
     "Package fwd provides a buffered reader and writer.  Each has methods
that help improve the encoding/decoding performance of some binary
protocols.")
    (license license:expat)))

(define-public go-github-com-phpdave11-gofpdi
  (package
    (name "go-github-com-phpdave11-gofpdi")
    (version "1.0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/phpdave11/gofpdi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zan4wmwd1rzrq57ynz4z5kf00b6xj0dnki123zai0j53xdngwhw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/phpdave11/gofpdi"))
    (propagated-inputs (list go-github-com-pkg-errors))
    (home-page "https://github.com/phpdave11/gofpdi")
    (synopsis "PDF document importer")
    (description
     "gofpdi allows you to import an existing PDF into a new PDF.")
    (license license:expat)))

(define-public go-github-com-pierrec-cmdflag
  (package
    (name "go-github-com-pierrec-cmdflag")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pierrec/cmdflag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nxmqkjwd7i3blmspvxib352vm6167h2ffqy4m9zc3fb9srvrpqc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pierrec/cmdflag"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (home-page "https://github.com/pierrec/cmdflag")
    (synopsis "Augment the flag package with commands")
    (description
     "Package @code{cmdflag} provides simple command line commands processing
on top of the standard library @code{flag} package.")
    (license license:bsd-3)))

(define-public go-github-com-pingcap-errors
  (package
    (name "go-github-com-pingcap-errors")
    (version "0.11.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pingcap/errors")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02k6b30m42aya763fnwx3paq4r8h28yav4i2kv2z4r28r70xxcgn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/pingcap/errors"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; XXX: Some parsing issues in these tests.
                       (list "TestFormatErrorf"
                             "TestFormatNew"
                             "TestFormatWithMessage"
                             "TestFormatWithStack"
                             "TestFormatWrap"
                             "TestFormatWrapf"
                             "TestFrameFormat"
                             "TestFrameLine"
                             "TestStackTrace"
                             "TestStackTraceFormat")
                       "|"))))
    (native-inputs
     (list go-github-com-pkg-errors))
    (home-page "https://github.com/pingcap/errors")
    (synopsis "Simple error handling primitives")
    (description
     "Package errors provides simple error handling primitives.  It's an
alternative fork of https://github.com/pkg/errors project.")
    (license license:bsd-2)))

(define-public go-github-com-pion-logging
  (package
    (name "go-github-com-pion-logging")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pion/logging/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13c8pkn6nyhayjax77bcysmv9fsyb63gllk2ns880b3hgdcl2l1a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/logging"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/pion/logging/")
    (synopsis "Logging library for Golang projects")
    (description
     "This package provides a logging library used by @url{https://github.com/pion,
Pion}.")
    (license license:expat)))

(define-public go-github-com-pkg-diff
  (package
    (name "go-github-com-pkg-diff")
    (version "0.0.0-20241224192749-4e6772a4315c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pkg/diff")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iwaa6g3mbbi1k6rw7fn85sg6lm6rlnjz07yb91hq1kll6494s18"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pkg/diff"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/pkg/diff/")
    (synopsis "Create and print diffs")
    (description
     "This package provides a Go library to create and print diffs.")
    (license license:bsd-3)))

(define-public go-github-com-pkg-term
  (package
    (name "go-github-com-pkg-term")
    (version "1.2.0-beta.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pkg/term")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fflh4lglbvdz8949h8spbw3vwdldgnl6zgps4ylzzr40hhyvgf5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pkg/term"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Tests fail with error: inappropriate ioctl for device.
               (list "TestTiocmbic"
                     "TestTiocmbis"
                     "TestTiocmget"
                     "TestTiocmset")
               "|"))))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/pkg/term")
    (synopsis "Manages POSIX terminals from Golang")
    (description
     "Package term manages POSIX terminals.  As POSIX terminals are connected
to, or emulate, a UART, this package also provides control over the various
UART and serial line parameters.")
    (license license:bsd-2)))

(define-public go-github-com-pkg-xattr
  (package
    (name "go-github-com-pkg-xattr")
    (version "0.4.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pkg/xattr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rxvfiwgd0z9alj4n8jympmyi68zh1swf3siim8r2h01nm4i0vqh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pkg/xattr"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/pkg/xattr")
    (synopsis "Support for extended file system attributes")
    (description
     "Package xattr provides support for extended attributes on Linux, Darwin
and FreeBSD.  Extended attributes are name:value pairs permanently associated
with files or directories.  They are similar to the environment strings
associated with a process.  An attribute may be defined or undefined.  If
defined, its value may be empty or non-empty.  You can find more details here:
@@url{https://en.wikipedia.org/wiki/Extended_file_attributes,
https://en.wikipedia.org/wiki/Extended_file_attributes}.")
    (license license:bsd-2)))

(define-public go-github-com-pmezard-go-difflib
  (package
    (name "go-github-com-pmezard-go-difflib")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pmezard/go-difflib")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c1cn55m4rypmscgf0rrb88pn58j3ysvc2d0432dp3c6fqg6cnzw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/pmezard/go-difflib/difflib"
      #:unpack-path "github.com/pmezard/go-difflib/"))
    (home-page "https://github.com/pmezard/go-difflib")
    (synopsis "Go diff implementation")
    (description
     "This package provides unified and context-aware diffs in Go.")
    (license license:bsd-3)))

(define-public go-github-com-polydawn-refmt
  (package
    (name "go-github-com-polydawn-refmt")
    (version "0.89.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/polydawn/refmt/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v4av75nlgvps5q2h3q6w3cmry0gg316l82zmj6sph9bp2c87621"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/polydawn/refmt"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (propagated-inputs
     (list go-github-com-go-yaml-yaml
           go-github-com-smartystreets-goconvey
           go-github-com-urfave-cli
           go-github-com-warpfork-go-wish))
    (home-page "https://github.com/polydawn/refmt/")
    (synopsis "Object mapping for Go language")
    (description
     "@code{refmt} is a serialization and object-mapping library.")
    (license license:expat)))

(define-public go-github-com-posener-complete
  (package
    (name "go-github-com-posener-complete")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/posener/complete")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nri6hkfb0z3dkxf2fsfidr4bxbn91rjsqhg5s0c2jplf0aclppz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/posener/complete"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-hashicorp-go-multierror))
    (home-page "https://github.com/posener/complete")
    (synopsis "Bash completion implemented in Golang")
    (description
     "Package complete provides a tool for bash writing bash completion in go,
and bash completion for the go command line.")
    (license license:expat)))

(define-public go-github-com-prometheus-community-go-runit
  (package
    (name "go-github-com-prometheus-community-go-runit")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus-community/go-runit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f5z10ynf67xx566ahnp5ndwxqz0i8rd8xnjiw7za6pz0wgj6wmk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/prometheus-community/go-runit"))
    (home-page "https://github.com/prometheus-community/go-runit")
    (synopsis "Wrapping runit service status")
    (description
     "This package provides a wrapper round runit service status.")
    (license license:expat)))

(define-public go-github-com-pterm-pterm
  (package
    (name "go-github-com-pterm-pterm")
    (version "0.12.79")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pterm/pterm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xvc4ywc2998r8vsi3zpp49z04kc79q60bsvxv88cjvamxfjxrvk"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Cycle: go-github-com-pterm-pterm -> go-github-com-marvinjwendt-testza
      ;; -> go-github-com-pterm-pterm
      #:tests? #f
      #:import-path "github.com/pterm/pterm"))
    (propagated-inputs
     (list go-atomicgo-dev-cursor
           go-atomicgo-dev-keyboard
           go-atomicgo-dev-schedule
           go-github-com-gookit-color
           go-github-com-lithammer-fuzzysearch
           go-github-com-mattn-go-runewidth
           go-golang-org-x-term
           go-golang-org-x-text))
    (home-page "https://github.com/pterm/pterm")
    (synopsis "Configurable consol outputs in Golang")
    (description
     "Package pterm is a modern go module to beautify console output.  It can be used
without configuration, but if desired, everything can be customized down to the
smallest detail.")
    (license license:expat)))

(define-public go-github-com-rakyll-statik
  (package
    (name "go-github-com-rakyll-statik")
    (version "0.1.7")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/rakyll/statik")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y0kbzma55vmyqhyrw9ssgvxn6nw7d0zg72a7nz8vp1zly4hs6va"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            ;; Fix compatibility with go-1.18+
            (substitute* "statik.go"
              (("fmt\\.Println\\(helpText\\)")
               "fmt.Print(helpText + \"\\n\")"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rakyll/statik"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list
                "TestOpen/Files_should_retain_their_original_file*"
                "TestOpen/Images_should_successfully_unpack"
                "TestOpen/'index.html'_files_should_be_returned*"
                "TestOpen/listed_all_sub_directories_in_deep_directory"
                "TestOpen/Paths_containing_dots_should_be_properly_sanitized")
               "|"))))
    (home-page "https://github.com/rakyll/statik/")
    (synopsis "Embed files into a Go executable")
    (description "Statik allows you to embed a directory of static files into
your Go binary to be later served from an http.FileSystem.")
    (license license:asl2.0)))

(define-public go-github-com-raulk-go-watchdog
  (package
    (name "go-github-com-raulk-go-watchdog")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/raulk/go-watchdog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mpvphqihxmcnz66a3yisk18pm2yxpcg81pnkd80ax80fyvzwl0z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/raulk/go-watchdog"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-benbjohnson-clock
           go-github-com-containerd-cgroups
           go-github-com-elastic-gosigar
           go-github-com-opencontainers-runtime-spec))
    (home-page "https://github.com/raulk/go-watchdog")
    (synopsis "Go memory watchdog")
    (description
     "Package watchdog runs a singleton memory watchdog in the process, which
watches memory utilization and forces Go GC in accordance with a user-defined
policy.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-redis-go-redis-v9
  (package
    (name "go-github-com-redis-go-redis-v9")
    (version "9.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/redis/go-redis")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19scv5fbwacrbpv329w2a48z1w5wmxi7ax93bp1p398k4yqx6izf"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/redis/go-redis/extra/rediscensus/v9
            ;; - github.com/redis/go-redis/extra/rediscmd/v9
            ;; - github.com/redis/go-redis/extra/redisotel/v9
            ;; - github.com/redis/go-redis/extra/redisprometheus/v9
            ;; - github.com/redis/go-redis/internal/customvet
            (delete-file-recursively "extra")
            (delete-file-recursively "internal/customvet")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/redis/go-redis/v9"
      #:test-flags
      ;; Tests requir running Redis server.
      #~(list "-skip" "Example|TestGinkgoSuite")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/example")))))))
    (native-inputs
     (list go-github-com-bsm-ginkgo-v2
           go-github-com-bsm-gomega))
    (propagated-inputs
     (list go-github-com-cespare-xxhash-v2
           go-github-com-dgryski-go-rendezvous))
    (home-page "https://github.com/redis/go-redis")
    (synopsis "Redis client for Golang")
    (description
     "go-redis is the official Redis client library for the Go programming
language.  It offers a straightforward interface for interacting with Redis
servers.")
    (license license:bsd-2)))

(define-public go-github-com-reiver-go-porterstemmer
  ;; The latest commit contain test fixtures.
  (let ((commit "ab0f922907ea0321367a5776bd7a6c35d505d53b")
        (revision "0"))
    (package
      (name "go-github-com-reiver-go-porterstemmer")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/reiver/go-porterstemmer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "10inyn0hdwdp619fk9lamq7cffalq4b201s3m75dfsqrb46az1ph"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/reiver/go-porterstemmer"
        ;; Could not download test file (from web) from URL:
        ;; [http://tartarus.org/martin/PorterStemmer/voc.txt]
        #:test-flags #~(list "-skip" "TestHasSuffix|TestStemString")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-tests
              (lambda* (#:key import-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" import-path)
                  (substitute* "porterstemmer_has_suffix_test.go"
                    ;; (*testing.common).Errorf format %d has arg
                    ;; datum.Expected of wrong type bool
                    (("%d") "%t"))))))))
      (home-page "https://github.com/reiver/go-porterstemmer")
      (synopsis "Clean room implementation of the Porter Stemming algorithm")
      (description
       "This package provides a native Go clean room implementation of the
Porter Stemming Algorithm.")
      (license license:expat))))

(define-public go-github-com-remeh-sizedwaitgroup
  (package
    (name "go-github-com-remeh-sizedwaitgroup")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/remeh/sizedwaitgroup")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xwdzby27xzcghsqhli3il165iz3vkx3g4abgvkl99wysyhcvn0a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/remeh/sizedwaitgroup"))
    (home-page "https://github.com/remeh/sizedwaitgroup")
    (synopsis "Goroutines limit amount implementation of standard @code{sync.WaitGroup}")
    (description
     "This package implements a feature of limiting the maximum number of
concurrently started routines which has the same role and API as
@code{sync.WaitGroup}.  It could for example be used to start multiples
routines querying a database but without sending too much queries in order to
not overload the given database.")
    (license license:expat)))

(define-public go-github-com-rican7-retry
  (package
    (name "go-github-com-rican7-retry")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Rican7/retry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ly1dh83kiq6ngxqk7s2gys0n49qzqldsqq62a7ggqx1z7s3l5a5"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/Rican7/retry/tools
            (delete-file-recursively "tools")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Rican7/retry"))
    (home-page "https://github.com/Rican7/retry")
    (synopsis "Perform actions repetitively until successful")
    (description
     "Package retry provides a simple, stateless, functional mechanism to
perform actions repetitively until successful.")
    (license license:expat)))

(define-public go-github-com-rifflock-lfshook
  (package
    (name "go-github-com-rifflock-lfshook")
    (version "2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rifflock/lfshook")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wxqjcjfg8c0klmdgmbw3ckagby3wg9rkga9ihd4fsf05x5scxrc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rifflock/lfshook"))
    (propagated-inputs
     (list go-github-com-sirupsen-logrus))
    (home-page "https://github.com/rifflock/lfshook")
    (synopsis "Local File System hook for Logrus logger")
    (description
     "This package provides a hook for Logrus to write directly to a file on
the file system.  The log levels are dynamic at instantiation of the hook, so
it is capable of logging at some or all levels.")
    (license license:expat)))

(define-public go-github-com-rivo-tview
  (package
    (name "go-github-com-rivo-tview")
    (version "0.0.0-20241227133733-17b7edb88c57")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rivo/tview")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w59vyvs8k5mzl7k39zpwcwwdgam2pbz0rnrfnnjgvkld1rh81dc"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/rivo/tview"))
    (propagated-inputs
     (list go-github-com-gdamore-tcell-v2
           go-github-com-lucasb-eyer-go-colorful
           go-github-com-rivo-uniseg))
    (home-page "https://github.com/rivo/tview")
    (synopsis "Rich Interactive Widgets for Terminal UIs")
    (description
     "The tview package implements rich widgets for terminal based user
interfaces.  The widgets provided with this package are useful for data
exploration and data entry.")
    (license license:expat)))

(define-public go-github-com-rivo-uniseg
  (package
    (name "go-github-com-rivo-uniseg")
    (version "0.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rivo/uniseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nlcqyvq4vhq3hqhk84h6fp0jbqkjj88kcpcl853yr7sh4sisdxc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rivo/uniseg"))
    (home-page "https://github.com/rivo/uniseg")
    (synopsis "Unicode Text Segmentation for Go")
    (description
     "This package implements Unicode Text Segmentation according to
@url{https://unicode.org/reports/tr29/, Unicode Standard Annex #29}.")
    (license license:expat)))

(define-public go-github-com-riywo-loginshell
  (package
    (name "go-github-com-riywo-loginshell")
    (version "0.0.0-20200815045211-7d26008be1ab")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/riywo/loginshell")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "138yvis6lipw9x02jyiz7472bxi20206bcfikcar54i3xsww9q4i"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/riywo/loginshell"
           ;; Tests try to get the current user's login shell; the build
           ;; user doesn't have one.
           #:tests? #f))
    (home-page "https://github.com/riywo/loginshell")
    (synopsis "Get the user's login shell in Go")
    (description
     "The loginshell package provides a Go library to get the login shell of
the current user.")
    (license license:expat)))

(define-public go-github-com-robfig-cron-v3
  (package
    (name "go-github-com-robfig-cron-v3")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/robfig/cron")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1agzbw2dfk2d1mpmddr85s5vh6ygm8kqrvfg87i9d2wqnlsnliqm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/robfig/cron/v3"))
    (home-page "https://godoc.org/github.com/robfig/cron")
    (synopsis "Cron library for Go")
    (description
     "This package provides a cron library for Go.  It implements a cron spec
parser and job runner.")
    (license license:expat)))

(define-public go-github-com-rogpeppe-fastuuid
  (package
    (name "go-github-com-rogpeppe-fastuuid")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rogpeppe/fastuuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "028acdg63zkxpjz3l639nlhki2l0canr2v5jglrmwa1wpjqcfff8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rogpeppe/fastuuid"))
    (home-page "https://github.com/rogpeppe/fastuuid")
    (synopsis "192-bit UUID generator in Golang")
    (description
     "Package fastuuid provides UUID generation of 192 bit universally unique
identifiers.  It also provides simple support for 128-bit RFC-4122 V4 UUID
strings.")
    (license license:bsd-3)))

(define-public go-github-com-rogpeppe-go-internal
  (package
    (name "go-github-com-rogpeppe-go-internal")
    (version "1.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rogpeppe/go-internal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18szjxqrjjvgsvyjbkqs6xw4bvg5nn1myg5hhb5qzwz5xl4wvw5a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/rogpeppe/go-internal"
      #:test-flags #~(list "-skip" "TestSimple/cover")))
    (propagated-inputs
     (list go-golang-org-x-mod
           go-golang-org-x-sys
           go-golang-org-x-tools))
    (home-page "https://github.com/rogpeppe/go-internal/")
    (synopsis "Internal packages from the Go standard library")
    (description
     "This package provides factors out an opinionated selection of internal
packages and functionality from the Go standard library.  Currently this
consists mostly of packages and testing code from within the Go tool
implementation.

Included are the following:
@itemize
@item dirhash: calculate hashes over directory trees the same way that the Go
tool does.
@item goproxytest: a GOPROXY implementation designed for test use.
@item gotooltest: Use the Go tool inside test scripts (see testscript below)
@item imports: list of known architectures and OSs, and support for reading
import import statements.
@item modfile: read and write go.mod files while preserving formatting and
comments.
@item module: module paths and versions.
@item par: do work in parallel.
@item semver: semantic version parsing.
@item testenv: information on the current testing environment.
@item testscript: script-based testing based on txtar files
@item txtar: simple text-based file archives for testing.
@end itemize")
    (license license:bsd-3)))

;; Breaking chanes and requires go-1.23+
(define-public go-github-com-rogpeppe-go-internal-1.14
  (package
    (inherit go-github-com-rogpeppe-go-internal)
    (name "go-github-com-rogpeppe-go-internal")
    (version "1.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rogpeppe/go-internal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14g7rmpcd25vjc0cm9issdy7160zy4h6r1wmsdihimc341ff3p78"))))
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/rogpeppe/go-internal"
      #:test-flags #~(list "-skip" "TestSimple/cover")))))

(define-public go-github-com-rootless-containers-proto-go-proto
  (package
    (name "go-github-com-rootless-containers-proto-go-proto")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/rootless-containers/proto")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01lmjg70l0ll2sf7kg114nhc1pd848j5xpgwb1xm3k0q81h4x9d3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/rootless-containers/proto"))
    (propagated-inputs (list go-google-golang-org-protobuf))
    (home-page "https://github.com/rootless-containers/proto")
    (synopsis "Protobuf descriptions for rootless containers purposes")
    (description
     "This package provides a protobuf definition of the
@code{user.rootlesscontainers} extended attribute.  The main purpose of this
attribute is to allow for a interoperable and standardised way of emulating
persistent syscalls in a @url{rootless container,
https://rootlesscontaine.rs/} (syscalls such as @code{chown(2)} which would
ordinarily fail).")
    (license license:asl2.0)))

(define-public go-github-com-rs-zerolog
  (package
    (name "go-github-com-rs-zerolog")
    (version "1.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rs/zerolog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14g5sy9n18k18pvzccdcvj5rqi670yw478yqm4n30hrfsdkm5jbp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rs/zerolog"
      ;; Unexpected error write unixgram @234fb->/run/systemd/journal/socket:
      ;; sendmsg: no such file or directory
      #:test-flags #~(list "-skip" "TestWriteReturnsNoOfWrittenBytes")))
    (propagated-inputs
     (list go-github-com-coreos-go-systemd-v22
           go-github-com-mattn-go-colorable
           go-github-com-pkg-errors
           go-github-com-rs-xid
           go-golang-org-x-tools))
    (home-page "https://github.com/rs/zerolog")
    (synopsis "Zero Allocation JSON Logger")
    (description
     "Package zerolog provides a lightweight logging library dedicated to JSON
logging.")
    (license license:expat)))

(define-public go-github-com-russross-blackfriday
  (package
    (name "go-github-com-russross-blackfriday")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/russross/blackfriday")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "036028ynpq52z9snmd2b1kjzyvv6n9sg71k651ndznggnw19aamp"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/russross/blackfriday"))
    (native-inputs
     (list go-github-com-pmezard-go-difflib))
    (propagated-inputs
     (list go-github-com-shurcool-sanitized-anchor-name))
    (home-page "https://github.com/russross/blackfriday")
    (synopsis "Markdown processor in Go")
    (description
     "Blackfriday is a Markdown processor in Go.")
    (license license:bsd-2)))

(define-public go-github-com-russross-blackfriday-v2
  (package
    (inherit go-github-com-russross-blackfriday)
    (name "go-github-com-russross-blackfriday-v2")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/russross/blackfriday")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d1rg1drrfmabilqjjayklsz5d0n3hkf979sr3wsrw92bfbkivs7"))))
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/russross/blackfriday/v2"))))

(define-public go-github-com-rwcarlsen-goexif
  ;; No release or version tag, Golang pseudo version:
  ;; 0.0.0-20190401172101-9e8deecbddbd.
  (let ((commit "9e8deecbddbd4989a3e8d003684b783412b41e7a")
        (revision "0"))
    (package
      (name "go-github-com-rwcarlsen-goexif")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rwcarlsen/goexif")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1drqhzplg72lvrf3qmb9awbggnjqp23hwn2pgvksi3spv17kc9h2"))))
      (build-system go-build-system)
      (arguments
       (list
        #:skip-build? #t
        #:import-path "github.com/rwcarlsen/goexif"))
      (home-page "https://github.com/rwcarlsen/goexif")
      (synopsis "Decode embedded EXIF meta data from image files")
      (description
       "This package provides decoding of basic EXIF and TIFF encoded data.
Functionality is split into packages:
@itemize

@item @code{exif} - implements decoding of EXIF data as defined in the EXIF
2.2 specification (http://www.exif.org/Exif2-2.PDF)

@item @code{mknote} - provides makernote parsers that can be used with
@code{goexif/exif}

@item @code{tiff} - implements TIFF decoding as defined in TIFF 6.0
specification at http://partners.adobe.com/public/developer/en/tiff/TIFF6.pdf

@end itemize")
      (license license:bsd-2))))

(define-public go-github-com-ryanuber-columnize
  (package
    (name "go-github-com-ryanuber-columnize")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ryanuber/columnize")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xxzzgvfabc2qx6n313vis8l4npkggiy5kjflv0arm2y7xnv73qj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ryanuber/columnize"))
    (home-page "https://github.com/ryanuber/columnize")
    (synopsis "Column formatted output for golang")
    (description
     "This package implements column-formatted output for Golang.")
    (license license:expat)))

(define-public go-github-com-sabhiram-go-gitignore
  (package
    (name "go-github-com-sabhiram-go-gitignore")
    (version "0.0.0-20210923224102-525f6e181f06")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sabhiram/go-gitignore")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "197giv3snczvbihzvkja5pq53yw5fc516rnjm71hni8gawb8jmh3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sabhiram/go-gitignore"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/sabhiram/go-gitignore")
    (synopsis "Gitignore parser for Go")
    (description "A @command{.gitignore} parser for Go.")
    (license license:expat)))

(define-public go-github-com-sahilm-fuzzy
  (package
    (name "go-github-com-sahilm-fuzzy")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sahilm/fuzzy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15j95gm7hcmg09x1b39vc4il8bryv4v0yljvvyq5vyc6iq66qrbz"))))
    (build-system go-build-system)
    (native-inputs
     (list go-github-com-kylelemons-godebug))
    (arguments
     (list #:import-path "github.com/sahilm/fuzzy"))
    (home-page "https://github.com/sahilm/fuzzy")
    (synopsis "Fuzzy string matching for Golang")
    (description
     "@code{fuzzy} provides fuzzy string matching optimized for filenames and code
symbols in the style of Sublime Text, VSCode, @code{IntelliJ} IDEA et al.")
    (license license:expat)))

(define-public go-github-com-samber-lo
  (package
    (name "go-github-com-samber-lo")
    (version "1.49.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/samber/lo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x82njn075hsb4vax6w7wih5g117chjvsjgzlgq9n1kn5ksi5i64"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/samber/lo"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-text))
    (home-page "https://github.com/samber/lo")
    (synopsis "Lodash-style Go library based on Go 1.18+ Generics")
    (description
     "This package implements a functionality to iterate over slices, maps,
channels etc. and heavily inspired by @url{https://github.com/lodash/lodash,
Lodash}.")
    (license license:expat)))

(define-public go-github-com-sap-go-hdb
  (package
    (name "go-github-com-sap-go-hdb")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SAP/go-hdb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zw37fi0msglsakwynj913zkbj3mfggjhdd5w042khnlm122b129"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/SAP/go-hdb/prometheus
            (delete-file-recursively "prometheus")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/SAP/go-hdb"
      ;; XXX: The most of the tests require access to database, run some
      ;; portion of unit tests only.
      #:test-subdirs #~(list "driver/internal/..."
                             "driver/unicode/cesu8")))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-golang-org-x-text))
    (home-page "https://github.com/SAP/go-hdb")
    (synopsis "SAP HANA Database Client for Golang")
    (description
     "Go-hdb is a native Go @url{https://en.wikipedia.org/wiki/SAP_HANA, HANA}
database driver for Go's sql package.  It implements the SAP HANA SQL command
network protocol.")
    (license license:asl2.0)))

(define-public go-github-com-saracen-walker
  (package
    (name "go-github-com-saracen-walker")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/saracen/walker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17i2zrbcp0zgwfgap555pk358wpqfa8qj8pmgwhjkzwd77nyl77g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/saracen/walker"))
    (inputs
     (list go-golang-org-x-sync))
    (home-page "https://github.com/saracen/walker")
    (synopsis "Faster, parallel version of Go's filepath.Walk")
    (description
     "The @code{walker} function is a faster, parallel version, of
@code{filepath.Walk}")
    (license license:expat)))

(define-public go-github-com-satori-go-uuid
  (package
    (name "go-github-com-satori-go-uuid")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/satori/go.uuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j4s5pfg2ldm35y8ls8jah4dya2grfnx2drb4jcbjsyrp4cm5yfb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/satori/go.uuid"))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://github.com/satori/go.uuid")
    (synopsis "UUID package for Go language")
    (description
     "Package uuid provides implementation of @acronym{Universally Unique
Identifier, UUID}. Supported versions are 1, 3, 4 and 5 (as specified in
@url{https://rfc-editor.org/rfc/rfc4122.html,RFC 4122}) and version 2 (as
specified in DCE 1.1).")
    (license license:expat)))

(define-public go-github-com-savsgio-gotils
  (package
    (name "go-github-com-savsgio-gotils")
    (version "0.0.0-20240704082632-aef3928b8a38")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/savsgio/gotils")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s3xsq4wy2xi17wq0kzw2zszj9vwndzvvmkx7pp7crx8k43x16ll"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/savsgio/gotils"))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-github-com-valyala-bytebufferpool))
    (home-page "https://github.com/savsgio/gotils")
    (synopsis "Golang utilities")
    (description
     "Golang utilities to make your life easier with zero allocations.")
    (license license:asl2.0)))

(define-public go-github-com-schachmat-ingo
  (package
    (name "go-github-com-schachmat-ingo")
    (version "0.0.0-20170403011506-a4bdc0729a3f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/schachmat/ingo")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gw0kddy7jh3467imsqni86cf9yq7k6vpfc0ywkbwj0zsjsdgd49"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/schachmat/ingo"))
    (home-page "https://github.com/schachmat/ingo")
    (synopsis "Go library to persist flags in a INI-like configuration file")
    (description
     "Ingo is a Go library helping you to persist flags in a INI-like
configuration file.")
    (license license:isc)))

(define-public go-github-com-schollz-cli-v2
  ;; It's ad-hoc fork of <https://github.com/urfave/cli> to build
  ;; <https://github.com/schollz/croc>.
  (hidden-package
   (package
     (name "go-github-com-schollz-cli-v2")
     (version "2.2.1")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/schollz/cli")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0wlqfhsrfib4b5b5xlkmgwglpzajjabrf4wisp7q8nvnw9ky86jh"))))
     (build-system go-build-system)
     (arguments
      (list
       #:import-path "github.com/schollz/cli/v2"
       #:test-flags
       ;; panic: flag "--foo" begins with - [recovered]
       ;; panic: flag "--foo" begins with -
       #~(list "-skip" "TestApp_RunAsSubCommandIncorrectUsage|TestToMan")))
     (propagated-inputs
      (list go-github-com-burntsushi-toml
            go-github-com-cpuguy83-go-md2man-v2
            go-gopkg-in-yaml-v2))
     (home-page "https://github.com/schollz/cli")
     (synopsis "Package for building command line apps in Golang")
     (description
      "Package @code{cli} provides a minimal framework for creating and
organizing command line Go applications.")
     (license license:expat))))

(define-public go-github-com-schollz-logger
  (package
    (name "go-github-com-schollz-logger")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/schollz/logger")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1680348j54vwfx7sczygchrd9dabnycj3mpxg3fmpf9a356vd2af"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/schollz/logger"
      ;; Go@1.24 forces vet, but tests are not ready yet.
      #:test-flags #~(list "-vet=off")))
    (home-page "https://github.com/schollz/logger")
    (synopsis "Simplistic, opinionated logging for Golang")
    (description
     "This package provides a opinionated logging for Golang.
Features:
@itemize
@item zero dependencies
@item Global logger (with optional local logger)
@item leveled
@item useful defaults / i.e. zero-config
@item simple API
@item colors on Linux
@item set leveling via environmental variables
@code{LOGGER=trace|debug|info|warn|error}
@end itemize")
    (license license:expat)))

(define-public go-github-com-schollz-progressbar-v3
  (package
    (name "go-github-com-schollz-progressbar-v3")
    (version "3.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/schollz/progressbar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hjahr5r52i7w6iyvl3rpzr46iignhfdh4694fl7m2b4gkaw9gd6"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/schollz/progressbar/v3"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-examples
                 (lambda* (#:key import-path #:allow-other-keys)
                   (delete-file-recursively
                    (string-append "src/" import-path "/examples"))))
               (replace 'check
                 (lambda* (#:key tests? import-path #:allow-other-keys)
                   (when tests?
                     ;; The full test suite requires Internet access, so only
                     ;; run the short tests.
                     (invoke "go" "test" "-test.short" import-path)))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth
           go-github-com-mitchellh-colorstring
           go-golang-org-x-term))
    (home-page "https://github.com/schollz/progressbar")
    (synopsis "Simple command-line interface (CLI) progress bar")
    (description
     "This package provides a very simple thread-safe progress bar.  The
@code{progressbar} implements an @code{io.Writer} so it can automatically
detect the number of bytes written to a stream, so you can use it as a
@code{progressbar} for an @code{io.Reader}.  When @code{progressbar}'s length
is undetermined, a customizable spinner is shown.")
    (license license:expat)))

(define-public go-github-com-sebdah-goldie-v2
  (package
    (name "go-github-com-sebdah-goldie-v2")
    (version "2.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sebdah/goldie")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06hyzl7cl09dfsmd0lb88icklv5g59hgs5wqgngl39fbawkbc411"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")
                 (delete-file-recursively "v2/vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sebdah/goldie/v2"
      #:unpack-path "github.com/sebdah/goldie"
      ;; Test requires to refresh test data: "The first time running go test,
      ;; with -update, without -clean".
      #:test-flags #~(list "-skip" "TestCleanFunction")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pmezard-go-difflib
           go-github-com-sergi-go-diff))
    (home-page "https://github.com/sebdah/goldie")
    (synopsis "Golden test utility")
    (description
     "Package goldie provides test assertions based on golden files.
It's typically used for testing responses with larger data bodies.")
    (license license:expat)))

(define-public go-github-com-seccomp-libseccomp-golang
  (package
    (name "go-github-com-seccomp-libseccomp-golang")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/seccomp/libseccomp-golang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lbrs4j8b8w47awczydb8snrky6gjgdfmcsvlwx4wvq20ipqr1nj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/seccomp/libseccomp-golang"))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libseccomp))
    (home-page "https://github.com/seccomp/libseccomp-golang")
    (synopsis "Bindings of @code{libseccomp} for Golang")
    (description
     "Package seccomp provides bindings for
@url{https://github.com/seccomp/libseccomp, libseccomp}, a library wrapping
the Linux seccomp syscall.  Seccomp enables an application to restrict system
call use for itself and its children.")
    (license license:bsd-2)))

(define-public go-github-com-segmentio-asm
  (package
    (name "go-github-com-segmentio-asm")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/segmentio/asm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01c90h83rq7fkvzfn28lz7x0455zxbvaxknd3c8259dfszfyr2zx"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/segmentio/asm/build
            (delete-file-recursively "build")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/segmentio/asm"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/segmentio/asm")
    (synopsis " Go library providing algorithms optimized for modern CPUs")
    (description
     "This package aims to provide algorithms optimized to
leverage advanced instruction sets of modern CPUs to maximize throughput and
take the best advantage of the available compute power.  It includes functions
that have often been designed to work on arrays of values, which is where SIMD
and branchless algorithms shine.")
    (license license:expat)))

(define-public go-github-com-segmentio-encoding
  (package
    (name "go-github-com-segmentio-encoding")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/segmentio/encoding")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gk2ry6s20h4j5gvl9vf83wi3badphnnzh6fhxfx3r24pbg7c2dx"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/segmentio/encoding/benchmarks
            ;; - github.com/segmentio/encoding/proto/fixtures
            (for-each delete-file-recursively
                      (list "benchmarks" "proto/fixtures"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:skip-build? #t
      #:import-path "github.com/segmentio/encoding"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestCodec/string#07"
                             "TestCodec/string#08"
                             "TestDecodeFixture"
                             "TestUnmarshalFixture")
                       "|"))))
    (propagated-inputs
     (list go-github-com-segmentio-asm))
    (home-page "https://github.com/segmentio/encoding")
    (synopsis "Encoding and decoding Go library")
    (description
     "Go package containing implementations of encoders and decoders for
various data formats.")
    (license license:expat)))

(define-public go-github-com-sereal-sereal-go-sereal
  (package
    (name "go-github-com-sereal-sereal-go-sereal")
    (version "0.0.0-20241017075749-134ea28a101c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Sereal/Sereal")
             (commit (go-version->git-ref version
                                          #:subdir "Go/sereal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16ig2v82kq3zpi04qvnd4a1swxyadd0pcp3886kq7ag29jq3p1na"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Sereal/Sereal/Go/sereal"
      #:unpack-path "github.com/Sereal/Sereal"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-datadog-zstd
           go-github-com-davecgh-go-spew
           go-github-com-dchest-siphash
           go-github-com-dgryski-go-ddmin
           go-github-com-golang-snappy
           go-github-com-google-go-cmp))
    (home-page "https://github.com/Sereal/Sereal")
    (synopsis "Binary serialization and deserialization Golang library")
    (description
     "Package sereal implements the @code{Sereal}, an efficient,
compact-output,binary and feature-rich serialization protocol.")
    (license license:bsd-2)))

(define-public go-github-com-sergi-go-diff
  (package
    (name "go-github-com-sergi-go-diff")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sergi/go-diff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c7lsa3kjxbrx66r93d0pvx1408b80ignpi39fzka1qc0ylshw32"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sergi/go-diff/diffmatchpatch"
      #:unpack-path "github.com/sergi/go-diff"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/sergi/go-diff/")
    (synopsis "Algorithms to perform operations for synchronizing plain text")
    (description "@code{go-diff} offers algorithms to perform operations required for
synchronizing plain text:
@itemize
@item compare two texts and return their differences
@item perform fuzzy matching of text
@item apply patches onto text
@end itemize")
    (license license:expat)))

(define-public go-github-com-sevlyar-go-daemon
  (package
    (name "go-github-com-sevlyar-go-daemon")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sevlyar/go-daemon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x2sn871g10jihga6jvng7ys1988dgj24wlkxzdzca6mvzysj80b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sevlyar/go-daemon"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/sevlyar/go-daemon")
    (synopsis "Library for writing system daemons")
    (description
     "Go-Daemon is a library for writing system daemons in Go.")
    (license license:expat)))

(define-public go-github-com-shirou-gopsutil
  (package
    (name "go-github-com-shirou-gopsutil")
    (version "2.21.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shirou/gopsutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gpb10xkdwfimn1sp4jhrvzz4p3zgmdb78q8v23nap3yi6v4bff5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shirou/gopsutil"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'remove-v3
                     (lambda* (#:key import-path #:allow-other-keys)
                       ;; We remove the separately included v3 module.
                       (delete-file-recursively (string-append "src/"
                                                               import-path
                                                               "/v3"))))
                   (add-before 'check 'remove-failing-tests
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively
                        ;; host_test.go tries to access files such as
                        ;; /var/run/utmp that do not exist in the build
                        ;; environment.
                        (string-append "src/" import-path "/host/host_test.go")))))))
    (propagated-inputs
     (list go-github-com-tklauser-go-sysconf go-golang-org-x-sys))
    (native-inputs
     (list go-github-com-stretchr-testify procps))
    (synopsis "Process and system monitoring in Go")
    (description
     "This package provides a library for retrieving information
on running processes and system utilization (CPU, memory, disks, network,
sensors).")
    (home-page "https://github.com/shirou/gopsutil")
    (license license:bsd-3)))

(define-public go-github-com-shirou-gopsutil-v3
  (package
    (inherit go-github-com-shirou-gopsutil)
    (name "go-github-com-shirou-gopsutil-v3")
    (version "3.24.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shirou/gopsutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r561fs7q2m0m03k8yrx3dpldjik3vbfjyfz0lz90zz0xvbavkxm"))))
    (arguments
     (list
      #:import-path "github.com/shirou/gopsutil/v3"
      ;; Two tests fail:
      ;; 1. error open /var/run/utmp: no such file or directory
      ;; 2. PlatformInformation() returns empty:
      #:test-flags #~(list "-skip" "TestUsers|TestPlatformInformation")))))

(define-public go-github-com-shirou-gopsutil-v4
  (package
    (inherit go-github-com-shirou-gopsutil-v3)
    (name "go-github-com-shirou-gopsutil-v4")
    (version "4.25.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/shirou/gopsutil")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q9125fpz4cw6mmxx7qa74qf3xq5x0nrraspfmwhlhdpabjag9lp"))))
    (arguments
     (list
      #:import-path "github.com/shirou/gopsutil/v4"
      ;; Three tests fail:
      ;; [1] error open /var/run/utmp: no such file or directory
      ;; [2] PlatformInformation() returns empty
      ;; [3] Could not get temperature
      #:test-flags #~(list "-skip" (string-join
                                    (list "TestUsers"               ;1
                                          "TestPlatformInformation" ;2
                                          "TestTemperatures")       ;3
                                    "|"))))))

(define-public go-github-com-shurcool-sanitized-anchor-name
  (package
    (name "go-github-com-shurcool-sanitized-anchor-name")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shurcooL/sanitized_anchor_name")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gv9p2nr46z80dnfjsklc6zxbgk96349sdsxjz05f3z6wb6m5l8f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shurcooL/sanitized_anchor_name"))
    (home-page "https://github.com/shurcooL/sanitized_anchor_name")
    (synopsis "Create sanitized anchor names")
    (description
     "This package provides a Go program for creating sanitized anchor
names.")
    (license license:expat)))

(define-public go-github-com-signintech-gopdf
  (package
    (name "go-github-com-signintech-gopdf")
    (version "0.29.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/signintech/gopdf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p63g8iqnq5i31v01i7hzzl09hjwi9474my2y1jzs0xfgfcg3mf1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/signintech/gopdf"
      #:test-flags
      #~(list "-skip" "TestImportPagesFromFile|TestTable|TestTableCenter")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))
    (propagated-inputs
     (list go-github-com-phpdave11-gofpdi
           go-github-com-pkg-errors))
    (home-page "https://github.com/signintech/gopdf")
    (synopsis "Generating PDF documents")
    (description "gopdf is a Go library for generating PDF documents.")
    (license license:expat)))

(define-public go-github-com-sirupsen-logrus
  (package
    (name "go-github-com-sirupsen-logrus")
    (version "1.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sirupsen/logrus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jz7nyq88i9fwfpp7krl046q62kjn6lb9j4r932bxnpypl1hwc49"))))
    (build-system go-build-system)
    (arguments
     (list
       #:import-path "github.com/sirupsen/logrus"
       #:test-flags #~(list "-skip" "TestNestedLoggingReportsCorrectCaller")
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs #:allow-other-keys #:rest args)
               (unless
                 ;; The tests fail when run with gccgo.
                 (false-if-exception (search-input-file inputs "/bin/gccgo"))
                 (apply (assoc-ref %standard-phases 'check) args)))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/sirupsen/logrus")
    (synopsis "Structured, pluggable logging for Go")
    (description "Logrus is a structured logger for Go, completely API
compatible with the standard library logger.")
    (license license:expat)))

(define-public go-github-com-skip2-go-qrcode
  (package
    (name "go-github-com-skip2-go-qrcode")
    (version "0.0.0-20200617195104-da1b6568686e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skip2/go-qrcode")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pghd6y2x8a5fqy4rjn4d8j5jcslb236naycdza5an7vyvinsgs9"))
       (patches (search-patches "go-github-com-skip2-go-qrcode-fix-tests.patch"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/skip2/go-qrcode"))
    (home-page "https://github.com/skip2/go-qrcode")
    (synopsis "QR code encoder")
    (description "This package provides a QR code encoder for the Goloang.")
    (license license:expat)))

(define-public go-github-com-skratchdot-open-golang
  (package
    (name "go-github-com-skratchdot-open-golang")
    (version "0.0.0-20200116055534-eef842397966")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skratchdot/open-golang")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n6387csjn024db8wldadsiy8ljz7lk7szl6ls28fcbkax7rw86y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f ; requires xdg-open and firefox in the PATH
      #:import-path "github.com/skratchdot/open-golang"))
    (home-page "https://github.com/skratchdot/open-golang")
    (synopsis "Open a file, directory, or URI using the default application")
    (description
     "Open a file, directory, or URI using the OS's default application for
that object type.  Optionally, you can specify an application to use.  On
GNU/Linux, this is a proxy for the @command{xdg-open} command.")
    (license license:expat)))

(define-public go-github-com-smacker-go-tree-sitter
  (package
    (name "go-github-com-smacker-go-tree-sitter")
    (version "0.0.0-20240827094217-dd81d9e9be82")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/smacker/go-tree-sitter")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15c7w5dv7zvhvgrf71fp5l74mjkiniqgz2cbv024pmkcwsvv48yj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/smacker/go-tree-sitter"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/smacker/go-tree-sitter")
    (synopsis "Golang bindings for tree-sitter")
    (description
     "This package provides a bindings for
@url{https://github.com/tree-sitter/tree-sitter, tree-sitter} in Golang.")
    (license license:expat)))

(define-public go-github-com-songgao-water
  (package
    (name "go-github-com-songgao-water")
    (version "0.0.0-20200317203138-2b4b6d7c09d8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/songgao/water")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1k5aildfszp6x66jzar4y36lic8ijkb5020hfaivpvq3bnwdiikl"))))
    (build-system go-build-system)
    (arguments '(#:tests? #f ; Tests require network interface access
                 #:import-path "github.com/songgao/water"))
    (home-page "https://github.com/songgao/water")
    (synopsis "Simple network TUN/TAP library")
    (description
      "This package provides a simple TUN/TAP interface library for Go that
efficiently works with standard packages like @code{io}, @code{bufio}, etc..
Use waterutil with it to work with TUN/TAP packets/frames.")
    (license license:bsd-3)))

(define-public go-github-com-songmu-gitconfig
  (package
    (name "go-github-com-songmu-gitconfig")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/songmu/gitconfig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p7b5n4h4vsjpb7ipfn4n1p8i978d8mlx8pi0m5dla57mj8v56hj"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Package's tests appear to be hardcoded to the author's gitconfig
      ;; and require network access.
      #:tests? #f
      #:import-path "github.com/Songmu/gitconfig"))
    (propagated-inputs
     (list go-github-com-goccy-go-yaml))
    (home-page "https://github.com/songmu/gitconfig")
    (synopsis "Go library to get configuration values from gitconfig")
    (description
     "@code{gitconfig} is a package to get configuration values from gitconfig.")
    (license license:expat)))

(define-public go-github-com-soniakeys-quant
  (package
    (name "go-github-com-soniakeys-quant")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/soniakeys/quant")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y50h4d9l4v1dxhf99ys6fha5c7viflwdnlfxn7glf2jr49x5z78"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/soniakeys/quant"))
    (home-page "https://github.com/soniakeys/quant")
    (synopsis "Interface for image color quantizers")
    (description
     "Quant provides an interface for image color quantizers.")
    (license license:expat)))

(define-public go-github-com-sosodev-duration
  (package
    (name "go-github-com-sosodev-duration")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sosodev/duration")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gbdx0gvlc2bh1q3204l8cfcfqk9qj859xxk3gnkr3f5kv914d71"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sosodev/duration"))
    (home-page "https://github.com/sosodev/duration")
    (synopsis "ISO 8601 duration parsing in Golang")
    (description
     "This package provides a functionality for parsing
@url{https://en.wikipedia.org/wiki/ISO_8601#Durations, ISO 8601 durations} and
converting them to the @code{time.Duration} type.")
    (license license:expat)))

(define-public go-github-com-sourcegraph-conc
  (package
    (name "go-github-com-sourcegraph-conc")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sourcegraph/conc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1anqhnpiak7fd6xxrjanwgrfz3c8ypksmx3zgx5f000bsfrlr1wq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sourcegraph/conc"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-go-uber-org-multierr))
    (home-page "https://github.com/sourcegraph/conc")
    (synopsis "Structured concurrency for Golang")
    (description
     "This package provides a more structured and controlled goroutines
implementation.")
    (license license:expat)))

(define-public go-github-com-spf13-afero
  (package
    (name "go-github-com-spf13-afero")
    ;; TODO: It's the latest version which does not require
    ;; google.golang.org/api, pulling 400+ missing dependencies.
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/afero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yi8p0yxiidpcg4cagxg2iyqcaapsng89rak4qyxmgails2fqg37"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spf13/afero"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-pkg-sftp
           go-golang-org-x-text))
    (home-page "https://github.com/spf13/afero")
    (synopsis "File system abstraction for Go")
    (description
     "This package provides a file system abstraction for Go.")
    (license license:asl2.0)))

(define-public go-github-com-spf13-cast
  (package
    (name "go-github-com-spf13-cast")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/cast")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l39v7zd8i8vvwls2frfxc583im5nj5x5lrdss4pcc36cd5a9vf4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spf13/cast"))
    (native-inputs
     (list go-github-com-frankban-quicktest))
    (home-page "https://github.com/spf13/cast")
    (synopsis "Safe and easy casting from one type to another in Go")
    (description
     "Safe and easy casting from one type to another in Go.")
    (license license:expat)))

(define-public go-github-com-spf13-cobra
  (package
    (name "go-github-com-spf13-cobra")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/cobra")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0brbyy5mc6n2j6m6q1xyswh907vxd3wdzvgaci45swgj0747lcf8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags #~(list "-skip" "TestGenManSeeAlso")
      #:import-path "github.com/spf13/cobra"))
    (native-inputs
     (list go-gopkg-in-yaml-v3))
    (propagated-inputs
     (list go-github-com-cpuguy83-go-md2man-v2
           go-github-com-spf13-pflag))
    (home-page "https://github.com/spf13/cobra")
    (synopsis "Go library for creating CLI applications")
    (description
     "Cobra is both a library for creating powerful modern CLI applications as
well as a program to generate applications and command files.")
    (license license:asl2.0)))

(define-public go-github-com-spf13-jwalterweatherman
  (package
    (name "go-github-com-spf13-jwalterweatherman")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/jwalterweatherman")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ywmkwci5zyd88ijym6f30fj5c0k2yayxarkmnazf5ybljv50q7b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spf13/jwalterweatherman"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/spf13/jwalterweatherman")
    (synopsis "Go logging library")
    (description
     "JWW is primarily a wrapper around the standard log library.  It provides
a few advantages over using the standard log library alone.

@itemize
@item ready to go out of the box
@item one library for both printing to the terminal and logging (to files)
@item really easy to log to either a temp file or a file you specify
@end itemize")
    (license license:expat)))

(define-public go-github-com-spf13-pflag
  (package
    (name "go-github-com-spf13-pflag")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/spf13/pflag")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ckdaa5q3afhgx5hi45czxn2pcc5fd0sz4axh4hqxyvgsjfjvmg0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spf13/pflag"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/spf13/pflag")
    (synopsis "Replacement for Go's @code{flag} package")
    (description
     "Pflag is library to replace Go's @code{flag} package.  It implements
POSIX/GNU-style command-line options with double hyphens.  It is is compatible
with the
@uref{https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html,
GNU extensions} to the POSIX recommendations for command-line options.")
    (license license:bsd-3)))

(define-public go-github-com-spf13-viper
  (package
    (name "go-github-com-spf13-viper")
    ;; Refreshing to a newer version requires long chain of missing packages.
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/viper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jcsvd9l05pv10rma6zicp44q2mlvxj0qlhnf4zcg2pymb5i0hq6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spf13/viper"
      ;; Optional modules are not packed.
      #:test-subdirs #~(list ".")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-fsnotify-fsnotify
           go-github-com-hashicorp-hcl
           go-github-com-magiconair-properties
           go-github-com-mitchellh-mapstructure
           go-github-com-pelletier-go-toml
           go-github-com-spf13-afero
           go-github-com-spf13-cast
           go-github-com-spf13-jwalterweatherman
           go-github-com-spf13-pflag
           go-github-com-subosito-gotenv
           go-gopkg-in-ini-v1
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/spf13/viper")
    (synopsis "Go configuration with fangs")
    (description
     "Viper is a complete configuration solution for Go applications including
12-Factor apps.  It is designed to work within an application, and can handle
all types of configuration needs and formats.")
    (license license:expat)))

(define-public go-github-com-ssor-bom
  (package
    (name "go-github-com-ssor-bom")
    (version "0.0.0-20170718123548-6386211fdfcf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ssor/bom")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09g5496ifwqxqclh2iw58plcwcz0sczlnxwqxzwmnl4shdl371ld"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ssor/bom"))
    (home-page "https://github.com/ssor/bom")
    (synopsis "Cleaning BOMs in Go")
    (description
     "The bom package provides small tools for cleaning BOMs from a byte array
or reader.")
    (license license:expat)))

(define-public go-github-com-stoewer-go-strcase
  (package
    (name "go-github-com-stoewer-go-strcase")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stoewer/go-strcase")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gqswiyj71h676lmqb78rs125iq8msklymhzw0w59afywxqsaj2z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/stoewer/go-strcase"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/stoewer/go-strcase")
    (synopsis "Convert snake case, camel case and kebap case strings")
    (description
     "Package strcase converts between different kinds of naming formats such
as camel case (@code{CamelCase}), snake case (@code{snake_case}) or kebab
case (@code{kebab-case}).  The package is designed to work only with strings
consisting of standard ASCII letters.")
    (license license:expat)))

(define-public go-github-com-stretchr-objx
  (package
    (name "go-github-com-stretchr-objx")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stretchr/objx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jcxpfgfpk82lryjkhbd5dy7xzx08d7b9dvbx4bpkmjvn6p339jl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/stretchr/objx"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (unless
                  ;; The tests fail when run with gccgo.
                  (false-if-exception (search-input-file inputs "/bin/gccgo"))
                (apply (assoc-ref %standard-phases 'check) args)))))))
    (native-inputs
     ;; go-spew and go-difflib are to cover testify-bootstrap and not required
     ;; for odjx itself.
     (list go-github-com-davecgh-go-spew
           go-github-com-pmezard-go-difflib
           go-github-com-stretchr-testify-bootstrap))
    (home-page "https://github.com/stretchr/objx")
    (synopsis "Go package for dealing with maps, slices, JSON and other data")
    (description
     "This package provides a Go library for dealing with maps,
slices, JSON and other data.")
    (license license:expat)))

(define-public go-github-com-subosito-gotenv
  (package
    (name "go-github-com-subosito-gotenv")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/subosito/gotenv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h7kb9mc67rl16kvls2d16pimdrz59l5x4l002qsv2p2766mpjif"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/subosito/gotenv"
      #:test-flags #~(list "-skip" "TestScanner")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-text))
    (home-page "https://github.com/subosito/gotenv")
    (synopsis "Go library for loading environment variables from files")
    (description
     "Go library for loading environment variables from files")
    (license license:expat)))

(define-public go-github-com-surge-glog
  (package
    (name "go-github-com-surge-glog")
    (version "0.0.0-20141108051140-2578deb2b95c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/surge/glog")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bxcwxvsvr2hfpjz9hrrn0wrgykwmrbyk567102k3vafw9xdcwk4"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/surge/glog"))
    (home-page "https://github.com/surge/glog")
    (synopsis "Leveled execution logs for Go")
    (description
     "Leveled execution logs for Go.")
    (license license:asl2.0)))

(define-public go-github-com-surgebase-porter2
  (package
    (name "go-github-com-surgebase-porter2")
    (version "0.0.0-20150829210152-56e4718818e8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/surgebase/porter2")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ivcf83jlj9s7q5y9dfbpyl0br35cz8fcp0dm8sxxvqh54py06v2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/surgebase/porter2"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-build
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "cmd/suffixfsm/suffixfsm.go"
                  ;; fmt.Println arg list ends with redundant newline
                  (("fmt.Println") "fmt.Printf"))))))))
    (native-inputs
     (list go-github-com-agonopol-go-stem
           go-github-com-dchest-stemmer
           go-github-com-kljensen-snowball
           go-github-com-reiver-go-porterstemmer
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-surge-glog))
    (home-page "https://github.com/surgebase/porter2")
    (synopsis "Go library implementing english Porter2 stemmer")
    (description
     "Porter2 implements the
@url{http://snowball.tartarus.org/algorithms/english/stemmer.html, english
Porter2 stemmer}.  It is written completely using finite state machines to do
suffix comparison, rather than the string-based or tree-based approaches.")
    (license license:asl2.0)))

(define-public go-github-com-syndtr-gocapability
  (package
    (name "go-github-com-syndtr-gocapability")
    (version "0.0.0-20200815063812-42c35b437635")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/syndtr/gocapability")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00zi0k190ydlm9drnafvj9p4cf6axm858wr71pbmq1p3r94iqws4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/syndtr/gocapability"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'go-generate
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path
                                                       "/capability/enumgen")
                (invoke "go" "generate" "-v" "-n")))))))
    (home-page "https://github.com/syndtr/gocapability")
    (synopsis "Utilities for manipulating POSIX capabilities in Golang")
    (description
     "This package provides utilities for manipulating POSIX capabilities.")
    (license license:bsd-2)))

(define-public go-github-com-syndtr-goleveldb
  (package
    (name "go-github-com-syndtr-goleveldb")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syndtr/goleveldb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "042k0gbzs5waqpxmd7nv5h93mlva861s66c3s9gfg1fym5dx4vmd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/syndtr/goleveldb"
      #:skip-build? #t
      #:test-flags #~(list "-skip" "TestBatchHeader")))
    (propagated-inputs
     (list go-github-com-onsi-gomega
           go-github-com-onsi-ginkgo
           go-github-com-golang-snappy))
    (home-page "https://github.com/syndtr/goleveldb")
    (synopsis "LevelDB implementation in Go")
    (description
     "This package provides a Go implementation of the LevelDB key/value
storage system.")
    (license license:bsd-2)))

(define-public go-github-com-tannerryan-ring
  (package
    (name "go-github-com-tannerryan-ring")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tannerryan/ring")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07q5qcg2wv696nnw3rrgc49mqijapdwp3xsscaxb5867bz79s841"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tannerryan/ring"))
    (home-page "https://github.com/tannerryan/ring")
    (synopsis "High performance bloom filter")
    (description
     "@code{ring} provides a high performance and thread safe Go implementation of a
@url{https://en.wikipedia.org/wiki/Bloom_filter, bloom filter}.")
    (license license:bsd-2)))

(define-public go-github-com-tdewolff-argp
  (package
    (name "go-github-com-tdewolff-argp")
    (version "0.0.0-20250430135133-0f54527d2b1e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/argp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06pjm6z0b21rjwwpq0b18mr7v2h7igf9rrv7zqlaadmv5i58cbh4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tdewolff/argp"))
    (native-inputs
     (list go-github-com-tdewolff-test))
    (propagated-inputs
     (list go-github-com-go-sql-driver-mysql
           go-github-com-jmoiron-sqlx
           go-github-com-pelletier-go-toml
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/tdewolff/argp")
    (synopsis "GNU command line argument parser")
    (description "Command line argument parser following the GNU standard.")
    (license license:expat)))

(define-public go-github-com-tdewolff-hasher
  (package
    (name "go-github-com-tdewolff-hasher")
    (version "0.0.0-20210521220142-bc97f602bca2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/hasher")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12dmxpmdy2z7c2z7qv2mv2aq4hyvjncb6fzr0ymg3y5bfjvl4dcw"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/tdewolff/hasher"))
    (native-inputs
     (list go-github-com-cespare-mph
           go-github-com-dgryski-go-mph))
    (home-page "https://github.com/tdewolff/hasher")
    (synopsis "Go known-keys fast-lookup map generator")
    (description
     "Hasher is a tool to automate the creation of methods and tables for a
@code{string} to @code{uint32} mapper.")
    (license license:bsd-3)))

(define-public go-github-com-teambition-rrule-go
  (package
    (name "go-github-com-teambition-rrule-go")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/teambition/rrule-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fnbava35w9z60carny5b7whd4nkv6hrf9g43wwg8d88gfij9zj2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/teambition/rrule-go"))
    (home-page "https://github.com/teambition/rrule-go")
    (synopsis "Recurrence rules for calendar dates for Golang")
    (description
     "This package provides a functionality to work with recurrence rules for
calendar dates.  It offers a complete implementation of the
@url{https://www.ietf.org/rfc/rfc2445.txt,RFC 2445} specification.")
    (license license:expat)))

;; XXX: Maybe split it into dedicated packages per module to easy importer.
(define-public go-github-com-tekwizely-go-parsing
  (package
    (name "go-github-com-tekwizely-go-parsing")
    (version "0.0.0-20221001173913-aa6d6749ea2d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tekwizely/go-parsing")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hz4jwvav1ccvigmlxgg50pal3nxklbl0psf7wdzwr1vzmzmj3n3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tekwizely/go-parsing"))
    (home-page "https://github.com/tekwizely/go-parsing")
    (synopsis "Text parsing, with lexers, parsers, and related tools")
    (description
     "This package provides Go modules focused on text parsing, with lexers,
parsers, and related tools.

Included modules are:
@itemize
@item github.com/tekwizely/go-parsing
@item github.com/tekwizely/go-parsing/lexer
@item github.com/tekwizely/go-parsing/lexer/token
@item github.com/tekwizely/go-parsing/parser
@end itemize")
    (license license:expat)))

(define-public go-github-com-templexxx-cpu
  (package
    (name "go-github-com-templexxx-cpu")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/templexxx/cpu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bfygzjz2n8fbc9n9k7hfdnaz5mw6g4n4n1q3a0hyv33j9bvl5hh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/templexxx/cpu"))
    (home-page "https://github.com/templexxx/cpu")
    (synopsis "@code{internal/cpu} in Go (add AVX512)")
    (description
     "Package cpu implements processor feature detection used by the Go
standard library.")
    (license license:bsd-3)))

(define-public go-github-com-templexxx-xorsimd
  (package
    (name "go-github-com-templexxx-xorsimd")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/templexxx/xorsimd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01227dcf5mjc2v9f3shvqvbx3vxz89la7imjnjfmas61fcjwdlj5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/templexxx/xorsimd"))
    (propagated-inputs (list go-github-com-templexxx-cpu))
    (home-page "https://github.com/templexxx/xorsimd")
    (synopsis "XOR in pure Golang")
    (description
     "This package provides XOR bitwise code engine.")
    (license license:expat)))

(define-public go-github-com-texttheater-golang-levenshtein
  (package
    (name "go-github-com-texttheater-golang-levenshtein")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/texttheater/golang-levenshtein")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14r17scr7qc7bcc9xidg9g6vb1dnk96ffcclppc53s11k63hrgaq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/texttheater/golang-levenshtein"))
    (home-page "https://github.com/texttheater/golang-levenshtein")
    (synopsis "Implementation of the Levenshtein algorithm in Golang")
    (description
     "This package implements the Levenshtein algorithm in Go, providing edit
distances, edit scripts and ratios for strings (slices of runes).")
    (license license:expat)))

(define-public go-github-com-thejerf-suture
  (package
    (name "go-github-com-thejerf-suture")
    (version "3.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thejerf/suture")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "166hbjc1gn7skvq9vcp5h1xkavw9zw6dwx63vhih8fzm3nbbp0ic"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/thejerf/suture"))
    (home-page "https://github.com/thejerf/suture")
    (synopsis "Supervisor trees for Go")
    (description "Suture provides Erlang-ish supervisor trees for Go.
\"Supervisor trees\" -> \"sutree\" -> \"suture\" -> holds your code together
when it's trying to die.

It is intended to deal gracefully with the real failure cases that can occur
with supervision trees (such as burning all your CPU time endlessly restarting
dead services), while also making no unnecessary demands on the \"service\"
code, and providing hooks to perform adequate logging with in a production
environment")
    (license license:expat)))

(define-public go-github-com-thejerf-suture-v4
  (package
    (inherit go-github-com-thejerf-suture)
    (name "go-github-com-thejerf-suture-v4")
    (version "4.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thejerf/suture")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15qi7v2a1kbf70yi3w6y26wbwj0sm8hv9f6xjrb4rl6nv9l8j88c"))))))

(define-public go-github-com-tidwall-cities
  (package
    (name "go-github-com-tidwall-cities")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/cities")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h7imqfhn3w9cfv3zaa7k55s53sas9n628amvjh6kzzjii1a0ix3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/cities"))
    (home-page "https://github.com/tidwall/cities")
    (synopsis "10,000 Cities with Latitude, Longitude, and Elevation")
    (description
     "This package provides geographical locations of countries and cities.")
    (license license:unlicense)))

(define-public go-github-com-tidwall-gjson
  (package
    (name "go-github-com-tidwall-gjson")
    (version "1.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/gjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gcjzbs5in4kics39d2v3j2v9gvfxkdgp0bdgbfmcsa5arqgq7g5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/gjson"))
    (propagated-inputs
     (list go-github-com-tidwall-match
           go-github-com-tidwall-pretty))
    (home-page "https://github.com/tidwall/gjson")
    (synopsis "JSON parser for Golang")
    (description
     "This package provides a fast and simple way to get values from a JSON
document.  It has features such as one line retrieval, dot notation paths,
iteration, and parsing JSON lines.")
    (license license:expat)))

(define-public go-github-com-tidwall-match
  (package
    (name "go-github-com-tidwall-match")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/match")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n25md63xr5m66r6zc77n6fgcpv2ljrlk92ivp9hvp8xya22as9k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/match"))
    (home-page "https://github.com/tidwall/match")
    (synopsis "Simple string pattern matcher for Golang")
    (description
     "Package match provides a simple pattern matcher with unicode support.")
    (license license:expat)))

(define-public go-github-com-tidwall-pretty
  (package
    (name "go-github-com-tidwall-pretty")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/pretty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0prj9vpjgrca70rvx40kkl566yf9lw4fsbcmszwamwl364696jsb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/pretty"))
    (home-page "https://github.com/tidwall/pretty")
    (synopsis "JSON beautifier and compactor for Golang")
    (description
     "This package provides fast methods for formatting JSON for human
readability, or to compact JSON for smaller payloads.")
    (license license:expat)))

(define-public go-github-com-tidwall-sjson
  (package
    (name "go-github-com-tidwall-sjson")
    (version "1.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/sjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16yaikpxiwqz00zxa70w17k2k52nr06svand88sv2br6b6i8v09r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/sjson"))
    (propagated-inputs
     (list go-github-com-tidwall-gjson
           go-github-com-tidwall-pretty))
    (home-page "https://github.com/tidwall/sjson")
    (synopsis "Quick value JSON values setting in Golang")
    (description
     "This package provides a fast and simple way to set a value in a JSON
document.")
    (license license:expat)))

(define-public go-github-com-timshannon-bolthold
  (package
    (name "go-github-com-timshannon-bolthold")
    (version "0.0.0-20240314194003-30aac6950928")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/timshannon/bolthold")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "107r4nwhvpdp0n9b5fls1lw8z8qsiajiykkpjs7947nrbc07ij1j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/timshannon/bolthold"
      ;; Test suite fails.
      #:tests? #f))
    (propagated-inputs (list go-go-etcd-io-bbolt))
    (home-page "https://github.com/timshannon/bolthold")
    (synopsis "Indexing and querying on top of a Bold database")
    (description
     "Package bolthold is an indexing and querying layer on top of a Bolt
database. The goal is to allow easy, persistent storage and retrieval of Go
types.  BoltDB is an embedded key-value store, and bolthold servers a similar
use case however with a higher level interface for common uses of BoltDB.")
    (license license:expat)))

(define-public go-github-com-tinylib-msgp
  (package
    (name "go-github-com-tinylib-msgp")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tinylib/msgp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mplb420i9cmf40qwsqzd1plln52nl0x0b7nkxffyr0pdh9za79a"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests require alternative Golang compiler
      ;; <https://github.com/tinygo-org/tinygo>.
      #:tests? #f
      #:import-path "github.com/tinylib/msgp"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs
     (list go-golang-org-x-tools go-github-com-philhofer-fwd))
    (home-page "http://msgpack.org/")
    (synopsis "MessagePack Code Generator")
    (description
     "This package provides a code generation tool for creating methods to
serialize and de-serialize Go data structures to and from data interchange
format - @url{https://en.wikipedia.org/wiki/MessagePack,MessagePack}.")
    (license license:expat)))


(define-public go-github-com-tj-go-buffer
  (package
    (name "go-github-com-tj-go-buffer")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/tj/go-buffer")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xs8dz8m5qy1n80qcpalvfzdjxdr7djmagmhp7mm87rmjkwn05lk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tj/go-buffer"))
    (native-inputs
     (list go-github-com-tj-assert))
    (home-page "https://github.com/tj/go-buffer")
    (synopsis "Generic buffer for batching entries, such as log events")
    (description
     "Package buffer provides a generic buffer or batching mechanism for
flushing entries at a given size or interval, useful for cases such as
batching log events.")
    (license license:expat)))

(define-public go-github-com-tj-go-spin
  (package
    (name "go-github-com-tj-go-spin")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tj/go-spin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11xr67991m5pwsy1dira3iwd0sr55vmn1cyjwmlqziw4bwpym64s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tj/go-spin"))
    (home-page "https://github.com/tj/go-spin")
    (synopsis "Terminal spinner package for Golang")
    (description "This package provides a little terminal spinner library.")
    (license license:expat)))

(define-public go-github-com-tklauser-go-sysconf
  (package
    (name "go-github-com-tklauser-go-sysconf")
    (version "0.3.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tklauser/go-sysconf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07vkimncnmh89706s49599h2w9gwa6jyrv70f8ifw90nsh766km9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tklauser/go-sysconf"
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'remove-failing-tests
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively
                        ;; sysconf_test.go (among others) tries to read the
                        ;; number of online CPUs using /proc/stat and
                        ;; /sys/devices/system/cpu/online. These files are not
                        ;; accessible in the test environment.
                        (string-append "src/" import-path
                                       "/cgotest/sysconf_test.go")))))))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-tklauser-numcpus))
    (home-page "https://github.com/tklauser/go-sysconf")
    (synopsis "Go implementation of @code{sysconf}")
    (description
     "This package implements @code{sysconf} and provides the associated
@code{SC_*} constants to query system configuration values at run time.")
    (license license:bsd-3)))

(define-public go-github-com-tklauser-numcpus
  (package
    (name "go-github-com-tklauser-numcpus")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tklauser/numcpus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xcwk42zr6q72zvkqdd9nbyhvq11rmwm2164mr2rvbb9z7alkff8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tklauser/numcpus"
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'remove-failing-tests
                     (lambda* (#:key import-path #:allow-other-keys)
                       (with-directory-excursion (string-append "src/"
                                                                import-path)
                         (for-each delete-file-recursively
                                   ;; These tests try to access
                                   ;; /sys/devices/system/cpu, which is not
                                   ;; available in the test environment.
                                   '("numcpus_test.go" "numcpus_linux_test.go"))))))))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/tklauser/numcpus")
    (synopsis "Provides information about the number of CPUs in the system")
    (description
     "This package provides both library functions and a command-line tool to
query information regarding the number of CPUs available to the system.")
    (license license:asl2.0)))

(define-public go-github-com-tkuchiki-go-timezone
  (package
    (name "go-github-com-tkuchiki-go-timezone")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tkuchiki/go-timezone")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rmvg4hh0br51vbsxacani2g0v5xxsayp8q4xli9jag25zi5rhd1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tkuchiki/go-timezone"))
    (home-page "https://github.com/tkuchiki/go-timezone")
    (synopsis "Timezone utility for Golang")
    (description
     "This package provides provides an utility for timezone manipulation,
implementing the following features:

@itemize
@item this library uses only the standard package
@item supports getting offset from timezone abbreviation, which is not
supported by the time package
@item determine whether the specified time.Time is daylight saving time
@item change the location of time.Time by specifying the timezone
@end itemize")
    (license license:expat)))

(define-public go-github-com-tomwright-dasel-v2
  (package
    (name "go-github-com-tomwright-dasel-v2")
    (version "2.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TomWright/dasel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qh32gq0x0lc4j4w326skxw4nblf2v71ychd8dk6h3ra5d22bbmy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list ".*\\.xml")
      #:import-path "github.com/tomwright/dasel/v2"))
    (propagated-inputs
     (list go-github-com-alecthomas-chroma-v2
           go-github-com-clbanning-mxj-v2
           go-github-com-pelletier-go-toml-v2
           go-github-com-spf13-cobra
           go-golang-org-x-net
           go-golang-org-x-text
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/tomwright/dasel")
    (synopsis "Tool to work with JSON, TOML, YAML, XML and CSV files")
    (description
     "Dasel (short for data-selector) allows you to query and modify data
structures using selector strings.  It's similar to @code{jq}/@code{yq}, but
supports JSON, YAML, TOML, XML and CSV with zero runtime dependencies.")
    (license license:expat)))

(define-public go-github-com-twpayne-go-shell
  (package
    (name "go-github-com-twpayne-go-shell")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twpayne/go-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rnwdlhiakcigmz55fhn0wib6vi064cqxfq512mi880j9yqx4114"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/twpayne/go-shell"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2))
    (home-page "https://github.com/twpayne/go-shell")
    (synopsis "Shell across multiple platforms")
    (description
     "Package @code{shell} returns a user's shell across multiple platforms.")
    (license license:expat)))

(define-public go-github-com-twpayne-go-vfs-v5
  (package
    (name "go-github-com-twpayne-go-vfs-v5")
    (version "5.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twpayne/go-vfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "152hbb6ww2f2ac2bf1d446vw8z8m22n1rsa7gvlzfa060vj9hjgx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/twpayne/go-vfs/v5"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2))
    (home-page "https://github.com/twpayne/go-vfs/")
    (synopsis "Abstraction of the @code{os} and @code{ioutil} Go packages")
    (description
     "Package @code{vfs} provides an abstraction of the @code{os} and
@code{ioutil} packages that is easy to test.")
    (license license:expat)))

;; For chezmoi@2.1.0
(define-public go-github-com-twpayne-go-vfs-v3
  (hidden-package (package (inherit go-github-com-twpayne-go-vfs-v5)
   (name "go-github-com-twpayne-go-vfs-v3")
   (version "3.0.0")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/twpayne/go-vfs")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0w7alyd2pdafny4xfi0ybpnhrwwbkb3fk73yjrwjj6h0rysvpxy2"))))
    (arguments
     (list
      #:import-path "github.com/twpayne/go-vfs/v3"))
    (native-inputs
     (list go-github-com-stretchr-testify)))))

(define-public go-github-com-twpayne-go-xdg-v6
  (package
    (name "go-github-com-twpayne-go-xdg-v6")
    (version "6.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twpayne/go-xdg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ihpljay4waw3qss40bwd230wwpq3vm25qiy3y5yfhmybk8wr6jx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/twpayne/go-xdg/v6"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2))
    (propagated-inputs
     (list go-github-com-twpayne-go-vfs-v5))
    (home-page "https://github.com/twpayne/go-xdg/")
    (synopsis "Functions related to freedesktop.org")
    (description
     "Package @code{xdg} provides functions related to
@uref{freedesktop.org}.")
    (license license:expat)))

(define-public go-github-com-txthinking-runnergroup
  (package
    (name "go-github-com-txthinking-runnergroup")
    (version "0.0.0-20241229123329-7b873ad00768")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/txthinking/runnergroup")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l0accx6c880smkcma0qca3c67kx8p1bc4q2zq54iv8yjy1b2h4w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/txthinking/runnergroup"))
    (home-page "https://github.com/txthinking/runnergroup")
    (synopsis "Golang standard @code{sync.WaitGroup} alternative")
    (description
     "This package implements a similar functionality like standard
@code{sync.WaitGroup}}, the difference is if one task stops, all will be
stopped.")
    (license license:expat)))

(define-public go-github-com-u-root-uio
  (package
    (name "go-github-com-u-root-uio")
    (version "0.0.0-20240224005618-d2acac8f3701")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/u-root/uio")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ckxps3xbgllij3wnmmp2k9ip3z9v8vyc4wdcv43fxczqfiq25xz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/u-root/uio"))
    (propagated-inputs
     (list go-github-com-pierrec-lz4-v4
           go-golang-org-x-sys))
    (home-page "https://github.com/u-root/uio")
    (synopsis "Log and IO utilities")
    (description
     "This package provides shared utilities used to break circular
dependencies of @url{https://u-root.org/, u-root} project.")
    (license license:bsd-3)))

(define-public go-github-com-urfave-cli
  (package
    (name "go-github-com-urfave-cli")
    (version "1.22.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "022abbjwr3g2vbyfbdc1hg09d753hfba21b69n2nkrx168ag5ahd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/urfave/cli"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-github-com-cpuguy83-go-md2man-v2
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/urfave/cli")
    (synopsis "Simple, fast, and fun package for building command line apps in Go")
    (description
     "@command{cli} is a simple, fast, and fun package for building command
line apps in Go.  The goal is to enable developers to write fast and
distributable command line applications in an expressive way.")
    (license license:expat)))

(define-public go-github-com-urfave-cli-v2
  (package
    (inherit go-github-com-urfave-cli)
    (name "go-github-com-urfave-cli-v2")
    (version "2.27.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03237hi2jqvms9cif4varyap3j1dhzcf1mr809dm7ncvzk7gxg83"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/urfave/cli/v2/cmd/urfave-cli-genflags
            (delete-file-recursively "cmd")))))
    (arguments
     (list #:import-path "github.com/urfave/cli/v2"))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-github-com-cpuguy83-go-md2man-v2
           go-github-com-xrash-smetrics
           go-gopkg-in-yaml-v3))))

(define-public go-github-com-urfave-cli-v3
  (package
    (inherit go-github-com-urfave-cli-v2)
    (name "go-github-com-urfave-cli-v3")
    (version "3.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g8vk5wpx2vnb600ppkjh9j8aql0jgdgp5rrk06h92j7sx17fnfc"))
       (modules '((guix build utils)))
       (snippet #~(begin
                    ;; Submodules with their own go.mod files and packaged separately:
                    ;;
                    ;; - github.com/urfave/cli/docs/v3
                    (delete-file-recursively "docs")))))
    (arguments
     (list
      #:import-path "github.com/urfave/cli/v3"
      #:test-flags
      #~(list "-vet=off")))
    (propagated-inputs '())))

(define-public go-github-com-urfave-cli-altsrc-v3
  (package
    (name "go-github-com-urfave-cli-altsrc-v3")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/cli-altsrc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nk1c31li7yijfa2p9s2723p5727ad3fl80dj6a5cwjg1qkbsl12"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ;tests need git in PATH
      #:import-path "github.com/urfave/cli-altsrc/v3"
      ;; Pattern autocomplete: cannot embed directory autocomplete: contains
      ;; no embeddable files.
      #:embed-files
      #~(list "bash_autocomplete"
              "powershell_autocomplete.ps1"
              "zsh_autocomplete")))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-github-com-urfave-cli-v3))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/urfave/cli-altsrc")
    (synopsis "Read values for urfave/cli/v3 flags from config files")
    (description
     "This package provides an extension for https://github.com/urfave/cli, to
read flag values from JSON, YAML, and TOML.")
    (license license:expat)))

(define-public go-github-com-valyala-bytebufferpool
  (package
    (name "go-github-com-valyala-bytebufferpool")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/bytebufferpool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01lqzjddq6kz9v41nkky7wbgk7f1cw036sa7ldz10d82g5klzl93"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/valyala/bytebufferpool"))
    (home-page "https://github.com/valyala/bytebufferpool")
    (synopsis "Anti-memory-waste byte buffer pool for Golang")
    (description
     "@code{bytebufferpool} implements a pool of byte buffers with
anti-fragmentation protection.")
    (license license:expat)))

(define-public go-github-com-valyala-fastjson
  (package
    (name "go-github-com-valyala-fastjson")
    (version "1.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/valyala/fastjson")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ly15rbdy9qmml39d8mazjvid3f13nhvj4v2zdlp13pn4gczdp3k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/valyala/fastjson"))
    (home-page "https://github.com/valyala/fastjson")
    (synopsis "JSON parser and validator for Golang")
    (description
     "Package fastjson provides fast JSON parsing comparing to std @code{encoding/json}.")
    (license license:expat)))

(define-public go-github-com-valyala-fasttemplate
  (package
    (name "go-github-com-valyala-fasttemplate")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/fasttemplate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12hywkz2mfvxzfpgabc53bm4jkxxmcssrr0k4wxzzrnv0v7mj6bj"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/valyala/fasttemplate"))
    (propagated-inputs
     (list go-github-com-valyala-bytebufferpool))
    (home-page "https://github.com/valyala/fasttemplate")
    (synopsis "Template engine for Golang")
    (description
     "Package fasttemplate implements simple and fast template library.")
    (license license:expat)))

(define-public go-github-com-vbatts-go-mtree
  (package
    (name "go-github-com-vbatts-go-mtree")
    (version "0.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/vbatts/go-mtree")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18m6dlhl9kl0wy0dficc5yy75qj58z315bcnaypgrm877vmzad9h"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/vbatts/go-mtree"))
    (native-inputs
     (list go-github-com-davecgh-go-spew))
    (propagated-inputs
     (list go-github-com-fatih-color
           go-github-com-sirupsen-logrus
           go-github-com-urfave-cli-v2
           go-golang-org-x-crypto
           go-golang-org-x-sys))
    (home-page "https://github.com/vbatts/go-mtree")
    (synopsis "File systems verification utility and library")
    (description
     "@code{mtree} is a filesystem hierarchy validation tooling and format.
This is a library and simple CLI tool for
@url{https://www.freebsd.org/cgi/man.cgi?mtree(8), mtree(8)} implemented in
Go.")
    (license license:bsd-3)))

(define-public go-github-com-vburenin-ifacemaker
  (package
    (name "go-github-com-vburenin-ifacemaker")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vburenin/ifacemaker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00i3kxyk9mp3ph2kq8qv2572w9czcizgv41j9ayasi0z9x0zk131"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/vburenin/ifacemaker"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-jessevdk-go-flags
           go-golang-org-x-tools))
    (home-page "https://github.com/vburenin/ifacemaker")
    (synopsis "Generate interfaces from structure methods in Golang")
    (description
     "This is a development helper program that generates a Golang interface
by inspecting the structure methods of an existing @code{.go} file.  The
primary use case is to generate interfaces for gomock, so that gomock can
generate mocks from those interfaces.")
    (license license:asl2.0)))

(define-public go-github-com-viant-toolbox
  (package
    (name "go-github-com-viant-toolbox")
    (version "0.37.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/viant/toolbox")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m49nar5cwx3wqym20awh7yw4fw2bdw21m4h71bx4g688gv1kri4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/viant/toolbox"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestCase_To"
                             "Test_NewReplayService")
                       "|"))
      #:test-subdirs
      #~(list "bridge/..."
              "cred/..."
              ;; Tests fail on i686-linux system:
              ;; <...>/conversion_test.go:142:17: cannot use 2323232323223
              ;; (untyped int constant) as int value in argument to aMap.Put
              ;; (overflows).
              #$@(if (target-64bit?)
                     '("data/...")
                     '())
              "format/..."
              "sampler/..."
              "secret/..."
              "ssh/..."
              "test/..."
              "unsafe/..."
              "url/...")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    ;; XXX: No go.mod to list dependencies, see
    ;; <https://github.com/viant/toolbox/pull/51>.
    (propagated-inputs
     (list ;go-cloud-google-com-go-storage
           ;go-github-com-aws-aws-sdk-go-aws
           ;go-github-com-aws-aws-sdk-go-aws-credentials
           ;go-github-com-aws-aws-sdk-go-aws-session
           ;go-github-com-aws-aws-sdk-go-service-kms
           ;go-github-com-aws-aws-sdk-go-service-s3
           ;go-github-com-aws-aws-sdk-go-service-s3-s3manager
           ;go-github-com-aws-aws-sdk-go-service-ssm
           go-github-com-go-errors-errors
           go-github-com-lunixbochs-vtclean
           go-github-com-pkg-errors
           go-github-com-viant-xunsafe
           go-golang-org-x-crypto
           go-golang-org-x-oauth2
           ;go-google-golang-org-api-cloudkms-v1
           ;go-google-golang-org-api-option
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/viant/toolbox")
    (synopsis "Utility library for Golang")
    (description
     "This package provides set of utilities/abstractions developed as part of
datastore connectivity and testing (viant/dsc, viant/dsunit).")
    (license license:asl2.0)))

(define-public go-github-com-viant-toolbox-bootstrap
  (hidden-package
   (package
     (inherit go-github-com-viant-toolbox)
     (arguments
      (list #:tests? #f
            #:import-path "github.com/viant/toolbox"
            #:phases
            #~(modify-phases %standard-phases
                (delete 'build))))
     (propagated-inputs
      (list go-github-com-go-errors-errors
            go-github-com-pkg-errors
            go-gopkg-in-yaml-v2)))))

(define-public go-github-com-viant-xreflect
  (package
    (name "go-github-com-viant-xreflect")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/viant/xreflect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r5y3cbs8z9frgfqrq2yv77z43fv36s7jrk49prpbfsskgpk8rh2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; to break import cycle
      #:import-path "github.com/viant/xreflect"))
    (propagated-inputs
     (list go-golang-org-x-mod))
    (home-page "https://github.com/viant/xreflect")
    (synopsis "Golang reflection extension")
    (description
     "This package provides a reflection extension.")
    (license license:unlicense)))

(define-public go-github-com-viant-xunsafe
  (package
    (name "go-github-com-viant-xunsafe")
    (version "0.10.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/viant/xunsafe")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q9zqz6p8spf7nq9r75yv9zizxf80mg0i6w0y9a0qxcgpnji0a3z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/viant/xunsafe"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-viant-xreflect))
    (home-page "https://github.com/viant/xunsafe")
    (synopsis "Faster Golang reflection")
    (description
     "This package provides a implementation reflection that greatly improved
performance, that is between 25 to 50x time faster than native one.")
    (license license:asl2.0)))

(define-public go-github-com-vito-midterm
  (package
    (name "go-github-com-vito-midterm")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vito/midterm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bvgw84750xfpm89hrab7pzfv1d5dy94igiqwzk5ivy4yca90ipw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/vito/midterm"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-charmbracelet-bubbletea
           go-github-com-creack-pty
           go-github-com-muesli-termenv
           go-github-com-sebdah-goldie-v2
           go-golang-org-x-term))
    (home-page "https://github.com/vito/midterm")
    (synopsis "In-memory terminal emulator")
    (description
     "This package implements an in-memory terminal emulator, designed to be
used as a component within a larger application for displaying logs, running
interactive shells, or rendering terminal output.")
    (license license:expat)))

(define-public go-github-com-vitrun-qart
  (let ((commit "bf64b92db6b05651d6c25a3dabf2d543b360c0aa")
        (revision "0"))
    (package
      (name "go-github-com-vitrun-qart")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vitrun/qart")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xk7qki703xmay9ghi3kq2bjf1iw9dz8wik55739d6i7sn77vvkc"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/vitrun/qart"))
      (home-page "https://github.com/vitrun/qart")
      (synopsis "Create QR codes with an embedded image")
      (description
       "This package provides a library for embedding human-meaningful
graphics in QR codes.  However, instead of scribbling on redundant pieces and
relying on error correction to preserve the meaning, @code{qart} engineers the
encoded values to create the picture in a code with no inherent errors.")
      (license license:bsd-3))))

(define-public go-github-com-vividcortex-ewma
  (package
    (name "go-github-com-vividcortex-ewma")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VividCortex/ewma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0whx516l9nm4n41spagb605ry7kfnz1qha96mcshnfjlahhnnylq"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/VividCortex/ewma"))
    (home-page "https://github.com/VividCortex/ewma")
    (synopsis "Exponentially Weighted Moving Average algorithms for Go")
    (description
     "This package implements algorithms for
@url{https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average,exponentially
weighted moving averages}.")
    (license license:expat)))

(define-public go-github-com-vmihailenco-msgpack-v4
  (package
    (name "go-github-com-vmihailenco-msgpack-v4")
    (version "4.3.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/vmihailenco/msgpack")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08jdq8wh6i1f2avl46l5ndfrvhfl4l276hz4y1xq6agwbwrvycdy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/vmihailenco/msgpack/v4"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (native-inputs
     (list go-gopkg-in-check-v1))
    (propagated-inputs
     (list go-github-com-vmihailenco-tagparser))
    (home-page "https://github.com/vmihailenco/msgpack")
    (synopsis "MessagePack encoding for Golang")
    (description
     "This package provides implementation of MessagePack encoding for Go
programming language.")
    (license license:bsd-2)))

(define-public go-github-com-vmihailenco-msgpack-v5
  (package
    (inherit go-github-com-vmihailenco-msgpack-v4)
    (name "go-github-com-vmihailenco-msgpack-v5")
    (version "5.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmihailenco/msgpack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vkyyywvip0vwjmnmnmkl9hz345k54nigj2mq8fbbjx96xpgghvz"))))
    (arguments
     (list
      #:import-path "github.com/vmihailenco/msgpack/v5"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-vmihailenco-tagparser
           go-google-golang-org-appengine))))

(define-public go-github-com-vmihailenco-tagparser
  (package
    (name "go-github-com-vmihailenco-tagparser")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmihailenco/tagparser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13arliaz3b4bja9jj7cr5ax4zvxaxm484fwrn0q6d6jjm1l35m1k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/vmihailenco/tagparser"))
    (home-page "https://github.com/vmihailenco/tagparser")
    (synopsis "Tag parser for Golang")
    (description
     "This package is a simple Golang implementation of tag parser.")
    (license license:bsd-2)))

(define-public go-github-com-wader-gojq
  ;; This is a modified fork only for fq.
  (hidden-package
   (package
     (name "go-github-com-wader-gojq")
     (version "0.12.1-0.20250208151254-0aa7b87b2c2b")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/wader/gojq")
              (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1byil5r5nzs6fx0si3ipanq1c8vcqbsr0rhyd5380vp7zr5j9cxl"))))
     (build-system go-build-system)
     (arguments
      (list
       #:import-path "github.com/wader/gojq"))
     (native-inputs
      (list go-github-com-google-go-cmp))
     (propagated-inputs
      (list go-github-com-itchyny-timefmt-go
            go-github-com-mattn-go-isatty
            go-github-com-mattn-go-runewidth
            go-gopkg-in-yaml-v3))
     (home-page "https://github.com/wader/gojq")
     (synopsis "Pure Go implementation of jq")
     (description
      "Package gojq provides the parser and the interpreter of gojq.  Please refer to
@url{https://github.com/itchyny/gojq#usage-as-a-library, Usage as a library} for
introduction. It's fork of github.com/itchyny/gojq, see github.com/wader/gojq fq
branc.")
     (license license:expat))))

(define-public go-github-com-wadey-gocovmerge
  (package
    (name "go-github-com-wadey-gocovmerge")
    (version "0.0.0-20160331181800-b5bfa59ec0ad")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wadey/gocovmerge")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00m7kxcmmw0l9z0m7z6ii06n5j4bcrxqjbhxjbfzmsdgdsvkic31"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/wadey/gocovmerge"))
    (propagated-inputs (list go-golang-org-x-tools))
    (home-page "https://github.com/wadey/gocovmerge")
    (synopsis "Merge coverprofile results from multiple go cover runs")
    (description
     "gocovmerge takes the results from multiple @command{go test -coverprofile} runs and
merges them into one profile.")
    (license license:bsd-2)))

(define-public go-github-com-wangjia184-sortedset
  (package
    (name "go-github-com-wangjia184-sortedset")
    (version "0.0.0-20220209072355-af6d6d227aa7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wangjia184/sortedset")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15y8n0m5s723jifh01487sp2jn067jiaizp8w4z965vbn066hh8n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/wangjia184/sortedset"))
    (home-page "https://github.com/wangjia184/sortedset")
    (synopsis "Sorted Set in Golang")
    (description
     "Package sortedset provides the data-struct allowing a fast access the
element in set by key or by score(order).  It is inspired by Sorted Set from
Redis.")
    (license license:bsd-2)))

(define-public go-github-com-warpfork-go-fsx
  (package
    (name "go-github-com-warpfork-go-fsx")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/warpfork/go-fsx")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yhh42vp12hnkhlfimcab4a2df2apprnlg3ll75yr2pd0b001p5b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/warpfork/go-fsx"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/warpfork/go-fsx")
    (synopsis "Extended filesystem interface for Golang")
    (description
     "This package, @code{fsx}, takes the style of the io/fs package, and
extends it with more features:
@itemize
@item fsx provides the ability to write files (using OpenFile, which is much
like the os package feature you're already familiar with)
@item fsx provides the ability to create directories
@item fsx provides the ability to delete files and directories
@item fsx provides features for reading symlinks, and creating them
@end itemize")
    ;; This library is multiply-licensed under either of Apache 2.0 or MIT or
    ;; BSD-3-Clause terms.
    (license (list license:expat license:asl2.0 license:bsd-3))))

(define-public go-github-com-whyrusleeping-base32
  (package
    (name "go-github-com-whyrusleeping-base32")
    (version "0.0.0-20170828182744-c30ac30633cc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/base32")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "060jj8j9rnm3m47vv7jfz9ddybch3ryvn1p9vhc63bqn73knalhf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/whyrusleeping/base32"))
    (home-page "https://github.com/whyrusleeping/base32")
    (synopsis "BASE32 encoding package from go with NoPadding option")
    (description
     "This package provides a base32 encoding package from go with NoPadding
option.")
    ;; No license provided, see
    ;; <https://github.com/whyrusleeping/base32/issues/5>
    (license  license:public-domain)))

(define-public go-github-com-whyrusleeping-go-keyspace
  (package
    (name "go-github-com-whyrusleeping-go-keyspace")
    (version "0.0.0-20160322163242-5b898ac5add1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/go-keyspace")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fkk7i7qxwbz1g621mm6a6inb69lr57cyc9ayyfiwhnjwfz78rbb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/whyrusleeping/go-keyspace"))
    (home-page "https://github.com/whyrusleeping/go-keyspace")
    (synopsis "Comparing key metrics within a given keyspace")
    (description
     "This is a package extracted from @code{go-ipfs}.  Its purpose to be used
to compare a set of keys based on a given metric.  The primary metric used is
XOR, as in kademlia.")
    (license license:expat)))

(define-public go-github-com-whyrusleeping-go-sysinfo
  (package
    (name "go-github-com-whyrusleeping-go-sysinfo")
    (version "0.0.0-20190219211824-4a357d4b90b1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/go-sysinfo")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s6yjp9incc579wbbga33vq0hcanv8j2xh9l90ya0r4fihz39jiq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/whyrusleeping/go-sysinfo"))
    (propagated-inputs
     (list go-github-com-dustin-go-humanize))
    (home-page "https://github.com/whyrusleeping/go-sysinfo")
    (synopsis "Package to extract system information")
    ;; There is not much information provided by the project, see
    ;; <https://github.com/whyrusleeping/go-sysinfo/issues>.
    (description
     "This package provides a basic system stats like @code{DiskUsage} and
@code{MemoryInfo}.")
    (license license:expat)))

(define-public go-github-com-workiva-go-datastructures
  (package
    (name "go-github-com-workiva-go-datastructures")
    (version "1.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Workiva/go-datastructures")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09k5zg0ma8z5bcfwk3viccaxzrrk5pyfhk5hkr9x34vgcnlk0jx7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Workiva/go-datastructures"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs (list go-github-com-tinylib-msgp))
    (home-page "https://github.com/Workiva/go-datastructures")
    (synopsis "Collection of Go data structures")
    (description
     "@code{go-datastructures} is a collection of useful, performant, and
thread-safe Go data structures.

It includes:
@itemize
@item Augmented Tree - Interval tree for collision in n-dimensional
ranges.  Implemented via a red-black augmented tree.

@item Bitarray - Bitarray used to detect existence without having to resort to
hashing with hashmaps.  Requires entities have a uint64 unique identifier.  Two
implementations exist, regular and sparse.

@item Futures - A helpful tool to send a @emph{broadcast} message to
listeners.

@item Queue - Package contains both a normal and priority queue.  Both
implementations never block on send and grow as much as necessary.

@item Fibonacci Heap - A standard Fibonacci heap providing the usual
operations.  Can be useful in executing Dijkstra or Prim's algorithms in the
theoretically minimal time.

@item Range Tree - Useful to determine if n-dimensional points fall within an
n-dimensional range.

@item Set - Set implementation which accepts items of type @code{interface{}}
and includes only a few methods.

@item Threadsafe - A package that is meant to contain some commonly used items
but in a threadsafe way.

@item AVL Tree - This is an example of a branch copy immutable
@acronym{Adelson-Velsky and Landis,AVL} @acronym{Balanced Binary Search
Trees,BBST}.

@item X-Fast Trie - An interesting design that treats integers as words and
uses a trie structure to reduce time complexities by matching prefixes.

@item Y-Fast Trie - An extension of the X-Fast trie in which an X-Fast trie is
combined with some other ordered data structure to reduce space consumption
and improve CRUD types of operations.

@item Fast Integer Hashmap - A datastructure used for checking existence but
without knowing the bounds of your data.  If you have a limited small bounds,
the bitarray package might be a better choice.

@item Skiplist - An ordered structure that provides amortized logarithmic
operations but without the complication of rotations that are required by
BSTs.

@item Sort - The sort package implements a multithreaded bucket sort that can
be up to 3x faster than the native Golang sort package.

@item Numerics - Early work on some nonlinear optimization problems.  The
initial implementation allows a simple use case with either linear or
nonlinear constraints.

@item B+ Tree - Initial implementation of a B+ tree.  Delete method still
needs added as well as some performance optimization.

@item Immutable B Tree - A btree based on two principles, immutability and
concurrency.

@item Ctrie - A concurrent, lock-free hash array mapped trie with efficient
non-blocking snapshots.

@item Dtrie - A persistent hash trie that dynamically expands or shrinks to
provide efficient memory allocation.

@item Persistent List - A persistent, immutable linked list.

@item Simple Graph - A mutable, non-persistent undirected graph where parallel
edges and self-loops are not permitted.
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-xaionaro-go-bytesextra
  (package
    (name "go-github-com-xaionaro-go-bytesextra")
    (version "0.0.0-20220103144954-846e454ddea9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xaionaro-go/bytesextra")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zggis1mdrfnashngg5hc6a6r6glcxzgj87ali7qdi02bnrl8s9i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xaionaro-go/bytesextra"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/xaionaro-go/bytesextra")
    (synopsis "Golang library for bytes I/O")
    (description
     "This package provides @code{io.ReadWriteSeeker} implementation for
@code{[]byte}.")
    (license license:cc0)))

(define-public go-github-com-xaionaro-go-unsafetools
  (package
    (name "go-github-com-xaionaro-go-unsafetools")
    (version "0.0.0-20241024014258-a46e1ce3763e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xaionaro-go/unsafetools")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "153qlgq5sjg3gsw5v6an0mck8v5pmxf5mnys41ykp97yqp53mgcp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xaionaro-go/unsafetools"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/xaionaro-go/unsafetools")
    (synopsis "Access to private/unexported fields of a structure")
    (description
     "This package provides function @code{FieldByName} to access to any field
(including private/unexported) of a structure.")
    (license license:cc0)))

(define-public go-github-com-xaionaro-gosrc
  (let ((commit "3fdf8476a735bd7ddd90579862e2e548787b17b6")
        (revision "0"))
    (package
      (name "go-github-com-xaionaro-gosrc")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xaionaro-go/gosrc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0j0y0r0nj39537x7yk9yfc3k6dqbw14b65ykjnbmsxkhpjjpxf3v"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/xaionaro-go/gosrc"))
      (native-inputs
       (list go-github-com-stretchr-testify))
      (propagated-inputs
       (list go-github-com-xaionaro-go-unsafetools
             go-github-com-fatih-structtag))
      (home-page "https://github.com/xaionaro-go/gosrc")
      (synopsis "Parse Go packages to handy structures")
      (description
       "This package just simplifies working with @code{go/*} packages to parse
a source code. Initially the package was written to simplify code
generation.")
      (license license:cc0))))

(define-public go-github-com-xdg-go-stringprep
  (package
    (name "go-github-com-xdg-go-stringprep")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xdg-go/stringprep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xvaf2xiy34ra4xz75d6kylr29jv00dnyk14mh8kmq5692n8lqvb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xdg-go/stringprep"))
    (propagated-inputs
     (list go-golang-org-x-text))
    (home-page "https://github.com/xdg-go/stringprep")
    (synopsis "Go implementation of RFC-3454 stringprep and RFC-4013 SASLprep")
    (description
     "Package stringprep provides data tables and algorithms for RFC-3454,
including errata.  It also provides a profile for SASLprep as defined in
RFC-4013.")
    (license license:asl2.0)))

(define-public go-github-com-xhit-go-str2duration-v2
  (package
    (name "go-github-com-xhit-go-str2duration-v2")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xhit/go-str2duration")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c9zi9mfy5ww413y1jpfh1rdis43lvd5v6gvajqzh4q1km9lyxjj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xhit/go-str2duration/v2"))
    (home-page "https://github.com/xhit/go-str2duration")
    (synopsis "Convert string to duration in golang")
    (description
     "This package provides a means to obtain @code{time.Duration} from a
string.  The string can be a string retorned for @code{time.Duration} or a
similar string with weeks or days too.")
    (license license:bsd-3)))

(define-public go-github-com-xlab-treeprint
  (package
    (name "go-github-com-xlab-treeprint")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xlab/treeprint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00k1xj3rwqa5f6n9fks8bh1m7vzssdjwxnam9kzy0rlbc74lgkl3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xlab/treeprint"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/xlab/treeprint")
    (synopsis "ASCII tree composing tool")
    (description
     "This package provides a simple ASCII tree composing tool.")
    (license license:expat)))

(define-public go-github-com-xo-terminfo
  (package
    (name "go-github-com-xo-terminfo")
    (version "0.0.0-20220910002029-abceb7e1c41e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xo/terminfo")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n3b37z76rz3l74mhrvviz66xa8dqwpvc2gb6cyzql5smbcs9y3a"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/xo/terminfo"))
    (native-inputs
     (list go-golang-org-x-exp))
    (home-page "https://github.com/xo/terminfo")
    (synopsis "Read the terminfo database in Go")
    (description
     "The terminfo package implements terminfo database reading for Go.")
    (license license:expat)))

(define-public go-github-com-xrash-smetrics
  (package
    (name "go-github-com-xrash-smetrics")
    (version "0.0.0-20240521201337-686a1a2994c1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xrash/smetrics")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "057wgbz16n416zp23j5iv2lsd0nidbd92r4f1h8s3c1svkkqvk0a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xrash/smetrics"))
    (home-page "https://github.com/xrash/smetrics")
    (synopsis "String metrics library")
    (description
     "Package smetrics provides a bunch of algorithms for calculating the
distance between strings. There are implementations for calculating the
popular Levenshtein distance (aka Edit Distance or Wagner-Fischer), as well as
the Jaro distance, the Jaro-Winkler distance, and more.")
    (license license:expat)))

(define-public go-github-com-xtgo-uuid
  (package
    (name "go-github-com-xtgo-uuid")
    (version "0.0.0-20140804021211-a0b114877d4c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xtgo/uuid")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10pmay90is5x8cv5ckcajw3s7g2rpk4ix6kl4qhq8qx05x2ivlrw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xtgo/uuid"))
    (home-page "https://github.com/xtgo/uuid")
    (synopsis "Go UUID parsing and generation")
    (description
     "Package uuid can be used to generate and parse universally unique
identifiers, a standardized format in the form of a 128 bit number.")
    (license license:bsd-3)))

(define-public go-github-com-xuanwo-go-locale
  (package
    (name "go-github-com-xuanwo-go-locale")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Xuanwo/go-locale")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09d68ay4kcic82xjdl9b3zi5nq0srxanprk5p32n5yxqirb0pbxd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Xuanwo/go-locale"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-golang-org-x-text))
    (home-page "https://github.com/Xuanwo/go-locale")
    (synopsis "Locale detection in Golang")
    (description
     "This package provides a functionality for cross-platform locale
detection.")
    (license license:asl2.0)))

(define-public go-github-com-yargevad-filepathx
  (package
    (name "go-github-com-yargevad-filepathx")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yargevad/filepathx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fcrzx3h9lxfhqiy85815m65djn64pgfyqrdcr1c0k6axvs49g2s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yargevad/filepathx"))
    (home-page "https://github.com/yargevad/filepathx")
    (synopsis "Double-star support to Golang's std @code{path/filepath}")
    (description
     "Package filepathx adds double-star globbing support to the Glob function
from the core path/filepath package.  You might recognize \"**\" recursive
globs from things like your .gitignore file, and zsh.  The \"**\" glob
represents a recursive wildcard matching zero-or-more directory levels deep.")
    (license license:expat)))

(define-public go-github-com-yookoala-realpath
  (let ((commit "d19ef9c409d9817c1e685775e53d361b03eabbc8")
        (revision "0"))
    (package
      (name "go-github-com-yookoala-realpath")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yookoala/realpath")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qvz1dcdldf53rq69fli76z5k1vr7prx9ds1d5rpzgs68kwn40nw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/yookoala/realpath"))
      (home-page "https://github.com/yookoala/realpath")
      (synopsis "@code{realpath} for Golang")
      (description
       "This package provides @code{realpath}, a Go module that when provided
with a valid relative path / alias path, it will return you with a string of
its real absolute path in the system.")
      (license license:expat))))

;; XXX: The latest release v0.1.1 was in 2014, master branch has more changes
;; since that time, use the latest commit.
(define-public go-github-com-yosuke-furukawa-json5
  (let ((commit "cf7bb3f354ffe5d5ad4c9b714895eab7e0498b5f")
        (revision "0"))
    (package
      (name "go-github-com-yosuke-furukawa-json5")
      (version (git-version "0.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yosuke-furukawa/json5")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "05h9ipfjr2ww8b89zq1sm1q9wfasjnmzwlxa241wppqajn3rvr7s"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/yosuke-furukawa/json5"))
      (home-page "https://github.com/yosuke-furukawa/json5")
      (synopsis "JSON5 implemented in Golang")
      (description
       "This package provides an implementation of
@url{https://github.com/yosuke-furukawa/json5, JSON5}.")
      (license license:bsd-3))))

(define-public go-github-com-yudai-gojsondiff
  (package
    (name "go-github-com-yudai-gojsondiff")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yudai/gojsondiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qnymi0027mb8kxm24mmd22bvjrdkc56c7f4q3lbdf93x1vxbbc2"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yudai/gojsondiff"
      #:test-subdirs #~(list "formatter/...")))
    (native-inputs
     (list go-github-com-onsi-ginkgo))
    (propagated-inputs
     (list go-github-com-sergi-go-diff
           go-github-com-yudai-golcs))
    (home-page "https://github.com/yudai/gojsondiff")
    (synopsis "JSON Diff and Patch for Golang")
    (description
     "This package implements a \"diff\" algorithm that compares two JSON
objects and generates deltas that describe the differences between them.  The
deltas can be applied to a JSON object to \"patch\" them.")
    (license license:expat)))

(define-public go-github-com-yudai-golcs
  (package
    (name "go-github-com-yudai-golcs")
    (version "0.0.0-20170316035057-ecda9a501e82")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yudai/golcs")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mx6wc5fz05yhvg03vvps93bc5mw4vnng98fhmixd47385qb29pq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yudai/golcs"))
    (home-page "https://github.com/yudai/golcs")
    (synopsis "Calculate @acronym{LCS, longest common sequence} in Golang")
    (description
     "This package provides functions to calculate @acronym{LCS, longest
common sequence} values from two arbitrary arrays.")
    (license license:expat)))

(define-public go-github-com-yuin-gopher-lua
  (package
    (name "go-github-com-yuin-gopher-lua")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/gopher-lua")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bvmd6kywbwzcpdqmmk6gjzrc2x4q24q1p25si4sm0s18kfqnmap"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/yuin/gopher-lua"
      #:phases
      #~(modify-phases %standard-phases
          ;; FIXME: "ls" needs to be substituted in _glua-tests/issues.lua and
          ;; _lua5.1-tests/files.lua with full path, but attempt was failed:
          ;; Throw to key `decoding-error' with args `("peek-char" "input
          ;; decoding error" 84 #<input: _lua5.1-tests/files.lua 11>)'.
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "script_test.go"
                  ((".issues.lua.*") "")
                  ((".files.lua.*") ""))
                (for-each delete-file
                          (list "_glua-tests/issues.lua"
                                "_lua5.1-tests/files.lua"))))))))
    (propagated-inputs
     (list go-github-com-chzyer-readline))
    (home-page "https://github.com/yuin/gopher-lua")
    (synopsis "VM and compiler for Lua in Golang")
    (description
     "GopherLua is a Lua5.1(+ goto statement in Lua5.2) VM and compiler.  It
provides Go APIs that allow you to easily embed a scripting language to your
Go host programs.")
    (license license:expat)))

(define-public go-github-com-zalando-go-keyring
  (package
    (name "go-github-com-zalando-go-keyring")
    (version "0.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zalando/go-keyring")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gavcs0k2wnw0q7zgcdhwca1phqls70wb93j2bdmjlvmrq9na6f4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zalando/go-keyring"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "dbus-run-session" "--"
                          "go" "test" "-v"
                          "-skip" (string-join
                                   ;; Disable tests which require a system
                                   ;; DBus instance.
                                   (list "TestDelete"
                                         "TestDeleteAll"
                                         "TestDeleteAllEmptyService"
                                         "TestDeleteNonExisting"
                                         "TestGet"
                                         "TestGetMultiLine"
                                         "TestGetNonExisting"
                                         "TestGetSingleLineHex"
                                         "TestGetUmlaut"
                                         "TestSet")
                                   "|")
                          "./..."))))))))
    (native-inputs
     (list dbus))
    (propagated-inputs
     (list go-github-com-godbus-dbus-v5))
    (home-page "https://github.com/zalando/go-keyring/")
    (synopsis "Library for working with system keyring")
    (description
     "@code{go-keyring} is a library for setting, getting and deleting secrets
from the system keyring.")
    (license license:expat)))

(define-public go-github-com-zclconf-go-cty
  (package
    (name "go-github-com-zclconf-go-cty")
    (version "1.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zclconf/go-cty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rvvs9rplvh9gv90lsvwgw203ysz47qksw1xz3whhdvhn7ns8s8f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/zclconf/go-cty"
      #:test-flags
      ;; Tests fail on non 64bit systems: unexpected error: invalid index:
      ;; value must be a whole number, between -2147483648 and 2147483647.
      #~(list #$@(if (not (target-64bit?)) '("-skip" "TestElement") '()))))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-apparentlymart-go-textseg-v15
           go-github-com-vmihailenco-msgpack-v5
           go-golang-org-x-text))
    (home-page "https://github.com/zclconf/go-cty")
    (synopsis "Type system for dynamic values in Go applications")
    (description
     "@code{cty} (pronounced \"see-tie\") is a dynamic type system for
applications written in Go that need to represent user-supplied values without
losing type information.  The primary intended use is for implementing
configuration languages, but other uses may be possible too.")
    (license license:expat)))

(define-public go-github-com-zclconf-go-cty-debug
  (package
    (name "go-github-com-zclconf-go-cty-debug")
    (version "0.0.0-20240509010212-0d6042c53940")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zclconf/go-cty-debug")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0izkqnciir10zamr1ca32bmyir1d5hfyxh84dj2q39bw1sj3fs7j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/zclconf/go-cty-debug"))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-zclconf-go-cty))
    (home-page "https://github.com/zclconf/go-cty-debug")
    (synopsis "Debugging and inspection utilities for cty")
    (description
     "This package implements a functionality for debugging and inspection
utilities for cty Golang module.")
    (license license:expat)))

(define-public go-github-com-zclconf-go-cty-yaml
  (package
    (name "go-github-com-zclconf-go-cty-yaml")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zclconf/go-cty-yaml")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wwfwrf77rwxi39ln8mhdwg2d2znqz109yksac9x0x9jhczmxbvf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zclconf/go-cty-yaml"))
    (propagated-inputs (list go-github-com-zclconf-go-cty))
    (home-page "https://github.com/zclconf/go-cty-yaml")
    (synopsis "YAML marshalling and unmarshalling for go-cty")
    (description
     "Package yaml can marshal and unmarshal cty values in YAML format.")
    (license license:asl2.0)))

(define-public go-github-com-zitadel-logging
  (package
    (name "go-github-com-zitadel-logging")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zitadel/logging")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04rrp9dhda78z8nidahhix1d2cn26fybk1vqmvnb89savadxqzzw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zitadel/logging"))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-gopkg-in-yaml-v2))
    (propagated-inputs
     (list go-github-com-sirupsen-logrus))
    (home-page "https://github.com/zitadel/logging")
    (synopsis "Logging extension for Golang")
    (description "This package implements an alternative logging extension.")
    (license license:asl2.0)))

(define-public go-github-com-zitadel-schema
  (package
    (name "go-github-com-zitadel-schema")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zitadel/schema")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nvxlqzz12ahhf6cmdy0gjfx8z30in0xamzk9ydag3ddgdqr5370"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zitadel/schema"))
    (home-page "https://github.com/zitadel/schema")
    (synopsis "Fills a struct with form values")
    (description
     "Package @code{zitadel/schema} converts structs to and from form values.
It is a maintained fork of @url{gorilla/schema,
https://github.com/gorilla/schema}")
    (license license:bsd-3)))

(define-public go-github-com-zyedidia-clipper
  (package
    (name "go-github-com-zyedidia-clipper")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zyedidia/clipper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18fv2cdll1d7d5wxs6r7kkhmk60pziiw3iy7knmdbcbhrk9rg112"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zyedidia/clipper"))
    (home-page "https://github.com/zyedidia/clipper")
    (synopsis "Clipboard access in Golang")
    (description
     "This package provides a cross-platform clipboard library.

Platforms supported:
@itemize
@item Linux via @code{xclip} or @code{xsel} or @code{wl-copy/wl-paste}
@item MacOS via @code{pbcopy/pbpaste}
@item Windows via the Windows clipboard API
@item WSL via clip.exe/powershell.exe
@item Android Termux via @code{termux-clipboard-set/termux-clipboard-get}
@item Plan9 via @code{/dev/snarf}
@item Anything else via a user-defined script
@end itemize")
    (license (list license:expat license:bsd-3))))

(define-public go-github-com-zyedidia-glob
  (package
    (name "go-github-com-zyedidia-glob")
    (version "0.0.0-20170209203856-dd4023a66dc3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zyedidia/glob")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vqw4xbqq6j8p5m7mwxvb448w69vjvgzx0ndsfzdh2cxfirwp3y7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zyedidia/glob"))
    (home-page "https://github.com/zyedidia/glob")
    (synopsis "String globbing in Go")
    (description
     "Package glob provides objects for matching strings with globs.")
    (license license:expat)))

;; For micro@2.0.14
(define-public go-github-com-zyedidia-go-runewidth
  (hidden-package
   (package
     (inherit go-github-com-mattn-go-runewidth)
     (name "go-github-com-zyedidia-go-runewidth")
     (version "0.0.12")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/zyedidia/go-runewidth")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "18gv5fkd69v8bwngj6r5zc572vyd1qhafz1wi3d7ynz3w0mmq85c"))))
     (arguments
      (list
       #:import-path "github.com/zyedidia/go-runewidth"))
     (home-page "https://github.com/zyedidia/go-runewidth")
     (description
      "It's an alternative fork of
@url{https://github.com/mattn/go-runewidth}."))))

(define-public go-github-com-zyedidia-go-shellquote
  (package
    (name "go-github-com-zyedidia-go-shellquote")
    (version "0.0.0-20200613203517-eccd813c0655")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zyedidia/go-shellquote")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jxjj60kicpzc6i7vigg0i8iwnhf6jawcalq201a5wkxnkmdlw9g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kballard/go-shellquote"))
    (home-page "https://github.com/zyedidia/go-shellquote")
    (synopsis "Go utilities for performing shell-like word splitting/joining")
    (description
     "Shellquote provides utilities for joining/splitting strings using sh's
word-splitting rules.

It's an alternative fork of @url{https://github.com/kballard/go-shellquote}.")
    (license license:expat)))

(define-public go-github-com-zyedidia-json5
  (package
    (name "go-github-com-zyedidia-json5")
    (version "0.0.0-20200102012142-2da050b1a98d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zyedidia/json5")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sgydazf3npr788b4w17ydmlh3fd1zmpriv9b69967ww90ckh2kz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zyedidia/json5"))
    (home-page "https://github.com/zyedidia/json5")
    (synopsis "Go JSON5 decoder package based on encoding/json")
    (description
     "This is a Go package that implements decoding of
@url{https://github.com/json5/json5, JSON5}.

It's an alternative fork of @url{https://github.com/titanous/json5}.")
    (license (list license:expat license:bsd-3))))

(define-public go-github-com-zyedidia-poller
  (package
    (name "go-github-com-zyedidia-poller")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zyedidia/poller")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10cjrqfk1j0l55bdbpm7kv4mqc665pngc8avai0p9chq03y2654g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zyedidia/poller"))
    (home-page "https://github.com/zyedidia/poller")
    (synopsis "File-descriptor multiplexer based on epoll(7)")
    (description
     "Package poller is a file-descriptor multiplexer.  It allows concurrent
Read and Write operations from and to multiple file-descriptors without
allocating one OS thread for every blocked operation.  It operates similarly
to Go's netpoller (which multiplexes network connections) without requiring
special support from the Go runtime.  It can be used with tty devices,
character devices, pipes, FIFOs, and any file-descriptor that is poll-able,
can be used with select(2), epoll(7), etc.

In addition, package poller allows the user to set timeouts (deadlines) for
read and write operations, and also allows for safe cancellation of blocked
read and write operations; a Close from another go-routine safely cancels
ongoing (blocked) read and write operations.

It's an active fork of @url{https://github.com/npat-efault/poller}.")
    (license license:bsd-2)))

;; For micro@2.0.14
(define-public go-github-com-zyedidia-tcell-v2
  (hidden-package
   (package
     (inherit go-github-com-gdamore-tcell-v2)
     (name "go-github-com-zyedidia-tcell-v2")
     (version "2.0.10")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/zyedidia/tcell")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1rbivmy79sc8hnygj7b3axhiqgbx6xc4f28pz69nhq9w2skk3zb9"))))
     (arguments
      (list
       #:import-path "github.com/zyedidia/tcell/v2"))
     (propagated-inputs
      (list go-github-com-gdamore-encoding
            go-github-com-lucasb-eyer-go-colorful
            go-github-com-mattn-go-runewidth
            go-github-com-xo-terminfo
            go-github-com-zyedidia-poller
            go-golang-org-x-sys
            go-golang-org-x-text))
     (home-page "https://github.com/zyedidia/tcell")
     (description
      "It's an alternative fork of @url{https://github.com/gdamore/tcell}."))))

(define-public go-github-com-zyedidia-terminal
  (package
    (name "go-github-com-zyedidia-terminal")
    (version "0.0.0-20230315200948-4b3bcf6dddef")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zyedidia/terminal")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lqplkpllv63msf7sp8igrhvkrnr8l8hz4v5daliyn4qwvgs3k63"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zyedidia/terminal"))
    (propagated-inputs (list go-github-com-creack-pty))
    (home-page "https://github.com/zyedidia/terminal")
    (synopsis "Package terminal is a vt10x terminal emulation backend")
    (description
     "Package terminal is a vt10x terminal emulation backend, influenced
largely by @code{st}, @code{rxvt}, @code{xterm}, and @code{iTerm} as
reference.  Use it for terminal muxing, a terminal emulation frontend, or
wherever else you need terminal emulation.

It's an active fork of @url{https://github.com/james4k/terminal}.")
    (license license:expat)))

(define-public go-gitlab-com-ambrevar-damerau
  (let ((commit "883829e1f25fad54015772ea663e69017cf22352")
        (revision "0"))
    (package
      (name "go-gitlab-com-ambrevar-damerau")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://gitlab.com/ambrevar/damerau")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1b9p8fypc914ij1afn6ir346zsgfqrc5mqc1k3d53n4snypq27qv"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "gitlab.com/ambrevar/damerau"))
      (home-page "https://gitlab.com/ambrevar/damerau")
      (synopsis "Damerau-Levenshtein distance for Golang")
      (description
       "This is a spelling corrector implementing the Damerau-Levenshtein
distance.  Takes a string value input from the user.  Looks for an identical
word on a list of words, if none is found, look for a similar word.")
      (license license:expat))))

(define-public go-go-abhg-dev-container-ring
  ;; The latest verion 0.3.0 was released in 2023, use the latest commit.
  (let ((commit "5feb657d1370c86f039188acc39afdc16172de0e")
        (revision "0"))
    (package
      (name "go-go-abhg-dev-container-ring")
      (version (git-version "0.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/abhinav/ring-go")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0fcqxrf7jzf0682fhw6ly10m3b1sm2b7pnmcmbxg7zn7kjw91353"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "go.abhg.dev/container/ring"))
      (native-inputs
       (list go-github-com-stretchr-testify))
      (propagated-inputs
       (list go-pgregory-net-rapid))
      (home-page "https://go.abhg.dev/container/ring")
      (synopsis "FIFO queue backed by a ring buffer.")
      (description
       "Package ring implements a FIFO queue backed by a ring buffer.")
      (license license:expat))))

(define-public go-go-abhg-dev-io-ioutil
  (package
    (name "go-go-abhg-dev-io-ioutil")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abhinav/ioutil-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sckmmpabnv9myccnjjrdr3an8zkzvcpwssx40x2z60kivd4zvv2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.abhg.dev/io/ioutil"))
    (home-page "https://go.abhg.dev/io/ioutil")
    (synopsis "Extensions for the Golang std @code{io} package")
    (description
     "Package ioutil contains extensions for the @code{io} package.")
    (license license:bsd-3)))

(define-public go-go-abhg-dev-komplete
  (package
    (name "go-go-abhg-dev-komplete")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abhinav/komplete")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "060ns34mcf3a39vqairv52lkxiknrv6ghpyy0prliswfhw0hwy67"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.abhg.dev/komplete"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-alecthomas-kong
           go-github-com-buildkite-shellwords))
    (home-page "https://go.abhg.dev/komplete")
    (synopsis "Shell completion support for Kong CLI parser")
    (description
     "This package provides a command-line completion engine for the
@url{https://github.com/alecthomas/kong, Kong CLI parser}.")
    (license license:bsd-3)))

(define-public go-go-abhg-dev-log-silog
  (package
    (name "go-go-abhg-dev-log-silog")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/abhinav/silog-go")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1287i6qqg7hp8g49mbbr4n0l67v5b3dqz263ynpqjzrz1yck5q3v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.abhg.dev/log/silog"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-charmbracelet-lipgloss
           go-github-com-muesli-termenv))
    (home-page "https://go.abhg.dev/log/silog")
    (synopsis "Colorful log/slog handler for CLI applications ")
    (description
     "Package silog provides a @code{slog.Handler} implementation that
produces human-readable, logfmt-style output.")
    (license license:bsd-3)))

(define-public go-go-etcd-io-bbolt
  (package
    (name "go-go-etcd-io-bbolt")
    (version "1.3.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etcd-io/bbolt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16s2l1yjn55rgybc9k8kh88zg7z8igm10y1xmx2qx1a147k64d31"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Extending the test timeout to 30 minutes still times out on aarch64.
      #:tests? (not target-arm?)
      #:import-path "go.etcd.io/bbolt"))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-go-etcd-io-gofail
           go-golang-org-x-sync))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://go.etcd.io/bbolt")
    (synopsis "Embedded key/value database for Go")
    (description
     "Bolt is a pure Go key/value store inspired by Howard Chu's LMDB project.
The goal of the project is to provide a simple, fast, and reliable database
for projects that don't require a full database server such as Postgres or
MySQL.")
    (license license:expat)))

(define-public go-go-lsp-dev-jsonrpc2
  (package
    (name "go-go-lsp-dev-jsonrpc2")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-language-server/jsonrpc2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mx7h0bak0kr3v18yqaqiq6ya9paw6lv3vqf30k55jsmwmrx5647"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.lsp.dev/jsonrpc2"))
    (propagated-inputs (list go-github-com-segmentio-encoding))
    (home-page "https://go.lsp.dev/jsonrpc2")
    (synopsis "JSON-RPC 2 Go library")
    (description "The @code{jsonrpc2F} package is an implementation of the
JSON-RPC 2 specification for Go.")
    (license license:bsd-3)))

(define-public go-go-lsp-dev-pkg
  (package
    (name "go-go-lsp-dev-pkg")
    (version "0.0.0-20210717090340-384b27a52fb2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-language-server/pkg")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n6mskf5g4m1h6hc12rwl622mn21a695kk7f2ldb5hdmlwib852g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "go.lsp.dev/pkg"))
    (home-page "https://go.lsp.dev/pkg")
    (synopsis "Library for the Go Language Server project")
    (description
     "Collection of Go modules for the Go Language Server project.")
    (license license:bsd-3)))

(define-public go-go-lsp-dev-protocol
  (package
    (name "go-go-lsp-dev-protocol")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-language-server/protocol")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14n0s7bs4xcsdp8m7fq9ridrh2nxsh5l80wg6xprgsr984dicpr8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.lsp.dev/protocol"))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-segmentio-encoding
           go-go-lsp-dev-jsonrpc2
           go-go-lsp-dev-pkg
           go-go-lsp-dev-uri
           go-go-uber-org-zap))
    (home-page "https://go.lsp.dev/protocol")
    (synopsis "Language Server Protocol (LSP) library for Go")
    (description
     "The @code{protocol} package implements the Language Server
Protocol (LSP) specification in Go.")
    (license license:bsd-3)))

(define-public go-go-lsp-dev-uri
  (package
    (name "go-go-lsp-dev-uri")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-language-server/uri")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mz1jnbf46rzs3iy8a601rdfmify6x56jsw2wsjvmyczn4vz8qwc"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "go.lsp.dev/uri"))
    (propagated-inputs (list go-github-com-google-go-cmp))
    (home-page "https://go.lsp.dev/uri")
    (synopsis "Go library for URI (uniform resource identifier)")
    (description
     "The @code{uri} package implements the URI Uniform Resource
Identifier (RFC3986) specification in Go.")
    (license license:bsd-3)))

(define-public go-go-mau-fi-util
  (package
    (name "go-go-mau-fi-util")
    (version "0.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mautrix/go-util")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zzkrbwgbxfppsxjck8qgj4xxzpiq25sx4p3zwjh6s1yz3kfb97p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.mau.fi/util"
      ;; dial tcp: lookup raw.githubusercontent.com on [::1]:53: read udp
      ;; [::1]:58818->[::1]:53: read: connection refused
      #:test-flags #~(list "-skip" "TestAdd_Full|TestFullyQualify_Full")))
    (native-inputs
     (list go-github-com-data-dog-go-sqlmock
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-mattn-go-sqlite3
           go-github-com-petermattis-goid
           go-github-com-rs-zerolog
           go-golang-org-x-exp
           go-golang-org-x-sys
           go-golang-org-x-text
           go-google-golang-org-protobuf
           go-gopkg-in-yaml-v3))
    (home-page "https://go.mau.fi/util")
    (synopsis "Golang utilities used by mautrix-go and bridges")
    (description
     "This package provides various Go utilities used by mautrix-go, bridges
written in Go, as well as some other related libraries like whatsmeow.")
    (license license:mpl2.0)))

(define-public go-go-mau-fi-webp
  (package
    (name "go-go-mau-fi-webp")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tulir/webp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g2m162kmnz1d2zpmbgja330q1p9ygdryllfzm71zr7d5pr9fc13"))
       (modules '((guix build utils)))
       ;; FIXME: The project indludes a copy of libwebp
       ;; (internal/libwebp-1.5.0) which is availalbe in Guix, find out how to
       ;; build it with it's source.
       (snippet
        #~(begin
            ;; Remove files which were auto generated by 'go generate'.
            (for-each delete-file
                      (find-files "." "^z_libwebp_src_.*\\.c$"))
            ;; An Apple user was here (mem).
            (for-each delete-file
                      (find-files "^\\.DS_Store$"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.mau.fi/webp"
      ;; reader_test.go:34: image: unknown format
      #:test-flags #~(list "-skip" "TestDecode")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "bench"))))
          (add-after 'unpack 'go-generate
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "go" "generate")))))))
    (propagated-inputs (list go-golang-org-x-image))
    (home-page "https://go.mau.fi/webp")
    (synopsis "WebP decoder and encoder for Golang")
    (description
     "Package webp implements a decoder and encoder for
@code{https://en.wikipedia.org/wiki/WebP, WebP} images.  It's a maintained
fork of @code{github.com/chai2010/webp}.")
    (license license:bsd-3)))

(define-public go-go-mau-fi-zeroconfig
  (package
    (name "go-go-mau-fi-zeroconfig")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tulir/zeroconfig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zi5jbrlvsmpiq6ph8mh2360f5chdpy69ykrlmycbm5518wigz2c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.mau.fi/zeroconfig"))
    (propagated-inputs
     (list go-github-com-rs-zerolog
           go-github-com-stretchr-testify
           go-gopkg-in-natefinch-lumberjack-v2))
    (home-page "https://go.mau.fi/zeroconfig")
    (synopsis "Declarative config format for zerolog")
    (description
     "This package provides a relatively simple declarative config format for
@url{https://github.com/rs/zerolog, zerolog} supporting configuration files
written in YAML or JSON.")
    (license license:mpl2.0)))

(define-public go-go-mongodb-org-mongo-driver
  (package
    (name "go-go-mongodb-org-mongo-driver")
    (version "1.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mongodb/mongo-go-driver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "160hwrk8y8h3nl9sh5v6pxnlyw1ywbssjgzb72lj0x68akgl8gff"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.mongodb.org/mongo-driver"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Some tests require running database and available network
               ;; connection.
               (list "TestAggregate"
                     "TestPollSRVRecords"
                     "TestPollSRVRecordsServiceName"
                     "TestPollingSRVRecordsLoadBalanced"
                     "TestPollingSRVRecordsSpec"
                     "TestServerHeartbeatOffTimeout"
                     "TestServerHeartbeatTimeout"
                     "TestTimeCodec"
                     "TestTopologyConstructionLogging"
                     "TestURIOptionsSpec")
               "|"))
      #:test-subdirs
      #~(list "bson/..." "event/..." "internal/..." "tag/..." "x/...")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build) ; no go files in project's root
          (add-after 'unpack 'remove-examples-and-benchmarks
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "benchmark"
                                "examples"
                                "cmd/godriver-benchmark"))))))))
    (native-inputs
     (list go-github-com-aws-aws-lambda-go))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-golang-snappy
           go-github-com-google-go-cmp
           go-github-com-klauspost-compress
           go-github-com-montanaflynn-stats
           go-github-com-xdg-go-scram
           go-github-com-xdg-go-stringprep
           go-github-com-youmark-pkcs8
           go-golang-org-x-crypto
           go-golang-org-x-sync))
    (home-page "https://go.mongodb.org/mongo-driver")
    (synopsis "MongoDB Go Driver")
    (description
     "This package provides a driver for @code{Mongo} data base.")
    (license license:asl2.0)))

(define-public go-go-senan-xyz-flagconf
  (package
    (name "go-go-senan-xyz-flagconf")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sentriz/flagconf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rms7hj1cdi5gfyhf1am1f8c4lq9ll4ashqi87yc6aq93gqgkag0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.senan.xyz/flagconf"))
    (propagated-inputs
     (list go-github-com-rogpeppe-go-internal))
    (home-page "https://go.senan.xyz/flagconf")
    (synopsis "Extensions to Go's flag package")
    (description
     "Flagconf provides extensions to Go's flag package to support prefixed
environment variables and a simple config file format.")
    (license license:expat)))

(define-public go-go-uber-org-atomic
  (package
    (name "go-go-uber-org-atomic")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/atomic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jdp4gfv9jx7dd066wzj7cj6wsk7fb5q04is42xn2wyp80sw5k3p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.uber.org/atomic"))
    (native-inputs
     (list go-github-com-stretchr-testify go-github-com-davecgh-go-spew))
    (home-page "https://pkg.go.dev/go.uber.org/atomic")
    (synopsis "Wrapper types for sync/atomic")
    (description
     "This package provides simple wrappers for primitive types to enforce
atomic access.")
    (license license:expat)))

(define-public go-go-uber-org-automaxprocs
  (package
    (name "go-go-uber-org-automaxprocs")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/automaxprocs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03arxcfaj7k6iwfdk0liaynxf9rjfj9m5glsjp7ws01xjkgrdpbc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.uber.org/automaxprocs"))
    (native-inputs (list go-github-com-stretchr-testify
                         go-github-com-prashantv-gostub))
    (home-page "https://github.com/uber-go/automaxprocs")
    (synopsis "CPU-count detection library for Go")
    (description
     "This package automatically set GOMAXPROCS to match Linux container
CPU quota.")
    (license license:expat)))

(define-public go-go-uber-org-dig
  (package
    (name "go-go-uber-org-dig")
    (version "1.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/dig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wmd4l3nn1mkv5kkx2xylk1fbkpp5a5f21fzavmc1rxlbfhfqvpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.uber.org/dig"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://pkg.go.dev/go.uber.org/dig")
    (synopsis "Reflection based dependency injection toolkit for Golang")
    (description
     "Package @code{dig} provides a functionality to implement resolving
object dependencies graph during the process startup.")
    (license license:expat)))

(define-public go-go-uber-org-fx
  (package
    (name "go-go-uber-org-fx")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/fx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03m26d2k9gwcdjjmwajz1fnihdjvb46l6c5ws07r8l8skcz1jx16"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - go.uber.org/fx/tools
            (delete-file-recursively "tools")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.uber.org/fx"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Tests require networking set up.
                       (list "TestNopLoggerOptionString"
                             "TestRun"
                             "TestDeepStack")
                       "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-go-uber-org-dig
           go-go-uber-org-goleak
           go-go-uber-org-multierr
           go-go-uber-org-zap
           go-golang-org-x-sys))
    (home-page "https://pkg.go.dev/go.uber.org/fx")
    (synopsis "Dependency injection based application framework for Golang")
    (description
     "Package @code{fx} is a framework that makes it easy to build
applications out of reusable, composable modules.")
    (license license:expat)))

(define-public go-go-uber-org-multierr
  (package
    (name "go-go-uber-org-multierr")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/multierr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s6skad93nbsq4b0sy5dsgacrdh2kzg0p8wjlhvff49vasqsi3qx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.uber.org/multierr"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-go-uber-org-atomic))
    (home-page "https://pkg.go.dev/go.uber.org/multierr")
    (synopsis "Error combination for Go")
    (description
     "@code{multierr} allows combining one or more Go errors together.")
    (license license:expat)))

(define-public go-go-uber-org-ratelimit
  (package
    (name "go-go-uber-org-ratelimit")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/ratelimit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12q3a1k88ifff8rnp3sk1bk730iln0gqmx32966a8w19yrgwrcdj"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - go.uber.org/ratelimit/tools
            (delete-file-recursively "tools")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.uber.org/ratelimit"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Tests are shaky, see
               ;; <https://github.com/uber-go/ratelimit/issues/128>.
               (list "TestInitial/With_Slack"
                     "TestDelayedRateLimiter/atomic"
                     "TestInitial/Without_Slack"
                     "TestPer/atomic"
                     "TestSlack/no_option,_defaults_to_10,_with_per/atomic"
                     "TestSlack/no_option,_defaults_to_10,_with_per/mutex"
                     "TestSlack/no_option,_defaults_to_10/atomic"
                     "TestSlack/slack_of_10,_like_default,_with_per/atomic"
                     "TestSlack/slack_of_10,_like_default/atomic"
                     "TestSlack/slack_of_150,_with_per/atomic"
                     "TestSlack/slack_of_150/atomic"
                     "TestSlack/slack_of_20,_with_per/atomic"
                     "TestSlack/slack_of_20/atomic")
               "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-benbjohnson-clock
           go-go-uber-org-atomic))
    (home-page "https://github.com/uber-go/ratelimit")
    (synopsis "Blocking leaky-bucket rate limit implementation in Golang")
    (description
     "This package implements the
@url{https://en.wikipedia.org/wiki/Leaky_bucket, leaky-bucket rate limit
algorithm}.  It refills the bucket based on the time elapsed between requests
instead of requiring an interval clock to fill the bucket discretely.")
    (license license:expat)))

(define-public go-go-uber-org-zap
  (package
    (name "go-go-uber-org-zap")
    (version "1.27.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/zap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h3ml2wqmdxwqv0xdfqvf7l4wma5yd0hdfsa6189mmbhkhzn8v3m"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - go.uber.org/zap/assets
            ;; - go.uber.org/zap/benchmarks
            ;; - go.uber.org/zap/exp
            ;; - go.uber.org/zap/tools
            ;; - go.uber.org/zap/zapgrpc/internal/test
            (for-each delete-file-recursively
                      (list "assets"
                            "benchmarks"
                            "exp"
                            "tools"
                            "zapgrpc/internal/test"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.uber.org/zap"
      #:test-flags
      ;; Failed to run 'go mod download'
      #~(list "-skip" "TestStacktraceFiltersVendorZap")))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-go-uber-org-goleak))
    (propagated-inputs
     (list go-go-uber-org-multierr
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/uber-go/zap")
    (synopsis "Structured and leveled logging in Golang")
    (description
     "This package implements a reflection-free, zero-allocation JSON encoder,
and the base Logger strives to avoid serialization overhead and allocations
wherever possible.  By building the high-level @code{SugaredLogger} on that
foundation, zap lets users choose when they need to count every allocation and
when they'd prefer a more familiar, loosely typed API.")
    (license license:expat)))

(define-public go-go-uber-org-zap-exp
  (package
    (name "go-go-uber-org-zap-exp")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/uber-go/zap")
              (commit (go-version->git-ref version
                                           #:subdir "exp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05i15278swdmpif3p6g18sy0sn7rnfdl3m2rj5p30cnyb0j29vig"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            ;; Consider to implement it as re-usable procedure in
            ;; guix/build/utils or guix/build-system/go.
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "exp")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "go.uber.org/zap/exp"
      #:unpack-path "go.uber.org/zap"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-go-uber-org-zap))
    (home-page "https://github.com/uber-go/zap")
    (synopsis "Experemental modules for Uber's Zap")
    (description
     "This package providies two additional libraries for go.uber.org/zap:

@itemize
@item @code{zapslog} implements @code{slog.Handler} which writes to the
supplied @code{zapcore.Core}
@item @code{zapfield} implements experimental @code{zap.Field} helpers whose
APIs may be unstable
@end itemize")
    (license license:expat)))

(define-public go-go-yaml-in-yaml-v2
  (package
    (name "go-go-yaml-in-yaml-v2")
    (version "2.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yaml/go-yaml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w0psni3i34l2lmdaiqgij2a8h7yy82d14fr18cb7kgmsxci2bx0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.yaml.in/yaml/v2"))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://github.com/yaml/go-yaml")
    (synopsis "YAML Support for the Go Language")
    (description
     "This packages is fork of @url{https://github.com/go-yaml/yaml,
gopkg.in/yaml.v2} maintained by @url{https://github.com/yaml/, YAML
organization}.")
    (license license:asl2.0)))

(define-public go-go-yaml-in-yaml-v3
  (package
    (inherit go-go-yaml-in-yaml-v2)
    (name "go-go-yaml-in-yaml-v3")
    (version "3.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yaml/go-yaml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03hdf855gm8bh8wnhxibin8y626ahaij6v5ivkmic2z86f48ah9n"))))
    (arguments
     (list
      #:import-path "go.yaml.in/yaml/v3"))))

(define-public go-go4-org
  ;; No release or version tag, Golang pseudo version:
  ;; 0.0.0-20230225012048-214862532bf5.
  (let ((commit "214862532bf518db360e7ecc2b8a94b66a10c176")
        (revision "0"))
    (package
      (name "go-go4-org")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/go4org/go4")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0cfgb3jjmqmxcb7n46kk4i8pqqda0fhxgs1wqmgicxqj46ny8bpk"))))
      (build-system go-build-system)
      (arguments
       (list
        #:go go-1.23
        #:skip-build? #t
        #:import-path "go4.org"
        #:test-subdirs
        #~(list "bytereplacer/..."
                ;; "cloud/..." ; missing packages
                "ctxutil/..."
                "errorutil/..."
                "fault/..."
                "jsonconfig/..."
                "legal/..."
                "lock/..."
                "media/..."
                "must/..."
                "net/..."
                "net/throttle/..."
                "oauthutil/..."
                "osutil/..."
                "readerutil/..."
                "reflectutil/..."
                "rollsum/..."
                "sort/..."
                "strutil/..."
                "syncutil/..."
                "testing/..."
                "testing/functest/..."
                "types/..."
                ;; "wkfs/..." ; missing packages
                "writerutil/..."
                "xdgdir/...")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'remove-examples
              (lambda* (#:key import-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" import-path)
                  (for-each delete-file
                            (find-files "." "example.*_test\\.go$"))))))))
      (propagated-inputs
       (list ;; go-cloud-google-com-go
             ;; go-cloud-google-com-go-storage
             go-github-com-rwcarlsen-goexif
             go-golang-org-x-net
             go-golang-org-x-oauth2
             go-golang-org-x-sys
             #; go-google-golang-org-api))
      (home-page "https://github.com/go4org/go4")
      (synopsis "Collection of packages for Go programmers")
      (description
       "This package provides a collection various self sufficient Golang
sub-packages:

@itemize
@item @code{bytereplacer} - provides a utility for replacing parts of byte
@item @code{ctxutil} -  contains golang.org/x/net/context related utilities
@item @code{errorutil} -  helps make better error messages
@item @code{fault} - handles fault injection for testing
@item @code{jsonconfig} - defines a helper type for JSON objects to be used
for configuration
@item @code{legal} -  provides in-process storage for compiled-in licenses
@item @code{lock} - provides a file locking library
@item @code{media/heif} - reads HEIF containers, as found in Apple HEIC/HEVC
images
@item @code{must} - contains helpers that panic on failure
@item @code{net/throttle} - provides a @code{net.Listener} that returns
artificially-delayed connections for testing real-world connectivity slices
@item @code{oauthutil} - contains OAuth 2 related utilities
@item @code{osutil} -  contains os level functions
@item @code{readerutil} - contains @code{io.Reader} types
@item @code{readerutil} - provides and operates on @code{io.Readers}
@item @code{reflectutil} - contains @code{reflect} utilities
@item @code{rollsum} - implements rolling checksums similar to apenwarr's bup,
which is similar to librsync
@item @code{sort} - provides primitives for sorting slices and user-defined
collections
@item @code{strutil} - contains string and byte processing functions
@item @code{syncutil/singleflight} - provides a duplicate function call
suppression mechanism
@item @code{syncutil/syncdebug} - contains facilities for debugging
synchronization problems
@item @code{syncutil} - provides various synchronization utilities
@item @code{testing/functest} - contains utilities to ease writing
table-driven tests for pure functions and method
@item @code{types} - provides various common types
@item @code{wkfs} - implements the pluggable well-known filesystem abstraction
layer
@item @code{writerutil} - contains {io.Writer} types
@item @code{xdgdir} - implements the Free Desktop Base Directory specification
for locating directories
@end itemize")
      (license (list license:asl2.0 license:bsd-3)))))

(define-public go-golang-org-rainycape-unidecode
  (let ((commit "cb7f23ec59bec0d61b19c56cd88cee3d0cc1870c")
        (revision "1"))
    (package
      (name "go-golang-org-rainycape-unidecode")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rainycape/unidecode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wvzdijd640blwkgmw6h09frkfa04kcpdq87n2zh2ymj1dzla5v5"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "golang.org/rainycape/unidecode"))
      (home-page "https://github.com/rainycape/unidecode")
      (synopsis "Unicode transliterator in Golang")
      (description
       "Unicode transliterator in Golang - Replaces non-ASCII characters with
their ASCII approximations.")
      (license license:asl2.0))))

(define-public go-golang-org-x-perf
  (package
    (name "go-golang-org-x-perf")
    (version "0.0.0-20250515181355-8f5f3abfb71a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/perf")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01qby8hvyamacndkavij7kk0dp95q3irssj4krpb7ppqwpq4j7l3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "golang.org/x/perf"))
    (propagated-inputs
     (list ;; go-cloud-google-com-go-storage
           go-github-com-aclements-go-gg
           go-github-com-aclements-go-moremath
           go-github-com-go-sql-driver-mysql
           go-github-com-google-safehtml
           ;; go-github-com-googlecloudplatform-cloudsql-proxy
           go-github-com-mattn-go-sqlite3
           go-golang-org-x-net
           go-golang-org-x-oauth2
           ;; go-gonum-org-v1-plot
           ;; go-google-golang-org-api
           go-google-golang-org-appengine))
    (home-page "https://cs.opensource.google/go/x/perf")
    (synopsis "Golang benchmark analysis tools and libraries")
    (description
     "This package provides tooling and utility libraries for perfoming and
reading benchmarks results.

@itemize
@item @code{benchfmt} - reads and writes the Go benchmark format
@item @code{benchunit} - manipulates benchmark units and formats numbers in
those units
@item @code{benchproc} - provides tools for filtering, grouping, and sorting
benchmark results
@item @code{benchmath} - provides tools for computing statistics over
distributions of benchmark measurements
@end itemize")
    (license license:bsd-3)))

(define-public go-google-golang-org-appengine
  (package
    (name "go-google-golang-org-appengine")
    (version "1.6.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/appengine")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17pbiximrd340wdx3pm1jkpsgvss80pax7hif906xglh7mj9zqx2"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - google.golang.org/appengine/v2
            (delete-file-recursively "v2")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "google.golang.org/appengine"))
    (propagated-inputs
     (list go-github-com-golang-protobuf
           go-golang-org-x-text
           go-google-golang-org-protobuf))
    (home-page "https://google.golang.org/appengine")
    (synopsis "Go App Engine packages")
    (description
     "Package appengine provides basic functionality for Google App Engine.")
    (license license:asl2.0)))

(define-public go-gopkg-in-alecthomas-kingpin-v2
  (package
    (inherit go-github-com-alecthomas-kingpin-v2)
    (name "go-gopkg-in-alecthomas-kingpin-v2")
    (arguments
     (list
      #:import-path "gopkg.in/alecthomas/kingpin.v2"))))

(define-public go-gopkg-in-inconshreveable-log15-v2
  (package
    (name "go-gopkg-in-inconshreveable-log15-v2")
    (version "2.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/inconshreveable/log15")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "117ivm1asxw2hlwb3zf72q553ywjk00bsn21bpwi99q784ghr4wd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/inconshreveable/log15.v2"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-import-path
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\.go$")
                  (("github.com/inconshreveable/log15") import-path))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-stack-stack
           go-github-com-mattn-go-colorable
           go-golang-org-x-term))
    (home-page "https://pkg.go.dev/github.com/inconshreveable/log15")
    (synopsis "Structured, composable logging for Golang")
    (description
     "This package provides a toolkit for logging that is both human and
machine readable.  It is modeled after the Go standard library's @code{io} and
@code{net/http} packages and is an alternative to the standard library's
@code{log} package.")
    (license license:asl2.0)))

(define-public go-gopkg-in-inconshreveable-log15-v3
  (package
    (inherit go-gopkg-in-inconshreveable-log15-v2)
    (name "go-gopkg-in-inconshreveable-log15-v3")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/inconshreveable/log15")
             (commit (string-append "v" version "-testing.5"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14b03w7mlsac0n5ig5qc0iggii6ig9mvbiv6656r1h19vh4kmx8x"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-gopkg-in-inconshreveable-log15-v2)
       ((#:import-path _) "gopkg.in/inconshreveable/log15.v3")))
    (propagated-inputs
     (list go-github-com-go-stack-stack
           go-github-com-mattn-go-colorable
           go-golang-org-x-term))))

(define-public go-gopkg-in-inf-v0
  (package
    (name "go-gopkg-in-inf-v0")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-inf/inf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00k5iqjcp371fllqxncv7jkf80hn1zww92zm78cclbcn4ybigkng"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/inf.v0"
      ;; Tests are not copatiblw with Go 1.24+.
      #:test-flags #~(list "-vet=off")))
    (home-page "https://github.com/go-inf/inf")
    (synopsis "Infinite precision decimal arithmetic in Golang")
    (description
     "This package (type @code{inf.Dec}) implements a \"infinite-precision\"
decimal arithmetic.")
    (license license:expat)))

(define-public go-gopkg-in-ini-v1
  (package
    (name "go-gopkg-in-ini-v1")
    (version "1.67.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ini/ini")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vpzkjmrwp7bqqsijp61293kk2vn6lcck56j8m5y6ks6cf21lpap"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/ini.v1"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://gopkg.in/ini.v1")
    (synopsis "Go library for ini files")
    (description "Go library for ini files")
    (license license:asl2.0)))

(define-public go-gopkg-in-natefinch-lumberjack-v2
  (package
    (name "go-gopkg-in-natefinch-lumberjack-v2")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/natefinch/lumberjack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1l3vlv72b7rfkpy1164kwd3qzrqmmjnb67akzxqp2mlvc66k6p3d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/natefinch/lumberjack.v2"))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/natefinch/lumberjack")
    (synopsis "Rolling logger for Go")
    (description
     "Lumberjack is a Go package for writing logs to rolling files.")
    (license license:expat)))

(define-public go-gopkg-in-op-go-logging-v1
  (package
    (inherit go-github-com-op-go-logging)
    (name "go-gopkg-in-op-go-logging-v1")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-op-go-logging)
       ((#:import-path _) "gopkg.in/op/go-logging.v1")))))

(define-public go-gopkg-in-tomb-v1
  (package
    (name "go-gopkg-in-tomb-v1")
    (version "1.0.0-20141024135613-dd632973f1e7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/tomb.v1")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lqmq1ag7s4b3gc3ddvr792c5xb5k6sfn0cchr3i2s7f1c231zjv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/tomb.v1"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-test
            (lambda* (#:key import-path #:allow-other-keys)
              (substitute* (string-append "src/" import-path "/tomb_test.go")
                (("t.Fatalf\\(`Killf\\(\"BO%s")
                 "t.Fatalf(`Killf(\"BO%%s")))))))
    (home-page "https://gopkg.in/tomb.v1")
    (synopsis "@code{tomb} handles clean goroutine tracking and termination")
    (description
     "The @code{tomb} package handles clean goroutine tracking and
termination.")
    (license license:bsd-3)))

(define-public go-gopkg-in-vmihailenco-msgpack-v2
  (package
    (name "go-gopkg-in-vmihailenco-msgpack-v2")
    (version "2.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmihailenco/msgpack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08rcdnshc252nmlxkswx8xgiwr2ry7cq806gcw1836b00hwlwmmz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/vmihailenco/msgpack.v2"
      #:test-flags #~(list "-skip" "TestTypes")))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://msgpack.uptrace.dev/")
    (synopsis "MessagePack encoding for Golang")
    (description
     "This package provides implementation of MessagePack encoding for Go
programming language.")
    (license license:bsd-2)))

(define-public go-gopkg-in-warnings-v0
  (package
    (name "go-gopkg-in-warnings-v0")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-warnings/warnings")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kzj50jn708cingn7a13c2wdlzs6qv89dr2h4zj8d09647vlnd81"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/warnings.v0"))
    (home-page "https://gopkg.in/warnings.v0")
    (synopsis "Error handling with non-fatal errors")
    (description
     "Package warnings implements error handling with non-fatal
errors (warnings).")
    (license license:bsd-2)))

(define-public go-gopkg-in-yaml-v2
  (package
    (name "go-gopkg-in-yaml-v2")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/yaml.v2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pbmrpj7gcws34g8vwna4i2nhm9p6235piww36436xhyaa10cldr"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; https://github.com/go-yaml/yaml/issues/441 and
            ;; https://github.com/go-yaml/yaml/pull/442
            ;; Don't assume 64-bit wide integers
            (substitute* "decode_test.go"
              (("bin: (-0b1000000000000000000000000000000000000000000000000000000000000000)" all number)
               (string-append "int64_min_base2: " number))
              (("map\\[string\\]interface\\{\\}\\{\"bin\": -9223372036854775808\\}")
               "map[string]int64{\"int64_min_base2\": math.MinInt64}"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/yaml.v2"))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://gopkg.in/yaml.v2")
    (synopsis "YAML reader and writer for the Go language")
    (description
     "This package provides a Go library for encode and decode YAML
values.")
    (license license:asl2.0)))

(define-public go-gopkg-in-yaml-v3
  (package
    (name "go-gopkg-in-yaml-v3")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/yaml.v3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01b0wjb7yzv8wzzz2iim8mjpkwjnykcanrwiq06pkl89lr6gv8hn"))
       (patches (search-patches "go-gopkg-in-yaml-v3-32bit.patch"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? (not (target-ppc32?))  ; Test killed with quit: ran too long (11m0s).
      #:import-path "gopkg.in/yaml.v3"))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://gopkg.in/yaml.v3")
    (synopsis "YAML reader and writer for the Go language")
    (description
     "This package provides a Go library for encode and decode YAML values.
The yaml package supports most of YAML 1.2, but preserves some behavior from
1.1 for backwards compatibility.")
    (license license:asl2.0)))

;; XXX: It's a source only variant to include in other packages as input (e.g
;; to build NNCP and remove vendor) dependency for Golang.  Full build depends
;; on Bazel <https://bazel.build/>.
(define-public go-gvisor-dev-gvisor
  (let ((commit "634ce95eed8d5b8e6b3e2ea30542c34cb30af148")
        (revision "0"))
    (package
      (name "go-gvisor-dev-gvisor")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/gvisor")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1l3si6qlchwgsvaq6lxyvj8iq8n8pqxlayb1hbdf3pynxi52hfkw"))))
      (build-system go-build-system)
      (arguments
       (list
        #:tests? #f
        #:skip-build? #t
        #:import-path "gvisor.dev/gvisor"))
      (propagated-inputs
       (list ;; go-github-com-bazelbuild-rules-go
             go-github-com-burntsushi-toml
             ;; go-github-com-cenkalti-backoff
             go-github-com-cilium-ebpf
             go-github-com-containerd-cgroups
             go-github-com-containerd-console
             ;; go-github-com-containerd-containerd
             go-github-com-containerd-fifo
             ;; go-github-com-containerd-go-runc
             go-github-com-containerd-typeurl
             go-github-com-coreos-go-systemd-v22
             go-github-com-godbus-dbus-v5
             go-github-com-gofrs-flock
             go-github-com-gogo-protobuf
             go-github-com-google-btree
             go-github-com-google-subcommands
             ;; go-github-com-kr-pty
             go-github-com-mattbaird-jsonpatch
             go-github-com-mohae-deepcopy
             go-github-com-opencontainers-runtime-spec
             go-github-com-sirupsen-logrus
             ;; go-github-com-syndtr-gocapability
             go-github-com-vishvananda-netlink
             go-golang-org-x-mod
             go-golang-org-x-sync
             go-golang-org-x-sys
             go-golang-org-x-time
             go-golang-org-x-tools
             go-google-golang-org-protobuf
             ;; go-k8s-io-api
             ;; go-k8s-io-apimachinery
             #; go-k8s-io-client-go))
      (home-page "https://gvisor.dev/gvisor")
      (synopsis "Application Kernel for Containers")
      (description
       "@code{gVisor} provides a strong layer of isolation between running
applications and the host operating system.  It is an application kernel that
implements a @url{https://en.wikipedia.org/wiki/Linux_kernel_interfaces,
Linux-like interface}.

gVisor includes an Open Container Initiative (OCI) runtime called runsc that
makes it easy to work with existing container tooling.  The runsc runtime
integrates with Docker and Kubernetes, making it simple to run sandboxed
containers.

This package provides the source only to include in other packages as
dependencies.")
      (license license:asl2.0))))

(define-public go-howett-net-plist
  (package
    (name "go-howett-net-plist")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DHowett/go-plist")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gcrxkmdj87xq01458asgxvvijrkih74ydbzfmir1p16xr9z0x39"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "howett.net/plist"
      ;; cmd requires gopkg.in/yaml.v1
      #:test-subdirs #~(list "internal/..." ".")))
    (propagated-inputs
     (list go-github-com-jessevdk-go-flags
           go-gopkg-in-check-v1))
    (home-page "https://github.com/DHowett/go-plist")
    (synopsis "Apple property list transcoder")
    (description
     "This list transcoder supports encoding/decoding property lists (Apple
XML, Apple Binary, OpenStep, and GNUStep) from/to arbitrary Go types.")
    (license license:giftware)))

(define-public go-k8s-io-apimachinery
  (package
    (name "go-k8s-io-apimachinery")
    (version "0.34.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kubernetes/apimachinery")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mdmw951k9a2v817c0xxlazvz2500lw80mh53xjwspss0yx9b8fb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "k8s.io/apimachinery"
      ;; TODO: Check why some tests fails in other subdirectories.
      #:test-subdirs #~(list "pkg/test/...")))
    (native-inputs
     (list go-github-com-armon-go-socks5
           go-github-com-stretchr-testify
           go-golang-org-x-time))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-fxamacker-cbor-v2
           go-github-com-gogo-protobuf
           go-github-com-google-gnostic-models
           go-github-com-google-go-cmp
           go-github-com-google-uuid
           go-github-com-moby-spdystream
           go-github-com-mxk-go-flowrate
           go-github-com-pmezard-go-difflib
           go-github-com-spf13-pflag
           go-golang-org-x-net
           go-gopkg-in-evanphx-json-patch-v4
           go-gopkg-in-inf-v0
           go-k8s-io-klog-v2
           go-k8s-io-kube-openapi
           go-k8s-io-utils
           go-sigs-k8s-io-json
           go-sigs-k8s-io-randfill
           go-sigs-k8s-io-structured-merge-diff-v6
           go-sigs-k8s-io-yaml))
    (home-page "https://k8s.io/apimachinery")
    (synopsis "Kubernetes and Kubernetes-like API utilities")
    (description
     "This package provides scheme, typing, encoding, decoding, and conversion
functions for Kubernetes and Kubernetes-like API objects.  It is a shared
dependency for servers and clients to work with Kubernetes API infrastructure
without direct type dependencies. Its first consumers are
@code{k8s.io/kubernetes}, @code{k8s.io/client-go}, and
@code{k8s.io/apiserver}.")
    (license license:asl2.0)))

(define-public go-k8s-io-gengo-v2
  (package
    (name "go-k8s-io-gengo-v2")
    (version "2.0.0-20250604051438-85fd79dbfd9f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/gengo")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "198nq5ndb5cawsrp90rab0m3vyhrnv7q3bxpqz7l926d15vqbvsd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "k8s.io/gengo/v2"
      #:unpack-path "k8s.io/gengo"
      #:test-flags
      ;; XXX: Figure out why these tests fail.
      #~(list "-skip" (string-join
                       (list "TestSnippetWriter"
                             "TestFindPackages"
                             "TestAlreadyLoaded"
                             "TestLoadPackagesInternal"
                             "TestLoadPackagesTo"
                             "TestUserRequestedPackages"
                             "TestAddOnePkgToUniverse"
                             "TestStructParse/generic"
                             "TestJSON")
                       "|"))))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-spf13-pflag
           go-golang-org-x-tools
           go-k8s-io-klog-v2))
    (home-page "https://github.com/kubernetes/gengo")
    (synopsis "Framework for building simple code generators")
    (description
     "This package implements a functionality for generating things based on
go files.  This mechanism was first used in Kubernetes code-generator and is
split out here for ease of reuse and maintainability.")
    (license license:asl2.0)))

(define-public go-k8s-io-klog-v2
  (package
    (name "go-k8s-io-klog-v2")
    (version "2.130.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/klog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12q9jhxfpq75sgmdxgcz85znbgdi04ic9zy3rm0c47n24clz6z73"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/klog/v2"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list "TestDestinationsWithDifferentFlags/with_log_file_only"
                     "TestDestinationsWithDifferentFlags/everything_disabled"
                     "TestDestinationsWithDifferentFlags/with_log_dir_only"
                     "TestDestinationsWithDifferentFlags/with_log_dir_only_and_one_output"
                     "TestDestinationsWithDifferentFlags/with_log_file_and_log_dir")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (propagated-inputs
     (list go-github-com-go-logr-logr))
    (home-page "https://github.com/kubernetes/klog")
    (synopsis "Leveled execution logs for Go")
    (description
     "Package klog implements logging analogous to the Google-internal C++
INFO/ERROR/V setup.  It provides functions @code{Info}, @code{Warning},
@code{Error}, @code{Fatal}, plus formatting variants such as @code{Infof}.  It
also provides V-style logging controlled by the @code{-v} and
@code{-vmodule=file=2} flags.  It's a is a permanent fork of
@code{https://github.com/golang/glog}.")
    (license license:asl2.0)))

(define-public go-k8s-io-utils
  (package
    (name "go-k8s-io-utils")
    (version "0.0.0-20241210054802-24370beab758")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/utils")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "158rsq780hrzv6zb7lycjg8b2lfd00nvmx3mn4pi9byjam2y7nds"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; XXX: all of them fail with various reasons
      #:import-path "k8s.io/utils"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-k8s-io-klog-v2))
    (home-page "https://github.com/kubernetes/utils")
    (synopsis "Utility libraries for Golang")
    (description
     "This package provides a set of libraries that implementing low-level,
kubernetes-independent packages supplementing the
@url{https://pkg.go.dev/std#stdlib,Go standard libs}.")
    (license license:asl2.0)))

(define-public go-kernel-org-pub-linux-libs-security-libcap-psx
  (package
    (name "go-kernel-org-pub-linux-libs-security-libcap-psx")
    (version "1.2.76")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/libs/libcap/libcap.git")
             (commit (go-version->git-ref version
                                          #:subdir "psx"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0va0bkq5kxf0ccsdpw598vsmk4kdzhaafjvym0g5b2n49c5sn59b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "kernel.org/pub/linux/libs/security/libcap/psx"
      #:unpack-path "kernel.org/pub/linux/libs/security/libcap"))
    ;; Redirects from <https://kernel.org/pub/linux/libs/security/libcap>
    (home-page "https://sites.google.com/site/fullycapable")
    (synopsis "API for invoking Linux system calls in Golang")
    (description
     "This package provides a support for system calls that are run
simultaneously on all threads under Linux.")
    (license license:gpl2)))

(define-public go-maunium-net-go-mauflag
  (package
    (name "go-maunium-net-go-mauflag")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tulir/mauflag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09jv1819jwq5i29y6ngf4j4ii6qwlshydvprfvsfplc419dkz1vx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "maunium.net/go/mauflag"))
    (home-page "https://maunium.net/go/mauflag")
    (synopsis "Extendable argument parser for Golang")
    (description
     "This package provides an extendable argument parser for Golang.  Mostly
follows the GNU
@@url{https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html,
Program Argument Syntax Conventions}.")
    (license license:gpl3)))

(define-public go-modernc-org-fileutil
  (package
    (name "go-modernc-org-fileutil")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/fileutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10q5xbik9yk9jw2ziq1fw0hpjfbv5h3qm7rlxlkwj0qxyyb7b9bi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/fileutil"))
    (propagated-inputs
     (list go-modernc-org-mathutil))
    (home-page "https://gitlab.com/cznic/fileutil")
    (synopsis "File utility functions")
    (description "Package fileutil collects some file utility functions.")
    (license license:bsd-3)))

(define-public go-modernc-org-lexer
  (package
    (name "go-modernc-org-lexer")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/lexer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00ahagg81vbm8k9an5l8gn8plr7d53955v54mv0g811hfqxacja2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/lexer"))
    (propagated-inputs
     (list go-golang-org-x-exp
           go-modernc-org-fileutil))
    (home-page "https://gitlab.com/cznic/lexer")
    (synopsis "Run time generator of action less scanners")
    (description
     "Package lexer provides generating actionless scanners (lexeme
recognizers) at run time.")
    (license license:bsd-3)))

(define-public go-modernc-org-memory
  (package
    (name "go-modernc-org-memory")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/memory")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "066pzk0i1jxahialzp97ra0k0f191y1756sgppiw50zkpnpwzjxr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/memory"))
    (propagated-inputs
     (list go-modernc-org-mathutil))
    (home-page "https://gitlab.com/cznic/memory")
    (synopsis "Memory allocator implementation")
    (description "Package memory implements a memory allocator.")
    (license license:bsd-3)))

(define-public go-modernc-org-opt
  (package
    (name "go-modernc-org-opt")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/opt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02jih8lgcyrm2v0vagdmq298rvhqkwi4pswg4nwlksdiayw356p5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/opt"))
    (home-page "https://gitlab.com/cznic/opt")
    (synopsis "Command-line flag parsing")
    (description "Package opt implements command-line flag parsing.")
    (license license:bsd-3)))

(define-public go-modernc-org-sortutil
  (package
    (name "go-modernc-org-sortutil")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/sortutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01s9vil8lvaz526x6q9f12h6vpc3jc8zvpag7knz1bdx9b15yljc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/sortutil"))
    (native-inputs
     (list go-modernc-org-mathutil))
    (home-page "https://gitlab.com/cznic/sortutil")
    (synopsis "Sort utility library")
    (description
     "Package sortutil provides utilities supplementing the standard
@code{sort} package.")
    (license license:bsd-3)))

(define-public go-modernc-org-strutil
  (package
    (name "go-modernc-org-strutil")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/strutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rk76c1n189hzg3kfab8pfvssa1h9v0vxk5jxy8pk32rqic0hdim"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/strutil"
      ;; Cannot determine import path using GOPATH.
      #:test-flags #~(list "-skip" "TestImportPath")))
    (native-inputs
     (list go-modernc-org-mathutil))
    (home-page "https://gitlab.com/cznic/strutil")
    (synopsis "Strings utility library for Golang")
    (description
     "Package strutil collects utils supplemental to the standard
@code{strings} package.")
    (license license:bsd-3)))

(define-public go-mvdan-cc-editorconfig
  (package
    (name "go-mvdan-cc-editorconfig")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mvdan/editorconfig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mi1cp6fyaknjn7smvaas4lj03fws5qib5vbi4mrz3qrmvmhh9l4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "mvdan.cc/editorconfig"))
    (native-inputs
     (list cmake-minimal))
    (home-page "https://github.com/mvdan/editorconfig")
    (synopsis "EditorConfig support in Go")
    (description
     "Package editorconfig allows parsing and using @code{EditorConfig} files, as
defined in @url{https://editorconfig.org/,https://editorconfig.org/}.")
    (license license:bsd-3)))

(define-public go-mvdan-cc-gofumpt
  (package
    (name "go-mvdan-cc-gofumpt")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mvdan/gofumpt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sz58av4jg0q26y1s4pznnvrzp1gi8brz9zw8aa46pzdmjw394wq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:build-flags
      ;; Gofumpt formats Go files, and therefore modifies them. To help the
      ;; developers diagnose issues, it replaces any occurrence of a
      ;; `//gofumpt:diagnose` comment with some debugging information which
      ;; includes the module version.
      #~(list (format #f "-ldflags=-X ~s"
                      (string-append "main.version=" #$version " (GNU Guix)")))
      #:import-path "mvdan.cc/gofumpt"
      #:skip-build? #t
      #:test-flags #~(list "-skip" "TestScript/diagnose")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-fallback-version
            ;; In the event gofumpt was built without module support, it falls
            ;; back to a string "(devel)". Since our build system does not yet
            ;; support modules, we'll inject our version string instead, since
            ;; this is more helpful.
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "internal/version/version.go"
                  (("^const fallbackVersion.+")
                   (format #f "const fallbackVersion = ~s~%"
                           (string-append #$version " (GNU Guix)"))))))))))
    (native-inputs
     (list go-github-com-go-quicktest-qt))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-rogpeppe-go-internal
           go-golang-org-x-mod
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-tools))
    (home-page "https://mvdan.cc/gofumpt/")
    (synopsis "Formats Go files with a stricter ruleset than gofmt")
    (description
     "Enforce a stricter format than @code{gofmt}, while being backwards
compatible.  That is, @code{gofumpt} is happy with a subset of the formats
that @code{gofmt} is happy with.")
    (license license:bsd-3)))

(define-public go-mvdan-cc-sh-v3
  (package
    (name "go-mvdan-cc-sh-v3")
    (version "3.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mvdan/sh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ryqrhdjvj0ll88fk6dh63a5hjl0rpww3x8kmzqfnf5r83jdz3sh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t ; we need just lib here
      #:import-path "mvdan.cc/sh/v3"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; XXX: Check why these tests fail.
                       (list "TestRunnerRun/#279"
                             "TestRunnerRun/#281"
                             "TestRunnerRun/#282"
                             "TestRunnerRun/#289"
                             "TestRunnerRun/#960"
                             "TestRunnerRun/#989"
                             "TestRunnerRun/#990"
                             "TestRunnerRun/#991"
                             "TestRunnerRunConfirm/#152"
                             "TestScript/flags")
                       "|"))))
    (native-inputs
     (list go-github-com-go-quicktest-qt
           go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-creack-pty
           go-github-com-google-renameio-v2
           go-github-com-muesli-cancelreader
           go-github-com-rogpeppe-go-internal
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-term
           go-mvdan-cc-editorconfig))
    (home-page "https://mvdan.cc/sh")
    (synopsis "Shell formatter with bash support")
    (description
     "This package provides a Golang library implementing a shell parser,
formatter, and interpreter with bash support.")
    (license license:bsd-3)))

(define-public go-ninefans-net-go
  ;; XXX: The package name in Guix uses 'ninefans' instead of '9fans' to
  ;; accomodate from a shortcoming of the go-build-system where the
  ;; `go-inputs' procedure in the `setup-go-environment' phase uses
  ;; `package-name->name+version', which returns 'go' as name for
  ;; go-9fans-net-go-acme, which gets removed from the results and thus
  ;; GOPATH.
  (package
    (inherit go-9fans-net-go)
    (name "go-ninefans-net-go")))

(define-public go-nullprogram-com-x-optparse
  (package
    (name "go-nullprogram-com-x-optparse")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeeto/optparse-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yzpzlhmxdm8gd8ikh9c91qmshjp1jg49l0qsslmm432wk19zym9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "nullprogram.com/x/optparse"))
    (home-page "https://nullprogram.com/x/optparse/")
    (synopsis "Traditional long option parser for Go")
    (description
     "Package optparse parses command line arguments very similarly to GNU
@code{getopt_long()}.  It supports long options and optional arguments, but
does not permute arguments.  It is intended as a replacement for Go's flag
package.")
    ;; License type does latterly says it' "UNLICENSE".
    (license license:unlicense)))

(define-public go-rsc-io-binaryregexp
  (package
    (name "go-rsc-io-binaryregexp")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsc/binaryregexp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kar0myy85waw418zslviwx8846zj0m9cmqkxjx0fvgjdi70nc4b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "rsc.io/binaryregexp"))
    (home-page "https://pkg.go.dev/rsc.io/binaryregexp")
    (synopsis "Golang regexp for binary/latin-1 data")
    (description
     "Package regexp implements regular expression search.  The syntax of the
regular expressions accepted is the same general syntax used by Perl, Python,
and other languages.  More precisely, it is the syntax accepted by RE2 and
described at https://golang.org/s/re2syntax, except for @code{\\C.}")
    (license license:bsd-3)))

(define-public go-rsc-io-goversion
  (package
    (name "go-rsc-io-goversion")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsc/goversion")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k5rskhl3653snrfwwv0a9ia3jf52gwkc6p4abj4xb04s23qczcs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "rsc.io/goversion"))
    (home-page "https://github.com/rsc/goversion")
    (synopsis "Print version used to build Go executables")
    (description
     "Goversion scans a directory tree and, for every executable it finds,
prints the Go version used to build that executable.")
    (license license:bsd-3)))

(define-public go-sigs-k8s-io-json
  (package
    (name "go-sigs-k8s-io-json")
    (version "0.0.0-20241014173422-cfa47c3a1cc8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes-sigs/json")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zdb3sq333ns8m5azv4fqqy62r0vrhssxl0przicm4v1nmrxj4jh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "sigs.k8s.io/json"))
    (home-page "https://github.com/kubernetes-sigs/json")
    (synopsis "JSON decoder with enchansed features")
    (description
     "This package provides case-sensitive, integer-preserving JSON
unmarshaling functions based on @code{encoding/json} @code{Unmarshal()}.")
    (license license:asl2.0)))

(define-public go-sigs-k8s-io-kustomize-cmd-config
  (package
    (name "go-sigs-k8s-io-kustomize-cmd-config")
    (version "0.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kubernetes-sigs/kustomize")
              (commit (go-version->git-ref version
                                           #:subdir "cmd/config"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12n8ij4gisah5mvxcgq263iic61gjpxdj3ml03826zckzn7wlv46"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            ;; Consider to implement it as re-usable procedure in
            ;; guix/build/utils or guix/build-system/go.
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "cmd")
            (delete-all-but "cmd" "config")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "sigs.k8s.io/kustomize/cmd/config"
      #:unpack-path "sigs.k8s.io/kustomize"
      ;; Full test suite requires Docker in the PATH.
      #:test-subdirs #~(list "internal/commands"
                             "runner/...")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-errors-errors
           go-github-com-spf13-cobra
           go-gopkg-in-inf-v0
           go-sigs-k8s-io-kustomize-kyaml))
    (home-page "https://sigs.k8s.io/kustomize")
    (synopsis "Kubernetes config filters")
    (description
     "This package implements a functionality to expose Kubernetes config
filters directly as CLI commands for the purposes of development of the
@code{kyaml} package and as a reference implementation for using the
libraries.")
    (license license:asl2.0)))

(define-public go-sigs-k8s-io-kustomize-kyaml
  (package
    (name "go-sigs-k8s-io-kustomize-kyaml")
    (version "0.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes-sigs/kustomize")
             (commit (go-version->git-ref version
                                          #:subdir "kyaml"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bpllzbaxvi93i74dw1z8k221ib2ydks0wmwx13vkh6cacrvydan"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "sigs.k8s.io/kustomize/kyaml"
      #:unpack-path "sigs.k8s.io/kustomize"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestCommandResultsChecker_UpdateExpectedFromActual"
                             "TestProcessorResultsChecker_UpdateExpectedFromActual")
                       "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-github-com-davecgh-go-spew))
    (propagated-inputs
     (list go-github-com-go-errors-errors
           go-github-com-google-gnostic-models
           go-github-com-google-go-cmp
           go-github-com-monochromegane-go-gitignore
           go-github-com-sergi-go-diff
           go-github-com-spf13-cobra
           go-github-com-xlab-treeprint
           go-golang-org-x-sys
           go-google-golang-org-protobuf
           go-k8s-io-kube-openapi
           go-sigs-k8s-io-yaml))
    (home-page "https://github.com/kubernetes-sigs/")
    (synopsis "Read Kubernetes config as YAML")
    (description
     "Package kyaml contains libraries for reading and writing Kubernetes Resource
configuration as YAML.")
    (license license:asl2.0)))

(define-public go-sigs-k8s-io-structured-merge-diff-v4
  (package
    (name "go-sigs-k8s-io-structured-merge-diff-v4")
    (version "4.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes-sigs/structured-merge-diff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "001h7lsnnglwj2nbbfhkmyfrym0y2dpwbfc6kqa3spl1dbl2lgac"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "sigs.k8s.io/structured-merge-diff/v4"))
    (native-inputs
     (list go-sigs-k8s-io-randfill))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-json-iterator-go
           go-sigs-k8s-io-yaml))
    (home-page "https://github.com/kubernetes-sigs/structured-merge-diff")
    (synopsis "Structured Merge and Diff")
    (description
     "This package provides a code which implements the Kubernetes \"apply\"
operation.")
    (license license:asl2.0)))

(define-public go-sigs-k8s-io-structured-merge-diff-v6
  (package
    (inherit go-sigs-k8s-io-structured-merge-diff-v4)
    (name "go-sigs-k8s-io-structured-merge-diff-v6")
    (version "6.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kubernetes-sigs/structured-merge-diff")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n7fx6l108ad7wmpx03zb7vzrzxwzs8fyhz0xxdr1vggsq5kdfn5"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-sigs-k8s-io-structured-merge-diff-v4)
       ((#:import-path _) "sigs.k8s.io/structured-merge-diff/v6")))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-json-iterator-go
           go-go-yaml-in-yaml-v2))))

(define-public go-sigs-k8s-io-yaml
  (package
    (name "go-sigs-k8s-io-yaml")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes-sigs/yaml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yjnmpwmvlb6md3djn6qx1ag4ld7gjz7jfyz1ldml88zyb9crpqx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "sigs.k8s.io/yaml"))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-gopkg-in-check-v1))
    (home-page "https://sigs.k8s.io/yaml")
    (synopsis "YAML marshaling and unmarshaling support for Go")
    (description
     "This package provides a Go library that first converts YAML to JSON
using @code{go-yaml} and then uses @code{json.Marshal} and
@code{json.Unmarshal} to convert to or from the struct.  This means that it
effectively reuses the JSON struct tags as well as the custom JSON methods
@code{MarshalJSON} and @code{UnmarshalJSON} unlike @code{go-yaml}.

kubernetes-sigs/yaml is a permanent fork of
@url{https://github.com/ghodss/yaml,ghodss/yaml}.")
    (license (list license:expat license:bsd-3))))

(define-public go-suah-dev-protect
  (package
    (name "go-suah-dev-protect")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/qbit/protect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f36w2435xivggl4d31813ggk37ya9v8q6w8ygbp35ypzwfgzlh3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "suah.dev/protect"))
    (home-page "https://codeberg.org/qbit/protect")
    (synopsis "Wrapper for OpenBSD's pledge and unveil system calls")
    (description
     "Package protect is a wrapper for OpenBSD's @code{pledge} and @code{unveil}
system calls.

It allows one to safely call Unveil / Pledge on non-OpenBSD operating systems.")
    (license license:isc)))

(define-public go-zgo-at-jfmt
  (package
    (name "go-zgo-at-jfmt")
    (version "0.0.0-20240726113937-e6436421fade")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/arp242/jfmt")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nc3n3lf0ixzpk85sadp2w4yg9v39pdb2z0i1rpxksdayax009wa"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "zgo.at/jfmt"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (propagated-inputs
     (list go-zgo-at-termtext
           go-zgo-at-zli
           go-zgo-at-zstd))
    (home-page "https://github.com/arp242/jfmt")
    (synopsis "JSON formatter written in Go")
    (description
     "@samp{jfmt} is a JSON formatter which tries to produce opinionated
output with more lines squashed into single one where possible (e.g. list,
brackets, ordering).")
    (license license:expat)))

(define-public go-zgo-at-runewidth
  (package
    (name "go-zgo-at-runewidth")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arp242/runewidth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17wbzwak831z04kj4xdvh2q78k3in3kay009n0yj8nlwn1p126ph"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "zgo.at/runewidth"))
    (home-page "https://github.com/arp242/runewidth")
    (synopsis "Get fixed width of the character or string")
    (description
     "@samp{runewidth} provides functions to get fixed width of the character
or string.  It is a fork of https://github.com/mattn/go-runewidth, updated to
the newest Unicode and having various helper functions removed, so all that
remains is just the @code{runewidth.RuneWidth()} function.")
    (license license:expat)))

(define-public go-zgo-at-termtext
  (package
    (name "go-zgo-at-termtext")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arp242/termtext")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0czhvcx6crmwfl6555l9hgl6gq21ykwr4bg2dqksc71qmv6b27hh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "zgo.at/termtext"))
    (propagated-inputs
     (list go-github-com-rivo-uniseg
           go-zgo-at-runewidth))
    (home-page "https://github.com/arp242/termtext")
    (synopsis "Deal with monospace text as interpreted by terminals")
    (description
     "Package @samp{termtext} deals with monospace text as interpreted by
terminals.")
    (license license:expat)))

(define-public go-zgo-at-zli
  (package
    (name "go-zgo-at-zli")
    (version "0.0.0-20250601161843-debde58580f1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/arp242/zli")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "120nlnxhzdmk1lh7cfgajkl85n8mfd00cn5csyislris9q2n2rxb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "zgo.at/zli"
      #:test-flags
      #~(list "-vet=off"))) ;Go@1.24 forces vet, but tests are not ready yet.
    (home-page "https://github.com/arp242/zli")
    (synopsis "Go library for writing command line interface programs")
    (description
     "@samp{zli} is a Go library for writing command line interface
programs.  It includes flag parsing, color escape codes, various
helpful utility functions, and makes testing fairly easy.")
    (license license:expat)))

(define-public go-zgo-at-zstd
  (package
    (name "go-zgo-at-zstd")
    (version "0.0.0-20250624130507-310bc16feb74")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arp242/zstd")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b8w6cl0z9wz32n8znj3d6il775gfbnw6k8x6cxki4acqhga8dd2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "zgo.at/zstd"
      #:test-flags #~(list "-skip" "TestExists/4")))
    (home-page "https://github.com/arp242/zstd")
    (synopsis "Extensions to Go's standard library")
    (description
     "Package @samp{zstd} is a collection of extensions to Go's standard
library.")
    (license license:expat)))

;;;
;;; Executables:
;;;

(define-public glua
  (package
    (inherit go-github-com-yuin-gopher-lua)
    (name "glua")
    (arguments
     (list
      #:tests? #f
      #:install-source? #f
      #:import-path "github.com/yuin/gopher-lua/cmd/glua"
      #:unpack-path "github.com/yuin/gopher-lua"))))

(define-public go-asmfmt
  (package
    (inherit go-github-com-klauspost-asmfmt)
    (name "go-asmfmp")
    (arguments
     (list
      #:tests? #f
      #:install-source? #f
      #:import-path "github.com/klauspost/asmfmt/cmd/asmfmt"
      #:unpack-path "github.com/klauspost/asmfmt"))))

(define-public go-csv2table
  (package/inherit go-github-com-olekukonko-tablewriter
    (name "go-csv2table")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-olekukonko-tablewriter)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:import-path _) "github.com/olekukonko/tablewriter/cmd/csv2table")
       ((#:unpack-path _ "") "github.com/olekukonko/tablewriter")))
    (native-inputs
     (append (package-native-inputs go-github-com-olekukonko-tablewriter)
             (package-propagated-inputs go-github-com-olekukonko-tablewriter)))
    (propagated-inputs '())
    (inputs '())
    (description
     (string-append (package-description go-github-com-olekukonko-tablewriter)
                    "\nThis package provides a command line interface (CLI) tool."))))

(define-public go-hclogvet
  (package/inherit go-github-com-hashicorp-go-hclog
    (name "go-hclogvet")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-hashicorp-go-hclog)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:import-path _) "github.com/hashicorp/go-hclog/hclogvet")
        ((#:unpack-path _ "") "github.com/hashicorp/go-hclog")))
    (native-inputs
     (package-propagated-inputs go-github-com-hashicorp-go-hclog))
    (propagated-inputs '())
    (inputs '())
    (description
     "@code{hclogvet} is a @code{go vet} tool for checking that the
Trace/Debug/Info/Warn/Error methods on @code{hclog.Logger} are used
correctly.")))

(define-public go-jfmt
  (package/inherit go-zgo-at-jfmt
    (name "go-jfmt")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-zgo-at-jfmt)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:import-path _) "zgo.at/jfmt/cmd/jfmt")
        ((#:unpack-path _ "") "zgo.at/jfmt")))
    (description
     (string-append (package-description go-zgo-at-jfmt)
                    "  This package provides a command line interface (CLI) tool."))))

(define-public go-jsonnet
  (package
    (name "go-jsonnet")
    (version "0.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-jsonnet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qfr6yvhj33rhx1icxh99bbpngh5kwq1x7r39315y53bw216vbrz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/google/go-jsonnet/cmd/jsonnet"
      #:unpack-path "github.com/google/go-jsonnet"))
    (native-inputs
     (list go-github-com-fatih-color
           go-github-com-sergi-go-diff
           go-gopkg-in-yaml-v2
           go-sigs-k8s-io-yaml))
    (home-page "https://github.com/google/go-jsonnet")
    (synopsis "Go implementation of Jsonnet")
    (description
     "This package provides an implementation of the @url{http://jsonnet.org/,
Jsonnet} data templating language in Go.  It is a
feature-complete,production-ready implementation, compatible with the original
Jsonnet C++implementation.")
    (license license:asl2.0)))

(define-public go-ifacemaker
  (package/inherit go-github-com-vburenin-ifacemaker
    (name "go-ifacemaker")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-vburenin-ifacemaker)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)))
    (native-inputs
     (package-propagated-inputs go-github-com-vburenin-ifacemaker))
    (propagated-inputs '())
    (inputs '())
    (description
     (string-append
      (package-description go-github-com-vburenin-ifacemaker)
      "\nThis package provides a command line interface (CLI) tool."))))

(define-public go-md2man
  (package/inherit go-github-com-cpuguy83-go-md2man-v2
    (name "go-md2man")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-cpuguy83-go-md2man-v2)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:import-path _ "github.com/cpuguy83/go-md2man/v2")
        "github.com/cpuguy83/go-md2man")))
    (native-inputs
     (package-propagated-inputs go-github-com-cpuguy83-go-md2man-v2))
    (propagated-inputs '())
    (inputs '())
    (description
     (string-append (package-description go-github-com-cpuguy83-go-md2man-v2)
                    "\nThis package provides a command line interface (CLI)
tool."))))

(define-public go-modifytags
  (package/inherit go-github-com-fatih-gomodifytags
    (name "go-modifytags")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-fatih-gomodifytags)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)))
    (native-inputs
     (append
      (package-native-inputs go-github-com-fatih-gomodifytags)
      (package-propagated-inputs go-github-com-fatih-gomodifytags)))
    (propagated-inputs '())
    (inputs '())
    (description
     (string-append (package-description go-github-com-fatih-gomodifytags)
                    "\nThis package provides a command line interface (CLI) tool."))))

(define-public go-msgio
  (package
    (inherit go-github-com-libp2p-go-msgio)
    (name "go-msgio")
    (arguments
     (list
      #:tests? #f ; no tests
      #:install-source? #f
      #:import-path "github.com/libp2p/go-msgio/msgio"
      #:unpack-path "github.com/libp2p/go-msgio"))
    (synopsis "CLI tool to wrap messages with msgio header")))

(define-public go-msgp
  (package
    (inherit go-github-com-tinylib-msgp)
    (name "go-msgp")
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      #:import-path "github.com/tinylib/msgp"))
    (description
     (string-append (package-description go-github-com-tinylib-msgp)
                    " This package provides an command line interface (CLI)
tool."))))

(define-public go-mtree
  (package/inherit go-github-com-vbatts-go-mtree
    (name "go-mtree")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-vbatts-go-mtree)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:import-path _) "github.com/vbatts/go-mtree/cmd/gomtree")
       ((#:unpack-path _ "") "github.com/vbatts/go-mtree")))
    (native-inputs
     (package-propagated-inputs go-github-com-vbatts-go-mtree))
    (propagated-inputs '())
    (inputs '())))

(define-public go-numcpus
  (package
    (inherit go-github-com-tklauser-numcpus)
    (name "go-numcpus")
    (arguments
     (list
      #:import-path "github.com/tklauser/numcpus/cmd/numcpus"
      #:unpack-path "github.com/tklauser/numcpus"
      #:install-source? #f))
    (description
     "This package provides a CLI build from the
go-github-com-tklauser-numcpus source.")))

(define-public go-pixelmatch
  (package
    (inherit go-github-com-orisano-pixelmatch)
    (name "go-pixelmatch")
    (arguments
     (list
      #:import-path "github.com/orisano/pixelmatch/cmd/pixelmatch"
      #:unpack-path "github.com/orisano/pixelmatch"
      #:install-source? #f))
    (synopsis "Pixel-level image comparison command")
    (description
     "This package provides a CLI build from the
go-github-com-orisano-pixelmatch source.")))

(define-public go-porter2
  (package
    (inherit go-github-com-surgebase-porter2)
    (name "go-porter2")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-surgebase-porter2)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:unpack-path _) "github.com/surgebase/porter2")
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda arguments
                (for-each
                 (lambda (cmd)
                   (apply (assoc-ref %standard-phases 'build)
                          `(,@arguments #:import-path ,cmd)))
                 (list "github.com/surgebase/porter2/cmd/compare"
                       "github.com/surgebase/porter2/cmd/suffixfsm"
                       "github.com/surgebase/porter2/cmd/switchvsmap"))))
            (replace 'install
              (lambda arguments
                (for-each
                 (lambda (cmd)
                   (apply (assoc-ref %standard-phases 'install)
                          `(,@arguments #:import-path ,cmd)))
                 (list "github.com/surgebase/porter2/cmd/compare"
                       "github.com/surgebase/porter2/cmd/suffixfsm"
                       "github.com/surgebase/porter2/cmd/switchvsmap"))))))))
    (description
     (string-append (package-description go-github-com-mattn-go-sixel)
                    "  This package provides an command line interface (CLI)
tools:
@itemize
@item @code{compare}
@item @code{suffixfsm} is a finite state machine generator for the porter2
@item @code{switchvsmap}
@end itemize"))))

(define-public go-sentences
  (package
    (inherit go-github-com-neurosnap-sentences)
    (name "go-sentences")
    (arguments
     (list
      #:import-path "github.com/neurosnap/sentences/cmd/sentences"
      #:unpack-path "github.com/neurosnap/sentences"
      #:install-source? #f))
    (description
     (string-append (package-description go-github-com-neurosnap-sentences)
                    "  This package provides an command line interface (CLI)
tool."))))

(define-public go-sixel
  (package
    (inherit go-github-com-mattn-go-sixel)
    (name "go-sixel")
    (arguments
     (list
      #:tests? #f ; tested in the library
      #:install-source? #f
      #:unpack-path "github.com/mattn/go-sixel"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda arguments
              (for-each
               (lambda (cmd)
                 (apply (assoc-ref %standard-phases 'build)
                        `(,@arguments #:import-path ,cmd)))
               (list "github.com/mattn/go-sixel/cmd/goscat"
                     "github.com/mattn/go-sixel/cmd/gosd"
                     "github.com/mattn/go-sixel/cmd/gosgif"
                     "github.com/mattn/go-sixel/cmd/gosl"
                     "github.com/mattn/go-sixel/cmd/gosr"))))
          (replace 'install
            (lambda arguments
              (for-each
               (lambda (cmd)
                 (apply (assoc-ref %standard-phases 'install)
                        `(,@arguments #:import-path ,cmd)))
               (list "github.com/mattn/go-sixel/cmd/goscat"
                     "github.com/mattn/go-sixel/cmd/gosd"
                     "github.com/mattn/go-sixel/cmd/gosgif"
                     "github.com/mattn/go-sixel/cmd/gosl"
                     "github.com/mattn/go-sixel/cmd/gosr")))))))
    (description
     (string-append (package-description go-github-com-mattn-go-sixel)
                    "  This package provides an command line interface (CLI)
tools."))))

(define-public go-tengo
  (package/inherit go-github-com-d5-tengo-v2
    (name "tengo")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-d5-tengo-v2)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:import-path "github.com/d5/tengo/v2")
        "github.com/d5/tengo/v2/cmd/tengo")
       ((#:unpack-path _ "") "github.com/d5/tengo/v2")))
    (native-inputs (package-propagated-inputs go-github-com-d5-tengo-v2))
    (propagated-inputs '())
    (inputs '())
    (description
     (string-append
      (package-description go-github-com-d5-tengo-v2)
      "\nThis package provides a command line interface (CLI) tool."))))

(define-public go-gronx-tasker
  (package/inherit go-github-com-adhocore-gronx
    (name "go-gronx-tasker")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-adhocore-gronx)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:import-path _) "github.com/adhocore/gronx/cmd/tasker")
       ((#:unpack-path _ "") "github.com/adhocore/gronx")))
    (native-inputs
     (package-propagated-inputs go-github-com-adhocore-gronx))
    (propagated-inputs '())
    (inputs '())))

(define-public go-toml
  (package
    (inherit go-github-com-pelletier-go-toml-v2)
    (name "go-toml")
    (arguments
     (list
      #:tests? #f ; tested in the library
      #:install-source? #f
      #:unpack-path "github.com/pelletier/go-toml"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda arguments
              (for-each
               (lambda (cmd)
                 (apply (assoc-ref %standard-phases 'build)
                        `(,@arguments #:import-path ,cmd)))
               (list "github.com/pelletier/go-toml/cmd/tomljson"
                     "github.com/pelletier/go-toml/cmd/tomll"))))
          (replace 'install
            (lambda arguments
              (for-each
               (lambda (cmd)
                 (apply (assoc-ref %standard-phases 'install)
                        `(,@arguments #:import-path ,cmd)))
               (list "github.com/pelletier/go-toml/cmd/tomljson"
                     "github.com/pelletier/go-toml/cmd/tomll")))))))
    (description
     (string-append (package-description go-github-com-pelletier-go-toml-v2)
                    "\nThis package provides command line interface (CLI)
tools."))))

(define-public go-tomlv
  (package/inherit go-github-com-burntsushi-toml
    (name "go-tomlv")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-burntsushi-toml)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:import-path "github.com/BurntSushi/toml")
        "github.com/BurntSushi/toml/cmd/tomlv")
       ((#:unpack-path _ "") "github.com/BurntSushi/toml")))
    (native-inputs (package-propagated-inputs go-github-com-burntsushi-toml))
    (propagated-inputs '())
    (inputs '())
    (description
     (string-append
      (package-description go-github-com-burntsushi-toml)
      "\nThis package provides a command line interface (CLI) tool."))))

(define-public go-ulid
  (package/inherit go-github-com-oklog-ulid-v2
    (name "go-ulid")
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/oklog/ulid/v2/cmd/ulid"
      #:unpack-path "github.com/oklog/ulid/v2"))
    (description
     (string-append (package-description go-github-com-oklog-ulid-v2)
                    "\nThis package provides a command line interface (CLI)
tool."))))

(define-public gofumpt
  (package/inherit go-mvdan-cc-gofumpt
    (name "gofumpt")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-mvdan-cc-gofumpt)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)))
    (native-inputs (package-propagated-inputs go-mvdan-cc-gofumpt))
    (propagated-inputs '())
    (inputs '())))

(define-public misspell
  (package/inherit go-github-com-client9-misspell
    (name "misspell")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-client9-misspell)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:import-path "github.com/client9/misspell")
        "github.com/client9/misspell/cmd/misspell")
       ((#:unpack-path _ "") "github.com/client9/misspell")))
    (native-inputs (package-propagated-inputs go-github-com-client9-misspell))
    (propagated-inputs '())
    (inputs '())))

(define-public gops
  (package/inherit go-github-com-google-gops
    (name "gops")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-google-gops)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)))
    (native-inputs (package-propagated-inputs go-github-com-google-gops))
    (propagated-inputs '())
    (inputs '())))

(define-public oci-runtime-tool
  (package/inherit go-github-com-opencontainers-runtime-tools
    (name "oci-runtime-tool")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-opencontainers-runtime-tools)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:tests? _ #t) #f)
       ((#:import-path "github.com/opencontainers/runtime-tools")
        "github.com/opencontainers/runtime-tools/cmd/oci-runtime-tool")
       ((#:unpack-path _ "") "github.com/opencontainers/runtime-tools")
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'install-man-pages
              (lambda* (#:key unpack-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" unpack-path)
                  (let ((man (string-append #$output "/share/man/man1")))
                    (mkdir-p man)
                    (invoke "go-md2man"
                            "-in" "man/oci-runtime-tool.1.md"
                            "-out" (string-append man "/oci-runtime-tool.1"))
                    (invoke "go-md2man"
                            "-in" "man/oci-runtime-tool-generate.1.md"
                            "-out" (string-append man "/oci-runtime-tool-generate.1"))
                    (invoke "go-md2man"
                            "-in" "man/oci-runtime-tool-validate.1.md"
                            "-out" (string-append man "/oci-runtime-tool-validate.1"))))))
            (add-after 'install 'install-bash-completions
              (lambda* (#:key unpack-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" unpack-path)
                  (let ((bash (string-append #$output "/share/bash-completion")))
                    (mkdir-p bash)
                    (copy-file "completions/bash/oci-runtime-tool"
                                 (string-append bash "/completions"))))))))))
    (native-inputs
     (append
      (modify-inputs (package-native-inputs go-github-com-opencontainers-runtime-tools)
        (append go-md2man))
      (package-propagated-inputs go-github-com-opencontainers-runtime-tools)))
    (propagated-inputs '())
    (inputs '())))

(define-public protoc-gen-validate
  (package
    (inherit go-github-com-envoyproxy-protoc-gen-validate)
    (name "protoc-gen-validate")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-envoyproxy-protoc-gen-validate)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda* (#:key import-path #:allow-other-keys #:rest arguments)
                (for-each
                 (lambda (cmd)
                   (apply (assoc-ref %standard-phases 'build)
                          `(,@arguments #:import-path ,cmd)))
                 (list import-path
                       (string-append import-path
                                      "/cmd/protoc-gen-validate-cpp")
                       (string-append import-path
                                      "/cmd/protoc-gen-validate-go")
                       (string-append import-path
                                      "/cmd/protoc-gen-validate-java")))))))))
    (native-inputs (package-propagated-inputs
                    go-github-com-envoyproxy-protoc-gen-validate))
    (propagated-inputs '())
    (inputs '())
    (description
     (string-append (package-description
                     go-github-com-envoyproxy-protoc-gen-validate)
                    "\nThis package provides command line interface (CLI)
tools."))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
