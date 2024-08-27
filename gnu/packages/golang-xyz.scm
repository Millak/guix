;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017-2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Pierre Neidhardt <ambrevar@gmail.com>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018, 2020 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019, 2021 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019-2021 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2019-2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021 raingloom <raingloom@riseup.net>
;;; Copyright © 2021 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Guix Together <jgart@dismail.de>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 raingloom <raingloom@riseup.net>
;;; Copyright © 2021, 2023, 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2022 (unmatched-parenthesis <paren@disroot.org>
;;; Copyright © 2022 Dhruvin Gandhi <contact@dhruvin.dev>
;;; Copyright © 2022 Dominic Martinez <dom@dominicm.dev>
;;; Copyright © 2022 Leo Nikkilä <hello@lnikki.la>
;;; Copyright © 2022 kiasoc5 <kiasoc5@disroot.org>
;;; Copyright © 2023 Benjamin <benjamin@uvy.fr>
;;; Copyright © 2023 Fries <fries1234@protonmail.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2023 Nguyễn Gia Phong <mcsinyx@disroot.org>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Sergey Trofimov <sarg@sarg.org.ru>
;;; Copyright © 2023 Thomas Ieong <th.ieong@free.fr>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2023 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Brian Kubisiak <brian@kubisiak.com>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Jean Simard <woshilapin@tuziwo.info>
;;; Copyright © 2024 Jesse Eisses <jesse@eisses.email>
;;; Copyright © 2024 Luis Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Spencer Peters <spencerpeters@protonmail.com>
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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages specifications))

;;; Commentary:
;;;
;;; Nomad Golang modules (libraries) are welcome here.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

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

(define-public go-git-sr-ht-emersion-go-scfg
  (package
    (name "go-git-sr-ht-emersion-go-scfg")
    (version "0.0.0-20240128091534-2ae16e782082")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~emersion/go-scfg")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gxhg40q4md3lj1wrrnms0jhyqsxhx2hcv6sm5yjbbqana5x26mx"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "git.sr.ht/~emersion/go-scfg"))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew))
    (home-page "https://git.sr.ht/~emersion/go-scfg")
    (synopsis "Go library for simple configuration file format")
    (description
     "Package go-scfg parses scfg files.")
    (license license:expat)))

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

(define-public go-git-sr-ht-rockorager-vaxis
  (package
    (name "go-git-sr-ht-rockorager-vaxis")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~rockorager/vaxis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pmi4bigqgrfdlk9d6ia3jxgcr4iadiyynf6bny9fdc3yyppcxic"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~rockorager/vaxis"))
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

(define-public go-github-com-a8m-envsubst
  (package
    (name "go-github-com-a8m-envsubst")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a8m/envsubst")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mjs729g9nmalx25l4nn3p07amm4vsciqmdf0jbh2jwpy1zymz41"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/a8m/envsubst"))
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
     `(#:import-path "github.com/alecthomas/chroma"))
    (native-inputs
     (list go-github-com-dlclark-regexp2
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
    (version "2.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/chroma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qgr4gywjks869sc85wb8nby612b8wvsa1dwpsbanjsljq7wq7mp"))))
    (arguments
     (list
      #:import-path "github.com/alecthomas/chroma/v2"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-testdata-and-cmd-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "lexers/testdata/python2/test_complex_file1.actual"
                                ;; Executible is packed as separate package.
                                "cmd")))))
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-dlclark-regexp2))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2
           go-github-com-alecthomas-repr))))

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

(define-public go-github-com-alecthomas-kingpin
  (package
    (name "go-github-com-alecthomas-kingpin")
    (version "2.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/kingpin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mndnv3hdngr3bxp7yxfd47cas4prv98sqw534mx7vp38gd88n5r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/kingpin"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-alecthomas-template
           go-github-com-alecthomas-units))
    (home-page "https://github.com/alecthomas/kingpin")
    (synopsis "Go library provides utilities for building command line interfaces")
    (description
     "Go library provides utilities for building command line interfaces.")
    (license license:expat)))

(define-public go-github-com-alecthomas-kingpin-v2
  (package
    (inherit go-github-com-alecthomas-kingpin)
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
    (arguments
     (list
      #:import-path "github.com/alecthomas/kingpin/v2"))
    (propagated-inputs
     (list go-github-com-alecthomas-units
           go-github-com-xhit-go-str2duration-v2))
    (native-inputs
     (list go-github-com-stretchr-testify))))

(define-public go-github-com-alecthomas-kong
  (package
    (name "go-github-com-alecthomas-kong")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/kong")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a9arf30h84ll8k612jh50c3vjmvdfj6i7dbvfnw3dalm6dn2aan"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; One test failed when set to go-1.18 o lower, see
      ;; <https://github.com/alecthomas/kong/issues/437>
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

(define-public go-github-com-alecthomas-participle-v2
  (package
    (name "go-github-com-alecthomas-participle-v2")
    (version "2.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/participle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k2vsd58rgwyylyn5zja6z6k1sg4m39g2fhd88lvja60ca51bh98"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/participle/v2"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2))
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

(define-public go-github-com-alecthomas-template
  ;; No release, see <https://github.com/alecthomas/template/issues/7>.
  (let ((commit "a0175ee3bccc567396460bf5acd36800cb10c49c")
        (revision "0"))
    (package
      (name "go-github-com-alecthomas-template")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alecthomas/template")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qjgvvh26vk1cyfq9fadyhfgdj36f1iapbmr5xp6zqipldz8ffxj"))))
      (build-system go-build-system)
      (arguments
       (list
        ;; XXX: Failing on a newer Golang version: FAIL: TestJSEscaping
        ;; (0.00s) exec_test.go:757: JS escaping [unprintable ﷿] got
        ;; [unprintable ﷿] want [unprintable \uFDFF]
        #:go go-1.17
        #:import-path "github.com/alecthomas/template"))
      (home-page "https://github.com/alecthomas/template")
      (synopsis "Fork of Go's text/template adding newline elision")
      (description
       "This is a fork of Go 1.4's text/template package with one addition: a
backslash immediately after a closing delimiter will delete all subsequent
newlines until a non-newline.")
      (license license:bsd-3))))

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
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  (("TestMaxDelay") "OffTestMaxDelay")))))
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
     (list
      #:import-path "github.com/avast/retry-go/v3"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  (("TestMaxDelay") "OffTestMaxDelay")))))
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))))

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
     (list
      #:import-path "github.com/avast/retry-go/v4"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))))

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

(define-public go-github-com-benbjohnson-clock
  (package
    (name "go-github-com-benbjohnson-clock")
    (version "1.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/benbjohnson/clock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p7n09pywqra21l981fbkma9vzsyf31pbvw6xg5r4hp8h8scf955"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/benbjohnson/clock"))
    (home-page "https://github.com/benbjohnson/clock")
    (synopsis "Small library for mocking time in Go")
    (description
     "@code{clock} is a small library for mocking time in Go.  It provides an
interface around the standard library's @code{time} package so that the application
can use the realtime clock while tests can use the mock clock.")
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
      #:import-path "github.com/beorn7/perks"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (home-page "https://github.com/beorn7/perks")
    (synopsis "Compute approximate quantiles over an unbounded data stream")
    (description
     "Perks contains the Go package @code{quantile} that computes
approximate quantiles over an unbounded data stream within low memory and CPU
bounds.")
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

(define-public go-github-com-briandowns-spinner
  (package
    (name "go-github-com-briandowns-spinner")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/briandowns/spinner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "036r59m068k8grr0q77a6b1rqw4dyxm00fsxj7b9w1fjviq8djs6"))
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

(define-public go-github-com-burntsushi-toml
  (package
    (name "go-github-com-burntsushi-toml")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BurntSushi/toml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vk0s7pcn80hkx0lcyws509gqs42c8y1rppv05zxiqj0yn2zrjnx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/BurntSushi/toml"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (home-page "https://github.com/BurntSushi/toml")
    (synopsis "Toml parser and encoder for Go")
    (description
     "This package is toml parser and encoder for Go.  The interface is
similar to Go's standard library @code{json} and @code{xml} package.")
    (license license:expat)))

;; XXX: This repository has been archived by the owner on Feb 21, 2018. It is
;; now read-only.
(define-public go-github-com-calmh-xdr
  (package
    (name "go-github-com-calmh-xdr")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/calmh/xdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "072wqdncz3nd4a3zkhvzzx1y3in1lm29wfvl0d8wrnqs5pyqh0mh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/calmh/xdr"))
    (home-page "https://github.com/calmh/xdr")
    (synopsis "XDR marshalling and unmarshalling")
    (description
     "XDR is an External Data Representation (XDR)
marshalling and unmarshalling library in Go.  It uses code generation and not
reflection.")
    (license license:expat)))

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
      #:import-path "github.com/charlievieth/fastwalk"))
    (home-page "https://github.com/charlievieth/fastwalk")
    (synopsis "Fast directory traversal for Golang")
    (description
     "Package fastwalk provides a faster version of
@url{/path/filepath#@code{WalkDir,filepath.WalkDir}} for file system scanning
tools.")
    (license license:expat)))

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
           go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/cheggaaa/pb/")
    (synopsis "Console progress bar for Go")
    (description
     "This package is a Go library that draws progress bars on the terminal.")
    (license license:bsd-3)))

(define-public go-github-com-cheggaaa-pb-v3
  (package
    (inherit go-github-com-cheggaaa-pb)
    (name "go-github-com-cheggaaa-pb-v3")
    (version "3.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cheggaaa/pb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mf86iav69qpyg0nd54g0f50yigjkfzdhaqzkbn4yfb3fnb75n2z"))))
    (arguments
     (list
      #:import-path "github.com/cheggaaa/pb/v3"
      #:unpack-path "github.com/cheggaaa/pb"))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-github-com-cheggaaa-pb)
       (append go-github-com-vividcortex-ewma)))))

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
     (list go-github-com-godbus-dbus))
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

(define-public go-github-com-creack-pty
  (package
    (name "go-github-com-creack-pty")
    (version "1.1.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/creack/pty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1chx7ml9wlpk8pzgnnxb97gblmxz1j1v37m5i1asb94l5c24r1fg"))))
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

(define-public go-github-com-cskr-pubsub
  (package
    (name "go-github-com-cskr-pubsub")
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
      #:tests? #t ; Tests require network interface access
      #:import-path "github.com/cskr/pubsub"))
    (home-page "https://github.com/cskr/pubsub")
    (synopsis "Simple pubsub package for go")
    (description
     "Package @code{pubsub} implements a simple multi-topic pub-sub library.")
    (license license:bsd-2)))

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

(define-public go-github-com-dave-jennifer
  (package
    (name "go-github-com-dave-jennifer")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dave/jennifer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01sgafbds8n5zs61qf057whn06yj6avz30xgxk6pllf22528558m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dave/jennifer"))
    (home-page "https://github.com/dave/jennifer")
    (synopsis "Code generator for Go")
    (description "This package provides functionality to generate Go code.")
    (license license:expat)))

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
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; XXX: varint_test.go:94: unexpected error: -11.
                  (("TestUvarint") "OffTestUvarint"))))))))
    (home-page "https://github.com/dennwc/varint")
    (synopsis "Fast varint library for Golang")
    (description
     "This package provides an optimized implementation of protobuf's varint
encoding/decoding.  It has no dependencies.")
    (license license:expat)))

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
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlclark/regexp2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1irfv89b7lfkn7k3zgx610ssil6k61qs1wjj31kvqpxb3pdx4kry"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dlclark/regexp2"))
    (home-page "https://github.com/dlclark/regexp2/")
    (synopsis "Full featured regular expressions for Go")
    (description
     "Regexp2 is a feature-rich RegExp engine for Go.")
    (license license:expat)))

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

(define-public go-github-com-dustin-gojson
  (package
    (name "go-github-com-dustin-gojson")
    (version "v0.0.0-20160307161227-2e71ec9dd5ad")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dustin/gojson")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vrmmyn7l568l1k71mxd54iqf3d54pn86cf278i374j86jn0bdxf"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Fix the library to work with go-1.21.
                   (substitute* "decode.go"
                     (("trying to unmarshal unquoted value into")
                      "trying to unmarshal unquoted value %v into"))
                   (substitute* "decode_test.go"
                     (("t.Fatalf\\(\"Unmarshal: %v\"\\)")
                      "t.Fatalf(\"Unmarshal: %v\", data)")) ;))))
                   (substitute* "scanner.go"
                     (("s := strconv.Quote\\(string\\(c\\)\\)")
                      "s := strconv.QuoteRune(rune(c))"))))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/dustin/gojson"))
    (home-page "https://github.com/dustin/gojson")
    (synopsis "Extended Golang's @code{encoding/json} module with the public scanner API")
    (description
     "This package provides a fork of Golang's @code{encoding/json} with the
scanner API made public.")
    (license license:bsd-3)))

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
     "This package implements functinoality of mapping files into memory.  It
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
     (list #:import-path "github.com/elliotchance/orderedmap"))
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
     (list
      #:import-path "github.com/elliotchance/orderedmap/v2"
      #:unpack-path "github.com/elliotchance/orderedmap"))))

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
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-webdav")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ay0x3c1frkj4z3j17s42yvf1hgmg8223qmsyr41yxwz88zsvjlj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-webdav"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
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

(define-public go-github-com-expr-lang-expr
  (package
    (name "go-github-com-expr-lang-expr")
    (version "1.16.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/expr-lang/expr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08p7gcxm7psgn1rzhhy2s2va59ssy77x8wd706gdp2pif7wln883"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/expr-lang/expr"))
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

(define-public go-github-com-fxamacker-cbor-v2
  (package
    (name "go-github-com-fxamacker-cbor-v2")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fxamacker/cbor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "039lk7n5155gy2sh55i1darcvxhv9fim2xmnvmx0xi9ihnrnczln"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Check if the most of the tests may be enabled:
      ;; src/github.com/fxamacker/cbor/v2/decode_test.go:328:9: cannot convert
      ;; 1000000000000 (untyped int constant) to type uint
      #:tests? (target-64bit?)
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
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-logr/logr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x0q9jkk2p5pz4lii1qs8ifnsib4ib5s8pigmjwdmagl976g8nhm"))))
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
      #:import-path "github.com/go-stack/stack"))
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
      #:import-path "github.com/gobwas/glob"))
    (home-page "https://github.com/gobwas/glob")
    (synopsis "Go globbing library")
    (description
     "This package provides a Go implementation of globs.")
    (license license:expat)))

(define-public go-github-com-goccy-go-yaml
  (package
    (name "go-github-com-goccy-go-yaml")
    (version "1.12.0")
    (home-page "https://github.com/goccy/go-yaml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06sf7vpz8gjyivrn3yhzcbbf3qhsqq5n7lsc23j91xw5xwpn37bk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/goccy/go-yaml"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/benchmarks"))))
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
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

(define-public go-github-com-google-renameio
  (package
    (name "go-github-com-google-renameio")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/renameio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qn84nra9nxqyqg4i496b6ijbhnxvx66hdndwl7qh7r6q8lz2ba5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/renameio"))
    (home-page "https://github.com/google/renameio/")
    (synopsis "Atomically create or replace a file or symbolic link")
    (description
     "@code{renameio} Go package provides a way to atomically create or
replace a file or symbolic link.")
    (license license:asl2.0)))

(define-public go-github-com-google-renameio-v2
  (package/inherit go-github-com-google-renameio
    (name "go-github-com-google-renameio-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/renameio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13vc7p43zz5pmgli4k18b15khxpca1zd8v1ga0ryq7ddyz55fg7i"))))
    (arguments
     (list
      #:import-path "github.com/google/renameio/v2"))))

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

(define-public go-github-com-hashicorp-go-hclog
  (package
    (name "go-github-com-hashicorp-go-hclog")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-hclog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lvr4ga95a0xb62vgq1hy558x3r65hn2d0h7bf0a88lsfsrcik0n"))))
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
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-version")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fw6hwvjadpbfj10yk7f64ypw8lmv5s5ny3s4ria0nv6xam1wpai"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/hashicorp/go-version"))
    (home-page "https://github.com/hashicorp/go-version")
    (synopsis "Go library for parsing and verifying versions and version
constraints")
    (description
     "This package is a library for parsing versions and version
constraints, and verifying versions against a set of constraints.  It can sort
a collection of versions properly, handles prerelease/beta versions, can
increment versions.")
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
        (base32 "0lb2ylv2bz6lsqhn6c2hsafjjcx0hsdbah6arhb778g3xbkpgvf3"))))
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
      #:import-path "github.com/hashicorp/hcl"))
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
    (version "2.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/hcl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f9flmmkj7fr1337fc56cqy73faq87ix375hnz3id4wc023przv1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/hcl/v2"))
    (native-inputs
     (list go-github-com-davecgh-go-spew))
    (inputs
     (list go-github-com-agext-levenshtein
           go-github-com-apparentlymart-go-textseg-v13
           go-github-com-mitchellh-go-wordwrap
           go-github-com-zclconf-go-cty))
    (synopsis "Go implementation of HashiCorp Configuration Language V2")
    (description
     "This package contains the main implementation of the @acronym{HCL,
HashiCorp Configuration Language}.  HCL is designed to be a language for
expressing configuration which is easy for both humans and machines to read.")
    (home-page "https://github.com/hashicorp/hcl")
    (license license:mpl2.0)))

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
      #:import-path "github.com/jbenet/go-temp-err-catcher"))
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
      #:import-path "github.com/jbenet/goprocess"))
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
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "nlp/segment/segment_test.go"
                  (("TestGoldenRules") "OffTestGoldenRules")))))
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-montanaflynn-stats
           go-github-com-neurosnap-sentences
           go-github-com-errata-ai-regexp2))
    (home-page "https://github.com/jdkato/twine")
    (synopsis "NLP-related string utilities")
    (description
     "NLP-related string utility functions for Golang.")
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
just matching lines.  This package proviedes a CLI tool and @code{colors}
library.")
    (license license:bsd-2)))

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
      #:import-path "github.com/karrick/godirwalk"))
    (home-page "https://github.com/karrick/godirwalk")
    (synopsis "Fast directory traversal library for Go")
    (description
     "This package provides functions to read and traverse directory trees.")
    (license license:bsd-2)))

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
    (build-system go-build-system)
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
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/logrusorgru/aurora/v3"))
    (native-inputs
     (list go-github-com-stretchr-testify))))

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
    (version "3.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Masterminds/sprig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gkwalx8j8h1jdhk6dz8bq8zp7vivxvcivr83dcq0h6nrn4xjqnl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Masterminds/sprig/v3"
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests tries to reach Google:
          ;; tpl := `{{"www.google.com" | getHostByName}}`
          (add-after 'unpack 'remove-network-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file
               (string-append "src/" import-path "/network_test.go")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-github-com-huandu-xstrings
           go-github-com-imdario-mergo
           go-github-com-masterminds-goutils
           go-github-com-masterminds-semver-v3
           go-github-com-mitchellh-copystructure
           go-github-com-mitchellh-reflectwalk
           go-github-com-shopspring-decimal
           go-github-com-spf13-cast
           go-golang-org-x-crypto))
    (home-page "https://github.com/Masterminds/sprig/")
    (synopsis "Template functions for Go templates")
    (description
     "Sprig is a library that provides more than 100 commonly used template
functions.")
    (license license:expat)))

(define-public go-github-com-matryer-try
  (package
    (name "go-github-com-matryer-try")
    (version "1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matryer/try")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15f0m5ywihivnvwzcw0mh0sg27aky9rkywvxqszxka9q051qvsmy"))))
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
     `(#:import-path "github.com/mattn/go-shellwords"
       #:phases
       (modify-phases %standard-phases
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
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Activate when go-build-system supports submodules.
          (delete 'build)
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
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
  (let ((commit "9520e82c474b0a04dd04f8a40959027271bab992")
        (revision "0"))
    (package
      (name "go-github-com-mgutz-ansi")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/mgutz/ansi")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00bz22314j26736w1f0q4jy9d9dfaml17vn890n5zqy3cmvmww1j"))))
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
      (license license:expat))))

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
      #:import-path "github.com/muesli/cancelreader"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/muesli/cancelreader")
    (synopsis "Cancelable reader for Golang")
    (description
     "This package provides a cancelable reader for Go.")
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
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
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
      #:import-path "github.com/muesli/termenv"))
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

(define-public go-github-com-nats-io-nats-go
  (package
    (name "go-github-com-nats-io-nats-go")
    (version "1.32.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nats-io/nats.go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08b3n5mdpxvn9hipz0j001bp5r67i43cqji9x9dyzikypqdfg38k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nats-io/nats.go"))
    (propagated-inputs (list go-golang-org-x-text
                         go-github-com-nats-io-nuid
                         go-github-com-nats-io-nkeys
                         go-github-com-klauspost-compress))
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

(define-public go-github-com-niklasfasching-go-org
  (package
    (name "go-github-com-niklasfasching-go-org")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/niklasfasching/go-org")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "100ay19a7my2m1za1ih0wvqxf5mq77byas1f23mx69qsbp391w04"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/niklasfasching/go-org"
      #:phases
      #~(modify-phases %standard-phases
          ;; FIXME: Pattern embedded: cannot embed directory embedded:
          ;; contains no embeddable files.
          ;;
          ;; This happens due to Golang can't determine the valid directory of
          ;; the module which is sourced during setup environment phase, but
          ;; easy resolved after coping to expected directory "vendor" within
          ;; the current package, see details in Golang source:
          ;;
          ;; - URL: <https://github.com/golang/go/blob/>
          ;; - commit: 82c14346d89ec0eeca114f9ca0e88516b2cda454
          ;; - file: src/cmd/go/internal/load/pkg.go#L2059
          (add-before 'build 'copy-input-to-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (mkdir "vendor")
                (copy-recursively
                 (string-append
                  #$(this-package-input "go-github-com-alecthomas-chroma-v2")
                  "/src/github.com")
                 "vendor/github.com"))))
          (add-before 'install 'remove-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "vendor")))))))
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
    (home-page "https://github.com/oklog/ulid")
    (synopsis "Universally Unique Lexicographically Sortable Identifier in Golang")
    (description
     "This package implements @acronym{ULID, Universally Unique
Lexicographically Sortable Identifier} as specificed in
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
      #:import-path "github.com/orisano/pixelmatch"))
    (home-page "https://github.com/orisano/pixelmatch")
    (synopsis "Pixelmatch port to Go")
    (description
     "This package provides a port of Pixelmatch, a pixel-level image
comparison library, to Go.  Both a library and a command-line tool are
included in this package.")
    (license license:expat)))

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
     "This package implments recursive copy functinoality for directory.")
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
     (list #:import-path "github.com/pelletier/go-toml"))
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
    (version "2.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pelletier/go-toml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gimgz33yxmvj0nmy56yy7zq4ay8j55ir8pfzmgwga7npgpzspk7"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/pelletier/go-toml/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs '())))

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
      #:import-path "github.com/pierrec/cmdflag"))
    (home-page "https://github.com/pierrec/cmdflag")
    (synopsis "Augment the flag package with commands")
    (description
     "Package @code{cmdflag} provides simple command line commands processing
on top of the standard library @code{flag} package.")
    (license license:bsd-3)))

(define-public go-github-com-pion-logging
  (package
    (name "go-github-com-pion-logging")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/logging/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11ay8c15xk3pv7y9nd80szk3mci480x67yqlgb10vswrz4h4mx3v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/logging"))
    (home-page "https://github.com/pion/logging/")
    (synopsis "Logging library for Golang projects")
    (description
"This package provides a logging library used by @url{https://github.com/pion,
Pion}.")
    (license license:expat)))

(define-public go-github-com-polydawn-refmt
  (package
    (name "go-github-com-polydawn-refmt")
    (version "0.89.1-0.20231129105047-37766d95467a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/polydawn/refmt/")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0grgzacc7igfndk1v3n1g6k4wdz6bjsiqfq3n5br2zpr7n40ha9n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/polydawn/refmt"))
    (propagated-inputs
     (list go-github-com-urfave-cli
           go-github-com-warpfork-go-wish
           go-github.com-smartystreets-goconvey
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/polydawn/refmt/")
    (synopsis "Object mapping for Go language")
    (description
     "@code{refmt} is a serialization and object-mapping library.")
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

(define-public go-github-com-rcrowley-go-metrics
  (let ((commit "cac0b30c2563378d434b5af411844adff8e32960")
        (revision "2"))
    (package
      (name "go-github-com-rcrowley-go-metrics")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rcrowley/go-metrics")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hfxffnpaw49pr3wrkbzq3pnv3nyzsvk5dxndv0yz70xlrbg8a04"))))
      (build-system go-build-system)
      (arguments
       ;; Arbitrary precision tests are known to be broken on aarch64, ppc64le
       ;; and s390x. See: https://github.com/rcrowley/go-metrics/issues/249
       `(#:tests? ,(not (string-prefix? "aarch64" (or (%current-target-system)
                                                      (%current-system))))
         #:import-path "github.com/rcrowley/go-metrics"))
      (propagated-inputs
       (list go-github-com-stathat-go))
      (synopsis "Go port of Coda Hale's Metrics library")
      (description "This package provides a Go implementation of Coda Hale's
Metrics library.")
      (home-page "https://github.com/rcrowley/go-metrics")
      (license license:bsd-2))))

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
      #:import-path "github.com/rogpeppe/go-internal"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  (("TestSimple") "OffTestSimple")))))
          ;; XXX: Replace when go-build-system supports nested path.
          (delete 'build)
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-golang-org-x-mod
           go-golang-org-x-sys
           go-golang-org-x-tools))
    (home-page "https://github.com/rogpeppe/go-internal/")
    (synopsis "Internal packages from the Go standard library")
    (description
     "This repository factors out an opinionated selection of internal
packages and functionality from the Go standard library.  Currently this
consists mostly of packages and testing code from within the Go tool
implementation.

Included are the following:
@itemize
@item dirhash: calculate hashes over directory trees the same way that the Go tool does.
@item goproxytest: a GOPROXY implementation designed for test use.
@item gotooltest: Use the Go tool inside test scripts (see testscript below)
@item imports: list of known architectures and OSs, and support for reading import import statements.
@item modfile: read and write go.mod files while preserving formatting and comments.
@item module: module paths and versions.
@item par: do work in parallel.
@item semver: semantic version parsing.
@item testenv: information on the current testing environment.
@item testscript: script-based testing based on txtar files
@item txtar: simple text-based file archives for testing.
@end itemize\n")
    (license license:bsd-3)))

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

(define-public go-github-com-sergi-go-diff
  (package
    (name "go-github-com-sergi-go-diff")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sergi/go-diff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cbj8nshllq102iiav0k1s01b8gwbkzj674g71n938qqna32y2pa"))))
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
    (version "3.24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shirou/gopsutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xlfcx6giqaxdah2m02q2i8ynwlzar953wr8wqx1j3004xdgaivd"))))
    (arguments
     (list
      #:import-path "github.com/shirou/gopsutil"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               ;; host_test.go tries to access files such as
               ;; /var/run/utmp that do not exist in the build
               ;; environment.
               (string-append "src/" import-path "/host/host_test.go")))))))))

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
     "@{gitconfig} is a package to get configuration values from gitconfig.")
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
      #:import-path "github.com/spf13/cobra"))
    (propagated-inputs
     (list go-github-com-spf13-pflag))
    (home-page "https://github.com/spf13/cobra")
    (synopsis "Go library for creating CLI applications")
    (description
     "Cobra is both a library for creating powerful modern CLI applications as
well as a program to generate applications and command files.")
    (license license:asl2.0)))

(define-public go-github-com-stathat-go
  (let ((commit "74669b9f388d9d788c97399a0824adbfee78400e")
        (revision "0"))
    (package
      (name "go-github-com-stathat-go")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stathat/go")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zzlsl24dyr202qkr2pay22m6d0gb7ssms77wgdx0r0clgm7dihw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/stathat/go"))
      (synopsis "Post statistics to StatHat")
      (description "This is a Go package for posting to a StatHat account.")
      (home-page "https://github.com/stathat/go")
      (license license:expat))))

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
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; XXX Failing on i686-linux:
                  ;; failed on input 0xde6d70588e18c85b, 0x85261e67
                  (("TestBatchHeader") "OffTestBatchHeader")))))
          ;; XXX: Replace when go-build-system supports nested path.
          (delete 'build)
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
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
    (version "1.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/dig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "020dak2r0yykk6fkfwad2wbz73pjbbdkdamn0ir7xf2irxsmjakm"))))
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
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/fx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16pbqmkij02zw6xa4660k905y0r0rc50vyqhg41i2411r68jrdnn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.uber.org/fx"))
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
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/multierr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "162941s8f6a9x2w04qm4qa3zz0zylwag9149hywrj9ibp2nzcsqz"))))
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

(define-public go-go-uber-org-zap
  (package
    (name "go-go-uber-org-zap")
    (version "1.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/zap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lzbbs87fvixzbyv4wpl3s70vm2m0jz2jgdvrviiksc2al451qgs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.uber.org/zap"
      #:phases
      #~(modify-phases %standard-phases
          ;; Remove test files requiring to download all dependencies for the
          ;; current Go module and reports their module paths and locations on
          ;; disk.
          (add-after 'unpack 'remove-test-files
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file
               (string-append "src/" import-path
                              "/stacktrace_ext_test.go")))))))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-go-uber-org-goleak
           go-golang-org-x-lint
           go-honnef-co-go-tools))
    (propagated-inputs
     (list go-github-com-benbjohnson-clock
           go-github-com-pkg-errors
           go-go-uber-org-atomic
           go-go-uber-org-multierr
           go-gopkg-in-yaml-v2))
    (home-page "https://pkg.go.dev/go.uber.org/zap")
    (synopsis "Logging library for Go")
    (description
     "This package provides a library for fast, structured, leveled logging in
Go.")
    (license license:expat)))

(define-public go-gopkg-in-alecthomas-kingpin-v2
  (package
    (inherit go-github-com-alecthomas-kingpin)
    (name "go-gopkg-in-alecthomas-kingpin-v2")
    (arguments
     (list
      #:import-path "gopkg.in/alecthomas/kingpin.v2"))))

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

(define-public go-gopkg-in-natefinch-lumberjack.v2
  (package
    (name "go-gopkg-in-natefinch-lumberjack.v2")
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

(define-public go-k8s-io-klog
  (package
    (name "go-k8s-io-klog")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/klog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cgannfmldcrcksb2wqdn2b5qabqyxl9r25w9y4qbljw24hhnlvn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "k8s.io/klog"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; Disable test requiring write access to creat test files.
                  (("TestRollover") "OffTestRollover"))))))))
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

(define-public go-k8s-io-klog-v2
  (package
    (inherit go-k8s-io-klog)
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
      #:import-path "k8s.io/klog/v2"))))

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
      #:import-path "go.mongodb.org/mongo-driver"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples-and-benchmarks
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "benchmark"
                                "examples"
                                "cmd/godriver-benchmark")))))
          (add-before 'check 'disable-failing-tests
            (lambda* (#:key tests? unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; Some tests require running database and available network connection.
                  (("TestAggregate") "OffTestAggregate")
                  (("TestPollSRVRecords") "OffTestPollSRVRecords")
                  (("TestPollSRVRecordsServiceName")
                   "OffTestPollSRVRecordsServiceName")
                  (("TestPollingSRVRecordsLoadBalanced")
                   "OffTestPollingSRVRecordsLoadBalanced")
                  (("TestPollingSRVRecordsSpec")
                   "OffTestPollingSRVRecordsSpec")
                  (("TestServerHeartbeatOffTimeout")
                   "OffTestServerHeartbeatTimeout")
                  (("TestServerHeartbeatTimeout")
                   "OffTestServerHeartbeatTimeout")
                  (("TestTimeCodec") "OffTestTimeCodec")
                  (("TestTopologyConstructionLogging")
                   "OffTestTopologyConstructionLogging")
                  (("TestURIOptionsSpec") "OffTestURIOptionsSpec")))))
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (for-each
                   (lambda (package)
                     (invoke "go" "test" (string-append "./" package "/...")))
                   (list "bson" "event" "internal" "tag" "x")))))))))
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
     (list cmake))
    (home-page "https://github.com/mvdan/editorconfig")
    (synopsis "EditorConfig support in Go")
    (description
     "Package editorconfig allows parsing and using @code{EditorConfig} files, as
defined in @url{https://editorconfig.org/,https://editorconfig.org/}.")
    (license license:bsd-3)))

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

(define-public go-chroma
  (package
    (name "go-chroma")
    (version "2.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/chroma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1qgr4gywjks869sc85wb8nby612b8wvsa1dwpsbanjsljq7wq7mp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/alecthomas/chroma/cmd/chroma"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2
           go-github-com-alecthomas-chroma-v2
           go-github-com-alecthomas-kong
           go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty))
    (home-page "https://github.com/alecthomas/chroma")
    (synopsis "General purpose syntax highlighter in pure Golang")
    (description
     (string-append (package-description go-github-com-alecthomas-chroma-v2)
                    "  This package provides an command line interface (CLI)
tool."))
    (license license:asl2.0)))

(define-public go-hclogvet
  (package
    (inherit go-github-com-hashicorp-go-hclog)
    (name "go-hclogvet")
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-hclog/hclogvet"
      #:unpack-path "github.com/hashicorp/go-hclog"
      #:install-source? #f))
    (propagated-inputs
     (list go-golang-org-x-tools))
    (description
     "@code{hclogvet} is a @code{go vet} tool for checking that the
Trace/Debug/Info/Warn/Error methods on @code{hclog.Logger} are used
correctly.")))

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
    (synopsis "CLI tool to wrap messages with msgio header.")))

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

(define-public go-tengo
  (package
    (inherit go-github-com-d5-tengo-v2)
    (name "tengo")
    (arguments
     (list
      #:import-path "github.com/d5/tengo/cmd/tengo"
      #:unpack-path "github.com/d5/tengo"
      #:install-source? #f))
    (description
     (string-append (package-description go-github-com-d5-tengo-v2)
                    "\nThis package provides an command line interface (CLI)
tool."))))

(define-public go-tomlv
  (package
    (inherit go-github-com-burntsushi-toml)
    (name "go-tomlv")
    (arguments
     (list
      #:install-source? #f
      #:tests? #f ; no tests.
      #:import-path "github.com/BurntSushi/toml/cmd/tomlv"
      #:unpack-path "github.com/BurntSushi/toml"))
    (description
     (string-append (package-description go-github-com-burntsushi-toml)
                    " This package provides an command line interface (CLI)
tool."))))

(define-public go-ulid
  (package
    (inherit go-github-com-oklog-ulid-v2)
    (name "go-ulid")
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/oklog/ulid/v2/cmd/ulid"
      #:unpack-path "github.com/oklog/ulid/v2"))
    (native-inputs
     (list go-github-com-pborman-getopt))
    (description
     (string-append (package-description go-github-com-oklog-ulid)
                    " This package provides an command line interface (CLI)
tool."))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
