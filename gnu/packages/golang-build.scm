;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 HiPhish <hiphish@posteo.de>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 hackeryarn <artemchernyak@gmail.com>
;;; Copyright © 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
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

(define-module (gnu packages golang-build)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages))

;;; Commentary:
;;;
;;; Modules (libraries) which are part of the Golang project but outside the
;;; main Golang tree, see <https://pkg.go.dev/golang.org/x>
;;;
;;; Since they are bound to be relied on by many, their dependencies should be
;;; kept minimal, and this module should not depend on other modules
;;; containing Golang packages.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public go-github-com-yuin-goldmark
  (package
    (name "go-github-com-yuin-goldmark")
    (version "1.7.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/goldmark")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01807xs8501cyhkrrgg6k9ghl9jrw6dp0ry9knygck48canckxs2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yuin/goldmark"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (home-page "https://github.com/yuin/goldmark/")
    (synopsis "Markdown parser")
    (description
     "This package provides a markdown parser.")
    (license license:expat)))

(define-public go-golang-org-x-crypto
  (package
    (name "go-golang-org-x-crypto")
    (version "0.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/crypto")
             (commit (string-append "v" version))))
       (file-name (string-append "go.googlesource.com-crypto-"
                                 version "-checkout"))
       (sha256
        (base32 "08124qkp0l0blq6rw68llfbf84kp99255q0yvxyv27c05b0wkh66"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/crypto"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; Source-only package
         (delete 'build))))
    (propagated-inputs
     (list go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-text))
    (home-page "https://go.googlesource.com/crypto/")
    (synopsis "Supplementary cryptographic libraries in Go")
    (description "This package provides supplementary cryptographic libraries
for the Go language.")
    (license license:bsd-3)))

(define-public go-golang-org-x-exp
  (package
    (name "go-golang-org-x-exp")
    (version "0.0.0-20240613232115-7f521ea00fb8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ccjgv19w5p9sbcq12czmfnkjwv3b7hfljifwax6r9wk4dx0fcn7"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Submodules with their own go.mod files and packed as separated
           ;; packages:
           ;;
           ;; - golang.org/x/exp/event
           ;; - golang.org/x/exp/jsonrpc2
           ;; - golang.org/x/exp/shiny
           ;; - golang.org/x/exp/sumbdb
           ;; - golang.org/x/exp/typeparams
           (for-each delete-file-recursively
                     (list "event" "jsonrpc2" "shiny" "sumdb" "typeparams"))))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/exp"
       ;; Source-only package
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://golang.org/x/exp")
    (synopsis "Experimental and deprecated Go packages")
    (description "This subrepository holds experimental and deprecated (in the
@code{old} directory) packages.")
    (license license:bsd-3)))

(define-public go-golang-org-x-exp-typeparams
  (package
    (name "go-golang-org-x-exp-typeparams")
    (version "0.0.0-20240707233637-46b078467d37")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17pwikql9x1bm5ci0kk4mlad7smkph0cgq1pi2b43gnhjz8m96l0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/exp/typeparams"
      #:unpack-path "golang.org/x/exp"))
    (home-page "https://pkg.go.dev/golang.org/x/exp/typeparams")
    (synopsis "Golang common utilities for writing tools")
    (description
     "Package typeparams contains common utilities for writing tools that
interact with generic Go code, as introduced with Go 1.18.

Many of the types and functions in this package are proxies for the new APIs
introduced in the standard library with Go 1.18.  For example, the
typeparams.Union type is an alias for @code{go/types.Union}, and the
@code{ForTypeSpec} function returns the value of the
@code{go/ast.TypeSpec.TypeParams} field.  At Go versions older than 1.18 these
helpers are implemented as stubs, allowing users of this package to write code
that handles generic constructs inline,even if the Go version being used to
compile does not support generics.")
    (license license:bsd-3)))

(define-public go-golang-org-x-image
  (package
    (name "go-golang-org-x-image")
    (version "0.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/image")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d7zwdsg06km24vhx6dzk1w26wpi3yhx9jfkf9jnsp5chv5pzlw3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/image"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: go-build-system can't install/build submodules.
          (delete 'build)
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/example"))))
          (add-before 'check 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          ;; tiff/reader_test.go:557:14: too many errors
                          (list "tiff/reader_test.go"
                                "tiff/writer_test.go")))))
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-golang-org-x-text))
    (home-page "https://pkg.go.dev/golang.org/x/image")
    (synopsis "Supplemental Go image libraries")
    (description
     "This package provides supplemental Go libraries for image processing.")
    (license license:bsd-3)))

(define-public go-golang-org-x-mod
  (package
    (name "go-golang-org-x-mod")
    (version "0.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/mod")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02wilb8q2bp6qhqcrbjxq1pjy3y5k8p11pxlg481609zx4rjiszc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/mod/"
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  ;; Source-only package
                  (delete 'build))))
    (home-page "https://golang.org/x/mod")
    (synopsis "Tools to work directly with Go module mechanics")
    (description
     "This repository holds packages for writing tools that work directly
with Go module mechanics.  That is, it is for direct manipulation of Go
modules themselves.

The specific case of loading packages should still be done by invoking the
@command{go} command, which remains the single point of truth for package
loading algorithms.")
    (license license:bsd-3)))

(define-public go-golang-org-x-net
  (let ((commit "8e0e7d8d38f2b6d21d742845570dde2902d06a1d")
        (revision "0"))
    (package
      (name "go-golang-org-x-net")
      (version (git-version "0.5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/net")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1fidlcn3vcz42v2lc0rpmqh3bz08bcklj6jvnmz2vvgc481ci5hy"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "golang.org/x/net"
        ;; Source-only package
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            ;; Source-only package
            (delete 'build))))
      (propagated-inputs
       (list go-golang-org-x-sys go-golang-org-x-term go-golang-org-x-text))
      (home-page "https://go.googlesource.com/net")
      (synopsis "Go supplemental networking libraries")
      (description "This package provides supplemental Go networking libraries.")
      (license license:bsd-3))))

;; XXX: Not in use by any other packages, consider to remove or merge with
;; go-golang-org-x-net.
(define-public go-golang-org-x-net-0.17
  (let ((commit "b225e7ca6dde1ef5a5ae5ce922861bda011cfabd")
        (revision "0"))
    (package
      (inherit go-golang-org-x-net)
      (name "go-golang-org-x-net")
      (version (git-version "0.17.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/net")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17zhim2m0r8nyy18g2lsawxm4rawix2qbjyn80x9vc6jc8fv05m9")))))))

(define-public go-golang-org-x-sync
  (package
    (name "go-golang-org-x-sync")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/sync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03yq3pnjwqxqy1cvbkaa39ca2b9cli1k5wnz76l3a65n9fafai6q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/sync"
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
    (home-page "https://go.googlesource.com/sync/")
    (synopsis "Additional Go concurrency primitives")
    (description
     "This package provides Go concurrency primitives in addition to the ones
provided by the language and @code{sync} and @code{sync/atomic} packages.
The package provides several Golang submodules:
@itemize
@item @code{errgroup} - synchronization, error propagation, and Context
cancelation for groups of goroutines working on subtasks of a common task
@item @code{semaphore} - a weighted semaphore implementation
@item @code{singleflight} - a duplicate function call suppression mechanism
@item @code{syncmap} - a concurrent map implementation
@end itemize")
    (license license:bsd-3)))

(define-public go-golang-org-x-sys
  (package
    (name "go-golang-org-x-sys")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/sys")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "000fcyi863sbmamdn3cwfv3s3z9ls5l34xnjavcbgjs591ghki8y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/sys"
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
    (home-page "https://go.googlesource.com/sys")
    (synopsis "Go support for low-level system interaction")
    (description "This package provides supplemental libraries offering Go
support for low-level interaction with the operating system.")
    (license license:bsd-3)))

(define-public go-golang-org-x-term
  (package
    (name "go-golang-org-x-term")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/term")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16s3d62fgdhiqvcib61s5pwxp08hhrmzx8bdv5zk1w1krjizdarl"))))
    (build-system go-build-system)
    (arguments '(#:import-path "golang.org/x/term"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://pkg.go.dev/golang.org/x/term")
    (synopsis "Go terminal/console support")
    (description "@code{term} provides support functions for dealing with
terminals, as commonly found on Unix systems.")
    (license license:bsd-3)))

(define-public go-golang-org-x-text
  (package
    (name "go-golang-org-x-text")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/text")
             (commit (string-append "v" version))))
       (file-name (string-append "go.googlesource.com-text-"
                                 version "-checkout"))
       (sha256
        (base32 "0pmn0i1xbpwvzl4cdgmjqcsk9vckhqrq6699fnr9mkglh4xj3p7a"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "golang.org/x/text"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://go.googlesource.com/text")
    (synopsis "Supplemental Go text processing libraries")
    (description "This package provides supplemental Go libraries for text
processing.")
    (license license:bsd-3)))

(define-public go-golang-org-x-time
  (let ((commit "9d24e82272b4f38b78bc8cff74fa936d31ccd8ef")
        (revision "2"))
    (package
      (name "go-golang-org-x-time")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/time")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1f5nkr4vys2vbd8wrwyiq2f5wcaahhpxmia85d1gshcbqjqf8dkb"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/time"
         ;; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
         ;; Source-only package
           (delete 'build))))
      (home-page "https://godoc.org/golang.org/x/time/rate")
      (synopsis "Supplemental Go time libraries")
      (description "This package provides supplemental Go libraries related to
time.")
      (license license:bsd-3))))

(define-public go-golang-org-x-tools
  (package
    (name "go-golang-org-x-tools")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jn73k6qiwa70cafdmyzczsimwqyzgl9wh9vmdjs96svf5nfzb5a"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; gopls versions are tagged separately, and it is a
           ;; separate Guix package.
           (delete-file-recursively "gopls")))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "golang.org/x/tools"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; Source-only package
         (delete 'build))))
    (propagated-inputs
     (list go-github-com-yuin-goldmark
           go-golang-org-x-mod
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://go.googlesource.com/tools/")
    (synopsis "Tools that support the Go programming language")
    (description "This package provides miscellaneous tools that support the
Go programming language.")
    (license license:bsd-3)))

(define-public go-golang-org-x-xerrors
  (let ((commit "5ec99f83aff198f5fbd629d6c8d8eb38a04218ca")
        (revision "0"))
    (package
      (name "go-golang-org-x-xerrors")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/xerrors")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dbzc3gmf2haazpv7cgmv97rq40g2xzwbglc17vas8dwhgwgwrzb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/xerrors"))
      (home-page "https://godoc.org/golang.org/x/xerrors")
      (synopsis "Go 1.13 error values")
      (description "This package holds the transition packages for the new Go
1.13 error values.")
      (license license:bsd-3))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
