;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 HiPhish <hiphish@posteo.de>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 hackeryarn <artemchernyak@gmail.com>
;;; Copyright © 2022 (unmatched-parenthesis <paren@disroot.org>
;;; Copyright © 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2024 Hilton Chain <hako@ultrarare.space>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc))

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

(define-public go-github-com-golang-protobuf
  (package
    (name "go-github-com-golang-protobuf")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/protobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03f1w2cd4s8a3xhl61x7yjx81kbzlrjpvnnwmbhqnz814yi7h43i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/golang/protobuf"
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
     (list go-google-golang-org-protobuf))
    (home-page "https://github.com/golang/protobuf")
    (synopsis "Go support for Protocol Buffers")
    (description
     "This package provides Go support for the Protocol Buffers data
serialization format.")
    (license license:bsd-3)))

(define-public go-github-com-google-go-cmp
  (package
    (name "go-github-com-google-go-cmp")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-cmp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n1j4hi50bl05pyys4i7y417k9g6k1blslj27z327qny7kkdl2ma"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-cmp/cmp"
      #:unpack-path "github.com/google/go-cmp"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? import-path inputs #:allow-other-keys)
              (when tests?
                ;; The tests fail when run with gccgo.
                (let ((gccgo? (false-if-exception
                               (search-input-file inputs "/bin/gccgo"))))
                  (if gccgo?
                      (format #t "skipping tests with gccgo compiler~%")
                      ;; XXX: Workaround for go-build-system's lack of Go
                      ;; modules support.
                      (with-directory-excursion (string-append "src/" import-path)
                        (invoke "go" "test" "-v" "./..."))))))))))
    (synopsis "Determine equality of values in Go")
    (home-page "https://github.com/google/go-cmp")
    (description
     "This package is intended to be a more powerful and safer
alternative to @code{reflect.DeepEqual} for comparing whether two values are
semantically equal.")
    (license license:bsd-3)))

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
                ;; We need to extend the timeout on some architectures.
                ;; 64 is the default in extra_test.go.
                (setenv "GOLDMARK_TEST_TIMEOUT_MULTIPLIER"
                        (number->string (* 64 5)))
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (home-page "https://github.com/yuin/goldmark/")
    (synopsis "Markdown parser")
    (description
     "This package provides a markdown parser.")
    (license license:expat)))

(define-public go-github-com-yuin-goldmark-emoji
  (package
    (name "go-github-com-yuin-goldmark-emoji")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/goldmark-emoji")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mx8rkxd3ksvgi41jvf365x9mf00sxiqq4wm75x4sasd2lgcbrl4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yuin/goldmark-emoji"))
    (propagated-inputs
     (list go-github-com-yuin-goldmark))
    (home-page "https://github.com/yuin/goldmark-emoji")
    (synopsis "Emoji extension for the goldmark markdown parser")
    (description
     "This package provides an emoji is a extension for the
@url{http://github.com/yuin/goldmark,goldmark}.")
    (license license:expat)))

(define-public go-golang-org-x-crypto
  (package
    (name "go-golang-org-x-crypto")
    (version "0.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/crypto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rc5zl3gxfy4wcwqjibqgnqj8wdm0v9ff25vkyirzb71l343ydlx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/crypto"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-test-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (list
                           ;; Network access requried: go mod download -json
                           ;; github.com/google/wycheproof@v0.0.0-20191219022705-2196000605e4.
                           "internal/wycheproof/aead_test.go"
                           "internal/wycheproof/aes_cbc_test.go"
                           "internal/wycheproof/dsa_test.go"
                           "internal/wycheproof/ecdh_stdlib_test.go"
                           "internal/wycheproof/ecdh_test.go"
                           "internal/wycheproof/ecdsa_test.go"
                           "internal/wycheproof/eddsa_test.go"
                           "internal/wycheproof/hkdf_test.go"
                           "internal/wycheproof/hmac_test.go"
                           "internal/wycheproof/rsa_oaep_decrypt_test.go"
                           "internal/wycheproof/rsa_pss_test.go"
                           "internal/wycheproof/rsa_signature_test.go"
                           "internal/wycheproof/wycheproof_test.go")))))
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v"
                          ;; acme - cycle with go-golang-org-x-net
                          "./argon2/..."
                          "./bcrypt/..."
                          "./blake2b/..."
                          "./blake2s/..."
                          "./blowfish/..."
                          "./bn256/..."
                          "./cast5/..."
                          "./chacha20/..."
                          "./chacha20poly1305/..."
                          "./cryptobyte/..."
                          "./curve25519/..."
                          "./ed25519/..."
                          "./hkdf/..."
                          "./internal/..."
                          "./md4/..."
                          "./nacl/..."
                          "./ocsp/..."
                          "./openpgp/..."
                          "./otr/..."
                          "./pbkdf2/..."
                          "./pkcs12/..."
                          "./poly1305/..."
                          "./ripemd160/..."
                          "./salsa20/..."
                          "./scrypt/..."
                          "./sha3/..."
                          "./ssh/..."
                          "./tea/..."
                          "./twofish/..."
                          "./x509roots/..."
                          "./xtea/..."
                          "./xts/..."))))))))
    (propagated-inputs
     (list go-golang-org-x-sys go-golang-org-x-term))
    (home-page "https://go.googlesource.com/crypto/")
    (synopsis "Supplementary cryptographic libraries in Go")
    (description
     "This package provides supplementary cryptographic libraries for the Go
language.")
    (license license:bsd-3)))

(define-public go-golang-org-x-exp
  (package
    (name "go-golang-org-x-exp")
    (version "0.0.0-20240808152545-0cdaa3abc0fa")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sa03fm57326qnchvfza7am7jjlz32l1yyqbdvy6mknw5bmp8a71"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
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
     (list
      #:import-path "golang.org/x/exp"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                ;; Benchmark requires other modules to pass tests, delete them.
                (delete-file-recursively "slog/benchmarks")
               (substitute* (find-files "." "\\_test.go$")
                 ;; Disable failing tests: error running `go mod init`: go:
                 ;; modules disabled by GO111MODULE=off; see 'go help modules'
                 ;; , exit status 1
                 (("TestFailure") "OffTestFailure")
                 (("TestRelease_gitRepo_uncommittedChanges")
                  "OffTestRelease_gitRepo_uncommittedChanges")))))
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-golang-org-x-mod
           go-golang-org-x-tools))
    (home-page "https://golang.org/x/exp")
    (synopsis "Experimental and deprecated Go packages")
    (description
     "This subrepository holds experimental and deprecated (in the @code{old}
directory) packages.")
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
    (version "0.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/image")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nb1wvy0xr0s9xylzf336vghkblkp5b9sq2hqh6wdav1awnpry90"))))
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
    (version "0.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/mod")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vrzahgr08idiz9g2ksngwsjhyaa0rryn4vh0lm54kkxgv9pcv7f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/mod"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-test-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (list
                           ;; Break cycle: go-golang-org-x-mod ->
                           ;; go-golang-org-x-tools -> go-golang-org-x-mod.
                           "zip/zip_test.go"
                           ;; Trying to access
                           ;; <http://ct.googleapis.com/logs/argon2020/ct/v1/get-sth>.
                           "sumdb/tlog/ct_test.go")))))
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
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
  (package
    (name "go-golang-org-x-net")
    (version "0.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/net")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fh4rnqsxc652lhncnrdb3qcsw811565qb9619z21z0p1fdgzlar"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/net"
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
     (list go-golang-org-x-crypto
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-text))
    (home-page "https://go.googlesource.com/net")
    (synopsis "Go supplemental networking libraries")
    (description
     "This package provides supplemental Go networking libraries.")
    (license license:bsd-3)))

(define-public go-golang-org-x-sync
  (package
    (name "go-golang-org-x-sync")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/sync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jssyq49g5z7dbhdb9bnzsb8gigvsy5f4aczbzn5paz07v9wbjxs"))))
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
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/sys")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j5hhwrbgjh5bnwqr9g1c32175j43rqlrpv199ay1b970iwiqd4l"))))
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
    (version "0.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/term")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pdv60bm4gfvgf98lyms92qm32zc4f4h4w1fagrhzllwflmsca8h"))))
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
    (version "0.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/text")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jqvh4mmg3db2slh5z7fxdhkgazyld5l858rwnbpfxvfrw0k9w1h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/text"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v"
                          "./cases/..."
                          ;; cmd - cycle with go-golang-org-x-tools
                          "./collate/..."
                          "./currency/..."
                          "./date/..."
                          "./encoding/..."
                          "./feature/..."
                          "./internal/..."
                          "./language/..."
                          ;; message - cycle with go-golang-org-x-tools
                          "./number/..."
                          "./runes/..."
                          "./search/..."
                          "./secure/..."
                          "./transform/..."
                          "./unicode/..."
                          "./width/..."))))))))
    (home-page "https://go.googlesource.com/text")
    (synopsis "Supplemental Go text processing libraries")
    (description
     "This package provides supplemental Go libraries for text
processing.")
    (license license:bsd-3)))

(define-public go-golang-org-x-time
  (package
    (name "go-golang-org-x-time")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/time")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14z7f1yx3r393f94yjv09mrsfaydq6da7pswv8wvk327mxa56vw1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/time"
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
    (home-page "https://godoc.org/golang.org/x/time/rate")
    (synopsis "Supplemental Go time libraries")
    (description
     "This package provides supplemental Go libraries related to
time.")
    (license license:bsd-3)))

(define-public go-golang-org-x-tools
  (package
    (name "go-golang-org-x-tools")
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "007n9bmhxn2yspvmcdbj8h0l132vhakszv4i0bd65cvravskcw8y"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; gopls versions are tagged separately, and it is a
           ;; separate Guix package.
           (delete-file-recursively "gopls")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/tools"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v"
                          ;; TODO: They contain final project executable builds,
                          ;; would be packed separately.
                          ;; - cmd
                          ;; - godoc

                          ;; FIXME: Figure out why they are failing:
                          ;; "./internal/..."
                          ;; "./present/..."
                          ;; "./refactor/eg/..."
                          ;; "./go/ssa/..."
                          ;; "./go/packages/..."
                          ;; "./go/analysis/..."

                          "./blog/..."  ;
                          "./container/..."
                          "./copyright/..."
                          "./cover/..."
                          "./go/ast/..."
                          "./go/buildutil/..."
                          "./go/callgraph/..."
                          "./go/cfg/..."
                          "./go/expect/..."
                          "./go/gccgoexportdata/..."
                          "./go/gcexportdata/..."
                          "./go/internal/..."
                          "./go/loader/..."
                          "./go/types/..."
                          "./imports/..."
                          "./playground/..."
                          "./refactor/importgraph/..."
                          "./refactor/rename/..."
                          "./refactor/satisfy/..."
                          "./txtar/..."))))))))
    (native-inputs
     (list gccgo-14))
    (propagated-inputs
     (list go-github-com-yuin-goldmark
           go-golang-org-x-mod
           go-golang-org-x-net
           go-golang-org-x-sync))
    (home-page "https://go.googlesource.com/tools/")
    (synopsis "Tools that support the Go programming language")
    (description
     "This package provides miscellaneous tools that support the
Go programming language.")
    (license license:bsd-3)))

(define-public go-golang-org-x-xerrors
  (let ((commit "104605ab7028f4af38a8aff92ac848a51bd53c5d")
        (revision "1"))
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
          (base32 "051xfwx95vq7yhmsy8p9rq0qw67bzvimhz1icjssahwrjndm7h92"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/xerrors"))
      (home-page "https://godoc.org/golang.org/x/xerrors")
      (synopsis "Go 1.13 error values")
      (description "This package holds the transition packages for the new Go
1.13 error values.")
      (license license:bsd-3))))

(define-public go-google-golang-org-protobuf
  (package
    (name "go-google-golang-org-protobuf")
    (version "1.31.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/protobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xf18kzz96hgfy1vlbnydrizzpxkqj2iamfdbj3dx5a1zz5mi8n0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "google.golang.org/protobuf"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; XXX Failing on i686-linux:
                  ;; panic: unaligned 64-bit atomic operation
                  (("TestDynamicTypesExtensionNotFound")
                   "OffTestDynamicTypesExtensionNotFound")
                  (("TestDynamicTypesFilesChangeAfterCreation")
                   "OffTestDynamicTypesFilesChangeAfterCreation")
                  (("TestDynamicTypesFindExtensionByNameOrNumber")
                   "OffTestDynamicTypesFindExtensionByNameOrNumber")))))
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs (list go-github-com-google-go-cmp))
    (home-page "https://google.golang.org/protobuf")
    (synopsis "Go library for Protocol Buffers")
    (description
     "The protobuf package provides a Go implementation of Protocol Buffers, a
language and platform neutral, extensible mechanism for serializing structured
data.  It is a successor to @code{go-github-com-golang-protobuf} with an
improved and cleaner API.")
    (license license:bsd-3)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
