;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017-2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019, 2020 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 HiPhish <hiphish@posteo.de>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020, 2022, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
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
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-public go-github-com-goccmack-gocc
  (package
    (name "go-github-com-goccmack-gocc")
    (version "0.0.0-20230228185258-2292f9e40198")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/goccmack/gocc")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ah1z1bmn9y9sbh2z1jxsjgsrv1rfrzzzi4c4nq646z2n25c2x8s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/goccmack/gocc"
      ;; Test requiring gocc in PATH.
      #:test-flags #~(list "-skip" "TestEmptyKeyword")))
    (propagated-inputs
     (list go-golang-org-x-mod))
    (home-page "https://github.com/goccmack/gocc")
    (synopsis "Parser/Scanner Generator")
    (description
     "Gocc is a compiler kit for Go written in Go.  Gocc generates lexers and
parsers or stand-alone DFAs or parsers from a BNF.  Lexers are DFAs, which
recognise regular languages. Gocc lexers accept UTF-8 input. Gocc parsers are
PDAs, which recognise LR-1 languages. Optional LR1 conflict handling
automatically resolves shift / reduce and reduce / reduce conflicts.")
    (license license:asl2.0)))

(define-public go-github-com-golang-glog
  (package
    (name "go-github-com-golang-glog")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/glog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sfgcf18wg4glcamgq9njmbny17xq0dd14g3094sj5c1cwjij982"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/golang/glog"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (home-page "https://github.com/golang/glog")
    (synopsis "Leveled execution logs for Golang")
    (description
     "This package implements logging analogous to C++ package
@url{https://github.com/google/glog,glog} INFO/ERROR/V setup.  It provides
functions that have a name matched by regex:.")
    (license license:asl2.0)))

(define-public go-github-com-golang-protobuf
  (package
    (name "go-github-com-golang-protobuf")
    (version "1.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/protobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bk7sa9ymi87hd2fv9jamxnxb3qjriamf2nsm8avp6ka37mrkz01"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/golang/protobuf"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-google-golang-org-protobuf))
    (home-page "https://github.com/golang/protobuf")
    (synopsis "Go support for Protocol Buffers")
    (description
     "This package provides Go support for the Protocol Buffers data
serialization format.")
    (license license:bsd-3)))

(define-public go-github-com-google-btree
  (package
    (name "go-github-com-google-btree")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/btree")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k3jfj24sp4qk494wxj055vf5fjwskiydscy4a42s5jiwgcjr9gz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/btree"))
    (home-page "https://github.com/google/btree")
    (synopsis "Simple, ordered, in-memory data structure for Go programs")
    (description
     "This package provides an in-memory B-Tree implementation for Go, useful
as an ordered, mutable data structure.")
    (license license:asl2.0)))

(define-public go-github-com-google-go-cmdtest
  (package
    (name "go-github-com-google-go-cmdtest")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-cmdtest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zkghc60ymxmg19j90r6j7clq3xifh5m9kg1bgr4zpr5sv148x72"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-cmdtest"))
    (propagated-inputs
     (list go-github-com-google-renameio go-github-com-google-go-cmp))
    (home-page "https://github.com/google/go-cmdtest")
    (synopsis "Testing for your CLI")
    (description
     "The cmdtest package simplifies testing of command-line interfaces.  It
provides a simple, cross-platform, shell-like language to express command
execution.  It can compare actual output with the expected output, and can
also update a file with new \"golden\" output that is deemed correct.")
    (license license:asl2.0)))

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

(define-public go-github-com-google-uuid
  (package
    (name "go-github-com-google-uuid")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/uuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "131d01minir79dq6d4jq55018343yidl5cs2bfhynx1klnr7ssam"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/uuid"))
    (home-page "https://github.com/google/uuid/")
    (synopsis "Generate and inspect UUIDs based on RFC 4122 and DCE 1.1")
    (description
     "The uuid package generates and inspects UUIDs based on RFC 4122 and DCE
1.1: Authentication and Security Services.")
    (license license:bsd-3)))

(define-public go-github-com-mmcloughlin-avo
  (package
    (name "go-github-com-mmcloughlin-avo")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mmcloughlin/avo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lv771lb9nxaxqiz7l83k35rc82588xihixxrik6yapg13v675mp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mmcloughlin/avo"))
    (propagated-inputs
     (list go-golang-org-x-arch
           go-golang-org-x-sys
           go-golang-org-x-tools))
    (home-page "https://github.com/mmcloughlin/avo")
    (synopsis "Generate x86 Assembly with Go")
    (description
     "The avo package presents a familiar assembly-like interface that
simplifies development without sacrificing performance.")
    (license license:bsd-3)))

;; XXX: This repository has been archived by the owner on Dec 1, 2021. It is
;; now read-only.
(define-public go-github-com-pkg-errors
  (package
    (name "go-github-com-pkg-errors")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pkg/errors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1761pybhc2kqr6v5fm8faj08x9bql8427yqg6vnfv6nhrasx1mwq"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests fail with a newer version of Golang (1.21) due to some API
      ;; changes in how the module path is calculated which is not reflected
      ;; in tests.
      #:tests? #f
      #:import-path "github.com/pkg/errors"))
    (home-page "https://github.com/pkg/errors")
    (synopsis "Go error handling primitives")
    (description
     "This package provides @code{error}, which offers simple error handling
primitives in Go.")
    (license license:bsd-2)))

(define-public go-github-com-twitchyliquid64-golang-asm
  (package
    (name "go-github-com-twitchyliquid64-golang-asm")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twitchyliquid64/golang-asm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1akw41i0snxqw9lqzmnn4gx6hd5js5dr1vmfkm49wxans4k14vw4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/twitchyliquid64/golang-asm"))
    (home-page "https://github.com/twitchyliquid64/golang-asm")
    (synopsis "Assembler from the Go compiler, in library form")
    (description
     "This package provides a mirror of the assembler from the Go compiler, with
import paths re-written for the assembler to be functional as a standalone
library.")
    (license license:bsd-3)))

(define-public go-github-com-yuin-goldmark
  (package
    (name "go-github-com-yuin-goldmark")
    (version "1.7.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/goldmark")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iz7x1hqdixx8dkcbaa8lr842i59n843mc553jv5grq057s76yjx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yuin/goldmark"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; We need to extend the timeout on some architectures.
                ;; 64 is the default in extra_test.go.
                (setenv "GOLDMARK_TEST_TIMEOUT_MULTIPLIER"
                        (number->string (* 64 5)))))))))
    (home-page "https://github.com/yuin/goldmark/")
    (synopsis "Markdown parser")
    (description
     "This package provides a markdown parser.")
    (license license:expat)))

(define-public go-github-com-yuin-goldmark-emoji
  (package
    (name "go-github-com-yuin-goldmark-emoji")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/goldmark-emoji")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "001dgjaa9crbl1yb803gyq1dbcnnfzvv205ycgd97qw9p4xjg21g"))))
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

(define-public go-golang-org-x-arch
  (package
    (name "go-golang-org-x-arch")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/arch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "104mnfx3v6lwjndjd35ly8r6yb4bb74lq5sq1cqpxw38mqyzqmx2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/arch"
      #:test-subdirs #~(list "arm/..."
                             "arm64/..."
                             "loong64/..."
                             "ppc64/..."
                             ;; "riscv64/..."; failed to build tests
                             "s390x/..."
                             "x86/...")))
    (native-inputs
     (list go-rsc-io-pdf))
    (home-page "https://go.googlesource.com/arch")
    (synopsis "Machine architecture information used by the Go toolchain")
    (description
     "This package provides a machine architecture information used by the Go
toolchain.  The parts needed in the main Go repository are copied in.")
    (license license:bsd-3)))

(define-public go-golang-org-x-crypto
  (package
    (name "go-golang-org-x-crypto")
    (version "0.31.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/crypto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pd3qipz2wb5wbbb44lgbfygxhf9lq5rknf82p2dmaicszn0rd53"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - olang.org/x/crypto/x509roots/fallback
            (for-each delete-file-recursively (list "x509roots/fallback"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/crypto"
      #:phases
      #~(modify-phases %standard-phases
          ;; Network access required: go mod download -json
          ;; github.com/google/wycheproof@v0.0.0-20191219022705-2196000605e4.
          ;;
          ;; internal/wycheproof/wycheproof_test.go
          ;; Download the JSON test files from github.com/google/wycheproof
          ;; using `go mod download -json` so the cached source of the testdata
          ;; can be used in the following tests.
          ;;
          ;; <https://github.com/google/wycheproof> ->
          ;; <https://github.com/C2SP/wycheproof>
          ;;
          ;; The structs for these tests are generated from the schemas
          ;; provided in
          ;; <https://github.com/google/wycheproof/tree/master/schemas> using
          ;; <https://github.com/a-h/generate>.
          (add-after 'unpack 'remove-test-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (find-files "internal/wycheproof" ".*_test\\.go$"))))))))
    (native-inputs
     (list go-golang-org-x-net-bootstrap
           go-golang-org-x-text-bootstrap))
    (propagated-inputs
     (list go-golang-org-x-sys go-golang-org-x-term))
    (home-page "https://go.googlesource.com/crypto/")
    (synopsis "Supplementary cryptographic libraries in Go")
    (description
     "This package provides supplementary cryptographic libraries for the Go
language.")
    (license license:bsd-3)))

(define-public go-golang-org-x-crypto-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-crypto)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/crypto"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-golang-org-x-exp
  (package
    (name "go-golang-org-x-exp")
    (version "0.0.0-20241217172543-b2144cdd0a67")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01dq7llbqqdybv5s372zwlfiyq2syqpfqs7h4lxvbpqjq0aayf60"))
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
      #:skip-build? #t
      #:import-path "golang.org/x/exp"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list
                ;; Disable failing tests: error running `go mod init`:
                ;; go: modules disabled by GO111MODULE=off.
                "TestRelease_gitRepo_uncommittedChanges"
                "TestFailure"
                ;; Delete: want nil discarded elements, got
                ;; 0xc000012858, 0xc000012860
                "TestDeleteClearTail"
                ;; DeleteFunc: want nil discarded elements, got
                ;; 0xc000012910, 0xc000012918
                "TestDeleteFuncClearTail"
                ;; Compact: want nil discarded elements, got
                ;; 0xc000012b30, 0xc000012b38
                "TestCompactClearTail"
                ;; CompactFunc: want nil discarded elements, got
                ;; 0xc000012be8, 0xc000012bf0
                "TestCompactFuncClearTail"
                ;; Replace: want nil discarded element, got
                ;; 0xc000013058
                "TestReplaceClearTail")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "slog/benchmarks")))))))
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

(define-public go-golang-org-x-exp-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-exp)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/exp"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-golang-org-x-exp-typeparams
  (package
    (name "go-golang-org-x-exp-typeparams")
    (version "0.0.0-20241210194714-1829a127f884")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16lc0sgydpr4gbb5c9ygq86jdmq6f9qjf0vv1m5mhh3dggc1fzpp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/exp/typeparams"
      #:unpack-path "golang.org/x/exp"
      ;; Could not import go/ast (can't find import: "go/ast").
      #:test-flags #~(list "-skip" "TestAPIConsistency")))
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
    (version "0.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/image")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xm3cqzh0j6s8m8k6c3rd170qbmz2lwb628jb48cl4wr6ivq5fp9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/image"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build) ; no go files in project's root
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/example")))))))
    (propagated-inputs
     (list go-golang-org-x-text))
    (home-page "https://pkg.go.dev/golang.org/x/image")
    (synopsis "Supplemental Go image libraries")
    (description
     "This package provides supplemental Go libraries for image processing.")
    (license license:bsd-3)))

(define-public go-golang-org-x-image-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-image)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/image"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-golang-org-x-mod
  (package
    (name "go-golang-org-x-mod")
    ;; XXX: To update to 0.22.0+ go-1.23 is required, wich provides
    ;; "go/version" module, see
    ;; <https://cs.opensource.google/go/go/+/refs/tags/
    ;; go1.23.0:src/go/version/version.go>.
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/mod")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rk4vbdrdmiaacx50a1q31hydidwl9rnlcl7rim3f535vyw01fxk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/mod"
      ;; Test tries to acces:
      ;; "http://ct.googleapis.com/logs/argon2020/ct/v1/get-sth": dial tcp:
      ;; lookup ct.googleapis.com
      #:test-flags #~(list "-skip" "TestCertificateTransparency")))
    (native-inputs
     (list go-golang-org-x-tools-bootstrap))
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

(define-public go-golang-org-x-mod-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-mod)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/mod"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-golang-org-x-net
  (package
    (name "go-golang-org-x-net")
    (version "0.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/net")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k3vbxj4dxyki7rflbnad95avz7hnapwr02aa7jqgs79vd9j9k7n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/net"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
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

(define-public go-golang-org-x-net-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-net)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/net"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-golang-org-x-sync
  (package
    (name "go-golang-org-x-sync")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/sync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yp7pcfvy4793mjbfq28hxbr05yg38m0njnwam580xkb54lfwshx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/sync"))
    (home-page "https://go.googlesource.com/sync/")
    (synopsis "Additional Go concurrency primitives")
    (description
     "This package provides Go concurrency primitives in addition to the ones
provided by the language and @code{sync} and @code{sync/atomic} packages.
The package provides several Golang submodules:
@itemize
@item @code{errgroup} - synchronization, error propagation, and Context
cancellation for groups of goroutines working on subtasks of a common task
@item @code{semaphore} - a weighted semaphore implementation
@item @code{singleflight} - a duplicate function call suppression mechanism
@item @code{syncmap} - a concurrent map implementation
@end itemize")
    (license license:bsd-3)))

(define-public go-golang-org-x-sync-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-sync)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/sync"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-golang-org-x-sys
  (package
    (name "go-golang-org-x-sys")
    (version "0.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/sys")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lgsisl36knlr41inqls3w51xcqfl6d3hkacxry0nqx39167b882"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/sys"))
    (home-page "https://go.googlesource.com/sys")
    (synopsis "Go support for low-level system interaction")
    (description
     "This package provides supplemental libraries offering Go support for
low-level interaction with the operating system.")
    (license license:bsd-3)))

(define-public go-golang-org-x-sys-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-sys)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/sys"))
     (native-inputs '())
     (propagated-inputs '()))))

;; This is the only one dependency for esbuild, which is the main tool to
;; produce all Node packages, keep it away from other golang.
(define-public go-golang-org-x-sys-for-esbuild
  (hidden-package
   (package
     (inherit go-golang-org-x-sys)
     (name "go-golang-org-x-sys")
     (version "0.25.0")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://go.googlesource.com/sys")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0hdysrba8imiifb4ksjgbhkzhk1mksm1g3fj59i3bas1zdc5lbgp")))))))

(define-public go-golang-org-x-telemetry
  (package
    (name "go-golang-org-x-telemetry")
    (version "0.0.0-20240912191618-22fe4a1e7b9c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/telemetry")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05gvxiv0yqfclckm2ysavbfy1jpz8v71r2glrcvhjq8wzw90g9gz"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - golang.org/x/telemetry/config
            ;; - golang.org/x/telemetry/godev
            (for-each delete-file-recursively (list "config" "godev"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Tests fail with error: failed to download config
                       ;; module.
                       (list "TestConcurrentStart"
                             "TestDownload"
                             "TestRun_Basic"
                             "TestRun_Concurrent"
                             "TestRun_DebugLog"
                             "TestRun_EmptyUpload"
                             "TestRun_MissingDate"
                             "TestRun_ModeHandling/on"
                             "TestRun_MultipleUploads"
                             "TestRun_Retries"
                             "TestStart") ; no upload occurred on 2786
                       "|"))
      #:import-path "golang.org/x/telemetry"))
    (propagated-inputs
     (list go-golang-org-x-mod
           go-golang-org-x-sync
           go-golang-org-x-sys))
    (home-page "https://go.googlesource.com/telemetry")
    (synopsis "Go Telemetry")
    (description
     "This repository holds the Go Telemetry server code and libraries, used
for hosting @url{https://telemetry.go.dev,telemetry.go.dev} and instrumenting
Go toolchain programs with opt-in telemetry.")
    (license license:bsd-3)))

(define-public go-golang-org-x-telemetry-config
  (package
    (name "go-golang-org-x-telemetry-config")
    (version "0.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/telemetry")
             (commit (go-version->git-ref version
                                          #:subdir "config"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nib4d3p1zknd8m0grkylpd3qfknnw7cffv2v1l4sq0rf30gi04m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/telemetry/config"
      #:unpack-path "golang.org/x/telemetry"))
    (home-page "https://golang.org/x/telemetry")
    (synopsis "Subset of telemetry data for Golang telemetry")
    (description
     "The config package holds the config.json file defining the Go telemetry
upload configuration and contains no actual Go code, and exists only so the
config.json file can be served by module proxies.

An upload configuration specifies the set of values that are permitted in
telemetry uploads: GOOS, GOARCH, Go version, and per-program counters.")
    (license license:bsd-3)))

(define-public go-golang-org-x-term
  (package
    (name "go-golang-org-x-term")
    (version "0.27.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/term")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gfykqmzgwgrb3adlbknjrb96i58bx2q2vjcdvfvwm554gznkgki"))))
    (build-system go-build-system)
    (arguments '(#:import-path "golang.org/x/term"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://pkg.go.dev/golang.org/x/term")
    (synopsis "Go terminal/console support")
    (description "@code{term} provides support functions for dealing with
terminals, as commonly found on Unix systems.")
    (license license:bsd-3)))

(define-public go-golang-org-x-term-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-term)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/term"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-golang-org-x-text
  (package
    (name "go-golang-org-x-text")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/text")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02zh18l5rlr8hg8ipn9r5m4rir3hskp80pzr4ljyfmgy72gxbhlv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/text"))
    (home-page "https://go.googlesource.com/text")
    (native-inputs
     (list go-golang-org-x-mod-bootstrap
           go-golang-org-x-sync-bootstrap
           go-golang-org-x-tools-bootstrap))
    (synopsis "Supplemental Go text processing libraries")
    (description
     "This package provides supplemental Go libraries for text
processing.")
    (license license:bsd-3)))

(define-public go-golang-org-x-text-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-text)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/text"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-golang-org-x-time
  (package
    (name "go-golang-org-x-time")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/time")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m2xfgq3a6y1xckl0al9n03il927z3rp2a8fvb8369035d3al3qh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/time"))
    (home-page "https://godoc.org/golang.org/x/time/rate")
    (synopsis "Supplemental Go time libraries")
    (description
     "This package provides supplemental Go libraries related to
time.")
    (license license:bsd-3)))

(define-public go-golang-org-x-tools
  (package
    (name "go-golang-org-x-tools")
    (version "0.25.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12r0cwsq898vka7jkxwjv1s8y8z2gxzq8z15ssl37y85hhcadkl8"))
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
                          ;; "./go/analysis/..."
                          ;; "./go/callgraph/..."
                          ;; "./go/packages/..."
                          ;; "./go/ssa/..."
                          ;; "./internal/..."
                          ;; "./present/..."
                          ;; "./refactor/eg/..."

                          "./blog/..."  ;
                          "./container/..."
                          "./copyright/..."
                          "./cover/..."
                          "./go/ast/..."
                          "./go/buildutil/..."
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
     (list gccgo-14
           go-github-com-google-go-cmp))
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

(define-public go-golang-org-x-tools-bootstrap
  (hidden-package
   (package
     (inherit go-golang-org-x-tools)
     (arguments
      (list #:skip-build? #t
            #:tests? #f
            #:import-path "golang.org/x/tools"))
     (native-inputs '())
     (propagated-inputs '()))))

(define-public go-golang-org-x-vuln
  (package
    (name "go-golang-org-x-vuln")
    ;; XXX: Newer version of govulncheck requires golang.org/x/telemetry,
    ;; which needs to be discussed if it may be included in Guix.
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/vuln")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0izm18r8ksx4n10an9nxyflc8cgr766qrwfmx5nbk702x80prln9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/vuln"
      ;; ts.RunParallel undefined (type *cmdtest.TestSuite has no field or
      ;; method RunParallel)
      ;;
      ;; go: modules disabled by GO111MODULE=off
      #:test-flags #~(list "-skip" "TestVet|TestGoModTidy|Test58509")
      #:test-subdirs #~(list "internal/..." "scan/..." ".")))
    (propagated-inputs
     (list go-github-com-google-go-cmdtest
           go-github-com-google-go-cmp
           go-golang-org-x-mod
           go-golang-org-x-sync
           go-golang-org-x-telemetry
           go-golang-org-x-tools))
    (home-page "https://golang.org/x/vuln")
    (synopsis "Go Vulnerability Management")
    (description
     "This repository contains packages for accessing and analyzing data from
the @url{https://vuln.go.dev,Go Vulnerability Database}.")
    (license license:bsd-3)))

(define-public go-golang-org-x-xerrors
  (package
    (name "go-golang-org-x-xerrors")
    (version "0.0.0-20240903120638-7835f813f4da")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/xerrors")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1amjwicv2k6mkafa27ghwavc19jy8vi8kfndpj6vrgn0p5rc4kkc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/xerrors"))
    (home-page "https://godoc.org/golang.org/x/xerrors")
    (synopsis "Go 1.13 error values")
    (description
     "This package holds the transition packages for the new Go 1.13 error
values.")
    (license license:bsd-3)))

(define-public go-google-golang-org-genproto-googleapis-rpc
  ;; No release or verion tags, use the latest commit.
  (let ((commit "e70fdf4c4cb4151b7aa3579ce8a3fb662bafe335")
        (revision "0"))
    (package
      (name "go-google-golang-org-genproto-googleapis-rpc")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/googleapis/go-genproto")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0jf332yxgf1c6nj76b1p52907g786gynzd78g899m3vrfa45wswz"))))
      (build-system go-build-system)
      (arguments
       (list
        #:skip-build? #t
        #:import-path "google.golang.org/genproto/googleapis/rpc"
        #:unpack-path "google.golang.org/genproto"))
      (propagated-inputs
       (list go-google-golang-org-protobuf))
      (home-page "https://google.golang.org/genproto")
      (synopsis "Common types for gRPC API")
      (description
       "This package provides a @code{rpc} Google's API gRPC type derived from
@code{google.golang.org/protobuf}.")
      (license license:asl2.0))))

(define-public go-google-golang-org-protobuf
  (package
    (name "go-google-golang-org-protobuf")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/protobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a6l9zcm1za7w9f9n86wjszn9fm53cfacl0liyk3wpsqx4h4x6dl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "google.golang.org/protobuf"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
    (propagated-inputs (list go-github-com-google-go-cmp))
    (home-page "https://google.golang.org/protobuf")
    (synopsis "Go library for Protocol Buffers")
    (description
     "The protobuf package provides a Go implementation of Protocol Buffers, a
language and platform neutral, extensible mechanism for serializing structured
data.  It is a successor to @code{go-github-com-golang-protobuf} with an
improved and cleaner API.")
    (license license:bsd-3)))

(define-public go-rsc-io-pdf
  (package
    (name "go-rsc-io-pdf")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsc/pdf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01qjjwa8nn5a2jzd360xqg5zc8s0i2fpwcn2w2g6y2jgn9wl8x84"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "rsc.io/pdf"))
    (home-page "https://rsc.io/pdf")
    (synopsis "PDF reader for Golang")
    (description "Package pdf implements reading of PDF files.")
    (license license:bsd-3)))

;;;
;;; Executables:
;;;

(define-public govulncheck
  (package
    (inherit go-golang-org-x-vuln)
    (name "govulncheck")
    (arguments
     (list
      #:tests? #f
      #:install-source? #f
      #:import-path "golang.org/x/vuln/cmd/govulncheck"
      #:unpack-path "golang.org/x/vuln"))))

(define-public protoc-gen-go
  (package
    (inherit go-github-com-golang-protobuf)
    (name "protoc-gen-go")
    (arguments
     (list
      #:tests? #f
      #:install-source? #f
      #:import-path "github.com/golang/protobuf/protoc-gen-go"
      #:unpack-path "github.com/golang/protobuf"))
    (synopsis "Protoc plugin to generate a Go protocol buffer package")))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
