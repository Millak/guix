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
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang))

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

(define-public go-cel-dev-expr
  (package
    (name "go-cel-dev-expr")
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/google/cel-spec")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fzy5njwzg48h1mqbfhczyq6hxmbq3yzdivkjh1x8ipj19v4hvfl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cel.dev/expr"))
    (propagated-inputs
     (list go-google-golang-org-genproto-googleapis-rpc
           go-google-golang-org-protobuf))
    (home-page "https://cel.dev/")
    (synopsis "Common Expression Language")
    (description
     "The Common Expression Language (CEL) implements common semantics for
expression evaluation, enabling different applications to more easily
interoperate.")
    (license license:asl2.0)))

(define-public go-github-com-ebitengine-purego
  (package
    (name "go-github-com-ebitengine-purego")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ebitengine/purego")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qkjn7xswbfrly8bwryww0jwfdasig9bfx24dnwryz8iakkyww6f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ebitengine/purego"
      ;; TODO: This failed test should be proper checked.
      ;; ld:
      ;; /tmp/guix-build-go-github-com-ebitengine-purego-0.9.0.drv-0/ccguQKNs.o:
      ;; relocation R_X86_64_32 against `.bss' can not be used when making a
      ;; shared object; recompile with -fPIC ld: failed to set dynamic section
      ;; sizes: bad value collect2: error: ld returned 1 exit status
      #:test-flags #~(list "-skip" "TestNestedDlopenCall")))
    (home-page "https://github.com/ebitengine/purego")
    (synopsis "Library for calling C functions from Go without Cgo")
    (description
     "This package provides a library for calling C functions from Go without
Cgo.

Featues:
@itemize
@item build for other platforms easily without a C compiler
@item efficiently cache entirely Go builds
@item using Cgo generates a C wrapper function for each C function called
@item load symbols at runtime and use it as a plugin system
@item call into other languages that are compiled into shared objects
@item works even with @code{CGO_ENABLED=1} so incremental porting is possible
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-goccmack-gocc
  (package
    (name "go-github-com-goccmack-gocc")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/goccmack/gocc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rv0v0k13lql0z9s9bffkjsan32a0i0m8405w3xng1y0jk3706mh"))))
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
recognise regular languages. Gocc lexers accept UTF-8 input.  Gocc parsers are
PDAs, which recognise LR-1 languages.  Optional LR1 conflict handling
automatically resolves shift / reduce and reduce / reduce conflicts.")
    (license license:asl2.0)))

(define-public go-github-com-golang-glog
  (package
    (name "go-github-com-golang-glog")
    (version "1.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/glog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15gza8cb5qs8brwqjn1lpbm9p5z5332m44gmxz9m0qxkr27lcmhr"))))
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
      #:import-path "github.com/google/go-cmdtest"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda _
              (substitute* "src/github.com/google/go-cmdtest/cmdtest_test.go"
                ;; Since Go 1.24, fmt procedures are checked to use a constant
                ;; format string.
                (("t.Errorf\\(diff)")
                 "t.Errorf(\"%s\", diff)")))))))
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
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-cmp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cys8lz68za30z5cabvwrpnv2pg1ppqxdncmiz8iy2j624a5kg15"))))
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

(define-public go-github-com-matttproud-golang-protobuf-extensions
  (package
    (name "go-github-com-matttproud-golang-protobuf-extensions")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matttproud/golang_protobuf_extensions")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xqsf9vpcrd4hp95rl6kgmjvkv1df4aicfw4l5vfcxcwxknfx2xs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/matttproud/golang_protobuf_extensions"))
    (propagated-inputs
     (list go-github-com-golang-protobuf))
    (home-page "https://github.com/matttproud/golang_protobuf_extensions")
    (synopsis "Support for streaming Protocol Buffer messages for Golang")
    (description
     "This package provides various Protocol Buffer extensions for the Go
language (golang), namely support for record length-delimited message
streaming.")
    (license license:asl2.0)))

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
      #:import-path "github.com/twitchyliquid64/golang-asm"
      #:test-flags
      #~(list "-vet=off")))   ;Go@1.24 forces vet, but tests are not ready yet.
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
    (version "1.7.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/yuin/goldmark")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "183v5bzgwr7ibbj6srnaaq7n98xqifaswa0c01yf693p5l6q3q6m"))))
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
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/goldmark-emoji")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09n5ws797ma47kj0jwg0g2gkwq899kb40ny62r9f44wg6dkrpppr"))))
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
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/arch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14s32fhg9wxvnsnl9szfsbhkxyxcql6dg2qwli055wfly8wly26m"))))
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
    (version "0.43.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://go.googlesource.com/crypto")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d49g86ndfzj40nrichhhsknn6lgl1gh8862dmgsx0l0885kik9i"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - golang.org/x/crypto/argon2/_asm
            ;; - golang.org/x/crypto/blake2b/_asm/AVX2
            ;; - golang.org/x/crypto/blake2b/_asm/standard
            ;; - golang.org/x/crypto/blake2s/_asm
            ;; - golang.org/x/crypto/chacha20poly1305/_asm
            ;; - golang.org/x/crypto/internal/poly1305/_asm
            ;; - golang.org/x/crypto/salsa20/salsa/_asm
            ;; - golang.org/x/crypto/sha3/_asm
            ;; - golang.org/x/crypto/x509roots/fallback
            (for-each delete-file-recursively
                      (list "argon2/_asm"
                            "blake2b/_asm/AVX2"
                            "blake2b/_asm/standard"
                            "blake2s/_asm"
                            "chacha20poly1305/_asm"
                            "internal/poly1305/_asm"
                            "salsa20/salsa/_asm"
                            "sha3/_asm"
                            "x509roots/fallback"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/crypto"
      ;; panic: testing: test using t.Setenv or t.Chdir can not use t.Parallel
      #:test-flags #~(list "-skip" "TestWithPebble")
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
     (list go-golang-org-x-sys
           go-golang-org-x-term))
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
    ;; Note: Beware, the updater gets this wrong.  Take the latest version
    ;; string from <https://pkg.go.dev/golang.org/x/exp?tab=versions>, or try
    ;; "guix import go golang.org/x/exp".
    (version "0.0.0-20250911091902-df9299821621")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jmpq16f2d8i3bnwxa78j384vi5gynfajh34m2nh3vhji3d35777"))
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
                ;; gorelease_test.go:310: error running `go mod init`: go:
                ;; modules disabled by GO111MODULE=off; see 'go help modules'
                "TestRelease_gitRepo_uncommittedChanges"
                ;; constraints_test.go:104:
                ;; /gnu/store/vr0097qq0kl1ansn6iv5smysjh9v7ycd-go-1.24.3/lib/go/bin/go
                ;; mod tidy: exit status 1 go: modules disabled by
                ;; GO111MODULE=off; see 'go help modules'
                "TestFailure")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "slog/benchmarks")))))))
    (native-inputs
     (list go-golang-org-x-tools-go-packages-packagestest))
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
    ;; Note: Beware, the updater gets this wrong.  Take the latest version
    ;; string from
    ;; <https://pkg.go.dev/golang.org/x/exp/typeparams?tab=versions>.
    (version "0.0.0-20251009144603-d2f985daa21b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kn9bcbrkdvg4f8kvc0cdahyyy72w2jf9v9c95k67k0b46fwh3i6"))))
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
    (version "0.27.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/image")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kc75lbfb0m9xp0idcqlpcis6xahblw2q7cj6vg9lmblxzqy5nvh"))))
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
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/mod")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17sjk98d3qwh9s6gqjmfy07z0gyj5hyv5a9kyg4si3yjfzbnwhx7"))))
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
    (version "0.40.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/net")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zsh08wbamzlvlwvlw6slgcqhaa59rwz9pq01lbyjigw6cfww406"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/net"
      #:test-flags
      ;; Golang does not support "-race" on ARM, one test fails with error:
      ;; ThreadSanitizer: unsupported VMA range.
      #~(list #$@(if (target-arm?) '("-skip" "TestRace") '()))))
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
    (version "0.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/sync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gpa1v6wnp1bszk5iyiakq1j37icgxswj3iysdrdyp0yadw0pm30"))))
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
    (version "0.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/sys")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19m090xd3abysvk1y07fhhd025k3s456i71ww0pq0b2pzsva5ra2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:test-flags
      #~(list #$@(if (target-arm?)
                     '("-skip" (string-join
                                (list "TestParseOrigDstAddr/udp4"
                                      "TestIoctlGetEthtoolDrvinfo"
                                      "TestIoctlGetEthtoolTsInfo"
                                      "TestRlimitAs")
                                "|"))
                     '()))
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
    ;; Beware: the updater gets this wrong.  Use the latest commit and its
    ;; matching date.
    (version "0.0.0-20250529002037-25d2f7894191")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/telemetry")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ymqig10vyrmzkali1wqhxrrb3fjvl7z9wmzf5g0dydb9a8ng42l"))
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
                             "TestStart" ;no upload occurred on 2786
                             ;; TestLoadedChartsAreValid fails with "go: list
                             ;; -cannot be used with GO111MODULE=off"
                             "TestLoadedChartsAreValid")
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
    (version "0.48.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/telemetry")
             (commit (go-version->git-ref version
                                          #:subdir "config"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lzybsdibr71y6n3x8qh37yh9vfwdmmdb6vksqimg4ayys373q7x"))))
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
    (version "0.32.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/term")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09q25x265yyy4jfjqpm10x8jai30wcbhb7bqgkjll4gw2sz3zhz1"))))
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
    (version "0.25.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/text")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r9532ml0psfby89agf20q23qzwfikhydl8q77ad5y73xvdx89lf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/text"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list
                ;; TestLinking fails with error: "dict_test.go:19: size(base)
                ;; - size(compact) = 4929873 - 4898852 = was 31021; want >
                ;; 1.5MB
                "TestLinking"
                "TestFullCycle")         ;requires go module support
               "|"))))
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
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/time")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bqgxv7b3n69h4mi4hwr51pfr1hr6s1h6k7nb3dl32dryy7xwr12"))))
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
    (version "0.37.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://go.googlesource.com/tools")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l0srynl2cikmz089q5vqgifz6ll2ic1762fbfvak26vqbcx0knz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Submodules with their own go.mod files and packaged separately:
           ;;
           ;; - golang.org/x/tools/cmd/auth
           ;; - golang.org/x/tools/gopls
           (delete-file-recursively "gopls")
           (delete-file-recursively "cmd/auth")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "golang.org/x/tools"
      #:test-subdirs
      #~(list "./blog/..."
              "./container/..."
              "./copyright/..."
              "./cover/..."
              "./go/ast/..."
              "./go/buildutil/..."
              "./go/cfg/..."
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
              "./txtar/...")
      #:test-flags
      #~(list "-skip" (string-join
                       (list
                        ;; The GenericPaths test fails with "invalid memory
                        ;; address or nil pointer dereference".
                        "TestGenericPaths"
                        ;; The ordering and paths tests fails because they
                        ;; can't find test packages (perhaps because we do not
                        ;; support Go modules).
                        "TestOrdering" "TestPaths")
                       "|"))))
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

(define-public go-golang-org-x-tools-go-expect
  (package
    (name "go-golang-org-x-tools-go-expect")
    (version "0.1.1-deprecated")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://go.googlesource.com/tools")
              (commit (go-version->git-ref version
                                           #:subdir "go/expect"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sjvngpahkb5x573i855fjlb1fdmr6n269nmb5xxnbabjb27mnvg"))
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
            (delete-all-but "go" "expect")
            (delete-all-but "." "go")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/tools/go/expect"
      #:unpack-path "golang.org/x/tools"))
    (propagated-inputs (list go-golang-org-x-mod))
    (home-page "https://golang.org/x/tools")
    (synopsis "Interpreting structured comments in Golang")
    (description
     "Package expect provides support for interpreting structured comments in
Go source code (including go.mod and go.work files) as test expectations.")
    (license license:bsd-3)))

(define-public go-golang-org-x-tools-go-packages-packagestest
  (package
    (name "go-golang-org-x-tools-go-packages-packagestest")
    (version "0.1.1-deprecated")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://go.googlesource.com/tools")
              (commit (go-version->git-ref version
                                           #:subdir "go/packages/packagestest"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sjvngpahkb5x573i855fjlb1fdmr6n269nmb5xxnbabjb27mnvg"))
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
            (delete-all-but "go" "packages")
            (delete-all-but "go/packages" "packagestest")
            (delete-all-but "." "go")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/tools/go/packages/packagestest"
      #:unpack-path "golang.org/x/tools"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-golang-org-x-tools-go-expect))
    (home-page "https://golang.org/x/tools")
    (synopsis "Temporary testing projects for Golang")
    (description
     "Package packagestest creates temporary projects on disk for testing go
tools on.")
    (license license:bsd-3)))

(define-public go-golang-org-x-tools-godoc
  (package
    (name "go-golang-org-x-tools-godoc")
    (version "0.1.0-deprecated")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (go-version->git-ref version
                                          #:subdir "godoc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1blk22apy424j9v58lfy4pxnrgh93yqchqhxsnf78dmx4vx5yi9r"))
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
            (delete-all-but "." "godoc")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/tools/godoc"
      #:unpack-path "golang.org/x/tools"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-github-com-yuin-goldmark))
    (home-page "https://golang.org/x/tools")
    (synopsis "Code for running a godoc server.")
    (description
     "This package provides most of the code for running a @code{godoc}
 server.")
    (license license:bsd-3)))

(define-public go-golang-org-x-vuln
  (package
    (name "go-golang-org-x-vuln")
    ;; XXX: Newer version of govulncheck requires golang.org/x/telemetry,
    ;; which needs to be discussed if it may be included in Guix.
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/vuln")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "004hmcaahgj0ajvpkrhbvs6av1nas8302vzy9is9msxyya3mclkp"))))
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
    (native-inputs
     (list go-golang-org-x-tools-go-packages-packagestest))
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
    (version "1.36.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/protobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lp1a6bcwdiil4my0aq85ranxf2k757m8q0ss9658jyrh5g7av79"))))
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
