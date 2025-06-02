;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>
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

(define-module (gnu packages golang-apps)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz))

;;; Commentary:
;;;
;;; Golang (Go) specific applications which are not inherited from their
;;; source live here e.g linters, language servers, REPL implementations etc.
;;;
;;; If the <cmd/APP> is inherited from its source place it in corresponded
;;; golang-*.scm file in the end after the section "Executables".
;;;
;;; Please: Try to add new variable in alphabetical order.
;;;
;;; Code:


(define-public godef
  (package
    (name "godef")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rogpeppe/godef")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rhhg73kzai6qzhw31yxw3nhpsijn849qai2v9am955svmnckvf4"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "vendor"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rogpeppe/godef"
      ;; The TestGoDef/Modules test fails, because of the lack of Go modules
      ;; support.
      #:test-flags #~(list "-skip" "TestGoDef/GOPATH|TestGoDef/Modules")))
    (inputs
     (list go-golang-org-x-tools
           go-ninefans-net-go))
    (home-page "https://github.com/rogpeppe/godef")
    (synopsis "Print where symbols are defined in Go source code")
    (description "The @command{godef} command prints the source location of
definitions in Go programs.")
    (license license:bsd-3)))

(define-public gomacro
  (package
    (name "gomacro")
    (version "0.0.0-20240506194242-2ff796e3da10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cosmos72/gomacro")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17v3vlq5s5mxplzvs5d414shd2mqkfj3jwxzfgq6cnr9hgr4b9kc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cosmos72/gomacro"
      ;; There are some unexplained test failures (see:
      ;; https://github.com/cosmos72/gomacro/issues/164), and even after
      ;; disabling the problematic tests, the test suite exits uncleanly with
      ;; an exit status of 1.
      #:tests? #f
      #:test-flags
      #~(list "-skip" (string-join '("TestFiles/slow.input"
                                     "TestFromReflect6")
                                   "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-problematic-tests
            (lambda _
              ;; Some test module(s) under go/types fail to build.
              (for-each delete-file
                        (find-files "src/github.com/cosmos72/gomacro/go/types"
                                    "_test\\.go(\\.off)?$")))))))
    (inputs (list go-golang-org-x-tools go-github-com-peterh-liner
                  go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/cosmos72/gomacro")
    (synopsis
     "Interactive Go interpreter and debugger with generics and macros")
    (description
     "@command{gomacro} is an almost complete Go interpreter, implemented in
pure Go.  It offers both an interactive REPL and a scripting mode, and does
not require a Go toolchain at runtime (except in one very specific case:
import of a 3rd party package at runtime).")
    (license license:mpl2.0)))

(define-public gopls
  (package
    (name "gopls")
    ;; XXX: Starting from 0.14.0 gppls needs golang.org/x/telemetry, which
    ;; needs to be discussed if it may be included in Guix.
    (version "0.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (go-version->git-ref version #:subdir "gopls"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dihdw4nzp21hlbwxf6qyhyfgavi1a55lmlyk36czd85v0jcp6a0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "golang.org/x/tools/gopls"
      #:unpack-path "golang.org/x/tools"
      ;; XXX: No tests in project's root, limit to some of subdris, try to
      ;; enable more.
      #:test-subdirs
      #~(list "internal/protocol/..."
              "internal/util/..."
              "internal/vulncheck/...")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'unpack 'override-tools
            (lambda _
              ;; XXX: Write a procedure deleting all but current module source
              ;; to cover case with monorepo.
              (delete-file-recursively "src/golang.org/x/tools")))
          (add-before 'check 'set-env
            (lambda _
              ;; Required for fingerprint_test.TestMatches.
              (setenv "GODEBUG" "gotypesalias=1"))))))
    (native-inputs
     (list go-github-com-fatih-gomodifytags
           go-github-com-google-go-cmp
           go-github-com-jba-templatecheck
           go-golang-org-x-mod
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-telemetry
           go-golang-org-x-text
           go-golang-org-x-tools
           go-golang-org-x-vuln
           go-gopkg-in-yaml-v3
           go-honnef-co-go-tools
           go-mvdan-cc-gofumpt
           go-mvdan-cc-xurls-v2))
    (home-page "https://golang.org/x/tools/gopls")
    (synopsis "Official language server for the Go language")
    (description
     "Pronounced ``Go please'', this is the official Go language server
developed by the Go team.  It provides IDE features to any LSP-compatible
editor.")
    (license license:bsd-3)))

(define-public gore
  (package
    (name "gore")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/x-motemen/gore")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d8ayzni43j1y02g9j2sx1rhml8j1ikbbzmcki2lyi4j0ix5ys7f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23                      ;required by motemen-go-quickfix
      #:import-path "github.com/x-motemen/gore"
      #:install-source? #f
      #:test-flags
      ;; Gore is configured for building modules with Go module
      ;; support, which fails in the build environment for the tests
      ;; making use of that.  Skip them.
      #~(list "-skip" (string-join
                       (list "TestAction_ArgumentRequired"
                             "TestAction_Clear"
                             "TestAction_CommandNotFound"
                             "TestAction_Help"
                             "TestAction_Import"
                             "TestAction_Quit"
                             "TestSessionEval_AutoImport"
                             "TestSessionEval_CompileError"
                             "TestSessionEval_Const"
                             "TestSessionEval_Copy"
                             "TestSessionEval_Declarations"
                             "TestSessionEval_Func"
                             "TestSessionEval_Gomod"
                             "TestSessionEval_Gomod_CompleteImport"
                             "TestSessionEval_Gomod_DeepDir"
                             "TestSessionEval_Gomod_Outside"
                             "TestSessionEval_MultipleValues"
                             "TestSessionEval_NotUsed"
                             "TestSessionEval_QuickFix_evaluated_but_not_used"
                             "TestSessionEval_QuickFix_no_new_variables"
                             "TestSessionEval_QuickFix_used_as_value"
                             "TestSessionEval_Struct"
                             "TestSessionEval_TokenError"
                             "TestSessionEval_import"
                             "TestSession_IncludePackage"
                             "TestSession_completeWord")
                       "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "src/github.com/x-motemen/gore"
                (substitute* "gopls.go"
                  (("\"gopls\"")
                   (format #f "~s" (search-input-file inputs "bin/gopls")))))))
          (replace 'install
            (lambda _
              (with-directory-excursion "src/github.com/x-motemen/gore"
                (invoke "make" "install")))))))
    (native-inputs
     (list go-github-com-motemen-go-quickfix
           go-github-com-peterh-liner
           go-github-com-stretchr-testify
           go-go-lsp-dev-jsonrpc2
           go-go-lsp-dev-protocol
           go-golang-org-x-text
           go-golang-org-x-tools))
    (inputs
     (list gopls))
    (home-page "https://github.com/x-motemen/gore")
    (synopsis "Go REPL with line editing and completion capabilities")
    (description
     "Gore is a Go @acronym{REPL, read-eval-print loop} that offers line
editing and auto-completion.  Some of its features include:
@itemize
@item Line editing with history
@item Multi-line input
@item Package importing with completion
@item Evaluates any expressions, statements and function declarations
@item No ``evaluated but not used'' errors
@item Code completion
@item Showing documents
@item Auto-importing (gore -autoimport)
@end itemize")
    (license license:expat)))
