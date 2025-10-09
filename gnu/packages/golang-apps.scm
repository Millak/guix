;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-public go-chroma
  (package
    (name "go-chroma")
    (version "2.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/alecthomas/chroma")
              (commit (string-append "v" version ))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05w4hnfcxqdlsz7mkc0m3jbp1aj67wzyhq5jh8ldfgnyjnlafia3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:embed-files #~(list ".*\\.xml")
      #:import-path "github.com/alecthomas/chroma/cmd/chroma"
      #:unpack-path "github.com/alecthomas/chroma"))
    (native-inputs
     (list go-github-com-alecthomas-chroma-v2
           go-github-com-alecthomas-kong
           go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty))
    (home-page "https://github.com/alecthomas/chroma")
    (synopsis "General purpose syntax highlighter")
    (description
     "This package implements a syntax highlighter for a long list of
programming languages.  It takes source code and other structured text and
converts it into syntax highlighted HTML, ANSI-coloured text, etc.  Chroma is
based heavily on @url{http://pygments.org/, Pygments}, and includes
translators for Pygments lexers and styles.")
    (license license:expat)))

(define-public go-fxlint
  (package
    (name "go-fxlint")
    (version "0.0.0-20250513223611-0a30575829d4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/uber-go/fx")
              (commit (go-version->git-ref version
                                           #:subdir "tools"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "058pnalgm23in7ijz1zakmn5appss5la9v0yzrh8psi8d1rzidsj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:install-source? #f
      #:import-path "go.uber.org/fx/tools/cmd/fxlint"
      #:unpack-path "go.uber.org/fx"))
    (native-inputs
     (list go-golang-org-x-tools))
    (home-page "https://go.uber.org/fx")
    (synopsis "Verify FX events")
    (description
     "This Package implements a Go analysis pass that verifies that an
@code{fxevent.Logger} implementation handles all known fxevent types.  As a
special case for no-op or fake fxevent.Loggers, it ignores implementations
that handle none of the event types.")
    (license license:expat)))

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
    (native-inputs
     ;; XXX: Remove in the next refresh cycle.
     (list go-golang-org-x-tools-go-packages-packagestest))
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
    (version "0.0.0-20250712144029-20095acfbf18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cosmos72/gomacro")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w7gcrympnxwrscmhwahx3anm2yp1ali8xqh1s23q93gyznzcpj5"))))
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
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/x-motemen/gore")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x67npdwmrpa11xg93756bfbc2mxrbf04pk8dk4i8hx67wr95z0h"))))
    (build-system go-build-system)
    (arguments
     (list
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
                             "TestAction_Doc"
                             "TestAction_Help"
                             "TestAction_Import"
                             "TestAction_Quit"
                             "TestAction_Type"
                             "TestSessionEval_AutoImport"
                             "TestSessionEval_CompileError"
                             "TestSessionEval_Const"
                             "TestSessionEval_Copy"
                             "TestSessionEval_Declarations"
                             "TestSessionEval_Func"
                             "TestSessionEval_Gomod"
                             "TestSessionEval_Gomod_AutoImport"
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
                             "TestSession_ExtraFiles"
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
