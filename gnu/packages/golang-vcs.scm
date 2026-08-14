;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 David Thompson <davet@gnu.org>
;;; Copyright © 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2025 Aleksandr Lebedev <alex.lebedev2003@icloud.com>
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

(define-module (gnu packages golang-vcs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages code)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages version-control))

;;; Commentary:
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-code-forgejo-org-f3-gof3-v3
  (package
    (name "go-code-forgejo-org-f3-gof3-v3")
    (version "3.11.39")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://code.forgejo.org/f3/gof3.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gisxivmsy1r6kn4pcn96ry03mmh6y6hl26yi4zkihlj9m48brsb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "code.forgejo.org/f3/gof3/v3"
      #:unpack-path "code.forgejo.org/f3/gof3/v3"
      #:build-flags
      #~(list "-tags" "'netgo osusergo'"
              (string-append "-ldflags="
                             "-X code.forgejo.org/f3/gof3/v3/cmd.Version="
                             #$version))
      #:embed-files
      ;; For go-github-com-urfave-cli-v3:
      #~(list "bash_autocomplete"
              "powershell_autocomplete.ps1"
              "zsh_autocomplete"
              "prelude.graphql"
              ;; For go-github-com-santhosh-tekuri-jsonschema-v6:
              "applicator"
              "content"
              "core"
              "format"
              "format-annotation"
              "format-assertion"
              "meta-data"
              "schema"
              "unevaluated"
              "validation")
    #:test-flags
    #~(list "-vet=off"
            "-skip" (string-join
                     ;; Depends on internal helper command "internal/hoverfly".
                     (list "TestInternal_Hoverfly"
                           ;; Test tries to resolve DNS on 4.4.4.4 to clone
                           ;; local directory.
                           "TestUtil_Exec_CommandTimeout")
                     "|"))))
    (native-inputs
     (list git-minimal/pinned
           go-github-com-google-go-cmp
           go-github-com-stretchr-testify
           go-github-com-urfave-cli-v3))
    (propagated-inputs
     (list go-github-com-42wim-httpsig
           go-github-com-aio-arch-graphlib
           ;; go-github-com-davidmz-go-pageant  ;Windows only
           go-github-com-hashicorp-go-version
           go-github-com-santhosh-tekuri-jsonschema-v6
           go-gitlab-com-gitlab-org-api-client-go-0.116
           go-golang-org-x-crypto))
    (home-page "https://code.forgejo.org/f3/gof3")
    (synopsis "Friendly Forge Format (F3)")
    (description
     "As a command or as a library, @code{GoF3} provides a single operation:
mirroring.  The origin and destination are designated by the URL of a forge
and a path to the resource.")
    (license license:expat)))

(define-public go-code-gitea-io-actions-proto-go-ping
  (package
    (name "go-code-gitea-io-actions-proto-go-ping")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/gitea/actions-proto-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gdrsr7kx20nhp1r54xyrq4gcwxvyzv636bzmsrchikffhq773b6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "code.gitea.io/actions-proto-go/ping/v1"
      #:unpack-path "code.gitea.io/actions-proto-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-connectrpc-com-connect))
    (home-page "https://code.gitea.io/actions-proto-go")
    (synopsis "Helper for the Gitea Action runner")
    (description
     "This package provides a helper for the Gitea Action runer.")
    (license license:expat)))

(define-public go-code-gitea-io-actions-proto-go-runner
  (package
    (name "go-code-gitea-io-actions-proto-go-runner")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/gitea/actions-proto-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gdrsr7kx20nhp1r54xyrq4gcwxvyzv636bzmsrchikffhq773b6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "code.gitea.io/actions-proto-go/runner/v1"
      #:unpack-path "code.gitea.io/actions-proto-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-connectrpc-com-connect))
    (home-page "https://code.gitea.io/actions-proto-go")
    (synopsis "Action runner for Gitea")
    (description
     "This package provides an Action runner for the Gitea forge.")
    (license license:expat)))

(define-public go-github-com-git-lfs-gitobj-v2
  (package
    (name "go-github-com-git-lfs-gitobj-v2")
    (version "2.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/git-lfs/gitobj")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sd7y4xbx00js1g2az4nq8g5lvsm4d7nqr3v4kxy8fxrfzdm63j9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/git-lfs/gitobj/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/git-lfs/gitobj")
    (synopsis "Read and write git objects")
    (description
     "This package reads and writes loose and packed (objects found in git
packfiles) Git objects.  It uses the pack package to search pack index files
and locate the corresponding delta-base chain in the appropriate pack file.
If gitobj can't find a loose object with the appropriate SHA-1, it will search
the repository's packfile(s) instead.  If it finds an object in a packfile, it
will reconstruct the object along its delta-base chain and return it.")
    (license license:expat)))

(define-public go-github-com-git-lfs-pktline
  (let ((commit "ca444d533ef1e474d0aab99cdbeed9b048d65241")
        (revision "1"))
    (package
      (name "go-github-com-git-lfs-pktline")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/git-lfs/pktline")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0mxp4c59g5b3y20ilf90b6cagbk2b473vsmdz159y4hssvi1ac7r"))))
      (build-system go-build-system)
      (arguments `(#:import-path "github.com/git-lfs/pktline"))
      (native-inputs
       (list go-github-com-stretchr-testify))
      (home-page "https://github.com/git-lfs/pktline")
      (synopsis "Git pkt-line Go toolkit")
      (description
       "This package is a Go language toolkit for reading and writing files
using the Git pkt-line format used in various Git operations.")
      (license license:expat))))

(define-public go-github-com-go-git-gcfg
  (package
    (name "go-github-com-go-git-gcfg")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-git/gcfg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lb14z4j35pwz2b2rbykkpsq515spwbndb00gwn2xlrzn949xb83"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-git/gcfg"
      ;; pass; got 10, error <nil>
      ;; failed to parse "a" as int: expected integer; *ptr==0
      #:test-flags #~(list "-skip" "TestParseInt|TestScanFully")))
    (propagated-inputs
     (list go-github-com-pkg-errors
           go-gopkg-in-warnings-v0))
    (home-page "https://github.com/go-git/gcfg")
    (synopsis "Gcfg reads INI-style configuration files into Go structs")
    (description "Gcfg reads INI-style configuration files into Go structs.")
    (license license:bsd-3)))

(define-public go-github-com-go-git-gcfg-v2
  (package
    (inherit go-github-com-go-git-gcfg)
    (name "go-github-com-go-git-gcfg-v2")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-git/gcfg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08mdrcifml8bvl8hs36xm3j2bczb1phi5zfyvwzlid5zw06qrjl9"))))
    (arguments
     (list
      #:import-path "github.com/go-git/gcfg/v2"))))

(define-public go-github-com-go-git-go-git-v5
  (package
    (name "go-github-com-go-git-go-git-v5")
    (version "5.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-git/go-git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mgzwwmq1awai9n8vkjp8xpq26hvivl53g57f1k2cgg6bkj13r2a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ;requires network connection
      #:import-path "github.com/go-git/go-git/v5"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'setup
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((git #$(this-package-native-input "git-minimal"))
                     (git-bin (string-append git "/bin"))
                     (git-exe (string-append git-bin "/git")))
                (setenv "GIT_DIST_PATH=" git)
                (setenv "GIT_EXEC_PATH=" git-bin)
                (setenv "HOME" (getcwd))
                (invoke git-exe "config" "--global" "user.email" "gha@example.com")
                (invoke git-exe "config" "--global" "user.name" "GitHub Actions")))))))
    (native-inputs
     (list git-minimal/pinned
           go-github-com-stretchr-testify
           go-gopkg-in-check-v1))
    (propagated-inputs
     (list go-dario-cat-mergo
           go-github-com-armon-go-socks5
           go-github-com-elazarl-goproxy
           go-github-com-emirpasic-gods
           go-github-com-gliderlabs-ssh
           go-github-com-go-git-gcfg
           go-github-com-go-git-go-billy-v5
           go-github-com-go-git-go-git-fixtures-v4
           go-github-com-golang-groupcache
           go-github-com-google-go-cmp
           go-github-com-jbenet-go-context
           go-github-com-kevinburke-ssh-config
           go-github-com-pjbgf-sha1cd
           go-github-com-protonmail-go-crypto
           go-github-com-sergi-go-diff
           go-github-com-skeema-knownhosts
           go-github-com-xanzy-ssh-agent
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-text))
    (home-page "https://github.com/go-git/")
    (synopsis "Git implementation library")
    (description "This package provides a Git implementation library.")
    (license license:asl2.0)))

(define-public go-github-com-go-git-go-git-v6
  (package
    (inherit go-github-com-go-git-go-git-v5)
    (name "go-github-com-go-git-go-git-v6")
    (version "6.0.0-alpha.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/go-git/go-git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1whmxb0035c7qxnr3lkypcdrdlcg3mc49d20c1kk1q4zyx2v4zgl"))))
    (arguments
     (substitute-keyword-arguments arguments
       ((#:import-path _) "github.com/go-git/go-git/v6")))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-github-com-go-git-go-git-v5)
       (replace "go-github-com-go-git-go-billy-v5"
         go-github-com-go-git-go-billy-v6)
       (replace "go-github-com-go-git-gcfg"
         go-github-com-go-git-gcfg-v2)))))

(define-public go-github-com-cli-go-gh-v2
  (package
    (name "go-github-com-cli-go-gh-v2")
    (version "2.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cli/go-gh")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a918ll60h65abs14mpnclk1w3468mi7khasixif04ihxd0rcsns"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/cli/go-gh/v2"
      #:embed-files #~(list ".*\\.xml")
      ;; Network access or git configuration are required.
      #:test-flags #~(list "-skip" "TestRemotes|TestNewHTTPClient")))
    (native-inputs
     (list go-github-com-alecthomas-chroma-v2
           go-github-com-leaanthony-go-ansi-parser
           go-github-com-stretchr-testify
           go-gopkg-in-h2non-gock-v1))
    (propagated-inputs
     (list go-github-com-alecaivazis-survey-v2
           go-github-com-charmbracelet-glamour
           go-github-com-charmbracelet-lipgloss
           go-github-com-cli-browser
           go-github-com-cli-safeexec
           go-github-com-cli-shurcool-graphql
           go-github-com-google-shlex
           go-github-com-henvic-httpretty
           go-github-com-itchyny-gojq
           go-github-com-makenowjust-heredoc
           go-github-com-masterminds-sprig-v3
           go-github-com-mgutz-ansi
           go-github-com-muesli-reflow
           go-github-com-muesli-termenv
           go-github-com-thlib-go-timezone-local
           go-golang-org-x-term
           go-golang-org-x-text
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/cli/go-gh")
    (synopsis "Go library for the GitHub CLI")
    (description
     "Package gh is a library for CLI Go applications to help interface with
the gh CLI tool, and the @code{GitHub} API.")
    (license license:expat)))

(define-public go-github-com-cli-go-internal
  (package
    (name "go-github-com-cli-go-internal")
    (version "0.0.0-20241025142207-6c48bcd5ce24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cli/go-internal")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2rnmsdj22hg8hz9xv2fj1vs1k9xfz8rvz9bm1cg3ddq33hl4a5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/cli/go-internal"
      #:test-flags
      ;; TestSimple/cover requires network access to proxy.golang.org.
      #~(list "-skip" "TestSimple/cover")))
    (propagated-inputs
     (list go-golang-org-x-mod
           go-golang-org-x-sys
           go-golang-org-x-tools))
    (home-page "https://github.com/cli/go-internal")
    (synopsis "Internal utilities for GitHub CLI")
    (description
     "This package provides internal utilities for the GitHub CLI.  It is a
hard fork of @code{testscript} to allow the @code{gh} maintainers to explore
its use while providing flexibility for modifications and extensions.")
    (license license:bsd-3)))

(define-public go-github-com-cli-shurcool-graphql
  (package
    (name "go-github-com-cli-shurcool-graphql")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cli/shurcooL-graphql")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w5ixfr4ia35n8adadr2rsca3f04bq36fxkgh27fw7rd0mxf199l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cli/shurcooL-graphql"))
    (home-page "https://github.com/cli/shurcooL-graphql")
    (synopsis "GraphQL client implementation for GitHub CLI")
    (description
     "This package provides a GraphQL client implementation forked from
@code{shurcooL/graphql}, customized for use in the GitHub CLI.")
    (license license:expat)))

(define-public go-github-com-jiangxin-goconfig
  (package
    (name "go-github-com-jiangxin-goconfig")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jiangxin/goconfig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dgmwa8dzzafxcpd4j5vmdfmqn2xvah3qd9rfnihywiw45748hg1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jiangxin/goconfig"
      #:test-flags #~(list "-vet=off")))
    (native-inputs
     (list git-minimal/pinned
           go-github-com-jiu2015-gotestspace
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-golang-groupcache
           go-github-com-spf13-pflag))
    (home-page "https://github.com/jiangxin/goconfig")
    (synopsis "Go parsing library for .gitconfig files")
    (description
     "@code{goconfig} is a Go library for parsing configuration files that
have the same syntax as @file{.gitconfig} files.  It understands multiple
values configuration, and can parse configurations include via
@code{include.path} directives.  @code{includeIf.*.path} directives are not
supported yet.")
    (license license:gpl2+)))

(define-public go-github-com-rhysd-actionlint
  (package
    (name "go-github-com-rhysd-actionlint")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rhysd/actionlint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xbsrcvklxn0lppikabwrizav945jk85d0mz16zc3spxc80plrvn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rhysd/actionlint"
      ;; Several tests try to download stuff from raw.githubusercontent.com;
      ;; skip them.
      #:test-flags #~(list "-skip"
                           (string-join
                            '("TestMain"          ;XXX: segfaults
                              "TestUpdate"        ;XXX: segfaults
                              "TestFetchRemoteYAML"
                              "TestWriteOutdatedActionAsJSONL"
                              "TestDetectNewRelease"
                              "TestDetectNoRelease"
                              "TestCouldNotFetch"
                              "TestDetectErrorBadRequest"
                              "TestFetchOK"
                              "TestFetchError/not_found")
                            "|"))))
    (native-inputs
     ;; Test dependencies.
     (append
      (list python-pyflakes)
      (if (supported-package? shellcheck)
          (list shellcheck)
          '())))
    (propagated-inputs
     (list go-gopkg-in-yaml-v3
           go-golang-org-x-sys
           go-golang-org-x-sync
           go-github-com-yuin-goldmark
           go-github-com-robfig-cron-v3
           go-github-com-mattn-go-shellwords
           go-github-com-mattn-go-runewidth
           go-github-com-mattn-go-colorable
           go-github-com-google-go-cmp
           go-github-com-fatih-color
           go-github-com-bmatcuk-doublestar-v4))
    (home-page "https://github.com/rhysd/actionlint")
    (synopsis "Statically check GitHub Action workflow files")
    (description
     "Package @code{actionlint} is the implementation of actionlint linter.
It's a static checker for GitHub Actions workflow files.")
    (license license:expat)))

(define-public go-github-com-sourcegraph-go-ctags
  (package
    (name "go-github-com-sourcegraph-go-ctags")
    (properties '((commit . "5ec1554485f9b47aeebf34a6856e8ab4ea1c831b")
                  (revision . "0")
                  (go-pseudo-version . "0.0.0-20260626114452-5ec1554485f9")))
    (version (git-version "0.0.0"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sourcegraph/go-ctags")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xd6xd24i98kgzngj2wlk6c7p96i1ij3ynrndc2k0n837a4kgrvb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sourcegraph/go-ctags"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CTAGS_COMMAND"
                      (search-input-file inputs "bin/ctags")))))))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-hexops-autogold
           universal-ctags))
    (home-page "https://github.com/sourcegraph/go-ctags")
    (synopsis "Go wrapper for universal-ctags")
    (description
     "This package provides a Go wrapper for @code{universal-ctags}.")
    (license license:asl2.0)))

(define-public go-github-com-sourcegraph-zoekt
  (package
    (name "go-github-com-sourcegraph-zoekt")
    (properties '((commit . "c6cd01494dc04d60883f7ae4c4e02ccdc97647c3")
                  (revision . "1")
                  (go-pseudo-version . "0.0.0-20260812144518-c6cd01494dc0")))
    (version (git-version "0.0.0"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sourcegraph/zoekt")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17isqmp49gkl88i0f6gy9xqx6a8d2zqsvh6sx4blmic5fp9lvvzx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sourcegraph/zoekt"
      #:unpack-path "github.com/sourcegraph/zoekt"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-shell-path
            (lambda* (#:key inputs unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (substitute* (find-files "." "\\.go$")
                  (("/bin/sh")
                   (search-input-file inputs "bin/sh")))))))))
    (native-inputs
     ;; Tests and cmd/ inputs which don't have to be propagated.
     (list git-minimal/pinned
           go-github-com-adalogics-go-fuzz-headers
           go-github-com-felixge-fgprof
           go-github-com-google-go-cmp
           go-github-com-google-slothfs
           go-github-com-sourcegraph-log
           go-github-com-stretchr-testify
           go-github-com-xeipuuv-gojsonschema
           go-pgregory-net-rapid))
    (propagated-inputs
     (list go-cloud-google-com-go-profiler
           go-code-gitea-io-sdk-gitea
           go-github-com-andygrunwald-go-gerrit
           go-github-com-bmatcuk-doublestar-v4
           go-github-com-cespare-xxhash-v2
           go-github-com-dustin-go-humanize
           go-github-com-fsnotify-fsnotify
           go-github-com-gfleury-go-bitbucket-v1
           go-github-com-go-enry-go-enry-v2
           go-github-com-go-git-go-billy-v5
           go-github-com-go-git-go-git-v5
           go-github-com-gobwas-glob
           go-github-com-google-go-github-v78
           go-github-com-grafana-regexp
           go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus
           go-github-com-grpc-ecosystem-go-grpc-middleware-v2
           go-github-com-mxk-go-flowrate
           go-github-com-opentracing-opentracing-go
           go-github-com-peterbourgon-ff-v3
           go-github-com-pkg-errors
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-procfs
           go-github-com-roaringbitmap-roaring-v2
           go-github-com-rs-xid
           go-github-com-shirou-gopsutil-v3
           go-github-com-sourcegraph-go-ctags
           go-github-com-sourcegraph-mountinfo
           go-github-com-uber-jaeger-client-go
           go-github-com-uber-jaeger-lib
           go-github-com-wasilibs-go-re2
           go-gitlab-com-gitlab-org-api-client-go
           go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
           go-go-opentelemetry-io-contrib-propagators-jaeger
           go-go-opentelemetry-io-contrib-propagators-ot
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-bridge-opentracing
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
           go-go-opentelemetry-io-otel-sdk
           go-go-opentelemetry-io-otel-trace
           go-go-uber-org-atomic
           go-go-uber-org-automaxprocs
           go-go-uber-org-multierr
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-google-golang-org-grpc
           go-google-golang-org-protobuf))
    (home-page "https://github.com/sourcegraph/zoekt")
    (synopsis "Fast trigram based code search")
    (description
     "Zoekt is a text search engine intended for use with source code,
supporting fast substring and regexp matching on source code, with a rich
query language that includes boolean operators (and, or, not).  It can search
individual repositories, and search across many repositories in a large
codebase.  Zoekt ranks search results using a combination of code-related
signals like whether the match is on a symbol.")
    (license license:asl2.0)))

(define-public go-gitlab-com-digitalxero-go-conventional-commit
  (package
    (name "go-gitlab-com-digitalxero-go-conventional-commit")
    (version "1.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.com/digitalxero/go-conventional-commit.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1191j3k9ma5sv8w2zsw8gb1407561zll0ca8hwy0dja6s0b3z0xy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitlab.com/digitalxero/go-conventional-commit"))
    (home-page "https://gitlab.com/digitalxero/go-conventional-commit")
    (synopsis "Go parser for conventional commits")
    (description
     "This package provides a Go parser for conventional-commit messages
@url{https://www.conventionalcommits.org/}.")
    (license license:expat)))

(define-public go-github-com-xanzy-go-gitlab
  (package
    (name "go-github-com-xanzy-go-gitlab")
    (version "0.114.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xanzy/go-gitlab")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "136iik1pqggdk2z3yz4wh5z05wp9sb6j1rpbf33bjn5djqxcxbbf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xanzy/go-gitlab"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-go-querystring
           go-github-com-hashicorp-go-cleanhttp
           go-github-com-hashicorp-go-retryablehttp
           go-golang-org-x-oauth2
           go-golang-org-x-time))
    (home-page "https://github.com/xanzy/go-gitlab")
    (synopsis "GitLab Go SDK")
    (description
     "This package provides a GitLab API client enabling Go programs to
interact with GitLab in a simple and uniform way.")
    (license license:asl2.0)))

;;;
;;; Executables:
;;;

(define-public f3-cli
  (package/inherit go-code-forgejo-org-f3-gof3-v3
    (name "f3-cli")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:import-path _) "code.forgejo.org/f3/gof3/v3/main")
       ((#:install-source? #t #t) #f)
       ((#:skip-build? #t #t) #f)
       ((#:tests? #t #t) #f)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'rename-binary
              (lambda _
                (with-directory-excursion #$output
                  (rename-file "bin/main"
                               "bin/f3-cli"))))))))
    (native-inputs
     (append
      (package-native-inputs go-code-forgejo-org-f3-gof3-v3)
      (package-propagated-inputs go-code-forgejo-org-f3-gof3-v3)))
    (inputs '())
    (propagated-inputs '())))

(define-public git-sync
  (package
    (name "git-sync")
    (version "4.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kubernetes/git-sync")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1faif57081yajmh0lfi2l8j9imkq1zv2nlccg84izi520rwjd6f6"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "k8s.io/git-sync"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-main-path
            ;; Build fails if that commit is kept.
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "main.go"
                  (("// import .k8s.io/git-sync/cmd/git-sync.") ""))))))))
    (native-inputs
     (list go-github-com-go-logr-logr
           go-github-com-golang-jwt-jwt-v4
           go-github-com-prometheus-client-golang
           go-github-com-spf13-pflag
           go-go-uber-org-goleak
           go-golang-org-x-sys))
    (home-page "https://github.com/kubernetes/git-sync")
    (synopsis "Keep repository in sync with the upstream")
    (description
     "git-sync is a simple command that pulls a git repository into a local
directory.  It is a perfect \"sidecar\" container in Kubernetes - it can
periodically pull files down from a repository so that an application can
consume them.")
    (license license:asl2.0)))

(define-public zoekt
  (package/inherit go-github-com-sourcegraph-zoekt
    (name "zoekt")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:import-path _ ) "github.com/sourcegraph/zoekt/cmd/...")
       ((#:install-source? _ #t) #f)
       ((#:tests? _ #t) #f)))
    (native-inputs
     (append
      (package-native-inputs go-github-com-sourcegraph-zoekt)
      (package-propagated-inputs go-github-com-sourcegraph-zoekt)))
    (propagated-inputs '())
    (inputs '())
    (description
     "This package provides a collection of Zoekt command line utilities:
@itemize
@item zoekt - supports searching over an index directory or shard
@item zoekt-archive-index - indexes a git archive
@item zoekt-dynamic-indexserver - starts a server to manage dynamic indexing
@item zoekt-git-clone - fetches all repos of a user or organization and clones
them
@item zoekt-git-index - indexes a single git repository
@item zoekt-index - indexes a directory of files
@item zoekt-indexserver - starts a service that periodically reindexes
repositories
@item zoekt-local-sync - synchronizes a Zoekt index with Git repositories
discovered under local directory roots
@item zoekt-merge-index - merges a set of index shards into a compound shard
@item zoekt-mirror-bitbucket-server - fetches all repos of a bitbucket
project, optionally of a specific type, and clones them
@item zoekt-mirror-gerrit - fetches all repos of a Gerrit host
@item zoekt-mirror-gitea - fetches all repos of a Gitea user or organization
and clones them
@item zoekt-mirror-github - fetches all repos of a GitHub user or organization
and clones them
@item zoekt-mirror-gitiles - fetches all repos of a Gitiles host
@item zoekt-mirror-gitlab - fetches all repos for a user from GitLab
@item zoekt-repo-index - indexes repository that uses the
@url{https://android.googlesource.com/tools/repo, Android repo tool}
@item zoekt-sourcegraph-indexserver - periodically reindexes repositories from
a Sourcegraph instance
@item zoekt-test - compares the zoekt results with raw substring search
@item zoekt-webserver - starts a server that responds to search queries, using
an index generated by another program such as zoekt-indexserver
@end itemize
")))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
