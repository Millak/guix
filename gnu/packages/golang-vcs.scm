;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 David Thompson <davet@gnu.org>
;;; Copyright © 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages haskell-apps)
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
      #:go go-1.23
      #:import-path "github.com/jiangxin/goconfig"))
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

;; TODO: Delete all node_modules in pkg/runner/testdata/actions.
(define-public go-github-com-nektos-act
  (package
    (name "go-github-com-nektos-act")
    (version "1.24.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://code.forgejo.org/forgejo/act.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fk5l44jycyzzlvxr0312ayns1rl15vl2kjc6nh726sjlghpa0d7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/nektos/act"
      #:embed-files #~(list ".*\\.json")
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Network access is required.
                       (list "TestArtifactFlow/GHSL-2023-004"
                             "TestArtifactFlow/upload-and-download"
                             "TestFindGitRemoteURL"
                             "TestGitCloneExecutor"
                             "TestGitFindRef"
                             "TestHandler_gcCache"
                             "TestImageExistsLocally"
                             "TestHandler"
                             "TestJobExecutor"
                             ;; Running Docker is required.
                             "TestDocker"
                             ;; Something else...
                             "TestActionCache"
                             "TestInterpolate"
                             "TestRunContext_EvalBool"
                             "TestDryrunEvent"
                             "TestEvaluateRunContext/toJSON"
                             "TestEvaluateRunContext/toJson"
                             "TestGetGitHubContext"
                             "TestMaskValues"
                             "TestRunActionInputs"
                             "TestRunDifferentArchitecture"
                             "TestRunEvent"
                             "TestRunEventHostEnvironment"
                             "TestRunEventPullRequest"
                             "TestRunEventSecrets"
                             "TestRunMatrixWithUserDefinedInclusions"
                             "TestRunSkipped"
                             "TestRunWithService"
                             "TestSetupEnv"
                             "TestStepActionRemotePreThroughActionToken")
                       "|"))
      #:phases
      #~(modify-phases %standard-phases
          ;; The buildkit dependency tree is *massive* and the only thing
          ;; that's used is the dockerignore module, which is just an alias to
          ;; github.com/moby/patternmatcher which this library already depends
          ;; on!
          (add-after 'unpack 'fix-dockerignore
            (lambda _
              (substitute* "src/github.com/nektos/act/pkg/container/docker_build.go"
                (("\"github.com/moby/buildkit/frontend/dockerfile/dockerignore\"")
                 "dockerignore \"github.com/moby/patternmatcher/ignorefile\"")))))))
    (native-inputs
     (list go-github-com-go-git-go-billy-v5
           go-github-com-stretchr-testify
           go-gotest-tools-v3))
    (propagated-inputs
     (list go-dario-cat-mergo
           go-github-com-adrg-xdg
           go-github-com-alecaivazis-survey-v2
           go-github-com-andreaskoch-go-fswatch
           go-github-com-creack-pty
           go-github-com-distribution-reference
           go-github-com-docker-cli
           go-github-com-docker-distribution
           go-github-com-docker-docker
           go-github-com-docker-go-connections
           go-github-com-go-git-go-git-v5
           go-github-com-gobwas-glob
           go-github-com-golang-jwt-jwt-v5
           go-github-com-imdario-mergo
           go-github-com-joho-godotenv
           go-github-com-julienschmidt-httprouter
           go-github-com-kballard-go-shellquote
           go-github-com-masterminds-semver
           go-github-com-mattn-go-isatty
           go-github-com-moby-patternmatcher
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-selinux
           go-github-com-pkg-errors
           go-github-com-rhysd-actionlint
           go-github-com-sabhiram-go-gitignore
           go-github-com-sirupsen-logrus
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-timshannon-bolthold
           go-go-etcd-io-bbolt
           go-golang-org-x-term
           go-google-golang-org-protobuf
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/nektos/act")
    (synopsis "Run GitHub Actions locally")
    (description
     "Helper tool to run @url{https://developer.github.com/actions/,GitHub
Actions} locally.")
    (license license:expat)))

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

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
