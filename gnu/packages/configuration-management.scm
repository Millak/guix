;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Josh Holland <josh@inv.alid.pw>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
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

(define-module (gnu packages configuration-management)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-vcs)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public bundlewrap
  (package
    (name "bundlewrap")
    (version "5.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/bundlewrap/bundlewrap")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p0082lwyfkppswm8cpr1yp28y0cm0f8rk3ly3xlym7qyidglkli"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "tests/unit")))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (inputs
     (list python-bcrypt
           python-cryptography
           python-jinja2
           python-librouteros
           python-mako
           python-pyyaml
           python-requests
           python-tomlkit))
    (home-page "https://bundlewrap.org")
    (synopsis "Config management with Python")
    (description
     "BundleWrap is a decentralized configuration management system that is
designed to be powerful, easy to extend and extremely versatile.


By allowing for easy and low-overhead config management, BundleWrap fills the
gap between complex deployments using Chef or Puppet and old school system
administration over SSH.  While most other config management systems rely on a
client-server architecture, BundleWrap works off a repository cloned to local
machine.  It then automates the process of SSHing into servers and making sure
everything is configured the way it's supposed to be.")
    (license license:gpl3)))

(define-public chezmoi
  (package
    (name "chezmoi")
    (version "2.72.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/twpayne/chezmoi")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qfp2iighx5889lssx8m0xv3pw8mysxards4x8vz8bzsflmgm3ig"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "chezmoi.io/chezmoi/v2"
      #:embed-files
      #~(list ".*\\.xml" "words.txt.gz" "betterleaks.toml"
              "cl100k_base.tiktoken.gz"
              ;; For go-github-com-urfave-cli-v3:
              "bash_autocomplete" "powershell_autocomplete.ps1"
              "zsh_autocomplete" "prelude.graphql")
      #:test-subdirs
      ;; XXX: Enable the rest of the tests.
      #~(list "." "internal/chezmoigit" "internal/chezmoilog"
              "internal/archivetest" "internal/chezmoitest"
              "internal/chezmoibubbles" "internal/cmds/lint-whitespace"
              "internal/cmds/execute-template"
              "internal/cmds/generate-install.sh"
              "internal/cmds/lint-commit-messages"
              "assets/chezmoi.io/docs/reference/commands")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'rename-binaries
            (lambda _
              (rename-file
               (string-append #$output "/bin/v2")
               (string-append #$output "/bin/chezmoi")))))))
    (native-inputs
     (list go-filippo-io-age
           go-github-com-azure-azure-sdk-for-go-sdk-azidentity
           go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-azsecrets
           go-github-com-burntsushi-toml
           go-github-com-masterminds-sprig-v3
           go-github-com-shopify-ejson
           go-github-com-alecthomas-assert-v2
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-service-secretsmanager
           go-github-com-bartventer-httpcache
           go-github-com-betterleaks-betterleaks
           go-github-com-bmatcuk-doublestar-v4
           go-github-com-bradenhilton-mozillainstallhash
           go-github-com-charmbracelet-bubbles
           go-github-com-charmbracelet-bubbletea
           go-github-com-charmbracelet-glamour
           go-github-com-charmbracelet-lipgloss
           go-github-com-coreos-go-semver
           go-github-com-fsnotify-fsnotify
           go-github-com-go-git-go-git-v5
           go-github-com-go-sprout-sprout
           go-github-com-go-viper-mapstructure-v2
           go-github-com-goccy-go-yaml
           go-github-com-google-go-github-v72
           go-github-com-google-renameio-v2
           go-github-com-gopasspw-gopass
           go-github-com-itchyny-gojq
           go-github-com-klauspost-compress
           go-github-com-mitchellh-copystructure
           go-github-com-muesli-combinator
           go-github-com-muesli-termenv
           go-github-com-nwaples-rardecode-v2
           go-github-com-pete-woods-go-expect
           go-github-com-rogpeppe-go-internal
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-tailscale-hujson
           go-github-com-tobischo-gokeepasslib-v3
           go-github-com-twpayne-go-pinentry-v4
           go-github-com-twpayne-go-shell
           go-github-com-twpayne-go-vfs-v5
           go-github-com-twpayne-go-xdg-v6
           go-github-com-ulikunitz-xz
           go-github-com-zalando-go-keyring
           go-go-etcd-io-bbolt
           go-golang-org-x-crypto
           go-golang-org-x-oauth2
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-text
           go-gopkg-in-ini-v1
           go-howett-net-plist
           go-mvdan-cc-sh-v3
           go-znkr-io-diff))
    (home-page "https://www.chezmoi.io/")
    (synopsis "Personal configuration files manager")
    (description "This package helps to manage personal configuration files
across multiple machines.")
    (license license:expat)))

(define-public konsave
  (package
    (name "konsave")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Prayag2/konsave")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0454cjcnlwpylia6lb40xzjvm07p3hxmfl21zmlxhl2xjlbjhsg4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false ; no tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-package-data
            ;; Fix detection of default configuration files without which the
            ;; programs fails to invoke.
            (lambda _
              (substitute* "setup.py"
                ((" package_data=.*")
                 " package_data = {'': ['*.yaml']},\n"))))
          (add-before 'sanity-check 'set-home-directory
            ;; sanity-check requires a home directory since importing the
            ;; `const.py' module creates a directory to save configurations.
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (inputs
     (list python-pyyaml))
    (home-page "https://github.com/prayag2/konsave")
    (synopsis "Dotfiles manager")
    (description
     "Konsave is @acronym{CLI, Command Line Program} that lets you backup your
dotfiles and switch to other ones.
Features:
@itemize
@item storing configurations in profiles
@item exporting profiles to '.knsv' files
@item import profiles from '.knsv' files
@item official support for KDE Plasma
@end itemize")
    (license license:gpl3)))

(define-public rcm
  (package
    (name "rcm")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thoughtbot/rcm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zkajv5qk3snfmnnhv9y2qwgvcg5cai9dzcns6n8g30xnjdlhpvj"))
       (modules '((guix build utils)))
       (snippet #~(substitute* "autogen.sh"
                    ;; Contributor list would be generated from git shortlog.
                    (("./maint/autocontrib ((man/rcm.7).mustache)" _ in out)
                     (simple-format #f "sed '/^\\.Sh CONTRIBUTORS/,$d' ~a > ~a"
                       in out))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:parallel-tests? #f              ;multiple tests write to /tmp/test
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-source-shebangs 'patch-tests
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "test/rcdn-hooks-failure.t"
                (("\\$ rcdn\\>") "$ $TESTDIR/../bin/rcdn"))
              (substitute* "test/rcup-hooks-failure.t"
                (("\\$ rcup\\>") "$ $TESTDIR/../bin/rcup"))
              (substitute* '("test/rcrc-tilde.t"
                             "test/rcdn-hooks-failure.t"
                             "test/rcdn-hooks-run-in-order.t"
                             "test/rcup-hooks-failure.t"
                             "test/rcup-hooks-run-in-order.t")
                (("/bin/sh") (search-input-file inputs "/bin/sh")))
              (substitute* "test/rcup-hooks.t"
                (("/usr/bin/env") (search-input-file inputs "/bin/env"))))))))
    (native-inputs (list autoconf automake perl python-cram))
    (home-page "https://github.com/thoughtbot/rcm")
    (synopsis "Management suite for dotfiles")
    (description
     "The rcm suite of tools is for managing dotfiles directories.  This is
a directory containing all the @file{.*rc} files in your home directory
(@file{.zshrc}, @file{.vimrc}, and so on).  These files have gone by many
names in history, such as “rc files” because they typically end in rc
or “dotfiles” because they begin with a period.  This suite is useful
for committing your rc files to a central repository to share, but it also
scales to a more complex situation such as multiple source directories
shared between computers with some host-specific or task-specific files.")
    (license license:bsd-3)))
