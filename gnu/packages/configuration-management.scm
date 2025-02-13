;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-vcs)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public chezmoi
  (package
    (name "chezmoi")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/chezmoi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kjjbns80pcd6wh51kmhpw8xlm57cqgq205qp2i2z78n82h3fijc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/twpayne/chezmoi"
      #:embed-files #~(list ".*\\.xml")
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestScript/autocommit"
                             "TestScript/autopush"
                             "TestScript/bitwarden"
                             "TestScript/builtingit"
                             "TestScript/cd_unix"
                             "TestScript/completion"
                             "TestScript/doctor_unix"
                             "TestScript/edit"
                             "TestScript/editconfig"
                             "TestScript/git"
                             "TestScript/gopass"
                             "TestScript/init"
                             "TestScript/issue1213"
                             "TestScript/keepassxc"
                             "TestScript/lastpass"
                             "TestScript/merge_unix"
                             "TestScript/modify_unix"
                             "TestScript/onepassword"
                             "TestScript/pass"
                             "TestScript/runscriptdir_unix"
                             "TestScript/script"
                             "TestScript/script_unix"
                             "TestScript/scriptonce_unix"
                             "TestScript/scriptorder_unix"
                             "TestScript/scriptsubdir_unix"
                             "TestScript/secret"
                             "TestScript/state_unix"
                             "TestScript/templatefuncs"
                             "TestScript/update"
                             "TestScript/vault")
                       "|"))))
    (native-inputs
     (list go-github-com-masterminds-sprig-v3
           go-github-com-bmatcuk-doublestar-v4
           go-github-com-bradenhilton-mozillainstallhash
           go-github-com-charmbracelet-glamour-0.3
           go-github-com-coreos-go-semver
           go-github-com-go-git-go-git-v5
           go-github-com-google-go-github-v36
           go-github-com-google-gops
           go-github-com-google-renameio
           go-github-com-mitchellh-mapstructure
           go-github-com-muesli-combinator
           go-github-com-pelletier-go-toml
           go-github-com-rogpeppe-go-internal
           go-github-com-rs-zerolog
           go-github-com-sergi-go-diff
           go-github-com-spf13-afero
           go-github-com-spf13-cobra
           go-github-com-spf13-viper
           go-github-com-stretchr-testify
           go-github-com-twpayne-go-shell
           go-github-com-twpayne-go-vfs-v3
           go-github-com-twpayne-go-xdg-v6
           go-github-com-zalando-go-keyring
           go-go-etcd-io-bbolt
           go-go-uber-org-multierr
           go-golang-org-x-oauth2
           go-golang-org-x-sys
           go-golang-org-x-term
           go-gopkg-in-yaml-v2
           go-gopkg-in-yaml-v3
           go-howett-net-plist))
    (home-page "https://www.chezmoi.io/")
    (synopsis "Personal configuration files manager")
    (description "This package helps to manage personal configuration files
across multiple machines.")
    (license license:expat)))

(define-public konsave
  (package
    (name "konsave")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Prayag2/konsave")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j5nszy41j4fd6b5w7188gphfk2s0dj44rs7fg55a4izvm0brbx9"))))
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
