;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages textutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public chezmoi
  (package
    (name "chezmoi")
    (version "1.8.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/chezmoi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ildvlq7v8vnw74y4fgnv3hpq49bpl6zh1wmakfh46crwg7ffmjb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/twpayne/chezmoi"
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Remove test script which expect additional user's programs available
          ;; in the PATH. The testdata directory is removed in the latest version
          ;; (2.46.1) of the program.
          (add-after 'unpack 'remove-failing-test-scripts
            (lambda* (#:key import-path #:allow-other-keys)
              (for-each (lambda (f)
                          (delete-file (string-append "src/" import-path "/testdata/scripts/" f)))
                        '("bitwarden.txt"
                          "cd.txt"
                          "cd_unix.txt"
                          "completion.txt"
                          "diff.txt"
                          "edit.txt"
                          "editconfig.txt"
                          "git.txt"
                          "gopass.txt"
                          "keepassxc.txt"
                          "lastpass.txt"
                          "onepassword.txt"
                          "pass.txt"
                          "runscriptdir_unix.txt"
                          "script_unix.txt"
                          "secretgeneric.txt"
                          "secretgopass.txt"
                          "secretkeepassxc.txt"
                          "secretlastpass.txt"
                          "secretonepassword.txt"
                          "secretpass.txt"))))
          ;; FIXME: Pattern embedded: cannot embed directory embedded:
          ;; contains no embeddable files.
          ;;
          ;; This happens due to Golang can't determine the valid directory of
          ;; the module which is sourced during setup environment phase, but
          ;; easy resolved after coping to expected directory "vendor" within
          ;; the current package, see details in Golang source:
          ;;
          ;; - URL: <https://github.com/golang/go/blob/>
          ;; - commit: 82c14346d89ec0eeca114f9ca0e88516b2cda454
          ;; - file: src/cmd/go/internal/load/pkg.go#L2059
          (add-before 'build 'copy-input-to-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (mkdir "vendor")
                (copy-recursively
                 (string-append
                  #$(this-package-native-input "go-github-com-charmbracelet-glamour")
                  "/src/github.com")
                 "vendor/github.com")
                (copy-recursively
                 (string-append
                  #$(this-package-native-input "go-github-com-alecthomas-chroma-v2")
                  "/src/github.com")
                 "vendor/github.com"))))
          (add-before 'install 'remove-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "vendor")))))))
    (native-inputs
     (list go-github-com-alecthomas-chroma-v2
           go-github-com-bmatcuk-doublestar-v2
           go-github-com-charmbracelet-glamour
           go-github-com-coreos-go-semver
           go-github-com-google-go-github-v33
           go-github-com-google-renameio
           go-github-com-masterminds-sprig-v3
           go-github-com-pelletier-go-toml
           go-github-com-pkg-diff
           go-github-com-rogpeppe-go-internal
           go-github-com-sergi-go-diff
           go-github-com-spf13-cobra
           go-github-com-spf13-viper
           go-github-com-stretchr-testify
           go-github-com-twpayne-go-shell
           go-github-com-twpayne-go-vfs
           go-github-com-twpayne-go-vfsafero
           go-github-com-twpayne-go-xdg-v3
           go-github-com-zalando-go-keyring
           go-github-go-git
           go-go-etcd-io-bbolt
           go-golang-org-x-oauth2
           go-golang-org-x-sys
           go-golang-org-x-term
           go-gopkg-in-yaml-v2
           go-howett-net-plist))
    (home-page "https://www.chezmoi.io/")
    (synopsis "Personal configuration files manager")
    (description "This package helps to manage personal configuration files
across multiple machines.")
    (license license:expat)))
