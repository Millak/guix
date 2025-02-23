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

(define-module (gnu packages golang-vcs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages version-control))

;;; Commentary:
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

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
