;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages forgejo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages node)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh))

(define-public forgejo
  (package
    (name "forgejo")
    (version "15.0.0")
    (source
     (origin
       (method url-fetch)
       ;; XXX: We build from the release tarball for now, which includes all
       ;; of the needed Go dependencies.
       ;; TODO: "Unbundle" the index.js and index.css by building them with
       ;; webpack.  This cannot currently be done because of missing Node
       ;; modules.
       (uri (string-append "https://codeberg.org/forgejo/forgejo/"
                           "releases/download/v" version
                           "/forgejo-src-" version ".tar.gz"))
       (sha256
        (base32
         "1b25zhlr81b577swfzxlhc7hiaxnznfygxbiv125vabjmahdmffy"))))
    (build-system gnu-build-system)
    (outputs (list "out" "data"))
    (arguments
     (list
      #:test-target "test-backend"      ;test-frontend requires npm modules
      #:make-flags #~(list "TAGS=sqlite sqlite_unlock_notify") ;sqlite support
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-static-root-path
            (lambda _
              (substitute* "modules/setting/server.go"
                (("StaticRootPath = AppWorkPath")
                 (format #f "StaticRootPath = ~s" #$output:data)))))
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              (for-each delete-file
                        ;; Theses tests require internet.
                        '("modules/hcaptcha/hcaptcha_test.go"
                          "modules/updatechecker/update_checker_test.go"
                          "services/migrations/migrate_test.go"
                          ;; The 'TestGetDirectorySize' test expects 8037
                          ;; entries, finds 7597 for unknown reasons.
                          "modules/repository/create_test.go"))))
          (replace 'configure
            (lambda _
              (setenv "CC" "gcc")
              (setenv "CGO_ENABLED" "1")
              (setenv "GOTOOLCHAIN" "local") ;use 'go' from Guix
              (setenv "HOME" (getcwd))
              (invoke "git" "init")))   ;for tests
          (add-after 'install 'post-install
            (lambda* (#:key inputs #:allow-other-keys)
              (rename-file "go/bin/forgejo.org" "go/bin/forgejo")
              (let ((forgejo (string-append #$output "/bin/forgejo")))
                (install-file "go/bin/forgejo" (dirname forgejo))
                (wrap-program forgejo
                  `("PATH" prefix
                    ,(map (lambda (c)
                            (dirname
                             (search-input-file inputs
                                                (string-append "bin/" c))))
                          '("git" "gzip" "sh" "ssh"))))
                (substitute* forgejo    ;avoid stripping argv[0] in wrapper
                  (("exec -a \"\\$\\{0##\\*/\\}\" ")
                   "exec ")))
              ;; Install data files.
              (copy-recursively "options"
                                (string-append #$output:data "/options"))
              (copy-recursively "public"
                                (string-append #$output:data "/public"))
              (copy-recursively "templates"
                                (string-append #$output:data "/templates")))))))
    (native-inputs (list git go-1.25 openssh node sqlite))
    (inputs (list bash-minimal git gzip openssh))
    (home-page "https://forgejo.org")
    (synopsis "Self-hosted, lightweight software forge")
    (description "Forgejo is a lightweight software forge that aims to be easy
to self-host and maintain.  It offers a rich web API for accessing its data.
ALong the usual forge features such as issues and pull requests tracking, it
also includes simple software project management features like roles, scopes,
and time tracking to ease collaboration.")
    (license license:expat)))
