;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2016-2021, 2024 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020-2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2021 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Benjamin Slade <slade@lambda-y.net>
;;; Copyright © 2024 David Pflug <david@pflug.io>
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

(define-module (gnu packages syncthing)
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages time))

(define-public syncthing
  (package
    (name "syncthing")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/syncthing/syncthing"
                                  "/releases/download/v" version
                                  "/syncthing-source-v" version ".tar.gz"))
              (sha256
               (base32
                "1fpwqbslw2jlhcg4dicdv6x6im3qwr4cbjdi58hh8g3mcqfff6zg"))))
    (build-system go-build-system)
    ;; The primary Syncthing executable goes to "out", while the auxiliary
    ;; server programs and utility tools go to "utils".  This reduces the size
    ;; of "out" by ~144 MiB.
    (outputs '("out" "utils"))
    (arguments
     (list
      #:go go-1.23
       #:modules '((srfi srfi-26) ; for cut
                   (guix build utils)
                   (guix build go-build-system))
       #:import-path "github.com/syncthing/syncthing"
       ;; Check 'go.mod' in the source distribution for the required version of Go.
       ;; We don't need to install the source code for end-user applications.
       #:install-source? #f
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'build 'increase-test-timeout
             (lambda _
               (substitute* "src/github.com/syncthing/syncthing/build.go"
                 (("120s") "999s"))))

           (replace 'build
             (lambda _
               (with-directory-excursion "src/github.com/syncthing/syncthing"
                 ; Build the primary Syncthing executable
                 (invoke "go" "run" "build.go" "-no-upgrade")
                 ; Build utilities used to run an independent Syncthing network
                 (for-each (cut invoke "go" "run" "build.go" "build" <>)
                           '("stcrashreceiver" "strelaypoolsrv" "stupgrades"
                             "ursrv")))))

           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (with-directory-excursion "src/github.com/syncthing/syncthing"
                   (invoke "go" "run" "build.go" "test")))))

           (replace 'install
             (lambda _
               (with-directory-excursion "src/github.com/syncthing/syncthing/bin"
                 (install-file "syncthing" (string-append #$output "/bin"))
                 (for-each (cut install-file <> (string-append #$output:utils "/bin/"))
                           '("stdiscosrv" "strelaysrv")))
               (with-directory-excursion "src/github.com/syncthing/syncthing"
                 (for-each (cut install-file <> (string-append #$output:utils "/bin/"))
                           '("ursrv" "stupgrades" "strelaypoolsrv" "stcrashreceiver")))))

           (add-after 'install 'install-docs
             (lambda _
               (let ((man (string-append #$output "/share/man"))
                     (man:utils (string-append #$output:utils "/share/man")))
                 ;; Install all the man pages to "out".
                 (for-each
                  (lambda (file)
                    (install-file file
                                  (string-append man "/man" (string-take-right file 1))))
                  (find-files "src/github.com/syncthing/syncthing/man" "\\.[1-9]"))
                 ;; Copy all the man pages to "utils"
                 (copy-recursively man man:utils)
                 ;; Delete extraneous man pages from "out" and "utils",
                 ;; respectively.
                 (delete-file (string-append man "/man1/stdiscosrv.1"))
                 (delete-file (string-append man "/man1/strelaysrv.1"))
                 (delete-file (string-append man:utils  "/man1/syncthing.1"))))))))
    (synopsis "Decentralized continuous file system synchronization")
    (description "Syncthing is a peer-to-peer file synchronization tool that
supports a wide variety of computing platforms.  It uses the Block Exchange
Protocol.")
    (home-page "https://github.com/syncthing/syncthing")
    (properties
     '((release-monitoring-url . "https://syncthing.net/downloads/")
       (upstream-name . "syncthing-source")
       ;; The hashing code greatly benefits from newer architecture support.
       (tunable? . #t)))
    (license mpl2.0)))

(define-public go-github-com-syncthing-notify
  (package
    (name "go-github-com-syncthing-notify")
    (version "0.0.0-20210616190510-c6b7342338d2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syncthing/notify")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mw7kxj0smcf4vgpxai7m9vncdx2d3blxqy13hffvza0fxnwkv37"))))
    (build-system go-build-system)
    (arguments
     (list
      #:parallel-tests? #f
      #:import-path "github.com/syncthing/notify"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/syncthing/notify")
    (synopsis "File system event notification library")
    (description
     "This package provides @code{notify}, a file system event notification
library in Go.")
    (license expat)))
