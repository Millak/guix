;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020-2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages time))

(define-public syncthing
  (package
    (name "syncthing")
    (version "1.27.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/syncthing/syncthing"
                                  "/releases/download/v" version
                                  "/syncthing-source-v" version ".tar.gz"))
              (sha256
               (base32
                "0g418jyqqik8ds8qcrlnmm2bhwwpbrfgd82fg2jyip4zw1aicqia"))))
    (build-system go-build-system)
    ;; The primary Syncthing executable goes to "out", while the auxiliary
    ;; server programs and utility tools go to "utils".  This reduces the size
    ;; of "out" by ~144 MiB.
    (outputs '("out" "utils"))
    (arguments
     (list #:modules '((srfi srfi-26) ; for cut
                       (guix build utils)
                       (guix build go-build-system))
           #:go go-1.20
           #:import-path "github.com/syncthing/syncthing"
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
                     ;; XXX The only way to build Syncthing without its automatic
                     ;; updater and to build the utilities is to "build all" and then
                     ;; "build syncthing" again with -no-upgrade.
                     ;; https://github.com/syncthing/syncthing/issues/6118
                     (invoke "go" "run" "build.go")
                     (delete-file "bin/syncthing")
                     (invoke "go" "run" "build.go" "-no-upgrade" "build" "syncthing"))))

             (replace 'check
               (lambda* (#:key tests? #:allow-other-keys)
                 (when tests?
                   (with-directory-excursion "src/github.com/syncthing/syncthing"
                     (invoke "go" "run" "build.go" "test")))))

             (replace 'install
               (lambda _
                 (with-directory-excursion "src/github.com/syncthing/syncthing/bin"
                   (install-file "../syncthing" (string-append #$output "/bin"))
                   (for-each (cut install-file <> (string-append #$output:utils "/bin/"))
                             '("stcompdirs" "stcrashreceiver"
                               "stdisco" "stdiscosrv" "stevents" "stfileinfo"
                               "stfinddevice" "stfindignored" "stgenfiles"
                               "strelaypoolsrv" "strelaysrv" "stsigtool"
                               "stvanity" "stwatchfile" "ursrv")))))

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

(define-public syncthing-gtk
  ;; The commit used below corresponds to the latest commit of the
  ;; python3-port branch maintained by Debian.  Upstream hasn't bothered
  ;; porting to Python 3 (see:
  ;; https://github.com/kozec/syncthing-gtk/issues/487).
  (let ((revision "1")
        (commit "c46fbd8ad1d12d409da8942702a2f119cf45514a"))
    (package
      (name "syncthing-gtk")
      (version (git-version "0.9.4.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://salsa.debian.org/debian/syncthing-gtk.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1x1c8snf0jpgjmyyidjw0015ksk5ishqn817wx8vs9i0lfgnnbbg"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'hardcode-dependencies
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((psmisc (assoc-ref inputs "psmisc"))
                     (syncthing (assoc-ref inputs "syncthing")))
                 ;; Hardcode dependencies paths to avoid propagation.
                 (substitute* "syncthing_gtk/tools.py"
                   (("killall") (string-append psmisc "/bin/killall")))
                 (substitute* "syncthing_gtk/configuration.py"
                   (("/usr/bin/syncthing") (string-append syncthing
                                                          "/bin/syncthing"))))))
           (add-after 'unpack 'fix-autostart-path
             ;; Change the autostart .desktop file 'Exec' command so it finds
             ;; the Python wrapper of 'syncthing-gtk', rather than the unwrapped
             ;; '.syncthing-gtk-real'.
             (lambda _
               (substitute* "syncthing_gtk/tools.py"
                 (("return executable")
                   "return \"syncthing-gtk\""))))
           (add-after 'unpack 'remove-windows.py
             (lambda _
               ;; A Windows-specific module that fails to load with
               ;; "ModuleNotFoundError: No module named 'msvcrt'.
               (delete-file "syncthing_gtk/windows.py")))
           (add-after 'wrap 'wrap-libs
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (wrap-program (string-append out "/bin/syncthing-gtk")
                   `("GI_TYPELIB_PATH" ":" prefix
                     (,(getenv "GI_TYPELIB_PATH"))))))))))
      (inputs
       (list gtk+
             libappindicator
             libnotify
             python-bcrypt
             python-dateutil
             python-pycairo
             python-pygobject
             psmisc
             syncthing))
      (home-page "https://github.com/syncthing/syncthing-gtk")
      (synopsis "GTK3 based GUI and notification area icon for Syncthing")
      (description "@code{syncthing-gtk} is a GTK3 Python based GUI and
notification area icon for Syncthing.  Supported Syncthing features:

@itemize
@item Everything that WebUI can display
@item Adding, editing and deleting nodes
@item Adding, editing and deleting repositories
@item Restart, shutdown server
@item Editing daemon settings
@end itemize\n")
      (license gpl2))))

(define-public qsyncthingtray
  (deprecated-package "qsyncthingtray" syncthing-gtk))

(define-public go-github-com-calmh-du
  (package
    (name "go-github-com-calmh-du")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/calmh/du")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qb3a6y3p9nkyn3s66k6zcm16y8n8578qh23ddj14cxf2scrr2n2"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/calmh/du"))
    (synopsis "Get total and available disk space of a given volume")
    (description "This is a Go implementation of `du`.  It provides disk usage
information, such as how much storage space is available, free, and used.")
    (home-page "https://github.com/calmh/du")
    (license public-domain)))

(define-public go-github-com-calmh-xdr
  (package
    (name "go-github-com-calmh-xdr")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/calmh/xdr")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "072wqdncz3nd4a3zkhvzzx1y3in1lm29wfvl0d8wrnqs5pyqh0mh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/calmh/xdr"))
    (synopsis "XDR marshalling and unmarshalling")
    (description "XDR is an External Data Representation (XDR)
marshalling and unmarshalling library in Go.  It uses code generation and not
reflection.")
    (home-page "https://github.com/calmh/xdr")
    (license expat)))

(define-public go-github-com-d4l3k-messagediff
  (package
    (name "go-github-com-d4l3k-messagediff")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/d4l3k/messagediff")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "104hl8x57ciaz7mzafg1vp9qggxcyfm8hsv9bmlihbz9ml3nyr8v"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/d4l3k/messagediff"))
    (synopsis "Diff arbitrary Go structs")
    (description "Messagediff is a library for calculating diffs of arbitrary
structs in the Go programming language.")
    (home-page "https://github.com/d4l3k/messagediff")
    (license expat)))

(define-public go-github-com-jackpal-gateway
  (package
    (name "go-github-com-jackpal-gateway")
    (version "1.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jackpal/gateway")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yms2dw4dnz4cvj9vhwh6193d50jhvn5awsp2g3a4lcc3sjrgd6m"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/jackpal/gateway"))
    (synopsis "Discover the address of a LAN gateway")
    (description "@code{gateway} is a Go library for discovering the IP
address of the default LAN gateway.")
    (home-page "https://github.com/jackpal/gateway")
    (license bsd-3)))

(define-public go-github-com-oschwald-geoip2-golang
  (package
    (name "go-github-com-oschwald-geoip2-golang")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oschwald/geoip2-golang")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jj4rbdpy87rbl79czg5hs5dyn6xlbnk0bnvyzi71dsxan57nixw"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-github-com-oschwald-maxminddb-golang go-golang-org-x-sys))
    (arguments
     `(#:import-path "github.com/oschwald/geoip2-golang"
       #:tests? #f)) ; Requires some unpackaged software and test data
    (synopsis "MaxMind GeoIP2 reader")
    (description "This package provides a library for reading MaxMind
GeoLite2 and GeoIP2 databases in Go.")
    (home-page "https://github.com/oschwald/geoip2-golang")
    (license isc)))

(define-public go-github-com-oschwald-maxminddb-golang
  (package
    (name "go-github-com-oschwald-maxminddb-golang")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oschwald/maxminddb-golang")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "100wd5qv00pkcm6cb8c4x5gavc9jnn7drh6xrqh85hzci4rils66"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-golang-org-x-sys))
    (arguments
     `(#:import-path "github.com/oschwald/maxminddb-golang"
       #:tests? #f)) ; Requires some unpackaged software and test data
    (synopsis "MaxMind DB Reader for Go")
    (description "This is a Go reader for the MaxMind DB format.  Although
this can be used to read GeoLite2 and GeoIP2 databases, @code{geoip2} provides a
higher-level API for doing so.")
    (home-page "https://github.com/oschwald/maxminddb-golang")
    (license isc)))

(define-public go-github-com-thejerf-suture
  (package
    (name "go-github-com-thejerf-suture")
    (version "3.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/thejerf/suture")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03bdrl78jfwk0kw40lj63ga9cxhgccgss8yi9lp5j0m0ml7921gh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/thejerf/suture"))
    (synopsis "Supervisor trees for Go")
    (description "Suture provides Erlang-ish supervisor trees for Go.
\"Supervisor trees\" -> \"sutree\" -> \"suture\" -> holds your code together
when it's trying to die.

It is intended to deal gracefully with the real failure cases that can occur
with supervision trees (such as burning all your CPU time endlessly restarting
dead services), while also making no unnecessary demands on the \"service\"
code, and providing hooks to perform adequate logging with in a production
environment")
    (home-page "https://github.com/thejerf/suture")
    (license expat)))

(define-public go-github-com-vitrun-qart
  (let ((commit "bf64b92db6b05651d6c25a3dabf2d543b360c0aa")
        (revision "0"))
    (package
      (name "go-github-com-vitrun-qart")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/vitrun/qart")
                      (commit commit)))
                (file-name (string-append "go-github-com-vitrun-qart-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1xk7qki703xmay9ghi3kq2bjf1iw9dz8wik55739d6i7sn77vvkc"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/vitrun/qart"))
      (synopsis "Create QR codes with an embedded image")
      (description "This package provides a library for embedding
human-meaningful graphics in QR codes.  However, instead of scribbling on
redundant pieces and relying on error correction to preserve the meaning,
@code{qart} engineers the encoded values to create the picture in a code with no
inherent errors.")
      (home-page "https://github.com/vitrun/qart")
      (license bsd-3))))

(define-public go-github-com-chmduquesne-rollinghash
  (let ((commit "a60f8e7142b536ea61bb5d84014171189eeaaa81")
        (revision "0"))
    (package
      (name "go-github-com-chmduquesne-rollinghash")
      (version (git-version "4.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/chmduquesne/rollinghash")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0fpaqq4zb0wikgbhn7vwqqj1h865f5xy195vkhivsp922p7qwsjr"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/chmduquesne/rollinghash/"))
      (synopsis "Rolling hashes in Go")
      (description "This package provides a Go implementation of several rolling
hashes.")
      (home-page "https://github.com/chmduquesne/rollinghash")
      (license expat))))

(define-public go-github-com-syncthing-notify
  (let ((commit "69c7a957d3e261f9744f46b3dd4d608d8480ad90")
        (revision "5"))
    (package
      (name "go-github-com-syncthing-notify")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/syncthing/notify")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1mmdzyfnmjabyhbipl4bggw4w5nlxyyjp0d93qd824kj07kmsr1f"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/syncthing/notify"))
      (propagated-inputs
       (list go-golang-org-x-sys))
      (synopsis "File system event notification library")
      (description "This package provides @code{notify}, a file system event
notification library in Go.")
      (home-page "https://github.com/syncthing/notify")
      (license expat))))

(define-public go-github-com-matttproud-golang-protobuf-extensions-pbutil
  (package
    (name "go-github-com-matttproud-golang-protobuf-extensions-pbutil")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/matttproud/golang_protobuf_extensions")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jw4vjycwx0a82yvixmp25805krdyqd960y8lnyggllb6br0vh41"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/matttproud/golang_protobuf_extensions/v2/pbutil"
      #:unpack-path "github.com/matttproud/golang_protobuf_extensions/v2"))
    (propagated-inputs
     (list go-github-com-golang-protobuf-proto
           go-google-golang-org-protobuf))
    (synopsis "Streaming Protocol Buffers in Go")
    (description "This package provides various Protocol Buffer
extensions for the Go language, namely support for record length-delimited
message streaming.")
    (home-page "https://github.com/matttproud/golang_protobuf_extensions")
    (license asl2.0)))

(define-public go-github-com-flynn-archive-go-shlex
  (let ((commit "3f9db97f856818214da2e1057f8ad84803971cff")
        (revision "0"))
    (package
      (name "go-github-com-flynn-archive-go-shlex")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/flynn-archive/go-shlex")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1j743lysygkpa2s2gii2xr32j7bxgc15zv4113b0q9jhn676ysia"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/flynn-archive/go-shlex"))
      (synopsis "Go lexer")
      (description "Shlex is a simple lexer for go that supports shell-style
quoting, commenting, and escaping.")
      (home-page "https://github.com/flynn-archive/go-shlex")
      (license asl2.0))))
