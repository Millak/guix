;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016-2018, 2020-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018, 2019, 2021, 2023 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 André Batista <nandre@riseup.net>
;;; Copyright © 2021-2023 Danial Behzadi <dani.behzi@ubuntu.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Jim Newsome <jnewsome@torproject.org>
;;; Copyright © 2025 Danial Behzadi <dani.behzi@ubuntu.com>
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

(define-module (gnu packages tor)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages xorg))

(define-public tor
  (package
    (name "tor")
    (version "0.4.8.16")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://dist.torproject.org/tor-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1jwhsg7w31y9chfzww7nnjjhyappgwxslc3msbkvh3qjg8vxsh35"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-gpl"
                   "--enable-lzma"
                   "--enable-zstd")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'adjust-torify
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Record in 'torify' the absolute file name of 'torsocks'.
                   (let ((torsocks (search-input-file
                                    inputs "/bin/torsocks")))
                     (substitute* "contrib/client-tools/torify"
                       (("pathfind torsocks")
                        "true")
                       (("exec torsocks")
                        (string-append "exec " torsocks))))))
               (add-before 'check 'skip-practracker
                 ;; This is a style linter.  It doesn't get to throw fatal errors.
                 (lambda _
                   (setenv "TOR_DISABLE_PRACTRACKER" "set")))
               #$@(if (or (target-x86-64?)
                          (target-x86-32?))
                     '()
                     ;; Work around upstream issues relating to libseccomp,
                     ;; sandboxing and glibc-2.33.  This is similar to the issue
                     ;; the tor-sandbox-i686 patch fixes but for other architectures.
                     ;; https://gitlab.torproject.org/tpo/core/tor/-/issues/40381
                     ;; https://gitlab.torproject.org/tpo/core/tor/-/issues/40599
                     ;; https://gitlab.torproject.org/tpo/core/tor/-/merge_requests/446
                     `((add-before 'check 'adjust-test-suite
                         (lambda _
                           (substitute* "src/test/test_include.sh"
                             ((".*Sandbox 1.*") ""))
                           (substitute* "src/test/test.c"
                             ((".*sandbox_tests.*") "")))))))))
    (native-inputs
     (list pkg-config python))             ; for tests
    (inputs
     (list libevent
           libseccomp
           openssl
           torsocks
           xz
           zlib
           `(,zstd "lib")))
    (home-page "https://www.torproject.org/")
    (synopsis "Anonymous network router to improve privacy on the Internet")
    (description
     "Tor protects you by bouncing your communications around a distributed
network of relays run by volunteers all around the world: it prevents
somebody watching your Internet connection from learning what sites you
visit, and it prevents the sites you visit from learning your physical
location.  Tor works with many of your existing applications, including
web browsers, instant messaging clients, remote login, and other
applications based on the TCP protocol.

This package is the full featured @code{tor} which is needed for running
relays, bridges or directory authorities.  If you just want to access the Tor
network or to setup an onion service you may install @code{tor-client}
instead.")
    (license license:bsd-3)))

(define-public tor-client
  (package
    (inherit tor)
    (name "tor-client")
    (arguments
     (substitute-keyword-arguments (package-arguments tor)
       ((#:configure-flags flags #~'())
        #~(append #$flags
                  (list "--disable-module-relay")))))
    (synopsis "Client to the anonymous Tor network")
    (description
     "Tor protects you by bouncing your communications around a distributed
network of relays run by volunteers all around the world: it prevents
somebody watching your Internet connection from learning what sites you
visit, and it prevents the sites you visit from learning your physical
location.  Tor works with many of your existing applications, including
web browsers, instant messaging clients, remote login, and other
applications based on the TCP protocol.

To @code{torify} applications (to take measures to ensure that an application,
which has not been designed for use with Tor such as ssh, will use only Tor for
internet connectivity, and also ensures that there are no leaks from DNS, UDP or
the application layer) you need to install @code{torsocks}.

This package only provides a client to the Tor Network.")))

(define-public torsocks
  (package
    (name "torsocks")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.torproject.org/tpo/core/torsocks/-/archive/v"
             version "/torsocks-v" version ".tar.bz2"))
       (sha256
        (base32
         "1a7k3njdhp7dz603knhisna1zvxw35j3g213p6dvczv9bcjy7cjl"))))
    (build-system gnu-build-system)
    (inputs
     (list libcap))
    (native-inputs
     (list autoconf automake libtool))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'absolutize
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/bin/torsocks"
               (("getcap=.*")
                (string-append "getcap=" (which "getcap") "\n"))))))))
    (home-page "https://www.torproject.org/")
    (synopsis "Transparently route an application's traffic through Tor")
    (description
     "Torsocks allows you to use most applications in a safe way with Tor.  It
ensures that DNS requests are handled safely and explicitly rejects UDP
traffic from the application you're using.")
    (properties
     '((release-monitoring-url
         . "https://gitlab.torproject.org/tpo/core/torsocks/-/tags")))
    ;; All the files explicitly say "version 2 only".
    (license license:gpl2)))

(define-public privoxy
  (package
    (name "privoxy")
    (version "3.0.34")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/ijbswa/Sources/"
                                 version "%20%28stable%29/privoxy-"
                                 version "-stable-src.tar.gz"))
             (sha256
              (base32
               "0b2x7hm34fbqaxb46afpx6bnc3x76d753y2p8rmn2kkgcnhvrk76"))))
    (build-system gnu-build-system)
    (arguments
     '(;; The default 'sysconfdir' is $out/etc; change that to
       ;; $out/etc/privoxy.
       #:configure-flags (list (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc/privoxy")
                               "--localstatedir=/var"
                               "--with-brotli"
                               "--with-openssl")
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-default-logging
           (lambda _
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               ;; Do not create /var/run nor /var/log/privoxy/logfile.
               (substitute* "GNUmakefile.in"
                 (("(logfile \\|\\| exit )1" _ match)
                  (string-append match "0"))
                 (("(\\$\\(DESTDIR\\)\\$\\(SHARE_DEST\\)) \\\\" _ match)
                  match)
                 ((".*\\$\\(LOG_DEST\\) \\$\\(DESTDIR\\)\\$\\(PID_DEST\\).*")
                  ""))
               ;; Disable logging in the default configuration to allow for
               ;; non-root users using it as is.
               (substitute* "config"
                 (("^logdir") "#logdir")
                 (("^logfile") "#logfile"))))))))
    (inputs
     (list brotli openssl pcre w3m zlib))
    (native-inputs
     (list autoconf automake))
    (home-page "https://www.privoxy.org")
    (synopsis "Web proxy with advanced filtering capabilities for enhancing privacy")
    (description
     "Privoxy is a non-caching web proxy with advanced filtering capabilities
for enhancing privacy, modifying web page data and HTTP headers, controlling
access, and removing ads and other obnoxious Internet junk.  Privoxy has a
flexible configuration and can be customized to suit individual needs and
tastes.  It has application for both stand-alone systems and multi-user
networks.")
    (license license:gpl2+)))

(define-public onionshare-cli
  (package
    (name "onionshare-cli")
    (version "2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onionshare/onionshare")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bhrp019a0923h7dnfxhgvgvdp81blvnsbnvzy34hp827abxf3ic"))
       (patches (search-patches "onionshare-cli-async-mode.patch"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (inputs
     ;; TODO: obfs4proxy
     (list python-click
           python-colorama
           python-eventlet
           python-flask
           python-flask-httpauth
           python-flask-socketio
           python-pynacl
           python-psutil
           python-pycryptodome
           python-pysocks
           python-requests
           python-stem
           python-unidecode
           python-urllib3
           tor))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'bake-tor
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (list "cli/onionshare_cli/common.py"
                                 "desktop/onionshare/gui_common.py")
                (("shutil\\.which\\(\\\"tor\\\"\\)")
                 (format #f "~s" (search-input-file inputs "bin/tor"))))
              (substitute* "cli/tests/test_cli_common.py"
                (("/usr/share/tor")
                 (search-input-directory inputs "share/tor")))))
          (add-before 'build 'change-directory
            (lambda _ (chdir "cli")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                ;; Greendns is not needed for testing, and if eventlet tries to
                ;; load it, an OSError is thrown when getprotobyname is called.
                ;; Thankfully there is an environment variable to disable the
                ;; greendns import, so use it:
                (setenv "EVENTLET_NO_GREENDNS" "yes")
                (invoke "pytest" "-v" "./tests")))))))
    (home-page "https://onionshare.org/")
    (synopsis "Securely and anonymously share files")
    (description "OnionShare lets you securely and anonymously share files,
host websites, and chat with friends using the Tor network.

This package contains @code{onionshare-cli}, a command-line interface to
OnionShare.")
    ;; Bundled, minified jquery and socket.io are expat licensed.
    (license (list license:gpl3+ license:expat))))

(define-public onionshare
  (package
    (inherit onionshare-cli)
    (name "onionshare")
    (arguments
     (substitute-keyword-arguments (package-arguments onionshare-cli)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'absolutize
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "desktop/onionshare/tab/mode/history.py"
                  (("Popen\\(\\[\"xdg-open\"")
                   (string-append "Popen([\"" (which "xdg-open") "\"")))))
            (replace 'change-directory
              (lambda _ (chdir "desktop/")))
            (add-after 'install 'install-data
              (lambda _
                (install-file "org.onionshare.OnionShare.svg"
                              (string-append #$output
                                             "/share/icons/hicolor/scalable/apps"))
                (install-file "org.onionshare.OnionShare.desktop"
                              (string-append #$output
                                             "/share/applications"))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  ;; Remove multiline load-path adjustment, so that
                  ;; onionshare-cli modules are loaded from input
                  (substitute* "tests/conftest.py"
                    (("\"cli\",")
                     "\"/nonexistent\""))
                  ;; Avoid `getprotobyname` issues:
                  (setenv "EVENTLET_NO_GREENDNS" "yes")
                  ;; Make Qt render "offscreen":
                  (setenv "QT_QPA_PLATFORM" "offscreen")
                  (setenv "HOME" "/tmp")
                  (apply invoke "xvfb-run" "pytest" "-vv"
                         (find-files "tests" "^test_gui.*\\.py$")))))))))
    (native-inputs
     (list python-pytest xvfb-run))
    (inputs
     ;; The desktop client uses onionshare-cli like a python module.  But
     ;; propagating onionshare-cli's inputs is not great, since a user would
     ;; not expect to have those installed when using onionshare-cli as a
     ;; standalone utility.  So add onionshare-cli's inputs here.
     (modify-inputs (package-inputs onionshare-cli)
       (prepend onionshare-cli          ;TODO: package obfs4proxy
                python-shiboken-2
                python-pyside-2
                python-qrcode
                xdg-utils)))
    (description "OnionShare lets you securely and anonymously share files,
host websites, and chat with friends using the Tor network.")))

(define-public nyx
  (package
    (name "nyx")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "02rrlllz2ci6i6cs3iddyfns7ang9a54jrlygd2jw1f9s6418ll8"))))
    (build-system python-build-system)
    (inputs
     (list python-stem))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-man-page
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (install-file "nyx.1" (string-append man "/man1"))
               #t)))
         (add-after 'install 'install-sample-configuration
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "web/nyxrc.sample" doc)
               #t))))
       ;; XXX The tests seem to require more of a real terminal than the build
       ;; environment provides:
       ;;   _curses.error: setupterm: could not find terminal
       ;; With TERM=linux, the tests try to move the cursor and still fail:
       ;;   _curses.error: cbreak() returned ERR
       #:tests? #f))
    (home-page "https://nyx.torproject.org/")
    (synopsis "Tor relay status monitor")
    (description
     "Nyx monitors the performance of relays participating in the
@uref{https://www.torproject.org/, Tor anonymity network}.  It displays this
information visually and in real time, using a curses-based terminal interface.
This makes Nyx well-suited for remote shell connections and servers without a
graphical display.  It's like @command{top} for Tor, providing detailed
statistics and status reports on:

@enumerate
@item connections (with IP address, hostname, fingerprint, and consensus data),
@item bandwidth, processor, and memory usage,
@item the relay's current configuration,
@item logged events,
@item and much more.
@end enumerate

Potential client and exit connections are scrubbed of sensitive information.")
    (license license:gpl3+)))

(define-public tractor
  (package
    (name "tractor")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "traxtor" version))
       (sha256
        (base32
         "0is50z3v0mw9am31zizblq5ai30vz83qp6qsdingbn7f7cqzldmh"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel
           (list glib "bin")))       ; for glib-compile-schemas.
    (inputs
     (list python-fire
           python-pygobject
           python-pysocks
           python-requests
           python-stem
           python-termcolor))
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    (list "not test_local"
                          "test_network"
                          "test_obfs"
                          "test_stop_do")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-man-page
            (lambda _
              (let ((man1 (string-append #$output "/share/man/man1")))
                (install-file "data/tractor.1" man1))))
          (add-after 'install 'install-bash-completion
            (lambda _
              (let ((bash-completion
                     (string-append #$output "/share/bash-completion/completions")))
                (install-file "data/completion/bash/tractor" bash-completion))))
          (add-after 'install 'install-gschema
            (lambda _
              (let ((schemas (string-append #$output "/share/glib-2.0/schemas")))
                (install-file "src/tractor/tractor.gschema.xml" schemas)
                ;; The following line is needed for packages having tractor as input.
                (invoke "glib-compile-schemas" schemas)))))))
    (home-page "https://framagit.org/tractor")
    (synopsis "Setup an onion routing proxy")
    (description
     "This package uses Python stem library to provide a connection through
the onion proxy and sets up proxy in user session, so you don't have to mess
up with TOR on your system anymore.")
    (license license:gpl3+)))
