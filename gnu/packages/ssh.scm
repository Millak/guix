;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2023, 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2018-2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2021, 2023 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018 Manuel Graf <graf@init.at>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021, 2022, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2023 Simon Streit <simon@netpanic.org>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages ssh)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public hss
  (package
    (name "hss")
    (version "1.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/six-ddc/hss")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12578xhvkg70ma411yh8nbpcpnys420bnm9g0dzypb0vn3jxpz8q"))))
    (inputs
     (list readline))
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "INSTALL_BIN=" #$output "/bin"))
           #:tests? #f                  ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-file-names
                 (lambda _
                   (substitute* "Makefile"
                     (("/usr/local/opt/readline")
                      #$(this-package-input "readline")))))
               (delete 'configure))))         ; no configure script
    (build-system gnu-build-system)
    (home-page "https://github.com/six-ddc/hss/")
    (synopsis "Interactive SSH client for multiple servers")
    (description
     "@command{hss} is an interactive SSH client for multiple servers.  Commands
are executed on all servers in parallel.  Execution on one server does not need
to wait for that on another server to finish before starting.  One can run a
command on hundreds of servers at the same time, with almost the same experience
as a local Bash shell.

It supports:
@itemize @bullet
@item interactive input: based on GNU readline.
@item history: responding to the @kbd{C-r} key.
@item auto-completion: @key{TAB}-completion from remote servers for commands and
file names.
@end itemize\n")
    (license license:expat)))

(define-public libssh
  (package
    (name "libssh")
    (version "0.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.libssh.org/files/"
                                  (version-major+minor version)
                                  "/libssh-" version ".tar.xz"))
              (sha256
               (base32
                "0y8v5ihrqnjxchvjhz8fcczndchaaxxim64bqm8q3q4i5v3xrdql"))
              (modules '((guix build utils)))
              (snippet
               ;; 'PATH_MAX' is undefined on GNU/Hurd; work around it.
               #~(substitute* (find-files "examples" "\\.c$")
                   (("#include \"examples_common\\.h\"" all)
                    (string-append all "\n"
                                   "#ifndef PATH_MAX\n"
                                   "# define PATH_MAX 4096\n"
                                   "#endif\n"))))
              (patches (search-patches "libssh-openssh-banner.patch"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     (list
      #:configure-flags
      #~(list #$@(if (%current-target-system)
                     #~()
                     #~("-DUNIT_TESTING=ON"))
              #$@(if (and (%current-target-system)
                          (not (target-64bit?)))
                     #~((string-append
                         "-DCMAKE_C_FLAGS=-g -O2"
                         " -Wno-error=incompatible-pointer-types"))
                     #~()))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-commands
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Runtime sources.
              (substitute* '("src/config.c"
                             "src/socket.c")
                (("\"/bin/sh\"")
                 (format #f "~s" (search-input-file inputs "/bin/sh"))))
              ;; Test sources.
              (substitute* '("tests/server/test_server/default_cb.c")
                (("\"/bin/sh\"")
                 (format #f "~s" (which "sh"))))))
          (add-before 'check 'prepare-for-tests
            ;; A few test rely on the assumption that HOME == user's pw_dir,
            ;; which is not satisfied in Guix, where `pw_dir' is '/' while
            ;; HOME is '/homeless-shelter'.
            (lambda _
              (setenv "HOME" "/"))))))
    (native-inputs (list cmocka))
    (inputs (list bash-minimal mit-krb5 openssl zlib))
    (synopsis "SSH client library")
    (description
     "libssh is a C library implementing the SSHv2 and SSHv1 protocol for client
and server implementations.  With libssh, you can remotely execute programs,
transfer files, and use a secure and transparent tunnel for your remote
applications.")
    (home-page "https://www.libssh.org")
    (license license:lgpl2.1+)))

(define-public libssh2
  (package
   (name "libssh2")
   (version "1.10.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "https://www.libssh2.org/download/libssh2-"
                   version ".tar.gz"))
            (sha256
             (base32
              "0l8xwhhscvss7q007vpbkbv7jh9s43579rx2sf8lnfgd7l7yjr1d"))))
   (build-system gnu-build-system)
   ;; The installed libssh2.pc file does not include paths to libgcrypt and
   ;; zlib libraries, so we need to propagate the inputs.
   (propagated-inputs (list libgcrypt zlib))
   (arguments
    (list #:configure-flags #~'("--with-libgcrypt"
                                "--disable-static")))
   (synopsis "Client-side C library implementing the SSH2 protocol")
   (description
    "libssh2 is a library intended to allow software developers access to
the SSH-2 protocol in an easy-to-use self-contained package.  It can be built
into an application to perform many different tasks when communicating with
a server that supports the SSH-2 protocol.")
   (license license:bsd-3)
   (home-page "https://www.libssh2.org/")))

(define-public openssh
  (package
   (name "openssh")
   (version "10.0p1")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://openbsd/OpenSSH/portable/"
                          "openssh-" version ".tar.gz"))
      (patches (search-patches "openssh-trust-guix-store-directory.patch"))
      (sha256
       (base32 "0p6qp47gkkjrxlqaignsvn81lh80wnlxasr5n5845pqfk9q2w6h2"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:test-target "tests"
     ;; Not all of the tests can be run in parallel, see
     ;; <https://marc.info/?l=openssh-unix-dev&m=170313565518842>.
     #:parallel-tests? #f
     ;; Otherwise, the test scripts try to use a nonexistent directory and fail.
     #:make-flags
     #~(list "REGRESSTMP=\"$${BUILDDIR}/regress\"")
     #:configure-flags
     #~(append
        (list "--sysconfdir=/etc/ssh"
              ;; Default value of 'PATH' used by sshd.
              "--with-default-path=/run/current-system/profile/bin"
              ;; configure needs to find krb5-config.
              (string-append "--with-kerberos5="
                             #$(this-package-input "mit-krb5")
                             "/bin")
              ;; libedit is needed for sftp completion.
              "--with-libedit")
        ;; Enable PAM support in sshd.
        (if #$(target-hurd?)
            '()
            (list "--with-pam"
                  ;; Support creation and use of ecdsa-sk, ed25519-sk keys.
                  "--with-security-key-builtin"))
        ;; "make install" runs "install -s" by default, which doesn't work for
        ;; cross-compiled binaries because it invokes 'strip' instead of
        ;; 'TRIPLET-strip'.  Work around this.
        (if #$(%current-target-system)
            (list "--disable-strip")
            '()))
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'configure 'set-store-location
           (lambda _
             (substitute* "misc.c"
               (("@STORE_DIRECTORY@")
                (string-append "\"" (%store-directory) "/\"")))))
         (add-before 'check 'patch-tests
           (lambda _
             (substitute* "regress/test-exec.sh"
               (("/bin/sh") (which "sh")))

             ;; Remove 't-exec' regress target which requires user 'sshd'.
             (substitute* (list "Makefile"
                                "regress/Makefile")
               (("^(tests:.*) t-exec(.*)" all pre post)
                (string-append pre post)))))
         (replace 'install
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             ;; Don't create /var/empty.
             (substitute* "Makefile"
               ((".*MKDIR_P.*PRIVSEP_PATH.*") ""))
             ;; Install without host keys and system configuration files.
             (apply invoke "make" "install-nosysconf" make-flags)
             (with-directory-excursion "contrib"
               (chmod "ssh-copy-id" #o555)
               (install-file "ssh-copy-id"
                             (string-append #$output "/bin/"))
               (install-file "ssh-copy-id.1"
                             (string-append #$output "/share/man/man1/"))))))))
   (native-inputs (list groff pkg-config))
   (inputs
    (cons* libedit
           openssl
           mit-krb5
           zlib
           xauth                        ; for 'ssh -X' and 'ssh -Y'
           (if (target-hurd?)
               '()
               (list linux-pam
                     libfido2))))       ; fails to build on GNU/Hurd
   (synopsis "Client and server for the secure shell (ssh) protocol")
   (description
    "The SSH2 protocol implemented in OpenSSH is standardised by the
IETF secsh working group and is specified in several RFCs and drafts.
It is composed of three layered components:

The transport layer provides algorithm negotiation and a key exchange.
The key exchange includes server authentication and results in a
cryptographically secured connection: it provides integrity, confidentiality
and optional compression.

The user authentication layer uses the established connection and relies on
the services provided by the transport layer.  It provides several mechanisms
for user authentication.  These include traditional password authentication
as well as public-key or host-based authentication mechanisms.

The connection layer multiplexes many different concurrent channels over the
authenticated connection and allows tunneling of login sessions and
TCP-forwarding.  It provides a flow control service for these channels.
Additionally, various channel-specific options can be negotiated.")
   (license (license:non-copyleft "file://LICENSE"
                               "See LICENSE in the distribution."))
   (properties
    '((release-monitoring-url . "https://ftp.openbsd.org/pub/OpenBSD/OpenSSH/portable/")))
   (home-page "https://www.openssh.com/")))

;; OpenSSH without X support. This allows using OpenSSH without dragging X
;; libraries to the closure.
(define-public openssh-sans-x
  (package
    (inherit openssh)
    (name "openssh-sans-x")
    (inputs (modify-inputs (package-inputs openssh)
              (delete "xauth")))
    (synopsis "OpenSSH client and server without X11 support")))

(define-public guile-ssh
  (package
    (name "guile-ssh")
    (version "0.18.0")
    (home-page "https://github.com/artyom-poptsov/guile-ssh")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zh1spkjl5q778y4rd6ml68fvz1r62xmk03khi4kp74z2rxgzcxb"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(;; It makes no sense to build libguile-ssh.a.
       #:configure-flags '("--disable-static")

       #:phases (modify-phases %standard-phases
                  (add-before 'bootstrap 'support-cross-compilation
                    (lambda _
                      ;; Support cross-compilation:
                      ;; <https://github.com/artyom-poptsov/guile-ssh/issues/30>.
                      (substitute* "libguile-ssh/Makefile.am"
                        (("\\$\\(guile_snarf\\)")
                         "CPP=\"$(CPP)\" $(guile_snarf)"))))
                  (add-before 'build 'fix-libguile-ssh-file-name
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Build and install libguile-ssh.so so that we can use
                      ;; its absolute file name in .scm files, before we build
                      ;; the .go files.
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib")))
                        (invoke "make" "install"
                                "-C" "libguile-ssh"
                                "-j" (number->string
                                      (parallel-job-count)))
                        (substitute* (find-files "." "\\.scm$")
                          (("\"libguile-ssh\"")
                           (string-append "\"" lib "/libguile-ssh\""))))))
                  ,@(if (%current-target-system)
                        '()
                        '((add-before 'check 'fix-guile-path
                             (lambda* (#:key inputs #:allow-other-keys)
                               (let ((guile (assoc-ref inputs "guile")))
                                 (substitute* "tests/common.scm"
                                   (("/usr/bin/guile")
                                    (string-append guile "/bin/guile"))))))))
                  (add-after 'install 'remove-bin-directory
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (examples (string-append
                                        out "/share/guile-ssh/examples")))
                        (mkdir-p examples)
                        (rename-file (string-append bin "/ssshd.scm")
                                     (string-append examples "/ssshd.scm"))
                        (rename-file (string-append bin "/sssh.scm")
                                     (string-append examples "/sssh.scm"))
                        (delete-file-recursively bin)))))))
    (native-inputs (list autoconf
                         automake
                         libtool
                         texinfo
                         pkg-config
                         which
                         guile-3.0)) ;needed when cross-compiling.
    (inputs (list guile-3.0 libssh libgcrypt))
    (synopsis "Guile bindings to libssh")
    (description
     "Guile-SSH is a library that provides access to the SSH protocol for
programs written in GNU Guile interpreter.  It is a wrapper to the underlying
libssh library.")
    (license license:gpl3+)))

(define-public guile2.2-ssh
  (package
    (inherit guile-ssh)
    (name "guile2.2-ssh")
    (native-inputs
     (modify-inputs (package-native-inputs guile-ssh)
       (delete "guile")
       (prepend guile-2.2 ;needed when cross-compiling.
                )))
    (inputs (modify-inputs (package-inputs guile-ssh)
              (replace "guile" guile-2.2)))))

(define-public corkscrew
  ;; The last 2.0 release hails from 2009.  Use a fork (submitted upstream as
  ;; <https://github.com/patpadgett/corkscrew/pull/5>) that adds now-essential
  ;; IPv6 and TLS support.
  (let ((revision "0")
        (commit "268b71e88ee51fddceab96d665b327394f1feb12"))
    (package
      (name "corkscrew")
      (version (git-version "2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rtgill82/corkscrew")
               (commit commit)))
         (sha256
          (base32 "1rylbimlfig3ii4bqr4r058lkc43pqkxnxqpqdpm31blh3xs0dcw"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list "--enable-ssl")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'update-metadata
             (lambda _
               (substitute* "configure.ac"
                 ;; Our version differs significantly.
                 (("2.0") (string-append ,version " (Guix)")))
               (substitute* "corkscrew.c"
                 ;; This domain's since been squat.
                 (("\\(agroman@agroman\\.net\\)")
                  (format #f "<~a>" ,(package-home-page this-package))))))
           (add-after 'install 'install-documentation
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (doc (string-append out "/share/doc/" ,name "-" ,version)))
                 (install-file "README.md" doc)
                 #t))))))
      (native-inputs
       (list autoconf automake pkg-config))
      (inputs
       (list openssl))
      (home-page "https://github.com/patpadgett/corkscrew")
      (synopsis "SSH tunneling through HTTP(S) proxies")
      (description
       "Corkscrew tunnels SSH connections through most HTTP and HTTPS proxies.
It supports proxy authentication through the HTTP basic authentication scheme
with optional @acronym{TLS, Transport-Level Security} to protect credentials.")
      (license license:gpl2+))))

(define-public mosh
  (package
    (name "mosh")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mosh.org/mosh-" version ".tar.gz"))
              (sha256
               (base32
                "1pax8sqlvcc7ammsxd9r53yx4m2hg1827wfz6f4rrwjx9q9lnbl7"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-FHS-file-names
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "scripts/mosh.pl"
                (("/bin/sh" shell)
                 (search-input-file inputs shell)))))
          (add-after 'install 'wrap
            (lambda _
              ;; Make sure 'mosh' can find 'mosh-client' and 'mosh-server'.
              (let ((bin (string-append #$output "/bin")))
                (wrap-program (string-append bin "/mosh")
                  `("PATH" ":" prefix (,bin)))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list bash-minimal
           boost
           ncurses
           openssl
           perl
           perl-io-tty
           protobuf
           zlib))
    (home-page "https://mosh.org/")
    (synopsis "Remote shell tolerant to intermittent connectivity")
    (description
     "Mosh is a remote terminal application that allows client roaming, supports
intermittent connectivity, and provides intelligent local echo and line editing
of user keystrokes.  It's a replacement for SSH that's more robust and
responsive, especially over Wi-Fi, cellular, and long-distance links.")
    (license license:gpl3+)))

(define-public dropbear
  (package
    (name "dropbear")
    (version "2025.88")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://matt.ucc.asn.au/dropbear/releases/"
             "dropbear-" version ".tar.bz2"))
       (sha256
        (base32 "1m2kvir3ckafi9lyb435iyxl9ypcdpdsz3spi7d1cz5i4zm50gvq"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "libtommath")
           (delete-file-recursively "libtomcrypt")
           (substitute* "configure"
             (("-ltomcrypt") "-ltomcrypt -ltommath"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--disable-bundled-libtom")
      ;; The test suite runs an instance of dropbear, which requires a
      ;; resolver ("Error resolving: Servname not supported for ai_socktype").
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'enable-x11-forwarding
                     (lambda _
                       (substitute* "src/default_options.h"
                         (("#define DROPBEAR_X11FWD 0")
                          "#define DROPBEAR_X11FWD 1")))))))
    (inputs (list libtomcrypt libtommath libxcrypt zlib))
    (synopsis "Small SSH server and client")
    (description "Dropbear is a relatively small SSH server and
client.  It runs on a variety of POSIX-based platforms.  Dropbear is
particularly useful for embedded systems, such as wireless routers.")
    (home-page "https://matt.ucc.asn.au/dropbear/dropbear.html")
    (license (license:x11-style "" "See file LICENSE."))))

(define-public liboop
  (package
    (name "liboop")
    (version "1.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://ftp.lysator.liu.se/pub/liboop/"
                          name "-" version ".tar.gz"))
      (sha256
       (base32
        "1q0p1l72pq9k3bi7a366j2rishv7dzzkg3i6r2npsfg7cnnidbsn"))))
    (build-system gnu-build-system)
    (home-page "https://www.lysator.liu.se/liboop/")
    (synopsis "Event loop library")
    (description "Liboop is a low-level event loop management library for
POSIX-based operating systems.  It supports the development of modular,
multiplexed applications which may respond to events from several sources.  It
replaces the \"select() loop\" and allows the registration of event handlers
for file and network I/O, timers and signals.  Since processes use these
mechanisms for almost all external communication, liboop can be used as the
basis for almost any application.")
    (license license:lgpl2.1+)))

(define-public lsh
  (package
    (name "lsh")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/lsh/lsh-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1qqjy9zfzgny0rkb27c8c7dfsylvb6n0ld8h3an2r83pmaqr9gwb"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "src/testsuite/functions.sh"
                    (("localhost")
                     ;; Avoid host name lookups since they don't work in
                     ;; chroot builds.
                     "127.0.0.1")
                    (("set -e")
                     ;; Make tests more verbose.
                     "set -e\nset -x"))

                  (substitute* (find-files "src/testsuite" "-test$")
                    (("localhost") "127.0.0.1"))

                  (substitute* "src/testsuite/login-auth-test"
                    (("/bin/cat") "cat"))))
              (patches (search-patches "lsh-fix-x11-forwarding.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           m4
           guile-2.0
           gperf
           psmisc))                       ; for `killall'
    (inputs
     (list nettle-2
           linux-pam
           ;; 'rl.c' uses the 'CPPFunction' type, which is no longer in
           ;; Readline 6.3.
           readline-6.2
           liboop
           zlib
           gmp
           ;; The server (lshd) invokes xauth when X11 forwarding is requested.
           ;; This adds 24 MiB (or 27%) to the closure of lsh.
           xauth
           libxau                       ;also required for x11-forwarding
           libxcrypt))
    (arguments
     '(;; Skip the `configure' test that checks whether /dev/ptmx &
       ;; co. work as expected, because it relies on impurities (for
       ;; instance, /dev/pts may be unavailable in chroots.)
       #:configure-flags '("lsh_cv_sys_unix98_ptys=yes"

                           ;; Use glibc's argp rather than the bundled one.
                           "--with-system-argp"

                           ;; 'lsh_argp.h' checks HAVE_ARGP_PARSE but nothing
                           ;; defines it.
                           "CPPFLAGS=-DHAVE_ARGP_PARSE"

                           ;; Fix the build of lsh@2.1 with GCC 10.
                           "CFLAGS=-O2 -g -fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; FIXME: Most tests won't run in a chroot, presumably because
             ;; /etc/profile is missing, and thus clients get an empty $PATH
             ;; and nothing works.  Run only the subset that passes.
             (delete-file "configure")  ;force rebootstrap
             (substitute* "src/testsuite/Makefile.am"
               (("seed-test \\\\")        ;prevent trailing slash
                "seed-test")
               (("^\t(lsh|daemon|tcpip|socks|lshg|lcp|rapid7|lshd).*test.*")
                ""))))
         (add-before 'configure 'pre-configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((nettle    (assoc-ref inputs "nettle"))
                    (sexp-conv (string-append nettle "/bin/sexp-conv")))
               ;; Remove argp from the list of sub-directories; we don't want
               ;; to build it, really.
               (substitute* "src/Makefile.in"
                 (("^SUBDIRS = argp")
                  "SUBDIRS ="))

               ;; Make sure 'lsh' and 'lshd' pick 'sexp-conv' in the right place
               ;; by default.
               (substitute* "src/environ.h.in"
                 (("^#define PATH_SEXP_CONV.*")
                  (string-append "#define PATH_SEXP_CONV \""
                                 sexp-conv "\"\n")))

               ;; Same for the 'lsh-authorize' script.
               (substitute* "src/lsh-authorize"
                 (("=sexp-conv")
                  (string-append "=" sexp-conv)))

               ;; Tell lshd where 'xauth' lives.  Another option would be to
               ;; hardcode "/run/current-system/profile/bin/xauth", thereby
               ;; reducing the closure size, but that wouldn't work on foreign
               ;; distros.
               (with-fluids ((%default-port-encoding "ISO-8859-1"))
                 (substitute* "src/server_x11.c"
                   (("define XAUTH_PROGRAM.*")
                    (string-append "define XAUTH_PROGRAM \""
                                   (assoc-ref inputs "xauth")
                                   "/bin/xauth\"\n")))))

             ;; Tests rely on $USER being set.
             (setenv "USER" "guix"))))))
    (home-page "https://www.lysator.liu.se/~nisse/lsh/")
    (synopsis "GNU implementation of the Secure Shell (ssh) protocols")
    (description
     "GNU lsh is a free implementation of the SSH version 2 protocol.  It is
used to create a secure line of communication between two computers,
providing shell access to the server system from the client.  It provides
both the server daemon and the client application, as well as tools for
manipulating key files.")
    (license license:gpl2+)))

(define-public sshpass
  (package
    (name "sshpass")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sshpass/sshpass/"
                           version "/sshpass-" version ".tar.gz"))
       (sha256
        (base32 "1npfvxxqs77qg6l4s6cn8q3b98zwr9n8rb9vra2n3dfb0g10c4dd"))))
    (build-system gnu-build-system)
    (home-page "https://sourceforge.net/projects/sshpass/")
    (synopsis "Non-interactive password authentication with SSH")
    (description "sshpass is a tool for non-interactively performing password
authentication with SSH's so-called @dfn{interactive keyboard password
authentication}.")
    (license license:gpl2+)))

(define-public autossh
  (package
    (name "autossh")
    (version "1.4g")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.harding.motd.ca/autossh/autossh-"
             version ".tgz"))
       (sha256
        (base32 "0xqjw8df68f4kzkns5gcah61s5wk0m44qdk2z1d6388w6viwxhsz"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; There is no "make check" or anything similar
    (inputs (list openssh))
    (synopsis "Automatically restart SSH sessions and tunnels")
    (description "autossh is a program to start a copy of @command{ssh} and
monitor it, restarting it as necessary should it die or stop passing traffic.")
    (home-page "https://www.harding.motd.ca/autossh/")
    (license
     ;; Why point to a source file?  Well, all the individual files have a
     ;; copy of this license in their headers, but there's no separate file
     ;; with that information.
     (license:non-copyleft "file://autossh.c"))))

(define-public pdsh
  (package
    (name "pdsh")
    (version "2.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/chaos/pdsh/"
                           "releases/download/pdsh-" version
                           "/pdsh-" version ".tar.gz"))
       (sha256
        (base32 "1s91hmhrz7rfb6h3l5k97s393rcm1ww3svp8dx5z8vkkc933wyxl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--with-ssh")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* '("tests/t0006-pdcp.sh"
                            "tests/t0004-module-loading.sh"
                            "tests/t2001-ssh.sh"
                            "tests/t1003-slurm.sh"
                            "tests/t6036-long-output-lines.sh"
                            "tests/aggregate-results.sh"
                            "tests/t2000-exec.sh"
                            "tests/t0002-internal.sh"
                            "tests/t1002-dshgroup.sh"
                            "tests/t5000-dshbak.sh"
                            "tests/t0001-basic.sh"
                            "tests/t0005-rcmd_type-and-user.sh"
                            "tests/test-lib.sh"
                            "tests/t2002-mrsh.sh"
                            "tests/t0003-wcoll.sh"
                            "tests/test-modules/pcptest.c")
               (("/bin/sh") (which "bash")))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "tests/t6036-long-output-lines.sh"
               (("which") (which "which")))
             #t)))))
    (inputs
     (list openssh mit-krb5 perl))
    (native-inputs
     (list which))
    (home-page "https://github.com/chaos/pdsh")
    (synopsis "Parallel distributed shell")
    (description "Pdsh is a an efficient, multithreaded remote shell client
which executes commands on multiple remote hosts in parallel.  Pdsh implements
dynamically loadable modules for extended functionality such as new remote
shell services and remote host selection.")
    (license license:gpl2+)))

(define-public python-asyncssh
  (package
    (name "python-asyncssh")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asyncssh" version))
       (sha256
        (base32 "08viv0k32l6f40sbraq4bqzizrkivf09zwy8kmqvjq0zq1hj2chs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    (list
                     ;; TODO Test fails for unknown reason
                     "not test_confirm"
                     #$@(if (target-aarch64?)
                            (list
                             ;; Tests fail with: asyncssh.misc.ConnectionLost:
                             ;; Connection lost
                             "test_connect_non_tcp_sock"
                             "test_connect_reverse_proxy"
                             "test_get_server_auth_methods_no_sockn"
                             "test_get_server_auth_methods_no_sockname"
                             "test_get_server_host_key_proxy")
                            '()))
                    " and not " ))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda* _
              (substitute* "tests/test_connection.py"
                ;; nc is always available.
                (("which nc") "true")))))))
    (native-inputs
     (list netcat
           openssh
           openssl
           python-aiofiles
           python-fido2
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-cryptography
           python-pyopenssl
           python-gssapi
           python-bcrypt
           python-typing-extensions))
    (home-page "https://asyncssh.readthedocs.io/")
    (synopsis "Asynchronous SSHv2 client and server library for Python")
    (description
     "AsyncSSH is a Python package which provides an asynchronous client and
server implementation of the SSHv2 protocol on top of the Python 3.6+ asyncio
framework.")
    (license license:epl2.0)))

(define-public clustershell
  (package
    (name "clustershell")
    (version "1.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cea-hpc/clustershell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zk3syrdck2gi27b9njaq98fnnjf14831yvkma2n4ydsf2mxnkaw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; tests require python-nose, and most of them fail
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-pathes
            (lambda _
              (let ((ssh #$(this-package-input "openssh")))
                (substitute* (list "lib/ClusterShell/Worker/Ssh.py"
                                   "lib/ClusterShell/Worker/fastsubprocess.py")
                  (("\"/bin/sh\"") (format #f "'~a'" (which "sh")))
                  (("\"ssh\"")     (format #f "'~a/bin/ssh'" ssh))
                  (("\"scp\"")     (format #f "'~a/bin/scp'" ssh)))
                (substitute* (find-files "./tests" "\\.py$")
                  (("\"/bin/hostname\"") (format #f "'~a'" (which "hostname")))
                  (("/bin/sleep")        "sleep")
                  (("/bin/echo")         "echo")
                  (("/bin/uname")        "uname")
                  (("/bin/false")        "false")
                  (("/bin/true")         "true")
                  (("/usr/bin/printf")   "printf"))))))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (inputs
     (list openssh))
    (propagated-inputs
     (list python-pyyaml))
    (home-page "https://cea-hpc.github.io/clustershell/")
    (synopsis "Scalable event-driven Python framework for cluster administration")
    (description
     "ClusterShell is an event-driven Python framework, designed to run local
or distant commands in parallel on server farms or on large GNU/Linux
clusters.  It will take care of common issues encountered on HPC clusters,
such as operating on groups of nodes, running distributed commands using
optimized execution algorithms, as well as gathering results and merging
identical outputs, or retrieving return codes.  ClusterShell takes advantage
of existing remote shell facilities such as SSH.")
    (license license:lgpl2.1+)))

(define-public endlessh
  (package
    (name "endlessh")
    (version "1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/skeeto/endlessh")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ziwr8j1frsp3dajr8h5glkm1dn5cci404kazz5w1jfrp0736x68"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          ,(string-append "CC=" (cc-for-target)))
       #:tests? #f                      ; no test target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (home-page "https://github.com/skeeto/endlessh")
    (synopsis "SSH tarpit that slowly sends an endless banner")
    (description
     "Endlessh is an SSH tarpit that very slowly sends an endless, random SSH
banner.  It keeps SSH clients locked up for hours or even days at a time.  The
purpose is to put your real SSH server on another port and then let the script
kiddies get stuck in this tarpit instead of bothering a real server.

Since the tarpit is in the banner before any cryptographic exchange occurs, this
program doesn't depend on any cryptographic libraries.  It's a simple,
single-threaded, standalone C program.  It uses @code{poll()} to trap multiple
clients at a time.")
    (license license:unlicense)))

(define-public webssh
  (package
    (name "webssh")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huashengdun/webssh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v0dxnqac9xdj06lhljv6bhi8hd16rn6h0qr7fkm640nvr55a8i1"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-paramiko
           python-tornado))
    (home-page "https://webssh.huashengdun.org/")
    (synopsis "Web application to be used as an SSH client")
    (description
     "This package provides a web application to be used as an SSH client.

Features:
@itemize @bullet
@item SSH password authentication supported, including empty password.
@item SSH public-key authentication supported, including DSA RSA ECDSA
Ed25519 keys.
@item Encrypted keys supported.
@item Two-Factor Authentication (time-based one-time password) supported.
@item Fullscreen terminal supported.
@item Terminal window resizable.
@item Auto detect the ssh server's default encoding.
@item Modern browsers are supported.
@end itemize")
    (license license:expat)))

(define-public x11-ssh-askpass
  (package
    (name "x11-ssh-askpass")
    (version "1.2.4.1")
    (source
     (origin
       (method url-fetch)
       ;; The project home page seams to be offline.
       (uri (string-append "https://pkgs.fedoraproject.org/repo/pkgs/openssh/"
                           name "-" version ".tar.gz"
                           "/8f2e41f3f7eaa8543a2440454637f3c3/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "124c1frwvdmg4nv8xqv435ibjhj2y8xc1bmfr6i8a8g75b1y63b2"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:make-flags
      #~(list (string-append "BINDIR=" #$output "/libexec")
              (string-append "MANDIR=" #$output "/share/man"))
      #:configure-flags
      #~(list (string-append "--mandir="
                             "/usr/share/man/test")
              (string-append "--libexecdir="
                             "/usr/lib/ssh/test")
              (string-append "--with-app-defaults-dir="
                             "/usr/share/X11/app-defaults/test"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'xmkmf
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((imake #$(this-package-native-input "imake")))
                (invoke "xmkmf")
                (substitute* "Makefile"
                  ;; These imake variables somehow remain undefined
                  (("DefaultGcc2[[:graph:]]*Opt") "-O2")
                  ;; Reset a few variable defaults that are set in imake
                  ;; templates.
                  ((imake) #$output)
                  (("(MANPATH = )[[:graph:]]*" _ front)
                   (string-append front #$output "/share/man"))))))
          (add-after 'xmkmf 'make-includes
            (lambda _
              (invoke "make" "includes")))
          (add-after 'install 'install/doc
            (lambda _
              (lambda _
                (invoke "make"
                        (string-append "MANDIR=" #$output "/share/man")
                        "install.man")))))))
    (native-inputs (list imake))
    (inputs (list libxt))
    (home-page "http://www.jmknoble.net/software/x11-ssh-askpass/")
    (synopsis "Lightweight passphrase dialog for SSH")
    (description "code{x11-ssh-askpass} is an X11-based pass-phrase dialog for
use with OpenSSH.")
    (license license:gpl2+)))
