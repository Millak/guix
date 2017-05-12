;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:autoload   (gnu packages boost) (boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:autoload   (gnu packages protobuf) (protobuf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public libssh
  (package
    (name "libssh")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://red.libssh.org/attachments/download/210/libssh-"
                    version ".tar.xz"))
              (sha256
               (base32
                "03bcp9ksqp0s1pmwfmzhcknvkxay5k0mjzzxp3rjlifbng1vxq9r"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DWITH_GCRYPT=ON")

       ;; TODO: Add 'CMockery' and '-DWITH_TESTING=ON' for the test suite.
       #:tests? #f))
    (inputs `(("zlib" ,zlib)
              ("libgcrypt" ,libgcrypt)))
    (synopsis "SSH client library")
    (description
     "libssh is a C library implementing the SSHv2 and SSHv1 protocol for
client and server implementations.  With libssh, you can remotely execute
programs, transfer files, and use a secure and transparent tunnel for your
remote applications.")
    (home-page "https://www.libssh.org")
    (license license:lgpl2.1+)))

(define-public libssh2
  (package
   (name "libssh2")
   (version "1.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "https://www.libssh2.org/download/libssh2-"
                   version ".tar.gz"))
            (sha256
             (base32
              "1m3n8spv79qhjq4yi0wgly5s5rc8783jb1pyra9bkx1md0plxwrr"))
            (patches
             (search-patches "libssh2-fix-build-failure-with-gcrypt.patch"))))
   (build-system gnu-build-system)
   ;; The installed libssh2.pc file does not include paths to libgcrypt and
   ;; zlib libraries, so we need to propagate the inputs.
   (propagated-inputs `(("libgcrypt" ,libgcrypt)
                        ("zlib" ,zlib)))
   (arguments '(#:configure-flags `("--with-libgcrypt")
                #:phases (modify-phases %standard-phases
                           (add-before 'configure 'autoreconf
                             (lambda _
                               (zero? (system* "autoreconf" "-v")))))))
   (native-inputs `(("autoconf" ,autoconf)
                    ("automake" ,automake)))
   (synopsis "Client-side C library implementing the SSH2 protocol")
   (description
    "libssh2 is a library intended to allow software developers access to
the SSH-2 protocol in an easy-to-use self-contained package.  It can be built
into an application to perform many different tasks when communicating with
a server that supports the SSH-2 protocol.")
   (license license:bsd-3)
   (home-page "http://www.libssh2.org/")))

(define-public openssh
  (package
   (name "openssh")
   (version "7.5p1")
   (source (origin
            (method url-fetch)
            (uri (let ((tail (string-append name "-" version ".tar.gz")))
                   (list (string-append "http://openbsd.cs.fau.de/pub/OpenBSD/OpenSSH/portable/"
                                        tail)
                         (string-append "http://ftp.fr.openbsd.org/pub/OpenBSD/OpenSSH/portable/"
                                        tail)
                         (string-append "http://ftp2.fr.openbsd.org/pub/OpenBSD/OpenSSH/portable/"
                                        tail))))
            (sha256 (base32
                     "1w7rb5gbrikxdkp8w7zxnci4549gk4bw1lml01s59w5rzb2y6ilq"))))
   (build-system gnu-build-system)
   (inputs `(("groff" ,groff)
             ("openssl" ,openssl)
             ("pam" ,linux-pam)
             ("mit-krb5" ,mit-krb5)
             ("zlib" ,zlib)
             ("xauth" ,xauth)))                   ;for 'ssh -X' and 'ssh -Y'
   (arguments
    `(#:test-target "tests"
      #:configure-flags  `("--sysconfdir=/etc/ssh"

                           ;; Default value of 'PATH' used by sshd.
                          "--with-default-path=/run/current-system/profile/bin"

                          ;; configure needs to find krb5-config
                          ,(string-append "--with-kerberos5="
                                          (assoc-ref %build-inputs "mit-krb5")
                                          "/bin")

                          ;; Enable PAM support in sshd.
                          "--with-pam")

      #:phases
      (modify-phases %standard-phases
        (add-after 'configure 'reset-/var/empty
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (substitute* "Makefile"
               (("PRIVSEP_PATH=/var/empty")
                (string-append "PRIVSEP_PATH=" out "/var/empty")))
             #t)))
        (add-before 'check 'patch-tests
         (lambda _
           ;; remove 't-exec' regress target which requires user 'sshd'
           (substitute* "regress/Makefile"
             (("^(REGRESS_TARGETS=.*) t-exec(.*)" all pre post)
              (string-append pre post)))
           #t))
        (replace 'install
         (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
           ;; install without host keys and system configuration files
           (and (zero? (apply system* "make" "install-nosysconf" make-flags))
                (begin
                  (install-file "contrib/ssh-copy-id"
                                (string-append (assoc-ref outputs "out")
                                               "/bin/"))
                  (chmod (string-append (assoc-ref outputs "out")
                                        "/bin/ssh-copy-id") #o555)
                  (install-file "contrib/ssh-copy-id.1"
                                (string-append (assoc-ref outputs "out")
                                               "/share/man/man1/"))
                  #t)))))))
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
   (home-page "http://www.openssh.org/")))

(define-public guile-ssh
  (package
    (name "guile-ssh")
    (version "0.10.2")
    (home-page "https://github.com/artyom-poptsov/guile-ssh")
    (source (origin
              ;; ftp://memory-heap.org/software/guile-ssh/guile-ssh-VERSION.tar.gz
              ;; exists, but the server appears to be too slow and unreliable.
              ;; Also, using this URL allows the GitHub updater to work.
              (method url-fetch)
              (uri (string-append home-page "/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pkiq3fm15pr4w1r420rrwwfmi4jz492r6l6vzjk6v73xlyfyfl3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'autoreconf
                    (lambda* (#:key inputs #:allow-other-keys)
                      (zero? (system* "autoreconf" "-vfi"))))
                  (add-before 'build 'fix-libguile-ssh-file-name
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Build and install libguile-ssh.so so that we can use
                      ;; its absolute file name in .scm files, before we build
                      ;; the .go files.
                      (and (zero? (system* "make" "install"
                                           "-C" "libguile-ssh"
                                           "-j" (number->string
                                                 (parallel-job-count))))
                           (let* ((out      (assoc-ref outputs "out"))
                                  (libdir   (string-append out "/lib")))
                             (substitute* (find-files "." "\\.scm$")
                               (("\"libguile-ssh\"")
                                (string-append "\"" libdir "/libguile-ssh\"")))
                             #t)))))

       ;; Tests are not parallel-safe.
       #:parallel-tests? #f))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("texinfo" ,texinfo)
                     ("pkg-config" ,pkg-config)
                     ("which" ,which)))
    (inputs `(("guile" ,guile-2.0)
              ("libssh" ,libssh)
              ("libgcrypt" ,libgcrypt)))
    (synopsis "Guile bindings to libssh")
    (description
     "Guile-SSH is a library that provides access to the SSH protocol for
programs written in GNU Guile interpreter.  It is a wrapper to the underlying
libssh library.")
    (license license:gpl3+)))

(define-public guile2.2-ssh
  ;; This is a snapshot of a unofficial copy of Guile-SSH, which hopefully
  ;; reflects the upcoming release well enough.
  (let ((commit "926a0843626f89e3db02d01a6b01cc1f0d9cefcf")
        (revision "0"))
    (package
      (inherit guile-ssh)
      (name "guile2.2-ssh")
      (version (string-append "0.10.2." revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://notabug.org/civodul/guile-ssh/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0hf28macq8d1w05g0xy2lw1s5vzmcrixh7m43x7qvvdd31c998ip"))))
      (inputs `(("guile" ,guile-2.2)
                ,@(alist-delete "guile" (package-inputs guile-ssh)))))))

(define-public corkscrew
  (package
    (name "corkscrew")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       ;; The agroman.net domain name expired on 2017-03-23, and the original
       ;; "http://www.agroman.net/corkscrew/corkscrew-2.0.tar.gz" now returns
       ;; bogus HTML.  Perhaps it will yet return.  Until then, use a mirror.
       (uri (string-append "https://downloads.openwrt.org/sources/"
                           "corkscrew-" version ".tar.gz"))
       (sha256 (base32
                "1gmhas4va6gd70i2x2mpxpwpgww6413mji29mg282jms3jscn3qd"))))
    (build-system gnu-build-system)
    (arguments
     ;; Replace configure phase as the ./configure script does not link
     ;; CONFIG_SHELL and SHELL passed as parameters
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs inputs system build target
                           #:allow-other-keys #:rest args)
             (let* ((configure (assoc-ref %standard-phases 'configure))
                    (prefix (assoc-ref outputs "out"))
                    (bash   (which "bash"))
                    ;; Set --build and --host flags as the provided config.guess
                    ;; is not able to detect them
                    (flags `(,(string-append "--prefix=" prefix)
                             ,(string-append "--build=" build)
                             ,(string-append "--host=" (or target build)))))
               (setenv "CONFIG_SHELL" bash)
               (zero? (apply system* bash
                             (string-append "." "/configure")
                             flags)))))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/corkscrew")))
               (install-file "README" doc)
               #t))))))
    (home-page "http://www.agroman.net/corkscrew")
    (synopsis "SSH tunneling through HTTP(S) proxies")
    (description
     "Corkscrew tunnels SSH connections through most HTTP and HTTPS proxies.
Proxy authentication is only supported through the plain-text HTTP basic
authentication scheme.")
    (license license:gpl2+)))

(define-public mosh
  (package
    (name "mosh")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mosh.org/mosh-" version ".tar.gz"))
              (sha256
               (base32
                "0xikz40q873g9ihvz3x6bwkcb9hb8kcnp5wpcmb72pg5c7s143ij"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure 'mosh' can find 'mosh-client' and
             ;; 'mosh-server'.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (wrap-program (string-append bin "/mosh")
                             `("PATH" ":" prefix (,bin)))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)
       ("perl" ,perl)
       ("perl-io-tty" ,perl-io-tty)
       ("zlib" ,zlib)
       ("ncurses" ,ncurses)
       ("protobuf" ,protobuf)
       ("boost-headers" ,boost)))
    (home-page "https://mosh.org/")
    (synopsis "Remote shell tolerant to intermittent connectivity")
    (description
     "Remote terminal application that allows roaming, supports intermittent
connectivity, and provides intelligent local echo and line editing of user
keystrokes.  Mosh is a replacement for SSH.  It's more robust and responsive,
especially over Wi-Fi, cellular, and long-distance links.")
    (license license:gpl3+)))

(define-public dropbear
  (package
    (name "dropbear")
    (version "2016.74")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://matt.ucc.asn.au/" name "/releases/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "14c8f4gzixf0j9fkx68jgl85q7b05852kk0vf09gi6h0xmafl817"))))
    (build-system gnu-build-system)
    (arguments  `(#:tests? #f)) ; There is no "make check" or anything similar
    (inputs `(("zlib" ,zlib)))
    (synopsis "Small SSH server and client")
    (description "Dropbear is a relatively small SSH server and
client.  It runs on a variety of POSIX-based platforms.  Dropbear is
particularly useful for embedded systems, such as wireless routers.")
    (home-page "https://matt.ucc.asn.au/dropbear/dropbear.html")
    (license (license:x11-style "" "See file LICENSE."))))

(define-public liboop
  (package
    (name "liboop")
    (version "1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://download.ofb.net/liboop/liboop-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0z6rlalhvfca64jpvksppc9bdhs7jwhiw4y35g5ibvh91xp3rn1l"))
      (patches (search-patches "liboop-mips64-deplibs-fix.patch"))))
    (build-system gnu-build-system)
    (home-page "http://www.lysator.liu.se/liboop/")
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
                    (("/bin/cat") "cat"))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("m4" ,m4)
       ("guile" ,guile-2.0)
       ("gperf" ,gperf)
       ("psmisc" ,psmisc)))                       ; for `killall'
    (inputs
     `(("nettle" ,nettle-2)
       ("linux-pam" ,linux-pam)

       ;; 'rl.c' uses the 'CPPFunction' type, which is no longer in
       ;; Readline 6.3.
       ("readline" ,readline-6.2)

       ("liboop" ,liboop)
       ("zlib" ,zlib)
       ("gmp" ,gmp)

       ;; The server (lshd) invokes xauth when X11 forwarding is requested.
       ;; This adds 24 MiB (or 27%) to the closure of lsh.
       ("xauth" ,xauth)))
    (arguments
     '(;; Skip the `configure' test that checks whether /dev/ptmx &
       ;; co. work as expected, because it relies on impurities (for
       ;; instance, /dev/pts may be unavailable in chroots.)
       #:configure-flags '("lsh_cv_sys_unix98_ptys=yes"

                           ;; Use glibc's argp rather than the bundled one.
                           "--with-system-argp"

                           ;; 'lsh_argp.h' checks HAVE_ARGP_PARSE but nothing
                           ;; defines it.
                           "CPPFLAGS=-DHAVE_ARGP_PARSE")

       ;; FIXME: Tests won't run in a chroot, presumably because
       ;; /etc/profile is missing, and thus clients get an empty $PATH
       ;; and nothing works.
       #:tests? #f

       #:phases
       (modify-phases %standard-phases
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
    (home-page "http://www.lysator.liu.se/~nisse/lsh/")
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
    (version "1.06")
    (synopsis "Non-interactive password authentication with SSH")
    (home-page "https://sourceforge.net/projects/sshpass/")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sshpass/sshpass/"
                           version "/sshpass-" version ".tar.gz"))
       (sha256
        (base32
         "0q7fblaczb7kwbsz0gdy9267z0sllzgmf0c7z5c9mf88wv74ycn6"))))
    (build-system gnu-build-system)
    (description "sshpass is a tool for non-interactivly performing password
authentication with SSH's so-called @dfn{interactive keyboard password
authentication}.")
    (license license:gpl2+)))

(define-public autossh
  (package
    (name "autossh")
    (version "1.4e")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.harding.motd.ca/autossh/autossh-"
             version ".tgz"))
       (sha256
        (base32 "0mlicw28vq2jxa0jf0dys5ja75v0fxpjavlq9dpif6bnknji13ly"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; There is no "make check" or anything similar
    (inputs `(("openssh" ,openssh)))
    (synopsis "Automatically restart SSH sessions and tunnels")
    (description "autossh is a program to start a copy of @command{ssh} and
monitor it, restarting it as necessary should it die or stop passing traffic.")
    (home-page "http://www.harding.motd.ca/autossh/")
    (license
     ;; Why point to a source file?  Well, all the individual files have a
     ;; copy of this license in their headers, but there's no separate file
     ;; with that information.
     (license:non-copyleft "file://autossh.c"))))
