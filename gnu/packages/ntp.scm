;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2022, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages ntp)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby-check)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system waf)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public chrony
  (package
    (name "chrony")
    (version "4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/chrony/chrony.git/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pqplqlfy1b05qkj7d5l954z7rdz5mbwr9d0hvsa7s6bwqsrl791"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules
      '((srfi srfi-26)
        (guix build utils)
        (guix build gnu-build-system))
      #:configure-flags
      #~(list "--enable-scfilter"
              "--with-ntp-era=0"
              "--with-sendmail=sendmail"
              "--with-user=chrony")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-CC
            (lambda _
              (setenv "CC" #$(cc-for-target))))
          #$@(if (this-package-native-input "ruby-asciidoctor")
                 #~()
                 #~((add-after 'unpack 'adjust-makefile
                      (lambda _
                        (substitute* "doc/Makefile.in"
                          (("install:")
                           (string-append "install:\n\n"
                                          "not-install:")))))))
          (add-after 'unpack 'stay-inside-out
            ;; Simply setting CHRONYVARDIR to something nonsensical at install
            ;; time would result in nonsense file names in man pages.
            (lambda _
              (substitute* "Makefile.in"
                (("mkdir -p \\$\\(DESTDIR\\)\\$\\(CHRONYVARDIR\\)") ":"))))
          (add-after 'install 'install-more-documentation
            (lambda _
              (let* ((doc (string-append #$output "/share/doc/"
                                         #$name "-" #$version)))
                (for-each (cut install-file <> doc)
                          (list "README"))
                (copy-recursively "examples"
                                  (string-append doc "/examples"))))))))
    (native-inputs
     (append (list bison
                   pkg-config)
             (if (supported-package? ruby-asciidoctor/minimal)
                 (list ruby-asciidoctor/minimal)
                 '())))
    (inputs
     (list gnutls libcap libseccomp nettle))
    (home-page "https://chrony-project.org/")
    (synopsis "System clock synchronization service that speaks NTP")
    (description
     "Chrony keeps your system time accurate.  It synchronises your computer's
clock with @acronym{NTP, Network Time Protocol} servers, reference clocks such
as GPS receivers, or even manual input of the correct time from a wristwatch.

Chrony will determine the rate at which the computer gains or loses time, and
compensate for it.  It can also operate as an
NTPv4 (@url{https://www.rfc-editor.org/rfc/rfc5905, RFC 5905}) server and peer to
tell time to other computers on the network.

It's designed to perform well even under adverse conditions: congested
networks, unreliable clocks drifting with changes in temperature, and devices
or virtual machines that are frequently turned off and connect to the Internet
for only a few minutes at a time.

Typical accuracy when synchronised over the Internet is several milliseconds.
On a local network this can reach tens of microseconds.  With hardware
time-stamping or reference clock, sub-microsecond accuracy is possible.")
    (license l:gpl2)))

(define-public ntp
  (package
    (name "ntp")
    (version "4.2.8p18")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append
                   "https://www.eecis.udel.edu/~ntp/ntp_spool/ntp4/ntp-"
                   (version-major+minor version) "/ntp-" version ".tar.gz")
                  (string-append "http://archive.ntp.org/ntp4/ntp-"
                                 (version-major+minor version) "/ntp-" version
                                 ".tar.gz")))
       (sha256
        (base32 "1rb8yksqxjcsjvww9kwnw1242qzszwixh916jj254a8szgrwb16g"))
       (patches (search-patches
                 "ntp-fix-dereferencing-the-wrong-variable.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Remove the bundled copy of libevent, but we must keep
                   ;; sntp/libevent/build-aux since configure.ac contains
                   ;; AC_CONFIG_AUX_DIR([sntp/libevent/build-aux])
                   (rename-file "sntp/libevent/build-aux"
                                "sntp/libevent:build-aux")
                   (delete-file-recursively "sntp/libevent")
                   (mkdir "sntp/libevent")
                   (rename-file "sntp/libevent:build-aux"
                                "sntp/libevent/build-aux")))))
    (native-inputs (list which pkg-config))
    (inputs (cons* openssl libevent
                   ;; Build with POSIX capabilities support on GNU/Linux.  This allows
                   ;; 'ntpd' to run as non-root (when invoked with '-u'.)
                   (if (target-linux?)
                       (list libcap)
                       '())))
    (arguments
     (list
      ;; Pass "--with-yielding-select=yes" so that 'configure' knows whether
      ;; 'select' yields when using pthreads in a cross-compilation context.
      #:configure-flags
      #~(list
         "CFLAGS=-g -O2 -Wno-error=int-conversion"
         "--with-yielding-select=yes")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-network-test
            (lambda _
              (substitute* "tests/libntp/Makefile.in"
                (("test-decodenetnum\\$\\(EXEEXT\\) ")
                 "")))))))
    (build-system gnu-build-system)
    (synopsis "Real time clock synchronization system")
    (description "NTP is a system designed to synchronize the clocks of
computers over a network.")
    (license (l:x11-style
              "https://www.eecis.udel.edu/~mills/ntp/html/copyright.html"
              "A non-copyleft free licence from the University of Delaware"))
    (home-page "https://www.ntp.org")))

(define-public ntpsec
  (package
    (name "ntpsec")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/NTPsec/ntpsec.git")
             (commit (string-append "NTPsec_"
                                    (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m23mzsj3iq36d14ncj0w74hy152blv61nxd8w4xcs52vsh48mps"))))
    (native-inputs (list bison
                         m4
                         pkg-config
                         pps-tools
                         python-waf
                         ruby-asciidoctor/minimal))
    (inputs (cons* gpsd openssl
                   ;; Build with POSIX capabilities and syscall filtering
                   ;; support on GNU/Linux, for extra security features.
                   (if (target-linux?)
                       (list libcap libseccomp)
                       '())))
    (arguments
     (list
      #:configure-flags
      #~(list "--refclock=all"
              "--enable-manpage"
              "--disable-doc"
              #$@(if (target-linux?)
                     '("--enable-seccomp")
                     '())
              "--enable-mssntp")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-network-test
            (lambda _
              (substitute* "tests/wscript"
                (("\"libntp/decodenetnum.c\",")
                 ""))
              (substitute* "tests/common/tests_main.c"
                (("RUN_TEST_GROUP\\(decodenetnum\\);")
                 ""))))
          ;; Tests fail without this. PYTHONPATH is ignored, unfortunately.
          (add-before 'build 'symlink-python-gps
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "build/main/tests/pylib")
              (let ((python-version #$(version-major+minor (package-version
                                                            python))))
                (symlink (string-append (assoc-ref inputs "gpsd")
                                        "/lib/python" python-version
                                        "/site-packages/gps")
                         "build/main/tests/pylib/gps")))))))
    (build-system waf-build-system)
    (synopsis "Real time clock synchronization system and utilities")
    (description
     "@acronym{NTP, Network Time Protocol}, the protocol, is used to keep
computer clocks accurate by synchronizing them over the Internet or a
local network, or by following an accurate hardware receiver that interprets
GPS, DCF-77, or similar time signals.

NTPsec is a hardened and improved implementation derived from the original
@url{https://www.ntp.org, NTP project}.  NTPsec supports
@acronym{NTS, Network Time Security} which provides cryptographically
authenticated time.

This package contains the NTP daemon and utility programs.  An NTP daemon
needs to be running on each host that is to have its clock accuracy controlled
by NTP.  The same NTP daemon is also used to provide NTP service to other hosts.")
    ;; All licenses are in file://LICENSES/
    (license (list l:bsd-2
                   l:bsd-3
                   l:bsd-4
                   l:isc
                   l:expat
                   l:asl2.0
                   l:cc-by4.0
                   (l:non-copyleft "file://LICENSES/Beerware.txt"
                    "See LICENSES/Beerware.txt in the distribution.")
                   (l:x11-style
                    "https://www.eecis.udel.edu/~mills/ntp/html/copyright.html"
                    "A non-copyleft free licence from the University of Delaware")))
    (home-page "https://www.ntpsec.org")))

(define-public openntpd
  (package
    (name "openntpd")
    (version "6.8p1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://openbsd/OpenNTPD/openntpd-" version ".tar.gz"))
              (sha256
               (base32
                "0ijsylc7a4jlpxsqa0jq1w1c7333id8pcakzl7a5749ria1xp0l5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let* ((libressl (assoc-ref %build-inputs "libressl"))
              (libressl-version ,(package-version
                                  (car (assoc-ref (package-inputs this-package)
                                                  "libressl")))))
         (list "--with-privsep-user=ntpd"
               "--localstatedir=/var"
               (string-append "--with-cacert=" libressl
                              "/share/libressl-" libressl-version
                              "/cert.pem")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'modify-install-locations
           (lambda _
             ;; Don't try to create /var/run or /var/db
             (substitute* "src/Makefile.in"
               (("DESTDIR\\)\\$\\(localstatedir") "TMPDIR"))
             #t)))))
    (inputs
     (list libressl)) ; enable TLS time constraints. See ntpd.conf(5).
    (home-page "https://www.openntpd.org/")
    (synopsis "NTP client and server by the OpenBSD Project")
    (description "OpenNTPD is the OpenBSD Project's implementation of a client
and server for the Network Time Protocol.  Its design goals include being
secure, easy to configure, and accurate enough for most purposes, so it's more
minimalist than ntpd.")
    (properties
     '((release-monitoring-url . "https://cdn.openbsd.org/pub/OpenBSD/OpenNTPD")))
    ;; A few of the source files are under bsd-3.
    (license (list l:isc l:bsd-3))))
