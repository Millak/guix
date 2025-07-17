;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Francesco Frassinelli <fraph24@gmail.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Adonay Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@hyperbola.info>
;;; Copyright © 2018 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2019, 2024 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2021, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 LibreMiami <packaging-guix@libremiami.org>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2022 Thomas Albers Raviola <thomas@thomaslabs.org>
;;; Copyright © 2023 Ivan Gankevich <igankevich@capybaramail.xyz>
;;; Copyright © 2025 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages telephony)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linphone)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages video)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt))

(define-public phonesim
  (package
    (name "phonesim")
    (version "1.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/network/ofono/phonesim")
             (commit "a7c844d45b047b2dae5b0877816c346fce4c47b9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rc1c2vr03dmi1dr3skj57v77ga9c22g29xs1qiphqms4isby9cq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-maintainer-mode"
             "CC=" ,(cc-for-target))))
    (native-inputs
     (list automake autoconf pkg-config))
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Phone Simulator for modem testing")
    (description
     "Phonesim is a modem emulator that oFono uses for development and
testing.  This allows oFono to be used by any host without requiring special
GSM (or other) hardware.")
    (home-page "https://git.kernel.org/pub/scm/network/ofono/phonesim")
    (license license:gpl2+)))

(define-public libilbc
  (package
    (name "libilbc")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/TimothyGu/libilbc")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32
         "1j1pn1w1198qvdiq2hgv9hkyq2nqcvmfnwlgppac633idkjnjrqx"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; No target
    (native-inputs
     (list pkg-config))
    (synopsis "Libre iLBC codec")
    (description "LibiLBC is a packaging friendly copy of the iLBC codec from
the WebRTC project.  It provides a base for distribution packages and can be
used as drop-in replacement for the non-free code from RFC 3591.")
    (home-page "https://github.com/TimothyGu/libilbc")
    (license license:bsd-3)))

(define-public spandsp
  (package
    (name "spandsp")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        ;; The original upstream has been down since the end of March 2020.
        (string-append "https://web.archive.org/web/20180626203108/"
                       "https://www.soft-switch.org/downloads/" name "/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32 "0rclrkyspzk575v8fslzjpgp4y2s4x7xk3r55ycvpi4agv33l1fc"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc" "static"))   ;doc contains HTML documentation
    (arguments
     `(#:configure-flags '("--enable-doc=yes" "--enable-tests=yes")
       #:parallel-build? #f ;non-deterministic build failures may occur otherwise
       #:parallel-tests? #f ;fails removing the same the files twice otherwise
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-configure.ac
                    (lambda _
                      ;; spandsp looks at hard coded locations of the FHS to
                      ;; find libxml2.
                      (substitute* "configure.ac"
                        (("AC_MSG_CHECKING\\(for libxml/xmlmemory\\.h.*" all)
                         (string-append all
                                        "PKG_CHECK_MODULES(XML2, libxml-2.0)\n"
                                        "CPPFLAGS+=\" $XML2_CFLAGS\"\n")))
                      ;; Force a regeneration of the autotools build system.
                      (delete-file "autogen.sh")
                      (delete-file "configure")
                      #t))
                  (add-after 'unpack 'do-not-install-data-files
                    ;; The .tiff images produced for tests are not
                    ;; reproducible and it is not desirable to have those
                    ;; distributed.
                    (lambda _
                      (substitute* '("test-data/itu/fax/Makefile.am"
                                     "test-data/etsi/fax/Makefile.am")
                        (("nobase_data_DATA")
                         "noinst_DATA"))
                      #t))
                  (add-after 'install 'install-doc
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((doc (string-append (assoc-ref outputs "doc")
                                                "/share/doc/" ,name "-" ,version)))
                        (copy-recursively "doc/t38_manual" doc)
                        #t)))
                  (add-after 'install 'move-static-libraries
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (static (assoc-ref outputs "static")))
                        (mkdir-p (string-append static "/lib"))
                        (with-directory-excursion out
                          (for-each (lambda (file)
                                      (rename-file file
                                                   (string-append static "/"
                                                                  file)))
                                    (find-files "lib" "\\.a$")))
                        #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ;; For the tests
       ("fftw" ,fftw)
       ("libpcap" ,libpcap)
       ("libsndfile" ,libsndfile)
       ("libjpeg" ,libjpeg-turbo)      ;XXX: should be propagated from libtiff
       ("libtiff" ,libtiff)
       ("netpbm" ,netpbm)
       ("sox" ,sox)
       ;; For the documentation
       ("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("doxygen" ,doxygen)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (synopsis "DSP library for telephony")
    (description "SpanDSP is a library of DSP functions for telephony, in the
8000 sample per second world of E1s, T1s, and higher order PCM channels.  It
contains low level functions, such as basic filters.  It also contains higher
level functions, such as cadenced supervisory tone detection, and a complete
software FAX machine.")
    (home-page "https://web.archive.org/web/20180626203108/\
https://www.soft-switch.org/index.html")
    (license (list license:lgpl2.1+  ;for the library
                   license:gpl2+)))) ;for the test suites and support programs

(define-public commoncpp
  (package
   (name "commoncpp")
   (version "1.8.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/" name "/commoncpp2-"
                   version ".tar.gz"))
            (sha256 (base32
                     "0kmgr5w3b1qwzxnsnw94q6rqs0hr8nbv9clf07ca2a2fyypx9kjk"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda _
             (substitute* "src/applog.cpp"
               (("^// TODO sc.*") "#include <sys/types.h>\n#include <sys/stat.h>\n"))
             #t)))))
   (build-system gnu-build-system)
   (synopsis "(u)Common C++ framework for threaded applications")
   (description "GNU Common C++ is an portable, optimized class framework for
threaded applications, supporting concurrent synchronization, inter-process
communications via sockets, and various methods for data handling, such as
serialization and XML parsing.  It includes the uCommon C++ library, a smaller
reimplementation.")
   (license license:gpl2+) ; plus runtime exception
   (properties '((ftp-directory . "/gnu/commoncpp")
                 (upstream-name . "commoncpp2")))
   (home-page "https://www.gnu.org/software/commoncpp/")))

(define-public ucommon
  (package
   (name "ucommon")
   (version "7.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/commoncpp/" name "-"
                   version ".tar.gz"))
            (sha256 (base32
                     "1mv080rvrhyxyhgqiqr8r9jdqhg3xhfawjvfj5zgj47h59nggjba"))))
   (build-system gnu-build-system)
   (arguments
    ;; Does not work with std=c++17, which is the default in modern GCC versions.
    `(#:configure-flags '("CXXFLAGS=-std=c++14")))
   (inputs (list gnutls))
   (synopsis "Common C++ framework for threaded applications")
   (description "GNU uCommon C++ is meant as a very light-weight C++ library
to facilitate using C++ design patterns even for very deeply embedded
applications, such as for systems using uclibc along with posix threading
support.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/commoncpp/")
   (properties '((ftp-directory . "/gnu/commoncpp")))))

(define-public ccrtp
  (package
   (name "ccrtp")
   (version "2.1.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/ccrtp/ccrtp-"
                   version ".tar.gz"))
            (sha256 (base32
                     "17ili8l7zqbbkzr1rcy4hlnazkf50mds41wg6n7bfdsx3c7cldgh"))))
   (build-system gnu-build-system)
   (inputs (list ucommon libgcrypt))
   (native-inputs (list pkg-config))
   (synopsis "Implementation of RTP (real-time transport protocol)")
   (description  "GNU ccRTP is an implementation of RTP, the real-time transport
protocol from the IETF.  It is suitable both for high capacity servers and
personal client applications.  It is flexible in its design, allowing it to
function as a framework for the framework, rather than just being a
packet-manipulation library.")
   (license license:gpl2+) ; plus runtime exception
   (home-page "https://www.gnu.org/software/ccrtp/")))

(define-public zrtpcpp
  (package
    (name "zrtpcpp")
    (version "4.6.6")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/wernerd/ZRTPCPP")
         (commit
          (string-append "V" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32
         "06vphvh4dgi7ah5qkq53wqvswv8l273x0xwbc447qmgvamm0x1vs"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; No target
    (native-inputs
     (list pkg-config))
    (inputs
     (list ccrtp ucommon))
    (synopsis "C++ Implementation of ZRTP protocol")
    (description  "GNU ZRTP C++ provides a library that adds ZRTP support to the
GNU ccRTP stack and serves as library for other RTP stacks
(PJSIP, GStreamer).")
    (home-page "https://www.gnu.org/software/ccrtp/zrtp")
    (license license:lgpl3+)))

(define-public osip
  (package
   (name "osip")
   (version "5.3.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/osip/libosip2-" version ".tar.gz"))
            (sha256
             (base32
              "0yfwd8g2nxf3i9d8gqh6a16ma350dlhih4awbb0nl9h82s2gx0py"))))
   (build-system gnu-build-system)

   (synopsis "Library implementing SIP (RFC-3261)")
   (description "GNU oSIP is an implementation of the SIP protocol.  It is
used to provide multimedia and telecom software developers with an interface
to initiate and control SIP sessions.")
   (license license:lgpl2.1+)
   (properties '((ftp-directory . "/gnu/osip")
                 (upstream-name . "libosip2")))
   (home-page "https://www.gnu.org/software/osip/")))

(define-public exosip
  (package
   (name "exosip")
   (version "5.3.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://savannah/exosip/libexosip2-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1hn9xgy8ny04hjqd1rra7c4nz5nf9abdd5ghr7nmrsiicjc26y2v"))))
   (build-system gnu-build-system)
   (inputs (list osip))
   (synopsis "Sip abstraction library")
   (description "EXosip is a library that hides the complexity of using the
SIP protocol for multimedia session establishment.  This protocol is mainly to
be used by VoIP telephony applications (endpoints or conference server) but
might be also useful for any application that wish to establish sessions like
multiplayer games.")
   (license license:gpl2+)
   ;; (plus OpenSSL linking exception)
   ;; http://git.savannah.gnu.org/cgit/exosip.git/plain/LICENSE.OpenSSL
    (home-page "https://savannah.nongnu.org/projects/exosip")))

(define-public sipwitch
  (package
   (name "sipwitch")
   (version "1.9.15")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/sipwitch/sipwitch-"
                   version ".tar.gz"))
            (sha256 (base32
                     "10lli9c703d7qbarzc0lgmz963ppncvnrklwrnri0s1zcmmahyia"))
            (patches
             (search-patches "sipwitch-fix-build-with-exosip5.patch"))))
   (build-system gnu-build-system)
   ;; The configure.ac uses pkg-config but in a kludgy way which breaks when
   ;; cross-compiling.  Among other issues there the program name "pkg-config"
   ;; is hard coded instead of respecting the PKG_CONFIG environment variable.
   ;; Fortunately we can avoid the use of pkg-config and set the dependency
   ;; flags ourselves.
   (arguments `(#:configure-flags
                `("--without-pkg-config"
                  ,(string-append "UCOMMON_CFLAGS=-I"
                                  (assoc-ref %build-inputs "ucommon") "/include")
                  "UCOMMON_LIBS=-lusecure -lucommon -lrt -ldl -lpthread"
                  ,(string-append "LIBOSIP2_CFLAGS=-I"
                                  (assoc-ref %build-inputs "osip") "/include")
                  "LIBOSIP2_LIBS=-losipparser2 -losip2"
                  ,(string-append "--sysconfdir=" (assoc-ref %outputs "out")
                                  "/etc")
                  "EXOSIP2_LIBS=-leXosip2"
                  ,(string-append "EXOSIP2_CFLAGS=-I"
                                  (assoc-ref %build-inputs "exosip")
                                  "/include"))))
   (inputs (list ucommon exosip osip))
   (synopsis "Secure peer-to-peer VoIP server for the SIP protocol")
   (description "GNU SIP Witch is a peer-to-peer Voice-over-IP server that
uses the SIP protocol.  Calls can be made from behind NAT firewalls and
without the need for a service provider.  Its peer-to-peer design ensures that
there is no central point for media intercept or capture and thus it can be
used to construct a secure telephone system that operates over the public
internet.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/sipwitch/")))

(define-public libsrtp
  (package
    (name "libsrtp")
    (version "2.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cisco/libsrtp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gswpjm4jacfxmgglbf8hxi3yzsag4drk4q943p0wkmv21zj8l78"))))
    (native-inputs
     (list psmisc ;some tests require 'killall'
           procps))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "runtest"
       #:phases (modify-phases %standard-phases
                  (add-after 'build 'build-shared
                    (lambda* (#:key (make-flags '()) #:allow-other-keys)
                      ;; Build the shared library separately because
                      ;; the test runner requires a static build.
                      (apply invoke "make" "shared_library" make-flags)
                      #t))
                  (add-after 'install 'remove-static-library
                    (lambda* (#:key outputs #:allow-other-keys)
                      (delete-file (string-append (assoc-ref outputs "out")
                                                  "/lib/libsrtp2.a"))
                      #t)))))
    (synopsis "Secure RTP (SRTP) Reference Implementation")
    (description
     "This package provides an implementation of the Secure Real-time Transport
Protocol (@dfn{SRTP}), the Universal Security Transform (@dfn{UST}), and a
supporting cryptographic kernel.")
    (home-page "https://github.com/cisco/libsrtp")
    (license license:bsd-3)))

(define-public libiax2
  (let ((commit "0e5980f1d78ce462e2d1ed6bc39ff35c8341f201"))
    ;; This is the commit used by the Ring Project.
    (package
      (name "libiax2")
      (version (string-append "0.0.0-1." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.savoirfairelinux.com/sflphone/libiax2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0d269474kk1933c55hx4azw3sak5ycfrxkw6ida0sb2cm00kfich"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake libtool))
      (home-page "https://gitlab.savoirfairelinux.com/sflphone/libiax2")
      (synopsis "Inter-Asterisk-Protocol library")
      (description "LibIAX2 implements the Inter-Asterisk-Protocol for relaying
Voice-over-IP (VoIP) communications.")
      ;; The file 'src/md5.c' is released into the public domain by RSA Data
      ;; Security.  The files 'src/answer.h', 'src/miniphone.c',
      ;; 'src/options.c', 'src/options.h', 'src/ring10.h', 'src/winiphone.c' are
      ;; covered under the 'GPL'.
      ;; The package as a whole is distributed under the LGPL 2.0.
      (license (list license:lgpl2.0
                     license:public-domain
                     license:gpl2+)))))

(define-public seren
  (package
    (name "seren")
    (version "0.0.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://holdenc.altervista.org/"
                                  "seren/downloads/seren-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "06mams6bng7ib7p2zpfq88kdr4ffril9svzc9lprkb0wjgmkglk9"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))  ; no "check" target
    (inputs
     (list alsa-lib gmp libogg ncurses opus))
    (synopsis "Simple VoIP program to create conferences from the terminal")
    (description
     "Seren is a simple VoIP program based on the Opus codec that allows you
to create a voice conference from the terminal, with up to 10 participants,
without having to register accounts, exchange emails, or add people to contact
lists.  All you need to join an existing conference is the host name or IP
address of one of the participants.")
    (home-page "http://holdenc.altervista.org/seren/")
    (license license:gpl3+)))

(define find-python-interpreter-cmake-modules
  (let ((commit "bb4d3ea8434eebef40df35434a9b6ef410fce0b2")
        (revision "0"))
   (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Krzmbrzl/FindPythonInterpreter")
          (commit commit)))
    (file-name (git-file-name "find-python-interpreter"
                              (git-version "0" revision commit)))
    (sha256
     (base32
      "1ryhda2yqgrhnwndfg52mscdsclg1ivv746hvalcay5m1wy2h5bm")))))

(define-public mumble
  (package
    (name "mumble")
    (version "1.5.634")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/mumble-voip/mumble/releases/download/v"
                version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0aar5if80w8ay9i03lpnznz6ln1gh1jjrzxfbj9fdc4as8rkckwh"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (srfi srfi-1)))
              (snippet
               `(begin
                  (let ((keep
                         '("arc4random"
                           "cmake-compiler-flags"
                           "flag-icons"
                           "minhook" ; unused, reqd for licenses
                           "qqbonjour"
                           "renamenoise"
                           "smallft"
                           "speexdsp" ; unbundled, reqd for licenses
                           "tracy" ; disabled below, reqd by cmake
                           "xinputcheck-src" ; reqd for licenses
                           )))
	            (with-directory-excursion "3rdparty"
	              (for-each delete-file-recursively
			        (lset-difference string=?
                                                 (scandir ".")
                                                 (cons* "." ".." keep))))
                    #t)))))
    (build-system qt-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Dbundled-gsl=off"
              "-Dbundled-json=off"
              "-Dbundled-speex=off"
              "-Dbundled-opus=off"
              "-Dalsa=off" ; use pulse
              "-Dcoreaudio=off" ; use pulse
              "-Dice=off" ; not packaged
              "-Djackaudio=off" ; use pulse
              "-Doss=off" ; use pulse
              "-Dpulseaudio=on"
              "-Dportaudio=off" ; use pulse
              "-Dpipewire=off" ; use pulse
              "-Doverlay-xcompile=off"
              "-Dupdate=off" ; don't phone home
              "-Dtests=on"
              "-Dtracy=off" ; no profiling
              "-Dbundle-qt-translations=off")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-submodules
            (lambda _
              (copy-recursively #$find-python-interpreter-cmake-modules
                                "3rdparty/FindPythonInterpreter")))
          (add-after 'unpack 'disable-murmur-ice
            (lambda _
              (substitute* "auxiliary_files/mumble-server.ini"
                (("^ice=") ";ice="))))
          ;; disable statistic gathering by default. see <https://bugs.gnu.org/25201>
          (add-after 'unpack 'fix-statistic-gathering-default
            (lambda _
              (substitute* "src/mumble/Settings.h"
                (("bUsage *= true;") "bUsage = false;"))))
          (add-after 'unpack 'fix-mumble-overlay
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (with-output-to-file "scripts/mumble-overlay"
                (lambda ()
                  (format #t "#!~a~%" (search-input-file inputs "/bin/bash"))
                  (format #t "export LD_PRELOAD=\"~a $LD_PRELOAD\"~%"
                          (string-append (assoc-ref outputs "out")
                                         "/lib/mumble/libmumbleoverlay.so"))
                  (format #t "exec \"${@}\"")))))
          (add-after 'unpack 'hardcode-pulseaudio
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/mumble/PulseAudio.cpp"
                (("libpulse.so") (search-input-file inputs "/lib/libpulse.so"))))))))
    (inputs
     (list avahi
           boost
           c++-gsl ; avoid bundled
           glib ; for speech-dispatcher
           libsndfile
           libxi
           mesa ; avoid bundled
           nlohmann-json ; avoid bundled
           openssl
           opus ; avoid bundled
           poco
           protobuf
           pulseaudio
           qtbase-5
           qtsvg-5
           speech-dispatcher
           speex ; avoid bundled
           speexdsp ; avoid bundled
           ))
    (native-inputs
     (list pkg-config python qttools-5))
    (synopsis "Low-latency, high quality voice chat software")
    (description
     "Mumble is an low-latency, high quality voice chat
software primarily intended for use while gaming.
Mumble consists of two applications for separate usage:
@code{mumble} for the client, and @code{murmur} for the server.")
    (home-page "https://wiki.mumble.info/wiki/Main_Page")
    (license (list license:bsd-3 ; mumble cmake-compiler-flags qqbonjour smallft
                   license:expat ; flag-icons
                   license:isc)))) ; arc4random

(define-public twinkle
  (package
    (name "twinkle")
    (version "1.10.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/LubosD/twinkle")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (patches
        (search-patches "twinkle-bcg729.patch")) ; To support new BCG729 API.
       (sha256
        (base32
         "0s0gi03xwvzp02ah4q6j33r9jx9nbayr6dxlg2ck9pwbay1nq1hx"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f                      ; no test target
       #:configure-flags
       (list
        ;; FIX-ME: Make Twinkle compatible with libre version of iLBC.
        ;; "-DWITH_ILBC=On"                ; For iLBC Codec Support
        "-DWITH_ZRTP=On"                ; For ZRTP Support
        "-DWITH_G729=On"                ; For G729 Codec Support
        "-DWITH_SPEEX=On")))            ; For Speex Codec Support
    (native-inputs
     (list bison flex qttools-5))
    (inputs
     (list alsa-lib
           bcg729
           zrtpcpp
           ccrtp
           file
           libilbc
           libsndfile
           libxml2
           qtbase-5
           qtdeclarative-5
           qtquickcontrols-5
           readline
           speex
           speexdsp
           ucommon))
    (synopsis "Softphone for voice over IP and instant messaging")
    (description "Twinkle is a softphone for your voice over IP and instant
messaging communcations using the SIP protocol.  You can use it for direct
IP phone to IP phone communication or in a network using a SIP proxy to route
your calls and messages.")
    (home-page "http://twinkle.dolezel.info/")
    (license license:gpl2+)))

(define-public pjproject
  (package
    (name "pjproject")
    (version "2.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pjsip/pjproject")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ld0adp9y2ydnz2ldwdzig3hpk4ayx1va6aqc3nja8zfdnd36fyb"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled libraries.
           (delete-file-recursively "third_party")
           (substitute* "aconfigure.ac"
             (("third_party/build/os-auto.mak") ""))
           (substitute* "Makefile"
             (("third_party/build") ""))))))
    (build-system gnu-build-system)
    (outputs '("out" "debug" "static"))
    (arguments
     (list
      #:test-target "selftest"
      #:configure-flags
      #~(list "--enable-shared"
              "--with-external-speex"
              "--with-external-gsm"
              "--with-external-srtp"
              "--with-external-pa"
              ;; The following flag is Linux specific.
              #$@(if (string-contains (or (%current-system)
                                          (%current-target-system)) "linux")
                     #~("--enable-epoll")
                     #~())
              "--with-gnutls"           ;disable OpenSSL checks
              "--disable-libyuv"        ;TODO: add missing package
              "--disable-silk"          ;TODO: add missing package
              "--disable-libwebrtc"     ;TODO: add missing package
              "--disable-ilbc-codec"    ;cannot be unbundled
              "--disable-g7221-codec"   ;TODO: add missing package
              "--enable-libsamplerate"
              ;; -DNDEBUG is set to prevent pjproject from raising
              ;; assertions that aren't critical, crashing
              ;; applications as the result.
              "CFLAGS=-DNDEBUG"
              ;; Specify a runpath reference to itself, which is missing and
              ;; causes the validate-runpath phase to fail.
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'build-dep
            (lambda _ (invoke "make" "dep")))
          ;; The check phases is moved after the install phase so to
          ;; use the installed shared libraries for the tests.
          (delete 'check)
          (add-after 'install 'move-static-libraries
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((s (string-append #$output:static "/lib")))
                (mkdir-p s)
                (with-directory-excursion #$output
                  (for-each (lambda (f)
                              (rename-file f (string-append s "/" (basename f))))
                            (find-files "." "\\.a$"))))))
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check))
          (add-before 'patch-source-shebangs 'autoconf
            (lambda _
              (invoke "autoconf" "-v" "-f" "-i" "-o"
                      "aconfigure" "aconfigure.ac")))
          (add-before 'autoconf 'disable-some-tests
            (lambda _
              (substitute* "pjlib/src/pjlib-test/test.h"
                ;; Disable network tests which are slow and/or require an
                ;; actual network.
                (("#define GROUP_NETWORK.*")
                 "#define GROUP_NETWORK 0\n"))
              (substitute* "self-test.mak"
                ;; Fails with: pjlib-util-test-x86_64-unknown-linux-gnu:
                ;; ../src/pjlib-util-test/resolver_test.c:1501: action2_1:
                ;; Assertio n `pj_strcmp2(&pkt->q[0].name, "_sip._udp."
                ;; "domain2.com")==0' failed.
                ((" pjlib_util_test ") ""))
              (substitute* "pjsip/src/test/test.h"
                ;; Fails with: Error: unable to acquire TCP transport:
                ;; [pj_status_t=120101] Network is unreachable.
                (("#define INCLUDE_TCP_TEST.*")
                 "#define INCLUDE_TCP_TEST 0\n")
                ;; The TSX tests takes a very long time to run; skip them.
                (("#define INCLUDE_TSX_GROUP.*")
                 "#define INCLUDE_TSX_GROUP 0\n")
                ;; The resolve test requires a working domain name resolver.
                (("#define INCLUDE_RESOLVE_TEST.*")
                 "#define INCLUDE_RESOLVE_TEST 0\n"))
              (substitute* "pjsip/src/test/dns_test.c"
                ;; The round_robin_test fails non-deterministically (depending
                ;; on load); skip it (see:
                ;; https://github.com/pjsip/pjproject/issues/2500).
                (("round_robin_test(pool)") 0))
              (substitute* "pjmedia/src/test/test.h"
                ;; The following tests require a sound card.
                (("#define HAS_MIPS_TEST.*")
                 "#define HAS_MIPS_TEST 0\n")
                (("#define HAS_JBUF_TEST.*")
                 "#define HAS_JBUF_TEST 0\n"))
              (substitute* "Makefile"
                ;; Disable the pjnath and pjsua tests, which require an actual
                ;; network and an actual sound card, respectively.
                (("pjnath-test pjmedia-test pjsip-test pjsua-test")
                 "pjmedia-test pjsip-test")))))))
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config))
    (inputs
     (list bcg729
           gnutls
           gsm
           libsamplerate
           libsrtp
           opus
           portaudio
           speex
           speexdsp))
    (home-page "https://www.pjsip.org")
    (synopsis "Session Initiation Protocol (SIP) stack")
    (description "PJProject provides an implementation of the Session
Initiation Protocol (SIP) and a multimedia framework.")
    (license license:gpl2+)))

(define-public pjproject-jami
  (let ((commit "8fc165b833eea6e3c88d67a541385424b129fd3f")
        (revision "3"))
    (package
      (inherit pjproject)
      (name "pjproject-jami")
      ;; The version is taken from
      ;; <https://raw.githubusercontent.com/savoirfairelinux/pjproject/master/version.mak>.
      (version (git-version "2.13.1" revision commit))
      (source (origin
                (inherit (package-source pjproject))
                ;; The Jami development team regularly issues patches to
                ;; pjproject to extend the its functionality and fix bugs;
                ;; they are submitted for inclusion upstream but larger
                ;; patches take time to be reviewed and merged, hence this
                ;; forked repository.
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/savoirfairelinux/pjproject")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "146gwpkhia9d7lqk3czlrwy0m3b8d9mhi2l05gffs0i0hljrj3mq"))))
      (arguments
       (substitute-keyword-arguments (package-arguments pjproject)
         ((#:configure-flags _ ''())
          ;; This package is tailored for DhtNet; see how it is built for its
          ;; CI in
          ;; <https://git.jami.net/savoirfairelinux/dhtnet/-/raw/master/Dockerfile>.
          #~(list
             ;; Some flags preserved flags from parent package.
             "--with-external-srtp"
             #$@(if (string-contains (or (%current-system)
                                         (%current-target-system)) "linux")
                    #~("--enable-epoll")
                    #~())
             "--with-gnutls"            ;disable OpenSSL checks
             ;; -DNDEBUG is set to prevent pjproject from raising
             ;; assertions that aren't critical, crashing
             ;; applications as the result.
             "CFLAGS=-DNDEBUG"
             ;; Specify a runpath reference to itself, which is missing and
             ;; causes the validate-runpath phase to fail.
             (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
             "--enable-shared"
             "--disable-libyuv"         ;TODO: add missing package

             ;; These flags are specific to DhtNet.
             "--disable-sound"
             "--enable-video"
             "--enable-ext-sound"
             "--disable-speex-aec"
             "--disable-g711-codec"
             "--disable-l16-codec"
             "--disable-gsm-codec"
             "--disable-g722-codec"
             "--disable-g7221-codec"
             "--disable-speex-codec"
             "--disable-ilbc-codec"
             "--disable-opencore-amr"
             "--disable-silk"
             "--disable-sdl"
             "--disable-ffmpeg"
             "--disable-v4l2"
             "--disable-openh264"
             "--disable-resample"
             "--disable-libwebrtc")))))))

(define-public libtgvoip
  (package
    (name "libtgvoip")
    (version "2.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grishka/libtgvoip")
             (commit version)))
       (file-name (git-file-name name version))
       ;; Fix compilation on i686-linux architecture.
       ;; NOTE: Applying these patches is order-dependent!
       ;; The patch for WebRTC /must/ precede the patch for SSE2.
       (patches
        (search-patches "libtgvoip-disable-webrtc.patch"
                        "libtgvoip-disable-sse2.patch"))
       (sha256
        (base32
         "122kn3jx6v0kkldlzlpzvlwqxgp6pmzxsjhrhcxw12bx9c08sar5"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib openssl opus pulseaudio))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; libtgvoip wants to dlopen libpulse and libasound, so tell it where
         ;; they are.
         (add-after 'unpack 'patch-dlopen
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "os/linux/AudioPulse.cpp"
               (("libpulse\\.so")
                (search-input-file inputs "/lib/libpulse.so")))
             (substitute* '("os/linux/AudioInputALSA.cpp"
                            "os/linux/AudioOutputALSA.cpp")
               (("libasound\\.so")
                (search-input-file inputs "/lib/libasound.so"))))))))
    (synopsis "VoIP library for Telegram clients")
    (description "A collection of libraries and header files for implementing
telephony functionality into custom Telegram clients.")
    (home-page "https://github.com/zevlg/libtgvoip")
    (license license:unlicense)))

(define-public coturn
  (package
    (name "coturn")
    (version "4.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coturn/coturn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16rr8666spi84qcc8l2qga42hpskjmvrpj1w58jbanxnpsijv8h4"))))
    (inputs
     (list openssl
           sqlite
           libevent-with-openssl
           hiredis))
    (native-inputs
     (list pkg-config))
    (build-system gnu-build-system)
    (synopsis "Implementation of a TURN and STUN server for VoIP")
    (description
     "This package provides a VoIP media traffic NAT traversal server and
gateway.  It implements the STUN (Session Traversal Utilities for NAT) and
TURN (Traversal Using Relays around NAT) server protocols.")
    (home-page "https://github.com/coturn/coturn")
    (license license:bsd-3)))

(define-public libosmocore
  (package
    (name "libosmocore")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitea.osmocom.org/osmocom/libosmocore.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "147ld3xwb9k6vb56hk8q8jkcb5ahxl66v87vdhazb6rxj3frsjqf"))))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-bin-sh
                          (lambda _
                            (substitute* '("git-version-gen" "src/exec.c")
                              (("/bin/sh")
                               (which "sh"))))))))
    (inputs (list gnutls
                  libmnl
                  libusb
                  lksctp-tools
                  pcsc-lite
                  talloc))
    (native-inputs (list autoconf
                         automake
                         coreutils
                         doxygen
                         libtool
                         pkg-config
                         python))
    (build-system gnu-build-system)
    (synopsis "Libraries for sharing common code between osmocom projects")
    (description
     "Libosmocore includes several libraries:
@itemize
@item libosmocore: general-purpose functions
@item libosmovty: interactive VTY command-line interface
@item libosmogsm: definitions and helper code related to GSM protocols
@item libosmoctrl: shared implementation of the Osmocom control interface
@item libosmogb: implementation of the Gb interface with its NS/BSSGP protocols
@item libosmocodec: implementation of GSM voice codecs
@item libosmocoding: implementation of GSM 05.03 burst transcoding functions
@item libosmosim: infrastructure to interface with SIM/UICC/USIM cards
@end itemize")
    (home-page "https://osmocom.org/projects/libosmocore/wiki/Libosmocore")
    (license license:gpl2+)))

(define-public xgoldmon
  ;; There are no releases nor tags.
  (let ((revision "1")
        (commit "f2d5372acee4e492f31f6ba8b850cfb48fbbe478"))
    (package
      (name "xgoldmon")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/2b-as/xgoldmon")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0dvgagqsbwq1sd5qjzk0hd9rxnv2vnmhazvv5mz4pj7v467amgdz"))))
      (arguments
       (list #:tests? #f ;no tests
             #:make-flags #~(list (string-append "CC="
                                                 #$(cc-for-target)))
             #:phases #~(modify-phases %standard-phases
                          (delete 'configure)
                          (replace 'install
                            (lambda _
                              (let ((bin (string-append #$output "/bin"))
                                    (doc (string-append #$output "/share/doc")))
                                (install-file "xgoldmon" bin)
                                (install-file "README" doc)
                                (install-file
                                 "screenshot-mtsms-while-in-a-call.png" doc)))))))
      (inputs (list libosmocore lksctp-tools talloc))
      (native-inputs (list pkg-config))
      (build-system gnu-build-system)
      (synopsis "Displays cellular network protocol traces in Wireshark")
      (description
       "xgoldmon is an utility that converts the USB logging mode
messages that various Intel/Infineon XGold modems send to the USB port to
gsmtap.  It then then sends them to a given IP address to enable users
to view cellular network protocol traces in Wireshark.

It supports the following smartphones:
@itemize
@item Samsung Galaxy S4, GT-I9500 variant
@item Samsung Galaxy SIII, GT-I9300 variant
@item Samsung Galaxy Nexus, GT-I9250 variant
@item Samsung Galaxy SII, GT-I9100 variant
@item Samsung Galaxy Note II, GT-N7100 variant
@end itemize")
      (home-page "https://github.com/2b-as/xgoldmon")
      (license license:gpl2+))))

(define-public sipp
  (package
    (name "sipp")
    (version "3.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SIPp/sipp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32 "0vplccia9zdva1wwny2xgs0b6rzmq4abxvw8lyz61wfw7jjmvin0"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DUSE_GSL=1" "-DUSE_PCAP=1" "-DUSE_SSL=1" "-DUSE_SCTP=1")
      #:phases
      #~(modify-phases %standard-phases
          ;; Modify build instructions to use external GTEST and GMOCK.
          (add-before 'configure 'unbundle-gtest
            (lambda _
              (rmdir "gtest")
              (symlink (assoc-ref %build-inputs "googletest") "gtest")
              (substitute* "CMakeLists.txt"
                ((".*gtest-all.*") "")
                ((".*gmock-all.*") "")
                (("target_compile_features\\(sipp_unittest" all)
                 (string-append "target_link_libraries(sipp_unittest gtest gmock)\n"
                                all)))))
          ;; Generate version.h without GIT.
          (add-before 'configure 'fix-version
            (lambda _
              (copy-file "include/version.h.in" "include/version.h")
              (substitute* "include/version.h" (("@VERSION@") #$version))
              (substitute* "CMakeLists.txt" (("find_package\\(Git\\)") ""))))
          (add-after 'build 'build-tests
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (invoke "make"
                      (string-append
                       "-j" (if parallel-build?
                                (number->string (parallel-job-count))
                                "1"))
                      "sipp_unittest")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./sipp_unittest")))))))
    (inputs
     (list gsl libpcap lksctp-tools ncurses/tinfo openssl))
    (native-inputs
     (list googletest pkg-config))
    (synopsis "Performance testing tool for the SIP protocol")
    (description "SIPp can be used to test many real SIP equipements like SIP
proxies, B2BUAs, SIP media servers, SIP/x gateways, and SIP PBXes.  It is also
very useful to emulate thousands of user agents calling your SIP system.")
    (home-page "https://sipp.readthedocs.io/")
    (license (list license:gpl2+        ; sipp's main license
                   license:bsd-3        ; send_packets.c, send_packets.h
                   license:zlib)))) ; md5.c, md5.h

(define-public sofia-sip
  (package
    (name "sofia-sip")
    (version "1.13.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/freeswitch/sofia-sip")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "sofia-sip" version))
              (sha256
               (base32
                "19m1ncvn641s5r9vfnivsz8g5960vcfmhhx0s119jfs49kcql2gd"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "CFLAGS=-g -O2 -Wno-error=incompatible-pointer-types")
      ;; run_addrinfo requires /etc/services for the 'echo' service.
      #:make-flags #~'("XFAIL_TESTS = run_addrinfo"
                       ;; libsofia-sip-ua/nta/Makefile.am sets
                       ;; TESTS_ENVIRONMENT = $(SHELL), which is odd, because
                       ;; according to the Automake manual, it should be
                       ;; AM_TESTS_ENVIRONMENT, and it should end with a
                       ;; semicolon.
                       "TESTS_ENVIRONMENT = \
export CHECK_NTA_VERBOSE=10; \
export CHECK_NUA_VERBOSE=10; ")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-shebangs
            (lambda _
              (patch-shebang "autogen.sh")))
          (add-after 'unpack 'disable-failing-test
            (lambda _
              ;; run_test_nta is disabled because it fails randomly (not for a
              ;; timeout-related reason).  The test suite is otherwise very
              ;; long, most tests backed by libcheck timeout even with a ×100
              ;; multiplier.  The tests are disabled here rather than put to
              ;; XFAIL_TESTS because it saves compilation time.  (see:
              ;; https://github.com/freeswitch/sofia-sip/issues/234)
              (substitute* "libsofia-sip-ua/nta/Makefile.am"
                (("TESTS =")
                 "TESTS = run_test_nta_api\n# Disabled: "))
              (substitute* "libsofia-sip-ua/nua/Makefile.am"
                (("TESTS \\+=")
                 "TESTS +=\n# Disabled: "))
              ;; The glib tests both wait forever without a timeout.
              (substitute* "libsofia-sip-ua-glib/su-glib/Makefile.am"
                (("TESTS =")
                 "TESTS =\n# Disabled: "))
              ;; Another timeout failing test:
              (substitute* "tests/Makefile.am"
                (("TESTS = test_nua")
                 "TESTS ="))
              ;; This test fails for unknown reason:
              (substitute* "tests/Makefile.am"
                (("TESTS \\+= check_dlopen_sofia check_sofia")
                 "TESTS += check_dlopen_sofia")))))))
    (inputs
     (list glib
           openssl
           zlib))
    (native-inputs
     (list autoconf
           autoconf-archive
           automake
           check
           libtool
           pkg-config))
    (home-page "https://sofia-sip.sourceforge.net/")
    (synopsis "SIP user-agent library")
    (description "Sofia-SIP is a @acronym{SIP, Session Initiation Protocol}
User-Agent library, compliant with the
@url{https://datatracker.ietf.org/doc/html/rfc3261, IETF RFC3261}
specification.  It can be used as a building block for @acronym{SIP} client
software foruses such as @acronym{VoIP, Voice over @acronym{IP, Internet
Protocol}}, @acronym{IM, Instant Messaging}, and many other real-time and
person-to-person communication services.")
    (license license:lgpl2.1)))

(define-public libcallaudio
  (package
    (name "libcallaudio")
    (version "0.1.9")
    (source (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/mobian1/callaudiod/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qnllb28101c2ss1k3iwr04gljfyjqmbla5csj6vkq1y63aagr9s"))))
    (build-system meson-build-system)
    (inputs (list alsa-lib glib pulseaudio))
    (native-inputs
     (list `(,glib "bin")          ;for gdbus-codegen
           pkg-config))
    (home-page "https://gitlab.com/mobian1/callaudiod")
    (synopsis "Library for audio routing during voice calls")
    (description "This package provides @command{callaudiod}, a daemon to
route audio during phone calls, and a library.")
    (license license:gpl3+)))

(define-public baresip-libre
  (package
     (name "baresip-libre")
     (version "3.24.0")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/baresip/re")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1k874v9bzipk5x9nr21f3259f5sk7nxnnz618kji0mx9aa0fvjf1"))))
     (build-system cmake-build-system)
     (native-inputs (list pkg-config))
     (inputs (list openssl zlib))
     (synopsis "Library for real-time communications with async IO support")
     (description "Libre is a portable and generic library for real-time
communications with async @acronym{IO, Input Output} support and a complete
@acronym{SIP, Session Initiation Protocol} stack with support for protocols such
as @acronym{SDP, Session Description Protocol}, @acronym{RTP, Real-time
Transport Protocol}/@acronym{RTCP, RTP Control Protocol}, @acronym{STUN, Session
Traversal Utilities for NAT}/@acronym{TURN, Traversal Using Relays around
NAT}/@acronym{ICE, Interactive Connectivity Establishment}, @acronym{BFCP,
Binary Floor Control Protocol}, @acronym{HTTP, Hypertext Transfer Protocol} and
@acronym{DNS, Domain Name System}.")
     (home-page "https://github.com/baresip/re")
     (license license:bsd-3)))

(define-public baresip
  (package
    (name "baresip")
    (version "3.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/baresip/baresip")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xwvhpvrs6anw8mq709ff9d6vm0mizf6sj1sz69y85s7p4qz4rfz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'neuter-module_path
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Never generate a config file with a set module_path.
                      (substitute* "src/config.c"
                        (("modpath_valid \\? \"\"")
                         "modpath_valid ? \"#\""))
                      ;; If no module_path config is defined, search modules in
                      ;; this package.
                      (substitute* "src/module.c"
                        (("\t\tpl_set_str\\(&path, \"\\.\");")
                         (string-append "pl_set_str(&path,\""
                                        (assoc-ref outputs "out")
                                        "/lib/baresip/modules\");")))))
                  (add-after 'unpack 'patch-paths
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (substitute* "cmake/FindWEBRTC_AEC.cmake"
                        (("/usr/include")
                         (string-append (assoc-ref inputs
                                                   "webrtc-audio-processing")
                                        "/include"))
                        (("/usr/lib/x86_64-linux-gnu")
                         (string-append (assoc-ref inputs
                                                   "webrtc-audio-processing")
                                        "/lib")))
                      (substitute* "share/com.github.baresip.desktop"
                        (("env")
                         (string-append (assoc-ref inputs "coreutils-minimal")
                                        "/bin/env"))
                        (("baresip")
                         (string-append (assoc-ref outputs "out")
                                        "/bin/baresip"))))))))
    (native-inputs (list gnuplot pkg-config python ))
    (inputs (list alsa-lib
                  baresip-libre
                  coreutils-minimal          ;for env in .desktop file
                  ffmpeg
                  gsm
                  gstreamer
                  gtk+
                  libsndfile
                  libtiff
                  libvpx
                  openssl
                  opus
                  pipewire
                  portaudio
                  pulseaudio
                  sdl2
                  spandsp
                  webrtc-audio-processing-0.3))
    (home-page "https://github.com/baresip/baresip")
    (synopsis "SIP user agent with audio and video support")
    (description
     "Baresip is a portable and modular @acronym{SIP, Session Initiation Protocol}
user agent with support for audio and video, and many
@acronym{IETF, Internet Engineering Task Force} standards such as
@acronym{SIP, Session Initiation Protocol}, @acronym{SDP, Session Description Protocol},
@acronym{RTP, Real-time Transport Protocol}/@acronym{RTCP, RTP Control Protocol},
@acronym{STUN, Session Traversal Utilities for NAT},
@acronym{TURN, Traversal Using Relays around NAT},
@acronym{ICE, Interactive Connectivity Establishment},
and @acronym{WebRTC, Web Real-Time Communication}.")
    (license license:bsd-3)))
