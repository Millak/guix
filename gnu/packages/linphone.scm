;;; GNU Guix --- Functional package management for GNU
;;;
;;; Copyright © 2020, 2021 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages linphone)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt))

(define-public bcunit
  (let ((commit "74021cc7cb20a4e177748dd2948173e1f9c270ae")
        (revision "0"))
    (package
      (name "bcunit")
      (version (git-version "3.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.linphone.org/bcunit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0npdwvanjkfg9vrqs5yi8vh6wliv50ycdli8pzavir84nb31nq1b"))))
      (build-system cmake-build-system)
      (outputs '("out" "doc"))
      (arguments
       `(#:configure-flags (list "-DENABLE_STATIC=NO"
                                 "-DENABLE_CURSES=ON"
                                 "-DENABLE_DOC=ON"
                                 "-DENABLE_EXAMPLES=ON"
                                 "-DENABLE_TEST=ON"
                                 "-DENABLE_MEMTRACE=ON")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda _
               ;; Include BCunit headers for examples.
               (substitute* "Examples/CMakeLists.txt"
                 (("\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}")
                  (string-append "${CMAKE_CURRENT_SOURCE_DIR} "
                                 "${PROJECT_SOURCE_DIR}/BCUnit/Headers "
                                 "${CMAKE_BINARY_DIR}/BCUnit/Headers")))
               ;; Link bcunit and bcunit_tests libraries.
               (substitute* "BCUnit/Sources/CMakeLists.txt"
                 (("target_include_directories\\(bcunit_test PUBLIC Test\\)")
                  (string-append
                   "target_include_directories(bcunit_test PUBLIC Test)\n"
                   "target_link_libraries(bcunit_test bcunit)")))))
           (replace 'check
             (lambda _
               (with-directory-excursion "BCUnit/Sources/Test"
                 (invoke "./test_bcunit"))))
           (add-after 'install 'move-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (doc (assoc-ref outputs "doc")))
                 (for-each mkdir-p
                           `(,(string-append doc "/share/doc")
                             ,(string-append doc "/share/BCUnit")))
                 (rename-file
                  (string-append out "/share/doc/BCUnit")
                  (string-append doc "/share/doc/BCUnit"))
                 (rename-file
                  (string-append out "/share/BCUnit/Examples")
                  (string-append doc "/share/BCUnit/Examples"))))))))
      (inputs
       (list ncurses))
      (synopsis "Belledonne Communications Unit Testing Framework")
      (description "BCUnit is a fork of the defunct project CUnit, with
several fixes and patches applied.  It is a unit testing framework for
writing, administering, and running unit tests in C.")
      (home-page "https://gitlab.linphone.org/BC/public/bcunit")
      (license license:lgpl2.0+))))

(define-public bctoolbox
  (package
    (name "bctoolbox")
    (version "5.2.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/bctoolbox.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b51308jy5z32gp594r78jvbyrha16sanxdnbcmxgrwnb4myqx5j"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags (list "-DENABLE_STATIC=OFF"
                               ;; Do not use -Werror, because due to skipping
                               ;; a test there are unused procedures.
                               "-DENABLE_STRICT=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-cmake
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Fix decaf dependency (see:
             ;; https://gitlab.linphone.org/BC/public/bctoolbox/-/issues/3).
             (let* ((decaf (assoc-ref inputs "libdecaf")))
               (substitute* (find-files "." "CMakeLists.txt")
                 (("find_package\\(Decaf CONFIG\\)")
                  "set(DECAF_FOUND 1)")
                 (("\\$\\{DECAF_INCLUDE_DIRS\\}")
                  (string-append decaf "/include/decaf"))
                 (("\\$\\{DECAF_TARGETNAME\\}")
                  "decaf")))))
         (add-after 'unpack 'skip-problematic-tests
           (lambda _
             ;; The following test relies on networking; disable it.
             (substitute* "tester/port.c"
               (("[ \t]*TEST_NO_TAG.*bctbx_addrinfo_sort_test\\),")
                ""))))
         (add-after 'unpack 'fix-installed-resource-directory-detection
           (lambda _
             ;; There's some broken logic in tester.c that checks if CWD, or
             ;; if its parent exist, and if so, sets the prefix where the test
             ;; resources are looked up to; disable it (see:
             ;; https://gitlab.linphone.org/BC/public/bctoolbox/-/issues/4).
             (substitute* "src/tester.c"
               (("if \\(file_exists\\(\".\"\\)\\)")
                "if (NULL)")
               (("if \\(file_exists\\(\"..\"\\)\\)")
                "if (NULL)"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "tester"
                 (invoke "./bctoolbox_tester"))))))))
    (inputs
     (list bcunit libdecaf mbedtls-lts))
    (synopsis "Belledonne Communications Tool Box")
    (description "BcToolBox is an utilities library used by Belledonne
Communications software like belle-sip, mediastreamer2 and linphone.")
    (home-page "https://gitlab.linphone.org/BC/public/bctoolbox")
    (license license:gpl3+)))

(define-public belr
  (package
    (name "belr")
    (version "5.2.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/belr.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bj8qd4ahbff476z0ccwsxy7qznqi6n5l1pdd7zbvk0h53zyj74c"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug" "tester"))
    (arguments
     (list
      #:configure-flags '(list "-DENABLE_STATIC=OFF")
       #:phases
       #~(modify-phases %standard-phases
           (delete 'check)              ;moved after the install phase
           (add-after 'install 'check
             (lambda* (#:key tests? outputs #:allow-other-keys)
               (when tests?
                 (invoke (string-append #$output:tester "/bin/belr_tester")))))
           (add-after 'install 'move-tester
             (lambda _
               (for-each mkdir-p
                         (list (string-append #$output:tester "/bin")
                               (string-append #$output:tester "/share")))
               (rename-file
                (string-append #$output "/bin/belr_tester")
                (string-append #$output:tester "/bin/belr_tester"))
               (rename-file
                (string-append #$output "/share/belr-tester/res")
                ;; The detect_res_prefix procedure in bctoolbox's tester.c
                ;; resolves the resource path based on the executable path and
                ;; name, so have it match.
                (string-append #$output:tester "/share/belr_tester")))))))
    (inputs
     (list bctoolbox))
    (synopsis "Belledonne Communications Language Recognition Library")
    (description "Belr is Belledonne Communications' language recognition
library, written in C++11.  It parses text inputs formatted according to a
language defined by an ABNF grammar, such as the protocols standardized at
IETF.")
    (home-page "https://gitlab.linphone.org/BC/public/belr")
    (license license:gpl3+)))

(define-public belcard
  (package
    (name "belcard")
    (version "5.2.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/belcard.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rl1x7rnlnncb45sjp8r2xbcwr9l8qv5bhfybhr0mmvsv3a4k4a3"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug" "tester"))
    (arguments
     (list
      #:configure-flags '(list "-DENABLE_STATIC=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-vcard-grammar-location
            (lambda _
              (let ((vcard-grammar
                     (string-append #$output
                                    "/share/belr/grammars/vcard_grammar")))
                (substitute* "include/belcard/vcard_grammar.hpp"
                  (("define VCARD_GRAMMAR \"vcard_grammar\"")
                   (format #f "define VCARD_GRAMMAR ~s" vcard-grammar))))))
          (add-after 'install 'install-tester
            (lambda _
              (let ((test-name (string-append #$name "_tester")))
                (for-each mkdir-p
                          (list (string-append #$output:tester "/bin")
                                (string-append #$output:tester "/share")))
                (rename-file (string-append #$output "/bin/" test-name)
                             (string-append #$output:tester "/bin/" test-name))
                (rename-file (string-append #$output "/share/" test-name)
                             (string-append #$output:tester "/share/" test-name)))))
          (delete 'check)
          (add-after 'install-tester 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke (string-append #$output:tester
                                       "/bin/belcard_tester"))))))))
    (inputs
     (list bctoolbox belr))
    (synopsis "Belledonne Communications VCard Library")
    (description "Belcard is a C++ library to manipulate VCard standard
format.")
    (home-page "https://gitlab.linphone.org/BC/public/belcard")
    (license license:gpl3+)))

(define-public bcmatroska2
  (package
    (name "bcmatroska2")
    (version "5.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/bcmatroska2.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14c79znw37q3yc7llbv2wmxmm4a3ws6iq3cvgkbmcnf7hmhm7zdi"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                                     ;No test target
      #:phases
      '(modify-phases %standard-phases
         ;; See
         ;; https://gitlab.linphone.org/BC/public/bcmatroska2/-/merge_requests/18
         (add-after 'unpack 'fix-build-system
           (lambda _
             (substitute* "corec/corec/CMakeLists.txt"
               (("helpers/file/file_libc.c") "")))))
      #:configure-flags
      '(list "-DENABLE_STATIC=NO"))) ;Not required
    (inputs (list bctoolbox))
    (synopsis "Belledonne Communications Media Container")
    (description "BcMatroska is a free and open standard multi-media container
format.  It can hold an unlimited number of video, audio, picture, or subtitle
tracks in one file.  This project provides a convenient distribution of the
Matroska multimedia container format.")
    (home-page "https://gitlab.linphone.org/BC/public/bcmatroska2")
    (license (list license:gpl2+        ;for this package (build system files)
                   license:bsd-4        ;for Core C and LibEBML2
                   license:lgpl2.1+)))) ;for LibMatroska2

(define-public bcg729
  (package
    (name "bcg729")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.linphone.org/bcg729")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hal6b3w6f8y5r1wa0xzj8sj2jjndypaxyw62q50p63garp2h739"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DENABLE_STATIC=NO"
                               "-DENABLE_TESTS=YES")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'copy-inputs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((test-patterns (assoc-ref inputs "test-patterns"))
                   (dest (string-append "test/bcg729-patterns.zip")))
               (copy-recursively test-patterns dest))))
         (replace 'check
           (lambda _
             (with-directory-excursion "test"
               (invoke "unzip" "bcg729-patterns.zip")
               (for-each
                (lambda (test-name)
                  (invoke "./testCampaign" "-s" test-name))
                (list "fixedCodebookSearch"
                      "postProcessing"
                      "adaptativeCodebookSearch"
                      "computeLP"
                      "computeAdaptativeCodebookGain"
                      "postFilter"
                      "decoder"
                      "LPSynthesisFilter"
                      "decodeLSP"
                      ;; "encoder"
                      ;; "LSPQuantization"
                      "preProcessing"
                      "decodeFixedCodeVector"
                      "CNGdecoder"
                      ;; "LP2LSPConversion"
                      "gainQuantization"
                      "findOpenLoopPitchDelay"
                      "decodeGains"
                      "computeWeightedSpeech"
                      "interpolateqLSPAndConvert2LP"
                      "decodeAdaptativeCodeVector"))))))))
    (native-inputs
     `(("perl" ,perl)
       ("test-patterns"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://www.belledonne-communications.com/"
                               "bc-downloads/bcg729-patterns.zip"))
           (sha256
            (base32 "1kivarhh3izrl9sg0szs6x6pbq2ap0y6xsraw0gbgspi4gnfihrh"))))
       ("unzip" ,unzip)))
    (synopsis "Belledonne Communications G729 Codec")
    (description "BcG729 is an implementation of both encoder and decoder of
the ITU G729 speech codec.  The library written in C 99 is fully portable and
can be executed on many platforms including both ARM and x86 processors.  It
supports concurrent channels encoding and decoding for multi call application
such as conferencing.")
    (home-page "https://linphone.org/technical-corner/bcg729")
    (license license:gpl3+)))

(define-public ortp
  (package
    (name "ortp")
    (version "5.2.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/ortp.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hzbrj1ny3lr9sql0lrxggc48sqv5j2yvbpnrdnph88pwzrdnbn5"))))
    (build-system cmake-build-system)
    (outputs '("out" "tester"
               "doc"))                  ;1.5 MiB of HTML doc
    (arguments
     (list
      #:tests? #f                       ;requires networking
      #:configure-flags '(list "-DENABLE_STATIC=NO"
                               "-DENABLE_DOC=NO" ;XXX: missing link for b64
                               "-DENABLE_TESTS=YES")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-version-strings
            ;; See: https://gitlab.linphone.org/BC/public/ortp/-/issues/5.
            (lambda _
              (substitute* "CMakeLists.txt"
                (("VERSION [0-9]+\\.[0-9]+\\.[0-9]+")
                 (string-append "VERSION " #$version))
                (("\\$\\{ORTP_DOC_VERSION\\}")
                 #$version))))
          (add-after 'install 'separate-outputs
            (lambda _
              (let* ((doc-src
                      (string-append #$output "/share/doc/ortp-" #$version))
                     (doc-dest
                      (string-append #$output:doc "/share/doc/ortp-" #$version)))
                (for-each mkdir-p (list (string-append #$output:doc "/share/doc")
                                        (string-append #$output:tester "/bin")))
                (rename-file doc-src doc-dest)
                (rename-file (string-append #$output "/bin")
                             (string-append #$output:tester "/bin"))))))))
    (native-inputs
     (list graphviz doxygen))
    (inputs
     (list bctoolbox))
    (synopsis "Belledonne Communications RTP Library")
    (description "oRTP is a C library implementing the RTP protocol.  It
implements the RFC 3550 standard.")
    (home-page "https://linphone.org/technical-corner/ortp")
    (license license:gpl3+)))

(define-public bzrtp
  (package
    (name "bzrtp")
    (version "5.2.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/bzrtp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dvn1w0g9c07llz9n82l6qdzz8lzz74jcdm1yyfks0jy7i63cr8w"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list
        "-DENABLE_STATIC=NO"
        "-DENABLE_TESTS=YES")))
    (inputs
     (list bctoolbox libxml2 sqlite))
    (synopsis "Belledonne Communications ZRTP Library")
    (description "BZRTP is an implementation of ZRTP keys exchange protocol,
written in C.  It is fully portable and can be executed on many platforms
including both ARM and x86.")
    (home-page "https://gitlab.linphone.org/BC/public/bzrtp")
    (license license:gpl3+)))

(define-public belle-sip
  (package
    (name "belle-sip")
    (version "5.2.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/belle-sip.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yx1qvzp11ysh24hxrvz7dm69j8zswa0xcx9m42vcv95z72166cq"))))
    (build-system cmake-build-system)
    (outputs '("out" "tester"))
    (arguments
     (list
      #:configure-flags '(list "-DENABLE_STATIC=NO"
                               "-DENABLE_MDNS=ON"
                               ;; We skip a test and thus have an unused
                               ;; procedure, so we need to disable -Werror.
                               "-DENABLE_STRICT=OFF")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'patch
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Fix mDNS dependency.
               (let* ((avahi (assoc-ref inputs "avahi")))
                 (substitute* (find-files "." "CMakeLists.txt")
                   (("find_package\\(DNSSD REQUIRED\\)")
                    "set(DNSSD_FOUND 1)")
                   (("\\$\\{DNSSD_INCLUDE_DIRS\\}")
                    (string-append avahi "/include/avahi-compat-libdns_sd"))
                   (("\\$\\{DNSSD_LIBRARIES\\}")
                    "dns_sd")))
               ;; Disable broken test.  This test uses
               ;; bctbx_unescaped_string_only_chars_in_rules from bctoolbox,
               ;; which unescapes too much.
               (substitute* "tester/belle_sip_base_uri_tester.c"
                 (("[ \t]*TEST_NO_TAG.*test_unescaping_good_chars\\),")
                  ""))
               (substitute* "src/sdp/parser.cc"
                 (("load\\(\"sdp_grammar\"\\)")
                  (string-append "load(\"" #$output
                                 "/share/belr/grammars/sdp_grammar\")")))
               (substitute* "src/CMakeLists.txt"
                 ;; ANTLR would use multithreaded DFA generation otherwise,
                 ;; which would not be reproducible.
                 (("-Xmultithreaded ") ""))))
           (delete 'check)              ;move after install
           (add-after 'install 'separate-outputs
             (lambda _
               (let ((tester-name "belle_sip_tester"))
                 (for-each mkdir-p (list (string-append #$output:tester "/bin")
                                         (string-append #$output:tester "/share")))
                 (rename-file (string-append #$output "/bin")
                              (string-append #$output:tester "/bin"))
                 (rename-file (string-append #$output "/share/" tester-name)
                              (string-append #$output:tester "/share/" tester-name)))))
           (add-after 'separate-outputs 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (let ((tester (string-append #$output:tester
                                              "/bin/belle_sip_tester")))
                   (for-each (lambda (suite-name)
                               (invoke tester "--suite" suite-name))
                             (list "Object inheritance"
                                   "SIP URI"
                                   "FAST SIP URI"
                                   "FAST SIP URI 2"
                                   "Generic uri"
                                   "Headers"
                                   "Core"
                                   "SDP"
                                   ;;"Resolver"
                                   "Message"
                                   "Authentication helper"
                                   ;;"Register"
                                   ;;"Dialog"
                                   "Refresher"
                                   ;;"HTTP stack"
                                   "Object")))))))))
    (inputs
     (list avahi bctoolbox belr zlib))
    (synopsis "Belledonne Communications SIP Library")
    (description "Belle-sip is a modern library implementing SIP transport,
transaction and dialog layers.  It is written in C, with an object-oriented
API.  It also comprises a simple HTTP/HTTPS client implementation.")
    (home-page "https://linphone.org/technical-corner/belle-sip")
    (license license:gpl3+)))

(define-public mediastreamer2
  (package
    (name "mediastreamer2")
    (version "5.2.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/mediastreamer2.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mj0q2xaac22p2wf5gvgaiga03fbydilxfxzwyc6nwp5fyjnzawd"))))
    (outputs '("out" "doc" "tester"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags '(list "-DENABLE_STATIC=NO"
                               "-DENABLE_PCAP=YES"
                               ;; Do not fail on compile warnings.
                               "-DENABLE_STRICT=NO"
                               "-DENABLE_PORTAUDIO=YES"
                               "-DENABLE_G729B_CNG=YES")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-version
            (lambda _
              (substitute* "CMakeLists.txt"
                (("VERSION [0-9]+\\.[0-9]+\\.[0-9]+")
                 (string-append "VERSION " #$version)))))
          (add-after 'unpack 'patch-source
            (lambda _
              (substitute* "src/otherfilters/mspcapfileplayer.c"
                (("O_BINARY") "L_INCR"))))
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a running X server.
              (system "Xvfb :1 +extension GLX &")
              (setenv "DISPLAY" ":1")
              ;; Tests write to $HOME.
              (setenv "HOME" (getenv "TEMP"))))
          (delete 'check)               ;move after install
          (add-after 'install 'separate-outputs
            (lambda _
              (let ((tester-name (string-append #$name "_tester"))
                    (doc-name (string-append #$name "-" #$version)))
                (for-each mkdir-p
                          (list (string-append #$output:tester "/bin")
                                (string-append #$output:tester "/share")
                                (string-append #$output:doc "/share/doc")))
                ;; Move the tester executable.
                (rename-file (string-append #$output "/bin/" tester-name)
                             (string-append #$output:tester "/bin/" tester-name))
                ;; Move the tester data files.
                (rename-file (string-append #$output "/share/" tester-name)
                             (string-append #$output:tester "/share/" tester-name))
                ;; Move the HTML documentation.
                (rename-file (string-append #$output "/share/doc/" doc-name)
                             (string-append #$output:doc "/share/doc/" doc-name)))))
          (add-after 'separate-outputs 'check
            (lambda _
              (let ((tester (string-append #$output:tester
                                           "/bin/mediastreamer2_tester")))
                (for-each (lambda (suite-name)
                            (invoke tester "--suite" suite-name))
                          ;; Some tests fail, due to requiring access to the
                          ;; sound card or the network.
                          (list "Basic Audio"
                                ;; "Sound Card"
                                ;; "AdaptiveAlgorithm"
                                ;; "AudioStream"
                                ;; "VideoStream"
                                "H26x Tools"
                                "Framework"
                                ;; "Player"
                                "TextStream"))))))))
    (native-inputs
     (list graphviz doxygen python-wrapper xorg-server-for-tests))
    (inputs
     (list alsa-lib
           bcg729
           bcmatroska2
           bctoolbox
           ffmpeg-4
           glew
           glu
           mesa-utils
           gsm
           mesa
           opus
           ortp
           libpcap
           portaudio
           pulseaudio
           spandsp
           speex
           speexdsp
           libsrtp
           libtheora
           libjpeg-turbo
           v4l-utils
           libvpx
           libx11
           libxv
           bzrtp))
    (synopsis "Belledonne Communications Streaming Engine")
    (description "Mediastreamer2 is a powerful and lightweight streaming engine
for telephony applications.  This media processing and streaming toolkit is
responsible for receiving and sending all multimedia streams in Linphone,
including media capture, encoding and decoding, and rendering.")
    (home-page "https://linphone.org/technical-corner/mediastreamer2")
    (license license:gpl3+)))

(define-public lime
  (package
    (name "lime")
    (version "5.2.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/lime.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mglnypxl3glwvwf2h5q4ikbm6wbcd9pb7kdws8zajjhk9q803jr"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags (list "-DENABLE_STATIC=NO"
                               "-DENABLE_C_INTERFACE=YES"
                               "-DENABLE_DOC=YES")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; Disable tests that require networking.
             (substitute* "tester/CMakeLists.txt"
               (("add_test\\(?.*\"Hello World\"\\)") "")
               (("add_test\\(?.*\"lime\"\\)") "")
               (("add_test\\(?.*\"FFI\"\\)") "")
               (("add_test\\(?.*\"Multidomains\"\\)") "")
               (("add_test\\(?.*\"Lime server\"\\)") ""))))
         (add-after 'build 'build-doc
           (lambda _
             (invoke "make" "doc")))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((doc (assoc-ref outputs "doc"))
                    (dir (string-append doc "/share/doc"))
                    (dest (string-append dir "/" ,name "-" ,version)))
               (mkdir-p dest)
               (copy-recursively "doc" dest)))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)))
    (inputs
     (list bctoolbox belle-sip soci))
    (synopsis "Belledonne Communications Encryption Library")
    (description "LIME is an encryption library for one-to-one and group
instant messaging, allowing users to exchange messages privately and
asynchronously.  It supports multiple devices per user and multiple users per
device.")
    (home-page "https://linphone.org/technical-corner/lime")
    (license license:gpl3+)))

(define-public liblinphone
  (package
    (name "liblinphone")
    (version "5.2.50")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/liblinphone.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lvbva234rmck57cxgswgqqvnq8r58i0ls4qgpymrxdfj74rinxj"))))
    (outputs '("out" "tester"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; Tests require networking
      #:configure-flags
      '(list "-DENABLE_FLEXIAPI=NO"  ;requires jsoncpp, but it cannot be found
             "-DENABLE_STATIC=NO"
             "-DENABLE_DOC=NO"       ;requires unpackaged javasphinx
             "-DENABLE_LDAP=YES"
             "-DENABLE_STRICT=NO")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'separate-outputs
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((tester-name (string-append #$name "_tester")))
                (for-each mkdir-p
                          (list (string-append #$output:tester "/bin")
                                (string-append #$output:tester "/share")))
                (rename-file (string-append #$output "/bin/" tester-name)
                             (string-append #$output:tester "/bin/" tester-name))
                (rename-file (string-append #$output "/bin/groupchat_benchmark")
                             (string-append #$output:tester "/bin/groupchat_benchmark"))
                (rename-file (string-append #$output "/share/" tester-name)
                             (string-append #$output:tester "/share/" tester-name))))))))
    (native-inputs
     (list graphviz
           doxygen
           gettext-minimal
           perl
           python-wrapper
           python-pystache
           python-six
           eudev))
    (inputs
     (list bctoolbox
           belcard
           belle-sip
           belr
           bzrtp
           lime
           libnotify
           libxml2
           mediastreamer2
           openldap-for-linphone
           ortp
           soci
           sqlite
           xsd
           zlib
           zxing-cpp))
    (synopsis "Belledonne Communications Softphone Library")
    (description "Liblinphone is a high-level SIP library integrating
all calling and instant messaging features into an unified
easy-to-use API.  It is the cross-platform VoIP library on which the
Linphone application is based on, and that anyone can use to add audio
and video calls or instant messaging capabilities to an application.")
    (home-page "https://linphone.org/technical-corner/liblinphone")
    (license license:gpl3+)))

(define-public linphone-desktop
  (package
    (name "linphone-desktop")
    (version "5.0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.linphone.org/BC/public/linphone-desktop")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0glrfsp087ni5hn6x6p4f6y63r4nyp061yyy0rfgddbxkzdqi2j1"))
       (patches (search-patches "linphone-desktop-without-sdk.patch"))))
    (build-system qt-build-system)
    (outputs '("out" "debug"))
    (arguments
     (list
      #:tests? #f                       ; No test target
      #:configure-flags
      #~(list (string-append "-DFULL_VERSION=" #$version)
              (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
              (string-append "-DCMAKE_INSTALL_BINDIR=" #$output "/bin")
              (string-append "-DCMAKE_INSTALL_DATAROOTDIR=" #$output "/share")
              (string-append "-DCMAKE_INSTALL_LIBDIR=" #$output "/lib")
              "-DENABLE_UPDATE_CHECK=NO"
              "-DENABLE_DAEMON=YES"
              "-DENABLE_CONSOLE_UI=YES")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'pre-configure
            (lambda _
              (make-file-writable "linphone-app/linphoneqt_version.cmake")
              (substitute* "linphone-app/linphoneqt_version.cmake"
                (("\\$\\{GUIX-SET-VERSION\\}") #$version))))
          (add-before 'install 'pre-install
            (lambda _
              (mkdir-p (string-append #$output "/share/linphone"))
              (symlink (string-append #$(this-package-input "liblinphone")
                                      "/share/sounds")
                       (string-append #$output
                                      "/share/sounds"))))
          (add-after 'install 'post-install
            (lambda _
              (let* ((liblinphone #$(this-package-input "liblinphone"))
                     (grammar-dest (string-append #$output "/share/belr/grammars")))
                ;; Remove unnecessary Qt configuration file.
                (delete-file (string-append #$output "/bin/qt.conf"))
                ;; Not using the FHS exposes an issue where the client
                ;; refers to its own directories, which lacks files
                ;; installed by the dependencies.
                (for-each
                 (lambda (file)
                   (symlink file
                            (string-append #$output "/lib/" (basename file))))
                 (find-files (string-append liblinphone "/lib")))
                (symlink (string-append liblinphone "/share/linphone/rootca.pem")
                         (string-append #$output "/share/linphone/rootca.pem"))
                (mkdir-p (dirname grammar-dest))
                (symlink (string-append liblinphone "/share/belr/grammars")
                         grammar-dest)))))))
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (list bctoolbox
           belcard
           belr
           liblinphone
           mediastreamer2
           ortp
           qtbase-5
           qtdeclarative-5
           qtgraphicaleffects
           qtquickcontrols-5
           qtquickcontrols2-5
           qtsvg-5
           qtwayland-5))
    (synopsis "Desktop client for the Linphone SIP softphone")
    (description "Linphone is a SIP softphone for voice and video over IP calling
(VoIP) and instant messaging.  Amongst its features are:
@itemize
@item High Definition (HD) audio and video calls
@item Multiple call management (pause and resume)
@item Call transfer
@item Audio conferencing (merge calls into a conference call)
@item Call recording and replay (audio only)
@item Instant Messaging with message delivery status (IMDN)
@item Picture and file sharing
@item Echo cancellation
@item Secure user authentication using TLS client certificates
@item SRTP, zRTP and SRTP-DTLS voice and video encryption
@item Telephone tone (DTMF) support using SIP INFO or RFC 4733
@item Audio codecs: opus, speex, g711, g729, gsm, iLBC, g722, SILK, etc.
@item Video codecs: VP8, H.264 and H.265 with resolutions up to 1080P, MPEG4
@end itemize")
    (home-page "https://linphone.org/technical-corner/linphone")
    (license license:gpl3+)))

(define-public msopenh264
  (let ((commit "88697cc95140017760d6da408cb0efdc5e86e40a")
        (revision "0"))
    (package
      (name "msopenh264")
      (version (git-version "1.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.linphone.org/BC/public/msopenh264.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "10y3b6s934f2wbsf60b3p0g6hffizjqrj5in8l4sida2fjdxlwwy"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; No test target
         #:configure-flags
         (list "-DENABLE_STATIC=NO")))  ; Not required
      (inputs
       (list bctoolbox mediastreamer2 openh264 ortp))
      (synopsis "Media Streamer H.264 Codec")
      (description "MsOpenH264 is an  H.264 encoder/decoder plugin for
 mediastreamer2 based on the openh264 library.")
      (home-page "https://gitlab.linphone.org/BC/public/msopenh264")
      (license license:gpl2+))))

(define-public mssilk
  (let ((commit "dd0f31ee795faa7ea89e601b072dae4cd1df7e3f")
        (revision "0"))
    (package
      (name "mssilk")
      (version (git-version "1.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.linphone.org/BC/public/mssilk.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dann5fnzqp6wjlwc6bl2k9b6rvn6bznqb3qsi1kgv9dnq44cbr0"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; No test target
         #:configure-flags
         (list "-DENABLE_STATIC=NO")))  ; Not required
      (inputs
       (list bctoolbox mediastreamer2 ortp))
      (synopsis "Media Streamer SILK Codec")
      (description "MSSILK is a plugin of MediaStreamer, adding support for AMR
codec.  It is based on the Skype's SILK implementation.")
      (home-page "https://gitlab.linphone.org/BC/public/mssilk")
      (license license:gpl2+))))

(define-public mswebrtc
  (let ((commit "946ca706733f36a6b4923f04e569531125462d1d")
        (revision "0"))
    (package
      (name "mswebrtc")
      (version (git-version "1.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.linphone.org/BC/public/mswebrtc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1pfg9m6bpbv0f53nx72rdxhlyriax9pg4yj0gpwq8ha6lqnpwg1x"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; No test target
         #:configure-flags
         (list
          "-DENABLE_STATIC=NO")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'copy-inputs
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((webrtc-from (assoc-ref inputs "webrtc"))
                      (webrtc-to (string-append (getcwd) "/webrtc")))
                 (copy-recursively webrtc-from webrtc-to))
               #t)))))
      (native-inputs
       `(("webrtc"
          ,(origin
             (method git-fetch)
             (uri
              (git-reference
               (url "https://gitlab.linphone.org/BC/public/external/webrtc")
               (commit "583acd27665cfadef8ab03eb85a768d308bd29dd")))
             (file-name
              (git-file-name "webrtc-for-mswebrtc" version))
             (sha256
              (base32
               "1maqychrgwy0z4zypa03qp726l2finw64z6cymdzhd58ql3p1lvm"))))
         ("python" ,python-wrapper)))
      (inputs
       (list bctoolbox mediastreamer2 ortp))
      (synopsis "Media Streamer WebRTC Codec")
      (description "MSWebRTC is a plugin of MediaStreamer, adding support for
WebRTC codec.  It includes features from WebRTC, such as, iSAC and AECM.")
      (home-page "https://gitlab.linphone.org/BC/public/mswebrtc")
      (license license:gpl2+))))

(define-public msamr
  (let ((commit "5ab5c098299107048dfcbfc741f7392faef167bd")
        (revision "0"))
    (package
      (name "msamr")
      (version (git-version "1.1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.linphone.org/BC/public/msamr")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1g79lw1qi1mlw3v1b0cixmqiwjql81gz9naakb15n8pvaag9aaqm"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; No test target
         #:configure-flags
         (list "-DENABLE_STATIC=NO"     ; Not required
               "-DENABLE_WIDEBAND=YES")))
      (inputs
       `(("bctoolbox" ,bctoolbox)
         ("mediastreamer2" ,mediastreamer2)
         ("opencoreamr" ,opencore-amr)
         ("ortp" ,ortp)
         ("voamrwbenc" ,vo-amrwbenc)))
      (synopsis "Media Streamer AMR Codec")
      (description "MSAMR is a plugin of MediaStreamer, adding support for AMR
codec.  It is based on the opencore-amr implementation.")
      (home-page "https://gitlab.linphone.org/BC/public/msamr")
      (license license:gpl3+))))
