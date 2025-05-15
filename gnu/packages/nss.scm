;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2019, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016-2019, 2021-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (gnu packages nss)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system mozilla)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages time))

(define-public nspr
  (package
    (name "nspr")
    (version "4.35")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                    version "/src/nspr-" version ".tar.gz"))
              (sha256
               (base32
                "13xwda56yhp1w7v02qvlxvlqiniw8kr4g3fxlljmv6wnlmz2k8vy"))))
    (build-system gnu-build-system)
    (inputs
     (list perl                         ;for 'compile-et.pl'
           bash-minimal))               ;for 'nspr-config'
    (native-inputs
     (list perl))
    (arguments
     (list
      ;; Prevent the 'native' perl from sneaking into the closure.
      ;; XXX it would be nice to do the same for 'bash-minimal',
      ;; but using 'canonical-package' causes loops.
      #:disallowed-references
      (if (%current-target-system)
          (list (gexp-input (this-package-native-input "perl") #:native? #t))
          #f)
      #:tests? #f                       ;no check target
      #:configure-flags
      #~(list "--disable-static"
              "--enable-64bit"
              (string-append "LDFLAGS=-Wl,-rpath="
                             (assoc-ref %outputs "out") "/lib")
              #$@(if (%current-target-system)
                     #~("HOST_CC=gcc")
                     #~()))
      ;; Use fixed timestamps for reproducibility.
      #:make-flags #~'("SH_DATE='1970-01-01 00:00:01'"
                       ;; This is epoch 1 in microseconds.
                       "SH_NOW=100000")
      #:phases #~(modify-phases %standard-phases
                   (add-before 'configure 'chdir
                     (lambda _ (chdir "nspr") #t)))))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSPR")
    (synopsis "Netscape API for system level and libc-like functions")
    (description "Netscape Portable Runtime (@dfn{NSPR}) provides a
platform-neutral API for system level and libc-like functions.  It is used
in the Mozilla clients.")
    (license license:mpl2.0)))

(define-public nspr-4.36
  (package
    (inherit nspr)
    (version "4.36")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                    version "/src/nspr-" version ".tar.gz"))
              (sha256
               (base32
                "15b83ipjxrmw0909l5qqz13pbarhp50d6i58vgjx4720y4bw7pjm"))))))

(define-public nspr-4.32
  (package
    (inherit nspr)
    (version "4.32")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "0v3zds1id71j5a5si42a658fjz8nv2f6zp6w4gqrqmdr6ksz8sxv"))))))

;; nss should track ESRs, but currently doesn't.  3.102.1 is the current ESR.

(define-public nss
  (package
    (name "nss")
    ;; IMPORTANT: Also update and test the nss-certs package, which duplicates
    ;; version and source to avoid a top-level variable reference & module
    ;; cycle.
    (version "3.99")
    (source (origin
              (method url-fetch)
              (uri (let ((version-with-underscores
                          (string-join (string-split version #\.) "_")))
                     (string-append
                      "https://ftp.mozilla.org/pub/mozilla.org/security/nss/"
                      "releases/NSS_" version-with-underscores "_RTM/src/"
                      "nss-" version ".tar.gz")))
              (sha256
               (base32
                "1g89ig40gfi1sp02gybvl2z818lawcnrqjzsws36cdva834c5maw"))
              ;; Create nss.pc and nss-config.
              (patches (search-patches "nss-3.56-pkgconfig.patch"
                                       "nss-getcwd-nonnull.patch"
                                       "nss-increase-test-timeout.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete the bundled copy of these libraries.
                  (delete-file-recursively "nss/lib/zlib")
                  (delete-file-recursively "nss/lib/sqlite")))))
    (build-system gnu-build-system)
    (outputs '("out" "bin"))
    (arguments
     (list
      #:make-flags
      #~(let ((rpath (string-append "-Wl,-rpath=" #$output "/lib/nss")))
          (list "-C" "nss"
                (string-append "PREFIX=" #$output)
                "NSDISTMODE=copy"
                "NSS_USE_SYSTEM_SQLITE=1"
                ;; The gtests fail to compile on riscv64.
                ;; Skipping them doesn't affect the test suite.
                #$@(if (target-riscv64?)
                       #~("NSS_DISABLE_GTESTS=1")
                       #~())
                ;; Ensure we are building for the (%current-target-system).
                #$@(if (%current-target-system)
                       #~((string-append
                            "OS_TEST="
                            (string-take #$(%current-target-system)
                                         (string-index #$(%current-target-system) #\-)))
                          (string-append
                            "KERNEL=" (cond (#$(target-hurd?) "gnu")
                                            (#$(target-linux?) "linux")
                                            (else ""))))
                       #~())
                #$@(if (%current-target-system)
                       #~("CROSS_COMPILE=1")
                       #~())
                (string-append "NSPR_INCLUDE_DIR="
                               (search-input-directory %build-inputs
                                                       "include/nspr"))
                ;; Add $out/lib/nss to RPATH.
                (string-append "RPATH=" rpath)
                (string-append "LDFLAGS=" rpath)))
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-26))
      #:tests? (not (or (%current-target-system)
                        ;; Tests take more than 30 hours on some architectures.
                        (target-riscv64?)
                        (target-ppc32?)))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "CCC" #$(cxx-for-target))
              (setenv "NATIVE_CC" "gcc")
              ;; No VSX on powerpc-linux.
              #$@(if (target-ppc32?)
                     #~((setenv "NSS_DISABLE_CRYPTO_VSX" "1"))
                     #~())
              ;; Tells NSS to build for the 64-bit ABI if we are 64-bit system.
              #$@(if (target-64bit?)
                     #~((setenv "USE_64" "1"))
                     #~())))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (begin
                    ;; Use 127.0.0.1 instead of $HOST.$DOMSUF as HOSTADDR for
                    ;; testing.  The latter requires a working DNS or /etc/hosts.
                    (setenv "DOMSUF" "localdomain")
                    (setenv "USE_IP" "TRUE")
                    (setenv "IP_ADDRESS" "127.0.0.1")

                    ;; This specific test is looking at performance "now
                    ;; verify that we can quickly dump a database", and
                    ;; we're not testing performance here (especially
                    ;; since we're using faketime), so raise the
                    ;; threshold
                    (substitute* "nss/tests/dbtests/dbtests.sh"
                      ((" -lt 5") " -lt 50"))

                    #$@(if (target-64bit?)
                           '()
                           ;; The script fails to determine the source
                           ;; directory when running under 'datefudge' (see
                           ;; <https://issues.guix.gnu.org/72239>).  Help it.
                           #~((substitute* "nss/tests/gtests/gtests.sh"
                                (("SOURCE_DIR=.*")
                                 (string-append "SOURCE_DIR=" (getcwd) "/nss\n")))))

                    ;; The "PayPalEE.cert" certificate expires every six months,
                    ;; leading to test failures:
                    ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=609734>.  To
                    ;; work around that, set the time to roughly the release date.
                    (invoke #$(if (target-64bit?) "faketime" "datefudge")
                            "2024-01-23" "./nss/tests/all.sh"))
                  (format #t "test suite not run~%"))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append (assoc-ref outputs "bin") "/bin"))
                     (inc (string-append out "/include/nss"))
                     (lib (string-append out "/lib/nss"))
                     (obj (match (scandir "dist" (cut string-suffix? "OBJ" <>))
                            ((obj) (string-append "dist/" obj)))))
                ;; Install nss-config to $out/bin.
                (install-file (string-append obj "/bin/nss-config")
                              (string-append out "/bin"))
                (delete-file (string-append obj "/bin/nss-config"))
                ;; Install nss.pc to $out/lib/pkgconfig.
                (install-file (string-append obj "/lib/pkgconfig/nss.pc")
                              (string-append out "/lib/pkgconfig"))
                (delete-file (string-append obj "/lib/pkgconfig/nss.pc"))
                (rmdir (string-append obj "/lib/pkgconfig"))
                ;; Install other files.
                (copy-recursively "dist/public/nss" inc)
                (copy-recursively (string-append obj "/bin") bin)
                (copy-recursively (string-append obj "/lib") lib)))))))
    (inputs (list sqlite zlib))
    (propagated-inputs (list nspr))               ;required by nss.pc.
    (native-inputs (list perl                     ;for tests
                         (if (target-64bit?) libfaketime datefudge)
                         which))

    ;; The NSS test suite takes around 48 hours on Loongson 3A (MIPS) when
    ;; another build is happening concurrently on the same machine.
    (properties '((timeout . 216000)))  ;60 hours

    (home-page "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS")
    (synopsis "Network Security Services")
    (description
     "Network Security Services (@dfn{NSS}) is a set of libraries designed to
support cross-platform development of security-enabled client and server
applications.  Applications built with NSS can support SSL v2 and v3, TLS,
PKCS #5, PKCS #7, PKCS #11, PKCS #12, S/MIME, X.509 v3 certificates, and other
security standards.")
    (license license:mpl2.0)))

(define-public nss/fixed
  (let ((actual-version "3.99"))
    (hidden-package
     (package
       (inherit nss)
       (version (string-append actual-version ".0")) ;for grafts requirements
       (source (origin
                 (inherit (package-source nss))
                 (uri (let ((version-with-underscores
                             (string-join (string-split actual-version #\.) "_")))
                        (string-append
                         "https://ftp.mozilla.org/pub/mozilla.org/security/nss/"
                         "releases/NSS_" version-with-underscores "_RTM/src/"
                         "nss-" actual-version ".tar.gz")))
                 (sha256
                  (base32
                   "1g89ig40gfi1sp02gybvl2z818lawcnrqjzsws36cdva834c5maw"))))
       (arguments
        (substitute-keyword-arguments (package-arguments nss)
          ((#:phases phases)
           #~(modify-phases #$phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (if tests?
                       (begin
                         ;; Use 127.0.0.1 instead of $HOST.$DOMSUF as HOSTADDR for
                         ;; testing.  The latter requires a working DNS or /etc/hosts.
                         (setenv "DOMSUF" "localdomain")
                         (setenv "USE_IP" "TRUE")
                         (setenv "IP_ADDRESS" "127.0.0.1")

                         ;; This specific test is looking at performance "now
                         ;; verify that we can quickly dump a database", and
                         ;; we're not testing performance here (especially
                         ;; since we're using faketime), so raise the
                         ;; threshold
                         (substitute* "nss/tests/dbtests/dbtests.sh"
                           ((" -lt 5") " -lt 50"))

                         ;; Since the test suite is very lengthy, run the test
                         ;; suite once, not thrice as done by default, by
                         ;; selecting only the 'standard' cycle.
                         (setenv "NSS_CYCLES" "standard")

                         ;; The "PayPalEE.cert" certificate expires every six months,
                         ;; leading to test failures:
                         ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=609734>.  To
                         ;; work around that, set the time to roughly the release date.
                         (invoke "faketime" "2024-01-23" "./nss/tests/all.sh"))
                       (format #t "test suite not run~%"))))))))))))

;; nss-rapid tracks the rapid release channel.  Unless your package requires a
;; newer version, you should prefer the `nss' package, which tracks the ESR
;; channel.
;;
;; See https://wiki.mozilla.org/NSS:Release_Versions
;; and https://wiki.mozilla.org/Rapid_Release_Model

(define-public nss-rapid
  (package
   (inherit nss)
   (name "nss-rapid")
   (version "3.109")
   (source (origin
             (inherit (package-source nss))
             (uri (let ((version-with-underscores
                         (string-join (string-split version #\.) "_")))
                    (string-append
                     "https://ftp.mozilla.org/pub/mozilla.org/security/nss/"
                     "releases/NSS_" version-with-underscores "_RTM/src/"
                     "nss-" version ".tar.gz")))
             (sha256
              (base32
               "12y156frnhaqvwkla1c07gqr2lnp4yb3619g4088kk8qc4jnr95y"))))
   (arguments
    (substitute-keyword-arguments (package-arguments nss)
      ((#:phases phases)
       #~(modify-phases #$phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (if tests?
                   (begin
                     ;; Use 127.0.0.1 instead of $HOST.$DOMSUF as HOSTADDR for
                     ;; testing.  The latter requires a working DNS or /etc/hosts.
                     (setenv "DOMSUF" "localdomain")
                     (setenv "USE_IP" "TRUE")
                     (setenv "IP_ADDRESS" "127.0.0.1")

                     ;; This specific test is looking at performance "now
                     ;; verify that we can quickly dump a database", and
                     ;; we're not testing performance here (especially
                     ;; since we're using faketime), so raise the
                     ;; threshold
                     (substitute* "nss/tests/dbtests/dbtests.sh"
                       ((" -lt 5") " -lt 50"))

                     ;; Since the test suite is very lengthy, run the test
                     ;; suite once, not thrice as done by default, by
                     ;; selecting only the 'standard' cycle.
                     (setenv "NSS_CYCLES" "standard")

                     ;; The "PayPalEE.cert" certificate expires every six months,
                     ;; leading to test failures:
                     ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=609734>.  To
                     ;; work around that, set the time to roughly the release date.
                     (invoke "faketime" "2025-03-01" "./nss/tests/all.sh"))
                   (format #t "test suite not run~%"))))))))
   (synopsis "Network Security Services (Rapid Release)")
   (description
    "Network Security Services (@dfn{NSS}) is a set of libraries designed to
support cross-platform development of security-enabled client and server
applications.  Applications built with NSS can support SSL v2 and v3, TLS,
PKCS #5, PKCS #7, PKCS #11, PKCS #12, S/MIME, X.509 v3 certificates, and other
security standards.

This package tracks the Rapid Release channel, which updates frequently.")))

(define-public nsncd
  (package
    (name "nsncd")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twosigma/nsncd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qiphwlwbnni2vfqjbdzv2a1qgqv2ycmygmyrbx8pgaak9gl5hfi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--"
         ;; These tests fail with the current builder network setup
         "--skip=ffi::test_gethostbyaddr_r"
         "--skip=ffi::test_gethostbyname2_r"
         "--skip=handlers::test::test_handle_getservbyname_name"
         "--skip=handlers::test::test_handle_getservbyname_name_proto"
         "--skip=handlers::test::test_handle_getservbyport_port"
         "--skip=handlers::test::test_handle_getservbyport_port_proto"
         "--skip=handlers::test::test_handle_getservbyport_port_proto_aliases")
      #:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-atoi" ,rust-atoi-2)
        ("rust-slog" ,rust-slog-2)
        ("rust-slog-async" ,rust-slog-async-2)
        ("rust-slog-term" ,rust-slog-term-2)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-nix" ,rust-nix-0.28)
        ("rust-num-derive" ,rust-num-derive-0.3)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-sd-notify" ,rust-sd-notify-0.4)
        ("rust-static-assertions" ,rust-static-assertions-1)
        ("rust-dns-lookup" ,rust-dns-lookup-2))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-temp-env" ,rust-temp-env-0.3))))
    (home-page "https://github.com/twosigma/nsncd")
    (synopsis "The name service non-caching daemon")
    (description
     "This package provides @command{nscd}, a daemon compatible that proxies
lookups, compatible with the GNU C Library's @command{nscd}, but without
caching.  It can be used in situations where you want to make an application
use @acronym{NSS, Name Service Switch} plugins available to a different libc
than the one the application will load.")
    (license (list license:asl2.0))))
