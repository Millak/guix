;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020, 2021, 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Baptiste Strazzul <bstrazzull@hotmail.fr>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (gnu packages gnuzilla)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libreoffice)  ;for hunspell
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite))

(define-public mozjs
  (package
    (name "mozjs")
    (version "102.2.0")
    (source (origin
              (method url-fetch)
              ;; TODO: Switch to IceCat source once available on ftp.gnu.org.
              (uri (string-append "https://ftp.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "1zwpgis7py1bf8p88pz3mpai6a02qrdb8ww2fa9kxxdl9b8r2k81"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules %cargo-utils-modules ;for `generate-all-checksums'
      #:modules `((guix build cargo-utils)
                  ,@%gnu-build-system-modules)
      #:test-target "check-jstests"
      #:configure-flags
      #~(list
         ;; Disable debugging symbols to save space.
         "--disable-debug"
         "--disable-debug-symbols"
         ;; This is important because without it gjs will segfault during the
         ;; configure phase.  With jemalloc only the standalone mozjs console
         ;; will work.
         "--disable-jemalloc"
         "--enable-tests"
         "--enable-hardening"
         "--enable-optimize"
         "--enable-release"
         "--enable-readline"
         "--enable-shared-js"
         "--with-system-icu"
         "--with-system-nspr"
         "--with-system-zlib"
         "--with-intl-api")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (let ((null-hash
                     "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
                (for-each (lambda (file)
                            (format #t "patching checksums in ~a~%" file)
                            (substitute* file
                              (("^checksum = \".*\"")
                               (string-append "checksum = \"" null-hash "\""))))
                          (find-files "." "Cargo\\.lock$"))
                (for-each generate-all-checksums
                          '("js" "third_party/rust")))))
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              ;; The configure script does not accept environment variables as
              ;; arguments.  It also must be run from a different directory,
              ;; but not the root directory either.
              (mkdir "run-configure-from-here")
              (chdir "run-configure-from-here")
              (setenv "SHELL" (which "sh"))
              (setenv "CONFIG_SHELL" (which "sh"))
              (setenv "AUTOCONF" (which "autoconf"))
              (apply invoke "python" "../configure.py"
                     "--enable-project=js"
                     (string-append "--prefix=" #$output)
                     configure-flags)))
          (add-before 'check 'adjust-tests
            (lambda _
              (with-directory-excursion "../js/src/tests"
                (substitute* "shell/os.js"
                  ;; FIXME: Why does the killed process have an exit status?
                  ((".*killed process should not have exitStatus.*")
                   ""))

                ;; The test suite expects a lightly patched ICU.  Disable tests
                ;; that do not work with the system version.  See
                ;; "intl/icu-patches" for clues.

                ;; See <https://unicode-org.atlassian.net/browse/ICU-20992> and
                ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=1636984> and
                ;; related patch for why this is failing.
                (delete-file "non262/Intl/DateTimeFormat/\
fractional-second-digits-append-item.js")
                ;; FIXME: got "0 \u251CAM/PM: noon\u2524", expected "0 (AM/PM: noon)"
                (delete-file "non262/Intl/DateTimeFormat/day-period-hour-cycle.js")
                ;; FIXME: got "en-US-posix", expected "en-US-POSIX".
                (delete-file "non262/Intl/available-locales-supported.js")
                ;; FIXME: got "en-US", expected "en-US-POSIX"
                (delete-file "non262/Intl/available-locales-resolved.js"))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "JSTESTS_EXTRA_ARGS"
                      (string-join
                       (list
                        ;; Do not run tests marked as "random".
                        "--exclude-random"
                        ;; Exclude web platform tests.
                        "--wpt=disabled"
                        ;; Respect the daemons configured number of jobs.
                        (string-append "--worker-count="
                                       (number->string (parallel-job-count)))))))))))
    (native-inputs
     (list autoconf
           llvm                         ;for llvm-objdump
           m4
           perl
           pkg-config
           python-wrapper
           rust
           `(,rust "cargo")))
    (inputs
     (list icu4c-71 readline zlib))
    (propagated-inputs
     (list nspr))                ; in the Requires.private field of mozjs-*.pc
    (home-page
     "https://spidermonkey.dev/")
    (synopsis "Mozilla JavaScript engine")
    (description "SpiderMonkey is Mozilla's JavaScript engine written
in C/C++.")
    (license license:mpl2.0))) ; and others for some files

(define-public mozjs-91
  (package
    (inherit mozjs)
    (version "91.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "0qh7j960wdp5zcfqhkj8ki47spp9i9ms12xx0v0kxvmmw36jpgjk"))))
    (arguments
     (substitute-keyword-arguments (package-arguments mozjs)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'check 'disable-timezone-tests
              (lambda _
                (with-directory-excursion "../js/src/tests"
                  ;; FIXME: Assertion failed: got "2021a", expected "2021a3"?
                  (delete-file "non262/Intl/DateTimeFormat/timeZone_version.js")
                  ;; XXX: Delete all tests that test time zone functionality,
                  ;; because the test suite uses /etc/localtime to figure out
                  ;; the offset from the hardware clock, which does not work
                  ;; in the build container.  See <tests/non262/Date/shell.js>.
                  (delete-file-recursively "non262/Date")
                  (delete-file
                   "non262/Intl/DateTimeFormat/tz-environment-variable.js"))))))))
    (inputs (modify-inputs (package-inputs mozjs)
              (replace "icu4c" icu4c)))))

(define-public mozjs-78
  (package
    (inherit mozjs)
    (name "mozjs")
    (version "78.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "0l91cxdc5v9fps79ckb1kid4gw6v5qng1jd9zvaacwaiv628shx4"))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments mozjs)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'configure
              (lambda* (#:key configure-flags #:allow-other-keys)
                ;; The configure script does not accept environment variables as
                ;; arguments.  It also must be run from a different directory,
                ;; but not the root directory either.
                (mkdir "run-configure-from-here")
                (chdir "run-configure-from-here")
                (setenv "SHELL" (which "sh"))
                (setenv "CONFIG_SHELL" (which "sh"))
                (setenv "AUTOCONF" (which "autoconf"))
                (apply invoke "../js/src/configure"
                       (cons (string-append "--prefix=" #$output)
                             configure-flags))))
            (replace 'adjust-tests
              (lambda _
                (with-directory-excursion "../js/src/tests"
                  ;; The test suite expects a lightly patched ICU 67.  Since
                  ;; Guix is about to switch to ICU 68, massage the tests to
                  ;; work with that instead of patching ICU.  Try removing this
                  ;; phase for newer versions of mozjs.

                  ;; These tests look up locale names and expects to get
                  ;; "GB" instead of "UK".
                  (substitute* "non262/Intl/DisplayNames/language.js"
                    (("Traditionell, GB")
                     "Traditionell, UK"))
                  (substitute* "non262/Intl/DisplayNames/region.js"
                    (("\"GB\": \"GB\"")
                     "\"GB\": \"UK\""))

                  ;; XXX: Some localized time formats have changed, and
                  ;; substitution fails for accented characters, even though
                  ;; it works in the REPL(?).  Just delete these for now.
                  (delete-file "non262/Intl/Date/toLocaleString_timeZone.js")
                  (delete-file "non262/Intl/Date/toLocaleDateString_timeZone.js")

                  ;; Similarly, these get an unexpected "A" suffix when looking
                  ;; up a time in the "ar-MA-u-ca-islamicc" locale, which is
                  ;; tricky to substitute.
                  (delete-file "non262/Intl/DateTimeFormat/format_timeZone.js")
                  (delete-file "non262/Intl/DateTimeFormat/format.js")

                  ;; This file compares a generated list of ICU locale names
                  ;; with actual lookups.  Some have changed slightly, i.e.
                  ;; daf-Latn-ZZ -> daf-Latn-CI, so drop it for simplicity.
                  (delete-file "non262/Intl/Locale/likely-subtags-generated.js"))))
            (replace 'pre-check
              (lambda _
                (with-directory-excursion "../js/src/tests"
                  (substitute* "shell/os.js"
                    ;; FIXME: Why does the killed process have an exit status?
                    ((".*killed process should not have exitStatus.*")
                     ""))

                  ;; XXX: Delete all tests that test time zone functionality,
                  ;; because the test suite uses /etc/localtime to figure out
                  ;; the offset from the hardware clock, which does not work
                  ;; in the build container.  See <tests/non262/Date/shell.js>.
                  (delete-file-recursively "non262/Date")
                  (delete-file "non262/Intl/DateTimeFormat/tz-environment-variable.js")

                  (setenv "JSTESTS_EXTRA_ARGS"
                          (string-join
                           (list
                            ;; Do not run tests marked as "random".
                            "--exclude-random"
                            ;; Exclude web platform tests.
                            "--wpt=disabled"
                            ;; Respect the daemons configured number of jobs.
                            (string-append "--worker-count="
                                           (number->string
                                            (parallel-job-count)))))))))))))
    (native-inputs
     (list autoconf-2.13
           automake
           llvm                         ;for llvm-objdump
           perl
           pkg-config
           python-3
           rust
           `(,rust "cargo")))
    (inputs
     (list icu4c readline zlib))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporary packaging of rust-cbindgen-0.23 and its dependencies
;; follow, pending their inclusion into (gnu packages rust-apps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rust-textwrap-0.15-promise
  (delay
    (package
     (inherit rust-textwrap-0.12)
     (name "rust-textwrap")
     (version "0.15.0")
     (source (origin
              (method url-fetch)
              (uri (crate-uri "textwrap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yw513k61lfiwgqrfvsjw1a5wpvm0azhpjr2kr0jhnq9c56is55i"))))
     (arguments
      `(#:skip-build? #t
        #:cargo-inputs (("rust-hyphenation" ,rust-hyphenation-0.8)
                        ("rust-smawk" ,rust-smawk-0.3)
                        ("rust-terminal-size" ,rust-terminal-size-0.1)
                        ("rust-unicode-linebreak" ,rust-unicode-linebreak-0.1)
                        ("rust-unicode-width" ,rust-unicode-width-0.1)))))))

(define rust-clap-lex-0.2
  (package
    (name "rust-clap-lex")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_lex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ib1a9v55ybnaws11l63az0jgz5xiy24jkdgsmyl7grcm3sz4l18"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-os-str-bytes" ,rust-os-str-bytes-6))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_lex")
    (synopsis "Minimal, flexible command line parser")
    (description "Minimal, flexible command line parser")
    (license (list license:expat license:asl2.0))))

(define rust-clap-derive-3.2.15-promise
  (delay
    (package
     (inherit rust-clap-derive-3)
     (name "rust-clap-derive")
     (version "3.2.15")
     (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1d2c4vs345fwihkd8cc7m6acbiydcwramkd5mnp36p0a7g6jm9cv"))))
     (arguments
      `(#:skip-build? #t
        #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                        ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                        ("rust-proc-macro2" ,rust-proc-macro2-1)
                        ("rust-quote" ,rust-quote-1)
                        ("rust-syn" ,rust-syn-1)))))))

(define rust-clap-3.2.16-promise
  (delay
    (package
     (inherit rust-clap-3)
     (name "rust-clap")
     (version "3.2.16")
     (source (origin
              (method url-fetch)
              (uri (crate-uri "clap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1af06z8z7m3327yz1xvzxfjanclgpvvy3lssb745rig7adkbpnx3"))))
     (arguments
      `(#:skip-build? #t
        #:cargo-inputs (("rust-atty" ,rust-atty-0.2)
                        ("rust-backtrace" ,rust-backtrace-0.3)
                        ("rust-bitflags" ,rust-bitflags-1)
                        ("rust-clap-derive" ,(force rust-clap-derive-3.2.15-promise))
                        ("rust-clap-lex" ,rust-clap-lex-0.2)
                        ("rust-indexmap" ,rust-indexmap-1)
                        ("rust-once-cell" ,rust-once-cell-1)
                        ("rust-regex" ,rust-regex-1)
                        ("rust-strsim" ,rust-strsim-0.10)
                        ("rust-termcolor" ,rust-termcolor-1)
                        ("rust-terminal-size" ,rust-terminal-size-0.1)
                        ("rust-textwrap" ,(force rust-textwrap-0.15-promise))
                        ("rust-unicase" ,rust-unicase-2)
                        ("rust-yaml-rust" ,rust-yaml-rust-0.4)))))))

(define rust-cbindgen-0.24-promise
  (delay
    (package
     (inherit rust-cbindgen-0.19)
     (name "rust-cbindgen")
     (version "0.24.3")
     (source (origin
              (method url-fetch)
              (uri (crate-uri "cbindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yqxqsz2d0cppd8zwihk2139g5gy38wqgl9snj6rnk8gyvnqsdd6"))))
     (arguments
      `(#:cargo-inputs (("rust-clap" ,(force rust-clap-3.2.16-promise))
                        ("rust-heck" ,rust-heck-0.4)
                        ("rust-indexmap" ,rust-indexmap-1)
                        ("rust-log" ,rust-log-0.4)
                        ("rust-proc-macro2" ,rust-proc-macro2-1)
                        ("rust-quote" ,rust-quote-1)
                        ("rust-serde" ,rust-serde-1)
                        ("rust-serde-json" ,rust-serde-json-1)
                        ("rust-syn" ,rust-syn-1)
                        ("rust-tempfile" ,rust-tempfile-3)
                        ("rust-toml" ,rust-toml-0.5))
        #:cargo-development-inputs (("rust-serial-test" ,rust-serial-test-0.5)))))))

;; Bug with IceCat 102 with cbindgen-0.24, see
;; https://bugzilla.mozilla.org/show_bug.cgi?id=1773259#c5 for
;; possible patch (untested)
(define rust-cbindgen-0.23-promise
  (delay
    (package
     (inherit (force rust-cbindgen-0.24-promise))
     (name "rust-cbindgen")
     (version "0.23.0")
     (source (origin
              (method url-fetch)
              (uri (crate-uri "cbindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "006rn3fn4njayjxr2vd24g1awssr9i3894nbmfzkybx07j728vav")))))))


(define mozilla-compare-locales
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url "https://hg.mozilla.org/l10n/compare-locales/")
          (changeset "RELEASE_8_1_0")))
    (file-name "mozilla-compare-locales")
    (sha256 (base32 "00bpkaqf2ng1nn9ajyb5mli0jq58q5fm2n3yy90jy0hp4q2gbs50"))))

(define (mozilla-locale locale changeset hash-string)
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url (string-append "https://hg.mozilla.org/l10n-central/"
                              locale))
          (changeset changeset)))
    (file-name (string-append "mozilla-locale-" locale))
    (sha256 (base32 hash-string))))

(define-syntax-rule (mozilla-locales (hash-string changeset locale) ...)
  (list (mozilla-locale locale changeset hash-string)
        ...))

(define all-mozilla-locales
  (mozilla-locales
   ;;                      sha256                            changeset    locale
   ;;---------------------------------------------------------------------------
   ("1y562h0dg33vhhhwfk6jl7xbr67gng21vcf3rpm96zzcgbnf8rjj" "503a7baec899" "ach")
   ("1cqixlk9f8p63jz20wzsvnfb7xa82ba725gzdydlwz2axgp09c26" "4e2c7d1ddbed" "af")
   ("19r1yhmfxqasyslc8gr9as5w1scscz1xr8iqy9zi4b90fdjzs0ac" "06897e40a7ea" "an")
   ("0nfknb1p03j9fgmkwlm1mzdyh10g0l33x34ab39kc072apziyv0n" "9272819b09e2" "ar")
   ("11qqblqfffbmkdr5b6mxzq02i8rj1hjq3iy6xv5i5xxy311b3vpb" "f706d22e6910" "ast")
   ("0q2p1a437qr2nshdd4934qkv2sblmykiwzir149c8p9m5sjk0dyw" "f5c2a9800add" "az")
   ("0gxxm3lv18kj0922bw0g897azc4nkrszm5irgwdkgmkbyr97li83" "98ba7d51484f" "be")
   ("10vrbbaabjns824ndya6c09znm815pak2xpvqgjydl3r5qvacx65" "5c79c77311cd" "bg")
   ("1il7yyifx5xzj0v2lna315mppl5pk1vi4m9r66hdz1lzc485m1wp" "c80c87ef7193" "bn")
   ("1p1ig4v9pczzrnnxzr20r61g7xk85k5lgnxhzgn1rx518p85xrkm" "6a1bcb9851b2" "br")
   ("08q33bk9kdvbyc4ib58bsn2b67ki3d2yzskkf5r2n5zlglblksa2" "939779cb58d6" "bs")
   ("1bdkywrqah85fh8kfnz163qnc02ffx0a4vlnx5pq1wg9j4f1gznf" "9a45ccf144f1" "ca")
   ("0hhmp5dzc0rssykl0b2n9h0vfy4svwhxmhpsr3rcfpbg2s0r5h6l" "4f60e18fc248" "ca-valencia")
   ("18phbl9raqsbys9wa8z0gq0h0pw3b55v6ngn67r4373lfc0s9cxv" "b4ef404c7de8" "cak")
   ("0147qyw1y97mgqhkb46wblxv61lq2mvvm5n5ihwf49l5cyizi0jg" "f56ef18f05df" "cs")
   ("08sbhnsxndlsaijnxndc367qcbrzb29m7bpkcawinz9fg6mz3573" "4f9d92f04f5e" "cy")
   ("09cm5kk1sh2a6ws1fj185jrknhfa6n4bhj7nfnr4lsk339n0kvv9" "902503567e30" "da")
   ("0r630bki5d4ylicq6lzh6s2mvsq225933szswmm0sap507rj75cm" "6000baf7a412" "de")
   ("0749qjsfv3rn94rnci3ydgndw27qlr3w3w97kfwma2gmsf3m3r0p" "4a948952d1f4" "dsb")
   ("0yc64i7mpxhs4qlyyh59i2aiz0vpap0bn8slcsk8b71wd1d7cy5m" "153a16a13733" "el")
   ("0d4m5ji6ii10yap8y24cxx3fr60ba1jqi10hy3j1cq6ni7hzavga" "7ce17ae529ac" "en-CA")
   ("12jzqcfbgdhfm8f2gqp15bdnin62li07jwicjc8ynn4cl9a19xiz" "a25d9eea7c23" "en-GB")
   ("0gbb8hfc5yvjah63mapjxm53wxrf98srbrbg43b9g734zfi4n8y5" "4ed3471dad5d" "eo")
   ("19lw7zmqx2irjmv5y6r7nncp6nysn06abf10n3dzj5nzgays6sln" "853fe7acb415" "es-AR")
   ("0rq4pa380b77rkv6dq7sqx8a8i33jk3vs5rgcl8fv7slqvsw23kd" "921b67bf27a5" "es-CL")
   ("1dyxzab9hln5spch66wmfagfzmfdiaxgpq7qlzvdfg98wkqdj25n" "c9a210ea496c" "es-ES")
   ("1gwpmfl37wxl7rynqswgj41liljflgxydvljd4zdhk3jzqn605fb" "ddd35183d81c" "es-MX")
   ("0c3blniddklapifnjbz03f2frw24jjgwgwn6swl5mwm2z0y6wa9f" "82d23ffaa7d3" "et")
   ("05mq2df6n6fr8m5m2xwl0f6n1q3zgjrnk465ldx1nfr9agrhd36c" "13975626d549" "eu")
   ("1l1jyqqiy551903j6yzh9ayg1pf26q2hr9h3jj4l51xzp7x4ms2q" "039e1fdb7c71" "fa")
   ("091l05y9sggxznv0y11b9zy5qf146p0hb5faw4ix7yn5p5kca2f5" "7bd3722d82de" "ff")
   ("1lllwjvmbl5dx44fcvsqg08fbflkc8dx5as9n6nf4xlkzydx6izm" "39808e88c9d1" "fi")
   ("10ha955vvyf5vbciricm72kplj9j0s00g2333lmg46jiwi7aviiv" "426d373db6a7" "fr")
   ("11zdfk8jvdy1k9z1q162cwapplcbz35l3dq4mv45brdin3v0x8kr" "96cd93d18389" "fy-NL")
   ("1l5xr25gmssyachwmfprlnp2v2xj4b0hp8gxrf7fi5bvv9c2fynb" "de3daf7d3f9d" "ga-IE")
   ("06h9ijfsn1cgz5fvxxbisk7fvqhgsw99id92575hgyg1p8xb1n69" "f04aea656d9e" "gd")
   ("19913i5n8yyfhz9j21krlk7wqsyy89vggxc1m1swv94y2ix8rpaj" "1b8cdb87bf69" "gl")
   ("0k5x31bfl2l0r9b5dkn03frm1gd8n6igai7ch2f1kj0skflynwww" "82df570c4241" "gn")
   ("03rygnj9xhfvykark7dls117kwhid13ghvrsqiial0vp8m7ymx79" "e2e41d7beaa5" "gu-IN")
   ("0vyraplj1x7b5im15anhzdaq03k2gqfyybgklrd4f9ry6msh5jhx" "de724e943805" "he")
   ("1zqps586jrgxpf7xx2l3qw3ch3fr31r9a25y861fpxrpvh7ygw7g" "898598e1a5c6" "hi-IN")
   ("0czq68l3qdhdc0mgazlrh8x83a6d5qrjwgjv8xvsmmzi7h68ww0l" "2711d1515af0" "hr")
   ("1wwvrnm38gk2rnw4vmcranh8hk61gjcj9292pck3ckiv6mq7kd4s" "03e02f3d0676" "hsb")
   ("0yvgdxlbyhhg3lk43hxx9gx66rcm7gaj02zzzx5zni8dhvlk2v6m" "d423ada9dc00" "hu")
   ("1kjm7bg4990jhfbxwc38r4lqm2fbcxc82nc2d4qkqwikr4aah4b9" "06836af0bd6e" "hy-AM")
   ("1dla7r3snvagb7m985xxqyi7gl498cr5xsz8msw0zpg6xmi05rlx" "299bd950d538" "ia")
   ("0w8w9xn93akir7nqcp5iwr3kqvba5gbirg7gmzlzg7mgrhw8pcsa" "dee087477b99" "id")
   ("10iakv1c1d20ihxg1s7c3zc1zfw18vr2kspcw7bk5k02rmrffgcn" "320095d063ed" "is")
   ("1xn5pa3rc7l6k2migm3c0dx71q1hk7mjls045xpv9cy8gvirnj94" "4722680fb5bf" "it")
   ("0va9zfj3wqh1yvbgh3x808yvdyxkg780vazjg887wbxpfk1n6hxa" "cb3cfe294621" "ja")
   ("0qvjc3fhk6jg2c3g6mymmnslg1rkkxmv9gi3whf2bc5mzfgyc5zw" "7efe92bd7780" "ja-JP-mac")
   ("0zfiwwz0axbd219ar32c7a6b8h816sf04ajx6jl74s5kyij79y82" "4c1fe3a18da9" "ka")
   ("1aiik4758r5df76q2a132y5fjdjrsxshjylk7irwsr7yy0c7f63g" "acdf76048aa0" "kab")
   ("0icxh4sgv6m1yykycb9d9c43k3r6w02f9c6jr04wm8hvqq5icfl5" "9b418ff7936b" "kk")
   ("1cqlhggf46lr7w399k7qb7a1hc56f32w1dy26i2racpppzza5plc" "9771ada0b5f8" "km")
   ("0p04irnb7x7y37m6lz388x9dynn8rnk000avpp296barajnhs5i8" "645aa37a2112" "kn")
   ("1lbc1fxr5i0ccymlsd8axz3633cqggg5k8ak5yqwi704g7kvv9g2" "1cd68952d119" "ko")
   ("11b55bxg73zqfdn5gy9nn5imab2mv0hxr603whggp7vymllgb7kb" "26bb83959bfe" "lij")
   ("15jsijm6d26i0105gz0f7sh2yh2v4pmw4j95cwkdrb1d8m935jlz" "b9829891f153" "lt")
   ("0liwwv13fgyw97nizhsyy53xrbf8jks5ics7qkkxfccawjrg5rlb" "e5f09d03d959" "lv")
   ("0w420yf3hdnl7dp9mn9ghc20cq79k24fn9adn3syk723ppl6mkb0" "7884845e94f3" "mk")
   ("13wkr7rgqsv9w3d9w7k8lnxdzgfixsq4bmc27zsyc770l6062ml6" "030db7412202" "mr")
   ("06nsadcnxx0ssdp67x47dj9crihn1ycgd5mwvn5m7lkb97r4ch9f" "40a7703e875c" "ms")
   ("0mlnjdzck6dp9p59ajj3sr63r36fn0qi8r9scrqrqcisgfclw9sg" "daca40056531" "my")
   ("0z1hgx9d5i9w20f9k9azzng1y3lmm5d6hdjkj7rf6r5710bhhzh5" "664bd049e105" "nb-NO")
   ("1466qvrs13g2iix1k35cknb2pia9b66li2ncvdyqf0jsd92z9k8x" "eaa6ae781ba0" "ne-NP")
   ("0jgmz2lkzj0aspd9mabygzic6li5s2b25y0r6vjg8mv3jm5pi86j" "5ef8f1c9739e" "nl")
   ("1m46x0h20vdfbzjxlz0ac43dbaq40xnyldr2yzbf7pk5di9pzxa6" "f08e15466d5d" "nn-NO")
   ("0r3zvihqh6iya3z1qm7if0m3zfgf81s9qp7x7qc701h2q357wkgf" "6712c0e12ec2" "oc")
   ("102j89jm28c223yhhkrj76xxj4kmy58hcs2r0jn15xa1qkqv1fdk" "8f36cc819e00" "pa-IN")
   ("1j9za6s0i46abn8dsrlkpck3qxxw35rhfcg1xs1vp8sc4ckg8qwi" "c3b0c1c02b94" "pl")
   ("1k9lbsvckpwl4xax8kxp5yxsfkspq2mhqm77jh5nl9aw88dsn55b" "cc32bf9630fe" "pt-BR")
   ("0f0jyvbn2sa5m66wqdzh4607g4gd0270xq37ipd9ls52b4764bd8" "5478d7242086" "pt-PT")
   ("19znkkialh1d4np7qcp80xkagrf1j2xah2s1xxzsh854p3d30zs7" "dd934a76fb01" "rm")
   ("1xn26r8g8zzkb5myfgmnjbk8k4i0brkvbrvnhp7k5nkrnsin25di" "d57ab3dee73d" "ro")
   ("04rhkxlmpp5pyzw85ldbfkmiigqr2p9y5cbns5dqs6sz1633ccxp" "7aea98f33a20" "ru")
   ("1zzkkxl7ayxh5gx9snyvp5czflsvi77hp48li613h3c2fii4n9qk" "7c986f4b5044" "sco")
   ("1219i0ghhqj3s1i0rm68jjkvivh6y40938wav7z4ifck527sq6r1" "bc9e55d4e3ea" "si")
   ("05i5p3n3a6hgl6rf10yjs4vag7y3rn2jwgxsddcdcqiv6405zn81" "8814afd7f67e" "sk")
   ("1wcqdbm1py1ldq6cj2g201m715nql79r6szr71jvrxacb711c62a" "20013dc06e96" "sl")
   ("0g2izkaa4ipwgwyhy77ciyrxxpf4pxzj9mjqvxriy5prmkhm3zjs" "d86d5d2b6eef" "son")
   ("1cc99m6srjg8698dkc3il70crby0mdv43v3ijwy069k4w50hyjjg" "49bb5fae5d9b" "sq")
   ("13kfssq4fhq9mb36as6sxiaffl17qyg1wdw8kpz3ilqm86bsjrgl" "d6a0ab79b06f" "sr")
   ("1wx9snbm0431q97i0q0nv4wbsqcv9nhllwfr88crlp7bfj5w8czw" "4bab04993da3" "sv-SE")
   ("0laglkfl8ml0ip5vmm3z2q790hgwax9gfqwq3wm68a2nnsyck8fw" "4b3316c4aa48" "szl")
   ("15pnlic4q7m48y0mc5yh8w5yscjwzx0mmfrbj3f2b9jxxa44nx4h" "48c7aab86db8" "ta")
   ("16qar5y0wns75ryi8bfv7yln3mjsyn7qi4345zcniz3ydgkczh46" "7bbb3dac8f47" "te")
   ("112g7x8h0qz16r5faam386x4a0rgwd4zy02d5agmg9m0kbficdfx" "009fd0852454" "th")
   ("1l80kh9byqxnz5vkz357rb39g4y9sgn9pr0v29ywp6d009img1qg" "dad9caecd7a9" "tl")
   ("1nalvjlxxsa9wldhhdb93bgfc7dl084xayh7i7mgg93yjgrym93x" "0c48082d37cd" "tr")
   ("0wkxqcfgsra2ljws28kiwajv73w9aa8drsc4fqlg9krbicnjl2n8" "f6d105faedcc" "trs")
   ("18jf4ysrfv3635fyxc1vwr970dr2r05bl3h8v599pwp2g7jzyjpm" "e1011388a55e" "uk")
   ("1ihmksjsz54yii23qda5iv8cxrj9144afy75hzhvfi6m182kj4h8" "c4e927eab511" "ur")
   ("0d42dhfa2vcw24wsvwf95pw20np0pz8c0is6p4307n981n8s207y" "7063df917cb3" "uz")
   ("04khnkrg8css55hyna01jqay9c2ppxk5znbx2zj9i25knhvvx1lq" "1753054e6ab8" "vi")
   ("0fi5kxn78xp7s15svkqlf4748j4pzxh941nm52n6kbbrhyi3qcqn" "93bc595dc32e" "xh")
   ("0jg676vd37wqgzjnm0yynj7xrvm6fsgdwg296h78wnyc33zc4ads" "edd4e468bc31" "zh-CN")
   ("1y4wldm3z95mfjlficp994jyqg0lj07wi35b79dy1s8ljy3jilil" "0bad1f7d2b2d" "zh-TW")))

;; XXXX: Workaround 'snippet' limitations.
(define computed-origin-method (@@ (guix packages) computed-origin-method))

(define %icecat-version "102.4.0-guix0-preview1")
(define %icecat-build-id "20221019000000") ;must be of the form YYYYMMDDhhmmss

;; 'icecat-source' is a "computed" origin that generates an IceCat tarball
;; from the corresponding upstream Firefox ESR tarball, using the 'makeicecat'
;; script from the upstream IceCat project.
(define icecat-source
  (let* ((base-version (first (string-split %icecat-version #\-)))

         (major-version (first  (string-split base-version #\.)))
         (minor-version (second (string-split base-version #\.)))
         (sub-version   (third  (string-split base-version #\.)))

         (upstream-firefox-version (string-append base-version "esr"))
         (upstream-firefox-source
          (origin
            (method url-fetch)
            (uri (string-append
                  "https://ftp.mozilla.org/pub/firefox/releases/"
                  upstream-firefox-version "/source/"
                  "firefox-" upstream-firefox-version ".source.tar.xz"))
            (sha256
             (base32
              "0klh3lbm0zdmv90kmmpkzgn15pfjibr7zsjy3kvbzpql97fhv7z7"))))

         (upstream-icecat-base-version "102.4.0") ; maybe older than base-version
         ;;(gnuzilla-commit (string-append "v" upstream-icecat-base-version))
         (gnuzilla-commit "8f1aa117ddca6e8cd0114265fb4ca9b5a927565a")
         (gnuzilla-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://git.savannah.gnu.org/gnuzilla.git")
                  (commit gnuzilla-commit)))
            (file-name (git-file-name "gnuzilla"
                                      ;;upstream-icecat-base-version
                                      (string-take gnuzilla-commit 8)))
            (sha256
             (base32
              "0ryrn8ivm763swd0qbqhlgdwc2dj4xjd81d9i2r6hb7bsb4ky3y5"))))

         ;; 'search-patch' returns either a valid file name or #f, so wrap it
         ;; in 'assume-valid-file-name' to avoid 'local-file' warnings.
         (makeicecat-patch
          (local-file (assume-valid-file-name
                       (search-patch "icecat-makeicecat.patch")))))

    (origin
      (method computed-origin-method)
      (file-name (string-append "icecat-" %icecat-version ".tar.xz"))
      (sha256 #f)
      (uri
       (delay
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils))
              (let ((firefox-dir
                     (string-append "firefox-" #$base-version))
                    (icecat-dir
                     (string-append "icecat-" #$%icecat-version)))

                (set-path-environment-variable
                 "PATH" '("bin")
                 (list #+rename
                       #+python
                       #+(canonical-package bash)
                       #+(canonical-package coreutils)
                       #+(canonical-package findutils)
                       #+(canonical-package patch)
                       #+(canonical-package xz)
                       #+(canonical-package sed)
                       #+(canonical-package grep)
                       #+(canonical-package bzip2)
                       #+(canonical-package gzip)
                       #+(canonical-package tar)))

                (set-path-environment-variable
                 "PYTHONPATH"
                 (list #+(format #f "lib/python~a/site-packages"
                                 (version-major+minor
                                  (package-version python))))
                 '#+(cons python-jsonschema
                          (map second
                               (package-transitive-propagated-inputs
                                python-jsonschema))))

                ;; Needed by the 'makeicecat' script.
                (setenv "RENAME_CMD" "rename")

                ;; We copy the gnuzilla source directory because it is
                ;; read-only in 'gnuzilla-source', and the makeicecat script
                ;; uses "cp -a" to copy parts of it and assumes that the
                ;; copies will be writable.
                (copy-recursively #+gnuzilla-source "/tmp/gnuzilla"
                                  #:log (%make-void-port "w"))

                (with-directory-excursion "/tmp/gnuzilla"
                  (make-file-writable "makeicecat")
                  (invoke "patch" "--force" "--no-backup-if-mismatch"
                          "-p1" "--input" #+makeicecat-patch)
                  (patch-shebang "makeicecat")
                  (substitute* "makeicecat"
                    (("^readonly FFMAJOR=(.*)" all ffmajor)
                     (unless (string=? #$major-version
                                       (string-trim-both ffmajor))
                       ;; The makeicecat script cannot be expected to work
                       ;; properly on a different version of Firefox, even if
                       ;; no errors occur during execution.
                       (error "makeicecat major version mismatch"))
                     (string-append "readonly FFMAJOR=" #$major-version "\n"))
                    (("^readonly FFMINOR=.*")
                     (string-append "readonly FFMINOR=" #$minor-version "\n"))
                    (("^readonly FFSUB=.*")
                     (string-append "readonly FFSUB=" #$sub-version "\n"))
                    (("^readonly DATADIR=.*")
                     "readonly DATADIR=/tmp/gnuzilla/data\n")
                    (("^readonly SOURCEDIR=.*")
                     (string-append "readonly SOURCEDIR=" icecat-dir "\n"))
                    (("/bin/sed")
                     #+(file-append (canonical-package sed) "/bin/sed"))))

                (format #t "Unpacking upstream firefox tarball...~%")
                (force-output)
                (invoke "tar" "xf" #+upstream-firefox-source)
                (rename-file firefox-dir icecat-dir)

                (with-directory-excursion icecat-dir
                  (format #t "Populating l10n directory...~%")
                  (force-output)
                  (mkdir "l10n")
                  (with-directory-excursion "l10n"
                    (for-each
                     (lambda (locale-dir)
                       (let ((locale
                              (string-drop (basename locale-dir)
                                           (+ 32  ; length of hash
                                              (string-length "-mozilla-locale-")))))
                         (format #t "  ~a~%" locale)
                         (force-output)
                         (copy-recursively locale-dir locale
                                           #:log (%make-void-port "w"))
                         (for-each make-file-writable (find-files locale))
                         (with-directory-excursion locale
                           (when (file-exists? ".hgtags")
                             (delete-file ".hgtags"))
                           (mkdir-p "browser/chrome/browser/preferences")
                           (call-with-output-file
                               "browser/chrome/browser/preferences/advanced-scripts.dtd"
                             (lambda (port) #f)))))
                     '#+all-mozilla-locales)
                    (copy-recursively #+mozilla-compare-locales
                                      "compare-locales"
                                      #:log (%make-void-port "w"))
                    (delete-file "compare-locales/.gitignore")
                    (delete-file "compare-locales/.hgignore")
                    (delete-file "compare-locales/.hgtags")))

                (format #t "Running makeicecat script...~%")
                (force-output)
                (invoke "bash" "/tmp/gnuzilla/makeicecat")

                (format #t "Packing IceCat source tarball...~%")
                (force-output)
                (setenv "XZ_DEFAULTS" (string-join (%xz-parallel-args)))
                (invoke "tar" "cfa" #$output
                        ;; Avoid non-determinism in the archive.  We set the
                        ;; mtime of files in the archive to early 1980 because
                        ;; the build process fails if the mtime of source
                        ;; files is pre-1980, due to the creation of zip
                        ;; archives.
                        "--mtime=@315619200" ; 1980-01-02 UTC
                        "--owner=root:0"
                        "--group=root:0"
                        "--sort=name"
                        icecat-dir)))))))))

(define-public icecat
  (package
    (name "icecat")
    (version %icecat-version)
    (source icecat-source)
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib
           bzip2
           cups
           dbus-glib
           gdk-pixbuf
           glib
           gtk+
           gtk+-2
           ;; UNBUNDLE-ME! graphite2
           cairo
           pango
           freetype
           font-dejavu
           ;; UNBUNDLE-ME! harfbuzz
           libcanberra
           libgnome
           libjpeg-turbo
           libpng-apng
           ;; UNBUNDLE-ME! libogg
           ;; UNBUNDLE-ME! libtheora ; wants theora-1.2, not yet released
           ;; UNBUNDLE-ME! libvorbis
           libxft
           libevent
           libxinerama
           libxscrnsaver
           libxcomposite
           libxt
           libffi
           ffmpeg
           libvpx
           icu4c-71  ; TODO: Change to 'icu4c' when its version is >= 71.
           pixman
           pulseaudio
           mesa
           pciutils
           mit-krb5
           hunspell
           libnotify
           ;; See <https://bugs.gnu.org/32833>
           ;;   and related comments in the 'remove-bundled-libraries' phase.
           ;; UNBUNDLE-ME! nspr
           ;; UNBUNDLE-ME! nss
           shared-mime-info
           sqlite
           eudev
           unzip
           zip
           zlib))
    (native-inputs
     ;; The following patches are specific to the Guix packaging of IceCat,
     ;; and therefore we prefer to leave them out of 'source', which should be
     ;; a tarball suitable for compilation on any system that IceCat supports.
     ;; (Bug fixes and security fixes, however, should go in 'source').
     (list
      ;; XXX TODO: Adapt these patches to IceCat 102.
      ;; ("icecat-avoid-bundled-libraries.patch"
      ;;  ,(search-patch "icecat-avoid-bundled-libraries.patch"))
      ;; ("icecat-use-system-graphite2+harfbuzz.patch"
      ;;  ,(search-patch "icecat-use-system-graphite2+harfbuzz.patch"))
      ;; ("icecat-use-system-media-libs.patch"
      ;;  ,(search-patch "icecat-use-system-media-libs.patch"))
      ;; TODO: Change the following lines to use 'rust' when it's >= 1.59.
      rust
      `(,rust "cargo")
      (force rust-cbindgen-0.23-promise)
      llvm
      clang
      perl
      node
      python-wrapper
      yasm
      nasm                         ; XXX FIXME: only needed on x86_64 and i686
      pkg-config
      m4
      which))
    (arguments
     (list
      #:tests? #f                       ;not worth the cost

      ;; Some dynamic lib was determined at runtime, so rpath check may fail.
      #:validate-runpath? #f

      #:configure-flags
      #~(list
         "--enable-application=browser"
         "--with-distribution-id=org.gnu"
         "--enable-geckodriver"
         ;; Do not require addons in the global app or system directories to
         ;; be signed by Mozilla.
         "--with-unsigned-addon-scopes=app,system"
         "--allow-addon-sideload"

         "--enable-pulseaudio"

         "--disable-tests"
         "--disable-updater"
         "--disable-crashreporter"
         "--disable-eme"

         ;; Building with debugging symbols takes ~5GiB, so disable it.
         "--disable-debug"
         "--disable-debug-symbols"

         "--enable-rust-simd"
         "--enable-release"
         "--enable-optimize"
         "--enable-strip"
         "--disable-elf-hack"

         ;; Clang is needed to build Stylo, Mozilla's new CSS engine.  We must
         ;; specify the clang paths manually, because otherwise the Mozilla
         ;; build system looks in the directories returned by llvm-config
         ;; --bindir and llvm-config --libdir, which return paths in the llvm
         ;; package where clang is not found.
         (string-append "--with-clang-path="
                        (search-input-file %build-inputs "bin/clang"))
         (string-append "--with-libclang-path="
                        (dirname (search-input-file %build-inputs
                                                    "lib/libclang.so")))

         ;; Hack to work around missing "unofficial" branding in icecat.
         "--enable-official-branding"

         ;; TODO: Add support for wasm sandboxed libraries.
         "--without-wasm-sandboxed-libraries"

         ;; Avoid bundled libraries.
         "--with-system-jpeg"           ;must be libjpeg-turbo
         "--with-system-png"            ;must be libpng-apng
         "--with-system-zlib"
         ;; UNBUNDLE-ME! "--with-system-bz2"
         ;; UNBUNDLE-ME! "--with-system-libevent"
         ;; UNBUNDLE-ME! "--with-system-ogg"
         ;; UNBUNDLE-ME! "--with-system-vorbis"
         ;; UNBUNDLE-ME! "--with-system-theora" ; wants theora-1.2, not yet released
         ;; UNBUNDLE-ME! "--with-system-libvpx"
         "--with-system-icu"

         ;; See <https://bugs.gnu.org/32833>
         ;;   and related comments in the
         ;;   'remove-bundled-libraries' phase below.
         ;; UNBUNDLE-ME! "--with-system-nspr"
         ;; UNBUNDLE-ME! "--with-system-nss"

         ;; UNBUNDLE-ME! "--with-system-harfbuzz"
         ;; UNBUNDLE-ME! "--with-system-graphite2"
         "--enable-system-pixman"
         "--enable-system-ffi"
         ;; UNBUNDLE-ME! "--enable-system-sqlite"
         )

      #:imported-modules %cargo-utils-modules ;for `generate-all-checksums'

      #:modules `((ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (rnrs bytevectors)
                  (rnrs io ports)
                  (guix elf)
                  (guix build gremlin)
                  ,@%gnu-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'apply-guix-specific-patches
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let ((patch (search-input-file inputs "bin/patch")))
                (for-each (match-lambda
                            ((label . file)
                             (when (and (string-prefix? "icecat-" label)
                                        (string-suffix? ".patch" label))
                               (format #t "applying '~a'...~%" file)
                               (invoke patch "--force" "--no-backup-if-mismatch"
                                       "-p1" "--input" file))))
                          (or native-inputs inputs)))))
          (add-after 'apply-guix-specific-patches 'remove-bundled-libraries
            (lambda _
              ;; Remove bundled libraries that we don't use, since they may
              ;; contain unpatched security flaws, they waste disk space and
              ;; memory, and may cause confusion.
              (for-each (lambda (file)
                          (format #t "deleting '~a'...~%" file)
                          (delete-file-recursively file))
                        '( ;; FIXME: Removing the bundled icu breaks configure.
                          ;;   * The bundled icu headers are used in some places.
                          ;;   * The version number is taken from the bundled copy.
                          ;;"intl/icu"
                          ;;
                          ;; FIXME: A script from the bundled nspr is used.
                          ;;"nsprpub"
                          ;;
                          ;; FIXME: With the update to IceCat 60, using system NSS
                          ;;        broke certificate validation.  See
                          ;;        <https://bugs.gnu.org/32833>.  For now, we use
                          ;;        the bundled NSPR and NSS.  TODO: Investigate,
                          ;;        and try to unbundle these libraries again.
                          ;; UNBUNDLE-ME! "security/nss"
                          ;;
                          ;; TODO: Use more system media libraries.  See:
                          ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=517422>
                          ;;   * libtheora: esr60 wants v1.2, not yet released.
                          ;;   * soundtouch: avoiding the bundled library would
                          ;;     result in some loss of functionality.  There's
                          ;;     also an issue with exception handling
                          ;;     configuration.  It seems that this is needed in
                          ;;     some moz.build:
                          ;;       DEFINES['ST_NO_EXCEPTION_HANDLING'] = 1
                          ;;   * libopus
                          ;;   * speex
                          ;;
                          "modules/freetype2"
                          ;; "media/libjpeg"  ; needed for now, because media/libjpeg/moz.build is referenced from config/external/moz.build
                          ;; UNBUNDLE-ME! "modules/zlib"
                          ;; UNBUNDLE-ME! "ipc/chromium/src/third_party/libevent"
                          ;; UNBUNDLE-ME! "media/libvpx"
                          ;; UNBUNDLE-ME! "media/libogg"
                          ;; UNBUNDLE-ME! "media/libvorbis"
                          ;; UNBUNDLE-ME! "media/libtheora" ; wants theora-1.2, not yet released
                          ;; UNBUNDLE-ME! "media/libtremor"
                          ;; UNBUNDLE-ME! "gfx/harfbuzz"
                          ;; UNBUNDLE-ME! "gfx/graphite2"
                          "js/src/ctypes/libffi"
                          ;; UNBUNDLE-ME! "db/sqlite3"
                          ))))
          (add-after 'remove-bundled-libraries 'fix-ffmpeg-runtime-linker
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Arrange to load libavcodec.so by its absolute file name.
              (substitute* "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
                (("libavcodec\\.so")
                 (search-input-file inputs "lib/libavcodec.so")))))
          (add-after 'fix-ffmpeg-runtime-linker 'build-sandbox-whitelist
            (lambda* (#:key inputs #:allow-other-keys)
              (define (runpath-of lib)
                (call-with-input-file lib
                  (compose elf-dynamic-info-runpath
                           elf-dynamic-info
                           parse-elf
                           get-bytevector-all)))
              (define (runpaths-of-input label)
                (let* ((dir (string-append (assoc-ref inputs label) "/lib"))
                       (libs (find-files dir "\\.so$")))
                  (append-map runpath-of libs)))
              ;; Populate the sandbox read-path whitelist as needed by ffmpeg.
              (let* ((whitelist
                      (map (cut string-append <> "/")
                           (delete-duplicates
                            `(,(string-append (assoc-ref inputs "shared-mime-info")
                                              "/share/mime")
                              ,(string-append (assoc-ref inputs "font-dejavu")
                                              "/share/fonts")
                              "/run/current-system/profile/share/fonts"
                              ,@(append-map runpaths-of-input
                                            '("mesa" "ffmpeg"))))))
                     (whitelist-string (string-join whitelist ","))
                     (port (open-file "browser/app/profile/icecat.js" "a")))
                (format #t "setting 'security.sandbox.content.read_path_whitelist' to '~a'~%"
                        whitelist-string)
                (format port "~%pref(\"security.sandbox.content.read_path_whitelist\", ~S);~%"
                        whitelist-string)
                (close-output-port port))))
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (use-modules (guix build cargo-utils))
              (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
                (for-each (lambda (file)
                            (format #t "patching checksums in ~a~%" file)
                            (substitute* file
                              (("^checksum = \".*\"")
                               (string-append "checksum = \"" null-hash "\""))))
                          (find-files "." "Cargo.lock$"))
                (for-each generate-all-checksums
                          '("services"
                            "js"
                            "third_party/rust"
                            "dom/media"
                            "dom/webauthn"
                            "toolkit"
                            "gfx"
                            "storage"
                            "modules"
                            "xpcom/rust"
                            "media"
                            "mozglue/static/rust"
                            "netwerk"
                            "remote"
                            "intl"
                            "servo"
                            "security/manager/ssl"
                            "build")))))
          (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
            (lambda _
              ;; Remove --frozen flag from cargo invokation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                           (("\"--frozen\",") ""))))
          (delete 'bootstrap)
          (replace 'configure
            ;; configure does not work followed by both "SHELL=..." and
            ;; "CONFIG_SHELL=..."; set environment variables instead
            (lambda* (#:key outputs configure-flags #:allow-other-keys)
              (let* ((bash (which "bash"))
                     (abs-srcdir (getcwd))
                     (flags `(,(string-append "--prefix=" #$output)
                              ,(string-append "--with-l10n-base="
                                              abs-srcdir "/l10n")
                              ,@configure-flags)))
                (setenv "SHELL" bash)
                (setenv "CONFIG_SHELL" bash)

                (setenv "AR" "llvm-ar")
                (setenv "NM" "llvm-nm")
                (setenv "CC" "clang")
                (setenv "CXX" "clang++")
                (setenv "LDFLAGS" (string-append "-Wl,-rpath="
                                                 #$output "/lib/icecat"))

                (setenv "MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE" "system")
                (setenv "MOZ_BUILD_DATE" #$%icecat-build-id) ; avoid timestamp

                ;; XXX TODO: Fix this to work on systems other than x86_64-linux.
                (setenv "GUIX_PYTHONPATH"
                        (string-append (getcwd)
                                       "/obj-x86_64-pc-linux-gnu/_virtualenvs/build"))

                (mkdir ".mozbuild")
                (setenv "MOZBUILD_STATE_PATH"
                        (string-append (getcwd) "/.mozbuild"))

                (format #t "build directory: ~s~%" (getcwd))
                (format #t "configure flags: ~s~%" flags)

                (call-with-output-file "mozconfig"
                  (lambda (port)
                    (for-each (lambda (flag)
                                (format port "ac_add_options ~a\n" flag))
                              flags)))

                (invoke "./mach" "configure"))))
          (replace 'build
            (lambda* (#:key (make-flags '()) (parallel-build? #t)
                      #:allow-other-keys)
              (apply invoke "./mach" "build"
                     ;; mach will use parallel build if possible by default
                     `(,@(if parallel-build?
                             '()
                             '("-j1"))
                       ,@make-flags))))
          (add-after 'build 'neutralise-store-references
            (lambda _
              ;; Mangle the store references to compilers & other build tools in
              ;; about:buildconfig, reducing IceCat's closure by 1 GiB on x86-64.
              (let* ((obj-dir (match (scandir "." (cut string-prefix? "obj-" <>))
                                ((dir) dir)))
                     (file (string-append
                            obj-dir
                            "/dist/bin/chrome/toolkit/content/global/buildconfig.html")))
                (substitute* file
                  (("[0-9a-df-np-sv-z]{32}" hash)
                   (string-append (string-take hash 8)
                                  "<!-- Guix: not a runtime dependency -->"
                                  (string-drop hash 8)))))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (invoke "./mach" "install")
              ;; The geckodriver binary is not installed by the above, for some
              ;; reason.  Use 'find-files' to avoid having to deal with the
              ;; system/architecture-specific file name.
              (install-file (first (find-files "." "geckodriver"))
                            (string-append #$output "/bin"))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib (string-append #$output "/lib"))
                     (gtk #$(this-package-input "gtk+"))
                     (gtk-share (string-append gtk "/share"))
                     (ld-libs '#$(map (lambda (label)
                                        (file-append (this-package-input label) "/lib"))
                                      '("libpng-apng"
                                        "libxscrnsaver"
                                        "mesa"
                                        "pciutils"
                                        "mit-krb5"
                                        "eudev"
                                        "pulseaudio"
                                        ;; For the integration of native notifications
                                        "libnotify"))))
                (wrap-program (car (find-files lib "^icecat$"))
                  `("XDG_DATA_DIRS" prefix (,gtk-share))
                  ;; The following line is commented out because the icecat
                  ;; package on guix has been observed to be unstable when
                  ;; using wayland, and the bundled extensions stop working.
                  ;;   `("MOZ_ENABLE_WAYLAND" = ("1"))
                  `("LD_LIBRARY_PATH" prefix ,ld-libs)))))
          (add-after 'wrap-program 'install-desktop-entry
            (lambda _
              ;; Install the '.desktop' file.
              (let* ((desktop-file "taskcluster/docker/icecat-snap/icecat.desktop")
                     (applications (string-append #$output "/share/applications")))
                (substitute* desktop-file
                  (("^Exec=icecat")     (string-append "Exec=" #$output "/bin/icecat"))
                  (("IceCat")           "GNU IceCat")
                  (("Icon=.*")          "Icon=icecat\n")
                  (("NewWindow")        "new-window")
                  (("NewPrivateWindow") "new-private-window")
                  (("StartupNotify=true")
                   "StartupNotify=true\nStartupWMClass=Navigator"))
                (install-file desktop-file applications))))
          (add-after 'install-desktop-entry 'install-icons
            (lambda _
              (with-directory-excursion "browser/branding/official"
                (for-each
                 (lambda (file)
                   (let* ((size (string-filter char-numeric? file))
                          (icons (string-append #$output "/share/icons/hicolor/"
                                                size "x" size "/apps")))
                     (mkdir-p icons)
                     (copy-file file (string-append icons "/icecat.png"))))
                 '("default16.png" "default22.png" "default24.png"
                   "default32.png" "default48.png" "content/icon64.png"
                   "mozicon128.png" "default256.png"))))))))
    (home-page "https://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.  This package also includes the
@command{geckodriver} command, which can be useful for automated web
testing.

WARNING: IceCat 102 has not yet been released by the upstream IceCat project.
This is a preview release, and does not currently meet the privacy-respecting
standards of the IceCat project.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))

(define %icedove-build-id "20221012000000") ;must be of the form YYYYMMDDhhmmss
(define %icedove-version "102.3.3")

;; Provides the "comm" folder which is inserted into the icecat source.
;; Avoids the duplication of Icecat's source tarball.
(define thunderbird-source
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url "https://hg.mozilla.org/releases/comm-esr102")
          (changeset "afeec21c1fcc27ba58f98f629e85609a728f79e6")))
    (file-name (string-append "thunderbird-" %icedove-version "-checkout"))
    (sha256
     (base32
      "1n4cj673akv9rwymc4bj3g3cx39amg9xpi504vkjpmykfbvrvr01"))))

(define-public icedove
  (package
    (name "icedove")
    (version %icedove-version)
    (source icecat-source)
    (properties
     `((cpe-name . "thunderbird_esr")))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                             ;no check target
      #:imported-modules %cargo-utils-modules ;for `generate-all-checksums'
      #:modules `((guix build utils)          ;find-files
                  (sxml simple)
                  (ice-9 regex)
                  ,@%gnu-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-thunderbird-sources
            (lambda _
              (mkdir "comm")
              (copy-recursively #$thunderbird-source "comm")
              (delete-file "sourcestamp.txt")))
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (use-modules (guix build cargo-utils))
              (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934\
ca495991b7852b855"))
                (for-each (lambda (file)
                            (format #t "patching checksums in ~a~%" file)
                            (substitute* file
                              (("^checksum = \".*\"")
                               (string-append "checksum = \"" null-hash "\""))))
                          (find-files "." "Cargo.lock$"))
                (for-each generate-all-checksums
                          '("third_party/rust"
                            "toolkit/library/rust")))))
          (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
            (lambda _
              ;; Remove --frozen flag from cargo invokation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                (("\"--frozen\",") ""))))
          ;; Fixes issue where each installation directory generates its own
          ;; profile (see:
          ;; https://trac.torproject.org/projects/tor/ticket/31457).
          (add-after 'patch-source-shebangs 'fix-profile-setting
            (lambda _
              (substitute* "comm/mail/moz.configure"
                (("MOZ_DEDICATED_PROFILES, True")
                 "MOZ_DEDICATED_PROFILES, False"))))
          (add-after 'prepare-thunderbird-sources 'rename-to-icedove
            (lambda _
              (substitute* "comm/mail/confvars.sh"
                (("MOZ_APP_NAME=thunderbird")
                 "MOZ_APP_NAME=icedove")
                (("MOZ_UPDATER=1")
                 "MOZ_UPDATER=0"))
              ;; Remove branding to comply with Mozilla's trademark policy
              (with-directory-excursion "comm/mail/branding/nightly"
                (delete-file "content/about-wordmark.svg")
                (call-with-output-file "content/about-wordmark.svg"
                  (lambda (port)
                    (sxml->xml '(svg (@ (xmlns "http://www.w3.org/2000/svg")
                                        (viewBox "0 0 789.1 90.78")
                                        (width "333")
                                        (height "48")
                                        (fill "#fff"))
                                     (text (@ (x "400") (y "70")
                                              (text-anchor "middle")
                                              (font-size "90"))
                                           "Icedove Daily"))
                               port)))
                (substitute* '("locales/en-US/brand.properties"
                               "locales/en-US/brand.ftl"
                               "locales/en-US/brand.dtd"
                               "configure.sh")
                  (("Thunderbird") "Icedove")
                  (("mozilla.org") "guix.gnu.org")))
              ;; Remove other mentions of Thunderbird in user-visible text.
              (with-directory-excursion "comm/mail/base/content"
                (substitute* '("overrides/app-license-name.html")
                  (("Thunderbird") "Icedove")))
              (with-directory-excursion "comm/mail/components/"
                (substitute* '("MailGlue.jsm"
                               "extensions/schemas/addressBook.json"
                               "extensions/schemas/tabs.json"
                               "extensions/schemas/cloudFile.json"
                               "extensions/schemas/chrome_settings_overrides.json"
                               "extensions/schemas/windows.json"
                               "extensions/parent/ext-mail.js"
                               "im/messages/mail/Info.plist"
                               "enterprisepolicies/moz.build"
                               "enterprisepolicies/helpers/moz.build"
                               "enterprisepolicies/schemas/moz.build")
                  (("Thunderbird") "Icedove")))
              (substitute* '("comm/mailnews/base/prefs/content/accountUtils.js"
                             "comm/mail/base/content/customizeToolbar.js"
                             "comm/suite/components/customizeToolbar.js")
                (("AppConstants.MOZ_APP_NAME (.)= \"thunderbird" _ e)
                 (format #f "AppConstants.MOZ_APP_NAME ~a= \"icedove" e)))

              ;; Override addon URLs and settings
              (substitute* "comm/mail/app/profile/all-thunderbird.js"
                (("(pref\\(\"extensions.webservice.discoverURL\").*" _ m)
                 (string-append m ", \"https://directory.fsf.org/wiki/Icedove\");"))
                (("(pref\\(\"extensions.getAddons.search.url\").*" _ m)
                 (string-append m ", \"https://guix.gnu.org/packages\");"))
                (("(pref\\(\"extensions.update.enabled\").*" _ m)
                 (string-append m ", false);"))
                (("(pref\\(\"extensions.systemAddon.update.enabled\").*" _ m)
                 (string-append m ", false);"))
                (("(pref\\(\"lightweightThemes.update.enabled\").*" _ m)
                 (string-append m ", false);")))))
          (add-after 'build 'neutralize-store-references
            (lambda _
              ;; Mangle the store references to compilers & other build tools in
              ;; about:buildconfig, reducing Icedove's closure significant.
              ;; The resulting files are saved in lib/thunderbird/omni.ja
              (substitute*
                  ;; Use find because the path "obj-x86_64-pc-linux-gnu" contains
                  ;; the architecture and the system -> more complicated.
                  (find-files "." "buildconfig.html")
                (((format #f "(~a/)([0-9a-df-np-sv-z]{32})"
                          (regexp-quote (%store-directory)))
                  _ store hash)
                 (string-append store
                                (string-take hash 8)
                                "<!-- Guix: not a runtime dependency -->"
                                (string-drop hash 8))))))
          (delete 'bootstrap)
          (replace 'configure
            (lambda* (#:key inputs configure-flags #:allow-other-keys)
              (let* ((bash (which "bash"))
                     (abs-srcdir (getcwd))
                     (srcdir (string-append "../" (basename abs-srcdir)))
                     (flags `(,(string-append "--prefix=" #$output)
                              ,@configure-flags))
                     (mozconfig (string-append (getcwd) "/.mozconfig")))
                (setenv "SHELL" bash)
                (setenv "CONFIG_SHELL" bash)
                (setenv "QA_CONFIGURE_OPTIONS" ".*")
                (setenv "MOZBUILD_STATE_PATH"
                        (string-append (getcwd) "/mach_state"))
                (setenv "MOZCONFIG"
                        (string-append (getcwd) "/.mozconfig"))

                (setenv "AR" "llvm-ar")
                (setenv "NM" "llvm-nm")
                (setenv "CC" "clang")
                (setenv "CXX" "clang++")

                (setenv "MOZ_NOSPAM" "1")
                (setenv "MACH_USE_SYSTEM_PYTHON" "1")
                (setenv "PYTHON"
                        (search-input-file inputs "/bin/python"))
                (setenv "MOZ_BUILD_DATE" #$%icedove-build-id) ; avoid timestamp
                (setenv "MOZ_APP_NAME" "icedove")
                (setenv "LDFLAGS" (string-append "-Wl,-rpath=" #$output
                                                 "/lib/icedove"))
                (mkdir-p (string-append (getcwd) "/builddir"))
                (with-output-to-file mozconfig
                  (lambda ()
                    (display
                     (string-append
                      "ac_add_options --disable-crashreporter\n"
                      "ac_add_options --disable-debug\n"
                      "ac_add_options --disable-debug-symbols\n"
                      "ac_add_options --disable-elf-hack\n"
                      "ac_add_options --disable-jit\n"
                      "ac_add_options --disable-necko-wifi\n"
                      "ac_add_options --disable-official-branding\n"
                      "ac_add_options --disable-tests\n"
                      "ac_add_options --disable-updater\n"
                      "ac_add_options --disable-webrtc\n"
                      "ac_add_options --enable-application=comm/mail\n"
                      "ac_add_options --enable-default-toolkit=\"cairo-gtk3\"\n"
                      "ac_add_options --enable-optimize\n"
                      "ac_add_options --enable-pulseaudio\n"
                      "ac_add_options --enable-release\n"
                      "ac_add_options --enable-strip\n"
                      "ac_add_options --enable-system-ffi\n"
                      "ac_add_options --enable-system-pixman\n"
                      "ac_add_options --prefix=" #$output "\n"
                      "ac_add_options --with-clang-path=" (assoc-ref %build-inputs "clang") "/bin/clang\n"
                      "ac_add_options --with-distribution-id=org.gnu\n"
                      "ac_add_options --with-libclang-path=" (assoc-ref %build-inputs "clang") "/lib\n"
                      "ac_add_options --with-system-bz2\n"
                      "ac_add_options --with-system-icu\n"
                      "ac_add_options --with-system-jpeg\n"
                      "ac_add_options --with-system-libevent\n"
                      "ac_add_options --with-system-nspr\n"
                                        ;"ac_add_options --with-system-nss\n"
                      "ac_add_options --with-system-zlib\n"
                      "ac_add_options --without-wasm-sandboxed-libraries\n"
                      "mk_add_options MOZ_MAKE_FLAGS=-j"
                      (number->string (parallel-job-count)) "\n"))))
                (display (getcwd))
                (newline)
                (display "mach configure")
                (invoke "./mach" "configure"))))
          (replace 'build
            (lambda _ (invoke "./mach" "build")))
          (replace 'install
            (lambda _ (invoke "./mach" "install")))
          ;; Thunderbird doesn't provide any .desktop file.
          ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1637575
          (add-after 'install 'install-desktop-file
            (lambda _
              (let ((apps (string-append #$output "/share/applications")))
                (mkdir-p apps)
                (with-output-to-file (string-append apps "/icedove.desktop")
                  (lambda _
                    (format #t
                            "[Desktop Entry]~@
                            Name=Icedove~@
                            Exec=~a/bin/icedove~@
                            Icon=icedove~@
                            GenericName=Mail/News Client~@
                            Categories=Network;Email;~@
                            Terminal=false~@
                            StartupNotify=true~@
                            MimeType=x-scheme-handler/mailto;~@
                            Type=Application~@
                            Actions=ComposeMessage;~@
                            [Desktop Action ComposeMessage]~@
                            Name=Write new message~@
                            Exec=~@*~a/bin/icedove -compose~%"
                            #$output))))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib (string-append #$output "/lib"))
                     (gtk #$(this-package-input "gtk+"))
                     (gtk-share (string-append gtk "/share"))
                     (pulseaudio #$(this-package-input "pulseaudio"))
                     (pulseaudio-lib (string-append pulseaudio "/lib"))
                     (eudev #$(this-package-input "eudev"))
                     (eudev-lib (string-append eudev "/lib")))
                (wrap-program (car (find-files lib "^icedove$"))
                  `("XDG_DATA_DIRS" prefix (,gtk-share))
                  `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib ,eudev-lib)))))))))
    (inputs
     (list alsa-lib
           bzip2
           cairo
           cups
           dbus-glib
           ffmpeg
           freetype
           gdk-pixbuf
           glib
           gtk+
           gtk+-2
           hunspell
           icu4c-71
           libcanberra
           libevent
           libffi
           libgnome
           libjpeg-turbo
           libpng-apng
           libvpx
           libxcomposite
           libxft
           libxinerama
           libxscrnsaver
           libxt
           mesa
           mit-krb5
           nspr-4.32
           ;; FIXME: create nss >= 3.68 after core-updates merge
           ;;nss
           pango
           pixman
           pulseaudio
           sqlite
           startup-notification
           eudev
           unzip
           zip
           zlib))
    (native-inputs
     (list `(,rust "cargo")
           clang
           llvm
           m4
           nasm
           node
           perl
           pkg-config
           python-wrapper
           rust
           (force rust-cbindgen-0.23-promise)
           which
           yasm))
    (home-page "https://www.thunderbird.net")
    (synopsis "Rebranded Mozilla Thunderbird email client")
    (description
     "This package provides an email client built based on Mozilla
Thunderbird.  It supports email, news feeds, chat, calendar and contacts.")
    (license license:mpl2.0)))

(define-public icedove/wayland
  (package
    (inherit icedove)
    (name "icedove-wayland")
    (native-inputs '())
    (inputs
     `(("bash" ,bash-minimal)
       ("icedove" ,icedove)))
    (build-system trivial-build-system)
    (arguments
      '(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let* ((bash    (assoc-ref %build-inputs "bash"))
                 (icedove (assoc-ref %build-inputs "icedove"))
                 (out     (assoc-ref %outputs "out"))
                 (exe     (string-append out "/bin/icedove")))
            (mkdir-p (dirname exe))

            (call-with-output-file exe
              (lambda (port)
                (format port "#!~a
 MOZ_ENABLE_WAYLAND=1 exec ~a $@"
                        (string-append bash "/bin/bash")
                        (string-append icedove "/bin/icedove"))))
            (chmod exe #o555)

            ;; Provide the manual and .desktop file.
            (copy-recursively (string-append icedove "/share")
                              (string-append out "/share"))
            (substitute* (string-append
                          out "/share/applications/icedove.desktop")
              ((icedove) out))
            #t))))))

(define-public firefox-decrypt
  (package
    (name "firefox-decrypt")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Unode/firefox_decrypt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17yyyxp47z4m8hnflcq34rc1y871515kr3f1y42j1l0yx3g0il07"))))
    (build-system trivial-build-system)
    (inputs
     (list nss python))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (setenv "PATH"
                 (string-append
                  (assoc-ref %build-inputs "python") "/bin"))
         (copy-file (string-append (assoc-ref %build-inputs "source")
                                   "/firefox_decrypt.py")
                    "firefox_decrypt.py")
         (substitute* "firefox_decrypt.py"
           (("/usr/bin/env python") (which "python3"))
           (("libnss3.so") (string-append (assoc-ref %build-inputs "nss")
                                          "/lib/nss/libnss3.so")))
         (install-file "firefox_decrypt.py" (string-append %output "/bin"))
         #t)))
    (home-page "https://github.com/Unode/firefox_decrypt/")
    (synopsis "Tool to extract passwords from Mozilla profiles")
    (description "Firefox Decrypt is a tool to extract passwords from
Mozilla (Firefox, Waterfox, Thunderbird, SeaMonkey) profiles.")
    (license license:gpl3+)))

(define-public lz4json
  (package
    (name "lz4json")
    (version "2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/andikleen/lz4json")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xxn8yzr6j8j6prmbj6mxspdczigarfiv3vlm9k70yxmky65ijh3"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list lz4))
    (arguments
     `(#:tests? #f                              ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)                    ; no configure script
         (replace 'install                      ; no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "lz4jsoncat" bin)
               (install-file "lz4jsoncat.1" man)))))
       #:make-flags `(,(string-append "CC=" ,(cc-for-target)))))
    (home-page "https://github.com/andikleen/lz4json")
    (synopsis "C decompress tool for mozilla lz4json format")
    (description
     "@code{lz4json} is a little utility to unpack lz4json files as generated
by Firefox's bookmark backups and session restore.  This is a different format
from what the normal lz4 utility expects.  The data is dumped to stdout.")
    (license license:bsd-2)))
