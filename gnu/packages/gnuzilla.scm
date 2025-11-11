;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014-2025 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016-2019, 2021, 2024, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017, 2023 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020, 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020-2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021-2023, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Baptiste Strazzul <bstrazzull@hotmail.fr>
;;; Copyright © 2022 SeerLite <seerlite@disroot.org>
;;; Copyright © 2024 Aleksandr Vityazev <avityazew@gmail.com>
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
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:autoload (ice-9 pretty-print) (pretty-print)
  #:autoload (ice-9 textual-ports) (get-string-all)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system mozilla)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages time))

(define-public mozjs
  (package
    (name "mozjs")
    (version "140.3.0")
    (source (origin
              (method url-fetch)
              ;; TODO: Switch to IceCat source once available on ftp.gnu.org.
              (uri (string-append "https://ftp.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "05i3czn3v2qnhir8apcphbqy7rmy1dn7kcwx5yyi2qvmjcyfpipg"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules %cargo-utils-modules ;for `generate-all-checksums'
      #:modules `((guix build cargo-utils)
                  ,@%default-gnu-modules)
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
         "--enable-rust-simd"
         "--enable-shared-js"
         "--with-system-icu"
         "--with-system-nspr"
         "--with-system-zlib"
         "--with-intl-api")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'python-3.11-compatibility
            (lambda _
              (substitute* '("python/mozbuild/mozpack/files.py"
                             "python/mozbuild/mozbuild/util.py"
                             "python/mozbuild/mozbuild/action/process_define_files.py"
                             "python/mozbuild/mozbuild/backend/base.py"
                             "python/mozbuild/mozbuild/preprocessor.py")
                (("\"rU\"") "\"r\""))))
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
              ;; Configure script writes to $HOME.
              (setenv "HOME" (getcwd))
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

                ;; Most of the timezone related failures are probably
                ;; attributable to our use of a system-provided icu4c library
                ;; instead of the bundled one.
                (for-each
                 delete-file
                 '( ;; FIXME: An one-hour difference is produced after DST
                   ;; starting in the timezone the test suite uses.
                   "non262/Date/15.9.5.7.js"

                   ;; The test suite expects a lightly patched ICU.  Disable tests
                   ;; that do not work with the system version.  See
                   ;; "intl/icu-patches" for clues.

                   ;; See <https://unicode-org.atlassian.net/browse/ICU-20992> and
                   ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=1636984> and
                   ;; related patch for why this is failing.
                   "non262/Intl/DateTimeFormat/fractional-second-digits-append-item.js"
                   ;; FIXME: got "0 \u251CAM/PM: noon\u2524", expected "0 (AM/PM: noon)"
                   "non262/Intl/DateTimeFormat/day-period-hour-cycle.js"
                   ;; FIXME: got "en-US-posix", expected "en-US-POSIX".
                   "non262/Intl/available-locales-supported.js"
                   ;; FIXME: got "en-US", expected "en-US-POSIX"
                   "non262/Intl/available-locales-resolved.js"

;;; Since 115:
                   ;; Mismatching array lengths
                   "non262/Intl/supportedValuesOf-timeZones-canonical.js"
                   ;; TODO: tzdata 2024a expected – find a way to regenerate
                   ;; these generated tests
                   "non262/Intl/DateTimeFormat/timeZone_version.js"

                   ;; FIXME: got "\uD840\uDDF2", expected "\u5047"
                   "non262/Intl/Collator/implicithan.js"
                   ;; FIXME: got "\uD840\uDDF2", expected "\u3467"
                   "non262/Intl/Collator/big5han-gb2312han.js"

                   ;; Since 128:
                   ;; FIXME: got (void 0), expected "GMT"
                   "non262/Intl/DateTimeFormat/formatRange-timeZoneName-matches-format.js"
                   ;; FIXME: got 7, expected 9: parts count mismatch
                   "non262/Intl/DateTimeFormat/formatRange-timeZone-offset.js"
                   "non262/Intl/DateTimeFormat/formatRange-timeZoneName.js"

                   ;; Since 140:
                   ;; RangeError: invalid time zone: America/Coyhaique
                   "non262/Temporal/ZonedDateTime/zones-and-links.js"
                   ;; got 2042, expected 2043
                   "non262/Temporal/Intl/consistent-dates.js"
                   ;;  got "Pacific/Auckland", expected "Antarctica/McMurdo"
                   "non262/Intl/DateTimeFormat/timeZone_links.js"
                   "test262/staging/sm/Temporal/ZonedDateTime/zones-and-links.js"
                   ;; Test262Error: Expected true but got false
                   "test262/staging/Intl402/Temporal/old/zoneddatetime-dst-corner-cases.js"
                   ;; Test262Error: getTimeZoneTransition(next) does not return its input
                   "test262/intl402/Temporal/ZonedDateTime/prototype/\
getTimeZoneTransition/result-type.js"
                   ;; Expected SameValue («1912-01-01T00:16:08+00:00[Africa/Abidjan]», «null»
                   "test262/intl402/Temporal/ZonedDateTime/prototype/\
getTimeZoneTransition/transition-at-instant-boundaries.js"
                   ;; Expected SameValue(«1762063200000000000n», «1572760800000000000n»
                   "test262/intl402/Temporal/ZonedDateTime/prototype/\
getTimeZoneTransition/specific-tzdb-values.js"
                   ;; Expected SameValue(«"2025-03-30T03:00:00+02:00[Europe/Berlin]"»,
                   ;; «"2020-10-25T02:00:00+01:00[Europe/Berlin]"»
                   "test262/intl402/Temporal/ZonedDateTime/prototype/\
getTimeZoneTransition/nanoseconds-subtracted-or-added-at-dst-transition.js"
                   ;; TypeError: can't access property "epochNanoseconds",
                   ;; before.getTimeZoneTransition(...) is null
                   "test262/intl402/Temporal/ZonedDateTime/prototype/\
getTimeZoneTransition/transitions-close-together.js")))))
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
           `(,rust "cargo")
           rust-cbindgen))
    (inputs
     (list icu4c-77 readline zlib))
    (propagated-inputs
     (list nspr))                ; in the Requires.private field of mozjs-*.pc
    (home-page
     "https://spidermonkey.dev/")
    (synopsis "Mozilla JavaScript engine")
    (description "SpiderMonkey is Mozilla's JavaScript engine written
in C/C++.")
    (license license:mpl2.0))) ; and others for some files

(define-public mozjs-128
  (package
    (inherit mozjs)
    (version "128.3.1")
    (source (origin
              (method url-fetch)
              ;; TODO: Switch to IceCat source once available on ftp.gnu.org.
              (uri (string-append "https://ftp.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "1a3h7p7126pxzpidb1lqckvhfh1had805mai4l96mnc878phbx61"))))
    (arguments
     (substitute-keyword-arguments (package-arguments mozjs)
       ((#:configure-flags flags)
        #~(delete "--enable-rust-simd" #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
          (replace 'adjust-tests
            (lambda _
              (with-directory-excursion "../js/src/tests"
                (substitute* "shell/os.js"
                  ;; FIXME: Why does the killed process have an exit status?
                  ((".*killed process should not have exitStatus.*")
                   ""))

                ;; This was fixed sometime between 102.15.1 and 115.11.0.
                ;; These tests are supposed to be skipped on all 64-bit systems.
                #$@(if (target-riscv64?)
                       #~((substitute* '("non262/Array/regress-157652.js"
                                         "non262/regress/regress-422348.js")
                            (("mips64") "mips64|riscv64")))
                       #~())

                ;; FIXME: An one-hour difference is produced after DST
                ;; starting in the timezone the test suite uses.
                (delete-file "non262/Date/15.9.5.7.js")

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
                (delete-file "non262/Intl/available-locales-resolved.js")

                ;;; Since 115:
                ;; Mismatching array lengths
                (delete-file "non262/Intl/supportedValuesOf-timeZones-canonical.js")
                ;; FIXME: got "America/Santa_Isabel", expected "America/Tijuana":
                ;; America/Santa_Isabel -> America/Tijuana
                (delete-file "non262/Intl/DateTimeFormat/timeZone_backward_links.js")
                ;; TODO: tzdata 2024a expected – find a way to regenerate
                ;; these generated tests
                (delete-file "non262/Intl/DateTimeFormat/timeZone_version.js")

                ;; FIXME: got "\uD840\uDDF2", expected "\u5047"
                (delete-file "non262/Intl/Collator/implicithan.js")
                ;; FIXME: got "\uD840\uDDF2", expected "\u3467"
                (delete-file "non262/Intl/Collator/big5han-gb2312han.js")

                ;; Since 128:
                ;; FIXME: got (void 0), expected "GMT"
                (delete-file "non262/Intl/DateTimeFormat/formatRange-timeZoneName-matches-format.js")
                ;; FIXME: got 7, expected 9: parts count mismatch
                (delete-file "non262/Intl/DateTimeFormat/formatRange-timeZone-offset.js")
                (delete-file "non262/Intl/DateTimeFormat/formatRange-timeZoneName.js"))))))))
    (inputs
     (list icu4c-73 readline zlib))))

(define-public mozjs-115
  (package
    (inherit mozjs)
    (version "115.26.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "0xvwk3vkbxnybpi3gwk48nxffg44lbv58mbk2xq6cz50ffq0k5k2"))))
    (arguments
     (substitute-keyword-arguments (package-arguments mozjs)
       ((#:configure-flags flags)
        #~(delete "--enable-rust-simd" #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'adjust-tests
              (lambda _
                (with-directory-excursion "../js/src/tests"
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
                (delete-file "non262/Intl/available-locales-resolved.js")

                ;;; Since 115:
                ;; Mismatching array lengths
                (delete-file "non262/Intl/supportedValuesOf-timeZones-canonical.js")
                ;; FIXME: got "America/Santa_Isabel", expected "America/Tijuana":
                ;; America/Santa_Isabel -> America/Tijuana
                (delete-file "non262/Intl/DateTimeFormat/timeZone_backward_links.js")
                ;; TODO: tzdata 2024a expected – find a way to regenerate
                ;; these generated tests
                (delete-file "non262/Intl/DateTimeFormat/timeZone_version.js")

                ;; FIXME: got "\uD840\uDDF2", expected "\u5047"
                (delete-file "non262/Intl/Collator/implicithan.js")
                ;; FIXME: got "\uD840\uDDF2", expected "\u3467"
                (delete-file "non262/Intl/Collator/big5han-gb2312han.js"))))

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
    (inputs
     (list icu4c-73 readline zlib))))

(define computed-origin-method (@@ (guix packages) computed-origin-method))

(define mozilla-compare-locales
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/mozilla/compare-locales")
          (commit "RELEASE_9_0_4")))    ;use the latest release
    (file-name "mozilla-compare-locales")
    (sha256 (base32 "13qn983j0pgs2550fgd5gvnl4lq6ywqjvgbyx850jwg79w8b0ifz"))))

(define mozilla-l10n
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://github.com/mozilla-l10n/firefox-l10n")
           ;; Use the revision specified in the
           ;; browser/locales/l10n-changesets.json file of the used firefox
           ;; source (all the languages normally use the same revision).
          (commit "64046fdc97c1b1886a479dead61e6dc5428ae6e6")))
    (file-name "mozilla-l10n")
    (sha256 (base32 "1rvk1m8bjnk9x61663s7bhgax6ig37v9m1d64g89fk1qwsk3djhh"))))

(define (format-locales all-locales-file)
  "Format a Scheme list of all the locales string found in ALL-LOCALES-FILE.
In the case of Thunderbird, that file is comm/mail/locales/all-locales, while
in the case of Firefox, it is browser/locales/all-locales."
  (pretty-print (string-split
                 (string-trim-right
                  (call-with-input-file all-locales-file
                    get-string-all))
                 #\newline)))

;;; To regenerate, use the above `format-locales' procedure.
(define %icecat-locales
  '("ach"
    "af"
    "an"
    "ar"
    "ast"
    "az"
    "be"
    "bg"
    "bn"
    "bo"
    "br"
    "brx"
    "bs"
    "ca"
    "ca-valencia"
    "cak"
    "ckb"
    "cs"
    "cy"
    "da"
    "de"
    "dsb"
    "el"
    "en-CA"
    "en-GB"
    "eo"
    "es-AR"
    "es-CL"
    "es-ES"
    "es-MX"
    "et"
    "eu"
    "fa"
    "ff"
    "fi"
    "fr"
    "fur"
    "fy-NL"
    "ga-IE"
    "gd"
    "gl"
    "gn"
    "gu-IN"
    "he"
    "hi-IN"
    "hr"
    "hsb"
    "hu"
    "hy-AM"
    "hye"
    "ia"
    "id"
    "is"
    "it"
    "ja"
    "ja-JP-mac"
    "ka"
    "kab"
    "kk"
    "km"
    "kn"
    "ko"
    "lij"
    "lo"
    "lt"
    "ltg"
    "lv"
    "meh"
    "mk"
    "ml"
    "mr"
    "ms"
    "my"
    "nb-NO"
    "ne-NP"
    "nl"
    "nn-NO"
    "oc"
    "pa-IN"
    "pl"
    "pt-BR"
    "pt-PT"
    "rm"
    "ro"
    "ru"
    "sat"
    "sc"
    "scn"
    "sco"
    "si"
    "sk"
    "skr"
    "sl"
    "son"
    "sq"
    "sr"
    "sv-SE"
    "szl"
    "ta"
    "te"
    "tg"
    "th"
    "tl"
    "tr"
    "trs"
    "uk"
    "ur"
    "uz"
    "vi"
    "wo"
    "xh"
    "zh-CN"
    "zh-TW"))

(define %icecat-base-version "140.6.0")
(define %icecat-version (string-append %icecat-base-version "-gnu1"))
(define %icecat-build-id "20251211000000") ;must be of the form YYYYMMDDhhmmss

;; 'icecat-source' is a "computed" origin that generates an IceCat tarball
;; from the corresponding upstream Firefox ESR tarball, using the 'makeicecat'
;; script from the upstream IceCat project.
(define icecat-source
  (let* ((major-version (first  (string-split %icecat-base-version #\.)))
         (minor-version (second (string-split %icecat-base-version #\.)))
         (sub-version   (third  (string-split %icecat-base-version #\.)))

         (upstream-firefox-version (string-append %icecat-base-version "esr"))
         (upstream-firefox-source
          (origin
            (method url-fetch)
            (uri (string-append
                  "https://ftp.mozilla.org/pub/firefox/releases/"
                  upstream-firefox-version "/source/"
                  "firefox-" upstream-firefox-version ".source.tar.xz"))
            (sha256
             (base32
              "1jadc0ynq49zcqd7ix9nxlrqy5gfhm61p7yliwy068bma2mwjdbc"))))

         (gnuzilla-commit "f4e50b9a4d5384ce2e860133bf0beaaccbf19b46")
         (gnuzilla-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://git.savannah.gnu.org/gnuzilla.git")
                  (commit gnuzilla-commit)))
            (file-name (git-file-name "gnuzilla"
                                      (string-take gnuzilla-commit 8)))
            (sha256
             (base32
              "0zjzw7blal3niyzyvw33w2cg906pnjbmvf7gsgas2in9xy3rlix6"))))

         ;; 'search-patch' returns either a valid file name or #f, so wrap it
         ;; in 'assume-valid-file-name' to avoid 'local-file' warnings.
         (makeicecat-patch
          (local-file (assume-valid-file-name
                       (search-patch "icecat-makeicecat.patch")))))

    (origin
      (method computed-origin-method)
      (file-name (string-append "icecat-" %icecat-version ".tar.zst"))
      (sha256 #f)
      (uri
       (delay
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils))
              (let ((firefox-dir
                     (string-append "firefox-" #$%icecat-base-version))
                    (icecat-dir
                     (string-append "icecat-" #$%icecat-version)))

                (set-path-environment-variable
                 "PATH" '("bin")
                 (list #+python
                       #+(canonical-package bash)
                       #+(canonical-package coreutils)
                       #+(canonical-package findutils)
                       #+(canonical-package patch)
                       #+(canonical-package xz)
                       #+(canonical-package zstd)
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
                     (lambda (locale)
                       (let ((locale-dir (string-append #+mozilla-l10n "/"
                                                        locale)))
                         (format #t "  ~a~%" locale)
                         (force-output)
                         (copy-recursively locale-dir locale
                                           #:log (%make-void-port "w"))
                         (for-each make-file-writable (find-files locale))
                         (with-directory-excursion locale
                           (mkdir-p "browser/chrome/browser/preferences")
                           (call-with-output-file "browser/chrome/browser/\
preferences/advanced-scripts.dtd"
                             (lambda (port) #f)))))
                     '#+%icecat-locales)
                    (copy-recursively #+mozilla-compare-locales
                                      "compare-locales"
                                      #:log (%make-void-port "w"))
                    (delete-file "compare-locales/.gitignore")))

                (format #t "Running makeicecat script...~%")
                (force-output)
                (invoke "bash" "/tmp/gnuzilla/makeicecat")

                (format #t "Packing IceCat source tarball...~%")
                (force-output)
                (setenv "ZSTD_NBTHREADS" (number->string (parallel-job-count)))
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

(define-public icecat-minimal
  (package
    (name "icecat-minimal")
    (version %icecat-version)
    (source icecat-source)
    (build-system mozilla-build-system)
    (inputs
     (list alsa-lib
           bash-minimal
           bzip2
           cups
           dbus-glib
           gdk-pixbuf
           glib
           gtk+
           ;; UNBUNDLE-ME! graphite2
           cairo
           pango
           freetype
           font-dejavu
           ;; UNBUNDLE-ME! harfbuzz
           libcanberra
           libgnome
           libjpeg-turbo
           libpng-apng-next
           ;; UNBUNDLE-ME! libogg
           ;; UNBUNDLE-ME! libtheora ; wants theora-1.2, not yet released
           ;; UNBUNDLE-ME! libvorbis
           libxft
           libevent
           libwebp
           libxinerama
           libxscrnsaver
           libxcomposite
           libxt
           libffi
           ;; See <https://bugzilla.mozilla.org/show_bug.cgi?id=1962139>
           ffmpeg-6
           libvpx
           icu4c-77
           pixman
           pulseaudio
           mesa
           pciutils
           mit-krb5
           hunspell
           libnotify
           nspr
           ;;nss-rapid            ;pending resolution of
                                  ;<https://codeberg.org/guix/guix/issues/661>
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
      ;; XXX TODO: Adapt these patches to current IceCat.
      ;; ("icecat-avoid-bundled-libraries.patch"
      ;;  ,(search-patch "icecat-avoid-bundled-libraries.patch"))
      ;; ("icecat-use-system-graphite2+harfbuzz.patch"
      ;;  ,(search-patch "icecat-use-system-graphite2+harfbuzz.patch"))
      ;; ("icecat-use-system-media-libs.patch"
      ;;  ,(search-patch "icecat-use-system-media-libs.patch"))
      rust
      `(,rust "cargo")
      rust-cbindgen
      llvm-20
      clang-20
      perl
      node-lts
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
         "--disable-bootstrap"     ;do not attempt to fetch toolchain binaries
         "--disable-fhs"
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
         ;; The --disable-eme option is not available on aarch64.
         #$(if (target-aarch64?) "" "--disable-eme")

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

         ;; TODO: Add support for wasm sandboxed libraries.
         "--without-wasm-sandboxed-libraries"

         ;; Avoid bundled libraries.
         "--enable-system-pixman"
         "--with-system-ffi"
         "--with-system-icu"
         "--with-system-jpeg"           ;must be libjpeg-turbo
         "--with-system-libevent"
         "--with-system-libvpx"
         "--with-system-nspr"
         ;; "--with-system-nss"
         ;;   (pending resolution of <https://codeberg.org/guix/guix/issues/661>)
         "--with-system-png"            ;must be libpng-apng
         "--with-system-webp"
         "--with-system-zlib"

         ;; TODO: To be implemented.
         ;; UNBUNDLE-ME! "--with-system-bz2"
         ;; UNBUNDLE-ME! "--with-system-ogg"
         ;; UNBUNDLE-ME! "--with-system-vorbis"
         ;; UNBUNDLE-ME! "--with-system-theora" ; wants theora-1.2, not yet released
         ;; UNBUNDLE-ME! "--with-system-harfbuzz"
         ;; UNBUNDLE-ME! "--with-system-graphite2"
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
                  ,@%default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'apply-guix-specific-patches
            (lambda _
              (for-each
               (lambda (file) (invoke "patch" "--force" "-p1" "-i" file))
               '(#$(local-file
                    (search-patch "icecat-compare-paths.patch"))
                 #$(local-file
                    (search-patch "icecat-use-system-wide-dir.patch"))
                 #$(local-file
                    (search-patch "icecat-fhs-configure-option.patch"))
                 #$(local-file
                    (search-patch "icecat-adjust-mozilla-desktop.patch"))))))
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
                          ;; FIXME: Some of the bundled NSS sources are used
                          ;; to build third_party/prio.
                          ;;"security/nss"
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
                          "ipc/chromium/src/third_party/libevent"
                          "js/src/ctypes/libffi"
                          "media/libjpeg"
                          "media/libvpx"
                          "modules/freetype2"

                          ;; UNBUNDLE-ME! "modules/zlib"
                          ;; UNBUNDLE-ME! "media/libogg"
                          ;; UNBUNDLE-ME! "media/libvorbis"
                          ;; UNBUNDLE-ME! "media/libtheora" ; wants theora-1.2, not yet released
                          ;; UNBUNDLE-ME! "media/libtremor"
                          ;; UNBUNDLE-ME! "gfx/harfbuzz"
                          ;; UNBUNDLE-ME! "gfx/graphite2"
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
              ;; Remove --frozen flag from cargo invocation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                (("args.append\\(\"--frozen\"\\)") "pass"))
              (substitute* "config/makefiles/rust.mk"
                (("cargo_build_flags \\+= --frozen") ""))))
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

                ;; WM_CLASS (default is "$MOZ_APP_NAME-$MOZ_UPDATE_CHANNEL").
                (setenv "MOZ_APP_REMOTINGNAME" "Icecat")

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
                     ;; mach will use a wide parallel build if possible by
                     ;; default, so reign it in if requested.
                     `(,(string-append
                         "-j" (number->string (if parallel-build?
                                                  (parallel-job-count)
                                                  1)))
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
                            (string-append #$output "/bin"))
              ;; Install a policies.json file as an extra step to ensure
              ;; IceCat does not call home.  The available policies can be
              ;; found at <https://mozilla.github.io/policy-templates/>.

              ;; TODO: Disable remote settings feature when it becomes
              ;; possible to do so (see:
              ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=1988070>).
              (let ((policies.json (string-append
                                    #$output
                                    "/lib/icecat/distribution/policies.json")))
                (mkdir-p (dirname policies.json))
                (call-with-output-file policies.json
                  (lambda (p)
                    (format p "\
{
  \"policies\": {
    \"DisableFirefoxAccounts\": true,
    \"DisableTelemetry\": true,
    \"DisablePocket\": true
  }
}~%"))))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib (string-append #$output "/lib"))
                     (gtk #$(this-package-input "gtk+"))
                     (gtk-share (string-append gtk "/share"))
                     (ld-libs '#$(cons
                                  (file-append
                                   (this-package-input "libcanberra")
                                   "/lib/gtk-3.0/modules")
                                  (map (lambda (label)
                                         (file-append (this-package-input label) "/lib"))
                                       '("libpng-apng"
                                         "libxscrnsaver"
                                         "mesa"
                                         "pciutils"
                                         "mit-krb5"
                                         "eudev"
                                         "pulseaudio"
                                         ;; For the integration of native notifications
                                         ;; (same reason as icedove)
                                         "libnotify")))))
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
              (let* ((desktop-file (string-append "toolkit/mozapps/installer"
                                                  "/linux/rpm/mozilla.desktop"))
                     (applications (string-append #$output "/share/applications")))
                (substitute* desktop-file
                  (("@MOZ_APP_NAME@")
                   "icecat")
                  (("^Exec=icecat")
                   (string-append "Exec=" #$output "/bin/icecat"))
                  (("@MOZ_APP_DISPLAYNAME@")
                   "GNU IceCat")
                  (("@MOZ_APP_REMOTINGNAME@")
                   "Icecat"))
                (mkdir-p applications)
                (copy-file desktop-file
                           (string-append applications "/icecat.desktop")))))
          (add-after 'install-desktop-entry 'install-icons
            (lambda _
              (with-directory-excursion "browser/branding/unofficial"
                (for-each
                 (lambda (file)
                   (let* ((size (string-filter char-numeric? file))
                          (icons (string-append #$output "/share/icons/hicolor/"
                                                size "x" size "/apps")))
                     (mkdir-p icons)
                     (copy-file file (string-append icons "/icecat.png"))))
                 '("default16.png" "default22.png" "default24.png"
                   "default32.png" "default48.png" "default256.png"
                   "content/icon64.png" "mozicon128.png" ))))))))
    (native-search-paths
     (list (search-path-specification
             (variable "MOZILLA_SYSTEM_DIR")
             (separator #f)             ;single entry
             (files '("lib/icecat")))))
    (home-page "https://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.  This package also includes the
@command{geckodriver} command, which can be useful for automated web
testing.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))

(define %icedove-build-id "20251112000000") ;must be of the form YYYYMMDDhhmmss
;;; See <https://product-details.mozilla.org/1.0/thunderbird_versions.json>
;;; for the source of truth regarding Thunderbird releases.
(define %icedove-version "140.5.0")

;; Provides the "comm" folder which is inserted into the icecat source.
;; Avoids the duplication of Icecat's source tarball.  Pick the changeset that
;; matches the most recent tag of the form 'THUNDERBIRD_140_2_0esr_RELEASE'.
(define thunderbird-comm-source
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url "https://hg.mozilla.org/releases/comm-esr140")
          (changeset "6a3011b7161c6f3a36d5116f2608d51b19fb4d58")))
    (file-name (string-append "thunderbird-" %icedove-version "-checkout"))
    (sha256
     (base32
      "0n94yznvvha12xri2m9p0jqd7hwcb8s7lgakfhvz3brp3ivqphn3"))
    (patches (search-patches "icedove-observer-fix.patch"))))

;;; To regenerate, see the `format-locales' helper defined above.
(define %icedove-locales
  '("af"
    "ar"
    "ast"
    "be"
    "bg"
    "br"
    "ca"
    "cak"
    "cs"
    "cy"
    "da"
    "de"
    "dsb"
    "el"
    "en-CA"
    "en-GB"
    "es-AR"
    "es-ES"
    "es-MX"
    "et"
    "eu"
    "fi"
    "fr"
    "fy-NL"
    "ga-IE"
    "gd"
    "gl"
    "he"
    "hr"
    "hsb"
    "hu"
    "hy-AM"
    "id"
    "is"
    "it"
    "ja"
    "ja-JP-mac"
    "ka"
    "kab"
    "kk"
    "ko"
    "lt"
    "lv"
    "mk"
    "ms"
    "nb-NO"
    "nl"
    "nn-NO"
    "pa-IN"
    "pl"
    "pt-BR"
    "pt-PT"
    "rm"
    "ro"
    "ru"
    "sk"
    "sl"
    "sq"
    "sr"
    "sv-SE"
    "th"
    "tr"
    "uk"
    "uz"
    "vi"
    "zh-CN"
    "zh-TW"))

(define thunderbird-comm-l10n
  ;; The commit to use can be found in the mail/locales/l10n-changesets.json
  ;; file in Thunderbird's source (e.g.:
  ;; <https://hg-edge.mozilla.org/releases/comm-esr140/file/efb07defaa2d56105675dc1d936af581ebfd8ffa/mail/locales/l10n-changesets.json>)
  (let* ((commit "b6fd3d6c75ba35d91fe131a654df76ca86f35ac5")
         (revision "0")
         (version (git-version %icedove-version revision commit)))
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/thunderbird/thunderbird-l10n")
             (commit commit)))
      (file-name (git-file-name "thunderbird-l10n" version))
      (sha256
       (base32
        "0n4df6kv70a6mxxsqwc83nhj8vl7acv9bcbf07nkcsjjxh3szvqc")))))

(define icedove-source
  (let ((name (string-append "icedove-" %icedove-version)))
    (origin
      (method computed-origin-method)
      (file-name (string-append name ".tar.zst"))
      (sha256 #f)
      (uri
       (delay
         (with-imported-modules (source-module-closure '((guix build utils)))
           #~(begin
               (use-modules (guix build utils)
                            (sxml simple))

               (set-path-environment-variable
                "PATH" '("bin")
                (list #+(canonical-package tar)
                      #+(canonical-package zstd)))

               ;; Extract the base Icecat tarball, renaming its top-level
               ;; directory.
               (invoke "tar" "--transform" (string-append "s,[^/]*," #$name ",")
                       "-xf" #$icecat-source)
               (chdir #$name)

               ;; Merge the Thunderdbird localization data.
               (copy-recursively #$thunderbird-comm-l10n "l10n")

               ;; Add the Thunderbird-specific "comm" directory.
               (mkdir "comm")
               (copy-recursively #$thunderbird-comm-source "comm")
               (for-each make-file-writable (find-files "comm"))
               (delete-file "sourcestamp.txt")

               ;; Adjust the application name.
               (substitute* "comm/mail/moz.configure"
                 (("\"MOZ_APP_NAME\", \"thunderbird\"")
                  "\"MOZ_APP_NAME\", \"icedove\""))

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
                 (substitute* '("MailGlue.sys.mjs"
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
               (substitute* '("devtools/startup/DevToolsStartup.sys.mjs"
                              "comm/mailnews/base/prefs/content/accountUtils.js"
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
                  (string-append m ", false);"))
                 (("(pref\\(\"services.settings.server\").*" _ m)
                  (string-append m ", \"\");"))

                 ;; XXX: The autoDisableScopes is tweaked by the makeicecat
                 ;; script, but it doesn't know about Thunderbird.  This is
                 ;; necessary to allow picking up the extensions found in the
                 ;; system global application directory, such as the language
                 ;; packs.
                 (("\"extensions.autoDisableScopes\", 15")
                  "\"extensions.autoDisableScopes\", 3")

                 ;; Set the default locale to that of the operating system.
                 ((".*extensions.autoDisableScopes.*" anchor)
                  (string-append anchor
                                 "pref(\"intl.locale.requested\", \"\");\n")))

               ;; Fix more discrepancies caused by the fact that the
               ;; makeicecat script didn't run on the Thunderbird sources.
               (substitute* '("Cargo.lock"
                              "comm/rust/Cargo.lock"
                              "comm/rust/Cargo.toml"
                              "gfx/qcms/Cargo.toml"
                              "toolkit/library/rust/shared/Cargo.toml")
                 (("IceCatGraphics") "FirefoxGraphics")
                 (("firefox-on-glean") "icecat-on-glean"))

               ;; Step out of the directory and create the tarball.
               (chdir "..")
               (format #t "Packing Icedove source tarball...~%")
               (force-output)
               (setenv "ZSTD_NBTHREADS" (number->string (parallel-job-count)))
               (invoke "tar" "cfa" #$output
                       "--mtime=@315619200" ;1980-01-02 UTC
                       "--owner=root:0"
                       "--group=root:0"
                       "--sort=name"
                       #$name))))))))

(define-public icedove-minimal
  (package
    (name "icedove-minimal")
    (version %icedove-version)
    (source icedove-source)
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
                  ,@%default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-verify-vendored-rust-dependencies
            (lambda _
              (substitute* "comm/python/rocbuild/rocbuild/rust.py"
                (("result = check_vendored_dependencies\\(topsrcdir)")
                 "sys.exit(0)"))))
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
                            "build"
                            ;; Thunderbird-specific.
                            "comm")))))
          (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
            (lambda _
              ;; Remove --frozen flag from cargo invocation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                (("args.append\\(\"--frozen\"\\)") "pass"))
              (substitute* "config/makefiles/rust.mk"
                (("cargo_build_flags \\+= --frozen") ""))))
          ;; Fixes issue where each installation directory generates its own
          ;; profile (see:
          ;; https://trac.torproject.org/projects/tor/ticket/31457).
          (add-after 'patch-source-shebangs 'fix-profile-setting
            (lambda _
              (substitute* "comm/mail/moz.configure"
                (("\"MOZ_DEDICATED_PROFILES\", True")
                 "\"MOZ_DEDICATED_PROFILES\", False"))))
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
            (lambda* (#:key native-inputs inputs configure-flags
                      #:allow-other-keys)
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
                (setenv "MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE" "system")
                (setenv "GUIX_PYTHONPATH"
                        (string-append (getcwd)
                                       "/obj-x86_64-pc-linux-gnu/_virtualenvs/build"))

                (setenv "MOZ_BUILD_DATE" #$%icedove-build-id) ; avoid timestamp
                (setenv "MOZ_APP_NAME" "icedove")
                ;; WM_CLASS (default is "$MOZ_APP_NAME-$MOZ_UPDATE_CHANNEL").
                (setenv "MOZ_APP_REMOTINGNAME" "Icedove")

                (setenv "LDFLAGS" (string-append "-Wl,-rpath=" #$output
                                                 "/lib/icedove"))
                (mkdir-p (string-append (getcwd) "/builddir"))
                (with-output-to-file mozconfig
                  (lambda ()
                    (display
                     (string-append
                      "ac_add_options --allow-addon-sideload\n"
                      "ac_add_options --with-unsigned-addon-scopes=app,system\n"
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
                      "ac_add_options --enable-rust-simd\n"
                      "ac_add_options --enable-strip\n"
                      "ac_add_options --enable-system-ffi\n"
                      "ac_add_options --enable-system-pixman\n"
                      "ac_add_options --prefix=" #$output "\n"
                      "ac_add_options --with-clang-path="
                      (search-input-file (or native-inputs inputs)
                                         "bin/clang") "\n"
                      "ac_add_options --with-distribution-id=org.gnu\n"
                      "ac_add_options --with-libclang-path="
                      #$(this-package-native-input "clang") "/lib\n"
                      "ac_add_options --with-system-bz2\n"
                      "ac_add_options --with-system-icu\n"
                      "ac_add_options --with-system-jpeg\n"
                      "ac_add_options --with-system-libevent\n"
                      "ac_add_options --with-system-nspr\n"
                      "ac_add_options --with-system-nss\n"
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
                            Exec=~a/bin/icedove %u~@
                            Icon=icedove~@
                            GenericName=Mail/News Client~@
                            Categories=Network;Email;~@
                            Terminal=false~@
                            StartupNotify=true~@
                            MimeType=x-scheme-handler/mailto;~@
                            Type=Application~@
                            Actions=ComposeMessage;~@
                            StartupWMClass=Icedove;~@
                            [Desktop Action ComposeMessage]~@
                            Name=Write new message~@
                            Exec=~@*~a/bin/icedove -compose~%"
                            #$output))))))
          (add-after 'install-desktop-file 'install-icons
            ;; TODO: Use actual Icedove branding icons (currently the stock
            ;; Thunderbird icon is used).
            (lambda _
              (with-directory-excursion "comm/mail/branding/thunderbird"
                (for-each
                 (lambda (file)
                   (let* ((size (string-filter char-numeric? file))
                          (icons (string-append #$output "/share/icons/hicolor/"
                                                size "x" size "/apps")))
                     (mkdir-p icons)
                     (copy-file file (string-append icons "/icedove.png"))))
                 '("default16.png" "default22.png" "default24.png"
                   "default32.png" "default48.png" "default256.png")))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib (string-append #$output "/lib"))
                     (gpgme #$(this-package-input "gpgme"))
                     (gpgme-lib (string-append gpgme "/lib"))
                     (gtk #$(this-package-input "gtk+"))
                     (gtk-share (string-append gtk "/share"))
                     (pulseaudio #$(this-package-input "pulseaudio"))
                     (pulseaudio-lib (string-append pulseaudio "/lib"))
                     (eudev #$(this-package-input "eudev"))
                     (eudev-lib (string-append eudev "/lib"))
                     ;; For the integration of native notifications (same reason as icecat)
                     (libnotify #$(this-package-input "libnotify"))
                     (libnotify-lib (string-append libnotify "/lib"))
                     (mesa #$(this-package-input "mesa"))
                     (mesa-lib (string-append mesa "/lib"))
                     (pciutils #$(this-package-input "pciutils"))
                     (pciutils-lib (string-append pciutils "/lib"))
                     (libva #$(this-package-input "libva"))
                     (libva-lib (string-append libva "/lib"))
                     (libotr #$(this-package-input "libotr"))
                     (libotr-lib (string-append libotr "/lib")))
                (wrap-program (car (find-files lib "^icedove$"))
                  `("XDG_DATA_DIRS" prefix (,gtk-share))
                  `("LD_LIBRARY_PATH" prefix
                    ( ,pulseaudio-lib ,eudev-lib ,libnotify-lib ,gpgme-lib
                      ,mesa-lib ,libva-lib ,libotr-lib ,pciutils-lib)))))))))
    (inputs
     (list alsa-lib
           bash-minimal
           bzip2
           cairo
           cups
           dbus-glib
           eudev
           ffmpeg
           freetype
           gdk-pixbuf
           glib
           gpgme
           gtk+
           hunspell
           icu4c-77
           libcanberra
           libevent
           libffi
           libgnome
           libjpeg-turbo
           libnotify
           libotr
           libpng-apng
           libva
           libvpx
           libxcomposite
           libxft
           libxinerama
           libxscrnsaver
           libxt
           mesa
           mit-krb5
           nspr
           nss-rapid
           pango
           pciutils
           pixman
           pulseaudio
           sqlite
           startup-notification
           unzip
           zip
           zlib))
    (native-inputs
     (list clang-20
           llvm-20
           m4
           nasm
           node-lts
           perl
           pkg-config
           python-wrapper
           rust
           `(,rust "cargo")
           rust-cbindgen
           which
           yasm))
    (home-page "https://www.thunderbird.net")
    (synopsis "Rebranded Mozilla Thunderbird email client")
    (description
     "This package provides an email client built based on Mozilla
Thunderbird.  It supports email, news feeds, chat, calendar and contacts.")
    (license license:mpl2.0)))

(define (make-l10n-package project version source locales)
  "Return a package for PROJECT, a symbol (either icecat or icedove), with
their corresponding VERSION, SOURCE and LOCALES variables."
  (unless (member project '(icecat icedove))
    (error "only icecat or icedove components are currently supported"))

  (let ((name (if (eq? 'icecat project)
                  "IceCat"
                  "Icedove")))
    (package
      (name (format #f "~a-l10n" project))
      (version version)
      (source source)
      (outputs (cons "out" locales))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 format)
                    (ice-9 ftw)
                    (srfi srfi-1)
                    (srfi srfi-26))
        #:tests? #f                     ;no tests, this is data
        #:phases
        #~(modify-phases %standard-phases
            (delete 'bootstrap)
            (delete 'install)
            (replace 'configure
              (lambda _
                ;; The following configuration is inspired by guidance at
                ;; https://firefox-source-docs.mozilla.org/build/buildsystem/locales.html.
                (call-with-output-file ".mozconfig"
                  (lambda (p)
                    (format p "~{~a~%~}"
                            (list (if (eq? 'icecat '#$project)
                                      "ac_add_options --enable-project=browser"
                                      "ac_add_options --enable-project=comm/mail")
                                  "ac_add_options --disable-compile-environment"
                                  (string-append
                                   "ac_add_options --with-l10n-base="
                                   (getcwd) "/l10n")
                                  ;; Hack, otherwise the build system throws:
                                  ;; 'RuntimeError: File "brand.dtd" not found'.
                                  "ac_add_options --enable-official-branding"
                                  "mk_add_options MOZ_OBJDIR=obj"))))
                (setenv "CONFIG_SHELL" (which "bash"))
                (setenv "MOZBUILD_STATE_PATH"
                        (string-append (getcwd) "/mach_state"))
                (setenv "MOZCONFIG" (string-append (getcwd) "/.mozconfig"))
                (setenv "MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE" "system")
                (setenv "BUILD_BACKENDS" "FasterMake,RecursiveMake")))
            (replace 'build             ;build and install data files
              (lambda* (#:key outputs #:allow-other-keys)
                (define (find-file dir name)
                  (let ((files (find-files dir name)))
                    (when (null? files)
                      (error "could not find file in dir" name dir))
                    (car files)))

                ;; Register "tb_common" as a valid site, to please the mach
                ;; virtualenv machinery (see:
                ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=1986420>).
                (substitute* "python/mach/mach/site.py"
                  (("\"mach\", \"build\", \"common\"" all)
                   (string-append all ", \"tb_common\"")))

                (for-each
                 (lambda (l)
                   (let* ((out (assoc-ref outputs l))
                          ;; The older lib/$project/distribution/extensions
                          ;; directory is deprecated.  Use the newer app-global
                          ;; directory, which is lib/$project/extensions.
                          (ext-dir-prefix
                           (format
                            #f "lib/~a/~:[~;browser/~]extensions"
                            '#$project (eq? 'icecat '#$project)))
                          (all-ext (string-append #$output "/" ext-dir-prefix))
                          (ext-dir (string-append out "/" ext-dir-prefix))
                          ;; XXX: Because Icedove doesn't have a makeicedove
                          ;; script that substitutes all the Thunderbird
                          ;; references to Icedove, the MOZ_LANGPACK_EID
                          ;; defined in comm/mail/locales/Makefile.in uses
                          ;; 'thunderbird' in its ID extension rather than
                          ;; 'icedove'.
                          (name (format #f "langpack-~a@~a.mozilla.org.xpi"
                                        l (if (eq? 'icedove '#$project)
                                              'thunderbird
                                              '#$project))))
                     (format #t "processing locale `~a'...~%" l)
                     ;; TODO: Revert to use 'invoke' here, after
                     ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=1988069>
                     ;; is fixed.
                     (system* "./mach" "build" (string-append "langpack-" l))
                     (mkdir-p ext-dir)
                     (let ((xpi (find-file "obj" (string-append
                                                  "\\." l "\\.langpack\\.xpi$"))))
                       (copy-file xpi (string-append ext-dir "/" name))
                       ;; Symlink to the main output so that a user can
                       ;; install all of the language packs at once.
                       (mkdir-p all-ext)
                       (symlink (string-append ext-dir "/" name)
                                (string-append all-ext "/" name)))))
                 '#$locales))))))
      (native-inputs
       (list m4
             perl
             python
             python-aiohttp
             python-async-timeout
             python-dateutil
             node-lts
             unzip))
      (home-page "https://www.mozilla.org/")
      (synopsis (string-append "Language localization data for " name))
      (description (string-append "This package contains the various language
localization data files (language pack extensions) for " name ".  The
individual localization packages can be installed by using the output
associated with their name."))
      (license license:mpl2.0))))

(define-public icecat-l10n
  (make-l10n-package 'icecat %icecat-version icecat-source %icecat-locales))

(define-public icedove-l10n
  (make-l10n-package 'icedove %icedove-version icedove-source %icedove-locales))

;;; This hack exists because there's no way to configure extra extension
;;; search paths for IceCat or Icedove.  The global extensions directory is
;;; constructed relatively to the executable file name.
(define (make-mozilla-with-l10n project base l10n-package)
  "Return a package definition for PROJECT (a symbol such as 'icecat or
'icedove) that combines the BASE package with L10N-PACKAGE."

  (unless (member project '(icecat icedove))
    (error "only icecat or icedove components are currently supported"))

  (let ((name (symbol->string project))
        (icecat? (eq? 'icecat project)))
    (package
      (inherit base)
      (name (symbol->string project))
      ;; Use the copy-build-system, as it provides the necessary UTF-8 locales
      ;; support.
      (build-system copy-build-system)
      (arguments
       (list
        #:imported-modules `(,@%copy-build-system-modules
                             (guix build union))
        #:modules '((guix build copy-build-system)
                    (guix build union)
                    (guix build utils))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'install
              (lambda _
                (union-build #$output (list #$base #$l10n-package)
                             #:create-all-directories? #t)

                (define* (expose name #:optional (proc copy-file)
                                 #:key (source #$base))
                  (let ((dest (string-append #$output "/" name)))
                    (mkdir-p (dirname dest))
                    (proc (string-append source "/" name) dest)))

                (let ((wrapper (string-append "lib/" #$name "/" #$name))
                      (real-binary (string-append "lib/" #$name "/." #$name
                                                  "-real"))
                      (desktop-file (string-append "share/applications/"
                                                   #$name ".desktop")))
                  ;; Copy wrapper file.
                  (delete-file (string-append #$output "/" wrapper))
                  (expose wrapper)

                  ;; Recreate bin symlink.
                  (delete-file (string-append #$output "/bin/" #$name))
                  (symlink (string-append #$output "/" wrapper)
                           (string-append #$output "/bin/" #$name))

                  ;; Copy actual binary.
                  (delete-file (string-append #$output "/" real-binary))
                  (expose real-binary)

                  ;; Copy desktop file.
                  (delete-file (string-append #$output "/" desktop-file))
                  (expose desktop-file)

                  ;; Adjust the references in the desktop file and wrapper.
                  (substitute* (list (string-append #$output "/" desktop-file)
                                     (string-append #$output "/" wrapper))
                    ((#$base) #$output)))))))))))

(define-public icecat
  (make-mozilla-with-l10n 'icecat icecat-minimal icecat-l10n))

(define-public icedove
  (make-mozilla-with-l10n 'icedove icedove-minimal icedove-l10n))

(define-public icedove/wayland
  (package
    (inherit icedove-minimal)
    (name "icedove-wayland")
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((exe (string-append #$output "/bin/icedove")))
            (mkdir-p (dirname exe))
            (call-with-output-file exe
              (lambda (port)
                (format port "#!~a
 MOZ_ENABLE_WAYLAND=1 exec ~a \"$@\""
                        #$(file-append bash-minimal "/bin/bash")
                        #$(file-append icedove-minimal "/bin/icedove"))))
            (chmod exe #o555)
            ;; Provide the manual and .desktop file.
            (copy-recursively (string-append #$icedove-minimal "/share")
                              (string-append #$output "/share"))
            (substitute* (string-append #$output
                                        "/share/applications/icedove.desktop")
              ((#$icedove-minimal) #$output))))))
    (native-inputs '())
    (inputs '())))

(define-public firefox-decrypt
  (package
    (name "firefox-decrypt")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Unode/firefox_decrypt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dnyl5z7kcgmgdgbz7vg8ha21pk9jiympvq0qc97lgdcdd8wxy0w"))))
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
           (("/usr/bin/env python3") (which "python3"))
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
