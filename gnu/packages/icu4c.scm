;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2020, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages icu4c)
  #:use-module (gnu packages)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu))

(define-public icu4c
  (package
    (name "icu4c")
    (version "71.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/unicode-org/icu/releases/download/release-"
                    (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                    "/icu4c-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    "-src.tgz"))
              (sha256
               (base32 "1gqywaqj9jmdwrng9lm6inyqmi5j2cz36db9dcqg3yk13zjyd9v7"))))
    (build-system gnu-build-system)
    (native-inputs
     (append (list python-minimal)
             (if (%current-target-system)
                 ;; When cross-compiling, this package needs a source directory
                 ;; of a native-build of itself.
                 (list icu4c-build-root)
                 '())))
    (inputs
     (list perl))
    (arguments
     (list
      #:configure-flags
      #~(list
         "--enable-rpath"
         #$@(if (%current-target-system)
                #~((string-append "--with-cross-build="
                                  #+(this-package-native-input
                                     "icu4c-build-root")))
                #~()))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-source
            (lambda _ (chdir "source")))
          (add-after 'chdir-to-source 'update-LDFLAGS
            (lambda _
              ;; Do not create a "data-only" libicudata.so because it causes
              ;; problems on some architectures (notably armhf and MIPS).
              (substitute* "config/mh-linux"
                (("LDFLAGSICUDT=-nodefaultlibs -nostdlib")
                 "LDFLAGSICUDT="))))
          #$@(if (target-riscv64?)
                 #~((add-after 'unpack 'disable-failing-test
                      ;; It is unknown why this test is failing.
                      (lambda _
                        (substitute* "source/test/intltest/numbertest_api.cpp"
                          (("(TESTCASE_AUTO\\(unitUsage\\));" all)
                           (string-append "//" all))))))
                 #~())
          (add-after 'install 'avoid-coreutils-reference
            ;; Don't keep a reference to the build tools.
            (lambda _
              (substitute* (find-files (string-append #$output "/lib/icu")
                                       "\\.inc$")
                (("INSTALL_CMD=.*/bin/install") "INSTALL_CMD=install")))))))
    (synopsis "International Components for Unicode")
    (description
     "ICU is a set of C/C++ and Java libraries providing Unicode and
globalisation support for software applications.  This package contains the
C/C++ part.")
    (license x11)
    (home-page "http://site.icu-project.org/")))

(define-public icu4c-73
  (package
    (inherit icu4c)
    (name "icu4c")
    (version "73.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/unicode-org/icu/releases/download/release-"
                    (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                    "/icu4c-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    "-src.tgz"))
              (sha256
               (base32
                "0iccpdvc0kvpww5a31k9gjkqigyz016i7v80r9zamd34w4fl6mx4"))
              (patches
               (append
                (search-patches
                 "icu4c-icu-22132-fix-vtimezone.patch"
                 "icu4c-fix-TestHebrewCalendarInTemporalLeapYear.patch")
                (origin-patches (package-source icu4c))))))))

(define-public icu4c-70
  (package
    (inherit icu4c)
    (version "70.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/unicode-org/icu/releases/download/release-"
                    (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                    "/icu4c-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    "-src.tgz"))
              (sha256
               (base32
                "1m9zgkaf5lyh65nyc6n0n5bs2f5k53nnj1ih6nskpwbvq4l5884d"))))
    (arguments
     (if (target-riscv64?)
       (substitute-keyword-arguments (package-arguments icu4c)
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'disable-failing-test
                ;; It is unknown why these tests are failing.
                (lambda _
                  (substitute* "source/test/cintltst/ucptrietest.c"
                    ((".*addTest.*") ""))
                  (substitute* "source/test/intltest/numbertest_api.cpp"
                    (("(TESTCASE_AUTO\\(unitUsage\\));" all)
                     (string-append "//" all))))))))
       (package-arguments icu4c)))))

(define-public icu4c-69
  (package
    (inherit icu4c)
    (version "69.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/unicode-org/icu/releases/download/release-"
                    (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                    "/icu4c-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    "-src.tgz"))
              (sha256
               (base32
                "0icps0avkwy5df3wwc5kybxcg63hcgk4phdh9g244g0xrmx7pfjc"))))))

(define-public icu4c-build-root
  (package
    (inherit icu4c)
    (name "icu4c-build-root")
    (arguments
     (substitute-keyword-arguments (package-arguments icu4c)
       ((#:tests? _ #f)
         #f)
        ((#:out-of-source? _ #t)
         #t)
        ((#:phases phases)
         #~(modify-phases #$phases
             (replace 'install
               (lambda _
                 (copy-recursively "../build" #$output)))))))
    (native-inputs '())))

(define-public java-icu4j
  (package
    (name "java-icu4j")
    (version "70.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/unicode-org/icu/releases/download/release-"
                    (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                    "/icu4j-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    ".tgz"))
              (sha256
               (base32 "0qrs75iyzn19kf54q55jn8wf6xjlpkrihdwqpxm39jdh2hz4cgvj"))))
    (build-system ant-build-system)
    (arguments
     `(#:make-flags
       ,#~(list
           (string-append "-Djunit.core.jar="
                          (car (find-files
                                #$(this-package-native-input "java-junit")
                                ".*.jar$")))
           (string-append "-Djunit.junitparams.jar="
                          (car (find-files
                                #$(this-package-native-input "java-junitparams")
                                ".*.jar$")))
           (string-append "-Djunit.hamcrest.jar="
                          (car (find-files
                                #$(this-package-native-input "java-hamcrest-core")
                                ".*.jar$"))))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "..")))
         (add-before 'build 'remove-ivy
           (lambda _
             ;; This target wants to download ivy and use it to download
             ;; junit.
             (substitute* "build.xml"
               (("depends=\"test-init-junit-dependency\"") ""))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out")
                                         "/share/java/")))
               (mkdir-p share)
               (install-file "icu4j.jar" share)))))))
    (native-inputs
     (list java-junit java-junitparams java-hamcrest-core))
    (home-page "http://site.icu-project.org/")
    (synopsis "International Components for Unicode")
    (description
     "ICU is a set of C/C++ and Java libraries providing Unicode and
globalisation support for software applications.  This package contains the
Java part.")
    (license x11)))

(define-public icu4c-for-skia
  ;; The current version of skia needs this exact commit
  ;; for its test dependencies.
  (let ((commit "a0718d4f121727e30b8d52c7a189ebf5ab52421f")
        (revision "0"))
    (package
      (inherit icu4c)
      (name "icu4c-for-skia")
      (version "skia")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://chromium.googlesource.com/chromium/deps/icu.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qxws2p91f6dmhy7d3967r5ygz06r88pkmpm97px067x0zzdz384"))))
      (arguments
       (list
        #:make-flags #~(list (string-append "DESTDIR=" #$output))
        #:configure-flags #~(list "--prefix=" "--exec-prefix=")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir-to-source
              (lambda _ (chdir "source")))
            (replace 'configure
              (lambda* (#:key inputs parallel-build? configure-flags
                        #:allow-other-keys)
                (setenv "CONFIG_SHELL" (which "sh"))
                (setenv "OPTS" (string-join configure-flags))
                (invoke "./runConfigureICU" "Linux/gcc"
                        "--disable-layout" "--disable-tests")))
            (add-after 'install 'install-cleanup
              (lambda* (#:key make-flags #:allow-other-keys)
                (with-directory-excursion "data"
                  (apply invoke "make" "clean" make-flags))))
            (add-after 'install-cleanup 'configure-filtered-data
              (lambda* (#:key configure-flags #:allow-other-keys)
                (setenv "OPTS" (string-join configure-flags))
                (setenv "ICU_DATA_FILTER_FILE"
                        (string-append (getcwd) "/../filters/common.json"))
                (invoke "./runConfigureICU" "Linux/gcc"
                        "--disable-layout" "--disable-tests")))
            (add-after 'configure-filtered-data 'build-filtered-data
              (lambda* (#:key parallel-build? make-flags #:allow-other-keys)
                (let ((job-count (if parallel-build?
                                     (number->string (parallel-job-count))
                                     "1")))
                  (apply invoke "make" "-j" job-count make-flags)
                  (setenv "DESTDIR" #$output)
                  (invoke "bash" "../scripts/copy_data.sh" "common"))))
            (add-after 'build-filtered-data 'install-scripts-and-data
              (lambda _
                (let* ((share (string-append #$output "/share"))
                       (scripts (string-append share "/scripts"))
                       (data (string-append share "/data/common")))
                  ;; Install scripts.
                  (mkdir-p scripts)
                  (copy-recursively "../scripts/" scripts)
                  ;; Install data.
                  (mkdir-p data)
                  (copy-recursively "./dataout/common/data/out/tmp" data)
                  (symlink (string-append data "/icudt69l.dat")
                           (string-append data "/icudtl.dat")))))
            (add-before 'check 'disable-failing-uconv-test
              (lambda _
                (substitute* "extra/uconv/Makefile.in"
                  (("check: check-local")
                   "")))))))
      (native-inputs (list cpio pkg-config python)))))
