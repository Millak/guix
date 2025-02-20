;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2016, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Sergei Trofimovich <slyfox@inbox.ru>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019, 2020, 2023, 2024 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2019 Giovanni Biscuolo <g@xelera.eu>
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019, 2020, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021, 2023 Sharlatan Hellseher <sharlatanus@mgail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Chadwain Holness <chadwainholness@gmail.com>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 Lu Hui <luhux76@gmail.com>
;;; Copyright © 2022 Pier-Hugues Pellerin <phpellerin@gmail.com>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022 Dhruvin Gandhi <contact@dhruvin.dev>
;;; Copyright © 2022, 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2023 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Greg Hogan <code@greghogan.com>
;;; Copyright © 2024 Brennan Vincent <brennan@umanwizard.com>
;;; Copyright © 2024 André Batista <nandre@riseup.net>
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

(define-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix memoization)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages bootstrap) #:select (glibc-dynamic-linker))
  #:use-module (gnu packages check)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

;; According to https://go.dev/doc/install/gccgo, gccgo-11 includes a complete
;; implementation of go-1.16 and gccgo-12 includes a complete implementation of
;; go-1.18.  Starting with go-1.5 go cannot be built without an existing
;; installation of go, so we need to use go-1.4 or gccgo.  For architectures which
;; are not supported with go-1.4 we use a version of gccgo to bootstrap them.

(define-public go-1.4
  (package
    (name "go")
    ;; The C-language bootstrap of Go:
    ;; https://golang.org/doc/install/source#go14
    (version "1.4-bootstrap-20171003")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://storage.googleapis.com/golang/"
                                  name version ".tar.gz"))
              (sha256
               (base32
                "0liybk5z00hizsb5ypkbhqcawnwwa6mkwgvjjg4y3jm3ndg5pzzl"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"
               "tests"))
    (arguments
     `(#:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:tests? #f ; Tests are run by the all.bash script.
       ,@(if (string-prefix? "aarch64-linux" (or (%current-system)
                                                 (%current-target-system)))
             '(#:system "armhf-linux")
             '())
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'patch-generated-file-shebangs 'chdir
           (lambda _
             (chdir "src")
             #t))
         (add-before 'build 'prebuild
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((gcclib (string-append (assoc-ref inputs "gcc:lib") "/lib"))
                    (ld (string-append (assoc-ref inputs "libc") "/lib"))
                    (loader (car (find-files ld "^ld-linux.+")))
                    (net-base (assoc-ref inputs "net-base"))
                    (tzdata-path
                     (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo"))
                    (output (assoc-ref outputs "out")))

               ;; Removing net/ tests, which fail when attempting to access
               ;; network resources not present in the build container.
               (for-each delete-file
                         '("net/multicast_test.go" "net/parse_test.go"
                           "net/port_test.go"))

               ;; Add libgcc to the RUNPATH.
               (substitute* "cmd/go/build.go"
                 (("cgoldflags := \\[\\]string\\{\\}")
                  (string-append "cgoldflags := []string{"
                                 "\"-rpath=" gcclib "\"}"))
                 (("ldflags := buildLdflags")
                  (string-append
                   "ldflags := buildLdflags\n"
                   "ldflags = append(ldflags, \"-r\")\n"
                   "ldflags = append(ldflags, \"" gcclib "\")\n")))

               (substitute* "os/os_test.go"
                 (("/usr/bin") (getcwd))
                 (("/bin/pwd") (which "pwd")))

               ;; Disable failing tests: these tests attempt to access
               ;; commands or network resources which are neither available or
               ;; necessary for the build to succeed.
               (for-each
                (match-lambda
                  ((file regex)
                   (substitute* file
                     ((regex all before test_name)
                      (string-append before "Disabled" test_name)))))
                '(("net/net_test.go" "(.+)(TestShutdownUnix.+)")
                  ("net/dial_test.go" "(.+)(TestDialTimeout.+)")
                  ("os/os_test.go" "(.+)(TestHostname.+)")
                  ("time/format_test.go" "(.+)(TestParseInSydney.+)")

                  ;; XXX: This test fails with tzdata 2020b and newer.  Later
                  ;; Go releases work fine, so just disable this for the
                  ;; bootstrap Go.
                  ("time/example_test.go" "(.+)(ExampleParseInLocation.+)")

                  ("os/exec/exec_test.go" "(.+)(TestEcho.+)")
                  ("os/exec/exec_test.go" "(.+)(TestCommandRelativeName.+)")
                  ("os/exec/exec_test.go" "(.+)(TestCatStdin.+)")
                  ("os/exec/exec_test.go" "(.+)(TestCatGoodAndBadFile.+)")
                  ("os/exec/exec_test.go" "(.+)(TestExitStatus.+)")
                  ("os/exec/exec_test.go" "(.+)(TestPipes.+)")
                  ("os/exec/exec_test.go" "(.+)(TestStdinClose.+)")
                  ("syscall/syscall_unix_test.go" "(.+)(TestPassFD\\(.+)")
                  ("os/exec/exec_test.go" "(.+)(TestExtraFiles.+)")))

               (substitute* "net/lookup_unix.go"
                 (("/etc/protocols") (string-append net-base "/etc/protocols")))
               (substitute* "time/zoneinfo_unix.go"
                 (("/usr/share/zoneinfo/") tzdata-path))
               (substitute* (find-files "cmd" "asm.c")
                 (("/lib/ld-linux.*\\.so\\.[0-9]") loader))
               #t)))

         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; FIXME: Some of the .a files are not bit-reproducible.
             (let* ((output (assoc-ref outputs "out")))
               (setenv "CC" (which "gcc"))
               (setenv "GOOS" "linux")
               (setenv "GOROOT" (dirname (getcwd)))
               (setenv "GOROOT_FINAL" output)
               (setenv "GO14TESTS" "1")
               (invoke "sh" "all.bash"))))

         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((output (assoc-ref outputs "out"))
                    (doc_out (assoc-ref outputs "doc"))
                    (bash (string-append (assoc-ref inputs "bash") "bin/bash"))
                    (docs (string-append doc_out "/share/doc/" ,name "-" ,version))
                    (tests (string-append
                            (assoc-ref outputs "tests") "/share/" ,name "-" ,version)))
               (mkdir-p tests)
               (copy-recursively "../test" (string-append tests "/test"))
               (delete-file-recursively "../test")
               (mkdir-p docs)
               (copy-recursively "../api" (string-append docs "/api"))
               (delete-file-recursively "../api")
               (copy-recursively "../doc" (string-append docs "/doc"))
               (delete-file-recursively "../doc")

               (for-each (lambda (file)
                           (let ((file (string-append "../" file)))
                             (install-file file docs)
                             (delete-file file)))
                         '("README" "CONTRIBUTORS" "AUTHORS" "PATENTS"
                           "LICENSE" "VERSION" "robots.txt"))
               (copy-recursively "../" output)
               #t))))))
    (inputs
     `(("tzdata" ,tzdata)
       ("pcre" ,pcre)
       ("gcc:lib" ,(canonical-package gcc) "lib")))
    (native-inputs
     (list pkg-config which net-base perl))

    (home-page "https://go.dev/")
    (synopsis "Compiler and libraries for Go, a statically-typed language")
    (description "Go, also commonly referred to as golang, is an imperative
programming language designed primarily for systems programming.  Go is a
compiled, statically typed language in the tradition of C and C++, but adds
garbage collection, various safety features, and concurrent programming features
in the style of communicating sequential processes (@dfn{CSP}).")
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux"))
    (license license:bsd-3)))

(define-public go-1.16
  (package
    (inherit go-1.4)
    (name "go")
    (version "1.16.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vlk0r4600ah9fg5apdd93g7i369k0rkzcgn7cs8h6qq2k6hpxjl"))))
    (arguments
     (substitute-keyword-arguments
       (strip-keyword-arguments '(#:tests? #:system) (package-arguments go-1.4))
       ((#:phases phases)
        `(modify-phases ,phases
            ;; Time bomb in TLS tests: "Most of the test certificates
            ;; (e.g. testRSACertificate, testRSACertificateIssuer,
            ;; testRSA2048CertificateIssuer) have a not after of Jan 1
            ;; 00:00:00 2025 GMT."
            ;; https://github.com/golang/go/issues/71077
            ;; https://github.com/golang/go/issues/71103
            ;; https://github.com/golang/go/issues/71104
            (add-after 'unpack 'skip-crypto-tls-tests
              (lambda _
                (substitute* (list "src/crypto/tls/handshake_client_test.go"
                                   "src/crypto/tls/handshake_server_test.go")
                  (("TestVerifyConnection.*" all)
                   (string-append all "\n        t.Skip(\"golang.org/issue/71077\")\n"))
                  (("TestResumptionKeepsOCSPAndSCT.*" all)
                   (string-append all "\n        t.Skip(\"golang.org/issue/71077\")\n"))
                  (("TestCrossVersionResume.*" all)
                   (string-append all "\n        t.Skip(\"golang.org/issue/71077\")\n")))))
           (add-after 'unpack 'remove-unused-sourcecode-generators
             (lambda _
               ;; Prevent perl from inclusion in closure through unused files
               (for-each delete-file (find-files "src" "\\.pl$"))))
           (replace 'prebuild
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((gcclib (string-append (assoc-ref inputs "gcc:lib") "/lib"))
                      (net-base (assoc-ref inputs "net-base"))
                      (tzdata-path
                       (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo")))

                 ;; Having the patch in the 'patches' field of <origin> breaks
                 ;; the 'TestServeContent' test due to the fact that
                 ;; timestamps are reset.  Thus, apply it from here.
                 (invoke "patch" "-p2" "--force" "-i"
                         (assoc-ref inputs "go-skip-gc-test.patch"))
                 (invoke "patch" "-p2" "--force" "-i"
                         (assoc-ref inputs "go-fix-script-tests.patch"))

                 (for-each make-file-writable (find-files "."))

                 (substitute* "os/os_test.go"
                   (("/usr/bin") (getcwd))
                   (("/bin/sh") (which "sh")))

                 (substitute* "cmd/go/testdata/script/cgo_path_space.txt"
                   (("/bin/sh") (which "sh")))

                 ;; Add libgcc to runpath
                 (substitute* "cmd/link/internal/ld/lib.go"
                   (("!rpath.set") "true"))
                 (substitute* "cmd/go/internal/work/gccgo.go"
                   (("cgoldflags := \\[\\]string\\{\\}")
                    (string-append "cgoldflags := []string{"
                                   "\"-Wl,-rpath=" gcclib "\""
                                   "}"))
                   (("\"-lgcc_s\", ")
                    (string-append
                     "\"-Wl,-rpath=" gcclib "\", \"-lgcc_s\", ")))
                 (substitute* "cmd/go/internal/work/gc.go"
                   (("ldflags = setextld\\(ldflags, compiler\\)")
                    (string-append
                     "ldflags = setextld(ldflags, compiler)\n"
                     "ldflags = append(ldflags, \"-r\")\n"
                     "ldflags = append(ldflags, \"" gcclib "\")\n")))

                 ;; Disable failing tests: these tests attempt to access
                 ;; commands or network resources which are neither available
                 ;; nor necessary for the build to succeed.
                 (for-each
                  (match-lambda
                    ((file regex)
                     (substitute* file
                       ((regex all before test_name)
                        (string-append before "Disabled" test_name)))))
                  '(("net/net_test.go" "(.+)(TestShutdownUnix.+)")
                    ("net/dial_test.go" "(.+)(TestDialTimeout.+)")
                    ("net/cgo_unix_test.go" "(.+)(TestCgoLookupPort.+)")
                    ("net/cgo_unix_test.go" "(.+)(TestCgoLookupPortWithCancel.+)")
                    ;; 127.0.0.1 doesn't exist
                    ("net/cgo_unix_test.go" "(.+)(TestCgoLookupPTR.+)")
                    ;; 127.0.0.1 doesn't exist
                    ("net/cgo_unix_test.go" "(.+)(TestCgoLookupPTRWithCancel.+)")
                    ;; /etc/services doesn't exist
                    ("net/parse_test.go" "(.+)(TestReadLine.+)")
                    ("os/os_test.go" "(.+)(TestHostname.+)")
                    ;; The user's directory doesn't exist
                    ("os/os_test.go" "(.+)(TestUserHomeDir.+)")
                    ("time/format_test.go" "(.+)(TestParseInSydney.+)")
                    ("time/format_test.go" "(.+)(TestParseInLocation.+)")
                    ("os/exec/exec_test.go" "(.+)(TestEcho.+)")
                    ("os/exec/exec_test.go" "(.+)(TestCommandRelativeName.+)")
                    ("os/exec/exec_test.go" "(.+)(TestCatStdin.+)")
                    ("os/exec/exec_test.go" "(.+)(TestCatGoodAndBadFile.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExitStatus.+)")
                    ("os/exec/exec_test.go" "(.+)(TestPipes.+)")
                    ("os/exec/exec_test.go" "(.+)(TestStdinClose.+)")
                    ("os/exec/exec_test.go" "(.+)(TestIgnorePipeErrorOnSuccess.+)")
                    ("syscall/syscall_unix_test.go" "(.+)(TestPassFD\\(.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExtraFiles/areturn.+)")
                    ("cmd/go/go_test.go" "(.+)(TestCoverageWithCgo.+)")
                    ("cmd/go/go_test.go" "(.+)(TestTwoPkgConfigs.+)")
                    ("os/exec/exec_test.go" "(.+)(TestOutputStderrCapture.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExtraFiles.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExtraFilesRace.+)")
                    ("net/lookup_test.go" "(.+)(TestLookupPort.+)")
                    ("syscall/exec_linux_test.go"
                     "(.+)(TestCloneNEWUSERAndRemapNoRootDisableSetgroups.+)")))

                 ;; These tests fail on aarch64-linux
                 (substitute* "cmd/dist/test.go"
                   (("t.registerHostTest\\(\"testsanitizers/msan.*") ""))

                 ;; fix shebang for testar script
                 ;; note the target script is generated at build time.
                 (substitute* "../misc/cgo/testcarchive/carchive_test.go"
                   (("#!/usr/bin/env") (string-append "#!" (which "env"))))

                 (substitute* "net/lookup_unix.go"
                   (("/etc/protocols") (string-append net-base "/etc/protocols")))
                 (substitute* "net/port_unix.go"
                   (("/etc/services") (string-append net-base "/etc/services")))
                 (substitute* "time/zoneinfo_unix.go"
                   (("/usr/share/zoneinfo/") tzdata-path)))))
           (add-before 'build 'set-bootstrap-variables
             (lambda* (#:key outputs inputs #:allow-other-keys)
               ;; Tell the build system where to find the bootstrap Go.
               (let ((go  (assoc-ref inputs "go")))
                 (setenv "GOROOT_BOOTSTRAP" go)
                 (setenv "GOGC" "400"))))
           (replace 'build
             (lambda* (#:key inputs outputs (parallel-build? #t)
                       #:allow-other-keys)
               ;; FIXME: Some of the .a files are not bit-reproducible.
               ;; (Is this still true?)
               (let* ((njobs (if parallel-build? (parallel-job-count) 1))
                      (output (assoc-ref outputs "out"))
                      (loader (string-append (assoc-ref inputs "libc")
                                             ,(glibc-dynamic-linker))))
                 (setenv "CC" (which "gcc"))
                 (setenv "GO_LDSO" loader)
                 (setenv "GOOS" "linux")
                 (setenv "GOROOT" (dirname (getcwd)))
                 (setenv "GOROOT_FINAL" output)
                 (setenv "GOCACHE" "/tmp/go-cache")
                 (setenv "GOMAXPROCS" (number->string njobs))
                 (invoke "sh" "make.bash" "--no-banner"))))
           (replace 'check
             (lambda* (#:key target (tests? (not target)) (parallel-tests? #t)
                       #:allow-other-keys)
               (let* ((njobs (if parallel-tests? (parallel-job-count) 1)))
                 (when tests?
                   (setenv "GOMAXPROCS" (number->string njobs))
                   (invoke "sh" "run.bash" "--no-rebuild")))))
           (add-before 'install 'unpatch-perl-shebangs
             (lambda _
               ;; Rewrite references to perl input in test scripts
               (substitute* "net/http/cgi/testdata/test.cgi"
                 (("^#!.*") "#!/usr/bin/env perl\n"))))
           (replace 'install
             ;; TODO: Most of this could be factorized with Go 1.4.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((output (assoc-ref outputs "out"))
                      (doc_out (assoc-ref outputs "doc"))
                      (docs (string-append doc_out "/share/doc/" ,name "-" ,version))
                      (src (string-append
                            (assoc-ref outputs "tests") "/share/" ,name "-" ,version)))
                 ;; Prevent installation of the build cache, which contains
                 ;; store references to most of the tools used to build Go and
                 ;; would unnecessarily increase the size of Go's closure if it
                 ;; was installed.
                 (delete-file-recursively "../pkg/obj")

                 (mkdir-p src)
                 (copy-recursively "../test" (string-append src "/test"))
                 (delete-file-recursively "../test")
                 (mkdir-p docs)
                 (copy-recursively "../api" (string-append docs "/api"))
                 (delete-file-recursively "../api")
                 (copy-recursively "../doc" (string-append docs "/doc"))
                 (delete-file-recursively "../doc")

                 (for-each
                  (lambda (file)
                    (let* ((filein (string-append "../" file))
                           (fileout (string-append docs "/" file)))
                      (copy-file filein fileout)
                      (delete-file filein)))
                  ;; Note the slightly different file names compared to 1.4.
                  '("README.md" "CONTRIBUTORS" "AUTHORS" "PATENTS"
                    "LICENSE" "VERSION" "CONTRIBUTING.md" "robots.txt"))

                 (copy-recursively "../" output))))))))
    (native-inputs
     `(,@(if (member (%current-system) (package-supported-systems go-1.4))
           `(("go" ,go-1.4))
           `(("go" ,gccgo-12)))
       ("go-skip-gc-test.patch" ,(search-patch "go-skip-gc-test.patch"))
       ,@(match (%current-system)
           ((or "armhf-linux" "aarch64-linux")
            `(("gold" ,binutils-gold)))
           (_ `()))
       ("go-fix-script-tests.patch" ,(search-patch "go-fix-script-tests.patch"))
       ,@(package-native-inputs go-1.4)))
    (supported-systems (fold delete %supported-systems
                             (list "powerpc-linux" "i586-gnu" "x86_64-gnu")))))

;; https://github.com/golang/go/wiki/MinimumRequirements#microarchitecture-support
(define %go-1.17-arm-micro-architectures
  (list "armv5" "armv6" "armv7"))

(define %go-1.17-powerpc64le-micro-architectures
  (list "power8" "power9"))

(define-public go-1.17
  (package
    (inherit go-1.16)
    (name "go")
    (version "1.17.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "05m8gr050kagvn22lfnjrgms03l5iphd1m4v6z7yqlhn9gdp912d"))))
    (outputs '("out" "tests")) ; 'tests' contains distribution tests.
    (arguments
     `(#:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils))
       ;; TODO: Disable the test(s) in misc/cgo/test/cgo_test.go
       ;; that cause segfaults in the test suite.
       #:tests? ,(not (or (target-aarch64?) (target-riscv64?)))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((output (assoc-ref outputs "out"))
                   (loader (string-append (assoc-ref inputs "libc")
                                          ,(glibc-dynamic-linker))))
               (setenv "GOOS" "linux")
               (setenv "GO_LDSO" loader)
               (setenv "GOROOT" (getcwd))
               (setenv "GOROOT_FINAL" (string-append output "/lib/go"))
               (setenv "GOGC" "400")
               (setenv "GOCACHE" "/tmp/go-cache"))))

         ;; Time bomb in TLS tests: "Most of the test certificates
         ;; (e.g. testRSACertificate, testRSACertificateIssuer,
         ;; testRSA2048CertificateIssuer) have a not after of Jan 1
         ;; 00:00:00 2025 GMT."
         ;; https://github.com/golang/go/issues/71077
         ;; https://github.com/golang/go/issues/71103
         ;; https://github.com/golang/go/issues/71104
         (add-after 'unpack 'skip-crypto-tls-tests
           (lambda _
             (substitute* (list "src/crypto/tls/handshake_client_test.go"
                                "src/crypto/tls/handshake_server_test.go")
               (("TestVerifyConnection.*" all)
                (string-append all "\n        t.Skip(\"golang.org/issue/71077\")\n"))
               (("TestResumptionKeepsOCSPAndSCT.*" all)
                (string-append all "\n        t.Skip(\"golang.org/issue/71077\")\n"))
               (("TestCrossVersionResume.*" all)
                (string-append all "\n        t.Skip(\"golang.org/issue/71077\")\n")))))

         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((net-base (assoc-ref inputs "net-base"))
                    (tzdata-path (string-append (assoc-ref inputs "tzdata")
                                                "/share/zoneinfo")))
               ;; XXX: Remove when #49729 is merged?
               (for-each make-file-writable (find-files "src"))

               ;; Having the patch in the 'patches' field of <origin> breaks
               ;; the 'TestServeContent' test due to the fact that
               ;; timestamps are reset.  Thus, apply it from here.
               (invoke "patch" "-p1" "--force" "-i"
                       (assoc-ref inputs "go-skip-gc-test.patch"))
               (invoke "patch" "-p1" "--force" "-i"
                       (assoc-ref inputs "go-fix-script-tests.patch"))

               (substitute* "src/os/os_test.go"
                 (("/usr/bin") (getcwd))
                 (("/bin/sh") (which "sh")))

               (substitute* "src/cmd/go/testdata/script/cgo_path_space.txt"
                 (("/bin/sh") (which "sh")))

               ;; fix shebang for testar script
               ;; note the target script is generated at build time.
               (substitute* "misc/cgo/testcarchive/carchive_test.go"
                 (("/usr/bin/env bash") (which "bash")))

               (substitute* "src/net/lookup_unix.go"
                 (("/etc/protocols")
                  (string-append net-base "/etc/protocols")))
               (substitute* "src/net/port_unix.go"
                 (("/etc/services")
                  (string-append net-base "/etc/services")))
               (substitute* "src/time/zoneinfo_unix.go"
                 (("/usr/share/zoneinfo/") tzdata-path)))))
         ;; Keep this synchronized with the package inputs.
         ;; Also keep syncthonized with later versions of go.
         ,@(if (or (target-arm?) (target-ppc64le?))
             '((add-after 'unpack 'patch-gcc:lib
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((gcclib (string-append (assoc-ref inputs "gcc:lib") "/lib")))
                     ;; Add libgcc to runpath
                     (substitute* "src/cmd/link/internal/ld/lib.go"
                       (("!rpath.set") "true"))
                     (substitute* "src/cmd/go/internal/work/gccgo.go"
                       (("cgoldflags := \\[\\]string\\{\\}")
                        (string-append "cgoldflags := []string{"
                                       "\"-Wl,-rpath=" gcclib "\""
                                       "}"))
                       (("\"-lgcc_s\", ")
                        (string-append
                         "\"-Wl,-rpath=" gcclib "\", \"-lgcc_s\", ")))
                     (substitute* "src/cmd/go/internal/work/gc.go"
                       (("ldflags = setextld\\(ldflags, compiler\\)")
                        (string-append
                         "ldflags = setextld(ldflags, compiler)\n"
                         "ldflags = append(ldflags, \"-r\")\n"
                         "ldflags = append(ldflags, \"" gcclib "\")\n")))))))
             '())
         ;; Backported from later versions of go to workaround 64k page sizes.
         ,@(if (target-ppc64le?)
             '((add-after 'unpack 'adjust-test-suite
                 (lambda _
                   (substitute* "misc/cgo/testshared/shared_test.go"
                     (("100000") "256000")))))
             '())
         (add-after 'patch-source 'disable-failing-tests
           (lambda _
             ;; Disable failing tests: these tests attempt to access
             ;; commands or network resources which are neither available
             ;; nor necessary for the build to succeed.
             (for-each
              (match-lambda
                ((file test)
                 (let ((regex (string-append "^(func\\s+)(" test "\\()")))
                   (substitute* file
                     ((regex all before test_name)
                      (string-append before "Disabled" test_name))))))
              '(("src/net/cgo_unix_test.go" "TestCgoLookupPort")
                ("src/net/cgo_unix_test.go" "TestCgoLookupPortWithCancel")
                ;; 127.0.0.1 doesn't exist
                ("src/net/cgo_unix_test.go" "TestCgoLookupPTR")
                ("src/net/cgo_unix_test.go" "TestCgoLookupPTRWithCancel")
                ;; /etc/services doesn't exist
                ("src/net/parse_test.go" "TestReadLine")
                ;; The user's directory doesn't exist
                ("src/os/os_test.go" "TestUserHomeDir")))

             ;; These tests fail on aarch64-linux
             (substitute* "src/cmd/dist/test.go"
               (("t.registerHostTest\\(\"testsanitizers/msan.*") ""))))
         (add-after 'patch-source 'enable-external-linking
           (lambda _
             ;; Invoke GCC to link any archives created with GCC (that is, any
             ;; packages built using 'cgo'), because Go doesn't know how to
             ;; handle the runpaths but GCC does.  Use substitute* rather than
             ;; a patch since these files are liable to change often.
             ;;
             ;; XXX: Replace with GO_EXTLINK_ENABLED=1 or similar when
             ;; <https://github.com/golang/go/issues/31544> and/or
             ;; <https://github.com/golang/go/issues/43525> are resolved.
             (substitute* "src/cmd/link/internal/ld/config.go"
               (("iscgo && externalobj") "iscgo"))
             (substitute* '("src/cmd/nm/nm_cgo_test.go"
                            "src/cmd/dist/test.go")
               (("^func.*?nternalLink\\(\\).*" all)
                (string-append all "\n\treturn false\n")))))
         (replace 'build
           (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
             (let* ((njobs (if parallel-build? (parallel-job-count) 1)))
               (with-directory-excursion "src"
                 (setenv "GOMAXPROCS" (number->string njobs))
                 (invoke "sh" "make.bash" "--no-banner")))))
         (replace 'check
           (lambda* (#:key target (tests? (not target)) (parallel-tests? #t)
                     #:allow-other-keys)
             (let* ((njobs (if parallel-tests? (parallel-job-count) 1)))
               (when tests?
                 (with-directory-excursion "src"
                   (setenv "GOMAXPROCS" (number->string njobs))
                   (invoke "sh" "run.bash" "--no-rebuild"))))))
         (add-before 'install 'unpatch-perl-shebangs
           (lambda _
             ;; Avoid inclusion of perl in closure by rewriting references
             ;; to perl input in sourcecode generators and test scripts
             (substitute* (cons "src/net/http/cgi/testdata/test.cgi"
                                (find-files "src" "\\.pl$"))
               (("^#!.*") "#!/usr/bin/env perl\n"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Notably, we do not install archives (180M), which Go will
             ;; happily recompile quickly (and cache) if needed, almost
             ;; surely faster than they could be substituted.
             ;;
             ;; The main motivation for pre-compiled archives is to use
             ;; libc-linked `net' or `os' packages without a C compiler,
             ;; but on Guix a C compiler is necessary to properly link the
             ;; final binaries anyway.  Many build flags also invalidate
             ;; these pre-compiled archives, so in practice Go often
             ;; recompiles them anyway.
             ;;
             ;; Upstream is also planning to no longer install these
             ;; archives: <https://github.com/golang/go/issues/47257>
             ;;
             ;; When necessary, a custom pre-compiled library package can
             ;; be created with `#:import-path "std"' and used with
             ;; `-pkgdir'.
             (let* ((out (assoc-ref outputs "out"))
                    (tests (assoc-ref outputs "tests")))
               (for-each
                (lambda (file)
                  (copy-recursively file (string-append out "/lib/go/" file)))
                '("lib" "VERSION" "pkg/include" "pkg/tool"))

               (for-each
                (match-lambda
                  ((file dest output)
                   ;; Copy to output/dest and symlink from output/lib/go/file.
                   (let ((file* (string-append output "/lib/go/" file))
                         (dest* (string-append output "/" dest)))
                     (copy-recursively file dest*)
                     (mkdir-p (dirname file*))
                     (symlink (string-append "../../" dest) file*))))
                `(("bin"          "bin"                 ,out)
                  ("src"          "share/go/src"        ,out)
                  ("misc"         "share/go/misc"       ,out)
                  ("doc"          "share/doc/go/doc"    ,out)
                  ("api"          "share/go/api"        ,tests)
                  ("test"         "share/go/test"       ,tests))))))
         (add-after 'install 'install-doc-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (file)
                  (install-file file (string-append out "/share/doc/go")))
                '("AUTHORS" "CONTRIBUTORS" "CONTRIBUTING.md" "PATENTS"
                  "README.md" "SECURITY.md"))))))))
    (inputs (if (not (or (target-arm?) (target-ppc64le?)))
              (alist-delete "gcc:lib" (package-inputs go-1.16))
              (package-inputs go-1.16)))
    (properties
     `((compiler-cpu-architectures
         ("armhf" ,@%go-1.17-arm-micro-architectures)
         ("powerpc64le" ,@%go-1.17-powerpc64le-micro-architectures))))))

(define %go-1.18-x86_64-micro-architectures
  ;; GOAMD defaults to 'v1' so we match the default elsewhere.
  (list "x86-64" "x86-64-v2" "x86-64-v3" "x86-64-v4"))

(define-public go-1.18
  (package
    (inherit go-1.17)
    (name "go")
    (version "1.18.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ph3ajfq5q8j3nd91pfb25pm21aiphc58zf7fwis0h3a6nqbdyq9"))))
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.17)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'adjust-test-suite)
           ,@(if (or (target-arm?) (target-ppc64le?))
               '((replace 'patch-gcc:lib
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let* ((gcclib (string-append (assoc-ref inputs "gcc:lib") "/lib")))
                       ;; Add libgcc to runpath
                       (substitute* "src/cmd/link/internal/ld/lib.go"
                         (("!rpath.set") "true"))
                       (substitute* "src/cmd/go/internal/work/gccgo.go"
                         (("cgoldflags := \\[\\]string\\{\\}")
                          (string-append "cgoldflags := []string{"
                                         "\"-Wl,-rpath=" gcclib "\""
                                         "}"))
                         (("\"-lgcc_s\", ")
                          (string-append
                           "\"-Wl,-rpath=" gcclib "\", \"-lgcc_s\", ")))
                       (substitute* "src/cmd/go/internal/work/gc.go"
                         (("ldflags, err := setextld\\(ldflags, compiler\\)")
                          (string-append
                           "ldflags, err := setextld(ldflags, compiler)\n"
                           "ldflags = append(ldflags, \"-r\")\n"
                           "ldflags = append(ldflags, \"" gcclib "\")\n")))))))
               '())))))
    (properties
     `((compiler-cpu-architectures
         ("armhf" ,@%go-1.17-arm-micro-architectures)
         ("powerpc64le" ,@%go-1.17-powerpc64le-micro-architectures)
         ("x86_64" ,@%go-1.18-x86_64-micro-architectures))))))

(define-public go-1.19
  (package
    (inherit go-1.18)
    (name "go")
    (version "1.19.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rrpfhv6vdwqs0jnld0iqsky5wlirir05czf34kvsf2db21nzdi9"))))
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.18)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; These are recurring test failures, depending on having a new
            ;; enough version of gccgo.  gccgo-12.2 fails with go-1.19.7.
            ;; https://github.com/golang/go/issues/22224
            ;; https://github.com/golang/go/issues/25324
            (add-after 'unpack 'skip-TestGoPathShlibGccgo-tests
              (lambda _
                (substitute* "misc/cgo/testshared/shared_test.go"
                  (("TestGoPathShlibGccgo.*" all)
                   (string-append all "\n        t.Skip(\"golang.org/issue/22224\")\n"))
                  (("TestTwoGopathShlibsGccgo.*" all)
                   (string-append all "\n        t.Skip(\"golang.org/issue/22224\")\n")))))
            (replace 'install-doc-files
              (lambda _
                (for-each (lambda (file)
                            (install-file file (string-append
                                                #$output "/share/doc/go")))
                          '("CONTRIBUTING.md" "PATENTS" "README.md"
                            "SECURITY.md"))))))))))

(define-public go-1.20
  (package
    (inherit go-1.19)
    (name "go")
    (version "1.20.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/go")
                    (commit (string-append "go" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ir0x17i9067i48ffskwlmbx1j4kfhch46zl8cwl88y23aw59qa2"))))
    (native-inputs
     ;; Go 1.20 and later requires Go 1.17 as the bootstrap toolchain.
     ;; See 'src/cmd/dist/notgo117.go' in the source code distribution,
     ;; as well as the upstream discussion of this topic:
     ;; https://go.dev/issue/44505
     ;; We continue to use gccgo-12 since it provides go-1.18.
     (if (member (%current-system) (package-supported-systems go-1.4))
         (alist-replace "go" (list go-1.17) (package-native-inputs go-1.17))
         (package-native-inputs go-1.17)))))

(define-public go-1.21
  (package
    (inherit go-1.20)
    (name "go")
    (version "1.21.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/go")
                    (commit (string-append "go" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x4qdib1d3gzgz620aysi1rrg682g93710dar4ga32b0j0w5kbhj"))))
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.20)
       ;; Source patching phases are broken up into discrete steps to allow
       ;; future versions to discard individual phases without having to
       ;; discard all source patching.
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'skip-TestGoPathShlibGccgo-tests)
            (delete 'patch-source)
            (add-after 'unpack 'patch-os-tests
              (lambda _
                (substitute* "src/os/os_test.go"
                  (("/usr/bin") (getcwd))
                  (("/bin/sh") (which "sh")))))

            (add-after 'unpack 'apply-patches
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Having the patch in the 'patches' field of <origin> breaks
                ;; the 'TestServeContent' test due to the fact that timestamps
                ;; are reset.  Thus, apply it from here.
                (invoke "patch" "-p1" "--force" "-i"
                        (assoc-ref inputs "go-fix-script-tests.patch"))))

            (add-after 'unpack 'patch-src/net
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((net-base (assoc-ref inputs "net-base")))
                  (substitute* "src/net/lookup_unix.go"
                    (("/etc/protocols")
                     (string-append net-base "/etc/protocols")))
                  (substitute* "src/net/port_unix.go"
                    (("/etc/services")
                     (string-append net-base "/etc/services"))))))

            (add-after 'unpack 'patch-zoneinfo
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Add the path to this specific version of tzdata's zoneinfo
                ;; file to the top of the list to search. We don't want to
                ;; replace any sources because it will affect how binaries
                ;; compiled with this Go toolchain behave on non-guix
                ;; platforms.
                (substitute* "src/time/zoneinfo_unix.go"
                  (("var platformZoneSources.+" all)
                   (format #f "~a~%\"~a/share/zoneinfo\",~%"
                           all
                           (assoc-ref inputs "tzdata"))))))

            (add-after 'unpack 'patch-cmd/go/testdata/script
              (lambda _
                (substitute* "src/cmd/go/testdata/script/cgo_path_space.txt"
                  (("/bin/sh") (which "sh")))))

            (add-after 'enable-external-linking 'enable-external-linking-1.21
              (lambda _
                ;; Invoke GCC to link any archives created with GCC (that is,
                ;; any packages built using 'cgo'), because Go doesn't know
                ;; how to handle the runpaths but GCC does.  Use substitute*
                ;; rather than a patch since these files are liable to change
                ;; often.
                ;;
                ;; XXX: Replace with GO_EXTLINK_ENABLED=1 or similar when
                ;; <https://github.com/golang/go/issues/31544> and/or
                ;; <https://github.com/golang/go/issues/43525> are resolved.
                (substitute* "src/cmd/link/internal/ld/config.go"
                  (("\\(iscgo && \\(.+\\)") "iscgo"))
                (substitute* "src/internal/testenv/testenv.go"
                  (("!CanInternalLink.+") "true {\n"))
                (substitute* "src/syscall/exec_linux_test.go"
                  (("testenv.MustHaveExecPath\\(t, \"whoami\"\\)")
                   "t.Skipf(\"no passwd file present\")"))))

            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                ;; Notably, we do not install archives (180M), which Go will
                ;; happily recompile quickly (and cache) if needed, almost
                ;; surely faster than they could be substituted.
                ;;
                ;; The main motivation for pre-compiled archives is to use
                ;; libc-linked `net' or `os' packages without a C compiler,
                ;; but on Guix a C compiler is necessary to properly link the
                ;; final binaries anyway.  Many build flags also invalidate
                ;; these pre-compiled archives, so in practice Go often
                ;; recompiles them anyway.
                ;;
                ;; Upstream is also planning to no longer install these
                ;; archives: <https://github.com/golang/go/issues/47257>.
                ;;
                ;; When necessary, a custom pre-compiled library package can
                ;; be created with `#:import-path "std"' and used with
                ;; `-pkgdir'.
                ;;
                ;; When moving files into place, any files that come from
                ;; GOROOT should remain in GOROOT to continue functioning. If
                ;; they need to be referenced from some other directory, they
                ;; need to be symlinked from GOROOT. For more information,
                ;; please see <https://github.com/golang/go/issues/61921>.
                (let* ((out (assoc-ref outputs "out"))
                       (tests (assoc-ref outputs "tests")))
                  (for-each
                   (lambda (file)
                     (copy-recursively file (string-append out "/lib/go/" file)))
                   '("bin" "go.env" "lib" "VERSION" "pkg/include" "pkg/tool"))

                  (symlink "lib/go/bin" (string-append out "/bin"))

                  (for-each
                   (match-lambda
                     ((file dest output)
                      ;; Copy to output/dest and symlink from
                      ;; output/lib/go/file.
                      (let ((file* (string-append output "/lib/go/" file))
                            (dest* (string-append output "/" dest)))
                        (copy-recursively file dest*)
                        (mkdir-p (dirname file*))
                        (symlink (string-append "../../" dest) file*))))
                   `(("src"          "share/go/src"        ,out)
                     ("misc"         "share/go/misc"       ,out)
                     ("doc"          "share/doc/go/doc"    ,out)
                     ("api"          "share/go/api"        ,tests)
                     ("test"         "share/go/test"       ,tests))))))))))))

(define-public go-1.22
  (package
    (inherit go-1.21)
    (name "go")
    (version "1.22.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gdiyifsp65wlnfqfmnbv6n1rh23jbr13l79xwla3gavm67scx02"))))
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.21)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'unpatch-perl-shebangs
              (lambda _
                ;; Avoid inclusion of perl in closure by rewriting references
                ;; to perl input in sourcecode generators and test scripts
                (substitute* (find-files "src" "\\.pl$")
                  (("^#!.*")
                   "#!/usr/bin/env perl\n"))))
            (add-after 'unpack 'remove-flakey-thread-sanitizer-tests
              (lambda _
                ;; These tests have been identified as flakey:
                ;; https://github.com/golang/go/issues/66427
                (substitute* "src/cmd/cgo/internal/testsanitizers/tsan_test.go"
                  ((".*tsan1[34].*") ""))))))))
    (native-inputs
     ;; Go 1.22 and later requires Go 1.20 (min. 1.20.6, which we don't have)
     ;; as the bootstrap toolchain.
     (alist-replace "go" (list go-1.21) (package-native-inputs go-1.21)))))

(define-public go-1.23
  (package
    (inherit go-1.22)
    (name "go")
    (version "1.23.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iffjgsmh4ilc1r30zbidqvxz2dd8k0sml5rzzk1k4wkab0cjw0i"))))))

;;
;; Default Golang version used in guix/build-system/go.scm to build packages.
;;
(define-public go go-1.21)

(define make-go-std
  (mlambdaq (go)
    "Return a package which builds the standard library for Go compiler GO."
    (package
      (name (string-append (package-name go) "-std"))
      (version (package-version go))
      (source #f)
      (build-system go-build-system)
      (arguments
       `(#:import-path "std"
         #:build-flags `("-pkgdir" "pkg")      ; "Install" to build directory.
         #:allow-go-reference? #t
         #:substitutable? #f            ; Faster to build than download.
         #:tests? #f                    ; Already tested in the main Go build.
         #:go ,go
         #:phases
         (modify-phases %standard-phases
           (delete 'unpack)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (out-cache (string-append out "/var/cache/go/build")))
                 (copy-recursively (getenv "GOCACHE") out-cache)
                 (delete-file (string-append out-cache "/trim.txt"))
                 (delete-file (string-append out-cache "/README")))))
           (delete 'install-license-files))))
      (home-page (package-home-page go))
      (synopsis "Cached standard library build for Go")
      (description (package-description go))
      (license (package-license go)))))

(export make-go-std)

;; Make those public so they have a corresponding Cuirass job.
(define-public go-std-1.16 (make-go-std go-1.16))
(define-public go-std-1.17 (make-go-std go-1.17))
(define-public go-std-1.18 (make-go-std go-1.18))
(define-public go-std-1.19 (make-go-std go-1.19))
(define-public go-std-1.20 (make-go-std go-1.20))
(define-public go-std-1.21 (make-go-std go-1.21))
(define-public go-std-1.22 (make-go-std go-1.22))
(define-public go-std-1.23 (make-go-std go-1.23))

(define-public go-0xacab-org-leap-shapeshifter
  (let ((commit "0aa6226582efb8e563540ec1d3c5cfcd19200474")
        (revision "12"))
    (package
      (name "go-0xacab-org-leap-shapeshifter")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://0xacab.org/leap/shapeshifter")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0m4fla9ppl53k9syms4dsad92wakr74cdvids3xxv3amdh4d1w4i"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "0xacab.org/leap/shapeshifter"))
      (propagated-inputs
       (list go-github-com-operatorfoundation-obfs4
             go-github-com-operatorfoundation-shapeshifter-transports
             go-golang-org-x-net))
      (home-page "https://0xacab.org/leap/shapeshifter")
      (synopsis "Shapeshifter Dispatcher Library")
      (description "Shapeshifter provides network protocol shapeshifting
technology.  The purpose of this technology is to change the characteristics of
network traffic so that it is not identified and subsequently blocked by network
filtering devices.")
      (license license:bsd-2))))

(define-public go-github-com-operatorfoundation-shapeshifter-transports
  (package
    (name "go-github-com-operatorfoundation-shapeshifter-transports")
    (version "3.0.12")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/OperatorFoundation/shapeshifter-transports")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f1hzhk3q2fgqdg14zlg3z0s0ib1y9xwj89qnjk95b37zbgqjgsb"))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "github.com/OperatorFoundation/shapeshifter-transports"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'build)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Dust/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Dust/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Optimizer/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Optimizer/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Replicant/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Replicant/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/meeklite/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/meeklite/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/meekserver/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/meekserver/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs2/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs2/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs4/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs4/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/shadow/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/shadow/v3"))))
         (replace 'check
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'check)
                       `(,@arguments #:import-path ,directory)))
              (list
               ;;; ERROR: invalid memory address or nil pointer dereference.
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/Dust/v2"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/Dust/v3"
               ;;; ERROR: failed with status 1.
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/Optimizer/v2"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/Optimizer/v3"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/Replicant/v2"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/Replicant/v3"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/meeklite/v2"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/meeklite/v3"
               ;;; ERROR: bind: permission denied.
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/meekserver/v2"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/meekserver/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs2/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs2/v3"))))
               ;;; ERROR: failed with status 1.
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/obfs4/v2"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/obfs4/v3"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/shadow/v2"
               ;;"github.com/OperatorFoundation/shapeshifter-transports/transports/shadow/v3"))))
         (replace 'install
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'install)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Dust/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Dust/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Optimizer/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Optimizer/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Replicant/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/Replicant/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/meeklite/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/meeklite/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/meekserver/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/meekserver/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs2/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs2/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs4/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/obfs4/v3"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/shadow/v2"
               "github.com/OperatorFoundation/shapeshifter-transports/transports/shadow/v3")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-aead-chacha20
           go-github-com-blanu-dust
           go-github-com-deckarep-golang-set
           go-github-com-kataras-golog
           go-github-com-mufti1-interconv
           go-github-com-opentracing-opentracing-go
           go-github-com-operatorfoundation-monolith-go-1.0.4
           go-github-com-operatorfoundation-obfs4
           go-github-com-operatorfoundation-shapeshifter-ipc
           go-github-com-shadowsocks-go-shadowsocks2
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-torproject-org-pluggable-transports-goptlib))
    (home-page "https://github.com/OperatorFoundation/shapeshifter-transports")
    (synopsis "Go implementation of Pluggable Transports")
    (description "Shapeshifter-Transports is a set of Pluggable Transports
implementing the Go API from the Pluggable Transports 2.0 specification.
Each transport implements a different method of shapeshifting network traffic.
The goal is for application traffic to be sent over the network in a shapeshifted
form that bypasses network filtering, allowing the application to work on
networks where it would otherwise be blocked or heavily throttled.")
    (license license:expat)))

(define-public go-github-com-mufti1-interconv
  (let ((commit "d7c72925c6568d60d361757bb9f2d252dcca745c")
        (revision "0"))
    (package
      (name "go-github-com-mufti1-interconv")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/mufti1/interconv")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13f5pvr74afa28pbpmgvjzjx68vv5zmrwlvxp7hr5bl5625zlxmy"))))
      (build-system go-build-system)
      (arguments
       `(#:unpack-path "github.com/mufti1/interconv"
         #:import-path "github.com/mufti1/interconv/package"))
      (home-page "https://github.com/mufti1/interconv")
      (synopsis "Data type converter")
      (description "InterConv converts interfaces into any data type.")
      (license license:expat))))

(define-public go-github-com-operatorfoundation-monolith-go
  (package
    (name "go-github-com-operatorfoundation-monolith-go")
    (version "1.0.10")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/OperatorFoundation/monolith-go")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zzamnrakjvz9frxscyhrvyz2ikqq2klmynn218jk5dar6mc6xyf"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ; ERROR: Generated bytes do not match correct answer.
       #:unpack-path "github.com/OperatorFoundation/monolith-go"
       #:import-path "github.com/OperatorFoundation/monolith-go/monolith"))
    (propagated-inputs
     (list go-github-com-deckarep-golang-set))
    (home-page "https://github.com/OperatorFoundation/monolith-go")
    (synopsis "Byte sequences library")
    (description "Monolith-Go is a Go library for working with byte sequences.")
    (license license:expat)))

;; To build bitmask 0.21.11, remove when it's updated.
(define-public go-github-com-operatorfoundation-monolith-go-1.0.4
  (package
    (inherit go-github-com-operatorfoundation-monolith-go)
    (name "go-github-com-operatorfoundation-monolith-go")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/OperatorFoundation/monolith-go")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "066bqlgw5h7a3kxswqlv734asb7nw2y6snsn09yqk0ixj23qw22s"))))))

(define-public go-github-com-blanu-dust
  (package
    (name "go-github-com-blanu-dust")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/blanu/Dust")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lya21w06ramq37af5hdiafbrv5k1csjm7k7m00v0bfxg3ni01bs"))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "github.com/blanu/Dust"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'build)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/blanu/Dust/go/buf"
               "github.com/blanu/Dust/go/dist"
               "github.com/blanu/Dust/go/huffman"
               "github.com/blanu/Dust/go/model1"
               "github.com/blanu/Dust/go/prim1"
               "github.com/blanu/Dust/go/proc"
               "github.com/blanu/Dust/go/sillyHex"
               "github.com/blanu/Dust/go/skein"
               "github.com/blanu/Dust/go/v2/Dust2_proxy"
               "github.com/blanu/Dust/go/v2/Dust2_tool"
               "github.com/blanu/Dust/go/v2/crypting"
               "github.com/blanu/Dust/go/v2/interface"
               "github.com/blanu/Dust/go/v2/shaping"))))
         (replace 'check
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'check)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/blanu/Dust/go/buf"
               "github.com/blanu/Dust/go/dist"
               ;; Repository is missing test files directory.
               ;;"github.com/blanu/Dust/go/huffman"
               "github.com/blanu/Dust/go/model1"
               "github.com/blanu/Dust/go/prim1"
               "github.com/blanu/Dust/go/proc"
               "github.com/blanu/Dust/go/sillyHex"
               "github.com/blanu/Dust/go/skein"
               "github.com/blanu/Dust/go/v2/Dust2_proxy"
               "github.com/blanu/Dust/go/v2/Dust2_tool"
               "github.com/blanu/Dust/go/v2/crypting"
               "github.com/blanu/Dust/go/v2/interface"
               "github.com/blanu/Dust/go/v2/shaping"))))
         (replace 'install
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'install)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/blanu/Dust/go/buf"
               "github.com/blanu/Dust/go/dist"
               "github.com/blanu/Dust/go/huffman"
               "github.com/blanu/Dust/go/model1"
               "github.com/blanu/Dust/go/prim1"
               "github.com/blanu/Dust/go/proc"
               "github.com/blanu/Dust/go/sillyHex"
               "github.com/blanu/Dust/go/skein"
               "github.com/blanu/Dust/go/v2/Dust2_proxy"
               "github.com/blanu/Dust/go/v2/Dust2_tool"
               "github.com/blanu/Dust/go/v2/crypting"
               "github.com/blanu/Dust/go/v2/interface"
               "github.com/blanu/Dust/go/v2/shaping")))))))
    (propagated-inputs
     (list go-github-com-operatorfoundation-ed25519
           go-github-com-op-go-logging go-golang-org-x-crypto))
    (home-page "https://github.com/blanu/Dust")
    (synopsis "Censorship-resistant internet transport protocol")
    (description "Dust is an Internet protocol designed to resist a number of
attacks currently in active use to censor Internet communication.  While
adherence to the theoretical maxims of cryptographic security is observed where
possible, the focus of Dust is on real solutions to real attacks.")
    (license
     (list
      ;; Skein.
      license:bsd-2
      ;; Others.
      license:expat))))

(define-public go-github-com-operatorfoundation-shapeshifter-ipc
  (package
    (name "go-github-com-operatorfoundation-shapeshifter-ipc")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/OperatorFoundation/shapeshifter-ipc")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q1fcnllg462nfca16s5mr0n2jh92x3hj946qnaqc682phjz04lg"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f                      ; ERROR: undefined: Args.
       #:unpack-path "github.com/OperatorFoundation/shapeshifter-ipc"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'build)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/OperatorFoundation/shapeshifter-ipc/v2"
               "github.com/OperatorFoundation/shapeshifter-ipc/v3"))))
         (replace 'check
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'check)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/OperatorFoundation/shapeshifter-ipc/v2"
               "github.com/OperatorFoundation/shapeshifter-ipc/v3"))))
         (replace 'install
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'install)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/OperatorFoundation/shapeshifter-ipc/v2"
               "github.com/OperatorFoundation/shapeshifter-ipc/v3")))))))
    (home-page "https://github.com/OperatorFoundation/shapeshifter-ipc")
    (synopsis "Go implementation of the Pluggable Transports IPC protocol")
    (description "Shapeshifter-IPC is a library for Go implementing the IPC
protocol from the Pluggable Transports 2.0 specification.")
    (license license:expat)))

(define-public go-github-com-operatorfoundation-obfs4
  (package
    (name "go-github-com-operatorfoundation-obfs4")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/OperatorFoundation/obfs4")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s730xagdxs66wfh65hb5v9a5h01q5ncic3pyij0a043scagizgr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/OperatorFoundation/obfs4"
      #:test-subdirs #~(list "common/..."
                             "proxy_dialers/..."
                             "transports/obfs4/...")))
    (propagated-inputs
     (list go-github-com-dchest-siphash
           go-github-com-operatorfoundation-ed25519
           go-github-com-willscott-goturn
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-torproject-org-pluggable-transports-goptlib))
    (home-page "https://github.com/OperatorFoundation/obfs4")
    (synopsis "Network obfourscator to scramble network traffic")
    (description "Obfs4 is a look-like nothing obfuscation protocol that
incorporates ideas and concepts from Philipp Winter's ScrambleSuit protocol.
The notable differences between ScrambleSuit and obfs4 are:
@itemize
@item The handshake always does a full key exchange (no such thing as a Session
Ticket Handshake).
@item The handshake uses the Tor Project's ntor handshake with public keys
obfuscated via the Elligator 2 mapping.
@item The link layer encryption uses NaCl secret boxes (Poly1305/XSalsa20).
@end itemize")
    (license license:bsd-2)))

(define-public go-github-com-willscott-goturn
    (package
      (name "go-github-com-willscott-goturn")
      (version "0.0.0-20170802220503-19f41278d0c9")
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/willscott/goturn")
           (commit (go-version->git-ref version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zwvhfznr84ayzknn9flh65nvqjsixisgy9fkhz2jlahl1ldqcq7"))))
      (build-system go-build-system)
      (arguments
       `(#:tests? #f ; tests are broken on a newer go, starting from 1.17.
         #:import-path "github.com/willscott/goturn"))
      (home-page "https://github.com/willscott/goturn")
      (synopsis "Go TURN dialer")
      (description "GoTURN is a library providing a Go interface compatible with
the golang proxy package which connects through a TURN relay.  It provides
parsing and encoding support for STUN and TURN protocols.")
      (license license:bsd-3)))

(define-public lyrebird
  (package
    (name "lyrebird")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird")
                    (commit (string-append "lyrebird-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qk7npkj0a3a28rp38whl1jwjr0z0hdcsq5bgm8bl1fk9g6cqbnp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:unpack-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird"
      #:import-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird/cmd/lyrebird"))
    (propagated-inputs
     (list go-filippo-io-edwards25519
           go-github-com-dchest-siphash
           go-github-com-refraction-networking-utls
           go-gitlab-com-yawning-edwards25519-extra
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-snowflake-v2
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-webtunnel
           go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird")
    (synopsis "Look-like nothing obfuscation protocol")
    (description "This is a look-like nothing obfuscation protocol that
incorporates ideas and concepts from Philipp Winter's ScrambleSuit protocol.")
    (license (list license:bsd-2 license:bsd-3))))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-lyrebird
  ;; This is a final command, no need for a full name of the go.mod module path
  ;; style. The same is suggested in project's README and Makefile.
  (deprecated-package
   "go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-lyrebird"
   lyrebird))

(define-public go-github-com-apparentlymart-go-openvpn-mgmt
  (let ((commit "4d2ce95ae600ee04eeb020ee0997aabb82752210")
        (revision "0"))
    (package
      (name "go-github-com-apparentlymart-go-openvpn-mgmt")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/apparentlymart/go-openvpn-mgmt")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dn431jnswg5ns1ah10wswnw6wiv48zq21zr5xp1178l4waswj7k"))))
      (build-system go-build-system)
      (arguments
       `(#:unpack-path "github.com/apparentlymart/go-openvpn-mgmt"
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda arguments
               (for-each
                (lambda (directory)
                  (apply (assoc-ref %standard-phases 'build)
                         `(,@arguments #:import-path ,directory)))
                (list
                 "github.com/apparentlymart/go-openvpn-mgmt/demux"
                 "github.com/apparentlymart/go-openvpn-mgmt/openvpn"))))
           (replace 'check
             (lambda arguments
               (for-each
                (lambda (directory)
                  (apply (assoc-ref %standard-phases 'check)
                         `(,@arguments #:import-path ,directory)))
                (list
                 "github.com/apparentlymart/go-openvpn-mgmt/demux"
                 "github.com/apparentlymart/go-openvpn-mgmt/openvpn"))))
           (replace 'install
             (lambda arguments
               (for-each
                (lambda (directory)
                  (apply (assoc-ref %standard-phases 'install)
                         `(,@arguments #:import-path ,directory)))
                (list
                 "github.com/apparentlymart/go-openvpn-mgmt/demux"
                 "github.com/apparentlymart/go-openvpn-mgmt/openvpn")))))))
      (home-page "https://github.com/apparentlymart/go-openvpn-mgmt")
      (synopsis "Go client library for OpenVPN's management protocol")
      (description "Go-OpenVPN-Mgmt implements a client for the OpenVPN
management interface.  It can be used to monitor and control an OpenVPN process
running with its management port enabled.")
      (license license:expat))))

(define-public go-github-com-aarzilli-golua
  (let ((commit "03fc4642d792b1f2bc5e7343b403cf490f8c501d")
        (revision "0"))
    (package
      (name "go-github-com-aarzilli-golua")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/aarzilli/golua")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1d9hr29i36cza98afj3g6rs3l7xbkprwzz0blcxsr9dd7nak20di"))))
      (build-system go-build-system)
      ;; From go-1.10 onward, "pkg" compiled libraries are not re-used, so
      ;; when this package required as input for another one, it will have to
      ;; be built again.  Thus its CGO requirements must be made available in
      ;; the environment, that is, they must be propagated.
      (propagated-inputs
       (list lua))
      (arguments
       `(#:unpack-path "github.com/aarzilli/golua"
         #:import-path "github.com/aarzilli/golua/lua"
         #:phases
         (modify-phases %standard-phases
           ;; While it's possible to fix the CGO_LDFLAGS with the "-tags"
           ;; command line argument, go-1.10+ does not re-use the produced pkg
           ;; for dependencies, which means we would need to propagate the
           ;; same "-tags" argument to all golua referrers.  A substitution is
           ;; more convenient here.  We also need to propagate the lua
           ;; dependency to make it available to referrers.
           (add-after 'unpack 'fix-lua-ldflags
             (lambda _
               (substitute* "src/github.com/aarzilli/golua/lua/lua.go"
                 (("#cgo linux,!llua,!luaa LDFLAGS: -llua5.3")
                  "#cgo linux,!llua,!luaa LDFLAGS: -llua")))))))
      (home-page "https://github.com/aarzilli/golua")
      (synopsis "Go Bindings for the Lua C API")
      (description "This package provides @code{lua}, a Go module that can
run a Lua virtual machine.")
      (license license:expat))))

(define-public go-gitlab-com-ambrevar-golua-unicode
  (let ((commit "97ce517e7a1fe2407a90c317a9c74b173d396144")
        (revision "0"))
    (package
      (name "go-gitlab-com-ambrevar-golua-unicode")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://gitlab.com/ambrevar/golua")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1izcp7p8nagjwqd13shb0020w7xhppib1a3glw2d1468bflhksnm"))))
      (build-system go-build-system)
      (native-inputs
       (list lua go-github-com-aarzilli-golua))
      (arguments
       `(#:unpack-path "gitlab.com/ambrevar/golua"
         #:import-path "gitlab.com/ambrevar/golua/unicode"
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key import-path #:allow-other-keys)
               (setenv "USER" "homeless-dude")
               (invoke "go" "test" import-path))))))
      (home-page "https://gitlab.com/ambrevar/golua")
      (synopsis "Add Unicode support to Golua")
      (description "This extension to Arzilli's Golua adds Unicode support to
all functions from the Lua string library.  Lua patterns are replaced by Go
regexps.  This breaks compatibility with Lua, but Unicode support breaks it
anyways and Go regexps are more powerful.")
      (license license:expat))))

(define-public go-github-com-yookoala-realpath
  (let ((commit "d19ef9c409d9817c1e685775e53d361b03eabbc8")
        (revision "0"))
    (package
      (name "go-github-com-yookoala-realpath")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/yookoala/realpath")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0qvz1dcdldf53rq69fli76z5k1vr7prx9ds1d5rpzgs68kwn40nw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/yookoala/realpath"))
      (home-page "https://github.com/yookoala/realpath")
      (synopsis "@code{realpath} for Golang")
      (description "This package provides @code{realpath}, a Go module that
when provided with a valid relative path / alias path, it will return you with
a string of its real absolute path in the system.")
      (license license:expat))))

(define-public go-gitlab-com-ambrevar-damerau
  (let ((commit "883829e1f25fad54015772ea663e69017cf22352")
        (revision "0"))
    (package
      (name "go-gitlab-com-ambrevar-damerau")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://gitlab.com/ambrevar/damerau")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1b9p8fypc914ij1afn6ir346zsgfqrc5mqc1k3d53n4snypq27qv"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "gitlab.com/ambrevar/damerau"))
      (home-page "https://gitlab.com/ambrevar/damerau")
      (synopsis "Damerau-Levenshtein distance for Golang")
      (description "This is a spelling corrector implementing the
Damerau-Levenshtein distance.  Takes a string value input from the user.
Looks for an identical word on a list of words, if none is found, look for a
similar word.")
      (license license:expat))))

(define-public go-github-com-stevedonovan-luar
  (let ((commit "22d247e5366095f491cd83edf779ee99a78f5ead")
        (revision "0"))
    (package
      (name "go-github-com-stevedonovan-luar")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/stevedonovan/luar")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1acjgw9cz1l0l9mzkyk7irz6cfk31wnxgbwa805fvm1rqcjzin2c"))))
      (build-system go-build-system)
      (native-inputs
       (list go-github-com-aarzilli-golua))
      (arguments
       `(#:tests? #f                    ; Upstream tests are broken.
         #:import-path "github.com/stevedonovan/luar"))
      (home-page "https://github.com/stevedonovan/luar")
      (synopsis "Lua reflection bindings for Go")
      (description "Luar is designed to make using Lua from Go more
convenient.  Go structs, slices and maps can be automatically converted to Lua
tables and vice-versa.  The resulting conversion can either be a copy or a
proxy.  In the latter case, any change made to the result will reflect on the
source.

Any Go function can be made available to Lua scripts, without having to write
C-style wrappers.

Luar support cyclic structures (lists, etc.).

User-defined types can be made available to Lua as well: their exported
methods can be called and usual operations such as indexing or arithmetic can
be performed.")
      (license license:expat))))

(define-public go-github-com-wtolson-go-taglib
  (let ((commit "6e68349ff94ecea412de7e748cb5eaa26f472777")
        (revision "0"))
    (package
      (name "go-github-com-wtolson-go-taglib")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/wtolson/go-taglib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1cpjqnrviwflz150g78iir5ndrp3hh7a93zbp4dwbg6sb2q141p2"))))
      (build-system go-build-system)
      ;; From go-1.10 onward, "pkg" compiled libraries are not re-used, so
      ;; when this package required as input for another one, it will have to
      ;; be built again.  Thus its CGO requirements must be made available in
      ;; the environment, that is, they must be propagated.
      (propagated-inputs
       (list pkg-config taglib))
      (arguments
       `(#:import-path "github.com/wtolson/go-taglib"
         ;; Tests don't pass "vet" on Go since 1.11.  See
         ;; https://github.com/wtolson/go-taglib/issues/12.
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key import-path #:allow-other-keys)
               (invoke "go" "test"
                       "-vet=off"
                       import-path))))))
      (home-page "https://github.com/wtolson/go-taglib")
      (synopsis "Go wrapper for taglib")
      (description "Go wrapper for taglib")
      (license license:unlicense))))

(define-public go-golang-org-rainycape-unidecode
  (let ((commit "cb7f23ec59bec0d61b19c56cd88cee3d0cc1870c")
        (revision "1"))
    (package
      (name "go-golang-org-rainycape-unidecode")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/rainycape/unidecode")
                       (commit commit)))
                (file-name (string-append "go-golang-org-rainycape-unidecode-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1wvzdijd640blwkgmw6h09frkfa04kcpdq87n2zh2ymj1dzla5v5"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/rainycape/unidecode"))
      (home-page "https://github.com/rainycape/unidecode")
      (synopsis "Unicode transliterator in Golang")
      (description "Unicode transliterator in Golang - Replaces non-ASCII
characters with their ASCII approximations.")
      (license license:asl2.0))))

(define-public go-github-com-akosmarton-papipes
  (let ((commit "3c63b4919c769c9c2b2d07e69a98abb0eb47fe64")
        (revision "0"))
    (package
      (name "go-github-com-akosmarton-papipes")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/akosmarton/papipes")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16p77p3d1v26qd3knxn087jqlad2qm23q8m796cdr66hrdc0gahq"))))
      (build-system go-build-system)
      (inputs
       (list pulseaudio))
      (arguments
       `(#:import-path "github.com/akosmarton/papipes"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("src/github.com/akosmarton/papipes/common.go"
                              "src/github.com/akosmarton/papipes/sink.go"
                              "src/github.com/akosmarton/papipes/source.go")
                 (("exec.Command\\(\"pactl\"")
                  (string-append "exec.Command(\""
                                 (assoc-ref inputs "pulseaudio")
                                 "/bin/pactl\""))))))))
      (home-page "https://github.com/akosmarton/papipes")
      (synopsis "Pulseaudio client library for Go")
      (description
       "This is a Pulseaudio client library in Golang for creating virtual
sinks and sources.")
      (license license:expat))))

(define-public go-github-com-mesilliac-pulse-simple
  (let ((commit "75ac54e19fdff88f4fbd82f45125134b602230b0")
        (revision "0"))
    (package
      (name "go-github-com-mesilliac-pulse-simple")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mesilliac/pulse-simple")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1awwczsa9yy99p035ckajqfs8m6mab0lz82mzlj1c5cj9lnmwplj"))))
      (build-system go-build-system)
      (propagated-inputs
       (list pkg-config pulseaudio))
      (arguments
       (list
        #:import-path "github.com/mesilliac/pulse-simple"
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'remove-examples
                       (lambda* (#:key import-path #:allow-other-keys)
                         (delete-file-recursively
                          (string-append "src/" import-path "/examples")))))))
      (home-page "https://github.com/mesilliac/pulse-simple")
      (synopsis "Cgo bindings to PulseAudio's Simple API")
      (description
       "This package provides Cgo bindings to PulseAudio's Simple API, to play
or capture raw audio.")
      (license license:expat))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
