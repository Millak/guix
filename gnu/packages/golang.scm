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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages bootstrap) #:select (glibc-dynamic-linker))
  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
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

            (add-after 'unpack 'remove-failing-test
              (lambda _
                ;; This test fails with newer gcc's
                ;; https://github.com/golang/go/issues/57691
                (substitute* "src/cmd/cgo/internal/testsanitizers/asan_test.go"
                  ((".*arena_fail.*") ""))))

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
    (version "1.22.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f0fr92z3l3szmxf3wvh20w1sqayvd927gawdp5d44cc44pd6c0n"))))
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
    (version "1.23.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06c5cjjqk95p16cb6p8fgqqsddc1a1kj3w2m0na5v91gvwxbd0pq"))))))

(define-public go-1.24
  (package
    (inherit go-1.23)
    (name "go")
    (version "1.24.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b24pdsxrarw22gffv85sghpgvgamafvwwrvvhmyv3hqf89m97zk"))))
    (native-inputs
     ;; Go 1.24 and later requires Go 1.22+ as the bootstrap toolchain.
     (alist-replace "go" (list go-1.22) (package-native-inputs go-1.23)))))

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
(define-public go-std-1.24 (make-go-std go-1.24))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
