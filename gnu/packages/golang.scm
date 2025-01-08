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
    (version "1.22.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0piy2mc3v3cadn3ls1ylcm713vjl957j0m0bj4vqggqihy7rp54g"))))
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
    (version "1.23.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aiaphmns23i0bxbaxzkh4h6nz60sxm1vs381819vfg5n2gna6dd"))))))

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

(define-public go-github-com-kataras-golog
  (package
    (name "go-github-com-kataras-golog")
    (version "0.1.7")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/kataras/golog")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ll24g50j48wqikzf67cyaq0m0f57v1ap24nbz3cmv3yzqi6wdl9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/kataras/golog"))
    (propagated-inputs
     (list go-github-com-kataras-pio))
    (home-page "https://github.com/kataras/golog")
    (synopsis "Logging foundation for Go applications")
    (description "GoLog is a level-based logger written in Go.")
    (license license:bsd-3)))

(define-public go-github-com-kataras-pio
  (package
    (name "go-github-com-kataras-pio")
    (version "0.0.10")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/kataras/pio")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11d2jy9xz4airicgmjcy4nb80kwv22jp140wzn2l5412jdr4jmkp"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/kataras/pio"))
    (home-page "https://github.com/kataras/pio")
    (synopsis "Pill for Input/Output")
    (description "PIO is a low-level package that provides a way to centralize
different output targets.  Supports colors and text decoration to all popular
terminals.")
    (license license:bsd-3)))

(define-public go-github-com-kortschak-utter
  (package
    (name "go-github-com-kortschak-utter")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kortschak/utter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13lg8gzvgjnljf1lz8qsfz3qcmbvrsxp3ip7mp2kscfz07r69dyw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kortschak/utter"))
    (home-page "https://github.com/kortschak/utter")
    (synopsis "Deep pretty printer")
    (description
     "This package implements a deep pretty printer for Go data structures to
aid data snapshotting.")
    (license license:isc)))

(define-public go-github-com-leonelquinteros-gotext
  (package
    (name "go-github-com-leonelquinteros-gotext")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/leonelquinteros/gotext")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15zjc7s1p29izagc84andzhnxw17763rax31jqvf9r5fzvlm0ccn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/leonelquinteros/gotext"))
    (propagated-inputs (list go-golang-org-x-tools go-golang-org-x-text))
    (home-page "https://github.com/leonelquinteros/gotext")
    (synopsis "GNU gettext utilities in Go")
    (description "This package implements GNU gettext utilities in Go.  It features:
@itemize
@item Implements GNU gettext support in native Go.
@item Complete support for PO files including:
@item Support for MO files.
@item Thread-safe: This package is safe for concurrent use across multiple
goroutines.
@item It works with UTF-8 encoding as it's the default for Go language.
@item Unit tests available.
@item Language codes are automatically simplified from the form en_UK to en if
the first isn't available.
@item Ready to use inside Go templates.
@item Objects are serializable to []byte to store them in cache.
@item Support for Go Modules.
@end itemize")
    (license license:expat)))

(define-public go-github-com-schachmat-ingo
  (package
    (name "go-github-com-schachmat-ingo")
    (version "0.0.0-20170403011506-a4bdc0729a3f")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/schachmat/ingo")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1gw0kddy7jh3467imsqni86cf9yq7k6vpfc0ywkbwj0zsjsdgd49"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/schachmat/ingo"))
    (home-page "https://github.com/schachmat/ingo")
    (synopsis "Go library to persist flags in a INI-like configuration file")
    (description
      "Ingo is a Go library helping you to persist flags in a INI-like
configuration file.")
    (license license:isc)))

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

(define-public go-github-com-flopp-go-findfont
  (package
    (name "go-github-com-flopp-go-findfont")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flopp/go-findfont")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05jvs5sw6yid0qr2ld7aw0n1mjp47jxhvbg9lsdig86668i2fj2q"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/flopp/go-findfont"))
    (home-page "https://github.com/flopp/go-findfont")
    (synopsis "Go font finder library")
    (description
     "This package provides a platform-agnostic Go library to locate
TrueType font files in your system's user and system font directories.")
    (license license:expat)))

(define-public go-github-com-phpdave11-gofpdi
  (package
    (name "go-github-com-phpdave11-gofpdi")
    (version "1.0.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/phpdave11/gofpdi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01r8a3k2d48fxmhyvix0ry2dc1z5xankd14yxlm496a26vfnc9nq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/phpdave11/gofpdi"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'fix-source
                     (lambda _
                       (substitute* (find-files "." "writer\\.go$")
                         (("%s-%s") "%d-%s")))))))
    (propagated-inputs (list go-github-com-pkg-errors))
    (home-page "https://github.com/phpdave11/gofpdi")
    (synopsis "PDF document importer")
    (description
     "gofpdi allows you to import an existing PDF into a new PDF.")
    (license license:expat)))

(define-public go-github-com-signintech-gopdf
  (package
    (name "go-github-com-signintech-gopdf")
    (version "0.22.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/signintech/gopdf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h6cslvid5v8fiymydj4irrzi8f91knsx8rgbzp2b8favclhwxxg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/signintech/gopdf"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'remove-examples
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively
                        (string-append "src/" import-path "/examples")))))))
    (propagated-inputs (list go-github-com-pkg-errors
                             go-github-com-phpdave11-gofpdi))
    (home-page "https://github.com/signintech/gopdf")
    (synopsis "Generating PDF documents")
    (description "gopdf is a Go library for generating PDF documents.")
    (license license:expat)))

(define-public go-github-com-wraparound-wrap
  (package
    (name "go-github-com-wraparound-wrap")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Wraparound/wrap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0scf7v83p40r9k7k5v41rwiy9yyanfv3jm6jxs9bspxpywgjrk77"))
              (patches (search-patches
                        "go-github-com-wraparound-wrap-free-fonts.patch"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Wraparound/wrap/"
      #:tests? #f                       ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key import-path #:allow-other-keys)
              (invoke "go" "install" "-v" "-x"
                      "-ldflags=-s -w"
                      (string-append import-path "cmd/wrap"))))
          (add-after 'install 'wrap-fonts
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (for-each
               (lambda (program)
                 (wrap-program program
                   `("XDG_DATA_DIRS" suffix
                     ,(map dirname
                           (search-path-as-list '("share/fonts")
                                                (map cdr inputs))))))
               (find-files (string-append (assoc-ref outputs "out")
                                          "/bin"))))))))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-signintech-gopdf
                             go-github-com-flopp-go-findfont))
    (inputs (list font-liberation font-gnu-freefont))
    (home-page "https://github.com/Wraparound/wrap")
    (synopsis "Format Fountain screenplays")
    (description
     "Wrap is a command line tool that is able to convert Fountain files into a
correctly formatted screen- or stageplay as an HTML or a PDF.  It supports
standard Fountain, but also has some custom syntax extensions such as
translated keywords and acts.")
    (license license:gpl3)))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib
  (package
    (name "go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/goptlib")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kmdpxrbnxnpsi7dkgk85z005vjyj74b3wxxqrf68wg3svy69620"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/goptlib"))
    (home-page "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/goptlib")
    (synopsis "Go pluggable transports library")
    (description "GoPtLib is a library for writing Tor pluggable transports in
Go.")
    (license license:cc0)))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-lyrebird
  (package
    (name "go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-lyrebird")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird")
                    (commit (string-append "lyrebird-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bmljd81vc8b4kzmpgmx1n1vvjn5y1s2w01hjxwplmnchv9dndkl"))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird"
       #:import-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird/cmd/lyrebird"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'substitutions
           (lambda _
             (with-directory-excursion
                 "src/gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird"
               (for-each
                (lambda (file)
                  (substitute* file
                    (("edwards25519-extra.git") "edwards25519-extra")))
                (list "common/ntor/ntor_test.go"
                      "internal/x25519ell2/x25519ell2.go"))
               (substitute* "internal/x25519ell2/x25519ell2.go"
                 (("gitlab.com/yawning/obfs4.git")
                  "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird"))))))))
    (propagated-inputs
     (list go-filippo-io-edwards25519
           go-github-com-dchest-siphash
           go-github-com-refraction-networking-utls
           go-gitlab-com-yawning-edwards25519-extra
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-webtunnel
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-text))
    (home-page "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird")
    (synopsis "Look-like nothing obfuscation protocol")
    (description "This is a look-like nothing obfuscation protocol that
incorporates ideas and concepts from Philipp Winter's ScrambleSuit protocol.")
    (license (list license:bsd-2 license:bsd-3))))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-webtunnel
  (let ((commit "e64b1b3562f3ab50d06141ecd513a21ec74fe8c6")
        (revision "0"))
    (package
      (name "go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-webtunnel")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/webtunnel")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0nvd0qp1mdy7w32arnkhghxm5k2g6gy33cxlarxc6vdm4yh6v5nv"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/webtunnel"
         #:test-subdirs '(".")))
      (home-page "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/webtunnel")
      (synopsis "Go WebTunnel Pluggable Transport")
      (description "WebTunnel is a Go Pluggable Transport that attempts to imitate
web browsing activities based on HTTP Upgrade (HTTPT).")
      (license license:bsd-2))))

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

(define-public go-github-com-rakyll-statik
  (package
    (name "go-github-com-rakyll-statik")
    (version "0.1.7")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/rakyll/statik")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y0kbzma55vmyqhyrw9ssgvxn6nw7d0zg72a7nz8vp1zly4hs6va"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            ;; Fix compatibility with go-1.18+
            (substitute* "statik.go"
              (("fmt\\.Println\\(helpText\\)")
               "fmt.Print(helpText + \"\\n\")"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rakyll/statik"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list
                "TestOpen/Files_should_retain_their_original_file*"
                "TestOpen/Images_should_successfully_unpack"
                "TestOpen/'index.html'_files_should_be_returned*"
                "TestOpen/listed_all_sub_directories_in_deep_directory"
                "TestOpen/Paths_containing_dots_should_be_properly_sanitized")
               "|"))))
    (home-page "https://github.com/rakyll/statik/")
    (synopsis "Embed files into a Go executable")
    (description "Statik allows you to embed a directory of static files into
your Go binary to be later served from an http.FileSystem.")
    (license license:asl2.0)))

(define-public go-github-com-golangplus-fmt
  (package
    (name "go-github-com-golangplus-fmt")
    (version "1.0.0")
    (home-page "https://github.com/golangplus/fmt")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "07d5kxz0f8ss3v46y0c8jg02sagi0wlaaijhjzzp0r462jyzqii7"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ; failing with new Golang compiler.
       #:import-path "github.com/golangplus/fmt"))
    (synopsis "Additions to Go's standard @code{fmt} package")
    (description "This package provides additions to Go's stdlib @code{fmt}.")
    (license license:bsd-3)))

(define-public go-github-com-motemen-go-colorine
  (let ((commit "45d19169413a019e4e2be69629dde5c7d92f8706")
        (revision "0"))
    (package
      (name "go-github-com-motemen-go-colorine")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/motemen/go-colorine")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1mdy6q0926s1frj027nlzlvm2qssmkpjis7ic3l2smajkzh07118"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/motemen/go-colorine"))
      (propagated-inputs
       `(("github.com/daviddengcn/go-colortext" ,go-github-com-daviddengcn-go-colortext)))
      (synopsis "Simple colorized console logger for golang")
      (description
       "This package provides simple colorized console logger for golang.")
      (license license:expat))))

(define-public go-github-com-daviddengcn-go-colortext
  (package
    (name "go-github-com-daviddengcn-go-colortext")
    (version "1.0.0")
    (home-page "https://github.com/daviddengcn/go-colortext")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0j5ldwg3a768d3nniiglghr9axj4p87k7f7asqxa1a688xvcms48"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/daviddengcn/go-colortext"))
    (native-inputs
     (list go-github-com-golangplus-testing))
    (synopsis "Change the color of console text and background")
    (description
     "This is a package to change the color of the text and background in the
console, working both under Windows and other systems.

Under Windows, the console APIs are used.  Otherwise, ANSI texts are output.")
    ;; dual-licensed
    (license (list license:bsd-3 license:expat))))

(define-public go-github-com-nathan-osman-go-sunrise
  (let ((commit "c8f9f1eb869135f07378e7e3c5ec7a005f806c73")
        (revision "0"))
    (package
      (name "go-github-com-nathan-osman-go-sunrise")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nathan-osman/go-sunrise")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "017zwzx05r5spxcs07dp6bnh7waknzsd819k7aqd8kr819v3x9in"))))
      (build-system go-build-system)
      (arguments
       (list #:import-path "github.com/nathan-osman/go-sunrise"))
      (home-page "https://github.com/nathan-osman/go-sunrise")
      (synopsis "Calculate sunrise and sunset times in Go")
      (description
       "This package provides a Go library for calculating sunrise and
sunset times from geographical coordinates and a date.")
      (license license:expat))))

(define-public go-github-com-hebcal-gematriya
  (let ((commit "fe3043f73e415eb82727701d10f2fb40f87675e9")
        (revision "0"))
    (package
      (name "go-github-com-hebcal-gematriya")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hebcal/gematriya")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xmnb2i80dy380yv8c4pd04bbyqgbc7c40p8hz1vqj2lhbm6jabf"))))
      (build-system go-build-system)
      (arguments
       (list #:import-path "github.com/hebcal/gematriya"))
      (home-page "https://github.com/hebcal/gematriya")
      (synopsis "Print numbers as Hebrew letters in Go")
      (description
       "This package provides a Go library for printing numbers as
Hebrew letters.")
      (license license:bsd-2))))

(define-public go-gopkg.in-tomb.v2
  (let ((commit "d5d1b5820637886def9eef33e03a27a9f166942c")
        (revision "0"))
    (package
      (name "go-gopkg.in-tomb.v2")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/go-tomb/tomb")
                      (commit commit)))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "1sv15sri99szkdz1bkh0ir46w9n8prrwx5hfai13nrhkawfyfy10"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/tomb.v2"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda _
               ;; Add a missing % to fix the compilation of this test
               (substitute* "src/gopkg.in/tomb.v2/tomb_test.go"
                 (("t.Fatalf\\(`Killf\\(\"BO%s")
                  "t.Fatalf(`Killf(\"BO%%s"))
               #t)))))
      (synopsis "@code{tomb} handles clean goroutine tracking and termination")
      (description
       "The @code{tomb} package handles clean goroutine tracking and
termination.")
      (home-page "https://gopkg.in/tomb.v2")
      (license license:bsd-3))))

(define-public go-gopkg-in-tomb-v1
  (package
    (inherit go-gopkg.in-tomb.v2)
    (name "go-gopkg-in-tomb-v1")
    (version "1.0.0-20141024135613-dd632973f1e7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gopkg.in/tomb.v1")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lqmq1ag7s4b3gc3ddvr792c5xb5k6sfn0cchr3i2s7f1c231zjv"))))
    (arguments
     (list #:import-path "gopkg.in/tomb.v1"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-test
                 (lambda* (#:key import-path #:allow-other-keys)
                   (substitute* (string-append "src/" import-path
                                               "/tomb_test.go")
                     (("t.Fatalf\\(`Killf\\(\"BO%s")
                      "t.Fatalf(`Killf(\"BO%%s")))))))
    (home-page "https://gopkg.in/tomb.v1")))

(define-public go-github-com-jtolds-gls
  (package
    (name "go-github-com-jtolds-gls")
    (version "4.20")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jtolds/gls")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k7xd2q2ysv2xsh373qs801v6f359240kx0vrl0ydh7731lngvk6"))))
    (build-system go-build-system)
    (arguments
     (list
       #:import-path "github.com/jtolds/gls"
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs #:allow-other-keys #:rest args)
               (unless
                 ;; The tests fail when run with gccgo.
                 (false-if-exception (search-input-file inputs "/bin/gccgo"))
                 (apply (assoc-ref %standard-phases 'check) args)))))))
    (synopsis "@code{gls} provides Goroutine local storage")
    (description
     "The @code{gls} package provides a way to store a retrieve values
per-goroutine.")
    (home-page "https://github.com/jtolds/gls")
    (license license:expat)))

(define-public gopls
  (package
    (name "gopls")
    ;; XXX: Starting from 0.14.0 gppls needs golang.org/x/telemetry, which
    ;; needs to be discussed if it may be included in Guix.
    (version "0.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (go-version->git-ref version #:subdir "gopls"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qksn79nc94fig5bia0l8h7fzm1zbn9rvya25hwf0f18v8a0id9l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:install-source? #f
      #:import-path "golang.org/x/tools/gopls"
      #:unpack-path "golang.org/x/tools"
      ;; XXX: No tests in project's root, limit to some of subdris, try to
      ;; enable more.
      #:test-subdirs
      #~(list "internal/protocol/..."
              "internal/util/..."
              "internal/vulncheck/...")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'unpack 'override-tools
            (lambda _
              ;; XXX: Write a procedure deleting all but current module source
              ;; to cover case with monorepo.
              (delete-file-recursively "src/golang.org/x/tools"))))))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-jba-templatecheck
           go-golang-org-x-mod
           go-golang-org-x-sync
           go-golang-org-x-telemetry
           go-golang-org-x-text
           go-golang-org-x-vuln
           go-gopkg-in-yaml-v3
           go-honnef-co-go-tools
           go-mvdan-cc-gofumpt
           go-mvdan-cc-xurls-v2))
    (home-page "https://golang.org/x/tools/gopls")
    (synopsis "Official language server for the Go language")
    (description
     "Pronounced ``Go please'', this is the official Go language server
developed by the Go team.  It provides IDE features to any LSP-compatible
editor.")
    (license license:bsd-3)))

(define-public go-github-com-google-cadvisor
  (let ((commit "2ed7198f77395ee9a172878a0a7ab92ab59a2cfd")
        (revision "0"))
    (package
      (name "go-github-com-google-cadvisor")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/cadvisor")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1w8p345z5j0gk3yiq5ah0znd5lfh348p2s624k5r10drz04p3f55"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/google/cadvisor"))
      (home-page "https://github.com/google/cadvisor")
      (synopsis "Analyze resource usage of running containers")
      (description "The package provides @code{cadvisor}, which provides
information about the resource usage and performance characteristics of running
containers.")
      (license license:asl2.0))))

(define-public go-github-com-rifflock-lfshook
  (package
    (name "go-github-com-rifflock-lfshook")
    (version "2.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rifflock/lfshook")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wxqjcjfg8c0klmdgmbw3ckagby3wg9rkga9ihd4fsf05x5scxrc"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/rifflock/lfshook"))
    (propagated-inputs
     (list go-github-com-sirupsen-logrus))
    (home-page "https://github.com/rifflock/lfshook")
    (synopsis "Local File System hook for Logrus logger")
    (description "This package provides a hook for Logrus to write directly to
a file on the file system.  The log levels are dynamic at instantiation of the
hook, so it is capable of logging at some or all levels.")
    (license license:expat)))

(define-public go-github-com-kardianos-osext
  (let ((commit "ae77be60afb1dcacde03767a8c37337fad28ac14")
        (revision "1"))
    (package
      (name "go-github-com-kardianos-osext")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kardianos/osext")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "056dkgxrqjj5r18bnc3knlpgdz5p3yvp12y4y978hnsfhwaqvbjz"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/kardianos/osext"
         ;; The tests are flaky:
         ;; <https://github.com/kardianos/osext/issues/21>
         #:tests? #f))
      (synopsis "Find the running executable")
      (description "Osext provides a method for finding the current executable
file that is running.  This can be used for upgrading the current executable or
finding resources located relative to the executable file.")
      (home-page "https://github.com/kardianos/osext")
      (license license:bsd-3))))

(define-public go-github-com-docker-distribution
  (let ((commit "325b0804fef3a66309d962357aac3c2ce3f4d329")
        (revision "0"))
    (package
      (name "go-github-com-docker-distribution")
      (version (git-version "0.0.0" revision commit))
      (source
       ;; FIXME: This bundles many things, see
       ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31881#41>.
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/docker/distribution")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yg2zrikn3vkvkx5mn51p6bfjk840qdkn7ahhhvvcsc8mpigrjc6"))))
      (build-system go-build-system)
      (native-inputs
       (list go-golang-org-x-sys go-github-com-sirupsen-logrus
             go-golang-org-x-crypto))
      (arguments
       '(#:import-path "github.com/docker/distribution"))
      (home-page
       "https://github.com/docker/distribution")
      (synopsis "This package is a Docker toolset to pack, ship, store, and
deliver content")
      (description "Docker Distribution is a Docker toolset to pack, ship,
store, and deliver content.  It contains Docker Registry 2.0 and libraries
to interact with distribution components.")
      (license license:asl2.0))))

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

(define-public go-github-com-cli-safeexec
  (package
    (name "go-github-com-cli-safeexec")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cli/safeexec")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j6hspjx9kyxn98nbisawx6wvbi1d6rpzr6p2rzhllm673wibwr3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/cli/safeexec"))
    (home-page "https://github.com/cli/safeexec")
    (synopsis "Safe implementation of Go's exec.Command")
    (description "This package provides a Go module that provides a stabler
alternative to @@code{exec.LookPath()}.")
    (license license:bsd-2)))

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

(define-public go-github-com-michiwend-golang-pretty
  (let ((commit "8ac61812ea3fa540f3f141a444fcb0dd713cdca4")
        (revision "0"))
    (package
      (name "go-github-com-michiwend-golang-pretty")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/michiwend/golang-pretty")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0rjfms0csjqi91xnddzx3rcrcaikc7xc027617px3kdwdap80ir4"))))
      (build-system go-build-system)
      (native-inputs
       (list go-github-com-kr-text))
      (arguments
       `(#:tests? #f                    ; Upstream tests seem to be broken.
         #:import-path "github.com/michiwend/golang-pretty"))
      (home-page "https://github.com/michiwend/golang-pretty")
      (synopsis "Pretty printing for Go values")
      (description "Package @code{pretty} provides pretty-printing for Go
values.  This is useful during debugging, to avoid wrapping long output lines
in the terminal.

It provides a function, @code{Formatter}, that can be used with any function
that accepts a format string.  It also provides convenience wrappers for
functions in packages @code{fmt} and @code{log}.")
      (license license:expat))))

(define-public go-github-com-michiwend-gomusicbrainz
  (let ((commit "0cdeb13f9b24d2c714feb7e3c63d595cf7121d7d")
        (revision "0"))
    (package
      (name "go-github-com-michiwend-gomusicbrainz")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/michiwend/gomusicbrainz")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1li9daw0kghb80rdmxbh7g72qhxcvx3rvhwq5gs0jrr9hb8pjvcn"))))
      (build-system go-build-system)
      (native-inputs
       (list go-github-com-michiwend-golang-pretty go-github-com-kr-text))
      (arguments
       `(#:import-path "github.com/michiwend/gomusicbrainz"))
      (home-page "https://github.com/michiwend/gomusicbrainz")
      (synopsis "MusicBrainz WS2 client library for Golang")
      (description "Currently GoMusicBrainz provides methods to perform search
and lookup requests.  Browse requests are not supported yet.")
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

(define-public go-github-com-mr-tron-base58
  (let ((commit "d724c80ecac7b49e4e562d58b2b4f4ee4ed8c312")
        (revision "0"))
    (package
      (name "go-github-com-mr-tron-base58")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mr-tron/base58")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "12qhgnn9wf3c1ang16r4i778whk4wsrj7d90h2xgmz4fi1469rqa"))))
      (build-system go-build-system)
      (arguments
       `(#:unpack-path "github.com/mr-tron/base58"
         #:import-path "github.com/mr-tron/base58/base58"))
      (home-page "https://github.com/mr-tron/base58")
      (synopsis "Fast implementation of base58 encoding on Golang")
      (description "Fast implementation of base58 encoding on Golang.  A
trivial @command{big.Int} encoding benchmark results in 6 times faster
encoding and 8 times faster decoding.")
      (license license:expat))))

(define-public go-github-com-spaolacci-murmur3
  (package
    (name "go-github-com-spaolacci-murmur3")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spaolacci/murmur3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1lv3zyz3jy2d76bhvvs8svygx66606iygdvwy5cwc0p5z8yghq25"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/spaolacci/murmur3"))
    (home-page "https://github.com/spaolacci/murmur3")
    (synopsis "Native MurmurHash3 Go implementation")
    (description "Native Go implementation of Austin Appleby's third MurmurHash
revision (aka MurmurHash3).

Reference algorithm has been slightly hacked as to support the streaming mode
required by Go's standard Hash interface.")
    (license license:bsd-3)))

(define-public go-github-com-sabhiram-go-gitignore
  (let ((commit "525f6e181f062064d83887ed2530e3b1ba0bc95a")
        (revision "1"))
    (package
      (name "go-github-com-sabhiram-go-gitignore")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sabhiram/go-gitignore")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "197giv3snczvbihzvkja5pq53yw5fc516rnjm71hni8gawb8jmh3"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/sabhiram/go-gitignore"))
      (native-inputs
       (list go-github-com-stretchr-testify))
      (home-page "https://github.com/sabhiram/go-gitignore")
      (synopsis "Gitignore parser for Go")
      (description "A @command{.gitignore} parser for Go.")
      (license license:expat))))

(define-public go-github-com-go-md2man
  (package
    (name "go-github-com-go-md2man")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cpuguy83/go-md2man")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gqlkv1pv8cpvcj8g77d1hzy5bnp5a3k3xs02iahlr3a65m4azsi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cpuguy83/go-md2man"))
    (propagated-inputs
     (list go-github-com-russross-blackfriday-v2))
    (home-page "https://github.com/cpuguy83/go-md2man")
    (synopsis "Convert markdown into roff")
    (description
     "Go-md2man is a Go program that converts markdown to roff for the purpose
of building man pages.")
    (license license:expat)))

(define-public go-github-com-git-lfs-go-netrc
  (let ((commit "f0c862dd687a9d9a7e15b3cd7cb3fd3e81cdd5ef")
        (revision "0"))
    (package
      (name "go-github-com-git-lfs-go-netrc")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/git-lfs/go-netrc")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xvnjyg54gm3m3qszkfp12id0jmpg3583nqvv2llza1nr18w1sqi"))))
      (build-system go-build-system)
      (arguments `(#:import-path "github.com/git-lfs/go-netrc/netrc"
                   #:unpack-path "github.com/git-lfs/go-netrc"))
      (home-page "https://github.com/git-lfs/go-netrc")
      (synopsis "Netrc file parser for Go")
      (description "This package is for reading and writing netrc files.  This
package can parse netrc files, make changes to them, and then serialize them
back to netrc format, while preserving any whitespace that was present in the
source file.")
      (license license:expat))))

(define-public go-github-com-rubyist-tracerx
  (let ((commit "787959303086f44a8c361240dfac53d3e9d53ed2")
        (revision "0"))
    (package
      (name "go-github-com-rubyist-tracerx")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rubyist/tracerx")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xj5213r00zjhb7d2l6wlwv62g6mss50jwjpf7g8fk8djv3l29zz"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/rubyist/tracerx"))
      (home-page "https://github.com/rubyist/tracerx/")
      (synopsis "Output tracing information in your Go app")
      (description "This package is a simple tracing application that logs
messages depending on environment variables.  It is very much inspired by git's
GIT_TRACE mechanism.")
      (license license:expat))))

(define-public go-github-com-shurcool-sanitized-anchor-name
  (package
    (name "go-github-com-shurcool-sanitized-anchor-name")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shurcooL/sanitized_anchor_name")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gv9p2nr46z80dnfjsklc6zxbgk96349sdsxjz05f3z6wb6m5l8f"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/shurcooL/sanitized_anchor_name"))
    (home-page "https://github.com/shurcooL/sanitized_anchor_name")
    (synopsis "Create sanitized anchor names")
    (description "This package provides a Go program for creating sanitized
anchor names.")
    (license license:expat)))

(define-public go-github-com-lucasb-eyer-go-colorful
  (package
    (name "go-github-com-lucasb-eyer-go-colorful")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lucasb-eyer/go-colorful")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08c3fkf27r16izjjd4w94xd1z7w1r4mdalbl53ms2ka2j465s3qs"))))
    (build-system go-build-system)
    (propagated-inputs (list go-golang-org-x-image))
    (arguments
     (list #:import-path "github.com/lucasb-eyer/go-colorful"))
    (home-page "https://github.com/lucasb-eyer/go-colorful")
    (synopsis "Library for playing with colors in Go")
    (description
     "The colorful package provides a library for using colors in Go.
It stores colors in RGB and provides methods for converting these to
various color spaces.")
    (license license:expat)))

(define-public go-github-com-cention-sany-utf7
  (package
    (name "go-github-com-cention-sany-utf7")
    (version "0.0.0-20170124080048-26cad61bd60a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cention-sany/utf7")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jy15ryfcln1iwchrksqyrnyfy41gisymm4f9sr1d73ja029bznm"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/cention-sany/utf7"))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://github.com/cention-sany/utf7")
    (synopsis "UTF-7 for Go")
    (description
     "The utf7 package provides support for the obsolete UTF-7 text
encoding in Go.")
    (license license:bsd-3)))

(define-public go-github-com-cespare-mph
  (package
    (name "go-github-com-cespare-mph")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cespare/mph")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mvd6bkvf3i3555kqkkr3k9jd4c25scjq4xad35sxpny8f72nbg1"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/cespare/mph"))
    (home-page "https://github.com/cespare/mph")
    (synopsis "Minimal perfect hashing in Go")
    (description
     "@code{mph} is a Go package that implements a minimal perfect hash table
over strings.")
    (license license:expat)))

(define-public go-github-com-xo-terminfo
  (package
    (name "go-github-com-xo-terminfo")
    (version "0.0.0-20210125001918-ca9a967f8778")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xo/terminfo")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05gdcvcbwcrcwxznhvs1q1xh4irz2d10v2mz179pydjh30kjc0j5"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/xo/terminfo"))
    (home-page "https://github.com/xo/terminfo")
    (synopsis "Read the terminfo database in Go")
    (description
     "The terminfo package implements terminfo database reading for Go.")
    (license license:expat)))

(define-public go-github-com-lunixbochs-vtclean
  (package
    (name "go-github-com-lunixbochs-vtclean")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lunixbochs/vtclean")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jqn33l1kzy4dk66zbvxz7rlgkgg34s9mhc8z0lrz0i88466zhd8"))))
    (build-system go-build-system)
    (arguments (list #:import-path "github.com/lunixbochs/vtclean"))
    (home-page "https://github.com/lunixbochs/vtclean")
    (synopsis "Filter out terminal escape sequences")
    (description
     "The @code{vtclean} provides the @command{vtclean} command and a library
designed to clean up raw terminal output by stripping escape sequences,
optionally preserving color.")
    (license license:expat)))

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

(define-public go-github-com-golang-freetype
  (let ((commit "e2365dfdc4a05e4b8299a783240d4a7d5a65d4e4")
        (revision "1"))
    (package
      (name "go-github-com-golang-freetype")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/golang/freetype")
                       (commit commit)))
                (file-name (string-append "go-github-com-golang-freetype-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "194w3djc6fv1rgcjqds085b9fq074panc5vw582bcb8dbfzsrqxc"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/golang/freetype"))
      (propagated-inputs
       (list go-golang-org-x-image))
      (home-page "https://github.com/golang/freetype")
      (synopsis "Freetype font rasterizer in the Go programming language")
      (description "The Freetype font rasterizer in the Go programming language.")
      (license (list license:freetype
                     license:gpl2+)))))

(define-public go-github-com-fogleman-gg
  (package
    (name "go-github-com-fogleman-gg")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/fogleman/gg")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nkldjghbqnzj2djfaxhiv35kk341xhcrj9m2dwq65v684iqkk8n"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f      ; Issue with test flags.
       #:import-path "github.com/fogleman/gg"))
    (propagated-inputs
     (list go-github-com-golang-freetype))
    (home-page "https://github.com/fogleman/gg")
    (synopsis "2D rendering in Go")
    (description "@code{gg} is a library for rendering 2D graphics in pure Go.")
    (license license:expat)))

(define-public go-github-com-gedex-inflector
  (let ((commit "16278e9db8130ac7ec405dc174cfb94344f16325")
        (revision "1"))
    (package
      (name "go-github-com-gedex-inflector")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/gedex/inflector")
                       (commit commit)))
                (file-name (string-append "go-github-com-gedex-inflector-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "05hjqw1m71vww4914d9h6nqa9jw3lgjzwsy7qaffl02s2lh1amks"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gedex/inflector"))
      (home-page "https://github.com/gedex/inflector")
      (synopsis "Go library that pluralizes and singularizes English nouns")
      (description "Go library that pluralizes and singularizes English nouns.")
      (license license:bsd-2))))

(define-public go-github-com-surge-glog
  (let ((commit "2578deb2b95c665e6b1ebabf304ce2085c9e1985")
        (revision "1"))
    (package
      (name "go-github-com-surge-glog")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/surge/glog")
                       (commit commit)))
                (file-name (string-append "go-github-com-surge-glog-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1bxcwxvsvr2hfpjz9hrrn0wrgykwmrbyk567102k3vafw9xdcwk4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/surge/glog"))
      (home-page "https://github.com/surge/glog")
      (synopsis "Leveled execution logs for Go")
      (description "Leveled execution logs for Go.")
      (license license:asl2.0))))

(define-public go-github-com-surgebase-porter2
  (let ((commit "56e4718818e8dc4ea5ba6348402fc7661863732a")
        (revision "1"))
    (package
      (name "go-github-com-surgebase-porter2")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/surgebase/porter2")
                       (commit commit)))
                (file-name (string-append "go-github-com-surgebase-porter2-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1ivcf83jlj9s7q5y9dfbpyl0br35cz8fcp0dm8sxxvqh54py06v2"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/surgebase/porter2"))
      (native-inputs
       (list go-github-com-stretchr-testify go-github-com-surge-glog))
      (home-page "https://github.com/surgebase/porter2")
      (synopsis "Go library implementing english Porter2 stemmer")
      (description "Porter2 implements the
@url{http://snowball.tartarus.org/algorithms/english/stemmer.html, english
Porter2 stemmer}.  It is written completely using finite state machines to do
suffix comparison, rather than the string-based or tree-based approaches.")
      (license license:asl2.0))))

(define-public go-github-com-olekukonko-ts
  (let ((commit "78ecb04241c0121483589a30b0814836a746187d")
        (revision "0"))
    (package
      (name "go-github-com-olekukonko-ts")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/olekukonko/ts")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0k88n5rvs5k5zalbfa7c71jkjb8dhpk83s425z728qn6aq49c978"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/olekukonko/ts"
         #:tests? #f)) ; inappropriate ioctl for device.
      (home-page "https://github.com/olekukonko/ts/")
      (synopsis "Simple Go application to get the size of the terminal")
      (description "This package provides a simple Go application to get the
size of the terminal.")
      (license license:expat))))

(define-public go-github-com-alcortesm-tgz
  (let ((commit "9c5fe88206d7765837fed3732a42ef88fc51f1a1")
        (revision "1"))
    (package
      (name "go-github-com-alcortesm-tgz")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alcortesm/tgz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04dcwnz2c2i4wbq2vx3g2wrdgqpncr2r1h6p1k08rdwk4bq1h8c5"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (substitute* "tgz_test.go"
                      ;; Fix format error
                      (("t.Fatalf\\(\"%s: unexpected error extracting: %s\", err\\)")
                       "t.Fatalf(\"%s: unexpected error extracting: %s\", com, err)"))
                    #t))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/alcortesm/tgz"
         #:phases
         (modify-phases %standard-phases
           ;; Files are test fixtures, not generated.
           (delete 'reset-gzip-timestamps))))
      (home-page "https://github.com/alcortesm/tgz/")
      (synopsis "Go library to extract tgz files to temporal directories")
      (description "This package provides a Go library to extract tgz files to
temporal directories.")
      (license license:expat))))

(define-public go-github-com-twpayne-go-shell
  (package
    (name "go-github-com-twpayne-go-shell")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/go-shell")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hv0ggy3935iddjnmpp9vl0kqjknxpnbmm9w7xr3gds7fpbxz6yp"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/twpayne/go-shell"))
    (home-page "https://github.com/twpayne/go-shell/")
    (synopsis "Shell across multiple platforms")
    (description
     "Package @code{shell} returns a user's shell across multiple platforms.")
    (license license:expat)))

(define-public go-github-com-twpayne-go-vfsafero
  (package
    (name "go-github-com-twpayne-go-vfsafero")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/go-vfsafero")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18jwxhlrjd06z8xzg9ij0irl4f79jfy5jpwiz6xqlhzb1fja19pw"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/twpayne/go-vfsafero"))
    (native-inputs
     (list go-github-com-twpayne-go-vfs-1.0.1 go-github-com-spf13-afero-1.1.2))
    (home-page "https://github.com/twpayne/go-vfsafero/")
    (synopsis "Compatibility later between @code{go-vfs} and @code{afero}")
    (description
     "Package @code{vfsafero} provides a compatibility later between
@code{go-github-com-twpayne-go-vfs} and @code{go-github-com-spf13-afero}.")
    (license license:expat)))

(define-public go-github-com-twpayne-go-xdg-v3
  (package
    (name "go-github-com-twpayne-go-xdg-v3")
    (version "3.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/go-xdg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j8q7yzixs6jlaad0lpa8hs6b240gm2cmy0yxgnprrbpa0y2r7ln"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/twpayne/go-xdg/v3"))
    (native-inputs
     (list go-github-com-stretchr-testify go-github-com-twpayne-go-vfs-1.0.1))
    (home-page "https://github.com/twpayne/go-xdg/")
    (synopsis "Functions related to freedesktop.org")
    (description "Package @code{xdg} provides functions related to
@uref{freedesktop.org}.")
    (license license:expat)))

(define-public go-github-com-delthas-go-libnp
  (let ((commit "0e45ece1f878f202fee2c74801e287804668f677"))
    (package
      (name "go-github-com-delthas-go-libnp")
      (version (git-version "0.0.0" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/delthas/go-libnp")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                  (base32
                    "1hylpvwz3kb8wr00knba6mggjacak2vmqafwysansj0ns038lp8w"))))
      (build-system go-build-system)
      (arguments `(#:import-path "github.com/delthas/go-libnp"))
      (propagated-inputs (list go-github-com-godbus-dbus-v5))
      (home-page "https://github.com/delthas/go-libnp")
      (synopsis "Tiny library providing information about now-playing media")
      (description "@code{go-libnp} is a tiny cross-platform library for
extracting information about the music/image/video that is Now Playing on the
system.")
      (license license:expat))))

(define-public go-github-com-zalando-go-keyring
  (package
    (name "go-github-com-zalando-go-keyring")
    (version "0.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zalando/go-keyring")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p6qlsbj9rmqiwz9ly4c7jmifcx8m45xjhsbdwdvw2jzw5jc2ch1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zalando/go-keyring"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; Disable tests which require a system DBus instance.
                  (("TestDelete") "OffTestDelete")
                  (("TestGet") "OffTestGet")
                  (("TestSet") "OffTestSet")))))
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "dbus-run-session" "--"
                          "go" "test" "-v" "./..."))))))))
    (native-inputs
     (list dbus))
    (propagated-inputs
     (list go-github-com-godbus-dbus-v5))
    (home-page "https://github.com/zalando/go-keyring/")
    (synopsis "Library for working with system keyring")
    (description "@code{go-keyring} is a library for setting, getting and
deleting secrets from the system keyring.")
    (license license:expat)))

(define-public go-github-com-kardianos-minwinsvc
  (package
    (name "go-github-com-kardianos-minwinsvc")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kardianos/minwinsvc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0z941cxymkjcsj3p5l3g4wm2da3smz7iyqk2wbs5y8lmxd4kfzd8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kardianos/minwinsvc"))
    (home-page "https://github.com/kardianos/minwinsvc/")
    ;; some packages (Yggdrasil) need it to compile
    ;; it's a tiny package and it's easier to bundle it than to patch it out
    (synopsis "Minimal windows only service stub for Go")
    (description "Go programs designed to run from most *nix style operating
systems can import this package to enable running programs as services without
modifying them.")
    (license license:zlib)))

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

(define-public go-git-sr-ht-adnano-go-gemini
  (package
    (name "go-git-sr-ht-adnano-go-gemini")
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~adnano/go-gemini")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mv4x4cfwyhh77wfb3r221bhr84x4nmjpgysnvvjgmbnnafsgfns"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "git.sr.ht/~adnano/go-gemini"))
    (propagated-inputs
     (list go-golang-org-x-net go-golang-org-x-text))
    (home-page "https://git.sr.ht/~adnano/go-gemini")
    (synopsis "Gemini protocol in Go")
    (description
     "The @code{gemini} package implements the Gemini protocol in Go.  It
provides an API similar to that of NET/HTTP to facilitate the development of
Gemini clients and servers.")
    (license license:expat)))

(define-public gofumpt
  (package
    (name "gofumpt")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mvdan/gofumpt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13ahi8q1a9h4dj6a7xp95c79d5svz5p37b6z91aswbq043qd417k"))
              (modules '((guix build utils)))
              (snippet `(let ((fixed-version (string-append ,version
                                                            " (GNU Guix)")))
                          ;; Gofumpt formats Go files, and therefore modifies
                          ;; them. To help the developers diagnose issues, it
                          ;; replaces any occurrence of a `//gofumpt:diagnose`
                          ;; comment with some debugging information which
                          ;; includes the module version. In the event gofumpt
                          ;; was built without module support, it falls back
                          ;; to a string "(devel)". Since our build system
                          ;; does not yet support modules, we'll inject our
                          ;; version string instead, since this is more
                          ;; helpful.
                          (substitute* "internal/version/version.go"
                            (("^const fallbackVersion.+")
                             (format #f "const fallbackVersion = \"~a\"~%"
                                     fixed-version)))
                          ;; These tests rely on `//gofumpt:diagnose` comments
                          ;; being replaced with fixed information injected
                          ;; from the test scripts, but this requires a binary
                          ;; compiled as a Go module. Since we can't do this
                          ;; yet, modify the test scripts with the version
                          ;; string we're injecting.
                          (delete-file "testdata/script/diagnose.txtar")
                          (substitute* (find-files "testdata/script/"
                                                   "\\.txtar$")
                            (("v0.0.0-20220727155840-8dda8068d9f3")
                             fixed-version)
                            (("(devel)")
                             fixed-version)
                            (("v0.3.2-0.20220627183521-8dda8068d9f3")
                             fixed-version))))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "mvdan.cc/gofumpt"))
    (native-inputs (list go-gopkg-in-errgo-v2))
    (propagated-inputs (list go-github-com-pkg-diff
                             go-github-com-kr-text
                             go-github-com-kr-pretty
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-golang-org-x-mod
                             go-github-com-rogpeppe-go-internal
                             go-github-com-google-go-cmp
                             go-github-com-frankban-quicktest))
    (home-page "https://mvdan.cc/gofumpt/")
    (synopsis "Formats Go files with a stricter ruleset than gofmt")
    (description
     "Enforce a stricter format than @code{gofmt}, while being backwards compatible.
That is, @code{gofumpt} is happy with a subset of the formats that
@code{gofmt} is happy with.")
    (license license:bsd-3)))

(define-public go-mvdan-cc-gofumpt
  (package
    (inherit gofumpt)
    (name "go-mvdan-cc-gofumpt")
    (arguments
     `(#:import-path "mvdan.cc/gofumpt"
       #:tests? #f
       #:install-source? #t
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (propagated-inputs (package-inputs gofumpt))
    (native-inputs '())
    (inputs '())))

(define-public unparam
  (package
    (name "unparam")
    (version "0.0.0-20240528143540-8a5130ca722f")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mvdan/unparam")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qrwszcmb5slbzkq3acw57b896z22zwkv6cf6ldxwlc6p179g009"))))
    (build-system go-build-system)
    (arguments
     `(;; FIXME: <...>-go-1.21.5/lib/go/src/runtime/cgo/cgo.go:33:8: could not
       ;; import C (no metadata for C)
       ;; <...>-go-1.21.5/lib/go/src/net/cgo_linux.go:12:8: could not import C
       ;; (no metadata for C)
       #:tests? #f
       #:import-path "mvdan.cc/unparam"))
    (inputs (list go-github-com-pkg-diff go-golang-org-x-tools
                  go-github-com-rogpeppe-go-internal))
    (home-page "https://mvdan.cc/unparam/")
    (synopsis "Find unused parameters in Go")
    (description "Reports unused function parameters and results in Go code.")
    (license license:bsd-3)))

(define-public go-mvdan-cc-unparam
  (package
    (inherit unparam)
    (name "go-mvdan-cc-unparam")
    (arguments
     `(#:import-path "github.com/mvdan/unparam"
       #:tests? #f
       #:install-source? #t
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (propagated-inputs (package-inputs unparam))
    (native-inputs '())
    (inputs '())))

(define-public go-github-com-valyala-bytebufferpool
  (package
    (name "go-github-com-valyala-bytebufferpool")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/bytebufferpool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01lqzjddq6kz9v41nkky7wbgk7f1cw036sa7ldz10d82g5klzl93"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/valyala/bytebufferpool"))
    (home-page "https://github.com/valyala/bytebufferpool")
    (synopsis "Anti-memory-waste byte buffer pool for Golang")
    (description
     "@code{bytebufferpool} implements a pool of byte buffers with
anti-fragmentation protection.")
    (license license:expat)))

(define-public go-github-com-valyala-tcplisten
  (package
    (name "go-github-com-valyala-tcplisten")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/tcplisten")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fv5hxmq1jwrjn1rdjvbmjrrkb601zcdh01qhx6d8l7ss6n05zb8"))))
    (build-system go-build-system)
    (arguments
     ;; NOTE: (Sharlatan-20211218T165504+0000): Tests failing:
     ;;
     ;;   tcplisten_test.go:56: cannot create listener 0 using Config
     ;;   &tcplisten.Config{ReusePort:false, DeferAccept:false, FastOpen:false,
     ;;   Backlog:32}: lookup ip6-localhost on [::1]:53: read udp
     ;;   [::1]:33932->[::1]:53: read: connection refused
     ;;
     '(#:tests? #f
       #:import-path "github.com/valyala/tcplisten"))
    (home-page "https://github.com/valyala/tcplisten")
    (synopsis "Customizable TCP net.Listener for Go")
    (description
     "@code{tcplisten} provides customizable TCP net.Listener with various
performance-related options.")
    (license license:expat)))

(define-public go-github-com-vmihailenco-tagparser
  (package
    (name "go-github-com-vmihailenco-tagparser")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmihailenco/tagparser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13arliaz3b4bja9jj7cr5ax4zvxaxm484fwrn0q6d6jjm1l35m1k"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/vmihailenco/tagparser"))
    (home-page "https://github.com/vmihailenco/tagparser")
    (synopsis "Tag parser for Golang")
    (description "This package is a simple Golang implementation of tag
parser.")
    (license license:bsd-2)))

(define-public go-github-com-mtibben-percent
  (package
    (name "go-github-com-mtibben-percent")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mtibben/percent")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1iqivw8pigj259rj5yifibbvic70f9hb7k24a4sa967s4fj6agb6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mtibben/percent"
       #:phases %standard-phases))
    (synopsis "Package percent escapes strings using percent-encoding")
    (description
     "Package percent escapes strings using percent-encoding.")
    (home-page "https://github.com/mtibben/percent")
    (license license:expat)))

(define-public aws-vault
  (package
    (name "aws-vault")
    (version "7.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/99designs/aws-vault")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dqg6d2k8r80ww70afghf823z0pijha1i0a0c0c6918yb322zkj2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/99designs/aws-vault"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'patch-version
            (lambda _
              (substitute* "src/github.com/99designs/aws-vault/main.go"
                (("var Version = \"dev\"")
                 (string-append "var Version = \"v" #$version "\"")))))
          (add-after 'build 'contrib
            (lambda* (#:key import-path #:allow-other-keys)
              (let* ((zsh-site-dir
                      (string-append #$output "/share/zsh/site-functions"))
                     (bash-completion-dir
                      (string-append #$output "/share/bash-completion/completions"))
                     (fish-completion-dir
                      (string-append #$output "/share/fish/completions")))
                (for-each mkdir-p (list bash-completion-dir
                                        fish-completion-dir
                                        zsh-site-dir))
                (with-directory-excursion
                    (string-append "src/" import-path "/contrib/completions")
                  (copy-file "zsh/aws-vault.zsh"
                             (string-append zsh-site-dir "/_aws-vault"))
                  (copy-file "bash/aws-vault.bash"
                             (string-append bash-completion-dir "/aws-vault"))
                  (copy-file "fish/aws-vault.fish"
                             (string-append fish-completion-dir "/aws-vault.fish"))))))
          ;; aws-vault: error: add: mkdir /homeless-shelter: permission
          ;; denied.
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list go-github-com-99designs-keyring
           go-github-com-alecthomas-kingpin-v2
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-credentials
           go-github-com-aws-aws-sdk-go-v2-service-iam
           go-github-com-aws-aws-sdk-go-v2-service-sso
           go-github-com-aws-aws-sdk-go-v2-service-ssooidc
           go-github-com-aws-aws-sdk-go-v2-service-sts
           go-github-com-google-go-cmp
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-tty
           go-github-com-skratchdot-open-golang
           go-golang-org-x-term
           go-gopkg-in-ini-v1))
    (home-page "https://github.com/99designs/aws-vault")
    (synopsis "Vault for securely storing and accessing AWS credentials")
    (description
     "AWS Vault is a tool to securely store and access @acronym{Amazon Web
Services,AWS} credentials.

AWS Vault stores IAM credentials in your operating system's secure keystore and
then generates temporary credentials from those to expose to your shell and
applications.  It's designed to be complementary to the AWS CLI tools, and is
aware of your profiles and configuration in ~/.aws/config.")
    (license license:expat)))

(define-public go-github-com-gsterjov-go-libsecret
  (package
    (name "go-github-com-gsterjov-go-libsecret")
    (version "0.0.0-20161001094733-a6f4afe4910c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gsterjov/go-libsecret")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09zaiadnll83vs22ib89agg7anj0blw5fywvmckxllsgif6ak6v7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gsterjov/go-libsecret"))
    (propagated-inputs
     (list go-github-com-godbus-dbus))
    (home-page "https://github.com/gsterjov/go-libsecret")
    (synopsis "Manage secrets via the @code{Secret Service} DBus API")
    (description
     "This native Go library manages secrets via the freedesktop.org
@code{Secret Service} DBus interface.")
    (license license:expat)))

(define-public go-github-com-go-ini-ini
  (package
    (name "go-github-com-go-ini-ini")
    (version "1.67.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-ini/ini")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vpzkjmrwp7bqqsijp61293kk2vn6lcck56j8m5y6ks6cf21lpap"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/go-ini/ini"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://gopkg.in/ini.v1")
    (synopsis "INI file read and write functionality in Go")
    (description
     "This package provides INI file read and write functionality in Go.")
    (license license:asl2.0)))

;; XXX: This repository has been archived by the owner on Dec 29, 2022. It is
;; now read-only.  It's only used by kiln, consider to remove it when it does
;; no longer require it.
(define-public go-github-com-google-shlex
  (package
    (name "go-github-com-google-shlex")
    (version "0.0.0-20191202100458-e7afc7fbc510")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/shlex")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14z8hqyik910wk2qwnzgz8mjsmiamxa0pj55ahbv0jx6j3dgvzfm"))))
    (build-system go-build-system)
    (arguments (list #:import-path "github.com/google/shlex"))
    (home-page "https://github.com/google/shlex")
    (synopsis "Simple lexer for Go")
    (description
     "@code{shlex} implements a simple lexer which splits input into tokens
using shell-style rules for quoting and commenting.")
    (license license:asl2.0)))

(define-public go-github-com-peterbourgon-diskv
  (package
    (name "go-github-com-peterbourgon-diskv")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/peterbourgon/diskv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pdy8f7bkm65gx4vknwcvfa619hknflqxkdlvmf427k2mzm91gmh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/peterbourgon/diskv"))
    (propagated-inputs (list go-github-com-google-btree))
    (home-page "https://github.com/peterbourgon/diskv")
    (synopsis "Disk-backed key-value store")
    (description
     "Diskv (disk-vee) is a simple, persistent key-value store written in the Go
language.  It starts with a simple API for storing arbitrary data on a filesystem by
key, and builds several layers of performance-enhancing abstraction on top.  The end
result is a conceptually simple, but highly performant, disk-backed storage system.")
    (license license:expat)))

(define-public go-github-com-disintegration-imaging
  (package
    (name "go-github-com-disintegration-imaging")
    (version "1.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/disintegration/imaging")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sl201nmk601h0aii4234sycn4v2b0rjxf8yhrnik4yjzd68q9x5"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/disintegration/imaging"))
    (inputs (list go-golang-org-x-image))
    (home-page "https://github.com/disintegration/imaging")
    (synopsis "Simple image processing for Go")
    (description "This package provides basic image processing functions
(resize, rotate, crop, brightness/contrast adjustments, etc.).")
    (license license:expat)))

(define notmuch-fixtures
  (origin
    (method url-fetch)
    (uri "http://notmuchmail.org/releases/test-databases/database-v1.tar.xz")
    (sha256
     (base32
      "1lk91s00y4qy4pjh8638b5lfkgwyl282g1m27srsf7qfn58y16a2"))))

(define-public go-github-com-riywo-loginshell
  (package
    (name "go-github-com-riywo-loginshell")
    (version "0.0.0-20200815045211-7d26008be1ab")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/riywo/loginshell")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "138yvis6lipw9x02jyiz7472bxi20206bcfikcar54i3xsww9q4i"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/riywo/loginshell"
           ;; Tests try to get the current user's login shell; the build
           ;; user doesn't have one.
           #:tests? #f))
    (home-page "https://github.com/riywo/loginshell")
    (synopsis "Get the user's login shell in Go")
    (description
     "The loginshell package provides a Go library to get the login shell
of the current user.")
    (license license:expat)))

(define-public go-github-com-ssgelm-cookiejarparser
  (package
    (name "go-github-com-ssgelm-cookiejarparser")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ssgelm/cookiejarparser")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fnm53br0cg3iwzniil0lh9w4xd6xpzfypwfpdiammfqavlqgcw4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/ssgelm/cookiejarparser"))
    (propagated-inputs (list go-golang-org-x-net))
    (home-page "https://github.com/ssgelm/cookiejarparser")
    (synopsis "Parse a curl cookiejar with Go")
    (description
     "This package is a Go library that parses a curl (netscape) cookiejar
file into a Go http.CookieJar.")
    (license license:expat)))

(define-public go-github-com-ssor-bom
  (package
    (name "go-github-com-ssor-bom")
    (version "0.0.0-20170718123548-6386211fdfcf")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ssor/bom")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09g5496ifwqxqclh2iw58plcwcz0sczlnxwqxzwmnl4shdl371ld"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/ssor/bom"))
    (home-page "https://github.com/ssor/bom")
    (synopsis "Cleaning BOMs in Go")
    (description
     "The bom package provides small tools for cleaning BOMs from a byte
array or reader.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
