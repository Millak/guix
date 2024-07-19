;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2023 Efraim Flashner <efraim@flashner.co.il>
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
                             (list "powerpc-linux" "i586-gnu")))))

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
     (alist-replace "go" (list go-1.17) (package-native-inputs go-1.17)))))

(define-public go-1.21
  (package
    (inherit go-1.20)
    (name "go")
    (version "1.21.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/go")
                    (commit (string-append "go" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f11kya6rpqfldpw82g0yiknz657i655d3c0yh3qy6f8xa8x7zn2"))))
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
    (version "1.22.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p6v5dl4mzlrma6v1a26d8zr4csq5mm10d9sdhl3kn9d22vphql1"))))
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
     (alist-replace "go"
                    (list go-1.21)
                    (package-native-inputs go-1.21)))))

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

(define-public go-github-com-agext-levenshtein
  (package
    (name "go-github-com-agext-levenshtein")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/agext/levenshtein")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a26c8pp9h5w66bhd9vb6lpvmhp30mz46pnh3a8vrjx50givb2lw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/agext/levenshtein"))
    (home-page "https://github.com/agext/levenshtein")
    (synopsis "Calculating the Levenshtein distance between two strings in Go")
    (description
     "Package levenshtein implements distance and similarity metrics for
strings, based on the Levenshtein measure.")
    (license license:asl2.0)))

(define-public go-github-com-apparentlymart-go-textseg-v13
  (package
    (name "go-github-com-apparentlymart-go-textseg-v13")
    (version "13.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apparentlymart/go-textseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gdgi0d52rq1xsdn9icc8lghn0f2q927cifmrlfxflf7bf21vism"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/apparentlymart/go-textseg/v13"
       #:import-path "github.com/apparentlymart/go-textseg/v13/textseg"))
    (native-inputs
     (list ruby))
    (home-page "https://github.com/apparentlymart/go-textseg")
    (synopsis "Go implementation of Unicode Text Segmentation")
    (description
     "This package provides an implementation of the Unicode Text Segmentation
specification for Go.  Specifically, it currently includes only the grapheme
cluster segmentation algorithm.")
    ;; Project is released under Expat terms.  Some parts use Unicode and
    ;; ASL2.0 licenses.
    (license (list license:expat license:unicode license:asl2.0))))

(define-public go-github-com-apparentlymart-go-textseg-autoversion
  (package
    (inherit go-github-com-apparentlymart-go-textseg-v13)
    (name "go-github-com-apparentlymart-go-textseg-autoversion")
    (arguments
     '(#:unpack-path "github.com/apparentlymart/go-textseg/autoversion"
       #:import-path "github.com/apparentlymart/go-textseg/autoversion/textseg"))))

(define-public go-github-com-avast-retry-go
  (let ((commit "a322e24d96313ab405dec28ad5711f036c6d25a3")
        (revision "0"))
    (package
      (name "go-github-com-avast-retry-go")
      (version (git-version "2.4.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/avast/retry-go")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hb4b1668516a4gv8avmflr565b6c1h93phdb068hcjxxj8767ba"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/avast/retry-go"
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'remove-examples
                       (lambda* (#:key import-path #:allow-other-keys)
                         (delete-file-recursively
                          (string-append "src/" import-path "/examples")))))))
      (propagated-inputs (list go-github-com-stretchr-testify))
      (home-page "https://github.com/avast/retry-go")
      (synopsis "Simple golang library for retry mechanism")
      (description "This package is a simple Go library that provides retry
functionality for functions that may fail.  It includes various customizable
retry strategies, such as fixed delay, backoff delay, and random delay.")
      (license license:expat))))

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
           go-github-com-operatorfoundation-monolith-go
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

(define-public go-github-com-hanwen-go-fuse-v2
  (let ((commit "915cf5413cdef5370ae3f953f8eb4cd9ac176d5c")
        (revision "0"))
    (package
      (name "go-github-com-hanwen-go-fuse-v2")
      (version (git-version "2.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hanwen/go-fuse")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ik0yvs9m40vxccpb0rpxc22fyqmcgyysc7w0yl9kn3jyr6qa1d5"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/hanwen/go-fuse/v2"))
      (native-inputs (list
                      go-golang-org-x-sys
                      go-golang-org-x-sync
                      go-github-com-kylelemons-godebug))
      (home-page "https://github.com/hanwen/go-fuse")
      (synopsis "Go bindings for FUSE filesystems")
      (description
       "This is a repository containing Go bindings for writing FUSE file systems.")
      (license license:bsd-3))))

(define-public go-github-com-jacobsa-reqtrace
  (let ((commit "245c9e0234cb2ad542483a336324e982f1a22934")
        (revision "0"))
    (package
      (name "go-github-com-jacobsa-reqtrace")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jacobsa/reqtrace")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0zfyijig10896v42rvxka1n4wn6lijqz40y2281187l7mq8vv5jn"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/jacobsa/reqtrace"))
      (inputs (list
               go-golang-org-x-net))
      (home-page "https://github.com/jacobsa/reqtrace")
      (synopsis "Simple request tracing framework")
      (description
       "Package reqtrace contains a very simple request tracing framework.")
      (license license:asl2.0))))

(define-public go-github-com-jcmturner-gofork
  (package
    (name "go-github-com-jcmturner-gofork")
    (version "1.7.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jcmturner/gofork")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w1j6b671121r6md5w7hnh2d0sa332pw5q49yihw23wdfinknyin"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jcmturner/gofork"))
    (home-page "https://github.com/jcmturner/gofork")
    (synopsis "Modified Go standard library packages")
    (description
     "This repository contains modified Go standard library packages for use as work
arounds until issues are addressed in the official distribution.")
    (license license:bsd-3)))

(define-public go-github-com-jcmturner-rpc
  (package
    (name "go-github-com-jcmturner-rpc")
    (version "2.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jcmturner/rpc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nm4j2nwcszghldw39rwdx2hr56i1lybfpv33y4gd67w6qcqbpsi"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jcmturner/rpc"
       ;; Source-only package.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; Source-only package.
         (delete 'build))))
    (propagated-inputs
     (list go-golang-org-x-net go-github-com-stretchr-testify))
    (home-page "https://github.com/jcmturner/rpc")
    (synopsis "Remote Procedure Call libraries")
    (description
     "This package provides a partial Go implementation of the Remote Call
Procedure libraries, presented in
@@url{http://pubs.opengroup.org/onlinepubs/9629399/,CDE 1.1: Remote Procedure
Call}.")
    (license license:asl2.0)))

(define-public go-github-com-jcmturner-rpc-v2-ndr
  (package
    (inherit go-github-com-jcmturner-rpc)
    (name "go-github-com-jcmturner-rpc-v2-ndr")
    (arguments
     `(#:import-path "github.com/jcmturner/rpc/v2/ndr"
       #:unpack-path "github.com/jcmturner/rpc"))))

(define-public go-github-com-jcmturner-rpc-v2-mstypes
  (package
    (inherit go-github-com-jcmturner-rpc)
    (name "go-github-com-jcmturner-rpc-v2-mstypes")
    (arguments
     `(#:import-path "github.com/jcmturner/rpc/v2/mstypes"
       #:unpack-path "github.com/jcmturner/rpc"))))

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

(define-public go-github-com-pkg-xattr
  (package
    (name "go-github-com-pkg-xattr")
    (version "0.4.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pkg/xattr")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qg4zh0d8m4adaiicsd0cpw0w6g8sk01f4jz7jyxgirh1wfcsqyz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/pkg/xattr"))
    (native-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/pkg/xattr")
    (synopsis "Support for extended file system attributes")
    (description
     "Package xattr provides support for extended attributes on Linux, Darwin and
FreeBSD.  Extended attributes are name:value pairs permanently associated with
files or directories.  They are similar to the environment strings associated with
a process.  An attribute may be defined or undefined.  If defined, its value may
be empty or non-empty.  You can find more details here:
@@url{https://en.wikipedia.org/wiki/Extended_file_attributes,
https://en.wikipedia.org/wiki/Extended_file_attributes}
.")
    (license license:bsd-2)))

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

(define-public go-github-com-savsgio-gotils
  (let ((commit "52f3993e8d6d2629f18e7b7383b7f54a3d3f1d1f")
        (revision "0"))
    (package
      (name "go-github-com-savsgio-gotils")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/savsgio/gotils")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qr7i62h53frcig26vj027r2hn9zxsjzd7113wvbxy7qpprjjbjb"))))
      (build-system go-build-system)
      (native-inputs
       (list go-github-com-google-uuid
             go-github-com-valyala-bytebufferpool))
      (arguments
       '(#:import-path "github.com/savsgio/gotils"
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda arguments
               (for-each
                (lambda (directory)
                  (apply (assoc-ref %standard-phases 'build)
                         `(,@arguments #:import-path ,directory)))
                (list
                 "github.com/savsgio/gotils/bytes"
                 "github.com/savsgio/gotils/encoding/base64"
                 "github.com/savsgio/gotils/math"
                 "github.com/savsgio/gotils/nocopy"
                 "github.com/savsgio/gotils/strconv"
                 "github.com/savsgio/gotils/strings"
                 "github.com/savsgio/gotils/sync"
                 "github.com/savsgio/gotils/time"
                 "github.com/savsgio/gotils/uuid"))))
           (replace 'check
             (lambda arguments
               (for-each
                (lambda (directory)
                  (apply (assoc-ref %standard-phases 'check)
                         `(,@arguments #:import-path ,directory)))
                (list
                 "github.com/savsgio/gotils/bytes"
                 "github.com/savsgio/gotils/encoding/base64"
                 "github.com/savsgio/gotils/math"
                 "github.com/savsgio/gotils/nocopy"
                 "github.com/savsgio/gotils/strconv"
                 "github.com/savsgio/gotils/strings"
                 "github.com/savsgio/gotils/sync"
                 "github.com/savsgio/gotils/time"
                 "github.com/savsgio/gotils/uuid"))))
           (replace 'install
             (lambda arguments
               (for-each
                (lambda (directory)
                  (apply (assoc-ref %standard-phases 'install)
                         `(,@arguments #:import-path ,directory)))
                (list
                 "github.com/savsgio/gotils/bytes"
                 "github.com/savsgio/gotils/encoding/base64"
                 "github.com/savsgio/gotils/math"
                 "github.com/savsgio/gotils/nocopy"
                 "github.com/savsgio/gotils/strconv"
                 "github.com/savsgio/gotils/strings"
                 "github.com/savsgio/gotils/sync"
                 "github.com/savsgio/gotils/time"
                 "github.com/savsgio/gotils/uuid")))))))
      (home-page "https://github.com/savsgio/gotils")
      (synopsis "Golang utilities")
      (description
       "Golang utilities to make your life easier with zero allocations.")
      (license license:asl2.0))))

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
        (base32 "066bqlgw5h7a3kxswqlv734asb7nw2y6snsn09yqk0ixj23qw22s"))))
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

(define-public go-github-com-dpotapov-go-spnego
  (let ((commit "298b63a544303a239753d04314aada5bdbad7e4a")
        (revision "0"))
    (package
      (name "go-github-com-dpotapov-go-spnego")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/dpotapov/go-spnego")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0d3b0kazm0jskfml0pkhjn2v49m8dvqj4zymm49ldgvkhl9hcf6w"))))
      (build-system go-build-system)
      (arguments `(#:import-path "github.com/dpotapov/go-spnego"))
      (propagated-inputs (list go-github-com-stretchr-testify
                               go-github-com-jcmturner-gokrb5-v8
                               go-golang-org-x-net))
      (home-page "https://github.com/dpotapov/go-spnego")
      (synopsis "Simple golang library for retry mechanism")
      (description "This package is a simple Go library that provides retry
functionality for functions that may fail.  It includes various customizable
retry strategies, such as fixed delay, backoff delay, and random delay.")
      (license license:expat))))

(define-public go-github-com-deckarep-golang-set
  (package
    (name "go-github-com-deckarep-golang-set")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/deckarep/golang-set")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y64c0p6a7ww5jp6adm6fm97vsni86njw8wkwxfmciy466vhl0lf"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/deckarep/golang-set"))
    (home-page "https://github.com/deckarep/golang-set")
    (synopsis "Set type for Go")
    (description "Set is the set collection for the Go language.")
    (license license:expat)))

(define-public go-howett-net-plist
  (package
    (name "go-howett-net-plist")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DHowett/go-plist")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gcrxkmdj87xq01458asgxvvijrkih74ydbzfmir1p16xr9z0x39"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "howett.net/plist"))
    (propagated-inputs
     (list go-github-com-jessevdk-go-flags
           go-gopkg-in-check-v1))
    (home-page "https://github.com/DHowett/go-plist")
    (synopsis "Apple property list transcoder")
    (description
     "This list transcoder supports encoding/decoding property lists (Apple
XML, Apple Binary, OpenStep, and GNUStep) from/to arbitrary Go types.")
    (license license:giftware)))

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
     `(#:unpack-path "github.com/OperatorFoundation/obfs4"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'build)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/OperatorFoundation/obfs4/common/csrand"
               "github.com/OperatorFoundation/obfs4/common/drbg"
               "github.com/OperatorFoundation/obfs4/common/log"
               "github.com/OperatorFoundation/obfs4/common/ntor"
               "github.com/OperatorFoundation/obfs4/common/probdist"
               "github.com/OperatorFoundation/obfs4/common/pt_extras"
               "github.com/OperatorFoundation/obfs4/common/replayfilter"
               "github.com/OperatorFoundation/obfs4/common/socks5"
               "github.com/OperatorFoundation/obfs4/common/termmon"
               "github.com/OperatorFoundation/obfs4/common/uniformdh"
               "github.com/OperatorFoundation/obfs4/modes/pt_socks5"
               "github.com/OperatorFoundation/obfs4/modes/stun_udp"
               "github.com/OperatorFoundation/obfs4/modes/transparent_tcp"
               "github.com/OperatorFoundation/obfs4/modes/transparent_udp"
               "github.com/OperatorFoundation/obfs4/obfs4proxy"
               "github.com/OperatorFoundation/obfs4/proxy_dialers/proxy_http"
               "github.com/OperatorFoundation/obfs4/proxy_dialers/proxy_socks4"
               "github.com/OperatorFoundation/obfs4/transports"))))
         (replace 'check
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'check)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/OperatorFoundation/obfs4/common/csrand"
               "github.com/OperatorFoundation/obfs4/common/drbg"
               "github.com/OperatorFoundation/obfs4/common/log"
               "github.com/OperatorFoundation/obfs4/common/ntor"
               "github.com/OperatorFoundation/obfs4/common/probdist"
               "github.com/OperatorFoundation/obfs4/common/pt_extras"
               "github.com/OperatorFoundation/obfs4/common/replayfilter"
               "github.com/OperatorFoundation/obfs4/common/socks5"
               "github.com/OperatorFoundation/obfs4/common/termmon"
               "github.com/OperatorFoundation/obfs4/common/uniformdh"
               ;; ERROR: Println arg dialFn is a func value, not called.
               ;;"github.com/OperatorFoundation/obfs4/modes/pt_socks5"
               ;; ERROR: Infof format %s has arg ln of wrong type *net.UDPConn.
               ;;"github.com/OperatorFoundation/obfs4/modes/stun_udp"
               "github.com/OperatorFoundation/obfs4/modes/transparent_tcp"
               ;; ERROR: Infof format %s has arg ln of wrong type *net.UDPConn
               ;;"github.com/OperatorFoundation/obfs4/modes/transparent_udp"
               ;; ERROR: Println call has possible formatting directive %s.
               ;;"github.com/OperatorFoundation/obfs4/obfs4proxy"
               "github.com/OperatorFoundation/obfs4/proxy_dialers/proxy_http"
               "github.com/OperatorFoundation/obfs4/proxy_dialers/proxy_socks4"
               "github.com/OperatorFoundation/obfs4/transports"))))
         (replace 'install
           (lambda arguments
             (for-each
              (lambda (directory)
                (apply (assoc-ref %standard-phases 'install)
                       `(,@arguments #:import-path ,directory)))
              (list
               "github.com/OperatorFoundation/obfs4/common/csrand"
               "github.com/OperatorFoundation/obfs4/common/drbg"
               "github.com/OperatorFoundation/obfs4/common/log"
               "github.com/OperatorFoundation/obfs4/common/ntor"
               "github.com/OperatorFoundation/obfs4/common/probdist"
               "github.com/OperatorFoundation/obfs4/common/pt_extras"
               "github.com/OperatorFoundation/obfs4/common/replayfilter"
               "github.com/OperatorFoundation/obfs4/common/socks5"
               "github.com/OperatorFoundation/obfs4/common/termmon"
               "github.com/OperatorFoundation/obfs4/common/uniformdh"
               "github.com/OperatorFoundation/obfs4/modes/pt_socks5"
               "github.com/OperatorFoundation/obfs4/modes/stun_udp"
               "github.com/OperatorFoundation/obfs4/modes/transparent_tcp"
               "github.com/OperatorFoundation/obfs4/modes/transparent_udp"
               "github.com/OperatorFoundation/obfs4/obfs4proxy"
               "github.com/OperatorFoundation/obfs4/proxy_dialers/proxy_http"
               "github.com/OperatorFoundation/obfs4/proxy_dialers/proxy_socks4"
               "github.com/OperatorFoundation/obfs4/transports")))))))
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
  (let ((commit "19f41278d0c9251d64e0ee29f37d51e87a24a97b")
        (revision "0"))
    (package
      (name "go-github-com-willscott-goturn")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/willscott/goturn")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zwvhfznr84ayzknn9flh65nvqjsixisgy9fkhz2jlahl1ldqcq7"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/willscott/goturn"))
      (home-page "https://github.com/willscott/goturn")
      (synopsis "Go TURN dialer")
      (description "GoTURN is a library providing a Go interface compatible with
the golang proxy package which connects through a TURN relay.  It provides
parsing and encoding support for STUN and TURN protocols.")
      (license license:bsd-3))))

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
    (synopsis "go-findfont")
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
          (add-after 'wrap 'wrap-fonts
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
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird")
                    (commit (string-append "lyrebird-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rifg5kgqp4c3b44j48fjmx00m00ai7fa4gaqrgphiqs1fc5586s"))))
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
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-text))
    (home-page "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird")
    (synopsis "Look-like nothing obfuscation protocol")
    (description "This is a look-like nothing obfuscation protocol that
incorporates ideas and concepts from Philipp Winter's ScrambleSuit protocol.")
    (license (list license:bsd-2 license:bsd-3))))

(define-public go-github-com-sevlyar-go-daemon
  (package
    (name "go-github-com-sevlyar-go-daemon")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/sevlyar/go-daemon")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        ;; XXX: Remove when updating
        '(begin
           (substitute* "compilation_test.go"
             ((".*\"darwin/386\".*") ""))))
       (sha256
        (base32 "1y3gnxaifykcjcbzx91lz9bc93b95w3xj4rjxjbii26pm3j7gqyk"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/sevlyar/go-daemon"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/sevlyar/go-daemon")
    (synopsis "Library for writing system daemons")
    (description "Go-Daemon is a library for writing system daemons in Go.")
    (license license:expat)))

(define-public go-github-com-keybase-go-ps
  (let ((commit "91aafc93ba19d1988cff338c1929d35b6c6f5b50")
        (revision "0"))
    (package
      (name "go-github-com-keybase-go-ps")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/keybase/go-ps")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1la7m9pd1rrij727g34k9d2iapqwrkwdkqwpkbsbcq8ig0fg634h"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/keybase/go-ps"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (substitute* (find-files "." "test\\.go")
                 (("/bin/sleep" command)
                  (string-append
                   (assoc-ref (or native-inputs inputs) "coreutils")
                   command)))
               (substitute* "src/github.com/keybase/go-ps/process_openbsd.go"
                 (("^// \\+build ignore") "")))))))
      (native-inputs
       (list coreutils go-github-com-stretchr-testify))
      (home-page "https://github.com/keybase/go-ps")
      (synopsis "Process list library for Go")
      (description "Go-Ps is a library for Go that implements OS-specific APIs
to list and manipulate processes in a safe way.")
      (license license:expat))))

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

(define-public go-github-com-emersion-go-autostart
  (let ((commit "00ed301c8e9ae79e82878c6361c709983ac5dd2c")
        (revision "0"))
    (package
      (name "go-github-com-emersion-go-autostart")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/emersion/go-autostart")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0cqqvbzn32xv5lknfygrx01rx2sc6pi833k7008nlk9lsfgry06v"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/emersion/go-autostart"))
      (home-page "https://github.com/emersion/go-autostart")
      (synopsis "Autostart library in Go")
      (description "Go-Autostart is a Go library to run a command after login.")
      (license license:expat))))

(define-public go-github-com-dchest-siphash
  (package
    (name "go-github-com-dchest-siphash")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/dchest/siphash")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08s076y7vmjqnq7jz0762hkm896r6r31v8b31a3gy0n8rfa01k8k"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/dchest/siphash"))
    (home-page "https://github.com/dchest/siphash")
    (synopsis "Go library for pseudorandom functions")
    (description "SipHash is a family of pseudorandom functions (PRFs) optimized
for speed on short messages.")
    (license license:cc0)))

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
     `(#:import-path "github.com/rakyll/statik"))
    (home-page "https://github.com/rakyll/statik/")
    (synopsis "Embed files into a Go executable")
    (description "Statik allows you to embed a directory of static files into
your Go binary to be later served from an http.FileSystem.")
    (license license:asl2.0)))

(define-public go-github-com-alsm-ioprogress
  (let ((commit "063c3725f436e7fba0c8f588547bee21ffec7ac5")
        (revision "0"))
    (package
      (name "go-github-com-alsm-ioprogress")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/alsm/ioprogress")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10ym5qlq77nynmkxbk767f2hfwyxg2k7hrzph05hvgzv833dhivh"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/alsm/ioprogress"))
      (synopsis "Textual progress bars in Go")
      (description "@code{ioprogress} is a Go library with implementations of
@code{io.Reader} and @code{io.Writer} that draws progress bars.  The primary use
case for these are for command-line applications but alternate progress bar
writers can be supplied for alternate environments.")
      (home-page "https://github.com/alsm/ioprogress")
      (license license:expat))))

(define-public go-github-com-miolini-datacounter
  (package
    (name "go-github-com-miolini-datacounter")
    (version "1.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/miolini/datacounter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s1hxqy6666qd524rdp1dr3778davc8gx9brg9lkcjvr5l05s9wa"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/miolini/datacounter"))
    (home-page "https://github.com/miolini/datacounter")
    (synopsis "Counters for Go readers and writers")
    (description
     "The datacounter package provides counters for Go readers and writers.")
    (license license:expat)))

(define-public go-github-com-emersion-go-textwrapper
  (package
    (name "go-github-com-emersion-go-textwrapper")
    (version "0.0.0-20200911093747-65d896831594")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/go-textwrapper")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lh9d7zvj6gm1rr6sv5xlagklgx9d666hq5srd37a4sdcjkbiqmq"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-textwrapper"))
    (home-page "https://github.com/emersion/go-textwrapper")
    (synopsis "Text-wrapping writer for Go")
    (description
     "The textwrapper package provides a writer that wraps long text lines to
a specified length.")
    (license license:expat)))

(define-public go-github-com-aki237-nscjar
  (let ((commit "e2df936ddd6050d30dd90c7214c02b5019c42f06")
        (revision "0"))
    (package
      (name "go-github-com-aki237-nscjar")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/aki237/nscjar")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03y7zzq12qvhsq86lb06sgns8xrkblbn7i7wd886wk3zr5574b96"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/aki237/nscjar"))
      (synopsis "Handle Netscape / Mozilla cookies")
      (description "@code{nscjar} is a Go library used to parse and output
Netscape/Mozilla's old-style cookie files.  It also implements a simple cookie
jar struct to manage the cookies added to the cookie jar.")
      (home-page "https://github.com/aki237/nscjar")
      (license license:expat))))

(define-public go-github-com-gizak-termui
  (package
    (name "go-github-com-gizak-termui")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gizak/termui")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v3k8l5p95kb1v297ra5mw9sxdd59y82y6ibjzya5ma2pry6k5cn"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/gizak/termui"
       #:import-path "github.com/gizak/termui/v3"))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth
           go-github-com-mitchellh-go-wordwrap go-github.com-nsf-termbox-go))
    (home-page "https://github.com/gizak/termui")
    (synopsis "Terminal dashboard widget Go library")
    (description
     "The termui Go library draws customizable dashboard widgets in a text
terminal.  It includes several common widgets: lists, trees, tables and tabs,
but also more complex items such as (stacked) bar and pie charts, scatter plots,
gauges, and even images and a canvas for drawing `high resolution' braille dots.

You can also easily create new custom widgets.  Widgets can be coloured and
styled and positioned absolutely or relatively.  They respond to keyboard,
mouse, and terminal resizing events.")
    (license license:expat)))

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

(define-public go-github-com-mitchellh-go-wordwrap
  (package
    (name "go-github-com-mitchellh-go-wordwrap")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/go-wordwrap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12imq66hgj8q9ii2xqdy8apc0icphh6yimjb0div1pvl3s9gn83y"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mitchellh/go-wordwrap"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2))
    (home-page "https://github.com/mitchellh/go-wordwrap")
    (synopsis "Go library for word-wrapping strings")
    (description
     "This Go library automatically wraps words onto multiple lines.  It's
primary goal is to format command-line output, but of course word wrapping is a
generally useful thing to do.")
    (license license:expat)))

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

(define-public go-github-com-leodido-go-urn
  (package
    (name "go-github-com-leodido-go-urn")
    (version "1.4.0")
    (home-page "https://github.com/leodido/go-urn")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bn9dj6y299jdh8szfim32yxj9zip38cqgv965dj23cixgr7baxb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/leodido/go-urn"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (synopsis "Parser for uniform resource names as seen on RFC 2141")
    (description
     "This package implements a parser for uniform resource names (URN) as
specified by @uref{https://tools.ietf.org/html/rfc2141, IETF RFC 2141}.")
    (license license:expat)))

(define-public go-github-com-jessevdk-go-flags
  (package
    (name "go-github-com-jessevdk-go-flags")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jessevdk/go-flags")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "13ixw1yx4bvcj66lkc8zgwf9j7gkvj686g991gycdsafvdvca0lj"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jessevdk/go-flags"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; SOURCE_DATE_EPOCH messes with the date on the man page test.
             (substitute* "src/github.com/jessevdk/go-flags/help_test.go"
               (("TestMan") "DisabledTestMan")))))))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (synopsis "Go library for parsing command line arguments")
    (description
     "The @code{flags} package provides a command line option parser.  The
functionality is similar to the go builtin @code{flag} package, but
@code{flags} provides more options and uses reflection to provide a succinct
way of specifying command line options.")
    (home-page "https://github.com/jessevdk/go-flags")
    (license license:bsd-3)))

(define-public go-github-com-go-playground-locales
  (package
    (name "go-github-com-go-playground-locales")
    (version "0.13.0")
    (home-page "https://github.com/go-playground/locales")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qydcpkvss3mf8mk3xzg6a34n8i69aydrigcl2apifrkx72jw7pf"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-playground/locales"))
    (propagated-inputs
     (list go-golang-org-x-text))
    (synopsis "Set of locales generated from the CLDR Unicode Project")
    (description
     "This package provides a set of locales generated from the
@uref{http://cldr.unicode.org/, Unicode CLDR Project} which can be used
independently or within an internalization (i18n) package.  Its currently
implemented features include

@itemize
@item Rules generated from the CLDR data, v31.0.3
@item Contains Cardinal, Ordinal and Range Plural Rules
@item Contains Month, Weekday and Timezone translations built in
@item Contains Date & Time formatting functions
@item Contains Number, Currency, Accounting and Percent formatting functions
@item Supports the \"Gregorian\" calendar only
@end itemize")
    (license license:expat)))

(define-public go-github-com-go-playground-universal-translator
  (package
    (name "go-github-com-go-playground-universal-translator")
    (version "0.18.1")
    (home-page "https://github.com/go-playground/universal-translator")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lgz9wrkcfx6q3x6i9fprr8rfwnk0c6x61jgzacgikbmzsl7dw6v"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-playground/universal-translator"))
    (propagated-inputs
     (list go-github-com-go-playground-locales))
    (synopsis "Translator using Unicode CLDR data and pluralization rules")
    (description
     "This package offers an Internalization Translator for Go using
@uref{http://cldr.unicode.org/, Unicode CLDR Project} data and pluralization
rules.  Its currently implemented features include

@itemize
@item Rules generated from the CLDR data, v30.0.3
@item Contains Cardinal, Ordinal and Range Plural Rules
@item Contains Month, Weekday and Timezone translations built in
@item Contains Date & Time formatting functions
@item Contains Number, Currency, Accounting and Percent formatting functions
@item Supports the \"Gregorian\" calendar only
@item Support loading translations from files
@item Exporting translations to file(s), mainly for getting them
professionally translated
@end itemize")
    (license license:expat)))

(define-public go-gopkg-in-go-playground-validator-v9
  (package
    (name "go-gopkg-in-go-playground-validator-v9")
    (version "9.31.0")
    (home-page "https://gopkg.in/go-playground/validator.v9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-playground/validator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f8c77s8kx9rip2jarv27x5s4xkcmanh4ndyhbcwvrhncs5rq061"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/go-playground/validator.v9"))
    (native-inputs
     (list go-gopkg-in-go-playground-assert-v1))
    (propagated-inputs
     (list go-github-com-go-playground-universal-translator
           go-github-com-leodido-go-urn))
    (synopsis "Validator for structs and individual fields based on tags")
    (description
     "This package implements value validations for structs and individual
fields based on tags.  It has the following unique features:

@itemize
@item Cross Field and Cross Struct validations by using validation tags or
custom validators
@item Slice, Array and Map diving, which allows any or all levels of a
multidimensional field to be validated
@item Ability to dive into both map keys and values for validation
@item Handles type interface by determining it's underlying type prior to validation
@item Handles custom field types such as sql driver
@uref{https://golang.org/src/database/sql/driver/types.go?s=1210:1293#L29,
Valuer}
@item Alias validation tags, which allows for mapping of several validations
to a single tag for easier defining of validations on structs
@item Extraction of custom defined Field Name e.g. can specify to extract the
JSON name while validating and have it available in the resulting FieldError
@item Customizable i18n aware error messages.
@item Default validator for the @uref{https://github.com/gin-gonic/gin, gin}
web framework
@end itemize")
    (license license:expat)))

(define-public go-github-com-go-playground-validator-v10
  (package
    (inherit go-gopkg-in-go-playground-validator-v9)
    (name "go-github-com-go-playground-validator-v10")
    (version "10.22.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-playground/validator")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zff0qpqfzwa4xazppiq7jvpncnmx52m23qi4ih754b7rzhbk0iz"))))
    (arguments
     (list
      #:import-path "github.com/go-playground/validator/v10"))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs
                     go-gopkg-in-go-playground-validator-v9)
       (append go-github-com-gabriel-vasile-mimetype
               go-golang-org-x-crypto
               go-golang-org-x-text)))
    (native-inputs
     (list go-github-com-go-playground-assert-v2))))

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

(define-public go-github-com-hebcal-hebcal-go
  (let ((commit "d42e881860cfc9e8249fc79f268091c3c4d36b0d")
        (revision "0"))
    (package
      (name "go-github-com-hebcal-hebcal-go")
      (version (git-version "0.9.11" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hebcal/hebcal-go")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1m9akb8pwxchpaci05gambshrzw626gsrfhl25f36vjl7mq5292n"))))
      (build-system go-build-system)
      (arguments
       (list #:import-path "github.com/hebcal/hebcal-go"
             ;; Source-only package
             #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 ;; Source-only package
                 (delete 'build))))
      (native-inputs
       (list go-github-com-stretchr-testify))
      (propagated-inputs
       (list go-github-com-hebcal-gematriya
             go-github-com-nathan-osman-go-sunrise))
      (home-page "https://github.com/hebcal/hebcal-go")
      (synopsis "Go library for the Hebcal perpetual Jewish calendar")
      (description
       "This package provides a library for conversion between Hebrew
and Gregorian dates, and generation of lists of Jewish holidays for
a given year.  Shabbat and holiday candle lighting and havdalah times
are approximated based on location.

Torah readings, Daf Yomi, and counting of the Omer can also be
specified.  Algorithms are included to calculate yahrzeits, birthdays,
and anniversaries.")
      (license license:gpl2+))))

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

(define-public go-github.com-jtolds-gls
  (package
    (name "go-github.com-jtolds-gls")
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

(define-public go-github-com-saracen-walker
  (package
    (name "go-github-com-saracen-walker")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/saracen/walker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rq1lrp99lx7k1ysbfznn4c1iagnxdhb4lnnklsadnnzi3gvygqz"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/saracen/walker"))
    (inputs
     (list go-golang-org-x-sync))
    (home-page "https://github.com/saracen/walker")
    (synopsis "Faster, parallel version of Go's filepath.Walk")
    (license license:expat)
    (description "The @code{walker} function is a faster, parallel version, of
@code{filepath.Walk}")))

(define-public go-github-com-tdewolff-hasher
  (package
    (name "go-github-com-tdewolff-hasher")
    (version "0.0.0-20210521220142-bc97f602bca2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tdewolff/hasher")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12dmxpmdy2z7c2z7qv2mv2aq4hyvjncb6fzr0ymg3y5bfjvl4dcw"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/tdewolff/hasher"))
    (native-inputs
     (list go-github-com-cespare-mph
           go-github-com-dgryski-go-mph))
    (home-page "https://github.com/tdewolff/hasher")
    (synopsis "Go known-keys fast-lookup map generator")
    (description
     "Hasher is a tool to automate the creation of methods and tables for a
@code{string} to @code{uint32} mapper.")
    (license license:bsd-3)))

(define-public go-github-com-tj-docopt
  (package
    (name "go-github-com-tj-docopt")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tj/docopt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06h8hdg1mh3s78zqlr01g4si7k0f0g6pr7fj7lnvfg446hgc7080"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tj/docopt"))
    (synopsis "Go implementation of docopt")
    (description
     "This library allows the user to define a command-line interface from a
program's help message rather than specifying it programmatically with
command-line parsers.")
    (home-page "https://github.com/tj/docopt")
    (license license:expat)))

(define-public go-golang-org-x-vuln
  (package
    (name "go-golang-org-x-vuln")
    ;; XXX: Newer version of govulncheck requires golang.org/x/telemetry,
    ;; which needs to be discussed if it may be included in Guix.
    (version "0.0.0-20230110180137-6ad3e3d07815")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/vuln")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fhz27ni8bs872rgvqq700qacak9v45zy0fh2hilq21sk6dks72r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; it tires to download modules from the network
      #:import-path "golang.org/x/vuln"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v"
                          "./doc/..."
                          "./internal/..."
                          "./scan/..."))))))))
    (propagated-inputs
     (list go-github-com-google-go-cmdtest
           go-github-com-google-go-cmp-cmp
           go-golang-org-x-exp
           go-golang-org-x-mod
           go-golang-org-x-sync
           go-golang-org-x-tools))
    (home-page "https://golang.org/x/vuln")
    (synopsis "Go Vulnerability Management")
    (description
     "This repository contains packages for accessing and analyzing data from
the @url{https://vuln.go.dev,Go Vulnerability Database}.")
    (license license:bsd-3)))

(define-public govulncheck
  (package
    (inherit go-golang-org-x-vuln)
    (name "govulncheck")
    (arguments
     (list
      #:tests? #f
      #:install-source? #f
      #:import-path "golang.org/x/vuln/cmd/govulncheck"
      #:unpack-path "golang.org/x/vuln"))
    (native-inputs
     (list coreutils-minimal))))

(define-public gopls
  (package
    (name "gopls")
    ;; XXX: Starting from 0.14.0 gppls needs golang.org/x/telemetry, which
    ;; needs to be discussed if it may be included in Guix.
    (version "0.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (string-append "gopls/v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qym2c0xvv6vcgwh0kz8sw094r88lzrl08xpvmg08lrqi00ma6kx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "golang.org/x/tools/gopls"
      #:unpack-path "golang.org/x/tools"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'unpack 'override-tools
            (lambda _
              (delete-file-recursively "src/golang.org/x/tools"))))))
    (native-inputs
     (list go-github-com-google-go-cmp-cmp
           go-github-com-jba-printsrc
           go-github-com-jba-templatecheck
           go-github-com-sergi-go-diff
           go-golang-org-x-mod
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-text
           go-golang-org-x-tools
           go-golang-org-x-vuln
           go-gopkg-in-yaml-v3
           go-honnef-co-go-tools
           go-mvdan-cc-gofumpt
           go-mvdan-cc-xurls))
    (home-page "https://golang.org/x/tools/gopls")
    (synopsis "Official language server for the Go language")
    (description
     "Pronounced ``Go please'', this is the official Go language server
developed by the Go team.  It provides IDE features to any LSP-compatible
editor.")
    (license license:bsd-3)))

(define-public go-golang-org-x-oauth2
  (let ((commit "0f29369cfe4552d0e4bcddc57cc75f4d7e672a33")
        (revision "1"))
    (package
      (name "go-golang-org-x-oauth2")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/oauth2")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-oauth2-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "06jwpvx0x2gjn2y959drbcir5kd7vg87k0r1216abk6rrdzzrzi2"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/oauth2"))
      (propagated-inputs
       (list go-cloud-google-com-go-compute-metadata
             go-golang-org-x-net))
      (home-page "https://go.googlesource.com/oauth2")
      (synopsis "Client implementation of the OAuth 2.0 spec")
      (description "This package contains a client implementation for OAuth 2.0
 spec in Go.")
      (license license:bsd-3))))

(define-public go-github-com-jpillora-backoff
  (let ((commit
         "06c7a16c845dc8e0bf575fafeeca0f5462f5eb4d")
        (revision "0"))
    (package
      (name "go-github-com-jpillora-backoff")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jpillora/backoff")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xhvxr7bm47czdc5hy3kl508z3y4j91i2jm7vg774i52zych6k4l"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/jpillora/backoff"))
      (home-page "https://github.com/jpillora/backoff")
      (synopsis "Simple exponential backoff counter in Go")
      (description "This package is a simple exponential backoff counter in
Go.")
      (license license:expat))))

(define-public go-github-com-stretchr-objx
  (package
    (name "go-github-com-stretchr-objx")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stretchr/objx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dygds32qxx6x1x2mmn7msyjr15qi5r70pyzv8dz8cprxq32nzc1"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
       #:import-path "github.com/stretchr/objx"
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs #:allow-other-keys #:rest args)
               (unless
                 ;; The tests fail when run with gccgo.
                 (false-if-exception (search-input-file inputs "/bin/gccgo"))
                 (apply (assoc-ref %standard-phases 'check) args)))))))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-pmezard-go-difflib))
    (inputs
     (list go-github-com-stretchr-testify-bootstrap))
    (home-page "https://github.com/stretchr/objx")
    (synopsis "Go package for dealing with maps, slices, JSON and other data")
    (description "This package provides a Go library for dealing with maps,
slices, JSON and other data.")
    (license license:expat)))

(define-public go-github-com-technoweenie-multipartstreamer
  (package
    (name "go-github-com-technoweenie-multipartstreamer")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/technoweenie/multipartstreamer")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "159jhcabdkds8m5777zfs8p5z3snpjhzz7q9aq9wjpcvh6xlljqa"))))
    (build-system go-build-system)
    (arguments
     (list #:tests? #f                  ; Upstream tests are broken.
           #:import-path "github.com/technoweenie/multipartstreamer"))
    (home-page "https://github.com/technoweenie/multipartstreamer")
    (synopsis "MIME multipart format streamer")
    (description
     "This package helps you encode large files in MIME multipart format
without reading the entire content into memory.")
    (license license:expat)))

(define-public go-github-com-tevino-abool
  (let ((commit
          "3c25f2fe7cd0ef3eabefce1d90efd69a65d35b12")
        (revision "0"))
    (package
      (name "go-github-com-tevino-abool")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/tevino/abool")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "1wxqrclxk93q0aj15z596dx2y57x9nkhi64nbrr5cxnhxn8vwixm"))))
      (build-system go-build-system)
      (arguments
        '(#:import-path "github.com/tevino/abool"))
      (home-page "https://github.com/tevino/abool")
      (synopsis "Atomic boolean library for Go code")
      (description "This package is atomic boolean library for Go code,
optimized for performance yet simple to use.")
      (license license:expat))))

(define-public go-github-com-tomnomnom-gron
  (package
    (name "gron")
    (version "0.7.1")
    (home-page "https://github.com/tomnomnom/gron")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sj34b6yv0qigy3aq7qmwf8bqxp1a8qh9p10lzkpw58s1c0iyh36"))))
    (build-system go-build-system)
    (arguments
     (let ((import-path "github.com/tomnomnom/gron"))
       `(#:import-path ,import-path
         #:phases
         (modify-phases %standard-phases
           (add-after 'check 'remove-non-source
             (lambda _
               (for-each (lambda (dir)
                           (delete-file-recursively
                            (string-append "src/" ,import-path dir)))
                         '("/docs" "/script" "/testdata"))
               #t))))))
    (inputs
     `(("github.com/fatih/color" ,go-github-com-fatih-color)
       ("github.com/mattn/go-colorable" ,go-github-com-mattn-go-colorable)
       ("github.com/mattn/go-isatty" ,go-github-com-mattn-go-isatty)
       ("github.com/nwidger/jsoncolor" ,go-github-com-nwidger-jsoncolor)
       ("github.com/pkg/errors" ,go-github-com-pkg-errors)))
    (synopsis "Transform JSON to make it easier to grep")
    (description
     "This package transforms JSON into discrete assignments to make it easier
to use line-based tools such as grep to search for what you want and see the
absolute \"path\" to it.")
    (license license:expat)))

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

(define-public go-github-com-jonboulle-clockwork
  (let ((commit "e3653ace2d63753697e0e5b07b9393971c0bba9d")
        (revision "0"))
    (package
      (name "go-github-com-jonboulle-clockwork")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/jonboulle/clockwork")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "1avzqhks12a8x2yzpvjsf3k0gv9cy7zx2z88hn0scacnxkphisvc"))))
      (build-system go-build-system)
      (arguments
        '(#:import-path "github.com/jonboulle/clockwork"))
      (home-page "https://github.com/jonboulle/clockwork")
      (synopsis "Fake clock library for Go")
      (description
       "Replace uses of the @code{time} package with the
@code{clockwork.Clock} interface instead.")
      (license license:asl2.0))))

(define-public go-github-com-spf13-afero
  (package
    (name "go-github-com-spf13-afero")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/afero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j9r65qgd58324m85lkl49vk9dgwd62g7dwvkfcm3k6i9dc555a9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/spf13/afero"))
    (propagated-inputs (list go-github-com-pkg-sftp go-golang-org-x-text))
    (home-page "https://github.com/spf13/afero")
    (synopsis "File system abstraction for Go")
    (description
     "This package provides a file system abstraction for Go.")
    (license license:asl2.0)))

(define-public go-github-com-spf13-cast
  (package
    (name "go-github-com-spf13-cast")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/cast")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lb84788glr0qzrq2ifi36rgvp96qrgywvxrr3ggq5hrbr38hgn1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/spf13/cast"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/spf13/cast")
    (synopsis "Safe and easy casting from one type to another in Go")
    (description "Safe and easy casting from one type to another in Go")
    (license license:expat)))

(define-public go-github-com-spf13-jwalterweatherman
  (package
    (name "go-github-com-spf13-jwalterweatherman")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/jwalterweatherman")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ywmkwci5zyd88ijym6f30fj5c0k2yayxarkmnazf5ybljv50q7b"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/spf13/jwalterweatherman"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/spf13/jwalterweatherman")
    (synopsis "Go logging library")
    (description "Go logging library")
    (license license:expat)))

(define-public go-github-com-spf13-pflag
  (package
    (name "go-github-com-spf13-pflag")
    (version "1.0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/spf13/pflag")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0gpmacngd0gpslnbkzi263f5ishigzgh6pbdv9hp092rnjl4nd31"))
        (snippet
         #~(begin
             (use-modules (guix build utils))
             ;; Fix compatibility with go-1.19+
             ;; https://github.com/spf13/pflag/issues/368
             (substitute* "flag_test.go"
               (("fmt\\.Println") "fmt.Print")
               (("\\+ got\\)") "+ got + \"\\n\")")
               (("\\+ defaultOutput\\)") "+ defaultOutput + \"\\n\")"))))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/spf13/pflag"))
    (home-page "https://github.com/spf13/pflag")
    (synopsis "Replacement for Go's @code{flag} package")
    (description
     "Pflag is library to replace Go's @code{flag} package.  It implements
POSIX/GNU-style command-line options with double hyphens.  It is is compatible
with the
@uref{https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html,
GNU extensions} to the POSIX recommendations for command-line options.")
    (license license:bsd-3)))

(define-public go-github-com-spf13-viper
  (package
    (name "go-github-com-spf13-viper")
    ;; Refreshing to a newer version requires long chain of missing packages.
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/viper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "099n2g7fg6r8hqyszqw2axr775qyhyvwhsykvgw0f0s16ql48h5c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/spf13/viper"))
    (propagated-inputs
     (list go-github-com-fsnotify-fsnotify
           go-github-com-hashicorp-hcl
           go-github-com-magiconair-properties
           go-github-com-mitchellh-mapstructure
           go-github-com-pelletier-go-toml
           go-github-com-spf13-afero
           go-github-com-spf13-cast
           go-github-com-spf13-jwalterweatherman
           go-github-com-spf13-pflag
           go-github-com-subosito-gotenv
           go-gopkg-in-ini-v1
           go-gopkg-in-yaml-v2))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/spf13/viper")
    (synopsis "Go configuration with fangs")
    (description
     "Viper is a complete configuration solution for Go applications including
12-Factor apps.  It is designed to work within an application, and can handle
all types of configuration needs and formats.")
    (license license:expat)))

(define-public go-github-com-fsnotify-fsnotify
  (package
    (name "go-github-com-fsnotify-fsnotify")
    (version "1.4.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fsnotify/fsnotify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1i1r72knpbfwwql9frn9bqc3nhfc2ai5m6qllcyr6wban62lr40x"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/fsnotify/fsnotify"))
    (propagated-inputs
     `(("golang.org/x/sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/fsnotify/fsnotify")
    (synopsis "File system notifications for Go")
    (description "File system notifications for Go")
    (license license:bsd-3)))

(define-public go-github-com-nxadm-tail
  (package
    (name "go-github-com-nxadm-tail")
    (version "1.4.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nxadm/tail")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j2gi485fhwdpmyzn42wk62103fclwbfywg42p275z1qv2bsz1rc"))))
    (build-system go-build-system)
    (arguments (list #:import-path "github.com/nxadm/tail"))
    (propagated-inputs (list go-gopkg-in-tomb-v1
                             go-github-com-fsnotify-fsnotify))
    (home-page "https://github.com/nxadm/tail")
    (synopsis "Go implementation of the functionality of @command{tail -f}")
    (description
     "This package provides a Go library for reading from continuously
updating files, like @command{tail -f}.")
    (license license:expat)))

(define-public go-github-com-magiconair-properties
  (package
    (name "go-github-com-magiconair-properties")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magiconair/properties")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xy5nq7mwhrdcwjlgh4arjn6w5mjla0kni3cvl3z5vxcrnfrn3ax"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/magiconair/properties"))
    (home-page "https://github.com/magiconair/properties")
    (synopsis "Java properties scanner for Go")
    (description "Java properties scanner for Go")
    (license license:bsd-2)))

(define-public go-github-com-pelletier-go-toml
  (package
    (name "go-github-com-pelletier-go-toml")
    (version "1.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pelletier/go-toml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cqwnvlgs1wgdgjxlwv8j52f7d6syniadr51sjh2fya99m5wzvsn"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/pelletier/go-toml"))
    (native-inputs
     `(("github.com/BurntSushi/toml" ,go-github-com-burntsushi-toml)
       ("github.com/davecgh/go-spew" ,go-github-com-davecgh-go-spew)
       ("gopkg.in/yaml.v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/pelletier/go-toml")
    (synopsis "Go library for the TOML configuration language")
    (description "Go library for the TOML configuration language")
    (license license:expat)))

(define-public go-github-com-pelletier-go-toml-v2
  (package
    (inherit go-github-com-pelletier-go-toml)
    (name "go-github-com-pelletier-go-toml-v2")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pelletier/go-toml")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k0rwg7870f4va7jaavnpwvdn6d76gxgyr7c978bx2h829a9sx2a"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/pelletier/go-toml/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))))

(define-public go-github-com-subosito-gotenv
  (package
    (name "go-github-com-subosito-gotenv")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/subosito/gotenv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mav91j7r4arjkpq5zcf9j74f6pww8ic53x43wy7kg3ibw31yjs5"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/subosito/gotenv"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/subosito/gotenv")
    (synopsis "Go library for loading environment variables from files")
    (description "Go library for loading environment variables from files")
    (license license:expat)))

(define-public go-github-com-sirupsen-logrus
  (package
    (name "go-github-com-sirupsen-logrus")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sirupsen/logrus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "12i402dxq5js4npnncg043vx874h6nk4ffn4gswcccxrp6h10ivz"))))
    (build-system go-build-system)
    (arguments
     (list
       #:import-path "github.com/sirupsen/logrus"
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs #:allow-other-keys #:rest args)
               (unless
                 ;; The tests fail when run with gccgo.
                 (false-if-exception (search-input-file inputs "/bin/gccgo"))
                 (apply (assoc-ref %standard-phases 'check) args)))))))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew go-github-com-pmezard-go-difflib
           go-github-com-stretchr-testify go-golang-org-x-crypto
           go-golang-org-x-sys))
    (home-page "https://github.com/sirupsen/logrus")
    (synopsis "Structured, pluggable logging for Go")
    (description "Logrus is a structured logger for Go, completely API
compatible with the standard library logger.")
    (license license:expat)))

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

(define-public go-github-com-ayufan-golang-kardianos-service
  (let ((commit "0c8eb6d8fff2e2fb884a7bfd23e183fb63c0eff3")
        (revision "0"))
    (package
      (name "go-github-com-ayufan-golang-kardianos-service")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/ayufan/golang-kardianos-service")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0x0cn7l5gda2khsfypix7adxd5yqighzn04mxjw6hc4ayrh7his5"))))
      (build-system go-build-system)
      (native-inputs
       (list go-github-com-kardianos-osext))
      (arguments
       '(#:tests? #f                ;FIXME tests fail: Service is not running.
         #:import-path "github.com/ayufan/golang-kardianos-service"))
      (home-page "https://github.com/ayufan/golang-kardianos-service")
      (synopsis "Go interface to a variety of service supervisors")
      (description "This package provides @code{service}, a Go module that can
run programs as a service using a variety of supervisors, including systemd,
SysVinit, and more.")
      (license license:zlib))))

(define-public go-github-com-dgryski-go-metro
  (package
    (name "go-github-com-dgryski-go-metro")
    (version "0.0.0-20211217172704-adc40b04c140")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dgryski/go-metro")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16y5vc5qf7aipi8basqza8l939hlmp7wqsv4y6gsqac3sp9ziqyj"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/dgryski/go-metro"))
    (home-page "https://github.com/dgryski/go-metro")
    (synopsis "Go translation of MetroHash")
    (description
     "This package provides a Go translation of the
@url{https://github.com/jandrewrogers/MetroHash, reference C++ code for
MetroHash}, a high quality, high performance hash algorithm.")
    (license license:expat)))

(define-public go-github-com-dgryski-go-mph
  (package
    (name "go-github-com-dgryski-go-mph")
    (version "0.0.0-20211217222804-81a8625fb7ed")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dgryski/go-mph")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10q8l4jdzqf54bnnxka2jk6qzayri3ijv51knn1n0iimfric8w9g"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/dgryski/go-mph"))
    (propagated-inputs
     (list go-github-com-dgryski-go-metro))
    (home-page "https://github.com/dgryski/go-mph")
    (synopsis "Go minimal perfect hash function")
    (description
     "This package implements a hash/displace minimal perfect hash function.")
    (license license:expat)))

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

(define-public go-github-com-docker-go-connections
  (package
    (name "go-github-com-docker-go-connections")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/go-connections")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0svfa9g4xvbn87l5kiww1jkijmci9g5821wjp81xz1rfp13cqrk8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/docker/go-connections"))
    (home-page "https://github.com/docker/go-connections")
    (synopsis "Networking library for Go")
    (description
     "This package provides a library to work with network connections in the
Go language.  In particular it provides tools to deal with network address
translation (NAT), proxies, sockets, and transport layer security (TLS).")
    (license license:asl2.0)))

(define-public go-github-com-docker-go-units
  (package
    (name "go-github-com-docker-go-units")
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/docker/go-units")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0k8gja8ql4pqg5rzmqvka42vjfs6rzablak87whcnqba6qxpimvz"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/docker/go-units"))
    (home-page "https://github.com/docker/go-units")
    (synopsis "Parse and print size and time units in human-readable format")
    (description
     "@code{go-units} is a library to transform human friendly measurements into
machine friendly values.")
    (license license:asl2.0)))

(define-public go-github-com-docker-machine
  (let ((commit "7b7a141da84480342357c51838be142bf183b095")
        (revision "0"))
    (package
      (name "go-github-com-docker-machine")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/docker/machine")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bavk0lvs462yh0lnmnxi9psi5qv1x3nvzmd2b0drsahlp1gxi8s"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/docker/machine"))
      (home-page "https://github.com/docker/machine")
      (synopsis "Machine management for a container-centric world")
      (description
       "@dfn{Machine} lets you create Docker hosts on your computer, on
hosting providers, and inside your data center.  It creates servers, installs
Docker on them, then configures the Docker client to talk to them.")
      (license license:asl2.0))))

(define-public go-github-com-gorhill-cronexpr
  (let ((commit "f0984319b44273e83de132089ae42b1810f4933b")
        (revision "0"))
    (package
      (name "go-github-com-gorhill-cronexpr")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gorhill/cronexpr")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0dphhhqy3i7265znv3m8n57l80dmaq6z4hsj5kgd87qd19z8x0l2"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/gorhill/cronexpr"))
      (home-page "https://github.com/gorhill/cronexpr")
      (synopsis "Cron expression parser in the Go language")
      (description
       "This package provides a cron expression parser in the Go language.
Given a cron expression and a time stamp, you can get the next time stamp
which satisfies the cron expression.")
      (license (list license:gpl3+
                     license:asl2.0)))))

(define-public go-gopkg-in-ini-v1
  (package
    (name "go-gopkg-in-ini-v1")
    (version "1.56.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ini/ini")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j5z0cngg6mq2f9id083jcdi7k6r2h35714pashv6sdv2q7bmfc5"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/ini.v1"
       ;; Requires large unpackaged test framework
       #:tests? #f))
    (home-page "https://gopkg.in/ini.v1")
    (synopsis "Go library for ini files")
    (description "Go library for ini files")
    (license license:asl2.0)))

(define-public go-gopkg-in-yaml-v3
  (package
    (name "go-gopkg-in-yaml-v3")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/yaml.v3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01b0wjb7yzv8wzzz2iim8mjpkwjnykcanrwiq06pkl89lr6gv8hn"))
       (patches (search-patches "go-gopkg-in-yaml-v3-32bit.patch"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? ,(not (target-ppc32?))  ; Test killed with quit: ran too long (11m0s).
       #:import-path "gopkg.in/yaml.v3"))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://gopkg.in/yaml.v3")
    (synopsis "YAML reader and writer for the Go language")
    (description
     "This package provides a Go library for encode and decode YAML values.
The yaml package supports most of YAML 1.2, but preserves some behavior from
1.1 for backwards compatibility.")
    (license license:asl2.0)))

(define-public go-github-com-matrix-org-gomatrix
  (package
    (name "go-github-com-matrix-org-gomatrix")
    (version "0.0.0-20220926102614-ceba4d9f7530")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/matrix-org/gomatrix")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vq29bdswvffxsmwvi20wnk73xk92dva0fdr2k3zshr4z10ypm2x"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/matrix-org/gomatrix"))
    (home-page "https://github.com/matrix-org/gomatrix")
    (synopsis "Golang Matrix client")
    (description "This package provides a Golang Matrix client.")
    (license license:asl2.0)))

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

(define-public misspell
  (package
    (name "misspell")
    (version "0.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/client9/misspell")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vwf33wsc4la25zk9nylpbp9px3svlmldkm0bha4hp56jws4q9cs"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/client9/misspell"
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda arguments
                      (apply (assoc-ref %standard-phases
                                        'build)
                             `(,@arguments #:import-path
                               "github.com/client9/misspell/cmd/misspell")))))))
    (propagated-inputs (list go-github-com-gobwas-glob))
    (home-page "https://github.com/client9/misspell")
    (synopsis "Correct commonly misspelled English words in source files")
    (description
     "misspell assists with correcting commonly misspelled English words in
source files.  A neutral variety of English is used by default, but a US or UK
locale can be selected.")
    (license license:expat)))

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

(define-public go-github-com-client9-misspell
  (package
    (inherit misspell)
    (name "go-github-com-client9-misspell")
    (arguments
     `(#:import-path "github.com/client9/misspell"
       #:tests? #f
       #:install-source? #t
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (propagated-inputs (package-inputs misspell))
    (native-inputs '())
    (inputs '())))

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

(define-public go-github-com-gogo-protobuf
  (package
    (name "go-github-com-gogo-protobuf")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gogo/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x77x64sxjgfhmbijqfzmj8h4ar25l2w97h01q3cqs1wk7zfnkhp"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gogo/protobuf"
       ; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (synopsis "Protocol Buffers for Go with Gadgets")
    (description "Gogoprotobuf is a fork of golang/protobuf with extra code
generation features.  This code generation is used to achieve:
@itemize
@item fast marshalling and unmarshalling
@item more canonical Go structures
@item goprotobuf compatibility
@item less typing by optionally generating extra helper code
@item peace of mind by optionally generating test and benchmark code
@item other serialization formats
@end itemize")
    (home-page "https://github.com/gogo/protobuf")
    (license license:bsd-3)))

(define-public go-github-com-libp2p-go-flow-metrics
  (let ((commit "7e5a55af485341567f98d6847a373eb5ddcdcd43")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-flow-metrics")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-flow-metrics")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1p87iyk6q6f3g3xkncssx400qlld8f2z93qiz8m1f97grfyhjif1"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/libp2p/go-flow-metrics"
         ;; TODO: Tests hang.
         #:tests? #f))
      (home-page
       "https://github.com/libp2p/go-flow-metrics")
      (synopsis "Simple library for tracking flow metrics")
      (description "A simple alternative to rcrowley's @command{go-metrics}
that's a lot faster (and only does simple bandwidth metrics).")
      (license license:expat))))

(define-public go-github-com-btcsuite-btclog
  (let ((commit "84c8d2346e9fc8c7b947e243b9c24e6df9fd206a")
        (revision "0"))
    (package
      (name "go-github-com-btcsuite-btclog")
      (version (git-version "0.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/btcsuite/btclog")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "02dl46wcnfpg9sqvg0ipipkpnd7lrf4fnvb9zy56jqa7mfcwc7wk"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/btcsuite/btclog"))
      (home-page "https://github.com/btcsuite/btclog")
      (synopsis "Subsystem aware logger for Go")
      (description "Package @command{btclog} defines a logger interface and
provides a default implementation of a subsystem-aware leveled logger
implementing the same interface.")
      (license license:isc))))

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

(define-public go-github-com-twmb-murmur3
  (package
    (name "go-github-com-twmb-murmur3")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twmb/murmur3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00riapwkyf23l5wyis47mbr8rwr4yrjw491jfc30wpzs111c1gyy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/twmb/murmur3"))
    (home-page "https://github.com/twmb/murmur3")
    (synopsis "Native MurmurHash3 Go implementation")
    (description "Native Go implementation of Austin Appleby's third
MurmurHash revision (aka MurmurHash3).

Reference algorithm has been slightly hacked as to support the streaming mode
required by Go's standard Hash interface.")
    (license license:bsd-3)))

(define-public go-github-com-libp2p-go-libp2p-protocol
  (let ((commit "b29f3d97e3a2fb8b29c5d04290e6cb5c5018004b")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-libp2p-protocol")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-libp2p-protocol")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1xgjfnx9zcqglg9li29wdqywsp8hz22wx6phns9zscni2jsfidld"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/libp2p/go-libp2p-protocol"))
      (home-page "https://github.com/libp2p/go-libp2p-protocol")
      (synopsis "Type for protocol strings in Golang")
      (description "Just a type for protocol strings.  Nothing more.")
      (license license:expat))))

(define-public go-github-com-libp2p-go-libp2p-metrics
  (let ((commit "a10ff6e75dae3c868023867e8caa534a04bdc624")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-libp2p-metrics")
      (version (git-version "2.1.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-libp2p-metrics")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "05wy0cq4h6yg9bzgapcvm2criwriicbswx80ma82gyn4a9fdrk8m"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/libp2p/go-libp2p-metrics"))
      (propagated-inputs
       (list go-github-com-libp2p-go-flow-metrics
             go-github-com-libp2p-go-libp2p-peer
             go-github-com-libp2p-go-libp2p-protocol
             go-github-com-libp2p-go-libp2p-crypto
             go-github-com-multiformats-go-multihash
             go-github-com-btcsuite-btcd-btcec
             go-github-com-gogo-protobuf))
      (home-page "https://github.com/libp2p/go-libp2p-metrics")
      (synopsis "Connection wrapper for go-libp2p that provides bandwidth metrics")
      (description "A connection wrapper for @command{go-libp2p} that provides bandwidth
statistics for wrapped connections.")
      (license license:expat))))

(define-public go-github-com-mitchellh-go-homedir
  (let ((commit "ae18d6b8b3205b561c79e8e5f69bff09736185f4")
        (revision "0"))
    (package
      (name "go-github-com-mitchellh-go-homedir")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mitchellh/go-homedir")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0f0z0aa4wivk4z1y503dmnw0k0g0g403dly8i4q263gfshs82sbq"))))
      (build-system go-build-system)
      (arguments
       (quote (#:import-path "github.com/mitchellh/go-homedir"
               ;; TODO: Tests fail because it tries to access home.
               #:tests? #f)))
      (home-page "https://github.com/mitchellh/go-homedir")
      (synopsis "Go library for detecting and expanding the user's home directory without cgo")
      (description "This is a Go library for detecting the user's home
directory without the use of @command{cgo}, so the library can be used in
cross-compilation environments.

Usage is simple, just call homedir.Dir() to get the home directory for a user,
and homedir.Expand() to expand the @command{~} in a path to the home
directory.

Why not just use @command{os/user}?  The built-in @command{os/user} package
requires cgo on Darwin systems.  This means that any Go code that uses that
package cannot cross compile.  But 99% of the time the use for
@command{os/user} is just to retrieve the home directory, which we can do for
the current user without cgo.  This library does that, enabling
cross-compilation.")
      (license license:expat))))

(define-public go-github-com-mitchellh-mapstructure
  (package
    (name "go-github-com-mitchellh-mapstructure")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/mapstructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10f2v143lkip8h46shd99k5yavfqpgqmd7a6y42v7szc0lcn3mff"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mitchellh/mapstructure"))
    (home-page "https://github.com/mitchellh/mapstructure")
    (synopsis "Go library for decoding generic map values")
    (description "Go library for decoding generic map values")
    (license license:expat)))

(define-public go-github-com-mitchellh-reflectwalk
  (package
    (name "go-github-com-mitchellh-reflectwalk")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mitchellh/reflectwalk")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pa6a3nhzwv5s5yqcmsmsfhdp5ggxsg2wa86f3akawxrhrkjarnx"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mitchellh/reflectwalk"))
    (home-page "https://github.com/mitchellh/reflectwalk/")
    (synopsis "Walk a value in Go using reflection")
    (description "reflectwalk is a Go library for \"walking\" a value in Go
using reflection, in the same way a directory tree can be \"walked\" on the
file system.  Walking a complex structure can allow you to do manipulations on
unknown structures such as those decoded from JSON.")
    (license license:expat)))

(define-public go-github-com-mitchellh-copystructure
  (package
    (name "go-github-com-mitchellh-copystructure")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/copystructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "05njg92w1088v4yl0js0zdrpfq6k37i9j14mxkr3p90p5yd9rrrr"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mitchellh/copystructure"))
    (native-inputs
     (list go-github-com-mitchellh-reflectwalk))
    (home-page "https://github.com/mitchellh/copystructure")
    (synopsis "Go library for decoding deep copying values")
    (description "@code{copystructure} is a Go library for deep copying values
in Go.

This allows you to copy Go values that may contain reference values such as
maps, slices, or pointers, and copy their data as well instead of just their
references.")
    (license license:expat)))

(define-public go-github-com-whyrusleeping-tar-utils
  (let ((commit "8c6c8ba81d5c71fd69c0f48dbde4b2fb422b6dfc")
        (revision "0"))
    (package
      (name "go-github-com-whyrusleeping-tar-utils")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/whyrusleeping/tar-utils")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14jjdw3yics0k467xsyk388684wdpi0bbx8nqj0y4pqxa0s0in6s"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/whyrusleeping/tar-utils"))
      (home-page "https://github.com/whyrusleeping/tar-utils")
      (synopsis "Tar utilities extracted from go-ipfs codebase")
      (description "Tar utilities extracted from @command{go-ipfs} codebase.")
      (license license:expat))))

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

(define-public go-github-com-urfave-cli
  (package
    (name "go-github-com-urfave-cli")
    (version "1.22.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10mcnvi5qmn00vpyk6si8gjka7p654wr9hac4zc9w5h3ickhvbdc"))
       (patches (search-patches "go-github-com-urfave-cli-fix-tests.patch"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/urfave/cli"))
    (propagated-inputs
     (list go-github-com-go-md2man))
    (home-page "https://github.com/urfave/cli")
    (synopsis "Simple, fast, and fun package for building command line apps in Go")
    (description "@command{cli} is a simple, fast, and fun package for
building command line apps in Go.  The goal is to enable developers to write
fast and distributable command line applications in an expressive way.")
    (license license:expat)))

(define-public go-github-com-urfave-cli-v2
  (package
    (inherit go-github-com-urfave-cli)
    (name "go-github-com-urfave-cli-v2")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08pvn7gyfznni72xrxfh2x6xxa8ykr7l1ka278js8g8qkh71bj8l"))
       ;; XXX: Remove patch when updating.
       (patches
        (search-patches "go-github-com-urfave-cli-v2-fix-tests.patch"))))
    (arguments
     '(#:import-path "github.com/urfave/cli/v2"))))

(define-public go-github-com-go-md2man
  (package
    (name "go-github-com-go-md2man")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cpuguy83/go-md2man")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r1f7v475dxxgzqci1mxfliwadcrk86ippflx9n411325l4g3ghv"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "vendor")
                   #t))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/cpuguy83/go-md2man"))
    (propagated-inputs
     (list go-github-com-russross-blackfriday))
    (home-page "https://github.com/cpuguy83/go-md2man")
    (synopsis "Convert markdown into roff")
    (description "Go-md2man is a Go program that converts markdown to roff for
the purpose of building man pages.")
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

(define-public go-github-com-russross-blackfriday
  (package
    (name "go-github-com-russross-blackfriday")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/russross/blackfriday")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nlz7isdd4rgnwzs68499hlwicxz34j2k2a0b8jy0y7ycd2bcr5j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/russross/blackfriday"))
    (propagated-inputs
     (list go-github-com-shurcool-sanitized-anchor-name))
    (native-inputs
     (list go-github-com-pmezard-go-difflib))
    (home-page "https://github.com/russross/blackfriday")
    (synopsis "Markdown processor in Go")
    (description "Blackfriday is a Markdown processor in Go.")
    (license license:bsd-2)))

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

(define-public go-github-com-pmezard-go-difflib
  (package
    (name "go-github-com-pmezard-go-difflib")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pmezard/go-difflib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c1cn55m4rypmscgf0rrb88pn58j3ysvc2d0432dp3c6fqg6cnzw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/pmezard/go-difflib/difflib"
       #:unpack-path "github.com/pmezard/go-difflib/"))
    (home-page "https://github.com/pmezard/go-difflib")
    (synopsis "Go diff implementation")
    (description "This package provides unified and context-aware diffs in Go.")
    (license license:bsd-3)))

(define-public go-github-com-whyrusleeping-progmeter
  (let ((commit "f3e57218a75b913eff88d49a52c1debf9684ea04")
        (revision "0"))
    (package
      (name "go-github-com-whyrusleeping-progmeter")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/whyrusleeping/progmeter")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xs8rz6yhpvj9512c5v3b8dwr2kivywnyyfxzdfbr6fy1xc8zskb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/whyrusleeping/progmeter"))
      (home-page "https://github.com/whyrusleeping/progmeter")
      (synopsis "Progress meter for Go")
      (description "Progress meter for Go.")
      (license license:expat))))

(define-public go-github-com-whyrusleeping-stump
  (let ((commit "206f8f13aae1697a6fc1f4a55799faf955971fc5")
        (revision "0"))
    (package
      (name "go-github-com-whyrusleeping-stump")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/whyrusleeping/stump")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1s40qdppjnk8gijk7x6kbviiqz62nz3h6gic2q9cwcmq8r5isw7n"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/whyrusleeping/stump"))
      (home-page "https://github.com/whyrusleeping/stump")
      (synopsis "Very basic logging package for Go")
      (description "A simple log library, for when you don't really care to
have super fancy logs.")
      (license license:expat))))

(define-public go-github-com-kr-fs
  (let ((commit "1455def202f6e05b95cc7bfc7e8ae67ae5141eba")
        (revision "0"))
    (package
      (name "go-github-com-kr-fs")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kr/fs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11zg176x9hr9q7fsk95r6q0wf214gg4czy02slax4x56n79g6a7q"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/kr/fs"))
      (home-page "https://github.com/kr/fs")
      (synopsis "File-system-related functions for Go")
      (description
       "The fs package provides file-system-related Go functions.")
      (license license:bsd-3))))

(define-public go-github-com-direnv-go-dotenv
  (let ((commit "4cce6d1a66f7bc8dc730eab85cab6af1b801abed")
        (revision "0"))
    (package
      (name "go-github-com-direnv-go-dotenv")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/direnv/go-dotenv")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00wn4fc2lma0csf6ryvlc6k9jbpbifm4n7i3kkd2xrfw5qlm29b6"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/direnv/go-dotenv"))
      (home-page "https://github.com/direnv/go-dotenv")
      (synopsis "Go dotenv parsing library")
      (description "This package provides a library for parsing the dotenv
format in Go.")
      (license license:expat))))

(define-public go-github-com-kr-pretty
  (package
    (name "go-github-com-kr-pretty")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kr/pretty")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vzfz06y9q8gs2nxx0kys0591vzp78k0fvpb8digi5n15h3b25hy"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-github-com-kr-text))
    (arguments
     '(#:import-path "github.com/kr/pretty"))
    (synopsis "Pretty printer for Go values")
    (description "This package provides a pretty printer for Go values.")
    (home-page "https://github.com/kr/pretty")
    (license license:expat)))

(define-public go-github-com-kylelemons-godebug
  (package
    (name "go-github-com-kylelemons-godebug")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kylelemons/godebug")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dkk3friykg8p6wgqryx6745ahhb9z1j740k7px9dac6v5xjp78c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kylelemons/godebug/diff"
       #:unpack-path "github.com/kylelemons/godebug"))
    (home-page "https://github.com/kylelemons/godebug")
    (synopsis "Pretty printer for Go values")
    (description
     "This package will pretty print a compact representation of a Go data
structure.  It can also produce a much more verbose, one-item-per-line
representation suitable for computing diffs.")
    (license license:asl2.0)))

(define-public go-github-com-kr-text
  (package
    (name "go-github-com-kr-text")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kr/text")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hf58ypz6rxsw6nx3i856whir9lvy4sdx946wbw1nfaf2rdmr9vx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kr/text"))
    (propagated-inputs (list go-github-com-creack-pty))
    (synopsis "Text formatting in Go")
    (description "This package provides a text formatting functions in Go.")
    (home-page "https://github.com/kr/text")
    (license license:expat)))

(define-public go-github-com-go-sql-driver-mysql
  (package
    (name "go-github-com-go-sql-driver-mysql")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-sql-driver/mysql")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ihdqg411gkv454fwx8w5nbndgkm5dz5phfliksxgmhggyxxm7sn"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ;; tests require a network connection
       #:import-path "github.com/go-sql-driver/mysql"))
    (propagated-inputs
     (list go-filippo-io-edwards25519))
    (home-page "https://github.com/go-sql-driver/mysql")
    (synopsis "MySQL driver for golang")
    (description
     "This is a pure Go implementation of the MySQL API, compatible with
golang's database/sql package.")
    (license license:mpl2.0)))

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

(define-public go-github-com-gdamore-encoding
  (package
    (name "go-github-com-gdamore-encoding")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gdamore/encoding")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1vmm5zll92i2fm4ajqx0gyx0p9j36496x5nabi3y0x7h0inv0pk9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gdamore/encoding"))
    (inputs
     (list go-golang-org-x-text))
    (home-page "https://github.com/gdamore/encoding")
    (synopsis "Provide encodings missing from Go")
    (description "This package provides useful encodings not included in the
standard @code{Text} package, including some for dealing with I/O streams from
non-UTF-friendly sources.")
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

(define-public go-github-com-gdamore-tcell
  (let ((commit "aaadc574a6ed8dc3abe56036ca130dcee1ee6b6e")
        (version "1.1.2")
        (revision "1"))
    (package
      (name "go-github-com-gdamore-tcell")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gdamore/tcell")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0il2nnxp2cqiy73m49215dnf9in3vd25ji8qxbmq87c5qy7i1q9d"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gdamore/tcell"))
      (inputs
       (list go-github-com-mattn-go-runewidth
             go-github-com-lucasb-eyer-go-colorful
             go-golang-org-x-text
             go-github-com-gdamore-encoding))
      (home-page "https://github.com/gdamore/tcell")
      (synopsis "Provide a cell-based view for text terminals")
      (description "This package includes a full parser and expander for
terminfo capability strings to avoid hard-coding escape strings for
formatting.  It also favors portability, and includes support for all POSIX
systems.")
      (license license:asl2.0))))

(define-public go-github-com-gdamore-tcell-v2
    (package
      (inherit go-github-com-gdamore-tcell)
      (name "go-github-com-gdamore-tcell")
      (version "2.6.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gdamore/tcell")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0126hi8glnfqdx4l9zlh6dhd5f5c8bws7arv9pp4n2kqcnhdc6g2"))))
      (arguments
       (list #:import-path "github.com/gdamore/tcell/v2"
             #:phases
             #~(modify-phases %standard-phases
                 (add-before 'reset-gzip-timestamps 'make-files-writable
                   (lambda _
                     ;; Make sure .gz files are writable so that the
                     ;; 'reset-gzip-timestamps' phase can do its work.
                     (for-each make-file-writable
                               (find-files #$output "\\.gz$")))))))
      (propagated-inputs
       (modify-inputs (package-inputs go-github-com-gdamore-tcell)
         (prepend go-golang-org-x-term go-golang-org-x-sys)))))

(define-public go-git-sr-ht-rockorager-tcell-term
  (package
    (name "go-git-sr-ht-rockorager-tcell-term")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~rockorager/tcell-term")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "177ladvpiiw7sb0hsjjv9p2yv5wpqpw6nqardkm8mqqlj0swa9xx"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "git.sr.ht/~rockorager/tcell-term"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-golang-org-x-term
           go-gopkg-in-check-v1
           go-github-com-mattn-go-runewidth
           go-github-com-davecgh-go-spew
           go-github-com-stretchr-testify
           go-github-com-gdamore-tcell-v2
           go-github-com-creack-pty))
    (home-page "https://git.sr.ht/~rockorager/tcell-term")
    (synopsis "Terminal widget for @code{tcell}")
    (description
     "This package provides a virtual terminal widget for the @code{tcell}
Go library.")
    (license license:expat)))

(define-public go-github-com-rivo-tview
  (package
    (name "go-github-com-rivo-tview")
    (version "0.0.0-20220703182358-a13d901d3386")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rivo/tview")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gf1m3ndbc3kgxpv0ryq9a1ahijg6m896sc9k7dvwfjd8vy0q0yd"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/rivo/tview"))
    (propagated-inputs (list go-golang-org-x-term
                             go-golang-org-x-sys
                             go-github-com-rivo-uniseg
                             go-github-com-mattn-go-runewidth
                             go-github-com-lucasb-eyer-go-colorful
                             go-github-com-gdamore-tcell-v2))
    (home-page "https://github.com/rivo/tview")
    (synopsis "Rich Interactive Widgets for Terminal UIs")
    (description
     "The tview package implements rich widgets for terminal based user
interfaces.  The widgets provided with this package are useful for data
exploration and data entry.")
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

(define-public go-github-com-burntsushi-locker
  (let ((commit "a6e239ea1c69bff1cfdb20c4b73dadf52f784b6a")
        (revision "0"))
    (package
      (name "go-github-com-burntsushi-locker")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BurntSushi/locker")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1xak4aync4klswq5217qvw191asgla51jr42y94vp109lirm5dzg"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/BurntSushi/locker"))
      (home-page "https://github.com/BurntSushi/locker")
      (synopsis "Manage named ReadWrite mutexes in Go")
      (description "Golang package for conveniently using named read/write
locks.  These appear to be especially useful for synchronizing access to
session based information in web applications.

The common use case is to use the package level functions, which use a package
level set of locks (safe to use from multiple goroutines
simultaneously).  However, you may also create a new separate set of locks
test.

All locks are implemented with read-write mutexes.  To use them like a regular
mutex, simply ignore the RLock/RUnlock functions.")
      (license license:unlicense))))

(define-public go-github-com-cheekybits-genny
  (package
    (name "go-github-com-cheekybits-genny")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cheekybits/genny")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pcir5ic86713aqa51581rfb67rgc3m0c72ddjfcp3yakv9vyq87"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/cheekybits/genny"))
    (propagated-inputs
     (list go-golang-org-x-tools))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (synopsis "Generics for Go")
    (description "This package provides @code{genny}, a Go language
implementation of generics.")
    (home-page "https://github.com/cheekybits/genny/")
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

;; XXX: This repository has been archived by the owner on Dec 1, 2021. It is
;; now read-only.
(define-public go-github-com-pkg-errors
  (package
    (name "go-github-com-pkg-errors")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pkg/errors")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1761pybhc2kqr6v5fm8faj08x9bql8427yqg6vnfv6nhrasx1mwq"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests fail with a newer version of Golang (1.21) due to some API
      ;; changes in how the module path is calculated which is not reflected
      ;; in tests.
      #:tests? #f
      #:import-path "github.com/pkg/errors"))
    (synopsis "Go error handling primitives")
    (description "This package provides @code{error}, which offers simple
error handling primitives in Go.")
    (home-page "https://github.com/pkg/errors")
    (license license:bsd-2)))

(define-public go-github-com-maruel-panicparse
  (package
    (name "go-github-com-maruel-panicparse")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/maruel/panicparse")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13qkn7f64yln8jdmma37h6ra4c7anxkp3vfgvfyb6lb07dpr1ibq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/maruel/panicparse"))
    (synopsis "Toolkit for parsing Go stack traces")
    (description "This package provides a toolkit for parsing Go language panic
stack traces.  It simplifies the traces to make salient information more visible
and aid debugging.")
    (home-page "https://github.com/maruel/panicparse")
    (license license:asl2.0)))

(define-public go-github-com-robfig-cron
  (package
    (name "go-github-com-robfig-cron")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/robfig/cron")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1agzbw2dfk2d1mpmddr85s5vh6ygm8kqrvfg87i9d2wqnlsnliqm"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/robfig/cron"))
    (home-page "https://godoc.org/github.com/robfig/cron")
    (synopsis "Cron library for Go")
    (description "This package provides a cron library for Go.  It implements
a cron spec parser and job runner.")
    (license license:expat)))

(define-public go-github-com-ddevault-go-libvterm
  (let ((commit "b7d861da381071e5d3701e428528d1bfe276e78f")
        (revision "0"))
    (package
      (name "go-github-com-ddevault-go-libvterm")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/ddevault/go-libvterm")
                (commit commit)))
          (sha256
           (base32
            "06vv4pgx0i6hjdjcar4ch18hp9g6q6687mbgkvs8ymmbacyhp7s6"))
          (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/ddevault/go-libvterm"))
      (propagated-inputs
       (list go-github-com-mattn-go-pointer))
      (home-page "https://github.com/ddevault/go-libvterm")
      (synopsis "Go binding to libvterm")
      (description
       "This is a fork of another go-libvterm library for use with aerc.")
      (license license:expat))))

(define-public go-github-com-emersion-go-imap
  (package
    (name "go-github-com-emersion-go-imap")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/go-imap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ak2ysvfcc9w0g1070msis8x9sh6gzvf0nd65ks594siwbmqddw8"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-imap"))
    (propagated-inputs (list go-golang-org-x-text
                             go-github-com-emersion-go-sasl
                             go-github-com-emersion-go-message))
    (home-page "https://github.com/emersion/go-imap")
    (synopsis "IMAP4rev1 library written in Go")
    (description
     "This package provides an IMAP4rev1 library written in Go.  It
can be used to build IMAP clients and servers.")
    (license license:expat)))

(define-public go-github-com-emersion-go-imap-sortthread
  (package
    (name "go-github-com-emersion-go-imap-sortthread")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/go-imap-sortthread")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cfbgz1l5angnj52v9pxwggai2shx0h78ffcp7j4r4lr7lzflnwz"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-imap-sortthread"))
    (propagated-inputs (list go-golang-org-x-text
                             go-github-com-emersion-go-sasl
                             go-github-com-emersion-go-imap))
    (home-page "https://github.com/emersion/go-imap-sortthread")
    (synopsis "Sorting and threading of messages for the imap package")
    (description
     "The sortthread package implements message sorting and threading for
@code{go-github-com-emersion-go-imap}.")
    (license license:expat)))

(define-public go-github-com-emersion-go-smtp
  (package
    (name "go-github-com-emersion-go-smtp")
    (version "0.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/go-smtp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vhc0vpjd4yhxk6wrh01sdpi7nprjn98s46yy82xwlkm0cskl0h7"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-smtp"))
    (propagated-inputs (list go-github-com-emersion-go-sasl))
    (home-page "https://github.com/emersion/go-smtp")
    (synopsis "SMTP implementation for Go")
    (description
     "This package implements the Simple Mail Transfer Protocol as
defined by RFC 5321.")
    (license license:expat)))

(define-public go-github-com-emersion-go-sasl
  (let ((commit "0b9dcfb154ac3d7515b08bc2691a0332800edfe9")
        (revision "1"))
    (package
      (name "go-github-com-emersion-go-sasl")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/emersion/go-sasl")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1cbf86wkqrdinfydndgdlnayg4a5mg3d4vqra377j2sfkg7wj0hs"))))
      (build-system go-build-system)
      (arguments
       (list #:import-path "github.com/emersion/go-sasl"))
      (home-page "https://github.com/emersion/go-sasl")
      (synopsis "SASL library written in Go")
      (description "This package provides a SASL library written in Go.")
      (license license:expat))))

(define-public go-github-com-emersion-go-imap-idle
  (let ((commit "2704abd7050ed7f2143753554ee23affdf847bd9")
        (revision "0"))
    (package
      (name "go-github-com-emersion-go-imap-idle")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/emersion/go-imap-idle")
                (commit commit)))
          (sha256
           (base32
            "0blwcadmxgqsdwgr9m4jqfbpfa2viw5ah19xbybpa1z1z4aj5cbc"))
          (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/emersion/go-imap-idle"))
      (native-inputs
       (list go-github-com-emersion-go-imap go-github-com-emersion-go-sasl
             go-golang-org-x-text))
      (home-page "https://github.com/emersion/go-imap-idle")
      (synopsis "IDLE extension for go-imap")
      (description "This package provides an IDLE extension for go-imap.")
      (license license:expat))))

(define-public go-github-com-emersion-go-maildir
  (package
    (name "go-github-com-emersion-go-maildir")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/go-maildir")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rs9kbacjpcza25pmdkbm7sdm7r6gq4g44nihi9asyrvspx96zf2"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-maildir"))
    (home-page "https://github.com/emersion/go-maildir")
    (synopsis "Maildir interface for Go")
    (description
     "This package provides an interface to mailboxes in the Maildir
format.")
    (license license:expat)))

(define-public go-github-com-emersion-go-milter
  (package
    (name "go-github-com-emersion-go-milter")
    (version "0.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/go-milter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10vpry5gjz2bh9qchcx4p59zm7cc6cb6bfkii2n6vsn4svb950sa"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-milter"))
    (propagated-inputs (list go-github-com-emersion-go-message))
    (home-page "https://github.com/emersion/go-milter")
    (synopsis "Milter mail filters in Go")
    (description
     "This package provides an interface for implementing milter mail
filters for Go.")
    (license license:bsd-2)))

(define-public go-github-com-emersion-go-msgauth
  (package
    (name "go-github-com-emersion-go-msgauth")
    (version "0.6.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/go-msgauth")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ds8yr4cm9wigcxg1sxc2m0wmy4z9n6gws3mj50dmf2ayij69z9j"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-msgauth"
           #:tests? #f ; Source-only package.
           #:phases
           #~(modify-phases %standard-phases
               ;; Source-only package.
               (delete 'build))))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-emersion-go-milter
                             go-github-com-emersion-go-message))
    (home-page "https://github.com/emersion/go-msgauth")
    (synopsis "Email authentication for Go")
    (description
     "This package provides a Go library for authenticating emails.")
    (license license:expat)))

(define-public go-github-com-emersion-go-mbox
  (package
    (name "go-github-com-emersion-go-mbox")
    (version "1.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/go-mbox")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vnadh2khx7sxn0irrd8gz8ra02x7ij0q8zglq3rqffqil06nliv"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-mbox"))
    (home-page "https://github.com/emersion/go-mbox")
    (synopsis "Go library for handling @code{mbox} files")
    (description
     "This package provides a library for parsing and formatting
@code{mbox} files.")
    (license license:expat)))

(define-public go-github-com-google-go-cmp-cmp
  (package
    (name "go-github-com-google-go-cmp-cmp")
    (version "0.5.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/go-cmp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a13m7l1jrysa7mrlmra8y7n83zcnb23yjyg3a609p8i9lxkh1wm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/go-cmp/cmp"
       #:unpack-path "github.com/google/go-cmp"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys #:rest args)
             (unless
               ;; The tests fail when run with gccgo.
               (false-if-exception (search-input-file inputs "/bin/gccgo"))
               (apply (assoc-ref %standard-phases 'check) args)))))))
    (synopsis "Determine equality of values in Go")
    (description
     "This package is intended to be a more powerful and safer
alternative to @@code{reflect.DeepEqual} for comparing whether two values are
semantically equal.")
    (home-page "https://github.com/google/go-cmp")
    (license license:bsd-3)))

(define-public go-github-com-google-uuid
  (package
    (name "go-github-com-google-uuid")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/uuid")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hfxcf9frkb57k6q0rdkrmnfs78ms21r1qfk9fhlqga2yh5xg8zb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/uuid"))
    (home-page "https://github.com/google/uuid/")
    (synopsis "Generate and inspect UUIDs based on RFC 4122 and DCE 1.1")
    (description "The uuid package generates and inspects UUIDs based on RFC
4122 and DCE 1.1: Authentication and Security Services.")
    (license license:bsd-3)))

(define-public go-github-com-google-gopacket
  (package
    (name "go-github-com-google-gopacket")
    (version "1.1.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/gopacket")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "048qwm2n0wrpql4qqgd7jyynn3gk069yvqbxnshlayzmbhf87ls4"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/gopacket"))
    (home-page "https://github.com/google/gopacket")
    (synopsis "Packet processing capabilities library")
    (description
     "This package provides packet processing capabilities for Go.")
    (license license:bsd-3)))

(define-public go-github-com-google-goterm
  (let ((commit "fc88cf888a3fa99ecc23d1efc1a44284268457d3")
        (revision "1"))
    (package
      (name "go-github-com-google-goterm")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/goterm")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0809sf02dhg2bjhsz43pmlb5d7nbsnwxls3lw01zw5p7ri9bqwfb"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/google/goterm/term"
         #:unpack-path "github.com/google/goterm"))
      (home-page "https://github.com/google/goterm/")
      (synopsis "PTY creation and termios get/set attributes")
      (description "The term package implements PTY creation and termios get/set
attributes.  It also contains some convenience functions for colors, SSH to
and from termios translations, readCh, reading passwords, etc.")
      (license license:bsd-3))))

(define-public go-github-com-google-go-querystring
  (let ((commit "992e8021cf787c100d55520d5c906e01536c0a19") ;fix format in tests
        (revision "1"))
    (package
      (name "go-github-com-google-go-querystring")
      (version "1.0.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/go-querystring")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mbx4jvf7nz4sk2fgqfq1llz4xb3vc4625b4x398mspr3a5077rs"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/google/go-querystring/query"
         #:unpack-path "github.com/google/go-querystring"))
      (home-page "https://github.com/google/go-querystring/")
      (synopsis "Library for encoding structs into URL query parameters")
      (description "@code{go-querystring} is Go library for encoding structs
into URL query parameters.")
      (license license:bsd-3))))

(define-public go-github-com-google-renameio
  (package
    (name "go-github-com-google-renameio")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/renameio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ki2x5a9nrj17sn092d6n4zr29lfg5ydv4xz5cp58z6cw8ip43jx"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/google/renameio"))
    (home-page "https://github.com/google/renameio/")
    (synopsis "Atomically create or replace a file or symbolic link")
    (description "@code{renameio} Go package provides a way to atomically
create or replace a file or symbolic link.")
    (license license:asl2.0)))

(define (go-gotest-tools-source version sha256-base32-hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/gotestyourself/gotest.tools")
          (commit (string-append "v" version))))
    (file-name (git-file-name "go-gotest-tools" version))
    (sha256
     (base32 sha256-base32-hash))))

;; Note that version 3.0.0 is incompatible to 2.3.0.
;; See also <https://github.com/gotestyourself/gotest.tools/issues/166>.
(define (go-gotest-tools-package suffix)
  (package
    (name (string-append "go-gotest-tools-"
                         (string-replace-substring suffix "/" "-")))
    (version "2.3.0")
    (source
     (go-gotest-tools-source version
      "0071rjxp4xzcr3vprkaj1hdk35a3v45bx8v0ipk16wwc5hx84i2i"))
    (build-system go-build-system)
    (arguments
     `(#:import-path ,(string-append "gotest.tools/" suffix)
       #:unpack-path "gotest.tools"))
    (synopsis "@code{gotest-tools} part")
    (description "This package provides a part of @code{gotest-tools}.")
    (home-page "https://github.com/gotestyourself/gotest.tools")
    (license license:asl2.0)))

(define-public go-gotest-tools-internal-format
  (package (inherit (go-gotest-tools-package "internal/format"))
    (native-inputs
     (list go-github-com-pkg-errors go-github-com-google-go-cmp-cmp))
    (synopsis "Formats messages for use with gotest-tools")
    (description "This package provides a way to format messages for use
with gotest-tools.")))

(define-public go-gotest-tools-internal-difflib
  (package (inherit (go-gotest-tools-package "internal/difflib"))
    (synopsis "Differences for use with gotest-tools")
    (description "This package computes differences for use
with gotest-tools.")))

(define-public go-gotest-tools-internal-source
  (package (inherit (go-gotest-tools-package "internal/source"))
    (arguments
     (substitute-keyword-arguments
       (package-arguments (go-gotest-tools-package "internal/source"))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'check
              (lambda* (#:key inputs #:allow-other-keys #:rest args)
                (unless
                  ;; failed to parse source file: : open : no such file or directory
                  (false-if-exception (search-input-file inputs "/bin/gccgo"))
                  (apply (assoc-ref %standard-phases 'check) args))))))))
    (native-inputs
     (list go-github-com-pkg-errors go-github-com-google-go-cmp-cmp))
    (synopsis "Source code AST formatters for gotest-tools")
    (description "This package provides source code AST formatters for
gotest-tools.")))

(define-public go-gotest-tools-assert
  (package (inherit (go-gotest-tools-package "assert"))
    (name "go-gotest-tools-assert")
    (arguments
     `(#:tests? #f ; Test failure concerning message formatting (FIXME)
       #:import-path "gotest.tools/assert"
       #:unpack-path "gotest.tools"))
    (propagated-inputs
     (list go-github-com-google-go-cmp-cmp
           go-github-com-pkg-errors
           go-github-com-spf13-pflag
           go-golang-org-x-tools))
    (synopsis "Compare values and fail a test when a comparison fails")
    (description "This package provides a way to compare values and fail a
test when a comparison fails.")
    (home-page "https://github.com/gotestyourself/gotest.tools")
    (license license:asl2.0)))

(define-public gotestsum
  (package
    (name "gotestsum")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/gotestyourself/gotestsum")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y71qr3ss3hgc8c7nmvpwk946xy1jc5d8whsv6y77wb24ncla7n0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gotest.tools/gotestsum"))
    (native-inputs
     (list go-github-com-fatih-color
           go-golang-org-x-sync
           go-github-com-pkg-errors
           go-github-com-sirupsen-logrus
           go-github-com-spf13-pflag
           go-github-com-jonboulle-clockwork
           go-golang-org-x-crypto
           go-gotest-tools-assert
           go-github-com-google-go-cmp-cmp
           ;; TODO: This would be better as a propagated-input of
           ;; go-gotest-tools-assert, but that does not work for
           ;; some reason.
           go-gotest-tools-internal-format
           go-gotest-tools-internal-difflib
           go-gotest-tools-internal-source
           go-github-com-google-go-cmp-cmp))
    (synopsis "Go test runner with output optimized for humans")
    (description "This package provides a @code{go test} runner with output
optimized for humans, JUnit XML for CI integration, and a summary of the
test results.")
    (home-page "https://github.com/gotestyourself/gotestsum")
    (license license:asl2.0)))

(define-public go-github-com-golang-protobuf
  (package
    (name "go-github-com-golang-protobuf")
    (version "1.5.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03f1w2cd4s8a3xhl61x7yjx81kbzlrjpvnnwmbhqnz814yi7h43i"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/golang/protobuf"
           #:phases
           #~(modify-phases %standard-phases
               ;; XXX: Workaround for go-build-system's lack of Go modules
               ;; support.
               (delete 'build)
               (replace 'check
                 (lambda* (#:key tests? import-path #:allow-other-keys)
                   (when tests?
                     (with-directory-excursion (string-append "src/" import-path)
                       (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-google-golang-org-protobuf))
    (synopsis "Go support for Protocol Buffers")
    (description "This package provides Go support for the Protocol Buffers
data serialization format.")
    (home-page "https://github.com/golang/protobuf")
    (license license:bsd-3)))

(define-public go-google-golang-org-protobuf
  (package
    (name "go-google-golang-org-protobuf")
    (version "1.31.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xf18kzz96hgfy1vlbnydrizzpxkqj2iamfdbj3dx5a1zz5mi8n0"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "google.golang.org/protobuf"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-failing-tests
                 (lambda* (#:key tests? unpack-path #:allow-other-keys)
                   (with-directory-excursion (string-append "src/" unpack-path)
                     (substitute* (find-files "." "\\_test.go$")
                       ;; XXX Failing on i686-linux:
                       ;; panic: unaligned 64-bit atomic operation
                       (("TestDynamicTypesExtensionNotFound")
                        "OffTestDynamicTypesExtensionNotFound")
                       (("TestDynamicTypesFilesChangeAfterCreation")
                        "OffTestDynamicTypesFilesChangeAfterCreation")
                       (("TestDynamicTypesFindExtensionByNameOrNumber")
                        "OffTestDynamicTypesFindExtensionByNameOrNumber")))))
               ;; XXX: Workaround for go-build-system's lack of Go modules
               ;; support.
               (delete 'build)
               (replace 'check
                 (lambda* (#:key tests? import-path #:allow-other-keys)
                   (when tests?
                     (with-directory-excursion (string-append "src/" import-path)
                       (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs (list go-github-com-google-go-cmp-cmp))
    (home-page "https://google.golang.org/protobuf")
    (synopsis "Go library for Protocol Buffers")
    (description
     "The protobuf package provides a Go implementation of Protocol Buffers, a
language and platform neutral, extensible mechanism for serializing structured
data.

This package is a successor to @code{go-github-com-golang-protobuf} with an
improved and cleaner API.")
    (license license:bsd-3)))

(define-public go-github-com-macronut-go-tproxy
  (package
    (name "go-github-com-macronut-go-tproxy")
    (version "0.0.0-20190726054950-ef7efd7f24ed")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FutureProtocolLab/go-tproxy")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jibsg0xhsn0h1jq4g9qd4nr58w43y8majlwfri9ffk2cbfrwqdr"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "example"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/macronut/go-tproxy"))
    (home-page "https://github.com/FutureProtocolLab/go-tproxy")
    (synopsis "Linux Transparent Proxy library")
    (description
     "Golang TProxy provides an easy to use wrapper for the Linux Transparent
Proxy functionality.")
    (license license:expat)))

(define-public go-github-com-bits-and-blooms-bitset
  (package
    (name "go-github-com-bits-and-blooms-bitset")
    (version "1.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bits-and-blooms/bitset")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ialciixmr98p10rh61rnnkxpqi1j9hycbkv9rnjl0vnmsnpy0cy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bits-and-blooms/bitset"))
    (synopsis "Bitsets in Go")
    (description "This package provides a Go implementation of bitsets, which
are a mapping between non-negative integers and boolean values focused on
efficient space usage.")
    (home-page "https://github.com/bits-and-blooms/bitset")
    (license license:bsd-3)))

(define-public go-github-com-bits-and-blooms-bloom
  (package
    (name "go-github-com-bits-and-blooms-bloom")
    (version "3.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bits-and-blooms/bloom")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02rpjlgl7k3755qnlsk519xazgqlk73b8wvkpqlvccywms5w77bq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bits-and-blooms/bloom"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-import-path
           (lambda _
             ;; See 'go.mod' in the source distribution of Syncthing 1.5.0 for
             ;; more information.
             ;; <https://github.com/spaolacci/murmur3/issues/29>
             (substitute* "src/github.com/bits-and-blooms/bloom/bloom.go"
               (("spaolacci") "twmb"))
             #t)))))
    (propagated-inputs
     (list go-github-com-twmb-murmur3 go-github-com-bits-and-blooms-bitset))
    (synopsis "Bloom filters in Go")
    (description "This package provides a Go implementation of bloom filters,
based on murmurhash.")
    (home-page "https://github.com/bits-and-blooms/bitset")
    (license license:bsd-2)))

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

(define-public go-github-com-masterminds-goutils
  (package
    (name "go-github-com-masterminds-goutils")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Masterminds/goutils")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09m4mbcdlv9ng3xcrmjlxi0niavby52y9nl2jhjnbx1xxpjw0jrh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/Masterminds/goutils"))
    (home-page "https://github.com/Masterminds/goutils/")
    (synopsis "Utility functions to manipulate strings")
    (description "GoUtils provides utility functions to manipulate strings in
various ways.  It is a Go implementation of some string manipulation libraries
of Java Apache Commons.")
    (license license:asl2.0)))

(define-public go-github-com-huandu-xstrings
  (package
    (name "go-github-com-huandu-xstrings")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/huandu/xstrings")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pwar6rc0fqb6pll38a44s81g5kb65vbg71jg5lx8caphjnikq5r"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/huandu/xstrings"))
    (home-page "https://github.com/huandu/xstrings/")
    (synopsis "Collection of string functions")
    (description "Go package xstrings is a collection of string functions,
which are widely used in other languages but absent in Go package strings.")
    (license license:expat)))

(define-public go-github-com-imdario-mergo
  (package
    (name "go-github-com-imdario-mergo")
    (version "0.3.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/imdario/mergo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09h765p8yby9r8s0a3hv5kl8n2i382mda76wmvk48w1cc1w9s92p"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/imdario/mergo"))
    (native-inputs
     (list go-gopkg-in-yaml-v2))
    (home-page "https://github.com/imdario/mergo/")
    (synopsis "Helper to merge structs and maps in Golang")
    (description "Helper to merge structs and maps in Golang.  Useful for
configuration default values, avoiding messy if-statements.

Mergo merges same-type structs and maps by setting default values in
zero-value fields.  Mergo won't merge unexported (private) fields.  It will do
recursively any exported one.  It also won't merge structs inside
maps (because they are not addressable using Go reflection).")
    (license license:bsd-3)))

(define-public go-dario-cat-mergo
  (package
    (inherit go-github-com-imdario-mergo)
    (name "go-dario-cat-mergo")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/imdario/mergo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "037k2bd97vnbyhn2sczxk0j6ijmv06n1282f76i3ky73s3qmqnlf"))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "dario.cat/mergo"
       #:import-path "dario.cat/mergo"))
    (native-inputs
     (list go-gopkg-in-yaml-v3))))

(define-public go-github-com-bmatcuk-doublestar
  (package
    (name "go-github-com-bmatcuk-doublestar")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bmatcuk/doublestar")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bk5bixl6rqa8znxghyp6zndbccx9kdyrymjahgyp6qsrp7rk144"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/bmatcuk/doublestar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-incompatible-test
           ;; This test fails with Go 1.16.
           (lambda _
             (substitute* "src/github.com/bmatcuk/doublestar/doublestar_test.go"
               (("\\{\"a\\[\", \"a\", false, nil, false\\},.*")
                "")))))))
    (home-page "https://github.com/bmatcuk/doublestar/")
    (synopsis "Path pattern matching and globbing supporting doublestar")
    (description "@code{doublestar} is a Go implementation of path pattern
matching and globbing with support for \"doublestar\" patterns.")
    (license license:expat)))

;; For chezmoi-1.8.10
(define-public go-github-com-bmatcuk-doublestar-v2
  (package
    (inherit go-github-com-bmatcuk-doublestar)
    (name "go-github-com-bmatcuk-doublestar-v2")
    (version "2.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bmatcuk/doublestar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09ma8a9rhs8dg527vjhdf3lsb6lajaq193m6ksssm2k3qajhpi94"))))
    (arguments
     (list
      #:tests? #f ; tests have more broken parts
      #:unpack-path "github.com/bmatcuk/doublestar/v2"
      #:import-path "github.com/bmatcuk/doublestar/v2"))))

(define-public go-github-com-bmatcuk-doublestar-v4
  (package
    (inherit go-github-com-bmatcuk-doublestar)
    (name "go-github-com-bmatcuk-doublestar-v4")
    (version "4.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bmatcuk/doublestar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12rf4a9isgg2nh927gikgbmyaynaqp4kjahgscb4qnr04m3vpr41"))))
    (arguments
     (list
      #:import-path "github.com/bmatcuk/doublestar/v4"))))

(define-public go-github-com-muesli-reflow
  (package
    (name "go-github-com-muesli-reflow")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/muesli/reflow")
             (commit (string-append "v" version))))
       (file-name (git-file-name "go-github-com-muesli-reflow" version))
       (sha256
        (base32 "09zcz2cqdwgj1ilya5pqwndryk6lansn87x63fcm8j1xn74vd2ry"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/muesli/reflow"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/muesli/reflow/")
    (synopsis "Collection of methods helping to transform blocks of text")
    (description
     "This package provides a collection of ANSI-aware methods and io.Writers
helping you to transform blocks of text.")
    (license license:expat)))

(define-public go-github-com-muesli-termenv
  (package
    (name "go-github-com-muesli-termenv")
    (version "0.15.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/muesli/termenv")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19yhli6k79aqpra4djp0cl4q76mqxbc1f7in20y0dzhnjb7yz42p"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/muesli/termenv"))
    (propagated-inputs
     (list go-github-com-aymanbagabas-go-osc52-v2
           go-github-com-lucasb-eyer-go-colorful
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-runewidth
           go-golang-org-x-sys))
    (home-page "https://github.com/muesli/termenv/")
    (synopsis "Advanced styling options on the terminal")
    (description "termenv lets you safely use advanced styling options on the
terminal.  It gathers information about the terminal environment in terms of
its ANSI and color support and offers you convenient methods to colorize and
style your output, without you having to deal with all kinds of weird ANSI
escape sequences and color conversions.")
    (license license:expat)))

(define-public go-github-com-olekukonko-tablewriter
  (package
    (name "go-github-com-olekukonko-tablewriter")
    (version "0.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/olekukonko/tablewriter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02r0n2b9yh3x8xyf48k17dxlwj234hlgjycylbjxi6qg08hfmz2x"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/olekukonko/tablewriter"))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/olekukonko/tablewriter/")
    (synopsis "Generate ASCII table")
    (description "This package generates ASCII tables.  Features:
@itemize
@item automatic Padding
@item support Multiple Lines
@item supports Alignment
@item support Custom Separators
@item automatic Alignment of numbers and percentage
@item write directly to http , file etc via @code{io.Writer}
@item read directly from CSV file
@item optional row line via @code{SetRowLine}
@item normalise table header
@item make CSV Headers optional
@item enable or disable table border
@item set custom footer support
@item optional identical cells merging
@item set custom caption
@item optional reflowing of paragrpahs in multi-line cells
@end itemize\n")
    (license license:expat)))

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

(define-public go-github-com-charmbracelet-glamour
  (package
    (name "go-github-com-charmbracelet-glamour")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/charmbracelet/glamour")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "073kyx94r9f0hpjv5c3x9pfdd3dbpyqcy7jhx4yxz0ps25j1a41p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/glamour"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              ;; Some tests fail due to different number of '^[0m' symbols at
              ;; the beginning and the end of paragraphs.  To fix that we
              ;; re-generate 'readme.test' so the test output will match the
              ;; 'readme.test' contents.
              (chmod "src/github.com/charmbracelet/glamour/testdata/readme.test"
                     #o644)
              (substitute* "src/github.com/charmbracelet/glamour/glamour_test.go"
                (("	generate = false")
                 "	generate = true"))))
          ;; FIXME: Pattern embedded: cannot embed directory embedded:
          ;; contains no embeddable files.
          ;;
          ;; This happens due to Golang can't determine the valid directory of
          ;; the module which is sourced during setup environment phase, but
          ;; easy resolved after coping to expected directory "vendor" within
          ;; the current package, see details in Golang source:
          ;;
          ;; - URL: <https://github.com/golang/go/blob/>
          ;; - commit: 82c14346d89ec0eeca114f9ca0e88516b2cda454
          ;; - file: src/cmd/go/internal/load/pkg.go#L2059
          (add-before 'build 'copy-input-to-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (mkdir "vendor")
                (copy-recursively
                 (string-append
                  #$(this-package-input "go-github-com-alecthomas-chroma-v2")
                  "/src/github.com")
                 "vendor/github.com"))))
          (add-before 'install 'remove-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "vendor")))))))
    (propagated-inputs
     (list go-github-com-alecthomas-chroma-v2
           go-github-com-microcosm-cc-bluemonday
           go-github-com-muesli-reflow
           go-github-com-muesli-termenv
           go-github-com-olekukonko-tablewriter
           go-github-com-yuin-goldmark
           go-github-com-yuin-goldmark-emoji))
    (home-page "https://github.com/charmbracelet/glamour/")
    (synopsis "Write handsome command-line tools with glamour")
    (description "@code{glamour} lets you render markdown documents and
templates on ANSI compatible terminals.  You can create your own stylesheet or
use one of our glamorous default themes.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-harmonica
  (package
    (name "go-github-com-charmbracelet-harmonica")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/harmonica")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aasg0c0xxhwav4ivm1mqmsqab6lk407xky8c19pb85r1hdbq0n7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/harmonica"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'remove-examples
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively
                        (string-append "src/" import-path "/examples")))))))
    (home-page "https://github.com/charmbracelet/harmonica")
    (synopsis "Simple, physics-based animation library")
    (description
     "A simple, efficient spring animation library for smooth, natural motion.")
    (license license:expat)))

(define-public go-github-com-coreos-go-semver
  (package
    (name "go-github-com-coreos-go-semver")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/coreos/go-semver")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vs04yykv1bwgvbyvi1m7ps83w06wzplw4giw8jac2iidx0x74v5"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/coreos/go-semver"))
    (propagated-inputs (list go-gopkg-in-yaml-v2))
    (home-page "https://github.com/coreos/go-semver/")
    (synopsis "Semantic versioning library")
    (description "@code{go-semver} is a semantic versioning library for Go.
It lets you parse and compare two semantic version strings.")
    (license license:asl2.0)))

(define-public go-github-com-emirpasic-gods
  (package
    (name "go-github-com-emirpasic-gods")
    (version "1.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emirpasic/gods")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i5qqq7ajvw3mikr95zl9rsnfsjanzwpqqs6kzzplsfgsifybar1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/emirpasic/gods"
       ; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://github.com/emirpasic/gods/")
    (synopsis "Implementation of various data structures and algorithms in Go")
    (description "This package provides implementation of various data
structures and algorithms in Go.")
    (license license:bsd-2)))

(define-public go-gopkg-in-warnings
  (package
    (name "go-gopkg-in-warnings")
    (version "0.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-warnings/warnings")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kzj50jn708cingn7a13c2wdlzs6qv89dr2h4zj8d09647vlnd81"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "gopkg.in/warnings.v0"))
    (home-page "https://gopkg.in/warnings.v0")
    (synopsis "Error handling with non-fatal errors")
    (description "Package warnings implements error handling with non-fatal
errors (warnings).")
    (license license:bsd-2)))

(define-public go-github-com-go-git-gcfg
  (package
    (name "go-github-com-go-git-gcfg")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-git/gcfg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lb14z4j35pwz2b2rbykkpsq515spwbndb00gwn2xlrzn949xb83"))))
    (arguments
     `(#:import-path "github.com/go-git/gcfg"))
    (native-inputs
     (list go-gopkg-in-warnings go-github-com-pkg-errors))
    (build-system go-build-system)
    (home-page "https://github.com/go-git/gcfg/")
    (synopsis "Gcfg reads INI-style configuration files into Go structs")
    (description "Gcfg reads INI-style configuration files into Go structs.")
    (license license:bsd-3)))

(define-public go-github-com-go-git-go-billy
  (package
    (name "go-github-com-go-git-go-billy")
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-git/go-billy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wdzczfk1n50dl2zpgf46m69b0sm8qkan5xyv82pk9x53zm1dmdx"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-git/go-billy/v5"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://github.com/go-git/go-billy/")
    (synopsis "File system abstraction for Go")
    (description "Billy implements an interface based on the OS's standard
library to develop applications without depending on the underlying storage.
This makes it virtually free to implement mocks and testing over
file system operations.")
    (license license:asl2.0)))

(define-public go-github-com-jba-printsrc
  (package
    (name "go-github-com-jba-printsrc")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jba/printsrc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gyy3kmb5a5i710wkv3b7ah7i7sz5sdc7v3sab5m4rxch1sd2fpj"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jba/printsrc"
       ;; TODO: Open bug; expecting time.Local, but when local=UTC, we get time.UTC
       #:tests? #f))
    (home-page "https://github.com/jba/printsrc")
    (synopsis "Prints Go values as sourcecode")
    (description
     "Package printsrc prints Go values as Go source.  It strives to render
legal Go source code, and returns an error when detects that it cannot.")
    (license license:expat)))

(define-public go-github-com-jba-templatecheck
  (package
    (name "go-github-com-jba-templatecheck")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jba/templatecheck")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12iwkidz4p6wdl65jfddqxls80mv879k2rpb42dj7y4dja5advlc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jba/templatecheck"))
    (propagated-inputs (list go-github-com-google-safehtml))
    (home-page "https://github.com/jba/templatecheck")
    (synopsis "Checks Go templates for problems")
    (description
     "Package templatecheck checks Go templates for problems.  It can detect
many errors that are normally caught only during execution.  Use templatecheck
in tests to find template errors early, and along template execution paths
that might only rarely be reached.")
    (license license:expat)))

(define-public go-github-com-jbenet-go-context
  (let ((commit "d14ea06fba99483203c19d92cfcd13ebe73135f4")
        (revision "1"))
    (package
      (name "go-github-com-jbenet-go-context")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jbenet/go-context")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0q91f5549n81w3z5927n4a1mdh220bdmgl42zi3h992dcc4ls0sl"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/jbenet/go-context"
         ; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (home-page "https://github.com/jbenet/go-context/")
      (synopsis "@code{jbenet's} context extensions")
      (description "This package provides @code{jbenet's} context
extensions.")
      (license license:expat))))

(define-public go-github-com-juju-ansiterm
  (package
    (name "go-github-com-juju-ansiterm")
    (version "0.0.0-20210929141451-8b71cc96ebdc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/juju/ansiterm")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05mk7mlvg11dd6b0j0wlq547ghbmx2ywwrlbcb4kddpg7qaqp1va"))))
    (build-system go-build-system)
    (arguments (list #:import-path "github.com/juju/ansiterm"))
    (propagated-inputs
     (list go-gopkg-in-check-v1
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-colorable
           go-github-com-lunixbochs-vtclean))
    (home-page "https://github.com/juju/ansiterm")
    (synopsis "Writer to output ANSI escape codes for color and styles")
    (description
     "The ansiterm package provides a writer to output the ANSI escape codes
for color and styles.")
    (license license:lgpl3)))

(define-public go-github-com-kevinburke-ssh-config
  (package
    (name "go-github-com-kevinburke-ssh-config")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kevinburke/ssh_config")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05jvz5r58a057zxvic9dyr9v2wilha8l6366npwkqgxmnmk9sh5f"))))
    (arguments
     `(#:import-path "github.com/kevinburke/ssh_config"))
    (build-system go-build-system)
    (home-page "https://github.com/kevinburke/ssh_config/")
    (synopsis "Parser for @file{ssh_config} files")
    (description "This is a Go parser for @file{ssh_config} files.
Importantly, this parser attempts to preserve comments in a given file, so you
can manipulate a @file{ssh_config} file from a program.")
    (license license:expat)))

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

(define-public go-github-com-go-git-go-git-fixtures
  (package
    (name "go-github-com-go-git-go-git-fixtures")
    (version "4.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-git/go-git-fixtures")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "002yb1s2mxq2xijkl39ip1iyc3l52k23ikyi9ijfl4bgqxy79ljg"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-git/go-git-fixtures/v4"
       #:phases
       (modify-phases %standard-phases
         (delete 'reset-gzip-timestamps))))
    (native-inputs
     (list go-github-com-alcortesm-tgz go-github-com-go-git-go-billy
           go-golang-org-x-sys go-gopkg-in-check-v1))
    (home-page "https://github.com/go-git/go-git-fixtures/")
    (synopsis "Fixtures used by @code{go-git}")
    (description "This package provides fixtures used by @code{go-git}.")
    (license license:asl2.0)))

(define-public go-github-com-pkg-diff
  (package
    (name "go-github-com-pkg-diff")
    (version "0.0.0-20210226163009-20ebb0f2a09e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pkg/diff")
                    (commit "20ebb0f2a09e612109b224b32f79370409108bcc")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g3dzgwhz4fx3ddpsv7fsa4r1v5clsp2lbw2qrkdk9y1vc5gi8yi"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/pkg/diff"))
    (home-page "https://github.com/pkg/diff/")
    (synopsis "Create and print diffs")
    (description
     "This package provides a Go library to create and print diffs.")
    (license license:bsd-3)))

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

(define-public go-github-com-twpayne-go-vfs
  (package
    (name "go-github-com-twpayne-go-vfs")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/go-vfs")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19dm3gi45znwaqbzxhwcgkiz8059bwa3ank80hc6qhdl579bpjnz"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/twpayne/go-vfs"))
    (native-inputs
     (list go-github-com-bmatcuk-doublestar
           go-github-com-stretchr-testify))
    (home-page "https://github.com/twpayne/go-vfs/")
    (synopsis "Abstraction of the @code{os} and @code{ioutil} Go packages")
    (description "Package @code{vfs} provides an abstraction of the @code{os}
and @code{ioutil} packages that is easy to test.")
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
     (list go-github-com-twpayne-go-vfs go-github-com-spf13-afero))
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
     (list go-github-com-stretchr-testify go-github-com-twpayne-go-vfs))
    (home-page "https://github.com/twpayne/go-xdg/")
    (synopsis "Functions related to freedesktop.org")
    (description "Package @code{xdg} provides functions related to
@uref{freedesktop.org}.")
    (license license:expat)))

(define-public go-github-com-xdg-go-stringprep
  (package
    (name "go-github-com-xdg-go-stringprep")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xdg-go/stringprep")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1df0l5n3c520y9filzz83j42wa5c056jcygmfwhjyf1pq8f6jkv9"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/xdg-go/stringprep"))
    (propagated-inputs
     (list go-golang-org-x-text))
    (home-page "https://github.com/xdg-go/stringprep")
    (synopsis "Go implementation of RFC-3454 stringprep and RFC-4013 SASLprep")
    (description
     "Package stringprep provides data tables and algorithms for RFC-3454,
including errata.  It also provides a profile for SASLprep as defined in
RFC-4013.")
    (license license:asl2.0)))

(define-public go-github-com-xdg-go-pbkdf2
  (package
    (name "go-github-com-xdg-go-pbkdf2")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xdg-go/pbkdf2")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1nipijy5xkdnfyhkp5ryrjzm14si1i2v2xyfmblf84binwkbr8jh"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/xdg-go/pbkdf2"))
    (home-page "https://github.com/xdg-go/pbkdf2")
    (synopsis "Go implementation of PBKDF2")
    (description
     "Package pbkdf2 implements password-based key derivation using the PBKDF2
algorithm described in @url{https://rfc-editor.org/rfc/rfc2898.html,RFC 2898}
and @url{https://rfc-editor.org/rfc/rfc8018.html,RFC 8018}.")
    (license license:asl2.0)))

(define-public go-github-com-xdg-go-scram
  (package
    (name "go-github-com-xdg-go-scram")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xdg-go/scram")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1jyv4qgc1dgh3v96pazmgljpp9ij25k8hwn0v4fck18g16i0nccm"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/xdg-go/scram"))
    (propagated-inputs
     (list go-github-com-xdg-go-stringprep
           go-github-com-xdg-go-pbkdf2))
    (home-page "https://github.com/xdg-go/scram")
    (synopsis "Go implementation of RFC-5802")
    (description
     "Package scram provides client and server implementations of the
@acronym{Salted Challenge Response Authentication Mechanism, SCRAM} described in
RFC-5802 and RFC-7677.")
    (license license:asl2.0)))

(define-public go-github-com-godbus-dbus
  (package
    (name "go-github-com-godbus-dbus")
    (version "5.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/godbus/dbus")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kayd4x7idrhi06ahh5kqkgwzgh9icvv71mjar2d0jl486dfs8r5"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ;no /var/run/dbus/system_bus_socket
       #:import-path "github.com/godbus/dbus"))
    (native-inputs
     (list dbus)) ;dbus-launch
    (home-page "https://github.com/godbus/dbus/")
    (synopsis "Native Go client bindings for the D-Bus")
    (description "@code{dbus} is a library that implements native Go client
bindings for the D-Bus message bus system.")
    (license license:bsd-2)))

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
      (propagated-inputs (list go-github-com-godbus-dbus))
      (home-page "https://github.com/delthas/go-libnp")
      (synopsis "Tiny library providing information about now-playing media")
      (description "@code{go-libnp} is a tiny cross-platform library for
extracting information about the music/image/video that is Now Playing on the
system.")
      (license license:expat))))

(define-public go-github-com-delthas-go-localeinfo
  (let ((commit "686a1e18511819b2f1625f00437f6e1246c04a5d"))
    (package
      (name "go-github-com-delthas-go-localeinfo")
      (version (git-version "0.0.0" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/delthas/go-localeinfo")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                  (base32
                    "0r0v42ggvyss8327nggwinxl42pj4l0dwz96g9wk1w8h8vmfrh0z"))))
      (build-system go-build-system)
      (arguments `(#:tests? #f ; FIXME: tests assume certain locale
                   #:import-path "github.com/delthas/go-localeinfo"))
      (home-page "https://github.com/delthas/go-localeinfo")
      (synopsis "Library for extracting locale information")
      (description "@code{go-localeinfo} extracts monetary/numeric/time
formatting information, rather than the current locale name.")
      (license license:expat))))

(define-public go-github-com-prometheus-common
  (package
    (name "go-github-com-prometheus-common")
    (version "0.45.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/prometheus/common")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "006y6mlxglr2xzmdqxl5bwh899whfx1prcgjai7qhhs5ys5dspy5"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/prometheus/common"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; Source-only package
         (delete 'build))))
    (propagated-inputs
     (list go-github-com-golang-protobuf
           go-github-com-matttproud-golang-protobuf-extensions-v2
           go-github-com-prometheus-client-model))
    (synopsis "Prometheus metrics")
    (description "This package provides tools for reading and writing
Prometheus metrics.")
    (home-page "https://github.com/prometheus/common")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-procfs
  (package
    (name "go-github-com-prometheus-procfs")
    (version "0.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/prometheus/procfs")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z5jq5rjala0a0di4nwk1rai0z9f73qwqj6mgcbpjbg2qknlb544"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/prometheus/procfs"
       ;; The tests require Go modules, which are not yet supported in Guix's
       ;; Go build system.
       #:tests? #f))
    (propagated-inputs (list go-golang-org-x-sync))
    (synopsis "Go library for reading @file{/proc}")
    (description "The @code{procfs} Go package provides functions to retrieve
system, kernel, and process metrics from the @file{/proc} pseudo file system.")
    (home-page "https://github.com/prometheus/procfs")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-client-golang
  (package
    (name "go-github-com-prometheus-client-golang")
    (version "1.17.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/prometheus/client_golang")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v8vdvi9wlpf18nxi62diysfnh9gc3c3cqq6hvx378snsvvl6n82"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f
       #:import-path "github.com/prometheus/client_golang"
       #:phases
       (modify-phases %standard-phases
         ;; Source-only package
         (delete 'build))))
    (propagated-inputs
     (list go-github-com-beorn7-perks-quantile
           go-github-com-golang-protobuf
           go-github-com-prometheus-client-model
           go-github-com-prometheus-common
           go-github-com-prometheus-procfs
           go-github-com-cespare-xxhash))
    (synopsis "HTTP server and client tools for Prometheus")
    (description "This package @code{promhttp} provides HTTP client and
server tools for Prometheus metrics.")
    (home-page "https://github.com/prometheus/client_golang")
    (license license:asl2.0)))

(define-public go-github-com-zalando-go-keyring
  (package
    (name "go-github-com-zalando-go-keyring")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zalando/go-keyring")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kj54nkiyccy6m9iy9a53f6412a54xk96j88jaiq35yzdgfa4z3p"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ;XXX: Fix dbus tests
       #:import-path "github.com/zalando/go-keyring"))
    (propagated-inputs
     (list go-github-com-godbus-dbus dbus))
    (home-page "https://github.com/zalando/go-keyring/")
    (synopsis "Library for working with system keyring")
    (description "@code{go-keyring} is a library for setting, getting and
deleting secrets from the system keyring.")
    (license license:expat)))

(define-public go-github-com-zclconf-go-cty
  (package
    (name "go-github-com-zclconf-go-cty")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zclconf/go-cty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f9a6vy45gcx5pg5bnfs63manaqw80h7xzvmj3b80af38304zr71"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/zclconf/go-cty"
       #:import-path "github.com/zclconf/go-cty/cty"))
    (native-inputs
     (list go-github-com-google-go-cmp-cmp))
    (propagated-inputs
     (list go-golang-org-x-text
           go-github-com-vmihailenco-msgpack-v4
           go-github-com-apparentlymart-go-textseg-v13))
    (home-page "https://github.com/zclconf/go-cty")
    (synopsis "Type system for dynamic values in Go applications")
    (description
     "@code{cty} (pronounced \"see-tie\") is a dynamic type system for
applications written in Go that need to represent user-supplied values without
losing type information.  The primary intended use is for implementing
configuration languages, but other uses may be possible too.")
    (license license:expat)))

(define-public go-github-com-rogpeppe-go-internal
  (package
    (name "go-github-com-rogpeppe-go-internal")
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rogpeppe/go-internal")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bh08k8fy1qcc0vzyv0xkg0sx5kjx348zd1dpjmp3rbrr6xrpaaw"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/rogpeppe/go-internal"
       ; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs
     (list go-github-com-pkg-diff))
    (home-page "https://github.com/rogpeppe/go-internal/")
    (synopsis "Internal packages from the Go standard library")
    (description "This repository factors out an opinionated selection of
internal packages and functionality from the Go standard library.  Currently
this consists mostly of packages and testing code from within the Go tool
implementation.

Included are the following:
@itemize
@item dirhash: calculate hashes over directory trees the same way that the Go tool does.
@item goproxytest: a GOPROXY implementation designed for test use.
@item gotooltest: Use the Go tool inside test scripts (see testscript below)
@item imports: list of known architectures and OSs, and support for reading import import statements.
@item modfile: read and write go.mod files while preserving formatting and comments.
@item module: module paths and versions.
@item par: do work in parallel.
@item semver: semantic version parsing.
@item testenv: information on the current testing environment.
@item testscript: script-based testing based on txtar files
@item txtar: simple text-based file archives for testing.
@end itemize\n")
    (license license:bsd-3)))

(define-public go-gopkg-in-errgo-fmt-errors
  (package
    (name "go-gopkg-in-errgo-fmt-errors")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-errgo/errgo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "065mbihiy7q67wnql0bzl9y1kkvck5ivra68254zbih52jxwrgr2"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "gopkg.in/errgo.v2"
       #:tests? #f
       ;; Source-only package
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://godoc.org/gopkg.in/errgo.v2")
    (synopsis "Functions that use the fmt package to format error messages")
    (description "This package is the same as @code{gopkg.in/errgo.v2/errors}
except that it adds convenience functions that use the fmt package to format
error messages.")
    (license license:bsd-3)))

(define-public go-github-com-arceliar-phony
  (let ((commit "d0c68492aca0bd4b5c5c8e0452c9b4c8af923eaf")
        (revision "0"))
    (package
      (name "go-github-com-arceliar-phony")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Arceliar/phony")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0876y0hlb1zh8hn0pxrb5zfdadvaqmqwlr66p19yl2a76galz992"))))
      (arguments
       '(#:import-path "github.com/Arceliar/phony"))
      (build-system go-build-system)
      (home-page "https://github.com/Arceliar/phony")
      (synopsis "Very minimal actor model library")
      (description "Phony is a very minimal actor model library for Go,
inspired by the causal messaging system in the Pony programming language.")
      (license license:expat))))

(define-public go-github-com-gologme-log
  ;; this is the same as v1.2.0, only the LICENSE file changed
  (let ((commit "720ba0b3ccf0a91bc6018c9967a2479f93f56a55"))
    (package
      (name "go-github-com-gologme-log")
      (version "1.2.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gologme/log")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0z3gs5ngv2jszp42ypp3ai0pn410v3b2m674g73ma7vsbn2yjk1n"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/gologme/log"))
      (home-page "https://github.com/gologme/log/")
      (synopsis
       "Fork of the golang built in log package to add support for levels")
      (description "This package is a drop in replacement for the built-in Go
log package.  All the functionality of the built-in package still exists and
is unchanged.  This package contains a series of small enhancements and
additions.")
      (license license:bsd-3))))

(define-public go-golang-zx2c4-com-wireguard
  (package
    (name "go-golang-zx2c4-com-wireguard")
    (version "0.0.20211016")
    (source
     (origin
       (method git-fetch)
       ;; NOTE: module URL is a redirect
       ;; target: git.zx2c4.com/wireguard-go
       ;; source: golang.zx2c4.com/wireguard
       (uri (git-reference
             (url "https://git.zx2c4.com/wireguard-go/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09a4gsh75a8bj71wr042afrma9frriqp60cm0cx6c9a8lv5yzzi0"))))
    (build-system go-build-system)
    (arguments
     '(;; XXX: Failed on newer version of Golang, the recent release requires
       ;; gvisor.dev/gvisor, which is huge to pack.
       #:tests? #f
       #:import-path "golang.zx2c4.com/wireguard"))
    (propagated-inputs
     (list go-golang-org-x-crypto go-golang-org-x-net go-golang-org-x-sys
           go-golang-org-x-text))
    (home-page "https://git.zx2c4.com/wireguard")
    (synopsis "Implementation of WireGuard in Go")
    (description "This package is a Go Implementation of WireGuard.")
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

(define-public go-github-com-tekwizely-go-parsing
  (let ((commit "1548cfb17df54d365ce9febed0677c06a40a8ceb")
        (revision "0"))
    (package
      (name "go-github-com-tekwizely-go-parsing")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tekwizely/go-parsing")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bv5amka8hb9crc7qvlzi2kbycqrnh9d46b9wgcs8wqzl0z7w609"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/tekwizely/go-parsing"))
      (home-page "https://github.com/tekwizely/go-parsing")
      (synopsis "Text parsing, with lexers, parsers, and related tools")
      (description
       "This package provides Go modules focused on text parsing, with lexers,
parsers, and related tools.")
      (license license:expat))))

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

(define-public go-github-com-pborman-getopt
  (package
    (name "go-github-com-pborman-getopt")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pborman/getopt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sacv6g8cxfibxd3gnfjnzp7fynrnc4s2aaz5wbxivqqhvflc22l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/pborman/getopt"))
    (home-page "https://github.com/pborman/getopt")
    (synopsis "Getopt style option parsing for Go")
    (description
     "This package provides traditional getopt processing for implementing
programs that use traditional command lines.")
    (license license:bsd-3)))

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
    (native-inputs (list go-gopkg-in-errgo-fmt-errors))
    (propagated-inputs (list go-github-com-pkg-diff
                             go-github-com-kr-text
                             go-github-com-kr-pretty
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-golang-org-x-mod
                             go-github-com-rogpeppe-go-internal
                             go-github-com-google-go-cmp-cmp
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

(define-public xurls
  (package
    (name "xurls")
    (version "2.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mvdan/xurls")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b040nbk1vwlk1qljavh8w8fn2r243q700n6gr8j2asmnz0xq84p"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "mvdan.cc/xurls/v2"
       #:unpack-path "mvdan.cc/xurls/v2"
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda arguments
                      (apply (assoc-ref %standard-phases
                                        'build)
                             `(,@arguments #:import-path
                               "mvdan.cc/xurls/v2/cmd/xurls")))))))
    (inputs (list go-golang-org-x-sync go-github-com-rogpeppe-go-internal))
    (home-page "https://mvdan.cc/xurls/v2/")
    (synopsis "Extracts URLs from text")
    (description
     "Xurls extracts urls from plain text using regular expressions.  It can
be used as both a binary and a library.")
    (license license:bsd-3)))

(define-public go-mvdan-cc-xurls
  (package
    (inherit xurls)
    (name "go-mvdan-cc-xurls")
    (arguments
     `(#:import-path "mvdan.cc/xurls"
       #:tests? #f
       #:install-source? #t
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (propagated-inputs (package-inputs xurls))
    (native-inputs '())
    (inputs '())))

(define-public go-github-com-davecgh-go-xdr
  (package
    (name "go-github-com-davecgh-go-xdr")
    (version "0.0.0-20161123171359-e6a2ba005892")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/davecgh/go-xdr")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vifrz4iil4r7k8sz5iqlfbh80ysgs5abp2simgyhsbrkxrrsrrd"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/davecgh/go-xdr/xdr2"
       #:unpack-path "github.com/davecgh/go-xdr"))
    (home-page "https://github.com/davecgh/go-xdr")
    (synopsis "Pure Go implementation of the XDR standard")
    (description "@code{go-xdr} implements the data representation portion of
the External Data Representation (XDR) standard protocol as specified in RFC
4506 (obsoletes RFC 1832 and RFC 1014) in pure Go.")
    (license license:isc)))

(define-public go-github-com-dustin-go-humanize
  (package
    (name "go-github-com-dustin-go-humanize")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dustin/go-humanize")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1kqf1kavdyvjk7f8kx62pnm7fbypn9z1vbf8v2qdh3y7z7a0cbl3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/dustin/go-humanize"))
    (home-page "https://github.com/dustin/go-humanize")
    (synopsis "Humane unit formatter")
    (description "@code{go-humanize} provides formatters for units to human
friendly sizes.  It converts boring ugly numbers to human-friendly strings and
back.")
    (license license:expat)))

(define-public go-github-com-oneofone-xxhash
  (package
    (name "go-github-com-oneofone-xxhash")
    (version "1.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OneOfOne/xxhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0f98qk83l2fhpclvrgyxsa9b8m4pipf11fah85bnjl01wy4lvybw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/OneOfOne/xxhash"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'remove-benchmarks
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively
                        (string-append "src/" import-path "/benchmarks")))))))
    (home-page "https://github.com/OneOfOne/xxhash")
    (synopsis "Go implementation of xxHash")
    (description "This is a native Go implementation of the
@url{https://github.com/Cyan4973/xxHash, xxHash} algorithm, an extremely fast
non-cryptographic hash algorithm, working at speeds close to RAM limits.")
    (license license:asl2.0)))

(define-public go-gopkg-in-djherbis-times-v1
  (package
    (name "go-gopkg-in-djherbis-times-v1")
    (version "1.5.0")
    (home-page "https://gopkg.in/djherbis/times.v1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xvl3rgjif5yf62p16yk05kxrsmzhz1kkqisvw4k02svzq10qbfy"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "example"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/djherbis/times.v1"))
    (synopsis "Go library for getting file times")
    (description
     "Provides a platform-independent way to get atime, mtime, ctime and btime for files.")
    (license license:expat)))

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

(define-public go-github-com-vmihailenco-msgpack-v4
  (package
    (name "go-github-com-vmihailenco-msgpack-v4")
    (version "4.3.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmihailenco/msgpack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aiavk7b5fn050bbc0naldk2bsl60f8wil5i6a1cfp3lxxnvmvng"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/vmihailenco/msgpack/v4"))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (propagated-inputs
     (list go-github-com-vmihailenco-tagparser))
    (home-page "https://github.com/vmihailenco/msgpack")
    (synopsis "MessagePack encoding for Golang")
    (description
     "This package provides implementation of MessagePack encoding for Go
programming language.")
    (license license:bsd-2)))

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

(define-public go-github-com-rivo-uniseg
  (package
    (name "go-github-com-rivo-uniseg")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rivo/uniseg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j7h22vfmjj562vr8gpsyrkrwp1pq9ayh5fylv24skxb467g9f0q"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/rivo/uniseg"))
    (home-page "https://github.com/rivo/uniseg")
    (synopsis "Unicode Text Segmentation for Go")
    (description
     "This package implements Unicode Text Segmentation according to
@url{https://unicode.org/reports/tr29/, Unicode Standard Annex #29}.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-bubbletea
  (package
    (name "go-github-com-charmbracelet-bubbletea")
    (version "0.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/bubbletea")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1105cggi5fwqx69m0vrhgwx6kaw82w4ahn58sj0a81603c4yvrk0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/bubbletea"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'remove-examples
                     (lambda* (#:key import-path #:allow-other-keys)
                       (with-directory-excursion (string-append "src/" import-path)
                         (for-each delete-file-recursively
                                   '("examples" "tutorials"))))))))
    (propagated-inputs
     `(("github.com/mattn/go-isatty" ,go-github-com-mattn-go-isatty)
       ("github.com/muesli/termenv" ,go-github-com-muesli-termenv)
       ("github.com/mattn/go-runewidth" ,go-github-com-mattn-go-runewidth)
       ("go-github-com-muesli-reflow" ,go-github-com-muesli-reflow)
       ("go-github-com-lucasb-eyer-go-colorful" ,go-github-com-lucasb-eyer-go-colorful)
       ("github.com/containerd/console" ,go-github-com-containerd-console)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-golang-org-x-term" ,go-golang-org-x-term)
       ("github.com/mattn/go-isatty" ,go-github-com-mattn-go-isatty)))
    (home-page "https://github.com/charmbracelet/bubbletea")
    (synopsis "Powerful little TUI framework")
    (description
     "Bubble Tea is a Go framework based on The Elm Architecture.  It is
well-suited for simple and complex terminal applications, either inline,
full-window, or a mix of both.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-console
  (package
    (name "go-github-com-containerd-console")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/console")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pgx0y8x23jwc2f9jfk5hd5aslqk599nj6c7dj5846xvnkz2x7p2"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/containerd/console"))
    (propagated-inputs
     `(("golang.org/x/sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/containerd/console")
    (synopsis "Console package for Go")
    (description
     "This is Golang package for dealing with consoles.  It has few
dependencies and a simple API.")
    (license license:asl2.0)))

(define-public go-github-com-arceliar-ironwood
  (package
    (name "go-github-com-arceliar-ironwood")
    (version "v0.0.0-20240529054413-b8e59574e2b2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Arceliar/ironwood")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "06ay82gqm3k649m7x0r3a3crnqv9x0yxhyqfabrf1b7inki35mfs"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/Arceliar/ironwood"
           #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
             ;; Source-only package
             (delete 'build))))
    (propagated-inputs
     (list go-golang-org-x-crypto go-github-com-arceliar-phony))
    (home-page "https://github.com/Arceliar/ironwood")
    (synopsis "Experimental network routing library")
    (description
     "Ironwood is a routing library with a @code{net.PacketConn}-compatible
interface using @code{ed25519.PublicKey}s as addresses.  Basically, you use it
when you want to communicate with some other nodes in a network, but you can't
guarantee that you can directly connect to every node in that network.  It was
written to test improvements to / replace the routing logic in
@url{https://github.com/yggdrasil-network/yggdrasil-go,Yggdrasil}, but it may
be useful for other network applications.")
    (license license:mpl2.0)))

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
    (version "6.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/99designs/aws-vault")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fjdslg7nhlm9hl7bg69j1izrjx6sspbhwk973d0m8ig9nkdja06"))))
    (build-system go-build-system)
    (native-inputs
     (list go-github-com-99designs-go-keyring
           go-github-com-mtibben-androiddnsfix
           go-github-com-mtibben-percent
           go-github-com-jmespath-go-jmespath
           go-github-com-dvsekhvalnov-jose2go
           go-github-com-godbus-dbus
           go-github-com-gsterjov-go-libsecret
           go-github-com-mitchellh-go-homedir
           go-golang-org-x-crypto
           go-golang-org-x-sys
           go-golang-org-x-term
           go-gopkg-in-ini
           go-github-com-skratchdot-open-golang
           go-github-com-alecthomas-kingpin
           go-github-com-alecthomas-template
           go-github-com-alecthomas-units
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-service-iam
           go-github-com-aws-aws-sdk-go-v2-service-sso
           go-github-com-aws-aws-sdk-go-v2-service-ssooidc
           go-github-com-aws-aws-sdk-go-v2-service-sts))
    (arguments
     `(#:import-path "github.com/99designs/aws-vault"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-version
           (lambda _
             (substitute* "src/github.com/99designs/aws-vault/main.go"
               (("var Version = \"dev\"")
                (string-append "var Version = \"v" ,version "\"")))))
         (add-after 'build 'contrib
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zsh-site-dir (string-append out "/share/zsh/site-functions"))
                    (bash-completion-dir
                     (string-append out "/share/bash-completion/completions"))
                    (fish-completion-dir
                     (string-append out "/share/fish/completions")))
               (for-each mkdir-p
                         `(,zsh-site-dir ,bash-completion-dir ,fish-completion-dir))
               (with-directory-excursion
                   "src/github.com/99designs/aws-vault/contrib/completions"
                 (copy-file "zsh/aws-vault.zsh"
                            (string-append zsh-site-dir "/_aws-vault"))
                 (copy-file "bash/aws-vault.bash"
                            (string-append bash-completion-dir "/aws-vault"))
                 (copy-file "fish/aws-vault.fish"
                            (string-append fish-completion-dir "/aws-vault.fish")))))))))
    (synopsis
     "Vault for securely storing and accessing AWS credentials")
    (description
     "AWS Vault is a tool to securely store and access @acronym{Amazon Web
Services,AWS} credentials.

AWS Vault stores IAM credentials in your operating system's secure keystore and
then generates temporary credentials from those to expose to your shell and
applications.  It's designed to be complementary to the AWS CLI tools, and is
aware of your profiles and configuration in ~/.aws/config.")
    (home-page "https://github.com/99designs/aws-vault")
    (license license:expat)))

(define-public go-github-com-gsterjov-go-libsecret
  (let ((commit "a6f4afe4910cad8688db3e0e9b9ac92ad22d54e1")
        (revision "0"))
    (package
      (name "go-github-com-gsterjov-go-libsecret")
      (version "5.0.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gsterjov/go-libsecret")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09zaiadnll83vs22ib89agg7anj0blw5fywvmckxllsgif6ak6v7"))))
      (build-system go-build-system)
      (native-inputs
       (list go-github-com-godbus-dbus))
      (arguments
       '(#:import-path "github.com/gsterjov/go-libsecret"
         #:phases %standard-phases))
      (synopsis "Manage secrets via the \"Secret Service\" DBus API")
      (description
       "This native Go library manages secrets via the freedesktop.org
\"Secret Service\" DBus interface.")
      (home-page "https://github.com/gsterjov/go-libsecret")
      (license license:expat))))

(define-public go-github-com-mtibben-androiddnsfix
  (let ((commit "ff02804463540c36e3a148dcf4b009d003cf2a31")
        (revision "0"))
    (package
      (name "go-github-com-mtibben-androiddnsfix")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mtibben/androiddnsfix")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pcbjs793kd0yg3dcp79agfxm7xm3sldx2r7v66ipzpcq0j2npi2"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/mtibben/androiddnsfix"
         #:phases %standard-phases))
      (synopsis "Work around lack of @file{/etc/resolv.conf} on Android")
      (description
       "This package allows Go applications to work around lack of
@file{/etc/resolv.conf} on Android, as described in
@url{https://github.com/golang/go/issues/8877}.")
      (home-page "https://github.com/mtibben/androiddnsfix")
      (license license:expat))))

(define-public go-github-com-androiddnsfix
  (deprecated-package "go-github-com-androiddnsfix" go-github-com-mtibben-androiddnsfix))

(define-public go-gopkg-in-ini
  (package
    (name "go-gopkg-in-ini")
    (version "1.62.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gopkg.in/ini.v1")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dm9ydqyflasp5li22kb0w73s6kp2swii8naqfhnz64v171gmm5v"))))
    (build-system go-build-system)
    (native-inputs
     (list go-github.com-smartystreets-goconvey))
    (arguments
     '(#:import-path "gopkg.in/ini.v1"
       #:phases %standard-phases))
    (synopsis "INI file read and write functionality in Go")
    (description
     "This package provides INI file read and write functionality in Go.")
    (home-page "https://gopkg.in/ini.v1")
    (license license:asl2.0)))

;;; XXX: Since commit bfb61065f05a6eac0cf63b16db43d0c3e864c658, the
;;; canonical name of the ini package is `go-github-com-go-ini-ini`,
;;; not `go-gopkg-in-ini`.
(define-public go-github-com-go-ini-ini
  (package
    (inherit go-gopkg-in-ini)
    (name "go-github-com-go-ini-ini")
    (version "1.66.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-ini/ini")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kqg13606hnw8f75cb59fsy1m85kiqf3csi2g7q2512avdmaphc9"))))
    (arguments
     (list #:import-path "github.com/go-ini/ini"))
    (propagated-inputs (list go-github-com-stretchr-testify))))

(define-public go-github-com-skratchdot-open-golang
  (let ((commit "79abb63cd66e41cb1473e26d11ebdcd68b04c8e5")
        (revision "0"))
    (package
      (name "go-github-com-skratchdot-open-golang")
      (version "1.42.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/skratchdot/open-golang")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0jc13jn8cj7p14n79zhav2nwga6kf9rqs01ic5k7j7agwzzly3ww"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/skratchdot/open-golang"
         #:phases (modify-phases %standard-phases
                    (delete 'build)
                    (delete 'check))))
      (synopsis "Open a file, directory, or URI using the default application")
      (description
       "Open a file, directory, or URI using the OS's default application for
that object type.  Optionally, you can specify an application to use.  On
GNU/Linux, this is a proxy for the @command{xdg-open} command.")
      (home-page "https://github.com/skratchdot/open-golang")
      (license license:expat))))

(define-public go-github-com-dreamacro-go-shadowsocks2
  (package
    (name "go-github-com-dreamacro-go-shadowsocks2")
    (version "0.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Dreamacro/go-shadowsocks2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sjr3r77fav6q0ii6dnp4px9gaz7cq861a0yxppvb6a58420bx3h"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/Dreamacro/go-shadowsocks2"))
    (propagated-inputs (list go-golang-org-x-crypto))
    (home-page "https://github.com/Dreamacro/go-shadowsocks2")
    (synopsis "Shadowsocks implementation in Go")
    (description
     "This package is @code{shadowsocks} implementation in Go

Features:
@itemize
@item SOCKS5 proxy
@item Support for Netfilter TCP redirect (IPv6 should work but not tested)
@item UDP tunneling (e.g. relay DNS packets)
@item TCP tunneling (e.g. benchmark with iperf3)
@end itemize")
    (license license:asl2.0)))

(define-public go-sigs-k8s-io-yaml
  (package
    (name "go-sigs-k8s-io-yaml")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kubernetes-sigs/yaml")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qxs0ppqwqrfqs4aywyn1h28xh1qlj5ds4drmygaz1plrxj02dqn"))))
    (build-system go-build-system)
    (arguments '(#:import-path "sigs.k8s.io/yaml"))
    (propagated-inputs (list go-gopkg-in-yaml-v2 go-github-com-davecgh-go-spew))
    (home-page "https://sigs.k8s.io/yaml")
    (synopsis "YAML marshaling and unmarshaling support for Go")
    (description
     "This package provides a Go library that first converts YAML to JSON
using @code{go-yaml} and then uses @code{json.Marshal} and
@code{json.Unmarshal} to convert to or from the struct. This means that
it effectively reuses the JSON struct tags as well as the custom JSON
methods @code{MarshalJSON} and @code{UnmarshalJSON} unlike
@code{go-yaml}.

kubernetes-sigs/yaml is a permanent fork of
@url{https://github.com/ghodss/yaml,ghodss/yaml}.")
    (license (list license:expat license:bsd-3))))

(define-public go-github-com-google-go-jsonnet
  (package
    (name "go-github-com-google-go-jsonnet")
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/go-jsonnet")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dghqygag123zkgh2vrnq82cdag5z0p03v3489pwhs06r5g27wm3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/go-jsonnet/cmd/jsonnet"
       #:unpack-path "github.com/google/go-jsonnet"))
    (propagated-inputs (list go-sigs-k8s-io-yaml go-gopkg-in-yaml-v2
                             go-github-com-sergi-go-diff
                             go-github-com-fatih-color))
    (home-page "https://github.com/google/go-jsonnet")
    (synopsis "Go implementation of Jsonnet")
    (description
     "This package provides an implementation of the @url{http://jsonnet.org/,
Jsonnet} data templating language in Go.  It is a feature-complete,
production-ready implementation, compatible with the original Jsonnet C++
implementation.")
    (license license:asl2.0)))

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

(define-public go-github-com-google-btree
  (package
    (name "go-github-com-google-btree")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/btree")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cqa8ck26p3wqz877hcvmfss17xm8wcbwd68shxv795ppahpdd9b"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/btree"))
    (home-page "https://github.com/google/btree")
    (synopsis "Simple, ordered, in-memory data structure for Go programs")
    (description
     "This package provides an in-memory B-Tree implementation for Go, useful as an
ordered, mutable data structure.")
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

(define-public go-github-com-zenhack-go-notmuch
  (package
    (name "go-github-com-zenhack-go-notmuch")
    (version "0.0.0-20211022191430-4d57e8ad2a8b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zenhack/go.notmuch")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j2s5smjf7pp7i72dw12sm9iz961y3cy8nkm7hmrg53f6wna57h9"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/zenhack/go.notmuch"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-notmuch-path
                          (lambda* (#:key inputs import-path
                                    #:allow-other-keys)
                            (substitute* (find-files (string-append "src/"
                                                      import-path) "\\.go$")
                              (("// #cgo LDFLAGS:.*$")
                               (string-append "// #cgo LDFLAGS: -lnotmuch "
                                              "-L"
                                              #$(this-package-input "notmuch")
                                              "/lib\n"
                                              "// #cgo CFLAGS: "
                                              "-I"
                                              #$(this-package-input "notmuch")
                                              "/include\n")))))
                        (add-before 'check 'unpack-test-fixtures
                          (lambda* (#:key inputs import-path
                                    #:allow-other-keys)
                            (invoke "tar" "xf"
                                    #+notmuch-fixtures "-C"
                                    (string-append "src/" import-path
                                                   "/fixtures")))))))
    (inputs (list notmuch))
    (home-page "https://github.com/zenhack/go.notmuch")
    (synopsis "Go bindings to libnotmuch")
    (description
     "The notmuch package provides a Go language binding to the notmuch
email library.")
    (license license:gpl3+)))

(define-public go-github-com-emersion-go-message
  (package
    (name "go-github-com-emersion-go-message")
    (version "0.16.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/go-message")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j5qdhsna28xcs843zsiccw700rld5hin466dl0n3a0ax1w13ay0"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-message"))
    (propagated-inputs (list go-golang-org-x-text
                             go-github-com-emersion-go-textwrapper))
    (home-page "https://github.com/emersion/go-message")
    (synopsis "Internet messages and MIME for Go")
    (description
     "The message package implements the Internet Message Format and Multipurpose
Internet Mail Extensions in Go.")
    (license license:expat)))

(define-public go-github-com-jaytaylor-html2text
  (package
    (name "go-github-com-jaytaylor-html2text")
    (version "0.0.0-20211105163654-bc68cce691ba")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jaytaylor/html2text")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12ckgkp8xqgp0fh6019nwp4ssg2k1rv1a67cpk37ian4q5zrvppm"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/jaytaylor/html2text"
           ;; Tests are broken: <https://github.com/jaytaylor/html2text/issues/53>
           #:tests? #f))
    (propagated-inputs (list go-golang-org-x-net
                             go-github-com-olekukonko-tablewriter
                             go-github-com-ssor-bom))
    (home-page "https://github.com/jaytaylor/html2text")
    (synopsis "Convert HTML emails to text")
    (description
     "The html2text package converts HTML emails to plain text, allowing
text-only mail clients to display them.")
    (license license:expat)))

(define-public go-github-com-jhillyerd-enmime
  (package
    (name "go-github-com-jhillyerd-enmime")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jhillyerd/enmime")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03pir9wq9ha2i2ifj819yv5i0lvrgdn904ksbzgc3k8bqc497ydn"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/jhillyerd/enmime"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs (list
                        go-github-com-cention-sany-utf7
                        go-github-com-go-test-deep
                        go-github-com-gogs-chardet
                        go-github-com-jaytaylor-html2text
                        go-github-com-pkg-errors
                        go-golang-org-x-text))
    (home-page "https://github.com/jhillyerd/enmime")
    (synopsis "MIME encoder and decoder for Go")
    (description
     "The enmime package implements a MIME encoding and decoding
library geared towards parsing MIME encoded emails.")
    (license license:expat)))

(define-public go-github-com-gatherstars-com-jwz
  (package
    (name "go-github-com-gatherstars-com-jwz")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gatherstars-com/jwz")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zxg2vmka80m1vnlb1v1gdlrwnkpakcmwi1hxpl8jjjiyd4z2j2i"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/gatherstars-com/jwz"))
    (propagated-inputs (list go-github-com-rivo-tview
                             go-github-com-jhillyerd-enmime
                             go-github-com-gdamore-tcell-v2))
    (home-page "https://github.com/gatherstars-com/jwz")
    (synopsis "Implementation in Go of the email threading algorithm
originally invented for Netscape Mail")
    (description
     "The jwz package provides an implementation of the email threading
algorithm originally designed for use in Netscape Mail 2.0 for Go.")
    (license license:asl2.0)))

(define-public go-github-com-creack-pty
  (package
    (name "go-github-com-creack-pty")
    (version "1.1.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/creack/pty")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qqhcgfym0napz8damj7dhfw28g2qn2f5h3lr93i0sxawq926yzc"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/creack/pty"
           #:modules '((ice-9 popen)
                       (ice-9 textual-ports)
                       (guix build go-build-system)
                       (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'regenerate-types
                 (lambda* (#:key import-path #:allow-other-keys)
                   ;; Generated files are included (ztypes_*). We need to remake
                   ;; them with Cgo.
                   (with-directory-excursion (string-append "src/" import-path)
                     (let* ((go-arch
                             #$(car (go-target
                                     (or (%current-target-system)
                                         (nix-system->gnu-triplet (%current-system))))))
                            (file (string-append "ztypes_" go-arch ".go"))
                            (pipe (open-input-pipe "go tool cgo -godefs types.go"))
                            (text (get-string-all pipe)))
                       (close-pipe pipe)
                       (for-each delete-file
                         (find-files (getcwd) (file-name-predicate
                                               "ztypes_[a-zA-Z0-9_]+.go")))
                       (call-with-output-file file
                         (lambda (port)
                           (display text port))))))))))
    (home-page "https://github.com/creack/pty")
    (synopsis "Pseudoterminal handling in Go")
    (description
     "The pty package provides functions for working with Unix pseudoterminals.")
    (license license:expat)))

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

(define-public go-github-com-rylans-getlang
  (package
    (name "go-github-com-rylans-getlang")
    (version "0.0.0-20201227074721-9e7f44ff8aa0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rylans/getlang")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yf698h21j88d7d9wkzq69cfd7vs1mfp96nhb83lx6hhh7rfvb92"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/rylans/getlang"))
    (propagated-inputs
     (list go-golang-org-x-text))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/rylans/getlang")
    (synopsis "Natural language detection package in pure Go")
    (description
     "This package provides fast natural language detection in Go.")
    (license license:expat)))

(define-public go-github-com-kyoh86-xdg
  (package
    (name "go-github-com-kyoh86-xdg")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kyoh86/xdg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a5nz53fdz1c2qvwlf2dpjdd72nxri95i6q4b07c37kiipgaxncn"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/kyoh86/xdg"))
    (home-page "https://github.com/kyoh86/xdg")
    (synopsis "XDG base directories for Go")
    (description
     "The xdg package provides lightweight helper functions in Go to get
config, data and cache directories according to the XDG Base Directory
Specification.")
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
      #:import-path "github.com/ssgelm/cookiejarparser"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-embed-x-net
            (lambda _
              (delete-file-recursively "src/golang.org/x/net/publicsuffix/data")
              (copy-recursively
               #$(file-append (this-package-input "go-golang-org-x-net")
                              "/src/golang.org/x/net/publicsuffix/data")
               "src/golang.org/x/net/publicsuffix/data"))))))
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

(define-public go-github-com-gogs-chardet
  (package
    (name "go-github-com-gogs-chardet")
    (version "0.0.0-20211120154057-b7413eaefb8f")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gogs/chardet")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12j8q5wc9m4n51v2j2m40nahqdl9bh3hzpdp26clzq91kc2amiz0"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/gogs/chardet"))
    (home-page "https://github.com/gogs/chardet")
    (synopsis "Character set detection for Go")
    (description
     "The chardet package ports character set detection from
ICU to Go.")
    (license license:expat)))

(define-public go-github-com-niemeyer-pretty
  (package
    (name "go-github-com-niemeyer-pretty")
    (version "0.0.0-20200227124842-a10e7caefd8e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/niemeyer/pretty")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jmazh4xzaa3v6g46hz60q2z7nmqs9l9cxdzmmscn3kbcs2znq4v"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/niemeyer/pretty"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'int-to-string-with-rune
                 (lambda* (#:key import-path #:allow-other-keys)
                   (with-directory-excursion (string-append "src/" import-path)
                     (substitute* "formatter.go"
                       (("s \\+= string\\(i\\)")
                        "s += string(rune(i))"))))))))
    (propagated-inputs (list go-github-com-kr-text))
    (home-page "https://github.com/niemeyer/pretty")
    (synopsis "Pretty-print Go values")
    (description
     "The pretty package provides pretty-printing for Go values.  This is
useful during debugging, to avoid wrapping long output lines in the
terminal.")
    (license license:expat)))

(define-public go-github-com-arran4-golang-ical
  (package
    (name "go-github-com-arran4-golang-ical")
    (version "0.0.0-20220517104411-fd89fefb0182")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/arran4/golang-ical")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bxs0b5yg26liiifc0cc41l307r0wc93hp8iygv8dgpc60yzncaw"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/arran4/golang-ical"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-gopkg-in-check-v1
                             go-github-com-stretchr-testify
                             go-github-com-niemeyer-pretty
                             go-github-com-kr-text
                             go-github-com-davecgh-go-spew))
    (home-page "https://github.com/arran4/golang-ical")
    (synopsis "Handle iCalenders in Go")
    (description
     "The @code{ical} package provides an ICS/iCalender parser and
serialiser for Go.")
    (license license:asl2.0)))

(define-public go-github-com-lithammer-fuzzysearch
  (package
    (name "go-github-com-lithammer-fuzzysearch")
    (version "1.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lithammer/fuzzysearch")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b036sm42cf64diwlqhx24vxy6g5afrmfbdfyqhpg8zrii3lpwns"))))
    (build-system go-build-system)
    (propagated-inputs (list go-golang-org-x-text))
    (arguments
     (list #:import-path "github.com/lithammer/fuzzysearch"
           #:tests? #f ; Source-only package.
           #:phases
           #~(modify-phases %standard-phases
               ;; Source-only package.
               (delete 'build))))
    (home-page "https://github.com/lithammer/fuzzysearch")
    (synopsis "Tiny and fast fuzzy search in Go")
    (description
     "A speedy fuzzy matching package for Go inspired by the JavaScript
library bevacqua/fuzzysearch.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
