;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Benjamin <benjamin@uvy.fr>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Fries <fries1234@protonmail.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2024 Greg Hogan <code@greghogan.com>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
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

(define-module (gnu packages golang-check)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz))

;;; Commentary:
;;;
;;; Golang packages providing tools to unit-test, mock, assert, and lint
;;; processes for the Golang itself. They may provide executables and
;;; libraries, for which there are marked sections.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-github-com-alecthomas-assert-v2
  (package
    (name "go-github-com-alecthomas-assert-v2")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ai26ncfcwzg47rqaigs5v1fzfz6i8p11ki75ni5429xkjs77riz"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.18
           #:import-path "github.com/alecthomas/assert/v2"))
    (propagated-inputs
     (list go-github-com-alecthomas-repr
           go-github-com-hexops-gotextdiff))
    (home-page "https://github.com/alecthomas/assert/")
    (synopsis "Go assertion library")
    (description "Assertion library that:
@itemize
@item makes spotting differences in equality much easier
@item uses repr and diffmatchpatch to display structural differences in colour
@item aborts tests on first assertion failure
@end itemize")
    (license license:expat)))

(define-public go-github-com-cheekybits-is
  (let ((commit "68e9c0620927fb5427fda3708222d0edee89eae9")
        (revision "0"))
    (package
      (name "go-github-com-cheekybits-is")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cheekybits/is")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1mkbyzhwq3rby832ikq00nxv3jnckxsm3949wkxd8ya9js2jmg4d"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/cheekybits/is"))
      (home-page "https://github.com/cheekybits/is")
      (synopsis "Mini testing helper for Go")
      (description "A mini testing helper for Go.

@itemize
@item It has a simple interface (@command{is.OK} and @command{is.Equal}).
@item It plugs into existing Go toolchain (uses @command{testing.T}).
@item It's obvious for newcomers.
@item It also gives you @command{is.Panic} and @command{is.PanicWith} helpers
- because testing panics is ugly.
@end itemize\n")
      (license license:expat))))

(define-public go-github-com-chzyer-test
  (package
    (name "go-github-com-chzyer-test")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chzyer/test")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1axdlcnx2qjsn5wsr2pr1m0w0a8k4nk5kkrngh742fgh81vzzy8s"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests relating to a in-memory disk fail due to a Segfault see
      ;; <https://github.com/chzyer/test/issues/4>.
      #:tests? #f
      #:import-path "github.com/chzyer/test"))
    (propagated-inputs
     (list go-github-com-chzyer-logex))
    (home-page "https://github.com/chzyer/test")
    (synopsis "Testing library for Go")
    ;; Description is not provided, see
    ;; <https://github.com/chzyer/test/issues/3>.
    (description
     "A testing library for Go programs.")
    (license license:expat)))

(define-public go-github-com-davecgh-go-spew
  (package
    (name "go-github-com-davecgh-go-spew")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/davecgh/go-spew")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hka6hmyvp701adzag2g26cxdj47g21x6jz4sc6jjz1mn59d474y"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/davecgh/go-spew"
       #:import-path "github.com/davecgh/go-spew/spew"))
    (home-page "https://github.com/davecgh/go-spew")
    (synopsis "Deep pretty printer for Go data structures to aid in debugging")
    (description "Package @command{spew} implements a deep pretty printer
for Go data structures to aid in debugging.

A quick overview of the additional features spew provides over the built-in
printing facilities for Go data types are as follows:

@itemize
@item Pointers are dereferenced and followed.
@item Circular data structures are detected and handled properly.
@item Custom Stringer/error interfaces are optionally invoked, including on
unexported types.
@item Custom types which only implement the Stringer/error interfaces via a
pointer receiver are optionally invoked when passing non-pointer variables.
@item Byte arrays and slices are dumped like the hexdump -C command which
includes offsets, byte values in hex, and ASCII output (only when using Dump
style).
@end itemize")
    (license license:isc)))

(define-public go-github-com-felixge-fgprof
  (package
    (name "go-github-com-felixge-fgprof")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/felixge/fgprof")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00h4kphvmbcdgad0wmqbaclc4a1pipdb55ay41mwh6cnkdjjvhp0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/felixge/fgprof"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-pprof))
    (home-page "https://github.com/felixge/fgprof")
    (synopsis "Sampling profiler for Golang")
    (description
     "@code{fgprof} is a sampling Go profiler providing analyze On-CPU as well
as @url{http://www.brendangregg.com/offcpuanalysis.html, Off-CPU} (e.g. I/O)
time together.

Go's builtin sampling CPU profiler can only show On-CPU time, but it's better
than fgprof at that.  Go also includes tracing profilers that can analyze I/O,
but they can't be combined with the CPU profiler.

fgprof is designed for analyzing applications with mixed I/O and CPU
workloads.  This kind of profiling is also known as wall-clock profiling.")
    (license license:expat)))

(define-public go-github-com-frankban-quicktest
  (package
    (name "go-github-com-frankban-quicktest")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/frankban/quicktest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0b1b44b2hli2p969gqz30z8v9z6ahlklpqzi17nwk1lsjz9yv938"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/frankban/quicktest"))
    (propagated-inputs
     (list go-github-com-google-go-cmp-cmp go-github-com-kr-pretty))
    (home-page "https://github.com/frankban/quicktest")
    (synopsis "Quick helpers for testing Go applications")
    (description
     "Package quicktest provides a collection of Go helpers for writing
tests.")
    (license license:expat)))

(define-public go-github-com-go-test-deep
  (package
    (name "go-github-com-go-test-deep")
    (version "1.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-test/deep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mmw2w3by7y24jjpjwmf2gfl08c65jihn3si9m0sswmagmdsk8q0"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/go-test/deep"))
    (home-page "https://github.com/go-test/deep")
    (synopsis "Human-friendly deep variable equality in Go")
    (description
     "The deep package provides the deep.Equal function which is like
reflect.DeepEqual but returns a list of differences.  This is helpful
when comparing complex types like structures and maps.")
    (license license:expat)))

(define-public go-github-com-golangplus-testing
  (package
    (name "go-github-com-golangplus-testing")
    (version "1.0.0")
    (home-page "https://github.com/golangplus/testing")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a29m4zplf9m14k74lrb55dids2l17vx28sv0g3y3qcv1xygksiv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golangplus/testing"))
    (propagated-inputs
     (list go-github-com-golangplus-fmt))
    (synopsis "Additions to Go's standard testing package")
    (description "This package provides additions to Go's stdlib testing.")
    (license license:bsd-3)))

(define-public go-github-com-google-go-cmdtest
  (let ((commit "55ab3332a786118933ddf71544aae14951ba9bc5")
        (revision "0"))
    (package
      (name "go-github-com-google-go-cmdtest")
      (version (git-version "0.4.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/go-cmdtest")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "10kswvbdwissjb5mr0ys4b3ppxkxlpklqg7cr2z7rv21g2vwczbl"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/google/go-cmdtest"))
      (propagated-inputs
       (list go-github-com-google-renameio go-github-com-google-go-cmp-cmp))
      (home-page "https://github.com/google/go-cmdtest")
      (synopsis "Testing for your CLI")
      (description
       "The cmdtest package simplifies testing of command-line interfaces.  It
provides a simple, cross-platform, shell-like language to express command
execution.  It can compare actual output with the expected output, and can
also update a file with new \"golden\" output that is deemed correct.")
      (license license:asl2.0))))

(define-public go-github-com-google-gofuzz
  (let ((commit "fd52762d25a41827db7ef64c43756fd4b9f7e382")
        (revision "0"))
    (package
      (name "go-github-com-google-gofuzz")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/gofuzz")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yxmmr73h0lq7ryf3q9a7pcm2x5xrg4d5bxkq8n5pxwxwyq26kw8"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/google/gofuzz"))
      (home-page "https://github.com/google/gofuzz")
      (synopsis "Fuzz testing library for Go")
      (description "Gofuzz is a library for populationg Go objects with random
values for the purpose of fuzz testing.")
      (license license:asl2.0))))

;; XXX: Placing to (gnu package profiling) creates some failing cycles.
(define-public go-github-com-google-pprof
  (package
    (name "go-github-com-google-pprof")
    (version "0.0.0-20240402174815-29b9bb013b0f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/pprof")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09l37q5dql0q0zj8amlnrynajfvp1vrp846q5vgiwsbwz9b78f18"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/pprof"
      #:phases
      #~(modify-phases %standard-phases
          ;; To make this package smaller to use as a library where just
          ;; source is required.
          (delete 'build))))
    (propagated-inputs
     (list go-github-com-chzyer-readline
           go-github-com-ianlancetaylor-demangle
           go-golang-org-x-sys))
    (home-page "https://github.com/google/pprof")
    (synopsis "Visualization and analysis of profiling data")
    (description
     "@code{pprof} is a tool for visualization and analysis of profiling data.

It reads a collection of profiling samples in profile.proto format and
generates reports to visualize and help analyze the data.  It can generate
both text and graphical reports (through the use of the dot visualization
package).")
    (license (list
              ;; For go code: LICENSE
              license:asl2.0
              ;; For svgpan: third_party/svgpan/LICENSE
              ;; original source <https://code.google.com/archive/p/svgpan/>.
              license:bsd-3
              ;; For d3flamegraph: third_party/d3flamegraph/D3_LICENSE
              ;;
              ;; Bundle of d3-flame-graph and d3-selection JavaScript library
              ;; (NPM) <https://www.npmjs.com/package/d3-flame-graph> and
              ;; <https://www.npmjs.com/package/d3-selection>.
              license:asl2.0 ; for bundle and d3-flame-graph
              license:isc    ; for d3-selection
              ))))

(define-public go-github-com-hexops-gotextdiff
  (package
    (name "go-github-com-hexops-gotextdiff")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hexops/gotextdiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vgq6w0cfhr76qlczgm5khsj1wnjkva0vhkh3qspaa1nkfw3jny1"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/hexops/gotextdiff"))
    (home-page "https://github.com/hexops/gotextdiff")
    (synopsis "Unified text diffing in Go")
    (description
     "This package provides a library to generate unified diffs.")
    (license license:bsd-3)))

(define-public go-github-com-jacobsa-oglematchers
  (let ((commit "141901ea67cd4769c6800aa7bfdfc558fa22bda5")
        (revision "0"))
    (package
      (name "go-github-com-jacobsa-oglematchers")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jacobsa/oglematchers")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09ff5x6vbhd9zl1z4yzyk573ifh16rry38q1rx986kbz4hqkmniq"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/jacobsa/oglematchers"
         ;; break loop with with go-github-com-jacobsa-ogletest
         #:tests? #f))
      (home-page "https://github.com/jacobsa/oglematchers")
      (synopsis "Matchers for Go testing framework")
      (description
       "Package oglematchers provides a set of matchers useful in a testing or mocking
framework.  These matchers are inspired by and mostly compatible with Google
Test for C++ and Google JS Test.")
      (license license:asl2.0))))

(define-public go-github-com-jacobsa-oglemock
  (let ((commit "e94d794d06ffc6de42cb19d0dab3c219efdd6dcf")
        (revision "0"))
    (package
      (name "go-github-com-jacobsa-oglemock")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jacobsa/oglemock")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14yxf8ykwdwkcccksl6741xgzcf8qykyi58kp4maxpgscqhdl8rq"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/jacobsa/oglemock"
        ;; break loop with with go-github-com-jacobsa-ogletest
        #:tests? #f))
      (native-inputs
       (list go-github-com-jacobsa-oglematchers))
      (home-page "https://github.com/jacobsa/oglemock")
      (synopsis "Mocking framework for unit tests")
      (description
       "Package oglemock provides a mocking framework for unit tests.")
      (license license:asl2.0))))

(define-public go-github-com-jacobsa-ogletest
  (let ((commit "80d50a735a1108a2aeb7abc4a988d183f20c5292")
        (revision "0"))
    (package
      (name "go-github-com-jacobsa-ogletest")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jacobsa/ogletest")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1lbwbxzr75g65q07ry5k4kglxqs3ym7xkvqznzm55rm3qk76v83r"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/jacobsa/ogletest"
         ;; These tests should be made working
         #:tests? #f))
      (native-inputs
       (list go-github-com-jacobsa-oglematchers
             go-github-com-jacobsa-oglemock
             go-github-com-jacobsa-reqtrace
             go-golang-org-x-net))
      (home-page "https://github.com/jacobsa/ogletest")
      (synopsis "Expressive unit tests")
      (description
       "Package ogletest provides a framework for writing expressive unit tests.  It
integrates with the builtin testing package, so it works with the gotest
command.  Unlike the testing package which offers only basic capabilities for
signalling failures, it offers ways to express expectations and get nice failure
messages automatically.")
      (license license:asl2.0))))

(define-public go-github-com-jbenet-go-cienv
  (package
    (name "go-github-com-jbenet-go-cienv")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbenet/go-cienv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qyfjvr8n5chpb5zi6r9cf0danrwds3k5lbf7vp7ygcl6wnm0vmv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jbenet/go-cienv"))
    (home-page "https://github.com/jbenet/go-cienv")
    (synopsis "CI system environment variables")
    (description
     "Package @code{cienv} implements some helper functions to use during tests.
Many times certain facilities are not available, or tests must run
differently.")
    (license license:expat)))

(define-public go-github-com-onsi-ginkgo
  (package
    (name "go-github-com-onsi-ginkgo")
    (version "1.16.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onsi/ginkgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hh6n7q92y0ai8k6rj2yzw6wwxikhyiyk4j92zgvf1zad0gmqqmz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/onsi/ginkgo"))
    (propagated-inputs
     (list go-github-com-go-task-slim-sprig
           go-github-com-nxadm-tail
           go-github-com-onsi-gomega
           go-golang-org-x-sys
           go-golang-org-x-tools))
    (home-page "https://github.com/onsi/ginkgo")
    (synopsis "BDD-style testing framework for Go")
    (description
     "Ginkgo is a Behaviour-Driven Development testing framework for Go.  It
builds on top of Go's builtin @code{testing} library and is complemented by the
Gomega matcher library.")
    (license license:expat)))

(define-public go-github-com-onsi-ginkgo-v2
  (package
    (inherit go-github-com-onsi-ginkgo)
    (name "go-github-com-onsi-ginkgo-v2")
    (version "2.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onsi/ginkgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "089x6pz5563ldbxiwaqvd2g4dqfzlr25dflmas3gfq51ibwzh4vz"))))
    (arguments
     (list
      #:go go-1.20
      #:import-path "github.com/onsi/ginkgo/v2"))
    (propagated-inputs
     (list go-github-com-go-logr-logr
           ;; go-github-com-google-pprof ; not packed yet in Guix, for profiling
           go-github-com-onsi-gomega
           go-github-com-go-task-slim-sprig
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-tools))))

(define-public go-github-com-onsi-gomega
  (package
    (name "go-github-com-onsi-gomega")
    (version "1.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onsi/gomega")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "092phwk97sk4sv0nbx5pfhqs6x3x1lnrjwyda1m6b6zwrfmq5c6i"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/onsi/gomega"
           ;; Unless we disable the tests, we have a circular dependency on
           ;; ginkgo/v2.
           #:tests? #f))
    (propagated-inputs
     (list go-github-com-golang-protobuf-proto
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-text
           go-google-golang-org-protobuf
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/onsi/gomega")
    (synopsis "Matcher library for Ginkgo")
    (description
     "Gomega is the preferred matcher/assertion library for the Ginkgo test
framework.")
    (license license:expat)))

(define-public go-github-com-pkg-profile
  (package
    (name "go-github-com-pkg-profile")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pkg/profile")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ifr9gnycjwh7dbvsb5vgs9kzlr548cb4m45zvl8i8lgd3qhppy1"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Unit tests failing, see
      ;; <https://github.com/pkg/profile/issues/68>.
      #:tests? #f
      #:import-path "github.com/pkg/profile"
      #:phases
      #~(modify-phases %standard-phases
          ;; profile drops a cpu.pprof file inside its source directory
          ;; after tests which makes it unreproducible so we remove it.
          (add-after 'check 'delete-test-file
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (delete-file (string-append "src/" import-path
                                            "/cpu.pprof"))))))))
    (propagated-inputs
     (list go-github-com-felixge-fgprof))
    (home-page "https://github.com/pkg/profile")
    (synopsis "Simple profiling for Go")
    (description
     "Profile provides a simple way to manage runtime/pprof profiling of your
Go application.")
    (license license:bsd-2)))

(define-public go-github-com-prashantv-gostub
  (package
    (name "go-github-com-prashantv-gostub")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prashantv/gostub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "035xf5w4fqlicdbbjcflsqflc0z5gmrn6wr7q41xwqfwfpraf9ah"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/prashantv/gostub"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/prashantv/gostub")
    (synopsis "Stubbing library for Go")
    (description
     "Package gostub is used for stubbing variables in tests, and resetting the
original value once the test has been run.")
    (license license:expat)))

(define-public go-github-com-stretchr-testify
  (package
    (name "go-github-com-stretchr-testify")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stretchr/testify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ixgjsvafr3513pz3r6pmgk074s2dxkll0dadvl25gkf30rkmh10"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/stretchr/testify"))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-pmezard-go-difflib
           go-github-com-stretchr-objx
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/stretchr/testify")
    (synopsis "Go helper library for tests and invariant checking")
    (description "This package provide many tools for testifying that your
code will behave as you intend.

Features include:
@itemize
@item Easy assertions
@item Mocking
@item HTTP response trapping
@item Testing suite interfaces and functions.
@end itemize")
    (license license:expat)))

(define-public go-github-com-stretchr-testify-bootstrap
  (hidden-package
    (package
      (inherit go-github-com-stretchr-testify)
      (arguments
       '(#:import-path "github.com/stretchr/testify"
         #:tests? #f
         #:phases (modify-phases %standard-phases
                    (delete 'build))))
      (propagated-inputs
       (list go-gopkg-in-yaml-v3)))))

(define-public go-github-com-stretchr-testify-next
  (package
    (inherit go-github-com-stretchr-testify)
    (name "go-github-com-stretchr-testify")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stretchr/testify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12cnhk96h8b3ddlb7jfvwwavzc0j1c2iva92pszl9rv6r571ckzg"))))))

(define-public go-github-com-tdewolff-test
  (package
    (name "go-github-com-tdewolff-test")
    (version "1.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/test")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10myz3zdkqmx37cvj507h7l2ncb0rq9shqvz9ggq1swijbsvazff"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/tdewolff/test"))
    (home-page "https://github.com/tdewolff/test")
    (synopsis "Go test helper functions")
    (description
     "This package implements a few functions that are useful for io testing,
such as readers and writers that fail after N consecutive reads/writes.")
    (license license:expat)))

(define-public go-github.com-smartystreets-assertions
  (package
    (name "go-github.com-smartystreets-assertions")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smartystreets/assertions")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32 "0flf3fb6fsw3bk1viva0fzrzw87djaj1mqvrx2gzg1ssn7xzfrzr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/smartystreets/assertions"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (unless
                  ;; The tests fail when run with gccgo.
                  (false-if-exception (search-input-file inputs "/bin/gccgo"))
                (apply (assoc-ref %standard-phases 'check) args)))))))
    (native-inputs
     (list go-github.com-smartystreets-gunit))
    (home-page "https://github.com/smartystreets/assertions")
    (synopsis "Assertions for testing with Go")
    (description "The @code{assertions} package provides convenient assertion
functions for writing tests in Go.")
    (license license:expat)))

(define-public go-github.com-smartystreets-goconvey
  (package
    (name "go-github.com-smartystreets-goconvey")
    (version "1.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smartystreets/goconvey")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ph18rkl3ns3fgin5i4j54w5a69grrmf3apcsmnpdn1wlrbs3dxh"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/smartystreets/goconvey"))
    (propagated-inputs
     (list go-github.com-jtolds-gls go-github.com-smartystreets-assertions))
    (home-page "https://github.com/smartystreets/goconvey")
    (synopsis "Go testing tool with both a web and terminal user interface")
    (description "GoConvey is a testing tool for Go. It integrates with go
test, can show test coverage and has a web user interface that will refresh
automatically.")
    (license license:expat)))

(define-public go-github.com-smartystreets-gunit
  (package
    (name "go-github.com-smartystreets-gunit")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smartystreets/gunit")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00m4zg0kdj49mnpmf9klb44ba71p966xsk6zknrzqgfc8119f35z"))))
    (build-system go-build-system)
    (arguments
     '(;; TODO: This package depends on go-github.com-smartystreets-assertions
       ;; for running the tests, but go-github.com-smartystreets-assertions
       ;; depends on this package, so break this loop by not running the tests
       ;; for this package.
       #:tests? #f
       #:import-path "github.com/smartystreets/gunit"))
    (home-page "https://github.com/smartystreets/gunit")
    (synopsis "Testing tool for Go, in the style of xUnit")
    (description "@code{gunit} allows the test author to use a struct as the
scope for a group of related test cases, in the style of xUnit fixtures.  This
makes extraction of setup/teardown behavior (as well as invoking the system
under test) much simpler.")
    (license license:expat)))

(define-public go-golang-org-sql-mock
  (let ((commit "e98392b8111b45f8126e00af035a0dd95dc12e8b")
        (version "1.3.3")
        (revision "1"))
    (package
      (name "go-golang-org-sql-mock")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DATA-DOG/go-sqlmock")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "033vv29g2wf6fd757ajfmha30bqin3b07377037zkl051mk6mghs"))
         (modules '((guix build utils)))
         (snippet '(delete-file-recursively "examples"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/DATA-DOG/go-sqlmock"))
      (synopsis "Mock library implementing @code{sql/driver}")
      (description "This library simulates SQL-driver behavior in tests
without requiring a real database connection.")
      (home-page "https://github.com/DATA-DOG/go-sqlmock")
      (license license:expat))))

(define-public go-golang-org-x-lint
  (let ((commit "83fdc39ff7b56453e3793356bcff3070b9b96445")
        (revision "0"))
    (package
      (name "go-golang-org-x-lint")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/lint")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ms3rs5hvpnm9bxbr5f9743i7hn2bbmqdmvzxq6nmi0f24ypv1l3"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/lint"
         #:tests? #f)) ;; TODO: Fix tests
      (propagated-inputs
       (list go-golang-org-x-tools))
      (home-page "https://golang.org/x/lint")
      (synopsis "Linter for Go source code")
      (description
       "This is a linter for Go source code.  Unlike gofmt, it doesn't
reformat the source code, it only prints out style mistakes.")
      (license license:bsd-3))))

(define-public go-gopkg-in-check-v1
  (package
    (name "go-gopkg-in-check-v1")
    (version "1.0.0-20201130134442-10cb98267c6c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-check/check")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jwxndf8rsyx0fgrp47d99rp55yzssmryb92jfj3yf7zd8rjjljn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/check.v1"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (unless
                  ;; The tests fail when run with gccgo.
                  (false-if-exception (search-input-file inputs "/bin/gccgo"))
                (apply (assoc-ref %standard-phases 'check) args)))))))
    (propagated-inputs
     (list go-github-com-kr-pretty))
    (home-page "https://gopkg.in/check.v1")
    (synopsis "Test framework for the Go language")
    (description "This package provides a test library for the Go language.")
    (license license:bsd-2)))

(define-public go-gopkg-in-go-playground-assert-v1
  (package
    (name "go-gopkg-in-go-playground-assert-v1")
    (version "1.2.1")
    (home-page "https://github.com/go-playground/assert")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h4amgykpa0djwi619llr3g55p75ia0mi184h9s5zdl8l4rhn9pm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/go-playground/assert.v1"))
    (synopsis "Basic assertion library used alongside native Go testing")
    (description
     "This package provides basic assertions along with building blocks for
custom assertions to be used alongside native Go testing.")
    (license license:expat)))

(define-public go-github-com-go-playground-assert-v2
  (package
    (inherit go-gopkg-in-go-playground-assert-v1)
    (name "go-github-com-go-playground-assert-v2")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-playground/assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13mb07dxhcy9ydqbracnrpfj682g6sazjpm56yrlbn2jc1yfy44c"))))
    (arguments
     (list #:import-path "github.com/go-playground/assert/v2"))))

(define-public go-go-uber-org-goleak
  (package
    (name "go-go-uber-org-goleak")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/goleak")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1lpqw7ygffak8qki9i4vw8b99l25l8jrw8iwcplqsclk6fzkl24p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.uber.org/goleak"))
    (native-inputs
     (list go-github-com-stretchr-testify-next))
    (home-page "https://pkg.go.dev/go.uber.org/goleak")
    (synopsis "Goroutine leak detector")
    (description
     "Go package to verify that there are no unexpected goroutines running at
the end of a test.")
    (license license:expat)))

(define-public go-honnef-co-go-tools
  (package
    (name "go-honnef-co-go-tools")
    (version "0.4.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dominikh/go-tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qikkhanmc0vrpgvs0c44y853n88m8qqsk08zry6mvm8f41pgjhi"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "honnef.co/go/tools"
       #:tests? #f
       ;; Source-only package
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-golang-org-x-exp
           go-golang-org-x-mod
           go-golang-org-x-tools))
    (home-page "https://staticcheck.dev/")
    (synopsis "Staticcheck advanced Go linter library")
    (description
     "This package provides the Go source code for the @code{go-staticcheck}
advanced Go linter.")
    (license license:expat)))

(define-public go-pgregory-net-rapid
  (package
    (name "go-pgregory-net-rapid")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flyingmutant/rapid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1alyhcbdq6113sfymx7xxmxpcbgvkaipsn15cgjrcqrx8ks1hm5i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.18
      #:import-path "pgregory.net/rapid"))
    (home-page "https://pgregory.net/rapid/")
    (synopsis "Go property-based testing library")
    (description
     "@code{Rapid} is a Go library for property-based testing.

Rapid checks that properties you define hold for a large number of automatically
generated test cases.  If a failure is found, rapid automatically minimizes the
failing test case before presenting it.

Features:
@itemize
@item imperative Go API with type-safe data generation using generics
@item data generation biased to explore \"small\" values and edge cases more
thoroughly
@item fully automatic minimization of failing test cases
@item persistence and automatic re-running of minimized failing test cases
@item support for state machine (\"stateful\" or \"model-based\") testing
@item no dependencies outside the Go standard library
@end itemize")
    (license license:mpl2.0)))

;;;
;;; Executables:
;;;

(define-public go-keyify
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-keyify")
    (arguments
     `(#:go ,go-1.20
       #:import-path "honnef.co/go/tools/cmd/keyify"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Transform an unkeyed struct literal into a keyed one in Go")
    (description "This package turns unkeyed struct literals (@code{T{1, 2,
3}}) into keyed ones (@code{T{A: 1, B: 2, C: 3}}) in Go.")))

(define-public go-pprof
  (package
    (inherit go-github-com-google-pprof)
    (name "go-pprof")
    (arguments
     (list
      #:install-source? #f
      #:go go-1.19
      #:import-path "github.com/google/pprof"))
    (description
     (string-append (package-description go-github-com-google-pprof)
                    "  This package provides an command line interface (CLI)
tool."))))

(define-public go-staticcheck
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-staticcheck")
    (arguments
     `(#:go ,go-1.20
       #:import-path "honnef.co/go/tools/cmd/staticcheck"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Staticcheck advanced Go linter")
    (description
     "Staticcheck is a state of the art linter for the Go programming language.
Using static analysis, it finds bugs and performance issues, offers
simplifications, and enforces style rules.")))

(define-public go-structlayout
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-structlayout")
    (arguments
     `(#:go ,go-1.20
       #:import-path "honnef.co/go/tools/cmd/structlayout"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Display the layout (field sizes and padding) of structs in Go")
    (description "This package prints the layout of a struct in Go, which is
the byte offset and size of each field, respecting padding.  This information
is printed in human-readable form by default, or as JSON with the @code{-json}
flag.")))

(define-public go-structlayout-optimize
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-structlayout-optimize")
    (arguments
     `(#:go ,go-1.20
       #:import-path "honnef.co/go/tools/cmd/structlayout-optimize"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Reorder struct fields to minimize the amount of padding in Go")
    (description "This package reads @code{go-structlayout} JSON on stdin and
reorders fields to minimize the amount of padding.  It can emit JSON to feed
into @code{go-structlayout-pretty}.")))

(define-public go-structlayout-pretty
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-structlayout-pretty")
    (arguments
     `(#:go ,go-1.20
       #:import-path "honnef.co/go/tools/cmd/structlayout-pretty"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Format the output of go-structlayout with ASCII art in Go")
    (description "This package takes @code{go-structlayout}-like JSON and
prints an ASCII fraphic representing the memory layout.")))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
