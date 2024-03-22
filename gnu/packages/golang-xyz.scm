;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019-2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021 raingloom <raingloom@riseup.net>
;;; Copyright © 2021 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021, 2023, 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2022 Dominic Martinez <dom@dominicm.dev>
;;; Copyright © 2023 Benjamin <benjamin@uvy.fr>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Thomas Ieong <th.ieong@free.fr>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages golang-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages linux))

;;; Commentary:
;;;
;;; Nomad Golang modules (libraries) are welcome here.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-github-com-a8m-envsubst
  (package
    (name "go-github-com-a8m-envsubst")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a8m/envsubst")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mjs729g9nmalx25l4nn3p07amm4vsciqmdf0jbh2jwpy1zymz41"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/a8m/envsubst"))
    (home-page "https://github.com/a8m/envsubst")
    (synopsis "Environment variables substitution for Go")
    (description
     "This package provides a library for environment variables
substitution.")
    (license license:expat)))

(define-public go-github-com-alecthomas-chroma
  (package
    (name "go-github-com-alecthomas-chroma")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/chroma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hjzb61m5lzx95xss82wil9s8f9hbw1zb3jj73ljfwkq5lqk76zq"))
       (modules '((guix build utils)))
       ;; Delete git submodules and generated files by Hermit.
       (snippet '(delete-file-recursively "bin"))))
    (build-system go-build-system)
    ;; TODO: Build cmd/chroma and cmd/chromad commands.
    (arguments
     `(#:import-path "github.com/alecthomas/chroma"))
    (native-inputs
     (list go-github-com-dlclark-regexp2
           go-github-com-stretchr-testify))
    (home-page "https://github.com/alecthomas/chroma/")
    (synopsis "General purpose syntax highlighter in pure Go")
    (description
     "Chroma takes source code and other structured text and converts it into
syntax highlighted HTML, ANSI-coloured text, etc.")
    (license license:expat)))

(define-public go-github-com-alecthomas-chroma-v2
  (package
    (inherit go-github-com-alecthomas-chroma)
    (name "go-github-com-alecthomas-chroma-v2")
    (version "2.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/chroma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j9zz77ppi4r4ncnanzj84h7bsg0qdqrhgd5kkjiv09afm31jx83"))))
    (arguments
     (list #:go go-1.19
           #:import-path "github.com/alecthomas/chroma/v2"))
    (propagated-inputs
     (list go-github-com-dlclark-regexp2))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2
           go-github-com-alecthomas-repr))))

(define-public go-github-com-alecthomas-kingpin
  (package
    (name "go-github-com-alecthomas-kingpin")
    (version "2.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/kingpin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mndnv3hdngr3bxp7yxfd47cas4prv98sqw534mx7vp38gd88n5r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/kingpin"))
    (native-inputs
     (list go-github-com-alecthomas-template
           go-github-com-alecthomas-units
           go-github-com-stretchr-testify))
    (home-page "https://github.com/alecthomas/kingpin")
    (synopsis "Go library provides utilities for building command line interfaces")
    (description
     "Go library provides utilities for building command line interfaces.")
    (license license:expat)))

(define-public go-github-com-alecthomas-kingpin-v2
  (package
    (inherit go-github-com-alecthomas-kingpin)
    (name "go-github-com-alecthomas-kingpin-v2")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/kingpin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12xl62xzwq2h71hp1i0133403zhyqwsh95sr870fx18wmpqh8shf"))))
    (arguments
     (list
      #:import-path "github.com/alecthomas/kingpin/v2"))
    (propagated-inputs
     (list go-github-com-alecthomas-units
           go-github-com-xhit-go-str2duration-v2))
    (native-inputs
     (list go-github-com-stretchr-testify))))

(define-public go-github-com-alecthomas-participle-v2
  (package
    (name "go-github-com-alecthomas-participle-v2")
    (version "2.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/participle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k2vsd58rgwyylyn5zja6z6k1sg4m39g2fhd88lvja60ca51bh98"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.18
           #:import-path "github.com/alecthomas/participle/v2"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2))
    (home-page "https://github.com/alecthomas/participle")
    (synopsis "Parser library for Go")
    (description
     "This package provides a parser library for Golang which constructs
parsers from definitions in struct tags and parses directly into those
structs.  The approach is similar to how other marshallers work in Golang,
\"unmarshalling\" an instance of a grammar into a struct.")
    (license license:expat)))

(define-public go-github-com-alecthomas-template
  ;; No release, see <https://github.com/alecthomas/template/issues/7>.
  (let ((commit "a0175ee3bccc567396460bf5acd36800cb10c49c")
        (revision "0"))
    (package
      (name "go-github-com-alecthomas-template")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alecthomas/template")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qjgvvh26vk1cyfq9fadyhfgdj36f1iapbmr5xp6zqipldz8ffxj"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/alecthomas/template"))
      (home-page "https://github.com/alecthomas/template")
      (synopsis "Fork of Go's text/template adding newline elision")
      (description
       "This is a fork of Go 1.4's text/template package with one addition: a
backslash immediately after a closing delimiter will delete all subsequent
newlines until a non-newline.")
      (license license:bsd-3))))

(define-public go-github-com-alecthomas-units
  ;; No release, see <https://github.com/alecthomas/units/issues/9>.
  (let ((commit "2efee857e7cfd4f3d0138cc3cbb1b4966962b93a")
        (revision "0"))
    (package
      (name "go-github-com-alecthomas-units")
      (version "0.0.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alecthomas/units")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1j65b91qb9sbrml9cpabfrcf07wmgzzghrl7809hjjhrmbzri5bl"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/alecthomas/units"))
      (native-inputs
       (list go-github-com-stretchr-testify))
      (home-page "https://github.com/alecthomas/units")
      (synopsis "Helpful unit multipliers and functions for Go")
      (description
       "This library provides unit multipliers and functions for Go.")
      (license license:expat))))

(define-public go-github-com-anmitsu-go-shlex
  (package
    (name "go-github-com-anmitsu-go-shlex")
    (version "0.0.0-20200514113438-38f4b401e2be")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/anmitsu/go-shlex")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "17iz68yzbnr7y4s493asbagbv79qq8hvl2pkxvm6bvdkgphj8w1g"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/anmitsu/go-shlex"))
    (home-page "https://github.com/anmitsu/go-shlex")
    (synopsis "Simple shell-like lexical analyzer for Go")
    (description "This package provides a simple lexical analyzer to parse
shell-like commands.")
    (license license:expat)))

(define-public go-github-com-armon-go-radix
  (package
    (name "go-github-com-armon-go-radix")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/armon/go-radix")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m1k0jz9gjfrk4m7hjm7p03qmviamfgxwm2ghakqxw3hdds8v503"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/armon/go-radix"))
    (home-page "https://github.com/armon/go-radix")
    (synopsis "Go implementation of Radix trees")
    (description "This package provides a single @code{Tree} implementation,
optimized for sparse nodes of
@url{http://en.wikipedia.org/wiki/Radix_tree,radix tree}.")
    (license license:expat)))

(define-public go-github-com-benbjohnson-clock
  (package
    (name "go-github-com-benbjohnson-clock")
    (version "1.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/benbjohnson/clock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p7n09pywqra21l981fbkma9vzsyf31pbvw6xg5r4hp8h8scf955"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/benbjohnson/clock"
       #:go ,go-1.21))
    (home-page "https://github.com/benbjohnson/clock")
    (synopsis "Small library for mocking time in Go")
    (description
     "@code{clock} is a small library for mocking time in Go.  It provides an
interface around the standard library's @code{time} package so that the application
can use the realtime clock while tests can use the mock clock.")
    (license license:expat)))

(define-public go-github-com-beorn7-perks-quantile
  (package
    (name "go-github-com-beorn7-perks-quantile")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/beorn7/perks")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17n4yygjxa6p499dj3yaqzfww2g7528165cl13haj97hlx94dgl7"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/beorn7/perks/quantile"
           #:unpack-path "github.com/beorn7/perks"))
    (home-page "https://github.com/beorn7/perks")
    (synopsis "Compute approximate quantiles over an unbounded data stream")
    (description
     "Perks contains the Go package @code{quantile} that computes
approximate quantiles over an unbounded data stream within low memory and CPU
bounds.")
    (license license:expat)))

(define-public go-github-com-bitly-go-hostpool
  (package
    (name "go-github-com-bitly-go-hostpool")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bitly/go-hostpool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iibj7dwymczw7cknrh6glc6sdpp4yap2plnyr8qphynwrzlz73w"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bitly/go-hostpool"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/bitly/go-hostpool")
    (synopsis "Pool among multiple hosts from Golang")
    (description
     "This package provides a Go package to intelligently and flexibly pool among
multiple hosts from your Go application.  Host selection can operate in round
robin or epsilon greedy mode, and unresponsive hosts are avoided.")
    (license license:expat)))

(define-public go-github-com-bitly-timer-metrics
  (package
    (name "go-github-com-bitly-timer-metrics")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bitly/timer_metrics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02fhx8hx8126m2cgxw9fm8q2401r7zfann8b5zy5yyark1sgkrb4"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bitly/timer_metrics"))
    (home-page "https://github.com/bitly/timer_metrics")
    (synopsis "Capture timings and enable periodic metrics every @var{n} events")
    (description "This package provides an efficient way to capture timing
information and periodically output metrics")
    (license license:expat)))

(define-public go-github-com-blang-semver
  (package
    (name "go-github-com-blang-semver")
    (version "3.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blang/semver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16s66zbfkn35msmxpkiwf5dv91kzw7yzxzkcv8ma44j7lbgzx5qk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/blang/semver"))
    (home-page "https://github.com/blang/semver")
    (synopsis "Semantic versioning library written in Go")
    (description
     "Semver is a library for Semantic versioning written in Go.")
    (license license:expat)))

(define-public go-github-com-blang-semver-v4
  (package
    (inherit go-github-com-blang-semver)
    (name "go-github-com-blang-semver-v4")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blang/semver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14h9ys4n4kx9cbj42lkdf4i5k3nkll6sd62jcvl7cs565v6fiknz"))))
    (arguments
     (list
      #:import-path "github.com/blang/semver/v4"
      #:unpack-path "github.com/blang/semver"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))))

(define-public go-github-com-bmizerany-perks-quantile
  (package
    (name "go-github-com-bmizerany-perks-quantile")
    (version "0.0.0-20230307044200-03f9df79da1e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bmizerany/perks")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f2a99v3618bz2mf61iwhdjm3xi1gam6v4apqgcrz71gj7ba9943"))))
    (build-system go-build-system)
    (arguments
     (list #:unpack-path "github.com/bmizerany/perks"
           #:import-path "github.com/bmizerany/perks/quantile"))
    (home-page "https://github.com/bmizerany/perks")
    (synopsis "Library for computing quantiles")
    (description
     "Perks contains the Go package @code{quantile} that computes approximate
quantiles over an unbounded data stream within low memory and CPU bounds.")
    (license license:bsd-2)))

(define-public go-github-com-burntsushi-toml
  (package
    (name "go-github-com-burntsushi-toml")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BurntSushi/toml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v9czq4hsyvdz7yx70y6sgq77wmrgfmn09r9cj4w85z38jqnamv7"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/BurntSushi/toml"))
    (home-page "https://github.com/BurntSushi/toml")
    (synopsis "Toml parser and encoder for Go")
    (description
     "This package is toml parser and encoder for Go.  The interface is
similar to Go's standard library @code{json} and @code{xml} package.")
    (license license:expat)))

(define-public go-github-com-cheggaaa-pb-v3
  (package
    (name "go-github-com-cheggaaa-pb-v3")
    (version "3.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cheggaaa/pb/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d701s2niy39r650d1phjw19h4l27b1yfc2ih6s31f56b3zzqspx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/cheggaaa/pb/v3"
       #:unpack-path "github.com/cheggaaa/pb"))
    (propagated-inputs
     (list go-github-com-fatih-color
           go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-runewidth
           go-github-com-vividcortex-ewma))
    (home-page "https://github.com/cheggaaa/pb/")
    (synopsis "Console progress bar for Go")
    (description
     "This package is a Go library that draws progress bars on the terminal.")
    (license license:bsd-3)))

(define-public go-github-com-coocood-freecache
  (package
    (name "go-github-com-coocood-freecache")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coocood/freecache")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iw0s07qy8g1lncwl524c524wh56djl0vn6i3bm91cnwzav7ihjl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/coocood/freecache"))
    (propagated-inputs (list go-github-com-cespare-xxhash))
    (home-page "https://github.com/coocood/freecache")
    (synopsis "Caching library for Go")
    (description
     "This library provides caching capabilities for Go with no garbage
collection overhead and high concurrent performance.  An unlimited number of
objects can be cached in memory without increased latency or degraded
throughput.")
    (license license:expat)))

(define-public go-github-com-coreos-go-systemd-activation
  (package
    (name "go-github-com-coreos-go-systemd-activation")
    (version "0.0.0-20191104093116-d3cd4ed1dbcf")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/coreos/go-systemd")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "193mgqn7n4gbb8jb5kyn6ml4lbvh4xs55qpjnisaz7j945ik3kd8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/coreos/go-systemd/activation"
       #:unpack-path "github.com/coreos/go-systemd"))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "Go bindings to systemd socket activation")
    (description "Go bindings to systemd socket activation; for writing and
using socket activation from Go.")
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-systemd-daemon
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-daemon")
    (arguments
     '(#:import-path "github.com/coreos/go-systemd/daemon"
       #:unpack-path "github.com/coreos/go-systemd"))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "Go bindings to systemd for notifications")
    (description "Go bindings to systemd for notifying the daemon of service
status changes")))

(define-public go-github-com-coreos-go-systemd-dbus
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-dbus")
    (arguments
     '(#:tests? #f ;Tests require D-Bus daemon running.
       #:import-path "github.com/coreos/go-systemd/dbus"
       #:unpack-path "github.com/coreos/go-systemd"))
    (native-inputs (list go-github-com-godbus-dbus))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "Go bindings to systemd for managing services")
    (description "Go bindings to systemd for starting/stopping/inspecting
running services and units.")))

(define-public go-github-com-coreos-go-systemd-journal
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-journal")
    (arguments
     '(#:tests? #f ;Tests require access to journald socket.
       #:import-path "github.com/coreos/go-systemd/journal"
       #:unpack-path "github.com/coreos/go-systemd"))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "Go bindings to systemd for writing journald")
    (description "Go bindings to systemd for writing to systemd's logging
service, journald.")))

(define-public go-github-com-coreos-go-systemd-login1
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-login1")
    (arguments
     '(#:tests? #f ;Tests require D-Bus daemon running.
       #:import-path "github.com/coreos/go-systemd/login1"
       #:unpack-path "github.com/coreos/go-systemd"))
    (native-inputs (list go-github-com-godbus-dbus))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "Go bindings to systemd for integration with logind API")
    (description "Go bindings to systemd for integration with the systemd
logind API.")))

(define-public go-github-com-coreos-go-systemd-machine1
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-machine1")
    (arguments
     '(#:tests? #f ;Tests require D-Bus daemon running.
       #:import-path "github.com/coreos/go-systemd/machine1"
       #:unpack-path "github.com/coreos/go-systemd"))
    (native-inputs (list go-github-com-godbus-dbus))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "Go bindings to systemd for registering machines/containers")
    (description "Go bindings to systemd for registering
machines/containers.")))

(define-public go-github-com-coreos-go-systemd-sdjournal
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-sdjournal")
    (arguments
     '(#:tests? #f ;Tests require D-Bus daemon running.
       #:import-path "github.com/coreos/go-systemd/sdjournal"
       #:unpack-path "github.com/coreos/go-systemd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-sdjournal-header
           (lambda* (#:key import-path #:allow-other-keys)
             (substitute* (format #f
                                  "src/~a/journal.go"
                                  import-path)
               (("systemd/sd-journal.h")
                "elogind/sd-journal.h")
               (("systemd/sd-id128.h")
                "elogind/sd-id128.h")))))))
    (inputs (list elogind))
    (synopsis "Go bindings to systemd for journald")
    (description "Go bindings to systemd for reading from journald by wrapping
its C API.")))

(define-public go-github-com-coreos-go-systemd-unit
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-unit")
    (arguments
     '(#:tests? #f ;Tests require D-Bus daemon running.
       #:import-path "github.com/coreos/go-systemd/unit"
       #:unpack-path "github.com/coreos/go-systemd"))
    (native-inputs (list go-github-com-godbus-dbus))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "Go bindings to systemd for working with unit files")
    (description "Go bindings to systemd for (de)serialization and comparison
of unit files.")))

(define-public go-github-com-cskr-pubsub
  (package
    (name "go-github-com-cskr-pubsub")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cskr/pubsub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18kqfdzkfs7z8266a5q5wldwkcvnhc7yw09b9vr8r0s7svy8d5s6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #t ; Tests require network interface access
      #:go go-1.18
      #:import-path "github.com/cskr/pubsub"))
    (home-page "https://github.com/cskr/pubsub")
    (synopsis "Simple pubsub package for go")
    (description
     "Package @code{pubsub} implements a simple multi-topic pub-sub library.")
    (license license:bsd-2)))

(define-public go-github-com-cyberdelia-go-metrics-graphite
  (package
    (name "go-github-com-cyberdelia-go-metrics-graphite")
    (version "0.0.0-20161219230853-39f87cc3b432")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cyberdelia/go-metrics-graphite")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1nnpwryw8i110laffyavvhx38gcd1jnpdir69y6fxxzpx06d094w"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-github-com-rcrowley-go-metrics))
    (arguments
     '(#:tests? #f ; Tests require network interface access
       #:import-path "github.com/cyberdelia/go-metrics-graphite"))
    (home-page "https://github.com/cyberdelia/go-metrics-graphite")
    (synopsis "Graphite client for go-metrics")
    (description "This package provides a reporter for the
@url{https://github.com/rcrowley/go-metrics,go-metrics} library which posts
metrics to Graphite.")
    (license license:bsd-2)))

(define-public go-github-com-dave-jennifer
  (package
    (name "go-github-com-dave-jennifer")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dave/jennifer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01sgafbds8n5zs61qf057whn06yj6avz30xgxk6pllf22528558m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.20
      #:import-path "github.com/dave/jennifer"))
    (home-page "https://github.com/dave/jennifer")
    (synopsis "Code generator for Go")
    (description "This package provides functionality to generate Go code.")
    (license license:expat)))

(define-public go-github-com-dimchansky-utfbom
  (package
    (name "go-github-com-dimchansky-utfbom")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dimchansky/utfbom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ll3wqvifmdanfyg6wsvz31c7n4mnczg2yxb65j35qxrnak89hn3"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/dimchansky/utfbom"))
    (home-page "https://github.com/dimchansky/utfbom")
    (synopsis "Go Unicode byte order mark detection library")
    (description
     "This package provides a library for @acronym{BOM, Unicode Byte Order
Mark} detection.")
    (license license:asl2.0)))

(define-public go-github-com-djherbis-atime
  (package
    (name "go-github-com-djherbis-atime")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djherbis/atime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xsz55zpihd9wyrj6qvm3miqzb6x3mnp5apzs0dx1byndhb8adpq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/djherbis/atime"))
    (home-page "https://github.com/djherbis/atime")
    (synopsis "Access Times for files")
    (description "Package atime provides a platform-independent way to get
atimes for files.")
    (license license:expat)))

(define-public go-github-com-dustin-gojson
  (package
    (name "go-github-com-dustin-gojson")
    (version "v0.0.0-20160307161227-2e71ec9dd5ad")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dustin/gojson")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vrmmyn7l568l1k71mxd54iqf3d54pn86cf278i374j86jn0bdxf"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Fix the library to work with go-1.21.
                   (substitute* "decode.go"
                     (("trying to unmarshal unquoted value into")
                      "trying to unmarshal unquoted value %v into"))
                   (substitute* "decode_test.go"
                     (("t.Fatalf\\(\"Unmarshal: %v\"\\)")
                      "t.Fatalf(\"Unmarshal: %v\", data)")) ;))))
                   (substitute* "scanner.go"
                     (("s := strconv.Quote\\(string\\(c\\)\\)")
                      "s := strconv.QuoteRune(rune(c))"))))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/dustin/gojson"
       #:go ,go-1.21))
    (home-page "https://github.com/dustin/gojson")
    (synopsis "Extended Golang's @code{encoding/json} module with the public scanner API")
    (description
     "This package provides a fork of Golang's @code{encoding/json} with the
scanner API made public.")
    (license license:bsd-3)))

(define-public go-github-com-elliotchance-orderedmap
  (package
    (name "go-github-com-elliotchance-orderedmap")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elliotchance/orderedmap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06gq5hsgfmzfr46wds366ghyn16qkygyz83vrsgargf4l7db9zg7"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/elliotchance/orderedmap"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/elliotchance/orderedmap")
    (synopsis "Go ordered map library")
    (description
     "This package provides a ordered map library that maintains amortized O(1)
for @code{Set}, @code{Get}, @code{Delete} and @code{Len}.")
    (license license:expat)))

(define-public go-github-com-gabriel-vasile-mimetype
  (package
    (name "go-github-com-gabriel-vasile-mimetype")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gabriel-vasile/mimetype")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11swnjczhrza0xi8q2wlk056nnbcghm44vqs52zfv6rwqvy6imhj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.20
      #:import-path "github.com/gabriel-vasile/mimetype"
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'add-supported-mimes-md
                     (lambda* (#:key import-path #:allow-other-keys)
                       ;; This file needs to be available for writing during the
                       ;; tests otherwise they will fail.
                       (let ((file (format #f "src/~a/supported_mimes.md"
                                           import-path)))
                         (invoke "touch" file)
                         (chmod file #o644)))))))
    (propagated-inputs (list go-golang-org-x-net))
    (home-page "https://github.com/gabriel-vasile/mimetype")
    (synopsis "Golang library for media type and file extension detection")
    (description
     "This package provides a Golang module that uses magic number signatures
to detect the MIME type of a file.

Main features:
@itemize
@item Fast and precise MIME type and file extension detection.
@item Supports
@url{https://github.com/gabriel-vasile/mimetype/blob/master/supported_mimes.md,
many MIME types}.
@item Allows to
@url{https://pkg.go.dev/github.com/gabriel-vasile/mimetype#example-package-Extend,
extend} with other file formats.
@item Common file formats are prioritized.
@item
@url{https://pkg.go.dev/github.com/gabriel-vasile/mimetype#example-package-TextVsBinary,
Differentiation between text and binary files}.
@item Safe for concurrent usage.
@end itemize")
    (license license:expat)))

(define-public go-github-com-hashicorp-errwrap
  (package
    (name "go-github-com-hashicorp-errwrap")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/errwrap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p5wdz8p7dmwphmb33gwhy3iwci5k9wkfqmmfa6ay1lz0cqjwp7a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/errwrap"))
    (home-page "https://github.com/hashicorp/errwrap")
    (synopsis "Wrapping and querying errors for Golang")
    (description
     "@code{errwrap} is a package for Go that formalizes the pattern of
wrapping errors and checking if an error contains another error.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-hcl
  (package
    (name "go-github-com-hashicorp-hcl")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/hcl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q6ml0qqs0yil76mpn4mdx4lp94id8vbv575qm60jzl1ijcl5i66"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/hcl"))
    (native-inputs
     (list go-github-com-davecgh-go-spew))
    (synopsis "Go implementation of HashiCorp Configuration Language V1")
    (description
     "This package contains the main implementation of the @acronym{HCL,
HashiCorp Configuration Language}.  HCL is designed to be a language for
expressing configuration which is easy for both humans and machines to read.")
    (home-page "https://github.com/hashicorp/hcl")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-hcl-v2
  (package
    (name "go-github-com-hashicorp-hcl-v2")
    (version "2.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/hcl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f9flmmkj7fr1337fc56cqy73faq87ix375hnz3id4wc023przv1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/hcl/v2"))
    (native-inputs
     (list go-github-com-davecgh-go-spew))
    (inputs
     (list go-github-com-agext-levenshtein
           go-github-com-apparentlymart-go-textseg-v13
           go-github-com-mitchellh-go-wordwrap
           go-github-com-zclconf-go-cty))
    (synopsis "Go implementation of HashiCorp Configuration Language V2")
    (description
     "This package contains the main implementation of the @acronym{HCL,
HashiCorp Configuration Language}.  HCL is designed to be a language for
expressing configuration which is easy for both humans and machines to read.")
    (home-page "https://github.com/hashicorp/hcl")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-hclog
  (package
    (name "go-github-com-hashicorp-go-hclog")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-hclog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lvr4ga95a0xb62vgq1hy558x3r65hn2d0h7bf0a88lsfsrcik0n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-hclog"))
    (propagated-inputs
     (list go-github-com-fatih-color
           go-github-com-mattn-go-isatty
           go-golang-org-x-tools))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/hashicorp/go-hclog")
    (synopsis "Key/value logging interface for Go")
    (description
     "This package provides a simple key/value logging interface for Golang
for use in development and production environments.  Unlike the standard
library @code{log} package, this package provides logging levels that provide
decreased output based upon the desired amount of output.  It also comes with
a command-line program @code{hclogvet} that can be used to check that the logging level
methods on @code{hclog.Logger} are used correctly.")
    (license license:expat)))

(define-public go-github-com-hashicorp-go-multierror
  (package
    (name "go-github-com-hashicorp-go-multierror")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-multierror")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l4s41skdpifndn9s8y6s9vzgghdzg4z8z0lld9qjr28888wzp00"))))
    (build-system go-build-system)
    (inputs (list go-github-com-hashicorp-errwrap))
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-multierror"))
    (home-page "https://github.com/hashicorp/go-multierror")
    (synopsis "Representing a errors list as a single error for Golang")
    (description
     "@code{go-multierror} is Golang module providing a mechanism for
representing a list of @code{error} values as a single @code{error}.  It is
fully compatible with the standard @code{errors} package, including
the functions @code{As}, @code{Is}, and @code{Unwrap}.  This provides a
standardized approach for introspecting on error values.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-syslog
  (package
    (name "go-github-com-hashicorp-go-syslog")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-syslog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09vccqggz212cg0jir6vv708d6mx0f9w5bxrcdah3h6chgmal6v1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-syslog"))
    (home-page "https://github.com/hashicorp/go-syslog")
    (synopsis "Golang syslog wrapper, cross-compile friendly")
    (description
     "This package is a very simple wrapper around log/syslog")
    (license license:expat)))

(define-public go-github-com-hashicorp-go-uuid
  (package
    (name "go-github-com-hashicorp-go-uuid")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-uuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wd4maaq20alxwcvfhr52rzfnwwpmc2a698ihyr0vfns2sl7gkzk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-uuid"))
    (home-page "https://github.com/hashicorp/go-uuid")
    (synopsis "Generate UUID-format strings")
    (description
     "This package generates UUID-format strings using high quality bytes.
It is not intended to be RFC compliant, merely to use a well-understood string
representation of a 128-bit value.  It can also parse UUID-format strings into
their component bytes.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-version
  (package
    (name "go-github-com-hashicorp-go-version")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-version")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fw6hwvjadpbfj10yk7f64ypw8lmv5s5ny3s4ria0nv6xam1wpai"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/hashicorp/go-version"))
    (home-page "https://github.com/hashicorp/go-version")
    (synopsis "Go library for parsing and verifying versions and version
constraints")
    (description
     "This package is a library for parsing versions and version
constraints, and verifying versions against a set of constraints.  It can sort
a collection of versions properly, handles prerelease/beta versions, can
increment versions.")
    (license license:mpl2.0)))

(define-public go-github-com-hhrutter-tiff
  (package
    (name "go-github-com-hhrutter-tiff")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hhrutter/tiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09fzgvxwkd34izbfd26ln8vdbhc4j9gxpar3s7h9h125psrjvg0k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hhrutter/tiff"))
    (propagated-inputs (list go-golang-org-x-image go-github-com-hhrutter-lzw))
    (home-page "https://github.com/hhrutter/tiff")
    (synopsis "Extended version of @code{golang.org/x/image/tiff}")
    (description "This package is an enhanced version of the
@code{golang.org/x/image/tiff} library featuring:

@itemize
@item Read support for CCITT Group3/4 compressed images.
@item Read/write support for LZW compressed images.
@item Read/write support for the CMYK color model.
@end itemize")
    (license license:bsd-3)))

(define-public go-github-com-jbenet-go-random
  (package
    (name "go-github-com-jbenet-go-random")
    (version "0.0.0-20190219211222-123a90aedc0c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbenet/go-random")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kgx19m8p76rmin8s8y6j1padciv1dx37qzy7jkh9bw49ai3haw3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jbenet/go-random"))
    (propagated-inputs
     (list go-github-com-dustin-go-humanize))
    (home-page "https://github.com/jbenet/go-random")
    (synopsis "Go library and a program that outputs randomness")
    (description
     "This is a Unix utility that outputs randomness.  It is a thin
wrapper around @code{crypto/rand}.")
    (license license:expat)))

(define-public go-github-com-jbenet-go-temp-err-catcher
  (package
    (name "go-github-com-jbenet-go-temp-err-catcher")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbenet/go-temp-err-catcher")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n482jhh6jwq43jj21xkq8grqzx78hjh7f44p0q3n01zp1dsh97r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jbenet/go-temp-err-catcher"))
    (home-page "https://github.com/jbenet/go-temp-err-catcher")
    (synopsis "Error handling helper library")
    (description "Package @code{temperrcatcher} provides a @code{TempErrCatcher}
object, which implements simple error-retrying functionality.")
    (license license:expat)))

(define-public go-github-com-jbenet-goprocess
  (package
    (name "go-github-com-jbenet-goprocess")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbenet/goprocess")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z4a5skx9kh2c727pc6zz0vhf9v8acd320s7z0f1kwy3y1nbdhjk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jbenet/goprocess"))
    (native-inputs
     (list go-github-com-jbenet-go-cienv))
    (home-page "https://github.com/jbenet/goprocess")
    (synopsis "Manage process life cycles in Go")
    (description
     "@code{goprocess} introduces a way to manage process lifecycles in
Go.  It is much like @code{go.net/context} (it actually uses a Context), but it is
more like a Context-WaitGroup hybrid.  @code{goprocess} is about being able to start
and stop units of work, which may receive @code{Close} signals from many clients.")
    (license license:expat)))

(define-public go-github-com-jinzhu-copier
  (package
    (name "go-github-com-jinzhu-copier")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jinzhu/copier")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kf29cmmbic72kfrfd1xnass7l9j85impf8mqn5f3fd3ibi9bs74"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/jinzhu/copier"))
    (home-page "https://github.com/jinzhu/copier")
    (synopsis "Go copier library")
    (description
     "This package provides a library, which supports copying value from one
struct to another.")
    (license license:expat)))

(define-public go-github-com-josharian-intern
  (package
    (name "go-github-com-josharian-intern")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/josharian/intern")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1za48ppvwd5vg8vv25ldmwz1biwpb3p6qhf8vazhsfdg9m07951c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/josharian/intern"))
    (home-page "https://github.com/josharian/intern")
    (synopsis "String interning for Go")
    (description
     "This library defines functions to perform string interning in Go,
storing only one copy of each unique string in memory.  All functions may be
called concurrently with themselves and each other.")
    (license license:expat)))

(define-public go-github-com-k0kubun-pp
  (package
    (name "go-github-com-k0kubun-pp")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/k0kubun/pp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vpp5n3kdazk4s1ljhwbrhz3kilzvdvx5hya922bg0q9vnjqqvvc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/k0kubun/pp"))
    (propagated-inputs (list go-github-com-mattn-go-colorable
                             go-golang-org-x-text))
    (home-page "https://github.com/k0kubun/pp")
    (synopsis "Colored pretty-printer for Go")
    (description
     "This package provides a pretty-printer for Go.  The functions defined by
@code{pp} follow an API similar to @code{fmt} and its configuration can be
customized globally.")
    (license license:expat)))

(define-public go-github-com-lib-pq
  (package
    (name "go-github-com-lib-pq")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lib/pq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08j1smm6rassdssdks4yh9aspa1dv1g5nvwimmknspvhx8a7waqz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lib/pq"
      ;; The tests seem to fail without access to the network or a running
      ;; Postgres instance.
      #:tests? #f))
    (home-page "https://github.com/lib/pq")
    (synopsis "Golang Postgres driver for Go's database/sql")
    (description
     "This package provides a pure Go Postgres driver for Go's
database/sql package.")
    (license license:expat)))

(define-public go-github-com-kballard-go-shellquote
  ;; No release, see <https://github.com/kballard/go-shellquote/issues/13>.
  (let ((commit "95032a82bc518f77982ea72343cc1ade730072f0")
        (revision "1"))
    (package
      (name "go-github-com-kballard-go-shellquote")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kballard/go-shellquote")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1rspvmnsikdq95jmx3dykxd4k1rmgl98ryjrysvl0cf18hl1vq80"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/kballard/go-shellquote"))
      (synopsis "Shell-style string joins and splits")
      (description
       "Shellquote provides utilities for joining/splitting strings using sh's
word-splitting rules.")
      (home-page "https://github.com/kballard/go-shellquote")
      (license license:expat))))

(define-public go-github-com-matryer-try
  (package
    (name "go-github-com-matryer-try")
    (version "1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matryer/try")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15f0m5ywihivnvwzcw0mh0sg27aky9rkywvxqszxka9q051qvsmy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/matryer/try"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (substitute* (string-append "src/" import-path
                                          "/try_test.go")
                (("var value string")
                 "")
                (("value, err = SomeFunction\\(\\)")
                 "_, err = SomeFunction()")))))))
    (native-inputs
     (list go-github-com-cheekybits-is))
    (home-page "https://github.com/matryer/try")
    (synopsis "Simple idiomatic retry package for Go")
    (description "This package provides an idiomatic Go retry module.")
    (license license:expat)))

(define-public go-github-com-mattn-go-shellwords
  (package
    (name "go-github-com-mattn-go-shellwords")
    (version "1.0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-shellwords")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l0l5s4hlsrm4z6hygig2pp1qirk5ycrzn9z27ay3yvg9k7zafzx"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mattn/go-shellwords"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sh-path
           (lambda* (#:key import-path #:allow-other-keys)
             (substitute* (string-append
                           "src/" import-path "/util_posix.go")
               (("/bin/sh") (which "sh"))))))))
    (home-page "https://github.com/mattn/go-shellwords")
    (synopsis "Parse lines into shell words")
    (description "This package parses text into shell arguments.  Based on
the @code{cpan} module @code{Parse::CommandLine}.")
    (license license:expat)))

(define-public go-github-com-miekg-dns
  (package
    (name "go-github-com-miekg-dns")
    (version "1.1.48")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/miekg/dns")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "14m4wnbgmc1prj4ds1fsz1nwb1awaq365lhbp8clzsidxmhjf3hl"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/miekg/dns"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-golang-org-x-sys
           go-golang-org-x-sync
           go-golang-org-x-net))
    (home-page "https://github.com/miekg/dns")
    (synopsis "Domain Name Service library in Go")
    (description
      "This package provides a fully featured interface to the @acronym{DNS,
Domain Name System}.  Both server and client side programming is supported.
The package allows complete control over what is sent out to the @acronym{DNS,
Domain Name Service}.  The API follows the less-is-more principle, by
presenting a small interface.")
    (license license:bsd-3)))

(define-public go-github-com-mreiferson-go-options
  (package
    (name "go-github-com-mreiferson-go-options")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mreiferson/go-options")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pxs9ybrh196qy14ijn4zn51h2z28lj31y6vxrz2xxhgvpmfmxyl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mreiferson/go-options"))
    (home-page "https://github.com/mreiferson/go-options")
    (synopsis "Go package to structure and resolve options")
    (description
     "The @code{options} Go package resolves configuration values set via
command line flags, config files, and default struct values.")
    (license license:expat)))

(define-public go-github-com-mreiferson-go-svc
  ;; NSQ specific fork of github.com/judwhite/go-svc, as Guix go build system
  ;; does not support go.mod with `replace' statement.
  (let ((commit "7a96e00010f68d9436e3de53a70c53f209a0c244")
        (revision "0"))
    (package
      (name "go-github-com-mreiferson-go-svc")
      (version (git-version "1.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mreiferson/go-svc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1acgb0n3svhnraqj1fz5qc5n3b4vc5ffwyk9vfi6gcfkibm0hgmd"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/judwhite/go-svc"))
      (propagated-inputs (list go-golang-org-x-sys))
      (home-page "https://github.com/mreiferson/go-svc")
      (synopsis "Go Windows Service wrapper for GNU/Linux")
      (description
       "Go Windows Service wrapper compatible with GNU/Linux.  Windows tests
@url{https://github.com/judwhite/go-svc/raw/master/svc/svc_windows_test.go,here}.")
      (license license:expat))))

(define-public go-github-com-multiformats-go-varint
  (package
    (name "go-github-com-multiformats-go-varint")
    (version "0.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-varint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l4s0z3rc3d350zp6qximl1jjhic6l8w74wkmx244jgfzsxd93af"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-varint"))
    (home-page "https://github.com/multiformats/go-varint")
    (synopsis "Varint helpers that enforce minimal encoding")
    (description
     "This package provides a functionality for encoding and decoding unsigned
varints.")
    (license license:expat)))

(define-public go-github-com-nats-io-nats-go
  (package
    (name "go-github-com-nats-io-nats-go")
    (version "1.32.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nats-io/nats.go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08b3n5mdpxvn9hipz0j001bp5r67i43cqji9x9dyzikypqdfg38k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.20
      #:import-path "github.com/nats-io/nats.go"))
    (propagated-inputs (list go-golang-org-x-text
                         go-github-com-nats-io-nuid
                         go-github-com-nats-io-nkeys
                         go-github-com-klauspost-compress))
    (home-page "https://github.com/nats-io/nats.go")
    (synopsis "Go Client for NATS server")
    (description
     "This package provides a Go client for the NATS messaging system.")
    (license license:asl2.0)))

(define-public go-github-com-nats-io-nuid
  (package
    (name "go-github-com-nats-io-nuid")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nats-io/nuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11zbhg4kds5idsya04bwz4plj0mmiigypzppzih731ppbk2ms1zg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nats-io/nuid"))
    (home-page "https://github.com/nats-io/nuid")
    (synopsis "Go library implementing identifier generator for NATS ecosystem")
    (description
     "This package provides a unique identifier generator that is high performance,
very fast, and tries to be entropy pool friendly.")
    (license license:asl2.0)))

(define-public go-github-com-nbrownus-go-metrics-prometheus
  (package
    (name "go-github-com-nbrownus-go-metrics-prometheus")
    (version "0.0.0-20210712211119-974a6260965f")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nbrownus/go-metrics-prometheus")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1kl9l08aas544627zmhkgp843qx94sxs4inxm20nw1hx7gp79dz0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/nbrownus/go-metrics-prometheus"))
    (propagated-inputs
     (list go-github-com-stretchr-testify
           go-github-com-rcrowley-go-metrics
           go-github-com-prometheus-client-golang))
    (home-page "https://github.com/nbrownus/go-metrics-prometheus")
    (synopsis "Prometheus support for go-metrics")
    (description "This package provides a reporter for the @code{go-metrics}
library which posts the metrics to the Prometheus client registry and just
updates the registry.")
    (license license:asl2.0)))

(define-public go-github-com-nsqio-go-diskqueue
  (package
    (name "go-github-com-nsqio-go-diskqueue")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nsqio/go-diskqueue")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hp66hkmfn0nyf3c53a40f94ah11a9rj01r5zp3jph9p54j8rany"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nsqio/go-diskqueue"))
    (home-page "https://github.com/nsqio/go-diskqueue")
    (synopsis "Go package providing a file system backed FIFO queue")
    (description
     "The @code{diskqueue} Go package provides a file system backed FIFO
queue.")
    (license license:expat)))

(define-public go-github-com-nsqio-go-nsq
  (package
    (name "go-github-com-nsqio-go-nsq")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nsqio/go-nsq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h9z3z225sdgg7fl3l7x11xn5ch6lm5flgmcj046cdp453qj2qhf"))))
    (build-system go-build-system)
    (arguments
     (list #:tests? #f                  ;tests require networking
           #:import-path "github.com/nsqio/go-nsq"))
    (propagated-inputs (list go-github-com-golang-snappy))
    (home-page "https://github.com/nsqio/go-nsq")
    (synopsis "Consumer/producer library for NSQ")
    (description
     "The @code{nsq} Go module provides a high-level @code{Consumer} and
@code{Producer} types as well as low-level functions to communicate over the
NSQ protocol @url{https://nsq.io/}.")
    (license license:expat)))

(define-public go-github-com-op-go-logging
  (package
    (name "go-github-com-op-go-logging")
    (version "1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/op/go-logging")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01a6lkpj5p82gplddh55az194s9y3014p4j8x4zc8yv886z9c8gn"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ; ERROR: incorrect callpath: String.rec...a.b.c.Info.
       #:import-path "github.com/op/go-logging"))
    (home-page "https://github.com/op/go-logging")
    (synopsis "Go logging library")
    (description
     "Go-Logging implements a logging infrastructure for Go.  Its
output format is customizable and supports different logging backends like
syslog, file and memory.  Multiple backends can be utilized with different log
levels per backend and logger.")
    (license license:bsd-3)))

(define-public go-github-com-orisano-pixelmatch
  (package
    (name "go-github-com-orisano-pixelmatch")
    (version "0.0.0-20230914042517-fa304d1dc785")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/orisano/pixelmatch")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lplxfif5mfqnd0jjph2vd25c3bpr3idfs2axh8z0ib0zdkwca32"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/orisano/pixelmatch"))
    (home-page "https://github.com/orisano/pixelmatch")
    (synopsis "Pixelmatch port to Go")
    (description
     "This package provides a port of Pixelmatch, a pixel-level image
comparison library, to Go.  Both a library and a command-line tool are
included in this package.")
    (license license:expat)))

(define-public go-github-com-prometheus-client-model
  (let ((commit "14fe0d1b01d4d5fc031dd4bec1823bd3ebbe8016")
        (revision "2"))
    (package
      (name "go-github-com-prometheus-client-model")
      (version (git-version "0.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/prometheus/client_model")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zdmk6rbbx39cvfz0r59v2jg5sg9yd02b4pds5n5llgvivi99550"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/prometheus/client_model"
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           ;; Source-only package
           (delete 'build))))
      (propagated-inputs
       (list go-github-com-golang-protobuf-proto))
      (synopsis "Data model artifacts for Prometheus")
      (description "This package provides data model artifacts for Prometheus.")
      (home-page "https://github.com/prometheus/client_model")
      (license license:asl2.0))))

(define-public go-github-com-rcrowley-go-metrics
  (let ((commit "cac0b30c2563378d434b5af411844adff8e32960")
        (revision "2"))
    (package
      (name "go-github-com-rcrowley-go-metrics")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rcrowley/go-metrics")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hfxffnpaw49pr3wrkbzq3pnv3nyzsvk5dxndv0yz70xlrbg8a04"))))
      (build-system go-build-system)
      (arguments
       ;; Arbitrary precision tests are known to be broken on aarch64, ppc64le
       ;; and s390x. See: https://github.com/rcrowley/go-metrics/issues/249
       `(#:tests? ,(not (string-prefix? "aarch64" (or (%current-target-system)
                                                      (%current-system))))
         #:import-path "github.com/rcrowley/go-metrics"))
      (propagated-inputs
       (list go-github-com-stathat-go))
      (synopsis "Go port of Coda Hale's Metrics library")
      (description "This package provides a Go implementation of Coda Hale's
Metrics library.")
      (home-page "https://github.com/rcrowley/go-metrics")
      (license license:bsd-2))))

(define-public go-github-com-shirou-gopsutil
  (package
    (name "go-github-com-shirou-gopsutil")
    (version "2.21.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shirou/gopsutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gpb10xkdwfimn1sp4jhrvzz4p3zgmdb78q8v23nap3yi6v4bff5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.18
      #:import-path "github.com/shirou/gopsutil"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'remove-v3
                     (lambda* (#:key import-path #:allow-other-keys)
                       ;; We remove the separately included v3 module.
                       (delete-file-recursively (string-append "src/"
                                                               import-path
                                                               "/v3"))))
                   (add-before 'check 'remove-failing-tests
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively
                        ;; host_test.go tries to access files such as
                        ;; /var/run/utmp that do not exist in the build
                        ;; environment.
                        (string-append "src/" import-path "/host/host_test.go")))))))
    (propagated-inputs
     (list go-github-com-tklauser-go-sysconf go-golang-org-x-sys))
    (native-inputs
     (list go-github-com-stretchr-testify procps))
    (synopsis "Process and system monitoring in Go")
    (description
     "This package provides a library for retrieving information
on running processes and system utilization (CPU, memory, disks, network,
sensors).")
    (home-page "https://github.com/shirou/gopsutil")
    (license license:bsd-3)))

(define-public go-github-com-shirou-gopsutil-v3
  (package
    (inherit go-github-com-shirou-gopsutil)
    (name "go-github-com-shirou-gopsutil-v3")
    (version "3.24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shirou/gopsutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xlfcx6giqaxdah2m02q2i8ynwlzar953wr8wqx1j3004xdgaivd"))))
    (arguments
     (list
      #:go go-1.18
      #:import-path "github.com/shirou/gopsutil"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               ;; host_test.go tries to access files such as
               ;; /var/run/utmp that do not exist in the build
               ;; environment.
               (string-append "src/" import-path "/host/host_test.go")))))))))

(define-public go-github-com-skip2-go-qrcode
  (package
    (name "go-github-com-skip2-go-qrcode")
    (version "0.0.0-20200617195104-da1b6568686e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skip2/go-qrcode")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pghd6y2x8a5fqy4rjn4d8j5jcslb236naycdza5an7vyvinsgs9"))
       (patches (search-patches "go-github-com-skip2-go-qrcode-fix-tests.patch"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/skip2/go-qrcode"))
    (home-page "https://github.com/skip2/go-qrcode")
    (synopsis "QR code encoder")
    (description "This package provides a QR code encoder for the Goloang.")
    (license license:expat)))

(define-public go-github-com-songgao-water
  (package
    (name "go-github-com-songgao-water")
    (version "0.0.0-20200317203138-2b4b6d7c09d8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/songgao/water")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1k5aildfszp6x66jzar4y36lic8ijkb5020hfaivpvq3bnwdiikl"))))
    (build-system go-build-system)
    (arguments '(#:tests? #f ; Tests require network interface access
                 #:import-path "github.com/songgao/water"))
    (home-page "https://github.com/songgao/water")
    (synopsis "Simple network TUN/TAP library")
    (description
      "This package provides a simple TUN/TAP interface library for Go that
efficiently works with standard packages like @code{io}, @code{bufio}, etc..
Use waterutil with it to work with TUN/TAP packets/frames.")
    (license license:bsd-3)))

(define-public go-github-com-songmu-gitconfig
  (package
    (name "go-github-com-songmu-gitconfig")
    (version "0.1.0")
    (home-page "https://github.com/songmu/gitconfig")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y01h496a7pfj1g2bclls5b0nl3vnj7nz610jj1dzq9kxrwxk7fk"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Package's tests appear to be hardcoded to the author's gitconfig
      ;; and require network access.
      #:tests? #f
      #:go go-1.21
      #:import-path "github.com/Songmu/gitconfig"))
    (propagated-inputs
     (list go-github-com-goccy-yaml))
    (synopsis "Go library to get configuration values from gitconfig")
    (description
     "@{gitconfig} is a package to get configuration values from gitconfig.")
    (license license:expat)))

(define-public go-github-com-stathat-go
  (let ((commit "74669b9f388d9d788c97399a0824adbfee78400e")
        (revision "0"))
    (package
      (name "go-github-com-stathat-go")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stathat/go")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zzlsl24dyr202qkr2pay22m6d0gb7ssms77wgdx0r0clgm7dihw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/stathat/go"))
      (synopsis "Post statistics to StatHat")
      (description "This is a Go package for posting to a StatHat account.")
      (home-page "https://github.com/stathat/go")
      (license license:expat))))

(define-public go-github-com-tklauser-go-sysconf
  (package
    (name "go-github-com-tklauser-go-sysconf")
    (version "0.3.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tklauser/go-sysconf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07vkimncnmh89706s49599h2w9gwa6jyrv70f8ifw90nsh766km9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.18
      #:import-path "github.com/tklauser/go-sysconf"
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'remove-failing-tests
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively
                        ;; sysconf_test.go (among others) tries to read the
                        ;; number of online CPUs using /proc/stat and
                        ;; /sys/devices/system/cpu/online. These files are not
                        ;; accessible in the test environment.
                        (string-append "src/" import-path
                                       "/cgotest/sysconf_test.go")))))))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-tklauser-numcpus))
    (home-page "https://github.com/tklauser/go-sysconf")
    (synopsis "Go implementation of @code{sysconf}")
    (description
     "This package implements @code{sysconf} and provides the associated
@code{SC_*} constants to query system configuration values at run time.")
    (license license:bsd-3)))

(define-public go-github-com-tklauser-numcpus
  (package
    (name "go-github-com-tklauser-numcpus")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tklauser/numcpus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xcwk42zr6q72zvkqdd9nbyhvq11rmwm2164mr2rvbb9z7alkff8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.18
      #:import-path "github.com/tklauser/numcpus"
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'remove-failing-tests
                     (lambda* (#:key import-path #:allow-other-keys)
                       (with-directory-excursion (string-append "src/"
                                                                import-path)
                         (for-each delete-file-recursively
                                   ;; These tests try to access
                                   ;; /sys/devices/system/cpu, which is not
                                   ;; available in the test environment.
                                   '("numcpus_test.go" "numcpus_linux_test.go"))))))))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/tklauser/numcpus")
    (synopsis "Provides information about the number of CPUs in the system")
    (description
     "This package provides both library functions and a command-line tool to
query information regarding the number of CPUs available to the system.")
    (license license:asl2.0)))

(define-public go-github-com-vividcortex-ewma
  (package
    (name "go-github-com-vividcortex-ewma")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VividCortex/ewma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0whx516l9nm4n41spagb605ry7kfnz1qha96mcshnfjlahhnnylq"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/VividCortex/ewma"))
    (home-page "https://github.com/VividCortex/ewma")
    (synopsis "Exponentially Weighted Moving Average algorithms for Go")
    (description
     "This package implements algorithms for
@url{https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average,exponentially
weighted moving averages}.")
    (license license:expat)))

(define-public go-github-com-whyrusleeping-go-sysinfo
  (package
    (name "go-github-com-whyrusleeping-go-sysinfo")
    (version "0.0.0-20190219211824-4a357d4b90b1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/go-sysinfo")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s6yjp9incc579wbbga33vq0hcanv8j2xh9l90ya0r4fihz39jiq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/whyrusleeping/go-sysinfo"))
    (propagated-inputs
     (list go-github-com-dustin-go-humanize))
    (home-page "https://github.com/whyrusleeping/go-sysinfo")
    (synopsis "Package to extract system information")
    ;; There is not much information provided by the project, see
    ;; <https://github.com/whyrusleeping/go-sysinfo/issues>.
    (description
     "This package provides a basic system stats like @code{DiskUsage} and
@code{MemoryInfo}.")
    (license license:expat)))

(define-public go-github-com-xhit-go-str2duration-v2
  (package
    (name "go-github-com-xhit-go-str2duration-v2")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xhit/go-str2duration")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c9zi9mfy5ww413y1jpfh1rdis43lvd5v6gvajqzh4q1km9lyxjj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xhit/go-str2duration/v2"))
    (home-page "https://github.com/xhit/go-str2duration")
    (synopsis "Convert string to duration in golang")
    (description
     "This package provides a means to obtain @code{time.Duration} from a
string.  The string can be a string retorned for @code{time.Duration} or a
similar string with weeks or days too.")
    (license license:bsd-3)))

(define-public go-go-uber-org-automaxprocs
  (package
    (name "go-go-uber-org-automaxprocs")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/automaxprocs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03arxcfaj7k6iwfdk0liaynxf9rjfj9m5glsjp7ws01xjkgrdpbc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.uber.org/automaxprocs"))
    (native-inputs (list go-github-com-stretchr-testify
                         go-github-com-prashantv-gostub))
    (home-page "https://github.com/uber-go/automaxprocs")
    (synopsis "CPU-count detection library for Go")
    (description
     "This package automatically set GOMAXPROCS to match Linux container
CPU quota.")
    (license license:expat)))

(define-public go-go-uber-org-zap
  (package
    (name "go-go-uber-org-zap")
    (version "1.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/zap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lzbbs87fvixzbyv4wpl3s70vm2m0jz2jgdvrviiksc2al451qgs"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.uber.org/zap"
       #:tests? #f)) ; TODO: Fix tests
    (native-inputs
     (list go-github-com-stretchr-testify
           go-golang-org-x-lint
           go-honnef-co-go-tools))
    (propagated-inputs
     (list go-github-com-pkg-errors
           go-go-uber-org-atomic
           go-go-uber-org-multierr
           go-gopkg-in-yaml-v2))
    (home-page "https://go.uber.org/zap")
    (synopsis "Logging library for Go")
    (description
     "This package provides a library for fast, structured, leveled logging in
Go.")
    (license license:expat)))

(define-public go-gopkg-in-op-go-logging-v1
  (package
    (inherit go-github-com-op-go-logging)
    (name "go-gopkg-in-op-go-logging-v1")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-op-go-logging)
       ((#:import-path _) "gopkg.in/op/go-logging.v1")))))

;;;
;;; Executables:
;;;

(define-public go-hclogvet
  (package
    (inherit go-github-com-hashicorp-go-hclog)
    (name "go-hclogvet")
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-hclog/hclogvet"
      #:unpack-path "github.com/hashicorp/go-hclog"
      #:install-source? #f))
    (propagated-inputs
     (list go-golang-org-x-tools))
    (description
     "@code{hclogvet} is a @code{go vet} tool for checking that the
Trace/Debug/Info/Warn/Error methods on @code{hclog.Logger} are used
correctly.")))

(define-public go-numcpus
  (package
    (inherit go-github-com-tklauser-numcpus)
    (name "go-numcpus")
    (arguments
     (list
      #:go go-1.18
      #:import-path "github.com/tklauser/numcpus/cmd/numcpus"
      #:unpack-path "github.com/tklauser/numcpus"
      #:install-source? #f))
    (description
     "This package provides a CLI build from the
go-github-com-tklauser-numcpus source.")))

(define-public go-pixelmatch
  (package
    (inherit go-github-com-orisano-pixelmatch)
    (name "go-pixelmatch")
    (arguments
     (list
      #:import-path "github.com/orisano/pixelmatch/cmd/pixelmatch"
      #:unpack-path "github.com/orisano/pixelmatch"
      #:install-source? #f))
    (synopsis "Pixel-level image comparison command")
    (description
     "This package provides a CLI build from the
go-github-com-orisano-pixelmatch source.")))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
