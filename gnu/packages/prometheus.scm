;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2024 Dominic Martinez <dom@dominicm.dev>
;;; Copyright © 2024 Jesse Eisses <jesse@eisses.email>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages prometheus)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz))

;;; Commentary:
;;;
;;; Libraries and commands related to, or provided by Prometheus project
;;; <https://prometheus.io>.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-contrib-go-opencensus-io-exporter-prometheus
  (package
    (name "go-contrib-go-opencensus-io-exporter-prometheus")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/census-ecosystem/opencensus-go-exporter-prometheus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n09d2nbng4bws9vi2ddq2ffv9hr0c3i9mif6fkjr4chyyyiy8ik"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "contrib.go.opencensus.io/exporter/prometheus"))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-statsd-exporter
           go-go-opencensus-io))
    (home-page "https://github.com/census-ecosystem/opencensus-go-exporter-prometheus")
    (synopsis "OpenCensus Go Prometheus Exporter")
    (description
     "Package prometheus contains a Prometheus exporter that supports
exporting @code{OpenCensus} views as Prometheus metrics.")
    (license license:asl2.0)))

(define-public go-github-com-mwitkow-go-conntrack
  (package
    (name "go-github-com-mwitkow-go-conntrack")
    (version "0.0.0-20190716064945-2f068394615f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mwitkow/go-conntrack")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ymjmy12ks7smgwmrwsa5kf07d9w5kpk1dn650azlzr61b561aw7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/mwitkow/go-conntrack"
      #:phases
      #~(modify-phases %standard-phases
          ;; Breaking cycle:
          ;; go-github-com-prometheus-common ->
          ;; go-github-com-prometheus-client-golang ->
          ;; go-github-com-mwitkow-go-conntrack ->
          ;; go-github-com-prometheus-common
          (delete 'build))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-jpillora-backoff
           go-github-com-munnerz-goautoneg
           ;; go-github-com-prometheus-client-golang
           go-golang-org-x-net))
    (home-page "https://github.com/mwitkow/go-conntrack")
    (synopsis "Go middleware for @code{net.Conn} tracking")
    (description
     "@url{https://prometheus.io/,Prometheus} monitoring and
@url{https://godoc.org/golang.org/x/net/trace#@code{EventLog,(code}
x/net/trace)} tracing wrappers @code{net.Conn}, both inbound
(@@code{net.Listener}) and outbound (@@code{net.Dialer}).")
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
    (arguments
     (list
      ;; The project looks abandoned, tests failed with a new go-metrics, see
      ;; <https://github.com/nbrownus/go-metrics-prometheus/pull/2>.
      #:tests? #f
      #:import-path "github.com/nbrownus/go-metrics-prometheus"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-prometheus-client-golang
           go-github-com-rcrowley-go-metrics))
    (home-page "https://github.com/nbrownus/go-metrics-prometheus")
    (synopsis "Prometheus support for go-metrics")
    (description
     "This package provides a reporter for the @code{go-metrics} library which
posts the metrics to the Prometheus client registry and just updates the
registry.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-client-golang
  (package
    (name "go-github-com-prometheus-client-golang")
    (version "1.20.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/client_golang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q3n22p5ic22xzha6mffh0m0jzbxrkyjrcmnxsnanl61jwb4rkpw"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - dagger
            ;; - .bingo - fake module
            (delete-file-recursively "dagger")
            (delete-file-recursively ".bingo")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      ;; XXX: Check if the most of the tests may be enabled:
      ;; api/prometheus/v1/api_test.go:1063:23: cannot use 1634644800304
      ;; (untyped int constant) as int value in map literal (overflows)
      #:tests? (target-64bit?)
      #:import-path "github.com/prometheus/client_golang"
      #:test-flags
      #~(list "-skip" (string-append
                       ;; Test fails with Assertion error.
                       "TestHandler"
                       ;; Test fails on aarch64-linux system.
                       #$@(if (not (target-x86-64?))
                              '("|TestProcessCollector")
                              '())))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples-and-tutorials
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "api/prometheus/v1/example_test.go"
                                "examples"
                                "tutorials"))))))))
    (propagated-inputs
     (list go-github-com-beorn7-perks
           go-github-com-cespare-xxhash-v2
           go-github-com-google-go-cmp
           go-github-com-json-iterator-go
           go-github-com-klauspost-compress
           go-github-com-kylelemons-godebug
           go-github-com-prometheus-client-model
           go-github-com-prometheus-common
           go-github-com-prometheus-procfs
           go-golang-org-x-sys
           go-google-golang-org-protobuf))
    (home-page "https://github.com/prometheus/client_golang")
    (synopsis "HTTP server and client tools for Prometheus")
    (description
     "This package @code{promhttp} provides HTTP client and server tools for
Prometheus metrics.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-client-model
  (package
    (name "go-github-com-prometheus-client-model")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/client_model")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g1q2szzwp4rwkvayi2mnq2nwj6hj4ja7j43vwyi1iaz6d9z505c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/prometheus/client_model"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
        ;; Source-only package
        (delete 'build))))
    (propagated-inputs
     (list go-github-com-golang-protobuf))
    (home-page "https://github.com/prometheus/client_model")
    (synopsis "Data model artifacts for Prometheus")
    (description
     "This package provides data model artifacts for Prometheus.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-common
  (package
    (name "go-github-com-prometheus-common")
    (version "0.61.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/common")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wng61rzvh27s2rlaadvjbffwgpn74p1wjrz6insl57k1pg3cmcn"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packed as
            ;; separated packages:
            ;;
            ;; - github.com/prometheus/common/assets
            ;; - github.com/prometheus/common/sigv4
            (for-each delete-file-recursively
                      (list "assets" "sigv4"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/prometheus/common"
      #:test-subdirs
      #~(list
         ;; Skipp, as it requires
         ;; <github.com/prometheus/client_golang/prometheus>, which introduces
         ;; cycle.
         ;; "./config/..."

         ;; Some tests fail on non x86_64 architecture: Cannot use 9223372036
         ;; (untyped int constant) as int value in ;; struct literal
         ;; (overflows).  Cannot use math.MaxInt64 (untyped int constant
         ;; 9223372036854775807) as int value in argument to HumanizeTimestamp
         ;; (overflows)
         #$@(if (target-x86-64?)
                '("./helpers/...")
                '())
         "./expfmt/..."
         "./model/..."
         "./promlog/..."
         "./route/..."
         "./server/...")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-alecthomas-kingpin-v2
           go-github-com-go-kit-log
           go-github-com-google-go-cmp
           go-github-com-julienschmidt-httprouter
           go-github-com-munnerz-goautoneg
           go-github-com-mwitkow-go-conntrack
           go-github-com-prometheus-client-model
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-google-golang-org-protobuf
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/prometheus/common")
    (synopsis "Shared Prometheus Golang components")
    (description
     "This package provides Go libraries that are shared across Prometheus
components.

@itemize
@item @code{config} - common configuration structures
@item @code{expfmt} - decoding and encoding for the exposition format
@item @code{model} - shared data structures
@item @code{promslog} - a logging wrapper around log/slog
@item @code{route} - a routing wrapper around httprouter using context.Context
@item @code{server} - common servers
@item @code{version} version information and metrics
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-common-assets
  (package
    (name "go-github-com-prometheus-common-assets")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/common")
             (commit (go-version->git-ref version
                                          #:subdir "assets"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r7sym4yaysbkc5anyypl57v9ay0a1flq00j85j7lcficl2scwrs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/prometheus/common/assets"
      #:unpack-path "github.com/prometheus/common"))
    (home-page "https://github.com/prometheus/common")
    (synopsis "Prometheus assets")
    (description
     "This package provides Prometheus assets.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-community-pro-bing
  (package
    (name "go-github-com-prometheus-community-pro-bing")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus-community/pro-bing")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19757nqz0cpq7ir2w5xgjxpblhmkpk0j7spfw4j68agavbx6hxpm"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests requiring network setup, and root access.
      #:test-flags
      #~(list "-skip"
              (string-join
               (list "TestNewPingerValid"
                     "TestSetIPAddr"
                     "TestSetInterfaceName"
                     "TestSetResolveTimeout")
               "|"))
      #:import-path "github.com/prometheus-community/pro-bing"))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-golang-org-x-net
           go-golang-org-x-sync))
    (home-page "https://github.com/prometheus-community/pro-bing")
    (synopsis "Continuous probers Golang library")
    (description
     "This package implements @acronym{Internet Control Message
Protocol,ICMP} echo (ping) functionality.")
    (license license:expat)))

(define-public go-github-com-prometheus-exporter-toolkit
  (package
    (name "go-github-com-prometheus-exporter-toolkit")
    (version "0.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/exporter-toolkit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05k4sfrc1zs96iprgnap0gd42vwfq47j6vg2bv83nckcv731gmiv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/prometheus/exporter-toolkit"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v"
                          "-skip" "TestServerBehaviour|TestConfigReloading"
                          "./..."))))))))
    (native-inputs
     (list go-google-golang-org-protobuf))
    (propagated-inputs
     (list go-github-com-alecthomas-kingpin-v2
           go-github-com-coreos-go-systemd-v22
           go-github-com-go-kit-log
           go-github-com-mdlayher-vsock
           ; Imported for go-github-com-prometheus-common to break the cycle.
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-common
           go-golang-org-x-crypto
           go-golang-org-x-sync
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/prometheus/exporter-toolkit")
    (synopsis "Utility package to build Prometheus exporters")
    (description
     "This package provides tooling to build Prometheus exporters")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-procfs
  (package
    (name "go-github-com-prometheus-procfs")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/procfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f692685zcssryd38bahmamd72iaiilngp92gl1s9177891f44gm"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Check if the most of the tests may be enabled on non x86_64
      ;; architectures, disable for now: ./proc_stat_test.go:98:49: cannot use
      ;; math.MinInt64 (untyped int constant -9223372036854775808) as int
      ;; value in struct literal (overflows).
      #:tests? (target-x86-64?)
      #:import-path "github.com/prometheus/procfs"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'unpack-testdata
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "./ttar" "-C" "testdata/" "-x" "-f" "testdata/fixtures.ttar"))))
          ;; XXX: Replace when go-build-system supports nested path.
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./...")))))
          (add-after 'check 'remove-testdata
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "testdata")))))))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-golang-org-x-sync
           go-golang-org-x-sys))
    (synopsis "Go library for reading @file{/proc}")
    (home-page "https://github.com/prometheus/procfs")
    (description
     "The @code{procfs} Go package provides functions to retrieve system,
kernel, and process metrics from the @file{/proc} pseudo file system.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-sigv4
  (package
    (name "go-github-com-prometheus-sigv4")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/sigv4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yw37lw4x2l20l02i6yd4m4x948vgrfyaa0csl155rdyq3ynwa5w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/prometheus/sigv4"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-aws-aws-sdk-go
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-common
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/prometheus/sigv4")
    (synopsis "HTTP signed requests with Amazon's Signature Verification V4")
    (description
     "sigv4 provides a @code{http.RoundTripper} that will sign requests using
Amazon's Signature Verification V4 signing procedure, using credentials from
the default AWS credential chain.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-statsd-exporter
  (package
    (name "go-github-com-prometheus-statsd-exporter")
    (version "0.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/statsd_exporter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h7ypmsx1j6x1p5wdj03i3jzwms7ab03asn2capl1gg6x07k57w7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/prometheus/statsd_exporter"
      #:embed-files #~(list "landing_page.css" "landing_page.html")))
    (native-inputs
     (list go-github-com-stvp-go-udp-testing))
    (propagated-inputs
     (list go-github-com-alecthomas-kingpin-v2
           go-github-com-go-kit-log
           go-github-com-golang-groupcache
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-client-model
           go-github-com-prometheus-common
           go-github-com-prometheus-exporter-toolkit
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/prometheus/statsd_exporter")
    (synopsis "StatsD to Prometheus metrics exporter")
    (description
     "The StatsD exporter is a drop-in replacement for
@url{https://github.com/statsd/statsd,StatsD}.  The exporter translates StatsD
metrics to Prometheus metrics via configured mapping rules.  This package
provides a Golang module and @code{statsd_exporter} executable command.")
    (license license:asl2.0)))

;;;
;;; Executables:
;;;

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
