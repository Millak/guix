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
  #:use-module (gnu packages)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
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
    (version "1.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/client_golang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mx5q221pbkx081ycf1lp8sxz513220ya8qczkkvab943cwlcarv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/prometheus/client_golang"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples-and-tutorials
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "api/prometheus/v1/example_test.go"
                                "examples"
                                "tutorial")))))
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-beorn7-perks
           go-github-com-cespare-xxhash-v2
           go-github-com-davecgh-go-spew
           go-github-com-json-iterator-go
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
    (version "0.55.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/common")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bsbxil7qz8rhckhv0844nmn38g7i7347cjv5m6na47hbdpi0rqh"))
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
      #:import-path "github.com/prometheus/common"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v"
                          ;; "./config/..." requries
                          ;; <github.com/prometheus/client_golang/prometheus>,
                          ;; which introduce cycle.
                          "./expfmt/..."
                          "./helpers/..."
                          "./model/..."
                          "./promlog/..."
                          "./route/..."
                          "./server/..."))))))))
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
    (synopsis "Prometheus metrics")
    (description
     "This package provides tools for reading and writing Prometheus
metrics.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-common-assets
  (package
    (name "go-github-com-prometheus-common-assets")
    (version "0.55.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/common")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bsbxil7qz8rhckhv0844nmn38g7i7347cjv5m6na47hbdpi0rqh"))))
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

(define-public go-github-com-prometheus-common-sigv4
  (package
    (name "go-github-com-prometheus-common-sigv4")
    (version "0.55.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/common")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bsbxil7qz8rhckhv0844nmn38g7i7347cjv5m6na47hbdpi0rqh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/prometheus/common/sigv4"
      #:unpack-path "github.com/prometheus/common"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'unpack 'override-prometheus-common
            (lambda _
              (delete-file-recursively "src/github.com/prometheus/common"))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-aws-aws-sdk-go
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-common
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/prometheus/common")
    (synopsis "HTTP signed requests with Amazon's Signature Verification V4")
    (description
     "This package provides a @code{http.RoundTripper} that will sign requests
using Amazon's Signature Verification V4 signing procedure, using credentials
from the default AWS credential chain.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-procfs
  (package
    (name "go-github-com-prometheus-procfs")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/procfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "116ns8k1yjdj9a2vq5czlpmafrhy0yw5y0bcm1qqbqnn57agg68m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/prometheus/procfs"
      ;; The tests require Go modules, which are not yet supported in Guix's
      ;; Go build system.
      #:tests? #f))
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

;;;
;;; Executables:
;;;

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
