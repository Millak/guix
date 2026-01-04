;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Arthur Rodrigues <arthurhdrodrigues@proton.me>
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

(define-module (gnu packages kubernetes)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages prometheus))


;;;
;;; Libraries:
;;;

(define-public go-go-etcd-io-etcd-client-v3
  (package
    (name "go-go-etcd-io-etcd-client-v3")
    (version "3.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etcd-io/etcd")
             (commit (go-version->git-ref version #:subdir "client"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d9rjyl5h0xm9isgr8b2fz8528wk3pds71rjl8g08fgsmsa5kicb"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet #~(begin
                    (define (delete-all-but directory . preserve)
                      (with-directory-excursion directory
                        (let* ((pred (negate (cut member <>
                                                  (cons* "." ".." preserve))))
                               (items (scandir "." pred)))
                          (for-each (cut delete-file-recursively <>) items))))
                    ;; Replace symlinks to tests with file contents
                    (for-each
                     (lambda (f)
                       (delete-file (string-append "client/v3/" f))
                       (copy-file (string-append
                                   "tests/integration/clientv3/examples/" f)
                                  (string-append "client/v3/" f)))
                     (list "example_auth_test.go"
                           "example_cluster_test.go"
                           "example_kv_test.go"
                           "example_lease_test.go"
                           "example_maintenance_test.go"
                           "example_metrics_test.go"
                           "example_test.go"
                           "example_watch_test.go"))
                    (for-each
                     (lambda (f)
                       (delete-file
                        (string-append "client/v3/concurrency/" f))
                       (copy-file
                        (string-append "tests/integration/clientv3/concurrency/" f)
                        (string-append "client/v3/concurrency/" f)))
                     (list "example_election_test.go"
                           "example_mutex_test.go"
                           "example_stm_test.go"))
                    ;; Copy sertificates for tests.
                    (mkdir-p "client/v3/tests/fixtures")
                    (substitute* "client/v3/yaml/config_test.go"
                      (("\\.\\./\\.\\./\\.\\./") "../"))
                    (for-each
                     (lambda (f)
                       (copy-file
                        (string-append "tests/fixtures/" f)
                        (string-append "client/v3/tests/fixtures/" f)))
                     (list "server.crt"
                           "server.key.insecure"
                           "ca.crt"))
                    ;; Keep source related to expected import-path.
                    (delete-all-but "." "client")
                    (delete-file-recursively "client/pkg")))))
    (build-system go-build-system)
    (arguments
     (list
      ;; TODO: Tests a shaky and fail a lot, check how run unittests.
      #:tests? #f 
      #:import-path "go.etcd.io/etcd/client/v3"
      #:unpack-path "go.etcd.io/etcd"))
    (native-inputs
     (list go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus
           go-github-com-prometheus-client-golang
           go-github-com-stretchr-testify
           go-go-etcd-io-etcd-client-pkg-v3
           go-go-uber-org-zap
           go-google-golang-org-grpc))
    (propagated-inputs
     (list go-github-com-coreos-go-semver
           go-github-com-dustin-go-humanize
           go-github-com-grpc-ecosystem-go-grpc-middleware-v2
           go-go-etcd-io-etcd-api-v3
           go-sigs-k8s-io-yaml))
    (home-page "https://github.com/etcd-io/etcd")
    (synopsis "Golang client for ETCD")
    (description
     "This package implements the official Go client for etcd.")
    (license license:asl2.0)))

(define-public go-k8s-io-component-base
  (package
    (name "go-k8s-io-component-base")
    (version "0.34.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kubernetes/component-base")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "090ghb66zh4mln9fvp89vfq0g4pysm5y4lrp5n6801491mngyndm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-skip" "Tracing")
      #:import-path "k8s.io/component-base"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-blang-semver-v4
           go-github-com-go-logr-logr
           go-github-com-go-logr-zapr
           go-github-com-google-go-cmp
           go-github-com-moby-term
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-client-model
           go-github-com-prometheus-common
           go-github-com-prometheus-procfs
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
           go-go-opentelemetry-io-otel-sdk
           go-go-opentelemetry-io-otel-trace
           go-go-uber-org-zap
           go-go-yaml-in-yaml-v2
           go-golang-org-x-sys
           go-golang-org-x-text
           go-k8s-io-apimachinery
           go-k8s-io-client-go
           go-k8s-io-klog-v2
           go-k8s-io-utils
           go-sigs-k8s-io-json))
    (home-page "https://github.com/kubernetes/component-base")
    (synopsis "Kubernetes core components Golang source code")
    (description
     "This package contains shared code for Kubernetes core components.")
    (license license:asl2.0)))

(define-public go-sigs-k8s-io-apiserver-network-proxy-konnectivity-client
  (package
    (name "go-sigs-k8s-io-apiserver-network-proxy-konnectivity-client")
    (version "0.34.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kubernetes-sigs/apiserver-network-proxy")
              (commit (go-version->git-ref version
                                           #:subdir "konnectivity-client"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kh92blfpmrmklxqg5j0p07yz534gjzhm44xj8bh5pad3y5zw5m0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "sigs.k8s.io/apiserver-network-proxy/konnectivity-client"
      #:unpack-path "sigs.k8s.io/apiserver-network-proxy"))
    (native-inputs
     (list go-go-uber-org-goleak))
    (propagated-inputs
     (list go-github-com-prometheus-client-golang
           go-google-golang-org-grpc
           go-google-golang-org-protobuf
           go-k8s-io-klog-v2))
    (home-page "https://sigs.k8s.io/apiserver-network-proxy")
    (synopsis "Network proxy for Kubernetes API server")
    (description
     "This package provides a network proxy for the Kubernetes API server.")
    (license license:asl2.0)))


;;;
;;; Executables:
;;;

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order:
;;; guix import --insert=gnu/packages/python-xyz.scm pypi <package-name>.
;;;
