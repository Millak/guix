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
  #:use-module (gnu packages golang-crypto)
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

(define-public go-go-etcd-io-etcd-server-v3
  (package
    (name "go-go-etcd-io-etcd-server-v3")
    (version "3.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etcd-io/etcd")
             (commit (go-version->git-ref version
                                          #:subdir "server"))))
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
                    (delete-all-but "." "server")
                    (rename-file "server" "server.tmp")
                    (mkdir-p "server/v3")
                    (rename-file "server.tmp" "server/v3")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f ;Setup fails
      #:import-path "go.etcd.io/etcd/server/v3"
      #:unpack-path "go.etcd.io/etcd"))
    (propagated-inputs
     (list go-github-com-coreos-go-semver
           go-github-com-coreos-go-systemd-v22
           go-github-com-dustin-go-humanize
           go-github-com-gogo-protobuf
           go-github-com-golang-groupcache
           go-github-com-golang-jwt-jwt-v5
           go-github-com-golang-protobuf
           go-github-com-google-btree
           go-github-com-google-go-cmp
           go-github-com-grpc-ecosystem-go-grpc-middleware
           go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus
           go-github-com-grpc-ecosystem-grpc-gateway-v2
           go-github-com-jonboulle-clockwork
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-client-model
           go-github-com-soheilhy-cmux
           go-github-com-spf13-cobra
           go-github-com-stretchr-testify
           go-github-com-tmc-grpc-websocket-proxy
           go-github-com-xiang90-probing
           go-go-etcd-io-bbolt
           go-go-etcd-io-etcd-client-v3
           go-go-etcd-io-etcd-pkg-v3
           go-go-etcd-io-raft-v3
           go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
           go-go-opentelemetry-io-otel-sdk
           go-go-uber-org-zap
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-time
           go-google-golang-org-genproto-googleapis-api
           go-google-golang-org-grpc
           go-google-golang-org-protobuf
           go-gopkg-in-natefinch-lumberjack-v2
           go-sigs-k8s-io-json
           go-sigs-k8s-io-yaml))
    (home-page "https://go.etcd.io/etcd")
    (synopsis "Server package for ETCD")
    (description
     "This package provides a server for the ETCD distributed key-value storage
system.")
    (license license:asl2.0)))


(define-public go-go-etcd-io-etcd-pkg-v3
  (package
    (name "go-go-etcd-io-etcd-pkg-v3")
    (version "3.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/etcd-io/etcd")
              (commit (go-version->git-ref version
                                           #:subdir "pkg"))))
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
                    (delete-all-but "." "pkg")
                    (rename-file "pkg" "pkg.tmp")
                    (mkdir-p "pkg/v3")
                    (rename-file "pkg.tmp" "pkg/v3")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f       ;TODO: Tests hang, select some subset
      #:import-path "go.etcd.io/etcd/pkg/v3"
      #:unpack-path "go.etcd.io/etcd"))
    (native-inputs
     (list go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-creack-pty
           go-github-com-dustin-go-humanize
           go-go-etcd-io-etcd-client-pkg-v3
           go-go-uber-org-zap
           go-google-golang-org-grpc))
    (home-page "https://github.com/etcd-io/etcd")
    (synopsis "Utility packages for etcd")
    (description
     "This package is a collection of utility packages used by etcd without
being specific to etcd itself.")
    (license license:asl2.0)))

(define-public go-k8s-io-apiserver
  (package
    (name "go-k8s-io-apiserver")
    (version "0.34.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/apiserver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06bcbdnlhg1mwky6i1519n1nhlh8rxc9qv7jwqjhc03m2ql3bb8q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ;TODO: Tests require unpackaged dependencies
      #:embed-files
      #~(list "authoring\\.tmpl" "api__v1_openapi\\.json")
      #:import-path "k8s.io/apiserver"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-blang-semver-v4
           go-github-com-coreos-go-oidc
           go-github-com-coreos-go-systemd-v22
           go-github-com-emicklei-go-restful-v3
           go-github-com-fsnotify-fsnotify
           go-github-com-go-logr-logr
           go-github-com-gogo-protobuf
           go-github-com-google-btree
           go-github-com-google-cel-go
           go-github-com-google-gnostic-models
           go-github-com-google-go-cmp
           go-github-com-google-uuid
           go-github-com-gorilla-websocket
           go-github-com-grpc-ecosystem-go-grpc-prometheus
           go-github-com-munnerz-goautoneg
           go-github-com-mxk-go-flowrate
           go-github-com-spf13-pflag
           go-go-etcd-io-etcd-api-v3
           go-go-etcd-io-etcd-client-pkg-v3
           go-go-etcd-io-etcd-client-v3
           go-go-etcd-io-etcd-server-v3
           go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
           go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
           go-go-opentelemetry-io-otel-metric
           go-go-opentelemetry-io-otel-sdk
           go-go-opentelemetry-io-otel-trace
           go-go-uber-org-zap
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-time
           go-google-golang-org-genproto-googleapis-api
           go-google-golang-org-grpc
           go-google-golang-org-protobuf
           go-gopkg-in-evanphx-json-patch-v4
           go-gopkg-in-go-jose-go-jose-v2
           go-gopkg-in-natefinch-lumberjack-v2
           go-k8s-io-api
           go-k8s-io-apimachinery
           go-k8s-io-client-go
           go-k8s-io-component-base
           go-k8s-io-klog-v2
           go-k8s-io-kms
           go-k8s-io-kube-openapi
           go-k8s-io-utils
           go-sigs-k8s-io-apiserver-network-proxy-konnectivity-client
           go-sigs-k8s-io-json
           go-sigs-k8s-io-randfill
           go-sigs-k8s-io-structured-merge-diff-v6
           go-sigs-k8s-io-yaml))
    (home-page "https://github.com/kubernetes/apiserver")
    (synopsis "Library for building a Kubernetes aggregated API server")
    (description
     "This package provides a generic library for building a Kubernetes
aggregated API server.")
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
