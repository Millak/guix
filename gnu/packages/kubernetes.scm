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


;;;
;;; Executables:
;;;

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order:
;;; guix import --insert=gnu/packages/python-xyz.scm pypi <package-name>.
;;;
