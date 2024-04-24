;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020-2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Adam Kandur <kefironpremise@gmail.com>
;;; Copyright © 2022 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2022 jgart via Guix-patches via <guix-patches@gnu.org>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022, 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023 Fries <fries1234@protonmail.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Thomas Ieong <th.ieong@free.fr>
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages golang-web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages ipfs)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

;;; Commentary:
;;;
;;; Golang modules (libraries) related to HTML, CSS, SCSS, JavaScript, JSON,
;;; Web-framework, REST-API or similar functionality. They may provide
;;; executables and libraries, for which there are marked sections.

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-cloud-google-com-go-compute-metadata
  (package
    (name "go-cloud-google-com-go-compute-metadata")
    (version "0.81.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15jgynqb5pbxqbj3a7ii970yn4srsw1dbxzxnhpkfkmplalpgyh3"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "cloud.google.com/go"
       #:import-path "cloud.google.com/go/compute/metadata"))
    (home-page
     "https://pkg.go.dev/cloud.google.com/go/compute/metadata")
    (synopsis
     "Go wrapper for Google Compute Engine metadata service")
    (description
     "This package provides access to Google Compute Engine (GCE) metadata and
API service accounts for Go.")
    (license license:asl2.0)))

(define-public go-github-com-alexliesenfeld-health
  (package
    (name "go-github-com-alexliesenfeld-health")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alexliesenfeld/health")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fchlvxwidsscskwq07vhxfwcn5wbigbizi51619l8gg09mr158q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alexliesenfeld/health"
      #:phases #~(modify-phases %standard-phases
                   ;; Examples requires additional dependencies and comes with
                   ;; their own go.mod, consider to pack it as separate
                   ;; package if required.
                   (add-after 'unpack 'remove-examples
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively
                        (string-append "src/" import-path "/examples")))))))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/alexliesenfeld/health")
    (synopsis "Simple and flexible health check library for Go")
    (description
     "This library provides a @code{http.Handler} that acts as a health
endpoint.  It can be used by cloud infrastructure or other services to
determine the availability of an application.

Rather than simply returning a response with HTTP status code 200, this
library allows building health checks that test the availability of all
required dependencies.  The HTTP response contains the aggregated health
result and details about the health status of each component.")
    (license license:expat)))

(define-public go-github-com-andybalholm-cascadia
  (package
    (name "go-github-com-andybalholm-cascadia")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andybalholm/cascadia")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zgc9fjkn7d66cnmgnmalr9lrq4ii1spap95pf2x1hln4pflib5s"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/andybalholm/cascadia"))
    (native-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/andybalholm/cascadia/")
    (synopsis "CSS selectors for HTML")
    (description "The Cascadia package implements CSS selectors for use with
the parse trees produced by the html package.")
    (license license:bsd-2)))

(define-public go-github-com-aws-sdk
  (package
    (name "go-github-com-aws-sdk")
    (version "1.35.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ky5lw2s2zpslnnqcs6hgsrwvwbxwgflb5jwf16dd4aga3vrg10c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go/aws"
       #:unpack-path "github.com/aws/aws-sdk-go"))
    (propagated-inputs
     (list go-github-com-go-sql-driver-mysql
           go-github-com-jmespath-go-jmespath
           go-github-com-pkg-errors
           go-golang-org-x-net))
    (home-page "https://github.com/aws/aws-sdk-go")
    (synopsis "Library to access Amazon Web Services (AWS)")
    (description
     "This is the official AWS SDK for the Go programming language.")
    (license license:asl2.0)))

;; XXX: This package might be a duplicate of go-github-com-aws-sdk, it's not
;; in use anywhere. Keep it here for the farther review.
(define-public go-github-com-aws-aws-sdk-go
  (package
    (name "go-github-com-aws-aws-sdk-go")
    (version "1.36.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "169mkkw1cff1px6326krwvfpfj07sb4y5rbn003gi4bk176h6ry9"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-github-com-jmespath-go-jmespath))
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go"
       #:phases %standard-phases))
    (synopsis "The official AWS SDK for the Go programming language")
    (description
     "The official AWS SDK for the Go programming language.")
    (home-page "https://github.com/aws/aws-sdk-go")
    (license license:asl2.0)))

(define-public go-github-com-aws-aws-sdk-go-v2
  (package
    (name "go-github-com-aws-aws-sdk-go-v2")
    (version "1.17.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit "v1.17.3")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a07xab1cn96iff7zvp5a82fzhqwl0i4bhplkm2h1qbkxgldn6x0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-aws-smithy-go
           go-github-com-google-go-cmp-cmp
           go-github-com-jmespath-go-jmespath))
    (home-page "https://github.com/aws/aws-sdk-go-v2")
    (synopsis "AWS SDK for Go v2")
    (description
     "Package sdk is the official AWS SDK v2 for the Go programming language.")
    (license license:asl2.0)))

(define-public go-github-com-aws-aws-sdk-go-v2-config
  (package
    (inherit go-github-com-aws-aws-sdk-go-v2)
    (name "go-github-com-aws-aws-sdk-go-v2-config")
    (version "1.18.5")
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go-v2/config"
       #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-google-go-cmp-cmp
           go-github-com-aws-smithy-go))))

(define-public go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
  (package
    (inherit go-github-com-aws-aws-sdk-go-v2)
    (name "go-github-com-aws-aws-sdk-go-v2-feature-s3-manager")
    (version "1.11.44")
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go-v2/feature/s3/manager"
       #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs (list go-github-com-aws-smithy-go))))

(define-public go-github-com-aws-aws-sdk-go-v2-service-iam
  (package
    (inherit go-github-com-aws-aws-sdk-go-v2)
    (name "go-github-com-aws-aws-sdk-go-v2-service-iam")
    (version "1.44.161")
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go-v2/service/iam"
       #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-aws-smithy-go))))

(define-public go-github-com-aws-aws-sdk-go-v2-service-s3
  (package
    (inherit go-github-com-aws-aws-sdk-go-v2)
    (name "go-github-com-aws-aws-sdk-go-v2-service-s3")
    (version "1.30.0")
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go-v2/service/s3"
       #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-aws-smithy-go))))

(define-public go-github-com-aws-aws-sdk-go-v2-service-sso
  (package
    (inherit go-github-com-aws-aws-sdk-go-v2)
    (name "go-github-com-aws-aws-sdk-go-v2-service-sso")
    (version "1.11.27")
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go-v2/service/sso"
       #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-aws-smithy-go))))

(define-public go-github-com-aws-aws-sdk-go-v2-service-ssooidc
  (package
    (inherit go-github-com-aws-aws-sdk-go-v2)
    (name "go-github-com-aws-aws-sdk-go-v2-service-ssooidc")
    (version "1.13.10")
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go-v2/service/ssooidc"
       #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-aws-smithy-go))))

(define-public go-github-com-aws-aws-sdk-go-v2-service-sts
  (package
    (inherit go-github-com-aws-aws-sdk-go-v2)
    (name "go-github-com-aws-aws-sdk-go-v2-service-sts")
    (version "1.17.7")
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go-v2/service/sts"
       #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-aws-smithy-go))))

(define-public go-github-com-aws-smithy-go
  (package
    (name "go-github-com-aws-smithy-go")
    (version "1.13.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/smithy-go")
             (commit "v1.13.5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rgyk0m2d3agknnlzjqvac1a61wwdq1pbck7vyl587m38n5zi2cz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/aws/smithy-go"))
    (propagated-inputs
     (list go-github-com-jmespath-go-jmespath go-github-com-google-go-cmp-cmp))
    (home-page "https://github.com/aws/smithy-go")
    (synopsis "@url{https://smithy.io/2.0/index.html,Smithy} code generators
for Go")
    (description
     "Package smithy provides the core components for a Smithy SDK.")
    (license license:asl2.0)))

(define-public go-github-com-aymerick-douceur
  (package
    (name "go-github-com-aymerick-douceur")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aymerick/douceur/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hfysznib0fqbp8vqxpk0xiggpp0ayk2bsddi36vbg6f8zq5f81n"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/aymerick/douceur"))
    (native-inputs
     (list go-github-com-andybalholm-cascadia
           go-github-com-gorilla-css
           go-github-com-puerkitobio-goquery
           go-golang-org-x-net))
    (home-page "https://github.com/aymerick/douceur/")
    (synopsis "CSS parser and inliner")
    (description "This package provides a CSS parser and inliner.")
    (license license:expat)))

(define-public go-github-com-bep-golibsass
  (package
    (name "go-github-com-bep-golibsass")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bep/golibsass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xk3m2ynbydzx87dz573ihwc4ryq0r545vz937szz175ivgfrhh3"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "libsass_src")
           #t))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bep/golibsass/libsass"
       #:unpack-path "github.com/bep/golibsass"
       ;; The dev build tag modifies the build to link to system libsass
       ;; instead of including the bundled one (which we remove.)
       ;; https://github.com/bep/golibsass/blob/v0.7.0/internal/libsass/a__cgo_dev.go
       #:build-flags '("-tags" "dev")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-bindings
           ;; Generate bindings for system libsass, replacing the
           ;; pre-generated bindings.
           (lambda* (#:key inputs unpack-path #:allow-other-keys)
             (mkdir-p (string-append "src/" unpack-path "/internal/libsass"))
             (let ((libsass-src (string-append (assoc-ref inputs "libsass-src") "/src")))
               (substitute* (string-append "src/" unpack-path "/gen/main.go")
                 (("filepath.Join\\(rootDir, \"libsass_src\", \"src\"\\)")
                  (string-append "\"" libsass-src "\""))
                 (("../../libsass_src/src/")
                  libsass-src)))
             (invoke "go" "generate" (string-append unpack-path "/gen"))
             #t))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (if tests?
                 (invoke "go" "test" import-path "-tags" "dev"))
             #t)))))
    (propagated-inputs
     (list libsass))
    (native-inputs
     `(("go-github-com-frankban-quicktest" ,go-github-com-frankban-quicktest)
       ("libsass-src" ,(package-source libsass))))
    (home-page "https://github.com/bep/golibsass")
    (synopsis "Easy to use Go bindings for LibSass")
    (description
     "This package provides SCSS compiler support for Go applications.")
    (license license:expat)))

(define-public go-github-com-chris-ramon-douceur
  (package
    (name "go-github-com-chris-ramon-douceur")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chris-ramon/douceur")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hfysznib0fqbp8vqxpk0xiggpp0ayk2bsddi36vbg6f8zq5f81n"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/chris-ramon/douceur"))
    (propagated-inputs
     (list go-github-com-aymerick-douceur
           go-github-com-gorilla-css))
    (native-inputs
     (list go-github-com-puerkitobio-goquery
           go-github-com-andybalholm-cascadia
           go-golang-org-x-net))
    (home-page "https://github.com/chris-ramon/douceur/")
    (synopsis "CSS parser and inliner")
    (description "This package provides a CSS parser and inliner.")
    (license license:expat)))

(define-public go-github-com-coreos-go-oidc
  (package
    (name "go-github-com-coreos-go-oidc")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coreos/go-oidc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11m6slbpi33ynffml7812piq4anhjlf1qszjlsf26f5y7x3qh8n5"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/coreos/go-oidc"))
    (propagated-inputs
     (list go-github-com-pquerna-cachecontrol
           go-golang-org-x-oauth2
           go-gopkg-in-square-go-jose-v2))
    (home-page "https://github.com/coreos/go-oidc")
    (synopsis "OpenID Connect support for Go")
    (description
     "This package enables OpenID Connect support for the
@code{go-golang-org-x-oauth2} package.")
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-oidc-v3
  (package
    (inherit go-github-com-coreos-go-oidc)
    (name "go-github-com-coreos-go-oidc-v3")
    (version "3.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coreos/go-oidc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sbm6n3lp48lymn0g921afhq2j6inb38w3wy5rhyx9h8gpzhnxx9"))))
    (arguments
     (list ;; no Go files in [...]/src/github.com/coreos/go-oidc/v3.
      #:import-path "github.com/coreos/go-oidc/v3/oidc"
      #:unpack-path "github.com/coreos/go-oidc/v3"))
    (propagated-inputs
     (list go-github-com-go-jose-go-jose-v3
           go-golang-org-x-oauth2))))

(define-public go-github-com-emicklei-go-restful
  (package
    (name "go-github-com-emicklei-go-restful")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emicklei/go-restful")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m1y5a6xr6hmdj77afrvyh2llkbhn1166lcrgis654shl8zs9qhz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/emicklei/go-restful"))
    (home-page "https://github.com/emicklei/go-restful")
    (synopsis "Build REST-style web services using Go")
    (description "This package provides @code{go-restful}, which helps
developers to use @code{http} methods explicitly and in a way that's
consistent with the HTTP protocol definition.")
    (license license:expat)))

(define-public go-github-com-felixge-httpsnoop
  (package
    (name "go-github-com-felixge-httpsnoop")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/felixge/httpsnoop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ncd8lar5zxiwjhsp315s4hsl4bhnm271h49jhyxc66r5yffgmac"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/felixge/httpsnoop"))
    (home-page "https://github.com/felixge/httpsnoop/")
    (synopsis "Capture http related metrics")
    (description
     "Httpsnoop provides an easy way to capture http related
metrics (i.e. response time, bytes written, and http status code) from your
application's http.Handlers.")
    (license license:expat)))

;; This project looks like domain or abandoned, see
;; <https://github.com/francoispqt/gojay/issues/150>.
(define-public go-github-com-francoispqt-gojay
  (package
    (name "go-github-com-francoispqt-gojay")
    (version "1.2.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/francoispqt/gojay")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ix95qdyajfmxhf9y52vjrih63f181pjs4v5as8905s4d5vmkd06"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Disable failing tests on non-x86-64 architecture, see
      ;; <https://github.com/francoispqt/gojay/issues/173>.
      #:tests? (and (not (%current-target-system))
                    (target-x86-64?))
      #:import-path "github.com/francoispqt/gojay"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/francoispqt/gojay")
    (synopsis "JSON encoder/decoder with powerful stream API for Golang")
    (description
     "GoJay is a performant JSON encoder/decoder for Golang.  It has a simple
API and doesn't use reflection.  It relies on small interfaces to
decode/encode structures and slices.")
    (license license:expat)))

;; TODO: This repository has been archived by the owner on Aug 30, 2023. It is
;; now read-only. The raven-go SDK is no longer maintained and was superseded
;; by the sentry-go
(define-public go-github-com-getsentry-raven-go
  (let ((commit "5c24d5110e0e198d9ae16f1f3465366085001d92")
        (revision "0"))
    (package
      (name "go-github-com-getsentry-raven-go")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/getsentry/raven-go")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0lvc376sq8r8jhy2v1m6rf1wyld61pvbk0x6j9xpg56ivqy69xs7"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/getsentry/raven-go"))
      (propagated-inputs
       (list go-github-com-certifi-gocertifi go-github-com-pkg-errors))
      (home-page "https://github.com/getsentry/raven-go")
      (synopsis "Sentry client in Go")
      (description "This package is a Go client API for the Sentry event/error
logging system.")
      (license license:bsd-3))))

(define-public go-github-com-go-chi-chi-v5
  (package
    (name "go-github-com-go-chi-chi-v5")
    (version "5.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-chi/chi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rzrsxz4xj0973c6nxklvq2vmg2m795snhk25836i0gnd1jnx79k"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-chi/chi/v5"))
    (home-page "https://github.com/go-chi/chi")
    (synopsis "Composable router for HTTP services written in Go")
    (description
     "@code{go-github-com-go-chi-chi-v5} is an HTTP router that lets the user
decompose request handling into many smaller layers.")
    (license license:expat)))

(define-public go-github-com-go-jose-go-jose-v3
  (package
    (name "go-github-com-go-jose-go-jose-v3")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-jose/go-jose")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fnw0p49wc9gmd2xcji2x9jf97dgg9igagd5m6bmq3nw9jjfqdc5"))))
    (build-system go-build-system)
    (arguments
     '( #:import-path "github.com/go-jose/go-jose/v3"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (native-inputs
     (list go-github-com-google-go-cmp-cmp
           go-github-com-stretchr-testify))
    (home-page "https://github.com/go-jose/go-jose")
    (synopsis "Implementation of JOSE standards (JWE, JWS, JWT) in Go")
    (description
     "This package provides a Golang implementation of the Javascript Object
Signing and Encryption set of standards.  This includes support for JSON Web
Encryption, JSON Web Signature, and JSON Web Token standards.")
    (license license:asl2.0)))

(define-public go-github-com-go-telegram-bot-api-telegram-bot-api
  (package
    (name "go-github-com-go-telegram-bot-api-telegram-bot-api")
    (version "4.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-telegram-bot-api/telegram-bot-api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x6j0k3aiicsr8l53na99ci10zm3qpn2syz4f60fzh164w5k1l7w"))))
    (build-system go-build-system)
    (home-page "https://go-telegram-bot-api.dev/")
    (arguments
     (list #:tests? #f                  ; Upstream tests are broken.
           #:import-path "github.com/go-telegram-bot-api/telegram-bot-api"))
    (propagated-inputs
     (list go-github-com-technoweenie-multipartstreamer))
    (synopsis "Golang bindings for the Telegram Bot API")
    (description
     "This package provides Golang bindings for the Telegram Bot API.")
    (license license:expat)))

(define-public go-github-com-goccy-go-json
  (package
    (name "go-github-com-goccy-go-json")
    (version "0.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/goccy/go-json")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1krid2hlvs808jl9zmv7m6zx92samc60gymhwr2mwwykicwbnks8"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "benchmarks"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/goccy/go-json"))
    (home-page "https://github.com/goccy/go-json")
    (synopsis "JSON encoder/decoder in Go")
    (description
     "Fast JSON encoder/decoder compatible with encoding/json for Go.")
    (license license:expat)))

(define-public go-github-com-golang-groupcache
  (let ((commit "41bb18bfe9da5321badc438f91158cd790a33aa3")
        (revision "3"))
    (package
      (name "go-github-com-golang-groupcache")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/golang/groupcache")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "07amgr8ji4mnq91qbsw2jlcmw6hqiwdf4kzfdrj8c4b05w4knszc"))))
      (build-system go-build-system)
      (arguments
       (list #:import-path "github.com/golang/groupcache"))
      (propagated-inputs
       (list go-github-com-golang-protobuf-proto))
      (home-page "https://github.com/golang/groupcache")
      (synopsis "Groupcache is a caching and cache-filling library")
      (description
       "Groupcache is a caching and cache-filling library, intended
as a replacement for memcached in many cases.  It provides a data loading
mechanism with caching and de-duplication that works across a set of peer
processes.")
      (license license:asl2.0))))

(define-public go-github-com-google-go-github
  (package
    (name "go-github-com-google-go-github")
    (version "26.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-github")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x0zz1vcmllp6r6l2qin9b2llm5cxbf6n84rf99h8wrmhvzs2ipi"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ;application/octet-stream instead of text/plain
       #:import-path "github.com/google/go-github/v26/github"
       #:unpack-path "github.com/google/go-github/v26"))
    (native-inputs
     (list go-github-com-google-go-querystring go-golang-org-x-crypto))
    (home-page "https://github.com/google/go-github/")
    (synopsis "Client library for accessing the GitHub API v3")
    (description "@code{go-github} is a Go client library for accessing the
GitHub API v3.")
    (license license:bsd-3)))

;; For chezmoi-1.8.10
(define-public go-github-com-google-go-github-v33
  (package
    (inherit go-github-com-google-go-github)
    (name "go-github-com-google-go-github-v33")
    (version "33.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-github")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nzwgvaa9k1ky3sfynib6nhalam9dx66h5lxff334m9kk3rf5nn0"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-google-go-github)
       ((#:unpack-path _ "github.com/google/go-github/v26")
        "github.com/google/go-github/v33")
       ((#:import-path _ "github.com/google/go-github/v26/github")
        "github.com/google/go-github/v33/github")))))

(define-public go-github-com-google-safehtml
  (package
    (name "go-github-com-google-safehtml")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/safehtml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j2xjy8xrk9y9k6bqpvimj84i6hg1wwsyvwsb0axhmp49cmnrp86"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/safehtml"))
    (propagated-inputs
     (list go-golang-org-x-text))
    (home-page "https://github.com/google/safehtml")
    (synopsis "Safe HTML for Go")
    (description
     "Package safehtml provides immutable string-like types which represent
values that are guaranteed to be safe, by construction or by escaping or
sanitization, to use in various HTML contexts and with various DOM APIs.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-context
  (let ((commit "08b5f424b9271eedf6f9f0ce86cb9396ed337a42")
        (revision "0"))
    (package
      (name "go-github-com-gorilla-context")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gorilla/context")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03p4hn87vcmfih0p9w663qbx9lpsf7i7j3lc7yl7n84la3yz63m4"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/gorilla/context"))
      (home-page "https://github.com/gorilla/context")
      (synopsis "Go registry for request variables")
      (description
       "This package provides @code{gorilla/context}, which is a general
purpose registry for global request variables in the Go programming
language.")
      (license license:bsd-3))))

(define-public go-github-com-gorilla-csrf
  (package
    (name "go-github-com-gorilla-csrf")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/csrf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iryq0z48yi7crfbd8jxyn7lh1gsglpiglvjgnf23bz6xfisssav"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("github.com/gorilla/securecookie" ,go-github-com-gorilla-securecookie)
       ("github.com/pkg/errors" ,go-github-com-pkg-errors)))
    (arguments
     '(#:import-path "github.com/gorilla/csrf"))
    (home-page "https://github.com/gorilla/csrf")
    (synopsis "Cross Site Request Forgery (CSRF) prevention middleware")
    (description
     "Gorilla/csrf provides Cross Site Request Forgery (CSRF) prevention
middleware for Go web applications and services.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-css
  (package
    (name "go-github-com-gorilla-css")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/css")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "116fhy3n7bsq3psyn4pa0i4x9zy916kh1zxslmbbp0p9l4i7ysrj"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/css/scanner"
       #:unpack-path "github.com/gorilla/css"))
    (home-page "https://github.com/gorilla/css/")
    (synopsis "CSS3 tokenizer")
    (description "This package provides a CSS3 tokenizer.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-handlers
  (package
    (name "go-github-com-gorilla-handlers")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/handlers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15gycdz9lkjnsvvichsbdf25vf6pi1sfn41khhz53iqf300l0w0s"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f                      ; Tries to download from the internet
       #:import-path "github.com/gorilla/handlers"))
    (propagated-inputs
     (list go-github-com-felixge-httpsnoop))
    (home-page "https://github.com/gorilla/handlers")
    (synopsis "Middleware for Go HTTP services and web applications")
    (description "A collection of useful middleware for Go HTTP services and
web applications.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-mux
  (package
    (name "go-github-com-gorilla-mux")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/mux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18f0q9qxgq1yh4ji07mqhiydfcwvi56z9d775v7dc7yckj33kpdk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/mux"))
    (home-page "https://github.com/gorilla/mux")
    (synopsis "URL router and dispatcher for Go")
    (description
     "Gorilla/Mux implements a request router and dispatcher for matching
incoming requests with their respective handler.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-securecookie
  (package
    (name "go-github-com-gorilla-securecookie")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/securecookie")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16bqimpxs9vj5n59vm04y04v665l7jh0sddxn787pfafyxcmh410"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/securecookie"))
    (home-page "https://github.com/gorilla/securecookie")
    (synopsis "Encodes and decodes authenticated and optionally encrypted
cookie values")
    (description
     "Gorilla/securecookie encodes and decodes authenticated and optionally
encrypted cookie values for Go web applications.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-sessions
  (package
    (name "go-github-com-gorilla-sessions")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/sessions")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zjw2s37yggk9231db0vmgs67z8m3am8i8l4gpgz6fvlbv52baxp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/sessions"))
    (propagated-inputs
     (list go-github-com-gorilla-securecookie))
    (home-page "https://github.com/gorilla/sessions")
    (synopsis "Manage user sessions in web applications")
    (description
     "This package that provides infrastructure for creating and managing user
sessions in web applications.  It supports cookie and filesystem-based
sessions, flash messages, custom backends, and more.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-websocket
  (package
    (name "go-github-com-gorilla-websocket")
    (version "1.5.0")
    (home-page "https://github.com/gorilla/websocket")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xrr6snvs9g1nzxxg05w4i4pq6k1xjljl5mvavd838qc468n118i"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/websocket"))
    (synopsis "Fast WebSocket implementation for Go")
    (description "Gorilla WebSocket is a Go implementation of the WebSocket
protocol.")
    (license license:bsd-2)))

(define-public go-github-com-gregjones-httpcache
  (let ((commit "901d90724c7919163f472a9812253fb26761123d")
        (revision "0"))
    (package
      (name "go-github-com-gregjones-httpcache")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gregjones/httpcache")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "05r0xq51vfb55di11m7iv19341d73f7in33vq1ihcqs1nffdwiq0"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/gregjones/httpcache"))
      (home-page "https://github.com/gregjones/httpcache")
      (synopsis "Transport for @code{http.Client} that will cache responses")
      (description
       "Package @code{httpcache} provides a @code{http.RoundTripper}
implementation that works as a mostly @url{https://tools.ietf.org/html/rfc7234, RFC 7234}
compliant cache for HTTP responses.  It is only suitable for use as a
\"private\" cache (i.e. for a web-browser or an API-client and not for a
shared proxy).")
      (license license:expat))))

(define-public go-github-com-hashicorp-go-cleanhttp
  (package
    (name "go-github-com-hashicorp-go-cleanhttp")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-cleanhttp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i5xslizzwd966w81bz6dxjwzgml4q9bwqa186bsxd1vi8lqxl9p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-cleanhttp"))
    (home-page "https://github.com/hashicorp/go-cleanhttp")
    (synopsis "Functions for accessing clean Go @code{http.Client} values")
    (description
     "The Go standard library contains a default @code{http.Client} and it is
a common idiom to tweak it as necessary.  Unfortunately, this is a shared
value, and it is not uncommon for libraries to assume that they are free to
modify it at will.  This package provides some simple functions to get a
\"clean\" @code{http.Client}, namely one that uses the same default values as
the Go standard library, but returns a client that does not share any state
with other clients.")
    (license license:mpl2.0)))

(define-public go-github-com-hjson-hjson-go
  (package
    (name "go-github-com-hjson-hjson-go")
    (version "4.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hjson/hjson-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "138vmbnrwzxf64cia27k407clrydvs2jx927dlv6ziydiqyvy7m3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/hjson/hjson-go"))
    (home-page "https://hjson.org/")
    (synopsis "Human JSON implementation for Go")
    (description "Hjson is a syntax extension to JSON.  It is intended to be
used like a user interface for humans, to read and edit before passing the
JSON data to the machine.")
    (license license:expat)))

(define-public go-github-com-jackpal-go-nat-pmp
  (package
    (name "go-github-com-jackpal-go-nat-pmp")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackpal/go-nat-pmp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p2yrzfbkazc9nisr2iqjwzhb6q16zj6finyxxn2ikk7iiighl1g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackpal/go-nat-pmp"))
    (home-page "https://github.com/jackpal/go-nat-pmp")
    (synopsis "Port mapping and discovery of external IP address")
    (description
     "This package provides a Go client for the NAT-PMP internet protocol for
port mapping and discovering the external IP address of a firewall.")
    (license license:asl2.0)))

(define-public go-github-com-jcmturner-dnsutils-v2
  (package
    (name "go-github-com-jcmturner-dnsutils-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jcmturner/dnsutils")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "116zbgvfj88vv93fnapmmgyd5g8kzy774cdyzsnnzyzng92j61c9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jcmturner/dnsutils/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jcmturner/dnsutils")
    (synopsis "Go library with DNS utils")
    (description
     "The dnsutils package provides a Go function to return a map of Service
Records (SRV) in the order they should be used for a given service, protocol
and name.  The order is determined by the records' priority and randomized
selection based on their relative weighting.  This package is useful for
network applications that require accessing services using SRV records.")
    (license license:asl2.0)))

(define-public go-github-com-jcmturner-goidentity-v6
  (package
    (name "go-github-com-jcmturner-goidentity-v6")
    (version "6.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jcmturner/goidentity")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "064ysvxvrvij843s7qj1nkzl5qc6j1qbrsb3s0zmwd1sa7vq8q1n"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jcmturner/goidentity/v6"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-hashicorp-go-uuid))
    (home-page "https://github.com/jcmturner/goidentity")
    (synopsis "Hold authenticated identities and their attributes")
    (description "This package provides a standard interface for holding
authenticated identities and their attributes.")
    (license license:asl2.0)))

(define-public go-github-com-jcmturner-gokrb5-v8
  (package
    (name "go-github-com-jcmturner-gokrb5-v8")
    (version "8.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jcmturner/gokrb5")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w9d1pa3r6qmdblk25bghf78ncs03l15l1sxnh4n536c356rzq4b"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jcmturner/gokrb5/v8"
       #:unpack-path "github.com/jcmturner/gokrb5"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-gorilla-sessions
           go-github-com-hashicorp-go-uuid
           go-github-com-jcmturner-aescts-v2
           go-github-com-jcmturner-dnsutils-v2
           go-github-com-jcmturner-gofork
           go-github-com-jcmturner-goidentity-v6
           go-github-com-jcmturner-rpc-v2-mstypes
           go-github-com-jcmturner-rpc-v2-ndr
           go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://github.com/jcmturner/gokrb5")
    (synopsis "Pure Go Kerberos library for clients and services")
    (description "This package provides a pure Go Kerberos library.  It
features:
@itemize
@item Kerberos libraries for custom integration
@item Parsing Keytab files
@item Parsing krb5.conf files
@item Parsing client credentials cache files such as /tmp/krb5cc_$(id -u $(whoami))
@end itemize

On the client side, it provides a client that can authenticate to an SPNEGO
Kerberos authenticated web service, and the ability to change client's
password.

On the server side, the library provides a HTTP handler wrapper implements
SPNEGO Kerberos authentication, as well as a HTTP handler wrapper decodes
Microsoft AD PAC authorization data.")
    (license license:asl2.0)))

(define-public go-github-com-jmespath-go-jmespath
  (package
    (name "go-github-com-jmespath-go-jmespath")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jmespath/go-jmespath")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18zyr9nlywmwp3wpzcjxrgq9s9d2mmc6zg6xhsna00m663nkyc3n"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jmespath/go-jmespath"))
    (native-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-pmezard-go-difflib
           go-github-com-stretchr-objx
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/jmespath/go-jmespath")
    (synopsis "Golang implementation of JMESPath")
    (description
     "This package implements JMESPath, a query language for JSON.  It
transforms one JSON document into another through a JMESPath expression.")
    (license license:asl2.0)))

(define-public go-github-com-json-iterator-go
  (package
    (name "go-github-com-json-iterator-go")
    (version "1.1.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/json-iterator/go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c8f0hxm18wivx31bs615x3vxs2j3ba0v6vxchsjhldc8kl311bz"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/json-iterator/go"))
    (native-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-google-gofuzz
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-modern-go-concurrent
           go-github-com-modern-go-reflect2))
    (home-page "https://github.com/json-iterator/go")
    (synopsis "High-performance replacement for Golang @code{encoding/json}")
    (description
     "This package implements encoding and decoding of JSON as defined in
@uref{https://rfc-editor.org/rfc/rfc4627.html,RFC 4627} and provides
interfaces with identical syntax of standard lib encoding/json.  Converting
from encoding/json to jsoniter is no more than replacing the package with
jsoniter and variable type declarations (if any).  jsoniter interfaces gives
100% compatibility with code using standard lib.")
    (license license:expat)))

(define-public go-github-com-julienschmidt-httprouter
  (package
    (name "go-github-com-julienschmidt-httprouter")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/julienschmidt/httprouter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a6sy0ysqknsjssjh7qg1dqn21xmj9a36c57nrk7srfmab4ffmk1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/julienschmidt/httprouter"))
    (home-page "https://github.com/julienschmidt/httprouter")
    (synopsis "High performance HTTP request router")
    (description
     "Package @code{httprouter} is a trie based high performance HTTP request
router.")
    (license license:bsd-3)))

(define-public go-github-com-microcosm-cc-bluemonday
  (package
    (name "go-github-com-microcosm-cc-bluemonday")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microcosm-cc/bluemonday")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "071ph097c1iwbcc33x6kblj9rxb1r4mp3qfkrj4qw5mg7qcqxydk"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/microcosm-cc/bluemonday"))
    (propagated-inputs
     (list go-github-com-chris-ramon-douceur
           go-golang-org-x-net))
    (home-page "https://github.com/microcosm-cc/bluemonday/")
    (synopsis "HTML sanitizer")
    (description "@code{bluemonday} is a HTML sanitizer implemented in Go.")
    (license license:bsd-3)))

(define-public go-github-com-multiformats-go-multiaddr
  (package
    (name "go-github-com-multiformats-go-multiaddr")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-multiaddr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cdzlzh7cb1pj9mhq45va3r6gs6pcdfa9j7vdrqlv3zd6k3bxg39"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-multiaddr"))
    (propagated-inputs
     (list go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/multiformats/go-multiaddr")
    (synopsis "Composable and future-proof network addresses")
    (description
     "Multiaddr is a standard way to represent addresses that does the
following:

@itemize
@item Support any standard network protocols.
@item Self-describe (include protocols).
@item Have a binary packed format.
@item Have a nice string representation.
@item Encapsulate well.
@end itemize")
    (license license:expat)))

;; It's for the Kubo update; remove it when it is no longer needed.
(define-public go-github-com-multiformats-go-multiaddr-0.12
  (package
    (inherit go-github-com-multiformats-go-multiaddr)
    (name "go-github-com-multiformats-go-multiaddr")
    (version "0.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-multiaddr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rn02yn7494r7ayn585bbsddprbn8wdccxs4n2k5dmll4dyd39mp"))))
    (arguments
     (list
      #:go go-1.21
      #:import-path "github.com/multiformats/go-multiaddr"))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs (list go-github-com-ipfs-go-cid
                             go-github-com-multiformats-go-multibase
                             go-github-com-multiformats-go-varint
                             go-github-com-multiformats-go-multihash-0.2.3
                             go-golang-org-x-exp-2023))))

(define-public go-github-com-multiformats-go-multiaddr-dns
  (package
    (name "go-github-com-multiformats-go-multiaddr-dns")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-multiaddr-dns")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17qpcgxlmji6wdnjabl5ihc4zn69w2g0karad46zj70y5zg4y24r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.21
      #:import-path "github.com/multiformats/go-multiaddr-dns"
      #:unpack-path "github.com/multiformats/go-multiaddr-dns"))
    (propagated-inputs
     (list go-github-com-miekg-dns
           go-github-com-multiformats-go-multiaddr-0.12))
    (home-page "https://multiformats.io/multiaddr/")
    (synopsis "Library and CLI tool for DNS multiaddr resolution")
    (description
     "Go library for /dns4, /dns6, /dnsaddr multiaddr resolution.")
    (license license:expat)))

(define-public go-github-com-multiformats-go-multiaddr-net
  ;; This commit is from <2018-10-01> and associated with GX package manager,
  ;; since that time the project has changed versing stile and GX is dropped.
  ;; The latest versioned tag is v0.1.5 <2020-04-30>. The project is archved
  ;; on <2021-10-05>.
  (let ((commit "1cb9a0e8a6de3c8a10f6cee60d01d793603c4f7e")
        (revision "0"))
    (package
      (name "go-github-com-multiformats-go-multiaddr-net")
      (version (git-version "1.6.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiformats/go-multiaddr-net")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ypgi47xdz3bh8lh7f8cmk7w3ql9g4izx5l3kzdg9gda1xn5zxq3"))))
      (build-system go-build-system)
      (arguments
       (list
        ;; TODO: Tests fail because they try to access the network.
        #:tests? #f
        #:import-path "github.com/multiformats/go-multiaddr-net"))
      (propagated-inputs
       (list go-github-com-multiformats-go-multiaddr))
      (home-page "https://github.com/multiformats/go-multiaddr-net")
      (synopsis "Multiaddress net tools")
      (description
       "This package provides Multiaddr specific versions of common functions
in stdlib's @command{net} package.  This means wrappers of standard net
symbols like @command{net.Dial} and @command{net.Listen}, as well as
conversion to and from @command{net.Addr}.")
      (license license:expat))))

(define-public go-github-com-nwidger-jsoncolor
  (package
    (name "go-github-com-nwidger-jsoncolor")
    (version "0.3.0")
    (home-page "https://github.com/nwidger/jsoncolor")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13rd146pnj7qm70r1333gyd1f61x40nafxlpvdxlci9h7mx8c5p8"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/nwidger/jsoncolor"))
    (native-inputs
     (list go-github-com-fatih-color))
    (synopsis "Colorized JSON marshalling and encoding")
    (description
     "@code{jsoncolor} is a drop-in replacement for @code{encoding/json}'s
@code{Marshal} and @code{MarshalIndent} functions and @code{Encoder} type
which produce colorized output using github.com/fatih/color.")
    (license license:expat)))

(define-public go-github-com-opentracing-opentracing-go
  (package
    (name "go-github-com-opentracing-opentracing-go")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/opentracing/opentracing-go")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04rgdwl29kimp2wnm4dycnzp7941hvpj6wym85x23c6fclacm94h"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/opentracing/opentracing-go"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/opentracing/opentracing-go")
    (synopsis "OpenTracing API for Go")
    (description "OpenTracing-Go is a Go implementation of the OpenTracing API.")
    (license license:asl2.0)))

(define-public go-github-com-pkg-sftp
  (package
    (name "go-github-com-pkg-sftp")
    (version "1.13.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pkg/sftp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n35lzfrnrffjqy34ny6gxs27kq81s67ply6q8s1g19mhfzm6my7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pkg/sftp"))
    (propagated-inputs (list go-golang-org-x-crypto go-github-com-kr-fs))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/pkg/sftp")
    (synopsis "SFTP implementation for Go")
    (description
     "This package provides an @acronym{SFTP, SSH File Transfer Protocol}
implementation, as described in
@url{https://filezilla-project.org/specs/draft-ietf-secsh-filexfer-02.txt},
for Go.")
    (license license:bsd-2)))

(define-public go-github-com-pquerna-cachecontrol
  (package
    (name "go-github-com-pquerna-cachecontrol")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pquerna/cachecontrol")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d5zgv2w0sinh9m41pw3n015zzyabk7awgwwga7nmhjz452c9r5n"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/pquerna/cachecontrol"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/pquerna/cachecontrol")
    (synopsis "Golang HTTP Cache-Control Parser and Interpretation")
    (description
     "This package implements RFC 7234 Hypertext Transfer Protocol (HTTP/1.1):
Caching.")
    (license license:asl2.0)))

(define-public go-github-com-puerkitobio-goquery
  (package
    (name "go-github-com-puerkitobio-goquery")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PuerkitoBio/goquery")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gh1d99l5xc9hvwa4j40pfq3y9vfyq52mnrz6bf1kw2r2zr2gbcc"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/PuerkitoBio/goquery"))
    (propagated-inputs
     (list go-github-com-andybalholm-cascadia go-golang-org-x-net))
    (home-page "https://github.com/PuerkitoBio/goquery")
    (synopsis "Features similar to jQuery to the Go language")
    (description "@code{goquery} brings a syntax and a set of features similar
to jQuery to the Go language.")
    (license license:bsd-3)))

(define-public go-github-com-quic-go-qpack
  (package
    (name "go-github-com-quic-go-qpack")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quic-go/qpack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00mjz445hhx4yar5l8p21bpp4d06jyg2ajw0ax7bh64d37l4kx39"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests require ginkgo v2.
      #:tests? #f
      #:go go-1.20
      #:import-path "github.com/quic-go/qpack"))
    (propagated-inputs
     (list go-github-com-onsi-ginkgo
           go-github-com-onsi-gomega
           go-golang-org-x-net))
    (home-page "https://github.com/quic-go/qpack")
    (synopsis "Minimal QPACK (RFC 9204) implementation for Go")
    (description
     "A minimal QPACK (RFC 9204) implementation in Go.  It is minimal in the sense
that it doesn't use the dynamic table at all, but just the static table and (Huffman
encoded) string literals.  Wherever possible, it reuses code from the
@url{https://github.com/golang/net/tree/master/http2/hpack, HPACK implementation in
the Go standard library}.")
    (license license:expat)))

(define-public go-github-com-quic-go-quic-go
  (package
    (name "go-github-com-quic-go-quic-go")
    (version "0.39.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quic-go/quic-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0acabl3cz48nxpggc5s7fwxpmr5amyi09jygn5m5xxkkbhqs2cxq"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX More packages required...
      #:tests? #f
      #:go go-1.20
      #:import-path "github.com/quic-go/quic-go"))
    (propagated-inputs
     (list go-github-com-cheekybits-genny
           go-github-com-golang-protobuf-proto
           go-github-com-marten-seemann-chacha20
           go-github-com-quic-go-qpack
           go-github-com-quic-go-qtls-go1-20
           go-golang-org-x-crypto
           go-golang-org-x-exp
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys))
    (home-page "https://github.com/quic-go/quic-go")
    (synopsis "QUIC in Go")
    (description
     "This package provides a Go language implementation of the QUIC network
protocol.")
    (license license:expat)))

(define-public go-github-com-lucas-clemente-quic-go
  (deprecated-package "go-github-com-lucas-clemente-quic-go" go-github-com-quic-go-quic-go))

(define-public go-github-com-sourcegraph-jsonrpc2
  (package
    (name "go-github-com-sourcegraph-jsonrpc2")
    (version "0.1.0")
    (home-page "https://github.com/sourcegraph/jsonrpc2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dk0w32k96vxrwnmm24wqx337dn8ylch65qwrbc3wh7whw2xx71q"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/sourcegraph/jsonrpc2"))
    (propagated-inputs
     (list go-github-com-daviddengcn-go-colortext
           go-github-com-gorilla-websocket
           go-github-com-motemen-go-colorine))
    (synopsis "Provides a client and server implementation of JSON-RPC 2.0")
    (description
     "Package jsonrpc2 provides a Go implementation of JSON-RPC 2.0.")
    (license license:expat)))

(define-public go-github-com-tdewolff-minify-v2
  (package
    (name "go-github-com-tdewolff-minify-v2")
    (version "2.12.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/minify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qhslaq885zbqs83nvbi29yh09b89kkb6ycami8lz28wkwrlayap"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tdewolff/minify/v2"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'regenerate-hash
            (lambda* (#:key import-path #:allow-other-keys)
              (for-each
               (lambda (dir)
                 (with-directory-excursion
                     (format #f "src/~a/~a" import-path dir)
                   (make-file-writable "hash.go")
                   (format #t "Generating `hash.go' for ~a...~%" dir)
                   (invoke "go" "generate")))
               '("css" "html" "svg")))))))
    (propagated-inputs
     (list go-github-com-tdewolff-parse-v2))
    (native-inputs
     (list go-github-com-tdewolff-hasher
           go-github-com-tdewolff-test))
    (home-page "https://go.tacodewolff.nl/minify")
    (synopsis "Go minifiers for web formats")
    (description
     "This package provides HTML5, CSS3, JS, JSON, SVG and XML minifiers and
an interface to implement any other minifier.")
    (license license:expat)))

(define-public go-github-com-tdewolff-parse-v2
  (package
    (name "go-github-com-tdewolff-parse-v2")
    (version "2.6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/parse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dqki9ima079k9a3l72igmx5dml8qsl9z8rzw8a433f4gjhlv320"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tdewolff/parse/v2"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'regenerate-hash
            (lambda* (#:key import-path #:allow-other-keys)
              (for-each
               (lambda (dir)
                 (with-directory-excursion
                     (format #f "src/~a/~a" import-path dir)
                   (make-file-writable "hash.go")
                   (format #t "Generating `hash.go' for ~a...~%" dir)
                   (invoke "go" "generate")))
               '("css" "html")))))))
    (native-inputs
     (list go-github-com-tdewolff-hasher
           go-github-com-tdewolff-test))
    (home-page "https://github.com/tdewolff/parse")
    (synopsis "Go parsers for web formats")
    (description
     "This package contains several lexers and parsers written in Go.")
    (license license:expat)))

(define-public go-github-com-tv42-httpunix
  (let ((commit "2ba4b9c3382c77e7b9ea89d00746e6111d142a22")
        (revision "0"))
    (package
      (name "go-github-com-tv42-httpunix")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tv42/httpunix")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xbwpip2hsfhd2kd878jn5ndl8y1i9658lggha4x3xb5m1rsds9w"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/tv42/httpunix"))
      (home-page "https://github.com/tv42/httpunix")
      (synopsis "Go library to talk HTTP over Unix domain sockets")
      (description "This package is a Go library to talk HTTP over Unix domain
sockets.")
      (license license:expat))))

(define-public go-github-com-ugorji-go-codec
  (package
    (name "go-github-com-ugorji-go-codec")
    (version "1.2.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ugorji/go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11j0sd7kli2bh2npfr2znnvdjsk118rs8khqzfdp6pb5jm0l20ib"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ugorji/go/codec"
      #:unpack-path "github.com/ugorji/go"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'remove-benchmarks
                     (lambda* (#:key import-path #:allow-other-keys)
                       (delete-file-recursively (string-append "src/"
                                                               import-path
                                                               "/bench")))))))
    (propagated-inputs (list go-golang-org-x-tools))
    (home-page "https://github.com/ugorji/go")
    (synopsis "Codec and encoding library for various serialization formats")
    (description
     "This package provides a high performance and feature rich codec and
encoding library for the MessagePack, CBOR, JSON and the Binc formats.")
    (license license:expat)))

(define-public go-github-com-valyala-fasthttp
  (package
    (name "go-github-com-valyala-fasthttp")
    (version "1.39.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/fasthttp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12qwx0yk7wjj25v4fswgmj28r69gk94kqdmzavca8k9f0yznniz1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/valyala/fasthttp"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (unless
                  ;; Tests hang forever with gccgo.
                  (false-if-exception (search-input-file inputs "/bin/gccgo"))
                (apply (assoc-ref %standard-phases 'check) args)))))))
    (propagated-inputs
     (list go-github-com-andybalholm-brotli
           go-github-com-klauspost-compress
           go-github-com-valyala-bytebufferpool
           go-github-com-valyala-tcplisten
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-text))
    (home-page "https://github.com/valyala/fasthttp")
    (synopsis "Provides fast HTTP server and client API")
    (description
     "This package provides a Go module @code{fasthttp} which may be used as
replacement for native @code{net/http} module.")
    (license license:expat)))

(define-public go-github-com-whyrusleeping-json-filter
  (let ((commit "ff25329a9528f01c5175414f16cc0a6a162a5b8b")
        (revision "0"))
    (package
      (name "go-github-com-whyrusleeping-json-filter")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/whyrusleeping/json-filter")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0cai0drvx4c8j686l908vpcsz3mw3vxi3ziz94b0f3c5ylpj07j7"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/whyrusleeping/json-filter"))
      (home-page "https://github.com/whyrusleeping/json-filter")
      (synopsis "Library to query JSON objects marshalled into map[string]interface")
      (description "A library to query JSON objects marshalled into
@command{map[string]interface{}}.")
      (license license:expat))))

(define-public go-github-com-xeipuuv-gojsonpointer
  (let ((commit "4e3ac2762d5f479393488629ee9370b50873b3a6")
        (revision "0"))
    (package
      (name "go-github-com-xeipuuv-gojsonpointer")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xeipuuv/gojsonpointer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13y6iq2nzf9z4ls66bfgnnamj2m3438absmbpqry64bpwjfbsi9q"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/xeipuuv/gojsonpointer"))
      (home-page "https://github.com/xeipuuv/gojsonpointer")
      (synopsis "Implementation of JSON Pointer for Go")
      (description
       "This package provides an implementation of JSON Pointer for the Go
programming language.")
      (license license:asl2.0))))

(define-public go-github-com-xeipuuv-gojsonreference
  (let ((commit "bd5ef7bd5415a7ac448318e64f11a24cd21e594b")
        (revision "0"))
    (package
      (name "go-github-com-xeipuuv-gojsonreference")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xeipuuv/gojsonreference")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xby79padc7bmyb8rfbad8wfnfdzpnh51b1n8c0kibch0kwc1db5"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/xeipuuv/gojsonreference"))
      (propagated-inputs
       (list go-github-com-xeipuuv-gojsonpointer))
      (home-page "https://github.com/xeipuuv/gojsonreference")
      (synopsis "Implementation of JSON Reference for Go")
      (description
       "This package provides an implementation of JSON Reference for the Go
programming language.")
      (license license:asl2.0))))

(define-public go-github-com-xeipuuv-gojsonschema
  (let ((commit "6b67b3fab74d992bd07f72550006ab2c6907c416")
        (revision "0"))
    (package
      (name "go-github-com-xeipuuv-gojsonschema")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xeipuuv/gojsonschema")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q937a6q7canlr3dllqdw0qwa6z2fpwn1w9kycavx8jmwh6q3f69"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/xeipuuv/gojsonschema"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'disable-failing-tests
             (lambda* (#:key import-path #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 (substitute* "schema_test.go"
                   (("\\{\"phase\": \"remote ref, " all)
                    (string-append "// " all))
                   (("\\{\"phase\": \"valid definition" all)
                    (string-append "// " all))
                   (("\\{\"phase\": \"invalid definition" all)
                    (string-append "// " all)))))))))
      (native-inputs
       (list go-github-com-stretchr-testify))
      (propagated-inputs
       (list go-github-com-xeipuuv-gojsonreference
             go-github-com-xeipuuv-gojsonpointer))
      (home-page "https://github.com/xeipuuv/gojsonschema")
      (synopsis "Implementation of JSON Schema for Go")
      (description
       "This package provides an implementation of JSON Schema for the Go
programming language, which supports draft-04, draft-06 and draft-07.")
      (license license:asl2.0))))

(define-public go-gopkg-in-square-go-jose-v2
  (package
    (name "go-gopkg-in-square-go-jose-v2")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/square/go-jose")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b1nhqxfmhzwrfk7pkvp2w3z3d0pf5ir00vizmy2d4xdbnldn70r"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "gopkg.in/square/go-jose.v2"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (native-inputs
     (list go-github-com-google-go-cmp-cmp
           go-github-com-stretchr-testify))
    (home-page "https://gopkg.in/square/go-jose.v2")
    (synopsis "Implementation of JOSE standards (JWE, JWS, JWT) in Go")
    (description
     "This package aims to provide an implementation of the Javascript Object
Signing and Encryption set of standards.  This includes support for JSON Web
Encryption, JSON Web Signature, and JSON Web Token standards.")
    (license license:asl2.0)))

;;;
;;; Executables:
;;;

(define-public go-madns
  (package
    (inherit go-github-com-multiformats-go-multiaddr-dns)
    (name "go-madns")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-multiformats-go-multiaddr-dns)
       ((#:install-source? _ #t) #f)
       ((#:import-path _ "github.com/multiformats/go-multiaddr-dns")
        "github.com/multiformats/go-multiaddr-dns/madns")))
    (description
     "This package provides a CLI binary executible built from
go-github-com-multiformats-go-multiaddr-dns.")))

(define-public go-minify
  (package
    (inherit go-github-com-tdewolff-minify-v2)
    (name "go-minify")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-tdewolff-minify-v2)
       ((#:install-source? _ #t) #f)
       ((#:import-path _ "github.com/tdewolff/minify/v2")
        "github.com/tdewolff/minify/cmd/minify")))
    (inputs
     (list go-github-com-djherbis-atime
           go-github-com-dustin-go-humanize
           go-github-com-fsnotify-fsnotify
           go-github-com-matryer-try
           go-github-com-spf13-pflag))
    (description "This package provides a CLI binary executible built from
go-github-com-tdewolff-minify-v2 source.")))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
