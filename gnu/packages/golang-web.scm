;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 (unmatched-parenthesis <paren@disroot.org>
;;; Copyright © 2022 Adam Kandur <kefironpremise@gmail.com>
;;; Copyright © 2022 Dhruvin Gandhi <contact@dhruvin.dev>
;;; Copyright © 2022 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2022 Leo Nikkilä <hello@lnikki.la>
;;; Copyright © 2022 jgart via Guix-patches via <guix-patches@gnu.org>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022, 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Filip Lajszczak <filip@lajszczak.dev>
;;; Copyright © 2023 Fries <fries1234@protonmail.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Thomas Ieong <th.ieong@free.fr>
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Dominic Martinez <dom@dominicm.dev>
;;; Copyright © 2024 Jesse Eisses <jesse@eisses.email>
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
;;; Golang modules (libraries) for Web related projects: HTML, CSS, SCSS,
;;; JavaScript, JSON, Web-framework, REST-API or similar functionality; for
;;; Network related projects: OSI layers implementation algorithms, MIME,
;;; Email protocols implementations, and similar.  They may provide
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

(define-public go-git-sr-ht-emersion-gqlclient
  (package
    (name "go-git-sr-ht-emersion-gqlclient")
    (version "0.0.0-20230820050442-8873fe0204b9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~emersion/gqlclient")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x64kcryawdr0daq1w6fada60zqrddw75yi397835b9ij7wb5gmh"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "git.sr.ht/~emersion/gqlclient"))
    (home-page "https://git.sr.ht/~emersion/gqlclient")
    (synopsis "GraphQL client and code generator")
    (description
     "This package provides a GraphQL client and code generator for Go.")
    (license license:expat)))

(define-public go-git-sr-ht-rockorager-go-jmap
  (package
    (name "go-git-sr-ht-rockorager-go-jmap")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~rockorager/go-jmap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r8bmdlmvpk08i7xrqwgv0aaz05564wgcyji73nszdh2s32m4kzl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~rockorager/go-jmap"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (native-inputs
     (list
      go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-oauth2))
    (home-page "https://git.sr.ht/~rockorager/go-jmap")
    (synopsis "JSON meta application protocol in Golang")
    (description
     "Package jmap implements JMAP Core protocol as defined in
@@url{https://rfc-editor.org/rfc/rfc8620.html,RFC 8620} published on July
2019.")
    (license license:expat)))

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

(define-public go-github-com-anaskhan96-soup
  (package
    (name "go-github-com-anaskhan96-soup")
    (version "1.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anaskhan96/soup")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s19119sy4zqf05sgpdymcbdaz5bg86n7xwgd2m1vvxjmp485k5p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anaskhan96/soup"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/anaskhan96/soup")
    (synopsis "Web Scraper in Go, similar to BeautifulSoup")
    (description
     "Small web scraper package for Go, with its interface highly similar to
that of BeautifulSoup.")
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

(define-public go-github-com-audriusbutkevicius-pfilter
  (package
    (name "go-github-com-audriusbutkevicius-pfilter")
    (version "0.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AudriusButkevicius/pfilter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03kwi1hnhcz9qdblmhpaqg2063k2ch29hc5dr8cl2z7q5rp81m9i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/AudriusButkevicius/pfilter"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          ;; Remove tests requiring setting up local
                          ;; connection.
                          (list "quic_test.go"))))))))
    (propagated-inputs
     (list go-github-com-pkg-errors
           go-github-com-quic-go-quic-go
           go-golang-org-x-net))
    (home-page "https://github.com/AudriusButkevicius/pfilter")
    (synopsis "Filter packets into multiple virtual connections")
    (description
     "Pfilter is a Go package for filtering packets into multiple virtual
connections from a single physical connection.")
    (license license:expat)))

(define-public go-github-com-aws-aws-lambda-go
  (package
    (name "go-github-com-aws-aws-lambda-go")
    (version "1.47.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-lambda-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xki0n3va9nr6dmlgrb8zarkccx5jba6ig6g8zxcznw3rlllf1zv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-lambda-go"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/aws/aws-lambda-go")
    (synopsis "AWS Lambda for Go")
    (description
     "Libraries, samples, and tools to help Go developers develop AWS Lambda
functions.")
    (license license:asl2.0)))

(define-public go-github-com-aws-aws-sdk-go
  (package
    (name "go-github-com-aws-aws-sdk-go")
    (version "1.55.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wsl1vcig3j9z6v2hppfr1bvrvbisck026fwq2a7yzmx36pwnj6a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  (("TestProcessProviderTimeout")
                   "OffTestProcessProviderTimeout")))))
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-jmespath-go-jmespath))
    (home-page "https://github.com/aws/aws-sdk-go")
    (synopsis "The official AWS SDK for the Go programming language")
    (description
     "The official AWS SDK for the Go programming language.")
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
           go-github-com-google-go-cmp
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
     (list go-github-com-google-go-cmp
           go-github-com-aws-smithy-go))))

(define-public go-github-com-aws-aws-sdk-go-v2-credentials
  (package
    (name "go-github-com-aws-aws-sdk-go-v2-credentials")
    (version "1.17.27")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit (string-append "credentials/v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jdj7wim98g80hjbw3av7ffrr3dqxzbygprmhjs0cxc16cw62wj7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go-v2/credentials"
      #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-aws-smithy-go))
    (home-page "https://pkg.go.dev/github.com/aws/aws-sdk-go-v2/credentials")
    (synopsis "AWS SDK for Go v2 - credentials module")
    (description
     "Package credentials provides types for retrieving credentials from
credentials sources.")
    (license license:asl2.0)))

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
     (list go-github-com-jmespath-go-jmespath go-github-com-google-go-cmp))
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

(define-public go-github-com-azure-go-ntlmssp
  (package
    (name "go-github-com-azure-go-ntlmssp")
    (version "0.0.0-20221128193559-754e69321358")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/go-ntlmssp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dg20fwylf5lpsc5fgnnzw7jxz0885bg97lla1b5wrlhjas6lidn"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; See <https://github.com/Azure/go-ntlmssp/issues/40>.
      #:tests? #f
      #:import-path "github.com/Azure/go-ntlmssp"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/Azure/go-ntlmssp")
    (synopsis "NTLM negotiation in Go")
    (description
     "This package provides @acronym{NT (New Technology) LAN
Manager,NTLM}/Negotiate authentication over HTTP.")
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

(define-public go-github-com-ccding-go-stun
  (package
    (name "go-github-com-ccding-go-stun")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ccding/go-stun")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wjhckyg42kp04njhj7gni84cyk0s7m17n13kqf6r7mjzx8a83pw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ccding/go-stun"))
    (home-page "https://github.com/ccding/go-stun")
    (synopsis "STUN client implementation")
    (description
     "Go-stun is a go implementation of the STUN client (RFC 3489
and RFC 5389).")
    (license license:asl2.0)))

(define-public go-github-com-cenkalti-backoff-v4
  (package
    (name "go-github-com-cenkalti-backoff-v4")
    (version "4.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cenkalti/backoff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pwr7fzxgngb073q98qrz1f90bkk3pljynif6jl5a6q6kcsn7xf1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cenkalti/backoff/v4"))
    (home-page "https://github.com/cenkalti/backoff")
    (synopsis "The exponential backoff algorithm in Go")
    (description "This is a Go port of the exponential backoff algorithm from
@url{https://github.com/google/google-http-java-client/blob/da1aa993e90285ec18579f1553339b00e19b3ab5/google-http-client/src/main/java/com/google/api/client/util/ExponentialBackOff.java,
Google's HTTP Client Library for Java}.

@url{http://en.wikipedia.org/wiki/Exponential_backoff, Exponential backoff} is an
algorithm that uses feedback to multiplicatively decrease the rate of some process,
in order to gradually find an acceptable rate.  The retries exponentially increase
and stop increasing when a certain threshold is met.")
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

(define-public go-github-com-containerd-typeurl
  (package
    (name "go-github-com-containerd-typeurl")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/typeurl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wvfxlxgkln11d9s6rxay965c715bnpk203klbsq8m8qpjqrz620"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/containerd/typeurl"))
    (propagated-inputs
     (list go-github-com-gogo-protobuf
           go-github-com-pkg-errors))
    (home-page "https://github.com/containerd/typeurl")
    (synopsis "Managing marshaled types to @code{protobuf.Any}")
    (description
     "This package implements a functionality of managing the registration,
marshaling, and unmarshaling of encoded types.  It helps when types are sent
over a ttrpc/GRPC API and marshaled as a protobuf
@url{https://pkg.go.dev/google.golang.org/protobuf@@v1.27.1/types/known/anypb#Any,
Any}.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-typeurl-v2
  (package
    (inherit go-github-com-containerd-typeurl)
    (name "go-github-com-containerd-typeurl-v2")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/typeurl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n43s8zqwwrvpzb0pczm73xx4w8yb96ax31cripzxmfhj43z21b5"))))
    (arguments
     (list #:import-path "github.com/containerd/typeurl/v2"))
    (propagated-inputs
     (list go-github-com-gogo-protobuf
           go-google-golang-org-protobuf))))

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
    (version "3.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coreos/go-oidc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00nbv15pjfcfxdy0i4k366ricdm2cylhpwak3hmjlgh6lrzxypl9"))))
    (arguments
     (list
      #:import-path "github.com/coreos/go-oidc/v3"
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
     (list go-github-com-go-jose-go-jose-v4
           go-golang-org-x-net
           go-golang-org-x-oauth2))))

(define-public go-github-com-emersion-go-imap
  (package
    (name "go-github-com-emersion-go-imap")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-imap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ak2ysvfcc9w0g1070msis8x9sh6gzvf0nd65ks594siwbmqddw8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-imap"))
    (propagated-inputs
     (list go-golang-org-x-text
           go-github-com-emersion-go-sasl
           go-github-com-emersion-go-message))
    (home-page "https://github.com/emersion/go-imap")
    (synopsis "IMAP4rev1 library written in Go")
    (description
     "This package provides an IMAP4rev1 library written in Go.  It can be
used to build IMAP clients and servers.")
    (license license:expat)))

;; XXX: This repository has been archived by the owner on Sep 8, 2021. It is
;; now read-only.
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
       (list
        #:import-path "github.com/emersion/go-imap-idle"))
      (propagated-inputs
       (list go-github-com-emersion-go-imap))
      (home-page "https://github.com/emersion/go-imap-idle")
      (synopsis "IDLE extension for go-imap")
      (description "This package provides an IDLE extension for go-imap.")
      (license license:expat))))

(define-public go-github-com-emersion-go-imap-sortthread
  (package
    (name "go-github-com-emersion-go-imap-sortthread")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-imap-sortthread")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cfbgz1l5angnj52v9pxwggai2shx0h78ffcp7j4r4lr7lzflnwz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-imap-sortthread"))
    (propagated-inputs
     (list
      go-github-com-emersion-go-imap))
    (home-page "https://github.com/emersion/go-imap-sortthread")
    (synopsis "Sorting and threading of messages for the imap package")
    (description
     "The sortthread package implements message sorting and threading for
@code{go-github-com-emersion-go-imap}.")
    (license license:expat)))

(define-public go-github-com-emersion-go-maildir
  (package
    (name "go-github-com-emersion-go-maildir")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-maildir")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wa7spn3qa7ipmg29vrimw7phyybyaagdalrjklcazjb6rplvwpl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-maildir"))
    (home-page "https://github.com/emersion/go-maildir")
    (synopsis "Maildir interface for Go")
    (description
     "This package provides an interface to mailboxes in the Maildir format.")
    (license license:expat)))

(define-public go-github-com-emersion-go-mbox
  (package
    (name "go-github-com-emersion-go-mbox")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-mbox")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vnadh2khx7sxn0irrd8gz8ra02x7ij0q8zglq3rqffqil06nliv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-mbox"))
    (home-page "https://github.com/emersion/go-mbox")
    (synopsis "Go library for handling @code{mbox} files")
    (description
     "This package provides a library for parsing and formatting @code{mbox}
files.")
    (license license:expat)))

(define-public go-github-com-emersion-go-message
  (package
    (name "go-github-com-emersion-go-message")
    (version "0.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-message")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gzcgrs5sava8fpybp5cw6f3zqnbz096wf93hcgkrg94wl1g7kqb"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/emersion/go-message"))
    (propagated-inputs
     (list go-golang-org-x-text))
    (home-page "https://github.com/emersion/go-message")
    (synopsis "Internet messages and MIME for Go")
    (description
     "The message package implements the Internet Message Format and
Multipurpose Internet Mail Extensions in Go.")
    (license license:expat)))

(define-public go-github-com-emersion-go-milter
  (package
    (name "go-github-com-emersion-go-milter")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-milter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11qjwjz6ippsx9da81gylx46p1a96mk39j54ayw925m40skqhh3c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-milter"))
    (propagated-inputs
     (list go-github-com-emersion-go-message))
    (home-page "https://github.com/emersion/go-milter")
    (synopsis "Milter mail filters in Go")
    (description
     "This package provides an interface for implementing milter mail filters
for Go.")
    (license license:bsd-2)))

(define-public go-github-com-emersion-go-msgauth
  (package
    (name "go-github-com-emersion-go-msgauth")
    (version "0.6.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-msgauth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0az83i6jmk3bjglgdqw5zsvhh8698rav0mcg4dy8kr0cgq0lj5zs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-msgauth"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Run all tests, workaround for go-build-system's lack of Go
          ;; modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-github-com-emersion-go-milter
           go-github-com-emersion-go-message))
    (home-page "https://github.com/emersion/go-msgauth")
    (synopsis "Email authentication for Go")
    (description
     "This package provides a Go library for authenticating emails.")
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
          (base32 "1cbf86wkqrdinfydndgdlnayg4a5mg3d4vqra377j2sfkg7wj0hs"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/emersion/go-sasl"))
      (home-page "https://github.com/emersion/go-sasl")
      (synopsis "SASL library written in Go")
      (description
       "This package provides a SASL library written in Go.")
      (license license:expat))))

(define-public go-github-com-emersion-go-smtp
  (package
    (name "go-github-com-emersion-go-smtp")
    (version "0.21.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-smtp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0drvmvrkmhqhnv4m3my1hbkyyva2vi35b36j0pdi57xc9rflziq3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-smtp"))
    (propagated-inputs
     (list go-github-com-emersion-go-sasl))
    (home-page "https://github.com/emersion/go-smtp")
    (synopsis "SMTP implementation for Go")
    (description
     "This package implements the Simple Mail Transfer Protocol as defined by
RFC 5321.")
    (license license:expat)))

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

(define-public go-github-com-evanphx-json-patch
  (package
    (name "go-github-com-evanphx-json-patch")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/evanphx/json-patch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00sib9ba8j1h1n3r1cxx48zn8hs6sxwnrh78p6wbs28wcpz8nqxi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/evanphx/json-patch"))
    (propagated-inputs
     (list go-github-com-jessevdk-go-flags go-github-com-pkg-errors))
    (home-page "https://github.com/evanphx/json-patch")
    (synopsis "Apply and create JSON (RFC6902 and RFC7386) patches for Golang")
    (description
     "@code{jsonpatch} is a library which provides functionality for both
applying @url{http://tools.ietf.org/html/rfc6902,RFC6902 JSON patches} against
documents, as well as for calculating & applying
@url{https://tools.ietf.org/html/rfc7396,RFC7396 JSON merge patches}.")
    (license license:bsd-3)))

(define-public go-github-com-felixge-httpsnoop
  (package
    (name "go-github-com-felixge-httpsnoop")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/felixge/httpsnoop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xrvg5ndpz4gv9mf9xl6p6gjmvvv8bbzaspcr070qxx72jhlllkk"))))
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

(define-public go-github-com-gatherstars-com-jwz
  (package
    (name "go-github-com-gatherstars-com-jwz")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gatherstars-com/jwz")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d66axc3504wqpb4axlm8m9jq8rmwndxb4asbqwryymj3yh60cla"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/gatherstars-com/jwz"))
    (propagated-inputs
     (list go-github-com-rivo-tview
           go-github-com-jhillyerd-enmime
           go-github-com-gdamore-tcell-v2))
    (home-page "https://github.com/gatherstars-com/jwz")
    (synopsis "Email threading algorithm in Golang")
    (description
     "The jwz package provides an implementation of the email threading
algorithm originally designed for use in
@url{https://www.jwz.org/doc/threading.html,Netscape Mail 2.0 and 3.0} for
Golang.")
    (license license:asl2.0)))

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
    (version "3.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-jose/go-jose")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kbkplhzqv9ai28r4smhdsxxwh20d96srr3am37pwwnh48ivwch8"))))
    (build-system go-build-system)
    (arguments
     '( #:import-path "github.com/go-jose/go-jose/v3"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-stretchr-testify))
    (home-page "https://github.com/go-jose/go-jose")
    (synopsis "Implementation of JOSE standards (JWE, JWS, JWT) in Go")
    (description
     "This package provides a Golang implementation of the Javascript Object
Signing and Encryption set of standards.  This includes support for JSON Web
Encryption, JSON Web Signature, and JSON Web Token standards.")
    (license license:asl2.0)))

(define-public go-github-com-go-jose-go-jose-v4
  (package
    (inherit go-github-com-go-jose-go-jose-v3)
    (name "go-github-com-go-jose-go-jose-v4")
    (version "4.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-jose/go-jose")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bf444687q5rdxjgk41bkryhzhx49f6600b7i51m572xdl0r28a9"))))
    (arguments
     (list
      #:import-path "github.com/go-jose/go-jose/v4"))))

(define-public go-github-com-go-ldap-ldap
  (package
    (name "go-github-com-go-ldap-ldap")
    (version "3.4.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ldap/ldap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fbmhlc8ss5vn6zz0iiifvy4pm0mwaf13qpz70k83mmnv9vrv16x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-ldap/ldap/v3"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          ;; FAIL <...> LDAP Result Code 200 "Network Error":
                          ;; dial tcp: lookup ldap.itd.umich.edu on <...>
                          (list "ldap_test.go"))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-azure-go-ntlmssp
           go-github-com-go-asn1-ber-asn1-ber
           go-github-com-google-uuid))
    (home-page "https://github.com/go-ldap/ldap")
    (synopsis "LDAP v3 functionality for Go")
    (description "This package provides basic LDAP v3 functionality in the Go
language.")
    (license license:expat)))

(define-public go-github-com-go-openapi-analysis
  (package
    (name "go-github-com-go-openapi-analysis")
    (version "0.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/analysis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1sn6fzjv83y31b8lky0wh08xl8yj60y04jcidzcy5gmknavyfi"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Introduce cycle with go-github-com-go-openapi-loads.
                 (delete-file-recursively "analysis_test")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/analysis"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'disable-failing-tests
            (lambda* (#:key tests? unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; Tests requiring network access.
                  (("TestFlatten_RemoteAbsolute")
                   "OffTestFlatten_RemoteAbsolute")))))
          ;; FIXME: pattern schemas/*.json: cannot embed irregular file
          ;; schemas/jsonschema-draft-04.json
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
                  #$(this-package-input "go-github-com-go-openapi-spec")
                  "/src/github.com")
                 "vendor/github.com"))))
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./...")))))
          (add-before 'install 'remove-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "vendor")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-openapi-jsonpointer
           go-github-com-go-openapi-spec
           go-github-com-go-openapi-strfmt
           go-github-com-go-openapi-swag))
    (home-page "https://github.com/go-openapi/analysis")
    (synopsis "OpenAPI specification object model analyzer")
    (description
     "This package provides a foundational library to analyze an
@acronym{OpenAPI Initiative,OAI} specification document for easier reasoning
about the content.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-errors
  (package
    (name "go-github-com-go-openapi-errors")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/errors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nikzvknkv0nqdy44dfi096lcvkjnpjfrpg1gqlkg5ffccvdnd9s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/errors"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/errors")
    (synopsis "OpenAPI toolkit common errors")
    (description
     "Shared errors and error interface used throughout the various libraries
found in the go-openapi toolkit.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-jsonpointer
  (package
    (name "go-github-com-go-openapi-jsonpointer")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/jsonpointer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17gb0ab2r61j4gqbpwgpz6cvf9jy91xwn11gkbg3b1rq797if7vc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/jsonpointer"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-openapi-swag))
    (home-page "https://github.com/go-openapi/jsonpointer")
    (synopsis "JSON Pointer with structs")
    (description
     "This package provides an implementation of JSON Pointer, initially
prototyped in @url{https://github.com/xeipuuv/gojsonpointer}.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-jsonreference
  (package
    (name "go-github-com-go-openapi-jsonreference")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/jsonreference")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1121cnjjh07qdl4jdrd46kmdhx4dgsxn02rvsq5xzapl8gz5nhcn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/jsonreference"))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-openapi-jsonpointer
           go-github-com-go-openapi-swag
           go-github-com-puerkitobio-purell))
    (home-page "https://github.com/go-openapi/jsonreference")
    (synopsis "JSON Reference with structs")
    (description
     "This package provides an implementation of JSON Reference, initially
prototyped in @url{https://github.com/xeipuuv/gojsonreference}.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-loads
  (package
    (name "go-github-com-go-openapi-loads")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/loads")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qlcpdlm4y4v9r2r533aqvrc86b67nj31gsz29x9ilr7smr5299d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/loads"
      #:phases
      #~(modify-phases %standard-phases
          ;; FIXME: pattern schemas/*.json: cannot embed irregular file
          ;; schemas/jsonschema-draft-04.json
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
                  #$(this-package-input "go-github-com-go-openapi-spec")
                  "/src/github.com")
                 "vendor/github.com")
                (copy-recursively
                 (string-append
                  #$(this-package-input "go-github-com-go-openapi-analysis")
                  "/src/github.com")
                 "vendor/github.com"))))
          (add-before 'install 'remove-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "vendor")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-openapi-analysis
           go-github-com-go-openapi-spec
           go-github-com-go-openapi-swag
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/go-openapi/loads")
    (synopsis "Load OAI specification documents")
    (description
     "This package implements functionality of loading of @acronym{OpenAPI
Initiative,OAI} specification documents from local or remote locations.
Supports JSON and YAML documents.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-spec
  (package
    (name "go-github-com-go-openapi-spec")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/spec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07qp7gakrjwznbxg1w5ww8j0ky407s9rmyyrpwv5rsp8yw6qimjc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/spec"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-openapi-jsonpointer
           go-github-com-go-openapi-jsonreference
           go-github-com-go-openapi-swag
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/go-openapi/spec")
    (synopsis "OpenAPI specification object model")
    (description
     "This package implements functionality to marshal and unmarshal
@url{https://swagger.io/,Swagger} API specifications into a Golang object
model.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-strfmt
  (package
    (name "go-github-com-go-openapi-strfmt")
    (version "0.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/strfmt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00hqmfsgknhvp7mcbxfadpv4109f9gj59223yxhvmcviqg0a6a7b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/strfmt"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-asaskevich-govalidator
           go-github-com-go-openapi-errors
           go-github-com-google-uuid
           go-github-com-mitchellh-mapstructure
           go-github-com-oklog-ulid
           go-go-mongodb-org-mongo-driver))
    (home-page "https://github.com/go-openapi/strfmt")
    (synopsis "OpenAPI toolkit common string formats")
    (description
     "This package exposes a registry of data types to support string formats
in the @code{go-openapi} toolkit.  @code{strfmt} represents a well known
string format such as credit card or email.  The Go toolkit for OpenAPI
specifications knows how to deal with those.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-swag
  (package
    (name "go-github-com-go-openapi-swag")
    (version "0.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/swag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c03mnmy162rrd1rzfqxiwnblvlwl4v09yklq55q4j9p4jqv740g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/swag"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-mailru-easyjson go-gopkg-in-yaml-v3))
    (home-page "https://github.com/go-openapi/swag")
    (synopsis "Goodie bag in use in the go-openapi projects")
    (description
     "Contains a bunch of helper functions for go-openapi and go-swagger
projects.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-validate
  (package
    (name "go-github-com-go-openapi-validate")
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/validate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04inl03ha8sqqn1ccslbsl68shzf53qqk4yi88kvis0qdhcpl9fk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/validate"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                ;; Tests requiring network access.
                (for-each delete-file
                          (list "benchmark_test.go"
                                "example_validator_test.go"
                                "doc_test.go")))))
          ;; FIXME: pattern schemas/*.json: cannot embed irregular file
          ;; schemas/jsonschema-draft-04.json
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
                  #$(this-package-input "go-github-com-go-openapi-loads")
                  "/src/github.com")
                 "vendor/github.com")
                (copy-recursively
                 (string-append
                  #$(this-package-input "go-github-com-go-openapi-spec")
                  "/src/github.com")
                 "vendor/github.com")
                (copy-recursively
                 (string-append
                  #$(this-package-input "go-github-com-go-openapi-analysis")
                  "/src/github.com")
                 "vendor/github.com"))))
          (add-before 'install 'remove-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "vendor")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-go-openapi-analysis
           go-github-com-go-openapi-errors
           go-github-com-go-openapi-jsonpointer
           go-github-com-go-openapi-loads
           go-github-com-go-openapi-spec
           go-github-com-go-openapi-strfmt
           go-github-com-go-openapi-swag
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/go-openapi/validate")
    (synopsis "OpenAPI toolkit validation helpers")
    (description
     "This package provides helpers to validate Swagger 2.0 specification (aka
OpenAPI 2.0).")
    (license license:asl2.0)))

(define-public go-github-com-go-webauthn-webauthn
  (package
    (name "go-github-com-go-webauthn-webauthn")
    (version "0.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-webauthn/webauthn")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jbx3cd8cr4aaqq9s1x4sd1rlcs3lmam5aavpl08s5rj18m7rivf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/go-webauthn/webauthn"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (list
                           ;; It tryes to access outbound network:
                           ;;
                           ;; Get "https://mds.fidoalliance.org": dial tcp:
                           ;; lookup mds.fidoalliance.org on [::1]:53: read udp
                           ;; [::1]:52300->[::1]:53: read: connection refused
                           ;;
                           ;; Post "https://mds3.fido.tools/getEndpoints": dial
                           ;; tcp: lookup mds3.fido.tools on [::1]:53: read udp
                           ;; [::1]:46703->[::1]:53: read: connection refused
                           "metadata/metadata_test.go"
                           ;; Get "https://mds.fidoalliance.org": dial tcp:
                           ;; lookup mds.fidoalliance.org on [::1]:53: read udp
                           ;; [::1]:37459->[::1]:53: read: connection refused
                           "protocol/attestation_androidkey_test.go"
                           "protocol/attestation_apple_test.go"
                           "protocol/attestation_packed_test.go"
                           "protocol/attestation_safetynet_test.go"
                           "protocol/attestation_test.go"
                           "protocol/attestation_tpm_test.go"
                           "protocol/attestation_u2f_test.go")))))
          ;; XXX: Run all tests, workaround for go-build-system's lack of Go
          ;; modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-fxamacker-cbor-v2
           go-github-com-go-webauthn-x
           go-github-com-golang-jwt-jwt-v5
           go-github-com-google-go-tpm
           go-github-com-google-uuid
           go-github-com-mitchellh-mapstructure))
    (home-page "https://github.com/go-webauthn/webauthn")
    (synopsis "Webauthn/FIDO2 library for Golang")
    (description
     "This library is meant to handle @url{https://www.w3.org/TR/webauthn,Web
Authentication} for Go apps that wish to implement a passwordless solution for
users.  This library conforms as much as possible to the guidelines and
implementation procedures outlined by the document.  It's a successor of not
maintained https://github.com/duo-labs/webauthn library.")
    (license license:bsd-3)))

(define-public go-github-com-go-webauthn-x
  (package
    (name "go-github-com-go-webauthn-x")
    (version "0.1.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-webauthn/x")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h2ylzzh5xismgkz0gar3k8lwdwqqc2np4z5gmi1b5chh6qwy1bs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; no tests
      #:import-path "github.com/go-webauthn/x"
      #:phases
      #~(modify-phases %standard-phases
          ;; Source only package.
          (delete 'build))))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/go-webauthn/x")
    (synopsis "Low level packages for WebAuthn")
    (description
     "This package implements a low level functionality for
@url{https://github.com/go-webauthn/webauthn,WebAuthn} library.  It was forked
from CloudFlare's github.com/cloudflare/cfssl/revoke.")
    (license (list
              ;; For the CloudFlare's part: revoke/LICENSE.
              license:bsd-2
              ;; For the WebAuthn's fork: LICENSE.
              license:bsd-3))))

(define-public go-github-com-goccy-go-json
  (package
    (name "go-github-com-goccy-go-json")
    (version "0.10.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/goccy/go-json")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w9kjplhyzq8n4iainddapzj7dxnfbjiz4xdpb0hlb6h35grpxgn"))
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
       (list go-github-com-golang-protobuf))
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
    (propagated-inputs
     (list go-github-com-google-go-querystring
           go-golang-org-x-crypto
           go-golang-org-x-oauth2
           go-golang-org-x-sync))
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
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/csrf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01d56sr9yggn6gs4lf5bnj15q6bkwvsim8kzj8m4arv1ccj7918j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gorilla/csrf"))
    (propagated-inputs
     (list go-github-com-gorilla-securecookie
           go-github-com-pkg-errors))
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

(define-public go-github-com-hjson-hjson-go-v4
  (package
    (name "go-github-com-hjson-hjson-go-v4")
    (version "4.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hjson/hjson-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d4b2hpqsnzbmfhgxq15hd19rjr5hydjmpblrh5yzfgx9z3cz2by"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hjson/hjson-go/v4"))
    (home-page "https://hjson.org/")
    (synopsis "Human JSON implementation for Go")
    (description
     "Hjson is a syntax extension to JSON.  It is intended to be used like a
user interface for humans, to read and edit before passing the JSON data to
the machine.")
    (license license:expat)))

(define-public go-github-com-huin-goupnp
  (package
    (name "go-github-com-huin-goupnp")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huin/goupnp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04j5rmrfawjxcimiqpyjm9gm5phdndjxrmydf9f1ylij6m360nwl"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packed as separated
            ;; packages:
            ;;
            ;; - github.com/huin/goupnp/v2alpha
            (for-each delete-file-recursively
                      (list "v2alpha"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/huin/goupnp"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Run all tests, workaround for go-build-system's lack of Go
          ;; modules support.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (home-page "https://github.com/huin/goupnp")
    (propagated-inputs
     (list go-golang-org-x-sync))
    (synopsis "UPnP client library for Go")
    (description
     "@code{goupnp} is a @acronym{Universal Plug and Play, UPnP} client
library for Go.

Core components:
@itemize
@item @code{goupnp}: core library - contains datastructures and utilities
typically used by the implemented DCPs
@item @code{httpu}: HTTPU implementation, underlies SSDP
@item @code{ssdp}: SSDP client implementation (simple service discovery
protocol) - used to discover UPnP services on a network
@item @code{soap}: SOAP client implementation (simple object access protocol)
- used to communicate with discovered services
@end itemize")
    (license license:bsd-2)))

(define-public go-github-com-jackpal-gateway
  (package
    (name "go-github-com-jackpal-gateway")
    (version "1.0.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackpal/gateway")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dlspnbdz63b3kpavibd2764hdy53mx1v3vrqi721dsjy77r9ig3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackpal/gateway"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/jackpal/gateway")
    (synopsis "Discover the address of a LAN gateway")
    (description
     "@code{gateway} is a Go library for discovering the IP address of the
default LAN gateway.")
    (license license:bsd-3)))

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

(define-public go-github-com-jhillyerd-enmime
  (package
    (name "go-github-com-jhillyerd-enmime")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhillyerd/enmime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03pir9wq9ha2i2ifj819yv5i0lvrgdn904ksbzgc3k8bqc497ydn"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/jhillyerd/enmime"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-cention-sany-utf7
           go-github-com-go-test-deep
           go-github-com-gogs-chardet
           go-github-com-jaytaylor-html2text
           go-github-com-pkg-errors
           go-golang-org-x-text))
    (home-page "https://github.com/jhillyerd/enmime")
    (synopsis "MIME encoder and decoder for Go")
    (description
     "The enmime package implements a MIME encoding and decoding library
geared towards parsing MIME encoded emails.")
    (license license:expat)))

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

(define-public go-github-com-koron-go-ssdp
  (package
    (name "go-github-com-koron-go-ssdp")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/koron/go-ssdp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0agzxzlwvnhgwk6sxswjq7v1ghmf0l02gr7zpdih24i3g457af4f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/koron/go-ssdp"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; Test requiring network setup.
                  (("TestAdvertise_Alive") "OffTestAdvertise_Alive")
                  (("TestAdvertise_Bye") "OffTestAdvertise_Bye")
                  (("TestAnnounceAlive") "OffTestAnnounceAlive")
                  (("TestAnnounceBye") "OffTestAnnounceBye")
                  (("TestInterfaces") "OffTestInterfaces")
                  (("TestSearch_Request") "OffTestSearch_Request")
                  (("TestSearch_Response") "OffTestSearch_Response")
                  (("TestSearch_ServiceRawHeader") "OffTestSearch_ServiceRawHeader")))))
          ;; XXX: Run all tests, workaround for go-build-system's lack of Go
          ;; modules support.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/koron/go-ssdp")
    (synopsis "SSDP library for Golang")
    (description
     "@code{go-ssdp} is a @url{https://tools.ietf.org/html/draft-cai-ssdp-v1-03,
@acronym{Simple Service Discovery Protocol, SSDP}} library for Golang.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-cidranger
  (package
    (name "go-github-com-libp2p-go-cidranger")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-cidranger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05hzlk5hx7qna5znr3q1crr0qb7h8yrv1v96pj015dh0kbdkdaba"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Check if the most of the tests may be enabled:
      ;; src/github.com/libp2p/go-cidranger/trie_test.go:557:8: cannot use
      ;; 4294967295 (untyped int constant) as int value in assignment
      ;; (overflows).
      #:tests? (target-64bit?)
      #:import-path "github.com/libp2p/go-cidranger"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-detect-race))
    (home-page "https://github.com/libp2p/go-cidranger")
    (synopsis "Fast IP to CIDR lookup in Golang")
    (description
     "Fast IP to @url{https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing,
CIDR} block(s) lookup using trie in Golang, inspired by
@url{https://vincent.bernat.im/en/blog/2017-ipv4-route-lookup-linux, IPv4
route lookup Linux}.  Possible use cases include detecting if a IP address is
from published cloud provider CIDR blocks (e.g. 52.95.110.1 is contained in
published AWS Route53 CIDR 52.95.110.0/24), IP routing rules, etc.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-doh-resolver
  (package
    (name "go-github-com-libp2p-go-doh-resolver")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-doh-resolver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0asni7f3gd65bjfqz99fqchz9y75cpgmfwkkhsbq0y2dydagw666"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-doh-resolver"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-log-v2
           go-github-com-miekg-dns
           go-github-com-multiformats-go-multiaddr-dns))
    (home-page "https://github.com/libp2p/go-doh-resolver")
    (synopsis "DNS over HTTPS resolver")
    (description
     "This package provides an implementation DNS over HTTPS resolver as
specified in @url{https://datatracker.ietf.org/doc/html/rfc8484, RFC 8484}.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-flow-metrics
  (package
    (name "go-github-com-libp2p-go-flow-metrics")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-flow-metrics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13yb68vrcn2zdi0mjjh17dphfxl6y99bkq0ia53hasyfj6l6626h"))))
    (build-system go-build-system)
    (arguments
     ;; XXX: Tests may hang sometimes, see
     ;; <https://github.com/libp2p/go-flow-metrics/issues/30>.
     (list
      #:import-path "github.com/libp2p/go-flow-metrics"))
    (propagated-inputs
     (list go-github-com-benbjohnson-clock))
    (home-page "https://github.com/libp2p/go-flow-metrics")
    (synopsis "Simple library for tracking flow metrics")
    (description
     "A simple alternative to rcrowley's @command{go-metrics} that's a lot
faster (and only does simple bandwidth metrics).")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-asn-util
  (package
    (name "go-github-com-libp2p-go-libp2p-asn-util")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-libp2p-asn-util")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c94sq43bl1kp04lllcfrfyiy5z3zcfz0s65sm1vgb2s40zrwpr7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-libp2p-asn-util"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-exp))
    (home-page "https://github.com/libp2p/go-libp2p-asn-util")
    (synopsis "Golang library for IP to ASN mapping")
    (description
     "@code{go-libp2p-asn-util} is a Golang library to lookup the
@acronym{ASN, Autonomous System Number} for an IP address.  It uses the IPv6
to ASN database downloaded from https://iptoasn.com/.  Supports only IPv6
addresses for now.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-nat
  (package
    (name "go-github-com-libp2p-go-nat")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-nat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yyb3knxvfr7fi759nh7mhh88ap1jpkb7nky7niqrh75737phgh0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-nat"))
    (propagated-inputs
     (list go-github-com-huin-goupnp
           go-github-com-jackpal-go-nat-pmp
           go-github-com-koron-go-ssdp
           go-github-com-libp2p-go-netroute))
    (home-page "https://github.com/libp2p/go-nat")
    (synopsis "NAT port mapping library for Golang")
    (description
     "Package @code{go-nat} implements NAT handling facilities.")
    (license license:asl2.0)))

(define-public go-github-com-libp2p-go-netroute
  (package
    (name "go-github-com-libp2p-go-netroute")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-netroute")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06p68j63fd5nf2gf1fz2pnksmdmv735swpbpvnhb15vrgg3r528g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-netroute"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; Test requiring network access: no route found for 8.8.8.8
                  (("TestRoute") "OffTestRoute"))))))))
    (propagated-inputs
     (list go-github-com-google-gopacket
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/libp2p/go-netroute")
    (synopsis "Routing table abstraction library for Golang")
    (description
     "@code{go-netroute} provides an implementation of the
@url{https://godoc.org/github.com/google/gopacket/routing#Router,
gopacket/routing.Router} interface for Golang.")
    (license license:bsd-3)))

(define-public go-github-com-libp2p-go-reuseport
  (package
    (name "go-github-com-libp2p-go-reuseport")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-reuseport")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "012kgriw1bchf0apk6ff4y34n9mffbh0cmi15348v9vj3h4w3sa5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-reuseport"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-gopacket
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/libp2p/go-reuseport")
    (synopsis "Reuse TCP/UDP ports in Golang")
    (description
     "@code{go-reuseport} enables listening and dialing from the same TCP or
UDP port.  This means that @code{SO_REUSEADDR} and @code{SO_REUSEPORT} socket
options may be set.  This is particularly important when attempting to do TCP
NAT hole-punching, which requires a process to both @code{Listen} and
@code{Dial} on the same TCP port.  @code{go-reuseport} provides some utilities
around enabling this behaviour on various operating systems.")
    (license license:isc)))

(define-public go-github-com-libp2p-go-socket-activation
  (package
    (name "go-github-com-libp2p-go-socket-activation")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-socket-activation")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cqxzmjfg7838xifs07kigys9icardwlj1wl426mzgzmbwn6pg5s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-socket-activation"))
    (propagated-inputs
     (list go-github-com-coreos-go-systemd-v22
           go-github-com-ipfs-go-log
           go-github-com-multiformats-go-multiaddr))
    (home-page "https://github.com/libp2p/go-socket-activation")
    (synopsis "Multiaddr backed systemd socket activation")
    (description
     "This package provides access to sockets registered by the system's init
daemon as described in
@url{http://0pointer.de/blog/projects/socket-activation}.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-yamux-v4
  (package
    (name "go-github-com-libp2p-go-yamux-v4")
    (version "4.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-yamux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13px8fcjjp02cricabbf3x410jkr8sb6r369nqq1zrgr7v90s22j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-yamux/v4"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-libp2p-go-buffer-pool))
    (home-page "https://github.com/libp2p/go-yamux")
    (synopsis "Reuse TCP/UDP ports in Golang")
    (description
     "Yamux (Yet another Multiplexer) is a multiplexing library for Golang.
It relies on an underlying connection to provide reliability and ordering,
such as TCP or Unix domain sockets, and provides stream-oriented multiplexing.
It is inspired by SPDY but is not interoperable with it.")
    (license (list license:mpl2.0 license:bsd-3))))

(define-public go-github-com-mailru-easyjson
  (package
    (name "go-github-com-mailru-easyjson")
    (version "0.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mailru/easyjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0clifkvvy8f45rv3cdyv58dglzagyvfcqb63wl6rij30c5j2pzc1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mailru/easyjson"))
    (propagated-inputs
     (list go-github-com-josharian-intern))
    (home-page "https://github.com/mailru/easyjson")
    (synopsis "JSON serializer for Golang")
    (description
     "Package @code{easyjson} implements functionality to marshal/unmarshal
Golang structs to/from JSON without the use of reflection.  It also aims to
keep generated Go code simple enough so that it can be easily optimized or
fixed.")
    (license license:expat)))

(define-public go-github-com-makeworld-the-better-one-go-gemini
  (package
    (name "go-github-com-makeworld-the-better-one-go-gemini")
    (version "0.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/makew0rld/go-gemini")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "196rxfg7w8s3zn87gra1mxh1l8iav6kdmg909gkbnc9cxip65zc0"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-golang-org-x-net
           go-golang-org-x-text))
    (arguments
     (list
      #:import-path "github.com/makeworld-the-better-one/go-gemini"))
    (home-page "https://github.com/makew0rld/go-gemini")
    (synopsis "Client/server library for the Gemini protocol, in Go")
    (description
     "@code{go-gemini} is a library that provides an easy interface to create
clients that speak the Gemini protocol.")
    (license license:isc)))

(define-public go-github-com-makeworld-the-better-one-go-gemini-socks5
  (package
    (name "go-github-com-makeworld-the-better-one-go-gemini-socks5")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/makew0rld/go-gemini-socks5")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r8iljs12nhyn3nk5dzsji9hi88fivashbrcb5d42x5rvzry15px"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/makeworld-the-better-one/go-gemini-socks5"))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/makeworld-the-better-one/go-gemini-socks5")
    (synopsis "SOCKS5 proxy for go-gemini")
    (description
     "This package provides SOCKS5 proxy for
@@url{https://github.com/makeworld-the-better-one/go-gemini,go-gemini}.")
    (license license:expat)))

(define-public go-github-com-mattbaird-jsonpatch
  (package
    (name "go-github-com-mattbaird-jsonpatch")
    (version "0.0.0-20240118010651-0ba75a80ca38")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattbaird/jsonpatch")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nxbrpk8bvvmfgl4sfsbx82g0q44i2sakl7vigbsj3prx6nql5iv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattbaird/jsonpatch"))
    (native-inputs
     (list go-github-com-evanphx-json-patch go-github-com-stretchr-testify))
    (home-page "https://github.com/mattbaird/jsonpatch")
    (synopsis "JSON Patch library for Go")
    (description
     "@url{http://jsonpatch.com/, JSON Patch} implementation for Go as
specified in @url{https://datatracker.ietf.org/doc/html/rfc6902/, RFC 6902}
from the IETF.

JSON Patch allows you to generate JSON that describes changes you want to make
to a document, so you don't have to send the whole doc.  JSON Patch format is
supported by HTTP PATCH method, allowing for standards based partial updates
via REST APIs.")
    (license license:asl2.0)))

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

(define-public go-github-com-miekg-dns
  (package
    (name "go-github-com-miekg-dns")
    (version "1.1.62")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/miekg/dns")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wdsacp4ay6ji72vnszq6ksn5n060z2hv94wgjsn0pr7gpa3nk6c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/miekg/dns"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; Unable to run test server.
                  (("TestIsPacketConn") "OffTestIsPacketConn"))))))))
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

(define-public go-github-com-multiformats-go-multiaddr
  (package
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
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-multiaddr"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-cid
           go-github-com-multiformats-go-multibase
           go-github-com-multiformats-go-varint
           go-github-com-multiformats-go-multihash
           go-golang-org-x-exp))
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
      #:import-path "github.com/multiformats/go-multiaddr-dns"
      #:unpack-path "github.com/multiformats/go-multiaddr-dns"))
    (propagated-inputs
     (list go-github-com-miekg-dns
           go-github-com-multiformats-go-multiaddr))
    (home-page "https://multiformats.io/multiaddr/")
    (synopsis "Library and CLI tool for DNS multiaddr resolution")
    (description
     "Go library for /dns4, /dns6, /dnsaddr multiaddr resolution.")
    (license license:expat)))

(define-public go-github-com-multiformats-go-multiaddr-fmt
  (package
    (name "go-github-com-multiformats-go-multiaddr-fmt")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-multiaddr-fmt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "119qqrnhzcb9im428alssv2dz9rrj74hy0asd10bnfv2d5fd09nm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-multiaddr-fmt"))
    (propagated-inputs
     (list go-github-com-multiformats-go-multiaddr))
    (home-page "https://github.com/multiformats/go-multiaddr-fmt")
    (synopsis "Declarative validator for multiaddrs")
    (description
     "This package provides a validation checker for multiaddrs.  Some basic
validators for common address types are provided, but creating your own
combinations is easy.")
    (license license:expat)))

(define-public go-github-com-multiformats-go-multistream
  (package
    (name "go-github-com-multiformats-go-multistream")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-multistream")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mlcz16ii090vq6brm02dmmkj8akkafa55kyvkrrwpq6zvj1hy23"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/multiformats/go-multistream"))
    (propagated-inputs
     (list go-github-com-multiformats-go-varint))
    (home-page "https://github.com/multiformats/go-multistream")
    (synopsis "Implementation of the multistream protocol in Golang")
    (description
     "Package multistream implements a simple stream router for the
multistream-select protocol.  The protocol is defined at
@url{https://github.com/multiformats/multistream-select}")
    (license license:expat)))

(define-public go-github-com-munnerz-goautoneg
  (package
    (name "go-github-com-munnerz-goautoneg")
    (version "0.0.0-20191010083416-a7dc8b61c822")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/munnerz/goautoneg")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m4v6bw6yf1g0kvpc46isjp0qfhx2y8gnvlnyjf637jy64613mgg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/munnerz/goautoneg"))
    (home-page "https://github.com/munnerz/goautoneg")
    (synopsis "HTTP Content-Type Autonegotiation")
    (description
     "This package implements @url{https://rfc-editor.org/rfc/rfc2616.html,RFC
2616} HTTP/1.1 standard.")
    (license license:bsd-3)))

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

(define-public go-github-com-opentracing-contrib-go-stdlib
  (package
    (name "go-github-com-opentracing-contrib-go-stdlib")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opentracing-contrib/go-stdlib")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ssnfhbpljxy2v3nsw9aqmh7xlky49dpfwj275aj0b576w46ys6m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/opentracing-contrib/go-stdlib"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-opentracing-opentracing-go))
    (home-page "https://github.com/opentracing-contrib/go-stdlib")
    (synopsis "OpenTracing instrumentation for packages in the Golang stdlib")
    (description
     "This package provides an OpenTracing instrumentation for @code{net/http}
standard library.")
    (license license:asl2.0)))

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

(define-public go-github-com-oschwald-geoip2-golang
  (package
    (name "go-github-com-oschwald-geoip2-golang")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oschwald/geoip2-golang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0670cv1b9c2p0lx63rlwl7kplbvzr79apbw13109v0pv4qlapmhx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; Requires some unpackaged software and test data
      #:import-path "github.com/oschwald/geoip2-golang"))
    (propagated-inputs
     (list go-github-com-oschwald-maxminddb-golang))
    (home-page "https://github.com/oschwald/geoip2-golang")
    (synopsis "MaxMind GeoIP2 reader")
    (description
     "This package provides a library for reading MaxMind GeoLite2 and GeoIP2
databases in Go.")
    (license license:isc)))

(define-public go-github-com-oschwald-maxminddb-golang
  (package
    (name "go-github-com-oschwald-maxminddb-golang")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oschwald/maxminddb-golang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p0c10r6850znvarc9h3y0jlwika9qmq0ys7rmg2aj8x2cffz3z6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/oschwald/maxminddb-golang"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Remove most of the tests requiring test-data from submodule
          ;; <https://github.com/maxmind/MaxMind-DB>, there is a documented
          ;; process on how to generate it, consider to pack and activate
          ;; tests in the next update cycle.
          (add-after 'unpack 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (list "decoder_test.go"
                                "deserializer_test.go"
                                "example_test.go"
                                "reader_test.go"
                                "traverse_test.go"
                                "verifier_test.go"))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/oschwald/maxminddb-golang")
    (synopsis "MaxMind DB Reader for Go")
    (description
     "This is a Go reader for the MaxMind DB format.  Although this can be
used to read GeoLite2 and GeoIP2 databases, @code{geoip2} provides a
higher-level API for doing so.")
    (license license:isc)))

(define-public go-github-com-pion-datachannel
  (package
    (name "go-github-com-pion-datachannel")
    (version "1.5.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/datachannel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v2xwrzvflrh1s8x6p1dj9h0hmxsp30h8whbl5p544r30rvsbnp2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/datachannel"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-sctp
           go-github-com-pion-transport-v3))
    (home-page "https://github.com/pion/datachannel")
    (synopsis "Implementation of WebRTC Data Channels in Golang")
    (description
     "This package implements @code{WebRTC} Data Channels.")
    (license license:expat)))

(define-public go-github-com-pion-dtls
  (package
    (name "go-github-com-pion-dtls")
    (version "1.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/dtls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qc5dbgh31lilbd1lpmajj1fjzy4jx9iadzqgl9jd1ry9fj3ly1d"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: TestClientCertificate: Client failed(x509: certificate relies on
      ;; legacy Common Name field, use SANs instead)
      #:tests? #f
      #:import-path "github.com/pion/dtls"))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-transport
           go-golang-org-x-crypto))
    (home-page "https://github.com/pion/dtls")
    (synopsis "DTLS 1.2 Server/Client implementation for Go")
    (description
     "This package provides a native
@url{https://datatracker.ietf.org/doc/html/rfc6347, DTLS 1.2} implementation
in Golang.")
    (license license:expat)))

(define-public go-github-com-pion-dtls-v2
  (package
    (inherit go-github-com-pion-dtls)
    (name "go-github-com-pion-dtls-v2")
    (version "2.2.11")
    (source
     (origin
       (inherit (package-source go-github-com-pion-dtls))
       (uri (git-reference
             (url "https://github.com/pion/dtls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10nn9349f7snqkzncda5m013fgnzicrcxi6pb6ghc0vb6rhqkf30"))))
    (arguments
     (list
      #:import-path "github.com/pion/dtls/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-transport-v2
           go-github-com-pion-transport-v3
           go-golang-org-x-crypto
           go-golang-org-x-net))))

(define-public go-github-com-pion-ice
  (package
    (name "go-github-com-pion-ice")
    (version "0.7.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/ice/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17108z4fkr9b2fxf5icxspgif29a40gi57bhp9a50mlfr36yv9vk"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Source-only package
      #:tests? #f
      #:import-path "https://github.com/pion/ice"
      #:phases
      ;; Failed to build and only requried for inheritance:
      ;;
      ;; cannot use a.net (type *vnet.Net) as type transport.Net in field value:
      ;; *vnet.Net does not implement transport.Net (wrong type for CreateDialer method)
      ;;         have CreateDialer(*net.Dialer) vnet.Dialer
      ;;         want CreateDialer(*net.Dialer) transport.Dialer
      #~(modify-phases %standard-phases
          (delete 'build))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-github-com-pion-dtls-v2
           go-github-com-pion-logging
           go-github-com-pion-mdns
           go-github-com-pion-randutil
           go-github-com-pion-stun
           go-github-com-pion-transport
           go-github-com-pion-turn-v2
           go-golang-org-x-net))
    (home-page "https://github.com/pion/ice/")
    (synopsis "Go implementation of ICE")
    (description
     "This package provides an implementation of @acronym{ICE, Interactive
Connectivity Establishment protocol}, specified in
@url{https://datatracker.ietf.org/doc/html/rfc8445, RFC8445}.  It is used as a
part of @url{https://github.com/pion, Pion} WebRTC implementation.")
    (license license:expat)))

(define-public go-github-com-pion-ice-v2
  (package
    (inherit go-github-com-pion-ice)
    (name "go-github-com-pion-ice-v2")
    (version "2.3.24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/ice/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mh7l31vv15gxpl61f22jwpc77b5l9wx4hbjv4h8cgmpb9911cv8"))))
    (arguments
     (list
      #:tests? #f ;Tests require network access.
      #:import-path "github.com/pion/ice/v2"))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-github-com-pion-dtls-v2
           go-github-com-pion-logging
           go-github-com-pion-mdns
           go-github-com-pion-randutil
           go-github-com-pion-stun
           go-github-com-pion-transport-v2
           go-github-com-pion-turn-v2
           go-golang-org-x-net))))

(define-public go-github-com-pion-ice-v3
  (package
    (inherit go-github-com-pion-ice)
    (name "go-github-com-pion-ice-v3")
    (version "3.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/ice/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f9jy80law69zb26rkb6kl6w1c66vdghdrmifhwlmzngb644ihdb"))))
    (arguments
     (list
      #:tests? #f ;Tests require network access.
      #:import-path "github.com/pion/ice/v3"))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-github-com-pion-dtls-v2
           go-github-com-pion-logging
           go-github-com-pion-mdns-v2
           go-github-com-pion-randutil
           go-github-com-pion-stun-v2
           go-github-com-pion-transport-v3
           go-github-com-pion-turn-v3
           go-golang-org-x-net))))

(define-public go-github-com-pion-mdns
  (package
    (name "go-github-com-pion-mdns")
    (version "0.0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/mdns/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18nz0vddxik3q11mn4z65zvrfhspxv0xymxv9w3kgk2kszwq2byy"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests are implemented in GitHub Actions and require aditional
      ;; packaging, see
      ;; <https://github.com/pion/.goassets/blob/master/.github/workflows/test.reusable.yml>.
      #:tests? #f
      #:unpack-path "github.com/pion/mdns"
      #:import-path "github.com/pion/mdns"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-transport-v3
           go-golang-org-x-net))
    (home-page "https://github.com/pion/mdns/")
    (synopsis "Pure Go implementation of Multicast DNS")
    (description
     "This package implements a mDNS (multicast DNS) used by
@url{https://github.com/pion, Pion}.")
    (license license:expat)))

(define-public go-github-com-pion-mdns-v2
  (package
    (inherit go-github-com-pion-mdns)
    (name "go-github-com-pion-mdns-v2")
    (version "2.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/mdns/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03675hx82lx3c8akkxdbkch1z4dbq54r05jk6jgdyd7mrdh9k4lm"))))
    (arguments
     (substitute-keyword-arguments (package-arguments
                                    go-github-com-pion-mdns)
       ((#:unpack-path flags ''())
        "github.com/pion/mdns/v2")
       ((#:import-path flags ''())
        "github.com/pion/mdns/v2")))))

(define-public go-github-com-pion-rtp
  (package
    (name "go-github-com-pion-rtp")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/rtp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vrdvswvvbqq83kbjlyblarbsn5v0sjcwrcv03nncd605cggnbkx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/rtp"))
    (propagated-inputs
     (list go-github-com-pion-randutil))
    (home-page "https://github.com/pion/rtp")
    (synopsis "Go implementation of RTP")
    (description
     "This package provides a @acronym{Real-time Transport Protocol, RTP}
packetizer and depacketizer.")
    (license license:expat)))

(define-public go-github-com-pion-sctp
  (package
    (name "go-github-com-pion-sctp")
    (version "1.8.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/sctp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19yzpyrlmk3gvpkpn5846rad9cc8ffxw9jqwnpr6szqax7k0l0zw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/sctp"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-randutil
           go-github-com-pion-transport-v3))
    (home-page "https://github.com/pion/sctp")
    (synopsis "Implementation of SCTP in Golang")
    (description
     "This package implements the @acronym{Stream Control Transmission
Protocol,SCTP} as specified in
@uref{https://rfc-editor.org/rfc/rfc9260.html,RFC 9260}.")
    (license license:expat)))

(define-public go-github-com-pion-stun
  (package
    (name "go-github-com-pion-stun")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/stun")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0172fcm1xvzvy3d5lcpscayzpf3i5w4bpfydifdc9l4n2wslx0sm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/stun"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list  go-github-com-pion-dtls-v2
            go-github-com-pion-logging
            go-github-com-pion-transport-v2))
    (home-page "https://github.com/pion/stun")
    (synopsis "Go implementation of STUN")
    (description
     "Package @code{stun} implements Session Traversal Utilities for
+NAT (STUN) (@url{https://tools.ietf.org/html/rfc5389, RFC 5389}) protocol and
+@url{https://pkg.go.dev/github.com/pion/stun#Client, client} with no external
+dependencies and zero allocations in hot paths.  Client
+@url{https://pkg.go.dev/github.com/pion/stun#WithRTO, supports} automatic
+request retransmissions.")
    (license license:expat)))

(define-public go-github-com-pion-stun-v2
  (package
    (inherit go-github-com-pion-stun)
    (name "go-github-com-pion-stun-v2")
    (version "2.0.0")
    (source
     (origin
       (inherit (package-source go-github-com-pion-stun))
       (uri (git-reference
             (url "https://github.com/pion/stun")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zli55ls5izpr6cw0wj0gy44872xn9rk20i8ay9cfk7j2rb60y60"))))
    (arguments
     (list
      #:import-path "github.com/pion/stun/v2"))
    (propagated-inputs
     (list go-github-com-pion-dtls-v2
           go-github-com-pion-logging
           go-github-com-pion-transport-v3
           go-golang-org-x-crypto
           go-golang-org-x-net))))

(define-public go-github-com-pion-transport
  (package
    (name "go-github-com-pion-transport")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/transport")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0331kywqaa6fymc64wrqgwnxlhx31qdf299i927vifx1wdcl9ikp"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Source-only package
      #:tests? #f
      #:import-path "github.com/pion/transport"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/pion/transport")
    (synopsis "Golang networking related functions")
    (description
     "This package implements a various networking related functions used
throughout the @url{https://github.com/pion, Pion} modules.")
    (license license:expat)))

(define-public go-github-com-pion-transport-v2
  (package
    (inherit go-github-com-pion-transport)
    (name "go-github-com-pion-transport-v2")
    (version "2.2.5")
    (source
     (origin
       (inherit (package-source go-github-com-pion-transport))
       (uri (git-reference
             (url "https://github.com/pion/transport/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00q3v37l56yr1ch25g5w70jy8y923csbvy4krvy4dv3h5f1mdpmf"))))
    (arguments
     (list
      #:import-path "github.com/pion/transport/v2"))))

(define-public go-github-com-pion-transport-v3
  (package
    (inherit go-github-com-pion-transport)
    (name "go-github-com-pion-transport-v3")
    (version "3.0.2")
    (source
     (origin
       (inherit (package-source go-github-com-pion-transport))
       (uri (git-reference
             (url "https://github.com/pion/transport/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j7ljkbyf2qd7daxg7d1rd6c92md64agi59b69g6jyqpz5jww998"))))
    (arguments
     (list
      #:import-path "github.com/pion/transport/v3"))))

(define-public go-github-com-pion-turn
  (package
    (name "go-github-com-pion-turn")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/turn/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16lkgmrlks0qdzbk8jj0c0j66qfxhb54cvzgrfn4imvm56dbxp2n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ;Tests require network access.
      #:import-path "github.com/pion/turn"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-stun
           go-github-com-pion-transport
           go-github-com-pkg-errors))
    (home-page "https://github.com/pion/turn/")
    (synopsis "API for building TURN clients and servers in Golang")
    (description
     "This package provides a toolkit for building @acronym{TURN, Traversal
Using Relays around NAT}, specified in
@url{https://datatracker.ietf.org/doc/html/rfc8656, RFC 8656}, servers and
clients.

@code{pion/turn} is an API for building STUN/TURN clients and servers, not a
binary you deploy then configure.  It may require copying the examples and
making minor modifications to fit your need, no knowledge of Go is required
however.

The advantage of this is that you don't need to deal with complicated
configuration files, or custom APIs to modify the state of Pion TURN.  After
you instantiate an instance of a Pion TURN server or client you interact with
it like any library.  The quickest way to get started is to look at the
@url{https://github.com/pion/turn/blob/master/examples, examples} or
@url{https://godoc.org/github.com/pion/turn, GoDoc}.")
    (license license:expat)))

(define-public go-github-com-pion-turn-v2
  (package
    (inherit go-github-com-pion-turn)
    (name "go-github-com-pion-turn-v2")
    (version "2.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/turn/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iw7nvqsxpqy90k5a8mq3dyask272391m59cbiy30aak1y2wwaac"))))
    (arguments
     (substitute-keyword-arguments (package-arguments
                                    go-github-com-pion-turn)
       ((#:import-path flags ''())
        "github.com/pion/turn/v2")))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-randutil
           go-github-com-pion-stun
           go-github-com-pion-transport-v2
           go-golang-org-x-sys))))

(define-public go-github-com-pion-turn-v3
  (package
    (inherit go-github-com-pion-turn)
    (name "go-github-com-pion-turn-v3")
    (version "3.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/turn/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l78m9ym0sv1zfalbv95lwblmr789fc53d957ph5mdznhjx89lyx"))))
    (arguments
     (substitute-keyword-arguments (package-arguments
                                    go-github-com-pion-turn)
       ((#:import-path flags ''())
        "github.com/pion/turn/v3")))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-randutil
           go-github-com-pion-stun-v2
           go-github-com-pion-transport-v3
           go-golang-org-x-sys))))

(define-public go-github-com-pires-go-proxyproto
  (package
    (name "go-github-com-pires-go-proxyproto")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pires/go-proxyproto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p18w555xp187fl807h1yd092cvs8jarp98pa76zl84rxlk4k2h4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pires/go-proxyproto"))
    (home-page "https://github.com/pires/go-proxyproto")
    (synopsis "Implementation of the PROXY protocol")
    (description
     "Package proxyproto implements Proxy Protocol (v1 and v2) parser and
writer, as per specification:
@@url{https://www.haproxy.org/download/2.3/doc/proxy-protocol.txt}.  It is to
be used in one of or both proxy clients and proxy servers that need to support
said protocol.  Both protocol versions, 1 (text-based) and 2 (binary-based)
are supported. @acronym{TLV, tag-length-value} parsers extensions comming with
this library support AWS, Azure and GCP.")
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

(define-public go-github-com-puerkitobio-purell
  (package
    (name "go-github-com-puerkitobio-purell")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PuerkitoBio/purell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zv1pkfvnrpv18ji3mgqa1k77h066yz5hvhdr1mxdz19kdjc5l43"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/PuerkitoBio/purell"))
    (propagated-inputs
     (list go-golang-org-x-net go-golang-org-x-text))
    (home-page "https://github.com/PuerkitoBio/purell")
    (synopsis "Tiny Go library to normalize URLs")
    (description
     "This package provides implements a functionality of URL normalizer as
described in @url{http://tools.ietf.org/html/rfc3986#section-6, RFC 3986}.")
    (license license:bsd-3)))

(define-public go-github-com-puerkitobio-urlesc
  (package
    (name "go-github-com-puerkitobio-urlesc")
    (version "0.0.0-20170810143723-de5bf2ad4578")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PuerkitoBio/urlesc")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n0srpqwbaan1wrhh2b7ysz543pjs1xw2rghvqyffg9l0g8kzgcw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/PuerkitoBio/urlesc"))
    (home-page "https://github.com/PuerkitoBio/urlesc")
    (synopsis "Proper URL escaping as per RFC 3986")
    (description
     "Package @code{urlesc} implements query escaping as per
@url{https://rfc-editor.org/rfc/rfc3986.html,RFC 3986}.  It contains some
parts of the @code{net/url} package, modified so as to allow some reserved
characters incorrectly escaped by net/url.")
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
    (version "0.43.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quic-go/quic-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vqc1mb60flbm5jqf48gzhzm8m0k06klf9szpx6mgw30957qv3fn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/quic-go/quic-go"
      #:phases
      #~(modify-phases %standard-phases
          ;; TODO: Figure out why some tests fail.
          (add-after 'unpack 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (list "integrationtests/self/timeout_test.go"
                                "server_test.go")))))
          ;; Test steps are taken from GitHub Actions -
          ;; <https://github.com/quic-go/quic-go/blob/v0.42.0/.github/workflows/unit.yml>.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "ginkgo" "-r" "-v"
                          (string-append "--procs="
                                         (number->string (parallel-job-count)))
                          "--randomize-all"
                          "--randomize-suites"
                          "--skip-package"
                          "integrationtests"))))))))
    (native-inputs
     (list go-ginkgo
           go-github-com-onsi-ginkgo-v2
           go-go-uber-org-mock
           go-golang-org-x-time))
    (propagated-inputs
     (list go-github-com-francoispqt-gojay
           go-github-com-quic-go-qpack
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

(define-public go-github-com-quic-go-webtransport-go
  (package
    (name "go-github-com-quic-go-webtransport-go")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quic-go/webtransport-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zwr4jg4dg2b14kkypkbs8dpai5b5s44gm5gq0vrs3mmg6vq0v97"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/quic-go/webtransport-go"))
    (native-inputs
     (list go-go-uber-org-mock
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-quic-go-quic-go
           go-golang-org-x-exp))
    (home-page "https://github.com/quic-go/webtransport-go")
    (synopsis "WebTransport implementation based on quic-go")
    (description
     "webtransport-go is an implementation of the @code{WebTransport} protocol, based
on @@url{https://github.com/quic-go/quic-go,quic-go}.  It currently implements
@@url{https://www.ietf.org/archive/id/draft-ietf-webtrans-http3-02.html,draft-02}
of the specification.")
    (license license:expat)))

(define-public go-github-com-sherclockholmes-webpush-go
  (package
    (name "go-github-com-sherclockholmes-webpush-go")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SherClockHolmes/webpush-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qv16zvkd1c7q81v2ai8pfz590fxdrk4lfbgyymln0q7jn5wlvki"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/SherClockHolmes/webpush-go"))
    (propagated-inputs
     (list go-github-com-golang-jwt-jwt go-golang-org-x-crypto))
    (home-page "https://github.com/SherClockHolmes/webpush-go")
    (synopsis "Web Push API Encryption with VAPID support")
    (description
     "Web Push API Encryption with
@url{https://datatracker.ietf.org/doc/html/draft-ietf-webpush-vapid-01, VAPID}
support.")
    (license license:expat)))

(define-public go-github-com-shurcool-httpfs
  (package
    (name "go-github-com-shurcool-httpfs")
    (version "0.0.0-20230704072500-f1e31cf0ba5c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shurcooL/httpfs")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m0jjnfzr8372cjx0zjm2zm695kwaz8l1yk7gzgn05biadsklprm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shurcooL/httpfs"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Replace when go-build-system supports nested path.
          (delete 'build)
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (native-inputs
     (list go-golang-org-x-tools))
    (propagated-inputs
     (list go-github-com-shurcool-httpgzip))
    (home-page "https://github.com/shurcooL/httpfs")
    (synopsis "Utilities for @code{http.FileSystem}")
    (description
     "Collection of Go packages for working with the +@code{http.FileSystem}
interface.")
    (license license:expat)))

(define-public go-github-com-shurcool-httpgzip
  (package
    (name "go-github-com-shurcool-httpgzip")
    (version "0.0.0-20230704072819-d1585fc322fa")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shurcooL/httpgzip")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10fnndia8ij3hwwvknn8qy8z3955bm7xyvqd69yh5g2zh25zc5x2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shurcooL/httpgzip"))
    (native-inputs
     (list go-golang-org-x-tools))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/shurcooL/httpgzip")
    (synopsis "Primitives of @code{net-http}-like with gzip compression")
    (description
     "Package @code{httpgzip} provides @code{net/http}-like primitives that
use gzip compression when serving HTTP requests.")
    (license license:expat)))

(define-public go-github-com-shurcool-vfsgen
  (package
    (name "go-github-com-shurcool-vfsgen")
    (version "0.0.0-20230704071429-0000e147ea92")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shurcooL/vfsgen")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ypfdiv56ckb0yc7mccc2l8vc3gmfws2p7bcf9f0j415m7r0aq6q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shurcooL/vfsgen"))
    (native-inputs
     (list go-golang-org-x-tools))
    (propagated-inputs
     (list go-github-com-shurcool-httpfs))
    (home-page "https://github.com/shurcooL/vfsgen")
    (synopsis "Generate Go code from an @code{http.FileSystem}")
    (description
     "Package @code{vfsgen} takes an @code{http.FileSystem} (likely at
@code{go generate} time) and generates Go code that statically implements the
provided @code{http.FileSystem}.")
    (license license:expat)))

(define-public go-github-com-sourcegraph-jsonrpc2
  (package
    (name "go-github-com-sourcegraph-jsonrpc2")
    (version "0.2.0")
    (home-page "https://github.com/sourcegraph/jsonrpc2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1id35b4mhif9gy1b70mv0x7xkmpm2p8xydix8six10yjyhvm1wjh"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/sourcegraph/jsonrpc2"))
    (propagated-inputs
     (list go-github-com-gorilla-websocket))
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

(define-public go-github-com-ucarion-urlpath
  (package
    (name "go-github-com-ucarion-urlpath")
    (version "0.0.0-20200424170820-7ccc79b76bbb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ucarion/urlpath")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12ns9lqdz566agdp4y0whgksmidi0zp7759akvx0b79mjzyvypax"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ucarion/urlpath"))
    (home-page "https://github.com/ucarion/urlpath")
    (synopsis "REST-like URL path patterns matching")
    (description
     "Package urlpath matches paths against a template.  It's meant for
applications that take in REST-like URL paths, and need to validate and
extract data from those paths.")
    (license license:expat)))

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

(define-public go-github-com-whyrusleeping-cbor
  (package
    (name "go-github-com-whyrusleeping-cbor")
    (version "0.0.0-20171005072247-63513f603b11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/cbor")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v3kgzk8grz17my2vhv12qi9dgpx3z86hy9ff1c4qw83mg8hm67s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/whyrusleeping/cbor"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Replace when go-build-system supports nested path.
          (delete 'build)
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  ;; No test vectors were provided with git checkout:
                  ;; var errpath string = "../test-vectors/appendix_a.json"
                  (substitute* "go/cbor_test.go"
                    (("TestDecodeVectors") "offTestDecodeVectors"))
                  (invoke "go" "test" "-v" "./..."))))))))
    (home-page "https://github.com/whyrusleeping/cbor")
    (synopsis "Concise Binary Object Representation in Golang")
    (description
     "@acronym{Concise Binary Object Representation,CBOR} is a superset of
JSON's schema that's faster and more compact.")
    (license license:asl2.0)))

(define-public go-github-com-whyrusleeping-chunker
  (package
    (name "go-github-com-whyrusleeping-chunker")
    (version "0.0.0-20181014151217-fe64bd25879f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/chunker")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13q4flp9iwwyi0izqar786h42713rf3m22qlvg0masbmdi69qjr2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/whyrusleeping/chunker"))
    (home-page "https://github.com/whyrusleeping/chunker")
    (synopsis "Implementation of Content Defined Chunking in Golang")
    (description
     "Package chunker implements @acronym{Content Defined Chunking,CDC} based
on a rolling Rabin Checksum.  This package provides a modified fork of
https://github.com/restic/restic project.")
    (license license:bsd-2)))

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

(define-public go-github-com-whyrusleeping-multiaddr-filter
  (package
    (name "go-github-com-whyrusleeping-multiaddr-filter")
    (version "0.0.0-20160516205228-e903e4adabd7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/multiaddr-filter")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ksd8vnp207dvphmhrazwldj8if900fnyc1pqa9pfvj04qp92640"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; (*testing.common).Fatalf format %s has arg val of wrong type bool
      #:tests? #f
      #:import-path "github.com/whyrusleeping/multiaddr-filter"))
    (home-page "https://github.com/whyrusleeping/multiaddr-filter")
    (synopsis "Parsing ip filters and masks in the multiaddr format")
    (description
     "This module creates very simple
@url{https://github.com/jbenet/go-multiaddr,multiaddr} formatted cidr
netmasks.")
    (license license:expat)))

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

(define-public go-golang-org-x-oauth2
  (package
    (name "go-golang-org-x-oauth2")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/oauth2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pzpa9jqrfxxhxi1w7n5ljnvr9qfw42hzavz62fc9i6z9vk2466k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/oauth2"))
    (propagated-inputs
     (list go-cloud-google-com-go-compute-metadata
           go-github-com-google-go-cmp))
    (home-page "https://go.googlesource.com/oauth2")
    (synopsis "Client implementation of the OAuth 2.0 spec")
    (description
     "This package contains a client implementation for OAuth 2.0
 spec in Go.")
    (license license:bsd-3)))

(define-public go-golang-zx2c4-com-wireguard
  (package
    (name "go-golang-zx2c4-com-wireguard")
    (version "0.0.0-20231211153847-12269c276173")
    (source
     (origin
       (method git-fetch)
       ;; NOTE: module URL is a redirect
       ;; target: git.zx2c4.com/wireguard-go
       ;; source: golang.zx2c4.com/wireguard
       (uri (git-reference
             (url "https://git.zx2c4.com/wireguard-go/")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fbc6m0ahifhrd6jdrpdxi8l3b2slpp8fmv20kpq2yzz19vzzgkf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.zx2c4.com/wireguard"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v"
                          ;; "./tune/..." ; Requires gvisor.dev/gvisor, not packed yet
                          "./"
                          "./conn/..."
                          "./device/..."
                          "./ipc/..."
                          "./ratelimiter/..."
                          "./replay/..."
                          "./rwcancel/..."
                          "./tai64n/..."))))))))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://git.zx2c4.com/wireguard")
    (synopsis "Implementation of WireGuard in Go")
    (description "This package is a Go Implementation of WireGuard.")
    (license license:expat)))

;; XXX: This repository has been archived by the owner on Feb 27, 2023. It is
;; now read-only and it is DEPRECATED.
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
     (list
      ;; XXX: We strongly encourage users of square/go-jose to migrate to v3
      ;; of go-jose/go-jose. No support, security fixes or updates will be
      ;; delivered to the v1/v2 branches in the Square repository.
      #:tests? #f
      #:import-path "gopkg.in/square/go-jose.v2"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-stretchr-testify))
    (home-page "https://gopkg.in/square/go-jose.v2")
    (synopsis "Implementation of JOSE standards (JWE, JWS, JWT) in Go")
    (description
     "This package aims to provide an implementation of the Javascript Object
Signing and Encryption set of standards.  This includes support for JSON Web
Encryption, JSON Web Signature, and JSON Web Token standards.")
    (license license:asl2.0)))

(define-public go-mvdan-cc-xurls
  (package
    (name "go-mvdan-cc-xurls")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mvdan/xurls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1516hwlxbnhdca56qy7sx9h2n5askq6ddqpqyp3f5rvmzdkxf4zn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "mvdan.cc/xurls/v2"))
    (propagated-inputs
     (list go-github-com-rogpeppe-go-internal
           go-golang-org-x-mod
           go-golang-org-x-sync))
    (home-page "https://mvdan.cc/xurls/v2/")
    (synopsis "Extracts URLs from text")
    (description
     "Xurls extracts urls from plain text using regular expressions.  It can
be used as both a binary and a library.")
    (license license:bsd-3)))

(define-public go-nhooyr-io-websocket
  (package
    (name "go-nhooyr-io-websocket")
    (version "1.8.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nhooyr/websocket")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "019pm2bkzwyvzl61127nqzihchk35q5xh57wy50aa2syn9214fxm"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests require additional dependencies like `wasmbrowsertest`.
      #:tests? #f
      #:import-path "nhooyr.io/websocket"))
    (home-page "https://nhooyr.io/websocket")
    (synopsis "Minimal and idiomatic WebSocket library for Go")
    (description
     "Package websocket implements the
@@url{https://rfc-editor.org/rfc/rfc6455.html,RFC 6455} @code{WebSocket}
protocol.")
    (license license:isc)))

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

(define-public xurls
  (package
    (inherit go-mvdan-cc-xurls)
    (name "xurls")
    (arguments
     (list
      #:import-path "mvdan.cc/xurls/v2/cmd/xurls"
      #:unpack-path "mvdan.cc/xurls/v2"
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file "testdata/script/version.txtar")))))))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
