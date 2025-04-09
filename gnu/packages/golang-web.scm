;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020-2024 Efraim Flashner <efraim@flashner.co.il>
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
;;; Copyright © 2023 Miguel Ángel Moreno <mail@migalmoreno.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Thomas Ieong <th.ieong@free.fr>
;;; Copyright © 2023 conses <contact@conses.eu>
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Dominic Martinez <dom@dominicm.dev>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Jesse Eisses <jesse@eisses.email>
;;; Copyright © 2024 Roman Scherer <roman@burningswell.com>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2025 André Batista <nandre@riseup.net>
;;; Copyright © 2025 Jussi Timperi <jussi.timperi@iki.fi>
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
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages ipfs)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages specifications)
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

(define-public go-0xacab-org-leap-lb
  (package
    (name "go-0xacab-org-leap-lb")
    (version "0.0.0-20210225193050-570f848edccf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://0xacab.org/leap/lb.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "161bznz4srgvqr7q18z63chps52lvdfnldbf6cgm5sw1ly5vwjwi"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Module name has not been changed upstream.
            (substitute* (find-files "." "\\.go$")
              (("git.autistici.org/ale/lb")
               "0xacab.org/leap/lb"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "0xacab.org/leap/lb"))
    (propagated-inputs
     (list go-github-com-golang-protobuf
           go-google-golang-org-grpc))
    (home-page "https://0xacab.org/leap/lb")
    (synopsis "Smart load balancing Golang library")
    (description
     "This package provides an implementation of smart load balancing of simple,
redirect-based (i.e.  not directly reverse proxied) services. It's an
alternative fork of https://git.autistici.org/ale/lb.")
    (license license:gpl3+)))

(define-public go-0xacab-org-leap-obfsvpn
  (package
    (name "go-0xacab-org-leap-obfsvpn")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://0xacab.org/leap/obfsvpn.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iq3m2j6m9n1h9rkysaj97nnqx65fn7vz9jskl4qa4rwh002pv3d"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; It's the same project.
            (substitute* (find-files "." "\\.go$")
              (("git.torproject.org/pluggable-transports/goptlib.git")
               (string-append "gitlab.torproject.org/tpo/anti-censorship"
                              "/pluggable-transports/goptlib")))))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "0xacab.org/leap/obfsvpn"))
    (native-inputs
     (list go-github-com-spf13-pflag
           go-github-com-spf13-viper))
    (propagated-inputs
     (list go-github-com-labstack-echo-v4
           go-github-com-quic-go-quic-go
           go-github-com-sirupsen-logrus
           go-github-com-xtaci-kcp-go-v5
           go-gitlab-com-yawning-obfs4-git
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib))
    (home-page "https://0xacab.org/leap/obfsvpn")
    (synopsis "OBFS4 client and server proxies")
    (description
     "The @@code{obfsvpn} module contains a Go package that provides server
and client components to use variants of the obfs4 obfuscation protocol.  It
is intended to be used as a drop-in Pluggable Transport for @code{OpenVPN}
connections
(although it can be used for other, more generic purposes).")
    (license license:bsd-2)))

(define-public go-0xacab-org-leap-shapeshifter
  (let ((commit "0aa6226582efb8e563540ec1d3c5cfcd19200474")
        (revision "12"))
    (package
      (name "go-0xacab-org-leap-shapeshifter")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://0xacab.org/leap/shapeshifter")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0m4fla9ppl53k9syms4dsad92wakr74cdvids3xxv3amdh4d1w4i"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "0xacab.org/leap/shapeshifter"))
      (propagated-inputs
       (list go-github-com-operatorfoundation-obfs4
             go-github-com-operatorfoundation-shapeshifter-transports
             go-golang-org-x-net))
      (home-page "https://0xacab.org/leap/shapeshifter")
      (synopsis "Shapeshifter Dispatcher Library")
      (description
       "Shapeshifter provides network protocol shapeshifting technology.  The
purpose of this technology is to change the characteristics of network traffic
so that it is not identified and subsequently blocked by network filtering
devices.")
      (license license:bsd-2))))

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

(define-public go-code-gitea-io-sdk-gitea
  (package
    (name "go-code-gitea-io-sdk-gitea")
    (version "0.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/gitea/go-sdk")
             (commit (go-version->git-ref version
                                          #:subdir "gitea"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15mxaxi1nf6b9qacizf7r55jjcnil7qsdh77qzk6b59qcps940np"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; XXX: Tests are broken
      #:import-path "code.gitea.io/sdk/gitea"
      #:unpack-path "code.gitea.io/sdk"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-42wim-httpsig
           ;; go-github-com-davidmz-go-pageant ; for windows only
           go-github-com-go-fed-httpsig
           go-github-com-hashicorp-go-version
           go-golang-org-x-crypto))
    (home-page "https://code.gitea.io/sdk")
    (synopsis "Gitea Golang SDK client")
    (description
     "Package gitea implements a client for the Gitea API. The version
corresponds to the highest supported version of the gitea API, but
backwards-compatibility is mostly given.")
    (license license:expat)))

(define-public go-git-sr-ht-adnano-go-gemini
  (package
    (name "go-git-sr-ht-adnano-go-gemini")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~adnano/go-gemini")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xxcljhk9abjjdcl1dnxaq7qwvl13rq51bwps5nxlkib7fxgbyyl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~adnano/go-gemini"))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://git.sr.ht/~adnano/go-gemini")
    (synopsis "Gemini protocol in Go")
    (description
     "The @code{gemini} package implements the Gemini protocol in Go.  It
provides an API similar to that of NET/HTTP to facilitate the development of
Gemini clients and servers.")
    (license license:expat)))

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
     (list
      #:embed-files #~(list "prelude.graphql")
      #:import-path "git.sr.ht/~emersion/gqlclient"))
    ;; For the CLI.
    (native-inputs
     (list go-github-com-dave-jennifer
           go-github-com-vektah-gqlparser-v2))
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
@url{https://rfc-editor.org/rfc/rfc8620.html,RFC 8620} published on July
2019.")
    (license license:expat)))

(define-public go-github-com-42wim-httpsig
  (package
    (name "go-github-com-42wim-httpsig")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/42wim/httpsig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r6q3g0ghccnvqsw7g3g18s710q8haq5vzpvhrb48vmbcj0pdyn8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/42wim/httpsig"))
    (propagated-inputs (list go-golang-org-x-crypto))
    (home-page "https://github.com/42wim/httpsig")
    (synopsis "Golang implementation of the HTTP Signatures RFC draft")
    (description
     "This package implements HTTP request and response signing and
verification. Supports the major MAC and asymmetric key signature algorithms.
It has several safety restrictions: One, none of the widely known
non-cryptographically safe algorithms are permitted; Two, the RSA SHA256
algorithms must be available in the binary (and it should, barring export
restrictions); Finally, the library assumes either the Authorizationn or
Signature headers are to be set (but not both).

It's an alternative fork of @url{https://github.com/go-fed/httpsig}.")
    (license license:bsd-3)))

(define-public go-github-com-aki237-nscjar
  (package
    (name "go-github-com-aki237-nscjar")
    (version "0.0.0-20210417074043-bbb606196143")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aki237/nscjar")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vbagy9g795b17lnnkkm2f3pcrkxrzc4qbzc656g2cdkdprdyb4m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aki237/nscjar"))
    (home-page "https://github.com/aki237/nscjar")
    (synopsis "Handle Netscape / Mozilla cookies")
    (description
     "@code{nscjar} is a Go library used to parse and output
Netscape/Mozilla's old-style cookie files.  It also implements a simple cookie
jar struct to manage the cookies added to the cookie jar.")
    (license license:expat)))

(define-public go-github-com-akrylysov-algnhsa
  (package
    (name "go-github-com-akrylysov-algnhsa")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/akrylysov/algnhsa")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12a118j1f4jk5rr9wlyfvhshxylhc234pzwrqlbq9b3kcc7d74yj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/akrylysov/algnhsa"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-aws-aws-lambda-go))
    (home-page "https://github.com/akrylysov/algnhsa")
    (synopsis "AWS Lambda Go @code{net/http} server adapter")
    (description
     "This package implements a functionality to run Go web applications on
AWS Lambda and API Gateway or ALB without changing the existing HTTP
handlers.")
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

(define-public go-github-com-anacrolix-envpprof
  (package
    (name "go-github-com-anacrolix-envpprof")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/envpprof")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "121kk1fq1919f0gcnmaxsk6n8flspxa00pyfwl09dysyivwbpk67"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/envpprof"))
    (propagated-inputs (list go-github-com-anacrolix-log))
    (home-page "https://github.com/anacrolix/envpprof")
    (synopsis "Control HTTP mux via environment variable")
    (description
     "This package implements a functionality to configure Go's pprof features
and default HTTP mux using the environment variable @code{GOPPROF}.
@code{envpprof} has an @code{init} function that will run at process
initialization that checks the value of the @code{GOPPROF} environment
variable.  The variable can contain a comma-separated list of values, for
example @code{GOPPROF=http,block}.")
    (license license:expat)))

(define-public go-github-com-arceliar-ironwood
  (package
    (name "go-github-com-arceliar-ironwood")
    (version "v0.0.0-20241213013129-743fe2fccbd3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Arceliar/ironwood")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nnqn388lg4m9cq24vbgcp4z2wh78mga82p59gqzdl7d2cvpsk56"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Arceliar/ironwood"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/cmd/ironwood-example"))))
          ;; XXX: Replace when go-build-system supports nested path.
          (delete 'build)
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-arceliar-phony
           go-github-com-bits-and-blooms-bitset
           go-github-com-bits-and-blooms-bloom-v3
           go-golang-org-x-crypto))
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

(define-public go-github-com-armon-go-socks5
  (package
    (name "go-github-com-armon-go-socks5")
    (version "0.0.0-20160902184237-e75332964ef5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/armon/go-socks5")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "104w10jf0wlxyxi35hf6frndgf0ybz21h54xjmnkivpb6slycpyq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/armon/go-socks5"))
    (native-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/armon/go-socks5")
    (synopsis "SOCKS5 server in Golang")
    (description
     "This package provides the @code{socks5} package that implements a
@url{http://en.wikipedia.org/wiki/SOCKS,SOCKS5 server}.  SOCKS (Secure
Sockets) is used to route traffic between a client and server through an
intermediate proxy layer.  This can be used to bypass firewalls or NATs.")
    (license license:expat)))

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
    (version "1.32.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iflf2ski7vm2z78wdmbrqpchc3qr50macnf965wmdyfinvx58wn"))
       ;; XXX: It contains a lot of sub packages defined with go.mod, consider
       ;; to pack them separately.
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/aws/aws-sdk-go-v2/service/sqs
            (for-each delete-file-recursively
                      (list "service/sqs"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go-v2"
      #:test-subdirs #~(list ".")))
    (propagated-inputs
     (list go-github-com-jmespath-go-jmespath
           go-github-com-aws-smithy-go))
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
    (version "1.17.48")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit (go-version->git-ref version
                                          #:subdir "credentials"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n3spqncpw6w11pwkqaiq7jyv6dv0229jsbshibg24l2g3accdqi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go-v2/credentials"
      #:unpack-path "github.com/aws/aws-sdk-go-v2"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "test\\.go")
                  (("/bin/sleep") (which "sleep")))))))))
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

(define-public go-github-com-aws-aws-sdk-go-v2-service-secretsmanager
  (package
    (name "go-github-com-aws-aws-sdk-go-v2-service-secretsmanager")
    (version "1.34.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit (go-version->git-ref version
                                          #:subdir "service/secretsmanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n3spqncpw6w11pwkqaiq7jyv6dv0229jsbshibg24l2g3accdqi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go-v2/service/secretsmanager"
      #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-aws-smithy-go))
    (home-page "https://github.com/aws/aws-sdk-go-v2")
    (synopsis "AWS Secrets Manager service")
    (description
     "Package secretsmanager provides the API client, operations, and
parameter types for AWS Secrets Manager.")
    (license license:asl2.0)))

(define-public go-github-com-aws-aws-sdk-go-v2-service-sqs
  (package
    (name "go-github-com-aws-aws-sdk-go-v2-service-sqs")
    (version "1.37.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit (go-version->git-ref version
                                          #:subdir "service/sqs"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "158mgp8czzkhjdwli2wciwqihs56jp879ahjdjyy8c6fn0g3xdvb"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            ;; Consider to implement it as re-usable procedure in
            ;; guix/build/utils or guix/build-system/go.
            (define (delete-all-but directory . preserve)
              (define (directory? x)
                (and=> (stat x #f)
                       (compose (cut eq? 'directory <>) stat:type)))
              (with-directory-excursion directory
                (let* ((pred
                        (negate (cut member <> (append '("." "..") preserve))))
                       (items (scandir "." pred)))
                  (for-each (lambda (item)
                              (if (directory? item)
                                  (delete-file-recursively item)
                                  (delete-file item)))
                            items))))
            (delete-all-but "service" "sqs")
            (delete-all-but "." "service")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go-v2/service/sqs"
      #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-aws-smithy-go
           go-github-com-aws-aws-sdk-go-v2))
    (home-page "https://github.com/aws/aws-sdk-go-v2")
    (synopsis "AWS Golang SDK for Simple Queue Service")
    (description
     "Package sqs provides the API client, operations, and parameter types for
Amazon Simple Queue Service.")
    (license license:asl2.0)))

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
    (version "1.22.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/smithy-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16jbv7cyj85048f4kcrib8k2yif165sc099h0aklal5dwlf85xcg"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/aws/smithy-go/aws-http-auth
            ;; - github.com/aws/smithy-go/codegen
            ;; - github.com/aws/smithy-go/metrics/smithyotelmetrics
            ;; - github.com/aws/smithy-go/tracing/smithyoteltracing
            (for-each delete-file-recursively
                      (list "aws-http-auth"
                            "codegen"
                            "metrics/smithyotelmetrics"
                            "tracing/smithyoteltracing"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/smithy-go"))
    (home-page "https://github.com/aws/smithy-go")
    (synopsis "Smithy code generators for Go")
    (description
     "Package smithy provides the core components for a Smithy SDK.")
    (license license:asl2.0)))

(define-public go-github-com-aws-smithy-go-aws-http-auth
  (package
    (name "go-github-com-aws-smithy-go-aws-http-auth")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/smithy-go")
             (commit (go-version->git-ref version
                                          #:subdir "aws-http-auth"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iy9rlcj6qwy58rrddbvqy38lzw9d7y1i2d3mvf3f3z4i6rkwvd4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/aws/smithy-go/aws-http-auth"
      #:unpack-path "github.com/aws/smithy-go"))
    (home-page "https://github.com/aws/smithy-go")
    (synopsis "Consumable SigV4 and SigV4a request signing")
    (description
     "This package implements generically consumable SigV4 and SigV4a request
signing.")
    (license license:asl2.0)))

(define-public go-github-com-aws-smithy-go-codegen
  (package
    (name "go-github-com-aws-smithy-go-codegen")
    (version "0.0.0-20241226171254-10fbeed6f845")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/smithy-go")
             (commit (go-version->git-ref version
                                          #:subdir "codegen"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16jbv7cyj85048f4kcrib8k2yif165sc099h0aklal5dwlf85xcg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/aws/smithy-go/codegen"
      #:unpack-path "github.com/aws/smithy-go"))
    (home-page "https://github.com/aws/smithy-go")
    (synopsis "Smithy code generators for Golang")
    (description
     "This package provides Gradle templates for Smithy code generators.")
    (license license:asl2.0)))

(define-public go-github-com-aws-smithy-go-metrics-smithyotelmetrics
  (package
    (name "go-github-com-aws-smithy-go-metrics-smithyotelmetrics")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/smithy-go")
             (commit (go-version->git-ref version
                                          #:subdir "metrics/smithyotelmetrics"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16jbv7cyj85048f4kcrib8k2yif165sc099h0aklal5dwlf85xcg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/smithy-go/metrics/smithyotelmetrics"
      #:unpack-path "github.com/aws/smithy-go"))
    (propagated-inputs
     (list go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel)) ; for go.opentelemetry.io/otel/metric
    (home-page "https://github.com/aws/smithy-go")
    (synopsis "AWS Smithy OTEL metrics adapter")
    (description
     "Package smithyotelmetrics implements a Smithy client metrics adapter for
the OTEL Go SDK.")
    (license license:asl2.0)))

(define-public go-github-com-aws-smithy-go-tracing-smithyoteltracing
  (package
    (name "go-github-com-aws-smithy-go-tracing-smithyoteltracing")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/smithy-go")
             (commit (go-version->git-ref version
                                          #:subdir "tracing/smithyoteltracing"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16jbv7cyj85048f4kcrib8k2yif165sc099h0aklal5dwlf85xcg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/smithy-go/tracing/smithyoteltracing"
      #:unpack-path "github.com/aws/smithy-go"))
    (propagated-inputs
     (list go-go-opentelemetry-io-otel))
    (home-page "https://github.com/aws/smithy-go")
    (synopsis "AWS Smithy OTEL tracing adapter")
    (description
     "Package smithyoteltracing implements a Smithy client tracing adapter for
the OTEL Go SDK.")
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

(define-public go-github-com-babolivier-go-doh-client
  (package
    (name "go-github-com-babolivier-go-doh-client")
    (version "0.0.0-20201028162107-a76cff4cb8b6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/babolivier/go-doh-client")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ca72jz5d5wf5hkcjiwrjvh4fp9p0nqhgwyx9p3vq9sdrx524d21"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/babolivier/go-doh-client"))
    (home-page "https://github.com/babolivier/go-doh-client")
    (synopsis "DNS over HTTPS client implementation written in Golang")
    (description
     "Package doh implements client operations for @code{DoH} (DNS over HTTPS)
lookups.  It implements looking up the following records:
@itemize
@item A
@item AAAA
@item CNAME
@item MX
@item NS
@item TXT
@item SRV
@item SOA
@item PTR
@end itemize")
    (license license:gpl3)))

(define-public go-github-com-beevik-ntp
  (package
    (name "go-github-com-beevik-ntp")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/beevik/ntp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n2mr4nnbsv5f0w3hkk3kmyn3wd2xqi5zxgcm8s50fdizk0nqmi9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/beevik/ntp"
      ;; Tests requir access to <0.beevik-ntp.pool.ntp.org:53>.
      #:test-flags #~(list "-skip" "TestOnlineQuery|TestOnlineTime")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/beevik/ntp")
    (synopsis "NTP client package for Golang")
    (description
     "Package ntp provides an implementation of a Simple NTP (SNTP) client
capable of querying the current time from a remote NTP server as specified in
@url{https://rfc-editor.org/rfc/rfc5905.html, RFC 5905}.")
    (license license:bsd-2)))

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

(define-public go-github-com-bradenhilton-mozillainstallhash
  (package
    (name "go-github-com-bradenhilton-mozillainstallhash")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bradenhilton/mozillainstallhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j54ssnvk2vy77g23zrhi9acwblnamj63i7wmmdnb89fjg0xc9km"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bradenhilton/mozillainstallhash"))
    (propagated-inputs
     (list go-github-com-bradenhilton-cityhash
           go-golang-org-x-text))
    (home-page "https://github.com/bradenhilton/mozillainstallhash")
    (synopsis "Generates the hash used in installs of Mozilla software")
    (description
     "This package provides a functionality to get the hash used to
differentiate between installs of Mozilla software in @code{installs.ini} and
@code{profiles.ini}.")
    (license license:expat)))

(define-public go-github-com-caddyserver-certmagic
  (package
    (name "go-github-com-caddyserver-certmagic")
    (version "0.21.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/caddyserver/certmagic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "061whx9p00lpxlfnywizqx5z9b020ggqg5vx5r5v2qhdrprg1gkz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-skip"
              ;; Some tests require networking to run so skip them altogether.
              (string-join
               (list "TestLookupNameserversOK/physics.georgetown.edu."
                     "TestFindZoneByFqdn/domain_is_a_CNAME"
                     "TestFindZoneByFqdn/domain_is_a_non-existent_subdomain"
                     "TestFindZoneByFqdn/domain_is_a_eTLD"
                     "TestFindZoneByFqdn/domain_is_a_cross-zone_CNAME"
                     "TestFindZoneByFqdn/NXDOMAIN"
                     "TestFindZoneByFqdn/several_non_existent_nameservers")
               "|"))
      #:import-path "github.com/caddyserver/certmagic"))
    (propagated-inputs
     (list go-github-com-caddyserver-zerossl
           go-github-com-klauspost-cpuid-v2
           go-github-com-libdns-libdns
           go-github-com-mholt-acmez
           go-github-com-miekg-dns
           go-github-com-zeebo-blake3
           go-go-uber-org-zap
           go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://github.com/caddyserver/certmagic")
    (synopsis "Automatic HTTPS for any Go program")
    (description
     "@code{certmagic} provides API for TLS Automation with full control over almost
every aspect of the system.

Main features:
@itemize
@item Fully automated certificate management including issuance and renewal, with
support for certificate revocation.  Also works in conjunction with your own
certificates.
@item Wildcard certificates.
@item One-line, fully managed HTTPS servers, with HTTP->HTTPS redirects.
@item Multiple issuers supported: get certificates from multiple sources/CAs for
redundancy and resiliency.
@item Solves all 3 common ACME challenges: HTTP, TLS-ALPN, and DNS (and capable of
others.)
@item Robust error handling:
@itemize
@item Challenges are randomized to avoid accidental dependence and rotated to
overcome certain network blockages.
@item Robust retries for up to 30 days.
@item Exponential backoff with carefully-tuned intervals.
@item Retries with optional test/staging CA endpoint instead of production, to avoid
rate limits.
@end itemize
@item All libdns DNS providers work out-of-the-box.
@item Pluggable storage backends (default: file system) and key sources.
@item Automatic OCSP stapling.
@item Distributed solving of all challenges (works behind load balancers.)
@item Supports @samp{on-demand} issuance of certificates.
@item Optional event hooks for observation.
@item One-time private keys by default (new key for each cert) to discourage pinning
and reduce scope of key compromise.
@item Works with any certificate authority (CA) compliant with the ACME specification
@url{https://tools.ietf.org/html/rfc8555, RFC 8555}.
@item Must-Staple (optional; not default.)
@item Full support for draft-ietf-acme-ari (ACME Renewal Information; ARI) extension.
@end itemize")
    (license license:expat)))

(define-public go-github-com-caddyserver-zerossl
  (package
    (name "go-github-com-caddyserver-zerossl")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/caddyserver/zerossl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hr2kdabhm35hz5krp7m3g6wxvyb9xlqgmy3krf4wwb3yabsqp1m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/caddyserver/zerossl"))
    (home-page "https://github.com/caddyserver/zerossl")
    (synopsis "ZeroSSL REST API client implementation for Go")
    (description
     "@code{zerossl} implements the @url{https://zerossl.com/documentation/api/,
ZeroSSL REST API}.

The REST API is distinct from the @url{https://zerossl.com/documentation/acme/, ACME
endpoint}, which is a standardized way of obtaining certificates.")
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
    (description
     ;; URL https://raw.githubusercontent.com/googleapis/google-http-java-client
     ;; Commit da1aa993e90285ec18579f1553339b00e19b3ab5
     ;; Dirrectory google-http-client/src/main/java/com/google/api/client/util
     ;; File ExponentialBackOff.java
     "This is a Go port of the exponential backoff algorithm from
Google's HTTP Client Library for Java.

@url{http://en.wikipedia.org/wiki/Exponential_backoff, Exponential backoff} is
an algorithm that uses feedback to multiplicatively decrease the rate of some
process, in order to gradually find an acceptable rate.  The retries
exponentially increase and stop increasing when a certain threshold is met.")
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

(define-public go-github-com-circonus-labs-circonus-gometrics
  (package
    (name "go-github-com-circonus-labs-circonus-gometrics")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/circonus-labs/circonus-gometrics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s2wir711h0k2h8xsypgpzshccnx8jkwjfni7n32l7wd8yng9ngs"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Module name has been changed upstream, permament redirect:
            ;; <https://github.com/circonus-labs/circonusllhist> ->
            ;; <https://github.com/openhistogram/circonusllhist>.
            (substitute* (find-files "." "\\.go$")
              (("github.com/circonus-labs/circonusllhist")
               "github.com/openhistogram/circonusllhist"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags #~(list "-skip" "TestNew|TestFlushMetrics|TestPromOutput")
      #:import-path "github.com/circonus-labs/circonus-gometrics"))
    (propagated-inputs
     (list go-github-com-hashicorp-go-retryablehttp
           go-github-com-openhistogram-circonusllhist
           go-github-com-pkg-errors
           go-github-com-tv42-httpunix))
    (home-page "https://github.com/circonus-labs/circonus-gometrics")
    (synopsis "Circonus metrics tracking for Golang")
    (description
     "This library supports named counters, gauges and histograms.  It also
provides convenience wrappers for registering latency instrumented functions
with Go's builtin http server.")
    (license license:bsd-3)))

(define-public go-github-com-circonus-labs-circonus-gometrics-v3
  (package
    (inherit go-github-com-circonus-labs-circonus-gometrics)
    (name "go-github-com-circonus-labs-circonus-gometrics-v3")
    (version "3.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/circonus-labs/circonus-gometrics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wdnhj1xwm5p5wp76afs08aq30hkpgnq4802d6ylnpb3n46v0lj4"))))
    (build-system go-build-system)
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-circonus-labs-circonus-gometrics)
       ((#:import-path _) "github.com/circonus-labs/circonus-gometrics/v3")))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs
                     go-github-com-circonus-labs-circonus-gometrics)
       (prepend go-github-com-circonus-labs-go-apiclient)))))

(define-public go-github-com-circonus-labs-go-apiclient
  (package
    (name "go-github-com-circonus-labs-go-apiclient")
    (version "0.7.24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/circonus-labs/go-apiclient")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ysfycnjmqkn1prlz68k2nkrkk3570q5gx0d6vdvvwfhvlisj4c7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/circonus-labs/go-apiclient"))
    (propagated-inputs
     (list go-github-com-hashicorp-go-retryablehttp
           go-github-com-pkg-errors))
    (home-page "https://github.com/circonus-labs/go-apiclient")
    (synopsis "Circonus API Client for Golang")
    (description
     "Package apiclient provides methods for interacting with the Circonus
API.  See the full Circonus API Documentation at
@url{https://login.circonus.com/resources/api} for more information.")
    (license license:bsd-3)))

(define-public go-github-com-cli-browser
  (package
    (name "go-github-com-cli-browser")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cli/browser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f54ap2a4df32bwrwqsasfsikkxngkk4wr2wfbsns4lf0yymra6k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cli/browser"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/cli/browser")
    (synopsis "Helpers to open URLs, files, or readers in a web browser")
    (description
     "Package browser provides helpers to open files, readers, and URLs in a
browser window.")
    (license license:bsd-2)))

(define-public go-github-com-coder-websocket
  (package
    (name "go-github-com-coder-websocket")
    (version "1.8.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coder/websocket")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "172v9mqghswf50ga512qghb6ii0ivz5fi2iyjzdnbm42g0cr4fjj"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/coder/websocket/internal/thirdparty
            (delete-file-recursively "internal/thirdparty")))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/coder/websocket"))
    (native-inputs
     (list go-golang-org-x-time)) ; for examples
    (home-page "https://github.com/coder/websocket")
    (synopsis "WebSocket library for Go")
    (description
     "@code{websocket} is a minimal and idiomatic
@url{https://websockets.spec.whatwg.org/, WebSocket} library for Go.

Main features:
@itemize
@item Minimal and idiomatic API.
@item First class @url{https://blog.golang.org/context, context.Context}
support.
@item Fully passes the WebSocket
@url{https://github.com/crossbario/autobahn-testsuite, autobahn-testsuite}.
@item @url{https://pkg.go.dev/github.com/coder/websocket?tab=imports,
Zero dependencies}.
@item JSON helpers in the @url{https://pkg.go.dev/github.com/coder/websocket/wsjson,
wsjson} subpackage
@item Zero alloc reads and writes.
@item Concurrent writes.
@item @url{https://pkg.go.dev/github.com/coder/websocket#Conn.Close,
Close handshake}.
@item @url{https://pkg.go.dev/github.com/coder/websocket#NetConn, net.Conn}
wrapper
@item @url{https://pkg.go.dev/github.com/coder/websocket#Conn.Ping, Ping pong}
API.
@item @url{https://tools.ietf.org/html/rfc7692, RFC 7692} permessage-deflate
compression.
@item @url{https://pkg.go.dev/github.com/coder/websocket#Conn.CloseRead,
CloseRead} helper for write only connections
@item Compile to @url{https://pkg.go.dev/github.com/coder/websocket#hdr-Wasm,
Wasm}.
@end itemize
")
    (license license:isc)))

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
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coreos/go-oidc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "180wnxiim622v17xcnrjrg9g07mg4xizmlxxyrl9p42is0abi9c8"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/coreos/go-oidc"))
    (native-inputs
     (list go-golang-org-x-net))
    (propagated-inputs
     (list go-github-com-pquerna-cachecontrol
           go-golang-org-x-oauth2
           go-gopkg-in-go-jose-go-jose-v2))
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

(define-public go-github-com-cretz-bine
  (package
    ;; This package can be used with CGO to statically compile Tor.  This
    ;; package expects <https://github.com/cretz/tor-static> to be cloned at
    ;; $GOPATH/src/github.com/cretz/tor-static as if it was fetched with go
    ;; get.  If you use go modules the expected path would be
    ;; $GOPATH/pkg/mod/github.com/cretz/tor-static libs.  See
    ;; <https://github.com/cretz/bine/blob/v0.2.0/process/embedded/process.go#L7>.
    (name "go-github-com-cretz-bine")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cretz/bine")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16h7j7v4qbwb7zjsbc1p3b67xji7hgis95znz9cj8fw3rqxwvkcs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cretz/bine"
      #:test-subdirs #~(list "tests/..." "torutil/...")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://github.com/cretz/bine")
    (synopsis "Accessing and embedding Tor clients and servers from Golang")
    (description
     "Bine is a toolkit to assist in creating Tor clients and servers.
Features:
@itemize
@item full support for the Tor controller API
@item support for @code{net.Conn} and @code{net.Listen} style APIs
@item supports statically compiled Tor to embed Tor into the binary
@item supports v3 onion services
@item support for embedded control socket in Tor >= 0.3.5
@end itemize")
    (license license:expat)))

(define-public go-github-com-datadog-datadog-go
  (package
    (name "go-github-com-datadog-datadog-go")
    (version "4.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DataDog/datadog-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03dc3ld9zyynhmslzlciry6rs06hvd1c5finjip9vj300xaybazl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/DataDog/datadog-go"
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
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/DataDog/datadog-go")
    (synopsis "Golang client library to work with DataDog's API")
    (description
     "@code{datadog-go} is a library that provides a
@url{https://docs.datadoghq.com/developers/dogstatsd/?code-lang=go,@code{DogStatsD}}
client in Golang.")
    (license license:expat)))

(define-public go-github-com-datadog-datadog-go-v5
  (package
    (inherit go-github-com-datadog-datadog-go)
    (name "go-github-com-datadog-datadog-go-v5")
    (version "5.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DataDog/datadog-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05cw8n2hv8sa6s4qi4xkkv75y9bzn5qdqx1hv5g9h49cw92rkcas"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-datadog-datadog-go)
       ((#:import-path _) "github.com/DataDog/datadog-go/v5")))
    (native-inputs
     (modify-inputs (package-native-inputs go-github-com-datadog-datadog-go)
       (append go-github-com-golang-mock)))))

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
     (list
      #:import-path "github.com/davecgh/go-xdr/xdr2"
      #:unpack-path "github.com/davecgh/go-xdr"))
    (home-page "https://github.com/davecgh/go-xdr")
    (synopsis "Pure Go implementation of the XDR standard")
    (description
     "@code{go-xdr} implements the data representation portion of the External
Data Representation (XDR) standard protocol as specified in RFC
4506 (obsoletes RFC 1832 and RFC 1014) in pure Go.")
    (license license:isc)))

(define-public go-github-com-digitalocean-godo
  (package
    (name "go-github-com-digitalocean-godo")
    (version "1.138.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/digitalocean/godo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "083vhzb1hwzdmn5m14ygs949g2kabmafvpcxq2laylkylq1fd3rm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/digitalocean/godo"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Tests requiring networking setup.
                       (list "TestRegistry_DeleteManifest"
                             "TestRegistry_DeleteTag"
                             "TestRegistry_ListManifests"
                             "TestRepository_ListTags")
                       "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-go-querystring
           go-github-com-hashicorp-go-retryablehttp
           go-golang-org-x-oauth2
           go-golang-org-x-time))
    (home-page "https://github.com/digitalocean/godo")
    (synopsis "DigitalOcean Go API client")
    (description
     "Package godo is the @code{DigitalOcean} API v2 client for Go.")
    (license (list license:expat license:bsd-3))))

(define-public go-github-com-dimfeld-httptreemux
  (package
    (name "go-github-com-dimfeld-httptreemux")
    (version "4.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dimfeld/httptreemux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hs22xfipld6xqc0yqih6llm0m0k64slw12vhrx51r2dz91mjjrz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dimfeld/httptreemux"))
    (home-page "https://github.com/dimfeld/httptreemux")
    (synopsis "Tree-based HTTP router for Go")
    (description
     "This package provides a re-implementation of
@url{https://github.com/julienschmidt/httprouter, Julien Schmidt's
httprouter}, in that it uses a patricia tree, but the logic is rather
different.  Specifically, the routing rules are relaxed so that a single path
segment may be a wildcard in one route and a static token in another.")
    (license license:expat)))

(define-public go-github-com-dimfeld-httptreemux-v5
  (package
    (inherit go-github-com-dimfeld-httptreemux)
    (name "go-github-com-dimfeld-httptreemux-v5")
    (version "5.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dimfeld/httptreemux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02q700rrf9mr1bcc3nw0qh186lhv17rsmblajsgifj42n72h0llf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dimfeld/httptreemux/v5"))))

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
     (list
      #:import-path "github.com/docker/go-connections"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;;  Unable to verify certificate 1: x509: certificate signed by
               ;;  unknown authority.
               (list "TestConfigClientExclusiveRootPools"
                     "TestConfigServerExclusiveRootPools")
               "|"))))
    (home-page "https://github.com/docker/go-connections")
    (synopsis "Networking library for Go")
    (description
     "This package provides a library to work with network connections in the
Go language.  In particular it provides tools to deal with network address
translation (NAT), proxies, sockets, and transport layer security (TLS).")
    (license license:asl2.0)))

(define-public go-github-com-dpotapov-go-spnego
  (package
    (name "go-github-com-dpotapov-go-spnego")
    (version "0.0.0-20220426193508-b7f82e4507db")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dpotapov/go-spnego")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rpcgzkqhdwfsi8a9f9qar16i663pdx3gvwd6c0vfppy7qjmpjfr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dpotapov/go-spnego"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-jcmturner-gokrb5-v8
           go-golang-org-x-net))
    (home-page "https://github.com/dpotapov/go-spnego")
    (synopsis "HTTP calls with Kerberos authentication")
    (description
     "The package extends Go's HTTP Transport allowing Kerberos
authentication through Negotiate mechanism (see
@url{https://tools.ietf.org/html/rfc4559, RFC4559}).")
    (license license:expat)))

(define-public go-github-com-elazarl-go-bindata-assetfs
  (package
    (name "go-github-com-elazarl-go-bindata-assetfs")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elazarl/go-bindata-assetfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05j8gy417gcildmxa04m8ylriaakadr7zvwn2ggq56pdg7b63knc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/elazarl/go-bindata-assetfs"))
    (home-page "https://github.com/elazarl/go-bindata-assetfs")
    (synopsis "Serves embedded files with @code{net/http}")
    (description
     "assetfs allows packages to serve static content embedded with the
@url{https://github.com/go-bindata/go-bindata, go-bindata} tool with the
standard @code{net/http} package.")
    (license license:bsd-2)))

(define-public go-github-com-elazarl-goproxy
  (package
    (name "go-github-com-elazarl-goproxy")
    (version "0.0.0-20241221210044-9faedc2f9e9f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elazarl/goproxy")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j3v0y18igr3wy9vbwyg19fzy12jc41kmpfcz2jh1dnk6kxn2n67"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/elazarl/goproxy"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Networking or curl are required.
                       (list "TestCurlMinusP"
                             "TestSimpleHttpRequest"
                             "TestBasicConnectAuthWithCurl"
                             "TestBasicAuthWithCurl"
                             "TestConstantImageHandler"
                             "TestImageHandler"
                             "TestReplaceImage")
                       "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/elazarl/goproxy")
    (synopsis "HTTP proxy library for Go")
    (description
     "GoProxy is a library to create a customized HTTP/HTTPS proxy server
using Go (aka Golang), with several configurable settings available.  The
target of this project is to offer an optimized proxy server, usable with
reasonable amount of traffic, yet customizable and programmable.")
    (license license:bsd-3)))

(define-public go-github-com-ema-qdisc
  (package
    (name "go-github-com-ema-qdisc")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ema/qdisc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v2k2z1xjxfa0qrrnafvb51dxwxm0s3nbsi6n64cm2rylxjskfmy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ema/qdisc"))
    (propagated-inputs
     (list go-github-com-mdlayher-netlink))
    (home-page "https://github.com/ema/qdisc")
    (synopsis "Queuing discipline information via netlink")
    (description
     "Package @code{qdisc} allows getting queuing discipline information via
netlink,similarly to what @code{tc -s qdisc show} does.")
    (license license:expat)))

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

(define-public go-github-com-emersion-go-imap-id
  (let ((commit "f94a56b9ecde7e39e7ea38d62c745b557cb94139")
        (revision "0"))
    (package
      (name "go-github-com-emersion-go-imap-id")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emersion/go-imap-id")
               (commit commit)))
         (sha256
          (base32 "1pi87xq6nrb1kdf4za4xp8cfkpwr3p93kjrmzmnr4z0j90y26vfi"))
         (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/emersion/go-imap-id"))
      (propagated-inputs (list go-github-com-emersion-go-imap))
      (home-page "https://github.com/emersion/go-imap-id")
      (synopsis "ID extension for go-imap")
      (description
       "This package provides an ID extension for go-imap as specified in
@url{https://www.rfc-editor.org/rfc/rfc2971, RFC 2971}.")
      (license license:expat))))

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

(define-public go-github-com-emicklei-go-restful-v3
  (package
    (name "go-github-com-emicklei-go-restful-v3")
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
     (list
      #:import-path "github.com/emicklei/go-restful"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (home-page "https://github.com/emicklei/go-restful")
    (synopsis "Build REST-style web services using Go")
    (description
     "This package provides @code{go-restful}, which helps developers to use
@code{http} methods explicitly and in a way that's consistent with the HTTP
protocol definition.")
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

(define-public go-github-com-fasthttp-router
  (package
    (name "go-github-com-fasthttp-router")
    (version "1.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fasthttp/router")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1szc2s0jbk6jivgfmgxy7iykwqd6b0033jnnr0l47vyxbw7q8zvg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fasthttp/router"))
    (propagated-inputs
     (list go-github-com-savsgio-gotils
           go-github-com-valyala-bytebufferpool
           go-github-com-valyala-fasthttp))
    (home-page "https://github.com/fasthttp/router")
    (synopsis "Router implementation for fasthttp")
    (description
     "Package router is a trie based high performance HTTP request router.")
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

;; This project looks like dormant or abandoned, see
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
      #:import-path "github.com/francoispqt/gojay"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestMessage_Unmarshal"
                             "TestMessage_Marshal"
                             "TestGenerator_Generate")
                       "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples-and-benchmarks
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "benchmarks" "examples"))))))))
    (native-inputs
     (list go-github-com-go-errors-errors ; for CLI build
           go-github-com-stretchr-testify
           go-github-com-viant-assertly   ; for CLI build
           go-github-com-viant-toolbox))  ; for CLI build
    (home-page "https://github.com/francoispqt/gojay")
    (synopsis "JSON encoder/decoder with powerful stream API for Golang")
    (description
     "GoJay is a performant JSON encoder/decoder for Golang.  It has a simple
API and doesn't use reflection.  It relies on small interfaces to
decode/encode structures and slices.")
    (license license:expat)))

(define-public go-github-com-gaissmai-bart
  (package
    (name "go-github-com-gaissmai-bart")
    (version "0.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gaissmai/bart")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rnbmykmkl0c1fzz4vkv7q72l7hl2xpmalbm41f4lifdjscx5nk7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gaissmai/bart"))
    (propagated-inputs
     (list go-github-com-bits-and-blooms-bitset))
    (home-page "https://github.com/gaissmai/bart")
    (synopsis "Balanced Routing Table in Golang")
    (description "This package provides a Balanced-Routing-Table (BART).")
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

(define-public go-github-com-getkin-kin-openapi
  (package
    (name "go-github-com-getkin-kin-openapi")
    (version "0.128.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/getkin/kin-openapi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "162hxwqywpbmfybyj1m0s4a0nxx0qldx90k49d2mf52xps1jp5p2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/getkin/kin-openapi"
      #:test-flags
      ;; They try to access network.
      #~(list "-skip" "TestIssue495WithDraft04|TestExtraSiblingsInRemoteRef")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-openapi-jsonpointer
           go-github-com-gorilla-mux
           go-github-com-invopop-yaml
           go-github-com-mohae-deepcopy
           go-github-com-perimeterx-marshmallow
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/getkin/kin-openapi")
    (synopsis "OpenAPI 3.0 and Swagger v2 implementation for Golang")
    (description
     "This package implements a functionality for parsing, converting and
validating
@url{https://github.com/OAI/OpenAPI-Specification/blob/main/versions/2.0.md,
OpenAPI v2.0},
@url{https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.0.3.md,
OpenAPI v3.0} and
@url{https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md,
OpenAPI v3.1}.")
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

(define-public go-github-com-gin-contrib-sse
  (package
    (name "go-github-com-gin-contrib-sse")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gin-contrib/sse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "072nq91a65n5xvwslqjyvydfd0mfpnvb3vwjyfvmzm1ym96wr1nd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gin-contrib/sse"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/gin-contrib/sse")
    (synopsis "Server-Sent Events implementation in Golang")
    (description
     "@acronym{Server-sent events, SSE} is a technology where a browser
receives automatic updates from a server via HTTP connection.  The SSE
@code{EventSource} API is
@url{http://www.w3.org/TR/2009/WD-eventsource-20091029/,standardized as part
of HTML5[1] by the W3C}.")
    (license license:expat)))

(define-public go-github-com-gin-gonic-gin
  (package
    (name "go-github-com-gin-gonic-gin")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gin-gonic/gin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01xjvw2d46b77jnszgbwqbdzh9jx7y3h5ik3q30y9dn9gaq5mhks"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gin-gonic/gin"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-bytedance-sonic
           go-github-com-gin-contrib-sse
           go-github-com-go-playground-validator-v10
           go-github-com-goccy-go-json
           go-github-com-json-iterator-go
           go-github-com-mattn-go-isatty
           go-github-com-pelletier-go-toml-v2
           go-github-com-ugorji-go-codec
           go-golang-org-x-net
           go-google-golang-org-protobuf
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/gin-gonic/gin")
    (synopsis "HTTP web framework")
    (description
     "This package provides a Golang web framework wit martini-like API.

Features:
@itemize
@item zero allocation router
@item middleware support
@item crash-free
@item JSON validation
@item routes grouping
@item error management
@item rendering built-in
@item extendable
@end itemize")
    (license license:expat)))

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

(define-public go-github-com-go-fed-httpsig
  (package
    (name "go-github-com-go-fed-httpsig")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-fed/httpsig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h2yk2ih8vrma8zrs1z8bd4r48hbqdwhgbqykrs4siyj9c80ykd2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-fed/httpsig"
      ;; algorithms_test.go:153: "sha1": got true, want false
      #:test-flags #~(list "-skip" "TestIsAvailable")))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/go-fed/httpsig")
    (synopsis "Golang implementation of the HTTP Signatures RFC draft")
    (description
     "This package implements HTTP request and response signing and verification.
Supports the major MAC and asymmetric key signature algorithms.  It has
several safety restrictions: One, none of the widely known
non-cryptographically safe algorithms are permitted; Two, the RSA SHA256
algorithms must be available in the binary (and it should, barring export
restrictions); Finally, the library assumes either the Authorizationn or
Signature headers are to be set (but not both).")
    (license license:bsd-3)))

(define-public go-github-com-go-http-utils-headers
  (package
    (name "go-github-com-go-http-utils-headers")
    (version "0.0.0-20181008091004-fed159eddc2a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-http-utils/headers")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19h2sffi04hr56qxkag2baa17v91x4vp1a1zkm9rqr846xqwspvm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-http-utils/headers"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-http-utils/headers")
    (synopsis "HTTP header constants for Golang")
    (description
     "This package provides HTTP header constants.")
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
        (base32 "0kbkplhzqv9ai28r4smhdsxxwh20d96srr3am37pwwnh48ivwch8"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/go-jose/go-jose/jose-util
            (delete-file-recursively "jose-util")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-jose/go-jose/v3"))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-golang-org-x-crypto))
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

(define-public go-github-com-go-ldap-ldap-v3
  (package
    (name "go-github-com-go-ldap-ldap-v3")
    (version "3.4.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ldap/ldap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qcm4piyk7l5n3kplcism0y7zp40xcfmjl04hw1s276qqf7vi6hg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-ldap/ldap/v3"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestCompare"
                             "TestExtendedRequest_WhoAmI"
                             "TestExtendedRequest_FastBind"
                             "TestMatchDNError"
                             "TestMultiGoroutineSearch"
                             "TestSearch"
                             "TestSearchAsync"
                             "TestSearchAsyncAndCancel"
                             "TestSearchStartTLS"
                             "TestSearchWithPaging"
                             "TestSecureDialURL"
                             "TestStartTLS"
                             "TestTLSConnectionState"
                             "TestUnsecureDialURL")
                       "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-azure-go-ntlmssp
           go-github-com-go-asn1-ber-asn1-ber
           go-github-com-google-uuid
           go-github-com-jcmturner-gokrb5-v8))
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
      #:embed-files #~(list "jsonschema-draft-04\\.json" "schema\\.json")
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
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
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
      #:embed-files #~(list "jsonschema-draft-04\\.json" "schema\\.json")
      #:import-path "github.com/go-openapi/loads"))
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

(define-public go-github-com-go-openapi-runtime
  (package
    (name "go-github-com-go-openapi-runtime")
    (version "0.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/runtime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h8yqc6bb8mzb8jvr3m08fyws1gbrhbry6k5vj2cx2xdi50kqiy5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list "jsonschema-draft-04\\.json" "schema\\.json")
      #:import-path "github.com/go-openapi/runtime"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-docker-go-units
           go-github-com-go-openapi-analysis
           go-github-com-go-openapi-errors
           go-github-com-go-openapi-loads
           go-github-com-go-openapi-spec
           go-github-com-go-openapi-strfmt
           go-github-com-go-openapi-swag
           go-github-com-go-openapi-validate
           go-github-com-opentracing-opentracing-go
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-sdk
           go-golang-org-x-sync
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/go-openapi/runtime")
    (synopsis "OpenAPI runtime interfaces")
    (description
     "OpenAPI toolkit runtime component for use in code generation or as
untyped usage.")
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
      #:embed-files #~(list "jsonschema-draft-04\\.json" "schema\\.json")
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
                                "doc_test.go"))))))))
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

(define-public go-github-com-gobwas-httphead
  (package
    (name "go-github-com-gobwas-httphead")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gobwas/httphead")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "106l8ml5yihld3rrf45q5fhlsx64hrpj2dsvnnm62av4ya5nf0gb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gobwas/httphead"))
    (home-page "https://github.com/gobwas/httphead")
    (synopsis "Tiny HTTP header value parsing library in Golang")
    (description
     "Package httphead contains utils for parsing HTTP and HTTP-grammar
compatible text protocols headers.")
    (license license:expat)))

(define-public go-github-com-gobwas-ws
  (package
    (name "go-github-com-gobwas-ws")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gobwas/ws")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nqgb75cizx11igwjqx6b6mlzl7yxy6x683m9aaalgcx9n1qxan7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gobwas/ws"))
    (propagated-inputs
     (list go-github-com-gobwas-httphead
           go-github-com-gobwas-pool))
    (home-page "https://github.com/gobwas/ws")
    (synopsis "Tiny WebSocket library for Golang")
    (description
     "Package ws implements a client and server for the @code{WebSocket}
protocol as specified in @url{https://rfc-editor.org/rfc/rfc6455.html, RFC
6455}.")
    (license license:expat)))

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

(define-public go-github-com-gofiber-fiber-v2
  (package
    (name "go-github-com-gofiber-fiber-v2")
    (version "2.52.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gofiber/fiber")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12gr7a38dd02p7b9fimrk16ybxfp93krh7wah0jzc0v6syjmzfi0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gofiber/fiber/v2"
      #:test-flags
      #~(list "-skip"
              (string-join
               (list "Test_Proxy_DoRedirects_TooManyRedirects"
                     "Test_Proxy_Do_WithRealURL"
                     "Test_Proxy_DoRedirects_RestoreOriginalURL"
                     "Test_Proxy_Do_WithRedirect")
               "|"))))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-runewidth
           go-github-com-tinylib-msgp
           go-github-com-valyala-bytebufferpool
           go-github-com-valyala-fasthttp
           go-golang-org-x-sys))
    (home-page "https://github.com/gofiber/fiber")
    (synopsis "Express inspired web framework written in Golang")
    (description
     "Package fiber is an @code{https://github.com/expressjs/express, Express}
inspired web framework built on top of Fasthttp, the fastest HTTP engine for
Go.  Designed to ease things up for fast development with zero memory
allocation and performance in mind.")
    (license license:expat)))

(define-public go-github-com-gogo-protobuf
  (package
    (name "go-github-com-gogo-protobuf")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gogo/protobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dfv1bhx5zhb5bsj5sj757nkacf2swp1ajpcyj9d0b37n602m18a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/gogo/protobuf"
      ;; protoc: exec: "protoc-min-version": executable file not found in $PATH
      ;; err = exec: "protoc": executable file not found in $PATH:
      #:test-flags
      #~(list "-skip"
              (string-join
               (list "TestDashFilename"
                     "TestEmbedMarshaler"
                     "TestGolden"
                     "TestParameters"
                     "TestPopulateWarning"
                     "TestRepeatedEmbed"
                     "TestStdTypesGoString"
                     "TestTakesTooLongToDebug")
               "|"))))
    (home-page "https://github.com/gogo/protobuf")
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
    (license license:bsd-3)))

(define-public go-github-com-golang-groupcache
  (package
    (name "go-github-com-golang-groupcache")
    (version "0.0.0-20210331224755-41bb18bfe9da")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/groupcache")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07amgr8ji4mnq91qbsw2jlcmw6hqiwdf4kzfdrj8c4b05w4knszc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/golang/groupcache"))
    (propagated-inputs
     (list go-github-com-golang-protobuf))
    (home-page "https://github.com/golang/groupcache")
    (synopsis "Groupcache is a caching and cache-filling library")
    (description
     "Groupcache is a caching and cache-filling library, intended as a
replacement for memcached in many cases.  It provides a data loading mechanism
with caching and de-duplication that works across a set of peer processes.")
    (license license:asl2.0)))

(define-public go-github-com-google-go-github-v31
  (package
    (name "go-github-com-google-go-github-v31")
    (version "31.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-github")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bcybmr341hnp8k630pi4dcgia7561yzqc874l4c3nl4bc9rkh5j"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/google/go-github/scrape
            ;; - github.com/google/go-github/update-urls
            (delete-file-recursively "scrape")
            (delete-file-recursively "update-urls")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/google/go-github/v31"
      ;; repos_releases_test.go:449: Header.Get("Content-Type") returned
      ;; "application/octet-stream", want "text/plain; charset=utf-8"
      #:test-flags #~(list "-skip" "TestRepositoriesService_UploadReleaseAsset")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "example")))))))
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

;; For chezmoi@2.1.0
(define-public go-github-com-google-go-github-v36
  (package
    (inherit go-github-com-google-go-github-v31)
    (name "go-github-com-google-go-github-v36")
    (version "36.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-github")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "041a1rmi7pipimxiwjnsd0dngzb4djmcz8a8x4xv53d3373szaj6"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/google/go-github/scrape
            (delete-file-recursively "scrape")
            (delete-file-recursively "update-urls")))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-google-go-github-v31)
       ((#:import-path _) "github.com/google/go-github/v36")))))

(define-public go-github-com-google-go-github-v50
  (package
    (inherit go-github-com-google-go-github-v31)
    (name "go-github-com-google-go-github-v50")
    (version "50.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-github")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11qzikm988zfzs6g70lbdjfhw7kdndagahg0q2bkn2ibq2c47xxp"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/google/go-github/scrape
            (delete-file-recursively "scrape")
            (delete-file-recursively "update-urls")))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-google-go-github-v31)
       ((#:import-path _) "github.com/google/go-github/v50")))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-google-go-querystring
           go-github-com-protonmail-go-crypto
           go-golang-org-x-oauth2))))

(define-public go-github-com-google-go-querystring
  (package
    (name "go-github-com-google-go-querystring")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-querystring")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15k460c23nsmqd1nx3mvrnazws8bpb1gafrmffx7vf91m200mnwa"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-querystring/query"
      #:unpack-path "github.com/google/go-querystring"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (home-page "https://github.com/google/go-querystring/")
    (synopsis "Library for encoding structs into URL query parameters")
    (description
     "@code{go-querystring} is Go library for encoding structs into URL query
parameters.")
    (license license:bsd-3)))

(define-public go-github-com-google-gopacket
  (package
    (name "go-github-com-google-gopacket")
    (version "1.1.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/gopacket")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "048qwm2n0wrpql4qqgd7jyynn3gk069yvqbxnshlayzmbhf87ls4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/gopacket"
      ;; XXX: pfring/pfring.go:14:10: fatal error: pfring.h: No such file or
      ;; directory. Check how to fix all tests.
      #:test-subdirs #~(list ".")))
    (propagated-inputs
     (list go-github-com-vishvananda-netlink
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/google/gopacket")
    (synopsis "Packet processing capabilities library")
    (description
     "This package provides packet processing capabilities for Go.")
    (license license:bsd-3)))

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

(define-public go-github-com-gorilla-schema
  (package
    (name "go-github-com-gorilla-schema")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/schema")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16gk174mybvz0gg2w1wmpc96jhhi94i1vvclyzr3qkv7s6gadifn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gorilla/schema"))
    (home-page "https://github.com/gorilla/schema")
    (synopsis "Fills a struct with form values")
    (description
     "This package implements a functionality to fills a struct with form
values.")
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
    (version "1.5.3")
    (home-page "https://github.com/gorilla/websocket")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k84plbz6bxarbdrdcsrm9vhiy971prpvfnkcpsfv2q4ac80ccmx"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/websocket"))
    (synopsis "Fast WebSocket implementation for Go")
    (description "Gorilla WebSocket is a Go implementation of the WebSocket
protocol.")
    (license license:bsd-2)))

(define-public go-github-com-gregjones-httpcache
  (package
    (name "go-github-com-gregjones-httpcache")
    (version "0.0.0-20190611155906-901d90724c79")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gregjones/httpcache")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05r0xq51vfb55di11m7iv19341d73f7in33vq1ihcqs1nffdwiq0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gregjones/httpcache"))
    (propagated-inputs
     (list go-github-com-peterbourgon-diskv
           go-github-com-syndtr-goleveldb
           go-github-com-bradfitz-gomemcache
           go-github-com-gomodule-redigo))
    (home-page "https://github.com/gregjones/httpcache")
    (synopsis "Transport for @code{http.Client} that will cache responses")
    (description
     "Package @code{httpcache} provides a @code{http.RoundTripper}
implementation that works as a mostly
@url{https://tools.ietf.org/html/rfc7234, RFC 7234} compliant cache for HTTP
responses.  It is only suitable for use as a \"private\" cache (i.e. for a
web-browser or an API-client and not for a shared proxy).")
    (license license:expat)))

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

(define-public go-github-com-hashicorp-go-retryablehttp
  (package
    (name "go-github-com-hashicorp-go-retryablehttp")
    (version "0.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-retryablehttp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11bqzz3244vpa91l5bx8pp5pajbcg4qxrl8ic2x0qgwbrjfz362x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-retryablehttp"))
    (propagated-inputs
     (list go-github-com-hashicorp-go-hclog
           go-github-com-hashicorp-go-cleanhttp))
    (home-page "https://github.com/hashicorp/go-retryablehttp")
    (synopsis "Retryable HTTP client in Golang")
    (description
     "Package retryablehttp provides a familiar HTTP client interface with
automatic retries and exponential backoff.  It is a thin wrapper over the
standard @code{net/http} client library and exposes nearly the same public
API.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-sockaddr
  (package
    (name "go-github-com-hashicorp-go-sockaddr")
    (version "1.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-sockaddr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ajcffaqxrbqyg00b04a1ia7np0180x7z5q3bcxqxm0smqqag54z"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (delete-file-recursively "cmd/sockaddr/vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-sockaddr"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Tests require network set-up or fail randomly.
               (list "TestGetDefaultInterface"
                     "TestGetDefaultInterfaces"
                     "TestGetIfAddrs"
                     "TestGetPrivateIP"
                     "TestGetPrivateIPs"
                     "TestGetPrivateInterfaces"
                     "TestSockAddr_IPAddrs_IPAddrsByNetworkSize/0"
                     "TestSockAddr_Parse")
               "|"))))
    (propagated-inputs
     (list go-github-com-hashicorp-errwrap
           go-github-com-mitchellh-cli
           go-github-com-mitchellh-go-wordwrap
           go-github-com-ryanuber-columnize))
    (home-page "https://github.com/hashicorp/go-sockaddr")
    (synopsis "IP Address/UNIX Socket convenience functions for Golang")
    (description
     "This package provides an implementation of the UNIX socket family data
types and related helper functions.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-yamux
  (package
    (name "go-github-com-hashicorp-yamux")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/yamux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c46zdj94lv28sb9rmhinzcckl72vs3gwm197nsa80ca3b161yi6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/yamux"))
    (home-page "https://github.com/hashicorp/yamux")
    (synopsis "Golang connection multiplexing library")
    (description
     "Yamux (Yet another Multiplexer) relies on an underlying connection to
provide reliability and ordering, such as TCP or Unix domain sockets, and
provides stream-oriented multiplexing.  It is inspired by SPDY but is not
interoperable with it.

Features:
@itemize
@item streams can be opened by either client or server
@item useful for nat traversal
@item server-side push support
@item avoid starvation
@item back-pressure to prevent overwhelming a receiver
@item enables persistent connections over a load balancer
@item enables thousands of logical streams with low overhead
@end itemize")
    (license license:mpl2.0)))

(define-public go-github-com-hetznercloud-hcloud-go-v2
  (package
    (name "go-github-com-hetznercloud-hcloud-go-v2")
    (version "2.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hetznercloud/hcloud-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rmrp100clcymz6j741dpvx217d6ljnfqn9qfndlmy9rwi64ih8h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/hetznercloud/hcloud-go/v2"))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-jmattheis-goverter
           go-github-com-prometheus-client-golang
           go-github-com-vburenin-ifacemaker
           go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://github.com/hetznercloud/hcloud-go")
    (synopsis "Golang library for the Hetzner Cloud API")
    (description
     "This package provides a library for the Hetzner Cloud API.")
    (license license:expat)))

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

(define-public go-github-com-jaytaylor-html2text
  (package
    (name "go-github-com-jaytaylor-html2text")
    (version "0.0.0-20230321000545-74c2419ad056")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jaytaylor/html2text")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14r0ph8w4yxx129kfvj0qbx4cyid65md93qmwlz2cly4iwjnr7w2"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Module name has been changed upstream.
            (substitute* (find-files "." "\\.go$")
              (("jaytaylor.com/html2text") "github.com/jaytaylor/html2text"))))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/jaytaylor/html2text"))
    (propagated-inputs
     (list go-golang-org-x-net
           go-github-com-olekukonko-tablewriter
           go-github-com-ssor-bom))
    (home-page "https://github.com/jaytaylor/html2text")
    (synopsis "Convert HTML emails to text")
    (description
     "The html2text package converts HTML emails to plain text, allowing
text-only mail clients to display them.")
    (license license:expat)))

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
    (version "8.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jcmturner/gokrb5")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rv495j8j2x6avw5hqpf7rpiakr5gdsx6pv8rfn0ff7vi35zfa62"))))
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
           go-github-com-jcmturner-rpc-v2
           go-golang-org-x-crypto))
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

(define-public go-github-com-jcmturner-rpc
  (package
    (name "go-github-com-jcmturner-rpc")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jcmturner/rpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hkmvf8qdcifnzym8kv1xhq7lq0wpr0i6gzff159lh9xn0wfg175"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/jcmturner/rpc.v1"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jcmturner/rpc")
    (synopsis "Remote Procedure Call libraries")
    (description
     "This package provides a partial Go implementation of the Remote Call
Procedure libraries, presented in
@@url{http://pubs.opengroup.org/onlinepubs/9629399/,CDE 1.1: Remote Procedure
Call}.")
    (license license:asl2.0)))

(define-public go-github-com-jcmturner-rpc-v2
  (package
    (inherit go-github-com-jcmturner-rpc)
    (name "go-github-com-jcmturner-rpc-v2")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jcmturner/rpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nm4j2nwcszghldw39rwdx2hr56i1lybfpv33y4gd67w6qcqbpsi"))))
    (arguments
     (list
      #:import-path "github.com/jcmturner/rpc/v2"
      #:unpack-path "github.com/jcmturner/rpc"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
    (propagated-inputs
     (list go-golang-org-x-net))))

(define-public go-github-com-jhillyerd-enmime
  (package
    (name "go-github-com-jhillyerd-enmime")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhillyerd/enmime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mlgnk0y0d8njx7h66w6bhr95zh2ccg1hxlnm15i2lfh6l58s60q"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/jhillyerd/enmime"))
    (native-inputs
     (list go-github-com-go-test-deep
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-cention-sany-utf7
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

(define-public go-github-com-jlaffaye-ftp
  (package
    (name "go-github-com-jlaffaye-ftp")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jlaffaye/ftp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z9d1dxhx351158a22a08qbnfql7a1cajg6v3zm82m2rnp17ahir"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jlaffaye/ftp"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-hashicorp-go-multierror))
    (home-page "https://github.com/jlaffaye/ftp")
    (synopsis "FTP client package for Go")
    (description
     "Package ftp implements a @acronym{File Transfer Protocol,FTP} client as
described in @url{https://www.rfc-editor.org/rfc/rfc959,RFC 959}.")
    (license license:isc)))

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

(define-public go-github-com-jsimonetti-rtnetlink
  (package
    (name "go-github-com-jsimonetti-rtnetlink")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsimonetti/rtnetlink")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19m8fcrcbw98gc191snfsi6qhb80jxnjhxzy8gppcwwg6732wmm1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jsimonetti/rtnetlink"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-mdlayher-netlink go-golang-org-x-sys))
    (home-page "https://github.com/jsimonetti/rtnetlink")
    (synopsis "Low-level access to the Linux rtnetlink API")
    (description
     "This package allows the kernel's routing tables to be read and
altered.  Network routes, IP addresses, Link parameters, Neighbor
setups,Queueing disciplines, Traffic classes and Packet classifiers may all be
controlled.  It is based on netlink messages.")
    (license license:expat)))

(define-public go-github-com-jsimonetti-rtnetlink-v2
  (package
    (inherit go-github-com-jsimonetti-rtnetlink)
    (name "go-github-com-jsimonetti-rtnetlink-v2")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsimonetti/rtnetlink")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lgx3kip6hiigahcnvjy7d1qqxbm2vnfh2m3zrpfkqkh03dl39x4"))))
    (arguments
     (list
      #:import-path "github.com/jsimonetti/rtnetlink/v2"))))

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
     (list
      #:import-path "github.com/json-iterator/go"
      #:test-flags
      ;; XXX: Try to skip just "Test_symmetric/map[test.stringKeyType]string".
      #~(list "-skip" "Test_symmetric")))
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

(define-public go-github-com-justinas-alice
  (package
    (name "go-github-com-justinas-alice")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/justinas/alice")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19l88vi13rqyhjl100zd5z26ghy4iln74kqfd3hsmd2nydz7manz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/justinas/alice"))
    (home-page "https://github.com/justinas/alice")
    (synopsis "Middleware chaining for Golang")
    (description
     "Package alice provides a convenient way to chain HTTP handlers.")
    (license license:expat)))

(define-public go-github-com-kolo-xmlrpc
  (package
    (name "go-github-com-kolo-xmlrpc")
    (version "0.0.0-20220921171641-a4b6fa1dd06b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kolo/xmlrpc")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nhcnqycdc52k4ymdd4g2chcimsnvr86m0yx13ws91qxs5pgs9d2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kolo/xmlrpc"))
    (propagated-inputs
     (list go-golang-org-x-text))
    (home-page "https://github.com/kolo/xmlrpc")
    (synopsis "Implementation of XMLRPC protocol in Golang")
    (description
     "This package provides an implementation of client side part of XMLRPC
protocol in Go language.")
    (license license:expat)))

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
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Tests requiring network setup.
               (list "TestAdvertise_Alive"
                     "TestAdvertise_Bye"
                     "TestAnnounceAlive"
                     "TestAnnounceBye"
                     "TestInterfaces"
                     "TestSearch_Request"
                     "TestSearch_Response"
                     "TestSearch_ServiceRawHeader")
               "|"))))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/koron/go-ssdp")
    (synopsis "SSDP library for Golang")
    (description
     "@code{go-ssdp} is a @url{https://tools.ietf.org/html/draft-cai-ssdp-v1-03,
@acronym{Simple Service Discovery Protocol, SSDP}} library for Golang.")
    (license license:expat)))

(define-public go-github-com-kortschak-wol
  (package
    (name "go-github-com-kortschak-wol")
    (version "0.0.0-20200729010619-da482cc4850a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kortschak/wol")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16qyy1c1q5cxcnwdzl69v49pjmyxha4i88fsg0g83gwdyapzsyln"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kortschak/wol"))
    (home-page "https://github.com/kortschak/wol")
    (synopsis "Wake On LAN client")
    (description
     "Package wol provides a Wake On LAN function.")
    (license license:bsd-3)))

(define-public go-github-com-labbsr0x-goh
  (package
    (name "go-github-com-labbsr0x-goh")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/labbsr0x/goh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06yrd6ih6r0rkxyn88b66gfarjxsqmi3wzi8cgsxskq7mbah0iyp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/labbsr0x/goh"))
    (propagated-inputs
     (list go-github-com-go-cmd-cmd
           go-github-com-go-errors-errors
           go-github-com-sirupsen-logrus))
    (home-page "https://github.com/labbsr0x/goh")
    (synopsis "Utility library for writing web hooks")
    (description
     "This package provides an utility library for writing extremely simple
webhooks in Golang.")
    (license license:expat)))

(define-public go-github-com-labstack-echo-v4
  (package
    (name "go-github-com-labstack-echo-v4")
    (version "4.13.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/labstack/echo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i4w36f1k17bykc24dzr2ng5zpsyysfg5bzfvlbrphxxzhsngxdy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/labstack/echo/v4"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-labstack-gommon
           go-github-com-valyala-fasttemplate
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-time))
    (home-page "https://echo.labstack.com/")
    (synopsis "High performance, minimalist Go web framework")
    (description
     "Package echo implements a high performance, minimalist Go web framework.
Features:
@itemize
@item optimized HTTP router which smartly prioritize routes
@item build robust and scalable RESTful APIs
@item group APIs
@item extensible middleware framework
@item define middleware at root, group or route level
@item data binding for JSON, XML and form payload
@item handy functions to send variety of HTTP responses
@item centralized HTTP error handling
@item template rendering with any template engine
@item define your format for the logger
@item highly customizable
@item automatic TLS via Let’s Encrypt
@item HTTP/2 support
@end itemize")
    (license license:expat)))

(define-public go-github-com-levigross-grequests
  (package
    (name "go-github-com-levigross-grequests")
    (version "0.0.0-20231203190023-9c307ef1f48d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/levigross/grequests")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13r24vrcgfkps8r09crjlhsywpxs8bnnmlgn5qhbhqiqag754xdc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; most of them need network access
      #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/levigross/grequests"))
    (propagated-inputs
     (list go-github-com-google-go-querystring
           go-golang-org-x-net))
    (home-page "https://github.com/levigross/grequests")
    (synopsis "Requests library for Golang")
    (description
     "Package grequests implements a friendly API over Go's existing
@code{net/http} library.")
    (license license:asl2.0)))

(define-public go-github-com-libdns-libdns
  (package
    (name "go-github-com-libdns-libdns")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libdns/libdns")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00zx6yij1ac8mhswhsks1nchzgmhbzrsm9hr0faqbmx0vkip78j5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libdns/libdns"))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs (list go-golang-org-x-exp))
    (home-page "https://github.com/libdns/libdns")
    (synopsis "Universal DNS provider APIs for Go")
    (description
     "@code{libdns} is a collection of free-range DNS provider client
implementations.  It defines the core interfaces that provider packages should
implement.  They are small and idiomatic interfaces with well-defined semantics.

The interfaces include:
@itemize
@item @url{https://pkg.go.dev/github.com/libdns/libdns#RecordGetter, RecordGetter} to
list records.
@item @url{https://pkg.go.dev/github.com/libdns/libdns#RecordAppender,
RecordAppender} to append new records.
@item @url{https://pkg.go.dev/github.com/libdns/libdns#RecordSetter, RecordSetter} to
set (create or change existing) records.
@item @url{https://pkg.go.dev/github.com/libdns/libdns#RecordDeleter, RecordDeleter}
to delete records.
@end itemize")
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
      #:tests? (and (target-64bit?)
                    (not (%current-target-system)))
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
        (base32 "1c94sq43bl1kp04lllcfrfyiy5z3zcfz0s65sm1vgb2s40zrwpr7"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Delete generated binary file.
            (delete-file "sorted-network-list.bin")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-libp2p-asn-util"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'generate-sorted-network-list
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (setenv "ASN_IPV6_FILE"
                        (string-append
                         #$(this-package-native-input "specification-ip2asn-v6")
                         "/share/data/ip2asn-v6.tsv"))
                (invoke "go" "run" "./generate/")))))))
    (native-inputs
     (list go-github-com-stretchr-testify
           specification-ip2asn-v6))
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
    (version "4.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-yamux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jrj7pamcs8s6pd6c8dhkfv3a8cdjhb0kamcxa9cgv79n8rnyp86"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-yamux/v4"
      #:test-flags
      ;; Test fails on ARM with error: [ERR] yamux: Failed to read stream data
      ;; on stream 1: receive window exceeded (remain: 262144, recv:
      ;; 33554432).
      #~(list #$@(if (target-arm?)
                     '("-skip" "TestSendData_VeryLarge")
                     '()))))
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

(define-public go-github-com-libp2p-go-yamux-v5
  (package
    (inherit go-github-com-libp2p-go-yamux-v4)
    (name "go-github-com-libp2p-go-yamux-v5")
    (version "5.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-yamux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "019jj3qwzrbqcgwzri5iwi1vh2cn0ms6k8fx14jlmy856b25yl3y"))))
    (build-system go-build-system)
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-libp2p-go-yamux-v4)
       ((#:import-path _) "github.com/libp2p/go-yamux/v5")))))

(define-public go-github-com-libp2p-zeroconf-v2
  (package
    (name "go-github-com-libp2p-zeroconf-v2")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/zeroconf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xrqdi7s8296963zh7gz450ivbniar7723xlr8v9nh90cyy1ah3r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; it requires netwok setup
      #:import-path "github.com/libp2p/zeroconf/v2"))
    (propagated-inputs
     (list go-github-com-miekg-dns go-golang-org-x-net))
    (home-page "https://github.com/libp2p/zeroconf")
    (synopsis "mDNS/DNS-SD Service Discovery in pure Golang")
    (description
     "This package implements a service discovery functionality specified in
@url{https://tools.ietf.org/html/rfc6762, RFC 6762} (mDNS) and
@url{https://tools.ietf.org/html/rfc6763, RFC 6763} (DNS-SD) standards which
intends to be compatible with Avahi.")
    (license license:expat)))

(define-public go-github-com-macronut-go-tproxy
  (package
    (name "go-github-com-macronut-go-tproxy")
    (version "0.0.0-20190726054950-ef7efd7f24ed")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FutureProtocolLab/go-tproxy")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jibsg0xhsn0h1jq4g9qd4nr58w43y8majlwfri9ffk2cbfrwqdr"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "example"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/macronut/go-tproxy"))
    (home-page "https://github.com/FutureProtocolLab/go-tproxy")
    (synopsis "Linux Transparent Proxy library")
    (description
     "Golang TProxy provides an easy to use wrapper for the Linux Transparent
Proxy functionality.")
    (license license:expat)))

(define-public go-github-com-mailru-easyjson
  (package
    (name "go-github-com-mailru-easyjson")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mailru/easyjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00if9lpfy7bz853snqp7zgg76pn4mgpkk42h0riahcwk5v19jrcv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mailru/easyjson"
      ;; XXX: All tests in "tests" directory fail, figure out why.
      #:test-subdirs #~(list ".")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "benchmark")))))))
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

(define-public go-github-com-marten-seemann-tcp
  (package
    (name "go-github-com-marten-seemann-tcp")
    (version "0.0.0-20210406111302-dfbc87cc63fd")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marten-seemann/tcp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l03l7vx5j641bsgqzlcdshmsi7m1x0yny8i81hx5c5fbg5c25zx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/marten-seemann/tcp"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;;  Test fails with error: couldn't initialize from a wrapped
               ;;  conn: unknown connection type.
               (list "TestWrappedConn"
                     ;; Get "https://golang.org/robots.txt": dial tcp: lookup
                     ;; golang.org on [::1]:53: read udp
                     ;; [::1]:56806->[::1]:53: read: connection refused
                     "TestInfo"
                     ;; Test fails on ARM with error: raw-control tcp
                     ;; 127.0.0.1:49386: getsockopt: protocol not available.
                     #$@(if (target-arm?)
                            '("TestConcurrentReadWriteAndInfo")
                            '()))
               "|"))))
    (native-inputs
     (list go-golang-org-x-net))
    (propagated-inputs
     (list go-github-com-mikioh-tcp))
    (home-page "https://github.com/marten-seemann/tcp")
    (synopsis "TCP-level socket options implementation in Golang")
    (description
     "This package provides a TCP-level socket options that allow manipulation
of TCP connection facilities.")
    (license license:bsd-2)))

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

(define-public go-github-com-mattn-go-ieproxy
  (package
    (name "go-github-com-mattn-go-ieproxy")
    (version "0.0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-ieproxy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cqfjq106vdm9l9ddb4pps001wxwzabrq2q82f9dimszfq0my8av"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-ieproxy"))
    (propagated-inputs
     (list go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/mattn/go-ieproxy")
    (synopsis "Detect the proxy settings from Golang")
    (description
     "Package ieproxy is a utility to retrieve the proxy parameters.")
    (license license:expat)))

(define-public go-github-com-mattn-go-mastodon
  (package
    (name "go-github-com-mattn-go-mastodon")
    (version "0.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-mastodon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05hnh5359awm5x7qbaf8fvf772p2hc781alfqnp7ymr4rjfk3pql"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/mattn/go-mastodon/cmd/mstd
            (delete-file-recursively "cmd")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-mastodon"))
    (propagated-inputs
     (list go-github-com-gorilla-websocket
           go-github-com-tomnomnom-linkheader))
    (home-page "https://github.com/mattn/go-mastodon")
    (synopsis "Mastodon client for golang")
    (description
     "Package mastodon provides functions and structs for accessing the
mastodon API.")
    (license license:expat)))

(define-public go-github-com-mattn-go-xmlrpc
  (package
    (name "go-github-com-mattn-go-xmlrpc")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-xmlrpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hv843qf8lzyn01mgk4lwwhirzpzpz33a1cz2ws998shhvq75dvg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-xmlrpc"))
    (home-page "https://github.com/mattn/go-xmlrpc")
    (synopsis "XML RPC interface for Golang")
    (description
     "This package implements XML RPC interface for Go programming
language.")
    (license license:expat)))

(define-public go-github-com-mattn-godown
  (package
    (name "go-github-com-mattn-godown")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/godown")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f979h6z13nwwx42ahhqv3d4q2ywpg4l7v03qhnr9zaw319jjaln"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/godown"))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth
           go-golang-org-x-net))
    (home-page "https://github.com/mattn/godown")
    (synopsis "Convert HTML into Markdown")
    (description
     "This package provides a functionality to convert HTML into Markdown.")
    (license license:expat)))

(define-public go-github-com-mattn-goveralls
  (package
    (name "go-github-com-mattn-goveralls")
    (version "0.0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/goveralls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16jszydip2abwc0fws3sz5yzyy87w8mbkhzm2wzb8ijpjhk1by79"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/goveralls"
      ;; Test requires git.
      #:test-flags #~(list "-skip" "TestShowArg/with_show")))
    (propagated-inputs
     (list go-golang-org-x-mod
           go-golang-org-x-tools))
    (home-page "https://github.com/mattn/goveralls")
    (synopsis "Golang client for Coveralls.io")
    (description
     "This package provides a client for @url{http://coveralls.io/
Coveralls.io} continuous code coverage tracking system.")
    (license license:expat)))

(define-public go-github-com-mdlayher-ethtool
  (package
    (name "go-github-com-mdlayher-ethtool")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/ethtool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pwm4cg1yf364k0c4i0jrrvgn82k14b163xcvnm3bvpglfbcy2ks"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mdlayher/ethtool"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-josharian-native
           go-github-com-mdlayher-genetlink
           go-github-com-mdlayher-netlink
           go-golang-org-x-sys))
    (home-page "https://github.com/mdlayher/ethtool")
    (synopsis "Control of the Linux ethtool generic netlink interface")
    (description
     "Package ethtool allows control of the Linux ethtool generic netlink
interface. as it's described in
@url{https://www.kernel.org/doc/html/latest/networking/ethtool-netlink.html,
ethtool-netlink}.")
    (license license:expat)))

(define-public go-github-com-mdlayher-genetlink
  (package
    (name "go-github-com-mdlayher-genetlink")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/genetlink")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vppn8071nh8pnbyq9769j1zcgq76iadd5fry90xkmfq429if356"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mdlayher/genetlink"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-mdlayher-netlink
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/mdlayher/genetlink")
    (synopsis "Generic netlink interactions and data types")
    (description
     "Package genetlink implements generic netlink interactions and data
types.")
    (license license:expat)))

(define-public go-github-com-mdlayher-netlink
  (package
    (name "go-github-com-mdlayher-netlink")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/netlink")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pxd0qn73jr9n64gkp2kd8q8x7xgssm3v8a68vkh88al55g8jkma"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Cycles with go-github-com-jsimonetti-rtnetlink.
            (delete-file-recursively "internal/integration")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mdlayher/netlink"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Test fails to start command "ip": exec: "ip": executable
               ;; file not found in $PATH
               (list "TestIntegrationConnSetBuffersSyscallConn"
                     #$@(if (target-arm?)
                            ;; Tests fail on ARM systems with error: failed to
                            ;; dial netlink: socket: protocol not supported.
                            '("TestIntegrationConn"
                              "TestIntegrationConnClosedConn"
                              "TestIntegrationConnConcurrentClosePreventsReceive"
                              "TestIntegrationConnConcurrentCloseUnblocksReceive"
                              "TestIntegrationConnConcurrentOneConn"
                              "TestIntegrationConnConcurrentSerializeExecute"
                              "TestIntegrationConnExecuteTimeout"
                              "TestIntegrationConnExplicitPID"
                              "TestIntegrationConnReceiveTimeout"
                              "TestIntegrationConnSendTimeout"
                              "TestIntegrationConnSetBPF"
                              "TestIntegrationConnSetBPFEmpty"
                              "TestIntegrationConnStrict")
                            '())
                     "|")))))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-josharian-native
           go-github-com-mdlayher-socket
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/mdlayher/netlink")
    (synopsis "Low-level access to Linux netlink sockets")
    (description
     "This package provides a low-level access to Linux netlink
sockets (AF_NETLINK).")
    (license license:expat)))

(define-public go-github-com-mdlayher-socket
  (package
    (name "go-github-com-mdlayher-socket")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/socket")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bq6sphsffjqqk2v9wy8qkv5yf0r6d72pklapgy3znqlnpgvnqab"))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         ;; One test fails on ARM with error: conn_linux_test.go:120:
         ;; skipping, permission denied: failed to unshare network namespace:
         ;; operation not permitted
         #$@(if (target-arm?)
                '("-skip" "TestLinuxBindToDevice")
                '()))
      #:import-path "github.com/mdlayher/socket"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys))
    (home-page "https://github.com/mdlayher/socket")
    (synopsis "Low-level network connection type with async I/O and deadline support")
    (description
     "This package provides a low-level network connection type which
integrates with Go's runtime network poller to provide asynchronous I/O and
deadline support.")
    (license license:expat)))

(define-public go-github-com-mdlayher-vsock
  (package
    (name "go-github-com-mdlayher-vsock")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/vsock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pnhxz9jz25469gnd5p9a8jl10w3affcjxlvzhlni2c434s3l6kx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mdlayher/vsock"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-mdlayher-socket
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys))
    (home-page "https://github.com/mdlayher/vsock")
    (synopsis "Access to Linux VM sockets for Golang")
    (description
     "Package vsock provides access to Linux VM sockets (AF_VSOCK) for
communication between a hypervisor and its virtual machines.")
    (license license:expat)))

(define-public go-github-com-mdlayher-wifi
  (package
    (name "go-github-com-mdlayher-wifi")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/wifi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18h5n58gr3i4jbf7c4hfvx3w87bjjiaavcnpcl0j310bgrc6dnzi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mdlayher/wifi"))
    (native-inputs
     (list go-github-com-google-go-cmp))
    (propagated-inputs
     (list go-github-com-mdlayher-genetlink
           go-github-com-mdlayher-netlink
           go-golang-org-x-crypto
           go-golang-org-x-sys))
    (home-page "https://github.com/mdlayher/wifi")
    (synopsis "Access to IEEE 802.11 WiFi device actions")
    (description
     "Package wifi provides access to IEEE 802.11 @code{WiFi} device
operations on Linux using nl80211.")
    (license license:expat)))

(define-public go-github-com-mholt-acmez
  (package
    (name "go-github-com-mholt-acmez")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mholt/acmez")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xfl6p8izgjs1d26iygfilmmagxld409qsgdy60r1chfsrcnraby"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mholt/acmez"))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-golang-org-x-net
           go-go-uber-org-zap))
    (home-page "https://github.com/mholt/acmez")
    (synopsis "ACME (RFC 8555) client library for Go")
    (description
     "@code{ACMEz} is a lightweight, fully-compliant
@url{https://tools.ietf.org/html/rfc8555, RFC 8555} (ACME) implementation, that
provides an API for getting certificates.  @code{ACMEz} is suitable for large-scale
enterprise deployments.  It also supports common IETF-standardized ACME extensions.

This module has two primary packages:
@itemize
@item @code{acmez} is a high-level wrapper for getting certificates.  It implements
the ACME order flow described in RFC 8555 including challenge solving using pluggable
solvers.
@item @code{acme} is a low-level RFC 8555 implementation that provides the
fundamental ACME operations, mainly useful if you have advanced or niche
requirements.
@end itemize

Main features:
@itemize
@item Go API that thoroughly documented with spec citations.
@item Structured error values (@samp{problems} as defined in
@url{https://tools.ietf.org/html/rfc7807, RFC 7807}.)
@item Smart retries (resilient against network and server hiccups.)
@item Challenge plasticity (randomized challenges, and will retry others if one
fails.)
@item Context cancellation (suitable for high-frequency config changes or reloads.)
@item Highly flexible and customizable.
@item External Account Binding (EAB) support.
@item Tested with numerous ACME CAs (more than just Let's Encrypt.)
@item Implements niche aspects of RFC 8555 (such as alt cert chains and account key
rollover.)
@item Efficient solving of large SAN lists (e.g. for slow DNS record propagation.)
@item Utility functions for solving challenges: device attestation
challenges (draft-acme-device-attest-02), @url{https://tools.ietf.org/html/rfc8737,
RFC 8737} (tls-alpn-01 challenge), @url{https://tools.ietf.org/html/rfc8823, RFC
8823} (email-reply-00 challenge; S/MIME.)
@item ACME Renewal Information (ARI) support (draft-ietf-acme-ari-03.)
@end itemize")
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

(define-public go-github-com-mikioh-tcp
  (package
    (name "go-github-com-mikioh-tcp")
    (version "0.0.0-20190314235350-803a9b46060c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mikioh/tcp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mwldbqkl6j4lzxar5pnvi946w0iifmw43rmanbwzp7ngx27fz5a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mikioh/tcp"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;;  Get "https://golang.org/robots.txt": dial tcp: lookup
               ;;  golang.org on [::1]:53: read udp [::1]:47181->[::1]:53:
               ;;  read: connection refused.
               (list "TestInfo"
                     #$@(if (target-arm?)
                            ;; Test fails on ARM with error: raw-control tcp
                            ;; 127.0.0.1:58464: getsockopt: protocol not
                            ;; available.
                            '("TestConcurrentReadWriteAndInfo")
                            '()))
               "|"))))
    (native-inputs
     (list go-golang-org-x-net))
    (propagated-inputs
     (list go-github-com-mikioh-tcpinfo))
    (home-page "https://github.com/mikioh/tcp")
    (synopsis "TCP-level socket options implementation in Golang")
    (description
     "This package implements a TCP-level socket options that allow
manipulation of TCP connection facilities.")
    (license license:bsd-2)))

(define-public go-github-com-mikioh-tcpinfo
  (package
    (name "go-github-com-mikioh-tcpinfo")
    (version "0.0.0-20190314235526-30a79bb1804b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mikioh/tcpinfo")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "053dwvlawhhm7ly2vhjziqdifnqp12dav6rsbxbcivjjzyzw987f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mikioh/tcpinfo"
      #:phases
      #~(modify-phases %standard-phases
          ;; It inroduce cycle with go-github-com-mikioh-tcp.
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file "example_test.go")))))))
    (propagated-inputs
     (list go-github-com-mikioh-tcpopt))
    (home-page "https://github.com/mikioh/tcpinfo")
    (synopsis "Encoding and decoding of TCP-level socket options")
    (description
     "This package implements an encoding and decoding of TCP-level socket
options regarding connection information.")
    (license license:bsd-2)))

(define-public go-github-com-mikioh-tcpopt
  (package
    (name "go-github-com-mikioh-tcpopt")
    (version "0.0.0-20190314235656-172688c1accc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mikioh/tcpopt")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qdr0vmriy0wf6zg7hpq75g3b4nvp2p4gsc6xqvqg298v42zbrqj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mikioh/tcpopt"
      #:phases
      #~(modify-phases %standard-phases
          ;; It inroduce cycle with go-github-com-mikioh-tcp.
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file "example_test.go")))))))
    (home-page "https://github.com/mikioh/tcpopt")
    (synopsis "Encoding and decoding of TCP-level socket options in Golang")
    (description
     "This package implements an encoding and decoding of TCP-level socket
options.")
    (license license:bsd-2)))

(define-public go-github-com-minio-minio-go
  (package
    (name "go-github-com-minio-minio-go")
    (version "3.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/minio-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fihvi30wrjd4xgksryz8r315w30x4gyqp1qs1gzaipyyksim8d2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/minio/minio-go"))
    (home-page "https://github.com/minio/minio-go")
    (synopsis "Minio Go Client SDK for Amazon S3 Compatible Cloud Storage")
    (description
     "The Minio Go Client SDK provides simple APIs to access any Amazon S3
compatible object storage.")
    (license license:asl2.0)))

(define-public go-github-com-minio-minio-go-v7
  (package
    (inherit go-github-com-minio-minio-go)
    (name "go-github-com-minio-minio-go-v7")
    (version "7.0.86")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/minio-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k9ab0nqdfgwf1h46wsv0i5d207pdyw7dc6ccdj8i7adfbxa1zwa"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/minio-go/v7"
      #:embed-files
      #~(list
         ;; golang.org/x/net/publicsuffix/table.go:63:12: pattern
         ;; data/children: cannot embed irregular file data/children
         "children"
         ;; golang.org/x/net/publicsuffix/table.go:48:12: pattern data/nodes:
         ;; cannot embed irregular file data/nodes
         "nodes"
         ;; golang.org/x/net/publicsuffix/table.go:33:12: pattern data/text:
         ;; cannot embed irregular file data/text
         "text")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (substitute* (format #f
                                   "src/~a/pkg/credentials/credentials.sample"
                                   import-path)
                (("/bin/cat") (which "cat"))))))))
    (propagated-inputs
     (list go-github-com-dustin-go-humanize
           go-github-com-go-ini-ini
           go-github-com-goccy-go-json
           go-github-com-google-uuid
           go-github-com-klauspost-compress
           go-github-com-minio-crc64nvme
           go-github-com-minio-md5-simd
           go-github-com-rs-xid
           go-golang-org-x-crypto
           go-golang-org-x-net))))

(define-public go-github-com-multiformats-go-multiaddr
  (package
    (name "go-github-com-multiformats-go-multiaddr")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-multiaddr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0029zjhndbisfsc2msd2h18pcw23rqvf40drkcf7nxic3y2vaff7"))))
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

(define-public go-github-com-ncw-swift-v2
  (package
    (name "go-github-com-ncw-swift-v2")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncw/swift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "058mbdgm57rm24skscpl2lklqxs46sc4c3x5770xrd3yncry0rl8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncw/swift/v2"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Tests requiring access with TOKEN to API endpoints.
                       (list "TestAuthenticate"
                             "TestCDNDisable"
                             "TestCDNEnable"
                             "TestCDNMeta"
                             "TestContainerCreate"
                             "TestContainerDelete"
                             "TestOnReAuth")
                       "|"))))
    (home-page "https://github.com/ncw/swift")
    (synopsis "Interface to various cloud storage APIs")
    (description
     "Package swift provides an easy to use interface to Swift / Openstack
Object Storage / Rackspace Cloud Files.")
    (license license:expat)))

(define-public go-github-com-nrdcg-goinwx
  (package
    (name "go-github-com-nrdcg-goinwx")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nrdcg/goinwx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ld3spdi7q8cf4hf0wnbl7gyw2k8n4wp03fqncjx2gw2nsjng684"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nrdcg/goinwx"))
    (propagated-inputs
     (list
      go-github-com-fatih-structs
      go-github-com-kolo-xmlrpc
      go-github-com-mitchellh-mapstructure))
    (home-page "https://github.com/nrdcg/goinwx")
    (synopsis "INWX Go API client")
    (description
     "This go library implements some parts of the official
@url{https://www.inwx.com/en/help/apidoc, INWX XML-RPC API}.")
    (license license:expat)))

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

(define-public go-github-com-nytimes-gziphandler
  (package
    (name "go-github-com-nytimes-gziphandler")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nytimes/gziphandler")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rhrjlw220hnymzfccm0yir3pc9dpj7h3gwzhzq2cbsb3hhsqvyy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/NYTimes/gziphandler"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/NYTimes/gziphandler")
    (synopsis "Middleware to gzip HTTP responses")
    (description
     "This is a tiny Go package which wraps HTTP handlers to transparently
gzip the response body, for clients which support it.")
    (license license:asl2.0)))

(define-public go-github-com-opentracing-contrib-go-stdlib
  (package
    (name "go-github-com-opentracing-contrib-go-stdlib")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opentracing-contrib/go-stdlib")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18ws81a30igmff4pnqfvc2sv8hcy5gjb2saqz00mgz64y8nvjfx7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/opentracing-contrib/go-stdlib"))
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

(define-public go-github-com-pascaldekloe-goe
  (package
    (name "go-github-com-pascaldekloe-goe")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pascaldekloe/goe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mszfjcc29b6nvw3hs8w33iy6zx6ih5v2jlard0dsrgkpvsx5c81"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pascaldekloe/goe"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Replace when go-build-system supports nested path.
          (delete 'build)
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (home-page "https://github.com/pascaldekloe/goe")
    (synopsis "Enterprise tooling for Golang")
    (description
     "Common enterprise features for the Go programming language.")
    (license license:cc0)))

(define-public go-github-com-perimeterx-marshmallow
  (package
    (name "go-github-com-perimeterx-marshmallow")
    (version "1.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PerimeterX/marshmallow")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jxpjfyshcbrmj9qmj3sr21va6kw7hs55w2gaz9118jd861s6mbw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/perimeterx/marshmallow"))
    (native-inputs
     (list go-github-com-go-test-deep))
    (propagated-inputs
     (list go-github-com-mailru-easyjson
           go-github-com-ugorji-go-codec))
    (home-page "https://github.com/perimeterx/marshmallow")
    (synopsis "JSON unmarshalling in Golang")
    (description
     "This package provides a simple API to perform JSON unmarshalling.
It supports unmarshalling of some known and some unknown fields with zero
performance overhead.  While unmarshalling, it allows fully retaining the
original data and access it via a typed struct and a dynamic map.")
    (license license:expat)))

(define-public go-github-com-peterbourgon-unixtransport
  (package
    (name "go-github-com-peterbourgon-unixtransport")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/peterbourgon/unixtransport")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c5j01bqwh8zy3n2mynh6irh30wfv6sdd1a34yhhg39l9xbpj51g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/peterbourgon/unixtransport"))
    (propagated-inputs
     (list go-github-com-miekg-dns
           go-github-com-oklog-run
           go-github-com-peterbourgon-ff-v3))
    (home-page "https://github.com/peterbourgon/unixtransport")
    (synopsis "Support for Unix domain sockets in Go HTTP clients")
    (description
     "This package adds support for Unix domain sockets in Go HTTP clients.")
    (license license:asl2.0)))

(define-public go-github-com-pion-datachannel
  (package
    (name "go-github-com-pion-datachannel")
    (version "1.5.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/datachannel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1197ix9z1qg9xf8bhcy0k4a3ppql0jv4l8szv21wwkwzpylba0jk"))))
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
    (version "2.2.12")
    (source
     (origin
       (inherit (package-source go-github-com-pion-dtls))
       (uri (git-reference
             (url "https://github.com/pion/dtls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fihyk4p7mqilj4ymdrgns6fg3c2pfsi12v145im5vy1gxy6lc42"))))
    (arguments
     (list
      #:import-path "github.com/pion/dtls/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-transport-v2
           go-golang-org-x-crypto
           go-golang-org-x-net))))

(define-public go-github-com-pion-dtls-v3
  (package
    (inherit go-github-com-pion-dtls-v2)
    (name "go-github-com-pion-dtls-v3")
    (version "3.0.2")
    (source
     (origin
       (inherit (package-source go-github-com-pion-dtls))
       (uri (git-reference
             (url "https://github.com/pion/dtls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0czn0v2i9czq6934sirbimgkn6avgzvw63ifm2b0bkh2qmmpim01"))))
    (arguments
     (list
      #:import-path "github.com/pion/dtls/v3"))
    (native-inputs
     (modify-inputs (package-native-inputs go-github-com-pion-dtls-v2)
       (delete go-github-com-stretchr-testify)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-github-com-pion-dtls-v2)
       (replace "go-github-com-pion-transport-v2"
         go-github-com-pion-transport-v3)))))

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
      ;; Failed to build and only required for inheritance:
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
    (version "2.3.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/ice/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hiiwd3xchlybbvgd33s0i7rcwgrdiw3q963avzziycimia0qyvz"))))
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

(define-public go-github-com-pion-ice-v4
  (package
    (inherit go-github-com-pion-ice-v3)
    (name "go-github-com-pion-ice-v4")
    (version "4.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/ice/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kcq14li99dpm927rqizmmnkx6jwp9zr4fvhhv42id9dmn8y6yqj"))))
    (arguments
     (list
      #:tests? #f ;Tests require network access.
      #:import-path "github.com/pion/ice/v4"))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-github-com-pion-ice-v3)
       (replace "go-github-com-pion-dtls-v2" go-github-com-pion-dtls-v3)
       (replace "go-github-com-pion-stun-v2" go-github-com-pion-stun-v3)
       (replace "go-github-com-pion-turn-v3" go-github-com-pion-turn-v4)))))

(define-public go-github-com-pion-interceptor
  (package
    (name "go-github-com-pion-interceptor")
    (version "0.1.37")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/interceptor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dl6z8ysssq8nnkrvbi1qnhib12mdwb8psqmmdhj43yffjwb0gg6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/interceptor"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-rtcp
           go-github-com-pion-rtp
           go-github-com-pion-transport-v3))
    (home-page "https://github.com/pion/interceptor")
    (synopsis "Pluggable RTP/RTCP processors for building real time communication")
    (description
     "Interceptor is a framework for building RTP/RTCP communication software.
This framework defines a interface that each interceptor must satisfy.  These
interceptors are then run sequentially.")
    (license license:expat)))

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

(define-public go-github-com-pion-rtcp
  (package
    (name "go-github-com-pion-rtcp")
    (version "1.2.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/rtcp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00hfq0l17zq47slzfbrghgfc0v808hqiyaab3ni9kh1v7nmvp5ic"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/rtcp"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/pion/rtcp")
    (synopsis "Implementation of RTCP protocol in Golang")
    (description
     "Package rtcp implements encoding and decoding of RTCP packets according
to @url{https://www.rfc-editor.org/rfc/rfc3550, RFC 3550},
@url{https://www.rfc-editor.org/rfc/rfc5506, RFC 5506}.")
    (license license:expat)))

(define-public go-github-com-pion-rtp
  (package
    (name "go-github-com-pion-rtp")
    (version "1.8.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/rtp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a4fj31j7glhfsnxxwm8r72l6capz9mh8kfr340659ahgiijdkcg"))))
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
    (version "1.8.36")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/sctp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j9ibj7mhrnh4vmcm6dh6l7q52jq0h6788gvgsfmhh7mxrlajgy3"))))
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

(define-public go-github-com-pion-sdp-v3
  (package
    (name "go-github-com-pion-sdp-v3")
    (version "3.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/sdp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xpak20vwf3vmhi0lcj0xgy4snffsmyb6f2c6sq0w713drxqv2hb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/sdp/v3"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-randutil))
    (home-page "https://github.com/pion/sdp")
    (synopsis "Implementation of the SDP protocol in Golang")
    (description
     "Package sdp implements @acronym{Session Description Protocol,SDP}.")
    (license license:expat)))

(define-public go-github-com-pion-srtp-v2
  (package
    (name "go-github-com-pion-srtp-v2")
    (version "2.0.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/srtp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ijwx9mrc0ha8fam6y6xxih59dyr8hg9ly476kv6gfw564qfp7hk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/srtp/v2"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-rtcp
           go-github-com-pion-rtp
           go-github-com-pion-transport-v2))
    (home-page "https://github.com/pion/srtp")
    (synopsis "Implementation of SRTP protocol in Golang")
    (description
     "Package srtp implements Secure Real-time Transport Protocol.")
    (license license:expat)))

(define-public go-github-com-pion-srtp-v3
  (package
    (inherit go-github-com-pion-srtp-v2)
    (name "go-github-com-pion-srtp-v3")
    (version "3.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/srtp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03343fsgrawfy9plsl7y5022ygjln3jvsn3im5xl1qwnc68rb1a2"))))
    (arguments
     (list
      #:import-path "github.com/pion/srtp/v3"))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-github-com-pion-srtp-v2)
       (replace "go-github-com-pion-transport-v2"
         go-github-com-pion-transport-v3)))))

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

(define-public go-github-com-pion-stun-v3
  (package
    (inherit go-github-com-pion-stun-v2)
    (name "go-github-com-pion-stun-v3")
    (version "3.0.0")
    (source
     (origin
       (inherit (package-source go-github-com-pion-stun))
       (uri (git-reference
             (url "https://github.com/pion/stun")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yavl76y0fida9f1jfdmzdg7rm5jhp6kvdgn3smsf93jad1vbr2x"))))
    (arguments
     (list
      #:import-path "github.com/pion/stun/v3"))
    (propagated-inputs
     (list go-github-com-pion-dtls-v3
           go-github-com-pion-logging
           go-github-com-pion-transport-v3))))

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
    (version "2.2.10")
    (source
     (origin
       (inherit (package-source go-github-com-pion-transport))
       (uri (git-reference
             (url "https://github.com/pion/transport/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g5pg6mz61blprccxzysbwldkil84qgwp6404lsp4m9wh44312hf"))))
    (arguments
     (list
      #:import-path "github.com/pion/transport/v2"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-module-paths
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\.go$")
                  (("github.com/pion/transport/v3") import-path))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-github-com-pion-transport)
       (prepend go-github-com-wlynxg-anet)))))

(define-public go-github-com-pion-transport-v3
  (package
    (inherit go-github-com-pion-transport-v2)
    (name "go-github-com-pion-transport-v3")
    (version "3.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/transport")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05qv7packvz18dfw5bc616f7hy9ad9jz10yxckg29g60y43k5nkf"))))
    (arguments
     (substitute-keyword-arguments (package-arguments
                                    go-github-com-pion-transport-v2)
       ((#:import-path _) "github.com/pion/transport/v3")))))

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
      #:import-path "github.com/pion/turn"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Tests requiring networking setup.
               (list "TestClientWithSTUN/SendBindingRequest"
                     "TestClientWithSTUN/SendBindingRequestTo_Parallel")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
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

(define-public go-github-com-pion-turn-v4
  (package
    (inherit go-github-com-pion-turn-v3)
    (name "go-github-com-pion-turn-v4")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/turn/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yd0v6ijwl62qd2sz7imq9pd2grcrw4dhwnn4302m1k89pxr52wx"))))
    (arguments
     (substitute-keyword-arguments (package-arguments
                                    go-github-com-pion-turn-v3)
       ((#:import-path _) "github.com/pion/turn/v4")))
    (propagated-inputs
     (list go-github-com-pion-logging
           go-github-com-pion-randutil
           go-github-com-pion-stun-v3
           go-github-com-pion-transport-v3
           go-golang-org-x-sys))))

(define-public go-github-com-pion-webrtc-v3
  (package
    (name "go-github-com-pion-webrtc-v3")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/webrtc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f421a2s00mj5l9bj96xlignwfdfkp6kwk9qjs3vhazpmvqxzsgi"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Figure out why tests timeout and fail eventually.
      #:tests? #f
      #:import-path "github.com/pion/webrtc/v3"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples-and-benchmarks
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "examples"))))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-pion-datachannel
           go-github-com-pion-dtls-v2
           go-github-com-pion-ice-v2
           go-github-com-pion-interceptor
           go-github-com-pion-logging
           go-github-com-pion-randutil
           go-github-com-pion-rtcp
           go-github-com-pion-rtp
           go-github-com-pion-sctp
           go-github-com-pion-sdp-v3
           go-github-com-pion-srtp-v2
           go-github-com-pion-stun
           go-github-com-pion-transport-v2
           go-golang-org-x-net))
    (home-page "https://github.com/pion/webrtc")
    (synopsis "Implementation of the WebRTC API in Golang")
    (description
     "Package webrtc implements the @code{WebRTC} (Real-Time Communication in
Browsers) 1.0 as defined in W3C @url{https://www.w3.org/TR/webrtc/,WebRTC}
specification document.
Features:
@itemize
@item implementation of @url{https://w3c.github.io/webrtc-pc/,webrtc-pc} and
@code{https://www.w3.org/TR/webrtc-stats/,webrtc-stats}
@item DataChannels
@item Send/Receive audio and video
@item Renegotiation
@item Plan-B and Unified Plan
@item SettingEngine for Pion specific extensions
@item implemented connectivity - Full ICE Agent, ICE Restart, Trickle ICE,
STUN, TURN mDNS candidates
@end itemize")
    (license license:expat)))

(define-public go-github-com-pion-webrtc-v4
  (package
    (inherit go-github-com-pion-webrtc-v3)
    (name "go-github-com-pion-webrtc-v4")
    (version "4.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/webrtc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g7bka1x6cq4a85a73lwg8bmv9dqza0z54vdy2ny0hbypyj7xb23"))))
    (build-system go-build-system)
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-pion-webrtc-v3)
       ((#:import-path _) "github.com/pion/webrtc/v4")))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-github-com-pion-webrtc-v3)
       (replace "go-github-com-pion-dtls-v2" go-github-com-pion-dtls-v3)
       (replace "go-github-com-pion-ice-v2" go-github-com-pion-ice-v4)
       (replace "go-github-com-pion-srtp-v2" go-github-com-pion-srtp-v3)))))

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
are supported. @acronym{TLV, tag-length-value} parsers extensions coming with
this library support AWS, Azure and GCP.")
    (license license:asl2.0)))

(define-public go-github-com-pkg-browser
  (package
    (name "go-github-com-pkg-browser")
    (version "0.0.0-20240102092130-5ac0b6a4141c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pkg/browser")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05m0baqy1grd42by3vgih55473fa98cz2psyakbmf0swg8f949pn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pkg/browser"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/pkg/browser")
    (synopsis "Helpers to open files, readers, and URLs in a browser window")
    (description
     "Package browser provides helpers to open files, readers, and urls in a
browser window.  The choice of which browser is started is entirely client
dependant.")
    (license license:bsd-2)))

(define-public go-github-com-pkg-sftp
  (package
    (name "go-github-com-pkg-sftp")
    (version "1.13.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pkg/sftp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hhnsl327acwlmvp8wk4x1ml2mi6gdrdwm1dh9666n2cj9d8yj24"))))
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

(define-public go-github-com-pquerna-ffjson
  (package
    (name "go-github-com-pquerna-ffjson")
    (version "0.0.0-20190930134022-aa0246cd15f7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pquerna/ffjson")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mxmrvqmiinqhlaxncaqznxwfspf3p8bmg9vniz40dw5jpv24cwb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pquerna/ffjson"
      #:test-subdirs #~(list "fflib/v1/...")))
    (native-inputs
     (list go-github-com-google-gofuzz
           go-github-com-stretchr-testify))
    (home-page "https://github.com/pquerna/ffjson")
    (synopsis "Faster JSON for Golang")
    (description
     "This package implements functionality to generate static
@code{MarshalJSON} and @code{UnmarshalJSON} functions for structures in Go.
The generated functions reduce the reliance upon runtime reflection to do
serialization and are generally 2 to 3 times faster.  In cases where
@@code{ffjson} doesn't understand a Type involved, it falls back to
@@code{encoding/json}, meaning it is a safe drop in replacement.  By using
@code{ffjson} your JSON serialization just gets faster with no additional code
changes.")
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
    (version "0.45.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quic-go/quic-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0skg771b6h9xlssf7prkryypz4j8hnkz7k3i76qhxdc4iz4rqyfz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/quic-go/quic-go"
      #:phases
      #~(modify-phases %standard-phases
          ;; Test steps are taken from GitHub Actions -
          ;; <https://github.com/quic-go/quic-go/blob/v0.42.0/
          ;; .github/workflows/unit.yml>.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (setenv "TIMESCALE_FACTOR" "10")
                  (invoke "ginkgo" "-r" "-v" "--no-color"
                          (string-append
                           "--procs=" (number->string
                                       ;; All tests passed on 16 threads
                                       ;; mathine, but fail on
                                       ;; ci.guix.gnu.org.
                                       (if (> (parallel-job-count) 17)
                                           16
                                           (parallel-job-count))))
                          "--skip-package=integrationtests"))))))))
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
      #:import-path "github.com/quic-go/webtransport-go"
      ;; Error: "68" is not greater than "80"
      #:test-flags #~(list "-skip" "TestDatagrams")))
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

(define-public go-github-com-rcrowley-go-metrics
  (package
    (name "go-github-com-rcrowley-go-metrics")
    (version "0.0.0-20201227073835-cf1acfcdf475")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rcrowley/go-metrics")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s7zab04slz07c7l4h2cqz62qnqah69r6p157vvbd7725a7wzkr0"))))
    (build-system go-build-system)
    (arguments
     ;; Arbitrary precision tests are known to be broken on aarch64, ppc64le
     ;; and s390x. See: https://github.com/rcrowley/go-metrics/issues/249
     `(#:tests? ,(not (string-prefix? "aarch64" (or (%current-target-system)
                                                    (%current-system))))
       #:import-path "github.com/rcrowley/go-metrics"))
    (propagated-inputs
     (list go-github-com-stathat-go))
    (home-page "https://github.com/rcrowley/go-metrics")
    (synopsis "Go port of Coda Hale's Metrics library")
    (description
     "This package provides a Go implementation of Coda Hale's Metrics
library.")
    (license license:bsd-2)))

(define-public go-github-com-realclientip-realclientip-go
  (package
    (name "go-github-com-realclientip-realclientip-go")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/realclientip/realclientip-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mg3nrb4b1q38q9j1diz4pl2nqpa3ay45gi81i6ma3bvss8v7ri3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/realclientip/realclientip-go"))
    (home-page "https://github.com/realclientip/realclientip-go")
    (synopsis "Go reference implementation of \"real\" client IP algorithms")
    (description
     "Package realclientip provides strategies for obtaining the \"real\"
client IP from HTTP requests.")
    (license license:bsd-0)))

(define-public go-github-com-restic-chunker
  (package
    (name "go-github-com-restic-chunker")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/restic/chunker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bp6471lsjzjg17r3q359sz7cybjxpf5xrpndf9xhqc0v9vfx7f5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/restic/chunker"))
    (home-page "https://github.com/restic/chunker")
    (synopsis "Implementation of Content Defined Chunking Golang")
    (description
     "Package chunker implements @acronym{Content Defined Chunking,
@url{https://restic.net/blog/2015-09-12/restic-foundation1-cdc/, CDC}} based
on a rolling Rabin Checksum.")
    (license license:bsd-2)))

(define-public go-github-com-rs-cors
  (package
    (name "go-github-com-rs-cors")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rs/cors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qbzxk1aabn8k2smrkpz3h59mwr6s2zvg4faj6kjsp78hyi172xn"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodule(s) with their own go.mod files and packed as
            ;; separated packages:
            ;;
            ;; - github.com/rs/cors/wrapper/gin
            (for-each delete-file-recursively
                      (list "wrapper/gin"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rs/cors"
      #:phases
      #~(modify-phases %standard-phases
          ;; Examples requires additional dependencies and comes with their
          ;; own go.mod, consider to pack it as separate package if required.
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))
    (home-page "https://github.com/rs/cors")
    (synopsis "Golang @code{net/http} configurable handler for CORS requests")
    (description
     "Package cors is @code{net/http} handler to handle @acronym{Cross-origin
resource sharing,CORS} related requests as defined by
@url{http://www.w3.org/TR/cors/,http://www.w3.org/TR/cors/}.")
    (license license:expat)))

(define-public go-github-com-rs-xid
  (package
    (name "go-github-com-rs-xid")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rs/xid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dck1girg54kgwjms0vsizaxasc8rj6pby4rlz7m07xffa3pp45c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rs/xid"))
    (home-page "https://github.com/rs/xid")
    (synopsis "Globally Unique ID Generator")
    (description
     "Package xid is a globally unique id generator suited for web scale.
Features:
@itemize
@item zize: 12 bytes (96 bits), smaller than UUID, larger than snowflake
@item base32 hex encoded by default (20 chars when transported as printable
string, still sortable)
@item mon configured, you don't need set a unique machine and/or data center
id
@item k-ordered
@item embedded time with 1 second precision
@item unicity guaranteed for 16,777,216 (24 bits) unique ids per second and
per host/process
@item lock-free (i.e.: unlike UUIDv1 and v2)
@end itemize")
    (license license:expat)))

(define-public go-github-com-sacloud-go-http
  (package
    (name "go-github-com-sacloud-go-http")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sacloud/go-http")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c7anxj00sam5q06jlqhi1z39p19p7nffd5q07j78pahcskgvpim"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sacloud/go-http"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-hashicorp-go-retryablehttp
           go-go-uber-org-ratelimit))
    (home-page "https://github.com/sacloud/go-http")
    (synopsis "HTTP client library for SAKURA cloud in Go")
    (description
     "This package provides a HTTP client functionality that can be used
across various @url{https://www.sakura.ad.jp/, Sakura Cloud} APIs (IaaS,
ObjectStorage, PHY, etc.).")
    (license license:asl2.0)))

(define-public go-github-com-safchain-ethtool
  (package
    (name "go-github-com-safchain-ethtool")
    (version "0.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/safchain/ethtool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01q0qfyksnhd4a2w2824yzdmyb4g4xr0y2z4ffnpbzz1wp60rydg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/safchain/ethtool"
      #:test-flags
      ;; Unable to retrieve driver from any interface of this system.
      #~(list "-skip"
              (string-join
               (list "TestCmdGet"
                     "TestCmdGetMapped"
                     "TestMsglvlGet"
                     "TestStats"
                     "TestDriverName"
                     "TestBusInfo"
                     #$@(if (target-arm?)
                            ;; Inappropriate ioctl for device.
                            '("TestFeatures")
                            '()))
               "|"))))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/safchain/ethtool")
    (synopsis "Ethtool library for Golang")
    (description
     "The ethtool package aims to provide a library that implements easy
access to the Linux SIOCETHTOOL ioctl operations.  It can be used to retrieve
information from a network device such as statistics, driver related
information or even the peer of a VETH interface.")
    (license license:asl2.0)))

(define-public go-github-com-sherclockholmes-webpush-go
  (package
    (name "go-github-com-sherclockholmes-webpush-go")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SherClockHolmes/webpush-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dzc2nlxd1lfwr97cqmr4fc792zbsb6yssr7yqxxamqck1y8gnqm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/SherClockHolmes/webpush-go"))
    (propagated-inputs
     (list go-github-com-golang-jwt-jwt-v5 go-golang-org-x-crypto))
    (home-page "https://github.com/SherClockHolmes/webpush-go")
    (synopsis "Web Push API Encryption with VAPID support")
    (description
     "Web Push API Encryption with
@url{https://datatracker.ietf.org/doc/html/draft-ietf-webpush-vapid-01, VAPID}
support.")
    (license license:expat)))

(define-public go-github-com-shurcool-githubv4
  (package
    (name "go-github-com-shurcool-githubv4")
    (version "0.0.0-20240429030203-be2daab69064")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shurcooL/githubv4")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kkvqwv0waa8hj1jr9b4nvz8rmslqpchidl7gs9bplrkl3fvsxn6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shurcooL/githubv4"))
    (propagated-inputs
     (list go-github-com-shurcool-graphql
           go-golang-org-x-oauth2))
    (home-page "https://github.com/shurcooL/githubv4")
    (synopsis "Client library for GitHub GraphQL API v4")
    (description
     "Package githubv4 is a client library for accessing GitHub @code{GraphQL}
API v4.")
    (license license:expat)))

(define-public go-github-com-shurcool-graphql
  (package
    (name "go-github-com-shurcool-graphql")
    (version "0.0.0-20230722043721-ed46e5a46466")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shurcooL/graphql")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12cq16qak217bkpklqsmqhl42gz8cpadpzqw6fsivc3ambjpqdry"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shurcooL/graphql"))
    (home-page "https://github.com/shurcooL/graphql")
    (synopsis "GraphQL client")
    (description
     "Package graphql provides a @code{GraphQL} client implementation.")
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

(define-public go-github-com-ssgelm-cookiejarparser
  (package
    (name "go-github-com-ssgelm-cookiejarparser")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ssgelm/cookiejarparser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fnm53br0cg3iwzniil0lh9w4xd6xpzfypwfpdiammfqavlqgcw4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/ssgelm/cookiejarparser"))
    (propagated-inputs (list go-golang-org-x-net))
    (home-page "https://github.com/ssgelm/cookiejarparser")
    (synopsis "Parse a curl cookiejar with Go")
    (description
     "This package is a Go library that parses a curl (netscape) cookiejar
file into a Go http.CookieJar.")
    (license license:expat)))

(define-public go-github-com-stathat-go
  (package
    (name "go-github-com-stathat-go")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stathat/go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zzlsl24dyr202qkr2pay22m6d0gb7ssms77wgdx0r0clgm7dihw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/stathat/go"))
    (home-page "https://github.com/stathat/go")
    (synopsis "Post statistics to StatHat")
    (description
     "This is a Go package for posting to a @url{https://www.stathat.com/,
StatHat} account.")
    (license license:expat)))

(define-public go-github-com-swaggo-swag
  (package
    (name "go-github-com-swaggo-swag")
    (version "1.16.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaggo/swag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a9dg8clgmpsfww5wv3jbdpm7lqza61iihviskwp5rd7wvp57862"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/swaggo/swag"
      #:unpack-path "github.com/swaggo/swag"
      #:embed-files
      #~(list
         ;; github.com/go-openapi/spec/embed.go:8:12: pattern schemas/*.json:
         ;; cannot embed irregular file schemas/jsonschema-draft-04.json
         "jsonschema-draft-04\\.json"
         ;; github.com/go-openapi/spec/embed.go:8:27: pattern
         ;; schemas/*/*.json: cannot embed irregular file
         ;; schemas/v2/schema.json
         "schema\\.json")
      #:test-flags
      #~(list "-skip" (string-append "TestParseGoList/enableGOMODULE"
                                     "|TestParseDescriptionMarkdown"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (delete-file-recursively "example")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-openapi-spec
           go-github-com-kylebanks-depth
           go-github-com-urfave-cli-v2
           go-golang-org-x-text
           go-golang-org-x-tools
           go-sigs-k8s-io-yaml))
    (home-page "https://github.com/swaggo/swag")
    (synopsis "Generate RESTful API documentation with Swagger 2.0 for Go")
    (description
     "Package swag converts Go annotations to Swagger Documentation 2.0 for
verity of Go web frameworks which may be integrated with an existing project
using Swagger UI.")
    (license license:expat)))

(define-public go-github-com-tdewolff-minify-v2
  (package
    (name "go-github-com-tdewolff-minify-v2")
    (version "2.21.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/minify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vhblx1xim14i4npglzdp9hpjz92q2k29wbf9kp9m7id9cm7c7l9"))))
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
                   (invoke "go" "generate" "-v" "-n")))
               '("css" "html" "svg")))))))
    ;; For tests and the CLI.
    (native-inputs
     (list go-github-com-djherbis-atime
           go-github-com-fsnotify-fsnotify
           go-github-com-matryer-try
           go-github-com-tdewolff-argp
           go-github-com-tdewolff-hasher ; to generate go files
           go-github-com-tdewolff-test))
    (propagated-inputs
     (list go-github-com-tdewolff-parse-v2))
    (home-page "https://go.tacodewolff.nl/minify")
    (synopsis "Go minifiers for web formats")
    (description
     "This package provides HTML5, CSS3, JS, JSON, SVG and XML minifiers and
an interface to implement any other minifier.")
    (license license:expat)))

(define-public go-github-com-tdewolff-parse-v2
  (package
    (name "go-github-com-tdewolff-parse-v2")
    (version "2.7.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/parse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17yswndnxgdj425h5q25wfvchjxnjf6nxyx76k9yn12r16arbl44"))))
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

(define-public go-github-com-tetratelabs-wabin
  (package
    (name "go-github-com-tetratelabs-wabin")
    (version "0.0.0-20230304001439-f6f874872834")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tetratelabs/wabin")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "195bh4n2ba3rbgzcb1h7zi93dr0k38qxhg8m0laa0z41vl9i0igm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tetratelabs/wabin"
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
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/tetratelabs/wabin")
    (synopsis "WebAssembly Binary Format in Go")
    (description
     "This package provides @code{WebAssembly} a @code{WebAssembly} data model
and binary encoder.")
    (license license:asl2.0)))

(define-public go-github-com-tetratelabs-wazero
  (package
    (name "go-github-com-tetratelabs-wazero")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tetratelabs/wazero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xchvrkp6m729x3jknj3qwms4w2b2q8kcwyxhkmagms43yg4ykm5"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each delete-file-recursively
                      (list
                       ;; This directory holds the wazero site's source code.
                       "site"
                       ;; Windows related MSI packaging files.
                       "packaging"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tetratelabs/wazero"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestHugePageConfigs"
                             "TestRun"
                             "TestRun/3_1"
                             "Test_cli"
                             "Test_cli/cargo-wasi"
                             "Test_cli/cargo-wasi/test.txt"
                             "Test_cli/cargo-wasi/testcases/test.txt"
                             "Test_cli/tinygo"
                             "Test_cli/tinygo/test.txt"
                             "Test_cli/tinygo/testcases/test.txt"
                             "Test_cli/zig"
                             "Test_cli/zig-cc"
                             "Test_cli/zig-cc/test.txt"
                             "Test_cli/zig-cc/testcases/test.txt"
                             "Test_cli/zig/test.txt")
                       "|"))))
    (home-page "https://github.com/tetratelabs/wazero")
    (synopsis "Zero dependency WebAssembly runtime for Go")
    (description
     "wazero is a WebAssembly Core Specification
@url{https://www.w3.org/TR/2019/REC-wasm-core-1-20191205/,1.0} and
@code{https://www.w3.org/TR/2022/WD-wasm-core-2-20220419/,2.0} compliant
runtime.  It has zero dependencies, and doesn't rely on CGO.  This means you
can run applications in other languages and still keep cross compilation.")
    (license license:asl2.0)))

(define-public go-github-com-tomnomnom-linkheader
  (package
    (name "go-github-com-tomnomnom-linkheader")
    (version "0.0.0-20180905144013-02ca5825eb80")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tomnomnom/linkheader")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ghrv28vrvvrpyr4d4q817yby8g1j04mid8ql00sds1pvfv67d32"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tomnomnom/linkheader"))
    (home-page "https://github.com/tomnomnom/linkheader")
    (synopsis "Golang HTTP Link header parser")
    (description
     "Package linkheader provides functions for parsing HTTP Link headers.")
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

(define-public go-github-com-txthinking-socks5
  (package
    (name "go-github-com-txthinking-socks5")
    (version "0.0.0-20230325130024-4230056ae301")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/txthinking/socks5")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zbwczxwmx8ngs6s0bnb0v73jvx96m9ll753zfgcns8fvvgdi2lb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/txthinking/socks5"))
    (propagated-inputs
     (list go-github-com-miekg-dns
           go-github-com-patrickmn-go-cache
           go-github-com-txthinking-runnergroup))
    (home-page "https://github.com/txthinking/socks5")
    (synopsis "SOCKSv5 protocol Golang library")
    (description
     "This package provides a SOCKS protocol version 5 library with full
TCP/UDP and IPv4/IPv6 support.")
    (license license:expat)))

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

(define-public go-github-com-urfave-negroni
  (package
    (name "go-github-com-urfave-negroni")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/negroni")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gp6j74adi1cn8fq5v3wzlzhwl4zg43n2746m4fzdcdimihk3ccp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/urfave/negroni"))
    (home-page "https://github.com/urfave/negroni")
    (synopsis "Idiomatic HTTP Middleware for Golang")
    (description
     "Package negroni is an idiomatic approach to web middleware in Go.  It is
tiny,non-intrusive, and encourages use of @code{net/http} Handlers.")
    (license license:expat)))

(define-public go-github-com-urfave-negroni-v3
  (package
    (inherit go-github-com-urfave-negroni)
    (name "go-github-com-urfave-negroni-v3")
    (version "3.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/negroni")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04fbp15jq23sp10kgrpgmbif3mvzs82m1wx2bbmgknh27yy8i95y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/urfave/negroni/v3"))))

(define-public go-github-com-valyala-fasthttp
  (package
    (name "go-github-com-valyala-fasthttp")
    (version "1.58.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/fasthttp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r823fikgyhnmcn322anhs3ivkbzhdgbywvwi81v9kvfhi35plli"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/valyala/fasthttp"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Network set up is required.
                       (list "TestClientConfigureClientFailed"
                             "TestDialer_GetDialFunc")
                       "|"))
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

(define-public go-github-com-vektah-gqlparser-v2
  (package
    (name "go-github-com-vektah-gqlparser-v2")
    (version "2.5.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vektah/gqlparser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hrzm9f3kvcblff4hypf1p95kxsv5pww7vcghhw2jb7r8r4kmdf0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/vektah/gqlparser/v2"))
    (native-inputs
     (list go-github-com-andreyvit-diff
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-agnivade-levenshtein
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/vektah/gqlparser")
    (synopsis "Port of the parser from @code{graphql-js} into golang")
    (description
     "This is a parser for GraphQL, written to mirror the
@url{https://github.com/graphql/graphql-js, graphql-js} reference
implementation as closely while remaining idiomatic and easy to use.")
    (license license:expat)))

(define-public go-github-com-vishvananda-netlink
  (package
    (name "go-github-com-vishvananda-netlink")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vishvananda/netlink")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ckwb1ml7i2ccdd7kzc04s839naf4arlxav2ip5kf4rm4xhba9g7"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; The tests are unsupported on all architectures except x86_64-linux:
      ;; cannot use 0xabcdef99 (untyped int constant 2882400153) as int value
      ;; in struct literal (overflows)
      #:tests? (and (not (%current-target-system)) (target-x86-64?))
      #:test-flags
      ;; One test fails with error: operation not permitted.
      #~(list "-skip" "TestSocketXDPGetInfo")
      #:import-path "github.com/vishvananda/netlink"))
    (propagated-inputs
     (list go-github-com-vishvananda-netns
           go-golang-org-x-sys))
    (home-page "https://github.com/vishvananda/netlink")
    (synopsis "Simple netlink library for Go")
    (description
     "The netlink package provides a simple netlink library for Go.  Netlink
is the interface a user-space program in Linux uses to communicate with the
kernel.  It can be used to add and remove interfaces, set IP addresses and
routes, and configure IPsec.")
    (license license:asl2.0)))

(define-public go-github-com-vishvananda-netns
  (package
    (name "go-github-com-vishvananda-netns")
    (version "0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vishvananda/netns")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f96fbmjq93msdfxmicnypnn2lzvi7jrxy82fiyd9gwxdapfd061"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests fail with error: operation not permitted.
      #:test-flags #~(list "-skip" "TestGetNewSetDelete|TestThreaded")
      #:import-path "github.com/vishvananda/netns"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/vishvananda/netns")
    (synopsis "Simple network namespace handling for Go")
    (description
     "The netns package provides a simple interface for handling network
namespaces in Go.")
    (license license:asl2.0)))

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

(define-public go-github-com-willscott-goturn
  (package
    (name "go-github-com-willscott-goturn")
    (version "0.0.0-20170802220503-19f41278d0c9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/willscott/goturn")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zwvhfznr84ayzknn9flh65nvqjsixisgy9fkhz2jlahl1ldqcq7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; tests are broken on a newer go, starting from 1.17.
      #:import-path "github.com/willscott/goturn"))
    (home-page "https://github.com/willscott/goturn")
    (synopsis "Go TURN dialer")
    (description
     "GoTURN is a library providing a Go interface compatible with the
golang proxy package which connects through a TURN relay.  It provides parsing
and encoding support for STUN and TURN protocols.")
    (license license:bsd-3)))

(define-public go-github-com-wlynxg-anet
  (package
    (name "go-github-com-wlynxg-anet")
    (version "0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wlynxg/anet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09s3x28qb571xs92gnbqzkyiah4vhk56hqqb4cc4yfrzfv3430hp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/wlynxg/anet"))
    (home-page "https://github.com/wlynxg/anet")
    (synopsis "Adjusted @code{net.Interfaces()} for Golang")
    (description
     "This package implements a functionality to resolve some problems for
Android environment where standard @code{net} and @code{golang.org/x/net}
missing it.  It address the issues
@url{https://github.com/golang/go/issues/40569, #40569} and
@url{https://github.com/golang/go/issues/68082, #68082}.")
    (license license:bsd-3)))

(define-public go-github-com-xdg-go-scram
  (package
    (name "go-github-com-xdg-go-scram")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xdg-go/scram")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bkznny84fdn4sy3wq5680v424glsq9nh2169qy51213bah6by84"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xdg-go/scram"))
    (propagated-inputs
     (list go-github-com-xdg-go-stringprep
           go-github-com-xdg-go-pbkdf2))
    (home-page "https://github.com/xdg-go/scram")
    (synopsis "Go implementation of RFC-5802")
    (description
     "Package scram provides client and server implementations of the
@acronym{Salted Challenge Response Authentication Mechanism, SCRAM} described
in RFC-5802 and RFC-7677.")
    (license license:asl2.0)))

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
  (package
    (name "go-github-com-xeipuuv-gojsonschema")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xeipuuv/gojsonschema")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mqiq0r8qw4qlfp3ls8073r6514rmzwrmdn4j33rppk3zh942i6l"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; validation of time strings.  only RFC3339 not all of ISO 8601 are
      ;; valid.  expects: false, given true Schema: {"format":"time"} Data:
      ;; "01:01:01,1111"
      #:test-flags #~(list "-skip" "TestFormats")
      #:import-path "github.com/xeipuuv/gojsonschema"))
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
    (license license:asl2.0)))

(define-public go-github-com-xo-dburl
  (package
    (name "go-github-com-xo-dburl")
    (version "0.23.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xo/dburl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yxyfrm45dqd26513r2cvgkqfsclijsfzqrckbw45wydk45mq442"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xo/dburl"))
    (home-page "https://github.com/xo/dburl")
    (synopsis
     "URL style mechanism for parsing and opening SQL database connection strings")
    (description
     "Package dburl provides a standard, @code{net/url.URL} style mechanism
for parsing and opening SQL database connection strings for Go.  Provides
standardized way to parse and open @@url{#URL,URL}'s for popular databases
@code{PostgreSQL}, @code{MySQL}, SQLite3, Oracle Database, Microsoft SQL
Server, in addition to most other SQL databases with a publicly available Go
driver.")
    (license license:expat)))

(define-public go-github-com-xtaci-kcp-go-v5
  (package
    (name "go-github-com-xtaci-kcp-go-v5")
    (version "5.6.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xtaci/kcp-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17jqgl2zdra9rz8ap3zlrk7ljnm316gd4dy4cimlk2641n8r5bjx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xtaci/kcp-go/v5"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-klauspost-reedsolomon
           go-github-com-pkg-errors
           go-github-com-templexxx-xorsimd
           go-github-com-tjfoc-gmsm
           go-github-com-xtaci-lossyconn
           go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://github.com/xtaci/kcp-go")
    (synopsis "Crypto-Secure Reliable-UDP Library with FEC")
    (description
     "This package provides smooth, resilient, ordered, error-checked and
anonymous delivery of streams over UDP packets.
Features:
@itemize
@item designed for latency-sensitive scenarios
@item cache-friendly and memory-optimized design
@item handles >5K concurrent connections on a single commodity server
@item compatible with @code{net.Conn} and @code{net.Listener}, serving as a
drop-in replacement for @code{net.TCPConn}
@item @acronym{Forward Error Correction, FEC} support with
@url{https://en.wikipedia.org/wiki/Reed%E2%80%93Solomon_error_correction,
Reed-Solomon Codes}
@item packet-level encryption support with @code{3DES}, @code{AES},
@code{Blowfish}, @code{Cast5}, @code{PBKDF2}, @code{SM4}, @code{Salsa20},
@code{TEA}, @code{Twofish}, and @code{XTEA}
@item only a fixed number of goroutines are created for the entire server
application
@item compatible with @url{https://github.com/skywind3000, skywind3000}'s C
version with various improvements
@end itemize")
    (license license:expat)))

(define-public go-github-com-xtaci-lossyconn
  (package
    (name "go-github-com-xtaci-lossyconn")
    (version "0.0.0-20200209145036-adba10fffc37")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xtaci/lossyconn")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pizmd8py5x8yjqp6fz2y0kpvd1za3rbp7whyyh69zyk1a305fam"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: In CI with 16+ threads goroutines fail. Figure out how to detect
      ;; CI enviroment.
      #:tests? #f
      #:parallel-tests? #f
      #:import-path "github.com/xtaci/lossyconn"))
    (home-page "https://github.com/xtaci/lossyconn")
    (synopsis "Lossy connection simulator")
    (description
     "Package lossyconn is a lossy connection simulator for Golang.")
    (license license:expat)))

(define-public go-github-com-xtaci-smux
  (package
    (name "go-github-com-xtaci-smux")
    (version "1.5.30")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xtaci/smux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i4h672vmg2b4p8hkbhpsp8p2nk4d3qm6vf76yly389l2zb7h4l3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xtaci/smux"))
    (home-page "https://github.com/xtaci/smux")
    (synopsis "Introduction")
    (description
     "Smux (@strong{S}imple @strong{MU}ltiple@strong{X}ing) is a multiplexing
library for Golang.  It relies on an underlying connection to provide
reliability and ordering, such as TCP or
@url{https://github.com/xtaci/kcp-go,KCP}, and provides stream-oriented
multiplexing.  The original intention of this library is to power the
connection management for @url{https://github.com/xtaci/kcp-go,kcp-go}.")
    (license license:expat)))

(define-public go-github-com-yggdrasil-network-yggdrasil-go
  (package
    (name "go-github-com-yggdrasil-network-yggdrasil-go")
    (version "0.5.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yggdrasil-network/yggdrasil-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "072r52b6bkpc7bhn0v1z6dm6q5g9qf4k1xlqwrvzmzwai6fm0lrn"))
      (patches (search-patches "yggdrasil-extra-config.patch"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/yggdrasil-network/yggdrasil-go"))
    (propagated-inputs
     (list go-github-com-arceliar-ironwood
           go-github-com-arceliar-phony
           go-github-com-cheggaaa-pb-v3
           go-github-com-coder-websocket
           go-github-com-gologme-log
           go-github-com-hashicorp-go-syslog
           go-github-com-hjson-hjson-go-v4
           go-github-com-kardianos-minwinsvc
           go-github-com-olekukonko-tablewriter
           go-github-com-quic-go-quic-go
           go-github-com-vishvananda-netlink
           go-github-com-wlynxg-anet
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-text
           ;; go-golang-zx2c4-com-wintun
           go-golang-zx2c4-com-wireguard
           ;; go-golang-zx2c4-com-wireguard-windows
           go-suah-dev-protect))
    (home-page "https://github.com/yggdrasil-network/yggdrasil-go")
    (synopsis "IPv6 overlay network Golang library")
    (description
     "This package provides a source code for P2P overlay Yaggdrasil network.
It is to used for inputs in other packages.")
    (license
     ;; As a special exception to the GNU Lesser General Public License
     ;; version 3 ("LGPL3"), the copyright holders of this Library give you
     ;; permission to convey to a third party a Combined Work that links
     ;; statically or dynamically to this Library without providing any Minimal
     ;; Corresponding Source or Minimal Application Code as set out in 4d or
     ;; providing the installation information set out in section 4e, provided
     ;; that you comply with the other provisions of LGPL3 and provided that you
     ;; meet, for the Application the terms and conditions of the license(s)
     ;; which apply to the Application. Except as stated in this special
     ;; exception, the provisions of LGPL3 will continue to comply in full to
     ;; this Library. If you modify this Library, you may apply this exception
     ;; to your version of this Library, but you are not obliged to do so. If
     ;; you do not wish to do so, delete this exception statement from your
     ;; version. This exception does not (and cannot) modify any license terms
     ;; which apply to the Application, with which you must still comply
     license:lgpl3)))

(define-public go-gitlab-com-gitlab-org-api-client-go
  (package
    (name "go-gitlab-com-gitlab-org-api-client-go")
    (version "0.123.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/gitlab-org/api/client-go.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xrqyqvmaslfr9cj91519qi5f2sd3l2mvipxgjf13vd3v4fchacn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "gitlab.com/gitlab-org/api/client-go"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Tests requir network access.
               (list "TestGetMergeRequest"
                     "TestRepositoryFilesService_CreateFile"
                     "TestRepositoryFilesService_DeleteFile"
                     "TestRepositoryFilesService_GetFile"
                     "TestRepositoryFilesService_GetFileBlame"
                     "TestRepositoryFilesService_GetFileMetaData"
                     "TestRepositoryFilesService_GetRawFile"
                     "TestRepositoryFilesService_UpdateFile"
                     "TestRepositorySubmodulesService_UpdateSubmodule"
                     "TestUpdateRepositoryEnvironmentsEscapesURL")
               "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-go-querystring
           go-github-com-hashicorp-go-cleanhttp
           go-github-com-hashicorp-go-retryablehttp
           go-golang-org-x-oauth2
           go-golang-org-x-time))
    (home-page "https://gitlab.com/gitlab-org/api/client-go")
    (synopsis "GitLab client for Golang")
    (description
     "This package provides a GitLab API client enabling Go programs to
interact with GitLab in a simple and uniform way.")
    (license license:asl2.0)))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-geoip
  (package
    (name "go-gitlab-torproject-org-tpo-anti-censorship-geoip")
    (version "0.0.0-20210928150955-7ce4b3d98d01")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.torproject.org/tpo/anti-censorship/geoip.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i7dc717w1g7hk7488vscqxj0a10af6fz9jczxxsfyxagynfzqcq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitlab.torproject.org/tpo/anti-censorship/geoip"))
    (propagated-inputs
     (list go-github-com-smartystreets-goconvey))
    (home-page "https://gitlab.torproject.org/tpo/anti-censorship/geoip")
    (synopsis "GeoIP go library that uses the tor geoipdb")
    (description
     "This code is for loading database data that maps IP addresses to
countries for collecting and presenting statistics on snowflake use that might
alert us to censorship events.")
    (license license:bsd-3)))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib
  (package
    (name "go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/goptlib")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jw9vlvlx7rrl366kwz47414aciw3r37lwg6h4jq8cj5hb4bqnd9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/goptlib"))
    (home-page "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/goptlib")
    (synopsis "Go pluggable transports library")
    (description "GoPtLib is a library for writing Tor pluggable transports in
Go.")
    (license license:cc0)))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-ptutil
  (package
    (name
     "go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-ptutil")
    (version "0.0.0-20240710081135-6c4d8ed41027")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append
                   "https://gitlab.torproject.org/tpo/anti-censorship"
                   "/pluggable-transports/ptutil.git"))
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h7ssgsny6abhpycgks1kvqzvd20s081n39j5yxjjr7zn495ysdc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path
      "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/ptutil"))
    (propagated-inputs
     (list go-github-com-prometheus-client-golang
           go-github-com-prometheus-client-model
           go-google-golang-org-protobuf))
    (home-page
     "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/ptutil")
    (synopsis "Collection of utilities for Pluggable Transports")
    (description
     "This package provides a collection of utilities for Pluggable Transports.")
    (license license:bsd-3)))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-snowflake-v2
  (package
    (name
     "go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-snowflake-v2")
    (version "2.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append
                   "https://gitlab.torproject.org/tpo/anti-censorship"
                   "/pluggable-transports/snowflake"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14ypgzj6c6vjw9s85wf2vdfa9l06iandx7gz90i3w6r65q2cp6vj"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Project provides a Go library and also CLI builds.
      #:skip-build? #t
      #:import-path
      "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/v2"
      ;; panic: empty transcript [recovered]
      #:test-flags #~(list "-skip" "TestQueuePacketConnWriteToKCP")))
    (native-inputs
     (list go-github-com-golang-mock
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-credentials
           go-github-com-aws-aws-sdk-go-v2-service-sqs
           go-github-com-golang-mock
           go-github-com-gorilla-websocket
           go-github-com-miekg-dns
           go-github-com-pion-ice-v2
           go-github-com-pion-sdp-v3
           go-github-com-pion-stun-v3
           go-github-com-pion-transport-v2
           go-github-com-pion-webrtc-v3
           go-github-com-prometheus-client-golang
           go-github-com-realclientip-realclientip-go
           go-github-com-refraction-networking-utls
           go-github-com-smartystreets-goconvey
           go-github-com-stretchr-testify
           go-github-com-txthinking-socks5
           go-github-com-xtaci-kcp-go-v5
           go-github-com-xtaci-smux
           go-gitlab-torproject-org-tpo-anti-censorship-geoip
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-ptutil
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page
     "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake")
    (synopsis "Pluggable Transport using WebRTC, inspired by Flashproxy")
    (description
     "Pluggable Transport using @code{WebRTC}, inspired by Flashproxy and
adheres to the pluggable transports v2.1 Go AP.
This package provides:
@itemize
@item @code{broker} contains code for the Snowflake broker
@item @code{doc} contains Snowflake documentation and manpages
@item @code{client} contains the Tor pluggable transport client and client
library code
@item @code{common} contains generic libraries used by multiple pieces of
Snowflake
@item @code{proxy} contains code for the Go standalone Snowflake proxy
@item @code{probetest} contains code for a NAT probetesting service
@item @code{server} contains the Tor pluggable transport server and server
library code
@end itemize")
    (license license:bsd-3)))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-webtunnel
  (package
    (name "go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-webtunnel")
    (version "0.0.0-20240711104640-e64b1b3562f3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/webtunnel")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nvd0qp1mdy7w32arnkhghxm5k2g6gy33cxlarxc6vdm4yh6v5nv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/webtunnel"))
    (propagated-inputs
     (list go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib))
    (home-page "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/webtunnel")
    (synopsis "Go WebTunnel Pluggable Transport")
    (description
     "WebTunnel is a Go Pluggable Transport that attempts to imitate web
browsing activities based on HTTP Upgrade (HTTPT).")
    (license license:bsd-2)))

(define-public go-go-mau-fi-whatsmeow
  (package
    (name "go-go-mau-fi-whatsmeow")
    (version "0.0.0-20241215104421-68b0856cce22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tulir/whatsmeow")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y3h132g3w6ihc8jn587wvyha9xm3sinjlr0znqq7krvynz3z8id"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.mau.fi/whatsmeow"))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-github-com-gorilla-websocket
           go-github-com-rs-zerolog
           go-go-mau-fi-libsignal
           go-go-mau-fi-util
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-google-golang-org-protobuf))
    (home-page "https://go.mau.fi/whatsmeow")
    (synopsis "Go library for the WhatsApp web multidevice API")
    (description
     "Package whatsmeow implements a client for interacting with the
@code{WhatsApp} web multidevice API.

Features:
@itemize
@item sending messages to private chats and groups (both text and media)
@item receiving all messages
@item managing groups and receiving group change events
@item joining via invite messages, using and creating invite links
@item sending and receiving typing notifications
@item sending and receiving delivery and read receipts
@item reading and writing app state (contact list, chat pin/mute status, etc)
@item sending and handling retry receipts if message decryption fails
@item sending status messages (experimental, may not work for large contact
lists)
@end itemize")
    (license license:mpl2.0)))

(define-public go-go-opencensus-io
  (package
    (name "go-go-opencensus-io")
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/census-instrumentation/opencensus-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1923j8v214fyk9qlw0lfva6ah8p7s8cfkrysiada5pp4jim4k4xi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "go.opencensus.io"
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Tests requiring networking.
               (list "TestAgainstSpecs/Successful_GET_call_to_https.*"
                     "TestAgainstSpecs/Successfully_POST_call_to_https.*")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (propagated-inputs
     (list go-github-com-golang-groupcache
           go-github-com-golang-protobuf
           go-github-com-google-go-cmp
           go-github-com-stretchr-testify
           go-golang-org-x-net
           go-google-golang-org-grpc))
    (home-page "https://opencensus.io/")
    (synopsis "Stats collection and distributed tracing framework")
    (description
     "Package opencensus contains Go support for @code{OpenCensus}.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
  (package
    (name "go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp")
    (version "0.59.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "instrumentation/net/http/otelhttp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17kyba5816983migninw6v2si2d28j32973c0x8i08fswrjz5dm0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "go.opentelemetry.io/contrib/instrumentation/net/http/otelhttp"
      #:unpack-path "go.opentelemetry.io/contrib"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (delete-file-recursively
                 "instrumentation/net/http/otelhttp/example")))))))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-go-opentelemetry-io-otel-sdk
           go-go-opentelemetry-io-otel-sdk-metric))
    (propagated-inputs
     (list go-github-com-felixge-httpsnoop
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-metric
           go-go-opentelemetry-io-otel-trace))
    (home-page "https://opentelemetry.io/")
    (synopsis "Golang std @code{http.Handler} wrapper library")
    (description
     "Package otelhttp provides an http.Handler and functions that are
intended to be used to add tracing by wrapping existing handlers (with
Handler) and routes @code{WithRouteTag}.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel
  (package
    (name "go-go-opentelemetry-io-otel")
    (version "1.30.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mpkwz2ryah5j2fb835pdw9084nwhr6fj3jig2mnvwwnsymp2bvy"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packed as separated
            ;; packages:
            (for-each
             delete-file-recursively
             ;; Do not sort the list, it needs to be formatted with
             ;; the longest path to go.mod first, shell snippet to
             ;; produce the list:
             ;; find . -type f -name go.mod -printf "%d %p\n" | sort -rn
             (list "exporters/otlp/otlptrace/otlptracehttp"
                   "exporters/otlp/otlptrace/otlptracegrpc"
                   "exporters/otlp/otlpmetric/otlpmetrichttp"
                   "exporters/otlp/otlpmetric/otlpmetricgrpc"
                   "exporters/otlp/otlplog/otlploghttp"
                   "exporters/otlp/otlplog/otlploggrpc"
                   "exporters/stdout/stdouttrace"
                   "exporters/stdout/stdoutmetric"
                   "exporters/stdout/stdoutlog"
                   "exporters/otlp/otlptrace"
                   "sdk/metric"
                   "sdk/log"
                   "internal/tools"
                   "exporters/zipkin"
                   "exporters/prometheus"
                   "example/zipkin"
                   "example/prometheus"
                   "example/passthrough"
                   "example/otel-collector"
                   "example/opencensus"
                   "example/namedtracer"
                   "example/dice"
                   "bridge/opentracing"
                   "bridge/opencensus"
                   ;; "trace"  - introduces a cycle, keep it
                   "sdk"
                   "schema"
                   ;; "metric" - introduces a cycle, keep it
                   "log"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.opentelemetry.io/otel"
      ;; Error: Both arguments must be pointers.
      #:test-flags #~(list "-skip" "TestTraceProviderDelegatesSameInstance")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-logr-logr
           go-github-com-go-logr-stdr
           go-github-com-google-go-cmp))
    (home-page "https://opentelemetry.io/")
    (synopsis "OpenTelemetry implementation for Golang")
    (description
     "Package otel provides global access to the @code{OpenTelemetry} API.
The subpackages of the otel package provide an implementation of the
@code{OpenTelemetry} API.  This package contains 3 Golang modules:
go.opentelemetry.io/otel, go.opentelemetry.io/otel/metric and
go.opentelemetry.io/otel/trace.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-log
  (package
    (name "go-go-opentelemetry-io-otel-log")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "log"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sb36qyq389fif9qp5iiqp6w41dfcwi95gb0bsbvznvijhd8c1cc"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            ;; Consider to implement it as re-usable procedure in
            ;; guix/build/utils or guix/build-system/go.
            (define (delete-all-but directory . preserve)
              (define (directory? x)
                (and=> (stat x #f)
                       (compose (cut eq? 'directory <>) stat:type)))
              (with-directory-excursion directory
                (let* ((pred
                        (negate (cut member <> (append '("." "..") preserve))))
                       (items (scandir "." pred)))
                  (for-each (lambda (item)
                              (if (directory? item)
                                  (delete-file-recursively item)
                                  (delete-file item)))
                            items))))
            (delete-all-but "." "log")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.opentelemetry.io/otel/log"
      #:unpack-path "go.opentelemetry.io/otel"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-logr-logr
           go-go-opentelemetry-io-otel))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry Logs API")
    (description
     "This package is intended to be used by bridges between existing logging
libraries and OpenTelemetry.  Users should not directly use this package as a
logging library.  Instead, install one of the bridges listed in the
[registry], and use the associated logging library.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-metric
  (package/inherit go-go-opentelemetry-io-otel
    (name "go-go-opentelemetry-io-otel-metric")
    (arguments
     (list
      #:import-path "go.opentelemetry.io/otel/metric"
      #:unpack-path "go.opentelemetry.io/otel"))
    (synopsis "OpenTelemetry Metric API")
    (description
     "Package metric provides an implementation of the metric part of the
OpenTelemetry API.")))

(define-public go-go-opentelemetry-io-otel-sdk
  (package
    (name "go-go-opentelemetry-io-otel-sdk")
    (version "1.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version #:subdir "sdk"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sb36qyq389fif9qp5iiqp6w41dfcwi95gb0bsbvznvijhd8c1cc"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            ;; Consider to implement it as re-usable procedure in
            ;; guix/build/utils or guix/build-system/go.
            (define (delete-all-but directory . preserve)
              (define (directory? x)
                (and=> (stat x #f)
                       (compose (cut eq? 'directory <>) stat:type)))
              (with-directory-excursion directory
                (let* ((pred
                        (negate (cut member <> (append '("." "..") preserve))))
                       (items (scandir "." pred)))
                  (for-each (lambda (item)
                              (if (directory? item)
                                  (delete-file-recursively item)
                                  (delete-file item)))
                            items))))
            (delete-all-but "." "sdk")
            (delete-file-recursively "sdk/log")
            (delete-file-recursively "sdk/metric")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.opentelemetry.io/otel/sdk"
      #:unpack-path "go.opentelemetry.io/otel"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-logr-logr
           go-github-com-google-go-cmp
           go-github-com-google-uuid
           go-go-opentelemetry-io-otel
           go-golang-org-x-sys))
    (home-page "https://opentelemetry.io/")
    (synopsis "OpenTelemetry Golang SDK")
    (description
     "This package provides OpenTelemetry Otel SDK.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-sdk-log
  (package
    (name "go-go-opentelemetry-io-otel-sdk-log")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/log"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sb36qyq389fif9qp5iiqp6w41dfcwi95gb0bsbvznvijhd8c1cc"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            ;; Consider to implement it as re-usable procedure in
            ;; guix/build/utils or guix/build-system/go.
            (define (delete-all-but directory . preserve)
              (define (directory? x)
                (and=> (stat x #f)
                       (compose (cut eq? 'directory <>) stat:type)))
              (with-directory-excursion directory
                (let* ((pred
                        (negate (cut member <> (append '("." "..") preserve))))
                       (items (scandir "." pred)))
                  (for-each (lambda (item)
                              (if (directory? item)
                                  (delete-file-recursively item)
                                  (delete-file item)))
                            items))))
            (delete-all-but "sdk" "log")
            (delete-all-but "." "sdk")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.opentelemetry.io/otel/sdk/log"
      #:unpack-path "go.opentelemetry.io/otel"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-logr-logr
           go-github-com-go-logr-stdr
           go-github-com-google-go-cmp
           go-go-opentelemetry-io-otel-log
           go-go-opentelemetry-io-otel-sdk))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry Log SDK library")
    (description "Package log provides the @code{OpenTelemetry} Logs SDK.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-sdk-metric
  (package
    (name "go-go-opentelemetry-io-otel-sdk-metric")
    (version "1.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/metric"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sb36qyq389fif9qp5iiqp6w41dfcwi95gb0bsbvznvijhd8c1cc"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            ;; Consider to implement it as re-usable procedure in
            ;; guix/build/utils or guix/build-system/go.
            (define (delete-all-but directory . preserve)
              (define (directory? x)
                (and=> (stat x #f)
                       (compose (cut eq? 'directory <>) stat:type)))
              (with-directory-excursion directory
                (let* ((pred
                        (negate (cut member <> (append '("." "..") preserve))))
                       (items (scandir "." pred)))
                  (for-each (lambda (item)
                              (if (directory? item)
                                  (delete-file-recursively item)
                                  (delete-file item)))
                            items))))
            (delete-all-but "sdk" "metric")
            (delete-all-but "." "sdk")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.opentelemetry.io/otel/sdk/metric"
      #:unpack-path "go.opentelemetry.io/otel"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestGlobalInstRegisterCallback"
                             "TestMeterProviderDelegation")
                       "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-go-logr-logr
           go-github-com-go-logr-stdr
           go-github-com-google-go-cmp
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-sdk))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry Metric SDK library")
    (description
     "Package metric provides an implementation of the @code{OpenTelemetry}
metrics SDK.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-trace
  (package/inherit go-go-opentelemetry-io-otel
    (name "go-go-opentelemetry-io-otel-trace")
    (arguments
     (list
      #:import-path "go.opentelemetry.io/otel/trace"
      #:unpack-path "go.opentelemetry.io/otel"))
    (synopsis "OpenTelemetry Trace API")
    (description
     "Package trace provides an implementation of the tracing part of the
OpenTelemetry API.")))

(define-public go-goji-io
  (package
    (name "go-goji-io")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/goji/goji")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sckb1gayxfrlm12kdp33vwqq4gs5irqswr7pm0skkdz66swsvcc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "goji.io"))
    (home-page "https://goji.io")
    (synopsis "HTTP request multiplexer for Golang")
    (description
     "Goji is a HTTP request multiplexer, similar to std
@code{net/http.ServeMux}.  It compares incoming requests to a list of
registered Patterns, and dispatches to the @code{http.Handler} that
corresponds to the first matching Pattern.  Goji also supports
Middleware (composable shared functionality applied to every request) and uses
the standard @code{context} package to store request-scoped values.")
    (license license:expat)))

(define-public go-golang-org-x-oauth2
  (package
    (name "go-golang-org-x-oauth2")
    (version "0.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/oauth2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00r6qryh9nfnfq8q8h12hvqp0mhflhl68qyknrmzmw5ww52ghm9b"))))
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

(define-public go-google-golang-org-grpc
  (package
    (name "go-google-golang-org-grpc")
    (version "1.69.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc/grpc-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bvwnhgg04zhzwb9pxsv3n0c96hci5mdnpdaxr4ggy2m28df2q6m"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - google.golang.org/grpc/cmd/protoc-gen-go-grpc
            ;; - google.golang.org/grpc/examples
            ;; - google.golang.org/grpc/gcp/observability
            ;; - google.golang.org/grpc
            ;; - google.golang.org/grpc/interop/observability
            ;; - google.golang.org/grpc/interop/xds
            ;; - google.golang.org/grpc/security/advancedtls/examples
            ;; - google.golang.org/grpc/security/advancedtls
            ;; - google.golang.org/grpc/stats/opencensus
            ;; - google.golang.org/grpc/test/tools
            (for-each delete-file-recursively
                      (list "cmd/protoc-gen-go-grpc"
                            "examples"
                            "gcp/observability"
                            "interop/observability"
                            "interop/xds"
                            "security/advancedtls/examples"
                            "security/advancedtls"
                            "stats/opencensus"
                            "test/tools"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:tests? #f ; TODO: full test suite needs more packages
      #:import-path "google.golang.org/grpc"))
    (propagated-inputs
     (list go-github-com-cespare-xxhash-v2
           ;; go-github-com-cncf-xds-go
           ;; go-github-com-envoyproxy-go-control-plane
           ;; go-github-com-envoyproxy-go-control-plane-envoy
           go-github-com-golang-glog
           go-github-com-golang-protobuf
           go-github-com-google-go-cmp
           go-github-com-google-uuid
           ;; go-go-opentelemetry-io-contrib-detectors-gcp
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-metric
           go-go-opentelemetry-io-otel-sdk
           go-go-opentelemetry-io-otel-sdk-metric
           go-go-opentelemetry-io-otel-trace
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-google-golang-org-genproto-googleapis-rpc
           go-google-golang-org-protobuf))
    (home-page "https://google.golang.org/grpc")
    (synopsis "Golang implementation of gRPC - HTTP/2 based RPC")
    (description
     "Package grpc implements an RPC system called @code{gRPC}.")
    (license license:asl2.0)))

(define-public go-gopkg-in-go-jose-go-jose-v2
  (package
    (inherit go-github-com-go-jose-go-jose-v3)
    (name "go-gopkg-in-go-jose-go-jose-v2")
    (version "2.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-jose/go-jose")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0frg4g9gyqdgf7h0xai9771a47ndb0zqbw0rp5yk0dswsq1vk4kq"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-go-jose-go-jose-v3)
       ((#:import-path _) "gopkg.in/go-jose/go-jose.v2")))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-gopkg-in-alecthomas-kingpin-v2))))

;; This to satisfy alternative import path.
(define-public go-gopkg-in-jcmturner-rpc-v1
  (package
    (inherit go-github-com-jcmturner-rpc)
    (name "go-gopkg-in-jcmturner-rpc-v1")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-jcmturner-rpc)
       ((#:import-path _) "gopkg.in/jcmturner/rpc.v1")))))

;; This to satisfy alternative import path.
(define-public go-gopkg-in-jcmturner-rpc-v2
  (package
    (inherit go-github-com-jcmturner-rpc-v2)
    (name "go-gopkg-in-jcmturner-rpc-v2")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-jcmturner-rpc-v2)
       ((#:tests? _ #t) #f)
       ((#:import-path _) "gopkg.in/jcmturner/rpc.v2")))))

(define-public go-k8s-io-cri-api
  (package
    (name "go-k8s-io-cri-api")
    (version "0.32.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/cri-api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wzqf8860xp0k1y6csrksh37alzz3ksagwl3bv67r4x602l0zadv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/cri-api"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-gogo-protobuf
           go-google-golang-org-grpc))
    (home-page "https://github.com/kubernetes/cri-api")
    (synopsis "Container Runtime Interface a plugin interface")
    (description
     "This package provides the definitions for the Container Runtime
Interface (CRI).  CRI is a plugin interface which enables kubelet to use a
wide variety of container runtimes, without the need to recompile.  CRI
consists of a protocol buffers and @code{gRPC} API. Read more about CRI API at
@@url{https://kubernetes.io/docs/concepts/architecture/cri/,kubernetes
docs}.")
    (license license:asl2.0)))

(define-public go-k8s-io-kube-openapi
  (package
    (name "go-k8s-io-kube-openapi")
    (version "0.0.0-20241212222426-2c72e554b1e7")
    ;; XXX: Unbundle third_party in pkg.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/kube-openapi")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0crd349jd210bh68ic70pqmdkfns7cix2qhsa6pfya6kbvschyf9"))
       ;; XXX: test/integration contains submodule with it's own go.mod.
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Keeping just testdata.
            (for-each delete-file-recursively
                      (list "test/integration/builder"
                            "test/integration/builder3"
                            "test/integration/openapiconv"
                            "test/integration/pkg/generated"
                            "test/integration/testutil"
                            "test/integration/import.go"
                            "test/integration/integration_suite_test.go"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/kube-openapi"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
    (native-inputs
     (list go-github-com-getkin-kin-openapi
           go-github-com-google-gofuzz
           go-github-com-onsi-ginkgo-v2
           go-github-com-onsi-gomega
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-emicklei-go-restful-v3
           go-github-com-go-openapi-jsonreference
           go-github-com-go-openapi-swag
           go-github-com-google-gnostic-models
           go-github-com-google-go-cmp
           go-github-com-google-uuid
           go-github-com-munnerz-goautoneg
           go-github-com-nytimes-gziphandler
           go-github-com-spf13-pflag
           go-golang-org-x-tools
           go-google-golang-org-protobuf
           go-gopkg-in-yaml-v3
           go-k8s-io-gengo-v2
           go-k8s-io-klog-v2
           go-k8s-io-utils
           go-sigs-k8s-io-json
           go-sigs-k8s-io-structured-merge-diff-v4
           go-sigs-k8s-io-yaml))
    (home-page "https://github.com/kubernetes/kube-openapi")
    (synopsis "Kubernetes OpenAPI spec generation & serving")
    (description
     "This package implements a Kubernetes OpenAPI discovery spec generation,
providing support a subset of OpenAPI features to satisfy kubernetes use-cases
but implement that subset with little to no assumption about the structure of
the code or routes.")
    (license license:asl2.0)))

(define-public go-maunium-net-go-mautrix
  (package
    (name "go-maunium-net-go-mautrix")
    (version "0.22.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mautrix/go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0msqbs3qc9ljckj41hgvp16p0sbfzm25wzldb68av9svimscwnmm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "maunium.net/go/mautrix"
      #:embed-files
      #~(list
         ;; golang.org/x/net/publicsuffix/table.go:63:12: pattern
         ;; data/children: cannot embed irregular file data/children
         "children"
         ;; golang.org/x/net/publicsuffix/table.go:48:12: pattern data/nodes:
         ;; cannot embed irregular file data/nodes
         "nodes"
         ;; golang.org/x/net/publicsuffix/table.go:33:12: pattern data/text:
         ;; cannot embed irregular file data/text
         "text")
    #:test-flags
    #~(list "-skip" (string-join
                     ;; Network access is required for the tets.
                     (list "TestClient_Version"
                           "TestResolveServerName/RM_Step_3B"
                           "TestResolveServerName/RM_Step_3C"
                           "TestResolveServerName/RM_Step_3C_MSC4040"
                           "TestResolveServerName/RM_Step_3D"
                           "TestResolveServerName/RM_Step_4"
                           "TestResolveServerName/RM_Step_4_MSC4040"
                           "TestResolveServerName/maunium")
                     "|"))))
    ;; XXX: The final application needs a "libolm" package.
    (native-inputs
     (list olm))
    (propagated-inputs
     (list go-filippo-io-edwards25519
           go-github-com-chzyer-readline
           go-github-com-gorilla-mux
           go-github-com-gorilla-websocket
           go-github-com-lib-pq
           go-github-com-mattn-go-sqlite3
           go-github-com-rs-xid
           go-github-com-rs-zerolog
           go-github-com-skip2-go-qrcode
           go-github-com-stretchr-testify
           go-github-com-tidwall-gjson
           go-github-com-tidwall-sjson
           go-github-com-yuin-goldmark
           go-go-mau-fi-util
           go-go-mau-fi-zeroconfig
           go-golang-org-x-crypto
           go-golang-org-x-exp
           go-golang-org-x-net
           go-golang-org-x-sync
           go-gopkg-in-yaml-v3
           go-maunium-net-go-mauflag))
    (home-page "https://maunium.net/go/mautrix")
    (synopsis "Golang Matrix framework")
    (description
     "Package mautrix implements the Matrix Client-Server API and originated
from @url{https://github.com/matrix-org/gomatrix}.

Features:
@itemize
@item appservice support (Intent API like mautrix-python, room state storage,
etc)
@item end-to-end encryption support (incl. interactive SAS verification)
@item high-level module for building puppeting bridges
@item high-level module for building chat clients
@item wrapper functions for the Synapse admin API
@item structs for parsing event content
@item helpers for parsing and generating Matrix HTML
@item helpers for handling push rules
@end itemize")
    (license (list
              ;; This project
              license:mpl2.0
              ;; Based on <https://github.com/matrix-org/gomatrix> project, no
              ;; longer maintained since Feb 21, 2024.
              license:asl2.0))))

(define-public go-modernc-org-httpfs
  (package
    (name "go-modernc-org-httpfs")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/httpfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01q5rvhxmrd45h0ljh4185wlly7rxv6vvh28d2shsyan4nj67zf1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/httpfs"))
    (home-page "https://gitlab.com/cznic/httpfs")
    (synopsis "HTTP file system implementation in Golang")
    (description
     "Package httpfs implements @code{http.FileSystem} on top of a
@code{map[string]string}.")
    (license license:bsd-3)))

(define-public go-modernc-org-token
  (package
    (name "go-modernc-org-token")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/token")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vvnnfppmgq7hxmw18dx90fg6khwnxpwn9kwwf0hwxsckxfb5icv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/token"))
    (home-page "https://modernc.org/token")
    (synopsis "Variant of the Golang stdlib package @code{token}")
    (description
     "Package token is variant of the stdlib package token with types
@code{FileSet} and Token removed.")
    (license license:bsd-3)))

(define-public go-mvdan-cc-xurls-v2
  (package
    (name "go-mvdan-cc-xurls-v2")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mvdan/xurls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kk4mjizr23zjzsavs21krp13w52p3b8dcm4ahlrr6xkkfn8ry3i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "mvdan.cc/xurls/v2"
      #:build-flags #~(list (string-append "-ldflags=-X main.version="
                                           #$version))
      #:test-flags #~(list "-skip" "TestScript/version")))
    (propagated-inputs
     (list go-github-com-rogpeppe-go-internal-1.14
           go-golang-org-x-mod
           go-golang-org-x-sync))
    (home-page "https://github.com/mvdan/xurls")
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

(define-public lyrebird
  (package
    (name "lyrebird")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird")
             (commit (string-append "lyrebird-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hhilnk72s0h3cm7zw89n3kiqwa32c0r1a1y5ygp432hmrxvr2b0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:install-source? #f
      #:unpack-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird"
      #:import-path "gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird/cmd/lyrebird"))
    (propagated-inputs
     (list go-filippo-io-edwards25519
           go-github-com-dchest-siphash
           go-github-com-refraction-networking-utls
           go-gitlab-com-yawning-edwards25519-extra
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-snowflake-v2
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-webtunnel
           go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/lyrebird")
    (synopsis "Look-like nothing obfuscation protocol")
    (description
     "This is a look-like nothing obfuscation protocol that incorporates ideas
and concepts from Philipp Winter's ScrambleSuit protocol.")
    (license (list license:bsd-2 license:bsd-3))))

(define-public go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-lyrebird
  ;; This is a final command, no need for a full name of the go.mod module path
  ;; style. The same is suggested in project's README and Makefile.
  (deprecated-package
   "go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-lyrebird"
   lyrebird))

(define-public go-jose-util
  (package
    (name "go-jose-util")
    (version "0.0.0-20240226165647-31202557b449")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-jose/go-jose")
             (commit (go-version->git-ref version
                                          #:subdir "jose-util"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19kr2r9nnrnixpmpp37p5adnxc92bc3gzqz7rpybgaly2wfssz0q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/go-jose/go-jose/jose-util"
      #:unpack-path "github.com/go-jose/go-jose"))
    (native-inputs
     (list go-github-com-go-jose-go-jose-v3
           go-gopkg-in-alecthomas-kingpin-v2))
    (home-page "https://github.com/go-jose/go-jose")
    (synopsis "JOSE CLI")
    (description
     "The @code{jose-util} command line utility allows for encryption,
decryption,signing and verification of JOSE messages.  Its main purpose is to
facilitate dealing with JOSE messages when testing or debugging.")
    (license license:asl2.0)))

(define-public go-gojay
  (package
    (inherit go-github-com-francoispqt-gojay)
    (name "go-gojay")
    (arguments
     (list
      #:tests? #f ; already tested in the library
      #:install-source? #f
      #:import-path "github.com/francoispqt/gojay/gojay"
      #:unpack-path "github.com/francoispqt/gojay"))
    (description
     "This package provides a command line tool to generate gojay's marshaling
and unmarshaling interface implementation for custom struct type(s).")))

(define-public go-gqlclient
  (package
    (inherit go-git-sr-ht-emersion-gqlclient)
    (name "go-gqlclient")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-git-sr-ht-emersion-gqlclient)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:phases _ '%standard-phases)
        #~(modify-phases %standard-phases
            (replace 'build
              (lambda arguments
                (for-each
                 (lambda (cmd)
                   (apply (assoc-ref %standard-phases 'build)
                          `(,@arguments #:import-path ,cmd)))
                 (list "git.sr.ht/~emersion/gqlclient/cmd/gqlclient"
                       "git.sr.ht/~emersion/gqlclient/cmd/gqlclientgen"
                       "git.sr.ht/~emersion/gqlclient/cmd/gqlintrospect"))))
            (replace 'install
              (lambda arguments
                (for-each
                 (lambda (cmd)
                   (apply (assoc-ref %standard-phases 'install)
                          `(,@arguments #:import-path ,cmd)))
                 (list "git.sr.ht/~emersion/gqlclient/cmd/gqlclient"
                       "git.sr.ht/~emersion/gqlclient/cmd/gqlclientgen"
                       "git.sr.ht/~emersion/gqlclient/cmd/gqlintrospect"))))))))
    (description
     "This package provides command line tools: @code{gqlclient},
@code{gqlclientgen}, and @code{gqlintrospect}.  For the Golang libriray, see
go-git-sr-ht-emersion-gqlclient package.")))

(define-public go-html2text
  (package
    (inherit go-github-com-jaytaylor-html2text)
    (name "go-html2text")
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/jaytaylor/html2text/cmd/html2text"
      #:unpack-path "github.com/jaytaylor/html2text"))
    (native-inputs
     (list go-github-com-pborman-getopt))
    (description
     (string-append (package-description go-github-com-jaytaylor-html2text)
                    " This package provides an command line interface (CLI)
tool."))))

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
     "This package provides a CLI binary executable built from
go-github-com-multiformats-go-multiaddr-dns.")))

(define-public go-minify
  (package
    (inherit go-github-com-tdewolff-minify-v2)
    (name "go-minify")
    (arguments
     (list
      #:install-source?  #f
      #:tests? #f ; tested in the library
      #:import-path "github.com/tdewolff/minify/cmd/minify"
      #:unpack-path "github.com/tdewolff/minify"))
    (description "This package provides a CLI binary executable built from
go-github-com-tdewolff-minify-v2 source.")))

(define-public gron
  (package
    (name "gron")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tomnomnom/gron")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sj34b6yv0qigy3aq7qmwf8bqxp1a8qh9p10lzkpw58s1c0iyh36"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/tomnomnom/gron"))
    (native-inputs
     (list go-github-com-fatih-color
           go-github-com-mattn-go-colorable
           go-github-com-nwidger-jsoncolor
           go-github-com-pkg-errors))
    (home-page "https://github.com/tomnomnom/gron")
    (synopsis "Transform JSON to make it easier to grep")
    (description
     "This package transforms JSON into discrete assignments to make it easier
to use line-based tools such as grep to search for what you want and see the
absolute \"path\" to it.")
    (license license:expat)))

(define-public swag
  (package/inherit go-github-com-swaggo-swag
    (name "swag")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-swaggo-swag)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:import-path _ "github.com/swaggo/swag")
        "github.com/swaggo/swag/cmd/swag")))
    (native-inputs (package-propagated-inputs go-github-com-swaggo-swag))
    (propagated-inputs '())
    (inputs '())))

(define-public xurls
  (package/inherit go-mvdan-cc-xurls-v2
    (name "xurls")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-mvdan-cc-xurls-v2)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:import-path _ "mvdan.cc/xurls/v2") "mvdan.cc/xurls/cmd/xurls")
       ((#:unpack-path _ "") "mvdan.cc/xurls")))
    (native-inputs (package-propagated-inputs go-mvdan-cc-xurls-v2))
    (propagated-inputs '())
    (inputs '())))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
