;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-check))

;;; Commentary:
;;;
;;; Golang modules (libraries) related to HTML, CSS, SCSS, JavaScript, JSON,
;;; Web-framework, REST-API or similar functionality.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

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

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
