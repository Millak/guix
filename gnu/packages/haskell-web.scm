;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@gmail.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2022 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2022 Alice Brenon <alice.brenon@ens-lyon.fr>
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

(define-module (gnu packages haskell-web)
  #:use-module (gnu packages)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ghc-tagsoup
  (package
    (name "ghc-tagsoup")
    (version "0.14.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tagsoup" version))
       (sha256
        (base32
         "1m9sx6gr9y9yxvkmcap8xsks8cnhznvma1mrfl39zljkv005azms"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tagsoup")))
    (native-inputs
     (list ghc-quickcheck))
    (home-page "https://github.com/ndmitchell/tagsoup")
    (synopsis
     "Parsing and extracting information from (possibly malformed) HTML/XML
documents")
    (description
     "TagSoup is a library for parsing HTML/XML.  It supports the HTML 5
specification, and can be used to parse either well-formed XML, or
unstructured and malformed HTML from the web.  The library also provides
useful functions to extract information from an HTML document, making it ideal
for screen-scraping.")
    (license license:bsd-3)))

(define-public ghc-cookie
  (package
    (name "ghc-cookie")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cookie" version))
       (sha256
        (base32 "1ajbcsk4k0jc6v2fqn36scs6l8wa6fq46gd54pak75rbqdbajhcc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cookie")))
    (inputs (list ghc-data-default-class))
    (native-inputs (list ghc-hunit ghc-quickcheck ghc-tasty ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/snoyberg/cookie")
    (synopsis "HTTP cookie parsing and rendering")
    (description "HTTP cookie parsing and rendering library for Haskell.")
    (license license:expat)))

(define-public ghc-crypton-connection
  (package
    (name "ghc-crypton-connection")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypton-connection" version))
       (sha256
        (base32 "07lrkv6lwphsyp4797yp8ywnndzd270bk58r8gwyby0hr4xy52r0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypton-connection")))
    (inputs (list ghc-basement
                  ghc-data-default-class
                  ghc-network
                  ghc-tls-1.9
                  ghc-socks
                  ghc-crypton-x509
                  ghc-crypton-x509-store
                  ghc-crypton-x509-system
                  ghc-crypton-x509-validation))
    (home-page "https://github.com/kazu-yamamoto/crypton-connection")
    (synopsis "Simple and easy network connections API")
    (description
     "This package provides a simple network library for all your connection
needs.  Features: Really simple to use, SSL/TLS, SOCKS.  This library provides
a very simple API to create sockets to a destination with the choice of
SSL/TLS, and SOCKS.")
    (license license:bsd-3)))

(define-public ghc-curl
  (package
    (name "ghc-curl")
    (version "1.3.8")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "curl" version))
              (sha256
               (base32
                "0vj4hpaa30jz7c702xpsfvqaqdxz28zslsqnsfx6bf6dpwvck1wh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "curl")))
    (inputs
     (list curl))
    (home-page "https://hackage.haskell.org/package/curl")
    (synopsis "Haskell bindings for libcurl")
    (description
     "@code{libcurl} is a versatile client-side URL transfer library.
This package provides a Haskell binding to libcurl.")
    (license license:bsd-3)))

(define-public ghc-httpd-shed
  (package
    (name "ghc-httpd-shed")
    (version "0.4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "httpd-shed" version))
       (sha256
        (base32
         "19dgdimpzr7pxk7pqvyin6j87gmvnf0rm35gzhmna8qr835wy3sr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "httpd-shed")))
    (inputs
     (list ghc-network-bsd ghc-network-uri ghc-network))
    (home-page "https://hackage.haskell.org/package/httpd-shed")
    (synopsis "Simple web-server with an interact style API")
    (description
     "This web server promotes a function from @code{Request} to @code{IO
Response} into a local web server.  The user can decide how to interpret the
requests, and the library is intended for implementing Ajax APIs.")
    (license license:bsd-3)))

(define-public ghc-http-types
  (package
    (name "ghc-http-types")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "http-types" version))
       (sha256
        (base32
         "05j00b9nqmwh9zaq9y9x50k81v2pd3j7a71kd91zlnbl8xk4m2jf"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-types")))
    (native-inputs
     (list ghc-doctest ghc-hspec ghc-quickcheck ghc-quickcheck-instances
           hspec-discover))
    (inputs
     (list ghc-case-insensitive))
    (home-page "https://github.com/aristidb/http-types")
    (synopsis "Generic HTTP types for Haskell")
    (description "This package provides generic HTTP types for Haskell (for
both client and server code).")
    (license license:bsd-3)))

(define-public ghc-http
  (package
    (name "ghc-http")
    (version "4000.4.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "HTTP" version))
              (sha256
               (base32
                "0lyl5lpkk51xn3dfndh8ksgvwcdsviyigmsnp3d28lbpxkpxhcfz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "HTTP")))
    (inputs (list ghc-network ghc-network-uri))
    (native-inputs (list ghc-httpd-shed
                         ghc-hunit
                         ghc-puremd5
                         ghc-split
                         ghc-test-framework
                         ghc-test-framework-hunit))
    (arguments
     `(#:tests? #f ; Tests fail due to missing /etc/protocols?
       #:cabal-revision ("1"
                         "04y04vbxbnblpmqqmpl8km4bkcjaj96nbxkssdr1zgbhqisxay5q")))
    (home-page "https://github.com/haskell/HTTP")
    (synopsis "Library for client-side HTTP")
    (description
     "The HTTP package supports client-side web programming in Haskell.  It
lets you set up HTTP connections, transmitting requests and processing the
responses coming back.")
    (license license:bsd-3)))

(define-public ghc-http-client
  (package
    (name "ghc-http-client")
    (version "0.7.13.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "http-client" version))
              (sha256
               (base32
                "09qfmakjk285jz2rnb53c1m9c764fg8vngfq43ipga1g72h8d3n9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-client")))
    (inputs (list ghc-http-types
                  ghc-blaze-builder
                  ghc-network
                  ghc-streaming-commons
                  ghc-case-insensitive
                  ghc-base64-bytestring
                  ghc-cookie
                  ghc-random
                  ghc-mime-types
                  ghc-iproute
                  ghc-async
                  ghc-network-uri))
    (native-inputs (list ghc-hspec
                         ghc-monad-control
                         ghc-zlib
                         ghc-hspec
                         ghc-monad-control
                         ghc-zlib
                         hspec-discover))
    (arguments (list #:tests? #f)) ; Tests try to access httpbin.org.
    (home-page "https://github.com/snoyberg/http-client")
    (synopsis "HTTP client engine")
    (description
     "This package provides an HTTP client engine, intended as a base layer
for more user-friendly packages.")
    (license license:expat)))

(define-public ghc-http-client-tls
  (package
    (name "ghc-http-client-tls")
    (version "0.3.6.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "http-client-tls" version))
              (sha256
               (base32
                "03f8p9gxdzl6slyw1r6vpv2dqhsyjvbaawbjv75kaq0vlj3gz7xi"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-client-tls")))
    ;; Tests require Internet access
    (arguments `(#:tests? #f))
    (inputs
     (list ghc-data-default-class
           ghc-http-client
           ghc-connection
           ghc-network
           ghc-tls
           ghc-http-types))
    (native-inputs
     (list ghc-hspec))
    (home-page "https://github.com/snoyberg/http-client")
    (synopsis "Backend for http-client using the TLS library")
    (description
     "This package provides a backend for the http-client package using the
connection and TLS libraries.  It is intended for use by higher-level
libraries, such as http-conduit.")
    (license license:expat)))

(define-public ghc-http-client-restricted
  (package
    (name "ghc-http-client-restricted")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "http-client-restricted" version))
       (sha256
        (base32 "1vfm9qc3zr0rmq2ddgyg13i67020cdk8xqhyzfc2zcn1km2p6r85"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-client-restricted")))
    (inputs
     (list ghc-http-client
           ghc-http-client-tls
           ghc-connection
           ghc-data-default
           ghc-network
           ghc-network-bsd
           ghc-utf8-string))
    (home-page "https://hackage.haskell.org/package/http-client-restricted")
    (synopsis "Restrict the servers used by http-client")
    (description
     "This library makes it possible to restrict the HTTP servers that can be
used by the @code{http-client} and @code{http-client-tls} libraries.  This is
useful when a security policy needs to, e.g., prevent connections to HTTP
servers on localhost or only allow connections to a specific server.")
    (license license:expat)))

(define-public ghc-http-date
  (package
    (name "ghc-http-date")
    (version "0.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "http-date" version))
       (sha256
        (base32
         "1lzlrj2flcnz3k5kfhf11nk5n8m6kcya0lkwrsnzxgfr3an27y9j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-date")))
    (inputs
     (list ghc-attoparsec))
    (native-inputs
     (list ghc-doctest ghc-hspec hspec-discover ghc-old-locale))
    (home-page "https://github.com/kazu-yamamoto/http-date")
    (synopsis "HTTP Date parser/formatter")
    (description "Library for Parsing and formatting HTTP
Date in Haskell.")
    (license license:bsd-3)))

(define-public ghc-http2
  (package
    (name "ghc-http2")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "http2" version))
              (sha256
               (base32
                "1kv99i3pnnx31xndlkaczrpd2j5mvzbqlfz1kaw6cwlwkdnl5bhv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http2")))
    (inputs (list ghc-async
                  ghc-case-insensitive
                  ghc-http-types
                  ghc-network
                  ghc-network-byte-order
                  ghc-psqueues
                  ghc-time-manager
                  ghc-unix-time
                  ghc-network-run
                  ghc-cryptonite
                  ghc-aeson
                  ghc-aeson-pretty
                  ghc-base16-bytestring
                  ghc-unordered-containers
                  ghc-vector
                  ghc-word8))
    (native-inputs (list ghc-doctest
                         ghc-hspec
                         ghc-typed-process
                         ghc-hspec
                         ghc-typed-process
                         ghc-hspec
                         ghc-glob
                         ghc-hspec
                         hspec-discover))
    (home-page "https://github.com/kazu-yamamoto/http2")
    (synopsis "HTTP/2 library including frames, priority queues and HPACK")
    (description
     "This package provides a HTTP/2.0 library including frames
and HPACK.  Currently HTTP/2 16 framing and HPACK 10 is supported.")
    (license license:bsd-3)))

(define-public ghc-http-conduit
  (package
    (name "ghc-http-conduit")
    (version "2.3.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "http-conduit" version))
       (sha256
        (base32 "11zf4hyw8f1gpj0w1cmgc9g62xwy2v4hhzqazdsla4q49iqbzxgd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-conduit")))
    (inputs (list ghc-attoparsec
                  ghc-resourcet
                  ghc-conduit
                  ghc-conduit-extra
                  ghc-http-types
                  ghc-http-client
                  ghc-http-client-tls
                  ghc-unliftio-core
                  ghc-aeson))
    (native-inputs (list ghc-hunit
                         ghc-hspec
                         ghc-data-default-class
                         ghc-warp-tls
                         ghc-tls
                         ghc-blaze-builder
                         ghc-utf8-string
                         ghc-case-insensitive
                         ghc-unliftio
                         ghc-wai
                         ghc-warp
                         ghc-wai-conduit
                         ghc-cookie
                         ghc-streaming-commons
                         ghc-temporary
                         ghc-network))
    (arguments
     `(#:cabal-revision ("1"
                         "1wvr0v948s5fmlf47r4pqjan355x6v65rm7dz7y65ngj10xwk5f9")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "http-conduit.cabal"
               ((", network") ", network\n                 , connection\n")))))))
    (home-page "https://github.com/snoyberg/http-client")
    (synopsis "HTTP/HTTPS client with conduit interface")
    (description
     "This library uses attoparsec for parsing the actual
contents of the HTTP connection.  It also provides higher-level functions
which allow you to avoid direct usage of conduits.")
    (license license:bsd-3)))

(define-public ghc-http-reverse-proxy
  (package
    (name "ghc-http-reverse-proxy")
    (version "0.6.0.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "http-reverse-proxy" version))
              (sha256
               (base32
                "0a0fc9rqr1crbb1sbq3gzbkwjcfff662d4bgmy3vzspk7ky697ld"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-reverse-proxy")))
    (inputs (list ghc-case-insensitive
                  ghc-http-types
                  ghc-word8
                  ghc-blaze-builder
                  ghc-http-client
                  ghc-wai
                  ghc-network
                  ghc-conduit
                  ghc-conduit-extra
                  ghc-wai-logger
                  ghc-resourcet
                  ghc-unliftio
                  ghc-streaming-commons))
    (native-inputs (list ghc-hspec ghc-warp ghc-http-conduit))
    (home-page "https://github.com/fpco/http-reverse-proxy")
    (synopsis
     "Reverse proxy HTTP requests, either over raw sockets or with WAI")
    (description
     "Provides a simple means of reverse-proxying HTTP requests.  The raw
approach uses the same technique as leveraged by keter, whereas the WAI
approach performs full request/response parsing via WAI and http-conduit.")
    (license license:bsd-3)))

(define-public ghc-wai
  (package
    (name "ghc-wai")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wai" version))
       (sha256
        (base32
         "1y19h9v0cq1fl17ywcyyvd6419fhgyw2s0yk0ki8z60021adcx2m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wai")))
    (inputs
     (list ghc-bytestring-builder
           ghc-unix-compat
           ghc-vault
           ghc-blaze-builder
           ghc-network
           ghc-http-types))
    (native-inputs
     (list hspec-discover ghc-quickcheck ghc-hunit ghc-hspec))
    (home-page "https://hackage.haskell.org/package/wai")
    (synopsis "Web application interface for Haskell")
    (description "This package provides a Web Application Interface (WAI)
library for the Haskell language.  It defines a common protocol for
communication between web applications and web servers.")
    (license license:bsd-3)))

(define-public ghc-wai-logger
  (package
    (name "ghc-wai-logger")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wai-logger" version))
       (sha256
        (base32
         "02i9jsy5gdglqwwk5gcvax8y498lz9flrfp4v9nrv8rmrmd66zh5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wai-logger")))
    (arguments `(#:tests? #f)) ; FIXME: Tests cannot find libraries exported
                               ; by propagated-inputs.
    (inputs
     (list ghc-auto-update
           ghc-byteorder
           ghc-easy-file
           ghc-unix-time
           ghc-blaze-builder
           ghc-case-insensitive
           ghc-fast-logger
           ghc-http-types
           ghc-network
           ghc-wai))
    (home-page "https://hackage.haskell.org/package/wai-logger")
    (synopsis "Logging system for WAI")
    (description "This package provides the logging system for WAI.")
    (license license:bsd-3)))

(define-public ghc-wai-extra
  (package
    (name "ghc-wai-extra")
    (version "3.1.13.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "wai-extra" version))
              (sha256
               (base32
                "1h4cqd5akrq0vhv3l0fzryy7qw0c2jb58lngx7x8ij63bckjs3fz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wai-extra")))
    (inputs (list ghc-aeson
                  ghc-ansi-terminal
                  ghc-base64-bytestring
                  ghc-call-stack
                  ghc-case-insensitive
                  ghc-cookie
                  ghc-data-default-class
                  ghc-fast-logger
                  ghc-http-types
                  ghc-hunit
                  ghc-iproute
                  ghc-network
                  ghc-resourcet
                  ghc-streaming-commons
                  ghc-vault
                  ghc-wai
                  ghc-wai-logger
                  ghc-warp
                  ghc-word8))
    (native-inputs (list ghc-hspec ghc-temporary ghc-zlib hspec-discover))
    (arguments
     `(#:cabal-revision ("1"
                         "0dyvg2bb37im790757khncxpnf45451dd8575p736ry4g8rpqgpw")))
    (home-page "http://github.com/yesodweb/wai")
    (synopsis "Some basic WAI handlers and middleware")
    (description "This library provides basic WAI handlers and middleware
functionality.")
    (license license:expat)))

(define-public ghc-wai-conduit
  (package
    (name "ghc-wai-conduit")
    (version "3.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wai-conduit" version))
       (sha256
        (base32
         "07yn41rn2skd5p3wqqa09wa761vj7ibl8l19gh4bi4i8slxhk417"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wai-conduit")))
    (inputs
     (list ghc-conduit ghc-http-types ghc-wai ghc-blaze-builder))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "Conduit wrappers for Haskell's WAI")
    (description "This package provides data streaming abstraction for
Haskell's Web Application Interface (WAI).")
    (license license:expat)))

(define-public ghc-bsb-http-chunked
  (package
    (name "ghc-bsb-http-chunked")
    (version "0.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bsb-http-chunked" version))
       (sha256
        (base32
         "0z0f18yc6zlwh29c6175ivfcin325lvi4irpvv0n3cmq7vi0k0ql"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bsb-http-chunked")))
    (arguments
     `(#:tests? #f ; Tests fail: Variable not in scope.
       #:cabal-revision
       ("3" "15hg352id2f4x0dnvv47bdiz6gv5hp5a2mki9yzmhc7ajpk31mdd")))
    (native-inputs
     (list ghc-attoparsec
           ghc-blaze-builder
           ghc-hedgehog
           ghc-tasty
           ghc-tasty-hedgehog
           ghc-tasty-hunit
           ghc-doctest))
    (home-page "https://github.com/sjakobi/bsb-http-chunked")
    (synopsis "Chunked HTTP transfer encoding for bytestring builders")
    (description "This Haskell library contains functions for encoding
bytestring builders for chunked Hypertext Transfer Protocol (HTTP) 1.1
transfers.")
    (license license:bsd-3)))

(define-public ghc-warp
  (package
    (name "ghc-warp")
    (version "3.3.23")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "warp" version))
       (sha256
        (base32 "0y1r7czq5zrgklqrx1b9pmxn5lhmf7zpqdjz7hfmnzsmr3vndmms"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "warp")))
    (inputs
     (list ghc-async
           ghc-auto-update
           ghc-bsb-http-chunked
           ghc-case-insensitive
           ghc-hashable
           ghc-http-types
           ghc-iproute
           ghc-network
           ghc-streaming-commons
           ghc-time-manager
           ghc-unix-compat
           ghc-vault
           ghc-wai
           ghc-word8
           ghc-http-date
           ghc-simple-sendfile
           ghc-unliftio
           ghc-x509
           ghc-http2
           ghc-recv))
    (native-inputs
     (list curl
           ghc-silently
           ghc-hspec
           ghc-doctest
           ghc-lifted-base
           ghc-quickcheck
           ghc-hunit
           ghc-http-client
           hspec-discover))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "HTTP server library for Haskell's WAI")
    (description "Warp is a server library for HTTP/1.x and HTTP/2
based WAI (Web Application Interface in Haskell).")
    (license license:expat)))

(define-public ghc-tls-session-manager
  (package
  (name "ghc-tls-session-manager")
  (version "0.0.4")
  (source
    (origin
      (method url-fetch)
      (uri (hackage-uri "tls-session-manager" version))
      (sha256
        (base32
          "134kb5nz668f4xrr5g98g7fc1bwb3ri6q433a1i6asjkniwpy85s"))))
  (build-system haskell-build-system)
  (properties '((upstream-name . "tls-session-manager")))
  (inputs
    (list ghc-auto-update ghc-clock ghc-psqueues ghc-tls))
  (home-page "https://hackage.haskell.org/package/tls-session-manager")
  (synopsis "In-memory TLS session manager")
  (description "This Haskell library provides a TLS session manager with
limitation, automatic pruning, energy saving and replay resistance.")
  (license license:bsd-3)))

(define-public ghc-warp-tls
  (package
    (name "ghc-warp-tls")
    (version "3.3.4")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "warp-tls" version))
              (sha256
               (base32
                "00vgs9v7k0fapl05knqii9g47svf4lapb7ixkll7xr4zvmkk0r0m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "warp-tls")))
    (inputs (list ghc-wai
                  ghc-warp
                  ghc-data-default-class
                  ghc-tls
                  ghc-cryptonite
                  ghc-network
                  ghc-streaming-commons
                  ghc-tls-session-manager
                  ghc-unliftio))
    (home-page "http://github.com/yesodweb/wai")
    (synopsis "SSL/TLS support for Warp")
    (description "This package provides SSL/TLS support for Warp,
a WAI handler, via the native Haskell TLS implementation.")
    (license license:expat)))

(define-public ghc-websockets
  (package
    (name "ghc-websockets")
    (version "0.12.7.3")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "websockets" version))
              (sha256
               (base32
                "0g3z0n4irf3gvbdf9p97jq05ybdg0gwjy5bj4nfc7ivsvyhaic6k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "websockets")))
    (inputs (list ghc-async
                  ghc-attoparsec
                  ghc-base64-bytestring
                  ghc-bytestring-builder
                  ghc-case-insensitive
                  ghc-clock
                  ghc-network
                  ghc-random
                  ghc-sha
                  ghc-streaming-commons
                  ghc-entropy))
    (native-inputs (list ghc-hunit ghc-quickcheck ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2))
    (arguments
     `(#:cabal-revision ("1"
                         "1yx97y6jl74vy200y43vjxfyzx338kh10dx8vxkjhr0mfh36wldq")))
    (home-page "http://jaspervdj.be/websockets")
    (synopsis "Write WebSocket-capable servers in Haskell")
    (description
     "This library allows you to write WebSocket-capable servers.

An example server:
@url{https://github.com/jaspervdj/websockets/blob/master/example/server.lhs}
An example client:
@url{https://github.com/jaspervdj/websockets/blob/master/example/client.hs}

See also:
@itemize
@item The specification of the WebSocket protocol:
@url{http://www.whatwg.org/specs/web-socket-protocol/}
@item The JavaScript API for dealing with WebSockets:
@url{http://www.w3.org/TR/websockets/}
@end itemize")
    (license license:bsd-3)))

(define-public ghc-wai-websockets
  (package
    (name "ghc-wai-websockets")
    (version "3.0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wai-websockets" version))
       (sha256
        (base32
         "0b2xmdsrsqpssyib53wbr6r8hf75789ndyyanv37sv99iyqcwz4i"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wai-websockets")))
    (inputs
     (list ghc-wai ghc-case-insensitive ghc-network ghc-websockets
           ghc-http-types))
    (arguments
     `(#:configure-flags '("--flags=-example")))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis
     "Provide a bridge between WAI and the websockets package")
    (description
     "Use websockets with WAI applications, primarily those hosted via Warp.")
    (license license:expat)))

(define-public ghc-xss-sanitize
  (package
    (name "ghc-xss-sanitize")
    (version "0.3.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "xss-sanitize" version))
       (sha256
        (base32 "0in9kn51i2ddh5c8scyf9l8zi6zxidwznn34qwj02nglw5dpzfqv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xss-sanitize")))
    (inputs (list ghc-attoparsec ghc-css-text ghc-network-uri ghc-tagsoup
                  ghc-utf8-string))
    (native-inputs (list ghc-hunit ghc-hspec))
    (arguments
     `(#:cabal-revision ("1"
                         "1l8y52nja9a2iyxawm3vp23jcs46ziwx0yj2w46drb7knaa306d0")))
    (home-page "https://github.com/yesodweb/haskell-xss-sanitize#readme")
    (synopsis "Sanitize untrusted HTML to prevent XSS attacks")
    (description
     "This library provides @code{sanitizeXSS}.  Run untrusted
HTML through @code{Text.HTML.SanitizeXSS.sanitizeXSS} to prevent XSS
attacks.")
    (license license:bsd-2)))

(define-public ghc-css-text
  (package
    (name "ghc-css-text")
    (version "0.1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "css-text" version))
       (sha256
        (base32
         "0ynd9f4hn2sfwqzbsa0y7phmxq8za7jiblpjwx0ry8b372zhgxaz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "css-text")))
    (inputs
     (list ghc-attoparsec ghc-hspec ghc-quickcheck))
    (home-page "https://www.yesodweb.com/")
    (synopsis "CSS parser and renderer")
    (description "This package provides a CSS parser and renderer for
Haskell.")
    (license license:bsd-3)))

(define-public ghc-mattermost-api
  (package
    (name "ghc-mattermost-api")
    (version "90000.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mattermost-api" version))
       (sha256
        (base32 "1ka3r4bnfwlbjnkws8vkg8i9gj8wzsyss137p7hxrx4sr75s6iyv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mattermost-api")))
    (inputs (list ghc-websockets
                  ghc-aeson
                  ghc-crypton-connection
                  ghc-memory
                  ghc-resource-pool
                  ghc-http
                  ghc-http-media
                  ghc-network-uri
                  ghc-modern-uri
                  ghc-unordered-containers
                  ghc-hashable
                  ghc-gitrev
                  ghc-microlens
                  ghc-microlens-th
                  ghc-pretty-show
                  ghc-split
                  ghc-connection))
    (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-hunit))
    (arguments
     (list #:tests? #f)) ;tests require networking and Mattermost Docker image
    (home-page "https://hackage.haskell.org/package/mattermost-api")
    (synopsis "Client API for Mattermost chat system")
    (description
     "This package implements the client API for the Mattermost chat system.
Mattermost is a flexible messaging platform.  This library provides network
API interaction with the Mattermost server.")
    (license license:bsd-3)))

(define-public ghc-mattermost-api-qc
  (package
    (name "ghc-mattermost-api-qc")
    (version "90000.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "mattermost-api-qc" version))
       (sha256
        (base32 "0lrb8l8nbrdp4y2ala8hchr8ikv5hqw710ffiiw1sz6z2dqiqbxm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mattermost-api-qc")))
    (inputs (list ghc-mattermost-api ghc-quickcheck))
    (home-page "https://github.com/matterhorn-chat/mattermost-api-qc")
    (synopsis "QuickCheck instances for the Mattermost client API library")
    (description
     "This package provides a library providing @code{QuickCheck} for the
mattermost-api library to allow testing.  This is provided as a separate
library to allow use of the API library without testing dependencies.")
    (license license:isc)))

(define-public ghc-mime-types
  (package
    (name "ghc-mime-types")
    (version "0.1.0.9")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "mime-types" version))
              (sha256
               (base32
                "1lkipa4v73z3l5lqs6sdhl898iq41kyxv2jb9agsajzgd58l6cha"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "mime-types")))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "Basic MIME type handling types and functions")
    (description
     "This library provides basic MIME type handling types and functions.")
    (license license:expat)))

(define-public ghc-modern-uri
  (package
    (name "ghc-modern-uri")
    (version "0.3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "modern-uri" version))
       (sha256
        (base32 "1sag8l91qd7xs56rlx8r6dz9zxxmqsnfw0v47az7l8nirv7zjih2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "modern-uri")))
    (inputs (list ghc-quickcheck
                  ghc-hashable
                  ghc-megaparsec
                  ghc-profunctors
                  ghc-reflection
                  ghc-tagged))
    (native-inputs (list ghc-hspec
                         ghc-hspec-megaparsec
                         ghc-hspec-discover))
    (arguments
     `(#:cabal-revision ("1"
                         "0c0mr8aqs963nmy7i8yfih24snaijgwkxim2q2khw12capshac0q")))
    (home-page "https://github.com/mrkkrp/modern-uri")
    (synopsis "Library for working with URIs")
    (description "This is a library for working with URIs in Haskell as
per RFC 3986.")
    (license license:bsd-3)))

(define-public ghc-html
  (package
    (name "ghc-html")
    (version "1.0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "html" version))
       (sha256
        (base32
         "0q9hmfii62kc82ijlg238fxrzxhsivn42x5wd6ffcr9xldg4jd8c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "html")))
    (home-page
     "https://hackage.haskell.org/package/html")
    (synopsis "HTML combinator library")
    (description
     "This package contains a combinator library for constructing HTML
documents.")
    (license license:bsd-3)))

(define-public ghc-html-conduit
  (package
    (name "ghc-html-conduit")
    (version "1.3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "html-conduit" version))
       (sha256
        (base32
         "09bwrdam3y47kqllgg6w098ghqb8jb10dp4wxirsvx5ddpx9zpi6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "html-conduit")))
    (inputs
     (list ghc-resourcet
           ghc-conduit
           ghc-xml-conduit
           ghc-xml-types
           ghc-attoparsec
           ghc-conduit-extra))
    (native-inputs
     (list ghc-hspec ghc-hunit))
    (home-page "https://github.com/snoyberg/xml")
    (synopsis "Parse HTML documents using xml-conduit datatypes")
    (description
     "This package provides a parser for HTML documents that uses
tagstream-conduit.  It automatically balances mismatched tags, so that
there shouldn't be any parse failures.  It does not handle a full HTML
document rendering, such as adding missing html and head tags.  Note that,
since version 1.3.1, it uses an inlined copy of tagstream-conduit with
entity decoding bugfixes applied.")
    (license license:expat)))

(define-public ghc-blaze-html
  (package
    (name "ghc-blaze-html")
    (version "0.9.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "blaze-html" version))
       (sha256
        (base32
         "0k1r1hddjgqighazcazxrx6xfhvy2gm8il8l82ainv3cai13yl30"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "blaze-html")))
    (arguments
     `(#:tests? #f ; TODO: Depends on quickcheck<2.14
       #:cabal-revision
       ("1" "0wvlfb3rd9cm3p894p5rl9kggrsr5da3n8x9ydrbagx91yvkxns9")))
    (inputs
     (list ghc-blaze-builder ghc-blaze-markup))
    (native-inputs
     (list ghc-hunit ghc-quickcheck ghc-test-framework
           ghc-test-framework-hunit ghc-test-framework-quickcheck2))
    (home-page "https://jaspervdj.be/blaze")
    (synopsis "Fast HTML combinator library")
    (description "This library provides HTML combinators for Haskell.")
    (license license:bsd-3)))

(define-public ghc-aeson
  (package
    (name "ghc-aeson")
    (version "2.0.3.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "aeson" version))
              (sha256
               (base32
                "09dk0j33n262dm75vff3y3i9fm6lh06dyqswwv7a6kvnhhmhlxhr"))
              (patches (search-patches "ghc-aeson-encodeDouble.patch"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "aeson")))
    (inputs (list ghc-base-compat-batteries
                  ghc-time-compat
                  ghc-attoparsec
                  ghc-data-fix
                  ghc-dlist
                  ghc-hashable
                  ghc-indexed-traversable
                  ghc-onetuple
                  ghc-primitive
                  ghc-quickcheck
                  ghc-scientific
                  ghc-semialign
                  ghc-strict
                  ghc-tagged
                  ghc-text-short
                  ghc-th-abstraction
                  ghc-these
                  ghc-unordered-containers
                  ghc-uuid-types
                  ghc-vector
                  ghc-witherable))
    (native-inputs (list ghc-base-compat
                         ghc-base-orphans
                         ghc-base16-bytestring
                         ghc-diff
                         ghc-generic-deriving
                         ghc-integer-logarithms
                         ghc-quickcheck-instances
                         ghc-tasty
                         ghc-tasty-golden
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1zrgn63jzrpk3n3vd44zkzgw7kb5qxlvhx4nk6g3sswwrsz5j32i")))
    (home-page "https://github.com/haskell/aeson")
    (synopsis "Fast JSON parsing and encoding")
    (description
     "This package provides a JSON parsing and encoding library
for Haskell, optimized for ease of use and high performance.  (A note on
naming: in Greek mythology, Aeson was the father of Jason.)")
    (license license:bsd-3)))

(define-public ghc-aeson-pretty
  (package
    (name "ghc-aeson-pretty")
    (version "0.8.9")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "aeson-pretty" version))
              (sha256
               (base32
                "021az9az6xik9c9s3rnar5fr1lgy2h3igibf5ixnc7ps3m2lzg2x"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "aeson-pretty")))
    (inputs
     (list ghc-aeson
           ghc-base-compat
           ghc-scientific
           ghc-vector
           ghc-unordered-containers
           ghc-attoparsec
           ghc-cmdargs))
    (home-page "https://github.com/informatikr/aeson-pretty")
    (synopsis "JSON pretty-printing library and command-line tool")
    (description
     "This package provides a JSON pretty-printing library compatible with aeson
as well as a command-line tool to improve readability of streams of JSON data.
The library provides the function @code{encodePretty}.  It is a drop-in
replacement for aeson's @code{encode} function, producing JSON-ByteStrings for
human readers.  The command-line tool reads JSON from stdin and writes
prettified JSON to stdout.  It also offers a complementary \"compact\"-mode,
essentially the opposite of pretty-printing.")
    (license license:bsd-3)))

(define-public ghc-aeson-qq
  (package
    (name "ghc-aeson-qq")
    (version "0.8.4")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "aeson-qq" version))
              (sha256
               (base32
                "0dpklq2xdhrkg1rdc7zfdjnzm6c3qxx2i1xskrqdxpqi84ffnlyh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "aeson-qq")))
    (inputs
     (list ghc-base-compat
           ghc-attoparsec
           ghc-scientific
           ghc-vector
           ghc-aeson
           ghc-haskell-src-meta))
    (native-inputs
     (list ghc-hspec hspec-discover))
    (home-page "https://github.com/zalora/aeson-qq")
    (synopsis "JSON quasiquoter for Haskell")
    (description
     "aeson-qq provides a JSON quasiquoter for Haskell.  This package exposes
the function @code{aesonQQ} that compile-time converts a string representation
of a JSON value into a @code{Data.Aeson.Value}.")
    (license license:expat)))

(define-public ghc-aeson-better-errors
  (package
    (name "ghc-aeson-better-errors")
    (version "0.9.1.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "aeson-better-errors" version))
              (sha256
               (base32
                "05yibq9kqbjb8rh84n12sbax05amvd8jccpja0hyadz58pjy4jnk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "aeson-better-errors")))
    (inputs (list ghc-aeson
                  ghc-unordered-containers
                  ghc-dlist
                  ghc-scientific
                  ghc-vector
                  ghc-transformers-compat
                  ghc-void))
    (arguments
     `(#:cabal-revision ("1"
                         "0wzvrmhn5q2x1mcv43cyxhlck815ldkhx7c7gz5ijjyva1iicgn2")))
    (home-page "https://github.com/hdgarrood/aeson-better-errors")
    (synopsis "Better error messages when decoding JSON values in Haskell")
    (description
     "Gives you the tools to build parsers to decode JSON values, and gives
good error messages when parsing fails.  See also
@url{http://harry.garrood.me/blog/aeson-better-errors/}.")
    (license license:expat)))

(define-public ghc-multipart
  (package
    (name "ghc-multipart")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "multipart" version))
       (sha256
        (base32
         "0p6n4knxpjv70nbl6cmd6x7gkdjsjqp4ya7fz00bfrqp7jvhlivn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "multipart")))
    (inputs
     (list ghc-stringsearch))
    (arguments
     `(#:cabal-revision ("1"
                         "03gaapwnvn843hpm5qwdci9df1wf383msd42p8w9ghilpfjj4qy9")))
    (home-page
     "http://www.github.com/silkapp/multipart")
    (synopsis
     "HTTP multipart library")
    (description
     "HTTP multipart split out of the cgi package, for Haskell.")
    (license license:bsd-3)))

(define-public ghc-uri-encode
  (package
    (name "ghc-uri-encode")
    (version "1.5.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "uri-encode" version))
       (sha256
        (base32
         "0lj2h701af12539p957rw24bxr07mfqd5r4h52i42f43ax165767"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "uri-encode")))
    (inputs
     (list ghc-utf8-string ghc-network-uri))
    (arguments
     `(#:cabal-revision ("2"
                         "03pmvbi56gmg1z2wr3glncc7dbyh666bqp565inh31qzsp9zwmgj")))
    (home-page "https://hackage.haskell.org/package/uri-encode")
    (synopsis "Unicode aware uri-encoding")
    (description "Unicode aware uri-encoding for Haskell.")
    (license license:bsd-3)))

(define-public ghc-path-pieces
  (package
    (name "ghc-path-pieces")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "path-pieces" version))
       (sha256
        (base32
         "0vx3sivcsld76058925hym2j6hm3g71f0qjr7v59f1g2afgx82q8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "path-pieces")))
    (native-inputs (list ghc-hunit ghc-hspec ghc-quickcheck))
    (home-page "https://github.com/yesodweb/path-pieces")
    (synopsis "Used in Yesod to automatically marshall data in the request path")
    (description  "This Haskell package provides two typeclasses for converting
Haskell data types to and from route pieces.")
    (license license:bsd-3)))

(define-public ghc-skein
  (package
    (name "ghc-skein")
    (version "1.0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "skein" version))
       (sha256
        (base32
         "1jdqdk0rz2wnvw735clnj8jh0a9rkrbqjg7vk3w6wczdql6cm0pq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "skein")))
    (inputs (list ghc-cereal ghc-tagged ghc-crypto-api))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/yesodweb/path-pieces")
    (synopsis "Skein family of cryptographic hash functions for Haskell")
    (description "@uref{(http://www.skein-hash.info, Skein} is a family of
fast secure cryptographic hash functions designed by Niels Ferguson, Stefan
Lucks, Bruce Schneier, Doug Whiting, Mihir Bellare, Tadayoshi Kohno, Jon
Callas and Jesse Walker.

This Haskell package uses bindings to the optimized C implementation of Skein.")
    (license license:bsd-3)))

(define-public ghc-clientsession
  (package
    (name "ghc-clientsession")
    (version "0.9.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "clientsession" version))
       (sha256
        (base32
         "0s6h4ykj16mpf7nlw2iqn2ji0p8g1fn5ni0s7yqaili6vv2as5ar"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "clientsession")))
    (inputs (list ghc-cereal
                  ghc-tagged
                  ghc-crypto-api
                  ghc-skein
                  ghc-base64-bytestring
                  ghc-entropy
                  ghc-cprng-aes
                  ghc-cipher-aes
                  ghc-crypto-random
                  ghc-setenv))
    (native-inputs (list ghc-hunit ghc-hspec ghc-quickcheck))
    (home-page "https://github.com/yesodweb/clientsession/tree/master")
    (synopsis "Haskell library for securely store session data in a
client-side cookie")
    (description "This Haskell package achieves security through AES-CTR
encryption and Skein-MAC-512-256 authentication.  Uses Base64 encoding to
avoid any issues with characters.")
    (license license:expat)))

(define-public ghc-yesod-core
  (package
    (name "ghc-yesod-core")
    (version "1.6.24.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "yesod-core" version))
       (sha256
        (base32 "0cql4gk83ya0lyv0nyrp387nljpab4dwwy288rzp8klq9z5r2a7j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "yesod-core")))
    (inputs (list ghc-aeson
                  ghc-auto-update
                  ghc-blaze-html
                  ghc-blaze-markup
                  ghc-case-insensitive
                  ghc-cereal
                  ghc-clientsession
                  ghc-conduit
                  ghc-conduit-extra
                  ghc-cookie
                  ghc-entropy
                  ghc-fast-logger
                  ghc-http-types
                  ghc-memory
                  ghc-monad-logger
                  ghc-path-pieces
                  ghc-primitive
                  ghc-random
                  ghc-resourcet
                  ghc-shakespeare
                  ghc-unix-compat
                  ghc-unliftio
                  ghc-unordered-containers
                  ghc-vector
                  ghc-wai
                  ghc-wai-extra
                  ghc-wai-logger
                  ghc-warp
                  ghc-word8))
    (native-inputs (list ghc-hspec
                         ghc-hunit
                         ghc-async
                         ghc-hspec
                         ghc-hspec-expectations
                         ghc-network
                         ghc-streaming-commons))
    (home-page "http://www.yesodweb.com/")
    (synopsis "Core package for the Yesod web framework")
    (description
     "This Haskell package provides all core functionality, for
Yesod, on which other packages can be built.  It provides dispatch, handler
functions, widgets, etc.")
    (license license:expat)))

(define-public ghc-yesod-persistent
  (package
    (name "ghc-yesod-persistent")
    (version "1.6.0.8")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "yesod-persistent" version))
              (sha256
               (base32
                "02vm0qm0yxqn6x61iir81wf6ibwnf8gkia8lw71fgpxgav154ig6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "yesod-persistent")))
    (inputs (list ghc-yesod-core
                  ghc-persistent
                  ghc-persistent-template
                  ghc-blaze-builder
                  ghc-conduit
                  ghc-resourcet
                  ghc-resource-pool))
    (native-inputs (list ghc-hspec ghc-wai-extra ghc-persistent-sqlite
                         hspec-discover))
    (home-page "http://www.yesodweb.com/")
    (synopsis "Helpers for using Persistent from Yesod")
    (description "This Haskell package provides helpers for using Persistent
from Yesod.")
    (license license:expat)))

(define-public ghc-yesod-form
    (package
      (name "ghc-yesod-form")
      (version "1.7.4")
      (source
       (origin
         (method url-fetch)
         (uri (hackage-uri "yesod-form" version))
         (sha256
          (base32 "012w6pq0zznwqn19nx5h30rmd7dazcd0d75a6426d7brxvf9vn98"))))
      (build-system haskell-build-system)
      (properties '((upstream-name . "yesod-form")))
      (inputs (list ghc-aeson
                    ghc-attoparsec
                    ghc-blaze-builder
                    ghc-blaze-html
                    ghc-blaze-markup
                    ghc-byteable
                    ghc-data-default
                    ghc-email-validate
                    ghc-persistent
                    ghc-resourcet
                    ghc-shakespeare
                    ghc-wai
                    ghc-xss-sanitize
                    ghc-yesod-core
                    ghc-yesod-persistent
                    ghc-network-uri))
      (native-inputs (list ghc-hspec))
      (home-page "http://www.yesodweb.com/")
      (synopsis "Form handling support for Yesod Web Framework")
      (description
       "This Haskell package provides a set of basic form inputs such
as text, number, time, checkbox, select, textarea, etc through the
@code{Yesod.Form.Fields} module.  Also, there is @code{Yesod.Form.Nic} module
providing richtext field using Nic editor.")
      (license license:expat)))

(define-public ghc-yesod
  (package
    (name "ghc-yesod")
    (version "1.6.2.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "yesod" version))
              (sha256
               (base32
                "1qglaxqx96c7wi4817ff67c9g2fxlnjzdpgic458i80khpdlmb5c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "yesod")))
    (inputs (list ghc-aeson
                  ghc-conduit
                  ghc-data-default-class
                  ghc-fast-logger
                  ghc-file-embed
                  ghc-monad-logger
                  ghc-shakespeare
                  ghc-streaming-commons
                  ghc-unordered-containers
                  ghc-wai
                  ghc-wai-extra
                  ghc-wai-logger
                  ghc-warp
                  ghc-yaml
                  ghc-yesod-core
                  ghc-yesod-form
                  ghc-yesod-persistent))
    (home-page "http://www.yesodweb.com/")
    (synopsis "Framework for creating type-safe, RESTful web applications")
    (description
     "The Haskell package package groups together the various
Yesod related packages into one cohesive whole.  This is the version of Yesod,
whereas most of the core code lives in @code{ghc-yesod-core}.")
    (license license:expat)))

(define-public ghc-hxt-charproperties
  (package
    (name "ghc-hxt-charproperties")
    (version "9.5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hxt-charproperties" version))
       (sha256
        (base32
         "0jm98jddbsd60jc2bz8wa71rslagbaqf00ia7fvfsaiaa54nk0r8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hxt-charproperties")))
    (home-page "https://github.com/UweSchmidt/hxt")
    (synopsis "Character properties and classes for XML and Unicode")
    (description
     "The modules provided by this package contain predicates for Unicode
blocks and char properties and character predicates defined by XML.  The
supported Unicode version is 7.0.0")
    (license license:expat)))

(define-public ghc-hxt-unicode
  (package
    (name "ghc-hxt-unicode")
    (version "9.0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hxt-unicode" version))
       (sha256
        (base32
         "0rj48cy8z4fl3zpg5bpa458kqr83adav6jnqv4i71dclpprj6n3v"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hxt-unicode")))
    (inputs
     (list ghc-hxt-charproperties))
    (home-page
     "http://www.fh-wedel.de/~si/HXmlToolbox/index.html https://github.com/UweSchmidt/hxt")
    (synopsis
     "Unicode en-/decoding functions for utf8, iso-latin-* and other encodings")
    (description
     "This package provides Unicode encoding and decoding functions for
encodings used in the Haskell XML Toolbox.  ISO Latin 1-16, utf8, utf16, ASCII
are supported. Decoding is done with lazy functions, errors may be detected or
ignored.")
    (license license:expat)))

(define-public ghc-hxt-regex-xmlschema
  (package
    (name "ghc-hxt-regex-xmlschema")
    (version "9.2.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hxt-regex-xmlschema" version))
       (sha256
        (base32
         "0ynrf65m7abq2fjnarlwq6i1r99pl89npibxx05rlplcgpybrdmr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hxt-regex-xmlschema")))
    (inputs
     (list ghc-hxt-charproperties ghc-hunit))
    (home-page "https://wiki.haskell.org/Regular_expressions_for_XML_Schema")
    (synopsis "Regular expression library for W3C XML Schema regular expressions")
    (description
     "This library supports full W3C XML Schema regular expressions inclusive
all Unicode character sets and blocks.  It is implemented by the technique of
derivations of regular expressions.")
    (license license:expat)))

(define-public ghc-hxt
  (package
    (name "ghc-hxt")
    (version "9.3.1.22")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hxt" version))
       (sha256
        (base32
         "1n9snbdl46x23ka7bbsls1vsn0plpmfmbpbl0msjfm92fkk2yq7g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hxt")))
    (outputs '("out" "static" "doc"))
    (inputs
     (list ghc-hxt-charproperties ghc-hxt-unicode ghc-hxt-regex-xmlschema
           ghc-network-uri))
    (home-page "https://github.com/UweSchmidt/hxt")
    (synopsis "Collection of tools for processing XML with Haskell")
    (description
     "The Haskell XML Toolbox bases on the ideas of HaXml and HXML, but
introduces a more general approach for processing XML with Haskell.")
    (license license:expat)))

(define-public ghc-hxt-xpath
  (package
    (name "ghc-hxt-xpath")
    (version "9.1.2.2")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hxt-xpath" version))
              (sha256
               (base32
                "0wlq9s01icalnvjkkilx5zaqp3ff4v5limj1xy8i18qpzjspqdsh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hxt-xpath")))
    (inputs (list ghc-hxt))
    (home-page "https://github.com/UweSchmidt/hxt")
    (synopsis "The XPath modules for HXT")
    (description
     "This extension for the Haskell XML Toolbox defines data types to
represent XPath, navigation trees and primitives to select and edit subtrees
from them.  Some primitives have both a functional and an arrow interface.")
    (license license:expat)))

(define-public ghc-http-common
  (package
    (name "ghc-http-common")
    (version "0.8.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "http-common" version))
       (sha256
        (base32
         "1xpbnfac0fqa5r670ggwm4kq3cmz9jpaw9bx40j9w9qiw6xi4i28"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-common")))
    (inputs
     (list ghc-base64-bytestring
           ghc-blaze-builder
           ghc-case-insensitive
           ghc-network
           ghc-random
           ghc-unordered-containers))
    (home-page "https://github.com/afcowie/http-streams/")
    (synopsis "Common types for HTTP clients and servers")
    (description "Base types used by a variety of HTTP clients and
servers.  See http-streams @code{Network.Http.Client} or pipes-http
@code{Pipes.Http.Client} for full documentation.  You can import
@code{Network.Http.Types} if you like, but both http-streams and
pipes-http re-export this package's types and functions.")
    (license license:bsd-3)))

(define-public ghc-http-streams
  (package
    (name "ghc-http-streams")
    (version "0.8.9.6")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "http-streams" version))
              (sha256
               (base32
                "1h8nnp1y4ngv6mwr3fxv428kcvrd3ming179sza8fkn49pcwdlxs"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "http-streams")))
    (inputs (list ghc-attoparsec
                  ghc-base64-bytestring
                  ghc-blaze-builder
                  ghc-case-insensitive
                  ghc-io-streams
                  ghc-hsopenssl
                  ghc-openssl-streams
                  ghc-unordered-containers
                  ghc-aeson
                  ghc-http-common
                  ghc-network-uri
                  ghc-network))
    (native-inputs (list ghc-hunit
                         ghc-lifted-base
                         ghc-aeson-pretty
                         ghc-hspec
                         ghc-hspec-expectations
                         ghc-random
                         ghc-snap-core
                         ghc-snap-server))
    (home-page "https://github.com/aesiniath/http-streams/")
    (synopsis "HTTP client using io-streams")
    (description
     "An HTTP client using the Snap Framework's io-streams
library to handle the streaming IO.  The API is optimized for ease of
use for the rather common case of code needing to query web services and
deal with the result.")
    (license license:bsd-3)))

;; Breaks cycle between ghc-http-streams and ghc-snap-server
(define-public ghc-http-streams-bootstrap
  (package
    (inherit ghc-http-streams)
    (name "ghc-http-streams-bootstrap")
    (arguments `(#:tests? #f))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-snap-core
  (package
    (name "ghc-snap-core")
    (version "1.0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "snap-core" version))
       (sha256
        (base32
         "0hf671g7h4nikfvi05q3mmcxhfcsh874dkansssn0mc68k9fsak4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "snap-core")))
    (arguments
     `(#:cabal-revision
       ("3" "02r6plphl4vqig3xap9amdib0qjd98nqpn5jhy6hsbiwh3p7cy9b")))
    (inputs
     (list ghc-old-locale
           ghc-hunit
           ghc-attoparsec
           ghc-bytestring-builder
           ghc-case-insensitive
           ghc-lifted-base
           ghc-io-streams
           ghc-hashable
           ghc-monad-control
           ghc-random
           ghc-readable
           ghc-regex-posix
           ghc-transformers-base
           ghc-unix-compat
           ghc-unordered-containers
           ghc-vector
           ghc-network-uri
           ghc-network))
    (native-inputs
     (list ghc-quickcheck
           ghc-parallel
           ghc-test-framework
           ghc-test-framework-hunit
           ghc-test-framework-quickcheck2
           ghc-zlib))
    (home-page "http://snapframework.com/")
    (synopsis "Haskell Web Framework (core interfaces and types)")
    (description "Snap is a simple and fast web development framework
and server written in Haskell.  For more information, you can visit the
Snap project website at @uref{http://snapframework.com/}.  This library
contains the core definitions and types for the Snap framework.")
    (license license:bsd-3)))

(define-public ghc-snap-server
  (package
    (name "ghc-snap-server")
    (version "1.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "snap-server" version))
       (sha256
        (base32
         "0w4yv9a5ilpma0335ariwap2iscmdbaaif88lq3cm7px910nyc4j"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "snap-server")))
    (inputs
     (list ghc-attoparsec
           ghc-blaze-builder
           ghc-bytestring-builder
           ghc-case-insensitive
           ghc-clock
           ghc-io-streams
           ghc-io-streams-haproxy
           ghc-lifted-base
           ghc-network
           ghc-old-locale
           ghc-snap-core
           ghc-unix-compat
           ghc-vector))
    (native-inputs
     (list ghc-base16-bytestring
           ghc-monad-control
           ghc-random
           ghc-threads
           ghc-hunit
           ghc-quickcheck
           ghc-http-streams-bootstrap
           ghc-http-common
           ghc-parallel
           ghc-test-framework
           ghc-test-framework-hunit
           ghc-test-framework-quickcheck2))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "snap-server.cabal"
               (("\\b(attoparsec|base|bytestring|time)\\s+[^,]+" all dep)
                dep)))))))
    (home-page "http://snapframework.com/")
    (synopsis "Web server for the Snap Framework")
    (description "Snap is a simple and fast web development framework
and server written in Haskell.  For more information, you can visit the
Snap project website at @uref{http://snapframework.com/}.  The Snap HTTP
server is a high performance web server library written in Haskell.
Together with the snap-core library upon which it depends, it provides a
clean and efficient Haskell programming interface to the HTTP
protocol.")
    (license license:bsd-3)))

(define-public ghc-js-jquery
  (package
    (name "ghc-js-jquery")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (hackage-uri "js-jquery" version))
       (sha256
        (base32
         "16q68jzbs7kp07dnq8cprdcc8fd41rim38039vg0w4x11lgniq70"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "js-jquery")))
    (arguments `(#:tests? #f)) ; tests do network IO
    (home-page "https://github.com/ndmitchell/js-jquery")
    (synopsis "Obtain minified jQuery code")
    (description "This package bundles the minified
@url{http://jquery.com/, jQuery} code into a Haskell package, so it can
be depended upon by Cabal packages.  The first three components of the
version number match the upstream jQuery version.  The package is
designed to meet the redistribution requirements of downstream
users (e.g. Debian).")
    (license license:expat)))

(define-public ghc-js-flot
  (package
    (name "ghc-js-flot")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (hackage-uri "js-flot" version))
       (sha256
        (base32
         "0yjyzqh3qzhy5h3nql1fckw0gcfb0f4wj9pm85nafpfqp2kg58hv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "js-flot")))
    (inputs
     (list ghc-http))
    (home-page "https://github.com/ndmitchell/js-flot")
    (synopsis "Obtain minified flot code")
    (description "This package bundles the minified
@url{http://www.flotcharts.org/, Flot} code (a jQuery plotting library)
into a Haskell package, so it can be depended upon by Cabal packages.
The first three components of the version number match the upstream flot
version.  The package is designed to meet the redistribution
requirements of downstream users (e.g. Debian).")
    (license license:expat)))

(define-public ghc-happstack-server
  (package
    (name "ghc-happstack-server")
    (version "7.7.2")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "happstack-server" version))
              (sha256
               (base32
                "175aal1l4g558y89skck3s04db0bjblkxp77bijf1s9iyc07n669"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "happstack-server")))
    (inputs (list ghc-network
                  ghc-network-uri
                  ghc-base64-bytestring
                  ghc-blaze-html
                  ghc-extensible-exceptions
                  ghc-hslogger
                  ghc-html
                  ghc-monad-control
                  ghc-old-locale
                  ghc-semigroups
                  ghc-sendfile
                  ghc-system-filepath
                  ghc-syb
                  ghc-threads
                  ghc-transformers-base
                  ghc-transformers-compat
                  ghc-utf8-string
                  ghc-zlib))
    (native-inputs (list ghc-hunit))
    (home-page "https://happstack.com")
    (synopsis "Web related tools and services for Haskell")
    (description
     "Happstack Server provides an HTTP server and a rich set of functions for
routing requests, handling query parameters, generating responses, working with
cookies, serving files, and more.")
    (license license:bsd-3)))

(define-public ghc-sendfile
  (package
    (name "ghc-sendfile")
    (version "0.7.11.4")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "sendfile" version))
              (sha256
               (base32
                "1i2i0w18l2ysambyylv93jzy0adiiqwwnhg7zagqb7p2srybxc3k"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sendfile")))
    (inputs (list ghc-network))
    (home-page "https://github.com/Happstack/sendfile")
    (synopsis "Portable sendfile library for Haskell")
    (description
     "Haskell library which exposes zero-copy sendfile functionality in a portable way.")
    (license license:bsd-3)))

(define-public ghc-scalpel-core
  (package
    (name "ghc-scalpel-core")
    (version "0.6.2.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "scalpel-core" version))
              (sha256
               (base32
                "1yl1lsi5xm3qdlww2sb6vyppjiisj54f4yzvffv3qg8dgkfjfdra"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "scalpel-core")))
    (inputs (list ghc-data-default
                  ghc-fail
                  ghc-pointedlist
                  ghc-regex-base
                  ghc-regex-tdfa
                  ghc-tagsoup
                  ghc-vector))
    (native-inputs (list ghc-hunit))
    (home-page "https://github.com/fimad/scalpel")
    (synopsis "High level web scraping library for Haskell")
    (description
     "Scalpel core provides a subset of the scalpel web scraping library
that is intended to have lightweight dependencies and to be free of all
non-Haskell dependencies.")
    (license license:asl2.0)))

(define-public ghc-scalpel
  (package
    (name "ghc-scalpel")
    (version "0.6.2.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "scalpel" version))
              (sha256
               (base32
                "0w3l38czfsgbyd3x6yir7qw9bl8nmhclrbpbwfyhs39728jlscnc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "scalpel")))
    (inputs (list ghc-scalpel-core
                  ghc-case-insensitive
                  ghc-data-default
                  ghc-http-client
                  ghc-http-client-tls
                  ghc-tagsoup))
    (home-page "https://github.com/fimad/scalpel")
    (synopsis "High level web scraping library for Haskell")
    (description
     "Scalpel is a web scraping library inspired by libraries like Parsec
and Perl's @code{Web::Scraper}.  Scalpel builds on top of TagSoup to provide a
declarative and monadic interface.")
    (license license:asl2.0)))

(define-public ghc-sourcemap
  (package
    (name "ghc-sourcemap")
    (version "0.1.7")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "sourcemap" version))
              (sha256
               (base32
                "09i340mhzlfi5ayy9cb0378glnygdmpdhhsgikm3zrvwf2wmwr2h"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "sourcemap")))
    (inputs (list ghc-aeson ghc-unordered-containers ghc-attoparsec
                  ghc-utf8-string))
    ;(native-inputs (list node))
    (arguments (list #:tests? #f)) ; Needs node and module source-map.
    (home-page
     "https://hackage.haskell.org/package/sourcemap")
    (synopsis
     "Implementation of source maps as proposed by Google and Mozilla")
    (description
     "Sourcemap provides an implementation of source maps, revision 3,
proposed by Google and Mozilla here
@url{https://wiki.mozilla.org/DevTools/Features/SourceMap}.")
    (license license:bsd-3)))

(define-public ghc-language-javascript
  (package
    (name "ghc-language-javascript")
    (version "0.7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "language-javascript" version))
       (sha256
        (base32
         "0s6igb54cxm2jywgc3sq53f52gcsc39wd3g78yisfzvl9jm3d86i"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "language-javascript")))
    (inputs
     (list ghc-blaze-builder ghc-utf8-string))
    (native-inputs
     (list ghc-alex ghc-quickcheck ghc-happy ghc-hspec ghc-utf8-light))
    (home-page
     "https://github.com/erikd/language-javascript")
    (synopsis "Parser for JavaScript")
    (description
     "Parses Javascript into an Abstract Syntax Tree (AST).  Initially intended
as frontend to hjsmin.")
    (license license:bsd-3)))

(define-public ghc-bower-json
  (package
    (name "ghc-bower-json")
    (version "1.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bower-json" version))
       (sha256
        (base32
         "0lnhcgivg38nicncb6czkkk3z2mk3jbifv1b4r51lk3p9blzydfl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bower-json")))
    (inputs
     (list ghc-aeson ghc-aeson-better-errors ghc-scientific
           ghc-transformers ghc-unordered-containers))
    (native-inputs
     (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/hdgarrood/bower-json")
    (synopsis "Read bower.json from Haskell")
    (description
     "This package provides a data type and ToJSON/FromJSON instances for
Bower's package manifest file, bower.json.")
    (license license:expat)))

(define-public ghc-dav
  (package
    (name "ghc-dav")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "DAV" version))
       (sha256
        (base32 "1isvi4fahq70lzxfz23as7qzkc01g7kba568l6flrgd0j1984fsy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "DAV")))
    (inputs
     (list ghc-case-insensitive
           ghc-data-default
           ghc-exceptions
           ghc-http-client
           ghc-http-client-tls
           ghc-http-types
           ghc-lens
           ghc-transformers-base
           ghc-transformers-compat
           ghc-utf8-string
           ghc-xml-conduit
           ghc-xml-hamlet
           ghc-network
           ghc-network-uri
           ghc-optparse-applicative))
    (home-page "http://floss.scru.org/hDAV")
    (synopsis "RFC 4918 WebDAV support")
    (description "This package provides a library for the Web Distributed
Authoring and Versioning (WebDAV) extensions to HTTP as well an executable,
@command{hdav}, for command-line operation.")
    (license license:gpl3)))

(define-public ghc-yesod-test
  (package
    (name "ghc-yesod-test")
    (version "1.6.15")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "yesod-test" version))
              (sha256
               (base32
                "16q4f1l3m4l8iy5vmaa8c0vm2iiqhpghf3kykymlh41xy96mqpn3"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "yesod-test")))
    (inputs (list ghc-hunit
                  ghc-aeson
                  ghc-attoparsec
                  ghc-blaze-builder
                  ghc-blaze-html
                  ghc-case-insensitive
                  ghc-conduit
                  ghc-cookie
                  ghc-hspec-core
                  ghc-html-conduit
                  ghc-http-types
                  ghc-network
                  ghc-memory
                  ghc-pretty-show
                  ghc-wai
                  ghc-wai-extra
                  ghc-xml-conduit
                  ghc-xml-types
                  ghc-yesod-core
                  ghc-blaze-markup))
    (native-inputs (list ghc-hspec ghc-yesod-form ghc-unliftio
                         ghc-unliftio-core))
    (home-page "http://www.yesodweb.com")
    (synopsis "Integration testing for WAI/Yesod Applications")
    (description
     "This package's main goal is to encourage integration
and system testing of web applications by making everything easy to
test.  Tests are like browser sessions that keep track of cookies and
the last visited page.  You can perform assertions on the content of
HTML responses using CSS selectors.")
    (license license:expat)))

(define-public ghc-wai-app-static
  (package
    (name "ghc-wai-app-static")
    (version "3.1.7.4")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "wai-app-static" version))
              (sha256
               (base32
                "1h8zy3dprqjxvlqxrids65yg5qf1h4f63ddspwxrbp0r9d28hwb4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wai-app-static")))
    (inputs (list ghc-wai
                  ghc-http-types
                  ghc-unix-compat
                  ghc-old-locale
                  ghc-file-embed
                  ghc-http-date
                  ghc-blaze-html
                  ghc-blaze-markup
                  ghc-mime-types
                  ghc-unordered-containers
                  ghc-zlib
                  ghc-wai-extra
                  ghc-optparse-applicative
                  ghc-warp
                  ghc-cryptonite
                  ghc-memory))
    (native-inputs (list ghc-hspec ghc-network ghc-temporary ghc-mockery))
    (home-page "http://www.yesodweb.com/book/web-application-interface")
    (synopsis "WAI application for static serving")
    (description
     "This package provides a Web Application
Interface (WAI) application for static serving.  It also provides some
helper functions and datatypes for use outside of WAI.")
    (license license:expat)))

(define-public ghc-hjsmin
  (package
    (name "ghc-hjsmin")
    (version "0.2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hjsmin" version))
       (sha256
        (base32
         "1r2p5rjdjr25j3w4s57q5hxw2c3ymw12x7ms18yvglnq2ivr9fc1"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hjsmin")))
    (arguments
     `(#:cabal-revision ("2"
                         "184g49wsj2sfm8d75kgr7ylfw29gbyrqbqp4syyz30ch047jd0af")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dist-directory-for-tests
           (lambda _
             (substitute* '("test/test-cli.hs" "test/cli/core/runner")
               (("dist-newstyle") "dist")))))))
    (inputs
     (list ghc-language-javascript ghc-optparse-applicative))
    (native-inputs
     (list ghc-extra))
    (home-page "https://github.com/erikd/hjsmin")
    (synopsis "Haskell implementation of a JavaScript minifier")
    (description "This library provides tools reduce the size of
JavaScript files by stripping out extraneous whitespace and other
syntactic elements, without changing the semantics.")
    (license license:bsd-3)))

(define-public ghc-yesod-static
  (package
    (name "ghc-yesod-static")
    (version "1.6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "yesod-static" version))
       (sha256
        (base32
         "18f5hm9ncvkzl8bkn39cg841z0k5iqs5w45afsyk9y6k98pjd54p"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "yesod-static")))
    (inputs
     (list ghc-async
           ghc-attoparsec
           ghc-base64-bytestring
           ghc-blaze-builder
           ghc-conduit
           ghc-cryptonite
           ghc-cryptonite-conduit
           ghc-css-text
           ghc-data-default
           ghc-file-embed
           ghc-hashable
           ghc-hjsmin
           ghc-http-types
           ghc-memory
           ghc-mime-types
           ghc-unix-compat
           ghc-unordered-containers
           ghc-wai
           ghc-wai-app-static
           ghc-yesod-core))
    (native-inputs
     (list ghc-hspec ghc-yesod-test ghc-wai-extra ghc-hunit ghc-rio))
    (home-page "https://www.yesodweb.com/")
    (synopsis "Static file serving subsite for Yesod")
    (description "This package provides a static file serving subsite
for the Yesod Web Framework.")
    (license license:expat)))

(define-public ghc-wai-handler-launch
  (package
    (name "ghc-wai-handler-launch")
    (version "3.0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wai-handler-launch" version))
       (sha256
        (base32
         "1ifqgyc1ccig5angh5l1iq7vyms4lvi8wzvysg5dw82nml49n02m"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wai-handler-launch")))
    (inputs
     (list ghc-wai ghc-warp ghc-http-types ghc-streaming-commons
           ghc-async))
    (home-page "https://hackage.haskell.org/package/wai-handler-launch")
    (synopsis "Launch a Web application in the default browser")
    (description "This package handles cross-platform Web browser
launching and inserts JavaScript code to ping the server.  When the
server no longer receives pings, it shuts down.")
    (license license:expat)))

(define-public ghc-wai-cors
  (package
    (name "ghc-wai-cors")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "wai-cors" version))
       (sha256
        (base32
         "10gv3jjlkcb13031frr818p56v2s0qf6dqjsfpwlhwdxdssvx5r5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "wai-cors")))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; As of version 0.2.7, there are two test suites: "unit-tests"
         ;; and "phantomjs".  Since we do not have a PhantomJS package,
         ;; we only run the unit tests.
         (replace 'check
           (lambda _
             (invoke "runhaskell" "Setup.hs" "test" "unit-tests"))))))
    (inputs
     (list ghc-attoparsec ghc-base-unicode-symbols ghc-case-insensitive
           ghc-http-types ghc-wai))
    (native-inputs
     (list ghc-network
           ghc-wai-websockets
           ghc-warp
           ghc-websockets
           ghc-tasty
           ghc-tasty-hunit
           ghc-wai-extra
           ghc-wai-websockets
           ghc-warp
           ghc-websockets))
    (home-page "https://github.com/larskuhtz/wai-cors")
    (synopsis "Cross-Origin Resource Sharing (CORS) for WAI")
    (description "This package provides an implementation of Cross-Origin
Resource Sharing (CORS) for the Web Application Framework (WAI) that
aims to be compliant with @url{https://www.w3.org/TR/cors}.")
    (license license:expat)))

(define-public ghc-network-run
  (package
    (name "ghc-network-run")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "network-run" version))
       (sha256
        (base32 "08662w7ja9w4a4fwikaawxnxcszkd0mdmaajmshas2dd25xyikwi"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "network-run")))
    (inputs (list ghc-network))
    (home-page "https://hackage.haskell.org/package/network-run")
    (synopsis "Simple network runner library")
    (description
     "This package provides a simple network runner library in Haskell.")
    (license license:bsd-3)))

