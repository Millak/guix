;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024, 2025 Igor Goryachev <igor@goryachev.org>
;;; Copyright © 2025, 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>
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

(define-module (gnu packages erlang-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system rebar)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public erlang-binpp
  (package
    (name "erlang-binpp")
    (version "1.1.1")
    (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jtendo/binpp")
               (commit version)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dv7mg0j6q4vs4bcp14df3q1y3if58dskca98wmnsrbwkibkn6vp"))
         (patches (search-patches "erlang-binpp-disable-failing-tests.patch"))))
    (build-system rebar-build-system)
    (synopsis "Erlang Binary Pretty Printer")
    (description "@code{Binpp} will use @code{io:format} to output the formatted
binary by default.  However there are options making @code{pprint} functions
return formatted data instead of performing direct IO write.")
    (home-page "https://github.com/jtendo/binpp")
    (license license:wtfpl2)))

(define-public erlang-coveralls
  (package
    (name "erlang-coveralls")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "coveralls" version))
       (sha256
        (base32 "18q4c8bcrpa48mvwpwdh51ma84zfxhcmd70qh2956jy6m05dnm6d"))))
    (build-system rebar-build-system)
    (propagated-inputs (list erlang-jsx))
    (synopsis "Coveralls for Erlang")
    (description "This package provides @code{erlang-coveralls}, an Erlang
module to convert and send cover data to Coveralls.")
    (home-page "https://hex.pm/packages/coveralls")
    (license license:bsd-2)))

(define-public erlang-cowboy
  (package
    (name "erlang-cowboy")
    (version "2.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cowboy" version))
       (sha256
        (base32 "0pwd9v4rvbyhyq1n7r146sfhsv9kg4mhawk4a36l40w2sp4y9w44"))))
    (build-system rebar-build-system)
    (propagated-inputs (list erlang-cowlib erlang-ranch))
    (synopsis "Small, fast, modern HTTP server")
    (description "Cowboy aims to provide a complete HTTP stack in a small code
base.  It is optimized for low latency and low memory usage, in part because it
uses binary strings.

Cowboy provides routing capabilities, selectively dispatching requests to
handlers written in Erlang.  Because it uses Ranch for managing connections,
Cowboy can easily be embedded in any other application.")
    (home-page "https://hex.pm/packages/cowboy")
    (license license:isc)))

(define-public erlang-cowboy-telemetry
  (package
    (name "erlang-cowboy-telemetry")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cowboy_telemetry" version))
       (sha256
        (base32 "1pn90is3k9dq64wbijvzkqb6ldfqvwiqi7ymc8dx6ra5xv0vm63x"))))
    (build-system rebar-build-system)
    (propagated-inputs (list erlang-cowboy erlang-telemetry))
    (synopsis "Telemetry instrumentation for Cowboy")
    (description "This package provides @code{erlang-cowboy-telemetry}, a
library implementing Telemetry instrumentation for Cowboy.")
    (home-page "https://github.com/beam-telemetry/cowboy_telemetry")
    (license license:asl2.0)))

(define-public erlang-cowlib
  (package
    (name "erlang-cowlib")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cowlib" version))
       (sha256
        (base32 "06nwaaqj2rff408cfg2pn80ag01darzaz8dmvk7x8qhilqrrvy64"))))
    (build-system rebar-build-system)
    (arguments
     ;; FIXME: tests fail with
     ;; src/cow_base64url.erl:{27,14}:
     ;; can't find include lib "proper/include/proper.hrl"
     (list #:tests? #f))
    (native-inputs
     (list erlang-proper))
    (synopsis "Manipulate Web protocols")
    (description "This package provides @code{erlang-cowlib}, a support library
for manipulating Web protocols.")
    (home-page "https://hex.pm/packages/cowlib")
    (license license:isc)))

(define-public erlang-erlang-color
  (package
    (name "erlang-erlang-color")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "erlang_color" version))
       (sha256
        (base32 "04vczpwp105nmx2s6c2lp3rh4dy2zy4l1md73w2ycv98v7gw6sbs"))))
    (build-system rebar-build-system)
    (arguments
     (list
      #:tests? #f)) ;some required rebar plugins
    (synopsis "ANSI colors for your Erlang")
    (description "This library implements ANSI colors for your Erlang.")
    (home-page "https://hex.pm/packages/erlang_color")
    (license license:expat)))

(define-public erlang-erlydtl
  (package
    (name "erlang-erlydtl")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/erlydtl/erlydtl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00p194jgmvzqza7xr7fdm2n091ymkyy66aj4gc82n0kzdlh03vbm"))))
    (build-system rebar-build-system)
    (synopsis "Django templates for Erlang")
    (description
     "ErlyDTL compiles Django Template Language to Erlang bytecode.")
    (home-page "https://github.com/erlydtl/erlydtl")
    (license license:expat)))

(define-public erlang-exometer-core
  (package
    (name "erlang-exometer-core")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "exometer_core" version))
       (sha256
        (base32 "191kjqqbvz0jjijfhkbs73lnsmlrr2yinw30pkby0fx363l378xf"))))
    (build-system rebar-build-system)
    (native-inputs
     (list erlang-meck))
    (propagated-inputs (list erlang-hut erlang-parse-trans erlang-setup))
    (synopsis "Code instrumentation and metrics collection")
    (description "This package provides @code{erlang-exometer-core}, a library
for code instrumentation and metrics collection.")
    (home-page "https://hexdocs.pm/exometer_core/")
    (license license:mpl2.0)))

(define-public erlang-h2
  (package
    (name "erlang-h2")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "h2" version))
       (sha256
        (base32 "0n63z9kn80yvf7sqslw5fqdxhd8jw3q2np7kx9sn7iqd3lv287y6"))))
    (build-system rebar-build-system)
    (native-inputs (list erlang-proper))
    (synopsis "HTTP/2 protocol library for Erlang")
    (description "This package provides an HTTP/2 protocol library for
Erlang.")
    (home-page "https://hex.pm/packages/h2")
    (license license:asl2.0)))

(define-public erlang-luerl
  (package
    (name "erlang-luerl")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "luerl" version))
       (sha256
        (base32 "144ka135kqaimwpvnnc1b0yyxqjdimlahidjjg55s3dakf28vy5b"))))
    (build-system rebar-build-system)
    (synopsis "Implementation of Lua on Erlang")
    (description "This package provides implementation of Lua on Erlang.")
    (home-page "https://hex.pm/packages/luerl")
    (license license:asl2.0)))

(define-public erlang-jose
  (package
    (name "erlang-jose")
    (version "1.11.12")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "jose" version))
       (sha256
        (base32 "0yxb3djf8qygz291s0d9sany2za56xa8ipawfsbbc44j7rjjps9i"))))
    (build-system rebar-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Do not treat warnings as errors, for more info see:
          ;; https://github.com/potatosalad/erlang-jose/issues/168
          (add-after 'unpack 'relax-build-options
            (lambda _
              (substitute* "rebar.config"
                (("debug_info,") "debug_info"))
              (substitute* "rebar.config"
                (("warnings_as_errors") "")))))))
    (synopsis
     "JSON Object Signing and Encryption for Erlang and Elixir")
    (description
     "This package provides JSON Object Signing and Encryption (JOSE) for
Erlang and Elixir.")
    (home-page "https://hex.pm/packages/jose")
    (license license:expat)))

(define-public erlang-jiffy
  (package
    (name "erlang-jiffy")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "jiffy" version))
       (sha256
        (base32 "1m7r76l9608x2fs825kvxxz9l2r66mj9q494yanczgahqh9nwrv7"))))
    (build-system rebar-build-system)
    (native-inputs (list erlang-pc
                         python)) ; for tests
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "JSON Decoder/Encoder")
    (description "This package provides JSON Decoder/Encoder for Erlang.")
    (home-page "https://hex.pm/packages/jiffy")
    (license license:expat)))

(define-public erlang-hackney
  (package
    (name "erlang-hackney")
    (version "4.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "hackney" version))
       (sha256
        (base32 "1r0hh44z4ny3b09wgbqqgj98in96v3bfksv26gpi263bg993xl7s"))))
    (build-system rebar-build-system)
    (native-inputs
     (list erlang-cowboy erlang-jsx rebar3-ex-doc))
    (propagated-inputs
     (list erlang-certifi
           erlang-h2
           erlang-idna
           erlang-mimerl
           erlang-parse-trans
           erlang-quic
           erlang-ssl-verify-fun
           erlang-webtransport))
    (synopsis "Simple HTTP client")
    (description "This package provides @code{erlang-hackney}, a simple HTTP
client.")
    (home-page "https://hexdocs.pm/hackney/")
    (license license:asl2.0)))

(define-public erlang-hut
  (package
    (name "erlang-hut")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "hut" version))
       (sha256
        (base32 "00xvdqyycljks3dh79jz4rjmdwcp7ky6158dywva765fkd5p1y3s"))))
    (build-system rebar-build-system)
    (synopsis
     "Helper logging library for Erlang")
    (description
     "This package provides an helper library for making Erlang libraries
logging framework agnostic.")
    (home-page "https://hex.pm/packages/hut")
    (license license:expat)))

(define-public erlang-meck
  (package
    (name "erlang-meck")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "meck" version))
       (sha256
        (base32 "1yaaxqnhacqn6kqg6qpmr6z6licpnwhkbanjgkdr4jgg0s1x1kx2"))))
    (build-system rebar-build-system)
    (arguments
     (list
      ;; FIXME: Tests depend on elixir-unite which is not
      ;; yet packaged.
      #:tests? #f))
    (synopsis "Mocking framework for Erlang")
    (description "This package provides @code{meck}, a mocking framework for
Erlang.")
    (home-page "https://hexdocs.pm/meck/")
    (license license:asl2.0)))

(define-public erlang-metrics
  (package
    (name "erlang-metrics")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "metrics" version))
       (sha256
        (base32 "0jviygsq88x6hymrhrg1zwg1wp71ccmpk6my2xyykgf3r0w1j1mf"))))
    (build-system rebar-build-system)
    (native-inputs
     (list erlang-exometer-core))
    (synopsis "Generic interface to metrics systems in Erlang")
    (description
     "This package provides a generic interface to different metrics systems in
Erlang.")
    (home-page "https://hex.pm/packages/metrics")
    (license license:bsd-2)))

(define-public erlang-metrics-1.0
  (package
    (inherit erlang-metrics)
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "metrics" version))
       (sha256
        (base32 "05lz15piphyhvvm3d1ldjyw0zsrvz50d2m5f2q3s8x2gvkfrmc39"))))))

(define-public erlang-mimerl
  (package
    (name "erlang-mimerl")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "mimerl" version))
       (sha256
        (base32 "1i7cx6drwvxz208m94znrqbr9x227w9dvdd89jl4xqdscph8qr6v"))))
    (build-system rebar-build-system)
    (synopsis "Library to handle mimetypes")
    (description "This package provides a library to handle mimetypes.")
    (home-page "https://hex.pm/packages/mimerl")
    (license license:expat)))

(define-public erlang-mochiweb
  (package
    (name "erlang-mochiweb")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mochi/mochiweb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "191c7vzny2f1467ikw8pcdy9bsnm14ydb4ah2kxg2sd75812ymi7"))
       (patches
        (search-patches
         "erlang-mochiweb-disable-known-to-fail-tests.patch"))))
    (build-system rebar-build-system)
    (synopsis "MochiMedia Web Server")
    (description "This package provides @code{MochiMedia}, an Erlang Web
Server.")
    (home-page "https://github.com/mochi/mochiweb")
    (license license:expat)))

(define-public erlang-pkix
  (package
    (name "erlang-pkix")
    (version "1.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "pkix" version))
       (sha256
        (base32 "03jxmjirg98r1zq7b1f3mnwm8pb1iac2iaxi85615jwl63w688g0"))))
    (build-system rebar-build-system)
    (synopsis "PKIX management")
    (description "This package provides PKIX management for Erlang.")
    (home-page "https://hex.pm/packages/pkix")
    (license license:asl2.0)))

(define-public erlang-p1-acme
  (package
    (name "erlang-p1-acme")
    (version "1.0.31")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_acme" version))
       (sha256
        (base32 "076v6ar5fdshvl4bfgrwdn5fdimdsg55rpk4ks4aakdr1alj4ncv"))))
    (build-system rebar-build-system)
    (inputs (list erlang-base64url erlang-idna erlang-jiffy erlang-jose
                  erlang-yconf))
    (native-inputs (list erlang-pc))
    (arguments
     (list
      #:tests? #f ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (synopsis "ACME client for Erlang")
    (description "This package provides ACME client for Erlang.")
    (home-page "https://hex.pm/packages/p1_acme")
    (license license:asl2.0)))

(define-public erlang-p1-oauth2
  (package
    (name "erlang-p1-oauth2")
    (version "0.6.14")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_oauth2" version))
       (sha256
        (base32 "13xfk4flaqb3nsxirf3vmy3yv67n6s6xzil7bafjswj39r3srlqz"))))
    (build-system rebar-build-system)
    (synopsis "OAuth 2.0 implementation for Erlang")
    (description "This package provides OAuth 2.0 implementation for Erlang.")
    (home-page "https://hex.pm/packages/p1_oauth2")
    (license (list license:expat license:asl2.0))))

(define-public erlang-p1-utils
  (package
    (name "erlang-p1-utils")
    (version "1.0.29")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_utils" version))
       (sha256
        (base32 "1fg1mdjas6h1qlzykxli4crir18l9ya4s7g92kqzzf5mmlf44w90"))))
    (build-system rebar-build-system)
    (synopsis "ProcessOne utility modules for Erlang")
    (description "This package provides ProcessOne utility modules for Erlang.")
    (home-page "https://hex.pm/packages/p1_utils")
    (license license:asl2.0)))

(define-public erlang-p1-mysql
  (package
    (name "erlang-p1-mysql")
    (version "1.0.28")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_mysql" version))
       (sha256
        (base32 "06crdlv7nln38j1m0fgz6vcg4p21zspb7nn8j58bl1g5xkl00kbf"))))
    (build-system rebar-build-system)
    (synopsis "Pure Erlang MySQL driver")
    (description "This package provides pure Erlang @code{MySQL} driver.")
    (home-page "https://hex.pm/packages/p1_mysql")
    (license license:asl2.0)))

(define-public erlang-p1-pgsql
  (package
    (name "erlang-p1-pgsql")
    (version "1.1.41")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_pgsql" version))
       (sha256
        (base32 "0wm10ijr0awqfdlhlczxpchgqlva5jmrvzxksknn81257nx2mvi6"))))
    (build-system rebar-build-system)
    (inputs (list erlang-stringprep))
    (synopsis "PostgreSQL driver for Erlang")
    (description "This package provides @code{PostgreSQL} driver for Erlang.")
    (home-page "https://hex.pm/packages/p1_pgsql")
    (license license:asl2.0)))

(define-public erlang-quic
  (package
    (name "erlang-quic")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "quic" version))
       (sha256
        (base32 "17bhmp9w6c92g3fda0q4bwzv1hya90s2rxg5zsnw7xbq3m7pi1cv"))))
    (build-system rebar-build-system)
    (native-inputs (list erlang-meck erlang-proper))
    (synopsis "Erlang QUIC implementation (RFC 9000)")
    (description "This package provides a pure Erlang QUIC (RFC 9000)
implementation.")
    (home-page "https://hex.pm/packages/quic")
    (license license:asl2.0)))

(define-public erlang-ranch
  (package
    (name "erlang-ranch")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ranch" version))
       (sha256
        (base32 "1rzqykpqfiwagmym523yzzxk5bmxsfl9x9cp8652300cg2hrj2zs"))))
    (build-system rebar-build-system)
    (synopsis "Socket acceptor pool for TCP protocols.")
    (description "Ranch aims to provide everything you need to accept TCP
connections with a small code base and low latency while being easy to use
directly as an application or to embed into your own.")
    (home-page "https://hex.pm/packages/ranch")
    (license license:isc)))

(define-public erlang-setup
  (package
    (name "erlang-setup")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "setup" version))
       (sha256
        (base32 "1d3x8mhsp03ppahny1yj2i910qqvgqixx3knw7283jijgpabpfpp"))))
    (build-system rebar-build-system)
    (synopsis "Generic setup application for Erlang-based systems")
    (description "This package implements a generic setup application for
Erlang-based systems.")
    (home-page "https://github.com/uwiger/setup")
    (license license:asl2.0)))

(define-public erlang-shards
  (package
    (name "erlang-shards")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "shards" version))
       (sha256
        (base32 "1nlfx82x9wpqlhyc5j22xjxdpl1kyhdx71mzbyhwss36mrfh96hn"))))
    (build-system rebar-build-system)
    (native-inputs
     (list erlang-covertool rebar3-ex-doc rebar3-proper))
    (arguments
     (list
      ;; FIXME: Tests depend on rebar3-hex, which is not packaged yet.
      #:tests? #f))
    (synopsis "Partitioned or sharded ETS tables")
    (description
     "Erlang/Elixir library for partitioned or sharded ETS tables.")
    (home-page "https://hexdocs.pm/shards/")
    (license license:expat)))

(define-public erlang-sqlite3
  (package
    (name "erlang-sqlite3")
    (version "1.1.15")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "sqlite3" version))
       (sha256
        (base32 "0mr8kpv8hf4yknx8vbmyakgasrhk64ldsbafvr4svhi26ghs82rw"))))
    (build-system rebar-build-system)
    (native-inputs (list erlang-pc sqlite))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "SQLite3 driver for Erlang")
    (description "This package provides SQLite3 driver for Erlang.")
    (home-page "https://hex.pm/packages/sqlite3")
    (license license:asl2.0)))

(define-public erlang-stringprep
  (package
    (name "erlang-stringprep")
    (version "1.0.34")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "stringprep" version))
       (sha256
        (base32 "0iwvijm0nvx670gvqc9ibvzl0irhmp9lbs7vavh685w8f6iqxrr7"))))
    (build-system rebar-build-system)
    (inputs (list erlang-p1-utils))
    (native-inputs (list erlang-pc))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "Fast Stringprep Erlang/Elixir implementation")
    (description "This package provides fast Stringprep Erlang/Elixir
implementation.")
    (home-page "https://hex.pm/packages/stringprep")
    (license license:asl2.0)))

(define-public erlang-tdiff
  (package
    (name "erlang-tdiff")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "tdiff" version))
       (sha256
        (base32 "0xbq7p9ii2kp49ms1kylj92ih2jiwvqwimb8jy4aalljz5lf3hp0"))))
    (build-system rebar-build-system)
    (synopsis "Difference library")
    (description "This package provides a difference library.")
    (home-page "https://hex.pm/packages/tdiff")
    (license license:lgpl2.0)))

(define-public erlang-telemetry
  (package
    (name "erlang-telemetry")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "telemetry" version))
       (sha256
        (base32 "1cgb3zv0mdjlp8k33vkb6aj93n7f15k69lf0c9w0f1k50san93wj"))))
    (build-system rebar-build-system)
    (synopsis "Dynamic dispatching library for metrics and instrumentations")
    (description
     "@code{Telemetry} is a lightweight library for dynamic dispatching of
events, with a focus on metrics and instrumentation.  Any Erlang or Elixir
library can use @code{telemetry} to emit events.  Application code and other
libraries can then hook into those events and run custom handlers.")
    (home-page "https://hexdocs.pm/telemetry/")
    (license license:asl2.0)))

(define-public erlang-telemetry-poller
  (package
    (name "erlang-telemetry-poller")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "telemetry_poller" version))
       (sha256
        (base32 "04ajnflsj9xg8sv2012npazsjvj3fjcxp5sqyx84lm18f7nqpwai"))))
    (build-system rebar-build-system)
    (propagated-inputs (list erlang-telemetry))
    (synopsis
     "Periodically collect measurements and dispatch them as Telemetry events")
    (description
     "This package provides a library to periodically collect measurements and
dispatch them as Telemetry events.")
    (home-page "https://hexdocs.pm/telemetry_poller/")
    (license license:asl2.0)))

(define-public erlang-cache-tab
  (package
    (name "erlang-cache-tab")
    (version "1.0.34")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cache_tab" version))
       (sha256
        (base32 "14mnh96agbbg88na895z9knvn9zsbvi2b0gaz34if74lycbz7f8d"))))
    (build-system rebar-build-system)
    (inputs (list erlang-p1-utils))
    (native-inputs (list erlang-pc))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "In-memory cache Erlang/Elixir library")
    (description "This package provides in-memory cache Erlang/Elixir library.")
    (home-page "https://hex.pm/packages/cache_tab")
    (license license:asl2.0)))

(define-public erlang-eimp
  (package
    (name "erlang-eimp")
    (version "1.0.27")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "eimp" version))
       (sha256
        (base32 "1z7kazcd1909fwwqw8g4ls2c9f5pc95zqm7h3c5gbymwjgi86ziw"))))
    (build-system rebar-build-system)
    (inputs (list erlang-p1-utils))
    (native-inputs (list erlang-pc))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "Erlang/Elixir image converter")
    (description "This package provides Erlang/Elixir image converter.")
    (home-page "https://hex.pm/packages/eimp")
    (license license:asl2.0)))

(define-public erlang-mqtree
  (package
    (name "erlang-mqtree")
    (version "1.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "mqtree" version))
       (sha256
        (base32 "1l1y7055rqcqx39xvq8pa2gbwkqb2ipgj2v0943899s8kixy1h2y"))))
    (build-system rebar-build-system)
    (inputs (list erlang-p1-utils))
    (native-inputs (list erlang-pc))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "Index tree for MQTT topic filters")
    (description "This package provides index tree for MQTT topic filters.")
    (home-page "https://hex.pm/packages/mqtree")
    (license license:asl2.0)))

(define-public erlang-ezlib
  (package
    (name "erlang-ezlib")
    (version "1.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ezlib" version))
       (sha256
        (base32 "01844li2kfdbkg9qhcl9dm3jpnx88bqxypv4mrzcn7ix8109b0dl"))))
    (build-system rebar-build-system)
    (inputs (list erlang-p1-utils))
    (native-inputs (list erlang-pc))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "Native zlib driver for Erlang/Elixir")
    (description "This package provides native zlib driver for Erlang/Elixir.")
    (home-page "https://hex.pm/packages/ezlib")
    (license license:asl2.0)))

(define-public erlang-fast-tls
  (package
    (name "erlang-fast-tls")
    (version "1.1.26")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "fast_tls" version))
       (sha256
        (base32 "0gwvnnqsbhikvahh34d32n15z3w00agz7b59p9g5cdwh6394s3bb"))))
    (build-system rebar-build-system)
    (inputs (list erlang-p1-utils openssl))
    (native-inputs (list erlang-pc openssl))
    (arguments
     (list
      #:tests? #f ; some required files are absent
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-/bin/sh
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((sh (search-input-file inputs "/bin/sh")))
                (substitute* "configure"
                  (("/bin/sh") sh)))))
          (add-after 'unpack 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc")
              (let ((openssl (assoc-ref %build-inputs "openssl")))
                (setenv "LDFLAGS" (string-append "-L" openssl "/lib"))
                (setenv "CFLAGS" (string-append "-I" openssl "/include")))))
          (add-before 'build 'configure
            (lambda _
              (invoke "./configure"))))))
    (synopsis "TLS/SSL OpenSSL-based native driver for Erlang/Elixir")
    (description
     "This package provides TLS/SSL @code{OpenSSL}-based native driver
for Erlang/Elixir.")
    (home-page "https://hex.pm/packages/fast_tls")
    (license license:asl2.0)))

(define-public erlang-stun
  (package
    (name "erlang-stun")
    (version "1.2.22")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "stun" version))
       (sha256
        (base32 "12hq8cb4w859ss94h2a899pv2c0cjh30l9ivyn4a0dsj3nqv821l"))))
    (build-system rebar-build-system)
    (inputs (list erlang-fast-tls erlang-p1-utils))
    (synopsis "STUN and TURN library for Erlang/Elixir")
    (description "This package provides STUN and TURN library for
Erlang/Elixir.")
    (home-page "https://hex.pm/packages/stun")
    (license license:asl2.0)))

(define-public erlang-fast-xml
  (package
    (name "erlang-fast-xml")
    (version "1.1.60")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "fast_xml" version))
       (sha256
        (base32 "1i9pplm3yvfax7rczjfzdw4s0bv48m8an13src14f827vm4jg7ng"))))
    (build-system rebar-build-system)
    (inputs (list erlang-p1-utils))
    (native-inputs (list erlang-pc))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "Fast Expat-based Erlang/Elixir XML parsing library")
    (description "This package provides fast Expat-based Erlang/Elixir XML
parsing library.")
    (home-page "https://hex.pm/packages/fast_xml")
    (license license:asl2.0)))

(define-public erlang-webtransport
  (package
    (name "erlang-webtransport")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "webtransport" version))
       (sha256
        (base32 "17dbgmjgv6waaxya3326hsc375m5l19xh7dg7kp6pdfdjrkcgs1s"))))
    (build-system rebar-build-system)
    (native-inputs (list erlang-proper))
    (propagated-inputs (list erlang-h2 erlang-quic))
    (synopsis "WebTransport protocol for Erlang (HTTP/2 and HTTP/3)")
    (description
     "This package provides the @code{WebTransport} protocol for Erlang, over
HTTP/2 and HTTP/3.")
    (home-page "https://hex.pm/packages/webtransport")
    (license license:asl2.0)))

(define-public erlang-xmpp
  (package
    (name "erlang-xmpp")
    (version "1.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "xmpp" version))
       (sha256
        (base32 "0j3gd95l7hz8df3c1xci7k83jijq60m2jjvrm8mfdzncvfj8lr05"))))
    (build-system rebar-build-system)
    (inputs (list erlang-ezlib
                  erlang-fast-tls
                  erlang-fast-xml
                  erlang-idna
                  erlang-p1-utils
                  erlang-stringprep))
    (native-inputs (list erlang-pc erlang-provider-asn1))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "XMPP parsing and serialization library for Erlang/Elixir")
    (description "This package provides XMPP parsing and serialization library
for Erlang/Elixir.")
    (home-page "https://hex.pm/packages/xmpp")
    (license license:asl2.0)))

(define-public erlang-esip
  (package
    (name "erlang-esip")
    (version "1.0.60")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "esip" version))
       (sha256
        (base32 "0kql0gyzli9v9b8rj4dbbdk4zki00viwyc08q291bnn8pgacn106"))))
    (build-system rebar-build-system)
    (inputs (list erlang-fast-tls erlang-p1-utils erlang-stun))
    (native-inputs (list erlang-pc))
    (arguments
     (list
      #:tests? #f ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "SIP server component in Erlang")
    (description "This package provides ProcessOne SIP server component in
Erlang.")
    (home-page "https://hex.pm/packages/esip")
    (license license:asl2.0)))

(define-public erlang-fast-yaml
  (package
    (name "erlang-fast-yaml")
    (version "1.0.40")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "fast_yaml" version))
       (sha256
        (base32 "11mk69nlg3kvlh7i2lsvclp8p4pssn3z76xsd0r6y71cj0lmqw17"))))
    (build-system rebar-build-system)
    (inputs (list erlang-p1-utils))
    (native-inputs (list erlang-pc libyaml))
    (arguments
     (list
      #:tests? #f ; some required files are absent
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc")
              (let ((openssl (assoc-ref %build-inputs "libyaml")))
                (setenv "LDFLAGS" (string-append "-L" openssl "/lib"))
                (setenv "CFLAGS" (string-append "-I" openssl "/include"))))))))
    (synopsis "Fast YAML native library for Erlang/Elixir")
    (description "This package provides fast YAML native library for
Erlang/Elixir.")
    (home-page "https://hex.pm/packages/fast_yaml")
    (license license:asl2.0)))

(define-public erlang-yconf
  (package
    (name "erlang-yconf")
    (version "1.0.23")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "yconf" version))
       (sha256
        (base32 "0dqlqgd4291nlgd0sqyfxdgbaj3xp1pmrkvllz56l0i8afvxzd15"))))
    (build-system rebar-build-system)
    (inputs (list erlang-fast-yaml))
    (synopsis "YAML configuration processor")
    (description "This package provides YAML configuration processor.")
    (home-page "https://hex.pm/packages/yconf")
    (license license:asl2.0)))

(define-public erlang-epam
  (package
    (name "erlang-epam")
    (version "1.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "epam" version))
       (sha256
        (base32 "12frsirp8m0ajdb19xi1g86zghhgvld5cgw459n2m9w553kljd1g"))))
    (build-system rebar-build-system)
    (native-inputs (list erlang-pc linux-pam))
    (arguments
     (list
      #:tests? #f ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc"))))))
    (synopsis "Helper for PAM authentication support")
    (description "This package provides epam helper for PAM authentication
support.")
    (home-page "https://hex.pm/packages/epam")
    (license license:asl2.0)))

(define-public erlang-eredis
  (package
    (name "erlang-eredis")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "eredis" version))
       (sha256
        (base32 "1h9wihjqs4fmgr5ihqpisf7k99h006dsf71lygp5zmgycv2m8avw"))))
    (build-system rebar-build-system)
    (synopsis
     "Non-blocking Redis client with focus on performance and robustness")
    (description
     "This package provides non-blocking Redis client for Erlang with focus
on performance and robustness.")
    (home-page "https://hex.pm/packages/eredis")
    (license license:expat)))

(define-public erlang-unicode-util-compat
  (package
    (name "erlang-unicode-util-compat")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "unicode_util_compat" version))
       (sha256
        (base32 "0hinn81kwkr3fvxb4vvp6qqnf19f23hd2jkl34v27bp39j2igadk"))))
    (build-system rebar-build-system)
    (synopsis "Compatibility library for Erlang < 20")
    (description "This package provides @code{unicode_util} compatibility
library for Erlang < 20.")
    (home-page "https://hex.pm/packages/unicode_util_compat")
    (license license:asl2.0)))

(define-public erlang-idna
  (package
    (name "erlang-idna")
    (version "7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "idna" version))
       (sha256
        (base32 "07y7z3fxdzza5pc48k584ra1p2aljs6m1f6am1hxydmz4nh5ksba"))))
    (build-system rebar-build-system)
    (inputs (list erlang-unicode-util-compat))
    (synopsis "Pure Erlang IDNA implementation")
    (description "This package provides a pure Erlang IDNA implementation.")
    (home-page "https://hex.pm/packages/idna")
    (license license:expat)))

(define-public erlang-base64url
  (package
    (name "erlang-base64url")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "base64url" version))
       (sha256
        (base32 "0p4zf53v86zfpnk3flinjnk6cx9yndsv960386qaj0hsfgaavczr"))))
    (build-system rebar-build-system)
    (synopsis "URL safe base64-compatible codec")
    (description "This package provides URL safe base64-compatible codec
for Erlang.")
    (home-page "https://hex.pm/packages/base64url")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
