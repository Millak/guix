;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>
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

(define-module (gnu packages elixir-web)
  #:use-module (gnu packages)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages erlang-xyz)
  #:use-module (gnu packages elixir-databases)
  #:use-module (gnu packages elixir-i18n)
  #:use-module (gnu packages elixir-markup)
  #:use-module (gnu packages elixir-xyz)
  #:use-module (guix build-system mix)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages))

(define-public elixir-bandit
  (package
    (name "elixir-bandit")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "bandit" version))
       (sha256
        (base32 "08jll47yxkj4n08nh7xm8bm36z077a458j4xlqp2mzr0xm7gyn44"))))
    (build-system mix-build-system)
    (arguments
     ;; Tests depend on elixir-req which is not yet packaged.
     (list #:tests? #f))
    (propagated-inputs
     (list elixir-hpax
           elixir-plug
           erlang-telemetry
           elixir-thousand-island
           elixir-websock))
    (synopsis "HTTP server built for Plug & WebSock apps")
    (description
     "This package provides a pure-Elixir HTTP server built for Plug & @code{WebSock}
apps.")
    (home-page "https://hexdocs.pm/bandit/")
    (license license:expat)))

(define-public elixir-con-cache
  (package
    (name "elixir-con-cache")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "con_cache" version))
       (sha256
        (base32 "05dfx20f6sncxcpmld7s3gc4jmibkyhn1g2vbg3n8r99xhdlvvqx"))))
    (build-system mix-build-system)
    (propagated-inputs (list erlang-telemetry))
    (synopsis
     "ETS based key-value storage")
    (description
     "This package provides @code{con_cache}, an ETS based key-value storage
with support for row-level isolated writes, TTL auto-purge, and modification
callbacks.")
    (home-page "https://hexdocs.pm/con_cache/")
    (license license:expat)))

;; This package lives here to avoid module level circular dependencies as it
;; depends on elixir-plug.
(define-public elixir-ecto-shorts
  (package
    (name "elixir-ecto-shorts")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ecto_shorts" version))
       (sha256
        (base32 "1r7cc0dhhvmqnwicrnay9lsaf2xahr919g993rbbakdca2xw0q3l"))))
    (build-system mix-build-system)
    (native-inputs (list elixir-credo elixir-excoveralls))
    (propagated-inputs (list elixir-ecto-sql elixir-error-message))
    (synopsis
     "Helper tools for making ecto interactions shorter")
    (description
     "Helper tools for making ecto interactions more pleasant and shorter.")
    (home-page "https://hexdocs.pm/ecto_shorts/")
    (license license:expat)))

(define-public elixir-emote
  (package
    (name "elixir-emote")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "emote" version))
       (sha256
        (base32 "1km0hkvjpswxrvadrz4wjarpnv6wz3p15bdmmlw0yswnfvmij4ni"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-phoenix-html))
    (synopsis
     "Convert emoticons and emoji names to emoji characters or images")
    (description
     "This package provides @code{elixir-emote}, a small library for
converting emoticons and emoji names to emoji characters or images,
including custom emoji.")
    (home-page "https://hexdocs.pm/emote/")
    (license license:wtfpl2)))

;; This package lives here to avoid module level circular dependencies as it
;; depends on elixir-plug.
(define-public elixir-error-message
  (package
    (name "elixir-error-message")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "error_message" version))
       (sha256
        (base32 "0wsbcwby3grnb5s6558s2x5wrhy8f7vwbbfdk88rsacbf1w0l6k7"))))
    (build-system mix-build-system)
    (native-inputs (list elixir-excoveralls))
    (propagated-inputs (list elixir-jason elixir-plug))
    (synopsis "Make errors consistent across your system")
    (description
     "Error system to help make errors consistent across your system.")
    (home-page "https://hexdocs.pm/error_message/")
    (license license:expat)))

;; This package lives here to avoid module level circular dependencies as it
;; depends on elixir-mint.
(define-public elixir-ex-cldr-dates-times
  (package
    (name "elixir-ex-cldr-dates-times")
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr_dates_times" version))
       (sha256
        (base32 "00zzwr07x8i8dvhqi2vxpa858s874l941h2m41x9ip2k96dgc3nk"))))
    (build-system mix-build-system)
    (propagated-inputs
     (list elixir-calendar-interval
           elixir-ex-cldr
           elixir-ex-cldr-calendars
           elixir-ex-cldr-units
           elixir-jason
           elixir-tz))
    (synopsis
     "Date, Time and DateTime localization, internationalization and formatting")
    (description
     "Date, Time and @code{DateTime} localization, internationalization and formatting
functions using the Common Locale Data Repository (CLDR).")
    (home-page "https://hexdocs.pm/ex_cldr_dates_times/")
    (license license:asl2.0)))

(define-public elixir-ex-cldr-plugs
  (package
    (name "elixir-ex-cldr-plugs")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr_plugs" version))
       (sha256
        (base32 "0apm3x98abjnlhzb59p3qnhh2pmzrbk8gh0x209k0h5cgq4rx0ih"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-ex-cldr elixir-gettext elixir-jason elixir-plug))
    (synopsis
     "Plugs supporting CLDR")
    (description
     "Plugs supporting CLDR and setting the locale from requests and request headers.")
    (home-page "https://hexdocs.pm/ex_cldr_plugs/")
    (license license:asl2.0)))

(define-public elixir-exjsx
  (package
    (name "elixir-exjsx")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "exjsx" version))
       (sha256
        (base32 "01rfr1har8akbwwnsba4a248hfym5955348fhdkymzvwm4h5is9j"))))
    (build-system mix-build-system)
    (propagated-inputs (list erlang-jsx))
    (synopsis "JSON for Elixir")
    (description "This package provides @code{exjsx}, a JSON library for
Elixir.")
    (home-page "https://hexdocs.pm/exjsx/")
    (license license:expat)))

(define-public elixir-hpack
  (package
    (name "elixir-hpack")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "hpack" version))
       (sha256
        (base32 "1gh4p21mlnnbprh7fxk03i53lmbdqsgi0my3r9sv9ivl9i6nm0rq"))))
    (build-system mix-build-system)
    (synopsis
     "Implementation of the
@uref{https://http2.github.io/http2-spec/compression.html,HPack} protocol")
    (description
     "This package provides @code{elixir-hpack}, an implementation of the
@uref{https://http2.github.io/http2-spec/compression.html,HPack} protocol: a
compression format for efficiently representing HTTP header fields, to be used
in HTTP/2.")
    (home-page "https://hexdocs.pm/hpack/")
    (license license:expat)))

(define-public elixir-hpax
  (package
    (name "elixir-hpax")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "hpax" version))
       (sha256
        (base32 "0jpkgn2abg0pr1anw7p9v3dkbbwlx243b92brv11hncdz8f6xawf"))))
    (build-system mix-build-system)
    (native-inputs
     (list erlang-coveralls
           elixir-excoveralls))
    (propagated-inputs
     (list elixir-castore
           elixir-hpack
           elixir-stream-data))
    (synopsis "Implementation of the HPACK protocol (RFC 7541) for Elixir")
    (description "This package provides @code{elixir-hpax}, an implementation of
the HPACK protocol (RFC 7541) for Elixir.")
    (home-page "https://hexdocs.pm/hpax/")
    (license license:asl2.0)))

(define-public elixir-httparrot
  (package
    (name "elixir-httparrot")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edgurgel/httparrot.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0grbqr41c6lf34k8q69v5ki6mlxwwkvgpjv5l7gmwfb8jmwbn6p7"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-earmark erlang-meck))
    (propagated-inputs
     (list elixir-con-cache erlang-cowboy elixir-exjsx))
    (synopsis "HTTP Request & Response Server")
    (description "HTTP server built on top of Cowboy using (mostly)
@code{cowboy_rest} handlers to serve useful endpoints for testing
purposes.  Its goal is to be as close as possible to
@uref{http://httpbin.org, HTTPBin}.")
    (home-page "https://hexdocs.pm/httparrot/")
    (license license:expat)))

(define-public elixir-httpoison
  (package
    (name "elixir-httpoison")
    (version "2.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edgurgel/httpoison")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k9g4dc6y30wr9ryxjs23izifpg4dqkkqk8xz39ff27jn5s97k8i"))
       ;; Waiting for upstream inclusion at
       ;; https://github.com/edgurgel/httpoison/pull/502
       (patches
        (search-patches "elixir-httpoison-tag-network-dependent-test-cases.patch"))))
    (build-system mix-build-system)
    (arguments
     (list
      #:test-flags
      ;; These tests require network access to badssl.com.
      #~(list "--exclude" "network")))
    (native-inputs
     (list erlang-cowboy
           elixir-earmark
           elixir-jason
           elixir-httparrot
           elixir-mimic))
    (propagated-inputs (list erlang-hackney))
    (synopsis "Yet Another HTTP client for Elixir")
    (description "Yet Another HTTP client for Elixir powered by hackney.")
    (home-page "https://hexdocs.pm/httpoison/")
    (license license:expat)))

(define-public elixir-iconify-ex
  (package
    (name "elixir-iconify-ex")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "iconify_ex" version))
       (sha256
        (base32 "1j6vzp6ayx278s6q7n4rdk849b23sx9wf040h336naxphqxm3qk6"))))
    (build-system mix-build-system)
    (propagated-inputs
     (list elixir-arrows
           elixir-emote
           elixir-floki
           elixir-jason
           elixir-phoenix-live-favicon
           elixir-phoenix-live-view
           elixir-recase
           elixir-surface
           elixir-untangle))
    (synopsis
     "Phoenix helpers for using icon sets")
    (description
     "This package provides Phoenix helpers for using the 100,000+ SVG icons
from 100+ icon sets from @uref{https://iconify.design, iconify.design}.")
    (home-page "https://hexdocs.pm/iconify_ex/")
    (license license:expat)))

(define-public elixir-mint-web-socket
  (package
    (name "elixir-mint-web-socket")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "mint_web_socket" version))
       (sha256
        (base32 "14d9c2ryqggl2p54bh0rhm3ab17j6l7ir817rsh4nnn455alqz82"))))
    (build-system mix-build-system)
    (arguments
     ;; Tests depend on gun from hex.pm which is not packaged yet.
     (list #:tests? #f))
    (native-inputs (list elixir-jason erlang-cowboy))
    (propagated-inputs (list elixir-mint))
    (synopsis "WebSocket support for Mint")
    (description "HTTP/1 and HTTP/2 @code{WebSocket} support for Mint.")
    (home-page "https://hexdocs.pm/mint_web_socket/")
    (license license:asl2.0)))

(define-public elixir-mint
  (package
    (name "elixir-mint")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "mint" version))
       (sha256
        (base32 "06r3kb9vkzpx8lgp6fjngaxz9hzidl8sw91hxvfh2hzjs2ja1szw"))))
    (build-system mix-build-system)
    (arguments
     ;; Tests depend on elixir-mox which is not packaged yet.
     (list #:tests? #f))
    (native-inputs
     (list elixir-excoveralls))
    (propagated-inputs (list elixir-castore elixir-hpax))
    (synopsis "Functional HTTP client for Elixir with support for HTTP/1 and
HTTP/2")
    (description "Mint is different from most Erlang and Elixir HTTP clients
because it provides a process-less architecture.  Instead, Mint is based on a
functional and immutable data structure that represents an HTTP connection.

This data structure wraps a TCP or SSL socket.  This allows for more
fine-tailored architectures where the developer is responsible for wrapping the
connection struct, such as having one process handle multiple connections or
having different kinds of processes handle connections.")
    (home-page "https://hexdocs.pm/mint/")
    (license license:asl2.0)))

(define-public elixir-neuron
  (package
    (name "elixir-neuron")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "neuron" version))
       (sha256
        (base32 "1kmnhlihpv1075i3f5izysx2vdgqw71lnnxw8yifxh6r1l7dpk93"))))
    (build-system mix-build-system)
    (arguments
     ;; Tests depend on Coverex, which is not packaged yet.
     (list #:tests? #f))
    (propagated-inputs (list elixir-httpoison elixir-jason))
    (synopsis "GraphQL client for Elixir")
    (description "This package provides a @code{GraphQL} client for Elixir.")
    (home-page "https://hexdocs.pm/neuron/")
    (license license:isc)))

(define-public elixir-phoenix-ecto
  (package
    (name "elixir-phoenix-ecto")
    (version "4.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix_ecto" version))
       (sha256
        (base32 "0jwdybzrsbrxjqsqw5pzzf2x8vsw0i2d1bccfhdz61zlxq435v16"))))
    (build-system mix-build-system)
    (propagated-inputs
     (list elixir-ecto
           elixir-phoenix-html
           elixir-plug
           elixir-postgrex))
    (synopsis "Phoenix and Ecto integration with support for concurrent
acceptance testing")
    (description "This package provides a library that integrates Phoenix with
Ecto, implementing all relevant protocols.")
    (home-page "https://hexdocs.pm/phoenix_ecto/")
    (license license:expat)))

(define-public elixir-phoenix-html
  (package
    (name "elixir-phoenix-html")
    (version "4.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix_html" version))
       (sha256
        (base32 "0lz04g7h49x9rg4k3l6zz29va84fxy62m3xfb7cmswg21880iwfg"))))
    (build-system mix-build-system)
    (synopsis "Phoenix view functions for working with HTML templates")
    (description "This package provides @code{elixir-phoenix-html}, a library
implementing view functions for working with HTML templates in the Phoenix
framework.")
    (home-page "https://hexdocs.pm/phoenix_html/")
    (license license:expat)))

(define-public elixir-phoenix-live-favicon
  (package
    (name "elixir-phoenix-live-favicon")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix_live_favicon" version))
       (sha256
        (base32 "17m7gpx6qw0cr0d2acwajjhz2qzfi2zywzyk2i139y9higb22q8k"))))
    (build-system mix-build-system)
    (arguments
     ;; Tests depend on makeup_diff from hex.pm but it is not packaged yet.
     (list #:tests? #f))
    (propagated-inputs (list elixir-phoenix-live-head))
    (synopsis "Favicon manipulation for Phoenix Live Views")
    (description "This package provides a lib enabling dynamic favicons in
Phoenix Live View applications.")
    (home-page "https://hexdocs.pm/phoenix_live_favicon/")
    (license license:expat)))

(define-public elixir-phoenix-live-head
  (package
    (name "elixir-phoenix-live-head")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix_live_head" version))
       (sha256
        (base32 "060bbni63v4jqyiqrykcl2sdra3brhvr15d1q7wg1c48cki01ah3"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-excoveralls))
    (propagated-inputs
     (list elixir-ex-doc elixir-jason elixir-phoenix
           elixir-phoenix-html elixir-phoenix-live-view))
    (synopsis "HTML Head manipulation for Phoenix Live Views")
    (description "This package provides commands for manipulating the HTML Head
of Phoenix Live View applications while minimizing data over the wire.

The available command actions support a variety of utility operations useful for
HTML Head manipulation.  Such as setting or removing tag attributes and adding
or removing CSS classes.

A special feature is the use of the @code{@{dynamic@}} tag in values.  This
saves data over the wire by only sending the dynamic part of an attributes
value.")
    (home-page "https://hexdocs.pm/phoenix_live_head/")
    (license license:expat)))

(define-public elixir-phoenix-live-reload
  (package
    (name "elixir-phoenix-live-reload")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix_live_reload" version))
       (sha256
        (base32 "1875i11fl7nr1hx645cjwq43ziiy7vrrjxf1pgqgxr56sm1kh9vl"))))
    (build-system mix-build-system)
    (arguments
     ;; Tests fail with:
     ;;
     ;; [warning] Could not start Phoenix live-reload because we cannot listen
     ;; to the file system.
     ;; You don't need to worry! This is an optional feature used during
     ;; development to refresh your browser when you save files and it does not
     ;; affect production.
     (list #:tests? #f))
    (propagated-inputs (list elixir-file-system elixir-phoenix))
    (synopsis "Live-reload functionality for Phoenix")
    (description
     "This package provides live-reload functionality for Phoenix.")
    (home-page "https://hexdocs.pm/phoenix_live_reload/")
    (license license:expat)))

(define-public elixir-phoenix-live-view
  (package
    (name "elixir-phoenix-live-view")
    (version "1.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix_live_view" version))
       (sha256
        (base32 "1v7p6q0lz9fzfyj9w4kwbyqi1vqdzsdllaw3xvw9whdqfcsyj1lx"))))
    (build-system mix-build-system)
    (arguments
     ;; Tests fail with:
     ;;
     ;; [warning] Could not start Phoenix live-reload because we cannot listen
     ;; to the file system.
     ;; You don't need to worry! This is an optional feature used during
     ;; development to refresh your browser when you save files and it does not
     ;; affect production.
     (list #:tests? #f))
    (native-inputs
     (list elixir-html-entities
           elixir-phoenix-live-reload))
    (propagated-inputs
     (list elixir-floki
           elixir-jason
           elixir-phoenix
           elixir-phoenix-html
           elixir-phoenix-template
           elixir-phoenix-view
           elixir-plug
           erlang-telemetry))
    (synopsis "Rich, real-time user experiences with server-rendered HTML")
    (description "LiveView brings a unified experience to building web
applications.  You no longer have to split work between client and server,
across different toolings, layers, and abstractions.  Instead, LiveView
enriches the server with a declarative and powerful model while keeping your
code closer to your data (and ultimately your source of truth):

@itemize
@item Declarative server side rendering
@item Rich templating language
@item Diffs over the wire
@item Live form validation
@item File uploads with progress indicators
@item Optimistic updates and transitions
@item Live navigation
@item Latency simulator
@end itemize")
    (home-page "https://hexdocs.pm/phoenix_live_view/")
    (license license:expat)))

(define-public elixir-phoenix-pubsub
  (package
    (name "elixir-phoenix-pubsub")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix_pubsub" version))
       (sha256
        (base32 "00p5dvizhawhqbia2cakdn4whaxsm2adq3lzfn3b137xvk0np85v"))))
    (build-system mix-build-system)
    (arguments
     ;; Tests require network
     (list #:tests? #f))
    (synopsis "Distributed PubSub and Presence platform")
    (description "This package provides @code{elixir-phoenix-pubsub}, a library
implementing a distributed @code{PubSub} and @code{Presence} platform for the
Phoenix Framework.")
    (home-page "https://hexdocs.pm/phoenix_pubsub/")
    (license license:expat)))

(define-public elixir-phoenix-template
  (package
    (name "elixir-phoenix-template")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix_template" version))
       (sha256
        (base32 "01j28jf0q1h5pk9ndf7s6jx9m489jyf24byabjpkyxf6wpq8231c"))))
    (build-system mix-build-system)
    (native-inputs (list elixir-jason))
    (propagated-inputs (list elixir-phoenix-html))
    (synopsis "Template rendering for Phoenix")
    (description "This module provides functions for loading and compiling
templates from disk. A markup language is compiled to Elixir code via an
engine.")
    (home-page "https://hexdocs.pm/phoenix_template/")
    (license license:expat)))

(define-public elixir-phoenix-view
  (package
    (name "elixir-phoenix-view")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix_view" version))
       (sha256
        (base32 "02v2xd1h415kbf1ncclqx6acrbslh6i2gnrmfgjizwqlrqi216af"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-jason))
    (propagated-inputs (list elixir-phoenix-html elixir-phoenix-template))
    (synopsis "View layer in Phoenix v1.0-v1.6 apps")
    (description "Within LiveView and HTML apps, @code{Phoenix.View} has fallen
out of fashion in favor of @code{Phoenix.Component}.  See the \"Replaced by
@code{Phoenix.Component}\" section in the @code{Phoenix.View} module
documentation for more information and migration steps.

If you want to render other formats, such as XML, @code{Phoenix.View} may still
be a useful addition to your projects.")
    (home-page "https://hexdocs.pm/phoenix_view/")
    (license license:expat)))

(define-public elixir-phoenix
  (package
    (name "elixir-phoenix")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phoenix" version))
       (sha256
        (base32 "1cigrl8myx2079mns0ww2k6n11awxw0vv6bhabkwghvp5qmpvmw4"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-mint
           elixir-mint-web-socket
           elixir-phx-new))
    (propagated-inputs
     (list elixir-castore
                  elixir-jason
                  elixir-phoenix-pubsub
                  elixir-phoenix-html
                  elixir-phoenix-template
                  elixir-phoenix-view
                  elixir-plug
                  elixir-plug-cowboy
                  elixir-plug-crypto
                  erlang-telemetry
                  elixir-websock-adapter))
    (synopsis "Web development framework")
    (description "Phoenix is a web development framework written in Elixir
which implements the server-side Model View Controller (MVC) pattern.  Many of
its components and concepts will seem familiar to those with experience in other
web frameworks like Ruby on Rails or Python's Django.")
    (home-page "https://www.phoenixframework.org/")
    (license license:expat)))

(define-public elixir-phx-new
  (package
    (name "elixir-phx-new")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "phx_new" version))
       (sha256
        (base32 "05kv8i0w8f1ky6gx3rz0ps5mwp8nkln7dq6565zw660i3cxpnwzn"))))
    (build-system mix-build-system)
    (synopsis
     "Phoenix framework project generator")
    (description
     "Phoenix framework project generator.  Provides a @command{mix phx.new}
task to bootstrap a new Elixir application with Phoenix dependencies.")
    (home-page "https://hexdocs.pm/phx_new/")
    (license license:expat)))

(define-public elixir-plug-crypto
  (package
    (name "elixir-plug-crypto")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "plug_crypto" version))
       (sha256
        (base32 "0z1424zcywdg47wsbvhmdbrngbz4lzhzsbv1jza8n774zzkbqw34"))))
    (build-system mix-build-system)
    (synopsis "Crypto-related functionality for the web")
    (description "This package provides @code{elixir-plug-crypto}, a library
implementing crypto-related functionality for the web, used by Plug.")
    (home-page "https://hexdocs.pm/plug_crypto/")
    (license license:asl2.0)))

(define-public elixir-plug-cowboy
  (package
    (name "elixir-plug-cowboy")
    (version "2.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "plug_cowboy" version))
       (sha256
        (base32 "18wa2qiy18g9s61anb5krcmmvcmivs201mx5w2x1a9h1swmn71cv"))))
    (build-system mix-build-system)
    (arguments
     (list
      #:tests? #f))
    (native-inputs
     (list elixir-x509 erlang-hackney))
    (propagated-inputs (list erlang-cowboy erlang-cowboy-telemetry elixir-plug))
    (synopsis "A Plug adapter for Cowboy")
    (description "This package provides a Plug adapter for Cowboy.")
    (home-page "https://hexdocs.pm/plug_cowboy/")
    (license license:asl2.0)))

(define-public elixir-plug
  (package
    (name "elixir-plug")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "plug" version))
       (sha256
        (base32 "1hkcahx1l6yi1hcxn7lw6cbcscwdrwrjsza3cjsj5d7j1nvpv9ap"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-mime elixir-plug-crypto erlang-telemetry))
    (synopsis "Compose web applications with functions")
    (description "Plug is:

@itemize
@item A specification for composing web applications with functions
@item Connection adapters for different web servers in the Erlang VM
@end itemize

In other words, Plug allows you to build web applications from small pieces and
run them on different web servers.  Plug is used by web frameworks such as
Phoenix to manage requests, responses, and websockets.  This documentation will
show some high-level examples and introduce the Plug's main building blocks.")
    (home-page "https://hexdocs.pm/plug/")
    (license license:asl2.0)))

(define-public elixir-surface
  (package
    (name "elixir-surface")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "surface" version))
       (sha256
        (base32 "04yl1k13sd22iwlf90l46dd7a09x3wq0g1iq6caw9y9p4ljl4chk"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-ecto
           elixir-floki
           elixir-jason
           elixir-phoenix-ecto
           elixir-sourceror))
    (propagated-inputs
     (list elixir-phoenix-html
           elixir-phoenix-live-view
           elixir-sourceror))
    (synopsis "Server-side rendering component library for Phoenix")
    (description
     "This package provides Surface is a server-side rendering component library
that allows developers to build rich interactive user-interfaces, writing
minimal custom JavaScript.")
    (home-page "https://surface-ui.org")
    (license license:expat)))

;; This package lives here to avoid module level circular dependencies as it
;; depends on elixir-mint.
(define-public elixir-tz
  (package
    (name "elixir-tz")
    (version "0.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "tz" version))
       (sha256
        (base32 "12lchkhdxbv8ai09i2lsy394yx1xrfq1yz5p8dn3qr0236ma3p5z"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-castore elixir-mint))
    (synopsis "Time zone support for Elixir")
    (description "Time zone support for Elixir.")
    (home-page "https://hexdocs.pm/tz/")
    (license license:asl2.0)))

(define-public elixir-websock-adapter
  (package
    (name "elixir-websock-adapter")
    (version "0.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "websock_adapter" version))
       (sha256
        (base32 "10ip7zyvb5nb9qyl9vmw8npk3kk79qcssh2iyfsi48jmclc9lnri"))))
    (build-system mix-build-system)
    (propagated-inputs
     (list elixir-bandit
           elixir-plug
           elixir-plug-cowboy
           elixir-websock))
    (synopsis "WebSock adapters for common web servers")
    (description
     "This package provides a set of @code{WebSock} adapters for common web servers.")
    (home-page "https://hexdocs.pm/websock_adapter/")
    (license license:expat)))

(define-public elixir-websock
  (package
    (name "elixir-websock")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "websock" version))
       (sha256
        (base32 "0lxlp1h18595nqczfg15iy34kw5xbbab3yk6ml9cf8mcgwyla1b1"))))
    (build-system mix-build-system)
    (synopsis "Specification for WebSocket connections")
    (description
     "This package provides a specification for @code{WebSocket} connections.")
    (home-page "https://hexdocs.pm/websock/")
    (license license:expat)))

(define-public elixir-x509
  (package
    (name "elixir-x509")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "x509" version))
       (sha256
        (base32 "1iyg91719bkxpps3l97aj2hd67xvf4xlrq2v1x5msmkyd5sxwpjc"))))
    (build-system mix-build-system)
    (synopsis
     "Elixir package for working with X.509 certificates")
    (description
     "Elixir package for working with X.509 certificates, Certificate Signing Requests
(CSRs), Certificate Revocation Lists (CRLs) and RSA/ECC key pairs.")
    (home-page "https://hexdocs.pm/x509/")
    (license license:bsd-3)))
