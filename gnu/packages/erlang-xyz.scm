;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Igor Goryachev <igor@goryachev.org>
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

(define-public erlang-luerl
  (package
    (name "erlang-luerl")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "luerl" version))
       (sha256
        (base32 "0paj3gr0kn8v9g6wmdnz1f16q0iy1pb83zbqsalzbw2g17vd9bww"))))
    (build-system rebar-build-system)
    (synopsis "Implementation of Lua on Erlang")
    (description "This package provides implementation of Lua on Erlang.")
    (home-page "https://hex.pm/packages/luerl")
    (license license:asl2.0)))

(define-public erlang-jose
  (package
    (name "erlang-jose")
    (version "1.11.10")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "jose" version))
       (sha256
        (base32 "0576jdjygby37qmzrs8cm5l6n622b0mi3z28j6r4s5xsz1px6v0d"))))
    (build-system rebar-build-system)
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
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "jiffy" version))
       (sha256
        (base32 "10gkbi48in96bzkv7f2cqw9119krpd40whcsn0yd7fr0lx1bqqdv"))))
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
    (version "1.0.23")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_acme" version))
       (sha256
        (base32 "0nfxs3ldgbh7a8r7wxl0gw98r1v50jaj54c0nw8fl8ixdvr9dqcc"))))
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
    (version "1.0.26")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_utils" version))
       (sha256
        (base32 "0k5hfqrjrvgh7gknqj5pzpra9k1g0bgc2ac19zb8pfan2669wdyh"))))
    (build-system rebar-build-system)
    (synopsis "ProcessOne utility modules for Erlang")
    (description "This package provides ProcessOne utility modules for Erlang.")
    (home-page "https://hex.pm/packages/p1_utils")
    (license license:asl2.0)))

(define-public erlang-p1-mysql
  (package
    (name "erlang-p1-mysql")
    (version "1.0.24")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_mysql" version))
       (sha256
        (base32 "0pv5zfpkw6sgmn6jbm5k635xnx8kkcvgynka5ix50zr5cigqcn7h"))))
    (build-system rebar-build-system)
    (synopsis "Pure Erlang MySQL driver")
    (description "This package provides pure Erlang @code{MySQL} driver.")
    (home-page "https://hex.pm/packages/p1_mysql")
    (license license:asl2.0)))

(define-public erlang-p1-pgsql
  (package
    (name "erlang-p1-pgsql")
    (version "1.1.27")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "p1_pgsql" version))
       (sha256
        (base32 "1fgzchikv31gbyid3qqy9wzwmap1nalwb7fqal77hs6b09v1lkcf"))))
    (build-system rebar-build-system)
    (inputs (list erlang-xmpp))
    (synopsis "PostgreSQL driver for Erlang")
    (description "This package provides @code{PostgreSQL} driver for Erlang.")
    (home-page "https://hex.pm/packages/p1_pgsql")
    (license license:asl2.0)))

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
    (version "1.0.30")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "stringprep" version))
       (sha256
        (base32 "16hb49f9ghx8sqrs7928fjig9wyah22z7cl91y1pff50hhrrpz7n"))))
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

(define-public erlang-cache-tab
  (package
    (name "erlang-cache-tab")
    (version "1.0.31")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cache_tab" version))
       (sha256
        (base32 "0lb35g3p3ybij5f7fnilq3ni37nfgzhajnrmhvplgch9985bd0l5"))))
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
    (version "1.0.23")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "eimp" version))
       (sha256
        (base32 "12k2df0mwq16rh77ziiahq9mjd9cy2jfdnsbzkj96a6b4c07hz4h"))))
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
    (version "1.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "mqtree" version))
       (sha256
        (base32 "18z23c1axn6y9799f0ydb0m29wxvq956b57bzk886ixwiz7vgs2z"))))
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
    (version "1.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ezlib" version))
       (sha256
        (base32 "1fxz584ry1ib35r74hbmapwq71j5iv5rymm927ys0mgdz2rjmrly"))))
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
    (version "1.1.21")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "fast_tls" version))
       (sha256
        (base32 "0nsh5597pa1643kj2mmp05anss2r0gq83al0rm45w0ip768l458k"))))
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
    (version "1.2.14")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "stun" version))
       (sha256
        (base32 "10qm6wh6smj3ixpy251x5w2b9iv51vhgxvk49vczz3bs3dxq0d71"))))
    (build-system rebar-build-system)
    (inputs (list erlang-fast-tls erlang-p1-utils))
    (synopsis "STUN and TURN library for Erlang/Elixir")
    (description "This package provodes STUN and TURN library for
Erlang/Elixir.")
    (home-page "https://hex.pm/packages/stun")
    (license license:asl2.0)))

(define-public erlang-fast-xml
  (package
    (name "erlang-fast-xml")
    (version "1.1.52")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "fast_xml" version))
       (sha256
        (base32 "1z8vrpnjx8y9qfqhhj4is8k4lzvjlnzhp6d62r8bdlh61qwr4lbr"))))
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

(define-public erlang-xmpp
  (package
    (name "erlang-xmpp")
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "xmpp" version))
       (sha256
        (base32 "04gpka0dp03jvgnin1g12ha4yzwy1h44rds3zzf1ia49k9ghcw7d"))))
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
    (version "1.0.54")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "esip" version))
       (sha256
        (base32 "1vip49gbz1z6sgrab6021fdlqq7g7712d5znvanwsnbjkn0sz1w1"))))
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
    (version "1.0.37")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "fast_yaml" version))
       (sha256
        (base32 "0sd72nal5i6mbmicsmb494mr4g0gvs719lzp2hj1gqpp3dr6is4d"))))
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
    (version "1.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "yconf" version))
       (sha256
        (base32 "12bcb0w0nc30bws4lx2ghnz7n755r6nf9a75nb3i31zkfcr82iz9"))))
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
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "unicode_util_compat" version))
       (sha256
        (base32 "08952lw8cjdw8w171lv8wqbrxc4rcmb3jhkrdb7n06gngpbfdvi5"))))
    (build-system rebar-build-system)
    (synopsis "Compatibility library for Erlang < 20")
    (description "This package provides @code{unicode_util} compatibility
library for Erlang < 20.")
    (home-page "https://hex.pm/packages/unicode_util_compat")
    (license license:asl2.0)))

(define-public erlang-idna
  (package
    (name "erlang-idna")
    (version "6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "idna" version))
       (sha256
        (base32 "1sjcjibl34sprpf1dgdmzfww24xlyy34lpj7mhcys4j4i6vnwdwj"))))
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
