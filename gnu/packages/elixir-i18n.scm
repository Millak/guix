;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu packages elixir-i18n)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages elixir-markup)
  #:use-module (gnu packages elixir-xyz)
  #:use-module (guix build-system mix)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages))

(define-public elixir-cldr-utils
  (package
    (name "elixir-cldr-utils")
    (version "2.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cldr_utils" version))
       (sha256
        (base32 "1dgzaxfj0whv2rjkf57jnzkl63az50wypzjwcwnz31yilpckq220"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-castore erlang-certifi elixir-decimal))
    (synopsis
     "Helpers for @code{ex_cldr}")
    (description
     "@code{Map}, @code{Calendar}, @code{Digits}, @code{Decimal}, @code{HTTP},
@code{Macro}, @code{Math}, and @code{String} helpers for @code{ex_cldr}.")
    (home-page "https://hexdocs.pm/cldr_utils/")
    (license license:asl2.0)))

(define-public elixir-digital-token
  (package
    (name "elixir-digital-token")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "digital_token" version))
       (sha256
        (base32 "1rp9r5jb0db52f969jmgv5sp8k0vd98dnqwrgca0fyzsqalgbmlf"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-cldr-utils elixir-jason))
    (synopsis
     "ISO 24165 Digital Tokens")
    (description
     "Elixir integration for ISO 24165 Digital Tokens (crypto currencies) through the
DTIF registry data.")
    (home-page "https://hexdocs.pm/digital_token/")
    (license license:asl2.0)))

(define-public elixir-ex-cldr-minimal
  (package
    (name "elixir-ex-cldr-minimal")
    (version "2.43.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr" version))
       (sha256
        (base32 "0ai765853c2zjmrwni9j0753c8fv9431na93gpvnc488pnkkfl89"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-stream-data))
    (propagated-inputs
     (list elixir-cldr-utils elixir-decimal elixir-gettext elixir-jason
           elixir-nimble-parsec))
    (synopsis
     "Common Locale Data Repository (CLDR) functions for Elixir")
    (description
     "Common Locale Data Repository (CLDR) functions for Elixir to localize and format
numbers, dates, lists, messages, languages, territories and units with support
for over 700 locales for internationalized (i18n) and localized (L10N)
applications.")
    (home-page "https://hexdocs.pm/ex_cldr/")
    (license license:asl2.0)))

(define-public elixir-ex-cldr
  (package
    (inherit elixir-ex-cldr-minimal)
    (name "elixir-ex-cldr")
    (version "2.43.2")
    (source
     (origin
       ;; The hex.pm package ships only the en locale.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elixir-cldr/cldr.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wgwp74ya0gkvd5pk4qv9s38p85r5yw370y4wv7jywy3hrbrj9f5"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            ;; test That locales with no version are replaced with current version
            ;; test That locales with an old version are replaced with current version
            (lambda _
              (for-each delete-file '("test/locale_upgrade_test.exs")))))))))
