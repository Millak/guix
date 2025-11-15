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

(define-public elixir-ex-cldr-calendars
  (package
    (name "elixir-ex-cldr-calendars")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr_calendars" version))
       (sha256
        (base32 "0lxgii8mz86zdlyljq10yk6xr1h86apaw52z8b9b85jkzzi4zlj2"))))
    (build-system mix-build-system)
    (native-inputs
     (list elixir-stream-data))
    (propagated-inputs
     (list elixir-calendar-interval
           elixir-ex-cldr-lists
           elixir-ex-cldr-numbers
           elixir-ex-cldr-units
           elixir-ex-doc
           elixir-jason))
    (synopsis
     "Localized month and week-based calendars")
    (description
     "Localized month and week-based calendars and calendar functions based upon CLDR
data.")
    (home-page "https://hexdocs.pm/ex_cldr_calendars/")
    (license license:asl2.0)))

(define-public elixir-ex-cldr-currencies
  (package
    (name "elixir-ex-cldr-currencies")
    (version "2.16.5")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr_currencies" version))
       (sha256
        (base32 "1lrhiyi5ywcvpirf0k4qs6l9pwvijwix1bvqwafki9zh5281g5s3"))))
    (build-system mix-build-system)
    (propagated-inputs (list elixir-ex-cldr elixir-jason))
    (synopsis
     "Currency localization data encapsulation function")
    (description
     "Currency localization data encapsulation functions for the Common Locale Data
Repository (CLDR).")
    (home-page "https://hexdocs.pm/ex_cldr_currencies/")
    (license license:asl2.0)))

(define-public elixir-ex-cldr-lists
  (package
    (name "elixir-ex-cldr-lists")
    (version "2.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr_lists" version))
       (sha256
        (base32 "1qmdnk8am9dy6k4z93svq7lz288cwmi8aswsn4c3zjqca421q5h0"))))
    (build-system mix-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'override-mix-env
            (lambda _
              (symlink (string-append (getcwd) "/config/release.exs")
                       "config/prod.exs"))))))
    (propagated-inputs (list elixir-ex-cldr-numbers elixir-ex-doc elixir-jason))
    (synopsis
     "List formatting functions for @code{ex_cldr}.")
    (description
     "List formatting functions for the Common Locale Data Repository (CLDR) package
@code{ex_cldr}.")
    (home-page "https://hexdocs.pm/ex_cldr_lists/")
    (license license:asl2.0)))

(define-public elixir-ex-cldr-locale-display
  (package
    (name "elixir-ex-cldr-locale-display")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr_locale_display" version))
       (sha256
        (base32 "05kh2320r2yc8illlzvn43czbfx01zdpfdwnzf90fjn31ckw69nn"))))
    (build-system mix-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'override-mix-env
            (lambda _
              (symlink (string-append (getcwd) "/config/release.exs")
                       "config/prod.exs"))))))
    (propagated-inputs
     (list elixir-ex-cldr
           elixir-ex-cldr-currencies
           elixir-ex-cldr-territories
           elixir-jason))
    (synopsis
     "Locale display name presentation")
    (description
     "Locale display name presentation for Common Locale Data Repository (CLDR)
locales.")
    (home-page "https://hexdocs.pm/ex_cldr_locale_display/")
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

(define-public elixir-ex-cldr-numbers
  (package
    (name "elixir-ex-cldr-numbers")
    (version "2.35.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr_numbers" version))
       (sha256
        (base32 "1pan719k511ya67s7vldz3djry5m6vsb4vynkfggwznyly0zrdbd"))))
    (build-system mix-build-system)
    (propagated-inputs
     (list elixir-decimal elixir-digital-token elixir-ex-cldr
           elixir-ex-cldr-currencies elixir-jason))
    (synopsis
     "Number and currency localization and formatting")
    (description
     "Number and currency localization and formatting functions for the Common Locale
Data Repository (CLDR).")
    (home-page "https://hexdocs.pm/ex_cldr_numbers/")
    (license license:asl2.0)))

(define-public elixir-ex-cldr-territories
  (package
    (name "elixir-ex-cldr-territories")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr_territories" version))
       (sha256
        (base32 "0gyka7fyfcl959x20xzilzf7pzifj9r1j7hh33a80dm2b7h0mv4k"))))
    (build-system mix-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'override-mix-env
            (lambda _
              (symlink (string-append (getcwd) "/config/release.exs")
                       "config/prod.exs"))))))
    (propagated-inputs (list elixir-ex-cldr elixir-jason))
    (synopsis
     "Terrritory formatting functions")
    (description
     "Terrritory formatting functions for the Common Locale Data Repository (CLDR)
package @code{ex_cldr}.")
    (home-page "https://hexdocs.pm/ex_cldr_territories/")
    (license license:expat)))

(define-public elixir-ex-cldr-units
  (package
    (name "elixir-ex-cldr-units")
    (version "3.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ex_cldr_units" version))
       (sha256
        (base32 "1y7i5gwa1d5p11dgyv9il9ii4yhjf79c4l0gwpcmkh8ybip3n0k3"))))
    (build-system mix-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'override-mix-env
            (lambda _
              (symlink (string-append (getcwd) "/config/release.exs")
                       "config/prod.exs"))))))
    (propagated-inputs
     (list elixir-cldr-utils
           elixir-decimal
           elixir-ex-cldr-lists
           elixir-ex-cldr-numbers
           elixir-ex-doc
           elixir-jason))
    (synopsis
     "Unit formatting (volume, area, length), conversion and arithmetic")
    (description
     "Unit formatting (volume, area, length), conversion and arithmetic functions
based upon the Common Locale Data Repository (CLDR).")
    (home-page "https://hexdocs.pm/ex_cldr_units/")
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
