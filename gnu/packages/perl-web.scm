;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages perl-web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (guix download)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages web))

(define-public perl-mojolicious
  (package
    (name "perl-mojolicious")
    (version "9.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SR/SRI/Mojolicious-"
                           version ".tar.gz"))
       (sha256
        (base32
         "13dxjhr03dhh1f5bbxbb3jiwdv7jby96qqb97l3arf5x043yd9hd"))))
    (build-system perl-build-system)
    (home-page "https://mojolicious.org/")
    (synopsis "Real-time web framework")
    (description "Back in the early days of the web, many people learned Perl
because of a wonderful Perl library called @code{CGI}.  It was simple enough
to get started without knowing much about the language and powerful enough to
keep you going, learning by doing was much fun.  While most of the techniques
used are outdated now, the idea behind it is not.  Mojolicious is a new
endeavor to implement this idea using modern technologies.")
    (license license:artistic2.0)))

(define-public perl-uri-db
  (package
    (name "perl-uri-db")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DW/DWHEELER/URI-db-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0n56xxlw7c39pfar0dxckr9mbmp6yrzk53ic0cb24raiykm9v6f4"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-uri perl-uri-nested))
    (home-page "https://metacpan.org/release/URI-db")
    (synopsis "Handle database URIs")
    (description
     "This module defines a format for database URIs, and provides a @{URI}
class to handle these.")
    (license license:perl-license)))

(define-public perl-uri-escape
  (package
    (name "perl-uri-escape")
    (version "1.76")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OA/OALDERS/URI-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0gj1aj18k43kmzc3y1zhj5giinf8rksacf757r475xfna0fqxjdj"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-needs))
    (home-page "https://github.com/libwww-perl/URI")
    (synopsis "Percent-encode and percent-decode unsafe characters")
    (description "This module provides functions to percent-encode and
percent-decode URI strings as defined by RFC 3986.  Percent-encoding URI's is
informally called URI escaping.  This is the terminology used by this module,
which predates the formalization of the terms by the RFC by several years.")
    (license license:perl-license)))

(define-public perl-uri-nested
  (package
    (name "perl-uri-nested")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DW/DWHEELER/URI-Nested-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1bzg6f11m8wfnmycflvp858rs99xknsx8hkip0xcdfjzlqwi75z1"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-uri))
    (home-page "https://metacpan.org/release/URI-Nested")
    (synopsis "Nested URIs")
    (description
     "@code{URI::Nested} provides support for nested URIs, where the scheme is
a prefix, and the remainder of the URI is another URI.")
    (license license:perl-license)))

(define-public perl-mojo-dom58
  (package
    (name "perl-mojo-dom58")
    (version "3.001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DB/DBOOK/Mojo-DOM58-"
                           version ".tar.gz"))
       (sha256
        (base32 "15jp9wiczrnvdx8cld0jn43bmj20v5ll6cfzvmd4252k3ra6vchq"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-role-tiny))
    (home-page "https://metacpan.org/release/Mojo-DOM58")
    (synopsis "Minimalistic HTML/XML DOM parser with CSS selectors")
    (description "This package provides a HTML and XML DOM parser.  It also
supports CSS selectors.")
    (license license:artistic2.0)))

(define-public perl-gravatar-url
 (package
    (name "perl-gravatar-url")
    (version "1.07")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MS/MSCHWERN/Gravatar-URL-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1mxnd70xsb7kxd1sf8s7phcds2nf0m8b8asmy3ikv76wzfpbvji3"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build
                         perl-test-mockrandom
                         perl-test-warn))
    (propagated-inputs (list perl-net-dns perl-uri))
    (home-page "https://metacpan.org/release/Gravatar-URL")
    (synopsis "Make URLs for Gravatars from an email address")
    (description
     "This module looks up the Gravatar for any email address by constructing
a URL to get the image from @url{gravatar.com}.  A Gravatar is a Globally
Recognized Avatar for a given email address. This allows you to have a global
picture associated with your email address.")
    (license license:perl-license)))

(define-public perl-io-sessiondata
  (package
    (name "perl-io-sessiondata")
    (version "1.03")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/P/PH/PHRED/IO-SessionData-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1p9d77pqy9a8dbgw7h7vmmkg0rlckk19dchd4c8gvcyv7qm73934"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/IO-SessionData")
    (synopsis "Supporting module for SOAP::Lite")
    (description
     "Package extracted from SOAP::Lite.  Slightly modified to work on
multiple platforms.")
    (license license:perl-license)))

(define-public perl-soap-lite
  (package
    (name "perl-soap-lite")
    (version "1.27")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/P/PH/PHRED/SOAP-Lite-"
                    version ".tar.gz"))
              (sha256
               (base32
                "00fkvmnxiy5mr45rj5qmxmflw0xdkw2gihm48iha2i8smdmi0ng3"))))
    (build-system perl-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch
                          (lambda _
                            (substitute* "t/Issues/rt53375.t"
                              (("XML::Parser::Lite")
                               "XML::Parser")))))))
    (native-inputs (list perl-test-most perl-test-warn))
    (propagated-inputs (list perl-class-inspector
                             perl-constant
                             perl-io-compress
                             perl-io-sessiondata
                             perl-io-socket-ssl
                             perl-libwww
                             perl-lwp-protocol-https
                             perl-mime-base64
                             perl-scalar-list-utils
                             perl-task-weaken
                             perl-uri
                             perl-xml-parser))
    (home-page "https://metacpan.org/release/SOAP-Lite")
    (synopsis "Client/server interface to the Simple Object Access Protocol")
    (description
     "@code{SOAP::Lite} is a collection of Perl modules that provide a
simple and lightweight interface to the
@acronym{SOAP, Simple Object Access Protocol}, both on client and server
side.")
    (license license:perl-license)))
