;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Wilko Meyer <w@wmeyer.eu>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system perl)
  #:use-module (guix download)
  #:use-module (guix gexp))

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
     "This module defines a format for database URIs, and provides a @code{URI}
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

(define-public perl-www-pastebin-pastebincom-create
  (package
    (name "perl-www-pastebin-pastebincom-create")
    (version "1.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/Z/ZO/ZOFFIX/WWW-Pastebin-PastebinCom-Create-"
             version ".tar.gz"))
       (sha256
        (base32 "0xs29hilmlr51hcg4azbvi7c30yivan5gzwiq2z0dwrdy13lvcln"))))
    (build-system perl-build-system)
    ;; TODO: tests currently fail, disabled for now
    (arguments
     (list #:tests? #f))
    (native-inputs (list perl-module-build))
    (propagated-inputs (list perl-moo perl-www-mechanize))
    (home-page "https://metacpan.org/release/WWW-Pastebin-PastebinCom-Create")
    (synopsis "Paste on Pastebin without API keys")
    (description
     "@code{WWW::Pastebin::PastebinCom::Create} provides functionality to
paste on @url{https://www.pastebin.com, Pastebin} without API keys.")
    (license license:artistic2.0)))

(define-public geolite-country-data
  ;; TODO: Figure out how to get an updated, free database.
  (hidden-package
   (package
     (name "geolite-country-data")
     (version "2019-12-06")
     (source (origin
               (method url-fetch)
               (uri (string-append
                     "https://web.archive.org/web/20181229152721/"
                     "http://geolite.maxmind.com/download/geoip/database/"
                     "GeoLiteCountry/" "GeoIP.dat.gz"))
               (sha256
                (base32
                 "0j84ms2x893cpn7x8gffy082gnx882pmr0f6zpfsd46gpyw5xh5r"))))
     (build-system copy-build-system)
     (arguments
      (list #:install-plan #~'(("GeoIP.dat" "share/GeoIP/"))))
     (home-page "https://dev.maxmind.com/geoip/geolite2-free-geolocation-data")
     (synopsis "Country-level GeoIP data (December 2018)")
     (description
      "This package provides an old GeoIP database in the legacy @file{.dat}
format.  The data was collected before MaxMind changed the license and format
of their databases.  It is intended only as a compatibility package for
SpamAssassin.")
     (license license:cc-by-sa4.0))))

(define-public perl-geo-ip
  (package
    (name "perl-geo-ip")
    (version "1.51")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MA/MAXMIND/Geo-IP-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1fka8fr7fw6sh3xa9glhs1zjg3s2gfkhi7n7da1l2m2wblqj0c0n"))))
    (build-system perl-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch
                          (lambda _
                            (substitute* "lib/Geo/IP.pm"
                              (("/usr/local/share/GeoIP/GeoIP.dat")
                               (string-append
                                #$(this-package-input "geolite-country-data")
                                "/share/GeoIP/GeoIP.dat"))))))))
    (inputs (list geolite-country-data))
    (home-page "https://metacpan.org/release/Geo-IP")
    (synopsis "Look up location and network information by IP Address")
    (description
     "The Perl module @code{Geo::IP} looks up location and network information
by IP Address.")
    (license license:perl-license)))

(define-public perl-net-smtps
  (package
    (name "perl-net-smtps")
    (version "0.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/T/TO/TOMO/src/Net-SMTPS-" version
                    ".tar.gz"))
              (sha256
               (base32
                "1w6h7yjbk6a8xyi6rxgb0xlp2yqn55axl23wnqx687fy9y7gmzx9"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-authen-sasl
                             perl-io-socket-ssl))
    (home-page "https://metacpan.org/release/Net-SMTPS")
    (synopsis "SSL/STARTTLS support for Net::SMTP")
    (description
     "This module implements a wrapper for @code{Net::SMTP}, enabling
over-SSL/STARTTLS support.  This module inherits most of all the methods from
@code{Net::SMTP}(2.X).")
    (license license:perl-license)))

(define-public perl-net-ip
  (package
    (name "perl-net-ip")
    (version "1.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MANU/Net-IP-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ffn2xqqbkfi7v303sp5dwgbv36jah3vg8r4nxhxfiv60vric3q4"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Net-IP")
    (synopsis "Perl extension for manipulating IPv4/IPv6 addresses")
    (description
     "This module provides functions to deal with IPv4/IPv6 addresses.  The
module can be used as a class, allowing the user to instantiate IP objects,
which can be single IP addresses, prefixes, or ranges of addresses.  There is
also a procedural way of accessing most of the functions. Most subroutines can
take either IPv4 or IPv6 addresses transparently.")
    (license license:perl-license)))

(define-public perl-net-imap-simple
  (package
    (name "perl-net-imap-simple")
    (version "1.2212")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JE/JETTERO/Net-IMAP-Simple-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0mbzbfn0v1x1brd8nsyw8bzdkz31lw9qp7h6nrl3fcxylc5cs4lq"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-parse-recdescent))
    (home-page "https://metacpan.org/release/Net-IMAP-Simple")
    (synopsis "Perl extension for simple IMAP account handling.")
    (description
     "This package provides a class method constructs a new
@code{Net::IMAP::Simple} object.  It takes one required parameter which is the
server to connect to, and additional optional parameters.")
    (license license:perl-license)))

(define-public perl-mail-dmarc
  (package
    (name "perl-mail-dmarc")
    (version "1.20230215")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MB/MBRADSHAW/Mail-DMARC-"
                    version ".tar.gz"))
              (sha256
               (base32
                "00xrgq7cz75aj2m6bpfrb162fi0kyr016579as7494nbb53zbp2p"))))
    (build-system perl-build-system)
    (arguments
     (list #:tests? #f)) ;some require network, like 03.Base and 06.Result
    (native-inputs (list perl-file-sharedir-install
                         perl-module-build
                         perl-test-exception
                         perl-test-file-sharedir
                         perl-test-output))
    (propagated-inputs (list perl-config-tiny
                             perl-dbd-sqlite
                             perl-dbix-simple
                             perl-email-mime
                             perl-email-sender
                             perl-email-simple
                             perl-file-sharedir
                             perl-io-socket-ssl
                             perl-mail-dkim
                             perl-net-dns
                             perl-net-idn-encode
                             perl-net-imap-simple
                             perl-net-ip
                             perl-net-smtps
                             perl-net-ssleay
                             perl-regexp-common
                             perl-socket6
                             perl-uri
                             perl-xml-libxml))
    (home-page "https://metacpan.org/release/Mail-DMARC")
    (synopsis "Perl implementation of DMARC")
    (description
     "This module is a suite of tools for implementing DMARC. It adheres to
the 2013 DMARC draft, intending to implement every MUST and every SHOULD.")
    (license license:perl-license)))

(define-public perl-net-libidn
  (package
    (name "perl-net-libidn")
    (version "0.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/T/TH/THOR/Net-LibIDN-" version
                    ".tar.gz"))
              (sha256
               (base32
                "05xvpn9z7dpv62a5wgjx1n9kxblkqsgl9kb3vk3nx1mk8aacr2ig"))))
    (build-system perl-build-system)
    (inputs (list libidn))
    (home-page "https://metacpan.org/release/Net-LibIDN")
    (synopsis "Perl bindings for GNU Libidn")
    (description
     "The package provides bindings for GNU Libidn, a C library for handling
Internationalized Domain Names according to IDNA (RFC 3490), in a way very
much inspired by Turbo Fredriksson's PHP-IDN.")
    (license (list license:gpl1+ license:clarified-artistic))))

(define-public perl-razor2-client-agent
  ;; TODO: Package razor-agents and razor-agents-sdk
  (package
    (name "perl-razor2-client-agent")
    (version "2.86")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/T/TO/TODDR/Razor2-Client-Agent-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0pw4jhxhm2naqkz0h3gjgdjnygf4019zmvp712vj8pmnxc12w1jy"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-digest-sha1 perl-uri))
    (home-page "https://metacpan.org/release/Razor2-Client-Agent")
    (synopsis "Collaborative, content-based spam filtering network agent.")
    (description
     "This package provides a client library for Vipul's Razor.  Vipul's Razor
is a distributed, collaborative, spam detection and filtering network.
Through user contribution, Razor establishes a distributed and constantly
updating catalogue of spam in propagation that is consulted by email clients
to filter out known spam.  Detection is done with statistical and randomized
signatures that efficiently spot mutating spam content.  User input is
validated through reputation assignments based on consensus on report and
revoke assertions which in turn is used for computing confidence values
associated with individual signatures.")
    (license license:perl-license)))

(define-public spamassassin
  (package
    (name "spamassassin")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SI/SIDNEY/Mail-SpamAssassin-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0djgm04kvgq0iab4hhv66gxhl2bhyhj1lfpjcdsw7qq3s6krv5v5"))
              (modules '((guix build utils)))
              (snippet #~(delete-file "spamc/configure"))))
    (build-system perl-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'configure 'setup
                          (lambda _
                            (delete-file "t/debug.t") ;hangs
                            (delete-file "t/dnsbl_subtests.t") ;6 failures
                            (delete-file "t/spamc_x_e.t") ;3 failures
                            (setenv "CONFIG_SHELL" (which "sh"))
                            (with-directory-excursion "spamc"
                              (invoke "autoreconf" "-vif"))))
                        (add-after 'install 'fix-lib-path
                          (lambda _
                            (let ((bin (string-append #$output "/bin/")))
                              (for-each
                               (lambda (file)
                                 (substitute* file
                                   ;; TODO: Can we find a way to keep -T?
                                   (("perl -T")
                                    "perl"))
                                 (wrap-program file
                                   `("PERL5LIB" ":" prefix
                                     (,(getenv "PERL5LIB")
                                      ,(string-append
                                        #$output
                                        "/lib/perl5/site_perl")))))
                               (map (lambda (file)
                                      (string-append bin file))
                                    '("spamd" "sa-awl" "sa-check_spamd"
                                      "sa-compile" "sa-learn"
                                      "sa-update" "spamassassin")))))))))
    (native-inputs (list autoconf
                         automake
                         perl-devel-cycle
                         perl-net-dns
                         perl-critic
                         perl-critic-policy-perlsecret
                         perl-test-most
                         perl-test-pod
                         perl-text-diff))
    (propagated-inputs (list perl-archive-zip
                             perl-bsd-resource
                             perl-dbd-sqlite
                             perl-dbi
                             perl-email-address-xs
                             perl-encode-detect
                             perl-geo-ip
                             perl-html-parser
                             perl-io-socket-inet6
                             perl-io-socket-ssl
                             perl-io-string
                             perl-libwww
                             perl-mail-dkim
                             perl-mail-dmarc
                             perl-mail-spf
                             perl-net-cidr-lite
                             perl-net-dns
                             perl-net-ip
                             perl-net-libidn
                             perl-net-patricia
                             perl-netaddr-ip
                             perl-razor2-client-agent))
    (home-page "https://metacpan.org/release/Mail-SpamAssassin")
    (synopsis
     "Extensible email filter used to identify spam")
    (description
     "Apache SpamAssassin is an anti-spam platform giving system
administrators a filter to classify email and block spam (unsolicited bulk
email).  It uses a robust scoring framework and plug-ins to integrate a wide
range of advanced heuristic and statistical analysis tests on email headers
and body text including text analysis, Bayesian filtering, DNS blocklists,
and collaborative filtering databases.")
    (license license:asl2.0)))
