;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Aljosha Papsch <misc@rpapsch.de>
;;; Copyright © 2014-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015-2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Raoul Jean Pierre Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015-2020, 2024 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2016-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016, 2023 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016–2024 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Bake Timmons <b3timmons@speedymail.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017-2018, 2020-2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2019, 2020 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018, 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019, 2020-2021, 2023, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Pierre-Moana Levesque <pierre.moana.levesque@gmail.com>
;;; Copyright © 2019, 2020 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020, 2021, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2019, 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020, 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2020, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2021 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2021, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 la snesne <lasnesne@lagunposprasihopre.org>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021, 2023 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 cage <cage-dev@twistfold.it>
;;; Copyright © 2022 Pradana Aumars <paumars@courrier.dev>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2023 VÖRÖSKŐI András <voroskoi@gmail.com>
;;; Copyright © 2023 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Evgeny Pisemsky <mail@pisemsky.site>
;;; Copyright © 2024 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024, 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Raven Hallsby <karl@hallsby.com>
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

(define-module (gnu packages web)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix cvs-download)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnu-doc)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages re2c)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages skribilo)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module ((srfi srfi-1) #:select (delete-duplicates)))

(define-public qhttp
  (package
    (name "qhttp")
    (version "3.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/azadkuh/qhttp")
         (commit (string-append "version-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cx23g4y4k4v9p5ph6h7gfhp8sfy1gcdv1g6bl44hppar1y0zfdq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no target
       #:imported-modules
       ((guix build copy-build-system)
        ,@%default-gnu-imported-modules)
       #:modules
       (((guix build copy-build-system) #:prefix copy:)
        (guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "commondir.pri"
               (("\\$\\$PRJDIR/xbin")
                (string-append (assoc-ref outputs "out") "/lib"))
               (("-L")
                "-lhttp_parser -L")
               (("\\$\\$PRJDIR/3rdparty")
                ""))
             (substitute* "src/src.pro"
               (("SOURCES  \\+= \\$\\$PRJDIR/3rdparty/http-parser/http_parser.c")
                "")
               (("HEADERS  \\+= \\$\\$PRJDIR/3rdparty/http-parser/http_parser.h")
                ""))
             (substitute* '("src/private/qhttpbase.hpp" "src/qhttpabstracts.cpp")
               (("http-parser/http_parser.h")
                "http_parser.h"))
             #t))
         (replace 'configure
           (lambda _ (invoke "qmake")))
         (replace 'install
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("src" "include"
                       #:include-regexp ("\\.hpp$")))
                    args)))
         (add-after 'install 'remove-examples
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion
                 (string-append (assoc-ref outputs "out") "/lib")
               (for-each delete-file
                         (list
                          "basic-server"
                          "helloworld"
                          "postcollector")))
             #t)))))
    (inputs
     (list http-parser qtbase-5))
    (home-page "https://github.com/azadkuh/qhttp/")
    (synopsis "Qt-based HTTP Library")
    (description
     "Qhttp is a light-weight and asynchronous HTTP library
(both server & client) in Qt5 and C++14.")
    (license license:expat)))

(define-public httpd
  (package
    (name "httpd")
    (version "2.4.58")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://apache/httpd/httpd-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1id45r2ccgkbjm9i998997ch32lvicpyynyx8x6aa4420wmdf5ps"))))
    (build-system gnu-build-system)
    (native-inputs (list `(,pcre "bin")))       ;for 'pcre-config'
    (inputs (list apr apr-util libxcrypt openssl perl)) ; needed to run bin/apxs
    (arguments
     (list
      #:test-target "test"
      #:configure-flags #~(list "--enable-rewrite"
                                "--enable-userdir"
                                "--enable-vhost-alias"
                                "--enable-ssl"
                                "--enable-mime-magic"
                                (string-append "--sysconfdir="
                                               #$output
                                               "/etc/httpd"))))
    (synopsis "Featureful HTTP server")
    (description
     "The Apache HTTP Server Project is a collaborative software development
effort aimed at creating a robust, commercial-grade, featureful, and
freely-available source code implementation of an HTTP (Web) server.  The
project is jointly managed by a group of volunteers located around the world,
using the Internet and the Web to communicate, plan, and develop the server
and its related documentation.")
    (license license:asl2.0)
    (home-page "https://httpd.apache.org/")))

;; A package variant that may be out of date and vulnerable. Only for use in
;; test suites and should never be referred to by a built package.
(define-public httpd/pinned
  (hidden-package
    (package
      (inherit httpd)
      (version "2.4.52")
      (source (origin
               (method url-fetch)
               (uri (string-append "mirror://apache/httpd/httpd-"
                                   version ".tar.bz2"))
               (sha256
                (base32
                 "1jgmfbazc2n9dnl7axhahwppyq25bvbvwx0lqplq76by97fgf9q1")))))))

(define-public leafnode
  (package
    (name "leafnode")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/leafnode/leafnode/"
                                  version "/leafnode-" version ".tar.gz"))
              (sha256
               (base32
                "1pkryzndqaxs1ym7gs77r6x8mmzpnm5x7n2ph8ga45zn45rwwrxl"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-tests
                 (lambda _
                   (substitute* "Makefile.am"
                     (("/bin/sh") (which "sh"))))))))
    (native-inputs (list autoconf automake))
    (inputs (list pcre2))
    (home-page "https://sourceforge.net/projects/leafnode/")
    (synopsis "NNTP news proxy")
    (description
     "Leafnode is a caching Usenet news proxy that enables online newsreaders
to read news off-line and aggregates news from various NNTP servers into
one.")
    ;; Most of the code is under Expat license, with some GPL, LGPL exceptions.
    (license license:gpl2+)))

(define-public miniflux
  (package
    (name "miniflux")
    (version "2.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/miniflux/v2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ynhx8bzbgq6qznsxga34wmh8lpd8xhnff7hy51xsnk52maqf3h1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:install-source? #f
      #:import-path "miniflux.app/v2"
      #:build-flags
      #~(list (string-append
               "-ldflags= -X miniflux.app/v2/internal/version.Version="
               #$version))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-manpage
            (lambda* (#:key import-path #:allow-other-keys)
              (let ((man1 (string-append #$output "/share/man/man1/"))
                    (page (format #f "src/~a/miniflux.1" import-path)))
                (install-file page man1))))
          (add-after 'install-manpage 'rename-binary
            (lambda _
              (let ((bindir (string-append #$output "/bin/")))
                (rename-file (string-append bindir "v2")
                             (string-append bindir "miniflux"))))))))
    (inputs
     (list go-github-com-abadojack-whatlanggo
           go-github-com-andybalholm-brotli
           go-github-com-coreos-go-oidc-v3
           go-github-com-go-webauthn-webauthn
           go-github-com-gorilla-mux
           go-github-com-lib-pq
           go-github-com-prometheus-client-golang
           go-github-com-puerkitobio-goquery
           go-github-com-tdewolff-minify-v2
           go-github-com-yuin-goldmark
           go-golang-org-x-crypto
           go-golang-org-x-image
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-golang-org-x-term
           go-golang-org-x-text))
    (home-page "https://miniflux.app/")
    (synopsis "Minimalist and opinionated feed reader")
    (description
     "Miniflux is a minimalist and opinionated feed reader:

@itemize
@item Written in Go (Golang)
@item Works only with Postgresql
@item Doesn't use any ORM
@item Doesn't use any complicated framework
@item Use only modern vanilla Javascript (ES6 and Fetch API)
@item Single binary compiled statically without dependency
@item The number of features is voluntarily limited
@end itemize")
    (license license:asl2.0)))

(define-public mod-wsgi
  (package
    (name "mod-wsgi")
    (version "4.9.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/GrahamDumpleton/mod_wsgi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zf921nd9xxdvvc8awzzfrljr0n29vi28mlam0jdwvsk0xv4gd7a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:disallowed-references (,httpd)
       #:tests? #f                 ; TODO: can't figure out if there are tests
       #:make-flags (list
                     (string-append "DESTDIR="
                                    (assoc-ref %outputs "out"))
                     "LIBEXECDIR=/modules")))
    (native-inputs
     `(("httpd" ,httpd)))
    (inputs
     `(("python" ,python-wrapper)))
    (synopsis "Apache HTTPD module for Python WSGI applications")
    (description
     "The mod_wsgi module for the Apache HTTPD Server adds support for running
applications that support the Python @acronym{WSGI, Web Server Gateway
Interface} specification.")
    (license license:asl2.0)
    (home-page "https://modwsgi.readthedocs.io/")))

(define-public ablorb
  (package
    (name "ablorb")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/lilyp/ablorb")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i705p2gw5aryj0myfj3rmsrmj3ilqdn5w7xd5dwjkyi80rc20kj"))))
    (build-system meson-build-system)
    (inputs (list glib gconf gnome-vfs libxml2))
    (native-inputs (list pkg-config))
    (home-page "https://gitlab.gnome.org/lilyp/ablorb")
    (synopsis "Replace asset links with data URIs")
    (description "Ablorb takes an XML file and resolves relative links,
replacing them with data URIs.")
    (license license:gpl3+)))

(define-public monolith
  (package
    (name "monolith")
    (version "2.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Y2Z/monolith")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "082xh0zmmy9abz7y3zjybbwffq7d0j1jl78ggzbwwanvam65v0dp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-base64" ,rust-base64-0.22)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-3)
        ("rust-cssparser" ,rust-cssparser-0.34)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-html5ever" ,rust-html5ever-0.27)
        ("rust-markup5ever-rcdom" ,rust-markup5ever-rcdom-0.3)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-regex" ,rust-regex-1)
        ("rust-reqwest" ,rust-reqwest-0.12)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-default-to-vendored-openssl
           (lambda _
             (substitute* "Cargo.toml"
               ((".*\"vendored-openssl\".*") "")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/Y2Z/monolith")
    (synopsis "Command line tool for saving web pages as a single HTML file")
    (description
     "Monolith bundles any web page into a single HTML file.

Unlike conventional ``Save page as…'', Monolith not only saves the target
document, it embeds CSS, image, and JavaScript assets all at once, producing
a single HTML5 document.

If compared to saving websites with @samp{wget -mpk}, Monolith embeds
all assets as data URLs and therefore displays the saved page exactly
the same, being completely separated from the Internet.")
    (license license:unlicense)))

(define-public nginx
  (package
    (name "nginx")
    ;; Please update the nginx-documentation package together with this one!
    ;; Track the ‘mainline’ branch.  Upstream considers it more reliable than
    ;; ’stable’ and recommends that “in general you deploy the NGINX mainline
    ;; branch at all times” (https://www.nginx.com/blog/nginx-1-6-1-7-released/)
    (version "1.27.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nginx.org/download/nginx-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00vrkdx0a6fpy8n0n7m9xws0dfa7dbb9pqnh3jv3c824ixbaj8xs"))))
    (build-system gnu-build-system)
    (inputs (list libxcrypt libxml2 libxslt openssl pcre zlib))
    (arguments
     (list
      #:tests? #f                       ; no test target
      #:configure-flags
      #~(list "--with-http_ssl_module"
              "--with-http_v2_module"
              "--with-http_xslt_module"
              "--with-http_gzip_static_module"
              "--with-http_gunzip_module"
              "--with-http_addition_module"
              "--with-http_sub_module"
              "--with-pcre-jit"
              "--with-debug"
              "--with-compat"
              "--with-stream"
              "--with-stream_ssl_module"
              "--with-http_stub_status_module"
              ;; Even when not cross-building, we pass the
              ;; --crossbuild option to avoid customizing for the
              ;; kernel version on the build machine.
              #$(let ((system "Linux")  ; uname -s
                      (release "3.2.0") ; uname -r
                      ;; uname -m
                      (machine (match (or (%current-target-system)
                                          (%current-system))
                                 ("x86_64-linux"   "x86_64")
                                 ("i686-linux"     "i686")
                                 ("mips64el-linux" "mips64")
                                 ;; Prevent errors when querying
                                 ;; this package on unsupported
                                 ;; platforms, e.g. when running
                                 ;; "guix package --search="
                                 (_                "UNSUPPORTED"))))
                  (string-append "--crossbuild="
                                 system ":" release ":" machine)))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-/bin/sh
            (lambda _
              (substitute* "auto/feature"
                (("/bin/sh") (which "sh")))))
          (replace 'configure
            ;; The configure script is hand-written, not from GNU autotools.
            (lambda* (#:key configure-flags inputs #:allow-other-keys)
              (setenv "CC" #$(cc-for-target))
              ;; Fix ./configure test for ‘#include <libxml/parser.h>’.
              (setenv "CFLAGS"          ; CPPFLAGS is not respected
                      (string-append "-O2 -g "
                                     "-I" (search-input-directory
                                           inputs "/include/libxml2")))
              (format #t "configure flags: ~s~%" configure-flags)
              (apply invoke "./configure"
                     (string-append "--prefix=" #$output)
                     configure-flags)))
          (add-after 'install 'install-man-page
            (lambda _
              (let ((man (string-append #$output "/share/man")))
                (install-file "objs/nginx.8" (string-append man "/man8")))))
          (add-after 'install 'fix-root-dirs
            (lambda _
              ;; 'make install' puts things in strange places, so we need to
              ;; clean it up ourselves.
              (let* ((out #$output)
                     (share (string-append out "/share/nginx")))
                ;; This directory is empty, so get rid of it.
                (rmdir (string-append out "/logs"))
                ;; Example configuration and HTML files belong in
                ;; /share.
                (mkdir-p share)
                (rename-file (string-append out "/conf")
                             (string-append share "/conf"))
                (rename-file (string-append out "/html")
                             (string-append share "/html"))))))))
    (home-page "https://nginx.org")
    (synopsis "HTTP and reverse proxy server")
    (description
     "Nginx (\"engine X\") is a high-performance web and reverse proxy server
created by Igor Sysoev.  It can be used both as a stand-alone web server
and as a proxy to reduce the load on back-end HTTP or mail servers.")
    ;; Almost all of nginx is distributed under the bsd-2 license.
    ;; The exceptions are:
    ;;   * The 'nginx-http-push' module is covered by the expat license.
    ;;   * The 'nginx-development-kit' module is mostly covered by bsd-3,
    ;;     except for two source files which are bsd-4 licensed.
    (license (list license:bsd-2 license:expat license:bsd-3 license:bsd-4))))

(define-public nginx-documentation
  ;; This documentation should be relevant for the current nginx package.
  (let ((version "1.27.3")
        (revision 3156)
        (changeset "5c6ef6def8bc"))
    (package
      (name "nginx-documentation")
      (version (simple-format #f "~A-~A-~A" version revision changeset))
      (source
       (origin (method hg-fetch)
               (uri (hg-reference
                     (url "http://hg.nginx.org/nginx.org")
                     (changeset changeset)))
               (file-name (string-append name "-" version))
               (sha256
                (base32
                 "09wdvgvsr7ayjz3ypq8qsm12idb9z626j5ibmknc8phm10gh8cgk"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f                    ; no test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (replace 'build
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((output (assoc-ref outputs "out")))
                 (substitute* "tools/umasked.sh"
                   ((" /bin/sh") (string-append " " (which "sh"))))
                 ;; The documentation includes a banner, which makes sense on
                 ;; the NGinx website, but doesn't make much sense when
                 ;; viewing locally. Therefore, modify the CSS to remove the
                 ;; banner.
                 (substitute* "xslt/style.xslt"
                   (("#banner           \\{ background:     black;")
                    "#banner           { display:        none;"))
                 (invoke "make")
                 #t)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((output (assoc-ref outputs "out")))
                 (mkdir-p output)
                 (copy-recursively "libxslt" output)
                 #t))))))
      (native-inputs
       (list libxml2 libxslt nginx-xslscript))
      (home-page "https://nginx.org")
      (synopsis "Documentation for the nginx web server")
      (description
       "This package provides HTML documentation for the nginx web server.")
      (license license:bsd-2))))

(define-public nginx-accept-language-module
  ;; Upstream has never made a release; use current commit instead.
  (let ((commit "2f69842f83dac77f7d98b41a2b31b13b87aeaba7")
        (revision "1"))
    (package
      (name "nginx-accept-language-module")
      (version (git-version "0.0.0" ;upstream has no version number
                            revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/giom/nginx_accept_language_module")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hjysrl15kh5233w7apq298cc2bp4q1z5mvaqcka9pdl90m0vhbw"))))
      (build-system gnu-build-system)
      (inputs `(("libxcrypt" ,libxcrypt)
                ("openssl" ,openssl)
                ("pcre" ,pcre)
                ("nginx-sources" ,(package-source nginx))
                ("zlib" ,zlib)))
      (arguments
       `(#:tests? #f                      ; no test target
         #:make-flags (list "modules")
         #:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (ice-9 popen)
                    (ice-9 regex)
                    (ice-9 textual-ports))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-nginx-sources
             (lambda* (#:key inputs native-inputs #:allow-other-keys)
               (begin
                 ;; The nginx source code is part of the module’s source.
                 (format #t "decompressing nginx source code~%")
                 (let ((tar (assoc-ref inputs "tar"))
                       (nginx-srcs (assoc-ref inputs "nginx-sources")))
                   (invoke (string-append tar "/bin/tar")
                           "xvf" nginx-srcs "--strip-components=1"))
                 #t)))
           (add-after 'unpack 'convert-to-dynamic-module
             (lambda _
               (begin
                 (with-atomic-file-replacement "config"
                   (lambda (in out)
                     ;; cf. https://www.nginx.com/resources/wiki/extending/new_config/
                     (format out "ngx_module_type=HTTP~%")
                     (format out "ngx_module_name=\
ngx_http_accept_language_module~%")
                     (let* ((str (get-string-all in))
                            (rx (make-regexp
                                 "NGX_ADDON_SRCS=\"\\$NGX_ADDON_SRCS (.*)\""))
                            (m (regexp-exec rx str))
                            (srcs (match:substring m 1)))
                       (format out (string-append "ngx_module_srcs=\""
                                                  srcs "\"~%")))
                     (format out ". auto/module~%")
                     (format out "ngx_addon_name=$ngx_module_name~%"))))))
           (add-before 'configure 'patch-/bin/sh
             (lambda _
               (substitute* "auto/feature"
                 (("/bin/sh") (which "sh")))
               #t))
           (replace 'configure
             ;; This phase is largely copied from the nginx package.
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((flags
                      (list ;; A copy of nginx’ flags follows, otherwise we
                            ;; get a binary compatibility error.  FIXME: Code
                            ;; duplication is bad.
                       (string-append "--prefix=" (assoc-ref outputs "out"))
                       "--with-http_ssl_module"
                       "--with-http_v2_module"
                       "--with-pcre-jit"
                       "--with-debug"
                       "--with-compat"
                       ;; Even when not cross-building, we pass the
                       ;; --crossbuild option to avoid customizing for the
                       ;; kernel version on the build machine.
                       ,(let ((system "Linux")    ; uname -s
                              (release "3.2.0")   ; uname -r
                              ;; uname -m
                              (machine (match (or (%current-target-system)
                                                  (%current-system))
                                         ("x86_64-linux"   "x86_64")
                                         ("i686-linux"     "i686")
                                         ("mips64el-linux" "mips64")
                                         ;; Prevent errors when querying
                                         ;; this package on unsupported
                                         ;; platforms, e.g. when running
                                         ;; "guix package --search="
                                         (_                "UNSUPPORTED"))))
                          (string-append "--crossbuild="
                                         system ":" release ":" machine))
                       ;; The following are the args decribed on
                       ;; <https://www.nginx.com/blog/compiling-dynamic-modules-nginx-plus>.
                       ;; Enabling --with-compat here and in the nginx package
                       ;; would ensure binary compatibility even when using
                       ;; different configure options from the main nginx
                       ;; package.  This is not needed for Guix.
                       ;; "--with-compat"
                       "--add-dynamic-module=.")))
                 (setenv "CC" "gcc")
                 (format #t "environment variable `CC' set to `gcc'~%")
                 (format #t "configure flags: ~s~%" flags)
                 (apply invoke "./configure" flags)
                 #t)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (modules-dir (string-append out "/etc/nginx/modules"))
                      (doc-dir (string-append
                                out "/share/doc/nginx-accept-language-module")))
                 (mkdir-p modules-dir)
                 (copy-file "objs/ngx_http_accept_language_module.so"
                            (string-append
                             modules-dir "/ngx_http_accept_language_module.so"))
                 (mkdir-p doc-dir)
                 (copy-file "README.textile"
                            (string-append doc-dir "/README.textile"))
                 #t))))))
      (home-page
       "https://www.nginx.com/resources/wiki/modules/accept_language/")
      (synopsis "Nginx module for parsing the Accept-Language HTTP header")
      (description
       "This nginx module parses the Accept-Language field in HTTP headers and
chooses the most suitable locale for the user from the list of locales
supported at your website.")
      (license (delete-duplicates
                (cons license:bsd-2 ;license of nginx-accept-language-module
                      ;; The module’s code is linked statically with nginx,
                      ;; therefore nginx’ other licenses may also apply to its
                      ;; binary:
                      (package-license nginx)))))))

(define nginx-xslscript
  (let ((revision 11)
        (changeset "01dc9ba12e1b"))
    (package
      (name "nginx-xslscript")
      (version
       (simple-format #f "2014-03-31-~A-~A" revision changeset))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://hg.nginx.org/xslscript")
                      (changeset changeset)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "0am8zvdx3jmiwkg5q07qjaw5r26r4i2v5i4yr8a1k0jgib6ii08g"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f  ; No test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out-bin (string-append
                               (assoc-ref outputs "out")
                               "/bin")))
                 (mkdir-p out-bin)
                 (copy-file "xslscript.pl"
                            (string-append
                             out-bin
                             "/xslscript.pl"))
                 #t))))))
      (home-page "https://hg.nginx.org/xslscript")
      (synopsis "XSLScript with NGinx specific modifications")
      (description
       "XSLScript is a terse notation for writing complex XSLT stylesheets.
This is modified version, specifically intended for use with the NGinx
documentation.")
      (license license:bsd-2))))

(define nginx-socket-cloexec
  (package
    (inherit nginx)
    (name "nginx-socket-cloexec") ;required for lua-resty-shell
    (source
     (origin
       (inherit (package-source nginx))
       (patches (append (search-patches "nginx-socket-cloexec.patch")
                        (origin-patches (package-source nginx))))))))

(define-public nginx-lua-module
  (package
    (inherit nginx)
    (name "nginx-lua-module")
    (version "0.10.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openresty/lua-nginx-module")
             (commit (string-append "v" version))))
       (file-name (git-file-name "lua-nginx-module" version))
       (sha256
        (base32 "0nwcbqm1visg1dkxav7qa16w0d0n8cgqn4881xiqn88xfkxj0dyg"))))
    (build-system gnu-build-system)
    (inputs
     `(("nginx-sources" ,(package-source nginx-socket-cloexec))
       ("luajit" ,luajit)
       ,@(package-inputs nginx)))
    (arguments
     (substitute-keyword-arguments
         `(#:make-flags '("modules")
           #:modules ((guix build utils)
                      (guix build gnu-build-system)
                      (ice-9 popen)
                      (ice-9 regex)
                      (ice-9 textual-ports))
           ,@(package-arguments nginx)
           #:configure-flags '("--add-dynamic-module=."))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'unpack-nginx-sources
              (lambda* (#:key inputs native-inputs #:allow-other-keys)
                (begin
                  ;; The nginx source code is part of the module’s source.
                  (format #t "decompressing nginx source code~%")
                  (let ((tar (assoc-ref inputs "tar"))
                        (nginx-srcs (assoc-ref inputs "nginx-sources")))
                    (invoke (string-append tar "/bin/tar")
                            "xvf" nginx-srcs "--strip-components=1"))
                  #t)))
            (add-before 'configure 'set-luajit-env
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((luajit (assoc-ref inputs "luajit")))
                  (setenv "LUAJIT_LIB"
                          (string-append luajit "/lib"))
                  (setenv "LUAJIT_INC"
                          (string-append luajit "/include/luajit-2.1"))
                  #t)))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((modules-dir (string-append (assoc-ref outputs "out")
                                                  "/etc/nginx/modules")))
                  (install-file "objs/ngx_http_lua_module.so" modules-dir)
                  #t)))
            (delete 'fix-root-dirs)
            (delete 'install-man-page)))))
    (synopsis "NGINX module for Lua programming language support")
    (description "This NGINX module provides a scripting support with Lua
programming language.")))

(define-public nginx-rtmp-module
  (package
    (inherit nginx)
    (name "nginx-rtmp-module")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arut/nginx-rtmp-module")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y45bswk213yhkc2v1xca2rnsxrhx8v6azxz9pvi71vvxcggqv6h"))))
    (build-system gnu-build-system)
    (inputs
     `(("nginx-sources" ,(package-source nginx))
       ,@(package-inputs nginx)))
    (arguments
     (substitute-keyword-arguments
         `(#:make-flags '("modules") ;Only build this module not all of nginx.
           ,@(package-arguments nginx))
       ((#:configure-flags flags)
        #~(cons "--add-dynamic-module=." #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'unpack-nginx-sources
              (lambda _
                (begin
                  ;; The nginx source code is part of the module’s source.
                  (format #t "decompressing nginx source code~%")
                  (invoke "tar" "xvf" #$(this-package-input "nginx-sources")
                          ;; This package's LICENSE file would be
                          ;; overwritten with the one from nginx when
                          ;; unpacking the nginx source, so rename the nginx
                          ;; one when unpacking.
                          "--transform=s,/LICENSE$,/LICENSE.nginx,"
                          "--strip-components=1"))))
            (replace 'install
              (lambda _
                (let ((modules-dir (string-append #$output
                                                  "/etc/nginx/modules")))
                  (install-file "objs/ngx_rtmp_module.so" modules-dir))))
            (delete 'fix-root-dirs)
            (delete 'install-man-page)))))
    (home-page "https://github.com/arut/nginx-rtmp-module")
    (synopsis "NGINX module for audio and video streaming with RTMP")
    (description "This NGINX module provides streaming with the @acronym{RTMP,
Real-Time Messaging Protocol}, @acronym{DASH, Dynamic Adaptive Streaming over HTTP},
and @acronym{HLS, HTTP Live Streaming} protocols.  It allows NGINX to accept
incoming RTMP streams for recording or redistribution.  It also supports
on-demand streaming from a file on disk and pulling from an upstream RTMP
stream.  Remote control of the module is possible over HTTP.")
    (license license:bsd-2)))

(define-public nginx-headers-more-module
  (package
    (inherit nginx)
    (name "nginx-headers-more-module")
    (version "0.38")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openresty/headers-more-nginx-module")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dbgwzkpni616nawjkrq0xid60wdgab3vciy7nr966ac6rjyiliy"))))
    (build-system gnu-build-system)
    (inputs
     `(("nginx-sources" ,(package-source nginx))
       ,@(package-inputs nginx)))
    (arguments
     (substitute-keyword-arguments
         `(#:make-flags '("modules") ;Only build this module not all of nginx.
           ,@(package-arguments nginx))
       ((#:configure-flags flags)
        #~(cons "--add-dynamic-module=." #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'unpack-nginx-sources
              (lambda _
                (begin
                  ;; The nginx source code is needed to compile the module.
                  (format #t "decompressing nginx source code~%")
                  (invoke "tar" "xvf" #$(this-package-input "nginx-sources")
                          ;; This package's LICENSE file would be
                          ;; overwritten with the one from nginx when
                          ;; unpacking the nginx source, so rename the nginx
                          ;; one when unpacking.
                          "--transform=s,/LICENSE$,/LICENSE.nginx,"
                          "--strip-components=1"))))
            (replace 'install
              (lambda _
                (let ((modules-dir (string-append #$output
                                                  "/etc/nginx/modules")))
                  (install-file "objs/ngx_http_headers_more_filter_module.so"
                                modules-dir))))
            (delete 'fix-root-dirs)
            (delete 'install-man-page)))))
    (home-page "https://github.com/openresty/headers-more-nginx-module")
    (synopsis "Set, add, and clear input and output headers in NGINX http servers")
    (description "This NGINX module allows adding, setting, or clearing any
output or input header specified.

This is an enhanced version of the standard headers module because it provides
more utilities like resetting or clearing \"builtin headers\" like @code{Content-Type},
@code{Content-Length}, and @code{Server}.

It also allows you to specify an optional HTTP status code criteria using the
@code{-s} option and an optional content type criteria using the @code{-t}
option while modifying the output headers with the more_set_headers and
more_clear_headers directives.")
    (license license:bsd-2)))

(define-public nginx-module-vts
  (package
    (inherit nginx)
    (name "nginx-module-vts")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vozlt/nginx-module-vts")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "017298vpp1ra16xyfdbsczdrz0b0y67x6adkzcc98y6gb3kg52n7"))))
    (build-system gnu-build-system)
    (inputs
     `(("nginx-sources" ,(package-source nginx))
       ,@(package-inputs nginx)))
    (arguments
     (substitute-keyword-arguments
         `(#:make-flags '("modules") ;Only build this module not all of nginx.
           ,@(package-arguments nginx))
       ((#:configure-flags flags)
        #~(cons "--add-dynamic-module=." #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'unpack-nginx-sources
              (lambda _
                (begin
                  ;; The nginx source code is part of the module’s source.
                  (format #t "decompressing nginx source code~%")
                  (invoke "tar" "xvf" #$(this-package-input "nginx-sources")
                          ;; This package's LICENSE file would be
                          ;; overwritten with the one from nginx when
                          ;; unpacking the nginx source, so rename the nginx
                          ;; one when unpacking.
                          "--transform=s,/LICENSE$,/LICENSE.nginx,"
                          "--strip-components=1"))))
            (replace 'install
              (lambda _
                (let ((modules-dir (string-append #$output
                                                  "/etc/nginx/modules")))
                  (install-file "objs/ngx_http_vhost_traffic_status_module.so" modules-dir))))
            (delete 'fix-root-dirs)
            (delete 'install-man-page)))))
    (home-page "https://github.com/vozlt/nginx-module-vts")
    (synopsis "NGINX module for monitoring virtual host traffic status")
    (description "This NGINX module provides access to virtual host status information,
similar to live activity monitoring provided with NGINX plus.")
    (license license:bsd-2)))

(define-public lighttpd
  (package
    (name "lighttpd")
    (version "1.4.78")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.lighttpd.net/lighttpd/"
                                  "releases-" (version-major+minor version) ".x/"
                                  "lighttpd-" version ".tar.xz"))
              (sha256
               (base32
                "0giavficc2yjl53cdbc7pd50wjyx0k1y32gs3kyfkjbmpkl3j1rw"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--with-krb5"
                   "--with-ldap"
                   "--with-libev"
                   "--with-libunwind"
                   "--with-openssl"
                   "--with-pam"
                   "--with-sasl")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'embed-/bin/sh-reference
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/mod_ssi.c"
                     (("/bin/sh") (search-input-file inputs "/bin/sh")))))
               (add-after 'unpack 'fix-tests
                 (lambda _
                   (setenv "SHELL" (which "sh"))
                   ;; gethostbyaddr fails
                   (substitute* "tests/LightyTest.pm"
                     (("\\{HOSTNAME\\} = \\$name;")
                      "{HOSTNAME} = \"127.0.0.1\";"))))
               (add-after 'unpack 'skip-failing-tests
                 ;; XXX It would be wonderful if you, reader, felt suddenly and
                 ;; irresistibly compelled to investigate & fix these failures.
                 (lambda _
                   ;; Throws a bunch of ‘connect failed: Connection refused’.
                   (delete-file "tests/mod-scgi.t")

                   ;; test_mod_ssi_read_fd: Assertion `cq->first' failed.
                   (substitute* "src/t/test_mod.c"
                     ((".*\\btest_mod_ssi\\b.*") "")))))))
    (inputs
     (list bash-minimal
           cyrus-sasl
           libev
           libunwind
           linux-pam
           mit-krb5
           openldap
           openssl
           pcre2
           zlib))
    (native-inputs
     (list autoconf
           automake
           libtool
           perl ; for tests
           pkg-config which))
    (home-page "https://www.lighttpd.net/")
    (synopsis "Lightweight HTTP and reverse proxy server")
    (description
     "Lighttpd is a secure, fast, compliant, and very flexible web-server that
has been optimized for high-performance environments.  It has a very low
memory footprint compared to other webservers.  Its features include FastCGI,
CGI, authentication, output compression, URL rewriting and many more.")
    (license license:bsd-3)))

(define-public fcgi
  (package
    (name "fcgi")
    (version "2.4.2")
    (source
     (origin
       (method git-fetch)
       ;; Upstream has disappeared.
       (uri (git-reference
             (url "https://github.com/FastCGI-Archives/fcgi2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jhz6jfwv5kawa8kajvg18nfwc1b30f38zc0lggszd1vcmrwqkz1"))))
    (build-system gnu-build-system)
    ;; Parallel building is not supported.
    (arguments `(#:parallel-build? #f))
    (native-inputs
     (list autoconf automake libtool))
    ;; This is an archived fork of the original home page, www.fastcgi.com.
    (home-page "https://fastcgi-archives.github.io/")
    (synopsis "Language-independent, high-performant extension to CGI")
    (description "FastCGI is a language-independent, scalable extension to CGI
that provides high performance without the limitations of server specific
APIs.")
    ;; This package is released under the Open Market License, a variant of
    ;; the Expat license, incompatible with the GPL.
    (license (license:non-copyleft "file://LICENSE.TERMS"))))

(define-public fcgiwrap
  (let ((commit "2870d2729a3930988f0041e2d78fec672e69afac")
        (revision "1"))
    (package
      (name "fcgiwrap")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; Upstream last updated in 2015, this forked version has better
               ;; socket cleanup.
               (url "https://github.com/flu0r1ne/fcgiwrap")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0rkpp4apfhdcrmym3pcpqlncd0r4fyr3pa45i8g6x4p38b4azmmm"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no tests included
         #:make-flags (list "CC=gcc")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-CFLAGS
             ;; Remove broken options unconditionally added to CFLAGS.
             (lambda _
               (substitute* "configure.ac"
                 ((" -Werror") ""))
               #t)))))
      (native-inputs
       (list autoconf automake pkg-config))
      (inputs
       (list fcgi))
      (home-page "https://nginx.localdomain.pl/wiki/FcgiWrap")
      (synopsis "Simple server for running CGI applications over FastCGI")
      (description "Fcgiwrap is a simple server for running CGI applications
over FastCGI.  It hopes to provide clean CGI support to Nginx (and other web
servers that may need it).")
      (license license:expat))))

(define-public starman
  (package
    (name "starman")
    (version "0.4015")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Starman-" version ".tar.gz"))
       (sha256
        (base32 "1y1kn4929k299fbf6sw9lxcsdlq9fvq777p6yrzk591rr9xhkx8h"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-libwww perl-module-build-tiny perl-test-requires))
    (propagated-inputs
     (list perl-data-dump
           perl-http-date
           perl-http-message
           perl-http-parser-xs
           perl-net-server
           perl-plack
           perl-test-tcp))
    (home-page "https://metacpan.org/release/Starman")
    (synopsis "PSGI/Plack web server")
    (description "Starman is a PSGI perl web server that has unique features
such as high performance, preforking, signal support, superdaemon awareness,
and UNIX socket support.")
    (license license:perl-license)))

(define-public icedtea-web
  (package
    (name "icedtea-web")
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://icedtea.wildebeest.org/download/source/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "004kwrngyxxlrlzby4vzxjr0xcyngcdc9dfgnvi61ffnjr006ryf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list  "--disable-plugin"         ;NPAPI plugins are obsolete nowadays.
              (string-append "BIN_BASH="
                             (search-input-file %build-inputs "/bin/bash"))
             (string-append "--with-jdk-home=" (assoc-ref %build-inputs "jdk")))))
    (outputs '("out" "doc"))
    (native-inputs
     (list pkg-config zip))
    (inputs
     `(("gtk+" ,gtk+)
       ("jdk" ,icedtea "jdk")))
    (home-page "http://icedtea.classpath.org/wiki/IcedTea-Web")
    (synopsis "Java Web Start")
    (description
     "IcedTea-Web is an implementation of the @dfn{Java Network Launching
Protocol}, also known as Java Web Start.  This package provides tools and
libraries for working with JNLP applets.")
    ;; The program is mainly GPL2+, with some individual files under LGPL2.1+
    ;; or dual licenses.
    (license license:gpl2+)))

(define-public jansson
  (package
    (name "jansson")
    (version "2.14")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/akheron/jansson"
                                 "/releases/download/v" version
                                 "/jansson-" version ".tar.bz2"))
             (sha256
              (base32
               "1fdgji964mrrz19glx0zh91asji542fvybymvzk6rrbagkr5dagv"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~'("--disable-static")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              ;; Take a fix from upstream for testing with symbol versioning:
              ;; https://github.com/akheron/jansson/pull/593
              (substitute* "test/suites/api/check-exports"
                (("(grep ' \\[DT\\] ' \\$test_log/symbols.*) \\| sort" _ cmd)
                 (string-append cmd "| sed 's/@@libjansson.*//' | sort"))))))))
    (home-page "https://github.com/akheron/jansson")
    (synopsis "JSON C library")
    (description
     "Jansson is a C library for encoding, decoding and manipulating JSON
data.")
    (license license:expat)))

(define-public json-c
  (package
    (name "json-c")
    (version "0.15")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://s3.amazonaws.com/json-c_releases/releases/json-c-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1im484iz08j3gmzpw07v16brwq46pxxj65i996kkp2vivcfhmn5q"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/json-c/json-c/wiki")
    (synopsis "JSON implementation in C")
    (description
     "JSON-C implements a reference counting object model that allows you to
easily construct JSON objects in C, output them as JSON-formatted strings and
parse JSON-formatted strings back into the C representation of JSON objects.
It aims to conform to RFC 7159.")
    (license license:x11)))

;; TODO: Remove these old versions when all dependents have been updated.
(define-public json-c-0.13
  (package
    (inherit json-c)
    (version "0.13.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://s3.amazonaws.com/json-c_releases/releases/json-c-"
                   version ".tar.gz"))
             (sha256
              (base32 "0ws8dz9nk8q2c0gbf66kg2r6mrkl7kamd3gpdv9zsyrz9n6n0zmq"))
              (patches (search-patches "json-c-0.13-CVE-2020-12762.patch"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Somehow 'config.h.in' is older than
                 ;; 'aclocal.m4', which would trigger a rule to
                 ;; run 'autoheader'.
                 (set-file-time "config.h.in"
                                (stat "aclocal.m4"))
                 #t))))
    (build-system gnu-build-system)))

(define-public json-c-0.12
  (package
    (inherit json-c-0.13)
    (version "0.12.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://s3.amazonaws.com/json-c_releases/releases/json-c-"
                   version ".tar.gz"))
             (sha256
              (base32 "08qibrq29a5v7g23wi5icy6l4fbfw90h9ccps6vq0bcklx8n84ra"))
              (patches (search-patches "json-c-0.12-CVE-2020-12762.patch"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Somehow 'config.h.in' is older than
                 ;; 'aclocal.m4', which would trigger a rule to
                 ;; run 'autoheader'.
                 (set-file-time "config.h.in"
                                (stat "aclocal.m4"))

                 ;; Don't try to build with -Werror.
                 (substitute* (find-files "." "Makefile\\.in")
                   (("-Werror") ""))
                 #t))))))

(define-public json-parser
  (package
    (name "json-parser")
    (version "1.1.0")
    (source (origin
              ;; do not use auto-generated tarballs
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/udp/json-parser")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ls7z4fx0sq633s5bc0j1gh36sv087gmrgr7rza22wjq2d4606yf"))))
    ;; FIXME: we should build the python bindings in a separate package
    (build-system gnu-build-system)
    ;; the tests are written for the python bindings which are not built here
    (arguments '(#:tests? #f))
    (home-page "https://github.com/udp/json-parser")
    (synopsis "JSON parser written in ANSI C")
    (description "This package provides a very low footprint JSON parser
written in portable ANSI C.

@itemize
@item BSD licensed with no dependencies (i.e. just drop the C file into your
project)
@item Never recurses or allocates more memory than it needs
@item Very simple API with operator sugar for C++
@end itemize")
    (license license:bsd-2)))

(define-public qjson
  (package
    (name "qjson")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flavio/qjson")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f4wnxzx0qdmxzc7hqk28m0sva7z9p9xmxm6aifvjlp0ha6pmfxs"))))
    (build-system cmake-build-system)
    (arguments
     ;; The tests require a running X server.
     `(#:configure-flags '("-DQJSON_BUILD_TESTS=ON"
                           "-DCMAKE_CXX_FLAGS=-std=gnu++11 -fPIC")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-broken-test
           (lambda _
             ;; FIXME: One test fails.  See
             ;; https://github.com/flavio/qjson/issues/105
             (substitute* "tests/scanner/testscanner.cpp"
               (("QTest::newRow\\(\"too large exponential\"\\)" line)
                (string-append "//" line)))
             #t))
         (add-before 'check 'render-offscreen
           (lambda _ (setenv "QT_QPA_PLATFORM" "offscreen") #t)))))
    (inputs
     (list qtbase-5))
    (home-page "https://qjson.sourceforge.net")
    (synopsis "Library that maps JSON data to QVariant objects")
    (description "QJson is a Qt-based library that maps JSON data to
@code{QVariant} objects.  JSON arrays will be mapped to @code{QVariantList}
instances, while JSON's objects will be mapped to @code{QVariantMap}.")
    ;; Only version 2.1 of the license
    (license license:lgpl2.1)))

(define-public qoauth
  (package
    (name "qoauth")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ayoy/qoauth")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b2jdqs526ac635yb2whm049spcsk7almnnr6r5b4yqhq922anw3"))))
    (build-system gnu-build-system)
    (inputs
     (list qca qtbase-5))
    (arguments
     '(#:tests? #f                      ;FIXME: some tests are failing
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-code
           (lambda _
             (make-file-writable "src/qoauth.pc")
             (substitute* "src/src.pro"
               (("/lib64") "/lib"))
             #t))
         (add-after 'unpack 'adjust-mkspecs-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/src.pro"
               ;; Do not attempt to install the .prf file into qtbase
               ;; "lib/qt5/mkspecs/features", ref <https://bugs.gnu.org/45031>.
               (("\\$\\$\\[QMAKE_MKSPECS\\]")
                (string-append (assoc-ref outputs "out") "/lib/qt5/mkspecs")))
             #t))
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((qca (assoc-ref inputs "qca")))
               (invoke
                "qmake"
                (string-append "PREFIX=" (assoc-ref %outputs "out"))
                (string-append "QMAKE_INCDIR+=" qca "/include/Qca-qt5/QtCrypto")
                (string-append "LIBS+=-L" qca "/lib")
                (string-append "LIBS+=-lqca-qt5"))))))))
    (home-page "https://github.com/ayoy/qoauth")
    (synopsis "Qt-based C++ library for OAuth authorization scheme")
    (description "QOAuth is an attempt to support interaction with
OAuth-powered network services in a Qt way, i.e. simply, clearly and
efficiently.  It gives the application developer no more than 4 methods.")
    (license license:lgpl2.1+)))

(define-public krona-tools
  (package
   (name "krona-tools")
   (version "2.8")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/marbl/Krona/releases/download/v"
                   version "/KronaTools-" version ".tar"))
             (sha256
              (base32
               "1h698wddb3hii68mnkby7s1x81vbhd4z1sf4ivm1lsi2nqlc1vsn"))))
   (build-system perl-build-system)
   (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There is no configure or build steps.
         (delete 'configure)
         (delete 'build)
         ;; Install script "install.pl" expects the build directory to remain
         ;; after installation, creating symlinks etc., so re-implement it
         ;; here.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin   (string-append (assoc-ref outputs "out") "/bin"))
                   (perl  (string-append (assoc-ref outputs "out")
                                         "/lib/perl5/site_perl/krona-tools/lib"))
                   (share (string-append (assoc-ref outputs "out")
                                         "/share/krona-tools")))
               (mkdir-p bin)
               (for-each
                (lambda (script)
                  (let ((executable (string-append "scripts/" script ".pl")))
                    ;; Prefix executables with 'kt' as install script does.
                    (copy-file executable (string-append bin "/kt" script))))
                '("ClassifyBLAST"
                  "GetContigMagnitudes"
                  "GetLCA"
                  "GetTaxIDFromAcc"
                  "GetTaxInfo"
                  "ImportBLAST"
                  "ImportDiskUsage"
                  "ImportEC"
                  "ImportFCP"
                  "ImportGalaxy"
                  "ImportKrona"
                  "ImportMETAREP-BLAST"
                  "ImportMETAREP-EC"
                  "ImportMGRAST"
                  "ImportPhymmBL"
                  "ImportRDP"
                  "ImportRDPComparison"
                  "ImportTaxonomy"
                  "ImportText"
                  "ImportXML"))
               (for-each
                (lambda (directory)
                  (copy-recursively directory
                                    (string-append perl "/../" directory)))
                (list "data" "img" "taxonomy" "src"))
               (install-file "lib/KronaTools.pm" perl)

               ;; Install downloaders
               (substitute* "updateAccessions.sh"
                 (("ktPath=.*") (string-append "ktPath=" share "\n")))
               (substitute* "updateTaxonomy.sh"
                 (("ktPath=.*") (string-append "ktPath=" share "\n"))
                 (("command -v curl")
                  (string-append "command -v " (which "curl")))
                 (("curl -s")
                  (string-append (which "curl") " -s"))
                 (("curl\\$timestring")
                  (string-append (which "curl") "$timestring"))
                 (("perl -M")
                  (string-append (which "perl") " -M"))
                 (("make ")
                  (string-append (which "make") " ")))
               (for-each (lambda (file)
                           (install-file file (string-append share "/scripts")))
                         '("scripts/extractTaxonomy.pl"
                           "scripts/accession2taxid.make"
                           "scripts/taxonomy.make"))
               (for-each (lambda (file)
                           (install-file file share))
                         '("updateAccessions.sh"
                           "updateTaxonomy.sh")))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "PERL5LIB")))
               (for-each
                (lambda (executable)
                  (wrap-program executable
                    `("PERL5LIB" ":" prefix
                      (,(string-append out "/lib/perl5/site_perl/krona-tools/lib")))))
                (find-files (string-append out "/bin/") ".*")))))
         (delete 'check)
         (add-after 'wrap-program 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (with-directory-excursion "data"
               (invoke (string-append (assoc-ref outputs "out") "/bin/ktImportText")
                       "ec.tsv")))))))
   (inputs
    (list bash-minimal curl gnu-make perl))
   (home-page "https://github.com/marbl/Krona/wiki")
   (synopsis "Hierarchical data exploration with zoomable HTML5 pie charts")
   (description
    "Krona is a flexible tool for exploring the relative proportions of
hierarchical data, such as metagenomic classifications, using a radial,
space-filling display.  It is implemented using HTML5 and JavaScript, allowing
charts to be explored locally or served over the Internet, requiring only a
current version of any major web browser.")
   (license license:bsd-3)))

(define-public rapidjson
  ;; Last release was in 2016, but this commit is from 2023.
  (let ((commit "949c771b03de448bdedea80c44a4a5f65284bfeb")
        (revision "1"))
    (package
      (name "rapidjson")
      (version (git-version "1.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Tencent/rapidjson")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xlj0cj88ls3avwmlhd2gf5757fjpfbqx6qf49z1mzi381gcl72m"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Remove code using the problematic JSON license (see
                    ;; <https://www.gnu.org/licenses/license-list.html#JSON>).
                    (delete-file-recursively "bin/jsonchecker")))))
      (build-system cmake-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-march=native
             (lambda _
               (substitute* "CMakeLists.txt"
                 (("-m[^-]*=native") "")))))))
      (home-page "https://github.com/Tencent/rapidjson")
      (synopsis "JSON parser/generator for C++ with both SAX/DOM style API")
      (description
       "RapidJSON is a fast JSON parser/generator for C++ with both SAX/DOM
style API.")
      (license license:expat))))

(define-public libjwt
  (package
    (name "libjwt")
    (version "1.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/benmcollins/libjwt/releases/download/v"
                           version "/libjwt-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "1bpfaa0y8bccz5hr677lkrprs07akx02k0qbf82z2c8syr24a77i"))))
    (build-system gnu-build-system)
    (inputs
     (list jansson openssl))
    (native-inputs
     (list check pkg-config))
    (home-page "https://github.com/benmcollins/libjwt")
    (synopsis "C @acronym{JWT, JSON Web Token} library")
    (description "@code{libjwt} is a @acronym{JWT, JSON Web Token} library for
C.")
    (license license:mpl2.0)))

(define-public yajl
  (package
    (name "yajl")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lloyd/yajl")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00yj06drb6izcxfxfqlhimlrb089kka0w0x8k27pyzyiq7qzcvml"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-tests
           (lambda _
             (substitute* "test/parsing/run_tests.sh"
               (("`which echo`") (which "echo"))))))))
    (home-page "https://lloyd.github.io/yajl/")
    (synopsis "C library for parsing and generating JSON")
    (description
     "@acronym{YAJL, Yet Another JSON Library} is a small event-driven
(SAX-style) JSON parser and validating generator written in ANSI C.")
    (license license:isc)))

(define-public libyajl
  (deprecated-package "libyajl" yajl))

(define-public libwebsockets
  (package
    (name "libwebsockets")
    (version "4.3.2")
    (source (origin
              ;; The project does not publish tarballs, so we have to take
              ;; things from Git.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/warmcat/libwebsockets")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0rxgb05f6jignb0y367rs88cla2s1ndd9jfl4ms77q8w0wnbq762"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs (list perl))             ; to build the HTML doc
    (inputs (list zlib openssl))
    (synopsis "WebSockets library written in C")
    (description
     "Libwebsockets is a library that allows C programs to establish client
and server WebSockets connections---a protocol layered above HTTP that allows
for efficient socket-like bidirectional reliable communication channels.")
    (home-page "https://libwebsockets.org")
    (license license:expat)))

(define-public wabt
  (package
    (name "wabt")
    (version "1.0.36")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WebAssembly/wabt")
             (commit version)
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gypy9bn2nvmfa469fi6kwsyw11j0vqkxm7givs3gidjpsy1bk0a"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "third_party/gtest/"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Tests on non-x86_64 architectures are not well supported upstream.
      #:tests? (target-x86-64?)
      #:test-target "run-tests"
      #:configure-flags '(list "-DUSE_SYSTEM_GTEST=ON")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'use-gcc
           (lambda _ (setenv "CC" "gcc")))
         ;; XXX This is the only test that fails.
         (add-after 'unpack 'delete-broken-test
           (lambda _
             (delete-file "test/wasm2c/spec/memory_init.txt"))))))
    (native-inputs (list python googletest))
    (home-page "https://github.com/WebAssembly/wabt")
    (synopsis "WebAssembly Binary Toolkit")
    (description "WABT (pronounced: wabbit) is a suite of tools for
WebAssembly, including:

@enumerate
@item @command{wat2wasm} translates from WebAssembly text format to the
WebAssembly binary format
@item @command{wasm2wat} is the inverse; it translates from the binary format
back to the text format (also known as a .wat)
@item @command{wasm-objdump} prints information about a wasm binary, similarly
to @command{objdump}.
@item @command{wasm-interp} decodes ands run a WebAssembly binary file using a
stack-based interpreter
@item @command{wat-desugar} parses .wat text form as supported by the spec
interpreter (s-expressions, flat syntax, or mixed) and prints the canonical
flat format
@item @command{wasm2c} converts a WebAssembly binary file to a C source and
header file.
@end enumerate

These tools are intended for use in (or for development of) toolchains or
other systems that want to manipulate WebAssembly files.")
    (license license:asl2.0)))

(define-public wasm3
  ;; Use an unreleased version with 'm3_GetTableFunction' for tic80.
  (let ((commit "139076a98b8321b67f850a844f558b5e91b5ac83"))
    (package
      (name "wasm3")
      (version (git-version "0.5.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wasm3/wasm3")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0329kx43n0ns9qxj813239wna69qyi9ghfb1sks46f39h24q0n3x"))))
      (build-system cmake-build-system)
      (arguments
       ;; The default WASI option "uvwasi" causes CMake to initiate a 'git
       ;; clone' which cannot happen within the build container.
       '(#:configure-flags '("-DBUILD_WASI=simple")
         ;; No check target.  There are tests but they require a network
         ;; connection to download the WebAssembly core test suite.
         #:tests? #f
         ;; There is no install target.  Instead, we have to manually copy the
         ;; wasm3 build artifacts to the output directory.
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bindir (string-append out "/bin"))
                      (includedir (string-append out "/include"))
                      (libdir (string-append out "/lib")))
                 (mkdir-p bindir)
                 (mkdir-p includedir)
                 (mkdir-p libdir)
                 (copy-file "wasm3" (string-append bindir "/wasm3"))
                 (for-each (lambda (header)
                             (copy-file header
                                        (string-append includedir "/"
                                                       (basename header))))
                           (find-files "../source/source" "\\.h$"))
                 (copy-file "source/libm3.a"
                            (string-append libdir "/libm3.a"))))))))
      (home-page "https://github.com/wasm3/wasm3")
      (synopsis "WebAssembly interpreter")
      (description "WASM3 is a fast WebAssembly interpreter.")
      (license license:expat))))

(define-public wasm-micro-runtime
  (package
    (name "wasm-micro-runtime")
    (version "2.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bytecodealliance/wasm-micro-runtime")
                    (commit (string-append "WAMR-" version))))
              (file-name (git-file-name "WAMR" version))
              (sha256
               (base32
                "0nhhsvy4a9al4lnxdkmyy63a7qd0x33s5wpcsdvl93kj6klzc6a6"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Running the tests is difficult.  The test script in
      ;; tests/wamr-test-suites insists on downloading and building wabt (even
      ;; if we provide it) and it has a hard time accepting a separately
      ;; provided clone of the https://github.com/WebAssembly/spec repository.
      ;; Future releases provide unit tests which may be easier to run.
      #:tests? #false
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "product-mini/platforms/linux"))))))
    (home-page "https://bytecodealliance.github.io/wamr.dev")
    (synopsis "WebAssembly Micro Runtime")
    (description "WebAssembly Micro Runtime (WAMR) is a lightweight standalone
WebAssembly (Wasm) runtime with small footprint, high performance and highly
configurable features for applications cross from embedded, IoT, edge to
Trusted Execution Environment (TEE), smart contract, cloud native and other
features.")
    (license license:asl2.0)))

(define-public websocketpp
  (package
    (name "websocketpp")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zaphoyd/websocketpp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ww4fhl8qf12hkv6jaild8zzsygdspxm1gqpk2f63gv1xfi31wpm"))
       (patches (search-patches "websocketpp-fix-for-cmake-3.15.patch"))))
    (build-system cmake-build-system)
    (inputs (list boost openssl))
    (arguments '(#:configure-flags '("-DBUILD_TESTS=ON")
                 #:phases
                 (modify-phases %standard-phases
                   (add-after 'install 'remove-tests
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((install-dir (assoc-ref outputs "out"))
                              (bin-dir (string-append install-dir "/bin")))
                         (delete-file-recursively bin-dir)
                         #t))))))
    (home-page "https://www.zaphoyd.com/websocketpp/")
    (synopsis "C++ library implementing the WebSocket protocol")
    (description "WebSocket++ is a C++ library that can be used to implement
WebSocket functionality.  The goals of the project are to provide a WebSocket
implementation that is simple, portable, flexible, lightweight, low level, and
high performance.")
    (license license:bsd-3)))

(define-public wslay
  (package
    (name "wslay")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tatsuhiro-t/wslay")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w32iljg4inqf0712w5fxxhggvmjh6ipl2lnz0h36dv1xrj0d964"))))
    (build-system gnu-build-system)
    (arguments
     ;; Parallel builds don't reliably succeed.
     `(#:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-sphinx-error
           ;; XXX: Remove in next version: fix applied upstream.  See
           ;; <https://github.com/tatsuhiro-t/wslay/commit/43fda1207ea5977043630500e0c8e77b98b35320>.
           (lambda _
             (substitute* "doc/sphinx/conf.py.in"
               (("add_stylesheet") "add_css_file")))))))
    (native-inputs
     (list autoconf
           automake
           cunit ; for tests
           libtool
           pkg-config
           python-sphinx))
    (home-page "https://tatsuhiro-t.github.io/wslay/")
    (synopsis "C WebSocket library")
    (description "@code{Wslay} is an event-based C library for the WebSocket
protocol version 13, described in RFC 6455.  Besides a high-level API it
provides callbacks for sending and receiving frames directly.  @code{Wslay}
only supports the data transfer part of WebSocket protocol and does not
perform the opening handshake in HTTP.")
    (license license:expat)))

(define-public libpsl
  (package
    (name "libpsl")
    (version "0.21.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rockdaboot/libpsl/"
                                  "releases/download/" version
                                  "/libpsl-" version ".tar.gz"))
              (sha256
               (base32
                "0k0d46bbh1jj2ll369f134vciplrzbqkg7fv9m62bl6lzghy2v5c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)

       ;; For tests.
       ("python" ,python-wrapper)))
    (inputs
     (list libidn2 libunistring))
    (home-page "https://github.com/rockdaboot/libpsl")
    (synopsis "C library for the Publix Suffix List")
    (description
     "A \"public suffix\" is a domain name under which Internet users can
directly register own names.

Browsers and other web clients can use it to avoid privacy-leaking
\"supercookies\", avoid privacy-leaking \"super domain\" certificates, domain
highlighting parts of the domain in a user interface, and sorting domain lists
by site.

Libpsl has built-in PSL data for fast access, allowing to load PSL data from
files, checks if a given domain is a public suffix, provides immediate cookie
domain verification, finds the longest public part of a given domain, finds
the shortest private part of a given domain, works with international
domains (UTF-8 and IDNA2008 Punycode), is thread-safe, and handles IDNA2008
UTS#46.")
    (license license:x11)))

(define-public esbuild
  (package
    (name "esbuild")
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/evanw/esbuild")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j99m7rdql6iq3llrr8bm85hq34ssc8bmb6vhwr1ibgspjl0jd3k"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Remove prebuilt binaries
            (delete-file-recursively "npm")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/evanw/esbuild/cmd/esbuild"
      #:unpack-path "github.com/evanw/esbuild"
      #:test-flags #~(list #$(if (and (target-64bit?)
                                      ;; The -race option is not supported on riscv64
                                      (not (target-riscv64?)))
                                 "-race" "-short"))
      ;; Test subdirectories are compiled from #:import-path.
      #:test-subdirs #~(list "../../internal/..." "../../pkg/..." )))
    (inputs
     (list go-golang-org-x-sys-for-esbuild))
    (home-page "https://esbuild.github.io/")
    (synopsis "Bundler and minifier tool for JavaScript and TypeScript")
    (description
     "The esbuild tool provides a unified bundler, transpiler and minifier.
It packages up JavaScript and TypeScript code, along with JSON and other data,
for distribution on the web.")
    (license license:expat)))

(define-public tinyproxy
  (package
    (name "tinyproxy")
    (version "1.11.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/tinyproxy/tinyproxy/"
                                  "releases/download/" version "/tinyproxy-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0sm2i05sq2mkyix7dsvm9abb3vr2nnciqywmwa3wk4b6f206h4ka"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"             ; ‘make check’ silently does nothing
       #:configure-flags
       (list
        ;; For the log file, etc.
        "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda* (#:key inputs #:allow-other-keys #:rest args)
             ;; Uncommenting the next two lines may assist in debugging
             ;; (substitute* "docs/man5/Makefile" (("a2x") "a2x -v"))
             ;; (setenv "XML_DEBUG_CATALOG" "1")
             #t)))))
    (native-inputs
     (list perl))                 ; for tests
    (home-page "https://tinyproxy.github.io/")
    (synopsis "Light-weight HTTP/HTTPS proxy daemon")
    (description "Tinyproxy is a light-weight HTTP/HTTPS proxy
daemon.  Designed from the ground up to be fast and yet small, it is an ideal
solution for use cases such as embedded deployments where a full featured HTTP
proxy is required, but the system resources for a larger proxy are
unavailable.")
    (license license:gpl2+)))

(define-public polipo
  (package
    (name "polipo")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.pps.univ-paris-diderot.fr/~jch/software/files/polipo/polipo-"
             version ".tar.gz"))
       (sha256
        (base32
         "05g09sg9qkkhnc2mxldm1w1xkxzs2ylybkjzs28w8ydbjc3pand2"))))
    (native-inputs (list texinfo))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)
                            (string-append "LOCAL_ROOT="
                                           out "/share/polipo/www")
                            "CC=gcc"))
       ;; No 'check' target.
       #:tests? #f))
    (home-page "https://www.pps.univ-paris-diderot.fr/~jch/software/polipo/")
    (synopsis "Small caching web proxy")
    (description
     "Polipo is a small caching web proxy (web cache, HTTP proxy, and proxy
server).  It was primarily designed to be used by one person or a small group
of people.")
    (license license:expat)))

(define-public websockify
  (package
    (name "websockify")
    (version "0.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/novnc/websockify")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ysqylpyv17s52634wn3vrwf7y9b5ig7fdfv8vwj1272lvv68qgk"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-nose2 python-setuptools python-wheel))
    (inputs
     (list python-jwcrypto
           python-numpy
           python-redis
           python-requests
           ;; TODO: Remove simplejson for versions > 0.11.0.
           python-simplejson))
    (home-page "https://github.com/novnc/websockify")
    (synopsis "WebSockets support for any application/server")
    (description "Websockify translates WebSockets traffic to normal socket
traffic.  Websockify accepts the WebSockets handshake, parses it, and then
begins forwarding traffic between the client and the target in both
directions.")
    (license license:lgpl3)))

;; This is a variant of esbuild that builds and installs the nodejs API.
;; Eventually, this should probably be merged with the esbuild package.
(define-public esbuild-node
  (package
    (inherit esbuild)
    (name "esbuild-node")
    (version "0.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/evanw/esbuild")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09r1xy0kk6c9cpz6q0mxr4why373pwxbm439z2ihq3k1d5kk7x4w"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove prebuilt binaries
        '(delete-file-recursively "lib/npm/exit0"))))
    (arguments
     (list
      #:import-path "github.com/evanw/esbuild/cmd/esbuild"
      #:unpack-path "github.com/evanw/esbuild"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'build-platform
            (lambda* (#:key unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                ;; Must be writable.
                (for-each make-file-writable (find-files "." "."))
                (invoke "node" "scripts/esbuild.js"
                        (string-append #$output "/bin/esbuild"))
                (let ((modules (string-append #$output "/lib/node_modules/esbuild")))
                  (mkdir-p modules)
                  (copy-recursively "npm/esbuild" modules)))))
          (replace 'check
            (lambda* (#:key tests? unpack-path #:allow-other-keys)
              (when tests?
                ;; The "Go Race Detector" is only supported on 64-bit
                ;; platforms, this variable disables it.
                ;; TODO: Causes too many rebuilds, rewrite to limit to x86_64,
                ;; aarch64 and ppc64le.
                #$(if (target-riscv64?)
                      `(setenv "ESBUILD_RACE" "")
                      #~(unless #$(target-64bit?)
                          (setenv "ESBUILD_RACE" "")))
                (with-directory-excursion (string-append "src/" unpack-path)
                  (invoke "make" "test-go"))))))))
    (native-inputs
     (list go-github-com-kylelemons-godebug node-lts))))

(define-public wwwoffle
  (package
    (name "wwwoffle")
    (version "2.9j")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.gedanken.org.uk/software/"
                                  "wwwoffle/download/wwwoffle-"
                                  version ".tgz"))
              (sha256
               (base32
                "1ihil1xq9dp21hf108khxbw6f3baq0w5c0j3af038y6lkmad4vdi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-gnutls")
       #:tests? #f))                         ; no test target
    (native-inputs (list flex))
    (inputs `(("gnutls" ,gnutls)
              ("libcrypt" ,libgcrypt)))
    (home-page "https://www.gedanken.org.uk/software/wwwoffle/")
    (synopsis "Caching web proxy optimized for intermittent internet links")
    (description "WWWOFFLE is a proxy web server that is especially good for
intermittent internet links.  It can cache HTTP, HTTPS, FTP, and finger
protocols, and supports browsing and requesting pages while offline, indexing,
modifying pages and incoming and outgoing headers, monitoring pages for
changes, and much more.")
    (license license:gpl2+)))

(define-public libjuice
  (package
    (name "libjuice")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/paullouisageneau/libjuice")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "035kqyp5bcawzkpqpjb1qjwqf0dp2gm9h2j9py5iiahcvfjwh70i"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ; requires internet access
    (home-page "https://github.com/paullouisageneau/libjuice")
    (synopsis "UDP Interactive Connectivity Establishment library")
    (description "@code{libjuice} opens bidirectionnal User Datagram
Protocol (UDP) streams with Network Address Translator (NAT) traversal.  It's a
simplified implementation of the Interactive Connectivity Establishment (ICE)
protocol, client-side and server-side, written in C without dependencies for
POSIX platforms.")
    (license license:mpl2.0)))

(define-public libdatachannel
  (package
    (name "libdatachannel")
    (version "0.21.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/paullouisageneau/libdatachannel")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11icbyd71dw5ywjdviq580xvad24yfsjj3c5zpjqsxc883i40dxi"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                  ; requires internet access
           #:configure-flags
           #~'("-DPREFER_SYSTEM_LIB=ON")))
    (inputs (list libjuice libsrtp nlohmann-json openssl plog usrsctp))
    (home-page "https://libdatachannel.org/")
    (synopsis "WebRTC Data Channels and WebSockets library")
    (description "@code{libdatachannel} is a standalone implementation of WebRTC
Data Channels, WebRTC Media Transport, and WebSockets in C++17 with C bindings
for POSIX platforms.  WebRTC is a W3C and IETF standard enabling real-time
peer-to-peer data and media exchange between two devices.")
    (license license:mpl2.0)))

(define-public liboauth
  (package
    (name "liboauth")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/liboauth/liboauth-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "07w1aq8y8wld43wmbk2q8134p3bfkp2vma78mmsfgw2jn1bh3xhd"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags ''("--enable-nss")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-C-unicode-locale
            (lambda _
              (substitute* "tests/commontest.c"
                (("en_US\\.UTF-8")
                 "C.UTF-8")))))))
    (native-inputs (list pkg-config))
    (propagated-inputs
     (list curl nss))
    (home-page "https://sourceforge.net/projects/liboauth")
    (synopsis "C library implementing the OAuth API")
    (description
     "liboauth is a collection of C functions implementing the OAuth API.
liboauth provides functions to escape and encode strings according to OAuth
specifications and offers high-level functionality built on top to sign
requests or verify signatures using either NSS or OpenSSL for calculating the
hash/signatures.")
    ;; Source code may be distributed under either license.
    (license (list license:expat license:gpl2+))))

(define-public libquvi-scripts
  (package
    (name "libquvi-scripts")
    (version "0.4.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/quvi/" (version-major+minor version) "/"
             name "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0d0giry6bb57pnidymvdl7i5x9bq3ljk3g4bs294hcr5mj3cq0kw"))))
    (build-system gnu-build-system)
    (home-page "https://quvi.sourceforge.net/")
    (synopsis "Media stream URL parser")
    (description "This package contains support scripts called by libquvi to
parse media stream properties.")
    (license license:lgpl2.1+)))

(define-public libquvi
  (package
    (name "libquvi")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/quvi/" (version-major+minor version) "/" name "/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "00x9gbmzc5cns0gnfag0hsphcr3cb33vbbb9s7ppvvd6bxz2z1mm"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs
     (list curl
           cyrus-sasl
           libquvi-scripts
           lua-5.1
           openssl
           zlib))
    (arguments
     ;; Lua provides no .pc file, so add CFLAGS/LIBS manually.
     '(#:configure-flags
       (let ((lua (assoc-ref %build-inputs "lua")))
         (list
          (string-append "liblua_CFLAGS=-I" lua "/include")
          (string-append "liblua_LIBS=-L" lua "/libs -llua")))))
    (home-page "https://quvi.sourceforge.net/")
    (synopsis "Media stream URL parser")
    (description "libquvi is a library with a C API for parsing media stream
URLs and extracting their actual media files.")
    (license license:lgpl2.1+)))

(define-public quvi
  (package
    (name "quvi")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/" name "/"  (version-major+minor version)
             "/" name "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "09lhl6dv5zpryasx7yjslfrcdcqlsbwapvd5lg7w6sm5x5n3k8ci"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs
     (list curl libquvi))
    (home-page "https://quvi.sourceforge.net/")
    (synopsis "Media stream URL parser")
    (description "quvi is a command-line-tool suite to extract media files
from streaming URLs.  It is a command-line wrapper for the libquvi library.")
    (license license:lgpl2.1+)))

(define-public serf
  (package
    (name "serf")
    (version "1.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/serf/serf-"
                           version ".tar.bz2"))
       (patches (search-patches "serf-python3.patch"))
       (sha256
        (base32 "1k47gbgpp52049andr28y28nbwh9m36bbb0g8p0aka3pqlhjv72l"))))
    (build-system scons-build-system)
    (propagated-inputs
     (list apr apr-util openssl-1.1))
    (inputs
     (list ;; TODO: Fix build with gss.
           ;;("gss" ,gss)
           zlib))
    (arguments
     `(#:scons-flags (list (string-append "APR=" (assoc-ref %build-inputs "apr"))
                           (string-append "APU=" (assoc-ref %build-inputs "apr-util"))
                           (string-append "OPENSSL=" (assoc-ref %build-inputs "openssl"))
                           ;; (string-append "GSSAPI=" (assoc-ref %build-inputs "gss"))
                           (string-append "ZLIB=" (assoc-ref %build-inputs "zlib"))
                           (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scons-propagate-environment
                    (lambda _
                      ;; By design, SCons does not, by default, propagate
                      ;; environment variables to subprocesses.  See:
                      ;; <http://comments.gmane.org/gmane.linux.distributions.nixos/4969>
                      ;; Here, we modify the SConstruct file to arrange for
                      ;; environment variables to be propagated.
                      (substitute* "SConstruct"
                        (("^env = Environment\\(")
                         "env = Environment(ENV=os.environ, "))))
         (add-before 'check 'disable-broken-tests
           (lambda _
             ;; These tests rely on SSL certificates that expired 2017-04-18.
             ;; While there are newer certs available upstream, we don't want
             ;; this package to suddenly "expire" some time in the future.
             ;; https://bugs.gnu.org/26671
             (let ((broken-tests
                    '("test_ssl_trust_rootca"
                      "test_ssl_certificate_chain_with_anchor"
                      "test_ssl_certificate_chain_all_from_server"
                      "test_ssl_no_servercert_callback_allok"
                      "test_ssl_large_response"
                      "test_ssl_large_request"
                      "test_ssl_client_certificate"
                      "test_ssl_future_server_cert"
                      "test_setup_ssltunnel"
                      "test_ssltunnel_basic_auth"
                      "test_ssltunnel_basic_auth_server_has_keepalive_off"
                      "test_ssltunnel_basic_auth_proxy_has_keepalive_off"
                      "test_ssltunnel_basic_auth_proxy_close_conn_on_200resp"
                      "test_ssltunnel_digest_auth")))
               (for-each
                (lambda (test)
                  (substitute* "test/test_context.c"
                    (((string-append "SUITE_ADD_TEST\\(suite, " test "\\);")) "")))
                broken-tests)
               #t))))))
    (home-page "https://serf.apache.org/")
    (synopsis "High-performance asynchronous HTTP client library")
    (description
     "serf is a C-based HTTP client library built upon the Apache Portable
Runtime (APR) library.  It multiplexes connections, running the read/write
communication asynchronously.  Memory copies and transformations are kept to a
minimum to provide high performance operation.")
    ;; Most of the code is covered by the Apache License, Version 2.0, but the
    ;; bundled CuTest framework uses a different non-copyleft license.
    (properties
     '((release-monitoring-url . "https://serf.apache.org/download")))
    (license (list license:asl2.0 (license:non-copyleft "file://test/CuTest-README.txt")))))

(define-public libsass
  (package
    (name "libsass")
    ;; When updating, check whether sassc/libsass-3.5 is still needed.
    (version "3.6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sass/libsass")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r8lfqvr3rjhjd8r036zd1wc9q17gyiskppcw9m13jks9an7xp4j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'set-LIBSASS_VERSION
           (lambda _
             (setenv "LIBSASS_VERSION" ,version)
             #t)))))
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://sass-lang.com/libsass")
    (synopsis "SASS Compiler, implemented as a C/C++ library")
    (description
     "LibSass is a @acronym{SASS,Syntactically awesome style sheets} compiler
library designed for portability and efficiency.  To actually compile SASS
stylesheets, you'll need to use another program that uses this library,
@var{sassc} for example.")
    (license license:expat)))

(define-public sassc
  (package
    (name "sassc")
    (version "3.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sass/sassc")
                    (commit  version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m7flrs0hz3ivib8kvsgn3d0fgkabqviadkp1dyspa6iibx3gjwd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile
           (lambda _
             (substitute* "Makefile"
               (("build-shared: \\$\\(RESOURCES\\) \\$\\(OBJECTS\\) \\$\\(LIB_SHARED\\)")
                "build-shared: $(RESOURCES) $(OBJECTS)")
               (("\\$\\(SASSC_EXE\\): libsass build")
                "$(SASSC_EXE): build")
               (("install: libsass-install-\\$\\(BUILD\\) \\\\")
                "install: \\"))
             #t))
         ;; This phase fails because…
         (delete 'bootstrap)
         ;; …there is no configure script to be generated.
         (delete 'configure)
         (add-before 'build 'setup-environment
           (lambda _
             (setenv "BUILD" "shared")
             (setenv "SASSC_VERSION" ,version)
             #t)))))
    (inputs
     (list libsass))
    (synopsis "CSS pre-processor")
    (description "SassC is a compiler written in C for the CSS pre-processor
language known as SASS.")
    (home-page "https://sass-lang.com/libsass")
    (license license:expat)))

(define-public sassc/libsass-3.5
  ;; Newer libsass versions suffor from a memory leak when building (some?)
  ;; GTK themes <https://github.com/sass/libsass/issues/3033>.
  (package
    (inherit sassc)
    (name "sassc")
    (inputs
     (list (package (inherit libsass)
                    (name "libsass")
                    (version "3.5.5")
                    (source (origin (method git-fetch)
                                    (uri (git-reference (url
                                                             "https://github.com/sass/libsass")
                                                        (commit version)))
                                    (file-name (git-file-name name version))
                                    (sha256 (base32
                                                    "0830pjcvhzxh6yixj82x5k5r1xnadjqzi16kp53213icbly0r9ma")))))))
    (properties '((hidden? . #t)))))


(define-public perl-apache-logformat-compiler
  (package
    (name "perl-apache-logformat-compiler")
    (version "0.36")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KA/KAZEBURO/"
                           "Apache-LogFormat-Compiler-" version ".tar.gz"))
       (sha256
        (base32 "05xcl7j65vakx7x79jqjikyw0nzf60bc2w6hhc0q5sklxq1ral4l"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-http-message perl-module-build-tiny perl-test-mocktime
           perl-try-tiny perl-uri))
    (propagated-inputs
     (list perl-posix-strftime-compiler))
    (arguments `(#:tests? #f))          ; TODO: Timezone test failures
    (home-page "https://metacpan.org/release/Apache-LogFormat-Compiler")
    (synopsis "Compile a log format string to perl-code")
    (description "This module provides methods to compile a log format string
to perl-code, for faster generation of access_log lines.")
    (license license:perl-license)))

(define-public perl-authen-sasl
  (package
    (name "perl-authen-sasl")
    (version "2.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GB/GBARR/"
                           "Authen-SASL-" version ".tar.gz"))
       (sha256
        (base32
         "02afhlrdq5hh5g8b32fa79fqq5i76qzwfqqvfi9zi57h31szl536"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           ;; Fix the build with Perl 5.26.0. Try removing this phase for later
           ;; versions of perl-authen-sasl.
           (lambda _ (setenv "PERL_USE_UNSAFE_INC" "1") #t)))))
    (propagated-inputs
     (list perl-digest-hmac perl-gssapi))
    (home-page "https://metacpan.org/release/Authen-SASL")
    (synopsis "SASL authentication framework")
    (description "Authen::SASL provides an SASL authentication framework.")
    (license license:perl-license)))

(define-public perl-catalyst-action-renderview
  (package
    (name "perl-catalyst-action-renderview")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Action-RenderView-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0j1rrld13cjk7ks92b5hv3xw4rfm2lvmksb4rlzd8mx0a0wj0rc5"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-http-request-ascgi perl-module-install))
    (propagated-inputs
     (list perl-catalyst-runtime perl-data-visitor perl-mro-compat))
    (home-page "https://metacpan.org/release/Catalyst-Action-RenderView")
    (synopsis "Sensible default Catalyst action")
    (description "This Catalyst action implements a sensible default end
action, which will forward to the first available view.")
    (license license:perl-license)))

(define-public perl-catalyst-action-rest
  (package
    (name "perl-catalyst-action-rest")
    (version "1.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/J/JJ/JJNAPIORK/"
                                  "Catalyst-Action-REST-" version ".tar.gz"))
              (sha256
               (base32
                "086bykggzalbjfk0islac4b48g9s2ypj7y81d6ns1lq0aax1py6c"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-requires perl-module-install))
    (propagated-inputs
     (list perl-catalyst-runtime
           perl-class-inspector
           perl-config-general
           perl-cpanel-json-xs
           perl-libwww
           perl-moose
           perl-mro-compat
           perl-namespace-autoclean
           perl-params-validate
           perl-uri-find
           perl-xml-simple))
    (home-page "https://metacpan.org/release/Catalyst-Action-REST")
    (synopsis "Automated REST Method Dispatching")
    (description "This Action handles doing automatic method dispatching for
REST requests.  It takes a normal Catalyst action, and changes the dispatch to
append an underscore and method name.  First it will try dispatching to an
action with the generated name, and failing that it will try to dispatch to a
regular method.")
    (license license:perl-license)))

(define-public perl-catalyst-authentication-store-dbix-class
  (package
    (name "perl-catalyst-authentication-store-dbix-class")
    (version "0.1506")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Catalyst-Authentication-Store-DBIx-Class-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0i5ja7690fs9nhxcij6lw51j804sm8s06m5mvk1n8pi8jljrymvw"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-catalyst-plugin-authorization-roles
           perl-catalyst-plugin-session-state-cookie perl-dbd-sqlite
           perl-module-install perl-test-www-mechanize-catalyst))
    (propagated-inputs
     (list perl-catalyst-runtime perl-catalyst-plugin-authentication
           perl-dbix-class perl-catalyst-model-dbic-schema))
    (home-page
     "https://metacpan.org/release/Catalyst-Authentication-Store-DBIx-Class")
    (synopsis "Storage class for Catalyst authentication using DBIx::Class")
    (description "The Catalyst::Authentication::Store::DBIx::Class class
provides access to authentication information stored in a database via
DBIx::Class.")
    (license license:perl-license)))

(define-public perl-catalyst-component-instancepercontext
  (package
    (name "perl-catalyst-component-instancepercontext")
    (version "0.001001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRODITI/"
                           "Catalyst-Component-InstancePerContext-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0wfj4vnn2cvk6jh62amwlg050p37fcwdgrn9amcz24z6w4qgjqvz"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install))
    (propagated-inputs
     (list perl-catalyst-runtime perl-moose))
    (home-page
     "https://metacpan.org/release/Catalyst-Component-InstancePerContext")
    (synopsis "Create only one instance of Moose component per context")
    (description "Catalyst::Component::InstancePerContext returns a new
instance of a component on each request.")
    (license license:perl-license)))

(define-public perl-catalyst-devel
  (package
    (name "perl-catalyst-devel")
    (version "1.42")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Catalyst-Devel-" version ".tar.gz"))
       (sha256
        (base32 "1gcaqivyxwsdq87v9za1ijjibh6llirzqsbpwjbw1f5mravg1iky"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-file-sharedir-install perl-test-fatal))
    (propagated-inputs
     (list perl-catalyst-action-renderview
           perl-catalyst-plugin-configloader
           perl-catalyst-plugin-static-simple
           perl-catalyst-runtime
           perl-config-general
           perl-file-changenotify
           perl-file-copy-recursive
           perl-file-sharedir
           perl-module-install
           perl-moose
           perl-moosex-emulate-class-accessor-fast
           perl-namespace-autoclean
           perl-namespace-clean
           perl-path-class
           perl-template-toolkit))
    (home-page "https://metacpan.org/release/Catalyst-Devel")
    (synopsis "Catalyst Development Tools")
    (description "The Catalyst-Devel distribution includes a variety of
modules useful for the development of Catalyst applications, but not required
to run them.  Catalyst-Devel includes the Catalyst::Helper system, which
autogenerates scripts and tests; Module::Install::Catalyst, a Module::Install
extension for Catalyst; and requirements for a variety of development-related
modules.")
    (license license:perl-license)))

(define-public perl-catalyst-dispatchtype-regex
  (package
    (name "perl-catalyst-dispatchtype-regex")
    (version "5.90035")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MG/MGRIMES/"
                           "Catalyst-DispatchType-Regex-" version ".tar.gz"))
       (sha256
        (base32
         "06jq1lmpq88rmp9zik5gqczg234xac0hiyc3l698iif7zsgcyb80"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build ;needs Module::Build >= 0.4004
           perl-namespace-autoclean perl-catalyst-runtime))
    (propagated-inputs
     (list perl-moose perl-text-simpletable))
    (home-page "https://metacpan.org/release/Catalyst-DispatchType-Regex")
    (synopsis "Regex DispatchType for Catalyst")
    (description "Dispatch type managing path-matching behaviour using
regexes.  Regex dispatch types have been deprecated and removed from Catalyst
core.  It is recommend that you use Chained methods or other techniques
instead.  As part of the refactoring, the dispatch priority of Regex vs Regexp
vs LocalRegex vs LocalRegexp may have changed.  Priority is now influenced by
when the dispatch type is first seen in your application.")
    (license license:perl-license)))

(define-public perl-catalyst-model-dbic-schema
  (package
  (name "perl-catalyst-model-dbic-schema")
  (version "0.65")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://cpan/authors/id/G/GB/GBJK/"
                          "Catalyst-Model-DBIC-Schema-"
                          version ".tar.gz"))
      (sha256
        (base32
          "1spfjcjc0b9dv3k2gbanqj1m1cqzyxb32p76dhdwizzpbvpi3a96"))))
  (build-system perl-build-system)
  (native-inputs
   (list perl-dbd-sqlite perl-module-install perl-test-exception
         perl-test-requires))
  (propagated-inputs
   (list perl-carp-clan
         perl-catalyst-component-instancepercontext
         perl-catalyst-runtime
         perl-catalystx-component-traits
         perl-dbix-class
         perl-dbix-class-cursor-cached
         perl-dbix-class-schema-loader
         perl-hash-merge
         perl-list-moreutils
         perl-module-runtime
         perl-moose
         perl-moosex-markasmethods
         perl-moosex-nonmoose
         perl-moosex-types
         perl-moosex-types-loadableclass
         perl-namespace-autoclean
         perl-namespace-clean
         perl-tie-ixhash
         perl-try-tiny))
  (home-page "https://metacpan.org/release/Catalyst-Model-DBIC-Schema")
  (synopsis "DBIx::Class::Schema Model Class")
  (description "This is a Catalyst Model for DBIx::Class::Schema-based
Models.")
  (license license:perl-license)))

(define-public perl-catalyst-plugin-accesslog
  (package
    (name "perl-catalyst-plugin-accesslog")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AR/ARODLAND/"
                           "Catalyst-Plugin-AccessLog-" version ".tar.gz"))
       (sha256
        (base32
         "0811rj45q4v2y8wka3wb9d5m4vbyhcmkvddf2wz4x69awzjbhgc7"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-catalyst-runtime perl-datetime perl-moose
           perl-namespace-autoclean))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-AccessLog")
    (synopsis "Request logging from within Catalyst")
    (description "This Catalyst plugin enables you to create \"access logs\"
from within a Catalyst application instead of requiring a webserver to do it
for you.  It will work even with Catalyst debug logging turned off.")
    (license license:perl-license)))

(define-public perl-catalyst-plugin-authentication
  (package
    (name "perl-catalyst-plugin-authentication")
    (version "0.10023")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Plugin-Authentication-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0v6hb4r1wv3djrnqvnjcn3xx1scgqzx8nyjdg9lfc1ybvamrl0rn"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install))
    (propagated-inputs
     (list perl-catalyst-plugin-session
           perl-catalyst-runtime
           perl-class-inspector
           perl-moose
           perl-moosex-emulate-class-accessor-fast
           perl-mro-compat
           perl-namespace-autoclean
           perl-string-rewriteprefix
           perl-test-exception
           perl-try-tiny))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-Authentication")
    (synopsis "Infrastructure plugin for the Catalyst authentication framework")
    (description "The authentication plugin provides generic user support for
Catalyst apps.  It is the basis for both authentication (checking the user is
who they claim to be), and authorization (allowing the user to do what the
system authorises them to do).")
    (license license:perl-license)))

(define-public perl-catalyst-plugin-authorization-roles
  (package
    (name "perl-catalyst-plugin-authorization-roles")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Plugin-Authorization-Roles-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0l83lkwmq0lngwh8b1rv3r719pn8w1gdbyhjqm74rnd0wbjl8h7f"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install perl-test-exception))
    (propagated-inputs
     (list perl-catalyst-plugin-authentication perl-catalyst-runtime
           perl-set-object perl-universal-isa))
    (home-page
     "https://metacpan.org/release/Catalyst-Plugin-Authorization-Roles")
    (synopsis "Role-based authorization for Catalyst")
    (description "Catalyst::Plugin::Authorization::Roles provides role-based
authorization for Catalyst based on Catalyst::Plugin::Authentication.")
    (license license:perl-license)))

(define-public perl-catalyst-plugin-captcha
  (package
    (name "perl-catalyst-plugin-captcha")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DI/DIEGOK/"
                           "Catalyst-Plugin-Captcha-" version ".tar.gz"))
       (sha256
        (base32
         "0llyj3v5nx9cx46jdbbvxf1lc9s9cxq5ml22xmx3wkb201r5qgaa"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-catalyst-plugin-session perl-catalyst-runtime
           perl-gd-securityimage perl-http-date))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-Captcha")
    (synopsis "Captchas for Catalyst")
    (description "This plugin creates and validates Captcha images for
Catalyst.")
    (license license:perl-license)))

(define-public perl-catalyst-plugin-configloader
  (package
    (name "perl-catalyst-plugin-configloader")
    (version "0.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Catalyst-Plugin-ConfigLoader-"
                           version ".tar.gz"))
       (sha256
        (base32 "0w8r3bbxqnlykvra6sx3sh3wh8ylkj914xg5ql6nw11ddy56jaly"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-path-class perl-module-install))
    (propagated-inputs
     (list perl-catalyst-runtime perl-config-any perl-data-visitor
           perl-mro-compat))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-ConfigLoader")
    (synopsis "Load config files of various types")
    (description "This module will attempt to load find and load configuration
files of various types.  Currently it supports YAML, JSON, XML, INI and Perl
formats.")
    (license license:perl-license)))

(define-public perl-catalyst-plugin-session
  (package
    (name "perl-catalyst-plugin-session")
    (version "0.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JJ/JJNAPIORK/"
                           "Catalyst-Plugin-Session-" version ".tar.gz"))
       (sha256
        (base32 "0a451997zc2vjx7rvndgx1ldbrpic8sfbddyvncynh0zr8bhlqc5"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install perl-test-deep perl-test-exception))
    (propagated-inputs
     (list perl-catalyst-runtime
           perl-moose
           perl-moosex-emulate-class-accessor-fast
           perl-mro-compat
           perl-namespace-clean
           perl-object-signature
           perl-test-www-mechanize-psgi))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-Session")
    (synopsis "Catalyst generic session plugin")
    (description "This plugin links the two pieces required for session
management in web applications together: the state, and the store.")
    (license license:perl-license)))

(define-public perl-catalyst-plugin-session-state-cookie
  (package
    (name "perl-catalyst-plugin-session-state-cookie")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MS/MSTROUT/"
                           "Catalyst-Plugin-Session-State-Cookie-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1rvxbfnpf9x2pc2zgpazlcgdlr2dijmxgmcs0m5nazs0w6xikssb"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install))
    (propagated-inputs
     (list perl-catalyst-plugin-session perl-catalyst-runtime perl-moose
           perl-mro-compat perl-namespace-autoclean))
    (home-page
     "https://metacpan.org/release/Catalyst-Plugin-Session-State-Cookie")
    (synopsis "Maintain session IDs using cookies")
    (description "In order for Catalyst::Plugin::Session to work, the session
ID needs to be stored on the client, and the session data needs to be stored
on the server.  This plugin stores the session ID on the client using the
cookie mechanism.")
    (license license:perl-license)))

(define-public perl-catalyst-plugin-session-store-fastmmap
  (package
    (name "perl-catalyst-plugin-session-store-fastmmap")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Plugin-Session-Store-FastMmap-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0x3j6zv3wr41jlwr6yb2jpmcx019ibyn11y8653ffnwhpzbpzsxs"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-cache-fastmmap
           perl-catalyst-plugin-session
           perl-catalyst-runtime
           perl-moosex-emulate-class-accessor-fast
           perl-mro-compat
           perl-path-class))
    (home-page
     "https://metacpan.org/release/Catalyst-Plugin-Session-Store-FastMmap")
    (synopsis "FastMmap session storage backend")
    (description "Catalyst::Plugin::Session::Store::FastMmap is a fast session
storage plugin for Catalyst that uses an mmap'ed file to act as a shared
memory interprocess cache.  It is based on Cache::FastMmap.")
    (license license:perl-license)))

(define-public perl-catalyst-plugin-stacktrace
  (package
    (name "perl-catalyst-plugin-stacktrace")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-Plugin-StackTrace-" version ".tar.gz"))
       (sha256
        (base32
         "1b2ksz74cpigxqzf63rddar3vfmnbpwpdcbs11v0ml89pb8ar79j"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install))
    (propagated-inputs
     (list perl-catalyst-runtime perl-devel-stacktrace perl-mro-compat))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-StackTrace")
    (synopsis "Stack trace on the Catalyst debug screen")
    (description "This plugin enhances the standard Catalyst debug screen by
including a stack trace of your application up to the point where the error
occurred.  Each stack frame is displayed along with the package name, line
number, file name, and code context surrounding the line number.")
    (license license:perl-license)))

(define-public perl-catalyst-plugin-static-simple
  (package
    (name "perl-catalyst-plugin-static-simple")
    (version "0.36")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Catalyst-Plugin-Static-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "0m4l627p2fvzr4i6sgdxhdvsx4wpa6qmaibsbxlg5x5yjs7k7drn"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install))
    (propagated-inputs
     (list perl-catalyst-runtime perl-mime-types perl-moose
           perl-moosex-types perl-namespace-autoclean))
    (home-page "https://metacpan.org/release/Catalyst-Plugin-Static-Simple")
    (synopsis "Simple serving of static pages")
    (description "The Static::Simple plugin is designed to make serving static
content in your application during development quick and easy, without
requiring a single line of code from you.  This plugin detects static files by
looking at the file extension in the URL (such as .css or .png or .js).  The
plugin uses the lightweight MIME::Types module to map file extensions to
IANA-registered MIME types, and will serve your static files with the correct
MIME type directly to the browser, without being processed through Catalyst.")
    (license license:perl-license)))

(define-public perl-catalyst-runtime
  (package
    (name "perl-catalyst-runtime")
    (version "5.90124")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JJ/JJNAPIORK/"
                           "Catalyst-Runtime-" version ".tar.gz"))
       (sha256
        (base32
         "001yk1i0xwn4v308qx15nvnp6v9qfdigdlvz1rgw5zpnq7kwnq1a"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-fatal))
    (propagated-inputs
     (list perl-cgi-simple
           perl-cgi-struct
           perl-class-c3-adopt-next
           perl-class-date
           perl-class-load
           perl-data-dump
           perl-http-body
           perl-http-message
           perl-json-maybexs
           perl-libwww
           perl-module-pluggable
           perl-moose
           perl-moosex-emulate-class-accessor-fast
           perl-moosex-getopt
           perl-moosex-methodattributes
           perl-namespace-clean
           perl-path-class
           perl-perlio-utf8_strict
           perl-plack
           perl-plack-middleware-fixmissingbodyinredirect
           perl-plack-middleware-methodoverride
           perl-plack-middleware-removeredundantbody
           perl-plack-middleware-reverseproxy
           perl-plack-test-externalserver
           perl-safe-isa
           perl-string-rewriteprefix
           perl-text-simpletable
           perl-tree-simple
           perl-tree-simple-visitorfactory
           perl-try-tiny
           perl-uri
           perl-uri-ws))
    (home-page "https://metacpan.org/release/Catalyst-Runtime")
    (synopsis "The Catalyst Framework Runtime")
    (description "Catalyst is a modern framework for making web applications.
It is designed to make it easy to manage the various tasks you need to do to
run an application on the web, either by doing them itself, or by letting you
\"plug in\" existing Perl modules that do what you need.")
    (license license:perl-license)))

(define-public perl-catalyst-traitfor-request-proxybase
  (package
    (name "perl-catalyst-traitfor-request-proxybase")
    (version "0.000005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "Catalyst-TraitFor-Request-ProxyBase-"
                           version ".tar.gz"))
       (sha256
        (base32
         "02kir63d5cs2ipj3fn1qlmmx3gqi1xqzrxfr4pv5vjhjgsm0zgx7"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-catalyst-runtime perl-catalystx-roleapplicator
           perl-http-message perl-module-install))
    (propagated-inputs
     (list perl-moose perl-namespace-autoclean perl-uri))
    (home-page
     "https://metacpan.org/release/Catalyst-TraitFor-Request-ProxyBase")
    (synopsis "Replace request base with value passed by HTTP proxy")
    (description "This module is a Moose::Role which allows you more
flexibility in your application's deployment configurations when deployed
behind a proxy.  Using this module, the request base ($c->req->base) is
replaced with the contents of the X-Request-Base header.")
    (license license:perl-license)))

(define-public perl-catalyst-view-download
  (package
    (name "perl-catalyst-view-download")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GA/GAUDEON/"
                           "Catalyst-View-Download-" version ".tar.gz"))
       (sha256
        (base32
         "1qgq6y9iwfbhbkbgpw9czang2ami6z8jk1zlagrzdisy4igqzkvs"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-catalyst-runtime
           perl-module-install
           perl-test-simple
           perl-test-www-mechanize-catalyst
           perl-text-csv
           perl-xml-simple))
    (home-page "https://metacpan.org/release/Catalyst-View-Download")
    (synopsis "Download data in many formats")
    (description "The purpose of this module is to provide a method for
downloading data into many supportable formats.  For example, downloading a
table based report in a variety of formats (CSV, HTML, etc.).")
    (license license:perl-license)))

(define-public perl-catalyst-view-json
  (package
    (name "perl-catalyst-view-json")
    (version "0.37")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Catalyst-View-JSON-" version ".tar.gz"))
       (sha256
        (base32
         "1v4xkzazs743sc7cd1kxkbi99cf00a4dadyyancckcbpi9p3znn5"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install perl-yaml))
    (inputs
     (list perl-catalyst-runtime perl-json-maybexs perl-mro-compat))
    (home-page "https://metacpan.org/release/Catalyst-View-JSON")
    (synopsis "Catalyst JSON view")
    (description "Catalyst::View::JSON is a Catalyst View handler that returns
stash data in JSON format.")
    (license license:perl-license)))

(define-public perl-catalyst-view-tt
  (package
    (name "perl-catalyst-view-tt")
    (version "0.45")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Catalyst-View-TT-" version ".tar.gz"))
     (sha256
      (base32 "0jzgpkgq5pwq82zlb0nykdyk40dfpsyn9ilz91d0wpixgi9i5pr8"))))
  (build-system perl-build-system)
  (propagated-inputs
   (list perl-catalyst-runtime
         perl-class-accessor
         perl-data-dump
         perl-mro-compat
         perl-path-class
         perl-template-timer
         perl-template-toolkit))
  (home-page "https://metacpan.org/release/Catalyst-View-TT")
  (synopsis "Template View Class")
  (description "This module is a Catalyst view class for the Template
Toolkit.")
  (license license:perl-license)))

(define-public perl-catalystx-component-traits
  (package
    (name "perl-catalystx-component-traits")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RK/RKITOVER/"
                           "CatalystX-Component-Traits-" version ".tar.gz"))
       (sha256
        (base32
         "0iq4ci8m6g2c4g01fvdl568y7pjz28f3widk986v3pyhr7ll8j88"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-moose perl-catalyst-runtime perl-moosex-methodattributes))
    (propagated-inputs
     (list perl-catalyst-runtime
           perl-class-load
           perl-moose
           perl-moosex-traits-pluggable
           perl-namespace-autoclean
           perl-list-moreutils))
    (home-page "https://metacpan.org/release/CatalystX-Component-Traits")
    (synopsis "Trait Loading and Resolution for Catalyst Components")
    (description "Adds a \"COMPONENT\" in Catalyst::Component method to your
Catalyst component base class that reads the optional \"traits\" parameter
from app and component config and instantiates the component subclass with
those traits using \"new_with_traits\" in MooseX::Traits from
MooseX::Traits::Pluggable.")
    (license license:perl-license)))

(define-public perl-catalystx-roleapplicator
  (package
    (name "perl-catalystx-roleapplicator")
    (version "0.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HD/HDP/"
                           "CatalystX-RoleApplicator-" version ".tar.gz"))
       (sha256
        (base32
         "0vwaapxn8g5hs2xp63c4dwv9jmapmji4272fakssvgc9frklg3p2"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-catalyst-runtime perl-moose perl-moosex-relatedclassroles))
    (home-page "https://metacpan.org/release/CatalystX-RoleApplicator")
    (synopsis "Apply roles to Catalyst classes")
    (description "CatalystX::RoleApplicator applies roles to Catalyst
application classes.")
    (license license:perl-license)))

(define-public perl-catalystx-script-server-starman
  (package
    (name "perl-catalystx-script-server-starman")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AB/ABRAXXA/"
                           "CatalystX-Script-Server-Starman-"
                           version ".tar.gz"))
       (sha256
        (base32
         "08jvibq4v8xjj0c3cr93h0w8w0c88ajwjn37xjy7ygxl9krlffp6"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install perl-pod-parser perl-test-www-mechanize-catalyst))
    (propagated-inputs
     (list perl-catalyst-runtime perl-moose perl-namespace-autoclean
           starman))
    (home-page "https://metacpan.org/release/CatalystX-Script-Server-Starman")
    (synopsis "Catalyst development server with Starman")
    (description "This module provides a Catalyst extension to replace the
development server with Starman.")
    (license license:perl-license)))

(define-public perl-cgi
  (package
    (name "perl-cgi")
    (version "4.55")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEEJO/"
                           "CGI-" version ".tar.gz"))
       (sha256
        (base32 "1ck4ik5i0v394qgg9qah4p6x9hyls311g6iwi6ildprzn6a5x2b7"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-nowarnings perl-test-warn))
    (propagated-inputs
     (list perl-html-parser))
    (home-page "https://metacpan.org/release/CGI")
    (synopsis "Handle Common Gateway Interface requests and responses")
    (description "CGI.pm is a stable, complete and mature solution for
processing and preparing HTTP requests and responses.  Major features include
processing form submissions, file uploads, reading and writing cookies, query
string generation and manipulation, and processing and preparing HTTP
headers.")
    (license license:perl-license)))

(define-public perl-cgi-formbuilder
  (package
    (name "perl-cgi-formbuilder")
    (version "3.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cpan.metacpan.org/authors/id/B/BI/BIGPRESH/"
             "CGI-FormBuilder-" version ".tar.gz"))
       (sha256
        (base32
         "163ixq9kninqq094z2rnkg9pv3bcmvjphlww4vksfrzhq3h9pjdf"))))
    (build-system perl-build-system)
    (inputs (list perl-cgi))
    (home-page
     "https://metacpan.org/release/CGI-FormBuilder")
    (synopsis
     "Generate and process stateful forms")
    (description
     "@code{CGI::FormBuilder} provides an easy way to generate and process CGI
form-based applications.")
    (license license:perl-license)))

(define-public perl-cgi-session
  (package
    (name "perl-cgi-session")
    (version "4.48")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MA/MARKSTOS/CGI-Session-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xsl2pz1jrh127pq0b01yffnj4mnp9nvkp88h5mndrscq9hn8xa6"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (inputs (list perl-cgi))
    (home-page
     "https://metacpan.org/release/CGI-Session")
    (synopsis
     "Persistent session data in CGI applications")
    (description
     "@code{CGI::Session} provides modular session management system across
HTTP requests.")
    (license license:perl-license)))

(define-public perl-cgi-simple
  (package
    (name "perl-cgi-simple")
    (version "1.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MANWAR/"
                           "CGI-Simple-" version ".tar.gz"))
       (sha256
        (base32 "13c7iwnnavky10ab87pi8jc1kqph03s0rhvj7myn7szhbfisc4gn"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-io-stringy ; for IO::Scalar
           perl-module-build perl-test-exception perl-test-nowarnings))
    (home-page "https://metacpan.org/release/CGI-Simple")
    (synopsis "CGI interface that is CGI.pm compliant")
    (description "CGI::Simple provides a relatively lightweight drop in
replacement for CGI.pm.  It shares an identical OO interface to CGI.pm for
parameter parsing, file upload, cookie handling and header generation.")
    (license license:perl-license)))

(define-public perl-cgi-struct
  (package
    (name "perl-cgi-struct")
    (version "1.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FU/FULLERMD/"
                           "CGI-Struct-" version ".tar.gz"))
       (sha256
        (base32
         "0v4xq2qpryr7i6jngw1wpn8yr2kiib10yxp4aih90vfdznkqsgfi"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-deep))
    (home-page "https://metacpan.org/release/CGI-Struct")
    (synopsis "Build structures from CGI data")
    (description "This is a module for building structured data from CGI
inputs, in a manner reminiscent of how PHP does.")
    (license license:bsd-2)))

(define-public perl-datetime-format-http
  (package
    (name "perl-datetime-format-http")
    (version "0.42")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CK/CKRAS/"
                           "DateTime-Format-HTTP-" version ".tar.gz"))
       (sha256
        (base32
         "0h6qqdg1yzqkdxp7hqlp0qa7d1y64nilgimxs79dys2ryjfpcknh"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-datetime perl-http-date))
    (home-page "https://metacpan.org/release/DateTime-Format-HTTP")
    (synopsis "Date conversion routines")
    (description "This module provides functions that deal with the date
formats used by the HTTP protocol.")
    (license license:perl-license)))

(define-public perl-digest-md5-file
  (package
    (name "perl-digest-md5-file")
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DM/DMUEY/"
                           "Digest-MD5-File-" version ".tar.gz"))
       (sha256
        (base32
         "060jzf45dlwysw5wsm7av1wvpl06xgk415kwwpvv89r6wda3md5d"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-libwww))
    (home-page "https://metacpan.org/release/Digest-MD5-File")
    (synopsis "MD5 sums for files and urls")
    (description "Digest::MD5::File is a Perl extension for getting MD5 sums
for files and urls.")
    (license license:perl-license)))

(define-public perl-encode-locale
  (package
    (name "perl-encode-locale")
    (version "1.05")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/Encode-Locale-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1h8fvcdg3n20c2yp7107yhdkkx78534s9hnvn7ps8hpmf4ks0vqp"))))
    (build-system perl-build-system)
    (license license:perl-license)
    (synopsis "Perl locale encoding determination")
    (description
     "The POSIX locale system is used to specify both the language
conventions requested by the user and the preferred character set to
consume and output.  The Encode::Locale module looks up the charset and
encoding (called a CODESET in the locale jargon) and arranges for the
Encode module to know this encoding under the name \"locale\".  It means
bytes obtained from the environment can be converted to Unicode strings
by calling Encode::encode(locale => $bytes) and converted back again
with Encode::decode(locale => $string).")
    (home-page "https://metacpan.org/release/Encode-Locale")))

(define-public perl-feed-find
  (package
    (name "perl-feed-find")
    (version "0.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BT/BTROTT/"
                                  "Feed-Find-" version ".tar.gz"))
              (sha256
               (base32
                "0sa33cm8ww55cymnl8j7b5yspi2y5xkkkgqqa4h6fs3wdqylz600"))))
    (build-system perl-build-system)
    (arguments
     ;; Tests expect to query files at http://stupidfool.org/perl/feeds/
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _ (setenv "PERL_USE_UNSAFE_INC" "1"))))))
    (inputs
     (list perl-class-errorhandler perl-html-parser perl-libwww perl-uri))
    (home-page "https://metacpan.org/release/Feed-Find")
    (synopsis "Syndication feed auto-discovery")
    (description "@code{Feed::Find} implements feed auto-discovery for finding
syndication feeds, given a URI.  It will discover the following feed formats:
RSS 0.91, RSS 1.0, RSS 2.0, Atom.")
    (license license:perl-license)))

(define-public perl-file-listing
  (package
    (name "perl-file-listing")
    (version "6.15")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/P/PL/PLICEASE/File-Listing-"
                   version ".tar.gz"))
             (sha256
              (base32
               "033p2ckkjbxrl390x8aq4wq4fpj5aidsazkbw82mhqxrksgzpi26"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-http-date))
    (license license:perl-license)
    (synopsis "Perl directory listing parser")
    (description
     "The File::Listing module exports a single function called parse_dir(),
which can be used to parse directory listings.")
    (home-page "https://metacpan.org/release/File-Listing")))

(define-public perl-finance-quote
  (package
   (name "perl-finance-quote")
   (version "1.65")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "https://cpan.metacpan.org/authors/id/B/BP/BPSCHUCK/"
                          "Finance-Quote-" version ".tar.gz"))
      (sha256
       (base32 "0603zricynb6n9kq7agsb03v4idlqy0rdc1nq2knzbf9l9jljyhb"))))
   (build-system perl-build-system)
   (native-inputs
     (list perl-test-harness
           perl-date-manip
           perl-date-range
           perl-date-simple
           perl-datetime
           perl-datetime-format-iso8601
           perl-string-util
           perl-pathtools
           perl-test-pod
           perl-test-pod-coverage))
   (propagated-inputs
    (list perl-datetime
          perl-datetime-format-strptime
          perl-html-parser
          perl-html-tableextract
          perl-html-tokeparser-simple
          perl-html-tree
          perl-html-treebuilder-xpath
          perl-http-cookiejar
          perl-http-cookies
          perl-http-message
          perl-io-string
          perl-json
          perl-libwww
          perl-lwp-protocol-https
          perl-mozilla-ca
          perl-spreadsheet-xlsx
          perl-readonly
          perl-string-util
          perl-text-template
          perl-try-tiny
          perl-web-scraper
          perl-xml-libxml))
   (home-page "https://metacpan.org/release/Finance-Quote")
   (synopsis "Stock and mutual fund quotes")
   (description
    "The @code{Finance::Quote} module retries stock quotes from various
internet sources, including Yahoo! Finance, Fidelity Investments, and the
Australian Stock Exchange.")
   (license license:gpl2)))

(define-public perl-gssapi
  (package
    (name "perl-gssapi")
    (version "0.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AG/AGROLMS/"
                           "GSSAPI-" version ".tar.gz"))
       (sha256
        (base32
         "1mkhwxjjlhr58pd770i9gnf7zy7jj092iv6jfbnb8bvnc5xjr3vx"))))
    (build-system perl-build-system)
    (inputs `(("gssapi" ,mit-krb5)))
    (arguments
     `(#:make-maker-flags
       `(,(string-append "--gssapiimpl=" (assoc-ref %build-inputs "gssapi")))))
    (home-page "https://metacpan.org/release/GSSAPI")
    (synopsis "Perl extension providing access to the GSSAPIv2 library")
    (description "This is a Perl extension for using GSSAPI C bindings as
described in RFC 2744.")
    (license license:perl-license)))

(define-public perl-html-element-extended
  (package
    (name "perl-html-element-extended")
    (version "1.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MS/MSISK/"
                           "HTML-Element-Extended-" version ".tar.gz"))
       (sha256
        (base32
         "0axknss8c368r5i082yhkfj8mq0w4nglfrpcxcayyzzj13qimvzk"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-html-tree))
    (home-page "https://metacpan.org/release/HTML-Element-Extended")
    (synopsis "Manipulate tables of HTML::Element")
    (description
     "HTML::Element::Extended is a Perl extension for manipulating a table
composed of HTML::Element style components.")
    (license license:perl-license)))

(define-public perl-html-form
  (package
    (name "perl-html-form")
    (version "6.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OA/OALDERS/"
                           "HTML-Form-" version ".tar.gz"))
       (sha256
        (base32 "14i4ldyvdvhdhvfhh9kiq6z853q2f84biq8vcpv1k5w2r80wdiin"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-html-parser perl-html-tagset perl-http-message
           perl-lwp-mediatypes perl-uri))
    (home-page "https://metacpan.org/release/HTML-Form")
    (synopsis "Perl class representing an HTML form element")
    (description "Objects of the HTML::Form class represents a single HTML
<form> ... </form> instance.")
    (license license:perl-license)))

(define-public perl-html-scrubber
  (package
    (name "perl-html-scrubber")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/N/NI/NIGELM/HTML-Scrubber-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "06p7w4zd42b2yh541mlzyqj40lwmvvn3fyqi8big4mf34la7m2jm"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build
           perl-test-cpan-meta
           perl-test-differences
           perl-test-eol
           perl-test-memory-cycle
           perl-test-notabs))
    (inputs
     (list perl-html-parser))
    (home-page
     "https://metacpan.org/release/HTML-Scrubber")
    (synopsis
     "Perl extension for scrubbing/sanitizing html")
    (description
     "@code{HTML::Scrubber} Perl extension for scrubbing/sanitizing HTML.")
    (license license:perl-license)))

(define-public perl-html-lint
  (package
    (name "perl-html-lint")
    (version "2.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/"
                           "HTML-Lint-" version ".tar.gz"))
       (sha256
        (base32 "0lk02xpfxcg7ij4dvpsa4wjlzhmiizj0jfr3rwmdpbj69nvc93br"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-html-parser perl-html-tagset perl-libwww))
    (home-page "https://metacpan.org/release/HTML-Lint")
    (synopsis "Check for HTML errors in a string or file")
    (description "HTML::Lint is a pure-Perl HTML parser and checker for
syntactic legitmacy.")
    (license license:artistic2.0)))

(define-public perl-html-selector-xpath
  (package
    (name "perl-html-selector-xpath")
    (version "0.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/C/CO/CORION/HTML-Selector-XPath-" version
             ".tar.gz"))
       (sha256
        (base32 "03wdwnrf0bvr2dw01njnz3a9mw2kl7ad7krh25j3vkyj7vq1f9s3"))))
    (build-system perl-build-system)
    (native-inputs (list perl-test-base
                         perl-test-pod))
    (home-page "https://metacpan.org/release/HTML-Selector-XPath")
    (synopsis "CSS Selector to XPath compiler")
    (description "@code{HTML::Selector::XPath} is a Perl module for parsing
and scraping XML/HTML documents using XPath expressions.")
    (license license:perl-license)))

(define-public perl-html-tableextract
  (package
    (name "perl-html-tableextract")
    (version "2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cpan.metacpan.org/authors/id/M/MS/MSISK/"
                           "HTML-TableExtract-" version ".tar.gz"))
       (sha256
        (base32
         "01jimmss3q68a89696wmclvqwb2ybz6xgabpnbp6mm6jcni82z8a"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-html-element-extended perl-html-parser))
    (home-page "https://metacpan.org/release/HTML-TableExtract")
    (synopsis "Extract contents from HTML tables")
    (description
     "HTML::TableExtract is a Perl module for extracting the content contained
in tables within an HTML document, either as text or encoded element trees.")
    (license license:perl-license)))

(define-public perl-html-tokeparser-simple
  (package
    (name "perl-html-tokeparser-simple")
    (version "3.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/O/OV/OVID/HTML-TokeParser-Simple-"
             version ".tar.gz"))
       (sha256
        (base32 "17aa1v62sp8ycxcicwhankmj4brs6nnfclk9z7mf1rird1f164gd"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build))
    (propagated-inputs (list perl-html-parser
                             perl-html-tagset
                             perl-sub-override))
    (home-page "https://metacpan.org/release/HTML-TokeParser-Simple")
    (synopsis "Easy to use parsing interface")
    (description "Provides @code{HTML::TokeParser::Simple}, a simpler interface
to @code{HTML::TokeParser} for parsing HTML.")
    (license license:perl-license)))

(define-public perl-html-tree
  (package
    (name "perl-html-tree")
    (version "5.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KE/KENTNL/"
                           "HTML-Tree-" version ".tar.gz"))
       (sha256
        (base32
         "1gyvm4qlwm9y6hczkpnrdfl303ggbybr0nqxdjw09hii8yw4sdzh"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-fatal))
    (propagated-inputs
     (list perl-html-parser perl-html-tagset perl-libwww))
    (home-page "https://metacpan.org/release/HTML-Tree")
    (synopsis "Work with HTML in a DOM-like tree structure")
    (description "This distribution contains a suite of modules for
representing, creating, and extracting information from HTML syntax trees.")
    (license license:perl-license)))

(define-public perl-html-parser
  (package
    (name "perl-html-parser")
    (version "3.72")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/HTML-Parser-"
                   version ".tar.gz"))
             (sha256
              (base32
               "12v05ywlnsi9lc17z32k9jxx3sj1viy7y1wpl7n4az76v7hwfa7c"))))
    (build-system perl-build-system)
    (inputs
     (list perl-html-tagset perl-http-message))
    (license license:perl-license)
    (synopsis "Perl HTML parser class")
    (description
     "Objects of the HTML::Parser class will recognize markup and separate
it from plain text (alias data content) in HTML documents.  As different
kinds of markup and text are recognized, the corresponding event handlers
are invoked.")
    (home-page "https://metacpan.org/release/HTML-Parser")))

(define-public perl-html-tagset
  (package
    (name "perl-html-tagset")
    (version "3.20")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/P/PE/PETDANCE/HTML-Tagset-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1qh8249wgr4v9vgghq77zh1d2zs176bir223a8gh3k9nksn7vcdd"))))
    (build-system perl-build-system)
    (license license:perl-license)
    (synopsis "Perl data tables useful in parsing HTML")
    (description
     "The HTML::Tagset module contains several data tables useful in various
kinds of HTML parsing operations.")
    (home-page "https://metacpan.org/release/HTML-Tagset")))

(define-public perl-html-template
  (package
    (name "perl-html-template")
    (version "2.97")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SA/SAMTREGAR/"
                                  "HTML-Template-" version ".tar.gz"))
              (sha256
               (base32
                "17qjw8swj2q4b1ic285pndgrkmvpsqw0j68nhqzpk1daydhsyiv5"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-cgi))
    (home-page "https://metacpan.org/release/HTML-Template")
    (synopsis "HTML-like templates")
    (description
     "This module attempts to make using HTML templates simple and natural.
It extends standard HTML with a few new HTML-esque tags: @code{<TMPL_VAR>},
@code{<TMPL_LOOP>}, @code{<TMPL_INCLUDE>}, @code{<TMPL_IF>},
@code{<TMPL_ELSE>} and @code{<TMPL_UNLESS>}.  The file written with HTML and
these new tags is called a template.  Using this module you fill in the values
for the variables, loops and branches declared in the template.  This allows
you to separate design from the data.")
    (license license:perl-license)))

(define-public perl-html-treebuilder-xpath
  (package
    (name "perl-html-treebuilder-xpath")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MI/MIROD/HTML-TreeBuilder-XPath-"
             version ".tar.gz"))
       (sha256
        (base32 "1wx4i1scng20n405fp3a4vrwvvq9bvbmg977wnd5j2ja8jrbvsr5"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-html-tree perl-xml-xpathengine))
    (home-page "https://metacpan.org/release/HTML-TreeBuilder-XPath")
    (synopsis "XPath support for @code{HTML::TreeBuilder}")
    (description "This module implements @code{HTML::TreeBuilder::XPath} for
@code{HTML::TreeBuilder}, making it easy to parse documents using XPath.")
    (license license:perl-license)))

(define-public perl-http-body
  (package
    (name "perl-http-body")
    (version "1.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GE/GETTY/"
                           "HTTP-Body-" version ".tar.gz"))
       (sha256
        (base32
         "15vj488i62mdp4ps9k77h39prj70i7anb6b0j8nm7l9vbdc2q3gw"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-deep))
    (propagated-inputs
     (list perl-file-temp perl-http-message)) ;For HTTP::Headers
    (home-page "https://metacpan.org/release/HTTP-Body")
    (synopsis "HTTP Body Parser")
    (description "HTTP::Body parses chunks of HTTP POST data and supports
application/octet-stream, application/json, application/x-www-form-urlencoded,
and multipart/form-data.")
    (license license:perl-license)))

(define-public perl-http-cookiejar
  (package
    (name "perl-http-cookiejar")
    (version "0.012")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "HTTP-CookieJar-" version ".tar.gz"))
       (sha256
        (base32 "0jk0ps4i67dhhhwaxwwa9nkv3n6n5w44xlnwyzvk59735pwvyjh0"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-deep perl-test-requires perl-time-mock perl-uri))
    (inputs
     (list perl-time-local perl-http-date))
    (home-page "https://metacpan.org/release/HTTP-CookieJar")
    (synopsis "Minimalist HTTP user agent cookie jar")
    (description "This module implements a minimalist HTTP user agent cookie
jar in conformance with RFC 6265 <http://tools.ietf.org/html/rfc6265>.")
    (license license:asl2.0)))

(define-public perl-http-cookies
  (package
    (name "perl-http-cookies")
    (version "6.10")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/HTTP-Cookies-"
                   version ".tar.gz"))
             (sha256
              (base32
               "01vhppq18g6ppn3z9mvdfghfzibwg1sczzvnp3jbbrjw7iikcvz3"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-http-message))
    (license license:perl-license)
    (synopsis "Perl HTTP cookie jars")
    (description
     "The HTTP::Cookies class is for objects that represent a cookie jar,
that is, a database of all the HTTP cookies that a given LWP::UserAgent
object knows about.")
    (home-page "https://metacpan.org/release/GAAS/HTTP-Cookies-6.01")))

(define-public perl-http-daemon
  (package
    (name "perl-http-daemon")
    (version "6.14")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/HTTP-Daemon-"
                   version ".tar.gz"))
             (sha256
              (base32
               "079fkcq2vdrzdf0bml52kz73n9gdv1xg0qf72c9v505v7izpwxph"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-needs))
    (propagated-inputs
     (list perl-http-message perl-lwp-mediatypes))
    (license license:perl-license)
    (synopsis "Perl simple http server class")
    (description
     "Instances of the HTTP::Daemon class are HTTP/1.1 servers that listen
on a socket for incoming requests.  The HTTP::Daemon is a subclass of
IO::Socket::INET, so you can perform socket operations directly on it too.")
    (home-page "https://metacpan.org/release/HTTP-Daemon")))

(define-public perl-http-date
  (package
    (name "perl-http-date")
    (version "6.05")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/HTTP-Date-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0awjdbz7x0jd5pna55dwxhs3k6xp3sw6b2zg3p2yndxxvya64p9n"))))
    (build-system perl-build-system)
    (license license:perl-license)
    (synopsis "Perl date conversion routines")
    (description
     "The HTTP::Date module provides functions that deal with date formats
used by the HTTP protocol (and then some more).")
    (home-page "https://metacpan.org/release/HTTP-Date")))

(define-public perl-http-lite
  (package
    (name "perl-http-lite")
    (version "2.44")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/N/NE/NEILB/HTTP-Lite-"
            version ".tar.gz"))
      (sha256
       (base32
        "0z77nflj8zdcfg70kc93glq5kmd6qxn2nf7h70x4xhfg25wkvr1q"))))
    (build-system perl-build-system)
    (native-inputs (list perl-cgi))
    (home-page "https://metacpan.org/release/HTTP-Lite")
    (synopsis "Lightweight HTTP implementation")
    (description "@code{HTTP::Lite} is a stand-alone lightweight
HTTP/1.1 implementation for perl.  It is intended for use in
situations where it is desirable to install the minimal number of
modules to achieve HTTP support.  @code{HTTP::Lite} is ideal for
CGI (or mod_perl) programs or for bundling for redistribution with
larger packages where only HTTP GET and POST functionality are
necessary.  @code{HTTP::Lite} is compliant with the Host header,
necessary for name based virtual hosting, and supports proxies.
Additionally, @code{HTTP::Lite} supports a callback to allow
processing of request data as it arrives.")
    (license license:perl-license)))

(define-public perl-http-message
  (package
    (name "perl-http-message")
    (version "6.37")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/HTTP-Message-"
                   version ".tar.gz"))
             (sha256
              (base32
               "00nq0xnpdba4valzgvzy3fgvck1ijrksdyzb4w9q6j72hl5dln8f"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-needs perl-try-tiny))
    (propagated-inputs
     (list perl-encode-locale perl-http-date perl-io-html
           perl-lwp-mediatypes perl-uri))
    (license license:perl-license)
    (synopsis "Perl HTTP style message")
    (description
     "An HTTP::Message object contains some headers and a content body.")
    (home-page "https://metacpan.org/release/ETHER/HTTP-Message-6.11")))

(define-public perl-http-negotiate
  (package
    (name "perl-http-negotiate")
    (version "6.01")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/HTTP-Negotiate-"
                   version ".tar.gz"))
             (sha256
              (base32
               "05p053vjs5g91v5cmjnny7a3xzddz5k7vnjw81wfh01ilqg9qwhw"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-http-message))
    (license license:perl-license)
    (synopsis "Perl http content negotiation")
    (description
     "The HTTP::Negotiate module provides a complete implementation of the
HTTP content negotiation algorithm specified in
draft-ietf-http-v11-spec-00.ps chapter 12.  Content negotiation allows for
the selection of a preferred content representation based upon attributes
of the negotiable variants and the value of the various Accept* header
fields in the request.")
    (home-page "https://metacpan.org/release/HTTP-Negotiate")))

(define-public perl-http-parser
  (package
    (name "perl-http-parser")
    (version "0.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ED/EDECA/"
                           "HTTP-Parser-" version ".tar.gz"))
       (sha256
        (base32
         "0idwq3jk595xil65lmxz128ha7s3r2n5zknisddpgwnqrghs3igq"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-http-message perl-uri))
    (home-page "https://metacpan.org/release/HTTP-Parser")
    (synopsis "Parse HTTP/1.1 requests")
    (description "This is an HTTP request parser.  It takes chunks of text as
received and returns a @code{hint} as to what is required, or returns the
HTTP::Request when a complete request has been read.  HTTP/1.1 chunking is
supported.")
    (license license:perl-license)))

(define-public perl-http-parser-xs
  (package
    (name "perl-http-parser-xs")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KA/KAZUHO/"
                           "HTTP-Parser-XS-" version ".tar.gz"))
       (sha256
        (base32
         "02d84xq1mm53c7jl33qyb7v5w4372vydp74z6qj0vc96wcrnhkkr"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install))
    (home-page "https://metacpan.org/release/HTTP-Parser-XS")
    (synopsis "Fast HTTP request parser")
    (description "HTTP::Parser::XS is a fast, primitive HTTP request/response
parser.")
    (license license:perl-license)))

(define-public perl-http-request-ascgi
  (package
    (name "perl-http-request-ascgi")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FL/FLORA/"
                           "HTTP-Request-AsCGI-" version ".tar.gz"))
       (sha256
        (base32
         "1smwmiarwcgq7vjdblnb6ldi2x1s5sk5p15p7xvm5byiqq3znnwl"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-class-accessor perl-http-message))
    (home-page "https://metacpan.org/release/HTTP-Request-AsCGI")
    (synopsis "Set up a CGI environment from an HTTP::Request")
    (description "This module provides a convenient way to set up a CGI
environment from an HTTP::Request.")
    (license license:perl-license)))

(define-public perl-http-server-simple
  (package
    (name "perl-http-server-simple")
    (version "0.52")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BP/BPS/"
                           "HTTP-Server-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "0k6bg7k6mjixfzxdkkdrhqvaqmdhjszx0zsk8g0bimiby6j9z4yq"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-cgi))
    (arguments
     ;; See the discussion of a related tests issue at
     ;; https://lists.gnu.org/archive/html/guix-devel/2015-01/msg00346.html
     `(#:tests? #f

       #:phases (modify-phases %standard-phases
                   (add-before 'configure 'set-search-path
                     (lambda _
                       ;; Work around "dotless @INC" build failure.
                       (setenv "PERL5LIB"
                               (string-append (getcwd) ":"
                                              (getenv "PERL5LIB")))
                       #t)))))
    (home-page "https://metacpan.org/release/HTTP-Server-Simple")
    (synopsis "Lightweight HTTP server")
    (description "HTTP::Server::Simple is a simple standalone HTTP daemon with
no non-core module dependencies.  It can be used for building a standalone
http-based UI to your existing tools.")
    (license license:perl-license)))

(define-public perl-http-tiny
  (package
    (name "perl-http-tiny")
    (version "0.076")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "HTTP-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "11wkxxqj3ff84rgj9q2gzkdgscwp3fzj205846k9ycqinlpsmgfx"))))
    (build-system perl-build-system)
    (inputs
     (list perl-http-cookiejar perl-io-socket-ip perl-io-socket-ssl
           perl-mozilla-ca perl-net-ssleay))
    (home-page "https://metacpan.org/release/HTTP-Tiny")
    (synopsis "HTTP/1.1 client")
    (description "This is a very simple HTTP/1.1 client, designed for doing
simple requests without the overhead of a large framework like LWP::UserAgent.
It supports proxies and redirection.  It also correctly resumes after EINTR.")
    (license license:perl-license)))

(define-public perl-http-tinyish
  (package
    (name "perl-http-tinyish")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MI/MIYAGAWA/HTTP-Tinyish-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "199sa722amvwhq0czjfb7psj3hbqmvni5vxkrm579r5943pg0rax"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-file-which perl-ipc-run3))
    (home-page "https://metacpan.org/release/HTTP-Tinyish")
    (synopsis "@code{HTTP::Tiny} compatible HTTP client wrappers")
    (description
     "@code{HTTP::Tinyish} is a wrapper module for @acronym{LWP,libwww-perl},
@code{HTTP::Tiny}, curl and wget.

It provides an API compatible to HTTP::Tiny.")
    (license license:perl-license)))

(define-public perl-io-html
  (package
    (name "perl-io-html")
    (version "1.00")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/C/CJ/CJM/IO-HTML-"
                   version ".tar.gz"))
             (sha256
              (base32
               "06nj3a0xgp5jxwxx6ayglfk2v7npf5a7gwkqsjlkapjkybarzqh4"))))
    (build-system perl-build-system)
    (license license:perl-license)
    (synopsis "Perl module to open an HTML file with automatic charset detection")
    (description
     "IO::HTML provides an easy way to open a file containing HTML while
automatically determining its encoding.  It uses the HTML5 encoding sniffing
algorithm specified in section 8.2.2.1 of the draft standard.")
    (home-page "https://metacpan.org/release/IO-HTML")))

(define-public perl-io-socket-ip
  (package
    (name "perl-io-socket-ip")
    (version "0.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PEVANS/"
                           "IO-Socket-IP-" version ".tar.gz"))
       (sha256
        (base32 "0ihlpxrkq1xrvhnq52nhghanskic718ch8kpp642afgq72i4b6l4"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build))
    (home-page "https://metacpan.org/release/IO-Socket-IP")
    (synopsis "Family-neutral IP socket supporting both IPv4 and IPv6")
    (description "This module provides a protocol-independent way to use IPv4
and IPv6 sockets, intended as a replacement for IO::Socket::INET.")
    (license license:perl-license)))

(define-public perl-io-socket-ssl
  (package
    (name "perl-io-socket-ssl")
    (version "2.081")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SU/SULLR/"
                                  "IO-Socket-SSL-" version ".tar.gz"))
              (sha256
               (base32
                "0hw4c62abq0cs3ixi0ws96i2y0fij3452514dlqn7d6nm0kgig87"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-net-ssleay
           ;; for IDN support
           perl-uri))
    (synopsis "Nearly transparent SSL encapsulation for IO::Socket::INET")
    (description
     "IO::Socket::SSL makes using SSL/TLS much easier by wrapping the
necessary functionality into the familiar IO::Socket interface and providing
secure defaults whenever possible.  This way existing applications can be made
SSL-aware without much effort, at least if you do blocking I/O and don't use
select or poll.")
    (license license:perl-license)
    (home-page "https://github.com/noxxi/p5-io-socket-ssl")))

(define-public perl-libwww
  (package
    (name "perl-libwww")
    (version "6.67")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/libwww-perl-"
                   version ".tar.gz"))
             (sha256
              (base32
               "08xp4q90nkvpwnks2qfqjhqgff6447myayqi6kc1panh7w5c9vln"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-fatal perl-test-needs perl-test-requiresinternet))
    (propagated-inputs
     (list perl-encode-locale
           perl-file-listing
           perl-html-parser
           perl-http-cookies
           perl-http-daemon
           perl-http-date
           perl-http-message
           perl-http-negotiate
           perl-net-http
           perl-try-tiny
           perl-uri
           perl-www-robotrules))
    (license license:perl-license)
    (synopsis "Perl modules for the WWW")
    (description
     "The libwww-perl collection is a set of Perl modules which provides a
simple and consistent application programming interface to the
World-Wide Web.  The main focus of the library is to provide classes
and functions that allow you to write WWW clients.  The library also
contains modules that are of more general use and even classes that
help you implement simple HTTP servers.")
    (home-page "https://metacpan.org/release/libwww-perl")))

(define-public perl-lwp-online
  (package
    (name "perl-lwp-online")
    (version "1.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AD/ADAMK/LWP-Online-"
             version ".tar.gz"))
       (sha256
        (base32
         "176f6vbk1018i0y7xj9d406ndbjgwzan2j9nihxnsahzg2vr2vz2"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-libwww perl-uri))
    (native-inputs
     (list perl-module-install))
    (home-page "https://metacpan.org/release/LWP-Online")
    (synopsis "Checks whether your process has access to the web")
    (description "This module attempts to answer, as accurately as it can, one
of the nastiest technical questions there is: am I on the internet?

A host of networking and security issues make this problem very difficult.
There are firewalls, proxies (both well behaved and badly behaved).  We might
not have DNS.  We might not have a network card at all!")
    (license license:perl-license)))

(define-public perl-lwp-mediatypes
  (package
    (name "perl-lwp-mediatypes")
    (version "6.04")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/LWP-MediaTypes-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1n8rg6csv3dsvymg06cmxipimr6cb1g9r903ghm1qsmiv89cl6wg"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-fatal))
    (license license:perl-license)
    (synopsis "Perl module to guess the media type for a file or a URL")
    (description
     "The LWP::MediaTypes module provides functions for handling media (also
known as MIME) types and encodings.  The mapping from file extensions to
media types is defined by the media.types file.  If the ~/.media.types file
exists it is used instead.")
    (home-page "https://metacpan.org/release/LWP-MediaTypes")))

(define-public perl-lwp-protocol-https
  (package
    (name "perl-lwp-protocol-https")
    (version "6.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OA/OALDERS/"
                           "LWP-Protocol-https-" version ".tar.gz"))
       (sha256
        (base32 "14pm785cgyrnppks6ccasb2vkqifh0a8fz36nmnhc2v926jy3kqn"))))
    (build-system perl-build-system)
    (native-inputs
     ;; For tests.
     (list perl-test-requiresinternet))
    (propagated-inputs
     (list perl-io-socket-ssl perl-libwww perl-mozilla-ca perl-net-http))
    (home-page "https://metacpan.org/release/LWP-Protocol-https")
    (synopsis "HTTPS support for LWP::UserAgent")
    (description "The LWP::Protocol::https module provides support for using
https schemed URLs with LWP.")
    (license license:perl-license)))

(define-public perl-lwp-useragent-cached
  (package
    (name "perl-lwp-useragent-cached")
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OL/OLEG/"
                           "LWP-UserAgent-Cached-" version ".tar.gz"))
       (sha256
        (base32
         "1hw7wy7f82kl61xjwkgmhv1ixgg56dhgfr45wxn6ahc0qys5mkix"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-libwww))
    (home-page "https://metacpan.org/release/LWP-UserAgent-Cached")
    (synopsis "Simple caching for LWP::UserAgent")
    (description "LWP::UserAgent::Cached is an LWP::UserAgent subclass with
cache support.  It returns responses from the local file system, if available,
instead of making an HTTP request.")
    (license license:perl-license)))

(define-public perl-lwp-useragent-determined
  (package
    (name "perl-lwp-useragent-determined")
    (version "1.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AL/ALEXMV/"
                           "LWP-UserAgent-Determined-" version ".tar.gz"))
       (sha256
        (base32
         "0lyvbpjng7yfvyha9rp2y2c6liz5hhplmd2grc8jlsfkih7dbn06"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-libwww))
    (home-page "https://metacpan.org/release/LWP-UserAgent-Determined")
    (synopsis "Virtual browser that retries errors")
    (description "LWP::UserAgent::Determined works just like LWP::UserAgent,
except that when you use it to get a web page but run into a
possibly-temporary error (like a DNS lookup timeout), it'll wait a few seconds
and retry a few times.")
    (license license:perl-license)))

(define-public perl-lwpx-paranoidagent
  (package
    (name "perl-lwpx-paranoidagent")
    (version "1.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/S/SA/SAXJAZMAN/lwp/LWPx-ParanoidAgent-"
            version ".tar.gz"))
      (sha256
       (base32
        "0gfhw3jbs25yya2dryv8xvyn9myngcfcmsybj7gkq62fnznil16c"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-libwww
           ;; Users should instead make sure SSL_ca_path is set properly.
           ;; ("perl-mozilla-ca" ,perl-mozilla-ca)
           perl-net-dns))
    (home-page "https://metacpan.org/release/LWPx-ParanoidAgent")
    (synopsis "Security enhanced subclass of LWP::UserAgent")
    (description "@code{LWPx::ParanoidAgent} is a class subclassing
@code{LWP::UserAgent} but paranoid against attackers.  Its purpose is
to vet requests for a remote resource on behalf of a possibly
malicious user.  The class can do the same as @code{LWP::UserAgent},
except that proxy support has been removed.  Support for URI schemes
is limited to http and https.")
    (license license:perl-license)))

(define-public perl-net-amazon-s3
  (package
    (name "perl-net-amazon-s3")
    (version "0.60")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PF/PFIG/"
                           "Net-Amazon-S3-" version ".tar.gz"))
       (sha256
        (base32
         "10dcsq4s2kc9cb1vccx17r187c81drirc3s1hbxh3rb8489kg2b2"))
       (patches (search-patches
                 "perl-net-amazon-s3-moose-warning.patch"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-libwww perl-test-exception))
    (propagated-inputs
     (list perl-data-stream-bulk
           perl-datetime-format-http
           perl-digest-hmac
           perl-digest-md5-file
           perl-file-find-rule
           perl-http-date
           perl-http-message
           perl-lwp-useragent-determined
           perl-mime-types
           perl-moose
           perl-moosex-strictconstructor
           perl-moosex-types-datetime-morecoercions
           perl-path-class
           perl-regexp-common
           perl-term-encoding
           perl-term-progressbar-simple
           perl-uri
           perl-xml-libxml))
    (home-page "https://metacpan.org/release/Net-Amazon-S3")
    (synopsis "Perl interface to Amazon S3")
    (description "This module provides a Perlish interface to Amazon S3.")
    (license license:perl-license)))

(define-public perl-net-http
  (package
    (name "perl-net-http")
    (version "6.22")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/O/OA/OALDERS/"
                   "Net-HTTP-" version ".tar.gz"))
             (sha256
              (base32
               "18m1b1274wmsl3cdfwg27pm7s1fgrrlhwy4gw4zl8da2p2jzkyk2"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-io-socket-ssl perl-uri))
    (license license:perl-license)
    (synopsis "Perl low-level HTTP connection (client)")
    (description
     "The Net::HTTP class is a low-level HTTP client.  An instance of the
Net::HTTP class represents a connection to an HTTP server.  The HTTP protocol
is described in RFC 2616.  The Net::HTTP class supports HTTP/1.0 and
HTTP/1.1.")
    (home-page "https://metacpan.org/release/Net-HTTP")))

(define-public perl-net-server
  (package
    (name "perl-net-server")
    (version "2.009")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RH/RHANDOM/"
                           "Net-Server-" version ".tar.gz"))
       (sha256
        (base32
         "0gw1k9gcw7habbkxvsfa2gz34brlbwcidk6khgsf1qjm0dbccrw2"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Net-Server")
    (synopsis "Extensible Perl server engine")
    (description "Net::Server is an extensible, generic Perl server engine.
It attempts to be a generic server as in Net::Daemon and NetServer::Generic.
It includes with it the ability to run as an inetd
process (Net::Server::INET), a single connection server (Net::Server or
Net::Server::Single), a forking server (Net::Server::Fork), a preforking
server which maintains a constant number of preforked
children (Net::Server::PreForkSimple), or as a managed preforking server which
maintains the number of children based on server load (Net::Server::PreFork).
In all but the inetd type, the server provides the ability to connect to one
or to multiple server ports.")
    (license license:perl-license)))

(define-public perl-net-smtp-ssl
  (package
    (name "perl-net-smtp-ssl")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Net-SMTP-SSL-" version ".tar.gz"))
       (sha256
        (base32
         "001a6dcfahf7kkyirqkc8jd4fh4fkal7n7vm9c4dblqrvmdc8abv"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-io-socket-ssl))
    (home-page "https://metacpan.org/release/Net-SMTP-SSL")
    (synopsis "SSL support for Net::SMTP")
    (description "SSL support for Net::SMTP.")
    (license license:perl-license)))

(define-public perl-plack
  (package
    (name "perl-plack")
    (version "1.0033")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Plack-" version ".tar.gz"))
       (sha256
        (base32
         "081jg0xddzpg2anmqi9i6d7vs6c8z7k557bf8xl6vgb3h95pin5w"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-requires perl-file-sharedir-install))
    (propagated-inputs
     (list perl-apache-logformat-compiler
           perl-devel-stacktrace
           perl-devel-stacktrace-ashtml
           perl-file-sharedir
           perl-hash-multivalue
           perl-http-body
           perl-http-message
           perl-http-tiny
           perl-libwww
           perl-stream-buffered
           perl-test-tcp
           perl-try-tiny
           perl-uri))
    (home-page "https://metacpan.org/release/Plack")
    (synopsis "Perl Superglue for Web frameworks and servers (PSGI toolkit)")
    (description "Plack is a set of tools for using the PSGI stack.  It
contains middleware components, a reference server, and utilities for Web
application frameworks.  Plack is like Ruby's Rack or Python's Paste for
WSGI.")
    (license license:perl-license)))

(define-public perl-plack-middleware-deflater
  (package
    (name "perl-plack-middleware-deflater")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/K/KA/KAZEBURO/"
             "Plack-Middleware-Deflater-" version ".tar.gz"))
       (sha256
        (base32
         "0xf2visi16hgwgyp9q0cjr10ikbn474hjia5mj8mb2scvbkrbni8"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install perl-test-requires))
    (propagated-inputs
     (list perl-plack))
    (home-page "https://metacpan.org/release/Plack-Middleware-Deflater")
    (synopsis "Compress response body with Gzip or Deflate")
    (description
     "Plack::Middleware::Deflater is a middleware to encode your response body
in gzip or deflate, based on \"Accept-Encoding\" HTTP request header.  It
would save the bandwidth a little bit but should increase the Plack server
load, so ideally you should handle this on the frontend reverse proxy
servers.")
    (license license:perl-license)))

(define-public perl-plack-middleware-fixmissingbodyinredirect
  (package
    (name "perl-plack-middleware-fixmissingbodyinredirect")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SW/SWEETKID/"
                           "Plack-Middleware-FixMissingBodyInRedirect-"
                           version ".tar.gz"))
       (sha256
        (base32
         "14dkrmccq7a5vpymx5dv8032gfcvhsw2i6v5sh3c4ym5ymlx08kc"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-html-parser ;for HTML::Entities
           perl-http-message perl-plack))     ;for Plack::Test
    (home-page
     "https://metacpan.org/release/Plack-Middleware-FixMissingBodyInRedirect")
    (synopsis "Plack::Middleware which sets body for redirect response")
    (description "This module sets the body in redirect response, if it's not
already set.")
    (license license:perl-license)))

(define-public perl-plack-middleware-methodoverride
  (package
    (name "perl-plack-middleware-methodoverride")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Plack-Middleware-MethodOverride-"
                           version ".tar.gz"))
       (sha256
        (base32 "1wdmmav3rbhv49zpw311zrxxqmg1fz3f3q9src0ypgs8zcp5myyv"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-plack))
    (home-page "https://metacpan.org/release/Plack-Middleware-MethodOverride")
    (synopsis "Override REST methods to Plack apps via POST")
    (description "This middleware allows for POST requests that pretend to be
something else: by adding either a header named X-HTTP-Method-Override to the
request, or a query parameter named x-tunneled-method to the URI, the client
can say what method it actually meant.")
    (license license:perl-license)))

(define-public perl-plack-middleware-removeredundantbody
  (package
    (name "perl-plack-middleware-removeredundantbody")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SW/SWEETKID/"
                           "Plack-Middleware-RemoveRedundantBody-"
                           version ".tar.gz"))
       (sha256
        (base32 "0zh83001rn5aqwpc1pn3di2h3ibzlf2dvkmkv05hnadpss9mzm40"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-plack))
    (home-page
     "https://metacpan.org/release/Plack-Middleware-RemoveRedundantBody")
    (synopsis "Plack::Middleware which removes body for HTTP response")
    (description "This module removes the body in an HTTP response if it's not
required.")
    (license license:perl-license)))

(define-public perl-plack-middleware-reverseproxy
  (package
    (name "perl-plack-middleware-reverseproxy")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Plack-Middleware-ReverseProxy-"
                           version ".tar.gz"))
       (sha256
        (base32 "0a512n62pnk5ayj3zdzyj50iy1qi8nwh6ygks2h7nrh7gp9k2jc7"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install))
    (propagated-inputs
     (list perl-plack))
    (home-page "https://metacpan.org/release/Plack-Middleware-ReverseProxy")
    (synopsis "Supports app to run as a reverse proxy backend")
    (description "Plack::Middleware::ReverseProxy resets some HTTP headers,
which are changed by reverse-proxy.  You can specify the reverse proxy address
and stop fake requests using @code{enable_if} directive in your app.psgi.")
    (license license:perl-license)))

(define-public perl-plack-test-externalserver
  (package
    (name "perl-plack-test-externalserver")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Plack-Test-ExternalServer-" version ".tar.gz"))
       (sha256
        (base32 "1l1yj1l25679x7cbpd27ii7s1f1ajpkspif9xqnl21hczrbmrbsv"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-plack))
    (home-page "https://metacpan.org/release/Plack-Test-ExternalServer")
    (synopsis "Run HTTP tests on external live servers")
    (description "This module allows your to run your Plack::Test tests
against an external server instead of just against a local application through
either mocked HTTP or a locally spawned server.")
    (license license:perl-license)))

(define-public perl-test-tcp
  (package
    (name "perl-test-tcp")
    (version "2.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Test-TCP-" version ".tar.gz"))
       (sha256
        (base32 "0mvv9rqwrwlcfh8qrs0s47p85rhlnw15d4gbpyi802bddp0c6lry"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-test-sharedfork))
    (arguments `(#:tests? #f))          ;related to signaling in t/05_sigint.t
    (home-page "https://metacpan.org/release/Test-TCP")
    (synopsis "Testing TCP programs")
    (description "Test::TCP is test utilities for TCP/IP programs.")
    (license license:perl-license)))

(define-public perl-test-www-mechanize
  (package
    (name "perl-test-www-mechanize")
    (version "1.52")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/"
                           "Test-WWW-Mechanize-" version ".tar.gz"))
       (sha256
        (base32 "1jsywlbxhqw39ij7s8vmgff5vys58vlfaq27072awacnxc65aal4"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-carp-assert-more
           perl-html-form
           perl-html-lint
           perl-http-server-simple
           perl-libwww
           perl-test-longstring
           perl-www-mechanize))
    (home-page "https://metacpan.org/release/Test-WWW-Mechanize")
    (synopsis "Testing-specific WWW::Mechanize subclass")
    (description "Test::WWW::Mechanize is a subclass of the Perl module
WWW::Mechanize that incorporates features for web application testing.")
    (license license:artistic2.0)))

(define-public perl-test-www-mechanize-catalyst
  (package
    (name "perl-test-www-mechanize-catalyst")
    (version "0.62")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MS/MSTROUT/"
                           "Test-WWW-Mechanize-Catalyst-" version ".tar.gz"))
       (sha256
        (base32 "1cdc2q16vs6fb335pzaislz2rx1ph9acaxyp7v5hv9xbwwddwfqq"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-catalyst-plugin-session
           perl-catalyst-plugin-session-state-cookie
           perl-module-install
           perl-test-exception
           perl-test-pod
           perl-test-utf8))
    (propagated-inputs
     (list perl-catalyst-runtime
           perl-class-load
           perl-libwww
           perl-moose
           perl-namespace-clean
           perl-test-www-mechanize
           perl-www-mechanize))
    (home-page "https://metacpan.org/release/Test-WWW-Mechanize-Catalyst")
    (synopsis "Test::WWW::Mechanize for Catalyst")
    (description "The Test::WWW::Mechanize::Catalyst module meshes the
Test::WWW:Mechanize module and the Catalyst web application framework to allow
testing of Catalyst applications without needing to start up a web server.")
    (license license:perl-license)))

(define-public perl-test-www-mechanize-psgi
  (package
    (name "perl-test-www-mechanize-psgi")
    (version "0.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OA/OALDERS/"
                           "Test-WWW-Mechanize-PSGI-" version ".tar.gz"))
       (sha256
        (base32
         "0fsh2i05kf1kfavv2r9kmnjl7qlyqrd11ikc0qcqzzxsqzzjkg9r"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-pod))
    (propagated-inputs
     (list perl-plack perl-test-www-mechanize))
    (home-page "https://metacpan.org/release/Test-WWW-Mechanize-PSGI")
    (synopsis "Test PSGI programs using WWW::Mechanize")
    (description "PSGI is a specification to decouple web server environments
from web application framework code.  Test::WWW::Mechanize is a subclass of
WWW::Mechanize that incorporates features for web application testing.  The
Test::WWW::Mechanize::PSGI module meshes the two to allow easy testing of PSGI
applications.")
    (license license:perl-license)))

(define-public perl-uri
  (package
    (name "perl-uri")
    (version "5.05")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://cpan/authors/id/O/OA/OALDERS/"
                                 "URI-" version ".tar.gz"))
             (sha256
              (base32
               "1v3r3ck67w272kzfgm1nd3wb41av1hlnza56vkxxj1i7s3917hd5"))))
    (build-system perl-build-system)
    (native-inputs
     ;; For tests.
     (list perl-test-needs))
    (license license:perl-license)
    (synopsis "Perl Uniform Resource Identifiers (absolute and relative)")
    (description
     "The URI module implements the URI class.  Objects of this class
represent \"Uniform Resource Identifier references\" as specified in RFC 2396
and updated by RFC 2732.")
    (home-page "https://metacpan.org/release/URI")))

(define-public perl-uri-fetch
  (package
    (name "perl-uri-fetch")
    (version "0.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                                  "URI-Fetch-" version ".tar.gz"))
              (sha256
               (base32
                "0355rnw3xbgfwy9fgs6zrjmrsychzmwpkc9jcd9mrbkd9kr3k7rp"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f)) ; Tests require internet connection to succeed
    (inputs
     (list perl-class-errorhandler perl-libwww perl-uri))
    (home-page "https://metacpan.org/release/URI-Fetch")
    (synopsis "Smart URI fetching/caching")
    (description "@code{URI::Fetch} is a smart client for fetching HTTP pages,
notably syndication feeds (RSS, Atom, and others), in an intelligent, bandwidth-
and time-saving way.")
    (license license:perl-license)))

(define-public perl-uri-find
  (package
    (name "perl-uri-find")
    (version "20160806")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MS/MSCHWERN/"
                           "URI-Find-" version ".tar.gz"))
       (sha256
        (base32
         "1mk3jv8x0mcq3ajrn9garnxd0jc7sw4pkwqi88r5apqvlljs84z2"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-uri))
    (home-page "https://metacpan.org/release/URI-Find")
    (synopsis "Find URIs in arbitrary text")
    (description "This module finds URIs and URLs (according to what URI.pm
considers a URI) in plain text.  It only finds URIs which include a
scheme (http:// or the like), for something a bit less strict, consider
URI::Find::Schemeless.  For a command-line interface, urifind is provided.")
    (license license:perl-license)))

(define-public perl-uri-ws
  (package
    (name "perl-uri-ws")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PL/PLICEASE/"
                           "URI-ws-" version ".tar.gz"))
       (sha256
        (base32
         "1vs1wm80sq685944g1l4a0fxcbccc00c0f9648yabdmcf90hwsvf"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-uri))
    (home-page "https://metacpan.org/release/URI-ws")
    (synopsis "WebSocket support for URI package")
    (description "With this module, the URI package provides the same set of
methods for WebSocket URIs as it does for HTTP URIs.")
    (license license:perl-license)))

(define-public perl-web-scraper
  (package
    (name "perl-web-scraper")
    (version "0.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/Web-Scraper-"
             version ".tar.gz"))
       (sha256
        (base32 "1gs3fmbc83j34c0sig1hkpnm26ngnyi5kgq5dl8vxvkzimgnwnzr"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build-tiny
                         perl-test-base
                         perl-test-requires))
    (propagated-inputs (list perl-html-parser
                             perl-html-selector-xpath
                             perl-html-tagset
                             perl-html-tree
                             perl-html-treebuilder-xpath
                             perl-libwww
                             perl-universal-require
                             perl-uri
                             perl-xml-xpathengine
                             perl-yaml))
    (home-page "https://metacpan.org/release/Web-Scraper")
    (synopsis
     "Web Scraping toolkit using HTML and CSS Selectors or XPath expressions")
    (description "Perl module @code{Web::Scraper} is a toolkit for
traversing and scraping sites, inspired by Ruby's Scapi.")
    (license license:perl-license)))

(define-public perl-uri-template
  (package
    (name "perl-uri-template")
    (version "0.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BR/BRICAS/URI-Template-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1phibcmam2hklrddzj79l43va1gcqpyszbw21ynxq53ynmhjvbk8"))))
    (build-system perl-build-system)
    (inputs
     (list perl-uri))
    (native-inputs
     (list perl-test-pod-coverage perl-test-pod perl-module-install
           perl-json))
    (home-page "https://metacpan.org/release/URI-Template")
    (synopsis "Object for handling URI templates")
    (description "This perl module provides a wrapper around URI templates as described in
RFC 6570.")
    (license license:perl-license)))

(define-public perl-www-curl
  (package
    (name "perl-www-curl")
    (version "4.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SZ/SZBALINT/WWW-Curl-"
                    version".tar.gz"))
              (patches (search-patches "perl-www-curl-fix-struct-void.patch"
                                       "perl-www-curl-remove-symbol.patch"))
              (sha256
               (base32
                "1fmp9aib1kaps9vhs4dwxn7b15kgnlz9f714bxvqsd1j1q8spzsj"))))
    (build-system perl-build-system)
    (arguments
     '(#:tests? #f                          ;XXX: tests require network access
       #:phases (modify-phases %standard-phases
                   (add-before 'configure 'set-search-path
                     (lambda _
                       ;; Work around "dotless @INC" build failure.
                       (setenv "PERL5LIB"
                               (string-append (getcwd) ":"
                                              (getenv "PERL5LIB")))
                       #t)))))
    (native-inputs
     (list perl-module-install))
    (inputs (list curl))
    (synopsis "Perl extension interface for libcurl")
    (description
     "This is a Perl extension interface for the libcurl file downloading
library.")
    (license license:perl-license)
    (home-page "https://metacpan.org/release/WWW-Curl")))

(define-public perl-www-mechanize
  (package
    (name "perl-www-mechanize")
    (version "1.91")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OA/OALDERS/"
                           "WWW-Mechanize-" version ".tar.gz"))
       (sha256
        (base32 "0cb14m1vhaf0mgn2fqwi5hm72xhfi77hpq2g57swgy0w83x7m27b"))))
    (build-system perl-build-system)
    (native-inputs                      ;only for tests
     (list perl-cgi perl-test-deep perl-test-fatal perl-test-output
           perl-test-warnings))
    (propagated-inputs
     (list perl-html-form
           perl-html-parser
           perl-html-tree
           perl-http-message
           perl-http-server-simple
           perl-libwww
           perl-test-warn
           perl-uri))
    (home-page "https://metacpan.org/release/WWW-Mechanize")
    (synopsis "Web browsing in a Perl object")
    (description "WWW::Mechanize is a Perl module for stateful programmatic
web browsing, used for automating interaction with websites.")
    (license license:perl-license)))

(define-public perl-www-opensearch
  (package
    (name "perl-www-opensearch")
    (version "0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BR/BRICAS/"
                                  "WWW-OpenSearch-" version ".tar.gz"))
              (sha256
               (base32
                "1yxplx1q1qk2fvnzqrbk01lz26fy1lyhay51a3ky7q3jgh9p01rb"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-class-errorhandler
           perl-datetime
           perl-datetime-format-mail
           perl-datetime-format-w3cdtf
           perl-feed-find
           perl-module-install
           perl-module-pluggable
           perl-uri-fetch
           perl-test-simple
           perl-xml-atom
           perl-xml-rss))
    (inputs
     (list perl-data-page
           perl-libwww
           perl-uri
           perl-uri-template
           perl-xml-feed
           perl-xml-libxml))
    (home-page "https://metacpan.org/release/WWW-OpenSearch")
    (synopsis "Search A9 OpenSearch compatible engines")
    (description
     "@code{WWW::OpenSearch} is a module to search @url{A9's OpenSearch,
http://opensearch.a9.com} compatible search engines.")
    (license license:perl-license)))

(define-public perl-www-robotrules
  (package
    (name "perl-www-robotrules")
    (version "6.02")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GA/GAAS/WWW-RobotRules-"
                   version ".tar.gz"))
             (sha256
              (base32
               "07m50dp5n5jxv3m93i55qvnd67a6g7cvbvlik115kmc8lbkh5da6"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-uri))
    (license license:perl-license)
    (synopsis "Perl database of robots.txt-derived permissions")
    (description
     "The WWW::RobotRules module parses /robots.txt files as specified in
\"A Standard for Robot Exclusion\", at
<http://www.robotstxt.org/wc/norobots.html>.  Webmasters can use the
/robots.txt file to forbid conforming robots from accessing parts of
their web site.")
    (home-page "https://metacpan.org/release/WWW-RobotRules")))

(define-public python-lambda-4dn
  (package
    (name "python-lambda-4dn")
    (version "0.12.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-lambda-4dn" version))
              (sha256
               (base32
                "1p5i8wsi8q5fpq63i7n7ri1w1lnh4gpn17f88vhkbh14aah5wxj1"))))
    (properties '(("upstream-name" . "python-lambda-4dn")))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pip-install
            (lambda _
              (substitute* "aws_lambda/aws_lambda.py"
                ;; This package uses pip to install Python packages, wrap them
                ;; up, and push them to AWS Lambda.  We need to reset
                ;; GUIX_PYTHONPATH to avoid introducing package conflicts that
                ;; would cause pip to fail.
                (("(subprocess.call\\(\\[sys.executable.*'--no-cache-dir'\\])\\)" _ m)
                 (string-append m ", env={\"GUIX_PYTHONPATH\":\""
                                #$(this-package-input "python")
                                "/lib/python"
                                #$(version-major+minor
                                   (package-version (this-package-input "python")))
                                "/site-packages/\"})"))
                ;; Zipfile uses the mtime of the temporary directory to build
                ;; a zip file.  But the temp directory has a timestamp of 0,
                ;; and zipfile refuses to build a zip archive dated before
                ;; 1980.  So we reset the mtime of all temp files before they
                ;; are added to the zip archive.
                (("^( +)arcname = os.path.join" line indent)
                 (string-append indent
                                "os.utime(os.path.join(root, file), (315619200, 315619200))\n"
                                line))))))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (inputs (list python))
    (propagated-inputs
     (list python-boto3
           python-botocore
           python-docutils
           python-pip
           python-six
           python-virtualenv))
    (home-page "https://github.com/4dn-dcic/python-lambda")
    (synopsis
     "Toolkit for developing and deploying Python code in AWS Lambda")
    (description
     "This is a toolset for developing and deploying serverless Python code in
AWS Lambda.  This is a fork of Nick Ficano's Python-lambda package.  It is
frozen for the needs of projects at the 4D Nucleome Data Coordination and
Integration Center (4DN-DCIC).")
    (license (list license:isc license:expat))))

(define-public python-feedparser
  (package
    (name "python-feedparser")
    (version "6.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "feedparser" version ".tar.gz"))
       (sha256
        (base32 "0lfa1c8s6abnlksbwxdpq78bg4rb6603mcgarmip3kip8rglini7"))
       (patches (search-patches "python-feedparser-missing-import.patch"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-sgmllib3k))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "tests/runtests.py")))))))
    (home-page "https://github.com/kurtmckee/feedparser")
    (synopsis "Parse feeds in Python")
    (description
     "Universal feed parser which handles RSS 0.9x, RSS 1.0, RSS 2.0,
CDF, Atom 0.3, and Atom 1.0 feeds.")
    (license (list license:bsd-2           ; source code
                   license:freebsd-doc)))) ; documentation

(define-public python-tibanna
  (package
    (name "python-tibanna")
    (version "5.4.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tibanna" version))
              (sha256
               (base32
                "11pbyw881qaj08syc9mwr301rm3jhy6vyci98pxin2dwvyzkgwhd"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Tests require AWS credentials and access to the internet.
     (list #:tests? #false))
    (propagated-inputs
     (list python-benchmark-4dn
           python-boto3 python-botocore
           python-lambda-4dn python-tomlkit))
    (native-inputs
     (list python-poetry-core
           python-pytest
           python-pytest-cov
           python-pytest-mock))
    (home-page "https://github.com/4dn-dcic/tibanna")
    (synopsis "Tibanna runs portable workflows on the AWS Cloud")
    (description
     "Tibanna runs portable pipelines (in CWL/WDL and Snakemake) on the AWS
Cloud.")
    (license license:expat)))

(define-public guix-data-service
  (let ((commit "d60a8a44cbc0efdf1f9194295e22769818ac2a3a")
        (revision "66"))
    (package
      (name "guix-data-service")
      (version (string-append "0.0.1-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://codeberg.org/guix/data-service.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1c9iyllkkrww9znqa7zfpm6104ga5mcv1qq0j2ykdg53qxykgxnn"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules '((guix build utils)
                    (guix build gnu-build-system)
                    (ice-9 ftw)
                    (ice-9 match)
                    (ice-9 rdelim)
                    (ice-9 popen))
        #:test-target "check-with-tmp-database"
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-GUILE_AUTO_COMPILE
              (lambda _
                ;; To avoid warnings relating to 'guild'.
                (setenv "GUILE_AUTO_COMPILE" "0")))
            (add-after 'install 'wrap-executable
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin"))
                       (guile (assoc-ref inputs "guile"))
                       (guile-effective-version
                        (read-line
                         (open-pipe* OPEN_READ
                                     (string-append guile "/bin/guile")
                                     "-c" "(display (effective-version))")))
                       (scm (string-append out "/share/guile/site/"
                                           guile-effective-version))
                       (go  (string-append out "/lib/guile/"
                                           guile-effective-version
                                           "/site-ccache")))
                  (for-each
                   (lambda (file)
                     (simple-format (current-error-port)
                                    "wrapping: ~A\n"
                                    (string-append bin "/" file))
                     (wrap-program (string-append bin "/" file)
                       `("PATH" ":" prefix
                         ,(cons*
                           bin
                           (map (lambda (input)
                                  (string-append
                                   (assoc-ref inputs input)
                                   "/bin"))
                                '("ephemeralpg"
                                  "util-linux"
                                  "postgresql"))))
                       `("GUILE_LOAD_PATH" ":" prefix
                         (,scm ,(getenv "GUILE_LOAD_PATH")))
                       `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                         (,go ,(getenv "GUILE_LOAD_COMPILED_PATH")))))
                   (scandir bin
                            (match-lambda
                              ((or "." "..") #f)
                              (_ #t)))))))
            (delete 'strip))))          ; As the .go files aren't compatible
      (inputs
       (list bash-minimal
             ephemeralpg
             util-linux
             postgresql-13
             sqitch
             bash-minimal))
      (propagated-inputs
       (list guix
             guile-fibers-next
             guile-knots
             guile-json-4
             guile-email
             guile-prometheus
             guile-squee
             guile-lzlib))
      (native-inputs
       (list (car (assoc-ref (package-native-inputs guix) "guile"))
             autoconf
             automake
             emacs-minimal
             emacs-htmlize
             pkg-config))
      (synopsis "Store and provide data about GNU Guix")
      (description
       "The Guix Data Service stores data about GNU Guix, and provides this
through a web interface.  It supports listening to the guix-commits mailing
list to find out about new revisions, then loads the data from these in to a
PostgreSQL database.")
      (home-page "https://data.guix.gnu.org/")
      (license license:agpl3+))))

(define-public gumbo-parser
  (package
    (name "gumbo-parser")
    (version "0.10.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/gumbo-parser")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xslckwdh2i0g2qjsb6rnm8mjmbagvziz0hjlf7d1lbljfms1iw1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))         ;tests require bundling googletest sources
    ;; The release tarball lacks the generated files.
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://github.com/google/gumbo-parser")
    (synopsis "HTML5 parsing library")
    (description
     "Gumbo is an implementation of the HTML5 parsing algorithm implemented as
a pure C99 library.")
    (license license:asl2.0)))

(define-public uwsgi
  (package
    (name "uwsgi")
    (version "2.0.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://projects.unbit.it/downloads/uwsgi-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10zmk4npknigmbqcq1wmhd461dk93159px172112vyq0i19sqwj9"))))
    (build-system gnu-build-system)
    (outputs '("out" "python"))
    (arguments
     '(;; XXX: The 'check' target runs cppcheck to do static code analysis.
       ;;      But there is no obvious way to run the real tests.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; Configuration is done by writing an ini file.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (bindir    (string-append out "/bin"))
                    (plugindir (string-append out "/lib/uwsgi")))
               ;; The build phase outputs files to these directories directly.
               (mkdir-p bindir)
               (mkdir-p plugindir)
               ;; XXX: Enable other plugins.
               (call-with-output-file "buildconf/guix.ini"
                 (lambda (port)
                   (format port "[uwsgi]
yaml = libyaml
bin_name = ~a/uwsgi
plugin_dir = ~a

inherit = base
plugins = cgi,python
embedded_plugins =
" bindir plugindir))))
             (setenv "PROFILE" "guix")
             #t))
         (replace 'install
           ;; Move plugins into their own output.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out           (assoc-ref outputs "out"))
                    (plugindir     (string-append out "/lib/uwsgi"))
                    (python-plugin (string-append
                                    plugindir "/python_plugin.so")))
               (install-file python-plugin
                             (string-append
                              (assoc-ref outputs "python") "/lib/uwsgi"))
               (delete-file python-plugin)
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     (list jansson
           libxml2
           libyaml
           openssl
           pcre
           zlib
           ;; For plugins.
           python))
    (home-page "https://uwsgi-docs.readthedocs.org/")
    (synopsis "Application container server")
    (description
     "uWSGI presents a complete stack for networked/clustered web applications,
implementing message/object passing, caching, RPC and process management.
It uses the uwsgi protocol for all the networking/interprocess communications.")
    (license license:gpl2+))) ; with linking exception

(define-public jq
  (package
    (name "jq")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jqlang/jq"
                           "/releases/download/jq-" version
                           "/jq-" version ".tar.gz"))
       (sha256
        (base32 "1hl0wppdwwrqf3gzg3xwc260s7i1br2lnc97zr1k8bpx56hrr327"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled onigurama.
        '(delete-file-recursively "modules"))))
    (inputs
     (list oniguruma))
    (native-inputs
     (append
       ;; TODO: fix gems to generate documentation
       ;(list ruby bundler)
       '()
       (if (member (%current-system)
                   (package-supported-systems valgrind/pinned))
         (list valgrind/pinned)
         '())))
    (build-system gnu-build-system)
    (home-page "https://jqlang.github.io/jq/")
    (synopsis "Command-line JSON processor")
    (description "jq is like sed for JSON data – you can use it to slice and
filter and map and transform structured data with the same ease that sed, awk,
grep and friends let you play with text.  It is written in portable C.  jq can
mangle the data format that you have into the one that you want with very
little effort, and the program to do so is often shorter and simpler than
you'd expect.")
    (license (list license:expat license:cc-by3.0))))

(define-public go-github-com-mikefarah-yq-v4
  (package
    (name "go-github-com-mikefarah-yq-v4")
    (version "4.44.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mikefarah/yq")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s7c8r6y5jv6wda2v3k47hawfdr9j3rwk717l6byvh5qsbbml0vd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/mikefarah/yq/v4"
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests need this.
          (add-after 'unpack 'fix-access-to-doc
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each make-file-writable
                          (find-files "./pkg/yqlib/doc" "\\.md"))))))))
    (propagated-inputs
     (list go-github-com-a8m-envsubst
           go-github-com-alecthomas-participle-v2
           go-github-com-alecthomas-repr
           go-github-com-dimchansky-utfbom
           go-github-com-elliotchance-orderedmap
           go-github-com-fatih-color
           go-github-com-goccy-go-json
           go-github-com-goccy-go-yaml
           go-github-com-jinzhu-copier
           go-github-com-magiconair-properties
           go-github-com-pelletier-go-toml-v2
           go-github-com-pkg-diff
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-yuin-gopher-lua
           go-golang-org-x-net
           go-golang-org-x-text
           go-gopkg-in-op-go-logging-v1
           go-gopkg-in-yaml-v3))
    (home-page "https://mikefarah.gitbook.io/yq/")
    (synopsis
     "Command-line YAML, JSON, XML, CSV, TOML and properties processor")
    (description
     "This package provides @code{yq}, a command-line YAML, JSON and XML
processor.  It uses @code{jq}-like syntax but works with YAML files as well as
JSON, XML, properties, CSV and TSV.")
    (license license:expat)))

(define-public yq
  (package
    (inherit go-github-com-mikefarah-yq-v4)
    (name "yq")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-mikefarah-yq-v4)
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:tests? _ #t) #f)
       ((#:import-path _) "github.com/mikefarah/yq")))
    (propagated-inputs '())
    (inputs (package-propagated-inputs go-github-com-mikefarah-yq-v4))))

(define-public go-github-com-itchyny-gojq
  (package
    (name "go-github-com-itchyny-gojq")
    (version "0.12.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/itchyny/gojq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0raipf3k392bihjk6kddzl3xsnap8wlvhplngmzx2vkp2f11x6fc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/itchyny/gojq"))
    (inputs
     (list go-github-com-google-go-cmp
           go-github-com-itchyny-timefmt-go
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-runewidth
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/itchyny/gojq")
    (synopsis "Pure Go implementation of jq")
    (description
     "@command{gojq} is an Go implementation and library of the jq JSON
processor.")
    (license license:expat)))

(define-public gojq
  (package
    (inherit go-github-com-itchyny-gojq)
    (name "gojq")
    (arguments
     (ensure-keyword-arguments
      (package-arguments go-github-com-itchyny-gojq)
      (list #:import-path "github.com/itchyny/gojq/cmd/gojq"
            #:unpack-path "github.com/itchyny/gojq"
            #:install-source? #f)))))

(define-public go-jqp
  (package
    (name "go-jqp")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/noahgorstein/jqp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11xqh4113gkzp32hd4dg4cvjp40q3hxfh3889wd4bw2snl0alvcb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:embed-files #~(list ".*.xml")
      #:install-source? #f
      #:import-path "github.com/noahgorstein/jqp"))
    (inputs
     (list go-github-com-spf13-viper
           go-github-com-spf13-cobra
           go-github-com-muesli-termenv
           go-github-com-itchyny-gojq
           go-github-com-itchyny-timefmt-go
           go-github-com-charmbracelet-lipgloss
           go-github-com-charmbracelet-bubbletea
           go-github-com-charmbracelet-bubbles
           go-github-com-atotto-clipboard
           go-github-com-alecthomas-chroma-v2))
    (home-page "https://github.com/noahgorstein/jqp")
    (synopsis "TUI playground to experiment with jq")
    (description
     "This package provides an interactive TUI to explor the @code{jq} command
line utility.  The command accepts an optional query argument which will be
executed against the input JSON or newline-delimited JSON (NDJSON).")
    (license license:expat)))

(define-public pup
  (let ((revision "1")
        (commit "681d7bb639334bf485476f5872c5bdab10931f9a"))
    (package
      (name "pup")
      (version (git-version "0.4.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ericchiang/pup")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hx1k0qlc1bq6gg5d4yprn4d7kvqzagg6mi5mvb39zdq6c4y17vr"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/ericchiang/pup"))
      (home-page "https://github.com/ericchiang/pup")
      (synopsis "Parse HTML at the command line")
      (description
       "@command{pup} is a command line tool for processing HTML.  It reads
from stdin, prints to stdout, and allows the user to filter parts of the page
using CSS selectors.  Inspired by @command{jq}, @command{pup} aims to be a
fast and flexible way of exploring HTML from the terminal.")
      (license license:expat))))

(define-public uhttpmock
  (package
    (name "uhttpmock")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://tecnocode.co.uk/downloads/uhttpmock/"
                           "uhttpmock-" version ".tar.xz"))
       (sha256
        (base32 "1gw4g3m99j00rjd3flbxigv3qgbkafnkhf77c76hv7yy58dc1vgy"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection
           ;; For check phase.
           glib-networking gsettings-desktop-schemas pkg-config))
    (inputs (list libsoup))
    (arguments
     (list #:glib-or-gtk? #t
           #:configure-flags #~(list "-Dgtk_doc=false")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-home-for-tests
                 (lambda _
                   (setenv "HOME" "/tmp"))))))
    (home-page "https://gitlab.com/groups/uhttpmock")
    (synopsis "Library for mocking web service APIs which use HTTP or HTTPS")
    (description
     "Uhttpmock is a project for mocking web service APIs which use HTTP or
HTTPS.  It provides a library, libuhttpmock, which implements recording and
playback of HTTP request/response traces.")
    (license license:lgpl2.1+)))

(define-public uhttpmock-with-libsoup2
  (package
    (inherit uhttpmock)
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://tecnocode.co.uk/downloads/uhttpmock/"
                           "uhttpmock-" version ".tar.xz"))
       (sha256
        (base32 "0bqizz69hxk8rn4z57asz1d45vizl1rj6i5k3rzxn2x3qcik514h"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-home-for-tests
                 (lambda _
                   (setenv "HOME" "/tmp"))))))
    (inputs (list libsoup-minimal-2))))

(define-public woof
  (package
    (name "woof")
    (version "20220202")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/simon-budig/woof")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rm8xs5dhy42jhjpx30vwnvps2rnmrh8scfr89j6dnihc6mpjkmn"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (out    (assoc-ref %outputs "out"))
                (bin    (string-append out "/bin"))
                (python (assoc-ref %build-inputs "python")))
           (mkdir-p bin)
           (with-directory-excursion bin
             (copy-file (in-vicinity source "woof") "woof")
             (patch-shebang "woof" (list (string-append python "/bin"))))
           #t))))
    (inputs (list python))
    (home-page "http://www.home.unix-ag.org/simon/woof.html")
    (synopsis "Single file web server")
    (description "Woof (Web Offer One File) is a small simple web server that
can easily be invoked on a single file.  Your partner can access the file with
tools they trust (e.g. wget).")
    (license license:gpl2+)))

(define netsurf-buildsystem
  (package
    (name "netsurf-buildsystem")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           "buildsystem-" version ".tar.gz"))
       (sha256
        (base32
         "0yadmrpgvl9r08b56qiy5f77jracc7g9n4krn727fip4d7akjgix"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build))))
    (home-page "https://www.netsurf-browser.org")
    (synopsis "Build system for the Netsurf project")
    (description
     "This package provides the shared build system for Netsurf project
libraries.")
    (license license:expat)))

(define netsurf-buildsystem-arguments
  `(#:make-flags `("COMPONENT_TYPE=lib-shared"
                   "CC=gcc" "BUILD_CC=gcc"
                   ,(string-append "PREFIX=" %output)
                   ,(string-append "NSSHARED="
                                   (assoc-ref %build-inputs
                                              "netsurf-buildsystem")
                                   "/share/netsurf-buildsystem"))
    #:test-target "test"
    #:phases (modify-phases %standard-phases
               (delete 'configure))))

(define-public libparserutils
  (package
    (name "libparserutils")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "0ffi5q1jlcdl66nk3cax0mnzvhrjvvjvlx0rfasjfygi333xazii"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem pkg-config perl))                 ;for test harness
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/projects/libparserutils/")
    (synopsis "Parser building library")
    (description
     "LibParserUtils is a library for building efficient parsers, written in
C.  It is developed as part of the NetSurf project.")
    (license license:expat)))

(define-public hubbub
  (package
    (name "hubbub")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           "libhubbub-" version "-src.tar.gz"))
       (sha256
        (base32
         "19fm5h8arnsgxd4w5vr9s2fcb422acciffar3la0b36lygsydhca"))
       (patches (search-patches "hubbub-sort-entities.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem
           pkg-config
           doxygen
           gperf
           json-c-0.12 ; check whether json-c-0.12 can be removed
           perl))
    (propagated-inputs
     (list libparserutils)) ;for libhubbub.pc
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/projects/hubbub/")
    (synopsis "HTML5 compliant parsing library")
    (description
     "Hubbub is an HTML5 compliant parsing library, written in C, which can
parse both valid and invalid web content.  It is developed as part of the
NetSurf project.")
    (license license:expat)))

(define-public ikiwiki
  (package
    (name "ikiwiki")
    (version "3.20200202.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.ikiwiki.info/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "04ijislp7png18bg1carb71xk3sij9x5xpizfkxp6jbip6wdxsml"))
       (snippet
        '(begin
           ;; The POT file requires write permission during the build
           ;; phase.
           (chmod "po/ikiwiki.pot" #o644)))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-Makefiles
           (lambda _
             (substitute* "Makefile.PL"
                          (("SYSCONFDIR\\?=") "SYSCONFDIR?=$(PREFIX)"))
             (with-directory-excursion "po"
               (substitute* "Makefile"
                            (("PERL5LIB=") "PERL5LIB=${PERL5LIB}:")))))
         (add-before 'build 'set-modification-times
           ;; The wiki '--refresh' steps, which are executed during
           ;; the check phase, require recent timestamps on files in
           ;; the 'doc' and 'underlays' directories.
           (lambda _
             (invoke "find"  "doc" "underlays" "-type" "f" "-exec"
                     "touch" "{}" "+")))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Six tests use IPC::Run.  For these tests the PERL5LIB
             ;; variable is needed in the runtime environment and also
             ;; in the search path list in the setup file.
             (substitute*
              '("t/aggregate-file.t" "t/git-cgi.t" "t/git-untrusted.t"
                "t/passwordauth.t" "t/relativity.t" "t/wrapper-environ.t")
              (("(.*)\"perl\"(.*)$" _ prefix suffix)
               (string-append prefix "qw(env), 'PERL5LIB='.$ENV{PERL5LIB}"
                              ", qw(perl)" suffix))
              (("(.*) PERL5LIB=(.*) perl(.*)$" _ prefix middle suffix)
               (string-append prefix "), 'PERL5LIB='.$ENV{PERL5LIB}.':"
                              middle "', qw(perl" suffix))
              (("(.*)setup(.* )getcwd(.*)$" _ prefix middle suffix)
               (string-append prefix "setup" middle
                              "$ENV{PERL5LIB}.':'.getcwd" suffix))
              (("^ENV(.*): '(.*)$" _ middle suffix)
               (string-append "ENV" middle
                              ": '$ENV{PERL5LIB}:" suffix)))
             ;; XDG_DATA_DIRS is needed by the podcast.t test.
             (setenv "XDG_DATA_DIRS"
                     (string-append (assoc-ref inputs "shared-mime-info")
                                    "/share"))
             ;; This test fails: "Cannot read from a file without refname".
             (rename-file "t/po.t" "t/po.t-")
             ;; CC is needed by IkiWiki/Wrapper.pm.
             (setenv "CC" "gcc")))
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin/"))
                    (path (getenv "PERL5LIB")))
               (for-each (lambda (file)
                           (wrap-program file
                             `("PERL5LIB" ":" prefix (,path))))
                         (find-files bin))))))))
    (native-inputs
     (list which
           gettext-minimal
           subversion
           git
           bazaar
           cvs
           mercurial))
    (inputs
     (list bash-minimal
           python-wrapper
           perl-authen-passphrase
           perl-cgi-simple
           perl-db-file
           perl-file-mimeinfo
           perl-html-tagset
           perl-image-magick
           perl-ipc-run
           perl-lwpx-paranoidagent
           perl-xml-feed
           perl-xml-sax
           perl-xml-twig
           perl-yaml-tiny

           ;; Ikiwiki loads po4a as a library, and thus needs the po4a dependencies
           ;; available.  Duplicate them here.
           ;; XXX: It would be ideal to hard code these in po4a somehow.
           perl-syntax-keyword-try
           perl-xs-parse-keyword
           po4a))
    (propagated-inputs
     (list perl-cgi-formbuilder
           perl-cgi-session
           perl-html-parser
           perl-html-scrubber
           perl-html-template
           perl-json
           perl-mail-sendmail
           perl-text-markdown-discount
           perl-timedate
           perl-uri
           perl-xml-simple
           perl-yaml-libyaml))
    (home-page "https://ikiwiki.info/")
    (synopsis "Wiki compiler, capable of generating HTML")
    (description
     "Ikiwiki is a wiki compiler, capable of generating a static set of web
pages, but also incorporating dynamic features like a web based editor and
commenting.")
    (license license:gpl2+)))

(define-public libwapcaplet
  (package
    (name "libwapcaplet")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           "libwapcaplet-" version "-src.tar.gz"))
       (sha256
        (base32
         "0p0c2q9lsj4vs97aa7vjllfhw33zv3dpysdkjblzhib6dpfs2alv"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem pkg-config check-0.14))          ;for tests
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/projects/libwapcaplet/")
    (synopsis "String internment library")
    (description
     "LibWapcaplet provides a reference counted string internment system
designed to store small strings and allow rapid comparison of them.  It is
developed as part of the Netsurf project.")
    (license license:expat)))

(define-public libcss
  (package
    (name "libcss")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           "libcss-" version "-src.tar.gz"))
       (sha256
        (base32
         "0khmf5bdpkc09fpsgwzi23sihpadvyr02jx0q5h1vm9lxjxibwid"))
       (patches (search-patches "libcss-check-format.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem pkg-config perl))
    (propagated-inputs                  ;needed for libcss.pc
     (list libparserutils libwapcaplet))
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/projects/libcss/")
    (synopsis "CSS parser and selection library")
    (description
     "LibCSS is a CSS (Cascading Style Sheet) parser and selection engine,
written in C.  It is developed as part of the NetSurf project.")
    (license license:expat)))

(define-public libdom
  (package
    (name "libdom")
    (version "0.4.2")                 ;TODO include patch for additional tags?
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           "libdom-" version "-src.tar.gz"))
       (sha256
        (base32
         "0g0gqcglk8f8gbygbcq5ylcx84zsf0vczbm3n3118w2l2splapnh"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem
           pkg-config
           perl ;for test harness
           perl-libxml
           perl-switch
           perl-xml-xpath))
    (inputs
     (list libparserutils libwapcaplet))
    (propagated-inputs
     (list expat ;needed for headers and linking
           hubbub))             ;for libdom.pc
    (arguments
     `(#:tests? #f                 ;TODO: re-enable. tests take a looong time.
       ,@netsurf-buildsystem-arguments))
    (home-page "https://www.netsurf-browser.org/projects/libdom/")
    (synopsis "Implementation of the W3C DOM")
    (description
     "LibDOM is an implementation of the W3C DOM, written in C.  It is
developed as part of the NetSurf project.")
    (license license:expat)))

(define-public libsvgtiny
  (package
    (name "libsvgtiny")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "0750q884ax8wygl64wq03zdjj8h838ch3f8jdfkv4gz809zj4my3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem pkg-config gperf-3.0))
    (inputs
     (list libwapcaplet))
    (propagated-inputs
     (list libdom))             ;for libsvgtiny.pc
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/projects/libsvgtiny/")
    (synopsis "Library for parsing SVG files")
    (description
     "Libsvgtiny takes some SVG as input and returns a list of paths and texts
which can be rendered easily, as defined in
@url{http://www.w3.org/TR/SVGMobile/}.  It is developed as part of the NetSurf
project.")
    (license license:expat)))

(define-public libnsbmp
  (package
    (name "libnsbmp")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "14r2v1ich4lxn3sdwpiwq5adydrd1qlhbd8mbamalaqj59laf1sl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem))
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/projects/libnsbmp/")
    (synopsis "Decoding library for BMP and ICO files")
    (description
     "Libnsbmp is a decoding library for BMP and ICO image file formats,
written in C.  It is developed as part of the NetSurf project.")
    (license license:expat)))

(define-public libnsfb
  (package
    (name "libnsfb")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32 "16m3kv8x8mlic4z73h2s3z8lqmyp0z8i30x95lzr1pslxfinqi5y"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem pkg-config))
    (inputs
     ;; SDL is needed to accept any (keyboard, mouse) input.  Don't propagate it
     ;; to satisfy libnsfb.pc: netsurf is the only user and not worth the pain.
     (list sdl))
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/projects/libnsfb/")
    (synopsis "Framebuffer display abstraction library")
    (description
     "LibNSFB is a framebuffer abstraction library, written in C.  It is
developed as part of the NetSurf project and is intended to be suitable for use
in other projects too.

The overall idea of the library is to provide a generic abstraction to a linear
section of memory which corresponds to a visible array of pixel elements on a
display device.  Different colour depths are supported and the library provides
routines for tasks such as drawing onto the framebuffer and rectangle copy
operations.")
    (license license:expat)))

(define-public libnsgif
  (package
    (name "libnsgif")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "06q69hn0nz3c6hnwmzfcldyrppkvimx3s97ql3sx4m0lyr1ch530"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem))
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/projects/libnsgif/")
    (synopsis "Decoding library for GIF files")
    (description
     "Libnsgif is a decoding library for the GIF image file format, written in
C.  It is developed as part of the NetSurf project.")
    (license license:expat)))

(define-public libnslog
  (package
    (name "libnslog")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           "libnslog-" version "-src.tar.gz"))
       (sha256
        (base32
         "1l2k0kdv9iv18svhv360vszjavhl4g09cp8a8yb719pgsylxr67w"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem
           pkg-config
           check ; For tests
           bison
           flex))
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/")
    (synopsis "Logging library")
    (description
     "Libnslog provides a category-based logging library which supports
complex logging filters, multiple log levels, and provides context through to
client applications.  It is developed as part of the NetSurf project.")
    (license license:expat)))

(define-public libnsutils
  (package
    (name "libnsutils")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "14pakllwf7a205d0dkvyg8jhmqfbi5sh5riw840d13j5dr9b952n"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem))
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/")
    (synopsis "Utility library for NetSurf")
    (description
     "Libnsutils provides a small number of useful utility routines.  It is
developed as part of the NetSurf project.")
    (license license:expat)))

(define-public libnspsl
  (package
    (name "libnspsl")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           "libnspsl-" version "-src.tar.gz"))
       (sha256
        (base32
         "105cjkb622wz11z26il4j1n4ydyrrgv0nglr67aawpam5z1wx11n"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem))
    (arguments netsurf-buildsystem-arguments)
    (home-page "https://www.netsurf-browser.org/")
    (synopsis "Library to generate a static Public Suffix List")
    (description
     "Libnspsl is a library to generate a static code representation of the
Public Suffix List.  It is developed as part of the NetSurf project.")
    (license license:expat)))

(define-public nsgenbind
  (package
    (name "nsgenbind")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           "nsgenbind-" version "-src.tar.gz"))
       (sha256
        (base32
         "0p9q9ffn9hf1qrphz2qxq2xvyysn5kg2dbl8cbnkwb5wdkvf0b13"))))
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem bison flex))
    (arguments
     (substitute-keyword-arguments netsurf-buildsystem-arguments
       ((#:make-flags flags)
        `(delete "COMPONENT_TYPE=lib-shared" ,flags))))
    (home-page "https://www.netsurf-browser.org/")
    (synopsis "Generate JavaScript to DOM bindings")
    (description
     "@code{nsgenbind} is a tool to generate JavaScript to DOM bindings from
w3c webidl files and a binding configuration file.")
    (license license:expat)))

(define-public netsurf
  (package
    (name "netsurf")
    (version "3.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/netsurf/"
                           "releases/source/netsurf-" version "-src.tar.gz"))
       (sha256
        (base32
         "1chw40nx7krpy7m14bajfrcj88h98if8py0k7c2qshpfxxm652n2"))
       (patches (search-patches "netsurf-system-utf8proc.patch"
                                "netsurf-y2038-tests.patch"
                                "netsurf-longer-test-timeout.patch"
                                "netsurf-message-timestamp.patch"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list netsurf-buildsystem
           nsgenbind
           libidn ;only for tests
           check
           perl
           perl-html-parser
           pkg-config
           xxd))
    (inputs
     `(("curl" ,curl)
       ("gtk+" ,gtk+)
       ("openssl" ,openssl)
       ("utf8proc" ,utf8proc)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-turbo)
       ("libcss" ,libcss)
       ("libdom" ,libdom)
       ("libnsbmp" ,libnsbmp)
       ("libnsgif" ,libnsgif)
       ("libnslog" ,libnslog)
       ("libnspsl" ,libnspsl)
       ("libnsutils" ,libnsutils)
       ("libsvgtiny" ,libsvgtiny)
       ("miscfiles" ,miscfiles)))
    (arguments
     `(#:make-flags `("CC=gcc" "BUILD_CC=gcc"
                      "TARGET=gtk3"
                      ,(string-append "PREFIX=" %output)
                      ,(string-append "NSSHARED="
                                      (assoc-ref %build-inputs
                                                 "netsurf-buildsystem")
                                      "/share/netsurf-buildsystem"))
       #:test-target "test"
       #:modules ((ice-9 rdelim)
                  (ice-9 match)
                  (srfi srfi-1)
                  (sxml simple)
                  ,@%glib-or-gtk-build-system-default-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'remove-timestamps
           ;; Avoid embedding timestamp for reproducible builds
           (lambda _
             (substitute* "tools/git-testament.pl"
               (("WT_COMPILEDATE ..$compiledate")
                "WT_COMPILEDATE \\\""))))
         (add-after 'build 'adjust-welcome
           (lambda _
             (substitute* "frontends/gtk/res/welcome.html"
               ;; Close some XHTML tags.
               (("<(img|input)([^>]*)>" _ tag contents)
                (string-append "<" tag contents " />"))
               ;; Increase freedom.
               ((" open source") ", free software"))
             (with-atomic-file-replacement "frontends/gtk/res/welcome.html"
               (lambda (in out)
                 ;; Leave the DOCTYPE header as is.
                 (display (read-line in 'concat) out)
                 (sxml->xml
                  (let rec ((sxml (xml->sxml in
                                             #:default-entity-handler
                                             (lambda (port name)
                                               (string-append "<ENTITY>"
                                                              (symbol->string name)
                                                              "</ENTITY>")))))
                    ;; We'd like to use sxml-match here, but it can't
                    ;; match against generic tag symbols...
                    (match sxml
                      ;; Remove default links so it doesn't seem we're
                      ;; endorsing them.
                      (`(div (@ (class "links")) . ,rest)
                       '())
                      ;; Prefer a more privacy-respecting default search
                      ;; engine.
                      (`(form . ,rest)
                       `(form (@ (action "https://lite.duckduckgo.com/lite/")
                                 (method "post"))
                              (div (@ (class "websearch"))
                                   (input (@ (type "text")
                                             (size "42")
                                             (name "q")
                                             (autocomplete "off")
                                             (value "")))
                                   (input (@ (type "submit")
                                             (value "DuckDuckGo Search"))))))
                      (`(ENTITY ,ent)
                       `(*ENTITY* ,ent))
                      ((x ...)
                       (map rec x))
                      (x x)))
                  out)))
             #t))
         (add-before 'check 'patch-check
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("test/bloom.c" "test/hashtable.c")
               (("/usr/share/dict/words")
                (search-input-file inputs "/share/web2")))
             #t))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (desktop (string-append out "/share/applications/"
                                            "netsurf.desktop")))
               (mkdir-p (dirname desktop))
               (copy-file "frontends/gtk/res/netsurf-gtk.desktop"
                          desktop)
               (substitute* desktop
                 (("netsurf-gtk") (string-append out "/bin/netsurf-gtk3"))
                 (("netsurf.png") (string-append out "/share/netsurf/"
                                                 "netsurf.xpm")))
               (install-file "docs/netsurf-gtk.1"
                             (string-append out "/share/man/man1/"))
               #t))))))
    (home-page "https://www.netsurf-browser.org")
    (synopsis "Web browser")
    (description
     "NetSurf is a lightweight web browser that has its own layout and
rendering engine entirely written from scratch.  It is small and capable of
handling many of the web standards in use today.")
    (license license:gpl2+)))

(define-public surfraw
  (let ((commit "ebb8131c7c623ef90d3345cd9d64203693861013")
        (revision "0"))
    (package
      (name "surfraw")
      (version (git-version "2.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/surfraw/Surfraw/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1y3qybbyv8fnfpaw76xkh1b53pd7dvx1zr9pj71df649g4kbbibs"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'configure 'patch-perl
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((perl (assoc-ref inputs "perl")))
                          (substitute* "surfraw.IN"
                            (("perl -e")
                             (string-append perl "/bin/perl -e"))) #t)))
                    (add-after 'install 'compress-elvi.1sr
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; The manpages of the elvis are symlinks to elvi.1sr.gz
                        ;; but elvi.1sr does not get compressed by our manpage phase.
                        (let* ((out (assoc-ref %outputs "out"))
                               (man (string-append out "/share/man/man1")))
                          (with-directory-excursion man
                            (invoke "gzip" "elvi.1sr"))))))))
      (native-inputs (list autoconf automake))
      (inputs (list perl perl-www-opensearch perl-html-parser perl-libwww))
      (synopsis "Unix command line interface to the www")
      (description
       "Surfraw (Shell Users' Revolutionary Front Rage Against the Web)
provides a unix command line interface to a variety of popular www search engines
and similar services.")
      (home-page "http://surfraw.org/")
      (license license:public-domain))))

(define-public darkhttpd
  (package
    (name "darkhttpd")
    (version "1.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emikulic/darkhttpd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15mmq1v8p50mm9wx5w6g4rlr40b7d044lw7rs1wyzdiw9lcnihvm"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)            ; no configure script
          (replace 'install
            (lambda _
              (install-file "darkhttpd" (string-append #$output "/bin")))))))
    (native-inputs (list which python-minimal))
    (synopsis "Simple static web server")
    (description "darkhttpd is a simple static web server.  It is
standalone and does not need inetd or ucspi-tcp.  It does not need any
config files---you only have to specify the www root.")
    (home-page "https://unix4lyfe.org/darkhttpd/")
    (license license:isc)))

(define-public goaccess
  (package
    (name "goaccess")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://tar.goaccess.io/goaccess-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0dvqxk9rbsp24fp1r5xdz7rcnvvl0q26p07nfmgmzaf4wd4yxw29"))
              (modules '((guix build utils)))
              (snippet '(substitute* '("src/error.h"
                                       "src/parser.c")
                          (("__DATE__") "\"1970-01-01\"")
                          (("__TIME__") "\"00:00:00\"")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      '(list "--enable-geoip=mmdb"
             "--enable-utf8")))
    (inputs
     (list glib ncurses libmaxminddb openssl))
    (native-inputs
     (list pkg-config))
    (home-page "https://goaccess.io")
    (synopsis "Analyze Web server logs in real time")
    (description
     "GoAccess is a real-time web log analyzer and interactive viewer that
runs in a terminal or through your browser.  It provides fast and valuable
HTTP statistics for system administrators that require a visual server report
on the fly.")
    (license license:x11)))

(define-public hitch
  (package
    (name "hitch")
    (version "1.7.3")
    (home-page "https://hitch-tls.org/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "source/hitch-" version ".tar.gz"))
              (sha256
               (base32
                "11wp50zs5irb5bj5xyanm060nlvna6ha328wqf6p2nvpbnaz86qs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'pre-check
                    (lambda _
                      ;; Our grep is compiled without perl regexp support. So,
                      ;; rewrite the grep command to not use it. \t tab
                      ;; characters are supported only in perl regexps. So,
                      ;; put in literal tabs using printf instead.
                      (substitute* "src/tests/test32-proxy-authority.sh"
                        (("grep -Pq") "grep -q")
                        (("extension:\\\\tdefault")
                         "extension:$(printf '\\011')default"))
                      ;; Most tests attempts to access hitch-tls.org which is
                      ;; unavailable in the build container.  Run them against
                      ;; a dummy local web server instead.
                      (for-each (lambda (test)
                                  (substitute* test
                                    (("\\[hitch-tls\\.org\\]:80")
                                     "[localhost]:8000")))
                                (find-files "src/tests" "\\.sh$"))
                      (system "python3 -m http.server &")

                      ;; The build container does not reap zombie processes,
                      ;; causing stop_hitch to hang indefinitely while waiting
                      ;; for the process to terminate because 'kill -0' never
                      ;; succeeds.  Use a different test to see whether the
                      ;; process has shut down.
                      (substitute* "src/tests/hitch_test.sh"
                        (("kill -0 \"\\$HITCH_PID\"")
                         "$(ps -p $HITCH_PID -o state= | grep -qv '^Z$')")))))))
    (native-inputs
     (list pkg-config

           ;; For tests.
           curl
           grep
           lsof
           procps
           python))
    (inputs
     (list libev openssl))
    (synopsis "Scalable TLS proxy")
    (description
     "Hitch is a performant TLS proxy based on @code{libev}.  It terminates
SSL/TLS connections and forwards the unencrypted traffic to a backend such
as a web server.  It is designed to handle many thousand connections on
multicore machines.")
    (license license:bsd-2)))

(define-public httptunnel
  (package
    (name "httptunnel")
    (version "3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.nocrew.org/software/httptunnel/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mn5s6p68n32xzadz6ds5i6bp44dyxzkq68r1yljlv470jr84bql"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Remove non-free IETF RFC documentation.
                   (delete-file-recursively "doc")
                   #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The default configure phase tries to pass environment variables as
         ;; command-line arguments, which confuses the ./configure script.
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
    (home-page "http://www.nocrew.org/software/httptunnel.html")
    (synopsis "Tunnel data connections through HTTP requests")
    (description "httptunnel creates a bidirectional virtual data connection
tunnelled through HTTP (HyperText Transfer Protocol) requests.  This can be
useful for users behind restrictive firewalls.  As long as Web traffic is
allowed, even through a HTTP-only proxy, httptunnel can be combined with other
tools like SSH (Secure Shell) to reach the outside world.")
    (license license:gpl2+)))

(define-public stunnel
  (package
  (name "stunnel")
  (version "5.66")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "https://www.stunnel.org/downloads/stunnel-"
                          version ".tar.gz"))
      (sha256
       (base32 "172pkzp8qilj0gd92bhdi96739gjpgbcav5c7a4gd98s9mq7i0am"))))
  (build-system gnu-build-system)
  (arguments
   (list #:configure-flags
         #~(list (string-append "--with-ssl="
                                #$(this-package-input "openssl")))
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'patch-output-directories
               (lambda _
                 ;; Some (not all) Makefiles have a hard-coded incorrect docdir.
                 (substitute* (list "Makefile.in"
                                    "doc/Makefile.in"
                                    "tools/Makefile.in")
                   (("/doc/stunnel")
                    (string-append "/doc/" #$name "-" #$version)))))
             (add-after 'install 'prune-documentation
               (lambda _
                 (let* ((doc (string-append #$output "/share/doc/"
                                            #$name "-" #$version)))
                   (for-each delete-file (find-files doc "^INSTALL"))))))))
  (native-inputs
   ;; For tests.
   (list iproute
         netcat
         procps
         python))
  (inputs (list openssl perl))
  (home-page "https://www.stunnel.org")
  (synopsis "TLS proxy for clients or servers")
  (description "Stunnel is a proxy designed to add TLS encryption
functionality to existing clients and servers without any changes in the
programs' code.  Its architecture is optimized for security, portability, and
scalability (including load-balancing), making it suitable for large
deployments.")
  (license license:gpl2+)))

(define-public varnish
  (package
    (name "varnish")
    (home-page "https://varnish-cache.org/")
    (version "7.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "_downloads/varnish-" version ".tgz"))
              (sha256
               (base32
                "0p2xf4a8bk2w8j9q20fazrc93fwcfhw8zcvdd8ssbahvlg2q78mb"))))
    (build-system gnu-build-system)
    (arguments
     (append
      (if (target-x86-32?)
          '(#:make-flags
            (list "CFLAGS+=-fexcess-precision=standard"))
          '())
      (list
       #:configure-flags
       #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
               (string-append "CC=" #$(cc-for-target))
               ;; Use absolute path of GCC so it's found at runtime.
               (string-append "PTHREAD_CC="
                              (search-input-file %build-inputs
                                                 "/bin/gcc"))
               "--localstatedir=/var")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'use-absolute-file-names
             (lambda _
               (substitute* '("bin/varnishtest/vtc_varnish.c"
                              "bin/varnishtest/vtc_process.c"
                              "bin/varnishtest/vtc_haproxy.c"
                              "bin/varnishtest/tests/u00014.vtc"
                              "bin/varnishd/mgt/mgt_vcc.c")
                 (("/bin/sh") (which "bash")))
               (let* ((rm (which "rm")))
                 (substitute* "bin/varnishd/mgt/mgt_shmem.c"
                   (("rm -rf") (string-append rm " -rf")))
                 (substitute* "bin/varnishtest/vtc_main.c"
                   (("/bin/rm") rm))
                 (substitute* "bin/varnishd/mgt/mgt_main.c"
                   (("rm -rf") (string-append rm " -rf"))))
               (substitute* "bin/varnishtest/tests/u00000.vtc"
                 (("/bin/echo") (which "echo")))))
           (add-after 'unpack 'remove-failing-tests
             (lambda _
               ;; This test seems to fail because of
               ;; Failed: Servname not supported for ai_socktype
               (delete-file "bin/varnishtest/tests/b00085.vtc")))
           (add-before 'install 'patch-Makefile
             (lambda _
               (substitute* "Makefile"
                 ;; Do not create /var/varnish during install.
                 (("^install-data-am: install-data-local")
                  "install-data-am: "))))
           (add-after 'install 'wrap-varnishd
             ;; Varnish uses GCC to compile VCL, so wrap it with required GCC
             ;; environment variables to avoid propagating them to profiles.
             (lambda* (#:key inputs #:allow-other-keys)
               (wrap-program (string-append #$output "/sbin/varnishd")
                 ;; Add binutils to PATH so gcc finds the 'as' executable.
                 `("PATH" ":" prefix (,(dirname (which "as"))))
                 ;; Make sure 'crti.o' et.al is found.
                 `("LIBRARY_PATH" ":" prefix
                   (,(dirname
                      (search-input-file inputs "lib/libc.so")))))))))))
    (native-inputs
     (list pkg-config
           python-sphinx
           python-docutils))
    (inputs
     (list bash-minimal
           coreutils-minimal
           jemalloc
           ncurses
           pcre2
           python-minimal
           readline))
    (synopsis "Web application accelerator")
    (description
     "Varnish is a high-performance HTTP accelerator.  It acts as a caching
reverse proxy and load balancer.  You install it in front of any server that
speaks HTTP and configure it to cache the contents through an extensive
configuration language.")
    (properties
     '((release-monitoring-url . "https://varnish-cache.org/releases/index.html")))
    (license (list license:bsd-2           ;main distribution
                   license:zlib            ;lib/libvgz/*
                   license:public-domain   ;bin/varnishncsa/as64.c, include/miniobj.h
                   license:bsd-3))))       ;include/vqueue.h, lib/libvarnishcompat/daemon.c

(define-public varnish-modules
  (package
    (name "varnish-modules")
    (home-page "https://github.com/varnish/varnish-modules")
    (version "0.25.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jan3lwynp14awh6jk4zc052lm8m02vqms8ryc7zmjnm5jifdzlv"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           ;; For bootstrapping.
           autoconf
           automake
           libtool
           ;; For generating manuals.
           python-docutils))
    (inputs
     (list python varnish))
    (synopsis "Collection of Varnish modules")
    (description
     "This package provides a collection of modules (@dfn{vmods}) for the Varnish
cache server, extending the @acronym{VCL, Varnish Configuration Language} with
additional capabilities.")
    (license license:bsd-2)))

(define-public xinetd
  (package
    (name "xinetd")
    ;; This is the maintenance fork currently used by openSUSE and Debian.
    (version "2.3.15.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/xinetd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lrp3lcj6azhjplwxws2rx40bkyp6i6bp7n77ndcisb7ninad30q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-loadavg")
       #:tests? #f))                    ; no tests
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://github.com/openSUSE/xinetd")
    (synopsis "Internet services daemon")
    (description "@code{xinetd}, a more secure replacement for @code{inetd},
listens for incoming requests over a network and launches the appropriate
service for that request.  Requests are made using port numbers as identifiers
and xinetd usually launches another daemon to handle the request.  It can be
used to start services with both privileged and non-privileged port numbers.")
    (license (license:fsf-free "file://COPYRIGHT"))))

(define-public tidy-html
  (package
    (name "tidy-html")
    (version "5.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/htacg/tidy-html5")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1vd50q6xqxvidaclinsm89p6r0494wj72j1gpk32vkkhhx15cddz"))))
    (build-system cmake-build-system)
    (outputs '("out"
               "static"))               ; 1.3MiB of .a files
    (arguments
     `(#:tests? #f                      ; no tests available
       #:build-type "Release"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move static libraries to the "static" output.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (static (assoc-ref outputs "static"))
                    (slib   (string-append static "/lib")))
               (mkdir-p slib)
               (for-each (lambda (file)
                           (install-file file slib)
                           (delete-file file))
                         (find-files lib "\\.a$"))
               #t))))))
    (native-inputs
     (list libxslt))
    (home-page "https://www.html-tidy.org/")
    (synopsis "HTML Tidy with HTML5 support")
    (description
     "Tidy is a console application which corrects and cleans up
HTML and XML documents by fixing markup errors and upgrading
legacy code to modern standards.

Tidy also provides @code{libtidy}, a C static and dynamic library that
developers can integrate into their applications to make use of the
functions of Tidy.")
    (license license:bsd-3)))

(define-public tidy
  (deprecated-package "tidy" tidy-html))

(define-public hiawatha
  (package
    (name "hiawatha")
    (version "10.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.hiawatha-webserver.org/files/"
                           "hiawatha-" version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; We use packaged libraries, so delete the bundled copies.
                   (for-each delete-file-recursively
                             (list "extra/nghttp2.tgz" "mbedtls"))
                   #t))
       (sha256
        (base32 "09wpgilbv13zal71v9lbsqr8c3fignygadykpd1p1pb8blb5vn3r"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:configure-flags (list (string-append "-DUSE_SYSTEM_MBEDTLS=on")
                               (string-append "-DENABLE_HTTP2=on")
                               (string-append "-DUSE_SYSTEM_NGHTTP2=on")
                               (string-append "-DENABLE_TOMAHAWK=on")
                               (string-append "-DLOG_DIR=/var/log/hiawatha")
                               (string-append "-DPID_DIR=/run")
                               (string-append "-DWEBROOT_DIR="
                                              (assoc-ref %outputs "out")
                                              "/share/hiawatha/html")
                               (string-append "-DWORK_DIR=/var/lib/hiawatha"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'install-no-empty-directories
           (lambda _
             (substitute* "CMakeLists.txt"
               (("install\\(DIRECTORY DESTINATION" match)
                (string-append "#" match)))
             #t))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'hiawatha' finds 'mbedtls'.
             (let* ((out (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin"))
                    (mbed (assoc-ref inputs "mbedtls")))
               (wrap-program (string-append sbin "/hiawatha")
                 `("PATH" ":" prefix (,mbed)))))))))
    (inputs
     ;; TODO: package "hiawatha-monitor", an optional dependency of "hiawatha".
     (list bash-minimal libxcrypt libxslt libxml2 mbedtls-for-hiawatha
           `(,nghttp2 "lib") zlib))
    (home-page "https://www.hiawatha-webserver.org")
    (synopsis "Webserver with focus on security")
    (description
     "Hiawatha has been written with security in mind.
Features include the ability to stop SQL injections, XSS and CSRF attacks and
exploit attempts.")
    (license license:gpl2)))

(define-public python-httpbin
  (package
    (name "python-httpbin")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "httpbin" version))
       (sha256
        (base32 "1a8pcf6411pqkpl3c5z93wml0nw4xb6j9dnjl976ij31h9llh8b3"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-brotlicffi
           python-decorator
           python-flasgger
           python-flask
           python-greenlet-2
           python-itsdangerous
           python-markupsafe
           python-six
           python-werkzeug))
    ;; The archive in PyPI points to a fork of
    ;; <https://github.com/postmanlabs/httpbin> which is unmaintained for 6y,
    ;; where <https://github.com/Runscope/httpbin> rediects to.  See
    ;; <https://github.com/postmanlabs/httpbin/issues/719>
    (home-page "https://github.com/psf/httpbin")
    (synopsis "HTTP request and response service")
    (description
     "Testing an HTTP Library can become difficult sometimes.
@code{RequestBin} is fantastic for testing POST requests, but doesn't let you
control the response.  This exists to cover all kinds of HTTP scenarios.  All
endpoint responses are JSON-encoded.")
    (license license:isc)))

(define-public python-pytest-httpbin
  (package
    (name "python-pytest-httpbin")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_httpbin" version))
       (sha256
        (base32 "1iikdji2136mybjk7sczqa2qivlb6gchhkzyz4kq68j3hj1pj1fl"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-httpbin
           python-pytest
           python-six))
    (home-page "https://github.com/kevin1024/pytest-httpbin")
    (synopsis "Test your HTTP library against a local copy of httpbin")
    (description
     "@code{Pytest-httpbin} creates a @code{pytest} fixture that is
dependency-injected into your tests.  It automatically starts up a HTTP server
in a separate thread running @code{httpbin} and provides your test with the
URL in the fixture.")
    (license license:expat)))

(define-public http-parser
  (let ((commit "ec8b5ee63f0e51191ea43bb0c6eac7bfbff3141d")
        (revision "1"))
    (package
      (name "http-parser")
      (version (git-version "2.9.4" revision commit))
      (home-page "https://github.com/nodejs/http-parser")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page)
                             (commit commit)))
         (sha256
          (base32 "0f297hrbx0kvy3qwgm9rhmbnjww6iljlcz9grsc9d4km1qj1071i"))
         (file-name (git-file-name name version))
         (patches
          (append
           (search-patches "http-parser-CVE-2020-8287.patch")
           (list
            (origin
              ;; Treat an empty port (e.g. `http://hostname:/`) when parsing
              ;; URLs as if no port were specified.  This patch is applied
              ;; to Fedora's http-parser and to libgit2's bundled version.
              (method url-fetch)
              (uri (string-append
                    "https://src.fedoraproject.org/rpms/http-parser/raw/"
                    "e89b4c4e2874c19079a5a1a2d2ccc61b551aa289/"
                    "f/0001-url-treat-empty-port-as-default.patch"))
              (sha256
               (base32
                "0pbxf2nq9pcn299k2b2ls8ldghaqln9glnp79gi57mamx4iy0f6g"))))))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; This assertion fails when building for i686-linux.
             (substitute* "test.c"
               (("assert\\(sizeof\\(http_parser\\) == 32\\);")
                "assert(1);"))
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:make-flags
         (list (string-append "PREFIX="
                              (assoc-ref %outputs "out"))
               "library"
               ,@(if (%current-target-system)
                     '()
                     '("CC=gcc")))
         #:phases
         (modify-phases %standard-phases
           ,@(if (%current-target-system)
                 '((replace 'configure
                     (lambda* (#:key target #:allow-other-keys)
                       (substitute* (find-files "." "Makefile")
                         (("CC\\?=.*$")
                          (string-append "CC=" target "-gcc\n"))
                         (("AR\\?=.*$")
                          (string-append "AR=" target "-ar\n")))
                       #t)))
                 '((delete 'configure))))))
      (synopsis "HTTP request/response parser for C")
      (description "This is a parser for HTTP messages written in C.  It
parses both requests and responses.  The parser is designed to be used in
high-performance HTTP applications.  It does not make any syscalls nor
allocations, it does not buffer data, it can be interrupted at anytime.
Depending on your architecture, it only requires about 40 bytes of data per
message stream (in a web server that is per connection).")
      (license license:expat))))

(define-public llhttp
  (package
    (name "llhttp")
    (version "9.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nodejs/llhttp")
                    (commit (string-append "release/v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nkv64c5fs8x6n5f9f6g28w5hvg776p55cwa0f82ni548nx279s1"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;FIXME: tests depend on node-mocha
    (home-page "https://github.com/nodejs/llhttp")
    (synopsis "Port of http_parser to llparse")
    (description "@code{llparse} is a port of @code{http_parser} to
@code{llparse} which aims making it more maintainable, verifiable and
efficient where possible.")
    (license license:expat)))

(define-public jo
  (package
    (name "jo")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jpmens/jo/releases/download/"
                           version "/jo-" version ".tar.gz"))
       (sha256
        (base32 "17y73657z5v792ik3plcvk9f5g5h2yawv6afahhkq42159pwv581"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/jpmens/jo")
    (synopsis "Output JSON from a shell")
    (description "@command{jo} is a command-line utility to create JSON objects
or arrays.  It creates a JSON string on stdout from words provided as
command-line arguments or read from stdin.")
    (license (list license:gpl2+
                   license:expat)))) ; json.c, json.h

(define-public python-internetarchive
  (package
    (name "python-internetarchive")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jjjake/internetarchive")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "186nx0dj0lgqrqkg9kzng5h0scbz3m6bk44vj83wzckr8yh3q08z"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             (string-append
              ;; These tests need Internet access.
              "not test_get_item_with_kwargs"
              " and not test_upload"
              " and not test_ia"))))
    (propagated-inputs
     (list python-backports-csv
           python-clint
           python-docopt
           python-importlib-metadata
           python-jsonpatch
           python-requests
           python-six
           python-schema
           python-tqdm))
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-pytest-capturelog
           python-responses
           python-setuptools
           python-wheel))
    (home-page "https://github.com/jjjake/internetarchive")
    (synopsis "Command-line interface to archive.org")
    (description "@code{ia} is a command-line tool for using
@url{archive.org} from the command-line.  It also implements the
internetarchive python module for programmatic access to archive.org.")
    (license license:agpl3+)))

(define-public python-clf
  (let ((commit-test-clf "d01d25923c599d3261910f79fb948825b4270d07")) ; 0.5.7
    (package
      (name "python-clf")
      (version "0.5.7")
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "clf" version))
         (sha256
          (base32
           "0zlkzqnpz7a4iavsq5vaz0nf5nr7qm5znpg1vlpz6rwnx6hikjdb"))))
      (build-system python-build-system)
      (propagated-inputs
       (list python-docopt
             python-pygments
             python-requests
             python-nose
             python-lxml
             python-pyaml))
      (inputs
       `(("test-clf"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://raw.githubusercontent.com"
                                 "/ncrocfer/clf/" commit-test-clf
                                 "/test_clf.py"))
             (sha256
              (base32
               "19lr5zdzsmxgkg7wrjq1yzkiahd03wi4k3dskssyhmjls8c10nqd"))))))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'get-tests
             (lambda _
               (copy-file (assoc-ref %build-inputs "test-clf") "test_clf.py")
               #t))
           (replace 'check
             (lambda _
               (invoke "nosetests"
                       ;; These tests require an Internet connection.
                       "--exclude=test_browse"
                       "--exclude=test_command"
                       "--exclude=test_search"))))))
      (home-page "https://github.com/ncrocfer/clf")
      (synopsis "Search code snippets on @url{https://commandlinefu.com}")
      (description "@code{clf} is a command line tool for searching code
snippets on @url{https://commandlinefu.com}.")
      (license license:expat))))

(define-public nntpit
  (let ((commit "c0d654736460d174a680b2e06c3a81ce883fc09a")
        (revision "0"))
    (package
      (name "nntpit")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/taviso/nntpit")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1kkdh2qpkfw97hzw9jsxy5jzmhhv8261bj63mvr5c9qwlp6qs46g"))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf automake glib pkg-config))
      (inputs (list curl json-c libev))
      (synopsis "Minimal reddit2nntp gateway server")
      (description
       "This is a simple reddit2nntp gateway server that lets you use a newsreader
 to follow discussions on reddit.  The intention is for you to run it locally,
 tell your newsreader to connect to localhost, and subreddits will appear as newsgroups!")
      (home-page "https://github.com/taviso/nntpit")
      (license license:expat))))

(define-public rss-bridge
  (package
    (name "rss-bridge")
    (version "2020-11-10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RSS-Bridge/rss-bridge")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mir6mcm37sbdrhl5kgs6schpp3l4r5mfamhiic0yfbz4hqwmg44"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/rss-bridge"))
       #:phases
       (modify-phases %standard-phases
         ;;Change paths to not use source directory.
         (add-before 'install 'patch-paths
           (lambda _
             (substitute* "lib/rssbridge.php"
               (("PATH_ROOT . 'cache/'")
                "'/var/cache/rss-bridge/'")
               (("PATH_ROOT . 'whitelist.txt'")
                "'/etc/rss-bridge/whitelist.txt'")
               (("PATH_ROOT . 'config.ini.php'")
                "'/etc/rss-bridge/config.ini.php'")))))))
    (home-page "https://github.com/RSS-Bridge/rss-bridge")
    (synopsis "Generate Atom feeds for social networking websites")
    (description "rss-bridge generates Atom feeds for social networking
websites lacking feeds.  Supported websites include Facebook, Twitter,
Instagram and YouTube.")
    (license (list license:public-domain
                   license:expat))))    ; vendor/simplehtmldom/simple_html_dom.php

(define-public linkchecker
  (package
    (name "linkchecker")
    (version "10.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linkchecker/linkchecker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19giahk5bs2r2ay54cc6b2ba5hr3lszn5a89m7zmwb0bk9655z56"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
        ;; OSError: Command ... '-m', 'linkcheck', '-V']' returned non-zero
        ;; exit status 2.
         "--deselect=tests/test_linkchecker.py::TestLinkchecker::test_linkchecker"
         ;; FileNotFoundError: [Errno 2] No such file or directory: 'msgfmt'
         "--deselect=tests/test_po.py::TestPo::test_pos")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-pytest
           python-setuptools-scm))
    (inputs
     (list python-beautifulsoup4
           python-dnspython
           python-requests))
    (home-page "https://linkchecker.github.io/linkchecker/")
    (synopsis "Check websites for broken links")
    (description "LinkChecker is a website validator.  It checks for broken
links in websites.  It is recursive and multithreaded providing output in
colored or normal text, HTML, SQL, CSV, XML or as a sitemap graph.  It
supports checking HTTP/1.1, HTTPS, FTP, mailto, news, nntp, telnet and local
file links.")
    (license (list license:gpl2+
                   license:bsd-2              ; linkcheck/better_exchook2.py
                   license:bsd-3              ; linkcheck/colorama.py
                   license:psfl               ; linkcheck/gzip2.py
                   license:expat))))          ; linkcheck/mem.py

(define-public castor
  (package
    (name "castor")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~julienxx/castor")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gda77ya2qbmjxfbw3yfr64inm8xw8243iwnfsgwwiwl35pw70n9"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-cargo-requirements
            (lambda _
              (substitute* "Cargo.toml" (("~") ""))))
          (add-after 'install 'install-data
            (lambda _
              (invoke "make" (string-append "PREFIX=" #$output)
                      "copy-data"))))
      #:parallel-tests? #f  ; As per the Makefile
      #:install-source? #f
      #:cargo-inputs
      `(("rust-ansi-parser" ,rust-ansi-parser-0.6)
        ("rust-dirs" ,rust-dirs-3)
        ("rust-gdk" ,rust-gdk-0.13)
        ("rust-gtk" ,rust-gtk-0.8)
        ("rust-linkify" ,rust-linkify-0.7)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-open" ,rust-open-2)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-textwrap" ,rust-textwrap-0.14)
        ("rust-url" ,rust-url-2))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list at-spi2-core
           cairo
           gdk-pixbuf
           gtk+
           openssl-3.0
           pango))
    (home-page "https://git.sr.ht/~julienxx/castor")
    (synopsis "Graphical client for plain-text protocols")
    (description
     "Castor is a graphical client for plain-text protocols written in
Rust with GTK.  It currently supports the Gemini, Gopher and Finger
protocols.")
    (license license:expat)))

(define-public clearsilver
  (package
    (name "clearsilver")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blong42/clearsilver/")
             (commit "fbe4926ba9a756163fd1539ff6eee3522cf1f5d8")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02ad43gmqwy7wmh71mh5pk6gl1lax76sjnf42sknj0ijdga170kl"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false ;there is not test target and tests are run during build
      #:configure-flags
      '(list "--disable-java" "--disable-python")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'prepare-streamhtmlparser
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "streamhtmlparser")
                               (string-append (getcwd) "/streamhtmlparser"))
             (for-each make-file-writable
                       (find-files "streamhtmlparser" "."
                                   #:directories? #t))))
         (add-after 'unpack 'pre-bootstrap
           (lambda _
             ;; We don't need the Java stuff
             (substitute* "configure.in"
               (("AC_JNI_INCLUDE_DIR") ""))

             ;; This script will call /bin/sh, so it's easier to just
             ;; bootstrap manually.
             (delete-file "autogen.sh")
             (substitute* "rules.mk.in"
               (("@PTHREAD_LIBS@") "-lpthread")
               (("@PTHREAD_CFLAGS@") "")
               (("@PTHREAD_CC@") "gcc"))

             ;; The GNU variadic macros actually work, whereas the C99
             ;; implementation fails to build.
             (substitute* "util/neo_misc.h"
               (("#define USE_C99_VARARG_MACROS") "#define USE_GNUC_VARARG_MACROS"))

             (setenv "CFLAGS" "-fPIC")

             ;; This directory is created some time during the build, but the
             ;; early libtool processes assume the directory exists.  When
             ;; they are run first they copy the libraries themselves to the
             ;; file "libs" instead of moving them into the directory.
             (mkdir-p "libs")))
         (add-after 'build 'build-documentation
           (lambda _ (invoke "make" "man")))
         (add-after 'install 'install-streamhtmlparser
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (with-directory-excursion "streamhtmlparser"
               (apply invoke "make" "-j" (if parallel-build?
                                             (number->string (parallel-job-count))
                                             "1")
                      "install" make-flags)))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("python" ,python-2)
       ("streamhtmlparser"
        ,(let ((commit "551109ac02a31957a0e776416774c7b515b4b7c7"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/streamhtmlparser/")
                   (commit commit)))
             (file-name (git-file-name "streamhtmlparser" commit))
             (sha256
              (base32
               "0bmrdakk930q3m8fmq0xcy7n7cdvlk1xma4z9204919hvb1gk9md")))))))
    (home-page "https://github.com/blong42/clearsilver")
    (synopsis "CGI kit and HTML templating system")
    (description
     "This package includes Clearsilver, the CGI kit and HTML templating
system.")
    (license license:bsd-3)))

(define-public python-py-ubjson
  (package
    (name "python-py-ubjson")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py-ubjson" version))
       (sha256
        (base32
         "03l9m9w5ip4hw0y69wlys5gzsfb7zcq3a77blj88grgiqhn5vm5n"))))
    (build-system python-build-system)
    (home-page "https://github.com/Iotic-Labs/py-ubjson")
    (synopsis "Universal Binary JSON encoder/decoder")
    (description
     "Py-ubjson is a Python module providing an Universal Binary JSON
encoder/decoder based on the draft-12 specification for UBJSON.")
    (license license:asl2.0)))

(define-public java-tomcat
  (package
    (name "java-tomcat")
    (version "8.5.63")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/tomcat/tomcat-8/v"
                                  version "/src/apache-tomcat-" version "-src.tar.gz"))
              (sha256
               (base32
                "1wr6mpgbk2gs18vp8mdggiq6vifj68a875dd1fkdf7cs31q54rns"))
              (modules '((guix build utils)))
              ;; Delete bundled jars.
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.jar$"))
                  (for-each delete-file (find-files "." "\\.bat$"))
                  #t))))
    (build-system ant-build-system)
    (inputs
     (list java-commons-daemon java-ecj))
    (arguments
     `(#:build-target "deploy"
       #:tests? #f; requires downloading some files.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prevent-download
           (lambda _
             ;; This directory must exist
             (mkdir "downloads")
             ;; We patch build.xml so it doesn't download any dependency, because
             ;; we already have all of them.
             (substitute* "build.xml"
               (("download-compile,") "")
               (("depends=\"validate\"") "depends=\"build-prepare\"")
               ((",download-validate") ""))
             #t))
         (add-after 'unpack 'strip-timestamps
           (lambda _
             (substitute* "build.xml"
               (("<filter token=\"YEAR\" value=.*")
                "<filter token=\"YEAR\" value=\"1970\"/>")
               (("<filter token=\"VERSION_BUILT\" value=.*")
                "<filter token=\"VERSION_BUILT\" value=\"Jan 1 1970 00:00:00 UTC\"/>"))
             #t))
         (add-after 'unpack 'modify-deploy
           (lambda _
             ;; The Tomcat build downloads and copies these files to the
             ;; bin and lib directory.
             ;; We instead symlink to the input (see below).
             (substitute* "build.xml"
               (("<copy tofile=\"\\$\\{tomcat.build\\}/bin/commons-daemon.jar.*") "")
               (("<copy file=\"\\$\\{jdt.jar\\}\" todir=\"\\$\\{tomcat.build\\}/lib\"/>")
                ""))
             #t))
         (add-after 'install 'symlink-commons-daemon
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((commons-daemon (assoc-ref inputs "java-commons-daemon"))
                    (files (find-files commons-daemon "commons-daemon-.*\\.jar"))
                    (daemon-jar (car files))
                    (out-bin (string-append (assoc-ref outputs "out") "/bin"))
                    (target (string-append out-bin "/commons-daemon.jar")))
               (symlink daemon-jar target)
               #t)))
         (add-after 'install 'symlink-java-ecj
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((java-ecj (assoc-ref inputs "java-ecj"))
                    (files (find-files java-ecj "ecj.*\\.jar"))
                    (java-ecj-jar (car files))
                    (out-lib (string-append (assoc-ref outputs "out") "/lib"))
                    (target (string-append out-lib "/java-ecj.jar")))
               (symlink java-ecj-jar target)
               #t)))
         (add-after 'unpack 'generate-properties
           (lambda _
             ;; This could have been passed to make-flags, but getcwd returns
             ;; a different directory then.
             (with-output-to-file "build.properties"
               (lambda _
                 (display
                   (string-append "base.path=" (getcwd) "/downloads\n"))))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "output/build" out))
             #t)))))
    (properties '((cpe-name . "tomcat")))
    (home-page "https://tomcat.apache.org")
    (synopsis "Java Servlet, JavaServer Pages, Java Expression Language and Java
WebSocket")
    (description "Apache Tomcat is a free implementation of the Java
Servlet, JavaServer Pages, Java Expression Language and Java WebSocket
technologies.")
    (license license:asl2.0)))

(define-public java-eclipse-jetty-test-helper
  (package
    (name "java-eclipse-jetty-test-helper")
    (version "4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eclipse/jetty.toolchain/")
                     (commit (string-append "jetty-test-helper-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g7cdh03nfwbdxzvwm84ysgvw08xx7431lsjryj2gmf3lrqpizgb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-jetty-test-helper.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-test-helper")
             #t))
         (add-before 'build 'fix-paths
           (lambda _
             ;; TODO:
             ;; This file assumes that the build directory is named "target"
             ;; but it is not the case with our ant-build-system. Once we have
             ;; maven though, we will have to rebuild this package because this
             ;; assumption is correct with maven-build-system.
             (substitute*
               "src/main/java/org/eclipse/jetty/toolchain/test/MavenTestingUtils.java"
               (("\"target\"") "\"build\"")
               (("\"tests\"") "\"test-classes\""))
             ;; Tests assume we are building with maven, so that the build
             ;; directory is named "target", and not "build".
             (with-directory-excursion "src/test/java/org/eclipse/jetty/toolchain/test"
               (substitute* '("FSTest.java" "OSTest.java" "TestingDirTest.java"
                              "MavenTestingUtilsTest.java")
                 (("target/tests") "build/test-classes")
                 (("\"target") "\"build")))
             #t)))))
    (inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-all)))
    (home-page "https://www.eclipse.org/jetty/")
    (synopsis "Helper classes for jetty tests")
    (description "This package contains helper classes for testing the Jetty
Web Server.")
    ;; This program is licensed under both epl and asl.
    (license (list license:epl1.0 license:asl2.0))))

(define-public java-eclipse-jetty-perf-helper
  (package
    (inherit java-eclipse-jetty-test-helper)
    (name "java-eclipse-jetty-perf-helper")
    (arguments
     `(#:jar-name "eclipse-jetty-perf-helper.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-perf-helper")
             #t)))))
    (inputs
     `(("hdrhistogram" ,java-hdrhistogram)))))

(define-public java-eclipse-jetty-util
  (package
    (name "java-eclipse-jetty-util")
    (version "9.4.39")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eclipse/jetty.project/"
                                  "archive/jetty-" version ".v20210325.tar.gz"))
              (sha256
               (base32
                "0b4hy4zmdmfbqk9bzmxk7v75y2ysqiappkip4z3hb9lxjvjh0b19"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-jetty-util.jar"
       #:source-dir "src/main/java"
       #:tests? #f; require junit 5
       #:test-exclude
       (list "**/Abstract*.java"
             ;; requires network
             "**/InetAddressSetTest.java"
             ;; Assumes we are using maven
             "**/TypeUtilTest.java"
             ;; Error on the style of log
             "**/StdErrLogTest.java")
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-util")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)))
    (home-page "https://www.eclipse.org/jetty/")
    (synopsis "Utility classes for Jetty")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides utility classes.")
    (license (list license:epl1.0 license:asl2.0))))

;; This version is required by maven-wagon
(define-public java-eclipse-jetty-util-9.2
  (package
    (inherit java-eclipse-jetty-util)
    (version "9.2.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eclipse/jetty.project/"
                                  "archive/jetty-" version ".v20170606.tar.gz"))
              (sha256
               (base32
                "1i51qlsd7h06d35kx5rqpzbfadbcszycx1iwr6vz7qc9gf9f29la"))))
    (arguments
     `(#:jar-name "eclipse-jetty-util.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-exclude
       (list "**/Abstract*.java"
             ;; requires network
             "**/InetAddressSetTest.java"
             ;; Assumes we are using maven
             "**/TypeUtilTest.java"
             ;; We don't have an implementation for slf4j
             "**/LogTest.java"
             ;; Error on the style of log
             "**/StdErrLogTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-util")
             #t))
         (add-before 'build 'fix-test-sources
           (lambda _
             ;; We need to fix issues caused by changes in newer versions of
             ;; jetty-test-helper
             (let ((src "src/test/java/org/eclipse/jetty/util/resource"))
               (substitute* (string-append src "/AbstractFSResourceTest.java")
                 (("testdir.getDir\\(\\)") "testdir.getPath().toFile()")
                 (("testdir.getFile\\(\"foo\"\\)")
                  "testdir.getPathFile(\"foo\").toFile()")
                 (("testdir.getFile\\(name\\)")
                  "testdir.getPathFile(name).toFile()")))
             #t)))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-all" ,java-hamcrest-all)
       ("perf-helper" ,java-eclipse-jetty-perf-helper)
       ("test-helper" ,java-eclipse-jetty-test-helper)))))

(define-public java-eclipse-jetty-io
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-io")
    (arguments
     `(#:jar-name "eclipse-jetty-io.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; require junit 5
       #:test-exclude (list "**/Abstract*.java"
                            ;; Abstract class
                            "**/EndPointTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-io")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Jetty :: IO Utility")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides IO-related utility classes.")))

(define-public java-eclipse-jetty-io-9.2
  (package
    (inherit java-eclipse-jetty-io)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("mockito" ,java-mockito-1)
       ("cglib" ,java-cglib)
       ("objenesis" ,java-objenesis)
       ("asm" ,java-asm)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-http
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-http")
    (arguments
     `(#:jar-name "eclipse-jetty-http.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; require junit 5
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-http")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "src/main/resources/" "build/classes/")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)
       ("io" ,java-eclipse-jetty-io)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Jetty :: Http Utility")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides HTTP-related utility classes.")))

(define-public java-eclipse-jetty-http-9.2
  (package
    (inherit java-eclipse-jetty-http)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ("io" ,java-eclipse-jetty-io-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-jmx
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-jmx")
    (arguments
     `(#:jar-name "eclipse-jetty-jmx.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; FIXME: requires com.openpojo.validation
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-jmx")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Jetty :: JMX Management")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides the JMX management.")))

(define-public java-eclipse-jetty-jmx-9.2
  (package
    (inherit java-eclipse-jetty-jmx)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))))

(define java-eclipse-jetty-http-test-classes
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-http-test-classes")
    (arguments
     `(#:jar-name "eclipse-jetty-http.jar"
       #:source-dir "src/test"
       #:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-http")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("java-javaee-servletapi" ,java-javaee-servletapi)
       ("http" ,java-eclipse-jetty-http)
       ("io" ,java-eclipse-jetty-io)
       ("util" ,java-eclipse-jetty-util)))))

(define java-eclipse-jetty-http-test-classes-9.2
  (package
    (inherit java-eclipse-jetty-http-test-classes)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("http" ,java-eclipse-jetty-http-9.2)
       ,@(package-inputs java-eclipse-jetty-http-9.2)))
    (native-inputs (package-native-inputs java-eclipse-jetty-util-9.2))))

(define-public java-eclipse-jetty-server
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-server")
    (arguments
     `(#:jar-name "eclipse-jetty-server.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; requires a mockito version we don't have
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-server")
             #t))
         (add-before 'build 'fix-source
           (lambda _
             ;; Explicit casts to prevent build failures
             (substitute* "src/main/java/org/eclipse/jetty/server/Request.java"
               (("append\\(LazyList")
                "append((CharSequence)LazyList"))
             (substitute*
               "src/main/java/org/eclipse/jetty/server/handler/ContextHandler.java"
               (((string-append
                   "Class<\\? extends EventListener> clazz = _classLoader==null"
                   "\\?Loader.loadClass\\(ContextHandler.class,className\\):"
                   "_classLoader.loadClass\\(className\\);"))
                (string-append "Class<? extends EventListener> clazz = "
                               "(Class<? extends EventListener>) "
                               "(_classLoader==null?Loader.loadClass("
                               "ContextHandler.class,className):"
                               "_classLoader.loadClass(className));")))
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)
       ("http" ,java-eclipse-jetty-http)
       ("io" ,java-eclipse-jetty-io)
       ("jmx" ,java-eclipse-jetty-jmx)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Core jetty server artifact")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides the core jetty server
artifact.")))

(define-public java-eclipse-jetty-server-9.2
  (package
    (inherit java-eclipse-jetty-server)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ("jmx" ,java-eclipse-jetty-jmx-9.2)
       ("io" ,java-eclipse-jetty-io-9.2)
       ("http" ,java-eclipse-jetty-http-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("test-classes" ,java-eclipse-jetty-http-test-classes-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-security
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-security")
    (arguments
     `(#:jar-name "eclipse-jetty-security.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; require junit 5
       #:test-exclude (list "**/ConstraintTest.*") ; This test fails
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-security")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("servlet" ,java-javaee-servletapi)
       ("http" ,java-eclipse-jetty-http)
       ("server" ,java-eclipse-jetty-server)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Jetty security infrastructure")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides the core jetty security
infrastructure")))

(define-public java-eclipse-jetty-security-9.2
  (package
    (inherit java-eclipse-jetty-security)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ("http" ,java-eclipse-jetty-http-9.2)
       ("server" ,java-eclipse-jetty-server-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("io" ,java-eclipse-jetty-io-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-util-ajax
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-util-ajax")
    (arguments
     `(#:jar-name "eclipse-jetty-util-ajax.jar"
       #:source-dir "jetty-util-ajax/src/main/java"
       #:tests? #f)); require junit 5
    (inputs
     (list java-eclipse-jetty-util java-javaee-servletapi))))

(define-public java-eclipse-jetty-servlet
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-servlet")
    (arguments
     `(#:jar-name "eclipse-jetty-servlet.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; require junit 5
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-servlet")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("java-javaee-servletapi" ,java-javaee-servletapi)
       ("java-eclipse-jetty-util-ajax" ,java-eclipse-jetty-util-ajax)
       ("http" ,java-eclipse-jetty-http)
       ("io" ,java-eclipse-jetty-io)
       ("jmx" ,java-eclipse-jetty-jmx)
       ("security" ,java-eclipse-jetty-security)
       ("server" ,java-eclipse-jetty-server)
       ("util" ,java-eclipse-jetty-util)))
    (synopsis "Jetty Servlet Container")
    (description "The Jetty Web Server provides an HTTP server and Servlet
container capable of serving static and dynamic content either from a standalone
or embedded instantiation.  This package provides the core jetty servlet
container.")))

(define-public java-eclipse-jetty-servlet-9.2
  (package
    (inherit java-eclipse-jetty-servlet)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (arguments
     `(#:jar-name "eclipse-jetty-servlet.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; doesn't work
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-servlet")
             #t)))))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ("jmx" ,java-eclipse-jetty-jmx-9.2)
       ("io" ,java-eclipse-jetty-io-9.2)
       ("http" ,java-eclipse-jetty-http-9.2)
       ("security" ,java-eclipse-jetty-security-9.2)
       ("http-test" ,java-eclipse-jetty-http-test-classes-9.2)
       ("server" ,java-eclipse-jetty-server-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-xml
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-xml")
    (arguments
     `(#:jar-name "eclipse-jetty-xml.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; most tests require network
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-xml")
             #t)))))
    (inputs
     (list java-eclipse-jetty-util))
    (native-inputs
     (modify-inputs (package-native-inputs java-eclipse-jetty-util)
       (prepend java-eclipse-jetty-io)))))

(define-public java-eclipse-jetty-xml-9.2
  (package
    (inherit java-eclipse-jetty-xml)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (arguments
     `(#:jar-name "eclipse-jetty-xml.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; most tests require network
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-xml")
             #t)))))
    (inputs
     `(("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("java-eclipse-jetty-io-9.2" ,java-eclipse-jetty-io-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-webapp
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-webapp")
    (arguments
     `(#:jar-name "eclipse-jetty-webapp.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; require junit 5
       ;; One test fails
       #:test-exclude (list "**/WebAppContextTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-webapp")
             #t)))))
    (inputs
     (list java-eclipse-jetty-util
           java-eclipse-jetty-http
           java-eclipse-jetty-io
           java-eclipse-jetty-server
           java-eclipse-jetty-servlet
           java-eclipse-jetty-security
           java-eclipse-jetty-xml
           java-javaee-servletapi))))

(define-public java-eclipse-jetty-webapp-9.2
  (package
    (inherit java-eclipse-jetty-webapp)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (arguments
     `(#:jar-name "eclipse-jetty-webapp.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-exclude (list "**/WebAppContextTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-webapp")
             #t)))))
    (inputs
     `(("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-http-9.2" ,java-eclipse-jetty-http-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-eclipse-jetty-xml-9.2" ,java-eclipse-jetty-xml-9.2)
       ("java-javaee-servletapi" ,java-javaee-servletapi)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("java-eclipse-jetty-io-9.2" ,java-eclipse-jetty-io-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-jsoup
  (package
    (name "java-jsoup")
    (version "1.10.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jhy/jsoup")
                     (commit (string-append "jsoup-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hdpdx0x140r5x3yc251v7dj1h4j5a7nh9k885aw9q5vvz49lkf4"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jsoup.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (let ((classes-dir (string-append (getcwd) "/build/classes")))
               (with-directory-excursion "src/main/java"
                 (for-each (lambda (file)
                             (let ((dist (string-append classes-dir "/" file)))
                               (mkdir-p (dirname dist))
                               (copy-file file dist)))
                   (find-files "." ".*.properties"))))
             #t)))))
    (native-inputs
     (list java-junit java-hamcrest-core java-gson))
    (home-page "https://jsoup.org")
    (synopsis "HTML parser")
    (description "Jsoup is a Java library for working with real-world HTML.  It
provides a very convenient API for extracting and manipulating data, using the
best of DOM, CSS, and jQuery-like methods.")
    (license license:expat)))

(define-public java-signpost-core
  (package
    (name "java-signpost-core")
    (version "1.2.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mttkay/signpost")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l04yj2znch3hpyw90c4g4jan453w7d88l84bgl0c72i2kbb8z7h"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "signpost-core.jar"
       #:source-dir "signpost-core/src/main/java"
       #:test-dir "signpost-core/src/test"
       ;; Tests all fail with InstantiationException from mockito
       #:tests? #f))
    (propagated-inputs
     (list java-commons-codec))
    (home-page "https://github.com/mttkay/signpost")
    (synopsis "Lightweight client-side OAuth library for Java")
    (description "Signpost is the easy and intuitive solution for signing
HTTP messages on the Java platform in conformance with the OAuth Core 1.0a
standard.  Signpost follows a modular and flexible design, allowing you to
combine it with different HTTP messaging layers.")
    (license license:asl2.0)))

(define-public tidyp
  (package
    (name "tidyp")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/downloads/petdance/tidyp/tidyp-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0f5ky0ih4vap9c6j312jn73vn8m2bj69pl2yd3a5nmv35k9zmc10"))))
    (build-system gnu-build-system)
    ;; ./test-thing.sh tries to run ./testall.sh, which is not included.
    (arguments `(#:tests? #f))
    (home-page "http://www.tidyp.com/")
    (synopsis "Validate HTML")
    (description "Tidyp is a program that can validate your HTML, as well as
modify it to be more clean and standard.  tidyp does not validate HTML 5.

libtidyp is the library on which the program is based.  It can be used by any
other program that can interface to it.  The Perl module @code{HTML::Tidy} is
based on this library, allowing Perl programmers to easily validate HTML.")
    ;; See htmldoc/license.html
    (license license:bsd-3)))

(define-public perl-html-tidy
  (package
    (name "perl-html-tidy")
    (version "1.60")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/HTML-Tidy-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1iyp2fd6j75cn1xvcwl2lxr8qpjxssy2360cyqn6g3kzd1fzdyxw"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tidyp-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile.PL"
               (("^my \\$inc = \"" line)
                (string-append line
                               "-I" (assoc-ref inputs "tidyp") "/include/tidyp "))
               (("-L/usr/lib")
                (string-append
                 "-L" (assoc-ref inputs "tidyp") "/lib")))
             #t)))))
    (inputs
     (list perl-libwww tidyp))
    (native-inputs
     (list perl-test-exception))
    (home-page "https://metacpan.org/release/HTML-Tidy")
    (synopsis "(X)HTML validation in a Perl object")
    (description "@code{HTML::Tidy} is an HTML checker in a handy dandy
object.  It's meant as a replacement for @code{HTML::Lint}, which is written
in Perl but is not nearly as capable as @code{HTML::Tidy}.")
    (license license:artistic2.0)))

(define-public gophernicus
  ;; Contains some unreleased fixes.
  (let ((commit "da3390089c2a856db1ab2e3bd9751b9a9101a33a")
        (revision "0"))
    (package
      (name "gophernicus")
      (version (git-version "3.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gophernicus/gophernicus")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0a7kpymwqcsqzszdxvcqppbg61bpyg9f7raj783pldm4kf2wjyij"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f                ; No tests
             #:configure-flags
             ;; Listener and hostname used only in configuration files, which
             ;; we don't install.
             ;; This is what's done in the release.sh script.
             #~(list "--listener=none" "--hostname=HOSTNAME")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-version
                   (lambda _
                     (substitute* "Makefile.in"
                       (("^(VERSION += ).*" _ prefix)
                        (string-append prefix #$version "\n")))
                     ;; This is done in the release.sh script.
                     (substitute* "README.md"
                       (("^(This release: Version )DEVEL\\b.*" _ prefix)
                        (string-append prefix #$version "\n"))
                       (("^NOTE: The master branch is rolling Development\\b.*")
                        ""))))
                 (replace 'configure
                   ;; The configure script is hand-written, not from GNU autotools.
                   (lambda* (#:key configure-flags #:allow-other-keys)
                     (setenv "CC" #$(cc-for-target))
                     (setenv "HOSTCC" "gcc")
                     (apply invoke "./configure"
                            (string-append "--prefix=" #$output)
                            configure-flags))))))
      ;; TODO: Make configure script find libwrap.
      ;;(inputs
      ;; (list tcp-wrappers))
      (home-page "https://gophernicus.org/")
      (synopsis "Gopher protocol server")
      (description
       "Gophernicus is a Gopher protocol server.  Its features include:
@itemize
@item written with security in mind;
@item automatically generated Gopher menus;
@item gophertags for virtually renaming directories;
@item personal gopherspaces, located in @file{~/public_gopher/};
@item virtual hosting;
@item CGI support;
@item output filtering and PHP support;
@item charset support and conversions;
@item selector rewriting;
@item session tracking and statistics;
@item TLS/SSL and proxy support.
@end itemize")
      (license license:bsd-2))))

(define-public gemget
  (package
    (name "gemget")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/makew0rld/gemget")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03x9apk73lwyafc4fd2vs033z7vcpk4k0jf97452l7pnlx2v57rz"))))
    (build-system go-build-system)
    (native-inputs
     (list go-github-com-dustin-go-humanize
           go-github-com-makeworld-the-better-one-go-gemini
           go-github-com-makeworld-the-better-one-go-gemini-socks5
           go-github-com-schollz-progressbar-v3
           go-github-com-spf13-pflag))
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/makeworld-the-better-one/gemget"))
    (home-page "https://github.com/makew0rld/gemget")
    (synopsis "Command line downloader for the Gemini protocol")
    (description
     "Gemget is a command line downloader for the Gemini protocol.
It works well with streams and can print headers for debugging as well.")
    (license license:expat)))

(define-public geomyidae
  (package
    (name "geomyidae")
    (version "0.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://r-36.net/geomyidae")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02afgrk36wkdkflyqr2xgh49v9zq6ma454jshk7igvhpxfb5l3ks"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:tests? #f                      ; no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "http://r-36.net/scm/geomyidae/file/README.html")
    (synopsis "Small Gopher server")
    (description
     "Geomyidae is a server for distributed hypertext protocol Gopher.  Its
features include:

@enumerate
@item Gopher menus (see @file{index.gph} for an example);
@item directory listings (if no @file{index.gph} was found);
@item CGI support (@file{.cgi} files are executed);
@item search support in CGI files;
@item logging with multiple log levels.
@end enumerate\n")
    (license license:expat)))

(define-public monsterid
  (let ((commit "5597f177b473343ff5cad9a6e0e5b255312c6096")
        (revision "0"))
    (package
      (name "monsterid")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/splitbrain/monsterID")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ixyrrcbw96plcdna2rx1pqwisqy9hnr57kvamgj13lzlv2whdb3"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("monsterid.php" "share/web/monsterid/")
                          ("parts/" "share/web/monsterid/parts/"
                           #:include-regexp ("\\.png$")))))
      (home-page "https://www.splitbrain.org/projects/monsterid")
      (synopsis "The original MonsterID implementation")
      (description
       "MonsterID is a method to generate a unique monster image based upon a
certain identifier (IP address, email address, whatever).  It can be
used to automatically provide personal avatar images in blog comments
or other community services.")
      (license license:expat))))

(define-public cat-avatar-generator
  (let ((commit "9360ea33f79d1dad3e43494b09878b5e3f6b41fa")
        (revision "1"))
    (package
      (name "cat-avatar-generator")
      (version (git-version "1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://framagit.org/Deevad/cat-avatar-generator.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0js4grqzsm4gvmcbmxv7zw4samfzi6nk4mn977ddcvla9g222rkm"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils)
                        (srfi srfi-1)
                        (srfi srfi-26))
           (let ((source (assoc-ref %build-inputs "source"))
                 (php-dir (string-append %output "/share/web/" ,name)))
             (install-file (string-append source "/cat-avatar-generator.php") php-dir)
             (copy-recursively (string-append source "/avatars") (string-append php-dir "/avatars"))
             ;; The cache directory must not be in the store, but in a writable
             ;; location.  The webserver will give us this location.
             (substitute* (string-append php-dir "/cat-avatar-generator.php")
               (("\\$cachepath = .*")
                "if(isset($_SERVER['CACHE_DIR']))
$cachepath = $_SERVER['CACHE_DIR'];
else
die('You need to set the CACHE_DIR variable first.');"))
             #t))))
      (home-page "https://framagit.org/Deevad/cat-avatar-generator")
      (synopsis "Random avatar generator")
      (description "Cat avatar generator is a generator of cat pictures optimised
to generate random avatars, or defined avatar from a \"seed\".  This is a
derivation by David Revoy from the original MonsterID by Andreas Gohr.")
      ;; expat for the code, CC-BY 4.0 for the artwork
      (license (list license:expat
                     license:cc-by4.0)))))

(define-public nghttp2
  (package
    (name "nghttp2")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/nghttp2/nghttp2/"
                           "releases/download/v" version "/"
                           "nghttp2-" version ".tar.xz"))
       (sha256
        (base32
         "1q4ps8acr7nyia7mf2z11m0yh3fn1imhyv855j3xjbx91l2a6s2a"))))
    (build-system gnu-build-system)
    (outputs (list "out"
                   "lib"))              ; only libnghttp2
    (native-inputs
     (list pkg-config
           ;; Required by tests.
           cunit python tzdata-for-tests))
    (inputs
     ;; Required to build the tools (i.e. without ‘--enable-lib-only’).
     (append
      (if (target-hurd?)
          `((,openssl "static"))
          (list jemalloc))              ; fight nghttpd{,x} heap fragmentation
      (list c-ares
            jansson                     ; for HPACK tools
            libev
            libxml2                     ; for ‘nghttp -a’
            openssl)))
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--libdir=" #$output:lib "/lib")
              "--enable-app"            ; build all the tools
              "--enable-hpack-tools"    ; ...all the tools
              "--disable-examples"
              "--disable-static"        ; don't bother building .a files
              #$@(if (%current-target-system)
                     '("--disable-python-bindings")
                     '()))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'break-circular-reference
            ;; libnghttp2.pc by default retains a reference to the ‘out’ output,
            ;; which is not allowed.  Break this cycle.  While we could install
            ;; only the library to ‘out’ and move everything else to a separate
            ;; output, this would inconvenience the majority of (human) users.
            (lambda _
              (substitute* "lib/libnghttp2.pc.in"
                (("@prefix@")
                 #$output:lib))))
          (add-before 'check 'set-timezone-directory
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (setenv "TZDIR" (search-input-directory
                               (or native-inputs inputs)
                               "share/zoneinfo")))))))
    (home-page "https://nghttp2.org/")
    (synopsis "HTTP/2 protocol client, proxy, server, and library")
    (description
     "nghttp2 implements the Hypertext Transfer Protocol, version
2 (@dfn{HTTP/2}).

A reusable C library provides the HTTP/2 framing layer, with several tools built
on top of it:

@itemize
@item @command{nghttp}, a command-line HTTP/2 client.  It exposes many advanced
and low-level aspects of the protocol and is useful for debugging.
@item @command{nghttpd}, a fast, multi-threaded HTTP/2 static web server that
serves files from a local directory.
@item @command{nghttpx}, a fast, multi-threaded HTTP/2 reverse proxy that can be
deployed in front of existing web servers that don't support HTTP/2.
Both @command{nghttpd} and @command{nghttpx} can fall back to HTTP/1.1 for
backwards compatibility with clients that don't speak HTTP/2.
@item @command{h2load} for benchmarking (only!) your own HTTP/2 servers.
@item HTTP/2 uses a header compression method called @dfn{HPACK}.
nghttp2 provides a HPACK encoder and decoder as part of its public API.
@item @command{deflatehd} converts JSON data or HTTP/1-style header fields to
compressed JSON header blocks.
@item @command{inflatehd} converts such compressed headers back to JSON pairs.
@end itemize\n")
    (license license:expat)))

(define-public nghttp3
  (package
    (name "nghttp3")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ngtcp2/nghttp3/"
                           "releases/download/v" version "/"
                           "nghttp3-" version ".tar.gz"))
       (sha256
        (base32
         "0gpnqibb1ndqq7yacl2f9d7iznfbzws71rza12kaf72shqvyn1zv"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-lib-only")))
    (home-page "https://nghttp2.org/nghttp3/")
    (synopsis "HTTP/3 protocol library")
    (description
     "nghttp3 is an implementation of RFC 9114 HTTP/3 mapping over QUIC and
RFC 9204 QPACK in C.  It does not depend on any particular QUIC transport
implementation.

It implements extensions specified in RFC 9218 and RFC 9220.  It supports
SETTINGS_H3_DATAGRAM from RFC 9297.

It does not support server push.")
    (license license:expat)))

(define-public hpcguix-web
  (package
    (name "hpcguix-web")
    (version "0.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/UMCUGenetics/hpcguix-web")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09xfyyz3004qcfjjlg903gnsb9wsrrdk7gw7xawsvw58l6vrialb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (ice-9 popen)
                  (ice-9 rdelim))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-variables
           (lambda _
             ;; This prevents a few warnings
             (setenv "GUILE_AUTO_COMPILE" "0")
             (setenv "XDG_CACHE_HOME" (getcwd))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (guix     (assoc-ref inputs "guix"))
                    (guile    (assoc-ref inputs "guile"))
                    (gcrypt   (assoc-ref inputs "guile-gcrypt"))
                    (git      (assoc-ref inputs "guile-git"))
                    (gnutls   (assoc-ref inputs "guile-gnutls"))
                    (bs       (assoc-ref inputs "guile-bytestructures"))
                    (json     (assoc-ref inputs "guile-json"))
                    (zlib     (assoc-ref inputs "guile-zlib"))
                    (syntax   (assoc-ref inputs "guile-syntax-highlight"))
                    (guile-cm (assoc-ref inputs
                                         "guile-commonmark"))
                    (deps (list guile gcrypt git gnutls bs zlib guile-cm
                                syntax guix json))
                    (effective
                     (read-line
                      (open-pipe* OPEN_READ
                                  (string-append guile "/bin/guile")
                                  "-c" "(display (effective-version))")))
                    (path   (string-join
                             (map (cut string-append <>
                                       "/share/guile/site/"
                                       effective)
                                  deps)
                             ":"))
                    (gopath (string-join
                             (map (cut string-append <>
                                       "/lib/guile/" effective
                                       "/site-ccache")
                                  deps)
                             ":")))
               (wrap-program (string-append out "/bin/hpcguix-web")
                 `("GUILE_LOAD_PATH" ":" prefix (,path))
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,gopath)))))))))
    (native-inputs
     (list autoconf automake uglify-js pkg-config
           (lookup-package-native-input guix "guile")))
    (inputs
     (list bash-minimal
           (lookup-package-native-input guix "guile")
           guix
           guile-zlib
           guile-commonmark
           guile-json-4
           guile-syntax-highlight
           guile-gnutls    ;used to connect to https://disarchive.guix.gnu.org
           bash-minimal))
    (home-page "https://github.com/UMCUGenetics/hpcguix-web")
    (synopsis "Web interface for cluster deployments of Guix")
    (description "Hpcguix-web provides a web interface to the list of packages
provided by Guix.  The list of packages is searchable and provides
instructions on how to use Guix in a shared HPC environment.")
    (license license:agpl3+)))

(define-public httrack
  (package
    (name "httrack")
    (version "3.49.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xroche/httrack")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1izn1h7gaxb2barclm2pj5kaz1mmddx2c35n70m0552q8ms4lvks"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'autogen
            ;; Force reconfiguration to generate "test-driver".
            (lambda _
              (substitute* "configure.ac"
                ;; Fix errors when running "configure" script.
                (("AX_CHECK_(COMPILE|LINK)_FLAG\\(.*") "")
                (("AX_CHECK_ALIGNED_ACCESS_REQUIRED") "")
                (("gl_VISIBILITY") ""))
              (invoke "autoreconf" "-vif")))
          (add-after 'unpack 'copy-coucal-source
            ;; Install Coucal source to work around missing submodule.
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each (lambda (f) (install-file f "src/coucal"))
                        (find-files #$(this-package-input "coucal")
                                    "\\.(c|h|diff|orig)$")))))))
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list coucal libressl zlib))
    (home-page "https://www.httrack.com/")
    (synopsis "Easy-to-use offline browser utility")
    (description "HTTrack allows you to download a World Wide Web site from
the Internet to a local directory, building recursively all directories,
getting HTML, images, and other files from the server to your computer.

HTTrack arranges the original site's relative link-structure.  Simply open
a page of the @emph{mirrored} website in your browser, and you can browse the
site from link to link, as if you were viewing it online.  HTTrack can also
update an existing mirrored site, and resume interrupted downloads.

HTTrack is fully configurable, and has an integrated help system.")
    (license license:gpl3+)))

(define-public binaryen
  (package
    (name "binaryen")
    (version "112")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WebAssembly/binaryen")
             (commit (string-append "version_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0970iz22yjxgi27d67kwmrx4zq7hig3i6b92vmlp4c4bd1bacny5"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'use-system-googletest
           (lambda _
             (substitute* "third_party/CMakeLists.txt"
               (("  googletest/.*") "")
               (("add_library\\(gtest.*") ""))
             (substitute* "CMakeLists.txt"
               (("add_subdirectory\\(test/gtest\\)")
                "find_package(GTest REQUIRED)")))))))
    (native-inputs (list googletest))
    (home-page "https://github.com/WebAssembly/binaryen")
    (synopsis "Optimizer and compiler/toolchain library for WebAssembly")
    (description "Binaryen is a compiler and toolchain infrastructure library
for WebAssembly, written in C++.  It aims to make compiling to WebAssembly
easy, fast, and effective.")
    (license license:asl2.0)))

(define-public buku
  (package
    (name "buku")
    (version "4.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "buku" version))
       (sha256
        (base32 "1n4d1mkjyvzdxbyq067p1p9skb3iwx0msd86nzr224dlqrfh9675"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                     ; FIXME: many tests need network access
       #:phases
       (modify-phases %standard-phases
         ;; XXX: missing inputs, e.g. python-flask-admin
         (delete 'sanity-check))))
    (inputs
     (list python-beautifulsoup4 python-certifi python-cryptography
           python-flask python-html5lib python-urllib3))
    (home-page "https://github.com/jarun/buku")
    (synopsis "Bookmark manager")
    (description
     "buku is a powerful bookmark manager written in Python3 and SQLite3.
@command{buku} can auto-import bookmarks from your browser and present them
in an interactive command-line interface that lets you compose and update
bookmarks directly.  It can also present them in a web interface with
@command{bukuserver}.")
    (license license:gpl3+)))

(define-public buku-run
  (package
    (name "buku-run")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/carnager/buku_run")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zyjjf3b8g3dnymcrg683rbnc6qrvx8ravfm833n7kjrqky3bczn"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no tests
           #:make-flags
           #~(list (string-append "DESTDIR=" #$output)
                   "PREFIX=")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'fixpath
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "buku_run"
                     ((" \\<(rofi)\\>" all cmd)
                      (string-append " " (search-input-file inputs "/bin/rofi")))
                     (("\\<(buku)\\> " all cmd)
                      (string-append (search-input-file inputs "/bin/buku") " "))
                     (("\\<(awk|gawk)\\>" cmd)
                      (search-input-file inputs "/bin/awk"))
                     (("/etc/buku_run.config" path)
                      (string-append #$output path))))))))
    (inputs (list buku rofi))
    (home-page "https://github.com/carnager/buku_run")
    (synopsis "@command{rofi} frontend for buku bookmarks manager")
    (description
     "This package provides a rofi frontend for the buku bookmark manager.")
    (license license:gpl3+)))

(define-public tissue
  (package
    (name "tissue")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tissue.systemreboot.net/releases/tissue-"
                                  version ".tar.lz"))
              (sha256
               (base32
                "0vsybgnzv8nnwf58pnxrs4101xczl8jvxd1wzmk4vmdyrp8a2kkm"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "prefix=" #$output))
           #:modules `(((guix build guile-build-system)
                        #:select (target-guile-effective-version))
                       (guix build gnu-build-system)
                       (guix build utils))
           #:phases
           (with-imported-modules '((guix build guile-build-system))
             #~(modify-phases %standard-phases
                 (replace 'patch-source-shebangs
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "bin/tissue"
                       (("^exec guile")
                        (string-append "exec "
                                       (search-input-file inputs "/bin/guile"))))))
                 (delete 'configure)
                 (add-after 'install 'wrap
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out"))
                           (effective-version (target-guile-effective-version)))
                       (wrap-program (string-append out "/bin/tissue")
                         `("GUILE_LOAD_PATH" prefix
                           (,(string-append out "/share/guile/site/" effective-version)
                            ,(getenv "GUILE_LOAD_PATH")))
                         `("GUILE_LOAD_COMPILED_PATH" prefix
                           (,(string-append out "/lib/guile/"
                                            effective-version "/site-ccache")
                            ,(getenv "GUILE_LOAD_COMPILED_PATH")))))))))))
    (inputs (list bash-minimal guile-3.0 guile-filesystem guile-git guile-xapian))
    (native-inputs (list lzip))
    (propagated-inputs (list skribilo))
    (home-page "https://tissue.systemreboot.net")
    (synopsis "Text based project information management system")
    (description "tissue is an issue tracker and project information
management system built on plain text files and git.  It is specifically
intended for small free software projects.  It features a static site
generator to build a project website and a powerful search interface to search
through project issues and documentation.  The search interface is built on
the Xapian search engine library, and is available both as a command-line
program and as a web server.")
    (license license:gpl3+)))

(define-public anonip
  (package
    (name "anonip")
    (version "1.1.0")
    ;; The version on PyPi does not include fixture definitions for tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DigitaleGesellschaft/Anonip")
                    (commit "beab328945547b0147a53655f32c5cc76ab4488b")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cssdcridadjzichz1vv1ng7jwphqkn8ihh83hpz9mcjmxyb94qc"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" "tests.py" "anonip.py")))))))
    (native-inputs
     (list python-pytest python-pytest-cov))
    (home-page "https://github.com/DigitaleGesellschaft/Anonip")
    (synopsis "Anonymize IP addresses in log files")
    (description
     "Anonip masks the last bits of IPv4 and IPv6 addresses in log files.
That way most of the relevant information is preserved, while the IP address
does not match a particular individuum anymore.

Depending on your Web server, the log entries may be piped to Anonip directly
or via a FIFO (named pipe).  Thus the unmasked IP addresses will never be
written to any file.

It's also possible to rewrite existing log files.

Anonip can also be uses as a Python module in your own Python application.")
    (license license:bsd-3)))

(define-public poussetaches
  (package
    (name "poussetaches")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tsileo/poussetaches")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0kckcwvqklavd855np9aq5js6mg84isrlwchr504yigwma0sm7hm"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-github-com-robfig-cron-v3 go-golang-org-x-time))
    (arguments
     `(#:import-path "github.com/tsileo/poussetaches"))
    (home-page "https://github.com/tsileo/poussetaches")
    (synopsis "Lightweight asynchronous task execution service")
    (description "Poussetaches (which literally means \"push tasks\" in
French) is a lightweight asynchronous task execution service that aims to
replace Celery and RabbitMQ for small Python applications.

The app posts base64-encoded payload to poussetaches and specifies the
endpoint that will be used to trigger the task.  Poussetaches makes HTTP
requests with the registered payload until the right status code is
returned.")
    (license license:isc)))

(define-public htmlcxx
  (package
    (name "htmlcxx")
    (version "0.87")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/htmlcxx/v"
                       version "/htmlcxx-" version ".tar.gz"))
       (sha256
        (base32 "1j3mzjlczjrk4ahc43s6kzpvzypzjmqz4sillnca5yadrwwgjf2x"))))
    (build-system gnu-build-system)
    (arguments
     ;; ISO C++17 does not allow dynamic exception specifications
     `(#:configure-flags '("CXXFLAGS=-std=c++11")))
    (home-page "https://htmlcxx.sourceforge.net/")
    (synopsis "Simple non-validating CSS1 and HTML parser for C++")
    (description "htmlcxx is a simple non-validating CSS1 and HTML parser for
C++.  Although there are several other HTML parsers available, htmlcxx has some
characteristics that make it unique:
@itemize
@item STL like navigation of DOM tree, using excelent's tree.hh library from
Kasper Peeters
@item It is possible to reproduce exactly, character by character, the original
document from the parse tree
@item Bundled CSS parser
@item Optional parsing of attributes
@item C++ code that looks like C++ (not so true anymore)
@item Offsets of tags/elements in the original document are stored in the nodes
of the DOM tree
@end itemize")
    (license (list license:lgpl2.0
                   license:gpl2
                   license:asl2.0))))

(define-public librocket
  (package
    (name "librocket")
    (version "1.3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/libRocket/libRocket")
         (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n6gq007vqijyfasfnfg6c8d2rc9qarl4bhzbgkz062m4h5izlfs"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "Build"))))))
    (inputs
     (list freetype))
    (home-page "https://github.com/libRocket/libRocket") ; http://librocket.com/ is down.
    (synopsis "HTML/CSS user interface library")
    (description "libRocket is a C++ user interface package based on the HTML
and CSS standards.  libRocket uses the open standards XHTML1.0 and
CSS2.0 (while borrowing features from HTML5 and CSS3), and extends them with
features suited towards real-time applications.  It is designed as a complete
solution for any project's interface needs:

@itemize
@item Dynamic layout system.
@item Efficient application-wide styling, with a custom-built templating engine.
@item Fully featured control set: buttons, sliders, drop-downs, etc.
@item Runtime visual debugging suite.
@item Easily integrated and extensible with Python or Lua scripting.
@end itemize\n")
    (license license:expat)))

(define-public gmid
  (package
    (name "gmid")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/omar-polo/gmid/releases/download/"
                    version "/gmid-" version ".tar.gz"))
              (sha256
               (base32
                "1gy41858xxgbvngw7b162sq8vddd104a3cdd53pp2vk1f91gxc4y"))))
    (build-system gnu-build-system)
    (arguments
     (list #:test-target "regress"
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (setenv "CC" #$(cc-for-target))
                   (invoke "./configure"
                           (string-append "PREFIX=" #$output)))))))
    (native-inputs (list bison
                         coreutils
                         flex
                         pkg-config
                         procps))
    (inputs (list libevent libressl))
    (home-page "https://gmid.omarpolo.com/")
    (synopsis "Simple and secure Gemini server")
    (description "@command{gmid} is a fast Gemini server written with security
in mind.  It has features such as:
@itemize
@item reload the running configuration without interruption
@item automatic redirect/error pages
@item IRI support (RFC3987)
@item reverse proxying
@item CGI and FastCGI support
@item virtual hosts
@item location rules
@item event-based asynchronous I/O model
@item low memory footprint.
@end itemize")
    (license license:isc)))

(define-public kiln
  (package
    (name "kiln")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~adnano/kiln")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ynb079jsyv6viwdksavwar5lqj84ssfw39dl5da98z683xrvch5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~adnano/kiln"
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-man
            (lambda _
              (let ((man1 (string-append #$output "/share/man/man1")))
                (system (string-append
                         "scdoc"
                         "< src/git.sr.ht/~adnano/kiln/docs/kiln.1.scd"
                         "> kiln.1"))
                (install-file "kiln.1" man1)))))))
    (native-inputs
     (list scdoc))
    (propagated-inputs
     (list go-github-com-google-shlex
           go-github-com-pelletier-go-toml
           go-gopkg-in-yaml-v3))
    (home-page "https://kiln.adnano.co/")
    (synopsis "Simple static site generator")
    (description
     "Kiln takes a different approach to building static sites.
Instead of packing all functionality into kiln itself, the core is lightweight
and can be extended with the use of external commands.")
    (license license:expat)))

(define-public siege
  (package
    (name "siege")
    (version "4.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.joedog.org/siege/siege-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1a74py0ib1gr3znv9ah5acw67ngl08b14dbc90ww9clvgdr2ag0l"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list (string-append "--with-zlib="
                                              (assoc-ref %build-inputs "zlib"))
                               (string-append "--with-ssl="
                                              (assoc-ref %build-inputs
                                                         "openssl")))))
    (inputs (list openssl zlib))
    (home-page "https://www.joedog.org/siege-home/")
    (synopsis "HTTP/FTP load tester and benchmarking utility")
    (description
     "Siege is a multi-threaded HTTP/FTP load tester and benchmarking utility.  It
can stress test a single URL with a user defined number of simulated users, or
it can read many URLs into memory and stress them simultaneously.  The program
reports the total number of hits recorded, bytes transferred, response time,
concurrency, and return status.")
    ;; GPLv3+ with OpenSSL linking exception.
    (license license:gpl3+)))

(define-public gmnisrv
  (package
    (name "gmnisrv")
    (version "1.0")
    (home-page "https://git.sr.ht/~sircmpwn/gmnisrv")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (sha256
        (base32 "115r1dw9k08r2nvygy8ll21qvsc5kmzi5jcqm7g7r8q8hifxglap"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no check target
       #:configure-flags (list "--sysconfdir=/etc"
                               (string-append "--with-mimedb="
                                              (assoc-ref %build-inputs "mailcap")
                                              "/etc/mime.types"))
       #:make-flags (list (string-append "CC=" ,(cc-for-target)))))
    (inputs
     (list mailcap openssl))
    (native-inputs
     (list pkg-config scdoc))
    (synopsis "Simple Gemini protocol server")
    (description "gmnisrv is a simple Gemini protocol server written in C.")
    (license (list license:gpl3+
                   license:bsd-3)))) ;; for ini.c and ini.h

(define-public vger
  (package
    (name "vger")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://tildegit.org/solene/vger")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jiwzn5dqadwq4ih3vzld66yq23gqsf7281sllh29bf6kmf9dz2k"))))
    (build-system gnu-build-system)
    (arguments
     (list #:test-target "test"
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-makefile
                 (lambda _
                   (substitute* "Makefile"
                     (("\\binstall -o root -g wheel vger ")
                      "install vger ")
                     (("\\binstall -o root -g wheel vger\\.8 ")
                      "install -m 644 vger.8 "))))
               (add-before 'install 'make-install-dirs
                 (lambda _
                   (mkdir-p (string-append #$output "/bin"))
                   (mkdir-p (string-append #$output "/man/man8")))))))
    (inputs
     (list libbsd))
    (home-page "https://tildegit.org/solene/vger")
    (synopsis "Gemini protocol server")
    (description "Vger is a Gemini protocol server that supports chroots,
virtualhosts, CGI, default language choice, redirections and MIME-type
detection.  It delegates TLS support to an external daemon, for example
@command{stunnel} on @command{inetd}.")
    (license license:bsd-2)))

(define-public kineto
  (package
    (name "kineto")
    (version "0.0.0-20211105093215-857f8c97ebc5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~sircmpwn/kineto")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r17c904i76yy5ilvhjczmhnq5g7r4nkjwmsjcfxcqzly0ia7m2k"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "git.sr.ht/~sircmpwn/kineto/"))
    (propagated-inputs
     (list go-git-sr-ht-sircmpwn-getopt go-git-sr-ht-adnano-go-gemini))
    (home-page "https://git.sr.ht/~sircmpwn/kineto/")
    (synopsis "HTTP proxy for Gemini")
    (description
     "This is an @acronym{HTTP} to
@url{https://gemini.circumlunar.space/,Gemini} proxy designed to provide
service for a single domain, i.e.  to make your Gemini site available over
HTTP.  It can proxy to any domain in order to facilitate linking to the rest
of Geminispace, but it defaults to a specific domain.")
    (license license:gpl3+)))

(define-public libzim
  (package
    (name "libzim")
    (version "8.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openzim/libzim")
                    (commit version)))
              (sha256
               (base32
                "1g735aqw0vlxqgyjv02lvq24dr5shydp4y8mqianf8720s5fs73f"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (arguments
     ;; TODO: Find out why tests fail.
     '(#:tests? #f))
    (inputs
     (list icu4c
           python-wrapper ; for libzim-compile-resources
           xapian
           xz
           (list util-linux "lib")
           (list zstd "lib")))
    (native-inputs
     (list pkg-config googletest))
    (home-page "https://wiki.openzim.org/wiki/Main_Page")
    (synopsis "Reference implementation of the ZIM specification")
    (description "The openZIM project proposes offline storage solutions for
content coming from the Web.  The zimlib is the standard implementation of the
ZIM specification.  It is a library which implements the read and write method
for ZIM files.")
    (license license:gpl2)))

(define-public kiwix-lib
  (package
    (name "kiwix-lib")
    (version "13.0.0")
    (home-page "https://github.com/kiwix/kiwix-lib/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (sha256
               (base32
                "0mvlppbj0mqn4ka3cfaaj1pvn062cxbgz01c0nq04x0mzq1xwh5w"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-paths-and-includes
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/aria2.cpp"
               (("ARIA2_CMD \"aria2c\"")
                (string-append "ARIA2_CMD \""
                               (search-input-file inputs "/bin/aria2c")
                               "\""))))))))
    (inputs
     (list aria2
           curl
           icu4c
           libmicrohttpd
           libzim
           pugixml
           xapian
           zlib
           `(,zstd "lib")))
    (native-inputs
     (list cpp-mustache
           pkg-config
           ;; for kiwix-compile-resources
           python-wrapper))
    (synopsis "Common code base for all Kiwix ports")
    (description "The Kiwix library provides the Kiwix software suite core.
It contains the code shared by all Kiwix ports.")
    (license license:gpl3)))

(define-public kiwix-desktop
  (package
    (name "kiwix-desktop")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.kiwix.org/release/kiwix-desktop/kiwix-desktop-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "0hlk05gcb3fmnxhwj6gan51v98rdq3iv2lklwbpmm1bazmz8i7br"))
              (patches (search-patches "kiwix-desktop-newer-libkiwix.patch"))))
    (build-system qt-build-system)
    (arguments
     `(#:test-target "check"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX="
                                    (assoc-ref outputs "out")))))
         (add-after 'install 'wrap-qt-process-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/kiwix-desktop"))
                    (qt-process-path (search-input-file
                                      inputs "/lib/qt5/libexec/QtWebEngineProcess")))
               (wrap-program bin
                 `("QTWEBENGINEPROCESS_PATH" = (,qt-process-path)))))))))
    (inputs
     (list bash-minimal
           curl
           icu4c
           kiwix-lib
           libmicrohttpd
           libzim
           pugixml
           qtbase-5
           qtdeclarative-5
           qtwebchannel-5
           qtwebengine-5
           qtwayland-5
           xapian
           zlib
           `(,zstd "lib")))
    (native-inputs
     (list pkg-config
           qtbase-5))
    (home-page "https://wiki.kiwix.org/wiki/Software")
    (synopsis "Viewer and manager of ZIM files")
    (description "Kiwix Desktop allows you to enjoy a lot of different content
offline (such as Wikipedia), without any access to Internet.")
    (license license:gpl3)))

(define-public kiwix-tools
  (package
    (name "kiwix-tools")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.kiwix.org/release/"
                                  "kiwix-tools/kiwix-tools-" version ".tar.xz"))
              (sha256
               (base32
                "0q6b7viy1jr212q0glqid2hqxnsd2mxsx5gzcalkc4gb0bzgj32d"))))
    (build-system meson-build-system)
    (inputs
     (list curl
           icu4c
           kiwix-lib
           libmicrohttpd
           libzim
           pugixml
           xapian
           zlib
           `(,zstd "lib")))
    (native-inputs
     (list pkg-config))
    (home-page "https://wiki.kiwix.org/wiki/Software")
    (synopsis "Kiwix command line tools")
    (description "The Kiwix tools are a collection of Kiwix-related command line
tools:
@itemize
@item kiwix-manage: Manage XML based library of ZIM files
@item kiwix-read: Read ZIM file content
@item kiwix-search: Fulltext search in ZIM files
@item kiwix-serve: HTTP daemon serving ZIM files
@end itemize\n")
    (license license:gpl3+)))

(define-public uriparser
  (package
    (name "uriparser")
    (version "0.9.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/uriparser/uriparser")
                    (commit (string-append "uriparser-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qr3rc0iz1zxim1ylwzf7zijgnxpzv4m35fzvv5kf66a8bqhrw2k"))))
    (build-system cmake-build-system)
    (native-inputs (list googletest doxygen graphviz))
    (arguments (if (%current-target-system)
                   (list #:configure-flags #~(list "-DURIPARSER_BUILD_TESTS=OFF"))
                   '()))
    (synopsis "Strictly RFC 3986 compliant URI parsing and handling library")
    (description "uriparser is a strictly RFC 3986 compliant URI parsing and
handling library written in C89 (\"ANSI C\").  uriparser is fast and supports
Unicode.")
    (home-page "https://uriparser.github.io/")
    (license license:bsd-3)))

(define-public quark
  ;; No releases yet
  (let ((revision "0")
        (commit "c6a9055e5a30be570e30da8d216c39662c3a3f99"))
    (package
      (name "quark")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.suckless.org/quark/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1znvnr30xi5vgd6n3wvgv9pwj992zpzzjk0fmq28ydf1l6kqvkm7"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f ; no tests
             #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target))
                     (string-append "PREFIX=" (assoc-ref %outputs "out")))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)))) ; no configure script
      (home-page "https://tools.suckless.org/quark/")
      (synopsis "Small and simple HTTP GET/HEAD-only web server for static
content")
      (description "Quark is an extremely small and simple HTTP GET/HEAD only
web server for static content.  TLS is not natively supported and should be
provided by a TLS reverse proxy (e.g. tlstunnel, hitch or stunnel).")
      (license license:isc)

      ;; XXX: Ignore this CVE to work around a name clash with the unrelated
      ;; "cpe:2.3:a:comelz:quark" package.  The proper fix is for (guix cve)
      ;; to account for "vendor names".
      (properties '((lint-hidden-cve . ("CVE-2019-15520")))))))

(define-public go-webring
  (let ((commit "6786a27b0c57df75323217453369c83a4d9f4dea")
        (revision "0"))
    (package
      (name "go-webring")
      (version (git-version "20220426" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~amolith/go-webring")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xra0mapdmda8n0z6zsgcqppdzvxc47p0fahsdyig5qmpk89ar8l"))))
      (build-system go-build-system)
      (arguments
       (list #:import-path "git.sr.ht/~amolith/go-webring"
             #:install-source? #f))
      (inputs (list go-github-com-spf13-pflag))
      (home-page "https://git.sr.ht/~amolith/go-webring")
      (synopsis "Simple webring implementation")
      (description
"@code{go--webring} provides a simple webring implementation as used by
the Fediring.")
      (license (list license:cc0 license:bsd-2)))))

(define-public archivebox
  (package
    (name "archivebox")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri name version))
              (sha256
               (base32
                "1mnq82ynq01l7vx957bbx4bvgwdh59qsnx6pdydaqszbakp74yyc"))))
    (build-system python-build-system)
    (propagated-inputs
     (list curl
           node-lts))
    (inputs
     (list python
           youtube-dl
           wget
           git
           python-w3lib
           python-ipython
           python-croniter
           python-crontab
           python-dateparser
           python-django-extensions
           python-django-3.1.14
           python-mypy-extensions))
    (native-inputs
     (list python-wheel))
    (synopsis "Self-hosted Web archiving")
    (description "ArchiveBox is a powerful, self-hosted Web archiving
solution to collect, save, and view sites you want to preserve offline.
You can feed it URLs one at a time, or schedule regular imports.  It saves
snapshots of the URLs you feed it in several formats.")
    (home-page "https://archivebox.io/")
    (license license:expat)))

(define-public awslogs
  (package
    (name "awslogs")
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "awslogs" version))
              (sha256
               (base32
                "0zpp72ixxz18mf1kay7l07sbmf80mik30zw6p4wsxpraza3ry90v"))))
    ;; XXX: doesn't work with pyproject-build-system
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               (("'jmespath>=0.7.1.*',")
                "'jmespath>=0.7.1',"))))
         (add-after 'unpack 'patch-tests
           (lambda _
             ;; XXX These tests fail for unknown reasons, and we can't easily
             ;; figure out why, because stdout is redirected to a string.
             (substitute* "tests/test_it.py"
               (("test_main_get_with_color")
                "_skip_test_main_get_with_color")
               (("test_main_get_query")
                "_skip_test_main_get_query")))))))
    (propagated-inputs
     (list python-boto3 python-jmespath python-dateutil python-termcolor))
    (home-page "https://github.com/jorgebastida/awslogs")
    (synopsis "Command line tool to read AWS CloudWatch logs")
    (description
     "This package provides awslogs, a simple command line tool to download
and read AWS CloudWatch logs.")
    (license license:bsd-3)))

(define-public orcania
  (package
    (name "orcania")
    (version "2.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/babelouest/orcania")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dhczbhwvf3f9mj38qm46j10rpr77yz1np68mabfw8zcfwhr0pn4"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_ORCANIA_TESTING=ON"
                                     "-DBUILD_ORCANIA_DOCUMENTATION=ON")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'build 'build-doc
                          (lambda _
                            (invoke "make" "doc")))
                        (add-after 'install 'install-doc
                          (lambda _
                            (let ((doc (string-append #$output
                                                      "/share/doc/orcania")))
                              (mkdir-p doc)
                              (copy-recursively "../source/doc/html" doc)))))))
    (native-inputs (list check doxygen subunit))
    (home-page "https://babelouest.github.io/orcania/")
    (synopsis "Collection of C functions for Ulfius")
    (description
     "Orcania is a library with functions that can be shared among C programs.
It is intended to provide low-level functionalities for the Ulfius and Yder
libraries.")
    (license license:lgpl2.1)))

(define-public yder
  (package
    (name "yder")
    (version "1.4.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/babelouest/yder")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02jgrqby39ykfdhc7z0bh3x5aqisqybz6lnvn7msh9wqbj5zvzi8"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DWITH_JOURNALD=OFF"
                                     "-DBUILD_YDER_TESTING=ON"
                                     "-DBUILD_YDER_DOCUMENTATION=ON")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'build 'build-doc
                          (lambda _
                            (invoke "make" "doc")))
                        (add-after 'install 'install-doc
                          (lambda _
                            (let ((doc (string-append #$output
                                                      "/share/doc/yder")))
                              (mkdir-p doc)
                              (copy-recursively "../source/doc/html" doc)))))))
    (native-inputs (list check doxygen subunit))
    (inputs (list orcania))
    (home-page "https://babelouest.github.io/yder/")
    (synopsis "Logging library for C applications")
    (description
     "Yder is a logging library written in C.  It can log messages to the
console, a file, syslog, journald, or a callback function.")
    (license license:lgpl2.1)))

(define-public ulfius
  (package
    (name "ulfius")
    (version "2.7.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/babelouest/ulfius")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dfwdpqmqki63dddi53bfv6jd0kzv8gh2w1lxsv6mzk3sxl6qakf"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_ULFIUS_TESTING=ON"
                                     "-DBUILD_ULFIUS_DOCUMENTATION=ON")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'build 'build-doc
                          (lambda _
                            (invoke "make" "doc")))
                        (replace 'check
                          (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
                            (when tests?
                              (let ((job-count (number->string
                                                (or (and parallel-tests?
                                                         (parallel-job-count))
                                                    1))))
                                ;; Skip failing tests that try to start a server.
                                (invoke "ctest" "--output-on-failure"
                                        "-j" job-count "-E"
                                        "(core|framework|example_callbacks)")))))
                        (add-after 'install 'install-doc
                          (lambda _
                            (let ((doc (string-append #$output
                                                      "/share/doc/ulfius")))
                              (mkdir-p doc)
                              (copy-recursively "../source/doc/html" doc)))))))
    (native-inputs (list check doxygen subunit))
    (inputs (list zlib))
    (propagated-inputs ;for libulfius.pc
     (list curl
           gnutls
           jansson
           libgcrypt
           libmicrohttpd
           orcania
           yder))
    (home-page "https://babelouest.github.io/ulfius/")
    (synopsis "HTTP Framework for REST Applications in C")
    (description
     "Ulfius is a HTTP Framework library for REST Applications written in C.
It is based on GNU libmicrohttpd for the backend web server, Jansson for the
JSON manipulation library, and libcurl for the http/smtp client API.  It can
be used to facilitate creation of web applications in C programs with a small
memory footprint, as in embedded systems applications.  It can create
webservices in HTTP or HTTPS mode, stream data, or implement server
websockets.")
    (license license:lgpl2.1)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
