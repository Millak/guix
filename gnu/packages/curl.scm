;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015, 2020, 2021, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Dale Mellor <guix-devel-0brg6b@rdmp.org>
;;; Copyright © 2020, 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (gnu packages curl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module ((guix search-paths) #:select ($SSL_CERT_DIR $SSL_CERT_FILE))
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (srfi srfi-1))

(define-public curl
  (package
    (name "curl")
    (version "8.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://curl.se/download/curl-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "05fv468yjrb7qwrxmfprxkrcckbkij0myql0vwwnalgr3bcmbk9w"))
              (patches (search-patches "curl-use-ssl-cert-env.patch"))))
    (outputs '("out"
               "doc"))                  ;1.2 MiB of man3 pages
    (build-system gnu-build-system)
    (arguments
     (list
      #:disallowed-references '("doc")
      #:configure-flags
      #~(list "--with-gnutls"
              (string-append "--with-gssapi="
                             (dirname (dirname
                                       (search-input-file
                                        %build-inputs "lib/libgssrpc.so"))))
              "--disable-static")
      #:test-target "test-nonflaky"     ;avoid tests marked as "flaky"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-record-configure-flags
            (lambda _
              ;; Do not save the configure options to avoid unnecessary references.
              (substitute* "curl-config.in"
                (("@CONFIGURE_OPTIONS@")
                 "\"not available\""))))
          (add-after 'install 'move-man3-pages
            (lambda _
              ;; Move section 3 man pages to "doc".
              (mkdir-p (string-append #$output:doc "/share/man"))
              (rename-file (string-append #$output "/share/man/man3")
                           (string-append #$output:doc "/share/man/man3"))))
          (replace 'check
            (lambda* (#:key tests? parallel-tests? make-flags #:allow-other-keys)
              (substitute* "tests/runtests.pl"
                (("/bin/sh") (which "sh")))
              (when tests?
                (let* ((job-count (string-append
                                   "-j"
                                   (if parallel-tests?
                                       (number->string (parallel-job-count))
                                       "1")))
                       ;; Ignore test 1477 due to a missing file in the 8.5.0
                       ;; release.  See
                       ;; <https://github.com/curl/curl/issues/12462>.
                       (arguments `("-C" "tests" "test"
                                    ,@make-flags
                                    ,(if #$(or (system-hurd?)
                                               (target-arm32?)
                                               (target-aarch64?))
                                         ;; protocol FAIL
                                         (string-append "TFLAGS=~1474 "
                                                        "!1477 "
                                                        job-count)
                                         (string-append "TFLAGS=\"~1477 "
                                                        job-count "\"")))))
                  ;; The top-level "make check" does "make -C tests quiet-test", which
                  ;; is too quiet.  Use the "test" target instead, which is more
                  ;; verbose.
                  (apply invoke "make" arguments)))))
          #$@(if (system-hurd?)
                 #~((add-after 'unpack 'skip-tests
                      (lambda _
                        (let ((port (open-file "tests/data/DISABLED" "a")))
                          (display "526\n" port)
                          (display "527\n" port)
                          (display "532\n" port)
                          (display "533\n" port)
                          (display "537\n" port)
                          (display "546\n" port)
                          (display "564\n" port)
                          (display "575\n" port)
                          (display "1021\n" port)
                          (display "1501\n" port)
                          (close port)))))
                 #~()))))
    (native-inputs
     (list nghttp2 perl pkg-config python-minimal-wrapper))
    (inputs
     (list gnutls libidn libpsl mit-krb5 `(,nghttp2 "lib") zlib))
    (native-search-paths
     ;; These variables are introduced by curl-use-ssl-cert-env.patch.
     (list $SSL_CERT_DIR
           $SSL_CERT_FILE
           ;; Note: This search path is respected by the `curl` command-line
           ;; tool only.  Patching libcurl to read it too would bring no
           ;; advantages and require maintaining a more complex patch.
           (search-path-specification
            (variable "CURL_CA_BUNDLE")
            (file-type 'regular)
            (separator #f)              ;single entry
            (files '("etc/ssl/certs/ca-certificates.crt")))))
    (synopsis "Command line tool for transferring data with URL syntax")
    (description
     "curl is a command line tool for transferring data with URL syntax,
supporting DICT, FILE, FTP, FTPS, Gopher, HTTP, HTTPS, IMAP, IMAPS, LDAP,
LDAPS, POP3, POP3S, RTMP, RTSP, SCP, SFTP, SMTP, SMTPS, Telnet and TFTP.
curl supports SSL certificates, HTTP POST, HTTP PUT, FTP uploading, HTTP
form based upload, proxies, cookies, file transfer resume, user+password
authentication (Basic, Digest, NTLM, Negotiate, kerberos...), proxy
tunneling, and so on.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))
    (home-page "https://curl.haxx.se/")))

(define-public gnurl (deprecated-package "gnurl" curl))

(define-public curl-ssh
  (package/inherit curl
    (arguments
     (substitute-keyword-arguments (package-arguments curl)
       ((#:configure-flags flags)
        #~(cons "--with-libssh2" #$flags))))
    (inputs
     (modify-inputs (package-inputs curl)
       (prepend libssh2)))
    (properties `((hidden? . #t)))))

(define-public kurly
  (package
    (name "kurly")
    (version "1.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.com/davidjpeacock/kurly.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "003jv2k45hg2svhjpy5253ccd250vi2r17x2zhm51iw54kgwxipm"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "gitlab.com/davidjpeacock/kurly"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-documentation
           (lambda* (#:key import-path outputs #:allow-other-keys)
             (let* ((source (string-append "src/" import-path))
                    (out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (man (string-append out "/share/man/man1")))
               (with-directory-excursion source
                 (install-file "README.md" doc)
                 (mkdir-p man)
                 (copy-file "doc/kurly.man"
                            (string-append man "/kurly.1")))
               #t))))))
    (inputs
     (list go-github-com-alsm-ioprogress go-github-com-aki237-nscjar
           go-github-com-urfave-cli))
    (synopsis "Command-line HTTP client")
    (description "kurly is an alternative to the @code{curl} program written in
Go.  kurly is designed to operate in a similar manner to curl, with select
features.  Notably, kurly is not aiming for feature parity, but common flags and
mechanisms particularly within the HTTP(S) realm are to be expected.  kurly does
not offer a replacement for libcurl.")
    (home-page "https://gitlab.com/davidjpeacock/kurly")
    (license license:asl2.0)))

(define-public guile-curl
  (package
   (name "guile-curl")
   (version "0.9")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.lonelycactus.com/tarball/"
                                "guile_curl-" version ".tar.gz"))
            (sha256
             (base32
              "0y7wfhilfm6vzs0wyifrrc2pj9nsxfas905c7qa5cw4i6s74ypmi"))))
   (build-system gnu-build-system)
   (arguments
    `(#:modules (((guix build guile-build-system)
                  #:select (target-guile-effective-version))
                 ,@%default-gnu-modules)
      #:imported-modules ((guix build guile-build-system)
                          ,@%default-gnu-imported-modules)
      #:configure-flags (list (string-append
                               "--with-guilesitedir="
                               (assoc-ref %outputs "out")
                               "/share/guile/site/"
                               (target-guile-effective-version
                                (assoc-ref %build-inputs "guile")))
                              (string-append
                               "-with-guileextensiondir="
                               (assoc-ref %outputs "out")
                               "/lib/guile/"
                               (target-guile-effective-version
                                (assoc-ref %build-inputs "guile"))
                               "/extensions"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-undefined-references
          (lambda* _
            (substitute* "module/curl.scm"
              ;; The following #defines are missing from our curl package
              ;; and therefore result in the evaluation of undefined symbols.
              ((",CURLOPT_HAPROXYPROTOCOL") "#f")
              ((",CURLOPT_DISALLOW_USERNAME_IN_URL") "#f")
              ((",CURLOPT_TIMEVALUE_LARGE") "#f")
              ((",CURLOPT_DNS_SHUFFLE_ADDRESSES") "#f")
              ((",CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS") "#f"))))
        (add-after 'install 'patch-extension-path
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out      (assoc-ref outputs "out"))
                   (curl.scm (string-append
                              out "/share/guile/site/"
                              (target-guile-effective-version)
                              "/curl.scm"))
                   (curl.go  (string-append
                              out "/lib/guile/"
                              (target-guile-effective-version)
                              "/site-ccache/curl.go"))
                   (ext      (string-append out "/lib/guile/"
                                            (target-guile-effective-version)
                                            "/extensions/libguile-curl")))
              (substitute* curl.scm (("libguile-curl") ext))
              ;; The build system does not actually compile the Scheme module.
              ;; So we can compile it and put it in the right place in one go.
              (invoke "guild" "compile" curl.scm "-o" curl.go)))))))
   (native-inputs (list pkg-config))
   (inputs
    (list curl guile-3.0))
   (home-page "http://www.lonelycactus.com/guile-curl.html")
   (synopsis "Curl bindings for Guile")
   (description "@code{guile-curl} is a project that has procedures that allow
Guile to do client-side URL transfers, like requesting documents from HTTP or
FTP servers.  It is based on the curl library.")
   (license license:gpl3+)))

(define-public guile2.2-curl
  (package
    (inherit guile-curl)
    (name "guile2.2-curl")
    (inputs
     (list curl guile-2.2))))

(define-public curlpp
  (package
    (name "curlpp")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jpbarrette/curlpp")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1b0ylnnrhdax4kwjq64r1fk0i24n5ss6zfzf4hxwgslny01xiwrk"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    ;; There are no build tests to be had.
    (arguments
     '(#:tests? #f))
    ;; The installed version needs the header files from the C library.
    (propagated-inputs
     (list curl))
    (synopsis "C++ wrapper around libcURL")
    (description
     "This package provides a free and easy-to-use client-side C++ URL
transfer library, supporting FTP, FTPS, HTTP, HTTPS, GOPHER, TELNET, DICT,
FILE and LDAP; in particular it supports HTTPS certificates, HTTP POST, HTTP
PUT, FTP uploading, kerberos, HTTP form based upload, proxies, cookies,
user+password authentication, file transfer resume, http proxy tunneling and
more!")
    (home-page "https://www.curlpp.org")
    (license license:expat)))

(define-public h2c
  (package
    (name "h2c")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/curl/h2c")
             (commit version)))
       (sha256
        (base32
         "1n8z6avzhg3yb330di2y9zymsps1qp1235p29kidcp4fkmn7fgb2"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("./h2c" "bin/"))))
    (inputs
     (list perl))
    (home-page "https://curl.se/h2c/")
    (synopsis "Convert HTTP headers to a curl command line")
    (description
     "Provided a set of HTTP request headers, h2c outputs how to invoke
curl to obtain exactly that HTTP request.")
    (license license:expat)))

(define-public coeurl
  (package
    (name "coeurl")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://nheko.im/nheko-reborn/coeurl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l9lx8chpc4bx7xxycdgr8mgqqjdrvipljmq869c2x1zjwrnasil"))))
    (build-system meson-build-system)
    (native-inputs
     (list doctest pkg-config))
    (inputs
     (list curl libevent spdlog))
    (home-page "https://nheko.im/nheko-reborn/coeurl")
    (synopsis "Simple async wrapper around CURL for C++")
    (description "Coeurl is a simple library to do HTTP requests
asynchronously via cURL in C++.")
    (license license:expat)))

(define-public curlie
  (package
    (name "curlie")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rs/curlie")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "04gwd9sqpykappnzyw9icgn5253cx1vwpr2h1fg7sgkyq3fjmsv0"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/rs/curlie"))
    (inputs (list curl
                  go-golang-org-x-crypto
                  go-golang-org-x-sys
                  go-golang-org-x-term))
    (home-page "https://curlie.io")
    (synopsis "The power of curl, the ease of use of httpie")
    (description "If you like the interface of HTTPie but miss the features of
curl, curlie is what you are searching for.  Curlie is a frontend to
@code{curl} that adds the ease of use of @code{httpie}, without compromising
on features and performance.  All @code{curl} options are exposed with syntax
sugar and output formatting inspired from @code{httpie}.")
    (license license:expat)))

(define-public trurl
  (package
    (name "trurl")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/curl/trurl")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10gsl0fdpybfcffmgf3qww7cpw3ifczl601042a2mqmwwrlx5zj7"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (native-inputs (list python))
    (inputs (list curl))
    (home-page "https://curl.se/trurl/")
    (synopsis "Command line tool for URL parsing and manipulation")
    (description "@code{trurl} is a command line tool that parses and
manipulates URLs, designed to help shell script authors everywhere.

It is similar in spirit to @code{tr}.  Here, @code{tr} stands for translate or
transpose.")
   (license (license:non-copyleft "file://COPYING"
                                  "See COPYING in the distribution."))))
