;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
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

(define-module (gnu packages php)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public php
  (package
    (name "php")
    (version "8.3.10")
    (home-page "https://www.php.net/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "distributions/"
                                  "php-" version ".tar.xz"))
              (sha256
               (base32
                "13cpl7983wss2462cr75kzcs7349ix1c7jqj39iyf7wk02figwm0"))
              (modules '((guix build utils)))
              (snippet
               '(with-directory-excursion "ext"
                  (for-each delete-file-recursively
                            ;; Some of the bundled libraries have no proper upstream.
                            ;; Ideally we'd extract these out as separate packages:
                            ;;"mbstring/libmbfl"
                            ;;"date/lib"
                            ;;"bcmath/libbcmath"
                            ;;"fileinfo/libmagic" ; a patched version of libmagic
                            '("gd/libgd"
                              "pcre/pcre2lib"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let-syntax ((with (syntax-rules ()
                            ((_ option input)
                             (string-append option "="
                                            (assoc-ref %build-inputs input))))))
         (list (with "--with-bz2" "bzip2")
               (with "--with-curl" "curl")
               (with "--with-gdbm" "gdbm")
               (with "--with-gettext" "libc")  ; libintl.h
               (with "--with-gmp" "gmp")
               (with "--with-ldap" "openldap")
               (with "--with-ldap-sasl" "cyrus-sasl")
               (with "--with-pdo-pgsql" "postgresql")
               (with "--with-pdo-sqlite" "sqlite")
               (with "--with-pgsql" "postgresql")
               ;; PHP’s Pspell extension, while retaining its current name,
               ;; now uses the Aspell library.
               (with "--with-pspell" "aspell")
               (with "--with-readline" "readline")
               (with "--with-sodium" "libsodium")
               (with "--with-sqlite3" "sqlite")
               (with "--with-tidy" "tidy")
               (with "--with-xsl" "libxslt")
               (with "--with-zlib-dir" "zlib")
               ;; We could add "--with-snmp", but it requires netsnmp that
               ;; we don't have a package for. It is used to build the snmp
               ;; extension of php.
               "--with-external-pcre"
               "--with-external-gd"
               "--with-iconv"
               "--with-openssl"
               "--with-mysqli"          ; Required for, e.g. wordpress
               "--with-pdo-mysql"
               "--with-zip"
               "--with-zlib"
               "--enable-bcmath"        ; Required for, e.g. Zabbix frontend
               "--enable-calendar"
               "--enable-dba=shared"
               "--enable-exif"
               "--enable-flatfile"
               "--enable-fpm"
               "--enable-ftp"
               "--enable-gd"
               "--enable-inifile"
               "--enable-intl"
               "--enable-mbstring"
               "--enable-pcntl"
               "--enable-sockets"
               "--enable-sysvsem"))     ; Required for, e.g. Nextcloud
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-record-build-flags
           (lambda _
             ;; Prevent configure flags from being stored and causing
             ;; unnecessary runtime dependencies.
             (substitute* "scripts/php-config.in"
               (("@CONFIGURE_OPTIONS@") "")
               (("@PHP_LDFLAGS@") ""))
             ;; This file has ISO-8859-1 encoding.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "main/build-defs.h.in"
                 (("@CONFIGURE_COMMAND@") "(omitted)")))))
         (add-before 'build 'patch-/bin/sh
           (lambda _
             (substitute* '("run-tests.php" "ext/standard/proc_open.c")
               (("/bin/sh") (which "sh")))))
         (add-before 'check 'prepare-tests
           (lambda _
             ;; Some of these files have ISO-8859-1 encoding, whereas others
             ;; use ASCII, so we can't use a "catch-all" find-files here.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* '("ext/mbstring/tests/mb_send_mail02.phpt"
                              "ext/mbstring/tests/mb_send_mail04.phpt"
                              "ext/mbstring/tests/mb_send_mail05.phpt"
                              "ext/mbstring/tests/mb_send_mail06.phpt")
                 (("/bin/cat") (which "cat"))))
             (substitute* '("ext/mbstring/tests/mb_send_mail01.phpt"
                            "ext/mbstring/tests/mb_send_mail03.phpt"
                            "ext/mbstring/tests/bug52681.phpt"
                            "ext/standard/tests/general_functions/bug34794.phpt"
                            "ext/standard/tests/general_functions/bug44667.phpt"
                            "ext/standard/tests/general_functions/proc_open.phpt")
               (("/bin/cat") (which "cat")))

             ;; The encoding of this file is not recognized, so we simply drop it.
             (delete-file "ext/mbstring/tests/mb_send_mail07.phpt")

             (substitute* "ext/standard/tests/streams/bug60602.phpt"
               (("'ls'") (string-append "'" (which "ls") "'")))

             ;; Drop tests known to fail on different architectures:
             (for-each delete-file
             ,(cond
                ((target-arm32?)
                 `(list "ext/calendar/tests/unixtojd_error1.phpt"
                        "ext/opcache/tests/preload_006.phpt"
                        "ext/opcache/tests/preload_011.phpt"
                        ;; arm can be a lot slower, so a time-related test fails
                        "ext/fileinfo/tests/cve-2014-3538-nojit.phpt"
                        "ext/pcntl/tests/pcntl_unshare_01.phpt"
                        "ext/pcre/tests/bug76514.phpt"
                        "ext/pcre/tests/preg_match_error3.phpt"
                        "ext/pcre/tests/cache_limit.phpt"
                        "ext/sockets/tests/socket_getopt.phpt"
                        "ext/sockets/tests/socket_sendrecvmsg_error.phpt"
                        "ext/standard/tests/general_functions/var_export-locale.phpt"
                        "ext/standard/tests/general_functions/var_export_basic1.phpt"
                        "ext/intl/tests/timezone_getErrorCodeMessage_basic.phpt"
                        "ext/intl/tests/timezone_getOffset_error.phpt"
                        "sapi/cli/tests/cli_process_title_unix.phpt"
                        "Zend/tests/concat_003.phpt"))
                ((target-x86-32?)
                 `(list "ext/dba/tests/dba_gdbm.phpt"))
                ((target-ppc32?)
                 `(list "sapi/phpdbg/tests/watch_001.phpt"
                        "sapi/phpdbg/tests/watch_003.phpt"
                        "sapi/phpdbg/tests/watch_004.phpt"))
                ((target-ppc64le?)
                 `(list
                    ;; phpdbg watchpoints don't work.
                    ;; Bug tracked upstream at:
                    ;; https://bugs.php.net/bug.php?id=81408
                    "sapi/phpdbg/tests/watch_001.phpt"
                    "sapi/phpdbg/tests/watch_003.phpt"
                    "sapi/phpdbg/tests/watch_004.phpt"
                    "sapi/phpdbg/tests/watch_005.phpt"
                    "sapi/phpdbg/tests/watch_006.phpt"))
                (else `'())))

             ;; Drop tests that are known to fail.
             (for-each delete-file
                       '("ext/posix/tests/posix_getgrgid.phpt"    ; Requires /etc/group.
                         "ext/posix/tests/posix_getgrnam_basic.phpt" ; Requires /etc/group.
                         "ext/sockets/tests/bug63000.phpt"        ; Fails to detect OS.
                         ;; These need exotic locales.
                         "ext/standard/tests/strings/setlocale_basic1.phpt"
                         "ext/standard/tests/strings/setlocale_basic2.phpt"
                         "ext/standard/tests/strings/setlocale_basic3.phpt"
                         "ext/standard/tests/strings/setlocale_variation1.phpt"
                         ;; This bug should have been fixed in gd 2.2.2.
                         ;; Is it a regression?
                         "ext/gd/tests/bug65148.phpt"
                         ;; This bug should have been fixed in the gd 2.2
                         ;; series.  Perhaps a regression introduced by gd
                         ;; 2.3.0?
                         "ext/gd/tests/bug66590.phpt"
                         ;; This bug should have been fixed in the php-5.5
                         ;; series.  Perhaps a regression introduced by gd
                         ;; 2.3.0?
                         "ext/gd/tests/bug70102.phpt"
                         ;; This bug should have been fixed in the php-5.6
                         ;; series.  Perhaps a regression introduced by gd
                         ;; 2.3.0?
                         "ext/gd/tests/bug73869.phpt"
                         ;; Some WebP related tests fail.
                         "ext/gd/tests/webp_basic.phpt"
                         "ext/gd/tests/imagecreatefromstring_webp.phpt"
                         ;; TODO: Enable these when libgd is built with xpm support.
                         "ext/gd/tests/xpm2gd.phpt"
                         "ext/gd/tests/xpm2jpg.phpt"
                         "ext/gd/tests/xpm2png.phpt"
                         ;; AVIF support disabled
                         "ext/gd/tests/avif_decode_encode.phpt"
                         ;; Typo in expected outputs
                         "ext/gd/tests/bug72339.phpt"
                         ;; AVIF support disabled
                         "ext/gd/tests/imagecreatefromstring_avif.phpt"

                         ;; XXX: These test failures appear legitimate, needs investigation.
                         ;; open_basedir() restriction failure.
                         "ext/curl/tests/curl_setopt_ssl.phpt"

                         ;; Fail because there is no "root" in the build container's
                         ;; /etc/passwd
                         "sapi/fpm/tests/bug68591-conf-test-group.phpt"
                         "sapi/fpm/tests/bug68591-conf-test-listen-group.phpt"
                         "sapi/fpm/tests/bug68591-conf-test-listen-owner.phpt"

                         ;; The test expects an Array, but instead get the contents(?).
                         "ext/gd/tests/bug43073.phpt"
                         ;; imagettftext() returns wrong coordinates.
                         "ext/gd/tests/bug48732-mb.phpt"
                         "ext/gd/tests/bug48732.phpt"
                         ;; Similarly for imageftbbox().
                         "ext/gd/tests/bug48801-mb.phpt"
                         "ext/gd/tests/bug48801.phpt"
                         ;; Different expected output from imagecolorallocate().
                         "ext/gd/tests/bug53504.phpt"
                         ;; Wrong image size after scaling an image.
                         "ext/gd/tests/bug73272.phpt"
                         ;; PCRE with/without JIT gives different result
                         "ext/pcre/tests/gh11374.phpt"
                         "ext/pcre/tests/gh11956.phpt"
                         ;; reported bug only seems to affect windows
                         "ext/standard/tests/directory/bug74589_utf8.phpt"
                         ;; this test seems to be unreliable/flaky
                         "sapi/cli/tests/php_cli_server_pdeathsig.phpt"
                         ;; This test fails on most architectures.
                         "sapi/cli/tests/upload_2G.phpt"))

             ;; Accomodate two extra openssl errors flanking the expected one:
             ;; random number generator:RAND_{load,write}_file:Cannot open file
             ;; This is due to an invalid $HOME, but changing it in the test
             ;; still prints the first one & changing it globally is overkill.
             (substitute* "ext/openssl/tests/bug80747.phpt"
               ((".*error:%s:key size too small.*" match)
                (string-append "%s\n" match "%s\n")))

             ;; Skip tests requiring network access.
             (setenv "SKIP_ONLINE_TESTS" "1")
             ;; Without this variable, 'make test' passes regardless of failures.
             (setenv "REPORT_EXIT_STATUS" "1")
             ;; Skip tests requiring I/O facilities that are unavailable in the
             ;; build environment
             (setenv "SKIP_IO_CAPTURE_TESTS" "1"))))
       #:test-target "test"))
    (inputs
     `(("aspell" ,aspell)
       ("bzip2" ,bzip2)
       ("curl" ,curl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("gd" ,gd)
       ("gdbm" ,gdbm)
       ("gmp" ,gmp)
       ("gnutls" ,gnutls)
       ("icu4c" ,icu4c)
       ("libgcrypt" ,libgcrypt)
       ("libpng" ,libpng)
       ("libsodium" ,libsodium)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libx11" ,libx11)
       ("libzip" ,libzip)
       ("oniguruma" ,oniguruma)
       ("openldap" ,openldap)
       ("openssl" ,openssl)
       ("pcre" ,pcre2)
       ("postgresql" ,postgresql)
       ("readline" ,readline)
       ("sqlite" ,sqlite)
       ("tidy" ,tidy-html)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("bison" ,bison)
       ("gettext" ,gettext-minimal)
       ("procps" ,procps)))             ; for tests
    (synopsis "PHP programming language")
    (description
     "PHP (PHP Hypertext Processor) is a server-side (CGI) scripting
language designed primarily for web development but is also used as
a general-purpose programming language.  PHP code may be embedded into
HTML code, or it can be used in combination with various web template
systems, web content management systems and web frameworks." )
    (license (list
              (license:non-copyleft "file://LICENSE")       ; The PHP license.
              (license:non-copyleft "file://Zend/LICENSE")  ; The Zend license.
              license:lgpl2.1                               ; ext/mbstring/libmbfl
              license:lgpl2.1+                              ; ext/bcmath/libbcmath
              license:bsd-2                                 ; ext/fileinfo/libmagic
              license:expat))))                             ; ext/date/lib
