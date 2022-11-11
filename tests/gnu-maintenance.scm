;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
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

(define-module (test-gnu-maintenance)
  #:use-module (guix gnu-maintenance)
  #:use-module (guix tests)
  #:use-module (guix tests http)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "gnu-maintenance")

(test-assert "release-file?"
  (and (every (lambda (project+file)
                (apply release-file? project+file))
              '(("gcc" "gcc-5.3.0.tar.bz2")
                ("texmacs" "TeXmacs-1.0.7.9-src.tar.gz")
                ("icecat" "icecat-38.4.0-gnu1.tar.bz2")
                ("mit-scheme" "mit-scheme-9.2.tar.gz")
                ("mediainfo" "mediainfo_20.09.tar.xz")
                ("exiv2" "exiv2-0.27.3-Source.tar.gz")
                ("mpg321" "mpg321_0.3.2.orig.tar.gz")
                ("bvi" "bvi-1.4.1.src.tar.gz")
                ("hostscope" "hostscope-V2.1.tgz")))
       (every (lambda (project+file)
                (not (apply release-file? project+file)))
              '(("guile" "guile-www-1.1.1.tar.gz")
                ("guile" "guile-2.0.11.tar.gz.sig")
                ("mit-scheme" "mit-scheme-9.2-i386.tar.gz")
                ("mit-scheme" "mit-scheme-9.2-doc-pdf.tar.gz")
                ("gnutls" "gnutls-3.2.18-w32.zip")
                ("valgrind" "valgrind-3.20.0.RC1.tar.bz2")))))

(test-assert "tarball->version"
  (let ((tarball->version (@@ (guix gnu-maintenance) tarball->version)))
    (every (match-lambda
             ((file version)
              (equal? (tarball->version file) version)))
           '(("coreutils-8.32.tar.gz" "8.32")
             ("mediainfo_20.09.tar.xz" "20.09")
             ("exiv2-0.27.3-Source.tar.gz" "0.27.3")
             ("mpg321_0.3.2.orig.tar.gz" "0.3.2")
             ("bvi-1.4.1.src.tar.gz" "1.4.1")))))

(test-assert "latest-html-release, scheme-less URIs"
  (with-http-server
      `((200 "<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>Releases (on another domain)!</title>
</head>
<body
<a href=\"//another-site/foo-2.tar.gz\">version 1</a>
</body>
</html>"))
    (let ()
      (define package
        (dummy-package "foo"
          (source
           (dummy-origin
            (uri (string-append (%local-url) "/foo-1.tar.gz"))))
          (properties
           `((release-monitoring-url . ,(%local-url))))))
      (define update ((upstream-updater-latest %generic-html-updater) package))
      (define expected-new-url "http://another-site/foo-2.tar.gz")
      (and (pk 'u update)
           (equal? (upstream-source-version update) "2")
           (equal? (list expected-new-url) (upstream-source-urls update))))))

(test-assert "latest-html-release, no signature"
  (with-http-server
      `((200 "<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>Releases!</title>
</head>
<body>
<a href=\"bar/foo-1.tar.gz\">version 1</a>
<a href=\"bar/foo-2.tar.gz\">version 2</a>
</body>
</html>"))
    (let ()
      (define package
        (dummy-package "foo"
          (source
           (dummy-origin
            (uri (string-append (%local-url) "/foo-1.tar.gz"))))
          (properties
           `((release-monitoring-url . ,(%local-url))))))
      (define update ((upstream-updater-latest %generic-html-updater) package))
      (define expected-new-url
        (string-append (%local-url) "/foo-2.tar.gz"))
      (and (pk 'u update)
           (equal? (upstream-source-version update) "2")
           (equal? (list expected-new-url)
                   (upstream-source-urls update))
           (null? ;; both #false and the empty list are acceptable
            (or (upstream-source-signature-urls update) '()))))))

(test-assert "latest-html-release, signature"
  (with-http-server
      `((200 "<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>Signed releases!</title>
</head>
<body>
<a href=\"bar/foo-1.tar.gz\">version 1</a>
<a href=\"bar/foo-2.tar.gz\">version 2</a>
<a href=\"bar/foo-1.tar.gz.sig\">version 1 signature</a>
<a href=\"bar/foo-2.tar.gz.sig\">version 2 signature</a>
</body>
</html>"))
    (let ()
      (define package
        (dummy-package "foo"
          (source
           (dummy-origin
            (uri (string-append (%local-url) "/foo-1.tar.gz"))))
          (properties
           `((release-monitoring-url . ,(%local-url))))))
      (define update ((upstream-updater-latest %generic-html-updater) package))
      (define expected-new-url
        (string-append (%local-url) "/foo-2.tar.gz"))
      (define expected-signature-url
        (string-append (%local-url) "/foo-2.tar.gz.sig"))
      (and (pk 'u update)
           (equal? (upstream-source-version update) "2")
           (equal? (list expected-new-url)
                   (upstream-source-urls update))
           (equal? (list expected-signature-url)
                   (upstream-source-signature-urls update))))))

(test-end)
