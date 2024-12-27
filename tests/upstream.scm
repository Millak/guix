;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2023-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-upstream)
  #:use-module (gnu packages base)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix import print)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix upstream)
  #:use-module (guix tests)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))


(test-begin "upstream")

(test-equal "coalesce-sources same version"
  '((source "foo" "1"
            ("ftp://example.org/foo-1.tar.xz"
             "ftp://example.org/foo-1.tar.gz")
            ("ftp://example.org/foo-1.tar.xz.sig"
             "ftp://example.org/foo-1.tar.gz.sig")))

  (map (lambda (source)
         `(source ,(upstream-source-package source)
                  ,(upstream-source-version source)
                  ,(upstream-source-urls source)
                  ,(upstream-source-signature-urls source)))
       (coalesce-sources (list (upstream-source
                                (package "foo") (version "1")
                                (urls '("ftp://example.org/foo-1.tar.gz"))
                                (signature-urls
                                 '("ftp://example.org/foo-1.tar.gz.sig")))
                               (upstream-source
                                (package "foo") (version "1")
                                (urls '("ftp://example.org/foo-1.tar.xz"))
                                (signature-urls
                                 '("ftp://example.org/foo-1.tar.xz.sig")))))))

(test-equal "preferred-upstream-source"
  '(("http://example.org/foo-2.0.tar.xz")
    ("http://example.org/foo-2.0.tar.xz.sig"))
  (let* ((package (dummy-package
                   "foo"
                   (version "1.0")
                   (source
                    (dummy-origin (uri "http://example.org/foo-1.0.tar.xz")))))
         (source (upstream-source
                  (package "foo")
                  (version "2.0")
                  (urls '("http://example.org/foo-2.0.tar.gz"
                          "http://example.org/foo-2.0.tar.xz"))
                  (signature-urls (map (cut string-append <> ".sig") urls))))
         (preferred (preferred-upstream-source source package)))
    (list (upstream-source-urls preferred)
          (upstream-source-signature-urls preferred))))

(test-end)
