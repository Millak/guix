;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019,2026 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2024 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (guix import apache)
  #:use-module ((guix import utils) #:select (find-version))
  #:use-module (guix http-client)
  #:use-module (guix gnu-maintenance)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (web uri)

  #:export (%apache-updater))

;;; Commentary:
;;;
;;; This package provides not an actual importer but simply an updater for
;;; ASF packages.  It grabs available files from the 'find-ls2.txt.gz' file
;;; available on dlcdn.apache.org.
;;;
;;; Code:

(define (tarball->version tarball)
  "Return the version TARBALL corresponds to.  TARBALL is a file name like
\"coreutils-8.23.tar.xz\"."
  (let-values (((name version)
                (gnu-package-name->name+version
                 (tarball-sans-extension tarball))))
    version))

(define %apache-file-list-uri
  ;; URI of the file list (ls -lR format) for dlcdn.apache.org.
  (string->uri "https://dlcdn.apache.org/zzz/find-ls2.txt.gz"))

(define (dlcdn.apache.org-files)
  ;;"Return the list of files available at dlcdn.apache.org."

  (define (find-ls2-line->filename line)
    ;; Remove mode, blocks, user, group, size, date, time and timezone
    (substring line 67))

  (define (write-cache input cache)
    "Read gzipped find-ls2 from INPUT, and write it as a list of file paths to
CACHE."
    (call-with-decompressed-port 'gzip input
      (lambda (input)
        (let loop_entries ((files '()))
          (let ((line (read-line input)))
            (if (eof-object? line)
                (write (reverse files) cache)
                (let ((line (find-ls2-line->filename line)))
                  (if (string-prefix? "/spark/docs/" line) ;; skip ca. 457.000 lines
                      (loop_entries files)
                      (loop_entries (cons line files))))))))))

  (define (cache-miss uri)
    (format (current-error-port) "fetching ~a~%This will take some time since the file has more then 500.000 lines...~%" (uri->string uri)))

  (let* ((port (http-fetch/cached %apache-file-list-uri
                                  #:ttl 3600
                                  #:write-cache write-cache
                                  #:cache-miss cache-miss))
         (files (read port)))
    (close-port port)
    files))

(define (uri->apache-path-pattern uri)
  "Build a regexp from the package's URI suitable for matching the package
path version-agnostic.

Example:
Input:
   mirror://apache/accumulo/2.1.4/accumulo-2.1.4-src.tar.gz
Output:
   /accumulo/[^/]+/
"

  (define version-regexp
    ;; regexp for matching versions as used in the find-ls2 file
    (make-regexp
     (string-join '("^v?([0-9]+\\.)+[0-9]+-?"  ;; 5.12.90, 4.2.0-incubating, v0.2.0
                    "^v[0-9]+$"   ;            ;; v2
                    "^[0-9]-[0-9a-f]+"         ;; 4-8e2bc77e581e
                    ".*-([0-9]+\\.)+[0-9]+$")  ;; ambari-2.7.8, …-solr-7.x-plugin-2.2.1
                    "|")))

  (define (version->pattern part)
    ;; If a path element might be a version, replace it by a catch-all part
    (if (regexp-exec version-regexp part)
        "[^/]+"
        part))

  (let* ((path (uri-path uri))
         (directory-parts (string-split (dirname path) #\/)))
    (make-regexp
     (string-append
      (string-join (map version->pattern directory-parts) "/")
      "/"))))

(define* (import-apache-release package #:key version partial-version?)
  "Return the latest release of PACKAGE, a APACHE package, or #f if it could
not be determined. Optionally include a VERSION string to fetch a specific
version, which may be a partial prefix when PARTIAL-VERSION? is #t."
  (let* ((uri      (string->uri (origin-uri (package-source package))))
         (path-rx  (uri->apache-path-pattern uri))
         (name     (package-upstream-name* package))
         (files    (dlcdn.apache.org-files))
         ;; Select archives for this package.
         (relevant (filter (lambda (file)
                             (and (regexp-exec path-rx file)
                                  (release-file? name (basename file))))
                           files))
         ;; Build an association list of file names keyed by their version.
         (all-tarballs (map (lambda (x)
                              (cons (tarball->version x) x))
                            relevant))
         (versions (map car all-tarballs))
         ;; Find the latest version.
         (version (find-version versions version partial-version?))
         ;; Find all archives matching this version.
         (tarballs (and version
                        (map cdr (filter (match-lambda
                                           ((x . file-name)
                                            (string=? version x)))
                                         all-tarballs)))))
    (and version tarballs
         (upstream-source
          (package name)
          (version version)
          (urls (map (lambda (file)
                       (string-append "mirror://apache" file))
                     tarballs))))))

(define %apache-updater
  (upstream-updater
    (name 'apache)
    (description "Updater for ASF/Apache packages")
    (pred (url-prefix-predicate "mirror://apache/"))
    (import import-apache-release)))
