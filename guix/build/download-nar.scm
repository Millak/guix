;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020, 2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build download-nar)
  #:use-module (guix build download)
  #:use-module ((guix serialization) #:hide (dump-port*))
  #:autoload   (lzlib) (call-with-lzip-input-port)
  #:use-module (guix progress)
  #:use-module (web uri)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (download-nar))

;;; Commentary:
;;;
;;; Download a normalized archive or "nar", similar to what 'guix substitute'
;;; does.  The intent here is to use substitute servers as content-addressed
;;; mirrors of VCS checkouts.  This is mostly useful for users who have
;;; disabled substitutes.
;;;
;;; Code:

(define (urls-for-item item)
  "Return the fallback nar URL for ITEM--e.g.,
\"/gnu/store/cabbag3…-foo-1.2-checkout\"."
  ;; Here we hard-code nar URLs without checking narinfos.  That's probably OK
  ;; though.
  ;; TODO: Use HTTPS?  The downside is the extra dependency.
  (let ((bases '("http://bordeaux.guix.gnu.org"
                 "http://ci.guix.gnu.org"))
        (item  (basename item)))
    (append (map (cut string-append <> "/nar/lzip/" item) bases)
            (map (cut string-append <> "/nar/" item) bases))))

(define (restore-lzipped-nar port item size)
  "Restore the lzipped nar read from PORT, of SIZE bytes (compressed), to
ITEM."
  (call-with-lzip-input-port port
    (lambda (decompressed-port)
      (restore-file decompressed-port
                    item))))

(define* (download-nar item #:optional (output item))
  "Download and extract to OUTPUT the normalized archive for ITEM, a store
item.  Return #t on success, #f otherwise."
  ;; Let progress reports go through.
  (setvbuf (current-error-port) 'none)
  (setvbuf (current-output-port) 'none)

  (let loop ((urls (urls-for-item item)))
    (match urls
      ((url rest ...)
       (format #t "Trying content-addressed mirror at ~a...~%"
               (uri-host (string->uri url)))
       (let-values (((port size)
                     (catch #t
                       (lambda ()
                         (http-fetch (string->uri url)))
                       (lambda (key . args)
                         (format #t "Unable to fetch from ~a, ~a: ~a~%"
                                 (uri-host (string->uri url))
                                 key
                                 args)
                         (values #f #f)))))
         (if (not port)
             (loop rest)
             (begin
               (if size
                   (format #t "Downloading from ~a (~,2h MiB)...~%" url
                           (/ size (expt 2 20.)))
                   (format #t "Downloading from ~a...~%" url))
               (let* ((reporter (progress-reporter/file
                                 url
                                 size
                                 (current-error-port)
                                 #:abbreviation nar-uri-abbreviation))
                      (port-with-progress
                       (progress-report-port reporter port
                                             #:download-size size)))
                 (if (string-contains url "/lzip")
                     (restore-lzipped-nar port-with-progress
                                          output
                                          size)
                     (restore-file port-with-progress
                                   output)))
               (newline)
               #t))))
      (()
       #f))))
