;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Nguyễn Gia Phong <cnx@loang.net>
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
;;;
;;; Commentary:
;;;
;;; An <origin> method that open Fossil checkout at a specific version.
;;; The repository URI and version are specified
;;; with a <fossil-reference> object.
;;;
;;; Code:

(define-module (guix fossil-download)
  #:use-module (guix build-system)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (web uri)
  #:export (fossil-reference
            fossil-reference?
            fossil-reference-uri
            fossil-reference-check-in

            fossil-fetch
            fossil-version
            fossil-file-name))

(define-record-type* <fossil-reference>
  fossil-reference make-fossil-reference fossil-reference?
  (uri      fossil-reference-uri)
  (check-in fossil-reference-check-in))

(define (fossil-version version revision check-in)
  "Return the version string for packages using fossil-download."
  ;; fossil-version is almost exclusively executed while modules
  ;; are being loaded, leading to any errors hiding their backtrace.
  ;; Avoid the mysterious error "Value out of range 0 to N: 10"
  ;; when the check-in ID is too short, which can happen, for example,
  ;; when the user swapped the revision and check-in arguments by mistake.
  (when (< (string-length check-in) 10)
    (raise
      (condition
       (&message (message "fossil-version: check-in ID unexpectedly short")))))
  (string-append version "-" revision "." (string-take check-in 10)))

(define (fossil-file-name name version)
  "Return the file-name for packages using fossil-download."
  (string-append name "-" version ".tar.gz"))

(define* (fossil-fetch ref hash-algo hash
                       #:optional name
                       #:key (system (%current-system))
                             (guile (default-guile))
                             (fossil (@* (gnu packages version-control)
                                         fossil)))
  "Return a fixed-output derivation that fetches REF, a <fossil-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (let* ((modules (source-module-closure '((guix build fossil)
                                           (guix build download)
                                           (guix build download-nar))))
         (uri (fossil-reference-uri ref))
         (scheme-of-uri (uri-scheme (string->uri-reference uri)))
         (check-in (fossil-reference-check-in ref))
         (tarball-name (or name (fossil-file-name (basename uri) check-in)))
         (tarball-url (and (eq? 'https scheme-of-uri)
                           (simple-format #f "~a/tarball/~a/~a"
                             uri check-in tarball-name)))
         (guile-json (@* (gnu packages guile) guile-json-4))
         (gnutls (@* (gnu packages tls) guile-gnutls))
         (guile-lzlib (@* (gnu packages guile) guile-lzlib))
         (build
          (with-imported-modules modules
            (with-extensions (list guile-json gnutls ;for (guix swh)
                                   guile-lzlib)
              #~(begin
                  (use-modules (guix build fossil)
                               ((guix build download)
                                #:select (download-method-enabled? url-fetch))
                               (guix build download-nar))
                  (or (and (download-method-enabled? 'upstream)
                           (or (and #$tarball-url
                                    (url-fetch #$tarball-url #$output))
                               (fossil-fetch
                                #$(if scheme-of-uri uri (local-file uri))
                                #$check-in
                                #$output
                                #:fossil-command
                                #+(file-append fossil "/bin/fossil"))))
                      (and (download-method-enabled? 'nar)
                           (download-nar #$output))))))))
    (mlet %store-monad ((guile (package->derivation guile system)))
      (gexp->derivation tarball-name build
                        #:leaked-env-vars '("http_proxy" "https_proxy"
                                            "COLUMNS" "USER")
                        #:env-vars (match (getenv "GUIX_DOWNLOAD_METHODS")
                                     (#f '())
                                     (value
                                      `(("GUIX_DOWNLOAD_METHODS" . ,value))))
                        #:system system
                        #:hash-algo hash-algo
                        #:hash hash
                        #:recursive? #t
                        #:guile-for-build guile
                        #:local-build? #t))))
