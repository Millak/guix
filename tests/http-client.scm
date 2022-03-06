;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-http-client)
  #:use-module (guix http-client)
  #:use-module (guix tests http)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (web response)
  #:use-module (web uri))

(test-begin "http-client")

(test-equal "http-fetch, one request, binary"
  (string->utf8 "Hello, world.")
  (with-http-server `((200 "Hello, world."))
    (let* ((port (http-fetch (%local-url)))
           (bv   (get-bytevector-all port)))
      (close-port port)
      bv)))

(test-equal "http-fetch, one request, text"
  "Hello, world."
  (with-http-server `((200 "Hello, world."))
    (let* ((port (http-fetch (%local-url) #:text? #t))
           (data (get-string-all port)))
      (close-port port)
      data)))

(test-equal "http-fetch, redirect"
  "Hello, world."
  (with-http-server `((,(build-response
                         #:code 301
                         #:headers
                         `((location
                            . ,(string->uri-reference "/elsewhere")))
                         #:reason-phrase "Moved")
                       "Redirect!")
                      (200 "Hello, world."))
    (let* ((port (http-fetch (%local-url)))
           (data (get-string-all port)))
      (close-port port)
      data)))

(test-equal "http-fetch, error"
  404
  (with-http-server `((404 "Ne trovita."))
    (guard (c ((http-get-error? c) (http-get-error-code c)))
      (http-fetch (%local-url))
      #f)))

(test-equal "http-fetch, redirect + error"
  403
  (with-http-server `((,(build-response
                         #:code 302
                         #:headers
                         `((location
                            . ,(string->uri-reference "/elsewhere")))
                         #:reason-phrase "Moved")
                       "Redirect!")
                      (403 "Verboten."))
    (guard (c ((http-get-error? c) (http-get-error-code c)))
      (http-fetch (%local-url))
      #f)))

(test-end "http-client")
