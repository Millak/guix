;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (test-rpm)
  #:use-module (guix rpm)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71))

;; For white-box testing.
(define-syntax-rule (expose-internal name)
  (define name (@@ (guix rpm) name)))

(expose-internal RPMTAG_ARCH)
(expose-internal RPMTAG_LICENSE)
(expose-internal RPMTAG_NAME)
(expose-internal RPMTAG_OS)
(expose-internal RPMTAG_RELEASE)
(expose-internal RPMTAG_SUMMARY)
(expose-internal RPMTAG_VERSION)
(expose-internal header-entry-count)
(expose-internal header-entry-tag)
(expose-internal header-entry-value)
(expose-internal header-entry?)
(expose-internal make-header)
(expose-internal make-header-entry)
(expose-internal make-header-index+data)

(test-begin "rpm")

(test-equal "lead must be 96 bytes long"
  96
  (length (generate-lead "hello-2.12.1")))

(define header-entries
  (list (make-header-entry RPMTAG_NAME 1 "hello")
        (make-header-entry RPMTAG_VERSION 1 "2.12.1")
        (make-header-entry RPMTAG_RELEASE 1 "0")
        (make-header-entry RPMTAG_SUMMARY 1
                           "Hello, GNU world: An example GNU package")
        (make-header-entry RPMTAG_LICENSE 1 "GPL 3 or later")
        (make-header-entry RPMTAG_OS 1 "Linux")
        (make-header-entry RPMTAG_ARCH 1 "x86_64")))

(define expected-header-index-length
  (* 16 (length header-entries)))       ;16 bytes per index entry

(define expected-header-data-length
  (+ (length header-entries)            ;to account for null bytes
     (fold + 0 (map (compose string-length (cut header-entry-value <>))
                    header-entries))))

(let ((index data (make-header-index+data header-entries)))
  (test-equal "header index"
    expected-header-index-length
    (length index))

  ;; This test depends on the fact that only STRING entries are used, and that
  ;; they are composed of single byte characters and the delimiting null byte.
  (test-equal "header data"
    expected-header-data-length
    (length data)))

(test-equal "complete header section"
  (+ 16                                 ;leading magic + count bytes
     expected-header-index-length expected-header-data-length)
  (length (make-header header-entries)))

(test-end)
