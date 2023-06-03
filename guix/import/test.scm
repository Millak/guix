;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix import test)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module ((guix utils) #:select (version-prefix?))
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (%test-updater))

;;; Commentary:
;;;
;;; This module defines a pseudo updater whose sole purpose is to allow
;;; testing of the whole 'guix refresh' command.
;;;
;;; Code:

(define test-target-version
  ;; VHash that maps package names to version/URL tuples.
  (make-parameter
   (or (and=> (getenv "GUIX_TEST_UPDATER_TARGETS")
              (lambda (str)
                (alist->vhash (call-with-input-string str read))))
       vlist-null)))

(define (available-updates package)
  "Return the list of available <upstream-source> records for PACKAGE."
  (vhash-fold* (lambda (version+updates result)
                 (match version+updates
                   ((version (updates ...))
                    (if (version-prefix? version
                                         (package-version package))
                        (append (map (match-lambda
                                       ((version url)
                                        (upstream-source
                                         (package (package-name package))
                                         (version version)
                                         (urls (list url))))
                                       ((version url (inputs ...))
                                        (upstream-source
                                         (package (package-name package))
                                         (version version)
                                         (urls (list url))
                                         (inputs
                                          (map (lambda (name)
                                                 (upstream-input
                                                  (name name)
                                                  (downstream-name name)))
                                               inputs)))))
                                     updates)
                                result)
                        result))))
               '()
               (package-name package)
               (test-target-version)))

(define (test-package? package)
  "Return true if PACKAGE has pseudo updates available."
  (and (not (vlist-null? (test-target-version)))  ;cheap test
       (pair? (available-updates package))))

(define* (import-release package #:key (version #f))
  "Return the <upstream-source> record denoting either the latest version of
PACKAGE or VERSION."
  (match (available-updates package)
    (() #f)
    ((sources ...)
     (if version
         (find (lambda (source)
                 (string=? (upstream-source-version source)
                           version))
               sources)
         (first sources)))))

(define %test-updater
  (upstream-updater
   (name 'test)
   (description "Pseudo updater for testing purposes.")
   (pred test-package?)
   (import import-release)))
