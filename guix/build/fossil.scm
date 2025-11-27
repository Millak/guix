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
;;; This is the build-side support code of (guix fossil-download).
;;; It allows a Fossil repository to be opened at a specific revision.
;;;
;;; Code:

(define-module (guix build fossil)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (web uri)
  #:export (fossil-fetch))

(define* (fossil-fetch uri check-in file #:key (fossil-command "fossil"))
  "Fetch CHECK-IN from URI into DIRECTORY.  CHECK-IN must be a valid
Fossil check-in name.  Return #t on success, else raise an exception."
  (setenv "FOSSIL_HOME" "/tmp")
  (invoke fossil-command
    "tarball" check-in file "-R"
    (case (uri-scheme (string->uri-reference uri))
      ((file https)                     ;clone the repository first
       (match-let ((repository (simple-format #f "/tmp/~a.fossil"
                                 (basename file ".tar.gz")))
                   ((input . output) (pipe)))
         ;; Trust the TLS certificate of the server,
         ;; since we'll later verify the tarball's checksum.
         (display "y" output)
         (close-port output)
         (with-input-from-port input
           (cut invoke fossil-command "clone"
                "--no-open" "--once" uri repository))
         (close-port input)
         repository))
      ((ssh)                            ;TODO: authentication for SSH
       (let ((message (string-append "fetching a Fossil repository through SSH"
                                     " is not supported: " uri)))
         (raise (condition (&message (message message))))))
      ((#f) uri))))                     ;local file
