#! /bin/sh
# -*-scheme-*-
export LANG=C LANGUAGE=C LC_TIME=C
export TZ=UTC0
exec guile --no-auto-compile -L $srcdir -C $srcdir -e '(mdate-from-git)' -s "$0" "$@"
!#

;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:
;;;
;;; Usage: mdate-from-git.scm FILE
;;;
;;; This script is compatible with Automake's `mdate-sh' but uses the timestamp
;;; from Git instead of from the file system.  Also, it can be appended to
;;; mdate-sh.

;;; As a special exception for Guix, it caters for doc/guix.LANG.texi files that
;;; are not stored in Git, by using po/doc/guix-manual.LANG.po for the Git
;;; timestamp.  Test doing something like:
;;;
;;; build-aux/mdate-from-git.scm doc/guix.de.texi
;;;
;;;; Code:

(define-module (mdate-from-git)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (main))

(define (pipe-command command)
  (let* ((port (apply open-pipe* OPEN_READ command))
         (output (read-string port)))
    (close-port port)
    output))

(define (guix.LANG.texi->guix-manual.LANG.po file-name)
  "Translated manuals doc/guix.LANG.texi are not tracked in Git and are
generated from po/doc/guix-manual.LANG.po.  For such an untraced .TEXI file,
return its .PO counterpart."
  (let ((m (string-match "doc/guix.([^.]+).texi" file-name)))
    (if (not m) file-name
        (let ((lang (match:substring m 1)))
          (format #f "po/doc/guix-manual.~a.po" lang)))))


;;;
;;; Entry point.
;;;
(define (main args)
  (match args
    ((script file-name)
     (let* ((command `("git" "ls-files" "--error-unmatch" "--" ,file-name))
            (tracked? (zero? (with-error-to-port (%make-void-port "w")
                               (lambda _
                                 (with-output-to-port (%make-void-port "w")
                                   (lambda _ (apply system* command)))))))
            (file-name (if tracked? file-name
                           (guix.LANG.texi->guix-manual.LANG.po file-name)))
            (command `("git" "log" "--pretty=format:%ct" "-n1" "--" ,file-name))
            (timestamp (with-error-to-port  (%make-void-port "w")
                         (lambda _ (pipe-command command))))
            (source-date-epoch (or (getenv "SOURCE_DATE_EPOCH") "1"))
            (timestamp (if (string-null? timestamp) source-date-epoch
                           timestamp))
            (time (gmtime (string->number timestamp)))
            (d-m-y (strftime "%-d %B %Y" time)))
       (display d-m-y)))
    (_
     (format (current-error-port) "Usage: mdate-from-git.scm FILE\n")
     (exit 2))))
