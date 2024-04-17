#! /bin/sh
# -*-scheme-*-
build_aux=$(dirname $0)
srcdir=$build_aux/..
export LC_ALL=en_US.UTF-8
export TZ=UTC0
exec guile --no-auto-compile -L $srcdir -C $srcdir -e main -s "$0" "$@"
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
;;; This script provides an xgettext wrapper to (re)set POT-Creation-Date from
;;; a Git timestamp.  Test doing something like:
;;;
;;; build-aux/xgettext.scm --files-from=po/guix/POTFILES.in --default-domain=test
;;;
;;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 curried-definitions)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (guix build utils))

(define ((option? name) option)
  (string-prefix? name option))

(define (get-option args name)
  (let ((option (find (option? name) args)))
    (and option
         (substring option (string-length name)))))

(define (pipe-command command)
  (let* ((port (apply open-pipe* OPEN_READ command))
         (output (read-string port)))
    (close-port port)
    output))


;;;
;;; Entry point.
;;;
(define (main args)
  (fluid-set! %default-port-encoding #f)
  (let* ((files-from (get-option args "--files-from="))
         (default-domain (get-option args "--default-domain="))
         (directory (or (get-option args "--directory=") "."))
         (xgettext (or (get-option args "--xgettext=") "xgettext"))
         (xgettext-args (filter (negate (option? "--xgettext=")) args))
         (command (match xgettext-args
                    ((xgettext.scm args ...)
                     `(,xgettext ,@args))))
         (result (apply system* command))
         (status (/ result 256)))
    (if (or (not (zero? status))
            (not files-from))
        (exit status)
        (let* ((text (with-input-from-file files-from read-string))
               (lines (string-split text #\newline))
               (files (filter (negate (cute string-prefix? "#" <>)) lines))
               (files (map (cute string-append directory "/" <>) files))
               (git-command `("git" "log" "--pretty=format:%ci" "-n1" ,@files))
               (timestamp (pipe-command git-command))
               (source-date-epoch (or (getenv "SOURCE_DATE_EPOCH") "1"))
               (timestamp (if (string-null? timestamp) source-date-epoch
                              timestamp))
               (po-file (string-append default-domain ".po")))
          (substitute* po-file
            (("(\"POT-Creation-Date: )[^\\]*" all header)
             (string-append header timestamp)))))))
