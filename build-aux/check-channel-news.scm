;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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
;;; Validate 'etc/news.scm'.
;;;

(use-modules (git)
             (guix git)
             (guix ui)
             (guix channels)
             (srfi srfi-26)
             (rnrs bytevectors)
             (ice-9 match))

;; XXX: These two things are currently private.
(define read-channel-news (@@ (guix channels) read-channel-news))
(define channel-news-entries (cut struct-ref <> 0))

(define (all-the-news repository commit)
  "Return the <channel-news> read from the 'guix' channel REPOSITORY at COMMIT."
  (let* ((tree (commit-tree (commit-lookup repository commit)))
         (oid (tree-entry-id (tree-entry-bypath tree "etc/news.scm")))
         (blob (blob-lookup repository oid)))
    (call-with-input-string (utf8->string (blob-content blob))
      read-channel-news)))

(define (validate-texinfo str type language)
  "Parse STR as a Texinfo fragment and raise an error if that fails."
  (catch #t
    (lambda ()
      (texi->plain-text str))
    (lambda (key . args)
      (print-exception (current-error-port) #f key args)
      (report-error (G_ "the Texinfo snippet below is invalid (~a, ~a):~%")
                    type language)
      (display str (current-error-port))
      (exit 1))))

(define (validate-news-entry repository entry)
  "Validate ENTRY, a <channel-news-entry>, making sure it refers to an
existent commit of REPOSITORY and contains only valid Texinfo."
  (catch 'git-error
    (lambda ()
      (let ((commit (commit-lookup repository
                                   (string->oid
                                    (channel-news-entry-commit entry)))))
        (for-each (match-lambda
                    ((language . title)
                     (validate-texinfo title 'title language)))
                  (channel-news-entry-title entry))
        (for-each (match-lambda
                    ((language . body)
                     (validate-texinfo body 'body language)))
                  (channel-news-entry-body entry))))
    (lambda (key error . rest)
      (if (= GIT_ENOTFOUND (git-error-code error))
          (leave (G_ "commit '~a' of entry '~a' does not exist~%")
                 (channel-news-entry-commit entry)
                 (channel-news-entry-title entry))
          (apply throw key error rest)))))

(with-repository (dirname (dirname (current-filename))) repository
  (let* ((head (reference-target (repository-head repository)))
         (entries (channel-news-entries (all-the-news repository head))))
    (for-each (cut validate-news-entry repository <>)
              entries)
    (info (G_ "All ~a channel news entries are valid at commit ~a.~%")
          (length entries)
          (oid->string head))))
