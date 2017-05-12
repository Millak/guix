;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts challenge)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:use-module (guix serialization)
  #:use-module (guix scripts substitute)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 format)
  #:use-module (web uri)
  #:export (compare-contents

            comparison-report?
            comparison-report-item
            comparison-report-result
            comparison-report-local-sha256
            comparison-report-narinfos

            comparison-report-match?
            comparison-report-mismatch?
            comparison-report-inconclusive?

            guix-challenge))

;;; Commentary:
;;;
;;; Challenge substitute servers, checking whether they provide the same
;;; binaries as those built locally.
;;;
;;; Here we completely bypass the daemon to access substitutes.  This is
;;; because we want to be able to report fine-grain information about
;;; discrepancies: We need to show the URL of the offending nar, its hash, and
;;; so on.
;;;
;;; Code:

(define ensure-store-item                         ;XXX: move to (guix ui)?
  (@@ (guix scripts size) ensure-store-item))

;; Representation of a comparison report for ITEM.
(define-record-type <comparison-report>
  (%comparison-report item result local-sha256 narinfos)
  comparison-report?
  (item         comparison-report-item)    ;string, /gnu/store/… item
  (result       comparison-report-result)  ;'match | 'mismatch | 'inconclusive
  (local-sha256 comparison-report-local-sha256)   ;bytevector | #f
  (narinfos     comparison-report-narinfos))      ;list of <narinfo>

(define-syntax comparison-report
  ;; Some sort of a an enum to make sure 'result' is correct.
  (syntax-rules (match mismatch inconclusive)
    ((_ item 'match rest ...)
     (%comparison-report item 'match rest ...))
    ((_ item 'mismatch rest ...)
     (%comparison-report item 'mismatch rest ...))
    ((_ item 'inconclusive rest ...)
     (%comparison-report item 'inconclusive rest ...))))

(define (comparison-report-predicate result)
  "Return a predicate that returns true when pass a REPORT that has RESULT."
  (lambda (report)
    (eq? (comparison-report-result report) result)))

(define comparison-report-mismatch?
  (comparison-report-predicate 'mismatch))

(define comparison-report-match?
  (comparison-report-predicate 'match))

(define comparison-report-inconclusive?
  (comparison-report-predicate 'inconclusive))

(define (locally-built? store item)
  "Return true if ITEM was built locally."
  ;; XXX: For now approximate it by checking whether there's a build log for
  ;; ITEM.  There could be false negatives, if logs have been removed.
  (->bool (log-file store item)))

(define (query-locally-built-hash item)
  "Return the hash of ITEM, a store item, if ITEM was built locally.
Otherwise return #f."
  (lambda (store)
    (guard (c ((nix-protocol-error? c)
               (values #f store)))
      (if (locally-built? store item)
          (values (query-path-hash store item) store)
          (values #f store)))))

(define-syntax-rule (report args ...)
  (format (current-error-port) args ...))

(define (compare-contents items servers)
  "Challenge the substitute servers whose URLs are listed in SERVERS by
comparing the hash of the substitutes of ITEMS that they serve.  Return the
list of <comparison-report> objects.

This procedure does not authenticate narinfos from SERVERS, nor does it verify
that they are signed by an authorized public keys.  The reason is that, by
definition, we may want to target unknown servers.  Furthermore, no risk is
taken since we do not import the archives."
  (define (compare item reference)
    ;; Return a procedure to compare the hash of ITEM with REFERENCE.
    (lambda (narinfo url)
      (or (not narinfo)
          (let ((value (narinfo-hash->sha256 (narinfo-hash narinfo))))
            (bytevector=? reference value)))))

  (define (select-reference item narinfos urls)
    ;; Return a "reference" narinfo among NARINFOS.
    (match narinfos
      ((first narinfos ...)
       (match servers
         ((url urls ...)
          (if (not first)
              (select-reference item narinfos urls)
              (narinfo-hash->sha256 (narinfo-hash first))))))))

  (mlet* %store-monad ((local     (mapm %store-monad
                                        query-locally-built-hash items))
                       (remote -> (append-map (cut lookup-narinfos <> items)
                                              servers))
                       ;; No 'assert-valid-narinfo' on purpose.
                       (narinfos -> (fold (lambda (narinfo vhash)
                                            (vhash-cons (narinfo-path narinfo) narinfo
                                                        vhash))
                                          vlist-null
                                          remote)))
    (return (map (lambda (item local)
                   (match (vhash-fold* cons '() item narinfos)
                     (()                          ;no substitutes
                      (comparison-report item 'inconclusive local '()))
                     ((narinfo)
                      (if local
                          (if ((compare item local) narinfo (first servers))
                              (comparison-report item 'match
                                                 local (list narinfo))
                              (comparison-report item 'mismatch
                                                 local (list narinfo)))
                          (comparison-report item 'inconclusive
                                             local (list narinfo))))
                     ((narinfos ...)
                      (let ((reference
                             (or local (select-reference item narinfos
                                                         servers))))
                        (if (every (compare item reference) narinfos servers)
                            (comparison-report item 'match
                                               local narinfos)
                            (comparison-report item 'mismatch
                                               local narinfos))))))
                 items
                 local))))

(define* (summarize-report comparison-report
                           #:key
                           (hash->string bytevector->nix-base32-string)
                           verbose?)
  "Write to the current error port a summary of REPORT, a <comparison-report>
object.  When VERBOSE?, display matches in addition to mismatches and
inconclusive reports."
  (define (report-hashes item local narinfos)
    (if local
        (report (G_ "  local hash: ~a~%") (hash->string local))
        (report (G_ "  no local build for '~a'~%") item))
    (for-each (lambda (narinfo)
                (report (G_ "  ~50a: ~a~%")
                        (uri->string (narinfo-uri narinfo))
                        (hash->string
                         (narinfo-hash->sha256 (narinfo-hash narinfo)))))
              narinfos))

  (match comparison-report
    (($ <comparison-report> item 'mismatch local (narinfos ...))
     (report (G_ "~a contents differ:~%") item)
     (report-hashes item local narinfos))
    (($ <comparison-report> item 'inconclusive #f narinfos)
     (warning (G_ "could not challenge '~a': no local build~%") item))
    (($ <comparison-report> item 'inconclusive locals ())
     (warning (G_ "could not challenge '~a': no substitutes~%") item))
    (($ <comparison-report> item 'match local (narinfos ...))
     (when verbose?
       (report (G_ "~a contents match:~%") item)
       (report-hashes item local narinfos)))))


;;;
;;; Command-line options.
;;;

(define (show-help)
  (display (G_ "Usage: guix challenge [PACKAGE...]
Challenge the substitutes for PACKAGE... provided by one or more servers.\n"))
  (display (G_ "
      --substitute-urls=URLS
                         compare build results with those at URLS"))
  (display (G_ "
      -v, --verbose      show details about successful comparisons"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  (list  (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix challenge")))

         (option '("substitute-urls") #t #f
                 (lambda (opt name arg result . rest)
                   (apply values
                          (alist-cons 'substitute-urls
                                      (string-tokenize arg)
                                      (alist-delete 'substitute-urls result))
                          rest)))
         (option '("verbose" #\v) #f #f
                 (lambda (opt name arg result . rest)
                   (apply values
                          (alist-cons 'verbose? #t result)
                          rest)))))

(define %default-options
  `((system . ,(%current-system))
    (substitute-urls . ,%default-substitute-urls)))


;;;
;;; Entry point.
;;;

(define (guix-challenge . args)
  (with-error-handling
    (let* ((opts     (parse-command-line args %options (list %default-options)))
           (files    (filter-map (match-lambda
                                   (('argument . file) file)
                                   (_ #f))
                                 opts))
           (system   (assoc-ref opts 'system))
           (urls     (assoc-ref opts 'substitute-urls))
           (verbose? (assoc-ref opts 'verbose?)))
      (leave-on-EPIPE
       (with-store store
         ;; Disable grafts since substitute servers normally provide only
         ;; ungrafted stuff.
         (parameterize ((%graft? #f))
           (let ((files (match files
                          (()
                           (filter (cut locally-built? store <>)
                                   (live-paths store)))
                          (x
                           files))))
             (set-build-options store
                                #:use-substitutes? #f)

             (run-with-store store
               (mlet* %store-monad ((items   (mapm %store-monad
                                                   ensure-store-item files))
                                    (reports (compare-contents items urls)))
                 (for-each (cut summarize-report <> #:verbose? verbose?)
                           reports)

                 (exit (cond ((any comparison-report-mismatch? reports) 2)
                             ((every comparison-report-match? reports) 0)
                             (else 1))))
               #:system system))))))))

;;; challenge.scm ends here
