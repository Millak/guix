;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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

;; TODO: We should rename the importers:
;; - "texlive" -> "ctan"
;; - "texlive-tlpdb" -> "texlive"

(define-module (guix import texlive-tlpdb)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 peg)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages))

;; Define a PEG parser for the tlpdb format
(define-peg-pattern key-name all "name")
(define-peg-pattern key-category all "category")
(define-peg-pattern key-post-action all "postaction")
(define-peg-pattern key-revision all "revision")
(define-peg-pattern key-shortdesc all "shortdesc")
(define-peg-pattern key-longdesc all "longdesc")
(define-peg-pattern key-depend all "depend")
(define-peg-pattern key-catalogue-ctan all "catalogue-ctan")
(define-peg-pattern key-catalogue-contact-repository all "catalogue-contact-repository")
(define-peg-pattern key-catalogue-contact-home all "catalogue-contact-home")
(define-peg-pattern key-catalogue-license all "catalogue-contact-license")
(define-peg-pattern key-catalogue-version all "catalogue-contact-version")
(define-peg-pattern key-catalogue-rest body (and "catalogue" (* (and "-" (+ (range #\a #\z))))))
(define-peg-pattern key-relocated all "relocated")
(define-peg-pattern key-containersize all "containersize")
(define-peg-pattern key-containerchecksum all "containerchecksum")
(define-peg-pattern key-doccontainersize all "doccontainersize")
(define-peg-pattern key-doccontainerchecksum all "doccontainerchecksum")
(define-peg-pattern key-docfiles all "docfiles")
(define-peg-pattern key-srccontainersize all "srccontainersize")
(define-peg-pattern key-srccontainerchecksum all "srccontainerchecksum")
(define-peg-pattern key-srcfiles all "srcfiles")
(define-peg-pattern key-runfiles all "runfiles")
(define-peg-pattern key-binfiles all "binfiles")
(define-peg-pattern key-execute all "execute")
(define-peg-pattern key body (or key-name key-category key-revision
                                 key-shortdesc key-longdesc key-post-action
                                 key-depend key-relocated key-containersize
                                 key-containerchecksum key-doccontainersize
                                 key-doccontainerchecksum key-docfiles
                                 key-srccontainersize key-srccontainerchecksum
                                 key-srcfiles key-runfiles  key-binfiles
                                 key-execute key-catalogue-ctan
                                 key-catalogue-contact-repository
                                 key-catalogue-contact-home key-catalogue-license
                                 key-catalogue-version key-catalogue-rest))
(define-peg-pattern space none " ")
(define-peg-pattern NL none "\n")
(define-peg-pattern rest-of-line body (and space
                                           (* (and (not-followed-by NL) peg-any))
                                           (or NL (not-followed-by peg-any))))
(define-peg-pattern body-line all rest-of-line)
(define-peg-pattern entry-value all (and rest-of-line (* body-line)))
(define-peg-pattern entry-item all (and key entry-value))
(define-peg-pattern entry all (* entry-item))
(define-peg-pattern tlpdb body (* (and entry (?  NL))))

(define (parse- input)
  (let* ((match (match-pattern tlpdb input))
  	 (end   (peg:end match))
  	 (pt    (peg:tree match)))
    (if (eq? (string-length input) end)
  	pt
  	(if match
  	    (begin
  	      (format (current-error-port) "parse error: at offset: ~a\n" end)
  	      (pretty-print pt (current-error-port))
  	      #f)
  	    (begin
  	      (format (current-error-port) "parse error: no match\n")
  	      #f)))))

(define (parse-string input)
  (let* ((pt (parse- input)))
    pt))

(define (parse port)
  (parse-string (read-string port)))

(define parsed-db #f)

(define* (parse-db #:optional db-file)
  (unless parsed-db
    (set! parsed-db (call-with-input-file (or db-file (get-tlpdb)) parse)))
  parsed-db)

(define* (pretty-print-db #:optional db-file)
  ;; TODO: Remove this function?
  (pretty-print (parse-db (or db-file (get-tlpdb)))))

(define (texlive-bin-package)
  "Return the 'texlive-bin' package.  This is a lazy reference so that we
don't depend on (gnu packages tex)."
  (module-ref (resolve-interface '(gnu packages tex)) 'texlive-bin))

(define (get-tlpdb)
  (let ((texlive-base-path (with-store store
                             (derivation->output-path
                              (package-derivation store (texlive-bin-package))))))
    (string-append texlive-base-path "/share/tlpkg/texlive.tlpdb")))

(define (texlive-fetch name)
  (find
   (lambda (entry)
     (match entry
       ((and all
             ('entry ('entry-item ('key-name "name") ('entry-value entry-name)) . _)
             (?  (lambda _ (string=? entry-name name))))
        #t)
       (_ #f)))
   (parse-db)))
