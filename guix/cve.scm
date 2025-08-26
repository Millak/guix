;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (guix cve)
  #:use-module (guix utils)
  #:use-module (guix http-client)
  #:use-module (guix i18n)
  #:use-module ((guix diagnostics) #:select (formatted-message))
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:export (json->cve-items

            cve-item?
            cve-item-id
            cve-item-configurations
            cve-item-published-date
            cve-item-last-modified-date

            cve-reference?
            cve-reference-url
            cve-reference-tags

            vulnerability?
            vulnerability-id
            vulnerability-packages

            json->vulnerabilities
            current-vulnerabilities
            vulnerabilities->lookup-proc))

;;; Commentary:
;;;
;;; This modules provides the tools to fetch, parse, and digest part of the
;;; Common Vulnerabilities and Exposures (CVE) feeds provided by the US NIST
;;; at <https://nvd.nist.gov/vuln/data-feeds>.
;;;
;;; Code:

(define (string->date* str)
  (string->date str "~Y-~m-~dT~H:~M:~S"))

(define-json-mapping <cve-item> cve-item cve-item?
  json->cve-item
  (id             cve-item-id "id")       ;string
  (configurations cve-item-configurations ;list of sexps
                  "configurations" configuration-data->cve-configurations)
  (published-date cve-item-published-date
                  "published" string->date*)
  (last-modified-date cve-item-last-modified-date
                      "lastModified" string->date*))

(define-json-mapping <cve-reference> cve-reference cve-reference?
  json->cve-reference
  (url            cve-reference-url)              ;string
  (tags           cve-reference-tags              ;list of strings
                  "tags" vector->list))

(define %cpe-package-rx
  ;; For applications: "cpe:2.3:a:VENDOR:PACKAGE:VERSION", or sometimes
  ;; "cpe:2.3:a:VENDOR:PACKAGE:VERSION:PATCH-LEVEL".
  (make-regexp "^cpe:2\\.3:a:([^:]+):([^:]+):([^:]+):([^:]+):"))

(define (cpe->package-identifier cpe)
  "Converts the Common Platform Enumeration (CPE) string CPE to a package
identifier, in a very naive way.  Return three values: the CPE vendor, the
package name, and its version string.
Return three #f values if CPE does not look like an application CPE string."
  (cond ((regexp-exec %cpe-package-rx cpe)
         =>
         (lambda (matches)
           (values (match:substring matches 1)
                   (match:substring matches 2)
                   (match (match:substring matches 3)
                     ("*" '_)
                     (version
                      (string-append version
                                     (match (match:substring matches 4)
                                       ("" "")
                                       (patch-level
                                        ;; Drop the colon from things like
                                        ;; "cpe:2.3:a:openbsd:openssh:6.8:p1".
                                        (string-drop patch-level 1)))))))))
        (else
         (values #f #f #f))))

(define (cpe-match->cve-configuration alist)
  "Convert ALIST, a \"cpeMatch\" alist, into an sexp representing the package
and versions matched.  Return #f if ALIST doesn't correspond to an application
package."
  (let ((cpe    (assoc-ref alist "criteria"))
        (starti (assoc-ref alist "versionStartIncluding"))
        (starte (assoc-ref alist "versionStartExcluding"))
        (endi   (assoc-ref alist "versionEndIncluding"))
        (ende   (assoc-ref alist "versionEndExcluding")))
    ;; Normally "criteria" is here in each "cpeMatch" item, but CVE-2020-0534
    ;; has a configuration that lacks it.
    (and cpe
         (let ((vendor package version (cpe->package-identifier cpe)))
           (and package
                `(,vendor
                  ,package
                  ,(cond ((and (or starti starte) (or endi ende))
                          `(and ,(if starti `(>= ,starti) `(> ,starte))
                                ,(if endi `(<= ,endi) `(< ,ende))))
                         (starti `(>= ,starti))
                         (starte `(> ,starte))
                         (endi   `(<= ,endi))
                         (ende   `(< ,ende))
                         (else   version))))))))

(define (configuration-data->cve-configurations vector)
  "Given ALIST, a JSON dictionary for the baroque \"configurations\"
element found in CVEs, return an sexp such as (\"binutils\" (<
\"2.31\")) that represents matching configurations."
  (define string->operator
    (match-lambda
      ("OR" 'or)
      ("AND" 'and)))

  (define (maybe-vector->alist vector)
    (vector->list (or (and (unspecified? vector) #()) vector #())))

  (define (node->configuration node)
    (let ((operator (string->operator (assoc-ref node "operator"))))
      (cond
       ((assoc-ref node "cpeMatch")
        =>
        (lambda (matches)
          (let ((matches (vector->list matches)))
            (match (filter-map cpe-match->cve-configuration
                               matches)
              (()    #f)
              ((one) one)
              (lst   (cons operator lst))))))
       ((assoc-ref node "children")               ;typically for 'and'
        =>
        (lambda (children)
          (match (filter-map node->configuration (vector->list children))
            (()    #f)
            ((one) one)
            (lst   (cons operator lst)))))
       (else
        #f))))

  (let* ((alist (maybe-vector->alist vector))
         (nodes (if (null? alist)
                    '()
                     (maybe-vector->alist (assoc-ref (car alist) "nodes")))))
    (filter-map node->configuration nodes)))

(define (json->cve-items json)
  "Parse JSON, an input port or a string, and return a list of <cve-item>
records."
  (let ((alist   (json->scm json)))
    (match (assoc-ref alist "format")
      ("NVD_CVE"
       #t)
      (format
       (raise (formatted-message (G_ "unsupported CVE format: '~a'")
                                 format))))
    (match (assoc-ref alist "version")
      ("2.0"
       #t)
      (version
       (raise (formatted-message (G_ "unsupported CVE data version: '~a'")
                                 version))))

    (map (compose json->cve-item (cut assoc-ref <> "cve"))
         (vector->list (assoc-ref alist "vulnerabilities")))))

(define (version-matches? version sexp)
  "Return true if VERSION, a string, matches SEXP."
  (match sexp
    ('_
     #t)
    ((? string? expected)
     (version-prefix? expected version))
    (('or sexps ...)
     (any (cut version-matches? version <>) sexps))
    (('and sexps ...)
     (every (cut version-matches? version <>) sexps))
    (('< max)
     (version>? max version))
    (('<= max)
     (version>=? max version))
    (('> min)
     (version>? version min))
    (('>= min)
     (version>=? version min))))

(define (vulnerability-matches? vuln vendor hidden-vendors)
  "Checks if a VENDOR matches at least one of <vulnerability> VULN
packages.  When VENDOR is #f, ignore packages that have a vendor among
HIDDEN-VENDORS."
  (define hidden-vendor?
    (if (list? hidden-vendors)
        (cut member <> hidden-vendors)
        (const #f)))
  (define vendor=?
    (if vendor
        (cut string=? <> vendor)
        (const #f)))

  (match vuln
    (($ <vulnerability> id packages)
     (any (match-lambda
            ((? vendor=?)
             #t)
            ((? hidden-vendor?)
             #f)
            (otherwise
             (not vendor)))
          (map car packages)))))  ;candidate vendors


;;;
;;; High-level interface.
;;;

(define %now
  (current-date))
(define %current-year
  (date-year %now))
(define %past-year
  (- %current-year 1))

(define (yearly-feed-uri year)
  "Return the URI for the CVE feed for YEAR."
  (string->uri
   (string-append "https://nvd.nist.gov/feeds/json/cve/2.0/nvdcve-2.0-"
                  (number->string year) ".json.gz")))

(define %current-year-ttl
  ;; According to <https://nvd.nist.gov/download.cfm#CVE_FEED>, feeds are
  ;; updated "approximately every two hours."
  (* 60 30))

(define %past-year-ttl
  ;; Update the previous year's database more and more infrequently.
  (* 3600 24 (date-month %now)))

(define-record-type <vulnerability>
  (vulnerability id packages)
  vulnerability?
  (id         vulnerability-id)             ;string
  (packages   vulnerability-packages))      ;((v1 p1 sexp1) (v2 p2 sexp2) ...)

(define vulnerability->sexp
  (match-lambda
    (($ <vulnerability> id packages)
     `(v ,id ,packages))))

(define sexp->vulnerability
  (match-lambda
    (('v id (packages ...))
     (vulnerability id packages))))

(define sexp-v1->vulnerability
  (match-lambda
    (('v id (packages ...))
     (vulnerability id (map (cut cons #f <>) packages)))))

(define vendor-tuple?
  (match-lambda
    ((vendor package _)
     #t)
    (otherwise
     #f)))

(define (cve-configuration->package-list config)
  "Parse CONFIG, a config sexp, and return a list of the form (V P SEXP)
where V is a CPE vendor, P is a package name and SEXP expresses constraints on
the matching versions."
  (let loop ((config config)
             (results '()))
    (match config
      (('or configs ...)
       (fold loop results configs))
      (('and config _ ...)                            ;XXX
       (loop config results))
      (((? string? vendor) (? string? package) '_)  ;any version
       (cons `(,vendor ,package _) (remove vendor-tuple? results)))
      (((? string? vendor) (? string? package) sexp)
       (match (assoc-ref (assoc-ref results vendor) package)
         ((previous)
          (cons `(,vendor ,package (or ,sexp ,previous))
                (remove vendor-tuple? results)))
         (_
          (cons `(,vendor ,package ,sexp) results)))))))

(define (merge-package-lists lst)
  "Merge the list in LST, each of which has the form (V P SEXP), where V is a
CPE vendor, P is the name of a package and SEXP is an sexp that constrains
matching versions."
  (fold (lambda (plist result)                    ;XXX: quadratic
          (fold (match-lambda*
                  (((vendor package version) result)
                   (match (assoc-ref result vendor)
                     (((? (cut string=? package <>)) previous)
                      (cons `(,vendor ,package (or ,version ,previous))
                            (remove vendor-tuple? result)))
                     (_
                      (cons `(,vendor ,package ,version) result)))))
                result
                plist))
        '()
        lst))

(define (cve-item->vulnerability item)
  "Return a <vulnerability> corresponding to ITEM, a <cve-item> record;
return #f if ITEM does not list any configuration or if it does not list
any \"a\" (application) configuration."
  (match (cve-item-configurations item)
    (()                                         ;no configurations
     #f)
    ((configs ...)
     (vulnerability (cve-item-id item)
                    (merge-package-lists
                     (map cve-configuration->package-list configs))))))

(define (json->vulnerabilities json)
  "Parse JSON, an input port or a string, and return the list of
vulnerabilities found therein."
  (filter-map cve-item->vulnerability (json->cve-items json)))

(define (write-cache input cache)
  "Read vulnerabilities as gzipped JSON from INPUT, and write it as a compact
sexp to CACHE."
  (call-with-decompressed-port 'gzip input
    (lambda (input)
      (define vulns
        (json->vulnerabilities input))

      (write `(vulnerabilities
               2                                  ;format version
               ,(map vulnerability->sexp vulns))
             cache))))

(define* (fetch-vulnerabilities year ttl #:key (timeout 10))
  "Return the list of <vulnerability> for YEAR, assuming the on-disk cache has
the given TTL (fetch from the NIST web site when TTL has expired)."
  (define (cache-miss uri)
    (format (current-error-port) "fetching CVE database for ~a...~%" year))

  (define (read* port)
    ;; Disable read options to avoid populating the source property weak
    ;; table, which speeds things up, saves memory, and works around
    ;; <https://lists.gnu.org/archive/html/guile-devel/2017-09/msg00031.html>.
    (let ((options (read-options)))
      (dynamic-wind
        (lambda ()
          (read-disable 'positions))
        (lambda ()
          (read port))
        (lambda ()
          (read-options options)))))

  ;; Note: We used to keep the original JSON files in cache but parsing it
  ;; would take typically ~15s for a year of data.  Thus, we instead store a
  ;; summarized version thereof as an sexp, which can be parsed in 1s or so.
  (let* ((port (http-fetch/cached (yearly-feed-uri year)
                                  #:ttl ttl
                                  #:write-cache write-cache
                                  #:cache-miss cache-miss
                                  #:timeout timeout))
         (sexp (read* port)))
    (close-port port)
    (match sexp
      (('vulnerabilities 2 vulns)
       (map sexp->vulnerability vulns))
      (('vulnerabilities 1 vulns)  ;old format, lacks vendor info
       (map sexp-v1->vulnerability vulns)))))

(define* (current-vulnerabilities #:key (timeout 10))
  "Return the current list of Common Vulnerabilities and Exposures (CVE) as
published by the US NIST.  TIMEOUT specifies the timeout in seconds for
connection establishment."
  (let ((past-years (unfold (cut > <> 3)
                            (lambda (n)
                              (- %current-year n))
                            1+
                            1))
        (past-ttls  (unfold (cut > <> 3)
                            (lambda (n)
                              (* n %past-year-ttl))
                            1+
                            1)))
    (append-map (cut fetch-vulnerabilities <> <> #:timeout timeout)
                (cons %current-year past-years)
                (cons %current-year-ttl past-ttls))))

(define (vulnerabilities->lookup-proc vulnerabilities)
  "Return a lookup procedure built from VULNERABILITIES that takes a package
name and optionally a version number.  When the version is omitted, the lookup
procedure returns a list of vulnerabilities; otherwise, it returns a list of
vulnerabilities affecting the given package version."
  (define table
    ;; Map package names to lists of version/vulnerability pairs.
    (fold (lambda (vuln table)
            (match vuln
              (($ <vulnerability> id packages)
               (fold (lambda (package table)
                       (match package
                         ((vendor name . versions)
                          (vhash-cons name (cons vuln versions)
                                      table))))
                     table
                     packages))))
          vlist-null
          vulnerabilities))

  (lambda* (package #:optional version #:key (vendor #f) (hidden-vendors '()))
    (vhash-fold*
     (lambda (pair result)
       (match pair
         ((vuln sexp)
          (if (and (or (and (not vendor) (null? hidden-vendors))
                       (vulnerability-matches? vuln vendor hidden-vendors))
                   (or (not version) (version-matches? version sexp)))
              (cons vuln result)
              result))))
     '()
     package table)))


;;; cve.scm ends here
