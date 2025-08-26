;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-cve)
  #:use-module (guix cve)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64))

;; Generated from the 2019 database :
;; jq -M '.vulnerabilities |= map(select(.cve.id | IN("CVE-2019-14811", "CVE-2019-17365", "CVE-2019-1010180", "CVE-2019-1010204", "CVE-2019-18192", "CVE-2019-0001"))) | .totalResults = (.vulnerabilities | length) | .resultsPerPage = (.vulnerabilities | length)'
(define %sample
  (search-path %load-path "tests/cve-sample.json"))

(define (vulnerability id packages)
  (make-struct/no-tail (@@ (guix cve) <vulnerability>) id packages))

(define %expected-vulnerabilities
  ;; What we should get when reading %SAMPLE.
  (list
   (vulnerability "CVE-2019-1010204"
                  '(("gnu" "binutils" (and (>= "2.21") (<= "2.31.1")))
                    ("gnu" "binutils_gold" (and (>= "1.11") (<= "1.16")))))
   (vulnerability "CVE-2019-1010180"
                  '(("gnu" "gdb" (< "9.1"))))
   (vulnerability "CVE-2019-14811"
                  '(("artifex" "ghostscript" (< "9.50"))))
   (vulnerability "CVE-2019-17365"
                  '(("nixos" "nix" (<= "2.3"))))
   (vulnerability "CVE-2019-18192"
                  '(("gnu" "guix" "1.0.1")))
   ;; Only the "a" CPE configurations are kept; the "o" configurations are discarded.
   ;; This is why CVE-2019-0001 doesn't appear here.
   ))


(test-begin "cve")

(test-equal "json->cve-items"
  '("CVE-2019-0001"
    "CVE-2019-1010204"
    "CVE-2019-1010180"
    "CVE-2019-14811"
    "CVE-2019-17365"
    "CVE-2019-18192")
  (map cve-item-id
       (call-with-input-file %sample json->cve-items)))

(test-equal "cve-item-published-date"
  '(2019)
  (delete-duplicates
   (map (compose date-year cve-item-published-date)
        (call-with-input-file %sample json->cve-items))))

(test-equal "json->vulnerabilities"
  %expected-vulnerabilities
  (call-with-input-file %sample json->vulnerabilities))

(test-equal "vulnerabilities->lookup-proc"
  (list (list (first %expected-vulnerabilities))  ;binutils
        '()
        (list (first %expected-vulnerabilities))
        '()

        (list (second %expected-vulnerabilities))  ;gdb
        (list (second %expected-vulnerabilities))

        (list (third %expected-vulnerabilities))  ;ghostscript
        (list (third %expected-vulnerabilities))
        '()

        (list (fourth %expected-vulnerabilities)) ;nix
        '())
  (let* ((vulns  (call-with-input-file %sample json->vulnerabilities))
         (lookup (vulnerabilities->lookup-proc vulns)))
    (list (lookup "binutils" "2.31.1")
          (lookup "binutils" "2.10")
          (lookup "binutils_gold" "1.11")
          (lookup "binutils" "2.32")
          (lookup "gdb")
          (lookup "gdb" "9.0")
          (lookup "ghostscript")
          (lookup "ghostscript" "9.27")
          (lookup "ghostscript" "9.51")
          (lookup "nix")
          (lookup "nix" "2.4"))))

(test-end "cve")
