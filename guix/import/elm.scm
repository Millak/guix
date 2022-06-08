;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
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

(define-module (guix import elm)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (guix utils)
  #:use-module (guix base32)
  #:use-module (guix hash)
  #:use-module (guix http-client)
  #:use-module (guix memoization)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module ((guix ui) #:select (display-hint))
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-package-name->name+version)
                          find-files
                          invoke))
  #:use-module (guix import utils)
  #:use-module (guix git)
  #:use-module (guix import json)
  #:autoload   (gcrypt hash) (hash-algorithm sha256)
  #:use-module (json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system elm)
  #:export (elm-recursive-import
            %elm-package-registry
            %current-elm-checkout
            elm->guix-package))

(define %registry-url
  ;; It is much nicer to fetch this small (< 40 KB gzipped)
  ;; file once than to do many HTTP requests.
  "https://package.elm-lang.org/all-packages")

(define %elm-package-registry
  ;; This is a parameter to support both testing and memoization.
  ;; In pseudo-code, it has the contract:
  ;;     (parameter/c (-> json/c)
  ;;                  (promise/c (vhash/c string? (listof string?))))
  ;; To set the parameter, provide a thunk that returns a value suitable
  ;; as an argument to 'json->registry-vhash'.  Accessing the parameter
  ;; returns a promise wrapping the resulting vhash.
  (make-parameter
   (lambda ()
     (cond
      ((json-fetch %registry-url #:http-fetch http-fetch/cached))
      (else
       (raise (formatted-message
               (G_ "error downloading Elm package registry from ~a")
               %registry-url)))))
   (lambda (thunk)
     (delay (json->registry-vhash (thunk))))))

(define (json->registry-vhash jsobject)
  "Parse the '(json)' module's representation of the Elm package registry to a
vhash mapping package names to lists of available versions, sorted from latest
to oldest."
  (fold (lambda (entry vh)
          (match entry
            ((name . vec)
             (vhash-cons name
                         (sort (vector->list vec) version>?)
                         vh))))
        vlist-null
        jsobject))

(define (json->direct-dependencies jsobject)
  "Parse the '(json)' module's representation of an 'elm.json' file's
'dependencies' or 'test-dependencies' field to a list of strings naming direct
dependencies, handling both the 'package' and 'application' grammars."
  (cond
   ;; *unspecified*
   ((not (pair? jsobject))
    '())
   ;; {"type":"application"}
   ((every (match-lambda
             (((or "direct" "indirect") (_ . _) ...)
              #t)
             (_
              #f))
           jsobject)
    (map car (or (assoc-ref jsobject "direct") '())))
   ;; {"type":"package"}
   (else
    (map car jsobject))))

;; <project-info> handles both {"type":"package"} and {"type":"application"}
(define-json-mapping <project-info> make-project-info project-info?
  json->project-info
  (dependencies project-info-dependencies
                "dependencies" json->direct-dependencies)
  (test-dependencies project-info-test-dependencies
                     "test-dependencies" json->direct-dependencies)
  ;; "synopsis" and "license" may be missing for {"type":"application"}
  (synopsis project-info-synopsis
            "summary" (lambda (x)
                        (if (string? x)
                            x
                            "")))
  (license project-info-license
           "license" (lambda (x)
                       (if (string? x)
                           (spdx-string->license x)
                           #f))))

(define %current-elm-checkout
  ;; This is a parameter for testing purposes.
  (make-parameter
   (lambda (name version)
     (define-values (checkout _commit _relation)
       ;; Elm requires that packages use this very specific format
       (update-cached-checkout (string-append "https://github.com/" name)
                               #:ref `(tag . ,version)))
     checkout)))

(define (make-elm-package-sexp name version)
  "Return two values: the `package' s-expression for the Elm package with the
given NAME and VERSION, and a list of Elm packages it depends on."
  (define checkout
    ((%current-elm-checkout) name version))
  (define info
    (call-with-input-file (string-append checkout "/elm.json")
      json->project-info))
  (define dependencies
    (project-info-dependencies info))
  (define test-dependencies
    (project-info-test-dependencies info))
  (define guix-name
    (elm->package-name name))
  (values
   `(package
      (name ,guix-name)
      (version ,version)
      (source (elm-package-origin
               ,name
               version ;; no ,
               (base32
                ,(bytevector->nix-base32-string
                  (file-hash* checkout
                              #:algorithm (hash-algorithm sha256)
                              #:recursive? #t)))))
      (build-system elm-build-system)
      ,@(maybe-propagated-inputs (map elm->package-name dependencies))
      ,@(maybe-inputs (map elm->package-name test-dependencies))
      (home-page ,(string-append "https://package.elm-lang.org/packages/"
                                 name "/" version))
      (synopsis ,(project-info-synopsis info))
      (description
       ;; Try to use the first paragraph of README.md (which Elm requires),
       ;; or fall back to synopsis otherwise.
       ,(beautify-description
         (match (chunk-lines (call-with-input-file
                                 (string-append checkout "/README.md")
                               read-lines))
           ((_ par . _)
            (string-join par " "))
           (_
            (project-info-synopsis info)))))
      ,@(let ((inferred-name (infer-elm-package-name guix-name)))
          (if (equal? inferred-name name)
              '()
              `((properties '((upstream-name . ,name))))))
      (license ,(project-info-license info)))
   (append dependencies test-dependencies)))

(define elm->guix-package
  (memoize
   (lambda* (package-name #:key repo version)
     "Fetch the metadata for PACKAGE-NAME, an Elm package registered at
package.elm.org, and return two values: the `package' s-expression
corresponding to that package (or #f on failure) and a list of Elm
dependencies."
     (cond
      ((vhash-assoc package-name (force (%elm-package-registry)))
       => (match-lambda
            ((_found latest . _versions)
             (make-elm-package-sexp package-name (or version latest)))))
      (else
       (values #f '()))))))

(define* (elm-recursive-import package-name #:optional version)
  (recursive-import package-name
                    #:version version
                    #:repo->guix-package elm->guix-package
                    #:guix-name elm->package-name))
