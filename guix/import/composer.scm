;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix import composer)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (guix base32)
  #:use-module (guix build git)
  #:use-module (guix build utils)
  #:use-module (guix build-system)
  #:use-module (guix build-system composer)
  #:use-module ((guix diagnostics) #:select (warning))
  #:use-module (guix hash)
  #:use-module (guix i18n)
  #:use-module (guix import json)
  #:use-module (guix import utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix serialization)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (composer->guix-package
            %composer-updater
            composer-recursive-import

            %composer-base-url))

(define %composer-base-url
  (make-parameter "https://repo.packagist.org"))

(define (fix-version version)
  "Return a fixed version from a version string.  For instance, v10.1 -> 10.1"
  (cond
    ((string-prefix? "version" version)
     (if (char-set-contains? char-set:digit (string-ref version 7))
         (substring version 7)
         (substring version 8)))
    ((string-prefix? "v" version)
     (substring version 1))
    (else version)))

(define (json->require dict)
  (if dict
      (let loop ((result '()) (require dict))
        (match require
          (() result)
          ((((? (cut string-contains <> "/") name) . _)
             require ...)
           (loop (cons name result) require))
          ((_ require ...) (loop result require))
          (_ result)))
      '()))

(define-json-mapping <composer-source> make-composer-source composer-source?
  json->composer-source
  (type      composer-source-type)
  (url       composer-source-url)
  (reference composer-source-reference))

(define-json-mapping <composer-package> make-composer-package composer-package?
  json->composer-package
  (description composer-package-description)
  (homepage    composer-package-homepage)
  (source      composer-package-source "source" json->composer-source)
  (name        composer-package-name "name" php-package-name)
  (version     composer-package-version "version" fix-version)
  (require     composer-package-require "require" json->require)
  (dev-require composer-package-dev-require "require-dev" json->require)
  (license     composer-package-license "license"
               (lambda (vector)
                 (let ((l (map string->license (vector->list vector))))
                   (if (eq? (length l) 1)
                       (car l)
                       `(list ,@l))))))

(define (valid-version? v)
  (let ((d (string-downcase v)))
    (and (not (string-contains d "dev"))
         (not (string-contains d "beta"))
         (not (string-contains d "rc")))))

(define* (composer-fetch name #:key version partial-version?)
  "Return a composer-package representation of the Composer metadata for the
package NAME with optional VERSION, or #f on failure.  VERSION may be gien as
version prefix if PARTIAL-VERSION? is #t."
  (and-let* ((url (string-append (%composer-base-url) "/p/" name ".json"))
             (packages (and=> (json-fetch url)
                              (lambda (pkg)
                                (let ((pkgs (assoc-ref pkg "packages")))
                                  (or (assoc-ref pkgs name) pkg)))))
             (all-versions (map car packages))
             (valid-versions (filter valid-version? all-versions))
             (version (or (find-version valid-versions version partial-version?)
                          (and version
                               ;; If the user-provided VERSION could not be
                               ;; found, fallback to look through all
                               ;; versions.
                               (find-version all-versions version
                                             partial-version?)))))
    (json->composer-package (assoc-ref packages version))))

(define (php-package-name name)
  "Given the NAME of a package on Packagist, return a Guix-compliant name for
the package."
  (let ((name (string-join (string-split name #\/) "-")))
    (if (string-prefix? "php-" name)
        (snake-case name)
        (string-append "php-" (snake-case name)))))

(define (make-php-sexp composer-package)
  "Return the `package' s-expression for a PHP package for the given
COMPOSER-PACKAGE."
  (let* ((source (composer-package-source composer-package))
         (dependencies (map php-package-name
                            (composer-package-require composer-package)))
         (dev-dependencies (map php-package-name
                                (composer-package-dev-require composer-package)))
         (git? (equal? (composer-source-type source) "git")))
    ((if git? call-with-temporary-directory call-with-temporary-output-file)
     (lambda* (temp #:optional port)
       (and (if git?
               (begin
                 (mkdir-p temp)
                 (git-fetch (composer-source-url source)
                            (composer-source-reference source)
                            temp))
               (url-fetch (composer-source-url source) temp))
            `(package
               (name ,(composer-package-name composer-package))
               (version ,(composer-package-version composer-package))
               (source
                (origin
                  ,@(if git?
                        `((method git-fetch)
                          (uri (git-reference
                                (url ,(if (string-suffix?
                                           ".git"
                                           (composer-source-url source))
                                          (string-drop-right
                                           (composer-source-url source)
                                           (string-length ".git"))
                                          (composer-source-url source)))
                                (commit ,(composer-source-reference source))))
                          (file-name (git-file-name name version))
                          (sha256
                           (base32
                            ,(bytevector->nix-base32-string
                              (file-hash* temp)))))
                        `((method url-fetch)
                          (uri ,(composer-source-url source))
                          (sha256 (base32 ,(guix-hash-url temp)))))))
               (build-system composer-build-system)
               ,@(if (null? dependencies)
                     '()
                     `((inputs
                        (list ,@(map string->symbol dependencies)))))
               ,@(if (null? dev-dependencies)
                     '()
                     `((native-inputs
                        (list ,@(map string->symbol dev-dependencies)))))
               (synopsis "")
               (description ,(composer-package-description composer-package))
               (home-page ,(composer-package-homepage composer-package))
               (license ,(or (composer-package-license composer-package)
                             'unknown-license!))))))))

(define composer->guix-package
  (memoize
   (lambda* (package-name #:key (version #f) #:allow-other-keys)
     "Fetch the metadata for PACKAGE-NAME from packagist.org, and return the
`package' s-expression corresponding to that package and its list of
dependencies, or #f and the empty list on failure."
     (let ((package (composer-fetch package-name #:version version)))
       (if package
           (let* ((dependencies-names (composer-package-require package))
                  (dev-dependencies-names (composer-package-dev-require package)))
             (values (make-php-sexp package)
                     (append dependencies-names dev-dependencies-names)))
           (values #f '()))))))

(define (guix-name->composer-name name)
  "Given a guix package name, return the name of the package in Packagist."
  (if (string-prefix? "php-" name)
      (let ((components (string-split (substring name 4) #\-)))
        (match components
          ((namespace name ...)
           (string-append namespace "/" (string-join name "-")))))
      name))

(define (guix-package->composer-name package)
  "Given a Composer PACKAGE built from Packagist, return the name of the
package in Packagist."
  (or (assoc-ref (package-properties package) 'upstream-name)
      (guix-name->composer-name (package-name package))))

(define (string->license str)
  "Convert the string STR into a license object."
  (or (spdx-string->license str)
      (match str
        ("GNU LGPL" 'license:lgpl2.0)
        ("GPL" 'license:gpl3)
        ((or "BSD" "BSD License") 'license:bsd-3)
        ((or "MIT" "MIT license" "Expat license") 'license:expat)
        ("Public domain" 'license:public-domain)
        ((or "Apache License, Version 2.0" "Apache 2.0") 'license:asl2.0)
        (_ 'unknown-license!))))

(define (php-package? package)
  "Return true if PACKAGE is a PHP package from Packagist."
  (and
   (eq? (package-build-system package) composer-build-system)
   (string-prefix? "php-" (package-name package))))

(define (dependency->input dependency type)
  (upstream-input
   (name dependency)
   (downstream-name (php-package-name dependency))
   (type type)))

(define* (import-release package #:key version partial-version?)
  "Return an <upstream-source> for VERSION or the latest release of PACKAGE.
If PARTIAL-VERSION? is #t, the provided VERSION may be a partial version
prefix."
  (let* ((php-name (guix-package->composer-name package))
         (composer-package (composer-fetch php-name
                                           #:version version
                                           #:partial-version?
                                           partial-version?)))
    (if composer-package
        (upstream-source
         (package (composer-package-name composer-package))
         (version (composer-package-version composer-package))
         (urls (list (composer-source-url
                      (composer-package-source composer-package))))
         (inputs (append
                  (map (cut dependency->input <> 'regular)
                       (composer-package-require composer-package))
                  (map (cut dependency->input <> 'native)
                       (composer-package-dev-require composer-package)))))
        (begin
          (warning (G_ "failed to parse ~a~%") php-name)
          #f))))

(define %composer-updater
  (upstream-updater
   (name 'composer)
   (description "Updater for Composer packages")
   (pred php-package?)
   (import import-release)))

(define* (composer-recursive-import package-name #:optional version)
  (recursive-import package-name
                    #:version version
                    #:repo->guix-package composer->guix-package
                    #:guix-name php-package-name))
