;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2020, 2023, 2024 Jelle Licht <jlicht@fsfe.org>
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

(define-module (guix import npm-binary)
  #:use-module ((gnu services configuration) #:select (alist?))
  #:use-module (gcrypt hash)
  #:use-module (gnu packages)
  #:use-module (guix base32)
  #:use-module (guix http-client)
  #:use-module (guix import json)
  #:use-module (guix import utils)
  #:use-module (guix memoization)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-9)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (npm-binary-recursive-import
            npm-binary->guix-package
            %npm-registry
            make-versioned-package
            name+version->symbol))

;; Autoload Guile-Semver so we only have a soft dependency.
(module-autoload! (current-module)
		  '(semver)
                  '(string->semver semver? semver->string semver=? semver>?))
(module-autoload! (current-module)
		  '(semver ranges)
                  '(*semver-range-any* string->semver-range semver-range-contains?))

;; Dist-tags
(define-json-mapping <dist-tags> make-dist-tags dist-tags?
  json->dist-tags
  (latest dist-tags-latest "latest" string->semver))

(define-record-type <versioned-package>
  (make-versioned-package name version)
  versioned-package?
  (name  versioned-package-name)       ;string
  (version versioned-package-version)) ;string

(define (dependencies->versioned-packages entries)
  (match entries
    (((names . versions) ...)
     (map make-versioned-package names versions))
    (_ '())))

(define (extract-license license-string)
  (if (unspecified? license-string)
      'unspecified!
      (spdx-string->license license-string)))

(define-json-mapping <dist> make-dist dist?
  json->dist
  (tarball dist-tarball))

(define (empty-or-string s)
  (if (string? s) s ""))

(define-json-mapping <package-revision> make-package-revision package-revision?
  json->package-revision
  (name package-revision-name)
  (version package-revision-version "version"           ;semver
           string->semver)
  (home-page package-revision-home-page "homepage")     ;string
  (dependencies package-revision-dependencies           ;list of versioned-package
                "dependencies"
                dependencies->versioned-packages)
  (dev-dependencies package-revision-dev-dependencies   ;list of versioned-package
                    "devDependencies" dependencies->versioned-packages)
  (peer-dependencies package-revision-peer-dependencies ;list of versioned-package
                    "peerDependencies" dependencies->versioned-packages)
  (license package-revision-license "license"           ;license | #f
           (match-lambda
             ((? unspecified?) #f)
             ((? string? str) (spdx-string->license str))
             ((? alist? alist)
              (match (assoc "type" alist)
                ((_ . (? string? type))
                 (spdx-string->license type))
                (_ #f)))))
  (description package-revision-description             ;string
               "description" empty-or-string)
  (dist package-revision-dist "dist" json->dist))       ;dist

(define (versions->package-revisions versions)
  (match versions
    (((version . package-spec) ...)
     (map json->package-revision package-spec))
    (_ '())))

(define (versions->package-versions versions)
  (match versions
    (((version . package-spec) ...)
     (map string->semver versions))
    (_ '())))

(define-json-mapping <meta-package> make-meta-package meta-package?
  json->meta-package
  (name meta-package-name)                                       ;string
  (description meta-package-description)                         ;string
  (dist-tags meta-package-dist-tags "dist-tags" json->dist-tags) ;dist-tags
  (revisions meta-package-revisions "versions" versions->package-revisions))

(define %npm-registry
  (make-parameter "https://registry.npmjs.org"))
(define %default-page "https://www.npmjs.com/package")

(define (lookup-meta-package name)
  (let ((json (json-fetch (string-append (%npm-registry) "/" (uri-encode name)))))
    (and=> json json->meta-package)))

(define lookup-meta-package* (memoize lookup-meta-package))

(define (meta-package-versions meta)
  (map package-revision-version
       (meta-package-revisions meta)))

(define (meta-package-latest meta)
  (and=> (meta-package-dist-tags meta) dist-tags-latest))

(define* (meta-package-package meta #:optional
                               (version (meta-package-latest meta)))
  (match version
    ((? semver?) (find (lambda (revision)
                         (semver=? version (package-revision-version revision)))
                       (meta-package-revisions meta)))
    ((? string?) (meta-package-package meta (string->semver version)))
    (_ #f)))

(define* (semver-latest svs #:optional (svr *semver-range-any*))
  (find (cut semver-range-contains? svr <>)
        (sort svs semver>?)))

(define* (resolve-package name #:optional (svr *semver-range-any*))
  (let ((meta (lookup-meta-package* name)))
    (and meta
         (let* ((version (semver-latest (or (meta-package-versions meta) '()) svr))
                (pkg (meta-package-package meta version)))
           pkg))))


;;;
;;; Converting packages
;;;

(define (hash-url url)
  "Downloads the resource at URL and computes the base32 hash for it."
  (bytevector->nix-base32-string (port-sha256 (http-fetch url))))

(define (npm-name->name npm-name)
  "Return a Guix package name for the npm package with name NPM-NAME."
  (define (clean name)
    (string-map (lambda (chr) (if (char=? chr #\/) #\- chr))
                (string-filter (negate (cut char=? <> #\@)) name)))
  (downstream-package-name "node-" (clean npm-name)))

(define (name+version->symbol name version)
  (string->symbol (string-append name "-" version)))

(define (package-revision->symbol package)
  (let* ((npm-name (package-revision-name package))
         (version (semver->string (package-revision-version package)))
         (name (npm-name->name npm-name)))
    (name+version->symbol name version)))

(define (npm-package->package-sexp npm-package)
  "Return the `package' s-expression for an NPM-PACKAGE."
  (define resolve-spec
    (match-lambda
      (($ <versioned-package> name version)
       (resolve-package name (string->semver-range version)))))

  (if (package-revision? npm-package)
      (let ((name (package-revision-name npm-package))
            (version (package-revision-version npm-package))
            (home-page (package-revision-home-page npm-package))
            (dependencies (package-revision-dependencies npm-package))
            (dev-dependencies (package-revision-dev-dependencies npm-package))
            (peer-dependencies (package-revision-peer-dependencies npm-package))
            (license (package-revision-license npm-package))
            (description (package-revision-description npm-package))
            (dist (package-revision-dist npm-package)))
        (let* ((name (npm-name->name name))
               (url (dist-tarball dist))
               (home-page (if (string? home-page)
                              home-page
                              (string-append %default-page "/" (uri-encode name))))
               (synopsis description)
               (resolved-deps (map resolve-spec
                                   (append dependencies peer-dependencies)))
               (peer-names (map versioned-package-name peer-dependencies))
               ;; lset-difference for treating peer-dependencies as dependencies,
               ;; which leads to dependency cycles.  lset-union for treating them as
               ;; (ignored) dev-dependencies, which leads to broken packages.
               (dev-names
                (lset-union string=
                            (map versioned-package-name dev-dependencies)
                            peer-names))
               (extra-phases
                (match dev-names
                  (() '())
                  ((dev-names ...)
                   `((add-after 'patch-dependencies 'delete-dev-dependencies
                       (lambda _
                         (modify-json
                          (delete-dependencies '(,@(reverse dev-names)))))))))))
          (values
           `(package
              (name ,name)
              (version ,(semver->string (package-revision-version npm-package)))
              (source (origin
                        (method url-fetch)
                        (uri ,url)
                        (sha256 (base32 ,(hash-url url)))))
              (build-system node-build-system)
              (arguments
               (list
                #:tests? #f
                #:phases
                #~(modify-phases %standard-phases
                    (delete 'build)
                    ,@extra-phases)))
              ,@(match dependencies
                  (() '())
                  ((dependencies ...)
                   `((inputs
                      (list ,@(map package-revision->symbol resolved-deps))))))
              (home-page ,home-page)
              (synopsis ,synopsis)
              (description ,description)
              (license ,license))
           (map (match-lambda (($ <package-revision> name version)
                               (list name (semver->string version))))
                resolved-deps))))
      (values #f '())))


;;;
;;; Interface
;;;

(define npm-binary->guix-package
  (lambda* (name #:key (version *semver-range-any*) #:allow-other-keys)
    (let* ((svr (match version
                  ((? string?) (string->semver-range version))
                  (_ version)))
           (pkg (resolve-package name svr)))
      (npm-package->package-sexp pkg))))

(define* (npm-binary-recursive-import package-name #:key version)
  (recursive-import package-name
                    #:repo->guix-package (memoize npm-binary->guix-package)
                    #:version version
                    #:guix-name npm-name->name))
