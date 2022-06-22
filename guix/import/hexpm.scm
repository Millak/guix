;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017, 2019-2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020-2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix import hexpm)
  #:use-module (guix base32)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (gcrypt hash)
  #:use-module (guix http-client)
  #:use-module (json)
  #:use-module (guix import utils)
  #:use-module ((guix import json) #:select (json-fetch))
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-package-name->name+version)
                          dump-port))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (guix build-system rebar)
  #:export (hexpm->guix-package
            guix-package->hexpm-name
            strings->licenses   ;;  why used here?
            hexpm-recursive-import
            %hexpm-updater))

;;;
;;; Interface to https://hex.pm/api, version 2.
;;; REST-API end-points:
;;;   https://github.com/hexpm/specifications/blob/master/apiary.apib
;;; Repository end-points:
;;;   https://github.com/hexpm/specifications/blob/master/endpoints.md
;;;

(define %hexpm-api-url
  (make-parameter "https://hex.pm/api"))

(define (package-url name)
  (string-append (%hexpm-api-url) "/packages/" name))

;;
;; Hexpm Package. /packages/${name}
;; https://github.com/hexpm/specifications/blob/master/apiary.apib#Package
;;
;; Each package can have several "releases", each of which has its own set of
;; requirements, build-tool, etc. - see <hexpm-release> below.
(define-json-mapping <hexpm-pkgdef> make-hexpm-pkgdef hexpm-pkgdef?
  json->hexpm
  (name          hexpm-name)                          ; string
  (html-url      hexpm-html-url      "html_url")      ; string
  (docs-html-url hexpm-docs-html-url "docs_html_url") ; string | 'null
  (meta          hexpm-meta "meta" json->hexpm-meta)
  (versions      hexpm-versions "releases" ; list of <hexpm-version>
                 (lambda (vector)
                   (map json->hexpm-version
                        (vector->list vector))))
  ;; "latest_version" and "latest_stable_version" are not named in the
  ;; specification, butt seen in practice.
  (latest-version hexpm-latest-version "latest_version") ; string
  (latest-stable  hexpm-latest-stable "latest_stable_version")) ; string

;; Hexpm package metadata.
(define-json-mapping <hexpm-meta> make-hexpm-meta hexpm-meta?
  json->hexpm-meta
  (description hexpm-meta-description)        ;string
  (licenses    hexpm-meta-licenses "licenses" ;list of strings
               (lambda (vector)
                 (or (and vector (vector->list vector))
                     #f))))

;; Hexpm package versions.
(define-json-mapping <hexpm-version> make-hexpm-version hexpm-version?
  json->hexpm-version
  (number  hexpm-version-number "version")   ;string
  (url     hexpm-version-url))               ;string


(define (lookup-hexpm name)
  "Look up NAME on hex.pm and return the corresopnding <hexpm> record
or #f if it was not found."
  (and=> (json-fetch (package-url name))
         json->hexpm))

;;
;; Hexpm release. /packages/${name}/releases/${version}
;; https://github.com/hexpm/specifications/blob/master/apiary.apib#Release
;;
(define-json-mapping <hexpm-release> make-hexpm-release hexpm-release?
  json->hexpm-release
  (version hexpm-release-version)  ; string
  (url     hexpm-release-url)      ; string
  (meta    hexpm-release-meta "meta" json->hexpm-release-meta)
  ;; Specification names the next fields "dependencies", but in practice it is
  ;; "requirements".
  (dependencies hexpm-requirements "requirements")) ; list of <hexpm-dependency>

;; Hexpm release meta.
;; https://github.com/hexpm/specifications/blob/main/package_metadata.md
(define-json-mapping <hexpm-release-meta>
  make-hexpm-release-meta hexpm-release-meta?
  json->hexpm-release-meta
  (app         hexpm-release-meta-app)        ; string
  (elixir      hexpm-release-meta-elixir)     ; string
  (build-tools hexpm-release-meta-build-tools "build_tools" ; list of strings
               (lambda (vector)
                 (or (and vector (vector->list vector))
                     (list)))))

;; Hexpm dependency.  Each requirement has information about the required
;; version, such as "~> 2.1.2" or ">= 2.1.2 and < 2.2.0", see
;; <https://hexdocs.pm/elixir/Version.html#module-requirements>, and whether
;; the dependency is optional.
(define-json-mapping <hexpm-dependency> make-hexpm-dependency
  hexpm-dependency?
  json->hexpm-dependency
  (name        hexpm-dependency-name "app")   ; string
  (requirement hexpm-dependency-requirement)  ; string
  (optional    hexpm-dependency-optional))    ; bool

(define (hexpm-release-dependencies release)
  "Return the list of dependency names of RELEASE, a <hexpm-release>."
  (let ((reqs (or (hexpm-requirements release) '#())))
    (map first reqs)))  ;; TODO: also return required version


(define (lookup-hexpm-release version*)
  "Look up RELEASE on hexpm-version-url and return the corresopnding
<hexpm-release> record or #f if it was not found."
  (and=> (json-fetch (hexpm-version-url version*))
         json->hexpm-release))


;;;
;;; Converting hex.pm packages to Guix packages.
;;;

(define (maybe-inputs package-inputs input-type)
  "Given a list of PACKAGE-INPUTS, tries to generate the 'inputs' field of a
package definition.  INPUT-TYPE, a symbol, is used to populate the name of
the input field."
  (match package-inputs
    (()
     '())
    ((package-inputs ...)
     `((,input-type (list ,@package-inputs))))))

(define (dependencies->package-names names)
  "Given a list of hexpm package NAMES, returns a list of guix package names
as symbols."
  ;; TODO: Base name on language of dependency.
  ;; The language used for implementing the dependency is not know without
  ;; recursing the dependencies.  So for now assume more packages are based on
  ;; Erlang and prefix all dependencies with "erlang-" (the default).
  (map string->symbol
       (map hexpm-name->package-name
            (sort names string-ci<?))))

(define* (make-hexpm-sexp #:key name version tarball-url
                          home-page synopsis description license
                          language build-system dependencies
                          #:allow-other-keys)
  "Return the `package' s-expression for a hexpm package with the given NAME,
VERSION, TARBALL-URL, HOME-PAGE, SYNOPSIS, DESCRIPTION, and LICENSE. The
created package's name will stem from LANGUAGE. BUILD-SYSTEM defined the
build-system, and DEPENDENCIES the inputs for the package."
  (call-with-temporary-output-file
   (lambda (temp port)
     (and (url-fetch tarball-url temp)
          (values
       `(package
         (name ,(hexpm-name->package-name name language))
         (version ,version)
         (source (origin
                   (method url-fetch)
                   (uri (hexpm-uri ,name version))
                   (sha256 (base32 ,(guix-hash-url temp)))))
         (build-system ,build-system)
         ,@(maybe-inputs (dependencies->package-names dependencies) 'inputs)
         (synopsis ,synopsis)
         (description ,(beautify-description description))
         (home-page ,(match home-page
                            (() "")
                            (_ home-page)))
         (license ,(match license
                          (() #f)
                          ((license) license)
                          (_ `(list ,@license))))))))))

(define (strings->licenses strings)
  "Convert the list of STRINGS into a list of license objects."
  (filter-map (lambda (license)
                (and (not (string-null? license))
                     (not (any (lambda (elem) (string=? elem license))
                               '("AND" "OR" "WITH")))
                     (or (spdx-string->license license)
                         license)))
              strings))

(define (hexpm-latest-release package)
  "Return the version string for the latest stable release of PACKAGE."
  ;; Use latest-stable if specified (see comment in hexpm-pkgdef above),
  ;; otherwise compare the lists of release versions.
  (let ((latest-stable (hexpm-latest-stable package)))
    (if (not (unspecified? latest-stable))
        latest-stable
        (let ((versions (map hexpm-version-number (hexpm-versions package))))
          (fold (lambda (a b)
                  (if (version>? a b) a b)) (car versions) versions)))))

(define* (hexpm->guix-package package-name #:key repo version)
  "Fetch the metadata for PACKAGE-NAME from hexpms.io, and return the
`package' s-expression corresponding to that package, or #f on failure.
When VERSION is specified, attempt to fetch that version; otherwise fetch the
latest version of PACKAGE-NAME."

  (define package
    (lookup-hexpm package-name))

  (define version-number
    (and package
         (or version
             (hexpm-latest-release package))))

  (define version*
    (and package
         (find (lambda (version)
                 (string=? (hexpm-version-number version)
                           version-number))
               (hexpm-versions package))))

  (define release
    (and package version*
         (lookup-hexpm-release version*)))

  (define release-meta
    (and package version*
         (hexpm-release-meta release)))

  (define build-system
    (and package version*
         (let ((build-tools (hexpm-release-meta-build-tools release-meta)))
           (cond
            ((member "rebar3" build-tools) 'rebar-build-system)
            ((member "mix" build-tools) 'mix-build-system)
            ((member "make" build-tools) 'gnu-build-system)
            (else #f)))))

  (define language
    (and package version*
         (let ((elixir (hexpm-release-meta-elixir release-meta)))
           (cond
            ((and (string? elixir) (not (string-null? elixir))) "elixir")
            (else "erlang")))))

  (and package version*
       (let ((dependencies  (hexpm-release-dependencies release))
             (pkg-meta      (hexpm-meta package))
             (docs-html-url (hexpm-docs-html-url package)))
         (values
          (make-hexpm-sexp
           #:language language
           #:build-system build-system
           #:name package-name
           #:version version-number
           #:dependencies dependencies
           #:home-page (or (and (not (eq? docs-html-url 'null))
                                docs-html-url)
                           ;; TODO: Homepage?
                           (hexpm-html-url package))
           #:synopsis (hexpm-meta-description pkg-meta)
           #:description (hexpm-meta-description pkg-meta)
           #:license (or (and=> (hexpm-meta-licenses pkg-meta)
                                strings->licenses))
           #:tarball-url (hexpm-uri package-name version-number))
          dependencies))))

(define* (hexpm-recursive-import pkg-name #:optional version)
  (recursive-import pkg-name
                    #:version version
                    #:repo->guix-package hexpm->guix-package
                    #:guix-name hexpm-name->package-name))

(define (guix-package->hexpm-name package)
  "Return the hex.pm name of PACKAGE."
  (define (url->hexpm-name url)
    (hyphen-package-name->name+version
     (basename (file-sans-extension url))))

  (match (and=> (package-source package) origin-uri)
    ((? string? url)
     (url->hexpm-name url))
    ((lst ...)
     (any url->hexpm-name lst))
    (#f #f)))

(define* (hexpm-name->package-name name #:optional (language "erlang"))
  (string-append language "-" (string-join (string-split name #\_) "-")))


;;;
;;; Updater
;;;

(define (latest-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."
  (let* ((hexpm-name (guix-package->hexpm-name package))
         (hexpm      (lookup-hexpm hexpm-name))
         (version    (hexpm-latest-release hexpm))
         (url        (hexpm-uri hexpm-name version)))
    (upstream-source
     (package (package-name package))
     (version version)
     (urls (list url)))))

(define %hexpm-updater
  (upstream-updater
   (name 'hexpm)
   (description "Updater for hex.pm packages")
   (pred (url-prefix-predicate hexpm-package-url))
   (latest latest-release)))
