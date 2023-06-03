;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix import gem)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module ((guix build-system ruby) #:select (rubygems-uri))
  #:export (gem->guix-package
            %gem-updater
            gem-recursive-import))

;; Gems as defined by the API at <https://rubygems.org/api/v1/gems>.
(define-json-mapping <gem> make-gem gem?
  json->gem
  (name          gem-name)                        ;string
  (platform      gem-platform)                    ;string
  (version       gem-version)                     ;string
  (authors       gem-authors)                     ;string
  (licenses      gem-licenses "licenses"          ;list of strings
                 (lambda (licenses)
                   ;; This is sometimes #nil (the JSON 'null' value).  Arrange
                   ;; to always return a list.
                   (cond ((not licenses) '())
                         ((unspecified? licenses) '())
                         ((vector? licenses) (vector->list licenses))
                         (else '()))))
  (info          gem-info)
  (sha256        gem-sha256 "sha"                 ;bytevector
                 base16-string->bytevector)
  (home-page     gem-home-page "homepage_uri")    ;string
  (dependencies  gem-dependencies "dependencies"  ;<gem-dependencies>
                 json->gem-dependencies))

(define-json-mapping <gem-dependencies> make-gem-dependencies
  gem-dependencies?
  json->gem-dependencies
  (development   gem-dependencies-development     ;list of <gem-dependency>
                 "development"
                 json->gem-dependency-list)
  (runtime       gem-dependencies-runtime         ;list of <gem-dependency>
                 "runtime"
                 json->gem-dependency-list))

(define (json->gem-dependency-list vector)
  (if (and vector (not (unspecified? vector)))
      (map json->gem-dependency (vector->list vector))
      '()))

(define-json-mapping <gem-dependency> make-gem-dependency gem-dependency?
  json->gem-dependency
  (name          gem-dependency-name)             ;string
  (requirements  gem-dependency-requirements))    ;string


(define* (rubygems-fetch name #:optional version)
  "Return a <gem> record for the package NAME and VERSION, or #f on failure.  If VERSION is #f or missing, return the latest version gem."
  (and=> (json-fetch
          (if version
              (string-append "https://rubygems.org/api/v2/rubygems/" name "/versions/" version ".json")
              (string-append "https://rubygems.org/api/v1/gems/" name ".json")))
         json->gem))

(define (ruby-package-name name)
  "Given the NAME of a package on RubyGems, return a Guix-compliant name for
the package."
  (if (string=? name "bundler")
      name                                        ;special case: no prefix
      (if (string-prefix? "ruby-" name)
          (snake-case name)
          (string-append "ruby-" (snake-case name)))))

(define (make-gem-sexp name version hash home-page synopsis description
                       dependencies licenses)
  "Return the `package' s-expression for a Ruby package with the given NAME,
VERSION, HASH, HOME-PAGE, DESCRIPTION, DEPENDENCIES, and LICENSES."
  `(package
     (name ,(ruby-package-name name))
     (version ,version)
     (source (origin
               (method url-fetch)
               (uri (rubygems-uri ,name version))
               (sha256
                (base32
                 ,(bytevector->nix-base32-string hash)))))
     (build-system ruby-build-system)
     ,@(if (null? dependencies)
           '()
           `((propagated-inputs
              (list ,@(map string->symbol dependencies)))))
     (synopsis ,synopsis)
     (description ,description)
     (home-page ,home-page)
     (license ,(match licenses
                 (() #f)
                 ((license) (license->symbol license))
                 (_ `(list ,@(map license->symbol licenses)))))))

(define* (gem->guix-package package-name #:key (repo 'rubygems) version
                            #:allow-other-keys)
  "Fetch the metadata for PACKAGE-NAME from rubygems.org, and return the
`package' s-expression corresponding to that package, or #f on failure.
Optionally include a VERSION string to fetch a specific version gem."
  (let ((gem (if version
                 (rubygems-fetch package-name version)
                 (rubygems-fetch package-name))))
    (if gem
        (let* ((dependencies-names (map gem-dependency-name
                                        (gem-dependencies-runtime
                                         (gem-dependencies gem))))
               (dependencies (map ruby-package-name dependencies-names))
               (licenses     (map string->license (gem-licenses gem))))
          (values (make-gem-sexp (gem-name gem) (gem-version gem)
                                 (gem-sha256 gem) (gem-home-page gem)
                                 (gem-info gem)
                                 (beautify-description (gem-info gem))
                                 dependencies
                                 licenses)
                  dependencies-names))
        (values #f '()))))

(define (guix-package->gem-name package)
  "Given a PACKAGE built from rubygems.org, return the name of the
package on RubyGems."
  (let ((source-url (and=> (package-source package) origin-uri)))
    ;; The URL has the form:
    ;; 'https://rubygems.org/downloads/' +
    ;; package name + '-' + version + '.gem'
    ;; e.g. "https://rubygems.org/downloads/hashery-2.1.1.gem"
    (substring source-url 31 (string-rindex source-url #\-))))

(define (string->license str)
  "Convert the string STR into a license object."
  (match str
    ("GNU LGPL" license:lgpl2.0)
    ("GPL" license:gpl3)
    ((or "BSD" "BSD License") license:bsd-3)
    ((or "MIT" "MIT license" "Expat license") license:expat)
    ("Public domain" license:public-domain)
    ((or "Apache License, Version 2.0" "Apache 2.0") license:asl2.0)
    (_ #f)))

(define gem-package?
  (url-prefix-predicate "https://rubygems.org/downloads/"))

(define* (import-release package #:key (version #f))
  "Return an <upstream-source> for the latest release of PACKAGE."
  (let* ((gem-name (guix-package->gem-name package))
         (gem      (rubygems-fetch gem-name))
         (inputs   (map (lambda (dependency)
                          (let ((name (gem-dependency-name dependency)))
                            (upstream-input
                             (name name)
                             (downstream-name
                              (ruby-package-name name))
                             (type 'propagated))))
                        (gem-dependencies-runtime (gem-dependencies gem))))
         (version  (or version (gem-version gem)))
         (url      (rubygems-uri gem-name version)))
    (upstream-source
     (package (package-name package))
     (version version)
     (urls (list url))
     (inputs inputs))))

(define %gem-updater
  (upstream-updater
   (name 'gem)
   (description "Updater for RubyGem packages")
   (pred gem-package?)
   (import import-release)))

(define* (gem-recursive-import package-name #:optional version)
  (recursive-import package-name
                    #:repo '()
                    #:repo->guix-package gem->guix-package
                    #:guix-name ruby-package-name
                    #:version version))
