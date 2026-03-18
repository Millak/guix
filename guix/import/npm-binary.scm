;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2020, 2023, 2024 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>
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
  #:use-module (guix build json-utils)
  #:use-module (guix http-client)
  #:use-module ((guix import git) #:select (get-tags))
  #:use-module (guix import json)
  #:use-module (guix import utils)
  #:use-module (guix memoization)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (json)
  ;; Autoload Guile-Semver so we only have a soft dependency.
  #:autoload (semver) (string->semver semver? semver->string semver=? semver>?)
  #:autoload (semver ranges) ( *semver-range-any* string->semver-range
                               semver-range-contains?)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-9)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (npm-binary-recursive-import
            npm-binary->guix-package
            %npm-registry
            make-versioned-package
            name+version->symbol))

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

(define (string-or-#f s)
  (and (string? s) s))

(define-json-mapping <repository> make-repository repository?
  json->repository
  (type repository-type "type" string-or-#f)
  (url repository-url "url" string-or-#f))

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
                (_ #f)))
             ((? vector? vector)
              (match (filter-map
                      (match-lambda
                        ((? string? str) (spdx-string->license str))
                        (_ #f))
                      (vector->list vector))
                ((license rest ...)
                 (cons* license rest))
                ((license)
                 license)))))
  (description package-revision-description             ;string
               "description" empty-or-string)
  (dist package-revision-dist "dist" json->dist)
  (repository package-revision-repository "repository" ;<repository>
              (lambda (value)
                ;; Guard against historical values such as strings or anything
                ;; else.
                (match value
                  ((? alist?)
                   (json->repository value))
                  ((? unspecified?) #f)
                  ((? string? url)
                   (make-repository "git" url))
                  (_ #f)))))

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
  (and-let*
      ((meta (lookup-meta-package* name))
       (version (semver-latest (or (meta-package-versions meta) '()) svr))
       (pkg (meta-package-package meta version)))
    pkg))


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

(define* (npm-package->package-sexp npm-package #:key binary?)
  "Return the `package' s-expression for an NPM-PACKAGE.  Prefer the binary
distribution URL as the source origin when BINARY? is #t, otherwise use the
associated Git repository, if available."
  (define resolve-spec
    (match-lambda
      (($ <versioned-package> name version)
       (let ((pkg (resolve-package name (string->semver-range version))))
         (unless pkg
           (warning (G_ "failed to resolve dependency ~a~%") name))
         pkg))))

  (define (github-hosted? url)
    (let ((uri (string->uri url)))
      (string=? "github.com" (uri-host uri))))

  (define (sanitize-home-page-url url)
    ;; Strip #readme from a github home page URL if any.
    (if (and (github-hosted? url)
             (string-suffix? "#readme" url))
        (string-drop-right url 7)
        url))

  (define (normalize-git-url url)
    (let ((url (cond
                ((string-prefix? "git+https://" url)
                 (string-replace-substring url "git+https://" "https://"))
                ((string-prefix? "git+ssh://" url)
                 (string-replace-substring url "git+ssh://" "https://"))
                ((string-prefix? "git://" url)
                 (string-replace-substring url "git://" "https://"))
                (else url))))
      (if (and (github-hosted? url)
               (string-suffix? ".git" url))
          (string-drop-right url 4)
          url)))

  (define (sexpify-url/maybe dist-url name version)
    ;; Return a S-exp for the package URL, which is computed using the package
    ;; version, if it matches the distribution tarball DIST-URL.

    ;; The newer "scoped" packages use a different scheme for their dist URL
    ;; (see: <https://docs.npmjs.com/about-scopes>).
    (let* ((scope name (if (and (string-prefix? "@" name)
                                (string-contains name "/"))
                           (apply values (string-split name #\/))
                           (values #f name)))
           (versioned-url (string-append (%npm-registry)
                                         (if scope
                                             (string-append "/" scope "/")
                                             "/")
                                         name "/-/" name "-" version ".tgz")))
      (if (string=? dist-url versioned-url)
          `(string-append ,(string-append (%npm-registry)
                                          (if scope
                                              (string-append "/" scope "/")
                                              "/")
                                          name)
                          ,(string-append "/-/" name "-")  version ".tgz")
          dist-url)))

  (define (sexpify-git-tag/maybe tag version)
    ;; Try to find a string-append pattern to build TAG from VERSION.  For example,
    ;;
    ;; (sexpify-git-tag/maybe "3.0.0v3.0.0v3.0.0" "3.0.0")
    ;;   => '(string-append version "v" version "v" version)
    (match (string-split (string-replace-substring
                          tag version ":")
                         #\:)
      (("" "")                          ;tag == version
       'version)
      ((prefix "")
       `(string-append ,prefix version))
      (("" suffix)
       `(string-append version ,suffix))
      ((component components* ..1)
       ;; Intersperse 'version between each component.
       (let ((interspersed (append
                            (append-map
                             (lambda (x)
                               (list x 'version))
                             (drop-right `(,component ,@components*) 1))
                            (list (last components*)))))
         `(string-append ,@(remove (lambda (x)
                                     (and (string? x)
                                          (string-null? x)))
                                   interspersed))))
      (_ tag)))

  (match npm-package
    (($ <package-revision>
        name version home-page dependencies dev-dependencies
        peer-dependencies license description dist repository)
     (let* ((version-string (semver->string
                             (package-revision-version npm-package)))
            (dist-url (dist-tarball dist))
            (url (sexpify-url/maybe dist-url name version-string))
            (git-url? (and (not binary?) repository
                           (and=> (repository-type repository)
                                  (cut string=? "git" <>))))
            (git-url (and git-url? (and=> (repository-url repository)
                                          normalize-git-url)))
            (git-tag (and git-url
                          (and=> (false-if-exception (get-tags git-url))
                                 (cut assoc-ref <> version-string))))
            (use-git-url? (and git-url git-tag))
            (name (npm-name->name name))
            (home-page (if (string? home-page)
                           (sanitize-home-page-url home-page)
                           (string-append %default-page "/" (uri-encode name))))
            (synopsis description)
            (resolved-deps (filter-map resolve-spec
                                       (append dependencies peer-dependencies)))
            (peer-names (map versioned-package-name peer-dependencies))
            (dev-names (append (map versioned-package-name dev-dependencies)
                               peer-names))
            (extra-phases
             (append
              (if binary?
                  '((delete 'build))
                  '())
              (match dev-names
                (() '())
                ((dev-names ...)
                 `((add-after 'patch-dependencies 'delete-dev-dependencies
                     (lambda _
                       (modify-json (delete-dev-dependencies))))))))))
       (values
        `(package
           (name ,name)
           (version ,version-string)
           (source ,(if use-git-url?
                        (git->origin git-url sexpify-git-tag/maybe
                                     #:ref (cons 'tag git-tag)
                                     #:eager? #t
                                     git-tag version-string)
                        `(origin
                           (method url-fetch)
                           (uri ,url)
                           (sha256 (base32 ,(hash-url dist-url))))))
           (build-system node-build-system)
           (arguments
            (list
             #:tests? #f
             ,@(match extra-phases
                 (() '())
                 ((phase ..1)
                  `(#:phases
                    #~(modify-phases %standard-phases
                        ,@extra-phases))))))
           ,@(match dependencies
               (() '())
               ((dependencies ...)
                `((inputs
                   (list ,@(map package-revision->symbol resolved-deps))))))
           (home-page ,home-page)
           (synopsis ,(beautify-synopsis synopsis))
           (description ,(beautify-description description))
           (license ,(if (list? license)
                         `(list ,@license)
                         license)))
        (map (match-lambda (($ <package-revision> name version)
                            (list name (semver->string version))))
             resolved-deps))))
    (_
     (values #f '()))))


;;;
;;; Interface
;;;

(define npm-binary->guix-package
  (lambda* (name #:key (version *semver-range-any*) binary? #:allow-other-keys)
    (let* ((svr (match version
                  ((? string?) (string->semver-range version))
                  (_ version)))
           (pkg (resolve-package name svr)))
      (npm-package->package-sexp pkg #:binary? binary?))))

(define* (npm-binary-recursive-import package-name #:key version binary?)
  (recursive-import package-name
                    #:repo->guix-package (memoize npm-binary->guix-package)
                    #:version version
                    #:guix-name npm-name->name
                    #:binary? binary?))
