;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015-2017, 2019-2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2025 Danny Milosavljevic <dannym@friendly-machines.com>
;;; Copyright © 2025 Zheng Junjie <z572@z572.online>
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

(define-module (guix import nuget)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 binary-ports)
  #:use-module ((rnrs) #:select (put-bytevector))
  #:use-module ((sxml xpath) #:select (sxpath)) ; filter... grr
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34) ; For catch
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-39) ; parameters
  #:use-module (srfi srfi-71) ; multi-value let
  #:use-module (sxml simple)
  #:use-module (sxml match)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (semver)
  #:use-module (semver ranges)
  #:use-module (semver partial)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix http-client)
  #:use-module (guix memoization)
  #:use-module (guix utils)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (guix base32)
  #:use-module (guix build utils)
  #:use-module (guix git)
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix http-client)
  #:export (%nuget-v3-registration-url
            %nuget-v3-package-versions-url
            %nuget-symbol-packages-url
            nuget->guix-package
            nuget-recursive-import))

;; copy from guix/import/pypi.scm
(define non-empty-string-or-false
  (match-lambda
    ("" #f)
    ((? string? str) str)
    ((or 'null #f) #f)))

;;; See also <https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort>.
;;; Therefore, excluding prerelease and metadata labels, a version string is Major.Minor.Patch.Revision.
;;; As per version normalization described above, if Revision is zero, it is omitted from the normalized version string.
;;; TODO: NuGetVersion uses case insensitive string comparisons for pre-release components. This means that 1.0.0-alpha and 1.0.0-Alpha are equal.
;;; TODO: nuget-name->guix-name should also provide semantic major version; what about 0. ?
;;;
;;; NuGet considers a package version to be SemVer v2.0.0 specific, if either of the following is true:
;;; - The pre-release label is dot-separated, for example, 1.0.0-alpha.1
;;; - The version has build-metadata, for example, 1.0.0+githash

(define %nuget-v3-service-index-url "https://api.nuget.org/v3/index.json")
;; Example: <https://api.nuget.org/v3/registration5-semver1/newtonsoft.json/index.json>.
;; List of all packages.  You get a lot of references to @type CatalogPage out.
(define %nuget-v3-feed-catalog-url "https://api.nuget.org/v3/catalog0/index.json")
(define %nuget-v3-registration-url
  (make-parameter "https://api.nuget.org/v3/registration5-semver1/"))
(define %nuget-v3-package-versions-url
  (make-parameter "https://api.nuget.org/v3-flatcontainer/"))
(define %nuget-symbol-packages-url
  (make-parameter "https://globalcdn.nuget.org/symbol-packages/"))

(define %nuget-nuspec "http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd")
;;; Version index https://api.nuget.org/v3-flatcontainer/{id-lower}/index.json
;;; nupkg download url https://api.nuget.org/v3-flatcontainer/{id-lower}/{version-lower}/{id-lower}.{version-lower}.nupkg
;;; nuspec url https://api.nuget.org/v3-flatcontainer/{id-lower}/{version-lower}/{id-lower}.nuspec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1: Version Range Solver
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fancy false-if-exception.
(define* (string->semver/safe str)
  "A safe wrapper around 'string->semver'. Returns a semver object on
success or #f on failure, without crashing."
  (catch #t
    (lambda ()
      (string->semver str))
    (lambda (key . args)
      (warning (G_ "Not a semantic version: ~s.~%") str)
      #f)))

(define* (parse-nuget-range->primitives range-str)
  "Parses a NuGet-style version range string and returns a list of lists of
primitives suitable for the 'semver-range' constructor."
  (let ((str (string-trim range-str)))
    (let ((m-range (string-match "^([([])([^,]*),([^,]*)([])])$" str)))
      (if m-range
          (let* ((lower-bracket (match:substring m-range 1))
                 (min-ver-str   (string-trim (match:substring m-range 2)))
                 (max-ver-str   (string-trim (match:substring m-range 3)))
                 (upper-bracket (match:substring m-range 4)))
            (filter-map
             identity
             (list
              (if (string-null? min-ver-str)
                  #f
                  (let* ((op (if (string=? lower-bracket "[")
                                 '>=
                                 '>)))
                    (and=> (string->semver/safe min-ver-str)
                           (cute list op <>))))
              (if (string-null? max-ver-str)
                  #f
                  (let* ((op (if (string=? upper-bracket "]")
                                 '<=
                                 '<)))
                    (and=> (string->semver/safe max-ver-str)
                           (cute list op <>)))))))
          (let ((m-exact (string-match "^\\[([0-9][^]]*)\\]$" str)))
            (if m-exact
                (let ((slice (string->partial-semver (match:substring m-exact 1))))
                  (if slice (list `((= ,slice))) '()))
                (if (string-match "^[0-9.]+" str)
                    (let ((slice (string->partial-semver str)))
                      (if slice
                          `((>= ,slice))
                          '()))
                    (begin
                      (warning (G_ "Unrecognized NuGet range format: '~a'.~%") str)
                      '()))))))))

;; Make this testable.
(set! parse-nuget-range->primitives parse-nuget-range->primitives)

(define (nuget->semver-range range-str)
  (semver-range (parse-nuget-range->primitives range-str)))

(define* (nuget-find-best-version-for-range name range-str)
  "Given a package NAME and a version RANGE string, find the highest
  version that satisfies the range. This version correctly handles list
  creation and filtering to avoid type errors."
  (let* ((name-lower (string-downcase name))
         (versions-url (string-append (%nuget-v3-package-versions-url) name-lower "/index.json")))
    (let ((versions-json (json-fetch versions-url)))
      (if versions-json
          (let* ((available-versions (vector->list (or (assoc-ref versions-json "versions")
                                                       #())))
                 (semver-range (nuget->semver-range range-str)))
            ;; Create a single, clean list of all valid semver objects first.
            (let ((all-valid-semvers (filter-map string->semver/safe available-versions)))
              ;; First, try to find a matching stable version from this list.
              (let* ((stable-semvers (filter (lambda (sv)
                                               (null? (semver-pre sv)))
                                             all-valid-semvers))
                     (matching-stable (filter (lambda (sv)
                                                (semver-range-contains? semver-range sv))
                                              stable-semvers)))
                (if (not (null? matching-stable))
                    ;; Success: Found a stable version.
                    (semver->string (car (sort matching-stable semver>?)))

                    ;; Fallback: No stable version matched. Check pre-releases.
                    (begin
                      (warning (G_ "No stable version of ~a matches '~a'. Checking for pre-releases...~%") name range-str)
                      (let ((matching-all (filter
                                           (lambda (sv)
                                             (semver-range-contains? semver-range sv #:include-prerelease? #t))
                                           all-valid-semvers)))
                        (if (not (null? matching-all))
                            (semver->string (car (sort matching-all semver>?)))
                            (begin
                              (warning (G_ "No suitable stable or pre-release version found.~%"))
                              #f))))))))
          (begin
            (warning (G_ "Failed to fetch version list for ~a~%") name)
            #f)))))

;; Make this testable.
(set! nuget-find-best-version-for-range nuget-find-best-version-for-range)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 2: Core Data Fetching and Package Generation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (nuget-fetch-catalog-entry name version)
  "Fetch the full 'catalogEntry' JSON object for a specific package version,
  correctly handling the paginated structure of the registration index."
  (let* ((name-lower (string-downcase name))
         (index-url (string-append (%nuget-v3-registration-url) name-lower "/index.json"))
         (index-json (json-fetch index-url)))
    (if index-json
        (let loop ((pages-to-check
                    ;; Get the list of page objects.
                    (vector->list (or (assoc-ref index-json "items")
                                      #()))))
          (if (null? pages-to-check)
              ;; If we've checked all pages and found nothing, fail.
              (begin
                (warning (G_ "Could not find catalog entry for ~a version ~a in any page.~%") name version)
                #f)
              (let* ((current-page (car pages-to-check))
                     ;; Get the items for the current page. This may require another
                     ;; network request if the items are not inlined.
                     (page-items
                      (or (and=> (assoc-ref current-page "items")
                                 vector->list)
                          (let ((page-json (json-fetch (assoc-ref current-page "@id"))))
                            (if page-json
                                (vector->list (or (assoc-ref page-json "items")
                                                  #()))
                                '())))))
                ;; Search for our specific version within THIS page's items.
                (let ((entry-in-page
                       (find (lambda (item)
                               (and=>
                                (and=> (assoc-ref item "catalogEntry")
                                       (cut assoc-ref <> "version"))
                                (lambda (v) (string=? v version)))) ; fixme semver equal
                             page-items)))
                  (if entry-in-page
                      ;; Found it! Return the catalogEntry object.
                      (assoc-ref entry-in-page "catalogEntry")
                      ;; Not in this page, recurse to the next page.
                      (loop (cdr pages-to-check)))))))
        (begin
          (warning (G_ "Failed to fetch registration index for ~a~%") name)
          #f))))

;; Make this testable.
(set! nuget-fetch-catalog-entry nuget-fetch-catalog-entry)

(define (car-safe lst)
  (if (null? lst)
      '()
      (car lst)))

(define* (fetch-repo-info-from-snupkg package-name version)
  "Fetch the .snupkg file for PACKAGE-NAME and VERSION, extract the .nuspec
file using the system 'unzip' command, and parse it to find the repository URL
and commit.  Returns an association list with 'url' and 'commit' keys on
success, or #f on failure."
  (let* ((name (string-append (string-downcase package-name) "." version ".snupkg"))
         (snupkg-url (string-append (%nuget-symbol-packages-url) name)))
    (format (current-error-port)
            "~%;; Source repository not found in NuGet catalog entry.~%;; ~
             Attempting to find it in symbol package: ~a~%"
            snupkg-url)
    (catch #t
      (lambda ()
        (guard (c ((http-get-error? c)
                   (warning (G_ "Failed to download: ~a~%")
                            (uri->string (http-get-error-uri c)))
                   #f))
          (let* ((port (http-fetch snupkg-url))
                 (body (get-bytevector-all port)))
            (call-with-temporary-directory
             (lambda (dir)
               (with-directory-excursion dir
                 (call-with-output-file name
                   (cut put-bytevector <> body))
                 (invoke "unzip" "-q" name "-d" dir)
                 (let ((nuspec-files (find-files dir "\\.nuspec$")))
                   (chmod (car nuspec-files) #o400) ; some have 000 (example: flurl)
                   (if (null? nuspec-files)
                       (begin
                         (warning (G_ "No .nuspec file found in ~a~%") snupkg-url)
                         #f)
                       (call-with-input-file (car nuspec-files)
                         (lambda (port)
                           (let* ((sxml
                                   (xml->sxml
                                    port
                                    #:trim-whitespace? #t
                                    #:namespaces `((#f . ,%nuget-nuspec)))))
                             (car-safe
                              (filter-map
                               (lambda (node)
                                 (sxml-match node
                                   ((repository
                                     (@ . ,attrs-raw)
                                     . ,_)
                                    (let ((attrs (map (lambda (attr)
                                                        (cons (symbol->string (car attr))
                                                              (cadr attr)))
                                                      attrs-raw)))
                                      (if (or (assoc-ref attrs "url")
                                              (assoc-ref attrs "commit"))
                                          attrs
                                          #f)))
                                   (,otherwise #f)))
                               ((sxpath '(// metadata repository))
                                sxml))))))))))))))
      (lambda (key . args)
        (warning (G_ "Failed to fetch or process snupkg file: ~a (Reason: ~a ~s)~%")
                 snupkg-url key args)
        '()))))

(define (nuget-name->guix-name name)
  (string-append "dotnet-" (snake-case name)))

;; Make this testable.
(set! nuget-name->guix-name nuget-name->guix-name)

(define nuget->guix-package
  (memoize
   (lambda* (package-name #:key (repo 'nuget) (version #f) (license-prefix identity) #:allow-other-keys)
     "Fetch metadata for PACKAGE-NAME and return two values: the Guix package
s-expression and a flat list of its dependency names (strings)."
     (let ((resolved-version (or version
                                 (nuget-find-best-version-for-range package-name
                                                                    "[0.0.0,)"))))
       (if (not resolved-version)
           (values #f '())
           (let ((entry (nuget-fetch-catalog-entry package-name resolved-version)))
             (if (not entry)
                 (values #f '())
                 (let* ((name (assoc-ref entry "id"))
                        (guix-name (nuget-name->guix-name name))
                        (description (beautify-description
                                      (non-empty-string-or-false
                                       (assoc-ref entry "description"))))
                        (synopsis (beautify-synopsis
                                   (non-empty-string-or-false
                                    (assoc-ref entry "summary"))))
                        (home-page (or (assoc-ref entry "projectUrl")
                                       (string-append "https://www.nuget.org/packages/" name)))
                        (license (license-prefix (or (and=> (assoc-ref entry "licenseExpression")
                                                            string->symbol)
                                                     'unspecified)))
                                        ;(repository (assoc-ref entry "repository"))
                        (repository
                         (let ((repo-from-catalog (assoc-ref entry "repository")))
                           (if (and (list? repo-from-catalog)
                                    (let ((url (assoc-ref repo-from-catalog "url")))
                                      (and (string? url)
                                           ;; For example <https://api.nuget.org/v3/catalog0/data/2023.03.08.07.46.17/newtonsoft.json.13.0.3.json>
                                           ;; has "repository" "".
                                           (not (string-null? url))))
                                    (assoc-ref repo-from-catalog "commit"))
                               repo-from-catalog
                               (fetch-repo-info-from-snupkg name resolved-version))))
                        (dependencies (delete-duplicates
                                       (flatten
                                        (filter-map
                                         (lambda (group)
                                           (map (lambda (dep) (assoc-ref dep "id"))
                                                (vector->list (or
                                                               (assoc-ref group "dependencies")
                                                               #()))))
                                         (or (and=> (assoc-ref entry "dependencyGroups")
                                                    vector->list)
                                             '()))))))
                   (let ((package-sexp
                          `(package
                             (name ,guix-name)
                             (version ,resolved-version)
                             (source
                              ,(if (and repository (assoc-ref repository "url")
                                        (assoc-ref repository "commit"))
                                   `(origin (method git-fetch)
                                            (uri (git-reference
                                                  (url ,(assoc-ref repository "url"))
                                                  (commit ,(assoc-ref repository "commit"))))
                                            (file-name (git-file-name name version))
                                            (sha256 (base32 "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5")))
                                   `(origin (method url-fetch)
                                            (uri "FIXME: No source URL found.")
                                            (sha256 (base32 "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5")))))
                             (build-system mono-build-system)
                             ,@(maybe-inputs
                                (map nuget-name->guix-name dependencies))
                             (home-page ,home-page)
                             (synopsis ,synopsis)
                             (description ,description)
                             (license ,license))))
                     (values package-sexp dependencies))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 3: Recursive Importer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (nuget-recursive-import package-name #:key (repo 'nuget) version
                                 (license-prefix identity))
  "Recursively import PACKAGE-NAME and all its dependencies from NuGet. This
procedure is a simple wrapper around the generic 'recursive-import' helper."
  (recursive-import package-name
                    #:version version
                    #:repo repo
                    #:repo->guix-package nuget->guix-package
                    #:guix-name nuget-name->guix-name
                    #:license-prefix license-prefix))
