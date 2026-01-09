;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (test-nuget)
  #:use-module (guix import nuget)
  #:use-module (guix tests)
  #:use-module (guix tests http)
  #:use-module (guix http-client)
  #:use-module (json)
  #:use-module (semver)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71)
  #:use-module (web uri))

(define (make-versions-json versions)
  "Generate a NuGet package versions index JSON string."
  (scm->json-string
   `((versions . ,(list->vector versions)))))

(define (make-catalog-entry id version description summary home-page license
                            repo-url repo-commit dependencies)
  "Generate a NuGet catalog entry alist."
  `((id . ,id)
    (version . ,version)
    (description . ,description)
    (summary . ,summary)
    (projectUrl . ,home-page)
    (licenseExpression . ,license)
    ,@(if repo-url
          `((repository . ((url . ,repo-url)
                           (commit . ,repo-commit))))
          '())
    (dependencyGroups
     . ,(list->vector
         (if (null? dependencies)
             '()
             `(((targetFramework . "net6.0")
                (dependencies
                 . ,(list->vector
                     (map (lambda (dep)
                            `((id . ,(car dep))
                              (range . ,(cdr dep))))
                          dependencies))))))))))

(define (make-registration-index-json catalog-entry)
  "Generate a NuGet registration index JSON string."
  (scm->json-string
   `((items
      . #(((items
            . #(((catalogEntry . ,catalog-entry))))))))))

(define test-avalonia-versions-json
  (make-versions-json '("0.10.0" "0.10.1" "11.0.0" "11.0.1" "11.1.0")))

(define test-avalonia-catalog-entry
  (make-catalog-entry "Avalonia" "11.1.0"
                      "A cross-platform UI framework for .NET"
                      "Avalonia UI Framework"
                      "https://avaloniaui.net/"
                      "MIT"
                      "https://github.com/AvaloniaUI/Avalonia.git"
                      "abc123def456"
                      '(("System.Text.Json" . "[6.0.0, )"))))

(define test-avalonia-index-json
  (make-registration-index-json test-avalonia-catalog-entry))

(define test-system-text-json-versions-json
  (make-versions-json '("6.0.0" "6.0.1" "7.0.0" "8.0.0")))

(define test-system-text-json-catalog-entry
  (make-catalog-entry "System.Text.Json" "8.0.0"
                      "Provides high-performance JSON APIs"
                      "JSON library"
                      "https://dot.net/"
                      "MIT"
                      "https://github.com/dotnet/runtime.git"
                      "def789abc012"
                      '()))

(define test-system-text-json-index-json
  (make-registration-index-json test-system-text-json-catalog-entry))

(define test-package-no-repo-versions-json
  (make-versions-json '("1.0.0")))

(define test-package-no-repo-catalog-entry
  (make-catalog-entry "TestPackage" "1.0.0"
                      "Test package without repository"
                      #f
                      "https://example.com/"
                      "MIT"
                      #f #f
                      '()))

(define test-package-no-repo-index-json
  (make-registration-index-json test-package-no-repo-catalog-entry))

(define-syntax-rule (with-nuget responses body ...)
  (with-http-server responses
    (parameterize ((%nuget-v3-package-versions-url
                    (%local-url #:path "/versions/"))
                   (%nuget-v3-registration-url
                    (%local-url #:path "/registration/"))
                   (%nuget-symbol-packages-url
                    (%local-url #:path "/symbols/")))
      body ...)))

(test-begin "nuget")

(test-assert "nuget->guix-package"
  ;; Replace network resources with sample data.
  (with-nuget `(("/versions/avalonia/index.json" 200 ,test-avalonia-versions-json)
                ("/registration/avalonia/index.json" 200 ,test-avalonia-index-json))
    (let ((package-sexp dependencies (nuget->guix-package "Avalonia")))
      (match package-sexp
        (`(package
            (name "dotnet-avalonia")
            (version "11.1.0")
            (source
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/AvaloniaUI/Avalonia.git")
                     (commit "abc123def456")))
               (file-name (git-file-name name version))
               (sha256 (base32 ,(?  string?)))))
            (build-system mono-build-system)
            (inputs (list dotnet-system-text-json))
            (home-page "https://avaloniaui.net/")
            (synopsis ,(?  string?))
            (description ,(?  string?))
            (license ,?))
         (equal? dependencies '("System.Text.Json")))
        (x
         (pk 'fail x #f))))))

(test-assert "nuget-name->guix-name"
  (and (string=? ((@@ (guix import nuget) nuget-name->guix-name) "Avalonia")
                 "dotnet-avalonia")
       (string=? ((@@ (guix import nuget) nuget-name->guix-name) "System.Text.Json")
                 "dotnet-system-text-json")))

(test-assert "nuget-recursive-import"
  ;; Replace network resources with sample data.
  ;; recursive-import returns a list of package s-expressions in topological order.
  (with-nuget `(("/versions/avalonia/index.json" 200 ,test-avalonia-versions-json)
                ("/registration/avalonia/index.json" 200 ,test-avalonia-index-json)
                ("/versions/system.text.json/index.json" 200
                 ,test-system-text-json-versions-json)
                ("/registration/system.text.json/index.json" 200
                 ,test-system-text-json-index-json))
    (let ((packages (nuget-recursive-import "Avalonia")))
      (match packages
        ((first second)
         ;; Check that we got two packages
         (and (match first
                (`(package (name ,name1) . ,_)
                 (or (string=? name1 "dotnet-system-text-json")
                     (string=? name1 "dotnet-avalonia"))))
              (match second
                (`(package (name ,name2) . ,_)
                 (or (string=? name2 "dotnet-system-text-json")
                     (string=? name2 "dotnet-avalonia"))))))
        (x
         (pk 'fail-recursive-count x #f))))))

(test-assert "parse-nuget-range->primitives: exact version"
  (let ((result ((@@ (guix import nuget) parse-nuget-range->primitives) "[1.0.0]")))
    (match result
      (((('= slice)))
       (equal? slice '(1 0 0 0 () ())))
      (_ #f))))

(test-assert "parse-nuget-range->primitives: minimum version"
  (let ((result ((@@ (guix import nuget) parse-nuget-range->primitives) "1.0.0")))
    (match result
      ((('>= slice))
       (equal? slice '(1 0 0 0 () ())))
      (_ #f))))

(test-assert "parse-nuget-range->primitives: range with brackets"
  (let ((result ((@@ (guix import nuget) parse-nuget-range->primitives) "[1.0.0,2.0.0]")))
    (match result
      ((('>= sv1) ('<= sv2))
       (and (semver? sv1)
            (semver? sv2)
            (string=? (semver->string sv1) "1.0.0")
            (string=? (semver->string sv2) "2.0.0")))
      (_ #f))))

(test-assert "parse-nuget-range->primitives: range with parens"
  (let ((result ((@@ (guix import nuget) parse-nuget-range->primitives) "(1.0.0,2.0.0)")))
    (match result
      ((('> sv1) ('< sv2))
       (and (semver? sv1)
            (semver? sv2)
            (string=? (semver->string sv1) "1.0.0")
            (string=? (semver->string sv2) "2.0.0")))
      (_ #f))))

(test-assert "parse-nuget-range->primitives: open-ended range"
  (let ((result ((@@ (guix import nuget) parse-nuget-range->primitives) "[1.0.0, )")))
    (match result
      ((('>= sv))
       (and (semver? sv)
            (string=? (semver->string sv) "1.0.0")))
      (_ #f))))

(test-assert "nuget-find-best-version-for-range: stable version"
  ;; Test that it finds the highest stable version matching a range
  (with-nuget `(("/versions/avalonia/index.json" 200 ,test-avalonia-versions-json))
    (let ((version ((@@ (guix import nuget) nuget-find-best-version-for-range)
                    "Avalonia" "[11.0.0,)")))
      (string=? version "11.1.0"))))

(test-assert "nuget-find-best-version-for-range: closed range"
  (with-nuget `(("/versions/avalonia/index.json" 200 ,test-avalonia-versions-json))
    (let ((version ((@@ (guix import nuget) nuget-find-best-version-for-range)
                    "Avalonia" "[11.0.0,12.0.0]")))
      (string=? version "11.1.0"))))

(test-assert "nuget-fetch-catalog-entry: finds specific version"
  (with-nuget `(("/registration/avalonia/index.json" 200 ,test-avalonia-index-json))
    (let ((entry ((@@ (guix import nuget) nuget-fetch-catalog-entry)
                  "Avalonia" "11.1.0")))
      (and entry
           (string=? (assoc-ref entry "version") "11.1.0")
           (string=? (assoc-ref entry "id") "Avalonia")))))

(test-assert "nuget->guix-package: package without repository"
  ;; Test package with no repository info (source should have FIXME)
  (with-nuget `(("/versions/testpackage/index.json" 200
                 ,test-package-no-repo-versions-json)
                ("/registration/testpackage/index.json" 200
                 ,test-package-no-repo-index-json)
                ("/symbols/testpackage.1.0.0.snupkg" 404 ""))
    (let ((package-sexp dependencies (nuget->guix-package "TestPackage")))
      (match package-sexp
        (`(package
            (name "dotnet-testpackage")
            (version "1.0.0")
            (source
             (origin
               (method url-fetch)
               (uri "FIXME: No source URL found.")
               . ,_))
            . ,_)
         (equal? dependencies '()))
        (x
         (pk 'fail-no-repo x #f))))))

(test-end "nuget")
