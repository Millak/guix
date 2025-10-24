;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2022, 2023, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
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

(define-module (test-import-utils)
  #:use-module (guix tests)
  #:use-module ((guix diagnostics) #:select (location))
  #:use-module (guix import utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix tests git)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match))

(test-begin "import-utils")

(test-equal "beautify-description: empty string"
  "This package lacks a description.  Run \
\"info '(guix) Synopses and Descriptions'\" for more information."
  (beautify-description ""))

(test-equal "beautify-description: not a string"
  "This package lacks a description.  Run \
\"info '(guix) Synopses and Descriptions'\" for more information."
  (beautify-description '()))

(test-equal "beautify-description: use double spacing"
  "\
Trust me Mr. Hendrix, M. Night Shyamalan et al.  \
Differences are hard to spot,
e.g. in CLOS vs. GOOPS."
  (beautify-description
   "
Trust me Mr. Hendrix, M. Night Shyamalan et al. \
Differences are hard to spot, e.g. in CLOS vs. GOOPS."))

(test-equal "beautify-description: transform fragment into sentence"
  "This package provides a function to establish world peace."
  (beautify-description "A function to establish world peace"))

(test-equal "beautify-description: remove single quotes"
  "CRAN likes to quote acronyms and function names."
  (beautify-description "CRAN likes to 'quote' acronyms and 'function' names."))

(test-equal "beautify-description: escape @"
  "This @@ is not Texinfo syntax.  Neither is this %@@>%."
  (beautify-description "This @ is not Texinfo syntax.  Neither is this %@>%."))

(test-equal "beautify-description: escape @stuff"
  "This is not valid syntax: @code{@@importFrom} oh dear."
  (beautify-description "This is not valid syntax: @importFrom oh dear."))

(test-equal "beautify-description: wrap PascalCase words in @code"
  "The term @code{DelayedMatrix} refers to a class."
  (beautify-description "The term DelayedMatrix refers to a class."))

(test-equal "beautify-description: do not wrap acronyms in @code"
  "The term API is not code, but @code{myAPI} might be."
  (beautify-description "The term API is not code, but myAPI might be."))

(test-equal "beautify-description: do not include punctuation when wrapping in @code"
  "Code (@code{DelayedMatrix}, @code{MaMa}, or @code{MeMe}) should be wrapped."
  (beautify-description "Code (DelayedMatrix, MaMa, or MeMe) should be wrapped."))

(test-equal "beautify-description: wrap function names in @code"
  "The main functions are: @code{haplo.em()} and @code{haplo.power()}; FYI."
  (beautify-description "The main functions are: haplo.em() and haplo.power(); FYI."))

(test-equal "beautify-synopsis: escape @"
  "This is invalid @@syntax"
  (beautify-synopsis "This is invalid @syntax"))

(test-equal "beautify-synopsis: escape @"
  "Just an @@ in the middle"
  (beautify-synopsis "Just an @ in the middle"))

(test-equal "license->symbol"
  'license:lgpl2.0
  (license->symbol license:lgpl2.0))

(test-equal "recursive-import"
  '((package                         ;package expressions in topological order
      (name "bar"))
    (package
      (name "foo")
      (inputs `(("bar" ,bar)))))
  (recursive-import "foo"
                    #:repo 'repo
                    #:repo->guix-package
                    (match-lambda*
                      (("foo" #:repo 'repo . rest)
                       (values '(package
                                  (name "foo")
                                  (inputs `(("bar" ,bar))))
                               '("bar")))
                      (("bar" #:repo 'repo . rest)
                       (values '(package
                                  (name "bar"))
                               '())))
                    #:guix-name identity))

(test-equal "recursive-import: skip false packages (toplevel)"
  '()
  (recursive-import "foo"
                    #:repo 'repo
                    #:repo->guix-package
                    (match-lambda*
                      (("foo" #:repo 'repo . rest)
                       (values #f '())))
                    #:guix-name identity))

(test-equal "recursive-import: skip false packages (dependency)"
  '((package
      (name "foo")
      (inputs `(("bar" ,bar)))))
  (recursive-import "foo"
                    #:repo 'repo
                    #:repo->guix-package
                    (match-lambda*
                      (("foo" #:repo 'repo . rest)
                       (values '(package
                                  (name "foo")
                                  (inputs `(("bar" ,bar))))
                               '("bar")))
                      (("bar" #:repo 'repo . rest)
                       (values #f '())))
                    #:guix-name identity))

(test-assert "alist->package with simple source"
  (let* ((meta '(("name" . "hello")
                 ("version" . "2.10")
                 ("source" .
                  ;; Use a 'file://' URI so that we don't cause a download.
                  ,(string-append "file://"
                                  (search-path %load-path "guix.scm")))
                 ("build-system" . "gnu")
                 ("home-page" . "https://gnu.org")
                 ("synopsis" . "Say hi")
                 ("description" . "This package says hi.")
                 ("license" . "GPL-3.0+")))
         (pkg (alist->package meta)))
    (and (package? pkg)
         (license:license? (package-license pkg))
         (build-system? (package-build-system pkg))
         (origin? (package-source pkg)))))

(test-assert "alist->package with explicit source"
  (let* ((meta '(("name" . "hello")
                 ("version" . "2.10")
                 ("source" . (("method" . "url-fetch")
                              ("uri"    . "mirror://gnu/hello/hello-2.10.tar.gz")
                              ("sha256" .
                               (("base32" .
                                 "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i")))))
                 ("build-system" . "gnu")
                 ("home-page" . "https://gnu.org")
                 ("synopsis" . "Say hi")
                 ("description" . "This package says hi.")
                 ("license" . "GPL-3.0+")))
         (pkg (alist->package meta)))
    (and (package? pkg)
         (license:license? (package-license pkg))
         (build-system? (package-build-system pkg))
         (origin? (package-source pkg))
         (equal? (content-hash-value (origin-hash (package-source pkg)))
                 (base32 "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i")))))

(test-equal "alist->package with false license"  ;<https://bugs.gnu.org/30470>
  'license-is-false
  (let* ((meta '(("name" . "hello")
                 ("version" . "2.10")
                 ("source" . (("method" . "url-fetch")
                              ("uri"    . "mirror://gnu/hello/hello-2.10.tar.gz")
                              ("sha256" .
                               (("base32" .
                                 "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i")))))
                 ("build-system" . "gnu")
                 ("home-page" . "https://gnu.org")
                 ("synopsis" . "Say hi")
                 ("description" . "This package says hi.")
                 ("license" . #f))))
    ;; Note: Use 'or' because comparing with #f otherwise succeeds when
    ;; there's an exception instead of an actual #f.
    (or (package-license (alist->package meta))
        'license-is-false)))

(test-equal "alist->package with SPDX license name 1/2"  ;<https://bugs.gnu.org/45453>
  license:expat
  (let* ((meta '(("name" . "hello")
                 ("version" . "2.10")
                 ("source" . (("method" . "url-fetch")
                              ("uri"    . "mirror://gnu/hello/hello-2.10.tar.gz")
                              ("sha256" .
                               (("base32" .
                                 "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i")))))
                 ("build-system" . "gnu")
                 ("home-page" . "https://gnu.org")
                 ("synopsis" . "Say hi")
                 ("description" . "This package says hi.")
                 ("license" . "expat"))))
    (package-license (alist->package meta))))

(test-equal "alist->package with SPDX license name 2/2"  ;<https://bugs.gnu.org/45453>
  license:expat
  (let* ((meta '(("name" . "hello")
                 ("version" . "2.10")
                 ("source" . (("method" . "url-fetch")
                              ("uri"    . "mirror://gnu/hello/hello-2.10.tar.gz")
                              ("sha256" .
                               (("base32" .
                                 "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i")))))
                 ("build-system" . "gnu")
                 ("home-page" . "https://gnu.org")
                 ("synopsis" . "Say hi")
                 ("description" . "This package says hi.")
                 ("license" . "MIT"))))
    (package-license (alist->package meta))))

(test-equal "alist->package with dependencies"
  `(("gettext" ,(specification->package "gettext")))
  (let* ((meta '(("name" . "hello")
                 ("version" . "2.10")
                 ("source" . (("method" . "url-fetch")
                              ("uri"    . "mirror://gnu/hello/hello-2.10.tar.gz")
                              ("sha256" .
                               (("base32" .
                                 "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i")))))
                 ("build-system" . "gnu")
                 ("home-page" . "https://gnu.org")
                 ("synopsis" . "Say hi")
                 ("description" . "This package says hi.")
                                                  ;
                 ;; Note: As with Guile-JSON 3.x, JSON arrays are represented
                 ;; by vectors.
                 ("native-inputs" . #("gettext"))

                 ("license" . #f))))
    (package-native-inputs (alist->package meta))))

(test-assert "alist->package with properties"
  (let* ((meta '(("name" . "hello")
                 ("version" . "2.10")
                 ("source" .
                  ;; Use a 'file://' URI so that we don't cause a download.
                  ,(string-append "file://"
                                  (search-path %load-path "guix.scm")))
                 ("build-system" . "gnu")
                 ("properties" . (("hidden?" . #t)
                                  ("upstream-name" . "hello-upstream")))
                 ("home-page" . "https://gnu.org")
                 ("synopsis" . "Say hi")
                 ("description" . "This package says hi.")
                 ("license" . "GPL-3.0+")))
         (pkg (alist->package meta)))
    (and (package? pkg)
         (equal? (package-upstream-name pkg) "hello-upstream")
         (hidden-package? pkg))))

(test-equal "spdx-string->license"
  '(license:gpl3+ license:agpl3 license:gpl2+)
  (map spdx-string->license
       '("GPL-3.0-oR-LaTeR" "AGPL-3.0" "GPL-2.0+")))

;;;
;;; default-git-error
;;;

(test-assert "default-git-error: returns a procedure without location argument"
  (procedure?
   (default-git-error "https://github.com/user/repo")))

(test-assert "default-git-error: returns a procedure with location argument"
  (procedure?
   (default-git-error "https://github.com/user/repo"
     (location "none.scm" 42 0))))

(test-equal "default-git-error: procedure handles git-error"
  #f
  (let ((home-page "https://github.com/user/repo"))
    ((default-git-error home-page) '(git-error "some error message"))))

(test-equal "default-git-error: returns #f for non-git-error"
  #f
  (let ((home-page "https://github.com/user/repo"))
    ((default-git-error home-page) '(some-other-error "message"))))

;;;
;;; generate-git-source
;;;

(define (test-generate-git-source git-version version)
  "Helper to test generate-git-source. Creates a temporary git repository with
GIT-VERSION tag, attempts to generate source for VERSION, and returns two
values: the git-source commit S-expression, and a boolean indicating if the
error procedure has been called."
  (with-temporary-git-repository directory
      `((add "README" "Initial commit")
        (commit "First commit")
        (tag ,git-version ,version))
    (mock ((guix import utils) git-repository-url? (const #t))
          (let* ((error-called? #f)
                 (error-proc (lambda args
                               (set! error-called? #t)
                               #f)))
            (match (generate-git-source directory version error-proc)
              (`(origin
                  (method git-fetch)
                  (uri (git-reference (url ,url)
                                      (commit ,commit-sexp)))
                  . ,rest)
               (values commit-sexp error-called?))
              (_
               (values #f error-called?)))))))

(test-equal "generate-git-source: version with 'v' prefix tag"
  '(string-append "v" version)
  (test-generate-git-source "v1.0.0" "1.0.0"))

(test-equal "generate-git-source: version without 'v' prefix tag"
  'version
  (test-generate-git-source "1.0.0" "1.0.0"))

(test-assert "generate-git-source: calls error-procedure when tag not found"
  (let ((sexp error-called? (test-generate-git-source "1.0.0" "2.0.0")))
    error-called?))

(test-end "import-utils")
