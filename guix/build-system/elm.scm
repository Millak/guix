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

(define-module (guix build-system elm)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (elm->package-name
            guix-package->elm-name
            infer-elm-package-name
            elm-package-origin
            %elm-build-system-modules
            %elm-default-modules
            elm-build
            elm-build-system))

(define (elm->package-name name)
  "Given the NAME of an Elm package, return a Guix-style package name."
  (let ((converted
         (string-join (string-split (string-downcase name) #\/) "-")))
    (if (string-prefix? "elm-" converted)
        converted
        (string-append "elm-" converted))))

(define (guix-package->elm-name package)
  "Given an Elm PACKAGE, return the possibly-inferred upstream name, or #f the
upstream name is not specified and can't be inferred."
  (or (assoc-ref (package-properties package) 'upstream-name)
      (infer-elm-package-name (package-name package))))

(define (infer-elm-package-name guix-name)
  "Given the GUIX-NAME of an Elm package, return the inferred upstream name,
or #f if it can't be inferred.  If the result is not #f, supplying it to
'elm->package-name' would produce GUIX-NAME.

See also 'guix-package->elm-name', which respects the 'upstream-name'
property."
  (define (parts-join part0 parts)
    (string-join (cons part0 parts) "-"))
  (match (string-split guix-name #\-)
    (("elm" "explorations" part0 parts ...)
     (string-append "elm-explorations/"
                    (parts-join part0 parts)))
    (("elm" owner part0 parts ...)
     (string-append owner "/" (parts-join part0 parts)))
    (("elm" repo)
     (string-append "elm/" repo))
    (_
     #f)))

(define (elm-package-origin elm-name version hash)
  "Return an origin for the Elm package with upstream name ELM-NAME at the
given VERSION with sha256 checksum HASH."
  ;; elm requires this very specific repository structure and tagging regime
  (origin
    (method git-fetch)
    (uri (git-reference
          (url (string-append "https://github.com/" elm-name))
          (commit version)))
    (file-name (git-file-name (elm->package-name elm-name) version))
    (sha256 hash)))

(define %elm-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build elm-build-system)
    (guix build json)
    (guix build union)
    ,@%gnu-build-system-modules))

(define %elm-default-modules
  ;; Modules in scope in the build-side environment.
  '((guix build elm-build-system)
    (guix build utils)
    (guix build json)
    (guix build union)))

(define (default-elm)
  "Return the default Elm package for builds."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((elm (resolve-interface '(gnu packages elm))))
    (module-ref elm 'elm)))

(define (default-elm-core)
  "Return the default elm-core package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((elm (resolve-interface '(gnu packages elm))))
    (module-ref elm 'elm-core)))

(define (default-elm-json)
  "Return the default elm-json package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((elm (resolve-interface '(gnu packages elm))))
    (module-ref elm 'elm-json)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (implicit-elm-package-inputs? #t)
                (elm (default-elm))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:implicit-elm-package-inputs? #:elm #:inputs #:native-inputs))
  (cond
   (target
    ;; Cross-compilation is not yet supported.  It should be easy, though,
    ;; since the build products are all platform-independent.
    #f)
   (else
    (bag
      (name name)
      (system system)
      (host-inputs
       `(,@(if source
               `(("source" ,source))
               '())
         ,@inputs
         ("elm" ,elm)
         ,@(cond
            (implicit-elm-package-inputs?
             ;; These are needed for elm-build-system even if not actually
             ;; needed by the package being built.  But "elm/json" is often
             ;; present in practice, and "elm/core" always is: only add the
             ;; default packages if no suitable inputs have been given
             ;; explicitly.
             (filter-map
              (match-lambda
                ((name get-default)
                 (cond
                  ((find (match-lambda
                           ((_ pkg . _)
                            (equal? name (guix-package->elm-name pkg))))
                         inputs)
                   #f)
                  (else
                   `(,name ,(get-default))))))
              `(("elm/core" ,default-elm-core)
                ("elm/json" ,default-elm-json))))
            (else
             '()))
         ;; TODO: probably don't need most of (standard-packages)
         ,@(standard-packages)))
      (outputs outputs)
      (build elm-build)
      (arguments (strip-keyword-arguments private-keywords arguments))))))

(define* (elm-build name inputs
                    #:key
                    source
                    (tests? #t)
                    (phases '%standard-phases)
                    (outputs '("out"))
                    (search-paths '())
                    (system (%current-system))
                    (guile #f)
                    (imported-modules %elm-build-system-modules)
                    (modules %elm-default-modules))
  "Build SOURCE using ELM."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (elm-build #:name #$name
                     #:source #+source
                     #:system #$system
                     #:tests? #$tests?
                     #:phases #$phases
                     #:outputs #$(outputs->gexp outputs)
                     #:search-paths '#$(sexp->gexp
                                        (map search-path-specification->sexp
                                             search-paths))
                     #:inputs #$(input-tuples->gexp inputs)))))
  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define elm-build-system
  (build-system
    (name 'elm)
    (description "The Elm build system")
    (lower lower)))
