;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
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

(define-module (guix build-system agda)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:export (default-agda

            %agda-build-system-modules
            agda-build-system))

(define (default-agda)
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((agda (resolve-interface '(gnu packages agda))))
    (module-ref agda 'agda)))

(define %agda-build-system-modules
  `((guix build agda-build-system)
    ,@%gnu-build-system-modules))

(define %default-modules
  '((guix build agda-build-system)
    (guix build utils)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (agda (default-agda))
                gnu-and-haskell?
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:agda #:gnu-and-haskell? #:inputs #:native-inputs))

  ;; TODO: cross-compilation support
  (and (not target)
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs))
         (build-inputs `(("agda" ,agda)
                         ,@(if gnu-and-haskell?
                               (cons*
                                (list "ghc" (default-haskell))
                                (standard-packages))
                               '())
                         ,(assoc "locales" (standard-packages))
                         ,@native-inputs))
         (outputs outputs)
         (build agda-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (agda-build name inputs
                     #:key
                     source
                     (phases '%standard-phases)
                     (outputs '("out"))
                     (search-paths '())
                     (unpack-path "")
                     (build-flags ''())
                     (tests? #t)
                     (system (%current-system))
                     (guile #f)
                     plan
                     (extra-files '("^\\./.*\\.agda-lib$"))
                     (imported-modules %agda-build-system-modules)
                     (modules %default-modules))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (agda-build #:name #$name
                      #:source #+source
                      #:system #$system
                      #:phases #$phases
                      #:outputs #$(outputs->gexp outputs)
                      #:search-paths '#$(sexp->gexp
                                         (map search-path-specification->sexp
                                              search-paths))
                      #:unpack-path #$unpack-path
                      #:build-flags #$build-flags
                      #:tests? #$tests?
                      #:inputs #$(input-tuples->gexp inputs)
                      #:plan '#$plan
                      #:extra-files '#$extra-files))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define agda-build-system
  (build-system
    (name 'agda)
    (description
     "Build system for Agda libraries")
    (lower lower)))
