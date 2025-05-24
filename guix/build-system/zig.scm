;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
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

(define-module (guix build-system zig)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (zig-build-system))


(define (default-zig)
  "Return the default zig package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((zig (resolve-interface '(gnu packages zig))))
    (module-ref zig 'zig)))

(define %zig-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build zig-build-system)
    (guix build zig-utils)
    ,@%default-gnu-imported-modules))

(define* (zig-build name inputs
                    #:key
                    source
                    (tests? #t)
                    (test-target #f)
                    (parallel-build? #t)
                    (parallel-tests? #t)
                    (install-source? #t)
                    (skip-build? #f)
                    (zig-build-target #f)
                    (zig-build-flags ''())
                    (zig-test-flags ''())
                    (zig-release-type #f)
                    (phases '%standard-phases)
                    (outputs '("out"))
                    (search-paths '())
                    (system (%current-system))
                    (guile #f)
                    (imported-modules %zig-build-system-modules)
                    (modules '((guix build zig-build-system)
                               (guix build utils))))
  "Build SOURCE using Zig, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (zig-build #:name #$name
                     #:source #+source
                     #:system #$system
                     #:test-target #$test-target
                     #:parallel-build? #$parallel-build?
                     #:parallel-tests? #$parallel-tests?
                     #:install-source? #$install-source?
                     #:skip-build? #$skip-build?
                     #:zig-build-flags #$zig-build-flags
                     ;; For reproducibility.
                     #:zig-build-target
                     (or #$zig-build-target
                         #$(platform-target
                            (lookup-platform-by-system system)))
                     #:zig-test-flags #$zig-test-flags
                     #:zig-release-type #$zig-release-type
                     #:tests? #$(and tests? (not skip-build?))
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

(define* (zig-cross-build name
                          #:key
                          source target
                          build-inputs target-inputs host-inputs
                          (phases '%standard-phases)
                          (outputs '("out"))
                          (search-paths '())
                          (native-search-paths '())
                          (tests? #t)
                          (test-target #f)
                          (parallel-build? #t)
                          (parallel-tests? #t)
                          (install-source? #t)
                          (skip-build? #f)
                          (zig-build-target #f)
                          (zig-build-flags ''())
                          (zig-test-flags ''())
                          (zig-destdir "out")
                          (zig-test-destdir "test-out")
                          (zig-release-type #f)
                          (system (%current-system))
                          (guile #f)
                          (imported-modules %zig-build-system-modules)
                          (modules '((guix build zig-build-system)
                                     (guix build utils))))
  "Build SOURCE using Zig, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (define %build-host-inputs
            #+(input-tuples->gexp build-inputs))

          (define %build-target-inputs
            (append #$(input-tuples->gexp host-inputs)
              #+(input-tuples->gexp target-inputs)))

          (define %build-inputs
            (append %build-host-inputs %build-target-inputs))

          (define %outputs
            #$(outputs->gexp outputs))

          (zig-build #:name #$name
                     #:source #+source
                     #:system #$system
                     #:phases #$phases
                     #:outputs %outputs
                     #:target #$target
                     #:test-target #$test-target
                     #:parallel-build? #$parallel-build?
                     #:parallel-tests? #$parallel-tests?
                     #:inputs %build-target-inputs
                     #:native-inputs %build-host-inputs
                     #:search-paths '#$(map search-path-specification->sexp
                                            search-paths)
                     #:native-search-paths '#$(map
                                                search-path-specification->sexp
                                                native-search-paths)
                     #:install-source? #$install-source?
                     #:skip-build? #$skip-build?
                     #:zig-build-flags #$zig-build-flags
                     #:zig-build-target (or #$zig-build-target #$target)
                     #:zig-test-flags #$zig-test-flags
                     #:zig-release-type #$zig-release-type
                     #:zig-destdir #$zig-destdir
                     #:zig-test-destdir #$zig-test-destdir
                     #:tests? #$(and tests? (not skip-build?))
                     #:search-paths '#$(sexp->gexp
                                        (map search-path-specification->sexp
                                             search-paths))))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
        (gexp->derivation name builder
                          #:system system
                          #:target target
                          #:graft? #f
                          #:substitutable? substitutable?
                          #:guile-for-build guile)))


(define* (lower name
                #:key source inputs native-inputs outputs system target
                (zig (default-zig))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."

  (define private-keywords
    '(#:target #:zig #:inputs #:native-inputs #:outputs))

  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source
                        `(("source" ,source))
                        '())
                    ,@`(("zig" ,zig))
                    ,@native-inputs
                    ,@(if target '() inputs)
                    ,@(if target
                        ;; Use the standard cross inputs of
                        ;; 'gnu-build-system'.
                        (standard-cross-packages target 'host)
                        '())
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(standard-packages)))
    (host-inputs (if target inputs '()))
    (target-inputs (if target
                     (standard-cross-packages target 'target)
                     '()))
    (outputs outputs)
    (build (if target zig-cross-build zig-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define zig-build-system
  (build-system
    (name 'zig)
    (description
     "Zig build system, to build Zig packages")
    (lower lower)))
