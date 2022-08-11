;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix build-system perl)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (%perl-build-system-modules
            perl-build
            perl-cross-build
            perl-build-system))

;; Commentary:
;;
;; Standard build procedure for Perl packages using the "makefile
;; maker"---i.e., "perl Makefile.PL".  This is implemented as an extension of
;; `gnu-build-system'.  Cross-compilation is supported for some simple Perl
;; packages, but not for any Perl packages that do things like XS (Perl's FFI),
;; which makes C-style shared libraries, as it is currently not known how to
;; tell Perl to properly cross-compile.
;;
;; Code:

(define %perl-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build perl-build-system)
    ,@%gnu-build-system-modules))

(define (default-perl)
  "Return the default Perl package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages perl))))
    (module-ref module 'perl)))

(define* (lower name
                #:key source inputs native-inputs outputs
                system target
                (perl (default-perl))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:perl #:inputs #:native-inputs
      ,@(if target '() '(#:target))))

  (bag
    (name name)
    (system system) (target target)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs
                   ;; For interpreters in #! (shebang)
                   ,@(if target
                         `(("perl" ,perl))
                         '())

                   ;; Keep the standard inputs of 'gnu-build-system'.
                   ;; TODO: make this unconditional, putting this into
                   ;; 'build-inputs'.
                   ,@(if target
                         '()
                         (standard-packages))))
    (build-inputs `(("perl" ,perl)
                    ,@native-inputs
                    ,@(if target
                          (standard-cross-packages target 'host)
                          '())
                    ,@(if target
                          (standard-packages)
                          '())))
    ;; Keep the standard inputs of 'gnu-build-system'.
    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))
    (outputs outputs)
    (build (if target
               perl-cross-build
               perl-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (perl-build name inputs
                     #:key source
                     (search-paths '())
                     (tests? #t)
                     (parallel-build? #t)
                     (parallel-tests? #t)
                     (make-maker? #f)
                     (make-maker-flags ''())
                     (module-build-flags ''())
                     (phases '(@ (guix build perl-build-system)
                                 %standard-phases))
                     (outputs '("out"))
                     (system (%current-system))
                     (guile #f)
                     (imported-modules %perl-build-system-modules)
                     (modules '((guix build perl-build-system)
                                (guix build utils))))
  "Build SOURCE using PERL, and with INPUTS.  This assumes that SOURCE
provides a `Makefile.PL' file as its build system."
  (define build
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(perl-build #:name #$name
                            #:source #+source
                            #:search-paths '#$(sexp->gexp
                                               (map search-path-specification->sexp
                                                    search-paths))
                            #:make-maker? #$make-maker?
                            #:make-maker-flags #$make-maker-flags
                            #:module-build-flags #$(sexp->gexp module-build-flags)
                            #:phases #$(if (pair? phases)
                                           (sexp->gexp phases)
                                           phases)
                            #:system #$system
                            #:test-target "test"
                            #:tests? #$tests?
                            #:parallel-build? #$parallel-build?
                            #:parallel-tests? #$parallel-tests?
                            #:outputs %outputs
                            #:inputs %build-inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name build
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:guile-for-build guile)))

(define* (perl-cross-build name #:key
                           source
                           target
                           build-inputs host-inputs target-inputs
                           (search-paths '())
                           (native-search-paths '())
                           (tests? #f) ; usually not possible when cross-compiling
                           (parallel-build? #t)
                           (parallel-tests? #t)
                           (make-maker? #f)
                           (make-maker-flags ''())
                           (module-build-flags ''())
                           (phases '(@ (guix build perl-build-system)
                                       %standard-phases))
                           (outputs '("out"))
                           (system (%current-system))
                           (build (nix-system->gnu-triplet system))
                           (guile #f)
                           (imported-modules %perl-build-system-modules)
                           (modules '((guix build perl-build-system)
                                      (guix build utils))))
  "Cross-build SOURCE to TARGET using PERL, and with INPUTS.  This assumes
that SOURCE provides a `Makefile.PL' file as its build system and does not use
XS or similar."
  (define inputs
    #~(append #$(input-tuples->gexp host-inputs)
              #+(input-tuples->gexp target-inputs)))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (perl-build #:name #$name
                      #:source #+source
                      #:search-paths '#$(sexp->gexp
                                         (map search-path-specification->sexp
                                              search-paths))
                      #:native-search-paths
                      '#$(sexp->gexp
                          (map search-path-specification->sexp
                               native-search-paths))
                      #:make-maker? #$make-maker?
                      #:make-maker-flags #$make-maker-flags
                      #:module-build-flags #$(sexp->gexp module-build-flags)
                      #:phases #$phases
                      #:build #$build
                      #:system #$system
                      #:target #$target
                      #:test-target "test"
                      #:tests? #$tests?
                      #:parallel-build? #$parallel-build?
                      #:parallel-tests? #$parallel-tests?
                      #:outputs #$(outputs->gexp outputs)
                      #:inputs #$inputs
                      #:native-inputs #+(input-tuples->gexp build-inputs)))))
  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target target
                      #:graft? #false
                      #:guile-for-build guile)))

(define perl-build-system
  (build-system
    (name 'perl)
    (description "The standard Perl build system")
    (lower lower)))

;;; perl.scm ends here
