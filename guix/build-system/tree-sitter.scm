;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
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

(define-module (guix build-system tree-sitter)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system node)
  #:use-module (ice-9 match)
  #:export (%tree-sitter-build-system-modules
            tree-sitter-build
            tree-sitter-build-system))

(define %tree-sitter-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build tree-sitter-build-system)
    ,@%node-build-system-modules))

(define (default-guile-json)
  "Return the default guile-json package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((mod (resolve-interface '(gnu packages guile))))
    (module-ref mod 'guile-json-4)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME from the given arguments."
  (define private-keywords
    `(#:inputs #:native-inputs #:outputs ,@(if target
                                               '()
                                               '(#:target))))
  (define node
    (module-ref (resolve-interface '(gnu packages node))
                'node-lts))
  (define tree-sitter
    (module-ref (resolve-interface '(gnu packages tree-sitter))
                'tree-sitter))
  (define tree-sitter-cli
    (module-ref (resolve-interface '(gnu packages tree-sitter))
                'tree-sitter-cli))
  ;; Grammars depend on each other via JS modules, which we package into a
  ;; dedicated js output.
  (define grammar-inputs
    (map (match-lambda
           ((name package)
            `(,name ,package "js")))
         inputs))
  (bag
    (name name)
    (system system) (target target)
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ("node" ,node)
                    ("tree-sitter-cli" ,tree-sitter-cli)
                    ,@native-inputs
                    ,@(if target '() grammar-inputs)
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(if target
                          (standard-cross-packages target 'host)
                          '())
                    ,@(standard-packages)))
    (host-inputs `(("tree-sitter" ,tree-sitter)
                   ,@(if target grammar-inputs '())))
    ;; Keep the standard inputs of 'gnu-buid-system'.
    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))
    ;; XXX: this is a hack to get around issue #41569.
    (outputs (match outputs
               (("out") (cons "js" outputs))
               (_ outputs)))
    (build (if target tree-sitter-cross-build tree-sitter-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (tree-sitter-build name inputs
                            #:key
                            source
                            (phases '%standard-phases)
                            (grammar-directories '("."))
                            (tests? #t)
                            (outputs '("out" "js"))
                            (search-paths '())
                            (system (%current-system))
                            (guile #f)
                            (guile-json (default-guile-json))
                            (imported-modules %tree-sitter-build-system-modules)
                            (modules '((guix build utils)
                                       (guix build tree-sitter-build-system))))
  (define builder
    (with-extensions (list guile-json)
      (with-imported-modules imported-modules
        #~(begin
            (use-modules #$@(sexp->gexp modules))
            (tree-sitter-build #:name #$name
                               #:source #+source
                               #:system #$system
                               #:phases #$phases
                               #:tests? #$tests?
                               #:grammar-directories '#$grammar-directories
                               #:outputs #$(outputs->gexp outputs)
                               #:search-paths
                               '#$(sexp->gexp
                                   (map search-path-specification->sexp
                                        search-paths))
                               #:inputs #$(input-tuples->gexp inputs))))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define* (tree-sitter-cross-build name
                                  #:key
                                  target
                                  build-inputs target-inputs host-inputs
                                  guile source
                                  (phases '%standard-phases)
                                  (grammar-directories '("."))
                                  (tests? #t)
                                  (outputs '("out" "js"))
                                  (search-paths '())
                                  (native-search-paths '())
                                  (system (%current-system))
                                  (guile-json (default-guile-json))
                                  (build (nix-system->gnu-triplet system))
                                  (imported-modules
                                   %tree-sitter-build-system-modules)
                                  (modules
                                   '((guix build utils)
                                     (guix build tree-sitter-build-system))))
  (define builder
    (with-extensions (list guile-json)
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

            (tree-sitter-build #:name #$name
                               #:source #+source
                               #:system #$system
                               #:build #$build
                               #:target #$target
                               #:phases #$phases
                               #:tests? #$tests?
                               #:grammar-directories '#$grammar-directories
                               #:outputs #$(outputs->gexp outputs)
                               #:inputs %build-target-inputs
                               #:native-inputs %build-host-inputs
                               #:search-paths '
                               #$(sexp->gexp
                                  (map search-path-specification->sexp
                                       search-paths))
                               #:native-search-paths
                               '#$(sexp->gexp
                                   (map
                                    search-path-specification->sexp
                                    native-search-paths)))))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target target
                      #:guile-for-build guile)))

(define tree-sitter-build-system
  (build-system
    (name 'tree-sitter)
    (description "The Tree-sitter grammar build system")
    (lower lower)))

;;; tree-sitter.scm ends here
