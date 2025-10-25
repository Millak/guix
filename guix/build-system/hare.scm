;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Lilah Tascheter <lilah@lunabee.space>
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

(define-module (guix build-system hare)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:export (%hare-supported-systems
            target->hare-arch
            %default-hare-imported-modules
            %default-hare-modules
            hare-build-system))

(define %hare-supported-systems
  '("x86_64-linux" "aarch64-linux" "riscv64-linux"))

(define* (target->hare-arch #:optional (target (or (%current-target-system)
                                                   (%current-system))))
  ;; Only error on supported systems, so we don't break guix pull.
  (if (member (%current-system) %hare-supported-systems)
      (cond ((target-x86-64? target) "x86_64")
            ((target-aarch64? target) "aarch64")
            ((target-riscv64? target) "riscv64")
            (else (error "unsupported hare target" target)))
      "UNKNOWN"))

(define %default-hare-imported-modules
  (cons '(guix build hare-build-system) %default-gnu-imported-modules))
(define %default-hare-modules
  '((guix build hare-build-system) (guix build utils)))

(define* (lower name #:key source inputs native-inputs outputs system target
                (implicit-inputs? #t) (implicit-cross-inputs? #t)
                #:allow-other-keys #:rest arguments)
  (let* ((hare (module-ref (resolve-interface '(gnu packages hare)) 'hare))
         (private-arguments '(#:inputs #:native-inputs #:target
                              #:build-inputs #:target-inputs #:host-inputs
                              #:implicit-inputs? #:implicit-cross-inputs?)))
    (bag
      (name name)
      (system system)
      (target target)
      (arguments (strip-keyword-arguments private-arguments arguments))
      (build-inputs (append (if source `(("source" ,source)) '())
                            native-inputs
                            (if implicit-inputs? `(("hare" ,hare)) '())
                            (if (and implicit-cross-inputs? target)
                                (standard-cross-packages target 'host) '())
                            (if implicit-inputs?
                                (standard-packages system) '())))
      (host-inputs inputs)
      (target-inputs  (if (and implicit-cross-inputs? target)
                          (standard-cross-packages target 'target) '()))
      (outputs outputs)
      (build (if target hare-build (lambda (name inputs . rest)
                                     (apply hare-build name
                                            #:host-inputs inputs rest)))))))

(define* (hare-build name #:key guile system (target #f) source
                     (build-inputs '()) (target-inputs '()) (host-inputs '())
                     (phases '%standard-phases) (outputs '("out"))
                     (imported-modules %default-hare-imported-modules)
                     (modules %default-hare-modules)

                     (search-paths '()) (native-search-paths '())
                     (make-flags ''()) (hare-arch #f)
                     (parallel-builds? #t)
                     (tests? #t) (test-target "check") (parallel-tests? #t)
                     (binary-output (if (member "bin" outputs) "bin" "out"))
                     (module-output (if (member "lib" outputs) "lib" "out"))
                     (validate-runpath? #t)

                     (substitutable? #t)
                     allowed-references disallowed-references)
  (define builder
    (with-imported-modules imported-modules
      #~(begin (use-modules #$@modules)
          (define %build-host-inputs #+(input-tuples->gexp build-inputs))
          (define %build-target-inputs
            (append #$(input-tuples->gexp host-inputs)
                    #+(input-tuples->gexp target-inputs)))
          (define %build-inputs
            (append %build-host-inputs %build-target-inputs))

          (define %outputs #$(outputs->gexp outputs))
          (define %binary-output (assoc-ref %outputs #$binary-output))
          (define %module-output (assoc-ref %outputs #$module-output))

          (hare-build
            #:source #+source
            #:system #$system
            #:target #$target
            #:phases #$phases
            #:inputs %build-target-inputs
            #:native-inputs %build-host-inputs
            #:outputs %outputs
            #:search-paths '#$(map search-path-specification->sexp search-paths)
            #:native-search-paths '#$(map search-path-specification->sexp
                                          native-search-paths)
            #:hare-arch #$(or hare-arch (target->hare-arch (or target system)))
            #:binary-output %binary-output
            #:module-output %module-output
            #:make-flags #$make-flags
            #:parallel-builds? #$parallel-builds?
            #:tests? #$tests?
            #:test-target #$test-target
            #:parallel-tests? #$parallel-tests?
            #:validate-runpath? #$validate-runpath?))))
  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target target
                      #:graft? #f ; same as gnu-build-system
                      #:substitutable? substitutable?
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references
                      #:guile-for-build guile)))

(define hare-build-system
  (build-system
    (name 'hare)
    (description "Hare de facto build systems")
    (lower lower)))
