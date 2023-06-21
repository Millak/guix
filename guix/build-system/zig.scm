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
    (guix build syscalls)
    ,@%gnu-build-system-modules))

(define* (zig-build name inputs
                    #:key
                    source
                    (tests? #t)
                    (test-target #f)
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
                     #:zig-build-flags #$zig-build-flags
                     #:zig-test-flags #$zig-test-flags
                     #:zig-release-type #$zig-release-type
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

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (zig (default-zig))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."

  (define private-keywords
    '(#:target #:zig #:inputs #:native-inputs #:outputs))

  ;; TODO: support cross-compilation
  ;; It's as simple as adding some build flags to `zig-build-flags`
  ;; -Dtarget=aarch64-linux-musl, for example.
  (and (not target)
       (bag
         (name name)
         (system system)
         (target target)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'
                        ;; TODO: do we need this?
                        ,@(standard-packages)))
         (build-inputs `(("zig" ,zig)
                         ,@native-inputs))
         (outputs outputs)
         (build zig-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define zig-build-system
  (build-system
    (name 'zig)
    (description
     "Zig build system, to build Zig packages")
    (lower lower)))
