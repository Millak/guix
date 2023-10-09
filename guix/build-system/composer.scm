;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
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

(define-module (guix build-system composer)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%composer-build-system-modules
            lower
            composer-build
            composer-build-system))

;; Commentary:
;;
;; Standard build procedure for PHP packages using Composer. This is implemented
;; as an extension of `gnu-build-system'.
;;
;; Code:

(define (default-php)
  "Return the default PHP package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages php))))
    (module-ref module 'php)))

(define (default-findclass)
  "Return the default findclass script."
  (search-auxiliary-file "findclass.php"))

(define (default-composer-classloader)
  "Return the default composer-classloader package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages php-xyz))))
    (module-ref module 'composer-classloader)))

(define %composer-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build composer-build-system)
    (guix build union)
    ,@%default-gnu-imported-modules))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (php (default-php))
                (composer-classloader (default-composer-classloader))
                (findclass (default-findclass))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:php #:composer-classloader #:findclass #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("php" ,php)
                         ("findclass.php" ,findclass)
			 ("composer-classloader" ,composer-classloader)
                         ,@native-inputs))
         (outputs outputs)
         (build composer-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (composer-build name inputs
                         #:key
                         guile source
                         (outputs '("out"))
                         (configure-flags ''())
                         (search-paths '())
                         (out-of-source? #t)
                         (composer-file "composer.json")
                         (tests? #t)
                         (test-target "test")
                         (test-flags ''())
                         (install-target "install")
                         (validate-runpath? #t)
                         (patch-shebangs? #t)
                         (strip-binaries? #t)
                         (strip-flags #~'("--strip-debug"))
                         (strip-directories #~'("lib" "lib64" "libexec"
                                               "bin" "sbin"))
                         (phases '(@ (guix build composer-build-system)
                                     %standard-phases))
                         (system (%current-system))
                         (imported-modules %composer-build-system-modules)
                         (modules '((guix build composer-build-system)
                                    (guix build utils))))
  "Build SOURCE using PHP, and with INPUTS. This assumes that SOURCE provides
a 'composer.json' file as its build system."
  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile))
                'guile-json-4))

  (define builder
    (with-extensions (list guile-json)
      (with-imported-modules imported-modules
        #~(begin
            (use-modules #$@(sexp->gexp modules))

            #$(with-build-variables inputs outputs
                #~(composer-build
                   #:source #$source
                   #:system #$system
                   #:outputs %outputs
                   #:inputs %build-inputs
                   #:search-paths '#$(map search-path-specification->sexp
                                          search-paths)
                   #:phases #$phases
                   #:out-of-source? #$out-of-source?
                   #:composer-file #$composer-file
                   #:tests? #$tests?
                   #:test-target #$test-target
                   #:test-flags #$test-flags
                   #:install-target #$install-target
                   #:validate-runpath? #$validate-runpath?
                   #:patch-shebangs? #$patch-shebangs?
                   #:strip-binaries? #$strip-binaries?
                   #:strip-flags #$strip-flags
                   #:strip-directories #$strip-directories))))))

  (gexp->derivation name builder
                    #:system system
                    #:target #f
                    #:graft? #f
                    #:guile-for-build guile))

(define composer-build-system
  (build-system
    (name 'composer)
    (description "The standard Composer build system")
    (lower lower)))

;;; composer.scm ends here
