;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix build-system rebar)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (hexpm-uri
            hexpm-package-url
            %rebar-build-system-modules
            rebar-build
            rebar-build-system))

;;;
;;; Definitions for the hex.pm repository,
;;;

;; URL and paths from
;; https://github.com/hexpm/specifications/blob/master/endpoints.md
(define %hexpm-repo-url
  (make-parameter "https://repo.hex.pm"))

(define hexpm-package-url
  (string-append (%hexpm-repo-url) "/tarballs/"))

(define (hexpm-uri name version)
  "Return a URI string for the package hosted at hex.pm corresponding to NAME
and VERSION."
  (string-append hexpm-package-url name "-" version ".tar"))

;;
;; Standard build procedure for Erlang packages using Rebar.
;;

(define %rebar-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build rebar-build-system)
    ,@%gnu-build-system-modules))

(define (default-rebar3)
  "Return the default Rebar3 package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((erlang-mod (resolve-interface '(gnu packages erlang))))
    (module-ref erlang-mod 'rebar3)))

(define (default-erlang)
  "Return the default Erlang package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((erlang-mod (resolve-interface '(gnu packages erlang))))
    (module-ref erlang-mod 'erlang)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (rebar (default-rebar3))
                (erlang (default-erlang))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME from the given arguments."
  (define private-keywords
    '(#:target #:rebar #:erlang #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs))
         (build-inputs `(("rebar" ,rebar)
                         ("erlang" ,erlang) ;; for escriptize
                         ,@native-inputs
                         ;; Keep the standard inputs of 'gnu-build-system'.
                         ,@(standard-packages)))
         (outputs outputs)
         (build rebar-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (rebar-build name inputs
                       #:key
                       guile source
                       (rebar-flags ''("skip_deps=true" "-vv"))
                       (tests? #t)
                       (test-target "eunit")
                       ;; TODO: install-name  ; default: based on guix package name
                       (install-profile "default")
                       (phases '(@ (guix build rebar-build-system)
                                   %standard-phases))
                       (outputs '("out"))
                       (search-paths '())
                       (native-search-paths '())
                       (system (%current-system))
                       (imported-modules %rebar-build-system-modules)
                       (modules '((guix build rebar-build-system)
                                  (guix build utils))))
  "Build SOURCE with INPUTS."

  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(rebar-build #:source #+source
                      #:system #$system
                      #:name #$name
                      #:rebar-flags #$rebar-flags
                      #:tests? #$tests?
                      #:test-target #$test-target
                      ;; TODO: #:install-name #$install-name
                      #:install-profile #$install-profile
                      #:phases #$(if (pair? phases)
                                     (sexp->gexp phases)
                                     phases)
                      #:outputs %outputs
                      #:search-paths '#$(sexp->gexp
                                         (map search-path-specification->sexp
                                              search-paths))
                      #:inputs %build-inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    ;; Note: Always pass #:graft? #f.  Without it, ALLOWED-REFERENCES &
    ;; co. would be interpreted as referring to grafted packages.
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:guile-for-build guile)))

(define rebar-build-system
  (build-system
    (name 'rebar)
    (description "The standard Rebar build system")
    (lower lower)))
