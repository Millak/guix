;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Pierre-Henry Fröhring <contact@phfrohring.com>
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

;; Commentary:
;;
;; Standard build procedure for Elixir packages using 'mix'.  This is
;; implemented as an extension of 'gnu-build-system'.
;;
;; Code:

(define-module (guix build-system mix)
  #:use-module (guix build mix-build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (mix-build-system hexpm-uri))

;; Lazily resolve bindings to avoid circular dependencies.
(define (default-glibc-utf8-locales)
  (let* ((base (resolve-interface '(gnu packages base))))
    (module-ref base 'glibc-utf8-locales)))

(define (default-elixir-hex)
  (let ((elixir (resolve-interface '(gnu packages elixir))))
    (module-ref elixir 'elixir-hex)))

(define (default-rebar3)
  (let ((erlang (resolve-interface '(gnu packages erlang))))
    (module-ref erlang 'rebar3)))

(define (default-elixir)
  (let ((elixir (resolve-interface '(gnu packages elixir))))
    (module-ref elixir 'elixir)))

(define* (strip-prefix name #:optional (prefix "elixir-"))
  "Return NAME without the prefix PREFIX."
  (if (string-prefix? prefix name)
      (string-drop name (string-length prefix))
      name))

(define (hexpm-uri name version)
  "Return the URI where to fetch the sources of a Hex package NAME at VERSION.
NAME is the name of the package which should look like: elixir-pkg-name-X.Y.Z
See: https://github.com/hexpm/specifications/blob/main/endpoints.md"
  ((compose
    (cute string-append "https://repo.hex.pm/tarballs/" <> "-" version ".tar")
    (cute string-replace-substring <> "-" "_")
    strip-prefix)
   name))

;; A number of environment variables specific to the Mix build system are
;; reflected here.  They are documented at
;; https://hexdocs.pm/mix/1.15.7/Mix.html#module-environment-variables.  Other
;; parameters located in mix.exs are defined at
;; https://hexdocs.pm/mix/1.15.7/Mix.Project.html#module-configuration
(define* (mix-build name
                    inputs
                    #:key
                    source
                    (tests? #t)
                    (mix-path #f) ;See MIX_PATH.
                    (mix-exs "mix.exs") ;See MIX_EXS.
                    (build-per-environment #t) ;See :build_per_environment.
                    (phases '%standard-phases)
                    (outputs '("out"))
                    (search-paths '())
                    (system (%current-system))
                    (guile #f)
                    (imported-modules `((guix build mix-build-system)
                                        ,@%gnu-build-system-modules))
                    (modules '((guix build mix-build-system)
                               (guix build utils))))
  "Build SOURCE using Elixir, and with INPUTS."

  ;; Check the documentation of :build_per_environment here:
  ;; https://hexdocs.pm/mix/1.15.7/Mix.Project.html#module-configuration And
  ;; "Environments" here:
  ;; https://hexdocs.pm/mix/1.15.7/Mix.html#module-environments
  (define mix-environments
    (if build-per-environment
        `("prod" ,@(if tests? '("test") '()))
        '("shared")))

  (define builder
    (with-imported-modules imported-modules
      #~(begin

          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(mix-build #:name #$name
                           #:source #+source
                           #:system #$system
                           #:tests? #$tests?
                           #:mix-path #$mix-path
                           #:mix-exs #$mix-exs
                           #:mix-environments '#$mix-environments
                           #:build-per-environment #$build-per-environment
                           #:phases #$(if (pair? phases)
                                          (sexp->gexp phases)
                                          phases)
                           #:outputs %outputs
                           #:search-paths '#$(sexp->gexp
                                              (map
                                               search-path-specification->sexp
                                               search-paths))
                           #:inputs
                           %build-inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system
                                                  #:graft? #f)))
    (gexp->derivation name
                      builder
                      #:system system
                      #:graft? #f       ;consistent with 'gnu-build'
                      #:target #f
                      #:guile-for-build guile)))

(define* (lower name
                #:key
                (elixir (default-elixir))
                (elixir-hex (default-elixir-hex))
                (glibc-utf8-locales (default-glibc-utf8-locales))
                (inputs '())
                (native-inputs '())
                (propagated-inputs '())
                (rebar3 (default-rebar3))
                (tests? #t)
                outputs
                source
                system
                target
                #:allow-other-keys #:rest arguments)
  "Return a bag for NAME."
  (let ((private-keywords
         '(#:inputs #:native-inputs
           #:outputs #:system #:target
           #:elixir #:elixir-hex #:glibc-utf8-locales
           #:rebar3 #:erlang))
        (build-inputs
         `(,@(standard-packages)
           ("glibc-utf8-locales" ,glibc-utf8-locales)
           ("erlang" ,(lookup-package-input elixir "erlang"))
           ("rebar3" ,rebar3)
           ("elixir" ,elixir)
           ("elixir-hex" ,elixir-hex)
           ,@inputs
           ,@native-inputs)))
  (bag (name name)
       (system system)
       (build-inputs build-inputs)
       (host-inputs (if target inputs '()))
       (outputs outputs)
       (build mix-build)
       (arguments (strip-keyword-arguments private-keywords arguments)))))

(define mix-build-system
  (build-system (name 'mix)
                (description "The standard Mix build system")
                (lower lower)))

;;; mix.scm ends here
