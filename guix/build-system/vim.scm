;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Jonathan Scoresby <me@jonscoresby.com>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix build-system vim)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:export (%vim-build-system-modules vim-build vim-build-system))

;; Commentary:
;;
;; Standard package installer for vim and neovim plugins.
;; This is implemented as an extension of the `copy-build-system'
;; and takes advantage of vim and neovim's built-in package manager.
;; It extends the installation procedure from the copy-build-system
;; to put files in the correct place and then generates help tags.
;;
;; Code:

(define %vim-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build vim-build-system)
    ,@%copy-build-system-modules))

(define (default-vim)
  "Return the default Vim package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((vim (resolve-interface '(gnu packages vim))))
    (module-ref vim 'vim)))

(define (default-neovim)
  "Return the default Neovim package."
  (let ((vim (resolve-interface '(gnu packages vim))))
    (module-ref vim 'neovim)))

(define* (lower name
                #:key source
                inputs
                native-inputs
                outputs
                system
                target
                (vim? #f)
                (neovim? #f)
                (plugin-name name)
                (vim (default-vim))
                (neovim (default-neovim))
                #:allow-other-keys #:rest arguments)
  "Return a bag for NAME."
  (let* ((private-keywords '(#:target #:vim #:neovim #:inputs #:native-inputs))
         (vim? (or (string-prefix? "vim-" name)
                   vim?))
         (neovim? (or (string-prefix? "neovim-" name)
                      neovim?))
         (vim-inputs (append (if vim?
                                 `(("vim" ,vim))
                                 '())
                             (if neovim?
                                 `(("neovim" ,neovim))
                                 '())))
         (vim-arguments (append arguments
                                `(#:vim? ,vim?
                                  #:neovim? ,neovim?))))
    (bag (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '()) ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(,@vim-inputs ,@native-inputs))
         (outputs outputs)
         (build vim-build)
         (arguments (strip-keyword-arguments private-keywords vim-arguments)))))

(define* (vim-build name inputs
                    #:key guile
                    source
                    (vim? #f)
                    (neovim? #f)
                    (mode "start")
                    (plugin-name name)
                    (install-plan ''())
                    (phases '(@ (guix build vim-build-system) %standard-phases))
                    (outputs '("out"))
                    (out-of-source? #t)
                    (tests? #t)
                    (validate-runpath? #t)
                    (patch-shebangs? #t)
                    (strip-binaries? #t)
                    (strip-flags %strip-flags)
                    (strip-directories %strip-directories)
                    (search-paths '())
                    (system (%current-system))
                    (substitutable? #t)
                    (imported-modules %vim-build-system-modules)
                    (modules '((guix build vim-build-system)
                               (guix build utils))))

  (define build
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)
          #$(with-build-variables inputs outputs
              #~(vim-build #:name #$name
                           #:vim? #$vim?
                           #:neovim? #$neovim?
                           #:mode #$mode
                           #:plugin-name #$plugin-name
                           #:install-plan #$(if (pair? install-plan)
                                                (sexp->gexp install-plan)
                                                install-plan)
                           #:source #+source
                           #:system #$system
                           #:phases #$(if (pair? phases)
                                          (sexp->gexp phases)
                                          phases)
                           #:outputs %outputs
                           #:search-paths '#$(sexp->gexp
                                               (map search-path-specification->sexp
                                                    search-paths))
                           #:inputs %build-inputs
                           #:out-of-source? #$out-of-source?
                           #:tests? #$tests?
                           #:validate-runpath? #$validate-runpath?
                           #:patch-shebangs? #$patch-shebangs?
                           #:strip-binaries? #$strip-binaries?
                           #:strip-flags #$strip-flags
                           #:strip-directories #$strip-directories)))))

  (mlet %store-monad
        ((guile (package->derivation (or guile (default-guile))
                                     system #:graft? #f)))
        (gexp->derivation name
                          build
                          #:system system
                          #:target #f
                          #:graft? #f
                          #:substitutable? substitutable?
                          #:guile-for-build guile)))

(define vim-build-system
  (build-system (name 'vim)
                (description "The standard Vim build system")
                (lower lower)))

;;; vim.scm ends here
