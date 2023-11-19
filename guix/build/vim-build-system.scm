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

(define-module (guix build vim-build-system)
  #:use-module ((guix build copy-build-system)
                #:prefix copy:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases vim-build))

;; Commentary:
;;
;; System for installing vim and neovim plugins.  It downloads
;; the source and copies the appropriate files to vim and nvim
;; packpaths.  It then generates helptags.
;;
;; Code:

(define copy:install
  (assoc-ref copy:%standard-phases 'install))

(define vim-path
  "/share/vim/vimfiles/pack/guix/")
(define nvim-path
  "/share/nvim/site/pack/guix/")

(define* (install #:key plugin-name
                  install-plan
                  neovim?
                  vim?
                  mode
                  outputs
                  #:allow-other-keys)

  (let* ((include-regexp '(".*\\/.*\\/.*"))
         (exclude-regexp '("^scripts/.*"
                           "tests?/.*" "^t/.*"
                           "assets/.*"
                           ".*\\/\\..*"))
         (vim-install
           (if vim?
             `(("." ,(string-append vim-path mode "/" plugin-name "/")
                #:include-regexp ,include-regexp
                #:exclude-regexp ,exclude-regexp))
             '()))
         (neovim-install
           (if neovim?
             `(("." ,(string-append nvim-path mode "/" plugin-name "/")
                #:include-regexp ,include-regexp
                #:exclude-regexp ,exclude-regexp))
             '())))
    (copy:install #:outputs outputs
                  #:install-plan (append vim-install
                                         neovim-install
                                         install-plan))))

(define* (generate-helptags #:key plugin-name
                            neovim?
                            vim?
                            mode
                            outputs
                            #:allow-other-keys)

  (define (vim-generate-helptags output)
    (invoke "vim" "--clean" "-en" "--cmd"
            (string-append "helptags "
                           output vim-path mode "/" plugin-name "/doc")
            "--cmd" "q"))

  (define (neovim-generate-helptags output)
    (invoke "nvim" "--clean" "--headless" "-en" "--cmd"
            (string-append "helptags "
                           output nvim-path mode "/" plugin-name "/doc")
            "--cmd" "q"))

  (when (scandir "./doc")
    (let ((out (assoc-ref outputs "out")))
      (when vim?
            (vim-generate-helptags out))
      (when neovim?
            (neovim-generate-helptags out)))))

(define %standard-phases
  ;; Everything is as with the Copy Build System except for
  ;; the addition of the generate-helptags phase and a few
  ;; custom actions are added to the install phase
  (modify-phases copy:%standard-phases
    (replace 'install install)
    (add-after 'install 'generate-helptags generate-helptags)))

(define* (vim-build #:key inputs
                    (phases %standard-phases)
                    #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply copy:copy-build
         #:inputs inputs
         #:phases phases
         args))

;;; vim-build-system.scm ends here
