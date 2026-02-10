;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Carlos Durán Domínguez <wurt@wurt.eu>
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

(define-module (gnu packages array-languages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash))

;;;
;;; Code:

(define-public goal
  (package
    (name "goal")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/anaseto/goal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ckkjd2hf2v6f8x0ck9brp1ii4q8lcssvvwn7d6l1l5n9kcfjw7n"))))
    (build-system go-build-system)
    (inputs (list bash-minimal))
    (outputs (list "out" "lib"))
    (arguments
     (list
      #:unpack-path "codeberg.org/anaseto/goal"
      #:import-path "codeberg.org/anaseto/goal/cmd/goal"
      #:build-flags
      #~(list "-tags" "full,nosse4") ;disable SSE4.2 optimizations
      #:test-flags
      #~(list "-skip" "TestSources/version.goal" ;git tests require a repo
              "-tags" "full,nosse4")
      #:phases
      #~(modify-phases %standard-phases
          ;; Goal requires a shell interpreter.
          (add-after 'unpack 'fix-sh-path
            (lambda* (#:key unpack-path #:allow-other-keys)
              (substitute* (string-append "src/" unpack-path "/os/os.go")
                (("/bin/sh")
                 #$(file-append bash-minimal "/bin/sh")))))
          ;; Tests require the goal binary.
          (add-before 'check 'add-goal-path
            (lambda _
              (setenv "PATH"
                      (string-append (getenv "PATH") ":" #$output "/bin"))))
          ;; Goal is embeddable, but end users typically don't need the source
          ;; code.
          (replace 'install
            (lambda* (#:key unpack-path #:allow-other-keys #:rest arguments)
              (apply (assoc-ref %standard-phases 'install)
                     `(,@arguments #:import-path ,unpack-path))
              (mkdir-p #$output:lib)
              (rename-file (string-append #$output "/src")
                           (string-append #$output:lib "/src"))))
          (add-after 'install 'install-goal-libs-and-documentation
            (lambda* (#:key unpack-path inputs #:allow-other-keys #:rest
                      arguments)
              (let ((outdir (string-append #$output "/share/goal/"
                                           #$(version-major version) "/lib")))
                (mkdir-p outdir)
                (copy-recursively (string-append "src/" unpack-path "/lib")
                                  outdir))
              (install-file (string-append "src/" unpack-path "/docs/goal.1")
                            (string-append #$output "/share/man/man1")))))))
    (native-search-paths
     (list (search-path-specification
            (variable "GOALLIB")
            (separator #f)
            (files `(,(string-append "share/goal/"
                                     (version-major version) "/lib"))))))
    (home-page "https://anaseto.codeberg.page/goal-docs/")
    (synopsis "Embeddable scripting array language")
    (description
     "Goal is an embeddable array programming language with a bytecode
interpreter, written in Go.  The command line intepreter can execute scripts
or run in interactive mode.  Goal shines the most in common scripting tasks,
like handling columnar data or text processing.  It is also suitable for
exploratory programming.")
    (license license:isc)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
