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

(define-module (guix build tree-sitter-build-system)
  #:use-module ((guix build node-build-system) #:prefix node:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (%standard-phases
            tree-sitter-build))

;; Commentary:
;;
;; Build procedures for tree-sitter grammar packages.  This is the
;; builder-side code, which builds on top of the node build-system.
;;
;; Tree-sitter grammars are written in JavaScript and compiled to a native
;; shared object.  The `tree-sitter generate' command invokes `node' in order
;; to evaluate the grammar.js into a grammar.json file, which is then
;; translated into C code.  We then compile the C code ourselves.  Packages
;; also sometimes add extra manually written C/C++ code.
;;
;; In order to support grammars depending on each other, such as C and C++,
;; JavaScript and TypeScript, this build-system installs the source of the
;; node module in a dedicated "js" output.
;;
;; Code:

(define* (patch-dependencies #:key inputs #:allow-other-keys)
  "Rewrite dependencies in 'package.json'.  We remove all runtime dependencies
and replace development dependencies with tree-sitter grammar node modules."

  (node:with-atomic-json-file-replacement
   (lambda (pkg-meta-alist)
     (map (match-lambda
            (("dependencies" dependencies ...)
             '("dependencies"))
            (("devDependencies" dev-dependencies ...)
             `("devDependencies"
               ,@(filter-map (match-lambda
                               ((key . directory)
                                (let ((node-module
                                       (string-append directory
                                                      "/lib/node_modules/"
                                                      key)))
                                  (and (directory-exists? node-module)
                                       `(,key . ,node-module)))))
                             (alist-delete "node" inputs))))
            (other other))
          pkg-meta-alist))))

;; FIXME: The node build-system's configure phase does not support
;; cross-compiling so we re-define it.
(define* (configure #:key native-inputs inputs #:allow-other-keys)
  (invoke (search-input-file (or native-inputs inputs) "/bin/npm")
          "--offline" "--ignore-scripts" "install"))

(define* (build #:key grammar-directories #:allow-other-keys)
  (for-each (lambda (dir)
              (with-directory-excursion dir
                ;; Avoid generating binding code for other languages, we do
                ;; not support this use-case yet and it relies on running
                ;; `node-gyp' to build native addons.
                (invoke "tree-sitter" "generate" "--no-bindings")))
            grammar-directories))

(define* (check #:key grammar-directories tests? #:allow-other-keys)
  (when tests?
    (for-each (lambda (dir)
                (with-directory-excursion dir
                  (invoke "tree-sitter" "test")))
              grammar-directories)))

(define* (install #:key target grammar-directories outputs #:allow-other-keys)
  (let ((lib (string-append (assoc-ref outputs "out")
                            "/lib/tree-sitter")))
    (mkdir-p lib)
    (define (compile-language dir)
      (with-directory-excursion dir
        (let ((lang (assoc-ref (call-with-input-file "src/grammar.json"
                                 json->scm)
                               "name"))
              (source-file (lambda (path)
                             (if (file-exists? path)
                                 path
                                 #f))))
          (apply invoke
                 `(,(if target
                        (string-append target "-g++")
                        "g++")
                   "-shared"
                   "-fPIC"
                   "-fno-exceptions"
                   "-O2"
                   "-g"
                   "-o" ,(string-append lib "/libtree-sitter-" lang ".so")
                   ;; An additional `scanner.{c,cc}' file is sometimes
                   ;; provided.
                   ,@(cond
                      ((source-file "src/scanner.c")
                       => (lambda (file) (list "-xc" "-std=c99" file)))
                      ((source-file "src/scanner.cc")
                       => (lambda (file) (list file)))
                      (else '()))
                   "-xc" "src/parser.c")))))
    (for-each compile-language grammar-directories)))

(define* (install-js #:key native-inputs inputs outputs #:allow-other-keys)
  (invoke (search-input-file (or native-inputs inputs) "/bin/npm")
          "--prefix" (assoc-ref outputs "js")
          "--global"
          "--offline"
          "--loglevel" "info"
          "--production"
          ;; Skip scripts to prevent building bindings via GYP.
          "--ignore-scripts"
          "install" "../package.tgz"))

(define %standard-phases
  (modify-phases node:%standard-phases
    (replace 'patch-dependencies patch-dependencies)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'install 'install-js install-js)))

(define* (tree-sitter-build #:key inputs (phases %standard-phases)
                            #:allow-other-keys #:rest args)
  (apply node:node-build #:inputs inputs #:phases phases args))

;;; tree-sitter-build-system.scm ends here
