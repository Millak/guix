;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2014, 2020-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2018 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2019, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
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

(define-module (guix scripts import)
  #:use-module (guix import utils)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix read-print)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (%standard-import-options
            guix-import))


;;;
;;; Command line options.
;;;

(define %standard-import-options '())


;;;
;;; Entry point.
;;;

(define importers '("gnu" "pypi" "cpan" "hackage" "stackage" "egg" "elpa"
                    "gem" "go" "cran" "crate" "texlive" "json" "opam"
                    "minetest" "elm" "hexpm" "composer" "npm-binary"))

(define (resolve-importer name)
  (let ((module (resolve-interface
                 `(guix scripts import ,(string->symbol name))))
        (proc (string->symbol (string-append "guix-import-" name))))
    (module-ref module proc)))

(define (show-help)
  (display (G_ "Usage: guix import IMPORTER ARGS ...
Run IMPORTER with ARGS.\n"))
  (newline)
  (display (G_ "IMPORTER must be one of the importers listed below:\n"))
  (newline)
  (format #t "~{   ~a~%~}" importers)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -i, --insert           insert packages into file alphabetically"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (import-as-definitions importer args proc)
  "Wrap package expressions from IMPORTER with 'define-public and invoke
PROC callback."
  (if (member importer importers)
      (match (apply (resolve-importer importer) args)
        ((and expr (or ('package _ ...)
                       ('let _ ...)))
         (proc (package->definition expr)))
        ((and expr ('define-public _ ...))
         (proc expr))
        ((expressions ...)
         (for-each (lambda (expr)
                     (match expr
                       ((and expr (or ('package _ ...)
                                      ('let _ ...)))
                        (proc (package->definition expr)))
                       ((and expr ('define-public _ ...))
                        (proc expr))))
                   expressions))
        (x
         (leave (G_ "'~a' import failed~%") importer)))
      (let ((hint (string-closest importer importers #:threshold 3)))
        (report-error (G_ "~a: invalid importer~%") importer)
        (when hint
          (display-hint (G_ "Did you mean @code{~a}?~%") hint))
        (exit 1))))

(define-command (guix-import . args)
  (category packaging)
  (synopsis "import a package definition from an external repository")

  (match args
    (()
     (format (current-error-port)
             (G_ "guix import: missing importer name~%")))
    ((or ("-h" _ ...) ("--help" _ ...))
     (leave-on-EPIPE (show-help))
     (exit 0))
    ((or ("-V" _ ...) ("--version" _ ...))
     (show-version-and-exit "guix import"))
    ((or ("-i" file importer args ...)
         ("--insert" file importer args ...))
     (let ((find-and-insert
             (lambda (expr)
               (match expr
                 (('define-public term _ ...)
                  (let ((source-properties
                          (find-definition-insertion-location
                            file term)))
                    (if source-properties
                      (insert-expression source-properties expr)
                      (let ((port (open-file file "a")))
                        (pretty-print-with-comments port expr)
                        (newline port)
                        (close-port port)))))))))
       (import-as-definitions importer args find-and-insert)))
    ((importer args ...)
     (let ((print (lambda (expr)
                    (leave-on-EPIPE
                      (pretty-print-with-comments
                        (current-output-port) expr)
                      ;; Two newlines: one after the closing paren, and
                      ;; one to leave a blank line.
                      (newline) (newline)))))
       (import-as-definitions importer args print)))))
