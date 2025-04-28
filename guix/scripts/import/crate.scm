;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2023 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2025 Herman Rimm <herman@rimm.ee>
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

(define-module (guix scripts import crate)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix read-print)
  #:use-module (guix scripts)
  #:use-module (guix import crate)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-crate))


;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix import crate PACKAGE-NAME
Import and convert the crates.io package for PACKAGE-NAME.\n"))
  (display (G_ "
  -r, --recursive        import packages recursively"))
  (display (G_ "
      --recursive-dev-dependencies
                         include dev-dependencies recursively"))
  (newline)
  (display (G_ "
      --allow-yanked     allow importing yanked crates if no alternative
                         satisfying the version requirement is found"))
  (display (G_ "
      --mark-missing     comment out the desired dependency if no
                         sufficient package exists for it"))
  (newline)
  (display (G_ "
  -f, --lockfile=FILE    import dependencies from FILE, a 'Cargo.lock' file"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix import crate")))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         (option '("recursive-dev-dependencies") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive-dev-dependencies #t result)))
         (option '("allow-yanked") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'allow-yanked #t result)))
         (option '("mark-missing") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'mark-missing #t result)))
         (option '(#\f "lockfile") #f #t
                 (lambda (opt name arg result)
                   (if (file-exists? arg)
                       (alist-cons 'lockfile arg result)
                       (leave (G_ "file '~a' does not exist~%") arg))))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-crate . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (lockfile (assoc-ref opts 'lockfile))
         (file-to-insert (assoc-ref opts 'file-to-insert))
         (args (filter-map (match-lambda
                             (('argument . value)
                              value)
                             (_ #f))
                           (reverse opts))))
    (match args
      ((spec)
       (define-values (name version)
         (package-name->name+version spec))

       (match (cond
               (lockfile
                (let ((source-expressions
                       _
                       (cargo-lock->expressions lockfile name)))
                  (when file-to-insert
                    (let* ((source-expressions
                            cargo-inputs-entry
                            (cargo-lock->expressions lockfile name))
                           (term (first cargo-inputs-entry))
                           (cargo-inputs
                            `(define-cargo-inputs lookup-cargo-inputs
                               ,@(sort
                                  (cons cargo-inputs-entry
                                        (extract-cargo-inputs
                                         file-to-insert #:exclude term))
                                  (lambda (a b)
                                    (string< (symbol->string (first a))
                                             (symbol->string (first b)))))))
                           (_
                            (and=> (find-cargo-inputs-location file-to-insert)
                                   delete-expression))
                           (port (open-file file-to-insert "a")))
                      (pretty-print-with-comments port cargo-inputs)
                      (newline port)
                      (close-port port)))
                  source-expressions))
               ((assoc-ref opts 'recursive)
                (crate-recursive-import
                 name #:version version
                 #:recursive-dev-dependencies?
                 (assoc-ref opts 'recursive-dev-dependencies)
                 #:allow-yanked? (assoc-ref opts 'allow-yanked)))
               (else
                (crate->guix-package
                 name #:version version #:include-dev-deps? #t
                 #:allow-yanked? (assoc-ref opts 'allow-yanked)
                 #:mark-missing? (assoc-ref opts 'mark-missing))))
         ((or #f '())
          (leave (G_ "failed to download meta-data for package '~a'~%")
                 (if version
                     (string-append name "@" version)
                     name)))
         ((? list? sexps) sexps)
         (sexp (list sexp))))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))
