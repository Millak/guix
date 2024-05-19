;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
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

(define-module (guix scripts import npm-binary)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import npm-binary)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-41)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-npm-binary))


;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix import npm-binary PACKAGE-NAME [VERSION]
Import and convert the npm package PACKAGE-NAME using the
`node-build-system' (but without building the package from source)."))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -r, --recursive        import packages recursively"))
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
                   (show-version-and-exit "guix import npm-binary")))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         %standard-import-options))

(define* (package-name->name+version* spec)
  "Given SPEC, a package name like \"@scope/pac@^0.9.1\", return two values:
\"@scope/pac\" and \"^0.9.1\".  When the version part is unavailable, SPEC and \"*\"
are returned.  The first part may start with '@', the latter part must not contain
contain '@'."
  (match (string-rindex spec #\@)
    (#f  (values spec "*"))
    (0  (values spec "*"))
    (idx (values (substring spec 0 idx)
                 (substring spec (1+ idx))))))


;;;
;;; Entry point.
;;;

(define (guix-import-npm-binary . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                             (('argument . value)
                              value)
                             (_ #f))
                           (reverse opts))))
    (match args
      ((spec)
       (define-values (package-name version)
         (package-name->name+version* spec))
       (match (if (assoc-ref opts 'recursive)
                  ;; Recursive import
                  (npm-binary-recursive-import package-name #:version version)
                  ;; Single import
                  (npm-binary->guix-package package-name #:version version))
         ((or #f '())
          (leave (G_ "failed to download meta-data for package '~a@~a'~%")
                 package-name version))
         (('package etc ...) `(package ,@etc))
         ((? list? sexps)
          (map (match-lambda
                 ((and ('package ('name name) ('version version) . rest) pkg)
                  `(define-public ,(name+version->symbol name version)
                     ,pkg))
                 (_ #f))
               sexps))))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))
