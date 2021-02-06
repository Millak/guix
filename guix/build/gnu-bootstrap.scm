;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2022 Timothy Sample <samplet@ngyro.com>
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
;; These procedures can be used to adapt the GNU Build System to build
;; pure Scheme packages targeting the bootstrap Guile.
;;
;; Code:

(define-module (guix build gnu-bootstrap)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (system base compile)
  #:export (bootstrap-configure
            bootstrap-build
            bootstrap-install))

(define (bootstrap-configure name version modules scripts)
  "Create a procedure that configures an early bootstrap package.  The
procedure will search each directory in MODULES and configure all of the
'.in' files with NAME and VERSION.  It will then search the SCRIPTS
directory and configure all of the '.in' files with the bootstrap
Guile and its module and object directories."
  (lambda* (#:key inputs outputs #:allow-other-keys)
    (let* ((out (assoc-ref outputs "out"))
           (guile-dir (assoc-ref inputs "guile"))
           (guile (string-append guile-dir "/bin/guile"))
           (moddir (string-append out "/share/guile/site/"
                                  (effective-version)))
           (godir (string-append out "/lib/guile/"
                                 (effective-version)
                                 "/site-ccache")))
      (for-each (lambda (template)
                  (format #t "Configuring ~a~%" template)
                  (let ((target (string-drop-right template 3)))
                    (copy-file template target)
                    (substitute* target
                      (("@PACKAGE_NAME@") name)
                      (("@VERSION@") version))))
                (append-map (lambda (dir) (find-files dir "\\.in$"))
                            modules))
      (for-each (lambda (template)
                  (format #t "Configuring ~a~%" template)
                  (let ((target (string-drop-right template 3)))
                    (copy-file template target)
                    (substitute* target
                      (("@GUILE@") guile)
                      (("@MODDIR@") moddir)
                      (("@GODIR@") godir))
                    (chmod target #o755)))
                (find-files scripts
                            (lambda (fn st)
                              (string-suffix? ".in" fn))))
      #t)))

(define (bootstrap-build modules)
  "Create a procedure that builds an early bootstrap package.  The
procedure will search each directory in MODULES and compile all of the
'.scm' files."
  (lambda _
    (add-to-load-path (getcwd))
    (for-each (lambda (scm)
                (let* ((base (string-drop-right scm 4))
                       (go (string-append base ".go"))
                       (dir (dirname scm)))
                  (format #t "Compiling ~a~%" scm)
                  (compile-file scm #:output-file go)))
              (append-map (lambda (dir) (find-files dir "\\.scm$"))
                          modules))
    #t))

(define (bootstrap-install modules scripts)
  "Create a procedure that installs an early bootstrap package.  The
procedure will install all of the '.scm' and '.go' files in each of the
directories in MODULES, and all the executable files in the SCRIPTS
directory."
  (lambda* (#:key inputs outputs #:allow-other-keys)
    (let* ((out (assoc-ref outputs "out"))
           (guile-dir (assoc-ref inputs "guile"))
           (guile (string-append guile-dir "/bin/guile"))
           (moddir (string-append out "/share/guile/site/"
                                  (effective-version)))
           (godir (string-append out "/lib/guile/"
                                 (effective-version)
                                 "/site-ccache")))
      (for-each (lambda (scm)
                  (let* ((base (string-drop-right scm 4))
                         (go (string-append base ".go"))
                         (dir (dirname scm)))
                    (format #t "Installing ~a~%" scm)
                    (install-file scm (string-append moddir "/" dir))
                    (format #t "Installing ~a~%" go)
                    (install-file go (string-append godir "/" dir))))
                (append-map (lambda (dir) (find-files dir "\\.scm$"))
                            modules))
      (for-each (lambda (script)
                  (format #t "Installing ~a~%" script)
                  (install-file script (string-append out "/bin")))
                (find-files scripts
                            (lambda (fn st)
                              (executable-file? fn))))
      #t)))
