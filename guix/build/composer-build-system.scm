;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (guix build composer-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            composer-build))

;; Commentary:
;;
;; Builder-side code of the standard composer build procedure.
;;
;; Code:

(define (json->require dict)
  (if dict
      (let loop ((result '()) (require dict))
        (match require
          (() result)
          ((((? (cut string-contains <> "/") name) . _)
             require ...)
           (loop (cons name result) require))
          ((_ require ...) (loop result require))
          (_ result)))
      '()))

(define (if-specified-to-list fn)
  (match-lambda
    ((? unspecified?) '())
    (arg (fn arg))
    (_ '())))

(define-json-mapping <composer-autoload> make-composer-autoload
  composer-autoload?
  json->composer-autoload
  (psr-4 composer-autoload-psr-4 "psr-4"
         (match-lambda
           ((? unspecified?) '())
           ((? (lambda (al)
                 (and (list? al) (pair? (car al)) (vector? (cdar al)))) al)
            (append-map
             (lambda (vect-el)
               (list (cons (caar al) vect-el)))
             (vector->list (cdar al))))
           ((? list? l)                  l)
           (_                           '())))
  (psr-0 composer-autoload-psr-0 "psr-0" (if-specified-to-list identity))
  (classmap composer-autoload-classmap "classmap"
            (if-specified-to-list vector->list))
  (files composer-autoload-files "files"
         (if-specified-to-list vector->list)))

(define-json-mapping <composer-package> make-composer-package composer-package?
  json->composer-package
  (name         composer-package-name)
  (autoload     composer-package-autoload "autoload"
                (if-specified-to-list json->composer-autoload))
  (autoload-dev composer-package-autoload-dev "autoload-dev"
                (if-specified-to-list json->composer-autoload))
  (require      composer-package-require "require" json->require)
  (dev-require  composer-package-dev-require "require-dev" json->require)
  (scripts      composer-package-scripts "scripts"
                (if-specified-to-list identity))
  (binaries     composer-package-binaries "bin"
                (if-specified-to-list vector->list)))

(define* (read-package-data #:key (filename "composer.json"))
  (call-with-input-file filename
    (lambda (port)
      (json->composer-package (json->scm port)))))

(define* (create-test-autoload #:key composer-file inputs outputs tests?
                               #:allow-other-keys)
  "Create the autoload.php file for tests.  This is a standalone phase so that
the autoload.php file can be edited before the check phase."
  (when tests?
    (mkdir-p "vendor")
    (create-autoload (string-append (getcwd) "/vendor") composer-file
                     inputs #:dev-dependencies? #t)))

(define (find-bin script inputs)
  (search-input-file inputs
                     (string-append
                      "bin/"
                      (string-drop script (string-length "vendor/bin/")))))

(define* (check #:key composer-file inputs
                tests? test-target test-flags #:allow-other-keys)
  "Test the given package.
Please note that none of the PHP packages at the time of the rewrite of the
build-system did use the test-script field.  This means that the @code{match
test-script} part is not tested on a real example and relies on the original
implementation."
  (if tests?
      (let* ((package-data (read-package-data #:filename composer-file))
             (scripts (composer-package-scripts package-data))
             (test-script (assoc-ref scripts test-target)))
        (match test-script
          ((? string? bin)
           (let ((command (find-bin bin inputs)))
             (unless (zero? (apply system command test-flags))
               (throw 'failed-command command))))
          (('@ (? string? bins) ...)
           (for-each
            (lambda (c)
              (let ((command (find-bin c inputs)))
                (unless (zero? (apply system command test-flags))
                  (throw 'failed-command command))))
            bins))
          (_ (if (file-exists? "phpunit.xml.dist")
                 (apply invoke
                        (with-exception-handler
                            (lambda (exn)
                              (if (search-error? exn)
                                  (error "\
Missing php-phpunit-phpunit native input.~%")
                                  (raise exn)))
                          (lambda ()
                            (search-input-file (or inputs '()) "bin/phpunit")))
                        test-flags))
             (format #t "No test suite found.~%"))))
      (format #t "Test suite not run.~%")))

(define* (create-autoload vendor composer-file inputs #:key dev-dependencies?)
  "creates an autoload.php file that sets up the class locations for this package,
so it can be autoloaded by PHP when the package classes are required."
  (with-output-to-file (string-append vendor "/autoload.php")
    (lambda _
      (display (string-append
                 "<?php
// autoload.php @generated by Guix
$psr4map = $classmap = array();
require_once '" vendor "/autoload_conf.php';
require_once '" (assoc-ref inputs "composer-classloader") "/share/web/composer/ClassLoader.php';
$loader = new \\Composer\\Autoload\\ClassLoader();
foreach ($psr4map as $namespace => $paths) {
    foreach ($paths as $path) {
        $loader->addPsr4($namespace, $path);
    }
}
$loader->addClassMap($classmap);
$loader->register();
"))))
  ;; Now, create autoload_conf.php that contains the actual data, as a set
  ;; of arrays
  (let* ((package-data (read-package-data #:filename composer-file))
         (autoload (composer-package-autoload package-data))
         (autoload-dev (composer-package-autoload-dev package-data))
         (dependencies (composer-package-require package-data))
         (dependencies-dev (composer-package-dev-require package-data)))
    (with-output-to-file (string-append vendor "/autoload_conf.php")
      (lambda _
        (format #t "<?php~%")
        (format #t "// autoload_conf.php @generated by Guix~%")
        (force-output)
        (for-each
         (match-lambda
           ((key . value)
            (let ((vals (if (list? value)
                            (reverse value)
                            (list value))))
              (apply
               format
               #t
               (string-append
                "$psr4map['~a'][] = ["
                (string-join
                 (make-list (length vals) "'~a/../~a'") ",")
                "];~%")
               (cons* (string-join (string-split key #\\) "\\\\")
                      (append-map (lambda (v) (list vendor v)) vals)))))
           (_ (format #t "")))
         (append
          (composer-autoload-psr-4 autoload)
          (if (and dev-dependencies? (not (null? autoload-dev)))
              (composer-autoload-psr-4 autoload-dev)
              '())))
        (for-each
         (lambda (psr0)
           (match psr0
             ((key . value)
              (format #t "$psr4map['~a'][] = ['~a/../~a/~a'];~%"
                      (string-join (string-split key #\\) "\\\\")
                      vendor
                      value
                      (string-join (string-split key #\\) "/")))
             (_ (format #t ""))))
         (append
          (composer-autoload-psr-0 autoload)
          (if (and dev-dependencies? (not (null? autoload-dev)))
              (composer-autoload-psr-0 autoload-dev)
              '())))
        (for-each
         (lambda (classmap)
           (for-each
            (lambda (file)
              (invoke "php" (assoc-ref inputs "findclass.php")
                      "-i" (string-append vendor "/..") "-f" file))
            (find-files classmap ".(php|hh|inc)$")))
         (append
          (composer-autoload-classmap autoload)
          (if (and dev-dependencies? (not (null? autoload-dev)))
              (composer-autoload-classmap autoload-dev)
              '())))
        (for-each
         (lambda (file)
           (format #t "require_once '~a/../~a';~%" vendor file))
         (append
          (composer-autoload-files autoload)
          (if (and dev-dependencies? (not (null? autoload-dev)))
              (composer-autoload-files autoload-dev)
              '())))
        (for-each
         (lambda (dep)
           (format
            #t "require_once '~a';~%"
            (search-input-file
             inputs
             (string-append "/share/web/" dep "/vendor/autoload_conf.php"))))
          dependencies)
        ;; Also add native-inputs that are not necessarily given in the
        ;; composer.json. This allows to simply add a package in tests by
        ;; adding it in native-inputs, without the need to patch composer.json.
        (for-each
         (match-lambda
           ((name . loc)
            (match (find-files loc "autoload_conf\\.php$")
              (() #t)
              (((? string? conf) . ())
               (format #t "require_once '~a';~%" conf))
              (_ #t)))
           (_ #t))
         (or inputs '()))))))

(define* (install #:key inputs outputs composer-file #:allow-other-keys)
  "Install the given package."
  (let* ((out (assoc-ref outputs "out"))
         (package-data (read-package-data #:filename composer-file))
         (name (composer-package-name package-data))
         (php-dir (string-append out "/share/web/" name))
         (bin-dir (string-append php-dir "/vendor/bin"))
         (bin (string-append out "/bin"))
         (binaries (composer-package-binaries package-data)))
      (mkdir-p php-dir)
      (copy-recursively "." php-dir)
      (mkdir-p (string-append php-dir "/vendor"))
      (when binaries
        (mkdir-p bin-dir)
        (mkdir-p bin)
        (for-each
          (lambda (file)
            (let ((installed-file (string-append bin-dir "/" (basename file)))
                  (bin-file (string-append bin "/" (basename file)))
                  (original-file (string-append php-dir "/" file)))
              (symlink original-file installed-file)
              (symlink original-file bin-file)))
          binaries))
      (create-autoload (string-append php-dir "/vendor")
                       composer-file inputs)))

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `configure'
  ;; , `build', `check' and `install' phases.
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'build)
    (delete 'check)
    (replace 'install install)
    (add-after 'install 'check check)
    (add-after 'install 'create-test-autoload create-test-autoload)))

(define* (composer-build #:key inputs (phases %standard-phases)
                         #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; composer-build-system.scm ends here
