;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
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

(define-module (guix build agda-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            agda-build))

(define* (set-locpath #:key inputs native-inputs #:allow-other-keys)
  (let ((locales (assoc-ref (or native-inputs inputs) "locales")))
    (when locales
      (setenv "GUIX_LOCPATH" (string-append locales "/lib/locale")))))

(define %agda-possible-extensions
  (cons
   ".agda"
   (map (cute string-append ".lagda" <>)
        '(""
          ".md"
          ".org"
          ".rst"
          ".tex"))))

(define (pattern-predicate pattern)
  (define compiled-rx (make-regexp pattern))
  (lambda (file stat)
    (regexp-exec compiled-rx file)))

(define* (build #:key plan #:allow-other-keys)
  (for-each
   (match-lambda
     ((pattern . options)
      (for-each
       (lambda (file)
         (apply invoke (cons* "agda" file options)))
       (let ((files (find-files "." (pattern-predicate pattern))))
         (if (null? files)
             (raise
              (make-compound-condition
               (condition
                (&message
                 (message (format #f "Plan pattern `~a' did not match any files"
                                  pattern))))
               (condition
                (&error))))
             files))))
     (x
      (raise
       (make-compound-condition
        (condition
         (&message
          (message (format #f "Malformed plan element `~a'" x))))
        (condition
         (&error))))))
   plan))

(define* (install #:key outputs name extra-files #:allow-other-keys)
  (define libdir (string-append (assoc-ref outputs "out") "/lib/agda/" name))
  (define agda-version
    (car (scandir "./_build/"
                  (lambda (entry)
                    (not (member entry '("." "..")))))))
  (define agdai-files
    (with-directory-excursion
        (string-join (list "." "_build" agda-version "agda") "/")
      (find-files ".")))
  (define (install-source agdai)
    (define dir (dirname agdai))
    ;; Drop .agdai
    (define no-ext (string-drop-right agdai 6))
    (define source
      (match (filter file-exists? (map (cute string-append no-ext <>)
                                       %agda-possible-extensions))
        ((single) single)
        (res (raise
              (make-compound-condition
               (condition
                (&message
                 (message
                  (format #f
                          "Cannot find unique source file for agdai file `~a`, got `~a`"
                          agdai res))))
               (condition
                (&error)))))))
    (install-file source (string-append libdir "/" dir)))
  (for-each install-source agdai-files)
  (copy-recursively "_build" (string-append libdir "/_build"))
  (for-each
   (lambda (pattern)
     (for-each
      (lambda (file)
        (install-file file libdir))
      (find-files "." (pattern-predicate pattern))))
   extra-files))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (add-before 'install-locale 'set-locpath set-locpath)
    (delete 'bootstrap)
    (delete 'configure)
    (replace 'build build)
    (delete 'check) ;; No universal checker
    (replace 'install install)))

(define* (agda-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  "Build the given Agda package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
