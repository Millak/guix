;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (gnu tests emacs)
  #:use-module (gnu tests)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages vim)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (%test-emacs-native-comp-replacable))

(define (run-native-comp-replacable-test old-emacs new-emacs)
  (define vm (virtual-machine (marionette-operating-system %simple-os)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-1)
                       (srfi srfi-64))

          (define marionette (make-marionette (list #$vm)))
          (define (marionette-emacs-eval emacs code)
            (marionette-eval
             `(begin
                (use-modules (ice-9 rdelim) (ice-9 popen))
                (read-line
                 (open-pipe*
                  OPEN_READ
                  ,emacs "--batch"
                  ,(string-append "--eval=" code))))
             marionette))

          (define (emacs-native-comp-dir emacs)
            (marionette-emacs-eval emacs "(princ comp-native-version-dir)"))
          (define (emacs-abi-hash emacs)
            (marionette-emacs-eval emacs "(princ comp-abi-hash)"))
          (define (emacs-effective-version emacs)
            (marionette-emacs-eval
             emacs
             "(princ
               (format \"%s.%s\" emacs-major-version emacs-minor-version))"))

          (define old-emacs-bin #$(file-append old-emacs "/bin/emacs"))
          (define new-emacs-bin #$(file-append new-emacs "/bin/emacs"))

          (test-runner-current (system-test-runner #$output))
          (test-begin "emacs-native-comp-replacable")
          (test-equal "comp-abi-hash"
            (emacs-abi-hash old-emacs-bin)
            (emacs-abi-hash new-emacs-bin))
          (test-equal "native-comp-dir"
            (emacs-native-comp-dir old-emacs-bin)
            (emacs-native-comp-dir new-emacs-bin))
          (test-assert "old emacs has hierarchical layout"
            (file-exists?
             (string-append #$old-emacs "/lib/emacs/"
                            (emacs-effective-version old-emacs-bin)
                            "/native-lisp/"
                            (emacs-native-comp-dir old-emacs-bin)
                            "/preloaded/emacs-lisp/comp.eln")))
          (test-assert "new emacs has hierarchical layout"
            (file-exists?
             (string-append #$new-emacs "/lib/emacs/"
                            (emacs-effective-version new-emacs-bin)
                            "/native-lisp/"
                            (emacs-native-comp-dir new-emacs-bin)
                            "/preloaded/emacs-lisp/comp.eln")))
          (test-end))))

  (gexp->derivation "emacs-native-comp-compatible" test))

(define (package-without-replacement pkg)
  (package (inherit pkg) (replacement #f)))

(define %test-emacs-native-comp-replacable
  (system-test
   (name "emacs-native-comp")
   (description "Test whether an emacs replacement (if any) is valid.")
   (value (run-native-comp-replacable-test
           (package-without-replacement emacs)
           emacs))))
