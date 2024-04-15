;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Richard Sent <richard@freakingpenguin.com>.
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

(define-module (gnu tests sddm)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:export (%test-sddm))

(define %sddm-os
  (operating-system
    (inherit %simple-os)
    (services
     (cons* (service sddm-service-type)
            (modify-services %desktop-services
              (delete gdm-service-type))))))

(define (run-sddm-test)
  "Run tests in %SDDM-OS."

  (define os (marionette-operating-system
              %sddm-os
              #:imported-modules (source-module-closure
                                  '((gnu services herd)))))

  (define vm (virtual-machine
              (operating-system os)
              ;; Test may nondeterministically fail with default memory size.
              (memory-size 1024)))

  (define test
    (with-imported-modules (source-module-closure
                            '((gnu build marionette)))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-26)
                       (srfi srfi-64))

          (let ((marionette (make-marionette (list #$vm))))

            (test-runner-current (system-test-runner #$output))
            (test-begin "sddm")

            (test-assert "service is running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  ;; sddm's shepherd service is called xorg-server
                  (start-service 'xorg-server))
               marionette))

            (test-assert "service can be stopped"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (stop-service 'xorg-server))
               marionette))

            (test-assert "service can be restarted"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (restart-service 'xorg-server))
               marionette))

            (test-assert "login screen up"
              ;; GNU Ocrad fails to recognize any text, so use Tesseract.
              (wait-for-screen-text marionette
                                    ;; Login button not recognized, use SDDM's
                                    ;; welcome message.
                                    (cut string-contains <> (string-append "Welcome to "
                                                                           #$(operating-system-host-name %sddm-os)))
                                    #:ocr #$(file-append tesseract-ocr
                                                         "/bin/tesseract")
                                    #:timeout 60))

            (test-end)))))

  (gexp->derivation "sddm-test" test))

(define %test-sddm
  (system-test
   (name "sddm")
   (description "Basic tests for the SDDM service.")
   (value (run-sddm-test))))
