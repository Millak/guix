;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer newt services)
  #:use-module (gnu installer services)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt utils)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (newt)
  #:export (run-services-page))

(define (run-desktop-environments-cbt-page)
  "Run a page allowing the user to choose between various desktop
environments."
  (run-checkbox-tree-page
   #:info-text (G_ "Please select the desktop(s) environment(s) you wish to \
install. If you select multiple desktops environments, we will be able to \
choose the one to use on the log-in screen with F1.")
   #:title (G_ "Desktop environment")
   #:items %desktop-environments
   #:item->text desktop-environment-name
   #:checkbox-tree-height 5
   #:cancel-button-callback-procedure
   (lambda ()
     (raise
      (condition
       (&installer-step-abort))))))

(define (run-services-page)
  (run-desktop-environments-cbt-page))
