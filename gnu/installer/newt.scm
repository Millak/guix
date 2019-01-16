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

(define-module (gnu installer newt)
  #:use-module (gnu installer record)
  #:use-module (gnu installer newt ethernet)
  #:use-module (gnu installer newt final)
  #:use-module (gnu installer newt hostname)
  #:use-module (gnu installer newt keymap)
  #:use-module (gnu installer newt locale)
  #:use-module (gnu installer newt menu)
  #:use-module (gnu installer newt network)
  #:use-module (gnu installer newt services)
  #:use-module (gnu installer newt timezone)
  #:use-module (gnu installer newt user)
  #:use-module (gnu installer newt utils)
  #:use-module (gnu installer newt welcome)
  #:use-module (gnu installer newt wifi)
  #:use-module (guix discovery)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-26)
  #:use-module (newt)
  #:export (newt-installer))

(define (init)
  (newt-init)
  (clear-screen)
  (set-screen-size!))

(define (exit)
  (newt-finish))

(define (exit-error key . args)
  (newt-finish))

(define (final-page result prev-steps)
  (run-final-page result prev-steps))

(define* (locale-page #:key
                      supported-locales
                      iso639-languages
                      iso3166-territories)
  (run-locale-page
   #:supported-locales supported-locales
   #:iso639-languages iso639-languages
   #:iso3166-territories iso3166-territories))

(define (timezone-page zonetab)
  (run-timezone-page zonetab))

(define (welcome-page logo)
  (run-welcome-page logo))

(define (menu-page steps)
  (run-menu-page steps))

(define* (keymap-page layouts)
  (run-keymap-page layouts))

(define (network-page)
  (run-network-page))

(define (hostname-page)
  (run-hostname-page))

(define (user-page)
  (run-user-page))

(define (services-page)
  (run-services-page))

(define newt-installer
  (installer
   (name 'newt)
   (init init)
   (exit exit)
   (exit-error exit-error)
   (final-page final-page)
   (keymap-page keymap-page)
   (locale-page locale-page)
   (menu-page menu-page)
   (network-page network-page)
   (timezone-page timezone-page)
   (hostname-page hostname-page)
   (user-page user-page)
   (services-page services-page)
   (welcome-page welcome-page)))
