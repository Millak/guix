#!/bin/sh
exec guile -e main -s "$0" "$@"
!#

;;; GNU Guix --- Functional package management for GNU
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

(use-modules (guix packages)
             (guix gexp)
             (gnu services)
             (gnu packages)
             ((gnu services xorg) #:select (%default-xorg-modules))
             ((gnu system) #:select (%base-packages %base-firmware))
             (gnu tests install)
             (ice-9 match)
             ((ice-9 session) #:select (procedure-arguments))
             (guix utils)
             (srfi srfi-26)
             (srfi srfi-1)
             (ice-9 receive))

;; Thunked fields hold a procedure of exactly one argument: the record.
;; This cannot be told apart from a plain one-argument procedure field.
(define* (thunked-field? field obj)
  (and (procedure? obj)
       (match (procedure-arguments obj)
         ;; One required argument. No optional, keyword or rest arguments.
         ((('required _) ('optional) ('keyword) _ ('rest . #f)) #t)
         (_ #f))))

;; Returns a heterogenous list of packages or services.
;; Each service is followed by the list of packages it depends on.
(define* (all-packages obj #:optional (pred package?))
  (match obj
    ((? pred)
     (list obj))
    ((? gexp?)
     (list))
    ((a . b)
     (append (all-packages a pred)
             (all-packages b pred)))
    ((_ ...)
     (apply append
            (map (cut all-packages <> pred)
                 obj)))
    (#(_ ...)
     (all-packages (vector->list obj) pred))
    ((or (? service-type?)
         (? origin?))
     (list))
    ((? record?)
     (let* ((record-type (record-type-descriptor obj))
            (record-fields (record-type-fields record-type))
            (child-pkgs (apply append
                               (map (lambda (field)
                                      (let* ((accessor (record-accessor record-type field))
                                             (field-obj (accessor obj)))
                                        (if (thunked-field? field field-obj)
                                            (let ((value (field-obj obj)))
                                              ;; A procedure-valued field such as
                                              ;; 'transformation' maps a record to another
                                              ;; record of the same type; don't descend to prevent
                                              ;; infinite identity loops
                                              (if (and (record? value)
                                                       (eq? (record-type-descriptor value)
                                                            record-type))
                                                  '()
                                                  (all-packages value pred)))
                                            (all-packages field-obj pred))))
                                    record-fields))))
       (if (service? obj)
           (cons obj child-pkgs)
           child-pkgs)))
    ((? parameter?)
     (list))
    (_
     (list))))

;; Remove from pkgs-or-services all packages which do not need to be named
;; explicitly because they are pulled in by another listed package.
(define (without-dependent-packages pkgs-or-services)
  (define all-inputs
    (apply append
           (map
            (lambda (pkg-or-service)
              (if (package? pkg-or-service)
                  ;; package-propagated-inputs returns a label as well as the package.
                  ;; second gives us just the package
                  (map second (package-propagated-inputs pkg-or-service))
                  (list)))
            pkgs-or-services)))
  (filter
   (lambda (pkg) (not (member pkg all-inputs)))
   pkgs-or-services))

(define (without-duplicates pkgs-or-services)
  (delete-duplicates pkgs-or-services))

(define (without-minimal-packages pkgs-or-services)
  (filter
   (lambda (pkg-or-service)
     (not (and (package? pkg-or-service)
               (member pkg-or-service (all-packages minimal-installer-os)))))
   pkgs-or-services))

;; Removes all services which, as a result of previous cleaning, no longer require any packages.
(define (without-redundant-services pkgs-or-services)
  (define (without-tail-services pkgs-or-services)
    (reverse (drop-while service? (reverse pkgs-or-services))))
  (without-tail-services
   (fold-right (lambda (pkg-or-service result)
                 (if (and (service? pkg-or-service) (service? (first result)))
                     result
                     (cons pkg-or-service result)))
               (list (last pkgs-or-services))
               pkgs-or-services)))

;; Packages not attributed to any service by all-packages.
(define (lone-packages pkgs-or-services)
  (take-while package? pkgs-or-services))

(define (without-lone-packages pkgs-or-services)
  (drop-while package? pkgs-or-services))

;; Many inputs are implied by gnome, so including it makes the list easier to review.
(define (with-gnome pkgs-or-services)
  (receive (head tail)
      (break (lambda (pkg-or-service)
               (and (service? pkg-or-service)
                    (equal? (service-type-name (service-kind pkg-or-service)) 'gnome-desktop)))
             pkgs-or-services)
    (append
     head
     (list (car tail) ; gnome-desktop-service
           (specification->package "gnome"))
     (cdr tail))))    ; the remaining packages, most of which will be cleaned out as redundant inputs

; %default-xorg-modules is added separately to keep the list of copied packages short.
(define (without-xorg-modules pkgs-or-services)
  (filter (lambda (pkg-or-service)
            (not (member pkg-or-service %default-xorg-modules)))
          pkgs-or-services))

; %base-packages is added separately to keep the list of copied packages short.
(define (without-base pkgs-or-services)
  (filter (lambda (pkg-or-service)
            (and (not (member pkg-or-service %base-firmware))
                 (not (member pkg-or-service %base-packages))))
          pkgs-or-services))

;; Returns the shortest specification string which will uniquely identify pkg
;; via specification->package. Errors if there is no public specification for
;; pkg.
(define (minimal-specification pkg)
  (define (build-specification name version-parts)
    (if (null? version-parts)
        name
        (format #f "~a@~a" name (string-join version-parts "."))))

  (define (sufficient-spec? name version-parts)
    (equal? pkg (specification->package (build-specification name version-parts))))

  (define (generate-spec name versions-used versions-remaining)
    (if (sufficient-spec? name versions-used)
        (build-specification name versions-used)
        ; recursion will error if no package spec is found
        (generate-spec name
                       (append versions-used (list (car versions-remaining)))
                       (cdr versions-remaining))))

  (generate-spec (package-name pkg) '() (string-split (package-version pkg) #\.)))

(define (print-pkg-service-list pkgs-or-services)
  (define (print pkg-or-service)
         (match pkg-or-service
           ((? package?) (format #t "\"~a\"~%" (minimal-specification pkg-or-service)))
           ((? service?) (format #t ";; ~a-service~%" (service-type-name (service-kind pkg-or-service))))))
  (map print (without-lone-packages pkgs-or-services))
  (format #t ";; packages~%")
  (map print (lone-packages pkgs-or-services)))

(define (cleaned-packages-full pkgs-or-services)
  ((compose without-redundant-services
           without-duplicates
           without-dependent-packages
           without-minimal-packages
           without-xorg-modules
           with-gnome) pkgs-or-services))

(define (cleaned-packages-minimal pkgs-or-services)
  ((compose without-redundant-services
            without-duplicates
            without-dependent-packages
            without-base) pkgs-or-services))

(define (print-all-packages obj)
  (print-pkg-service-list (all-packages obj)))

(define full-installer-os
  ((@@ (gnu tests install) installation-target-desktop-os-for-gui-tests)))

(define minimal-installer-os
  ((@@ (gnu tests install) installation-target-os-for-gui-tests)))

(define (print-desktop-manifest-packages)
  (print-pkg-service-list (cleaned-packages-full (all-packages full-installer-os))))

(define (print-minimal-manifest-packages)
  (print-pkg-service-list (cleaned-packages-minimal (all-packages minimal-installer-os))))

(define (print-usage)
  (format (current-error-port) "Usage: os-manifest-generator.scm MANIFEST_TO_PRINT

This script can be used to generate the minimal and desktop release manifests.
MANIFEST_TO_PRINT may equal `desktop`, to print the desktop manifest, or `minimal`, to print the minimal manifest.

Refer to the files etc/release/release-manifest.scm and etc/release/release-desktop.scm to see what to do with the output.
"))

(define (main . args)
  (match (cdar args)
    (("desktop") (print-desktop-manifest-packages))
    (("minimal") (print-minimal-manifest-packages))
    (anything (print-usage))))
