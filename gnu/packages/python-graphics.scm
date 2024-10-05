;;; GNU Guix --- Functional package management for GNU

(define-module (gnu packages python-graphics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build-system python) #:select (pypi-uri))
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz))

;;; Commentary:
;;;
;;; Python modules which are meant to be used in GUI creation or mainly are
;;; bindings to low level libraries such as Glue, Mesa, OpenGL, Xorg etc.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:


;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetical order.
;;;
