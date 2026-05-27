;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages heads)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages compiler-tools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages python)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages file)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (srfi srfi-1))

;; This package provides a "dev.cpio" file usable as a base for booting Heads.
(define-public heads-dev-cpio
  (package
    (name "heads-dev-cpio")
    (version "0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build syscalls)
                  (guix cpio))
       #:builder (begin
                   (use-modules (guix build utils)
                                (guix cpio)
                                (srfi srfi-26))
                   (mkdir-p "dev") ; input directory.
                   (let* ((out (assoc-ref %outputs "out"))
                          (libexec (string-append out "/libexec")))
                     (mkdir-p libexec)
                     (call-with-output-file (string-append libexec "/dev.cpio")
                      (lambda (port)
                        (write-cpio-archive '("dev" "dev/console") port
                         #:file->header
                         (lambda (name)
                           (if (string=? "dev/console" name)
                               (special-file->cpio-header* name 'char-special 5 1 #o600)
                               (file->cpio-header* name))))))
                     #t))))
    (synopsis "@file{dev.cpio} for Heads")
    (description "This package provides a @file{dev.cpio} file usable as a
base for heads' initrd.")
    (home-page "https://osresearch.net/")
    (license license:bsd-2)))
