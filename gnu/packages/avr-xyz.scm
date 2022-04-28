;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages avr-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages avr)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ruby))

(define-public simavr
  (package
    (name "simavr")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/buserror/simavr")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0njz03lkw5374x1lxrq08irz4b86lzj2hibx46ssp7zv712pq55q"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (replace 'check
                     (lambda* (#:key tests? outputs #:allow-other-keys)
                       (when tests?
                         (invoke "make"
                                 "-C"
                                 "tests"
                                 (string-append "CC=" #$(cc-for-target))
                                 "RELEASE=1"
                                 "run_tests")))))
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           "RELEASE=1"
                           (string-append "PREFIX=" #$output)
                           (string-append "DESTDIR=" #$output))))
    (propagated-inputs
     (list avr-toolchain))
    (native-inputs
     (list autoconf
           which
           git
           automake
           pkg-config
           ncurses
           ruby))
    (inputs
     (list bash-minimal libelf freeglut))
    (home-page "https://github.com/buserror/simavr")
    (synopsis "Lean, mean and hackable simulator for AVR CPUs/MCUs")
    (description
     "simavr is a new AVR simulator for GNU/Linux or any platform that uses
@command{avr-gcc}.  It uses avr-gcc's own register definition to simplify
creating new targets for supported AVR devices.  The core was made to be small
and compact, and hackable so allow quick prototyping of an AVR project.  The
AVR core is now stable for use with parts with <= 128KB flash, and with
preliminary support for the bigger parts.  The simulator loads ELF files
directly, and there is even a way to specify simulation parameterps directly
in the emulated code using an @code{.elf} section.  You can also load
multipart HEX files.")
    (license license:gpl3)))
