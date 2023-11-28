;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages avr)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix memoization)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages gcc)
  #:export (make-avr-libc
            make-avr-toolchain))

;;; Commentary:
;;;
;;; This module defines a procedure that can be used to create a complete
;;; avr-toolchain package.  The procedure must not be used at the top level,
;;; to avoid cyclic module dependencies caused by the (gnu packages
;;; cross-base) module referring to top level bindings from (gnu packages
;;; gcc).
;;;
;;; It also contains packages for working with or targeting the AVR system.
;;;

(define make-avr-binutils
  (mlambda ()
    (package
      (inherit (cross-binutils "avr"))
      (name "avr-binutils"))))

(define* (make-avr-gcc/implementation #:key (xgcc gcc))
  "Return a XGCC-base cross-compiler for the AVR target."
  (let ((xgcc (cross-gcc "avr" #:xgcc xgcc #:xbinutils (make-avr-binutils))))
    (package
      (inherit xgcc)
      (name "avr-gcc")
      (arguments
       (substitute-keyword-arguments (package-arguments xgcc)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((gcc (assoc-ref inputs  "gcc")))
                    ;; Remove the default compiler from CPLUS_INCLUDE_PATH
                    ;; to prevent header conflict with the GCC from
                    ;; native-inputs.
                    (setenv "CPLUS_INCLUDE_PATH"
                            (string-join
                             (delete (string-append gcc "/include/c++")
                                     (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                   #\:))
                             ":"))
                    (format #t
                            "environment variable `CPLUS_INCLUDE_PATH' \
changed to ~a~%"
                            (getenv "CPLUS_INCLUDE_PATH")))))))))
      (native-search-paths
       (list (search-path-specification
              (variable "CROSS_C_INCLUDE_PATH")
              (files '("avr/include")))
             (search-path-specification
              (variable "CROSS_CPLUS_INCLUDE_PATH")
              (files '("avr/include")))
             (search-path-specification
              (variable "CROSS_OBJC_INCLUDE_PATH")
              (files '("avr/include")))
             (search-path-specification
              (variable "CROSS_OBJCPLUS_INCLUDE_PATH")
              (files '("avr/include")))
             (search-path-specification
              (variable "CROSS_LIBRARY_PATH")
              (files '("avr/lib")))))
      (native-inputs
       `(("gcc" ,gcc)
         ,@(package-native-inputs xgcc))))))

(define make-avr-gcc
  (memoize make-avr-gcc/implementation))

(define* (make-avr-libc/implementation #:key
                                       (xbinutils (cross-binutils "avr"))
                                       (xgcc (cross-gcc "avr"
                                                        #:xbinutils xbinutils)))
  (package
    (name "avr-libc")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah//avr-libc/avr-libc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "15svr2fx8j6prql2il2fc0ppwlv50rpmyckaxx38d3gxxv97zpdj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:target "avr"
       #:out-of-source? #t
       ;; Avoid including itself as this package is a target input and cannot
       ;; use the normal cross compilation inputs.
       #:implicit-cross-inputs? #f))
    (native-inputs `(("cross-binutils" ,xbinutils)
                     ("cross-gcc" ,xgcc)))
    (home-page "https://www.nongnu.org/avr-libc/")
    (synopsis "AVR C Library")
    (description
     "AVR Libc is a project whose goal is to provide a high quality C library
for use with GCC on Atmel AVR microcontrollers.")
    (license
     (license:non-copyleft "http://www.nongnu.org/avr-libc/LICENSE.txt"))))

(define make-avr-libc
  (memoize make-avr-libc/implementation))

(define* (make-avr-toolchain/implementation #:key (xgcc gcc))
  (let ((avr-binutils (make-avr-binutils))
        (avr-libc (make-avr-libc #:xgcc (cross-gcc "avr" #:xgcc xgcc)))
        (avr-gcc (make-avr-gcc #:xgcc xgcc)))
    ;; avr-libc checks the compiler version and passes "--enable-device-lib"
    ;; for avr-gcc > 5.1.0.  It wouldn't install the library for atmega32u4
    ;; etc if we didn't use the corret avr-gcc.
    (package
      (name "avr-toolchain")
      (version (package-version avr-gcc))
      (source #f)
      (build-system trivial-build-system)
      (arguments '(#:builder (begin (mkdir %output) #t)))
      (propagated-inputs
       `(("avrdude" ,avrdude)
         ("binutils" ,avr-binutils)
         ("gcc" ,avr-gcc)
         ("libc" ,avr-libc)))
      (synopsis "Complete GCC tool chain for AVR microcontroller development")
      (description "This package provides a complete GCC tool chain for AVR
microcontroller development.  This includes the GCC AVR cross compiler and
avrdude for firmware flashing.  The supported programming languages are C and
C++.")
      (home-page (package-home-page avr-libc))
      (license (package-license avr-gcc)))))

(define make-avr-toolchain
  (memoize make-avr-toolchain/implementation))
