;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages avr)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
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
     (list (make-avr-toolchain)))
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

(define-public lufa
  (package
    (name "lufa")
    (version "210130")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/abcminiuser/lufa")
                    (commit (string-append "LUFA-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ylr7qsiikcy827k18zj1vdzf0kb8hb0gjmifd75y8krkhhar49g"))))
    (outputs '("bootloaders" "demos" "projects" "doc"))
    (build-system gnu-build-system)
    (arguments
     ;; There are tests, but even LUFA's own CI doesn't run them (they are
     ;; only built).
     (list
      #:tests? #f
      #:target "avr"
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match)
                  (srfi srfi-26))
      #:make-flags
      #~(list (string-append "SHELL="(search-input-file %build-inputs
                                                        "bin/sh")))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (add-before 'build 'build-documentation
                     (lambda* (#:key make-flags #:allow-other-keys)
                       (apply invoke "make" "-j" (number->string
                                                  (or (parallel-job-count)
                                                      1))
                              "doxygen"
                              ;; Ignore errors (see:
                              ;; https://github.com/abcminiuser/lufa/issues/90).
                              "-i"
                              make-flags)))
                   (replace 'install
                     ;; There is no default install target as the library is
                     ;; intended to be integrated in source form in a
                     ;; project.  Install the example projects and demos
                     ;; binaries as well as the documentation.
                     (lambda _
                       (let ((doc (string-append #$output:doc
                                                 "/share/doc/lufa/"))
                             (html-dirs (find-files "."  "^html$"
                                                    #:directories? #t)))
                         (for-each (cut install-file <> #$output:bootloaders)
                                   (find-files "Bootloaders" "\\.hex$"))
                         (for-each (cut install-file <> #$output:demos)
                                   (find-files "Demos" "\\.hex$"))
                         (for-each (cut install-file <> #$output:projects)
                                   (find-files "Projects" "\\.hex$"))
                         ;; Install Doxygen generated HTML documentation.
                         (for-each
                          (lambda (html)
                            (let* ((suffix "Documentation/html")
                                   (l (string-length suffix))
                                   (dest (string-append
                                          doc
                                          (string-drop
                                           (if (string-suffix? suffix html)
                                               (string-drop-right html l)
                                               (error "unexpected path" html))
                                           1)))) ;drop leading "."
                              (mkdir-p dest)
                              (copy-recursively html dest)))
                          html-dirs)))))))
    (native-inputs (list doxygen))
    (home-page "https://www.lufa-lib.org/")
    (synopsis "Lightweight USB Framework for AVRs")
    (description "UFA is a simple to use, lightweight framework which sits
atop the hardware USB controller in specific AVR microcontroller models, and
allows for the quick and easy creation of complex USB devices and hosts.  This
package contains the user-submitted projects and bootloaders for use with
compatible microcontroller models, as well as the demos and the
documentation.")
    (license license:expat)))           ;see LUFA/License.txt

(define-public microscheme
  (package
    (name "microscheme")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ryansuchocki/microscheme")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1bflwirpcd58bngbs6hgjfwxl894ni2gpdd4pj10pm2mjhyj5dgw"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list #:parallel-build? #f                   ;fails to build otherwise
           #:tests? #f                            ;no tests
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))

           ;; The 'check' target runs 'cppcheck' and 'clang-format', but it fails
           ;; unless given an old version of the former, such as 2.10.3.  Since
           ;; the 'all' target depends on 'check', explicitly ask for 'build'.
           #:make-flags #~(list "build"
                                (string-append "PREFIX=" #$output))))
    (native-inputs (list unzip xxd))
    (home-page "https://github.com/ryansuchocki/microscheme/")
    (synopsis "Scheme subset for Atmel microcontrollers")
    (description
     "Microscheme, or @code{(ms)} for short, is a functional programming
language for the Arduino, and for Atmel 8-bit AVR microcontrollers in general.
Microscheme is a subset of Scheme, in the sense that every valid @code{(ms)}
program is also a valid Scheme program (with the exception of Arduino
hardware-specific primitives).  The @code{(ms)} compiler performs function
inlining, and features an aggressive tree-shaker, eliminating unused top-level
definitions.  Microscheme has a robust @dfn{Foreign Function Interface} (FFI)
meaning that C code may be invoked directly from (ms) programs.")
    (license license:expat)))
