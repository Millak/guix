;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2020, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Denis Carikli <GNUtoo@cyberdimension.org>
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

(define-module (gnu packages valgrind)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages perl))

(define-public valgrind
  (package
    (name "valgrind")
    ;; Note: check "guix refresh -l -e '(@ (gnu packages valgrind) valgrind)'"
    ;; when updating this package to find which branch it should go to.
    (version "3.17.0")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://sourceware.org/pub/valgrind"
                                        "/valgrind-" version ".tar.bz2")
                         (string-append "ftp://sourceware.org/pub/valgrind"
                                        "/valgrind-" version ".tar.bz2")))
              (sha256
               (base32
                "18l5jbk301j3462gipqn9bkfx44mdmwn0pwr73r40gl1irkfqfmd"))
              (patches (search-patches "valgrind-enable-arm.patch"))))
    (build-system gnu-build-system)
    (outputs '("doc"                              ;16 MB
               "out"))
    (arguments
     `(,@(if (string-prefix? "powerpc" (or (%current-target-system)
                                           (%current-system)))
           `(#:make-flags '("CFLAGS+=-maltivec"))
           '())
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-suppression-files
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Don't assume the FHS.
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/lib/valgrind")))
               (substitute* (find-files dir "\\.supp$")
                 (("obj:/lib") "obj:*/lib")
                 (("obj:/usr/X11R6/lib") "obj:*/lib")
                 (("obj:/usr/lib") "obj:*/lib"))
               #t)))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((orig (format #f "~a/share/doc" (assoc-ref outputs "out")))
                   (dest (format #f "~a/share" (assoc-ref outputs "doc"))))
               (mkdir-p dest)
               (rename-file orig dest)
               #t))))))
    (native-inputs
     (list perl))
    (home-page "https://www.valgrind.org/")
    (synopsis "Debugging and profiling tool suite")
    (description
     "Valgrind is an instrumentation framework for building dynamic analysis
tools.  There are Valgrind tools that can automatically detect many memory
management and threading bugs, and profile your programs in detail.  You can
also use Valgrind to build new tools.")
    ;; https://valgrind.org/info/platforms.html
    (supported-systems (delete "riscv64-linux" %supported-systems))
    (license gpl2+)

    ;; Hide this variant so end users get the "interactive" Valgrind below.
    (properties '((hidden? . #t)))))

(define-public valgrind/interactive
  (package/inherit
   valgrind
   (version "3.17.0")
   (source (origin
             (method url-fetch)
             (uri (list (string-append "https://sourceware.org/pub/valgrind"
                                       "/valgrind-" version ".tar.bz2")
                        (string-append "ftp://sourceware.org/pub/valgrind"
                                       "/valgrind-" version ".tar.bz2")))
             (sha256
              (base32
               "18l5jbk301j3462gipqn9bkfx44mdmwn0pwr73r40gl1irkfqfmd"))
             (patches (search-patches
                       "valgrind-enable-arm.patch"
                       "valgrind-fix-default-debuginfo-path.patch"))))
   (inputs
    ;; GDB is needed to provide a sane default for `--db-command'.
    (list gdb `(,(canonical-package glibc) "debug")))
   (arguments
    (substitute-keyword-arguments (package-arguments valgrind)
      ((#:phases phases #~%standard-phases)
       #~(modify-phases #$phases
           (add-before 'configure 'patch-default-debuginfo-path
             (lambda* (#:key inputs #:allow-other-keys)
               ;; This helps Valgrind find the debug symbols of ld.so.
               ;; Without it, Valgrind does not work in a Guix shell
               ;; container and cannot be used as-is during packages tests
               ;; phases.
               ;; TODO: Remove on the next rebuild cycle, when libc is not
               ;; longer fully stripped.
               (define libc-debug
                 (string-append (ungexp (this-package-input "glibc") "debug")
                                "/lib/debug"))

               (substitute* '("coregrind/m_debuginfo/readelf.c"
                              "docs/xml/manual-core-adv.xml"
                              "docs/xml/manual-core.xml")
                 (("DEFAULT_DEBUGINFO_PATH")
                  libc-debug))
               ;; We also need to account for the bigger path in
               ;; the malloc-ed variables.
               (substitute* '("coregrind/m_debuginfo/readelf.c")
                 (("DEBUGPATH_EXTRA_BYTES_1")
                  (number->string
                   (+ (string-length libc-debug)
                      (string-length "/.build-id//.debug")
                      1))))
               (substitute* '("coregrind/m_debuginfo/readelf.c")
                 (("DEBUGPATH_EXTRA_BYTES_2")
                  (number->string
                   (+ (string-length libc-debug)
                      (string-length "/usr/lib/debug")
                      1))))))))))
   (properties '())))

(define-public valgrind-3.18
  (package
    (inherit valgrind/interactive)
    (version "3.18.1")
    (source (origin
              (inherit (package-source valgrind/interactive))
              (uri (list (string-append "https://sourceware.org/pub/valgrind"
                                        "/valgrind-" version ".tar.bz2")
                         (string-append "ftp://sourceware.org/pub/valgrind"
                                        "/valgrind-" version ".tar.bz2")))
              (sha256
               (base32
                "1xgph509i6adv9w2glviw3xrmlz0dssg8992hbvxsbkp7ahrm180"))))))
