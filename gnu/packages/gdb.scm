;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2019, 2020, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2019, 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 Zheng Junjie <z572@z572.online>
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

(define-module (gnu packages gdb)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (srfi srfi-1))

(define-public gdb/pinned
  ;; This is the fixed version that packages depend on.  Update it rarely
  ;; enough to avoid massive rebuilds.
  (package
    (name "gdb")
    (version "12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gdb/gdb-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1vczsqcbh5y0gx7qrclpna0qzx26sk7lra6y8qzxam1biyzr65qf"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (arguments
     (list
      #:tests? #f                       ;FIXME: 217 unexpected failures
      #:out-of-source? #t
      #:modules `((srfi srfi-1)
                  ,@%default-gnu-modules)
      #:configure-flags (if (target-hurd64?)
                            #~'("--enable-targets=i586-pc-gnu,x86_64-pc-gnu")
                            #~'())
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-paths
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let ((sh (string-append (assoc-ref inputs "bash")
                                                "/bin/sh")))
                         (substitute* '("gdb/ser-pipe.c"
                                        "gdbsupport/pathstuff.cc")
                           (("\"/bin/sh\"")
                            (format #f "~s" sh))))))
                   (add-after 'configure 'post-configure
                     (lambda _
                       (for-each patch-makefile-SHELL
                                 (find-files "." "Makefile\\.in"))))
                   (add-after 'install 'remove-libs-already-in-binutils
                     (lambda* (#:key native-inputs inputs outputs
                               #:allow-other-keys)
                       ;; Like Binutils, GDB installs libbfd, libopcodes, etc.
                       ;; However, this leads to collisions when both are
                       ;; installed, and really is none of its business,
                       ;; conceptually.  So remove them.
                       (let* ((binutils (or (assoc-ref inputs "binutils")
                                            (assoc-ref native-inputs "binutils")))
                              (out      (assoc-ref outputs "out"))
                              (files1   (with-directory-excursion binutils
                                          (append (find-files "lib")
                                                  (find-files "include"))))
                              (files2   (with-directory-excursion out
                                          (append (find-files "lib")
                                                  (find-files "include"))))
                              (common   (lset-intersection string=?
                                                           files1 files2)))
                         (with-directory-excursion out
                           (for-each delete-file common))))))))
    (inputs
     `(("bash" ,bash)
       ("expat" ,expat)
       ("mpfr" ,mpfr)
       ("gmp" ,gmp)
       ("readline" ,readline)
       ("ncurses" ,ncurses)
       ("guile" ,guile-3.0)
       ("python-wrapper" ,python-wrapper)
       ("source-highlight" ,source-highlight)

       ;; Allow use of XML-formatted syscall information.  This enables 'catch
       ;; syscall' and similar commands.
       ("libxml2" ,libxml2)

       ;; The Hurd needs -lshouldbeinlibc.
       ,@(if (target-hurd?)
             `(("hurd" ,hurd))
             '())))
    (native-inputs
     `(("texinfo" ,texinfo)
       ("dejagnu" ,dejagnu)
       ("pkg-config" ,pkg-config)
       ,@(if (target-hurd?)
             ;; When cross-compiling from x86_64-linux, make sure to use a
             ;; 32-bit MiG because we assume target i586-pc-gnu.
             `(("mig" ,(if (%current-target-system)
                           (cross-mig (%current-target-system))
                           mig)))
             '())))
    ;; TODO: Add support for the GDB_DEBUG_FILE_DIRECTORY environment
    ;; variable in GDB itself instead of relying on some glue code in
    ;; the Guix-provided .gdbinit file.
    (native-search-paths (list (search-path-specification
                                (variable "GDB_DEBUG_FILE_DIRECTORY")
                                (files '("lib/debug")))))
    (home-page "https://www.gnu.org/software/gdb/")
    (synopsis "The GNU debugger")
    (description
     "GDB is the GNU debugger.  With it, you can monitor what a program is
doing while it runs or what it was doing just before a crash.  It allows you
to specify the runtime conditions, to define breakpoints, and to change how
the program is running to try to fix bugs.  It can be used to debug programs
written in C, C++, Ada, Objective-C, Pascal and more.")
    (properties `((hidden? . #t)))
    (license gpl3+)))

(define-public gdb-14
  (package
    (inherit gdb/pinned)
    (version "14.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gdb/gdb-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0wkprsjyyh204fdjlkaz20k847l88i9y8m9zqsv15vcd3l3dhk9d"))))
    (properties '())))

(define-public gdb-15
  (package
    (inherit gdb-14)
    (version "15.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gdb/gdb-"
                                  version ".tar.xz"))
              (patches (search-patches "gdb-hurd64.patch"))
              (sha256
               (base32
                "0k9i8mizg4hby020k53kqmc835pajh9c8d5klv5s1ddm6p6hqdc3"))))))

(define-public gdb-16
  (package
    (inherit gdb-14)
    (version "16.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gdb/gdb-"
                                  version ".tar.xz"))
              (patches (search-patches "gdb-16-hurd64.patch"))
              (sha256
               (base32
                "1i940b04404xr44xc66c4r4nk091czqz7zzrmhbpk64aaaax1z5w"))))))

(define-public gdb
  ;; The "default" version.
  gdb-14)

(define-public gdb-multiarch
  (package/inherit gdb-14
    (name "gdb-multiarch")
    (arguments
     (substitute-keyword-arguments (package-arguments gdb-14)
       ((#:configure-flags flags '())
        #~(cons* "--enable-targets=all"
                 "--enable-multilib"
                 "--enable-interwork"
                 "--enable-languages=c,c++"
                 "--disable-nls"
                 #$flags))))
    (synopsis "The GNU debugger (with all architectures enabled)")))

(define-public gdb-minimal
  (package/inherit gdb-14
    (name "gdb-minimal")
    (inputs (fold alist-delete (package-inputs gdb)
                  '("libxml2" "ncurses" "python-wrapper" "source-highlight")))))

(define-public gdb-minimal-15
  (package/inherit gdb-15
    (name "gdb-minimal")
    (inputs (fold alist-delete (package-inputs gdb-15)
                  '("libxml2" "ncurses" "python-wrapper" "source-highlight")))))

(define-public avr-gdb
  (package/inherit gdb-14
    (name "avr-gdb")
    (arguments
     (substitute-keyword-arguments (package-arguments gdb-14)
       ((#:configure-flags flags '())
        #~(cons* "--target=avr"
                 "--disable-nls"
                 "--enable-languages=c,c++"
                 "--with-system-readline"
                 "--enable-source-highlight"
                 #$flags))))
    (synopsis "The GNU Debugger for AVR")
    (description
     "GDB is the GNU debugger.  With it, you can monitor what a program is
doing while it runs or what it was doing just before a crash.  It allows you
to specify the runtime conditions, to define breakpoints, and to change how
the program is running to try to fix bugs.

This variant of GDB can be used to debug programs written for the AVR
microcontroller architecture.")))
