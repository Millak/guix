;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Carl Dong <contact@carldong.me>
;;; Copyright © 2021 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundation.xyz>
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

(define-module (gnu packages mingw)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages cross-base)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:export (make-mingw-w64))

(define* (make-mingw-w64/implementation machine
                                        #:key
                                        xgcc
                                        xbinutils
                                        with-winpthreads?)
  "Return a mingw-w64 for targeting MACHINE.  If XGCC or XBINUTILS is specified,
use that gcc or binutils when cross-compiling.  If WITH-WINPTHREADS? is
specified, recurse and return a mingw-w64 with support for winpthreads."
  (let* ((triplet (string-append machine "-" "w64-mingw32")))
    (package
      (name (string-append "mingw-w64" "-" machine
                           (if with-winpthreads? "-winpthreads" "")))
      (version "12.0.0")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/mingw-w64/mingw-w64/"
               "mingw-w64-release/mingw-w64-v" version ".tar.bz2"))
         (sha256
          (base32 "0bzdprdrb8jy5dhkl2j2yhnr2nsiv6wk2wzxrzaqsvjbmj58jhfc"))))
      (native-inputs `(("xgcc-core" ,(if xgcc xgcc (cross-gcc triplet)))
                       ("xbinutils" ,(if xbinutils xbinutils
                                         (cross-binutils triplet)))
                       ,@(if with-winpthreads?
                             `(("xlibc" ,(make-mingw-w64
                                          machine
                                          #:xgcc xgcc
                                          #:xbinutils xbinutils)))
                             '())))
      (build-system gnu-build-system)
      (search-paths
       (list (search-path-specification
              (variable "CROSS_C_INCLUDE_PATH")
              (files `("include" ,(string-append triplet "/include"))))
             (search-path-specification
              (variable "CROSS_LIBRARY_PATH")
              (files
               `("lib" "lib64"
                 ,(string-append triplet "/lib")
                 ,(string-append triplet "/lib64"))))))
      (arguments
       (list #:parallel-build? #f ; parallel builds often fail with empty .a files
             #:tests? #f ; compiles and includes glibc headers
             #:strip-binaries? #f
             #:configure-flags
             #~(list #$(string-append "--host=" triplet)
                     #$@(if with-winpthreads?
                            #~("--with-libraries=winpthreads")
                            #~())
                     ;; The default msvcrt changed on 12.0.0 to use UCRT as the
                     ;; default, this could cause problems with programs expecting
                     ;; MSVCRT as the default.
                     ;;
                     ;; XXX: A new target to use UCRT can be introduced as
                     ;; the MSYS2 project does, e.g: x86_64-w64-ucrt-mingw32.
                     "--with-default-msvcrt=msvcrt")
             #:make-flags #~'("DEFS=-DHAVE_CONFIG_H -D__MINGW_HAS_DXSDK=1")
             #:phases
             #~(modify-phases %standard-phases
                 (add-before 'configure 'setenv
                   (lambda _
                     (let ((xgcc-core #+(this-package-native-input
                                         "xgcc-core"))
                           (mingw-headers (string-append
                                           (getcwd) "/mingw-w64-headers")))
                       (setenv "CPP"
                               (string-append
                                xgcc-core "/bin/" #$triplet "-cpp"))
                       (setenv "CROSS_C_INCLUDE_PATH"
                               (string-append
                                mingw-headers
                                ":" mingw-headers "/include"
                                ":" mingw-headers "/crt"
                                ":" mingw-headers "/defaults/include"
                                ":" mingw-headers "/direct-x/include"))
                       #$@(if with-winpthreads?
                              #~((let ((xlibc #+(this-package-native-input
                                                 "xlibc")))
                                   (setenv "CROSS_LIBRARY_PATH"
                                           (string-append
                                            xlibc "/lib" ":"
                                            xlibc "/" #$triplet "/lib"))))
                              #~())))))))
      (home-page "https://mingw-w64.org")
      (synopsis "Minimalist GNU for Windows")
      (description
       "Minimalist GNU for Windows (@dfn{MinGW}) is a complete software
development environment for creating native Microsoft Windows applications.

It includes a set of Windows-specific header files and static import libraries
which enable the use of the Windows API.  It does not rely on any third-party C
runtime dynamic-link libraries (@dfn{DLL}s).

Mingw-w64 is an advancement of the original mingw.org project and provides
several new APIs such as DirectX and DDK, and 64-bit support.")
      (license license:fdl1.3+))))

(define make-mingw-w64
  (memoize make-mingw-w64/implementation))

(define-public mingw-w64-i686-sans-winpthreads
  (make-mingw-w64 "i686"))

(define-public mingw-w64-x86_64-sans-winpthreads
  (make-mingw-w64 "x86_64"))

(define-public mingw-w64-i686-winpthreads
  (make-mingw-w64 "i686"
                  #:with-winpthreads? #t))

(define-public mingw-w64-x86_64-winpthreads
  (make-mingw-w64 "x86_64"
                  #:with-winpthreads? #t))

(define-public mingw-w64-i686 mingw-w64-i686-winpthreads)
(define-public mingw-w64-x86_64 mingw-w64-x86_64-winpthreads)
(define-public mingw-w64 mingw-w64-i686)

(define-public mingw-w64-tools
  (package
    (name "mingw-w64-tools")
    (version "12.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/mingw-w64/mingw-w64/"
             "mingw-w64-release/mingw-w64-v" version ".tar.bz2"))
       (sha256
        (base32 "0bzdprdrb8jy5dhkl2j2yhnr2nsiv6wk2wzxrzaqsvjbmj58jhfc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
      #:phases
      #~(append
         (modify-phases %standard-phases
           (add-after 'unpack 'cd-gendef
             (lambda _
               (chdir "mingw-w64-tools/gendef"))))
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda _
               (chdir "../genidl"))))
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda _
               (chdir "../genpeimg"))))
         (append-map
          (lambda (target)
            (modify-phases %standard-phases
              (replace 'unpack
                (lambda _
                  (chdir "../widl")
                  (false-if-exception
                   (delete-file-recursively "../build"))))
              (replace 'configure
                (lambda args
                  (apply (assoc-ref %standard-phases 'configure)
                         (append args (list #:out-of-source? #t
                                            #:configure-flags
                                            `("--target" ,target
                                              "--program-prefix"
                                              ,(string-append target "-")))))))))
          '("i686-w64-mingw32" "x86_64-w64-mingw32")))))
    (home-page "https://mingw-w64.org")
    (synopsis "Tools of Minimalist GNU for Windows")
    (description "This package provides the tools of Minimalist GNU for
Windows, a complete software development environment for creating native
Microsoft Windows applications.")
    (license (list license:gpl3+ ;gendef, genidl, genlib, genpeimg, genstubdll
                   license:lgpl2.1+)))) ;widl
