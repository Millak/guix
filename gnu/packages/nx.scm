;;; This file is part of GNU Guix.
;;; Copyright © 2024 Nicolas Debonnaire <nicolas.debonnaire@gmail.com>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages nx)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public nx-libs
  (package
    (name "nx-libs")
    (version "3.5.99.27")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ArcticaProject/nx-libs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ykbza39ksycpyydaiwwbp7hkmdk96v7b36pn989k39lhfwnn8kz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f   ;no test suite
      ;; The build randomly fails when run in parallel (see:
      ;; https://github.com/ArcticaProject/nx-libs/issues/1072).
      #:parallel-build? #f
      #:make-flags #~(let ((sh (search-input-file %build-inputs "bin/sh")))
                       (list (string-append "PREFIX=" #$output)
                             (string-append "ETCDIR_NX=" #$output "/etc")
                             (string-append "LOCAL_LDFLAGS=-Wl,"
                                            "-rpath=" #$output "/lib,"
                                            "-rpath=" #$output "/lib/nx/X11")
                             (string-append "IMAKE_DEFINES=-DUseTIRPC=1"
                                            " -DBourneShell=" sh
                                            " -DProjectRoot=" #$output
                                            " -DDefaultUserPath="
                                            #$output "/bin")
                             (string-append "CONFIG_SHELL=" sh)
                             (string-append "SHELL=" sh)
                             ;; Autoreconf being run by Make, the generated
                             ;; configure script shebangs thus haven't been
                             ;; patched; workaround this by running explicitly
                             ;; via the shell.
                             (string-append "CONFIGURE=" sh " ./configure "
                                            "--prefix=" #$output)
                             "VERBOSE=1"))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (add-before 'build 'adjust-Makefile
                     (lambda _
                       ;; It's best to source a script via its absolute path,
                       ;; otherwise it's looked from PATH and fails for POSIX
                       ;; shells, such as our Bash-provided 'sh' (see:
                       ;; https://github.com/ArcticaProject/nx-libs/issues/1071).
                       (substitute* "Makefile"
                         (("\\. replace.sh")
                          ". ./replace.sh"))))
                   (add-after 'install 'wrap-nxdialog
                     (lambda* (#:key inputs #:allow-other-keys)
                       (wrap-program (string-append #$output "/bin/nxdialog")
                         `("GUIX_PYTHONPATH" =
                           (,(getenv "GUIX_PYTHONPATH")))
                         ;; Ensure GObject Introspection typelibs are found.
                         `("GI_TYPELIB_PATH" ":" =
                           (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list autoconf
           automake
           bash-minimal                 ;for wrap-program
           font-util
           gccmakedep
           imake
           libtool
           perl
           pkg-config
           quilt
           which))
    (inputs
     (list gtk+
           libjpeg-turbo
           libtirpc
           libxcomposite
           libxdamage
           libxext
           libxfont
           libxinerama
           libxml2
           libxpm
           libxrandr
           libxtst
           pixman
           python-pygobject
           python-wrapper
           xkbcomp))
    (propagated-inputs
     (list libpng))                     ;in Requires of nxcomp.pc
    (synopsis "NX X11 protocol compression libraries")
    (description "NX is a software suite which implements very efficient
compression of the X11 protocol.  This increases performance when using X
applications over a network, especially a slow one.  This package provides the
following libraries:
@table @code
@item NX_X11
NX's modified X Window System (X11) library
@item Xcomp
NX differential compression library for X11
@item Xcompshad
Session shadowing library
@end table

The following commands are also provided:

@table @command
@item nxagent
Agent providing NX transport of X sessions
@item nxproxy
The NX proxy (client) binary
@item nxdialog
Helper script
@end table")
    (home-page "https://github.com/ArcticaProject/nx-libs")
    (license license:gpl2)))
