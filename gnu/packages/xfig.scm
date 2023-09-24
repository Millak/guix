;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu packages xfig)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages compression))

(define-public fig2dev
  (package
    (name "fig2dev")
    (version "3.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mcj/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1cch429zbmrg2zy1mkx9xwnpvkjhmlw40c88bvi2virws744dqhm"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((programs
                     (find-files (string-append #$output "/bin")))
                    (path
                     (search-path-as-list
                      '("bin")
                      (map (cut assoc-ref inputs <>)
                           (list "ghostscript" "imagemagick")))))
                (for-each (lambda (program)
                            (wrap-program program
                              `("PATH" ":" prefix ,path)))
                          programs)))))))
    (inputs
     (list libpng zlib
           ;; Quoth INSTALL:
           ;; “To run fig2dev, the packages
           ;;    ghostscript, and one out of
           ;;    netpbm | ImageMagick | GraphicsMagick
           ;; are needed to produce various bitmap output formats, or process
           ;; fig files with embedded images.”
           ghostscript
           imagemagick))
    (native-inputs
     ;; XXX: Tests fail if netpbm is absent.
     (list netpbm))
    (home-page "https://sourceforge.net/projects/mcj")
    (synopsis "Translate Fig to other graphic description formats")
    (description "Fig2dev is a set of tools for creating TeX documents with
graphics which are portable, in the sense that they can be printed in a wide
variety of environments.")
    (license
     (license:non-copyleft "file://Makefile.am"
                           "See <https://spdx.org/licenses/Xfig.html>."))))

(define-public transfig
  (deprecated-package "transfig" fig2dev))

(define-public xfig
  (package
    (name "xfig")
    (version "3.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mcj/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1xy2zqbd1wn2fij95kgnj39850r7xk74kvx7kp0dxhmvs429vv8k"))
       ;; TODO: Remove these patches and snippet when updating,
       ;; upstreamed since commit `84375ac05e923b46bbacc8b336b0dfbe29497b6b'.
       (patches
        (search-patches "xfig-Enable-error-message-for-missing-libraries.patch"
                        "xfig-Use-pkg-config-to-set-fontconfig-CFLAGS-and-LIBS.patch"
                        "xfig-Fix-double-free-when-requesting-MediaBox.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; The patch-dot-desktop-files phase requires a relative name.
        #~(begin
            (substitute* "xfig.desktop"
              (("^(Exec=)/usr/bin/" _ key) key))
            ;; This forces autoreconf to be invoked, needed for patches
            ;; to be effective.
            (delete-file "configure")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((path
                     (search-path-as-list
                      '("bin")
                      (map (cut assoc-ref inputs <>)
                           (list "ghostscript" "fig2dev")))))
                (wrap-program (string-append #$output "/bin/xfig")
                  `("PATH" ":" prefix ,path))))))))
    (native-inputs
     (list pkg-config
           ;; TODO: Remove the import on (gnu packages autotools)
           ;; and related packages in the next update.
           autoconf automake libtool
           ;; For tests.
           desktop-file-utils))
    (inputs
     (list ghostscript
           fig2dev
           libxaw3d
           libjpeg-turbo
           libpng
           libxpm
           libx11
           libxft
           libxt))
    (home-page "https://mcj.sourceforge.net/")
    (synopsis "Interactive drawing tool")
    (description
     "Xfig is an interactive drawing tool which runs under X Window System.
In xfig, figures may be drawn using objects such as circles, boxes, lines,
spline curves, text, etc.  It is also possible to import images in formats
such as GIF, JPEG, EPSF (PostScript), etc.  Those objects can be created,
deleted, moved or modified.  Attributes such as colors or line styles can be
selected in various ways.  For text, 35 fonts are available.")
    (license
     (license:non-copyleft "file://Makefile.am"
                           "See <https://spdx.org/licenses/Xfig.html>."))))
