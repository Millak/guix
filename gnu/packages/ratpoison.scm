;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages ratpoison)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages fontutils))

(define-public ratpoison
  (package
    (name "ratpoison")
    (version "1.4.9")
    (source
     (origin (method url-fetch)
             (uri (string-append "mirror://savannah/ratpoison/ratpoison-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1wfir1gvh5h7izgvx2kd1pr2k7wlncd33zq7qi9s9k2y0aza93yr"))
             (patches (search-patches "ratpoison-shell.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules `((ice-9 format)
                  ,@%default-gnu-modules)
      ;; Specify the absolute location of xterm, as the user experience sucks
      ;; when no terminal is available (can't consult help with 'C-t ?', for
      ;; example).
      #:configure-flags
      #~(list (string-append "--with-xterm="
                             (search-input-file %build-inputs "bin/xterm")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-xsession
            (lambda _
              ;; Add a .desktop file to xsessions.
              (let ((xsessions (string-append #$output "/share/xsessions")))
                (mkdir-p xsessions)
                (call-with-output-file (string-append xsessions
                                                      "/ratpoison.desktop")
                  (lambda (port)
                    (format port
                            "[Desktop Entry]~@
                            Name=ratpoison~@
                            Comment=Tiling window manager: say goodbye to the rodent!~@
                            Exec=~a/bin/ratpoison~@
                            TryExec=~@*~a/bin/ratpoison~@
                            Type=Application~%"
                            #$output)))))))))
    (inputs
     (list fontconfig
           freetype
           libxft
           libxi
           libxrandr
           libxpm
           libxt
           libxtst
           libx11
           readline
           xorgproto
           xterm))
    (native-inputs
     (list perl
           pkg-config))
    (home-page "https://www.nongnu.org/ratpoison/")
    (synopsis "Simple mouse-free tiling window manager")
    (description
     "Ratpoison is a simple window manager with no fat library
dependencies, no fancy graphics, no window decorations, and no
rodent dependence.  It is largely modelled after GNU Screen which
has done wonders in the virtual terminal market.

The screen can be split into non-overlapping frames.  All windows
are kept maximized inside their frames to take full advantage of
your precious screen real estate.

All interaction with the window manager is done through keystrokes.
Ratpoison has a prefix map to minimize the key clobbering that
cripples Emacs and other quality pieces of software.")
    (license gpl2+)))
