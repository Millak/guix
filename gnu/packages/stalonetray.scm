;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages stalonetray)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public stalonetray
  (package
    (name "stalonetray")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri
	(git-reference
	 (url "https://github.com/kolbusa/stalonetray")
	 (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "074wy1xppfycillbxq6fwrq87ik9glc95083df5vgm20mhzni7pz"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
	  (add-after 'unpack 'fix-docbook-root
	    (lambda _
	      (substitute* "configure.ac"
		(("AC_SUBST\\(DOCBOOK_ROOT\\)" all)
		 (string-append "DOCBOOK_ROOT="
				#$(this-package-native-input "docbook-xsl")
				"/xml/xsl/docbook-xsl-"
				#$(package-version (this-package-native-input "docbook-xsl"))
				"; " all))))))))
    (inputs (list libx11 libxpm))
    (native-inputs (list autoconf automake libxslt docbook-xsl))
    (build-system gnu-build-system)
    (home-page "https://kolbusa.github.io/stalonetray")
    (synopsis "Standalone freedesktop.org and KDE systray implementation")
    (description
     "Stalonetray is a stand-alone freedesktop.org and KDE system
tray (notification area) for X Window System/X11 (e.g. X.Org or XFree86).  It
has full XEMBED support and minimal dependencies: an X11 lib only.  Stalonetray
works with virtually any EWMH-compliant window manager.")
    (license gpl2+)))
