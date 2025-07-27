;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Blake Shaw <blake@nonconstructivism.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2025 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages notcurses)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages video)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libunistring))

(define-public notcurses
  (package
    (name "notcurses")
    (version "3.0.16")
    (source
     (origin
       (method url-fetch)
       ;; Note: the upstream git repository contains non-free media (see the
       ;; documentation for DFSG_BUILD; but the project provides a sanitized
       ;; tarball for distributions.  If switching to a git source, we need
       ;; to find a way to elide the non-free demos with a source 'snippet'.
       (uri (string-append "https://github.com/dankamongmen/notcurses/releases"
                           "/download/v" version "/notcurses_" version
                           "+dfsg.orig.tar.xz"))
       (file-name (string-append name "-" version ".tar.xz"))
       (sha256
        (base32 "074pq81rf4jhlrsq12dv44w840lv3kkibaj612gj5wm79wiw7bgk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       ;; These flags are documented in 'INSTALL.md' in the source distribution.
       #:configure-flags
       '( ;; Do not build "coverage"
         "-DUSE_COVERAGE=off"
         ;; Do not build HTML documentation
         "-DUSE_DOXYGEN=off"
         ;; Unfortunately this disables the manpages.
         ,@(if (supported-package? pandoc)
             '()
             '("-DUSE_PANDOC=off"))
         ;; Don't include mouse support
         "-DUSE_GPM=off"
         ;; Use FFmpeg for multimedia support
         "-DUSE_MULTIMEDIA=ffmpeg"
         ;; Follow the Debian Free Software Guidelines, omitting nonfree content.
         "-DDFSG_BUILD=ON")))
    (native-inputs
     (append
       (list pkg-config)
       (if (supported-package? pandoc)
         (list pandoc)
         '())
       (list doctest)))
    (inputs
     (list ffmpeg
           libdeflate
           libunistring
           ncurses
           zlib))
    (synopsis "Textual user interfaces")
    (description "Notcurses is a library for building complex textual user
interfaces on modern terminals.")
    (home-page "https://notcurses.com")
    (license license:asl2.0)))
