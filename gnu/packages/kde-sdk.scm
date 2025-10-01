;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Sughosha <sughosha@disroot.org>
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

(define-module (gnu packages kde-sdk)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages qt))

(define-public kapptemplate
  (package
    (name "kapptemplate")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kapptemplate-" version ".tar.xz"))
       (sha256
        (base32 "1wiv509y80m6gf891yw55d9429a35axngi922k119zvxfk5641as"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kcompletion
           kconfigwidgets
           kcoreaddons
           kirigami-addons
           ki18n
           kio))
    (home-page "https://apps.kde.org/kapptemplate/")
    (synopsis "Factory for easy creation of KDE/Qt components and programs")
    (description "KAppTemplate is an application to start development quickly
using existing templates providing basic repeatedly written code and a proper
structure.  It features:

@itemize
@item Templates for C++, Ruby, Python and PHP
@item Categories
@item Templates for different build-systems and frameworks
@item Templates especially for KDE-development (plugins for Plasma, QtQuick
 KTextEditor, KRunner, Akonadi)
@item New templates using space holders and a simple CMake-command
@item Integration into KDevelop
@end itemize")
    (license license:gpl2+)))
