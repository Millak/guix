;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages fcitx)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

