;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Lilah Tascheter <lilah@lunabee.space>
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

(define-module (gnu packages hare-xyz)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (guix build-system hare)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public hare-ssh
  (package
    (name "hare-ssh")
    (version "0.25.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.sr.ht/~sircmpwn/hare-ssh")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
                (base32
                  "1293xpyl3masbwq6qsamiqpbv241fkc622bdsa7vcb5q89if8hgn"))))
    (build-system hare-build-system)
    (supported-systems %hare-supported-systems)
    (home-page "https://sr.ht/~sircmpwn/hare-ssh")
    (synopsis "SSH library for Hare")
    (description "This package is an implementation of the SSH client, server,
and agent protocols in pure Hare.")
    (license license:mpl2.0)))
