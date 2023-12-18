;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Clément Lassieur <clement@lassieur.org>
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

(define-module (gnu build icecat-extension)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:export (make-icecat-extension))

(define* (make-icecat-extension pkg #:optional (pkg-output "out"))
  "Create an Icecat extension from package PKG and return a package that,
when installed, will make the extension contained in PKG available as an
Icecat browser extension.  PKG-OUTPUT specifies which output of PKG to use."
  (package
    (inherit pkg)
    (location (package-location pkg))
    (name (string-append (package-name pkg) "-icecat"))
    (native-inputs '())
    (inputs '())
    (propagated-inputs (package-propagated-inputs pkg))
    (outputs '("out"))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((addon-id #$(assq-ref (package-properties pkg) 'addon-id))
                 (moz-app-id "{ec8030f7-c20a-464f-9b0e-13a3a9e97384}")
                 (search-dir (string-append #$output "/lib/icecat/extensions/"
                                            moz-app-id)))
            ;; Icecat's iterates over `search-dir` for directories.  If a
            ;; directory's name is not a valid add-on ID, it is ignored.  See
            ;; `DirectoryLocation::readAddons()` in XPIProvider.jsm.

            ;; This directory has to be a symlink, because Icecat's
            ;; `_readLinkFile(aFile)` calls `normalize()` only if `aFile` is a
            ;; symlink.

            ;; Normalizing is required because Icecat compares the add-on path
            ;; against its local database to know if there is an extension
            ;; update.  We want the add-on path to be the package store path,
            ;; so that a path change is detected every time the package is
            ;; updated.  See `updateExistingAddon()` in XPIDatabase.jsm, with
            ;; our patch `icecat-compare-paths.patch`.

            ;; We don't want the add-on path to be the profile store path,
            ;; which would change too often.  We don't want the add-on path to
            ;; be hard-coded either because it would never change (but it
            ;; wouldn't make sense anyway).

            (mkdir-p search-dir)
            (symlink (in-vicinity (ungexp pkg pkg-output) addon-id)
                     (in-vicinity search-dir addon-id))))))))
