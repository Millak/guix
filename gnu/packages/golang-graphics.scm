;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Sughosha <sughosha@disroot.org>
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

(define-module (gnu packages golang-graphics)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config))

;;; Commentary:
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-github-com-diamondburned-gotk4
  (package
    (name "go-github-com-diamondburned-gotk4")
    ;; 0.3.1 (2024-08-03); the latest commit includes fixes for tests, revert
    ;; back to git tag when released.
    (properties '((commit . "d44ab4b5b24e200c90c6a9ffb6632ada7e166a79")
                  (revision . "0")))
    (version (git-version "0.3.1"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/diamondburned/gotk4")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xwsl5n3hksrbyap11zhm4jp2hx1v8zqk796bh87qplw7468pn51"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/diamondburned/gotk4"
      #:test-flags #~(list "-vet=off")
      ;; XXX: Code is generated with "go generate", but the atemp to
      ;; re-generate it before build/test fails, see:
      ;; <https://github.com/diamondburned/gotk4/issues/174>.
      ;; #:phases
      #;
      #~(modify-phases %standard-phases
        (add-before 'build 'go-generate
          (lambda* (#:key import-path #:allow-other-keys)
            (with-directory-excursion (string-append "src/" import-path)
              (invoke "go" "generate" "-v" "-n")))))))
    (native-inputs
     (list at-spi2-core
           cairo
           gdk-pixbuf
           glib
           go-github-com-alecthomas-assert-v2
           gobject-introspection
           graphene
           gtk
           gtk+
           pango
           pkg-config))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-fatih-color
           go-github-com-karpeleslab-weak               ;for gotk4/pkg/go.mod
           go-go4-org-unsafe-assume-no-moving-gc        ;for gotk4/pkg/go.mod
           go-golang-org-x-sync))
    (home-page "https://github.com/diamondburned/gotk4")
    (synopsis "GTK4 bindings generator for Go")
    (description
     "gotk4 is a GTK4 bindings generator for Go.  It also provides generated
bindings for ATK, Cairo, GdkPixbuf, GLib, Graphene, GTK4, GTK+3 and Pango.")
    (license (list license:agpl3+    ;used by gotk4/gir
                   license:mpl2.0    ;used by gotk4/pkg
                   license:expat)))) ;used by gotk4/pkg/cairo
