;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Sughosha <sughosha@disroot.org>
;;; Copyright © 2026 orahcio <orahcio@gmail.com>
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
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

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

(define-public go-github-com-diamondburned-gotk4-layer-shell
  ;; There are no tags or releases upstream.
  (let ((commit "6efa9f6dc438e10869f9ef0b8f7b5197e585bac1")
        (revision "0"))
    (package
      (name "go-github-com-diamondburned-gotk4-layer-shell")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/diamondburned/gotk4-layer-shell")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03kch2g9fzgbd7q2lnp7xhdbi9rsh1fd997nw1phdvw80xshsvlr"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/diamondburned/gotk4-layer-shell"
        #:embed-files #~(list "capitalized.txt" "replaced.txt")
        #:phases
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
             go-github-com-davecgh-go-spew
             gobject-introspection
             graphene
             gtk
             gtk+
             gtk-layer-shell
             gtk4-layer-shell
             pango
             pkg-config))
      (propagated-inputs
       (list go-github-com-diamondburned-gotk4
             go-github-com-fatih-color
             go-github-com-mattn-go-colorable
             go-github-com-mattn-go-isatty
             go-github-com-pkg-errors
             go-golang-org-x-sync
             go-golang-org-x-sys))
      (home-page "https://github.com/diamondburned/gotk4-layer-shell")
      (synopsis "Layer Shell bindings generator for Go and gotk4")
      (description
       "gotk4-layer-shell is a GTK Layer Shell and GTK4 Layer Shell bindings
 generator for Go and gotk4.  It also provides generated bindings.")
      (license (list license:agpl3       ;used by the generator
                     license:mpl2.0))))) ;used by the generated code

(define-public go-github-com-dlasky-gotk3-layershell
  ;; There are no tags or releases upstream.
  (let ((commit "5c5115f0d77479c452ef686e3d7f327f2a341b3c")
        (revision "0"))
    (package
      (name "go-github-com-dlasky-gotk3-layershell")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/dlasky/gotk3-layershell")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0k7flgwmkwfq3lki4fjwx0868nxv2wa2igbqxjgqfm6150mnhmp8"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dlasky/gotk3-layershell"))
      (native-inputs
       (list cairo
             gdk-pixbuf
             glib
             gtk+
             gtk-layer-shell
             pango
             pkg-config))
      (propagated-inputs
       (list go-github-com-gotk3-gotk3))
      (home-page "https://github.com/dlasky/gotk3-layershell")
      (synopsis "gotk3 addon module for GTK Layer Shell compatibility")
      (description
       "gotk3-layershell is a simple golang library to provide bindings for
GTK Layer Shell library which can also be consumed in the gotk3 library.")
      (license license:expat))))

(define-public go-github-com-gotk3-gotk3
  (package
    (name "go-github-com-gotk3-gotk3")
    (version "0.6.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gotk3/gotk3")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hbqy4skn56xi69wqbys9rc1g46cx9sazj5n8ckq9638wphnp202"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/gotk3/gotk3"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires opening a display server.
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1"))))))
    (native-inputs
     (list cairo
           gdk-pixbuf
           glib
           gtk+
           pango
           pkg-config
           xorg-server))
    (home-page "https://github.com/gotk3/gotk3")
    (synopsis "Go bindings for GTK+3")
    (description
     "gotk3 provides Go bindings for GTK+3 and dependent projects.

Partial binding support for the following libraries is currently implemented:

@itemize
@item GTK 3 (3.12 and later)
@item GDK 3 (3.12 and later)
@item GLib 2 (2.36 and later)
@item Cairo (1.10 and later)
@end itemize

Functions use the same names as the native C function calls, but use
CamelCase.  In cases where native GTK uses pointers to values to simulate
multiple return values, Go's native multiple return values are used instead.
Whenever a native GTK call could return an unexpected NULL pointer, an
additional error is returned in the Go binding.")
    (license license:isc)))

(define-public go-github-com-sergeymakinen-go-bmp
  (package
    (name "go-github-com-sergeymakinen-go-bmp")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sergeymakinen/go-bmp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bglvxqmjvgclcfjpkznxyifj5r0bh837c2iw6x1vylc7lqcq07h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sergeymakinen/go-bmp"))
    (home-page "https://github.com/sergeymakinen/go-bmp")
    (synopsis "BMP image decoder and encoder")
    (description "Package bmp implements a BMP image decoder and encoder.")
    (license license:bsd-3)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order:
;;; guix import --insert=gnu/packages/golang-graphics.scm pypi <package-name>.
;;;
