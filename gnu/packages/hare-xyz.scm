;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Lilah Tascheter <lilah@lunabee.space>
;;; Copyright © 2025 jgart <jgart@dismail.de>
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

(define-public hare-compress
  (package
    (name "hare-compress")
    (version "0.25.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~sircmpwn/hare-compress")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iwfqaaiin0zi97fi7p0p1v42lf78yf6cpyakpjhyyfhgk3bm4mk"))))
    (build-system hare-build-system)
    (supported-systems %hare-supported-systems)
    (home-page "https://sr.ht/~sircmpwn/hare-compress")
    (synopsis "Compression algorithms for Hare")
    (description "This package provides compression algorithms for Hare.")
    (license license:mpl2.0)))

(define-public hare-ev
  (package
    (name "hare-ev")
    (version "0.25.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~sircmpwn/hare-ev")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ak9anvzg2qfdsvxpcszshimw5cqbm52zfq7kwx7z0yf6qi6zm0f"))))
    (build-system hare-build-system)
    (supported-systems %hare-supported-systems)
    (home-page "https://sr.ht/~sircmpwn/hare-ev")
    (synopsis "Event loop for Hare")
    (description "This package provides an event loop for Hare programs.")
    (license license:mpl2.0)))

(define-public hare-json
  (package
    (name "hare-json")
    (version "0.25.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~sircmpwn/hare-json")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dkvjkxap4vm5lc04y22mdk7mn8ainljak6wc4bhjf0zq6la2w0v"))))
    (build-system hare-build-system)
    (supported-systems %hare-supported-systems)
    (home-page "https://sr.ht/~sircmpwn/hare-json")
    (synopsis "JSON support for Hare")
    (description "This package provides JSON support for Hare.")
    (license license:mpl2.0)))

(define-public hare-irc
  (package
    (name "hare-irc")
    (version "0.25.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~sircmpwn/hare-irc")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ani8prdwjsfh8iqn5kfmkiim9ihl8dvbr28g69c037f6gbdb08v"))))
    (build-system hare-build-system)
    (supported-systems %hare-supported-systems)
    (home-page "https://sr.ht/~sircmpwn/hare-irc")
    (synopsis "IRC client protocol implementation for Hare")
    (description "This package provides an implementation of the IRC client
protocol for Hare programs, based on the @url{ircdocs.horse} specifications.")
    (license license:mpl2.0)))

(define-public hare-scfg
  (package
    (name "hare-scfg")
    (version "0.25.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~chrisppy/hare-scfg")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nj9gqvnvmwh3j39rj1h856p5qzghv5in01rxi4x74mqirl5q2ch"))))
    (build-system hare-build-system)
    (supported-systems %hare-supported-systems)
    (home-page "https://git.sr.ht/~chrisppy/hare-scfg")
    (synopsis "Hare library for a simple configuration file format")
    (description "This package provides a Hare library for parsing
@uref{https://git.sr.ht/~emersion/scfg, scfg}, a simple configuration file
format with one directive per line.")
    (license license:mpl2.0)))

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

(define-public hare-gi
  (package
    (name "hare-gi")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.sr.ht/~yerinalexey/hare-gi")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
                (base32
                  "1vizzaf81fb5fwggw2jh55nkdj8q0cwvwwssj1hj5rby86smml1g"))))
    (build-system hare-build-system)
    (arguments
      (list #:tests? #f ; no tests
            #:make-flags ''("hare-gi")
            #:phases
            #~(modify-phases %standard-phases
                (add-before 'build 'patch-scripts
                  (lambda* (#:key inputs #:allow-other-keys)
                    (map (lambda (file)
                           (substitute* file
                             (("/usr/(share/gir-1.0/[^ ]*\\.gir)" _ name)
                              (search-input-file inputs name))))
                      '("scripts/generate-gtk3" "scripts/generate-gtk4"))))
                (add-after 'patch-scripts 'build-scripts
                  (lambda _
                    ;; hare-gi is used in generation scripts, so we have to
                    ;; always build it natively, then later rebuild it.
                    (invoke "make" ".gen")
                    (when (file-exists? "hare-gi") (delete-file "hare-gi"))))
                ;; needed to prevent rebuilds during install
                (add-after 'build 'touch-build
                  (lambda _ (close-port (open-output-file ".gen")))))))
    ;; All possible inputs are needed to generate the bindings from the GObject
    ;; Introspection files.
    (propagated-inputs (list at-spi2-core gdk-pixbuf glib gobject-introspection
                             graphene gtk gtk+ harfbuzz pango))
    (supported-systems %hare-supported-systems)
    (outputs '("out" "bin"))
    (home-page "https://sr.ht/~yerinalexey/hare-gi")
    (synopsis "GTK library Hare binding generator")
    (description "hare-gi is a binding generator for GTK libraries, based on
GObject Introspection development files.  This package includes both the
binding generator binary, as well as bindings for several common libraries,
including GTK3, GTK4, and GLib.")
    (license license:mpl2.0)))

(define-public hare-adwaita
  (package
    (name "hare-adwaita")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.sr.ht/~sircmpwn/hare-adwaita")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
                (base32
                  "07j204v0dml967jz0mfdz9d37dm86qhfgpprn66nagpphq3fay8k"))))
    (build-system hare-build-system)
    (arguments
      (list #:tests? #f ; no tests
            #:phases
            #~(modify-phases %standard-phases
                (add-before 'build 'patch-scripts
                  (lambda* (#:key inputs #:allow-other-keys)
                    (substitute* "scripts/generate"
                      (("/usr/(share/gir-1.0/[^ ]*\\.gir)" _ name)
                       (search-input-file inputs name))))))))
    (propagated-inputs (list libadwaita))
    (native-inputs `((,hare-gi "bin")))
    (supported-systems %hare-supported-systems)
    (home-page "https://git.sr.ht/~sircmpwn/hare-adwaita")
    (synopsis "Adwaita bindings for Hare")
    (description "This package is a set of libadwaita bindings for the Hare
language.")
    (license license:mpl2.0)))

(define-public hare-gtk4-layer-shell
  (package
    (name "hare-gtk4-layer-shell")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.sr.ht/~sircmpwn/hare-gtk4-layer-shell")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
                (base32
                  "1y52vjz1j4v615wi6n1yjapw9r3plwh67lcg45sa778rv5316vfz"))))
    (build-system hare-build-system)
    (arguments
      (list #:tests? #f ; no tests
            #:phases
            #~(modify-phases %standard-phases
                (add-before 'build 'patch-scripts
                  (lambda* (#:key inputs #:allow-other-keys)
                    (substitute* "scripts/generate"
                      (("/usr/(share/gir-1.0/[^ ]*\\.gir)" _ name)
                       (search-input-file inputs name))))))))
    (propagated-inputs (list gtk4-layer-shell))
    (native-inputs `((,hare-gi "bin")))
    (supported-systems %hare-supported-systems)
    (home-page "https://git.sr.ht/~sircmpwn/hare-gtk4-layer-shell")
    (synopsis "GTK layer-shell bindings for Hare")
    (description "This package is a set of gtk-layer-shell bindings for the Hare
language.")
    (license license:mpl2.0)))

(define-public hare-xml
  (package
    (name "hare-xml")
    (version "0.25.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~sircmpwn/hare-xml")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h216z59piz1g7a0iiy1qic1wj8lbajw3mw5zw9nid0bz2vx81b0"))))
    (build-system hare-build-system)
    (supported-systems %hare-supported-systems)
    (home-page "https://git.sr.ht/~sircmpwn/hare-xml")
    (synopsis "XML support for Hare")
    (description "This package provides XML support for Hare.")
    (license license:mpl2.0)))
