;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2023 Sergio Pastor Pérez <sergio.pastorperez@outlook.es>
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

(define-module (gnu packages crates-gtk)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages webkit))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
(define-public rust-atk-sys-0.14
  (package
    (name "rust-atk-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sl3pqfb2jaf9kcfxj9k43d7iv8gcl5zgdgn3j5vp13w2mqgdp5s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ; missing files
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-gobject-sys" ,rust-gobject-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs
     (list at-spi2-core glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libatk-1")
    (description "FFI bindings to libatk-1")
    (license license:expat)))

(define-public rust-atk-sys-0.10
  (package
    (inherit rust-atk-sys-0.14)
    (name "rust-atk-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1knzvq2jdkx1nav619jbqsx2ivzh901rsp2wl57wr50x2fpy8c7m"))))
    (arguments
     `(#:tests? #f                      ;missing files
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs
     (list at-spi2-core glib))))

(define-public rust-atk-sys-0.9
  (package
    (inherit rust-atk-sys-0.14)
    (name "rust-atk-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vzcm1inhax78bcsbapr6mrp4z7lk783csyh200wi91pcxvw2lp5"))))
    (arguments
     `(#:tests? #f                      ;missing files
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-atk-0.14
  (package
    (name "rust-atk")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fi6f3k1lgd0iymar58hp88k76fm5pd1npi2avdn9r3mmb922fx8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-atk-sys" ,rust-atk-sys-0.14)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.14)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (inputs
     (list at-spi2-core glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the ATK library")
    (description "Rust bindings for the ATK library")
    (license license:expat)))

(define-public rust-atk-0.8
  (package
    (inherit rust-atk-0.14)
    (name "rust-atk")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gk6ijqsd6kh8cki1wznw570499psbppg3d5bqaayagjapxawka4"))))
    (arguments
     `(#:cargo-inputs
       (("rust-atk-sys" ,rust-atk-sys-0.9)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-cairo-rs-0.19
  (package
    (name "rust-cairo-rs")
    (version "0.19.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qp5rixgipdj9d8yd5458hzfxam1rgpzcxi90vq6q0v91r6jmb5j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.19)
                       ("rust-freetype-rs" ,rust-freetype-rs-0.35)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-float-eq" ,rust-float-eq-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list cairo))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Cairo library")
    (description "This package provides Rust bindings for the Cairo library.")
    (license license:expat)))

(define-public rust-cairo-rs-0.18
  (package
    (inherit rust-cairo-rs-0.19)
    (name "rust-cairo-rs")
    (version "0.18.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qjfkcq3mrh3p01nnn71dy3kn99g21xx3j8xcdvzn8ll2pq6x8lc"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.18)
                       ("rust-freetype-rs" ,rust-freetype-rs-0.32)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-float-eq" ,rust-float-eq-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-float-eq" ,rust-float-eq-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-cairo-rs-0.17
  (package
    (inherit rust-cairo-rs-0.18)
    (name "rust-cairo-rs")
    (version "0.17.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02lz7kvml4m6fx02hy1pgk6ysils9di8n75m166kcpla0b206dmb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.17)
        ("rust-freetype-rs" ,rust-freetype-rs-0.32)
        ("rust-glib" ,rust-glib-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-float-eq" ,rust-float-eq-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-cairo-rs-0.15
  (package
    (inherit rust-cairo-rs-0.17)
    (name "rust-cairo-rs")
    (version "0.15.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g396fdz8crf74dmmjarcsdbsm8qgxy3a5x9kw6m2d9xn28y6vn7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.15)
        ("rust-freetype" ,rust-freetype-0.7)
        ("rust-glib" ,rust-glib-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-cairo-rs-0.14
  (package
    (inherit rust-cairo-rs-0.15)
    (name "rust-cairo-rs")
    (version "0.14.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10ml7hxzj6w5k6yjdkcmxx0ds4mnrn9j3bdbk1nmh36vg5cp5d9k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.14)
        ("rust-freetype" ,rust-freetype-0.7)
        ("rust-glib" ,rust-glib-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-cairo-rs-0.9
  (package
    (inherit rust-cairo-rs-0.14)
    (name "rust-cairo-rs")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f5x6ipfpzz0ffph0pg0xfkdfcbr0jp59714zz857jp88zhg5h65"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.10)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-cairo-rs-0.8
  (package
    (inherit rust-cairo-rs-0.9)
    (name "rust-cairo-rs")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cairo-rs" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "11303v1fv6hsc9n70ak380gknkf0098phpcxqdhkmahqjsx4jw0m"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-cairo-sys-rs-0.19
  (package
    (name "rust-cairo-sys-rs")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r0yp0lph77lm4blrn6fvdmz2i3r8ibkkjg6nmwbvvv4jq8v6fzx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6)
                       ("rust-windows-sys" ,rust-windows-sys-0.52)
                       ("rust-x11" ,rust-x11-2))))
    (native-inputs (list pkg-config))
    (inputs (list cairo))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libcairo")
    (description "This package provides FFI bindings to libcairo.")
    (license license:expat)))

(define-public rust-cairo-sys-rs-0.18
  (package
    (inherit rust-cairo-sys-rs-0.19)
    (name "rust-cairo-sys-rs")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lfsxl7ylw3phbnwmz3k58j1gnqi6kc2hdc7g3bb7f4hwnl9yp38"))))
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-x11" ,rust-x11-2))))))

(define-public rust-cairo-sys-rs-0.17
  (package
    (inherit rust-cairo-sys-rs-0.18)
    (name "rust-cairo-sys-rs")
    (version "0.17.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ccw4z59dx60khngx79g64c75abfwy7wnq57h2z82j7vn5k0q7b9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11" ,rust-x11-2))))))

(define-public rust-cairo-sys-rs-0.15
  (package
    (inherit rust-cairo-sys-rs-0.17)
    (name "rust-cairo-sys-rs")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j6k4pps3dv6g0vlpmxc2xyk0s40vj2wpzi55lbwjspmpqlx8m9w"))))
    (arguments
     `(#:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11" ,rust-x11-2))))))

(define-public rust-cairo-sys-rs-0.14
  (package
    (inherit rust-cairo-sys-rs-0.15)
    (name "rust-cairo-sys-rs")
    (version "0.14.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w3md4xk87ign30wb3rqfmmj0q6pvg5arbm35flgsd08jxvbhj5l"))))
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-3)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11" ,rust-x11-2))))
    (native-inputs '())))

(define-public rust-cairo-sys-rs-0.10
  (package
    (inherit rust-cairo-sys-rs-0.14)
    (name "rust-cairo-sys-rs")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19wch8zc11hbi724mn16hhqyff8kw5c5bsbdlzpxdwfmkadn7lif"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11" ,rust-x11-2))))))

(define-public rust-cairo-sys-rs-0.9
  (package
    (inherit rust-cairo-sys-rs-0.10)
    (name "rust-cairo-sys-rs")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qsdy6s57yvscg2rfm7wdqrlhzbn1aq9lhk3dy1vw5f7r81blrgz"))))
    (arguments
     `(#:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11" ,rust-x11-2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-gdk-0.14
  (package
    (name "rust-gdk")
    (version "0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fwkm4glh7cici6yd96qlddyp4s2l029wa1sgh6xxn00zkf4kmxr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.14)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.14)
        ("rust-gdk-sys" ,rust-gdk-sys-0.14)
        ("rust-gio" ,rust-gio-0.14)
        ("rust-glib" ,rust-glib-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.14))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (inputs
     (list cairo gdk-pixbuf glib gtk+ pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GDK 3 library")
    (description "This package provides Rust bindings for the GDK 3 library.")
    (license license:expat)))

(define-public rust-gdk-0.13
  (package
    (inherit rust-gdk-0.14)
    (name "rust-gdk")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zbb9bwg2z9vlcjj9b59qch3mfmszsrxya7syc5a39v85adq606v"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.9)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.10)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.9)
        ("rust-gdk-sys" ,rust-gdk-sys-0.10)
        ("rust-gio" ,rust-gio-0.9)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.9))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-0.12
  (package
    (inherit rust-gdk-0.13)
    (name "rust-gdk")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12bmk9vfpk7f03fx22cq9ps00xylsxcpmp8c8r95r1n05xvyirgv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.8)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.8)
        ("rust-gdk-sys" ,rust-gdk-sys-0.9)
        ("rust-gio" ,rust-gio-0.8)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.8))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-pixbuf-0.19
  (package
    (name "rust-gdk-pixbuf")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16c6kznkh3vi82843ays8awdm37fwjd1fblv6g3h64824shsnkk2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.19)
                       ("rust-gio" ,rust-gio-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gdk-pixbuf))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GdkPixbuf library")
    (description
     "This package provides Rust bindings for the @code{GdkPixbuf} library.")
    (license license:expat)))

(define-public rust-gdk-pixbuf-0.18
  (package
    (inherit rust-gdk-pixbuf-0.19)
    (name "rust-gdk-pixbuf")
    (version "0.18.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v7svvl0g7zybndmis5inaqqgi1mvcc6s1n8rkb31f5zn3qzbqah"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.18)
        ("rust-gio" ,rust-gio-0.18)
        ("rust-glib" ,rust-glib-0.18)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-pixbuf-0.17
  (package
    (inherit rust-gdk-pixbuf-0.18)
    (name "rust-gdk-pixbuf")
    (version "0.17.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05q7ajsp2z8xi355h26k7lvq7n3lj9xm61vhn045g3238v46npb9"))))
    (arguments
     `(#:tests? #f      ; `Errors` doesn't implement `std::fmt::Display`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.17)
        ("rust-gio" ,rust-gio-0.17)
        ("rust-glib" ,rust-glib-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (inputs (list gdk-pixbuf))))

(define-public rust-gdk-pixbuf-0.15
  (package
    (inherit rust-gdk-pixbuf-0.17)
    (name "rust-gdk-pixbuf")
    (version "0.15.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16k1z21r76m620z61kfmgid1n6s8dnxpa4zlrppcr6dhr2fdsf5d"))))
    (arguments
     `(;; FIXME: error[E0277]: `Errors` doesn't implement `std::fmt::Display`
       #:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.15)
        ("rust-gio" ,rust-gio-0.15)
        ("rust-glib" ,rust-glib-0.15)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-pixbuf-0.14
  (package
    (inherit rust-gdk-pixbuf-0.15)
    (name "rust-gdk-pixbuf")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03xi6pi0h9jwkxvja18k75x5pblsiym4p39cmf7ypnh1iz5r4hak"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.14)
        ("rust-gio" ,rust-gio-0.14)
        ("rust-glib" ,rust-glib-0.14)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-pixbuf-0.9
  (package
    (inherit rust-gdk-pixbuf-0.14)
    (name "rust-gdk-pixbuf")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12lrk7zwshid8dgx9vg87bk0h4a0ilpi7w48idsrpm4xp4yawvcg"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.10)
        ("rust-gio" ,rust-gio-0.9)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-pixbuf-0.8
  (package
    (inherit rust-gdk-pixbuf-0.9)
    (name "rust-gdk-pixbuf")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mxxca0fkcw2rsd3kl3nvlb8ys4cgxqx4n5isjbv0adk8q624j72"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gio" ,rust-gio-0.8)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-pixbuf-sys-0.19
  (package
    (name "rust-gdk-pixbuf-sys")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y93g24mdgskvyhva46xv3qyb1cvj5xpi0yqnh7cb31wz2j0byjf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gio-sys" ,rust-gio-sys-0.19)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list gdk-pixbuf gtk+))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgdk_pixbuf-2.0")
    (description
     "This package provides FFI bindings to @code{libgdk_pixbuf-2.0}.")
    (license license:expat)))

(define-public rust-gdk-pixbuf-sys-0.18
  (package
    (inherit rust-gdk-pixbuf-sys-0.19)
    (name "rust-gdk-pixbuf-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xya543c4ffd2n7aiwwrdxsyc9casdbasafi6ixcknafckm3k61z"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gio-sys" ,rust-gio-sys-0.18)
        ("rust-glib-sys" ,rust-glib-sys-0.18)
        ("rust-gobject-sys" ,rust-gobject-sys-0.18)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gdk-pixbuf-sys-0.17
  (package
    (inherit rust-gdk-pixbuf-sys-0.18)
    (name "rust-gdk-pixbuf-sys")
    (version "0.17.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jvh91lzanr1a8c5h6ya8i4jzx7ifs8mjxjnmg8dfriw24yfr1cj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gio-sys" ,rust-gio-sys-0.17)
        ("rust-glib-sys" ,rust-glib-sys-0.17)
        ("rust-gobject-sys" ,rust-gobject-sys-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     (list gdk-pixbuf))))

(define-public rust-gdk-pixbuf-sys-0.15
  (package
    (inherit rust-gdk-pixbuf-sys-0.17)
    (name "rust-gdk-pixbuf-sys")
    (version "0.15.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19q2qjrzmmgc7bbs59sk6k0sv3xhpmnk9a2h0cajfr95g19jy2ql"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gio-sys" ,rust-gio-sys-0.15)
        ("rust-glib-sys" ,rust-glib-sys-0.15)
        ("rust-gobject-sys" ,rust-gobject-sys-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gdk-pixbuf-sys-0.14
  (package
    (inherit rust-gdk-pixbuf-sys-0.15)
    (name "rust-gdk-pixbuf-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14759y4z2najwv3hpvdqi2qqnz4lqrcdqqhpkkvciyq189qc15zh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-gio-sys" ,rust-gio-sys-0.14)
        ("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-gobject-sys" ,rust-gobject-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gdk-pixbuf-sys-0.10
  (package
    (inherit rust-gdk-pixbuf-sys-0.14)
    (name "rust-gdk-pixbuf-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13qyxazj9bmw3llvkh6br8v7sypnbin2nxis366ppsa3gy54dziv"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gdk-pixbuf-sys-0.9
  (package
    (inherit rust-gdk-pixbuf-sys-0.10)
    (name "rust-gdk-pixbuf-sys")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gdk-pixbuf-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1c2andpjb10y7bahh6nxnksh9m3g5qh4mgq9znx634cy1831p6fq"))))
    (arguments
     `(#:tests? #f      ; tests not included in release
       #:cargo-inputs
       (("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gdk-sys-0.14
  (package
    (name "rust-gdk-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07hz3gg039sy7iffy2w5srxzsnqf15i3ryxkqfd995k67lyin28f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.14)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.14)
        ("rust-gio-sys" ,rust-gio-sys-0.14)
        ("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-gobject-sys" ,rust-gobject-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.14)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-system-deps" ,rust-system-deps-3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     (list cairo gdk-pixbuf gtk+ glib pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgdk-3")
    (description "FFI bindings to libgdk-3")
    (license license:expat)))

(define-public rust-gdk-sys-0.10
  (package
    (inherit rust-gdk-sys-0.14)
    (name "rust-gdk-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s8d2jymffbv2kpwrx53krf7mpy3vdfhbb5i2n02dz80qp7m75ha"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.10)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.10)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.10)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gdk-sys-0.9
  (package
    (inherit rust-gdk-sys-0.10)
    (name "rust-gdk-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fwgr1b3n0khlkhdq9145m6baz9y8207ya30d30g1gyij6g6gpva"))))
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gdk4-0.7
  (package
    (name "rust-gdk4")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xiacc63p73apr033gjrb9dsk0y4yxnsljwfxbwfry41snd03nvy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; `Errors` doesn't implement `std::fmt::Display`
       #:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.18)
                       ("rust-gdk4-sys" ,rust-gdk4-sys-0.7)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-gl" ,rust-gl-0.14)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list cairo gdk-pixbuf gtk))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings of the GDK 4 library")
    (description "This package provides rust bindings of the GDK 4 library.")
    (license license:expat)))

(define-public rust-gdk4-sys-0.7
  (package
    (name "rust-gdk4-sys")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w7yvir565sjrrw828lss07749hfpfsr19jdjzwivkx36brl7ayv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; ‘GDK_MEMORY_A16’ undeclared
       #:cargo-inputs (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.18)
                       ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.18)
                       ("rust-gio-sys" ,rust-gio-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.18)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list cairo gdk-pixbuf glib gtk pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings of GDK 4")
    (description "This package provides FFI bindings of GDK 4.")
    (license license:expat)))

(define-public rust-gio-0.19
  (package
    (name "rust-gio")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1znz5ngfvv3gbndf6lzz3hs27hlb8ysls4axlfccrzvkscbz2jac"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=settings::test::bool_set_get"
         "--skip=settings::test::string_get")
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gio-sys" ,rust-gio-sys-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serial-test" ,rust-serial-test-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Gio library")
    (description "This package provides Rust bindings for the Gio library.")
    (license license:expat)))

(define-public rust-gio-0.18
  (package
    (inherit rust-gio-0.19)
    (name "rust-gio")
    (version "0.18.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wsc6mnx057s4ailacg99dwgna38dbqli5x7a6y9rdw75x9qzz6l"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=settings::test::bool_set_get"
         "--skip=settings::test::string_get")
       #:cargo-inputs
       (("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.18)
        ("rust-glib" ,rust-glib-0.18)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serial-test" ,rust-serial-test-2))))))

(define-public rust-gio-0.17
  (package
    (inherit rust-gio-0.18)
    (name "rust-gio")
    (version "0.17.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02nkqpq1a6ikzhv5x4nyfvzx8zk5dkjsjm50ns4qdybwjf93x5x6"))))
    (arguments
     `(#:tests? #f      ; `Errors` doesn't implement `std::fmt::Display`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.17)
        ("rust-glib" ,rust-glib-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serial-test" ,rust-serial-test-1))))))

(define-public rust-gio-0.15
  (package
    (inherit rust-gio-0.17)
    (name "rust-gio")
    (version "0.15.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fr8qiqjf9yzl8867kgqdsxpkcx2jrns3xwmlf0jfiid668brzb8"))))
    (arguments
     `(;; FIXME: error[E0277]: `Errors` doesn't implement `std::fmt::Display`
       #:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.15)
        ("rust-glib" ,rust-glib-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serial-test" ,rust-serial-test-0.6))))))

(define-public rust-gio-0.14
  (package
    (inherit rust-gio-0.15)
    (name "rust-gio")
    (version "0.14.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c2w47mznpplr3mmhgs4m4nlkv8gs4c182cwi9brbl7bncr3c73i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.14)
        ("rust-glib" ,rust-glib-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serial-test" ,rust-serial-test-0.4))))))

(define-public rust-gio-0.9
  (package
    (inherit rust-gio-0.14)
    (name "rust-gio")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qc5aqi2ijval5i9df0qryy4wbmayyhy7ng5v9r0fw7zpx105dhz"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serial-test" ,rust-serial-test-0.4))))))

(define-public rust-gio-0.8
  (package
    (inherit rust-gio-0.9)
    (name "rust-gio")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19cnla2ya0mi6wwaabd5mxbq2kzq46dg6jq2z19rpqyc2na0zl8c"))))
    (arguments
     `(#:tests? #f                    ; Not all files included in the tarball.
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serial-test" ,rust-serial-test-0.1)
        ("rust-serial-test-derive" ,rust-serial-test-derive-0.1))))))

(define-public rust-gio-sys-0.20
  (package
    (name "rust-gio-sys")
    (version "0.20.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ya7i4m3jirrhryy6h8x5kgp3fxn6m4111009ws5aiz0ilvgqzjg"))))
    (build-system cargo-build-system)
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'extend-include-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((gio-headers (search-input-directory
                                          inputs "include/gio-unix-2.0")))
                        ;; Tests rely on these headers.
                        (setenv "C_INCLUDE_PATH"
                                (string-append gio-headers ":"
                                               (getenv "C_INCLUDE_PATH")))))))))
    (native-inputs (list pkg-config))
    (inputs (list glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgio-2.0")
    (description "This package provides FFI bindings to libgio-2.0.")
    (license license:expat)))

(define-public rust-gio-sys-0.19
  (package
    (inherit rust-gio-sys-0.20)
    (name "rust-gio-sys")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vylsskpipfwl7mvffp1s0227d0k5amyhd32dfnp3mhl8yx47mrc"))))
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'extend-include-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((gio-headers (search-input-directory
                                          inputs "include/gio-unix-2.0")))
                        ;; Tests rely on these headers.
                        (setenv "C_INCLUDE_PATH"
                                (string-append gio-headers ":"
                                               (getenv "C_INCLUDE_PATH")))))))))))

(define-public rust-gio-sys-0.18
  (package
    (inherit rust-gio-sys-0.19)
    (name "rust-gio-sys")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lip8z35iy9d184x2qwjxlbxi64q9cpayy7v1p5y9xdsa3w6smip"))))
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.18)
        ("rust-gobject-sys" ,rust-gobject-sys-0.18)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6)
        ("rust-shell-words" ,rust-shell-words-1)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'extend-include-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((gio-headers (search-input-directory
                                          inputs "include/gio-unix-2.0")))
                        ;; Tests rely on these headers.
                        (setenv "C_INCLUDE_PATH"
                                (string-append gio-headers ":"
                                               (getenv "C_INCLUDE_PATH")))))))))))

(define-public rust-gio-sys-0.17
  (package
    (inherit rust-gio-sys-0.18)
    (name "rust-gio-sys")
    (version "0.17.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hr84vgpz1hbs9q7wgvpnwhbxwh9kim0z5aqv6v6ki0j1b1qgkqc"))))
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.17)
        ("rust-gobject-sys" ,rust-gobject-sys-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'extend-include-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((gio-headers (search-input-directory
                                          inputs "include/gio-unix-2.0")))
                        ;; Tests rely on these headers.
                        (setenv "C_INCLUDE_PATH"
                                (string-append gio-headers ":"
                                               (getenv "C_INCLUDE_PATH")))))))))))

(define-public rust-gio-sys-0.15
  (package
    (inherit rust-gio-sys-0.17)
    (name "rust-gio-sys")
    (version "0.15.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13fgmc2xdzg9qk9l3nlp1bilwn6466mrqbiq4fhc9qkia93pl59j"))))
    (arguments
     `(;; FIXME: some GLib macros are not found
       #:tests? #f
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.15)
        ("rust-gobject-sys" ,rust-gobject-sys-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'extend-include-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((gio-headers (search-input-directory
                                          inputs "include/gio-unix-2.0")))
                        ;; Tests rely on these headers.
                        (setenv "C_INCLUDE_PATH"
                                (string-append gio-headers ":"
                                               (getenv "C_INCLUDE_PATH")))))))))))

(define-public rust-gio-sys-0.14
  (package
    (inherit rust-gio-sys-0.15)
    (name "rust-gio-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yj8dx2rna07av3jwyd93s832kw8dg14zxxwqj3w5z2pdvv1v960"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-gobject-sys" ,rust-gobject-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-3)
        ("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-gio-sys-0.10
  (package
    (inherit rust-gio-sys-0.14)
    (name "rust-gio-sys")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b2f6jvghm2djymj3qrgbqfci2f95gyhdin2pgv2qpcg5xszn92y"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1)
        ("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-gio-sys-0.9
  (package
    (inherit rust-gio-sys-0.10)
    (name "rust-gio-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "049rafihxp3maxg3fwj6062ni9dcfilvhfq6ibnfgsmr89925bag"))))
    (arguments
     `(#:tests? #f              ; Some test libraries not included in release.
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gir-format-check-0.1
  (package
    (name "rust-gir-format-check")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gir-format-check" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "05n4g8yqkyzrnxbqyrkjqjxsfxdy3q78jk0ny54ffv2qm09sjp9s"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/gtk-rs/gir-format-check")
    (synopsis "File format checker")
    (description "File format checker in Rust.")
    (license license:expat)))

(define-public rust-glib-0.19
  (package
    (name "rust-glib")
    (version "0.19.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i2ak1scmzfmfxbm2dr146jl4y9mafxf1ald05jr8iimy5wh4r9r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=structured_log")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-task" ,rust-futures-task-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gio-sys" ,rust-gio-sys-0.19)
                       ("rust-glib-macros" ,rust-glib-macros-0.19)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-trybuild2" ,rust-trybuild2-1))))
    (native-inputs (list pkg-config))
    (inputs (list glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GLib library")
    (description "This package provides Rust bindings for the GLib library.")
    (license license:expat)))

(define-public rust-glib-0.18
  (package
    (inherit rust-glib-0.19)
    (name "rust-glib")
    (version "0.18.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r8fw0627nmn19bgk3xpmcfngx3wkn7mcpq5a8ma3risx3valg93"))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=structured_log")
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-task" ,rust-futures-task-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.18)
        ("rust-glib-macros" ,rust-glib-macros-0.18)
        ("rust-glib-sys" ,rust-glib-sys-0.18)
        ("rust-gobject-sys" ,rust-gobject-sys-0.18)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-trybuild2" ,rust-trybuild2-1))))))

(define-public rust-glib-0.17
  (package
    (inherit rust-glib-0.18)
    (name "rust-glib")
    (version "0.17.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "glib" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jqlipn9zixj8fpqlg45v0f06j2ghdz72cml2akcxlnlm1dx9ynk"))))
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-task" ,rust-futures-task-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.17)
        ("rust-glib-macros" ,rust-glib-macros-0.17)
        ("rust-glib-sys" ,rust-glib-sys-0.17)
        ("rust-gobject-sys" ,rust-gobject-sys-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.4)
        ("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-trybuild2" ,rust-trybuild2-1))))))

(define-public rust-glib-0.15
  (package
    (inherit rust-glib-0.17)
    (name "rust-glib")
    (version "0.15.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pahikbwxr3vafdrr5l2hnlhkf9xi4illryan0l59ayhp9pk1c7d"))))
    (arguments
     `(;; FIXME: error[E0277]: `Errors` doesn't implement `std::fmt::Display`
       #:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-task" ,rust-futures-task-0.3)
        ("rust-glib-macros" ,rust-glib-macros-0.15)
        ("rust-glib-sys" ,rust-glib-sys-0.15)
        ("rust-gobject-sys" ,rust-gobject-sys-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-glib-0.14
  (package
    (inherit rust-glib-0.15)
    (name "rust-glib")
    (version "0.14.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "194n6w5yy869lls3pr46x5nm049cn02qsljzcgv1w5dzc8g5ylbw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-task" ,rust-futures-task-0.3)
        ("rust-glib-macros" ,rust-glib-macros-0.14)
        ("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-gobject-sys" ,rust-gobject-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-smallvec" ,rust-smallvec-1))))))

(define-public rust-glib-0.10
  (package
    (inherit rust-glib-0.14)
    (name "rust-glib")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ifh56nhvhcrssxqkad876qqrlnl16q6b8ap3f7ncpjinw9m0s0c"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-preview" ,rust-futures-preview-0.3)
        ("rust-futures-task" ,rust-futures-task-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-glib-macros" ,rust-glib-macros-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1))))))

(define-public rust-glib-0.9
  (package
    (inherit rust-glib-0.10)
    (name "rust-glib")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glib" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h3100mf7kdfxibjz5na0sqzbd2mcsyd8pzivn3666w414x5gys0"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-preview" ,rust-futures-preview-0.3)
        ("rust-futures-task" ,rust-futures-task-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-glib-macros-0.19
  (package
    (name "rust-glib-macros")
    (version "0.19.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mzsh8jkg8vldvgvr9gsaidvn2myn5cbdn8a6m8rgbhlg8kv0aa4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-heck" ,rust-heck-0.5)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-glib" ,rust-glib-0.19)
                                   ("rust-trybuild2" ,rust-trybuild2-1))))
    (native-inputs (list pkg-config))
    (inputs (list glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GLib library, proc macros crate")
    (description
     "This package provides Rust bindings for the GLib library, proc macros crate.")
    (license license:expat)))

(define-public rust-glib-macros-0.18
  (package
    (inherit rust-glib-macros-0.19)
    (name "rust-glib-macros")
    (version "0.18.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p5cla53fcp195zp0hkqpmnn7iwmkdswhy7xh34002bw8y7j5c0b"))))
    (arguments
     `(#:cargo-inputs
       (("rust-heck" ,rust-heck-0.4)
        ("rust-proc-macro-crate" ,rust-proc-macro-crate-2)
        ("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs
       (("rust-glib" ,rust-glib-0.18)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-trybuild2" ,rust-trybuild2-1))))))

(define-public rust-glib-macros-0.17
  (package
    (inherit rust-glib-macros-0.18)
    (name "rust-glib-macros")
    (version "0.17.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "glib-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09nyh22nryip4i22mdrixzl4q0r5h5lxcn40mgqr30rk6y9wg9gc"))))
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-heck" ,rust-heck-0.4)
        ("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
        ("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs
       (("rust-glib" ,rust-glib-0.17)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-trybuild2" ,rust-trybuild2-1))))))

(define-public rust-glib-macros-0.15
  (package
    (inherit rust-glib-macros-0.17)
    (name "rust-glib-macros")
    (version "0.15.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jiwvywmkqhih6glipj1c7aylwfr738jid8nmjr4yvx2dygsxihh"))))
    (arguments
     `(;; XXX: Circular dependency on rust-glib??
       #:tests? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-heck" ,rust-heck-0.4)
        ("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
        ("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-glib-macros-0.14
  (package
    (inherit rust-glib-macros-0.15)
    (name "rust-glib-macros")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0675i7m7pksg4cd9kfpzjnw0x5r3y4gcac7mfgy6nyb63wv6db9a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-heck" ,rust-heck-0.3)
        ("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
        ("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-glib-macros-0.10
  (package
    (inherit rust-glib-macros-0.14)
    (name "rust-glib-macros")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fa00s6gnj3hgri9z926199jidczlmjr1db0n4r80sins4k6lj21"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-heck" ,rust-heck-0.3)
        ("rust-itertools" ,rust-itertools-0.9)
        ("rust-proc-macro-crate" ,rust-proc-macro-crate-0.1)
        ("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-glib-sys-0.20
  (package
    (name "rust-glib-sys")
    (version "0.20.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0avm6dr3q2xvlad7fcbq7an7qasaqf3k480dn19s99ngi1fwm7jz"))))
    (build-system cargo-build-system)
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libglib-2.0")
    (description "This package provides FFI bindings to libglib-2.0.")
    (license license:expat)))

(define-public rust-glib-sys-0.19
  (package
    (inherit rust-glib-sys-0.20)
    (name "rust-glib-sys")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19f4q8x77vd7c1d9ikw492yskq5kpd7k04qb8xnh1c427a6w2baw"))))
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-glib-sys-0.18
  (package
    (inherit rust-glib-sys-0.19)
    (name "rust-glib-sys")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "164qhsfmlzd5mhyxs8123jzbdfldwxbikfpq5cysj3lddbmy4g06"))))
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-glib-sys-0.17
  (package
    (inherit rust-glib-sys-0.18)
    (name "rust-glib-sys")
    (version "0.17.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "glib-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w64ppa12s9ky6gfdaqhq9w30ad6hskll812jb3sl2xsggmac2nq"))))))

(define-public rust-glib-sys-0.15
  (package
    (inherit rust-glib-sys-0.17)
    (name "rust-glib-sys")
    (version "0.15.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "glib-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m5sqm69fdk8vaw6hggyizhs1r1vivx73splrdvczsb5iqpijjzg"))))
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-glib-sys-0.14
  (package
    (inherit rust-glib-sys-0.15)
    (name "rust-glib-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bjlymn3fw4g8slij6iiggaipknf9072mr2qm3i4a91199an078w"))))
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs '())))

(define-public rust-glib-sys-0.10
  (package
    (inherit rust-glib-sys-0.14)
    (name "rust-glib-sys")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hfdwilf3dnrv3pnfbwmp3h2afgwvfsapcgjfg8276kflsbvksf7"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-glib-sys-0.9
  (package
    (inherit rust-glib-sys-0.10)
    (name "rust-glib-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qhnwfqqcp63mx4q9744rfkq78g6ky2j8ppsxxgw0ipl08w6z1cm"))))
    (arguments
     `(#:tests? #f              ; Some test libraries not included in release.
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gobject-sys-0.20
  (package
    (name "rust-gobject-sys")
    (version "0.20.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qk4d5ifybf5cnsv8bss85afz7vpxlqffawxq87cyy44zz979im4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgobject-2.0")
    (description "This package provides FFI bindings to libgobject-2.0.")
    (license license:expat)))

(define-public rust-gobject-sys-0.19
  (package
    (inherit rust-gobject-sys-0.20)
    (name "rust-gobject-sys")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17lb7dfbpcg8zchwlfbc08kckwf0a7d9n5ly3pyic13f5ljpws9f"))))
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gobject-sys-0.18
  (package
    (inherit rust-gobject-sys-0.19)
    (name "rust-gobject-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i6fhp3m6vs3wkzyc22rk2cqj68qvgddxmpaai34l72da5xi4l08"))))
    (arguments
     `(#:tests? #f ; `G_TYPE_FUNDAMENTAL_MAX` constant mismatch with gcc
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.18)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gobject-sys-0.17
  (package
    (inherit rust-gobject-sys-0.18)
    (name "rust-gobject-sys")
    (version "0.17.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gobject-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ql0pcab6dxjapiglxcjaavbbh1sznyc2wj5q273b9j0fwqw6d6d"))))
    (arguments
     `(;; XXX: Tests are sensitive to the version of glib, even though
       ;; the library supports a wide range.  Skip for now.
       #:tests? #f
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gobject-sys-0.15
  (package
    (inherit rust-gobject-sys-0.17)
    (name "rust-gobject-sys")
    (version "0.15.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02hyilvpi4hw4gr03z2plsbf1zicsfs5l0xxadqx3v3b4i2cwmqd"))))
    (arguments
     `(;; FIXME: Constant value mismatch for G_TYPE_FUNDAMENTAL_MAX
       ;; Rust: "255"
       ;; C:    "1020"
       #:tests? #f
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gobject-sys-0.14
  (package
    (inherit rust-gobject-sys-0.15)
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name (string-append "rust-gobject-sys-" version ".tar.gz"))
       (sha256
        (base32 "1xf3jiwzrjingq8jr15bjkbv6m5dypzp67cjnm5f7njrjzicm4ma"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs '())))

(define-public rust-gobject-sys-0.10
  (package
    (inherit rust-gobject-sys-0.14)
    (name "rust-gobject-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1311d3zbdhl1g7ibj1iy1650513yrhxbjxgfhazn52ii1jv368cm"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gobject-sys-0.9
  (package
    (inherit rust-gobject-sys-0.10)
    (name "rust-gobject-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nakflbp3gjaas4fw7sn3p1p32khyfpcq1h06z7yqd10yq2ail9i"))))
    (arguments
     `(#:tests? #f              ; Some test libraries not included in release.
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-graphene-rs-0.18
  (package
    (name "rust-graphene-rs")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "graphene-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00f4q1ra4haap5i7lazwhkdgnb49fs8adk2nm6ki6mjhl76jh8iv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; `Errors` doesn't implement `std::fmt::Display`
       #:cargo-inputs (("rust-glib" ,rust-glib-0.18)
                       ("rust-graphene-sys" ,rust-graphene-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib graphene))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Graphene library")
    (description "Rust bindings for the Graphene library.")
    (license license:expat)))

(define-public rust-graphene-sys-0.18
  (package
    (name "rust-graphene-sys")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "graphene-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n8zlg7z26lwpnvlqp1hjlgrs671skqwagdpm7r8i1zwx3748hfc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib graphene))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgraphene-1.0")
    (description "FFI bindings to libgraphene-1.0.")
    (license license:expat)))

(define-public rust-gsk4-0.7
  (package
    (name "rust-gsk4")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gsk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zhzs2dkgiinhgc11akpn2harq3x5n1iq21dnc4h689g3lsqx58d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; `Errors` doesn't implement `std::fmt::Display`
       #:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-gdk4" ,rust-gdk4-0.7)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-graphene-rs" ,rust-graphene-rs-0.18)
                       ("rust-gsk4-sys" ,rust-gsk4-sys-0.7)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list gtk))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings of the GSK 4 library")
    (description "Rust bindings of the GSK 4 library.")
    (license license:expat)))

(define-public rust-gsk4-sys-0.7
  (package
    (name "rust-gsk4-sys")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gsk4-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mbdlm9qi1hql48rr29vsj9vlqwc7gxg67wg1q19z67azwz9xg8j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; `GskFillRule` undeclared
       #:cargo-inputs (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.18)
                       ("rust-gdk4-sys" ,rust-gdk4-sys-0.7)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-graphene-sys" ,rust-graphene-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.18)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list cairo gdk-pixbuf graphene gtk pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings of GSK 4")
    (description "This package provides FFI bindings of GSK 4.")
    (license license:expat)))

(define-public rust-gstreamer-0.18
  (package
    (name "rust-gstreamer")
    (version "0.18.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mjlnw9917j3wwij8225bjp54k7408lxqjjnh6r6wksyryx66qyn"))))
    (build-system cargo-build-system)
    (arguments
     (list #:tests? #f  ; https://github.com/gtk-rs/gtk3-rs/issues/768
           #:cargo-inputs `(("rust-bitflags" ,rust-bitflags-1)
                            ("rust-cfg-if" ,rust-cfg-if-1)
                            ("rust-futures-channel" ,rust-futures-channel-0.3)
                            ("rust-futures-core" ,rust-futures-core-0.3)
                            ("rust-futures-util" ,rust-futures-util-0.3)
                            ("rust-glib" ,rust-glib-0.15)
                            ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.18)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-muldiv" ,rust-muldiv-1)
                            ("rust-num-integer" ,rust-num-integer-0.1)
                            ("rust-num-rational" ,rust-num-rational-0.4)
                            ("rust-once-cell" ,rust-once-cell-1)
                            ("rust-option-operations" ,rust-option-operations-0.4)
                            ("rust-paste" ,rust-paste-1)
                            ("rust-pretty-hex" ,rust-pretty-hex-0.3)
                            ("rust-serde" ,rust-serde-1)
                            ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                            ("rust-thiserror" ,rust-thiserror-1))
           #:cargo-development-inputs
            `(("rust-futures-executor" ,rust-futures-executor-0.3)
              ("rust-gir-format-check" ,rust-gir-format-check-0.1)
              ("rust-ron" ,rust-ron-0.7)
              ("rust-serde-json" ,rust-serde-json-1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer")
    (description "Rust bindings for GStreamer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-app-0.18
  (package
    (name "rust-gstreamer-app")
    (version "0.18.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-app" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "115nykpdvfccyzvfi73qkhn061f6rdyhcaj9ajnw2ik5pimdyjk6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; https://github.com/gtk-rs/gtk3-rs/issues/768
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-glib" ,rust-glib-0.15)
                       ("rust-gstreamer" ,rust-gstreamer-0.18)
                       ("rust-gstreamer-app-sys" ,rust-gstreamer-app-sys-0.18)
                       ("rust-gstreamer-base" ,rust-gstreamer-base-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1))
       #:cargo-development-inputs (("rust-futures-executor" ,rust-futures-executor-0.3)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer App library")
    (description "Rust bindings for the GStreamer App library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-app-sys-0.18
  (package
    (name "rust-gstreamer-app-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-app-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fsgdb3b23s45xc7s06xw96x7avza0jpyj02x1fkw6vk3pr03d63"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.15)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.18)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstapp-1.0")
    (description "FFI bindings to libgstapp-1.0, part of Gstreamer.")
    (license license:expat)))

(define-public rust-gstreamer-audio-0.18
  (package
    (name "rust-gstreamer-audio")
    (version "0.18.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-audio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zl0bj03rz19qlrm50w7i5sagh0i0p5d8gr7ig1k6k5yd7k47sww"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; https://github.com/gtk-rs/gtk3-rs/issues/768
       #:cargo-inputs (("rust-array-init" ,rust-array-init-2)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-glib" ,rust-glib-0.15)
                       ("rust-gstreamer" ,rust-gstreamer-0.18)
                       ("rust-gstreamer-audio-sys" ,rust-gstreamer-audio-sys-0.18)
                       ("rust-gstreamer-base" ,rust-gstreamer-base-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1))
       #:cargo-development-inputs (("rust-gir-format-check" ,rust-gir-format-check-0.1)
                                   ("rust-itertools" ,rust-itertools-0.10))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for the GStreamer Audio library")
    (description "Rust bindings for the GStreamer Audio library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-audio-sys-0.18
  (package
    (name "rust-gstreamer-audio-sys")
    (version "0.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-audio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z3xryblh75xp08xyw3m6jfz9azarcvl06dd3psc0n65agxmhhm3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=cross_validate_constants_with_c")
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.15)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.15)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.18)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstaudio-1.0")
    (description "FFI bindings to libgstaudio, part of Gstreamer.")
    (license license:expat)))

(define-public rust-gstreamer-base-0.18
  (package
    (name "rust-gstreamer-base")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-base" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gw6sr75h01y3j6lpxhc7p1frvkba9a4imyyb2ppqh42cprkakr2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; https://github.com/gtk-rs/gtk3-rs/issues/768
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-glib" ,rust-glib-0.15)
                       ("rust-gstreamer" ,rust-gstreamer-0.18)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Base library")
    (description "Rust bindings for GStreamer Base library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-base-sys-0.18
  (package
    (name "rust-gstreamer-base-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-base-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13h79fcn3b9bkg7h8j0vxc1zryp92shbvvk6gkx723il7hy4k0x0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.15)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.15)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstbase-1.0")
    (description "FFI bindings to libgstbase-1.0, part of GStreamer.")
    (license license:expat)))

(define-public rust-gstreamer-sys-0.18
  (package
    (name "rust-gstreamer-sys")
    (version "0.18.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qikgp5m3xj41fbfyfl6ckb5i8dxadfvlvj5bf8girn2sdjpllg3"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs `(("rust-glib-sys" ,rust-glib-sys-0.15)
                            ("rust-gobject-sys" ,rust-gobject-sys-0.15)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-system-deps" ,rust-system-deps-6))
           #:cargo-development-inputs `(("rust-shell-words" ,rust-shell-words-1)
                                        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstreamer-1.0")
    (description
     "Foreign Function Interface (FFI) bindings to libgstreamer-1.0.")
    (license license:expat)))

(define-public rust-gtk-0.14
  (package
    (name "rust-gtk")
    (version "0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0367kb9d9x5cry6zpn2ccsahvynia6hzmr61gqrfj5rkvli13d9f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;requires running server
       #:cargo-inputs
       (("rust-atk" ,rust-atk-0.14)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.14)
        ("rust-field-offset" ,rust-field-offset-0.3)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-gdk" ,rust-gdk-0.14)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.14)
        ("rust-gio" ,rust-gio-0.14)
        ("rust-glib" ,rust-glib-0.14)
        ("rust-gtk-sys" ,rust-gtk-sys-0.14)
        ("rust-gtk3-macros" ,rust-gtk3-macros-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pango" ,rust-pango-0.14)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs
     (list at-spi2-core cairo glib gtk+ pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GTK+ 3 library")
    (description "This package provides Rust bindings for the GTK+ 3 library.")
    (license license:expat)))

(define-public rust-gtk-0.8
  (package
    (inherit rust-gtk-0.14)
    (name "rust-gtk")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13ygzblfv40l2kp70rnjymz7vk2g2wdjs04lhmk9q8wh0bbyiqc7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-atk" ,rust-atk-0.8)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.8)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-cc" ,rust-cc-1)
        ("rust-gdk" ,rust-gdk-0.12)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.8)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gdk-sys" ,rust-gdk-sys-0.9)
        ("rust-gio" ,rust-gio-0.8)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-gtk-sys" ,rust-gtk-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.8)
        ("rust-pango-sys" ,rust-pango-sys-0.9))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gtk-rs-lgpl-docs-0.1
  (package
    (name "rust-gtk-rs-lgpl-docs")
    (version "0.1.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-rs-lgpl-docs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xm3lm15j8yfn2jzh3sz6hrq2g2k917ahnp5caxw9c7z8sgr9f4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rustdoc-stripper" ,rust-rustdoc-stripper-0.1))))
    (home-page "https://gtk-rs.org/")
    (synopsis "LGPL-licensed docs for Gtk-rs crates")
    (description
     "LGPL-licensed docs for Gtk-rs crates.")
    (license license:lgpl2.0)))

(define-public rust-gtk-sys-0.14
  (package
    (name "rust-gtk-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gihp9zh4z7lycp0zbmq1w0k9ddbnd2h64jsgid7hi85vb9wh54c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;missing files
       #:cargo-inputs
       (("rust-atk-sys" ,rust-atk-sys-0.14)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.14)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.14)
        ("rust-gdk-sys" ,rust-gdk-sys-0.14)
        ("rust-gio-sys" ,rust-gio-sys-0.14)
        ("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-gobject-sys" ,rust-gobject-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.14)
        ("rust-system-deps" ,rust-system-deps-3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs
     (list gtk+))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgtk-3")
    (description "This package provides FFI bindings to libgtk-3.")
    (license license:expat)))

(define-public rust-gtk-sys-0.10
  (package
    (inherit rust-gtk-sys-0.14)
    (name "rust-gtk-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mq4i161kk6dwiz19ayxgm9fhx7n3r5lm9lbjiyk0qs811pxmb49"))))
    (arguments
     `(#:tests? #f                      ;tests FAILED.
       #:cargo-inputs
       (("rust-atk-sys" ,rust-atk-sys-0.10)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.10)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.10)
        ("rust-gdk-sys" ,rust-gdk-sys-0.10)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.10)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+))))

(define-public rust-gtk-sys-0.9
  (package
    (inherit rust-gtk-sys-0.14)
    (name "rust-gtk-sys")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hd4w49iaivzjkbxi0bhabqp1ifkzg9g47n822sh12xlqxhgdpjk"))))
    (arguments
     `(#:tests? #f                      ;missing files
       #:cargo-inputs
       (("rust-atk-sys" ,rust-atk-sys-0.9)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gdk-sys" ,rust-gdk-sys-0.9)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gtk3-macros-0.14
  (package
    (name "rust-gtk3-macros")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk3-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yavfirn2iw9nsxik6m7s2cdxdrl5l5jfbiwn0zl85y1dnlivpi1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-heck" ,rust-heck-0.3)
        ("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
        ("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GTK 3 library")
    (description "This package provides Rust bindings for the GTK 3
library.")
    (license license:expat)))

(define-public rust-gtk4-0.7
  (package
    (name "rust-gtk4")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hh8nzglmz94v1m1h6vy8z12m6fr7ia467ry0md5fa4p7sm53sss"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; `Errors` doesn't implement `std::fmt::Display`
       #:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-field-offset" ,rust-field-offset-0.3)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.18)
                       ("rust-gdk4" ,rust-gdk4-0.7)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-graphene-rs" ,rust-graphene-rs-0.18)
                       ("rust-gsk4" ,rust-gsk4-0.7)
                       ("rust-gtk4-macros" ,rust-gtk4-macros-0.7)
                       ("rust-gtk4-sys" ,rust-gtk4-sys-0.7)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list cairo glib gtk))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings of the GTK 4 library")
    (description "Rust bindings of the GTK 4 library.")
    (license license:expat)))

(define-public rust-gtk4-macros-0.7
  (package
    (name "rust-gtk4-macros")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk4-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bw3cchiycf7dw1bw4p8946gv38azxy05a5w0ndgcmxnz6fc8znm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Failed to initialize GTK
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quick-xml" ,rust-quick-xml-0.30)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-gtk4" ,rust-gtk4-0.7)
                       ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-gtk4" ,rust-gtk4-0.7)
                                   ("rust-trybuild2" ,rust-trybuild2-1))))
    (native-inputs (list pkg-config))
    (inputs (list gdk-pixbuf gtk))
    (home-page "https://gtk-rs.org/")
    (synopsis "Macros helpers for GTK 4 bindings")
    (description "Macros helpers for GTK 4 bindings.")
    (license license:expat)))

(define-public rust-gtk4-sys-0.7
  (package
    (name "rust-gtk4-sys")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk4-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f2ylskyqkjdik9fij2m46pra4jagnif5xyalbxfk3334fmc9n2l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; Can't find gtk/gtk-a11y.h from gtk+
       #:cargo-inputs (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.18)
                       ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.18)
                       ("rust-gdk4-sys" ,rust-gdk4-sys-0.7)
                       ("rust-gio-sys" ,rust-gio-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-graphene-sys" ,rust-graphene-sys-0.18)
                       ("rust-gsk4-sys" ,rust-gsk4-sys-0.7)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.18)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list cairo gdk-pixbuf graphene gtk pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings of GTK 4")
    (description "This package provides FFI bindings of GTK 4.")
    (license license:expat)))

(define-public rust-libadwaita-0.5
  (package
    (name "rust-libadwaita")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libadwaita" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "174pzn9dwsk8ikvrhx13vkh0zrpvb3rhg9yd2q5d2zjh0q6fgrrg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.18)
                       ("rust-gdk4" ,rust-gdk4-0.7)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-gtk4" ,rust-gtk4-0.7)
                       ("rust-libadwaita-sys" ,rust-libadwaita-sys-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18))))
    (native-inputs (list pkg-config))
    (inputs (list libadwaita))
    (home-page "https://world.pages.gitlab.gnome.org/Rust/libadwaita-rs")
    (synopsis "Rust bindings for libadwaita")
    (description "Rust bindings for libadwaita.")
    (license license:expat)))

(define-public rust-libadwaita-sys-0.5
  (package
    (name "rust-libadwaita-sys")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libadwaita-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16n6xsy6jhbj0jbpz8yvql6c9b89a99v9vhdz5s37mg1inisl42y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; ‘AdwBannerClass’ undeclared
       #:cargo-inputs (("rust-gdk4-sys" ,rust-gdk4-sys-0.7)
                       ("rust-gio-sys" ,rust-gio-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gtk4-sys" ,rust-gtk4-sys-0.7)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.18)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list gtk libadwaita))
    (home-page "https://world.pages.gitlab.gnome.org/Rust/libadwaita-rs/")
    (synopsis "FFI bindings for libadwaita")
    (description "FFI bindings for libadwaita.")
    (license license:expat)))

(define-public rust-pango-0.19
  (package
    (name "rust-pango")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kffxkk7730csly86fkgja50k1184zj9lz49sv7qb0059233439z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gio" ,rust-gio-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.19))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Pango library")
    (description "This package provides Rust bindings for the Pango library.")
    (license license:expat)))

(define-public rust-pango-0.18
  (package
    (inherit rust-pango-0.19)
    (name "rust-pango")
    (version "0.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r5ygq7036sv7w32kp8yxr6vgggd54iaavh3yckanmq4xg0px8kw"))))
    (arguments
     `(#:cargo-inputs (("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pango-sys" ,rust-pango-sys-0.18))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pango-0.17
  (package
    (inherit rust-pango-0.18)
    (name "rust-pango")
    (version "0.17.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j0xj9yw81qivqb0y34j9c5azjsdspxp1zzpvxi1zri0qrplbgim"))))
    (arguments
     `(;; FIXME: error[E0277]: `Errors` doesn't implement `std::fmt::Display`
       #:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-gio" ,rust-gio-0.17)
        ("rust-glib" ,rust-glib-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pango-sys" ,rust-pango-sys-0.17))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pango-0.15
  (package
    (inherit rust-pango-0.17)
    (name "rust-pango")
    (version "0.15.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ksf85fqkw4y5pf21p84g5xn4fnqn21cbrmx2d9yx6k591ah9r12"))))
    (arguments
     `(;; FIXME: error[E0277]: `Errors` doesn't implement `std::fmt::Display`
       #:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pango-sys" ,rust-pango-sys-0.15))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pango-0.14
  (package
    (inherit rust-pango-0.15)
    (name "rust-pango")
    (version "0.14.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10c5q8wl9gkjh323whq6pg9yfvr2vmz00f98z1d77jp506cdavsl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pango-sys" ,rust-pango-sys-0.14))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pango-0.9
  (package
    (inherit rust-pango-0.14)
    (name "rust-pango")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f64hynd9vq6966wb66mrg5kq9q371bkhncp37nqrgdyh22hcdwr"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pango-sys" ,rust-pango-sys-0.10))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pango-0.8
  (package
    (inherit rust-pango-0.9)
    (name "rust-pango")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pango" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0xq50950il3228grzs4xvc5s6phxcl5l50grz6syvs0vixr6p70y"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pango-sys-0.19
  (package
    (name "rust-pango-sys")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "182bcd6255v5yvnskbhxnb6kwak240z7sn54si2b5h46l17xl0zz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libpango-1.0")
    (description "This package provides FFI bindings to @code{libpango-1.0}.")
    (license license:expat)))

(define-public rust-pango-sys-0.18
  (package
    (inherit rust-pango-sys-0.19)
    (name "rust-pango-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1iaxalcaaj59cl9n10svh4g50v8jrc1a36kd7n9yahx8j7ikfrs3"))))
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-pango-sys-0.17
  (package
    (inherit rust-pango-sys-0.18)
    (name "rust-pango-sys")
    (version "0.17.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15aindwdxsydzvqcvhc9ysamx5v1jmq8qbs61ncxic2h72grz9ix"))))
    (arguments
     `(#:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.17)
        ("rust-gobject-sys" ,rust-gobject-sys-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-pango-sys-0.15
  (package
    (inherit rust-pango-sys-0.17)
    (name "rust-pango-sys")
    (version "0.15.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1an3c931kbkr08n9d9d1dapsq3n26zs0xn4ixn11jrp4rn0h186j"))))
    (arguments
     `(#:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.15)
        ("rust-gobject-sys" ,rust-gobject-sys-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-pango-sys-0.14
  (package
    (inherit rust-pango-sys-0.15)
    (name "rust-pango-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zj236n9kjldf47wwlxvhshwm5zhg589a0fml5mm8qg7lnf0jrr3"))))
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-gobject-sys" ,rust-gobject-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-3))))))

(define-public rust-pango-sys-0.10
  (package
    (inherit rust-pango-sys-0.14)
    (name "rust-pango-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1468xzyk2wanxb2b32fsmwk6bnafdaicxl5b4301dlb2ic66bli4"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1))))))

(define-public rust-pango-sys-0.9
  (package
    (inherit rust-pango-sys-0.10)
    (name "rust-pango-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zdynikh6jrfa31fpmbrwnz46a57idci73zzkf0z0g3vj223vfc6"))))
    (arguments
     `(#:tests? #f                  ; Some test files not included in release.
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-pangocairo-0.19
  (package
    (name "rust-pangocairo")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pangocairo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n8wrqy260zpfiifb2n10mbsv3kbrvxm1z7pv8b4w77c08yb9j74"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.19)
                       ("rust-pangocairo-sys" ,rust-pangocairo-sys-0.19))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the PangoCairo library")
    (description
     "This package provides Rust bindings for the @code{PangoCairo} library.")
    (license license:expat)))

(define-public rust-pangocairo-0.18
  (package
    (inherit rust-pangocairo-0.19)
    (name "rust-pangocairo")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pangocairo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "199qdgw5mf1vbqmslscy1qzz0szw2dcd21p6z61wzjngm64na0sp"))))
    (arguments
     `(#:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18)
                       ("rust-pangocairo-sys" ,rust-pangocairo-sys-0.18))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pangocairo-0.17
  (package
    (inherit rust-pangocairo-0.18)
    (name "rust-pangocairo")
    (version "0.17.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pangocairo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kr2b31034b8zif4r3lf4rniqphk2bxi2q7n8iwq2wrf3k5jkgw6"))))
    (arguments
     `(;; FIXME: error[E0277]: `Errors` doesn't implement `std::fmt::Display`
       #:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.17)
        ("rust-glib" ,rust-glib-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.17)
        ("rust-pangocairo-sys" ,rust-pangocairo-sys-0.17))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pangocairo-0.15
  (package
    (inherit rust-pangocairo-0.17)
    (name "rust-pangocairo")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pangocairo" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0rjk0clrjxah4kc0kybn7l7bxa5m5kpxkihxc2i7a6hx3xfa8xkq"))))
    (arguments
     `(;; FIXME: error[E0277]: `Errors` doesn't implement `std::fmt::Display`
       #:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.15)
        ("rust-glib" ,rust-glib-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.15)
        ("rust-pangocairo-sys" ,rust-pangocairo-sys-0.15))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pangocairo-sys-0.19
  (package
    (name "rust-pangocairo-sys")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pangocairo-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1myq3p8qrd63nlacd4sba66c17lfqgvzv8mpyn2rg1rqhi4h86ar"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.19)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.19)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs
     ;; XXX: Should these be propagated from their respective crates?
     (list cairo glib pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to PangoCairo")
    (description "This package provides FFI bindings to @code{PangoCairo}.")
    (license license:expat)))

(define-public rust-pangocairo-sys-0.18
  (package
    (inherit rust-pangocairo-sys-0.19)
    (name "rust-pangocairo-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pangocairo-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cw82261ixgr9xw549rx11w74h0wz4lw0pqxxir7lzm3fvv8yg7w"))))
    (arguments
     `(#:cargo-inputs (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.18)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-pangocairo-sys-0.17
  (package
    (inherit rust-pangocairo-sys-0.18)
    (name "rust-pangocairo-sys")
    (version "0.17.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pangocairo-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ymr4aqrdpysz0rh383s4h6hj8nbkyj7n4723f45zzzqkf6x7pwl"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.17)
        ("rust-glib-sys" ,rust-glib-sys-0.17)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.17)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-pangocairo-sys-0.15
  (package
    (inherit rust-pangocairo-sys-0.17)
    (name "rust-pangocairo-sys")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pangocairo-sys" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "154llaawy60zh8fkw2yq0r31ynpmqlsr7brryzaq2v4ijijp9kvq"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.15)
        ("rust-glib-sys" ,rust-glib-sys-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.15)
        ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-soup-sys-0.10
  (package
    (name "rust-soup-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "soup-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gm1b1dj6z3l94sklw6jvqblhik8i8gz2mgrf6xhnqv5hpqaviy3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;tests FAILED
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib libsoup-minimal-2))
    (home-page "https://github.com/gtk-rs/soup-sys-rs")
    (synopsis "Soup FFI crate for Rust")
    (description "This crate provides Soup FFI for Rust.")
    (license license:expat)))

(define-public rust-webkit2gtk-sys-0.12
  (package
    (name "rust-webkit2gtk-sys")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webkit2gtk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0686iy2jrq8h2i2p4zb53mg32ql5zagba1fskcdi23asr0w537iq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;tests FAILED
       #:cargo-inputs
       (("rust-atk-sys" ,rust-atk-sys-0.10)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.10)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.10)
        ("rust-gdk-sys" ,rust-gdk-sys-0.10)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-sys" ,rust-gtk-sys-0.10)
        ("rust-javascriptcore-rs-sys" ,rust-javascriptcore-rs-sys-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.10)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-soup-sys" ,rust-soup-sys-0.10))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib webkitgtk-with-libsoup2))
    (home-page "https://github.com/tauri-apps/webkit2gtk-rs")
    (synopsis "Rust binding for webkit-gtk library")
    (description "This crate provides Rust binding for webkit-gtk library.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
