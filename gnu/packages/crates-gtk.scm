;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2023 Sergio Pastor Pérez <sergio.pastorperez@outlook.es>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (gnu packages crates-audio)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages webkit))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-aperture-0.3
  (package
    (name "rust-aperture")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aperture" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02bjzskxp91br91yvf5f32wakp1i9948sxbsy9hdrxs52w38hr61"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs (list rust-gst-plugin-gtk4-0.11
                                rust-gstreamer-0.21
                                rust-gstreamer-pbutils-0.21
                                rust-gstreamer-video-0.21
                                rust-gtk4-0.7
                                rust-log-0.4
                                rust-pkg-config-0.3)))
    (native-inputs (list pkg-config))
    (inputs (list gdk-pixbuf glib graphene gstreamer gst-plugins-base
                  gst-plugins-bad gtk pango))
    (home-page "https://gitlab.gnome.org/GNOME/snapshot")
    (synopsis "GTK Widget for cameras using gstreamer and pipewire")
    (description
     "This package provides GTK Widget for cameras using gstreamer and pipewire.")
    (license license:gpl3+)))

(define-public rust-ashpd-0.6
  (package
    (name "rust-ashpd")
    (version "0.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ashpd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "109d7w6v0rnpy9lv4kmhwgh0sff0440s2vybj1k0ik4ib3d2xhja"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--" "--skip=src/lib.rs - (line 21)")
       #:cargo-inputs (("rust-async-fs" ,rust-async-fs-2)
                       ("rust-async-net" ,rust-async-net-2)
                       ("rust-enumflags2" ,rust-enumflags2-0.7)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gdk4-wayland" ,rust-gdk4-wayland-0.7)
                       ("rust-gdk4-x11" ,rust-gdk4-x11-0.7)
                       ("rust-gtk4" ,rust-gtk4-0.7)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pipewire" ,rust-pipewire-0.7)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.1)
                       ("rust-wayland-client" ,rust-wayland-client-0.30)
                       ("rust-wayland-protocols" ,rust-wayland-protocols-0.30)
                       ("rust-zbus" ,rust-zbus-3))
       #:cargo-development-inputs (("rust-byteorder" ,rust-byteorder-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/bilelmoussaoui/ashpd")
    (synopsis "XDG portals wrapper in Rust using zbus")
    (description
     "This package provides an XDG portals wrapper in Rust using zbus.")
    (license license:expat)))

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

(define-public rust-atk-sys-0.18
  (package
    (name "rust-atk-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0280k0xw21n3zzri8ynk5mxy7v1mk9d506l962lhngp3j1yhn7i5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=cross_validate_constants_with_c"
                            "--skip=cross_validate_layout_with_c")
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs
     (list at-spi2-core glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libatk-1")
    (description "This package provides FFI bindings to libatk-1.")
    (license license:expat)))

(define-public rust-atk-sys-0.14
  (package
    (inherit rust-atk-sys-0.18)
    (name "rust-atk-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sl3pqfb2jaf9kcfxj9k43d7iv8gcl5zgdgn3j5vp13w2mqgdp5s"))))
    (arguments
     `(#:tests? #f                      ; missing files
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.14)
        ("rust-gobject-sys" ,rust-gobject-sys-0.14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

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

(define-public rust-cairo-rs-0.20
  (package
    (name "rust-cairo-rs")
    (version "0.20.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rp3jjgdz2996lk69chr84fjxr73y1gdl7c138093rks3ng6kynp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.20)
                       ("rust-freetype-rs" ,rust-freetype-rs-0.37)
                       ("rust-glib" ,rust-glib-0.20)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs (("rust-float-eq" ,rust-float-eq-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list cairo))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Cairo library")
    (description "This package provides Rust bindings for the Cairo library.")
    (license license:expat)))

(define-public rust-cairo-rs-0.19
  (package
    (inherit rust-cairo-rs-0.20)
    (name "rust-cairo-rs")
    (version "0.19.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qp5rixgipdj9d8yd5458hzfxam1rgpzcxi90vq6q0v91r6jmb5j"))))
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.19)
                       ("rust-freetype-rs" ,rust-freetype-rs-0.35)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-float-eq" ,rust-float-eq-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

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

(define-public rust-cairo-sys-rs-0.20
  (package
    (name "rust-cairo-sys-rs")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13x85l52nl2izmdb48zzpkhhh1a4dsgqlp8gys4n1f5r2kwr10j2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7)
                       ("rust-windows-sys" ,rust-windows-sys-0.52)
                       ("rust-x11" ,rust-x11-2))))
    (native-inputs (list pkg-config))
    (inputs (list cairo))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libcairo")
    (description "This package provides FFI bindings to libcairo.")
    (license license:expat)))

(define-public rust-cairo-sys-rs-0.19
  (package
    (inherit rust-cairo-sys-rs-0.20)
    (name "rust-cairo-sys-rs")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r0yp0lph77lm4blrn6fvdmz2i3r8ibkkjg6nmwbvvv4jq8v6fzx"))))
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6)
                       ("rust-windows-sys" ,rust-windows-sys-0.52)
                       ("rust-x11" ,rust-x11-2))))))

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

(define-public rust-gdk-pixbuf-0.20
  (package
    (name "rust-gdk-pixbuf")
    (version "0.20.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "068vj60x1hbw0anhjvg2id3sr96d202wp18a4zc3f8z9m5qr1hn4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.20)
                       ("rust-gio" ,rust-gio-0.20)
                       ("rust-glib" ,rust-glib-0.20)
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

(define-public rust-gdk-pixbuf-0.19
  (package
    (inherit rust-gdk-pixbuf-0.20)
    (name "rust-gdk-pixbuf")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16c6kznkh3vi82843ays8awdm37fwjd1fblv6g3h64824shsnkk2"))))
    (arguments
     `(#:cargo-inputs (("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.19)
                       ("rust-gio" ,rust-gio-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

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

(define-public rust-gdk-pixbuf-sys-0.20
  (package
    (name "rust-gdk-pixbuf-sys")
    (version "0.20.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gdz43p4zsck7w6isin0zi3ydacg0c3l6yyqzgrza7drb6q46wv8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gio-sys" ,rust-gio-sys-0.20)
                       ("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list gdk-pixbuf gtk+))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgdk_pixbuf-2.0")
    (description
     "This package provides FFI bindings to @code{libgdk_pixbuf-2.0}.")
    (license license:expat)))

(define-public rust-gdk-pixbuf-sys-0.19
  (package
    (inherit rust-gdk-pixbuf-sys-0.20)
    (name "rust-gdk-pixbuf-sys")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y93g24mdgskvyhva46xv3qyb1cvj5xpi0yqnh7cb31wz2j0byjf"))))
    (arguments
     `(#:cargo-inputs (("rust-gio-sys" ,rust-gio-sys-0.19)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

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

(define-public rust-gdk-sys-0.18
  (package
    (name "rust-gdk-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1whznljhqqni5sk1qwazkc75ik5gmc1zh8590cbswv9qndn8bzri"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags (list "--release" "--"
                                "--skip=cross_validate_constants_with_c")
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
    (inputs
     (list cairo gdk-pixbuf gtk+ glib pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgdk-3")
    (description "This package provides FFI bindings to libgdk-3.")
    (license license:expat)))

(define-public rust-gdk-sys-0.14
  (package
    (inherit rust-gdk-sys-0.18)
    (name "rust-gdk-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07hz3gg039sy7iffy2w5srxzsnqf15i3ryxkqfd995k67lyin28f"))))
    (arguments
     `(#:skip-build? #t
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
        ("rust-tempfile" ,rust-tempfile-3))))))

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

(define-public rust-gdk4-0.8
  (package
    (name "rust-gdk4")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01qak43mrlszsy9cfsmqk1ql4228m2rylbg514g3fsidsjfmq9nv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.19)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.19)
                       ("rust-gdk4-sys" ,rust-gdk4-sys-0.8)
                       ("rust-gio" ,rust-gio-0.19)
                       ("rust-gl" ,rust-gl-0.14)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.19))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list cairo gdk-pixbuf gtk))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings of the GDK 4 library")
    (description "This package provides rust bindings of the GDK 4 library.")
    (license license:expat)))

(define-public rust-gdk4-0.7
  (package
    (inherit rust-gdk4-0.8)
    (name "rust-gdk4")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xiacc63p73apr033gjrb9dsk0y4yxnsljwfxbwfry41snd03nvy"))))
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
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk4-sys-0.8
  (package
    (name "rust-gdk4-sys")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pb6vklx9ik7jx9cmrw2vywlx9ssqhll8q77ky8p8w56x2s8yhf9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.19)
                       ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.19)
                       ("rust-gio-sys" ,rust-gio-sys-0.19)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.19)
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

(define-public rust-gdk4-sys-0.7
  (package
    (inherit rust-gdk4-sys-0.8)
    (name "rust-gdk4-sys")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w7yvir565sjrrw828lss07749hfpfsr19jdjzwivkx36brl7ayv"))))
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
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gdk4-wayland-0.7
  (package
    (name "rust-gdk4-wayland")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-wayland" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04zkspjs1r6l4gj241p9xm2zmp91phm1khakw5jvsm8yy4pi1f8d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gdk4" ,rust-gdk4-0.7)
                       ("rust-gdk4-wayland-sys" ,rust-gdk4-wayland-sys-0.7)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-khronos-egl" ,rust-khronos-egl-5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.1)
                       ("rust-wayland-client" ,rust-wayland-client-0.30)
                       ("rust-xkb" ,rust-xkb-0.3))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gtk))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "Rust bindings of the GDK 4 Wayland library")
    (description
     "This package provides Rust bindings of the GDK 4 Wayland library.")
    (license license:expat)))

(define-public rust-gdk4-wayland-sys-0.7
  (package
    (name "rust-gdk4-wayland-sys")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "092nbn4gk82kbdvji2qnqy181l4pf5i8961bb8nj3q3a4nz5k0fl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gtk))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "FFI bindings of GDK4 Wayland")
    (description "This package provides FFI bindings of GDK4 Wayland.")
    (license license:expat)))

(define-public rust-gdk4-win32-0.7
  (package
    (name "rust-gdk4-win32")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-win32" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mv04mipl57v1lj94j2rkrk9qm75jvdlnp7qm6nl2kpn8466arpy"))))
    (build-system cargo-build-system)
    (arguments
     ;; The system library `gtk4-win32` required by gtk4-win32-sys` was not found.
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gdk4" ,rust-gdk4-0.7)
                       ("rust-gdk4-win32-sys" ,rust-gdk4-win32-sys-0.7)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-khronos-egl" ,rust-khronos-egl-5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-windows" ,rust-windows-0.52))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "Rust bindings of the GDK4 Win32 library")
    (description
     "This package provides Rust bindings of the GDK4 Win32 library.")
    (license license:expat)))

(define-public rust-gdk4-win32-sys-0.7
  (package
    (name "rust-gdk4-win32-sys")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-win32-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v9rkv1i1sbzckigdr5zjy0xzz6qf9iccikvg3qxjfnd8rsihp2b"))))
    (build-system cargo-build-system)
    (arguments
     ;; The system library `gtk4-win32` required by gtk4-win32-sys` was not found.
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gdk4-sys" ,rust-gdk4-sys-0.7)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "FFI bindings of GDK4 Win32")
    (description "This package provides FFI bindings of GDK4 Win32.")
    (license license:expat)))

(define-public rust-gdk4-x11-0.7
  (package
    (name "rust-gdk4-x11")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-x11" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l54c1m0gsdm07drvy171a0i97ic2kygmzf3fjg4da0yxbwbpj98"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gdk4" ,rust-gdk4-0.7)
                       ("rust-gdk4-x11-sys" ,rust-gdk4-x11-sys-0.7)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-khronos-egl" ,rust-khronos-egl-5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-x11" ,rust-x11-2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gtk))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "Rust bindings of the GDK4 X11 library")
    (description
     "This package provides Rust bindings of the GDK4 X11 library.")
    (license license:expat)))

(define-public rust-gdk4-x11-sys-0.7
  (package
    (name "rust-gdk4-x11-sys")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-x11-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09gill32x6qy4s55xjckqvqrfxw4jfjrlcpmd4iijn076w4igpm3"))))
    (build-system cargo-build-system)
    (arguments
     ;; tests/manual.h:3:10: fatal error: gdk/gdkx.h: No such file or directory
     `(#:tests? #f
       #:cargo-inputs (("rust-gdk4-sys" ,rust-gdk4-sys-0.7)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gtk))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "FFI bindings of GDK4 X11")
    (description "This package provides FFI bindings of GDK4 X11.")
    (license license:expat)))

(define-public rust-gio-0.20
  (package
    (name "rust-gio")
    (version "0.20.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "071n06a1zsngjr4q7k0kr7qzbxmg1pm0psjby3hkvkjnmnlx49l8"))))
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
                       ("rust-gio-sys" ,rust-gio-sys-0.20)
                       ("rust-glib" ,rust-glib-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-smallvec" ,rust-smallvec-1))
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

(define-public rust-gio-0.19
  (package
    (inherit rust-gio-0.20)
    (name "rust-gio")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1znz5ngfvv3gbndf6lzz3hs27hlb8ysls4axlfccrzvkscbz2jac"))))
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
        ("rust-serial-test" ,rust-serial-test-3))))))

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
    (version "0.20.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g19jl1rg5r9isz5mpi211wswwanrgpxk9y1nlb4ij1l6mpxyrdr"))))
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

(define-public rust-glib-0.20
  (package
    (name "rust-glib")
    (version "0.20.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qfmp4cgcm7zv92z5bc1cv8rmh8cjdbdn06r6nhvb2lrwx73xgc6"))))
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
                       ("rust-gio-sys" ,rust-gio-sys-0.20)
                       ("rust-glib-macros" ,rust-glib-macros-0.20)
                       ("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-smallvec" ,rust-smallvec-1))
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

(define-public rust-glib-0.19
  (package
    (inherit rust-glib-0.20)
    (name "rust-glib")
    (version "0.19.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i2ak1scmzfmfxbm2dr146jl4y9mafxf1ald05jr8iimy5wh4r9r"))))
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
        ("rust-trybuild2" ,rust-trybuild2-1))))))

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

(define-public rust-glib-macros-0.20
  (package
    (name "rust-glib-macros")
    (version "0.20.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mn1054ypcw906qpgvkqdz1wfjjb2kgfsmh4lwfykhyggai1rlp7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-heck" ,rust-heck-0.5)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-glib" ,rust-glib-0.20)
                                   ("rust-trybuild2" ,rust-trybuild2-1))))
    (native-inputs (list pkg-config))
    (inputs (list glib))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GLib library, proc macros crate")
    (description
     "This package provides Rust bindings for the GLib library, proc macros crate.")
    (license license:expat)))

(define-public rust-glib-macros-0.19
  (package
    (inherit rust-glib-macros-0.20)
    (name "rust-glib-macros")
    (version "0.19.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mzsh8jkg8vldvgvr9gsaidvn2myn5cbdn8a6m8rgbhlg8kv0aa4"))))
    (arguments
     `(#:cargo-inputs (("rust-heck" ,rust-heck-0.5)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-glib" ,rust-glib-0.19)
                                   ("rust-trybuild2" ,rust-trybuild2-1))))))

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
    (version "0.20.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pn2hynpky8kh776wrlyd7wp2spzaxfqw8pv1lnc87v2x0kih2rx"))))
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
    (name "rust-gobject-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
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

(define-public rust-graphene-rs-0.19
  (package
    (name "rust-graphene-rs")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "graphene-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1krblj6kbnixgkmz2b3494jmlm2xlv3qz5qm585frn943l1qdyzm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib" ,rust-glib-0.19)
                       ("rust-graphene-sys" ,rust-graphene-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib graphene))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Graphene library")
    (description "Rust bindings for the Graphene library.")
    (license license:expat)))

(define-public rust-graphene-rs-0.18
  (package
    (inherit rust-graphene-rs-0.19)
    (name "rust-graphene-rs")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "graphene-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00f4q1ra4haap5i7lazwhkdgnb49fs8adk2nm6ki6mjhl76jh8iv"))))
    (arguments
     `(#:tests? #f ; `Errors` doesn't implement `std::fmt::Display`
       #:cargo-inputs (("rust-glib" ,rust-glib-0.18)
                       ("rust-graphene-sys" ,rust-graphene-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-graphene-sys-0.19
  (package
    (name "rust-graphene-sys")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "graphene-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01dg4wgqxaqkdv0vl7hr14b6kbbm96gwdsb5a2ss9jxw8h4hwlrg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
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

(define-public rust-graphene-sys-0.18
  (package
    (inherit rust-graphene-sys-0.19)
    (name "rust-graphene-sys")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "graphene-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n8zlg7z26lwpnvlqp1hjlgrs671skqwagdpm7r8i1zwx3748hfc"))))
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gsk4-0.8
  (package
    (name "rust-gsk4")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gsk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gi1f9s2nd5m2zfwb91vijpzr6nxbfa58inrwml497wkyr5qhqvm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.19)
                       ("rust-gdk4" ,rust-gdk4-0.8)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-graphene-rs" ,rust-graphene-rs-0.19)
                       ("rust-gsk4-sys" ,rust-gsk4-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.19))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list gtk))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings of the GSK 4 library")
    (description "Rust bindings of the GSK 4 library.")
    (license license:expat)))

(define-public rust-gsk4-0.7
  (package
    (inherit rust-gsk4-0.8)
    (name "rust-gsk4")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gsk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zhzs2dkgiinhgc11akpn2harq3x5n1iq21dnc4h689g3lsqx58d"))))
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
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gsk4-sys-0.8
  (package
    (name "rust-gsk4-sys")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gsk4-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p5kf912s8qs38lhzzwnm26v498wkp68mx92z38vnf3ccgr4n0i3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.19)
                       ("rust-gdk4-sys" ,rust-gdk4-sys-0.8)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-graphene-sys" ,rust-graphene-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.19)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list cairo gdk-pixbuf graphene gtk pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings of GSK 4")
    (description "This package provides FFI bindings of GSK 4.")
    (license license:expat)))

(define-public rust-gsk4-sys-0.7
  (package
    (inherit rust-gsk4-sys-0.8)
    (name "rust-gsk4-sys")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gsk4-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mbdlm9qi1hql48rr29vsj9vlqwc7gxg67wg1q19z67azwz9xg8j"))))
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
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gst-plugin-gtk4-0.11
  (package
    (name "rust-gst-plugin-gtk4")
    (version "0.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gst-plugin-gtk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hmky9p16hnhrwm5i63ynlwfl1bpc9fp3as5ibrni1qlfq0vhwdj"))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:cargo-inputs (list rust-async-channel-2
                            rust-gdk4-wayland-0.7
                            rust-gdk4-win32-0.7
                            rust-gdk4-x11-0.7
                            rust-gst-plugin-version-helper-0.8
                            rust-gstreamer-0.21
                            rust-gstreamer-allocators-0.21
                            rust-gstreamer-base-0.21
                            rust-gstreamer-gl-0.21
                            rust-gstreamer-gl-egl-0.21
                            rust-gstreamer-gl-wayland-0.21
                            rust-gstreamer-gl-x11-0.21
                            rust-gstreamer-video-0.21
                            rust-gtk4-0.7
                            rust-once-cell-1
                            rust-windows-sys-0.52)))
    (native-inputs (list pkg-config))
    (inputs (list gdk-pixbuf glib graphene gstreamer gst-plugins-base
                  gtk pango))
    (home-page "https://gitlab.freedesktop.org/gstreamer/gst-plugins-rs")
    (synopsis "GStreamer GTK 4 sink element")
    (description "This package provides GStreamer GTK 4 sink element.")
    (license license:mpl2.0)))

(define-public rust-gst-plugin-version-helper-0.8
  (package
    (name "rust-gst-plugin-version-helper")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gst-plugin-version-helper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0alv0v7jfg7ryybb3qnbdwx3nqzkdl305il1xk92y9b02r7qfpjf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-toml-edit" ,rust-toml-edit-0.22))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "build.rs helper function for GStreamer plugin metadata")
    (description
     "This package provides build.rs helper function for GStreamer plugin metadata.")
    (license license:expat)))

(define-public rust-gstreamer-0.21
  (package
    (name "rust-gstreamer")
    (version "0.21.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mchpvvll5i4ck8zr7aarrz6p975n0dcyy92wksg8ycf9hzp15fy"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-test-flags
           `(list "--release" "--"
                  "--skip=typefind::tests::test_typefind_call_function")
           #:cargo-inputs `(("rust-cfg-if" ,rust-cfg-if-1)
                            ("rust-futures-channel" ,rust-futures-channel-0.3)
                            ("rust-futures-core" ,rust-futures-core-0.3)
                            ("rust-futures-util" ,rust-futures-util-0.3)
                            ("rust-glib" ,rust-glib-0.18)
                            ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.21)
                            ("rust-itertools" ,rust-itertools-0.12)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-muldiv" ,rust-muldiv-1)
                            ("rust-num-integer" ,rust-num-integer-0.1)
                            ("rust-num-rational" ,rust-num-rational-0.4)
                            ("rust-option-operations" ,rust-option-operations-0.5)
                            ("rust-paste" ,rust-paste-1)
                            ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                            ("rust-pretty-hex" ,rust-pretty-hex-0.4)
                            ("rust-serde" ,rust-serde-1)
                            ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                            ("rust-smallvec" ,rust-smallvec-1)
                            ("rust-thiserror" ,rust-thiserror-1))
           #:cargo-development-inputs
            `(("rust-futures-executor" ,rust-futures-executor-0.3)
              ("rust-gir-format-check" ,rust-gir-format-check-0.1)
              ("rust-ron" ,rust-ron-0.8)
              ("rust-serde-json" ,rust-serde-json-1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer")
    (description "Rust bindings for GStreamer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-0.20
  (package
    (inherit rust-gstreamer-0.21)
    (name "rust-gstreamer")
    (version "0.20.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jcg143f5k1s4p8knqw0cc8x81shgax0spx1dypiranl4021b960"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=typefind::tests::test_typefind_call_function")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-glib" ,rust-glib-0.17)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-muldiv" ,rust-muldiv-1)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-rational" ,rust-num-rational-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-option-operations" ,rust-option-operations-0.5)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-pretty-hex" ,rust-pretty-hex-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-ron" ,rust-ron-0.8)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-gstreamer-0.18
  (package
    (inherit rust-gstreamer-0.21)
    (name "rust-gstreamer")
    (version "0.18.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mjlnw9917j3wwij8225bjp54k7408lxqjjnh6r6wksyryx66qyn"))))
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
              ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-gstreamer-allocators-0.21
  (package
    (name "rust-gstreamer-allocators")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-allocators" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14s244qk5dznd1pvswps7k61s6ksjd3pv0fzvpgybm1k9c5v57vc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-glib" ,rust-glib-0.18)
        ("rust-gstreamer" ,rust-gstreamer-0.21)
        ("rust-gstreamer-allocators-sys" ,rust-gstreamer-allocators-sys-0.21)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Allocators library")
    (description
     "This package provides Rust bindings for GStreamer Allocators library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-allocators-sys-0.21
  (package
    (name "rust-gstreamer-allocators-sys")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-allocators-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1brps4xpzigavifcwr6hs6368nilc4l03ivnv13fxb2svdw885f1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--"
                            "--skip=cross_validate_constants_with_c"
                            "--skip=cross_validate_layout_with_c")
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstallocators-1.0")
    (description "This package provides FFI bindings to libgstallocators-1.0.")
    (license license:expat)))

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

(define-public rust-gstreamer-audio-0.21
  (package
    (name "rust-gstreamer-audio")
    (version "0.21.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-audio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b91wjhhq0harwl7kyfv5l0kwp6w1vklpnpynakv92f8x6jci5vs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; https://github.com/gtk-rs/gtk3-rs/issues/768
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-gstreamer" ,rust-gstreamer-0.21)
                       ("rust-gstreamer-audio-sys" ,rust-gstreamer-audio-sys-0.21)
                       ("rust-gstreamer-base" ,rust-gstreamer-base-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-itertools" ,rust-itertools-0.12)
        ("rust-serde-json" ,rust-serde-json-1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for the GStreamer Audio library")
    (description "Rust bindings for the GStreamer Audio library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-audio-0.18
  (package
    (inherit rust-gstreamer-audio-0.21)
    (name "rust-gstreamer-audio")
    (version "0.18.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-audio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zl0bj03rz19qlrm50w7i5sagh0i0p5d8gr7ig1k6k5yd7k47sww"))))
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
                                   ("rust-itertools" ,rust-itertools-0.10))))))

(define-public rust-gstreamer-audio-sys-0.21
  (package
    (name "rust-gstreamer-audio-sys")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-audio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lamp4s9cl0hhpbfcwdprn36fll6qq4xihrqbf2pfwqpifp99gbq"))))
    (build-system cargo-build-system)
    (arguments
     ;; Some symbols are missing, i.e. ?GstDsdFormat? and more.
     `(#:tests? #f
       #:cargo-test-flags
       (list "--release" "--"
             "--skip=cross_validate_constants_with_c")
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.21)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.21)
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

(define-public rust-gstreamer-audio-sys-0.18
  (package
    (inherit rust-gstreamer-audio-sys-0.21)
    (name "rust-gstreamer-audio-sys")
    (version "0.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-audio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z3xryblh75xp08xyw3m6jfz9azarcvl06dd3psc0n65agxmhhm3"))))
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
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gstreamer-base-0.21
  (package
    (name "rust-gstreamer-base")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-base" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zqnld0w2jqkz1m5xna3a3nnrpvrchpcrrzdgwim54540ilhn5fb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; https://github.com/gtk-rs/gtk3-rs/issues/768
       #:cargo-inputs (("rust-atomic-refcell" ,rust-atomic-refcell-0.1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-gstreamer" ,rust-gstreamer-0.21)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Base library")
    (description "Rust bindings for GStreamer Base library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-base-0.18
  (package
    (inherit rust-gstreamer-base-0.21)
    (name "rust-gstreamer-base")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-base" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gw6sr75h01y3j6lpxhc7p1frvkba9a4imyyb2ppqh42cprkakr2"))))
    (arguments
     `(#:tests? #f  ; https://github.com/gtk-rs/gtk3-rs/issues/768
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-glib" ,rust-glib-0.15)
                       ("rust-gstreamer" ,rust-gstreamer-0.18)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gstreamer-base-sys-0.21
  (package
    (name "rust-gstreamer-base-sys")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-base-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r2bb4rmkpxs1l2jy61rn2srqzsp1f8q0k5j55di3zkqj0gp1jpl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.21)
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

(define-public rust-gstreamer-base-sys-0.18
  (package
    (inherit rust-gstreamer-base-sys-0.21)
    (name "rust-gstreamer-base-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-base-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13h79fcn3b9bkg7h8j0vxc1zryp92shbvvk6gkx723il7hy4k0x0"))))
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.15)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.15)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gstreamer-check-0.21
  (package
    (name "rust-gstreamer-check")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-check" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sf4jiykz4mc45ydlszggfn2ly9liqgvam1cmiiyxz9l58pascj2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
      (("rust-glib" ,rust-glib-0.18)
       ("rust-gstreamer" ,rust-gstreamer-0.21)
       ("rust-gstreamer-check-sys" ,rust-gstreamer-check-sys-0.21))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Check library")
    (description
     "This package provides Rust bindings for GStreamer Check library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-check-sys-0.21
  (package
    (name "rust-gstreamer-check-sys")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-check-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yvsz1pf4zr9gya7d8mbq3y4qai72iz1hgdxfiqqn136rrazpa6z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstcheck-1.0")
    (description "This package provides FFI bindings to libgstcheck-1.0.")
    (license license:expat)))

(define-public rust-gstreamer-gl-0.21
  (package
    (name "rust-gstreamer-gl")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-gl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "140vnlxnkq12c8qqgc5i2y9wdz8c8dga25d99021cg16508vkkry"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib" ,rust-glib-0.18)
                       ("rust-gstreamer" ,rust-gstreamer-0.21)
                       ("rust-gstreamer-base" ,rust-gstreamer-base-0.21)
                       ("rust-gstreamer-gl-sys" ,rust-gstreamer-gl-sys-0.21)
                       ("rust-gstreamer-video" ,rust-gstreamer-video-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serde-json" ,rust-serde-json-1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base mesa))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer GL library")
    (description
     "This package provides Rust bindings for GStreamer GL library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-gl-egl-0.21
  (package
    (name "rust-gstreamer-gl-egl")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-gl-egl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10nwlmyw1z4jccyrbqijx6iny2c64164jaz05dgnvi5378ianwx1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-glib" ,rust-glib-0.18)
        ("rust-gstreamer" ,rust-gstreamer-0.21)
        ("rust-gstreamer-gl" ,rust-gstreamer-gl-0.21)
        ("rust-gstreamer-gl-egl-sys" ,rust-gstreamer-gl-egl-sys-0.21)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base mesa))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer GL library (EGL support)")
    (description
     "This package provides Rust bindings for GStreamer GL library (EGL support).")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-gl-egl-sys-0.21
  (package
    (name "rust-gstreamer-gl-egl-sys")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-gl-egl-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m79m0lbk2s89cm4vc6ckwkgs9khmh2ri7x3gfgmz2hwy2v8hg7f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gstreamer-gl-sys" ,rust-gstreamer-gl-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base mesa))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstgl-1.0 (EGL support)")
    (description
     "This package provides FFI bindings to libgstgl-1.0 (EGL support).")
    (license license:expat)))

(define-public rust-gstreamer-gl-sys-0.21
  (package
    (name "rust-gstreamer-gl-sys")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-gl-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kgi8rrlw2qx1p6q9ybk52wxpjn5wscx84lqfg4ng9lr7hdrg06m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--"
                            "--skip=cross_validate_constants_with_c"
                            "--skip=cross_validate_layout_with_c")
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.21)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.21)
                       ("rust-gstreamer-video-sys" ,rust-gstreamer-video-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base mesa))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstgl-1.0")
    (description "This package provides FFI bindings to libgstgl-1.0.")
    (license license:expat)))

(define-public rust-gstreamer-gl-wayland-0.21
  (package
    (name "rust-gstreamer-gl-wayland")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-gl-wayland" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zz7as2qlf021dvpy1qs4rbahf94p6jb1msmfsgx08nhyai7dhpy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-glib" ,rust-glib-0.18)
        ("rust-gstreamer" ,rust-gstreamer-0.21)
        ("rust-gstreamer-gl" ,rust-gstreamer-gl-0.21)
        ("rust-gstreamer-gl-wayland-sys" ,rust-gstreamer-gl-wayland-sys-0.21)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base mesa))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer GL library (Wayland support)")
    (description
     "This package provides Rust bindings for GStreamer GL library (Wayland support).")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-gl-wayland-sys-0.21
  (package
    (name "rust-gstreamer-gl-wayland-sys")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-gl-wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "188j8i47zn93gph6ngmpjqbvm44jy0wzybr5052s6lxadzqqcywi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gstreamer-gl-sys" ,rust-gstreamer-gl-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base mesa))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstgl-1.0 (Wayland support)")
    (description
     "This package provides FFI bindings to libgstgl-1.0 (Wayland support).")
    (license license:expat)))

(define-public rust-gstreamer-gl-x11-0.21
  (package
    (name "rust-gstreamer-gl-x11")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-gl-x11" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zgn5aih3bcz3ci3xkdpc2jzxrxiz1hdpkwq121w5ln96ag1n0np"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib" ,rust-glib-0.18)
                       ("rust-gstreamer" ,rust-gstreamer-0.21)
                       ("rust-gstreamer-gl" ,rust-gstreamer-gl-0.21)
                       ("rust-gstreamer-gl-x11-sys" ,rust-gstreamer-gl-x11-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base mesa))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer GL library (X11 support)")
    (description
     "This package provides Rust bindings for GStreamer GL library (X11 support).")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-gl-x11-sys-0.21
  (package
    (name "rust-gstreamer-gl-x11-sys")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-gl-x11-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p5wdrn3h55jx15963z1wnk7whwplpjfymy5yjsmkqdrqw1yz6n4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gstreamer-gl-sys" ,rust-gstreamer-gl-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base mesa))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstgl-1.0 (X11 support)")
    (description
     "This package provides FFI bindings to libgstgl-1.0 (X11 support).")
    (license license:expat)))

(define-public rust-gstreamer-pbutils-0.21
  (package
    (name "rust-gstreamer-pbutils")
    (version "0.21.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-pbutils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0idr354x9j77q8qrb99r6m6hrpa0z8j97jncqim5m08vhgbij9sb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
      (("rust-glib" ,rust-glib-0.18)
       ("rust-gstreamer" ,rust-gstreamer-0.21)
       ("rust-gstreamer-audio" ,rust-gstreamer-audio-0.21)
       ("rust-gstreamer-pbutils-sys" ,rust-gstreamer-pbutils-sys-0.21)
       ("rust-gstreamer-video" ,rust-gstreamer-video-0.21)
       ("rust-libc" ,rust-libc-0.2)
       ("rust-serde" ,rust-serde-1)
       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-gir-format-check" ,rust-gir-format-check-0.1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Base Utils library")
    (description
     "This package provides Rust bindings for GStreamer Base Utils library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-pbutils-sys-0.21
  (package
    (name "rust-gstreamer-pbutils-sys")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-pbutils-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0scx3w02wkyvmq76ia2jr6zhkf24zivn9vyphrcwmj2b8piydakg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gstreamer-audio-sys" ,rust-gstreamer-audio-sys-0.21)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.21)
                       ("rust-gstreamer-video-sys" ,rust-gstreamer-video-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstpbutils-1.0")
    (description "This package provides FFI bindings to libgstpbutils-1.0.")
    (license license:expat)))

(define-public rust-gstreamer-sys-0.21
  (package
    (name "rust-gstreamer-sys")
    (version "0.21.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i1vrqs9ys5y0ljl4nxh1x25dnwlcyh9hiybh4dysviy5dwdlk2n"))))
    (build-system cargo-build-system)
    (arguments
     (list #:tests? #f ;; tests/constant.c:193:20: error:
                       ;; ?GST_ELEMENT_FACTORY_TYPE_TIMESTAMPER? undeclared (first use in this function);
                       ;; did you mean ?GST_ELEMENT_FACTORY_TYPE_MUXER??
           #:cargo-inputs `(("rust-glib-sys" ,rust-glib-sys-0.18)
                            ("rust-gobject-sys" ,rust-gobject-sys-0.18)
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

(define-public rust-gstreamer-sys-0.20
  (package
    (inherit rust-gstreamer-sys-0.21)
    (name "rust-gstreamer-sys")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ll6ax5wjbvhccq7cx35w4jw6lyvzm017g58mzdlfzggmm3y0vz5"))))
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.17)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.17)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gstreamer-sys-0.18
  (package
    (inherit rust-gstreamer-sys-0.21)
    (name "rust-gstreamer-sys")
    (version "0.18.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qikgp5m3xj41fbfyfl6ckb5i8dxadfvlvj5bf8girn2sdjpllg3"))))
    (arguments
     (list #:cargo-inputs `(("rust-glib-sys" ,rust-glib-sys-0.15)
                            ("rust-gobject-sys" ,rust-gobject-sys-0.15)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-system-deps" ,rust-system-deps-6))
           #:cargo-development-inputs `(("rust-shell-words" ,rust-shell-words-1)
                                        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gstreamer-video-0.21
  (package
    (name "rust-gstreamer-video")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-video" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r8mhzb1bq4dnj08f4szgarxd2fvqbakwv400fp9hyiv3m6jlnz8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-gstreamer" ,rust-gstreamer-0.21)
                       ("rust-gstreamer-base" ,rust-gstreamer-base-0.21)
                       ("rust-gstreamer-video-sys" ,rust-gstreamer-video-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-itertools" ,rust-itertools-0.11)
        ("rust-serde-json" ,rust-serde-json-1))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Video library")
    (description
     "This package provides Rust bindings for GStreamer Video library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-video-sys-0.21
  (package
    (name "rust-gstreamer-video-sys")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-video-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vhn7fiibwc2q5h8cjlg44imh8i0xss6nns83r859c76k26k20h3"))))
    (build-system cargo-build-system)
    (arguments
     ;; Some symbols are missing, i.e. ?GST_VIDEO_FORMAT_A420_12BE?,
     ;; ?GST_VIDEO_FORMAT_A420_12LE?, ?GST_VIDEO_FORMAT_A420_16BE?
     ;; and more.
     `(#:tests? #f
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.21)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list glib gstreamer gst-plugins-base))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstvideo-1.0")
    (description "This package provides FFI bindings to libgstvideo-1.0.")
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

(define-public rust-gtk-sys-0.18
  (package
    (name "rust-gtk-sys")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08kpdl7ldj8lyv4dyhp9pdk61qj8yyyha5f1jssc1hg23nzkf53p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags (list "--release" "--"
                                "--skip=cross_validate_layout_with_c")
       #:cargo-inputs (("rust-atk-sys" ,rust-atk-sys-0.18)
                       ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.18)
                       ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.18)
                       ("rust-gdk-sys" ,rust-gdk-sys-0.18)
                       ("rust-gio-sys" ,rust-gio-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.18)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs
     (list gtk+))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgtk-3")
    (description "This package provides FFI bindings to libgtk-3.")
    (license license:expat)))

(define-public rust-gtk-sys-0.14
  (package
    (inherit rust-gtk-sys-0.18)
    (name "rust-gtk-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gihp9zh4z7lycp0zbmq1w0k9ddbnd2h64jsgid7hi85vb9wh54c"))))
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
        ("rust-tempfile" ,rust-tempfile-3))))))

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

(define-public rust-gtk4-0.8
  (package
    (name "rust-gtk4")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1avinslgnsz3wywf4dfaza8w9c29krd10hxmi8si3bq8kcqi2kmh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Failed to initialize GTK
       #:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.19)
                       ("rust-field-offset" ,rust-field-offset-0.3)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.19)
                       ("rust-gdk4" ,rust-gdk4-0.8)
                       ("rust-gio" ,rust-gio-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-graphene-rs" ,rust-graphene-rs-0.19)
                       ("rust-gsk4" ,rust-gsk4-0.8)
                       ("rust-gtk4-macros" ,rust-gtk4-macros-0.8)
                       ("rust-gtk4-sys" ,rust-gtk4-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.19))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list cairo glib gtk))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings of the GTK 4 library")
    (description "Rust bindings of the GTK 4 library.")
    (license license:expat)))

(define-public rust-gtk4-0.7
  (package
    (inherit rust-gtk4-0.8)
    (name "rust-gtk4")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hh8nzglmz94v1m1h6vy8z12m6fr7ia467ry0md5fa4p7sm53sss"))))
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
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gtk4-macros-0.8
  (package
    (name "rust-gtk4-macros")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk4-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0214a8y68kknxcnihsfxwsqvll7ss2rbiplr51cyk34dz1z5lrgc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Failed to initialize GTK
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-3)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quick-xml" ,rust-quick-xml-0.31)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-gtk4" ,rust-gtk4-0.8)
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

(define-public rust-gtk4-macros-0.7
  (package
    (inherit rust-gtk4-macros-0.8)
    (name "rust-gtk4-macros")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk4-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bw3cchiycf7dw1bw4p8946gv38azxy05a5w0ndgcmxnz6fc8znm"))))
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
                                   ("rust-trybuild2" ,rust-trybuild2-1))))))

(define-public rust-gtk4-sys-0.8
  (package
    (name "rust-gtk4-sys")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk4-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dapgvbkhf0kcm2jfmj8r98wzyhwmr5iv358dvb73sl5gxmsi2lc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; Can't find gtk/gtk-a11y.h from gtk+
       #:cargo-inputs (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.19)
                       ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.19)
                       ("rust-gdk4-sys" ,rust-gdk4-sys-0.8)
                       ("rust-gio-sys" ,rust-gio-sys-0.19)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-graphene-sys" ,rust-graphene-sys-0.19)
                       ("rust-gsk4-sys" ,rust-gsk4-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.19)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list cairo gdk-pixbuf graphene gtk pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings of GTK 4")
    (description "This package provides FFI bindings of GTK 4.")
    (license license:expat)))

(define-public rust-gtk4-sys-0.7
  (package
    (inherit rust-gtk4-sys-0.8)
    (name "rust-gtk4-sys")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk4-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f2ylskyqkjdik9fij2m46pra4jagnif5xyalbxfk3334fmc9n2l"))))
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
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-libadwaita-0.6
  (package
    (name "rust-libadwaita")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libadwaita" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nf5hxmk1bzjj8hxavwgz04kiv3hxb52qjh9f9gfrqdr9019kd4i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.20)
                       ("rust-gdk4" ,rust-gdk4-0.8)
                       ("rust-gio" ,rust-gio-0.20)
                       ("rust-glib" ,rust-glib-0.20)
                       ("rust-gtk4" ,rust-gtk4-0.8)
                       ("rust-libadwaita-sys" ,rust-libadwaita-sys-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.20))))
    (native-inputs (list pkg-config))
    (inputs (list libadwaita))
    (home-page "https://world.pages.gitlab.gnome.org/Rust/libadwaita-rs")
    (synopsis "Rust bindings for libadwaita")
    (description "Rust bindings for libadwaita.")
    (license license:expat)))

(define-public rust-libadwaita-0.5
  (package
    (inherit rust-libadwaita-0.6)
    (name "rust-libadwaita")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libadwaita" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "174pzn9dwsk8ikvrhx13vkh0zrpvb3rhg9yd2q5d2zjh0q6fgrrg"))))
    (arguments
     `(#:cargo-inputs (("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.18)
                       ("rust-gdk4" ,rust-gdk4-0.7)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-gtk4" ,rust-gtk4-0.7)
                       ("rust-libadwaita-sys" ,rust-libadwaita-sys-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18))))))

(define-public rust-libadwaita-sys-0.6
  (package
    (name "rust-libadwaita-sys")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libadwaita-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a513qlanw6n8dksm1br20a7iz2x1ff5cgg9v5f2dq9bx7j4i9r3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gdk4-sys" ,rust-gdk4-sys-0.8)
                       ("rust-gio-sys" ,rust-gio-sys-0.19)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-gtk4-sys" ,rust-gtk4-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.19)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list gtk libadwaita))
    (home-page "https://world.pages.gitlab.gnome.org/Rust/libadwaita-rs/")
    (synopsis "FFI bindings for libadwaita")
    (description "FFI bindings for libadwaita.")
    (license license:expat)))

(define-public rust-libadwaita-sys-0.5
  (package
    (inherit rust-libadwaita-sys-0.6)
    (name "rust-libadwaita-sys")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libadwaita-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16n6xsy6jhbj0jbpz8yvql6c9b89a99v9vhdz5s37mg1inisl42y"))))
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
                                   ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-pango-0.20
  (package
    (name "rust-pango")
    (version "0.20.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jjml2rmdh6fmayqzm6nm5gp50rbm04i89g5swrkjbn5l9y4xqvi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gio" ,rust-gio-0.20)
                       ("rust-glib" ,rust-glib-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.20))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Pango library")
    (description "This package provides Rust bindings for the Pango library.")
    (license license:expat)))

(define-public rust-pango-0.19
  (package
    (inherit rust-pango-0.20)
    (name "rust-pango")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kffxkk7730csly86fkgja50k1184zj9lz49sv7qb0059233439z"))))
    (arguments
     `(#:cargo-inputs (("rust-gio" ,rust-gio-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango-sys" ,rust-pango-sys-0.19))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

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

(define-public rust-pango-sys-0.20
  (package
    (name "rust-pango-sys")
    (version "0.20.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f5qyap96f01h7xa122s798hmz7rmc0c5fz299a0cbzigf8nbzc4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list pango))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libpango-1.0")
    (description "This package provides FFI bindings to @code{libpango-1.0}.")
    (license license:expat)))

(define-public rust-pango-sys-0.19
  (package
    (inherit rust-pango-sys-0.20)
    (name "rust-pango-sys")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "182bcd6255v5yvnskbhxnb6kwak240z7sn54si2b5h46l17xl0zz"))))
    (arguments
     `(#:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))
       #:cargo-development-inputs (("rust-shell-words" ,rust-shell-words-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))))

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
        (base32 "1iaxalcaaj59cl9n10svh4g50v8jrc1a36kd7n9yahx8j7ikfrs3"))
       (modules '((guix build utils)))
       (snippet
        ;; Don't try to use a version of pango newer than we have packaged.
        '(begin (substitute* "Cargo.toml"
                  (("1\\.51") "1.50"))))))
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

(define-public rust-search-provider-0.8
  (package
    (name "rust-search-provider")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "search-provider" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a683ndxh99sin4lr919wc8aakzgjiqlic1xglf4qs6gfpvs2prq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.19)
                       ("rust-gdk4" ,rust-gdk4-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-zbus" ,rust-zbus-4))))
    (home-page "https://gitlab.gnome.org/World/Rust/search-provider")
    (synopsis "Rust wrapper around the GNOME Shell search provider API")
    (description
     "This package provides a Rust wrapper around the GNOME Shell search
provider API.")
    (license license:gpl3+)))

(define-public rust-search-provider-0.6
  (package
    (inherit rust-search-provider-0.8)
    (name "rust-search-provider")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "search-provider" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01jby7xq0dd9rafw5kgskpbxgppn4imzm71w1sdj8iy9ci4ghh9z"))))
    (arguments
     `(#:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.18)
                       ("rust-gdk4" ,rust-gdk4-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-zbus" ,rust-zbus-3))))))

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
