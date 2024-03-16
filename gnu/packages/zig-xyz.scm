;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maya Tomasek <maya.tomasek@disroot.org>
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2024 Justin Veilleux <terramorpha@cock.li>
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

(define-module (gnu packages zig-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system zig)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zig))

(define-public river
  (package
    (name "river")
    (version "0.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/riverwm/river")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nvhqs6wwisf8ama7y1y3q3nf2jm9sh5bn46z8kyds8cikm0x1vh"))))
    (build-system zig-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-wayland-session
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (wayland-sessions
                      (string-append out "/share/wayland-sessions")))
                (mkdir-p wayland-sessions)
                (install-file "contrib/river.desktop"
                              wayland-sessions)))))
      #:zig-build-flags #~(list "-Dxwayland") ;experimental xwayland support
      #:zig-release-type "safe"))
    (native-inputs (list libevdev
                         libxkbcommon
                         pkg-config
                         pixman
                         scdoc
                         wayland
                         wayland-protocols
                         wlroots-0.16))
    (home-page "https://github.com/riverwm/river")
    (synopsis "Dynamic tiling Wayland compositor")
    (description
     "River is a dynamic tiling Wayland compositor with flexible
runtime configuration.  It can run nested in an X11/Wayland session or also
directly from a tty using KMS/DRM.")
    (license license:gpl3)))

(define-public tigerbeetle
  (package
    (name "tigerbeetle")
    (version "0.13.35")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tigerbeetledb/tigerbeetle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x8msknvq8s6vnlczq5fxmaiqvig2sbcv60c3x8zbgr28dsqpmll"))))
    (build-system zig-build-system)
    (arguments
     (list
      #:zig zig-0.9
      #:zig-release-type "safe"))
    (synopsis "Distributed financial accounting database")
    (description "TigerBeetle is a financial accounting database designed for
mission-critical safety and performance for financial services.")
    (home-page "https://github.com/tigerbeetledb/tigerbeetle")
    (license license:asl2.0)))

(define-public zig-zls
  (package
    (name "zig-zls")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zigtools/zls")
                    (commit version)
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lsks7h3z2m4psyn9mwdylv1d6a9i3z54ssadiz76w0clbh8ch9k"))))
    (build-system zig-build-system)
    (inputs (list zig-0.10 python))
    (arguments
     ;; The tests fail with memory leaks.
     (list #:tests? #f))
    (synopsis "Zig language server")
    (description
     "Zig Language Server is a language server implementing the @acronym{LSP,
Language Server Protocol} for the Zig programming language.")
    (home-page "https://github.com/zigtools/zls")
    (license license:expat)))
