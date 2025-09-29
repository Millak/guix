;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2025 宋文武 <iyzsong@envs.net>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024 gemmaro <gemmaro.dev@gmail.com>
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

(define-module (gnu packages easyrpg)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml))

(define-public liblcf
  (package
    (name "liblcf")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://easyrpg.org/downloads/player/" version
                    "/liblcf-" version ".tar.gz"))
              (sha256
               (base32
                "1b68yhs14b4ql1wfbm0jzklyqyi3b2wm3pm9zhx0ij2a98c8cnli"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; Required by 'liblcf.pc'.
     (list expat icu4c libinih))
    (home-page "https://easyrpg.org/")
    (synopsis "Library to handle RPG Maker 2000 and 2003 game data")
    (description
     "@code{liblcf} is a library to handle RPG Maker 2000 and 2003 game data.
It can read and write LCF and XML files.")
    ;; It includes a copy of span-lite (boost-1.0):
    ;;   src/lcf/third_party/span.h
    (license license:expat)))

(define-public easyrpg-player
  (package
    (name "easyrpg-player")
    (version "0.8.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://easyrpg.org/downloads/player/" version
                    "/easyrpg-player-" version ".tar.gz"))
              (sha256
               (base32
                "0aa60568cvhxf93065wn2r6js7xh81vhsl2nw3bhs5g3l1smq28z"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--enable-fmmidi"
             (string-append "--with-bash-completion-dir="
                            %output "/etc/bash_completion.d/"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list fluidsynth
           fmt
           freetype
           harfbuzz
           liblcf
           libpng
           libsndfile
           libvorbis
           libxmp
           mpg123
           opusfile
           pixman
           sdl2
           speexdsp
           wildmidi
           zlib))
    (home-page "https://easyrpg.org/")
    (synopsis "Play RPG Maker 2000 and 2003 games")
    (description
     "EasyRPG Player is a game interpreter to play RPG Maker 2000, 2003 and
EasyRPG games.  It uses the LCF parser library (liblcf) to read RPG Maker game
data.")
    ;; It bundles FMMidi YM2608 FM synthesizer emulator (bsd-3):
    ;;   src/midisynth.h
    ;;   src/midisynth.cpp
    ;; and WAV audio loader and writer (public-domain):
    ;;   src/external/dr_wav.h
    (license license:gpl3+)))

(define-public libretro-easyrpg
  (let ((libretro-common
         (origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/libretro/libretro-common")
                  (commit "668749ae38a9e85744d1c15a652a1e8db8ab9e82")))
           (file-name "libretro-common-checkout")
           (sha256
            (base32
             "007hd1ys3ikyjx4zigkxl2h0172p7d9p9vj09739yqfkvxkwlbl2")))))
    (package
      (inherit easyrpg-player)
      (name "libretro-easyrpg")
      (source (origin
                (inherit (package-source easyrpg-player))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
                     (copy-recursively #$libretro-common
                                       "builds/libretro/libretro-common")))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f                ;no tests
             #:configure-flags #~'("-DPLAYER_TARGET_PLATFORM=libretro")
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'install
                   (lambda _
                     (install-file
                      "easyrpg_libretro.so"
                      (string-append #$output "/lib/libretro/")))))))
      (synopsis "Libretro core to play RPG Maker 2000 and 2003 games"))))
