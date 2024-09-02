;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
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
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
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
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://easyrpg.org/downloads/player/" version
                    "/liblcf-" version ".tar.gz"))
              (sha256
               (base32
                "0kskflh2izc8q5p5x0rfxw8xa3591xfkmqf74rj72ff34xri4nj1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; Required by 'liblcf.pc'.
     (list expat icu4c))
    (home-page "https://easyrpg.org/")
    (synopsis "Library to handle RPG Maker 2000 and 2003 game data")
    (description
     "@code{liblcf} is a library to handle RPG Maker 2000 and 2003 game data.
It can read and write LCF and XML files.")
    ;; It includes a copy of Boost Preprocessor Cat and Stringize (boost-1.0):
    ;;   src/boost/preprocessor/config.hpp
    ;;   src/boost/preprocessor/cat.hpp
    ;;   src/boost/preprocessor/stringize.hpp
    ;; and a copy of inih (bsd-3):
    ;;   src/ini.h
    ;;   src/ini.cpp
    ;;   src/inireader.h
    ;;   src/inireader.cpp
    ;; TODO: Unbundle them.
    (license license:expat)))

(define-public easyrpg-player
  (package
    (name "easyrpg-player")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://easyrpg.org/downloads/player/" version
                    "/easyrpg-player-" version ".tar.gz"))
              (sha256
               (base32
                "1brx2iix9d5i2lyjjcs03pq1xgl1gyh0yas8avazahgd9sn47696"))))
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
           sdl2-mixer
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
    ;; and PicoJSON JSON parser/serializer (bsd-2):
    ;;   src/picojson.h
    ;; TODO: Unbundle them.
    (license license:gpl3+)))
