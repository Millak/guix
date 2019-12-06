;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016, 2018, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Evan Straw <evan.straw99@gmail.com>
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

(define-module (gnu packages mpd)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph))

(define-public libmpdclient
  (package
    (name "libmpdclient")
    (version "2.16")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://musicpd.org/download/libmpdclient/"
                              (car (string-split version #\.))
                              "/libmpdclient-" version ".tar.xz"))
              (sha256
               (base32
                "0r24cl3i9nvs6a47mvwaxk1kb5wmnhkhrw1q5cq9010fgjvdlszs"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)

       ;; For building HTML documentation.
       ("doxygen" ,doxygen)

       ;; For tests.
       ("check" ,check)))
    (arguments
     `(#:configure-flags
       (list "-Ddocumentation=true"
             "-Dtest=true")))
    (synopsis "Music Player Daemon client library")
    (description "A stable, documented, asynchronous API library for
interfacing MPD in the C, C++ & Objective C languages.")
    (home-page "https://www.musicpd.org/libs/libmpdclient/")
    (license license:bsd-3)))

(define-public mpd
  (package
    (name "mpd")
    (version "0.21.16")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://musicpd.org/download/mpd/"
                              (version-major+minor version)
                              "/mpd-" version ".tar.xz"))
              (sha256
               (base32
                "14jd9byvxlxmn9ygkyap1izy0d7i2lsssib74y3q8f6pyzfipkrh"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Ddocumentation=true"))) ;the default is 'false'...
    (inputs `(("ao" ,ao)
              ("alsa-lib" ,alsa-lib)
              ("avahi" ,avahi)
              ("boost" ,boost)
              ("curl" ,curl)
              ("ffmpeg" ,ffmpeg)
              ("flac" ,flac)
              ("glib" ,glib)
              ("icu4c" ,icu4c)
              ;; The LAME decoder comes from FFmpeg, but is added here so that
              ;; configure picks up the LAME encoder.
              ("lame" ,lame)
              ("libid3tag" ,libid3tag)
              ("libmpdclient" ,libmpdclient)
              ("libsamplerate" ,libsamplerate)
              ("libsndfile" ,libsndfile)
              ("libvorbis" ,libvorbis)
              ("opus" ,opus)
              ("pulseaudio" ,pulseaudio)
              ("sqlite" ,sqlite)
              ("zlib" ,zlib)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("python-sphinx" ,python-sphinx)))
    ;; Missing optional inputs:
    ;;   libyajl
    ;;   libcdio_paranoia
    ;;   libmms
    ;;   libadplug
    ;;   libaudiofile
    ;;   faad2
    ;;   fluidsynth
    ;;   libgme
    ;;   libshout
    ;;   libmpg123
    ;;   libmodplug
    ;;   libmpcdec
    ;;   libsidplay2
    ;;   libwavpack
    ;;   libwildmidi
    ;;   libtwolame
    ;;   libroar
    ;;   libjack
    ;;   OpenAL
    (synopsis "Music Player Daemon")
    (description "Music Player Daemon (MPD) is a flexible, powerful,
server-side application for playing music.  Through plugins and libraries it
can play a variety of sound files while being controlled by its network
protocol.")
    (home-page "https://www.musicpd.org/")
    (license license:gpl2)))

(define-public mpd-mpc
  (package
    (name "mpd-mpc")
    (version "0.33")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://www.musicpd.org/download/mpc/"
                              (car (string-split version #\.))
                              "/mpc-" version ".tar.xz"))
              (sha256
               (base32
                "15hjpzqs83v1zx49x8nkpwy9hpl1jxd55z1w50vm82gm32zcqh2g"))))
    (build-system meson-build-system)
    (inputs `(("libmpdclient" ,libmpdclient)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)))
    (synopsis "Music Player Daemon client")
    (description "MPC is a minimalist command line interface to MPD, the music
player daemon.")
    (home-page "https://www.musicpd.org/clients/mpc/")
    (license license:gpl2)))

(define-public ncmpc
  (package
    (name "ncmpc")
    (version "0.36")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://musicpd.org/download/ncmpc/"
                              (version-major version)
                              "/ncmpc-" version ".tar.xz"))
              (sha256
               (base32
                "0b1yg93is9057p9d7vfv5nz85d11p6jq94fw07jn3zlfzhyqfcj6"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       ;; Otherwise, they are installed incorrectly, in
       ;; '$out/share/man/man/man1'.
       (list (string-append "-Dmandir=" (assoc-ref %outputs "out")
                            "/share"))))
    (inputs `(("boost" ,boost)
              ("pcre" ,pcre)
              ("libmpdclient" ,libmpdclient)
              ("ncurses" ,ncurses)))
    (native-inputs `(("gettext" ,gettext-minimal) ; for xgettext
                     ("pkg-config" ,pkg-config)
                     ("python-sphinx" ,python-sphinx)))
    (synopsis "Curses Music Player Daemon client")
    (description "ncmpc is a fully featured MPD client, which runs in a
terminal using ncurses.")
    (home-page "https://www.musicpd.org/clients/ncmpc/")
    (license license:gpl2)))

(define-public ncmpcpp
  (package
    (name "ncmpcpp")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://ncmpcpp.rybczak.net/stable/ncmpcpp-"
                              version ".tar.bz2"))
              (sha256
               (base32
                "0m0mjb049sl62vx13h9waavysa30mk0rphacksnvf94n13la62v5"))))
    (build-system gnu-build-system)
    (inputs `(("libmpdclient" ,libmpdclient)
              ("boost"  ,boost)
              ("readline" ,readline)
              ("ncurses" ,ncurses)
              ("taglib" ,taglib)
              ("icu4c" ,icu4c)
              ("curl" ,curl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:configure-flags
       '("BOOST_LIB_SUFFIX=" "--with-taglib")))
    (synopsis "Featureful ncurses based MPD client inspired by ncmpc")
    (description "Ncmpcpp is an mpd client with a UI very similar to ncmpc,
but it provides new useful features such as support for regular expressions
for library searches, extended song format, items filtering, the ability to
sort playlists, and a local file system browser.")
    (home-page "https://ncmpcpp.rybczak.net/")
    (license license:gpl2+)))

(define-public mpdscribble
  (package
    (name "mpdscribble")
    (version "0.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.musicpd.org/download/mpdscribble/"
                                  version "/mpdscribble-" version ".tar.gz"))
              (sha256
               (base32
                "0f0ybx380x2z2g1qvdndpvcrhkrgsfqckhz3ryydq2w3pl12v27z"))))
    (build-system gnu-build-system)
    (inputs `(("libmpdclient" ,libmpdclient)
              ("curl" ,curl)
              ("glib" ,glib)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (synopsis "MPD client for track scrobbling")
    (description "mpdscribble is a Music Player Daemon client which submits
information about tracks being played to a scrobbler, such as Libre.FM.")
    (home-page "https://www.musicpd.org/clients/mpdscribble/")
    (license license:gpl2+)))

(define-public python-mpd2
  (package
    (name "python-mpd2")
    (version "0.5.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-mpd2" version))
              (sha256
               (base32
                "0laypd7h1j14b4vrmiayqlzdsh2j5hc3zv4l0fqvbrbw9y6763ii"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "python" "mpd_test.py"))))))
    (native-inputs `(("python-mock" ,python-mock)))
    (home-page "https://github.com/Mic92/python-mpd2")
    (synopsis "Python MPD client library")
    (description "Python-mpd2 is a Python library which provides a client
interface for the Music Player Daemon.")
    (license license:lgpl3+)))

(define-public python2-mpd2
  (package-with-python2 python-mpd2))

(define-public sonata
  (package
    (name "sonata")
    (version "1.7b1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/multani/sonata/archive/v"
                              version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07gq2nxqwxs0qyxjbay7k5j25zd386bn7wdr2dl1gk53diwnn7s0"))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build python-build-system)
                           (guix build glib-or-gtk-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'install 'wrap-sonata
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/sonata")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("python-mpd2" ,python-mpd2)
       ("gtk+" ,gtk+)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gobject-introspection" ,gobject-introspection)
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Elegant client for the Music Player Daemon")
    (description "Sonata is an elegant graphical client for the Music Player
Daemon (MPD).  It supports playlists, multiple profiles (connecting to different
MPD servers, search and multimedia key support.")
    (home-page "https://www.nongnu.org/sonata/")
    (license license:gpl3+)))

(define-public ashuffle
  (package
    (name "ashuffle")
    (version "2.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/joshkunz/ashuffle.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11aa95cg0yca2m2d00sar6wr14g3lc7cfm9bin1h7lk7asdm8azp"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libmpdclient" ,libmpdclient)))
    (build-system meson-build-system)
    (home-page "https://github.com/joshkunz/ashuffle")
    (synopsis "Automatic library-wide shuffle for mpd")
    (description "ashuffle is an application for automatically shuffling your
MPD library in a similar way to many other music players' 'shuffle library'
feature. ashuffle works like any other MPD client, and can be used alongside
other MPD frontends.")
    (license license:expat)))
