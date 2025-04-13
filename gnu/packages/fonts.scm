;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2023 Sou Bunnbu <iyzsong@envs.net>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2016 Jookia <166291@gmail.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Dmitry Nikolaev <cameltheman@gmail.com>
;;; Copyright © 2016-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Toni Reina <areina@riseup.net>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2017–2023 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Mohammed Sadiq <sadiq@sadiqpk.org>
;;; Copyright © 2018 Charlie Ritter <chewzerita@posteo.net>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019, 2020 Jens Mølgaard <jens@zete.tk>
;;; Copyright © 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Baptiste Strazzulla <bstrazzull@hotmail.fr>
;;; Copyright © 2019 Alva <alva@skogen.is>
;;; Copyright © 2019 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Damien Cassou <damien@cassou.me>
;;; Copyright © 2020 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020, 2024 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2020, 2021, 2022 Simen Endsjø <simendsjo@gmail.com>
;;; Copyright © 2020 Tim Van den Langenbergh <tmt_vdl@gmx.com>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021 Antoine Côté <antoine.cote@posteo.net>
;;; Copyright © 2021 Sergiu Ivanov <sivanov@colimite.fr>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021-2023 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2021, 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022-2023 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Kitzman <kitzman@disroot.org>
;;; Copyright © 2021 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2022 Jose G Perez Taveras <josegpt27@gmail.com>
;;; Copyright © 2022 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2022 Nguyễn Gia Phong <mcsinyx@disroot.org>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Ahmad Draidi <a.r.draidi@redscript.org>
;;; Copyright © 2023 Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;;; Copyright © 2023, 2024 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2023 chris <chris@bumblehead.com>
;;; Copyright © 2023, 2024 Luis Felipe López Acevedo <sirgazil@zoho.com>
;;; Copyright © 2024 Christina O'Donnell <cdo@mutix.org>
;;; Copyright © 2025 Ashvith Shetty <ashvithshetty10@gmail.com>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Josep Bigorra <jjbigorra@gmail.com>
;;; Copyright © 2023 Santiago Payà Miralta <santiagopim@gmail.com>
;;; Copyright © 2025 Kurome <hunt31999@gmail.com>
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

(define-module (gnu packages fonts)
  #:use-module (ice-9 regex)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages c)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages xorg))

(define-public font-adwaita
  (package
    (name "font-adwaita")
    (version "48.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/adwaita-fonts/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sk6kb6v4ims3jzyfh71mx2kmwv55idr2yd1xgxlqc9lk59zhymd"))))
    (build-system font-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Install font licenses instead of the buildsystem license.
          (replace 'install-license-files
            (lambda _
              (let ((doc-dir (string-append #$output "/share/doc/"
                                            #$name "-" #$version)))
                (define (install-license form)
                  (install-file (string-append form "/LICENSE.md")
                                (string-append doc-dir "/" form)))
                (install-license "sans")
                (install-license "mono")))))))
    (home-page "https://gitlab.gnome.org/GNOME/adwaita-fonts/")
    (synopsis "GNOME Adwaita Fonts")
    (description
     "This package provides Adwaita Fonts, a variation of Inter, and Adwaita
 Mono, Iosevka customized to match Inter.")
    ;; Buildsystem and shell scripts are under the GPL, but fonts themselves are
    ;; under OFL-1.1.
    ;; https://gitlab.gnome.org/GNOME/adwaita-fonts/-/issues/14
    (license license:silofl1.1)))

(define-public font-arapey
  (let ((commit  "28fa45c7f31afe62f577b0b857570ab0326b9113")
        (revision "1"))
    (package
      (name "font-arapey")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/etunni/arapey")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1wvzx8gkw1d2dc2zqp8a75lc1kr6fvg90asyjkbrcynqi6qnwkaf"))))
      (build-system font-build-system)
      (home-page "https://github.com/etunni/arapey")
      (synopsis "Typeface with soft lines, curving tips, and rhythmic nuances")
      (description
       "Arapey (Ah-ra-pay) is a contemporary modern typeface with some
features of a Bodoni, but the structures, soft lines, and finishes leave
a calm and distinguished feeling.")
      (license license:silofl1.1))))

(define-public font-artifika
  (package
    (name "font-artifika")
    (version "1.102")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cyrealtype/Artifika")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nwjm44nys1qz3wyg0mm15gdjpz641xpmsz00n6m8065xrw86q7i"))))
    (build-system font-build-system)
    (outputs '("out" "ttf" "woff"))
    (home-page "https://github.com/cyrealtype/Artifika")
    (synopsis "Upright italic font")
    (description "Artifika is an upright italic font for fashionable display
titling.")
    (license license:silofl1.1)))

(define-public font-cardo
  (package
    (name "font-cardo")
    (version "1.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://scholarsfonts.net/cardo"
                                  (string-delete #\. version) ".zip"))
              (sha256
               (base32
                "0ps55zjva4fzmg47w2i8srrh8sqxz1wkcclihwgzlwfbaxixn0cl"))))
    (build-system font-build-system)
    (home-page "https://scholarsfonts.net/cardofnt.html")
    (synopsis "Unicode font for classical scholarship")
    (description
     "Cardo is a large unicode font specifically designed for the needs of
classicists, biblical scholars, medievalists, and linguists.  Since it may be
used to prepare materials for publication, it also contains features that are
required for high-quality typography, such as ligatures, text figures (also
known as old style numerals), true small capitals and a variety of punctuation
and space characters.")
    (license license:silofl1.1)))

(define-public font-chivo
  (let ((commit "dc61c468d79781eb5183426e88e844af16cdc3e5")
        (revision "0"))
    (package
      (name "font-chivo")
      (version (git-version "20221010" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Omnibus-Type/Chivo")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0gdsnflnzwy8ajrk93dxwjashxisln58qcqa6dh4smnk7k0a34qs"))))
      (build-system font-build-system)
      (outputs '("out" "ttf" "woff"))
      (home-page "https://fonts.google.com/specimen/Chivo")
      (synopsis "The Chivo family of fonts")
      (description "Google Chivo Fonts is a grotesque family of fonts, ideal for
highlights and headlines.  In october 2022, the family is upgraded to a
variable font ranging from Thin to Black, including matching italics.  The
glyphset has also been extended, supporting now a wider number of languages.")
      (license license:silofl1.1))))

(define-public font-ibm-plex
  (package
    (name "font-ibm-plex")
    (version "6.4.2")
    ;; We prefer git-fetch since it lets us get the opentype, truetype and web
    ;; fonts all in one download. The zip archive releases separate the
    ;; opentype, truetype and web fonts into three separate archives.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/IBM/plex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00lzbm1b7zbx5q3p0s8fh9q9zj6z4k01fn7n177iybh9xn4jgx0p"))))
    (build-system font-build-system)
    (outputs '("out" "ttf" "woff"))
    (home-page "https://github.com/IBM/plex")
    (synopsis "IBM Plex typeface")
    (description
     "This package provides the Plex font family.  It comes in a Sans, Serif,
Mono and Sans Condensed, all with roman and true italics.  The fonts have been
designed to work well in user interface (UI) environments as well as other
mediums.")
    (license license:silofl1.1)))

(define-public font-lilex
  (package
    (name "font-lilex")
    (version "2.510")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mishamyrt/Lilex/releases/download/"
                    version
                    "/Lilex.zip"))
              (sha256
               (base32
                "0dq54qk4q1ymdqnp0skxdxzhx475g2gihzs8ijx0nffa29fwzn9g"))))
    (build-system font-build-system)
    (home-page "https://github.com/mishamyrt/Lilex")
    (synopsis "IBM Plex typeface with extended character sets and ligatures")
    (description "Lilex is a modern programming font containing a set of
ligatures for common programming multi-character combinations.")
    (license license:silofl1.1)))

(define-public font-lisnoti
  (let ((commit "9adec42aa352918bfb399c4f0b273f191b922836")
        (revision "1"))
    (package
      (name "font-lisnoti")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Lisnoti/Lisnoti")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0agy7l2qjmdxvw52bbmnfxqxc5n7l76pq5ha4zg1c1fk8vnkapk7"))))
      (build-system font-build-system)
      (home-page "https://github.com/Lisnoti/Lisnoti")
      (synopsis "Proportional sans serif font for general use and code")
      (description
       "Lisnoti is a proportional sans serif font derived from Noto's sans
serif fonts and is intended for both general use and for writing
computer code, including in a maths and science context.

Lisnoti is available in regular, italic, bold and bold-italic
variants.")
      (license license:silofl1.1))))

(define-public font-inconsolata
  (package
    (name "font-inconsolata")
    (version "3.000")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/googlefonts/Inconsolata/"
                           "releases/download/v" version "/fonts_otf.zip"))
       (sha256
        (base32 "1wavvv86nwsqm5sbmnkv1bprj7l7zdrkxpvjy6w8yag93k6hrlx1"))))
    (build-system font-build-system)
    (home-page "https://levien.com/type/myfonts/inconsolata.html")
    (synopsis "Monospace font")
    (description "A monospace font, designed for code listings and the like,
in print.  With attention to detail for high resolution rendering.")
    (license license:silofl1.1)))

(define-public font-intel-one-mono
  (package
    (name "font-intel-one-mono")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/intel-one-mono")
                    (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1snwxpcdxl62z1mcax19bmsmbw0hi6m0cqkxqz79ydynfch95sd0"))))
    (outputs '("out" "ttf" "woff"))
    (build-system font-build-system)
    (home-page "https://github.com/intel/intel-one-mono")
    (synopsis "Expressive monospaced font family")
    (description
     "This package provides Intel One Mono, an expressive monospaced font
family that's built with clarity, legibility, and the needs of developers in
mind.")
    (license license:silofl1.1)))

(define-public font-dejavu
  (package
    (name "font-dejavu")
    (version "2.37")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/dejavu/dejavu/"
                                 version "/dejavu-fonts-ttf-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1mqpds24wfs5cmfhj57fsfs07mji2z8812i5c4pi5pbi738s977s"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-conf
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((conf-dir (string-append (assoc-ref outputs "out")
                                            "/share/fontconfig/conf.avail")))
               (copy-recursively "fontconfig" conf-dir)
               #t))))))
    (home-page "https://dejavu-fonts.github.io/")
    (synopsis "Vera font family derivate with additional characters")
    (description "DejaVu provides an expanded version of the Vera font family
aiming for quality and broader Unicode coverage while retaining the original
Vera style.  DejaVu currently works towards conformance to the Multilingual
European Standards (MES-1 and MES-2) for Unicode coverage.  The DejaVu fonts
provide serif, sans and monospaced variants.")
    (license
     (license:x11-style
      "http://dejavu-fonts.org/"))))

(define-public font-bitstream-vera
  (package
    (name "font-bitstream-vera")
    (version "1.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/ttf-bitstream-vera/"
                                 version "/ttf-bitstream-vera-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1p3qs51x5327gnk71yq8cvmxc6wgx79sqxfvxcv80cdvgggjfnyv"))))
    (build-system font-build-system)
    (home-page "https://www.gnome.org/fonts/")
    (synopsis "Bitstream Vera sans-serif typeface")
    (description "Vera is a sans-serif typeface from Bitstream, Inc.  This
package provides the TrueType (TTF) files.")
    (license
     (license:fsdg-compatible
      "https://www.gnome.org/fonts/#Final_Bitstream_Vera_Fonts"
      "The Font Software may be sold as part of a larger software package but
no copy of one or more of the Font Software typefaces may be sold by
itself."))))

(define-public font-canada1500
  (package
    (name "font-canada1500")
    (version "1.101")
    (source (origin
              (method url-fetch/zipbomb)
              (uri "https://typodermicfonts.com/wp-content/uploads/2017/06/canada1500.zip")
              (sha256
               (base32
                "0cdcb89ab6q7b6jd898bnvrd1sifbd2xr42qgji98h8d5cq4b6fp"))))
    (build-system font-build-system)
    (arguments
     '(#:license-file-regexp "^license.pdf$"))
    (outputs '("out" "ttf"))
    (home-page "https://typodermicfonts.com/canada1500/")
    (synopsis "Canadian typeface that supports English, French and Aboriginal languages")
    (description "Canada1500 is a display typeface originally created for the
Canadian sesquicentennial with four weights, italics and space symbols which
includes lining and old-style numerals, tabular and proportional.  Greek,
Cyrillic, Canadian Syllabics and most Latin based languages are supported.")
    (license license:cc0)))

(define-public font-abattis-cantarell
  ;; Use the latest commit, as the last released version, 0.303, has problems
  ;; with the newer statmake.  The dependency has been removed in the latest
  ;; code base.
  (let ((commit "e049149faf0c15b0711e8d790e2333be923f0486")
        (revision "0"))
    (package
      (name "font-abattis-cantarell")
      (version (git-version "0.303" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.gnome.org/GNOME/cantarell-fonts")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "032csq99bkmmgh9mmmbrgg4fzxgkcsvxv4wy595qms72mmlgmcc7"))))
      (build-system meson-build-system)
      (arguments
       (list #:configure-flags #~(list "-Dbuildstatics=true")))
      (native-inputs
       (list gettext-minimal
             psautohint
             python
             python-cffsubr
             python-fontmath
             python-ufo2ft))
      (home-page "https://wiki.gnome.org/Projects/CantarellFonts")
      (synopsis "Cantarell sans-serif typeface")
      (description "The Cantarell font family is a contemporary Humanist
sans-serif designed for on-screen reading.  It is used by GNOME@tie{}3.
This package contains both the non-variable as well as the variable versions
of the font.")
      (license license:silofl1.1))))

(define-public font-atkinson-hyperlegible
  (let ((commit "1cb311624b2ddf88e9e37873999d165a8cd28b46")
        (revision "0"))
    (package
      (name "font-atkinson-hyperlegible")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/googlefonts/atkinson-hyperlegible")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qw0pr1dhmqrgmw1acyw8dv8kbm66xa1bq0mrn7yin4q1kk2dpj4"))))
      (build-system font-build-system)
      (home-page "https://github.com/googlefonts/atkinson-hyperlegible")
      (synopsis "Typeface with greater legibility and readability for low vision readers")
      (description
       "Atkinson Hyperlegible is a freely available typeface built around a grotesque sans-serif core,
intended to be optimally legible for readers who are partially visually impaired, with
all characters maximally distinguishable from one another.")
      (license license:silofl1.1))))

(define-public font-lato
  (package
    (name "font-lato")
    (version "2.015")                   ; also update description
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.latofonts.com/download/Lato2OFL.zip"))
              (sha256
               (base32
                "1f5540g0ja1nx3ddd3ywn77xc81ssrxpq8n3gyb9sabyq2b4xda2"))))
    (build-system font-build-system)
    (home-page "https://www.latofonts.com/lato-free-fonts/")
    (synopsis "Lato sans-serif typeface")
    (description
     "Lato is a sanserif typeface family.  It covers over 3000 glyphs per style.
The Lato 2.010 family supports more than 100 Latin-based languages, over
50 Cyrillic-based languages as well as Greek and IPA phonetics.")
    (license license:silofl1.1)))

(define-public font-carlito
  (let ((commit "3a810cab78ebd6e2e4eed42af9e8453c4f9b850a")
        (revision "1"))
    (package
      (name "font-carlito")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/googlefonts/carlito")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qf3zdpbmx3ylamka82nqxjpm3qjfm0nca2yzzsxmvg7krjyz12k"))))
      (build-system font-build-system)
      (home-page "https://github.com/googlefonts/carlito")
      (synopsis "Free alternative to Calibri")
      (description
       "Carlito is a font designed by Łukasz Dziedzic derived from
Lato (also designed by Łukasz Dziedzic) that is metric-compatible with
Calibri.")
      (license license:silofl1.1))))

(define-public font-gfs-ambrosia
  ;; Based on
  ;; https://src.fedoraproject.org/rpms/gfs-ambrosia-fonts
  ;; /blob/rawhide/f/gfs-ambrosia-fonts.spec.
  (package
    (name "font-gfs-ambrosia")
    (version "20080624")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.greekfontsociety-gfs.gr/"
                           "_assets/fonts/GFS_Ambrosia.zip"))
       (sha256
        (base32
         "0vnnsal61slgj9r4q35wiznd4mbcv49dl18n91s3nvv6jzd4r8b4"))))
    (build-system font-build-system)
    (home-page "https://www.greekfontsociety-gfs.gr/")
    (synopsis "GFS Ambrosia, a Greek majuscule font family")
    (description "GFS Ambrosia is a Greek typeface that has the main
characteristics of the majuscule forms of the early Christian tradition.  The
font is provided in the OpenType font (OTF) format.")
    (license license:silofl1.1)))

(define-public font-gnu-freefont
  (package
    (name "font-gnu-freefont")
    (version "20120503")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/freefont/freefont-src-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0yk58blhcd4hm7nyincmqq4jrzjjk82wif2zmk1l3y2m4vif4qhd"))
             (patches (search-patches "font-gnu-freefont-python3-compat.patch"))
             (snippet
              '(begin (delete-file "tools/generate/buildutils.pyc")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'install
                   (lambda _
                     (let ((doc-dir  (string-append %output "/share/doc/"
                                                    ,name "-" ,version))
                           (ttf-font-dir (string-append %output
                                                        "/share/fonts/truetype"))
                           (otf-font-dir (string-append %output
                                                        "/share/fonts/opentype"))
                           (woff-font-dir (string-append %output
                                                         "/share/fonts/webfonts")))
                       (mkdir-p doc-dir)
                       (substitute* "Makefile"
                         (("\\$\\(TMPDIR\\)") doc-dir)
                         (("sfd/\\*.ttf") "")
                         (("sfd/\\*.otf") "")
                         (("sfd/\\*.woff") ""))
                       ;; XXX The FreeFont Makefile tries to use the current
                       ;; time and date as names for generated files, and fails
                       ;; silently. But the fonts are still installed, so we
                       ;; leave the issue alone for now.
                       ;; See <https://bugs.gnu.org/40783>
                       (system* "make" "ttftar" "otftar" "wofftar")
                       (mkdir-p ttf-font-dir)
                       (mkdir-p otf-font-dir)
                       (mkdir-p woff-font-dir)
                       (for-each (lambda (file)
                                   (install-file file ttf-font-dir))
                                 (filter
                                   (lambda (file) (string-suffix? "ttf" file))
                                   (find-files "." "")))
                       (for-each (lambda (file)
                                   (install-file file otf-font-dir))
                                 (filter
                                   (lambda (file) (string-suffix? "otf" file))
                                   (find-files "." "")))
                       (for-each (lambda (file)
                                   (install-file file woff-font-dir))
                                 (filter
                                   (lambda (file) (string-suffix? "woff" file))
                                   (find-files "." "")))))))
       #:test-target "tests"))
    (native-inputs (list fontforge))
    (home-page "https://www.gnu.org/software/freefont/")
    (synopsis "Unicode-encoded outline fonts")
    (description
     "The GNU Freefont project aims to provide a set of free outline
 (PostScript Type0, TrueType, OpenType...) fonts covering the ISO
10646/Unicode UCS (Universal Character Set).")
    (license license:gpl3+)
    (properties '((upstream-name . "freefont")
                  (ftp-directory . "/gnu/freefont")))))

(define-public font-liberation
  (package
    (name "font-liberation")
    (version "2.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/liberationfonts/liberation-fonts/"
             "files/7261482/liberation-fonts-ttf-" version ".tar.gz"))
       (sha256
        (base32 "1l15iwk0x75621q67qlh9wv561c0gc7x0kh9l9rrz29qpxlwd4bi"))))
    (build-system font-build-system)
    (home-page "https://github.com/liberationfonts")
    (synopsis "Fonts compatible with Arial, Times New Roman, and Courier New")
    (description
     "The Liberation font family aims at metric compatibility with
Arial, Times New Roman, and Courier New.
There are three sets:

@enumerate
@item Sans (a substitute for Arial, Albany, Helvetica, Nimbus Sans L, and
Bitstream Vera Sans);
@item Serif (a substitute for Times New Roman, Thorndale, Nimbus Roman, and
Bitstream Vera Serif);
@item Mono (a substitute for Courier New, Cumberland, Courier, Nimbus Mono L,
and Bitstream Vera Sans Mono).
@end enumerate\n")
    (license license:silofl1.1)))

(define-public font-linuxlibertine
  (package
    (name "font-linuxlibertine")
    (version "5.3.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "mirror://sourceforge/linuxlibertine/"
                                  "linuxlibertine/" version
                                  "/LinLibertineSRC_" version "_2012_07_02.tgz"))
              (sha256
               (base32
                "0x7cz6hvhpil1rh03rax9zsfzm54bh7r4bbrq8rz673gl9h47v0v"))))
    (build-system font-build-system)
    (outputs '("out" "ttf"))
    (arguments
     `(#:license-file-regexp "^(GPL|LICENCE|OFL-1\\.1)\\.txt$"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'build
           (lambda _
             (let ((compile
                    (lambda (name ext)
                      (invoke
                       "fontforge" "-lang=ff"
                       "-c" (string-append "Open('" name "');"
                                           "Generate('"
                                           (basename name "sfd") ext
                                           "')")))))
               (for-each (lambda (name)
                           (and (compile name "ttf")
                                (compile name "otf")))
                         (find-files "." "\\.sfd$"))
               #t))))))
    (native-inputs
     (list fontforge))
    (home-page "http://www.linuxlibertine.org/")
    (synopsis "Serif and sans serif typefaces")
    (description "The Linux Libertine fonts is a set of typefaces containing
both a Serif version (\"Linux Libertine\") and a Sans Serif (\"Linux
Biolinum\") designed to be used together as an alternative for Times/Times New
Roman and Helvetica/Arial.  The Serif typeface comes in two shapes and two
weights, and with a Small Capitals version of the regular typeface.  Linux
Biolinum is available in both Regular and Bold weights.")
    ;; The fonts are released under either of these licenses.
    (license (list license:gpl2+ license:silofl1.1))))

(define-public font-libertinus
  (package
    (name "font-libertinus")
    (version "7.040")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/alerque/libertinus/releases"
                           "/download/v" version "/libertinus-" version
                           ".zip"))
       (sha256
        (base32 "1xkj993hwkr49q63dd2dnkvdkm9sckxm3zjwhdxsxn21fi80ikic"))))
    (build-system font-build-system)
    (outputs '("out" "woff"))
    (home-page "https://github.com/alerque/libertinus")
    (synopsis "Font family based on Linux Libertine")
    (description
     "The Libertinus font family is a fork of Linux Libertine that addresses
many bugs in the unmaintained original and adds a new mathematical companion
font for use with OpenType math-capable applications like LuaTex or XeTeX.

The unified Libertinus family consists of:
@enumerate
@item Libertinus Serif, forked from Linux Libertine;
@item Libertinus Sans Serif, forked from Linux Biolinum;
@item Libertinus Mono, forked from Linux Libertine Mono; and
@item Libertinus Math, an original matching OpenType math font.
@end enumerate\n")
    (license license:silofl1.1)))

(define-public font-libre-franklin
  (let ((commit "0022627ebb2a582327569ee45af5d0d9ef31dfea")
        (revision "1"))
    (package
      (name "font-libre-franklin")
      (version (git-version "1.502" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/impallari/Libre-Franklin")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kfipv9vfivgn1789b9yc6r0l31r6l02nz06icw99zvmfcjbpzf0"))))
      (build-system font-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            ;; To avoid installing legacy fonts
            (add-before 'install 'chdir
              (lambda _
                (chdir "fonts")))
            (add-after 'install 'chdir-back
              (lambda _
                (chdir ".."))))))
      (home-page "https://fonts.google.com/specimen/Libre+Franklin")
      (synopsis "Font family based on Franklin Gothic")
      (description
       "The Libre Franklin font family is an open source interpretation and
expansion of Franklin Gothic, a classic font.  It covers 105 Latin Languages.")
      (license license:silofl1.1))))

(define-public font-terminus
  (package
    (name "font-terminus")
    (version "4.49.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/terminus-font/terminus-font-"
                           (version-major+minor version)
                           "/terminus-font-" version ".tar.gz"))
       (sha256
        (base32 "0yggffiplk22lgqklfmd2c0rw8gwchynjh5kz4bz8yv2h6vw2qfr"))))
    (build-system gnu-build-system)
    (outputs (list "out" "pcf-8bit" "otb"))
    (arguments
     `(#:tests? #f                      ; no test target in tarball
       #:modules
       ((guix build gnu-build-system)
        (guix build utils)
        (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-more-bits
           ;; X11 8-bit code pages aren't installed by default (they were
           ;; until version 4.46).  Build and install them separately.
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "pcf-8bit" make-flags)))
         (add-after 'install 'install-more-bits
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((pcf-8bit (assoc-ref outputs "pcf-8bit")))
               (apply invoke "make" "install-pcf-8bit" (string-append "prefix="
                                                                      pcf-8bit)
                      make-flags))))
         (add-after 'build-more-bits 'build-otb
           ;; Build Open Type Bitmap
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "otb" make-flags)))
         (add-after 'install 'install-otb
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((otb (assoc-ref outputs "otb")))
               (apply invoke "make" "install-otb" (string-append "prefix=" otb)
                      make-flags))))
         (add-after 'install 'install-documentation
           ;; There's no way to decipher the cryptic file names without this.
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (match-lambda
                         ((name . directory)
                          (install-file "README"
                                        (string-append directory "/share/doc/"
                                                       ,name "-" ,version))))
                       outputs))))))
    (native-inputs
     (list bdftopcf font-util mkfontdir pkg-config python))
    (home-page "https://terminus-font.sourceforge.net/")
    (synopsis "Simple bitmap programming font")
    (description "Terminus Font is a clean, fixed-width bitmap font, designed
for long periods of working with computers (8 or more hours per day).")
    (license license:silofl1.1)))

(define-public font-adobe-source-han-serif
  (package
    (name "font-adobe-source-han-serif")
    (version "2.003")
    (source
     (origin
       ;; SuperOTC (all variations in one file) is not in the repository.
       (method url-fetch)
       (uri (string-append
             "https://github.com/adobe-fonts/source-han-serif/releases/download/"
             version "R/01_SourceHanSerif.ttc.zip"))
       (sha256
        (base32 "0pvrgba8r3khliws3zqhcqqa1gbjqfakx9q4yrdf6jl9aymqkrkf"))))
    (build-system font-build-system)
    (home-page "https://source.typekit.com/source-han-serif/")
    (synopsis "Pan-CJK typeface in OpenType/CFF format")
    (description
     "This package provides Source Han Serif, a Pan-CJK typeface available
in OpenType/CFF format.  It supports four different East Asian languages
— Simplified Chinese, Traditional Chinese, Japanese, and Korean — and the
65,535 glyphs in each of its seven weights are designed to work together.
Also included is a set of Western glyphs supporting the Latin, Greek, and
Cyrillic scripts.")
    (license license:silofl1.1)))

(define-public font-adobe-source-han-sans
  (package
    (name "font-adobe-source-han-sans")
    (version "2.004")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/adobe-fonts/source-han-sans")
                    (commit (string-append version "R"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sgfvdigq9vdmf8wizapy8wcyzqrqj8il9sx1xzfm20qy376qvbf"))))
    (outputs '("out"                   ; OpenType/CFF Collection (OTC), 112 MiB.
               "cn" "hk" "jp" "kr" "tw")) ; Region-specific Subset OpenType/CFF.
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("SubsetOTF/CN" "share/fonts/opentype" #:output "cn")
          ("SubsetOTF/HK" "share/fonts/opentype" #:output "hk")
          ("SubsetOTF/JP" "share/fonts/opentype" #:output "jp")
          ("SubsetOTF/KR" "share/fonts/opentype" #:output "kr")
          ("SubsetOTF/TW" "share/fonts/opentype" #:output "tw"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-otc
            (lambda _
              (let ((destination-directory
                     (string-append #$output "/share/fonts/opentype")))
                (mkdir-p destination-directory)
                (invoke "unzip" "SuperOTC/SourceHanSans.ttc.zip"
                        "-d" destination-directory)))))))
    (native-inputs (list unzip))
    (home-page "https://github.com/adobe-fonts/source-han-sans")
    (synopsis "Pan-CJK fonts")
    (description
     "Source Han Sans is a sans serif Pan-CJK font family that is offered in
seven weights: ExtraLight, Light, Normal, Regular, Medium, Bold, and Heavy.  And
in several OpenType/CFF-based deployment configurations to accommodate various
system requirements or limitations.  As the name suggests, Pan-CJK fonts are
intended to support the characters necessary to render or display text in
Simplified Chinese, Traditional Chinese (Taiwan, Hong Kong), Japanese, and
Korean.")
    (license license:silofl1.1)))

(define-public font-cns11643
  ;; Since upstream doesn't provide any version numbers, the date of the last
  ;; edit is used, taken from https://data.gov.tw/dataset/5961
  ;; XXX: The source is also updated in-place, so it may be desirable to mirror
  ;; it elsewhere to avoid suddenly losing the current source file.
  (package
    (name "font-cns11643")
    (version "98.1.20180605")
    (source (origin
              (method url-fetch)
              (uri "http://www.cns11643.gov.tw/AIDB/Open_Data.zip")
              (sha256
               (base32
                "000a9whrjr1cd4pjc23pbl60zwkq3wcb5g61p9qi7fn3hwkp0kyw"))))
    (build-system font-build-system)
    (home-page "http://www.cns11643.gov.tw/AIDB/welcome.do")
    (synopsis "CJK TrueType fonts, TW-Kai and TW-Sung")
    (description
     "@code{CNS 11643} character set (Chinese National Standard, or Chinese
Standard Interchange Code) is the standard character set of the Republic of
China (Taiwan) for Chinese Characters and other Unicode symbols.  Contained
are six TrueType fonts based on two script styles, Regular script (Kai), and
Sung/Ming script, each with three variants:

@itemize
@item @code{CNS 11643} (@code{TW-Kai} and @code{TW-Sung}): Tens of thousands
of CJK characters from frequency tables published by the Taiwanese
Ministry of Education.  ISO 10646 and Unicode compatible encoding.
@item @code{Big-5 Plus}: Several thousand frequently used CJK characters
encoded in the user defined area of the Big-5 code.
@item @code{Big-5 Extended}: A Big-5 character set based on the
@code{Big-5 Plus} and @code{CNS 11643} character sets.
@end itemize\n")
    (license (license:non-copyleft
              "http://data.gov.tw/license")))) ; CC-BY 4.0 compatible

(define-public font-cns11643-swjz
  (package
    (name "font-cns11643-swjz")
    (version "1")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.moedict.tw/fonts/truetype/cns11643/ebas927.ttf")
       (sha256
        (base32
         "1qkljldbmb53zp1rcmpsb8rzy67rnsqcjxi549m9743ifk4isl78"))))
    (build-system font-build-system)
    (home-page
     (string-append "http://www.cns11643.gov.tw/AIDB/download.do"
                    "?name=%E5%AD%97%E5%9E%8B%E4%B8%8B%E8%BC%89"))
    (synopsis "TrueType seal script font")
    (description
     "@code{Shuowen Jiezi} is a TrueType seal script font based on the ancient
text of the same name published by the Executive Yuan of Taiwan.  6721 glyphs
are included, at Unicode compatible code points corresponding to their modern
variants.")
    ;; Original text only available in Chinese. More info at
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26703#11
    (license (license:non-copyleft
              "http://www.cns11643.gov.tw/AIDB/copyright.do"))))

(define-public font-wqy-zenhei
  (package
    (name "font-wqy-zenhei")
    (version "0.9.45")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/wqy/wqy-zenhei/" version
                    "%20%28Fighting-state%20RC1%29/wqy-zenhei-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1mkmxq8g2hjcglb3zajfqj20r4r88l78ymsp2xyl5yav8w3f7dz4"))))
    (build-system font-build-system)
    (arguments
     '(#:license-file-regexp "^(COPYING|README)$"))
    (home-page "http://wenq.org/wqy2/")
    (synopsis "CJK font")
    (description
     "WenQuanYi Zen Hei is a Hei-Ti style (sans-serif type) Chinese outline
font.  It is designed for general purpose text formatting and on-screen
display of Chinese characters and symbols from many other languages.
WenQuanYi Zen Hei provides a rather complete coverage of Chinese Hanzi glyphs,
including both simplified and traditional forms.  The total glyph number in
this font is over 35,000, including over 21,000 Chinese Hanzi.  This font has
full coverage of the GBK (CP936) charset, CJK Unified Ideographs, as well as
the code-points needed for zh_cn, zh_sg, zh_tw, zh_hk, zh_mo, ja (Japanese) and
ko (Korean) locales for @code{fontconfig}.")
    ;; GPLv2 with font embedding exception.
    (license license:gpl2)))

(define-public font-wqy-microhei
  (package
    (name "font-wqy-microhei")
    (version "0.2.0-beta")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/wqy/wqy-microhei/"
                                  version "/wqy-microhei-" version ".tar.gz"))
              (sha256
               (base32
                "0gi1yxqph8xx869ichpzzxvx6y50wda5hi77lrpacdma4f0aq0i8"))))
    (build-system font-build-system)
    (arguments
     '(#:license-file-regexp "^(LICENSE.*|README)\\.txt$"))
    (home-page "http://wenq.org/wqy2/")
    (synopsis "CJK font")
    (description
     "WenQuanYi Micro Hei is a Sans-Serif style (also known as Hei, Gothic or
Dotum among the Chinese/Japanese/Korean users) high quality CJK outline font.
It was derived from \"Droid Sans Fallback\" and \"Droid Sans\" released by
Google Inc.  This font contains all the unified CJK Han glyphs in the range of
U+4E00-U+9FC3 defined in Unicode Standard 5.1, together with many other
languages unicode blocks, including Latins, Extended Latins, Hanguls and
Kanas.  The font file is extremely compact (~4M) compared with most known CJK
fonts.")
    ;; This font is licensed under Apache2.0 or GPLv3 with font embedding
    ;; exceptions.
    (license license:gpl3)))

(define-public font-rachana
  (package
    (name "font-rachana")
    (version "7.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/smc/fonts/rachana")
             (commit (string-append "Version" version))))
       (sha256
        (base32 "0r100pvk56y1s38nbv24d78s8nd7dkblgasbn8s887dzj6dps23d"))
       (file-name (git-file-name name version))))
    (build-system font-build-system)
    (home-page "https://smc.org.in")
    (synopsis "Malayalam font")
    (description
     "Rachana is a Malayalam font designed by Hussain K H.  The project was
part of Rachana Aksharavedi for the original script of Malayalam in computing.
Rachana has about 1,200+ glyphs for Malayalam and contains glyphs required for
printing old Malayalam books without compromising the writing style.")
    ;; This font is licensed under SIL 1.1 or GPLv3+ with font embedding
    ;; exceptions.
    (license (list license:silofl1.1 license:gpl3+))))

(define-public font-tex-gyre
  (package
    (name "font-tex-gyre")
    (version "2.005")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "http://www.gust.org.pl/projects/e-foundry/"
                           "tex-gyre/whole/tg-" version "otf.zip"))
       (sha256
        (base32
         "0kph9l3g7jb2bpmxdbdg5zl56wacmnvdvsdn7is1gc750sqvsn31"))))
    (build-system font-build-system)
    (home-page "https://www.gust.org.pl/projects/e-foundry/tex-gyre/")
    (synopsis "Remake of Ghostscript fonts")
    (description "The TeX Gyre collection of fonts is the result of an
extensive remake and extension of the freely available base PostScript fonts
distributed with Ghostscript version 4.00.  The collection contains the
following fonts in the OpenType format: Adventor, Bonum, Chorus, Cursor,
Heros, Pagella, Schola, Termes.")
    (license license:gfl1.0)))

(define-public font-latin-modern
  (package
    (name "font-latin-modern")
    (version "2.004")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://www.gust.org.pl/projects/e-foundry/"
                           "latin-modern/download/lm" version "otf.zip"))
       (sha256
        (base32 "06qnvd6kh07gy2197vx3nmskhiqhp7ip9cpi2rkbwa1p3l2kc0jv"))))
    (build-system font-build-system)
    (home-page "https://www.gust.org.pl/projects/e-foundry/latin-modern")
    (synopsis "OpenType fonts based on Computer Modern")
    (description "The Latin Modern fonts are a set of scalable fonts based on
the PostScript Type 1 version of the Computer Modern fonts and contain many
additional characters (mostly accented ones).  This package provides the
OpenType variant of these fonts.")
    (license license:gfl1.0)))

(define-public font-amiri
  (package
    (name "font-amiri")
    (version "1.000")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aliftype/amiri")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c4yg1b03aihdqvz6w5ak8wapni3l8p18mw6bkqhblmm75jb5kif"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules `(,@%default-gnu-imported-modules
                           (guix build font-build-system))
      #:modules `(,@%default-gnu-modules
                  ((guix build font-build-system) #:prefix font:))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-source
                     (lambda _
                       (substitute* "Makefile"
                         (("^TAG=.*") (string-append "TAG=" #$version "\n")))))
                   (delete 'configure)
                   (replace 'install
                     (assoc-ref font:%standard-phases 'install)))))
    (native-inputs
     (list python-fonttools
           python-glyphsets
           python-pcpp
           python-opentype-sanitizer
           python-sfdlib
           python-ufolib2
           python-ufo2ft
           python-wrapper))
    (home-page "https://www.amirifont.org/")
    (synopsis "Body text Naskh typeface")
    (description "Amiri (أميري) is a classical Arabic typeface in Naskh style
for typesetting books and other running text.  Amiri is a revival of the
typeface pioneered in early 20th century by Bulaq Press in Cairo, also known
as Amiria Press, after which the font is named.  The uniqueness of this
typeface comes from its balance between the beauty of Naskh calligraphy on one
hand and the constraints and requirements of elegant typography on the
other.")
    (license license:silofl1.1)))

(define-public font-anonymous-pro
  (package
    (name "font-anonymous-pro")
    (version "1.002")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.marksimonson.com/assets/content/fonts/"
                    "AnonymousPro-" version ".zip"))
              (sha256
               (base32
                "1asj6lykvxh46czbal7ymy2k861zlcdqpz8x3s5bbpqwlm3mhrl6"))))
    (build-system font-build-system)
    (home-page "https://www.marksimonson.com/fonts/view/anonymous-pro")
    (synopsis "Fixed-width fonts designed with coding in mind")
    (description "Anonymous Pro is a family of four fixed-width fonts designed
with coding in mind.  Anonymous Pro features an international, Unicode-based
character set, with support for most Western and Central European Latin-based
languages, plus Greek and Cyrillic.")
    (license license:silofl1.1)))

(define-public font-anonymous-pro-minus
  (package
    (inherit font-anonymous-pro)
    (name "font-anonymous-pro-minus")
    ;; The -Minus variant doesn't necessarily track the regular version above.
    (version "1.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.marksimonson.com/assets/content/fonts/"
                           "AnonymousProMinus-" version ".zip"))
       (sha256
        (base32 "1p2n91jja37d2cshp5pjwld9lq0v7gnpk7ywwn2blq7k46q6vq38"))))
    (synopsis "Fixed-width fonts designed with coding in mind, without bitmaps")
    (description "Anonymous Pro is a family of four fixed-width fonts designed
with coding in mind.  Anonymous Pro features an international, Unicode-based
character set, with support for most Western and Central European Latin-based
languages, plus Greek and Cyrillic.

Anonymous Pro Minus is identical to Anonymous Pro, minus its embedded bitmaps
for use at smaller text sizes")))

(define-public font-gnu-unifont
  (package
    (name "font-gnu-unifont")
    (version "16.0.02")
    (source
     (origin
       (method url-fetch)
       (uri (list
             (string-append "https://unifoundry.com/pub/unifont/unifont-"
                            version "/unifont-" version ".tar.gz")
             (string-append "mirror://gnu/unifont/unifont-"
                            version "/unifont-" version ".tar.gz")))
       (sha256
        (base32 "0bd5mf3j7f0wggh9ss3a6rji62qwcfak37q6zb8lq9pjcf3yqa7i"))
       (snippet
        '(begin
           (use-modules (guix build utils))
           (delete-file-recursively "font/precompiled")))))
    (build-system gnu-build-system)
    (outputs '("out"   ; TrueType/OpenType version
               "pcf"   ; PCF (bitmap) version
               "psf"   ; PSF (console) version
               "bin")) ; Utilities to manipulate '.hex' format
    (arguments
     `(#:tests? #f          ; no check target
       #:parallel-build? #f ; Race condition in the font Makefile
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             "BUILDFONT=TRUE")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* (find-files "." "Makefile")
               (("/bin/sh\\b") (which "sh")))
             ;; Skip thumbnail generation as it requires ImageMagick and the generated
             ;; thumbnails will not be installed.
             (substitute* "font/Makefile"
               (("^(compiled-files:.+)thumbnails(.+)" _ pre post)
                (string-append pre post)))))
         (replace 'install
          (lambda* (#:key make-flags outputs #:allow-other-keys)
            (let* ((ttf (string-append (assoc-ref outputs "out")
                                       "/share/fonts/truetype"))
                   (otf (string-append (assoc-ref outputs "out")
                                       "/share/fonts/opentype"))
                   (pcf (string-append (assoc-ref outputs "pcf")
                                       "/share/fonts/misc"))
                   (psf (string-append (assoc-ref outputs "psf")
                                       "/share/consolefonts"))
                   (bin (assoc-ref outputs "bin")))
              (apply invoke "make" "install"
                     (string-append "PREFIX=" bin)
                     (string-append "TTFDEST=" ttf)
                     (string-append "OTFDEST=" otf)
                     (string-append "PCFDEST=" pcf)
                     (string-append "CONSOLEDEST=" psf)
                     make-flags)))))))
    (native-inputs
     (list bdftopcf console-setup fontforge))
    (inputs
     (list perl perl-gd))       ; for utilities
    (synopsis
     "Large bitmap font covering Unicode's Basic Multilingual Plane")
    (description
     "GNU Unifont is a bitmap font covering essentially all of
Unicode's Basic Multilingual Plane.  The package also includes
utilities to ease adding new glyphs to the font.")
    (home-page "http://unifoundry.com/unifont/index.html")
    (properties '((upstream-name . "unifont")))
    (license license:gpl2+)))

(define-public font-google-noto
  (package
    (name "font-google-noto")
    (version "2025.04.01")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/notofonts/notofonts.github.io")
             (commit (string-append "noto-monthly-release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mfxikyzk6d7av02bnzykliggasy23lnz6z07bfr3mlv2plwy7f5"))))
    (build-system font-build-system)
    (arguments
     (list
      #:modules
      '((guix build font-build-system)
        (guix build utils)
        (ice-9 ftw))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              (define* (install source #:optional (output #$output))
                (let ((%install (assoc-ref %standard-phases 'install)))
                  (with-directory-excursion source
                    (%install #:outputs `(("out" . ,output))))))

              (define (scan-directory name)
                (scandir name (lambda (file)
                                (not (member file '("." ".." "LICENSE"))))))

              (define (install-font-variant variant)
                "Given font variant VARIANT, install one of its formats,
variable TTF or OTF or TTF."
                (with-directory-excursion variant
                  (let ((formats (scan-directory ".")))
                    (cond
                     ((member "variable-ttf" formats)
                      (install "variable-ttf"))
                     ((member "otf" formats)
                      (install "otf"))
                     ((member "ttf" formats)
                      (install "ttf"))))))

              (define (install-font font)
                "Given FONT, install one of its variants, either full or
unhinted, and install its hinted variant into 'ttf' output.  According to the
source, unhinted and hinted variants are always available."
                (with-directory-excursion font
                  (if (member "full" (scan-directory "."))
                      (install-font-variant "full")
                      (install-font-variant "unhinted"))
                  (install "hinted" #$output:ttf)))

              (with-directory-excursion "fonts"
                (for-each install-font (scan-directory "."))))))))
    (outputs '("out" "ttf"))
    (home-page "https://fonts.google.com/noto")
    (synopsis "Fonts to cover all languages")
    (description "Google Noto Fonts is a family of fonts designed to support
all languages with a consistent look and aesthetic.  Its goal is to properly
display all Unicode symbols.")
    (license license:silofl1.1)))

(define-public font-google-noto-emoji
  (package
    (name "font-google-noto-emoji")
    (version "2.047")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googlefonts/noto-emoji")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0p9wa55fyxih0bm81h55ip9rc12rh1c2v0dq9dxd2y19rxgcnnxz"))))
    (build-system font-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-font-directory
            (lambda _
              ;; Note this ensures the correct license file is installed.
              (chdir "fonts")))
          (replace 'install
            (lambda _
              (let ((dir (string-append #$output "/share/fonts/truetype")))
                (install-file "NotoColorEmoji.ttf" dir)))))))
    (home-page "https://fonts.google.com/noto/specimen/Noto+Color+Emoji")
    (synopsis "Font for rendering color emoji characters")
    (description
     "This package provides the color emoji font from the Google Noto font
family.")
    (license license:silofl1.1)))

(define-public font-google-noto-sans-cjk
  (package
    (name "font-google-noto-sans-cjk")
    (version "2.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/googlefonts/noto-cjk/releases/download/Sans"
             version "/01_NotoSansCJK-OTF-VF.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "1ka37kqyd0sfqwk485nv6ihrdjl5xycr38m4jq40r2lzmpmkmqym"))))
    (build-system font-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (chdir "..")         ;For license.
                   (let ((install (assoc-ref %standard-phases 'install)))
                     (with-directory-excursion "Variable/OTC"
                       (install #:outputs `(("out" . ,#$output))))
                     (with-directory-excursion "Variable/OTF"
                       (install #:outputs `(("out" . ,#$output:otf))))))))))
    (outputs '("out" "otf"))
    (home-page "https://www.google.com/get/noto/")
    (synopsis "Fonts to cover all languages")
    (description "Google Noto Fonts is a family of fonts designed to support
all languages with a consistent look and aesthetic.  Its goal is to properly
display all Unicode symbols.  This package provides the Sans Serif variant of
CJK fonts.")
    (license license:silofl1.1)))

(define-public font-google-noto-serif-cjk
  (package
    (name "font-google-noto-serif-cjk")
    (version "2.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/googlefonts/noto-cjk/releases/download/Serif"
             version "/02_NotoSerifCJK-OTF-VF.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "17jjsacnv5lmnidhha9xs1kz6gspbijda01c5y50vk2n86swz63q"))))
    (build-system font-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (chdir "..")         ;For license.
                   (let ((install (assoc-ref %standard-phases 'install)))
                     (with-directory-excursion "Variable/OTC"
                       (install #:outputs `(("out" . ,#$output))))
                     (with-directory-excursion "Variable/OTF"
                       (install #:outputs `(("out" . ,#$output:otf))))))))))
    (outputs '("out" "otf"))
    (home-page "https://www.google.com/get/noto/")
    (synopsis "Fonts to cover all languages")
    (description "Google Noto Fonts is a family of fonts designed to support
all languages with a consistent look and aesthetic.  Its goal is to properly
display all Unicode symbols.  This package provides the Serif variant of CJK
fonts.")
    (license license:silofl1.1)))

(define-public font-google-roboto
  (package
    (name "font-google-roboto")
    (version "2.136")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/google/roboto/releases/download/"
                           "v" version "/roboto-hinted.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "0spscx08fad7i8qs7icns96iwcapniq8lwwqqvbf7bamvs8qfln4"))))
    (build-system font-build-system)
    (home-page "https://github.com/google/roboto")
    (synopsis "The Roboto family of fonts")
    (description
     "Roboto is Google’s signature family of fonts, the default font on Android
and Chrome OS, and the recommended font for the
visual language \"Material Design\".")
    (license license:asl2.0)))

(define-public font-borg-sans-mono
  (package
    (name "font-borg-sans-mono")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/charje/borg-sans-mono"
             "/releases/download/v" version "/borg-sans-mono.zip"))
       (sha256
        (base32
         "0xzi866ag9w4q114bn984yjfy72pmfs563v5yy1rkbqycphgwwyp"))))
    (build-system font-build-system)
    (home-page "https://github.com/charje/borg-sans-mono")
    (synopsis "The Borg Sans Mono font")
    (description "Borg Sans Mono is a monospaced font derived from Droid Sans
Mono.  It includes additions commonly found in programming fonts such as a
slashed zero and ligatures for operators.")
    (license license:asl2.0)))

(define-public font-un
  (package
    (name "font-un")
    (version "1.0.2-080608")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://kldp.net/unfonts/release/2607-"
                    "un-fonts-core-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13liaz2pmww3aqabm55la5npd08m1skh334ky7qfidxaz5s742iv"))))
    (build-system font-build-system)
    (home-page "https://kldp.net/projects/unfonts/")
    (synopsis "Collection of Korean fonts")
    (description
     "Un-fonts is a family of mainly Korean fonts.
It contains the following fonts and styles:

@enumerate
@item UnBatang, UnBatangBold: serif;
@item UnDotum, UnDotumBold: sans-serif;
@item UnGraphic, UnGraphicBold: sans-serif style;
@item UnDinaru, UnDinaruBold, UnDinaruLight;
@item UnPilgi, UnPilgiBold: script;
@item UnGungseo: cursive, brush-stroke.
@end enumerate\n")
    (license license:gpl2+)))

(define-public font-unscii
  (package
    (name "font-unscii")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://viznut.fi/unscii/unscii-"
                    version "-src.tar.gz"))
              (sha256
               (base32
                "0msvqrq7x36p76a2n5bzkadh95z954ayqa08wxd017g4jpa1a4jd"))))
    (build-system gnu-build-system)
    (outputs '("out" "otf" "ttf" "woff"))
    (native-inputs (list bdftopcf fontforge perl perl-text-charwidth))
    (inputs (list sdl sdl-image))
    (arguments
     (list #:tests? #f                  ;no tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ;no configure script
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((pcfdir (string-append
                                  (assoc-ref outputs "out")
                                  "/share/fonts/misc"))
                         (otfdir (string-append
                                  (assoc-ref outputs "otf")
                                  "/share/fonts/opentype"))
                         (ttfdir (string-append
                                  (assoc-ref outputs "ttf")
                                  "/share/fonts/truetype"))
                         (woffdir (string-append
                                   (assoc-ref outputs "woff")
                                   "/share/fonts/webfonts"))
                         (install-fonts
                          (lambda (pred dir)
                            (for-each
                             (lambda (f) (install-file f dir))
                             (find-files "." pred)))))
                     (install-fonts "\\.pcf$" pcfdir)
                     (install-fonts "\\.otf$" otfdir)
                     (install-fonts "\\.ttf$" ttfdir)
                     (install-fonts "\\.woff$" woffdir)))))))
    (synopsis "Classic bitmapped Unicode fonts")
    (description
     "Unscii is a set of bitmapped Unicode fonts based on classic system
fonts.  Unscii attempts to support character cell art well while also being
suitable for terminal and programming use.  The two main variants are
unscii-8 (8×8 pixels per glyph) and unscii-16 (8×16).")
    (home-page "http://viznut.fi/unscii/")
    ;; "unscii-16-full" falls under GPL, the other are in the Public Domain.
    (license (list license:gpl2+ license:public-domain))))

(define-public font-fantasque-sans
  (package
    (name "font-fantasque-sans")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/belluzj/fantasque-sans")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17l18488qyl9gdj80r8pcym3gp3jkgsdikwalnrp5rgvwidqx507"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ttfautohint" ,ttfautohint)
       ("woff-tools" ,woff-tools)
       ("fontforge" ,fontforge)
       ("woff2" ,woff2)
       ("woff2:bin" ,woff2 "bin")
       ("zip" ,zip)))
    (arguments
     `(#:tests? #f                 ;test target intended for visual inspection
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configuration
                  (add-before 'build 'support-python@3
                    ;; Rather than use a Python 2 fontforge, replace Python-2-
                    ;; specific code with a passable Python 3 equivalent.
                    (lambda _
                      (substitute* "Scripts/fontbuilder.py"
                        (("xrange") "range"))
                      (substitute* "Scripts/features.py"
                        (("f\\.write\\(fea_code\\)")
                         "f.write(str.encode(fea_code))"))
                      #t))
                  (replace 'install
                    ;; 'make install' wants to install to ~/.fonts, install to
                    ;; output instead.  Install only the "Normal" variant.
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (font-dir (string-append out "/share/fonts"))
                             (truetype-dir (string-append font-dir "/truetype"))
                             (opentype-dir (string-append font-dir "/opentype"))
                             (webfonts-dir (string-append font-dir "/webfonts")))
                        (with-directory-excursion "Variants/Normal"
                          (copy-recursively "OTF" opentype-dir)
                          (for-each (lambda (f) (install-file f truetype-dir))
                                    (find-files "." "\\.ttf$"))
                          (copy-recursively "Webfonts" webfonts-dir)
                          #t)))))))
    (synopsis "Font family with a monospaced variant for programmers")
    (description
     "Fantasque Sans Mono is a programming font designed with functionality in
mind.  The font includes a bold version and a good italic version with new
glyph designs, not just an added slant.")
    (home-page "https://fontlibrary.org/en/font/fantasque-sans-mono")
    (license license:silofl1.1)))

(define-public font-hack
  (package
    (name "font-hack")
    (version "3.003")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://github.com/source-foundry/Hack/releases/download/v"
                    version "/Hack-v" version "-ttf.zip"))
              (sha256
               (base32
                "1b4hh8zkrx92m2v2vfkja1napb0192p0j3laqr0m018z3dih89hc"))))
    (build-system font-build-system)
    (home-page "https://sourcefoundry.org/hack/")
    (synopsis "Typeface designed for source code")
    (description
     "Hack is designed to be a workhorse typeface for code.  It expands upon
the Bitstream Vera & DejaVu projects, provides over 1,500 glyphs, and includes
Powerline support.")
    (license
     ;; See https://github.com/source-foundry/Hack/issues/271 for details.
     (list license:expat                ; the Hack modifications to...
           license:public-domain        ; ...the DejaVu modifications to...
           (license:x11-style           ; ...the Bitstream Vera typeface
            "file://LICENSE.md" "Bitstream Vera License")))))

(define-public font-adobe-source-code-pro
  (package
    (name "font-adobe-source-code-pro")
    (version "2.042R-u-1.062R-i-1.026R-vf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adobe-fonts/source-code-pro")
             (commit (regexp-substitute/global
                      ;; The upstream tag uses "/" among its upright, italic and
                      ;; variable versions, so substitute our "-" separator
                      ;; here.
                      #f "((R-u)|(R-i))(-)" version
                      'pre 1 "/" 'post))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mir6yi5i0h1kk6fr8dsqmv4m8cwrbldadpy7rnlyvkd26wdqpiy"))))
    (build-system font-build-system)
    (home-page "https://github.com/adobe-fonts/source-code-pro")
    (synopsis
     "Monospaced font family for user interface and coding environments")
    (description
     "Source Code Pro is a set of monospaced OpenType fonts that have been
designed to work well in user interface environments.")
    (license license:silofl1.1)))

(define-public font-adobe-source-sans
  (package
    (name "font-adobe-source-sans")
    (version "3.052")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adobe-fonts/source-sans")
             (commit (string-append version "R"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06s0xn49x454c9zbrawcm4b672qpaqah3glmh2jn3m2jyv5xhdnb"))))
    (build-system font-build-system)
    (outputs '("out" "ttf" "woff"))
    (home-page "https://github.com/adobe-fonts/source-sans")
    (synopsis
     "Sans serif font family for user interface environments")
    (description
     "Source Sans is a set of OpenType fonts that have been designed to work
well in user interface (UI) environments.")
    (license license:silofl1.1)))

;; https://github.com/adobe-fonts/source-sans/issues/192
(define-public font-adobe-source-sans-pro
  (deprecated-package "font-adobe-source-sans-pro"  font-adobe-source-sans))

(define-public font-adobe-source-serif
  (package
    (name "font-adobe-source-serif")
    (version "4.005")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adobe-fonts/source-serif")
             (commit (string-append version "R"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0khb24wcpwa3lqbpaklcph3xx5b4ksadhfgb3vj2mahacwjr2dvn"))))
    (build-system font-build-system)
    (outputs '("out" "ttf" "woff"))
    (home-page "https://github.com/adobe-fonts/source-serif")
    (synopsis
     "Serif typeface to complement Source Sans for setting text")
    (description
     "Source Serif is a set of OpenType fonts to complement the Source Sans
family.")
    (license license:silofl1.1)))

;; https://github.com/adobe-fonts/source-serif/issues/77
(define-public font-adobe-source-serif-pro
  (deprecated-package "font-adobe-source-serif-pro" font-adobe-source-serif))

(define-public font-microsoft-cascadia
  (package
    (name "font-microsoft-cascadia")
    (version "2407.24")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://github.com/microsoft/cascadia-code/"
                    "releases/download/v"
                    version
                    "/CascadiaCode-"
                    version
                    ".zip"))
              (sha256
               (base32
                "0x441jjkswz6vidg6kdv9rmbqlm7dqcvsm4higs67nw66gp6hyp6"))))
    (build-system font-build-system)
    (home-page "https://github.com/microsoft/cascadia-code")
    (synopsis "Monospaced font with programming ligatures")
    (description
     "Cascadia is a fun new coding font that comes bundled with Windows
Terminal, and is now the default font in Visual Studio as well.")
    (license license:silofl1.1)))

(define-public font-fira-sans
  ;; Fira Sans v4.203 (which corresponds to Fira Mono v3.206) is the final
  ;; version to include UFO sources. It is the same version packaged by other
  ;; notable distributors, including Google Fonts. Note that the "reserved
  ;; font name" was removed by the copyright holders.
  ;;
  ;; The upstream release includes a "Fira Code" which "is Fira Mono 3.206
  ;; with less Line Space (1.0) – does not include programming ligatures". We
  ;; do not package that: our 'font-fira-code' package (like e.g. Debian's
  ;; "fonts-firacode") is the much better known Fira Code font by Nikita
  ;; Prokopov, which is an older, independent adaptation of Fira Mono. For the
  ;; historical relationship between them, see:
  ;; https://github.com/mozilla/Fira/issues/218
  ;;
  ;; For a lengthy discussion of the available sources and versions,
  ;; see: https://github.com/LiberalArtist/FiraSans/
  ;;
  ;; See also:
  ;;   - https://github.com/mozilla/Fira/pull/219
  ;;   - https://github.com/bBoxType/FiraSans/issues/4#issuecomment-695833327
  (package
    (name "font-fira-sans")
    (version "4.203")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/bBoxType/FiraSans")
                     (commit "a606927401bcc3951587339fee53aa882856b51b")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r9kb7v9jg83nnxwkl6gx9ix1rng3ksr7v33qrm46qb4fhwsyc2n"))))
    (build-system font-build-system)
    (arguments
     `(#:modules
       ((ice-9 match)
        (ice-9 regex)
        (guix build utils)
        (guix build font-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda* (#:key outputs #:allow-other-keys)
             (define-values (pkg-name _version)
               (package-name->name+version
                (strip-store-file-name (assoc-ref outputs "out"))))
             (define variant
               (string-capitalize
                (match:substring (string-match "fira-([a-z]+)" pkg-name) 1)))
             (match (find-files "." (format #f "^Fira_~a_[0-9]" variant)
                                #:directories? #t)
               ((dir)
                (chdir dir)))))
         (add-before 'install-license-files 'enter-license-directory
           (lambda _
             (chdir "../OFL_Licence"))))))
    ;; While the repository has moved,
    ;; this specimen still works well as the home-page:
    (home-page "https://mozilla.github.io/Fira/")
    (synopsis
     "Humanist sans-serif with numerous weights emphasizing legibility")
    (description "Fira Sans is a humanist sans-serif typeface with an emphasis
on legibility, commissioned by Mozilla from Erik Spiekermann and Ralph du
Carrois.  The large family includes 2,709 glyphs in normal, condensed, and
compressed cuts at 11 weights (plus 6 experimental weights), each with
corresponding italics.

The package @code{font-fira-mono} provides a corresponding monospace cut.")
    (license license:silofl1.1)))

(define-public font-fira-mono
  (package
    (inherit font-fira-sans)
    (name "font-fira-mono")
    (version "3.206")
    (synopsis "Monospace cut of Fira Sans")
    (description
     "Fira Mono is a monospace cut of Fira Sans (see @code{font-fira-sans}).
It includes regular, medium, and bold weights.")
    (license license:silofl1.1)))

(define-public font-fira-go
  (package
    (name "font-fira-go")
    (version "1.000")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/bBoxType/FiraGO")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10rcfg1fijv00yxv5n9l3lm0axhafa1irkg42zpmasd70flgg655"))))
    (build-system font-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install-license-files 'enter-license-directory
            (lambda _
              (chdir "OFL_Licence"))))))
    (home-page "https://github.com/bBoxType/FiraGO")
    (synopsis "Multilingual extension of the Fira Sans font family")
    (description "FiraGO is a multilingual extension of the Fira Sans font
family.  Based on the Fira Sans 4.3 glyph set, FiraGO adds support for the
Arabic, Devanagari, Georgian, Hebrew and Thai scripts.

Note that FiraGO does not include corresponding source.")
    ;; See:
    ;;   - https://github.com/bBoxType/FiraGO/issues/42
    ;;   - https://github.com/bBoxType/FiraSans/issues/4#issuecomment-699882058
    ;; For further discussion, see comments on font-fira-sans.
    (license license:silofl1.1)))

(define-public font-fira-code
  (package
    (name "font-fira-code")
    (version "6.2")
    (source
     (origin
       ;; changing to git-fetch would require building from source
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/tonsky/FiraCode/releases/"
                           "download/" version
                           "/Fira_Code_v" version ".zip"))
       (sha256
        (base32 "0y9y7snyrr30z75kxz2zgh6q6hizcbzsf41xv6gxh97bm1dr2j89"))))
    (build-system font-build-system)
    ;; This font began as an independent derived work of Fira Mono.
    ;; It was never affiliated with Mozilla.
    ;; See comments on font-fira-sans for further discussion.
    (home-page "https://github.com/tonsky/FiraCode")
    (synopsis "Monospaced font with programming ligatures")
    (description
     "Fira Code is a monospace font by Nikita Prokopov featuring ligatures for
common programming multi-character combinations.  It began as an extension of
Fira Mono.  The ligatures are just a font rendering feature: underlying code
remains ASCII-compatible.  They are designed to help people to read and
understand code faster.  For some frequent sequences like @code{..} or
@code{//}, ligatures are used to simulate proportional spacing.")
    (license license:silofl1.1)))

(define-public font-awesome
  (package
   (name "font-awesome")
   ;; XXX The build scripts of version 5 are not freely licensed and
   ;; so we have to stick with version 4 for now:
   ;; <https://bugs.gnu.org/32916>
   (version "4.7.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                   (url "https://github.com/FortAwesome/Font-Awesome")
                   (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0w30y26jp8nvxa3iiw7ayl6rkza1rz62msl9xw3srvxya1c77grc"))))
   (build-system font-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (source (string-append (getcwd) "/fonts"))
                   (fonts (string-append out "/share/fonts")))
              (for-each (lambda (file)
                          (install-file file (string-append fonts "/truetype")))
                        (find-files source "\\.(ttf|ttc)$"))
              (for-each (lambda (file)
                          (install-file file (string-append fonts "/opentype")))
                        (find-files source "\\.(otf|otc)$"))
              #t))))))
   (home-page "https://fontawesome.com/")
   (synopsis "Font that contains a rich iconset")
   (description
    "Font Awesome is a full suite of pictographic icons for easy scalable
vector graphics.")
   (license license:silofl1.1)))

(define-public font-tamzen
  (package
    (name "font-tamzen")
    (version "1.11.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sunaku/tamzen-font")
              (commit (string-append "Tamzen-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00x5fipzqimglvshhqwycdhaqslbvn3rl06jnswhyxfvz16ymj7s"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let* ((out      (assoc-ref %outputs "out"))
                (font-dir (string-append out "/share/fonts/misc"))
                (psf-dir  (string-append out "/share/kbd/consolefonts")))
           (chdir (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (mkdir-p psf-dir)
           (for-each (lambda (pcf)
                       (install-file pcf font-dir))
                     (find-files "pcf" "\\.pcf$"))
           (for-each (lambda (psf)
                       (install-file psf psf-dir))
                     (find-files "psf" "\\.psf$"))
           #t))))
    (home-page "https://github.com/sunaku/tamzen-font")
    (synopsis "Monospaced bitmap font for console and X11")
    (description
     "Tamzen is a fork of the @code{Tamsyn} font.  It is programmatically forked
from @code{Tamsyn} version 1.11, backporting glyphs from older versions while
deleting deliberately empty glyphs (which are marked as unimplemented) to
allow secondary/fallback fonts to provide real glyphs at those codepoints.

The @code{TamzenForPowerline} fonts provide additional @code{Powerline} symbols,
which are programmatically injected with @code{bitmap-font-patcher} and
later hand-tweaked with the gbdfed(1) editor:

@enumerate
@item all icons are expanded to occupy the maximum available space
@item the branch of the fork icon ( U+E0A0) was made larger than the trunk
@item for the newline icon ( U+E0A1), the @emph{N} was made larger at the bottom
@item the keyhole in the padlock icon ( U+E0A2) was replaced with @emph{//} lines.
@end enumerate\n")
    (license (license:non-copyleft "file://LICENSE"))))

(define-public font-comic-neue
  (package
    (name "font-comic-neue")
    (version "2.51")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "http://www.comicneue.com/comic-neue-" version ".zip"))
              (sha256
               (base32
                "0883542v915crz98v1ij6smgy40dg6gxwsid3j5nbmmqjf69kpal"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Delete Mac OS X specific files. If not deleted, these cause
         ;; several hidden files to be installed.
         (add-before 'install 'delete-macosx-files
           (lambda _
             (delete-file-recursively "__MACOSX")))
         (add-after 'install 'install-conf
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((conf-dir (string-append (assoc-ref outputs "out")
                                            "/share/fontconfig/conf.avail")))
               (mkdir-p conf-dir)
               (call-with-output-file
                   (string-append conf-dir "/30-comic-neue.conf")
                 (lambda (port)
                   (format port "<?xml version=\"1.0\"?>
<!DOCTYPE fontconfig SYSTEM \"fonts.dtd\">
<fontconfig>
  <!-- If Comic Sans is missing, use Comic Neue instead. -->
  <alias>
    <family>Comic Sans MS</family>
    <prefer>
      <family>Comic Neue</family>
    </prefer>
  </alias>
</fontconfig>\n"))))))
         (add-before 'install-license-files 'enter-license-directory
           (lambda _
             (chdir (string-append "comic-neue-" ,version)))))))
    (home-page "https://www.comicneue.com/")
    (synopsis "Font that fixes the shortcomings of Comic Sans")
    (description
     "Comic Neue is a font that attempts to create a respectable casual
typeface, by mimicking Comic Sans while fixing its most obvious shortcomings.")
    (license license:silofl1.1)))

;; When updating the version (and hash) of font-iosevka, also update the hash
;; of the Iosevka variants further below.
;; The following script downloads all Iosevka variants to the store and prints
;; their hash at the end.
#|
guix repl <<EOF
(use-modules (guix base32)
             (guix download)
             (guix packages)
             (guix store)
             (gcrypt hash)
             (ice-9 string-fun)
             (gnu packages fonts))

(let ((new-version "20.0.0")
      (iosevka-hashes #nil)
      (iosevka-fails #nil))
  (for-each (lambda (font)
              (let ((file (download-to-store (open-connection)
                                             (string-replace-substring
                                              (origin-uri (package-source font))
                                              (package-version font)
                                              new-version))))
                (if file
                    (set! iosevka-hashes
                          (acons file (bytevector->nix-base32-string
                                       (file-sha256 file))
                                 iosevka-hashes))
                    (set! iosevka-fails (cons font iosevka-fails)))))
            (list font-iosevka
                  font-iosevka-slab
                  font-iosevka-term
                  font-iosevka-term-slab
                  font-iosevka-aile
                  font-iosevka-curly
                  font-iosevka-curly-slab
                  font-iosevka-etoile
                  font-iosevka-ss01
                  font-iosevka-ss02
                  font-iosevka-ss03
                  font-iosevka-ss04
                  font-iosevka-ss05
                  font-iosevka-ss06
                  font-iosevka-ss07
                  font-iosevka-ss08
                  font-iosevka-ss09
                  font-iosevka-ss10
                  font-iosevka-ss11
                  font-iosevka-ss12
                  font-iosevka-ss13
                  font-iosevka-ss14
                  font-iosevka-ss15
                  font-iosevka-ss16
                  font-iosevka-ss17
                  font-iosevka-ss18))
  (for-each (lambda (hash)
              (format #t "~a: ~a~%" (car hash) (cdr hash)))
            (reverse iosevka-hashes))
  (for-each (lambda (fail)
              (format #t "~a: failed to download latest version~%" fail))
            (reverse iosevka-fails)))
EOF
|#
(define-public font-iosevka
  (package
    (name "font-iosevka")
    (version "32.5.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-Iosevka-" version ".zip"))
       (sha256
        (base32 "1w7043q2pxllry2njrk4cpqwvralb0a1d3bxac36y0ai9al3m5w0"))))
    (build-system font-build-system)
    (home-page "https://be5invis.github.io/Iosevka/")
    (synopsis "Coders' typeface, built from code")
    (description
     "Iosevka is a slender monospace sans-serif or slab-serif typeface inspired
by Pragmata Pro, M+, and PF DIN Mono, designed to be the ideal font for
programming.  Iosevka is completely generated from its source code.")
    (license (list license:silofl1.1    ;build artifacts (i.e., the fonts)
                   license:bsd-3))))    ;supporting code

(define-public font-iosevka-slab
  (package
    (inherit font-iosevka)
    (name "font-iosevka-slab")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSlab-" version ".zip"))
       (sha256
        (base32 "03r7ldsk3qrvy46ag08fhs3ppxmzycn8gh6xxmi3pw972hn9kgpi"))))))

(define-public font-iosevka-term
  (package
    (inherit font-iosevka)
    (name "font-iosevka-term")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTF-IosevkaTerm-" version ".zip"))
       (sha256
        (base32 "13g3czmd36k261ip9l8r94hcq6swbvag80zb9g7i9frjsr97xn2h"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".*"))
             #t)))))))

(define-public font-iosevka-term-slab
  (package
    (inherit font-iosevka)
    (name "font-iosevka-term-slab")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka/"
                           "releases/download/v" version
                           "/PkgTTF-IosevkaTermSlab-" version ".zip"))
       (sha256
        (base32 "00dwp06za3i8kyqqk5xqsd9div2ydf0rgdnj3vl5g6g5621asd39"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".*"))
             #t)))))))

(define-public font-iosevka-aile
  (package
    (inherit font-iosevka)
    (name "font-iosevka-aile")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaAile-" version ".zip"))
       (sha256
        (base32 "10sf5q4f3l9z4pai46sszkswnwfnjmyqzs6ark1dlsifxh8c86wb"))))))

(define-public font-iosevka-curly
  (package
    (inherit font-iosevka)
    (name "font-iosevka-curly")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka/"
                           "releases/download/v" version
                           "/PkgTTC-IosevkaCurly-" version ".zip"))
       (sha256
        (base32 "0qg4r8hid50hnjj3gs0xagdymm15z919s0pn6n2dnlmmxvw1c5nv"))))))

(define-public font-iosevka-curly-slab
  (package
    (inherit font-iosevka)
    (name "font-iosevka-curly-slab")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka/"
                           "releases/download/v" version
                           "/PkgTTC-IosevkaCurlySlab-" version ".zip"))
       (sha256
        (base32 "1lkcs3aqxvdlpgbplhpd4i6vza1xyqnim2awak9b310byyf35d0c"))))))

(define-public font-iosevka-etoile
  (package
    (inherit font-iosevka)
    (name "font-iosevka-etoile")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaEtoile-" version ".zip"))
       (sha256
        (base32 "1yh8vlhrcczmsir87f84qr605rf1kp86bvf1c459myc4a6hcqc3z"))))))

(define-public font-iosevka-ss01
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss01")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS01-" version ".zip"))
       (sha256
        (base32 "0h1ln3bisbgwd7cc6c9jcmplzvgca42csd5cfxj1jjs4i2vmx2d6"))))))

(define-public font-iosevka-ss02
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss02")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS02-" version ".zip"))
       (sha256
        (base32 "0z0b14cwhbb85fjp508cb1ihscxcc75asjmq9fw5rfl3kx85w6h8"))))))

(define-public font-iosevka-ss03
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss03")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS03-" version ".zip"))
       (sha256
        (base32 "18gs7h6mb5c9fqmx7ip1a5k3garqbgp640m338h776pcq9mlnndv"))))))

(define-public font-iosevka-ss04
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss04")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS04-" version ".zip"))
       (sha256
        (base32 "1yjywm9r390gnagla6g5yjach0ydng0m0q752k39fb2z5rpjzphp"))))))

(define-public font-iosevka-ss05
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss05")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS05-" version ".zip"))
       (sha256
        (base32 "1jn8xb7f3hxs4yl54hmiar49wwdl7489xv0f0sqr5c66ip2jar0k"))))))

(define-public font-iosevka-ss06
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss06")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS06-" version ".zip"))
       (sha256
        (base32 "0c6wysyg8nzgan39z4w6l41abx2d0gj89mli63ydagqlljk7vs71"))))))

(define-public font-iosevka-ss07
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss07")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS07-" version ".zip"))
       (sha256
        (base32 "006khr7hh14dcq81mqzn3fkfrpbix1vghrvwff289r7cfy45ijkk"))))))

(define-public font-iosevka-ss08
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss08")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS08-" version ".zip"))
       (sha256
        (base32 "1dbnhbljybal9l61iq88h85jkmc16aaw8kxcq3r0ikzv2pq3vslm"))))))

(define-public font-iosevka-ss09
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss09")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS09-" version ".zip"))
       (sha256
        (base32 "0b218h42n5qg920q95ip3mva2xz012ijzgxj8r9m638qar0ba1qi"))))))

(define-public font-iosevka-ss10
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss10")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS10-" version ".zip"))
       (sha256
        (base32 "005i3am8amvwg8skq5lh5pisq1ll8hi9nj3imlcvcwj0w2i8b04w"))))))

(define-public font-iosevka-ss11
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss11")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS11-" version ".zip"))
       (sha256
        (base32 "0gnk9arz1dfksjx2d7gnb6vx0p0p7hpz4gcnkaj6ppdry9a5lmdn"))))))

(define-public font-iosevka-ss12
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss12")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS12-" version ".zip"))
       (sha256
        (base32 "17ri9f2ka7jsb4n3bbb18vxnjxk432f8740hzbjpnvzy16av5nhk"))))))

(define-public font-iosevka-ss13
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss13")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS13-" version ".zip"))
       (sha256
        (base32 "1nb3vd7q3xcbijawk9pqznkcfrjdykrc83jl58dnx7jf0znk7j4d"))))))

(define-public font-iosevka-ss14
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss14")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS14-" version ".zip"))
       (sha256
        (base32 "0dqkrf86h77ps90rdbsndngzg0l8c4jh8l3f3vcmdy1vlcrycpbi"))))))

(define-public font-iosevka-ss15
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss15")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS15-" version ".zip"))
       (sha256
        (base32 "0qd2di4phlxakprm5bqi0kz9yy48wqrhkxidzjswagf6bnm3mk5v"))))))

(define-public font-iosevka-ss16
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss16")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS16-" version ".zip"))
       (sha256
        (base32 "15yr94a3faribc19j91q7jna0hx3dsdpz1axkzr5gy56w7hg6bw4"))))))

(define-public font-iosevka-ss17
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss17")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS17-" version ".zip"))
       (sha256
        (base32 "0k8blbzymyvwczngafz3vijwdy4iwhxc4lrfqxgddrfi57lbnj5r"))))))

(define-public font-iosevka-ss18
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss18")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTC-IosevkaSS18-" version ".zip"))
       (sha256
        (base32 "08ijx9rbcx95yiaiwv6k25xmsi24rdy50mkmmaw94mmwv22mxdra"))))))

(define-public font-aporetic
  (package
    (name "font-aporetic")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/protesilaos/aporetic")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cihagy7vhw5pqznbihwv3pgb516i94iqfnvfm73njrx1a4dalz6"))))
    (build-system font-build-system)
    (home-page "https://github.com/protesilaos/aporetic")
    (synopsis "Customised build of the Iosevka typeface")
    (description
     "Aporetic fonts are a custom build of Iosevka with
  different style and metrics than the default.  Aporetic optimises for
  inter-glyph and inter-style consistency within the overarching
  constraint of usability at small point sizes.")
    (license (list license:silofl1.1))))

(define-public font-iosevka-comfy
  (deprecated-package "font-iosevka-comfy"  font-aporetic))

(define-public font-junicode
  (package
    (name "font-junicode")
    (version "2.211")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/psb1558/Junicode-font")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nk6fgby5sp6035p542pfk2fgjir36vk315mj5z5xf7rafy13jhb"))))
    (build-system font-build-system)
    (home-page "https://github.com/psb1558/Junicode-font")
    (synopsis "Unicode font for medievalists, linguists, and others")
    (description
     "The Junicode font was developed for students and scholars of
medieval Europe, but its large glyph repertoire also makes it useful as a
general-purpose font.  Its visual design is based on the typography used by
Oxford University Press in the late 17th and early 18th centuries.  The font
implements the @acronym{MUFI, Medieval Unicode Font Initiative} recommendation
for encoding ligatures, alternative letter forms, and other features of
interest to medievalists using Unicode's Private Use Area.

Junicode 2 is a major reworking of the font family.  Its OpenType programming
has been rebuilt to support the creation of searchable, accessible electronic
documents using the @acronym{MUFI} characters.  The family includes five
weights and five widths in both Roman and Italic, plus variable fonts.")
    (license license:silofl1.1)))

(define-public font-sarasa-gothic
  (package
    (name "font-sarasa-gothic")
    (version "1.0.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/be5invis/Sarasa-Gothic"
                           "/releases/download/v"
                           version
                           "/Sarasa-TTC-"
                           version
                           ".7z"))
       (sha256
        (base32 "1y82wp3rgm1xnn92f0jppgiqjsimdy83ljyh5q9dybzx3fp0x8w7"))))
    (build-system font-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (mkdir "source")
              (chdir "source")
              (invoke "7z" "x" source))))))
    (native-inputs (list p7zip))
    (home-page "https://github.com/be5invis/Sarasa-Gothic")
    (license license:silofl1.1)
    (synopsis "CJK programming font based on Iosevka and Source Han Sans")
    (description
     "Sarasa Gothic is a programming font based on Iosevka and Source Han Sans,
most CJK characters are same height, and double width as ASCII characters.")
    (properties '((upstream-name . "Sarasa")))))

(define-public font-space-grotesk
  (package
    (name "font-space-grotesk")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/floriankarsten/space-grotesk")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aiivn0rl7ydiyqvsr0fa2hx82h3br3x48w3100fcly23n0fdcby"))))
    (build-system font-build-system)
    ;; TODO: Package fontmake and gftools and build from source.
    (home-page "https://floriankarsten.github.io/space-grotesk/")
    (synopsis "Proportional variant of the fixed-width Space Mono family")
    (description
     "Space Grotesk is a proportional sans-serif typeface variant based on Colophon
Foundry's fixed-width Space Mono family.  It retains the monospace's idiosyncratic
details while optimizing for improved readability at non-display sizes.

Space Grotesk includes Latin Vietnamese, Pinyin, and all Western, Central, and
South-Eastern European language support, as well as several OpenType features:
old-style and tabular figures, superscript and subscript numerals, fractions,
and stylistic alternates.")
    (license license:silofl1.1)))

(define-public font-go
  (let ((commit "41969df76e82aeec85fa3821b1e24955ea993001")
        (revision "2"))
    (package
      (name "font-go")
      (version (git-version "2.010" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/image")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1z7lxkb84ca013ys0pr1bma2zdfzrvqip4yl6s941iyby0xfiyws"))))
      (build-system font-build-system)
      (arguments
       (list
        #:license-file-regexp "^(LICENSE|PATENTS)$"
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'chdir
              (lambda _
                (chdir "font/gofont/ttfs")
                #t))
            (add-before 'install-license-files 'enter-license-directory
              (lambda _
                (chdir "../../.."))))))
      (home-page "https://go.dev/blog/go-fonts")
      (synopsis "The Go font family")
      (description
       "The Go font family is a set of WGL4 TrueType fonts from the Bigelow &
Holmes type foundry, released under the same license as the Go programming
language.  It includes a set of proportional, sans-serif fonts, and a set of
monospace, slab-serif fonts.")
      (license license:bsd-3))))

(define-public font-google-material-design-icons
  (package
    (name "font-google-material-design-icons")
    (version "4.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/material-design-icons")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c8ah9rj82a1y0jgi5j0hszn7ndv4jb5kxmikv71alqq69xd8zn1"))))
    (build-system font-build-system)
    (home-page "https://google.github.io/material-design-icons")
    (synopsis "Icon font of Google Material Design icons")
    (description
     "Material design system icons are simple, modern, friendly, and sometimes
quirky.  Each icon is created using our design guidelines to depict in simple
and minimal forms the universal concepts used commonly throughout a UI.
Ensuring readability and clarity at both large and small sizes, these icons
have been optimized for beautiful display on all common platforms and display
resolutions.")
    (license license:asl2.0)))

(define-public font-opendyslexic
  (package
    (name "font-opendyslexic")
    (version "0.91.12")
    (source
      (origin
        (method url-fetch/zipbomb)
        (uri (string-append "https://github.com/antijingoist/opendyslexic/"
                            "releases/download/v" version
                            "/opendyslexic-0.910.12-rc2-2019.10.17.zip"))
        (sha256
         (base32
          "11ml7v4iyf3hr0fbnkwz8afb8vi58wbcfnmn4gyvrwh9jk5pybdr"))))
    (build-system font-build-system)
    (native-inputs (list unzip))
    (home-page "https://opendyslexic.org/")
    (synopsis "Font for dyslexics and high readability")
    (description "OpenDyslexic is a font designed to help readability for some
of the symptoms of dyslexia.  Letters have heavy weighted bottoms to provide
an indication of orientation to make it more difficult to confuse with other
similar letters.  Consistently weighted bottoms can also help reinforce the
line of text.  The unique shapes of each letter can help prevent flipping and
swapping.  The italic style for OpenDyslexic has been crafted to be used for
emphasis while still being readable.")
    (license license:silofl1.1)))

(define-public font-openmoji
  (package
    (name "font-openmoji")
    (version "15.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hfg-gmuend/openmoji/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iil5jmkkqrqgq06q3gvgv7j1bq8499q3h2340prwlfi2sqcqzlk"))))
    (build-system font-build-system)
    (arguments
     (list
      #:modules `((ice-9 ftw)
                  (guix build font-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "font")))
          (add-after 'chdir 'strip-alternative-variants
            (lambda _
              (let ((keep '("OpenMoji-black-glyf" "OpenMoji-color-glyf_colr_0"
                            "." "..")))
                (for-each (lambda (f)
                            (unless (member f keep)
                              (delete-file-recursively f)))
                          (scandir ".")))))
          (add-before 'install-license-files 'chdir-back
            (lambda _
              (chdir ".."))))))
    (home-page "https://openmoji.org")
    (synopsis "Font for rendering emoji characters")
    (description
     "This package provides the OpenMoji font in both color and black
variants.")
    (license license:cc-by-sa4.0)))

(define-public font-dosis
  (package
    (name "font-dosis")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://web.archive.org/web/20180228233737/"
                           "https://www.impallari.com/media/releases/dosis-"
                           "v" version ".zip"))
       (sha256
        (base32 "1qhci68f68mf87jd69vjf9qjq3wydgw1q7ivn3amjb65ls1s0c4s"))))
    (build-system font-build-system)
    (home-page (string-append "https://web.archive.org/web/20180228233737/"
                              "https://www.impallari.com/dosis"))
    (synopsis "Very simple, rounded, sans serif family")
    (description
     "Dosis is a very simple, rounded, sans serif family.
The lighter weights are minimalist.  The bolder weights have more personality.
The medium weight is nice and balanced.  The overall result is a family
that's clean and modern, and can express a wide range of voices & feelings.
It comes in 7 incremental weights:
ExtraLight, Light, Book, Medium, Semibold, Bold & ExtraBold")
    (license license:silofl1.1)))

(define-public font-culmus
  (package
    (name "font-culmus")
    (version "0.140")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/"
                           "culmus/files/culmus/" version "/culmus-src-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0fn79vndpa45gqr4mjmxzwy910x4ls1s6wbnycyf44bhpz4b4z5s"))))
    (build-system font-build-system)
    (arguments
     `(#:license-file-regexp "^GNU-GPL|LICENSE"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'build
           (lambda _
             (for-each (lambda (font)
                         (invoke "./GenerateOTF.py" font)
                         (invoke "./GenerateTTF.py" font))
                       (find-files "." "\\.sfd$")))))))
    (native-inputs
     (list fontforge))
    (home-page "https://culmus.sourceforge.io/")
    (synopsis "TrueType Hebrew Fonts for X11")
    (description "14 Hebrew trivial families.  Contain ASCII glyphs from various
sources.  Those families provide a basic set of a serif (Frank Ruehl), sans
serif (Nachlieli) and monospaced (Miriam Mono) trivials.  Also included Miriam,
Drugulin, Aharoni, David, Hadasim etc.  Cantillation marks support is
available in Keter YG.")
    (license license:gpl2))) ; consult the LICENSE file included

(define-public font-lohit
  (package
    (name "font-lohit")
    (version "20140220")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.pagure.org/lohit/lohit-ttf-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1rmgr445hw1n851ywy28csfvswz1i6hnc8mzp88qw2xk9j4dn32d"))))
    (build-system font-build-system)
    (home-page "https://pagure.io/lohit")
    (synopsis "Lohit TrueType Indic fonts")
    (description "Lohit is a font family designed to cover Indic scripts.
Lohit supports the Assamese, Bengali, Devanagari (Hindi, Kashmiri, Konkani,
Maithili, Marathi, Nepali, Sindhi, Santali, Bodo, Dogri languages), Gujarati,
Kannada, Malayalam, Manipuri, Oriya, Punjabi, Tamil and Telugu scripts.")
    (license license:silofl1.1)))

(define-public font-blackfoundry-inria
  (package
    (name "font-blackfoundry-inria")
    (version "1.200")
    (home-page "https://github.com/BlackFoundry/InriaFonts")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "06775y99lyh6hj5hzvrx56iybdck8a8xfqkipqd5c4cldg0a9hh8"))
              (file-name (string-append name "-" version "-checkout"))))
    ;; XXX: There are .ufo directories (the "source") so in theory we should
    ;; be able to rebuild TTF and OTF files with FontForge.  Unfortunately a
    ;; command like:
    ;;
    ;;  fontforge -lang=ff -c "Open('InriaSans-Regular.ufo'); Generate('foo.ttf');"
    ;;
    ;; segfaults in '_UFOLoadGlyph', which calls out to libpython.  :-/
    ;; In the meantime we ship the precompiled OTF and TTF files.
    (build-system font-build-system)
    (synopsis "Inria Sans and Inria Serif type family")
    (description
     "Inria Sans and Inria Serif are the two members of a type family designed
for Inria, a public research institute in computer science and mathematics.")
    (license license:silofl1.1)))

(define-public font-sil-gentium
  (package
    (name "font-sil-gentium")
    (version "6.200")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://software.sil.org/downloads/r/gentium/GentiumPlus-"
                    version ".zip"))
              (sha256
               (base32
                "0wxhsxv7xqsfbrywax0lcbmyfbrvrcm5g4c7a2v4j4cng4xi08cv"))))
    ;; Note: The zip file provides TTF files only, but the developer release,
    ;; which contains additional files, has a 'SOURCES.txt' file that says
    ;; that "the primary source files for the fonts are the fonts themselves".
    ;; Thus it looks like the TTF can be considered source.
    (build-system font-build-system)
    (synopsis "Serif font for the Cyrillic, Greek, and Latin alphabets")
    (description
     "Gentium is a typeface family designed to enable the diverse ethnic
groups around the world who use the Latin, Cyrillic and Greek scripts to
produce readable, high-quality publications.  The font comes with regular and
italics shapes.  This package provides only TrueType files (TTF).")
    (home-page "https://software.sil.org/gentium/")
    (license license:silofl1.1)))

(define-public font-sil-andika
  (package
    (name "font-sil-andika")
    (version "6.200")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://software.sil.org/downloads/r/andika/Andika-"
                    version ".zip"))
              (sha256
               (base32
                "0z7qvjlidn3m2k40mwnm3azf3wd8pi1yvy2q30p5vkyyzhipb6nc"))))
    ;; As for Gentium (see above), the TTF files are considered source.
    (build-system font-build-system)
    (synopsis "Sans serif font designed especially for literacy use")
    (description
     "Andika SIL is a sans serif, Unicode-compliant font designed especially
for literacy use, taking into account the needs of beginning readers.  The
focus is on clear, easy-to-perceive letterforms that will not be readily
confused with one another.  This package provides only TrueType files (TTF).")
    (home-page "https://software.sil.org/andika/")
    (license license:silofl1.1)))

(define-public font-sil-charis
  (package
    (name "font-sil-charis")
    (version "6.200")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://software.sil.org/downloads/r/charis/CharisSIL-"
                    version ".zip"))
              (sha256
               (base32
                "1pksr5wc9grdj75md4phr1a0gpjxk7xlmhv2nybsd2hbfrssl2ab"))))
    ;; As for Gentium (see above), the TTF files are considered source.
    (build-system font-build-system)
    (synopsis "Serif font for the Cyrillic and Latin alphabets")
    (description
     "Charis SIL is a Unicode-based font family that supports the wide range
of languages that use the Latin and Cyrillic scripts.  It is specially
designed to make long texts pleasant and easy to read, even in less than ideal
reproduction and display environments.  This package provides only TrueType
files (TTF).")
    (home-page "https://software.sil.org/charis/")
    (license license:silofl1.1)))

(define-public font-monaspace
  (package
    (name "font-monaspace")
    (version "1.200")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/githubnext/monaspace")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0llhn40mbi67slkb9y3g16165v6hayczr11kygpz0zx6azg3m1lv"))))
    (build-system font-build-system)
    (outputs '("out" "ttf" "woff"))
    (home-page "https://monaspace.githubnext.com")
    (synopsis "Innovative superfamily of fonts for code")
    (description
     "The Monaspace type system is a monospaced type superfamily with some
modern tricks up its sleeve.  It consists of five variable axis typefaces.
Each one has a distinct voice, but they are all metrics-compatible with one
another, allowing you to mix and match them for a more expressive
typographical palette.")
    (license license:silofl1.1)))

(define-public font-mononoki
  (package
    (name "font-mononoki")
    (version "1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/madmalik/mononoki/")
                    (commit version)))
              (sha256
               (base32
                "1mqinvb3hyimga8qjgpwgfv30yddzyicd79shhjbsns91v5z4x33"))
              (file-name (git-file-name name version))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files ".")))))))
    (synopsis "Font for programming and code review")
    (description
     "Mononoki is a typeface by Matthias Tellen, created to enhance code
formatting.")
    (home-page "https://madmalik.github.io/mononoki/")
    (license license:silofl1.1)))

(define-public font-paytone-one
  (let ((version "0")
        (commit "b1438bc11966d48a1e9e8943b7b8a32dcb0c533c")
        (revision "0"))
    (package
      (name "font-paytone-one")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/googlefonts/paytoneFont")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1gbmrjx8yj8xjh6fs0pzh8j0kxvwvhhjlp16gmv5c7i6b8s7r4r2"))))
      (build-system font-build-system)
      (home-page "https://github.com/googlefonts/paytoneFont")
      (synopsis "Sans serif typeface")
      (description "Paytone One is a sans serif typeface developed for
use as a display and headlining webfont.

The face has a slight casual appearance with ample round bowls.  The
slanted stroke terminals add some visual play to the overall appearance
of the font.")
      (license license:silofl1.1))))

(define-public font-plemoljp
  (package
    (name "font-plemoljp")
    (version "1.2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/yuru7/PlemolJP/releases/download/"
                    "v" version "/PlemolJP_v" version ".zip"))
              (sha256
               (base32
                "0pkkys5kl5s79shd1jmwfyk469ih8cymqb4vjwdadj52kzq4m9z6"))))
    (build-system font-build-system)
    (home-page "https://github.com/yuru7/PlemolJP")
    (synopsis "Plex Mono Language JP")
    (description "PlemolJP (Plex Mono Language JP) is a Japanese programming
font that is a composite of IBM Plex Mono and IBM Plex Sans JP.")
    (license license:silofl1.1)))

(define-public font-public-sans
  (package
    (name "font-public-sans")
    (version "2.001")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uswds/public-sans")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p3rl4w8m381jxb7in6z9iwjbk1chd7s0gk6jz96c8ci1bddbxk0"))))
    (build-system font-build-system)
    (home-page "https://public-sans.digital.gov/")
    (synopsis "Neutral typeface for interfaces, text, and headings")
    (description
     "Public Sans is a strong, neutral, sans-serif typeface for text or
display based on Libre Franklin.")
    (license license:silofl1.1)))

(define-public font-hermit
  (package
    (name "font-hermit")
    (version "2.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://pcaro.es/d/otf-hermit-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "09rmy3sbf1j1hr8zidighjgqc8kp0wsra115y27vrnlf10ml6jy0"))))
    (build-system font-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://pcaro.es/p/hermit/")
    (synopsis "Monospace font")
    (description
     "Hermit is a monospace font designed to be clear, pragmatic and very
readable.  Its creation has been focused on programming.  Every glyph was
carefully planned and calculated, according to defined principles and rules.
For this reason, Hermit is coherent and regular.

Symbols stand out from common text.  Dots and commas are easily seen, and
operators are clear even when not surrounded by spaces.  Similar characters
have been designed to be very distinguishable from each other.")
    (license license:silofl1.1)))

(define-public font-dseg
  (package
    (name "font-dseg")
    (version "0.46")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "https://github.com/keshikan/DSEG/"
                         "releases/download/v" version
                         "/fonts-DSEG_v"
                         (string-concatenate (string-split version #\.))
                         ".zip"))
        (sha256
          (base32 "13133kpa1ndsji9yq5ppkds5yq2y094qvrv2f83ah74p40sz9hm6"))))
    (build-system font-build-system)
    (arguments
     `(#:license-file-regexp "^DSEG-LICENSE.txt$"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (font-dir (string-append out "/share/fonts"))
                    (truetype-dir (string-append font-dir "/truetype")))
               (for-each (lambda (f) (install-file f truetype-dir))
                         (find-files "." "\\.ttf$"))
               #t))))))
    (home-page "https://www.keshikan.net/fonts-e.html")
    (synopsis "DSEG: 7-segment and 14-segment fonts")
    (description
     "DSEG is a font family that imitates seven- and fourteen-segment LCD
displays (7SEG, 14SEG).  DSEG includes the roman alphabet and symbol glyphs.
This package provides the TrueType fonts.")
    (license license:silofl1.1)))

(define-public font-sil-ezra
  (package
    (name "font-sil-ezra")
    (version "2.51")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://software.sil.org/downloads/r/ezra/EzraSIL-"
                           version ".zip"))
       (sha256
        (base32
         "1h8cfrvjdwxk963bw359jdg86bycwyyhvviqy6lwcfj7qhzcnszi"))))
    (build-system font-build-system)
    (home-page "https://software.sil.org/ezra/")
    (synopsis "Biblia Hebraica Stuttgartensia (BHS) typography inspired typeface")
    (description "Ezra SIL is a typeface fashioned after the square letter
forms of the typography of the Biblia Hebraica Stuttgartensia (BHS), a
beautiful Old Testament volume familiar to Biblical Hebrew scholars.  This
font package provides @code{Ezra SIL} as well as @code{Ezra SIL SR}, which has
a different style of marking.")
    (license license:expat)))

(define-public font-jetbrains-mono
  (package
    (name "font-jetbrains-mono")
    (version "2.304")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/JetBrains/JetBrainsMono/releases/"
                       "download/v" version "/JetBrainsMono-" version ".zip"))
       (sha256
        (base32 "1gvv5w0vfzndzp8k7g15j5i3yvnpr5z3imrwjs5flq19xp37cqvg"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install-license-files 'change-directory-to-archive-root
           ;; Find the license file outside of the default subdirectory.
           (lambda _
             (chdir "..")
             #t)))))
    (home-page "https://www.jetbrains.com/lp/mono/")
    (synopsis "Mono typeface for developers")
    (description
     "JetBrains Mono is a font family dedicated to developers.  JetBrains
Mono’s typeface forms are simple and free from unnecessary details.  Rendered
in small sizes, the text looks crisper.")
    (license license:silofl1.1)))

(define-public font-juliamono
  (package
    (name "font-juliamono")
    (version "0.043")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/cormullion/juliamono/releases/download/"
             "v" version "/JuliaMono-ttf.tar.gz"))
       (sha256
        (base32
         "0vb7n9yqgasnxzps13ckklay5bla6b0i79pzmfqvjms1r37079gh"))))
    (build-system font-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (mkdir "source")
                      (chdir "source")
                      (invoke "tar" "xzf" source))))))
    (native-inputs (list tar))
    (home-page "https://github.com/cormullion/juliamono")
    (synopsis "Monospaced font for programming")
    (description
     "JuliaMono is a monospaced font for scientific and technical computing,
designed to work for programming in the Julia Programming Language and other
text environments.")
    (license license:silofl1.1)))

(define-public font-vazir
  (package
    (name "font-vazir")
    (version "22.1.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append "https://github.com/rastikerdar/vazir-font/"
                       "releases/download/v" version
                       "/vazir-font-v" version ".zip"))
       (sha256
        (base32
         "0w3gwb5q33x5llw7cfs8qhaxr4ssg6rsx4b9day3993zn24xq031"))))
    (build-system font-build-system)
    (home-page "https://rastikerdar.github.io/vazir-font/")
    (synopsis "Vazir Persian typeface")
    (description
     "Vazir is a beautiful and elegant Persian typeface originally based on
DejaVu, and comes in six variants: Thin, Light, Normal, Medium, Bold, and
Black.  This package provides four versions of Vazir:

@itemize
@item @code{Vazir}: The main version; includes Latin glyphs from Roboto.
@item @code{Vazir-FD}: Like @code{Vazir}, but (always) uses Farsi digit glyphs
instead of Latin ones.
@item @code{Vazir-WOL}: Like @code{Vazir}, but without Roboto's Latin glyphs.
@item @code{Vazir-FD-WOL}: Combination of @code{Vazir-FD} and @code{Vazir-WOL}:
always uses Farsi digits, and does not include Latin glyphs from Roboto.
@end itemize\n")
    (license
     ;; See https://github.com/rastikerdar/vazir-font/blob/master/LICENSE for
     ;; details.
     (list license:public-domain        ; the Vazir modifications to DejaVu
                                        ; and the DejaVu modifications to...
           (license:x11-style           ; ...the Bitstream Vera typeface
            "file://LICENSE" "Bitstream Vera License")
           license:asl2.0))))           ; Latin glyphs from Roboto

(define-public font-victor-mono
  (package
   (name "font-victor-mono")
   (version "1.5.3")
   (source (origin
            (method url-fetch/zipbomb)
            (uri (string-append
                       "https://github.com/rubjo/victor-mono/raw/v"
                       version
                       "/public/VictorMonoAll.zip"))
            (sha256 "1axiwxz8l46cc60jfp2la8a9qpj866236lz3dc5l6m35r9as56l3")))
   (build-system font-build-system)
   (synopsis "Font with support for italics and ligatures")
   (description "Victor Mono is an open-source monospaced font with
optional semi-connected cursive italics and programming symbol ligatures.
This package provides only TrueType files (TTF).
It comes in seven weights and Roman, Italic and Oblique styles.")
   (home-page "https://rubjo.github.io/victor-mono/")
   (license license:expat)))

(define-public font-dongle
  (let ((commit "f7127c4d2450e1cad20254ec692591347e2fc260")
        (revision "1"))
    (package
      (name "font-dongle")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yangheeryu/Dongle")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1gwrjv468bqfa3nxh01vprk7rp24cnhk3zlkrv5mzqcbcdf96nqp"))))
      (build-system font-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'build
             (lambda _
               (begin
                 (chdir "sources")
                 (invoke "unzip" "Dongle.zip")
                 (chdir "..")
                 (invoke "python3" "build.py")))))))
      (native-inputs
       (list python
             python-glyphslib
             python-fonttools
             python-ufolib2
             python-ufo2ft
             zip))
      (synopsis
       "Rounded sans-serif typeface, supporting Hangeul and Latin glyphs")
      (description
       "Dongle(동글) is a rounded sans-serif typeface for display.  It is a
modular Hangeul with the de-square frame, creating a playful and rhythmic
movement.  The name, Dongle comes from a Korean onomatopoeia, meaning 'rounded
or curved shape (with adorable impression)’.

Dongle was originally designed as a 'Jamo (consonant and vowel in Hangeul)
typing module' for the author's student project.  Later it revised into
‘syllabic module’ to be released to the public.  As the character size varies
according to the syllable structure, Dongle typeface is much smaller compared
to other square frame Korean typefaces.  Therefore, it is better to adjust the
font size visually to your liking, rather than relying on the point size of
the editing program.

It is designed especially for Hangeul typography, but it also includes Latin
alphabet as a part of KS X 1001.  This typeface has a light, regular, and bold
weight.")
      (home-page "https://github.com/yangheeryu/Dongle")
      (license license:silofl1.1))))

(define-public font-meera-inimai
  (package
    (name "font-meera-inimai")
    (version "2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/smc/meera-inimai")
             (commit "0f39cdd7dbf1b6d1bed7df85834d33789dce20a7")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1x5mhrpx24imh0r4l83mkaiszxgwi1q4ppyyvq63h3ddwk20cwdg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list fontforge
           `(,harfbuzz "bin")
           python-brotli
           python-fonttools-minimal
           python-minimal))
    (arguments
     (list #:make-flags #~(list "PY=python3"
                                (string-append "DESTDIR=" #$output)
                                "fontpath=/share/fonts/truetype")
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))))
    (home-page "https://gitlab.com/smc/meera-inimai")
    (synopsis "Meera Inimai Tamil font")
    (description "Meera Inimai is a Unicode font for the Tamil Script.  Meera
Inimai is a san-serif typeface.  It is best used as a screen font for body
text.  It is also useful for body text of printed pamphlets or single page
designs.  Meera Inimai can be thought of as similar to Helvetica and its
variation Arial.  Tamil characters are inherently vertically-elliptical.  The
orthography of Roman glyphs of Meera Inimai are also based on this
characteristic so that they sit smoothly with the Tamil glyphs.")
    (license license:silofl1.1)))

(define-public font-ipa
  (package
    (name "font-ipa")
    (version "003.03")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://moji.or.jp/wp-content/ipafont/IPAfont/IPAfont"
                    (string-join (string-split version #\.) "") ".zip"))
              (sha256
               (base32
                "1rbgfq14ld0cwas6bx5h7pwyv2hkfa8ihnphsaz1brxqliwysmgp"))))
    (build-system font-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-read-only
            (lambda _
              ;; Otherwise the files have the executable bit set.
              (for-each (lambda (file) (chmod file #o444))
                        (find-files "." #:directories? #f))))
          (add-after 'install 'install-doc
            (lambda _
              (let ((font+version
                     #$(string-append "IPAfont"
                                      (string-join (string-split version #\.)
                                                   "")))
                    (doc-dir (string-append #$output "/share/doc/" #$name)))
                (with-directory-excursion font+version
                  (mkdir-p doc-dir)
                  (copy-file (string-append "Readme_" font+version ".txt")
                             (string-append doc-dir "/README"))
                  (copy-file "IPA_Font_License_Agreement_v1.0.txt"
                             (string-append doc-dir "/LICENSE")))))))))
    (home-page "https://moji.or.jp/ipafont/")
    (synopsis "Japanese font from the Information-technology Promotion Agency")
    (description "This package provides Japanese outline fonts by
Information-technology Promotion Agency, Japan (IPA)")
    (license license:ipa)))

(define-public font-ipa-ex
  (package
    (name "font-ipa-ex")
    (version "004.01")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://moji.or.jp/wp-content/ipafont/IPAexfont/"
                    "IPAexfont" (string-join (string-split version #\.) "")
                    ".zip"))
              (sha256
               (base32
                "0jwpszgisrls1lsgq1ngcm99zjaikb8hshr02512qrzrnd53gy5w"))))
    (build-system font-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-read-only
            (lambda _
              ;; Otherwise the files have the executable bit set.
              (for-each (lambda (file)
                          (chmod file #o444))
                        (find-files "." #:directories? #f))))
          (add-after 'install 'install-doc
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((font+version
                     #$(string-append
                        "IPAexfont"
                        (string-join (string-split version #\.) "")))
                    (doc-dir (string-append #$output "/share/doc/" #$name)))
                (with-directory-excursion font+version
                  (mkdir-p doc-dir)
                  (copy-file (string-append "Readme_" font+version ".txt")
                             (string-append doc-dir "/README"))
                  (copy-file "IPA_Font_License_Agreement_v1.0.txt"
                             (string-append doc-dir "/LICENSE")))))))))
    (home-page "https://moji.or.jp/ipafont/")
    (synopsis "Japanese font from the Information-technology Promotion Agency")
    (description "IPAex Fonts are suitable for both display and printing.
This is a modernized version of IPA Fonts that aims to provide a good balance
for authoring Japanese documents mixed with Western characters, while
following Japanese printing tradition.  Japanese characters (Kanji, Kana and
punctuation marks) are full width mono-space pitch, and Western characters are
proportional pitch.")
    (license license:ipa)))

(define-public font-ipa-mj-mincho
  (package
    (name "font-ipa-mj-mincho")
    (version "006.01")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://mojikiban.ipa.go.jp/OSCDL/IPAmjMincho"
                                  "/ipamjm" (string-join (string-split version #\.) "")
                                  ".zip"))
              (sha256
               (base32
                "0s2vs9p7vd7ajnn6c2icli069sjwi4d45a39fczqpwwn507lwj9m"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc-dir (string-append (assoc-ref outputs "out")
                                           "/share/doc/font-ipa-mj-mincho")))
               (mkdir-p doc-dir)
               (copy-file "Readme.txt" (string-append doc-dir "/README"))
               (copy-file "IPA_Font_License_Agreement_v1.0.txt"
                          (string-append doc-dir "/LICENSE"))
               #t))))))
    (home-page "https://mojikiban.ipa.go.jp/1300.html")
    (synopsis "Japanese font from the Information-technology Promotion Agency")
    (description "MJM Mincho is a font that aims at, for example, allowing you
to write people's name, or for formal business situations where it is necessary
to have a detailed and proper character style.")
    (license license:ipa)))

(define-public font-jigmo
  (package
    (name "font-jigmo")
    (version "20230816")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://kamichikoichi.github.io/jigmo/Jigmo-"
                           version ".zip"))
       (sha256
        (base32 "1higvn4qvz25hx215fs2vqlzh2d2645jbxf07yd82y09drl429jd"))))
    (build-system font-build-system)
    (home-page "https://kamichikoichi.github.io/jigmo/")
    (synopsis "Font contains most CJK unified ideograph characters")
    (description "Jigmo is a font contains all CJK unified ideograph characters
in Unicode 15.1 standard, ranges from CJK extension A to CJK extension I.
It is generated by the font shape data in GlyphWiki using KAGE system, Clipper
and FontForge.")
    (license license:cc0)))

(define-public font-plangothic
  (package
    (name "font-plangothic")
    (version "1.8.5760")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Fitzgerald-Porthmouth-Koenigsegg/"
             "Plangothic-Project/releases/download/V"
             version "/Plangothic.ttc"))
       (sha256
        (base32 "0ha2hcgy95ibmjk8lqfz0ihfc09swrzz3grlchma7qrwyxqbwpc0"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(let* ((out #$output)
               (dest (string-append out "/share/fonts/truetype")))
          (use-modules (guix build utils))
          (mkdir-p dest)
          (copy-file #$(package-source this-package)
                     (string-append dest "/Plangothic.ttc")))))
    (home-page
     "https://github.com/Fitzgerald-Porthmouth-Koenigsegg/Plangothic-Project")
    (synopsis "Sans font covers most CJK unified ideograph characters")
    (description "Plangothic is a sans font based on Source Han Sans,
modified to cover most CJK unified ideograph characters.")
    (license license:silofl1.1)))

(define-public font-fontna-yasashisa-antique
  (package
    (name "font-fontna-yasashisa-antique")
    (version "0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://flop.sakura.ne.jp/font/fontna-op/"
                                  "YasashisaAntiqueFont.zip"))
              (sha256
               (base32
                "1hl2qk3lzmh9h2vv5647vhlslkn3vqbq9rqgp4wzybajafx8c6nj"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; encoding issues cause many phases to fail
         (add-after 'unpack 'fix-encoding
           (lambda _
             ;; This directory, TrueType（サポート外）, is not properly encoded,
             ;; which makes rename-file fail. Instead, use shell globbing to
             ;; select and rename the directory.
             (invoke "sh" "-c" "mv TrueType* TrueType")))
         (add-before 'install-license-files 'enter-license-directory
           (lambda _
             (chdir "IPAexfont00201"))))))
    (native-inputs (list bash-minimal coreutils))
    (home-page "http://www.fontna.com/blog/1122/")
    (synopsis "Mix font of gothic kanji and minchou kana")
    (description "Antique is a font that is popular to write manga bubbles,
dictionary headwords and picture books.  This font reduces the thickness
differences in characters compared to other antique fonts.")
    (license (list license:ipa
                   (license:non-copyleft "mplus-TESTFLIGHT-057/LICENSE_E")))))

(define-public font-mplus-testflight
  (package
    (name "font-mplus-testflight")
    (version "063a")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://osdn.net/frs/redir.php?"
                                  "m=gigenet&f=mplus-fonts%2F62344%2Fmplus-TESTFLIGHT-"
                                  version ".tar.xz"))
              (file-name (string-append name "-" version ".tar.xz"))
              (sha256
               (base32
                "0yfx9i77638yrgclnwwl4af79ri3vifi1nslwi6mgzva9cxrgss4"))))
    (build-system font-build-system)
    (home-page "https://mplus-fonts.osdn.jp/index.html")
    (synopsis "Japanese font collection")
    (description "M+ is a collection of Japanese fonts with all Latin glyph
sets, with Basic Latin, Latin-1 Supplement, Latin Extended-A, and IPA
Extensions.  In addition to European letters used in many Western European
languages, it contains Japanese characters, including Kana glyphs and more
than 5,300 Kanji glyphs, as well major international phonetic symbols,
operators and special symbols.")
    (license (license:non-copyleft "file:///LICENSE_E"))))

(define-public font-catamaran
  (let ((commit "7559b4906f9c9148fb22c6f89508c3053a78a296")
        (revision "1"))
    (package
      (name "font-catamaran")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/VanillaandCream/Catamaran-Tamil")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1wpp41l7j2kpgnyavhgxcc5wp852a4wqsnwravn39gp980s84yxw"))))
      (build-system font-build-system)
      (home-page "https://github.com/VanillaandCream/Catamaran-Tamil")
      (synopsis "9 weight Tamil and Latin type")
      (description "Catamaran is a 9 weight Tamil and Latin type.  Catamaran
is a stylish type with a polished yet relaxed feel.  Its versatility makes it
suitable for a wide range of uses.")
      (license license:silofl1.1))))

(define-public font-cozette
  (package
    (name "font-cozette")
    (version "1.13.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/slavfox/Cozette")
                     (commit (string-append "v." version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "178i812n4sfsvid7jhnm683jlxqmrv4ck6qbb4nwyllhwg3gyq60"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-depend-on-git
           (lambda _
             (substitute* "build.py"
               ;; Merely importing this module requires a git repository.
               ;; We don't use get_changelog, so just disable the import.
               (("from cozette_builder\\.changeloggen import get_changelog")
                ""))))
         (add-before 'install 'build
           (lambda _
             (invoke "python3" "build.py" "fonts"))))))
    (native-inputs
     (list fontforge
           python
           python-crayons
           python-fonttools
           python-numpy
           python-pillow))
    (home-page "https://github.com/slavfox/Cozette")
    (synopsis "Bitmap programming font")
    (description "Cozette is a 6x13px (bounding box) bitmap font based on Dina
and heavily inspired by Creep.")
    (license license:expat)))

(define-public font-montserrat
  (package
    (name "font-montserrat")
    (version "7.222")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/JulietaUla/Montserrat")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03cfk45r5g694dqp2gjgg0qwra8w78nl6hc6p56qwd5dcfgr2l3r"))))
    (build-system font-build-system)
    (home-page "https://github.com/JulietaUla/Montserrat")
    (synopsis "The Montserrat font")
    (description
     "The old posters and signs in the traditional Montserrat neighborhood of
Buenos Aires inspired Julieta Ulanovsky to design this typeface and rescue the
beauty of urban typography that emerged in the first half of the twentieth
century.")
    (license license:silofl1.1)))

(define-public font-overpass
  (package
    (name "font-overpass")
    (version "3.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RedHatOfficial/Overpass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vsp94h7v5sn29hajv2ng94gyx4pqb0xgvn3gf7jp2q80gdv8pkm"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-webfonts
           (lambda _
             (delete-file-recursively "webfonts"))))))
    (home-page "https://overpassfont.org")
    (synopsis "Sans serif font family inspired by Highway Gothic")
    (description
     "Overpass is a sans-serif typeface based on the U.S. interstate highway
road signage typefaces, adapted for on-screen display and user interfaces.
Overpass includes proportional and monospace variants.")
    (license (list license:silofl1.1
                   license:lgpl2.1))))

(define-public font-cormorant
  (package
    (name "font-cormorant")
    (version "3.609")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CatharsisFonts/Cormorant")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fjp2xk4bjx8i6jamkyjq2fdr7324fh41pbn634iwnhdvvawvbav"))))
    (build-system font-build-system)
    (outputs '("out" "ttf" "woff"))
    (home-page "https://github.com/CatharsisFonts/Cormorant")
    (synopsis
     "Extravagant display serif typeface in the spirit of Garamond")
    (description
     "Cormorant is an extravagant display serif typeface inspired by
the Garamond heritage.  The design goal of Cormorant was to distill
the aesthetic essence of Garamond, unfetter it from the limitations of
metal printing, and allow it to bloom into its natural refined form at
high definition.  Cormorant is characterized by scandalously small
counters, razor-sharp serifs, dangerously smooth curves, and
flamboyantly tall accents.  While many implementations of Garamond at
small optical sizes already exist, Cormorant aims for the sparsely
populated niche of display-size counterparts that exploit the high
resolution of contemporary screens and print media to the fullest.")
    (license license:silofl1.1)))

(define-public font-bravura
  (package
    (name "font-bravura")
    (version "1.393")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/steinbergmedia/bravura")
             ;; Should be:
             ;;   (string-append "bravura-" version)
             ;; but missing tag for 1.393. Requested upstream at:
             ;; https://github.com/steinbergmedia/bravura/issues/61
             (commit "3df1714e6f9d522a8d2b6ee6888fa3e68e71199d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d0a2z1gl0kzfnd5z0nv2gd226qwll13kis2xrhx213w6r849180"))))
    (build-system font-build-system)
    (home-page "https://www.smufl.org/fonts/")
    (synopsis
     "OpenType music font and SMuFL reference implementation")
    (description
     "Bravura is an OpenType music font and the reference implementation for
the W3C Standard Music Font Layout (SMuFL).  Bravura draws on the heritage of
the finest European music engraving of the 19th and early 20th centuries, with
a bolder and more substantial look than most other music fonts: thin strokes
are slightly thicker than in other fonts, improving the overall ``blackness''
of the font and its legibility when read at a distance.

In addition to Bravura itself, which is for use with music notation
software (such as MuseScore), the family includes a Bravura Text variant
optimized for using musical symbols inline with regular text.")
    (license license:silofl1.1)))

(define-public font-charter
  (let ((butterick-version "210112")) ;; yymmdd
    (package
      (name "font-charter")
      (version (string-append "2.0.0-" butterick-version))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://practicaltypography.com/fonts/Charter%20"
                             butterick-version ".zip"))
         (file-name (string-append name "-" version ".zip"))
         (sha256
          (base32 "1j8iv2dl695zrabs2knb7jsky8mjis29a2ddpna4by8mlvqrf0ml"))))
      (build-system font-build-system)
      (arguments (list #:license-file-regexp "^Charter license.txt$"))
      (home-page "https://practicaltypography.com/charter.html")
      (synopsis "Charter fonts in OpenType and TrueType formats")
      (description "Charter was designed by Matthew Carter in 1987 and was
contributed by Bitstream to the X Consortium in 1992.  This package provides
OpenType, TrueType, and @acronym{WOFF2, Web Open Font Format 2} versions
converted from the Type 1 originals by Matthew Butterick.")
      (license
       (license:non-copyleft
        "file://Charter license.txt"
        (string-append
         "Bitstream contributed the Charter family "
         "to the X Consortium in 1992.  "
         "The license is also embedded in the font metadata."))))))

(define-public font-termsyn
  (package
    (name "font-termsyn")
    (version "1.8.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/termsyn/termsyn-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15vsmc3nmzl0pkgdpr2993da7p38fiw2rvcg01pwldzmpqrmkpn6"))))
    (build-system font-build-system)
    (outputs '("out" "psf" "otf"))
    (native-inputs (list fontforge))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'install 'build
                    (lambda _
                      (use-modules (ice-9 regex)
                                   (ice-9 match))
                      (define (pcf2 name ext)
                        (invoke "fontforge" "-lang=ff" "-c"
                                (string-append "Open('"
                                               name
                                               "');"
                                               "Generate('"
                                               (basename name "pcf")
                                               ext
                                               "','ttf')")))
                      (for-each (lambda (pcf)
                                  (pcf2 pcf "otf"))
                                (find-files "." "\\.pcf$")) #t))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((pcf (assoc-ref outputs "out")) (psf (assoc-ref
                                                                   outputs
                                                                   "psf"))
                             (otf (assoc-ref outputs "otf"))
                             (pcf-font-dir (string-append pcf
                                            "/share/fonts/termsyn"))
                             (otf-font-dir (string-append otf
                                            "/share/fonts/termsyn-otf"))
                             (psf-font-dir (string-append psf
                                            "/share/kbd/consolefonts")))
                        (mkdir-p pcf-font-dir)
                        (mkdir-p otf-font-dir)
                        (mkdir-p psf-font-dir)
                        (for-each (lambda (pcf)
                                    (install-file pcf pcf-font-dir))
                                  (find-files "." "\\.pcf$"))
                        (for-each (lambda (psfu)
                                    (install-file psfu psf-font-dir))
                                  (find-files "." "\\.psfu$"))
                        (for-each (lambda (otf)
                                    (install-file otf otf-font-dir))
                                  (find-files "." "\\.otf$"))) #t)))))
    (home-page "https://sourceforge.net/projects/termsyn/")
    (synopsis "Monospaced font based on terminus and tamsyn")
    (description
     "Termsyn is a clean monospaced bitmap font based on Terminus and Tamsyn.

This package contains the following outputs:
@enumerate
@item out: pcf font
@item otf: otf font
@item psf: psfu font
@end enumerate
")
    (license license:gpl2)))

(define-public font-arphic-ukai
  (package
    (name "font-arphic-ukai")
    (version "0.2.20080216.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://deb.debian.org/debian/pool/main"
                                  "/f/fonts-arphic-ukai/fonts-arphic-ukai_"
                                  version ".orig.tar.bz2"))
              (sha256
               (base32
                "1lp3i9m6x5wrqjkh1a8vpyhmsrhvsa2znj2mx13qfkwza5rqv5ml"))))
    (build-system font-build-system)
    (home-page "https://www.freedesktop.org/wiki/Software/CJKUnifonts/")
    (synopsis "Truetype fonts for Taiwanese and Hakka")
    (description
     "This package provides a set of Truetype fonts, which contain all
characters necessary to display Taiwanese and Hakka.")
    (license (license:fsdg-compatible
              "https://www.freedesktop.org/wiki/Arphic_Public_License/"))))

(define-public font-atui-feather
  (let ((version "1.1.0")               ;from package.json
        (commit "2ac71612ee85b3d1e9e1248cec0a777234315253")
        (revision "1"))
    (package
      (name "font-atui-feather")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (commit commit)
                      (url "https://github.com/AT-UI/feather-font/")))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0fnj5rh6z3ymny4cywha04x15i52r3h8ds87nx0lhqhfw6y8g02v"))))
      (build-system font-build-system)
      (home-page "https://at-ui.github.io/feather-font/")
      (synopsis "Iconfont version of Feather")
      (description
       "Feather is a collection of simply beautiful icons.  Each
icon is designed on a 24x24 grid with an emphasis on simplicity, consistency,
and readability.  This package bundles those icons into a font.")
      (license license:expat))))

(define-public font-lxgw-wenkai
  (package
    (name "font-lxgw-wenkai")
    (version "1.511")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/lxgw/LxgwWenKai/releases/download/v"
                    version "/lxgw-wenkai-v" version ".tar.gz"))
              (sha256
               (base32
                "1q3fdxnvn9xk6ifxrq2j3bg59hk13cl81dzgksxnmg9z7jhwd0bh"))))
    (build-system font-build-system)
    (home-page "https://lxgw.github.io/2021/01/28/Klee-Simpchin/")
    (synopsis "Simplified Chinese Imitation Song typeface")
    (description
     "LXGW Wenkai is a Simplified Chinese Imitation Song typeface covering the
CJK Unified Ideographs (base block) and commonly used Hangul.  For characters
within GB 2312, standard glyphs for Mainland China is used.")
    (license license:silofl1.1)
    (properties '((upstream-name . "lxgw-wenkai")))))

(define-public font-lxgw-wenkai-tc
  (package
    (inherit font-lxgw-wenkai)
    (name "font-lxgw-wenkai-tc")
    (version "1.511")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/lxgw/LxgwWenKaiTC/releases/download/v"
                    version "/lxgw-wenkai-tc-v" version ".tar.gz"))
              (sha256
               (base32
                "0hijm0jhzbzg8rlc2hic032m9qmhm5i48jn12spc6mrkmhvqwxyr"))))
    (home-page "https://github.com/lxgw/LxgwWenKaitc")
    (synopsis "Traditional Chinese Imitation Song typeface")
    (description
     "LXGW Wenkai TC is a Traditional Chinese inherited glyphs form Imitation
Song typeface covering commonly used characters as well as written form of
dialects in Hong Kong and Taiwan.")
    (properties '((upstream-name . "lxgw-wenkai-tc")))))

(define-public font-lxgw-neozhisong
  (package
    (name "font-lxgw-neozhisong")
    (version "0.920.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxgw/LxgwNeoZhiSong/releases/download/v"
             version "/LXGWNeoZhiSong.ttf"))
       (sha256
        (base32 "02jv5ysd450i47m3qmdwm3w23bp4wlqrjdwk6iirhgpv169p901j"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(let ((out #$output)
              (dest (string-append #$output "/share/fonts/truetype")))
          (use-modules (guix build utils))
          (mkdir-p dest)
          (copy-file #$(package-source this-package)
                     (string-append dest "/LXGWNeoZhiSong.ttf")))))
    (home-page "https://github.com/lxgw/LxgwNeoZhiSong")
    (synopsis "Simplified Chinese Song typeface derived from IPAmj Mincho")
    (description "LXGW NeoZhiSong is a Simplified Chinese Song typeface derived
from IPAmj Mincho, modified to adapt to the standard glyph shape used in
Mainland China.")
    (license license:ipa)))

(define-public font-lxgw-heartserif
  (package
    (name "font-lxgw-heartserif")
    (version "0.920.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxgw/LxgwNeoZhiSong/releases/download/v"
             version "/LXGWHeartSerif.ttf"))
       (sha256
        (base32 "1nbhvy0b9vb0w5pfpp5f0jdkb6fs422avkdxzqydmv74g5v8gz07"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(let ((out #$output)
              (dest (string-append #$output "/share/fonts/truetype")))
          (use-modules (guix build utils))
          (mkdir-p dest)
          (copy-file #$(package-source this-package)
                     (string-append dest "/LXGHeartSerif.ttf")))))
    (home-page "https://github.com/lxgw/LxgwNeoZhiSong")
    (synopsis "Simplified Chinese Song typeface derived from Kokoro Mincho")
    (description "LXGW HeartSerif is a Simplified Chinese Song typeface derived
from Kokoro Mincho, modified to adapt to the standard glyph shape used in
Mainland China.")
    (license license:ipa)))

(define-public font-chiron-sung-hk
  (package
    (name "font-chiron-sung-hk")
    (version "1.017")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chiron-fonts/chiron-sung-hk")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pg0zh4gajn699am26j4ldpsa51bafn7n0jc5s4v6sslixj3ccwg"))))
    (build-system font-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (let ((install (assoc-ref %standard-phases 'install)))
                     (with-directory-excursion "VAR"
                       (for-each delete-file (find-files "." "\\.ttf$"))
                       (install #:outputs `(("out" . ,#$output))))
                     (with-directory-excursion "OTF"
                       (install #:outputs `(("out" . ,#$output:otf))))
                     (with-directory-excursion "TTF"
                       (install #:outputs `(("out" . ,#$output:ttf))))))))))
    (outputs '("out" "otf" "ttf"))
    (home-page "https://chiron-fonts.github.io/")
    (synopsis "Traditional Chinese Song typeface")
    (description
     "Chiron Sung HK is a Traditional Chinese Song typeface based on the Hong
Kong variant of Adobe’s Source Han Serif.  The font aims at providing a modern,
region-agnostic glyph set adopting the “modern” glyph style that is similar to
prevalent typefaces in Traditional Chinese regions.")
    (license license:silofl1.1)))

(define-public font-chiron-hei-hk
  (package
    (inherit font-chiron-sung-hk)
    (name "font-chiron-hei-hk")
    (version "2.524")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chiron-fonts/chiron-hei-hk")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "077f50yjcf5slr2jzrdampwcrlaswvdnin5iwnirzsms3x9vsm69"))))
    (synopsis "Traditional Chinese Gothic typeface")
    (description
     "Chiron Hei HK is a Traditional Chinese Gothic typeface based on the Hong
Kong variant of Adobe’s Source Han Sans.  The font aims at providing a modern,
region-agnostic glyph set adopting the “modern” glyph style that is similar to
prevalent typefaces in Traditional Chinese regions.")))

(define-public font-spleen
  (package
    (name "font-spleen")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/fcambus/spleen/releases/download/"
                    version "/spleen-" version ".tar.gz"))
              (sha256
               (base32
                "0brp1kyp6i5q9zk3hpj5ngxh4js0aw2m6d4yrzxmif3f39pwaiwb"))))
    (build-system font-build-system)
    (outputs '("out" ;OTB
               "bdf" "otf" "pcf" "psf"))
    (home-page "https://www.cambus.net/spleen-monospaced-bitmap-fonts/")
    (synopsis "Monospaced bitmap font for consoles and terminals")
    (description
     "Spleen is a monospaced bitmap font available in 6 sizes:
 5x8, 6x12, 8x16, 12x24, 16x32, 32x64.

All sizes are provided in the Glyph Bitmap Distribution Format (BDF),
 PCF, PSF (for the Linux console), and OTB formats.  All sizes, except
 5x8, are provided in OTF format also.

All font sizes, except 5x8 and 6x12, contain all ISO/IEC 8859-1
 characters (Basic Latin and Latin-1 Supplement Unicode block), Latin
 Extended-A characters, as well as Box Drawing, Block Elements, and
 Braille Patterns Unicode blocks.

The 5x8 and 6x12 versions only contain printable ASCII characters,
 the Braille Patterns Unicode block, and light Box Drawing characters.

Spleen also has support for Powerline symbols out of the box.")
    (license license:bsd-2)))

(define-public font-stix-two
  (package
    (name "font-stix-two")
    (version "2.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stipub/stixfonts")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02wy9n49nzyvhc55jjmpxrv7hh6ncxv31liniqjgjn7vp68fj40n"))))
    (build-system font-build-system)
    (home-page "https://www.stixfonts.org/")
    (synopsis
     "OpenType Unicode fonts for scientific, technical, and mathematical texts")
    (description
     "The mission of the Scientific and Technical Information Exchange (STIX)
font creation project is the preparation of a comprehensive set of fonts that
serve the scientific and engineering community in the process from manuscript
creation through final publication, both in electronic and print formats.")
    (license license:silofl1.1)))

(define-public font-scientifica
  (package
    (name "font-scientifica")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/nerdypepper/scientifica/releases/download/"
                    "v" version "/scientifica.tar"))
              (sha256
               (base32
                "0zwa3s75lvbky2vn73i1fmxa37hi3zfm7f6wfpqwcip8l1lpi1gh"))))
    (build-system font-build-system)
    (outputs '("out" ;OTB
               "bdf" "ttf"))
    (home-page "https://github.com/nerdypepper/scientifica")
    (synopsis "Tall and condensed bitmap font for geeks")
    (description
     "@code{scientifica} is largely based on
@url{https://github.com/romeovs/creep, @code{creep}}, with a number of
minor tweaks to improve readability (a matter of taste of course).
Most characters are just 4px wide, which is brilliant for low dpi(90-120) displays.")
    (license license:silofl1.1)))

(define-public font-tuffy
  (package
    (name "font-tuffy")
    (version "20120614")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://tulrich.com/fonts/tuffy-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "02vf72bgrp30vrbfhxjw82s115z27dwfgnmmzfb0n9wfhxxfpyf6"))
       (snippet '(delete-file "._Tuffy.otf"))))
    (build-system font-build-system)
    ;; TODO: remove this when font-build-system have 'zstd'.
    (native-inputs (list zstd))
    (home-page "http://tulrich.com/fonts/")
    (synopsis "The Tuffy Truetype Font Family")
    (description
     "Thatcher Ulrich's first outline font design.  He started with the goal
of producing a neutral, readable sans-serif text font.  There are lots of
\"expressive\" fonts out there, but he wanted to start with something very
plain and clean, something he might want to actually use.")
    (license license:public-domain)))

(define-public font-velvetyne-jgs
  ;; There are no releases nor tags.
  (let ((revision "1")
        (commit "b1fe344c6ab4cb97aa9ceb09ba3b6056f826b040"))
    (package
      (name "font-velvetyne-jgs")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/velvetyne/jgs")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1hwaylgih66cqwcf68i2xdccrn0p0rbvmlz5f3mlzvm51s5pzqb8"))))
      (build-system font-build-system)
      (home-page "http://www.velvetyne.org/fonts/jgs-font")
      (synopsis "Font designed especially for ASCII art")
      (description
       "The jgs font family can be used to combine several
characters to form contiguous lines.  It contains several fonts:
@enumerate
@item jgs5 for sizes multiple of 1o (10px, 20px, 30px)
@item jgs7 for sizes multiple of 14 (14px, 28px, 42px)
@item jgs9 for sizes multiples of 18 (18px, 36px, 54px)
@end enumerate")
      (license license:silofl1.1))))

(define-public font-recursive
  (package
    (name "font-recursive")
    (version "1.085")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/arrowtype/recursive/"
                                  "releases/download/v"
                                  version
                                  "/ArrowType-Recursive-"
                                  version
                                  ".zip"))
              (sha256
               (base32
                "00ns6zwizp0wyxyrf7fxqmxm4gl7ygarxq1mj952h78q1rxdzjyb"))))
    (build-system font-build-system)
    ;; Default to ttf, which has "Rec Mono" for code and variable font.
    (outputs '("out" "otf" "woff"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'remove-separate-statics
            (lambda _
              ;; Prefer otc/ttc collection over those seperate files.
              (delete-file-recursively
               "Recursive_Desktop/separate_statics/"))))))
    (home-page "https://www.recursive.design/")
    (synopsis "Variable font family for code & UI")
    (description "Recursive Sans & Mono is a variable type family built for
better code & UI.  It is inspired by casual script signpainting, but designed
primarily to meet the needs of programming environments and application
interfaces.")
    (license license:silofl1.1)))

(define-public fonts-tlwg
 (package
   (name "fonts-tlwg")
   (version "0.7.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/tlwg/" name
           "/releases/download/v" version "/" name "-" version ".tar.xz"))
     (sha256
      (base32
       "00mv8rmjpsk8jbbl978q3yrc2pxj8a86a3d092563dlc9n8gykkf"))))
   (native-inputs (list fontforge))
   (build-system gnu-build-system)
   (home-page "https://github.com/tlwg/fonts-tlwg/")
   (synopsis "Collection of scalable Thai fonts")
   (description "Fonts-TLWG is a collection of Thai scalable fonts.  Its goal
is to provide fonts that conform to existing standards and recommendations, so
that it can be a reference implementation.")
   (license license:gpl2+)))

(define-public font-orbitron
  (let ((version "0")
        (commit "13e6a5222aa6818d81c9acd27edd701a2d744152")
        (revision "0"))
    (package
      (name "font-orbitron")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/theleagueof/orbitron")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1c6jb7ayr07j1pbnzf3jxng9x9bbqp3zydf8mqdw9ifln1b4ycyf"))))
      (build-system font-build-system)
      (outputs '("out" "ttf" "woff"))
      (home-page "https://github.com/theleagueof/orbitron")
      (synopsis "Futuristic geometric sans-serif")
      (description "Orbitron is a geometric sans-serif typeface intended
for display purposes.  It features four weights (light, medium, bold,
and black), a stylistic alternative, small caps, and many alternate
glyphs.")
      (license license:silofl1.1))))

(define-public font-oswald
  (let ((version "0")
        (commit "6e65651c229e897dc55fb8d17097ee7f75b2769b")
        (revision "0"))
    (package
      (name "font-oswald")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/googlefonts/OswaldFont")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0m5c98crw6df6hbhxv4smh6ldzk5fx434fyri8xgnsjjcrkqxy0h"))))
      (build-system font-build-system)
      (home-page "https://github.com/googlefonts/OswaldFont")
      (synopsis "Gothic typeface")
      (description "Oswald is a reworking of the classic gothic typeface
style historically represented by designs such as 'Alternate Gothic'.
The characters of Oswald have been re-drawn and reformed to better fit
the pixel grid of standard digital screens.  Oswald is designed to be
used freely across the internet by web browsers on desktop computers,
laptops and mobile devices.")
      (license license:silofl1.1))))

(define-public font-teko
  (let ((version "0")
        (commit "2bf909d46b0061a5e3e16e8acc4fef670e36a8f2")
        (revision "0"))
    (package
      (name "font-teko")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/googlefonts/teko")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "18gwb3k4a3a2406pxpxh9zcziggajl8wwki4730xsh1m066f6gk9"))))
      (build-system font-build-system)
      (home-page "https://github.com/googlefonts/teko")
      (synopsis "Devanagari and Latin scripts typeface")
      (description "Teko is a typeface that currently supports the
Devanagari and Latin scripts.  This font family has been created for
use in headlines and other display-sized text on screen.  Five font
styles make up the initial release.

The Teko typeface features letterforms with low stroke contrast,
square proportions and a structure that appears visually simple.

The Regular, Medium and Semibold fonts are recommended for use in long
headlines, while Bold is intended primarily for setting just one or
two words.  The Light is a variant that may be put to good use in
large headlines on websites.  At display sizes, Teko works equally
well on screen or in print.  Each font contains 1090 glyphs, offering
full support for the conjuncts and ligatures required by languages
written with the Devanagari script.")
      (license license:silofl1.1))))

(define-public font-dina
  (package
    (name "font-dina")
    (version "2.92")
    (source (origin
              (method url-fetch/zipbomb)
              (uri "https://www.dcmembers.com/jibsen/download/61/?wpdmdl=61")
              (file-name "Dina.zip")
              (sha256
               (base32
                "1kq86lbxxgik82aywwhawmj80vsbz3hfhdyhicnlv9km7yjvnl8z"))))
    (build-system font-build-system)
    (home-page "https://www.dcmembers.com/jibsen/download/61/")
    (synopsis "Dina programming font")
    (description "Dina is a monospace bitmap font, primarily aimed at programmers.
It is relatively compact to allow a lot of code on screen, while (hopefully) clear
enough to remain readable even at high resolutions.")
    (license license:expat)))

(define-public font-et-book
  (let ((commit "24d3a3bbfc880095d3df2b9e9d60d05819138e89"))
    (package
      (name "font-et-book")
      (version "1.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/edwardtufte/et-book")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bhpk1fbp8jdhzc5j8y5v3bpnzy2sz3dpgjgzclb0dnm51hqqrpn"))))
      (build-system font-build-system)
      (home-page "https://edwardtufte.github.io/et-book/")
      (synopsis "ET Book fonts")
      (description
       "ET Book is a Bembo-like font for the computer designed by Dmitry Krasny,
Bonnie Scranton, and Edward Tufte.")
      (license license:expat))))

(define-public font-cica
  (package
    (name "font-cica")
    (version "5.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/miiton/Cica/releases/download/"
                           "v" version "/Cica_v" version ".zip"))
       (sha256
        (base32 "0vshn2cd70mnbavsw9cbagcasa95wiv9qdj4wkzxn7gxygqvrlfb"))))
    (build-system font-build-system)
    (home-page "https://github.com/miiton/Cica")
    (synopsis "Japanese monospaced font for programming")
    (description
     "Cica is a Japanese monospaced font for programming.
Hack + DejaVu Sans Mono is used for ASCII, and Rounded Mgen+ for the other.
In addition, Nerd Fonts, Noto Emoji, Icons for Devs, and some adjustment forked
from the Ricty generator are converted and adjusted.")
    (license license:silofl1.1)))
