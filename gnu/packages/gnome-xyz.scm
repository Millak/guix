;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2019, 2021 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2020 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2020, 2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Ellis Kenyo <me@elken.dev>
;;; Copyright © 2020 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Songlin Jiang <hollowman@hollowman.ml>
;;; Copyright © 2021, 2022, 2024 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2021 Attila Lendvai <attila@lendvai.name>
;;; Copyright © 2021 Charles Jackson <charles.b.jackson@protonmail.com>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2022 Sughosha <sughosha@proton.me>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2022 Trevor Richards <trev@trevdev.ca>
;;; Copyright © 2023 Eidvilas Markevičius <markeviciuseidvilas@gmail.com>
;;; Copyright © 2025 aurtzy <aurtzy@gmail.com>
;;; Copyright © 2025 Ashvith Shetty <ashvithshetty0010@zohomail.in>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages gnome-xyz)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public arc-icon-theme
  (package
    (name "arc-icon-theme")
    (version "20161122")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/horst3180/arc-icon-theme")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ch3hp08qri93510hypzz6m2x4xgg2h15wvnhjwh1x1s1b7jvxjd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-configure-during-bootstrap
           (lambda _
             (substitute* "autogen.sh"
               (("^\"\\$srcdir/configure\".*") ""))
             #t)))))
    (native-inputs
     (list autoconf automake))
    ;; When Arc is missing an icon, it looks in the Moka icon theme for it.
    (propagated-inputs
     (list moka-icon-theme))
    (synopsis "Arc icon theme")
    (description "The Arc icon theme provides a set of icons matching the
style of the Arc GTK theme.  Icons missing from the Arc theme are provided by
the Moka icon theme.")
    (home-page "https://github.com/horst3180/arc-icon-theme")
    (license license:gpl3+)))

(define-public delft-icon-theme
  (package
    (name "delft-icon-theme")
    (version "1.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/madmaxms/iconpack-delft")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1iw85cxx9lv7irs28qi3815dk9f9vldv2j7jf1x5l1dqzwaxgwpb"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("." "share/icons" #:exclude ("README.md" "LICENSE" "logo.jpg")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-index.theme
           (lambda _
            (substitute* "Delft/index.theme"
              (("gnome") "Adwaita"))
            #t)))))
    (home-page "https://www.gnome-look.org/p/1199881/")
    (synopsis "Continuation of Faenza icon theme with up to date app icons")
    (description "Delft is a fork of the popular icon theme Faenza with up to
date app icons.  It will stay optically close to the original Faenza icons,
which haven't been updated for some years.  The new app icons are ported from
the Obsidian icon theme.")
    (license license:gpl3)))

(define-public faba-icon-theme
  (package
    (name "faba-icon-theme")
    (version "4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snwh/faba-icon-theme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xh6ppr73p76z60ym49b4d0liwdc96w41cc5p07d48hxjsa6qd6n"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-post-install
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script.*") "")))))))
    (native-inputs
     (list autoconf automake))
    (synopsis "Faba icon theme")
    (description
     "Faba is a minimal icon set used as a basis for other themes such as
Moka")
    (home-page "https://snwh.org/moka")
    (license (list license:lgpl3+
                   license:cc-by-sa4.0))))

(define-public moka-icon-theme
  (package
    (inherit faba-icon-theme)
    (name "moka-icon-theme")
    (version "5.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snwh/moka-icon-theme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "015l02im4mha5z91dbchxf6xkp66d346bg3xskwg0rh3lglhjsrd"))))
    (propagated-inputs
     ;; Moka is based on Faba by using it as a fallback icon set instead of
     ;; bundling it, so we need to add it as a propagated input.
     (list faba-icon-theme))
    (synopsis "Moka icon theme")
    (description "Moka is a stylized desktop icon set, designed to be clear,
simple and consistent.")
    (license (list license:gpl3+
                   license:cc-by-sa4.0))))

(define-public papirus-icon-theme
  (package
    (name "papirus-icon-theme")
    (version "20250201")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PapirusDevelopmentTeam/papirus-icon-theme")
             (commit version)))
       (sha256
        (base32 "1sbzmfcmbsa57grq0jy17mmagdsg95qr2v75zpqlp53jpjq74v53"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no test suite
      #:make-flags #~(list "CP_OPTS=--preserve=links"
                           (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (delete 'configure)
          (delete 'build)
          (add-before 'install 'halve-inode-consumption
            ;; This package uses over 100K inodes, which is a lot.  We can easily
            ;; halve that number by using (hard) links, to no ill effect.
            ;; See <https://logs.guix.gnu.org/guix/2023-01-31.log#171227>.
            ;; However, the source checkout will still use the full amount!
            (lambda _
              (let ((symlink? (lambda (_ stat)
                                (eq? 'symlink (stat:type stat)))))
                (for-each (lambda (file)
                            (let ((target (canonicalize-path file)))
                              (when (eq? 'regular (stat:type (stat target)))
                                (delete-file file)
                                (link target file))))
                          (find-files "." symlink?)))))
           (add-before 'install 'remove-executable-bit
             (lambda _
               (let ((file? (lambda (_ stat)
                              (eq? 'regular (stat:type stat)))))
                 (for-each (lambda (file) (chmod file #o644))
                           (find-files "." file?))))))))
    (native-inputs
     (list `(,gtk+ "bin")))
    (home-page "https://git.io/papirus-icon-theme")
    (synopsis "Fork of Paper icon theme with a lot of new icons and a few extras")
    (description "Papirus is a fork of the icon theme Paper with a lot of new icons
and a few extra features.")
    (license license:gpl3)))

(define-public qogir-icon-theme
  (package
    (name "qogir-icon-theme")
    (version "2025.02.15")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vinceliuice/Qogir-icon-theme")
                    (commit (string-replace-substring version "." "-"))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet '(substitute* "install.sh"
                          (("gtk-update-icon-cache") "true")))
              (sha256
               (base32
                "08qcphdp49ciyr294fgk9i1s3h6svx5bdzfglikb20jzh5d167hj"))))
    (build-system copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (let* ((dest (string-append #$output "/share/icons"))
                          (flags (list "--theme" "all"
                                       "--color" "all"
                                       "--dest" dest)))
                     (mkdir-p dest)
                     (apply invoke "bash" "install.sh" flags)))))))
    (home-page "https://www.pling.com/p/1296407/")
    (synopsis "Flat colorful design icon theme")
    (description "This package provides a flat colorful design icon theme.")
    (license license:gpl3)))

(define-public flat-remix-icon-theme
  (package
    (name "flat-remix-icon-theme")
    (version "20220525")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/daniruiz/flat-remix")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ygazxccqf7hn1hxnf1mmsp17gm1m4hpcandfz9v5ijrgkd1m596"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no included tests
       #:make-flags `(,(string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "https://drasite.com/flat-remix")
    (synopsis "Icon theme with material design")
    (description "Flat Remix is an icon theme inspired by material design.  It
is mostly flat using a colorful palette with some shadows, highlights, and
gradients for some depth.")
    (license license:gpl3+)))

(define-public flat-remix-gtk-theme
  (package
    (name "flat-remix-gtk-theme")
    (version "20220627")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/daniruiz/flat-remix-gtk")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kwahlrcm9rfsrd97q9lsbfz5390qafwbv78zl6j2vqgqnxhpwng"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no included tests
       #:make-flags `(,(string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "https://drasite.com/flat-remix-gtk")
    (synopsis "GTK application theme with material design")
    (description "Flat Remix GTK is a GTK application theme inspired by
material design.  It is mostly flat using a colorful palette with some
shadows, highlights, and gradients for some depth.")
    (license license:gpl3+)))

(define-public flat-remix-gnome-theme
  (package
    (name "flat-remix-gnome-theme")
    (version "20230508")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/daniruiz/flat-remix-gnome")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b31ayb4qvcr5m3dqcidl9ilpp3w4mr56wq6vrp73g4cj558pi9h"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("share" "/")
         ("themes" "/share/"))))
    (home-page "https://drasite.com/flat-remix-gnome")
    (synopsis "GNOME shell theme with material design")
    (description "Flat Remix GNOME is a GNOME shell theme inspired by material
design.  It is mostly flat using a colorful palette with some shadows,
highlights, and gradients for some depth.")
    (license license:gpl3+)))

(define-public bibata-cursor-theme
  (package
    (name "bibata-cursor-theme")
    (version "2.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ful1e5/Bibata_Cursor")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bhspswgxizc4sr2bihfjic8wm4khd6waw9qgw0yssfy0fm3nafc"))))
    (build-system trivial-build-system)
    (native-inputs (list python-attrs python-clickgen))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((themes-dir (string-append #$output "/share/icons")))
            (mkdir-p themes-dir)
            (let loop
                ((themes '(("Bibata-Modern-Amber" . "Yellowish and rounded")
                           ("Bibata-Modern-Classic" . "Black and rounded")
                           ("Bibata-Modern-Ice" . "White and rounded")
                           ("Bibata-Original-Amber" . "Yellowish and sharp")
                           ("Bibata-Original-Classic" . "Black and sharp")
                           ("Bibata-Original-Ice" . "White and sharp"))))
              (define theme
                (car themes))
              (invoke (search-input-file %build-inputs "/bin/ctgen")
                      (string-append #$source "/build.toml")
                      "-p" "x11"
                      "-d" (string-append #$source "/bitmaps/" (car theme))
                      "-n" (car theme)
                      "-c" (string-append (cdr theme) " edge Bibata cursors")
                      "-o" themes-dir)
              (unless (null? (cdr themes))
                (loop (cdr themes))))))))
    (home-page "https://github.com/ful1e5/Bibata_Cursor")
    (synopsis "Open-source, compact, and material-designed cursor set")
    (description
     "Bibata is an open-source, compact, and material designed
cursor set.  This project aims at improving the cursor experience.")
    (license license:gpl3)))

(define-public gnome-plots
  (package
    (name "gnome-plots")
    (version "0.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alexhuntley/Plots")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "168wcsrkmvq79xmwvbq615msd4q0rg7f57xqicidnr78jx4x37rd"))))
    (build-system python-build-system)
    (inputs
     (list bash-minimal                 ; for wrap-program
           gtk+
           pango
           python-freetype-py
           python-jinja2
           python-lark
           python-numpy
           python-pycairo
           python-pyglm
           python-pygobject
           python-pyopengl))
    (native-inputs
     (list python-pytest))
    (arguments
     (list
      #:imported-modules `((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
      #:modules '((guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-more
            (lambda _
              (let* ((datadir (string-append #$output "/share"))
                     (help (string-append datadir "/help"))
                     (icons (string-append datadir "/icons/hicolor")))
                (map (lambda (filename)
                       (match (string-split filename #\/)
                         ((_ lang dir ... name)
                          (install-file filename
                                        (string-join (cons* help lang "plots" dir)
                                                     "/")))))
                     (find-files "help"))
                (install-file "res/com.github.alexhuntley.Plots.desktop"
                              (string-append datadir "/applications/"))
                (install-file "res/com.github.alexhuntley.Plots.svg"
                              (string-append icons "/scalable/apps/"))
                (install-file "res/com.github.alexhuntley.Plots-symbolic.svg"
                              (string-append icons "/symbolic/apps/")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv"))))
          (add-after 'wrap 'gi-wrap
            (lambda _
              (let ((prog (string-append #$output "/bin/plots")))
                (wrap-program prog
                  `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))
          (add-after 'wrap 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (home-page "https://apps.gnome.org/app/com.github.alexhuntley.Plots/")
    (synopsis "Simple graph plotting")
    (description "Plots makes it easy to visualise mathematical formulae.  In
addition to basic arithmetic operations, it supports trigonometric,
hyperbolic, exponential, and logarithmic functions, as well as arbitrary sums
and products.  Plots is designed to integrate well with the GNOME desktop and
takes advantage of modern hardware using OpenGL.")
    (license license:gpl3+)))

(define-public portfolio
  (package
    (name "portfolio")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tchx84/Portfolio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ai9mx801m5lngkljg42vrpvhbvc3071sp4jypsvbzw55hxnn5ba"))))
    (arguments
     (list #:glib-or-gtk? #t
           #:imported-modules `(,@%meson-build-system-modules
                                (guix build python-build-system))
           #:modules '((guix build meson-build-system)
                       ((guix build python-build-system)
                        #:prefix python:)
                       (guix build utils))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'rename-executable
                          (lambda _
                            (with-directory-excursion (string-append #$output
                                                                     "/bin")
                              (symlink "dev.tchx84.Portfolio" "portfolio"))))
                        (add-after 'unpack 'skip-gtk-update-icon-cache
                          (lambda _
                            (substitute* "build-aux/meson/postinstall.py"
                              (("gtk-update-icon-cache") "true"))))
                        (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (wrap-program (search-input-file outputs
                                                             "bin/dev.tchx84.Portfolio")
                              `("GUIX_PYTHONPATH" =
                                (,(getenv "GUIX_PYTHONPATH") ,(python:site-packages
                                                               inputs
                                                               outputs)))
                              `("GI_TYPELIB_PATH" =
                                (,(getenv "GI_TYPELIB_PATH")))))))))
    (build-system meson-build-system)
    (inputs (list bash-minimal python-pygobject gtk libadwaita))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           pkg-config
           python))
    (home-page "https://github.com/tchx84/Portfolio")
    (synopsis "Minimalist file manager for Linux mobile devices")
    (description
     "Portfolio is a minimalist file manager for those who want to use Linux
mobile devices.  Tap to activate and long press to select, to browse, open,
copy, move, delete, or edit your files.")
    (license license:gpl3+)))

(define-public gnome-shell-extension-unite-shell
  (package
    (name "gnome-shell-extension-unite-shell")
    (version "82")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hardpixel/unite-shell")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kghsaq8p38r7ya0kd2ggcwjpq4f3gszrfk5x11ji2m91cfk9sh9"))))
    (build-system copy-build-system)
    (native-inputs (list `(,glib "bin") gettext-minimal))
    (inputs (list xprop))
    (arguments
     (list #:install-plan ''(("./unite@hardpixel.eu"
                              "share/gnome-shell/extensions/unite@hardpixel.eu"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-xprop-bin
                          (lambda _
                            (substitute* "unite@hardpixel.eu/window.js"
                              (("xprop")
                               (string-append #$(this-package-input "xprop")
                                              "/bin/xprop")))))
                        (add-before 'install 'compile-schemas
                          (lambda _
                            (with-directory-excursion "unite@hardpixel.eu/schemas"
                              (invoke "glib-compile-schemas" ".")))))))
    (home-page "https://github.com/hardpixel/unite-shell")
    (synopsis "Top panel and window decoration extension for GNOME Shell")
    (description
     "Unite is a GNOME Shell extension which makes a few layout
tweaks to the top panel and removes window decorations to make it look like
Ubuntu Unity Shell.")
    (license license:gpl3)))

(define-public gnome-shell-extension-appindicator
  (package
    (name "gnome-shell-extension-appindicator")
    (version "59")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/ubuntu/gnome-shell-extension-appindicator")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1k1h9haj0qgcv9hm3hw2nz7ppznp9zrpg922mhrfa6nj97carmqh"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs (list jq gnu-gettext
                         `(,glib "bin")))
    (synopsis "Adds KStatusNotifierItem support to GNOME Shell")
    (description "This extension integrates Ubuntu AppIndicators
and KStatusNotifierItems (KDE's successor of the systray) into
GNOME Shell.")
    (home-page "https://github.com/ubuntu/gnome-shell-extension-appindicator/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-clipboard-indicator
  (package
    (name "gnome-shell-extension-clipboard-indicator")
    (version "65")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     (string-append
                      "https://github.com/Tudmotu/"
                      "gnome-shell-extension-clipboard-indicator"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p10qnrjrlcyw270k87xwx92kp2cmn8vp4vsk59y13v6k9yaq4q8"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove pre-compiled settings schemas and translations from
               ;; source, as they are generated as part of build. Upstream
               ;; includes them for people who want to run the software
               ;; directly from source tree.
               '(begin
                  (delete-file "schemas/gschemas.compiled")
                  (for-each delete-file
                            (find-files "locale" "\\.mo$"))))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "share/gnome-shell/extensions/clipboard-indicator@tudmotu.com"
           #:include-regexp ("\\.css$" "\\.compiled$" "\\.js(on)?$" "\\.mo$" "\\.xml$")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'compile-schemas
            (lambda _
              (with-directory-excursion "schemas"
                (invoke "glib-compile-schemas" ".")))))))
    (native-inputs
     (list `(,glib "bin") gettext-minimal))
    (home-page "https://github.com/Tudmotu/gnome-shell-extension-clipboard-indicator")
    (synopsis "Clipboard manager extension for GNOME Shell")
    (description "Clipboard Indicator is a clipboard manager for GNOME Shell
that caches clipboard history.")
    (license license:expat)))

(define-public gnome-shell-extension-customize-ibus
  (package
    (name "gnome-shell-extension-customize-ibus")
    (version "90")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/Customize-IBus.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i7cw9kgp99b9awb2vfpxkwqdln4bfbf3qya19xnmzw16f5igbmn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
       #~(list (string-append "VERSION=" #$version)
               (string-append "INSTALLBASE=" #$output
                              "/share/gnome-shell/extensions"))
       #:tests? #f ; No test target
       #:phases
       #~(modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure))))
    (native-inputs
     (list gettext-minimal `(,glib "bin")))
    (propagated-inputs
     (list ibus))
    (home-page "https://github.com/openSUSE/Customize-IBus")
    (synopsis "GNOME Shell Extension for IBus Customization")
    (description "Customize IBus provides full customization of appearance,
behavior, system tray and input source indicator for IBus.")
    (license license:gpl3+)))

(define-public gnome-shell-extension-dash-to-dock
  (package
    (name "gnome-shell-extension-dash-to-dock")
    (version "99")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/micheleg/dash-to-dock")
                    (commit (string-append "extensions.gnome.org-v"
                                           version))))
              (sha256
               (base32
                "0hjmqd2xd33chxq7b7753m87by7z2mvr5njvs5wvshhg70kwwmxj"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list (string-append "INSTALLBASE="
                                          #$output
                                          "/share/gnome-shell/extensions"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (delete 'configure))))
    (native-inputs
     (list
      `(,glib "bin")
      intltool
      pkg-config
      sassc))
    (propagated-inputs
     (list glib))
    (synopsis "Transforms GNOME's dash into a dock")
    (description "This extension moves the dash out of the
overview, transforming it into a dock for easier application launching and
faster window switching.")
    (home-page "https://micheleg.github.io/dash-to-dock/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-gsconnect
  (package
    (name "gnome-shell-extension-gsconnect")
    (version "58")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/GSConnect"
                                        "/gnome-shell-extension-gsconnect.git"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jzpik0i03wpnxxmmhmn86vimpzvjmdkx76casi9sd7pwwdvi73f"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:tests? #f ;; every test fails
      #:configure-flags
      #~(let ((out #$output)
              (gnome-shell #$(this-package-input "gnome-shell"))
              (openssh #$(this-package-input "openssh"))
              (openssl #$(this-package-input "openssl")))
          (list
           (string-append "-Dgnome_shell_libdir=" gnome-shell "/lib")
           (string-append "-Dopenssl_path=" openssl "/bin/openssl")
           (string-append "-Dsshadd_path=" openssh "/bin/ssh-add")
           (string-append "-Dsshkeygen_path=" openssh "/bin/ssh-keygen")
           (string-append "-Dsession_bus_services_dir=" out "/share/dbus-1/services")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-post-installation
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false"))))
          ;; TODO: Remove after 'patch-shebangs is fixed to handle '/usr/bin/env -S'
          ;; shebangs (see bug#74450).
          (add-after 'unpack 'patch-gjs-shebangs
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each (lambda (file)
                          (substitute* file
                            (("^#!/usr/bin/env -S gjs.*$")
                             (string-append "#!" (which "gjs") " -m"))))
                        '("installed-tests/minijasmine"
                          "src/gsconnect-preferences"
                          "src/service/nativeMessagingHost.js"
                          "src/service/daemon.js"
                          "webextension/gettext.js"))))
          (add-before 'configure 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gapplication (search-input-file inputs "/bin/gapplication"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                (for-each
                 (lambda (file)
                   (substitute* file
                     (("gapplication") gapplication)))
                 '("data/org.gnome.Shell.Extensions.GSConnect.desktop.in"
                   "data/org.gnome.Shell.Extensions.GSConnect.Preferences.desktop.in"))
                (for-each (lambda (file)
                            (with-atomic-file-replacement
                             file
                             (lambda (input output)
                               (format output "~a"
                                       (string-append
                                        "'" gi-typelib-path "'.split(':').forEach("
                                        "path => imports.gi.GIRepository.Repository."
                                        "prepend_search_path(path));\n"))
                               (dump-port input output))))
                          '("src/extension.js" "src/prefs.js")))))
          (add-after 'install 'wrap-programs
            (lambda _
              (let* ((out #$output)
                     (gsconnect-dir (string-append
                                     out "/share/gnome-shell/extensions"
                                     "/gsconnect@andyholmes.github.io"))
                     (service-dir (string-append gsconnect-dir "/service"))
                     (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                (wrap-program (string-append gsconnect-dir "/gsconnect-preferences")
                  `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))
                (wrap-program (string-append service-dir "/daemon.js")
                  `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
    (inputs
     (list
      at-spi2-core
      bash-minimal
      caribou
      evolution-data-server
      gjs
      glib
      `(,glib "bin") ;for /bin/gapplication
      gsound
      gnome-shell
      gtk+
      nautilus
      openssh
      openssl
      python-pygobject
      upower))
    (native-inputs
     (list
      gettext-minimal
      gobject-introspection
      libxml2
      pkg-config))
    (home-page "https://github.com/GSConnect/gnome-shell-extension-gsconnect/wiki")
    (synopsis "Connect GNOME Shell with your Android phone")
    (description "GSConnect is a complete implementation of KDE Connect
especially for GNOME Shell, allowing devices to securely share content, like
notifications or files, and other features like SMS messaging and remote
control.")
    (license license:gpl2)))

(define-public gnome-shell-extension-just-perfection
  (package
    (name "gnome-shell-extension-just-perfection")
    (version "30.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/jrahmatzadeh/just-perfection/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y7m4y8zx7l6vl2f8w9nxac21x48ajcs5gf07r1k34adnf7wh8p2"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("src"
           "share/gnome-shell/extensions/just-perfection-desktop@just-perfection"
           #:include-regexp ("\\.css$" "\\.compiled$" "\\.js(on)?$" "\\.ui$"))
          ("locale"
           "share/gnome-shell/extensions/just-perfection-desktop@just-perfection/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'drop-executable-bits
            (lambda _
              (for-each
               (lambda (file)
                 (let ((stat (lstat file)))
                   (chmod file (logand (stat:mode stat) (lognot #o111)))))
               (find-files "." #:directories? #f))))
          (add-before 'install 'build
            (lambda _
              (invoke "glib-compile-schemas" "src/schemas")
              (for-each
               (lambda (file)
                 (let* ((base (basename file))
                        (noext (substring base 0 (- (string-length base) 3)))
                        (dest (string-append "locale/" noext "/LC_MESSAGES/"))
                        (out (string-append dest "just-perfection.mo")))
                   (mkdir-p dest)
                   (invoke "msgfmt" "-c" file "-o" out)))
               (find-files "po" "\\.po$")))))))
    (native-inputs
     (list `(,glib "bin") gettext-minimal))
    (home-page "https://gitlab.gnome.org/jrahmatzadeh/just-perfection")
    (synopsis "Customize GNOME Shell behaviour")
    (description "Just Perfection allows you to change various settings, that
GNOME Shell itself does not provide out of the box, such as the ability to hide
certain elements or change animation speeds.")
    (license license:gpl3)))

(define-public gnome-shell-extension-dash-to-panel
  (package
    (name "gnome-shell-extension-dash-to-panel")
    (version "64")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/home-sweet-gnome/dash-to-panel")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0dgp15qr9s9h9wam0a994sqd8rdab36wii6j6rai3s5p8bw8gsfn"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list (string-append "INSTALLBASE="
                                          #$output
                                          "/share/gnome-shell/extensions")
                           (string-append "VERSION="
                                          #$version))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'bootstrap)
           (delete 'configure))))
    (native-inputs
     (list
      `(,glib "bin")
      intltool
      pkg-config))
    (propagated-inputs
     (list glib))
    (synopsis "Icon taskbar for GNOME Shell")
    (description "This extension moves the dash into the gnome main
panel so that the application launchers and system tray are combined
into a single panel, similar to that found in KDE Plasma and Windows 7+.")
    (home-page "https://github.com/home-sweet-gnome/dash-to-panel/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-noannoyance
  ;; There are different forks of the NoAnnoyance extension. This is the one
  ;; named “NoAnnoyance (fork)” at
  ;; https://extensions.gnome.org/extension/6109/noannoyance-fork/ because it
  ;; supports newer GNOME Shell versions than the previously used “NoAnnoyance
  ;; v2”.
  (let ((commit "8312e010908119b0b0a744c27e661c89b35eddb7")
        ;; “NoAnnoyance v2” version 17 correlates with
        ;; c6804a47063659f9f48d13a0942b78ce98aac72b, from which we count
        ;; commits.
        (revision "23"))
    (package
      (name "gnome-shell-extension-noannoyance")
      (version (git-version "17" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jirkavrba/noannoyance")
                      (commit commit)))
                (sha256
                 (base32
                  "1pf575pwm304cn4kdjdjcxiyjsggmkcy9mrar901an0xr4vbm3pg"))
                (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~'(("." "share/gnome-shell/extensions/noannoyance-fork@vrba.dev"))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'compile-schemas
              (lambda _
                (with-directory-excursion "schemas"
                  (invoke "glib-compile-schemas" ".")))))))
      (native-inputs (list `(,glib "bin")))
      (synopsis "Remove 'Window is ready' annotation")
      (description "One of the many extensions that remove this message.
It uses ES6 syntax and claims to be more actively maintained than others.")
      (home-page "https://extensions.gnome.org/extension/2182/noannoyance/")
      (license license:gpl2))))

(define-public gnome-shell-extension-paperwm
  (package
    (name "gnome-shell-extension-paperwm")
    (version "47.1.0")                  ; still compatible with GNOME 46
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/paperwm/PaperWM")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vgrkda38va1pwrzf5bkkmfbpm2i61487x86zky2f02wimxl9i7m"))
              (snippet
               '(begin (delete-file "schemas/gschemas.compiled")))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "share/gnome-shell/extensions/paperwm@paperwm.github.com"
           #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                             "\\.xml$" "\\.compiled$" "\\.svg$")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'compile-schemas
            (lambda _
              (with-directory-excursion "schemas"
                (invoke "make")))))))
    (native-inputs
     (list `(,glib "bin"))) ; for glib-compile-schemas
    (home-page "https://github.com/paperwm/PaperWM")
    (synopsis "Tiled scrollable window management for GNOME Shell")
    (description "PaperWM is an experimental GNOME Shell extension providing
scrollable tiling of windows and per monitor workspaces.  It's inspired by paper
notebooks and tiling window managers.")
    (license license:gpl3)))

(define-public gnome-shell-extension-night-theme-switcher
  (package
    (name "gnome-shell-extension-night-theme-switcher")
    (version "78")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.com/rmnvgr/nightthemeswitcher-gnome-shell-extension")
             (commit version)))
       (sha256
        (base32 "13rqql22q3pv6dp3lmms7xbqna6h6x13gmd337fcqzc0k952xckc"))
       (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config (list glib "bin")))
    (synopsis "Automatic theme switcher for GNOME Shell")
    (description
     "Automatically toggle your GNOME desktop's color scheme between light and
dark, switch backgrounds and run custom commands at sunset and sunrise.")
    (home-page "https://nightthemeswitcher.romainvigier.fr")
    (license license:gpl2+)))

(define-public gpaste
  (package
    (name "gpaste")
    (version "44.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Keruspe/GPaste")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1amfr8hwf7401xa3fzaa8w17w3v3lxx0fkr7rqkkyfy57iavrykk"))
              (patches
               (search-patches "gpaste-fix-paths.patch"))))
    (build-system meson-build-system)
    (native-inputs
     (list gcr
           gettext-minimal
           gobject-introspection
           (list glib "bin")            ; for glib-compile-resources
           pkg-config
           vala))
    (inputs
     (list appstream-glib
           desktop-file-utils           ; for update-desktop-database
           gjs
           gtk+
           mutter
           libadwaita))
    (arguments
     (list #:glib-or-gtk? #true
           #:configure-flags
           #~(list
              (string-append "-Dcontrol-center-keybindings-dir="
                             #$output "/share/gnome-control-center/keybindings")
              (string-append "-Ddbus-services-dir="
                             #$output "/share/dbus-1/services")
              (string-append "-Dsystemd-user-unit-dir="
                             #$output "/etc/systemd/user"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-introspection-install-dir
                 (lambda _
                   (substitute* "src/libgpaste/gpaste/gpaste-settings.c"
                     (("@gschemasCompiled@")
                      (string-append #$output "/share/glib-2.0/schemas/")))
                   (substitute* '("src/gnome-shell/extension.js"
                                  "src/gnome-shell/prefs.js")
                     (("@typelibPath@")
                      (string-append #$output "/lib/girepository-1.0/"))))))))
    (home-page "https://github.com/Keruspe/GPaste")
    (synopsis "Clipboard management system for GNOME Shell")
    (description "GPaste is a clipboard manager, a tool which allows you to
keep a trace of what you’re copying and pasting.  Is is really useful when
you go through tons of documentation and you want to keep around a bunch of
functions you might want to use, for example.  The clipboard manager will
store an history of everything you do, so that you can get back to older
copies you now want to paste.")
    (license license:bsd-2)))

(define-public gnome-shell-extension-v-shell
  (package
    (name "gnome-shell-extension-v-shell")
    (version "47.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/G-dH/vertical-workspaces")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0c2hl7wi83aacl5w09h5daph06zwiv63vn9za2v63az1i8333sgi"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." #$(string-append
                  "share/gnome-shell/extensions/"
                  "vertical-workspaces@G-dH.github.com")
           #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                             "\\.xml$" "\\.compiled$" "\\.gresource$")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'build
            (lambda _
              (invoke "make" "all"))))))
    (native-inputs
     (list gettext-minimal `(,glib "bin")))
    (home-page "https://github.com/G-dH/vertical-workspaces")
    (synopsis "Shell configuration with horizontal or vertical workspaces")
    (description "V-Shell (Vertical Workspaces) lets the user configure different parts of the
shell, including panels, corners, workspaces.")
    (license license:gpl3)))

(define-public gnome-shell-extension-vertical-overview
  (deprecated-package "gnome-shell-extension-vertical-overview"
                      gnome-shell-extension-v-shell))

(define-public gnome-shell-extension-burn-my-windows
  (package
    (name "gnome-shell-extension-burn-my-windows")
    (version "44")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Schneegans/Burn-My-Windows/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1br4hv0xnb9q30p844z360f37xzk6xflfq27i8ipbxpk6bd12ik6"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." #$(string-append
                  "share/gnome-shell/extensions/"
                  "burn-my-windows@schneegans.github.com")
           #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                             "\\.xml$" "\\.compiled$" "\\.gresource$")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'compile-resources
            (lambda _
              (invoke "make" "resources/burn-my-windows.gresource")))
          (add-before 'install 'compile-schemas
            (lambda _
              (with-directory-excursion "schemas"
                (invoke "glib-compile-schemas" ".")))))))
    (native-inputs
     (list `(,glib "bin")))  ; for glib-compile-resources
    (home-page "https://github.com/Schneegans/Burn-My-Windows")
    (synopsis "Application closing effects extension")
    (description "Burn My Windows is a shell extension that stylizes the
animation of closing windowed applications.")
    (license license:gpl3)))

(define-public gnome-shell-extension-blur-my-shell
  (package
    (name "gnome-shell-extension-blur-my-shell")
    (version "67")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aunetx/blur-my-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zxz71iq8wzv7swfp4an8vwf8r4sswb6gi85cwh5afyc4mj5fn2r"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       (let ((extension "share/gnome-shell/extensions/blur-my-shell@aunetx"))
         `(("src/" ,extension)
           ("resources/" ,extension
            #:include-regexp ("\\.svg$" "\\.ui"))
           ("." ,extension
            #:exclude-regexp ("src/" "resources/")
            #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                              "\\.xml$" "\\.compiled$"))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'compile-schemas
           (lambda _
             (with-directory-excursion "schemas"
               (invoke "glib-compile-schemas" ".")))))))
    (native-inputs
     (list (list glib "bin"))) ; for glib-compile-schemas
    (home-page "https://github.com/aunetx/blur-my-shell")
    (synopsis "Blurs different parts of the GNOME Shell")
    (description "Blur My Shell adds a blur look to different parts of the
GNOME Shell, including the top panel, dash and overview.")
    (license license:gpl3)))

(define-public gnome-shell-extension-radio
  (package
    (name "gnome-shell-extension-radio")
    (version "22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/hslbck/gnome-shell-extension-radio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f1cjch60yv39lsngj9xrlyzc4f1gcaydm5xc5kc0rqi48f83d1f"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("radio@hslbck.gmail.com"
                          "/share/gnome-shell/extensions/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'glib-compile-schemas
            (lambda _
              (invoke "glib-compile-schemas"
                      "radio@hslbck.gmail.com/schemas"))))))
    (native-inputs (list `(,glib "bin")))
    (home-page "https://github.com/hslbck/gnome-shell-extension-radio")
    (synopsis "Internet radio for GNOME Shell")
    (description "This extension implements an internet radio player
directly inside GNOME Shell.  It can manage stations and play streams.")
    (license license:gpl3+)))


(define-public gnome-shell-extension-vitals
  (package
    (name "gnome-shell-extension-vitals")
    (version "69.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/corecoding/Vitals")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d587hh4l2v4skbvispy77lvkrj0c3wwkbvgxqya3dphmvc8pn27"))
              (modules '((guix build utils)))
              (snippet '(begin
                          (delete-file "schemas/gschemas.compiled")
                          (for-each delete-file
                                    (find-files "locale" "\\.mo$"))))))
    (build-system copy-build-system)
    (native-inputs (list `(,glib "bin") gettext-minimal))
    (inputs (list libgtop))
    (arguments
     (list #:modules '((guix build copy-build-system)
                       (guix build utils)
                       (ice-9 string-fun))
           #:phases #~(modify-phases %standard-phases
                        (add-before 'install 'compile-schemas
                          (lambda _
                            (invoke "glib-compile-schemas" "--strict"
                                    "schemas")))
                        (add-before 'install 'compile-locales
                          (lambda _
                            (for-each (lambda (file)
                                        (let ((destfile (string-replace-substring
                                                         file ".po" ".mo")))
                                          (invoke "msgfmt" "-c" file "-o"
                                                  destfile)))
                                      (find-files "locale" "\\.po$")))))
           #:install-plan #~'(("."
                               "share/gnome-shell/extensions/Vitals@CoreCoding.com"
                               #:include-regexp ("\\.js(on)?$" "\\.css$"
                                                 "\\.ui$"
                                                 "\\.svg$"
                                                 "\\.xml$"
                                                 "\\.mo$"
                                                 "\\.compiled$")))))
    (home-page "https://github.com/corecoding/Vitals")
    (synopsis
     "GNOME Shell extension displaying computer resource/sensor stats")
    (description
     "Vitals is a GNOME Shell extension that can display the computer
temperature, voltage, fan speed, memory usage and CPU load from the top menu
bar of the GNOME Shell.")
    (license license:gpl2+)))

(define-public arc-theme
  (package
    (name "arc-theme")
    (version "20221218")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jnsh/arc-theme")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yznqjz1a1mcwks8z7pybgzrjiwg978bfpdmkaq926wy82qslngd"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags
       '("-Dthemes=gnome-shell,gtk2,gtk3,gtk4,metacity,plank,unity,xfwm")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-home   ;placate Inkscape
           (lambda _
             (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list `(,glib "bin") ; for glib-compile-resources
           gnome-shell
           gtk+
           inkscape/pinned
           optipng
           pkg-config
           python
           sassc/libsass-3.5))
    (inputs (list gtk-engines)) ;for gtk+-2 to work properly
    (synopsis "Flat GTK+ theme with transparent elements")
    (description "Arc is a flat theme with transparent elements for GTK 3, GTK
2, and GNOME Shell which supports GTK 3 and GTK 2 based desktop environments
like GNOME, Unity, Budgie, Pantheon, XFCE, Mate, etc.")
    (home-page "https://github.com/horst3180/arc-theme")
    ;; No "or later" language found.
    (license license:gpl3+)))

(define-public greybird-gtk-theme
  (package
    (name "greybird-gtk-theme")
    (version "3.22.13")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/shimmerproject/Greybird")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "154qawiga792iimkpk3a6q8f4gm4r158wmsagkbqqbhj33kxgxhg"))))
    (build-system meson-build-system)
    (native-inputs
     (list gtk+
           `(,glib "bin") ; for "glib-compile-resources"
           (librsvg-for-system)
           pkg-config
           ruby-sass
           sassc))
    (home-page "https://shimmerproject.org/")
    (synopsis "Grey GTK+ theme based on Bluebird")
    (description "Greybird is a grey derivative of the Bluebird theme by the
Shimmer Project.  It supports GNOME, Unity, and Xfce.")
    (license (list license:gpl2+ license:cc-by-sa3.0))))

(define-public matcha-theme
  (package
    (name "matcha-theme")
    (version "2024-05-01")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/vinceliuice/Matcha-gtk-theme")
            (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "19dsa7bx37g76sm0l3x65kzq2sg4id3q6j649ny88a69kx2k1d5n"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (source (assoc-ref %build-inputs "source"))
                (bash (assoc-ref %build-inputs "bash"))
                (coreutils (assoc-ref %build-inputs  "coreutils"))
                (themesdir (string-append out "/share/themes")))
           (setenv "PATH"
                   (string-append coreutils "/bin:"
                                  (string-append bash "/bin:")))
           (copy-recursively source (getcwd))
           (patch-shebang "install.sh")
           (mkdir-p themesdir)
           (invoke "./install.sh" "-d" themesdir)
           #t))))
    (inputs
     (list gtk-engines))
    (native-inputs
     (list bash coreutils))
    (synopsis "Flat design theme for GTK 3, GTK 2 and GNOME-Shell")
    (description "Matcha is a flat Design theme for GTK 3, GTK 2 and
Gnome-Shell which supports GTK 3 and GTK 2 based desktop environments
like Gnome, Unity, Budgie, Pantheon, XFCE, Mate and others.")
    (home-page "https://github.com/vinceliuice/matcha")
    (license license:gpl3+)))

(define-public materia-theme
  (package
    (name "materia-theme")
    (version "20210322")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/nana-4/materia-theme")
            (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1fsicmcni70jkl4jb3fvh7yv0v9jhb8nwjzdq8vfwn256qyk0xvl"))))
    (build-system meson-build-system)
    (native-inputs
     (list gtk+ sassc))
    (home-page "https://github.com/nana-4/materia-theme")
    (synopsis "Material Design theme for a wide range of environments")
    (description "Materia is a Material Design theme for GNOME/GTK based
desktop environments.  It supports GTK 2, GTK 3, GNOME Shell, Budgie,
Cinnamon, MATE, Unity, Xfce, LightDM, GDM, Chrome theme, etc.")
    (license license:gpl2+)))

(define-public numix-gtk-theme
  (package
    (name "numix-gtk-theme")
    (version "2.6.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/numixproject/numix-gtk-theme")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12mw0kr0kkvg395qlbsvkvaqccr90cmxw5rrsl236zh43kj8grb7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "INSTALL_DIR="
                            (assoc-ref %outputs "out")
                            "/share/themes/Numix"))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (native-inputs
     (list `(,glib "bin")               ; for glib-compile-schemas
           gnome-shell
           gtk+
           libxml2
           ruby-sass))
    (synopsis "Flat theme with light and dark elements")
    (description "Numix is a modern flat theme with a combination of light and
dark elements.  It supports GNOME, Unity, Xfce, and Openbox.")
    (home-page "https://numixproject.github.io")
    (license license:gpl3+)))

(define-public orchis-theme
  (package
    (name "orchis-theme")
    (version "2024-11-03")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/vinceliuice/Orchis-theme")
            (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
           "1m1kml068pfnw0zl81khm8d0km5r56ynx29xddawh512a15n5h9b"))
        (modules '((guix build utils)
                   (ice-9 regex)
                   (srfi srfi-26)))
        (snippet
         '(begin
            (for-each
             (lambda (f)
               (let* ((r (make-regexp "\\.scss"))
                      (f* (regexp-substitute #f (regexp-exec r f) 'pre ".css")))
                 (if (file-exists? f*)
                     (delete-file f*))))
             (find-files "." ".*\\.scss"))
            #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list
                          "--dest" (string-append
                                    (assoc-ref %outputs "out")
                                    "/share/themes")
                          "--theme" "all")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (replace 'build (lambda _ (invoke "./parse-sass.sh")))
         (replace 'install
           (lambda* (#:key configure-flags #:allow-other-keys)
             (mkdir-p
              (cadr (or (member "--dest" configure-flags)
                        (member "-d" configure-flags))))
             (apply invoke "./install.sh" configure-flags)
             #t)))))
    (inputs
     (list gtk-engines))
    (native-inputs
     (list ;("coreutils" ,coreutils)
           gtk+ sassc))
    (home-page "https://github.com/vinceliuice/Orchis-theme")
    (synopsis "Material Design theme for a wide range of environments")
    (description "Orchis is a Material Design them for GNOME/GTK based
desktop environments.  It is based on materia-theme and adds more color
variants.")
    (license (list license:gpl3            ; According to COPYING.
                   license:lgpl2.1         ; Some style sheets.
                   license:cc-by-sa4.0)))) ; Some icons

(define-public libmegapixels
  (package
    (name "libmegapixels")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/megapixels-org/libmegapixels")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06q6igyf5m6nd75jlihvr6f5hf0q2b2vb2dcjw91f65c6db5q9jk"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libconfig))
    (home-page "https://gitlab.com/megapixels-org/libmegapixels")
    (synopsis "Library for the Megapixels application")
    (description "This package provides a device abstraction library for the
Megapixels application.")
    (license license:gpl3+)))

(define-public megapixels
  (package
    (name "megapixels")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/megapixels-org/Megapixels")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "006gpkfgwp8gzn5ryvxgh1s9rq9a6fgy4rz4q23k5nxvcf1g8yk5"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((paths (map
                            (lambda (p)
                              (string-append (assoc-ref inputs p) "/bin"))
                            '("coreutils"
                              "imagemagick"
                              "libraw"
                              "perl-image-exiftool"))))
                (wrap-program
                    (string-append #$output "/share/megapixels/postprocess.sh")
                  `("PATH" prefix ,paths))))))))
    (native-inputs
     (list desktop-file-utils           ;for update-desktop-database
           `(,glib "bin")               ;glib-compile-schemas, etc.
           `(,gtk "bin")                ;for gtk-update-icon-cache
           pkg-config))
    (inputs
     (list bash-minimal
           coreutils-minimal
           feedbackd
           gtk
           imagemagick
           libepoxy
           libtiff
           libraw
           perl-image-exiftool
           zbar))
    (synopsis "Camera application for mobile devices")
    (description "This package provides a camera application for mobile
devices that captures a five frames burst of raw frames that are later
post-processed and saved as JPEG files.")
    (home-page "https://gitlab.com/megapixels-org/Megapixels")
    (license license:gpl3+)))

(define-public postmarketos-theme
  (package
    (name "postmarketos-theme")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/postmarketOS/postmarketos-theme")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09in7737cirmw2c0ac40ac29szfgdva6q0zl32mdi12marybd2g5"))))
    (build-system meson-build-system)
    (native-inputs (list sassc))
    (home-page "https://gitlab.com/postmarketOS/postmarketos-theme")
    (synopsis "PostmarketOS themed themes")
    (description
     "@code{postmarketos-theme} contains a GTK3 and GTK4 theme which is based
on Adwaita but replaces the standard blue highlights in the theme with
postmarketOS green.  There's also the oled and paper variants of the theme
that are completely black and completely white.")
    (license license:lgpl2.0+)))

(define-public postmarketos-tweaks
  (package
    (name "postmarketos-tweaks")
    (version "0.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/postmarketOS/postmarketos-tweaks")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "020blf2v588q9g5zq8imcii7iykca7v5an6if6bf9p4fd3yh7ar8"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-install-dir
            (lambda* _
              (substitute* "data/meson.build"
                (("/etc/init.d") (string-append %output "/etc/init.d")))))
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin/")))
                (for-each
                 (lambda (program)
                   (wrap-program (string-append bin program)
                     `("GUIX_PYTHONPATH" =
                       (,(getenv "GUIX_PYTHONPATH")
                        ,(python:site-packages inputs outputs)))
                     `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))
                 (list "pmos-tweaks" "pmos-tweakd" "pk-tweaks-action")))))
          (add-after 'glib-or-gtk-wrap 'python-wrap
            (assoc-ref python:%standard-phases 'wrap)))))
    (native-inputs
     (list desktop-file-utils           ;for update-desktop-database
           `(,gtk+ "bin")               ;for gtk-update-icon-cache
           `(,glib "bin")               ;glib-compile-schemas, etc.
           pkg-config))
    (inputs
     (list bash-minimal
           gtk+
           libhandy
           python
           python-pygobject
           python-pyyaml))
    (home-page "https://gitlab.com/postmarketOS/postmarketos-tweaks")
    (synopsis "Settings configuration utility for postmarketOS")
    (description "postmarketOS tweaks is an application for tweaking settings
on desktop environments supported by postmarketOS.")
    (license license:lgpl3+)))

(define-public eiciel
  (package
    (name "eiciel")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rofirrim/eiciel")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lhnrxhbg80pqjy9f8yiqi7x48rb6m2cmkffv25ssjynsmdnar0s"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:tests? #f ; no tests
      #:configure-flags
      #~(list (string-append "-Dnautilus-extension-dir="
                             #$output
                             "/lib/nautilus/site-extensions"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache : true")
                 "gtk_update_icon_cache : false")))))))
    (native-inputs
     (list gettext-minimal `(,glib "bin") itstool pkg-config))
    (inputs
     (list acl attr glibmm gtkmm nautilus))
    (home-page "https://rofi.roger-ferrer.org/eiciel")
    (synopsis "Manage extended file attributes")
    (description "Eiciel is a plugin for nautilus to graphically edit ACL and
extended file attributes.  It also functions as a standalone command.")
    (license license:gpl2+)))

(define-public markets
  (package
    (name "markets")
    (version "0.5.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/bitstower/markets")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ch6dfmdcpw32r23s58riv8agnyw0f1cqd1y6j7zkx5sb3zyn3zy"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'unpack 'skip-update-desktop-database
           ;; Don't update desktop file database.
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("update-desktop-database") "true")))))))
    (inputs
     (list gtk+
           gettext-minimal
           gsettings-desktop-schemas
           libgee
           libhandy
           libsoup-minimal-2
           json-glib
           vala))
    (native-inputs
     (list pkg-config
           python-wrapper
           `(,glib "bin"))) ; for 'glib-compile-resources'
    (home-page "https://github.com/bitstower/markets")
    (synopsis "Stock, currency and cryptocurrency tracker")
    (description
     "Markets is a GTK application that displays financial data, helping users
track stocks, currencies and cryptocurrencies.")
    (license license:gpl3+)))

(define-public vala-language-server
  (package
    (name "vala-language-server")
    ;; Note to maintainer: VLS must be built with a Vala toolchain the same
    ;; version or newer. Therefore when you update this package you may need
    ;; to update Vala too.
    (version "0.48.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/benwaffle/vala-language-server")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1ini6nd5yim6mql13b9mb15gs02gm08x7zphd0vlv9jxl2646pjn"))))
    (build-system meson-build-system)
    (arguments '(#:glib-or-gtk? #t))
    (inputs
     (list glib json-glib jsonrpc-glib libgee vala))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/benwaffle/vala-language-server")
    (synopsis "Language server for Vala")
    (description "The Vala language server is an implementation of the Vala
language specification for the Language Server Protocol (LSP).  This tool is
used in text editing environments to provide a complete and integrated
feature-set for programming Vala effectively.")
    (license license:lgpl2.1+)))


(define-public yaru-theme
  (package
    (name "yaru-theme")
    (version "22.10.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ubuntu/yaru")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0f052a5cyf4lijyrdp4kjvxrx6d5fbj7109pi2bhxs9lk5jy8z86"))))
    (build-system meson-build-system)
    (native-inputs
     (list python sassc pkg-config `(,glib "bin") `(,gtk+ "bin")))
    (arguments
     (list #:configure-flags #~'("-Dmate=true"
                                 "-Dmate-dark=true"
                                 "-Dxfwm4=true"
                                 "-Dmetacity=true"
                                 "-Dsessions=false")))
    (home-page "https://github.com/ubuntu/yaru")
    (synopsis "Ubuntu community theme yaru")
    (description "Yaru is the default theme for Ubuntu.

It contains:

@itemize
@item a GNOME Shell theme based on the upstream GNOME shell theme
@item a light and dark GTK theme (gtk2 and gtk3) based on the upstream Adwaita
 Gtk theme
@item an icon & cursor theme, derived from the Unity8 Suru icons and Suru icon
 theme
@item a sound theme, combining sounds from the WoodenBeaver and Touch-Remix
 sound themes.
@end itemize")
    (license (list license:lgpl2.1 license:lgpl3 license:cc-by-sa4.0))))

(define-public nordic-theme
  (let ((commit "07d764c5ebd5706e73d2e573f1a983e37b318915")
	(revision "0"))
  (package
   (name "nordic-theme")
   (version (git-version "1.9.0" revision commit))
   (source
     (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/EliverLara/Nordic")
             (commit commit)))
     (sha256
       (base32
         "0y2s9d6h1b195s6afp1gb5rb1plfslkpbw2brd30a9d66wfvsqk0"))
     (file-name (git-file-name name version))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      `(("." "share/themes/nord"
         #:exclude ("README.md" "LICENSE" "Art/" "package.json"
                    "package-lock.json" "Gulpfile.js")))))
   (home-page "https://github.com/EliverLara/Nordic")
   (synopsis "Dark Gtk3.20+ theme using the Nord color palette")
   (description "Nordic is a Gtk3.20+ theme created using the Nord color
palette.")
   (license license:gpl3))))

(define-public tiramisu
  (package
    (name "tiramisu")
    (version "2.0.20211107")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Sweets/tiramisu")
                    (commit version)))
              (sha256
               (base32
                "1n1x1ybbwbanibw7b90k7v4cadagl41li17hz2l8s2sapacvq3mw"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "tiramisu" (string-append out "/bin"))
               #t))))
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target)))))
    (inputs
     (list glib))
    (native-inputs
     (list pkg-config vala))
    (home-page "https://github.com/Sweets/tiramisu")
    (synopsis "Desktop notifications, the UNIX way")
    (description "tiramisu is a notification daemon based on dunst that outputs
notifications to STDOUT in order to allow the user to process notifications any
way they prefer.")
    (license license:expat)))

(define-public libpqmarble
  (package
    (name "libpqmarble")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/raggesilver/marble")
             ;; Tag for v2.0.0 is currently missing, so use commit instead
             (commit "f240b2ec7d5cdacb8fdcc553703420dc5101ffdb")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jx53yadqkcsfk9khkqmapznd8g9xg98wkgkigh964dj6gpp7fx1"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-gtk4-update-icon-cache
                          (lambda _
                            (substitute* "build-aux/meson/postinstall.py"
                              (("gtk-update-icon-cache")
                               "true")))))))
    (native-inputs (list pkg-config
                         vala
                         (list glib "bin")  ;for glib-compile-schemas, etc.
                         desktop-file-utils ;for update-desktop-database
                         gobject-introspection))
    (inputs (list gtk))
    (home-page "https://gitlab.gnome.org/raggesilver/marble")
    (synopsis "Utility library for GNOME apps")
    (description "@code{libpqmarble} is a utility library for GNOME apps.")
    (license license:gpl3+)))

(define-public blackbox-terminal
  (package
    (name "blackbox-terminal")
    (version "0.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/raggesilver/blackbox")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g7n2z0m7jjbn93zvx3ix7ph4mpncwq80cjjc2prp878cksj3g3r"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:configure-flags #~(list "-Dblackbox_is_flatpak=false")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-gtk4-update-icon-cache
                          (lambda _
                            (substitute* "build-aux/meson/postinstall.py"
                              (("gtk-update-icon-cache")
                               "true")))))))
    (native-inputs (list pkg-config
                         vala
                         python
                         desktop-file-utils ;for update-desktop-database
                         (list glib "bin") ;for glib-compile-schemas, etc.
                         gettext-minimal))
    (inputs (list gtk
                  vte-with-sixel
                  json-glib
                  libpqmarble
                  libadwaita
                  pcre2
                  libxml2
                  libgee
                  librsvg))
    (home-page "https://gitlab.gnome.org/raggesilver/blackbox/")
    (synopsis "Beautiful GTK 4 terminal")
    (description
     "@code{blackbox-terminal} is an elegant and customizable terminal for GNOME.")
    (license license:gpl3+)))
