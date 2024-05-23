;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018, 2021, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Andrew Miloradovsky <andrew@interpretmath.pw>
;;; Copyright © 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Hunter Jozwiak <hunter.t.joz@gmail.com>
;;; Copyright © 2023 Ivan Gankevich <igankevich@capybaramail.xyz>
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

(define-module (gnu packages accessibility)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages music)
  #:use-module (gnu packages language)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages java)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb))

(define-public libbraille
  (package
    (name "libbraille")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/" name "/files/" name "/"
                       name "-" version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "05g8r0ypazqn10i7k48iibs8bzc3scdfcnhcykab8j16lhzd27d0"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "bin"))
    (arguments
     `(#:tests? #f                      ; Tests require drivers
       #:configure-flags
       (list "--disable-static"
             "--enable-fake")
       #:phases
       (modify-phases %standard-phases
         ,@(if (this-package-native-input "config")
               `((add-after 'unpack 'update-config-scripts
                   (lambda* (#:key native-inputs inputs #:allow-other-keys)
                     (for-each
                       (lambda (dir)
                         (for-each (lambda (file)
                                     (install-file
                                       (search-input-file
                                         (or native-inputs inputs)
                                         (string-append "/bin/" file)) dir))
                                   '("config.guess" "config.sub")))
                       '("." "libltdl")))))
               '()))))
    (native-inputs
     (append
       (if (or (target-aarch64?)
               (target-ppc64le?)
               (target-riscv64?))
           (list config)
           '())
       (list latex2html pkg-config python-wrapper swig)))
    (inputs
     (list glib gtk+-2 libusb-compat))
    (synopsis "Portable Braille Library")
    (description "Libbraille is a library to easily access Braille displays and
terminals.")
    (home-page "https://libbraille.org")
    (license license:lgpl2.1+)))

(define-public brltty
  (package
    (name "brltty")
    (version "6.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://brltty.app/archive/brltty-" version ".tar.gz"))
       (sha256
        (base32 "1z54rin4zhg3294pq47gamzjy2c56zfkl07rx2qy2khlpyczds0k"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:tests? #f                ; no target
      ;; High parallelism may cause errors such as:
      ;;  ranlib: ./libbrlapi_stubs.a: error reading brlapi_stubs.o: file truncated
      #:parallel-build? #f
      #:configure-flags
      #~(list
         (string-append "--with-libbraille="
                        #$(this-package-input "libbraille"))
         (string-append "--with-espeak_ng="
                        #$(this-package-input "espeak-ng"))
         (string-append "--with-espeak="
                        #$(this-package-input "espeak"))
         (string-append "--with-flite="
                        #$(this-package-input "flite"))
         ;; Required for RUNPATH validation.
         (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
      #:make-flags
      #~(list
         (string-append "JAVA_JAR_DIR=" #$output)
         (string-append "JAVA_JNI_DIR=" #$output)
         (string-append "OCAML_DESTDIR=" #$output "/lib")
         (string-append "PYTHON_PREFIX=" #$output)
         "PYTHON_ROOT=/"
         (string-append "TCL_DIR=" #$output "/lib")
         "INSTALL_WRITABLE_DIRECTORY=no-thanks")
      #:imported-modules `((guix build python-build-system)
                           ,@%glib-or-gtk-build-system-modules)
      #:modules '((guix build utils)
                  (guix build glib-or-gtk-build-system)
                  ((guix build python-build-system) #:prefix python:))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-errors
            (lambda _
              (substitute* "configure"
                (("/sbin/ldconfig")
                 (which "true")))
              ;; Make Python bindings use rpath.
              (substitute* "Bindings/Python/setup.py.in"
                (("extra_compile_args =")
                 (string-append "extra_link_args = ['-Wl,-rpath="
                                #$output
                                "/lib'], "
                                "extra_compile_args = ")))))
          (add-before 'install 'set-pythonpath
            (assoc-ref python:%standard-phases
                       'add-install-to-pythonpath)))))
    (native-inputs
     (append
       (list clisp
             python-cython
             doxygen
             gettext-minimal)
       ;; icedtea doesn't build reliably on all architectures.
       (if (or (target-x86?)
               (target-aarch64?))
           (list `(,icedtea "jdk"))
           '())
       (list ;; ("linuxdoc" ,linuxdoc-tools)
             ocaml
             ocaml-findlib
             pkg-config
             python-wrapper
             tcl)))
    (inputs
     (list alsa-lib
           at-spi2-core
           bluez
           dbus
           espeak
           espeak-ng
           expat
           festival
           flite
           glib
           gpm
           icu4c
           libbraille
           pcre2
           liblouis
           ncurses
           polkit
           speech-dispatcher
           util-linux
           `(,util-linux "lib")
           libx11
           libxaw
           libxaw3d
           libxext
           libxfixes
           libxt
           libxtst))
    (synopsis "Braille TTY")
    (description "BRLTTY is a background process (daemon) which provides access
to the Linux/Unix console (when in text mode) for a blind person using a
refreshable braille display.  It drives the braille display, and provides
complete screen review functionality.  Some speech capability has also been
incorporated.")
    (home-page "https://brltty.app/")
    (license license:lgpl2.1+)))

(define-public florence
  (package
    (name "florence")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/florence/florence/" version
                           "/florence-" version ".tar.bz2"))
       (sha256
        (base32
         "07h9qm22krlwayhzvc391lr23vicw81s48g7rirvx1fj0zyr4aa2"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list "--with-xtst"
                               "--without-docs"
                               "--with-notification")))
    (inputs
     (list libxml2
           libglade
           (librsvg-for-system)
           gstreamer
           cairo
           gtk+
           libxtst
           libxcomposite
           libnotify))
    (native-inputs
     (list gettext-minimal intltool pkg-config))
    (home-page "https://florence.sourceforge.net/")
    (synopsis "Extensible, scalable virtual keyboard for X11")
    (description
     "Florence is an extensible scalable virtual keyboard for X11.
It is useful for people who can't use a real hardware keyboard (for
example for people with disabilities), but you must be able to use
a pointing device (as a mouse, a trackball, a touchscreen or opengazer).

Florence stays out of your way when you don't need it: it appears on the
screen only when you need it.  A timer-based auto-click input method is
available to help to click.")
    ;; The documentation is under FDL1.2, but we do not install the
    ;; documentation.
    (license license:gpl2+)))

(define-public footswitch
  (let ((commit "e455d6752221b9e9c3818cc304c873b9c2792490")
        (revision "0"))
    (package
      (name "footswitch")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rgerganov/footswitch")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xkk60sg3szpgbl3z8djlpagglsldv9viqibsih6wcnbhikzlc6j"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ; no tests
        #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            ;; Install target in the Makefile does not work for Guix.
            (replace 'install
              (lambda _
                (let ((bin (string-append #$output "/bin")))
                  (install-file "footswitch" bin)
                  (install-file "scythe" bin)))))))
      (native-inputs
       (list pkg-config))
      (inputs
       (list hidapi))
      (home-page "https://github.com/rgerganov/footswitch")
      (synopsis "Command line utilities for PCsensor and Scythe foot switches")
      (description
       "This package provides command line utilities for programming PCsensor
and Scythe foot switches.  It works for both single pedal and three pedal
devices.")
      (license license:expat))))

(define-public xmagnify
  (package
    (name "xmagnify")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/amiloradovsky/magnify.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ngnp5f5zl3v35vhbdyjpymy6mwrs0476fm5nd7dzkba7n841jdh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; none included
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list libx11))
    (home-page "https://gitlab.com/amiloradovsky/magnify")
    (synopsis "Tiny screen magnifier for X11")
    (description
     "This program magnifies a screen region by an integer positive factor and
draws the result on a window.  It is useful as an accessibility tool, which
works with every X Window System based GUI (depends only on libX11); or as an
assistant for graphic designers, who need to select individual pixels.")
    ;; Licensed either under Expat or GPLv2+.
    (license (list license:expat license:gpl2+))))

(define-public espeakup
  (package
    (name "espeakup")
    (version "0.90")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linux-speakup/espeakup")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lmjwafvfxy07zn18v3dzjwwpnid2xffgvy2dzlwkbns8gb60ds2"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list espeak-ng alsa-lib))
    (license license:gpl3+)
    (synopsis "Bridge for espeak and speakup")
    (description
     "Espeakup is a bridge between the speakup driver implemented in
the Linux kernel and the espeak-ng text to speech synthesizer.
In order for this package to work, you need to have the following
kernel modules built:
@itemize @bullet
@item
CONFIG_SPEAKUP=m
@item
CONFIG_SPEAKUP_SOFT=m
@end itemize")
    (home-page "https://github.com/linux-speakup/espeakup")))

(define-public mouseloupe
  (package
    (name "mouseloupe")
    (version "0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/" name "/" name "/v" version
                            "/" name "-v" version ".tar.gz"))
        (sha256
         (base32 "0cvdkfakw7cix07j0c4iy10fkbqn6n8l1gr5dd3iy4f2d9bkza43"))
        (snippet
         #~(begin (use-modules (guix build utils))
                  (substitute* "Makefile"
                    (("-D__i386__") ""))))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:tests? #f  ; there are no tests
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'strtof
             (lambda _
               (substitute* "mouseloupe.c"
                 (("\\bstrtof\\b") "mouseloupe_strtof"))))
           (replace 'install
             (lambda _
               (install-file "mouseloupe" (string-append #$output "/bin"))
               (install-file "mouseloupe.1.gz"
                             (string-append #$output "/share/man/man1")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libx11 libxext libxcomposite libxdamage libxrender))
    (synopsis "Screen magnifier tool for people with low vision")
    (description "MouseLoupe is a kind of magnifying glass combined with the
mouse pointer which allows an easy and pleasant web navigation.")
    (home-page "https://sourceforge.net/projects/mouseloupe/")
    (license license:gpl2+)))
