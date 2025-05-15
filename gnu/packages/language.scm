;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Nikita <nikita@n0.is>
;;; Copyright © 2019 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020, 2022 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2022 Milran <milranmike@protonmail.com>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023, 2025 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024 Charles <charles@charje.net>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages language)
  #:use-module (gnu packages)
  #:use-module (gnu packages anthy)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-database)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages dictionaries)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils))

(define-public nimf
  (package
    (name "nimf")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/hamonikr/nimf.git")
         (commit
          (string-append "nimf-" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "01qi7flmaqrn2fk03sa42r0caks9d8lsv88s0bgxahhxwk1x76gc"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "gtk" "qt" "doc"))
    (arguments
     (list
      #:imported-modules `(,@%glib-or-gtk-build-system-modules
                           (guix build cmake-build-system)
                           (guix build qt-build-system)
                           (guix build qt-utils))
      #:modules `(((guix build qt-build-system) #:prefix qt:)
                  ,@%glib-or-gtk-build-system-default-modules)
      #:configure-flags
      #~(list "--with-im-config-data"
              "--with-imsettings-data"
              (string-append "--with-html-dir=" #$output:doc
                             "/share/gtk-doc/html"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-qt4
            (lambda _
              (substitute* '("configure.ac" "modules/clients/Makefile.am")
                (("\\[QtGui\\]")
                 "[Qt5Gui]")
                ((" qt4")
                 ""))))
          (add-after 'disable-qt4 'patch-flags
            (lambda _
              (substitute* "configure.ac"
                (("-Werror")
                 "-Wno-error"))))
          (add-after 'patch-flags 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "configure.ac"
                (("/usr/share/anthy/anthy.dic")
                 (search-input-file inputs "/share/anthy/anthy.dic")))
              (substitute* "configure.ac"
                ;; Do not provide the PATH argument to AC_PATH_PROG; so that
                ;; the needed binaries are looked from PATH (the default
                ;; behavior).
                (("\\[/usr/bin:\\$GTK3_LIBDIR/libgtk-3-0]")
                 "")
                (("\\[/usr/bin:\\$GTK2_LIBDIR/libgtk2.0-0]")
                 "")
                (("\\[/usr/bin:\\$GTK3_LIBDIR/libgtk-3-0:\
\\$GTK2_LIBDIR/libgtk2.0-0]")
                 ""))
              (substitute* "modules/clients/gtk/Makefile.am"
                (("\\$\\(GTK3_LIBDIR\\)")
                 (string-append #$output:gtk "/lib"))
                (("\\$\\(GTK2_LIBDIR\\)")
                 (string-append #$output:gtk "/lib")))
              (substitute* "modules/clients/qt5/Makefile.am"
                (("\\$\\(QT5_IM_MODULE_DIR\\)")
                 (string-append #$output:qt
                                "/lib/qt5/plugins/inputmethods")))
              (substitute* '("bin/nimf-settings/Makefile.am"
                             "data/apparmor-abstractions/Makefile.am"
                             "data/Makefile.am" "data/im-config/Makefile.am"
                             "data/imsettings/Makefile.am")
                (("/etc")
                 (string-append #$output "/etc"))
                (("/usr/share")
                 (string-append #$output "/share")))))
          (add-after 'install 'qt-wrap
            (assoc-ref qt:%standard-phases 'qt-wrap)))))
    (native-inputs
     (list autoconf
           automake
           docbook-xml-4.3
           gettext-minimal
           gobject-introspection
           `(,gtk+-2 "bin")
           `(,gtk+ "bin")
           gtk-doc/stable
           intltool
           libtool
           perl
           pkg-config
           which))
    (inputs
     (list anthy
           libappindicator
           gtk+-2
           gtk+
           libhangul
           m17n-db
           m17n-lib
           qtbase-5
           librime
           (librsvg-for-system)
           wayland
           wayland-protocols
           libx11
           libxkbcommon
           libxklavier))
    (propagated-inputs (list glib))
    (synopsis "Lightweight input method framework")
    (description "Nimf is a lightweight, fast and extensible input method
framework.  This package provides a fork of the original nimf project, that
focuses especially on Korean input (Hangul, Hanja, ...).")
    (home-page "https://github.com/hamonikr/nimf/")
    (license license:lgpl3+)))

(define-public hime
  (package
    (name "hime")
    (version "0.9.11")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/hime-ime/hime.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1wn0ici78x5qh6hvv50bf76ld7ds42hzzl4l5qz34hp8wyvrwakw"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:imported-modules
       (,@%glib-or-gtk-build-system-modules
        (guix build cmake-build-system)
        (guix build qt-build-system)
        (guix build qt-utils))
       #:modules
       (((guix build qt-build-system) #:prefix qt:)
        ,@%glib-or-gtk-build-system-default-modules)
       #:configure-flags
       (list
        ;; FIXME
        ;; error: unknown type name ‘GtkStatusIcon’
        "--disable-system-tray")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-std
           (lambda _
             (substitute* "configure"
               (("gnu17")
                "gnu11")
               (("gnu++17")
                "gnu++11"))
             #t))
         (add-after 'install 'qt-wrap
           (assoc-ref qt:%standard-phases 'qt-wrap)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("whereis" ,util-linux)))
    (inputs
     `(("anthy" ,anthy)
       ("appindicator" ,libappindicator)
       ("chewing" ,libchewing)
       ("gtk+" ,gtk+)
       ("qtbase" ,qtbase-5)
       ("xtst" ,libxtst)))
    (synopsis "HIME Input Method Editor")
    (description "Hime is an extremely easy-to-use input method framework.  It
is lightweight, stable, powerful and supports many commonly used input methods,
including Cangjie, Zhuyin, Dayi, Ranked, Shrimp, Greek, Anthy, Korean, Latin,
Random Cage Fighting Birds, Cool Music etc.")
    (home-page "https://hime-ime.github.io/")
    (license (list license:gpl2+ license:lgpl2.1+
                   license:fdl1.2+)))) ; documentation

(define-public libchewing
  (package
    (name "libchewing")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chewing/libchewing")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gh64wvrk5pn0fhmpvj1j99d5g7f7697rk96zbkc8l72yjr819z5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:modules ((guix build cargo-build-system)
                  (guix build utils)
                  ((guix build cmake-build-system) #:prefix cmake:))
       #:imported-modules ((guix build cmake-build-system)
                           ,@%cargo-build-system-modules)
       #:install-source? #f
       ;; Keep the vendor-dir outside of cmake's directories.
       #:vendor-dir "../guix-vendor"
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-mangen" ,rust-clap-mangen-0.2)
        ("rust-der" ,rust-der-0.7)
        ("rust-directories" ,rust-directories-5)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-rusqlite" ,rust-rusqlite-0.29))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'cmake-configure
           (lambda args
             (apply (assoc-ref cmake:%standard-phases 'configure)
                    ;; For the tests.
                    (append args (list #:out-of-source? #f)))))
         (add-after 'unpack 'work-around-genkeystroke
           (lambda _
             ;; Remove this phase when we can find ncurses with cmake.
             (substitute* "tests/CMakeLists.txt"
               (("CURSES_FOUND") "FALSE"))))
         (replace 'build
           (assoc-ref cmake:%standard-phases 'build))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys #:rest args)
             (when tests?
               ((assoc-ref cmake:%standard-phases 'check)))))
         (replace 'install
           (assoc-ref cmake:%standard-phases 'install)))))
    (native-inputs
     (list corrosion cmake-minimal))
    (inputs
     (list ncurses sqlite))
    (synopsis "Chinese phonetic input method")
    (description "Chewing is an intelligent phonetic (Zhuyin/Bopomofo) input
method, one of the most popular choices for Traditional Chinese users.")
    (home-page "https://chewing.im/")
    (license license:lgpl2.1+)))

(define-public liblouis
  (package
    (name "liblouis")
    (version "3.31.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/liblouis/liblouis")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02bga2l4jiyrgfqdl27wszz5yd6h80n2dmq3p6nb2br83jywisfh"))))
    (build-system gnu-build-system)
    (outputs '("out" "bin" "doc" "python"))
    (arguments
     (list
      #:configure-flags #~(list "--disable-static" "--enable-ucs4")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-python-lib-path
            (lambda _
              (with-directory-excursion "python"
                (substitute* "louis/__init__.py.in"
                  (("###LIBLOUIS_SONAME###")
                   (string-append #$output "/lib/###LIBLOUIS_SONAME###"))))))
          (add-after 'install 'install-python-extension
            (lambda _
              (with-directory-excursion "python"
                (invoke "python3" "setup.py" "install" "--root=/"
                        (string-append "--prefix=" #$output:python))))))))
    (native-inputs
     (list autoconf
           automake
           clang
           help2man
           libtool
           libyaml
           texinfo
           perl
           pkg-config
           python-minimal))
    (synopsis "Braille translator and back-translator")
    (description "Liblouis is a braille translator and back-translator named in
honor of Louis Braille.  It features support for computer and literary braille,
supports contracted and uncontracted translation for many languages and has
support for hyphenation.  New languages can easily be added through tables that
support a rule- or dictionary based approach.  Tools for testing and debugging
tables are also included.  Liblouis also supports math braille, Nemeth and
Marburg.")
    (home-page "http://liblouis.org/")
    (license (list license:lgpl2.1+     ; library
                   license:gpl3+))))    ; tools

(define-public liblouisutdml
  ;; Use the latest commit, which includes test suite fixes not yet released.
  (let ((commit "00ca7838e30ebd5ed6f635236aa235e2c8f089c1")
        (revision "0"))
    (package
      (name "liblouisutdml")
      (version (git-version "2.12.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/liblouis/liblouisutdml")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1pr3wys48bzblr6kav24gr8slsp409f81iqxw19922k24y5y31l7"))))
      (build-system gnu-build-system)
      (outputs '("out" "bin" "doc"))
      (arguments
       (list #:configure-flags
             #~(list "--disable-static")))
      (native-inputs
       (list autoconf
             automake
             help2man
             `(,icedtea "jdk")
             libtool
             texinfo
             pkg-config))
      (inputs
       (list libxml2))
      (propagated-inputs
       (list liblouis
             `(,liblouis "bin")))
      (synopsis "Braille transcription services")
      (description "Liblouisutdml is a library providing complete braille
transcription services for xml, html and text documents.  It translates into
appropriate braille codes and formats according to its style sheet and the
specifications in the document.")
      (home-page "http://liblouis.org/")
      (license (list license:lgpl3+       ; library
                     license:gpl3+)))))    ; tools

(define-public libstemmer
  (package
    (name "libstemmer")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://snowballstem.org/dist/libstemmer_c-"
                           version ".tar.gz"))
       (sha256
        (base32 "1hvphdl8pfq1q3cgh7bshsabsxc7id6wswrqilplwszkkkzdjhdr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests exist
       #:make-flags
       (list
        (string-append "CC=" ,(cc-for-target))
        "CFLAGS=-fPIC")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin"))
                    (out-include (string-append out "/include"))
                    (out-lib (string-append out "/lib")))
               (install-file "stemwords" out-bin)
               (install-file "include/libstemmer.h" out-include)
               (install-file "libstemmer.a" out-lib)))))))
    (synopsis "Stemming Library")
    (description "LibStemmer provides stemming library, supporting several
languages.")
    (home-page "https://snowballstem.org/")
    (properties
     '((release-monitoring-url . "https://snowballstem.org/download.html")
       (upstream-name . "libstemmer_c")))
    (license license:bsd-3)))

(define-public perl-lingua-en-findnumber
  (package
    (name "perl-lingua-en-findnumber")
    (version "1.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Lingua-EN-FindNumber-" version ".tar.gz"))
       (sha256
        (base32
         "015ximzdp42v824llwlg2pd77vd0d172lb4xs55q9f9zhqf6s5qx"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-lingua-en-words2nums))
    (home-page "https://metacpan.org/release/Lingua-EN-FindNumber")
    (synopsis "Locate (written) numbers in English text")
    (description "This module provides a regular expression for finding
numbers in English text.  It also provides functions for extracting and
manipulating such numbers.")
    (license license:perl-license)))

(define-public perl-lingua-en-inflect
  (package
    (name "perl-lingua-en-inflect")
    (version "1.903")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DC/DCONWAY/"
                           "Lingua-EN-Inflect-" version ".tar.gz"))
       (sha256
        (base32
         "0j8d1f1wvmgc11d71pc8xp8fv5a1nb2yfw1dgd19xhscn1klpvzw"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build))
    (home-page "https://metacpan.org/release/Lingua-EN-Inflect")
    (synopsis "Convert singular to plural")
    (description "Lingua::EN::Inflect provides plural inflections,
\"a\"/\"an\" selection for English words, and manipulation of numbers as
words.  Plural forms of all nouns, most verbs, and some adjectives are
provided.  Where appropriate, \"classical\" variants (for example: \"brother\"
-> \"brethren\", \"dogma\" -> \"dogmata\", etc.) are also provided.")
    (license license:perl-license)))

(define-public perl-lingua-en-inflect-number
  (package
    (name "perl-lingua-en-inflect-number")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Lingua-EN-Inflect-Number-" version ".tar.gz"))
       (sha256
        (base32
         "1gxccynkaqav43ww43jp4rzkyr36x97jd03yb5f6yx0jhn1k7yv6"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-lingua-en-inflect))
    (home-page "https://metacpan.org/release/Lingua-EN-Inflect-Number")
    (synopsis "Force number of words to singular or plural")
    (description "This module extends the functionality of Lingua::EN::Inflect
with three new functions for determining plurality of a word and forcefully
converting a word to singular or plural.")
    (license license:perl-license)))

(define-public perl-lingua-en-inflect-phrase
  (package
    (name "perl-lingua-en-inflect-phrase")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RK/RKITOVER/"
                           "Lingua-EN-Inflect-Phrase-" version ".tar.gz"))
       (sha256
        (base32
         "1a6y1l2pjim2242wcpgz066di4pbzfgsjjdl7vg5a5wzm48qj1am"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-nowarnings))
    (propagated-inputs
     (list perl-lingua-en-findnumber perl-lingua-en-inflect
           perl-lingua-en-inflect-number perl-lingua-en-number-isordinal
           perl-lingua-en-tagger))
    (home-page "https://metacpan.org/release/Lingua-EN-Inflect-Phrase")
    (synopsis "Inflect short English phrases")
    (description "This module attempts to pluralize or singularize short
English phrases.")
    (license license:perl-license)))

(define-public perl-lingua-en-number-isordinal
  (package
    (name "perl-lingua-en-number-isordinal")
    (version "0.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RK/RKITOVER/"
                           "Lingua-EN-Number-IsOrdinal-" version ".tar.gz"))
       (sha256
        (base32
         "1mhqjvh2ad30gjab5b3a6mbr4aysyrscp4wp42yy5x6001a6km98"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-try-tiny perl-test-fatal))
    (propagated-inputs
     (list perl-lingua-en-findnumber))
    (home-page "https://metacpan.org/release/Lingua-EN-Number-IsOrdinal")
    (synopsis "Detect if English number is ordinal or cardinal")
    (description "This module will tell you if a number, either in words or as
digits, is a cardinal or ordinal number.")
    (license license:perl-license)))

(define-public perl-lingua-en-tagger
  (package
    (name "perl-lingua-en-tagger")
    (version "0.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AC/ACOBURN/"
                           "Lingua-EN-Tagger-" version ".tar.gz"))
       (sha256
        (base32
         "0nrnkvsf9f0a7lp82sanmy89ms2nqq1lvjqicvsagsvzp513bl5b"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-memoize-expirelru perl-lingua-stem perl-html-parser
           perl-html-tagset))
    (home-page "https://metacpan.org/release/Lingua-EN-Tagger")
    (synopsis "Part-of-speech tagger for English natural language processing")
    (description "This module is a probability based, corpus-trained tagger
that assigns part-of-speech tags to English text based on a lookup dictionary
and a set of probability values.  The tagger assigns appropriate tags based on
conditional probabilities - it examines the preceding tag to determine the
appropriate tag for the current word.  Unknown words are classified according
to word morphology or can be set to be treated as nouns or other parts of
speech.  The tagger also extracts as many nouns and noun phrases as it can,
using a set of regular expressions.")
    (license license:gpl3)))

(define-public perl-lingua-en-words2nums
  (package
    (name "perl-lingua-en-words2nums")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JO/JOEY/"
                           "Lingua-EN-Words2Nums-" version ".tar.gz"))
       (sha256
        (base32
         "118xx8qr1zbx30psv7ic55w65h15mc1vz6zicshfm96jgiwmcrb8"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Lingua-EN-Words2Nums")
    (synopsis "Convert English text to numbers")
    (description "This module converts English text into numbers.  It supports
both ordinal and cardinal numbers, negative numbers, and very large numbers.")
    (license license:perl-license)))

(define-public perl-lingua-pt-stemmer
  (package
    (name "perl-lingua-pt-stemmer")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Lingua-PT-Stemmer-" version ".tar.gz"))
       (sha256
        (base32
         "17c48sfbgwd2ivlgf59sr6jdhwa3aim8750f8pyzz7xpi8gz0var"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Lingua-PT-Stemmer")
    (synopsis "Portuguese language stemming")
    (description "This module implements a Portuguese stemming algorithm
proposed in the paper A Stemming Algorithm for the Portuguese Language by
Moreira, V. and Huyck, C.")
    (license license:perl-license)))

(define-public perl-lingua-stem
  (package
    (name "perl-lingua-stem")
    (version "0.84")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SN/SNOWHARE/"
                           "Lingua-Stem-" version ".tar.gz"))
       (sha256
        (base32
         "12avh2mnnc7llmmshrr5bgb473fvydxnlqrqbl2815mf2dp4pxcg"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-lingua-pt-stemmer
           perl-lingua-stem-fr
           perl-lingua-stem-it
           perl-lingua-stem-ru
           perl-lingua-stem-snowball-da
           perl-snowball-norwegian
           perl-snowball-swedish
           perl-text-german))
    (home-page "https://metacpan.org/release/Lingua-Stem")
    (synopsis "Stemming of words in various languages")
    (description "This routine applies stemming algorithms to its parameters,
returning the stemmed words as appropriate to the selected locale.")
    (license license:perl-license)))

(define-public perl-lingua-stem-fr
  (package
    (name "perl-lingua-stem-fr")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SD/SDP/"
                           "Lingua-Stem-Fr-" version ".tar.gz"))
       (sha256
        (base32
         "0vyrspwzaqjxm5mqshf4wvwa3938mkajd1918d9ii2l9m2rn8kwx"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Lingua-Stem-Fr")
    (synopsis "Porter's stemming algorithm for French")
    (description "This module uses a modified version of the Porter Stemming
Algorithm to return a stemmed French word.")
    (license license:perl-license)))

(define-public perl-lingua-stem-it
  (package
    (name "perl-lingua-stem-it")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AC/ACALPINI/"
                           "Lingua-Stem-It-" version ".tar.gz"))
       (sha256
        (base32
         "1207r183s5hlh4mfwa6p46vzm0dhvrs2dnss5s41a0gyfkxp7riq"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Lingua-Stem-It")
    (synopsis "Porter's stemming algorithm for Italian")
    (description "This module applies the Porter Stemming Algorithm to its
parameters, returning the stemmed Italian word.")
    (license license:perl-license)))

(define-public perl-lingua-stem-ru
  (package
    (name "perl-lingua-stem-ru")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Lingua-Stem-Ru-" version ".tar.gz"))
       (sha256
        (base32
         "0a2jmdz7jn32qj5hyiw5kbv8fvlpmws8i00a6xcbkzb48yvwww0j"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Lingua-Stem-Ru")
    (synopsis "Porter's stemming algorithm for Russian")
    (description "This module applies the Porter Stemming Algorithm to its
parameters, returning the stemmed Russian (KOI8-R only) word.")
    (license license:perl-license)))

(define-public perl-lingua-stem-snowball-da
  (package
    (name "perl-lingua-stem-snowball-da")
    (version "1.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CI/CINE/"
                           "Lingua-Stem-Snowball-Da-" version ".tar.gz"))
       (sha256
        (base32
         "0mm0m7glm1s6i9f6a78jslw6wh573208arxhq93yriqmw17bwf9f"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Lingua-Stem-Snowball-Da")
    (synopsis "Porters stemming algorithm for Danish")
    (description "Lingua::Stem::Snowball::Da is a perl port of the danish
stemmer at http://snowball.sourceforge.net, it was originally altered from the
Lingua::Stem::Snowball::Se.")
    (license license:gpl2)))

(define-public perl-snowball-norwegian
  (package
    (name "perl-snowball-norwegian")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AS/ASKSH/"
                           "Snowball-Norwegian-" version ".tar.gz"))
       (sha256
        (base32
         "0675v45bbsh7vr7kpf36xs2q79g02iq1kmfw22h20xdk4rzqvkqx"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build))
    (home-page "https://metacpan.org/release/Snowball-Norwegian")
    (synopsis "Porters stemming algorithm for Norwegian")
    (description "Lingua::Stem::Snowball::No is a perl port of the norwegian
stemmer at http://snowball.tartarus.org.")
    (license license:perl-license)))

(define-public perl-snowball-swedish
  (package
    (name "perl-snowball-swedish")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AS/ASKSH/"
                           "Snowball-Swedish-" version ".tar.gz"))
       (sha256
        (base32
         "0agwc12jk5kmabnpsplw3wf4ii5w1zb159cpin44x3srb0sr5apg"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build))
    (home-page "https://metacpan.org/release/Snowball-Swedish")
    (synopsis "Porters stemming algorithm for Swedish")
    (description "Lingua::Stem::Snowball::Se is a perl port of the swedish
stemmer at http://snowball.sourceforge.net.")
    (license license:perl-license)))

(define-public perl-string-toidentifier-en
  (package
    (name "perl-string-toidentifier-en")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RK/RKITOVER/"
                           "String-ToIdentifier-EN-" version ".tar.gz"))
       (sha256
        (base32
         "12nw7h2yiybhdw0vnnpc7bif8ylhsn6kqf6s39dsrf9h54iq9yrs"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-lingua-en-inflect-phrase perl-text-unidecode
           perl-namespace-clean))
    (home-page "https://metacpan.org/release/String-ToIdentifier-EN")
    (synopsis "Convert strings to English program identifiers")
    (description "This module provides a utility method, \"to_identifier\" for
converting an arbitrary string into a readable representation using the ASCII
subset of \"\\w\" for use as an identifier in a computer program.  The intent
is to make unique identifier names from which the content of the original
string can be easily inferred by a human just by reading the identifier.")
    (license license:perl-license)))

(define-public perl-text-german
  (package
    (name "perl-text-german")
    (version "0.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/U/UL/ULPFR/"
                           "Text-German-" version ".tar.gz"))
       (sha256
        (base32
         "1p87pgap99lw0nv62i3ghvsi7yg90lhn8vsa3yqp75rd04clybcj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Text-German")
    (synopsis "German grundform reduction")
    (description "This module is a rather incomplete implementation of work
done by Gudrun Putze-Meier.")
    (license license:perl-license)))

(define-public link-grammar
  (package
    (name "link-grammar")
    (version "5.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.abisource.com/downloads/"
                                  "link-grammar/" version
                                  "/link-grammar-" version ".tar.gz"))
              (sha256
               (base32
                "0ak1v469k56v3511kxxkxvx1nw6zcxcl0f1kcvc82ffacqbr4y96"))))
    (build-system gnu-build-system)
    (home-page "https://www.abisource.com/projects/link-grammar/")
    (synopsis "Link grammar parser")
    (description "The Link Grammar Parser is a syntactic parser of English,
Russian, Arabic and Persian (and other languages as well), based on Link
Grammar, an original theory of syntax and morphology.  Given a sentence, the
system assigns to it a syntactic structure, which consists of a set of
labelled links connecting pairs of words.  The parser also produces a
\"constituent\" (HPSG style phrase tree) representation of a sentence (showing
noun phrases, verb phrases, etc.).")
    (license license:bsd-3)))

(define-public praat
  (package
    (name "praat")
    (version "6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/praat/praat")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rabv4175r1kbgb6n5xbir4j9ldpfr3wr6xa8jakzgny3dwlmsbg"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "CC="
                                               #$(cc-for-target)))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'delete-failing-tests
                          (lambda _
                            (delete-file "test/sys/graphicsText.praat")))
                        (replace 'configure
                          (lambda _
                            (copy-file "makefiles/makefile.defs.linux.pulse"
                                       "makefile.defs")))
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "./praat" "--run"
                                      "test/runAllTests_batch.praat"))))
                        (replace 'install
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let* ((out (assoc-ref outputs "out"))
                                   (bin (string-append out "/bin")))
                              (mkdir-p bin)
                              (copy-file "praat"
                                         (string-append bin "/praat"))))))))
    (inputs (list alsa-lib gtk+ jack-1 pulseaudio))
    (native-inputs (list pkg-config))
    (home-page "https://www.fon.hum.uva.nl/praat/")
    (synopsis "Doing phonetics by computer")
    (description
     "Praat is a tool to perform phonetics tasks.  It can do speech
analysis (pitch, formant, intensity, ...), speech synthesis, labelling, segmenting
and manipulation.")
    (license license:gpl2+)))

(define-public libskk
  (package
    (name "libskk")
    (version "1.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ueno/libskk")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y279pcgs3jrsi9vzx086xhz9jbz23dqqijp4agygc9ackp9sxy5"))
              (patches
               (search-patches
                "libskk-fix-invalid-escape.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:parallel-tests? #f        ;Concurrency issues in tests.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'symlink-skk-jisyo
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((dict-dir "share/skk"))
                     (symlink (search-input-directory inputs dict-dir)
                              (in-vicinity #$output dict-dir))))))))
    (native-inputs (list autoconf
                         automake
                         gettext-minimal
                         gobject-introspection
                         libtool
                         pkg-config
                         vala))
    (inputs (list libgee json-glib libxkbcommon skk-jisyo))
    (home-page "https://github.com/ueno/libskk")
    (synopsis "Dealing with Japanese kana-to-kanji conversion")
    (description
     "libskk is a library to deal with Japanese kana-to-kanji conversion method.")
    (license license:gpl3+)))

(define-public skktools
  (package
    (name "skktools")
    (version "1.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/skk-dev/skktools")
                    (commit (string-append "skktools-"
                                           (string-replace-substring version
                                                                     "." "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zway8jsm18279xq8zlpr84iqiw373x3v0ysay74n9bjqxbl234a"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Maybe requires jgawk
                          (delete-file "unannotation.awk")
                          (delete-file "convert2skk/edict2skk.awk")
                          (delete-file "convert2skk/wnn2skk.awk")
                          (delete-file "convert2skk/wnn2skk.sed") ;Used with wnn2skk.awk
                          (delete-file "convert2skk/wnn2skk.sh") ;Depends on 2 files above
                          ;; Requires jperl
                          (delete-file "convert2skk/alpha-kana.pl")
                          (delete-file "convert2skk/atok2skk.pl")
                          (delete-file "convert2skk/read.me") ;Readme for jperl scripts
                          (delete-file "convert2skk/wx2skk.pl")
                          (delete-file-recursively "dbm")
                          ;; Needs a lot requirements
                          (delete-file "convert2skk/doc2skk.sh")
                          ;; Obsolete scripts
                          (delete-file-recursively "convert2skk/obsolete")
                          ;; Contains syntax error
                          (delete-file "convert2skk/pubdic2list")))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:modules ((guix build gnu-build-system)
                  ((guix build emacs-build-system)
                   #:prefix emacs:)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%default-gnu-imported-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'fix-library-loading
                    (lambda* (#:key outputs #:allow-other-keys)
                      (for-each (lambda (path)
                                  (substitute* path
                                    (("require 'skkdictools'")
                                     "require_relative 'skkdictools'")))
                                (list "filters/annotation-filter.rb"
                                      "filters/asayaKe.rb"
                                      "filters/complete-numerative.rb"
                                      "filters/conjugation.rb"
                                      "filters/make-tankan-dic.rb"))))
                  (add-after 'install 'install-scripts
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((share (string-append (assoc-ref outputs "out")
                                                  "/share/skktools")))
                        (install-file "filters/skkdictools.rb" share)
                        (for-each (lambda (file)
                                    (invoke "chmod" "755" file)
                                    (install-file file share))
                                  (append (find-files "." "\\.rb$")
                                          (find-files "." "\\.scm$")
                                          (find-files "." "\\.py$")
                                          (find-files "convert2skk" "\\.pl")
                                          (find-files "convert2skk" "\\.rb")
                                          (list "convert2skk/adddummy"
                                                "convert2skk/list2skk"
                                                "convert2skk/removedummy"
                                                "convert2skk/skk2list")
                                          (find-files "filters" "\\.rb$"))))))
                  ;; Install and make autoloads for skk-xml.el.
                  (add-after 'unpack 'make-autoloads
                    (assoc-ref emacs:%standard-phases
                               'make-autoloads))
                  (add-after 'install 'install-emacs-files
                    (assoc-ref emacs:%standard-phases
                               'install))
                  (add-after 'install-emacs-files 'compile-emacs-files
                    (assoc-ref emacs:%standard-phases
                               'build))
                  (add-after 'install 'install-docs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((doc (string-append (assoc-ref outputs "out")
                                                "/share/doc/"
                                                ,name "-"
                                                ,version)))
                        (install-file "ChangeLog" doc)
                        (for-each (lambda (file)
                                    (install-file file doc))
                                  (append (find-files "READMEs")))
                        (copy-file "filters/README.md"
                                   (string-append doc "/README.filters.md"))
                        (copy-file "convert2skk/README.md"
                                   (string-append doc "/README.convert2skk.md")))))
                  (add-after 'install-scripts 'check-scripts
                    ;; Skipped tests for:
                    ;; * skk2cdb.py: Requires cdb file
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (string-append (assoc-ref outputs "out")
                                                "/share/skktools")))
                        (for-each (lambda (args)
                                    (apply invoke
                                           (string-append out "/"
                                                          (car args))
                                           (cdr args)))
                                  '(("abbrev-convert.rb")
                                    ("abbrev-simplify-keys.rb")
                                    ("adddummy")
                                    ("annotation-filter.rb")
                                    ("aozora2skk.rb")
                                    ("asayaKe.rb")
                                    ("canna2skk.rb" "/dev/null")
                                    ("chasen2skk.rb")
                                    ("complete-numerative.rb")
                                    ("conjugation.rb")
                                    ("ctdicconv.rb")
                                    ("dic-it2skk.rb" "/dev/null")
                                    ("ipadic2skk.rb")
                                    ("list2skk")
                                    ("make-tankan-dic.rb")
                                    ("prime2skk.rb")
                                    ("removedummy")
                                    ("saihenkan.rb")
                                    ("skk2list")
                                    ("/skkdic-diff.scm" "/dev/null"
                                     "/dev/null")
                                    ("skk-wordpicker.rb")))))))))
    (native-inputs (list
                    ;; for skkdic-expr2
                    pkg-config
                    ;; for installing Emacs Lisp files
                    emacs-minimal))
    (inputs (list bdb-4.8
                  glib ;for skkdic-expr2
                  ;; For scripts
                  gauche
                  perl
                  python-2
                  ruby))
    (home-page "https://github.com/skk-dev/skktools")
    (synopsis "SKK dictionary maintenance tools")
    (description
     "The skktools are SKK dictionary maintenance tools.  This includes
commands such as @command{skkdic-count}, @command{skkdic-expr}, and
@command{skkdic-sort}.")
    (license license:gpl2+)))

(define-public mecab
  (package
    (name "mecab")
    (version "0.996")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/taku910/mecab")
                     ;; latest commit
                     (commit "046fa78b2ed56fbd4fac312040f6d62fc1bc31e3")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hdv7rgn8j0ym9gsbigydwrbxa8cx2fb0qngg1ya15vvbw0lk4aa"))
              (patches
                (search-patches
                  "mecab-variable-param.patch"))))
    (build-system gnu-build-system)
    (native-search-paths
      (list (search-path-specification
              (variable "MECAB_DICDIR")
              (separator #f)
              (files '("lib/mecab/dic")))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "mecab")))
         (add-before 'build 'add-mecab-dicdir-variable
           (lambda _
             (substitute* "mecabrc.in"
               (("dicdir = .*")
                "dicdir = $MECAB_DICDIR"))
             (substitute* "mecab-config.in"
               (("echo @libdir@/mecab/dic")
                "if [ -z \"$MECAB_DICDIR\" ]; then
  echo @libdir@/mecab/dic
else
  echo \"$MECAB_DICDIR\"
fi")))))))
    (inputs (list libiconv))
    (home-page "https://taku910.github.io/mecab")
    (synopsis "Morphological analysis engine for texts")
    (description "Mecab is a morphological analysis engine developed as a
collaboration between the Kyoto university and Nippon Telegraph and Telephone
Corporation.  The engine is independent of any language, dictionary or corpus.")
    (license (list license:gpl2+ license:lgpl2.1+ license:bsd-3))))

(define-public mecab-ipadic
  (package
    (name "mecab-ipadic")
    (version "2.7.0")
    (source (package-source mecab))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-dicdir=" (assoc-ref %outputs "out")
                            "/lib/mecab/dic")
             "--with-charset=utf8")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "mecab-ipadic")))
         (add-before 'configure 'set-mecab-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "MECAB_DICDIR" (string-append (assoc-ref outputs "out")
                                                   "/lib/mecab/dic")))))))
    (native-inputs (list mecab)); for mecab-config
    (home-page "https://taku910.github.io/mecab")
    (synopsis "Dictionary data for MeCab")
    (description "This package contains dictionary data derived from
ipadic for use with MeCab.")
    (license (license:non-copyleft "mecab-ipadic/COPYING"))))

(define-public mecab-unidic
  (package
    (name "mecab-unidic")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://clrd.ninjal.ac.jp/unidic_archive/cwj/"
                                  version "/unidic-cwj-" version ".zip"))
              (sha256
               (base32
                "1z132p2q3bgchiw529j2d7dari21kn0fhkgrj3vcl0ncg2m521il"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." "lib/mecab/dic"
          #:include-regexp ("\\.bin$" "\\.def$" "\\.dic$" "dicrc")))))
    (native-inputs (list unzip))
    (home-page "https://clrd.ninjal.ac.jp/unidic/en/")
    (synopsis "Dictionary data for MeCab")
    (description "UniDic for morphological analysis is a dictionary for
analysis with the morphological analyser MeCab, where the short units exported
from the database are used as entries (heading terms).")
    ;; triple-licensed (at the user’s choice)
    (license (list license:gpl2+ license:lgpl2.1 license:bsd-3))))

(define-public dparser
  (package
    (name "dparser")
    (version "1.33a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jplevyak/dparser/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vzfi7d573qsmfxkgnzqkalhv06i2zc8hm0pwcgrgj8382g01zg1"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list
                           (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))
           #:test-target "test"))
    (synopsis "Scannerless GLR parser generator")
    (description
     "DParser is scannerless GLR parser generator.  The form of the text to be
parsed can be specified using a combination of regular expressions and grammar
productions.  Because of the parsing technique, a scannerless GLR parser based
on the Tomita algorithm the grammar can be ambiguous, right or left recursive,
have any number of null productions, and because there is no separate
tokenizer, can include whitespace in terminals and have terminals which are
prefixes of other terminals.")
    (home-page "https://dparser.sourceforge.net/")
    (license (list license:bsd-3))))
