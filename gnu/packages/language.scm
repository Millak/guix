;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Nikita <nikita@n0.is>
;;; Copyright © 2019 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
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
     `(#:imported-modules
       (,@%glib-or-gtk-build-system-modules
        (guix build cmake-build-system)
        (guix build qt-build-system)
        (guix build qt-utils))
       #:modules
       ((guix build glib-or-gtk-build-system)
        ((guix build qt-build-system)
         #:prefix qt:)
        (guix build utils))
       #:configure-flags
       (list
        "--with-im-config-data"
        "--with-imsettings-data"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-qt4
           (lambda _
             (substitute* '("configure.ac" "modules/clients/Makefile.am")
               (("\\[QtGui\\]")
                "[Qt5Gui]")
               ((" qt4")
                ""))
             #t))
         (add-after 'disable-qt4 'patch-flags
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure.ac"
               (("-Werror")
                "-Wno-error"))
             #t))
         (add-after 'patch-flags 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "nimf-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml-4.3")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'patch-docbook-xml 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "configure.ac"
               (("/usr/share/anthy/anthy.dic")
                (search-input-file inputs "/share/anthy/anthy.dic")))
             (substitute* "configure.ac"
               (("/usr/bin:\\$GTK3_LIBDIR/libgtk-3-0")
                (string-append (assoc-ref inputs "gtk+:bin")
                               "/bin:$GTK3_LIBDIR/libgtk-3-0"))
               (("/usr/bin:\\$GTK2_LIBDIR/libgtk2.0-0")
                (string-append (assoc-ref inputs "gtk+-2:bin")
                               "/bin:$GTK2_LIBDIR/libgtk2.0-0")))
             (substitute* "modules/clients/gtk/Makefile.am"
               (("\\$\\(GTK3_LIBDIR\\)")
                (string-append (assoc-ref outputs "gtk")
                               "/lib"))
               (("\\$\\(GTK2_LIBDIR\\)")
                (string-append (assoc-ref outputs "gtk")
                               "/lib")))
             (substitute* "modules/clients/qt5/Makefile.am"
               (("\\$\\(QT5_IM_MODULE_DIR\\)")
                (string-append (assoc-ref outputs "qt")
                               "/lib/qt5/plugins/inputmethods")))
             (substitute* '("bin/nimf-settings/Makefile.am"
                            "data/apparmor-abstractions/Makefile.am"
                            "data/Makefile.am" "data/im-config/Makefile.am"
                            "data/imsettings/Makefile.am")
               (("/etc")
                (string-append (assoc-ref outputs "out")
                               "/etc"))
               (("/usr/share")
                (string-append (assoc-ref outputs "out")
                               "/share")))
             #t))
         (add-after 'install 'qt-wrap
           (assoc-ref qt:%standard-phases 'qt-wrap)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("docbook-xml-4.3" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+-2:bin" ,gtk+-2 "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("gtk-doc" ,gtk-doc/stable)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("anthy" ,anthy)
       ("appindicator" ,libappindicator)
       ("gtk+-2" ,gtk+-2)
       ("gtk+" ,gtk+)
       ("hangul" ,libhangul)
       ("m17n-db" ,m17n-db)
       ("m17n-lib" ,m17n-lib)
       ("qtbase" ,qtbase-5)
       ("rime" ,librime)
       ("rsvg" ,librsvg)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ("x11" ,libx11)
       ("xkbcommon" ,libxkbcommon)
       ("xklavier" ,libxklavier)))
    (propagated-inputs
     (list glib))
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
       ((guix build glib-or-gtk-build-system)
        ((guix build qt-build-system)
         #:prefix qt:)
        (guix build utils))
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
    (home-page "http://hime-ime.github.io/")
    (license (list license:gpl2+ license:lgpl2.1+
                   license:fdl1.2+)))) ; documentation

(define-public libchewing
  (package
    (name "libchewing")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/chewing/libchewing.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "04d09w6xdd08v6laj9y4qmqsijw5i2jvshcilhh4vg6cfnfgl2my"))))
    (build-system gnu-build-system)
    (arguments
     `(;; test-easy-symbol and test-fullshape fail with multiple cores.
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "test/Makefile.am"
               (("	test-bopomofo ")
                "")
               (("	test-config ")
                "")
               (("	test-reset ")
                "")
               (("	test-symbol ")
                "")
               (("	test-keyboardless ")
                "")
               (("	test-special-symbol ")
                "")
               (("	test-keyboard ")
                "")
               (("	test-regression ")
                "")
               (("	test-userphrase ")
                ""))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("texinfo" ,texinfo)))
    (inputs
     (list ncurses sqlite))
    (synopsis "Chinese phonetic input method")
    (description "Chewing is an intelligent phonetic (Zhuyin/Bopomofo) input
method, one of the most popular choices for Traditional Chinese users.")
    (home-page "http://chewing.im/")
    (license license:lgpl2.1+)))

(define-public liblouis
  (package
    (name "liblouis")
    (version "3.15.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/liblouis/liblouis")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ljy5xsy7vf2r0ix0d7bqcr6qvr6897f8madsx9zlm1mrj31n5px"))))
    (build-system gnu-build-system)
    (outputs '("out" "bin" "doc" "python"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-ucs4")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-python-extension
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion "python"
               (invoke "python" "setup.py" "install"
                       (string-append "--prefix="
                                      (assoc-ref outputs "python"))
                       "--root=/")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("clang-format" ,clang)
       ("help2man" ,help2man)
       ("libtool" ,libtool)
       ("libyaml" ,libyaml)
       ("makeinfo" ,texinfo)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
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
  (package
    (name "liblouisutdml")
    (version "2.9.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/liblouis/liblouisutdml")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c32cfcfp0lyfd655c9ihhh3p7lhrb9q3xbll7q5dw4km86gaq6w"))))
    (build-system gnu-build-system)
    (outputs '("out" "bin" "doc"))
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("help2man" ,help2man)
       ("jdk" ,icedtea "jdk")
       ("libtool" ,libtool)
       ("makeinfo" ,texinfo)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list libxml2))
    (propagated-inputs
     `(("liblouis" ,liblouis)
       ("liblouis:bin" ,liblouis "bin")))
    (synopsis "Braille transcription services")
    (description "Liblouisutdml is a library providing complete braille
transcription services for xml, html and text documents.  It translates into
appropriate braille codes and formats according to its style sheet and the
specifications in the document.")
    (home-page "http://liblouis.org/")
    (license (list license:lgpl3+       ; library
                   license:gpl3+))))    ; tools

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

(define* (tegaki-release-uri proj version
                             #:optional (ext "tar.gz"))
  (string-append "https://github.com/tegaki/tegaki/releases/download"
                 "/v" version "/" proj "-" version "." ext))

(define remove-pre-compiled-files
  (lambda exts
    "Return snippet for removing pre-compiled files matching one of the
extensions in EXTS."
    `(begin (for-each delete-file
                      (find-files "."
                                  (lambda (name _)
                                    (any (cut string-suffix? <> name)
                                         (map (cut string-append "." <>)
                                              ',exts)))))
            #t)))

;;; modules required for the above snippet
(define remove-pre-compiled-files-modules
  '((guix build utils)
    (srfi srfi-1)
    (srfi srfi-26)))

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
    (version "6.1.30")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/praat/praat")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pjfifyv3wjn68l3i2dr83xm75nf2kxvfxrk9qqbmwz58p183jw4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (copy-file "makefiles/makefile.defs.linux.pulse" "makefile.defs")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (copy-file "praat" (string-append bin "/praat")))
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("gtk" ,gtk+-2)
       ("jack" ,jack-1)
       ("publesaudio" ,pulseaudio)))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.fon.hum.uva.nl/praat/")
    (synopsis "Doing phonetics by computer")
    (description "Praat is a tool to perform phonetics tasks.  It can do speech
analysis (pitch, formant, intensity, ...), speech synthesis, labelling, segmenting
and manipulation.")
    (license license:gpl2+)))
