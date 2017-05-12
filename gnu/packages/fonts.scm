;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 ng0 <ng0@libertad.pw>
;;; Copyright © 2016 Jookia <166291@gmail.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Dmitry Nikolaev <cameltheman@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Toni Reina <areina@riseup.net>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Brendan Tildesley <brendan.tildesley@openmailbox.org>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip))

(define-public font-inconsolata
  (package
    (name "font-inconsolata")
    (version "0.80")
    (source (origin
              (method url-fetch)
              (uri "http://www.levien.com/type/myfonts/Inconsolata.otf")
              (sha256
               (base32
                "06js6znbcf7swn8y3b8ki416bz96ay7d3yvddqnvi88lqhbfcq8m"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((font-dir (string-append %output
                                                  "/share/fonts/opentype"))
                         (source (assoc-ref %build-inputs "source")))
                     (mkdir-p font-dir)
                     (copy-file source
                                (string-append font-dir "/" "inconsolata.otf"))))))
    (native-inputs `(("source" ,source)))
    (home-page "http://levien.com/type/myfonts/inconsolata.html")
    (synopsis "Monospace font")
    (description "A monospace font, designed for code listings and the like,
in print.  With attention to detail for high resolution rendering.")
    (license license:silofl1.1)))

(define-public font-ubuntu
  (package
    (name "font-ubuntu")
    (version "0.83")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://font.ubuntu.com/download/ubuntu-font-family-"
                    version ".zip"))
              (sha256
               (base32
                "0hjvq2x758dx0sfwqhzflns0ns035qm7h6ygskbx1svzg517sva5"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let ((PATH     (string-append (assoc-ref %build-inputs
                                                             "unzip")
                                                  "/bin"))
                         (font-dir (string-append %output
                                                  "/share/fonts/truetype"))
                         (doc-dir  (string-append %output "/share/doc/"
                                                  ,name "-" ,version)))
                     (setenv "PATH" PATH)
                     (system* "unzip" (assoc-ref %build-inputs "source"))

                     (mkdir-p font-dir)
                     (mkdir-p doc-dir)
                     (chdir (string-append "ubuntu-font-family-" ,version))
                     (for-each (lambda (ttf)
                                 (install-file ttf font-dir))
                               (find-files "." "\\.ttf$"))
                     (for-each (lambda (doc)
                                 (install-file doc doc-dir))
                               (find-files "." "\\.txt$"))))))
    (native-inputs `(("source" ,source)
                     ("unzip" ,unzip)))
    (home-page "http://font.ubuntu.com/")
    (synopsis "The Ubuntu Font Family")
    (description "The Ubuntu Font Family is a unique, custom designed font
that has a very distinctive look and feel.  This package provides the
TrueType (TTF) files.")
    (license
     (license:non-copyleft
      "http://font.ubuntu.com/ufl/ubuntu-font-licence-1.0.txt"
      "Ubuntu Font License v1.0"))))

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
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))

                   (let ((tar      (string-append (assoc-ref %build-inputs
                                                             "tar")
                                                  "/bin/tar"))
                         (PATH     (string-append (assoc-ref %build-inputs
                                                             "bzip2")
                                                  "/bin"))
                         (font-dir (string-append
                                    %output "/share/fonts/truetype"))
                         (conf-dir (string-append
                                    %output "/share/fontconfig/conf.avail"))
                         (doc-dir  (string-append
                                    %output "/share/doc/" ,name "-" ,version)))
                     (setenv "PATH" PATH)
                     (system* tar "xvf" (assoc-ref %build-inputs "source"))

                     (mkdir-p font-dir)
                     (mkdir-p conf-dir)
                     (mkdir-p doc-dir)
                     (chdir (string-append "dejavu-fonts-ttf-" ,version))
                     (for-each (lambda (ttf)
                                 (install-file ttf font-dir))
                               (find-files "ttf" "\\.ttf$"))
                     (for-each (lambda (conf)
                                 (install-file conf conf-dir))
                               (find-files "fontconfig" "\\.conf$"))
                     (for-each (lambda (doc)
                                 (install-file doc doc-dir))
                               (find-files "." "\\.txt$|^[A-Z][A-Z]*$"))))))
    (native-inputs `(("source" ,source)
                     ("tar" ,tar)
                     ("bzip2" ,bzip2)))
    (home-page "http://dejavu-fonts.org/")
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
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let ((tar      (string-append (assoc-ref %build-inputs
                                                             "tar")
                                                  "/bin/tar"))
                         (PATH     (string-append (assoc-ref %build-inputs
                                                             "bzip2")
                                                  "/bin"))
                         (font-dir (string-append %output
                                                  "/share/fonts/truetype"))
                         (doc-dir  (string-append %output "/share/doc/"
                                                  ,name "-" ,version)))
                     (setenv "PATH" PATH)
                     (system* tar "xvf" (assoc-ref %build-inputs "source"))

                     (mkdir-p font-dir)
                     (mkdir-p doc-dir)
                     (chdir (string-append "ttf-bitstream-vera-" ,version))
                     (for-each (lambda (ttf)
                                 (install-file ttf font-dir))
                               (find-files "." "\\.ttf$"))
                     (for-each (lambda (doc)
                                 (install-file doc doc-dir))
                               (find-files "." "\\.TXT$"))))))
    (native-inputs `(("source" ,source)
                     ("tar" ,tar)
                     ("bzip2" ,bzip2)))
    (home-page "http://www.gnome.org/fonts/")
    (synopsis "Bitstream Vera sans-serif typeface")
    (description "Vera is a sans-serif typeface from Bitstream, Inc.  This
package provides the TrueType (TTF) files.")
    (license
     (license:fsdg-compatible
      "https://www.gnome.org/fonts/#Final_Bitstream_Vera_Fonts"
      "The Font Software may be sold as part of a larger software package but
no copy of one or more of the Font Software typefaces may be sold by
itself."))))

(define-public font-cantarell
  (package
    (name "font-abattis-cantarell")
    (version "0.0.25")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/cantarell-fonts/"
                                  (version-major+minor version)
                                  "/cantarell-fonts-" version ".tar.xz"))
              (sha256
               (base32
                "0zvkd8cm1cg2919v1js9qmzwa02sjl7qajj3gcvgqvai1fm2i8hl"))))
    (build-system gnu-build-system)
    (home-page "https://wiki.gnome.org/Projects/CantarellFonts")
    (synopsis "Cantarell sans-serif typeface")
    (description "The Cantarell font family is a contemporary Humanist
sans-serif designed for on-screen reading.  It is used by GNOME@tie{}3.")
    (license license:silofl1.1)))

(define-public font-gnu-freefont-ttf
  (package
    (name "font-gnu-freefont-ttf")
    (version "20120503")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/freefont/freefont-src-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0yk58blhcd4hm7nyincmqq4jrzjjk82wif2zmk1l3y2m4vif4qhd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'install
                   (lambda _
                     (let ((doc-dir  (string-append %output "/share/doc/"
                                                    ,name "-" ,version))
                           (font-dir (string-append %output
                                                    "/share/fonts/truetype")))
                       (mkdir-p doc-dir)
                       (substitute* "Makefile"
                         (("\\$\\(TMPDIR\\)") doc-dir)
                         (("sfd/\\*.ttf") ""))
                       (system* "make" "ttftar")
                       (mkdir-p font-dir)
                       (for-each (lambda (file)
                                   (install-file file font-dir))
                                 (filter
                                   (lambda (file) (string-suffix? "ttf" file))
                                   (find-files "." "")))))))
       #:test-target "tests"))
    ;; replace python 3 with python 2
    ;; python 3 support commits aren't yet released in 20120503
    ;; so freefont needs python 2 support in fontforge
    (native-inputs `(("fontforge" ,(package (inherit fontforge)
                                     (inputs `(("python-2" ,python-2)
                                     ,@(package-inputs fontforge)))))))
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
    (version "2.00.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://fedorahosted.org/releases/l/i/"
                                  "liberation-fonts/liberation-fonts-ttf-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "010m4zfqan4w04b6bs9pm3gapn9hsb18bmwwgp2p6y6idj52g43q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let ((tar      (string-append (assoc-ref %build-inputs "tar")
                                        "/bin/tar"))
               (PATH     (string-append (assoc-ref %build-inputs "gzip")
                                        "/bin"))
               (font-dir (string-append %output "/share/fonts/truetype"))
               (doc-dir  (string-append %output "/share/doc/" ,name)))
           (setenv "PATH" PATH)
           (system* tar "xvf" (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (mkdir-p doc-dir)
           (chdir (string-append "liberation-fonts-ttf-" ,version))
           (for-each (lambda (ttf)
                       (install-file ttf font-dir))
                     (find-files "." "\\.ttf$"))
           (for-each (lambda (doc)
                       (install-file doc doc-dir))
                     '("AUTHORS" "ChangeLog" "LICENSE" "README" "TODO"))))))
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "https://fedorahosted.org/liberation-fonts/")
    (synopsis
     "Fonts compatible with Arial, Times New Roman, and Courier New")
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
@end enumerate

The Liberation Fonts are sponsored by Red Hat.")
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
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (let ((compile
                    (lambda (name ext)
                      (zero? (system*
                              "fontforge" "-lang=ff"
                              "-c" (string-append "Open('" name "');"
                                                  "Generate('"
                                                  (basename name "sfd") ext
                                                  "')"))))))
               (every (lambda (name)
                        (and (compile name "ttf")
                             (compile name "otf")))
                      (find-files "." "\\.sfd$")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((font-dir (string-append (assoc-ref outputs "out")
                                            "/share/fonts/truetype")))
               (mkdir-p font-dir)
               (for-each (cut install-file <> font-dir)
                         (find-files "." "\\.(otf|ttf)$"))
               #t))))))
    (native-inputs
     `(("fontforge" ,fontforge)))
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

(define-public font-terminus
  (package
    (name "font-terminus")
    (version "4.40")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://sourceforge/terminus-font/terminus-font-"
               version
               "/terminus-font-"
               version
               ".tar.gz"))
        (sha256
         (base32
          "0487cyx5h1f0crbny5sg73a22gmym5vk1i7646gy7hgiscj2rxb4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("bdftopcf" ,bdftopcf)
       ("font-util" ,font-util)
       ("mkfontdir" ,mkfontdir)))
    (arguments
     `(#:configure-flags (list
                          ;; install fonts into subdirectory of package output
                          ;; instead of font-util-?.?.?/share/fonts/X11
                          (string-append "--with-fontrootdir="
                                         %output "/share/fonts/X11"))
       #:tests? #f)) ;; No test target in tarball
    (home-page "http://terminus-font.sourceforge.net/")
    (synopsis "Simple bitmap programming font")
    (description "Terminus Font is a clean, fixed-width bitmap font, designed
for long periods of working with computers (8 or more hours per day).")
    (license license:silofl1.1)))

(define-public font-adobe-source-han-sans
  (package
    (name "font-adobe-source-han-sans")
    (version "1.004")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/adobe-fonts/source-han-sans/archive/"
                    version "R.tar.gz"))
              (file-name (string-append "source-han-sans-" version "R.tar.gz"))
              (sha256
               (base32
                "1ssx0fw90sy6mj8fv8fv4dgzszpqwbmwpjnlx16g4pvaqzdmybbz"))))
    (outputs '("out"                 ; OpenType/CFF Collection (OTC), 121 MiB.
               "cn" "jp" "kr" "tw")) ; Region-specific Subset OpenType/CFF.
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar  (string-append (assoc-ref %build-inputs
                                               "tar")
                                    "/bin/tar"))
               (PATH (string-append (assoc-ref %build-inputs
                                               "gzip")
                                    "/bin"))
               (install-opentype-fonts
                (lambda (fonts-dir out)
                  (copy-recursively fonts-dir
                                    (string-append (assoc-ref %outputs out)
                                                   "/share/fonts/opentype")))))
           (setenv "PATH" PATH)
           (system* tar "xvf" (assoc-ref %build-inputs "source"))
           (chdir (string-append "source-han-sans-" ,version "R"))
           (install-opentype-fonts "OTC" "out")
           (install-opentype-fonts "SubsetOTF/CN" "cn")
           (install-opentype-fonts "SubsetOTF/JP" "jp")
           (install-opentype-fonts "SubsetOTF/KR" "kr")
           (install-opentype-fonts "SubsetOTF/TW" "tw")
           (for-each delete-file (find-files %output "\\.zip$"))))))
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
    (home-page "https://github.com/adobe-fonts/source-han-sans")
    (synopsis "Pan-CJK fonts")
    (description
     "Source Han Sans is a sans serif Pan-CJK font family that is offered in
seven weights: ExtraLight, Light, Normal, Regular, Medium, Bold, and Heavy.
And in several OpenType/CFF-based deployment configurations to accommodate
various system requirements or limitations.  As the name suggests, Pan-CJK
fonts are intended to support the characters necessary to render or display
text in Simplified Chinese, Traditional Chinese, Japanese, and Korean.")
    (license license:silofl1.1)))

(define-public font-cns11643
  (package
    (name "font-cns11643")
    (version "98.1.20170405")
    (source (origin
              (method url-fetch)
              (uri "http://www.cns11643.gov.tw/AIDB/Open_Data.zip")
              (sha256
               (base32
                "02kb3bwjrra0k2hlr2p8xswd2y0xs6j8d9vm6yrby734h02a40qf"))))
    (outputs '("out" "tw-kai" "tw-sung"))
    (build-system trivial-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((font-dir "/share/fonts/truetype/cns11643")
                (out (string-append
                      (assoc-ref %outputs "out") font-dir))
                (tw-kai (string-append
                         (assoc-ref %outputs "tw-kai") font-dir))
                (tw-sung (string-append
                          (assoc-ref %outputs "tw-sung") font-dir))
                (unzip (string-append
                        (assoc-ref %build-inputs "unzip") "/bin/unzip")))
           (system* unzip (assoc-ref %build-inputs "source"))
           (chdir "Open_Data/Fonts/")
           (install-file "TW-Kai-98_1.ttf" tw-kai)
           (install-file "TW-Sung-98_1.ttf" tw-sung)
           (install-file "TW-Kai-98_1.ttf" out)
           (install-file "TW-Kai-Ext-B-98_1.ttf" out)
           (install-file "TW-Kai-Plus-98_1.ttf" out)
           (install-file "TW-Sung-98_1.ttf" out)
           (install-file "TW-Sung-Ext-B-98_1.ttf" out)
           (install-file "TW-Sung-Plus-98_1.ttf" out)
           #t))))
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
              (file-name (string-append "wqy-zenhei-" version ".tar.gz"))
              (sha256
               (base32
                "1mkmxq8g2hjcglb3zajfqj20r4r88l78ymsp2xyl5yav8w3f7dz4"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((PATH (string-append (assoc-ref %build-inputs "tar")  "/bin:"
                                    (assoc-ref %build-inputs "gzip") "/bin"))
               (font-dir (string-append (assoc-ref %outputs "out")
                                        "/share/fonts/wenquanyi/")))
           (setenv "PATH" PATH)
           (mkdir-p font-dir)
           (system* "tar" "xvf" (assoc-ref %build-inputs "source"))
           (chdir "wqy-zenhei")
           (install-file "wqy-zenhei.ttc" font-dir)))))
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
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
    ;; GPLv2 with font embedding exception
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
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((PATH (string-append (assoc-ref %build-inputs "tar")  "/bin:"
                                    (assoc-ref %build-inputs "gzip") "/bin"))
               (font-dir (string-append (assoc-ref %outputs "out")
                                        "/share/fonts/wenquanyi")))
           (mkdir-p font-dir)
           (setenv "PATH" PATH)
           (system* "tar" "xvf" (assoc-ref %build-inputs "source"))
           (install-file "wqy-microhei/wqy-microhei.ttc" font-dir)))))
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
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

(define-public font-tex-gyre
  (package
    (name "font-tex-gyre")
    (version "2.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.gust.org.pl/projects/e-foundry/"
                           "tex-gyre/whole/tg-" version "otf.zip"))
       (sha256
        (base32
         "0kph9l3g7jb2bpmxdbdg5zl56wacmnvdvsdn7is1gc750sqvsn31"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let ((unzip    (string-append (assoc-ref %build-inputs "unzip")
                                        "/bin/unzip"))
               (font-dir (string-append %output "/share/fonts/opentype")))
           (mkdir-p font-dir)
           (system* unzip
                    (assoc-ref %build-inputs "source")
                    "-d" font-dir)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.gust.org.pl/projects/e-foundry/tex-gyre/")
    (synopsis "Remake of Ghostscript fonts")
    (description "The TeX Gyre collection of fonts is the result of an
extensive remake and extension of the freely available base PostScript fonts
distributed with Ghostscript version 4.00.  The collection contains the
following fonts in the OpenType format: Adventor, Bonum, Chorus, Cursor,
Heros, Pagella, Schola, Termes.")
    (license license:gfl1.0)))

(define-public font-anonymous-pro
  (package
    (name "font-anonymous-pro")
    (version "1.002")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.marksimonson.com/assets/content/fonts/"
                    "AnonymousPro-" version ".zip"))
              (sha256
               (base32
                "1asj6lykvxh46czbal7ymy2k861zlcdqpz8x3s5bbpqwlm3mhrl6"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((unzip (string-append (assoc-ref %build-inputs "unzip")
                                     "/bin/unzip"))
               (font-dir (string-append %output "/share/fonts/truetype"))
               (doc-dir  (string-append %output "/share/doc/" ,name)))
           (system* unzip (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (mkdir-p doc-dir)
           (chdir (string-append "AnonymousPro-" ,version ".001"))
           (for-each (lambda (ttf)
                       (install-file ttf font-dir))
                     (find-files "." "\\.ttf$"))
           (for-each (lambda (doc)
                       (install-file doc doc-dir))
                     (find-files "." "\\.txt$"))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.marksimonson.com/fonts/view/anonymous-pro")
    (synopsis "Fixed-width fonts designed with coding in mind")
    (description "Anonymous Pro is a family of four fixed-width fonts designed
with coding in mind.  Anonymous Pro features an international, Unicode-based
character set, with support for most Western and Central European Latin-based
languages, plus Greek and Cyrillic.")
    (license license:silofl1.1)))

(define-public font-gnu-unifont
  (package
    (name "font-gnu-unifont")
    (version "9.0.06")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/unifont/unifont-" version "/unifont-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0ybyraxi8pngibazfq4zlsqmg8kn5xlhvaiwnxb11znhfi61vi87"))))
    (build-system gnu-build-system)
    (outputs '("out" ; TrueType version
               "pcf" ; PCF (bitmap) version
               "psf" ; PSF (console) version
               "bin" ; Utilities to manipulate '.hex' format
               ))
    (arguments
     '(#:parallel-build? #f ; parallel build fails
       #:tests? #f          ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace
          'configure
          (lambda _ (setenv "CC" "gcc") #t))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((ttf (string-append (assoc-ref outputs "out")
                                       "/share/fonts/truetype"))
                   (pcf (string-append (assoc-ref outputs "pcf")
                                       "/share/fonts/misc"))
                   (psf (string-append (assoc-ref outputs "psf")
                                       "/share/consolefonts"))
                   (bin (assoc-ref outputs "bin")))
              (system* "make"
                       (string-append "PREFIX=" bin)
                       (string-append "TTFDEST=" ttf)
                       (string-append "PCFDEST=" pcf)
                       (string-append "CONSOLEDEST=" psf)
                       "install")
              ;; Move Texinfo file to the right place.
              (mkdir (string-append bin "/share/info"))
              (rename-file (string-append bin "/share/unifont/unifont.info.gz")
                           (string-append bin "/share/info/unifont.info.gz"))
              #t))))))
    (inputs
     `(("perl" ,perl))) ; for utilities
    (synopsis
     "Large bitmap font covering Unicode's Basic Multilingual Plane")
    (description
     "GNU Unifont is a bitmap font covering essentially all of
Unicode's Basic Multilingual Plane.  The package also includes
utilities to ease adding new glyphs to the font.")
    (home-page "http://unifoundry.com/unifont.html")
    (license license:gpl2+)))

(define-public font-google-noto
  (package
    (name "font-google-noto")
    (version "20150929")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://noto-website-2.storage.googleapis.com/"
                                  "pkgs/Noto-hinted.zip"))
              (sha256
               (base32
                "13jhpqzhsqhyby8n0ksqg155a3jyaif3nzj9anzbq8s2gn1xjyd9"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let ((PATH     (string-append (assoc-ref %build-inputs
                                                             "unzip")
                                                  "/bin"))
                         (font-dir (string-append %output
                                                  "/share/fonts/truetype")))
                     (setenv "PATH" PATH)
                     (system* "unzip" (assoc-ref %build-inputs "source"))

                     (mkdir-p font-dir)
                     (for-each (lambda (ttf)
                                 (install-file ttf font-dir))
                               (find-files "." "\\.ttf$"))
                     (for-each (lambda (otf)
                                 (install-file otf font-dir))
                               (find-files "." "\\.otf$"))))))
    (native-inputs `(("unzip" ,unzip)))
    (home-page "https://www.google.com/get/noto/")
    (synopsis "Fonts to cover all languages")
    (description "Google Noto Fonts is a family of fonts designed to support
all languages with a consistent look and aesthetic.  Its goal is to properly
display all Unicode symbols.")
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
    (native-inputs `(("unzip" ,unzip)))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let ((PATH (string-append (assoc-ref %build-inputs
                                                         "unzip")
                                              "/bin"))
                         (font-dir (string-append %output
                                                  "/share/fonts/truetype")))
                     (setenv "PATH" PATH)
                     (system* "unzip" (assoc-ref %build-inputs "source"))

                     (mkdir-p font-dir)
                     (chdir "roboto-hinted")
                     (for-each (lambda (ttf)
                                 (install-file ttf font-dir))
                               (find-files "." "\\.ttf$"))))))
    (home-page "https://github.com/google/roboto")
    (synopsis "The Roboto family of fonts")
    (description
     "Roboto is Google’s signature family of fonts, the default font on Android
and Chrome OS, and the recommended font for the
visual language \"Material Design\".")
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
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let ((tar      (string-append (assoc-ref %build-inputs "tar")
                                        "/bin/tar"))
               (PATH     (string-append (assoc-ref %build-inputs "gzip")
                                        "/bin"))
               (font-dir (string-append %output "/share/fonts/truetype"))
               (doc-dir  (string-append %output "/share/doc/" ,name)))
           (setenv "PATH" PATH)
           (system* tar "xvf" (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (mkdir-p doc-dir)
           (chdir (string-append "un-fonts"))
           (for-each (lambda (ttf)
                       (install-file ttf font-dir))
                     (find-files "." "\\.ttf$"))
           (for-each (lambda (doc)
                       (install-file doc doc-dir))
                     '("COPYING" "README"))))))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
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

(define-public font-fantasque-sans
  (package
    (name "font-fantasque-sans")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/belluzj/fantasque-sans/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07fpy53k2x2nz5q61swkab6cfk9gw2kc4x4brsj6zjgbm16fap85"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ttfautohint" ,ttfautohint)
       ("woff-tools" ,woff-tools)
       ("fontforge" ,fontforge)
       ("woff2" ,woff2)
       ("ttf2eot" ,ttf2eot)))
    (arguments
     `(#:tests? #f                 ;test target intended for visual inspection
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configuration
                  (replace 'install
                    ;; 'make install' wants to install to ~/.fonts, install to
                    ;; output instead.
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (font-dir (string-append out "/share/fonts"))
                             (truetype-dir (string-append font-dir "/truetype"))
                             (opentype-dir (string-append font-dir "/opentype"))
                             (webfonts-dir (string-append font-dir "/webfonts")))
                        (copy-recursively "OTF" opentype-dir)
                        (for-each (lambda (f) (install-file f truetype-dir))
                                  (find-files "." "\\.ttf$"))
                        (copy-recursively "Webfonts" webfonts-dir)
                        #t))))))
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
    (version "2.020")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/chrissimpkins/Hack/releases/download/v"
                    version "/Hack-v"
                    (string-replace-substring version "." "_")
                    "-ttf.zip"))
              (sha256
               (base32
                "16kkmc3psckw1b7k07ccn1gi5ymhlg9djh43nqjzg065g6p6d184"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let ((PATH     (string-append (assoc-ref %build-inputs
                                                             "unzip")
                                                  "/bin"))
                         (font-dir (string-append %output
                                                  "/share/fonts/truetype")))
                     (setenv "PATH" PATH)
                     (system* "unzip" (assoc-ref %build-inputs "source"))

                     (mkdir-p font-dir)
                     (for-each (lambda (ttf)
                                 (install-file ttf font-dir))
                               (find-files "." "\\.ttf$"))))))
    (native-inputs
     `(("source" ,source)
       ("unzip" ,unzip)))
    (home-page "https://sourcefoundry.org/hack/")
    (synopsis "Typeface designed for source code")
    (description
     "Hack is designed to be a workhorse typeface for code.  It expands upon
the Bitstream Vera & DejaVu projects, provides 1561 glyphs, and includes
Powerline support.")
    (license (license:x11-style
              "https://github.com/chrissimpkins/Hack/blob/master/LICENSE.md"
              "Hack Open Font License v2.0"))))

(define-public font-adobe-source-code-pro
  (package
    (name "font-adobe-source-code-pro")
    (version "2.030R-ro-1.050R-it")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/adobe-fonts/source-code-pro/archive/"
                    (regexp-substitute/global
                     ;; The upstream tag uses "/" between the roman and italic
                     ;; versions, so substitute our "-" separator here.
                     #f "R-ro-" version 'pre "R-ro/" 'post) ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0arhhsf3i7ss39ykn73d1j8k4n8vx7115xph6jwkd970p1cxvr54"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar  (string-append (assoc-ref %build-inputs "tar")
                                    "/bin/tar"))
               (PATH (string-append (assoc-ref %build-inputs "gzip")
                                    "/bin"))
               (font-dir (string-append %output "/share/fonts/opentype")))
           (setenv "PATH" PATH)
           (mkdir-p font-dir)
           (zero? (system* tar "-C" font-dir "--strip-components=2"
                           "-xvf" (assoc-ref %build-inputs "source")
                           (string-append "source-code-pro-"
                                          ,version "/OTF")))))))
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
    (home-page "https://github.com/adobe-fonts/source-code-pro")
    (synopsis
     "Monospaced font family for user interface and coding environments")
    (description
     "Source Code Pro is a set of monospaced OpenType fonts that have been
designed to work well in user interface environments.")
    (license license:silofl1.1)))

(define-public font-fira-mono
  (package
    (name "font-fira-mono")
    (version "3.206")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://carrois.com/downloads/fira_mono_3_2/"
                                  "FiraMonoFonts"
                                  (string-replace-substring version "." "")
                                  ".zip"))
              (sha256
               (base32
                "1z65x0dw5dq6rs6p9wyfrir50rlh95vgzsxr8jcd40nqazw4jhpi"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((unzip (string-append (assoc-ref %build-inputs "unzip")
                                     "/bin/unzip"))
               (font-dir (string-append %output "/share/fonts/opentype")))
           (mkdir-p font-dir)
           (system* unzip
                    "-j"
                    (assoc-ref %build-inputs "source")
                    "*.otf"
                    "-d" font-dir)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://mozilla.github.io/Fira/")
    (synopsis "Mozilla's monospace font")
    (description "This is the typeface used by Mozilla in Firefox OS.")
    (license license:silofl1.1)))

(define-public font-awesome
  (package
   (name "font-awesome")
   (version "4.7.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://fontawesome.io/assets/"
                                name "-" version ".zip"))
            (sha256
             (base32
              "1frhmw41lnnm9rda2zs202pvfi5vzlrsw4xfp4mswl0qgws61mcd"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("unzip" ,unzip)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let* ((font-dir (string-append %output
                                                  "/share/fonts/opentype"))
                         (source (assoc-ref %build-inputs "source"))
                         (src-otf-file (string-append "font-awesome-"
                                                      ,version
                                                      "/fonts/FontAwesome.otf"))
                         (dest-otf-file (string-append font-dir "/FontAwesome.otf"))
                         (unzip (assoc-ref %build-inputs "unzip")))
                    (setenv "PATH" (string-append unzip "/bin"))
                    (mkdir-p font-dir)
                    (system* "unzip" source "-d" ".")
                    (copy-file src-otf-file dest-otf-file)))))
   (home-page "http://fontawesome.io")
   (synopsis "Font that contains a rich iconset")
   (description
    "Font Awesome is a full suite of pictographic icons for easy scalable
vector graphics.")
   (license license:silofl1.1)))

(define-public font-tamzen
  (package
    (name "font-tamzen")
    (version "1.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sunaku/tamzen-font/archive/"
                           "Tamzen-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ryd7gp6qiwaqw73jqbmh4kwlriyd8xykh4j7z90z8xp9fm7lrys"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let ((tar      (string-append (assoc-ref %build-inputs "tar")
                                        "/bin/tar"))
               (PATH     (string-append (assoc-ref %build-inputs "gzip")
                                        "/bin"))
               (font-dir (string-append %output "/share/fonts/misc"))
               (psf-dir (string-append %output "/share/kbd/consolefonts"))
               (src-pcf-dir (string-append "tamzen-font-Tamzen-"
                                            ,version "/pcf")))
           (setenv "PATH" PATH)
           (system* tar "xvf" (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (mkdir-p psf-dir)
           (chdir src-pcf-dir)
           (for-each (lambda (pcf)
                       (install-file pcf font-dir))
                     (find-files "." "\\.pcf$"))
           (chdir "../psf")
           (for-each (lambda (psf)
                       (install-file psf psf-dir))
                     (find-files "." "\\.psf$"))
           #t))))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "https://github.com/sunaku/tamzen-font")
    (synopsis "Monospaced bitmap font for console and X11")
    (description
     "Tamzen is a fork of the @code{Tamsyn} font.  It is programatically forked
from @code{Tamsyn} version 1.11, backporting glyphs from older versions while
deleting deliberately empty glyphs (which are marked as unimplemented) to
allow secondary/fallback fonts to provide real glyphs at those codepoints.

The @code{TamzenForPowerline} fonts provide additional @code{Powerline} symbols,
which are programatically injected with @code{bitmap-font-patcher} and
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
   (version "2.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://www.comicneue.com/comic-neue-" version ".zip"))
            (sha256
             (base32
              "1695hkpd8kqnr2a88p8xs496slgzxjjkzpa9aa33ml3pnh7519zk"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let ((font-dir (string-append %output
                                                 "/share/fonts/truetype"))
                        (source (assoc-ref %build-inputs "source"))
                        (unzip  (string-append (assoc-ref %build-inputs "unzip")
                                               "/bin/unzip")))
                    (mkdir-p font-dir)
                    (system* unzip source)
                    (with-directory-excursion
                     (string-append "Web")
                     (for-each (lambda (ttf)
                                 (install-file ttf font-dir))
                               (find-files "." "\\.ttf$")))))))
   (native-inputs `(("unzip" ,unzip)))
   (home-page "http://www.comicneue.com/")
   (synopsis "Font that fixes the shortcomings of Comic Sans")
   (description
    "Comic Neue is a font that attempts to create a respectable casual
typeface, by mimicking Comic Sans while fixing its most obvious shortcomings.")
   (license license:silofl1.1)))

(define-public font-iosevka
  (package
   (name "font-iosevka")
   (version "1.11.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/be5invis/Iosevka/releases/download/v"
                  version "/iosevka-pack-" version ".zip"))
            (sha256
             (base32
              "0d8prdk7s5z94sdfd0y92cvqq531yqrlg7hnadbnhd7fs9jqr5hj"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let ((font-dir (string-append %output
                                                 "/share/fonts/truetype"))
                        (source (assoc-ref %build-inputs "source"))
                        (unzip  (string-append (assoc-ref %build-inputs "unzip")
                                               "/bin/unzip")))
                    (mkdir-p font-dir)
                    (system* unzip "-d" font-dir source)))))
   (native-inputs `(("unzip" ,unzip)))
   (home-page "https://be5invis.github.io/Iosevka/")
   (synopsis "Coders' typeface, built from code")
   (description
    "Iosevka is a slender monospace sans-serif or slab-serif typeface inspired
by Pragmata Pro, M+, and PF DIN Mono, designed to be the ideal font for
programming.  Iosevka is completely generated from its source code.")
   (license (list license:silofl1.1  ; build artifacts (i.e. the fonts)
                  license:bsd-3))))  ; supporting code

(define-public font-go
  (let ((commit "b7f8df6bc082334698d4505fb85fa05e99156b72")
        (revision "1"))
    (package
     (name "font-go")
     (version (string-append "20161115-" revision "." (string-take commit 7)))
     (source (origin
              (file-name (string-append "go-image-" version "-checkout"))
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/image")
                    (commit commit)))
              (sha256
               (base32
                "1ywxs6dmcyzwwsmnan3qqza7znprnbvmdi260x6sjmydz6dyq2zs"))))
     (build-system trivial-build-system)
     (arguments
      `(#:modules ((guix build utils))
        #:builder (begin
                    (use-modules (guix build utils))
                    (let ((font-dir (string-append %output
                                                   "/share/fonts/truetype"))
                          (source (assoc-ref %build-inputs "source")))
                      (mkdir-p font-dir)
                      (with-directory-excursion
                       (string-append source "/font/gofont/ttfs")
                       (for-each (lambda (ttf)
                                   (install-file ttf font-dir))
                                 (find-files "." "\\.ttf$")))))))
     (home-page "https://blog.golang.org/go-fonts")
     (synopsis "The Go font family")
     (description
      "The Go font family is a set of WGL4 TrueType fonts from the Bigelow &
Holmes type foundry, released under the same license as the Go programming
language.  It includes a set of proportional, sans-serif fonts, and a set of
monospace, slab-serif fonts.")
     (license (package-license go-1.4)))))

(define-public font-google-material-design-icons
  (package
   (name "font-google-material-design-icons")
   (version "3.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "https://github.com/google/material-design-icons/archive/"
                    version ".tar.gz"))
            (sha256
             (base32
              "183n0qv3q8w6n27libarq1fhc4mqv2d3sasbfmbn7x9r5pw9c6ga"))
            (file-name (string-append name "-" version ".tar.gz"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("tar" ,tar)
      ("gzip" ,gzip)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let* ((font-dir (string-append %output
                                                  "/share/fonts/truetype"))
                         (source (assoc-ref %build-inputs "source"))
                         (font-filename "MaterialIcons-Regular.ttf")
                         (src-ttf-file (string-append "material-design-icons-"
                                                      ,version
                                                      "/iconfont/"
                                                      font-filename))
                         (dest-ttf-file (string-append font-dir font-filename))
                         (gzip (assoc-ref %build-inputs "gzip"))
                         (tar (assoc-ref %build-inputs "tar")))
                    (setenv "PATH" (string-append gzip "/bin:"
                                                  tar "/bin:"))
                    (system* "tar" "xf" source)
                    (mkdir-p font-dir)
                    (copy-file src-ttf-file dest-ttf-file)))))
   (home-page "http://google.github.io/material-design-icons")
   (synopsis "Icon font of Google Material Design icons")
   (description
    "Material design system icons are simple, modern, friendly, and sometimes
quirky.  Each icon is created using our design guidelines to depict in simple
and minimal forms the universal concepts used commonly throughout a UI.
Ensuring readability and clarity at both large and small sizes, these icons
have been optimized for beautiful display on all common platforms and display
resolutions.")
   (license license:asl2.0)))
