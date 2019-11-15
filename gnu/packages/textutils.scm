;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 ng0 <ng0@n0.is>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017,2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Yoshinori Arai <kumagusu08@gmail.com>
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

(define-module (gnu packages textutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages java)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages web))

(define-public dos2unix
  (package
    (name "dos2unix")
    (version "7.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://waterlan.home.xs4all.nl/" name "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "12h4c61g376bhq03y5g2xszkrkrj5hwd928rly3xsp6rvfmnbixs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure script
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)))
    (home-page "https://waterlan.home.xs4all.nl/dos2unix.html")
    (synopsis "DOS/Mac to Unix and vice versa text file format converter")
    (description
     "dos2unix is a tool to convert line breaks in a text file from Unix format
to DOS format and vice versa.")
    (license license:bsd-2)))

(define-public recode
  (package
    (name "recode")
    (version "3.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rrthomas/recode/releases/"
                           "download/v" version "/recode-" version ".tar.gz"))
       (sha256
        (base32 "0m59sd1ca0zw1aydpc3m8sw03nc885knmccqryg7byzmqs585ia6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python)
       ("python-cython" ,python-cython)))
    (home-page "https://github.com/rrthomas/recode")
    (synopsis "Text encoding converter")
    (description "The Recode library converts files between character sets and
usages.  It recognises or produces over 200 different character sets (or about
300 if combined with an iconv library) and transliterates files between almost
any pair.  When exact transliteration are not possible, it gets rid of
offending characters or falls back on approximations.  The recode program is a
handy front-end to the library.")
    (license license:gpl3+)))

(define-public enca
  (package
    (name "enca")
    (version "1.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/nijel/enca")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19q7cwwxmmk5j9438bsqdpjvdjawsd3zmw1zyqgi7s4m0rasr3ah"))))
    (build-system gnu-build-system)
    ;; enca-1.19 tests fail with recent recode.
    ;(inputs `(("recode" ,recode)))
    (home-page "https://github.com/nijel/enca")
    (synopsis "Text encoding detection tool")
    (description "Enca (Extremely Naive Charset Analyser) consists of libenca,
an encoding detection library, and enca, a command line frontend, integrating
libenca and several charset conversion libraries and tools.")
    (license license:gpl2)))

(define-public utf8proc
  (package
    (name "utf8proc")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaStrings/utf8proc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i42hqwc8znqii9brangwkxk5cyc2lk95ip405fg88zr7z2ncr34"))))
    (build-system gnu-build-system)
    (native-inputs           ;test data that is otherwise downloaded with curl
     `(("NormalizationTest.txt"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://www.unicode.org/Public/12.1.0/ucd/"
                               "NormalizationTest.txt"))
           (sha256
            (base32 "0hb97k9xv1lr847hwz0719ksqy39s47xw6k01dgs1368jdibvawc"))))
       ("GraphemeBreakTest.txt"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://www.unicode.org/Public/12.1.0/ucd/"
                               "auxiliary/GraphemeBreakTest.txt"))
           (sha256
            (base32 "0qc90ppmrwfn3y9cdn8jcjrn7qpdf0fhxkwh945yp4rvh37mbgcm"))))

       ;; For tests.
       ("perl" ,perl)))
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'check-data
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (i)
                         (copy-file (assoc-ref inputs i)
                                    (string-append "data/" i)))
                       '("NormalizationTest.txt" "GraphemeBreakTest.txt"))
             (substitute* "data/GraphemeBreakTest.txt"
               (("÷") "/")
               (("×") "+"))
             #t)))))
    (home-page "https://juliastrings.github.io/utf8proc/")
    (synopsis "C library for processing UTF-8 Unicode data")
    (description "utf8proc is a small C library that provides Unicode
normalization, case-folding, and other operations for data in the UTF-8
encoding, supporting Unicode version 9.0.0.")
    (license license:expat)))

(define-public libconfuse
  (package
    (name "libconfuse")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/martinh/libconfuse/"
                           "releases/download/v" version
                           "/confuse-" version ".tar.xz"))
       (sha256
        (base32 "02r1mmzik2m0iigbc2da3y754vj24i18r3ml5p2wzs027mjhn959"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/martinh/libconfuse")
    (synopsis "Configuration file parser library")
    (description "libconfuse is a configuration file parser library.  It
supports sections and (lists of) values (strings, integers, floats, booleans
or other sections), as well as some other features (such as
single/double-quoted strings, environment variable expansion, functions and
nested include statements).")
    (license license:isc)))

(define-public libgtextutils
  (package
    (name "libgtextutils")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/agordon/libgtextutils/releases/download/"
             version "/libgtextutils-" version ".tar.gz"))
       (sha256
        (base32 "0jiybkb2z58wa2msvllnphr4js2hvjvh988pavb3mzkgr6ihwbkr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _ (invoke "sh" "reconf"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/agordon/libgtextutils")
    (synopsis "Gordon's text utils library")
    (description
     "libgtextutils is a text utilities library used by the fastx toolkit from
the Hannon Lab.")
    (license license:agpl3+)))

(define-public cityhash
  (let ((commit "8af9b8c"))
    (package
      (name "cityhash")
      (version (string-append "1.1-2." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/cityhash.git")
                      (commit commit)))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "0n6skf5dv8yfl1ckax8dqhvsbslkwc9158zf2ims0xqdvzsahbi6"))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags (list "CXXFLAGS=-g -O3")
         #:phases
         (modify-phases %standard-phases
           ;; citycrc is not installed by default but is used by some
           ;; programs.
           (add-after 'install 'install-citycrc
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (include (string-append out "/include")))
                 (install-file "src/citycrc.h" include))
               #t)))))
      (home-page "https://github.com/google/cityhash")
      (synopsis "C++ hash functions for strings")
      (description
       "CityHash provides hash functions for strings.  The functions mix the
input bits thoroughly but are not suitable for cryptography.")
      (license license:expat))))

(define-public ustr
  (package
    (name "ustr")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.and.org/ustr/" version
                                  "/ustr-" version ".tar.bz2"))
              (sha256
               (base32
                "1i623ygdj7rkizj7985q9d6vj5amwg686aqb5j3ixpkqkyp6xbrx"))
              (patches (search-patches "ustr-fix-build-with-gcc-5.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             "HIDE="
             ;; Override "/sbin/ldconfig" with "echo" because we don't need
             ;; "ldconfig".
             "LDCONFIG=echo"
             (string-append "prefix=" (assoc-ref %outputs "out"))
             "all-shared")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-check-for-stdint
           (lambda _
             ;; Of course we have stdint.h, just not in /usr/include
             (substitute* '("Makefile"
                            "ustr-import.in")
               (("-f \"/usr/include/stdint.h\"") "-z \"\""))
             #t))
         ;; No configure script
         (delete 'configure))))
    (home-page "http://www.and.org/ustr/")
    (synopsis "String library with very low memory overhead")
    (description
     "Ustr is a string library for C with very low memory overhead.")
    ;; Quoted from the home page: "The License for the code is MIT, new-BSD,
    ;; LGPL, etc. ... if you need another license to help compatibility, just
    ;; ask for it.  It's basically public domain, without all the legal
    ;; problems for everyone that trying to make something public domain
    ;; entails."
    (license license:public-domain)))

(define-public ascii2binary
  (package
    (name "ascii2binary")
    (version "2.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://billposer.org/Software/Downloads/"
                           "ascii2binary-" version ".tar.bz2"))
       (sha256
        (base32 "0dc9fxcdmppbs9s06jvq61zbk552laxps0xyk098gj41697ihd96"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (home-page "https://billposer.org/Software/a2b.html")
    (synopsis "Convert between ASCII, hexadecimal and binary representations")
    (description "The two programs are useful for generating test data, for
inspecting binary files, and for interfacing programs that generate textual
output to programs that require binary input and conversely.  They can also be
useful when it is desired to reformat numbers.

@itemize

@item @command{ascii2binary} reads input consisting of ascii or hexadecimal
   representation numbers separated by whitespace and produces as output
   the binary equivalents.  The type and precision of the binary output
   is selected using command line flags.

@item @command{binary2ascii} reads input consisting of binary numbers
   and converts them to their ascii or hexadecimal representation.
   Command line flags specify the type and size of the binary numbers
   and provide control over the format of the output.
   Unsigned integers may be written out in binary, octal, decimal,
   or hexadecimal.

   Signed integers may be written out only in binary or decimal.  Floating
   point numbers may be written out only decimal, either in standard or
   scientific notation.  (If you want to examine the binary representation
   of floating point numbers, just treat the input as a sequence of unsigned
   characters.)

@end itemize")
    (license license:gpl3)))

(define-public uniutils
  (package
    (name "uniutils")
    (version "2.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://billposer.org/Software/Downloads/"
                           "uniutils-" version ".tar.bz2"))
       (sha256
        (base32 "19w1510w87gx7n4qy3zsb0m467a4rn5scvh4ajajg7jh6x5xri08"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-dependency-tracking")
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'fix-paths
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (a2b (assoc-ref inputs "ascii2binary"))
                   (iconv (assoc-ref inputs "libiconv")))
               (substitute* "utf8lookup"
                 (("^ascii2binary ") (string-append a2b "/bin/ascii2binary "))
                 (("^uniname ") (string-append out "/bin/uniname "))
                 (("^iconv ") (string-append iconv "/bin/iconv ")))
             #t))))))
    (inputs
     `(("ascii2binary" ,ascii2binary)
       ("libiconv" ,libiconv)))
    (home-page "https://billposer.org/Software/unidesc.html")
    (synopsis "Find out what is in a Unicode file")
    (description "Useful tools when working with Unicode files when one
doesn't know the writing system, doesn't have the necessary font, needs to
inspect invisible characters, needs to find out whether characters have been
combined or in what order they occur, or needs statistics on which characters
occur.

@itemize

@item @command{uniname} defaults to printing the character offset of each
character, its byte offset, its hex code value, its encoding, the glyph
itself, and its name.  It may also be used to validate UTF-8 input.

@item @command{unidesc} reports the character ranges to which different
portions of the text belong.   It can also be used to identify Unicode encodings
(e.g. UTF-16be) flagged by magic numbers.

@item @command{unihist} generates a histogram of the characters in its input.

@item @command{ExplicateUTF8} is intended for debugging or for learning about
Unicode.  It determines and explains the validity of a sequence of bytes as a
UTF8 encoding.

@item @command{utf8lookup} provides a handy way to look up Unicode characters
from the command line.

@item @command{unireverse} reverse each line of UTF-8 input
character-by-character.

@end itemize")
    (license license:gpl3)))

(define-public libconfig
  (package
    (name "libconfig")
    (version "1.7.2")
    (home-page "https://hyperrealm.github.io/libconfig/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/dist/libconfig-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1ngs2qx3cx5cbwinc5mvadly0b5n7s86zsc68c404czzfff7lg3w"))))
    (build-system gnu-build-system)
    (synopsis "C/C++ configuration file library")
    (description
     "Libconfig is a simple library for manipulating structured configuration
files.  This file format is more compact and more readable than XML.  And
unlike XML, it is type-aware, so it is not necessary to do string parsing in
application code.")
    (license license:lgpl2.1+)))

(define-public pfff
  (package
    (name "pfff")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/pfff/pfff")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nxkfm7zliq3rmr7yp871sppwfnz71iz364m2sgazny71pzykggc"))))
    (build-system cmake-build-system)
    (home-page "http://biit.cs.ut.ee/pfff/")
    (synopsis "Probabilistic fast file fingerprinting tool")
    (description
     "pfff is a tool for calculating a compact digital fingerprint of a file
by sampling randomly from the file instead of reading it in full.
Consequently, the computation has a flat performance characteristic,
correlated with data variation rather than file size.  pfff can be as reliable
as existing hashing techniques, with provably negligible risk of collisions.")
    (license license:bsd-3)))

(define-public oniguruma
  (package
    (name "oniguruma")
    (version "6.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/kkos/"
                                  "oniguruma/releases/download/v" version
                                  "/onig-" version ".tar.gz"))
              (sha256
               (base32
                "0pvj37r1rd5h5vw99mdk8z4k44gq1ldwrapkamdiicksdfkr4ndb"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/kkos/oniguruma")
    (synopsis "Regular expression library")
    (description "Oniguruma is a regular expressions library.  The special
characteristic of this library is that different character encoding for every
regular expression object can be specified.")
    (license license:bsd-2)))

(define-public antiword
  (package
    (name "antiword")
    (version "0.37")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.winfield.demon.nl/linux"
                                  "/antiword-" version ".tar.gz"))
              (sha256
               (base32
                "1b7mi1l20jhj09kyh0bq14qzz8vdhhyf35gzwsq43mn6rc7h0b4f"))
              (patches (search-patches "antiword-CVE-2014-8123.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests
       #:make-flags
       (list "-f" "Makefile.Linux"
             (string-append "GLOBAL_INSTALL_DIR="
                            (assoc-ref %outputs "out") "/bin")
             (string-append "GLOBAL_RESOURCES_DIR="
                            (assoc-ref %outputs "out") "/share/antiword"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Ensure that mapping files can be found in the actual package
             ;; data directory.
             (substitute* "antiword.h"
               (("/usr/share/antiword")
                (string-append (assoc-ref outputs "out") "/share/antiword")))
             #t))
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" `("global_install" ,@make-flags)))))))
    (home-page "http://www.winfield.demon.nl/")
    (synopsis "Microsoft Word document reader")
    (description "Antiword is an application for displaying Microsoft Word
documents.  It can also convert the document to PostScript or XML.  Only
documents made by MS Word version 2 and version 6 or later are supported.  The
name comes from: \"The antidote against people who send Microsoft Word files
to everybody, because they believe that everybody runs Windows and therefore
runs Word\".")
    (license license:gpl2+)))

(define-public catdoc
  (package
    (name "catdoc")
    (version "0.95")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.wagner.pp.ru/pub/catdoc/"
                                  "catdoc-" version ".tar.gz"))
              (patches (search-patches "catdoc-CVE-2017-11110.patch"))
              (sha256
               (base32
                "15h7v3bmwfk4z8r78xs5ih6vd0pskn0rj90xghvbzdjj0cc88jji"))))
    (build-system gnu-build-system)
    ;; TODO: Also build `wordview` which requires `tk` – make a separate
    ;; package for this.
    (arguments
     '(#:tests? #f ; There are no tests
       #:configure-flags '("--disable-wordview")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'fix-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/man/man1"))))))))
    (home-page "http://www.wagner.pp.ru/~vitus/software/catdoc/")
    (synopsis "MS-Word to TeX or plain text converter")
    (description "@command{catdoc} extracts text from MS-Word files, trying to
preserve as many special printable characters as possible.  It supports
everything up to Word-97. Also supported are MS Write documents and RTF files.

@command{catdoc} does not preserve complex word formatting, but it can
translate some non-ASCII characters into TeX escape codes.  It's goal is to
extract plain text and allow you to read it and, probably, reformat with TeX,
according to TeXnical rules.

This package also provides @command{xls2csv}, which extracts data from Excel
spreadsheets and outputs it in comma-separated-value format, and
@command{catppt}, which extracts data from PowerPoint presentations.")
    (license license:gpl2+)))

(define-public utfcpp
  (package
    (name "utfcpp")
    (version "2.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/nemtrif/utfcpp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gr98d826z6wa58r1s5i7rz7q2x3r31v7zj0pjjlrc7gfxwklr4s"))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install              ; no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include (string-append out "/include"))
                    (doc (string-append out "/share/doc/" ,name)))
               (copy-recursively "source" include)
               (install-file "README.md" doc)
               #t))))))
    (home-page "https://github.com/nemtrif/utfcpp")
    (synopsis "Portable C++ library for handling UTF-8")
    (description "UTF8-CPP is a C++ library for handling UTF-8 encoded text
in a portable way.")
    (license license:boost1.0)))

(define-public dbacl
  (package
    (name "dbacl")
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.lbreyer.com/gpl/"
                           "dbacl-" version ".tar.gz"))
       (sha256
        (base32
         "0224g6x71hyvy7jikfxmgcwww1r5lvk0jx36cva319cb9nmrbrq7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
        (string-append "-I" (assoc-ref %build-inputs "slang")
                       "/include/slang")
        (string-append "-I" (assoc-ref %build-inputs "ncurses")
                       "/include/ncurses"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-sample6-and-japanese
           (lambda _
             (substitute* "doc/Makefile.am"
               (("sample6.txt") "")
               (("japanese.txt") ""))
             (delete-file "doc/sample6.txt")
             (delete-file "doc/japanese.txt")
             (substitute* (list "src/tests/Makefile.am"
                                "src/tests/Makefile.in")
               (("dbacl-jap.shin") "")
               (("dbacl-jap.sh") ""))
             #t))
         (add-after 'unpack 'delete-test
           ;; See comments about the license.
           (lambda _
             (delete-file "src/tests/dbacl-jap.shin")
             #t))
         (add-after 'delete-sample6-and-japanese 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vif")
             #t))
         (add-after 'unpack 'fix-test-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (substitute* (find-files "src/tests/" "\\.shin$")
                 (("PATH=/bin:/usr/bin")
                  "#PATH=/bin:/usr/bin")
                 (("diff") (string-append (which "diff")))
                 (("tr") (string-append (which "tr"))))
               #t))))))
    (inputs
     `(("ncurses" ,ncurses)
       ("perl" ,perl)
       ("readline" ,readline)
       ("slang" ,slang)))
    (native-inputs
     `(("libtool" ,libtool)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.lbreyer.com/dbacl.html")
    (synopsis "Bayesian text and email classifier")
    (description
     "dbacl is a fast Bayesian text and email classifier.  It builds a variety
of language models using maximum entropy (minimum divergence) principles, and
these can then be used to categorize input data automatically among multiple
categories.")
    ;; The software is licensed as GPLv3 or later, but
    ;; includes various sample texts in the doc dir:
    ;; - sample1.txt, sample3 and sampe5.txt are in the public domain,
    ;;   by Mark Twain.
    ;; - sample2.txt, sample4.txt are in the public domain, by Aristotle.
    ;; - sample6.txt is a forwarded email, copyright unknown.
    ;;   Guix does exclude sample6.txt.
    ;; - japanese.txt is a Japanese unoffical translation of the
    ;;   GNU General Public License, (c) by the Free Software Foundation.
    ;;   Guix excludes this file.
    (license (list license:gpl3+ license:public-domain))))

(define-public dotconf
  (package
    (name "dotconf")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/williamh/dotconf.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sc95hw5k2xagpafny0v35filmcn05k1ds5ghkldfpf6xw4hakp7"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f))  ; FIXME maketest.sh does not work.
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/williamh/dotconf")
    (synopsis "Configuration file parser library")
    (description
     "C library for creating and parsing configuration files.")
    (license (list license:lgpl2.1         ; Main distribution.
                   license:asl1.1))))      ; src/readdir.{c,h}

(define-public java-rsyntaxtextarea
  (package
    (name "java-rsyntaxtextarea")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bobbylight/"
                                  "RSyntaxTextArea/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c5mqg2klj5rvf8fhycrli8rf6s37l9p7a8knw9gpp65r1c120q2"))))
    (build-system ant-build-system)
    (arguments
     `(;; FIXME: some tests fail because locale resources cannot be found.
       ;; Even when I add them to the class path,
       ;; RSyntaxTextAreaEditorKitDumbCompleteWordActionTest fails.
       #:tests? #f
       #:jar-name "rsyntaxtextarea.jar"))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "https://bobbylight.github.io/RSyntaxTextArea/")
    (synopsis "Syntax highlighting text component for Java Swing")
    (description "RSyntaxTextArea is a syntax highlighting, code folding text
component for Java Swing.  It extends @code{JTextComponent} so it integrates
completely with the standard @code{javax.swing.text} package.  It is fast and
efficient, and can be used in any application that needs to edit or view
source code.")
    (license license:bsd-3)))

;; We use the sources from git instead of the tarball from pypi, because the
;; latter does not include the Cython source file from which bycython.cpp is
;; generated.
(define-public python-editdistance
  (let ((commit "3ea84a7dd3258c76aa3be851ef3d50e59c886846")
        (revision "1"))
    (package
      (name "python-editdistance")
      (version (string-append "0.3.1-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aflc/editdistance.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1l43svsv12crvzphrgi6x435z6xg8m086c64armp8wzb4l8ccm7g"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'build-cython-code
             (lambda _
               (with-directory-excursion "editdistance"
                 (delete-file "bycython.cpp")
                 (invoke "cython" "--cplus" "bycython.pyx")))))))
      (native-inputs
       `(("python-cython" ,python-cython)))
      (home-page "https://www.github.com/aflc/editdistance")
      (synopsis "Fast implementation of the edit distance (Levenshtein distance)")
      (description
       "This library simply implements Levenshtein distance algorithm with C++
and Cython.")
      (license license:expat))))

(define-public go-github.com-mattn-go-runewidth
  (let ((commit "703b5e6b11ae25aeb2af9ebb5d5fdf8fa2575211")
        (version "0.0.4")
        (revision "1"))
    (package
      (name "go-github.com-mattn-go-runewidth")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mattn/runewidth")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0znpyz71gajx3g0j2zp63nhjj2c07g16885vxv4ykwnrfmzbgk4w"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/mattn/go-runewidth"))
      (synopsis "@code{runewidth} provides Go functions to work with string widths")
      (description
       "The @code{runewidth} library provides Go functions for padding,
measuring and checking the width of strings, with support for East Asian
text.")
      (home-page "https://github.com/mattn/runewidth")
      (license license:expat))))

(define-public docx2txt
  (package
    (name "docx2txt")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/docx2txt/docx2txt/v"
                    version "/docx2txt-" version ".tgz"))
              (sha256
               (base32
                "06vdikjvpj6qdb41d8wzfnyj44jpnknmlgbhbr1w215420lpb5xj"))))
    (build-system gnu-build-system)
    (inputs
     `(("unzip" ,unzip)
       ("perl" ,perl)))
    (arguments
     `(#:tests? #f                      ; No tests.
       #:make-flags (list (string-append "BINDIR="
                                         (assoc-ref %outputs "out") "/bin")
                          (string-append "CONFIGDIR="
                                         (assoc-ref %outputs "out") "/etc")
                          ;; Makefile seems to be a bit dumb at guessing.
                          (string-append "INSTALL=install")
                          (string-append "PERL=perl"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'fix-install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (config (string-append out "/etc/docx2txt.config"))
                    (unzip (assoc-ref inputs "unzip")))
               ;; According to INSTALL, the .sh wrapper can be skipped.
               (delete-file (string-append bin "/docx2txt.sh"))
               (rename-file (string-append bin "/docx2txt.pl")
                            (string-append bin "/docx2txt"))
               (substitute* config
                 (("config_unzip         => '/usr/bin/unzip',")
                  (string-append "config_unzip         => '"
                                 unzip
                                 "/bin/unzip',")))
               ;; Makefile is wrong.
               (chmod config #o644)
               #t))))))
    (synopsis "Recover text from @file{.docx} files, with good formatting")
    (description
     "@command{docx2txt} is a Perl based command line utility to convert
Microsoft Office @file{.docx} documents to equivalent text documents.  Latest
version supports following features during text extraction.

@itemize
@item Character conversions; currency characters are converted to respective
names like Euro.
@item Capitalisation of text blocks.
@item Center and right justification of text fitting in a line of
(configurable) 80 columns.
@item Horizontal ruler, line breaks, paragraphs separation, tabs.
@item Indicating hyperlinked text along with the hyperlink (configurable).
@item Handling (bullet, decimal, letter, roman) lists along with (attempt at)
indentation.
@end itemize\n")
    (home-page "http://docx2txt.sourceforge.net")
    (license license:gpl3+)))

(define-public odt2txt
  (package
    (name "odt2txt")
    (version "0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dstosberg/odt2txt/")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0im3kzvhxkjlx57w6h13mc9584c74ma1dyymgvpq2y61av3gc35v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no make check
       #:make-flags (list "CC=gcc"
                          (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/dstosberg/odt2txt/")
    (synopsis "Converter from OpenDocument Text to plain text")
    (description "odt2txt is a command-line tool which extracts the text out
of OpenDocument Texts, as produced by OpenOffice.org, KOffice, StarOffice and
others.

odt2txt can also extract text from some file formats similar to OpenDocument
Text, such as OpenOffice.org XML (*.sxw), which was used by OpenOffice.org
version 1.x and older StarOffice versions.  To a lesser extent, odt2txt may be
useful to extract content from OpenDocument spreadsheets (*.ods) and
OpenDocument presentations (*.odp).")
    (license license:gpl2)))

(define-public opencc
  (package
    (name "opencc")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/BYVoid/OpenCC")
              (commit (string-append "ver." version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pv5md225qwhbn8ql932zdg6gh1qlx3paiajaks8gfsa07yzvhr4"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; TODO: Unbundle tclap, darts-clone, gtest
           (delete-file-recursively "deps/rapidjson-0.11") #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-3rd-party-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((rapidjson (assoc-ref inputs "rapidjson")))
               (substitute* "src/CMakeLists.txt"
                 (("../deps/rapidjson-0.11")
                  (string-append rapidjson "/include/rapidjson")))
             #t))))))
    (native-inputs
     `(("python" ,python-wrapper)
       ("rapidjson" ,rapidjson)))
    (home-page "https://github.com/BYVoid/OpenCC")
    (synopsis "Convert between Traditional Chinese and Simplified Chinese")
    (description "Open Chinese Convert (OpenCC) converts between Traditional
Chinese and Simplified Chinese, supporting character-level conversion,
phrase-level conversion, variant conversion, and regional idioms among
Mainland China, Taiwan, and Hong-Kong.")
    (license license:asl2.0)))

(define-public nkf
  (let ((commit "08043eadf4abdddcf277842217e3c77a24740dc2")
        (revision "1"))
    (package
      (name "nkf")
      ;; The commits corresponding to specific versions are published
      ;; here:
      ;; https://ja.osdn.net/projects/nkf/scm/git/nkf/
      (version "2.1.5")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nurse/nkf.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0anw0knr1iy4p9w3d3b3pbwzh1c43p1i2q4c28kw9zviw8kx2rly"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; test for perl module
         #:make-flags (list "CC=gcc" "CFLAGS=-O2 -Wall -pedantic"
                            (string-append "prefix=" %output)
                            "MKDIR=mkdir -p")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)))) ; No ./configure script
      (home-page "https://ja.osdn.net/projects/nkf/")
      (synopsis "Network Kanji Filter")
      (description "Nkf is yet another kanji code converter among networks,
hosts and terminals.  It converts input kanji code to designated kanji code
such as ISO-2022-JP, Shift_JIS, EUC-JP, UTF-8, UTF-16 or UTF-32.")
      (license license:zlib))))

(define-public python-pandocfilters
  (package
    (name "python-pandocfilters")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandocfilters" version))
       (sha256
        (base32
         "1a8d9b7s48gmq9zj0pmbyv2sivn5i7m6mybgpkk4jm5vd7hp1pdk"))))
    (build-system python-build-system)
    (home-page "https://github.com/jgm/pandocfilters")
    (synopsis "Python module for writing Pandoc filters")
    (description "Pandoc is a powerful utility to transform various
input formats into a wide range of output formats.  To alter the
exported output document, Pandoc allows the usage of filters, which
are pipes that read a JSON serialization of the Pandoc AST from stdin,
transform it in some way, and write it to stdout.  It allows therefore
to alter the processing of Pandoc's supported input formats, for
instance one can add new syntax elements to markdown, etc.

This package provides Python bindings.")
    (license license:bsd-3)))
