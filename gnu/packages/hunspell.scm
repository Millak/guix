;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages hunspell)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages textutils))

(define-public hunspell
  (package
    (name "hunspell")
    (version "1.7.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hunspell/hunspell")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0qxlkd012r45ppd21kldbq9k5ac5nmxz290z6m2kch9l56v768k1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list perl))
    (native-search-paths (list (search-path-specification
                                (variable "DICPATH")
                                (files '("share/hunspell")))))
    (home-page "https://hunspell.github.io/")
    (synopsis "Spell checker")
    (description "Hunspell is a spell checker and morphological analyzer
library and program designed for languages with rich morphology and complex
word compounding or character encoding.")
    ;; Triple license, including "mpl1.1 or later".
    (license (list license:mpl1.1 license:gpl2+ license:lgpl2.1+))))

(define (dicollecte-french-dictionary variant synopsis)
  ;; Return a French dictionary package from dicollecte.org, for the given
  ;; VARIANT.
  (package
    (name (match variant
            ("classique" "hunspell-dict-fr")
            (_ (string-append "hunspell-dict-fr-" variant))))
    (version "6.2")
    (source (origin
              (uri (string-append
                    "http://www.dicollecte.org/download/fr/hunspell-french-dictionaries-v"
                    version ".zip"))
              (method url-fetch)
              (sha256
               (base32
                "139hfrn5p87sl8hqmgkf6sgvnxrk2mm8vd8xsm8sm98qjnwlg0f9"))))
    (build-system trivial-build-system)
    (native-inputs (list unzip))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let* ((out      (assoc-ref %outputs "out"))
                          (hunspell (string-append out "/share/hunspell"))
                          (myspell  (string-append out "/share/myspell"))
                          (doc      (string-append out "/share/doc/"
                                                   ,name))
                          (unzip    (assoc-ref %build-inputs "unzip")))
                     (invoke (string-append unzip "/bin/unzip")
                             (assoc-ref %build-inputs "source"))
                     (for-each (cut install-file <> hunspell)
                               (find-files "."
                                           ,(string-append variant
                                                           "\\.(dic|aff)$")))
                     (mkdir-p myspell)
                     (symlink hunspell (string-append myspell "/dicts"))
                     (for-each (cut install-file <> doc)
                               (find-files "." "\\.(txt|org|md)$"))
                     #t))))
    (synopsis synopsis)
    (description
     "This package provides a dictionary for the Hunspell spell-checking
library.")
    (home-page "https://www.dicollecte.org/home.php?prj=fr")
    (license license:mpl2.0)))

(define-syntax define-french-dictionary
  (syntax-rules (synopsis)
    ((_ name variant (synopsis text))
     (define-public name
       (dicollecte-french-dictionary variant text)))))

(define-french-dictionary hunspell-dict-fr-classique
  "classique"
  ;; TRANSLATORS: In French, this is "Français classique".
  (synopsis "Hunspell dictionary for ``classic'' French (recommended)"))

(define-french-dictionary hunspell-dict-fr-moderne
  "moderne"
  ;; TRANSLATORS: In French, this is "Français moderne".
  (synopsis "Hunspell dictionary for ``modern'' French"))

(define-french-dictionary hunspell-dict-fr-réforme-1990
  "reforme1990"
  (synopsis "Hunspell dictionary for the post @dfn{1990 réforme} French"))

(define-french-dictionary hunspell-dict-fr-toutes-variantes
  "toutesvariantes"
  (synopsis "Hunspell dictionary for all variants of French"))

(define-public hunspell-dict-pl
  (package
    (name "hunspell-dict-pl")
    (version "20200327")
    (source
     (origin
       (method url-fetch)
       ;; Since creators of dictionary host only the latest daily release,
       ;; we're using version mirrored by Arch Linux, which seems good
       ;; enough. They're mirroring hunspell-pl releases since 2011.
       (uri (string-append "https://sources.archlinux.org/other/community/"
                           "hunspell-pl/sjp-myspell-pl-"
                           version ".zip"))
       (sha256 (base32
                "14mzf8glxkp2775dcqisb1zv6r8ncm3bvzl46q352rwyl2dg1c59"))))

    (build-system trivial-build-system)
    (native-inputs (list unzip))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let* ((out      (assoc-ref %outputs "out"))
                          (hunspell (string-append out "/share/hunspell"))
                          (myspell  (string-append out "/share/myspell"))
                          (doc      (string-append out "/share/doc/"
                                                   ,name))
                          (unzip (search-input-file %build-inputs
                                                    "/bin/unzip")))
                     (invoke unzip "-j" "-o" (assoc-ref %build-inputs "source"))
                     (invoke unzip "-j" "-o" "pl_PL.zip")
                     (for-each (cut install-file <> hunspell)
                               (find-files "."
                                           ,(string-append "pl_PL"
                                                           "\\.(dic|aff)$")))
                     (mkdir-p myspell)
                     (symlink hunspell (string-append myspell "/dicts"))
                     (for-each (cut install-file <> doc)
                               (find-files "." "\\.(txt|org|md)$"))
                     #t))))
    (synopsis "Hunspell dictionary for Polish")
    (description
     "This package provides a dictionary for the Hunspell spell-checking
library.")
    (home-page "https://sjp.pl/slownik/ort/")
    (license
     (list license:gpl2 license:mpl1.1 license:cc-by4.0 license:lgpl2.1 license:asl2.0))))

(define-public hunspell-dict-de
  (package
    (name "hunspell-dict-de")
    (version "20161207")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.j3e.de/ispell/igerman98/dict/"
                           "igerman98-" version ".tar.bz2"))
       (sha256
        (base32 "1a3055hp2bc4q4nlg3gmg0147p3a1zlfnc65xiv2v9pyql1nya8p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("hunspell/de_DE.dic")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install              ;no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/hunspell/")))
               (install-file "hunspell/de_DE.aff" share)
               (install-file "hunspell/de_DE.dic" share)
               #t))))
       #:tests? #f))        ; no tests
    (native-inputs
     (list hunspell ispell perl))
    (synopsis "Hunspell dictionary for German (de_DE)")
    (description "This package provides a dictionary for the Hunspell
spell-checking library.")
    (home-page "https://www.j3e.de/ispell/igerman98/")
    (license (list license:gpl2 license:gpl3))))

(define-public hunspell-dict-hu
  (let ((revision "2")
        (major+minor "1.7"))
    (package
      (name "hunspell-dict-hu")
      (version (string-append major+minor "-" revision))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "mirror://sourceforge/magyarispell/Magyar Ispell/"
                             major+minor
                             "/magyarispell-" version ".tar.gz"))
         (sha256
          (base32 "0r22rvqrp5bzgr9sqyap82kibi5z9n6xy5b06si28idqijw7c772"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags
             #~(list "myspell"
                     "--jobs=1"     ;the Makefile is not ready for parallelism
                     (string-append "SH="
                                    (search-input-file %build-inputs
                                                       "/bin/bash"))
                     (string-append "AWK="
                                    (search-input-file %build-inputs
                                                       "/bin/awk")))
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'configure
                   (lambda* (#:key outputs #:allow-other-keys)
                     (substitute* "config"
                       (("/usr/bin/awk")
                        (which "awk")))))
                 (replace 'install                ;no install target
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (share (string-append out "/share/hunspell/")))
                       (install-file "hu_HU.aff" share)
                       (install-file "hu_HU.dic" share)))))
             #:tests? #f))                        ; no tests
      (native-inputs
       (list hunspell m4 recode))
      (synopsis "Hunspell dictionary for Hungarian (hu_HU)")
      (description "This package provides a dictionary for the Hunspell
spell-checking library.")
      (home-page "https://magyarispell.sourceforge.net/")
      (license (list license:gpl2 license:gpl3)))))

(define* (hunspell-dictionary dict-name full-name #:key synopsis home-page license)
  (package
    (name (string-append
           "hunspell-dict-"
           ;; Downcase and replace underscore in package names
           ;; to follow Guix naming conventions.
           (string-map (match-lambda
                         (#\_ #\-)
                         (chr chr))
                       (string-downcase dict-name))))
    (version "7.6.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://anongit.freedesktop.org/git/"
                                 "libreoffice/dictionaries.git/"))
             (commit
              (string-append "libreoffice-" version))))
       (file-name (git-file-name "libreoffice-dictionaries" version))
       (sha256
        (base32 "1f54z1kmpwv9s5a9jdgf97m43nhwbmsar0i6rri3qkgf3kkgz1f7"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("source" ,source)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((dictionary
                           (string-append (assoc-ref %build-inputs "source")
                                          "/" ,dict-name
                                          "/" ,dict-name))
                          (hunspell (string-append %output "/share/hunspell/"))
                          (myspell (string-append %output "/share/myspell")))
                     (for-each
                      (lambda (ext)
                        (install-file (string-append dictionary ext)
                                      hunspell))
                      '(".aff" ".dic"))
                     (symlink hunspell myspell)
                     #t))))
    (synopsis synopsis)
    (description "This package provides a dictionary for the Hunspell
spell-checking library.")
    (license license)
    (home-page home-page)))

(define-public hunspell-dict-he-il
  (let ((synopsis identity))
    (hunspell-dictionary "he_IL" "Hebrew"
                         #:synopsis (synopsis "Hunspell dictionary for Hebrew")
                         #:home-page "http://hspell.ivrix.org.il/"
                         #:license license:agpl3+)))

(define-public hunspell-dict-it-it
  (let ((synopsis identity))
    (hunspell-dictionary "it_IT" "Italian"
                         #:synopsis (synopsis "Hunspell dictionary for Italian")
                         #:home-page "https://www.libreitalia.org/"
                         #:license license:gpl3)))

;;;
;;; Hunspell packages made from the Aspell word lists.
;;;

(define* (aspell-word-list language synopsis
                           #:optional
                           (nick (string-map (lambda (chr)
                                               (if (char=? #\_ chr)
                                                   #\-
                                                   chr))
                                             (string-downcase language))))
  (package
    (name (string-append "hunspell-dict-" nick))
    (version "2018.04.16")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/wordlist/SCOWL/"
                    version "/scowl-" version ".tar.gz"))
              (sha256
               (base32
                "11lkrnhwrf5mvrrq45k4mads3n9aswgac8dc25ba61c75alxb5rs"))))
    (native-inputs
     (list tar gzip perl aspell))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "speller/README_en.txt.sh"
               (("\\bdate\\b") ""))))
         (delete 'configure)
         (delete 'check)
         (replace 'build
           (lambda _
             (substitute* "speller/make-hunspell-dict"
               (("zip -9 .*$")
                "return\n"))
             (mkdir "speller/hunspell")

             ;; XXX: This actually builds all the dictionary variants.
             (invoke "make" "-C" "speller" "hunspell")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out      (assoc-ref %outputs "out"))
                    (hunspell (string-append out "/share/hunspell"))
                    (myspell  (string-append out "/share/myspell"))
                    (doc      (string-append out "/share/doc/"
                                             ,name))
                    (dot-dic  ,(string-append "speller/" language ".dic")))
               (mkdir-p myspell)

               ;; Usually there's only a 'LANGUAGE.dic' file, but for the "en"
               ;; dictionary, there no 'en.dic'.  Instead, there's a set of
               ;; 'en*.dic' files, hence the 'find-files' call below.
               (if (file-exists? dot-dic)
                   (install-file dot-dic hunspell)
                   (for-each (lambda (dic)
                               (install-file dic hunspell))
                             (find-files "speller"
                                         ,(string-append language ".*\\.dic$"))))

               ;; Install affix files corresponding to installed dictionaries
               (for-each (lambda (dic)
                           (install-file (string-append
                                           "speller/" (basename dic ".dic") ".aff")
                                         hunspell))
                         (find-files hunspell ".*\\.dic$"))
               (symlink hunspell (string-append myspell "/dicts"))
               (for-each (lambda (file)
                           (install-file file doc))
                         (find-files "."
                                     "^(Copyright|.*\\.(txt|org|md))$"))
               #t))))))
    (synopsis synopsis)
    (description
     "This package provides a dictionary for the Hunspell spell-checking
library.")
    (home-page "http://wordlist.aspell.net/")
    (license (license:non-copyleft "file://Copyright"
                                   "Word lists come from several sources, all
under permissive licensing terms.  See the 'Copyright' file."))))

(define-syntax define-word-list-dictionary
  (syntax-rules (synopsis)
    ((_ name language (synopsis text))
     (define-public name
       (aspell-word-list language text)))
    ((_ name language nick (synopsis text))
     (define-public name
       (aspell-word-list language text nick)))))

(define-word-list-dictionary hunspell-dict-en
  "en"
  (synopsis "Hunspell dictionary for English"))

(define-word-list-dictionary hunspell-dict-en-au
  "en_AU"
  (synopsis "Hunspell dictionary for Australian English"))

(define-word-list-dictionary hunspell-dict-en-ca
  "en_CA"
  (synopsis "Hunspell dictionary for Canadian English"))

(define-word-list-dictionary hunspell-dict-en-gb
  "en_GB-ise" "en-gb"
  (synopsis "Hunspell dictionary for British English, with -ise endings"))

(define-word-list-dictionary hunspell-dict-en-gb-ize
  "en_GB-ize"
  (synopsis "Hunspell dictionary for British English, with -ize endings"))

(define-word-list-dictionary hunspell-dict-en-us
  "en_US"
  (synopsis "Hunspell dictionary for United States English"))

