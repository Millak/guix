;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2023 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2025 Lee Thompson <lee.p.thomp@gmail.com>
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

(define-module (gnu packages apl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite))

(define-public apl
  (let ((revision 1550))
    (package
      (name "apl")
      (version (string-append "1.8-r" (number->string revision)))
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "svn://svn.savannah.gnu.org/apl/trunk")
               (revision revision)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1bgc3a09f35zrqq2irhm1hspppnxjqas0fmcw14hkc7910br9ip3"))))
      (build-system gnu-build-system)
      (home-page "https://www.gnu.org/software/apl/")
      (inputs
       (list gettext-minimal
             openblas
             pcre2
             readline
             sqlite))
      (arguments
       (list #:configure-flags #~(list (string-append
                                        "--with-sqlite3="
                                        #$(this-package-input "sqlite")))
             #:phases
             #~(modify-phases %standard-phases
                 (add-before 'configure 'fix-configure
                   (lambda _
                     (substitute* "buildtag.sh"
                       ;; Don't exit on failed SVN-related calls.
                       (("^ +return 0\n") "")
                       ;; Manually set the SVN revision, since the directory is
                       ;; unversioned and we know it anyway.
                       (("^SVNINFO=.*")
                        (string-append "SVNINFO=" #$(number->string revision) "\n"))
                       ;; Requires running ‘svn info’ on a versioned directory.
                       (("\\\\\"\\$ARCHIVE_SVNINFO\\\\\"") "\\\"\\\"")))))))
      (synopsis "APL interpreter")
      (description
       "GNU APL is a free interpreter for the programming language APL.  It is
an implementation of the ISO standard 13751.")
      (license license:gpl3+))))

(define-public font-apl2741-unicode
  (let ((commit "1e11efae38e5095bfe49a786b111d563e83dad03"))
    (package
      (name "font-apl2741-unicode")
      (version "1668049300")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/abrudz/APL2741.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0i1yk1x99lr2swlbq9r7dny5w70zwiwi8lpfcw4n7k7pfbw0xh7y"))))
      (build-system trivial-build-system)
      (native-inputs (list fontforge))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((source (assoc-ref %build-inputs "source"))
                  (out (assoc-ref %outputs "out"))
                  (dest (string-append out "/share/fonts/truetype"))
                  (fontforge (string-append
                              (assoc-ref %build-inputs "fontforge")
                              "/bin/fontforge")))
             (mkdir-p dest)
             (invoke fontforge "-lang=ff" "-c" "Open($1); Generate($2)"
                     (string-append source "/APL2741.sfd")
                     (string-append dest "/APL2741.ttf"))))))
      (synopsis "APL2741 Unicode font")
      (home-page "https://abrudz.github.io/APL2741/")
      (description "APL font based on Adrian Smith's IBM Selectric APL2741
golf-ball font.  It supports most special characters used by popular APL
implementations, some additional mathematical and typographical symbols,
single line drawing characters, as well as the full Unicode APL range,
including both uppercase and lowercase underscored alphabets, as-of-yet unused
symbols, and almost all Latin-1 accented letters.")
      (license license:unlicense))))

(define-public font-apl333
  (package
    (name "font-apl333")
    ;; Version number as for apl-385, last modified 2013-04-20.
    (version "20130420")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://apl385.com/fonts/" "apl333.zip"))
       (sha256
        (base32 "0yn0ha7d14vp4ma3lxbc9kpyrn20m7brjisr6w55c9mi24w9v3a5"))))
    (build-system font-build-system)
    (home-page "https://apl385.com/fonts/index.htm")
    (synopsis "Variable-width APL font inspired by Comic Sans Serif")
    (description
     "Variable-width version of Adrian Smith's APL385 font developed with APL
software vendors in the late 1980s.")
    (license license:public-domain)))

(define-public font-apl385
  (package
    (name "font-apl385")
    ;; No version number or release, unzipping source and checking file times
    ;; shows the font file was last modified on 2016-08-21.
    (version "20160821")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://apl385.com/fonts/" "apl385.zip"))
       (sha256
        (base32 "132qfsnx0v6qf8x8iy3flivv449nz42nnpkwjysmz65w6wqxpk1g"))))
    (build-system font-build-system)
    (home-page "https://apl385.com/fonts/index.htm")
    (synopsis "Monospaced APL font inspired by Comic Sans Serif")
    (description
     "Adrian Smith's monospaced APL font developed with APL software vendors
in the late 1980s.")
    (license license:public-domain)))

(define-public dzaima-apl
  (package
    (name "dzaima-apl")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dzaima/APL.git")
               (commit (string-append "v" version))))
        (sha256
          (base32 "1hnrq0mlff6b9c9129afphcnmzd05wdyyfs905n421diyd5xa0il"))
        (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs (list bash-minimal openjdk18))
    (native-inputs (list `(,openjdk18 "jdk") zip))
    (arguments
     (list
       #:imported-modules `(,@%default-gnu-imported-modules
                            (guix build ant-build-system))
       #:modules `((guix build gnu-build-system)
                   ((guix build ant-build-system) #:prefix ant:)
                   (guix build utils)
                   (ice-9 ftw)
                   (ice-9 regex)
                   (srfi srfi-26))
       #:phases
       `(modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((javac   (search-input-file inputs "/bin/javac"))
                    (jar     (search-input-file inputs "/bin/jar")))
               (mkdir-p "src/build")
               (apply invoke javac "-encoding" "UTF-8" "-d" "src/build"
                      (let ((files '()))
                        (ftw "src/APL/"
                          (lambda (filename statinfo flags)
                            (if (string-match ".*\\.java" filename)
                              (set! files (cons filename files)))
                            #t))
                        files))
               (with-directory-excursion "src/build"
                 (invoke jar "--create" "--verbose"
                             "--file=dzaima-apl.jar"
                             "--main-class=APL.Main"
                             "APL")))))
         (delete 'check) ;; Upstream implements no tests
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (bin     (string-append out "/bin"))
                    (share   (string-append out "/share/java"))
                    (wrapper (string-append bin "/dzaima-apl")))
               (mkdir-p share)
               (mkdir-p bin)
               (install-file "src/build/dzaima-apl.jar" share)
               (with-output-to-file wrapper
                 (lambda _
                   (display (string-append
                              "#!" (search-input-file inputs "/bin/sh") "\n"
                              (search-input-file inputs "/bin/java")
                              " -jar " share "/dzaima-apl.jar \"$@\""))))
               (chmod wrapper #o555))))
         (add-after 'install 'reorder-jar-content
           (lambda* (#:key outputs #:allow-other-keys)
              (apply (assoc-ref ant:%standard-phases 'reorder-jar-content)
                     #:outputs (list outputs))))
         (add-after 'reorder-jar-content 'generate-jar-indices
           (lambda* (#:key outputs #:allow-other-keys)
              (apply (assoc-ref ant:%standard-phases 'generate-jar-indices)
                     #:outputs (list outputs))))
         (add-after 'generate-jar-indices 'reorder-jar-content
           (lambda* (#:key outputs #:allow-other-keys)
              (apply (assoc-ref ant:%standard-phases 'reorder-jar-content)
                     #:outputs (list outputs)))))))
    (home-page "https://github.com/dzaima/APL")
    (synopsis "Implementation of the APL programming language in Java")
    (description
     "This package provides an implementation of APL in Java, extended from
Dyalog APL.")
    (license license:expat)))
