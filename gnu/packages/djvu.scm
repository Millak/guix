;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021, 2025 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages djvu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public djvulibre
  (package
    (name "djvulibre")
    (version "3.5.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/djvu/DjVuLibre/"
                           (string-replace-substring version "." ",")
                           "/djvulibre-" version ".tar.gz"))
       (sha256
        (base32 "0manxn1ly5n8nqamv47hz7akxi6v0rzwc9j1c3x99vngrjlr5qw2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; This bundles software (e.g., zlib) and is entirely superfluous.
           (delete-file-recursively "win32")
           #t))))
    (build-system gnu-build-system)
    (native-inputs
     ;; The 3.5.28 release tarball isn't bootstrapped.
     (list autoconf automake libtool))
    (inputs
     (list libjpeg-turbo libtiff zlib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'bootstrap 'make-reproducible
           (lambda _
             ;; Ensure there are no timestamps in .svgz files.
             (substitute* "desktopfiles/Makefile.am"
               (("gzip") "gzip -n"))
             #t)))))
    (home-page "https://djvu.sourceforge.net/")
    (synopsis "Implementation of DjVu, the document format")
    (description "DjVuLibre is an implementation of DjVu,
including viewers, browser plugins, decoders, simple encoders, and
utilities.")
    (license license:gpl2+)))

(define-public djview
  (package
    (name "djview")
    (version "4.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/djvu/djview-git")
             (commit (string-append "release." version))))
       (sha256
        (base32 "0mn9ywjbc7iga50lbjclrk892g0x0rap0dmb6ybzjyarybdhhcxp"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config qttools-5))
    (inputs
     (list djvulibre glib libxt libtiff qtbase-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-desktop-file
           ;; Executable is "djview", not "djview4".
           (lambda _
             (substitute* "desktopfiles/djvulibre-djview4.desktop"
               (("Exec=djview4 %f") "Exec=djview %f"))
             #t))
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable
                       (find-files "."))
             #t)))))
    (home-page "https://djvu.sourceforge.net/djview4.html")
    (synopsis "Viewer for the DjVu image format")
    (description "DjView is a standalone viewer for DjVu files.

Its features include navigating documents, zooming and panning page images,
producing and displaying thumbnails, displaying document outlines, searching
documents for particular words in the hidden text layer, copying hidden text
to the clipboard, saving pages and documents as bundled or indirect multi-page
files, and printing page and documents.

The viewer can simultaneously display several pages using a side-by-side or
a continuous layout.")
    (license license:gpl2+)))

(define-public pdf2djvu
  (package
    (name "pdf2djvu")
    (version "0.9.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jwilk/pdf2djvu/releases/download/" version
             "/pdf2djvu-" version ".tar.xz"))
       (sha256
        (base32 "16ma6z62mqpb9q4cg784m3vqc1jx987g6kg8gyghg50m2f0a8igb"))))
    (build-system gnu-build-system)
    (native-inputs (list gettext-minimal pkg-config))
    (inputs
     (list djvulibre
           exiv2
           graphicsmagick
           poppler
           poppler-data
           `(,util-linux "lib")))       ;for libuuid
    (arguments
     `(#:test-target "test"
       #:tests? #f                                ;requires Python 2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "tests/test-xmp-broken.py"
               ;; Error message changed in recent versions of XML parser
               (("XML parsing failure")
                "Error in XMLValidator"))))
         (add-before 'check 'set-home-for-tests
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (synopsis "PDF to DjVu converter")
    (description
     "@code{pdf2djvu} creates DjVu files from PDF files.
It is able to extract:
@itemize
@item graphics,
@item text layer,
@item hyperlinks,
@item document outline (bookmarks),
@item metadata (including XMP metadata).
@end itemize\n")
    (home-page "https://jwilk.net/software/pdf2djvu")
    (license license:gpl2)))

(define-public djvu2pdf
  (package
    (name "djvu2pdf")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://0x2a.at/site/projects/djvu2pdf/djvu2pdf-"
                           version ".tar.gz"))
       (sha256
        (base32 "0v2ax30m7j1yi4m02nzn9rc4sn4vzqh5vywdh96r64j4pwvn5s5g"))))
    (build-system gnu-build-system)
    (inputs
     (list djvulibre
           gawk
           ghostscript
           grep
           ncurses
           which))
    (arguments
     `(#:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((djvulibre (assoc-ref inputs "djvulibre"))
                   (gawk (assoc-ref inputs "gawk"))
                   (ghostscript (assoc-ref inputs "ghostscript"))
                   (grep (assoc-ref inputs "grep"))
                   (ncurses (assoc-ref inputs "ncurses"))
                   (which (assoc-ref inputs "which")))
               (substitute* "djvu2pdf"
                 (("awk")
                  (string-append gawk "/bin/awk"))
                 (("ddjvu")
                  (string-append djvulibre "/bin/ddjvu"))
                 (("djvudump")
                  (string-append djvulibre "/bin/djvudump"))
                 (("grep")
                  (string-append grep "/bin/grep"))
                 (("gs")
                  (string-append ghostscript "/bin/gs"))
                 (("tput ")
                  (string-append ncurses "/bin/tput "))
                 (("which")
                  (string-append which "/bin/which"))))
             #t))
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref %outputs "out")))
               (install-file "djvu2pdf"
                             (string-append out "/bin"))
               (install-file "djvu2pdf.1.gz"
                             (string-append out "/share/man/man1"))
               #t))))))
    (synopsis "DjVu to PDF converter")
    (description "This is a small tool to convert DjVu files to PDF files.")
    (home-page "https://0x2a.at/site/projects/djvu2pdf/")
    (license license:gpl2+)))

(define-public minidjvu
  (package
    (name "minidjvu")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/minidjvu/minidjvu/"
                           version "/minidjvu-" version ".tar.gz"))
       (sha256
        (base32 "0jmpvy4g68k6xgplj9zsl6brg6vi81mx3nx2x9hfbr1f4zh95j79"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     (list libjpeg-turbo libtiff zlib))
    (arguments
     '(#:configure-flags '("--disable-static")
       #:parallel-build? #f
       #:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda _
             (substitute* "Makefile.in"
               (("/usr/bin/gzip")
                "gzip"))
             #t))
         (add-before 'install 'make-lib-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib"))
             #t)))))
    (synopsis "Black and white DjVu encoder")
    (description
     "@code{minidjvu} is a multipage DjVu encoder and single page
encoder/decoder.  It doesn't support colors or grayscales, just black
and white.")
    (home-page "https://sourceforge.net/projects/minidjvu/")
    (license license:gpl2)))

(define-public didjvu
  (let ((revision "0")
        (commit "43e2735d0b3575ca04e82a427dca88bc68fa4931"))
    (package
      (name "didjvu")
      (version (git-version "0.10.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/FriedrichFroebel/didjvu")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ippf3hsjy13xj6pqnqr30dz8lsncsfcan2r1vbxfk1g602m3p4c"))))
      (build-system python-build-system)
      (arguments
       `(;; FIXME: Tests fail because they try to load the libxmp and pyexiv2
         ;; modules that should not be enabled, as we only enable the gexiv2
         ;; module.
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'check 'disable-failing-test
             (lambda _
               (substitute* "tests/test_ipc.py"
                 ;; test_wait_signal gets stuck forever
                 (("self\\._test_signal\\(name\\)")
                  "return True")
                 ;; test_path fails to find a file it should have created
                 (("path = os\\.getenv\\('PATH'\\)")
                  "return True"))
               (substitute* "tests/test_timestamp.py"
                 ;; test_timezones fails with:
                 ;;   '2009-12-18T21:25:14Z' != '2009-12-18T22:25:14+01:00'
                 (("samples = \\[" all)
                  (string-append "return True\n        " all)))))
           (add-after 'install 'wrap-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (djvulibre (assoc-ref inputs "djvulibre")))
                 (wrap-program (string-append out "/bin/didjvu")
                   `("PATH" ":" prefix (,(string-append djvulibre "/bin"))))))))))
      (native-inputs (list python-nose))
      (inputs
       (list bash-minimal
             djvulibre
             gexiv2
             minidjvu
             python-gamera
             python-pillow
             python-pygobject
             python-wrapper))
      (synopsis "DjVu encoder with foreground/background separation")
      (description
       "@code{didjvu} uses the @code{Gamera} framework to separate the foreground
and background layers of images, which can then be encoded into a DjVu file.")
      (home-page "https://jwilk.net/software/didjvu")
      (license license:gpl2))))

(define-public ocrodjvu
  (let ((revision "0")
        (commit "0dd3364462fc77d5674b4457fcc8230835323c30"))
    (package
      (name "ocrodjvu")
      (version (git-version "0.12" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; Use the following fork repository, as upstream
                      ;; doesn't seem too concerned with Python 3
                      ;; compatibility.
                      (url "https://github.com/rmast/ocrodjvu")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0x64hg9ysrk8sismxb4jgk0sq7r9j90v2i9765xhmxpiy6f0lpni"))))
      (build-system gnu-build-system)
      (native-inputs
       (list (libc-utf8-locales-for-target) libxml2 python-nose python-pillow))
      (inputs
       (list bash-minimal
             djvulibre
             ocrad
             python-djvulibre
             python-future
             python-html5lib
             python-lxml
             python-pyicu
             python-regex
             python-wrapper
             tesseract-ocr))
      (arguments
       (list
        #:modules '((guix build gnu-build-system)
                    ((guix build python-build-system) #:prefix python:)
                    (guix build utils))
        #:imported-modules `(,@%default-gnu-imported-modules
                             (guix build python-build-system))
        #:test-target "test"
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (add-after 'unpack 'fix-for-python-3.11
              (lambda _
                (substitute* "lib/cli/ocrodjvu.py"
                  ;; The getargspec function has been removed in python 3.11.
                  (("init_args, _, _, _ = inspect.getargspec\\(cls.__init__\\)")
                   "init_args = inspect.getfullargspec(cls.__init__).args"))))
            (add-before 'check 'disable-failing-test
              (lambda _
                (substitute* "tests/test_ipc.py"
                  ;; test_wait_signal gets stuck forever
                  (("yield self\\._test_signal, name")
                   "return True")
                  ;; test_path fails to find a file it should have created
                  (("path = os\\.getenv\\('PATH'\\)\\.split\\(':'\\)")
                   "return True"))
                ;; Disable tests with tesseract. They can't work without
                ;; the language files that must downloaded by the final user
                ;; as they are not packaged in Guix.
                (substitute* "tests/ocrodjvu/test.py"
                  (("engines = stdout\\.getvalue\\(\\)\\.splitlines\\(\\)")
                   "engines = ['ocrad']"))
                (substitute* "tests/ocrodjvu/test_integration.py"
                  (("engines = 'tesseract', 'cuneiform', 'gocr', 'ocrad'")
                   "engines = 'ocrad'"))))
            (replace 'install
              (lambda _
                (invoke "make" "install"
                        "DESTDIR=" (string-append "PREFIX=" #$output))))
            (add-after 'install 'wrap-python
              (assoc-ref python:%standard-phases 'wrap))
            (add-after 'wrap-python 'wrap-path
              (lambda* (#:key outputs #:allow-other-keys)
                (for-each (lambda (file)
                            (wrap-program (search-input-file outputs file)
                              `("PATH" ":" prefix
                                (,(string-append
                                   #$(this-package-input "djvulibre") "/bin:"
                                   #$(this-package-input "ocrad") "/bin:"
                                   #$(this-package-input "tesseract-ocr")
                                   "/bin")))))
                          '("bin/djvu2hocr"
                            "bin/hocr2djvused"
                            "bin/ocrodjvu")))))))
      (synopsis "Program to perform OCR on DjVu files")
      (description
       "@code{ocrodjvu} is a wrapper for OCR systems, that allows you to perform
OCR on DjVu files.")
      (home-page "https://jwilk.net/software/ocrodjvu")
      (license license:gpl2))))
