;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2023, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Pkill -9 <pkill9@runbox.com>
;;; Copyright © 2020, 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 Tomasz Jeneralczyk <tj@schwi.pl>
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

(define-module (gnu packages animation)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph))

(define-public rlottie
  (package
    (name "rlottie")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Samsung/rlottie.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "10bxr1zf9wxl55d4cw2j02r6sgqln7mbxplhhfvhw0z92fi40kr3"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list
        "-Dlog=true"
        "-Dtest=true"
        "-Dcpp_std=gnu++17")))
    (native-inputs
     (list googletest pkg-config))
    (synopsis "Lottie Animation Library")
    (description "Rlottie is a platform independent standalone c++ library for
rendering vector based animations and art in realtime.")
    (home-page "https://github.com/Samsung/rlottie/")
    (license license:expat)))

;; ETL, synfig, and Synfig Studio are updated in tandem.
(define synfig-version "1.5.3")

(define-public etl
  (package
    (name "etl")
    (version synfig-version)
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/synfig/synfig"
                                  "/releases/download/v" version
                                  "/ETL-" version ".tar.gz"))
              (sha256
               (base32
                "1gq63fj714a66g7fz3016ki1c73362sj73fy5005iyy1phn4s3v4"))))
    (build-system gnu-build-system)
    (inputs (list glibmm-2.66))
    (native-inputs (list pkg-config))
    (home-page "https://www.synfig.org")
    (synopsis "Extended C++ template library")
    (description
     "ETL is a class and template library designed to add new datatypes and
functions which combine well with the existing types and functions from the
C++ @dfn{Standard Template Library} (STL).")
    (license license:gpl3+)))

(define-public synfig
  (package
    (name "synfig")
    (version synfig-version)
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/synfig/synfig"
                                  "/releases/download/v" version
                                  "/synfig-" version ".tar.gz"))
              (sha256
               (base32
                "1a0ya2cdm5rskrhn033jwsg2ycg0b9dpqq5kndnzvn2sdvp9qg4i"))))
    (build-system gnu-build-system)
    (inputs
     (list boost
           libdv
           libjpeg-turbo
           libpng
           libmng
           zlib))
    ;; synfig.pc lists the following as required: Magick++ libavcodec
    ;; libavformat libswscale freetype2 fontconfig OpenEXR ETL glibmm-2.4
    ;; giomm-2.4 libxml++-2.6 sigc++-2.0 cairo fftw3 pango pangocairo mlt++
    (propagated-inputs
     (list cairo
           etl
           ffmpeg
           fftw
           fontconfig
           freetype
           glibmm-2.66 ; latest possible version as of synfig 1.5.3
           imagemagick
           libxml++-2 ; libxml <= 2 required as of synfig 1.5.3
           libsigc++
           mlt
           openexr
           pango))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://www.synfig.org")
    (synopsis "Vector-based 2D animation renderer")
    (description
     "Synfig is a vector-based 2D animation package.  It is designed to be
capable of producing feature-film quality animation.  It eliminates the need
for tweening, preventing the need to hand-draw each frame.")
    (license license:gpl3+)))

(define-public synfigstudio
  (package
    (name "synfigstudio")
    (version synfig-version)
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/synfig/synfig"
                                  "/releases/download/v" version
                                  "/synfigstudio-" version ".tar.gz"))
              (sha256
               (base32
                "1rdvyy5l8fl1gv60gvpw0zmbihdg2a048x8bwqbwj2za6qx00hvz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; This fixes the file chooser crash that happens with GTK 3.
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((gtk (assoc-ref inputs "gtk+"))
                     (gtk-share (string-append gtk "/share")))
                (wrap-program (string-append #$output "/bin/synfigstudio")
                  `("XDG_DATA_DIRS" ":" prefix (,gtk-share)))))))))
    (inputs
     (list bash-minimal gtkmm-3 gtk+ libsigc++ synfig))
    (native-inputs
     (list pkg-config intltool))
    (home-page "https://www.synfig.org")
    (synopsis "Vector-based 2D animation package (GUI)")
    (description
     "Synfig is a vector-based 2D animation package.  It is designed to
be capable of producing feature-film quality animation.  It eliminates the
need for tweening, preventing the need to hand-draw each frame.  This package
contains the graphical user interface for synfig.")
    (license license:gpl3+)))

;; This package provides a standalone (no browser plugin) version of
;; Lightspark.
(define-public lightspark
  (package
    (name "lightspark")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lightspark/lightspark")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j5rk7c9cg7bmnjmgsyn57h1gfdyjj76vjj6xl8j6y547p1adqnv"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;requires Adobe Flex SDK, see README.tests
       ;; Disable browser plugins because neither NPAPI nor PPAPI is
       ;; supported in the browsers we have.
       #:configure-flags
       '("-DCOMPILE_NPAPI_PLUGIN=FALSE"
         "-DCOMPILE_PPAPI_PLUGIN=FALSE")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-build-environment
           (lambda _
             ;; Use relative etc path.
             (substitute* "CMakeLists.txt" (("\\/etc") "etc"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./tests")))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           nasm
           perl
           pkg-config
           python-wrapper))
    (inputs
     (list cairo
           curl
           ffmpeg
           freeglut
           glew
           glibmm
           gnutls
           libjpeg-turbo
           openssl
           pango
           pcre2
           rtmpdump
           sdl2
           zlib))
    (home-page "https://lightspark.github.io/")
    (synopsis "Flash player implementation")
    (description
     "Lightspark is a Flash player implementation for playing files in the SWF
format.  It supports SWF files written on all versions of the ActionScript
language.")
    ;; NOTE: The bundled pugixml is a fork specific to Lightspark and
    ;; incompatible with the one we have.
    ;; FIXME: we also have jxrlib, but the build fails to find JXRMeta.h so we
    ;; use the bundled one for now.
    (license (list license:lgpl3+ ;lightspark
                   license:mpl2.0 ;avmplus
                   license:bsd-2 ;jxrlib
                   license:expat)))) ;pugixml, PerlinNoise

(define-public papagayo
  (let ((commit "e143684b30e59fe4a554f965cb655d23cbe93ee7")
        (revision "1"))
    (package
      (name "papagayo")
      (version (string-append "2.0b1-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/LostMoho/Papagayo")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1p9gffjhbph34jhrvgpg93yha75bf88vkvlnk06x1r9601ph5321"))
                (modules '((guix build utils)))
                ;; Delete bundled libsndfile sources.
                (snippet
                 '(begin
                    (delete-file-recursively "libsndfile_1.0.19")
                    (delete-file-recursively "libsndfile_1.0.25")))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((libsndfile (assoc-ref inputs "libsndfile")))
                  ;; Do not use bundled libsndfile sources
                  (substitute* "Papagayo.pro"
                    (("else \\{")
                     (string-append "\nINCLUDEPATH += " libsndfile
                                    "/include"
                                    "\nLIBS +=" libsndfile
                                    "/lib/libsndfile.so\n"
                                    "win32 {"))))
                (invoke "qmake"
                        (string-append "DESTDIR=" #$output "/bin"))))
            ;; Ensure that all required Qt plugins are found at runtime.
            (add-after 'install 'wrap-executable
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((qt '("qtbase" "qtmultimedia")))
                  (wrap-program (string-append #$output "/bin/Papagayo")
                    `("QT_PLUGIN_PATH" ":" prefix
                      ,(map (lambda (label)
                              (string-append (assoc-ref inputs label)
                                             "/lib/qt5/plugins/"))
                            qt)))))))))
      (inputs
       (list bash-minimal qtbase-5 qtmultimedia-5 libsndfile))
      (native-inputs
       (list qttools-5))
      (home-page "https://www.lostmarble.com/papagayo/")
      (synopsis "Lip-syncing for animations")
      (description
       "Papagayo is a lip-syncing program designed to help you line up
phonemes with the actual recorded sound of actors speaking.  Papagayo makes it
easy to lip sync animated characters by making the process very simple – just
type in the words being spoken, then drag the words on top of the sound’s
waveform until they line up with the proper sounds.")
      (license license:gpl3+))))

(define-public pencil2d
  (package
    (name "pencil2d")
    (version "0.6.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pencil2d/pencil")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b1nwiwyg01087q318vymg4si76dw41ykxbn2zwd6dqbxzbpr1dh"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal qtbase-5 qtxmlpatterns qtmultimedia-5 qtsvg-5))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda _
              ;; Its bundled catch2-2.5.0 fails to build with glibc-2.35.
              (copy-file #$(file-append catch2 "/include/catch2/catch.hpp")
                         "tests/src/catch.hpp")))
          (replace 'configure
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (invoke "qmake" (string-append "PREFIX=" out)))))
          (add-after 'install 'wrap-executable
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (plugin-path (getenv "QT_PLUGIN_PATH")))
                (wrap-program (string-append out "/bin/pencil2d")
                  `("QT_PLUGIN_PATH" ":" prefix (,plugin-path)))))))))
    (home-page "https://www.pencil2d.org")
    (synopsis "Make 2D hand-drawn animations")
    (description
     "Pencil2D is an easy-to-use and intuitive animation and drawing tool.  It
lets you create traditional hand-drawn animations (cartoons) using both bitmap
and vector graphics.")
    (license license:gpl2)))

(define-public tgs2png
  (let ((commit "25c15b7c2ca3b1a580a383d9d3cb13bf8531d04a")
        (revision "0"))
    (package
      (name "tgs2png")
      (version (git-version "0.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zevlg/tgs2png")
               (commit commit)))
         (sha256
          (base32 "0camvzapkfvr9v0nkk96n26rdmw0g8wbpv41i5l03j6bzdgm4myl"))
         (file-name (git-file-name "tgs2png" version))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (delete-file-recursively "rlottie")
             #t))))
      (arguments '(#:tests? #f))
      (build-system cmake-build-system)
      (inputs
       (list rlottie libpng))
      (native-inputs
       (list pkg-config))
      (home-page "https://github.com/zevlg/tgs2png")
      (synopsis "Convert Telegram's animated stickers to PNG")
      (description "Convert Telegram's animated stickers in TGS format into series of PNG images.")
      (license license:gpl3+))))

(define-public swftools
  ;; Last release of swftools was 0.9.2 on 2012-04-21 - it is really old and
  ;; does not compile with what's available in guix, master on the other hand
  ;; works.
  (let ((commit "c6a18ab0658286f98d6ed2b3d0419058e86a14a0")
        (revision "2"))
    (package
      (name "swftools")
      (version (git-version "0.9.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/matthiaskramm/swftools")
               (commit commit)))
         (sha256
                 (base32 "1hdfyprjjhhlba3gszaarf1rv3jynm4hs4v8kdys03qz4g8i53by"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; XXX: Swftools includes the source tarball of an old version of
             ;; xpdf.

             ;; To fix a linking error I followed the workaround in:
             ;; https://github.com/matthiaskramm/swftools/issues/178
             ;; and implented it as a two-step snippet because substitute*
             ;; does not match multiline regexes.
             (substitute* "lib/lame/quantize.c"
               ;; move inline keywords to the same line as their function headers
               (("^inline.*\n") "inline "))
             (substitute* "lib/lame/quantize.c"
               ;; make this particular function not inline
               (("inline (void bitpressure_strategy1)" _ f) f))))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f ; no rule for check
             #:parallel-build? #f))
      (inputs (list zlib freetype giflib libjpeg-turbo lame))
      (native-inputs (list perl))
      (home-page "http://www.swftools.org")
      (synopsis "Collection of utilities for working with Adobe Flash files")

      ;; XXX: This package will built all of swftools' tools but one: PDF2SWF,
      ;; purposefuly commented out of the description below.
      (description "SWFTools is a collection of utilities for working with
Adobe Flash files (SWF files).  The tool collection includes programs for
reading SWF files, combining them, and creating them from other content (like
images, sound files, videos or sourcecode).  The current collection is
 comprised of the programs detailed below:

@itemize
@comment PDF2SWF is not currentlybeing  build alongside other tools.  The next
@comment two lines should be uncommented if this will ever get fixed.
@comment @item
@comment @command{pdf2swf} A PDF to SWF Converter.

@item
@command{swfcombine} A multi-function tool for inserting, contatenating,
stacking and changing parameters in SWFs.

@item
@command{swfstrings} Scans SWFs for text data.
@item
@command{swfdump} Prints out various information about SWFs.

@item
@command{jpeg2swf} Takes one or more JPEG pictures and generates a SWF
slideshow from them.

@item
@command{png2swf} Like JPEG2SWF, only for PNGs.

@item
@command{gif2swf} Converts GIFs to SWF.  Also able to handle animated GIFs.

@item
@command{wav2swf} Converts WAV audio files to SWFs, using the LAME MP3
 encoder library.

@item
@command{font2swf} Converts font files (TTF, Type1) to SWF.

@item
@command{swfbbox} reads out, optimizes and readjusts SWF bounding boxes.

@item
@command{swfc} A tool for creating SWF files from simple script files.  Supports
both ActionScript 2.0 aand 3.0.

@item
@command{swfextract} extracts Movieclips, Sounds, Images etc. from SWF
 files.

@item
@command{as3compile} A standalone ActionScript 3.0 compiler.  Mostly compatible
 with Flex.
@end itemize")
      (license license:gpl2+))))
