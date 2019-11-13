;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages wxwidgets)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages video)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define-public wxwidgets
  (package
    (name "wxwidgets")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/wxWidgets/wxWidgets/"
                           "releases/download/v" version
                           "/wxWidgets-" version ".tar.bz2"))
       (sha256
        (base32 "1w7pgfqjab7n84lc4aarydl3g55d1hdgl2ilwml766r6inc7y5cn"))))
    (build-system glib-or-gtk-build-system)
    (inputs
     `(("glu" ,glu)
       ;; XXX gstreamer-0.10 builds fail
       ;; ("gstreamer" ,gstreamer-0.10)
       ("gtk" ,gtk+)
       ("libjpeg" ,libjpeg)
       ("libmspack" ,libmspack)
       ("libsm" ,libsm)
       ("libtiff" ,libtiff)
       ("mesa" ,mesa)
       ("webkitgtk" ,webkitgtk)
       ("sdl" ,sdl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       '("--with-regex" "--with-libmspack"
         "--with-sdl"
         "--enable-webview"
         "--enable-webkit"
         "--enable-webviewwebkit"
         ,@(if (string=? "aarch64-linux"
                         (%current-system))
             '("--build=aarch64-unknown-linux-gnu")
             '()))
       #:make-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       ;; No 'check' target.
       #:tests? #f))
    (home-page "https://www.wxwidgets.org/")
    (synopsis "Widget toolkit for creating graphical user interfaces")
    (description
     "wxWidgets is a C++ library that lets developers create applications with
a graphical user interface.  It has language bindings for Python, Perl, Ruby
and many other languages.")
    (license (list l:lgpl2.0+ (l:fsf-free "file://doc/license.txt")))))

(define-public wxwidgets-2
  (package
    (inherit wxwidgets)
    (version "2.8.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/wxWidgets/wxWidgets/"
                           "releases/download/v" version
                           "/wxGTK-" version ".tar.gz"))
       (sha256
        (base32 "1gjs9vfga60mk4j4ngiwsk9h6c7j22pw26m3asxr1jwvqbr8kkqk"))))
    (inputs
     `(("gtk" ,gtk+-2)
       ("libjpeg" ,libjpeg)
       ("libtiff" ,libtiff)
       ("libmspack" ,libmspack)
       ("sdl" ,sdl)
       ("unixodbc" ,unixodbc)))
    (arguments
     `(#:configure-flags
       '("--enable-unicode" "--with-regex=sys" "--with-sdl")
       #:make-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       ;; No 'check' target.
       #:tests? #f))))

(define-public wxwidgets-gtk2
  (package (inherit wxwidgets)
           (inputs `(("gtk+" ,gtk+-2)
                     ,@(alist-delete
                        "gtk+"
                        (package-inputs wxwidgets))))
           (name "wxwidgets-gtk2")))

;; Development version of wxWidgets, required to build against gstreamer-1.x.
;; This can be removed when wxWidgets is updated to the next stable version.
(define-public wxwidgets-3.1
  (package (inherit wxwidgets)
           (version "3.1.0")
           (source
            (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wxWidgets/wxWidgets.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "wxwidgets" version))
              (sha256
               (base32
                "14kl1rsngm70v3mbyv1mal15iz2b18k97avjx8jn7s81znha1c7f"))))
           (inputs `(("gstreamer" ,gstreamer)
                     ("gst-plugins-base" ,gst-plugins-base)
                     ,@(package-inputs wxwidgets)))
           (arguments
            (substitute-keyword-arguments (package-arguments wxwidgets)
              ((#:configure-flags flags)
               `(cons "--enable-mediactrl" ,flags))))))

(define-public wxwidgets-gtk2-3.1
  (package (inherit wxwidgets-3.1)
           (inputs `(("gtk+" ,gtk+-2)
                     ,@(alist-delete
                        "gtk+"
                        (package-inputs wxwidgets-3.1))))
           (name "wxwidgets-gtk2")))

(define-public python-wxpython
  (package
    (name "python-wxpython")
    (version "4.0.7.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wxPython" version))
       (sha256
        (base32
         "1jppcr3n428m8pgwb9q3g0iiqydxd451ncri4njk8b53xsiflhys"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled wxwidgets
           (delete-file-recursively "ext/wxWidgets")
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "WXWIN" (assoc-ref inputs "wxwidgets"))
             ;; Copy the waf executable to the source directory since it needs
             ;; to be in a writable directory.
             (copy-file (string-append (assoc-ref inputs "python-waf") "/bin/waf")
                        "bin/waf")
             (setenv "WAF" "bin/waf")
             ;; The build script tries to copy license files from the
             ;; wxwidgets source tree. Prevent it.
             (substitute* "wscript"
               (("updateLicenseFiles\\(cfg\\)" all)
                (string-append "#" all)))
             ;; The build script tries to write to demo/version.py. So, we set
             ;; correct write permissions.
             (chmod "demo/version.py" #o644)
             ;; Build only the python bindings, not wxwidgets also.
             (substitute* "setup.py"
               (("'build']") "'build_py', '--use_syswx']"))
             #t)))))
    (inputs
     `(("gtk+" ,gtk+)
       ("wxwidgets" ,wxwidgets)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-waf" ,python-waf)))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-six" ,python-six)))
    (home-page "http://wxPython.org/")
    (synopsis "Cross platform GUI toolkit for Python")
    (description "wxPython is a cross-platform GUI toolkit for the Python
programming language.  It is implemented as a set of Python extension modules
that wrap the GUI components of the popular wxWidgets cross platform C++
library.  In most cases, wxPython uses the native widgets on each platform to
provide a 100% native look and feel for the application.")
    (license l:wxwindows3.1+)))

(define-public python2-wxpython
  (package
    (name "python2-wxpython")
    (version "3.0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/wxpython/wxPython/"
                            version "/wxPython-src-" version ".tar.bz2"))
        (sha256
         (base32
          "0qfzx3sqx4mwxv99sfybhsij4b5pc03ricl73h4vhkzazgjjjhfm"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (lambda (folder)
              (delete-file-recursively (string-append "src/" folder))
              '("expat" "jpeg" "png" "tiff" "zlib" "msw" "osx" "msdos"))
            (substitute* '("wxPython/setup.py")
              ;; setup.py tries to keep its own license the same as wxwidget's
              ;; license (which it expects under $WXWIN/docs).
              (("'preamble.txt', 'licence.txt', 'licendoc.txt', 'lgpl.txt'")
               ""))
            #t))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f ; tests fail
       ;; wxPython directly extends distutils command classes,
       ;; we can't easily make setup.py use setuptools.
       #:use-setuptools? #f
       #:configure-flags (list "WXPORT=gtk2"
                               "UNICODE=1")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'chdir
           (lambda _
             (chdir "wxPython")
             #t))
         (add-after 'chdir 'set-wx-out-dir
           (lambda* (#:key outputs #:allow-other-keys)
             ;; By default, install phase tries to copy the wxPython headers in
             ;; gnu/store/...-wxwidgets-3.0.2 , which it can't, so they are
             ;; redirected to the output directory by setting WXPREFIX.
             (substitute* "config.py"
               (("= getWxConfigValue\\('--prefix'\\)")
                (string-append "= '" (assoc-ref outputs "out") "'")))
             (substitute* "wx/build/config.py"
               (("= getWxConfigValue\\('--prefix'\\)")
                (string-append "= '" (assoc-ref outputs "out") "'")))
             #t))
         (add-after 'set-wx-out-dir 'setenv
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "WXWIN" (assoc-ref inputs "wxwidgets"))
             (use-modules (ice-9 popen) (ice-9 rdelim))
             (let ((port (open-pipe* OPEN_READ
                                     (string-append (assoc-ref inputs "wxwidgets")
                                                    "/bin/wx-config") "--cppflags")))
               (setenv "CPPFLAGS" (read-string port))
               (close-pipe port))
             #t)))))
    (native-inputs
     `(("mesa" ,mesa) ; for glcanvas
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+-2) ; for wxPython/src/helpers.cpp
       ("wxwidgets" ,wxwidgets-gtk2)))
    (synopsis "Python 2 Bindings for wxWidgets")
    (description "@code{wxpython} provides Python 2 bindings for wxWidgets.")
    (home-page "http://wxpython.org/")
    (license (package-license wxwidgets))))

(define-public wxsvg
  (package
    (name "wxsvg")
    (version "1.5.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/wxsvg/wxsvg/"
                            version "/wxsvg-" version ".tar.bz2"))
       (sha256
        (base32
         "1hn3h9kzsjs4wimlpknzjfgn7q0n792hh7v3mshjgsjxdcrckzan"))))
    (build-system glib-or-gtk-build-system)
    (inputs
     `(("wxwidgets" ,wxwidgets-3.1)
       ("cairo" ,cairo)
       ("pango" ,pango)
       ("libexif" ,libexif)
       ("ffmpeg" ,ffmpeg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "C++ library to create, manipulate and render SVG files")
    (description "wxSVG is a C++ library to create, manipulate and render
@dfn{Scalable Vector Graphics} (SVG) files with the wxWidgets toolkit.")
    (home-page "http://wxsvg.sourceforge.net")

    ;; wxSVG is licenced under the "wxWindows library licence", which is
    ;; the LGPL2.0+, with a few extra permissions.
    (license (list l:lgpl2.0+ (l:fsf-free "file://COPYING")))))
