;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Adam Kandur <kefironpremise@gmail.com>
;;; Copyright © 2021 Daniel Meißner <daniel.meissner-i4k@ruhr-uni-bochum.de>
;;; Copyright © 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2023 Adam Faiz <adam.faiz@disroot.org>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>

(define-module (gnu packages python-graphics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build-system python) #:select (pypi-uri))
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg))

;;; Commentary:
;;;
;;; Python modules which are meant to be used in GUI creation or mainly are
;;; bindings to low level libraries such as Glue, Mesa, OpenGL, Xorg etc.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public python-pyglet
  (package
    (name "python-pyglet")
    (version "2.0.18")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyglet" version))
       (sha256
        (base32 "1p4rizj81vfqxal288m3afbrgk0il3w7k5kmhai2sah8f26j7ybw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "tests"
              "--ignore=tests/interactive"
              ;; All tests errored
              "--ignore=tests/integration/media/test_openal.py"
              "--ignore=tests/integration/text/test_empty_document.py"
              ;; Tests failed
              "-k" (string-append "not test2"
                                  " and not test5"
                                  " and not testPad"
                                  " and not testPad"
                                  " and not testRange"
                                  " and not testRect"
                                  " and not testRect"
                                  " and not testSet"
                                  " and not testSquare"
                                  " and not testSquare"
                                  " and not testTuple"
                                  " and not testTupleRange"
                                  " and not test_context_noshare_texture"
                                  " and not test_context_share_texture"
                                  " and not test_method"
                                  " and not test_openal_listener"
                                  " and not test_parse_all_controller_mappings"
                                  " and not test_player_play"
                                  " and not test_player_play_multiple"
                                  " and not test_stream_write_needed"
                                  " and not test_window_caption"
                                  " and not test_window_caption_from_argv"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-lib-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (list "pyglet/font/fontconfig.py"
                                 "pyglet/font/freetype_lib.py"
                                 "pyglet/gl/lib_glx.py"
                                 "pyglet/input/linux/evdev.py"
                                 "pyglet/libs/egl/egl.py"
                                 "pyglet/libs/egl/lib.py"
                                 "pyglet/libs/x11/xf86vmode.py"
                                 "pyglet/libs/x11/xinerama.py"
                                 "pyglet/libs/x11/xlib.py"
                                 "pyglet/libs/x11/xsync.py"
                                 "pyglet/media/drivers/openal/lib_openal.py"
                                 "pyglet/media/drivers/pulse/lib_pulseaudio.py")
                (("'EGL'")
                 (format #f "'~a/~a'" #$(this-package-input "mesa")
                         "lib/libEGL.so"))
                (("\"fontconfig\"")
                 (format #f "'~a/~a'" #$(this-package-input "fontconfig-minimal")
                         "lib/libfontconfig.so"))
                (("\"freetype\"")
                 (format #f "'~a/~a'" #$(this-package-input "freetype")
                         "lib/libfreetype.so"))
                (("'GL'")
                 (format #f "'~a/~a'" #$(this-package-input "mesa")
                         "lib/libGL.so"))
                (("'GLU'")
                 (format #f "'~a/~a'" #$(this-package-input "glu")
                         "lib/libGLU.so"))
                (("'X11'")
                 (format #f "'~a/~a'" #$(this-package-input "libx11")
                         "lib/libX11.so"))
                (("'Xext'")
                 (format #f "'~a/~a'" #$(this-package-input "libxext")
                         "lib/libXext.so"))
                (("'Xinerama'")
                 (format #f "'~a/~a'" #$(this-package-input "libxinerama")
                         "lib/libXinerama.so"))
                (("'Xxf86vm'")
                 (format #f "'~a/~a'" #$(this-package-input "libxxf86vm")
                         "lib/libXxf86vm.so"))
                (("'c'")
                 (format #f "'~a/~a'" #$(this-package-input "glibc")
                         "lib/libc.so"))
                (("'gdk-x11-2.0'")
                 (format #f "'~a/~a'" #$(this-package-input "gtk+")
                         "lib/gdk-pixbuf-2.0.so"))
                (("'gdk_pixbuf-2.0'")
                 (format #f "'~a/~a'" #$(this-package-input "gdk-pixbuf")
                         "lib/libgdk_pixbuf-2.0.so"))
                (("'openal'")
                 (format #f "'~a/~a'" #$(this-package-input "openal")
                         "lib/libopenal.so"))
                (("'pulse'")
                 (format #f "'~a/~a'" #$(this-package-input "pulseaudio")
                         "lib/libpulse.so")))))
          (add-before 'check 'prepare-test-environment
            (lambda _
              ;; The test suite requires a running X server.
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1")
              (setenv "HOME" "/tmp")
              (setenv "PYGLET_HEADLESS" "True"))))))
    (native-inputs
     (list python-flit-core
           python-pytest
           unzip
           xorg-server-for-tests))
    (inputs
     (list ffmpeg
           fontconfig
           freeglut
           freetype
           gdk-pixbuf
           glibc
           glu
           gtk+
           libx11
           libxext
           libxinerama
           libxxf86vm
           mesa
           openal
           pulseaudio))
    (home-page "https://github.com/pyglet/pyglet")
    (synopsis "Windowing and multimedia library")
    (description
     "Pyglet is a Python library for developing games and other visually-rich
applications.  It supports windowing, user interface event handling,
Joysticks, OpenGL graphics, loading images and videos, playing sounds and
music." )
    (license license:bsd-3)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetical order.
;;;
