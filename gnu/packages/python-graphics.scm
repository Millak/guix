;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2023 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021 Adam Kandur <kefironpremise@gmail.com>
;;; Copyright © 2021 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2021, 2023 Daniel Meißner <daniel.meissner-i4k@ruhr-uni-bochum.de>
;;; Copyright © 2022 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2023 Adam Faiz <adam.faiz@disroot.org>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Sisiutl <sisiutl@egregore.fun>
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

(define-module (gnu packages python-graphics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages simulation)
  #:use-module (gnu packages swig)
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

(define-public python-asyncgui
  (package
    (name "python-asyncgui")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/asyncgui/asyncgui")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b074msb580wify2wag3swm3s21x23kckxpw974y6dibsmrfr5n3"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-poetry-core
           python-pytest))
    (home-page "https://github.com/asyncgui/asyncgui")
    (synopsis "Enables async/await without an event loop")
    (description
     "This package provides support for async/await applications without
requiring an event loop, useful for creative responsive GUIs.")
    (license license:expat)))

(define-public python-asynckivy
  (package
    (name "python-asynckivy")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/asyncgui/asynckivy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gjddv6d7bbjymvly2x5zaay1gyihls1c4bh7y1ppbvz15152lkj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              ;; 'kivy/__init__.py' wants to create $HOME/.kivy.
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-poetry-core
           python-pytest))
    (propagated-inputs
     (list python-kivy
           python-asyncgui))
    (home-page "https://github.com/asyncgui/asynckivy")
    (synopsis "Async library for Kivy")
    (description
     "This package provides async versions of Kivy functions to avoid the
callback-heavy mode of interaction typical in some Kivy applications.")
    (license license:expat)))

(define-public python-glcontext
  (let (;; Upstream is known for abusing mutable tag, hence pinpoint the
        ;; relevant commit.
        (revision "2")
        (commit "f2875abdb18b24e785c3958cc22845c81725d5cd"))
    (package
      (name "python-glcontext")
      (version (git-version "3.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/moderngl/glcontext")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15pngnc4agdvm8crq782vjlb5v1qrclln5xpqiyhz3jhmipsqb8q"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'fix-lib-paths
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((mesa (assoc-ref inputs "mesa"))
                      (libx11 (assoc-ref inputs "libx11")))
                  (substitute* '("glcontext/x11.cpp"
                                 "glcontext/egl.cpp")
                    (("\"libGL.so\"")
                     (string-append "\"" mesa "/lib/libGL.so\""))
                    (("\"libEGL.so\"")
                     (string-append "\"" mesa "/lib/libEGL.so\""))
                    (("\"libX11.so\"")
                     (string-append "\"" libx11 "/lib/libX11.so\"")))
                  (substitute* '("glcontext/__init__.py")
                    (("find_library\\('GL'\\)")
                     (string-append "'" mesa "/lib/libGL.so'"))
                    (("find_library\\('EGL'\\)")
                     (string-append "'" mesa "/lib/libEGL.so'"))
                    (("find_library\\(\"X11\"\\)")
                     (string-append "'" libx11 "/lib/libX11.so'"))))))
            (add-before 'check 'prepare-test-environment
              (lambda _
                (system "Xvfb :1 &")
                (setenv "DISPLAY" ":1"))))))
      (inputs
       (list libx11
             mesa))
      (native-inputs
       (list python-psutil
             python-pytest
             python-setuptools
             python-wheel
             xorg-server-for-tests))
      (home-page "https://github.com/moderngl/glcontext")
      (synopsis "Portable OpenGL Context for ModernGL")
      (description
       "Python-glcontext is a library providing an OpenGL implementation for
ModernGL on multiple platforms.")
      (license license:expat))))

(define-public python-glfw
  (package
    (name "python-glfw")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "glfw" version))
       (sha256
        (base32 "1w36jvn6fx8p7irhwj6bbl67m2id3s0227b3w7bgw9hbicr0vsch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; no tests provided
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-lib-paths
            (lambda _
              (substitute* "glfw/library.py"
                (("_get_library_search_paths\\(\\), ")
                 (format #f "[ '~a/lib' ],"
                         #$(this-package-input "glfw")))))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (inputs
     (list glfw))
    (home-page "https://github.com/FlorianRhiem/pyGLFW")
    (synopsis "Python bindings for GLFW")
    (description
     "This package provides Python bindings for @url{http://www.glfw.org/,
GLFW} OpenGL application development library.")
    (license license:expat)))

(define-public python-kivy
  (package
    (name "python-kivy")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Kivy" version))
       (sha256
        (base32 "1ngrnkrp6xgfl4x32i2nv3bml13l8qwa87cwrymv9k826ng98cq8"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f              ; Tests require many optional packages
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-generated-file-shebangs 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "KIVY_SDL2_PATH"
                     (search-input-directory inputs "/include/SDL2"))))
         (add-before 'build 'set-home
           (lambda _
             ;; 'kivy/__init__.py' wants to create $HOME/.kivy.
             (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list pkg-config
           python-cython
           ;; Not packaged yet, for tests.
           ;; python-kivy-deps-glew
           ;; python-kivy-deps-glew-dev
           ;; python-kivy-deps-gstreamer
           ;; python-kivy-deps-gstreamer-dev
           ;; python-kivy-deps-sdl2
           ;; python-kivy-deps-sdl2-dev
           python-packaging
           python-setuptools
           python-wheel))
    (inputs
     (list gstreamer
           mesa
           (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))))
    (propagated-inputs
     (list python-docutils
           python-filetype
           python-kivy-garden
           python-pygments
           python-requests))
    (home-page "https://kivy.org")
    (synopsis "Multitouch application framework")
    (description
     "Kivy is a software library for rapid development of hardware-accelerated
multitouch applications.")
    (license license:expat)))

(define-public python-kivymd
  (package
    (name "python-kivymd")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/kivymd/KivyMD")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nprldcm54qybbwf7zlb32fkmz375j8i3k3g41d6ykc6vasq3w5j"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; <https://github.com/pyinstaller/pyinstaller> is not packaged yet in
      ;; Guix.
      #~(list "--ignore=kivymd/tests/pyinstaller"
              "-k" (string-join
                    ;; FIXME: Only some tests passed, the most fail with err:
                    ;; ValueError: KivyMD: App object must be initialized
                    ;; before loading root widget.
                    (list "not test_backdrop_raw_app"
                          "test_bottom_navigation_m3_style_raw_app"
                          "test_card_m3_style_raw_app"
                          "test_chip_raw_app"
                          "test_imagelist_raw_app"
                          "test_list_raw_app"
                          "test_navigationdrawer_raw_app"
                          "test_tab_raw_app"
                          "test_textfield_raw_app")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-modules
            (lambda _
              (substitute* "setup.py"
                ;; If excluded, sanity check fails with error:
                ;; ModuleNotFoundError: No module named 'kivymd.tools.release'
                (("\"kivymd.tools.release\"") ""))
              ;; Check phase fails struggling to find tests module.
              (with-output-to-file "kivymd/tests/__init__.py"
                (lambda _ (display "")))))
          (add-before 'check 'set-home
            (lambda _
              ;; FileNotFoundError: [Errno 2] No such file or directory:
              ;; '/homeless-shelter/.kivy'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-docutils
           python-pytest
           python-pytest-asyncio
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-kivy
           python-pillow
           python-pygments
           python-kivy-garden))
    (home-page "https://github.com/kivymd/KivyMD")
    (synopsis "Material Design compliant widgets for use with Kivy")
    (description
     "This package provides Kivy widgets that approximate Google's Material
Design spec without sacrificing ease of use or application performance.")
    (license license:expat)))

(define-public python-pivy
  (package
    (name "python-pivy")
    (version "0.6.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/coin3d/pivy")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00l4r06dwmgn8h29nrl3g3yv33cfyizyylk28x1j95qyj36sggfb"))))
    (build-system python-build-system)
    (arguments
     (list
      ;; The test suite fails due to an import cycle between 'pivy' and '_coin'
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-cmake-include-dirs
            (lambda _
              ;; Patch buildsystem to respect Coin3D include directory
              (substitute* "CMakeLists.txt"
                (("\\$\\{SoQt_INCLUDE_DIRS}")
                 "${Coin_INCLUDE_DIR};${SoQt_INCLUDE_DIRS}")))))))
    (native-inputs
      (list cmake swig))
    (inputs
      (list python-wrapper
            qtbase-5
            libxi
            libice
            glew
            coin3d))
    (home-page "https://github.com/coin3d/pivy")
    (synopsis "Python bindings to Coin3D")
    (description
      "Pivy provides python bindings for Coin, a 3D graphics library with an
Application Programming Interface based on the Open Inventor 2.1 API.")
    (license license:isc)))

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

(define-public python-pyopengl
  (package
    (name "python-pyopengl")
    (version "3.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyOpenGL" version))
       (sha256
        (base32
         "09syrsfrcknr1k2wmj05gfd5d0dyjfxzbipzbd0agv9775vwi9lf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; XXX: Check why these test fail.
                    (list "not test_get_read_fb_binding"
                          "test_get_version"
                          "test_glCallLists_twice2"
                          "test_glmultidraw"
                          "test_lookupint"
                          "test_numpyConversion"
                          "test_pointers")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-paths
            (lambda _
              (substitute* '("OpenGL/platform/ctypesloader.py")
                (("filenames_to_try = \\[\\]") "filenames_to_try = [name]"))
              (substitute* '("OpenGL/platform/glx.py"
                             "OpenGL/platform/egl.py"
                             "OpenGL/platform/osmesa.py"
                             "OpenGL/platform/darwin.py"
                             "tests/check_glut_load.py")
                (("'GLU'")
                 (format #f "'~a/~a'" #$(this-package-input "glu")
                         "lib/libGLU.so"))
                (("'glut',")
                 (format #f "'~a/~a'," #$(this-package-input "freeglut")
                         "lib/libglut.so"))
                (("'(GL|EGL|GLESv1_CM|GLESv2|OSMesa)'" all gl-library)
                 (format #f "'~a/~a'" #$(this-package-input "mesa")
                         (string-append "lib/lib" gl-library ".so"))))
              ;; Not providing libgle. It seems to be very old.
              )))))
    (native-inputs
     (list python-cython
           python-numpy
           python-pygame
           python-pytest
           python-setuptools
           python-wheel))
    (inputs
     (list freeglut
           glu
           mesa))
    (home-page "https://pyopengl.sourceforge.net")
    (synopsis "Standard OpenGL bindings for Python")
    (description
     "PyOpenGL is the most common cross platform Python binding to OpenGL and
related APIs.  The binding is created using the standard @code{ctypes}
library.")
    (license license:bsd-3)))

(define-public python-pyopengl-accelerate
  (package
    (inherit python-pyopengl)
    (name "python-pyopengl-accelerate")
    (version "3.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyopengl_accelerate" version))
       (sha256
        (base32
         "1ww6kbyj1hshxfi3gskkygv1w2f7klzj9jcyfpzihn4pfry7r5c5"))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'fix-paths))))
    (synopsis "Acceleration code for PyOpenGL")
    (description
     "This is the Cython-coded accelerator module for PyOpenGL.")))

(define-public python-pysdl2
  (package
    (name "python-pysdl2")
    (version "0.9.17")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PySDL2" version))
       (sha256
        (base32 "19id1qswgcj4v4j5kn49shq1xxx3slhjpm0102w87mczsdbi1rck"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;; Requires /dev/dri, OpenGL module, etc.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* "sdl2/dll.py"
                ;; Disable pysdl2-dll. It can't be packaged on GNU Guix
                ;; as it duplicates an existing package (sdl2).
                (("prepath = os\\.getenv\\('PYSDL2_DLL_PATH'\\)")
                 "prepath = \"system\"")
                (("^import sdl2dll$") "")
                (("postpath = os\\.getenv\\('PYSDL2_DLL_PATH'\\)")
                 "postpath = \"system\"")
                (("DLL\\(.*, os\\.getenv\\(\"PYSDL2_DLL_PATH\"\\)\\)")
                 (format #f "DLL('SDL2', ['SDL2', 'SDL2-2.0', 'SDL2-2.0.0'], '~a/lib')"
                         #$(this-package-input "sdl2"))))
              (substitute* "sdl2/sdlimage.py"
                (("os\\.getenv\\(\"PYSDL2_DLL_PATH\"\\)")
                 (format #f "'~a/~a'" #$(this-package-input "sdl2-image")
                         "lib/libSDL2_image.so")))
              (substitute* "sdl2/sdlgfx.py"
                (("os\\.getenv\\(\"PYSDL2_DLL_PATH\"\\)")
                 (format #f "'~a/~a'" #$(this-package-input "sdl2-gfx")
                         "lib/libSDL2_gfx.so")))
              (substitute* "sdl2/sdlmixer.py"
                (("os\\.getenv\\(\"PYSDL2_DLL_PATH\"\\)")
                 (format #f "'~a/~a'" #$(this-package-input "sdl2-mixer")
                         "lib/libSDL2_mixer.so")))
              (substitute* "sdl2/sdlttf.py"
                (("os\\.getenv\\(\"PYSDL2_DLL_PATH\"\\)")
                 (format #f "'~a/~a'" #$(this-package-input "sdl2-ttf")
                         "lib/libSDL2_ttf.so"))))))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (inputs
     (list sdl2 sdl2-image sdl2-gfx sdl2-mixer sdl2-ttf))
    (home-page "https://github.com/py-sdl/py-sdl2")
    (synopsis "Python bindings around the SDL2 game development library")
    (description
     "PySDL2 is a pure Python wrapper around the
@code{SDL2},@code{SDL2_mixer}, @code{SDL2_image}, @code{SDL2_ttf}, and
@code{SDL2_gfx} libraries.  Instead of relying on C code, it uses the built-in
ctypes module to interface with SDL2, and provides simple Python classes and
wrappers for common SDL2 functionality.")
    (license license:cc0)))

(define-public python-vispy
  (package
    (name "python-vispy")
    (version "0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vispy" version))
       (sha256
        (base32 "07fkk4bdffn0iq5cprk7ydj978rqc4lvzfcs2vkzgfh8m53vifzg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Test requiring network access to download test data from
      ;; <https://raw.githubusercontent.com/vispy/demo-data/main/CONTRIBUTING.txt>.
      #~(list "-k" (string-append "not test_read_write_image"
                                  " and not test_wavefront"
                                  " and not test_config"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-library-path
            (lambda _
              (substitute* (list "vispy/ext/egl.py"
                                 "vispy/ext/fontconfig.py"
                                 "vispy/gloo/gl/es2.py"
                                 "vispy/gloo/gl/gl2.py")
                (("ctypes\\.util\\.find_library\\('EGL'\\)")
                 (format #f "'~a/~a'" #$(this-package-input "mesa")
                         "lib/libEGL.so"))
                (("ctypes\\.util\\.find_library\\('GL'\\)")
                 (format #f "'~a/~a'" #$(this-package-input "mesa")
                         "lib/libGL.so"))
                (("ctypes\\.util\\.find_library\\('GLESv2'\\)")
                 (format #f "'~a/~a'" #$(this-package-input "mesa")
                         "lib/libGLESv2.so"))
                (("util\\.find_library\\('fontconfig'\\)")
                 (format #f "'~a/~a'" #$(this-package-input "fontconfig-minimal")
                         "lib/libfontconfig.so")))))
          (add-before 'check 'prepare-test-environment
            (lambda _
              ;; XXX: Check how to set DPI to run headless tests, fails when
              ;; DISPLAY is set.
              ;; E RuntimeError: could not determine DPI
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-cython-3
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (inputs
     (list fontconfig
           mesa))
    (propagated-inputs
     (list python-freetype-py
           python-hsluv
           python-kiwisolver
           python-meshio
           python-numpy
           python-packaging
           python-pillow))
    (home-page "http://vispy.org")
    (synopsis "Interactive scientific visualization in Python")
    (description
     "VisPy is a high-performance interactive 2D/3D data visualization library
leveraging the computational power of modern Graphics Processing Units (GPUs)
through the OpenGL library to display very large datasets.")
    (license license:bsd-3)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetical order.
;;;
