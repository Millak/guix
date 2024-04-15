;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017-2019, 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021, 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Kaelyn Takata <kaelyn.alexi@protonmail.com>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (gnu packages gl)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (gnu packages cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define-public glu
  (package
    (name "glu")
    (version "9.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/mesa/glu")
                    (commit (string-append "glu-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1khxfidyglpx4yd8f3xvrj05ah823cz1ygcszhcaa4w7h9kd1lbr"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           autoconf
           automake
           libtool))
    (propagated-inputs
     (list mesa)) ; according to glu.pc
    (home-page "http://www.opengl.org/archives/resources/faq/technical/glu.htm")
    (synopsis "Mesa OpenGL Utility library")
    (description
     "GLU, or OpenGL Utility Library provides some higher-level functionality
not provided by just OpenGL itself.  Some of GLU's Features
include: Scaling of 2D images and creation of mipmap pyramids,
Transformation of object coordinates into device coordinates and
vice versa, Support for NURBS surfaces, Support for tessellation
of concave or bow tie polygonal primitives, Specialty transformation
matrices for creating perspective and orthographic projections,
positioning a camera, and selection/picking, Rendering of disk,
cylinder, and sphere primitives, Interpreting OpenGL error values
as ASCII text.")
    (license (license:x11-style "http://directory.fsf.org/wiki/License:SGIFreeBv2"))))

(define-public freeglut
  (package
    (name "freeglut")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/FreeGLUTProject/freeglut/releases"
                    "/download/v" version "/freeglut-" version ".tar.gz"))
              (sha256
               (base32
                "1v7ayg3a03mv8b6lsr1qm21lbr8xg8dh3gdfxnbhl64vbn8wn2rw"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ;no test target
       #:configure-flags '("-DFREEGLUT_BUILD_STATIC_LIBS=OFF")))
    (inputs (list libx11 libxi libxrandr libxxf86vm))
    (propagated-inputs
     ;; Headers from Mesa and GLU are needed.
     (list glu mesa))
    (home-page "https://freeglut.sourceforge.net/")
    (synopsis "Alternative to the OpenGL Utility Toolkit (GLUT)")
    (description
     "Freeglut is a completely Free/OpenSourced alternative to
the OpenGL Utility Toolkit (GLUT) library.  GLUT was originally
written by Mark Kilgard to support the sample programs in the
second edition OpenGL @code{RedBook}.  Since then, GLUT has been used
in a wide variety of practical applications because it is simple,
widely available and highly portable.

GLUT (and hence freeglut) allows the user to create and manage windows
containing OpenGL contexts on a wide range of platforms and also read
the mouse, keyboard and joystick functions.  Freeglut is released under
the X-Consortium license.")
    (license license:x11)))

;; Needed for "kiki".
(define-public freeglut-2.8
  (package (inherit freeglut)
    (name "freeglut")
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/freeglut/freeglut/"
                    version "/freeglut-" version ".tar.gz"))
              (sha256
               (base32
                "16lrxxxd9ps9l69y3zsw6iy0drwjsp6m26d1937xj71alqk6dr6x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))))

(define-public ftgl
  (package
    (name "ftgl")
    (version "2.4.0")
    (home-page "https://github.com/frankheckenbach/ftgl")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zjs1h9w30gajq9lndzvjsa26rsmr1081lb1fbpbj10yhcdcsc79"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    ;; The pkg-config file lists "freetype2" as Requires.private.
    (propagated-inputs (list freetype))
    (inputs (list libx11 mesa glu))
    (native-inputs
     (list pkg-config autoconf automake libtool))
    (synopsis "Font rendering library for OpenGL applications")
    (description
     "FTGL is a font rendering library for OpenGL applications.  Supported
rendering modes are: Bitmaps, Anti-aliased pixmaps, Texture maps, Outlines,
Polygon meshes, and Extruded polygon meshes.")
    (license license:x11)))

(define-public glad-0.1
  (package
    (name "glad")
    (version "0.1.36")
    (source
     (origin
       ;; We fetch the sources from the repository since the PyPI archive
       ;; doesn't contain the CMakeLists.txt file which is useful for
       ;; integration with other software, such as the openboardview package.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Dav1dde/glad")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0m55ya1zrmg6n2cljkajy80ilmi5sblln8742fm0k1sw9k7hzn8n"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-cmakelists.txt
                 (lambda _
                   (let ((share (string-append #$output "/share/"
                                               #$(package-name this-package))))
                     (install-file "CMakeLists.txt" share)))))))
    (home-page "https://github.com/Dav1dde/glad")
    (synopsis "Multi-language GL/GLES/EGL/GLX/WGL loader generator")
    (description "Glad uses the official Khronos XML specifications to
generate a GL/GLES/EGL/GLX/WGL loader tailored for specific requirements.")
    (license license:expat)))

(define-public glad
  (package
    (inherit glad-0.1)
    (name "glad")
    (version "2.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Dav1dde/glad")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pam6imhcmcyqrqi6wzzxprb23y8x6zdbvsjavnz26k72i9dbbja"))))
    (build-system python-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments glad-0.1)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (replace 'install-cmakelists.txt
              (lambda _
                (let ((share (string-append #$output "/share/"
                                            #$(package-name this-package))))
                  (install-file "cmake/CMakeLists.txt" share))))))))
    (propagated-inputs (list python-jinja2))))

(define-public s2tc
  (package
    (name "s2tc")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/divVerent/s2tc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fg323fk7wlv2xh6lw66wswgcv6qi8aaadk7c28h2f2lj1s7njnf"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list mesa-headers))
    (home-page "https://github.com/divVerent/s2tc")
    (synopsis "S3 Texture Compression implementation")
    (description
     "S2TC is a patent-free implementation of S3 Texture Compression (S3TC,
also known as DXTn or DXTC) for Mesa.")
    (license license:expat)))

;;; Mesa needs LibVA headers to build its Gallium-based VA API implementation;
;;; LibVA itself depends on Mesa.  We use the following to solve the circular
;;; dependency.
(define libva-without-mesa
  ;; Delay to work around circular import problem.
  (delay
    (package
      (inherit libva)
      (name "libva-without-mesa")
      (inputs (fold alist-delete (package-inputs libva)
                    '("mesa" "wayland")))
      (arguments
       (strip-keyword-arguments
        '(#:make-flags)
        (substitute-keyword-arguments (package-arguments libva)
          ((#:configure-flags flags)
           '(list "--disable-glx"))))))))

(define-public mesa
  (package
    (name "mesa")
    (version "24.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://archive.mesa3d.org/"
                                 "mesa-" version ".tar.xz")
                  (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
                                 "mesa-" version ".tar.xz")))
       (sha256
        (base32
         "1w25lwdrb0ffrx2fjk9izbvpcgf9ypfc7v32zybwvjwql0qbvzlh"))))
    (build-system meson-build-system)
    (propagated-inputs
     ;; The following are in the Requires.private field of gl.pc.
     (list libdrm
           libvdpau
           libx11
           libxdamage
           libxfixes
           libxshmfence
           libxxf86vm
           xorgproto))
    (inputs
     (list elfutils                  ;libelf required for r600 when using llvm
           expat
           (force libva-without-mesa)
           libxml2
           libxrandr
           libxvmc
           llvm-for-mesa
           vulkan-loader
           wayland
           wayland-protocols
           `(,zstd "lib")))
    (native-inputs
     (cons* bison
            flex
            gettext-minimal
            glslang
            pkg-config
            python-libxml2              ;for OpenGL ES 1.1 and 2.0 support
            python-mako
            python-wrapper
            (@ (gnu packages base) which)
            (if (%current-target-system)
              (list cmake-minimal-cross
                    pkg-config-for-build
                    wayland
                    wayland-protocols)
              '())))
    (outputs '("out" "bin"))
    (arguments
     (list
      #:configure-flags
      #~(list
         #$@(cond
             ((or (target-aarch64?) (target-arm32?))
              '("-Dgallium-drivers=etnaviv,freedreno,kmsro,lima,nouveau,\
panfrost,r300,r600,svga,swrast,tegra,v3d,vc4,virgl,zink"))
             ((or (target-ppc64le?) (target-ppc32?) (target-riscv64?))
              '("-Dgallium-drivers=nouveau,r300,r600,radeonsi,svga,swrast,virgl,zink"))
             (else
              '("-Dgallium-drivers=crocus,iris,nouveau,r300,r600,radeonsi,\
svga,swrast,virgl,zink")))
         ;; Enable various optional features.  TODO: opencl requires libclc,
         ;; omx requires libomxil-bellagio
         "-Dplatforms=x11,wayland"
         "-Dglx=dri"               ;Thread Local Storage, improves performance
         ;; "-Dopencl=true"
         ;; "-Domx=true"
         "-Dosmesa=true"
         "-Dgallium-xa=enabled"

         ;; features required by wayland
         "-Dgles2=enabled"
         "-Dgbm=enabled"
         "-Dshared-glapi=enabled"

         ;; Explicitly enable Vulkan on some architectures.
         #$@(cond
             ((or (target-x86-32?) (target-x86-64?))
              '("-Dvulkan-drivers=intel,intel_hasvk,amd,swrast"))
             ((or (target-ppc64le?) (target-ppc32?))
              '("-Dvulkan-drivers=amd,swrast"))
             ((target-aarch64?)
              '("-Dvulkan-drivers=freedreno,amd,broadcom,swrast"))
             ((target-riscv64?)
              '("-Dvulkan-drivers=amd,swrast"))
             (else
              '("-Dvulkan-drivers=auto")))

         ;; Enable the Vulkan overlay layer on all architectures.
         "-Dvulkan-layers=device-select,overlay"

         ;; Enable all the codecs that were built by default as part of the
         ;; 21.3.x releases to avoid functionality regressions.
         "-Dvideo-codecs=all"

         ;; Enable ZSTD compression for shader cache.
         "-Dzstd=enabled"

         ;; Also enable the tests.
         "-Dbuild-tests=true"

         "-Dllvm=enabled")              ; default is x86/x86_64 only

       ;; XXX: 'debugoptimized' causes LTO link failures on some drivers.  The
       ;; documentation recommends using 'release' for performance anyway.
       #:build-type "release"

       #:modules '((ice-9 match)
                   (srfi srfi-1)
                   (guix build utils)
                   (guix build meson-build-system))
       #:phases
       #~(modify-phases %standard-phases
         #$@(if (%current-target-system)
              #~((add-after 'unpack 'fix-cross-compiling
                   (lambda* (#:key native-inputs #:allow-other-keys)
                     ;; When cross compiling, we use cmake to find llvm, not
                     ;; llvm-config, because llvm-config cannot be executed
                     ;; see https://github.com/llvm/llvm-project/issues/58984
                     (substitute* "meson.build"
                       (("method : host_machine\\.system.*")
                        "method : 'cmake',\n"))
                     (setenv "CMAKE"
                             (search-input-file
                              native-inputs "/bin/cmake")))))
              #~())
         (add-after 'unpack 'disable-failing-test
           (lambda _
             ;; Disable the intel vulkan (anv_state_pool) tests, as they may
             ;; fail in a nondeterministic fashion (see:
             ;; https://gitlab.freedesktop.org/mesa/mesa/-/issues/5446).
             (substitute* "src/intel/vulkan/meson.build"
               (("if with_tests")
                "if false"))
             #$@(match (%current-system)
                 ("riscv64-linux"
                  ;; According to the test logs the llvm JIT is not designed
                  ;; for this architecture and the llvmpipe tests all segfault.
                  ;; The same is true for mesa:gallium / osmesa-render.
                  `((substitute* '("src/gallium/drivers/llvmpipe/meson.build"
                                   "src/gallium/targets/osmesa/meson.build")
                      (("if with_tests") "if false"))))
                 ("powerpc64le-linux"
                  ;; Disable some of the llvmpipe tests.
                  `((substitute* "src/gallium/drivers/llvmpipe/lp_test_arit.c"
                      (("0\\.5, ") ""))))
                 ("powerpc-linux"
                  ;; There are some tests which fail specifically on powerpc.
                  `((substitute* '(;; LLVM ERROR: Relocation type not implemented yet!
                                   "src/gallium/drivers/llvmpipe/meson.build"
                                   "src/gallium/targets/osmesa/meson.build")
                      (("if with_tests") "if not with_tests"))
                    ;; This is probably a big-endian test failure.
                    (substitute* "src/amd/common/meson.build"
                      (("and not with_platform_windows")
                       "and with_platform_windows"))))
                 ("i686-linux"
                  ;; This test is known to fail on i686 (see:
                  ;; https://gitlab.freedesktop.org/mesa/mesa/-/issues/4091).
                  `((substitute* "src/util/meson.build"
                      ((".*'tests/u_debug_stack_test.cpp',.*") ""))))
                 ("armhf-linux"
                  ;; Disable some of the llvmpipe tests.
                  `((substitute* "src/gallium/drivers/llvmpipe/meson.build"
                      (("'lp_test_arit', ") ""))))
                 (_
                  '((display "No tests to disable on this architecture.\n"))))))
         (add-before 'configure 'fix-dlopen-libnames
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((out #$output))
               ;; Remain agnostic to .so.X.Y.Z versions while doing
               ;; the substitutions so we're future-safe.
               (substitute* "src/glx/meson.build"
                 (("-DGL_LIB_NAME=\"lib@0@\\.so\\.@1@\"")
                  (string-append "-DGL_LIB_NAME=\"" out
                                 "/lib/lib@0@.so.@1@\"")))
               (substitute* "src/gbm/backends/dri/gbm_dri.c"
                 (("\"libglapi\\.so")
                  (string-append "\"" out "/lib/libglapi.so")))
               (substitute* "src/gbm/main/backend.c"
                 ;; No need to patch the gbm_gallium_drm.so reference;
                 ;; it's never installed since Mesa removed its
                 ;; egl_gallium support.
                 (("\"gbm_dri\\.so")
                  (string-append "\"" out "/lib/dri/gbm_dri.so")))
               (substitute* "src/gallium/drivers/zink/zink_screen.c"
                 (("util_dl_open\\(VK_LIBNAME\\)")
                  (format #f "util_dl_open(\"~a\")"
                          (search-input-file inputs
                                             "lib/libvulkan.so.1")))))))
         (add-after 'install 'split-outputs
           (lambda _
             (let ((out #$output)
                   (bin #$output:bin))
               ;; Not all architectures have the Vulkan overlay control script.
               (mkdir-p (string-append out "/bin"))
               (call-with-output-file (string-append out "/bin/.empty")
                 (const #t))
               (copy-recursively (string-append out "/bin")
                                 (string-append bin "/bin"))
               (delete-file-recursively (string-append out "/bin")))))
         (add-after 'install 'symlinks-instead-of-hard-links
           (lambda _
             ;; All the drivers and gallium targets create hard links upon
             ;; installation (search for "hardlink each megadriver instance"
             ;; in the makefiles).  This is no good for us since we'd produce
             ;; nars that contain several copies of these files.  Thus, turn
             ;; them into symlinks, which saves ~124 MiB.
             (let* ((out    #$output)
                    (lib    (string-append out "/lib"))
                    (files  (find-files lib
                                        (lambda (file stat)
                                          (and (string-contains file ".so")
                                               (eq? 'regular
                                                    (stat:type stat))))))
                    (inodes (map (compose stat:ino stat) files)))
               (for-each (lambda (inode)
                           (match (filter-map (match-lambda
                                                ((file ino)
                                                 (and (= ino inode) file)))
                                              (zip files inodes))
                             ((_)
                              #f)
                             ((reference others ..1)
                              (format #t "creating ~a symlinks to '~a'~%"
                                      (length others) reference)
                              (for-each delete-file others)
                              (for-each (lambda (file)
                                          (if (string=? (dirname file)
                                                        (dirname reference))
                                              (symlink (basename reference)
                                                       file)
                                              (symlink reference file)))
                                        others))))
                         (delete-duplicates inodes)))))
         (add-after 'install 'set-layer-path-in-manifests
           (lambda _
             (let* ((out #$output)
                    (implicit-path (string-append
                                    out
                                    "/share/vulkan/implicit_layer.d/"))
                    (explicit-path (string-append
                                    out
                                    "/share/vulkan/explicit_layer.d/"))
                    (fix-layer-path
                     (lambda (layer-name)
                       (let* ((explicit (string-append explicit-path layer-name ".json"))
                              (implicit (string-append implicit-path layer-name ".json"))
                              (manifest (if (file-exists? explicit)
                                            explicit
                                            implicit)))
                         (substitute* manifest
                           (((string-append "\"lib" layer-name ".so\""))
                             (string-append "\"" out "/lib/lib" layer-name ".so\"")))))))
               (for-each fix-layer-path '("VkLayer_MESA_device_select"
                                          "VkLayer_MESA_overlay"))))))))
    (native-search-paths
     (list (search-path-specification
            ;; Ensure the Mesa VDPAU drivers can be found.
            (variable "VDPAU_DRIVER_PATH")
            (separator #f)
            (files '("lib/vdpau")))))
    (home-page "https://mesa3d.org/")
    (synopsis "OpenGL and Vulkan implementations")
    (description "Mesa is a free implementation of the OpenGL and Vulkan
specifications - systems for rendering interactive 3D graphics.  A variety of
device drivers allows Mesa to be used in many different environments ranging
from software emulation to complete hardware acceleration for modern GPUs.")
    (license license:x11)))

(define-public mesa-opencl
  (package/inherit mesa
    (name "mesa-opencl")
    (source (origin
              (inherit (package-source mesa))))
    (arguments
     (substitute-keyword-arguments (package-arguments mesa)
       ((#:configure-flags flags)
        #~(cons "-Dgallium-opencl=standalone" #$flags))))
    (inputs
     (modify-inputs (package-inputs mesa)
       (prepend libclc)))
    (native-inputs
     (modify-inputs (package-native-inputs mesa)
       (prepend clang-15)))))

(define-public mesa-opencl-icd
  (package/inherit mesa-opencl
    (name "mesa-opencl-icd")
    (arguments
      (substitute-keyword-arguments (package-arguments mesa)
        ((#:configure-flags flags)
         #~(cons "-Dgallium-opencl=icd"
                (delete "-Dgallium-opencl=standalone" #$flags)))
        ((#:phases phases)
         #~(modify-phases #$phases
            (add-after 'install 'mesa-icd-absolute-path
              (lambda _
                ;; Use absolute path for OpenCL platform library.
                ;; Otherwise we would have to set LD_LIBRARY_PATH=LIBRARY_PATH
                ;; for ICD in our applications to find OpenCL platform.
                (use-modules (guix build utils)
                             (ice-9 textual-ports))
                (let* ((out #$output)
                       (mesa-icd (string-append out "/etc/OpenCL/vendors/mesa.icd"))
                       (old-path (call-with-input-file mesa-icd get-string-all))
                       (new-path (string-append out "/lib/" (string-trim-both old-path))))
                  (if (file-exists? new-path)
                    (call-with-output-file mesa-icd
                      (lambda (port) (format port "~a\n" new-path)))))))))))))

(define-public mesa-headers
  (package/inherit mesa
    (name "mesa-headers")
    (propagated-inputs '())
    (inputs '())
    (native-inputs '())
    (outputs '("out"))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "include" (string-append
                                          (assoc-ref outputs "out")
                                          "/include")))))))))

;;; The mesa-demos distribution contains non-free files, many files with no
;;; clear license information, and many demos that aren't useful for most
;;; people, so we just use this for the mesa-utils package below, and possibly
;;; other packages in the future.  This is modeled after Debian's solution.
(define (mesa-demos-source version)
  (origin
    (method url-fetch)
    (uri (string-append "ftp://ftp.freedesktop.org/pub/mesa/demos"
                        "/mesa-demos-" version ".tar.bz2"))
    (sha256 (base32 "0zgzbz55a14hz83gbmm0n9gpjnf5zadzi2kjjvkn6khql2a9rs81"))))

(define-public mesa-utils
  (package
    (name "mesa-utils")
    (version "8.4.0")
    (source (mesa-demos-source version))
    (build-system gnu-build-system)
    (inputs
     (list mesa freeglut glew))
    (native-inputs
     (list pkg-config))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out #$output))
                (mkdir-p (string-append out "/bin"))
                (for-each (lambda (file)
                            (copy-file file
                                       (string-append out "/bin/"
                                                      (basename file))))
                          '("src/xdemos/glxdemo" "src/xdemos/glxgears"
                            "src/egl/opengl/eglinfo"
                            "src/xdemos/glxinfo" "src/xdemos/glxheads"))))))))
    (home-page "https://mesa3d.org/")
    (synopsis "Utility tools for Mesa")
    (description
     "The mesa-utils package contains several utility tools for Mesa: eglinfo,
glxdemo, glxgears, glxheads, and glxinfo.")
    ;; glxdemo is public domain; others expat.
    (license (list license:expat license:public-domain))))

(define-public glew
  (package
    (name "glew")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/glew/glew/" version
                                  "/glew-" version ".tgz"))
              (sha256
               (base32
                "1qak8f7g1iswgswrgkzc7idk7jmqgwrs58fhg2ai007v7j4q5z6l"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "config/Makefile.linux"
                    (("= cc") "= gcc")
                    (("/lib64") "/lib"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list #$@(if (%current-target-system)
                                       #~((string-append "CC=" #$(cc-for-target))
                                          (string-append "LD=" #$(cc-for-target))
                                          (string-append "STRIP=" #$(strip-for-target)))
                                       #~())
                                (string-append "GLEW_PREFIX=" #$output)
                                (string-append "GLEW_DEST=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'install 'delete-static
                 (lambda _
                   (delete-file (string-append #$output "/lib/libGLEW.a")))))
           #:tests? #f))                ;no 'check' target
    (inputs
     (list libxi libxmu libx11 mesa))

    ;; <GL/glew.h> includes <GL/glu.h>.
    (propagated-inputs (list glu))

    (home-page "http://glew.sourceforge.net/")
    (synopsis "OpenGL extension loading library for C and C++")
    (description
     "The OpenGL Extension Wrangler Library (GLEW) is a C/C++ extension
loading library.  GLEW provides efficient run-time mechanisms for determining
which OpenGL extensions are supported on the target platform.  OpenGL core and
extension functionality is exposed in a single header file.")
    (license license:bsd-3)))

(define-public guile-opengl
  (package
    (name "guile-opengl")
    (version "0.2.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/guile-opengl/guile-opengl-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0rbc2wf9x63ilj3n85h8wyllzc2b22abmhs2p2ghjgc253n8gw5q"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list guile-2.2 mesa glu freeglut))
    (arguments
     '(#:phases (modify-phases %standard-phases
                 (add-before 'build 'patch-dynamic-link
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (substitute* "gl/runtime.scm"
                       (("\\(dynamic-link\\)")
                        (string-append "(dynamic-link \""
                                       (assoc-ref inputs "mesa")
                                       "/lib/libGL.so" "\")")))
                     (define (dynamic-link-substitute file lib input)
                       (substitute* file
                         (("dynamic-link \"lib([a-zA-Z]+)\"" _ lib)
                          (string-append "dynamic-link \""
                                         (assoc-ref inputs input)
                                         "/lib/lib" lib "\""))))
                     ;; Replace dynamic-link calls for libGL, libGLU, and
                     ;; libglut with absolute paths to the store.
                     (dynamic-link-substitute "glx/runtime.scm" "GL" "mesa")
                     (dynamic-link-substitute "glu/runtime.scm" "GLU" "glu")
                     (dynamic-link-substitute "glut/runtime.scm" "glut"
                                              "freeglut"))))))
    (home-page "https://gnu.org/s/guile-opengl")
    (synopsis "Guile binding for the OpenGL graphics API")
    (description
     "Guile-OpenGL is a library for Guile that provides bindings to the
OpenGL graphics API.")
    (license license:lgpl3+)))

(define-public guile3.0-opengl
  (package
    (inherit guile-opengl)
    (name "guile3.0-opengl")
    (inputs
     (list guile-3.0 mesa glu freeglut))))

(define-public libepoxy
  (package
    (name "libepoxy")
    (version "1.5.10")
    (home-page "https://github.com/anholt/libepoxy")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jw02bzdwynyrwsn5rhcacv92h9xx928j3xp436f8gdnwlyb5641"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((mesa-lib
                     (lambda (file)
                       (search-input-file inputs (string-append "lib/" file)))))
                (substitute* (find-files "." "\\.[ch]$")
                  (("libGL.so.1") (mesa-lib "libGL.so.1"))
                  (("libEGL.so.1") (mesa-lib "libEGL.so.1"))
                  (("libGLESv1_CM.so.1") (mesa-lib "libGLESv1_CM.so.1"))
                  (("libGLESv2.so.2") (mesa-lib "libGLESv2.so.2")))))))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config python))
    (propagated-inputs
     ;; epoxy.pc: 'Requires.private: gl egl'
     (list mesa))
    (synopsis "Library for handling OpenGL function pointer management")
    (description
     "A library for handling OpenGL function pointer management.")
    (license license:x11)))

(define-public libglvnd
  (package
    (name "libglvnd")
    (version "1.7.0")
    (home-page "https://gitlab.freedesktop.org/glvnd/libglvnd")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07v3bmwzmg0d4g2zp835v1g7j22j8vz7hjfmqrdqjgxjj6v4jkyr"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Dx11=enabled")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-glx-tests
                    (lambda _
                      ;; This package is meant to be used alongside Mesa.
                      ;; To avoid a circular dependency, disable tests that
                      ;; require a running Xorg server.
                      (substitute* "tests/meson.build"
                        (("if with_glx")
                         "if false")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libx11 libxext xorgproto))
    (synopsis "Vendor-neutral OpenGL dispatch library")
    (description
     "libglvnd is a vendor-neutral dispatch layer for arbitrating OpenGL
API calls between multiple vendors.  It allows multiple drivers from
different vendors to coexist on the same file system, and determines which
vendor to dispatch each API call to at runtime.

Both GLX and EGL are supported, in any combination with OpenGL and OpenGL ES.")
    ;; libglvnd is available under a custom X11-style license, and incorporates
    ;; code with various other licenses.  See README.md for details.
    (license (list (license:x11-style "file://README.md")
                   license:x11
                   license:expat))))

(define-public libopenglrecorder
  (package
    (name "libopenglrecorder")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Benau/libopenglrecorder")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sfx2kdw2mca3mx4fnk1yy74pilp2i9npcpyj894qkngz5aaz2wl"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f)) ;no test suite
    (native-inputs (list pkg-config))
    (inputs (list libjpeg-turbo))
    (home-page "https://github.com/Benau/libopenglrecorder")
    (synopsis "Async readback OpenGL frame buffer with audio recording")
    (description
     "libopenglrecorder is a library allowing optional async readback OpenGL
frame buffer with optional audio recording.  It will do video and audio
encoding together.")
    (license license:bsd-3)))

(define-public soil
  (package
    (name "soil")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              ;; No versioned archive available.
              (uri "http://www.lonesock.net/files/soil.zip")
              (sha256
               (base32
                "00gpwp9dldzhsdhksjvmbhsd2ialraqbv6v6dpikdmpncj6mnc52"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'init-build
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (setenv "CFLAGS" "-fPIC") ; needed for shared library
                        ;; Use alternate Makefile
                        (copy-file "projects/makefile/alternate Makefile.txt"
                                   "src/Makefile")
                        (chdir "src")
                        (substitute* '("Makefile")
                          (("INCLUDEDIR = /usr/include/SOIL")
                           (string-append "INCLUDEDIR = " out "/include/SOIL"))
                          (("LIBDIR = /usr/lib")
                           (string-append "LIBDIR = " out "/lib"))
                          ;; Remove these flags from 'install' commands.
                          (("-o root -g root") ""))))))))
    (native-inputs
     (list unzip))
    (inputs
     (list mesa))
    (home-page "https://www.lonesock.net/soil.html")
    (synopsis "OpenGL texture loading library")
    (description
     "SOIL is a tiny C library used primarily for uploading textures into
OpenGL.")
    (license license:public-domain)))

(define-public glfw
  (package
    (name "glfw")
    (version "3.3.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/glfw/glfw"
                                  "/releases/download/" version
                                  "/glfw-" version ".zip"))
              (sha256
               (base32
                "023dn97n4h14n5lbjpzjv0y6a2plj254c0w3rr3wraf3z08189jm"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  (ice-9 format))
      #:tests? #f                       ;no test target
      #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-sonames
            (lambda* (#:key inputs #:allow-other-keys)
              (let-syntax ((patch-sonames
                            (syntax-rules ()
                              ((_ (file ...) soname ...)
                               (substitute* (list file ...)
                                 (((format #f "(~@{~a~^|~})" soname ...) lib)
                                  (search-input-file
                                   inputs (string-append
                                           "lib/" lib))))))))
                ;; Avoid looking in LD_LIBRARY_PATH for dlopen calls.
                (patch-sonames ("src/egl_context.c"
                                "src/glx_context.c"
                                "src/vulkan.c"
                                "src/wl_init.c"
                                "src/x11_init.c")
                               "libEGL.so.1"
                               "libGL.so"
                               "libGL.so.1"
                               "libGLESv1_CM.so.1"
                               "libGLESv2.so.2"
                               "libvulkan.so.1"
                               "libwayland-cursor.so.0"
                               "libwayland-egl.so.1"
                               "libxkbcommon.so.0"
                               "libXxf86vm.so.1"
                               "libXi.so.6"
                               "libXrandr.so.2"
                               "libXcursor.so.1"
                               "libXinerama.so.1"
                               "libX11-xcb.so.1"
                               "libXrender.so.1")))))))
    (native-inputs (list doxygen unzip))
    (inputs (list libxkbcommon wayland vulkan-loader))
    (propagated-inputs
     (list mesa              ;included in public headers
           ;; These are in 'Requires.private' of 'glfw3.pc'.
           libx11
           libxrandr
           libxi
           libxinerama
           libxcursor
           libxxf86vm))
    (home-page "https://www.glfw.org")
    (synopsis "OpenGL application development library")
    (description
     "GLFW is a library for OpenGL, OpenGL ES and Vulkan development for
desktop computers.  It provides a simple API for creating windows, contexts
and surfaces, receiving input and events.")
    (license license:zlib)))

(define-public nanovg-for-extempore
  (let ((version "0.7.1")
        (revision "0")
        (commit "3c60175fcc2e5fe305b04355cdce35d499c80310"))
    (package
      (name "nanovg-for-extempore")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/extemporelang/nanovg")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ddn3d3mxqn8hj9967v3pss7lz1wn08pcdnqzc118g7yjkq7hxzy"))))
      (build-system cmake-build-system)
      (arguments `(#:tests? #f))        ; no tests included
      (inputs
       (list mesa))
      ;; Extempore refuses to build on architectures other than x86_64
      (supported-systems '("x86_64-linux"))
      (home-page "https://github.com/extemporelang/nanovg")
      (synopsis "2D vector drawing library on top of OpenGL")
      (description "NanoVG is small antialiased vector graphics rendering
library for OpenGL.  It has lean API modeled after HTML5 canvas API.  It is
aimed to be a practical and fun toolset for building scalable user interfaces
and visualizations.")
      (license license:zlib))))

(define-public gl2ps
  (package
    (name "gl2ps")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://geuz.org/gl2ps/src/gl2ps-"
             version ".tgz"))
       (sha256
        (base32 "1sgzv547h7hrskb9qd0x5yp45kmhvibjwj2mfswv95lg070h074d"))))
    (build-system cmake-build-system)
    (inputs
     (list libpng mesa zlib))
    (arguments
     `(#:tests? #f))                    ; no tests
    (home-page "https://www.geuz.org/gl2ps/")
    (synopsis "OpenGL to PostScript printing library")
    (description "GL2PS is a C library providing high quality vector
output for any OpenGL application.  GL2PS uses sorting algorithms
capable of handling intersecting and stretched polygons, as well as
non-manifold objects.  GL2PS provides many features including advanced
smooth shading and text rendering, culling of invisible primitives and
mixed vector/bitmap output.")
    ;; GL2PS is dual-licenced and can be used under the terms of either.
    (license (list license:lgpl2.0+
                   (license:fsf-free "http://www.geuz.org/gl2ps/COPYING.GL2PS"
                                     "GPL-incompatible copyleft license")))))

(define-public virtualgl
  (package
    (name "virtualgl")
    (version "2.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VirtualGL/virtualgl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yyc553xsb5n0rx7jp9p4wdbd7md07b3qrkf3ssyjavqqg908qg9"))))
    (arguments
     `(#:tests? #f                      ; no tests are available
       #:configure-flags (list
                          (string-append "-DCMAKE_INSTALL_LIBDIR="
                                         (assoc-ref %outputs "out") "/lib")
                          "-DVGL_USESSL=1"))) ; use OpenSSL
    (build-system cmake-build-system)
    (inputs (list glu libjpeg-turbo libxtst mesa openssl))
    (native-inputs (list pkg-config))
    (home-page "https://www.virtualgl.org")
    (synopsis "Redirects 3D commands from an OpenGL application onto a 3D
graphics card")
    (description "VirtualGL redirects the 3D rendering commands from OpenGL
applications to 3D accelerator hardware in a dedicated server and displays the
rendered output interactively to a thin client located elsewhere on the
network.")
    (license license:wxwindows3.1+)))

(define-public mojoshader
  (let ((changeset "5887634ea695"))
    (package
      (name "mojoshader")
      (version (string-append "20190825" "-" changeset))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://hg.icculus.org/icculus/mojoshader/")
               (changeset changeset)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ibl4z1696jiifv9j5drir7jm0b5px0vwkwckbi7cfd46p7p6wcy"))))
      (arguments
       ;; Tests only for COMPILER_SUPPORT=ON.
       `(#:tests? #f
         #:configure-flags '("-DBUILD_SHARED=ON"
                             "-DFLIP_VIEWPORT=ON"
                             "-DDEPTH_CLIPPING=ON")
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib"))
                      (header (string-append out "/include")))
                 (install-file "libmojoshader.so" lib)
                 (for-each (lambda (f)
                             (install-file f header))
                           (find-files "../source" "mojoshader.*\\.h$"))
                 (let ((profiles-header (string-append header "/profiles")))
                   (mkdir-p profiles-header)
                   (rename-file (string-append header "/mojoshader_profile.h")
                                (string-append profiles-header "/mojoshader_profile.h"))))
               #t)))))
      (build-system cmake-build-system)
      (home-page "https://www.icculus.org/mojoshader/")
      (synopsis "Work with Direct3D shaders on alternate 3D APIs")
      (description "MojoShader is a library to work with Direct3D shaders on
alternate 3D APIs and non-Windows platforms.  The primary motivation is moving
shaders to OpenGL languages on the fly.  The developer deals with \"profiles\"
that represent various target languages, such as GLSL or ARB_*_program.

This allows a developer to manage one set of shaders, presumably written in
Direct3D HLSL, and use them across multiple rendering backends.  This also
means that the developer only has to worry about one (offline) compiler to
manage program complexity, while MojoShader itself deals with the reduced
complexity of the bytecode at runtime.

MojoShader provides both a simple API to convert bytecode to various profiles,
and (optionally) basic glue to rendering APIs to abstract the management of
the shaders at runtime.")
      (license license:zlib))))

(define-public mojoshader-with-viewport-flip
  ;; Changeset c586d4590241 replaced glProgramViewportFlip with
  ;; glProgramViewportInfo.
  ;; https://hg.icculus.org/icculus/mojoshader/rev/c586d4590241
  (let ((changeset "2e37299b13d8"))
    (package
      (inherit mojoshader)
      (name "mojoshader-with-viewport-flip")
      (version (string-append "20190725" "-" changeset))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://hg.icculus.org/icculus/mojoshader/")
               (changeset changeset)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ffws7cqbskxwc3hjsnnzq4r2bbf008kdr3b11pa3kr7dsi50y6i"))))
      (synopsis "Work with Direct3D shaders on alternate 3D APIs (with viewport flip)")
      (description "This is the last version of the mojoshader library with
the glProgramViewportFlip before it was replaced with glProgramViewportInfo.")
      (license license:zlib))))

(define-public glmark2
  (package
    (name "glmark2")
    (version "2023.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/glmark2/glmark2")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "094dr0ljg1hq6wymw2hb3369p4g91sn5c2qf554dl0dbdbjdqasq"))))
    (build-system meson-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:configure-flags
       (list (string-append "-Dflavors="
                            (string-join '("x11-gl" "x11-glesv2"
                                           "drm-gl" "drm-glesv2"
                                           "wayland-gl" "wayland-glesv2")
                                         ",")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((mesa (assoc-ref inputs "mesa")))
               (substitute* (find-files "src" "gl-state-.*\\.cpp$")
                 (("libGL.so") (string-append mesa "/lib/libGL.so"))
                 (("libEGL.so") (string-append mesa "/lib/libEGL.so"))
                 (("libGLESv2.so") (string-append mesa "/lib/libGLESv2.so")))
               #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list eudev
           libdrm
           libjpeg-turbo
           libpng
           libx11
           libxcb
           mesa
           wayland
           wayland-protocols))
    (home-page "https://github.com/glmark2/glmark2")
    (synopsis "OpenGL 2.0 and OpenGL ES 2.0 benchmark")
    (description
     "glmark2 is an OpenGL 2.0 and OpenGL ES 2.0 benchmark based on the
original glmark benchmark by Ben Smith.")
    (license license:gpl3+)))

(define-public waffle
  (package
    (name "waffle")
    (version "1.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.freedesktop.org/mesa/waffle")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1mrw0arlrpm83cwaz7rnimkkjv3a134rcmi1h512y2g4yjzhnm8r"))
              (modules '((ice-9 ftw)
                         (guix build utils)))
              (snippet #~(with-directory-excursion "third_party"
                           (let ((keep '("." ".." "meson.build" "threads")))
                             (for-each (lambda (f)
                                         (unless (member f keep)
                                           (delete-file-recursively f)))
                                       (scandir ".")))))))
    (build-system meson-build-system)
    (propagated-inputs (list mesa wayland))
    (native-inputs (list cmocka pkg-config))
    (home-page "https://waffle.freedesktop.org/")
    (synopsis "Choose OpenGL API at runtime")
    (description "Waffle is a library that allows one to defer selection of an
 OpenGL API and a window system until runtime.")
    (license license:bsd-2)))

(define-public piglit
  (let ((revision "1")
        (commit "814046fe6942eac660ee4a6cc5fcc54011a49945"))
    (package
     (name "piglit")
     (version (git-version "0.0.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/mesa/piglit")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1bzaalcxskckfnwprw77sbbmfqi59by2j8imaq8ghnlzhlxv7mk7"))))
     (build-system cmake-build-system)
     (arguments
      (list #:configure-flags #~(list "-DPIGLIT_SSE2=OFF")
            ;; Tests are not invoked through cmake.  Instead, there are
            ;; pytest/tox-based tests for the framework, but they require
            ;; unpackaged plugins.
            #:tests? #f
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'patch-source
                  (lambda* (#:key inputs #:allow-other-keys)
                    (substitute* (find-files "framework/" "\\.py$")
                      (("'wflinfo'")
                       (string-append "'"
                                      (search-input-file inputs "/bin/wflinfo")
                                      "'")))))
                (add-after 'install 'wrap
                  (lambda* (#:key outputs #:allow-other-keys)
                    (wrap-script (string-append (assoc-ref outputs "out")
                                                "/bin/piglit")
                      `("GUIX_PYTHONPATH" prefix
                        (,(getenv "GUIX_PYTHONPATH")))))))))
     (inputs (list guile-3.0            ; for wrap-script
                   libxkbcommon
                   python python-lxml python-mako python-numpy
                   glslang vulkan-headers vulkan-loader
                   waffle))
     (native-inputs (list pkg-config))
     (home-page "https://piglit.freedesktop.org/")
     (synopsis "Test OpenGL implementations")
     (description "Piglit is a collection of automated tests for OpenGL and
OpenCL implementations.")
     ;; A mix of licenses for various tests
     (license (list license:expat
                    license:bsd-3
                    license:gpl2+
                    license:gpl3+)))))
