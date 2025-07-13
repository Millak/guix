;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2023-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022 Kaelyn Takata <kaelyn.alexi@protonmail.com>
;;; Copyright © 2022, 2024 dan <i@dan.games>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2025 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages vulkan)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wine)
  #:use-module (gnu packages xorg))

(define-public spirv-headers
  (package
    (name "spirv-headers")
    (version "1.4.313.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Headers")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32
         "1ndbzcqq337gs5nkh0yf1lz1n5sdanc06aqqrwl8l9ggdpp2sj3d"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no tests
    (home-page "https://github.com/KhronosGroup/SPIRV-Headers")
    (synopsis "Machine-readable files from the SPIR-V Registry")
    (description
     "SPIRV-Headers is a repository containing machine-readable files from
the SPIR-V Registry.  This includes:
@itemize
@item Header files for various languages.
@item JSON files describing the grammar for the SPIR-V core instruction set,
and for the GLSL.std.450 extended instruction set.
@item The XML registry file.
@end itemize\n")
    (license (license:x11-style
              (string-append "https://github.com/KhronosGroup/SPIRV-Headers/blob/"
                             version "/LICENSE")))))

(define-public spirv-tools
  (package
    (name "spirv-tools")
    (version "1.4.313.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/KhronosGroup/SPIRV-Tools")
            (commit (string-append "vulkan-sdk-" version))))
      (sha256
       (base32 "0s1v894024bmhqjp4pk7706j0vaxm8chxz6nk6vgasrf24wq8v4w"))
      (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON"
                               ;; Some packages like mpv fail to link
                               ;; when the static libraries are built.
                               "-DSPIRV_TOOLS_BUILD_STATIC=OFF"
                               (string-append
                                "-DSPIRV-Headers_SOURCE_DIR="
                                (assoc-ref %build-inputs "spirv-headers")))))
    (inputs (list spirv-headers))
    (native-inputs (list pkg-config python))
    (home-page "https://github.com/KhronosGroup/SPIRV-Tools")
    (synopsis "API and commands for processing SPIR-V modules")
    (description
     "The SPIR-V Tools project provides an API and commands for processing
SPIR-V modules.  The project includes an assembler, binary module
parser,disassembler, validator, and optimizer for SPIR-V.")
    (license license:asl2.0)))

(define-public spirv-cross
  (package
    (name "spirv-cross")
    (version "1.4.313.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Cross")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32 "1h246sy4hxpb5yw0a34b2bhd5qrrvflqrgr20n0058f6aigggxj6"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(;; Disable tests for now due to upstream issue hit when running
       ;; update-reference-shaders phase:
       ;; <https://github.com/KhronosGroup/SPIRV-Tools/issues/5980>.
       #:tests? #f
       #:configure-flags
       (list "-DSPIRV_CROSS_SHARED=YES")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests-to-find-deps
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{CMAKE_(.*)_DIR\\}/external/glslang(.*)/bin")
                (string-append (assoc-ref inputs "glslang") "/bin")))
             (substitute* "CMakeLists.txt"
               (("\\$\\{CMAKE_(.*)_DIR\\}/external/spirv-tools(.*)/bin")
                (string-append (assoc-ref inputs "spirv-tools") "/bin")))))
         (add-before 'check 'update-reference-shaders
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "../source"
                 (invoke "./update_test_shaders.sh"))))))))
    (inputs
     (list glslang spirv-headers spirv-tools))
    (native-inputs (list python))
    (home-page "https://github.com/KhronosGroup/SPIRV-Cross")
    (synopsis "Parser for and converter of SPIR-V to other shader languages")
    (description
     "SPIRV-Cross tries hard to emit readable and clean output from the
SPIR-V, aiming to emit GLSL or MSL that looks like human-written code.")
    (license license:asl2.0)))

;; WARNING: This package is a dependency of mesa.
(define-public spirv-llvm-translator
  (package
    (name "spirv-llvm-translator")
    (version "18.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-LLVM-Translator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yfz02mlnf4ffn67g2ms0w8f7jgdsn438w2dbxd5mvcf5dk2x27b"))))
    (build-system cmake-build-system)
    (arguments
     ;; The test suite is known to fail on several architectures:
     ;; https://github.com/llvm/llvm-project/issues/59637
     (list
       #:tests? (and (not (%current-target-system))
                     (target-x86-64?))
       #:configure-flags
       #~(list (string-append "-DLLVM_EXTERNAL_SPIRV_HEADERS_SOURCE_DIR="
                              #+(this-package-native-input "spirv-headers"))
               (string-append "-DLLVM_EXTERNAL_LIT="
                              #+(this-package-native-input "python-lit")
                              "/bin/lit")
               (string-append "-DCMAKE_EXE_LINKER_FLAGS=-Wl,-rpath="
                              #$output "/lib")
               "-DBUILD_SHARED_LIBS=ON"
               "-DLLVM_SPIRV_INCLUDE_TESTS=ON")
       #:modules '((guix build cmake-build-system)
                   ((guix build gnu-build-system) #:prefix gnu:)
                   (guix build utils))
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:rest args)
               (apply (assoc-ref gnu:%standard-phases 'check)
                      #:test-target "test" args))))))
    (inputs (list llvm-18))
    (native-inputs (list clang-18 llvm-18 python-lit spirv-headers))
    (home-page "https://github.com/KhronosGroup/SPIRV-LLVM-Translator")
    (synopsis "Bi-directional translation between SPIR-V and LLVM IR")
    (description
     "The LLVM/SPIR-V Bi-Directional Translator is a library and tool for
translation between LLVM IR and SPIR-V.")
    (license license:asl2.0)))

(define-public glslang
  (package
    (name "glslang")
    (version "1.4.313.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/glslang")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32
         "1b3znvbvbhcnzcab221pj99zs60905fmkhav856f00vflbh4y08z"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DALLOW_EXTERNAL_SPIRV_TOOLS=ON"
                           ,@(if (target-riscv64?)
                                 `("-DCMAKE_EXE_LINKER_FLAGS=-latomic")
                                 '()))
       #:phases (modify-phases %standard-phases
                  ,@(if (target-ppc32?)
                        `((add-after 'unpack 'skip-failing-test
                            (lambda _
                              ;; TODO: Figure out why this test fails.
                              (substitute* "Test/runtests"
                                ((".*remap\\.invalid" all)
                                 (string-append "# " all))))))
                        '())
                  (replace 'check
                    (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
                      (when tests?
                        (invoke "ctest"
                                "-j" (if parallel-tests?
                                       (number->string (parallel-job-count))
                                       "1")
                                "--rerun-failed"
                                "--output-on-failure")))))))
    (inputs (list spirv-tools))
    (native-inputs
     (list pkg-config python))
    (home-page "https://github.com/KhronosGroup/glslang")
    (synopsis "OpenGL and OpenGL ES shader front end and validator")
    (description
     "Glslang is the official reference compiler front end for the
OpenGL@tie{}ES and OpenGL shading languages.  It implements a strict
interpretation of the specifications for these languages.")
    ;; Modified BSD license. See "copyright" section of
    ;; https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/
    (license (list license:bsd-3
                   ;; include/SPIRV/{bitutils,hex_float}.h are Apache 2.0.
                   license:asl2.0))))

(define-public vkbasalt
  (package
    (name "vkbasalt")
    (version "0.3.2.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DadSchoorse/vkBasalt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f3qmcmqnnh8i9qd2sd3p5w0akn8rkzfm5z0hc0wazgci4lqjbhq"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           ;; Needed for giving full path to library.
           #~(list "-Dappend_libdir_vkbasalt=TRUE")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-libdir
                 (lambda _
                   (substitute* "meson.build"
                     (("\\$LIB") "lib")))))))
    (native-inputs (list pkg-config
                         ;; for glslangValidator
                         glslang))
    (inputs (list libx11 spirv-headers vulkan-headers))
    (home-page "https://github.com/DadSchoorse/vkBasalt")
    (synopsis "Vulkan post processing layer for GNU/Linux")
    (description "vkBasalt is a Vulkan post processing layer to enhance the
visual graphics of games.

Currently, the built-in effects are:

@itemize @bullet
@item
Contrast Adaptive Sharpening
@item
Denoise Luma Sharpening
@item
Fast Approximate Anti-Aliasing
@item
Enhanced Subpixel Morphological Anti-Aliasing
@item
3D color LookUp Table
@end itemize")
    (license (list license:bsd-3    ; src/reshade/LICENSE.md
                   license:zlib)))) ; LICENSE

;; vulkan-headers, but without the path to vulkan-loader patched in.
(define-public vulkan-headers/no-loader
  (package
    (name "vulkan-headers")
    (version "1.4.313.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Headers")
             (commit (string-append "vulkan-sdk-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mfmdxip5sxf2mc0b7vg80hc7mcc9ygg9mgdjfd113czg1079fvi"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; No tests.
    (properties '((hidden? . #t)))
    (home-page
     "https://github.com/KhronosGroup/Vulkan-Headers")
    (synopsis "Vulkan Header files and API registry")
    (description
     "Vulkan-Headers contains header files and API registry for Vulkan.")
    (license (list license:asl2.0)))) ;LICENSE.txt

(define-public vulkan-headers
  (package
    (inherit vulkan-headers/no-loader)
    (arguments
     (substitute-keyword-arguments (package-arguments vulkan-headers/no-loader)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch-libvulkan-file-name
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "include/vulkan/vulkan.hpp"
                  (("dlopen\\( \"libvulkan.so")
                   (string-append "dlopen(\""
                                  (search-input-file
                                   inputs "/lib/libvulkan.so"))))))))))
    (inputs
     (modify-inputs (package-inputs vulkan-headers/no-loader)
       (prepend vulkan-loader)))
    (properties '())))

(define-public vulkan-loader
  (package
    (name "vulkan-loader")
    (version "1.4.313.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Loader")
             (commit (string-append "vulkan-sdk-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ycwgz012098xhgi8an7jy3n755k5j47v18wpq62sikldz4j7qh9"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; As many as 23 tests are expected to fail per architecture.
      ;; Limit the tests to those architectures tested upstream.
      #:tests? (and (not (%current-target-system))
                    (target-x86?))
      #:parallel-tests? #f
      #:configure-flags
      #~(list (string-append "-DVULKAN_HEADERS_INSTALL_DIR="
                             (dirname (dirname
                                       (search-input-directory
                                        %build-inputs "include/vulkan"))))
              #$@(if (%current-target-system)
                     #~("-DBUILD_TESTS=OFF" "-DUSE_GAS=OFF"
                        (string-append "-DPKG_CONFIG_EXECUTABLE="
                                       (search-input-file
                                        %build-inputs
                                        (string-append "bin/" #$(pkg-config-for-target)))))
                     #~("-DBUILD_TESTS=ON")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'fix-pkg-config-file
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((vulkan-headers (dirname (search-input-directory
                                               inputs "include/vulkan"))))
                 ;; Ensure the pkg-config file refers to vulkan-headers.
                 (substitute* "loader/vulkan.pc.in"
                   (("^includedir=.*")
                    (string-append "includedir=" vulkan-headers "\n"))))))
           (add-after 'unpack 'use-system-googletest
             (lambda _
               (substitute* "tests/CMakeLists.txt"
                 (((string-append "message\\(FATAL_ERROR \"Could not "
                                  "find googletest directory. See BUILD.md\"\\)"))
                  "find_package(GTest REQUIRED)"))
               ;; Use the namespaced variable.
               (substitute* "tests/framework/CMakeLists.txt"
                 (("PUBLIC gtest ")
                  "PUBLIC GTest::gtest ")))))))
    (native-inputs
     (list googletest
           libxrandr
           pkg-config
           python
           wayland))
    (inputs
     (list vulkan-headers/no-loader libxrandr))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (home-page
     "https://github.com/KhronosGroup/Vulkan-Loader")
    (synopsis "Khronos official ICD loader and validation layers for Vulkan")
    (description
     "Vulkan allows multiple @dfn{Installable Client Drivers} (ICDs) each
supporting one or more devices to be used collectively.  The loader is
responsible for discovering available Vulkan ICDs on the system and inserting
Vulkan layer libraries, including validation layers between the application
and the ICD.")
    ;; This software is mainly Apache 2.0 licensed, but contains some components
    ;; covered by other licenses.  See COPYRIGHT.txt for details.
    (license (list license:asl2.0       ;LICENSE.txt
                   (license:x11-style "file://COPYRIGHT.txt")
                   license:bsd-3))))

(define-public vulkan-tools
  (package
    (name "vulkan-tools")
    (version "1.4.309.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Tools")
             (commit (string-append "vulkan-sdk-" version))))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet #~(substitute* "tests/icd/mock_icd_tests.cpp"
                    ;; Disable driver info test since it relies on git branch info
                    (("ASSERT_EQ\\(std::string\\(driver_properties\\.driverInfo\\)")
                     "// ASSERT_EQ(std::string(driver_properties.driverInfo)")))
       (sha256
        (base32
         "0ywvvkra29y2cvw8i9laf4skn6cl7phrwshcc7z9dljb3il87cym"))))
    (build-system cmake-build-system)
    (inputs
     (list glslang libxrandr vulkan-loader wayland wayland-protocols))
    (native-inputs
     (list googletest pkg-config python vulkan-volk vulkan-headers))
    (arguments
     `(#:configure-flags (list "-DBUILD_TESTS=ON")
       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "./tests/vulkan_tools_tests")))))))
    (home-page
     "https://github.com/KhronosGroup/Vulkan-Tools")
    (synopsis "Tools and utilities for Vulkan")
    (description
     "Vulkan-Tools provides tools and utilities that can assist development by
enabling developers to verify their applications correct use of the Vulkan
API.")
    (license (list license:asl2.0)))) ;LICENSE.txt

(define-public shaderc
  (package
    (name "shaderc")
    ;; shaderc doesn't follow the versioning scheme of vulkan sdk
    (version "2025.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/shaderc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0w1mj5b3n6kp0brqindb7fppvllzlywkdk1zglkbj3bw8k0795mb"))))
    (build-system cmake-build-system)
    (arguments
     `(;; FIXME: Skip most of the tests, because enabling system gtest breaks
       ;; the build: <https://github.com/google/shaderc/issues/470>.
       #:configure-flags
       (list "-DSHADERC_SKIP_TESTS=ON"
             ;; The two flags are copied from:
             ;; https://sdk.lunarg.com/sdk/download/1.3.280.0/linux/config.json
             "-DSHADERC_ENABLE_SHARED_CRT=ON"
             "-DSHADERC_SKIP_COPYRIGHT_CHECK=ON"
             "-DPYTHON_EXECUTABLE=python3"
             ;; Note: despite the name, this just specifies the headers.
             (string-append "-Dglslang_SOURCE_DIR="
                            (assoc-ref %build-inputs "glslang") "/include/glslang"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-look-for-bundled-sources
           (lambda _
             (substitute* "CMakeLists.txt"
               (("add_subdirectory\\(third_party\\)")
                ""))

             (substitute* "glslc/test/CMakeLists.txt"
               (("\\$<TARGET_FILE:spirv-dis>")
                (which "spirv-dis")))

             ;; Do not attempt to use git to encode version information.
             (substitute* "glslc/CMakeLists.txt"
               (("add_dependencies\\(glslc_exe build-version\\)")
                ""))
             (call-with-output-file "glslc/src/build-version.inc"
               (lambda (port)
                 (format port "\"~a\"\n\"~a\"\n\"~a\"~%"
                         ,version
                         ,(package-version spirv-tools)
                         ,(package-version glslang))))
             #t)))))
    (inputs
     (list glslang spirv-headers spirv-tools))
    (native-inputs
     (list pkg-config python))
    (home-page "https://github.com/google/shaderc")
    (synopsis "Tools for shader compilation")
    (description "Shaderc is a collection of tools, libraries, and tests for
shader compilation.")
    (license license:asl2.0)))

(define-public vkd3d
  (let ((commit "56cd4a94d541707959ce7677af6d1a34739e5579")) ; Release 1.2.
    (package
     (name "vkd3d")
     (version "1.2")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://source.winehq.org/git/vkd3d.git")
             (commit commit)))
       (sha256
        (base32
         "1n4a622drgnprvz5hjxzyzcsg2lp5rlf1sajki2vzf5gsx6fdpk8"))
       (file-name (string-append name "-" version "-checkout"))))
     (build-system gnu-build-system)
     (arguments
      `(#:configure-flags '("--with-spirv-tools")
        #:phases (modify-phases %standard-phases
                   (add-after 'unpack 'patch-for-new-vulkan
                     (lambda _
                       ;; Mimic upstream commit 8e7bf8a5c3e0047 for
                       ;; compatibility with newer vulkan-headers.
                       (substitute* "libs/vkd3d/vkd3d_private.h"
                         (("VK_PIPELINE_BIND_POINT_RANGE_SIZE")
                          "2u"))
                       #t)))))
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("gettext" ,gettext-minimal)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
     (inputs
      (list libx11
            libxcb
            spirv-headers
            spirv-tools
            vulkan-headers
            vulkan-loader
            wine-minimal ; Needed for 'widl'.
            xcb-util
            xcb-util-keysyms
            xcb-util-wm))
     (home-page "https://source.winehq.org/git/vkd3d.git/")
     (synopsis "Direct3D 12 to Vulkan translation library")
     (description "vkd3d is a library for translating Direct3D 12 to Vulkan.")
     (license license:lgpl2.1))))

(define-public vulkan-validationlayers
  (package
    (name "vulkan-validationlayers")
    (version "1.4.309.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/KhronosGroup/Vulkan-ValidationLayers")
                    (commit (string-append "vulkan-sdk-" version))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              ;; We don't build static libraries in SPIRV-Tools.
              (snippet '(substitute* "tests/CMakeLists.txt"
                          (("-static")
                           "")))
              (sha256
               (base32
                "1dvgbgfxcp3ypy06j5m561j1gag0hk40zqd477cdv1kizv6i7nsk"))))
    (build-system cmake-build-system)
    (inputs (list glslang
                  libxrandr
                  mesa
                  robin-hood-hashing
                  shaderc
                  spirv-tools
                  vulkan-loader
                  vulkan-utility-libraries
                  wayland))
    (native-inputs (list googletest pkg-config python spirv-headers vulkan-headers))
    (arguments
     (list #:tests? #f ; tests crash on some hardware (various upstream issues)
           #:configure-flags
           #~(list "-DBUILD_TESTS=ON")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'set-layer-path-in-manifest
                          (lambda _
                            (let ((manifest (string-append #$output
                                             "/share/vulkan/explicit_layer.d"
                                             "/VkLayer_khronos_validation.json")))
                              (substitute* manifest
                                (("\"libVkLayer_khronos_validation.so\"")
                                 (string-append "\"" #$output
                                                "/lib/libVkLayer_khronos_validation.so\""))))))
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "VK_LAYER_PATH"
                                      (string-append (getcwd) "/layers"))
                              (setenv "LD_LIBRARY_PATH"
                                      (string-append #$(this-package-input
                                                        "vulkan-loader") "/lib"))
                              (setenv "MESA_SHADER_CACHE_DIR"
                                      (string-append (getcwd) "/shader-cache"))
                              (setenv "XDG_RUNTIME_DIR" (getcwd))
                              (invoke "./tests/vk_layer_validation_tests")))))))
    (home-page "https://github.com/KhronosGroup/Vulkan-ValidationLayers")
    (synopsis "Khronos official validation layers for Vulkan")
    (description
     "Vulkan-ValidationLayers provides the Khronos official validation layers that
can assist development by enabling developers to verify their applications correctly
use the Vulkan API.")
    (license license:asl2.0)))

(define-public vulkan-volk
  (package
    (name "vulkan-volk")
    (version "1.4.304")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zeux/volk")
                    (commit (string-append "vulkan-sdk-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0slgshhrr1l08wkc4n0ky2z670cfrnzw8gxdrznmja4ly13cc8pr"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                      ;no test
      #:configure-flags #~(list "-DVOLK_INSTALL=ON" "-DVOLK_PULL_IN_VULKAN=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-loader-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "volk.c"
                (("dlopen\\(\"libvulkan.so")
                 (string-append "dlopen(\""
                                (search-input-file
                                 inputs "/lib/libvulkan.so")))))))))
    (inputs (list vulkan-headers vulkan-loader))
    (synopsis "Meta loader for Vulkan API")
    (description
     "Volk is a meta-loader for Vulkan.  It allows you to dynamically load
entrypoints required to use Vulkan without linking the Vulkan loader.
Additionally, volk simplifies the use of Vulkan extensions by automatically
loading all associated entrypoints.  Finally, volk enables loading Vulkan
entrypoints directly from the driver which can increase performance by
skipping loader dispatch overhead.")
    (home-page "https://github.com/zeux/volk")
    (license license:expat)))

(define-public vulkan-memory-allocator
  (package
    (name "vulkan-memory-allocator")
    (version "3.2.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0y8ccx080bqrgv71ggixxpl57vc5znq55rnvl4v4srfkjxhz6yiy"))))
    (build-system cmake-build-system)
    (arguments
     ;; no test
     `(#:tests? #f))
    (inputs (list vulkan-loader vulkan-headers))
    (synopsis "Vulkan memory allocation library")
    (description
     "The Vulkan Memory Allocator (VMA) library provides a simple and easy to
integrate API to help users allocate memory for Vulkan buffer and image
storage.")
    (home-page
     "https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator")
    (license license:expat)))

(define-public vulkan-utility-libraries
  (package
    (name "vulkan-utility-libraries")
    (version "1.4.313")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Utility-Libraries")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32 "0gymlk0qz2k2970gyrijvk749zw49ffhc25zxqhzsgxxar8vhq1j"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DBUILD_TESTS=ON")))
    (inputs (list vulkan-headers))
    (native-inputs (list googletest magic-enum))
    (home-page "https://github.com/KhronosGroup/Vulkan-Utility-Libraries")
    (synopsis "Utility libraries for Vulkan developers")
    (description "Utility libraries for Vulkan developers.")
    (license license:asl2.0)))
