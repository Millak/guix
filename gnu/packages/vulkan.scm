;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022 Kaelyn Takata <kaelyn.alexi@protonmail.com>
;;; Copyright © 2022, 2024 dan <i@dan.games>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
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
    (version "1.3.280.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Headers")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32
         "17jw5gwj2vmicyd6522b1zp7x551krfj826j0fg5kl0ixv0q08wk"))
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
    (version "1.3.280.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/KhronosGroup/SPIRV-Tools")
            (commit (string-append "vulkan-sdk-" version))))
      (sha256
       (base32 "1sj84ngwcgmydlj88nx1a9jfmhmxlij7wc92khp8wf1vsfplayas"))
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
    (version "1.3.280.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Cross")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32 "1k6fbkradknxis85akzzksz9ipm3v42xvrzaamwj2lrgfm8d6r4d"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
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
           (lambda _
             (with-directory-excursion "../source"
               (invoke "./update_test_shaders.sh")))))))
    (inputs
     (list glslang spirv-headers spirv-tools))
    (native-inputs (list python))
    (home-page "https://github.com/KhronosGroup/SPIRV-Cross")
    (synopsis "Parser for and converter of SPIR-V to other shader languages")
    (description
     "SPIRV-Cross tries hard to emit readable and clean output from the
SPIR-V, aiming to emit GLSL or MSL that looks like human-written code.")
    (license license:asl2.0)))

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
                              #$(this-package-native-input "spirv-headers")
                              "/include/spirv")
               (string-append "-DLLVM_EXTERNAL_LIT="
                              #$(this-package-native-input "python-lit")
                              "/bin/lit")
               (string-append "-DCMAKE_EXE_LINKER_FLAGS=-Wl,-rpath="
                              #$output "/lib")
               "-DBUILD_SHARED_LIBS=ON"
               "-DLLVM_SPIRV_INCLUDE_TESTS=ON")))
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
    (version "1.3.280.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/glslang")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32
         "1vvgqvwhsimlz8wkk38b9cvp9abggq840iws8al0znzz3mnvkfdn"))
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

(define-public vulkan-headers
  (package
    (name "vulkan-headers")
    (version "1.3.280.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Headers")
             (commit (string-append "vulkan-sdk-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13mmv5621z73hlfnsrccbcb4z0d7kwj92a081701vbpss45a4whj"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; No tests.
    (home-page
     "https://github.com/KhronosGroup/Vulkan-Headers")
    (synopsis "Vulkan Header files and API registry")
    (description
     "Vulkan-Headers contains header files and API registry for Vulkan.")
    (license (list license:asl2.0)))) ;LICENSE.txt

(define-public vulkan-loader
  (package
    (name "vulkan-loader")
    (version "1.3.280.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Loader")
             (commit (string-append "vulkan-sdk-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0glix3clqkdbi9kqcp8abmglqpgjd2r2bjqvi11r8sair0z54hnf"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; As many as 23 tests are expected to fail per architecture.
      ;; Limit the tests to those architectures tested upstream.
      #:tests? (and (%current-system)
                    (target-x86?))
      #:configure-flags
      #~(list (string-append "-DVULKAN_HEADERS_INSTALL_DIR="
                             (dirname (dirname
                                       (search-input-directory
                                        %build-inputs "include/vulkan"))))
              #$@(if (%current-target-system)
                     #~("-DBUILD_TESTS=OFF" "-DUSE_GAS=OFF")
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
     (list vulkan-headers libxrandr))
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
    (version "1.3.280.0")
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
         "0w0m04vscr4a6vr682g3mn7mfni740cmai9ylzlgfdggb77y58xz"))))
    (build-system cmake-build-system)
    (inputs
     (list glslang libxrandr vulkan-loader wayland wayland-protocols))
    (native-inputs
     (list googletest pkg-config python volk vulkan-headers))
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
    (version "2024.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/shaderc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m5jncn6y8c6g83f8nwh86hz33mvv03x7fyr5zq0ynwanrcpn2hb"))))
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
    (version "1.3.280.0")
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
                "1w6fsaicrgnzkj5vz2v86a2gk1n7478q6n66ac2920avnin9a64c"))))
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

(define-public volk
  (package
    (name "volk")
    (version "1.3.280.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zeux/volk")
                    (commit (string-append "vulkan-sdk-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x4jhc8n9c4k8svmmcaxxs613xbsav7wam94gacddlm738cwp13v"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ;no test
       #:configure-flags '("-DVOLK_INSTALL=ON" "-DVOLK_PULL_IN_VULKAN=ON")))
    (inputs (list vulkan-headers))
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
    (version "3.0.1")
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
         "1hpzjwl5bgqv9hmf1fdldihfllcbdg515f391a200klg0rnixdds"))))
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
    (version "1.3.280.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Utility-Libraries")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32 "17fmalilczs4x435f8kdx8bf0x5mnjhkmcp34xap8lanpbyzs84q"))
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
