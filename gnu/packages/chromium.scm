;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2025 Andre Batista <nandre@riseup.net>
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

(define-module (gnu packages chromium)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define %preserved-third-party-files
  '("base/third_party/cityhash" ;Expat
    "base/third_party/double_conversion" ;BSD-3
    "base/third_party/icu" ;Unicode, X11-style
    "base/third_party/superfasthash" ;BSD-3
    "base/third_party/symbolize" ;BSD-3
    "base/third_party/xdg_user_dirs" ;Expat
    "net/third_party/mozilla_security_manager" ;MPL-1.1/GPL2+/LGPL2.1+
    "net/third_party/nss" ;MPL-2.0
    "net/third_party/quiche" ;BSD-3
    "net/third_party/uri_template" ;ASL2.0
    "third_party/abseil-cpp" ;ASL2.0
    "third_party/angle" ;BSD-3
    "third_party/angle/src/common/third_party/xxhash" ;BSD-2
    "third_party/angle/src/third_party/ceval" ;Expat
    "third_party/angle/src/third_party/libXNVCtrl" ;Expat
    "third_party/angle/src/third_party/volk" ;Expat
    "third_party/anonymous_tokens" ;ASL2.0
    "third_party/apple_apsl" ;APSL2.0
    "third_party/axe-core" ;MPL2.0
    "third_party/bidimapper" ;ASL2.0
    "third_party/blink" ;BSD-3, LGPL2+
    "third_party/boringssl" ;OpenSSL/ISC (Google additions are ISC)
    "third_party/boringssl/src/third_party/fiat" ;Expat
    "third_party/breakpad" ;BSD-3
    "third_party/brotli" ;Expat
    "third_party/catapult" ;BSD-3
    "third_party/catapult/common/py_vulcanize/third_party/rcssmin" ;ASL2.0
    "third_party/catapult/common/py_vulcanize/third_party/rjsmin" ;ASL2.0
    "third_party/catapult/third_party/polymer" ;BSD-3
    "third_party/catapult/third_party/six" ;Expat
    ;; XXX: This is a minified version of <https://d3js.org/>.
    "third_party/catapult/tracing/third_party/d3" ;BSD-3
    "third_party/catapult/tracing/third_party/gl-matrix" ;Expat
    "third_party/catapult/tracing/third_party/jpeg-js" ;ASL2.0
    ;; XXX: Minified version of <https://github.com/Stuk/jszip>.
    "third_party/catapult/tracing/third_party/jszip" ;Expat or GPL3
    "third_party/catapult/tracing/third_party/mannwhitneyu" ;Expat
    "third_party/catapult/tracing/third_party/oboe" ;BSD-2
    ;; XXX: Minified version of <https://github.com/nodeca/pako>.
    "third_party/catapult/tracing/third_party/pako" ;Expat
    "third_party/ced" ;BSD-3
    "third_party/cld_3" ;ASL2.0
    "third_party/closure_compiler" ;ASL2.0
    "third_party/compiler-rt" ;ASL2.0
    "third_party/compiler-rt/src/lib" ;ASL2.0
    "third_party/compiler-rt/src/lib/builtins" ;ASL2.0
    "third_party/content_analysis_sdk" ;BSD-3
    "third_party/cpuinfo" ;BSD-2
    "third_party/crabbyavif" ;ASL2.0
    "third_party/crabbyavif/src/sys/dav1d-sys" ;ASL2.0
    "third_party/crabbyavif/src/sys/aom-sys" ;ASL2.0
    "third_party/crabbyavif/src/sys/libgav1-sys" ;ASL2.0
    "third_party/crabbyavif/src/sys/libyuv-sys" ;ASL2.0
    "third_party/crabbyavif/src/sys/ndk-sys" ;ASL2.0
    "third_party/crashpad" ;ASL2.0
    "third_party/crashpad/crashpad/third_party/lss" ;ASL2.0
    "third_party/crashpad/crashpad/third_party/zlib/zlib_crashpad.h" ;Zlib
    "third_party/crc32c" ;BSD-3
    "third_party/cros_system_api" ;BSD-3
    "third_party/dav1d" ;BSD-2
    "third_party/dawn" ;ASL2.0
    ;; TODO: can likely be unbundled when Vulkan is updated.
    "third_party/dawn/third_party/khronos" ;ASL2.0
    "third_party/dawn/third_party/gn/webgpu-cts" ;BSD-3
    "third_party/devtools-frontend" ;BSD-3
    "third_party/devtools-frontend/src/front_end"
    "third_party/devtools-frontend/src/front_end/third_party"
    "third_party/devtools-frontend/src/front_end/third_party/acorn" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party\
/additional_readme_paths.json" ;no explicit license; trivial
    "third_party/devtools-frontend/src/front_end/third_party/json5" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party/axe-core" ;MPL2.0
    "third_party/devtools-frontend/src/front_end/third_party/chromium" ;BSD-3
    "third_party/devtools-frontend/src/front_end/third_party/codemirror" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party/diff" ;ASL2.0
    "third_party/devtools-frontend/src/front_end/third_party/i18n" ;ASL2.0
    "third_party/devtools-frontend/src/front_end/third_party/intl-messageformat" ;BSD-3
    "third_party/devtools-frontend/src/front_end/third_party/lighthouse" ;ASL2.0
    "third_party/devtools-frontend/src/front_end/third_party/lit" ;BSD-3
    "third_party/devtools-frontend/src/front_end/third_party/marked" ;Expat, BSD-3
    "third_party/devtools-frontend/src/front_end/third_party/puppeteer" ;ASL2.0
    "third_party/devtools-frontend/src/front_end/third_party/puppeteer/package/lib/esm/third_party"
    "third_party/devtools-frontend/src/front_end/third_party/puppeteer\
/package/lib/esm/third_party/mitt" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party/puppeteer/third_party/mitt" ; Expat
    "third_party/devtools-frontend/src/front_end/third_party/puppeteer/third_party/parsel" ; Expat
    "third_party/devtools-frontend/src/front_end/third_party/puppeteer/third_party/rxjs" ; ASL2.0
    "third_party/devtools-frontend/src/front_end/third_party/puppeteer-replay" ;ASL2.0
    "third_party/devtools-frontend/src/front_end/third_party\
/vscode.web-custom-data" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party/third-party-web" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party/wasmparser" ;ASL2.0
    "third_party/devtools-frontend/src/third_party/i18n" ;ASL2.0
    "third_party/devtools-frontend/src/third_party/pyjson5" ;ASL2.0

    "third_party/dom_distiller_js" ;BSD-3
    "third_party/dragonbox" ;ASL2.0, Boost1.0
    "third_party/d3" ;Non-copyleft
    "third_party/eigen3" ;MPL2.0
    "third_party/emoji-segmenter" ;ASL2.0
    "third_party/farmhash" ;Expat
    "third_party/fast_float" ;ASL2.0, Boost1.0, Expat
    "third_party/fdlibm" ;non-copyleft
    "third_party/ffmpeg" ; LGPL2.1+, GPL2.0+, Expat and BSD
    "third_party/fft2d" ;non-copyleft
    "third_party/flatbuffers" ;ASL2.0
    "third_party/fp16" ;Expat
    "third_party/fusejs" ;ASL2.0
    "third_party/gemmlowp" ;ASL2.0
    "third_party/google_input_tools" ;ASL2.0
    "third_party/google_input_tools/third_party/closure_library" ;ASL2.0
    "third_party/google_input_tools/third_party/closure_library/third_party/closure" ;Expat
    "third_party/googletest" ;BSD-3
    "third_party/harfbuzz-ng" ;Expat
    "third_party/highway" ;ASL2.0
    "third_party/hunspell" ;MPL1.1/GPL2+/LGPL2.1+
    "third_party/ink" ;ASL2.0
    "third_party/ink_stroke_modeler" ;ASL2.0
    "third_party/inspector_protocol" ;BSD-3
    "third_party/ipcz" ;BSD-3
    "third_party/jinja2" ;BSD-3
    "third_party/jsoncpp" ; Public domain, Expat
    "third_party/khronos" ;Expat, SGI
    "third_party/lens_server_proto" ;Non-copyleft
    "third_party/leveldatabase" ;BSD-3
    "third_party/libaddressinput" ;ASL2.0
    "third_party/libaom" ;BSD-2 or "Alliance for Open Media Patent License 1.0"
    "third_party/libaom/source/libaom/third_party/fastfeat" ;BSD-3
    "third_party/libaom/source/libaom/third_party/SVT-AV1" ;BSD-3
    "third_party/libaom/source/libaom/third_party/vector" ;Expat
    "third_party/libaom/source/libaom/third_party/x86inc" ;ISC
    "third_party/libdrm" ; Expat
    "third_party/libgav1" ;ASL2.0
    "third_party/libjingle_xmpp" ;BSD-3
    "third_party/libphonenumber" ;ASL2.0
    "third_party/libsecret" ;LGPL2.1+
    "third_party/libsrtp" ;BSD-3
    "third_party/libsync" ;ASL2.0
    "third_party/libtess2" ;SGIFreeB2.0
    "third_party/liburlpattern" ;Expat
    "third_party/libva_protected_content" ;Expat

    ;; FIXME: build/linux/unbundle/libvpx.gn does not work for all users.
    "third_party/libvpx" ;BSD-3
    "third_party/libvpx/source/libvpx/third_party/x86inc" ;Expat

    "third_party/libwebm" ;BSD-3
    "third_party/libxml/chromium" ;BSD-3
    "third_party/libyuv" ;BSD-3
    "third_party/libzip" ;BSD-3
    "third_party/lit" ;BSD-3
    "third_party/lit/v3_0" ;BSD-3
    "third_party/lottie" ;Expat
    "third_party/lss" ;BSD-3
    "third_party/lzma_sdk" ;public domain
    "third_party/mako" ;Expat
    "third_party/markupsafe" ;BSD-3
    "third_party/material_color_utilities" ;ASL2.0
    "third_party/metrics_proto" ;BSD-3
    "third_party/minigbm" ;BSD-3
    "third_party/ml_dtypes" ;ASL2.0
    "third_party/modp_b64" ;BSD-3
    "third_party/nasm" ;BSD-2
    "third_party/nearby" ;ASL2.0
    "third_party/node" ;Expat
    "third_party/omnibox_proto" ;BSD-3
    "third_party/one_euro_filter" ;BSD-3
    "third_party/openscreen" ;BSD-3
    "third_party/openscreen/src/third_party/tinycbor" ;Expat
    "third_party/ots" ;BSD-3
    "third_party/pdfium" ;BSD-3
    "third_party/pdfium/third_party/agg23" ;Expat
    "third_party/pdfium/third_party/bigint" ;Public domain, BSD-3
    "third_party/pdfium/third_party/libtiff" ;non-copyleft
    "third_party/pdfium/third_party/freetype/include/pstables.h" ;FreeType
    "third_party/perfetto" ;ASL2.0
    "third_party/perfetto/protos/third_party/chromium" ;BSD-3
    "third_party/pffft" ;the "FFTPACK" license
    "third_party/ply" ;BSD-3
    "third_party/polymer" ;BSD-3
    "third_party/private_membership" ;ASL2.0
    "third_party/private-join-and-compute" ;ASL2.0
    "third_party/protobuf" ;BSD-3
    "third_party/protobuf/third_party/utf8_range" ; Expat
    "third_party/pthreadpool" ;BSD-2
    "third_party/puffin" ;Non-copyleft
    "third_party/pyjson5" ;ASL2.0
    "third_party/rapidhash" ;BSD-2
    "third_party/readability" ;ASL2.0
    "third_party/re2" ;Non-copyleft
    "third_party/rnnoise" ;BSD-3
    "third_party/rust" ; Expat
    "third_party/ruy" ;ASL2.0
    "third_party/s2cellid" ;ASL2.0
    "third_party/search_engines_data" ;BSD-3
    "third_party/securemessage" ;ASL2.0
    "third_party/selenium-atoms" ;ASL2.0
    "third_party/shell-encryption" ;ASL2.0
    "third_party/simdutf" ;Non-copyleft
    "third_party/simplejson" ; Non-copyleft
    "third_party/skia" ;BSD-3
    "third_party/skia/third_party/vulkanmemoryallocator" ;BSD-3, Expat
    "third_party/smhasher" ;Expat, public domain

    ;; FIXME: the snappy "replacement" shim (see replace_gn_files.py below) does
    ;; not declare a library dependency added in M93, causing a link failure.
    "third_party/snappy" ;BSD-3

    "third_party/speech-dispatcher" ;GPL2+
    "third_party/spirv-headers" ;ASL2.0
    "third_party/spirv-tools" ;ASL2.0
    "third_party/sqlite" ;Public domain
    "third_party/swiftshader" ;ASL2.0
    "third_party/swiftshader/third_party/astc-encoder" ;ASL2.0
    "third_party/swiftshader/third_party/llvm-10.0" ;ASL2.0, with LLVM exception
    "third_party/swiftshader/third_party/llvm-subzero" ;NCSA
    "third_party/swiftshader/third_party/marl" ;ASL2.0
    "third_party/swiftshader/third_party/subzero" ;NCSA
    "third_party/swiftshader/third_party/SPIRV-Headers" ;X11-style
    "third_party/swiftshader/third_party/SPIRV-Tools" ;ASL2.0
    "third_party/tensorflow-text" ;ASL2.0
    "third_party/tensorflow_models" ;ASL2.0
    "third_party/tflite" ;ASL2.0
    "third_party/tflite/src/third_party/fft2d" ;ASL2.0
    "third_party/tflite/src/third_party/xla" ;ASL2.0
    "third_party/tflite/src/third_party/xla/xla" ;ASL2.0
    "third_party/tflite/src/third_party/xla/xla/tsl/framework" ;ASL2.0
    "third_party/tflite/src/third_party/xla/xla/tsl/lib/random" ;ASL2.0
    "third_party/tflite/src/third_party/xla/xla/tsl/platform" ;ASL2.0
    "third_party/tflite/src/third_party/xla/xla/tsl/protobuf" ;ASL2.0
    "third_party/tflite/src/third_party/xla/xla/tsl/util" ;ASL2.0
    "third_party/tflite/src/third_party/xla/third_party/tsl" ;ASL2.0
    "third_party/tflite/src/third_party/xla/third_party/tsl/tsl" ;ASL2.0
    "third_party/tflite/src/third_party/xla/third_party/tsl/tsl/platform" ;ASL2.0
    "third_party/tflite/src/third_party/xla/third_party/tsl/tsl/profiler" ;ASL2.0
    "third_party/ukey2" ;ASL2.0
    "third_party/ungoogled-chromium" ;BSD-3
    "third_party/usb_ids" ;BSD-3
    "third_party/utf" ;Expat
    "third_party/vulkan-deps" ;ASL2.0, BSD-3, Expat
    "third_party/vulkan-headers" ;ASL2.0
    "third_party/vulkan-loader" ;ASL2.0
    "third_party/vulkan_memory_allocator" ;Expat
    "third_party/vulkan-tools" ;ASL2.0
    "third_party/vulkan-validation-layers" ;ASL2.0
    "third_party/vulkan-utility-libraries" ;ASL2.0
    "third_party/wasm_tts_engine" ; ASL2.0, Non-copyleft
    "third_party/wayland" ;BSD-3, Expat
    "third_party/wayland-protocols" ;Expat
    "third_party/webdriver" ;ASL2.0
    "third_party/webgpu-cts" ;BSD-3
    "third_party/webrtc" ;BSD-3
    "third_party/webrtc/common_audio/third_party/ooura" ;Non-copyleft
    "third_party/webrtc/common_audio/third_party/spl_sqrt_floor" ;Public domain
    "third_party/webrtc/modules/third_party/fft" ;Non-copyleft
    "third_party/webrtc/modules/third_party/g711" ;Public domain
    "third_party/webrtc/modules/third_party/g722" ;Public domain
    "third_party/webrtc/rtc_base/third_party/sigslot" ;Public domain
    "third_party/widevine/cdm/widevine_cdm_common.h" ;BSD-3
    "third_party/woff2" ;ASL2.0
    "third_party/wuffs" ;ASL2.0
    "third_party/xcbproto" ;X11
    "third_party/xdg-utils" ;Expat
    "third_party/xnnpack" ;BSD-3

    ;; These are forked components of the X11 keybinding code.
    "third_party/libxcb-keysyms" ;X11
    "third_party/libx11/src/KeyBind.c" ;X11
    "third_party/libx11/src/xkb/XKBBind.c" ;X11
    "third_party/x11proto/keysymdef.h" ;X11

    "third_party/zlib/google" ;BSD-3
    "third_party/zstd" ;BSD
    "third_party/zxcvbn-cpp" ;Expat
    "url/third_party/mozilla" ;BSD-3, MPL1.1/GPL2+/LGPL2.1+
    "v8/third_party/glibc/src/sysdeps/ieee754/dbl-64" ;LGPL2.1+
    "v8/third_party/inspector_protocol" ;BSD-3
    "v8/third_party/rapidhash-v8" ;BSD-2
    "v8/third_party/siphash" ;CC0 1.0
    "v8/third_party/utf8-decoder" ;Non-copyleft
    "v8/third_party/valgrind" ;BSD
    "v8/third_party/v8/builtins" ;PSFL2.0
    "v8/third_party/v8/codegen" ;Expat
    ))

(define %blacklisted-files
  ;; 'third_party/blink/perf_tests/resources/svg/HarveyRayner.svg' carries a
  ;; nonfree license according to LICENSES in the same directory.  As we don't
  ;; run the Blink performance tests, just remove everything to save ~70MiB.
  '("third_party/blink/perf_tests"))

(define %chromium-version "139.0.7258.154")
(define %ungoogled-revision (string-append %chromium-version "-1"))
(define %debian-revision (string-append "debian/" %ungoogled-revision))

(define %ungoogled-origin
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/ungoogled-software/ungoogled-chromium")
                        (commit %ungoogled-revision)))
    (file-name (git-file-name "ungoogled-chromium" %ungoogled-revision))
    (sha256
     (base32
      "12z1qqwqkxd34xwkfc6hc873rsj545zg4rkdsvr4nk4w9898fsqq"))))

(define %debian-origin
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://salsa.debian.org/chromium-team/chromium.git")
          (commit %debian-revision)))
    (file-name (git-file-name "debian-chromium-patches"
                              (match (string-split %debian-revision #\/)
                                ((_ version) version))))
    (sha256
     (base32
      "0zs7z0kk5ni01qgc85lwr045pmgn5sghp15scp7x79i8i8a5rfcq"))))

(define (origin-file origin file)
  (computed-file
   (basename file)
   #~(symlink (string-append #$origin "/" #$file)
              #$output)))

(define (debian-patch name)
  (origin-file %debian-origin (string-append "/debian/patches/" name)))

(define %debian-patches
  (map debian-patch
       '("bookworm/clang19.patch"
	 "bookworm/foreach.patch"
	 "disable/node-version-ck.patch"
	 "fixes/rust-clanglib.patch"
	 "system/openjpeg.patch"
	 ;; adler2 is not part of our rust toolchain, check on next version.
	 "trixie/adler1.patch"
	 "trixie/libxml-parseerr.patch")))

(define %guix-patches
  (list (local-file
	 (assume-valid-file-name
          (search-patch
           "ungoogled-chromium-extension-search-path.patch")))
	(local-file
	 (assume-valid-file-name
          (search-patch
	   "ungoogled-chromium-nullptr_t.patch")))
        (local-file
	 (assume-valid-file-name
          (search-patch
           "ungoogled-chromium-RUNPATH.patch")))
        (local-file
	 (assume-valid-file-name
          (search-patch
           "ungoogled-chromium-system-nspr.patch")))))

(define %patches
  (append %debian-patches
          %guix-patches))

;; This is a source 'snippet' that does the following:
;; *) Applies various patches for unbundling purposes and libstdc++ compatibility.
;; *) Runs the ungoogled patch-, domain substitution-, and scrubbing scripts.
;; *) Prunes all third_party directories that are not explicitly preserved.
;; *) Adjusts "GN" build files such that system libraries are preferred.
(define ungoogled-chromium-snippet
  ;; Note: delay to cope with cyclic module imports at the top level.
  (delay
    #~(begin
        (let ((chromium-dir (getcwd)))
          (set-path-environment-variable
           "PATH" '("bin")
           (list #+patch #+python-wrapper #+xz #+zstd))

          ;; Apply patches before running the ungoogled scripts because
          ;; domain substitution may break some of the patches.
          (format #t "Applying assorted build fixes...~%")
          (force-output)
          (for-each (lambda (patch)
                      (invoke "patch" "-p1" "--force" "--input"
                              patch "--no-backup-if-mismatch"))
                    '#+%patches)

          (with-directory-excursion #+%ungoogled-origin
            (format #t "Ungooglifying...~%")
            (force-output)
            (invoke "python" "utils/prune_binaries.py" chromium-dir
                    "pruning.list")
            (invoke "python" "utils/patches.py" "apply" chromium-dir
                    "patches")
            (invoke "python" "utils/domain_substitution.py" "apply" "-r"
                    "domain_regex.list" "-f" "domain_substitution.list"
                    "-c" "/tmp/domainscache.tar.gz" chromium-dir))

          ;; Run after the ungoogled scripts to avoid interfering with
          ;; patches or file lists.
          (format #t "Removing blacklisted files...~%")
          (force-output)
          (for-each delete-file-recursively '#$%blacklisted-files)

          (format #t "Pruning third party files...~%")
          (force-output)
          (apply invoke "python"
                 "build/linux/unbundle/remove_bundled_libraries.py"
                 "--do-remove" '#$%preserved-third-party-files)

          (format #t "Replacing GN files...~%")
          (force-output)
          (substitute* "tools/generate_shim_headers/generate_shim_headers.py"
            ;; The "is_official_build" configure option enables certain
            ;; release optimizations like those used in the commercial
            ;; Chrome browser.  Unfortunately it also requires using the
            ;; bundled libraries: lose that restriction.
            (("#if defined\\(OFFICIAL_BUILD\\)")
             "#if 0"))
          (invoke "python" "build/linux/unbundle/replace_gn_files.py"
                  "--system-libraries" "flac" "fontconfig" "freetype"
                  "harfbuzz-ng" "icu" "libjpeg"
                  "libpng" "libwebp" "libxml" "libxslt" "openh264" "opus"
                  "zlib")))))

(define opus+custom
  (package/inherit opus
    (name "opus+custom")
    (arguments
     (substitute-keyword-arguments (package-arguments opus)
       ((#:configure-flags flags ''())
        ;; Opus Custom is an optional extension of the Opus
        ;; specification that allows for unsupported frame
        ;; sizes.  Chromium requires that this is enabled.
        `(cons "--enable-custom-modes"
               ,flags))))))

(define lld-as-ld-wrapper-19
  (make-lld-wrapper lld-19 #:lld-as-ld? #t))

(define-public ungoogled-chromium
  (package
    (name "ungoogled-chromium")
    (version %ungoogled-revision)
    (synopsis "Graphical web browser")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://commondatastorage.googleapis.com"
                                  "/chromium-browser-official/chromium-"
                                  %chromium-version "-lite.tar.xz"))
              (sha256
               (base32
                "0bgnay6g2qbl6jxzr7kzl4nf0k2a27k0h7nhcj1if9plm611jzn9"))
              (modules '((guix build utils)))
              (snippet (force ungoogled-chromium-snippet))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:configure-flags
      ;; See tools/gn/docs/cookbook.md and
      ;; https://www.chromium.org/developers/gn-build-configuration
      ;; for usage.  Run "gn args . --list" in the Release
      ;; directory for an exhaustive list of supported flags.
      ;; (Note: The 'configure' phase will do that for you.)
      #~(list "is_debug=false"
              ;; Use the "official" release optimizations, as opposed to
              ;; a developer build.
              "is_official_build=true"
              "clang_use_chrome_plugins=false"
              "use_custom_libcxx=false" ; support for this is deprecated and to be removed.
              "optimize_webui=false"
              "safe_browsing_use_unrar=false"
              "chrome_pgo_phase=0"
              "use_sysroot=false"
              "goma_dir=\"\""
              "enable_nacl=false"
              "use_unofficial_version_number=false"
              "treat_warnings_as_errors=false"
              "use_official_google_api_keys=false"
              "disable_fieldtrial_testing_config=true"
              "safe_browsing_mode=0"
              "enable_mdns=false"
              "enable_reading_list=false"
              "enable_remoting=false"
              "enable_reporting=false"
              "enable_service_discovery=false"
              "enable_vr=false"
              "enable_enterprise_companion=false"
              "enable_glic=false"
              "enable_iterator_debugging=false"
              "enable_hangout_services_extension=false"
              "enable_widevine=false"
              ;; Disable type-checking for the Web UI to avoid a Java dependency.
              "enable_js_type_check=false"
              ;; Disable code using TensorFlow until it has been scrutinized
              ;; by the ungoogled project.
              "build_with_tflite_lib=false"
              ;; Avoid dependency on code formatting tools.
              "blink_enable_generated_code_formatting=false"
              ;; Don't bother building Dawn tests.
              "build_dawn_tests=false"

              "angle_has_histograms=false"
              "angle_build_tests=false"
              "build_angle_perftests=false"

              "clang_warning_suppression_file=\"\""

              "use_qt5=false"
              "use_qt6=false"
              "is_cfi=false"

              "symbol_level=0"
              "v8_enable_backtrace=false"

              ;; Define a custom toolchain that simply looks up CC, AR and
              ;; friends from the environment.
              "custom_toolchain=\"//build/toolchain/linux/unbundle:default\""
              "host_toolchain=\"//build/toolchain/linux/unbundle:default\""

              (string-append "clang_base_path=\""
                             (dirname (dirname (search-input-file %build-inputs
                                                                  "/bin/clang")))
                             "\"")

              (string-append "clang_version="
			     #$(version-major (package-version clang-19)))

              (string-append "rust_sysroot_absolute=\""
                             (dirname (dirname (search-input-file %build-inputs
                                                                  "/bin/rustc")))
                             "\"")
              (string-append "rust_bindgen_root=\""
                             (dirname (dirname (search-input-file %build-inputs
                                                                  "/bin/bindgen")))
                             "\"")

              (string-append "rustc_version=\"rustc "
			     #$(package-version rust)
			     ;; These are 'don't care', but chromium's build parses
			     ;; the whole string as if returned from 'rustc --version'
			     ;; invokation and fails if it's just the version number.
			     " (f6e511eec 2024-10-15) (built from a source tarball)\"")

              ;; Prefer system libraries.
              "use_system_freetype=true"
              "use_system_harfbuzz=true"
              "use_system_lcms2=true"
              "use_system_libffi=true"
              "use_system_libjpeg=true"
              "use_system_libopenjpeg2=true"
              "use_system_libpng=true"

              "use_system_zlib=true"
              "use_gnome_keyring=false" ;deprecated by libsecret
              "use_pulseaudio=true"
              "link_pulseaudio=true"
              "icu_use_data_file=false"

              ;; VA-API acceleration is currently only supported on x86_64-linux.
              #$@(if (string-prefix? "x86_64" (or (%current-target-system)
                                                  (%current-system)))
                     '("use_vaapi=true")
                     '())

              ;; Do not artificially restrict formats supported by system ffmpeg.
              "proprietary_codecs=true"
              "ffmpeg_branding=\"Chrome\""

              "rtc_use_pipewire=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-stuff
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let* ((libopenjp2 (search-input-file inputs "lib/libopenjp2.so"))
                     (openjpeg (dirname (dirname libopenjp2)))
                     (compiler-rt-path (dirname
					(dirname
					 (search-input-file (or native-inputs inputs)
							    "/bin/hwasan_symbolize")))))
		;; This works on top of debian's 'rust-clanglib.patch'.
		(substitute* "build/config/clang/BUILD.gn"
		  (("\\$clang_base_path/lib/clang/\\$clang_version")
		   compiler-rt-path))

                (substitute* "third_party/pdfium/BUILD.gn"
                  ;; This include path is added by Debians openjpeg patch.
                  (("/usr/include/openjpeg-")
                   (string-append openjpeg "/include/openjpeg-")))

                (substitute*
                    (append
                     '("media/audio/audio_opus_encoder.h")
                     (find-files (string-append "third_party/webrtc/modules"
                                                "/audio_coding/codecs/opus")))
                  ;; Fix opus include path.
                  ;; Do not substitute opus_private.h.
                  (("#include \"opus\\.h\"")
                   "#include \"opus/opus.h\"")
                  (("#include \"opus_custom\\.h\"")
                   "#include \"opus/opus_custom.h\"")
                  (("#include \"opus_defines\\.h\"")
                   "#include \"opus/opus_defines.h\"")
                  (("#include \"opus_multistream\\.h\"")
                   "#include \"opus/opus_multistream.h\"")
                  (("#include \"opus_types\\.h\"")
                   "#include \"opus/opus_types.h\"")
                  (("\"third_party/opus/src/include/([a-z_-]+\\.h)\"" _ header)
                   (string-append "<opus/" header ">")))

                ;; Many files try to include ICU headers from "third_party/icu/...".
                ;; Remove the "third_party/" prefix to use system headers instead.
                (substitute* (find-files "chrome" "\\.cc$")
                  (("third_party/icu/source/(common|i18n)/")
                   ""))

                ;; XXX: Should be unnecessary when use_system_lcms2=true.
                (substitute* "third_party/pdfium/core/fxcodec/icc/icc_transform.h"
                  (("include \"third_party/lcms/include/lcms2\\.h\"")
                   "include \"lcms2.h\""))

                ;; The unbundling script leaves behind an empty pyyaml directory
                ;; which prevents the code that tries to use it from falling
                ;; back to the pyyaml provided by Guix.
                (delete-file-recursively "third_party/pyyaml")

                (substitute*
                    "third_party/breakpad/breakpad/src/common/linux/libcurl_wrapper.h"
                  (("include \"third_party/curl")
                   "include \"curl"))

                ;; Unknown flags as of clang-19, can be removed when building with
		;; newer clang.
                (substitute* "build/config/compiler/BUILD.gn"
                  (("-gsimple-template-names") ""))

                ;; Clang libs are not under bindgen root directory.
                (substitute* "build/rust/rust_bindgen_generator.gni"
                  (("(_libclang_path = )rust_bindgen_root" _ libclang)
                   (string-append libclang "clang_base_path"))))))
          (add-after 'patch-stuff 'add-absolute-references
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((cups-config (search-input-file inputs "/bin/cups-config"))
                    (libnssckbi.so (search-input-file inputs
                                                      "/lib/nss/libnssckbi.so"))
                    (gtk-libs '("libgio-2.0.so.0"
                                "libgdk_pixbuf-2.0.so.0"
                                "libgdk-3.so.0"
                                "libgtk-3.so.0")))
                (for-each (lambda (lib)
                            (substitute* "ui/gtk/gtk_compat.cc"
                              ((lib) (search-input-file
                                      inputs (string-append "lib/" lib)))))
                          gtk-libs)
                (substitute* "printing/cups_config_helper.py"
                  (("cups_config =.*")
                   (string-append "cups_config = '" cups-config "'\n")))
                (substitute* "crypto/nss_util.cc"
                  (("libnssckbi\\.so") libnssckbi.so)))))
          (add-after 'add-absolute-references 'adjust-CPLUS_INCLUDE_PATH
            ;; The libcxx include/c++/v1 directory is not exposed via
            ;; CPLUS_INCLUDE_PATH by default, causing errors like
            ;; "fatal error: 'format' file not found".
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (setenv "CPLUS_INCLUDE_PATH"
                      (string-append
                       (search-input-directory inputs
                                               "/include/c++/v1")
                       ":" (getenv "CPLUS_INCLUDE_PATH")))))
          (add-before 'configure 'prepare-build-environment
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (let ((node (search-input-file (or native-inputs inputs)
                                             "/bin/node")))
                ;; Define the GN toolchain.
                (setenv "AR" "llvm-ar") (setenv "NM" "llvm-nm")
                (setenv "CC" "clang") (setenv "CXX" "clang++")

                (setenv "CXXFLAGS"
                        (string-append " -Wno-conversion"
                                       " -Wno-unused-function"
                                       " -Wno-unused-variable"
                                       " -Wno-unused-private-field"
                                       " -Wno-deprecated-declarations"
                                       " -Wno-unknown-pragmas"
                                       " -fno-delete-null-pointer-checks"
                                       " -stdlib=libc++"))
                (setenv "LDFLAGS"
                        (string-append " -stdlib=libc++"
                                       " -Wl,--stats"))
                (setenv "CLANG_MVERS" #$(version-major
					 (package-version clang-19)))
                (setenv "RUSTC_BOOTSTRAP" "1")

                ;; TODO: pre-compile instead. Avoids a race condition.
                (setenv "PYTHONDONTWRITEBYTECODE" "1")

                ;; XXX: How portable is this.
                (mkdir-p "third_party/node/linux/node-linux-x64")
                (symlink (dirname node)
                         "third_party/node/linux/node-linux-x64/bin"))))
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (let ((args (string-join configure-flags " ")))
                ;; Generate ninja build files.
                (invoke "gn" "gen" "out/Release"
                        (string-append "--args=" args))

                ;; Print the full list of supported arguments as well as
                ;; their current status for convenience.
                (format #t "Dumping configure flags...\n")
                (invoke "gn" "args" "out/Release" "--list"))))
          (add-before 'build 'increase-resource-limits
            (lambda _
              ;; XXX: Chromiums linking step requires a lot of simultaneous file
              ;; accesses.  Having a too low ulimit will result in bogus linker
              ;; errors such as "foo.a: error adding symbols: malformed archive".

              ;; Try increasing the soft resource limit of max open files to 2048,
              ;; or equal to the hard limit, whichever is lower.
              (call-with-values (lambda () (getrlimit 'nofile))
                (lambda (soft hard)
                  (when (and soft (< soft 2048))
                    (if hard
                        (setrlimit 'nofile (min hard 2048) hard)
                        (setrlimit 'nofile 2048 #f))
                    (format #t
                            "increased maximum number of open files from ~d to ~d~%"
                            soft (if hard (min hard 2048) 2048)))))))
          (replace 'build
            (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
              (invoke "ninja" "-C" "out/Release"
                      "-j" (if parallel-build?
                               (number->string (parallel-job-count))
                               "1")
                      "chrome"
                      "chromedriver")))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out            (assoc-ref outputs "out"))
                     (bin            (string-append out "/bin"))
                     (exe            (string-append bin "/chromium"))
                     (lib            (string-append out "/lib"))
                     (man            (string-append out "/share/man/man1"))
                     (applications   (string-append out "/share/applications"))
                     (libs           '("chrome_100_percent.pak"
                                       "chrome_200_percent.pak"
                                       "resources.pak"
                                       "v8_context_snapshot.bin"
                                       "chrome_crashpad_handler"

                                       ;; Swiftshader ICD.
                                       "libvk_swiftshader.so"
                                       "vk_swiftshader_icd.json"

                                       ;; Chromium ships its own libGL
                                       ;; implementation called ANGLE.
                                       "libEGL.so" "libGLESv2.so"))
                     (locales        (string-append lib "/locales"))
                     (resources      (string-append lib "/resources"))
                     (preferences
                      ;; This file contains defaults for new user profiles.
                      #$(local-file
                         (search-auxiliary-file
                          "chromium/master-preferences.json")))
                     (gtk (dirname (dirname
                                    (search-input-file inputs "lib/libgtk-3.so"))))
                     (mesa (dirname (search-input-file inputs "lib/libGL.so")))
                     (xdg-utils (dirname (search-input-file inputs "bin/xdg-open"))))

                (substitute* '("chrome/app/resources/manpage.1.in"
                               "chrome/installer/linux/common/desktop.template")
                  (("@@MENUNAME@@") "Chromium")
                  (("@@PACKAGE@@") "chromium")
                  (("/usr/bin/@@USR_BIN_SYMLINK_NAME@@") exe))

                (mkdir-p man)
                (copy-file "chrome/app/resources/manpage.1.in"
                           (string-append man "/chromium.1"))

                (mkdir-p applications)
                (copy-file "chrome/installer/linux/common/desktop.template"
                           (string-append applications "/chromium.desktop"))

                (mkdir-p lib)
                (copy-file preferences (string-append lib "/master_preferences"))

                (with-directory-excursion "out/Release"
                  (for-each (cut install-file <> lib) libs)
                  (copy-file "chrome" (string-append lib "/chromium"))

                  (copy-recursively "locales" locales)
                  (copy-recursively "resources" resources)

                  (mkdir-p bin)
                  (symlink "../lib/chromium" exe)
                  (install-file "chromedriver" bin)

                  (for-each (lambda (so)
                              (install-file so (string-append lib "/swiftshader")))
                            (find-files "swiftshader" "\\.so$"))

                  (wrap-program exe
                    ;; Avoid file manager crash.  See <https://bugs.gnu.org/26593>.
                    `("XDG_DATA_DIRS" ":" prefix (,(string-append gtk "/share")))
                    ;; Provide libGL and libvulkan without patching all references.
                    ;; XXX: How to add on RUNPATH instead of this hack?
                    `("LD_LIBRARY_PATH" ":" prefix
                      (,mesa))
                    ;; Ensure xdg-open et al. is found.
                    `("PATH" ":" prefix (,xdg-utils))))

                (with-directory-excursion "chrome/app/theme/chromium"
                  (for-each
                   (lambda (size)
                     (let ((icons (string-append out "/share/icons/hicolor/"
                                                 size "x" size "/apps")))
                       (mkdir-p icons)
                       (copy-file (string-append "product_logo_" size ".png")
                                  (string-append icons "/chromium.png"))))
                   '("24" "48" "64" "128" "256")))))))))
    (native-inputs
     (list bison
           clang-19
           clang-runtime-19
           clang-toolchain-19
           gn
           gperf
           lld-as-ld-wrapper-19
           llvm-19
           ninja
           node-lts
           pkg-config
           which
           python-beautifulsoup4
           python-html5lib
           python-pyyaml
           python-wrapper
           rust
           rust-bindgen-cli
           rust-cbindgen
           wayland))
    (inputs
     (list alsa-lib
           at-spi2-core
           bash-minimal
           cups
           curl
           dbus
           expat
           flac
           fontconfig
           fp16
           freetype
           fxdiv
           gdk-pixbuf
           glib
           gtk+
           harfbuzz
           icu4c
           lcms
           libcxx
           libffi
           libjpeg-turbo
           libpng
           libva
           libvpx
           libwebp
           libx11
           libxcb
           libxcomposite
           libxcursor
           libxdamage
           libxext
           libxfixes
           libxi
           libxml2
           libxrandr
           libxrender
           libxscrnsaver
           libxslt
           libxtst
           mesa
           minizip
           mit-krb5
           nss
           openh264
           openjpeg                     ;PDFium only
           opus+custom
           pango
           pciutils
           pipewire
           pulseaudio
           qtbase
           snappy
           speech-dispatcher
           eudev
           valgrind/pinned
           wayland
           xdg-utils))
    (native-search-paths
     (list (search-path-specification
            (variable "CHROMIUM_EXTENSION_DIRECTORY")
            (separator #f)              ;single entry
            (files '("share/chromium/extensions")))))

    ;; Building Chromium takes ... a very long time.  On a single core, a busy
    ;; mid-end x86 system may need more than 24 hours to complete the build.
    (properties '((timeout . 144000)    ;40 hours
                  ;; The linking step may take more than an hour on some hardware.
                  (max-silent-time . 7200)
                  (cpe-name . "chrome")))

    (home-page "https://ungoogled-software.github.io/")
    (description
     "Ungoogled-Chromium is the Chromium web browser, with some functionality
disabled in order to protect the users privacy.  This package also includes
the @command{chromedriver} command, which can be useful for automated web
testing.")
    ;; Chromium is developed as BSD-3, but bundles a large number of third-party
    ;; components with other licenses.  For full information, see chrome://credits.
    (license (list license:bsd-3
                   license:bsd-2
                   license:expat
                   license:asl2.0
                   license:mpl1.1
                   license:mpl2.0
                   license:public-domain
                   license:isc
                   (license:non-copyleft "chrome://credits"
                                         "See chrome://credits for more information.")
                   license:lgpl2.1+))))

(define-public ungoogled-chromium/wayland
  (package
    (inherit ungoogled-chromium)
    (name "ungoogled-chromium-wayland")
    (native-inputs '())
    (inputs
     (list bash-minimal (libc-utf8-locales-for-target) ungoogled-chromium))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bash #$(this-package-input "bash-minimal"))
                 (chromium #$(this-package-input "ungoogled-chromium"))
                 (locales #$(this-package-input "glibc-utf8-locales"))
                 (exe (string-append #$output "/bin/chromium")))

            ;; Use a Unicode locale so we can substitute the file below.
            (setenv "GUIX_LOCPATH" (string-append locales "/lib/locale"))
            (setlocale LC_ALL "en_US.utf8")

            (mkdir-p (dirname exe))
            (symlink (string-append chromium "/bin/chromedriver")
                     (string-append #$output "/bin/chromedriver"))

            (call-with-output-file exe
              (lambda (port)
                (format port "#!~a
exec ~a --enable-features=UseOzonePlatform --ozone-platform=wayland \
--enable-features=WebRTCPipeWireCapturer $@"
                        (string-append bash "/bin/bash")
                        (string-append chromium "/bin/chromium"))))
            (chmod exe #o555)

            ;; Provide the manual and .desktop file.
            (copy-recursively (string-append chromium "/share")
                              (string-append #$output "/share"))
            (substitute* (string-append
                          #$output "/share/applications/chromium.desktop")
              ((chromium) #$output))))))))
