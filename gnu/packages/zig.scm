;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Calum Irwin <calumirwin1@gmail.com>
;;; Copyright © 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023, 2024 Hilton Chain <hako@ultrarare.space>
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

(define-module (gnu packages zig)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages llvm-meta))

(define (zig-source version commit hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ziglang/zig")
          (commit commit)))
    (file-name (git-file-name "zig" version))
    (sha256 (base32 hash))
    (modules '((guix build utils)))
    (snippet
     #~(for-each
        (lambda (file)
          (when (file-exists? file)
            (delete-file file)))
        (append
         '("stage1/zig1.wasm"
           "stage1/zig1.wasm.zst")
         ;; Generated from glibc sources, see also:
         ;; https://github.com/ziglang/zig/blob/master/lib/libc/glibc/README.md
         ;; https://github.com/ziglang/glibc-abi-tool
         '("lib/libc/glibc/abilists")
         ;; IETF RFC documents have nonfree license.
         (find-files "." "^rfc[0-9]+\\.txt"))))))

(define zig-0.9-glibc-abi-tool
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ziglang/glibc-abi-tool")
          (commit "6f992064f821c612f68806422b2780c9260cbc4c")))
    (file-name "glibc-abi-tool")
    (sha256
     (base32 "0lsi3f2lkixcdidljby73by2sypywb813yqdapy9md4bi2h8hhgp"))))

(define-public zig-0.9
  (package
    (name "zig")
    (version "0.9.1")
    (source
     (origin
       (inherit (zig-source
                 version version
                 "0nfvgg23sw50ksy0z0ml6lkdsvmd0278mq29m23dbb2jsirkhry7"))
       (patches
        (search-patches
         "zig-0.9-riscv-support.patch"
         "zig-0.9-use-baseline-cpu-by-default.patch"
         "zig-0.9-use-system-paths.patch"
         "zig-0.9-fix-runpath.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list #$@(if (%current-target-system)
                     (list (string-append "-DZIG_TARGET_TRIPLE="
                                          (%current-target-system)))
                     '()))
      #:out-of-source? #f         ; for tests
      ;; There are too many unclear test failures.
      #:tests? (not (or (target-riscv64?)
                        (%current-target-system)))
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (target-riscv64?)
                 ;; It is unclear why all these tests fail to build.
                 `((add-after 'unpack 'adjust-tests
                     (lambda _
                       (substitute* "build.zig"
                         ((".*addRuntimeSafetyTests.*") "")
                         ((".*addRunTranslatedCTests.*") ""))
                       (substitute* "test/standalone.zig"
                         ;; These tests fail to build on riscv64-linux.
                         ;; They both contain 'exe.linkSystemLibrary("c");'
                         ((".*shared_library.*") "")
                         ((".*mix_o_files.*") "")
                         ;; ld.lld: error: undefined symbol: __tls_get_addr
                         ;; Is this symbol x86 only in glibc?
                         ((".*link_static_lib_as_system_lib.*") "")))))
                 '())
          (add-after 'configure 'set-cache-dir
            (lambda _
              ;; Set cache dir, otherwise Zig looks for `$HOME/.cache'.
              (setenv "ZIG_GLOBAL_CACHE_DIR"
                      (string-append (getcwd) "/zig-cache"))))
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke (string-append #$output "/bin/zig")
                        ;; Testing the standard library takes >7.5GB RAM, and
                        ;; will fail if it is OOM-killed.  The 'test-toolchain'
                        ;; target skips standard library and doc tests.
                        "build" "test-toolchain"
                        ;; Stage 2 is experimental, not what we run with `zig',

                        "-Dskip-stage2-tests"
                        ;; Non-native tests try to link and execute non-native
                        ;; binaries.
                        "-Dskip-non-native"))))
          (add-before 'check 'install-glibc-abilists
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (mkdir-p "/tmp/glibc-abi-tool")
              (with-directory-excursion "/tmp/glibc-abi-tool"
                (copy-recursively
                 (dirname (search-input-file
                           (or native-inputs inputs) "consolidate.zig"))
                 ".")
                (for-each make-file-writable (find-files "."))
                (invoke (string-append #$output "/bin/zig")
                        "run" "consolidate.zig")
                (install-file
                 "abilists"
                 (string-append #$output "/lib/zig/libc/glibc"))))))))
    (inputs
     (list clang-13                     ;Clang propagates llvm.
           lld-13))
    ;; Zig compiles fine with GCC, but also needs native LLVM libraries.
    (native-inputs
     (list llvm-13
           zig-0.9-glibc-abi-tool))
    (native-search-paths
     (list
      (search-path-specification
       (variable "C_INCLUDE_PATH")
       (files '("include")))
      (search-path-specification
       (variable "CPLUS_INCLUDE_PATH")
       (files '("include/c++" "include")))
      (search-path-specification
       (variable "LIBRARY_PATH")
       (files '("lib" "lib64")))))
    (synopsis "General purpose programming language and toolchain")
    (description "Zig is a general-purpose programming language and
toolchain.  Among other features it provides
@itemize
@item an Optional type instead of null pointers,
@item manual memory management,
@item generic data structures and functions,
@item compile-time reflection and compile-time code execution,
@item integration with C using zig as a C compiler, and
@item concurrency via async functions.
@end itemize")
    (home-page "https://github.com/ziglang/zig")
    ;; Currently building zig can take up to 10GB of RAM for linking stage1:
    ;; https://github.com/ziglang/zig/issues/6485
    (supported-systems %64bit-supported-systems)
    ;; Stage3 can take a lot of time and isn't verbose.
    (properties `((max-silent-time . 9600)
                  ,@(clang-compiler-cpu-architectures "13")))
    (license license:expat)))

(define zig-0.10-glibc-abi-tool
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ziglang/glibc-abi-tool")
          (commit "b07bf67ab3c15881f13b9c3c03bcec04535760bb")))
    (file-name "glibc-abi-tool")
    (sha256
     (base32 "0csn3c9pj8wchwy5sk5lfnhjn8a3c8cp45fv7mkpi5bqxzdzf1na"))
    (modules '((guix build utils)))
    (snippet
     #~(substitute* "consolidate.zig"
         (("(@ctz.)u.., " _ prefix) prefix)
         (("(@popCount.)u.., " _ prefix) prefix)))))

(define-public zig-0.10
  (package
    (inherit zig-0.9)
    (name "zig")
    (version "0.10.1")
    (source
     (origin
       (inherit (zig-source
                 version version
                 "1sh5xjsksl52i4cfv1qj36sz5h0ln7cq4pdhgs3960mk8a90im7b"))
       (patches
        (search-patches
         "zig-0.9-use-baseline-cpu-by-default.patch"
         "zig-0.10-use-system-paths.patch"
         "zig-0.10-fix-runpath.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments zig-0.9)
       ((#:configure-flags flags ''())
        #~(cons* "-DZIG_TARGET_MCPU=baseline"
                 "-DZIG_SHARED_LLVM=ON"
                 (string-append "-DZIG_LIB_DIR=" #$output "/lib/zig")
                 #$flags))
       ((#:tests? _ #t) #t)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            #$@(if (target-riscv64?)
                   `((delete 'adjust-tests))
                   '())
            (add-after 'unpack 'set-CC
              (lambda _
                ;; Set CC, since the stage 2 zig relies on it to find the libc
                ;; installation, and otherwise silently links against its own.
                (setenv "CC" #$(cc-for-target))))
            (add-after 'patch-source-shebangs 'patch-more-shebangs
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Zig uses information about an ELF file to determine the
                ;; version of glibc and other data for native builds.
                (substitute* "lib/std/zig/system/NativeTargetInfo.zig"
                  (("/usr/bin/env") (search-input-file inputs "bin/clang++")))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke (string-append #$output "/bin/zig")
                          "build" "test"
                          ;; We're not testing the compiler bootstrap chain.
                          "-Dskip-stage1"
                          "-Dskip-stage2-tests"
                          ;; Non-native tests try to link and execute non-native
                          ;; binaries.
                          "-Dskip-non-native"))))))))
    (inputs
     (modify-inputs (package-inputs zig-0.9)
       (prepend zlib `(,zstd "lib"))
       (replace "clang" clang-15)
       (replace "lld" lld-15)))
    (native-inputs
     (modify-inputs (package-native-inputs zig-0.9)
       (replace "glibc-abi-tool" zig-0.10-glibc-abi-tool)
       (replace "llvm" llvm-15)))
    (properties `((max-silent-time . 9600)
                  ,@(clang-compiler-cpu-architectures "15")))))

(define-public zig zig-0.10)
