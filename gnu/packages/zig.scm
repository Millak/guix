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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages web))

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
       ((#:tests? _ #t)
        (not (%current-target-system)))
       ((#:configure-flags flags ''())
        #~(cons* "-DZIG_TARGET_MCPU=baseline"
                 "-DZIG_SHARED_LLVM=ON"
                 (string-append "-DZIG_LIB_DIR=" #$output "/lib/zig")
                 #$flags))
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


;;;
;;; Bootstrap path for Zig 0.11.
;;; See also: <https://git.jakstys.lt/motiejus/zig-repro>.
;;;

;; Restore C++ stage 1 and build the initial zig1.wasm.
(define zig-0.10.0-538-source
  (let ((commit "bf316e550671cc71eb498b3cf799493627bb0fdc")
        (revision "538"))
    (zig-source
     (git-version "0.10.0" revision commit)
     commit "1dchc2bp842jlw0byssqzindv8cigpqcj2hk3752667jrrww13vv")))

(define zig-0.10.0-539-patch
  (let ((commit "28514476ef8c824c3d189d98f23d0f8d23e496ea"))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/ziglang/zig/commit/" commit ".patch"))
      (file-name "zig-0.10.0-539.patch")
      (sha256
       (base32 "0qxxiafg2sd5rr4xhw0c12rygd7zh1rmf3x8hfialyxmsbi5pfxp")))))

(define zig-0.10.0-542-patch
  (let ((commit "3ba916584db5485c38ebf2390e8d22bc6d81bf8e"))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/ziglang/zig/commit/" commit ".patch"))
      (file-name "zig-0.10.0-542.patch")
      (sha256
       (base32 "1l09gmbr3vqzinb63kvaskgs1d0mvm1m7w3ai3ngwg5zlabyya35")))))

(define zig-0.10.0-610
  (let ((commit "e7d28344fa3ee81d6ad7ca5ce1f83d50d8502118")
        (revision "610")
        (base zig-0.10))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "08pm3f4hh6djl3szhqgm7fa3qisdl2xh9jrp18m0z7bk2vd0bzw7"))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ;; Patch for fixing RUNPATH not applied to intermediate versions.
         ((#:validate-runpath? _ #t) #f)
         ;; Disable tests for intermediate versions.
         ((#:tests? _ #t) #f)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'backup-source
                (lambda _
                  (copy-recursively "." "../source-backup")))
              (add-after 'backup-source 'prepare-source
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  ;; Revert "actually remove stage1".
                  (invoke "patch" "--reverse" "--strip=1"
                          "--input" #+zig-0.10.0-542-patch)
                  ;; Revert "remove `-fstage1` option".
                  (false-if-exception
                   (invoke "patch" "--reverse" "--strip=1"
                           "--input" #+zig-0.10.0-539-patch))
                  ;; Resolve conflicts in previous patching.
                  (invoke
                   "patch" "--forward" "--strip=1" "--input"
                   #+(local-file
                      (search-patch
                       "zig-0.10.0-610-bootstrap-resolve-conflicts.patch")))
                  ;; Restore build system.
                  (rename-file "stage1/config.zig.in" "src/config.zig.in")
                  (substitute* "src/config.zig.in"
                    (("(have_stage1 = )false" _ prefix)
                     (string-append prefix "true")))
                  (for-each
                   (lambda (file)
                     (copy-file (in-vicinity #+zig-0.10.0-538-source file)
                                file))
                   '("build.zig" "CMakeLists.txt"))))
              (add-after 'install 'restore-source
                (lambda _
                  (for-each delete-file-recursively (find-files "."))
                  (copy-recursively "../source-backup" ".")))
              (add-after 'restore-source 'build-zig1
                (lambda _
                  (invoke (string-append #$output "/bin/zig")
                          "build" "update-zig1" "--verbose")))
              (add-after 'build-zig1 'install-zig1
                (lambda _
                  (install-file "stage1/zig1.wasm.zst"
                                (string-append #$output:zig1 "/bin"))))
              (delete 'install-glibc-abilists)))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend binaryen)
         (delete "glibc-abi-tool")))
      (outputs '("out" "zig1")))))

;; Supply zig1.wasm.zst, build zig2 + zig1.wasm, install zig2 + zig1.wasm.zst.
(define zig-0.10.0-675
  (let ((commit "9d93b2ccf11f584320a2c5209dd2d94705167695")
        (revision "675")
        (base zig-0.10.0-610))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source
       (origin
         (inherit (zig-source
                   version commit
                   "1qsfsv8wg0kz616sgj7dw9ihdz5rsm80p3ambl5lnkrjhwym7z7x"))
         (patches (search-patches "zig-0.10.0-675-TypeOf-hack.patch"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'prepare-source
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  (install-file (search-input-file
                                 (or native-inputs inputs) "bin/zig1.wasm.zst")
                                "stage1")))
              (add-after 'prepare-source 'remove-stage3
                (lambda _
                  ;; Multiline substitution.
                  (invoke
                   "sed" "--in-place" "/^add_custom_target(stage3/,/^)$/d"
                   "CMakeLists.txt")))
              (replace 'install
                (lambda _
                  (install-file "zig2" (string-append #$output "/bin"))
                  (mkdir-p (string-append #$output "/lib"))
                  (copy-recursively "lib" (string-append #$output "/lib/zig"))))
              (replace 'build-zig1
                (lambda _
                  (invoke "./zig2" "build" "update-zig1" "--verbose")))
              (delete 'patch-more-shebangs)
              (delete 'backup-source)
              (delete 'restore-source)))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend `(,base "zig1")))))))

;; Supply zig1.wasm.zst, build zig2 + zig1.wasm, install zig2 + zig1.wasm.
(define zig-0.10.0-722
  (let ((commit "d10fd78d4615f329141f5c19f893039d56aff425")
        (revision "722")
        (base zig-0.10.0-675))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0829wymcwph71zlwql6v7i7j9gr1m96acyp2xsr69vq2h98wmlap"))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'prepare-source
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  (install-file (search-input-file
                                 (or native-inputs inputs) "bin/zig1.wasm.zst")
                                "stage1")
                  (invoke "zstd" "-d" "stage1/zig1.wasm.zst")
                  (make-file-writable "stage1/zig1.wasm")))
              (replace 'install-zig1
                (lambda _
                  (install-file "stage1/zig1.wasm"
                                (string-append #$output:zig1 "/bin"))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend zstd)
         (replace "zig" `(,base "zig1")))))))

;; Supply zig2, build zig1.wasm, install zig1.wasm.
(define zig-0.10.0-747
  (let ((commit "7b2a936173165002105ba5e76bed69654e132fea")
        (revision "747")
        (base zig-0.10.0-722))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source
       (origin
         (inherit (zig-source
                   version commit
                   "1z5ndywk4d1dcv2k3bw3n2zgjr3ysf3bi2ac4jhwqgnmzsw498wd"))
         (patches (search-patches "zig-0.10.0-747-CallOptions.patch"))))
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-722)
         ;; zig1.wasm is architecture-independent.
         ((#:target _ #f) #f)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'build-zig1
                (lambda _
                  (invoke "zig2" "build" "--zig-lib-dir" "lib"
                          "update-zig1" "--verbose")))
              (delete 'prepare-source)
              (delete 'configure)
              (delete 'build)
              (delete 'install)))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out"))
         (delete "zstd"))))))

;; Supply zig1.wasm, build zig2 + zig1.wasm, install zig1.wasm.
(define zig-0.10.0-748
  (let ((commit "08b2d491bcd8c79c68495267cc71967661caea1e")
        (revision "748")
        (base zig-0.10.0-747))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "1iv1wjgj0nfbb19sp3zw4d8hmrhkah4cmklzxm8c32zsg673kv3i"))
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-722)
         ;; zig1.wasm is architecture-independent.
         ((#:target _ #f) #f)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'prepare-source
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  (install-file (search-input-file
                                 (or native-inputs inputs) "bin/zig1.wasm")
                                "stage1")
                  (make-file-writable "stage1/zig1.wasm")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

;; Supply zig1.wasm, build zig2, install zig2.
(define zig-0.10.0-851
  (let ((commit "aac2d6b56f32134ea32fb3d984e3fcdfddd8aaf6")
        (revision "851")
        (base zig-0.10.0-748))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "026q8igib5a2wiqdxispijph7isx8g1m0p6xgclikrmwpkpr7wb8"))
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-748)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (delete 'build-zig1)
              (delete 'install-zig1)))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-853
  (let ((commit "2a5e1426aa9469fadb78e837d0100d689213b034")
        (revision "853")
        (base zig-0.10.0-851))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "18lqcf3vg6yi70hk6nzyv8mzw7rlhybawspk5z9s281bqv210v5s"))
      (arguments
       ;; zig1
       (substitute-keyword-arguments (package-arguments zig-0.10.0-747)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'build-zig1
                (lambda _
                  (invoke "zig2" "build" "update-zig1" "--verbose")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-961
  (let ((commit "54160e7f6aecb4628df633ceaef4c6d956429a3d")
        (revision "961")
        (base zig-0.10.0-853))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0f0n2bkygj8zxri275nisia3pdv2s4fikgnsnmag42bs747zn8bz"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-962
  (let ((commit "622311fb9ac7ee6d93dcb8cda4b608751f7e092a")
        (revision "962")
        (base zig-0.10.0-961))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "1yywzmxr6nwhdix5x9k5nrxa4n1nc8x8v1gqgyvwdhz47x9vw8b0"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-1027
  (let ((commit "a43fdc1620fa24c8c606f748505766bfd53d1049")
        (revision "1027")
        (base zig-0.10.0-962))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0w19qlzb2la5bnjalmv7n05m08lhz4x8c53hf34aqw66kagw47kj"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

;; Supply zig2, build zig1.wasm + zig2, install zig2.
(define zig-0.10.0-1073
  (let ((commit "4c1007fc044689b8cbc20634d73debb43df8efe1")
        (revision "1073")
        (base zig-0.10.0-1027))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "1mgvi3m2aph10c1ij9b4k6xs3jbp8hbswqgdnzxdi5y0ak7h1pd4"))
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-851)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-before 'build 'build-zig1
                (lambda _
                  (invoke "zig2" "build" "--zig-lib-dir" "lib"
                          "update-zig1" "--verbose")))
              (delete 'prepare-source)))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-1497
  (let ((commit "a9b68308b9eeb494524e2b7ab0d63cfa6b623cd0")
        (revision "1497")
        (base zig-0.10.0-1073))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0ja2555h41kibkxyyjpzrp5rradm3bknxhaspzz3brcbc6xvac21"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-1505-source
  (let ((commit "fe4ea31f7e9e1c8caea6a1df107b91e8ea1a7b8a")
        (revision "1505"))
    (zig-source
     (git-version "0.10.0" revision commit)
     commit "0q91hz824l867dlzz885i4mlkjdr0v5nfk3drsnvkvz2q52r0ffx")))

(define zig-0.10.0-1506
  (let ((commit "f16c10a86b7183e99e54a70344f4681211cd52bb")
        (revision "1506")
        (base zig-0.10.0-1497))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "17qbwknv33xi8908f1kdapvvj331bmibvvjhsza04j3siq3rpbz7"))
      ;; zig2+zig1
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-748)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'prepare-source 'restore-lib
                (lambda _
                  (delete-file-recursively "lib")
                  (copy-recursively
                   (string-append #+zig-0.10.0-1505-source "/lib")
                   "lib")))
              (replace 'build-zig1
                (lambda _
                  (invoke "./zig2" "build" "--zig-lib-dir" "lib"
                          "update-zig1" "--verbose")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-1637-source
  (let ((commit "4e6f21e2cb2c557b5c019f4acf445665a26edcba")
        (revision "1637"))
    (zig-source
     (git-version "0.10.0" revision commit)
     commit "1nd55j2c0br7rqx9fj6bkjyymkf1k6ms2m9f7byrc1ahggdyxrpv")))

(define zig-0.10.0-1638
  (let ((commit "7199d7c77715fe06606c5c89595e6852b3fa8c20")
        (revision "1638")
        (base zig-0.10.0-1506))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source
       (origin
         (inherit (zig-source
                   version commit
                   "15mvfgab9wglm1g5gakvqwb7l33v1vjwjwchb3pmhxmkxkrjkmgq"))
         (patches (search-patches "zig-0.10.0-1638-re-add-qualCast.patch"))))
      ;; zig2+zig1
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-748)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'prepare-source 'restore-lib
                (lambda _
                  (delete-file-recursively "lib")
                  (copy-recursively
                   (string-append #+zig-0.10.0-1637-source "/lib")
                   "lib")))
              (replace 'build-zig1
                (lambda _
                  (invoke "./zig2" "build" "--zig-lib-dir" "lib"
                          "update-zig1" "--verbose")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-1657
  (let ((commit "321ccbdc525ab0f5862e42378b962c10ec54e4a1")
        (revision "1657")
        (base zig-0.10.0-1638))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0nv16z8fm1ihszlwvdncnza0pgykj1ca87pf1w8nr8prqhsc7kj7"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-1681
  (let ((commit "0bb178bbb2451238a326c6e916ecf38fbc34cab1")
        (revision "1681")
        (base zig-0.10.0-1657))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0kg1wnxxhjgd8fszbch039ngck1spyjvb1l0z2ja40ihnk5bxnsz"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-1712
  (let ((commit "705d2a3c2cd94faf8e16c660b3b342d6fe900e55")
        (revision "1712")
        (base zig-0.10.0-1681))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "08znnqyacjkdls4dhx6mis6yi6za0zv4abi2q3xl2304yjczm288"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-1713
  (let ((commit "09a84c8384dffc7884528947b879f32d93c1bd90")
        (revision "1713")
        (base zig-0.10.0-1712))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "1rbyqwd7iqbgwnws6rqr2d5bxxdh4z6phbg23hzm9i1xlxka6y6v"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-1888
  (let ((commit "c839c180ef1686794c039fc6d3c20a8716e87357")
        (revision "1888")
        (base zig-0.10.0-1713))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0kxgjzzqqknqimi0a0c75blgr2ic550nchbag4mv1yfrhr5y0x27"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define-public zig zig-0.10)
