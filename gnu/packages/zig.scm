;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Calum Irwin <calumirwin1@gmail.com>
;;; Copyright © 2022-2024 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix platform)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages web)
  #:export (add-build.zig.zon
            rename-zig-dependencies))

(define* (add-build.zig.zon name version dependencies #:optional (paths '("")))
  "Snippet to generate build.zig.zon of DEPENDENCIES for package NAME@VERSION."
  `(let ((port (open-file "build.zig.zon" "w" #:encoding "utf8")))
     (format port "\
.{
    .name = \"~a\",
    .version = \"~a\",
    .paths = .{
~{\
        \"~a\",
~}\
    },
    .dependencies = .{
~{\
        .@\"~a\" = .{
            .url = \"\",
        },
~}\
    },
}~%" ,name ,version (quote ,paths) (quote ,dependencies))
     (close-port port)))

(define* (rename-zig-dependencies mapping #:optional (directories '(".")))
  "Snippet to rename Zig dependencies in build.zig and build.zig.zon."
  `(begin
     (use-modules (ice-9 match)
                  (guix build utils))
     (for-each
      (lambda (directory)
        (for-each
         (match-lambda
           ((old-name . new-name)
            (with-directory-excursion directory
              (substitute* "build.zig"
                (((string-append "([Dd]ependency.\")" old-name) _ prefix)
                 (string-append prefix new-name)))
              (substitute* "build.zig.zon"
                (((format #f "\\.(@\")?~a\"?" old-name))
                 (format #f ".@\"~a\"" new-name))))))
         (quote ,mapping)))
      (quote ,directories))))

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
         "zig-0.9-build-respect-PKG_CONFIG-env-var.patch"
         "zig-0.9-riscv-support.patch"
         "zig-0.9-use-baseline-cpu-by-default.patch"
         "zig-0.9-use-system-paths.patch"
         "zig-0.9-fix-runpath.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:imported-modules
      (cons '(guix build zig-utils)
            %cmake-build-system-modules)
      #:modules
      (cons '(guix build zig-utils)
            '((guix build cmake-build-system)
              (guix build utils)))
      #:configure-flags
      #~(list (string-append "-DZIG_LIB_DIR=" #$output "/lib/zig")
              "-DZIG_TARGET_MCPU=baseline"
              (string-append
               "-DZIG_TARGET_TRIPLE="
               (zig-target
                #$(platform-target
                   (lookup-platform-by-target-or-system
                    (or (%current-target-system)
                        (%current-system))))))
              "-DZIG_USE_LLVM_CONFIG=ON")
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
          (add-before 'configure 'zig-configure zig-configure)
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; error(libc_installation): msvc_lib_dir may not be empty for
                ;; windows-msvc.
                (unsetenv "ZIG_LIBC")
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
     (list $C_INCLUDE_PATH
           $CPLUS_INCLUDE_PATH
           $LIBRARY_PATH
           (search-path-specification
            (variable "GUIX_ZIG_PACKAGE_PATH")
            (files '("src/zig")))))
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
         "zig-0.10-build-respect-PKG_CONFIG-env-var.patch"
         "zig-0.9-use-baseline-cpu-by-default.patch"
         "zig-0.10-use-system-paths.patch"
         "zig-0.10-fix-runpath.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments zig-0.9)
       ((#:tests? _ #t)
        (not (%current-target-system)))
       ((#:configure-flags flags ''())
        #~(cons "-DZIG_SHARED_LLVM=ON"
                #$flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            #$@(if (target-riscv64?)
                   `((delete 'adjust-tests))
                   '())
            (add-after 'patch-source-shebangs 'patch-more-shebangs
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Zig uses information about an ELF file to determine the
                ;; version of glibc and other data for native builds.
                (substitute* "lib/std/zig/system/NativeTargetInfo.zig"
                  (("/usr/bin/env") (search-input-file inputs "bin/clang++")))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  ;; error(libc_installation): msvc_lib_dir may not be empty for
                  ;; windows-msvc.
                  (unsetenv "ZIG_LIBC")
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
         ;; Patch for cross-compilation not applied to intermediate versions.
         ((#:modules modules '())
          (cons '(srfi srfi-1) modules))
         ((#:configure-flags flags ''())
          #~(filter (lambda (flag)
                      (not (string-contains flag "ZIG_TARGET_TRIPLE")))
                    #$flags))
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
              (add-after 'unpack 'set-host-triple
                (lambda _
                  (substitute* "CMakeLists.txt"
                    (("\\$\\{(ZIG_)?HOST_TARGET_TRIPLE\\}")
                     (zig-target
                      #$(platform-target
                         (lookup-platform-by-system (%current-system))))))))
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

(define zig-0.10.0-1891
  (let ((commit "ac1b0e832b4b9d151098050e1d29e28a568e215c")
        (revision "1891")
        (base zig-0.10.0-1888))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "15qjgbmygk05p86wqm170lxmalq2mr9f81slf8svb7akkmx5qls2"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-2558
  (let ((commit "d3a237a98c5a2ccf72a774b5f93425c02fea4bea")
        (revision "2558")
        (base zig-0.10.0-1891))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0q9583w919gvi91wbp529q54ijr8pldj8s0p2yapkbniadxxil7y"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (inputs
       (modify-inputs (package-inputs base)
         (replace "clang" clang-16)
         (replace "lld" lld-16)))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "llvm" llvm-16)
         (replace "zig" `(,base "zig1"))))
      (properties `((max-silent-time . 9600)
                    ,@(clang-compiler-cpu-architectures "16"))))))

(define zig-0.10.0-2565-source
  (let ((commit "856a9c2e3120d9ffa1166eed13641600230946da")
        (revision "2565"))
    (zig-source
     (git-version "0.10.0" revision commit)
     commit "00wqqckiyl6c3zcvgqxssnnv4ajip872ghrgv4mfrc8sllnhkdwa")))

(define zig-0.10.0-2566
  (let ((commit "e2fe1907ecac075e4d4a37776359144318b6055a")
        (revision "2566")
        (base zig-0.10.0-2558))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "1vy36ksv7jdxdkspi0jvmfz2xwvbc26x3rqns51wk3a39ngx1g2f"))
      (arguments
       ;; zig1
       (substitute-keyword-arguments (package-arguments zig-0.10.0-747)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'restore-lib
                (lambda _
                  (let ((path "lib/std/start.zig"))
                    (copy-file (in-vicinity #+zig-0.10.0-2565-source path)
                               path))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-2571
  (let ((commit "31738de2817f7932fa9237492f20fb736bc07dd3")
        (revision "2571")
        (base zig-0.10.0-2566))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0hak2yydzjjn61lpbmmvkvykc1imgb92qkn90p4wi4fw9nfsqq0v"))
      ;; zig2+zig1
      (arguments (package-arguments zig-0.10.0-748))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-2796-source
  (let ((commit "42ee364e7b698822a69cba4cd2bda17868657e05")
        (revision "2796"))
    (zig-source
     (git-version "0.10" revision commit)
     commit "0jd9q7bk6vsvjfma8d8iic7r9hc7j6xkz1ghw2vpra5q20fl86ba")))

(define zig-0.10.0-2797
  (let ((commit "35d82d31be3d2f2611049f41dc2616f898d70871")
        (revision "2797")
        (base zig-0.10.0-2571))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0av4qw7a41d9jmdmyachhisq98c3vjw1dqln1lgy1y3jxjsdf27h"))
      ;; zig2+zig1
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-748)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'prepare-source 'restore-lib
                (lambda _
                  (delete-file-recursively "lib")
                  (copy-recursively
                   (string-append #+zig-0.10.0-2796-source "/lib")
                   "lib")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-2824-source
  (let ((commit "8d88dcdc61c61e3410138f4402482131f5074a80")
        (revision "2824"))
    (zig-source
     (git-version "0.10" revision commit)
     commit "0xyhr98hyyb9b3c3d2lv6hxysaq1k1kmw9gynci0z9wm1y82rir8")))

(define zig-0.10.0-2838
  (let ((commit "a8de15f66a51d273cefa07eed0d8fd2952e92387")
        (revision "2838")
        (base zig-0.10.0-2797))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0hhz1hijg5hnw41s4p4p15gllpql5hn9my6a3d80jxv8nmd367q1"))
      ;; zig2+zig1
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-748)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-before 'prepare-source 'backup-source
                (lambda _
                  (copy-recursively "lib" "../lib-backup")))
              (add-after 'prepare-source 'restore-lib
                (lambda _
                  (delete-file-recursively "lib")
                  (copy-recursively
                   (string-append #+zig-0.10.0-2824-source "/lib")
                   "lib")))
              (add-before 'build-zig1 'restore-source
                (lambda _
                  (delete-file-recursively "lib")
                  (copy-recursively "../lib-backup" "lib")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-3660
  (let ((commit "22c6b6c9a9378aaca75c83c2182a6d94298f6bc2")
        (revision "3660")
        (base zig-0.10.0-2838))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0dhcdi6008qqvy3ws2hhmj51wr213whbyghh7n9arai3zyg6y65g"))
      ;; zig2+zig1
      (arguments (package-arguments zig-0.10.0-748))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-3726
  (let ((commit "a6c8ee5231230947c928bbe1c6a39eb6e1bb9c5b")
        (revision "3726")
        (base zig-0.10.0-3660))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "148dhnzhm52lcwhvpwnnvpkpd3g6i1xh2vsac858agqr0slsd7g9"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-3728
  (let ((commit "a4d1edac8d65e1aa4b565f6fb11ab78541d97efa")
        (revision "3728")
        (base zig-0.10.0-3726))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "13vff9hqp83xhz0fab9wcwsf56hcz4sg3rwn1vziq85wkr9scj4b"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-3807
  (let ((commit "be0c69957e7489423606023ad820599652a60e15")
        (revision "3807")
        (base zig-0.10.0-3728))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "069w7d67imdn4qgdk7acddsfwl4dhs9nzna5k4h0cza2cl0xi0ic"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.10.0-3813
  (let ((commit "21ac0beb436f49fe49c6982a872f2dc48e4bea5e")
        (revision "3813")
        (base zig-0.10.0-3807))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0dmi1d8jg7y7zgi8xyq53g4g39ba4hnigyj491a5fj8xnkxqfrrb"))
      ;; zig2+zig1
      (arguments (package-arguments zig-0.10.0-748))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-3980
  (let ((commit "4bce7b1db964098e4a9163201fa3adcb26af6d97")
        (revision "3980")
        (base zig-0.10.0-3813))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0h76x1ak28dv5y60r4m4y524kwmf43ridwvsx3ahfql63jwxn0m3"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.10.0-3985
  (let ((commit "47d5bf26164b4ddb3228d17ae2158d1c29b8d040")
        (revision "3985")
        (base zig-0.10.0-3980))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.10.0" revision commit))
      (source (zig-source
               version commit
               "0w412aka8wjkkwyssqg8nvzb7qwaa29ywzwhg11bbwkpqyig36r4"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.11-glibc-abi-tool
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ziglang/glibc-abi-tool")
          (commit "13576b1ea957882be7ff2c99f4cdc27454930219")))
    (file-name "glibc-abi-tool")
    (sha256
     (base32 "09m0ipixxw0dnal0zsgk6kvcz29y9s256b9y00s4hkhj95n630il"))
    (modules '((guix build utils)))
    (snippet
     #~(begin
         (substitute* "consolidate.zig"
           ((".*minor = 3[5678].*") "")
           (("(w\\.writeIntLittle.u16, )(@intCast.*);" _ prefix suffix)
            (string-append prefix "@as(u16, " suffix ");")))
         (with-directory-excursion "glibc"
           (for-each delete-file-recursively
                     '("2.35" "2.36" "2.37" "2.38")))))))

(define-public zig-0.11
  (package
    (inherit zig-0.10)
    (name "zig")
    (version "0.11.0")
    (source
     (origin
       (inherit (zig-source
                 version version
                 "0qh7c27cd4wcdjj0mbpkarvwypfk1js8hkyxs0z149qv75zkbrca"))
       (patches
        (search-patches
         "zig-0.11-build-respect-PKG_CONFIG-env-var.patch"
         "zig-0.9-use-baseline-cpu-by-default.patch"
         "zig-0.11-use-system-paths.patch"
         "zig-0.11-fix-runpath.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments zig-0.10)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'set-host-triple
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("\\$\\{ZIG_HOST_TARGET_TRIPLE\\}")
                   (zig-target
                    #$(platform-target
                       (lookup-platform-by-system (%current-system))))))))
            (add-after 'unpack 'prepare-source
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (install-file (search-input-file
                               (or native-inputs inputs) "bin/zig1.wasm")
                              "stage1")
                (make-file-writable "stage1/zig1.wasm")))
            (add-after 'install 'build-zig1
              (lambda _
                (invoke (string-append #$output "/bin/zig")
                        "build" "update-zig1" "--verbose")))
            (add-after 'build-zig1 'install-zig1
              (lambda _
                (install-file "stage1/zig1.wasm"
                              (string-append #$output:zig1 "/bin"))))
            ;; TODO: Disable tests for macOS target and run full test.
            ;; Issue with glibc in CPLUS_INCLUDE_PATH:
            ;; <https://github.com/ziglang/zig/issues/18063>.
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke (string-append #$output "/bin/zig")
                          "test" "-I" "test" "test/behavior.zig"))))))))
    (inputs
     (modify-inputs (package-inputs zig-0.10)
       (replace "clang" clang-16)
       (replace "lld" lld-16)))
    (native-inputs
     (modify-inputs (package-native-inputs zig-0.10)
       (prepend binaryen `(,zig-0.10.0-3985 "zig1"))
       (replace "glibc-abi-tool" zig-0.11-glibc-abi-tool)
       (replace "llvm" llvm-16)))
    (outputs '("out" "zig1"))
    (properties `((max-silent-time . 9600)
                  ,@(clang-compiler-cpu-architectures "16")))))


;;;
;;; Bootstrap path for Zig 0.12.
;;; See also: <https://git.jakstys.lt/motiejus/zig-repro>.
;;;

(define zig-0.11.0-149
  (let ((commit "7a85ad151daece3d0bba3c8d23081502a0956c95")
        (revision "149")
        (base zig-0.11))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "1kb245d4wfs1dyv7ccw3xiawasggpln9qxfqwlp4gkdg50l1qyzw"))
      ;; zig1
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-747)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'build-zig1
                (lambda _
                  (invoke "zig" "build" "--zig-lib-dir" "lib"
                          "update-zig1" "--verbose")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.11.0-384
  (let ((commit "88f5315ddfc6eaf3e28433504ec046fb3252db7c")
        (revision "384")
        (base zig-0.11.0-149))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "0ybql8l4mg8i79n353rc7gbx88kqgd371xrlvpmy69mxdffh5bas"))
      ;; zig2+zig1
      (arguments (package-arguments zig-0.10.0-748))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.11.0-494
  (let ((commit "a8d2ed806558cc1472f3a532169a4994abe17833")
        (revision "494")
        (base zig-0.11.0-384))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "04jb7la7ang0mip9qbrx57j1ls2n29svqafschxbh5j23pf74dql"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (inputs
       (modify-inputs (package-inputs base)
         (replace "clang" clang-17)
         (replace "lld" lld-17)))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "llvm" llvm-17)
         (replace "zig" `(,base "zig1"))))
      (properties `((max-silent-time . 9600)
                    ,@(clang-compiler-cpu-architectures "17"))))))

(define zig-0.11.0-587
  (let ((commit "6bd54a1d3ebd8d997158c57057ba742824cf7e0c")
        (revision "587")
        (base zig-0.11.0-494))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "16v6yrbwg1pc888fp3cmdbk2wyz0nm8xp66a14fi3akxyavsmkxm"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.11.0-631
  (let ((commit "21780899eb17a0cb795ff40e5fae6556c38ea13e")
        (revision "631")
        (base zig-0.11.0-587))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "19j3gbdsjsp602n2c1lp3i96yay94acsiasyzns8hs3v1sc952rp"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.11.0-638
  (let ((commit "9763573ebb4f05eaa1c0bd5598f8dd6aee20ae9c")
        (revision "638")
        (base zig-0.11.0-631))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "13620a6hlcaklmnxax4g3f4irddr9d15646s6bgn6ymwf6m4w4g0"))
      ;; zig1+zig2
      (arguments (package-arguments zig-0.10.0-1073))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.11.0-702
  (let ((commit "63bd2bff12992aef0ce23ae4b344e9cb5d65f05d")
        (revision "702")
        (base zig-0.11.0-638))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "049fpdgkarp834amymw0clvsk6g20742d3940la60na03rc4vy5x"))
      ;; zig1
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-747)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'build-zig1
                (lambda _
                  (invoke "zig2" "build" "update-zig1" "--verbose")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.11.0-761
  (let ((commit "9a09651019b24a32945f73dd7a69562f2cf31581")
        (revision "761")
        (base zig-0.11.0-702))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "0d503vq76vl7m9fcmyqwx5nljy04fvjf06wfr7q7n5ircw5wjp0s"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.11.0-1967
  (let ((commit "6beae6c061a650ea2d695e6902683641df920aa8")
        (revision "1967")
        (base zig-0.11.0-761))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "0bah98h9rdaynwxlq4ibr925hmk69kwbrkgid2dgsksf6krb6sms"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.11.0-3245
  (let ((commit "4f782d1e853accbe1c4bfab2617c3813d4b1e59f")
        (revision "3245")
        (base zig-0.11.0-1967))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "0a4pnilfc528zl9ycwsi8kaqbq6q4bw3l1cpv50fpacsxgnsfjp3"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.11.0-3344
  (let ((commit "e646e0116196c2bd9668317366f5380f08c30e6e")
        (revision "3344")
        (base zig-0.11.0-3245))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "1vymn6qffmkhkl3rnp8zrxa1vwg8ayg5r2z3qz9bpfc6r2sb6ddq"))
      ;; zig1
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-747)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'build-zig1
                (lambda _
                  (invoke "zig2" "build" "update-zig1" "--verbose")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.11.0-3501
  (let ((commit "9b2345e182090e2f4c57e7684ec9739f195fdb1d")
        (revision "3501")
        (base zig-0.11.0-3344))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "0gwfnsjc3avx51cr5cwancms8s14hkrhqvk926cz3phgnn4jmpz0"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.11.0-3503
  (let ((commit "17673dcd6e3ffeb25fc9dc1cfc72334ab4e71b37")
        (revision "3503")
        (base zig-0.11.0-3501))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "09j6svmrlk9ybkz308vbdqqx87yc20yfj7jk59vj451478x4fcnb"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.11.0-3506
  (let ((commit "fb192df4f2d12dda5019e14bf6cab2693432cb36")
        (revision "3506")
        (base zig-0.11.0-3503))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "1k5qd1pf563wi4hk4qs29sxph38n7jd4ic5w7qw80pi3aq9c9vbx"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.11.0-3604
  (let ((commit "7611d90ba011fb030523e669e85acfb6faae5d19")
        (revision "3604")
        (base zig-0.11.0-3506))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.11.0" revision commit))
      (source (zig-source
               version commit
               "1yl91rpqsnlf38hg77phpl5hy7ds0cg57agr23hjhy50z4vdj3m4"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.12-glibc-abi-tool
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ziglang/glibc-abi-tool")
          (commit "fc5d0a7046b76795e4219f8f168e118ec29fbc53")))
    (file-name "glibc-abi-tool")
    (sha256
     (base32 "1q9plbqkkk3jzrvsgcjmj5jjdncz4ym9p0snglz4kkjwwm65gqs1"))))

(define-public zig-0.12
  (package
    (inherit zig-0.11)
    (name "zig")
    (version "0.12.1")
    (source
     (origin
       (inherit (zig-source
                 version version
                 "0ssgfrsk116p16rwjwq1z2pvvcdij6s30s19bhzjms7maz4s77hb"))
       (patches
        (search-patches
         "zig-0.12-build-respect-PKG_CONFIG-env-var.patch"
         "zig-0.12-use-baseline-cpu-by-default.patch"
         "zig-0.12-use-system-paths.patch"
         "zig-0.12-fix-runpath.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments zig-0.11)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (replace 'patch-more-shebangs
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Zig uses information about an ELF file to determine the
                ;; version of glibc and other data for native builds.
                (substitute* "lib/std/zig/system.zig"
                  (("/usr/bin/env")
                   (search-input-file inputs "bin/clang++")))))))))
    (inputs
     (modify-inputs (package-inputs zig-0.11)
       (replace "clang" clang-17)
       (replace "lld" lld-17)))
    (native-inputs
     (modify-inputs (package-native-inputs zig-0.11)
       (replace "glibc-abi-tool" zig-0.12-glibc-abi-tool)
       (replace "llvm" llvm-17)
       (replace "zig" `(,zig-0.11.0-3604 "zig1"))))
    (properties `((max-silent-time . 9600)
                  ,@(clang-compiler-cpu-architectures "17")))))


;;;
;;; Bootstrap path for Zig 0.13.
;;;

(define zig-0.12.0-109
  (let ((commit "b7799ef322103c8e449c45494c29fb4a8c9867df")
        (revision "109")
        (base zig-0.12))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.12.0" revision commit))
      (source (zig-source
               version commit
               "1zy19w93wrd7dfdih8hfk9h3brkgaspaa60ipcmf08hlx6z2f0bz"))
      ;; zig1
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-747)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              ;; Build errors when zig1.wasm is not found.
              (add-after 'unpack 'prepare-source
                (lambda _
                  (invoke "touch" "stage1/zig1.wasm")))
              (replace 'build-zig1
                (lambda _
                  (invoke "zig" "build" "--zig-lib-dir" "lib"
                          "update-zig1" "--verbose")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.13-glibc-abi-tool
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ziglang/glibc-abi-tool")
          (commit "fc5d0a7046b76795e4219f8f168e118ec29fbc53")))
    (file-name "glibc-abi-tool")
    (sha256
     (base32 "1q9plbqkkk3jzrvsgcjmj5jjdncz4ym9p0snglz4kkjwwm65gqs1"))))

(define-public zig-0.13
  (package
    (inherit zig-0.12)
    (name "zig")
    (version "0.13.0")
    (source
     (origin
       (inherit (zig-source
                 version version
                 "0ly8042lbsa8019g0d1jg4l06rxpq2530n9mijq66n4lmx7a5976"))
       (patches
        (search-patches
         "zig-0.13-build-respect-PKG_CONFIG-env-var.patch"
         "zig-0.12-use-baseline-cpu-by-default.patch"
         "zig-0.12-use-system-paths.patch"
         "zig-0.13-fix-runpath.patch"))))
    (inputs
     (modify-inputs (package-inputs zig-0.12)
       (replace "clang" clang-18)
       (replace "lld" lld-18)))
    (native-inputs
     (modify-inputs (package-native-inputs zig-0.12)
       (replace "glibc-abi-tool" zig-0.13-glibc-abi-tool)
       (replace "llvm" llvm-18)
       (replace "zig" `(,zig-0.12.0-109 "zig1"))))
    (properties `((max-silent-time . 9600)
                  ,@(clang-compiler-cpu-architectures "18")))))


;;;
;;; Bootstrap path for Zig 0.14.
;;;

(define zig-0.13.0-286
  (let ((commit "d72a8db2db1a5c77af2deb713248dc53f9adcb73")
        (revision "286")
        (base zig-0.13))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "1rnpv4lwc2jxz5yymrb4hlfy4iii48bn94rjpr3vll8yyydinpnb"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.13.0-503
  (let ((commit "25198810c88d474c63ff537e3647f12e7df6297c")
        (revision "503")
        (base zig-0.13.0-286))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "1i2baqav7qxv7ll94win6xfsm45dpzrwkwpayzrdll1aq9k4ywvb"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.13.0-1323
  (let ((commit "457c94d353b77b08786aa8794e1afc6a62d5c34a")
        (revision "1323")
        (base zig-0.13.0-503))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "1kh4iy7mk86s7fpqgrp5i2m1a3d8vhz5plmhkglzykdr3calqqhi"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.13.0-1528
  (let ((commit "8ec68c63fa1d0f7e267ff92c054e8aa335f91da4")
        (revision "1528")
        (base zig-0.13.0-1323))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "1a7q20wbadjqj0xmajq6f3kk9mlkz9wqgvw4zm62r5bn1xzcs5kd"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.13.0-1951
  (let ((commit "8573836892ba1b7cd34d377b46258930161256c3")
        (revision "1951")
        (base zig-0.13.0-1528))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "094yxy3db6sjvcni4jl1d6nf46hlarhw6hnxhcw7vwq4qaj9w9xq"))
      ;; zig2
      (arguments
       (substitute-keyword-arguments (package-arguments zig-0.10.0-851)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              ;; See also upstream commit
              ;; f1f804e532e603a3f58d559d32a1c8120691d891.
              (add-after 'unpack 'fix-compilation
                (lambda _
                  (substitute* "src/zig_llvm.cpp"
                    ((".*LastEnvironmentType.*") ""))))))))
      (inputs
       (modify-inputs (package-inputs base)
         (replace "clang" clang-19)
         (replace "lld" lld-19)))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "llvm" llvm-19)
         (replace "zig" `(,base "zig1"))))
      (properties `((max-silent-time . 9600)
                    ,@(clang-compiler-cpu-architectures "19"))))))

(define zig-0.13.0-1952
  (let ((commit "51706af908e0c6acb822ef36760b7fe31faf62a6")
        (revision "1952")
        (base zig-0.13.0-1951))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "1ypwd8is0zmnq042qzya34jks1sfkk1grdi1asymbqnmp0c3p4m7"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.13.0-2795
  (let ((commit "0cc9d68b77b31c6502da80ce7e6dabaf9316ca48")
        (revision "2795")
        (base zig-0.13.0-1952))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "11v7d6f34h9mayn00la5898h34xw335rh0fp9jk1gyxvd49k92ip"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.13.0-2899
  (let ((commit "8b5c4baed8aa99612734b763ecb1fa0364dc7940")
        (revision "2899")
        (base zig-0.13.0-2795))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "13kiyr9v7p2vyqyqb5ry178d7lsd1hmkhsw5p3hal7fndy8yyrph"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.13.0-2924
  (let ((commit "83991efe10d92c4b920d7b7fc75be98ed7854ad7")
        (revision "2924")
        (base zig-0.13.0-2899))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "1ksq4yr21y28x3m0mz9vn3c30pfrcvl3av5kc9zjxyip7r9as9xp"))
      ;; zig2
      (arguments (package-arguments zig-0.10.0-851))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.13.0-2925
  (let ((commit "b0a8931690660da753f9684d65db4b1bdfe29757")
        (revision "2925")
        (base zig-0.13.0-2924))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "19nxizmi0l70750kpn9fa0bdsz0arifjvrlms0zm0y7g8pzyqzvs"))
      ;; zig1
      (arguments (package-arguments zig-0.10.0-747))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "out")))))))

(define zig-0.13.0-3252
  (let ((commit "f90f8f59a5f98ae3264bc00dbff4d56a0e4ef65e")
        (revision "3252")
        (base zig-0.13.0-2925))
    (package
      (inherit base)
      (name "zig")
      (version (git-version "0.13.0" revision commit))
      (source (zig-source
               version commit
               "1hf9fvk2cm5n6ryg00w95sk50n96j5sgdnxh1bwmwcva0lib000y"))
      ;; zig2+zig1
      (arguments (package-arguments zig-0.10.0-748))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "zig" `(,base "zig1")))))))

(define zig-0.14-glibc-abi-tool
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ziglang/glibc-abi-tool")
          (commit "ed9d3bb356413e73b836955e75f399dfb3ec255e")))
    (file-name "glibc-abi-tool")
    (sha256
     (base32 "0ckhwlszm0vkiqsyp2rzp1zcfljh1ng24c7pdvpyfj51x4s612z1"))))

(define-public zig-0.14
  (package
    (inherit zig-0.13)
    (name "zig")
    (version "0.14.1")
    (source
     (origin
       (inherit (zig-source
                 version version
                 "0cnc3bzl965ckm70ayd6a29zn5h9cy3qzyfncp4n7mzkiwhlj58f"))
       (patches
        (search-patches
         "zig-0.14-use-baseline-cpu-by-default.patch"
         "zig-0.14-use-system-paths.patch"
         "zig-0.14-fix-runpath.patch"))
       (snippet
        #~(begin
            #$(origin-snippet (package-source zig-0.13))
            ;; TODO: Add this to zig-source.
            (substitute* "build.zig"
              (("\\.*.max_rss.*") ""))))))
    (inputs
     (modify-inputs (package-inputs zig-0.13)
       (replace "clang" clang-19)
       (replace "lld" lld-19)))
    (native-inputs
     (modify-inputs (package-native-inputs zig-0.13)
       (replace "glibc-abi-tool" zig-0.14-glibc-abi-tool)
       (replace "llvm" llvm-19)
       (replace "zig" `(,zig-0.13.0-3252 "zig1"))))
    (properties `((max-silent-time . 9600)
                  ,@(clang-compiler-cpu-architectures "19")))))

(define-public zig zig-0.13)
