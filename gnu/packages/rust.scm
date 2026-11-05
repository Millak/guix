;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017, 2019-2026 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020, 2021 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Jim Newsome <jnewsome@torproject.org>
;;; Copyright © 2022 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2023 Fries <fries1234@protonmail.com>
;;; Copyright © 2023 Herman Rimm <herman_rimm@protonmail.com>
;;; Copyright © 2026 Untrusem <mysticmoksh@riseup.net>
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

(define-module (gnu packages rust)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages mingw)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix search-paths)
  #:use-module (guix download)
  #:use-module (guix memoization)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-35))

;; This is the hash for the empty file, and the reason it's relevant is not
;; the most obvious.
;;
;; The root of the problem is that Cargo keeps track of a file called
;; Cargo.lock, that contains the hash of the tarball source of each dependency.
;;
;; However, tarball sources aren't handled well by Guix because of the need to
;; patch shebangs in any helper scripts. This is why we use Cargo's vendoring
;; capabilities, where instead of the tarball, a directory is provided in its
;; place. (In the case of rustc, the source code already ships with vendored
;; dependencies, but crates built with cargo-build-system undergo vendoring
;; during the build.)
;;
;; To preserve the advantages of checksumming, vendored dependencies contain
;; a file called .cargo-checksum.json, which contains the hash of the tarball,
;; as well as the list of files in it, with the hash of each file.
;;
;; The patch-cargo-checksums phase of cargo-build-system runs after
;; any Guix-specific patches to the vendored dependencies and regenerates the
;; .cargo-checksum.json files, but it's hard to know the tarball checksum that
;; should be written to the file - and taking care of any unhandled edge case
;; would require rebuilding everything that depends on rust. This is why we lie,
;; and say that the tarball has the hash of an empty file. It's not a problem
;; because cargo-build-system removes the Cargo.lock file. We can't do that
;; for rustc because of a quirk of its build system, so we modify the lock file
;; to substitute the hash.
(define %cargo-reference-hash
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(define* (rust-uri version #:key (dist "static"))
  (string-append "https://" dist ".rust-lang.org/dist/"
                 "rustc-" version "-src.tar.gz"))

(define* (rust-bootstrapped-package base-rust version checksum)
  "Bootstrap rust VERSION with source checksum CHECKSUM using BASE-RUST."
  (package
    (inherit base-rust)
    (version version)
    (source
     (origin
       (inherit (package-source base-rust))
       (uri (rust-uri version))
       (sha256 (base32 checksum))))
    (arguments
     (substitute-keyword-arguments (package-arguments base-rust)
       ((#:disallowed-references _ '())
        (list (this-package-native-input "rust")
              ;; Refer to cargo as #$rustc:cargo.
              (gexp-input (this-package-native-input "rust") "cargo")))))
    (native-inputs
     (list pkg-config
           python-minimal-wrapper
           python-setuptools-bootstrap
           base-rust
           `(,base-rust "cargo")))))

;;; Note: mrustc's only purpose is to be able to bootstrap Rust; it's designed
;;; to be used in source form.
(define %mrustc-commit-1.90 "a822472b9ee744e58d8b1a79bdd8080f7a7b008f")
(define %mrustc-source-1.90
  (let* ((version "0.12.0")
         (commit %mrustc-commit-1.90)
         (revision "1")
         (name "mrustc"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/thepowersgang/mrustc")
            (commit %mrustc-commit-1.90)))
      (file-name (git-file-name name (git-version version revision commit)))
      (sha256
       (base32 "0abf6z6nkgdrfzky99k81k3fqj9bc1sgplxgpi58m0vgz9pd0szz"))
      (patches (search-patches "mrustc-patches-1.90.patch"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Drastically reduces memory and build time requirements
          ;; by disabling debug by default.
          (substitute* (find-files "." "Makefile")
            (("LINKFLAGS := -g") "LINKFLAGS :=")
            (("-g ") ""))
          (substitute* "minicargo.mk"
            ;; Don't assume tarball with gzip compression.
            (("-xzf") "-xf")
            ;; Don't use the vendored openssl sources.
            (("--features vendored-openssl") "")))))))

;;; The rust-bootstrap packages are special in that they are built with mrustc,
;;; which shortens the bootstrap path.
(define-public rust-bootstrap-1.90
  (package
    (name "rust")
    (version "1.90.0")
    (source
     (origin
       (method url-fetch)
       (uri (rust-uri version))
       (sha256 (base32 "0zlv3ybd8za07brxwj4n03cma1snnpvbqj0h0wg3bmafpaf9z6kr"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file-recursively
                     '("src/llvm-project"
                       "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                       "vendor/openssl-src-300.5.0+3.5.0/openssl"
                       "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                       "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"))
           ;; Remove vendored dynamically linked libraries.
           ;; find . -not -type d -executable -exec file {} \+ | grep ELF
           ;; Also remove the bundled (mostly Windows) libraries.
           (for-each delete-file
                     (find-files "vendor" "\\.(a|dll|exe|lib)$"))
           ;; Adjust vendored dependency to explicitly use rustix with libc
           ;; backend.
           (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                          "vendor/tempfile-3.16.0/Cargo.toml"
                          "vendor/tempfile-3.19.0/Cargo.toml"
                          "vendor/tempfile-3.19.1/Cargo.toml"
                          "vendor/tempfile-3.20.0/Cargo.toml")
             (("features = \\[\"fs\"" all)
              (string-append all ", \"use-libc\"")))))))
    (outputs '("out" "cargo"))
    (properties '((hidden? . #t)
                  (timeout . 129600)          ;36 hours
                  (max-silent-time . 18000))) ;5 hours (for riscv64)
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal
           llvm-21
           openssl
           zlib))
    (native-inputs
     (list pkg-config %mrustc-source-1.90))
    (arguments
     (list
      #:imported-modules %cargo-utils-modules ;for `generate-all-checksums'
      #:modules '((guix build cargo-utils)
                  (guix build utils)
                  (guix build gnu-build-system))
      #:test-target "test"
      ;; Rust's own .so library files are not found in any RUNPATH, but
      ;; that doesn't seem to cause issues.
      #:validate-runpath? #f
      #:make-flags
      #~(let ((source #$(package-source this-package)))
          (list (string-append "RUSTC_TARGET="
                               #$(platform-rust-target
                                  (lookup-platform-by-target-or-system
                                   (or (%current-target-system)
                                       (%current-system)))))
                (string-append "RUSTC_VERSION=" #$version)
                (string-append "MRUSTC_TARGET_VER="
                               #$(version-major+minor version))
                (string-append "RUSTC_SRC_TARBALL=" source)
                "OUTDIR_SUF="))       ;do not add version suffix to output dir
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source inputs #:allow-other-keys)
              ((assoc-ref %standard-phases 'unpack)
               #:source #$(this-package-native-input
                           (origin-file-name %mrustc-source-1.90)))))
          (add-after 'unpack 'patch-makefiles
            ;; This disables building the (unbundled) LLVM.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("minicargo.mk"
                             "run_rustc/Makefile")
                ;; Use the system-provided LLVM.
                (("LLVM_CONFIG [:|?]= .*")
                 (string-append "LLVM_CONFIG := "
                                (search-input-file inputs "/bin/llvm-config") "\n")))
              (substitute* "Makefile"
                ;; Patch date and git obtained version information.
                ((" -D VERSION_GIT_FULLHASH=.*")
                 (string-append
                  " -D VERSION_GIT_FULLHASH=\\\"" #$%mrustc-commit-1.90 "\\\""
                  " -D VERSION_GIT_BRANCH=\\\"master\\\""
                  " -D VERSION_GIT_SHORTHASH=\\\""
                  #$(string-take %mrustc-commit-1.90 7) "\\\""
                  " -D VERSION_BUILDTIME="
                  "\"\\\"Thu, 01 Jan 1970 00:00:01 +0000\\\"\""
                  " -D VERSION_GIT_ISDIRTY=0\n")))
              (substitute* '("run_rustc/Makefile"
                             "run_rustc/rustc_proxy.sh")
                ;; Patch the shebang of a generated wrapper for rustc
                (("#!/bin/sh")
                 (string-append "#!" (which "sh"))))))
          (add-before 'configure 'configure-cargo-home
            (lambda _
              (let ((cargo-home (string-append (getcwd) "/.cargo")))
                (mkdir-p cargo-home)
                (setenv "CARGO_HOME" cargo-home))))
          (replace 'configure
            (lambda _
              (setenv "CC" "gcc")
              (setenv "CXX" "g++")
              ;; The Guix LLVM package installs only shared libraries.
              (setenv "LLVM_LINK_SHARED" "1")
              ;; rustc still insists on having 'cc' on PATH in some places
              ;; (e.g. when building the 'test' library crate).
              (mkdir-p "/tmp/bin")
              (symlink (which "gcc") "/tmp/bin/cc")
              (setenv "PATH" (string-append "/tmp/bin:" (getenv "PATH")))))
          (delete 'patch-generated-file-shebangs)
          (replace 'build
            (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
              (let ((job-count (if parallel-build?
                                   (parallel-job-count)
                                   1)))
                ;; Adapted from:
                ;; https://github.com/dtolnay/bootstrap/blob/master/build-1.90.0.sh.
                ;; Use PARLEVEL since both minicargo and mrustc use it
                ;; to set the level of parallelism.
                (setenv "PARLEVEL" (number->string job-count))
                (display "Building mrustc...\n")
                (apply invoke "make" make-flags)

                ;; This doesn't seem to build anything, but it
                ;; sets additional minicargo flags.
                (display "Building RUSTCSRC...\n")
                (apply invoke "make" "RUSTCSRC" make-flags)

                ;; Don't try to use the __bswapdi2 builtin, it's not available
                ;; on the riscv64-linux baseline.
                #$@(if (target-riscv64?)
                       #~((substitute*
                            "rustc-1.90.0-src/library/compiler-builtins/compiler-builtins/src/int/bswap.rs"
                            (("__bswap") "__builtin_bswap")))
                       #~())

                ;; This probably doesn't need to be called explicitly.
                (display "Building LIBS...\n")
                (apply invoke "make" "-f" "minicargo.mk" "LIBS" make-flags)

                (display "Building rustc...\n")
                (apply invoke "make" "-f" "minicargo.mk" "output/rustc"
                       make-flags)

                (display "Building cargo...\n")
                (apply invoke "make" "-f" "minicargo.mk" "output/cargo"
                       make-flags)

                ;; This one is listed in the README.
                (display "Rebuilding stdlib with rustc...\n")
                (apply invoke "make" "-C" "run_rustc" make-flags))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (cargo (assoc-ref outputs "cargo"))
                     (bin (string-append out "/bin"))
                     (rustc (string-append bin "/rustc"))
                     (cargo-bin (string-append cargo "/bin"))
                     (lib (string-append out "/lib"))
                     (system-lib-prefix
                      (string-append lib "/rustlib/"
                                     #$(platform-rust-target
                                        (lookup-platform-by-target-or-system
                                         (or (%current-target-system)
                                             (%current-system)))) "/lib")))
                (with-directory-excursion "run_rustc/output/prefix"
                  ;; The .rmeta files of the bootstrap compiler are not meant
                  ;; to be installed.
                  (for-each delete-file (find-files "." "\\.rmeta$"))

                  (mkdir-p (dirname rustc))
                  (copy-file "bin/rustc_binary" rustc)

                  (wrap-program rustc
                    `("LD_LIBRARY_PATH" = (,system-lib-prefix)))
                  (mkdir-p lib)
                  (copy-recursively "lib" lib)
                  (install-file "bin/cargo" cargo-bin))))))))
    (synopsis "Compiler for the Rust programming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://github.com/thepowersgang/mrustc")

    ;; List of systems where rust-bootstrap is explicitly known to build:
    (supported-systems '("x86_64-linux" "aarch64-linux" "riscv64-linux"))

    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-1.91
  (package
    (name "rust")
    (version "1.91.1")
    (outputs '("out" "cargo"))
    (source
     (origin
       (method url-fetch)
       (uri (rust-uri version))
       (sha256
        (base32 "08sp8idsymyrqj70sxhfjz5zx7nfl4vl4i7hc495fqczsc2y5p1q"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file-recursively
                     '("src/llvm-project"
                       "vendor/curl-sys-0.4.79+curl-8.12.0/curl"
                       "vendor/curl-sys-0.4.83+curl-8.15.0/curl"
                       "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                       "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                       "vendor/libffi-sys-3.3.2/libffi"
                       "vendor/libz-sys-1.1.21/src/zlib"
                       "vendor/libz-sys-1.1.22/src/zlib"
                       "vendor/libmimalloc-sys-0.1.42/c_src/mimalloc"
                       "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                       "vendor/openssl-src-300.5.0+3.5.0/openssl"
                       "vendor/openssl-src-300.5.2+3.5.2/openssl"
                       "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                       "vendor/tikv-jemalloc-sys-0.6.0+5.3.0-1-\
ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
           ;; Remove vendored dynamically linked libraries.
           ;; find . -not -type d -executable -exec file {} \+ | grep ELF
           ;; Also remove the bundled (mostly Windows) libraries.
           (for-each delete-file
                     (find-files "vendor" "\\.(a|dll|exe|lib)$"))
           ;; Use the packaged nghttp2.
           ;; Curl links to nghttp2 so that's the version we link to.
           (for-each
            (lambda (ver)
              (let ((vendored-dir
                     (format #f "vendor/libnghttp2-sys-~a/nghttp2" ver))
                    (build-rs
                     (format #f "vendor/libnghttp2-sys-~a/build.rs" ver)))
                (delete-file-recursively vendored-dir)
                (delete-file build-rs)
                (call-with-output-file build-rs
                  (lambda (port)
                    (format port "fn main() {~@
                       println!(\"cargo:rustc-link-lib=nghttp2\");~@
                       }~%")))))
            '("0.1.11+1.64.0"))
           ;; Adjust vendored dependency to explicitly use rustix with libc
           ;; backend.
           (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                          "vendor/tempfile-3.16.0/Cargo.toml"
                          "vendor/tempfile-3.19.1/Cargo.toml"
                          "vendor/tempfile-3.20.0/Cargo.toml"
                          "vendor/tempfile-3.21.0/Cargo.toml")
             (("features = \\[\"fs\"" all)
              (string-append all ", \"use-libc\"")))))))
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'
       #:modules ((guix build cargo-utils)
                  (guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match)
                  (srfi srfi-1))
       ;; Only the final Rust is tested, not the intermediate bootstrap ones,
       ;; for performance and simplicity.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-cc-shim-to-path
           (lambda _
             (mkdir-p "/tmp/bin")
             (symlink (which "gcc") "/tmp/bin/cc")
             (setenv "PATH" (string-append "/tmp/bin:" (getenv "PATH")))))
         (add-after 'unpack 'set-env
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "SHELL" (which "sh"))
             (setenv "CONFIG_SHELL" (which "sh"))
             (setenv "CC" (search-input-file inputs "/bin/gcc"))
             ;; The Guix LLVM package installs only shared libraries.
             (setenv "LLVM_LINK_SHARED" "1")))
         (add-after 'unpack 'set-linker-locale-to-utf8
           (lambda _
             (substitute* (find-files "." "^linker.rs$")
               (("linker.env\\(\"LC_ALL\", \"C\"\\);")
                "linker.env(\"LC_ALL\", \"C.UTF-8\");"))))
         (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
           (lambda _
             (substitute* (cons* "Cargo.lock"
                                 "src/bootstrap/Cargo.lock"
                                 "library/Cargo.lock"
                                 (filter
                                  ;; Don't mess with the lock files in the
                                  ;; Cargo testsuite; it messes up the tests.
                                  (lambda (path)
                                    (not (string-contains
                                           path "cargo/tests/testsuite")))
                                  (find-files "src/tools" "Cargo.lock")))
               (("(checksum = )\".*\"" all name)
                (string-append name "\"" ,%cargo-reference-hash "\"")))
             (generate-all-checksums "vendor")))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (call-with-output-file "config.toml"
               (lambda (port)
                 (display (string-append "
[llvm]
[build]
optimized-compiler-builtins = false
cargo = \"" (search-input-file inputs "bin/cargo") "\"
rustc = \"" (search-input-file inputs "bin/rustc") "\"
docs = false
python = \"" (search-input-file inputs "bin/python") "\"
vendor = true
submodules = false
[install]
prefix = \"" (assoc-ref outputs "out") "\"
sysconfdir = \"etc\"
[rust]
lld = false
download-rustc = false
debug=false
jemalloc=false
default-linker = \"" (search-input-file inputs "bin/gcc") "\"
channel = \"stable\"
rpath = true
[target." ,(platform-rust-target (lookup-platform-by-system (%current-system))) "]
llvm-config = \"" (search-input-file inputs "bin/llvm-config") "\"
cc = \"" (search-input-file inputs "bin/gcc") "\"
cxx = \"" (search-input-file inputs "bin/g++") "\"
ar = \"" (search-input-file inputs "bin/ar") "\"
[dist]
") port)))))
         (replace 'build
           ;; Just building stage 1 no longer builds cargo.

           ;; TODO: Try reverting to use "./x.py install rustc std cargo"
           ;; when Rust 1.91 comes out, which should no longer rebuild
           ;; things unnecessarily. If this works, the 'post-install'
           ;; phase carried from earlier should no longer be necessary.
           (lambda* (#:key parallel-build? #:allow-other-keys)
             (let ((job-spec (string-append
                               "-j" (if parallel-build?
                                      (number->string (parallel-job-count))
                                      "1"))))
               (invoke "./x.py" job-spec "build" "--stage=2"
                       "library/std"
                       "src/tools/cargo"))))
         (replace 'install
           ;; Rust 1.91+ outputs to stage2 instead of stage1.
           ;; Cannot use './x.py install' as it runs generate-copyright
           ;; which fails due to patched cargo checksums.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (cargo-out (assoc-ref outputs "cargo"))
                    (build (string-append "build/"
                             ,(platform-rust-target
                                (lookup-platform-by-target-or-system
                                 (or (%current-target-system)
                                     (%current-system)))))))
               (with-directory-excursion build
                 (install-file "stage2/bin/rustc"
                               (string-append out "/bin"))
                 (install-file "stage2-tools-bin/cargo"
                               (string-append cargo-out "/bin"))
                 (for-each delete-file
                           (find-files "stage2/lib" "\\.rmeta$"))
                 (for-each delete-file
                           (find-files "stage2/lib/rustlib"
                                       "^librustc_driver.*\\.so$"))
                 (copy-recursively "stage2/lib"
                                   (string-append out "/lib"))))))
         (add-after 'install 'delete-install-logs
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (lambda (f)
                         (false-if-exception (delete-file f)))
                       (append-map (lambda (output)
                                     (find-files (string-append
                                                  output "/lib/rustlib")
                                                 "(^install.log$|^manifest-)"))
                                   (map cdr outputs)))))
         (add-after 'install 'wrap-rustc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (libc (assoc-ref inputs "libc"))
                   (ld-wrapper (assoc-ref inputs "ld-wrapper")))
               ;; Let gcc find ld and libc startup files.
               (wrap-program (string-append out "/bin/rustc")
                 `("PATH" ":" prefix (,(string-append ld-wrapper "/bin")))
                 `("LIBRARY_PATH" ":"
                   suffix (,(string-append libc "/lib"))))))))))
    (native-inputs
     (list pkg-config
           python-minimal-wrapper
           python-setuptools-bootstrap
           rust-bootstrap-1.90
           (list rust-bootstrap-1.90 "cargo")))
    (inputs
     (list bash-minimal
           curl
           llvm-21
           openssl))
    ;; rustc invokes gcc, so we need to set its search paths accordingly.
    (native-search-paths
     %gcc-search-paths)
    ;; Limit this to systems where the final rust compiler builds successfully.
    (supported-systems '("x86_64-linux" "aarch64-linux" "riscv64-linux"))
    (synopsis "Compiler for the Rust programming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://www.rust-lang.org")
    (properties '((hidden? . #t)
                (timeout . 72000)           ;20 hours
                (max-silent-time . 18000))) ;5 hours (for riscv64)
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-1.92
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.91 "1.92.0"
          "1f6305lkp4vwj132fq232mfxdcxg0d5vymc2fpf5y9vybjkjq3cy")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/curl-sys-0.4.79+curl-8.12.0/curl"
                         "vendor/curl-sys-0.4.83+curl-8.15.0/curl"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/libffi-sys-4.0.0/libffi"
                         "vendor/libz-sys-1.1.21/src/zlib"
                         "vendor/libz-sys-1.1.22/src/zlib"
                         "vendor/libmimalloc-sys-0.1.44/c_src/mimalloc"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/openssl-src-300.5.0+3.5.0/openssl"
                         "vendor/openssl-src-300.5.3+3.5.4/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.0+5.3.0-1-\
ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Use the packaged nghttp2.
             (for-each
              (lambda (ver)
                (let ((vendored-dir
                       (format #f "vendor/libnghttp2-sys-~a/nghttp2" ver))
                      (build-rs
                       (format #f "vendor/libnghttp2-sys-~a/build.rs" ver)))
                  (delete-file-recursively vendored-dir)
                  (delete-file build-rs)
                  (call-with-output-file build-rs
                    (lambda (port)
                      (format port "fn main() {~@
                           println!(\"cargo:rustc-link-lib=nghttp2\");~@
                           }~%")))))
              '("0.1.11+1.64.0"))
             ;; Adjust vendored dependency to explicitly use rustix with libc
             ;; backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.16.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml"
                            "vendor/tempfile-3.20.0/Cargo.toml"
                            "vendor/tempfile-3.21.0/Cargo.toml"
                            "vendor/tempfile-3.23.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\"")))))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'add-cc-shim-to-path))))))))

(define-public rust-1.93
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.92 "1.93.1"
          "07z02gq7x54sdbisbzcigk95h01qz0ckg52hz77g7jfrnd20l8sc")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/curl-sys-0.4.79+curl-8.12.0/curl"
                         "vendor/curl-sys-0.4.83+curl-8.15.0/curl"
                         "vendor/curl-sys-0.4.84+curl-8.17.0/curl"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/libffi-sys-4.0.0/libffi"
                         "vendor/libz-sys-1.1.21/src/zlib"
                         "vendor/libz-sys-1.1.23/src/zlib"
                         "vendor/libmimalloc-sys-0.1.44/c_src/mimalloc"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/openssl-src-300.5.0+3.5.0/openssl"
                         "vendor/openssl-src-300.5.4+3.5.4/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.1+5.3.0-1-\
ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Use the packaged nghttp2.
             (for-each
              (lambda (ver)
                (let ((vendored-dir
                       (format #f "vendor/libnghttp2-sys-~a/nghttp2" ver))
                      (build-rs
                       (format #f "vendor/libnghttp2-sys-~a/build.rs" ver)))
                  (delete-file-recursively vendored-dir)
                  (delete-file build-rs)
                  (call-with-output-file build-rs
                    (lambda (port)
                      (format port "fn main() {~@
                         println!(\"cargo:rustc-link-lib=nghttp2\");~@
                         }~%")))))
              '("0.1.11+1.64.0"))
             ;; Adjust vendored dependency to explicitly use rustix with libc
             ;; backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.16.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml"
                            "vendor/tempfile-3.20.0/Cargo.toml"
                            "vendor/tempfile-3.21.0/Cargo.toml"
                            "vendor/tempfile-3.23.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.94
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.93 "1.94.1"
          "056mbblkhn5ig62v09r6938qmnb09ygy32n62vvwvqqjbxi2l52c")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches (search-patches
                   "rust-1.94-clippy-fix-proc-macros-aux-race.patch"))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/curl-sys-0.4.79+curl-8.12.0/curl"
                         "vendor/curl-sys-0.4.84+curl-8.17.0/curl"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/libffi-sys-4.1.0/libffi"
                         "vendor/libz-sys-1.1.21/src/zlib"
                         "vendor/libz-sys-1.1.23/src/zlib"
                         "vendor/libmimalloc-sys-0.1.44/c_src/mimalloc"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/openssl-src-300.5.0+3.5.0/openssl"
                         "vendor/openssl-src-300.5.4+3.5.4/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.1+5.3.0-1-\
ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Use the packaged nghttp2.
             (for-each
              (lambda (ver)
                (let ((vendored-dir
                       (format #f "vendor/libnghttp2-sys-~a/nghttp2" ver))
                      (build-rs
                       (format #f "vendor/libnghttp2-sys-~a/build.rs" ver)))
                  (delete-file-recursively vendored-dir)
                  (delete-file build-rs)
                  (call-with-output-file build-rs
                    (lambda (port)
                      (format port "fn main() {~@
                         println!(\"cargo:rustc-link-lib=nghttp2\");~@
                         }~%")))))
              '("0.1.11+1.64.0"))
             ;; Adjust vendored dependency to explicitly use rustix with libc
             ;; backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.16.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml"
                            "vendor/tempfile-3.20.0/Cargo.toml"
                            "vendor/tempfile-3.21.0/Cargo.toml"
                            "vendor/tempfile-3.23.0/Cargo.toml"
                            "vendor/tempfile-3.24.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\"")))))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'install
               ;; Cannot use './x.py install' as it runs generate-copyright
               ;; which fails due to patched cargo checksums.
               ;; In this version we don't delete the .rmeta files; they are
               ;; now needed in order to build the next rust version.
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (cargo-out (assoc-ref outputs "cargo"))
                        (build (string-append "build/"
                                 ,(platform-rust-target
                                    (lookup-platform-by-target-or-system
                                     (or (%current-target-system)
                                         (%current-system)))))))
                   (with-directory-excursion build
                     (install-file "stage2/bin/rustc"
                                   (string-append out "/bin"))
                     (install-file "stage2-tools-bin/cargo"
                                   (string-append cargo-out "/bin"))
                     (for-each delete-file
                               (find-files "stage2/lib/rustlib"
                                           "^librustc_driver.*\\.so$"))
                     (copy-recursively "stage2/lib"
                                       (string-append out "/lib")))))))))))))

(define-public rust-1.95
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.94 "1.95.0"
          "05d53hj717ildhvm3rln7821r08nzbbfk72nqcvpb5j67sl856za")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/curl-sys-0.4.79+curl-8.12.0/curl"
                         "vendor/curl-sys-0.4.83+curl-8.15.0/curl"
                         "vendor/curl-sys-0.4.84+curl-8.17.0/curl"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/libffi-sys-4.1.0/libffi"
                         "vendor/libmimalloc-sys-0.1.44/c_src/mimalloc"
                         "vendor/libz-sys-1.1.21/src/zlib"
                         "vendor/libz-sys-1.1.23/src/zlib"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/openssl-src-300.5.0+3.5.0/openssl"
                         "vendor/openssl-src-300.5.4+3.5.4/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.1+5.3.0-1-\
ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Use the packaged nghttp2.
             (for-each
              (lambda (ver)
                (let ((vendored-dir
                       (format #f "vendor/libnghttp2-sys-~a/nghttp2" ver))
                      (build-rs
                       (format #f "vendor/libnghttp2-sys-~a/build.rs" ver)))
                  (delete-file-recursively vendored-dir)
                  (delete-file build-rs)
                  (call-with-output-file build-rs
                    (lambda (port)
                      (format port "fn main() {~@
                         println!(\"cargo:rustc-link-lib=nghttp2\");~@
                         }~%")))))
              '("0.1.11+1.64.0"))
             ;; Adjust vendored dependency to explicitly use rustix with libc
             ;; backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.16.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml"
                            "vendor/tempfile-3.20.0/Cargo.toml"
                            "vendor/tempfile-3.21.0/Cargo.toml"
                            "vendor/tempfile-3.23.0/Cargo.toml"
                            "vendor/tempfile-3.24.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.96
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.95 "1.96.1"
          "19xpk1pamj8v1gpi3ayfvh31smb38432ibqjmqw8b1j1ihcvbafh")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/curl-sys-0.4.79+curl-8.12.0/curl"
                         "vendor/curl-sys-0.4.84+curl-8.17.0/curl"
                         "vendor/curl-sys-0.4.87+curl-8.19.0/curl"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/libffi-sys-4.1.0/libffi"
                         "vendor/libmimalloc-sys-0.1.44/c_src/mimalloc"
                         "vendor/libz-sys-1.1.21/src/zlib"
                         "vendor/libz-sys-1.1.23/src/zlib"
                         "vendor/libz-sys-1.1.24/src/zlib"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/openssl-src-300.5.0+3.5.0/openssl"
                         "vendor/openssl-src-300.5.4+3.5.4/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.1+5.3.0-1-\
ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Use the packaged nghttp2.
             (for-each
              (lambda (ver)
                (let ((vendored-dir
                       (format #f "vendor/libnghttp2-sys-~a/nghttp2" ver))
                      (build-rs
                       (format #f "vendor/libnghttp2-sys-~a/build.rs" ver)))
                  (delete-file-recursively vendored-dir)
                  (delete-file build-rs)
                  (call-with-output-file build-rs
                    (lambda (port)
                      (format port "fn main() {~@
                         println!(\"cargo:rustc-link-lib=nghttp2\");~@
                         }~%")))))
              '("0.1.11+1.64.0"
                "0.1.13+1.68.1"))
             ;; Adjust vendored dependency to explicitly use rustix with libc
             ;; backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.16.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml"
                            "vendor/tempfile-3.20.0/Cargo.toml"
                            "vendor/tempfile-3.21.0/Cargo.toml"
                            "vendor/tempfile-3.23.0/Cargo.toml"
                            "vendor/tempfile-3.27.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.97
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.96 "1.97.1"
          "178sxzghqr09awgjr4f1pqzdc779ahsx0l9svp0gvjskki12nb32")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches '())
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/curl-sys-0.4.79+curl-8.12.0/curl"
                         "vendor/curl-sys-0.4.87+curl-8.19.0/curl"
                         "vendor/curl-sys-0.4.88+curl-8.20.0/curl"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/libffi-sys-4.1.0/libffi"
                         "vendor/libmimalloc-sys-0.1.44/c_src/mimalloc"
                         "vendor/libz-sys-1.1.21/src/zlib"
                         "vendor/libz-sys-1.1.23/src/zlib"
                         "vendor/libz-sys-1.1.28/src/zlib"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/openssl-src-300.5.0+3.5.0/openssl"
                         "vendor/openssl-src-300.6.0+3.6.2/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.1+5.3.0-1-\
ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Use the packaged nghttp2.
             (for-each
              (lambda (ver)
                (let ((vendored-dir
                       (format #f "vendor/libnghttp2-sys-~a/nghttp2" ver))
                      (build-rs
                       (format #f "vendor/libnghttp2-sys-~a/build.rs" ver)))
                  (delete-file-recursively vendored-dir)
                  (delete-file build-rs)
                  (call-with-output-file build-rs
                    (lambda (port)
                      (format port "fn main() {~@
                         println!(\"cargo:rustc-link-lib=nghttp2\");~@
                         }~%")))))
              '("0.1.11+1.64.0"
                "0.1.13+1.68.1"))
             ;; Adjust vendored dependency to explicitly use rustix with libc
             ;; backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.16.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml"
                            "vendor/tempfile-3.20.0/Cargo.toml"
                            "vendor/tempfile-3.23.0/Cargo.toml"
                            "vendor/tempfile-3.27.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.98
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.97 "1.98.1"
          "0np6bv8y3njcq5hbnf1kcfd3lg8116sc2g64g9n4si1jgf8qp7yw")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/curl-sys-0.4.79+curl-8.12.0/curl"
                         "vendor/curl-sys-0.4.87+curl-8.19.0/curl"
                         "vendor/curl-sys-0.4.90+curl-8.21.0/curl"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/libffi-sys-4.1.0/libffi"
                         "vendor/libmimalloc-sys-0.1.49/c_src/mimalloc"
                         "vendor/libz-sys-1.1.21/src/zlib"
                         "vendor/libz-sys-1.1.23/src/zlib"
                         "vendor/libz-sys-1.1.29/src/zlib"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/openssl-src-300.5.0+3.5.0/openssl"
                         "vendor/openssl-src-300.6.1+3.6.3/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.1+5.3.0-1-\
ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Use the packaged nghttp2.
             (for-each
              (lambda (ver)
                (let ((vendored-dir
                       (format #f "vendor/libnghttp2-sys-~a/nghttp2" ver))
                      (build-rs
                       (format #f "vendor/libnghttp2-sys-~a/build.rs" ver)))
                  (delete-file-recursively vendored-dir)
                  (delete-file build-rs)
                  (call-with-output-file build-rs
                    (lambda (port)
                      (format port "fn main() {~@
                         println!(\"cargo:rustc-link-lib=nghttp2\");~@
                         }~%")))))
              '("0.1.11+1.64.0"
                "0.1.13+1.68.1"))
             ;; Adjust vendored dependency to explicitly use rustix with libc
             ;; backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.16.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml"
                            "vendor/tempfile-3.20.0/Cargo.toml"
                            "vendor/tempfile-3.23.0/Cargo.toml"
                            "vendor/tempfile-3.27.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define (make-ignore-test-list strs)
  "Function to make creating a list to ignore tests a bit easier."
  (map (lambda (str)
    `((,str) (string-append "#[ignore]\n" ,str)))
    strs))

;;; Note: Only the latest version of Rust is supported and tested.  The
;;; intermediate rusts are built for bootstrapping purposes and should not
;;; be relied upon.  This is to ease maintenance and reduce the time
;;; required to build the full Rust bootstrap chain.
;;;
;;; Here we take the latest included Rust, make it public, and re-enable tests
;;; and extra components such as rustfmt.
(define-public rust
  (let ((base-rust rust-1.98))
    (package
      (inherit base-rust)
      (properties (append
                    (alist-delete 'hidden? (package-properties base-rust))
                    ;; Keep in sync with the llvm used to build rust.
                    (clang-compiler-cpu-architectures "21")))
      (outputs (cons* "rust-src" "tools" (package-outputs base-rust)))
      (arguments
       (substitute-keyword-arguments
         (strip-keyword-arguments '(#:tests?)
                                  (package-arguments base-rust))
         ((#:modules modules)
          (cons '(srfi srfi-26) modules))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'patch-shebangs-in-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (with-directory-excursion
                   "src/tools/rust-analyzer/crates/parser/test_data"
                   (substitute* '("lexer/ok/shebang_frontmatter.rast"
                                  "parser/inline/ok/frontmatter.rast")
                     (("/usr/bin/env cargo")
                      (search-input-file inputs "bin/cargo")))
                   (substitute* "lexer/ok/single_line_comments.rast"
                     (("/usr/bin/env bash")
                      (search-input-file inputs "bin/bash"))))))
             (add-after 'unpack 'disable-tests-using-cargo-publish
               (lambda _
                 (substitute* "src/tools/cargo/tests/testsuite/install.rs"
                   ,@(make-ignore-test-list
                      '("fn failed_install_retains_temp_directory")))))
             ,@(if (target-riscv64?)
                   ;; Keep this phase separate so it can be adjusted without needing
                   ;; to adjust the skipped tests on other architectures.
                   `((add-after 'unpack 'disable-tests-broken-on-riscv64
                       (lambda _
                         (with-directory-excursion "src/tools/cargo/tests/testsuite"
                           (substitute* "build.rs"
                             ,@(make-ignore-test-list
                                 '("fn uplift_dwp_of_bin_on_linux")))
                           (substitute* "cache_lock.rs"
                             ,@(make-ignore-test-list
                                 '("fn multiple_shared"
                                   "fn multiple_download"
                                   "fn download_then_mutate"
                                   "fn mutate_err_is_atomic")))
                           (substitute* "global_cache_tracker.rs"
                             ,@(make-ignore-test-list
                                 '("fn package_cache_lock_during_build"))))
                         (with-directory-excursion "src/tools/clippy/tests"
                           ;; `"vectorcall"` is not a supported ABI for the current target
                           (delete-file "ui/missing_const_for_fn/could_be_const.rs")
                           (substitute* "missing-test-files.rs"
                             ,@(make-ignore-test-list
                                '("fn test_missing_tests")))))))
                   `())
             (add-after 'unpack 'disable-tests-requiring-network-access
               (lambda _
                 (substitute* "src/tools/cargo/tests/testsuite/git.rs"
                   ,@(make-ignore-test-list
                      '("fn dep_with_scp_like_submodule_url")))
                 (substitute* "src/tools/cargo/tests/testsuite/git_auth.rs"
                   ,@(make-ignore-test-list
                      '("fn net_err_suggests_fetch_with_cli")))))
             (add-after 'unpack 'patch-process-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (with-directory-excursion "library/std/src"
                     (substitute* "process/tests.rs"
                       (("\"/bin/sh\"")
                        (string-append "\"" bash "/bin/sh\"")))
                     ;; The three tests which are known to fail upstream on QEMU
                     ;; emulation on aarch64 and riscv64 also fail on x86_64 in
                     ;; Guix's build system.  Skip them on all builds.
                     (substitute* "sys/process/unix/common/tests.rs"
                       ;; We can't use make-ignore-test-list because we will get
                       ;; build errors due to the double [ignore] block.
                       (("target_arch = \"arm\"" arm)
                        (string-append "target_os = \"linux\",\n"
                                       "        " arm)))))))
             (add-after 'unpack 'disable-interrupt-tests
               (lambda _
                 ;; This test hangs in the build container; disable it.
                 (substitute* "src/tools/cargo/tests/testsuite/death.rs"
                   ,@(make-ignore-test-list
                      '("fn ctrl_c_kills_everyone")))))
             (add-after 'unpack 'adjust-rpath-values
               ;; This adds %output:out to rpath, allowing us to install utilities in
               ;; different outputs while reusing the shared libraries.
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (substitute* "src/bootstrap/src/core/builder/cargo.rs"
                     ((" = rpath.*" all)
                      (string-append all
                                     "                "
                                     "self.rustflags.arg(\"-Clink-args=-Wl,-rpath="
                                     out "/lib\");\n"))))))
             (add-after 'unpack 'unpack-profiler-rt
               ;; Copy compiler-rt sources to where libprofiler_builtins looks
               ;; for its vendored copy.
               (lambda* (#:key inputs #:allow-other-keys)
                 (mkdir-p "src/llvm-project/compiler-rt")
                 (copy-recursively
                  (search-input-directory inputs "/compiler-rt")
                  "src/llvm-project/compiler-rt")))
             (add-after 'unpack 'unpack-libunwind
               (lambda* (#:key inputs #:allow-other-keys)
                 (mkdir-p "src/llvm-project/libunwind")
                 (copy-recursively
                  (search-input-directory inputs "/libunwind")
                  "src/llvm-project/libunwind")))
             (replace 'patch-cargo-checksums
               (lambda _
                 (substitute*
                   (append '("Cargo.lock"
                             "src/bootstrap/Cargo.lock")
                           (find-files "compiler" "Cargo.lock")
                           (find-files "library" "Cargo.lock")
                           (find-files "src/doc" "Cargo.lock")
                           (remove
                             ;; Don't mess with the lock files in the Cargo
                             ;; testsuite; it messes up the tests.
                             (cut string-contains <>
                                  "cargo/tests/testsuite")
                             (find-files "src/tools" "Cargo.lock")))
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor")))
             (add-after 'configure 'enable-profiling
               (lambda _
                 (substitute* "config.toml"
                   (("^profiler =.*$") "")
                   (("\\[build\\]") "\n[build]\nprofiler = true\n"))))
             (add-after 'configure 'add-gdb-to-config
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((gdb (assoc-ref inputs "gdb")))
                   (substitute* "config.toml"
                     (("^python =.*" all)
                      (string-append all
                                     "gdb = \"" gdb "/bin/gdb\"\n"))))))
             (replace 'build
               ;; Phase overridden to also build more tools.
               (lambda* (#:key parallel-build? #:allow-other-keys)
                 (let ((job-spec (string-append
                                  "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
                   (invoke "./x.py" job-spec "build"
                           "library/std" ;rustc
                           "src/tools/cargo"
                           "src/tools/clippy"
                           "src/tools/llvm-bitcode-linker"
                           "src/tools/rust-analyzer"
                           "src/tools/rustfmt"))))
             (replace 'check
               ;; Phase overridden to also test more tools.
               (lambda* (#:key tests? parallel-build? #:allow-other-keys)
                 (when tests?
                   (let ((job-spec (string-append
                                    "-j" (if parallel-build?
                                             (number->string (parallel-job-count))
                                             "1"))))
                     (invoke "./x.py" job-spec "test" "-vv"
                             "library/std"
                             "src/tools/cargo"
                             "src/tools/clippy"
                             "src/tools/rust-analyzer"
                             "src/tools/rustfmt")))))
             (replace 'install
               ;; Phase overridden to also install more tools.
               (lambda* (#:key outputs #:allow-other-keys)
                 ;; Install rustc, std, and llvm-bitcode-linker.
                 ;; rust-src is handled separately in 'install-rust-src'.
                 (invoke "./x.py" "install" "compiler/rustc" "library/std"
                         "llvm-bitcode-linker")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'cargo' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "cargo"))))
                 (invoke "./x.py" "install" "cargo")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'tools' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "tools"))))
                 (invoke "./x.py" "install" "clippy")
                 (invoke "./x.py" "install" "rust-analyzer")
                 (invoke "./x.py" "install" "rustfmt")))
             (add-before 'patch-cargo-checksums 'save-old-library-manifest
               (lambda _
                 (copy-file "library/Cargo.lock" ".old-library-manifest")))
             (add-after 'install 'install-rust-src
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "rust-src"))
                       (dest "/lib/rustlib/src/rust"))
                   (mkdir-p (string-append out dest))
                   (copy-recursively "library" (string-append out dest "/library"))
                   ;; rust-analyzer needs the original checksums; otherwise
                   ;; it fails to cargo manifest in the stdlib, and then
                   ;; analysis/inference involving stdlib structs doesn't work.
                   ;;
                   ;; For example, in the following trivial program:
                   ;;
                   ;; fn main() {
                   ;;     let x = Vec::<usize>::new();
                   ;; }
                   ;;
                   ;; rust-analyzer since version 1.82 can't infer
                   ;; the type of x unless the following line is present.
                   (copy-file ".old-library-manifest"
                              (string-append out dest "/library/Cargo.lock"))
                   (copy-recursively "src" (string-append out dest "/src")))))
             (add-before 'install 'remove-uninstall-script
               (lambda _
                 ;; Don't install the uninstall script.  It has no use
                 ;; on Guix and it retains a reference to the host's bash.
                 (substitute* "src/tools/rust-installer/install-template.sh"
                   (("install_uninstaller \"") "# install_uninstaller \""))))
             (add-after 'install-rust-src 'dont-reference-previous-cargo-version
               (lambda* (#:key outputs #:allow-other-keys)
                 (with-directory-excursion
                   (string-append (assoc-ref outputs "rust-src")
                                  "/lib/rustlib/src/rust/src/tools/")
                   (substitute* (find-files "." "\\.(rs|rast)$")
                     (("#!.*/bin/cargo")
                      (string-append "#!" (assoc-ref outputs "cargo")
                                     "/bin/cargo"))))))
             (add-after 'install-rust-src 'wrap-rust-analyzer
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((bin (string-append (assoc-ref outputs "tools") "/bin")))
                   (rename-file (string-append bin "/rust-analyzer")
                                (string-append bin "/.rust-analyzer-real"))
                   (call-with-output-file (string-append bin "/rust-analyzer")
                     (lambda (port)
                       (format port "#!~a
if test -z \"${RUST_SRC_PATH}\";then export RUST_SRC_PATH=~S;fi;
exec -a \"$0\" \"~a\" \"$@\""
                               (which "bash")
                               (string-append (assoc-ref outputs "rust-src")
                                              "/lib/rustlib/src/rust/library")
                               (string-append bin "/.rust-analyzer-real"))))
                   (chmod (string-append bin "/rust-analyzer") #o755))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base-rust)
        (prepend
          ;; Keep in sync with the llvm used to build rust.
          (package-source clang-runtime-21)
          ;; Add test inputs.
          gdb/pinned
          git-minimal/pinned
          procps)))
      (native-search-paths
       (cons*
         ;; For HTTPS access, Cargo reads from a single-file certificate
         ;; specified with $CARGO_HTTP_CAINFO. See
         ;; https://doc.rust-lang.org/cargo/reference/environment-variables.html
         (search-path-specification
          (variable "CARGO_HTTP_CAINFO")
          (file-type 'regular)
          (separator #f)              ;single entry
          (files '("etc/ssl/certs/ca-certificates.crt")))
         ;; Make the sources discoverable not just when wrapped.
         (search-path-specification
          (variable "RUST_SRC_PATH")
          (separator #f)              ;single entry
          (files '("lib/rustlib/src/rust/library")))
         ;; rustc invokes gcc, so we need to set its search paths accordingly.
         %gcc-search-paths)))))

(define*-public (make-rust-sysroot target)
  (make-rust-sysroot/implementation target rust))

(define make-rust-sysroot/implementation
  (mlambda (target base-rust)
    (unless (platform-rust-target (lookup-platform-by-target target))
      (raise
       (condition
        (&package-unsupported-target-error
         (package base-rust)
         (target target)))))

    (package
      (inherit base-rust)
      (name (string-append "rust-sysroot-for-" target))
      (outputs '("out"))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:tests? _ #f) #f)           ; This package for cross-building.
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'unbundle-xz
                (lambda _
                  (delete-file-recursively "vendor/lzma-sys-0.1.20/xz-5.2")
                  ;; Remove the option of using the static library.
                  ;; This is necessary for building the sysroot.
                  (substitute* "vendor/lzma-sys-0.1.20/build.rs"
                    (("!want_static && ") ""))))
              #$@(if (target-mingw? target)
                     `((add-after 'set-env 'patch-for-mingw
                         (lambda* (#:key inputs #:allow-other-keys)
                           (let* ((arch ,(string-take target
                                                      (string-index target #\-)))
                                  (mingw (assoc-ref inputs
                                                    (string-append "mingw-w64-" arch
                                                                   "-winpthreads"))))
                             (setenv "LIBRARY_PATH"
                                     (string-join
                                       (delete
                                         (string-append mingw "/lib")
                                         (string-split (getenv "LIBRARY_PATH") #\:))
                                       ":"))
                             (setenv "CPLUS_INCLUDE_PATH"
                                     (string-join
                                       (delete
                                         (string-append mingw "/include")
                                         (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                                       ":"))))))
                     `())
              (replace 'set-env
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "SHELL" (which "sh"))
                  (setenv "CONFIG_SHELL" (which "sh"))
                  (setenv "CC" (which "gcc"))
                  ;; The Guix LLVM package installs only shared libraries.
                  (setenv "LLVM_LINK_SHARED" "1")

                  (setenv "CROSS_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
                  (setenv "CROSS_CPLUS_INCLUDE_PATH" (getenv "CPLUS_INCLUDE_PATH"))
                  (when (assoc-ref inputs (string-append "glibc-cross-" #$target))
                    (setenv "LIBRARY_PATH"
                            (string-join
                             (delete
                              (string-append
                               (assoc-ref inputs
                                          (string-append "glibc-cross-" #$target))
                               "/lib")
                              (string-split (getenv "LIBRARY_PATH") #\:))
                             ":"))
                    (setenv "CPLUS_INCLUDE_PATH"
                            (string-join
                             (delete
                              (string-append
                               (assoc-ref inputs
                                          (string-append "glibc-cross-" #$target))
                               "/include")
                              (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                             ":")))))
              (replace 'configure
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (target-cc
                          (search-input-file
                           inputs (string-append "/bin/" #$(cc-for-target target)))))
                    (call-with-output-file "config.toml"
                      (lambda (port)
                        (display (string-append "
[llvm]
[build]
cargo = \"" (search-input-file inputs "/bin/cargo") "\"
rustc = \"" (search-input-file inputs "/bin/rustc") "\"
docs = false
python = \"" (which "python") "\"
vendor = true
submodules = false
target = [\"" #$(platform-rust-target (lookup-platform-by-target target)) "\"]
[install]
prefix = \"" out "\"
sysconfdir = \"etc\"
[rust]
debug = false
jemalloc = false
default-linker = \"" target-cc "\"
channel = \"stable\"
[target." #$(platform-rust-target (lookup-platform-by-system (%current-system))) "]
# These are all native tools
llvm-config = \"" (search-input-file inputs "/bin/llvm-config") "\"
linker = \"" (which "gcc") "\"
cc = \"" (which "gcc") "\"
cxx = \"" (which "g++") "\"
ar = \"" (which "ar") "\"
[target." #$(platform-rust-target (lookup-platform-by-target target)) "]
llvm-config = \"" (search-input-file inputs "/bin/llvm-config") "\"
linker = \"" target-cc "\"
cc = \"" target-cc "\"
cxx = \"" (search-input-file inputs (string-append "/bin/" #$(cxx-for-target target))) "\"
ar = \"" (search-input-file inputs (string-append "/bin/" #$(ar-for-target target))) "\"
[dist]
") port))))))
              (replace 'build
                ;; Phase overridden to build the necessary directories.
                (lambda* (#:key parallel-build? #:allow-other-keys)
                  (let ((job-spec (string-append
                                   "-j" (if parallel-build?
                                            (number->string (parallel-job-count))
                                            "1"))))
                    ;; This works for us with the --sysroot flag
                    ;; and then we can build ONLY library/std
                    (invoke "./x.py" job-spec "build" "library/std"))))
              (replace 'install
                (lambda _
                  (invoke "./x.py" "install" "library/std")))
              (delete 'enable-profiling)
              (delete 'install-rust-src)
              (delete 'dont-reference-previous-cargo-version)
              (delete 'wrap-rust-analyzer)
              (delete 'wrap-rustc)))))
      (inputs
       (modify-inputs inputs
         (prepend xz)))                 ; for lzma-sys
      (propagated-inputs
       (if (target-mingw? target)
           (modify-inputs propagated-inputs
             (prepend
              (make-mingw-w64
                (string-take target (string-index target #\-))
                #:with-winpthreads? #t)))
           (package-propagated-inputs base-rust)))
      (native-inputs
       (modify-inputs native-inputs
         (prepend (cross-gcc target
                             #:libc (cross-libc target)))
         (prepend (if (target-mingw? target)
                      (make-mingw-w64
                        (string-take target (string-index target #\-))
                        #:with-winpthreads? #t)
                      (cross-libc target)))
         (prepend (cross-binutils target))))
      (properties
       `((hidden? . #t) ,(package-properties base-rust))))))

(define-public rust-sysroot-for-wasm32-unknown-unknown
  (package
    (inherit (make-rust-sysroot "i686-linux-gnu"))
    (name "rust-sysroot-for-wasm32-unknown-unknown")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'set-env
              (lambda _
                (setenv "SHELL" (which "sh"))
                (setenv "CONFIG_SHELL" (which "sh"))
                (setenv "CC" (which "clang"))
                ;; The Guix LLVM package installs only shared libraries.
                (setenv "LLVM_LINK_SHARED" "1")))
            (replace 'configure
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (call-with-output-file "config.toml"
                    (lambda (port)
                      (display (string-append "
[llvm]
[build]
cargo = \"" (search-input-file inputs "/bin/cargo") "\"
rustc = \"" (search-input-file inputs "/bin/rustc") "\"
docs = false
python = \"" (which "python") "\"
vendor = true
submodules = false
target = [\"wasm32-unknown-unknown\"]
[install]
prefix = \"" (assoc-ref outputs "out") "\"
sysconfdir = \"etc\"
[rust]
debug = false
jemalloc = false
default-linker = \"lld\"
channel = \"stable\"
[target." #$(platform-rust-target (lookup-platform-by-system (%current-system))) "]
# These are all native tools
llvm-config = \"" (search-input-file inputs "/bin/llvm-config") "\"
linker = \"" (which "gcc") "\"
cc = \"" (which "gcc") "\"
cxx = \"" (which "g++") "\"
ar = \"" (which "ar") "\"
[target.wasm32-unknown-unknown]
llvm-config = \"" (search-input-file inputs "/bin/llvm-config") "\"
linker = \"lld\"
cc = \"clang\"
cxx = \"clang++\"
ar = \"ar\"
[dist]
") port)))))))))
    ;; Use native-inputs of base rust.
    (native-inputs (package-native-inputs rust))))

(define-public rust-analyzer
  (package
    (name "rust-analyzer")
    (version (package-version rust))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
       #:modules '((guix build utils))
       #:builder
       #~(begin
           (use-modules (guix build utils))
           (let ((rust (assoc-ref %build-inputs "rust")))
             (install-file (string-append rust "/bin/rust-analyzer")
                           (string-append #$output "/bin"))
             (copy-recursively (string-append rust "/share")
                               (string-append #$output "/share"))))))
    (inputs
     (list (list rust "tools")))
    (home-page "https://rust-analyzer.github.io/")
    (synopsis "Experimental Rust compiler front-end for IDEs")
    (description "Rust-analyzer is a modular compiler frontend for the Rust
language.  It is a part of a larger rls-2.0 effort to create excellent IDE
support for Rust.")
    (license (list license:expat license:asl2.0))))
