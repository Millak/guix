;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017, 2019-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020, 2021 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Jim Newsome <jnewsome@torproject.org>
;;; Copyright © 2022 Mark H Weaver <mhw@netris.org>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

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

(define* (nix-system->gnu-triplet-for-rust
          #:optional (system (%current-system)))
  (match system
    ("x86_64-linux"   "x86_64-unknown-linux-gnu")
    ("i686-linux"     "i686-unknown-linux-gnu")
    ("armhf-linux"    "armv7-unknown-linux-gnueabihf")
    ("aarch64-linux"  "aarch64-unknown-linux-gnu")
    ("mips64el-linux" "mips64el-unknown-linux-gnuabi64")
    ("riscv64-linux"  "riscv64gc-unknown-linux-gnu")
    (_                (nix-system->gnu-triplet system))))

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
    (native-inputs
     (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                    (alist-replace "rustc-bootstrap" (list base-rust)
                                   (package-native-inputs base-rust))))))

;;; Note: mrustc's only purpose is to be able to bootstap Rust; it's designed
;;; to be used in source form.
(define %mrustc-commit "597593aba86fa2edbea80c6e09f0b1b2a480722d")
(define %mrustc-source
  (let* ((version "0.10")
         (commit %mrustc-commit)
         (revision "2")
         (name "mrustc"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/thepowersgang/mrustc")
            (commit commit)))
      (file-name (git-file-name name (git-version version revision commit)))
      (sha256
       (base32
        "09rvm3zgx1d86gippl8qzh13m641ynbw9q0zsc90g0h1khd3z3b6"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Drastically reduces memory and build time requirements
          ;; by disabling debug by default.
          (substitute* (find-files "." "Makefile")
            (("-g ") "")))))))

;;; Rust 1.54 is special in that it is built with mrustc, which shortens the
;;; bootstrap path.
(define rust-bootstrap
  (package
    (name "rust")
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (rust-uri version))
       (sha256 (base32 "0xk9dhfff16caambmwij67zgshd8v9djw6ha0fnnanlv7rii31dc"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file-recursively
                     '("src/llvm-project"))
           ;; Remove vendored dynamically linked libraries.
           ;; find . -not -type d -executable -exec file {} \+ | grep ELF
           (delete-file "vendor/vte/vim10m_match")
           (delete-file "vendor/vte/vim10m_table")
           ;; Also remove the bundled (mostly Windows) libraries.
           ;; find vendor -not -type d -exec file {} \+ | grep PE32
           (for-each delete-file
                     (find-files "vendor" ".*\\.(a|dll|exe|lib)$"))))
       (patches (search-patches "rustc-1.54.0-src.patch"))
       (patch-flags '("-p0"))))         ;default is -p1
    (outputs '("out" "cargo"))
    (properties '((timeout . 72000)           ;20 hours
                  (max-silent-time . 18000))) ;5 hours (for armel)
    (build-system gnu-build-system)
    (inputs
     `(("libcurl" ,curl)
       ("llvm" ,llvm-13)
       ("openssl" ,openssl-1.1)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ;; Required for the libstd sources.
       ("mrustc-source" ,%mrustc-source)))
    (arguments
     `(#:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'
       #:modules ((guix build cargo-utils)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:test-target "test"
       ;; Rust's own .so library files are not found in any RUNPATH, but
       ;; that doesn't seem to cause issues.
       #:validate-runpath? #f
       ;; Most of the build is single-threaded. This also improves the
       ;; build time on machines with "only" 8GB of RAM.
       #:parallel-build? ,(target-x86-64?)
       #:make-flags
       (list ,(string-append "RUSTC_TARGET="
                             (or (%current-target-system)
                                 (nix-system->gnu-triplet-for-rust)))
             ,(string-append "RUSTC_VERSION=" version)
             ,(string-append "MRUSTC_TARGET_VER="
                             (version-major+minor version))
             ;; mrustc expects a C11 compatible compiler. Use the default
             ;; C language dialect from the GCC-10 compiler.
             ;; This is necessary for some architectures.
             "CFLAGS=-std=gnu11"
             "OUTDIR_SUF=")           ;do not add version suffix to output dir
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-reference-to-cc
           ;; This prevents errors like 'error: linker `cc` not found' when
           ;; "cc" is not found on PATH.
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               (substitute* (find-files "." "^link.rs$")
                 (("\"cc\".as_ref")
                  (format #f "~s.as_ref" (string-append gcc "/bin/gcc")))))))
         (add-after 'unpack 'setup-mrustc-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "mrustc-source") "../mrustc")
             ;; The Makefile of mrustc expects the sources directory of rustc
             ;; to be at this location, and it simplifies things to make it
             ;; so.
             (symlink (getcwd)
                      (string-append "../mrustc/rustc-" ,version "-src"))
             (with-output-to-file "dl-version"
               (lambda _
                 (format #t "~a~%"
                         ,version)))))
         (add-after 'setup-mrustc-sources 'patch-makefiles
           ;; This disables building the (unbundled) LLVM.
           (lambda* (#:key inputs parallel-build? #:allow-other-keys)
             (let ((llvm (assoc-ref inputs "llvm")))
               (with-directory-excursion "../mrustc"
                 (substitute* '("minicargo.mk"
                                "run_rustc/Makefile")
                   ;; Use the system-provided LLVM.
                   (("LLVM_CONFIG [:|?]= .*")
                    (string-append "LLVM_CONFIG := " llvm "/bin/llvm-config\n")))
                 (substitute* "minicargo.mk"
                   ;; Do not try to fetch sources from the Internet.
                   (("@curl.*") ""))
                 (substitute* "Makefile"
                   ;; Patch date and git obtained version information.
                   ((" -D VERSION_GIT_FULLHASH=.*")
                    (string-append
                     " -D VERSION_GIT_FULLHASH=\\\"" ,%mrustc-commit "\\\""
                     " -D VERSION_GIT_BRANCH=\\\"master\\\""
                     " -D VERSION_GIT_SHORTHASH=\\\""
                     ,(string-take %mrustc-commit 7) "\\\""
                     " -D VERSION_BUILDTIME="
                     "\"\\\"Thu, 01 Jan 1970 00:00:01 +0000\\\"\""
                     " -D VERSION_GIT_ISDIRTY=0\n")))
                 (substitute* "minicargo.mk"
                   ;; Do not try to fetch sources from the Internet.
                   (("\\$\\(MINICARGO\\) \\$\\(RUSTC_SRC_DL\\)")
                    "$(MINICARGO)"))
                 (substitute* "run_rustc/Makefile"
                   (("[$]Vtime ")
                    "$V ")
                   ;; Unlock the number of parallel jobs for cargo.
                   (("-j [[:digit:]]+ ")
                    "")
                   ;; Patch the shebang of a generated wrapper for rustc
                   (("#!/bin/sh")
                    (string-append "#!" (which "sh"))))
                 (substitute* "run_rustc/rustc_proxy.sh"
                   (("#!/bin/sh")
                    (string-append "#!" (which "sh"))))))))
         (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
           (lambda* _
             (substitute* "Cargo.lock"
               (("(checksum = )\".*\"" all name)
                (string-append name "\"" ,%cargo-reference-hash "\"")))
             (generate-all-checksums "vendor")))
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
             (let* ((src-root (getcwd))
                    (job-count (if parallel-build?
                                   (parallel-job-count)
                                   1)))
               ;; Adapted from:
               ;; https://github.com/dtolnay/bootstrap/blob/master/build-1.54.0.sh.
               (chdir "../mrustc")
               ;; Use PARLEVEL since both minicargo and mrustc use it
               ;; to set the level of parallelism.
               (setenv "PARLEVEL" (number->string job-count))
               (display "Building mrustc...\n")
               (apply invoke "make" make-flags)

               ;; This doesn't seem to build anything, but it
               ;; sets additional minicargo flags.
               (display "Building RUSTCSRC...\n")
               (apply invoke "make" "RUSTCSRC" make-flags)

               ;; This probably doesn't need to be called explicitly.
               (display "Building LIBS...\n")
               (apply invoke "make" "-f" "minicargo.mk" "LIBS" make-flags)

               (display "Building rustc...\n")
               (apply invoke "make" "-f" "minicargo.mk" "output/rustc"
                      make-flags)

               (display "Building cargo...\n")
               (apply invoke "make" "-f" "minicargo.mk" "output/cargo"
                      make-flags)

               ;; This one isn't listed in the build script.
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
                    (gnu-triplet ,(or (%current-target-system)
                                      (nix-system->gnu-triplet-for-rust)))
                    (system-lib-prefix (string-append lib "/rustlib/"
                                                      gnu-triplet "/lib")))
               (mkdir-p (dirname rustc))
               (copy-file "run_rustc/output/prefix/bin/rustc_binary" rustc)
               (wrap-program rustc
                 `("LD_LIBRARY_PATH" = (,system-lib-prefix)))
               (mkdir-p lib)
               (copy-recursively "run_rustc/output/prefix/lib" lib)
               (install-file "run_rustc/output/prefix/bin/cargo" cargo-bin)))))))
    (synopsis "Compiler for the Rust programming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://github.com/thepowersgang/mrustc")

    ;; The intermediate generated code is known to be inefficient and
    ;; therefore the build process needs 8GB of RAM while building.
    ;; It may support i686 soon:
    ;; <https://github.com/thepowersgang/mrustc/issues/78>.
    ;; XXX: The rust bootstrap is currently broken on riscv64,
    ;; remove it until this is fixed.
    (supported-systems '("x86_64-linux" "aarch64-linux"))

    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define rust-1.55
  (package
    (name "rust")
    (version "1.55.0")
    (source
     (origin
       (method url-fetch)
       (uri (rust-uri version))
       (sha256 (base32 "07l28f7grdmi65naq71pbmvdd61hwcpi40ry7kp7dy7m233rldxj"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file-recursively
                     '("src/llvm-project"
                       "vendor/tikv-jemalloc-sys/jemalloc"))
           ;; Remove vendored dynamically linked libraries.
           ;; find . -not -type d -executable -exec file {} \+ | grep ELF
           (delete-file "vendor/vte/vim10m_match")
           (delete-file "vendor/vte/vim10m_table")
           ;; Also remove the bundled (mostly Windows) libraries.
           ;; find vendor -not -type d -exec file {} \+ | grep PE32
           (for-each delete-file
                     (find-files "vendor" ".*\\.(a|dll|exe|lib)$"))
           ;; Add support for riscv64-linux.
           (substitute* "vendor/tikv-jemallocator/src/lib.rs"
             (("    target_arch = \"s390x\"," all)
              (string-append all "\n    target_arch = \"riscv64\",")))))))
    (outputs '("out" "cargo"))
    (properties '((timeout . 72000)           ;20 hours
                  (max-silent-time . 18000))) ;5 hours (for armel)
    (build-system gnu-build-system)
    (arguments
     `(#:validate-runpath? #f
       ;; Only the final Rust is tested, not the intermediate bootstrap ones,
       ;; for performance and simplicity.
       #:tests? #f
       #:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'
       #:modules ((guix build cargo-utils)
                  (guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
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
                "linker.env(\"LC_ALL\", \"en_US.UTF-8\");"))))
         (add-after 'unpack 'add-cc-shim-to-path
           (lambda _
             (mkdir-p "/tmp/bin")
             (symlink (which "gcc") "/tmp/bin/cc")
             (setenv "PATH" (string-append "/tmp/bin:" (getenv "PATH")))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gcc (assoc-ref inputs "gcc"))
                    (python (assoc-ref inputs "python"))
                    (binutils (assoc-ref inputs "binutils"))
                    (rustc (assoc-ref inputs "rustc-bootstrap"))
                    (cargo (assoc-ref inputs "cargo-bootstrap"))
                    (llvm (assoc-ref inputs "llvm"))
                    (jemalloc (assoc-ref inputs "jemalloc")))
               ;; The compiler is no longer directly built against jemalloc, but
               ;; rather via the jemalloc-sys crate (which vendors the jemalloc
               ;; source). To use jemalloc we must enable linking to it (otherwise
               ;; it would use the system allocator), and set an environment
               ;; variable pointing to the compiled jemalloc.
               (setenv "JEMALLOC_OVERRIDE"
                       (search-input-file inputs
                                          "/lib/libjemalloc_pic.a"))
               (call-with-output-file "config.toml"
                 (lambda (port)
                   (display (string-append "
[llvm]
[build]
cargo = \"" cargo "/bin/cargo" "\"
rustc = \"" rustc "/bin/rustc" "\"
docs = false
python = \"" python "/bin/python" "\"
vendor = true
submodules = false
[install]
prefix = \"" out "\"
sysconfdir = \"etc\"
[rust]
debug=false
jemalloc=true
default-linker = \"" gcc "/bin/gcc" "\"
channel = \"stable\"
rpath = true
[target." ,(nix-system->gnu-triplet-for-rust) "]
llvm-config = \"" llvm "/bin/llvm-config" "\"
cc = \"" gcc "/bin/gcc" "\"
cxx = \"" gcc "/bin/g++" "\"
ar = \"" binutils "/bin/ar" "\"
[dist]
") port))))))
         (replace 'build
           ;; The standard library source location moved in this release.
           (lambda* (#:key parallel-build? #:allow-other-keys)
             (let ((job-spec (string-append
                              "-j" (if parallel-build?
                                       (number->string (parallel-job-count))
                                       "1"))))
               (invoke "./x.py" job-spec "build" "--stage=1"
                       "library/std"
                       "src/tools/cargo"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (cargo-out (assoc-ref outputs "cargo"))
                    (gnu-triplet ,(or (%current-target-system)
                                      (nix-system->gnu-triplet-for-rust)))
                    (build (string-append "build/" gnu-triplet)))
               ;; Manually do the installation instead of calling './x.py
               ;; install', as that is slow and needlessly rebuilds some
               ;; things.
               (install-file (string-append build "/stage1/bin/rustc")
                             (string-append out "/bin"))
               (copy-recursively (string-append build "/stage1/lib")
                                 (string-append out "/lib"))
               (install-file (string-append build "/stage1-tools-bin/cargo")
                             (string-append cargo-out "/bin")))))
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
     `(("cmake" ,cmake-minimal)
       ("pkg-config" ,pkg-config)       ; For "cargo"
       ("python" ,python-wrapper)
       ("rustc-bootstrap" ,rust-bootstrap)
       ("cargo-bootstrap" ,rust-bootstrap "cargo")
       ("which" ,which)))
    (inputs
     `(("jemalloc" ,jemalloc)
       ("llvm" ,llvm-13)
       ("openssl" ,openssl)
       ("libssh2" ,libssh2)             ; For "cargo"
       ("libcurl" ,curl)))              ; For "cargo"
    ;; rustc invokes gcc, so we need to set its search paths accordingly.
    ;; Note: duplicate its value here to cope with circular dependencies among
    ;; modules (see <https://bugs.gnu.org/31392>).
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "CPLUS_INCLUDE_PATH")
            (files '("include/c++" "include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib" "lib64")))))
    (synopsis "Compiler for the Rust programming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://www.rust-lang.org")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define rust-1.56
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.55 "1.56.1"
                    "04cmqx7nn63hzz7z27b2b0dj2qx18rck9ifvip43s6dampx8v2f3")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:validate-runpath? _ #t)
          #t)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'add-cc-shim-to-path)
             (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
               (lambda* _
                 (substitute* "Cargo.lock"
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor"))))))))))

(define rust-1.57
  (rust-bootstrapped-package
   ;; Verified that it *doesn't* build with 1.55. e.g.:
   ;; * feature `edition2021` is required
   rust-1.56 "1.57.0" "06jw8ka2p3kls8p0gd4p0chhhb1ia1mlvj96zn78n7qvp71zjiim"))

(define rust-1.58
  (rust-bootstrapped-package
   ;; Verified that it *doesn't* build with 1.56. e.g.:
   ;; * error: attributes starting with `rustc` are reserved for use by the
   ;;   `rustc` compiler
   ;; * error: cannot find attribute `rustc_do_not_const_check` in this scope
   ;; * error[E0522]: definition of an unknown language item:
   ;;   `const_eval_select_ct`
   rust-1.57 "1.58.1" "1iq7kj16qfpkx8gvw50d8rf7glbm6s0pj2y1qkrz7mi56vfsyfd8"))

(define rust-1.59
  (let ((base-rust
          (rust-bootstrapped-package
            ;; Verified that it *doesn't* build with 1.57. e.g.:
            ;; * error: `doc(primitive)` should never have been stable
            ;; * error[E0522]: definition of an unknown language item:
            ;;   `generator_return`
            ;; * error[E0206]: the trait `Copy` may not be implemented for this type
            rust-1.58 "1.59.0" "1yc5bwcbmbwyvpfq7zvra78l0r8y3lbv60kbr62fzz2vx2pfxj57")))
    (package
      (inherit base-rust)
        (arguments
         (if (target-riscv64?)
           (substitute-keyword-arguments (package-arguments base-rust)
             ((#:phases phases)
              `(modify-phases ,phases
                 (add-after 'unpack 'revert-riscv-pause-instruction
                   (lambda _
                     ;; This fails with:
                     ;; error: unknown directive, referring to '.insn'.
                     ;; This is due to building with llvm < 14.
                     ;; https://github.com/rust-lang/stdarch/issues/1291
                     ;; Partial roll-back from this commit:
                     ;; https://github.com/rust-lang/stdarch/pull/1271
                     (substitute*
                       "library/stdarch/crates/core_arch/src/riscv_shared/mod.rs"
                       (("\\.insn i 0x0F, 0, x0, x0, 0x010") ".word 0x0100000F")))))))
           (package-arguments base-rust))))))

(define rust-1.60
  (rust-bootstrapped-package
   ;; Verified that it *doesn't* build with 1.58. e.g.:
   ;; * error: unknown codegen option: `symbol-mangling-version`
   rust-1.59 "1.60.0" "1drqr0a26x1rb2w3kj0i6abhgbs3jx5qqkrcwbwdlx7n3inq5ji0"))

(define rust-1.61
  (let ((base-rust
          (rust-bootstrapped-package
           rust-1.60 "1.61.0" "1vfs05hkf9ilk19b2vahqn8l6k17pl9nc1ky9kgspaascx8l62xd")))
    (package
      (inherit base-rust)
      (source
        (origin
          (inherit (package-source base-rust))
          (snippet
           '(begin
              (for-each delete-file-recursively
                        '("src/llvm-project"
                          "vendor/tikv-jemalloc-sys/jemalloc"))
              ;; Remove vendored dynamically linked libraries.
              ;; find . -not -type d -executable -exec file {} \+ | grep ELF
              (delete-file "vendor/vte/vim10m_match")
              (delete-file "vendor/vte/vim10m_table")
              ;; Also remove the bundled (mostly Windows) libraries.
              (for-each delete-file
                        (find-files "vendor" ".*\\.(a|dll|exe|lib)$")))))))))

(define rust-1.62
  (rust-bootstrapped-package
   rust-1.61 "1.62.1" "0gqkg34ic77dcvsz69qbdng6g3zfhl6hnhx7ha1mjkyrzipvxb3j"))

(define rust-1.63
  (rust-bootstrapped-package
   rust-1.62 "1.63.0" "1l4rrbzhxv88pnfq94nbyb9m6lfnjwixma3mwjkmvvs2aqlq158z"))

(define-public rust-1.64
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.63 "1.64.0" "018j720b2n12slp4xk64jc6shkncd46d621qdyzh2a8s3r49zkdk")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches (search-patches "rust-1.64-fix-riscv64-bootstrap.patch"))
         (patch-flags '("-p1" "--reverse"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'patch-cargo-checksums
               (lambda _
                 (substitute* '("Cargo.lock"
                                "src/bootstrap/Cargo.lock"
                                "src/tools/rust-analyzer/Cargo.lock")
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor"))))))))))

(define rust-1.65
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.64 "1.65.0" "0f005kc0vl7qyy298f443i78ibz71hmmh820726bzskpyrkvna2q")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches '())
         (patch-flags '("-p1")))))))

(define rust-1.66
  (rust-bootstrapped-package
   rust-1.65 "1.66.1" "1fjr94gsicsxd2ypz4zm8aad1zdbiccr7qjfbmq8f8f7jhx96g2v"))

(define rust-1.67
  (let ((base-rust
          (rust-bootstrapped-package
           rust-1.66 "1.67.1" "0vpzv6rm3w1wbni17ryvcw83k5klhghklylfdza3nnp8blz3sj26")))
    (package
      (inherit base-rust)
      (inputs (modify-inputs (package-inputs base-rust)
                             (replace "llvm" llvm-15))))))

(define rust-1.68
  (rust-bootstrapped-package
   rust-1.67 "1.68.2" "15ifyd5jj8rd979dkakp887hgmhndr68pqaqvd2hqkfdywirqcwk"))

;;; Note: Only the latest version of Rust is supported and tested.  The
;;; intermediate rusts are built for bootstrapping purposes and should not
;;; be relied upon.  This is to ease maintenance and reduce the time
;;; required to build the full Rust bootstrap chain.
;;;
;;; Here we take the latest included Rust, make it public, and re-enable tests
;;; and extra components such as rustfmt.
(define-public rust
  (let ((base-rust rust-1.68))
    (package
      (inherit base-rust)
      (outputs (cons "rustfmt" (package-outputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:tests? _ #f)
          (not (%current-target-system)))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'relax-gdb-auto-load-safe-path
               ;; Allow GDB to load binaries from any location, otherwise the
               ;; gdbinfo tests fail.  This is only useful when testing with a
               ;; GDB version newer than 8.2.
               (lambda _
                 (setenv "HOME" (getcwd))
                 (with-output-to-file (string-append (getenv "HOME") "/.gdbinit")
                   (lambda _
                     (format #t "set auto-load safe-path /~%")))
                 ;; Do not launch gdb with '-nx' which causes it to not execute
                 ;; any init file.
                 (substitute* "src/tools/compiletest/src/runtest.rs"
                   (("\"-nx\".as_ref\\(\\), ")
                    ""))))
             (add-after 'unpack 'patch-cargo-env-shebang
               (lambda _
                 (substitute* '("src/tools/cargo/tests/testsuite/build.rs"
                                "src/tools/cargo/tests/testsuite/fix.rs")
                   ;; The cargo *_wrapper tests set RUSTC.*WRAPPER environment
                   ;; variable which points to /usr/bin/env.  Since it's not a
                   ;; shebang, it needs to be manually patched.
                   (("/usr/bin/env")
                    (which "env")))))
             (add-after 'unpack 'disable-tests-requiring-git
               (lambda _
                 (substitute* "src/tools/cargo/tests/testsuite/new.rs"
                   (("fn author_prefers_cargo")
                    "#[ignore]\nfn author_prefers_cargo")
                   (("fn finds_author_git")
                    "#[ignore]\nfn finds_author_git")
                   (("fn finds_local_author_git")
                    "#[ignore]\nfn finds_local_author_git"))))
             (add-after 'unpack 'disable-tests-requiring-mercurial
               (lambda _
                 (substitute*
                   "src/tools/cargo/tests/testsuite/init/simple_hg_ignore_exists/mod.rs"
                   (("fn simple_hg_ignore_exists")
                    "#[ignore]\nfn simple_hg_ignore_exists"))
                 (substitute*
                   "src/tools/cargo/tests/testsuite/init/mercurial_autodetect/mod.rs"
                   (("fn mercurial_autodetect")
                    "#[ignore]\nfn mercurial_autodetect"))))
             (add-after 'unpack 'disable-tests-broken-on-aarch64
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite/"
                   (substitute* "build_script_extra_link_arg.rs"
                     (("^fn build_script_extra_link_arg_bin_single" m)
                      (string-append "#[ignore]\n" m)))
                   (substitute* "build_script.rs"
                     (("^fn env_test" m)
                      (string-append "#[ignore]\n" m)))
                   (substitute* "collisions.rs"
                     (("^fn collision_doc_profile_split" m)
                      (string-append "#[ignore]\n" m)))
                   (substitute* "concurrent.rs"
                     (("^fn no_deadlock_with_git_dependencies" m)
                      (string-append "#[ignore]\n" m)))
                   (substitute* "features2.rs"
                     (("^fn dep_with_optional_host_deps_activated" m)
                      (string-append "#[ignore]\n" m))))))
             (add-after 'unpack 'patch-command-exec-tests
               ;; This test suite includes some tests that the stdlib's
               ;; `Command` execution properly handles in situations where
               ;; the environment or PATH variable are empty, but this fails
               ;; since we don't have `echo` available at its usual FHS
               ;; location.
               (lambda _
                 (substitute* (match (find-files "." "^command-exec.rs$")
                                ((file) file))
                   (("Command::new\\(\"echo\"\\)")
                    (format #f "Command::new(~s)" (which "echo"))))))
             (add-after 'unpack 'patch-command-uid-gid-test
               (lambda _
                 (substitute* (match (find-files "." "^command-uid-gid.rs$")
                                ((file) file))
                   (("/bin/sh")
                    (which "sh")))))
             (add-after 'unpack 'skip-shebang-tests
               ;; This test make sure that the parser behaves properly when a
               ;; source file starts with a shebang. Unfortunately, the
               ;; patch-shebangs phase changes the meaning of these edge-cases.
               ;; We skip the test since it's drastically unlikely Guix's
               ;; packaging will introduce a bug here.
               (lambda _
                 (delete-file "tests/ui/parser/shebang/sneaky-attrib.rs")))
             (add-after 'unpack 'patch-process-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (substitute* "library/std/src/process/tests.rs"
                     (("\"/bin/sh\"")
                      (string-append "\"" bash "/bin/sh\"")))
                   ;; The three tests which are known to fail upstream on QEMU
                   ;; emulation on aarch64 and riscv64 also fail on x86_64 in Guix's
                   ;; build system. Skip them on all builds.
                   (substitute* "library/std/src/sys/unix/process/process_common/tests.rs"
                     (("target_arch = \"arm\",") "target_os = \"linux\",")))))
             (add-after 'unpack 'disable-interrupt-tests
               (lambda _
                 ;; This test hangs in the build container; disable it.
                 (substitute* (match (find-files "." "^freshness.rs$")
                                ((file) file))
                   (("fn linking_interrupted")
                    "#[ignore]\nfn linking_interrupted"))
                 ;; Likewise for the ctrl_c_kills_everyone test.
                 (substitute* (match (find-files "." "^death.rs$")
                                ((file) file))
                   (("fn ctrl_c_kills_everyone")
                    "#[ignore]\nfn ctrl_c_kills_everyone"))))
             (add-after 'unpack 'adjust-rpath-values
               ;; This adds %output:out to rpath, allowing us to install utilities in
               ;; different outputs while reusing the shared libraries.
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (substitute* "src/bootstrap/builder.rs"
                      ((" = rpath.*" all)
                       (string-append all
                                      "                "
                                      "rustflags.arg(\"-Clink-args=-Wl,-rpath="
                                      out "/lib\");\n"))))))
             (add-after 'configure 'add-gdb-to-config
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((gdb (assoc-ref inputs "gdb")))
                   (substitute* "config.toml"
                     (("^python =.*" all)
                      (string-append all
                                     "gdb = \"" gdb "/bin/gdb\"\n"))))))
             (replace 'build
               ;; Phase overridden to also build rustfmt.
               (lambda* (#:key parallel-build? #:allow-other-keys)
                 (let ((job-spec (string-append
                                  "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
                   (invoke "./x.py" job-spec "build"
                           "library/std" ;rustc
                           "src/tools/cargo"
                           "src/tools/rustfmt"))))
             (replace 'check
               ;; Phase overridden to also test rustfmt.
               (lambda* (#:key tests? parallel-build? #:allow-other-keys)
                 (when tests?
                   (let ((job-spec (string-append
                                    "-j" (if parallel-build?
                                             (number->string (parallel-job-count))
                                             "1"))))
                     (invoke "./x.py" job-spec "test" "-vv"
                             "library/std"
                             "src/tools/cargo"
                             "src/tools/rustfmt")))))
             (replace 'install
               ;; Phase overridden to also install rustfmt.
               (lambda* (#:key outputs #:allow-other-keys)
                 (invoke "./x.py" "install")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'cargo' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "cargo"))))
                 (invoke "./x.py" "install" "cargo")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'rustfmt' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "rustfmt"))))
                 (invoke "./x.py" "install" "rustfmt")))))))
      ;; Add test inputs.
      (native-inputs (cons* `("gdb" ,gdb/pinned)
                            `("procps" ,procps)
                            (package-native-inputs base-rust))))))

(define-public rust-src-1.64
  (hidden-package
   (package
     (inherit rust-1.64)
     (name "rust-src")
     (build-system copy-build-system)
     (native-inputs '())
     (inputs '())
     (native-search-paths '())
     (outputs '("out"))
     (arguments
      `(#:install-plan
        '(("library" "lib/rustlib/src/rust/library")
          ("src" "lib/rustlib/src/rust/src"))))
     (synopsis "Source code for the Rust standard library")
     (description "This package provide source code for the Rust standard
library, only use by rust-analyzer, make rust-analyzer out of the box."))))
