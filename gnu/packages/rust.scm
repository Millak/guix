;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017, 2019-2025 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages mingw)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
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
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
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
        (list base-rust))))
    (native-inputs
     (modify-inputs (package-native-inputs base-rust)
       (replace "rustc-bootstrap" base-rust)
       (replace "cargo-bootstrap" (list base-rust "cargo"))))))

;;; Note: mrustc's only purpose is to be able to bootstap Rust; it's designed
;;; to be used in source form.
(define %mrustc-commit "06b87d1af49d2db3bd850fdee8888055dd540dd1")
(define %mrustc-source
  (let* ((version "0.11.2")
         (commit %mrustc-commit)
         (revision "1")
         (name "mrustc"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/thepowersgang/mrustc")
            (commit %mrustc-commit)))
      (file-name (git-file-name name (git-version version revision commit)))
      (sha256
       (base32 "1m6ya3d653b5z0ygvr4y8hay1445gww9s8vyk8h0jdi42zkhzqkf"))
      (patches (search-patches "mrustc-patches.patch"))
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
(define-public rust-bootstrap-1.54
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
                     '("src/llvm-project"
                       "vendor/openssl-src/openssl"
                       "vendor/tikv-jemalloc-sys/jemalloc"))
           ;; Remove vendored dynamically linked libraries.
           ;; find . -not -type d -executable -exec file {} \+ | grep ELF
           (delete-file "vendor/vte/vim10m_match")
           (delete-file "vendor/vte/vim10m_table")
           ;; Also remove the bundled (mostly Windows) libraries.
           ;; find vendor -not -type d -exec file {} \+ | grep PE32
           (for-each delete-file
                     (find-files "vendor" "\\.(a|dll|exe|lib)$"))
           ;; Adjust some sources for llvm-13, see llvm commit
           ;; acce401068e78a8c5dc9e06802111ffad3da763f
           (substitute* (find-files "." "powerpc64le_unknown_linux_gnu.rs")
             (("e-m:e-i64:64-n32:64-v256:256:256-v512:512:512")
              "e-m:e-i64:64-n32:64-S128-v256:256:256-v512:512:512"))))))
    (outputs '("out" "cargo"))
    (properties '((hidden? . #t)
                  (timeout . 129600)          ;36 hours
                  (max-silent-time . 18000))) ;5 hours (for armel)
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal
           llvm-13
           openssl-1.1
           zlib))
    (native-inputs
     `(("pkg-config" ,pkg-config)
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
       #:make-flags
       ,#~(let ((source #$(package-source this-package)))
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
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source inputs #:allow-other-keys)
             ((assoc-ref %standard-phases 'unpack)
              #:source (assoc-ref inputs "mrustc-source"))))
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
                 " -D VERSION_GIT_FULLHASH=\\\"" ,%mrustc-commit "\\\""
                 " -D VERSION_GIT_BRANCH=\\\"master\\\""
                 " -D VERSION_GIT_SHORTHASH=\\\""
                 ,(string-take %mrustc-commit 7) "\\\""
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
               ;; https://github.com/dtolnay/bootstrap/blob/master/build-1.54.0.sh.
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

               ;; The psm crate FTBFS on ppc64le with gcc.
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
                    (system-lib-prefix
                      (string-append lib "/rustlib/"
                                     ,(platform-rust-target
                                        (lookup-platform-by-target-or-system
                                          (or (%current-target-system)
                                              (%current-system)))) "/lib")))
               (with-directory-excursion "run_rustc/output/prefix"
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

    ;; The intermediate generated code is known to be inefficient and
    ;; therefore the build process needs 8GB of RAM while building.
    ;; It may support i686 soon:
    ;; <https://github.com/thepowersgang/mrustc/issues/78>.
    ;; List of systems where rust-bootstrap is explicitly known to build:
    (supported-systems '("x86_64-linux" "aarch64-linux"
                         "riscv64-linux" "powerpc64le-linux"))

    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-bootstrap-1.74
  (package
    (name "rust")
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (rust-uri version))
       (sha256 (base32 "0j8hrwjjjjf7spy0hy7gami96swhfzr6kandfzzdri91qd5mhaw8"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file-recursively
                     '("src/llvm-project"
                       "vendor/openssl-src/openssl"
                       "vendor/tikv-jemalloc-sys/jemalloc"))
           ;; Remove vendored dynamically linked libraries.
           ;; find . -not -type d -executable -exec file {} \+ | grep ELF
           ;; Also remove the bundled (mostly Windows) libraries.
           (for-each delete-file
                     (find-files "vendor" "\\.(a|dll|exe|lib)$"))
           ;; Adjust vendored dependency to explicitly use rustix with libc backend.
           (substitute* "vendor/tempfile/Cargo.toml"
             (("features = \\[\"fs\"" all)
              (string-append all ", \"use-libc\"")))))
       ;; Rust 1.70 adds the rustix library which depends on the vendored
       ;; fd-lock crate.  The fd-lock crate uses Outline assembly which expects
       ;; a precompiled static library.  Enabling the "cc" feature tells the
       ;; build.rs script to compile the assembly files instead of searching
       ;; for a precompiled library.
       (patches (search-patches "rust-1.70-fix-rustix-build.patch"))))
    (outputs '("out" "cargo"))
    (properties '((hidden? . #t)
                  (timeout . 129600)          ;36 hours
                  (max-silent-time . 18000))) ;5 hours (for riscv64)
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal
           llvm-17
           openssl
           zlib))
    (native-inputs
     (list pkg-config %mrustc-source))
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
                           (origin-file-name %mrustc-source)))))
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
                  " -D VERSION_GIT_FULLHASH=\\\"" #$%mrustc-commit "\\\""
                  " -D VERSION_GIT_BRANCH=\\\"master\\\""
                  " -D VERSION_GIT_SHORTHASH=\\\""
                  #$(string-take %mrustc-commit 7) "\\\""
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
                ;; https://github.com/dtolnay/bootstrap/blob/master/build-1.54.0.sh.
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
    (supported-systems '("x86_64-linux"))

    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-1.55
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
                       "vendor/openssl-src/openssl"
                       "vendor/tikv-jemalloc-sys/jemalloc"))
           ;; Remove vendored dynamically linked libraries.
           ;; find . -not -type d -executable -exec file {} \+ | grep ELF
           (delete-file "vendor/vte/vim10m_match")
           (delete-file "vendor/vte/vim10m_table")
           ;; Also remove the bundled (mostly Windows) libraries.
           ;; find vendor -not -type d -exec file {} \+ | grep PE32
           (for-each delete-file
                     (find-files "vendor" "\\.(a|dll|exe|lib)$"))
           ;; Add support for riscv64-linux.
           (substitute* "vendor/tikv-jemallocator/src/lib.rs"
             (("    target_arch = \"s390x\"," all)
              (string-append all "\n    target_arch = \"riscv64\",")))))))
    (outputs '("out" "cargo"))
    (properties '((hidden? . #t)
                  (timeout . 72000)           ;20 hours
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
                "linker.env(\"LC_ALL\", \"C.UTF-8\");"))))
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
                    (llvm (assoc-ref inputs "llvm")))
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
jemalloc=false
default-linker = \"" gcc "/bin/gcc" "\"
channel = \"stable\"
rpath = true
[target." ,(platform-rust-target (lookup-platform-by-system (%current-system))) "]
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
                    (build (string-append "build/"
                                          ,(platform-rust-target
                                            (lookup-platform-by-target-or-system
                                             (or (%current-target-system)
                                                 (%current-system)))))))
               ;; Manually do the installation instead of calling './x.py
               ;; install', as that is slow and needlessly rebuilds some
               ;; things.

               ;; TODO: Try reverting to use "./x.py install rustc std cargo"
               ;; for Rust 1.91 and newer, which should no longer rebuild
               ;; things unnecessarily.
               (with-directory-excursion build
                 (install-file "stage1/bin/rustc" (string-append out "/bin"))
                 (install-file "stage1-tools-bin/cargo"
                               (string-append cargo-out "/bin"))
                 ;; Clean up some extraneous library files.  The .rmeta files
                 ;; of the bootstrap compiler are not meant to be installed.
                 (for-each delete-file (find-files "stage1/lib" "\\.rmeta$"))
                 ;; This librustc_driver binary is internal and not supposed
                 ;; to be installed.  It started causing issues with the build
                 ;; around Rust 1.89.
                 (for-each delete-file
                           (find-files "stage1/lib/rustlib"
                                       "^librustc_driver.*\\.so$"))
                 (copy-recursively "stage1/lib" (string-append out "/lib"))))))
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
     `(("pkg-config" ,pkg-config)
       ("python" ,python-minimal-wrapper)
       ("rustc-bootstrap" ,rust-bootstrap-1.54)
       ("cargo-bootstrap" ,rust-bootstrap-1.54 "cargo")))
    (inputs
     `(("bash" ,bash-minimal)
       ("llvm" ,llvm-13)
       ("openssl" ,openssl)))
    ;; rustc invokes gcc, so we need to set its search paths accordingly.
    (native-search-paths
     %gcc-search-paths)
    ;; Limit this to systems where the final rust compiler builds successfully.
    (supported-systems '("x86_64-linux" "aarch64-linux" "riscv64-linux"))
    (synopsis "Compiler for the Rust programming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://www.rust-lang.org")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-1.56
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.55 "1.56.1"
                    "04cmqx7nn63hzz7z27b2b0dj2qx18rck9ifvip43s6dampx8v2f3")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments
         (strip-keyword-arguments '(#:validate-runpath?)
                                  (package-arguments base-rust))
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'add-cc-shim-to-path)
             (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
               (lambda _
                 (substitute* "Cargo.lock"
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor"))))))))))

(define-public rust-1.57
  (rust-bootstrapped-package
   ;; Verified that it *doesn't* build with 1.55. e.g.:
   ;; * feature `edition2021` is required
   rust-1.56 "1.57.0" "06jw8ka2p3kls8p0gd4p0chhhb1ia1mlvj96zn78n7qvp71zjiim"))

(define-public rust-1.58
  (rust-bootstrapped-package
   ;; Verified that it *doesn't* build with 1.56. e.g.:
   ;; * error: attributes starting with `rustc` are reserved for use by the
   ;;   `rustc` compiler
   ;; * error: cannot find attribute `rustc_do_not_const_check` in this scope
   ;; * error[E0522]: definition of an unknown language item:
   ;;   `const_eval_select_ct`
   rust-1.57 "1.58.1" "1iq7kj16qfpkx8gvw50d8rf7glbm6s0pj2y1qkrz7mi56vfsyfd8"))

(define-public rust-1.59
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

(define-public rust-1.60
  (rust-bootstrapped-package
   ;; Verified that it *doesn't* build with 1.58. e.g.:
   ;; * error: unknown codegen option: `symbol-mangling-version`
   rust-1.59 "1.60.0" "1drqr0a26x1rb2w3kj0i6abhgbs3jx5qqkrcwbwdlx7n3inq5ji0"))

(define-public rust-1.61
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
                          "vendor/openssl-src/openssl"
                          "vendor/tikv-jemalloc-sys/jemalloc"))
              ;; Remove vendored dynamically linked libraries.
              ;; find . -not -type d -executable -exec file {} \+ | grep ELF
              (delete-file "vendor/vte/vim10m_match")
              (delete-file "vendor/vte/vim10m_table")
              ;; Also remove the bundled (mostly Windows) libraries.
              (for-each delete-file
                        (find-files "vendor" "\\.(a|dll|exe|lib)$")))))))))

(define-public rust-1.62
  (rust-bootstrapped-package
   rust-1.61 "1.62.1" "0gqkg34ic77dcvsz69qbdng6g3zfhl6hnhx7ha1mjkyrzipvxb3j"))

(define-public rust-1.63
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
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/openssl-src/openssl"
                         "vendor/tikv-jemalloc-sys/jemalloc"))
             ;; ERROR: could not find native static library
             ;; `rustix_outline_powerpc64`, perhaps an -L flag is missing?
             ;; Adjust rustix to always build with cc.
             (substitute* "src/bootstrap/Cargo.lock"
               (("\"errno\",") "\"cc\",\n \"errno\","))
             ;; Add a dependency on the the 'cc' feature of rustix.
             (substitute* "vendor/fd-lock/Cargo.toml"
               (("\"fs\"") "\"fs\", \"cc\""))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             (delete-file "vendor/vte/vim10m_match")
             (delete-file "vendor/vte/vim10m_table")
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))))
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

(define-public rust-1.65
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

(define-public rust-1.66
  (rust-bootstrapped-package
   rust-1.65 "1.66.1" "1fjr94gsicsxd2ypz4zm8aad1zdbiccr7qjfbmq8f8f7jhx96g2v"))

(define-public rust-1.67
  (let ((base-rust
          (rust-bootstrapped-package
           rust-1.66 "1.67.1" "0vpzv6rm3w1wbni17ryvcw83k5klhghklylfdza3nnp8blz3sj26")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/openssl-src/openssl"
                         "vendor/tikv-jemalloc-sys/jemalloc"))
             ;; Adjust rustix to always build with cc.
             (substitute* '("Cargo.lock"
                            "src/bootstrap/Cargo.lock")
               (("\"errno\",") "\"cc\",\n \"errno\","))
             ;; Add a dependency on the the 'cc' feature of rustix.
             (substitute* "vendor/fd-lock/Cargo.toml"
               (("\"fs\"") "\"fs\", \"cc\""))
             (substitute* "vendor/is-terminal/Cargo.toml"
               (("\"termios\"") "\"termios\", \"cc\""))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             (delete-file "vendor/vte/vim10m_match")
             (delete-file "vendor/vte/vim10m_table")
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))))))
      (inputs (modify-inputs (package-inputs base-rust)
                             (replace "llvm" llvm-15))))))

(define-public rust-1.68
  (rust-bootstrapped-package
   rust-1.67 "1.68.2" "15ifyd5jj8rd979dkakp887hgmhndr68pqaqvd2hqkfdywirqcwk"))

(define-public rust-1.69
  (let ((base-rust
          (rust-bootstrapped-package
            rust-1.68 "1.69.0"
            "03zn7kx5bi5mdfsqfccj4h8gd6abm7spj0kjsfxwlv5dcwc9f1gv")))
    (package
      (inherit base-rust)
      (source
        (origin
          (inherit (package-source base-rust))
          (snippet
           '(begin
              (for-each delete-file-recursively
                        '("src/llvm-project"
                          "vendor/openssl-src/openssl"
                          "vendor/tikv-jemalloc-sys/jemalloc"))
              ;; Adjust rustix to always build with cc.
              (substitute* '("Cargo.lock"
                             "src/bootstrap/Cargo.lock")
                (("\"errno\",") "\"cc\",\n \"errno\","))
              ;; Add a dependency on the the 'cc' feature of rustix.
              (substitute* "vendor/fd-lock/Cargo.toml"
                (("\"fs\"") "\"fs\", \"cc\""))
              (substitute* "vendor/is-terminal/Cargo.toml"
                (("\"termios\"") "\"termios\", \"cc\""))
              ;; Also remove the bundled (mostly Windows) libraries.
              (for-each delete-file
                        (find-files "vendor" "\\.(a|dll|exe|lib)$")))))))))

(define-public rust-1.70
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.69 "1.70.0"
                      "0z6j7d0ni0rmfznv0w3mrf882m11kyh51g2bxkj40l3s1c0axgxj")))
   (package
     (inherit base-rust)
     (source
      (origin
        (inherit (package-source base-rust))
        (snippet
         '(begin
            (for-each delete-file-recursively
                      '("src/llvm-project"
                        "vendor/openssl-src/openssl"
                        "vendor/tikv-jemalloc-sys/jemalloc"))
             ;; Adjust rustix to always build with cc.
             (substitute* "Cargo.lock"
               (("\"errno\",") "\"cc\",\n \"errno\","))
            ;; Add a dependency on the the 'cc' feature of rustix.
            (substitute* '("vendor/is-terminal/Cargo.toml"
                           "vendor/is-terminal-0.4.4/Cargo.toml")
              (("\"termios\"") "\"termios\", \"cc\""))
            ;; Also remove the bundled (mostly Windows) libraries.
            (for-each delete-file
                      (find-files "vendor" "\\.(a|dll|exe|lib)$"))))
        ;; Rust 1.70 adds the rustix library which depends on the vendored
        ;; fd-lock crate.  The fd-lock crate uses Outline assembly which expects
        ;; a precompiled static library.  Enabling the "cc" feature tells the
        ;; build.rs script to compile the assembly files instead of searching
        ;; for a precompiled library.
        (patches (search-patches "rust-1.70-fix-rustix-build.patch")))))))

(define-public rust-1.71
  (let ((base-rust
          (rust-bootstrapped-package
           rust-1.70 "1.71.1" "0bj79syjap1kgpg9pc0r4jxc0zkxwm6phjf3digsfafms580vabg")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/openssl-src/openssl"
                         "vendor/tikv-jemalloc-sys/jemalloc"))
             ;; Adjust rustix to always build with cc.
             (substitute* '("Cargo.lock"
                            "src/tools/cargo/Cargo.lock")
               (("\"errno\",") "\"cc\",\n \"errno\","))
             ;; Add a dependency on the the 'cc' feature of rustix.
             (substitute* '("vendor/is-terminal/Cargo.toml"
                            "vendor/is-terminal-0.4.6/Cargo.toml")
               (("\"termios\"") "\"termios\", \"cc\""))
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'patch-cargo-checksums
               (lambda _
                 (substitute* (cons* "Cargo.lock"
                                     "src/bootstrap/Cargo.lock"
                                     (find-files "src/tools" "Cargo.lock"))
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor"))))))))))

(define-public rust-1.72
  (let ((base-rust
          (rust-bootstrapped-package
           rust-1.71 "1.72.1" "15gqd1jzhnc16a7gjmav4x1v83jjbzyjh1gvcdfvpkajd9gq8j3z")))
    (package
      (inherit base-rust)
      (source
        (origin
          (inherit (package-source base-rust))
          (snippet
           '(begin
              (for-each delete-file-recursively
                        '("src/llvm-project"
                          "vendor/openssl-src/openssl"
                          "vendor/tikv-jemalloc-sys/jemalloc"))
              ;; Remove vendored dynamically linked libraries.
              ;; find . -not -type d -executable -exec file {} \+ | grep ELF
              ;; Also remove the bundled (mostly Windows) libraries.
              (for-each delete-file
                        (find-files "vendor" "\\.(a|dll|exe|lib)$"))
              ;; Adjust some crates to explicitly use rustix with the libc backend.
              (substitute* '("vendor/is-terminal/Cargo.toml"
                             "vendor/is-terminal-0.4.7/Cargo.toml")
                (("\"termios\"") "\"termios\", \"use-libc\""))
              (substitute* "compiler/rustc_driver/Cargo.toml"
                (("rustix = \"=0.37.11\"")
                 (string-append "rustix = { version = \"=0.37.11\","
                                " features = [\"use-libc\"] }"))))))))))

(define-public rust-1.73
  (let ((base-rust (rust-bootstrapped-package rust-1.72 "1.73.0"
                    "0fmvn7vg3qg9xprgfwv10g3ygy8i4j4bkcxcr1xdy89d3xnjxmln")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/openssl-src/openssl"
                         "vendor/tikv-jemalloc-sys/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* "vendor/tempfile-3.6.0/Cargo.toml"
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.74
  (let ((base-rust (rust-bootstrapped-package rust-1.73 "1.74.1"
                    "07930r17dkj3dnsrmilywb6p9i2g2jx56ndfpa2wh8crzhi3xnv7")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/openssl-src/openssl"
                         "vendor/tikv-jemalloc-sys/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* "vendor/tempfile/Cargo.toml"
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\"")))))))
      (arguments
       (if (target-riscv64?)
         (substitute-keyword-arguments (package-arguments base-rust)
           ((#:phases phases)
            `(modify-phases ,phases
               ;; This phase is no longer needed.
               (delete 'revert-riscv-pause-instruction))))
         (package-arguments base-rust))))))

(define-public rust-1.75
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.74 "1.75.0"
                    "1260mf3066ki6y55pvr35lnf54am6z96a3ap3hniwd4xpi2rywsv")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches '())))
      (arguments
       (if (supported-package? rust-bootstrap-1.74)
           (substitute-keyword-arguments (package-arguments base-rust)
             ((#:phases phases)
              `(modify-phases ,phases
                 (add-after 'unpack 'add-cc-shim-to-path
                   (lambda _
                     (mkdir-p "/tmp/bin")
                     (symlink (which "gcc") "/tmp/bin/cc")
                     (setenv "PATH" (string-append "/tmp/bin:" (getenv "PATH")))))
                 (replace 'install
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (cargo-out (assoc-ref outputs "cargo"))
                            (build (string-append "build/"
                                                  ,(platform-rust-target
                                                    (lookup-platform-by-target-or-system
                                                     (or (%current-target-system)
                                                         (%current-system)))))))
                       ;; Manually do the installation instead of calling './x.py
                       ;; install', as that is slow and needlessly rebuilds some
                       ;; things.

                       ;; TODO: Try reverting to use "./x.py install rustc std cargo"
                       ;; for Rust 1.91 and newer, which should no longer rebuild
                       ;; things unnecessarily.
                       (with-directory-excursion build
                         (install-file "stage1/bin/rustc" (string-append out "/bin"))
                         (install-file "stage1-tools-bin/cargo"
                                       (string-append cargo-out "/bin"))
                         ;; Clean up some extraneous library files.  The .rmeta files
                         ;; of the bootstrap compiler are not meant to be installed.
                         (for-each delete-file (find-files "stage1/lib" "\\.rmeta$"))
                         ;; This librustc_driver binary is internal and not supposed
                         ;; to be installed.  It started causing issues with the build
                         ;; around Rust 1.89.
                         (for-each delete-file
                                   (find-files "stage1/lib/rustlib"
                                               "^librustc_driver.*\\.so$"))
                         (copy-recursively "stage1/lib" (string-append out "/lib")))))))))
           (package-arguments base-rust)))
      (native-inputs
       (if (supported-package? rust-bootstrap-1.74)
           (modify-inputs (package-native-inputs base-rust)
             (replace "cargo-bootstrap" (list rust-bootstrap-1.74 "cargo"))
             (replace "rustc-bootstrap" rust-bootstrap-1.74))
           (package-native-inputs base-rust)))
      (inputs (modify-inputs (package-inputs base-rust)
                (replace "llvm" llvm-17))))))

(define-public rust-1.76
  (let ((base-rust (rust-bootstrapped-package rust-1.75 "1.76.0"
                    "08f06shp6l72qrv5fwg1is7yzr6kwj8av0l9h5k243bz781zyp4y")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'add-cc-shim-to-path))))))))

(define-public rust-1.77
  (let ((base-rust (rust-bootstrapped-package rust-1.76 "1.77.1"
                    "18d4ncdzp0nfimmw029xdf7vv1hgh82v30mjnnixnllzar66w47f")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'configure 'no-optimized-compiler-builtins
               (lambda _
                 ;; Pre-1.77, the behavior was equivalent to this flag being
                 ;; "false" if the llvm-project submodule wasn't checked out.
                 ;;
                 ;; Now there's an explicit check, so the build fails if we don't
                 ;; manually disable this (given that we don't have the submodule checked out).
                 ;; Thus making the build behave the same as it did in 1.76 and earlier.
                 ;;
                 ;; TODO - make the build system depend on system llvm for this, so we
                 ;; can get the performance benefits of setting this to true?
                 (substitute* "config.toml"
                   (("\\[build\\]")
                    "[build]\noptimized-compiler-builtins = false")))))))))))

(define-public rust-1.78
  (let ((base-rust (rust-bootstrapped-package rust-1.77 "1.78.0"
                    "1afmj5g3bz7439w4i8zjhd68zvh0gqg7ymr8h5rz49ybllilhm7z")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         ;; see https://github.com/rust-lang/rust/pull/125844
         (patches (search-patches "rust-1.78-unwinding-fix.patch")))))))

(define-public rust-1.79
  (let ((base-rust (rust-bootstrapped-package rust-1.78 "1.79.0"
                    "1h282jb1yxc69999w4nhvqb08rw2jy32i9njdjqrz78zglycybhp")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/openssl-src-111.28.1+1.1.1w/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* '("vendor/tempfile-3.7.1/Cargo.toml"
                            "vendor/tempfile-3.10.1/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.80
  (let ((base-rust (rust-bootstrapped-package rust-1.79 "1.80.1"
                    "1i1dbpwnv6ak244lapsxvd26w6sbas9g4l6crc8bip2275j8y2rc")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* '("vendor/tempfile-3.7.1/Cargo.toml"
                            "vendor/tempfile-3.10.1/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.81
  (let ((base-rust (rust-bootstrapped-package rust-1.80 "1.81.0"
                    "19yggj1qivdhf68gx2652cfi7nxjkdgy39wh7h6facpzppz4h947")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         ;; see https://github.com/rust-lang/rust/issues/129268#issuecomment-2430133779
         (patches (search-patches "rust-1.81-fix-riscv64-bootstrap.patch")))))))

(define-public rust-1.82
  (let ((base-rust (rust-bootstrapped-package rust-1.81 "1.82.0"
                    "0ajiryki2aqsg3ydx3nfhrb5i1mmxvasfszs9qblw66skr8g8lvw")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches '())))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'patch-cargo-checksums
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
                 (generate-all-checksums "vendor"))))))))))

(define-public rust-1.83
  (let ((base-rust (rust-bootstrapped-package rust-1.82 "1.83.0"
                    "0vhwhk4cbyppnz0lcazfjyddyz811fgvadfxswldicpashxpfbbj")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/gcc"
                         "src/llvm-project"
                         "vendor/jemalloc-sys-0.3.2"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* '("vendor/tempfile-3.10.1/Cargo.toml"
                            "vendor/tempfile-3.13.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\"")))))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'configure 'use-system-llvm
               (lambda _
                 (substitute* "config.toml"
                   (("\\[llvm\\]") "[llvm]\ndownload-ci-llvm = false")
                   (("\\[rust\\]") "[rust]\ndownload-rustc = false"))))))))
      ;; Need llvm >= 18.0
      (inputs (modify-inputs (package-inputs base-rust)
                             (replace "llvm" llvm-19))))))

(define-public rust-1.84
  (rust-bootstrapped-package rust-1.83 "1.84.1"
   "0xdk3g1xq33fy4m7q6l38ba9ydxbanccyb0vczvlk998jvababsy"))

(define-public rust-1.85
  (let ((base-rust
         (rust-bootstrapped-package rust-1.84 "1.85.1"
                                    "12i3jkxrc2bbd2d423q7krwkbc39wlwkjacsimx7b61m1359aa8g")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/jemalloc-sys-0.3.2"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* '("vendor/tempfile-3.10.1/Cargo.toml"
                            "vendor/tempfile-3.14.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.86
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.85 "1.86.0"
          "107d3vbmwhn4b71gzr1kv31jwwyld7dxj9yj8jh00ygndll2fah2")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/jemalloc-sys-0.3.2"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/openssl-src-111.17.0+1.1.1m/openssl"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.15.0/Cargo.toml"
                            "vendor/tempfile-3.16.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\"")))))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'use-system-llvm 'disable-lld-wrapper
               (lambda _
                 (substitute* "config.toml"
                   (("\\[rust\\]") "[rust]\nlld = false")))))))))))

(define-public rust-1.87
  (let ((base-rust
         (rust-bootstrapped-package rust-1.86 "1.87.0"
          "0xlfzl24qhs4vlwn7d2hd2zkg6k2y1lgq03rx2j2sndy57yvk6ql")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/jemalloc-sys-0.3.2"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/openssl-src-111.17.0+1.1.1m/openssl"
                         "vendor/openssl-src-300.4.2+3.4.1/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.15.0/Cargo.toml"
                            "vendor/tempfile-3.17.1/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\"")))))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
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
                           "src/tools/cargo")))))))))))

(define-public rust-1.88
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.87 "1.88.0"
          "1gngrxjfqiyybyamzn8s4rfbhk7jss1vrmnijg8y72l46i2595rs")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         "vendor/jemalloc-sys-0.3.2"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/openssl-src-111.17.0+1.1.1m/openssl"
                         "vendor/openssl-src-300.4.2+3.4.1/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* '("vendor/tempfile-3.14.0/Cargo.toml"
                            "vendor/tempfile-3.17.1/Cargo.toml"
                            "vendor/tempfile-3.19.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.89
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.88 "1.89.0"
          "0395ax8d138ch65fzm1y9xf2s4661am5k3yj3cav16fx83sgjxi5")))
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
                         "vendor/curl-sys-0.4.80+curl-8.12.1/curl"
                         "vendor/curl-sys-0.4.82+curl-8.14.1/curl"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/libffi-sys-3.3.1/libffi"
                         "vendor/libz-sys-1.1.21/src/zlib"
                         "vendor/libz-sys-1.1.22/src/zlib"
                         "vendor/libmimalloc-sys-0.1.42/c_src/mimalloc"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/openssl-src-300.5.0+3.5.0/openssl"
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
                            "vendor/tempfile-3.19.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml"
                            "vendor/tempfile-3.20.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\"")))))))
      (inputs
       (modify-inputs (package-inputs base-rust)
         (prepend curl `(,nghttp2 "lib") libffi))))))

(define-public rust-1.90
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.89 "1.90.0"
          "0zlv3ybd8za07brxwj4n03cma1snnpvbqj0h0wg3bmafpaf9z6kr")))
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
                         "vendor/curl-sys-0.4.82+curl-8.14.1/curl"
                         "vendor/jemalloc-sys-0.5.3+5.3.0-patched/jemalloc"
                         "vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/libffi-sys-3.3.2/libffi"
                         "vendor/libz-sys-1.1.21/src/zlib"
                         "vendor/libz-sys-1.1.22/src/zlib"
                         "vendor/libmimalloc-sys-0.1.42/c_src/mimalloc"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/openssl-src-300.5.0+3.5.0/openssl"
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
                            "vendor/tempfile-3.19.0/Cargo.toml"
                            "vendor/tempfile-3.19.1/Cargo.toml"
                            "vendor/tempfile-3.20.0/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\"")))))))
      (inputs
       (modify-inputs (package-inputs base-rust)
         (replace "llvm" llvm-21))))))

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
  (let ((base-rust rust-1.90))
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
             (add-after 'unpack 'disable-tests-requiring-git
               (lambda _
                 (substitute* "src/tools/cargo/tests/testsuite/git.rs"
                   ,@(make-ignore-test-list
                      '("fn fetch_downloads_with_git2_first_"
                        "fn corrupted_checkout_with_cli")))
                 (substitute* "src/tools/cargo/tests/testsuite/build.rs"
                   ,@(make-ignore-test-list
                      '("fn build_with_symlink_to_path_dependency_with_build_script_in_git")))
                 (substitute* "src/tools/cargo/tests/testsuite/publish_lockfile.rs"
                   ,@(make-ignore-test-list
                      '("fn note_resolve_changes")))))
             (add-after 'unpack 'disable-tests-requiring-mercurial
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite/cargo_init"
                   (substitute* '("mercurial_autodetect/mod.rs"
                                  "simple_hg_ignore_exists/mod.rs")
                     ,@(make-ignore-test-list
                        '("fn case"))))))
             (add-after 'unpack 'disable-tests-using-cargo-publish
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite"
                   (substitute* "alt_registry.rs"
                     ,@(make-ignore-test-list
                        '("fn warn_for_unused_fields")))
                   (substitute* '("cargo_add/locked_unchanged/mod.rs"
                                  "cargo_add/lockfile_updated/mod.rs"
                                  "cargo_remove/update_lock_file/mod.rs")
                     ,@(make-ignore-test-list
                        '("fn case")))
                   (substitute* "git_shallow.rs"
                     ,@(make-ignore-test-list
                        '("fn gitoxide_clones_registry_with_shallow_protocol_and_aborts_and_updates_again"
                          "fn gitoxide_clones_registry_with_shallow_protocol_and_follow_up_fetch_maintains_shallowness"
                          "fn gitoxide_clones_registry_with_shallow_protocol_and_follow_up_with_git2_fetch"
                          "fn gitoxide_clones_registry_without_shallow_protocol_and_follow_up_fetch_uses_shallowness"
                          "fn gitoxide_clones_shallow_two_revs_same_deps"
                          "fn gitoxide_fetch_complete_index_then_shallow"
                          "fn gitoxide_fetch_shallow_dep_branch_and_rev"
                          "fn gitoxide_fetch_shallow_dep_branch_to_rev"
                          "fn gitoxide_fetch_shallow_dep_then_git2_fetch_complete"
                          "fn gitoxide_fetch_shallow_dep_then_gitoxide_fetch_complete"
                          "fn gitoxide_fetch_shallow_dep_two_revs"
                          "fn gitoxide_fetch_shallow_index_then_abort_and_update"
                          "fn gitoxide_fetch_shallow_index_then_git2_fetch_complete"
                          "fn gitoxide_fetch_shallow_index_then_preserve_shallow"
                          "fn gitoxide_git_dependencies_switch_from_branch_to_rev"
                          "fn gitoxide_shallow_clone_followed_by_non_shallow_update"
                          "fn shallow_deps_work_with_revisions_and_branches_mixed_on_same_dependency"
                          "fn gitoxide_clones_git_dependency_with_shallow_protocol_and_git2_is_used_for_followup_fetches")))
                   (substitute* "install.rs"
                     ,@(make-ignore-test-list
                        '("fn failed_install_retains_temp_directory")))
                   (substitute* "offline.rs"
                     ,@(make-ignore-test-list
                        '("fn gitoxide_cargo_compile_offline_with_cached_git_dep_shallow_dep")))
                   (substitute* "patch.rs"
                     ,@(make-ignore-test-list
                        '("fn gitoxide_clones_shallow_old_git_patch"))))))
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
             ,@(if (target-aarch64?)
                   ;; Keep this phase separate so it can be adjusted without needing
                   ;; to adjust the skipped tests on other architectures.
                   `((add-after 'unpack 'disable-tests-broken-on-aarch64
                       (lambda _
                         (with-directory-excursion "src/tools/cargo/tests/testsuite"
                           (substitute* "build_script_extra_link_arg.rs"
                             ,@(make-ignore-test-list
                                '("fn build_script_extra_link_arg_bin_single")))
                           (substitute* "build_script.rs"
                             ,@(make-ignore-test-list
                                '("fn env_test")))
                           (substitute* "cache_lock.rs"
                             ,@(make-ignore-test-list
                                '("fn download_then_mutate")))
                           (substitute* "collisions.rs"
                             ,@(make-ignore-test-list
                                '("fn collision_doc_profile_split")))
                           (substitute* "concurrent.rs"
                             ,@(make-ignore-test-list
                                '("fn no_deadlock_with_git_dependencies")))
                           (substitute* "features2.rs"
                             ,@(make-ignore-test-list
                                '("fn dep_with_optional_host_deps_activated"))))
                         (with-directory-excursion "src/tools/clippy/tests"
                           ;; `"vectorcall"` is not a supported ABI for the current target
                           (delete-file "ui/missing_const_for_fn/could_be_const.rs")
                           (substitute* "missing-test-files.rs"
                             ,@(make-ignore-test-list
                                '("fn test_missing_tests")))))))
                   `())
             (add-after 'unpack 'disable-tests-requiring-crates.io
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite"
                   (substitute* "install.rs"
                     ,@(make-ignore-test-list
                        '("fn install_global_cargo_config")))
                   (substitute* '("cargo_add/normalize_name_path_existing/mod.rs"
                                  "cargo_info/within_ws_with_alternative_registry/mod.rs")
                     ,@(make-ignore-test-list
                        '("fn case")))
                   (substitute* "package.rs"
                     ,@(make-ignore-test-list
                        '("fn workspace_with_local_deps_index_mismatch"))))))
             (add-after 'unpack 'disable-miscellaneous-broken-tests
               (lambda _
                 (substitute* "src/tools/cargo/tests/testsuite/check_cfg.rs"
                   ;; These apparently get confused by the fact that
                   ;; we're building in a directory containing the
                   ;; string "rustc"
                   ,@(make-ignore-test-list
                      '("fn config_fingerprint"
                        "fn features_fingerprint")))
                 (substitute* "src/tools/cargo/tests/testsuite/git_auth.rs"
                   ;; This checks for a specific networking error message
                   ;; that's different from the one we see in the builder
                   ,@(make-ignore-test-list
                      '("fn net_err_suggests_fetch_with_cli")))))
             (add-after 'unpack 'patch-command-exec-tests
               ;; This test suite includes some tests that the stdlib's
               ;; `Command` execution properly handles in situations where
               ;; the environment or PATH variable are empty, but this fails
               ;; since we don't have `echo` available at its usual FHS
               ;; location.
               (lambda _
                 (substitute* "tests/ui/command/command-exec.rs"
                   (("Command::new\\(\"echo\"\\)")
                    (format #f "Command::new(~s)" (which "echo"))))))
             (add-after 'unpack 'patch-command-uid-gid-test
               (lambda _
                 (substitute* "tests/ui/command/command-uid-gid.rs"
                   (("/bin/sh") (which "sh"))
                   (("/bin/ls") (which "ls")))))
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
                 (substitute* "src/tools/cargo/tests/testsuite/freshness.rs"
                   ,@(make-ignore-test-list
                      '("fn linking_interrupted")))
                 ;; Likewise for the ctrl_c_kills_everyone test.
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
                  (string-append (assoc-ref inputs "clang-source")
                                 "/compiler-rt")
                  "src/llvm-project/compiler-rt")))
             (add-after 'unpack 'unpack-libunwind
               (lambda* (#:key inputs #:allow-other-keys)
                 (mkdir-p "src/llvm-project/libunwind")
                 (copy-recursively
                  (string-append (assoc-ref inputs "clang-source")
                                 "/libunwind")
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
                           "src/tools/rust-analyzer"
                           "src/tools/rustfmt"
                           ;; Needed by rust-analyzer and editor plugins
                           "src/tools/rust-analyzer/crates/proc-macro-srv-cli"))))
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
                 (invoke "./x.py" "install")
                 ;; This one doesn't have an install target so
                 ;; we need to install manually.
                 (install-file (string-append
                                 "build/"
                                 ,(platform-rust-target
                                    (lookup-platform-by-system
                                      (%current-system)))
                                 "/stage1-tools-bin/rust-analyzer-proc-macro-srv")
                               (string-append (assoc-ref outputs "out") "/libexec"))
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
      (native-inputs (cons*
                      ;; Keep in sync with the llvm used to build rust.
                      `("clang-source" ,(package-source clang-runtime-21))
                      ;; Add test inputs.
                      `("gdb" ,gdb/pinned)
                      `("procps" ,procps)
                      (package-native-inputs base-rust)))
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
                                       ":")))
                           ;; When building a rust-sysroot this crate is only used for
                           ;; the rust-installer.
                           (substitute* '("vendor/num_cpus-1.13.0/src/linux.rs"
                                          "vendor/num_cpus-1.13.1/src/linux.rs"
                                          "vendor/num_cpus-1.16.0/src/linux.rs")
                             (("\\.ceil\\(\\)") ""))
                           ;; gcc doesn't recognize this flag.
                           (substitute*
                               "compiler/rustc_target/src/spec/base/windows_gnullvm.rs"
                             ((", \"--unwindlib=none\"") "")))))
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
              (delete 'wrap-rust-analyzer)
              (delete 'wrap-rustc)))))
      (inputs
       (modify-inputs (package-inputs base-rust)
         (prepend xz)))                 ; for lzma-sys
      (propagated-inputs
       (if (target-mingw? target)
           (modify-inputs (package-propagated-inputs base-rust)
             (prepend
              (make-mingw-w64
                (string-take target (string-index target #\-))
                #:with-winpthreads? #t)))
           (package-propagated-inputs base-rust)))
      (native-inputs
       (modify-inputs (package-native-inputs base-rust)
         ;; Build with the same version as the cross-gcc version.
         ;; TODO: Remove this when gcc and %xgcc are the same version again.
         (prepend gcc-14)
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
