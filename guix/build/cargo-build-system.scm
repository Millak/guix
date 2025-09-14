;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2019-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 Herman Rimm <herman@rimm.ee>
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

(define-module (guix build cargo-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((guix build utils) #:hide (delete))
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (json)
  #:export (%standard-phases
            cargo-build))

;; Commentary:
;;
;; Builder-side code of the standard Rust package build procedure.
;;
;; Code:

(define (manifest-targets)
  "Extract all targets from the Cargo.toml manifest"
  (let* ((command (string-join (list "cargo" "metadata" "--no-deps"
                                      "--format-version" "1")))
         (port (open-input-pipe command))
         (data (json->scm port))
         (packages (vector-ref (assoc-ref data "packages") 0))
         (targets (or (assoc-ref packages "targets") '())))
    (close-port port)
    targets))

(define (has-executable-target?)
  "Check if the current cargo project declares any binary targets."
  (let* ((bin? (lambda (kind) (string=? kind "bin")))
         (get-kinds (lambda (dep) (assoc-ref dep "kind")))
         (bin-dep? (lambda (dep) (vector-any bin? (get-kinds dep)))))
    (vector-any bin-dep? (manifest-targets))))

(define (cargo-package? dir)
  "Check if directory DIR contains a single package, or a Cargo workspace with
root package."
  (let ((manifest-file (in-vicinity dir "Cargo.toml")))
    (and (file-exists? manifest-file)
         (string-contains
          (call-with-input-file manifest-file get-string-all)
          "[package]"))))

(define* (crate-src? path #:key source)
  "Check if PATH refers to a crate source, namely a gzipped tarball with a
Cargo.toml file present at its root."
  (and (not (string=? path source))     ;Exclude self.
       (if (directory-exists? path)
           ;; The build system only handles sources containing single crate.
           ;; Workspaces should be packaged into crates (via 'package phase)
           ;; and used in inputs.
           (cargo-package? path)
           (and (not (string-suffix? "py" path)) ;sanity-check.py
                ;; First we print out all file names within the tarball to see
                ;; if it looks like the source of a crate. However, the tarball
                ;; will include an extra path component which we would like to
                ;; ignore (since we're interested in checking if a Cargo.toml
                ;; exists at the root of the archive, but not nested anywhere
                ;; else). We do this by cutting up each output line and only
                ;; looking at the second component. We then check if it matches
                ;; Cargo.toml exactly and short circuit if it does.
                (invoke "sh" "-c"
                        (string-append "tar -tf " path
                                       " | cut -d/ -f2"
                                       " | grep -q '^Cargo.toml$'"))))))

(define* (unpack-rust-crates #:key inputs (vendor-dir "guix-vendor")
                             #:allow-other-keys)
  (define (inputs->rust-inputs inputs)
    "Filter using the label part from INPUTS."
    (filter (lambda (input)
              (match input
                ((name . _) (rust-package? name))))
            inputs))
  (define (inputs->directories inputs)
    "Extract the directory part from INPUTS."
    (match inputs
      (((names . directories) ...)
       directories)))

  (let ((rust-inputs (inputs->directories (inputs->rust-inputs inputs))))
    (unless (null? rust-inputs)
      (mkdir-p "target/package")
      (mkdir-p vendor-dir)
      ;; TODO: copy only regular inputs to target/package, not native-inputs.
      (for-each
        (lambda (input-crate)
          (for-each
            (lambda (packaged-crate)
              (unless
                (file-exists?
                  (string-append "target/package/" (basename packaged-crate)))
                (install-file packaged-crate "target/package/")))
            (find-files
              (string-append input-crate "/share/cargo/registry") "\\.crate$")))
        (delete-duplicates rust-inputs))

      (for-each (lambda (crate)
                  (invoke "tar" "xzf" crate "-C" vendor-dir))
                (find-files "target/package" "\\.crate$")))))

(define (rust-package? name)
  (string-prefix? "rust-" name))

(define* (check-for-pregenerated-files #:key parallel-build? #:allow-other-keys)
  "Check the source code for files which are known to generally be bundled
libraries or executables."
  (format #t "Searching for binary files...~%")
  (let ((known-pattern (make-regexp "\\.(a|dll|dylib|exe|lib)$"))
        (empty-file?
         (lambda (file stat)
           (let ((size (stat:size stat)))
             (or (zero? size)
                 (and (eqv? 1 size)
                      (eqv? #\newline
                            (call-with-ascii-input-file file read-char))))))))
    (n-par-for-each
     (if parallel-build?
         (parallel-job-count)
         1)
     (lambda (file)
       ;; Print out binary files.
       (false-if-exception (invoke "grep" "-IL" "." file))
       ;; Warn about known pre-generated files.
       ;; Not failing here for compatibility with existing packages.
       (when (regexp-exec known-pattern file)
         (format #t "error: Possible pre-generated file found: ~a~%" file)))
     (find-files "." (negate empty-file?)))))

(define* (configure #:key source inputs native-inputs
                    target system
                    (cargo-target #f)
                    (vendor-dir "guix-vendor")
                    #:allow-other-keys)
  "Vendor Cargo.toml dependencies as guix inputs."
  (chmod "." #o755)
  ;; Prepare one new directory with all the required dependencies.
  ;; It's necessary to do this (instead of just using /gnu/store as the
  ;; directory) because we want to hide the libraries in subdirectories
  ;; share/rust-source/... instead of polluting the user's profile root.
  (mkdir-p vendor-dir)
  (for-each
    (match-lambda
      ((name . path)
       (let* ((basepath (strip-store-file-name path))
              (crate-dir (string-append vendor-dir "/" basepath)))
         (and (crate-src? path #:source source)
              ;; Gracefully handle duplicate inputs
              (not (file-exists? crate-dir))
              (if (directory-exists? path)
                  (copy-recursively path crate-dir)
                  (begin
                    (mkdir-p crate-dir)
                    ;; Cargo crates are simply gzipped tarballs but with a
                    ;; .crate extension. We expand the source to a directory
                    ;; name we control so that we can generate any cargo
                    ;; checksums.  The --strip-components argument is needed to
                    ;; prevent creating an extra directory within `crate-dir`.
                    (format #t "Unpacking ~a~%" name)
                    (invoke "tar" "xf" path "-C" crate-dir
                            "--strip-components" "1")))))))
    inputs)

  ;; For cross-building
  (when target
    (setenv "CARGO_BUILD_TARGET" cargo-target)
    (setenv "RUSTFLAGS"
            (string-append
             (or (getenv "RUSTFLAGS") "")
             " --sysroot "
             (assoc-ref
              (append inputs
                      ;; Workaround for other build systems, as no interface
                      ;; is available for target-inputs.
                      (or native-inputs '()))
              (string-append "rust-sysroot-for-" target))))

    (setenv "PKG_CONFIG" (string-append target "-pkg-config"))

    ;; We've removed all the bundled libraries, don't look for them.
    (setenv "WINAPI_NO_BUNDLED_LIBRARIES" "1")

    ;; Prevent targeting the build machine.
    (setenv "CRATE_CC_NO_DEFAULTS" "1"))

  ;; Support 16k kernel page sizes on aarch64 with jemalloc.
  (when (string-prefix? "aarch64" (or target system))
    (setenv "JEMALLOC_SYS_WITH_LG_PAGE" "14"))

  ;; Configure cargo to actually use this new directory with all the crates.
  (setenv "CARGO_HOME" (string-append (getcwd) "/.cargo"))
  (mkdir-p ".cargo")
  ;; Not .cargo/config.toml, rustc/cargo will generate .cargo/config otherwise.
  (let ((port (open-file ".cargo/config" "w" #:encoding "utf-8")))
    ;; Placed here so it doesn't cause random rebuilds.  Neither of these work.
    ;; sysroot = '" (assoc-ref inputs "rust-sysroot") "'
    ;; rustflags = ['--sysroot', '" (assoc-ref inputs "rust-sysroot") "']
    (when target
      (display (string-append "
[target." (getenv "CARGO_BUILD_TARGET") "]
linker = '" target "-gcc'

[build]
target = ['" (getenv "CARGO_BUILD_TARGET") "']") port))
    (display (string-append "
[source.crates-io]
replace-with = 'vendored-sources'

[source.vendored-sources]
directory = '" vendor-dir "'") port)
    (close-port port))

  ;; Lift restriction on any lints: a crate author may have decided to opt
  ;; into stricter lints (e.g. #![deny(warnings)]) during their own builds
  ;; but we don't want any build failures that could be caused later by
  ;; upgrading the compiler for example.
  (setenv "RUSTFLAGS" (string-append (or (getenv "RUSTFLAGS") "")
                                     " --cap-lints allow"))

  (if (assoc-ref inputs "cross-gcc")
    (begin
      (setenv "HOST_CC" "gcc")
      (setenv "TARGET_CC" (string-append target "-gcc"))
      (setenv "TARGET_AR" (string-append target "-ar"))
      (setenv "TARGET_PKG_CONFIG" (string-append target "-pkg-config")))
    (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc")))

  (setenv "GETTEXT_SYSTEM" "1")
  (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
  (setenv "LIBSQLITE3_SYS_USE_PKG_CONFIG" "1")
  (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
  (setenv "RUSTONIG_SYSTEM_LIBONIG" "1")
  (setenv "SODIUM_USE_PKG_CONFIG" "1")
  (setenv "ZSTD_SYS_USE_PKG_CONFIG" "1")
  (when (assoc-ref inputs "openssl")
    (setenv "OPENSSL_DIR" (assoc-ref inputs "openssl")))
  (when (assoc-ref inputs "clang")
    (setenv "LIBCLANG_PATH"
            (string-append (assoc-ref inputs "clang") "/lib")))

  ;; We don't use the Cargo.lock file to determine the package versions we use
  ;; during building, and in any case if one is not present it is created
  ;; during the 'build phase by cargo.
  (when (file-exists? "Cargo.lock")
    (delete-file "Cargo.lock")))

;; See: https://github.com/rust-lang/cargo/issues/11063.
(define* (patch-cargo-checksums #:key
                                (vendor-dir "guix-vendor")
                                #:allow-other-keys)
  "Add a stub checksum to each crate in VENDOR-DIR."
  (with-directory-excursion vendor-dir
    (call-with-output-file ".cargo-checksum.json"
      (cut display "{\"files\":{}}" <>))
    (for-each (lambda (dir)
                (copy-file ".cargo-checksum.json"
                           (string-append dir "/.cargo-checksum.json")))
              (drop (scandir ".") 3))
    (delete-file ".cargo-checksum.json")))

(define* (build #:key
                parallel-build?
                (skip-build? #f)
                (features '())
                (cargo-build-flags '("--release"))
                #:allow-other-keys)
  "Build a given Cargo package."
  (or skip-build?
      (apply invoke
             `("cargo" "build" "--offline"
               ,@(if parallel-build?
                     (list "-j" (number->string (parallel-job-count)))
                     (list "-j" "1"))
               ,@(if (null? features)
                     '()
                     `("--features" ,(string-join features)))
               ,@cargo-build-flags))))

(define* (check #:key
                parallel-build?
                parallel-tests?
                tests?
                (cargo-test-flags '())
                #:allow-other-keys)
  "Run tests for a given Cargo package."
  (when tests?
    (apply invoke
           `("cargo" "test" "--offline"
             ,@(if parallel-build?
                   (list "-j" (number->string (parallel-job-count)))
                   (list "-j" "1"))
             ,@cargo-test-flags
             ,@(if (member "--" cargo-test-flags)
                   '()
                   '("--"))
             ,@(if parallel-tests?
                   (list "--test-threads"
                         (number->string (parallel-job-count)))
                   (list "--test-threads" "1"))))))

(define* (package #:key
                  source
                  (skip-build? #f)
                  (install-source? #t)
                  (cargo-package-crates '())
                  (cargo-package-flags '("--no-metadata" "--no-verify"))
                  (vendor-dir "guix-vendor")
                  #:allow-other-keys)
  "Run 'cargo-package' for a given Cargo package."
  (if install-source?
    ;; NOTE: Cargo workspace packaging support:
    ;; #:install-source? #t + #:cargo-package-crates.
    (if (and (null? cargo-package-crates)
             skip-build?)
      (begin
        (install-file source "target/package")
        (with-directory-excursion "target/package"
          (for-each
            (lambda (file)
              (make-file-writable file)
              ;; Strip the hash and rust prefix and replace '.tar.gz' with '.crate'.
              (rename-file file
                           (string-append (string-drop-right
                                            (string-drop file 40)
                                            (string-length ".tar.gz"))
                                          ".crate")))
            (find-files "." "\\.tar\\.gz$"))))
      (begin
        ;;error: invalid inclusion of reserved file name Cargo.toml.orig in package source
        (when (file-exists? "Cargo.toml.orig")
          (delete-file "Cargo.toml.orig"))

        (if (null? cargo-package-crates)
            (apply invoke `("cargo" "package" "--offline" ,@cargo-package-flags))
            (for-each
             (lambda (pkg)
               (apply invoke "cargo" "package" "--offline" "--package" pkg
                      cargo-package-flags)
               (for-each
                (lambda (crate)
                  (invoke "tar" "xzf" crate "-C" vendor-dir))
                (find-files "target/package" "\\.crate$"))
               (patch-cargo-checksums #:vendor-dir vendor-dir))
             cargo-package-crates))

        ;; Then unpack the crate, reset the timestamp of all contained files, and
        ;; repack them.  This is necessary to ensure that they are reproducible.
        (with-directory-excursion "target/package"
          (for-each
            (lambda (crate)
              (invoke "tar" "xf" crate)
              (delete-file crate)
              ;; Some of the crate names have underscores, so we need to
              ;; search the current directory to find the unpacked crate.
              (let ((dir
                      (car (scandir "."
                                    (lambda (file)
                                      (and (not (member file '("." "..")))
                                           (not (string-suffix? ".crate" file))))))))
                ;; XXX: copied from (gnu build install)
                (for-each (lambda (file)
                            (let ((s (lstat file)))
                              (unless (eq? (stat:type s) 'symlink)
                                (utime file 0 0 0 0))))
                          (find-files dir #:directories? #t))

                (apply invoke "tar" "czf" (string-append dir ".crate")
                       ;; avoid non-determinism in the archive
                       "--sort=name" "--mtime=@0"
                       "--owner=root:0" "--group=root:0"
                       (find-files dir #:directories? #t))
                (delete-file-recursively dir)))
            (find-files "." "\\.crate$")))))
    (format #t "Not installing cargo sources, skipping `cargo package`.~%")))

(define* (install #:key
                  inputs
                  outputs
                  (skip-build? #f)
                  (install-source? #t)
                  (features '())
                  (cargo-install-paths '())
                  #:allow-other-keys)
  "Install a given Cargo package."
  (let* ((out      (assoc-ref outputs "out"))
         (registry (string-append out "/share/cargo/registry"))
         (sources  (string-append out "/share/cargo/src")))
    (mkdir-p out)

    ;; Make cargo reuse all the artifacts we just built instead
    ;; of defaulting to making a new temp directory
    (setenv "CARGO_TARGET_DIR" "./target")

    ;; Only install crates which include binary targets,
    ;; otherwise cargo will raise an error.
    (or skip-build?
        ;; NOTE: Cargo workspace installation support:
        ;; #:skip-build? #f + #:cargo-install-paths.
        (and (null? cargo-install-paths)
             (not (has-executable-target?)))
        (for-each
         (lambda (path)
           (invoke "cargo" "install" "--offline" "--no-track"
                   "--path" path "--root" out
                   "--features" (string-join features)))
         (if (null? cargo-install-paths)
             '(".")
             cargo-install-paths)))

    (when install-source?
      ;; Install crate tarballs and unpacked sources for later use.
      ;; TODO: Is there a better format/directory for these files?
      (mkdir-p sources)
      (for-each (lambda (crate)
                  (install-file crate registry))
                (find-files "target/package" "\\.crate$"))

      (for-each (lambda (crate)
                  (invoke "tar" "xzf" crate "-C" sources))
                (find-files registry "\\.crate$")))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'build 'package package)
    (add-after 'unpack 'unpack-rust-crates unpack-rust-crates)
    (add-after 'configure 'check-for-pregenerated-files check-for-pregenerated-files)
    (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums patch-cargo-checksums)))

(define* (cargo-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Cargo package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; cargo-build-system.scm ends here
