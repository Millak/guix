;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
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

(define-module (gnu packages rust-sources)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix search-paths)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module ((guix config) #:select (%storedir))
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages virtualization))

;;;
;;; Cargo workspaces and Rust libraries requiring external inputs to unbundle.
;;; These packages are hidden, as they are not interesting to users.
;;;

(define gemoji-source-for-rust-deunicode-1
  (let ((version "4.1.0"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/github/gemoji")
            (commit (string-append "v" version))))
      (file-name "gemoji-checkout")
      (sha256
       (base32
        "1yhs9lj9gnzbvimv0y5f1a4my0slbvygkcjjkaxd4wkkyfvfbkxy")))))

;; Workspace dependency of zed.
;;
;; Cargo side: Cargo unifies dependency versions across zed's
;; workspace, including this package's transitive dependencies,
;; and records them in zed's Cargo.lock.
;;
;; Guix side: zed's cargo-inputs entry (generated from zed's
;; Cargo.lock) already contains all of this package's transitive
;; dependencies, so this package reuses zed's cargo-inputs.
(define-public rust-alacritty-0.25.1.9d9640d
  (hidden-package
   (package
     (name "rust-alacritty")
     (version "0.25.1")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/alacritty")
          (commit "9d9640d4e56d67a09d049f9c0a300aae08d4f61e")))
        (file-name
         (git-file-name "rust-alacritty" "0.25.1.9d9640d"))
        (sha256
         (base32 "16g4jdbwdhqihc2x403fdaxf6m9gcj44cm53dyk68b3wl7chh29s"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("alacritty_terminal")
            #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                     "--exclude-lockfile")))
     (inputs (cargo-inputs 'rust-alacritty-0.25.1.9d9640d))
     (home-page "https://github.com/zed-industries/alacritty")
     (synopsis "Terminal emulator library")
     (description
      "This package provides a terminal emulator library.")
     (license license:asl2.0))))

;; Workspace dependency of zed.
;;
;; Cargo side: Cargo unifies dependency versions across zed's
;; workspace, including this package's transitive dependencies,
;; and records them in zed's Cargo.lock.
;;
;; Guix side: zed's cargo-inputs entry (generated from zed's
;; Cargo.lock) already contains all of this package's transitive
;; dependencies, so this package reuses zed's cargo-inputs.
(define-public rust-candle-0.9.1.724d75e
  (hidden-package
   (package
     (name "rust-candle")
     (version "0.9.1")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/candle")
          (commit "724d75eb3deebefe83f2a7381a45d4fac6eda383")))
        (file-name
         (git-file-name "rust-candle" "0.9.1.724d75e"))
        (sha256
         (base32 "1n9g84g0ikldw42f0j3ad975vjvx6ildwqh8dw3zi3rps518hx14"))
        (patches
         (search-patches
          "rust-candle-0.9.1-add-candle-onnx-to-workspace.patch"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("candle-core"
                                      "candle-nn"
                                      "candle-onnx")
            #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                     "--exclude-lockfile")))
     (inputs (cargo-inputs 'rust-candle-0.9.1.724d75e))
     (home-page "https://github.com/huggingface/candle")
     (synopsis "Minimalist ML framework for Rust")
     (description
      "This package provides a minimalist ML framework for Rust.")
     (license (list license:asl2.0 license:expat)))))

;; Workspace dependency of zed.
;;
;; Cargo side: Cargo unifies dependency versions across zed's
;; workspace, including this package's transitive dependencies,
;; and records them in zed's Cargo.lock.
;;
;; Guix side: zed's cargo-inputs entry (generated from zed's
;; Cargo.lock) already contains all of this package's transitive
;; dependencies, so this package reuses zed's cargo-inputs.
(define-public rust-dap-types-0.0.1.1b461b3
  (hidden-package
   (package
     (name "rust-dap-types")
     (version "0.0.1")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/dap-types")
          (commit "1b461b310481d01e02b2603c16d7144b926339f8")))
        (file-name
         (git-file-name "rust-dap-types" "0.0.1.1b461b3"))
        (sha256
         (base32 "0snc05ia4jvykajavsvyxcyn24s2jxbw7k9wmby23r1aqxf31i0z"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("dap-types")
            #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                     "--exclude-lockfile")))
     (inputs (cargo-inputs 'rust-dap-types-0.0.1.1b461b3))
     (home-page "https://github.com/zed-industries/dap-types")
     (synopsis "Rust types for the Debug Adapter Protocol")
     (description
      "This package provides Rust types for the Debug Adapter Protocol.")
     (license (list license:asl2.0 license:expat)))))

(define-public rust-deunicode-1
  (hidden-package
   (package
     (name "rust-deunicode")
     (version "1.6.1")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/kornelski/deunicode")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0bhbkfhh5qd4ygx47jj7j6h722i1skmkwashmj5wlh648f5gdmx4"))
               (modules '((guix build utils)))
               (snippet
                '(for-each delete-file-recursively
                           '("scripts/gemoji"
                             "src/mapping.txt"
                             "src/pointers.bin")))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:cargo-package-crates ''("deunicode")
            #:phases
            #~(modify-phases %standard-phases
                (add-before 'build 'compress-data
                  (lambda _
                    (with-directory-excursion "scripts"
                      (copy-recursively
                       #+(this-package-native-input "gemoji-checkout")
                       "gemoji")
                      (invoke "cargo" "run")))))))
     (native-inputs (list gemoji-source-for-rust-deunicode-1))
     ;; scripts/Cargo.toml
     (inputs (cargo-inputs 'rust-deunicode-1))
     (home-page "https://lib.rs/crates/deunicode")
     (synopsis "Convert Unicode strings to pure ASCII")
     (description
      "This package converts Unicode strings to pure ASCII by intelligently
transliterating them.  It supports Emoji and Chinese.")
     (license license:bsd-3))))

(define-public rust-deunicode-1.6.2.cfb8552
  (let ((commit "cfb8552fbbdf6d1f3f996ee4f2e78ec5e482bcef"))
    (hidden-package
     (package
       (inherit rust-deunicode-1)
       (version "1.6.2")
       (source (origin
                 (inherit (package-source rust-deunicode-1))
                 (uri (git-reference
                       (url "https://github.com/kornelski/deunicode")
                       (commit commit)))
                 (file-name (git-file-name "rust-deunicode" version))
                 ;; scripts/Cargo.toml now depends on the local deunicode
                 ;; crate, so keep the shipped generated data long enough for
                 ;; the generator to build before it overwrites them.
                 (snippet '(delete-file-recursively "scripts/gemoji"))
                 (sha256
                  (base32
                   "1khwjqx1qplwf9g1n6vgh2wk3j02j5gils95xx2kc4absbv20wdc"))))
       ;; scripts/Cargo.toml now includes a local path dependency on the
       ;; deunicode crate itself.  That self-dependency is resolved from the
       ;; checkout and does not need a cargo input.
       (inputs (cargo-inputs 'rust-deunicode-1.6.2.cfb8552))))))

;; Workspace dependency of zed.
;;
;; Cargo side: Cargo unifies dependency versions across zed's
;; workspace, including this package's transitive dependencies,
;; and records them in zed's Cargo.lock.
;;
;; Guix side: zed's cargo-inputs entry (generated from zed's
;; Cargo.lock) already contains all of this package's transitive
;; dependencies, so this package reuses zed's cargo-inputs.
(define-public rust-gh-workflow-0.8.0.c9eac0e
  (hidden-package
   (package
     (name "rust-gh-workflow")
     (version "0.8.0")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/gh-workflow")
          (commit "c9eac0ed361583e1072860d96776fa52775b82ac")))
        (file-name
         (git-file-name "rust-gh-workflow" "0.8.0.c9eac0e"))
        (sha256
         (base32 "172bn9b16f7qj3mn0h8r8bwiiiw2awgqlzj5n6f2gjcgk5nmh1hp"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("gh-workflow-macros"
                                      "gh-workflow")
            #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                     "--exclude-lockfile")))
     (inputs (cargo-inputs 'rust-gh-workflow-0.8.0.c9eac0e))
     (home-page "https://github.com/tailcallhq/gh-workflow")
     (synopsis "Type-safe GitHub Actions workflow generation")
     (description
      "This package provides type-safe GitHub Actions workflow generation.")
     (license license:expat))))

(define-public rust-hypher-0.1
  (hidden-package
   (package
     (name "rust-hypher")
     (version "0.1.6")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/typst/hypher")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0mbgfq51gf98dl8b4m7q0s66njjc4hb87qmd4lf361nmwh63m9hk"))
        (modules '((guix build utils)))
        ;; Pre-generated.
        (snippet '(for-each delete-file (find-files "tries")))))
     (build-system cargo-build-system)
     (arguments
      (list
       #:skip-build? #t
       #:cargo-package-crates ''("hypher")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'regenerate
             (lambda _
               ;; Stop the attempted dependency retrieval for a bench-marking
               ;; tool.
               (substitute* "Cargo.toml" (("^members.+$") ""))
               ;; --no-default-features lets us avoid using the upstream
               ;; source's pre-generated tries when running the
               ;; trie-regenerating "generate" test. We are throwing library
               ;; binary away, anyways, because we only want the source.
               ;; See also: <https://github.com/typst/hypher/issues/19>.
               (false-if-exception
                (invoke "cargo" "test" "--test" "generate"
                        "--no-default-features"))
               (delete-file-recursively "target"))))))
     (home-page "https://github.com/typst/hypher")
     (synopsis "Separate words into syllables")
     (description "@code{hypher} is a Rust library for syllabification.")
     (license (list license:expat license:asl2.0)))))

;; Workspace dependency of zed.
;;
;; Cargo side: Cargo unifies dependency versions across zed's
;; workspace, including this package's transitive dependencies,
;; and records them in zed's Cargo.lock.
;;
;; Guix side: zed's cargo-inputs entry (generated from zed's
;; Cargo.lock) already contains all of this package's transitive
;; dependencies, so this package reuses zed's cargo-inputs.
;;
;; Only livekit-protocol, livekit-runtime, and livekit-api are
;; packaged.  The libwebrtc, webrtc-sys, and livekit crates are
;; disabled on Linux by our zed-use-mock-livekit-on-linux patch
;; (which adds target_os = "linux" to the cfg gate), but their
;; Cargo.toml files must remain for workspace resolution; their
;; source is replaced with empty stubs.  The snippet also deletes
;; unused workspace members (livekit-ffi, soxr-sys, yuv-sys,
;; imgproc, examples) and the 1.6 MiB generated livekit.serde.rs
;; (guarded by a nonexistent "serde" feature, so never compiled).
;;
;; TODO: Regenerate livekit-protocol/src/livekit.rs (5650 lines)
;; from the .proto source in the livekit-protocol/protocol
;; submodule (github.com/livekit/protocol) using prost-build and
;; pbjson-build as native-inputs, then delete the shipped copy.
(define-public rust-livekit-0.7.8.5f04705
  (hidden-package
   (package
     (name "rust-livekit")
     (version "0.7.8")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/livekit-rust-sdks")
          (commit "5f04705ac3f356350ae31534ffbc476abc9ea83d")))
        (file-name
         (git-file-name "rust-livekit" "0.7.8.5f04705"))
        (sha256
         (base32 "1irnw9dax0nl8rl7jxdxs5vw6hg166jj4gf1s5cza6igvmrkl6y9"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (for-each delete-file-recursively
                      '(".github" "examples" "imgproc"
                        "livekit-ffi" "soxr-sys" "yuv-sys"
                        ;; Keep Cargo.toml for workspace resolution
                        ;; but replace source with stubs.
                        "libwebrtc/src"
                        "livekit/src"
                        "webrtc-sys/src"
                        "webrtc-sys/include"
                        "webrtc-sys/libwebrtc"))
            (for-each (lambda (f)
                        (mkdir-p (dirname f))
                        (call-with-output-file f (const #t)))
                      '("libwebrtc/src/lib.rs"
                        "livekit/src/lib.rs"
                        "webrtc-sys/src/lib.rs"))
            (delete-file
             "livekit-protocol/src/livekit.serde.rs")
            (substitute* "Cargo.toml"
              (("\"livekit-ffi\",") "")
              (("\"soxr-sys\",") "")
              (("\"yuv-sys\",") "")
              (("\"imgproc\",") ""))))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("livekit-protocol"
                                      "livekit-runtime"
                                      "livekit-api"
                                      "webrtc-sys-build"
                                      "webrtc-sys"
                                      "libwebrtc"
                                      "livekit")
            #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                     "--exclude-lockfile")
            #:phases
            #~(modify-phases %standard-phases
               (replace 'package
                 (lambda* (#:key source (cargo-package-crates '())
                           (cargo-package-flags '("--no-metadata" "--no-verify"))
                           (vendor-dir "guix-vendor")
                           #:allow-other-keys)
                   (use-modules (ice-9 ftw))
                   ;; Same as the standard package phase, but deletes
                   ;; cargo's target/package/tmp-crate/ scratch directory
                   ;; between the packaging loop and the repacking step.
                   ;; Cargo copies .crate files there during packaging and
                   ;; does not clean up.  The repacking step's recursive
                   ;; find-files picks them up, and scandir confuses the
                   ;; tmp-crate directory with an extracted crate when
                   ;; names sort after "tmp-crate" alphabetically.
                   (when (file-exists? "Cargo.toml.orig")
                     (delete-file "Cargo.toml.orig"))
                   (for-each
                    (lambda (pkg)
                      (apply invoke "cargo" "package" "--offline"
                             "--package" pkg cargo-package-flags)
                      (for-each
                       (lambda (crate)
                         (invoke "tar" "xzf" crate "-C" vendor-dir))
                       (find-files "target/package" "\\.crate$"))
                      ((assoc-ref %standard-phases 'patch-cargo-checksums)
                       #:vendor-dir vendor-dir))
                    cargo-package-crates)
                   ;; Delete cargo's scratch directory.
                   (when (file-exists? "target/package/tmp-crate")
                     (delete-file-recursively "target/package/tmp-crate"))
                   ;; Repack crates with deterministic timestamps.
                   (with-directory-excursion "target/package"
                     (for-each
                      (lambda (crate)
                        (invoke "tar" "xf" crate)
                        (delete-file crate)
                        (let ((dir
                               (car (scandir
                                     "."
                                     (lambda (file)
                                       (and (not (member file '("." "..")))
                                            (not (string-suffix?
                                                  ".crate" file))))))))
                          (for-each
                           (lambda (file)
                             (let ((s (lstat file)))
                               (unless (eq? (stat:type s) 'symlink)
                                 (utime file 0 0 0 0))))
                           (find-files dir #:directories? #t))
                          (apply invoke "tar" "czf"
                                 (string-append dir ".crate")
                                 "--sort=name" "--mtime=@0"
                                 "--owner=root:0" "--group=root:0"
                                 (find-files dir #:directories? #t))
                          (delete-file-recursively dir)))
                      (find-files "." "\\.crate$"))))))))
     (inputs (cargo-inputs 'rust-livekit-0.7.8.5f04705))
     (home-page "https://github.com/livekit/rust-sdks")
     (synopsis "LiveKit real-time communication SDK for Rust")
     (description
      "This package provides a LiveKit real-time communication SDK for Rust.")
     (license license:asl2.0))))

;; Workspace dependency of zed.
;;
;; Cargo side: Cargo unifies dependency versions across zed's
;; workspace, including this package's transitive dependencies,
;; and records them in zed's Cargo.lock.
;;
;; Guix side: zed's cargo-inputs entry (generated from zed's
;; Cargo.lock) already contains all of this package's transitive
;; dependencies, so this package reuses zed's cargo-inputs.
(define-public rust-notify-8.2.0.ce58c24
  (hidden-package
   (package
     (name "rust-notify")
     (version "8.2.0")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/notify.git")
          (commit "ce58c24cad542c28e04ced02e20325a4ec28a31d")))
        (file-name
         (git-file-name "rust-notify" "8.2.0.ce58c24"))
        (sha256
         (base32 "09zn3ll1vlsblgrdacyw3zx58xzdxlab8ba7h22bbb57n76dmkl0"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("notify-types"
                                      "notify")
            #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                     "--exclude-lockfile")))
     (inputs (cargo-inputs 'rust-notify-8.2.0.ce58c24))
     (home-page "https://github.com/notify-rs/notify")
     (synopsis "Cross-platform filesystem notification library")
     (description
      "This package provides a cross-platform filesystem notification library
for Rust.")
     (license (list license:asl2.0 license:expat)))))

(define-public rust-pcre2-utf32-0.2
  (hidden-package
   (package
     (name "rust-pcre2-utf32")
     (version "0.2.9")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/fish-shell/rust-pcre2")
                     (commit (string-append version "-utf32"))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0mhjw7fvrzxb3fd0c534a17qgy6svz0z8269d2fs6q8aw11610mr"))
               (modules '((guix build utils)))
               (snippet '(delete-file-recursively "pcre2-sys/upstream"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:cargo-package-crates
            ''("pcre2-sys" "pcre2")))
     (inputs (cargo-inputs 'rust-pcre2-utf32-0.2))
     (home-page "https://github.com/fish-shell/rust-pcre2")
     (synopsis "High level wrapper library for PCRE2")
     (description
      "This package provides @code{fish} shell's fork of @code{rust-pcre2} with
UTF-32 support.")
     (license (list license:expat license:unlicense)))))

(define-public rust-pipewire-0.8.0.fd3d8f7
  (let ((commit "fd3d8f7861a29c2eeaa4c393402e013578bb36d9")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-pipewire")
       (version (git-version "0.8.0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://gitlab.freedesktop.org/pipewire/pipewire-rs.git")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1hzyhz7xg0mz8a5y9j6yil513p1m610q3j9pzf6q55vdh5mcn79v"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates
              ''("libspa-sys" "libspa" "pipewire-sys" "pipewire")
              #:phases
              #~(modify-phases %standard-phases
                  ;; Avoid circular dependency.
                  (add-after 'unpack 'remove-dev-dependencies
                    (lambda _
                      (substitute* "libspa/Cargo.toml"
                        (("^pipewire.*") "")))))))
       (inputs (cargo-inputs 'rust-pipewire-0.8.0.fd3d8f7))
       (home-page "https://pipewire.org/")
       (synopsis "Rust bindings for PipeWire")
       (description "This package provides Rust bindings for PipeWire.")
       (license license:expat)))))

(define-public rust-pipewire-0.8.0.93138d0
  (let ((commit "93138d01b23628521b29b5604bbebe991cba4c65"))
    (hidden-package
     (package
       (inherit rust-pipewire-0.8.0.fd3d8f7)
       (name "rust-pipewire")
       (version (git-version "0.8.0" "0" commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gitlab.freedesktop.org/pipewire/pipewire-rs.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0zgqklzmyk893n82zq8d0is57npvy9lsfpqb83h1bbx9c42fl35p"))))
       (inputs (cargo-inputs 'rust-pipewire-0.8.0.93138d0))))))

;; Workspace dependency of zed.
;;
;; Cargo side: Cargo unifies dependency versions across zed's
;; workspace, including this package's transitive dependencies,
;; and records them in zed's Cargo.lock.
;;
;; Guix side: zed's cargo-inputs entry (generated from zed's
;; Cargo.lock) already contains all of this package's transitive
;; dependencies, so this package reuses zed's cargo-inputs.
(define-public rust-pet-0.1.0.d5b5bb0
  (hidden-package
   (package
     (name "rust-pet")
     (version "0.1.0")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/microsoft/python-environment-tools.git")
          (commit "d5b5bb0c4558a51d8cc76b514bc870fd1c042f16")))
        (file-name
         (git-file-name "rust-pet" "0.1.0.d5b5bb0"))
        (sha256
         (base32 "01djr1761l7c889v9lng7igjqxiz208r2vrwij661cn2fy5xcm5f"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("pet-fs"
                                      "pet-core"
                                      "pet-python-utils"
                                      "pet-jsonrpc"
                                      "pet-reporter"
                                      "pet-conda"
                                      "pet-virtualenv"
                                      "pet-virtualenvwrapper"
                                      "pet-env-var-path"
                                      "pet-global-virtualenvs"
                                      "pet-homebrew"
                                      "pet-linux-global-python"
                                      "pet-mac-commandlinetools"
                                      "pet-mac-python-org"
                                      "pet-mac-xcode"
                                      "pet-pipenv"
                                      "pet-pixi"
                                      "pet-poetry"
                                      "pet-pyenv"
                                      "pet-telemetry"
                                      "pet-uv"
                                      "pet-venv"
                                      "pet-windows-store"
                                      "pet-windows-registry"
                                      "pet")
            #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                     "--exclude-lockfile")
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'fix-versions
                  (lambda _
                    (let ((version #$(package-version this-package)))
                      (substitute* (find-files "." "^Cargo[.]toml$")
                        (("^(pet-[a-z-]*) = [{] path = \"([^\"]*)\" [}]" x pkg path)
                         (string-append pkg " = { version = \"" version "\", path = \"" path "\" }")))))))))
     (inputs (cargo-inputs 'rust-pet-0.1.0.d5b5bb0))
     (home-page "https://github.com/microsoft/python-environment-tools")
     (synopsis "Python environment tools")
     (description
      "This package provides Python environment tools.")
     (license license:expat))))

(define-public rust-pubgrub-0.3.0.b70cf70
  (let ((commit "b70cf707aa43f21b32f3a61b8a0889b15032d5c4")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-pubgrub")
       (version (git-version "0.3.0" revision commit))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/astral-sh/pubgrub")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "08rfk4hh2cx4v8fi62j365mwga3fgww9wcfszq7i5g4zmlhp8p8l"))
                 (modules '((guix build utils)))
                 ;; Pretend to be version 0.3.0.
                 (snippet
                  '(substitute* "Cargo.toml"
                     (("0\\.3\\.0-alpha\\.1") "0.3.0")))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates ''("version-ranges" "pubgrub")))
       (inputs (cargo-inputs 'rust-pubgrub-0.3.0.b70cf70))
       (home-page "https://github.com/pubgrub-rs/pubgrub")
       (synopsis "PubGrub version solving algorithm")
       (description
        "This package provides the @code{PubGrub} version solving algorithm.")
       (license license:mpl2.0)))))

(define-public rust-ring-0.17
  (hidden-package
   (package
     (name "rust-ring")
     (version "0.17.8")                 ;Not tagged.
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/briansmith/ring")
                     (commit "fa98b490bcbc99a01ff150896ec74c1813242d7f")))
               (file-name (git-file-name "rust-ring" version))
               (sha256
                (base32 "0rqfal81bf4l3dja98cajfjq2jbz1rcx7xdp2r33cxrm5y5psr28"))
               (patches (search-patches "rust-ring-0.17-ring-core.patch"))
               (modules '((guix build utils)))
               (snippet
                #~(begin
                    ;; It turns out Guix's nasm works just fine here.
                    (substitute* "build.rs"
                      (("./target/tools/windows/nasm/nasm") "nasm"))
                    ;; These files are pregenerated:
                    (delete-file "crypto/curve25519/curve25519_tables.h")
                    (delete-file "crypto/fipsmodule/ec/p256-nistz-table.h")
                    (delete-file "crypto/fipsmodule/ec/p256_table.h")
                    ;; As seen in git between 0.17.0 and 0.17.1.
                    (substitute* "crypto/curve25519/make_curve25519_tables.py"
                      (("static const uint8_t k25519Precomp")
                       "const uint8_t k25519Precomp"))
                    ;; This file causes problems during the 'package phase and
                    ;; is not distributed with the packaged crate.
                    (delete-file-recursively "bench")
                    (substitute* "Cargo.toml"
                      (("\"bench\",") ""))))))
     (build-system cargo-build-system)
     (arguments
      (list
       #:skip-build? #t
       #:cargo-package-crates ''("ring")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'regenerate
             (lambda _
               (setenv "HOME" (getcwd))
               (with-directory-excursion "crypto/curve25519"
                 (with-output-to-file "curve25519_tables.h"
                   (lambda _ (invoke "python3" "make_curve25519_tables.py"))))
               (with-directory-excursion "crypto/fipsmodule/ec"
                 (invoke "go" "run" "make_tables.go")
                 (invoke "go" "run" "make_ec_scalar_base_mult_tests.go"))
               (format #t "Generating the pregenerated files ...~%")
               (force-output)
               (mkdir-p "pregenerated/tmp/ring_core_generated")

               ;; We generate all the files which upstream would normally be
               ;; generate by using 'RING_PREGENERATE_ASM=1 cargo build
               ;; --target-dir=target/pregenerate_asm' in order to not include
               ;; a dependency on cargo when generating the sources.
               (define (prefix script)
                 (string-append
                  "pregenerated/"
                  (string-drop-right
                   (string-drop script
                                (string-index-right script #\/)) 3)))

               (for-each
                (lambda (script)
                  (invoke "perl" script "ios64"
                          (string-append (prefix script) "-ios64.S"))
                  (invoke "perl" script "linux64"
                          (string-append (prefix script) "-linux64.S"))
                  (invoke "perl" script "win64"
                          (string-append (prefix script) "-win64.S")))
                '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                  "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                  "crypto/chacha/asm/chacha-armv8.pl"
                  "crypto/cipher_extra/asm/chacha20_poly1305_armv8.pl"
                  "crypto/fipsmodule/aes/asm/vpaes-armv8.pl"
                  "crypto/fipsmodule/bn/asm/armv8-mont.pl"
                  "crypto/fipsmodule/ec/asm/p256-armv8-asm.pl"
                  "crypto/fipsmodule/modes/asm/ghash-neon-armv8.pl"
                  "crypto/fipsmodule/modes/asm/aesv8-gcm-armv8.pl"
                  "crypto/fipsmodule/sha/asm/sha512-armv8.pl"))

               (for-each
                (lambda (arch)
                  (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-armv8.pl"
                          arch (string-append
                                "pregenerated/sha256-armv8-" arch ".S")))
                '("ios64" "linux64" "win64"))

               (for-each
                (lambda (script)
                  (invoke "perl" script "linux32"
                          (string-append (prefix script) "-linux32.S")))
                '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                  "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                  "crypto/fipsmodule/aes/asm/bsaes-armv7.pl"
                  "crypto/fipsmodule/aes/asm/vpaes-armv7.pl"
                  "crypto/fipsmodule/bn/asm/armv4-mont.pl"
                  "crypto/chacha/asm/chacha-armv4.pl"
                  "crypto/fipsmodule/modes/asm/ghash-armv4.pl"
                  "crypto/fipsmodule/sha/asm/sha256-armv4.pl"
                  "crypto/fipsmodule/sha/asm/sha512-armv4.pl"))

               (for-each
                (lambda (script)
                  (invoke "perl" script "elf"
                          "-fPIC" "-DOPENSSL_IA32_SSE2"
                          (string-append (prefix script) "-elf.S"))
                  (invoke "perl" script "win32n"
                          "-fPIC" "-DOPENSSL_IA32_SSE2"
                          (string-append
                           "pregenerated/tmp/"
                           (string-drop (prefix script) 13) "-win32n.asm")))
                '("crypto/fipsmodule/aes/asm/aesni-x86.pl"
                  "crypto/fipsmodule/aes/asm/vpaes-x86.pl"
                  "crypto/fipsmodule/bn/asm/x86-mont.pl"
                  "crypto/chacha/asm/chacha-x86.pl"
                  "crypto/fipsmodule/modes/asm/ghash-x86.pl"))

               (for-each
                (lambda (script)
                  (invoke "perl" script "elf"
                          (string-append (prefix script) "-elf.S"))
                  (invoke "perl" script "macosx"
                          (string-append (prefix script) "-macosx.S"))
                  (invoke "perl" script "nasm"
                          (string-append
                           "pregenerated/tmp/"
                           (string-drop (prefix script) 13) "-nasm.asm")))
                '("crypto/chacha/asm/chacha-x86_64.pl"
                  "crypto/fipsmodule/aes/asm/aesni-x86_64.pl"
                  "crypto/fipsmodule/aes/asm/vpaes-x86_64.pl"
                  "crypto/fipsmodule/bn/asm/x86_64-mont.pl"
                  "crypto/fipsmodule/bn/asm/x86_64-mont5.pl"
                  "crypto/fipsmodule/ec/asm/p256-x86_64-asm.pl"
                  "crypto/fipsmodule/modes/asm/aesni-gcm-x86_64.pl"
                  "crypto/fipsmodule/modes/asm/ghash-x86_64.pl"
                  "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                  "crypto/cipher_extra/asm/chacha20_poly1305_x86_64.pl"))

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "elf" "pregenerated/sha256-x86_64-elf.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "macosx" "pregenerated/sha256-x86_64-macosx.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "nasm" "pregenerated/tmp/sha256-x86_64-nasm.asm")

               ;; TODO: Extract ring_core_generated/prefix_symbols_nasm.inc
               ;; and ring_core_generated/prefix_symbols_asm.h from build.rs.

               (for-each
                (lambda (script)
                  (invoke "nasm" "-o" (string-append (prefix script) "o")
                          "-f" "win32" "-i" "include/" "-i" "pregenerated/tmp/"
                          "-Xgnu" "-gcv8" script))
                (find-files "pregenerated/tmp" "win32n\\.asm"))

               (for-each
                (lambda (script)
                  (invoke "nasm" "-o" (string-append (prefix script) "o")
                          "-f" "win64" "-i" "include/" "-i" "pregenerated/tmp/"
                          "-Xgnu" "-gcv8" script))
                (find-files "pregenerated/tmp" "nasm\\.asm")))))))
     (native-inputs (list clang go gzip nasm perl python-minimal tar))
     (propagated-inputs (cargo-inputs 'rust-ring-0.17))
     (home-page "https://github.com/briansmith/ring")
     (synopsis "Safe, fast, small crypto using Rust")
     (description "This package provided safe, fast, small crypto using Rust.")
     (license (list license:isc license:openssl)))))

(define-public rust-ring-0.17.14
  (hidden-package
   (package
     (inherit rust-ring-0.17)
     (version "0.17.14")
     (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ring" version))
        (file-name (string-append "rust-ring-" version ".tar.gz"))
        (sha256
         (base32 "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))))
     (arguments
      (list #:skip-build? #t
            #:cargo-package-crates ''("ring"))))))

(define-public rust-rust-sdks-0.0.0.e2d1d1d
  (let ((commit "e2d1d1d230c6fc9df171ccb181423f957bb3c1f0")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-sdks")
       (version (git-version "0.0.0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/juberti-oai/rust-sdks.git")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "00xwa6w00kdv9nd4a2206wyz3aw61al914xd1g8kj4gx9bmdhic2"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:install-source? #t
              ;; rust-codex patches out the libwebrtc git dependency, so only
              ;; the remaining workspace members are currently needed here.
              #:cargo-package-crates ''("livekit-runtime"
                                        "livekit-protocol")
              #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                       "--exclude-lockfile")))
       (inputs (cargo-inputs 'rust-rust-sdks-0.0.0.e2d1d1d))
       (home-page "https://github.com/juberti-oai/rust-sdks")
       (synopsis "Workspace crates from rust-sdks")
       (description
        "This package provides the rust-sdks workspace crates used by Codex.")
       (license license:asl2.0)))))

(define-public rust-rustc-demangle-capi-0.1
  (hidden-package
   (package
     (name "rust-rustc-demangle-capi")
     (version "0.1.0")
     (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-demangle-capi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1s2g4z1yrh1sxl4qkmpd19ss3x2lr9115vbir7pnhgy63r1d63yv"))))
     (build-system cargo-build-system)
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'install-c-library
             (lambda _
               (install-file
                (car (find-files "." "^rustc_demangle\\.h$"))
                (string-append #$output "/include"))
               (install-file
                (car (find-files "." "^librustc_demangle.so$"))
                (string-append #$output "/lib")))))))
     (inputs (cargo-inputs 'rust-rustc-demangle-capi-0.1))
     (home-page "https://github.com/alexcrichton/rustc-demangle")
     (synopsis "C API for the @code{rustc-demangle} crate")
     (description "This package provides a C API library for the
@code{rustc-demangle} crate.")
     (license (list license:expat license:asl2.0)))))

(define-public rust-salsa-0.23.0.3713cd7
  (let ((commit "3713cd7eb30821c0c086591832dd6f59f2af7fe7")
        (revision "0"))
    (package
      (name "rust-salsa")
      (version (git-version "0.23.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/salsa-rs/salsa.git")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1c90zkhy9hvwqlk5is0nrs4wlpj9qzaga1z4jcdlyrdl343n7qlz"))))
      (build-system cargo-build-system)
      (arguments
       (list #:skip-build? #t
             #:cargo-package-crates
             ''("salsa-macro-rules" "salsa-macros" "salsa")))
      (inputs (cargo-inputs 'rust-salsa-0.23.0.3713cd7))
      (home-page "https://github.com/salsa-rs/salsa")
      (synopsis "Framework for incrementalized computation")
      (description "This package provides a generic framework for on-demand,
incrementalized computation (experimental).")
      (license (list license:asl2.0 license:expat)))))

(define-public rust-smithay-0.6.0.ede2707
  (let ((commit "ede27079f45eeb7c21796e22f3bc25b741b024ea")
        (revision "2"))
    (hidden-package
     (package
       (name "rust-smithay")
       (version (git-version "0.6.0" revision commit))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/Smithay/smithay")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "187vdwhy6fjjym9xmyzdhpw1r5xfdfsbbn7y8xv0x9dv12f1var2"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates ''("smithay" "smithay-drm-extras")))
       (inputs (cargo-inputs 'rust-smithay-0.6.0.ede2707))
       (home-page "https://github.com/Smithay/smithay")
       (synopsis "Smithy for Rust Wayland compositors")
       (description
        "Smithay aims to provide building blocks to create wayland compositors
in Rust.  While not being a full-blown compositor, it'll provide objects and
interfaces implementing common functionalities that pretty much any compositor
will need, in a generic fashion.

It supports the @code{wayland}, @code{wayland-protocols}, and some external
extensions, such as @code{wlr-protocols} and @code{plasma-wayland-protocols}.")
       (license license:expat)))))

(define-public rust-smithay-0.7.0.20d2dac
  (let ((commit "20d2dacd71394b5f96f6ace0a70a6f20dc62c0c6"))
    (hidden-package
     (package
       (inherit rust-smithay-0.6.0.ede2707)
       (name "rust-smithay")
       (version (git-version "0.7.0" "0" commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/Smithay/smithay")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0h1q1jgyg76axr8h95nv8sg29l15iqnyfr5qfl5rk2wc7iw04avl"))))
       (inputs (cargo-inputs 'rust-smithay-0.7.0.20d2dac))))))

(define inspired-github-color-scheme-for-rust-syntect-5
  (let ((version "1.3.0"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/sethlopez/InspiredGitHub.tmtheme")
            (commit (string-append "v" version))))
      (file-name "inspired-github-color-scheme-checkout")
      (sha256
       (base32
        "0w2sswa2kid1jwqy28xqvjav17xzkza32i9vvyj67m1kfm3dd6ww")))))

(define solarized-for-rust-syntect-5
  (let ((version "1.5.11"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/braver/Solarized")
            (commit version)))
      (file-name "solarized-checkout")
      (sha256
       (base32
        "05n8wq7zahydrnx36k7awqjz8svn13xsxcazyj0909h4akbsglj1")))))

(define spacegray-for-rust-syntect-5
  (let ((commit "2703e93f559e212ef3895edd10d861a4383ce93d"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/SublimeText/Spacegray")
            (commit commit)))
      (file-name "spacegray-checkout")
      (sha256
       (base32
        "0vzs9i3sdh6f1b25vdbxwyphmxzbqixrnjlgws56fzfngy4my9dj")))))

(define-public rust-syntect-5.2
  (hidden-package
   (package
     (name "rust-syntect")
     (version "5.2.0")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/trishume/syntect")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1wr5x6jy53s597j7kfyzhwph1d07a18qc45s47cx4f399f0xwk9l"))
               (modules '((guix build utils)))
               (snippet
                '(begin
                   (delete-file-recursively "scripts")
                   (for-each
                    (lambda (file)
                      (delete-file file)
                      (with-output-to-file file
                        (const (display "\n"))))
                    (find-files "assets" "dump$"))))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:cargo-package-crates ''("syntect")
            #:phases
            #~(modify-phases %standard-phases
                (replace 'build
                  (lambda _
                    (substitute* "Makefile"
                      (("git submodule.*") ""))
                    (with-directory-excursion "testdata"
                      (rmdir "InspiredGitHub.tmtheme")
                      (copy-recursively
                       #+(this-package-native-input
                          "inspired-github-color-scheme-checkout")
                       "InspiredGitHub.tmtheme")
                      (rmdir "Solarized")
                      (copy-recursively
                       #+(this-package-native-input "solarized-checkout")
                       "Solarized")
                      (rmdir "spacegray")
                      (copy-recursively
                       #+(this-package-native-input "solarized-checkout")
                       "spacegray"))
                    (invoke "make" "assets"))))))
     (native-inputs
      (list pkg-config
            inspired-github-color-scheme-for-rust-syntect-5
            solarized-for-rust-syntect-5
            spacegray-for-rust-syntect-5))
     (inputs (cons oniguruma (cargo-inputs 'rust-syntect-5.2)))
     (home-page "https://github.com/trishume/syntect")
     (synopsis "Library for syntax highlighting and code intelligence")
     (description
      "This package provides a library for syntax highlighting and code
intelligence.")
     (license license:expat))))

(define-public rust-syntect-5.3
  (hidden-package
    (package
      (inherit rust-syntect-5.2)
      (name "rust-syntect")
      (version "5.3.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/trishume/syntect")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "095yvdd4lpzfywcg5zr7pq28fm9iarx6a85mpgdc0g6d1waydp68"))
         (modules '((guix build utils)))
         (snippet (origin-snippet (package-source rust-syntect-5.2)))))
      (inputs (cons oniguruma
                    (cargo-inputs 'rust-syntect-5.3))))))

;; Workspace dependency of zed.
;;
;; Cargo side: Cargo unifies dependency versions across zed's
;; workspace, including this package's transitive dependencies,
;; and records them in zed's Cargo.lock.
;;
;; Guix side: zed's cargo-inputs entry (generated from zed's
;; Cargo.lock) already contains all of this package's transitive
;; dependencies, so this package reuses zed's cargo-inputs.
(define-public rust-tiktoken-rs-0.9.1.2570c43
  (hidden-package
   (package
     (name "rust-tiktoken-rs")
     (version "0.9.1")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/tiktoken-rs")
          (commit "2570c4387a8505fb8f1d3f3557454b474f1e8271")))
        (file-name
         (git-file-name "rust-tiktoken-rs" "0.9.1.2570c43"))
        (sha256
         (base32 "0yki1whx16xqgsrnr6gjsxakj6qzyvz958g6c9h15xp154kw6573"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("tiktoken-rs")
            #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                     "--exclude-lockfile")))
     (inputs (cargo-inputs 'rust-tiktoken-rs-0.9.1.2570c43))
     (home-page "https://github.com/zurawiki/tiktoken-rs")
     (synopsis "Rust library for OpenAI's BPE tokenizer")
     (description
      "This package provides Rust bindings for OpenAI's @acronym{BPE, byte
pair encoding} tokenizer, used to count and manage tokens for OpenAI
language models.")
     (license license:expat))))

(define-public rust-tikv-jemallocator-for-polars
  (let ((commit "c7991e5bb6b3e9f79db6b0f48dcda67c5c3d2936")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-tikv-jemallocator")
       (version (git-version "0.6.0" revision commit))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/pola-rs/jemallocator")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "0wwdw0f3a9vgck3x10gxq80606b2wam31vglhjw2fabdvq2wmxcy"))))
       (build-system cargo-build-system)
       (arguments
           (list #:skip-build? #t
                 #:cargo-package-crates
                 ''("tikv-jemalloc-sys"
                    "tikv-jemallocator")
                 #:phases
                 #~(modify-phases %standard-phases
                     (add-after 'unpack 'remove-workspace-members
                       (lambda _
                         ; Avoid dev dependency, that is excluded from workspace
                         (substitute* "jemallocator/Cargo.toml"
                           (("^tikv-jemalloc-ctl.*$") "")))))))
       (inputs (cargo-inputs 'rust-tikv-jemallocator-for-polars))
       (home-page "https://github.com/pola-rs/jemallocator")
       (synopsis "Rust allocator backed by jemalloc")
       (description "This package provides a Rust allocator backed by jemalloc.")
       (license (list license:expat license:asl2.0))))))

(define-public rust-codex-0.117.0
  (hidden-package
   (package
     (name "rust-codex")
     (version "0.117.0")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/openai/codex")
              (commit "4c70bff480af37b1bf1a9b352b8341060fe55755")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0xnszwd3xhh4j64zxlk611kcphvrw4ihky1f517y4m8cl4lpqdqk"))
        (patches (search-patches
                  "rust-codex-0.117.0-core-remove-self-dep.patch"
                  "codex-acp-0.11.1-disable-code-mode.patch"))))
     (build-system cargo-build-system)
     (arguments
      (list
       #:skip-build? #t
       #:cargo-package-crates
       ;; All workspace crates from openai/codex 0.117.0.
       ;; Order matters: dependencies must come before packages that need them.
       ''(;; Topologically sorted by internal dependency order.
          "codex-utils-absolute-path"
          "codex-git-utils"
          "codex-experimental-api-macros"
          "codex-execpolicy"
          "codex-utils-cache"
          "codex-utils-image"
          "codex-utils-string"
          "codex-protocol"
          "codex-utils-cargo-bin"
          "codex-app-server-protocol"
          "codex-utils-rustls-provider"
          "codex-client"
          "codex-config"
          "codex-keyring-store"
          "codex-terminal-detection"
          "codex-login"
          "codex-utils-plugins"
          "codex-plugin"
          "codex-analytics"
          "codex-ansi-escape"
          "codex-api"
          "codex-apply-patch"
          "codex-async-utils"
          "codex-code-mode"
          "codex-connectors"
          "codex-instructions"
          "codex-otel"
          "codex-skills"
          "codex-core-skills"
          "codex-utils-pty"
          "codex-exec-server"
          "codex-features"
          "codex-hooks"
          "codex-utils-home-dir"
          "codex-network-proxy"
          "codex-rmcp-client"
          "codex-file-search"
          "codex-state"
          "codex-utils-path"
          "codex-rollout"
          "codex-sandboxing"
          "codex-secrets"
          "codex-shell-command"
          "codex-shell-escalation"
          "codex-utils-output-truncation"
          "codex-utils-readiness"
          "codex-utils-stream-parser"
          "codex-utils-template"
          "codex-windows-sandbox"
          "codex-core"
          "codex-linux-sandbox"
          "codex-arg0"
          "codex-backend-openapi-models"
          "codex-backend-client"
          "codex-utils-cli"
          "codex-chatgpt"
          "codex-cloud-requirements"
          "codex-feedback"
          "codex-utils-json-to-toml"
          "codex-app-server"
          "codex-app-server-client"
          "codex-app-server-test-client"
          "codex-cloud-tasks-client"
          "codex-utils-approval-presets"
          "codex-utils-elapsed"
          "codex-utils-fuzzy-match"
          "codex-lmstudio"
          "codex-ollama"
          "codex-utils-oss"
          "codex-utils-sandbox-summary"
          "codex-utils-sleep-inhibitor"
          "codex-exec"
          "codex-mcp-server"
          "codex-process-hardening"
          "codex-responses-api-proxy"
          "codex-stdio-to-uds"
          "codex-debug-client"
          "codex-execpolicy-legacy")
       #:phases
       #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-workspace
            (lambda _
              (chdir "codex-rs")))
          (add-after 'chdir-to-workspace 'patch-git-deps-to-vendor
            (lambda _
              (substitute* "Cargo.toml"
                (("crossterm = \\{ git = [^}]+\\}")
                 "crossterm = { version = \"0.28.1\" }")
                (("ratatui = \\{ git = [^}]+\\}")
                 "ratatui = { version = \"0.29.0\" }")
                (("tokio-tungstenite = \\{ git = [^}]+\\}")
                 "tokio-tungstenite = { version = \"0.28.0\" }")
                (("tungstenite = \\{ git = [^}]+\\}")
                 "tungstenite = { version = \"0.27.0\" }")
                (("nucleo = \\{ git = [^}]+\\}")
                 "nucleo = { version = \"0.5.0\" }")
                (("runfiles = \\{ git = [^}]+\\}")
                 "runfiles = { version = \"0.1.0\" }"))
              ;; Disable V8 runtime in codex-code-mode.
              (substitute* "Cargo.toml"
                (("codex-code-mode = \\{ path = \"code-mode\" \\}")
                 "codex-code-mode = { path = \"code-mode\", default-features = false }"))))
          (add-after 'patch-git-deps-to-vendor 'add-version-to-workspace-deps
            (lambda _
              ;; cargo package requires all dependencies to have versions.
              ;; Add version = "0.117.0" to internal path dependencies.
              (let ((cargo-files (find-files "." "^Cargo\\.toml$")))
                (substitute* cargo-files
                  (("(codex-[a-z0-9-]+) = \\{ path = " all name)
                   (string-append name " = { version = \"0.117.0\", path = "))
                  (("(codex-[a-z0-9-]+) = \\{ package = " all name)
                   (string-append name " = { version = \"0.117.0\", package = "))
                  (("^(path = \"\\.\\./[^\"]*\")" all path-line)
                   (string-append path-line "\nversion = \"0.117.0\"")))))))))
     (inputs (cargo-inputs 'rust-codex-0.0.0.785c0c43))
     (home-page "https://github.com/openai/codex")
     (synopsis "OpenAI Codex workspace crates (for codex-acp)")
     (description
      "This package provides the workspace crates from the OpenAI Codex
repository, used as dependencies by codex-acp.")
     (license license:asl2.0))))

(define-public rust-codex-0.120.0
  (hidden-package
   (package
     (name "rust-codex")
     (version "0.120.0")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/openai/codex")
              (commit "65319eb1400cbd2890c43d572263dabd25f18ba9")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0gqzkqndb8jwvb9j5dxqgidyzk67x00ccfzhg54gdlv4adc1cgwj"))
        (modules '((guix build utils)))
        (snippet '(begin
                    ;;; These are JSON manifests with a dotslash
                    ;;; shebang that download and run pre-built
                    ;;; binaries (ripgrep, bash) at runtime.
                    (delete-file "codex-cli/bin/rg")
                    ;; Bundled bubblewrap source tree; includes a
                    ;; compiled BPF blob (demos/flatpak.bpf).
                    (delete-file-recursively "codex-rs/vendor/bubblewrap")))
        (patches (search-patches
                  "codex-acp-0.11.1-disable-code-mode.patch"
                  "rust-codex-0.120.0-core-remove-self-dep.patch"
                  "rust-codex-0.120.0-remove-libwebrtc.patch"))))
     (build-system cargo-build-system)
     (arguments
      (list
       #:skip-build? #t
       #:cargo-package-crates
       ;; Order matters: dependencies must come before packages that need them
       ''(;; Topologically sorted by internal dependency order.
          "codex-experimental-api-macros"
          "codex-utils-absolute-path"
          "codex-git-utils"
          "codex-async-utils"
          "codex-execpolicy"
          "codex-utils-home-dir"
          "codex-utils-rustls-provider"
          "codex-network-proxy"
          "codex-utils-cache"
          "codex-utils-image"
          "codex-utils-string"
          "codex-utils-template"
          "codex-protocol"
          "codex-shell-command"
          "codex-utils-cargo-bin"
          "codex-app-server-protocol"
          "codex-client"
          "codex-api"
          "codex-otel"
          "codex-features"
          "codex-model-provider-info"
          "codex-config"
          "codex-keyring-store"
          "codex-terminal-detection"
          "codex-login"
          "codex-utils-plugins"
          "codex-plugin"
          "codex-analytics"
          "codex-ansi-escape"
          "codex-utils-pty"
          "codex-exec-server"
          "codex-apply-patch"
          "codex-code-mode"
          "codex-connectors"
          "codex-instructions"
          "codex-skills"
          "codex-core-skills"
          "codex-feedback"
          "codex-hooks"
          "codex-rmcp-client"
          "codex-mcp"
          "codex-collaboration-mode-templates"
          "codex-response-debug-context"
          "codex-utils-output-truncation"
          "codex-models-manager"
          "codex-file-search"
          "codex-state"
          "codex-utils-path"
          "codex-rollout"
          "codex-sandboxing"
          "codex-secrets"
          "codex-shell-escalation"
          "codex-tools"
          "codex-utils-readiness"
          "codex-utils-stream-parser"
          "codex-windows-sandbox"
          "codex-core"
          "codex-linux-sandbox"
          "codex-arg0"
          "codex-backend-openapi-models"
          "codex-backend-client"
          "codex-utils-cli"
          "codex-chatgpt"
          "codex-cloud-requirements"
          "codex-utils-json-to-toml"
          "codex-app-server"
          "codex-app-server-client"
          "codex-app-server-test-client"
          "codex-cloud-tasks-client"
          "codex-cloud-tasks-mock-client"
          "codex-utils-approval-presets"
          "codex-utils-elapsed"
          "codex-utils-fuzzy-match"
          "codex-lmstudio"
          "codex-ollama"
          "codex-utils-oss"
          "codex-utils-sandbox-summary"
          "codex-utils-sleep-inhibitor"
          "codex-realtime-webrtc"
          "codex-exec"
          "codex-mcp-server"
          "codex-process-hardening"
          "codex-responses-api-proxy"
          "codex-stdio-to-uds"
          "codex-debug-client"
          "codex-execpolicy-legacy")
       #:phases
       #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-workspace
            (lambda _
              (chdir "codex-rs")))
          (add-after 'chdir-to-workspace 'patch-git-deps-to-vendor
            (lambda _
              (substitute* "Cargo.toml"
                (("crossterm = \\{ git = [^}]+\\}")
                 "crossterm = { version = \"0.28.1\" }")
                (("ratatui = \\{ git = [^}]+\\}")
                 "ratatui = { version = \"0.29.0\" }")
                (("tokio-tungstenite = \\{ git = [^}]+\\}")
                 "tokio-tungstenite = { version = \"0.28.0\" }")
                (("nucleo = \\{ git = [^}]+\\}")
                 "nucleo = { version = \"0.5.0\" }")
                (("runfiles = \\{ git = [^}]+\\}")
                 "runfiles = { version = \"0.1.0\" }"))
              ;; Disable V8 runtime in codex-code-mode.
              (substitute* "Cargo.toml"
                (("codex-code-mode = \\{ path = \"code-mode\" \\}")
                 "codex-code-mode = { path = \"code-mode\", default-features = false }"))))
          (add-after 'patch-git-deps-to-vendor 'add-version-to-workspace-deps
            (lambda _
              ;; cargo package requires all dependencies to have versions.
              ;; Add version = "0.120.0" to internal path dependencies.
              (let ((cargo-files (find-files "." "^Cargo\\.toml$")))
                (substitute* cargo-files
                  (("(codex-[a-z0-9-]+) = \\{ path = " all name)
                   (string-append name " = { version = \"0.120.0\", path = "))
                  (("(codex-[a-z0-9-]+) = \\{ package = " all name)
                   (string-append name " = { version = \"0.120.0\", package = "))
                  (("^(path = \"\\.\\./[^\"]*\")" all path-line)
                   (string-append path-line "\nversion = \"0.120.0\"")))))))))
     (inputs (cargo-inputs 'rust-codex-0.0.0.785c0c43))
     (home-page "https://github.com/openai/codex")
     (synopsis "OpenAI Codex workspace crates")
     (description
      "This package provides the workspace crates for the OpenAI Codex CLI
and runtime for AI-assisted coding.")
     (license license:asl2.0))))

;; Also update (@ (gnu packages gnome) glycin-loaders) when updating this.
(define-public rust-glycin-3
  (package
    (name "rust-glycin")
    (version "3.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.gnome.org/GNOME/glycin.git")
              ;; Rust library version is different from the tagged version.
              (commit "2.0.7")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0v6szxk5h8a4b28xb0lrhqrq6b4vka2ha4qgpdp35f6c49v9pdyp"))
       (patches (search-patches "glycin-sandbox-Adapt-bwrap-invocation.patch"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:cargo-package-crates ''("glycin-common" "glycin-utils" "glycin")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-bin-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "glycin/src/sandbox.rs"
                (("@bwrap@")
                 (search-input-file inputs "bin/bwrap"))
                (("@storedir@")
                   #$%storedir)
                (("/usr/bin/true")
                 (search-input-file inputs "bin/true"))))))))
    (inputs (cons* bubblewrap (cargo-inputs 'glycin)))
    ;; Uses XDG_DATA_DIRS to search for the loaders. See Config::data_dirs() in
    ;; config.rs.  Using a specific search path was discussed in
    ;; <https://codeberg.org/guix/guix/pulls/1653> and
    ;; <https://gitlab.gnome.org/GNOME/glycin/-/merge_requests/343>.
    (native-search-paths (list $XDG_DATA_DIRS))
    (home-page "https://gitlab.gnome.org/GNOME/glycin")
    (synopsis "Rust library for sandboxed image decoding")
    (description "Glycin is a sandbox image decoder for image viewers and
thumbnails to display untrusted content safely.")
    (license (list license:mpl2.0 license:lgpl2.1+))))

;; Workspace dependency of zed.
;;
;; Cargo side: Cargo unifies dependency versions across zed's
;; workspace, including this package's transitive dependencies,
;; and records them in zed's Cargo.lock.
;;
;; Guix side: zed's cargo-inputs entry (generated from zed's
;; Cargo.lock) already contains all of this package's transitive
;; dependencies, so this package reuses zed's cargo-inputs.
(define-public rust-zed-xim-0.4.0-zed.16f35a2
  (hidden-package
   (package
     (name "rust-zed-xim")
     (version "0.4.0-zed")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/xim-rs.git")
          (commit "16f35a2c881b815a2b6cdfd6687988e84f8447d8")))
        (file-name
         (git-file-name "rust-zed-xim" "0.4.0-zed.16f35a2"))
        (sha256
         (base32 "14cyxxdgjpbkf8ny4c46cfqjinavi2cyxzwfngldlls97m5zh555"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("xim-ctext"
                                      "xim-parser"
                                      "zed-xim")
            #:cargo-package-flags ''("--no-metadata" "--no-verify"
                                     "--exclude-lockfile")
            #:phases
            #~(modify-phases %standard-phases
               (replace 'package
                 (lambda* (#:key source (cargo-package-crates '())
                           (cargo-package-flags '("--no-metadata" "--no-verify"))
                           (vendor-dir "guix-vendor")
                           #:allow-other-keys)
                   (use-modules (ice-9 ftw))
                   ;; Same as the standard package phase, but deletes
                   ;; cargo's target/package/tmp-crate/ scratch directory
                   ;; between the packaging loop and the repacking step.
                   (when (file-exists? "Cargo.toml.orig")
                     (delete-file "Cargo.toml.orig"))
                   (for-each
                    (lambda (pkg)
                      (apply invoke "cargo" "package" "--offline"
                             "--package" pkg cargo-package-flags)
                      (for-each
                       (lambda (crate)
                         (invoke "tar" "xzf" crate "-C" vendor-dir))
                       (find-files "target/package" "\\.crate$"))
                      ((assoc-ref %standard-phases 'patch-cargo-checksums)
                       #:vendor-dir vendor-dir))
                    cargo-package-crates)
                   (when (file-exists? "target/package/tmp-crate")
                     (delete-file-recursively "target/package/tmp-crate"))
                   (with-directory-excursion "target/package"
                     (for-each
                      (lambda (crate)
                        (invoke "tar" "xf" crate)
                        (delete-file crate)
                        (let ((dir
                               (car (scandir
                                     "."
                                     (lambda (file)
                                       (and (not (member file '("." "..")))
                                            (not (string-suffix?
                                                  ".crate" file))))))))
                          (for-each
                           (lambda (file)
                             (let ((s (lstat file)))
                               (unless (eq? (stat:type s) 'symlink)
                                 (utime file 0 0 0 0))))
                           (find-files dir #:directories? #t))
                          (apply invoke "tar" "czf"
                                 (string-append dir ".crate")
                                 "--sort=name" "--mtime=@0"
                                 "--owner=root:0" "--group=root:0"
                                 (find-files dir #:directories? #t))
                          (delete-file-recursively dir)))
                      (find-files "." "\\.crate$"))))))))
     (inputs (cargo-inputs 'rust-zed-xim-0.4.0-zed.16f35a2))
     (home-page "https://github.com/XDeme1/xim-rs")
     (synopsis "X input method client and server")
     (description
      "This package provides a library for @code{xim}, the X input method.")
     (license license:expat))))
