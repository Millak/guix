;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023-2025 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python))

;;;
;;; Cargo workspaces and Rust libraries requiring external inputs to unbundle.
;;; These packages are hidden, as they are not interesting to users.
;;;

(define-public rust-bitcode-for-swww
  (let ((commit "5f25a59be3e66deef721e7eb2369deb1aa32d263")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-bitcode")
       (version (git-version "0.6.0" revision commit))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/SoftbearStudios/bitcode")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "07fa6l9jmjrbylqv4dwlagsbgh0h1j3b6g1mbvfnxsk69zv6ylhg"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates ''("bitcode_derive" "bitcode")))
       (inputs (cargo-inputs 'rust-bitcode-for-swww))
       (home-page "https://github.com/SoftbearStudios/bitcode")
       (synopsis "Binary encoder and decoder for Rust")
       (description
        "This package provides a binary encoder and decoder for Rust.")
       (license (list license:expat license:asl2.0))))))

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

(define-public rust-pipewire-for-niri
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
       (inputs (cargo-inputs 'rust-pipewire-for-niri))
       (home-page "https://pipewire.org/")
       (synopsis "Rust bindings for PipeWire")
       (description "This package provides Rust bindings for PipeWire.")
       (license license:expat)))))

(define-public rust-pubgrub-for-uv
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
       (inputs (cargo-inputs 'rust-pubgrub-for-uv))
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

(define-public rust-smithay-for-niri
  (let ((commit "0cd3345c59f7cb139521f267956a1a4e33248393")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-smithay")
       (version (git-version "0.4.0" revision commit))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/Smithay/smithay")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "191h87bpzg0l1ihfb4hmx00b86pfb5mwwc6s8i49al0vigc14l37"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates ''("smithay" "smithay-drm-extras")))
       (inputs (cargo-inputs 'rust-smithay-for-niri))
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
