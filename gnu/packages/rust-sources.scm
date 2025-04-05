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
