;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Danny Milosavljevic <dannym@friendly-machines.com>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages c2rust)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages llvm)
  #:use-module ((guix licenses) #:prefix license:))


(define-public rust-c2rust-ast-builder-0.18
  (package
    (name "rust-c2rust-ast-builder")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-ast-builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w63rp66g6axkymxd16avxp3gjnphy3mg9938gsh52p4aak83nq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://c2rust.com/")
    (synopsis "Rust AST builder support crate for the C2Rust project")
    (description
     "This package provides the rust AST builder support crate for the
C2Rust project.")
    (license license:bsd-3)))

(define-public rust-c2rust-ast-exporter-0.18
  (package
    (name "rust-c2rust-ast-exporter")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-ast-exporter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m589a7jld5mi7zv9z179p5j90zh7mkhbmj05mgx6bbp6m45kx7p"))
       (patches (search-patches "c2rust-ast-exporter-local-search.patch"))
       (modules '((guix build utils)))))
    (build-system cargo-build-system)
    (native-inputs
     (list cmake-minimal clang))
    (inputs
     `(("llvm" ,llvm)
       ("tinycbor-src" ,%tinycbor-source)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bundle-tinycbor
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The build process will slightly patch the sources.
             (copy-recursively (assoc-ref inputs "tinycbor-src")
                               "/tmp/tinycbor")
             (setenv "GUIX_TINYCBOR_SOURCE_DIR" "/tmp/tinycbor"))))
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.65)
                       ("rust-c2rust-build-paths" ,rust-c2rust-build-paths-0.18)
                       ("rust-clang-sys" ,rust-clang-sys-1)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-env-logger" ,rust-env-logger-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-serde-cbor" ,rust-serde-cbor-0.11))))
    (home-page "https://c2rust.com/")
    (synopsis "Clang AST extraction API for use in the C2Rust project")
    (description
     "This package provides the Clang AST extraction API for use in the
C2Rust project.")
    (license license:bsd-3)))

(define-public rust-c2rust-ast-printer-0.18
  (package
    (name "rust-c2rust-ast-printer")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-ast-printer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a02bnnxn1difq917c2rv8b7654ni65lyk37hdyklv9n96inr07r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-prettyplease" ,rust-prettyplease-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://c2rust.com/")
    (synopsis "Customized version of libsyntax rust pretty-printer")
    (description
     "This package provides a customized version of libsyntax rust pretty-printer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-c2rust-bitfields-0.18
  (package
    (name "rust-c2rust-bitfields")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-bitfields" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h9cnyijk65zypv8dqbmr5r238pqq9pa8njrdzx09xhfmc3kyg5l"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           (list rust-c2rust-bitfields-derive-0.18)
           #:cargo-development-inputs
           (list rust-libc-0.2)))
    (home-page "https://c2rust.com/")
    (synopsis
     "C-compatible struct bitfield implementation used in the C2Rust project")
    (description
     "This package provides a C-compatible struct bitfield implementation used
in the C2Rust project.")
    (license license:bsd-3)))

(define-public rust-c2rust-bitfields-derive-0.18
  (package
    (name "rust-c2rust-bitfields-derive")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-bitfields-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i95j6q1d61h1m1pk84i3ih00hsmbn8ib35xr129fz2rw81c3jyk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://c2rust.com/")
    (synopsis
     "C-compatible struct bitfield derive implementation used in the C2Rust project")
    (description
     "This package provides a C-compatible struct bitfield derive implementation
used in the C2Rust project.")
    (license license:bsd-3)))

(define-public rust-c2rust-build-paths-0.18
  (package
    (name "rust-c2rust-build-paths")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-build-paths" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b2liaxbqksgfbsmr6hacdia6czlq7m0pyqx3l2rrcfcnb2ksgv0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-print-bytes" ,rust-print-bytes-1))))
    (home-page "https://c2rust.com/")
    (synopsis "C2Rust utilities related to build paths, primarily at build time")
    (description
     "This package provides C2Rust utilities related to build paths, primarily
at build time.")
    (license license:bsd-3)))

;; Note: It has expat license.
;; Note: That is supposedly the (unreleased) version 0.6.3.
(define %tinycbor-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/intel/tinycbor.git")
          (commit "d393c16f3eb30d0c47e6f9d92db62272f0ec4dc7")))
    (file-name "tinycbor-src")
    (sha256
     (base32
      "0w38lzj0rz36skc1cn3shllc82c7nn32h88frb8f164a8haq3hkw"))))

(define-public rust-c2rust-transpile-0.18
  (package
    (name "rust-c2rust-transpile")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust-transpile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09fvi2id0qjhfvsqcz9222ac81lyl2j6rbq280dhn06y1nvy000c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-c2rust-ast-builder" ,rust-c2rust-ast-builder-0.18)
                       ("rust-c2rust-ast-exporter" ,rust-c2rust-ast-exporter-0.18)
                       ("rust-c2rust-ast-printer" ,rust-c2rust-ast-printer-0.18)
                       ("rust-c2rust-bitfields" ,rust-c2rust-bitfields-0.18)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-dtoa" ,rust-dtoa-1)
                       ("rust-failure" ,rust-failure-0.1)
                       ("rust-fern" ,rust-fern-0.6)
                       ("rust-handlebars" ,rust-handlebars-4)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-log-reroute" ,rust-log-reroute-0.1)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bencode" ,rust-serde-bencode-0.2)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-strum" ,rust-strum-0.24)
                       ("rust-strum-macros" ,rust-strum-macros-0.24)
                       ("rust-syn" ,rust-syn-1))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The build process will slightly patch the sources.
             (copy-recursively (assoc-ref inputs "tinycbor-src")
                               "/tmp/tinycbor")
             (setenv "GUIX_TINYCBOR_SOURCE_DIR" "/tmp/tinycbor"))))))
    (native-inputs
     `(("clang" ,clang)
       ("cmake" ,cmake-minimal)
       ("tinycbor-src" ,%tinycbor-source)))
    (inputs (list llvm))
    (home-page "https://c2rust.com/")
    (synopsis "C2Rust transpiler implementation")
    (description "This package provides the C2Rust transpiler implementation.")
    (license license:bsd-3)))

(define-public c2rust
  (package
    (name "c2rust")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "c2rust" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05cm423m7v30b6gwgfzizhyqn3ncnfndin5zbkhyg9ah3pqccgps"))))
    (build-system cargo-build-system)
    (native-inputs (list clang cmake-minimal %tinycbor-source))
    (inputs (cons llvm (cargo-inputs 'c2rust)))
    (arguments
     (list #:install-source? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'patch
                 (lambda _
                   ;; The build process will slightly patch the sources.
                   (copy-recursively
                    #+(this-package-native-input "tinycbor-src")
                    "/tmp/tinycbor")
                   (substitute*
                       (string-append "guix-vendor/rust-c2rust-ast-exporter-"
                                      #$(package-version this-package)
                                      ".tar.gz/src/CMakeLists.txt")
                     (("GIT_TAG .*") "")
                     (("GIT_REPOSITORY .*")
                      "SOURCE_DIR \"/tmp/tinycbor\"\n")))))))
    (home-page "https://c2rust.com/")
    (synopsis "C to Rust translation, refactoring, and cross-checking")
    (description
     "This package provides C to Rust translation, refactoring, and cross-checking.")
    (license license:bsd-3)))
