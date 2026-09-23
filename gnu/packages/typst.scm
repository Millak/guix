;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.com>
;;; Copyright © 2025 Alexis Simon <alexis.simon@runbox.com>
;;; Copyright © 2026 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2026 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (gnu packages typst)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system typst)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tls))

(define-public prettypst
  (package
    (name "prettypst")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/antonWetzel/prettypst")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0727anhd2wl967m8k5z3bxb37h45nsjbbjz8akjn3mq96cfbfsbw"))))
    (build-system cargo-build-system)
    (arguments (list #:install-source? #f))
    (inputs (cargo-inputs 'prettypst))
    (home-page "https://github.com/antonWetzel/prettypst")
    (synopsis "Configurable formatter for Typst")
    (description
     "Prettypst is a configurable source file formatter for the Typst
typesetting system.")
    (license license:expat)))

(define-public typst
  (package
    (name "typst")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/typst/typst")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "typst-support-GUIX_TYPST_PACKAGE_PATH.patch"))
       (sha256
        (base32 "0y727lyicn3ciq36mdpbcg8d09naff39y1i52467mlmr11p0l9xa"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:imported-modules (append %copy-build-system-modules
                                 %cargo-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build copy-build-system) #:prefix copy:)
                  (guix build utils))
      #:cargo-install-paths ''("crates/typst-cli")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version-string
            (lambda _
              (setenv "TYPST_VERSION" #$(package-version this-package))))
          (add-after 'unpack 'fix-dev-assets
            (lambda _
              (substitute* "Cargo.toml"
                (("typst-dev-assets = \\{[^}]*\\}")
                 (string-append "typst-dev-assets = {version=\""
                                #$version
                                "\"}")))))
          (add-after 'configure 'configure-artifacts
            (lambda _
              (setenv "GEN_ARTIFACTS" (string-append (getcwd) "/artifacts"))))
          (add-after 'install 'install-artifacts
            (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("artifacts/typst.bash"
                         "share/bash-completion/completions/typst")
                        ("artifacts/typst.elv"
                         "share//elvish/lib/typst")
                        ("artifacts/typst.fish"
                         "share/fish/vendor_completions.d/")
                        ("artifacts/_typst"
                         "share/zsh/site-functions/")
                        ("artifacts/" "share/man/man1/"
                         #:include-regexp ("\\.1$")))
                      args))))))
    (inputs (cons* openssl (cargo-inputs 'typst)))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_TYPST_PACKAGE_PATH")
            (files '("share/typst/packages")))
           (search-path-specification
            (variable "TYPST_FONT_PATHS")
            (files '("share/fonts" "share/texmf-dist/fonts")))))
    (home-page "https://typst.app/")
    (synopsis "LaTeX-like typesetting system")
    (description
     "Typst is a markup-based typesetting system that is designed to be as
powerful as LaTeX while being much easier to learn and use.  Features include
built-in markup for math typesetting, bibliography management and other common
tasks, an extensible scripting system for uncommon tasks, incremental
compilation, and intuitive error messages.")
    (license license:asl2.0)))

(define-public typstyle
  (package
    (name "typstyle")
    (version "0.13.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typstyle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06mg12rkls1hkiz8wxchj1jqf1l1bq963s80mrvjfiajb08zqdx1"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:modules
      '((guix build cargo-build-system)
        (guix build utils)
        (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-completions
            (lambda* (#:key native-inputs #:allow-other-keys)
              (for-each
               (match-lambda
                 ((shell . path)
                  (mkdir-p (in-vicinity #$output (dirname path)))
                  (let ((binary
                         (if #$(%current-target-system)
                             (search-input-file native-inputs "bin/typstyle")
                             (in-vicinity #$output "bin/typstyle"))))
                    (with-output-to-file (in-vicinity #$output path)
                      (lambda _
                        (invoke binary "completions" shell))))))
               '(("bash"   . "share/bash-completion/completions/typstyle")
                 ("elvish" . "share/elvish/lib/typstyle")
                 ("fish"   . "share/fish/vendor_completions.d/typstyle.fish")
                 ("zsh"    . "share/zsh/site-functions/_typstyle"))))))))
    (native-inputs
     (if (%current-target-system)
         (list this-package)
         '()))
    (inputs (cargo-inputs 'typstyle))
    (home-page "https://enter-tainer.github.io/typstyle/")
    (synopsis "Consistent formatter for Typst")
    (description
     "Typstyle is a formatter for the Typst typesetting system designed with
universal consistency and correctness as top priorities.  It is
configuration-free.")
    (license license:asl2.0)))

(define-public cetz-core-wasm
  (package
    (name "cetz-core-wasm")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cetz-package/cetz")
              (commit (string-append "v" version))))
       (file-name (git-file-name "cetz" version))
       (sha256
        (base32 "179nxacfxzkb23h3ikgk0x7m4003913zq64yn6l9zfz5qvfal0sh"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:tests? #f                       ; tests require rand
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _ (chdir "cetz-core")))
          (add-after 'configure 'configure-target
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((sysroot
                     (assoc-ref inputs
                                "rust-sysroot-for-wasm32-unknown-unknown")))
                (with-atomic-file-replacement ".cargo/config"
                  (lambda (in out)
                    (display (string-append "\
[target.wasm32-unknown-unknown]
linker = 'lld'

[build]
target = ['wasm32-unknown-unknown']
") out)
                    (dump-port in out)))
                (setenv "RUSTFLAGS" (string-append "--sysroot " sysroot)))))
          (replace 'install
            (lambda _
              (install-file
               (car (find-files "target" "cetz_core.wasm"))
               (string-append #$output "/share/typst/packages/preview/cetz/"
                              #$version "/cetz-core")))))))
    (inputs (cargo-inputs 'cetz-core))
    (native-inputs (list lld rust-sysroot-for-wasm32-unknown-unknown))
    (home-page "https://cetz-package.github.io")
    (synopsis "Library for drawing in Typst")
    (description "CeTZ is a library for drawing figures in Typst,
similar to TikZ.  This package provides the core WebAssembly primitives
used by the actual CeTZ package.")
    (license license:lgpl3+)))

(define cetz-common
  (let ((commit "fa1d1181f45609edf85b9632c18ce950fe87d048"))
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/cetz-package/common")
             (commit commit)))
      (file-name (git-file-name "cetz-common" (string-take commit 7)))
      (sha256
       (base32 "16934mjzy5glhw80vm80p1k1dr9l7yyj7s9425j8k3a0awykgyq0")))))

(define-public typst-cetz
  (package
    (inherit cetz-core-wasm)
    (name "typst-cetz")
    (build-system typst-build-system)
    (arguments
     (list #:tests? #f                  ; tt not yet packaged
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'skip-build
                 (lambda _
                   (substitute* "justfile"
                     ((": build") ":"))))
               (add-after 'unpack 'unpack-scripts
                 (lambda* (#:key inputs #:allow-other-keys)
                   (mkdir-p "common")
                   (copy-recursively (dirname
                                      (search-input-file inputs "/scripts/package"))
                                     "common/scripts"))))))
    (propagated-inputs (list cetz-core-wasm typst-oxifmt))
    (native-inputs (list bash-minimal
                         cetz-common
                         just
                         perl))))

(define-public typst-oxifmt
  (package
    (name "typst-oxifmt")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/PgBiel/typst-oxifmt")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0mwh8jmck9cxkq9prsjlmxd5msyvfd2a2si4xqc43sqw2dgiqivk"))))
    (build-system typst-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "typst" "compile"
                             "--root=."
                             "tests/strfmt-tests.typ")))))))
    (home-page "https://github.com/PgBiel/typst-oxifmt")
    (synopsis "Convenient string formatting")
    (description "This package provides string formatting and interpolation
through the @code{strfmt} function, with syntax similar to Rust's format
strings.")
    (license (list license:expat license:asl2.0))))
