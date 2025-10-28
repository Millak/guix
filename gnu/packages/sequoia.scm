;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021, 2023-2025 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages sequoia)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)  ; glibc
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define-public sequoia-chameleon-gnupg
  (package
    (name "sequoia-chameleon-gnupg")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-chameleon-gnupg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qpanr7ydsr79adbn4bxwgqmhhq4wn7y90lw42a0g6p71x2cg73h"))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:install-source? #f
       #:features '(list "crypto-nettle")
       #:cargo-test-flags
       '(list "--"
              ;; Some tests overly depend on specific versions of input crates.
              "--skip=gpg::generate_key"
              "--skip=gpg::list_keys"
              "--skip=gpg::migrate::migration_from_secring"
              "--skip=gpg::print_mds"
              "--skip=gpg::quick::add_key_default_default_iso_date"
              "--skip=gpg::quick::generate_key_default_default_iso_date"
              "--skip=gpg::sign"
              "--skip=gpg::verify")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'set-asset-out-dir
             (lambda _
               (setenv "ASSET_OUT_DIR" "target/assets")))
           (add-after 'install 'install-more
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (share (string-append out "/share"))
                      (bash-completions-dir
                        (string-append out "/etc/bash_completion.d"))
                      (zsh-completions-dir
                        (string-append share "/zsh/site-functions"))
                      (fish-completions-dir
                        (string-append share "/fish/vendor_completions.d"))
                      (elvish-completions-dir
                        (string-append share "/elvish/lib"))
                      (man1 (string-append share "/man/man1")))
                 ;; The completions are generated in build.rs.
                 (mkdir-p bash-completions-dir)
                 (mkdir-p elvish-completions-dir)
                 (for-each (lambda (file)
                             (install-file file man1))
                           (find-files "target/assets/man-pages" "\\.1$"))
                 (copy-file "target/assets/shell-completions/gpg-sq.bash"
                            (string-append bash-completions-dir "/gpg-sq"))
                 (copy-file "target/assets/shell-completions/gpgv-sq.bash"
                            (string-append bash-completions-dir "/gpgv-sq"))
                 (copy-file "target/assets/shell-completions/gpg-sq.elv"
                            (string-append elvish-completions-dir "/gpg-sq"))
                 (copy-file "target/assets/shell-completions/gpgv-sq.elv"
                            (string-append elvish-completions-dir "/gpgv-sq"))
                 (install-file "target/assets/shell-completions/_gpg-sq"
                               zsh-completions-dir)
                 (install-file "target/assets/shell-completions/_gpgv-sq"
                               zsh-completions-dir)
                 (install-file "target/assets/shell-completions/gpg-sq.fish"
                               fish-completions-dir)
                 (install-file "target/assets/shell-completions/gpgv-sq.fish"
                               fish-completions-dir)))))))
    (inputs
     (cons* nettle openssl sqlite (cargo-inputs 'sequoia-chameleon-gnupg)))
    (native-inputs
     (list clang gnupg pkg-config sequoia-sq))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Sequoia's reimplementation of the GnuPG interface")
    (description "This package provides Sequoia's reimplementation of the
@code{GnuPG} interface.

@code{gpg-sq} is Sequoia's alternative implementation of a tool following the
GnuPG command line interface.  It provides a drop-in but not feature-complete
replacement for the GnuPG project's @code{gpg}.

This Guix package is built to use the nettle cryptographic library.")
    (license license:gpl3+)))

(define-public sequoia-sq
  (package
    (name "sequoia-sq")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-sq" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01aph6n9lj7qcz1n8gr6q48an4ypn9d6xzxfprwpw80wv9ibc12w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:features '("crypto-nettle"
                    "sequoia-keystore/gpg-agent"
                    "sequoia-keystore/openpgp-card"
                    "sequoia-keystore/softkeys")
       #:cargo-test-flags
       (list "--"
             ;; The certificate has an expiration date.
             "--skip=sq_autocrypt_import")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-asset-out-dir
           (lambda _
             (setenv "ASSET_OUT_DIR" "target/assets")))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (bash-completions-dir
                     (string-append out "/etc/bash_completion.d"))
                    (zsh-completions-dir
                     (string-append share "/zsh/site-functions"))
                    (fish-completions-dir
                     (string-append share "/fish/vendor_completions.d"))
                    (elvish-completions-dir
                     (string-append share "/elvish/lib"))
                    (man1 (string-append share "/man/man1")))
               ;; The completions are generated in build.rs.
               (mkdir-p bash-completions-dir)
               (mkdir-p elvish-completions-dir)
               (for-each (lambda (file)
                           (install-file file man1))
                         (find-files "target/assets/man-pages" "\\.1$"))
               (copy-file "target/assets/shell-completions/sq.bash"
                          (string-append bash-completions-dir "/sq"))
               (install-file "target/assets/shell-completions/_sq"
                             zsh-completions-dir)
               (install-file "target/assets/shell-completions/sq.fish"
                             fish-completions-dir)
               (copy-file "target/assets/shell-completions/sq.elv"
                          (string-append elvish-completions-dir "/sq"))))))))
    (inputs
     (cons* nettle openssl pcsc-lite sqlite (cargo-inputs 'sequoia-sq)))
    (native-inputs
     (list capnproto clang pkg-config))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Command-line frontend for Sequoia OpenPGP")
    (description "This package provides the command-line frontend for Sequoia
OpenPGP.

This Guix package is built to use the nettle cryptographic library and the
gpg-agent, openpgp-card and softkeys keystore backends.")
    (license license:lgpl2.0+)))

(define-public sequoia-sqv
  (package
    (name "sequoia-sqv")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-sqv" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0q2ylgzpnx290mwp8626j14b5z318074ag5dj99282vj4qfmf949"))))
    (build-system cargo-build-system)
    (inputs
     (cons* nettle openssl (cargo-inputs 'sequoia-sqv)))
    (native-inputs
     (list clang pkg-config))
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-asset-out-dir
           (lambda _
             (setenv "ASSET_OUT_DIR" "target/assets")))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (bash-completions-dir
                     (string-append out "/etc/bash_completion.d"))
                    (zsh-completions-dir
                     (string-append share "/zsh/site-functions"))
                    (fish-completions-dir
                     (string-append share "/fish/vendor_completions.d"))
                    (elvish-completions-dir
                     (string-append share "/elvish/lib"))
                    (man1 (string-append share "/man/man1")))
               ;; The completions are generated in build.rs.
               (mkdir-p bash-completions-dir)
               (mkdir-p elvish-completions-dir)
               (for-each (lambda (file)
                           (install-file file man1))
                         (find-files "target/assets/man-pages" "\\.1$"))
               (copy-file "target/assets/shell-completions/sqv.bash"
                          (string-append bash-completions-dir "/sqv"))
               (install-file "target/assets/shell-completions/_sqv"
                             zsh-completions-dir)
               (install-file "target/assets/shell-completions/sqv.fish"
                             fish-completions-dir)
               (copy-file "target/assets/shell-completions/sqv.elv"
                          (string-append elvish-completions-dir "/sqv"))))))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Simple OpenPGP signature verification program")
    (description "@code{sqv} verifies detached OpenPGP signatures.  It is a
replacement for @code{gpgv}.  Unlike @code{gpgv}, it can take additional
constraints on the signature into account.

This Guix package is built to use the nettle cryptographic library.")
    (license license:lgpl2.0+)))

(define-public sequoia-wot-tools
  (package
    (name "sequoia-wot-tools")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.com/sequoia-pgp/sequoia-wot")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03wa4l582hx0qq28pkhrf0pagckyx6df01pb4chyngadk49q1xn7"))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:features '(list "sequoia-openpgp/crypto-nettle")
       #:cargo-package-crates ''("tools")
       #:cargo-test-flags '(list "--" "--skip=gpg_trust_roots")
       #:install-source? #f
       #:cargo-install-paths ''("tools")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'install-more
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out   (assoc-ref outputs "out"))
                      (share (string-append out "/share"))
                      (man1  (string-append share "/man/man1")))
                 (for-each (lambda (file)
                             (install-file file man1))
                           (find-files "target/release" "\\.1$"))
                 (mkdir-p (string-append out "/etc/bash_completion.d"))
                 (mkdir-p (string-append share "/fish/vendor_completions.d"))
                 (mkdir-p (string-append share "/elvish/lib"))
                 (copy-file (car (find-files "target/release" "sq-wot.bash"))
                            (string-append out "/etc/bash_completion.d/sq-wot"))
                 (copy-file (car (find-files "target/release" "sq-wot.fish"))
                            (string-append
                              share "/fish/vendor_completions.d/sq-wot.fish"))
                 (copy-file (car (find-files "target/release" "sq-wot.elv"))
                            (string-append share "/elvish/lib/sq-wot"))
                 (install-file (car (find-files "target/release" "_sq-wot"))
                               (string-append
                                 share "/zsh/site-functions"))))))))
    (inputs
     (cons* nettle openssl sqlite (cargo-inputs 'sequoia-wot-tools)))
    (native-inputs
     (list clang pkg-config))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Implementation of OpenPGP's web of trust")
    (description
     "This package provides an implementation of @code{OpenPGP's} web of trust.

This Guix package is built to use the nettle cryptographic library.")
    (license license:lgpl2.0+)))

;;

(define-public sequoia
  (package
    (name "sequoia")
    (version "2.0.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build union)
                  (guix build gnu-build-system)
                  (guix build gremlin)
                  (system vm elf))
      #:builder
      #~(begin
          (use-modules (guix build utils)
                       (guix build union)
                       (guix build gnu-build-system)
                       (ice-9 match))
          (let ((make-dynamic-linker-cache
                 (assoc-ref %standard-phases 'make-dynamic-linker-cache))
                (ld.so.cache
                 (string-append #$output "/etc/ld.so.cache")))
            (match %build-inputs
                   (((names . directories) ...)
                    (union-build #$output directories)))
            (delete-file ld.so.cache)
            (setenv "PATH"
                    (string-append (getenv "PATH") ":" #$glibc "/sbin"))
            (make-dynamic-linker-cache #:outputs %outputs)))))
    (inputs
     (list ;glibc ;; for ldconfig in make-dynamic-linker-cache
           sequoia-sq
           sequoia-sqv
           sequoia-wot-tools))
    (home-page "https://sequoia-pgp.org")
    (synopsis "New OpenPGP implementation (meta-package)")
    (description "Sequoia is a new OpenPGP implementation, written in Rust,
consisting of several Rust crates/packages.  This Guix meta-package combines
these packages into a single one for convenience.  Anyhow, you should not
depend other packages on this one avoid excessive compile-times for users.")
    (license license:lgpl2.0+)))
