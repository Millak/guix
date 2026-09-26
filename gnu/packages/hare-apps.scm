;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025, 2026 jgart <jgart@dismail.de>
;;; Copyright © 2026 Nemin <bergengocia@protonmail.com>
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

(define-module (gnu packages hare-apps)
  #:use-module (gnu packages hare-xyz)
  #:use-module (gnu packages man)
  #:use-module (guix build-system hare)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public bonsai
  (package
    (name "bonsai")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~stacyharper/bonsai")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1453qzlc24awd17jfh3szhmrdfr3c5xvpy91pg79b2jf708xjrs5"))))
    (build-system hare-build-system)
    (inputs (list hare-ev hare-json))
    (supported-systems %hare-supported-systems)
    (home-page "https://bonsai.builtwithhare.org")
    (synopsis "Finite State Machine structured as a tree that trigger commands")
    (description "Bonsai is a Finite State Machine structured as a tree.  It
has been designed to trigger commands when successive events and/or a precise
context is accepted.  There is 4 kind of transition with specific acceptance
rules:

@itemize
@item event transition: The received event name match the transition
one
@item context transition: The state context match the transition one @item
exec transition: The transition command is run and succeed
@item delay
transition: The state wait for the delay transition duration.  No other
accepted event is received while waiting
@end itemize

The state will transition following every accepted transition.  If there is no
more available transition, the state goes back to the initial position.")
    (license license:agpl3+)))

(define-public harebrew
  (package
    (name "harebrew")
    (version "0.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~empathy_monster/harebrew")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fipxgp4jk57ww4pii6dkw3zl6aqnhab7881prxp2d8q1krvyhvk"))))
    (build-system hare-build-system)
    (arguments
     (list
      #:tests? #f ; There are no tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'patch-data-dir
            (lambda _
              (substitute* "strutils/data_dir.ha"
                (("/usr/local/share/harebrew/licenses")
                 (string-append #$output "/share/harebrew/licenses")))))
          (add-before 'install 'create-bin-directory
            (lambda _
              (mkdir-p (string-append #$output "/bin")))))))
    (supported-systems %hare-supported-systems)
    (native-inputs (list scdoc))
    (home-page "https://sr.ht/~empathy_monster/harebrew")
    (synopsis "Command-line utility to bootstrap Hare files and projects")
    (description "Harebrew is a command-line utility for initiating Hare projects.")
    (license license:gpl3+)))

(define-public hare-lsp
  (package
    (name "hare-lsp")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~whynothugo/hare-lsp")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12fqd8jh5sd7i12dbbjjyl1h1zlr6h30pqf22l0xf0sfl97zywdr"))))
    (build-system hare-build-system)
    (inputs (list hare-json))
    (supported-systems %hare-supported-systems)
    (arguments '(#:tests? #f))
    (home-page "https://git.sr.ht/~whynothugo/hare-lsp/")
    (synopsis "Language server implementation for Hare")
    (description "This package provides a language server implementation for
Hare.")
    (license license:isc)))

(define-public hare-mcron
  (package
    (name "hare-mcron")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~stacyharper/mcron")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rl8i0c0jkgr0c4zfhlma81k8985iigrq5vcpj5sdl2vg7fwwb1z"))))
    (build-system hare-build-system)
    (native-inputs (list scdoc))
    (inputs (list hare-ev))
    (supported-systems %hare-supported-systems)
    (home-page "https://mcron.builtwithhare.org/")
    (synopsis "Sleeping cron job scheduler")
    (description "Mcron is a cron implementation that is designed to be used
on suspendable machines, like mobile phones.  It will wake the device from
suspension to trigger the tasks.")
    (license license:gpl3+)))

(define-public sxmobar
  (package
    (name "sxmobar")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.sr.ht/~stacyharper/sxmobar")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dlw4sbf3awzxvglmfml5fmi9w6zmqxr2f4xqsrwchi154bixix9"))))
    (build-system hare-build-system)
    (native-inputs (list scdoc))
    (supported-systems %hare-supported-systems)
    (home-page "https://sxmobar.builtwithhare.org")
    (synopsis "Status bar component manager")
    (description "sxmobar is a status bar component manager.  It is used to
generate status lines for @command{i3status}, @command{i3bar},
@command{swaybar}, and others.")
    (license license:agpl3+)))
