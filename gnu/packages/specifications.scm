;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages specifications)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy))

(define-public specification-multibase
  (let ((commit "4c8344e37852773de155f587dcf5897771b3fc19")
        (revision "1"))
    (package
      (name "specification-multibase")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiformats/multibase")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0lz4kdysa6nx7wf1i59647w7dgm344xpvfnsac3pfk2qdky2kq8b"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("." "share/multibase/"))))
      (home-page "https://github.com/multiformats/multibase")
      (synopsis "Self identifying base encodings")
      (description
       "Multibase is a protocol for disambiguating the encoding of
base-encoded (e.g., @code{base32}, @code{base36}, @code{base64}, @code{base58}, etc.)
binary appearing in text.")
      (license (list license:expat license:cc-by-sa3.0)))))

(define-public specification-multicodec
  (let ((commit "36789e0856be22fa02f4dc55582ec670b2b4318b")
        (revision "0"))
    (package
      (name "specification-multicodec")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiformats/multicodec")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0dyawicg8q4f8g6xj5bsj5l3c8rb8mrafjkrabc6a3p65845wp2r"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("." "share/multicodec/"))
         #:phases (modify-phases %standard-phases
                    (delete 'strip))))
      (home-page "https://github.com/multiformats/multicodec")
      (synopsis "Compact self-describing codecs")
      (description
       "Multicodec is an agreed-upon codec table.  It is designed for use in binary
representations, such as keys or identifiers (i.e @url{https://github.com/ipld/cid,
CID}).")
      (license (list license:expat license:cc-by-sa3.0)))))

(define-public specification-multihash
  (let ((commit "931febb97565395b1b6cd39ac677799df265a9e7")
        (revision "0"))
    (package
      (name "specification-multihash")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiformats/multihash")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1axr35z3iz061fng0170bh873vy20rj2mspznycxm1qkrkrh7p5j"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("." "share/multihash/"))
         #:phases (modify-phases %standard-phases
                    (delete 'strip))))
      (home-page "https://github.com/multiformats/multihash")
      (synopsis "Self-describing hashes")
      (description
       "Multihash is a protocol for differentiating outputs from various
well-established cryptographic hash functions, addressing size + encoding
considerations.")
      (license (list license:expat license:cc-by-sa3.0)))))
