;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Vasilii Smirnov <vasilii.smirnov@mailbox.org>
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

(define-public specification-ip2asn-v6
  (package
    (name "specification-ip2asn-v6")
    (version "20240820024646")
    (source
     (origin
       (method url-fetch)
       ;; Upstream updates the file too often without any versioning provided,
       ;; use archived variant.
       (uri (string-append "https://web.archive.org/web/" version
                           "/https://iptoasn.com/data/ip2asn-v6.tsv.gz"))
       (sha256
        (base32 "1n2l0s6vg7v0nxprhi0gkrbqrjq5q91gra15bp2lr7p5kwki52xf"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("ip2asn-v6.tsv" "share/data/"))))
    (home-page "https://iptoasn.com")
    (synopsis "Free IP address to ASN database")
    (description
     "This package provides a collection of IPv6 addresses to
@acronym{Autonomous system, ASN}, a self-hosted API is availalbe from
@url{https://github.com/jedisct1/iptoasn-webservice}.")
    ;; It's a TSV plain text file, distributed under Public Domain
    ;; Dedication license
    ;; <https://opendatacommons.org/licenses/pddl/1.0/>.
    (license license:public-domain)))

(define-public specification-ipld
  (let ((commit "84a5cc6c168314a26be0d447c26fe76e46ce2a42")
        (revision "0"))
    (package
      (name "specification-ipld")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ipld/ipld")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0avgjp4hjkh4jmsxx09vnwi74rax6409k28h29jwfl95v42h6yyl"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("./specs/schemas" "share/ipld/specs/"))
         #:phases (modify-phases %standard-phases
                    (delete 'strip))))
      (home-page "https://ipld.io/")
      (synopsis "InterPlanetary Linked Data")
      (description
       "This package provides specification schemas of
@acronym{InterPlanetary Linked Data, IPLD} which may be used for the test
suites of application implementing the standard.")
      ;; This library is dual-licensed under either of Apache 2.0 or MIT
      ;; terms.
      (license (list license:expat license:asl2.0)))))

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

(define-public specification-runtime-spec
  (package
    (name "specification-runtime-spec")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opencontainers/runtime-spec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "102smpg308dq984f6zkjzwq5jz8jbfswiwfwxcrp1hh197jydxf9"))))
    (build-system copy-build-system)
    (outputs '("out" "golang-src"))
    (arguments
     (list
      #:install-plan
      #~'(("./schema" "share/runtime-spec/schema")
          ("." "share/doc/" #:include-regexp ("\\.md$")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'copy-src
            (lambda _
              (copy-recursively "specs-go"
               (string-append #$output:golang-src
                              "/src/github.com"
                              "/opencontainers/runtime-spec/specs-go")))))))
    (home-page "https://opencontainers.org/")
    (synopsis "OCI Runtime Specification")
    (description
     "The Open Container Initiative develops specifications for standards on
Operating System process and application containers.  This package provides
documentation, schemas and source of Golang module.")
    (license license:asl2.0)))

(define-public specification-specreduce-data
  (let ((commit "dcba1c601348ee3a5797ae2d84a068d83393058e")
        (revision "0"))
    (package
      (name "specification-specreduce-data")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/astropy/specreduce-data")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1c6mrjfykkfbbyg489h7czr65nz6bcb3gszz5xa2cg2ccs00m8ii"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("specreduce_data/" "share/specreduce_data//"))
         #:phases (modify-phases %standard-phases
                    (delete 'strip))))
      (home-page "https://specreduce.readthedocs.io/")
      (synopsis "Reference and calibration data for the reducing optical/IR spectra")
      (description
       "This package provides a general reference and calibration data for
spectroscopic data reduction (e.g. standard star spectra, atmospheric
extinction curves, line lists for calibration lamps).")
      (license license:bsd-3))))

(define-public specification-ybsc
  (package
    (name "specification-ybsc")
    (version "5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://tdc-www.harvard.edu/catalogs/ybsc" version
                           ".gz"))
       (sha256
        (base32 "1vbvxhw1i3h2r61zgqy7hpwpa4pmc0jh6mknridvx3xyj0g4pi1x"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("ybsc5" "share/ybsc5"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-documentation
            (lambda _
              (let ((doc (string-append #$output "/share/doc")))
                (mkdir-p doc)
                (with-directory-excursion doc
                  (copy-file
                   #+(origin
                       (method url-fetch)
                       (uri (string-append
                             "http://tdc-www.harvard.edu/catalogs/ybsc"
                             (package-version this-package) ".readme"))
                       (sha256
                        (base32
                         "0vki8cjwgwdfck43sbmm5d8qlk7sag6s2vqq8gp5z29nrpa0j93d")))
                   "README")
                  (copy-file
                   #+(origin
                       (method url-fetch)
                       (uri (string-append
                             "http://tdc-www.harvard.edu/catalogs/ybsc"
                             (package-version this-package) ".notes.gz"))
                       (sha256
                        (base32
                         "018lvy24pzf3ifq7dkryjnz2ny46311g4dnfifpv0rddmv7di2fl")))
                   "notes.gz")
                  (system* "gunzip" "notes.gz"))))))))
    (home-page "http://tdc-www.harvard.edu/catalogs/bsc5.html")
    (synopsis "Yale Bright Star Catalogue")
    (description
     "This package provides an ASCII version of @acronym{Yale Bright Star
Catalogue, @url{https://en.wikipedia.org/wiki/Bright_Star_Catalogue, YBSC}}
catalog which is a widely used source of basic astronomical and astrophysical
data for stars brighter than magnitude 6.5.  It provides 9110 stars including
B1950 positions, proper motions, magnitudes, and, usually, spectral types.")
    (license license:public-domain)))
