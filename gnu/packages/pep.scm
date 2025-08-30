;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages pep)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mail) ; for libetpan
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sequoia)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public yml2
  (package
    (name "yml2")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/fdik/yml2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fm1x1fv4lwcpbn59s55idzf7x173n59xpz8rlrxalmi6gvsjijr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: There is no testing framework, only a samples directory.
      #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-lxml))
    (home-page "https://fdik.org/yml/")
    (synopsis "Use a Domain Specific Language for XML without defining
a grammar")
    (description
     "The YML compiler is a small Python script.  It provides the command line
front end yml2c.  As default, it compiles your script and outputs to stdout,
that usually is the terminal.  Your shell provides options to redirect the
output into a pipe or a file.")
    (license license:gpl2)))

(define fdik-libetpan
  ;; pEp Engine requires libetpan with a set of patches that have not been
  ;; upstreamed yet.
  (let ((commit "0b80c39dd1504462ba3a39dc53db7c960c3a63f3") ; 2020-11-27
        (checksum "0gv3ivaziadybjlf6rfpv1j5z5418243v5cvl4swlxd2njsh7gjk")
        (revision "6"))
   (package
    (inherit libetpan)
    (name "fdik-libetpan")
    (version (string-append "1.6-" revision "." (string-take commit 8)))
    (source
     (origin
       (inherit (package-source libetpan))
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/pEp.foundation/libetpan")
             (commit commit)))
       (file-name (string-append name "-" version))
       (sha256 (base32 checksum)))))))

(define-public pep-engine
  (package
    (name "pep-engine")
    (version "2.1.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/pEp.foundation/pEpEngine")
             (commit (string-append "Release_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00q96y9j985qfa382acsz02i0zf6ayq2gmg8z70jzl04isg1h3cn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f
       #:make-flags '("NDEBUG=1") ; release build
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; pEpEngine does not use autotools and configure,
           ;; but a local.conf. We need to tweak the values there.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (yml2 (assoc-ref inputs "yml2")))
               (with-output-to-file "local.conf"
                 (lambda ()
                   (format #t "
PREFIX=~a
PER_MACHINE_DIRECTORY=${PREFIX}/share/pEp
SYSTEM_DB=~a/share/pEp/system.db
ASN1C=~a
YML2_PATH=~a
OPENPGP=SEQUOIA
"
                           out out (which "asn1c")
                           (string-append yml2 "/bin"))))
               #t)))
         (delete 'check)
         (add-after 'install 'install-db
           (lambda _
             (invoke "make" "-C" "db" "install"))))))
    (native-inputs
     (list asn1c ; >= 0.9.27
           pkg-config yml2))
    (inputs
     (list fdik-libetpan
           nettle
           openssl
           sequoia
           sqlite
           `(,util-linux "lib"))) ;; uuid.h
    (home-page "https://pep.foundation/")
    (synopsis "Library for automatic key management and encryption of
messages")
    (description "The p≡p engine is the core part of p≡p (pretty Easy
privacy).")
    (license ;; code: GPL 3, docs: CC-BY-SA
     (list license:gpl3 license:cc-by-sa3.0))))

(define-public libpepadapter
  (package
    (name "libpepadapter")
    (version "2.1.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/pEp.foundation/libpEpAdapter")
             (commit (string-append "Release_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09ljj3x09y99wc47n063hpn62zi8cdvdha82rnaypvirrlga6a5w"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:tests? #f ;; building the tests fails
       #:make-flags '("NDEBUG=1") ; release build
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; libpEpAdapter does not use autotools and configure,
           ;; but a local.conf. We need to tweak the values there.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (engine (assoc-ref inputs "pep-engine")))
               (with-output-to-file "local.conf"
                 (lambda _
                   (format #t "
PREFIX=~a
ENGINE_LIB_PATH=~a/lib
ENGINE_INC_PATH=~a/include
" out engine engine))))
             #t)))))
    (inputs
     (list pep-engine))
    (home-page "https://pep.foundation/")
    (synopsis "Library for building p≡p adapters")
    (description "This C++ library provides common structures used in p≡p
(pretty Easy privacy) adapters.")
    (license license:bsd-3)))

