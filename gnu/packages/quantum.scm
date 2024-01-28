;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Michal Atlas <michal_atlas+git@posteo.net>
;;; Copyright © 2024 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu packages quantum)
  #:use-module (gnu packages)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (guix build-system asdf)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public sbcl-rpcq
  (package
    (name "sbcl-rpcq")
    (version "3.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rigetti/rpcq")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-rpcq" version))
       (sha256
        (base32 "1bvppxlacvp0pfdbpn7ls1zxd127jacl225ds7lph5s8f8cyvf17"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs (list sbcl-fiasco))
    (inputs
     (list sbcl-alexandria
           sbcl-bordeaux-threads
           sbcl-cl-messagepack
           sbcl-cl-ppcre
           sbcl-cl-syslog
           sbcl-flexi-streams
           sbcl-local-time
           sbcl-parse-float
           sbcl-pzmq
           sbcl-trivial-backtrace
           sbcl-uuid
           sbcl-yason))
    (synopsis "RPC framework for Rigetti Quantum Cloud Services")
    (description
     "This package provides the asynchronous RPC client-server framework and
message specification for Rigetti Quantum Cloud Services (QCS).  It implements
an efficient transport protocol by using ZeroMQ (ZMQ) sockets and MessagePack
(msgpack) serialization.")
    (home-page "https://github.com/rigetti/rpcq")
    (license license:asl2.0)))

(define-public cl-rpcq
  (sbcl-package->cl-source-package sbcl-rpcq))

(define-public sbcl-cl-quil
  (package
    (name "sbcl-cl-quil")
    (version "1.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quil-lang/quilc")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-quil" version))
       (sha256
        (base32 "1s99lk456sw9hhsx5cf3x9r97iw9s3ydqsd94zz3bjnq77wmkhz5"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     (list
      ;; Requires cyclic dependency with qvm
      #:tests? #f))
    (inputs
     (list sbcl-abstract-classes
           sbcl-alexa
           sbcl-alexandria
           sbcl-cl-algebraic-data-type
           sbcl-cl-grnm
           sbcl-cl-heap
           sbcl-cl-permutation
           sbcl-cl-yacc
           sbcl-global-vars
           sbcl-magicl
           sbcl-optima
           sbcl-parse-float
           sbcl-queues
           sbcl-salza2
           sbcl-split-sequence
           sbcl-trivial-garbage
           sbcl-yason))
    (synopsis "The optimizing Quil compiler")
    (description
     "Quil is the quantum instruction language, originally developed at
Rigetti Computing.  In Quil quantum algorithms are expressed using Quil's
standard gates and instructions")
    (home-page "https://github.com/quil-lang/quilc")
    (license license:asl2.0)))

(define-public cl-quil
  (sbcl-package->cl-source-package sbcl-cl-quil))
