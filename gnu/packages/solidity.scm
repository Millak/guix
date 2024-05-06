;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2022 Zhu Zihao  <all_but_last@163.com>
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

(define-module (gnu packages solidity)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pretty-print)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public solidity
  (package
    (name "solidity")
    (version "0.8.25")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ethereum/solidity/releases/download/v"
                       version "/solidity_" version ".tar.gz"))
       (sha256
        (base32 "0gr7mcrng7lkqx968n48js77kwz7fk8230yj0bhp1vw5hdglpxfy"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DSTRICT_Z3_VERSION=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unbundle-3rd-party-dependencies
            (lambda _
              (substitute* "CMakeLists.txt"
                (("include\\(fmtlib\\)")
                 "find_package(fmt)")
                (("include\\(range-v3\\)")
                 "find_package(range-v3)")
                (("include\\(jsoncpp\\)")
                 "find_package(jsoncpp)"))))
          (add-after 'unpack 'allow-newer-version-of-jsoncpp
            (lambda _
              (substitute* "libsolutil/JSON.cpp"
                (("JSONCPP_VERSION_PATCH ==") "JSONCPP_VERSION_PATCH >=")))))))
    (inputs
     (list boost-static fmt jsoncpp range-v3 z3))
    (native-inputs
     (list python ncurses findutils))
    (home-page "https://solidity.readthedocs.io")
    (synopsis "Contract-Oriented Programming Language")
    (description
     "Solidity is a statically-typed curly-braces programming language
designed for developing smart contracts that run on the Ethereum Virtual
Machine.")
    (license license:gpl3+)))
