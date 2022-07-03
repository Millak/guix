;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2022 Zhu Zihao  <all_but_last@163.com>
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
    (version "0.8.15")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ethereum/solidity/releases/download/v"
                       version "/solidity_" version ".tar.gz"))
       (sha256
        (base32 "0j9a8y5fizarl9yhbnwvd0x1nm6qsbskqb7j1fwsyqx47w5sa82p"))))
    (build-system cmake-build-system)
    (arguments
     (list
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
                 "find_package(jsoncpp)")))))))
    (inputs
     (list boost-static fmt-for-solidity jsoncpp range-v3 z3))
    (native-inputs
     (list python ncurses findutils))
    (home-page "https://solidity.readthedocs.io")
    (synopsis "Contract-Oriented Programming Language")
    (description
     "Solidity is a statically-typed curly-braces programming language
designed for developing smart contracts that run on the Ethereum Virtual
Machine.")
    (license license:gpl3+)))
