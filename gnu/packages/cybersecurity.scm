;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 c4droid <c4droid@foxmail.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages cybersecurity)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
  #:use-module (gnu packages emulators))

(define-public blacksmith
  (package
    (name "blacksmith")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/comsec-group/blacksmith")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15ib0lal2sdjb4j2a4r3645w5axbd1a6j8w9f0pxr8v3ra9cjp5m"))
              (modules '((guix build utils)))
              (snippet `(begin
                          (delete-file-recursively "external")
                          (substitute* "CMakeLists.txt"
                            (("add_subdirectory\\(external\\)") "")
                            (("[ \t]*FetchContent_MakeAvailable\\(asmjit\\)")
                             (string-append
                              "find_package(asmjit)\n"
                              "find_package(nlohmann_json)")))))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test-suite
       #:imported-modules
       ((guix build copy-build-system)
        ,@%cmake-build-system-modules)
       #:modules
       (((guix build copy-build-system) #:prefix copy:)
        (guix build cmake-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build
           (lambda _
             (substitute* "CMakeLists.txt"
               ;; Use default C++ standard instead.
               (("cxx_std_17") "")
               ;; This project tries to link argagg library, which doesn't
               ;; exist, as argagg project is a single header file.
               (("argagg") ""))))
         (replace 'install
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("." "bin" #:include ("blacksmith"))
                      ("." "lib" #:include-regexp ("\\.a$")))
                    args))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list argagg asmjit nlohmann-json))
    (home-page "https://comsec.ethz.ch/research/dram/blacksmith")
    (synopsis "Rowhammer fuzzer with non-uniform and frequency-based patterns")
    (description
     "Blacksmith is an implementation of Rowhammer fuzzer that crafts novel
non-uniform Rowhammer access patterns based on the concepts of frequency,
phase, and amplitude.  It is able to bypass recent @acronym{TRR, Target Row
Refresh}in-DRAM mitigations effectively and as such can trigger bit flips.")
    (license license:expat)))

(define-public gallia
  (package
    (name "gallia")
    (version "2.0.0b2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Fraunhofer-AISEC/gallia")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bf9zq89dcnnm8ir322l69assrhxrspa97m7yk153q0vv9vib6q9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:build-backend "poetry.core.masonry.api" ;XXX: python-uv-build is required
      ;; NOTE: Test steps are sourced from GitHub Actions attached to the
      ;; project. This is a minimal test suite, more precise tests require
      ;; setting up local service with Bats (Bash Automated Testing System)
      ;; <https://bats-core.readthedocs.io/en/stable/>. bs
      #:test-flags #~(list "tests/pytest")))
    (native-inputs
     (list python-poetry-core
           python-pygit2
           python-pytest
           python-pytest-asyncio))
    (inputs
     (list python-aiofiles
           python-aiosqlite
           python-argcomplete
           python-can
           python-construct
           python-exitcode
           python-httpx
           python-more-itertools
           python-msgspec
           python-platformdirs
           python-psutil
           python-pydantic
           python-pygit2
           python-tabulate
           python-zstandard))
    (home-page "https://github.com/Fraunhofer-AISEC/gallia")
    (synopsis "Extendable Pentesting Framework")
    (description
     "Gallia is an extendable pentesting framework with the focus on the
automotive domain.  The scope of the toolchain is conducting penetration tests
from a single ECU up to whole cars.")
    (license license:apsl2)))

(define-public ropgadget
  (package
    (name "ropgadget")
    (version "7.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ropgadget" version))
       (sha256
        (base32 "1hvl25j3fbiwihqa2p8a5i27h97pgspxp2ndwwn3l1r78r7cb0w8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; TODO PyPI lack test data, Git provides a collection of binaries for
      ;; the tests.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "test-suite-binaries"
                  (invoke  "./test.sh"))))))))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-capstone))
    (home-page "https://shell-storm.org/project/ROPgadget/")
    (synopsis "Semiautomatic return oriented programming")
    (description
     "This tool lets you search for @acronym{ROP, Return Oriented Programming}
gadgets in binaries.  Some facilities are included for automatically generating
chains of gadgets to execute system calls.")
    (license license:bsd-3)))

(define-public pwntools
  (package
    (name "pwntools")
    (version "4.15.0beta1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Gallopsled/pwntools")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "048b8szybf3f69xdp258a783nl5dcgj316a5156i8ajhyfw6aaw0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f  ;XXX: needs a specific version of unicorn
      #:phases
      '(modify-phases %standard-phases
          (add-after 'unpack 'relax-dependencies
            (lambda _
              (substitute* "pyproject.toml"
                (("^ *\"pip.*\",.*")
                 "")))))))
    (propagated-inputs
     (list capstone
           python-colored-traceback
           python-dateutil
           python-intervaltree
           python-mako
           python-packaging
           python-paramiko
           python-psutil
           python-pyelftools
           python-pygments
           python-pyserial
           python-pysocks
           python-requests
           ropgadget
           python-rpyc
           python-six
           python-sortedcontainers
           python-unix-ar
           python-zstandard
           unicorn))
    (native-inputs
     (list python-setuptools python-toml python-wheel))
    (home-page "https://github.com/Gallopsled/pwntools")
    (synopsis
     "Capture-the-flag (CTF) framework and exploit development library")
    (description
     "Pwntools is a capture-the-flag (CTF) framework and exploit development library.
Written in Python, it is designed for rapid prototyping and development, and
intended to make exploit writing as simple as possible.")
    (license license:expat)))
