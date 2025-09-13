;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018, 2019, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018-2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020, 2022, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2023, 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 TakeV <takev@disroot.org>
;;; Copyright © 2023 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2024 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Vinicius Monego <monego@posteo.net>
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

(define-module (gnu packages python-compression)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages check)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages sphinx))

(define-public python-blosc
  (package
    (name "python-blosc")
    (version "1.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "blosc" version))
       (sha256
        (base32
         "13h8ks58iy4h3ayk7havb4hmkma88598qkf4i4paj53qpa76bvc9"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "blosc/c-blosc")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'custom
      #:test-flags #~(list "-m" "blosc.test")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'find-blosc
            (lambda _
              (setenv "USE_SYSTEM_BLOSC" "1")
              (setenv "Blosc_ROOT" #$(this-package-input "c-blosc")))))))
    (propagated-inputs
     (list python-scikit-build))
    (inputs
     (list c-blosc))
    (native-inputs
     (list cmake-minimal
           ninja/pinned
           python-numpy
           python-psutil
           python-py-cpuinfo
           python-setuptools))
    (home-page "https://github.com/blosc/python-blosc")
    (synopsis "Python wrapper for the Blosc data compressor library")
    (description "Blosc is a high performance compressor optimized for binary
data.  It has been designed to transmit data to the processor cache faster
than the traditional, non-compressed, direct memory fetch approach via a
@code{memcpy()} system call.

Blosc works well for compressing numerical arrays that contains data with
relatively low entropy, like sparse data, time series, grids with
regular-spaced values, etc.

This Python package wraps the Blosc library.")
    (license license:bsd-3)))

(define-public python-blosc2
  (package
    (name "python-blosc2")
    (version "2.7.1")                   ;3.0.0 requires numpy>=1.25
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "blosc2" version))
       (sha256
        (base32 "1s4gpdf1hfbw5w3hpx0g8bfwjrws1b8wgmh7snafh5ivai0lvnrl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--pyargs" "blosc2")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'configure
            (lambda _
              (setenv "USE_SYSTEM_BLOSC2" "ON"))))))
    (inputs (list c-blosc2))
    (propagated-inputs
     (list python-msgpack
           python-ndindex
           python-numexpr
           python-numpy
           python-py-cpuinfo))
    (native-inputs
     (list cmake-minimal
           pkg-config
           python-cython
           python-pytest
           python-scikit-build
           python-setuptools))
    (home-page "https://github.com/blosc/python-blosc2")
    (synopsis "Python wrapper for the Blosc2 data compressor library")
    (description
     "Blosc2 is a high performance compressor optimized for binary
data.  It has been designed to transmit data to the processor cache faster
than the traditional, non-compressed, direct memory fetch approach via a
@code{memcpy()} system call.

Python-Blosc2 wraps the C-Blosc2 library, and it aims to leverage its new API
so as to support super-chunks, multi-dimensional arrays, serialization and
other features introduced in C-Blosc2.

Python-Blosc2 also reproduces the API of Python-Blosc and is meant to be able
to access its data, so it can be used as a drop-in replacement.")
    (license license:bsd-3)))

(define-public python-brotli
  (package
    (name "python-brotli")
    (version "1.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/brotli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fikasxf7r2dwlk8mv8w7nmjkn0jw5ic31ky3mvpkdzwgd4xfndl"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
           ;; Cherry-picked from upstream since the latest release
           ;; https://github.com/google/brotli/commit/09b0992b6acb7faa6fd3b23f9bc036ea117230fc
           (substitute* (find-files "scripts" "^lib.*pc\\.in")
             (("-R\\$\\{libdir\\} ") ""))))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools))
    (home-page "https://github.com/google/brotli")
    (synopsis "Python interface to Brotli")
    (description "This package provides a Python interface to the @code{brotli}
package, an implementation of the Brotli lossless compression algorithm.")
    (license license:expat)))

(define-deprecated-package python-google-brotli
  python-brotli)

(define-public python-multivolumefile
  (package
    (name "python-multivolumefile")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "multivolumefile" version))
       (sha256
        (base32
         "1mh9sz50s1p8ik83a455pqd57syprad7xhfmk28yb5mwmw58sr50"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools-scm
           python-coverage
           python-coveralls
           python-hypothesis
           python-pytest
           python-pytest-cov
           python-setuptools
           python-wheel))
    (home-page "https://github.com/miurahr/multivolume")
    (synopsis "Treat multiple files as one")
    (description "MultiVolumefile is a Python library that provides a
file-object abstraction, making it possible to use multiple files as if they
were a single file.")
    (license license:lgpl2.1+)))

(define-public python-cramjam
  (package
    (name "python-cramjam")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cramjam" version))
       (sha256
        (base32 "12kdwr313b8w8il4x1y9z366armd6lqv3hvpx4281bl4fd4ds8g8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules
                           ,@%pyproject-build-system-modules)
      #:modules '(((guix build cargo-build-system) #:prefix cargo:)
                  (guix build pyproject-build-system)
                  (guix build utils))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'prepare-cargo-build-system
                     (lambda args
                       (for-each
                        (lambda (phase)
                          (format #t "Running cargo phase: ~a~%" phase)
                          (apply (assoc-ref cargo:%standard-phases phase)
                                 #:cargo-target #$(cargo-triplet)
                                 args))
                        '(unpack-rust-crates
                          configure
                          check-for-pregenerated-files
                          patch-cargo-checksums)))))))
    (native-inputs
     (append
      (list maturin
            pkg-config
            python-pytest
            python-pytest-xdist
            python-numpy
            python-hypothesis
            rust
            `(,rust "cargo"))
      (or (and=> (%current-target-system)
                 (compose list make-rust-sysroot))
          '())))
    (inputs
     (cons* libdeflate
            lz4
            `(,zstd "lib")
            (cargo-inputs 'python-cramjam)))
    (home-page "https://github.com/milesgranger/cramjam")
    (synopsis "Python bindings to compression algorithms in Rust")
    (description
     "This package provides thin Python bindings to compression and
decomporession algorithms implemented in Rust.  This allows for using
algorithms such as Snappy without additional system dependencies.  The
following algorithms are available:

@itemize
@item Snappy
@item Brotli
@item Bzip2
@item LZ4
@item Gzip
@item Deflate
@item Zstd
@end itemize")
    (license license:expat)))

(define-public python-ewah-bool-utils
  (package
    (name "python-ewah-bool-utils")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ewah_bool_utils" version))
       (sha256
        (base32 "1hvs1fvf3g7kq6hnzxyxfrwvmykw503cmxf1l3irs67gr931z47b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/yt-project/ewah_bool_utils")
    (synopsis "EWAH Bool Array compression")
    (description "This package provide a Python wrapper to @acronym{EWAH,
 Enhanced Word-Aligned Hybrid} compression bitarray method.")
    (license (list
              ;; LICENSE: for Python code.
              license:bsd-3
              ;; XXX: Check if it's possible to have it as a dedicated package
              ;; and link here..
              ;; ewah_bool_utils/cpp/LICENSE: for C++ bundle, sourced from
              ;; <https://github.com/lemire/EWAHBoolArray>.
              license:asl2.0))))

(define-public python-pybcj
  (package
    (name "python-pybcj")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pybcj" version))
              (sha256
               (base32
                "1hvm3c3mb20z25kmbzyyn6pr5inx50z0ignl8b0bggxaik82ws4b"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-importlib-metadata))
    (native-inputs
     (list python-coverage
           python-hypothesis
           python-pytest
           python-pytest-cov
           python-setuptools-scm
           python-setuptools
           python-wheel))
    (home-page "https://codeberg.org/miurahr/pybcj")
    (synopsis "BCJ filter library")
    (description "In data compression, BCJ, short for Branch-Call-Jump, refers
to a technique that improves the compression of machine code of executable
binaries by replacing relative branch addresses with absolute ones. This
allows a LZMA compressor to identify duplicate targets and archive higher
compression rate.  BCJ is used in the 7-zip compression utility as the default
filter for executable binaries.

pybcj provides Python bindings to a BCJ implementation in C.")
    (license license:lgpl2.1+)))

;; XXX: Project is archived and not maintained since 2021.
(define-public python-bcj-cffi
  (package
    (name "python-bcj-cffi")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bcj-cffi" version))
       (sha256
        (base32 "1k6h9x8j65hssbgmvhl71sdjj9aq8d81drdibrdflaz7a895sjib"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-cffi))
    (home-page "https://github.com/miurahr/bcj-cffi")
    (synopsis "Branch / Call /Jump CFFI library in Python")
    (description "This package provides an implementation of the Branch / Call /
Jump conversion filter by CFFI for Python.")
    (license license:lgpl2.1+)))

(define-public python-brotlicffi
  (package
    (name "python-brotlicffi")
    (version "1.0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "brotlicffi" version))
       (sha256
        (base32 "15kxgdiqcg0cm6h5xq3vkbhw7674673hcx3n2yicd3wx29l8l90c"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (delete-file-recursively "libbrotli")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-shared-brotli
            (lambda _
              (setenv "USE_SHARED_BROTLI" "1")))
          (add-before 'check 'set-brotli-source
            (lambda _
              (let* ((brotli-source
                      #+(package-source (this-package-input "brotli")))
                     (brotli-test-data
                      (string-append brotli-source "/tests/testdata"))
                     (brotli-version-source
                      (string-append brotli-source "/c/common/version.h")))
                (substitute* "test/conftest.py"
                  (("TEST_DATA_DIR = .*")
                   (format #f "TEST_DATA_DIR = ~s~%" brotli-test-data)))
                (substitute* "test/test_compatibility.py"
                  (("open\\(version_h\\)")
                   (format #f "open(~s)" brotli-version-source)))))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (inputs
     (list brotli))
    (propagated-inputs
     (list python-cffi))
    (home-page "https://github.com/python-hyper/brotlicffi")
    (synopsis "Python CFFI bindings to the Brotli library")
    (description
     "This package provides Python CFFI bindings to the Brotli library.")
    (license license:expat)))

(define-public python-inflate64
  (package
    (name "python-inflate64")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "inflate64" version))
              (sha256
               (base32
                "0767j35gkwaykl1iq9qn8rc25j1ggv56x3d1vzjpk89bzpzdhbdm"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-importlib-metadata))
    (native-inputs
     (list python-pytest
           python-setuptools-scm
           python-setuptools
           python-wheel))
    (home-page "https://pypi.org/project/inflate64/")
    (synopsis "Compression/decompression library")
    (description "The @code{inflate64} package provides @code{Deflater} and
@code{Inflater} classes to compress and decompress with the Enhanced Deflate
compression algorithm.")
    (license license:lgpl2.1+)))

(define-public python-isal
  (package
    (name "python-isal")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "isal" version))
       (sha256
        (base32 "1d7j30922v547vnif171yhk1jml9cv14izda0w506qhslglk6hhj"))
       ;; Remove bundled isa-l source code
       (modules '((guix build utils)))
       (snippet
        #~(delete-file-recursively "src/isal/isa-l"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-dynamic-linking
            (lambda _
              (setenv "PYTHON_ISAL_LINK_DYNAMIC" "1"))))))
    (inputs (list isa-l))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (home-page "https://github.com/pycompression/python-isal")
    (synopsis "Python bindings for the ISA-L compression library")
    (description
     "This package aims to provide faster zlib and gzip compatible compression
and decompression by implementing Python bindings for the ISA-L library.")
    (license license:expat)))

(define-public python-pylsqpack
  (package
    (name "python-pylsqpack")
    (version "0.3.22")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pylsqpack" version))
              (sha256
               (base32
                "1npcdj416gqc5zvlkyh9z808k381lrm56zvz1zsdjw437hdp2zxn"))))
    ;; FIXME: Unbundle ls-qpack and xxhash!
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/aiortc/pylsqpack")
    (synopsis "Python bindings for @code{ls-qpack}")
    (description
     "@code{pylsqpack} is a wrapper around the @code{ls-qpack} library.
It provides Python Decoder and Encoder objects to read or write HTTP/3
headers compressed with QPACK.")
    (license license:expat)))

(define-public python-pyppmd
  (package
    (name "python-pyppmd")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyppmd" version))
              (sha256
               (base32
                "03w4x26mar0ha73c3v39psn1i0k6xrzwmaxfsxysic73jz99np07"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-coverage
           python-hypothesis
           python-pytest
           python-pytest-benchmark
           python-pytest-cov
           python-pytest-timeout
           python-setuptools-scm
           python-setuptools
           python-wheel))
    (home-page "https://github.com/miurahr/pyppmd")
    (synopsis "PPMd compression/decompression library")
    (description "Pyppmd provides classes and functions for compressing and
decompressing text data, using the @dfn{Prediction by partial matching} (PPM)
compression algorithm variation H and I.2.  It provides an API similar to
Python's zlib/bz2/lzma modules.")
    (license license:lgpl2.1+)))

;; XXX: Project is archived and not maintained since 2022.
(define-public python-ppmd-cffi
  (package
    (name "python-ppmd-cffi")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ppmd-cffi" version))
       (sha256
        (base32
         "0vprpl29fkflqx0m6anfpx7q7i4cw0d0qxcdm91k4pl82dcad81g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; AssertionError: assert False
      #:test-flags #~(list "--deselect=tests/test_cli.py::test_cli_help")))
    (propagated-inputs
     (list python-cffi))
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-setuptools
           python-setuptools-scm))
    (home-page "https://github.com/miurahr/ppmd")
    (synopsis "Prediction by Partial Matching compression library")
    (description "PPMd is a compression algorithm library using the Prediction
by Partial Matching statistical technique.  It is used in RAR and 7-Zip as one of
several possible methods.")
    (license license:lgpl2.1+)))

(define-public python-py7zr
  (package
    (name "python-py7zr")
    (version "0.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py7zr" version))
       (sha256
        (base32
         "0lwniinfr3rb10n0c203a09vz06vxnnj637yqn8ipdlml89gj7kr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--ignore=tests/test_benchmark.py")))
    (propagated-inputs
     (list python-brotli
           python-brotlicffi
           python-importlib-metadata
           python-inflate64
           python-multivolumefile
           python-psutil
           python-pybcj
           python-pycryptodomex
           python-pyppmd
           python-pyzstd
           python-texttable))
    (native-inputs
     (list python-setuptools
           python-libarchive-c
           python-py-cpuinfo
           python-pyannotate
           python-pytest
           python-pytest-benchmark
           python-pytest-remotedata
           python-pytest-timeout
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/miurahr/py7zr")
    (synopsis "7-zip in Python")
    (description "This package provides py7zr, which implements 7-zip
archive compression, decompression, encryption and decryption in
Python.")
    (license license:lgpl2.1+)))

(define-public python-lzo
  (package
    (name "python-lzo")
    (version "1.15")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-lzo" version))
       (sha256
        (base32 "0jbv6853p8flk65ks0nw37f6f5v0ryi6nhppv5fm3863ql0alym5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-setuppy
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "setup.py"
                (("include_dirs.append\\(.*\\)")
                 (format #f "include_dirs.append(~s)"
                         (search-input-directory inputs "include/lzo")))))))))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (inputs (list lzo))
    (home-page "https://github.com/jd-boyd/python-lzo")
    (synopsis "Python bindings for the LZO data compression library")
    (description
     "Python-LZO provides Python bindings for LZO, i.e. you can access
the LZO library from your Python scripts thereby compressing ordinary
Python strings.")
    (license license:gpl2+)))

(define-public python-lz4
  (package
    (name "python-lz4")
    (version "4.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lz4" version))
       (sha256
        (base32
         "1nmb757fx3k30zsjiaz7nj6cgp4zxl44w28s4l8k0ff4grid03q7"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Remove bundled copy of lz4.
                   (delete-file-recursively "lz4libs")))))
    (build-system pyproject-build-system)
    (arguments
     ;; Taken from tox.ini (excludes experimental tests).
     (list #:test-flags #~(list "tests/block" "tests/frame")))
    (native-inputs
     (list pkg-config
           python-pytest
           python-pkgconfig
           python-setuptools
           python-setuptools-scm
           python-psutil))
    (inputs
     (list lz4))
    (home-page "https://github.com/python-lz4/python-lz4")
    (synopsis "LZ4 bindings for Python")
    (description
     "This package provides python bindings for the lz4 compression library
by Yann Collet.  The project contains bindings for the LZ4 block format and
the LZ4 frame format.")
    (license license:bsd-3)))

(define-public python-lzstring
  (package
    (name "python-lzstring")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lzstring" version))
       (sha256
        (base32 "18ly9pppy2yspxzw7k1b23wk77k7m44rz2g0271bqgqrk3jn3yhs"))))
    (build-system pyproject-build-system)
    (arguments
     ;; No tests in PyPI, this project is a fork of
     ;; <https://github.com/eduardtomasek/lz-string-python> and doesn't provide
     ;; tags.
     (list #:tests? #f))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-future))
    (home-page "https://github.com/gkovacs/lz-string-python")
    (synopsis "String compression")
    (description "Lz-string is a string compressor library for Python.")
    (license license:expat)))

(define-public python-python-snappy
  (package
    ;; PyPI contains both `snappy' and `python-snappy' as completely distinct
    ;; packages. To avoid a name collision in Guix, we use the variable name
    ;; `python-python-snappy' for the package called `python-snappy' on PyPI.
    (name "python-python-snappy")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python_snappy" version))
       (sha256
        (base32 "1qyfhsaagpzgrw5n2zklx670zi0f3lm1djqyg2n3hbgvmldnq8a0"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-backend #~'unittest))
    (native-inputs
     (list python-cramjam
           python-setuptools))
    (inputs
     (list snappy))
    (home-page "https://github.com/andrix/python-snappy")
    (synopsis "Python bindings for the Snappy compression library")
    (description
     "@code{python-python-snappy} provides bindings to the Snappy library and
can be used to compress and decompress files and streams.  It can also be used
directly from the command line.")
    (license license:bsd-3)))

(define-public python-bitshuffle
  (package
    (name "python-bitshuffle")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bitshuffle" version))
              (sha256
               (base32
                "139xz3m2m8sal8riicvmb9i0sq4085s2hc6c148bwhmzpnvky3nw"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; TODO Remove bundled libraries: lz4, lzf, and zstd.
                  ;; Remove generated Cython files.
                  (delete-file "bitshuffle/h5.c")
                  (delete-file "bitshuffle/ext.c")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; FileNotFoundError: [Errno 2] Unable to synchronously open file
      ;; (unable to open file: name =
      ;; '/tmp/<...>/tests/data/regression_0.1.3.h5', errno = 2, error message
      ;; = 'No such file or directory', flags = 0, o_flags = 0)
      #~(list "--deselect=tests/test_regression.py::TestAll::test_regression")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pre-build
            ;; TODO: Check how to build on other architectures.
            ;; Taken form .github/workflows/wheels.yml.
            (lambda _
              #$@(if (target-x86-64?)
                     '((setenv "BITSHUFFLE_ARCH" "haswell")
                       (setenv "CIBW_SKIP" "pp* *musllinux* cp311-macosx*")
                       (setenv "CIBW_ARCHS" "x86_64"))
                     '())
              (setenv "HDF5_DIR" #$(this-package-input "hdf5")))))))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools))
    (inputs
     (list python-numpy
           python-h5py
           hdf5))
    (home-page "https://github.com/kiyo-masui/bitshuffle")
    (synopsis "Filter for improving compression of typed binary data")
    (description "Bitshuffle is an algorithm that rearranges typed, binary data
for improving compression, as well as a python/C package that implements this
algorithm within the Numpy framework.")
    (license license:expat)))

(define-deprecated-package bitshuffle
  python-bitshuffle)

(define-public bitshuffle-for-snappy
  (package/inherit python-bitshuffle
    (name "bitshuffle-for-snappy")
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (with-output-to-file "Makefile"
                (lambda _
                  (format #t "\
libbitshuffle.so: src/bitshuffle.o src/bitshuffle_core.o src/iochain.o lz4/lz4.o
\tgcc -O3 -ffast-math -std=c99 -o $@ -shared -fPIC $^

%.o: %.c
\tgcc -O3 -ffast-math -std=c99 -fPIC -Isrc -Ilz4 -c $< -o $@

PREFIX:=~a
LIBDIR:=$(PREFIX)/lib
INCLUDEDIR:=$(PREFIX)/include

install: libbitshuffle.so
\tinstall -dm755 $(LIBDIR)
\tinstall -dm755 $(INCLUDEDIR)
\tinstall -m755 libbitshuffle.so $(LIBDIR)
\tinstall -m644 src/bitshuffle.h $(INCLUDEDIR)
\tinstall -m644 src/bitshuffle_core.h $(INCLUDEDIR)
\tinstall -m644 src/iochain.h $(INCLUDEDIR)
\tinstall -m644 lz4/lz4.h $(INCLUDEDIR)
" #$output))))))))
    (inputs '())
    (native-inputs '())))

(define-public python-uncompresspy
  (package
    (name "python-uncompresspy")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "uncompresspy" version))
       (sha256
        (base32 "1110dipshnijhq6dk5dxzxx3zpynm6dx5kcc430fw24b8xwlc9in"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ;no tests
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/kYwzor/uncompresspy")
    (synopsis "Uncompressing LZW files in Python")
    (description
     "This package implement a pure Python module for uncompressing LZW
files (.Z), such as the ones created by Unix's shell tool compress.")
    (license license:bsd-3)))

(define-public python-unix-ar
  (package
    (name "python-unix-ar")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "unix_ar" version))
       (sha256
        (base32 "0kicwxsh28x8r34a7cgzv2i65gsd4qjw2vf29pwq4fpsf3n2i4xz"))))
    (build-system pyproject-build-system)
    (arguments
     ;; These tests have timestamp-related issues.
     (list #:test-flags
           #~(list "-m" "unittest" "-k" "not test_add and not test_addfile")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? test-flags #:allow-other-keys)
                   (apply invoke "python" test-flags))))))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/getninjas/unix_ar")
    (synopsis "AR file handling in Python")
    (description "This package provides utilities to handle AR files in
Python.")
    (license license:bsd-3)))

(define-public python-zipp
  (package
    (name "python-zipp")
    (version "3.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zipp" version))
       (sha256
        (base32 "0rj182i2d7d2bz067zrk39s19j09xsxkzprl82fqql8ji9c5fwd0"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))       ;TODO: Tests requrie extra packaging
    (native-inputs
     (list ;; python-big-o
           ;; python-coherent-licensed
           ;; python-jaraco-functools ; introduces cycle
           ;; python-jaraco-itertools
           ;; python-jaraco-test ; introduces cycle
           python-more-itertools
           python-pytest
           ;; python-pytest-ignore-flaky
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/jaraco/zipp")
    (synopsis
     "Backport of pathlib-compatible object wrapper for zip files")
    (description
     "This package provides a @code{pathlib}-compatible @code{Zipfile} object
wrapper.  It provides a backport of the @code{Path} object.")
    (license license:expat)))

(define-public python-deflate
  (package
    (name "python-deflate")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "deflate" version))
       (sha256
        (base32 "0cgk118r3sglrjqirr22y7mr7fnhijhwd3v6isykjbf2m2dqyy41"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'libdeflate-prefix
                     (lambda _
                       (setenv "LIBDEFLATE_PREFIX"
                               #$(this-package-input "libdeflate")))))))
    (native-inputs (list cmake-minimal python-pytest python-scikit-build-core))
    (inputs (list libdeflate))
    (home-page "https://github.com/dcwatson/deflate")
    (synopsis "Python wrapper for @code{libdeflate}")
    (description "This package contains a very thin Python wrapper for
@code{libdeflate}.")
    (license license:expat)))

(define-public python-xopen
  (package
    (name "python-xopen")
    ;; TODO: Newer versions require zlib-ng:
    ;; <https://github.com/zlib-ng/zlib-ng>,
    ;; <https://github.com/pycompression/python-zlib-ng>.
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xopen" version))
       (sha256
        (base32 "0h08wpd5zwnlzwnbbbhahbcs69kzsfbaaigqw0viq6ri8n4zrh00"))))
    (build-system pyproject-build-system)
    ;; tests: 343 passed, 5 skipped
    (native-inputs
     (list python-pytest
           python-pytest-timeout
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list pigz python-isal))
    (home-page "https://github.com/marcelm/xopen/")
    (synopsis "Open compressed files transparently")
    (description "This module provides an @code{xopen} function that works
like Python's built-in @code{open} function, but can also deal with compressed
files.  Supported compression formats are gzip, bzip2 and, xz, and are
automatically recognized by their file extensions.  The focus is on being as
efficient as possible on all supported Python versions.")
    (license license:expat)))

(define-public python-zipstream-ng
  (package
    (name "python-zipstream-ng")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zipstream-ng" version))
       (sha256
        (base32 "1z4zdqqs2rg3z36khgj96bpggv34av337isfv7yxg32prawj687r"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-pytest-cov python-setuptools python-wheel))
    (home-page "https://github.com/pR0Ps/zipstream-ng")
    (synopsis "Streamable zip file generator")
    (description
     "This package provides a modern and easy to use streamable zip file
generator")
    (license license:lgpl3)))

(define-public python-zopfli
  (package
    (name "python-zopfli")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zopfli" version ".zip"))
       (sha256
        (base32 "1z1akqx3fjnwa75insch9p08hafikqdvqkj6mxv1k6fr81sxnj9d"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'use-system-zopfli
                     (lambda _
                       (setenv "USE_SYSTEM_ZOPFLI" "1")))
                   (add-before 'build 'set-version
                     (lambda _
                       (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         (invoke "pytest" "-vv")))))))
    (native-inputs (list unzip python-pytest python-setuptools-scm))
    (inputs (list zopfli))
    (home-page "https://github.com/fonttools/py-zopfli")
    (synopsis "Python bindings for Zopfli")
    (description "@code{pyzopfli} is a straight forward wrapper around the
@code{ZlibCompress} method of the the @code{zopfli} library.")
    (license license:asl2.0)))

(define-public python-zstandard
  (package
    (name "python-zstandard")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zstandard" version))
       (sha256
        (base32 "02dwqq5dw73zypvwpadscra8x6rwbglblh57yxl5y9g710nwdn5j"))))
    (build-system pyproject-build-system)
    ;; TODO: Unbunle zstd.
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-build-system
            (lambda _
              (substitute* "pyproject.toml"
                ((":__legacy__") ""))))
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-pytest python-setuptools))
    (propagated-inputs
     (list python-cffi))
    (home-page "https://github.com/indygreg/python-zstandard")
    (synopsis "Zstandard bindings for Python")
    (description "This project provides Python bindings for interfacing with
the Zstandard compression library.  A C extension and CFFI interface are
provided.")
    (license license:bsd-3)))

(define-public python-pyzstd
  (package
    (name "python-pyzstd")
    (version "0.15.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyzstd" version))
       (sha256
        (base32 "1iycfmif15v1jhv0gsza1hyd1hn3sz0vn9s1y79abzv8axndxzfb"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove a bundled copy of the zstd sources.
           (delete-file-recursively "zstd")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:configure-flags
      #~'(("--build-option" . "--dynamic-link-zstd"))))
    (inputs (list `(,zstd "lib")))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/Rogdham/pyzstd")
    (synopsis "Zstandard bindings for Python")
    (description "This package provides Python bindings to the Zstandard (zstd)
compression library.  The API is similar to Python's bz2/lzma/zlib module.")
    (license license:bsd-3)))
