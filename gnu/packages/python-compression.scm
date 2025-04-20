;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2019, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018-2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020, 2022, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2023, 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 TakeV <takev@disroot.org>
;;; Copyright © 2023 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages check)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages sphinx))

(define-public python-blosc
  (package
    (name "python-blosc")
    (version "1.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "blosc" version))
       (sha256
        (base32
         "0xmjs28sgpnb940zrhw010dq2m9d8a5h4fgnjyk6645fgfr1j8f2"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "blosc/c-blosc")))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'find-blosc
                 (lambda _
                   (setenv "USE_SYSTEM_BLOSC" "1")
                   (setenv "Blosc_ROOT" #$(this-package-input "c-blosc"))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "python" "-m" "blosc.test")))))))
    (propagated-inputs
     (list python-scikit-build python-numpy))
    (inputs (list c-blosc))
    (native-inputs (list cmake-minimal))
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
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'build
                          (lambda* (#:key inputs #:allow-other-keys)
                            (invoke "python" "setup.py" "build"
                                    "-DUSE_SYSTEM_BLOSC2=ON")))
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "python" "-m" "pytest" "-vv")))))))
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
           python-cython-3
           python-pytest
           python-scikit-build))
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
           python-pyannotate
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
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cramjam" version))
       (sha256
        (base32 "1b69qlr0q7q3spa7zy55xc1dr5pjgsdavxx8ijhv2j60xqjbg7sp"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules
                           ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system)
                   #:prefix py:)
                  (guix build utils))
      #:phases #~(modify-phases %standard-phases
                   ;; We use Maturin to build the project.
                   (replace 'build
                     (assoc-ref py:%standard-phases
                                'build))
                   ;; Before being able to run Python tests, we need to
                   ;; install the module and add it to PYTHONPATH.
                   (delete 'install)
                   (add-after 'build 'install
                     (assoc-ref py:%standard-phases
                                'install))
                   (add-after 'install 'add-install-to-pythonpath
                     (assoc-ref py:%standard-phases
                                'add-install-to-pythonpath))
                   ;; Finally run the tests. Only Python tests are provided.
                   (replace 'check
                     (lambda* (#:key tests? inputs outputs #:allow-other-keys)
                       (when tests?
                         ;; Without the CI variable, tests are run in "local"
                         ;; mode, which sets a deadline for hypothesis.  For a
                         ;; deterministic build, we need to set CI.
                         (setenv "CI" "1")
                         (invoke "pytest" "-vv" "tests")))))
      #:cargo-inputs `(("rust-brotli" ,rust-brotli-3)
                       ("rust-bzip2" ,rust-bzip2-0.4)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-lz4" ,rust-lz4-1)
                       ("rust-pyo3" ,rust-pyo3-0.18)
                       ("rust-snap" ,rust-snap-1)
                       ("rust-zstd" ,rust-zstd-0.11))
      #:install-source? #f))
    (native-inputs (list maturin
                         pkg-config
                         python-pytest
                         python-pytest-xdist
                         python-numpy
                         python-hypothesis
                         python-wrapper))
    (inputs (list `(,zstd "lib")))
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

(define-public python-bcj-cffi
  (package
    (name "python-bcj-cffi")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bcj-cffi" version))
       (sha256
        (base32
         "1jcczrb8zgg6w7v76w1wpz3nw75fghk3xwxkn09ll7kck7sdf68d"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cffi python-toml python-setuptools-scm))
    (native-inputs
     (list python-setuptools python-coverage python-pytest
           python-pytest-cov))
    (home-page "https://github.com/miurahr/bcj-cffi")
    (synopsis "Branch / Call /Jump CFFI library in Python")
    (description "This package provides an implementation of the Branch / Call /
Jump conversion filter by CFFI for Python.")
    (license license:lgpl2.1+)))

(define-public python-brotlicffi
  (package
    (name "python-brotlicffi")
    (version "1.0.9.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "brotlicffi" version))
              (sha256
               (base32
                "15kxgdiqcg0cm6h5xq3vkbhw7674673hcx3n2yicd3wx29l8l90c"))
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
               (setenv "USE_SHARED_BROTLI" "1"))))))
    (propagated-inputs (list python-cffi))
    (inputs (list brotli))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/python-hyper/brotlicffi")
    (synopsis "Python CFFI bindings to the Brotli library")
    (description "This package provides Python CFFI bindings to the Brotli
library.")
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
     (list python-pyannotate
           python-pytest
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "isal" version))
       (sha256
        (base32 "01914gwfrb95dagz9sqnsmvc0hssg2pb6aj204fdamss4piz8r0k"))
       ;; Remove bundled isa-l source code
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "src/isal/isa-l"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-dynamic-linking
           (lambda _ (setenv "PYTHON_ISAL_LINK_DYNAMIC" "1"))))))
    (inputs (list isa-l))
    (native-inputs (list python-cython python-setuptools python-wheel))
    (home-page "https://github.com/pycompression/python-isal")
    (synopsis "Python bindings for the ISA-L compression library")
    (description
     "This package aims to provide faster zlib and gzip compatible compression
and decompression by implementing Python bindings for the ISA-L library.")
    (license license:expat)))

(define-public python-pylsqpack
  (package
    (name "python-pylsqpack")
    (version "0.3.17")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pylsqpack" version))
              (sha256
               (base32
                "1qiwmavmxy6ba89mrdkzk52hqrd4awnp4yca395pxp2np66pf81g"))))
    ;; FIXME: Unbundle ls-qpack and xxhash!
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
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
    (build-system python-build-system)
    (propagated-inputs
     (list python-cffi))
    (native-inputs
     (list python-hypothesis
           python-setuptools-scm
           python-coverage
           python-pytest
           python-pytest-cov))
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
     (list python-coverage
           python-setuptools
           python-coveralls
           python-libarchive-c
           python-py-cpuinfo
           python-pyannotate
           python-pytest
           python-pytest-benchmark
           python-pytest-cov
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
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-lzo" version))
       (sha256
        (base32 "0315nq6r39n51n8qqamb7xv0ib0qrh76q7g3a1977172mbndijw3"))))
    (build-system python-build-system)
    (arguments
     (list
      #:test-target "check"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-setuppy
            (lambda _
              (substitute* "setup.py"
                (("include_dirs.append\\(.*\\)")
                 (string-append "include_dirs.append('"
                                #$(this-package-input "lzo")
                                "/include/lzo"
                                "')"))))))))
    (inputs
     (list lzo))
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
    (version "4.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lz4" version))
       (sha256
        (base32
         "1nmc36j5xnk7mvwwpm0nb1sddjk5iv77h877fdkkxcngm621shz1"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Remove bundled copy of lz4.
                   (delete-file-recursively "lz4libs")))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; Taken from tox.ini (excludes experimental tests).
                     (invoke "pytest" "-vv" "tests/block" "tests/frame")))))))
    (native-inputs
     (list pkg-config python-pytest python-pkgconfig python-setuptools-scm
           ;; For tests.
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
        (base32
         "18ly9pppy2yspxzw7k1b23wk77k7m44rz2g0271bqgqrk3jn3yhs"))))
    (build-system python-build-system)
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
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-snappy" version))
       (sha256
        (base32 "0amv12w0ybn6n1lk36x70a3l8bdjv4mn7iflb59wqsi00smhg8dn"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "pytest" "-vv" "-k"
                                ;; CFFI is only supported for PyPy builds.
                                (string-append "not test_snappy_cffi_enum "
                                               "and not test_snappy_all_cffi"))))))))
    (inputs (list snappy))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/andrix/python-snappy")
    (synopsis "Python bindings for the Snappy compression library")
    (description
     "@code{python-python-snappy} provides bindings to the Snappy library and
can be used to compress and decompress files and streams.  It can also be used
directly from the command line.")
    (license license:bsd-3)))

(define-public bitshuffle
  (package
    (name "bitshuffle")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bitshuffle" version))
              (sha256
               (base32
                "1823x61kyax4dc2hjmc1xraskxi1193y8lvxd03vqv029jrj8cjy"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove generated Cython files.
                  (delete-file "bitshuffle/h5.c")
                  (delete-file "bitshuffle/ext.c")
                  #t))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f             ; fail: https://github.com/h5py/h5py/issues/769
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-neon-detection
           ;; Neon is only for aarch64 ATM
           ;; see: https://github.com/kiyo-masui/bitshuffle/pull/73
           (lambda _
             (substitute* "src/bitshuffle_core.c"
               (("#define USEARMNEON")
                "#ifdef __aarch64__\n#define USEARMNEON\n#endif"))
             #t))
         (add-after 'unpack 'dont-build-native
           (lambda _
             (substitute* "setup.py"
               (("'-march=native', ") ""))
             #t)))))
    (inputs
     `(("numpy" ,python-numpy)
       ("h5py" ,python-h5py)
       ("hdf5" ,hdf5)))
    (native-inputs
     `(("cython" ,python-cython)))
    (home-page "https://github.com/kiyo-masui/bitshuffle")
    (synopsis "Filter for improving compression of typed binary data")
    (description "Bitshuffle is an algorithm that rearranges typed, binary data
for improving compression, as well as a python/C package that implements this
algorithm within the Numpy framework.")
    (license license:expat)))

(define-public bitshuffle-for-snappy
  (package/inherit bitshuffle
    (name "bitshuffle-for-snappy")
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments bitshuffle)
       ((#:tests? _ #f) #f)
       ((#:phases phases)
        `(modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
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
" (assoc-ref outputs "out"))))
               #t))))))
    (inputs '())
    (native-inputs '())))

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
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zipp" version))
       (sha256
        (base32
         "0v3qayhqv7vyzydpydwcp51bqciw8p2ajddw68x5k8zppc0vx3yk"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-more-itertools))
    (native-inputs
     (list python-setuptools-scm))
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
    ;; CMake >= 3.26 required.
    (native-inputs (list cmake-next python-pytest python-scikit-build-core))
    (inputs (list libdeflate))
    (home-page "https://github.com/dcwatson/deflate")
    (synopsis "Python wrapper for @code{libdeflate}")
    (description "This package contains a very thin Python wrapper for
@code{libdeflate}.")
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
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zstandard" version))
       (sha256
        (base32 "0qvqhs121spk7yc1l20samflxx47waxv3xm55ksxpn1djk6jzl9i"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cffi))
    (native-inputs
     (list python-hypothesis))
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
    (build-system python-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--dynamic-link-zstd")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            ;; The python-build-system's phase doesn't honour configure-flags.
            (lambda* (#:key configure-flags #:allow-other-keys)
              (apply invoke "python" "./setup.py" "build"
                     configure-flags)))
          (replace 'check
            ;; The python-build-system's phase doesn't honour configure-flags.
            (lambda* (#:key tests? test-target configure-flags
                      #:allow-other-keys)
              (when tests?
                (apply invoke "python" "./setup.py" test-target
                       configure-flags)))))))
    (inputs (list `(,zstd "lib")))
    (home-page "https://github.com/animalize/pyzstd")
    (synopsis "Zstandard bindings for Python")
    (description "This package provides Python bindings to the Zstandard (zstd)
compression library.  The API is similar to Python's bz2/lzma/zlib module.")
    (license license:bsd-3)))
