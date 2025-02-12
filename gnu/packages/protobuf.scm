;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Daniel Pimentel <d4n1@d4n1.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018, 2019, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages protobuf)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages rails)
  #:use-module (gnu packages ruby)
  #:use-module (srfi srfi-1))

(define-public fstrm
  (package
    (name "fstrm")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.farsightsecurity.com/dist/fstrm/"
                           "fstrm-" version ".tar.gz"))
       (sha256
        (base32 "13q9iz5fpp607zvk0i39158fvvjciz4y5k14rly94b9ak0gar95w"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--disable-static")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libevent))
    (home-page "https://github.com/farsightsec/fstrm")
    (synopsis "Implementation of the Frame Streams data transport protocol")
    (description
     "fstrm is an optimised implementation of Frame Streams as a C library and
several tools built on top of it.

@dfn{Frame Streams} is a light-weight, binary-clean protocol that allows for
the transport of arbitrarily-encoded data payload sequences with minimal
framing overhead---just four bytes per data frame.  It does not specify an
encoding format for these data frames and can be used with any data
serialisation format that produces byte sequences, such as Protocol Buffers,
XML, JSON, MessagePack, YAML, etc.

Frame Streams can be used either as a streaming transport over a reliable byte
stream socket (TCP sockets, TLS connections, @code{AF_UNIX} sockets, etc.) for
data in motion, or as a file format for data at rest.")
    (license (list license:expat        ; the combined work
                   license:hpnd))))     ; libmy/argv*

(define-public protobuf
  (package
    (name "protobuf")
    (version "3.21.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/protocolbuffers/"
                    "protobuf/releases/download/v"
                    (string-join (drop (string-split version #\.) 1) ".")
                    "/protobuf-cpp-" version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "third_party"))
              (sha256
               (base32
                "01cl4l0rnnzjbhjjs2gyg2pk13505gh86ikh22jqjp54dp8mvp5x"))
              (patches (search-patches "protobuf-fix-build-on-32bit.patch"))))
    (outputs (list "out"
                   "static"))           ; ~12 MiB of .a files
    (build-system cmake-build-system)
    (arguments
     (list
      ;; TODO: Add the BUILD_SHARED_LIBS flag to cmake-build-system.
      #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON"
                                "-Dprotobuf_USE_EXTERNAL_GTEST=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-broken-tests
            ;; The following tests fail on 32 bit architectures such as
            ;; i686-linux.
            (lambda _
              (let-syntax ((disable-tests
                            (syntax-rules ()
                              ((_ file test ...)
                               (substitute* file
                                 ((test name)
                                  (string-append "DISABLED_" name)) ...)))))
                ;; See: https://github.com/protocolbuffers/protobuf/issues/8460.
                (disable-tests "src/google/protobuf/any_test.cc"
                               "TestPackFromSerializationExceedsSizeLimit")
                ;; See: https://github.com/protocolbuffers/protobuf/issues/8459.
                (disable-tests "src/google/protobuf/arena_unittest.cc"
                               "SpaceAllocated_and_Used"
                               "BlockSizeSmallerThanAllocation")
                ;; See: https://github.com/protocolbuffers/protobuf/issues/8082.
                (disable-tests "src/google/protobuf/io/zero_copy_stream_unittest.cc"
                               "LargeOutput"))))
          (add-before 'configure 'set-c++-standard
            (lambda _
              (substitute* "CMakeLists.txt"
                ;; The 32bit patch requires C++14.
                ;; TODO: Remove after next release.
                (("CMAKE_CXX_STANDARD 11") "CMAKE_CXX_STANDARD 14"))))
          (add-after 'install 'move-static-libraries
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Move static libraries to the "static" output.
              (let* ((out    (assoc-ref outputs "out"))
                     (lib    (string-append out "/lib"))
                     (static (assoc-ref outputs "static"))
                     (slib   (string-append static "/lib")))
                (mkdir-p slib)
                (for-each (lambda (file)
                            (install-file file slib)
                            (delete-file file))
                          (find-files lib "\\.a$"))))))))
    (native-inputs (append (if (%current-target-system)
                               (list this-package)
                               '())
                           (list googletest)))
    (inputs (list zlib googletest))
    (home-page "https://github.com/protocolbuffers/protobuf")
    (synopsis "Data encoding for remote procedure calls (RPCs)")
    (description
     "Protocol Buffers are a way of encoding structured data in an efficient
yet extensible format.  Google uses Protocol Buffers for almost all of its
internal RPC protocols and file formats.")
    (license license:bsd-3)))

;; Needed for python-mysql-connector-python
(define-public protobuf-3.20
  (package
    (inherit protobuf)
    (name "protobuf")
    (version "3.20.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/protocolbuffers/"
                    "protobuf/releases/download/v" version
                    "/protobuf-cpp-" version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "third_party"))
              (sha256
               (base32
                "0ggyfrfal7wms4n8pba224jwpjxn19rigd5y90m3x2bg97ych775"))))
    (build-system gnu-build-system)
    (arguments (substitute-keyword-arguments (package-arguments protobuf)
                 ;; XXX: insists on using bundled googletest
                 ((#:tests? _ #f) #false)
                 ((#:configure-flags _ #f)
                  #~(list))
                 ((#:phases phases)
                  #~(modify-phases #$phases
                      (delete 'set-c++-standard)))))))

;; Tensorflow requires version 3.6 specifically.
(define-public protobuf-3.6
  (package
    (inherit protobuf)
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-cpp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0a955bz59ihrb5wg7dwi12xajdi5pmz4bl0g147rbdwv393jwwxk"))))
    (build-system gnu-build-system)
    (arguments (substitute-keyword-arguments (package-arguments protobuf)
                 ((#:configure-flags _ #f)
                  #~(list))
                 ((#:phases phases)
                  #~(modify-phases #$phases
                      (delete 'set-c++-standard)))))))

;; The 3.5 series are the last versions that do not require C++ 11.
(define-public protobuf-3.5
  (package
    (inherit protobuf-3.6)
   (version "3.5.1")
   (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-cpp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14j0427ykjzrd9a66c2mpk0sjcccjlsx6q8ww6hzwb6sha3vm3f2"))))))

;; XXX Remove this old version when no other packages depend on it.
(define-public protobuf-2
  (package (inherit protobuf)
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "040rcs9fpv4bslhiy43v7dcrzakz4vwwpyqg4jp8bn24sl95ci7f"))))
    (build-system gnu-build-system)
    (arguments '())
    (outputs '("out"))))

(define-public protobuf-c
  (package
    (name "protobuf-c")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/protobuf-c/protobuf-c/"
                                  "releases/download/v" version
                                  "/protobuf-c-" version ".tar.gz"))
              (sha256
               (base32
                "17rk42r3gcc46c2svd1mxs542wnl4mi77a6klkhg6wl1a36zmi2c"))))
    (build-system gnu-build-system)
    (inputs (list protobuf))
    (native-inputs (append (if (%current-target-system)
                               (list protobuf)
                               '())
                           (list pkg-config)))
    (arguments (if (%current-target-system)
                   (list #:configure-flags
                         #~(list
                            (string-append
                             "PROTOC="
                             (search-input-file %build-inputs "bin/protoc"))))
                   (list)))
    (home-page "https://github.com/protobuf-c/protobuf-c")
    (synopsis "Protocol Buffers implementation in C")
    (description
     "This is protobuf-c, a C implementation of the Google Protocol Buffers
data serialization format.  It includes @code{libprotobuf-c}, a pure C library
that implements protobuf encoding and decoding, and @code{protoc-c}, a code
generator that converts Protocol Buffer @code{.proto} files to C descriptor
code.")
    (license license:bsd-2)))

(define-public protobuf-c-for-aiscm
  (package
    (inherit protobuf-c)
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/protobuf-c/protobuf-c/"
                                  "releases/download/v" version
                                  "/protobuf-c-" version ".tar.gz"))
              (sha256
               (base32
                "0y3yaanq97si7iyld06p8w20m0shpj7sf4xwzbhhvijhxw36d592"))))
    (inputs (list protobuf-3.6))))

(define-public protozero
  (package
    (name "protozero")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mapbox/protozero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "052cq5mdjjgcsgk612zkqi8k08p3ikl22r59dk6i6fq41dxldja7"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/mapbox/protozero")
    (synopsis "Minimalistic protocol buffer decoder and encoder in C++")
    (description "Protozero is a minimalistic protocol buffer decoder and
encoder in C++.  The developer using protozero has to manually translate the
@file{.proto} description into code.")
    (license (list
              license:asl2.0            ; for folly
              license:bsd-2))))

(define-public nanopb
  (package
    (name "nanopb")
    (version "0.4.6.4")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/nanopb/nanopb")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gb6q4igrjj8jap4p1ijza4y8dkjlingzym3cli1w18f90d7xlh7"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON"
                                "-DBUILD_STATIC_LIBS=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "../source/tests"
                  (invoke "scons"))))))))
    (native-inputs (list protobuf python-protobuf python-wrapper scons))
    (home-page "https://jpa.kapsi.fi/nanopb/")
    (synopsis "Small code-size Protocol Buffers implementation in ANSI C")
    (description "Nanopb is a small code-size Protocol Buffers implementation
in ansi C.  It is especially suitable for use in microcontrollers, but fits
any memory-restricted system.")
    (license license:zlib)))

(define-public python-mypy-protobuf
  (package
    (name "python-mypy-protobuf")
    (version "3.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nipunn1313/mypy-protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z03h9k68qvnlyhpk0ndwp01bdx77vrjr6mybxq4ldilkkbksklk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'generate-protos-for-tests
            (lambda _
              ;; Generate Python sources.
              (for-each (lambda (proto)
                          (invoke "protoc"
                                  "--proto_path=proto"
                                  "--experimental_allow_proto3_optional"
                                  "--python_out=test/generated" proto))
                        (find-files "." "\\.proto$"))
              ;; Generate GRPC protos.
              (for-each (lambda (proto)
                          (invoke "python" "-m" "grpc_tools.protoc"
                                  "--proto_path=proto"
                                  "--experimental_allow_proto3_optional"
                                  "--grpc_python_out=test/generated" proto))
                        (find-files "proto/testproto/grpc" "\\.proto$"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (setenv "PYTHONPATH" "test/generated")
              (invoke "pytest" "-vv" "--ignore=test/generated" "test"))))))
    (native-inputs
     (list python-grpc-stubs
           python-grpcio-tools
           python-pytest
           python-typing-extensions
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list protobuf
           python-protobuf
           python-types-protobuf))
    (home-page "https://github.com/nipunn1313/mypy-protobuf")
    (synopsis "Generate Mypy stub files from protobuf specifications")
    (description "This Python package provide tools to generate Mypy stubs
from protobuf specification files.")
    (license license:asl2.0)))

(define-public python-nanopb
  (package
    (inherit nanopb)
    (name "python-nanopb")
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ;no Python-specific tests
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda _
              (copy-file "extra/poetry/pyproject.toml" "pyproject.toml")
              (delete-file "build.py")
              ;; Mimick extra/poetry/poetry_build.sh.
              (mkdir "nanopb")
              (copy-recursively "generator" "nanopb/generator")
              (invoke "touch" "nanopb/__init__.py"
                      "nanopb/generator/__init__.py")
              (invoke "make" "-C" "nanopb/generator/proto")
              (invoke "python" "-m" "build" "--wheel" "--no-isolation" ".")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((whl (car (find-files "dist" "\\.whl$"))))
                (invoke "pip" "--no-cache-dir" "--no-input"
                        "install" "--no-deps" "--prefix" #$output whl)))))))
    (native-inputs (list poetry protobuf python-pypa-build))
    (propagated-inputs (list python-protobuf))
    (synopsis "Small code-size Protocol Buffers implementation in Python")))

(define-public python-protobuf-5
  (package
    (name "python-protobuf")
    (version "5.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "protobuf" version))
       (sha256
        (base32
         "0yzg3i40p7rbr51xr72avpvn72yin5xcxybkyc0y99c0j72dpfk4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ; no tests provided for Python variant
    (native-inputs
     (list python-setuptools
           python-wheel))
    (inputs (list protobuf))
    (home-page "https://github.com/protocolbuffers/protobuf")
    (synopsis "Protocol buffers is a data interchange format")
    (description
     "Protocol buffers are a language-neutral, platform-neutral extensible
mechanism for serializing structured data.")
    (license license:bsd-3)))

(define-public python-protobuf
  (package
    (name "python-protobuf")
    (version "3.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "protobuf" version))
       (sha256
        (base32
         "0l0p2lczs5iahgkhzm3298pjl49bk9iiwngkglg7ll7fkqqwlbbi"))))
    (build-system python-build-system)
    (inputs (list protobuf))
    (arguments
     `(;; Favor C++ implementation from protobuf over the native Python
       ;; implementation. The additional dependency yields significant
       ;; performance improvements for some workloads.
       #:configure-flags '("--cpp_implementation")))
    (home-page "https://github.com/google/protobuf")
    (synopsis "Protocol buffers is a data interchange format")
    (description
     "Protocol buffers are a language-neutral, platform-neutral extensible
mechanism for serializing structured data.")
    (license license:bsd-3)))

(define-public python-pure-protobuf
  (package
    (name "python-pure-protobuf")
    (version "2.0.1")
    (source
     (origin
       ;; The PyPI tarball is broken: it has no tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eigenein/protobuf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15dp5pvazd0jx4wzzh79080ah7hkpd3axh40al9vhzs2hf3v90hx"))))
    (build-system python-build-system)
    (native-inputs
     (list python-flake8 python-pytest python-pytest-cov python-isort))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "--cov-report" "term-missing" "--cov"
                     "pure_protobuf")
             (invoke "flake8" "pure_protobuf" "tests"
                     "--ignore=F541")
             (invoke "isort" "-rc" "-c" "pure_protobuf" "tests"))))))
    (home-page "https://pypi.org/project/pure-protobuf/")
    (synopsis "Protobuf implementation using dataclasses")
    (description
     "@code{python-pure-protobuf} takes advantage of the standard
dataclasses module to define message types.  Protocol buffers are a
language-neutral, platform-neutral extensible mechanism for serializing
structured data.")
    (license license:expat)))

;; For tensorflow.
(define-public python-protobuf-3.6
  (package
    (inherit python-protobuf)
    (name "python-protobuf")
    (version (package-version protobuf-3.6) )
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "protobuf" version))
       (sha256
        (base32
         "04bqb12smlckzmgkj6vgmpbr3cby0n6726cmz33bqr7kn1vb728l"))))
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'compatibility
           (lambda _
             (substitute* '("google/protobuf/internal/containers.py"
                            "google/protobuf/internal/well_known_types.py")
               (("collections.Mutable")
                "collections.abc.Mutable")))))))
    (inputs (list python-six))
    (native-inputs
     (list python-setuptools-for-tensorflow))))

(define-public python-proto-plus
  (package
    (name "python-proto-plus")
    (version "1.20.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "proto-plus" version))
       (sha256
        (base32 "1raad9qnmfva94nm33k40bcwrckgljbfky5pdwh4xhg6r5dj52zj"))))
    (build-system python-build-system)
    (propagated-inputs (list python-protobuf))
    (home-page "https://github.com/googleapis/proto-plus-python.git")
    (synopsis "Pythonic protocol buffers")
    (description "This is a wrapper around protocol buffers.  Protocol buffers
is a specification format for APIs, such as those inside Google.  This library
provides protocol buffer message classes and objects that largely behave like
native Python types.")
    (license license:asl2.0)))

(define-public emacs-protobuf-mode
  (package
    (name "emacs-protobuf-mode")
    (version (package-version protobuf))
    (source (package-source protobuf))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'expand-load-path 'change-working-directory
           (lambda _ (chdir "editors") #t)))))
    (home-page "https://github.com/protocolbuffers/protobuf")
    (synopsis "Protocol buffers major mode for Emacs")
    (description
     "This package provides an Emacs major mode for editing Protocol Buffer
source files.")
    (license license:bsd-3)))

(define-public ruby-protobuf
  (package
    (name "ruby-protobuf")
    (version "3.10.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ruby-protobuf/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12hp1clg83jfl35x1h2ymzpj5w83wrnqw7hjfc6mqa8lsvpw535r"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-use-bundler-for-tests
            (lambda _
              (substitute* "spec/spec_helper.rb"
                (("Bundler\\.setup.*") ""))))
          (add-after 'unpack 'relax-version-requirements
            (lambda _
              (substitute* ((@@ (guix build ruby-build-system) first-gemspec))
                (("'rake',.*")
                 "'rake'\n")
                (("\"rubocop\",.*")
                 "'rubocop'\n")
                (("\"parser\",.*")
                 "'parser'\n"))))
          (add-after 'unpack 'patch-protoc
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "lib/protobuf/tasks/compile.rake"
                (("\"protoc\"")
                 (string-append "\"" (search-input-file inputs "bin/protoc")
                                "\"")))))
          (add-after 'unpack 'skip-failing-test
            ;; See: https://github.com/ruby-protobuf/protobuf/issues/419
            (lambda _
              (substitute* "spec/lib/protobuf/rpc/connectors/ping_spec.rb"
                (("expect\\(::IO\\)\\.to receive\\(:select\\).*" all)
                 (string-append "        pending\n" all)))))
          (add-after 'replace-git-ls-files 'replace-more-git-ls-files
            (lambda _
              (substitute* ((@@ (guix build ruby-build-system) first-gemspec))
                (("`git ls-files -- \\{test,spec,features\\}/*`")
                 "`find test spec features -type f | sort`")
                (("`git ls-files -- bin/*`")
                 "`find bin -type f | sort`"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "rspec")))))))
    (native-inputs
     (list ruby-benchmark-ips
           ruby-ffi-rzmq
           ruby-parser
           ruby-pry-stack-explorer
           ruby-rake
           ruby-rspec
           ruby-rubocop
           ruby-ruby-prof
           ruby-simplecov
           ruby-timecop
           ruby-varint
           ruby-yard))
    (inputs
     (list protobuf))
    (propagated-inputs
     (list ruby-activesupport
           ruby-middleware
           ruby-thor
           ruby-thread-safe))
    (home-page "https://github.com/ruby-protobuf/protobuf")
    (synopsis "Implementation of Google's Protocol Buffers in Ruby")
    (description "Protobuf is an implementation of Google's Protocol Buffers
in pure Ruby.")
    (license license:expat)))
