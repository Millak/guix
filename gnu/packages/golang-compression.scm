;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
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

(define-module (gnu packages golang-compression)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages pkg-config))

;;; Commentary:
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public go-github-com-andybalholm-brotli
  (package
    (name "go-github-com-andybalholm-brotli")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andybalholm/brotli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zdvcwfzxnkljyh4p7izy0bfxrwidwwmp1p5h1fydyrgbs4xacly"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/andybalholm/brotli"))
    (home-page "https://github.com/andybalholm/brotli")
    (synopsis "Pure Go Brotli encoder and decoder")
    (description
     "This package is a brotli compressor and decompressor implemented in Go.
It was translated from the reference implementation
(@url{https://github.com/google/brotli,https://github.com/google/brotli}) with
the @code{c2go} tool at
@url{https://github.com/andybalholm/c2go,https://github.com/andybalholm/c2go}.")
    (license license:expat)))

(define-public go-github-com-datadog-zstd
  (package
    (name "go-github-com-datadog-zstd")
    (version "1.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DataDog/zstd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hri68jd5yh9kxy4bj2b4rfi7jz74zl20d4hk7rwcwykpgk90qid"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; These flags need to be applied in the final application as well to
      ;; build with system's libzstd,
      #:build-flags #~(list "-tags" "external_libzstd")
      #:test-flags #~(list "-tags" "external_libzstd")
      #:import-path "github.com/DataDog/zstd"))
    (native-inputs
     (list pkg-config))
    (inputs
     (list (list zstd "lib")))
    (home-page "https://github.com/DataDog/zstd")
    (synopsis "Zstd Golang wrapper")
    (description
     "This package provides a Go wrapper to
@url{https://github.com/facebook/zstd, zstd} C library.")
    (license license:bsd-3)))

(define-public go-github-com-dsnet-compress
  (package
    (name "go-github-com-dsnet-compress")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dsnet/compress")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wwjaymzb1xxq3ybch3nwn72xhi2s40cvz0cl986yad3w1xwzj91"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dsnet/compress"
      #:test-subdirs
      #~(list "brotli/..." "bzip2/..." "flate" "internal" "internal/prefix"
              "internal/testutil" "xflate/...")
      #:phases
      #~(modify-phases %standard-phases
          ;; Testdata directories contains some compressed files requiring
          ;; for running tests but not required on run time.
          (add-after 'check 'remove-testdata
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/bzip2/testdata"))
              (delete-file-recursively
               (string-append "src/" import-path "/brotli/testdata"))
              (delete-file-recursively
               (string-append "src/" import-path "/testdata")))))))
    (propagated-inputs
     (list go-github-com-dsnet-golib
           go-github-com-klauspost-compress
           go-github-com-ulikunitz-xz))
    (home-page "https://github.com/dsnet/compress")
    (synopsis "Collection of compression libraries for Golang")
    (description
     "Package compress is a collection of compression libraries implementing
Golang modules:
@table @code
@item brotli
Implements the Brotli format, described in RFC 7932.
@item bzip2
Implements the BZip2 compressed data format.
@item flate
Implements the DEFLATE format, described in RFC 1951.
@item xflate
Implements the XFLATE format, an random-access extension to DEFLATE.
@end table")
    (license license:bsd-3)))

(define-public go-github-com-golang-snappy
  (package
    (name "go-github-com-golang-snappy")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/snappy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "004cw699yz3pdpawhjhpa0y94c4w479nw1rf39zj6h6027kpwv2j"))
       (patches (search-patches "go-github-com-golang-snappy-32bit-test.patch"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/golang/snappy"))
    (home-page "https://github.com/golang/snappy")
    (synopsis "Snappy compression format in the Go programming language")
    (description "This package provides a Go implementation of the Snappy
compression format.")
    (license license:bsd-3)))

(define-public go-github-com-hhrutter-lzw
  (package
    (name "go-github-com-hhrutter-lzw")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hhrutter/lzw")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n13qhf8ih08jzm10wprdvjy56ylmy6fhakyqrddm6nszf397wch"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hhrutter/lzw"))
    (home-page "https://github.com/hhrutter/lzw")
    (synopsis "Extended version of @code{compress/lzw}")
    (description
     "This package provides an enhanced version of the @code{compress/lzw}
library included in the stdlib, and supports GIF, TIFF and PDF.")
    (license license:bsd-3)))

(define-public go-github-com-klauspost-compress
  (package
    (name "go-github-com-klauspost-compress")
    (version "1.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/klauspost/compress")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vpk98rkfc67pniqj7xvxm2b275xwzav8rnca06023py769rlkyy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags #~(list "-short" "-tags=nounsafe")
      #:import-path "github.com/klauspost/compress"))
    (home-page "https://github.com/klauspost/compress")
    (synopsis "Go compression library")
    (description "@code{compress} provides various compression algorithms.")
    (license license:bsd-3)))

(define-public go-github-com-klauspost-pgzip
  (package
    (name "go-github-com-klauspost-pgzip")
    (version "1.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/klauspost/pgzip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j29wr6nd9ncdbkjphyziv0h8p5s2mj222cgcfqxmzjnfn7623d8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/klauspost/pgzip"))
    (propagated-inputs (list go-github-com-klauspost-compress))
    (home-page "https://github.com/klauspost/pgzip")
    (synopsis "Parallel (de)compression of gzip files in Go")
    (description
     "This package implements parallel gzip compression and decompression in
Golang and is fully compatible with @code{compress/gzip} from the standard
library.  This is beneficial for large amounts of data, say more than 1MB at a
time, as otherwise the internal gzip library will likely be faster.")
    (license (list license:bsd-3 license:expat))))

(define-public go-github-com-mholt-archiver-v3
  (package
    (name "go-github-com-mholt-archiver-v3")
    (version "3.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mholt/archiver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1py186hfy4p69wghqmbsyi1r3xvw1nyl55pz8f97a5qhmwxb3mwp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mholt/archiver/v3"))
    (propagated-inputs
     (list go-github-com-andybalholm-brotli
           go-github-com-dsnet-compress
           go-github-com-golang-snappy
           go-github-com-klauspost-compress
           go-github-com-klauspost-pgzip
           go-github-com-nwaples-rardecode
           go-github-com-pierrec-lz4-v4
           go-github-com-ulikunitz-xz
           go-github-com-xi2-xz))
    (home-page "https://github.com/mholt/archiver")
    (synopsis "Multi format archiver Golang library and CLI command")
    (description
     "Package archiver facilitates convenient, cross-platform, high-level
archival and compression operations for a variety of formats and compression
algorithms.

Features:
@itemize
@item stream-oriented APIs
@item automatically identify archive and compression formats
@item traverse directories, archive files, and any other file uniformly as
@code{io/fs} file systems
@item compress and decompress files
@item create and extract archive files
@item walk or traverse into archive files
@item extract only specific files from archives
@item insert (append) into .tar and .zip archives
@item read from password-protected 7-Zip files
@item numerous archive and compression formats supported
@item extensible (add more formats just by registering them)
@item cross-platform, static binary
@item pure Golang (no cgo)
@item multithreaded Gzip
@item adjust compression levels
@item automatically add compressed files to zip archives without
re-compressing
@item open password-protected rar archives
@end itemize

Supported compression formats:
@itemize
@item brotli (.br)
@item bzip2 (.bz2)
@item flate (.zip)
@item gzip (.gz)
@item lz4 (.lz4)
@item lzip (.lz)
@item snappy (.sz)
@item xz (.xz)
@item zlib (.zz)
@item zstandard (.zst)
@end itemize

Supported archive formats:
@itemize
@item .zip
@item .tar (including any compressed variants like .tar.gz)
@item .rar (read-only)
@item .7z (read-only)
@end itemize")
    (license license:expat)))

(define-public go-github-com-nwaples-rardecode
  (package
    (name "go-github-com-nwaples-rardecode")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwaples/rardecode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s00b8a9gppka3yxkxh7z5wy0ahygl8wbb0fbyx2r0rj879a1c2z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nwaples/rardecode"))
    (home-page "https://github.com/nwaples/rardecode")
    (synopsis "Reading RAR archives in Go")
    (description
     "This package provides a library for reading RAR archives with Golang.")
    (license license:bsd-2)))

(define-public go-github-com-nwaples-rardecode-v2
  (package
    (inherit  go-github-com-nwaples-rardecode)
    (name "go-github-com-nwaples-rardecode-v2")
    (version "2.0.0-beta.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwaples/rardecode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1344mxfdgs5fps6mqxk6352arrfszi33kmq394rgmqpf4394f1y7"))))
    (arguments
     (list
      #:import-path "github.com/nwaples/rardecode/v2"))))

(define-public go-github-com-pierrec-lz4
  (package
    (name "go-github-com-pierrec-lz4")
    (version "2.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pierrec/lz4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vfn01gd3hcpbj6gb4ig3pw6bv0g4j5780awr0fv4kf9id8gjvyy"))
       (snippet
        ;; XXX: fiano uses this package as library only, cmd requires very
        ;; additional not packed and dated inputs. Overwrite with
        ;; go-github-com-pierrec-lz4-v4 when fiano is updated.
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "cmd")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pierrec/lz4"))
    (native-inputs
     (list go-github-com-frankban-quicktest))
    (home-page "https://github.com/pierrec/lz4")
    (synopsis "LZ4 compression in pure Go")
    (description
     "@code{lz4} provides a streaming interface to
@url{http://fastcompression.blogspot.fr/2013/04/lz4-streaming-format-final.html,
LZ4 data streams} as well as low level compress and uncompress functions for
LZ4 data blocks.  The implementation is based on the reference C
@url{https://github.com/lz4/lz4, one}.")
    (license license:bsd-3)))

(define-public go-github-com-pierrec-lz4-v4
  (package
    (inherit go-github-com-pierrec-lz4)
    (name "go-github-com-pierrec-lz4-v4")
    (version "4.1.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pierrec/lz4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nc2aprbw4s6cx2mijaqdswkgnizx8fqb0mzha82wrznl3gz69ni"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pierrec/lz4/v4"))
    ;; For CLI.
    (native-inputs
     (list go-code-cloudfoundry-org-bytefmt
           go-github-com-pierrec-cmdflag
           go-github-com-schollz-progressbar-v3))))

(define-public go-github-com-saracen-fastzip
  (package
    (name "go-github-com-saracen-fastzip")
    (version "0.1.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/saracen/fastzip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h63lhbwkga920n6lrh1ccfps2k4c3dn2pqap0i6mvjk6dba95s0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/saracen/fastzip"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-klauspost-compress
           go-github-com-saracen-zipextra
           go-golang-org-x-sync
           go-golang-org-x-sys))
    (home-page "https://github.com/saracen/fastzip")
    (synopsis "Zip archiver and extractor with a focus on speed")
    (description
     "Fastzip is an opinionated Zip archiver and extractor with a focus on
speed.
Features:
@itemize
@item archiving and extraction of files and directories can only occur within
a specified directory
@item permissions, ownership (uid, gid on linux/unix) and modification times
are preserved
@item buffers used for copying files are recycled to reduce allocations
@item files are archived and extracted concurrently
@item by default, @code{github.com/klauspost/compress/flate} library is used
for compression and decompression
@end itemize")
    (license license:expat)))

(define-public go-github-com-saracen-zipextra
  (package
    (name "go-github-com-saracen-zipextra")
    (version "0.0.0-20220303013732-0187cb0159ea")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/saracen/zipextra")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j24jdi5495nfq08xm6yjr9s32z13x6y961ry1ihhhgi6s8zdddj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/saracen/zipextra"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file "zipextra_example_test.go")))))))
    (home-page "https://github.com/saracen/zipextra")
    (synopsis "Encoding and decoding ZIP archive format's \"Extra Fields\"")
    (description
     "This package provides a library for encoding and decoding ZIP archive
format's \"Extra Fields\".  The intention is to eventually support and provide
a low-level API for the majority of PKWARE's and Info-ZIP's extra fields.")
    (license license:expat)))

(define-public go-github-com-ulikunitz-xz
  (package
    (name "go-github-com-ulikunitz-xz")
    (version "0.5.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ulikunitz/xz")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09n4zawzycab4mmk20sv0490xrx9ighv25g5hj578vsjgzz842n1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ulikunitz/xz"))
    (home-page "https://github.com/ulikunitz/xz")
    (synopsis "Read and write xz compressed streams in Go")
    (description
     "This package provides a support of reading and writing of xz
compressed streams.  It includes also a gxz command for compressing and
decompressing data.  The package is completely written in Go and doesn't have
any dependency on any C code.")
    (license license:bsd-3)))

(define-public go-github-com-xi2-xz
  (package
    (name "go-github-com-xi2-xz")
    (version "0.0.0-20171230120015-48954b6210f8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xi2/xz")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "178r0fa2dpzxf0sabs7dn0c8fa7vs87zlxk6spkn374ls9pir7nq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xi2/xz"))
    (home-page "https://github.com/xi2/xz")
    (synopsis "Native Golang XZ decompression package")
    (description
     "This package implements a native XZ decompression in Golang.")
    ;; This package is a modified version of XZ Embedded
    ;; <http://tukaani.org/xz/embedded.html>: 0BSD
    ;;
    ;; The contents of the testdata directory are modified versions of the
    ;; test files from XZ Utils <http://tukaani.org/xz/>: 0BSD
    (license license:public-domain)))

;;;
;;; Executables:
;;;

(define-public go-arc
  (package
    (inherit go-github-com-mholt-archiver-v3)
    (name "go-arc")
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/mholt/archiver/cmd/arc"
      #:unpack-path "github.com/mholt/archiver"))
    (description
     (string-append (package-description go-github-com-mholt-archiver-v3)
                    "\nThis package provides an command line interface (CLI)
tool."))))

(define-public go-lz4c
  (package
    (inherit go-github-com-pierrec-lz4-v4)
    (name "go-lz4c")
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/pierrec/lz4/cmd/lz4c"
      #:unpack-path "github.com/pierrec/lz4"))
    (description
     (string-append (package-description go-github-com-pierrec-lz4-v4)
                    "  This package provides an additional command line
interface tool to compress and decompress LZ4 files."))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
