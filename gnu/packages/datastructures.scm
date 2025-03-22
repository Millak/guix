;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2018, 2019, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019, 2020, 2022, 2024, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages datastructures)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson))

(define-public coucal
  (let ((commit "73ada075553b7607d083037a87cb9c73b3683bfc")
        (revision "1"))
    (package
      (name "coucal")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xroche/coucal")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01996vda3wj5ywpwg9yhysaq6cyi44xnkyhihbwwi43hrj1ic2vm"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'do-not-run-tests-early
              (lambda _
                (substitute* "Makefile"
                  (("(all: ).*" _ lead) (string-append lead "gcc")))))
            (add-after 'unpack 'remove-Werror
              ;; Prevent "this statement may fall through
              ;; [-Wimplicit-fallthrough=]" errors from "murmurhash3.h" file.
              (lambda _
                (substitute* "Makefile"
                  (("-Werror ") ""))))
            (delete 'configure)         ;no configure script
            (replace 'install           ;no install target
              (lambda _
                (let ((doc (string-append #$output
                                          "/share/doc/" #$name "-" #$version)))
                  (install-file "README.md" doc))
                (for-each (lambda (f) (install-file f #$output))
                          (find-files "." "(coucal|murmurhash)"))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "make" "tests" "runtests")))))))
      (home-page "https://github.com/xroche/coucal")
      (synopsis "Cuckoo-hashing-based hashtable with stash area C library")
      (description "Coucal is an implementation of the Cuckoo hashing
algorithm with a stash area using by default the MurmurHash hash function.")
      ;; Library is released under Expat terms, but the source includes
      ;; "murmurhash3.h", which is placed in the public domain.
      (license (list license:expat license:public-domain)))))

(define-public gdsl
  (package
    (name "gdsl")
    (version "1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://example.org") ;only hosted on Software Heritage
                    (commit "6adb53be8b8f9f2e4bbfc92d357eedeefb4c7430")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a52g12d9sf9hhcyvwfd7xdazj2a9i9jh97cnlqf2ymvwnvjk1g0"))))
    (build-system gnu-build-system)
    (home-page "https://web.archive.org/web/20170502005430/http://home.gna.org/gdsl/")
    (synopsis "Generic data structures library")
    (description "The Generic Data Structures Library (GDSL) is a collection
of routines for generic data structures manipulation.  It is a re-entrant
library fully written from scratch in pure ANSI C.  It is designed to offer
for C programmers common data structures with powerful algorithms, and hidden
implementation.  Available structures are lists, queues, stacks, hash tables,
binary trees, binary search trees, red-black trees, 2D arrays, permutations
and heaps.")
    (license license:gpl2+)))

(define-public marisa
  (package
    (name "marisa")
    (version "0.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/s-yata/marisa-trie")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "marisa-fix-MARISA_WORD_SIZE.patch"))
       (sha256
        (base32 "1hy8hfksizk1af6kg8z3b9waiz6d5ggd73fiqcvmhfgra36dscyq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://github.com/s-yata/marisa-trie")
    (synopsis "Trie data structure C++ library")
    (description "@acronym{MARISA, Matching Algorithm with Recursively
Implemented StorAge} is a static and space-efficient trie data structure C++
library.")

    ;; Dual-licensed, according to docs/readme.en.html (source files lack
    ;; copyright/license headers.)
    (license (list license:bsd-2 license:lgpl2.1+))))

(define-public sparsehash
  (package
    (name "sparsehash")
    (version "2.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sparsehash/sparsehash")
                     (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pf1cjvcjdmb9cd6gcazz64x0cd2ndpwh6ql2hqpypjv725xwxy7"))))
    (build-system gnu-build-system)
    (synopsis "Memory-efficient hashtable implementations")
    (description
     "This library contains several hash-map implementations, similar in API
to SGI's @code{hash_map} class, but with different performance
characteristics.  @code{sparse_hash_map} uses very little space overhead, 1-2
bits per entry.  @code{dense_hash_map} is very fast, particularly on lookup.
@code{sparse_hash_set} and @code{dense_hash_set} are the set versions of these
routines.  All these implementation use a hashtable with internal quadratic
probing.  This method is space-efficient -- there is no pointer overhead --
and time-efficient for good hash functions.")
    (home-page "https://github.com/sparsehash/sparsehash")
    (license license:bsd-3)))

(define-public ssdeep
  (package
    (name "ssdeep")
    (version "2.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ssdeep-project/ssdeep/"
                           "releases/download/release-" version "/"
                           "ssdeep-" version ".tar.gz"))
       (sha256
        (base32 "04qkjc6kksxkv7xbnk32rwmf3a8czdv2vvrdzfs0kw06h73snbpz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (home-page "https://ssdeep-project.github.io")
    (synopsis "Context-triggered piecewise hashing algorithm")
    (description "ssdeep computes and matches context triggered piecewise
hashes (CTPH), also called fuzzy checksums.  It can identify similar files
that have sequences of identical bytes in the same order, even though bytes
in between these sequences may be different in both content and length.")
    (license license:gpl2+)))

(define-public libcuckoo
  (package
    (name "libcuckoo")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/efficient/libcuckoo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h9yhpkhk813dk66y6bs2csybw3pbpfnp3cakr2xism02vjwy19l"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~'("-DBUILD_TESTS=1")))
    (home-page "https://efficient.github.io/libcuckoo/")
    (synopsis "Concurrent hash table")
    (description
     "@code{libcuckoo} provides a high-performance, compact hash table that
allows multiple concurrent reader and writer threads.")
    (license license:asl2.0)))

(define-public liburcu
  (package
    (name "liburcu")
    (version "0.14.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.lttng.org/files/urcu/"
                                  "userspace-rcu-" version ".tar.bz2"))
              (sha256
               (base32
                "1h5bg0k94by2v7cjq7fb3ridqixbd9pndw506vl27h3fvh9wn6i3"))))
    (build-system gnu-build-system)
    (native-inputs
     ;; riscv64 needs >= gcc-13.3.0
     (append
       (if (target-riscv64?)
           (list gcc-14)
           '())
       (list perl)))            ; for tests
    (home-page "https://liburcu.org/")
    (synopsis "User-space RCU data synchronisation library")
    (description "liburcu is a user-space @dfn{Read-Copy-Update} (RCU) data
synchronisation library.  It provides read-side access that scales linearly
with the number of cores.  liburcu-cds provides efficient data structures
based on RCU and lock-free algorithms.  These structures include hash tables,
queues, stacks, and doubly-linked lists.")
    (license (list license:lgpl2.1 license:expat))))

(define-public uthash
  (package
    (name "uthash")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/troydhanson/uthash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k80bjbb6ss5wpmfmfji6xbyjm990hg9kcshwwnhdnh73vxkcd1m"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl))
    (arguments
     `(#:make-flags
       (list "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; nothing to configure
         (delete 'build)                ; nothing to build
         (replace 'check
           (lambda* (#:key make-flags #:allow-other-keys)
             (with-directory-excursion "tests"
               (apply invoke "make" make-flags))))
         (replace 'install
           ;; There is no top-level Makefile to do this for us.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (include (string-append out "/include")))
               ;; Don't install HTML files: they're just the below .txt files
               ;; dolled up, can be stale, and regeneration requires asciidoc.
               (for-each (λ (file) (install-file file doc))
                         (find-files "doc" "\\.txt$"))
               (for-each (λ (file) (install-file file include))
                         (find-files "src" "\\.h$"))
               #t))))))
    (home-page "https://troydhanson.github.io/uthash/")
    (synopsis
     "Hash tables, lists, and other data structures implemented as C macros")
    (description
     "uthash implements a hash table and a few other basic data structures
as C preprocessor macros.  It aims to be minimalistic and efficient: it's
around 1,000 lines of code which, being macros, inline automatically.

Unlike function calls with fixed prototypes, macros operate on untyped
arguments.  Thus, they are able to work with any type of structure and key.
Any C structure can be stored in a hash table by adding @code{UT_hash_handle}
to the structure and choosing one or more fields to act as the key.")
    (license license:bsd-2)))

(define-public sdsl-lite
  (package
    (name "sdsl-lite")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/simongog/sdsl-lite/"
                                  "releases/download/v" version "/"
                                  "sdsl-lite-" version
                                  ".tar.gz.offline.install.gz"))
              (sha256
               (base32
                "1v86ivv3mmdy802i9xkjpxb4cggj3s27wb19ja4sw1klnivjj69g"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "external") #t))
              (patches
                (list (origin
                        (method url-fetch)
                        (uri "https://salsa.debian.org/science-team/libsdsl/raw/debian/2.1.1+dfsg-2/debian/patches/0001-Patch-cmake-files.patch")
                        (file-name "sdsl-lite-dont-use-bundled-libraries.patch")
                        (sha256
                         (base32
                          "0m542xpys54bni29zibgrfpgpd0zgyny4h131virxsanixsbz52z")))))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-file "lib/libsdsl_static.a"
                          (string-append out "/lib/libsdsl.a")))
             #t))
        (add-after 'install 'install-pkgconfig-file
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (lib (string-append out "/lib")))
              (mkdir-p (string-append lib "/pkgconfig"))
              (with-output-to-file (string-append lib "/pkgconfig/sdsl-lite.pc")
                (lambda _
                  (format #t "prefix=~a~@
                          exec_prefix=${prefix}~@
                          libdir=${exec_prefix}/lib~@
                          includedir=${prefix}/include~@
                          ~@
                          ~@
                          Name: sdsl~@
                          Version: ~a~@
                          Description: SDSL: Succinct Data Structure Library~@
                          Libs: -L${libdir} -lsdsl -ldivsufsort -ldivsufsort64~@
                          Cflags: -I${includedir}~%"
                          out ,version)))
              #t))))))
    (propagated-inputs
     (list libdivsufsort))
    (home-page "https://github.com/simongog/sdsl-lite")
    (synopsis "Succinct data structure library")
    (description "The Succinct Data Structure Library (SDSL) is a powerful and
flexible C++11 library implementing succinct data structures.  In total, the
library contains the highlights of 40 research publications.  Succinct data
structures can represent an object (such as a bitvector or a tree) in space
close to the information-theoretic lower bound of the object while supporting
operations of the original object efficiently.  The theoretical time
complexity of an operation performed on the classical data structure and the
equivalent succinct data structure are (most of the time) identical.")
    (license license:gpl3+)))

(define-public tllist
  (package
    (name "tllist")
    (version "1.1.0")
    (home-page "https://codeberg.org/dnkl/tllist")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03296h1w0rnsj87490cgy007axngyg1v8w3z5nvm6x5gcs6b8rg1"))))
    (build-system meson-build-system)
    (synopsis "Typed link list for C")
    (description
     "@code{tllist} is a @dfn{typed linked list} C header file only library
implemented using pre-processor macros.  It supports primitive data types as
well as aggregated ones such as structs, enums and unions.")
    (license license:expat)))

(define-public libdivsufsort
  (package
    (name "libdivsufsort")
    (version "2.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/y-256/libdivsufsort")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fgdz9fzihlvjjrxy01md1bv9vh12rkgkwbm90b1hj5xpbaqp7z2"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; there are no tests
       #:configure-flags
       ;; Needed for rapmap and sailfish.
       '("-DBUILD_DIVSUFSORT64=ON")))
    (home-page "https://github.com/y-256/libdivsufsort")
    (synopsis "Lightweight suffix-sorting library")
    (description "libdivsufsort is a software library that implements a
lightweight suffix array construction algorithm.  This library provides a
simple and an efficient C API to construct a suffix array and a
Burrows-Wheeler transformed string from a given string over a constant-size
alphabet.  The algorithm runs in O(n log n) worst-case time using only 5n+O(1)
bytes of memory space, where n is the length of the string.")
    (license license:expat)))

(define-public robin-map
  (package
    (name "robin-map")
    (version "0.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Tessil/robin-map")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1li70vwsksva9c4yly90hjafgqfixi1g6d52qq9p6r60vqc4pkjj"))))
    (build-system cmake-build-system)
    (native-inputs
     (list boost))  ; needed for tests
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (mkdir "tests")
             (with-directory-excursion "tests"
               (invoke "cmake" "../../source/tests")
               (invoke "cmake" "--build" ".")
               (invoke "./tsl_robin_map_tests")))))))
    (home-page "https://github.com/Tessil/robin-map")
    (synopsis "C++ implementation of a fast hash map and hash set")
    (description "The robin-map library is a C++ implementation of a fast hash
map and hash set using open-addressing and linear robin hood hashing with
backward shift deletion to resolve collisions.

Four classes are provided: tsl::robin_map, tsl::robin_set, tsl::robin_pg_map
and tsl::robin_pg_set. The first two are faster and use a power of two growth
policy, the last two use a prime growth policy instead and are able to cope
better with a poor hash function.")
    (license license:expat)))

(define-public zix
  (package
    (name "zix")
    (version "0.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/drobilla/zix.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07pbq4bi64iv39swldfbcp7131b5n4hs64pgd417gqlwv8qvgjcw"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~(list "-Ddocs=disabled"))) ;needs "sphinxygen"
    (native-inputs (list pkg-config))
    (home-page "https://gitlab.com/drobilla/zix")
    (synopsis "C library of portability wrappers and data structures")
    (description
     "Zix is a C library of portability wrappers and data structures.  It
provides the following components:
@table @code
@item ZixAllocator A customizable allocator.
@item ZixBumpAllocator A simple realtime-safe bump-pointer allocator.
@item ZixBTree A page-allocated B-tree.
@item ZixHash An open-addressing hash table.
@item ZixRing A lock-free realtime-safe ring buffer.
@item ZixSem A portable semaphore wrapper.
@item ZixThread A portable thread wrapper.
@item ZixTree A binary search tree.
@item zixgest.h Digest functions suitable for hashing arbitrary data.
zix/filesystem.h Functions for working with filesystems.
@item zix/path.h Functions for working with filesystem paths lexically.
@end table")
    (license license:isc)))
