;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2017, 2019, 2020, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2020, 2021, 2022, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015 Jeff Mickey <j@codemac.net>
;;; Copyright © 2015-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2019, 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2018-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Antoine Côté <antoine.cote@posteo.net>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Ahmad Jarara <git@ajarara.io>
;;; Copyright © 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024, 2025 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages benchmark)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public zlib
  (package
    (name "zlib")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "http://zlib.net/zlib-"
                                 version ".tar.gz")
                  (string-append "mirror://sourceforge/libpng/zlib/"
                                 version "/zlib-" version ".tar.gz")))
       (sha256
        (base32
         "08yzf8xz0q7vxs8mnn74xmpxsrs6wy0aan55lpmpriysvyvv54ws"))))
    (build-system gnu-build-system)
    (outputs '("out" "static"))
    (arguments
     (list
      #:make-flags
      (if (target-mingw?)
          #~(list #$(string-append "PREFIX=" (%current-target-system) "-")
                  "BINARY_PATH = $(prefix)/bin"
                  "INCLUDE_PATH = $(prefix)/include"
                  "LIBRARY_PATH = $(prefix)/lib"
                  "SHARED_MODE = 1"
                  (string-append "prefix = " #$output))
          #~'())
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              ;; Zlib's home-made `configure' fails when passed
              ;; extra flags like `--enable-fast-install', so we need to
              ;; invoke it with just what it understand.
              (setenv "CC" #$(cc-for-target))
              ;; 'configure' doesn't understand '--host'.
              #$@(if (%current-target-system)
                     #~((setenv "CHOST" #$(%current-target-system)))
                     #~())

              #$@(if (target-mingw?)
                     #~((rename-file "win32/Makefile.gcc" "Makefile"))
                     #~((invoke "./configure"
                                (string-append "--prefix=" #$output))))))
          (add-after 'install 'move-static-library
            (lambda _
              (with-directory-excursion (string-append #$output "/lib")
                (install-file "libz.a" (string-append #$output:static
                                                      "/lib"))
                (delete-file "libz.a")))))))
    (home-page "https://zlib.net/")
    (synopsis "Compression library")
    (description
     "zlib is designed to be a free, general-purpose, legally unencumbered --
that is, not covered by any patents -- lossless data-compression library for
use on virtually any computer hardware and operating system.  The zlib data
format is itself portable across platforms.  Unlike the LZW compression method
used in Unix compress(1) and in the GIF image format, the compression method
currently used in zlib essentially never expands the data. (LZW can double or
triple the file size in extreme cases.)  zlib's memory footprint is also
independent of the input data and can be reduced, if necessary, at some cost
in compression.")
    (license license:zlib)))

(define-public minizip
  (package
    (name "minizip")
    (version (package-version zlib))
    (source (package-source zlib))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "contrib/minizip"))))))
    (native-inputs
     (list autoconf automake libtool))
    (propagated-inputs (list zlib))
    (home-page (package-home-page zlib))
    (synopsis "Zip Compression library")
    (description
     "Minizip is a minimalistic library that supports compressing,
extracting and viewing ZIP archives.  This version is extracted from
the @code{zlib} source.")
    (license (package-license zlib))))

(define-public kubazip
  (package
    (name "kubazip")
    (version "0.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kuba--/zip")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "182p059nvs5vpdzmmkn2g6mphsvkjdcxq6gpbh58dcsjj507j9ab"))))
    (build-system cmake-build-system)
    (synopsis "Simple zip library")
    (home-page "https://github.com/kuba--/zip")
    (description
     "This package provides a simple zip library based on miniz.")
    (license license:unlicense)))

(define-public fastjar
  (package
   (name "fastjar")
   (version "0.98")
   (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/fastjar/fastjar-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0iginbz2m15hcsa3x4y7v3mhk54gr1r7m3ghx0pg4n46vv2snmpi"))))
   (build-system gnu-build-system)
   (inputs (list zlib))
   (home-page "https://savannah.nongnu.org/projects/fastjar")
   (synopsis "Replacement for Sun's 'jar' utility")
   (description
    "FastJar is an attempt to create a much faster replacement for Sun's
@code{jar} utility.  Instead of being written in Java, FastJar is written in C.")
   (license license:gpl2+)))

(define-public libtar
  (package
   (name "libtar")
   (version "1.2.20")
   (source (origin
            (method url-fetch)
            (uri (list
                   (string-append
                     "ftp://ftp.feep.net/pub/software/libtar/libtar-"
                     version ".tar.gz")
                   (string-append
                     "mirror://debian/pool/main/libt/libtar/libtar_"
                     version ".orig.tar.gz")))
            (sha256
             (base32
              "02cihzl77ia0dcz7z2cga2412vyhhs5pa2355q4wpwbyga2lrwjh"))
            (patches
             (search-patches "libtar-CVE-2013-4420.patch"
                             "libtar-CVE-2021-33643-CVE-2021-33644.patch"
                             "libtar-CVE-2021-33645-CVE-2021-33646.patch"))))
   (build-system gnu-build-system)
   (arguments `(#:tests? #f)) ; no "check" target
   (native-inputs
    (list autoconf automake libtool))
   (inputs
    (list zlib))
   (synopsis "C library for manipulating POSIX tar files")
   (description
    "libtar is a C library for manipulating POSIX tar files.  It handles
adding and extracting files to/from a tar archive.")
   (home-page "https://repo.or.cz/libtar.git")
   (license license:bsd-3)))

(define-public gzip
  (package
   (name "gzip")
   (version "1.14")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gzip/gzip-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1ihaii7d3vznvj9vk1fkmpvd7pqbz0c8fyzr2pvgs2r2pn0vi9q1"))))
   (build-system gnu-build-system)
   (synopsis "General file (de)compression (using lzw)")
   (arguments
    ;; FIXME: The test suite wants `less', and optionally Perl.
    '(#:tests? #f
      #:configure-flags '("ac_cv_prog_LESS=\"less\"")
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'use-absolute-name-of-gzip
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "gunzip.in"
              (("exec 'gzip'")
               (string-append "exec " (assoc-ref outputs "out")
                              "/bin/gzip"))))))))
   (inputs
    `(,@(if (%current-target-system)
            `(("bash" ,bash-minimal))
            '())))
   (description
    "GNU Gzip provides data compression and decompression utilities; the
typical extension is \".gz\".  Unlike the \"zip\" format, it compresses a single
file; as a result, it is often used in conjunction with \"tar\", resulting in
\".tar.gz\" or \".tgz\", etc.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/gzip/")))

(define-public bzip2
  (package
    (name "bzip2")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceware.org/pub/bzip2/bzip2-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0s92986cv0p692icqlw1j42y9nld8zd83qwhzbqd61p1dqbh6nmb"))))
    (build-system gnu-build-system)
    (arguments
     (list #:modules '((guix build gnu-build-system)
                       (guix build utils)
                       (ice-9 ftw)
                       (srfi srfi-1))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'set-paths 'hide-input-bzip2
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((bzip2 (assoc-ref inputs "bzip2")))
                     (if bzip2
                         ;; Prevent the build system from retaining a reference to
                         ;; BZIP2 from INPUTS.
                         (begin
                           (setenv "LIBRARY_PATH"
                                   (string-join (delete (string-append bzip2 "/lib")
                                                        (string-split (getenv "LIBRARY_PATH")
                                                                      #\:))
                                                ":"))
                           (format #t "environment variable `LIBRARY_PATH' set to `~a'~%"
                                   (getenv "LIBRARY_PATH")))
                         (format #t "no bzip2 found, nothing done~%"))
                     #t)))
               (replace 'configure
                 (lambda* (#:key target #:allow-other-keys)
                   (when #$(%current-target-system)
                     ;; Cross-compilation: use the cross tools.
                     (substitute* (find-files "." "Makefile")
                       (("CC=.*$")
                        (string-append "CC = " target "-gcc\n"))
                       (("AR=.*$")
                        (string-append "AR = " target "-ar\n"))
                       (("RANLIB=.*$")
                        (string-append "RANLIB = " target "-ranlib\n"))
                       (("^all:(.*)test" _ prerequisites)
                        ;; Remove 'all' -> 'test' dependency.
                        (string-append "all:" prerequisites "\n"))))
                   #t))
               (add-before 'build 'build-shared-lib
                 (lambda* (#:key inputs #:allow-other-keys)
                   (patch-makefile-SHELL "Makefile-libbz2_so")
                   (invoke "make" "-f" "Makefile-libbz2_so")))
               (add-after 'install 'install-shared-lib
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; The Makefile above does not have an 'install' target, nor does
                   ;; it create all the (un)versioned symlinks, so we handle it here.
                   (let* ((out    (assoc-ref outputs "out"))
                          (libdir (string-append out "/lib"))
                          (soname "libbz2.so")
                          ;; Locate the built library (e.g. "libbz2.so.1.0.6").
                          (lib (car (scandir "."
                                             (lambda (file)
                                               (and (string-prefix? soname file)
                                                    (eq? 'regular
                                                         (stat:type (lstat file))))))))
                          (soversion (string-drop lib (+ 1 (string-length soname)))))
                     (install-file lib libdir)
                     (with-directory-excursion libdir
                       ;; Create symlinks libbz2.so.1 -> libbz2.so.1.0, etc.
                       (let loop ((base soname)
                                  (numbers (string-split soversion #\.)))
                         (unless (null? numbers)
                           (let ((so-file (string-append base "." (car numbers))))
                             (symlink so-file base)
                             (loop so-file (cdr numbers))))))
                     #t)))
               (add-after 'install-shared-lib 'move-static-lib
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out"))
                         (static (assoc-ref outputs "static")))
                     (with-directory-excursion (string-append out "/lib")
                       (install-file "libbz2.a" (string-append static "/lib"))
                       (delete-file "libbz2.a")
                       #t))))
               (add-after 'install-shared-lib 'patch-scripts
                 (lambda* (#:key outputs inputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out")))
                     (substitute* (string-append out "/bin/bzdiff")
                       (("/bin/rm") "rm")))
                   #t)))

           #:make-flags #~(list (string-append "PREFIX="
                                               (assoc-ref %outputs "out")))

           ;; Don't attempt to run the tests when cross-compiling.
           #:tests? (not (%current-target-system))))
    (inputs
     `(,@(if (%current-target-system)
             `(("bash" ,bash-minimal))
             '())))
    (outputs '("out" "static"))
    (synopsis "High-quality data compression program")
    (description
     "bzip2 is a freely available, patent free (see below), high-quality data
compressor.  It typically compresses files to within 10% to 15% of the best
available techniques (the PPM family of statistical compressors), whilst
being around twice as fast at compression and six times faster at
decompression.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))
    (home-page "https://web.archive.org/web/20180801004107/http://www.bzip.org/")))

(define-public lbzip2
  ;; The last 2.5 release is 4 years behind the newest commit (from 2018) and
  ;; may create files that can't even be decompressed by newer bzip2 versions.
  (let ((commit "b6dc48a7b9bfe6b340ed1f6d72133608ad57144b")
        (revision "0"))
    (package
      (name "lbzip2")
      (version (git-version "2.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kjn/lbzip2")
               (commit commit)))
         (sha256
          (base32 "140xp00dmjsr6c3dwb4dwf0pzlgf159igri321inbinsjiclkngy"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-gnulib
             (lambda* (#:key inputs native-inputs #:allow-other-keys)
               (let ((gnulib
                      (dirname (search-input-file (or native-inputs inputs)
                                                  "gnulib-tool.py"))))
                 (copy-recursively gnulib "lib")
                 (setenv "PATH" (string-append "lib:" (getenv "PATH")))
                 #t)))
           (delete 'bootstrap)          ; gnulib still has unpatched shebangs
           (add-after 'patch-source-shebangs 'bootstrap
             (lambda _
               (invoke "sh" "build-aux/autogen.sh")
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gnulib"
          ,(let ((commit "2d431ac35c4943a3655c07ba91870d2323321b43"))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "git://git.savannah.gnu.org/gnulib.git")
                     (commit commit)))
               (sha256
                (base32 "1f0xr4w89bqvhzsfcflcagdixidrk41k00k7kpr91w9lazfis4kf"))
               (file-name (git-file-name "gnulib" commit)))))
         ("perl" ,perl)))
      (synopsis "Parallel bzip2 compression utility")
      (description
       "lbzip2 is a multi-threaded compression utility with support for the
bzip2 compressed file format.  lbzip2 can process standard bz2 files in
parallel.  It uses POSIX threading model (pthreads), which allows it to take
full advantage of symmetric multiprocessing (SMP) systems.  It has been proven
to scale linearly, even to over one hundred processor cores.  lbzip2 is fully
compatible with bzip2 – both at file format and command line level.")
      ;; lbzip2.org now looks fishy.  There is no source code to be found.
      ;; Reported upstream: <https://github.com/kjn/lbzip2/issues/26>.
      (home-page "https://github.com/kjn/lbzip2")
      (license license:gpl3+))))

(define-public pbzip2
  (package
    (name "pbzip2")
    (version "1.1.13")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://launchpad.net/pbzip2/"
                                 (version-major+minor version) "/" version
                                 "/+download/" name "-" version ".tar.gz"))
             (sha256
              (base32
               "1rnvgcdixjzbrmcr1nv9b6ccrjfrhryaj7jwz28yxxv6lam3xlcg"))))
    (build-system gnu-build-system)
    (inputs
     (list bzip2))
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure))  ; no configure script
       #:make-flags (list (string-append "PREFIX=" %output))))
    (home-page (string-append "https://web.archive.org/web/20180412020219/"
                              "http://compression.ca/pbzip2/"))
    (synopsis "Parallel bzip2 implementation")
    (description
     "Pbzip2 is a parallel implementation of the bzip2 block-sorting file
compressor that uses pthreads and achieves near-linear speedup on SMP machines.
The output of this version is fully compatible with bzip2 v1.0.2 (i.e. anything
compressed with pbzip2 can be decompressed with bzip2).")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

;; We call this streambuf-shrinkwrap because a Python package with the name
;; "shrinkwrap" already exists.
(define-public streambuf-shrinkwrap
  (package
    (name "streambuf-shrinkwrap")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jonathonl/shrinkwrap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1819va068kp68ks52f7h0dq74xq059a4m86zls2k7dj5zxhs8qs2"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      '(list "-DSHRINKWRAP_PREFER_STATIC=OFF"
             "-DBUILD_TESTS=ON")))
    (propagated-inputs (list bzip2 xz zlib `(,zstd "lib")))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/jonathonl/shrinkwrap")
    (synopsis "Wrapper around std::streambuf for zstd, xz, gzip, and bgzf files")
    (description
     "Shrinkwrap provides a @code{std::streambuf} wrapper for various compression
formats, including zstd, xz, gzip, and bgzf.")
    (license license:expat)))

(define-public xz
  (package
   (name "xz")
   (version "5.4.5")
   (source (origin
            (method url-fetch)
            (uri (list (string-append "http://tukaani.org/xz/xz-" version
                                      ".tar.gz")
                       (string-append "http://multiprecision.org/guix/xz-"
                                      version ".tar.gz")))
            (sha256
             (base32
              "1mmpwl4kg1vs6n653gkaldyn43dpbjh8gpk7sk0gps5f6jwr0p0k"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'install 'move-static-lib
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out    (assoc-ref outputs "out"))
                  (static (assoc-ref outputs "static")))
              (mkdir-p (string-append static "/lib"))
              (rename-file (string-append out "/lib/liblzma.a")
                           (string-append static "/lib/liblzma.a"))
              ;; Remove reference to the static library from the .la file
              ;; so Libtool does the right thing when both the shared and
              ;; static library is available.
              (substitute* (string-append out "/lib/liblzma.la")
                (("^old_library='liblzma.a'") "old_library=''"))
              #t))))))
   (outputs '("out" "static"))
   (inputs
    `(,@(if (%current-target-system)
            `(("bash" ,bash-minimal))
            '())))
   (synopsis "General-purpose data compression")
   (description
    "XZ Utils is free general-purpose data compression software with high
compression ratio.  XZ Utils were written for POSIX-like systems, but also
work on some not-so-POSIX systems.  XZ Utils are the successor to LZMA Utils.

The core of the XZ Utils compression code is based on LZMA SDK, but it has
been modified quite a lot to be suitable for XZ Utils.  The primary
compression algorithm is currently LZMA2, which is used inside the .xz
container format.  With typical files, XZ Utils create 30 % smaller output
than gzip and 15 % smaller output than bzip2.")
   (license (list license:gpl2+ license:lgpl2.1+)) ; bits of both
   (home-page "https://tukaani.org/xz/")))

(define-public heatshrink
  (package
    (name "heatshrink")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atomicobject/heatshrink/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sdhvk27yz8kahw18j8pddbpkgl78v8rh8fx6wspc3acj7w7yvrn"))
       ;; Add CMake build script, wanted by prusa-slicer and libbgcode, which are the
       ;; only users of this library (see
       ;; <https://github.com/NixOS/nixpkgs/pull/269758/commits/fa36136ceed0e2c58e0c9e21492a7e60c3a64470>.)
       (patches (search-patches "heatshrink-add-cmake.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; XXX: No tests available with CMake (see
      ;; <https://github.com/atomicobject/heatshrink/pull/77>.)
      #:tests? #f))
    (home-page "https://github.com/atomicobject/heatshrink/")
    (synopsis "Data compression library for embedded/real-time systems")
    (description
     "A data compression/decompression library for embedded/real-time systems.

Among its features are:
@itemize
@item Low memory usage (as low as 50 bytes.)  It is useful for some cases with less
than 50 bytes, and useful for many general cases with less than 300 bytes.
@item Incremental, bounded CPU use.  It can be used to chew on input data in
arbitrarily tiny bites.  This is a useful property in hard real-time environments.
@item Can use either static or dynamic memory allocation.
@end itemize
")
    (license license:isc)))

(define-public lhasa
  (package
    (name "lhasa")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/fragglet/lhasa/releases/download/v"
                    version "/lhasa-" version ".tar.gz"))
              (sha256
               (base32
                "1cy5429c7h1dp4l5y9ncdy40qarl9ix31ylkzdi8al2c60y8iz57"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'set-up-test-environment
                          (lambda* (#:key inputs #:allow-other-keys)
                            (setenv "TZDIR"
                                    (search-input-directory inputs
                                                            "share/zoneinfo")))))))
    (native-inputs (list tzdata-for-tests))
    (home-page "https://fragglet.github.io/lhasa/")
    (synopsis "LHA archive decompressor")
    (description
     "Lhasa is a replacement for the Unix LHa tool, for decompressing
@file{.lzh} (LHA / LHarc) and .lzs (LArc) archives.  The backend for the tool is
a library, so that it can be reused for other purposes.  Lhasa aims to be
compatible with as many types of @file{.lzh}/@file{lzs} archives as possible.
It also aims to generate the same output as the (non-free) Unix @command{lha}
tool, so that it will act as a free drop-in replacement.")
    (license license:isc)))

(define-public lzo
  (package
    (name "lzo")
    (version "2.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.oberhumer.com/opensource/lzo/download/lzo-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0wm04519pd3g8hqpjqhfr72q8qmbiwqaxcs3cndny9h86aa95y60"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-shared")))
    (home-page "http://www.oberhumer.com/opensource/lzo")
    (synopsis
     "Data compression library suitable for real-time data de-/compression")
    (description
     "LZO is a data compression library which is suitable for data
de-/compression in real-time.  This means it favours speed over
compression ratio.

LZO is written in ANSI C.  Both the source code and the compressed data
format are designed to be portable across platforms.")
    (license license:gpl2+)))

(define-public lzop
  (package
    (name "lzop")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.lzop.org/download/lzop-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0h9gb8q7y54m9mvy3jvsmxf21yx8fc3ylzh418hgbbv0i8mbcwky"))))
    (build-system gnu-build-system)
    (inputs (list lzo))
    (home-page "https://www.lzop.org/")
    (synopsis "Compress or expand files")
    (description
     "Lzop is a file compressor which is very similar to gzip.  Lzop uses the
LZO data compression library for compression services, and its main advantages
over gzip are much higher compression and decompression speed (at the cost of
some compression ratio).")
    (license license:gpl2+)))

(define-public lzip
  (package
    (name "lzip")
    (version "1.25")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/lzip/lzip-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0vx5y39y6ipks1lcc203mrxx7x9xf04nx1dxyl9m2gxqixnqlh89"))))
    (build-system gnu-build-system)
    (arguments
     ;; The configure script doesn't recognise the --build or --host
     ;; arguments, so set CXX here
     `(,@(if (%current-target-system)
             `(#:make-flags (list ,(string-append "CXX=" (cxx-for-target))))
             '())))
    (home-page "https://www.nongnu.org/lzip/lzip.html")
    (synopsis "Lossless data compressor based on the LZMA algorithm")
    (description
     "Lzip is a lossless data compressor with a user interface similar to the
one of gzip or bzip2.  Lzip decompresses almost as fast as gzip and compresses
more than bzip2, which makes it well-suited for software distribution and data
archiving.  Lzip is a clean implementation of the LZMA algorithm.")
    (license license:gpl3+)))

(define-public lziprecover
  (package
    (name "lziprecover")
    (version "1.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/lzip/lziprecover/"
                                  "lziprecover-" version ".tar.gz"))
              (sha256
               (base32
                "0wmmyi03fv2lflsir5ldrsv04q57k3hmlqajzb1m3p86gwbh967j"))))
    (build-system gnu-build-system)
    (arguments
     ;; The configure script doesn't recognise the --build or --host
     ;; arguments, so set CXX here
     `(,@(if (%current-target-system)
             `(#:make-flags (list ,(string-append "CXX=" (cxx-for-target))))
             '())))
    (home-page "https://www.nongnu.org/lzip/lziprecover.html")
    (synopsis "Recover and decompress data from damaged lzip files")
    (description
     "Lziprecover is a data recovery tool and decompressor for files in the lzip
compressed data format (.lz).  It can test the integrity of lzip files, extract
data from damaged ones, and repair most files with small errors (up to one
single-byte error per member) entirely.

Lziprecover is not a replacement for regular backups, but a last line of defence
when even the backups are corrupt.  It can recover files by merging the good
parts of two or more damaged copies, such as can be easily produced by running
@command{ddrescue} on a failing device.

This package also includes @command{unzcrash}, a tool to test the robustness of
decompressors when faced with corrupted input.")
    (license (list license:bsd-2        ; arg_parser.{cc,h}
                   license:gpl2+))))    ; everything else

(define-public sharutils
  (package
    (name "sharutils")
    (version "4.15.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/sharutils/sharutils-"
                          version ".tar.xz"))
      (patches (search-patches "sharutils-CVE-2018-1000097.patch"))
      (sha256
       (base32
        "16isapn8f39lnffc3dp4dan05b7x6mnc76v6q5nn8ysxvvvwy19b"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Adjust for newer libc versions.
          (substitute* (find-files "lib" "\\.c$")
            (("#if defined _IO_ftrylockfile")
             "#if defined _IO_EOF_SEEN"))
          (substitute* "lib/stdio-impl.h"
            (("^/\\* BSD stdio derived implementations")
             (string-append "#if !defined _IO_IN_BACKUP && defined _IO_EOF_SEEN\n"
                            "# define _IO_IN_BACKUP 0x100\n"
                            "#endif\n\n"
                            "/* BSD stdio derived implementations")))
          ;; ... and for newer GCC with -fno-common.
          (substitute* '("src/shar-opts.h"
                         "src/unshar-opts.h"
                         "src/uudecode-opts.h"
                         "src/uuencode-opts.h")
            (("char const \\* const program_name" all)
             (string-append "extern " all)))))))
    (build-system gnu-build-system)
    (native-inputs
     (list which))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'unpatch-source-shebang
           ;; revert the patch-shebang phase on a script which is
           ;; in fact test data
           (lambda _
             (substitute* "tests/shar-1.ok"
               (((which "sh")) "/bin/sh")))))))
    (home-page "https://www.gnu.org/software/sharutils/")
    (synopsis "Archives in shell scripts, uuencode/uudecode")
    (description
     "GNU sharutils is a package for creating and manipulating shell
archives that can be readily emailed.  A shell archive is a file that can be
processed by a Bourne-type shell to unpack the original collection of files.
This package is mostly for compatibility and historical interest.")
    (license license:gpl3+)))

(define-public sfarklib
  (package
    (name "sfarklib")
    (version "2.24")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/raboof/sfArkLib")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jrxy24gak7q5ml06p5jjgzk9i5r2mkfjk4ycirkp4kg7k5a237w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (substitute* "Makefile"
                      (("/usr/local") (assoc-ref outputs "out")))
                    #t)))))
    (inputs
     (list zlib))
    (home-page "https://github.com/raboof/sfArkLib")
    (synopsis "Library for SoundFont decompression")
    (description
     "SfArkLib is a C++ library for decompressing SoundFont files compressed
with the sfArk algorithm.")
    (license license:gpl3+)))

(define-public minizip-ng
  (package
    (name "minizip-ng")
    (version "4.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/zlib-ng/minizip-ng")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lgx4s4aykxn8x3b4m4c4isasd2608bbyfm4lxc2spcc4xqwhzkz"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON"
                                     "-DMZ_BUILD_TESTS=ON"
                                     "-DMZ_BUILD_UNIT_TESTS=ON")))
    (native-inputs (list googletest pkg-config))
    (inputs (list openssl zlib `(,zstd "lib")))
    (home-page "https://github.com/zlib-ng/minizip-ng")
    (synopsis "Zip manipulation library")
    (description "@code{minizip-ng} is a zip manipulation library written in
C, forked from the zip manipulation library found in the zlib distribution.")
    (license license:bsd-3)))

(define-public sfarkxtc
  (let ((commit "13cd6f93725a90d91ec5ea75babf1dbd694ac463")
        (revision "1"))
    (package
      (name "sfarkxtc")
      (version (git-version "0" revision commit))
      (source (origin
                ;; There are no release tarballs, so we just fetch the latest
                ;; commit at this time.
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/raboof/sfarkxtc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1mb1jyk1m11l1gppd9hmql9cyp55sdf7jk5rbc7acky1z4k4mv19"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ;no "check" target
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "Makefile"
                 (("/usr/local") (assoc-ref outputs "out")))
               #t)))))
      (inputs
       (list zlib sfarklib))
      (home-page "https://github.com/raboof/sfarkxtc")
      (synopsis "Basic sfArk decompressor")
      (description "SfArk extractor converts SoundFonts in the compressed legacy
sfArk file format to the uncompressed sf2 format.")
      (license license:gpl3+))))

(define-public draco
  (package
    (name "draco")
    (version "1.5.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/draco")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v1idvqr9mww9wi36yzb10lq66ls78dlrgnxchjjjv5paw2g0mk3"))))
    (build-system cmake-build-system)
    (arguments
     ;; There is a testdata directory but apparently no actual tests.
     ;; src/draco/tools/install_test contains a sanity check, but this
     ;; check is useless here.
     (list #:tests? #f
           #:configure-flags
           #~(list "-DBUILD_SHARED_LIBS=true"
                   (string-append "-DDRACO_EIGEN_PATH="
                                  #$(this-package-input "eigen"))
                   (string-append "-DDRACO_FILESYSTEM_PATH="
                                  #$(this-package-input "gulrak-filesystem"))
                   (string-append "-DDRACO_TINYGLTF_PATH="
                                  #$(this-package-input "tinygltf")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'delete-static-lib
                 (lambda _
                   (delete-file (string-append #$output
                                               "/lib/libdraco.a")))))))
    (inputs (list eigen gulrak-filesystem tinygltf))
    (home-page "https://google.github.io/draco/")
    (synopsis "Compress and decompress 3D geometric meshes and point clouds")
    (description "Draco is a library for compressing and decompressing 3D
geometric meshes and point clouds.  It is intended to improve the storage and
transmission of 3D graphics.")
    (license license:asl2.0)))

(define-public libmspack
  (package
    (name "libmspack")
    (home-page "https://cabextract.org.uk/libmspack/")
    (version "0.11")
    (source
     (origin
      (method url-fetch)
      (uri (string-append home-page name "-" version "alpha.tar.gz"))
      (sha256
       (base32 "06x2xq73lchw5lcq386sx9wk05v21s2f38bi3dwkdk5fy2r1zpbh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (synopsis "Compression tools for some formats used by Microsoft")
    (description
     "The purpose of libmspack is to provide both compression and
decompression of some loosely related file formats used by Microsoft.")
    (license license:lgpl2.1+)))

(define-public lz4
  (package
    (name "lz4")
    (version "1.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url "https://github.com/lz4/lz4")
                           (commit (string-append "v" version))))
       (sha256
        (base32 "1w02kazh1fps3sji2sn89fz862j1199c5ajrqcgl1bnlxj09kcbz"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (outputs (list "out" "static"))
    (native-inputs
     (list python)) ;; For tests.
    (arguments
     `(;; Not designed for parallel testing.
       ;; See https://github.com/lz4/lz4/issues/957#issuecomment-737419821
       #:parallel-tests? #f
       #:test-target "test"
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'check 'disable-broken-test
           (lambda _
             (substitute* "tests/Makefile"
               ;; This fails when $prefix is not a single top-level directory.
               (("^test: (.*) test-install" _ targets)
                (string-append "test: " targets)))))
         (add-after 'install 'move-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (static (assoc-ref outputs "static")))
               (mkdir-p (string-append static "/lib"))
               (rename-file (string-append out "/lib/liblz4.a")
                            (string-append static "/lib/liblz4.a"))))))))
    (home-page "https://www.lz4.org")
    (synopsis "Compression algorithm focused on speed")
    (description "LZ4 is a lossless compression algorithm, providing
compression speed at 400 MB/s per core (0.16 Bytes/cycle).  It also features an
extremely fast decoder, with speed in multiple GB/s per core (0.71 Bytes/cycle).
A high compression derivative, called LZ4_HC, is also provided.  It trades CPU
time for compression ratio.")
    ;; The libraries (lz4, lz4hc, and xxhash) are BSD licenced. The command
    ;; line interface programs (lz4, fullbench, fuzzer, datagen) are GPL2+.
    (license (list license:bsd-2 license:gpl2+))))

(define-public squashfs-tools
  (package
    (name "squashfs-tools")
    (version "4.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/plougher/squashfs-tools")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14nisidxx2d2qivyv7xfcg59qkj4fjiniir7nvymazdsng63gcr1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                      ; no check target
      #:make-flags
      #~(list
         (string-append "CC=" #$(cc-for-target))
         "XZ_SUPPORT=1"
         "LZO_SUPPORT=1"
         "LZ4_SUPPORT=1"
         "ZSTD_SUPPORT=1"
         (string-append "INSTALL_DIR=" #$output "/bin")
         (string-append "INSTALL_MANPAGES_DIR=" #$output "/share/man/man1"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (chdir "squashfs-tools")))
          (add-after 'patch-source-shebangs 'patch-generated-source-shebangs
            (lambda _
              (substitute* (find-files "generate-manpages" "\\.sh")
                (("print \"#!/bin/sh")
                 (string-append "print \"#!" (which "sh")))))))))
    (native-inputs
     (list coreutils-minimal help2man which))
    (inputs
     (list lz4 lzo xz zlib `(,zstd "lib")))
    (home-page "https://github.com/plougher/squashfs-tools")
    (synopsis "Tools to create and extract squashfs file systems")
    (description
     "Squashfs is a highly compressed read-only file system for Linux.  It
compresses files, inodes, and directories with one of several compressors.
All blocks are packed to minimize the data overhead, and block sizes of
between 4K and 1M are supported.  It is intended to be used for archival use,
for live media, and for embedded systems where low overhead is needed.
This package allows you to create and extract such file systems.")
    (license license:gpl2+)))

(define-public squashfs-tools-ng
  (package
    (name "squashfs-tools-ng")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AgentD/squashfs-tools-ng")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12ipqmjp10574sz64ls8qbgzkxz5dcbzk0l2fxyh2yrrhnjp34mi"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled third-party libraries.
           (for-each (lambda (directory)
                       (substitute* "Makefile.am"
                         (((format #f "^include ~a.*" directory)) ""))
                       (delete-file-recursively directory))
                     (list "lib/lz4"
                           "lib/zlib"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     `(("libselinux" ,libselinux)

       ;; Compression algorithms.
       ("bzip2" ,bzip2)
       ("lz4" ,lz4)
       ("lzo" ,lzo)
       ("xz" ,xz)
       ("zlib" ,zlib)
       ("zstd:lib" ,zstd "lib")))
    (home-page "https://github.com/AgentD/squashfs-tools-ng")
    (synopsis "Tools to create and extract squashfs file systems")
    (description
     "Squashfs is a highly compressed read-only file system for Linux.  It
compresses files, inodes, and directories with one of several compressors.
All blocks are packed to minimize the data overhead, and block sizes of
between 4K and 1M are supported.  It is intended to be used for archival use,
for live media, and for embedded systems where low overhead is needed.

The squashfs-tools-ng package offers alternative tooling to create and extract
such file systems.  It is not based on the older squashfs-tools package and
its tools have different names:

@enumerate
@item @command{gensquashfs} produces SquashFS images from a directory or
@command{gen_init_cpio}-like file listings and can generate SELinux labels.
@item @command{rdsquashfs} inspects and unpacks SquashFS images.
@item @command{sqfs2tar} and @command{tar2sqfs} convert between SquashFS and
tarballs.
@item @command{sqfsdiff} compares the contents of two SquashFS images.
@end enumerate

These commands are largely command-line wrappers around the included
@code{libsquashfs} library that intends to make SquashFS available to other
applications as an embeddable, extensible archive format.

Both the library and tools operate deterministically: same input will produce
byte-for-byte identical output.")
    ;; Upstream goes to some lengths to ensure that libsquashfs is LGPL3+.
    (license license:gpl3+)))

(define-public pigz
  (package
    (name "pigz")
    (version "2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://zlib.net/pigz/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l0g2zaz05pl3pijsjd8lqxqj122di88rggpr5cvw3hz1r7jp1zb"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))
           #:test-target "test"
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (replace 'install
                 (lambda _
                   (let* ((bin (string-append #$output "/bin"))
                          (man (string-append #$output "/share/man/man1")))
                     (install-file "pigz" bin)
                     (symlink "pigz" (string-append bin  "/unpigz"))
                     (install-file "pigz.1" man)))))))
    (native-inputs (list which))
    (inputs (list zlib))
    (home-page "https://zlib.net/pigz/")
    (synopsis "Parallel implementation of gzip")
    (description
     "This package provides a parallel implementation of gzip that exploits
multiple processors and multiple cores when compressing data.")

    ;; Things under zopfli/ are under ASL2.0, but 4 files at the top-level,
    ;; written by Mark Adler, are under another non-copyleft license.
    (license license:asl2.0)))

(define-public pixz
  (package
    (name "pixz")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/vasi/pixz/releases/download/v" version
                    "/pixz-" version ".tar.xz"))
              (sha256
               (base32
                "1ifxr18f2h75gkcrkx8033kwmwmrcgxshpaawyc2n4dzn1p2rqz5"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config libarchive))
    (home-page "https://github.com/vasi/pixz")
    (synopsis "Parallel indexing implementation of LZMA")
    (description
     "The existing XZ Utils provide great compression in the .xz file format,
but they produce just one big block of compressed data.  Pixz instead produces
a collection of smaller blocks which makes random access to the original data
possible and can compress in parallel.  This is especially useful for large
tarballs.")
    (license license:bsd-2)))

(define-public cabextract
 (package
   (name "cabextract")
   (home-page "https://cabextract.org.uk/")
   (version "1.11")
   (source (origin
              (method url-fetch)
              (uri (string-append home-page "cabextract-" version ".tar.gz"))
              (sha256
               (base32
                "1iis7a19n26dax3gsnrw9kb0vwq46rbpicnlyf7p2k2y2nqnsm5m"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled libmspack.
                  (delete-file-recursively "mspack")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--with-external-libmspack")
       #:phases
       (modify-phases %standard-phases
         ;; cabextract needs some of libmspack's header files.
         ;; These are located in the "mspack" directory of libmspack.
         (add-before 'build 'unpack-libmspack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((dir-name "libmspack-src"))
               (mkdir dir-name)
               (invoke "tar" "-xvf" (assoc-ref inputs "libmspack-source")
                       "-C" dir-name "--strip-components" "1")
               (rename-file (string-append dir-name "/mspack")
                            "mspack")
               (delete-file-recursively dir-name)
               #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("libmspack" ,libmspack)
       ("libmspack-source" ,(package-source libmspack))))
    (synopsis "Tool to unpack Cabinet archives")
    (description "Extracts files out of Microsoft Cabinet (.cab) archives")
    ;; Some source files specify gpl2+, lgpl2+, however COPYING is gpl3.
    (license license:gpl3+)))

(define-public libjcat
  (package
    (name "libjcat")
    (version "0.1.14")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/hughsie/libjcat")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dhyqhs2lg9ka47n8d1lalvic2m3rn5k6mya9bjyw242smkgzpjw"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection help2man pkg-config))
    (inputs
     (list glib
           gnupg
           gnutls
           gpgme
           json-glib
           vala))
    (home-page "https://github.com/hughsie/libjcat")
    (synopsis "Library for reading and writing Jcat files")
    (description
     "This library allows reading and writing gzip-compressed JSON catalog
files, which can be used to store GPG, PKCS-7 and SHA-256 checksums for each
file.")
    (license license:lgpl2.1+)))

(define-public xdelta
  (package
    (name "xdelta")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jmacd/xdelta")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09mmsalc7dwlvgrda56s2k927rpl3a5dzfa88aslkqcjnr790wjy"))
       (snippet
        ;; This file isn't freely distributable and has no effect on building.
        '(begin
           (delete-file "xdelta3/draft-korn-vcdiff.txt")
           #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-build-directory
           (lambda _ (chdir "xdelta3") #t)))))
    (home-page "http://xdelta.org")
    (synopsis "Delta encoder for binary files")
    (description "xdelta encodes only the differences between two binary files
using the VCDIFF algorithm and patch file format described in RFC 3284.  It can
also be used to apply such patches.  xdelta is similar to @command{diff} and
@command{patch}, but is not limited to plain text and does not generate
human-readable output.")
    (license license:asl2.0)))

(define-public lrzip
  (package
    (name "lrzip")
    (version "0.651")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://ck.kolivas.org/apps/lrzip/lrzip-" version ".tar.xz"))
       (sha256
        (base32 "1y822rpl7ak57s1a4xzd6ja82cp3sff9axrpkdn5khcpn3n8vga8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(;; nasm is only required when building for 32-bit x86 platforms
       ,@(if (string-prefix? "i686" (or (%current-target-system)
                                        (%current-system)))
             `(("nasm" ,nasm))
             '())
       ("perl" ,perl)))
    (inputs
     (list bzip2 lz4 lzo zlib))
    (home-page "http://ck.kolivas.org/apps/lrzip/")
    (synopsis "Large file compressor with a very high compression ratio")
    (description "lrzip is a compression utility that uses long-range
redundancy reduction to improve the subsequent compression ratio of
larger files.  It can then further compress the result with the ZPAQ or
LZMA algorithms for maximum compression, or LZO for maximum speed.  This
choice between size or speed allows for either better compression than
even LZMA can provide, or a higher speed than gzip while compressing as
well as bzip2.")
    (license (list license:gpl3+
                   license:public-domain)))) ; most files in lzma/

(define-public snappy
  (package
    (name "snappy")
    (version "1.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/google/snappy")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03zz56h79z0sgvi5sangjqn9dahhzvf645v26n1y0gwmfbmsax95"))
       (patches
        (search-patches "snappy-add-O2-flag-in-CmakeLists.txt.patch"
                        "snappy-add-inline-for-GCC.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             ;; These would be installed alongside Snappy otherwise.
             "-DBENCHMARK_ENABLE_INSTALL=OFF"
             "-DINSTALL_GTEST=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-third_party-subprojects
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (with-directory-excursion "third_party"
               (for-each (lambda (subproject)
                           (let* ((input (string-append subproject "-source"))
                                  (source (assoc-ref (or native-inputs inputs)
                                                     input)))
                             (with-directory-excursion subproject
                               ;; Take advantage of the coincidence that both
                               ;; use GIT-FETCH, which creates a directory.
                               (copy-recursively source "."))))
                         (list "benchmark"
                               "googletest"))
               #;punt))))))
    (native-inputs
     `(("benchmark-source" ,(package-source benchmark))
       ("googletest-source" ,(package-source googletest))))
    (home-page "https://github.com/google/snappy")
    (synopsis "Fast compressor/decompressor")
    (description "Snappy is a compression/decompression library.  It does not
aim for maximum compression, or compatibility with any other compression library;
instead, it aims for very high speeds and reasonable compression.  For instance,
compared to the fastest mode of zlib, Snappy is an order of magnitude faster
for most inputs, but the resulting compressed files are anywhere from 20% to
100% bigger.")
    (license license:asl2.0)
    (properties '((cpe-vendor . "google")))))

;; We need this for irods.
(define-public snappy-with-clang6
  (package
    (inherit snappy)
    (name "snappy-with-clang")
    ;; XXX 1.1.9 fails to build with clang with
    ;; error: invalid output constraint '=@ccz' in asm
    (version "1.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/snappy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j0kslq2dvxgkcxl1gakhvsa731yrcvcaipcp5k8k7ayicvkv9jv"))))
    (arguments
     `(#:configure-flags
       '("-DBUILD_SHARED_LIBS=ON"
         "-DCMAKE_CXX_COMPILER=clang++"
         "-DCMAKE_CXX_FLAGS=-stdlib=libc++"
         "-DCMAKE_EXE_LINKER_FLAGS=-lc++abi")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (let ((gcc (assoc-ref (or native-inputs inputs) "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (cons* (search-input-directory inputs
                                                       "/include/c++/v1")
                               ;; Hide GCC's C++ headers so that they do not interfere with
                               ;; the Clang headers.
                               (delete (string-append gcc "/include/c++")
                                       (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                     #\:)))
                        ":"))
               (format #true
                       "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                       (getenv "CPLUS_INCLUDE_PATH"))))))))
    (properties `((hidden? . #true)))
    (native-inputs
     `(("clang" ,clang-toolchain-6)))
    (inputs
     (list libcxx+libcxxabi-6 libcxxabi-6))))

(define-public p7zip
  (package
    (name "p7zip")
    (version "16.02")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/" name "/"
                                  version "/" name "_" version
                                  "_src_all.tar.bz2"))
              (sha256
               (base32
                "07rlwbbgszq8i7m8jh3x6j2w2hc9a72dc7fmqawnqkwlwb00mcjy"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove non-free source files
                  (for-each delete-file
                            (append
                             (find-files "CPP/7zip/Compress" "Rar.*")
                             (find-files "CPP/7zip/Crypto" "Rar.*")
                             (find-files "DOC/unRarLicense.txt")
                             (find-files  "Utils/file_Codecs_Rar_so.py")))
                  (delete-file-recursively "CPP/7zip/Archive/Rar")
                  (delete-file-recursively "CPP/7zip/Compress/Rar")
                  ;; Fix FTBFS with gcc-10.
                  (substitute* "CPP/Windows/ErrorMsg.cpp"
                    (("switch\\(errorCode\\) \\{")
                     "switch(static_cast<HRESULT>(errorCode)) {"))))
              (patches (search-patches "p7zip-CVE-2016-9296.patch"
                                       "p7zip-CVE-2017-17969.patch"
                                       "p7zip-fix-build-with-gcc-11.patch"
                                       "p7zip-remove-unused-code.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "DEST_HOME=" (assoc-ref %outputs "out")) "all3")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (copy-file
               ,(cond ((target-x86-64?)
                       "makefile.linux_amd64_asm")
                      ((target-x86-32?)
                       "makefile.linux_x86_asm_gcc_4.X")
                      (else
                        "makefile.linux_any_cpu_gcc_4.X"))
               "makefile.machine")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "test")
               (invoke "make" "test_7z")
               (invoke "make" "test_7zr")))))))
    (native-inputs
      (cond ((target-x86-64?)
             (list yasm))
            ((target-x86-32?)
             (list nasm))
            (else '())))
    (home-page "https://p7zip.sourceforge.net/")
    (synopsis "Command-line file archiver with high compression ratio")
    (description "p7zip is a command-line port of 7-Zip, a file archiver that
handles the 7z format which features very high compression ratios.")
    (license (list license:lgpl2.1+
                   license:gpl2+
                   license:public-domain))))

(define-public gzstream
  (package
    (name "gzstream")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri
                ;; No versioned URL, but last release was in 2003.
                "http://www.cs.unc.edu/Research/compgeom/gzstream/gzstream.tgz")
                (file-name (string-append name "-" version ".tgz"))
                (sha256
                 (base32
                  "00y19pqjsdj5zcrx4p9j56pl73vayfwnb7y2hvp423nx0cwv5b4r"))
                (modules '((guix build utils)))
                (snippet
                 ;; Remove pre-compiled object.
                 '(begin
                    (delete-file "gzstream.o")
                    #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         ;; Enable PIC, so it can be used in shared libraries.
         (add-after 'unpack 'use-pic
           (lambda _
             (substitute* "Makefile"
               (("CPPFLAGS = " all) (string-append all "-fPIC ")))
            #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (install-file "libgzstream.a" lib)
               (install-file "gzstream.h" include)
               #t))))))
    (propagated-inputs (list zlib))
    (home-page "http://www.cs.unc.edu/Research/compgeom/gzstream/")
    (synopsis "Compressed C++ iostream")
    (description "gzstream is a small library for providing zlib
functionality in a C++ iostream.")
    (license license:lgpl2.1+)))

(define-public zopfli
  (package
    (name "zopfli")
    (version "1.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/zopfli")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dr8n4j5nj2h9n208jns56wglw59gg4qm3s7c6y3hs75d0nnkhm4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test suite
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (home-page "https://github.com/google/zopfli")
    (synopsis "Very good, but slow, deflate or zlib compression")
    (description "Zopfli Compression Algorithm is a compression library
programmed in C to perform very good, but slow, deflate or zlib compression.
ZopfliCompress supports the deflate, gzip and zlib output formats.  This
library can only compress, not decompress; existing zlib or deflate libraries
can decompress the data.")
    (license license:asl2.0)))

(define-public zpaq
  (package
    (name "zpaq")
    (version "7.15")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "http://mattmahoney.net/dc/zpaq"
                           (string-delete #\. version) ".zip"))
       (sha256
        (base32
         "066l94yyladlfzri877nh2dhkvspagjn3m5bmv725fmhkr9c4pp8"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete irrelevant pre-compiled binaries.
        '(begin
           (for-each delete-file (find-files "." "\\.exe$"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))           ; no ‘configure’ script
       #:make-flags
       (list
        (string-append "CPPFLAGS=-Dunix"
                       ,(match (or (%current-target-system)
                                   (%current-system))
                               ("x86_64-linux"  "")
                               ("i686-linux"    "")
                               (_               " -DNOJIT")))
        ;; These should be safe, lowest-common-denominator instruction sets,
        ;; allowing for some optimisation while remaining reproducible.
        (string-append "CXXFLAGS=-O3 -DNDEBUG"
                       ,(match (or (%current-target-system)
                                   (%current-system))
                               ("x86_64-linux"  " -march=nocona -mtune=generic")
                               ("i686-linux"    " -march=i686 -mtune=generic")
                               ("armhf-linux"   " -mtune=generic-armv7-a")
                               (_               "")))
        (string-append "PREFIX="
                       (assoc-ref %outputs "out")))))
    (native-inputs
     (list perl))                 ; for pod2man
    (home-page "http://mattmahoney.net/dc/zpaq.html")
    (synopsis "Incremental journaling archiver")
    (description "ZPAQ is a command-line archiver for realistic situations with
many duplicate and already compressed files.  It backs up only those files
modified since the last update.  All previous versions remain untouched and can
be independently recovered.  Identical files are only stored once (known as
@dfn{de-duplication}).  Archives can also be encrypted.

ZPAQ is intended to back up user data, not entire operating systems.  It ignores
owner and group IDs, ACLs, extended attributes, or special file types like
devices, sockets, or named pipes.  It does not follow or restore symbolic links
or junctions, and always follows hard links.")
    (license (list license:public-domain
                   ;; libzpaq.cpp contains a mix of public-domain and
                   ;; expat-licenced (or ‘MIT’) code.
                   license:expat))))

(define-public unshield
  (package
    (name "unshield")
    (version "1.4.3")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                    (url "http://github.com/twogood/unshield")
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "19wn22vszhci8dfcixx5rliz7phx3lv5ablvhjlclvj75k2vsdqd"))))
    (build-system cmake-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("openssl" ,openssl)
       ;; Test data that is otherwise downloaded with curl.
       ("unshield-avigomanager11b22.zip"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.dropbox.com/s/8r4b6752swe3nhu/"
                 "unshield-avigomanager11b22.zip?dl=1"))
           (sha256
            (base32 "0fwq7lih04if68wpwpsk5wjqyvh32db76a41sq6gbx4dn1lc3ddn"))
           (file-name "unshield-avigomanager11b22.zip")))
       ("unshield-baldurs_gate_patch_v1_1_4315_international.zip"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.dropbox.com/s/9ruil8oi6amjbbk/"
                 "unshield-baldurs_gate_patch_v1_1_4315_international.zip?dl=1"))
           (sha256
            (base32 "0spaxf6dardlhqxz3ys09fzamj007q3nfyw4xng6gh3qp9780maj"))
           (file-name "unshield-baldurs_gate_patch_v1_1_4315_international.zip")))
       ("unshield-the-feeble-files-spanish.zip"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.dropbox.com/s/1ng0z9kfxc7eb1e/"
                 "unshield-the-feeble-files-spanish.zip?dl=1"))
           (sha256
            (base32 "1k5cw6vnpja8yjlnhx5124xrw9i8s1l539hfdqqrqz3l5gn0bnyd"))
           (file-name "unshield-the-feeble-files-spanish.zip")))))
    (native-inputs
     (list unzip))
    (arguments
     `(#:out-of-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (i)
                         (copy-file (assoc-ref inputs i)
                                    (string-append "test/v0/" i)))
                       '("unshield-avigomanager11b22.zip"
                         "unshield-baldurs_gate_patch_v1_1_4315_international.zip"
                         "unshield-the-feeble-files-spanish.zip"))
             (substitute* (find-files "test/" "/*\\.sh")
               ;; Tests expect the unshield binary in a specific
               ;; location.
               (("/var/tmp/unshield/bin/unshield")
                (string-append (getcwd) "/src/unshield"))
               ;; We no longer need to download the data.
               ((".?URL=.*$") "")
               (("curl -(|f)sSL -o test.zip .*") ""))
             (substitute* "test/v0/avigomanager.sh"
               (("test.zip")
                (string-append (getcwd)
                               "/test/v0/unshield-avigomanager11b22.zip")))
             (substitute* "test/v0/baldurs_gate_patch_v1_1_4315_international.sh"
               (("test.zip")
                (string-append
                 (getcwd)
                 "/test/v0/unshield-baldurs_gate_patch_v1_1_4315_international.zip")))
             (substitute* "test/v0/the-feeble-files-spanish.sh"
               (("test.zip")
                (string-append (getcwd)
                               "/test/v0/unshield-the-feeble-files-spanish.zip")))
             #t))
         (replace 'check
           (lambda _
             (invoke "./run-tests.sh"))))))
    (home-page "https://github.com/twogood/unshield")
    (synopsis "Extract CAB files from InstallShield installers")
    (description
     "@command{unshield} is a tool and library for extracting @file{.cab}
 archives from InstallShield installers.")
    (license license:expat)))

(define-public zstd
  (package
    (name "zstd")
    (version "1.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/facebook/zstd/releases/download/"
                           "v" version "/zstd-" version ".tar.gz"))
       (sha256
        (base32 "1h83si7s70jy7mcy0mv1c9mbkz66qqpawxs0zkmc3b1ayinf0acc"))))
    (build-system gnu-build-system)
    (outputs '("out"                    ;1.5MiB executables and documentation
               "lib"                    ;1.2MiB shared library and headers
               "static"))               ;1.2MiB static library
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-command-file-names
                 ;; Don't require hard requirements to be in $PATH.
                 (lambda _
                   (let* ((our (lambda (name) (string-append #$output "/bin/" name))))
                     (substitute* "programs/zstdgrep"
                       (("(:-)(grep)" _ prefix command)
                        (string-append prefix (which command)))
                       (("(:-)(zstdcat)" _ prefix command)
                        (string-append prefix (our command))))
                     (substitute* "programs/zstdless"
                       (("zstdcat" command)
                        (our command))))))
               (delete 'configure)            ;no configure script
               (add-after 'install 'adjust-library-locations
                 (lambda _
                   (let* ((out #$output)
                          (lib #$output:lib)
                          (static #$output:static)
                          (shared-libs (string-append lib "/lib"))
                          (static-libs (string-append static "/lib")))
                     (mkdir-p static-libs)
                     ;; This is based on the win64 release zip file from zstd.
                     #$@(if (target-mingw?)
                            #~((for-each delete-file (find-files out "\\.so"))
                               (for-each delete-file (find-files shared-libs "\\.so"))
                               (rename-file (string-append shared-libs "/libzstd.a")
                                            (string-append static-libs "/libzstd_static.lib"))
                               (delete-file-recursively
                                (string-append shared-libs "/pkgconfig"))
                               ;; no binary for interpreter `sh' found in $PATH
                               (delete-file (string-append out "/bin/zstdgrep"))
                               (delete-file (string-append out "/bin/zstdless"))
                               (delete-file (string-append out "/share/man/man1/zstdgrep.1"))
                               (delete-file (string-append out "/share/man/man1/zstdless.1")))
                            #~(;; Move the static library to its own output to save ~1MiB.
                               (for-each (lambda (ar)
                                           (link ar (string-append static-libs "/"
                                                                   (basename ar)))
                                           (delete-file ar))
                                         (find-files shared-libs "\\.a$"))

                               ;; Make sure the pkg-config file refers to the right output.
                               (substitute* (string-append shared-libs "/pkgconfig/libzstd.pc")
                                 (("^prefix=.*")
                                  ;; Note: The .pc file expects a trailing slash for 'prefix'.
                                  (string-append "prefix=" lib "/\n")))))))))
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "prefix=" #$output)
                   (string-append "libdir=" #$output:lib "/lib")
                   (string-append "includedir=" #$output:lib "/include")
                   #$@(if (target-mingw?)
                          `(;; See the note in the Makefile.
                            "TARGET_SYSTEM=Windows"
                            ;; Don't try to link with pthread.
                            "THREAD_LD="
                            ;; This isn't picked up correctly in the Makefiles.
                            "EXT=.exe")
                          '())
                   ;; Auto-detection is over-engineered and buggy.
                   "PCLIBDIR=lib"
                   "PCINCDIR=include"
                   ;; Skip auto-detection of, and creating a dependency on, the build
                   ;; environment's ‘xz’ for what amounts to a dubious feature anyway.
                   "HAVE_LZMA=0"
                   ;; Not currently detected, but be explicit & avoid surprises later.
                   "HAVE_LZ4=0"
                   "HAVE_ZLIB=0")
           #:tests? (not (or (target-hurd?)
                             (%current-target-system)))))
    (home-page "https://facebook.github.io/zstd/")
    (synopsis "Zstandard real-time compression algorithm")
    (description "Zstandard (@command{zstd}) is a lossless compression algorithm
that combines very fast operation with a compression ratio comparable to that of
zlib.  In most scenarios, both compression and decompression can be performed in
‘real time’.  The compressor can be configured to provide the most suitable
trade-off between compression ratio and speed, without affecting decompression
speed.")
    (license (list license:bsd-3         ; the main top-level LICENSE file
                   license:bsd-2         ; many files explicitly state 2-Clause
                   license:gpl2          ; the main top-level COPYING file
                   license:gpl3+         ; tests/gzip/*.sh
                   license:expat         ; lib/dictBuilder/divsufsort.[ch]
                   license:public-domain ; zlibWrapper/examples/fitblk*
                   license:zlib))))      ; zlibWrapper/{gz*.c,gzguts.h}

(define-public zstd-1.5.7
  (package
    (inherit zstd)
    ;; Don't hide this package from the UI.
    (properties '())
    (version "1.5.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/facebook/zstd/releases/download/"
                           "v" version "/zstd-" version ".tar.gz"))
       (sha256
        (base32 "18vgkvh7w6zw4jn2aj1mp0yv98m4fk52ay6da0wh4pm194gyaczb"))))))

(define-public pzstd
  (package/inherit zstd-1.5.7
    (name "pzstd")
    (outputs '("out"))
    (inputs
     `(,@(if (%current-target-system)
             `(("googletest" ,googletest))
             '())))
    (native-inputs
     `(,@(if (%current-system)
             `(("googletest" ,googletest))
             '())))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-subdirectory
           (lambda _ (chdir "contrib/pzstd") #t))
         (delete 'configure)            ; no configure script
         (add-before 'check 'compile-tests
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "tests" make-flags)))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name)))
               (mkdir-p doc)
               (install-file "README.md" doc)
               #t))))
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "CXX=" ,(cxx-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (home-page (package-home-page zstd))
    (synopsis "Threaded implementation of the Zstandard compression algorithm")
    (description "Parallel Zstandard (PZstandard or @command{pzstd}) is a
multi-threaded implementation of the @uref{http://zstd.net/, Zstandard
compression algorithm}.  It is fully compatible with the original Zstandard file
format and command-line interface, and can be used as a drop-in replacement.

Compression is distributed over multiple processor cores to improve performance,
as is the decompression of data compressed in this manner.  Data compressed by
other implementations will only be decompressed by two threads: one performing
the actual decompression, the other input and output.")
    (license (package-license zstd))))

(define-public zip
  (package
    (name "zip")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/infozip"
                           "/Zip%203.x%20%28latest%29/3.0/zip30.tar.gz"))
       (sha256
        (base32
         "0sb3h3067pzf3a7mlxn1hikpcjrsvycjcnj9hl9b1c3ykcgvps7h"))))
    (build-system gnu-build-system)
    (inputs (list bzip2))
    (arguments
     `(#:tests? #f ; no test target
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "-f" "unix/Makefile"
                            "CC=gcc -Wno-error=implicit-function-declaration"
                            (string-append "prefix=" out)
                            (string-append "MANDIR=" out "/share/man/man1")))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "generic" make-flags)))
         (delete 'configure))))
    (home-page "http://www.info-zip.org/Zip.html")
    (synopsis "Compression and file packing utility")
    (description
     "Zip is a compression and file packaging/archive utility.  Zip is useful
for packaging a set of files for distribution, for archiving files, and for
saving disk space by temporarily compressing unused files or directories.
Zip puts one or more compressed files into a single ZIP archive, along with
information about the files (name, path, date, time of last modification,
protection, and check information to verify file integrity).  An entire
directory structure can be packed into a ZIP archive with a single command.

Zip has one compression method (deflation) and can also store files without
compression.  Zip automatically chooses the better of the two for each file.
Compression ratios of 2:1 to 3:1 are common for text files.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))

(define-public unzip
  (package (inherit zip)
    (name "unzip")
    (version "6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/infozip"
                           "/UnZip%206.x%20%28latest%29/UnZip%206.0/unzip60.tar.gz"))
       (sha256
        (base32
         "0dxx11knh3nk95p2gg2ak777dd11pr7jx5das2g49l262scrcv83"))
       (patches (search-patches "unzip-CVE-2014-8139.patch"
                                "unzip-CVE-2014-8140.patch"
                                "unzip-CVE-2014-8141.patch"
                                "unzip-CVE-2014-9636.patch"
                                "unzip-CVE-2015-7696.patch"
                                "unzip-CVE-2015-7697.patch"
                                "unzip-allow-greater-hostver-values.patch"
                                "unzip-initialize-symlink-flag.patch"
                                "unzip-remove-build-date.patch"
                                "unzip-attribs-overflow.patch"
                                "unzip-overflow-on-invalid-input.patch"
                                "unzip-format-secure.patch"
                                "unzip-overflow-long-fsize.patch"

                                ;; From Fedora
                                "unzip-alt-iconv-utf8.patch"
                                "unzip-alt-iconv-utf8-print.patch"
                                "unzip-fix-recmatch.patch"
                                "unzip-case-insensitive.patch"
                                "unzip-close.patch"
                                "unzip-COVSCAN-fix-unterminated-string.patch"
                                "unzip-CVE-2016-9844.patch"
                                "unzip-CVE-2018-1000035.patch"
                                "unzip-CVE-2018-18384.patch"
                                "unzip-exec-shield.patch"
                                "unzip-manpage-fix.patch"
                                "unzip-overflow.patch"
                                "unzip-timestamp.patch"
                                "unzip-valgrind.patch"
                                "unzip-x-option.patch"
                                ;; CVE-2019-13232
                                "unzip-zipbomb-manpage.patch"
                                "unzip-zipbomb-part1.patch"
                                "unzip-zipbomb-part2.patch"
                                "unzip-zipbomb-part3.patch"

                                ;; https://github.com/madler/unzip/issues/2
                                "unzip-32bit-zipbomb-fix.patch"

                                "unzip-CVE-2022-0529+CVE-2022-0530.patch"))))
    (build-system gnu-build-system)
    ;; no inputs; bzip2 is not supported, since not compiled with BZ_NO_STDIO
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'fortify
                 (lambda _
                   ;; Mitigate CVE-2018-1000035, an exploitable buffer overflow.
                   ;; This environment variable is recommended in 'unix/Makefile'
                   ;; for passing flags to the C compiler.
                   (setenv "LOCAL_UNZIP" "-D_FORTIFY_SOURCE=1")
                   #t))
               (replace 'build
                 (lambda* (#:key make-flags #:allow-other-keys)
                   (apply invoke "make"
                          `("-j" ,(number->string
                                   (parallel-job-count))
                            ,@make-flags
                            "generic")))))
           #:make-flags
           ;; Fix cross-compilation without affecting native builds, as doing so
           ;; would trigger too many rebuilds: https://issues.guix.gnu.org/57127
           (if (%current-target-system)
               #~(list "-f" "unix/Makefile"
                       "CC=gcc -Wno-error=implicit-function-declaration"
                       (string-append "prefix=" #$output)
                       (string-append "MANDIR=" #$output "/share/man/man1"))
               #~(list "-f" "unix/Makefile"
                       "CC=gcc -Wno-error=implicit-function-declaration"
                       (string-append "prefix=" %output)
                       (string-append "MANDIR=" %output "/share/man/man1")))))
    (home-page "http://www.info-zip.org/UnZip.html")
    (synopsis "Decompression and file extraction utility")
    (description
     "UnZip is an extraction utility for archives compressed in .zip format,
also called \"zipfiles\".

UnZip lists, tests, or extracts files from a .zip archive.  The default
behaviour (with no options) is to extract into the current directory, and
subdirectories below it, all files from the specified zipfile.  UnZip
recreates the stored directory structure by default.")
    (properties `((lint-hidden-cve . ("CVE-2019-13232"))))
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))

(define-public ziptime
  (let ((commit "2a5bc9dfbf7c6a80e5f7cb4dd05b4036741478bc")
        (revision "0"))
  (package
    (name "ziptime")
    (version (git-version "0.0.0" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://android.googlesource.com/platform/build")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hrn61b3a97dlc4iqc28rwx8k8zf7ycbwzqqp93vj34zy5a541kn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "tools/ziptime")))
         (delete 'configure)            ; nothing to configure
         (replace 'build
           ;; There is no Makefile, only an ‘Android.bp’ file.  Ignore it.
           (lambda _
             (let ((c++ ,(cxx-for-target)))
               (apply invoke c++ "-O2" "-o" "ziptime"
                      (find-files "." "\\.cpp$")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "ziptime" bin)
               (install-file "README.txt" doc)))))))
    ;; There is no separate home page for this tiny bundled build tool.
    (home-page (string-append "https://android.googlesource.com/platform/build/"
                              "+/master/tools/ziptime/README.txt"))
    (synopsis "Normalize @file{.zip} archive header timestamps")
    (description
     "Ziptime helps make @file{.zip} archives reproducible by replacing
timestamps in the file header with a fixed time (1 January 2008).

``Extra fields'' are not changed, so you'll need to use the @code{-X} option to
@command{zip} to prevent it from storing the ``universal time'' field.")
    (license license:asl2.0))))

(define-public zziplib
  (package
    (name "zziplib")
    (version "0.13.78")
    (home-page "https://github.com/gdraheim/zziplib")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18578xbzj8j89srv4bwayjm11bg56fl34sya0znq4fwq3apm037i"))))
    (build-system cmake-build-system)
    (inputs
     (list zlib))
    (native-inputs (list perl ; for the documentation
                         pkg-config python zip)) ; to create test files
    (synopsis "Library for accessing zip files")
    (description
     "ZZipLib is a library based on zlib for accessing zip files.")
    ;; zziplib is dual licensed under LGPL2.0+ and MPL1.1.  Some example source
    ;; files carry the Zlib license; see "docs/copying.html" for details.
    (license (list license:lgpl2.0+ license:mpl1.1))))

(define-public libzip
  (package
    (name "libzip")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://libzip.org/download/libzip-" version ".tar.xz"))
              (sha256
               (base32
                "0dsrpb1faywhm0j8akx21gp7cn99wpz3h543jw8r7p5jnx99hgn9"))))
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list gnutls xz openssl zlib
           `(,zstd "lib")))
    (build-system cmake-build-system)
    (home-page "https://libzip.org")
    (synopsis "C library for reading, creating, and modifying zip archives")
    (description "Libzip is a C library for reading, creating, and modifying
zip archives.  Files can be added from data buffers, files, or compressed data
copied directly from other zip archives.  Changes made without closing the
archive can be reverted.")
    (license license:bsd-3)))

(define-public atool
  (package
    (name "atool")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/atool/atool-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0fvhzip2v08jgnlfpyj6rapan39xlsl1ksgq4lp8gfsai2ah1xma"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'embed-absolute-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "atool"
               (("(^\\$::cfg_path_file.*= )'file'" _ pre)
                (string-append pre "'" (assoc-ref inputs "file")
                               "/bin/file'")))
             #t)))))
    (inputs
     (list perl file))
    (home-page "https://www.nongnu.org/atool/")
    (synopsis  "Universal tool to manage file archives of various types")
    (description "The main command is @command{aunpack} which extracts files
from an archive.  The other commands provided are @command{apack} (to create
archives), @command{als} (to list files in archives), and @command{acat} (to
extract files to standard out).  As @command{atool} invokes external programs
to handle the archives, not all commands may be supported for a certain type
of archives.")
    (license license:gpl2+)))

(define-public lunzip
  (package
    (name "lunzip")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/lzip/lunzip/"
                           "lunzip-" version ".tar.gz"))
       (sha256
        (base32 "153qa674rlbw812fb7h8rfzdw4hvr6vgkjl1c0yfplj7p4h86z9w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list ,(string-append "CC=" (cc-for-target)))))
    (home-page "https://www.nongnu.org/lzip/lunzip.html")
    (synopsis "Small, stand-alone lzip decompressor")
    (description
     "Lunzip is a decompressor for files in the lzip compression format (.lz),
written as a single small C tool with no dependencies.  This makes it
well-suited to embedded and other systems without a C++ compiler, or for use in
applications such as software installers that need only to decompress files,
not compress them.
Lunzip is intended to be fully compatible with the regular lzip package.")
    (license (list license:bsd-2        ; carg_parser.[ch]
                   license:gpl2+))))    ; everything else

(define-public clzip
  (package
    (name "clzip")
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/lzip/clzip/"
                           "clzip-" version ".tar.gz"))
       (sha256
        (base32 "0wrd2s5lqxd3mjqby9isc6mpk4vyd2wbksbryrzy0bw3bcjf2gzn"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "CC=" #$(cc-for-target)))))
    (home-page "https://www.nongnu.org/lzip/clzip.html")
    (synopsis "Small, stand-alone lzip compressor and decompressor")
    (description
     "Clzip is a compressor and decompressor for files in the lzip compression
format (.lz), written as a single small C tool with no dependencies.  This makes
it well-suited to embedded and other systems without a C++ compiler, or for use
in other applications like package managers.
Clzip is intended to be fully compatible with the regular lzip package.")
    (license (list license:bsd-2        ; carg_parser.[ch], lzd in clzip.texi
                   license:gpl2+))))

(define-public lzlib
  (package
    (name "lzlib")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/lzip/lzlib/"
                           "lzlib-" version ".tar.gz"))
       (sha256
        (base32 "107vkzfgvb21cqq4gmxyfn97l4s2c0b3i14k9rkv594b2krmiax1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "CC=" ,(cc-for-target))
             "--disable-static"
             "--enable-shared")))       ; only static (.a) is built by default
    (home-page "https://www.nongnu.org/lzip/lzlib.html")
    (synopsis "Lzip data compression C library")
    (description
     "Lzlib is a C library for in-memory LZMA compression and decompression in
the lzip format.  It supports integrity checking of the decompressed data, and
all functions are thread-safe.  The library should never crash, even in case of
corrupted input.")
    (license (list license:bsd-2        ; the library itself
                   license:gpl2+))))    ; main.c (i.e. minilzip used by tests)

(define-public plzip
  (package
    (name "plzip")
    (version "1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/lzip/plzip/"
                           "plzip-" version ".tar.gz"))
       (sha256
        (base32 "0sv2sp0ppxkdds4qiwb3ddvlv22ydv61n3hk74rkh8ljbkjcpqyq"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "CXX=" #$(cxx-for-target)))))
    (inputs
     (list lzlib))
    (home-page "https://www.nongnu.org/lzip/plzip.html")
    (synopsis "Parallel lossless data compressor for the lzip format")
    (description
     "Plzip is a massively parallel (multi-threaded) lossless data compressor
and decompressor that uses the lzip file format (.lz).  Files produced by plzip
are fully compatible with lzip and can be rescued with lziprecover.
On multiprocessor machines, plzip can compress and decompress large files much
faster than lzip, at the cost of a slightly reduced compression ratio (0.4% to
2%).  The number of usable threads is limited by file size: on files of only a
few MiB, plzip is no faster than lzip.
Files that were compressed with regular lzip will also not be decompressed
faster by plzip, unless the @code{-b} option was used: lzip usually produces
single-member files which can't be decompressed in parallel.")
    (license (list license:bsd-2        ; arg_parser.{cc,h}
                   license:gpl2+))))    ; everything else

(define-public innoextract
  (package
   (name "innoextract")
   (version "1.9")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://constexpr.org/innoextract/files/"
                         "innoextract-" version "/"
                         "/innoextract-" version ".tar.gz"))
     (sha256
      (base32 "09l1z1nbl6ijqqwszdwch9mqr54qb7df0wp2sd77v17dq6gsci33"))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f))
   (inputs (list boost xz))
   (native-inputs (list pkg-config))
   (home-page "https://constexpr.org/innoextract/")
   (synopsis "Tool for extracting Inno Setup installers")
   (description "innoextract allows extracting Inno Setup installers under
non-Windows systems without running the actual installer using wine.")
   (license license:zlib)))

(define-public isa-l
  (package
   (name "isa-l")
   (version "2.30.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/intel/isa-l")
           ;; Corresponds to tag v2.30.0
           (commit "2df39cf5f1b9ccaa2973f6ef273857e4dc46f0cf")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "06ymkrf3hkkd94i59ahm79545rk709y8rd0v2l86w38z6is942q0"))))
   (build-system gnu-build-system)
   (native-inputs (list autoconf automake libtool nasm))
   (home-page "https://github.com/intel/isa-l")
   (synopsis "Intelligent storage acceleration library")
   (description "ISA-L is a collection of optimized low-level functions
targeting storage applications.  ISA-L includes:

@itemize
@item Erasure codes: fast block Reed-Solomon type erasure codes for any
  encode/decode matrix;
@item CRC: fast implementations of cyclic redundancy check.  Six different
  polynomials supported: iscsi32, ieee32, t10dif, ecma64, iso64, jones64;
@item Raid: calculate and operate on XOR and P+Q parity found in common RAID
  implementations;
@item Compression: fast deflate-compatible data compression;
@item De-compression: fast inflate-compatible data compression;
@item igzip: command line application like gzip, accelerated with ISA-L.
@end itemize
")
   (license license:bsd-3)))

(define-public brotli
  (package
    (name "brotli")
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
        '(begin
           ;; Cherry-picked from upstream since the latest release
           ;; https://github.com/google/brotli/commit/09b0992b6acb7faa6fd3b23f9bc036ea117230fc
           (substitute* (find-files "scripts" "^lib.*pc\\.in")
             (("-R\\$\\{libdir\\} ") ""))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'rename-static-libraries
           ;; The build tools put a 'static' suffix on the static libraries, but
           ;; other applications don't know how to find these.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref %outputs "out") "/lib/")))
               (rename-file (string-append lib "libbrotlicommon-static.a")
                            (string-append lib "libbrotlicommon.a"))
               (rename-file (string-append lib "libbrotlidec-static.a")
                            (string-append lib "libbrotlidec.a"))
               (rename-file (string-append lib "libbrotlienc-static.a")
                            (string-append lib "libbrotlienc.a"))
               #t))))
       #:configure-flags
       (list ;; Defaults to "lib64" on 64-bit archs.
             (string-append "-DCMAKE_INSTALL_LIBDIR="
                            (assoc-ref %outputs "out") "/lib"))))
    (home-page "https://github.com/google/brotli")
    (synopsis "General-purpose lossless compression")
    (description "This package provides the reference implementation of Brotli,
a generic-purpose lossless compression algorithm that compresses data using a
combination of a modern variant of the LZ77 algorithm, Huffman coding and 2nd
order context modeling, with a compression ratio comparable to the best
currently available general-purpose compression methods.  It is similar in speed
with @code{deflate} but offers more dense compression.

The specification of the Brotli Compressed Data Format is defined in RFC 7932.")
    (license license:expat)))

(define-public google-brotli
  (deprecated-package "google-brotli" brotli))

(define-public python-brotli
  (package
    (inherit brotli)
    (name "python-brotli")
    (build-system python-build-system)
    (arguments '())
    (synopsis "Python interface to Brotli")
    (description "This package provides a Python interface to the @code{brotli}
package, an implementation of the Brotli lossless compression algorithm.")))

(define-public python-google-brotli
  (deprecated-package "python-google-brotli" python-brotli))

(define-public java-brotli
  (package
    (inherit brotli)
    (name "java-brotli")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-brotli.jar"
       #:source-dir "java"
       ;; Tests are mixed in with java sources, the ant build system
       ;; doesn't allow that
       #:tests? #f))
    (native-inputs (list java-junit))))

(define-public ucl
  (package
    (name "ucl")
    (version "1.03")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.oberhumer.com/opensource/"
                                 name "/download/" name "-" version ".tar.gz"))
             (sha256
              (base32
               "0j036lkwsxvm15gr29n8wn07cqq79dswjs9k54939ms5zngjjrdq"))))
    (build-system gnu-build-system)
    (arguments
     `(;; UCL 1.03 fails to build with newer C standards.
       #:configure-flags '("CFLAGS=-std=gnu90"
                           "--enable-shared" "--disable-static")))
    (home-page "https://www.oberhumer.com/opensource/ucl/")
    (synopsis "Portable lossless data compression library")
    (description "UCL implements a number of compression algorithms that
achieve an excellent compression ratio while allowing fast decompression.
Decompression requires no additional memory.

Compared to LZO, the UCL algorithms achieve a better compression ratio but
decompression is a little bit slower.")
    (license license:gpl2+)))

(define-public upx
  (package
    (name "upx")
    (version "4.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/upx/upx/releases/download/v"
                           version "/upx-" version "-src.tar.xz"))
       (sha256
        (base32 "1i71p03861hlf5x1w217l67zm5inm449zhbg6kpv8zyj0wb5dmjy"))))
    (build-system cmake-build-system)
    (home-page "https://upx.github.io/")
    (synopsis "Compression tool for executables")
    (description
     "The Ultimate Packer for eXecutables (UPX) is an executable file
compressor.  UPX typically reduces the file size of programs and shared
libraries by around 50%--70%, thus reducing disk space, network load times,
download times, and other distribution and storage costs.")
    ;; These CVEs have been fixed since 4.0.2 but are still linted.
    (properties `((lint-hidden-cve . ("CVE-2023-23456" "CVE-2023-23457"))))
    (license license:gpl2+)))

(define-public quazip
  (package
    (name "quazip")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stachenov/quazip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jsw4xm5wyaqcj1pma5zzd8f5xbgd5lcjh18ah3kg36xz5i69yi4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no test
    (native-inputs
     (list doxygen))
    (inputs
     (list qtbase qt5compat zlib))
    (home-page "https://stachenov.github.io/quazip/index.html")
    (synopsis "Qt/C++ wrapper for Minizip")
    (description "QuaZIP is a simple C++ wrapper over Gilles Vollant's
ZIP/UNZIP package that can be used to access ZIP archives.  It uses
Trolltech's Qt toolkit.

QuaZIP allows you to access files inside ZIP archives using QIODevice
API, and that means that you can also use QTextStream, QDataStream or
whatever you would like to use on your zipped files.

QuaZIP provides complete abstraction of the ZIP/UNZIP API, for both
reading from and writing to ZIP archives.")
    ;; Project is distributed under LGPL, but "quazip/z*" "quazip/unzip.*" are
    ;; distributed under zlib terms.
    (license (list license:lgpl2.1+ license:zlib))))

(define-public quazip-5
  (package
    (inherit quazip)
    (name "quazip-qt5")
    (version "1.4")
    (inputs (list qtbase-5 zlib))))

(define-public zchunk
  (package
    (name "zchunk")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zchunk/zchunk")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08ngp6d54fllk19qhdz6r1s9832781g322gn7524akbr3v1v5jjz"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-file-name
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/zck_gen_zdict.c"
                (("/usr/(bin/zstd)" _ file)
                 (string-append (search-input-file inputs file)))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl zstd))
    (propagated-inputs
     (list `(,zstd "lib")))             ; in Requires.private of zck.pc
    (home-page "https://github.com/zchunk/zchunk")
    (synopsis "Compressed file format for efficient deltas")
    (description "The zchunk compressed file format allows splitting a file
into independent chunks.  This makes it possible to retrieve only changed
chunks when downloading a new version of the file, and also makes zchunk files
efficient over rsync.  Along with the library, this package provides the
following utilities:
@table @command
@item unzck
To decompress a zchunk file.
@item zck
To compress a new zchunk file, or re-compress an existing one.
@item zck_delta_size
To calculate the difference between two zchunk files.
@item zck_gen_zdict
To create a dictionary for a zchunk file.
@item zck_read_header
To read a zchunk header.
@item zckdl
To download a zchunk file.
@end table")
    (license license:bsd-2)))

(define-public zutils
  (package
    (name "zutils")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/zutils/zutils-" version ".tar.lz"))
       (sha256
        (base32 "1vl8mhvsl0zlh34hwhc05vj33a2xfr0w7i978hcwaw8wn1w59bkq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--sysconfdir=/etc")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "install"
                     (string-append "sysconfdir=" #$output "/etc")
                     make-flags))))))
    (native-inputs
     ;; Needed to extract the source tarball and run the test suite.
     (list lzip))
    (home-page "https://www.nongnu.org/zutils/zutils.html")
    (synopsis "Utilities that transparently operate on compressed files")
    (description
     "Zutils is a collection of utilities able to process any combination of
compressed and uncompressed files transparently.  If any given file, including
standard input, is compressed, its decompressed content is used instead.

@command{zcat}, @command{zcmp}, @command{zdiff}, and @command{zgrep} are
improved replacements for the shell scripts provided by GNU gzip.
@command{ztest} tests the integrity of supported compressed files.
@command{zupdate} recompresses files with lzip, similar to gzip's
@command{znew}.

Supported compression formats are bzip2, gzip, lzip, and xz.  Zutils uses
external compressors: the compressor to be used for each format is configurable
at run time, and must be installed separately.")
    (license (list license:bsd-2        ; arg_parser.{cc,h}
                   license:gpl2+))))    ; the rest

(define-public makeself-safeextract
  (let ((commit "1a95e121fa8e3c02d307ae37b9b7834e616c3683"))
    (package
      (name "makeself-safeextract")
      (version (git-version "0.0.0" "1" commit))
      (home-page "https://github.com/ssokolow/makeself_safeextract")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1anlinaj9lvfi8bn00wp11vzqq0f9sig4fm9yrspisx31v0z4a2c"))))
      (build-system trivial-build-system)
      (inputs
       `(("python" ,python-2)
         ("p7zip" ,p7zip)
         ("unzip" ,unzip)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((name "makeself_safeextract")
                  (source (string-append (assoc-ref %build-inputs "source")
                                         "/" name ".py"))
                  (bin (string-append (assoc-ref %outputs "out") "/bin"))
                  (target (string-append bin "/" name))
                  (python (string-append (assoc-ref %build-inputs "python") "/bin"))
                  (7z (search-input-file %build-inputs "/bin/7z"))
                  (unzip (search-input-file %build-inputs "/bin/unzip")))
             (setenv "PATH" (string-append (getenv "PATH") ":" python))
             (mkdir-p bin)
             (copy-file source target)
             (substitute* target
               (("'7z'") (format #f "'~a'" 7z))
               (("'unzip'") (format #f "'~a'" unzip)))
             (patch-shebang target)))))
      (synopsis "Extract makeself and mojo archives without running untrusted code")
      (description "This package provides a script to unpack self-extracting
archives generated by @command{makeself} or @command{mojo} without running the
possibly untrusted extraction shell script.")
      (license license:gpl3+))))

(define-public ncompress
  (package
    (name "ncompress")
    (version "5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vapier/ncompress")
                    (commit (string-append "v" version))))
              (patches (search-patches "ncompress-fix-softlinks.patch"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "090kksxrlqnsdc76fzz2j2ajc98mhmfwyn163ca2ia9niqmlpcm0"))))
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "BINDIR=" %output "/bin")
                          (string-append "MANDIR=" %output "/share/man/man1"))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)))) ; no configure script
    (build-system gnu-build-system)
    (home-page "https://github.com/vapier/ncompress/")
    (synopsis "Original Lempel-Ziv compress/uncompress programs")
    (description "(N)compress provides the original compress and uncompress
programs that used to be the de facto UNIX standard for compressing and
uncompressing files.  These programs implement a fast, simple Lempel-Ziv (LZW)
file compression algorithm.")
    (license license:gpl2+)))

(define-public xarchiver
  (package
    (name "xarchiver")
    (version "0.5.4.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ib/xarchiver")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ccxp106c3r759l1bjaszz3fwlw10qzzlsd9mnmpksnlwfwjkmb8"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list gettext-minimal intltool libxslt pkg-config))
    (inputs
     (list adwaita-icon-theme ; hard-coded theme
           gtk+))
    (home-page "https://github.com/ib/xarchiver")
    (synopsis "Graphical front-end for archive operations")
    (description "Xarchiver is a front-end to various command line archiving
tools.  It uses GTK+ tool-kit and is designed to be desktop-environment
independent.  Supported formats are 7z, ARJ, bzip2, gzip, LHA, lzma, lzop,
RAR, RPM, DEB, tar, and ZIP.  It cannot perform functions for archives, whose
archiver is not installed.")
    (license license:gpl2+)))

(define-public tarsplitter
  (package
    (name "tarsplitter")
    (version "2.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AQUAOSOTech/tarsplitter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17qkg95r97kcrs17b0mcqswx99280ni47j5yx8xa7nl3bdhm6325"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/AQUAOSOTech/tarsplitter"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-documentation
           (lambda* (#:key import-path outputs #:allow-other-keys)
             (let* ((source (string-append "src/" import-path))
                    (out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (with-directory-excursion source
                 (install-file "README.md" doc))
               #t))))))
    (home-page "https://github.com/AQUAOSOTech/tarsplitter")
    (synopsis "Multithreaded tar utility")
    (description
     "Archive huge numbers of files, or split massive tar archives into smaller
chunks.")
    (license license:expat)))

(define-public c-blosc
  (package
    (name "c-blosc")
    (version "1.21.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Blosc/c-blosc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gy9a7wp7x71x5l3rprx8wpb3c5cn7wqc77gdiffq35hr34q88p9"))
       (modules '((guix build utils)))
       (snippet
        ;; In a rare victory, we may delete all bundled libs to no ill effect.
        '(delete-file-recursively "internal-complibs"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DBUILD_STATIC=OFF"
         "-DDEACTIVATE_AVX2=ON"
         "-DPREFER_EXTERNAL_LZ4=ON"
         "-DPREFER_EXTERNAL_ZLIB=ON"
         "-DPREFER_EXTERNAL_ZSTD=ON")))
    (inputs
     (list lz4 snappy zlib `(,zstd "lib")))
    (home-page "https://www.blosc.org/")
    (synopsis "Blocking, shuffling and lossless compression library")
    (description
     "Blosc is a high performance compressor optimized for binary data.  It
has been designed to transmit data to the processor cache faster than the
traditional, non-compressed, direct memory fetch approach via a
@code{memcpy()} system call.  Blosc is meant not only to reduce the size of
large datasets on-disk or in-memory, but also to accelerate memory-bound
computations.")
    ;; Blosc itself is released under BSD-3 but it incorporates code under
    ;; other non-copyleft licenses.
    (license license:bsd-3)))

(define-public c-blosc2
  (package
    (name "c-blosc2")
    (version "2.15.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Blosc/c-blosc2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17kqwvw2n6bgzidi8f5906s5hc9wm1lbfbpd491gf7csxjck99sx"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_STATIC=OFF"
                                     "-DDEACTIVATE_AVX2=ON"
                                     "-DDEACTIVATE_AVX512=ON"
                                     "-DPREFER_EXTERNAL_LZ4=ON"
                                     "-DPREFER_EXTERNAL_ZLIB=ON"
                                     "-DPREFER_EXTERNAL_ZSTD=ON")))
    (inputs (list lz4 zlib `(,zstd "lib")))
    (home-page "https://blosc.org")
    (synopsis "Blocking, shuffling and lossless compression library")
    (description
     "Blosc is a high performance compressor optimized for binary
data (i.e. floating point numbers, integers and booleans, although it can
handle string data too).  It has been designed to transmit data to the
processor cache faster than the traditional, non-compressed, direct memory
fetch approach via a @code{memcpy()} system call.  Blosc main goal is not just
to reduce the size of large datasets on-disk or in-memory, but also to
accelerate memory-bound computations.

C-Blosc2 is the new major version of C-Blosc, and is backward compatible with
both the C-Blosc1 API and its in-memory format.  However, the reverse thing is
generally not true for the format; buffers generated with C-Blosc2 are not
format-compatible with C-Blosc1 (i.e. forward compatibility is not
supported).")
    (license license:bsd-3)))

(define-public ecm
  (package
    (name "ecm")
    (version "1.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alucryd/ecm-tools")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rvyx5gcy8lfklgj80szlz3312x45wzx0d9jsgwyvy8f6m4nnb0c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "Makefile"
               (("\\$\\(DESTDIR\\)/usr") "$(DESTDIR)"))
             #t)))))
    (home-page "https://github.com/alucryd/ecm-tools")
    (synopsis "Error code modeler")
    (description "ECM is a utility that converts ECM files, i.e., CD data files
with their error correction data losslessly rearranged for better compression,
to their original, binary CD format.")
    (license license:gpl3+)))

(define-public libdeflate
  (package
    (name "libdeflate")
    (version "1.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ebiggers/libdeflate")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "033d8jcaissiy3mk07wx6dap17c1xl768xy75righ22ggmm7c1hy"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DLIBDEFLATE_BUILD_STATIC_LIB=NO"
                   "-DLIBDEFLATE_BUILD_TESTS=YES")))
    (inputs
     (list zlib))
    (home-page "https://github.com/ebiggers/libdeflate")
    (synopsis "Library for DEFLATE/zlib/gzip compression and decompression")
    (description "Libdeflate is a library for fast, whole-buffer DEFLATE-based
compression and decompression.  The supported formats are:

@enumerate
@item DEFLATE (raw)
@item zlib (a.k.a. DEFLATE with a zlib wrapper)
@item gzip (a.k.a. DEFLATE with a gzip wrapper)
@end enumerate
")
    (license license:expat)))

(define-public tarlz
  (package
    (name "tarlz")
    (version "0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/lzip/tarlz/"
                           "tarlz-" version ".tar.lz"))
       (sha256
        (base32 "1x5dw03lcwfigcv97cg70gkbkfycjmv1012s9lwnl4izvl9235qg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list lzip))
    (inputs
     (list lzlib))
    (home-page "https://www.nongnu.org/lzip/tarlz.html")
    (synopsis "Combination of the tar archiver and the lzip compressor")
    (description
     "Tarlz is a massively parallel (multi-threaded) combined implementation of
the tar archiver and the lzip compressor.  Tarlz creates, lists, and extracts
archives in a simplified and safer variant of the POSIX pax format compressed
with lzip, keeping the alignment between tar members and lzip members.  The
resulting multimember tar.lz archive is fully backward compatible with standard
tar tools like GNU tar, which treat it like any other tar.lz archive.  Tarlz
can append files to the end of such compressed archives.")
    (license license:gpl2+)))

(define-public libcbor
  (package
    (name "libcbor")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PJK/libcbor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
          "00ibddiy62kbs9wl52c35j0hbqanx6pi7lvzkpzmbsizkj8mhp1p"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (lib (string-append out "/lib")))
         (list
          "-DCMAKE_BUILD_TYPE=Release"
          "-DBUILD_SHARED_LIBS=ON"
          "-DCBOR_CUSTOM_ALLOC=ON"
          (string-append "-DCMAKE_INSTALL_LIBDIR=" lib)
          (string-append "-DCMAKE_INSTALL_RPATH=" lib)))))
    (synopsis "The C library for parsing and generating CBOR")
    (description
     "@acronym{CBOR, The Concise Binary Object Representation} is a data format
whose design goals include the possibility of extremely small code size, fairly
small message size, and extensibility without the need for version
negotiation.  These design goals make it different from earlier binary
serializations such as ASN.1 and MessagePack.")
    (license license:expat)
    (home-page "https://github.com/PJK/libcbor")))

(define-public lzfse
  (package
    (name "lzfse")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lzfse/lzfse")
             (commit (string-append "lzfse-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mfh6y6vpvxsdwmqmfbkqkwvxc0pz2dqqc72c6fk9sbsrxxaghd5"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/lzfse/lzfse")
    (synopsis "LZFSE compression library and command line tool")
    (description "LZFSE is a Lempel-Ziv style data compression algorithm using
Finite State Entropy coding.  It targets similar compression rates at higher
compression and decompression speed compared to Deflate using Zlib.")
    (license license:bsd-3)))

(define-public fcrackzip
  (package
    (name "fcrackzip")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://oldhome.schmorp.de/marc/data/"
                                  "fcrackzip-" version ".tar.gz"))
              (sha256
               (base32
                "0l1qsk949vnz18k4vjf3ppq8p497966x4c7f2yx18x8pk35whn2a"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'fix-reference-to-unzip
           (lambda _
             (substitute* "main.c"
               (("\"unzip")
                (string-append "\"" (which "unzip")))))))))
    (inputs
     (list perl unzip))
    (home-page "http://oldhome.schmorp.de/marc/fcrackzip.html")
    (synopsis "Zip password cracker")
    (description "Fcrackzip is a Zip file password cracker.")
    (license license:gpl2+)))

(define-public unrar-free
  (package
    (name "unrar-free")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/bgermann/unrar-free")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qf2fdr4bl10kys2fsb6090r9xj0ascrk7pn0iklsv1ajwcql3qf"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           pkg-config))
    (inputs
     (list libarchive))
    (home-page "https://gitlab.com/bgermann/unrar-free")
    (synopsis "Extract files from RAR archives")
    (description
     "@code{unrar-free} is a free software version of the non-free @code{unrar}
utility.  This program is a simple command-line front-end to libarchive, and can
list and extract not only RAR archives but also other formats supported by
libarchive.  It does not rival the non-free @code{unrar} in terms of features,
but special care has been taken to ensure it meets most user's needs.")
    (license license:gpl2+)))

(define-public miniz
  (package
    (name "miniz")
    (version "3.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/richgel999/miniz")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0672q35vjrpakmsr1gwj9k5fwv5ihzhahm19bq4y74wqpn91p7fw"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      ''("-DBUILD_SHARED_LIBS=ON")
      ;; No tests
      #:tests? #f))
    (home-page "https://github.com/richgel999/miniz")
    (synopsis "Independent implementation of zlib and Deflate compression")
    (description "Miniz is a lossless data compression library that implements
the zlib (RFC 1950) and Deflate (RFC 1951) compressed data format
specification standards.  It supports the most commonly used functions
exported by the zlib library.")
    (license license:expat)))

(define-public miniz-for-pytorch
  (package
    (inherit miniz)
    (version "pytorch-2.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/richgel999/miniz")
                    (commit "3.0.2")))
              (file-name (git-file-name (package-name miniz) version))
              (sha256
               (base32
                "0672q35vjrpakmsr1gwj9k5fwv5ihzhahm19bq4y74wqpn91p7fw"))
              (patches (search-patches "miniz-for-pytorch.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments miniz)
       ((#:configure-flags flags '())
        ;; The changes break the examples.
        `(cons "-DBUILD_EXAMPLES=OFF" ,flags))))
    (properties '((hidden? . #t)))))
