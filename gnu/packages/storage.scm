;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages storage)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages authentication)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages dpdk)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ceph
  (package
    (name "ceph")
    (version "20.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ceph/ceph")
                     (commit (string-append "v" version))
                     ;; There are many source bundled libraries, and the build
                     ;; system does not yet support using dependencies from
                     ;; the system for all of them.
                     (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08k7f1nj3g1mb7h8vgw71xgpslz0dhqwr6ldzxs3naf2f73x0mh8"))
              (patches (search-patches "ceph-fix-cmake.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled libraries where feasible.
               '(for-each delete-file-recursively
                          '( ;; TODO: Unbundle these:
                            ;;"src/isa-l"
                            ;;"src/xxHash"
                            "src/BLAKE3"
                            "src/arrow"
                            "src/c-ares"
                            "src/fmt"
                            "src/googletest"
                            "src/jaegertracing"
                            "src/qatlib"
                            "src/qatzip"
                            "src/spdk"
                            "src/rocksdb"
                            "src/utf8proc"
                            "src/zstd"
                            "systemd")))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DCMAKE_INSTALL_RPATH="
                             #$output "/lib" ";" #$output "/lib/ceph")
              "-DENABLE_GIT_VERSION=OFF"
              "-DCMAKE_INSTALL_LOCALSTATEDIR=/var"
              "-DBUILD_SHARED_LIBS=ON"
              "-DCEPHADM_BUNDLED_DEPENDENCIES=none"
              "-DWITH_SYSTEM_ARROW=ON"
              "-DWITH_SYSTEM_BOOST=ON"
              "-DWITH_SYSTEM_FMT=ON"
              #$@(if (target-x86-64?)
                     #~("-DWITH_SYSTEM_QATLIB=ON"
                        "-DWITH_SYSTEM_QATZIP=ON")
                     #~())
              "-DWITH_SYSTEM_LIBURING=ON"
              "-DWITH_SYSTEM_ROCKSDB=ON"
              "-DWITH_SYSTEM_UTF8PROC=ON"
              "-DWITH_SYSTEM_ZSTD=ON"

              ;; TODO: Enable these when available in Guix.
              "-DWITH_MGR=OFF"        ;requires python-rook-client
              "-DWITH_MGR_DASHBOARD_FRONTEND=OFF" ;requires node + nodeenv
              "-DWITH_BABELTRACE=OFF"
              "-DWITH_JAEGER=OFF"     ;requires bundled opentelemetry-cpp
              "-DWITH_LTTNG=OFF"
              "-DWITH_RADOSGW=OFF"    ;requires bundled libkmip and rgw
              "-DWITH_SPDK=OFF"
              "-DWITH_RADOSGW_AMQP_ENDPOINT=OFF"

              ;; Use jemalloc instead of tcmalloc.
              "-DALLOCATOR=jemalloc"

              ;; Don't install systemd unit files.
              "-DWITH_SYSTEMD=OFF"

              ;; Do not bother building the tests; we are not currently running
              ;; them, and they do not build with system googletest as of 14.2.5.
              "-DWITH_TESTS=OFF")
      ;; FIXME: Some of the tests leak Btrfs subvolumes on Btrfs. See
      ;; <https://bugs.gnu.org/29674> for details. Disable tests until
      ;; resolved.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unbundle-blake3
            (lambda _
              ;; Unbundle Blake3.
              (substitute* "src/CMakeLists.txt"
                (("add_subdirectory.*BLAKE3.*") ""))))
          (add-after 'unpack 'relax-fmt-requirement
            (lambda _
              (substitute* "src/CMakeLists.txt"
                (("11.1.4 REQUIRED")
                 "<12 REQUIRED"))))
          (add-after 'unpack 'fix-zstd-include
            (lambda _
              ;; See <https://tracker.ceph.com/issues/63194>.
              (substitute* "src/compressor/zstd/ZstdCompressor.h"
                (("#include \"zstd/lib/zstd.h\"")
                 "#include \"zstd.h\""))))
          (add-after 'unpack 'set-source-file-times-to-1980
            (lambda _
              (let ((circa-1980 (* 10 366 24 60 60)))
                (for-each (lambda (f) (utime f circa-1980 circa-1980))
                          (find-files "src")))))
          (add-before 'build 'build-legacy-option-header
            (lambda _
              ;; Building this target in parallel causes races (see:
              ;; <https://tracker.ceph.com/issues/63402>), so build it before
              ;; the main build without parallelism.
              (invoke "make" "legacy-option-headers")))
          (add-after 'unpack 'patch-source
            (lambda _
              (substitute* "cmake/modules/Distutils.cmake"
                ;; Prevent creation of Python eggs.
                (("setup.py install")
                 "setup.py install --single-version-externally-managed --root=/")
                ;; Inject the -rpath linker argument when linking
                ;; Python C libraries so RUNPATH gets set up correctly.
                (("LDFLAGS=(.*)\n" _ flags)
                 (string-append "LDFLAGS=\\\"" flags
                                " -Wl,-rpath=" #$output "/lib\\\"\n")))
              (substitute* "udev/50-rbd.rules"
                (("/usr/bin/ceph-rbdnamer")
                 (string-append #$output "/bin/ceph-rbdnamer")))))
          (add-after 'install 'wrap-python-scripts
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((scripts '("bin/ceph" "bin/cephfs-top" "sbin/ceph-volume"))
                     (dependencies (map (lambda (input)
                                          (assoc-ref inputs input))
                                        '("python-prettytable" "python-pyyaml")))
                     (site (lambda (package)
                             (string-append package
                                            "/lib/python"
                                            #$(version-major+minor
                                               (package-version
                                                (this-package-input "python")))
                                            "/site-packages")))
                     (PYTHONPATH (string-join
                                  (map site (cons #$output dependencies))
                                  ":")))
                (for-each (lambda (s)
                            (wrap-program (string-append #$output "/" s)
                              `("GUIX_PYTHONPATH" prefix (,PYTHONPATH))))
                          scripts)))))))
    (native-inputs
     (list pkg-config
           python
           python-cython
           python-sphinx
           yasm))
    (inputs
     (append
      (if (target-x86-64?)
          (list qatlib qatzip)
          '())
      (list `(,apache-thrift "lib")
            `(,apache-thrift "include")
            `(,apache-arrow "lib")
            `(,util-linux "lib")
            bash-minimal
            boost-1.88
            cryptsetup-minimal
            curl
            eudev
            expat
            fcgi
            fmt-11
            fuse
            icu4c
            jemalloc
            keyutils
            leveldb
            libaio
            libatomic-ops
            libcap                      ;for src/extblkdev
            libcap-ng
            libblake3
            libnbd
            libnl
            librdkafka
            liburing
            lmdb
            lua
            lz4
            ncurses
            nss
            oath-toolkit
            openldap
            openssl
            python
            python-prettytable          ;used by ceph_daemon.py
            python-pyyaml               ;from python-common/setup.py
            rapidjson
            rdma-core
            rocksdb-for-ceph
            snappy
            sqlite
            utf8proc
            util-linux
            xfsprogs
            zlib
            `(,zstd "lib"))))
    (home-page "https://ceph.com/")
    (synopsis "Distributed object store and file system")
    (description
     "Ceph is a distributed storage system designed for reliability and
performance.  It provides network-based block devices (RBD), a POSIX
compliant file system (CephFS), and offers compatibility with various
storage protocols (S3, NFS, and others) through the RADOS gateway.")
    ;; The Ceph libraries are LGPL2.1 and most of the utilities fall under
    ;; GPL2. The installed erasure code plugins are BSD-3 licensed and do
    ;; not use the GPL code. The source archive includes a number of files
    ;; carrying other licenses; consult COPYING for more information. Note
    ;; that COPYING does not cover third-party bundled software.
    (license (list license:lgpl2.1 license:gpl2 ;some files are 'or later'
                   license:cc-by-sa3.0          ;documentation
                   license:bsd-3                ;isa-l,jerasure,++
                   license:expat))))            ;java bindings

(define-public libnbd
  (package
    (name "libnbd")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.libguestfs.org/" name "/"
                                  (version-major+minor version) "-stable/"
                                  name "-" version ".tar.gz"))

              (sha256
               (base32
                "1vz62w270p23787v1r51rbcfd9lh73b99mcaf7q27pldc7bazggm"))))
    (build-system gnu-build-system)
    (arguments
     ;; The fork-safe-execvpe test fails for unknown reasons (see:
     ;; <https://gitlab.com/nbdkit/libnbd/-/issues/14>).
     (list
      #:configure-flags
      #~(list (string-append "--with-python-installdir="
                             #$output "/lib/python"
                             #$(package-version (this-package-input
                                                 "python-minimal"))
                             "/site-packages"))
      #:make-flags #~(list "XFAIL_TESTS=test-fork-safe-execvpe.sh")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-commands
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* '("lib/utils.c"
                                      "sh/nbdsh.in")
                         (("/bin/sh")
                          (search-input-file inputs "bin/sh"))))))))
    (native-inputs (list pkg-config perl))
    (inputs (list bash-minimal fuse gnutls libxml2
                  python-minimal))      ;match the python used by libxml2
    (home-page "https://gitlab.com/nbdkit/libnbd")
    (synopsis "Network Block Device client library in userspace")
    (description "@acronym{NBD, Network Block Device} is a protocol
for accessing block devices (hard disks and disk-like things) over a network.
This is the NBD client library in userspace, a simple library for writing NBD
clients.  The key features are:
@itemize
@item Synchronous API for ease of use.
@item Asynchronous API for writing non-blocking, multithreaded clients.
@item High performance.
@item Minimal dependencies for the basic library.
@item Well-documented, stable API.
@item Bindings in several programming languages.
@item Shell (nbdsh) for command line and scripting.
@item Copying tool (nbdcopy) for high performance copying and streaming.
@item Hexdump tool (nbddump) to print NBD content.
@item Query tool (nbdinfo) to query NBD servers.
@item FUSE support (nbdfuse) to mount NBD in the local file system.
@item Linux ublk support (nbdublk) to create the userspace block device.
@end itemize")
    (license license:lgpl2.1+)))

(define-public spdk
  (package
    (name "spdk")
    (version "24.09")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spdk/spdk")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1cx0yj3ibmbngqsjdlnh7qg7as9mzpbiw0zscraw07b4rs6g0s6q"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; FIXME: Issue with dpdk: error: depends on
      ;; 'libspdk_env_dpdk_rpc.so.6.0', which cannot be found in RUNPATH
      #:tests? #f
      #:imported-modules
      (append %default-gnu-imported-modules
              %python-build-system-modules)
      #:validate-runpath? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:prefix python:)
                  (ice-9 rdelim)
                  (ice-9 popen))
      #:configure-flags
      #~(list "--with-shared"
              (string-append "--prefix=" #$output)
              (string-append "--with-dpdk=" #$(this-package-input "dpdk")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'substitute-git-submodules
            (lambda _
              (rmdir "isa-l")
              (symlink #$(package-source (this-package-input "isa-l")) "isa-l")))
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (substitute* "configure"
                (("#! /bin/sh") (string-append "#!" (which "sh"))))
              (setenv "CC" #$(cc-for-target))
              (setenv "CFLAGS" "-mssse3")
              (setenv "CONFIG_SHELL" (which "bash"))
              (setenv "LDFLAGS" "-lbsd")
              (setenv "SHELL" (which "bash"))
              (apply invoke "./configure" configure-flags)))
          (add-before 'install 'set-install-environment
            ;; same as ceph
            (lambda _
              (let ((py3sitedir
                     (string-append #$output "/lib/python"
                                    #$(version-major+minor
                                       (package-version python))
                                    "/site-packages")))
                (setenv "PYTHONPATH" py3sitedir))))
          (add-after 'install 'patchelf
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((binaries '("iscsi_tgt" "nvmf_tgt"
                                 "spdk_dd" "spdk_lspci" "spdk_nvme_discover"
                                 "spdk_tgt" "spdk_top" "spdk_trace"
                                 "spdk_nvme_identify" "spdk_nvme_perf"
                                 "spdk_trace_record" "vhost"))
                     (ld-so (search-input-file inputs #$(glibc-dynamic-linker)))
                     (libs
                      (list
                       (string-append #$(this-package-input "libbsd") "/lib")
                       (string-append #$(this-package-input "libarchive") "/lib")
                       (string-append #$(this-package-input "numactl") "/lib")
                       (string-append #$(gexp-input util-linux "lib") "/lib")
                       (string-append #$(this-package-input "openssl") "/lib")
                       (string-append #$(this-package-input "openlibm") "/lib")
                       (string-append #$(this-package-input "libaio") "/lib")
                       (string-append #$(this-package-input "dpdk") "/lib")
                       (string-append #$(this-package-input "fuse") "/lib")
                       (string-append #$(this-package-input "gcc") "/lib")
                       (string-append #$(this-package-input "glibc") "/lib")))
                     (rpath* (apply string-append (map (lambda (l) (string-append l ":")) libs))))
                (for-each
                 (lambda (b)
                   (let ((file (string-append #$output "/bin/" b)))
                     (invoke "patchelf" "--set-interpreter" ld-so file)
                     (let* ((pipe (open-pipe* OPEN_READ "patchelf" "--print-rpath" file))
                            (line (read-line pipe)))
                       (and (zero? (close-pipe pipe))
                            (invoke "patchelf" "--set-rpath" (string-append rpath* line)
                                    file)))
                     (wrap-program file
                       `("LD_LIBRARY_PATH" ":" prefix (,(string-append #$output "/lib"))))))
                 binaries))))
          (add-after 'patchelf 'python:wrap
            (assoc-ref python:%standard-phases 'wrap))
          (add-after 'python:wrap 'wrap-python-scripts
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((scripts '("bin/spdk_cli" "bin/spdk_rpc" ))
                     (dependencies (map (lambda (input)
                                          (assoc-ref inputs input))
                                        '("python-configshell-fb")))
                     (python-version #$(version-major+minor (package-version python)))
                     (sitedir (lambda (package)
                                (string-append package "/lib/python"
                                               python-version "/site-packages")))
                     (PYTHONPATH (string-join (map sitedir (cons #$output dependencies)) ":")))
                (for-each (lambda (executable)
                            (wrap-program (string-append #$output "/" executable)
                              `("GUIX_PYTHONPATH" ":" prefix (,PYTHONPATH))))
                          scripts)))))))
    (native-inputs
     (list autoconf
           automake
           patchelf
           pkg-config
           python-wrapper))
    (inputs
     (list cunit
           dpdk
           elfutils
           fuse
           (list gcc "lib")
           glibc
           isa-l
           jansson
           libaio
           libarchive
           libbsd
           libnl
           libpcap
           ncurses
           numactl
           openlibm
           openssl
           python-configshell-fb
           python-pip             ; XXX: expected by install, check why
           python-setuptools      ;
           python-wheel           ;
           (list util-linux "lib") ;; <uuid.h>
           zlib
           zstd))
    (home-page "https://spdk.io/")
    (synopsis "Storage Performance Development Kit")
    (description
     "@acronym{Storage Performance Development Kit, SPDK} provides a set of
tools and libraries for writing high performance,scalable, user-mode storage
applications.")
    (license license:bsd-3)))
