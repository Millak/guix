;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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
    (version "17.2.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.ceph.com/tarballs/ceph-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04m4zxp9hjvppl679ilnr90zr6ij08wr4ralr0whzldy0fskc8hp"))
              (patches
               (search-patches
                "ceph-disable-cpu-optimizations.patch"
                "ceph-fix-for-newer-boost.patch" ))
              (modules '((guix build utils)))
              (snippet
               '(for-each delete-file-recursively
                          '(;; TODO: Unbundle these:
                            "src/arrow"
                            ;;"src/isa-l"
                            ;;"src/lua"
                            ;;"src/xxHash"
                            ;;"src/zstd"
                            ;;"src/civetweb"
                            "src/c-ares"
                            "src/fmt"
                            "src/googletest"
                            "src/rapidjson"
                            "src/spdk"
                            "src/rocksdb"
                            "src/boost"
                            "src/utf8proc")))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:parallel-build? #f ;because mgr_legacy_options.h is not built in time
      #:configure-flags
      '(let* ((out (assoc-ref %outputs "out"))
              (lib (assoc-ref %outputs "lib"))
              (libdir (string-append lib "/lib")))
         (list (string-append "-DCMAKE_INSTALL_PREFIX=" out)
               (string-append "-DCMAKE_INSTALL_LIBDIR=" libdir)
               (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                              lib "/include")
               ;; We need both libdir and libdir/ceph in RUNPATH.
               (string-append "-DCMAKE_INSTALL_RPATH="
                              libdir ";" libdir "/ceph")
               (string-append "-DCMAKE_INSTALL_SYSCONFDIR=" out "/etc")
               (string-append "-DCMAKE_INSTALL_DATADIR=" lib "/share")
               (string-append "-DCMAKE_INSTALL_MANDIR=" out "/share/man")
               (string-append "-DCMAKE_INSTALL_DOCDIR=" out "/share/ceph/doc")
               (string-append "-DCMAKE_INSTALL_LIBEXECDIR=" out "/libexec")
               (string-append "-DKEYUTILS_INCLUDE_DIR="
                              (assoc-ref %build-inputs "keyutils") "/include")
               (string-append "-DXFS_INCLUDE_DIR="
                              (assoc-ref %build-inputs "xfsprogs") "/include")
               "-DCMAKE_INSTALL_LOCALSTATEDIR=/var"
               "-DBUILD_SHARED_LIBS=ON"
               "-DWITH_SYSTEM_ARROW=ON"
               "-DWITH_SYSTEM_BOOST=ON"
               "-DWITH_SYSTEM_ROCKSDB=ON"
               "-DWITH_SYSTEM_UTF8PROC=ON"

               ;; TODO: Enable these when available in Guix.
               "-DWITH_MGR_DASHBOARD_FRONTEND=OFF" ;requires node + nodeenv
               "-DWITH_BABELTRACE=OFF"
               "-DWITH_LTTNG=OFF"
               "-DWITH_SPDK=OFF"
               "-DWITH_RADOSGW_AMQP_ENDPOINT=OFF"

               ;; Use jemalloc instead of tcmalloc.
               "-DALLOCATOR=jemalloc"

               ;; Don't install systemd unit files.
               "-DWITH_SYSTEMD=OFF"

               ;; Do not bother building the tests; we are not currently running
               ;; them, and they do not build with system googletest as of 14.2.5.
               "-DWITH_TESTS=OFF"))
       ;; FIXME: Some of the tests leak Btrfs subvolumes on Btrfs. See
       ;; <https://bugs.gnu.org/29674> for details. Disable tests until
       ;; resolved.
       #:tests? #f
       #:phases
       `(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (lib (assoc-ref outputs "lib")))

                (substitute* "src/rgw/store/dbstore/sqlite/CMakeLists.txt"
                  (("add_library\\(sqlite_db \\$")
                   "add_library(sqlite_db STATIC $"))
                (substitute* "src/rgw/store/dbstore/CMakeLists.txt"
                  (("add_library\\(dbstore \\$")
                   "add_library(dbstore STATIC $")
                  (("add_library\\(dbstore_lib \\$")
                   "add_library(dbstore_lib STATIC $"))

                (substitute* "cmake/modules/Distutils.cmake"
                  ;; Prevent creation of Python eggs.
                  (("setup.py install")
                   "setup.py install --single-version-externally-managed --root=/")
                  ;; Inject the -rpath linker argument when linking
                  ;; Python C libraries so RUNPATH gets set up correctly.
                  (("LDFLAGS=(.*)\n" _ flags)
                   (string-append "LDFLAGS=\\\"" flags
                                  " -Wl,-rpath=" lib "/lib\\\"\n")))

                ;; Statically link libcrc32 because it does not get installed,
                ;; yet several libraries end up referring to it.
                (substitute* "src/common/CMakeLists.txt"
                  (("add_library\\(crc32")
                   "add_library(crc32 STATIC"))

                (substitute* "udev/50-rbd.rules"
                  (("/usr/bin/ceph-rbdnamer")
                   (string-append out "/bin/ceph-rbdnamer"))))))
          (add-before 'install 'set-install-environment
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (py3sitedir
                      (string-append out "/lib/python"
                                     ,(version-major+minor
                                       (package-version python))
                                     "/site-packages")))
                ;; The Python install scripts refuses to function if
                ;; the install directory is not on PYTHONPATH.
                (setenv "PYTHONPATH" py3sitedir))))
          (add-after 'install 'wrap-python-scripts
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (scripts '("bin/ceph" "bin/cephfs-top" "sbin/ceph-volume"))
                     (dependencies (map (lambda (input)
                                          (assoc-ref inputs input))
                                        '("python-prettytable" "python-pyyaml")))
                     (sitedir (lambda (package)
                                (string-append package
                                               "/lib/python"
                                               ,(version-major+minor
                                                 (package-version python))
                                               "/site-packages")))
                     (PYTHONPATH (string-join (map sitedir (cons out dependencies))
                                              ":")))
                (for-each (lambda (executable)
                            (wrap-program (string-append out "/" executable)
                              `("GUIX_PYTHONPATH" ":" prefix (,PYTHONPATH))))
                          scripts)))))))
    (outputs
     '("out" "lib"))
    (native-inputs
     (list git-minimal gperf pkg-config python-cython python-sphinx yasm))
    (inputs
     (list `(,apache-thrift "lib")
           `(,apache-thrift "include")
           `(,apache-arrow-for-ceph "lib")
           bash-minimal
           boost
           curl
           cryptsetup-minimal
           eudev
           expat
           fcgi
           fmt-8
           fuse-2
           icu4c
           jemalloc
           keyutils
           leveldb
           libaio
           libatomic-ops
           libcap-ng
           libnl
           librdkafka
           lua
           lz4
           oath-toolkit
           openldap
           openssl
           ncurses
           nss
           python-prettytable           ;used by ceph_daemon.py
           python-pyyaml                ;from python-common/setup.py
           python
           rapidjson
           rdma-core
           rocksdb
           snappy
           sqlite
           utf8proc
           util-linux
           `(,util-linux "lib")
           xfsprogs
           zlib))
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
    (license (list license:lgpl2.1 license:gpl2  ;some files are 'or later'
                   license:cc-by-sa3.0           ;documentation
                   license:bsd-3                 ;isa-l,jerasure,++
                   license:expat))))             ;civetweb,java bindings

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
