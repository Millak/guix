;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
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

(define-module (gnu packages pypy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public pypy
  (package
    (name "pypy")
    (version "7.3.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.python.org/pypy/"
                                  "pypy3.10-v" version "-src.tar.bz2"))
              (sha256
               (base32
                "0v9s6pwrnaxqi5h1pvmaphj6kgyczx07ykl07hcx656h34y77haa"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                      ;FIXME: 43 out of 364 tests are failing

      ;; XXX: ELF file 'pypy.debug' makes 'validate-needed-in-runpath' throw:
      ;; <https://issues.guix.gnu.org/57653>.
      #:validate-runpath? #f

      #:modules '((ice-9 ftw) (ice-9 match)
                  (guix build utils) (guix build gnu-build-system))
      #:disallowed-references (list nss-certs)
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("rpython/rlib/clibffi.py")
                ;; find_library does not work for libc
                (("ctypes\\.util\\.find_library\\('c'\\)") "'libc.so'"))
              (substitute* '("lib_pypy/cffi/_pycparser/ply/cpp.py")
                ;; Make reproducible (XXX: unused?)
                (("time\\.localtime\\(\\)") "time.gmtime(0)"))
              (substitute* '("pypy/module/sys/version.py")
                ;; Make reproducible
                (("t\\.gmtime\\(\\)") "t.gmtime(0)"))
              (substitute* '("lib_pypy/_tkinter/tklib_build.py")
                ;; Link to versioned libtcl and libtk
                (("linklibs = \\['tcl', 'tk'\\]")
                 "linklibs = ['tcl8.6', 'tk8.6']")
                (("incdirs = \\[\\]")
                 (string-append
                  "incdirs = ['"
                  #$(this-package-input "tcl") "/include', '"
                  #$(this-package-input "tk")  "/include']")))
              (substitute* '("lib_pypy/_curses_build.py")
                ;; Find curses
                (("/usr/local")
                 #$(this-package-input "ncurses")))
              (substitute* '("lib_pypy/_dbm.py")
                ;; Use gdbm compat library, so we don’t need to pull
                ;; in bdb.
                (("ctypes.util.find_library\\('db'\\)")
                 (format #f "~s" (search-input-file
                                  inputs "lib/libgdbm_compat.so"))))
              (substitute* '("lib_pypy/_sqlite3_build.py")
                ;; Always use search paths
                (("sys\\.platform\\.startswith\\('freebsd'\\)") "True")
                ;; Find sqlite3
                (("/usr/local") (assoc-ref inputs "sqlite"))
                (("libname = 'sqlite3'")
                 (format #f "libname = ~s"
                         (search-input-file inputs "lib/libsqlite3.so.0"))))
              (substitute* '("lib-python/3/subprocess.py")
                ;; Fix shell path
                (("/bin/sh")
                 (search-input-file inputs "/bin/sh")))
              (substitute* '("lib-python/3/distutils/unixccompiler.py")
                ;; gcc-toolchain does not provide symlink cc -> gcc
                (("\"cc\"") "\"gcc\""))))
          (add-after 'unpack 'set-source-file-times-to-1980
            ;; copied from python package, required by zip testcase
            (lambda _
              (let ((circa-1980 (* 10 366 24 60 60)))
                (ftw "." (lambda (file stat flag)
                           (utime file circa-1980 circa-1980)
                           #t)))))
          (replace 'build
            (lambda _
              (with-directory-excursion "pypy/goal"
                ;; Build with jit optimization.
                (invoke "python2"
                        "../../rpython/bin/rpython"
                        (string-append "--make-jobs="
                                       (number->string (parallel-job-count)))
                        "-Ojit"
                        "targetpypystandalone"
                        "--allworkingmodules"))
              ;; Build c modules and package everything, so tests work.
              (with-directory-excursion "pypy/tool/release"
                (invoke "python2" "package.py"
                        "--archive-name" "pypy-dist"
                        "--builddir" (getcwd)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (begin
                    (setenv "HOME" "/tmp") ; test_with_pip tries to
                                        ; access ~/.cache/pip
                    ;; Run library tests only (no interpreter unit tests).
                    ;; This is what Gentoo does.
                    (invoke "python" "pypy/test_all.py"
                            "--pypy=pypy/tool/release/pypy-dist/bin/pypy3"
                            "lib-python"))
                  (format #t "test suite not run~%"))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((bin-pypy3 (string-append #$output "/bin/pypy3"))
                     (shebang-match-python "#!.+/bin/python")
                     (shebang-pypy3 (string-append "#!" bin-pypy3))
                     (dist-dir "pypy/tool/release/pypy-dist"))
                (with-directory-excursion dist-dir
                  ;; Delete test data.
                  (for-each
                   (lambda (x)
                     (delete-file-recursively (string-append
                                               "lib/pypy3.10/" x)))
                   '("tkinter/test"
                     "test"
                     "sqlite3/test"
                     "lib2to3/tests"
                     "idlelib/idle_test"
                     "distutils/tests"
                     "ctypes/test"
                     "unittest/test"))
                  ;; Patch shebang referencing python.
                  (substitute* '("lib/pypy3.10/cgi.py"
                                 "lib/pypy3.10/encodings/rot_13.py")
                    ((shebang-match-python) shebang-pypy3))
                  (with-fluids ((%default-port-encoding "ISO-8859-1"))
                    (substitute* '("lib/pypy3.10/_md5.py"
                                   "lib/pypy3.10/_sha1.py")
                      ((shebang-match-python) shebang-pypy3))))
                (copy-recursively dist-dir #$output)))))))
    (native-inputs
     (list gzip
           nss-certs                    ; For ssl tests
           pkg-config
           python-2
           python2-pycparser
           tar))                        ; Required for package.py
    (inputs
     (list bzip2
           expat
           gdbm
           glibc
           libffi
           ncurses
           openssl
           sqlite
           tcl
           tk
           xz
           zlib))
    (home-page "https://www.pypy.org/")
    (synopsis "Python implementation with just-in-time compilation")
    (description "PyPy is a faster, alternative implementation of the Python
programming language employing a just-in-time compiler.  It supports most
Python code natively, including C extensions.")
    (license (list license:expat     ; pypy itself; _pytest/
                   license:psfl      ; python standard library in lib-python/
                   license:asl2.0    ; dotviewer/font/ and some of lib-python/
                   license:gpl3+ ; ./rpython/rlib/rvmprof/src/shared/libbacktrace/dwarf2.*
                   license:bsd-3 ; lib_pypy/cffi/_pycparser/ply/
                   (license:non-copyleft
                    "http://www.unicode.org/copyright.html")))))

(define-public pypy3
  (deprecated-package "pypy3" pypy))
