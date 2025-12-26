;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021, 2022, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2025 Laura Kirsch <laurakirsch240406@gmail.com>
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
  #:use-module (gnu packages nss)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python))

(define-public python2-pycparser
  (let ((base (package
                (version "2.18")
                (name "python-pycparser")
                (source
                 (origin
                   (method url-fetch)
                   (uri (pypi-uri "pycparser" version))
                   (sha256
                    (base32
                     "09mjyw82ibqzl449g7swy8bfxnfpmas0815d2rkdjlcqw81wma4r"))))
                (build-system pyproject-build-system)
                (arguments
                 (list
                  #:tests? #f
                  #:phases
                  #~(modify-phases %standard-phases
                      (replace 'build
                        (lambda _
                          (invoke "python" "setup.py" "build")))
                      (replace 'install
                        (lambda _
                          (invoke "python" "./setup.py" "install"
                                  (string-append "--prefix=" #$output)
                                  "--no-compile")
                          (invoke "python" "-m" "compileall" #$output))))))
                (home-page "https://github.com/eliben/pycparser")
                (synopsis "C parser in Python")
                (description
                 "Pycparser is a complete parser of the C language, written in
pure Python using the PLY parsing library.  It parses C code into an AST and
can serve as a front-end for C compilers or analysis tools.")
                (license license:bsd-3))))
    (package
      (inherit (package-with-python2 base))
      (native-inputs (list python-setuptools)))))

(define-public python2-cffi
  (let ((base (package
                (name "python-cffi")
                (version "1.15.1")
                (source
                 (origin
                   (method url-fetch)
                   (uri (pypi-uri "cffi" version))
                   (sha256
                    (base32 "1y9lr651svbzf1m03s4lqbnbv2byx8f6f0ml7hjm24vvlfwvy06l"))))
                (build-system pyproject-build-system)
                (arguments
                 (list
                  #:tests? #f
                  #:phases
                  #~(modify-phases %standard-phases
                      (replace 'build
                        (lambda _
                          (invoke "python" "setup.py" "build")))
                      (replace 'install
                        (lambda _
                          (let ((site (string-append #$output
                                                     "/lib/python2.7/site-packages/")))
                            (mkdir-p site)
                            (setenv "PYTHONPATH" site))
                          (invoke "python" "./setup.py" "install"
                                  (string-append "--prefix=" #$output)
                                  "--no-compile"))))))
                (native-inputs '())
                (inputs (modify-inputs (package-inputs python-cffi)
                          (append libxcrypt)))
                (propagated-inputs (list python2-pycparser))
                (home-page "https://cffi.readthedocs.io/")
                (synopsis "Foreign function interface for Python")
                (description "Foreign Function Interface for Python calling C code.")
                (license license:expat))))
    (package
      (inherit (package-with-python2 base))
      (native-inputs (list pkg-config python-setuptools)))))

;;; This Python 2 alternative implementation is useful for allowing faster
;;; iterations on the development of Pypy 3.
(define-public pypy2
  (package
    (name "pypy2")
    (version "7.3.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.python.org/pypy/"
                                  "pypy2.7-v" version "-src.tar.bz2"))
              (sha256
               (base32
                "0qiyd9bajxyzh4j5swk14iwyh4b7pn38sg3j721zfysz8jc895dz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((ice-9 ftw) (ice-9 match)
                  (guix build utils) (guix build gnu-build-system))
      ;; The stdlib tests do not all pass, this is known and tracked upstream
      ;; on their buildbot instance, for example at:
      ;; <https://buildbot.pypy.org/summary?branch=py3.11>.  That's also true
      ;; for Pypy 3.  The tests are also disabled to avoid having to package
      ;; extra Python 2 dependencies like python2-hypothesis.
      #:tests? #f
      ;; XXX: ELF file 'pypy.debug' makes 'validate-needed-in-runpath' throw:
      ;; <https://issues.guix.gnu.org/57653>.
      #:validate-runpath? #f
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
              (substitute* '("lib_pypy/dbm.py")
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
              (substitute* '("lib-python/2.7/subprocess.py")
                ;; Fix shell path
                (("/bin/sh")
                 (search-input-file inputs "/bin/sh")))
              (substitute* '("lib-python/2.7/distutils/unixccompiler.py")
                ;; gcc-toolchain does not provide symlink cc -> gcc
                (("\"cc\"") (format #f "~s" #$(cc-for-target))))
              (substitute* "lib-python/2.7/distutils/sysconfig_pypy.py"
                (("\"cc ")
                 (string-append "\"" #$(cc-for-target) " ")))))
          (add-after 'unpack 'use-libffi.so
            (lambda _
              (substitute* "rpython/rlib/clibffi.py"
                (("\"libffi\\.a\"")
                 "\"libffi.so\""))))
          (add-after 'unpack 'set-source-file-times-to-1980
            ;; copied from python package, required by zip testcase
            (lambda _
              (let ((circa-1980 (* 10 366 24 60 60)))
                (ftw "." (lambda (file stat flag)
                           (utime file circa-1980 circa-1980)
                           #t)))))
          (replace 'build
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (with-directory-excursion "pypy/goal"
                ;; Build with jit optimization.
                (invoke "python2"
                        "../../rpython/bin/rpython"
                        (string-append "--cc=" #$(cc-for-target))
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
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((bin-pypy2 (string-append #$output "/bin/pypy2"))
                     (shebang-match-python "#!.+/bin/python")
                     (shebang-pypy2 (string-append "#!" bin-pypy2))
                     (dist-dir "pypy/tool/release/pypy-dist"))
                (with-directory-excursion dist-dir
                  ;; Delete test data.
                  (for-each
                   (lambda (x)
                     (delete-file-recursively (string-append
                                               "lib-python/2.7/" x)))
                   '("lib-tk/test"
                     "test"
                     "lib2to3/tests"
                     "idlelib/idle_test"
                     "distutils/tests"
                     "ctypes/test"
                     "unittest/test"))
                  ;; Patch shebang referencing python.
                  (substitute* '("lib-python/2.7/cgi.py"
                                 "lib-python/2.7/encodings/rot_13.py")
                    ((shebang-match-python) shebang-pypy2)))
                (copy-recursively dist-dir #$output)))))))
    (native-inputs
     (list nss-certs                    ; For ssl tests
           pkg-config
           python-2
           python2-cffi))
    (inputs
     (list bzip2
           expat
           gdbm
           libffi
           ncurses
           openssl
           sqlite
           tcl
           tk
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

(define-public pypy
  (package
    (inherit pypy2)
    (name "pypy")
    (version "7.3.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.python.org/pypy/"
                                  "pypy3.11-v" version "-src.tar.bz2"))
              (sha256
               (base32
                "1yq6n888fxfdqid29q3w8bn7ii800bjkf44w82kjwgh0c2kxv1kp"))))
    (arguments
     (substitute-keyword-arguments (package-arguments pypy2)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'patch-source
              ;; Overridden from pypy2 because some file names have changed.
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
                  (("\"cc\"") (format #f "~s" #$(cc-for-target))))))
            (replace 'build
              (lambda _
                (with-directory-excursion "pypy/goal"
                  ;; Build with jit optimization.
                  (invoke "pypy2"
                          "../../rpython/bin/rpython"
                          (string-append "--cc=" #$(cc-for-target))
                          (string-append "--make-jobs="
                                         (number->string (parallel-job-count)))
                          "-Ojit"
                          "targetpypystandalone"
                          "--allworkingmodules"))
                ;; Build c modules and package everything, so tests work.
                (with-directory-excursion "pypy/tool/release"
                  (invoke "pypy2" "package.py"
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
                                                 "lib/pypy3.11/" x)))
                     '("tkinter/test"
                       "test"
                       "lib2to3/tests"
                       "idlelib/idle_test"
                       "distutils/tests"
                       "ctypes/test"
                       "unittest/test"))
                    ;; Patch shebang referencing python.
                    (substitute* '("lib/pypy3.11/cgi.py"
                                   "lib/pypy3.11/encodings/rot_13.py")
                      ((shebang-match-python) shebang-pypy3))
                    (with-fluids ((%default-port-encoding "ISO-8859-1"))
                      (substitute* '("lib/pypy3.11/_md5.py"
                                     "lib/pypy3.11/_sha1.py")
                        ((shebang-match-python) shebang-pypy3))))
                  (copy-recursively dist-dir #$output))))))))
    (native-inputs
     (modify-inputs (package-native-inputs pypy2)
       (replace "python2" pypy2)))
    (inputs
     (modify-inputs (package-inputs pypy2)
       (append xz)))))

(define-deprecated-package pypy3
  pypy)
