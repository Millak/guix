;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2019 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2016, 2017, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 John Doe <dftxbs3e@free.fr>
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

(define-module (gnu packages libffi)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sphinx)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby))

(define-public libffi
  (package
    (name "libffi")
    (version "3.4.4")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/libffi/libffi/releases"
                              "/download/v" version "/"
                              name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xpn5mqlbdmqgxgp910ba1qj79axpwr8nh7wklmcz0ls4nnmcv6n"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Prevent the build system from passing -march and -mtune to the
       ;; compiler.  See "ax_cc_maxopt.m4" and "ax_gcc_archflag.m4".
       #:configure-flags '("--enable-portable-binary"
                           "--without-gcc-arch")))
    (outputs '("out" "debug"))
    (synopsis "Foreign function call interface library")
    (description
     "The libffi library provides a portable, high level programming interface
to various calling conventions.  This allows a programmer to call any
function specified by a call interface description at run-time.

FFI stands for Foreign Function Interface.  A foreign function interface is
the popular name for the interface that allows code written in one language
to call code written in another language.  The libffi library really only
provides the lowest, machine dependent layer of a fully featured foreign
function interface.  A layer must exist above libffi that handles type
conversions for values passed between the two languages.")
    (home-page "https://www.sourceware.org/libffi/")
    (properties `((release-monitoring-url . ,home-page)))

    ;; See <https://github.com/atgreen/libffi/blob/master/LICENSE>.
    (license expat)))

;; Provide a variant without static trampolines as some packages
;; (particularly GHC < 9) cannot handle them.  See
;; <https://github.com/libffi/libffi/pull/647> for a discussion.
(define-public libffi-sans-static-trampolines
  (hidden-package
   (package/inherit libffi
     (arguments
      (substitute-keyword-arguments (package-arguments libffi)
        ((#:configure-flags flags #~'())
         #~(append #$flags '("--disable-exec-static-tramp"))))))))

(define-public python-cffi
  (package
    (name "python-cffi")
    (version "1.15.1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "cffi" version))
      (sha256
       (base32 "1y9lr651svbzf1m03s4lqbnbv2byx8f6f0ml7hjm24vvlfwvy06l"))))
    (build-system python-build-system)
    (inputs
     (list libffi))
    (propagated-inputs ; required at run-time
     (list python-pycparser))
    (native-inputs
     (list pkg-config python-pytest))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; XXX The "normal" approach of setting CC and friends does
             ;; not work here.  Is this the correct way of doing things?
             (substitute* "testing/embedding/test_basic.py"
               (("c = distutils\\.ccompiler\\.new_compiler\\(\\)")
                (string-append "c = distutils.ccompiler.new_compiler();"
                               "c.set_executables(compiler='gcc',"
                               "compiler_so='gcc',linker_exe='gcc',"
                               "linker_so='gcc -shared')")))
             (substitute* "testing/cffi0/test_ownlib.py"
               (("\"cc testownlib") "\"gcc testownlib"))
             (invoke "pytest" "-v" "c/" "testing/"
                     ;; Disable tests that fail (harmlessly) with glibc
                     ;; 2.34 and later:
                     ;; https://foss.heptapod.net/pypy/cffi/-/issues/528
                     "-k" (string-append "not TestFFI.test_dlopen_handle "
                                         "and not test_dlopen_handle"))))
         (add-before 'check 'patch-paths-of-dynamically-loaded-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Shared libraries should be referred by their absolute path as
             ;; using find_library or the like with their name fail when the
             ;; resolved .so object is a linker script rather than an ELF
             ;; binary (this is a limitation of the ctype library of Python).
             (let ((libm (search-input-file inputs "lib/libm.so.6"))
                   (libc (search-input-file inputs "lib/libc.so.6")))
               (substitute* '("testing/cffi0/test_function.py"
                              "testing/cffi0/test_parsing.py"
                              "testing/cffi0/test_unicode_literals.py"
                              "testing/cffi0/test_zdistutils.py"
                              "testing/cffi1/test_recompiler.py")
                 (("lib_m = ['\"]{1}m['\"]{1}")
                  (format #f "lib_m = '~a'" libm)))
               (substitute* '("testing/cffi0/test_verify.py"
                              "testing/cffi1/test_verify1.py")
                 (("lib_m = \\[['\"]{1}m['\"]{1}\\]")
                  (format #f "lib_m = ['~a']" libm)))
               (substitute* "c/test_c.py"
                 (("find_and_load_library\\(['\"]{1}c['\"]{1}")
                  (format #f "find_and_load_library('~a'" libc)))))))))
    (home-page "https://cffi.readthedocs.io/")
    (synopsis "Foreign function interface for Python")
    (description "Foreign Function Interface for Python calling C code.")
    (license expat)))

(define-public python-cffi-documentation
  (package
    (name "python-cffi-documentation")
    (version (package-version python-cffi))
    (source (package-source python-cffi))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'chdir
                    (lambda _ (chdir "doc") #t))
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key (make-flags '()) #:allow-other-keys)
                      (apply invoke "make" "html" make-flags)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (copy-recursively "build/html" (string-append out "/html"))
                        #t))))))
    (native-inputs
     `(("sphinx-build" ,python-sphinx)))
    (home-page (package-home-page python-cffi))
    (synopsis "Documentation for the Python CFFI interface")
    (description
     "This package contains HTML documentation for the @code{python-cffi}
project.")
    (license (package-license python-cffi))))

(define-public ruby-ffi
  (package
    (name "ruby-ffi")
    (version "1.15.5")
    (source (origin
              ;; Pull from git because the RubyGems release bundles LibFFI,
              ;; and comes with a gemspec that makes it difficult to unbundle.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ffi/ffi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qk55s1zwpdjykwkj9l37m71i5n228i7f8bg3ply3ks9py16m7s6"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'replace-git-ls-files
           (lambda _
             ;; Do not try to execute git, or include the (un)bundled LibFFI.
             (substitute* "ffi.gemspec"
               (("git ls-files -z")
                "find * -type f -print0 | sort -z")
               (("lfs \\+?= .*")
                "lfs = []\n"))
             (substitute* "Rakefile"
               (("git .*ls-files -z")
                "find * -type f -print0 | sort -z")
               (("LIBFFI_GIT_FILES = .*")
                "LIBFFI_GIT_FILES = []\n"))))
         (replace 'build
          (lambda _
            ;; Tests depend on the native extensions, so we build it
            ;; beforehand without going through the gem machinery.
             (invoke "rake" "compile")

             ;; XXX: Ideally we'd use "rake native gem" here to prevent the
             ;; install phase from needlessly rebuilding everything, but that
             ;; requires the bundled LibFFI, and the install phase can not
             ;; deal with such gems anyway.
             (invoke "gem" "build" "ffi.gemspec")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (begin
                   (setenv "MAKE" "make")
                   (setenv "CC" "gcc")
                   (invoke "rspec" "spec"))
                 (format #t "test suite not run~%")))))))
    (native-inputs
     (list ruby-rake-compiler ruby-rspec ruby-rubygems-tasks))
    (inputs
     (list libffi))
    (synopsis "Ruby foreign function interface library")
    (description "Ruby-FFI is a Ruby extension for programmatically loading
dynamic libraries, binding functions within them, and calling those functions
from Ruby code.  Moreover, a Ruby-FFI extension works without changes on Ruby
and JRuby.")
    (home-page "https://wiki.github.com/ffi/ffi")
    (license bsd-3)))
