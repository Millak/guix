;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2024 jgart <jgart@dismail.de>
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

(define-module (gnu packages prolog)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public gprolog
  (package
    (name "gprolog")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        ;; Recent versions are not hosted on the GNU mirrors.
        (uri (list (string-append "http://gprolog.org/gprolog-" version
                                  ".tar.gz")
                   (string-append "mirror://gnu/gprolog/gprolog-" version
                                  ".tar.gz")))
        (sha256
         (base32
          "009ca4wn2q6xdmb0js0vz647cw5ygsqyyqc9svmjgahg7js441k7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append
              "--with-install-dir=" %output "/share/gprolog"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'change-dir
           (lambda _
             (chdir "src"))))))
    (home-page "https://www.gnu.org/software/gprolog/")
    (synopsis "Prolog compiler")
    (description
     "GNU Prolog is a standards-compliant Prolog compiler with constraint
solving over finite domains.  It accepts Prolog+ constraint programs and
produces a compiled, native binary which can function in a stand-alone
manner.  It also features an interactive interpreter.")
    (license (list license:gpl2+
                   license:lgpl3+))

    ;; See 'configure' for the list of supported architectures.
    (supported-systems (fold delete
                             %supported-systems
                             '("armhf-linux" "mips64el-linux")))))

(define-public swi-prolog
  (package
    (name "swi-prolog")
    (version "9.3.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SWI-Prolog/swipl-devel")
                    (recursive? #t) ; TODO: Determine if this can be split out.
                    (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q2dg7n3vf0qc9nxppbcqgbzj4rcjw8sv2vbg9s6i4ixxlsxr5y3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:parallel-build? #t
       #:configure-flags
       ,#~(list
           #$@(if (%current-target-system)
                  ;; Set this manually, otherwise CMake would need to
                  ;; run a cross-compiled binary, which it can't do.
                  ;; These values were found on a Linux system.
                  #~("-DBSD_SIGNALS=1" "-DQSORT_R_GNU=1"
                     ;; If absent, the non-existent 'cc' is used.
                     "-DCMAKE_HOST_CC=gcc"
                     ;; swi-prolog needs a native copy of itself for
                     ;; cross-compilation.
                     "-DSWIPL_NATIVE_FRIEND=/nowhere"
                     (string-append "-DPROG_SWIPL="
                                    #+(this-package-native-input "swi-prolog")
                                    "/bin/swipl"))
                  #~())
           "-DINSTALL_DOCUMENTATION=ON"
           "-DSWIPL_INSTALL_IN_LIB=OFF") ; FIXME: Breaks RUNPATH validation.
       #:phases
       ,#~(modify-phases %standard-phases
            ;; XXX: Delete the test phase that attempts to write to the
            ;; immutable store.
            (add-after 'unpack 'delete-failing-tests
              (lambda _
                (substitute* "src/CMakeLists.txt"
                  ((" save") ""))
                (substitute* "src/test.pl"
                  (("testdir\\('Tests/save'\\).") ""))
                (delete-file-recursively "src/Tests/save")))
            #$@(if (%current-target-system)
                   ;; Prevent man_server.pl and swipl-lfr.pl from keeping a
                   ;; reference to the native swi-prolog.
                   ;; FIXME: libswipl.so and swipl-ld keep a reference to the
                   ;; cross-compiler.
                   #~((add-after 'install 'fix-cross-references
                        (lambda _
                          (define bin `(,(string-append #$output "/bin")))
                          (for-each (lambda (file) (patch-shebang file bin))
                                    (find-files #$output ".pl$")))))
                   #~()))))
    (native-inputs
     `(,@(if (%current-target-system)
             (begin
               (unless (equal? (target-64bit?)
                               (target-64bit? (%current-system)))
                 (error "swi-prolog requires --system and --target to have \
the same word size"))
               `(("swi-prolog" ,this-package)))
             '())
       ("texinfo" ,texinfo)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bash-minimal" ,bash-minimal)   ;for some scripts in 'lib'
       ("zlib" ,zlib)
       ("gmp" ,gmp)
       ("readline" ,readline)
       ("libarchive" ,libarchive)
       ("libunwind" ,libunwind)
       ("libjpeg" ,libjpeg-turbo)
       ("libxcrypt" ,libxcrypt)
       ("libxft" ,libxft)
       ("fontconfig" ,fontconfig)
       ("openssl" ,openssl)))
    (home-page "https://www.swi-prolog.org/")
    (synopsis "ISO/Edinburgh-style Prolog interpreter")
    (description "SWI-Prolog is a fast and powerful ISO/Edinburgh-style Prolog
compiler with a rich set of built-in predicates.  It offers a fast, robust and
small environment which enables substantial applications to be developed with
it.")
    (license license:bsd-2)))

(define-public trealla
  (package
    (name "trealla")
    (version "2.71.31")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/trealla-prolog/trealla")
         (commit (string-append "v" version))))
       (sha256
        (base32 "09rh67xc2h7wmwjm8czrdask3np4wwdph47jqxq6hpm4mcjp1g7w"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (append
       (if (supported-package? valgrind/pinned)
           (list valgrind/pinned)
           '())
       (list xxd)))
    (inputs
     (list libffi openssl readline))
    (arguments
     (list
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
      #:test-target (if (this-package-native-input "valgrind")
                        "check"
                        "test")
      #:phases
      #~(modify-phases %standard-phases
          ;; Upstream does not use a configure script.
          (delete 'configure)
          (add-before 'build 'patch-package-version
            (lambda _
              (substitute* "Makefile"
                (("\\$\\(shell git describe --abbrev=4 --dirty --always --tags\\)")
                 (string-append "v" #$version)))))
          (replace 'install
            ;; Upstream does not provide an install target.
            (lambda _
              (install-file "tpl" (string-append #$output "/bin")))))))
    (home-page "https://trealla-prolog.org/")
    (synopsis "Compact and efficient Prolog interpreter")
    (description "This package provides a compact and efficient Prolog
interpreter with ISO Prolog aspirations.")
    (license
     (list license:expat
           ;; The tiny-regex-c library uses the Unlicense license.
           license:unlicense))))

(define-public logtalk
  (package
    (name "logtalk")
    (version "3.80.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://logtalk.org/files/logtalk-"
                           version ".tar.bz2"))
       (sha256 (base32 "0g4cc0qv921234548k3rkv1sdayicaa6zzh15vgn82ri9apwm3nh"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ;no configure script and Makefile
               (delete 'build)
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (mkdir #$output)
                   (invoke "./scripts/install.sh" "-p" #$output))))))
    (native-search-paths
     (list (search-path-specification
            (variable "LOGTALKHOME")
            (separator #f)              ;single valued
            (files '("share/logtalk")))))
    (home-page "https://logtalk.org/")
    (synopsis "Object-oriented logic programming language")
    (description "Logtalk is a declarative object-oriented logic programming language
that extends and leverages the Prolog language with a feature set suitable for
programming in the large.  As a multi-paradigm language, Logtalk includes support for
both prototypes and classes, protocols (interfaces), categories (components and
hot-patching), event-driven programming, coinduction, lambda expressions, and
high-level multi-threading programming.")
    ;; Most are under Apache License 2.0, some contributed libraries and ports are
    ;; under other licenses.
    (license (list license:asl2.0
                   license:artistic2.0
                   license:bsd-2
                   license:bsd-3
                   license:cc0
                   license:expat
                   license:gpl2+
                   license:osl2.1))))
