;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020, 2023 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021, 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Franck Pérignon <franck.perignon@univ-grenoble-alpes.fr>
;;; Copyright © 2021 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2025 David Elsing <david.elsing@posteo.net>
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

(define-module (gnu packages boost)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages mpi))

(define (version-with-underscores version)
  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version))

(define (boost-patch name version hash)
  (origin
    (method url-fetch)
    (uri (string-append "https://www.boost.org/patches/"
                        (version-with-underscores version) "/" name))
    (file-name (string-append "boost-" name))
    (sha256 (base32 hash))))

(define-public boost
  (package
    (name "boost")
    (version "1.89.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archives.boost.io/release/"
                                  version "/source/boost_"
                                  (version-with-underscores version) ".tar.bz2"))
              (sha256
               (base32
                "0yhcb8dn7g5k9dfr54j99wpd4qwk59g1lpp8z0ag7d114si3z8w5"))))
    (build-system gnu-build-system)
    (inputs
     (append
      (list icu4c zlib)
      (if (%current-target-system)
          '()
          (list python-minimal-wrapper))))
    (native-inputs
     (list perl tcsh))
    (arguments
     (list
      #:imported-modules `((guix build python-build-system)
                           ,@%default-gnu-imported-modules)
      #:modules `(((guix build python-build-system) #:select (python-version))
                  ,@%default-gnu-modules)
      #:tests? #f
      #:configure-flags
      #~(let ((icu (dirname (dirname (search-input-file
                                      %build-inputs "bin/uconv")))))
          (list
           ;; Auto-detection looks for ICU only in traditional
           ;; install locations.
           (string-append "--with-icu=" icu)
           ;; Ditto for Python.
           #$@(if (%current-target-system)
                  #~()
                  #~((let ((python (dirname (dirname (search-input-file
                                                      %build-inputs
                                                      "bin/python")))))
                       (string-append "--with-python-root=" python)
                       (string-append "--with-python=" python
                                      "/bin/python")
                       (string-append "--with-python-version="
                                      (python-version python)))))
           "--with-toolset=gcc"))
      #:make-flags
      #~(list "threading=multi" "link=shared"

              ;; Set the RUNPATH to $libdir so that the libs find each other.
              (string-append "linkflags=-Wl,-rpath="
                             #$output "/lib")
              #$@(if (%current-target-system)
                     #~("--user-config=user-config.jam"
                        ;; Python is not supported when cross-compiling.
                        "--without-python"
                        "binary-format=elf"
                        "target-os=linux"
                        #$@(cond
                            ((string-prefix? "x86_64" (%current-target-system))
                             #~()) ; Implies boost.stacktrace.from_exception=on
                            ;;; Note: With llvm's libc++, enabling that for
                            ;;; non-x86_64 non-mingw32 would be a bad idea.
                            ;;; libc++'s backtrace is not thread-safe and
                            ;;; would leak then.
                            ;;;
                            ;;; We disable it here completely.  Alternatively,
                            ;;; we could disable it only if the user used
                            ;;; (package-with-c-toolchain ... clang) or
                            ;;; otherwise has libc++ in their dependencies.
                            ;;;
                            ;;; In the latter case, we would have to set
                            ;;; BOOST_STACKTRACE_LIBCXX_RUNTIME_MAY_CAUSE_MEMORY_LEAK
                            ;;; and that seems ill-advised (if a future
                            ;;; update broke it in other ways, we would
                            ;;; be blind to it).
                            ;;;
                            ;;; See also:
                            ;;; <https://codeberg.org/guix/guix/issues/4541>.
                            ((string-prefix? "arm" (%current-target-system))
                             #~("abi=aapcs"
                                "address-model=32"
                                "architecture=arm"
                                ;; See also:
                                ;; <https://codeberg.org/guix/guix/issues/4541>.
                                "boost.stacktrace.from_exception=off"))
                            ((string-prefix? "aarch64" (%current-target-system))
                             #~("abi=aapcs"
                                "address-model=64"
                                "architecture=arm"
                                ;; See also:
                                ;; <https://codeberg.org/guix/guix/issues/4541>.
                                "boost.stacktrace.from_exception=off"))
                            (else
                             ;; See also:
                             ;; <https://codeberg.org/guix/guix/issues/4541>.
                             #~("boost.stacktrace.from_exception=off"))))
                     ;; Not cross-compiling.
                     #~(#$@(cond
                         ((string-suffix? "mingw32" (%current-system))
                          #~()) ; Implies boost.stacktrace.from_exception=on
                         ((string-prefix? "x86_64" (%current-system))
                          #~()) ; Implies boost.stacktrace.from_exception=on
                         (else
                          ;; See also:
                          ;; <https://codeberg.org/guix/guix/issues/4541>.
                          #~("boost.stacktrace.from_exception=off"))))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-shells
            (lambda _
              (substitute* '("libs/config/configure"
                             "libs/spirit/classic/phoenix/test/runtest.sh"
                             "tools/build/src/engine/execunix.cpp")
                (("/bin/sh") (which "sh")))))
          (delete 'bootstrap)
          (replace 'configure
            (lambda* (#:key (configure-flags ''()) #:allow-other-keys)
              (setenv "SHELL" (which "sh"))
              (setenv "CONFIG_SHELL" (which "sh"))

              #$@(if (%current-target-system)
                     #~((call-with-output-file "user-config.jam"
                          (lambda (port)
                            (format port
                                    "using gcc : cross : ~a-c++ ;"
                                    #$(%current-target-system)))))
                     #~())

              (apply invoke "./bootstrap.sh"
                     (string-append "--prefix=" #$output)
                     configure-flags)))
          (replace 'build
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "./b2"
                     (format #f "-j~a" (parallel-job-count))
                     make-flags)))
          (replace 'install
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "./b2" "install" make-flags)))
          #$@(if (%current-target-system)
                 #~()
                 #~((add-after 'install 'provide-libboost_python
                      (lambda* (#:key make-flags inputs outputs #:allow-other-keys)
                        (let* ((static? (member "link=static" make-flags))
                               (libext (if static? ".a" ".so"))
                               (python (dirname (dirname (search-input-file
                                                          inputs "bin/python"))))
                               (python-version (python-version python))
                               (libboost_pythonNN
                                (string-append "libboost_python"
                                               (string-join (string-split
                                                             python-version #\.)
                                                            "")
                                               libext)))
                          (with-directory-excursion (string-append #$output "/lib")
                            (symlink libboost_pythonNN
                                     (string-append "libboost_python" libext))
                            ;; Some packages only look for the major version.
                            (symlink libboost_pythonNN
                                     (string-append "libboost_python"
                                                    (string-take python-version 1)
                                                    libext)))))))))))

    (home-page "https://www.boost.org")
    (synopsis "Peer-reviewed portable C++ source libraries")
    (description
     "A collection of libraries intended to be widely useful, and usable
across a broad spectrum of applications.")
    (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt"
                                "Some components have other similar licences."))))

(define-public boost-1.83
  (hidden-package (package (inherit boost)
    (name "boost")
    (version "1.83.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archives.boost.io/release/"
                                  version "/source/boost_"
                                  (version-with-underscores version) ".tar.bz2"))
              (patches
                 (list (boost-patch
                        "0001-unordered-fix-copy-assign.patch" version
                        "09j61m5xh7099k5na9i43x5rra51znf7vm2nyh89yqpizcll9q66")))
              (patch-flags '("-p2"))
              (sha256
               (base32
                "13iviiwk1srpw9dmiwabkxv56v0pl0zggjp8zxy1419k5zzfsy34"))))
    (arguments
     (substitute-keyword-arguments (package-arguments boost)
      ((#:make-flags _ #f)
       #~(list "threading=multi" "link=shared"
              ;; Set the RUNPATH to $libdir so that the libs find each other.
              (string-append "linkflags=-Wl,-rpath="
                             #$output "/lib")
              #$@(if (%current-target-system)
                     #~("--user-config=user-config.jam"
                        ;; Python is not supported when cross-compiling.
                        "--without-python"
                        "binary-format=elf"
                        "target-os=linux"
                        #$@(cond
                            ((string-prefix? "arm" (%current-target-system))
                             #~("abi=aapcs"
                                "address-model=32"
                                "architecture=arm"))
                            ((string-prefix? "aarch64" (%current-target-system))
                             #~("abi=aapcs"
                                "address-model=64"
                                "architecture=arm"))
                            (else
                             #~())))
                     ;; Not cross-compiling.
                     #~()))))))))

(define-deprecated-package boost-with-python3
  boost)

(define-public boost-static
  (package
    (inherit boost)
    (name "boost-static")
    (arguments
     (substitute-keyword-arguments (package-arguments boost)
       ((#:make-flags flags)
        #~(cons "link=static" (delete "link=shared" #$flags)))))))

(define-public boost-for-mysql
  ;; Older version for MySQL 5.7.23.
  (package
    (inherit boost)
    (version "1.59.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archives.boost.io/release/"
                                  version "/source/boost_"
                                  (version-with-underscores version) ".tar.bz2"))
              (sha256
               (base32
                "1jj1aai5rdmd72g90a3pd8sw9vi32zad46xv5av8fhnr48ir6ykj"))))
    (arguments
     (substitute-keyword-arguments (package-arguments boost)
       ((#:configure-flags _ #~'())
        #~(let ((icu (dirname (dirname (search-input-file
                                        %build-inputs "bin/uconv")))))
            (list
             ;; Auto-detection looks for ICU only in traditional
             ;; install locations.
             (string-append "--with-icu=" icu)
             "--with-toolset=gcc")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'apply-gcc-14-patch
              (lambda _
                (substitute* "tools/build/src/engine/build.sh"
                  (("=gcc")
                   "=\"gcc -Wno-error=implicit-function-declaration\""))
                (substitute* "tools/build/src/engine/build.jam"
                  ((": -pedantic -fno-strict-aliasing" all)
                   (string-append
                    all
                    " -Wno-error=implicit-function-declaration")))))
            (replace 'patch-shells
              (lambda _
                (substitute* (append
                              (find-files "tools/build/src/engine/" "execunix\\.c.*")
                              '("libs/config/configure"
                                "libs/spirit/classic/phoenix/test/runtest.sh"
                                "tools/build/doc/bjam.qbk"
                                "tools/build/src/engine/Jambase"))
                  (("/bin/sh") (which "sh")))))
            (delete 'provide-libboost_python)))
       ((#:make-flags make-flags)
        #~(cons* "--without-python" #$make-flags))))
    (inputs
     (modify-inputs (package-inputs boost)
       (delete "python-minimal-wrapper")))
    (properties '((hidden? . #t)))))

(define-public boost-numpy
  (package
    (inherit boost-1.83)
    (name "boost-numpy")
    (native-inputs
     (modify-inputs (package-native-inputs boost)
       (append python-numpy)))))

(define-public boost-sync
  (let ((commit "e690de2d30e2f1649ff500c9a6f3539814994b1c")
        (version "1.55")
        (revision "2"))
    (package
      (name "boost-sync")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/boostorg/sync")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0473hb15affjq2804xa99ikk4y1gzi46rygd9zhncl28ib7mnn26"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((source (assoc-ref %build-inputs "source")))
             (copy-recursively (string-append source "/include")
                               (string-append %output "/include"))))))
      (home-page "https://github.com/boostorg/sync")
      (synopsis "Boost.Sync library")
      (description "The Boost.Sync library provides mutexes, semaphores, locks
and events and other thread related facilities.  Boost.Sync originated from
Boost.Thread.")
      (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt")))))

(define-public boost-signals2
  ;; Don't use the ‘boost-x.y.z’ tags; they are not immutable upstream.
  (let ((commit "2ecf1b53bc970dd2b5e5d0f36fe8adf5d2181638")
        (revision "0"))
    (package
      (name "boost-signals2")
      (version (git-version (package-version boost) revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/boostorg/signals2")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "101ayw7dz4gdvva2yzyrfad69w4xbvv3man83xwqjbkib3a92ca8"))))
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let ((source (assoc-ref %build-inputs "source")))
              (copy-recursively (string-append source "/include")
                                (string-append %output "/include"))))))
      (home-page "https://github.com/boostorg/signals2")
      (synopsis "Boost.Signals2 library")
      (description "The Boost.Signals2 library is an implementation of a managed
signals and slots system.")
      (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt")))))


(define-public boost-mpi
  (package
    (inherit boost)
    (name "boost-mpi")
    (inputs
     (modify-inputs (package-inputs boost)
       (append openmpi)))
    (arguments
     (substitute-keyword-arguments (package-arguments boost)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'configure 'update-jam
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((output-port (open-file "project-config.jam" "a")))
                  (display "using mpi ;" output-port)
                  (newline output-port)
                  (close output-port))))))))
    (home-page "https://www.boost.org")
    (synopsis "Message Passing Interface (MPI) library for C++")))

(define-public mdds
  (package
    (name "mdds")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/mdds/mdds")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0866020brc1kmiryh7dmhjamnywlsd56ks649hy87283k0p7d3bb"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake))
    (propagated-inputs
      (list boost-1.83)) ; inclusion of header files
    (home-page "https://gitlab.com/mdds/mdds")
    (synopsis "Multi-dimensional C++ data structures and indexing algorithms")
    (description "Mdds (multi-dimensional data structure) provides a
collection of multi-dimensional data structures and indexing algorithms
for C++.  It includes flat segment trees, segment trees, rectangle sets,
point quad trees, multi-type vectors and multi-type matrices.")
    (license license:expat)))
