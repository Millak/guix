;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2019, 2020, 2021, 2023, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016, 2018, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Corentin Bocquillon <corentin@nybble.fr>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017–2019, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2023 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2023, 2024 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2024 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024, 2025 David Elsing <david.elsing@posteo.net>
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

(define-module (gnu packages serialization)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages time))

(define-public avro-cpp
  (package
    (name "avro-cpp")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://apache/avro/avro-" version
                    "/avro-src-" version ".tar.gz"))
              (sha256
               (base32 "0ywg7s7m7ngiddcg78hwb34c49yjzal6glcckinvcik2fr9nmg88"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'chdir
                 (lambda _ (chdir "lang/c++")))
               (add-after 'chdir 'fix-dependencies
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("^FetchContent_MakeAvailable\\(fmt\\)")
                      "find_package(fmt REQUIRED)")))))))
    (inputs
     (list boost fmt snappy))
    (home-page "https://avro.apache.org/")
    (synopsis "Data serialization system")
    (description "Apache Avro is a data serialization system.  Avro provides:
@enumerate
@item Rich data structures;
@item a compact, fast, binary data format;
@item a container file, to store persistent data;
@item remote procedure call (RPC); and
@item simple integration with dynamic languages.
@end enumerate

Code generation is not required to read or write data files nor to use or
implement RPC protocols.")
    (license license:asl2.0)))

(define-public cereal
  (package
    (name "cereal")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/USCiLab/cereal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02sd90ynya7wg013zwzjr79fsv4bzqgfg9l2mapd4j38rv06gahx"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~'("-DSKIP_PORTABILITY_TEST=ON"
                            ;; Don't bother building the sandbox examples.
                            "-DSKIP_PERFORMANCE_COMPARISON=ON"
                            "-DBUILD_SANDBOX=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-doc
            (lambda _
              (let ((doc (string-append #$output "/share/doc/html")))
                (invoke "make" "doc")
                (mkdir-p doc)
                (copy-recursively "doc/html" doc)))))))
    (native-inputs
     (list doxygen gcc-10))
    (home-page "https://uscilab.github.io/cereal/")
    (synopsis "C++11 library for serialization")
    (description
     "Cereal is a header-only C++11 serialization library.  Cereal takes
arbitrary data types and reversibly turns them into different representations,
such as compact binary encodings, XML, or JSON.")
    ;; Note: Cereal bundles forked versions of rapidxml and rapidjson
    ;; (see include/cereal/external/), so list their licenses too.
    (license (list license:bsd-3        ;Cereal itself
                   ;; The bundled RapidXML is dual Boost/Expat (users choice).
                   ;; RapidJSON is Expat licensed, and further bundles a
                   ;; stdint.h with BSD-3.
                   license:boost1.0 license:expat
                   ;; Finally, include/cereal/external/base64.hpp has a
                   ;; home-grown BSD-like license.
                   (license:non-copyleft
                    "file://include/cereal/external/LICENSE")))))

(define-public msgpack-c
  (package
    (name "msgpack-c")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/msgpack/msgpack-c/releases/download/"
         "c-" version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1fmf08babfsjq5qkgw034wk2nw6mayxp1qlkm7h55p2jqvigam1n"))
       (snippet
        '(let ((p (open-file "msgpack-c.pc.in" "a")))
           (display "Requires: zlib\n" p)
           (close-output-port p)))))
    (build-system cmake-build-system)
    (arguments (list #:configure-flags #~(list "-DMSGPACK_BUILD_TESTS=ON")))
    (native-inputs (list googletest))
    (propagated-inputs (list zlib))  ;zbuffer.h includes zlib.h
    (home-page "https://www.msgpack.org")
    (synopsis "Binary serialization library")
    (description "Msgpack is a library for C that implements binary
serialization.")
    (license license:boost1.0)))

(define-public msgpack-cxx
  (package
    (inherit msgpack-c)
    (name "msgpack-cxx")
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/msgpack/msgpack-c/releases/download/"
         "cpp-" version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1rrrf3nskcv994z3pbq6a5z2021piz118rccmm1y7zlf7klygv93"))))
    (build-system cmake-build-system)
    (propagated-inputs (list boost zlib)) ;included in headers
    (description "Msgpack is a library for C++ that implements binary
serialization.")))

;;; The msgpack package was split into msgpack-c and msgpack-cxx starting from
;;; version 4.0.0.
(define-deprecated-package msgpack
  msgpack-c)

(define-public msgpack-3
  (package
    (inherit msgpack-c)
    (name "msgpack")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/msgpack/msgpack-c/releases/download/"
         "cpp-" version "/msgpack-" version ".tar.gz"))
       (snippet
        '(let ((p (open-file "msgpack.pc.in" "a")))
           (display
            (string-append "Requires: " "zlib" "\n") p)
           (close-output-port p)))
       (sha256
        (base32 "0yzhq50ijvwrfkr97knhvn54lj3f4hr3zy39yq8wpf6xll94s4bf"))))
    (native-inputs (list googletest-1.8))
    (description "Msgpack is a library for C/C++ that implements binary
serialization.  This is the legacy version that predates the split into C and
C++ specific packages.")))

(define-public libmpack
  (package
    (name "libmpack")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tarruda/libmpack")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rai5djdkjz7bsn025k5489in7r1amagw1pib0z4qns6b52kiar2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list libtool))
    (home-page "https://github.com/tarruda/libmpack")
    (synopsis "Small binary serialization library")
    (description "Libmpack is a small binary serialization and RPC library
that implements both the msgpack and msgpack-rpc specifications.")
    (license license:expat)))

(define-public lua-libmpack
  (package (inherit libmpack)
    (name "lua-libmpack")
    (version "1.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libmpack/libmpack-lua")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ijvzgq5hvib03w5rghv31wi7byamwg7qdx5pawvhvnflaii8ivw"))))
    (build-system gnu-build-system)
    (arguments
     `(;; FIXME: tests require "busted", which is not yet available in Guix.
       #:tests? #f
       #:test-target "test"
       #:make-flags
       (let* ((lua-version ,(package-version lua))
              (lua-major+minor ,(version-major+minor (package-version lua))))
         (list "CC=gcc"
               "FETCH=echo"  ; don't fetch anything from the web
               "UNTGZ=echo"  ; and don't try to unpack it
               "USE_SYSTEM_LUA=yes"
               (string-append "MPACK_LUA_VERSION=" lua-version)
               (string-append "MPACK_LUA_VERSION_NOPATCH=" lua-major+minor)
               (string-append "PREFIX="
                              (assoc-ref %outputs "out"))
               (string-append "LUA_CMOD_INSTALLDIR="
                              (assoc-ref %outputs "out")
                              "/lib/lua/" lua-major+minor)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'unpack-mpack-sources
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This is broken because mpack-src is not a file, but all
             ;; prerequisites are added to the inputs of the gcc invocation.
             (substitute* "Makefile"
               (("\\$\\(MPACK\\): mpack-src") "$(MPACK): "))
             (copy-recursively (assoc-ref inputs "libmpack")
                               "mpack-src")
             #t)))))
    (inputs
     (list lua))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libmpack" ,(package-source libmpack))))
    (home-page "https://github.com/libmpack/libmpack-lua")
    (synopsis "Lua bindings for the libmpack binary serialization library")))

(define-public lua5.1-libmpack
  (package/inherit lua-libmpack
    (name "lua5.1-libmpack")
    (arguments
     (substitute-keyword-arguments (package-arguments lua-libmpack)
       ((#:make-flags flags)
        `(let* ((lua-version ,(package-version lua-5.1))
                (lua-major+minor ,(version-major+minor (package-version lua-5.1))))
           (list "CC=gcc"
                 "USE_SYSTEM_LUA=yes"
                 (string-append "MPACK_LUA_VERSION=" lua-version)
                 (string-append "MPACK_LUA_VERSION_NOPATCH=" lua-major+minor)
                 (string-append "PREFIX="
                                (assoc-ref %outputs "out"))
                 (string-append "LUA_CMOD_INSTALLDIR="
                                (assoc-ref %outputs "out")
                                "/lib/lua/" lua-major+minor))))))
    (inputs
     `(("lua" ,lua-5.1)))))

(define-public lua5.2-libmpack
  (package/inherit lua-libmpack
    (name "lua5.2-libmpack")
    (arguments
     (substitute-keyword-arguments (package-arguments lua-libmpack)
       ((#:make-flags flags)
        `(let* ((lua-version ,(package-version lua-5.2))
                (lua-major+minor ,(version-major+minor (package-version lua-5.2))))
           (list "CC=gcc"
                 "USE_SYSTEM_LUA=yes"
                 (string-append "MPACK_LUA_VERSION=" lua-version)
                 (string-append "MPACK_LUA_VERSION_NOPATCH=" lua-major+minor)
                 (string-append "PREFIX="
                                (assoc-ref %outputs "out"))
                 (string-append "LUA_CMOD_INSTALLDIR="
                                (assoc-ref %outputs "out")
                                "/lib/lua/" lua-major+minor))))))
    (inputs
     `(("lua" ,lua-5.2)))))

(define-public libcsv
  (package
    (name "libcsv")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libcsv/libcsv/libcsv-"
                                  version "/libcsv-" version ".tar.gz"))
              (sha256
               (base32
                "1r6pxdxrc3vfil1f9ng1dblm82asdqz6hkz7dj4vkkh3p0f47h6r"))))
    (build-system gnu-build-system)
    (home-page "http://sourceforge.net/projects/libcsv/")
    (synopsis "CSV parser and writer library")
    (description
     "This package provides a C library for parsing and writing CSV data.")
    (license license:lgpl2.1+)))

(define-public qtcsv
  (package
    (name "qtcsv")
    (version "1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/iamantony/qtcsv")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c9i93kr7wvpr01i4wixi9mf991nd3k2adg5fy0vxwwlvvc7dgdw"))))
    (build-system qt-build-system)
    (arguments
     (list #:modules '((guix build cmake-build-system)
                       (guix build qt-build-system)
                       ((guix build gnu-build-system) #:prefix gnu:)
                       (guix build utils))
           #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:rest args)
                   (apply (assoc-ref gnu:%standard-phases 'check)
                          #:test-target "tests" args))))))
    (home-page "https://github.com/iamantony/qtcsv")
    (synopsis "Library for reading and writing CSV files in Qt")
    (description
     "@code{qtcsv} is a library for reading and writing
@acronym{CSV,comma-seperated values} files in Qt.")
    (license license:expat)))

(define-public libscfg
  (package
    (name "libscfg")
    (version "0.1.1")
    (home-page "https://git.sr.ht/~emersion/libscfg")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~emersion/libscfg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b1ps7wba4anm3x1yndnd730dwl6rdz3zwjgxmsyq31fnjrjydv9"))))
    (build-system meson-build-system)
    (synopsis "Scfg library written in C")
    (description
     "This package provides a C library for to parse
@uref{https://git.sr.ht/~emersion/scfg, scfg}, a simple configuration file
format with one directive per line.")
    (license license:expat)))

(define-public libyaml
  (package
    (name "libyaml")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pyyaml.org/download/libyaml/yaml-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1x4fcw13r3lqy8ndydr3ili87wicplw2awbcv6r21qgyfndswhn6"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (home-page "https://pyyaml.org/wiki/LibYAML")
    (synopsis "YAML 1.1 parser and emitter written in C")
    (description
     "LibYAML is a YAML 1.1 parser and emitter written in C.")
    (license license:expat)))

(define-public libyaml+static
  (package
    (inherit libyaml)
    (name "libyaml+static")
    (arguments
     '(#:configure-flags '("--enable-static")))))

(define-public libcyaml
  (package
    (name "libcyaml")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tlsa/libcyaml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "libcyaml-libyaml-compat.patch"))
       (sha256
        (base32 "0gvf3h8r8300wdwfjgxw3nzlj7w14q63m67p8wdm5fvpha017n4y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (inputs
     (list libyaml))
    (native-inputs
     (list pkg-config))
    (synopsis "C library for reading and writing YAML")
    (description
     "LibCYAML is a C library written in ISO C11 for reading and writing
structured YAML documents.  The fundamental idea behind CYAML is to allow
applications to construct schemas which describe both the permissible
structure of the YAML documents to read/write, and the C data structure(s)
in which the loaded data is arranged in memory.")
    (home-page "https://github.com/tlsa/libcyaml")
    (license license:isc)))

(define-public libfyaml
  (package
    (name "libfyaml")
    (version "0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pantoniou/libfyaml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "002g0grddfi5y42lq06zj8266rf7h27wq76sr598ad5pxllx3y3g"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config))
    (home-page "https://github.com/pantoniou/libfyaml")
    (synopsis "YAML version 1.2 and JSON parser and writer")
    (description "Libfyaml is a YAML parser and emitter, supporting the latest
YAML spec and passing the full YAML testsuite.  It is designed to be very
efficient, avoiding copies of data, and has no artificial limits like the 1024
character limit for implicit keys.")
    (license license:expat)))

(define-public yaml-cpp
  (let ((commit "2f86d13775d119edbb69af52e5f566fd65c6953b")
        (revision "0"))
    (package
      (name "yaml-cpp")
      (version (git-version "0.8.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jbeder/yaml-cpp")
               (commit commit)))
         (modules '((guix build utils)))
         (snippet #~(delete-file-recursively "test/googletest-1.13.0"))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12ncx2hlsl5vp9yfja6myxalg85j0kgxwbargn37yiwi8rn17m8s"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DYAML_BUILD_SHARED_LIBS=ON"
                             "-DYAML_CPP_BUILD_TESTS=ON"
                             "-DYAML_USE_SYSTEM_GTEST=ON")))
      (native-inputs
       (list python))
      (inputs (list googletest))
      (home-page "https://github.com/jbeder/yaml-cpp")
      (synopsis "YAML parser and emitter in C++")
      (description "YAML parser and emitter in C++ matching the YAML 1.2 spec.")
      (license license:bsd-3))))

(define-public jsoncpp
  (package
    (name "jsoncpp")
    (version "1.9.6")
    (home-page "https://github.com/open-source-parsers/jsoncpp")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "070xg4i52z3yv5b9bw5k95qskw0daivh0njka87mzj0d3zf1qsyy"))))
    (build-system meson-build-system)
    (synopsis "C++ library for interacting with JSON")
    (description "JsonCpp is a C++ library that allows manipulating JSON values,
including serialization and deserialization to and from strings.  It can also
preserve existing comment in unserialization/serialization steps, making
it a convenient format to store user input files.")
    (license license:expat)))

(define-public jsoncpp/pinned
  (hidden-package
   ;; Version that rarely changes, depended on by CMake.
   (package/inherit jsoncpp
     (version "1.9.6")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference (url (package-home-page jsoncpp)) (commit version)))
        (file-name (git-file-name (package-name jsoncpp) version))
        (sha256
         (base32
          "070xg4i52z3yv5b9bw5k95qskw0daivh0njka87mzj0d3zf1qsyy")))))))

(define-public json.sh
  (let ((commit "0d5e5c77365f63809bf6e77ef44a1f34b0e05840") ;no releases
        (revision "1"))
    (package
      (name "json.sh")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dominictarr/JSON.sh")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14lxvp5xbdk0dcwkjbdp098z1108j8z48zaibndh4i731kkcz43i"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '(("JSON.sh" "bin/"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'install 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests? (invoke "./all-tests.sh"))
               #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (wrap-program (string-append (assoc-ref outputs "out") "/bin/JSON.sh")
                 `("PATH" ":" prefix
                   (,(string-join
                     (map (lambda (in) (string-append (assoc-ref inputs in) "/bin"))
                          '("grep" "sed"))
                      ":"))))
                #t)))))
      (inputs
       (list bash-minimal grep sed))
      (synopsis "Pipeable JSON parser written in shell")
      (description
        "This package provides a JSON parser written in shell, compatible with
ash, Bash, Dash and Zsh.  Pipe JSON to it, and it traverses the JSON objects
and prints out the path to the current object (as a JSON array) and then the
object, without whitespace.")
      (home-page "https://github.com/dominictarr/JSON.sh")
      (license (list license:expat license:asl2.0))))) ;dual-licensed

(define-public ckdl
  (package
    (name "ckdl")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tjol/ckdl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zdpil61lm141lcxmfrg7jvfjp2n98v5q2rfqzm4wiqcdprgmasv"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON"
                                     "-DDOWNLOAD_TEST_DATA=OFF")))
    (home-page "https://ckdl.readthedocs.io/en/latest/")
    (synopsis "C library for parsing and emitting KDL")
    (description "@samp{ckdl} is a C library for parsing and emitting KDL.
This package also provides @samp{kdlpp}, a C++20 wrapper around @samp{ckdl}.")
    (license license:expat)))

(define-public capnproto
  (package
    (name "capnproto")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://capnproto.org/capnproto-c++-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1vs2zpk4l55hb4lq4kldbwdqcjnwm1jblhcaqxmii9dxrd7f807d"))
              (patches (search-patches "capnproto-fix-test.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-tmp-for-temporary-files
           (lambda _
             ;; Use /tmp for temporary files, as the default /var/tmp directory
             ;; doesn't exist.
             (substitute* "src/kj/filesystem-disk-test.c++"
               (("VAR\\_TMP \"/var/tmp\"")
                "VAR_TMP \"/tmp\"")))))))
    (home-page "https://capnproto.org")
    (synopsis "Capability-based RPC and serialization system")
    (description
     "Cap'n Proto is a very fast data interchange format and capability-based
RPC system.  Think JSON, except binary.  Or think Protocol Buffers, except faster.")
    (license license:expat)))

(define-public python-msgspec
  (package
    (name "python-msgspec")
    (version "0.18.6")
    (source (origin
              ;; There are no tests in the PyPI tarball.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jcrist/msgspec")
                    (commit version)))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               ;; Delete autogenerated file, regenerate in a phase.
               '(begin
                  (delete-file "msgspec/atof_consts.h")))
              (sha256
               (base32
                "0akq8lc3547i0j67dpnq1si3dvdc51r4f66dka2h7mq6c4zxq3fn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Disable only one failing test.
      ;;
      ;; AssertionError: msgspec/structs.pyi:7: error: Positional-only
      ;; parameters are only supported in Python 3.8 and greater
      #:test-flags #~(list "-k" "not test_mypy")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'versioneer
                     (lambda _
                       (invoke "versioneer" "install")
                       (substitute* "setup.py"
                         (("version=versioneer.get_version\\(),")
                          (format #f "version=~s," #$version)))))
                   (add-after 'versioneer 'atof-consts
                     (lambda _
                       (with-directory-excursion "scripts"
                         ;; Regenerate the autogenerated file.
                         (invoke "python" "generate_atof_consts.py")))))))
    (native-inputs (list python-attrs
                         python-gcovr
                         python-msgpack
                         python-mypy
                         python-pytest
                         python-setuptools-scm
                         python-versioneer
                         python-setuptools
                         python-wheel))
    (propagated-inputs (list python-pyyaml python-tomli python-tomli-w))
    (home-page "https://jcristharif.com/msgspec/")
    (synopsis "Fast serialization/validation library")
    (description "@code{msgspec} is a fast serialization and validation
library, with builtin support for JSON, MessagePack, YAML, and TOML.  It
includes the following features:

@itemize
@item High performance encoders/decoders for common protocols.
@item Support for a wide variety of Python types.
@item Zero-cost schema validation using familiar Python type annotations.
@item A speedy Struct type for representing structured data.
@end itemize")
    ;; XXX: It might support more architectures but GitHub Actions listed only
    ;; two right now. Try to build for the rest supported by Guix.  See:
    ;; https://github.com/jcrist/msgspec/blob/main/.github/workflows/ci.yml#L83
    (supported-systems (list "x86_64-linux" "aarch64-linux"))
    (license license:bsd-3)))

(define-public python-ruamel.yaml
  (package
    ;; TODO: Fix package name.
    (name "python-ruamel.yaml")
    (version "0.18.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruamel.yaml" version))
       (sha256
        (base32
         "1dsj3zcb5p73p2xpimdig45hrcvjgnzyyc37jcaxyr63mrmbf9vj"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))     ;no tests in PyPI archive
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-ruamel.yaml.clib))
    (home-page "https://sourceforge.net/projects/ruamel-yaml/")
    (synopsis "YAML 1.2 parser/emitter")
    (description
     "This package provides YAML parser/emitter that supports roundtrip
preservation of comments, seq/map flow style, and map key order.  It
is a derivative of Kirill Simonov’s PyYAML 3.11.  It supports YAML 1.2
and has round-trip loaders and dumpers.  It supports comments.  Block
style and key ordering are kept, so you can diff the source.")
    (license license:expat)))

(define-public python-ruamel.yaml-0.16
  (package
    (inherit python-ruamel.yaml)
    (version "0.16.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruamel.yaml" version))
       (sha256
        (base32
         "0hm9yg785f46bkrgqknd6fdvmkby9dpzjnm0b63qf0i748acaj5v"))))
    (arguments
     `(;; TODO: Tests require packaging "ruamel.std.pathlib".
       #:tests? #f))))

(define-public python-ruamel.yaml.clib
  (package
    ;; TODO: Fix the name
    (name "python-ruamel.yaml.clib")
    (version "0.2.12")
    (source
      (origin
        ;; pypi release code has cythonized code without corresponding source.
        (method hg-fetch)
        (uri (hg-reference
               (url "http://hg.code.sf.net/p/ruamel-yaml-clib/code")
               (changeset version)))
        (file-name (hg-file-name name version))
        (sha256
         (base32
          "12ixp46706pl911f6i4wmik8x0j9vnxy2cqx65ixbdl9cnvqva2l"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file "_ruamel_yaml.c")))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f  ; This package is split from python-ruamel.yaml and
                    ; depends on modules from it for the test suite.
       #:phases
       (modify-phases %standard-phases
         (delete 'sanity-check) ; Depends on python-ruamel.yaml
         (add-after 'ensure-no-cythonized-files 'cythonize-code
           (lambda _
             (invoke "cython" "_ruamel_yaml.pyx"))))))
    (native-inputs
     (list python-cython python-setuptools))
    (home-page "https://sourceforge.net/p/ruamel-yaml-clib/code/ci/default/tree")
    (synopsis "C version of reader, parser and emitter for ruamel.yaml")
    (description
     "This package provides a C version of the reader, parser and emitter for
@code{ruamel.yaml} derived from libyaml.")
    (license license:expat)))

(define-public python-strictyaml
  (package
    (name "python-strictyaml")
    (version "1.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "strictyaml" version))
       (sha256
        (base32 "01y4hrakk1psdj6ir5k70apqkjjipvja0c40pbfvahmbzjjm9y12"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Unbundle ruamel.
            (delete-file-recursively "strictyaml/ruamel")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Tests require running Docker and an external test framework,
      ;; see: <https://github.com/crdoconnor/strictyaml/blob/1.7.3/key.sh>
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-bundled-ruamel
            (lambda _
              (substitute* "pyproject.toml"
                ((", \"strictyaml.ruamel\"")
                 "")
                (("\"python-dateutil>=2.6.0\"" dateutils)
                 (string-append dateutils ", \"ruamel.yaml\"")))
              (substitute* "strictyaml/parser.py"
                (("from strictyaml import ruamel")
                 "import ruamel.yaml"))
              (substitute* (find-files "." "\\.py$")
                (("from strictyaml.ruamel")
                 "from ruamel.yaml")))))))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-dateutil
           python-ruamel.yaml-0.16))
    (home-page "https://hitchdev.com/strictyaml/")
    (synopsis "Strict, typed YAML parser")
    (description "StrictYAML is a type-safe YAML parser that parses and
validates a restricted subset of the YAML specification.")
    (license license:expat)))

(define-public python-cbor
  (package
    (name "python-cbor")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cbor" version))
       (sha256
        (base32
         "1dmv163cnslyqccrybkxn0c9s1jk1mmafmgxv75iamnz5lk5l8hk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests are broken upstrea, see <https://github.com/brianolson/cbor_py/issues/6>.
      #:tests? #f))
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/brianolson/cbor_py")
    (synopsis "Implementation of the Concise Binary Object Representation")
    (description
     "Python-cbor provides an implementation of the Concise Binary Object
Representation (@dfn{CBOR}).  CBOR is comparable to JSON, has a superset of
JSON's ability, but serializes to a binary format which is smaller and faster
to generate and parse.  The two primary functions are @code{cbor.loads} and
@code{cbor.dumps}.")
    (license license:asl2.0)))

(define-public flatbuffers
  (package
    (name "flatbuffers")
    (version "24.12.23")               ; Keep in sync with python-flatbuffers.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/flatbuffers")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "flatbuffers" version))
              (sha256
               (base32
                "01g64kmjw8dfhj12j5fgyx70avix9p1ml4w25lm726dixmpq9gp8"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "Release"
       #:configure-flags
       (list "-DFLATBUFFERS_BUILD_SHAREDLIB=ON"
             (string-append "-DCMAKE_INSTALL_LIBDIR="
                            (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _ (for-each make-file-writable (find-files ".")))))))
    (home-page "https://google.github.io/flatbuffers/")
    (synopsis "Memory-efficient serialization library")
    (description "FlatBuffers is a cross-platform serialization library for C++,
C#, C, Go, Java, JavaScript, PHP, and Python.  It was originally created for
game development and other performance-critical applications.")
    (license license:asl2.0)))

(define-public flatbuffers-23.5
  (hidden-package
   (package
     (inherit flatbuffers)
     ;; needed explicitly by onnxruntime and tensorflow-lite@2.14.0
     (version "23.5.26")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/google/flatbuffers")
               (commit (string-append "v" version))))
        (file-name (git-file-name "flatbuffers" version))
        (sha256
         (base32 "0cd12dvkzqdafz46q4302mzgpzbz589zmmiga7bq07f2sqy4vrvv"))))
     (arguments
      (substitute-keyword-arguments (package-arguments flatbuffers)
        ((#:configure-flags flags #~'())
         #~(append #$flags '("-DCMAKE_POSITION_INDEPENDENT_CODE=ON"))))))))

(define-public go-github-com-google-flatbuffers
  (package/inherit flatbuffers
    (name "go-github-com-google-flatbuffers")
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/google/flatbuffers"))))

(define-public python-flatbuffers
  (package
    (name "python-flatbuffers")
    (version (package-version flatbuffers))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flatbuffers" version))
       (sha256
        (base32 "1b1dqa2hgcx2lf3g51amzd0a56vhl2vx061bvmwdpdp9dayb0419"))))
    (build-system pyproject-build-system)
    ;; XXX: No tests on Pypi.  Even in the upstream repository, it's unclear
    ;; if there is a way to run tests for the python library only.
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools))
    (home-page "https://flatbuffers.dev")
    (synopsis "FlatBuffers serialization for Python")
    (description
     "This package provides the @code{FlatBuffers} serialization format for
Python.")
    (license license:asl2.0)))

(define-public python-feather-format
  (package
    (name "python-feather-format")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "feather-format" version))
        (sha256
         (base32
          "00w9hwz7sj3fkdjc378r066vdy6lpxmn6vfac3qx956k8lvpxxj5"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pandas python-pyarrow))
    (home-page "https://github.com/wesm/feather")
    (synopsis "Python wrapper to the Feather file format")
    (description "This package provides a Python wrapper library to the
Apache Arrow-based Feather binary columnar serialization data frame format.")
    (license license:asl2.0)))

(define-public libnop
  (let ((commit "35e800d81f28c632956c5a592e3cbe8085ecd430")
        (revision "0"))
    (package
      (name "libnop")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/libnop")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qqbaljq54qiq0dky9nj47igfcs065ry526jg9a0aafbfl9krpy2"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:test-target "test"
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "out/test"))))
            (replace 'install
              (lambda _
                (copy-recursively
                 "include" (string-append #$output "/include")))))))
      (native-inputs (list googletest))
      (home-page "https://github.com/google/libnop")
      (synopsis "C++ Native Object Protocols")
      (description "@code{libnop} is a header-only library for serializing and
deserializing C++ data types without external code generators or runtime
support libraries.")
      (license license:asl2.0))))

(define-public valijson
  (package
    (name "valijson")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tristanpenman/valijson")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ai6bj6mxd12mpvf1xwlad2vic5lsqy44adilp17wa7dq275vwf2"))))
    (build-system cmake-build-system)
    ;; The test suite is disabled as it requires Boost, Qt 5, Poco as well as
    ;; other bundled libraries.
    (arguments (list #:tests? #f))
    (home-page "https://github.com/tristanpenman/valijson")
    (synopsis "JSON schema validation C++ header-only library")
    (description "Valijson is a header-only JSON Schema validation library for
C++11.  It provides a simple validation API that allows loading JSON Schemas,
and validate documents loaded by one of several supported parser libraries.")
    (license license:bsd-2)))

(define-public libvarlink
  (package
    (name "libvarlink")
    (version "24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/varlink/libvarlink")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "098jw9h48p2py6lwxyjrlzvv9zqvqgfsxc4pddfsviab65n9n5gw"))))
    (build-system meson-build-system)
    (native-inputs
     (list python-minimal
           glibc-utf8-locales)) ;needed for unit tests
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-/bin/sh
                     (lambda _
                       (substitute* "lib/meson.build"
                         (("/bin/sh")
                          (which "sh")))))
                   (add-after 'unpack 'patch-/usr/bin/env
                     (lambda _
                       (substitute* "varlink-wrapper.py"
                         (("/usr/bin/env")
                          (which "env"))))))))
    (home-page "https://varlink.org/")
    (synopsis "Varlink C IPC library and command line tool")
    (description
     "This package provides the C implementation of the varlink
interface description protocol and its associated command line tool")
    (license license:asl2.0)))

