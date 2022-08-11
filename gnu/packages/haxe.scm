;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (gnu packages haxe)
  #:use-module (gnu packages)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system dune)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public neko
  (package
    (name "neko")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HaxeFoundation/neko")
             (commit (string-append
                      "v"
                      (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xgw646pghsjjbzd8qlaq17vq96swlrazpivrvyrhdj36vb3sci3"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'prefix
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("\\\\\\$ORIGIN")
                      (string-append #$output "/lib"))))))))
    (inputs (list apr
                  apr-util
                  gtk+-2
                  httpd
                  libgc
                  mbedtls-apache
                  mysql
                  openssl
                  pcre
                  sqlite
                  zlib))
    (native-inputs (list git pkg-config)) ; git for source_archive and applying patch
    (home-page "https://nekovm.org/")
    (synopsis "High-level dynamically typed programming language and virtual
machine")
    (description
     "The Neko programming language is a high-level dynamically typed
programming language.  It can be used as an embedded scripting language.  It
has been designed to provide a common runtime for several different languages.
You can easily extend the language with C libraries.  You can also write
generators from your own language to Neko and then use the Neko Runtime to
compile, run, and access existing libraries.

Neko has a compiler and a virtual machine.  The Virtual Machine is both very
lightweight and well optimized, so it can run very quickly.  The VM can be
easily embedded into any application and your libraries can be accessed using
the C foreign function interface.

You can use the compiler as standalone command line executable separate from
the VM, or as a Neko library to perform compile-and-run funtions for
interactive languages.")
    (license license:expat)))

(define haxelib-src
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/HaxeFoundation/haxelib")
          ;; This should match the haxelib submodule in haxe.
          (commit "4b27f91d8a4ff279d9903091680fee2c93a0d574")
          ;; This repo includes some Haxe libs as well.
          (recursive? #t)))
    (sha256
     (base32
      "0mwrm6gxgclwziiprfiswmjbz6z3dnvdwl8gq3gaym18pvx4p3ny"))))

(define-public haxe
  (package
    (name "haxe")
    (version "4.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HaxeFoundation/haxe")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pl8vpyb7gl2yqjg85yc4zxq9c3ipvw4yrrpliaxs25ynrj3l51n"))))
    (build-system dune-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; Needs the haxelib sources for haxelib client
               (add-after 'unpack 'copy-haxelib-src
                 (lambda _
                   (copy-recursively #$haxelib-src
                                     "extra/haxelib_src")))
               ;; Change the default directory for the haxelib package
               ;; manager to be something writeable for a user.
               (add-after 'copy-haxelib-src 'change-default-dir
                 (lambda _
                   (substitute* "extra/haxelib_src/src/haxelib/client/Main.hx"
                     (("'/usr/lib/haxe/\\$REPNAME'")
                      "Path.addTrailingSlash( getHomePath() ) + '.haxe/$REPNAME'"))))
               (add-after 'unpack 'prefix
                 (lambda _
                   (substitute* "Makefile"
                     (("/usr/local")
                      (string-append #$output)))))
               ;; Haxe uses a straight forward make, dune runtest, and make
               ;; install process.
               (replace 'build
                 (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
                   (invoke "make" "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "dune" "runtest"))))
               (replace 'install
                 (lambda _
                   (invoke "make" "install"))))))
    (inputs (list libuv
                  mbedtls-apache
                  neko
                  ocaml-extlib
                  ocaml-luv
                  ocaml-ptmap
                  ocaml-sedlex
                  ocaml-sha
                  ocaml-xml-light
                  pcre
                  zlib))
    (native-inputs (list ocaml-findlib camlp5))
    (home-page "https://haxe.org/")
    (synopsis "Multi-target universal programming language")
    (description
     "Haxe is a toolkit based on a modern, high level, static-typed
programming language, a cross-compiler, a complete cross-platform standard
library and ways to access each platform's native capabilities.  This package
includes the compiler and library manager.")
    (license (list license:gpl2+     ; the compiler itself
                   license:expat)))) ; the standard library

(define-public hashlink
  (package
    (name "hashlink")
    (version "1.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HaxeFoundation/hashlink")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i5f1mxpgjcdirx60kxrw0r0y15qh3j16a6fj8mzkq3k7j2hc982"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled libraries (mainly used for Windows build).
           (delete-file-recursively "include")
           #t))))
    (build-system gnu-build-system)
    (arguments
     ;; Looks like there are tests with CMake, but there is confusion if this
     ;; is a supported way to build on Linux.  See, e.g.
     ;; https://github.com/HaxeFoundation/hashlink/issues/397.  Also, the
     ;; CMake build requires running the library manager haxelib in the build
     ;; process for the tests, likely requiring network access.
     ;; TODO: Use cmake-build-system instead and enable tests?
     (list #:tests? #f
           #:make-flags
           ;; The built hdll libraries need help finding libhl.so.
           #~(list (string-append "LIBFLAGS=-Wl,-rpath=" #$output "/lib"))
           #:phases
           #~(modify-phases %standard-phases
               ;; Don't try to build the (removed) bundled libraries pcre,
               ;; minimp3, and mikktspace.  Provide the approriate paths and
               ;; linking options.
               (add-after 'unpack 'use-system-libs
                 (lambda _
                   (substitute* "Makefile"
                     (("\\$\\{PCRE\\}") "")
                     (("-lpthread") "-lpthread -lpcre16")
                     (("include/minimp3")
                      (string-append #$(this-package-input "minimp3") "/include"))
                     (("include/mikktspace ")
                      (string-append #$(this-package-input "mikktspace") "/include "))
                     (("include/mikktspace/mikktspace.o") "")
                     (("-lpng") "-lpng -lmikktspace"))))
               (replace 'configure
                 (lambda* _
                   (setenv "CC" #$(cc-for-target))
                   (setenv "PREFIX" #$output)))
               (replace 'build
                 (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
                   (apply invoke "make" "-j" (if parallel-build?
                                                 (number->string (parallel-job-count))
                                                 "1")
                          make-flags))))))
    (inputs (list glu
                  haxe
                  libjpeg-turbo
                  libpng
                  libuv
                  libvorbis
                  mbedtls-apache
                  mikktspace
                  minimp3
                  openal
                  pcre
                  sdl2
                  sqlite
                  zlib))
    (native-inputs (list pkg-config))
    (home-page "https://hashlink.haxe.org/")
    (synopsis "Virtual machine for the Haxe language")
    (description
     "HashLink (HL) is a virtual machine for the Haxe language.  It can run
bytecode produced by the Haxe compiler, or converted to C by HL.  The HashLink runtime
includes the following features:
@itemize
@item Fully compatible with the Haxe specification
@item Support file I/O, regular expressions, network, etc.
@item Unicode strings by default
@item Mark-and-not-sweep Garbage Collector
@item x86 and x86-64 HL/C compilation
@item x86 and x86-64 HL/JIT compilation
@end itemize

While the standard HL runtime provides support for Haxe standard library,
HashLink also provides several libraries that can optionally be used to build
HL-specific applications.  This includes the FMT library for compression and
image support and SDL for mouse, keyboard, and game controller support,
OpenGL, and more.")
    (license license:expat)))
