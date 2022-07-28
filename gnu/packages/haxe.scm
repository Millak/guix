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
