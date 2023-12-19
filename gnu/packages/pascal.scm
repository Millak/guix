;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2017 Kei Kebreau <address@hidden>
;;; Copyright © 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages pascal)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define %fpc-version "3.2.2")
(define %fpc-release-date "2021/05/19")

;;; FIXME: Bootstrap properly; these are prebuilt binaries.
(define fpc-bootstrap-aarch64
  (origin
    (method url-fetch)
    (uri (string-append "mirror://sourceforge/freepascal/Linux/"
                        %fpc-version "/fpc-" %fpc-version ".aarch64-linux.tar"))
    (sha256
     (base32
      "0lalar6qk04acb2j8p6654hlz0yj6zdab046zi82zf5mnvwp155k"))))

(define fpc-bootstrap-i386
  (origin
    (method url-fetch)
    (uri (string-append "mirror://sourceforge/freepascal/Linux/"
                        %fpc-version "/fpc-" %fpc-version ".i386-linux.tar"))
    (sha256
     (base32
      "0n4r85dsr86zlk7r4hbd4nj14sda6rwgdgzxg4gj4q981fn80agn"))))

(define fpc-bootstrap-powerpc
  (origin
    (method url-fetch)
    (uri (string-append "mirror://sourceforge/freepascal/Linux/"
                        %fpc-version "/fpc-" %fpc-version ".powerpc-linux.tar"))
    (sha256
     (base32
      "1zhdypm99bzs5706g4nxwajiadv82jwd87cr300lrivy1rzj5h4a"))))

(define fpc-bootstrap-powerpc64le
  (origin
    (method url-fetch)
    (uri (string-append "mirror://sourceforge/freepascal/Linux/"
                        %fpc-version "/fpc-" %fpc-version ".powerpc64le-linux.tar"))
    (sha256
     (base32
      "12p3lmi1vn7agpw4pipp6ra8r85319sjcvbzh7z6kangmry7vic3"))))

(define fpc-bootstrap-x86_64
  (origin
    (method url-fetch)
    (uri (string-append "mirror://sourceforge/freepascal/Linux/"
                        %fpc-version "/fpc-" %fpc-version ".x86_64-linux.tar"))
    (sha256
     (base32
      "10qywczzz4qlcmmzxb7axnvwniq76ky130vd8iv6ljskll4c7njs"))))

(define-public fpc
  (package
    (name "fpc")
    (version %fpc-version)
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/freepascal/Source/"
                                  version "/fpcbuild-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07qna2pvlpa7j0i2wdixjxpizdvffv51nbr1waczk0xv8cq9kvw5"))
              (patches (search-patches "fpc-reproducibility.patch"
                                       "fpc-glibc-2.34-compat.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (rename-file "install/doc" "install-doc")
                  (rename-file "install/man" "install-man")
                  ;; Contains executables--some of them created by
                  ;; closed-source compilers.
                  (delete-file-recursively "install")
                  (mkdir-p "install")
                  (rename-file "install-doc" "install/doc")
                  (rename-file "install-man" "install/man")
                  (delete-file "fpcsrc/tests/utils/dosbox/exitcode.exe")))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"
                         "powerpc-linux" "powerpc64le-linux"
                         "aarch64-linux"))
    (inputs
     (list expat glibc ncurses zlib))
    (native-inputs
     ;; FPC is built with FPC, so we need bootstrap binaries.
     `(("fpc-binary" ,(match (or (%current-target-system)
                                 (%current-system))
                       ("aarch64-linux" fpc-bootstrap-aarch64)
                       ("i686-linux" fpc-bootstrap-i386)
                       ("powerpc-linux" fpc-bootstrap-powerpc)
                       ("powerpc64le-linux" fpc-bootstrap-powerpc64le)
                       ("x86_64-linux" fpc-bootstrap-x86_64)
                       ;; XXX: Wrong, but innocuous so long
                       ;; `supported-systems' is kept in sync.
                       (_ fpc-bootstrap-x86_64)))))
    (arguments
     `(#:tests? #f ; no tests available
       #:phases
       (let ((fpc-bootstrap-path
              (string-append (getcwd) "/" ,name "-" ,version "/fpc-bin"))
             (arch ,(cond
                      ((target-aarch64?) "aarch64")
                      ((target-x86-32?) "i386")
                      ((target-ppc32?) "powerpc")
                      ((target-ppc64le?) "powerpc64")
                      ((target-x86-64?) "x86_64")
                      (else "unknown"))))
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-bin
             (lambda* (#:key inputs #:allow-other-keys)
               (mkdir-p fpc-bootstrap-path)
               (with-directory-excursion fpc-bootstrap-path
                 (invoke "tar" "xvf" (assoc-ref inputs "fpc-binary")))))
           (add-after 'unpack-bin 'install-bin
             (lambda* (#:key inputs #:allow-other-keys)
               (with-directory-excursion
                 (string-append fpc-bootstrap-path "/fpc-" ,version "."
                                arch "-linux")
                 (let ((binary-tarball
                        (string-append "binary." arch "-linux.tar"))
                       (compiler-tarball
                        (string-append "base." arch "-linux.tar.gz"))
                       (fpcmake-tarball
                        (string-append "utils-fpcm." arch "-linux.tar.gz")))
                   ;; Only the base compiler and fpcmake are needed.
                   (invoke "tar" "xvf" binary-tarball compiler-tarball
                           fpcmake-tarball)
                   (invoke "tar" "xvf" compiler-tarball "-C..")
                   (invoke "tar" "xvf" fpcmake-tarball "-C..")))))
           (add-after 'patch-source-shebangs 'patch-inline-shebangs
             (lambda _
               (substitute* "fpcsrc/compiler/cscript.pas"
                 (("#!/bin/sh") (string-append "#!" (which "sh"))))))
           (add-before 'build 'patch-release-date
             (lambda _                  ; reproducibility
               (substitute* (list "fpcdocs/prog.tex"
                                  "fpcsrc/packages/amunits/examples/sortdemo.pas"
                                  "fpcsrc/packages/libogcfpc/src/ogc/libversion.inc"
                                  "fpcsrc/utils/fpcres/fpcjres.pas"
                                  "fpcsrc/utils/fpcres/fpcres.pas"
                                  "fpcsrc/utils/fpcm/fpcmmain.pp"
                                  "fpcsrc/utils/fpcreslipo/fpcreslipo.pp"
                                  "fpcsrc/compiler/version.pas")
                 (("\\{\\$I(NCLUDE)? %DATE%\\}")
                  (format #f "'~a'" ,%fpc-release-date)))))
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "fpcsrc/compiler/systems/t_linux.pas"
                 ;; Point to the current glibc dynamic linker.
                 (("/lib/ld-linux.so.2")
                  (search-input-file inputs ,(glibc-dynamic-linker)))
                 (("/lib64/ld-linux-x86-64.so.2")
                  (search-input-file inputs ,(glibc-dynamic-linker)))
                 (("/lib/ld.so.1")
                  (search-input-file inputs ,(glibc-dynamic-linker)))
                 (("/lib64/ld64.so.[12]")
                  (search-input-file inputs ,(glibc-dynamic-linker)))
                 (("/lib/ld-linux(-armhf)?.so.3")
                  (search-input-file inputs ,(glibc-dynamic-linker)))
                 (("/lib/ld-linux-aarch64.so.1")
                  (search-input-file inputs ,(glibc-dynamic-linker)))
                 ;; Add glibc to ld's search path.
                 (("if \\(isdll\\) then")
                  (string-append
                   "Add('SEARCH_DIR(\""
                   (assoc-ref inputs "libc") "/lib"
                   "\")');\n"
                   "if (isdll) then")))
               (substitute* "fpcsrc/compiler/options.pas"
                 (("exepath\\+'../etc/'")
                  (string-append "'" (assoc-ref outputs "out") "/etc'")))))
           (replace 'build
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((fpc-bin (string-append fpc-bootstrap-path "/bin"))
                      (fpc (string-append fpc-bin "/fpc"))
                      (fpcmake (string-append fpc-bin "/fpcmake")))
                 ;; The fpc binary needs to run the ppc[arch] binary (which
                 ;; does the actual compiling) in this directory.
                 (setenv "PATH"
                        (string-append (getenv "PATH") ":"
                                       fpc-bootstrap-path
                                       "/lib/fpc/" ,version))
                 (setenv "FPC" fpc)
                 ;; Specify target operating system using "-T" option
                 (invoke fpcmake (string-append "-T" arch "-linux"))
                 (invoke "make" "build" "NOGDB=1"))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                     ;; This is the suffix of the ppc[arch] binary.
                     (suffix ,(cond
                                ((target-aarch64?) "a64")
                                ((target-x86-32?) "386")
                                ((target-ppc32?) "ppc")
                                ((target-ppc64le?) "ppc64")
                                ((target-x86-64?) "x64")
                                (else "")))
                     (ppc (string-append "ppc" suffix)))
                 (invoke "make" "install" "NOGDB=1"
                         (string-append "INSTALL_PREFIX=" out))
                 ;; Remove files that fail RUNPATH validation.
                 ;; TODO: Fix it instead.
                 (delete-file (string-append out "/lib/libpas2jslib.so"))
                 ;; Add a symlink to the ppc[arch] binary so fpc works.
                 (symlink (string-append out "/lib/fpc/" ,version "/" ppc)
                          (string-append out "/bin/" ppc))
                 ;; Install the example configuration file.
                 (mkdir (string-append out "/etc"))
                 (invoke
                   (string-append out "/lib/fpc/" ,version "/samplecfg")
                   (string-append out "/lib/fpc/" ,version)
                   (string-append out "/etc")))))
           (add-after 'install 'wrap
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (fpc (string-append out "/bin/fpc"))
                      (ld (assoc-ref inputs "ld-wrapper"))
                      (glibc (assoc-ref inputs "glibc")))
                 (wrap-program fpc
                   `("PATH" ":" prefix (,(string-append ld "/bin")))
                   `("LIBRARY_PATH" ":" prefix
                     (,(string-append glibc "/lib")))))))))))
    ;; fpc invokes gcc, so make sure LIBRARY_PATH et.al are set.
    ;(native-search-paths (package-native-search-paths gcc))
    (home-page "https://www.freepascal.org")
    (synopsis "The Free Pascal Compiler")
    (description
     "Free Pascal is a professional Object Pascal compiler.  It supports the
Turbo Pascal 7.0, Delphi, and Mac Pascal dialects.  Free Pascal also supports
many useful extensions to the Pascal programming language.")
    ;; The majority of the software included is licensed under the GPLv2
    ;; or later.  For more licensing details, see the appropriate files in
    ;; the install/doc directory of the source distribution.
    (license license:gpl2+)))

(define-public p2c
  (package
    (name "p2c")
    (version "2.02")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://users.fred.net/tds/lab/p2c/p2c-"
                                  version ".zip"))
              (sha256
               (base32
                "17q6s0vbz298pks80bxf4r6gm8kwbrml1q3vfs6g6yj75sqj58xs"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "HOMEDIR=" #$output "/lib/p2c")
              (string-append "INCDIR=" #$output "/include/p2c")
              (string-append "BINDIR=" #$output "/bin")
              (string-append "LIBDIR=" #$output "/lib")
              (string-append "MANDIR=" #$output "/share/man/man1")
              "MANFILE=p2c.man.inst")
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'mkdir
            (lambda _
              (mkdir-p (string-append #$output "/share/man"))
              (mkdir-p (string-append #$output "/lib"))
              (mkdir-p (string-append #$output "/bin"))
              (mkdir-p (string-append #$output "/include"))))
          (add-before 'build 'chdir
            (lambda _ (chdir "src"))))))
    (native-inputs
     (list perl unzip which))
    (synopsis "p2c converts Pascal programs to C programs")
    (description "This package provides @command{p2c}, a program to convert
Pascal source code to C source code, and @command{p2cc}, a compiler for
Pascal programs.")
    (home-page "http://users.fred.net/tds/lab/p2c/")
    (license license:gpl2+)))

(define-public lazarus
  (package
    (name "lazarus")
    (version "2.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://gitlab.com/freepascal.org/lazarus/lazarus.git")
                    (commit (string-append "lazarus_"
                                           (string-join (string-split version
                                                                      #\.)
                                                        "_")))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0hpk6fxmy1h1q0df41jg1vnp8g8vynrg5v5ad43lv229nizfs3wj"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;No tests exist
           #:make-flags #~(list (string-append "INSTALL_PREFIX="
                                               #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (replace 'build
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (let* ((libdirs (map (lambda (x)
                                                   (assoc-ref inputs x))
                                                 '("glib" "gdk-pixbuf"
                                                   "gtk+"
                                                   "libx11"
                                                   "libx11"
                                                   "pango"
                                                   "cairo"
                                                   "atk")))
                                   (libs (append (map (lambda (name)
                                                        (string-append "-Fl"
                                                                       name
                                                                       "/lib"))
                                                      libdirs)
                                                 (map (lambda (name)
                                                        (string-append
                                                         "-k-rpath=" name
                                                         "/lib")) libdirs))))
                              (setenv "LAZARUS_LIBPATHS"
                                      (string-join libs " "))
                              (setenv "MAKEFLAGS"
                                      (string-append "LHELP_OPT="
                                                     (string-join libs "\\ "))))
                            (invoke "make" "bigide"))))))
    (native-inputs (list fpc pkg-config))
    (inputs (list glib
                  gdk-pixbuf
                  gtk+-2
                  libx11
                  pango
                  cairo
                  atk))
    (synopsis "Integrated development environment for Pascal")
    (description "This package provides an integrated development environment
for Pascal.")
    (home-page "https://www.lazarus-ide.org/")
    ;; Some Android stuff is under asl2.0. Some artwork is under CC-BY-SA-3
    ;; or CC-BY-SA-4.
    ;; Some components are under MIT expat.
    ;; The Freetype components are under Freetype license.
    ;; A lot of components are under LGPL-2+.
    ;; synedit and turbopower_ipro are under MPL-1.1
    ;; PascalScript is under a zlib-like license.
    (license (list license:gpl2+ license:lgpl2.0+))))
