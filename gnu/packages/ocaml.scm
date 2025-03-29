;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2018-2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2024 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Peter Kreye <kreyepr@gmail.com>
;;; Copyright © 2018, 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 divoplade <d@divoplade.fr>
;;; Copyright © 2020, 2021, 2022 pukkamustard <pukkamustard@posteo.net>
;;; Copyright © 2021 aecepoglu <aecepoglu@fastmail.fm>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Garek Dyszel <garekdyszel@disroot.org>
;;; Copyright © 2023 Csepp <raingloom@riseup.net>
;;; Copyright © 2023, 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2023 Arnaud DABY-SEESARAM <ds-ac@nanein.fr>
;;; Copyright © 2024 Sören Tempel <soeren@soeren-tempel.net>
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

(define-module (gnu packages ocaml)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module ((srfi srfi-1) #:hide (zip)))

;; A shortcut for files from ocaml forge. Downloaded files are computed from
;; their number, not their name.
(define (ocaml-forge-uri name version file-number)
  (string-append "https://forge.ocamlcore.org/frs/download.php/"
                 (number->string file-number) "/" name "-" version
                 ".tar.gz"))

(define (janestreet-origin name version hash)
  (origin (method url-fetch)
          (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                              (version-major+minor version) "/files/"
                              name "-v" (version-major+minor+point version)
                              ".tar.gz"))
          (sha256 (base32 hash))))

(define-public camlboot
  (let ((commit "45045d0afa82f7e9b7ea07314aab08be2d3cd64b")
        (revision "1"))
    (package
      (name "camlboot")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Ekdohibs/camlboot")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1f5gl3hzvixbgk0v3kmxiyn432znyy3jh5fa65cfzcaxzgfv1i1c"))
                (patches (search-patches
                           "camlboot-dynamically-allocate-stack-signal.patch"))
                (modules '((guix build utils)))
                (snippet
                 `(begin
                    ;; Remove bootstrap binaries and pre-generated source files,
                    ;; to ensure we actually bootstrap properly.
                    (for-each delete-file (find-files "ocaml-src" "^.depend$"))
                    (delete-file "ocaml-src/boot/ocamlc")
                    (delete-file "ocaml-src/boot/ocamllex")
                    ;; Ensure writable
                    (for-each
                     (lambda (file)
                       (chmod file (logior (stat:mode (stat file)) #o200)))
                     (find-files "." "."))))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "_boot/ocamlc") ; build target
         #:tests? #f                        ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'no-autocompile
             (lambda _
               ;; prevent a guile warning
               (setenv "GUILE_AUTO_COMPILE" "0")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (mkdir-p bin)
                 (install-file "_boot/ocamlc" bin)
                 (rename-file "miniml/interp/lex.byte" "ocamllex")
                 (install-file "ocamllex" bin)))))))
      (native-inputs
       (list guile-3.0))
      (properties
       ;; 10 hours, mostly for arm, more than 1 expected even on x86_64
       `((max-silent-time . 36000)))
      (home-page "https://github.com/Ekdohibs/camlboot")
      (synopsis "OCaml source bootstrap")
      (description "OCaml is written in OCaml.  Its sources contain a pre-compiled
bytecode version of @command{ocamlc} and @command{ocamllex} that are used to
build the next version of the compiler.  Camlboot implements a bootstrap for
the OCaml compiler and provides a bootstrapped equivalent to these files.

It contains a compiler for a small subset of OCaml written in Guile Scheme,
an interpreter for OCaml written in that subset and a manually-written lexer
for OCaml.  These elements eliminate the need for the binary bootstrap in
OCaml and can effectively bootstrap OCaml 4.07.

This package produces a native @command{ocamlc} and a bytecode @command{ocamllex}.")
      (license license:expat))))

(define-public ocaml-5.0
  (package
    (name "ocaml")
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocaml/ocaml")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p0p8wldrnbr61wfy3x4122017g4k5gjvfwlg3mvlqn8r2fxn2m5"))))
    (build-system gnu-build-system)
    (native-search-paths
     (list (search-path-specification
            (variable "OCAMLPATH")
            (files (list "lib/ocaml" "lib/ocaml/site-lib")))
           (search-path-specification
            (variable "CAML_LD_LIBRARY_PATH")
            (files (list "lib/ocaml/site-lib/stubslibs"
                         "lib/ocaml/site-lib/stublibs")))))
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list libx11 libiberty ;needed for objdump support
           zlib))                       ;also needed for objdump support
    (arguments
     `(#:configure-flags '("--enable-ocamltest")
       #:test-target "tests"
       #:make-flags '("defaultentry")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((sh (search-input-file inputs "/bin/sh"))
                    (quoted-sh (string-append "\"" sh "\"")))
               (with-fluids ((%default-port-encoding #f))
                 (for-each
                  (lambda (file)
                    (substitute* file
                      (("\"/bin/sh\"")
                       (begin
                         (format (current-error-port) "\
patch-/bin/sh-references: ~a: changing `\"/bin/sh\"' to `~a'~%"
                                 file quoted-sh)
                         quoted-sh))))
                  (find-files "." "\\.ml$")))))))))
    (home-page "https://ocaml.org/")
    (synopsis "The OCaml programming language")
    (description
     "OCaml is a general purpose industrial-strength programming language with
an emphasis on expressiveness and safety.  Developed for more than 20 years at
Inria it benefits from one of the most advanced type systems and supports
functional, imperative and object-oriented styles of programming.")
    ;; The compiler is distributed under qpl1.0 with a change to choice of
    ;; law: the license is governed by the laws of France.  The library is
    ;; distributed under lgpl2.0.
    (license (list license:qpl license:lgpl2.0))))


(define-public ocaml-4.14
  (package
    (name "ocaml")
    (version "4.14.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (sha256
               (base32
                "0vxvwxxycpc3r5p7ys59d86vw5vdr2lhmck1f3s6qms2096rf9y1"))))
    (build-system gnu-build-system)
    (native-search-paths
     (list (search-path-specification
            (variable "OCAMLPATH")
            (files (list "lib/ocaml" "lib/ocaml/site-lib")))
           (search-path-specification
            (variable "CAML_LD_LIBRARY_PATH")
            (files (list "lib/ocaml/site-lib/stubslibs"
                         "lib/ocaml/site-lib/stublibs")))))
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list libx11 libiberty ;needed for objdump support
           zlib))                       ;also needed for objdump support
    (arguments
     `(#:configure-flags '("--enable-ocamltest")
       #:test-target "tests"
       #:make-flags '("world.opt")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((sh (search-input-file inputs "/bin/sh"))
                    (quoted-sh (string-append "\"" sh "\"")))
               (with-fluids ((%default-port-encoding #f))
                 (for-each
                  (lambda (file)
                    (substitute* file
                      (("\"/bin/sh\"")
                       (begin
                         (format (current-error-port) "\
patch-/bin/sh-references: ~a: changing `\"/bin/sh\"' to `~a'~%"
                                 file quoted-sh)
                         quoted-sh))))
                  (find-files "." "\\.ml$")))))))))
    (home-page "https://ocaml.org/")
    (synopsis "The OCaml programming language")
    (description
     "OCaml is a general purpose industrial-strength programming language with
an emphasis on expressiveness and safety.  Developed for more than 20 years at
Inria it benefits from one of the most advanced type systems and supports
functional, imperative and object-oriented styles of programming.")
    ;; The compiler is distributed under qpl1.0 with a change to choice of
    ;; law: the license is governed by the laws of France.  The library is
    ;; distributed under lgpl2.0.
    (license (list license:qpl license:lgpl2.0))))

(define-public ocaml-4.09
  (package
    (inherit ocaml-4.14)
    (version "4.09.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (patches (search-patches
                         "ocaml-4.09-multiple-definitions.patch"
                         "ocaml-4.09-dynamically-allocate-signal-stack.patch"))
              (sha256
               (base32
                "1v3z5ar326f3hzvpfljg4xj8b9lmbrl53fn57yih1bkbx3gr3yzj"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((sh (search-input-file inputs "/bin/sh"))
                    (quoted-sh (string-append "\"" sh "\"")))
               (with-fluids ((%default-port-encoding #f))
                 (for-each
                  (lambda (file)
                    (substitute* file
                      (("\"/bin/sh\"")
                       (begin
                         (format (current-error-port) "\
patch-/bin/sh-references: ~a: changing `\"/bin/sh\"' to `~a'~%"
                                 file quoted-sh)
                         quoted-sh))))
                  (find-files "." "\\.ml$"))))))
         (replace 'build
           (lambda _
             (invoke "make" "-j" (number->string (parallel-job-count))
                     "world.opt")))
         (replace 'check
           (lambda _
             (with-directory-excursion "testsuite"
               (invoke "make" "all")))))))))

;; This package is a bootstrap package for ocaml-4.07. It builds from camlboot,
;; using the upstream sources for ocaml 4.07. It installs a bytecode ocamllex
;; and ocamlc, the bytecode interpreter ocamlrun, and generated .depend files
;; that we otherwise remove for bootstrap purposes.
(define ocaml-4.07-boot
  (package
    (inherit ocaml-4.09)
    (name "ocaml-boot")
    (version "4.07.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (sha256
               (base32
                "1f07hgj5k45cylj1q3k5mk8yi02cwzx849b1fwnwia8xlcfqpr6z"))
              (patches (search-patches
                         "ocaml-multiple-definitions.patch"
                         "ocaml-4.07-dynamically-allocate-signal-stack.patch"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Remove bootstrap binaries and pre-generated source files,
                  ;; to ensure we actually bootstrap properly.
                  (for-each delete-file (find-files "." "^.depend$"))
                  (delete-file "boot/ocamlc")
                  (delete-file "boot/ocamllex")))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'copy-bootstrap
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((camlboot (assoc-ref inputs "camlboot")))
               (copy-file (string-append camlboot "/bin/ocamllex") "boot/ocamllex")
               (copy-file (string-append camlboot "/bin/ocamlc") "boot/ocamlc")
               (chmod "boot/ocamllex" #o755)
               (chmod "boot/ocamlc" #o755))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mandir (string-append out "/share/man")))
               (invoke "./configure"
                       "--prefix" out
                       "--mandir" mandir))))
         (replace 'build
           (lambda* (#:key parallel-build? #:allow-other-keys)
             (define* (make . args)
               (apply invoke "make"
                      (append (if parallel-build?
                                  `("-j" ,(number->string (parallel-job-count)))
                                  '())
                              args)))
             ;; create empty .depend files because they are included by various
             ;; Makefiles, and they have no rule to generate them.
             (invoke "touch" ".depend" "stdlib/.depend" "byterun/.depend"
                     "tools/.depend"  "lex/.depend" "asmrun/.depend"
                     "debugger/.depend" "ocamltest/.depend" "ocamldoc/.depend"
                     "ocamldoc/stdlib_non_prefixed/.depend"
                     "otherlibs/bigarray/.depend" "otherlibs/graph/.depend"
                     "otherlibs/raw_spacetime_lib/.depend" "otherlibs/str/.depend"
                     "otherlibs/systhreads/.depend" "otherlibs/threads/.depend"
                     "otherlibs/unix/.depend" "otherlibs/win32unix/.depend")
             ;; We cannot build ocamldep until we have created all the .depend
             ;; files, so replace it with ocamlc -depend.
             (substitute* "tools/Makefile"
               (("\\$\\(CAMLRUN\\) ./ocamldep") "../boot/ocamlc -depend"))
             (substitute* '("otherlibs/graph/Makefile"
                            "otherlibs/systhreads/Makefile"
                            "otherlibs/threads/Makefile"
                            "otherlibs/unix/Makefile")
               (("\\$\\(CAMLRUN\\) ../../tools/ocamldep")
                "../../boot/ocamlc -depend"))
             (substitute* '("otherlibs/bigarray/Makefile"
                            "otherlibs/raw_spacetime_lib/Makefile"
                            "otherlibs/str/Makefile"
                            "otherlibs/win32unix/Makefile")
               (("\\$\\(CAMLRUN\\) \\$\\(ROOTDIR\\)/tools/ocamldep")
                "../../boot/ocamlc -depend"))
             ;; Ensure we copy needed file, so we can generate a proper .depend
             (substitute* "ocamldoc/Makefile"
               (("include Makefile.unprefix")
                "include Makefile.unprefix
depend: $(STDLIB_MLIS) $(STDLIB_DEPS)"))
             ;; Generate required tools for `alldepend'
             (make "-C" "byterun" "depend")
             (make "-C" "byterun" "all")
             (copy-file "byterun/ocamlrun" "boot/ocamlrun")
             (make "ocamlyacc")
             (copy-file "yacc/ocamlyacc" "boot/ocamlyacc")
             (make "-C" "stdlib" "sys.ml")
             (make "-C" "stdlib" "CAMLDEP=../boot/ocamlc -depend" "depend")
             ;; Build and copy files later used by `tools'
             (make "-C" "stdlib" "COMPILER="
                   "CAMLC=../boot/ocamlc -use-prims ../byterun/primitives"
                   "all")
             (for-each
              (lambda (file)
                (copy-file file (string-append "boot/" (basename file))))
              (cons* "stdlib/stdlib.cma" "stdlib/std_exit.cmo" "stdlib/camlheader"
                     (find-files "stdlib" ".*.cmi$")))
             (symlink "../byterun/libcamlrun.a" "boot/libcamlrun.a")
             ;; required for ocamldoc/stdlib_non_prefixed
             (make "parsing/parser.mli")
             ;; required for dependencies
             (make "-C" "tools"
                   "CAMLC=../boot/ocamlc -nostdlib -I ../boot -use-prims ../byterun/primitives -I .."
                   "make_opcodes" "cvt_emit")
             ;; generate all remaining .depend files
             (make "alldepend"
                   (string-append "ocamllex=" (getcwd) "/boot/ocamlrun "
                                  (getcwd) "/boot/ocamllex")
                   (string-append "CAMLDEP=" (getcwd) "/boot/ocamlc -depend")
                   (string-append "OCAMLDEP=" (getcwd) "/boot/ocamlc -depend")
                   (string-append "ocamldep=" (getcwd) "/boot/ocamlc -depend"))
             ;; Build ocamllex
             (make "CAMLC=boot/ocamlc -nostdlib -I boot -use-prims byterun/primitives"
                   "ocamlc")
             ;; Build ocamlc
             (make "-C" "lex"
                   "CAMLC=../boot/ocamlc -strict-sequence -nostdlib -I ../boot -use-prims ../byterun/primitives"
                   "all")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (depends (string-append out "/share/depends")))
               (mkdir-p bin)
               (mkdir-p depends)
               (install-file "ocamlc" bin)
               (install-file "lex/ocamllex" bin)
               (for-each
                (lambda (file)
                  (let ((dir (string-append depends "/" (dirname file))))
                    (mkdir-p dir)
                    (install-file file dir)))
                (find-files "." "^\\.depend$"))))))))
    (native-inputs
     `(("camlboot" ,camlboot)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))))

(define-public ocaml-4.07
  (package
    (inherit ocaml-4.07-boot)
    (name "ocaml")
    (arguments
      (substitute-keyword-arguments (package-arguments ocaml-4.09)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'configure 'copy-bootstrap
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((ocaml (assoc-ref inputs "ocaml")))
                  (copy-file (string-append ocaml "/bin/ocamllex") "boot/ocamllex")
                  (copy-file (string-append ocaml "/bin/ocamlc") "boot/ocamlc")
                  (chmod "boot/ocamllex" #o755)
                  (chmod "boot/ocamlc" #o755)
                  (let ((rootdir (getcwd)))
                    (with-directory-excursion (string-append ocaml "/share/depends")
                      (for-each
                        (lambda (file)
                          (copy-file file (string-append rootdir "/" file)))
                        (find-files "." ".")))))))
            (replace 'configure
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (mandir (string-append out "/share/man")))
                  ;; Custom configure script doesn't recognize
                  ;; --prefix=<PREFIX> syntax (with equals sign).
                  (invoke "./configure"
                          "--prefix" out
                          "--mandir" mandir))))))))
    (native-inputs
     `(("ocaml" ,ocaml-4.07-boot)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))))

(define-public ocaml ocaml-4.14)

(define-public ocamlbuild
  (package
    (name "ocamlbuild")
    (version "0.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/ocamlbuild")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16q8s22msyfq66i1sbz99wj04a9x9ad95x458ixxacxsv0qqh2j0"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:make-flags
       ,#~(list (string-append "OCAMLBUILD_PREFIX=" #$output)
                (string-append "OCAMLBUILD_BINDIR=" #$output "/bin")
                (string-append "OCAMLBUILD_LIBDIR=" #$output
                               "/lib/ocaml/site-lib")
                (string-append "OCAMLBUILD_MANDIR=" #$output "/share/man"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       ; some failures because of changes in OCaml's error message formatting
       #:tests? #f))
    (home-page "https://github.com/ocaml/ocamlbuild")
    (synopsis "OCaml build tool")
    (description "OCamlbuild is a generic build tool, that has built-in rules
for building OCaml library and programs.")
    (license license:lgpl2.1+)))

(define-public camlidl
  (package
    (name "camlidl")
    (version "1.09")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xavierleroy/camlidl")
             (commit "camlidl109")))
       (sha256
        (base32 "0zrkaq7fk23b2b9vg6jwdjx7l0hdqp4synbbrw1zcg8gjf6n3c80"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; No test suite
       #:make-flags
       (list
        (string-append
         "BINDIR=" (assoc-ref %outputs "out") "/bin")
        (string-append
         "OCAMLLIB=" (assoc-ref %outputs "out") "/lib/ocaml/site-lib/camlidl"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (copy-file "config/Makefile.unix" "config/Makefile")
             ;; Note: do not pass '-jN' as this appears to not be
             ;; parallel-safe (race condition related to libcamlidl.a).
             (invoke "make" "all")
             #t))
         (add-before 'install 'create-target-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out"))))
               (mkdir-p
                (string-append out "/bin"))
               (mkdir-p
                (string-append out "/lib/ocaml/site-lib/camlidl/stublibs"))
               (mkdir-p
                (string-append out "/lib/ocaml/site-lib/camlidl/caml")))
             #t))
         (add-after 'install 'install-meta
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-output-to-file
                   (string-append out "/lib/ocaml/site-lib/camlidl/META")
                 (lambda _
                   (display
                    (string-append
                     "description = \"Stub code generator for OCaml/C interface\"
version = \"" ,version "\"
directory = \"^\"
archive(byte) = \"com.cma\"
archive(native) = \"com.cmxa\"")))))
             #t)))))
    (native-inputs
     (list ocaml))
    (home-page "https://github.com/xavierleroy/camlidl")
    (synopsis "Stub code generator for OCaml/C interface")
    (description
     "Camlidl is a stub code generator for Objective Caml.  It generates stub
code for interfacing Caml with C from an IDL description of the C functions.")
    (license license:lgpl2.1)))

(define-public ocaml-extlib
  (package
    (name "ocaml-extlib")
    (version "1.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ygrek.org/p/release/ocaml-extlib/"
                                  "extlib-" version ".tar.gz"))
              (sha256
               (base32
                "1jydzw2n84cfiz9y6lk4gih4wbr8jybanmiryfs01svd07g4vpjq"))))
    (build-system dune-build-system)
    (arguments
     (list #:package "extlib"))
    (native-inputs
      (list ocaml-cppo))
    (home-page "https://github.com/ygrek/ocaml-extlib")
    (synopsis "Complete and small extension for OCaml standard library")
    (description "This library adds new functions to OCaml standard library
modules, modifies some functions in order to get better performances or
safety (tail-recursive) and also provides new modules which should be useful
for day to day programming.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-cudf
  (package
    (name "ocaml-cudf")
    (version "0.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.com/irill/cudf")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lvrmpscbk1kjv5ag5bzlzv520xk5zw2haf6q7chvz98gcm9g0hk"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-extlib))
    (native-inputs (list ocaml-ounit2))
    (home-page "https://www.mancoosi.org/cudf/")
    (synopsis "CUDF library (part of the Mancoosi tools)")
    (description
     "@acronym{CUDF, Common Upgradeability Description Format} is a format for
describing upgrade scenarios in package-based software distributions.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-mccs
  (package
    (name "ocaml-mccs")
    (version "1.1+14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/AltGr/ocaml-mccs")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17bvm0jhhs8h3p5sbb65asj53a8sxl634cc0kvcivpams74837zq"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-cudf))
    (home-page "https://www.i3s.unice.fr/~cpjm/misc/")
    (synopsis "Upgrade path problem solver")
    (description "Mccs (Multi Criteria CUDF Solver) is a CUDF problem solver.
Mccs take as input a CUDF problem and computes the best solution according to
a set of criteria.  It relies on a Integer Programming solver or a
Pseudo Boolean solver to achieve its task.  Mccs can use a wide set of
underlying solvers like Cplex, Gurobi, Lpsolver, Glpk, CbC, SCIP or WBO.")
    (license (list
               license:bsd-3
               license:gpl3+
               ;; With static-linking exception
               license:lgpl2.1+))))

(define-public ocaml-dose3
  (package
    (name "ocaml-dose3")
    (version "7.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/irill/dose3")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hcjh68svicap7j9bghgkp49xa12qhxa1pygmrgc9qwm0m4dhirb"))))
    (build-system dune-build-system)
    (arguments `(#:package "dose3"))
    (propagated-inputs (list ocaml-extlib
                             ocaml-base64-boot
                             ocaml-cudf
                             ocaml-graph
                             ocaml-re
                             ocaml-stdlib-shims))
    (native-inputs (list ocaml-ounit))
    (home-page "https://www.mancoosi.org/software/")
    (synopsis "Package distribution management framework")
    (description "Dose3 is a framework made of several OCaml libraries for
managing distribution packages and their dependencies.  Though not tied to
any particular distribution, dose3 constitutes a pool of libraries which
enable analyzing packages coming from various distributions.  Besides basic
functionalities for querying and setting package properties, dose3 also
implements algorithms for solving more complex problems such as monitoring
package evolutions, correct and complete dependency resolution and
repository-wide uninstallability checks.")
    ;; with static-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-down
  (package
    (name "ocaml-down")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://erratique.ch/software/down/releases/down-"
                            version ".tbz"))
        (sha256
         (base32
          "1q467y6qz96ndiybpmggdcnrcip49kxw2i93pb54j1xjzkv1vnl1"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:build-flags
       ,#~(list "build" "--lib-dir"
                (string-append #$output "/lib/ocaml/site-lib"))))
    (native-inputs
     (list ocaml-findlib ocamlbuild ocaml-topkg opam-installer))
    (home-page "https://erratique.ch/software/down")
    (synopsis "OCaml toplevel (REPL) upgrade")
    (description "Down is an unintrusive user experience upgrade for the
@command{ocaml} toplevel (REPL).

Simply load the zero dependency @code{down} library in the @command{ocaml}
toplevel and you get line edition, history, session support and identifier
completion and documentation with @command{ocp-index}.

Add this to your @file{~/.ocamlinit}:

@example
#use \"down.top\"
@end example

You may also need to add this to your @file{~/.ocamlinit} and declare
the environment variable @code{OCAML_TOPLEVEL_PATH}:

@example
let () =
  try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")
  with Not_found -> ()
@end example

OR

@example
let () = String.split_on_char ':' (Sys.getenv \"OCAMLPATH\")
         |> List.filter (fun x -> Filename.check_suffix x \"/site-lib\")
         |> List.map (fun x -> x ^ \"/toplevel\")
         (* remove the line below if you don't want to see the text
            every time you start the toplevel *)
         |> List.map (fun x -> Printf.printf \"adding directory %s\\n\" x; x)
         |> List.iter Topdirs.dir_directory;;
@end example")
    (license license:isc)))

(define-public ocaml-opam-file-format
  (package
    (name "ocaml-opam-file-format")
    (version "2.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/opam-file-format")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dmnb1mqdy4913f9ma446hi5m99q7hfibj6j0m8x2wsfnfy2fw62"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; No tests
       #:make-flags ,#~(list (string-append "LIBDIR=" #$output
                                            "/lib/ocaml/site-lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://opam.ocaml.org")
    (synopsis "Parser and printer for the opam file syntax")
    (description "This package contains a parser and a pretty-printer for
the opam file format.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define ocaml-opam-core
  (package
    (name "ocaml-opam-core")
    (version "2.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/opam")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ckd87rcmcz11iyhhm5qnmy27jbdffx6n1fr06hvrqqrzi00jljh"))))
    (build-system dune-build-system)
    (arguments `(#:package "opam-core"
                 ;; tests are run with the opam package
                 #:tests? #f
                 #:phases
                 (modify-phases %standard-phases
                   (add-before 'build 'pre-build
                     (lambda* (#:key inputs make-flags #:allow-other-keys)
                       (let ((bash (assoc-ref inputs "bash"))
                             (bwrap (search-input-file inputs "/bin/bwrap")))
                         (substitute* "src/core/opamSystem.ml"
                           (("\"/bin/sh\"")
                            (string-append "\"" bash "/bin/sh\""))
                           (("getconf")
                            (which "getconf")))))))))
    (propagated-inputs
     (list ocaml-graph
           ocaml-re
           ocaml-cppo))
    (inputs (list bubblewrap))
    (home-page "https://opam.ocamlpro.com/")
    (synopsis "Package manager for OCaml")
    (description
     "OPAM is a tool to manage OCaml packages.  It supports multiple
simultaneous compiler installations, flexible package constraints, and a
Git-friendly development workflow.")
    ;; The 'LICENSE' file waives some requirements compared to LGPLv3.
    (license license:lgpl3)))

(define ocaml-opam-format
  (package
    (inherit ocaml-opam-core)
    (name "ocaml-opam-format")
    (inputs '())
    (propagated-inputs (list ocaml-opam-core
                             ocaml-opam-file-format
                             ocaml-re))
    (arguments `(#:package "opam-format"
                 ;; tests are run with the opam package
                 #:tests? #f
                 #:phases %standard-phases))))

(define-public opam-installer
  (package
    (inherit ocaml-opam-core)
    (name "opam-installer")
    (native-inputs (list ocaml-opam-format
                         ocaml-cmdliner))
    (inputs '())
    (propagated-inputs '())
    (arguments `(#:package "opam-installer"
                 ;; requires all of opam
                 #:tests? #f))
    (synopsis "Tool for installing OCaml packages")
    (description "@var{opam-installer} is a tool for installing OCaml packages
based on @code{.install} files defined by the OPAM package manager.  It is
useful for installing OCaml packages without requiring the entirety of
OPAM.")
    (properties
     ;; opam-installer is used as a tool and not as a library, we can use the
     ;; OCaml 4.14 compiled opam until opam is compatible with OCaml 5.0.
     `((ocaml5.0-variant . ,(delay opam-installer))))))

(define ocaml-opam-repository
  (package
    (inherit ocaml-opam-core)
    (name "ocaml-opam-repository")
    (inputs '())
    (propagated-inputs (list ocaml-opam-format))
    (arguments `(#:package "opam-repository"
                 ;; tests are run with the opam package
                 #:tests? #f
                 #:phases %standard-phases))))

(define ocaml-opam-state
  (package
    (inherit ocaml-opam-core)
    (name "ocaml-opam-state")
    (arguments `(#:package "opam-state"
                 ;; tests are run with the opam package
                 #:tests? #f
                 #:phases
                 (modify-phases %standard-phases
                   (add-before 'build 'pre-build
                     (lambda* (#:key inputs make-flags #:allow-other-keys)
                       (let ((bwrap (search-input-file inputs "/bin/bwrap")))
                         ;; Use bwrap from the store directly.
                         (substitute* "src/state/shellscripts/bwrap.sh"
                           (("-v bwrap") (string-append "-v " bwrap))
                           (("exec bwrap") (string-append "exec " bwrap))
                           ;; Mount /gnu and /run/current-system in the
                           ;; isolated environment when building with opam.
                           ;; This is necessary for packages to find external
                           ;; dependencies, such as a C compiler, make, etc...
                           (("^add_sys_mounts /usr")
                            (string-append "add_sys_mounts "
                                           (%store-directory)
                                           " /run/current-system /usr")))))))))
    (inputs (list bubblewrap))
    (propagated-inputs (list ocaml-opam-repository))))

(define ocaml-opam-solver
  (package
    (inherit ocaml-opam-core)
    (name "ocaml-opam-solver")
    (inputs '())
    (propagated-inputs (list ocaml-opam-format
                             ocaml-mccs
                             ocaml-dose3))
    (arguments `(#:package "opam-solver"
                 ;; tests are run with the opam package
                 #:tests? #f
                 #:phases %standard-phases))))

(define ocaml-opam-client
  (package
    (inherit ocaml-opam-core)
    (name "ocaml-opam-client")
    (arguments `(#:package "opam-client"
                 ;; tests are run with the opam package
                 #:tests? #f
                 #:phases
                 (modify-phases %standard-phases
                   (add-before 'build 'pre-build
                     (lambda* (#:key inputs make-flags #:allow-other-keys)
                       (let ((bwrap (search-input-file inputs "/bin/bwrap")))
                         (substitute* "src/client/opamInitDefaults.ml"
                           (("\"bwrap\"") (string-append "\"" bwrap "\"")))))))))
    (inputs (list bubblewrap))
    (propagated-inputs
     (list ocaml-opam-state
           ocaml-opam-solver
           ocaml-opam-repository
           ocaml-base64
           ocaml-re
           ocaml-cmdliner))))

(define-public opam
  (package
    (inherit ocaml-opam-core)
    (name "opam")
    (build-system dune-build-system)
    (arguments
     `(#:package "opam"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'prepare-checks
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Opam tests need to run an isolated environment from a writable
             ;; home directory.
             (mkdir-p "test-home")
             (setenv "HOME" (string-append (getcwd) "/test-home"))
             (with-output-to-file (string-append (getcwd) "/test-home/.gitconfig")
               (lambda _
                 (display "[user]
email = guix@localhost.none
name = Guix Builder")
                 (newline)))

             ;; Opam tests require data from opam-repository. Instead of
             ;; downloading them with wget from the guix environment, copy the
             ;; content to the expected directory.
             (substitute* "tests/reftests/dune.inc"
               (("tar -C.*opam-archive-([0-9a-f]*)[^)]*" _ commit)
                (string-append "rmdir %{targets}) (run cp -r "
                               (assoc-ref inputs (string-append "opam-repo-" commit))
                               "/ %{targets}) (run chmod +w -R %{targets}"))
               (("wget[^)]*") "touch %{targets}")
               ;; Disable a failing test because it tries to clone a git
               ;; repository from inside bwrap
               (("diff upgrade-format.test upgrade-format.out") "run true")
               ;; Disable a failing test because it tries to figure out which
               ;; distro this is, and it doesn't know Guix
               (("diff pin.unix.test pin.unix.out") "run true")
               ;; Disable a failing test because of a failed expansion
               (("diff opamroot-versions.test opamroot-versions.out") "run true")
               ;; Disable a failing test, probably because the repository we
               ;; replaced is not as expected
               (("diff opamrt-big-upgrade.test opamrt-big-upgrade.out") "run true")
               ;; Disable a failing test because of missing sandboxing
               ;; functionality
               (("diff init.test init.out") "run true"))
             (substitute* "tests/reftests/dune"
               ;; Because of our changes to the previous file, we cannot check
               ;; it can be regenerated
               (("diff dune.inc dune.inc.gen") "run true"))
             ;; Ensure we can run the generated build.sh (no /bin/sh)
             (substitute* '("tests/reftests/legacy-local.test"
                            "tests/reftests/legacy-git.test")
               (("#! ?/bin/sh")
                (string-append "#!"
                               (search-input-file inputs "/bin/sh"))))
             (substitute* "tests/reftests/testing-env"
               (("OPAMSTRICT=1")
                (string-append "OPAMSTRICT=1\nLIBRARY_PATH="
                               (assoc-ref inputs "libc") "/lib"))))))))
    (native-inputs
      (let ((opam-repo (lambda (commit hash)
                         (origin
                           (method git-fetch)
                           (uri (git-reference
                                  (url "https://github.com/ocaml/opam-repository")
                                  (commit commit)))
                           (file-name (git-file-name "opam-repo" commit))
                           (sha256 (base32 hash))))))
       `(("dune" ,dune)
         ("ocaml-cppo" ,ocaml-cppo)

         ;; For tests.
         ("git" ,git-minimal/pinned)
         ("openssl" ,openssl)
         ("python" ,python-wrapper)
         ("rsync" ,rsync)
         ("unzip" ,unzip)
         ("which" ,which)

         ;; Data for tests
         ("opam-repo-0070613707"
          ,(opam-repo "00706137074d536d2019d2d222fbe1bea929deda"
                      "1gv1vvmfscj7wirfv6qncp8pf81wygnpzjwd0lyqcxm7g8r8lb4w"))
         ("opam-repo-009e00fa"
          ,(opam-repo "009e00fa86300d11c311309a2544e5c6c3eb8de2"
                      "1wwy0rwrsjf4q10j1rh1dazk32fbzhzy6f7zl6qmndidx9b1bq7w"))
         ("opam-repo-7090735c"
          ,(opam-repo "7090735c9d1dd2dc481c4128c5ef4d3667238f15"
                      "1bccsgjhlp64lmvfjfn6viywf3x73ji75myg9ssf1ij1fkmabn0z"))
         ("opam-repo-a5d7cdc0"
          ,(opam-repo "a5d7cdc0c91452b0aef4fa71c331ee5237f6dddd"
                      "0z7kawqisy07088p5xjxwpvmvzlbj1d9cgdipsj90yx7nc5qh369"))
         ("opam-repo-ad4dd344"
          ,(opam-repo "ad4dd344fe5cd1cab49ced49d6758a9844549fb4"
                      "1a1qj47kj8xjdnc4zc50ijrix1kym1n7k20n3viki80a7518baw8"))
         ("opam-repo-c1842d168d"
          ,(opam-repo "c1842d168de956caf06d7ac8588e65020d7594d8"
                      "142y1ac7sprygyh91shcp0zcyfxjjkshi9g44qgg4rx60rbsbhai"))
         ("opam-repo-c1d23f0e"
          ,(opam-repo "c1d23f0e17ec83a036ebfbad1c78311b898a2ca0"
                      "0j9abisx3ifzm66ci3p45mngmz4f0fx7yd9jjxrz3f8w5jffc9ii"))
         ("opam-repo-f372039d"
          ,(opam-repo "f372039db86a970ef3e662adbfe0d4f5cd980701"
                      "0ld7fcry6ss6fmrpswvr6bikgx299w97h0gwrjjh7kd7rydsjdws"))
         ("opam-repo-11ea1cb"
          ,(opam-repo "11ea1cb6f2418b1f8a6679e4422771a04c9c3655"
                      "1s4p0wfn3bx97yvm8xvj3yhzv2pz0jwml68g2ybv37hj9mpbrsq0"))
         ("opam-repo-297366c"
          ,(opam-repo "297366cd01c3aaf29b967bf0b34ccc7989d4d5b3"
                      "1ysg69gys37nc2cxivs2ikh6xp0gj85if4rcrr874mqb9z12dm0j"))
         ("opam-repo-3235916"
          ,(opam-repo "3235916a162a59d7c82dac3fe24214975d48f1aa"
                      "1yf73rv2n740a4s9g7a9k4j91b4k7al88nwnw9cdw0k2ncbmr486"))
         ("opam-repo-de897adf36c4230dfea812f40c98223b31c4521a"
          ,(opam-repo "de897adf36c4230dfea812f40c98223b31c4521a"
                      "1m18x9gcwnbar8yv9sbfz8a3qpw412fp9cf4d6fb7syn0p0h96jw")))))
    (inputs (list ocaml-opam-client))
    (properties
     ;; OPAM is used as a tool and not as a library, we can use the OCaml 4.14
     ;; compiled opam until opam is compatible with OCaml 5.0.
     `((ocaml5.0-variant . ,(delay opam))))))

(define-public ocaml-opam-monorepo
  (package
    (name "ocaml-opam-monorepo")
    (version "0.3.5")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/tarides/opam-monorepo/")
                (commit version)))
              (file-name name)
              (sha256
               (base32
                "09lq788b1sai4v1nxd16b00pw0m55plcwrx3f9v5a90gpxg0a6sc"))))
    (build-system dune-build-system)
    (arguments
     ;; TODO
     ;; Too many tests require a fully initialized opam, disabling them would
     ;; be a huge pain.  "Mocking" opam init is difficult because it requires
     ;; networking access.
     '(#:tests? #f))
    ;; TODO: not entirely clear if these should be native, test cross-building
    (native-inputs (list ocaml-odoc
                         pkg-config))
    ;; (propagated-inputs lablgtk3) optional and is currently failing to build
    (home-page "https://github.com/tarides/opam-monorepo")
    (synopsis "Assemble and manage fully vendored Dune repositories")
    (description
     "The opam monorepo plugin provides a convenient interface to bridge the
opam package manager with having a local copy of all the source code required
to build a project using the dune build tool.")
    (license license:isc)))

(define-public ocaml-camlp-streams
  (package
    (name "ocaml-camlp-streams")
    (version "5.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ocaml/camlp-streams")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0r3wvffkzyyk4als78akirxanzbib5hvc3kvwxpk36mlmc38aywh"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (home-page "https://github.com/ocaml/camlp-streams")
    (synopsis "Stream and Genlex libraries for use with Camlp4 and Camlp5")
    (description
      "This package provides two library modules:

@itemize
@item Stream: imperative streams, with in-place update and memoization of
the latest element produced.
@item Genlex: a small parameterized lexical analyzer producing streams of
tokens from streams of characters.
@end itemize

The two modules are designed for use with Camlp4 and Camlp5: The stream
patterns and stream expressions of Camlp4/Camlp5 consume and produce data of
type 'a Stream.t.  The Genlex tokenizer can be used as a simple lexical
analyzer for Camlp4/Camlp5-generated parsers.

The Stream module can also be used by hand-written recursive-descent parsers,
but is not very convenient for this purpose.

The Stream and Genlex modules have been part of the OCaml standard library for a
long time, and have been distributed as part of the core OCaml system.  They
will be removed from the OCaml standard library at some future point, but will
be maintained and distributed separately in the camlpstreams package.")
    (license license:lgpl2.1)))

(define-public camlp5
  (package
    (name "camlp5")
    (version "8.00.03")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/camlp5/camlp5")
             (commit (string-append "rel" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fnvmaw9cland09pjx5h6w3f6fz9s23l4nbl4m9fcaa2i4dpraz6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; XXX TODO figure out how to run the tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (mandir (string-append out "/share/man")))
                      ;; Custom configure script doesn't recognize
                      ;; --prefix=<PREFIX> syntax (with equals sign).
                      (invoke "./configure"
                              "--prefix" out
                              "--mandir" mandir))))
         (add-before 'build 'fix-/bin-references
           (lambda _
             (substitute* "config/Makefile"
               (("/bin/rm") "rm"))
             #t))
         (replace 'build
                  (lambda _
                    (invoke "make" "-j" (number->string
                                         (parallel-job-count))
                            "world.opt")))
         ;; Required for findlib to find camlp5's libraries
         (add-after 'install 'install-meta
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "etc/META" (string-append (assoc-ref outputs "out")
                                                     "/lib/ocaml/camlp5/"))
             #t)))))
    (inputs
     (list ocaml ocaml-camlp-streams))
    (native-inputs
     (list perl ocaml-findlib))
    (home-page "https://camlp5.github.io/")
    (synopsis "Pre-processor Pretty Printer for OCaml")
    (description
     "Camlp5 is a Pre-Processor-Pretty-Printer for Objective Caml.  It offers
tools for syntax (Stream Parsers and Grammars) and the ability to modify the
concrete syntax of the language (Quotations, Syntax Extensions).")
    ;; Most files are distributed under bsd-3, but ocaml_stuff/* is under qpl.
    (license (list license:bsd-3 license:qpl))))

(define-public hevea
  (package
    (name "hevea")
    (version "2.36")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://hevea.inria.fr/old/"
                                  "hevea-" version ".tar.gz"))
              (sha256
               (base32
                "0j06f8gb8f5is34kzmzy3znb0jkm2qd2l6rcl5v5qa9af3bmjrsx"))))
    (build-system gnu-build-system)
    (inputs
     (list ocaml))
    (native-inputs
     (list ocamlbuild))
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'patch-/bin/sh
                    (lambda _
                      (substitute* "_tags"
                        (("/bin/sh") (which "sh")))
                      #t)))))
    (home-page "https://hevea.inria.fr/")
    (synopsis "LaTeX to HTML translator")
    (description
     "HeVeA is a LaTeX to HTML translator that generates modern HTML 5.  It is
written in Objective Caml.")
    (license license:qpl)))

(define-public ocaml-num
  (package
    (name "ocaml-num")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/num")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vzdnvpj5dbj3ifx03v25pj2jj1ccav072v4d29pk1czdba2lzfc"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-race
           (lambda _
             ;; There's a race between bng.o and bng_generic.c.  Both depend on
             ;; the architecture specific bng.c, but only the latter declares
             ;; the dependency.
             (mkdir-p "_build/default/src")
             (for-each
               (lambda (f)
                 (copy-file f (string-append "_build/default/" f)))
               (find-files "src" "bng_.*\\.c")))))))
    (home-page "https://github.com/ocaml/num")
    (synopsis "Arbitrary-precision integer and rational arithmetic")
    (description "OCaml-Num contains the legacy Num library for
arbitrary-precision integer and rational arithmetic that used to be part of
the OCaml core distribution.")
    (license license:lgpl2.1+))); with linking exception

(define-public emacs-tuareg
  (package
    (name "emacs-tuareg")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/tuareg")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p3xpk78i8ywgdmc59w05wjjy9dg6gm5gicm08szmrlnx08v2ihm"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules `(,@%default-gnu-imported-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
      #:modules '((guix build gnu-build-system)
                  ((guix build emacs-build-system) #:prefix emacs:)
                  (guix build emacs-utils)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-git-checkout-writable
            (lambda _
              (for-each make-file-writable (find-files "."))))
          (delete 'configure)
          (add-before 'install 'fix-install-path
            (lambda _
              (substitute* "Makefile"
                (("/emacs/site-lisp")
                 (emacs:elpa-directory #$output)))))
          (add-after 'install 'post-install
            (lambda _
              (symlink "tuareg.el"
                       (string-append (emacs:elpa-directory #$output)
                                      "/tuareg-autoloads.el")))))))
    (native-inputs
     (list emacs-minimal opam))
    (home-page "https://github.com/ocaml/tuareg")
    (synopsis "OCaml programming mode, REPL, debugger for Emacs")
    (description "Tuareg helps editing OCaml code, to highlight important
parts of the code, to run an OCaml REPL, and to run the OCaml debugger within
Emacs.")
    (license license:gpl2+)))

(define-public ocaml-menhir
  (package
    (name "ocaml-menhir")
    (version "20220210")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/fpottier/menhir.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f31isr3cyiishflz6qr4xc3gp9xwf32r3vxdvm5wnr2my1fnn1n"))))
    (build-system dune-build-system)
    (inputs
     (list ocaml))
    (arguments
     `(#:tests? #f)) ; No check target
    (properties `((ocaml4.07-variant . ,(delay (strip-ocaml4.07-variant ocaml-menhir)))))
    (home-page "https://gallium.inria.fr/~fpottier/menhir/")
    (synopsis "Parser generator")
    (description "Menhir is a parser generator.  It turns high-level grammar
specifications, decorated with semantic actions expressed in the OCaml
programming language into parsers, again expressed in OCaml.  It is based on
Knuth’s LR(1) parser construction technique.")
    ;; The file src/standard.mly and all files listed in src/mnehirLib.mlpack
    ;; that have an *.ml or *.mli extension are GPL licensed. All other files
    ;; are QPL licensed.
    (license (list license:gpl2+ license:qpl))))

(define-public ocaml-bigarray-compat
  (package
    (name "ocaml-bigarray-compat")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/bigarray-compat")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hif5baiwswdblymyfbxh9066pfqynlz5vj3b2brpn0a12k6i5fq"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (home-page "https://github.com/mirage/bigarray-compat")
    (synopsis "OCaml compatibility library")
    (description "This package contains a compatibility library for
@code{Stdlib.Bigarray} in OCaml.")
    (license license:isc)))

(define-public binsec
  (package
    (name "binsec")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/binsec/binsec")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1szfqb6rj19w2jdyaxdgy3plhgr7picijf7l4k5qq80kna2h0zm8"))))
    (build-system dune-build-system)
    (native-inputs (list gmp ocaml-qcheck ocaml-ounit2))
    (propagated-inputs (list dune-site
                             ocaml-base
                             ocaml-menhir
                             ocaml-graph
                             ocaml-zarith
                             ocaml-grain-dypgen
                             ocaml-toml))
    (synopsis "Binary-level analysis platform")
    (description
     "BINSEC is a binary analysis platform which implements analysis
techniques such as symbolic execution.  The goal of BINSEC is to improve
software security at the binary level through binary analysis.  BINSEC
is a research tool which relies on prior work in binary code analysis
at the intersection of formal methods, program analysis security and
software engineering.")
    (home-page "https://binsec.github.io/")
    (license license:lgpl2.1)))

(define-public unison
  (package
    (name "unison")
    (version "2.53.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bcpierce00/unison")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fy4c1wb6xn9gxdabs25yajbzik3amifyr7nzd4d9vn6r3gll9sw"))))
    (build-system dune-build-system)
    (propagated-inputs (list lablgtk3 zlib))
    (native-inputs (list ghostscript (texlive-local-tree '()) hevea lynx which))
    (arguments
     `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((doc (string-append (assoc-ref outputs "out")
                                         "/share/doc/unison")))
                 (mkdir-p doc)
                 ;; This file needs write-permissions, because it's
                 ;; overwritten by 'docs' during documentation generation.
                 (chmod "src/strings.ml" #o600)
                 (invoke "make" "docs"
                         "TEXDIRECTIVES=\\\\draftfalse")
                 (for-each (lambda (f)
                             (install-file f doc))
                           (map (lambda (ext)
                                  (string-append "doc/unison-manual." ext))
                                ;; Install only html documentation,
                                ;; since the build is currently
                                ;; non-reproducible with the ps, pdf,
                                ;; and dvi docs.
                                '(;; "ps" "pdf" "dvi"
                                  "html")))
                 #t))))))
    (home-page "https://www.cis.upenn.edu/~bcpierce/unison/")
    (synopsis "File synchronizer")
    (description
     "Unison is a file-synchronization tool.  It allows two replicas of
a collection of files and directories to be stored on different hosts
(or different disks on the same host), modified separately, and then
brought up to date by propagating the changes in each replica
to the other.")
    (license license:gpl3+)))

(define-public ocaml-findlib
  (package
    (name "ocaml-findlib")
    (version "1.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.camlcity.org/download/"
                                  "findlib" "-" version ".tar.gz"))
              (sha256
               (base32
                "0w9578j1561f5gi51sn2jgxm3kh3sn88cpannhdkqcdg1kk08iqd"))))
    (build-system gnu-build-system)
    (native-inputs
     (list m4 ocaml))
    (arguments
     `(#:tests? #f  ; no test suite
       #:parallel-build? #f
       #:make-flags (list "all" "opt")
       #:phases (modify-phases %standard-phases
                  (replace
                   'configure
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       (invoke
                        "./configure"
                        "-bindir" (string-append out "/bin")
                        "-config" (string-append out "/etc/ocamfind.conf")
                        "-mandir" (string-append out "/share/man")
                        "-sitelib" (string-append out "/lib/ocaml/site-lib")
                        "-with-toolbox"))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (invoke "make" "install"
                                (string-append "OCAML_CORE_STDLIB="
                                               out "/lib/ocaml/site-lib"))))))))
    (home-page "http://projects.camlcity.org/projects/findlib.html")
    (synopsis "Management tool for OCaml libraries")
    (description
     "The \"findlib\" library provides a scheme to manage reusable software
components (packages), and includes tools that support this scheme.  Packages
are collections of OCaml modules for which metainformation can be stored.  The
packages are kept in the file system hierarchy, but with strict directory
structure.  The library contains functions to look the directory up that
stores a package, to query metainformation about a package, and to retrieve
dependency information about multiple packages.  There is also a tool that
allows the user to enter queries on the command-line.  In order to simplify
compilation and linkage, there are new frontends of the various OCaml
compilers that can directly deal with packages.")
    (license license:x11)))

(define-public ocaml4.07-findlib
  (package
    (inherit ocaml-findlib)
    (name "ocaml4.07-findlib")
    (native-inputs
     (list m4 ocaml-4.07))))

(define-public ocaml4.09-findlib
  (package
    (inherit ocaml-findlib)
    (name "ocaml4.09-findlib")
    (native-inputs
     (list m4 ocaml-4.09))))

(define-public ocaml5.0-findlib
  (package
    (inherit ocaml-findlib)
    (name "ocaml5.0-findlib")
    (native-inputs
     (list m4 ocaml-5.0))))

(define-public ocaml-ounit2
  (package
    (name "ocaml-ounit2")
    (version "2.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gildor478/ounit.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04c841hpk2yij370w30w3pis8nibnr28v74mpq2qz7z5gb8l07p1"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-lwt ocaml-stdlib-shims))
    (home-page "https://github.com/gildor478/ounit")
    (synopsis "Unit testing framework for OCaml")
    (description "OUnit2 is a unit testing framework for OCaml.  It is similar
to JUnit and other XUnit testing frameworks.")
    (license license:expat)))

;; note that some tests may hang for no obvious reason.
(define-public ocaml-ounit
  (package
    (inherit ocaml-ounit2)
    (name "ocaml-ounit")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda _
             (invoke "make" "install-ounit" ,(string-append "version="
                                                            (package-version ocaml-ounit2))))))))
    (propagated-inputs
     (list ocaml-ounit2))
    (home-page "http://ounit.forge.ocamlcore.org")
    (synopsis "Unit testing framework for OCaml")
    (description "Unit testing framework for OCaml.  It is similar to JUnit and
other XUnit testing frameworks.")
    (license license:expat)))

(define-public ocaml-junit
  (package
    (name "ocaml-junit")
    (version "2.0.2")
    (home-page "https://github.com/Khady/ocaml-junit")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cam7zzarrh9p1l5m3ba3h5rkh9mhark8j37rjgw35a66qd0gds1"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "junit"
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'dune-subst
                    (lambda _
                      (invoke "dune" "subst") #t)))))
    (properties `((upstream-name . "junit")))
    (propagated-inputs (list ocaml-ounit ocaml-ptime ocaml-tyxml ocaml-odoc))
    (synopsis "JUnit XML reports generation library")
    (description "Ocaml-junit is a package for the creation of JUnit XML
reports.  It provides a typed API to produce valid reports.  They are supposed
to be accepted by Jenkins.")
    ;; with OCaml linking exception
    (license license:gpl3+)))

(define-public ocaml-junit-alcotest
  (package
    (inherit ocaml-junit)
    (name "ocaml-junit-alcotest")
    (propagated-inputs (list ocaml-odoc ocaml-alcotest ocaml-junit))
    (build-system dune-build-system)
    (arguments
     `(#:package "junit_alcotest"
       #:tests? #f)); tests fail
    (properties `((upstream-name . "junit_alcotest")))
    (synopsis "JUnit XML reports generation for alcotest tests")
    (description "This package generates JUnit XML reports from ocaml-alcotest
test suites.")
    ;; with OCaml linking exception
    (license license:gpl3+)))

(define-public camlzip
  (package
    (name "camlzip")
    (version "1.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/xavierleroy/camlzip")
                     (commit (string-append
                               "rel"
                               (string-join (string-split version #\.) "")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16jnn3czxnvyjngnz167x5kw097k7izdqvkix8qvgvhdmgvqm89b"))))
    (build-system ocaml-build-system)
    (inputs
     (list zlib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:install-target "install-findlib"
       #:make-flags
       ,#~(list "all" "allopt"
                (string-append "INSTALLDIR=" #$output "/lib/ocaml"))))
    (home-page "https://github.com/xavierleroy/camlzip")
    (synopsis "Provides easy access to compressed files")
    (description "Provides easy access to compressed files in ZIP, GZIP and
JAR format.  It provides functions for reading from and writing to compressed
files in these formats.")
    (license license:lgpl2.1+)))

(define-public ocamlmod
  (package
    (name "ocamlmod")
    (version "0.0.9")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1702))
              (sha256
               (base32
                "0cgp9qqrq7ayyhddrmqmq1affvfqcn722qiakjq4dkywvp67h4aa"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ounit" ,ocaml-ounit)
       ("ocamlbuild" ,ocamlbuild)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tests are done during build.
         (delete 'check))))
    (home-page "https://forge.ocamlcore.org/projects/ocamlmod")
    (synopsis "Generate modules from OCaml source files")
    (description "Generate modules from OCaml source files.")
    (license license:lgpl2.1+))) ; with an exception

(define-public ocaml-zarith
  (package
    (name "ocaml-zarith")
    (version "1.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/Zarith")
                     (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jslm1rv1j0ya818yh23wf3bb6hz7qqj9pn5fwl45y9mqyqa01s9"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list perl))
    (inputs
     (list gmp))
    (arguments
     `(#:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _ (invoke "./configure")))
         (add-after 'install 'move-sublibs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/ocaml/site-lib")))
               (mkdir-p (string-append lib "/stublibs"))
               (rename-file (string-append lib "/zarith/dllzarith.so")
                            (string-append lib "/stublibs/dllzarith.so"))))))))
    (home-page "https://forge.ocamlcore.org/projects/zarith/")
    (synopsis "Implements arbitrary-precision integers")
    (description "Implements arithmetic and logical operations over
arbitrary-precision integers.  It uses GMP to efficiently implement arithmetic
over big integers. Small integers are represented as Caml unboxed integers,
for speed and space economy.")
    (license license:lgpl2.1+))) ; with an exception

(define-public ocaml-frontc
  (package
    (name "ocaml-frontc")
    (version "4.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/BinaryAnalysisPlatform/FrontC")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mi1vh4qgscnb470qwidccaqd068j1bqlz6pf6wddk21paliwnqb"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".")))))))
    (native-inputs
     (list ocaml-menhir ocaml-odoc))
    (properties `((upstream-name . "FrontC")))
    (home-page "https://www.irit.fr/FrontC")
    (synopsis "C parser and lexer library")
    (description "FrontC is an OCAML library providing a C parser and lexer.
The result is a syntactic tree easy to process with usual OCAML tree management.
It provides support for ANSI C syntax, old-C K&R style syntax and the standard
GNU CC attributes.  It provides also a C pretty printer as an example of use.")
    (license license:lgpl2.1)))

(define-public ocaml-qcheck
  (package
    (name "ocaml-qcheck")
    (version "0.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/c-cube/qcheck")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r0m5p1dd06lbgfxqdpl1ya4vb8252z7hqkvdi9k444g4rx2ay3p"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-alcotest ocaml-ounit ocaml-ppxlib))
    (native-inputs
     (list ocamlbuild))
    (home-page "https://github.com/c-cube/qcheck")
    (synopsis "QuickCheck inspired property-based testing for OCaml")
    (description "QuickCheck inspired property-based testing for OCaml. This
module checks invariants (properties of some types) over randomly
generated instances of the type. It provides combinators for generating
instances and printing them.")
    (license license:lgpl3+)))

(define-public ocaml-qtest
  (package
    (name "ocaml-qtest")
    (version "2.11.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/vincent-hugot/qtest/")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04ghjshh6104xyglm0db9kv90m62qla5f4bfrlndv6dsvgw3rdjl"))))
    (build-system dune-build-system)
    (propagated-inputs
     `(("ounit" ,ocaml-ounit)
       ("qcheck" ,ocaml-qcheck)))
    (home-page "https://github.com/vincent-hugot/qtest")
    (synopsis "Inline (Unit) Tests for OCaml")
    (description "Qtest extracts inline unit tests written using a special
syntax in comments.  Those tests are then run using the oUnit framework and the
qcheck library.  The possibilities range from trivial tests -- extremely simple
to use -- to sophisticated random generation of test cases.")
    (license license:lgpl3+)))

(define-public ocaml-stringext
  (package
    (name "ocaml-stringext")
    (version "1.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/rgrinberg/stringext")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m09cmn3vrk3gdm60fb730qsygcfyxsyv7gl9xfzck08q1x2x9qx"))))
    (build-system dune-build-system)
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("qtest" ,ocaml-qtest)))
    (home-page "https://github.com/rgrinberg/stringext")
    (synopsis "Extra string functions for OCaml")
    (description "Provides a single module named Stringext that provides a grab
bag of often used but missing string functions from the stdlib.  E.g, split,
full_split, cut, rcut, etc..")
    ;; the only mention of a license in this project is in its `opam' file
    ;; where it says `mit'.
    (license license:expat)))

(define-public dune-bootstrap
  (package
    (name "dune")
    (version "3.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/dune")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12k8k2964s1z05mj71f9imwyvk0jyh5h6mpw4hpyr2d73iw53ink"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; require odoc
       #:make-flags ,#~(list "release"
                             (string-append "PREFIX=" #$output)
                             (string-append "LIBDIR=" #$output
                                            "/lib/ocaml/site-lib"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p "src/dune")
             (invoke "./configure")
             #t)))))
    (home-page "https://github.com/ocaml/dune")
    (synopsis "OCaml build system")
    (description "Dune is a build system for OCaml.  It provides a consistent
experience and takes care of the low-level details of OCaml compilation.
Descriptions of projects, libraries and executables are provided in
@file{dune} files following an s-expression syntax.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public ocaml4.09-dune-bootstrap
  (package-with-ocaml4.09 dune-bootstrap))

(define-public ocaml5.0-dune-bootstrap
  (package-with-ocaml5.0 dune-bootstrap))

(define-public dune-configurator
  (package
    (inherit dune-bootstrap)
    (name "dune-configurator")
    (build-system dune-build-system)
    (arguments
     `(#:package "dune-configurator"
       #:dune ,dune-bootstrap
       ; require ppx_expect
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; When building dune, these directories are normally removed after
         ;; the bootstrap.
         (add-before 'build 'remove-vendor
           (lambda _
             (delete-file-recursively "vendor/csexp")
             (delete-file-recursively "vendor/pp"))))))
    (propagated-inputs
     (list ocaml-csexp))
    (properties `((ocaml4.09-variant . ,(delay ocaml4.09-dune-configurator))
                  (ocaml5.0-variant . ,(delay ocaml5.0-dune-configurator))))
    (synopsis "Dune helper library for gathering system configuration")
    (description "Dune-configurator is a small library that helps writing
OCaml scripts that test features available on the system, in order to generate
config.h files for instance.  Among other things, dune-configurator allows one to:

@itemize
@item test if a C program compiles
@item query pkg-config
@item import #define from OCaml header files
@item generate config.h file
@end itemize")))

(define-public ocaml4.09-dune-configurator
  (package
    (inherit dune-configurator)
    (name "ocaml4.09-dune-configurator")
    (arguments
     `(,@(package-arguments dune-configurator)
       #:dune ,ocaml4.09-dune-bootstrap
       #:ocaml ,ocaml-4.09
       #:findlib ,ocaml4.09-findlib))
    (propagated-inputs
     `(("ocaml-csexp" ,ocaml4.09-csexp)))))

(define-public ocaml5.0-dune-configurator
  (package
    (inherit dune-configurator)
    (name "ocaml5.0-dune-configurator")
    (arguments
     `(,@(package-arguments dune-configurator)
       #:dune ,ocaml5.0-dune-bootstrap
       #:ocaml ,ocaml-5.0
       #:findlib ,ocaml5.0-findlib))
    (propagated-inputs (list ocaml5.0-csexp))))

(define-public dune
  (package
    (inherit dune-bootstrap)
    (propagated-inputs
     (list dune-configurator))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-dune))
                  (ocaml4.09-variant . ,(delay ocaml4.09-dune))
                  (ocaml5.0-variant . ,(delay ocaml5.0-dune))))))

(define-public ocaml4.09-dune
  (package
    (inherit ocaml4.09-dune-bootstrap)
    (propagated-inputs
     (list dune-configurator))))

(define-public ocaml4.07-dune
  (package
    (inherit (package-with-ocaml4.07 dune-bootstrap))
    (version "1.11.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/dune")
                     (commit version)))
              (file-name (git-file-name "dune" version))
              (sha256
               (base32
                "0l4x0x2fz135pljv88zj8y6w1ninsqw0gn1mdxzprd6wbxbyn8wr"))))))

(define-public ocaml5.0-dune
  (package
    (inherit ocaml5.0-dune-bootstrap)
    (propagated-inputs
     (list ocaml5.0-dune-configurator))))

(define-public ocaml-pp
  (package
    (name "ocaml-pp")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml-dune/pp")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ylwb8lbjzj1prnal3c5p404dvh7bv4s19cvgrplnd7s46lvnj50"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-odoc))
    (native-inputs (list ocaml-ppx-expect))
    (home-page "https://github.com/ocaml-dune/pp")
    (synopsis "Pretty-printing library")
    (description
     "This library provides an alternative to the @code{Format} module of the OCaml
standard library.  Pp uses the same concepts of boxes and break hints, and the
final rendering is done to formatter from the @code{Format} module.  However it
defines its own algebra which some might find easier to work with and reason
about.")
    (license license:expat)))

(define-public dune-ordering
  (package
    (inherit dune)
    (name "dune-ordering")
    (source (origin
              (inherit (package-source dune))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (delete-file-recursively "vendor/pp")
                   (delete-file-recursively "vendor/csexp")))))
    (build-system dune-build-system)
    (arguments
     `(#:package "ordering"
       ;; Tests have a cyclic dependency on stdune
       #:tests? #f))
    (properties '())
    (synopsis "Dune element ordering")
    (description "This library represents element ordering in OCaml.")))

(define-public dune-dyn
  (package
    (inherit dune-ordering)
    (name "dune-dyn")
    (build-system dune-build-system)
    (arguments
     `(#:package "dyn"
       ;; Tests have a cyclic dependency on stdune
       #:tests? #f))
    (propagated-inputs (list ocaml-pp dune-ordering))
    (synopsis "Dune dynamic types")
    (description "This library represents dynamic types in OCaml.")))

(define-public dune-stdune
  (package
    (inherit dune-ordering)
    (name "dune-stdune")
    (build-system dune-build-system)
    (arguments
     `(#:package "stdune"
       ;; Tests have a cyclic dependency on itself
       #:tests? #f))
    (propagated-inputs (list dune-dyn ocaml-pp))
    (synopsis "Unstable standard library from Dune")
    (description "This library implements the standard functions used by Dune.")))

(define-public dune-private-libs
  (package
    (inherit dune-ordering)
    (name "dune-private-libs")
    (build-system dune-build-system)
    (arguments
     `(#:package "dune-private-libs"
       #:tests? #f))
    (native-inputs (list dune-stdune ocaml-ppx-expect ocaml-ppx-inline-test))
    (synopsis "Private libraries of Dune")
    (description "This package contains code that is shared between various
dune packages.  However, it is not meant for public consumption and provides
no stability guarantee.")))

(define-public dune-site
  (package
    (inherit dune-ordering)
    (name "dune-site")
    (build-system dune-build-system)
    (arguments
     `(#:package "dune-site"
       #:tests? #f))
    (propagated-inputs (list dune-private-libs))
    (synopsis "Location information embedder")
    (description "This library helps embed location information inside
executables and libraries")))

(define-public ocaml-csexp
  (package
    (name "ocaml-csexp")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml-dune/csexp")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v5y4x1a21193h8q536c0s0d8hv3hyyky4pgzm2dw9807v36s2x4"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f; FIXME: needs ppx_expect, but which version?
       #:dune ,dune-bootstrap
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'chmod
           (lambda _
             (for-each (lambda (file) (chmod file #o644)) (find-files "." ".*"))
             #t)))))
    (propagated-inputs
     (list ocaml-result))
    (properties `((ocaml4.09-variant . ,(delay ocaml4.09-csexp))
                  (ocaml5.0-variant . ,(delay ocaml5.0-csexp))))
    (home-page "https://github.com/ocaml-dune/csexp")
    (synopsis "Parsing and printing of S-expressions in Canonical form")
    (description "This library provides minimal support for Canonical
S-expressions.  Canonical S-expressions are a binary encoding of
S-expressions that is super simple and well suited for communication
between programs.

This library only provides a few helpers for simple applications.  If
you need more advanced support, such as parsing from more fancy input
sources, you should consider copying the code of this library given
how simple parsing S-expressions in canonical form is.

To avoid a dependency on a particular S-expression library, the only
module of this library is parameterised by the type of S-expressions.")
    (license license:expat)))

(define-public ocaml4.09-csexp
  (package
    (inherit ocaml-csexp)
    (name "ocaml4.09-csexp")
    (arguments
     `(#:ocaml ,ocaml-4.09
       #:findlib ,ocaml4.09-findlib
       ,@(substitute-keyword-arguments (package-arguments ocaml-csexp)
           ((#:dune _) ocaml4.09-dune-bootstrap))))
    (propagated-inputs
     `(("ocaml-result" ,ocaml4.09-result)))))

(define-public ocaml5.0-csexp
  (package
    (inherit ocaml-csexp)
    (name "ocaml5.0-csexp")
    (arguments
     `(#:ocaml ,ocaml-5.0
       #:findlib ,ocaml5.0-findlib
       ,@(substitute-keyword-arguments (package-arguments ocaml-csexp)
           ((#:dune _) ocaml5.0-dune-bootstrap))))
    (propagated-inputs
     `(("ocaml-result" ,ocaml5.0-result)))))

(define-public ocaml-migrate-parsetree
  (package
    (name "ocaml-migrate-parsetree")
    (version "2.4.0")
    (home-page "https://github.com/ocaml-ppx/ocaml-migrate-parsetree")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a1qy0ik36j8hpqxvh3fxf4aibjqax989mihj73jncchv8qv4ynq"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ocaml-ppx-derivers ocamlbuild ocaml-result))
    (properties `((upstream-name . "ocaml-migrate-parsetree")))
    (synopsis "OCaml parsetree converter")
    (description "This library converts between parsetrees of different OCaml
versions.  For each version, there is a snapshot of the parsetree and conversion
functions to the next and/or previous version.")
    (license license:lgpl2.1+)))

(define-public ocaml-linenoise
  (package
    (name "ocaml-linenoise")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml-community/ocaml-linenoise")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gk11pflal08kg2dz1b5zrlpnhbxpg2rwf8cknw3vzmq6gsmk2kc"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs (list ocaml-result ocaml-odoc))
    (home-page "https://github.com/ocaml-community/ocaml-linenoise")
    (synopsis "Lightweight readline alternative")
    (description "This package is a line-reading library for OCaml that aims
to replace readline.")
    (license license:bsd-2)))

(define-public ocaml-bitstring
  (package
    (name "ocaml-bitstring")
    (version "4.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/xguerin/bitstring")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mghsl8b2zd2676mh1r9142hymhvzy9cw8kgkjmirxkn56wbf56b"))))
    (build-system dune-build-system)
    (native-inputs
     (list time autoconf automake))
    (propagated-inputs
     (list ocaml-stdlib-shims))
    (arguments
     `(#:package "bitstring"
       #:tests? #f; Tests fail to build
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'upgrade
           (lambda _
             (invoke "dune" "upgrade")
             #t)))))
    (home-page "https://github.com/xguerin/bitstring")
    (synopsis "Bitstrings and bitstring matching for OCaml")
    (description "Adds Erlang-style bitstrings and matching over bitstrings as
a syntax extension and library for OCaml.  You can use this module to both parse
and generate binary formats, files and protocols.  Bitstring handling is added
as primitives to the language, making it exceptionally simple to use and very
powerful.")
    (license license:isc)))

(define-public ocaml-ppx-bitstring
  (package
    (inherit ocaml-bitstring)
    (name "ocaml-ppx-bitstring")
    (arguments
     `(#:package "ppx_bitstring"
       ;; No tests
       #:tests? #f))
    (propagated-inputs (list ocaml-bitstring ocaml-ppxlib))
    (native-inputs (list ocaml-ounit))
    (properties `((upstream-name . "ppx_bitstring")))
    (synopsis "PPX extension for bitstrings and bitstring matching")
    (description
     "This package provides a way to write bitstrings and matching over
bitsrings in Erlang style as primitives to the language.")))

(define-public ocaml-result
  (package
    (name "ocaml-result")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/result")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "166laj8qk7466sdl037c6cjs4ac571hglw4l5qpyll6df07h6a7q"))))
    (build-system dune-build-system)
    (arguments
     `(#:dune ,dune-bootstrap))
    (properties `((ocaml4.09-variant . ,(delay ocaml4.09-result))
                  (ocaml5.0-variant . ,(delay ocaml5.0-result))))
    (home-page "https://github.com/janestreet/result")
    (synopsis "Compatibility Result module")
    (description "Uses the new result type defined in OCaml >= 4.03 while
staying compatible with older version of OCaml should use the Result module
defined in this library.")
    (license license:bsd-3)))

(define-public ocaml4.09-result
  (package
    (inherit ocaml-result)
    (name "ocaml4.09-result")
    (arguments
     `(#:dune ,ocaml4.09-dune-bootstrap
       #:ocaml ,ocaml-4.09
       #:findlib ,ocaml4.09-findlib))))

(define-public ocaml5.0-result
  (package
    (inherit ocaml-result)
    (name "ocaml5.0-result")
    (arguments
     `(#:dune ,ocaml5.0-dune-bootstrap
       #:ocaml ,ocaml-5.0
       #:findlib ,ocaml5.0-findlib))))

(define-public ocaml-iso8601
  (package
    (name "ocaml-iso8601")
    (version "0.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-community/ISO8601.ml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nzadswspizi7s6sf67icn2xgc3w150x8vdg5nk1mjrm2s98n6d3"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-stdlib-shims ocaml-core-unix ocaml-ounit))
    (synopsis "Parser and printer for date-times in ISO8601")
    (description "This package allows parsing of dates that follow the ISO 8601
and RFC 3339 formats in OCaml.")
    (home-page "https://github.com/ocaml-community/ISO8601.ml")
    (license license:expat)))

(define-public ocaml-toml
  (package
    (name "ocaml-toml")
    (version "7.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-toml/To.ml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z2873mj3i6h9cg8zlkipcjab8jympa4c4avhk4l04755qzphkds"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-mdx ocaml-menhir ocaml-iso8601))
    (synopsis "TOML library for OCaml")
    (description
     "This package provides an OCaml library for interacting with files
in the @acronym{TOML, Tom's Obvious Minimal Language} format.  Specifically,
it provides a parser, a serializer, and a pretty printer.")
    (home-page "https://github.com/ocaml-toml/To.ml")
    (license license:expat)))

(define-public ocaml-grain-dypgen
  (package
    (name "ocaml-grain-dypgen")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grain-lang/dypgen")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jyxkvi75nchk5kmhqixmjy70z55gmlqa83pxn0hsv2qxvyqxavw"))))
    (build-system ocaml-build-system)
    (arguments
     (list
      ;; Upstream does not have a test suite.
      #:tests? #f
      #:make-flags #~(let ((out #$output))
                       (list (string-append "OCAMLLIBDIR=" out
                                            "/lib/ocaml/site-lib")
                             (string-append "BINDIR=" out "/bin")
                             (string-append "MANDIR=" out "/share/man")))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure))))
    (properties `((upstream-name . "grain_dypgen")))
    (home-page "https://github.com/grain-lang/dypgen")
    (synopsis "Self-extensible parsers and lexers for OCaml")
    (description
     "This package provides a @acronym{GLR, generalized LR} parser generator
for OCaml.  It is able to generate self-extensible parsers (also called
adaptive parsers) as well as extensible lexers for the parsers it produces.")
    (license license:cecill-b)))

(define-public ocaml-topkg
  (package
    (name "ocaml-topkg")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/topkg/releases/"
                                  "topkg-" version ".tbz"))
              (sha256
               (base32
                "11ycfk0prqvifm9jca2308gw8a6cjb1hqlgfslbji2cqpan09kpq"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list opam-installer ocamlbuild))
    (propagated-inputs
     `(("result" ,ocaml-result)))
    (arguments
     `(#:tests? #f
       #:build-flags '("build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://erratique.ch/software/topkg")
    (synopsis "Transitory OCaml software packager")
    (description "Topkg is a packager for distributing OCaml software. It
provides an API to describe the files a package installs in a given build
configuration and to specify information about the package's distribution,
creation and publication procedures.")
    (license license:isc)))

(define-public ocaml-rresult
  (package
    (name "ocaml-rresult")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/rresult/releases/"
                                  "rresult-" version ".tbz"))
              (sha256
               (base32
                "0h2mjyzhay1p4k7n0mzaa7hlc7875kiy6m1i3r1n03j6hddpzahi"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list opam-installer ocamlbuild))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)))
    (arguments
     `(#:tests? #f
       #:build-flags '("build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://erratique.ch/software/rresult")
    (synopsis "Result value combinators for OCaml")
    (description "Handle computation results and errors in an explicit and
declarative manner, without resorting to exceptions.  It defines combinators
to operate on the result type available from OCaml 4.03 in the standard
library.")
    (license license:isc)))

(define-public ocaml-sqlite3
  (package
    (name "ocaml-sqlite3")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mmottl/sqlite3-ocaml")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ksm0a490315sf0yy8lmva5f3bgr0jnllffanyq89431grpj6x15"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list dune-configurator ocaml-odoc))
    (native-inputs
     (list ocaml-ppx-inline-test pkg-config sqlite))
    (home-page "https://mmottl.github.io/sqlite3-ocaml")
    (synopsis "SQLite3 Bindings for OCaml")
    (description
     "SQLite3-OCaml is an OCaml library with bindings to the SQLite3 client
API.  Sqlite3 is a self-contained, serverless, zero-configuration,
transactional SQL database engine with outstanding performance for many use
cases.  These bindings are written in a way that enables a friendly
coexistence with the old (version 2) SQLite and its OCaml wrapper
@code{ocaml-sqlite}.")
    (license license:expat)))

(define-public ocaml-csv
  (package
    (name "ocaml-csv")
    (version "2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Chris00/ocaml-csv")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0y2hlqlmqs7r4y5mfzc5qdv7gdp3wxbwpz458vf7fj4593vg94cf"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "csv"))
    (home-page "https://github.com/Chris00/ocaml-csv")
    (synopsis "Pure OCaml functions to read and write CSV")
    (description
     "@dfn{Comma separated values} (CSV) is a simple tabular format supported
by all major spreadsheets.  This library implements pure OCaml functions to
read and write files in this format as well as some convenience functions to
manipulate such data.")
    ;; This is LGPLv2.1 with an exception that allows packages statically-linked
    ;; against the library to be released under any terms.
    (license license:lgpl2.1)))

(define-public ocaml-mtime
  (package
    (name "ocaml-mtime")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://erratique.ch/software/mtime/releases/"
                                  "mtime-" version ".tbz"))
              (sha256
               (base32
                "1ss4w3qxsfp51d88r0j7dzqs05dbb1xdx11hn1jl9cvd03ma0g9z"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list ocamlbuild opam-installer))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://erratique.ch/software/mtime")
    (synopsis "Monotonic wall-clock time for OCaml")
    (description "Access monotonic wall-clock time.  It measures time
spans without being subject to operating system calendar time adjustments.")
    (license license:isc)))

(define-public ocaml-calendar
  ;; No tags.
  ;; Commit from 2019-02-03.
  (let ((commit "a447a88ae3c1e9873e32d2a95d3d3e7c5ed4a7da")
        (revision "0"))
    (package
      (name "ocaml-calendar")
      ;; Makefile.in says 2.04.
      (version (git-version "2.04" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ocaml-community/calendar")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "09d9gyqm3zkf3z2m9fx87clqihx6brf8rnzm4yq7c8kf1p572hmc"))))
      (build-system gnu-build-system)
      (arguments
       '(#:test-target "tests"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'make-deterministic
             (lambda _
               (substitute* "Makefile.in"
                 (("`date`") "no date for reproducibility"))))
           (add-before 'install 'set-environment-variables
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (setenv "OCAMLFIND_DESTDIR"
                         (string-append out "/lib/ocaml/site-lib"))
                 (setenv "OCAMLFIND_LDCONF" "ignore")
                 (mkdir-p (string-append
                           out "/lib/ocaml/site-lib/calendar"))))))))
      (native-inputs
       (list autoconf automake))
      (propagated-inputs
       `(("ocaml" ,ocaml)
         ("ocamlfind" ,ocaml-findlib)))
      (home-page "https://github.com/ocaml-community/calendar")
      (synopsis "OCaml library for handling dates and times")
      (description "This package provides types and operations over
dates and times.")
      ;; With linking exception.
      (license license:lgpl2.1+))))

(define-public ocaml-cmdliner
  (package
    (name "ocaml-cmdliner")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://erratique.ch/software/cmdliner/releases/"
                                  "cmdliner-" version ".tbz"))
              (sha256
               (base32
                "1yxm4x34cbi06bfld601ds9drlbnyx0797ym3n6yyh4rlz1qgbm1"))))
    (build-system ocaml-build-system)
    (inputs
     (list ocaml-result))
    (native-inputs
     (list ocamlbuild))
    (arguments
     `(#:tests? #f
       #:make-flags ,#~(list (string-append "LIBDIR=" #$output
                                            "/lib/ocaml/site-lib/cmdliner"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-source-file-order
           (lambda _
             (substitute* "build.ml"
               (("Sys.readdir dir")
                "let a = Sys.readdir dir in Array.sort String.compare a; a"))
             #t)))))
    (home-page "https://erratique.ch/software/cmdliner")
    (synopsis "Declarative definition of command line interfaces for OCaml")
    (description "Cmdliner is a module for the declarative definition of command
line interfaces.  It provides a simple and compositional mechanism to convert
command line arguments to OCaml values and pass them to your functions.  The
module automatically handles syntax errors, help messages and UNIX man page
generation. It supports programs with single or multiple commands and respects
most of the POSIX and GNU conventions.")
    (license license:bsd-3)))

(define-public ocaml-fmt
  (package
    (name "ocaml-fmt")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/fmt/releases/fmt-"
                            version ".tbz"))
        (sha256 (base32
                  "0q8j2in2473xh7k4hfgnppv9qy77f2ih89yp6yhpbp92ba021yzi"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list ocamlbuild
           opam-installer
           ocaml-topkg))
    (propagated-inputs
     (list ocaml-cmdliner
           ocaml-stdlib-shims
           ocaml-uchar))
    (arguments `(#:tests? #f
                 #:build-flags (list "build" "--with-base-unix" "true"
                                     "--with-cmdliner" "true")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (home-page "https://erratique.ch/software/fmt")
    (synopsis "OCaml Format pretty-printer combinators")
    (description "Fmt exposes combinators to devise Format pretty-printing
functions.")
    (license license:isc)))

(define-public ocaml-astring
  (package
    (name "ocaml-astring")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/astring/releases/astring-"
                            version ".tbz"))
        (sha256 (base32
                  "1ykhg9gd3iy7zsgyiy2p9b1wkpqg9irw5pvcqs3sphq71iir4ml6"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list ocamlbuild
           opam-installer
           ocaml-topkg))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://erratique.ch/software/astring")
    (synopsis "Alternative String module for OCaml")
    (description "Astring exposes an alternative String module for OCaml.  This
module balances minimality and expressiveness for basic, index-free, string
processing and provides types and functions for substrings, string sets and
string maps.  The String module exposed by Astring has exception safe functions,
removes deprecated and rarely used functions, alters some signatures and names,
adds a few missing functions and fully exploits OCaml's newfound string
immutability.")
    (license license:isc)))

(define-public ocaml-alcotest
  (package
    (name "ocaml-alcotest")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/alcotest")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0v01vciihd12r30pc4dai70s15p38gy990b4842sn16pvl0ab1az"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "alcotest"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test-format
           (lambda _
             ;; cmdliner changed the format and the tests fail
             (substitute* "test/e2e/alcotest/failing/unknown_option.expected"
               (("`") "'")
               (("\\.\\.\\.") "…")))))))
    (native-inputs
     (list ocamlbuild))
    (propagated-inputs
     (list ocaml-astring
           ocaml-cmdliner
           ocaml-fmt
           ocaml-re
           ocaml-stdlib-shims
           ocaml-uuidm
           ocaml-uutf))
    (home-page "https://github.com/mirage/alcotest")
    (synopsis "Lightweight OCaml test framework")
    (description "Alcotest exposes simple interface to perform unit tests.  It
exposes a simple TESTABLE module type, a check function to assert test
predicates and a run function to perform a list of unit -> unit test callbacks.
Alcotest provides a quiet and colorful output where only faulty runs are fully
displayed at the end of the run (with the full logs ready to inspect), with a
simple (yet expressive) query language to select the tests to run.")
    (license license:isc)))

(define-public ocaml-expect-test-helpers-core
  (package
    (name "ocaml-expect-test-helpers-core")
    (version "0.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/janestreet/expect_test_helpers_core")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bxs3g0zzym8agfcbpg5lmrh6hcb86z861bq40xhhfwqf4pzdbfa"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base
                             ocaml-base-quickcheck
                             ocaml-core
                             ocaml-ppx-jane
                             ocaml-sexp-pretty
                             ocaml-stdio
                             ocaml-re))
    (properties `((upstream-name . "expect_test_helpers_core")))
    (home-page "https://github.com/janestreet/expect_test_helpers_core")
    (synopsis "Helpers for writing expectation tests")
    (description "Helper functions for writing expect tests.")
    (license license:expat)))

(define-public ocaml-ppx-tools
  (package
    (name "ocaml-ppx-tools")
    (version "6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alainfrisch/ppx_tools")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ms2i063cwsm8wcw7jixz3qx2f2prrmf0k44gbksvsgqvm1rl6s2"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (native-inputs
     (list ocaml-cppo))
    (properties `((upstream-name . "ppx_tools")))
    (home-page "https://github.com/alainfrisch/ppx_tools")
    (synopsis "Tools for authors of ppx rewriters and other syntactic tools")
    (description
     "Ppx_tools provides tools for authors of ppx rewriters and other
syntactic tools.")
    (license license:expat)))

(define-public ocaml-yaml
  (package
    (name "ocaml-yaml")
    (version "3.2.0")
    (home-page "https://github.com/avsm/ocaml-yaml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m0i9qdazmziswfw1bz4m1x9mlzqyv336vbrss0c21am4im9n6k6"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-ppx-sexp-conv ocaml-ctypes ocaml-bos))
    (native-inputs (list ocaml-fmt
                         ocaml-sexplib
                         ocaml-logs
                         ocaml-mdx
                         ocaml-alcotest
                         ocaml-crowbar
                         ocaml-junit-alcotest
                         ocaml-ezjsonm))
    (synopsis "Parse and generate YAML 1.1/1.2 files")
    (description
     "This package is an OCaml library to parse and generate the YAML file
format.  It is intended to be interoperable with the @code{Ezjsonm}
JSON handling library, if the simple common subset of Yaml is used.  Anchors and
other advanced Yaml features are not implemented in the JSON compatibility
layer.")
    (license license:isc)))

(define-public ocaml-ppx-deriving-yaml
  (package
    (name "ocaml-ppx-deriving-yaml")
    (version "0.2.1")
    (home-page "https://github.com/patricoferris/ppx_deriving_yaml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cxcqgvyl4ykyl86mf2d4ka6frnq51m1yqy0z5v6vdxkixllf9jd"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-ppxlib ocaml-ppx-deriving ocaml-yaml
                             ocaml-odoc))
    (native-inputs (list ocaml-alcotest ocaml-bos ocaml-mdx ocaml-ezjsonm))
    (properties `((upstream-name . "ppx_deriving_yaml")))
    (synopsis "Yaml PPX Deriver")
    (description
     "This package contains @code{deriving} conversion functions to and from
yaml for OCaml types.")
    (license license:isc)))

(define-public ocaml-ppx-import
  (package
    (name "ocaml-ppx-import")
    (version "1.10.0")
    (home-page "https://github.com/ocaml-ppx/ppx_import")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06srfd6whfwkmjvl6m61kvc65fb7j9b25bhfr1mp338zm87smv5p"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-ppx-deriving ocaml-ppxlib
                             ocaml-ppx-sexp-conv))
    (native-inputs (list ocaml-ounit ocaml-sexplib0))
    (properties `((upstream-name . "ppx_import")))
    (synopsis "Extension for importing declarations from interface files")
    (description
     "Ppx-import is a syntax extension for importing declarations from
interface files.")
    (license license:expat)))

(define-public ocaml-parmap
  (package
    (name "ocaml-parmap")
    (version "1.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rdicosmo/parmap")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0x5gnfap9f7kmgh8j725vxlbkvlplwzbpn8jdx2ywfa3dd6bn6xl"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-odoc))
    (home-page "https://github.com/rdicosmo/parmap")
    (synopsis "Parallel map and fold primitives for OCaml")
    (description
     "Library to perform parallel fold or map taking advantage of multiple
core architectures for OCaml programs.  Drop-in replacement for these
@code{List} operations are provided:

@itemize
@item @code{List.map} -> @code{parmap}
@item @code{List.map} -> @code{parfold}
@item @code{List.mapfold} -> @code{parmapfold}
@end itemize

Also it allows specifying the number of cores to use with the optional
parameter @code{ncores}.")
    (license (list license:lgpl2.0
                   (license:fsdg-compatible "file://LICENSE"
                                            "See LICENSE file for details")))))

(define-public ocaml-pyml
  ;; NOTE: Using commit from master branch as 20220905 does not support
  ;; Python 3.10.
  (let ((revision "0")
        (commit "e33f4c49cc97e7bc6f8e5faaa64cce994470642e"))
    (package
      (name "ocaml-pyml")
      (version (git-version "20220905" revision commit))
      (source
        (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/thierry-martinez/pyml")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1v421i5cvj8mbgrg5cs78bz1yzdprm9r5r41niiy20d3j7j8jx9k"))))
      (build-system dune-build-system)
      (propagated-inputs
       (list ocaml-stdcompat
             python
             python-numpy))
      (home-page "https://github.com/thierry-martinez/pyml")
      (synopsis "Python bindings for OCaml")
      (description "Library that allows OCaml programs to interact with Python
modules and objects.  The library also provides low-level bindings to the
Python C API.

This library is an alternative to @code{pycaml} which is no longer
maintained.  The @code{Pycaml} module provides a signature close to
@code{pycaml}, to ease migration of code to this library.")
      (license license:bsd-2))))

(define-public ocaml-react
  (package
    (name "ocaml-react")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://erratique.ch/software/react/releases/react-"
                            version ".tbz"))
        (sha256 (base32
                  "16cg4byj8lfbbw96dhh8sks5y9n1c3fshz7f2p8m7wgisqax7bf4"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list ocamlbuild opam-installer ocaml-topkg))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://erratique.ch/software/react")
    (synopsis "Declarative events and signals for OCaml")
    (description "React is an OCaml module for functional reactive programming
(FRP).  It provides support to program with time varying values: declarative
events and signals.  React doesn't define any primitive event or signal, it
lets the client choose the concrete timeline.")
    (license license:bsd-3)))

(define-public ocaml-ssl
  (package
    (name "ocaml-ssl")
    (version "0.5.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/savonet/ocaml-ssl")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32
                  "1bg5vagklq6yfxsvcnj2i76xis8hb59088hkic82smyrxdjd1kjs"))))
    (build-system dune-build-system)
    (native-inputs
     (list autoconf automake ocaml-alcotest which))
    (propagated-inputs (list openssl))
    (home-page "https://github.com/savonet/ocaml-ssl/")
    (synopsis "OCaml bindings for OpenSSL")
    (description
     "OCaml-SSL is a set of bindings for OpenSSL, a library for communicating
through Transport Layer Security (@dfn{TLS}) encrypted connections.")
    (license license:lgpl2.1)))

(define-public ocaml-mmap
  (package
    (name "ocaml-mmap")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/mmap")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a7w7l682cbksn2zlmz24gb519x7wb65ivr5vndm9x5pi9fw5pfb"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-bigarray-compat))
    (home-page "https://github.com/mirage/mmap")
    (synopsis "File mapping for OCaml")
    (description "This project provides a @command{Mmap.map_file} function
for mapping files in memory.  This function is the same as the
@command{Unix.map_file} function added in OCaml >= 4.06.")
    (license (list license:qpl license:lgpl2.0))))

(define-public ocaml-psq
  (package
    (name "ocaml-psq")
    (version "0.2.1")
    (home-page "https://github.com/pqwy/psq")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url home-page)
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32
                 "0ahxbzkbq5sw8sqv31c2lil2zny4076q8b0dc7h5slq7i2r23d79"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-qcheck ocaml-alcotest))
    (synopsis "Functional Priority Search Queues for OCaml")
    (description
     "This library provides Functional Priority Search Queues for OCaml.
Typical applications are searches, schedulers and caches.")
    (license license:isc)))

(define-public ocaml-optint
  (package
    (name "ocaml-optint")
    (version "0.3.0")
    (home-page "https://github.com/mirage/optint")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url home-page)
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32
                 "1qj32bcw1in7s6raxdvbmjr3lvj99iwv98x1ar9cwxp4zf8ybfss"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-crowbar ocaml-monolith ocaml-fmt))
    (synopsis "Efficient integer types on 64-bit architectures for OCaml")
    (description
     "This OCaml library provides two new integer types, @code{Optint.t} and
@code{Int63.t}, which guarantee efficient representation on 64-bit
architectures and provide a best-effort boxed representation on 32-bit
architectures.")
    (license license:isc)))

(define-public ocaml-hmap
  (package
    (name "ocaml-hmap")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://erratique.ch/software/hmap/releases/hmap-0.8.1.tbz")
       (sha256
    (base32 "10xyjy4ab87z7jnghy0wnla9wrmazgyhdwhr4hdmxxdn28dxn03a"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags
       (list "build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs (list ocaml-topkg ocamlbuild opam-installer))
    (home-page "https://erratique.ch/software/hmap")
    (synopsis "Heterogeneous value maps for OCaml")
    (description
     "Hmap provides heterogeneous value maps for OCaml.  These maps bind keys to
values with arbitrary types.  Keys witness the type of the value they are bound
to which allows adding and looking up bindings in a type safe manner.")
    (license license:isc)))

(define ocaml-eio
  (package
    (name "ocaml-eio")
    (version "0.8.1")
    (home-page "https://github.com/ocaml-multicore/eio")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32
                "02q9i5wbb2497vd4ypc9d9p4zi3lmx0rsv8faiy7h8dnnzbjjf4z"))))
    (build-system dune-build-system)
    (arguments `(#:package "eio"))
    (propagated-inputs (list ocaml-bigstringaf
                             ocaml-cstruct
                             ocaml-lwt
                             ocaml-lwt-dllist
                             ocaml-logs
                             ocaml-optint
                             ocaml-psq
                             ocaml-fmt
                             ocaml-hmap
                             ocaml-mtime
                             ocaml-odoc))
    (native-inputs (list ocaml-astring
                         ocaml-crowbar
                         ocaml-alcotest
                         ocaml-mdx))
    (synopsis "Effect-based direct-style IO API for OCaml")
    (description "This package provides an effect-based IO API for multicore
OCaml with fibers.")
    (license license:isc)))

(define-public ocaml5.0-eio
  (package-with-ocaml5.0 ocaml-eio))

(define ocaml-eio-luv
  (package
    (inherit ocaml-eio)
    (name "ocaml-eio-luv")
    (arguments `(#:package "eio_luv"))
    (propagated-inputs (list ocaml-eio ocaml-luv))
    (native-inputs (list ocaml-mdx))
    (synopsis "Libuv-based backend for Ocaml Eio")
    (description "@code{Eio_luv} provides a cross-platform backend for
@code{Ocaml Eio}'s APIs using luv (libuv)")))

(define-public ocaml5.0-eio-luv
  (package-with-ocaml5.0 ocaml-eio-luv))

(define-public ocaml-unionfind
  (package
    (name "ocaml-unionfind")
    (version "20220122")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.inria.fr/fpottier/unionfind")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hdh56rbg8vfjd61q09cbmh8l5wmry5ykivg7gsm0v5ckkb3531r"))))
    (build-system dune-build-system)
    (arguments
     (list ;; The test allocates an Array that is too large for OCaml when on a
           ;; 32-bit architecture.
           #:tests? (target-64bit?)))
    (home-page "https://gitlab.inria.fr/fpottier/unionFind")
    (synopsis "Union-find data structure")
    (description "This package provides two union-find data structure
implementations for OCaml.  Both implementations are based on disjoint sets
forests, with path compression and linking-by-rank, so as to guarantee good
asymptotic complexity: every operation requires a quasi-constant number of
accesses to the store.")
    ;; Version 2 only, with linking exception.
    (license license:lgpl2.0)))

(define-public ocaml-uring
  (package
    (name "ocaml-uring")
    (version "0.5")
    (home-page "https://github.com/ocaml-multicore/ocaml-uring")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url home-page)
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32
                 "0ygx8v01bb5808wy6nppg40h1ns8b1f2l585lwc4389z4wrppk95"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-cstruct
           ocaml-fmt
           ocaml-optint))
    (native-inputs
     (list ocaml-lwt
           ocaml-bechamel
           ocaml-logs
           ocaml-cmdliner
           ocaml-mdx))
    (synopsis "OCaml bindings for Linux io_uring")
    (description "This package provides OCaml bindings to the Linux
@code{io_uring} kernel IO interfaces.")
    (license
     (list license:isc license:expat))))

(define ocaml-eio-linux
  (package
    (inherit ocaml-eio)
    (name "ocaml-eio-linux")
    (arguments `(#:package "eio_linux"))
    (propagated-inputs
     (list ocaml-eio
           ocaml-uring
           ocaml-logs
           ocaml-fmt))
    (native-inputs
     (list ocaml-mdx
           ocaml-alcotest
           ocaml-mdx))
    (synopsis "Linux backend for ocaml-eio")
    (description "@code{Eio_linux} provides a Linux io-uring backend for
@code{Ocaml Eio} APIs, plus a low-level API that can be used directly
(in non-portable code).")))

(define-public ocaml5.0-eio-linux
  (package-with-ocaml5.0 ocaml-eio-linux))

(define ocaml-eio-main
  (package
    (inherit ocaml-eio)
    (name "ocaml-eio-main")
    (arguments `(#:package "eio_main"
                 ;; tests require network
                 #:tests? #f))
    (propagated-inputs
     (list ocaml-eio
           ocaml-eio-luv
           ocaml-eio-linux))
    (native-inputs
     (list ocaml-mdx))
    (synopsis "Eio backend selector")
    (description "@code{Eio_main} selects an appropriate backend (e.g.
@samp{eio_linux} or @samp{eio_luv}), depending on your platform.")))

(define-public ocaml5.0-eio-main
  (package-with-ocaml5.0 ocaml-eio-main))

(define-public ocaml-lwt
  (package
    (name "ocaml-lwt")
    (version "5.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ocsigen/lwt")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32
                 "0cfmhw4nsnwba49p06l9fbnbcq75w9fd3kvrr615ihjc9frlmjsy"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "lwt"))
    (native-inputs
     (list ocaml-cppo pkg-config))
    (inputs
     (list glib))
    (propagated-inputs
     (list ocaml-mmap ocaml-ocplib-endian ocaml-result ocaml-seq libev))
    (home-page "https://github.com/ocsigen/lwt")
    (synopsis "Cooperative threads and I/O in monadic style")
    (description "Lwt provides typed, composable cooperative threads.  These
make it easy to run normally-blocking I/O operations concurrently in a single
process.  Also, in many cases, Lwt threads can interact without the need for
locks or other synchronization primitives.")
    (license license:lgpl2.1)))

;; TODO this alias is not ideal but ocaml-lwt already explicitly specifies a
;; package argument and at least this way the importer doesn't try to
;; re-import it.
(define ocaml-lwt-ppx ocaml-lwt)

(define-public ocaml-lwt-dllist
  (package
    (name "ocaml-lwt-dllist")
    (version "1.0.1")
    (home-page "https://github.com/mirage/lwt-dllist")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18bi8fb4yly1pyf43pjvvdhlyzb3wkgxifffx9d1g9y2mwsng6jw"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-lwt))
    (synopsis "OCaml library providing mutable doubly-linked list with Lwt iterators")
    (description "This OCaml library provides an implementation of a mutable
doubly-linked list with Lwt iterators.")
    (license license:expat)))


(define-public ocaml-shared-memory-ring
  (package
    (name "ocaml-shared-memory-ring")
    (version "3.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/shared-memory-ring")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12cpbia39aifnd8rxpsra0lhssqj5qw0zygb5fd8kg58zy2clmrr"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "shared-memory-ring"))
    (propagated-inputs (list ocaml-cstruct ocaml-ppx-cstruct ocaml-lwt-dllist
                             ocaml-mirage-profile))
    (native-inputs (list ocaml-ounit))
    (home-page "https://github.com/mirage/shared-memory-ring")
    (synopsis "Xen-style shared memory rings")
    (description
     "Libraries for creating shared memory producer/consumer rings.  The rings
follow the Xen ABI and may be used to create or implement Xen virtual
devices.")
    (license license:isc)))

(define-public ocaml-shared-memory-ring-lwt
  (package
    (inherit ocaml-shared-memory-ring)
    (name "ocaml-shared-memory-ring-lwt")
    (arguments
     '(#:package "shared-memory-ring-lwt"))
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       ocaml-shared-memory-ring)
                         (append ocaml-shared-memory-ring)))))

(define-public ocaml-xenstore
  (package
    (name "ocaml-xenstore")
    (version "2.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/ocaml-xenstore")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1acld5gxmvnhl5iyyy5ancpm7fv9d6ns1x32krcmb62p2czd00ky"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-cstruct ocaml-ppx-cstruct ocaml-lwt))
    (native-inputs (list ocaml-ounit2))
    (home-page "https://github.com/mirage/ocaml-xenstore")
    (synopsis "Xenstore protocol in pure OCaml")
    (description "Repository contents:
@itemize
@item client library, a merge of the Mirage and XCP ones
@item server library
@item server instance which runs under Unix with libxc
@item server instance which runs on mirage.
@end itemize
The client and the server libraries have sets of unit-tests.")
    ;; Has a linking exception, see LICENSE.md.
    (license license:lgpl2.1)))

(define-public ocaml-mirage-xen
  (package
    (name "ocaml-mirage-xen")
    (version "8.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/mirage-xen")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qydg92dbw8hj4b809apj0f51cjgmamq3zdf34a4wyn5jv85yzyx"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-cstruct
                             ocaml-lwt
                             ocaml-shared-memory-ring-lwt
                             ocaml-xenstore
                             ocaml-lwt-dllist
                             ;; ocaml-mirage-profile  dependency cycle
                             ocaml-io-page
                             ocaml-mirage-runtime
                             ocaml-logs
                             ocaml-fmt
                             ocaml-bheap
                             ocaml-duration))
    (home-page "https://github.com/mirage/mirage-xen")
    (synopsis "Xen core platform libraries for MirageOS")
    (description
     "MirageOS OS library for Xen targets, which handles the main
loop and timers.  It also provides the low level C startup code and C stubs
required by the OCaml code.")
    (license license:isc)))

(define-public ocaml-io-page
  (package
    (name "ocaml-io-page")
    (version "3.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/io-page")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lmvm1whdw5s7rvi7jnjzicrp2j919dkjl856jwyjlq38f7qn0zm"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-cstruct))
    (native-inputs (list pkg-config ocaml-ounit))
    (home-page "https://github.com/mirage/io-page")
    (synopsis "Support for efficient handling of I/O memory pages")
    (description
     "IO pages are page-aligned, and wrapped in the @code{Cstruct} library to
avoid copying the data contained within the page.")
    (license license:isc)))

(define-public ocaml-bheap
  (package
    (name "ocaml-bheap")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/backtracking/bheap")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b8md5zl4yz7j62jz0bf7lwyl0pyqkxqx36ghkgkbkxb4zzggfj1"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-stdlib-shims))
    (home-page "https://github.com/backtracking/bheap")
    (synopsis "Priority queues")
    (description
     "Traditional implementation of priority queues using a binary heap
encoded in a resizable array.")
    (license license:lgpl2.1)))

(define-public ocaml-luv
  (package
    (name "ocaml-luv")
    (version "0.5.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aantron/luv/releases/download/"
                                  version "/luv-" version ".tar.gz"))
              (sha256
               (base32
                "1h2n9iij4mh60sy3g437p1xwqyqpyw72fgh4417d8j9ahq46m7vn"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled configure and libuv.
                  (delete-file-recursively "src/c/vendor")
                  #t))))
    (build-system dune-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'use-system-libuv
                 (lambda _
                   (setenv "LUV_USE_SYSTEM_LIBUV" "yes")))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "dune" "runtest" "--no-buffer" "--force")))))))
    (inputs (list libuv))
    (propagated-inputs (list ocaml-ctypes ocaml-result ocaml-odoc))
    (native-inputs (list ocaml-base ocaml-alcotest))
    (home-page "https://github.com/aantron/luv")
    (synopsis "Binding to libuv: cross-platform asynchronous I/O")
    (description
     "Luv is a binding to libuv, the cross-platform C library that does
asynchronous I/O in Node.js and runs its main loop.  Besides asynchronous I/O,
libuv also supports multiprocessing and multithreading.  Multiple event loops
can be run in different threads.  libuv also exposes a lot of other
functionality, amounting to a full OS API, and an alternative to the standard
module Unix.")
    (license license:expat)))

(define-public ocaml-lwt-react
  (package
    (inherit ocaml-lwt)
    (name "ocaml-lwt-react")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocsigen/lwt")
                     ;; Version from opam
                     (commit "5.6.0")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12sglfwdx4anfslj437g7gxchklgzfvba6i4p478kmqr56j2xd0c"))))
    (arguments
     `(#:package "lwt_react"))
    (properties `((upstream-name . "lwt_react")))
    (propagated-inputs
     (list ocaml-lwt ocaml-react))))

(define-public ocaml-lwt-log
  (package
    (name "ocaml-lwt-log")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/aantron/lwt_log")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mbv5l9gj09jd1c4lr2axcl4v043ipmhjd9xrk27l4hylzfc6d1q"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)); require lwt_ppx
    (propagated-inputs
     `(("lwt" ,ocaml-lwt)))
    (properties `((upstream-name . "lwt_log")))
    (home-page "https://github.com/aantron/lwt_log")
    (synopsis "Logging library")
    (description "This package provides a deprecated logging component for
ocaml lwt.")
    (license license:lgpl2.1)))

(define-public ocaml-logs
  (package
    (name "ocaml-logs")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/logs/releases/"
                                  "logs-" version ".tbz"))
              (sha256
                (base32
                  "1jnmd675wmsmdwyb5mx5b0ac66g4c6gpv5s4mrx2j6pb0wla1x46"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build" "--with-js_of_ocaml" "false")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild opam-installer))
    (propagated-inputs
     `(("fmt" ,ocaml-fmt)
       ("lwt" ,ocaml-lwt)
       ("mtime" ,ocaml-mtime)
       ("result" ,ocaml-result)
       ("cmdliner" ,ocaml-cmdliner)
       ("topkg" ,ocaml-topkg)))
    (home-page "https://erratique.ch/software/logs")
    (synopsis "Logging infrastructure for OCaml")
    (description "Logs provides a logging infrastructure for OCaml.  Logging is
performed on sources whose reporting level can be set independently.  Log
message report is decoupled from logging and is handled by a reporter.")
    (license license:isc)))

(define-public ocaml-fpath
  (package
    (name "ocaml-fpath")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/fpath/releases/"
                                  "fpath-" version ".tbz"))
              (sha256
                (base32
                  "03z7mj0sqdz465rc4drj1gr88l9q3nfs374yssvdjdyhjbqqzc0j"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild opam-installer))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)
       ("astring" ,ocaml-astring)))
    (home-page "https://erratique.ch/software/fpath")
    (synopsis "File system paths for OCaml")
    (description "Fpath is an OCaml module for handling file system paths with
POSIX or Windows conventions.  Fpath processes paths without accessing the
file system and is independent from any system library.")
    (license license:isc)))

(define-public ocaml-bos
  (package
    (name "ocaml-bos")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/bos/releases/"
                                  "bos-" version ".tbz"))
              (sha256
                (base32
                  "0dwg7lpaq30rvwc5z1gij36fn9xavvpah1bj8ph9gmhhddw2xmnq"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild opam-installer))
    (propagated-inputs
     `(("topkg" ,ocaml-topkg)
       ("astring" ,ocaml-astring)
       ("fmt" ,ocaml-fmt)
       ("fpath" ,ocaml-fpath)
       ("logs" ,ocaml-logs)
       ("rresult" ,ocaml-rresult)))
    (home-page "https://erratique.ch/software/bos")
    (synopsis "Basic OS interaction for OCaml")
    (description "Bos provides support for basic and robust interaction with
the operating system in OCaml.  It has functions to access the process
environment, parse command line arguments, interact with the file system and
run command line programs.")
    (license license:isc)))

(define-public ocaml-xml-light
  (package
    (name "ocaml-xml-light")
    (version "2.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ncannasse/xml-light")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "089ywjz84y4p5iln94y54vh03b5fm2zrl2dld1398dyrby96dp6s"))))
    (build-system ocaml-build-system)
    (arguments
     (list #:tests? #f ; There are no tests.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'prefix
                 (lambda _
                   (substitute* "Makefile"
                     (("`\\$\\(OCAMLC\\) -where`")
                      (string-append #$output "/lib/ocaml/site-lib/xml-light")))))
               (delete 'configure) ; no configure
               (add-before 'install 'mkdir
                 (lambda _
                   (mkdir-p (string-append #$output "/lib/ocaml/site-lib/xml-light"))))
               (replace 'install
                 (lambda _
                   (invoke "make" "install_ocamlfind"))))))
    (home-page "https://github.com/ncannasse/xml-light")
    (synopsis "Minimal XML parser & printer for OCaml")
    (description
     "Xml-Light provides functions to parse an XML document into an OCaml data
structure, work with it, and print it back to an XML document.  It also
supports DTD parsing and checking, and is entirely written in OCaml, hence it
does not require additional C libraries.")
    (license license:lgpl2.1+))) ; with linking exception

(define-public ocaml-xmlm
  (package
    (name "ocaml-xmlm")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/xmlm/releases/"
                                  "xmlm-" version ".tbz"))
              (sha256
                (base32
                  "1ynrjba3wm3axscvggrfijfgsznmphhxnkffqch67l9xiqjm44h9"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild ocaml-topkg opam-installer))
    (home-page "https://erratique.ch/software/xmlm")
    (synopsis "Streaming XML codec for OCaml")
    (description "Xmlm is a streaming codec to decode and encode the XML data
format.  It can process XML documents without a complete in-memory
representation of the data.")
    (license license:isc)))

(define-public ocaml-gen
  (package
    (name "ocaml-gen")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/c-cube/gen")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z5nw5wljvcqp8q07h336bbvf9paynia0jsdh4486hlkbmr1ask1"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "gen"))
    (propagated-inputs
     (list ocaml-odoc ocaml-seq))
    (native-inputs
     (list ocaml-qtest ocaml-qcheck))
    (home-page "https://github.com/c-cube/gen/")
    (synopsis "Iterators for OCaml, both restartable and consumable")
    (description "Gen implements iterators of OCaml, that are both restartable
and consumable.")
    (license license:bsd-2)))

(define-public ocaml-sedlex
  (package
    (name "ocaml-sedlex")
    (version "3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocaml-community/sedlex")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vzsmp8mvx9vrgjr5chsk2p2s5ii08c9kizw9ilx78jj30nzamz5"))))
    (build-system dune-build-system)
    (arguments
     (list #:package "sedlex"
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'copy-resources
                 ;; These three files are needed by src/generator/data/dune,
                 ;; but would be downloaded using curl at build time.
                 (lambda* (#:key inputs #:allow-other-keys)
                   (with-directory-excursion "src/generator/data"
                     ;; Newer versions of dune emit an error if files it wants to
                     ;; build already exist. Delete the dune file so dune doesn't
                     ;; complain.
                     (delete-file "dune")
                     (for-each
                      (lambda (file)
                        (copy-file (search-input-file inputs file)
                                   (basename file)))
                      '("share/ucd/extracted/DerivedGeneralCategory.txt"
                        "share/ucd/DerivedCoreProperties.txt"
                        "share/ucd/PropList.txt")))))
               (add-before 'build 'chmod
                 (lambda _
                   (for-each (lambda (file) (chmod file #o644)) (find-files "." ".*")))))))
    (native-inputs (list ocaml-ppx-expect))
    (propagated-inputs
     (list ocaml-gen ocaml-ppxlib ocaml-uchar))
    (inputs
     (list ucd))
    (home-page "https://www.cduce.org/download.html#side")
    (synopsis "Lexer generator for Unicode and OCaml")
    (description "Lexer generator for Unicode and OCaml.")
    (license license:expat)))

(define-public ocaml-sedlex-2
  (package
    (inherit ocaml-sedlex)
    (name "ocaml-sedlex")
    (version "2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocaml-community/sedlex")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z8mmk1idh9hjhh2b9rp5b1h8kmzcxhagqkw0pvxn6ykx1brskq1"))))
    (arguments
     (substitute-keyword-arguments (package-arguments ocaml-sedlex)
       ((#:tests? _ #t) #f)))               ; no tests
    (native-inputs '())))

(define-public ocaml-uchar
  (package
    (name "ocaml-uchar")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ocaml/uchar/releases/download/v"
                            version "/uchar-" version ".tbz"))
        (sha256 (base32
                  "1w2saw7zanf9m9ffvz2lvcxvlm118pws2x1wym526xmydhqpyfa7"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "native=true" "native-dynlink=true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild opam-installer))
    (home-page "https://github.com/ocaml/uchar")
    (synopsis "Compatibility library for OCaml's Uchar module")
    (description "The uchar package provides a compatibility library for the
`Uchar` module introduced in OCaml 4.03.")
    (license license:lgpl2.1)))

(define-public ocaml-uutf
  (package
    (name "ocaml-uutf")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/uutf/releases/"
                                  "uutf-" version ".tbz"))
              (sha256
                (base32
                  "0s05r8ggp1g97zq4rnvbxzj22pv8ld0k5wsdw662jw0y7mhsawl7"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild
           opam-installer
           ocaml-topkg))
    (propagated-inputs
     `(("uchar" ,ocaml-uchar)
       ("cmdliner" ,ocaml-cmdliner)))
    (home-page "https://erratique.ch/software/uutf")
    (synopsis "Non-blocking streaming Unicode codec for OCaml")
    (description "Uutf is a non-blocking streaming codec to decode and encode
the UTF-8, UTF-16, UTF-16LE and UTF-16BE encoding schemes.  It can efficiently
work character by character without blocking on IO.  Decoders perform character
position tracking and support newline normalization.

Functions are also provided to fold over the characters of UTF encoded OCaml
string values and to directly encode characters in OCaml Buffer.t values.")
    (license license:isc)))

(define-public ocaml-uunf
  (package
    (name "ocaml-uunf")
    (version "15.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uunf/releases/uunf-"
                           version".tbz"))
       (sha256
        (base32
         "1s5svvdqfbzw16rf1h0zm9n92xfdr0qciprd7lcjza8z1hy6pyh7"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags (list "build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; reported and fixed upstream, will be available in next version.
         (add-before 'build 'fix-test
           (lambda _
             (substitute* "test/test.ml"
               (("test/NormalizationTest.txt") "-"))))
         (add-before 'check 'check-data
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-file (assoc-ref inputs "NormalizationTest.txt")
                        "test/NormalizationTest.txt")
             #t)))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam-installer" ,opam-installer)
       ("topkg" ,ocaml-topkg)
       ;; Test data is otherwise downloaded with curl
       ("NormalizationTest.txt"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://www.unicode.org/Public/"
                               version
                               "/ucd/NormalizationTest.txt"))
           (file-name (string-append "NormalizationTest-" version ".txt"))
           (sha256
              (base32 "09pkawfqpgy2xnv2nkkgmxv53rx4anprg65crbbcm02a2p6ci6pv"))))))
    (propagated-inputs (list ocaml-uutf))
    (home-page "https://erratique.ch/software/uunf")
    (synopsis "Unicode text normalization for OCaml")
    (description
     "Uunf is an OCaml library for normalizing Unicode text.  It supports all
Unicode normalization forms.  The library is independent from any
IO mechanism or Unicode text data structure and it can process text
without a complete in-memory representation.")
    (license license:isc)))

(define-public ocaml-jsonm
  (package
    (name "ocaml-jsonm")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/jsonm/releases/"
                                  "jsonm-" version ".tbz"))
              (sha256
                (base32
                  "1176dcmxb11fnw49b7yysvkjh0kpzx4s48lmdn5psq9vshp5c29w"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild
           opam-installer
           ocaml-topkg))
    (propagated-inputs
     `(("uutf" ,ocaml-uutf)
       ("cmdliner" ,ocaml-cmdliner)))
    (home-page "https://erratique.ch/software/jsonm")
    (synopsis "Non-blocking streaming JSON codec for OCaml")
    (description "Jsonm is a non-blocking streaming codec to decode and encode
the JSON data format.  It can process JSON text without blocking on IO and
without a complete in-memory representation of the data.")
    (license license:isc)))

(define-public ocaml-ocp-indent
  (package
    (name "ocaml-ocp-indent")
    (version "1.8.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/OCamlPro/ocp-indent")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1dvcl108ir9nqkk4mjm9xhhj4p9dx9bmg8bnms54fizs1x3x8ar3"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-cmdliner))
    (home-page "https://www.typerex.org/ocp-indent.html")
    (synopsis "Tool to indent OCaml programs")
    (description
      "Ocp-indent is based on an approximate, tolerant OCaml parser
and a simple stack machine.  Presets and configuration options are available,
with the possibility to set them project-wide.  It supports the most common
syntax extensions, and it is extensible for others.

This package includes:

@itemize
@item An indentor program, callable from the command-line or from within editors,
@item Bindings for popular editors,
@item A library that can be directly used by editor writers, or just for
      fault-tolerant and approximate parsing.
@end itemize")
    (license license:lgpl2.1)))

(define-public ocaml-ocp-index
  (package
    (name "ocaml-ocp-index")
    (version "1.3.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/OCamlPro/ocp-index")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "031b3s8ppqkpw1n6h87h6jzjkmny6yig9wfimmgwnljafcc83d3b"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "ocp-index"))
    (propagated-inputs
     (list ocaml-ocp-indent ocaml-re ocaml-cmdliner))
    (native-inputs
     (list ocaml-cppo))
    (home-page "https://www.typerex.org/ocp-index.html")
    (synopsis "Lightweight completion and documentation browsing for OCaml libraries")
    (description "This package includes only the @code{ocp-index} library
and command-line tool.")
    ;; All files in libs/ are GNU lgpl2.1
    ;; For static linking, clause 6 of LGPL is lifted
    ;; All other files under GNU gpl3
    (license (list license:gpl3+
                   license:lgpl2.1+))))

(define-public ocaml-domain-name
  (package
    (name "ocaml-domain-name")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hannesm/domain-name/")
                    (commit (string-append "v" version))))
              (file-name name)
              (sha256
               (base32
                "1a669zz1pc7sqbi1c13jsnp8algcph2b8gr5fjrjhyh3p232770k"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-alcotest))
    (home-page "https://github.com/hannesm/domain-name")
    (synopsis "RFC 1035 Internet domain name data structure and parser")
    (description
     "Parses and constructs RFC compliant domain names.  The invariants on the
length of domain names are preserved throughout the module.")
    (license license:isc)))

(define-public ocaml-macaddr
  (package
    (name "ocaml-macaddr")
    (version "5.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/ocaml-ipaddr/")
                    (commit (string-append "v" version))))
              (file-name name)
              (sha256
               (base32
                "1zgwx0ms3l4k4dzwnkrwq4zzqjrddjsvqn66mbd0rm6aq1ib019d"))))
    (build-system dune-build-system)
    (arguments '(#:package "macaddr"))
    (propagated-inputs (list ocaml-cstruct ocaml-domain-name))
    (native-inputs (list ocaml-ounit2 ocaml-ppx-sexp-conv))
    (home-page "https://github.com/mirage/ocaml-ipaddr")
    (synopsis "OCaml library for manipulation of MAC address representations")
    (description
     "Features:
@itemize
@item MAC-48 (Ethernet) address support
@item @code{Macaddr} is a @code{Map.OrderedType}
@item All types have sexplib serializers/deserializers optionally via the
@code{Macaddr_sexp} library
@end itemize")
    (license license:isc)))

(define-public ocaml-ipaddr
  ;; same repo and versions as ocaml-macaddr
  (package
    (inherit ocaml-macaddr)
    (name "ocaml-ipaddr")
    (arguments '(#:package "ipaddr"))
    (propagated-inputs (list ocaml-macaddr ocaml-domain-name))
    (synopsis
     "Library for manipulation of IP (and MAC) address representations")
    (description
     "IP address types with serialization, supporting a wide range of RFCs.")
    (license license:isc)))

(define-public ocaml-pecu
  (package
    (name "ocaml-pecu")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri
               "https://github.com/mirage/pecu/releases/download/v0.6/pecu-v0.6.tbz")
              (sha256
               (base32
                "1iz5jj9lyl1pah8dfni4wv0qml0ir5zknv4zhw7v50sc8kdbglm9"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-fmt ocaml-alcotest ocaml-crowbar ocaml-astring))
    (home-page "https://github.com/mirage/pecu")
    (synopsis "Encoder/Decoder of Quoted-Printable (RFC2045 & RFC2047)")
    (description
     "This package provides a non-blocking encoder/decoder of Quoted-Printable
according to RFC2045 and RFC2047 (about encoded-word).  Useful to translate
contents of emails.")
    (license license:expat)))

(define-public ocaml-emile
  (package
    (name "ocaml-emile")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri
               "https://github.com/mirage/emile/releases/download/v1.1/emile-v1.1.tbz")
              (sha256
               (base32
                "0r1141makr0b900aby1gn0fccjv1qcqgyxib3bzq8fxmjqwjan8p"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-angstrom
                             ocaml-ipaddr
                             ocaml-base64
                             ocaml-pecu
                             ocaml-bigstringaf
                             ocaml-uutf))
    (native-inputs (list ocaml-alcotest))
    (home-page "https://github.com/mirage/emile")
    (synopsis "Parser of email address according RFC822")
    (description
     "This package provides a parser of email address according RFC822, RFC2822,
RFC5321 and RFC6532.  It handles UTF-8 email addresses and encoded-word
according RFC2047.")
    (license license:expat)))

(define-public ocaml-parse-argv
  (package
    (name "ocaml-parse-argv")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/parse-argv")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16n18zik6vkfnhv8jaigr90fwp1ykg23p61aqchym0jil4i4yq01"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-astring))
    (native-inputs (list ocaml-ounit))
    (home-page "https://github.com/mirage/parse-argv")
    (synopsis "Process strings into sets of command-line arguments")
    (description "Small implementation of a simple argv parser.")
    (license license:isc)))

(define-public ocaml-functoria-runtime
  (package
    (name "ocaml-functoria-runtime")
    (version "4.3.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/mirage/mirage/")
         (commit (string-append "v" version))))
       (file-name (git-file-name "mirage" version))
       (sha256
        (base32
         "09mqbffrhnklbc50gaflkwb3h1xysqqiwb84a9q1phjl038pic6r"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "functoria-runtime"
       ;; TODO
       ;; again, requires opam for tests, which needs network access.
       ;; most other tests seem to pass.
       #:tests? #f))
    (propagated-inputs
     (list ocaml-cmdliner ocaml-fmt ocaml-logs ocaml-bos ocaml-ipaddr
           ocaml-emile ocaml-uri))
    (native-inputs
     (list ocaml-alcotest))
    (home-page "https://github.com/mirage/mirage")
    (synopsis "Runtime support library for functoria-generated code")
    (description
     "This is the runtime support library for code generated by functoria.")
    (license license:isc)))

(define-public ocaml-mirage-runtime
  (package
    (inherit ocaml-functoria-runtime)
    (name "ocaml-mirage-runtime")
    (build-system dune-build-system)
    (arguments
     '(#:package "mirage-runtime"
       ;; TODO again, wants opam, other tests seem to pass
       ;; look for a way to disable tests that want network access
       #:tests? #f))
    (propagated-inputs (list ocaml-ipaddr ocaml-functoria-runtime ocaml-fmt
                             ocaml-logs ocaml-lwt))
    (native-inputs (list ocaml-alcotest))
    (home-page "https://github.com/mirage/mirage")
    (synopsis
     "The base MirageOS runtime library, part of every MirageOS unikernel")
    (description
     "This package provides a bundle of useful runtime functions for
applications built with MirageOS")
    (license license:isc)))

(define-public ocaml-functoria
  (package
    (inherit ocaml-functoria-runtime)
    (name "ocaml-functoria")
    (build-system dune-build-system)
    (arguments
     '(#:package "functoria"
       ;; TODO again, wants opam, other tests seem to pass
       ;; look for a way to disable tests that want network access
       #:tests? #f))
    (propagated-inputs (list ocaml-cmdliner ocaml-rresult ocaml-result
                             ocaml-astring ocaml-fmt ocaml-logs ocaml-bos
                             ocaml-fpath ocaml-emile ocaml-uri))
    (native-inputs (list ocaml-alcotest ocaml-functoria-runtime))
    (home-page "https://github.com/mirage/mirage")
    (synopsis
     "DSL to organize functor applications")
    (description
     "DSL to describe a set of modules and functors, their types and
how to apply them in order to produce a complete application.  The main use
case is mirage.")
    (license license:isc)))

(define-public ocaml-mirage
  (package
    (inherit ocaml-functoria-runtime)
    (name "ocaml-mirage")
    (build-system dune-build-system)
    (arguments
     '(#:package "mirage"
       ;; TODO again, wants opam, other tests seem to pass
       ;; look for a way to disable tests that want network access
       #:tests? #f))
    (propagated-inputs
     (list ocaml-astring ocaml-bos ocaml-functoria ocaml-ipaddr ocaml-logs
           ocaml-mirage-runtime ocaml-opam-monorepo))
    (native-inputs (list ocaml-alcotest ocaml-fmt))
    (home-page "https://github.com/mirage/mirage")
    (synopsis
     "The MirageOS library operating system")
    (description
     "Library operating system that constructs unikernels for secure,
high-performance network applications across a variety of cloud computing and
mobile platforms.  Code can be developed on a normal OS and then compiled into
a fully-standalone, specialised unikernel.")
    (license license:isc)))

(define-public ocaml-mirage-bootvar-unix
  (package
    (name "ocaml-mirage-bootvar-unix")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/mirage-bootvar-unix")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vi13q0z5ffv5hf4q5lfvkia6j2s5520px0s2x4dbjgd52icizrz"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-lwt ocaml-parse-argv))
    (home-page "https://github.com/mirage/mirage-bootvar-unix")
    (synopsis "Unix implementation of MirageOS Bootvar interface")
    (description "Library for passing boot parameters from Solo5 to MirageOS.")
    (license license:isc)))

(define-public ocaml-duration
  (package
    (name "ocaml-duration")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/hannesm/duration/")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vvxi0ipxmdz1k4h501brvccniwf3wpc32djbccyyrzraiz7qkff"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-alcotest))
    (home-page "https://github.com/hannesm/duration")
    (synopsis "Conversions to various time units")
    (description
     "This package provides a duration is represented in nanoseconds as an
unsigned 64 bit integer.  This has a range of up to 584 years.  Functions
provided check the input and raise on negative or out of bound input.")
    (license license:isc)))

(define-public ocaml-mirage-time
  (package
    (name "ocaml-mirage-time")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mirage/mirage-time/releases/download/v"
                    version "/mirage-time-v3.0.0.tbz"))
              (sha256
               (base32
                "0z5xkhlgyhm22wyhwpf9r0rn4125cc3cxj6ccavyiiz2b2dr8h0d"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-lwt ocaml-duration))
    (home-page "https://github.com/mirage/mirage-time")
    (synopsis "Time operations for MirageOS")
    (description
     "Defines the signature for time-related operations for MirageOS.")
    (license license:isc)))

(define-public ocaml-mirage-clock
  (package
    (name "ocaml-mirage-clock")
    (version "4.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/mirage-clock")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rkara9i3dvnzrb8jl2vkx8hgamvxnksin67wmhbv9d4i758amjy"))))
    (build-system dune-build-system)
    (home-page "https://github.com/mirage/mirage-clock")
    (synopsis "Libraries and module types for portable clocks")
    (description
     "This library implements portable support for an operating system
timesource that is compatible with the MirageOS library interfaces.  It
implements an @code{MCLOCK} module that represents a monotonic timesource
since an arbitrary point, and @code{PCLOCK} which counts time since the Unix
epoch.")
    (license license:isc)))

(define-public ocaml-ptime
  (package
    (name "ocaml-ptime")
    ;; TODO 1.1.0 has some issues, so for now we are stuck with 0.8.5
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri
               "https://erratique.ch/software/ptime/releases/ptime-0.8.5.tbz")
              (sha256
               (base32
                "1fxq57xy1ajzfdnvv5zfm7ap2nf49znw5f9gbi4kb9vds942ij27"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags (list "build" "--with-js_of_ocaml" "true" "--tests"
                           "true")
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (propagated-inputs (list ocaml-result js-of-ocaml))
    (native-inputs (list ocaml-findlib ocamlbuild ocaml-topkg opam-installer))
    (home-page "https://erratique.ch/software/ptime")
    (synopsis "POSIX time for OCaml")
    (description
     "Ptime offers platform independent POSIX time support in pure OCaml.  It
provides a type to represent a well-defined range of POSIX timestamps with
picosecond precision, conversion with date-time values, conversion with RFC
3339 timestamps and pretty printing to a human-readable, locale-independent
representation.")
    (license license:isc)))

(define-public ocaml-mirage-unix
  (package
    (name "ocaml-mirage-unix")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mirage/mirage-unix/releases/download/v"
                    version "/mirage-unix-5.0.1.tbz"))
              (sha256
               (base32
                "1y44hvsd5lxqbazwkv9n6cn936lpn8l7v82wf55w4183fp70nnjk"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-lwt ocaml-duration ocaml-mirage-runtime))
    (home-page "https://github.com/mirage/mirage-unix")
    (synopsis "Unix core platform libraries for MirageOS")
    (description
     "This package provides the MirageOS `OS` library for Unix targets, which
handles the main loop and timers.")
    (license license:isc)))

(define-public ocaml-mirage-profile-unix
  (package
    (name "ocaml-mirage-profile-unix")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/mirage-profile/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11p3ai8g993algds9mbg4xf3is0agqah127r69fb7rm35dryzq95"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "mirage-profile-unix"
       #:tests? #f ;depends on ocaml-mirage-profile which would form a loop
       #:phases (modify-phases %standard-phases
                  ;; TODO is there a way to do this with dune build flags?
                  (add-after 'unpack 'disable-xen
                    (lambda _
                      ;; this way it is not detected as a build target
                      (rename-file "xen" "_xen"))))))
    (propagated-inputs (list ocaml-cstruct ocaml-ocplib-endian ocaml-lwt
                             ocaml-mtime ocaml-ppx-cstruct))
    (native-inputs (list ocaml-ppx-cstruct))
    (home-page "https://github.com/mirage/mirage-profile")
    (synopsis "Collects Ocaml/Lwt profiling information in CTF format")
    (description
     "Used to trace execution of OCaml/Lwt programs (such as Mirage
unikernels) at the level of Lwt threads.  The traces can be viewed using
JavaScript or GTK viewers provided by mirage-trace-viewer or processed by
tools supporting the Common Trace Format.
When compiled against a normal version of Lwt, OCaml's cross-module inlining
will optimise these calls away, meaning there should be no overhead in the
non-profiling case.")
    (license license:bsd-2)))

(define-public ocaml-mirage-profile
  (package
    (inherit ocaml-mirage-profile-unix)
    (name "ocaml-mirage-profile")
    (arguments
     '(#:package "mirage-profile"
       ;; TODO cyclic dependency with mirage-profile
       ;; It could be broken using package variants, if not for
       ;; propagated inputs leading to version conflicts.
       #:tests? #f))
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       ocaml-mirage-profile-unix)
                         (append ocaml-mirage-profile-unix)))))

(define-public ocaml-mirage-logs
  (package
    (name "ocaml-mirage-logs")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mirage/mirage-logs/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wv2hz1dj38jzc8nabin9p8im43ghy8f3crv7rf9szyyzyrdanp2"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-logs ocaml-ptime ocaml-mirage-clock
                             ocaml-mirage-profile ocaml-lwt))
    (native-inputs (list ocaml-alcotest))
    (home-page "https://github.com/mirage/mirage-logs")
    (synopsis
     "Reporter for the Logs library that writes to stderr with timestamps")
    (description
     "Uses a Mirage @code{CLOCK} to write timestamped log messages.  It can
also log only important messages to the console, while writing all received
messages to a ring buffer which is displayed if an exception occurs.  If
tracing is enabled (via mirage-profile), it also writes each log message to
the trace buffer.")
    (license license:isc)))

(define-public ocaml-ocurl
  (package
    (name "ocaml-ocurl")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ygrek.org.ua/p/release/ocurl/ocurl-"
                                  version ".tar.gz"))
              (sha256
                (base32
                  "0qvpsqbq4qbd397n0nlv9cwlqfyjw7gfb5mmq1awvnklr0c9fdg0"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-/bin/sh
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
               (("-/bin/sh") (string-append "-" (which "bash")))))))))
    (native-inputs
     (list pkg-config))
    (inputs (list curl))
    (home-page "http://ocurl.forge.ocamlcore.org/")
    (synopsis "OCaml bindings for libcurl")
    (description "Client-side URL transfer library, supporting HTTP and a
multitude of other network protocols (FTP/SMTP/RTSP/etc).")
    (license license:isc)))

(define-public ocaml-base64
  (package
    (name "ocaml-base64")
    (version "3.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/ocaml-base64")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jq349jp663hq51a941afr2y4yyh34r19zsxla73ks9bywj4mm2q"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-alcotest ocaml-bos ocaml-rresult))
    (home-page "https://github.com/mirage/ocaml-base64")
    (synopsis "Base64 encoding for OCaml")
    (description "Base64 is a group of similar binary-to-text encoding schemes
that represent binary data in an ASCII string format by translating it into a
radix-64 representation.  It is specified in RFC 4648.")
    (license license:isc)))

;; A variant without tests that is used to prevent a cyclic dependency when
;; compiling ocaml-dose3.
(define ocaml-base64-boot
  (package
    (inherit ocaml-base64)
    (arguments `(#:tests? #f))
    (native-inputs '())))

(define-public ocamlify
  (package
    (name "ocamlify")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.ocamlcore.org/ocamlify/ocamlify/"
                           version "/ocamlify-" version ".tar.gz"))
       (sha256
        (base32 "1f0fghvlbfryf5h3j4as7vcqrgfjb4c8abl5y0y5h069vs4kp5ii"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           ;; This package uses pre-generated setup.ml by oasis, but is
           ;; a dependency of oasis.  the pre-generated setup.ml is broken
           ;; with recent versions of OCaml, so we perform a bootstrap instead.
           (lambda _
             (substitute* "src/OCamlifyConfig.ml.ab"
               (("$pkg_version") ,version))
             (rename-file "src/OCamlifyConfig.ml.ab" "src/OCamlifyConfig.ml")
             (with-directory-excursion "src"
               (invoke "ocamlc" "OCamlifyConfig.ml" "ocamlify.ml" "-o"
                       "ocamlify"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "src/ocamlify" bin)
               #t))))))
    (home-page "https://forge.ocamlcore.org/projects/ocamlify")
    (synopsis "Include files in OCaml code")
    (description "OCamlify creates OCaml source code by including
whole files into OCaml string or string list.  The code generated can be
compiled as a standard OCaml file.  It allows embedding external resources as
OCaml code.")
    (license license:lgpl2.1+))); with the OCaml static compilation exception

(define-public omake
  (package
    (name "omake")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.camlcity.org/download/"
                                  "omake-" version ".tar.gz"))
              (sha256
               (base32
                "1i7pcv53kqplrbdx9mllrhbv4j57zf87xwq18r16cvn1lbc6mqal"))
              (patches (search-patches "omake-fix-non-determinism.patch"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:make-flags
       ,#~(list (string-append "PREFIX=" #$output))
       #:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-makefile
                     (lambda* (#:key outputs #:allow-other-keys)
                       (substitute* "mk/osconfig_unix.mk"
                                    (("CC = cc") "CC = gcc")))))))
    (native-inputs (list hevea))
    (home-page "http://projects.camlcity.org/projects/omake.html")
    (synopsis "Build system designed for scalability and portability")
    (description "Similar to make utilities you may have used, but it features
many additional enhancements, including:

@enumerate
@item Support for projects spanning several directories or directory hierarchies.
@item Fast, reliable, automated, scriptable dependency analysis using MD5 digests,
      with full support for incremental builds.
@item Dependency analysis takes the command lines into account — whenever the
      command line used to build a target changes, the target is considered
      out-of-date.
@item Fully scriptable, includes a library that providing support for standard
      tasks in C, C++, OCaml, and LaTeX projects, or a mixture thereof.
@end enumerate")
    (license (list license:lgpl2.1 ; libmojave
                   license:expat ; OMake scripts
                   license:gpl2)))) ; OMake itself, with ocaml linking exception
                                    ; see LICENSE.OMake

(define-public ocaml-benchmark
  (package
    (name "ocaml-benchmark")
    (version "1.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Chris00/ocaml-benchmark")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0d0vdfjgjzf1y6wkd714d8b0piv1z9qav5ahsapynqzk4b4ahhnp"))))
    (build-system dune-build-system)
    (home-page "https://github.com/Chris00/ocaml-benchmark")
    (synopsis "Benchmark running times of code")
    (description
      "This module provides a set of tools to measure the running times of
your functions and to easily compare the results.  A statistical test
is used to determine whether the results truly differ.")
    (license license:lgpl3+)))

(define-public ocaml-bechamel
  (package
    (name "ocaml-bechamel")
    (version "0.3.0")
    (home-page "https://github.com/mirage/bechamel")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url home-page)
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1x7sf45iy5dzx7kknbkkvpna42rcwpj5p55y0nqsg2fb4srj0b1q"))))
    (build-system dune-build-system)
    (arguments `(#:package "bechamel"))
    (propagated-inputs (list ocaml-fmt ocaml-stdlib-shims))
    (synopsis "Yet Another Benchmark in OCaml")
    (description
     "BEnchmark for a CHAMEL/camel/caml which is agnostic to the system.  It's a
micro-benchmark tool for OCaml which lets the user to re-analyzes and prints
samples.")
    (license license:expat)))

(define-public ocaml-batteries
  (package
    (name "ocaml-batteries")
    (version "3.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml-batteries-team/batteries-included")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07387jp93civ9p1q2ixmq8qkzzyssp94ssxd4w2ndvkg1nr6kfcl"))))
    (build-system ocaml-build-system)
    (propagated-inputs (list ocaml-num))
    (native-inputs
     (list ocamlbuild ocaml-benchmark ocaml-qcheck ocaml-qtest))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (for-each make-file-writable (find-files "." "."))))
         (add-before 'build 'fix-nondeterminism
           (lambda _
             (substitute* "setup.ml"
               (("Sys.readdir dirname")
                "let a = Sys.readdir dirname in Array.sort String.compare a; a"))
             #t)))))
    (home-page "http://batteries.forge.ocamlcore.org/")
    (synopsis "Development platform for the OCaml programming language")
    (description "Define a standard set of libraries which may be expected on
every compliant installation of OCaml and organize these libraries into a
hierarchy of modules.")
    (license license:lgpl2.1+)))

(define-public ocaml-pcre
  (package
    (name "ocaml-pcre")
    (version "7.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/mmottl/pcre-ocaml")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "048k1rl17fcml000yh8fnghk1a06h14lbyrnk9nbigxsymrz6cq2"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (propagated-inputs
     (list dune-configurator pcre))
    (native-inputs
     `(("pcre:bin" ,pcre "bin")))
    (home-page "https://mmottl.github.io/pcre-ocaml")
    (synopsis
      "Bindings to the Perl Compatibility Regular Expressions library")
    (description "Pcre-ocaml offers library functions for string
pattern matching and substitution, similar to the functionality
offered by the Perl language.")
    ;; With static linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-expect
  (package
    (name "ocaml-expect")
    (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1736))
              (sha256
               (base32
                "098qvg9d4yrqzr5ax291y3whrpax0m3sx4gi6is0mblc96r9yqk0"))))
    (arguments
     `(#:tests? #f))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("ocaml-num" ,ocaml-num)
       ("ocaml-pcre" ,ocaml-pcre)
       ("ounit" ,ocaml-ounit)))
    (propagated-inputs
     `(("batteries" ,ocaml-batteries)))
    (home-page "https://forge.ocamlcore.org/projects/ocaml-expect/")
    (synopsis "Simple implementation of expect")
    (description "This package provides utilities for building unitary testing
of interactive program.  You can match the question using a regular expression
or a timeout.")
    (license license:lgpl2.1+))) ; with the OCaml static compilation exception

(define-public ocaml-stdcompat
  (package
    (name "ocaml-stdcompat")
    (version "19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thierry-martinez/stdcompat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        #~(for-each delete-file '("Makefile.in" "configure")))
       (sha256
        (base32
         "0r9qcfjkn8634lzxp5bkagzwsi3vmg0hb6vq4g1p1515rys00h1b"))))
    (build-system dune-build-system)
    (arguments
     (list #:imported-modules `((guix build gnu-build-system)
                                ,@%dune-build-system-modules)
           #:modules '((guix build dune-build-system)
                       ((guix build gnu-build-system) #:prefix gnu:)
                       (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'bootstrap
                 (assoc-ref gnu:%standard-phases 'bootstrap))
               (add-before 'build 'prepare-build
                 (lambda _
                   (let ((bash (which "bash")))
                     (setenv "CONFIG_SHELL" bash)
                     (setenv "SHELL" bash)))))))
    (native-inputs
      (list autoconf
            automake
            ocaml
            ocaml-findlib))
    (home-page "https://github.com/thierry-martinez/stdcompat")
    (synopsis "Compatibility module for OCaml standard library")
    (description
     "Compatibility module for OCaml standard library allowing programs to use
some recent additions to the standard library while preserving the ability to
be compiled on former versions of OCaml.")
    (license license:bsd-2)))

(define-public ocaml-stdlib-shims
  (package
    (name "ocaml-stdlib-shims")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/stdlib-shims")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gmg8w67j3ww17llk7hl4dx0vq7p50rn5s4ib9sy984k543rz59h"))))
    (build-system dune-build-system)
    (home-page "https://github.com/ocaml/stdlib-shims")
    (synopsis "OCaml stdlib features backport to older OCaml compilers")
    (description "This package backports some of the new stdlib features to
older compilers, such as the Stdlib module.  This allows projects that require
compatibility with older compiler to use these new features in their code.")
    ;; with ocaml-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-fileutils
  (package
    (name "ocaml-fileutils")
    (version "0.6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/gildor478/ocaml-fileutils")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s2az5lv52r0fig4624652zn3jj5dy30d1xlw6gzd0s086i4bvjh"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-stdlib-shims))
    (native-inputs
     (list ocaml-ounit))
    (home-page "http://ocaml-fileutils.forge.ocamlcore.org")
    (synopsis "Pure OCaml functions to manipulate real file and filename")
    (description "Library to provide pure OCaml functions to manipulate real
file (POSIX like) and filename.")
    (license license:lgpl2.1+))) ; with the OCaml static compilation exception

(define-public ocaml-oasis
  (package
    (name "ocaml-oasis")
    (version "0.4.11")
    (source (origin
              (method url-fetch)
              (uri (ocaml-forge-uri name version 1757))
              (sha256
               (base32
                "0bn13mzfa98dq3y0jwzzndl55mnywaxv693z6f1rlvpdykp3vdqq"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (substitute* "test/test-main/Test.ml"
                  ;; most of these tests fail because ld cannot find crti.o, but according
                  ;; to the log file, the environment variables {LD_,}LIBRARY_PATH
                  ;; are set correctly when LD_LIBRARY_PATH is defined beforehand.
                  (("TestBaseCompat.tests;") "")
                  (("TestExamples.tests;") "")
                  (("TestFull.tests;") "")
                  (("TestPluginDevFiles.tests;") "")
                  (("TestPluginInternal.tests;") "")
                  (("TestPluginOCamlbuild.tests;") "")
                  (("TestPluginOMake.tests;") ""))
                #t))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     (list ocamlbuild ocamlify ocamlmod))
    (home-page "https://oasis.forge.ocamlcore.org")
    (synopsis "Integrates a configure, build, install system in OCaml projects")
    (description "OASIS is a tool to integrate a configure, build and install
system in your OCaml projects.  It helps to create standard entry points in your
build system and allows external tools to analyse your project easily.")
    (license license:lgpl2.1+))) ; with ocaml static compilation exception

(define-public ocaml-cppo
  (package
    (name "ocaml-cppo")
    (version "1.6.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mjambon/cppo")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32
                 "1c8jlr2s0allw1h6czz5q24vn5jsnrrh44j7hjyilzaifm17dlrm"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     (list ocamlbuild))
    (home-page "https://github.com/mjambon/cppo")
    (synopsis "Equivalent of the C preprocessor for OCaml programs")
    (description "Cppo is an equivalent of the C preprocessor for OCaml
programs.  It allows the definition of simple macros and file inclusion.  Cppo is:
@enumerate
@item more OCaml-friendly than @command{cpp}
@item easy to learn without consulting a manual
@item reasonably fast
@item simple to install and to maintain.
@end enumerate")
    (license license:bsd-3)))

(define-public ocaml-seq
  (package
    (name "ocaml-seq")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/c-cube/seq")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cjpsc7q76yfgq9iyvswxgic4kfq2vcqdlmxjdjgd4lx87zvcwrv"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((install-dir (string-append (assoc-ref outputs "out")
                                               "/lib/ocaml/site-lib/seq")))
               (mkdir-p install-dir)
               (with-output-to-file (string-append install-dir "/META")
                 (lambda _
                   (display "name=\"seq\"
version=\"[distributed with ocaml]\"
description=\"dummy package for compatibility\"
requires=\"\"")))
               #t))))))
    (home-page "https://github.com/c-cube/seq")
    (synopsis "OCaml's standard iterator type")
    (description "This package is a compatibility package for OCaml's
standard iterator type starting from 4.07.")
    (license license:lgpl2.1+)))

(define-public ocaml-re
  (package
    (name "ocaml-re")
    (version "1.10.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/ocaml-re")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g0vmpx6ylv8m0w77zarn215pgb4czc6gcpb2fi5da1s307zwr0w"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-seq))
    (native-inputs
     `(("ounit" ,ocaml-ounit)))
    (home-page "https://github.com/ocaml/ocaml-re/")
    (synopsis "Regular expression library for OCaml")
    (description "Pure OCaml regular expressions with:
@enumerate
@item Perl-style regular expressions (module Re_perl)
@item Posix extended regular expressions (module Re_posix)
@item Emacs-style regular expressions (module Re_emacs)
@item Shell-style file globbing (module Re_glob)
@item Compatibility layer for OCaml's built-in Str module (module Re_str)
@end enumerate")
    (license license:expat)))

(define-public ocaml-ocplib-endian
  (package
    (name "ocaml-ocplib-endian")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/OCamlPro/ocplib-endian/")
                     (commit version)))
              (sha256
               (base32
                "1klj4g451s7m5r8bxmwc1rpvngpqdm40csnx9smgc06pwy2fax2c"))
              (file-name (git-file-name name version))))
    (build-system dune-build-system)
    (native-inputs
     `(("cppo" ,ocaml-cppo)))
    (home-page "https://github.com/OCamlPro/ocplib-endian")
    (synopsis "Optimised functions to read and write int16/32/64 from strings
and bigarrays")
    (description "Optimised functions to read and write int16/32/64 from strings
and bigarrays, based on new primitives added in version 4.01.  It works on
strings, bytes and bigstring (Bigarrys of chars), and provides submodules for
big- and little-endian, with their unsafe counter-parts.")
    (license license:lgpl2.1)))

(define-public ocaml-cstruct
  (package
    (name "ocaml-cstruct")
    (version "6.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/ocaml-cstruct")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dpbirs6lzp0bclr3wcw407jjspll7iy66z18zks3mjccvlxd21w"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "cstruct"))
    (propagated-inputs
     (list ocaml-bigarray-compat))
    (native-inputs
     (list ocaml-alcotest))
    (home-page "https://github.com/mirage/ocaml-cstruct")
    (synopsis "Access C structures via a camlp4 extension")
    (description "Cstruct is a library and syntax extension to make it easier
to access C-like structures directly from OCaml.  It supports both reading and
writing to these structures, and they are accessed via the Bigarray module.")
    (license license:isc)))

;; TODO again, the "parent" package already has an explicit package argument,
;; so a variant package doesn't make sense, at least these aliases help the
;; importer out so it doesn't re-import things.  At least hopefully.
(define ocaml-cstruct-unix ocaml-cstruct)
(define ocaml-cstruct-sexp ocaml-cstruct)

(define-public ocaml-ppx-cstruct
  (package
    (inherit ocaml-cstruct)
    (name "ocaml-ppx-cstruct")
    (properties `((upstream-name . "ppx_cstruct")))
    (arguments
     '(#:package "ppx_cstruct"
       ;; TODO doesn't find test deps for some reason?
       ;; I have no clue why.
       #:tests? #f))
    (propagated-inputs (modify-inputs (package-propagated-inputs ocaml-cstruct)
                         (append ocaml-cstruct ocaml-ppxlib ocaml-sexplib)))
    (native-inputs (modify-inputs (package-propagated-inputs ocaml-cstruct)
                     (append ocaml-cstruct-sexp ocaml-findlib
                             ocaml-ppx-sexp-conv)))))

(define-public ocaml-hex
  (package
    (name "ocaml-hex")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mirage/ocaml-hex")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xnl5wxd2qrba7phm3mdrjwd2kk26kb17dv94ciwp49ljcj28qc1"))))
    (build-system dune-build-system)
    (propagated-inputs
     `(("ocaml-bigarray-compat" ,ocaml-bigarray-compat)
       ("cstruct" ,ocaml-cstruct)))
    (home-page "https://github.com/mirage/ocaml-hex/")
    (synopsis "Minimal library providing hexadecimal converters")
    (description "Hex is a minimal library providing hexadecimal converters.")
    (license license:isc)))

(define-public ocaml-ezjsonm
  (package
    (name "ocaml-ezjsonm")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/ezjsonm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "004knljxqxn9zq0rnq7q7wxl4nwlzydm8p9f5cqkl8il5yl5zkjm"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "ezjsonm"))
    (native-inputs (list ocaml-alcotest js-of-ocaml node-lts))
    (propagated-inputs (list ocaml-jsonm ocaml-uutf ocaml-sexplib0 ocaml-hex))
    (home-page "https://github.com/mirage/ezjsonm/")
    (synopsis "Read and write JSON data")
    (description "Ezjsonm provides more convenient (but far less flexible) input
and output functions that go to and from [string] values than jsonm.  This avoids
the need to write signal code, which is useful for quick scripts that manipulate
JSON.")
    (license license:isc)))

(define-public ocaml-uri
  (package
    (name "ocaml-uri")
    (version "4.2.0")
    (home-page "https://github.com/mirage/ocaml-uri")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1bgkc66cq00mgnkz3i535srwzwc4cpdsv0mly5dzvvq33451xwf0"))))
    (build-system dune-build-system)
    (arguments '(#:package "uri"))
    (propagated-inputs
     (list ocaml-stringext ocaml-angstrom))
    (native-inputs
     (list ocaml-ounit ocaml-ppx-sexp-conv))
    (properties `((upstream-name . "uri")))
    (synopsis "RFC3986 URI/URL parsing library")
    (description "OCaml-uri is a library for parsing URI/URL in the RFC3986 format.")
    (license license:isc)))

(define-public ocaml-easy-format
  (package
    (name "ocaml-easy-format")
    (version "1.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mjambon/easy-format")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xap6az4yyb60vb1jfs640wl3cf4njv78p538x9ihhf9f6ij3nh8"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "easy-format"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'make-writable
           (lambda _
             (for-each
               (lambda (file)
                 (chmod file #o644))
               (find-files "." "."))
             #t)))))
    (home-page "https://github.com/mjambon/easy-format")
    (synopsis "Interface to the Format module")
    (description "Easy-format is a high-level and functional interface to the
Format module of the OCaml standard library.")
    (license license:bsd-3)))

(define-public ocaml-piqilib
  (package
    (name "ocaml-piqilib")
    (version "0.6.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alavrik/piqi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mbhfrfrik3jlzx9zz680g0qdvv0b7cbjz28cgdlryp7nk4v4kx8"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-ocamlpath
           (lambda _
             (substitute* '("Makefile" "make/Makefile.ocaml")
               (("OCAMLPATH := ") "OCAMLPATH := $(OCAMLPATH):"))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "make/OCamlMakefile"
                 (("/bin/sh") (which "bash")))
               (invoke "./configure" "--prefix" out "--ocaml-libdir"
                       (string-append out "/lib/ocaml/site-lib")))))
       (add-after 'build 'build-ocaml
         (lambda* (#:key outputs #:allow-other-keys)
           (invoke "make" "ocaml")))
       (add-after 'install 'install-ocaml
         (lambda* (#:key outputs #:allow-other-keys)
           (invoke "make" "ocaml-install")))
       (add-after 'install-ocaml 'link-stubs
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                  (lib (string-append out "/lib/ocaml/site-lib/piqilib")))
             (mkdir-p stubs)
             (symlink (string-append lib "/dllpiqilib_stubs.so")
                      (string-append stubs "/dllpiqilib_stubs.so"))))))))
    (native-inputs
     (list which))
    (propagated-inputs
     `(("ocaml-xmlm" ,ocaml-xmlm)
       ("ocaml-sedlex" ,ocaml-sedlex-2)
       ("ocaml-easy-format" ,ocaml-easy-format)
       ("ocaml-base64" ,ocaml-base64)))
    (home-page "https://piqi.org")
    (synopsis "Data serialization and conversion library")
    (description "Piqilib is the common library used by the piqi command-line
tool and piqi-ocaml.")
    (license license:asl2.0)))

(define-public ocaml-uuidm
  (package
    (name "ocaml-uuidm")
    (version "0.9.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/uuidm/"
                                  "releases/uuidm-" version ".tbz"))
              (sha256
               (base32
                "1cr6xlzla9fmd587lfhzac0icifspjnqi9f4cdafshj3jn85nrpw"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags
       (list "build" "--tests" "true" "--with-cmdliner" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild opam-installer))
    (propagated-inputs
     `(("cmdliner" ,ocaml-cmdliner)
       ("topkg" ,ocaml-topkg)))
    (home-page "https://erratique.ch/software/uuidm")
    (synopsis "Universally unique identifiers for OCaml")
    (description "Uuidm is an OCaml module implementing 128 bits universally
unique identifiers (UUIDs) version 3, 5 (named based with MD5, SHA-1 hashing)
and 4 (random based) according to RFC 4122.")
    (license license:isc)))

(define-public ocaml-graph
  (package
    (name "ocaml-graph")
    (version "2.0.0")
    (home-page "https://github.com/backtracking/ocamlgraph/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gjrsyyamvvn2rd9n9yjx6hsglhw0dbm4cgazq0dpx0bbr4inwc3"))))
    (build-system dune-build-system)
    (arguments `(#:package "ocamlgraph"))
    (propagated-inputs (list ocaml-stdlib-shims))
    (native-inputs (list ocaml-graphics))
    (properties `((upstream-name . "ocamlgraph")))
    (synopsis "Graph library for OCaml")
    (description "OCamlgraph is a generic graph library for OCaml.")
    (license license:lgpl2.1)))

(define-public ocaml-piqi
  (package
    (name "ocaml-piqi")
    (version "0.7.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alavrik/piqi-ocaml")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12m9vxir0cs2155nxs0a3m3npf3w79kyxf9a5lmf18qvvgismfz8"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:make-flags
       ,#~(list (string-append "DESTDIR=" #$output)
                (string-append "SHELL="
                               #+(file-append (canonical-package bash-minimal)
                                              "/bin/sh")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (delete 'configure))))
    (native-inputs
     (list which protobuf)) ; for tests
    (propagated-inputs
     `(("ocaml-num" ,ocaml-num)
       ("ocaml-piqilib" ,ocaml-piqilib)
       ("ocaml-stdlib-shims" ,ocaml-stdlib-shims)))
    (home-page "https://github.com/alavrik/piqi-ocaml")
    (synopsis "Protocol serialization system for OCaml")
    (description "Piqi is a multi-format data serialization system for OCaml.
It provides a uniform interface for serializing OCaml data structures to JSON,
XML and Protocol Buffers formats.")
    (license license:asl2.0)))

(define-public ocaml-ppx-bap
  (package
    (name "ocaml-ppx-bap")
    (version "0.14.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/BinaryAnalysisPlatform/ppx_bap")
                     (commit (string-append "v" (version-major+minor version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c6rcdp8bicdiwqc2mb59cl9l2vxlp3y8hmnr9x924fq7acly248"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs (list ocaml-base-quickcheck
                             ocaml-ppx-assert
                             ocaml-ppx-bench
                             ocaml-ppx-bin-prot
                             ocaml-ppx-cold
                             ocaml-ppx-compare
                             ocaml-ppx-enumerate
                             ocaml-ppx-fields-conv
                             ocaml-ppx-hash
                             ocaml-ppx-here
                             ocaml-ppx-optcomp
                             ocaml-ppx-sexp-conv
                             ocaml-ppx-sexp-value
                             ocaml-ppx-variants-conv
                             ocaml-ppxlib))
    (properties `((upstream-name . "ppx_bap")))
    (home-page "https://github.com/BinaryAnalysisPlatform/ppx_bap")
    (synopsis "The set of ppx rewriters for BAP")
    (description
     "@code{ppx_bap} is the set of blessed ppx rewriters used in BAP projects.
It fills the same role as @code{ppx_base} or @code{ppx_jane} (from which it is
derived), but doesn't impose any style requirements and has only the minimal
necessary set of rewriters.")
    (license license:expat)))

(define-public bap
  (let (;; Let pin one commit because -alpha is subject to change.
        ;; The last stable release v2.5.0 is from July 2022.
        (revision "0")
        (commit "f995d28a4a34abb4cef8e0b3bd3c41cd710ccf1a"))
    (package
      (name "bap")
      (version (git-version "2.6.0-alpha" revision commit))
      (home-page "https://github.com/BinaryAnalysisPlatform/bap")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zfkc8nagf8kvxbypalbhf6gs0c7i48zx53hlpak2ipjwhvm5im5"))))
      (build-system ocaml-build-system)
      (arguments
       (list
        #:use-make? #t
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'configure 'fix-ncurses
              (lambda _
                (substitute* "oasis/llvm"
                  (("-lcurses") "-lncurses"))
                #t))
            (replace 'configure
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (for-each make-file-writable (find-files "." "."))
                ;; Package name changed
                (substitute* "oasis/elf-loader"
                  (("bitstring.ppx") "ppx_bitstring"))
                ;; We don't have a monolithic llvm
                (substitute* "oasis/llvm.setup.ml.in"
                  (("llvm_static = \"true\"") "true"))
                ;; Package update removed Make_binable, which was an alias
                ;; for Make_binable_without_uuid
                (substitute* (find-files "." ".")
                  (("Utils.Make_binable1\\(") "Utils.Make_binable1_without_uuid(")
                  (("Utils.Make_binable\\(") "Utils.Make_binable_without_uuid("))
                (invoke "./configure" "--prefix"
                        (assoc-ref outputs "out")
                        "--libdir"
                        (string-append
                         (assoc-ref outputs "out")
                         "/lib/ocaml/site-lib")
                        (string-append "--with-llvm-version=" #$(package-version llvm))
                        "--with-llvm-config=llvm-config"
                        "--disable-ghidra"
                        "--disable-llvm-static"
                        "--enable-llvm"
                        "--enable-everything"))))))
      (native-inputs (list clang ocaml-oasis ocaml-ounit))
      (propagated-inputs
       (list
        camlzip
        ocaml-bitstring
        ocaml-cmdliner
        ocaml-core-kernel
        ocaml-ezjsonm
        ocaml-fileutils
        ocaml-frontc
        ocaml-graph
        ocaml-linenoise
        ocaml-ocurl
        ocaml-piqi
        ocaml-ppx-bap
        ocaml-ppx-bitstring
        ocaml-re
        ocaml-uri
        ocaml-utop
        ocaml-uuidm
        ocaml-yojson
        ocaml-z3
        ocaml-zarith))
      (inputs
       (list gmp llvm ncurses))
      (synopsis "Binary Analysis Platform")
      (description "Binary Analysis Platform is a framework for writing program
analysis tools, that target binary files.  The framework consists of a plethora
of libraries, plugins, and frontends.  The libraries provide code reusability,
the plugins facilitate extensibility, and the frontends serve as entry points.")
      (license license:expat))))

(define-public ocaml-camomile
  (package
    (name "ocaml-camomile")
    (version "1.0.2")
    (home-page "https://github.com/yoriyuki/Camomile")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/releases/download/" version
                                  "/camomile-" version ".tbz"))
              (sha256
               (base32
                "0chn7ldqb3wyf95yhmsxxq65cif56smgz1mhhc7m0dpwmyq1k97h"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f ; Tests fail, see https://github.com/yoriyuki/Camomile/issues/82
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-usr-share
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("Camomile/dune" "configure.ml")
               (("/usr/share") (string-append (assoc-ref outputs "out") "/share")))
             #t)))))
    (synopsis "Comprehensive Unicode library")
    (description "Camomile is a Unicode library for OCaml.  Camomile provides
Unicode character type, UTF-8, UTF-16, UTF-32 strings, conversion to/from about
200 encodings, collation and locale-sensitive case mappings, and more.  The
library is currently designed for Unicode Standard 3.2.")
    ;; with an exception for linked libraries to use a different license
    (license license:lgpl2.0+)))

(define-public ocaml-charinfo-width
  ;; Add LICENSE file and Dune tests
  (let ((commit "20aaaa6dca8f1e0b1ace55b6f2a8ba5e5910b620"))
    (package
      (name "ocaml-charinfo-width")
      (version (git-version "1.1.0" "1" commit))
      (home-page "https://github.com/kandu/charinfo_width/")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04gil5hxm2jax9paw3i24d8zyzhyl5cphzfyryvy2lcrm3c485q0"))))
      (build-system dune-build-system)
      (propagated-inputs
       (list ocaml-result ocaml-camomile))
      (native-inputs
       (list ocaml-ppx-expect))
      (properties
       `((upstream-name . "charInfo_width")))
      (synopsis "Determine column width for a character")
      (description "This module implements purely in OCaml a character width
function that follows the prototype of POSIX's wcwidth.")
      (license license:expat))))

(define-public ocaml-zed
  (package
    (name "ocaml-zed")
    (version "3.2.1")
    (home-page "https://github.com/ocaml-community/zed")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17zdbm422y0qznc659civ9bmahhrbffxa50f8dnykiaq8v2ci91l"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-react
           ocaml-result
           ocaml-uchar
           ocaml-uutf
           ocaml-uucp
           ocaml-uuseg
           ocaml-odoc))
    (synopsis "Abstract engine for text edition in OCaml")
    (description
     "This module provides an abstract engine for text edition.  It can be
used to write text editors, edition widgets, readlines, and more.  The module
Zed uses Camomile to fully support the Unicode specification, and implements
an UTF-8 encoded string type with validation, and a rope datastructure to
achieve efficient operations on large Unicode buffers.  Zed also features a
regular expression search on ropes.  To support efficient text edition
capabilities, Zed provides macro recording and cursor management facilities.")
    (license license:bsd-3)))

(define-public ocaml-lambda-term
  (package
    (name "ocaml-lambda-term")
    (version "3.3.1")
    (home-page "https://github.com/ocaml-community/lambda-term")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pkamblc6h0rsbk901cqn3xr9gqa3g8wrwyx5zryaqvb2xpbhp8b"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-logs
           ocaml-lwt
           ocaml-lwt-react
           ocaml-mew-vi
           ocaml-odoc
           ocaml-react
           ocaml-zed))
    (synopsis "Terminal manipulation library for OCaml")
    (description "Lambda-Term is a cross-platform library for manipulating the
terminal.  It provides an abstraction for keys, mouse events, colors, as well as
a set of widgets to write curses-like applications.  The main objective of
Lambda-Term is to provide a higher level functional interface to terminal
manipulation than, for example, ncurses, by providing a native OCaml interface
instead of bindings to a C library.")
    (license license:bsd-3)))

(define-public ocaml-utop
  (package
    (name "ocaml-utop")
    (version "2.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-community/utop")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pcix3h9f7is06581iax4i08zkd6sv8y5hy1vvxhqhcsd9z0qfl3"))))
    (build-system dune-build-system)
    (native-inputs
     (list ocaml-cppo))
    (propagated-inputs
     (list ocaml-lambda-term
           ocaml-logs
           ocaml-lwt
           ocaml-lwt-react
           ocaml-react
           ocaml-zed))
    (home-page "https://github.com/ocaml-community/utop")
    (synopsis "Improved interface to the OCaml toplevel")
    (description "UTop is an improved toplevel for OCaml.  It can run in a
terminal or in Emacs.  It supports line editing, history, real-time and context
sensitive completion, colors, and more.")
    (license license:bsd-3)))

(define-public ocaml-ansiterminal
  (package
    (name "ocaml-ansiterminal")
    (version "0.8.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Chris00/ANSITerminal")
                    (commit version)
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "052qnc23vmxp90yympjz9q6lhqw98gs1yvb3r15kcbi1j678l51h"))))
    (build-system dune-build-system)
    (properties `((upstream-name . "ANSITerminal")))
    (home-page "https://github.com/Chris00/ANSITerminal")
    (synopsis
     "Basic control of ANSI compliant terminals and the windows shell")
    (description
     "ANSITerminal is a module allowing to use the colors and cursor
movements on ANSI terminals.")
    ;; Variant of the LGPL3+ which permits
    ;; static and dynamic linking when producing binary files.
    ;; In other words, it allows one to link to the library
    ;; when compiling nonfree software.
    (license (license:non-copyleft "LICENSE.md"))))

(define-public ocaml-ptmap
  (package
    (name "ocaml-ptmap")
    (version "2.0.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/backtracking/ptmap/releases/download/"
                              version "/ptmap-" version ".tbz"))
              (sha256
               (base32
                "1apk61fc1y1g7x3m3c91fnskvxp6i0vk5nxwvipj56k7x2pzilgb"))))
    (build-system dune-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "dune" "runtest")))))))
    (propagated-inputs (list ocaml-stdlib-shims ocaml-seq))
    (home-page "https://github.com/backtracking/ptmap")
    (synopsis "Maps of integers implemented as Patricia trees")
    (description
     "An implementation inspired by Okasaki & Gill's paper 'Fast Mergeable
Integer Maps.'")
    (license license:lgpl2.1))) ; with linking exception

(define-public ocaml-integers
  (package
    (name "ocaml-integers")
    (version "0.7.0")
    (home-page "https://github.com/ocamllabs/ocaml-integers")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c0bmy53ag6504kih0cvnp4yf7mbcimb18m1mgs592ffb0zj1rff"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (propagated-inputs
     (list ocaml-stdlib-shims))
    (synopsis "Various signed and unsigned integer types for OCaml")
    (description "The ocaml-integers library provides a number of 8-, 16-, 32-
and 64-bit signed and unsigned integer types, together with aliases such as
long and size_t whose sizes depend on the host platform.")
    (license license:expat)))

(define-public ocaml-ctypes
  (package
   (name "ocaml-ctypes")
   (version "0.20.1")
   (home-page "https://github.com/ocamllabs/ocaml-ctypes")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url home-page)
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0ilzq9qzvwv9rc08cc9wchsx636zp870i7qvqmbigaa2qb812m0z"))))
   (build-system ocaml-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'make-writable
          (lambda _
            (for-each make-file-writable
                      (find-files "."))))
        (delete 'configure))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("ounit" ,ocaml-ounit)
      ("lwt" ,ocaml-lwt)))
   (propagated-inputs
    `(("bigarray-compat" ,ocaml-bigarray-compat)
      ("integers" ,ocaml-integers)))
   (inputs
    (list libffi))
   (properties `((ocaml5.0-variant . ,(delay ocaml5.0-ctypes))))
   (synopsis "Library for binding to C libraries using pure OCaml")
   (description "Ctypes is a library for binding to C libraries using pure
OCaml.  The primary aim is to make writing C extensions as straightforward as
possible.  The core of ctypes is a set of combinators for describing the
structure of C types -- numeric types, arrays, pointers, structs, unions and
functions.  You can use these combinators to describe the types of the
functions that you want to call, then bind directly to those functions -- all
without writing or generating any C!")
   (license license:expat)))

(define-public ocaml5.0-ctypes
  ;; Contains fix to support OCaml 5.0
  ;; (https://github.com/ocamllabs/ocaml-ctypes/pull/709)
  (let ((commit "52ff621f47dbc1ee5a90c30af0ae0474549946b4")
        (revision "0"))
    (package-with-ocaml5.0
     (package
       (inherit ocaml-ctypes)
       (name "ocaml-ctypes")
       (version (git-version "0.20.1" revision commit))
       (home-page "https://github.com/ocamllabs/ocaml-ctypes")
       (arguments
        (append
         ;; Some tests fail. Failure seems to be due to OCaml 5.0.
         `(#:tests? #f)
         (package-arguments ocaml-ctypes)))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url home-page)
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "0vyiryqchz0mdfhal7bdivqsk5yhvzzckrqv495rlpbdxzklcp7g"))))
       (properties '())))))

(define-public ocaml-ocb-stubblr
  (package
   (name "ocaml-ocb-stubblr")
   (version "0.1.1")
   (home-page "https://github.com/pqwy/ocb-stubblr")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   home-page "/releases/download/v0.1.1/ocb-stubblr-"
                   version ".tbz"))
             (file-name (string-append name "-" version ".tbz"))
             (sha256
              (base32
               "167b7x1j21mkviq8dbaa0nmk4rps2ilvzwx02igsc2706784z72f"))))
   (build-system ocaml-build-system)
   (arguments
    `(#:build-flags (list "build" "--tests" "true")
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'fix-for-guix
          (lambda _
            (substitute* "src/ocb_stubblr.ml"
              ;; Do not fail when opam is not present or initialized
              (("error_msgf \"error running opam\"") "\"\"")
              ;; Guix doesn't have cc, but it has gcc
              (("\"cc\"") "\"gcc\""))
            #t)))))
   (inputs (list ocaml-topkg opam-installer))
   (native-inputs (list ocaml-astring ocamlbuild))
   (synopsis "OCamlbuild plugin for C stubs")
   (description "Ocb-stubblr is about ten lines of code that you need to
repeat over, over, over and over again if you are using ocamlbuild to build
OCaml projects that contain C stubs.")
   (license license:isc)))

(define-public ocaml-tsdl
  (package
    (name "ocaml-tsdl")
    (version "0.9.9")
    (home-page "https://erratique.ch/software/tsdl")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/releases/tsdl-"
                                  version ".tbz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m565jgfanijjzp64c1rylahkpmrrb03ywj202j49n06nvwp788s"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags '("build")
       #:tests? #f; tests require a display device
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list ocamlbuild ocaml-astring opam-installer pkg-config))
    (inputs
     `(("topkg" ,ocaml-topkg)
       ("sdl2" ,sdl2)
       ("integers" ,ocaml-integers)
       ("ctypes" ,ocaml-ctypes)))
    (synopsis "Thin bindings to SDL for OCaml")
    (description "Tsdl is an OCaml library providing thin bindings to the
cross-platform SDL C library.")
    (license license:isc)))

(define-public dedukti
  (package
    (name "dedukti")
    (version "2.7")
    (home-page "https://deducteam.github.io/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deducteam/dedukti")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dsr3s88kgmcg3najhc29cwfvsxa2plvjws1127fz75kmn15np28"))))
    (build-system dune-build-system)
    (inputs (list gmp ocaml-cmdliner ocaml-z3 z3))
    (native-inputs (list ocaml-menhir))
    (synopsis "Proof-checker for the λΠ-calculus modulo theory, an extension of
the λ-calculus")
    (description "Dedukti is a proof-checker for the λΠ-calculus modulo
theory.  The λΠ-calculus is an extension of the simply typed λ-calculus with
dependent types.  The λΠ-calculus modulo theory is itself an extension of the
λΠ-calculus where the context contains variable declaration as well as rewrite
rules.  This system is not designed to develop proofs, but to check proofs
developed in other systems.  In particular, it enjoys a minimalistic syntax.")
    (license license:cecill-c)))

(define-public ocaml-jst-config
  (package
    (name "ocaml-jst-config")
    (version "0.16.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/jst-config")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "01419gk2w2r1xqz11rw1glngi8fi6gq9sh79c06vasrk6vxriy0s"))))
    (build-system dune-build-system)
    (arguments '(#:tests? #f))           ; no tests
    (propagated-inputs
      (list ocaml-base ocaml-ppx-assert ocaml-stdio dune-configurator))
    (home-page "https://github.com/janestreet/jst-config")
    (synopsis "Compile-time configuration for Jane Street libraries")
    (description "Defines compile-time constants used in Jane Street libraries
such as Base, Core, and Async.  This package has an unstable interface; it is
intended only to share configuration between different packages from Jane
Street.  Future updates may not be backward-compatible, and we do not
recommend using this package directly.")
    (license license:expat)))

(define-public ocaml-jane-street-headers
  (package
    (name "ocaml-jane-street-headers")
    (version "0.15.0")
    (source
     (janestreet-origin
      "jane-street-headers" version
      "1r27r0bxxa0iaah5rm84lwhrmh784vfpmb6056hpv0p34rxs7r1l"))
    (build-system dune-build-system)
    (arguments '(#:tests? #f))           ; no tests
    (home-page "https://github.com/janestreet/jane-street-headers")
    (synopsis "Jane Street C header files")
    (description "C header files shared between the various Jane Street
packages.")
    (license license:expat)))

(define-public ocaml-time-now
  (package
    (name "ocaml-time-now")
    (version "0.15.0")
    (source
     (janestreet-origin
      "time_now" version
      "1a6b1f55mwci1bd8w8vji0qn6wbs60jbwixvwgy4klx2blq57cqk"))
    (build-system dune-build-system)
    (arguments '(#:tests? #f))           ; no tests
    (propagated-inputs
     (list ocaml-base ocaml-jane-street-headers ocaml-jst-config
           ocaml-ppx-base ocaml-ppx-optcomp))
    (properties `((upstream-name . "time_now")))
    (home-page
     "https://github.com/janestreet/time_now")
    (synopsis "Reports the current time")
    (description
     "Provides a single function to report the current time in nanoseconds
since the start of the Unix epoch.")
    (license license:expat)))

(define-public ocaml-ppx-inline-test
  (package
    (name "ocaml-ppx-inline-test")
    (version "0.15.0")
    (home-page "https://github.com/janestreet/ppx_inline_test")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1a0gaj9p6gbn5j7c258mnzr7yjlq0hqi3aqqgyj1g2dbk1sxdbjz"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ;see home page README for further information
    (propagated-inputs
     (list ocaml-base
           ocaml-migrate-parsetree
           ocaml-compiler-libs
           ocaml-sexplib0
           ocaml-stdio
           ocaml-ppxlib
           ocaml-time-now))
    (properties `((upstream-name . "ppx_inline_test")))
    (synopsis "Syntax extension for writing in-line tests in ocaml code")
    (description "This package contains a syntax extension for writing
in-line tests in ocaml code.  It is part of Jane Street's PPX rewriters
collection.")
    (license license:expat)))

(define-public ocaml-bindlib
  (package
    (name "ocaml-bindlib")
    (version "6.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rlepigre/ocaml-bindlib")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1viyws3igy49hfaj4jaiwm4iggck9zdn7r3g6kh1n4zxphqk57yk"))))
    (build-system dune-build-system)
    (native-inputs
     (list ocamlbuild ocaml-findlib))
    (home-page "https://rlepigre.github.io/ocaml-bindlib/")
    (synopsis "OCaml Bindlib library for bound variables")
    (description "Bindlib is a library allowing the manipulation of data
structures with bound variables.  It is particularly useful when writing ASTs
for programming languages, but also for manipulating terms of the λ-calculus
or quantified formulas.")
    (license license:gpl3+)))

(define-public ocaml-earley
  (package
    (name "ocaml-earley")
    (version "3.0.0")
    (home-page "https://github.com/rlepigre/ocaml-earley")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1vi58zdxchpw6ai0bz9h2ggcmg8kv57yk6qbx82lh47s5wb3mz5y"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-stdlib-shims))
    (synopsis "Parsing library based on Earley Algorithm")
    (description "Earley is a parser combinator library base on Earley's
algorithm.  It is intended to be used in conjunction with an OCaml syntax
extension which allows the definition of parsers inside the language.  There
is also support for writing OCaml syntax extensions in a camlp4 style.")
    (license license:cecill-b)))

(define-public ocaml-timed
  (package
    (name "ocaml-timed")
    (version "1.1")
    (home-page "https://github.com/rlepigre/ocaml-timed")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append home-page ".git"))
                    (commit version)))
              (sha256
               (base32
                "1aqmkpjv5jk95lc2m3qyyrhw8ra7n9wj8pv3bfc83l737zv0hjn1"))
              (file-name (git-file-name name version))))
    (build-system dune-build-system)
    (synopsis "Timed references for imperative state")
    (description "Timed references for imperative state.  This module provides
an alternative type for references (or mutable cells) supporting undo/redo
operations.  In particular, an abstract notion of time is used to capture the
state of the references at any given point, so that it can be restored.  Note
that usual reference operations only have a constant time / memory overhead
(compared to those of the standard library).

Moreover, we provide an alternative implementation based on the references
of the standard library (Pervasives module).  However, it is less efficient
than the first one.")
    (license license:expat)))

(define-public ocaml-biniou
 (package
   (name "ocaml-biniou")
   (version "1.2.2")
   (home-page "https://github.com/mjambon/biniou")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gd4nqffm9h7dzxyvpfpww24l61fqgazyh3p5f7k9jvgyv9y4vcn"))))
   (build-system dune-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-before 'build 'make-writable
          (lambda _ (for-each make-file-writable (find-files "." ".")))))))
   (inputs
    (list ocaml-easy-format ocaml-camlp-streams))
   (native-inputs
    (list which))
   (synopsis "Data format designed for speed, safety, ease of use and backward
compatibility")
   (description "Biniou (pronounced \"be new\" is a binary data format
designed for speed, safety, ease of use and backward compatibility as
protocols evolve.  Biniou is vastly equivalent to JSON in terms of
functionality but allows implementations several times faster (4 times faster
than yojson), with 25-35% space savings.")
   (license license:bsd-3)))

(define-public ocaml-yojson
  (package
    (name "ocaml-yojson")
    (version "2.0.2")
    (home-page "https://github.com/ocaml-community/yojson")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1habsh00ihjhk1g1csxqg3hj8izk5zvgc7wm579wyjw35vzcmwr1"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "yojson"))
    (propagated-inputs (list ocaml-seq))
    (native-inputs (list ocaml-alcotest ocaml-cppo))
    (synopsis "Low-level JSON library for OCaml")
    (description "Yojson is an optimized parsing and printing library for the
JSON format.  It addresses a few shortcomings of json-wheel including 2x
speedup, polymorphic variants and optional syntax for tuples and variants.
@code{ydump} is a pretty printing command-line program provided with the
yojson package.  The program @code{atdgen} can be used to derive OCaml-JSON
serializers and deserializers from type definitions.")
    (license license:bsd-3)))

(define-public ocaml-ppx-yojson-conv-lib
  (package
    (name "ocaml-ppx-yojson-conv-lib")
    (version "0.16.0")
    (home-page "https://github.com/janestreet/ppx_yojson_conv_lib")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
          (url home-page)
          (commit (string-append "v" version))))
        (sha256
         (base32
          "1npc1dbrcl3izi2rpf3rqz98jvsxrgzqn2vb95nf8wxgmh6gmrsc"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-yojson))
    (properties `((upstream-name . "ppx_yojson_conv_lib")))
    (synopsis "Runtime library used by ocaml PPX yojson convertor")
    (description "Ppx_yojson_conv_lib is the runtime library used by
ppx_yojson_conv, a ppx rewriter that can be used to convert ocaml types
to a Yojson.Safe value.")
    (license license:expat)))

(define-public ocaml-merlin-lib
  (package
    (name "ocaml-merlin-lib")
    (version "4.14-414")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/merlin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d9q6yl5i08j9lkmbywh5q1yfjxin5n9yp9bqwi7a9lanhwg8psi"))))
    (build-system dune-build-system)
    (arguments '(#:package "merlin-lib"
                 #:tests? #f))          ; no tests
    (propagated-inputs (list ocaml-csexp ocaml-menhir))
    (properties `((ocaml5.0-variant . ,(delay ocaml5.0-merlin-lib))))
    (home-page "https://ocaml.github.io/merlin/")
    (synopsis "Merlin libraries")
    (description "These libraries provides access to low-level compiler
interfaces and the standard higher-level merlin protocol.")
    (license license:expat)))

;; the 500 indicates that this version is for OCaml 5.0
(define ocaml-merlin-lib-500
  (package
    (inherit ocaml-merlin-lib)
    (name "ocaml-merlin-lib")
    (version "4.14-500")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/merlin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rx0h8a7m435jmfvpxjf4682dxgb2f74ar1k1m3c3hls6yxgw0a9"))))
    (properties '())))

(define-public ocaml5.0-merlin-lib
  (package-with-ocaml5.0 ocaml-merlin-lib-500))

(define-public ocaml-dot-merlin-reader
  (package
    (inherit ocaml-merlin-lib)
    (name "ocaml-dot-merlin-reader")
    (arguments '(#:package "dot-merlin-reader"
                 #:tests? #f))          ; no tests
    (propagated-inputs (list ocaml-merlin-lib))
    (properties `((ocaml5.0-variant . ,(delay ocaml5.0-dot-merlin-reader))))
    (synopsis "Reads config files for @code{ocaml-merlin}")
    (description "@code{ocaml-dot-merlin-reader} is an external reader for
@code{ocaml-merlin} configurations.")))

(define-public ocaml5.0-dot-merlin-reader
  (package-with-ocaml5.0
   (package
     (inherit ocaml-merlin-lib-500)
     (name "ocaml-dot-merlin-reader")
     (arguments '(#:package "dot-merlin-reader"
                  #:tests? #f))         ; no tests
     (propagated-inputs (list ocaml5.0-merlin-lib))
     (synopsis "Reads config files for @code{ocaml-merlin}")
     (description "@code{ocaml-dot-merlin-reader} is an external reader for
@code{ocaml-merlin} configurations."))))

(define-public ocaml-merlin
  (package
    (inherit ocaml-dot-merlin-reader)
    (name "ocaml-merlin")
    (arguments
     '(#:package "merlin"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "dune" "runtest" "-p" "merlin,dot-merlin-reader")))))))
    (propagated-inputs (list ocaml-merlin-lib ocaml-yojson))
    (properties `((ocaml5.0-variant . ,(delay ocaml5.0-merlin))))
    (native-inputs
     (list ocaml-dot-merlin-reader ; required for tests
           ocaml-ppxlib
           ocaml-mdx
           jq))
    (synopsis "Context sensitive completion for OCaml in Vim and Emacs")
    (description "Merlin is an editor service that provides modern IDE
features for OCaml.  Emacs and Vim support is provided out-of-the-box.
External contributors added support for Visual Studio Code, Sublime Text and
Atom.")
    (license license:expat)))

(define-public ocaml5.0-merlin
  (package-with-ocaml5.0
   (package
     (inherit ocaml-merlin-lib-500)
     (name "ocaml-merlin")
     (arguments
      '(#:package "merlin"
        #:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "dune" "runtest" "-p" "merlin,dot-merlin-reader")))))))
     (propagated-inputs (list ocaml-merlin-lib ocaml-yojson))
     (native-inputs
      (list ocaml-dot-merlin-reader     ; required for tests
            ocaml-ppxlib
            ocaml-mdx
            jq))
     (synopsis "Context sensitive completion for OCaml in Vim and Emacs")
     (description "Merlin is an editor service that provides modern IDE
features for OCaml.  Emacs and Vim support is provided out-of-the-box.
External contributors added support for Visual Studio Code, Sublime Text and
Atom.")
     (license license:expat))))

(define-public ocaml-lsp-server
  (package
    (name "ocaml-lsp-server")
    (version "1.17.0")
    (home-page "https://github.com/ocaml/ocaml-lsp")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                (url home-page)
                (commit version)))
              (sha256
               (base32
                "1w1m2mi7va3wcwgvgzqd3af6hrhx5zzyr3hqa228pcimp44w6f0h"))))
    (build-system dune-build-system)
    (arguments '(#:tests? #f)) ; tests are failing for v1.17
    (propagated-inputs (list
                             ocaml-re
                             ocaml-ppx-yojson-conv-lib
                             dune-rpc
                             ocaml-chrome-trace
                             dune-dyn
                             dune-stdune
                             ocaml-fiber
                             ocaml-xdg
                             dune-ordering
                             ocaml-dune-build-info
                             ocaml-spawn
                             ocamlc-loc
                             ocaml-uutf
                             ocaml-pp
                             ocaml-csexp
                             ocamlformat-rpc-lib
                             ocaml-odoc
                             ocaml-merlin-lib))
    (native-inputs (list ocaml-ppx-expect ocamlformat))
    (properties `((upstream-name . "ocaml-lsp-server")))
    (synopsis "LSP Server for OCaml")
    (description "This package implements an Ocaml language server implementation.")
    (license license:isc)))

(define-public ocaml5.0-lsp-server (package-with-ocaml5.0 ocaml-lsp-server))

(define-public ocaml-gsl
  (package
    (name "ocaml-gsl")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/mmottl/gsl-ocaml/releases/download/"
         version "/gsl-" version ".tbz"))
       (sha256
        (base32
         "1l5zkkkg8sglsihrbf10ivq9s8xzl1y6ag89i4jqpnmi4m43fy34"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-gsl-directory
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/config/discover.ml"
               (("/usr") (assoc-ref inputs "gsl"))))))))
    (inputs
     (list gsl))
    (propagated-inputs
     (list ocaml-base ocaml-stdio))
    (home-page "https://mmottl.github.io/gsl-ocaml")
    (synopsis "Bindings to the GNU Scientific Library")
    (description
     "GSL-OCaml is an interface to the @dfn{GNU scientific library} (GSL) for
the OCaml language.")
    (license license:gpl3+)))

(define-public cubicle
  (package
    (name "cubicle")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://cubicle.lri.fr/cubicle-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10kk80jdmpdvql88sdjsh7vqzlpaphd8vip2lp47aarxjkwjlz1q"))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake ocaml
           (@@ (gnu packages base) which)))
    (propagated-inputs
     (list ocaml-num z3))
    (arguments
     `(#:configure-flags (list "--with-z3")
       #:make-flags (list "QUIET=")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'make-deterministic
           (lambda _
             (substitute* "Makefile.in"
               (("`date`") "no date for reproducibility"))))
         (add-before 'configure 'configure-for-release
           (lambda _
             (substitute* "Makefile.in"
               (("SVNREV=") "#SVNREV="))
             #t))
         (add-before 'configure 'fix-/bin/sh
           (lambda _
             (substitute* "configure"
               (("-/bin/sh") (string-append "-" (which "sh"))))
             #t))
         (add-before 'configure 'fix-smt-z3wrapper.ml
           (lambda _
             (substitute* "Makefile.in"
               (("\\\\n") ""))
             #t))
         (add-before 'configure 'fix-ocaml-num
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("nums.cma") "num.cma num_core.cma")
               (("= \\$\\(FUNCTORYLIB\\)")
                (string-append "= -I "
                               (assoc-ref inputs "ocaml-num")
                               "/lib/ocaml/site-lib/num/core -I "
                               (assoc-ref inputs "ocaml-num")
                               "/lib/ocaml/site-lib/num"
                               " $(FUNCTORYLIB)")))
             #t)))))
    (home-page "https://cubicle.lri.fr/")
    (synopsis "Model checker for array-based systems")
    (description "Cubicle is a model checker for verifying safety properties
of array-based systems.  This is a syntactically restricted class of
parametrized transition systems with states represented as arrays indexed by
an arbitrary number of processes.  Cache coherence protocols and mutual
exclusion algorithms are typical examples of such systems.")
    (license license:asl2.0)))

(define-public ocaml-sexplib0
  (package
    (name "ocaml-sexplib0")
    (version "0.15.1")
    (home-page "https://github.com/janestreet/sexplib0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05m93g4m4jhj1v8pazg3s2ydcfymr3h4476yjhdca5fm4sn35bg8"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ;no tests
    (synopsis "Library containing the definition of S-expressions and some
base converters")
    (description "Part of Jane Street's Core library The Core suite of
libraries is an industrial strength alternative to OCaml's standard library
that was developed by Jane Street, the largest industrial user of OCaml.")
    (license license:expat)))

(define-public ocaml-parsexp
  (package
    (name "ocaml-parsexp")
    (version "0.15.0")
    (home-page "https://github.com/janestreet/parsexp")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1grzpxi39318vcqhwf723hqh11k68irh59zb3dyg9lw8wjn7752a"))))
    (build-system dune-build-system)
    (inputs
     (list ocaml-sexplib0 ocaml-base))
    (synopsis "S-expression parsing library")
    (description
     "This library provides generic parsers for parsing S-expressions from
strings or other medium.

The library is focused on performances but still provide full generic
parsers that can be used with strings, bigstrings, lexing buffers,
character streams or any other sources effortlessly.

It provides three different class of parsers:
@itemize
@item
the normal parsers, producing [Sexp.t] or [Sexp.t list] values
@item
the parsers with positions, building compact position sequences so
that one can recover original positions in order to report properly
located errors at little cost
@item
the Concrete Syntax Tree parsers, produce values of type
@code{Parsexp.Cst.t} which record the concrete layout of the s-expression
syntax, including comments
@end itemize

This library is portable and doesn't provide IO functions.  To read
s-expressions from files or other external sources, you should use
parsexp_io.")
    (license license:expat)))

(define-public ocaml-sexplib
  (package
    (name "ocaml-sexplib")
    (version "0.16.0")
    (home-page "https://github.com/janestreet/sexplib")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hl0zf2cgjivvlsrf85f5lg4xprcgbz7qg2z51k838y7k2121k78"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-num ocaml-parsexp ocaml-sexplib0))
    (synopsis
     "Library for serializing OCaml values to and from S-expressions")
    (description
     "This package is part of Jane Street's Core library.  Sexplib contains
functionality for parsing and pretty-printing s-expressions.")
    (license license:expat)))

(define-public ocaml-sexp-pretty
  (package
    (name "ocaml-sexp-pretty")
    (version "0.15.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/janestreet/sexp_pretty")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08z9jpvgag5f029ns0a06lcdymg00vwi232xsy1rdv82zvc0x4ah"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppx-base ocaml-sexplib ocaml-re))
    (properties `((upstream-name . "sexp_pretty")))
    (home-page "https://github.com/janestreet/sexp_pretty")
    (synopsis "S-expression pretty-printer")
    (description
     "Library for pretty-printing s-expressions, using better indentation
rules than the default pretty printer in Sexplib.")
    (license license:expat)))

(define-public ocaml-base
  (package
    (name "ocaml-base")
    (version "0.15.0")
    (home-page "https://github.com/janestreet/base")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/base")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1qyycqqr4dijvxm4hhy79c964wd91kpsfvb89kna1qwgllg0hrpj"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-sexplib0))
    (properties `((ocaml5.0-variant . ,(delay ocaml5.0-base))))
    (synopsis
     "Full standard library replacement for OCaml")
    (description
     "Base is a complete and portable alternative to the OCaml standard
library.  It provides all standard functionalities one would expect
from a language standard library.  It uses consistent conventions
across all of its module.

Base aims to be usable in any context.  As a result system dependent
features such as I/O are not offered by Base.  They are instead
provided by companion libraries such as
@url{https://github.com/janestreet/stdio, ocaml-stdio}.")
    (license license:expat)))

(define-public ocaml5.0-base
  ;; This version contains fixes for OCaml 5.0
  ;; (see https://github.com/ocaml/opam-repository/pull/21851)
  (let ((commit "423dbad212f55506767d758b1ceb2d6e0ee8e7f5")
        (revision "0"))
   (package-with-ocaml5.0
    (package
      (inherit ocaml-base)
      (name "ocaml-base")
      (version (git-version "0.15.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kit-ty-kate/base")
               (commit commit)))
         (file-name (git-file-name "ocaml5.0-base" version))
         (sha256
          (base32
           "15vsiv3q53l1bzrvqgspf3lp2104s9dzw62z3nl75f53jvjvsyf6"))))
      (properties '())))))

(define-public ocaml-compiler-libs
  (package
    (name "ocaml-compiler-libs")
    (version "0.12.4")
    (home-page "https://github.com/janestreet/ocaml-compiler-libs")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00if2f7j9d8igdkj4rck3p74y17j6b233l91mq02drzrxj199qjv"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ;no tests
    (properties `((upstream-name . "ocaml-compiler-libs")))
    (synopsis "Compiler libraries repackaged")
    (description "This package simply repackages the OCaml compiler libraries
so they don't expose everything at toplevel.  For instance, @code{Ast_helper}
is now @code{Ocaml_common.Ast_helper}.")
    (license license:expat)))

(define-public ocaml-stdio
  (package
    (name "ocaml-stdio")
    (version "0.15.0")
    (home-page "https://github.com/janestreet/stdio")
    (source
     (janestreet-origin "stdio" version
                        "0jsyg4jlp76d9gx1fngms6nfs7dcpsysdsvkywjq9a663n994wn3"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-sexplib0))
    (arguments `(#:tests? #f)) ;no tests
    (synopsis "Standard IO library for OCaml")
    (description
     "Stdio implements simple input/output functionalities for OCaml.  It
re-exports the input/output functions of the OCaml standard libraries using
a more consistent API.")
    (license license:expat)))

(define-public ocaml-ppx-deriving
  (package
    (name "ocaml-ppx-deriving")
    (version "5.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-ppx/ppx_deriving")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wqcnw4wi6pfjjhixpakckm03dpj990259za432804471a6spm2j"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-ppx-derivers ocaml-ppxlib ocaml-result))
    (native-inputs
     (list ocaml-cppo ocaml-ounit2))
    (properties `((upstream-name . "ppx_deriving")))
    (home-page "https://github.com/ocaml-ppx/ppx_deriving")
    (synopsis "Type-driven code generation for OCaml")
    (description
     "Ppx_deriving provides common infrastructure for generating code based
on type definitions, and a set of useful plugins for common tasks.")
    (license license:expat)))

(define-public ocaml-ppx-derivers
  (package
    (name "ocaml-ppx-derivers")
    (version "1.2.1")
    (home-page
     "https://github.com/ocaml-ppx/ppx_derivers")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0yqvqw58hbx1a61wcpbnl9j30n495k23qmyy2xwczqs63mn2nkpn"))))
    (build-system dune-build-system)
    (arguments
     '(#:tests? #f)) ;no tests
    (properties `((upstream-name . "ppx_derivers")))
    (synopsis "Shared @code{@@deriving} plugin registry")
    (description
     "Ppx_derivers is a tiny package whose sole purpose is to allow
ppx_deriving and ppx_type_conv to inter-operate gracefully when linked
as part of the same ocaml-migrate-parsetree driver.")
    (license license:bsd-3)))

(define-public ocaml-ppx-deriving-yojson
  (package
    (name "ocaml-ppx-deriving-yojson")
    (version "3.7.0")
    (home-page "https://github.com/ocaml-ppx/ppx_deriving_yojson")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nazam6zlzm9ngyyr1q7s1vmw162fnrvsn8r6bsn5lnpaygv28ly"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-yojson
            ocaml-result
            ocaml-ppx-deriving
            ocaml-ppxlib))
    (native-inputs (list ocaml-ounit))
    (properties `((upstream-name . "ppx_deriving_yojson")))
    (synopsis "JSON codec generator for OCaml")
    (description
     "Ppx_deriving_yojson is a ppx_deriving plugin that provides a JSON codec
generator.")
    (license license:expat)))

(define-public ocaml-cinaps
  ;; The commit removes the unused dependency of ocaml-ppx-jane. We need to
  ;; use this as we would otherwise have a dependency loop between
  ;; ocaml-ppxlib and ocaml-ppx-jane.
  (let ((commit "d974bb2db3ab1ab14e81f989b5bdb609462bff47")
        (revision "0"))
    (package
      (name "ocaml-cinaps")
      (version (git-version "0.15.1" revision commit))
      (home-page "https://github.com/ocaml-ppx/cinaps")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00kb04vqlnk1pynqjhna5qhn8790ab17baxf4na5py1l1h1js8qx"))))
      (build-system dune-build-system)
      (propagated-inputs (list ocaml-re))
      (synopsis "Trivial metaprogramming tool for OCaml")
      (description
       "Cinaps is a trivial Metaprogramming tool using the OCaml toplevel.  It is based
on the same idea as expectation tests.  The user writes some OCaml code inside
special comments and cinaps makes sure that what follows is what is printed by
the OCaml code.")
      (license license:expat))))

(define-public ocaml-ppxlib
  (package
    (name "ocaml-ppxlib")
    (version "0.28.0")
    (home-page "https://github.com/ocaml-ppx/ppxlib")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0alwn1bnc228z6ivj7dpcszrylbg1z3img8vrcihaa9crbx3xxcb"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test-format
           (lambda _
             ;; Since sexplib >= 0.15, error formatting has changed
             (substitute* "test/driver/exception_handling/run.t"
               (("\\(Failure ") "Failure("))
             (substitute* "test/base/test.ml"
               (("Invalid_argument \\((.*)\\)." _ m)
                (string-append "Invalid_argument " m "."))
               (("\\(Invalid_argument (.*)\\)" _ m)
                (string-append "Invalid_argument " m ".")))
             (substitute* "test/ppx_import_support/test.ml"
               (("\\(Failure") "Failure")
               (("  \"(Some ppx-es.*)\")" _ m)
                (string-append " \"" m "\".")))))
         (add-after 'fix-test-format 'fix-egrep
           (lambda _
             ;; egrep is obsolescent; using grep -E
             (substitute* "test/expansion_context/run.t"
               (("egrep") "grep -E")))))))
    (propagated-inputs
     (list ocaml-compiler-libs
           ocaml-ppx-derivers
           ocaml-sexplib0
           ocaml-stdlib-shims))
    (native-inputs
     (list ocaml-stdio
           ocaml-cinaps
           ocaml-base))
    (synopsis
     "Base library and tools for ppx rewriters")
    (description
     "A comprehensive toolbox for ppx development.  It features:
@itemize
@item an OCaml AST / parser / pretty-printer snapshot, to create a full frontend
independent of the version of OCaml;
@item a library for library for ppx rewriters in general, and type-driven code
generators in particular;
@item
a feature-full driver for OCaml AST transformers;
@item a quotation mechanism allowing to write values representing the
OCaml AST in the OCaml syntax;
@item a generator of open recursion classes from type definitions.
@end itemize")
    (license license:expat)))

(define-public ocaml-ppx-compare
  (package
    (name "ocaml-ppx-compare")
    (version "0.15.0")
    (source
     (janestreet-origin "ppx_compare" version
                        "11bkw7fgzfay8ws0piwphqip3y2lk2c9s2gil3zisnbvka92h1va"))
    (build-system dune-build-system)
    (arguments
     ;; Tests are currently failing
     ;; (see https://github.com/janestreet/ppx_compare/issues/10)
     '(#:tests? #f))
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_compare")))
    (home-page "https://github.com/janestreet/ppx_compare")
    (synopsis "Generation of comparison functions from types")
    (description "Generation of fast comparison functions from type expressions
and definitions.  Ppx_compare is a ppx rewriter that derives comparison functions
from type representations.  The scaffolded functions are usually much faster
than ocaml's Pervasives.compare.  Scaffolding functions also gives you more
flexibility by allowing you to override them for a specific type and more safety
by making sure that you only compare comparable values.")
    (license license:asl2.0)))

(define-public ocaml-fieldslib
  (package
    (name "ocaml-fieldslib")
    (version "0.15.0")
    (source (janestreet-origin
             "fieldslib" version
             "083izf854vzmi5zj63r7ipjf09y1dqf7iy8n6r4663444xrzs2h5"))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ; No tests
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "fieldslib")))
    (home-page "https://github.com/janestreet/fieldslib")
    (synopsis "Syntax extension to record fields")
    (description "Syntax extension to define first class values representing
record fields, to get and set record fields, iterate and fold over all fields
of a record and create new record values.")
    (license license:asl2.0)))

(define-public ocaml-variantslib
  (package
    (name "ocaml-variantslib")
    (version "0.15.0")
    (source
     (janestreet-origin "variantslib" version
              "12dssx4by6rgjzfrvksz83hkcpmsq0brn87dh22pv1rrwhw79n75"))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "variantslib")))
    (home-page "https://github.com/janestreet/variantslib")
    (synopsis "OCaml variants as first class values")
    (description "The Core suite of libraries is an alternative to OCaml's
standard library.")
    (license license:asl2.0)))

(define-public ocaml-ppx-fields-conv
  (package
    (name "ocaml-ppx-fields-conv")
    (version "0.15.0")
    (home-page "https://github.com/janestreet/ppx_fields_conv")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "094wsnw7fcwgl9xg6vkjb0wbgpn9scsp847yhdd184sz9v1amz14"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-fieldslib ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties `((upstream-name . "ppx_fields_conv")))
    (synopsis "Generation of accessor and iteration functions for ocaml records")
    (description "Ppx_fields_conv is a ppx rewriter that can be used to define
first class values representing record fields, and additional routines, to get
and set record fields, iterate and fold over all fields of a record and create
new record values.")
    (license license:asl2.0)))

(define-public ocaml-ppx-sexp-conv
  (package
    (name "ocaml-ppx-sexp-conv")
    (version "0.15.1")
    (home-page "https://github.com/janestreet/ppx_sexp_conv")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "163kn5gv1va84s1ysa1fm40fw3hm9xm3788qsvxmm4l3gij2g29m"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-ppxlib))
    (properties `((upstream-name . "ppx_sexp_conv")))
    (synopsis "Generation of S-expression conversion functions from type definitions")
    (description "This package generates S-expression conversion functions from type
definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-variants-conv
  (package
    (name "ocaml-ppx-variants-conv")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janestreet/ppx_variants_conv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dh0bw9dn246k00pymf59yjkl6x6bxd76lkk9b5xpq2692wwlc3s"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-variantslib ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties
     `((upstream-name . "ppx_variants_conv")))
    (home-page
     "https://github.com/janestreet/ppx_variants_conv")
    (synopsis "Generation of accessor and iteration functions for OCaml variant types")
    (description
     "This package generates accessors and iteration functions for OCaml
variant types.")
    (license license:asl2.0)))

(define-public ocaml-ppx-custom-printf
  (package
    (name "ocaml-ppx-custom-printf")
    (version "0.15.0")
    (home-page "https://github.com/janestreet/ppx_custom_printf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1k8nmq6kwqz2wpkm9ymq749dz1vd8lxrjc711knp1wyz5935hnsv"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-ppx-sexp-conv ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties `((upstream-name . "ppx_custom_printf")))
    (synopsis "Printf-style format-strings for user-defined string conversion")
    (description "Extensions to printf-style format-strings for user-defined
string conversion.")
    (license license:asl2.0)))

(define-public ocaml-ppx-stable-witness
  (package
    (name "ocaml-ppx-stable-witness")
    (version "0.16.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_stable_witness")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ppmazy1vw0j44z1p60ln6fddz414zmyrqkpi54w6cr93giqsl2v"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppxlib))
    (properties `((upstream-name . "ppx_stable_witness")))
    (home-page "https://github.com/janestreet/ppx_stable_witness")
    (synopsis "Mark a type as stable across versions")
    (description "This ppx extension is used for deriving a witness that a
type is intended to be stable.  In this context, stable means that the
serialization format will never change.  This allows programs running at
different versions of the code to safely communicate.")
    (license license:expat)))

(define-public ocaml-bin-prot
  (package
    (name "ocaml-bin-prot")
    (version "0.16.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/bin_prot")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ybs0152ilgr8sa1hqnc2jj0gbvg855ixl3c5b2pjbnk7blhqnd8"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-base
            ocaml-ppx-compare
            ocaml-ppx-custom-printf
            ocaml-ppx-fields-conv
            ocaml-ppx-optcomp
            ocaml-ppx-sexp-conv
            ocaml-ppx-stable-witness
            ocaml-ppx-variants-conv))
    (properties `((upstream-name . "bin_prot")))
    (home-page "https://github.com/janestreet/bin_prot")
    (synopsis "Binary protocol generator")
    (description "This library contains functionality for reading and writing
OCaml-values in a type-safe binary protocol.  It is extremely efficient,
typically supporting type-safe marshalling and unmarshalling of even highly
structured values at speeds sufficient to saturate a gigabit connection.  The
protocol is also heavily optimized for size, making it ideal for long-term
storage of large amounts of data.")
    (license license:expat)))

(define-public ocaml-protocol-version-header
  (package
    (name "ocaml-protocol-version-header")
    (version "0.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/janestreet/protocol_version_header")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s638cwf1357gg754rc4306654hhrhzqaqm2lp3yv5vj3ml8p4qy"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-core ocaml-ppx-jane))
    (properties `((upstream-name . "protocol_version_header")))
    (home-page "https://github.com/janestreet/protocol_version_header")
    (synopsis "Protocol versioning")
    (description
     "This library offers a lightweight way for applications protocols to
version themselves.  The more protocols that add themselves to
@code{Known_protocol}, the nicer error messages we will get when connecting to
a service while using the wrong protocol.")
    (license license:expat)))

(define-public ocaml-octavius
  (package
    (name "ocaml-octavius")
    (version "1.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml-doc/octavius")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c5m51xcn2jv42kjjpklr6g63sgx1k885wfdp1yr4wrmiaj9cbpx"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'make-writable
           (lambda _
             (for-each (lambda (file)
                         (chmod file #o644))
                       (find-files "." "."))
             #t)))))
    (properties `((upstream-name . "octavius")))
    (home-page "https://github.com/ocaml-doc/octavius")
    (synopsis "Ocamldoc comment syntax parser")
    (description "Octavius is a library to parse the `ocamldoc` comment syntax.")
    (license license:isc)))

(define-public ocaml-sha
  (package
    (name "ocaml-sha")
    (version "1.15.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/djs55/ocaml-sha/releases/download/"
                                  version "/sha-" version ".tbz"))
              (sha256
               (base32
                "1dzzhchknnbrpp5s81iqbvmqp4s0l75yrq8snj70ch3wkarmgg9z"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-stdlib-shims ocaml-odoc))
    (native-inputs (list ocaml-ounit2))
    (home-page "https://github.com/djs55/ocaml-sha")
    (synopsis "OCaml binding to the SHA cryptographic functions")
    (description
     "This is the binding for SHA interface code in OCaml, offering the same
interface as the MD5 digest included in the OCaml standard library.  It
currently provides SHA1, SHA256 and SHA512 hash functions.")
    (license license:isc)))

(define-public ocaml-ppx-hash
  (package
    (name "ocaml-ppx-hash")
    (version "0.15.0")
    (source
     (janestreet-origin "ppx_hash" version
                "048pim0xicj8j9whd5lnchf62788sk3w89h12aybbalk1xm6dfs5"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-ppx-compare ocaml-ppx-sexp-conv
           ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_hash")))
    (home-page "https://github.com/janestreet/ppx_hash")
    (synopsis "Generation of hash functions from type expressions and definitions")
    (description "This package is a collection of ppx rewriters that generate
hash functions from type exrpessions and definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-enumerate
  (package
    (name "ocaml-ppx-enumerate")
    (version "0.15.0")
    (source
     (janestreet-origin
      "ppx_enumerate" version
      "16yhk3xk2hskmlspb6mikmdp60qaypyiqgq9p17kxpial6fgpdfy"))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_enumerate")))
    (home-page "https://github.com/janestreet/ppx_enumerate")
    (synopsis "Generate a list containing all values of a finite type")
    (description "Ppx_enumerate is a ppx rewriter which generates a definition
for the list of all values of a type (for a type which only has finitely
many values).")
    (license license:asl2.0)))

(define-public ocaml-ppx-bench
  (package
    (name "ocaml-ppx-bench")
    (version "0.16.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_bench")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0pry0vgi1ilwdi5viqydyf9n7y289c17s8wfn16wyzzfr887769m"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs (list ocaml-ppx-inline-test ocaml-ppxlib))
    (properties `((upstream-name . "ppx_bench")))
    (home-page "https://github.com/janestreet/ppx_bench")
    (synopsis "Syntax extension for writing in-line benchmarks in ocaml code")
    (description "Syntax extension for writing in-line benchmarks in ocaml code.")
    (license license:expat)))

(define-public ocaml-ppx-here
  (package
    (name "ocaml-ppx-here")
    (version "0.15.0")
    (source
     (janestreet-origin "ppx_here" version
                        "1pyaw31j9n6r98ar947n3j2qj6rrszbdxr8jghk96j4ajdy05g65"))
    (build-system dune-build-system)
    (arguments
     ;; broken tests
     `(#:tests? #f))
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_here")))
    (home-page "https://github.com/janestreet/ppx_here")
    (synopsis "Expands [%here] into its location")
    (description
      "Part of the Jane Street's PPX rewriters collection.")
    (license license:asl2.0)))

(define-public ocaml-typerep
  (package
    (name "ocaml-typerep")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/typerep")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1qxfi01qim0hrgd6d0bgvpxg36i99mmm8cw4wqpr9kxyqvgzv26z"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)); no tests
    (propagated-inputs (list ocaml-base))
    (home-page "https://github.com/janestreet/typerep")
    (synopsis "Typerep is a library for runtime types")
    (description "Typerep is a library for runtime types.")
    (license license:expat)))

(define-public ocaml-ppx-sexp-value
  (package
    (name "ocaml-ppx-sexp-value")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_sexp_value")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0kz83j9v6yz3v8c6vr9ilhkcci4hhjd6i6r6afnx72jh6i7d3hnv"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-base ocaml-ppx-here ocaml-ppx-sexp-conv ocaml-ppxlib))
    (properties `((upstream-name . "ppx_sexp_value")))
    (home-page "https://github.com/janestreet/ppx_sexp_value")
    (synopsis "Simplify building s-expressions from ocaml values")
    (description "@samp{ppx-sexp-value} is a ppx rewriter that simplifies
building s-expressions from ocaml values.")
    (license license:expat)))

(define-public ocaml-ppx-sexp-message
  (package
    (name "ocaml-ppx-sexp-message")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_sexp_message")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0a7hx50bkkc5n5msc3zzc4ixnp7674x3mallknb9j31jnd8l90nj"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-base ocaml-ppx-here ocaml-ppx-sexp-conv ocaml-ppxlib))
    (properties `((upstream-name . "ppx_sexp_message")))
    (home-page "https://github.com/janestreet/ppx_sexp_message")
    (synopsis "Ppx rewriter for easy construction of s-expressions")
    (description "Ppx_sexp_message aims to ease the creation of s-expressions
in OCaml.  This is mainly motivated by writing error and debugging messages,
where one needs to construct a s-expression based on various element of the
context such as function arguments.")
    (license license:expat)))

(define-public ocaml-ppx-pipebang
  (package
    (name "ocaml-ppx-pipebang")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_pipebang")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0sm5dghyalhws3hy1cc2ih36az1k4q02hcgj6l26gwyma3y4irvq"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)); no tests
    (propagated-inputs (list ocaml-ppxlib))
    (properties `((upstream-name . "ppx_pipebang")))
    (home-page "https://github.com/janestreet/ppx_pipebang")
    (synopsis "Inline reverse application operators `|>` and `|!`")
    (description "A ppx rewriter that inlines reverse application operators
@code{|>} and @code{|!}.")
    (license license:expat)))

(define-public ocaml-ppx-module-timer
  (package
    (name "ocaml-ppx-module-timer")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_module_timer")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0lzi5hxi10p89ddqbrc667267f888kqslal76gfhmszyk60n20av"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
      (list ocaml-base ocaml-ppx-base ocaml-stdio ocaml-time-now ocaml-ppxlib))
    (properties `((upstream-name . "ppx_module_timer")))
    (home-page "https://github.com/janestreet/ppx_module_timer")
    (synopsis "Ppx rewriter that records top-level module startup times")
    (description "Modules using @samp{ppx_module_timer} have instrumentation
to record their startup time.")
    (license license:expat)))

(define-public ocaml-ppx-fixed-literal
  (package
    (name "ocaml-ppx-fixed-literal")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_fixed_literal")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "10siwcqrqa4gh0mg6fkaby0jjskc01r81pcblc67h3vmbjjh08j9"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs (list ocaml-base ocaml-ppxlib))
    (properties `((upstream-name . "ppx_fixed_literal")))
    (home-page "https://github.com/janestreet/ppx_fixed_literal")
    (synopsis "Simpler notation for fixed point literals")
    (description
      "@samp{ppx-fixed-literal} is a ppx rewriter that rewrites fixed point
literal of the  form 1.0v to conversion functions currently in scope.")
    (license license:expat)))

(define-public ocaml-ppx-optional
  (package
    (name "ocaml-ppx-optional")
    (version "0.15.0")
    (source
     (janestreet-origin
      "ppx_optional" version
      "0af7ayhfc1jc01mxs4k253gq49yss2ymkmjsy6fpcz39zhci7fvj"))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ; No tests
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib))
    (properties `((upstream-name . "ppx_optional")))
    (home-page "https://github.com/janestreet/ppx_optional")
    (synopsis "Pattern matching on flat options")
    (description
      "A ppx rewriter that rewrites simple match statements with an if then
else expression.")
    (license license:asl2.0)))

(define-public ocaml-ppx-optcomp
  (package
    (name "ocaml-ppx-optcomp")
    (version "0.15.0")
    (home-page "https://github.com/janestreet/ppx_optcomp")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ypivfipi8fcr9pqyvl2ajpcivmr1irdwwv248i4x6mggpc2pl0b"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-stdio ocaml-ppxlib))
    (properties `((upstream-name . "ppx_optcomp")))
    (synopsis "Optional compilation for OCaml")
    (description "Ppx_optcomp stands for Optional Compilation.  It is a tool
used to handle optional compilations of pieces of code depending of the word
size, the version of the compiler, ...")
    (license license:asl2.0)))

(define-public ocaml-ppx-let
  (package
    (name "ocaml-ppx-let")
    (version "0.15.0")
    (source
     (janestreet-origin "ppx_let" version
                        "0m9niyiiv3qzv5x8hw0ifxjjzshnmx40dchka9d93mmnx88jqx34"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-ppxlib ocaml-ppx-here))
    (properties `((upstream-name . "ppx_let")))
    (home-page "https://github.com/janestreet/ppx_let")
    (synopsis "Monadic let-bindings")
    (description "A ppx rewriter for monadic and applicative let bindings,
match expressions, and if expressions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-fail
  (package
    (name "ocaml-ppx-fail")
    (version "0.14.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_fail")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "012p9gv7w4sk3b4x0sdmqrmr2856w8xc424waxb6vrybid7qjs95"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppx-here ocaml-ppxlib))
    (properties `((upstream-name . "ppx_fail")))
    (home-page "https://github.com/janestreet/ppx_fail")
    (synopsis "Add location to calls to failwiths")
    (description "Syntax extension that makes [failwiths] always include a
position.")
    (license license:expat)))

(define-public ocaml-ppx-cold
  (package
    (name "ocaml-ppx-cold")
    (version "0.15.0")
    (home-page "https://github.com/janestreet/ppx_cold")
    (source
     (janestreet-origin "ppx_cold" version
                        "13gqmfw2sq80anag9bwpm35600l1fnfn7mh9cbj1291k84rsx7wb"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-ppxlib))
    (properties `((upstream-name . "ppx_cold")))
    (synopsis "Syntax extension for indicating cold path")
    (description
     "This package contains an syntax extension to indicate that the code is
on the cold path and should be kept out of the way to avoid polluting the
instruction cache on the hot path.  See also
https://github.com/ocaml/ocaml/issues/8563.")
    (license license:expat)))

(define-public ocaml-ppx-assert
  (package
    (name "ocaml-ppx-assert")
    (version "0.15.0")
    (source
     (janestreet-origin "ppx_assert" version
                        "0rsr1yz2rs12w6qw0dz09dg3k2x2pfgd014fgp6nj993hhznapsf"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base
           ocaml-ppx-cold
           ocaml-ppx-compare
           ocaml-ppx-here
           ocaml-ppx-sexp-conv
           ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties `((upstream-name . "ppx_assert")))
    (home-page "https://github.com/janestreet/ppx_assert")
    (synopsis "Assert-like extension nodes that raise useful errors on failure")
    (description "This package contains assert-like extension nodes that raise
useful errors on failure.")
    (license license:asl2.0)))

(define-public ocaml-ppx-expect
  (package
    (name "ocaml-ppx-expect")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janestreet/ppx_expect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "134dl5qhjxsj2mcmrx9f3m0iys0n5mjfpz9flj8zn8d2jir43776"))))
    (build-system dune-build-system)
    (arguments
     ;; Cyclic dependency with ocaml-ppx-jane
     `(#:tests? #f))
    (propagated-inputs
     (list ocaml-base
           ocaml-ppx-here
           ocaml-ppx-inline-test
           ocaml-stdio
           ocaml-ppxlib
           ocaml-migrate-parsetree
           ocaml-re))
    (properties `((upstream-name . "ppx_expect")
                  (ocaml5.0-variant . ,(delay ocaml5.0-ppx-expect))))
    (home-page "https://github.com/janestreet/ppx_expect")
    (synopsis "Cram like framework for OCaml")
    (description "Expect-test is a framework for writing tests in OCaml, similar
to Cram.  Expect-tests mimics the existing inline tests framework with the
@code{let%expect_test} construct.  The body of an expect-test can contain
output-generating code, interleaved with @code{%expect} extension expressions
to denote the expected output.")
    (license license:asl2.0)))

(define-public ocaml5.0-ppx-expect
  ;; Contains fixes for OCaml 5.0
  ;; (https://github.com/janestreet/ppx_expect/pull/39/).
  (let ((commit "83edfc1ee779e8dcdd975e26715c2e688326befa")
        (revision "0"))
    (package-with-ocaml5.0
     (package
       (inherit ocaml-ppx-expect)
       (name "ocaml-ppx-expect")
       (version (git-version "0.15.0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/janestreet/ppx_expect")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "05r7wlmrhb5biwyw6bjcpmr77srglijcbf7nm7h2hiil0d0i7bkz"))))
       (properties '())))))

(define-public ocaml-ppx-js-style
  (package
    (name "ocaml-ppx-js-style")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janestreet/ppx_js_style")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q2p9pvmlncgv0hprph95xiv7s6q44ynvp4yl4dckf1qx68rb3ba"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)) ; No tests
    (propagated-inputs
     (list ocaml-base ocaml-migrate-parsetree ocaml-octavius ocaml-ppxlib))
    (properties `((upstream-name . "ppx_js_style")))
    (home-page "https://github.com/janestreet/ppx_js_style")
    (synopsis "Code style checker for Jane Street Packages")
    (description "This package is a no-op ppx rewriter.  It is used as a
@code{lint} tool to enforce some coding conventions across all Jane Street
packages.")
    (license license:asl2.0)))

(define-public ocaml-ppx-typerep-conv
  (package
    (name "ocaml-ppx-typerep-conv")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/janestreet/ppx_typerep_conv/")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1q1lzykpm83ra4l5jh4rfddhd3c96kx4s4rvx0w4b51z1qk56zam"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-typerep ocaml-ppxlib))
    (properties `((upstream-name . "ppx_typerep_conv")))
    (home-page "https://github.com/janestreet/ppx_typerep_conv")
    (synopsis "Generation of runtime types from type declarations")
    (description "This package can automatically generate runtime types
from type definitions.")
    (license license:expat)))

(define-public ocaml-ppx-string
  (package
    (name "ocaml-ppx-string")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_string")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1dp5frk6cig5m3m5rrh2alw63snyf845x7zlkkaljip02pqcbw1s"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f)); no tests
    (propagated-inputs
      (list ocaml-base ocaml-ppx-base ocaml-stdio ocaml-ppxlib))
    (properties `((upstream-name . "ppx_string")))
    (home-page "https://github.com/janestreet/ppx_string")
    (synopsis "Ppx extension for string interpolation")
    (description "This extension provides a syntax for string interpolation.")
    (license license:expat)))

(define-public ocaml-ppx-stable
  (package
    (name "ocaml-ppx-stable")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janestreet/ppx_stable")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1as0v0x8c9ilyhngax55lvwyyi4a2wshyan668v0f2s1608cwb1l"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppxlib))
    (properties `((upstream-name . "ppx_stable")))
    (home-page "https://github.com/janestreet/ppx_stable")
    (synopsis "Stable types conversions generator")
    (description "This package is a ppx extension for easier implementation of
conversion functions between almost identical types.")
    (license license:expat)))

(define-public ocaml-ppx-base
  (package
    (name "ocaml-ppx-base")
    (version "0.15.0")
    (source
     (janestreet-origin
      "ppx_base" version
      "181w7y2has8jsrqdsvd08q5nhnkx523vwsk3lg0cjix55qssvfyn"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-ppx-compare
           ocaml-ppx-cold
           ocaml-ppx-enumerate
           ocaml-ppx-hash
           ocaml-ppx-js-style
           ocaml-ppx-sexp-conv
           ocaml-migrate-parsetree
           ocaml-ppxlib))
    (properties `((upstream-name . "ppx_base")))
    (home-page "https://github.com/janestreet/ppx_base")
    (synopsis "Base set of ppx rewriters")
    (description "Ppx_base is the set of ppx rewriters used for Base.

Note that Base doesn't need ppx to build, it is only used as a
verification tool.")
    (license license:asl2.0)))

(define-public ocaml-ppx-bin-prot
  (package
    (name "ocaml-ppx-bin-prot")
    (version "0.16.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_bin_prot")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "091l1gvgbibrqzy9a31vz03wzfs7z5zmk5ldl5yvg742x7idmmwj"))))
    (build-system dune-build-system)
    (arguments
     ;; Cyclic dependency with ocaml-ppx-jane
     `(#:tests? #f))
    (propagated-inputs
      (list ocaml-base ocaml-bin-prot ocaml-ppx-here ocaml-ppxlib))
    (properties `((upstream-name . "ppx_bin_prot")))
    (home-page "https://github.com/janestreet/ppx_bin_prot")
    (synopsis "Generation of bin_prot readers and writers from types")
    (description "Generation of binary serialization and deserialization
functions from type definitions.")
    (license license:expat)))

(define-public ocaml-ppx-ignore-instrumentation
  (package
    (name "ocaml-ppx-ignore-instrumentation")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_ignore_instrumentation")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "16fgig88g3jr0m3i636fr52h29h1yzhi8nhnl4029zn808kcdyj2"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ;no tests
    (propagated-inputs (list ocaml-ppxlib))
    (properties `((upstream-name . "ppx_ignore_instrumentation")))
    (home-page "https://github.com/janestreet/ppx_ignore_instrumentation")
    (synopsis "Ignore Jane Street specific instrumentation extensions")
    (description
      "Ignore Jane Street specific instrumentation extensions from internal
PPXs or compiler features not yet upstreamed.")
    (license license:expat)))

(define-public ocaml-ppx-log
  (package
    (name "ocaml-ppx-log")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_log")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "08i9gz3f4w3bmlrfdw7ja9awsfkhhldz03bnnc4hijfmn8sawzi0"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-base
            ocaml-ppx-here
            ocaml-ppx-sexp-conv
            ocaml-ppx-sexp-message
            ocaml-sexplib
            ocaml-ppxlib))
    (properties `((upstream-name . "ppx_log")))
    (home-page "https://github.com/janestreet/ppx_log")
    (synopsis "Extension nodes for lazily rendering log messages")
    (description "This package provides ppx_sexp_message-like extension
nodes for lazily rendering log messages.")
    (license license:expat)))

(define-public ocaml-ppx-disable-unused-warnings
  (package
    (name "ocaml-ppx-disable-unused-warnings")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_disable_unused_warnings")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0sb5i4v7p9df2bxk66rjs30k9fqdrwsq1jgykjv6wyrx2d9bv955"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppxlib))
    (properties `((upstream-name . "ppx_disable_unused_warnings")))
    (home-page "https://github.com/janestreet/ppx_disable_unused_warnings")
    (synopsis "Simple ppx extension for commonly unused warnings")
    (description "This package expands @code{@@disable_unused_warnings} into
@code{@@warning \"-20-26-32-33-34-35-36-37-38-39-60-66-67\"}")
    (license license:expat)))

(define-public ocaml-ppx-jane
  (package
    (name "ocaml-ppx-jane")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/ppx_jane")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1p6847gdfnnj6qpa4yh57s6wwpsl7rfgy0q7993chz24h9mhz5lk"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-base-quickcheck
            ocaml-ppx-assert
            ocaml-ppx-base
            ocaml-ppx-bench
            ocaml-ppx-bin-prot
            ocaml-ppx-custom-printf
            ocaml-ppx-disable-unused-warnings
            ocaml-ppx-expect
            ocaml-ppx-fields-conv
            ocaml-ppx-fixed-literal
            ocaml-ppx-here
            ocaml-ppx-ignore-instrumentation
            ocaml-ppx-inline-test
            ocaml-ppx-let
            ocaml-ppx-log
            ocaml-ppx-module-timer
            ocaml-ppx-optcomp
            ocaml-ppx-optional
            ocaml-ppx-pipebang
            ocaml-ppx-sexp-message
            ocaml-ppx-sexp-value
            ocaml-ppx-stable
            ocaml-ppx-string
            ocaml-ppx-typerep-conv
            ocaml-ppx-variants-conv
            ocaml-ppxlib))
    (properties `((upstream-name . "ppx_jane")))
    (home-page "https://github.com/janestreet/ppx_jane")
    (synopsis "Standard Jane Street ppx rewriters")
    (description "This package installs a ppx-jane executable, which is a ppx
driver including all standard Jane Street ppx rewriters.")
    (license license:expat)))

(define-public ocaml-base-bigstring
  (package
    (name "ocaml-base-bigstring")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janestreet/base_bigstring")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hv3hw2fwqmkrxms1g6rw3c18mmla1z5bva3anx45mnff903iv4q"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-int-repr ocaml-ppx-jane))
    (properties `((upstream-name . "base_bigstring")))
    (home-page "https://github.com/janestreet/base_bigstring")
    (synopsis "String type based on [Bigarray], for use in I/O and C-bindings")
    (description "This package provides string type based on [Bigarray], for
use in I/O and C-bindings.")
    (license license:expat)))

(define-public ocaml-splittable-random
  (package
    (name "ocaml-splittable-random")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/splittable_random")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0ap5z4z1aagz4z02q9642cbl25jzws9lbc2x5xkpyjlc0qcm9v3m"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-base
            ocaml-ppx-assert
            ocaml-ppx-bench
            ocaml-ppx-inline-test
            ocaml-ppx-sexp-message))
    (properties `((upstream-name . "splittable_random")))
    (home-page "https://github.com/janestreet/splittable_random")
    (synopsis "PRNG that can be split into independent streams")
    (description "This package provides a splittable
@acronym{PRNG,pseudo-random number generator} functions like a PRNG that can
be used as a stream of random values; it can also be split to produce a
second, independent stream of random values.

This library implements a splittable pseudo-random number generator that sacrifices
cryptographic-quality randomness in favor of performance.")
    (license license:expat)))

(define-public ocaml-base-quickcheck
  (package
    (name "ocaml-base-quickcheck")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/base_quickcheck")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0q73kfr67cz5wp4qn4rq3lpa922hqmvwdiinnans0js65fvlgqsi"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-base
            ocaml-ppx-base
            ocaml-ppx-fields-conv
            ocaml-ppx-let
            ocaml-ppx-sexp-message
            ocaml-ppx-sexp-value
            ocaml-splittable-random
            ocaml-ppxlib))
    (properties `((upstream-name . "base_quickcheck")))
    (home-page "https://github.com/janestreet/base_quickcheck")
    (synopsis
      "Randomized testing framework, designed for compatibility with Base")
    (description
      "@samp{base-quickcheck} provides randomized testing in the style of
Haskell's Quickcheck library, with support for built-in types as well as
types provided by Base.")
    (license license:expat)))

(define-public ocaml-spawn
  (package
    (name "ocaml-spawn")
    (version "0.15.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janestreet/spawn")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16aq5z3mq5lkpryfs4w0748b2w9v061myml0hn7nhh6r6i329w7a"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-odoc))
    (native-inputs (list ocaml-ppx-expect))
    (home-page "https://github.com/janestreet/spawn")
    (synopsis "Spawning sub-processes")
    (description
      "Spawn is a small library exposing only one functionality: spawning sub-process.

It has three main goals:

@itemize
@item provide missing features of Unix.create_process such as providing a
working directory,
@item provide better errors when a system call fails in the
sub-process.  For instance if a command is not found, you get a proper
@code{Unix.Unix_error} exception,
@item improve performances by using vfork when available.  It is often
claimed that nowadays fork is as fast as vfork, however in practice
fork takes time proportional to the process memory while vfork is
constant time.  In application using a lot of memory, vfork can be
thousands of times faster than fork.
@end itemize")
    (license license:asl2.0)))

(define-public ocaml-core
  (package
    (name "ocaml-core")
    (version "0.15.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/core")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "17vc2i5qb53dr0civ8pkrnnsn2nkydlq44ash7fhh93yb4sffy28"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "core"
       #:tests? #f)); Require a cyclic dependency: core_extended
    (propagated-inputs
      (list ocaml-base
            ocaml-base-bigstring
            ocaml-base-quickcheck
            ocaml-bin-prot
            ocaml-fieldslib
            ocaml-jane-street-headers
            ocaml-jst-config
            ocaml-ppx-assert
            ocaml-ppx-base
            ocaml-ppx-hash
            ocaml-ppx-inline-test
            ocaml-ppx-jane
            ocaml-ppx-sexp-conv
            ocaml-ppx-sexp-message
            ocaml-sexplib
            ocaml-splittable-random
            ocaml-stdio
            ocaml-time-now
            ocaml-typerep
            ocaml-variantslib))
    (home-page "https://github.com/janestreet/core")
    (synopsis "Alternative to OCaml's standard library")
    (description "The Core suite of libraries is an alternative to OCaml's
standard library that was developed by Jane Street.")
    ;; Also contains parts of OCaml, relicensed to expat, as permitted
    ;; by OCaml's license for consortium members (see THIRD-PARTY.txt).
    (license license:expat)))

(define-public ocaml-int-repr
  (package
    (name "ocaml-int-repr")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/int_repr")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0ph88ym3s9dk30n17si2xam40sp8wv1xffw5cl3bskc2vfya1nvl"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ;no tests
    (propagated-inputs (list ocaml-base ocaml-ppx-jane))
    (properties `((upstream-name . "int_repr")))
    (home-page "https://github.com/janestreet/int_repr")
    (synopsis "Integers of various widths")
    (description "Integers of various widths.")
    (license license:expat)))

(define-public ocaml-core-kernel
  (package
    (name "ocaml-core-kernel")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/core_kernel")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "05mb4vbf293iq1xx4acyrmi9cgcw6capwrsa54ils62alby6w6yq"))))
    (build-system dune-build-system)
    (arguments
     ;; Cyclic dependency with ocaml-core
     `(#:tests? #f))
    (propagated-inputs
      (list ocaml-base ocaml-core ocaml-int-repr ocaml-ppx-jane))
    (properties `((upstream-name . "core_kernel")))
    (home-page "https://github.com/janestreet/core_kernel")
    (synopsis "Portable standard library for OCaml")
    (description "Core is an alternative to the OCaml standard library.

Core_kernel is the system-independent part of Core.  It is aimed for cases when
the full Core is not available, such as in Javascript.")
    (license license:expat)))

(define-public ocaml-core-unix
  (package
    (name "ocaml-core-unix")
    (version "0.15.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/janestreet/core_unix")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h6lqaxpp4r06a63k8yr0g9y7wc8r35v2xzqgvkiiq1ypa48zzgm"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-core
                             ocaml-core-kernel
                             ocaml-expect-test-helpers-core
                             ocaml-jane-street-headers
                             ocaml-jst-config
                             ocaml-intrinsics
                             ocaml-ppx-jane
                             ocaml-sexplib
                             ocaml-timezone
                             ocaml-spawn))
    (properties `((upstream-name . "core_unix")))
    (home-page "https://github.com/janestreet/core_unix")
    (synopsis "Unix-specific portions of Core")
    (description
     "Unix-specific extensions to some of the modules defined in core and
core_kernel.")
    (license license:expat)))

(define-public ocaml-async-kernel
  (package
    (name "ocaml-async-kernel")
    (version "0.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/janestreet/async_kernel")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01if6c8l2h64v7sk56xr8acnmj6g9whxcjrzzzvczspq88hq2bfh"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-core ocaml-core-kernel ocaml-ppx-jane))
    (properties `((upstream-name . "async_kernel")))
    (home-page "https://github.com/janestreet/async_kernel")
    (synopsis "Monadic concurrency library")
    (description
     "Contains @code{Async}'s core data structures, like
@code{Deferred}.  @code{Async_kernel} is portable, and so can be used in
JavaScript using @code{Async_js}.")
    (license license:expat)))

(define-public ocaml-async-unix
  (package
    (name "ocaml-async-unix")
    (version "0.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/janestreet/async_unix")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z4fgpn93iw0abd7l9kac28qgzgc5qr2x0s1n2zh49lsdn02n6ys"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-async-kernel ocaml-core ocaml-core-kernel
                             ocaml-core-unix ocaml-ppx-jane))
    (properties `((upstream-name . "async_unix")))
    (home-page "https://github.com/janestreet/async_unix")
    (synopsis "Monadic concurrency library")
    (description
     "Unix-related dependencies for things like system calls and
threads.  Using these, it hooks the Async_kernel scheduler up to either epoll
or select, depending on availability, and manages a thread pool that blocking
system calls run in.")
    (license license:expat)))

(define-public ocaml-async-rpc-kernel
  (package
    (name "ocaml-async-rpc-kernel")
    (version "0.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/janestreet/async_rpc_kernel")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b5rp5yam03ir4f1sixpzjg1zdqmkb7lvnaa82kac4fzk80gfrfr"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-async-kernel ocaml-core ocaml-ppx-jane
                             ocaml-protocol-version-header))
    (properties `((upstream-name . "async_rpc_kernel")))
    (home-page "https://github.com/janestreet/async_rpc_kernel")
    (synopsis "Platform-independent core of Async RPC library")
    (description
     "Library for building RPC-style protocols.  This library is the portable
part of the Unix-oriented Async_rpc library, and is actively used in
JavaScript.")
    (license license:expat)))

(define-public ocaml-async
  (package
    (name "ocaml-async")
    (version "0.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/janestreet/async")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pykmnsil754jsnr8gss91ykyjvivngx4ii0ih3nsg1x2jl9xmy2"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-async-kernel
                             ocaml-async-rpc-kernel
                             ocaml-async-unix
                             ocaml-core
                             ocaml-core-kernel
                             ocaml-core-unix
                             ocaml-ppx-jane
                             ocaml-ppx-log
                             ocaml-textutils))
    ;; TODO one test dependency is deprecated, the other is nowhere to be found
    (arguments
     '(#:tests? #f))
    ;; (native-inputs (list ocaml-netkit-sockets ocaml-qtest-deprecated))
    (home-page "https://github.com/janestreet/async")
    (synopsis "Asynchronous execution library")
    (description
     "Library for asynchronous programming, i.e., programming where some part
of the program must wait for things that happen at times determined by some
external entity (like a human or another program).")
    (license license:expat)))

(define-public ocaml-textutils-kernel
  (package
    (name "ocaml-textutils-kernel")
    (version "0.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/janestreet/textutils_kernel")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "068g11d98wsb5a6ds0p5xybdmx5nx9bxa0k11dmh3l57kn4c169x"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-core ocaml-ppx-jane ocaml-uutf))
    (properties `((upstream-name . "textutils_kernel")))
    (home-page "https://github.com/janestreet/textutils_kernel")
    (synopsis "Text output utilities")
    (description
     "The subset of textutils using only core_kernel and working in
javascript.")
    (license license:expat)))

(define-public ocaml-textutils
  (package
    (name "ocaml-textutils")
    (version "0.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/janestreet/textutils")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wass49h645wql9b7nck2iqlkf4648dkxvlvxixr7z80zcnb5rxr"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-core
                             ocaml-core-kernel
                             ocaml-core-unix
                             ocaml-ppx-jane
                             ocaml-textutils-kernel
                             ocaml-uutf))
    (home-page "https://github.com/janestreet/textutils")
    (synopsis "Text output utilities")
    (description
     "Utilities for working with terminal output, such as color printing.")
    (license license:expat)))

(define-public ocaml-timezone
  (package
    (name "ocaml-timezone")
    (version "0.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/janestreet/timezone")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "00a007aji5rbz42kgbq1w90py6fm9k9akycs5abkcfll5rd0cbhx"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-core ocaml-ppx-jane))
    (home-page "https://github.com/janestreet/timezone")
    (synopsis "Time-zone handling")
    (description
      "Timezone handles parsing timezone data and create @code{Timezone.t}
that can later be used to manipulate time in core_kernel or core.")
    (license license:expat)))

(define-public ocaml-markup
  (package
    (name "ocaml-markup")
    (version "1.0.3")
    (home-page "https://github.com/aantron/markup.ml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1acgcbhx4rxx92rf65lsns588d6zzfrin2pnpkx24jw5vbgz7idn"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "markup"))
    (propagated-inputs
     (list ocaml-bisect-ppx ocaml-uchar ocaml-uutf ocaml-lwt))
    (native-inputs
     (list ocaml-ounit2 pkg-config))
    (synopsis "Error-recovering functional HTML5 and XML parsers and writers")
    (description "Markup.ml provides an HTML parser and an XML parser.  The
parsers are wrapped in a simple interface: they are functions that transform
byte streams to parsing signal streams.  Streams can be manipulated in various
ways, such as processing by fold, filter, and map, assembly into DOM tree
structures, or serialization back to HTML or XML.

Both parsers are based on their respective standards.  The HTML parser, in
particular, is based on the state machines defined in HTML5.

The parsers are error-recovering by default, and accept fragments.  This makes
it very easy to get a best-effort parse of some input.  The parsers can,
however, be easily configured to be strict, and to accept only full documents.

Apart from this, the parsers are streaming (do not build up a document in
memory), non-blocking (can be used with threading libraries), lazy (do not
consume input unless the signal stream is being read), and process the input in
a single pass.  They automatically detect the character encoding of the input
stream, and convert everything to UTF-8.")
    (license license:bsd-3)))

(define-public ocaml-tyxml
  (package
    (name "ocaml-tyxml")
    (version "4.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocsigen/tyxml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bh66wknc7sx2r63kscp0hg6h73dkv6qpkx0cdz2qp7p28pg2ixz"))))
    (build-system dune-build-system)
    (inputs
     (list ocaml-re ocaml-seq ocaml-uutf))
    (native-inputs
     (list ocaml-alcotest))
    (arguments `(#:package "tyxml"))
    (home-page "https://github.com/ocsigen/tyxml/")
    (synopsis "TyXML is a library for building correct HTML and SVG documents")
    (description "TyXML provides a set of convenient combinators that uses the
OCaml type system to ensure the validity of the generated documents.  TyXML can
be used with any representation of HTML and SVG: the textual one, provided
directly by this package, or DOM trees (@code{js_of_ocaml-tyxml}) virtual DOM
(@code{virtual-dom}) and reactive or replicated trees (@code{eliom}).  You can
also create your own representation and use it to instantiate a new set of
combinators.")
    (license license:lgpl2.1)))

(define-public ocaml-bisect-ppx
  (package
    (name "ocaml-bisect-ppx")
    (version "2.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aantron/bisect_ppx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1albx01qvr4fax4wkdmy0dd21q0fd9ixsgsvnr1z32ngj9nyi9fy"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-ppxlib ocaml-cmdliner))
    (arguments
     ;; Tests require ocamlformat which would lead to circular dependencies
     '(#:tests? #f))
    (properties `((upstream-name . "bisect_ppx")))
    (home-page "https://github.com/aantron/bisect_ppx")
    (synopsis "Code coverage for OCaml")
    (description "Bisect_ppx helps you test thoroughly.  It is a small
preprocessor that inserts instrumentation at places in your code, such as
if-then-else and match expressions.  After you run tests, Bisect_ppx gives a
nice HTML report showing which places were visited and which were missed.

Usage is simple - add package bisect_ppx when building tests, run your tests,
then run the Bisect_ppx report tool on the generated visitation files.")
    (license license:mpl2.0)))

(define-public ocaml-odoc
  (package
    (name "ocaml-odoc")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/odoc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19mww1lyrdi705aw3lwql6xc7p11fcq5gprmhyxpb4x80gnvlzrh"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f; not compatible with current version of ocaml-yojson
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test
           (lambda _
             ;; test results expects #!/bin/sh but gets a store path instead
             (substitute* "test/xref2/with.t/run.t"
               (("#!/bin/sh") (string-append "#!" (which "sh")))))))))
    (inputs
    (list ocaml-astring
          ocaml-bisect-ppx
          ocaml-cmdliner
          ocaml-fmt
          ocaml-fpath
          ocaml-logs
          ocaml-migrate-parsetree
          ocaml-odoc-parser
          ocaml-re
          ocaml-result
          ocaml-tyxml))
  (native-inputs
    (list ocaml-alcotest
          ocaml-bos
          ocaml-cppo
          ocaml-findlib
          ocaml-lwt
          ocaml-markup
          ocaml-ppx-expect
          ocaml-version
          ocaml-yojson
          jq))
    (home-page "https://github.com/ocaml/odoc")
    (synopsis "OCaml documentation generator")
    (description "Odoc is a documentation generator for OCaml.  It reads
@emph{doc comments}, delimited with @code{(** ... *)}, and outputs
@acronym{HTML}.

Text inside doc comments is marked up in ocamldoc syntax.  Odoc's main
advantage over ocamldoc is an accurate cross-referencer, which handles the
complexity of the OCaml module system.")
    (license license:isc)))

(define-public ocaml-odoc-parser
  (package
    (name "ocaml-odoc-parser")
    (version "2.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ocaml-doc/odoc-parser")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
           "1x48kf051xs98rd6cri591bk1ccp9hyp93n1rlf6qnxic55jw683"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-astring ocaml-camlp-streams ocaml-result))
    (native-inputs
      (list ocaml-ppx-expect))
    (home-page "https://github.com/ocaml-doc/odoc-parser")
    (synopsis "Parser for ocaml documentation comments")
    (description
     "This package provides a library for parsing the contents of OCaml
documentation comments, formatted using Odoc syntax, an extension of the
language understood by ocamldoc.")
    (license license:isc)))

(define-public ocaml-fftw3
  (package
    (name "ocaml-fftw3")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Chris00/fftw-ocaml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07ljbin9dsclsqh24p7haqjccz1w828sf5xfwlzl298d4a6zsbhs"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list fftw fftwf))
    (native-inputs
     (list ocaml-cppo ocaml-lacaml))
    (home-page
     "https://github.com/Chris00/fftw-ocaml")
    (synopsis
     "Bindings to FFTW3")
    (description
     "Bindings providing OCaml support for the seminal Fast Fourier Transform
library FFTW.")
    (license license:lgpl2.1))) ; with static linking exception.

(define-public ocaml-lacaml
  (package
    (name "ocaml-lacaml")
    (version "11.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mmottl/lacaml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "115535kphchh2a434b48b408x9794j8zzrsdmacsgqdsrgy3rck4"))
       (modules '((guix build utils)))
       (snippet '(substitute* '("src/dune" "src/config/dune")
                   (("-march=native") "")))))
    (properties '((tunable? . #t)))
    (build-system dune-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'find-openblas
                 (lambda* _
                   (setenv "LACAML_LIBS" "-lopenblas"))))
           #:tests? #f))                ; No test target.
    (native-inputs
     (list openblas ocaml-base ocaml-stdio))
    (home-page "https://mmottl.github.io/lacaml/")
    (synopsis
     "OCaml-bindings to BLAS and LAPACK")
    (description
     "Lacaml interfaces the BLAS-library (Basic Linear Algebra Subroutines) and
LAPACK-library (Linear Algebra routines).  It also contains many additional
convenience functions for vectors and matrices.")
    (license license:lgpl2.1)))

(define-public ocaml-cairo2
  (package
    (name "ocaml-cairo2")
    (version "0.6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Chris00/ocaml-cairo")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06ag9b88ihhr7yd3s9l0ac7ysig02fmlmsswybbsvz71ni0mb105"))))
    (build-system dune-build-system)
    (arguments
     (list #:package "cairo2"))
    (inputs
     `(("cairo" ,cairo)))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/Chris00/ocaml-cairo")
    (synopsis "Binding to Cairo, a 2D Vector Graphics Library")
    (description "Ocaml-cairo2 is a binding to Cairo, a 2D graphics library
with support for multiple output devices.  Currently supported output targets
include the X Window System, Quartz, Win32, image buffers, PostScript, PDF,
and SVG file output.")
    (license license:lgpl3+)))

(define-public ocaml-version
  (package
    (name "ocaml-version")
    (version "3.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocurrent/ocaml-version")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pnw2ym021j48zknhbi1kdiyfv9si8p2l04rdzbv4g51fclsqs92"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))          ; no tests
    (properties '((upstream-name . "ocaml-version")))
    (home-page
     "https://github.com/ocurrent/ocaml-version")
    (synopsis
     "Manipulate, parse and generate OCaml compiler version strings")
    (description
     "This library provides facilities to parse version numbers of the OCaml
compiler, and enumerates the various official OCaml releases and configuration
variants.")
    (license license:isc)))

(define-public ocaml-mdx
  (package
    (name "ocaml-mdx")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/realworldocaml/mdx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w1givvhwv9jzj9zbg4mmlpb35sqi75w83r99p2z50bdr69fdf57"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test-format
           (lambda _
             ;; cmdliner changed the format and the tests fail
             (substitute* '("test/bin/mdx-test/misc/no-such-file/test.expected"
                            "test/bin/mdx-test/misc/no-such-prelude/test.expected")
               (("`") "'")
               (("COMMAND") "[COMMAND]")
               (("\\.\\.\\.") "…"))))
         (add-after 'fix-test-format 'fix-egrep
           (lambda _
             ;; egrep is obsolescent; using grep -E
             (substitute* "test/bin/mdx-test/expect/padding/test-case.md"
               (("egrep") "grep -E")))))))
    (propagated-inputs
     (list ocaml-fmt
           ocaml-astring
           ocaml-logs
           ocaml-cmdliner
           ocaml-re
           ocaml-result
           ocaml-odoc
           ocaml-odoc-parser
           ocaml-version))
    (native-inputs
     (list ocaml-cppo ocaml-lwt ocaml-alcotest))
    (home-page
     "https://github.com/realworldocaml/mdx")
    (synopsis
     "Executable code blocks inside markdown files")
    (description
     "@code{ocaml-mdx} executes code blocks inside markdown files.
There are (currently) two sub-commands, corresponding
to two modes of operations: pre-processing (@code{ocaml-mdx pp})
and tests (@code{ocaml-mdx test}]).

The pre-processor mode allows mixing documentation and code,
and to practice @dfn{literate programming} using markdown and OCaml.

The test mode ensures that shell scripts and OCaml fragments
in the documentation always stays up-to-date.

@code{ocaml-mdx} is released as two binaries called @code{ocaml-mdx} and
@code{mdx} which are the same, mdx being the deprecated name, kept for now for
compatibility.")
    (license license:isc)))

(define-public ocaml-mparser
  (package
    (name "ocaml-mparser")
    (version "1.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/murmour/mparser")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "16j19v16r42gcsii6a337zrs5cxnf12ig0vaysxyr7sq5lplqhkx"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests.
     '(#:package "mparser"
       #:tests? #f))
    (home-page "https://github.com/murmour/mparser")
    (synopsis "Simple monadic parser combinator library")
    (description
      "This library implements a rather complete and efficient monadic parser
combinator library similar to the Parsec library for Haskell by Daan Leijen and
the FParsec library for FSharp by Stephan Tolksdorf.")
    ;; With static linking exception.
    (license license:lgpl2.1+)))

(define-public ocaml-mparser-re
  (package
    (inherit ocaml-mparser)
    (name "ocaml-mparser-re")
    (arguments
     ;; No tests.
     '(#:package "mparser-re"
       #:tests? #f))
    (propagated-inputs
     (list ocaml-mparser ocaml-re))
    (synopsis "MParser plugin for RE-based regular expressions")
    (description "This package provides RE-based regular expressions
support for Mparser.")))

(define-public ocaml-mparser-pcre
  (package
    (inherit ocaml-mparser)
    (name "ocaml-mparser-pcre")
    (arguments
     ;; No tests.
     '(#:package "mparser-pcre"
       #:tests? #f))
    (propagated-inputs
     (list ocaml-mparser ocaml-pcre))
    (synopsis "MParser plugin for PCRE-based regular expressions")
    (description "This package provides PCRE-based regular expressions
support for Mparser.")))

(define-public lablgtk3
  (package
    (name "lablgtk")
    (version "3.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/garrigue/lablgtk")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rhdr89w7yj8pkga5xc7iqmqvrs28034wb7sm7vx7faaxczwjifn"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "lablgtk3"))
    (propagated-inputs
     (list ocaml-cairo2 ocaml-camlp-streams))
    (inputs
     (list camlp5 gtk+))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/garrigue/lablgtk")
    (synopsis "OCaml interface to GTK+3")
    (description "LablGtk is an OCaml interface to GTK+ 1.2, 2.x and 3.x.  It
provides a strongly-typed object-oriented interface that is compatible with the
dynamic typing of GTK+.  Most widgets and methods are available.  LablGtk
also provides bindings to gdk-pixbuf, the GLArea widget (in combination with
LablGL), gnomecanvas, gnomeui, gtksourceview, gtkspell, libglade (and it can
generate OCaml code from .glade files), libpanel, librsvg and quartz.")
    ;; Version 2 only, with linking exception.
    (license license:lgpl2.0)))

(define-public ocaml-lablgtk3-sourceview3
  (package
    (inherit lablgtk3)
    (name "ocaml-lablgtk3-sourceview3")
    (propagated-inputs (list gtksourceview-3 lablgtk3))
    (arguments
     `(#:package "lablgtk3-sourceview3"))
    (synopsis "OCaml interface to GTK+ gtksourceview library")
    (description "This package provides the lablgtk interface to the
GTK+ gtksourceview library.")))

(define-public ocaml-reactivedata
  (package
    (name "ocaml-reactivedata")
    (version "0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocsigen/reactiveData")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gmpfnw08c7hx4bsgrgvp6w7pq2ghqxq3qd1cbdyscbg9n22jrca"))))
    (arguments
     `(#:tests? #f)) ;no tests
    (build-system dune-build-system)
    (properties `((upstream-name . "reactiveData")))
    (propagated-inputs
     (list ocaml-react))
    (home-page "https://github.com/ocsigen/reactiveData")
    (synopsis "Declarative events and signals for OCaml")
    (description
     "React is an OCaml module for functional reactive programming (FRP).  It
provides support to program with time varying values: declarative events and
 signals.  React doesn't define any primitive event or signal, it lets the
client chooses the concrete timeline.")
    (license license:lgpl2.1+)))

(define-public ocaml-uucd
  (package
    (name "ocaml-uucd")
    (version "15.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uucd/releases/"
                           "uucd-" version ".tbz"))
       (sha256
        (base32
         "1g26237yqmxr7sd1n9fg65qm5mxz66ybk7hr336zfyyzl25h6jqf"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:build-flags '("build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (propagated-inputs
     (list ocaml-xmlm))
    (native-inputs
     (list opam-installer ocaml-findlib ocamlbuild ocaml-topkg))
    (home-page "https://erratique.ch/software/uucd")
    (synopsis "Unicode character database decoder for OCaml")
    (description "Uucd is an OCaml module to decode the data of the Unicode
character database from its XML representation.  It provides high-level (but
not necessarily efficient) access to the data so that efficient
representations can be extracted.")
    (license license:isc)))

(define-public ocaml-uucp
  (package
    (name "ocaml-uucp")
    (version "15.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uucp/releases/"
                           "uucp-" version ".tbz"))
       (sha256
        (base32
         "0c2k9gkg442l7hnc8rn1vqzn6qh68w9fx7h3nj03n2x90ps98ixc"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:build-flags '("build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list opam-installer
           ocaml-findlib
           ocamlbuild
           ocaml-topkg
           ocaml-uucd
           ocaml-uunf
           ocaml-uutf))
    (home-page "https://erratique.ch/software/uucp")
    (synopsis "Unicode character properties for OCaml")
    (description "Uucp is an OCaml library providing efficient access to a
selection of character properties of the Unicode character database.")
    (license license:isc)))

(define-public ocaml-uuseg
  (package
    (name "ocaml-uuseg")
    (version "15.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uuseg/releases/"
                           "uuseg-" version ".tbz"))
       (sha256
        (base32
         "1qz130wlmnvb6j7kpvgjlqmdm2jqid4wb1dmrsls4hdm4rp7gk5b"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:build-flags '("build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (propagated-inputs
     (list ocaml-uucp ocaml-uutf ocaml-cmdliner))
    (native-inputs
     (list opam-installer ocaml-findlib ocamlbuild ocaml-topkg))
    (home-page "https://erratique.ch/software/uuseg")
    (synopsis "Unicode text segmentation for OCaml")
    (description "Uuseg is an OCaml library for segmenting Unicode text.  It
implements the locale independent Unicode text segmentation algorithms to
detect grapheme cluster, word and sentence boundaries and the Unicode line
breaking algorithm to detect line break opportunities.

The library is independent from any IO mechanism or Unicode text data
structure and it can process text without a complete in-memory
representation.")
    (license license:isc)))

(define-public ocaml-fix
  (package
    (name "ocaml-fix")
    (version "20220121")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://gitlab.inria.fr/fpottier/fix")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "15785v43jcbqsw1y653cnb89alrcnbdri1h0w6zl6p7769ja9rdj"))))
    (build-system dune-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (home-page "https://gitlab.inria.fr/fpottier/fix")
    (synopsis "Facilities for memoization and fixed points")
    (description "This package provides helpers with various constructions
that involve memoization and recursion.")
    (license license:lgpl2.0)))

(define-public ocaml-dune-build-info
  (package
    (inherit dune-ordering)
    (name "ocaml-dune-build-info")
    (build-system dune-build-system)
    (arguments
     '(#:package "dune-build-info"
       ;; No separate test suite from dune.
       #:tests? #f))
    (propagated-inputs
     (list ocaml-odoc))
    (synopsis "Embed build information inside an executable")
    (description "This package allows one to access information about how the
executable was built, such as the version of the project at which it was built
or the list of statically linked libraries with their versions.  It supports
reporting the version from the version control system during development to
get an precise reference of when the executable was built.")))

(define-public ocaml-xdg
  (package
    (inherit dune-ordering)
    (name "ocaml-xdg")
    (build-system dune-build-system)
    (arguments
     '(#:package "xdg"
       ;; Tests have a cyclic dependency on stdune
       #:tests? #f))
    (propagated-inputs (list ocaml-odoc))
    (synopsis "XDG Base Directory Specification library for ocaml")
    (description
     "This ocaml library returns user XDG directories such as XDG_CONFIG_HOME,
     XDG_STATE_HOME.")))

(define-public dune-rpc
  (package
    (inherit dune-ordering)
    (name "dune-rpc")
    (build-system dune-build-system)
    (arguments
     '(#:package "dune-rpc"
       ;; Tests have a cyclic dependency on stdune
       #:tests? #f))
    (propagated-inputs (list ocaml-csexp
                             dune-ordering
                             dune-dyn
                             ocaml-xdg
                             dune-stdune
                             ocaml-pp
                             ocaml-odoc))
    (synopsis "Communicate with ocaml dune using rpc")
    (description "Library to connect and control a running dune instance.")))

(define-public ocamlc-loc
  (package
    (inherit dune-ordering)
    (name "ocamlc-loc")
    (build-system dune-build-system)
    (arguments
     '(#:package "ocamlc-loc"
       ;; Tests have a cyclic dependency on stdune
       #:tests? #f))
    (propagated-inputs (list dune-dyn ocaml-odoc))
    (synopsis "Parse ocaml compiler output into structured form")
    (description
     "This library parses ocaml compiler output and returns it as ocaml values.
This library offers no backwards compatibility guarantees.")))

(define-public ocaml-chrome-trace
  (package
    (inherit dune-ordering)
    (name "ocaml-chrome-trace")
    (build-system dune-build-system)
    (arguments
     '(#:package "chrome-trace"
       ;; Tests have a cyclic dependency on stdune
       #:tests? #f))
    (propagated-inputs (list ocaml-odoc))
    (synopsis "Chrome trace event generation library for ocaml")
    (description
     "Output trace data to a file in Chrome's trace_event format. This format is
    compatible with chrome trace viewer chrome://tracing.
    This library offers no backwards compatibility guarantees.")
    (license license:expat)))

(define-public ocaml-fiber
  (package
    (name "ocaml-fiber")
    (home-page "https://github.com/ocaml-dune/fiber")
    (version "3.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "085v1dfxrb4wnkgysghj5q4vr4nx3nxr84rqmy874dr3pk30740n"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "fiber"))
    (propagated-inputs (list dune-stdune dune-dyn))
    (native-inputs (list ocaml-odoc ocaml-ppx-expect))
    (synopsis "Structured concurrency library")
    (description
     "This library implements structured concurrency for ocaml.
     It offers no backwards compatibility guarantees.")
    (license license:expat)))

(define-public ocaml-either
  (package
    (name "ocaml-either")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mirage/either")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32 "099p1m24vz5i0043zcfp88krzjsa2qbrphrm4bnx84gif5vgkxwm"))))
    (build-system dune-build-system)
    (arguments
     ;; no tests
     `(#:tests? #f))
    (home-page "https://github.com/mirage/either")
    (synopsis "Compatibility Either module")
    (description "This library is a compatibility module for the Either module
defined in OCaml 4.12.0.")
    (license license:expat)))

(define-public ocamlformat
  (package
    (name "ocamlformat")
    (version "0.24.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ocaml-ppx/ocamlformat")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0y1j5mwwrliy6a78cmpi6j8gw425shghqg9ylyl3qw5fx4b088pp"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "ocamlformat"
       #:phases
       (modify-phases %standard-phases
         ;; Tests related to other packages
         (add-after 'unpack 'remove-unrelated-tests
           (lambda _
             (delete-file-recursively "test/rpc")))
         (add-after 'unpack 'fix-test-format
           (lambda _
             (substitute* "test/cli/repl_file_errors.t/run.t"
               ((" ;;") ";;")))))))
    (propagated-inputs
      (list ocaml-version
            ocaml-base
            ocaml-cmdliner
            ocaml-dune-build-info
            ocaml-either
            ocaml-fix
            ocaml-fpath
            ocaml-menhir
            ocaml-odoc
            ocaml-ppxlib
            ocaml-re
            ocaml-odoc-parser
            ocaml-stdio
            ocaml-uuseg
            ocaml-uutf))
    (native-inputs
      (list git-minimal/pinned                     ;for tests
            ocaml-alcotest ocaml-ocp-indent ocaml-bisect-ppx))
    (home-page "https://github.com/ocaml-ppx/ocamlformat")
    (synopsis "Auto-formatter for OCaml code")
    (description "OCamlFormat is a tool to automatically format OCaml code in
a uniform style.")
    (license license:expat)))

(define-public ocamlformat-rpc-lib
  (package
    (inherit ocamlformat)
    (name "ocamlformat-rpc-lib")
    (arguments
     '(#:package "ocamlformat-rpc-lib"))
    (propagated-inputs (list ocaml-csexp ocaml-odoc))
    (synopsis "Auto-formatter for OCaml code in RPC mode")
    (description
     "OCamlFormat is a tool to automatically format OCaml code in a uniform style.
This package defines a RPC interface to OCamlFormat.")))

(define-public ocaml-bigstringaf
  (package
    (name "ocaml-bigstringaf")
    (version "0.9.0")
    (home-page "https://github.com/inhabitedtype/bigstringaf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "188j9awxg99vrp2l3rqfmdxdazq5xrjmg1wf62vfqsks9sff6wqx"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-bigarray-compat))
    (native-inputs
     (list ocaml-alcotest pkg-config))
    (synopsis
     "Bigstring intrinsics and fast blits based on memcpy/memmove")
    (description
     "The OCaml compiler has a bunch of intrinsics for Bigstrings, but they're
not widely-known, sometimes misused, and so programs that use Bigstrings are
slower than they have to be.  And even if a library got that part right and
exposed the intrinsics properly, the compiler doesn't have any fast blits
between Bigstrings and other string-like types.  @code{bigstringaf} provides
these missing pieces.")
    (license license:bsd-3)))

(define-public ocaml-intrinsics
  (package
    (name "ocaml-intrinsics")
    (version "0.15.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/janestreet/ocaml_intrinsics")
                    (commit (string-append "v" version))))
              (file-name name)
              (sha256
               (base32
                "1mazr1ka2zlm2s8bw5i555cnhi1bmr9yxvpn29d3v4m8lsnfm73z"))))
    (build-system dune-build-system)
    ;; TODO figure out how to get around this error:
    ;; No rule found for alias test/runtime-deps-of-tests
    (arguments
     '(#:tests? #f))
    (propagated-inputs (list dune-configurator))
    (native-inputs (list ocaml-expect-test-helpers-core ocaml-core))
    (properties `((upstream-name . "ocaml_intrinsics")))
    (home-page "https://github.com/janestreet/ocaml_intrinsics")
    (synopsis "AMD64 intrinsics with emulated fallbacks")
    (description
     "Provides an OCaml interface to operations that have dedicated hardware
instructions on some micro-architectures, with default implementations using C
stubs for all targets.")
    (license license:expat)))

(define-public ocaml-trie
  (package
    (name "ocaml-trie")
    (version "1.0.0")
    (home-page "https://github.com/kandu/trie/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s7p9swjqjsqddylmgid6cv263ggq7pmb734z4k84yfcrgb6kg4g"))))
    (build-system dune-build-system)
    (arguments
     '(#:tests? #f))                    ;no tests
    (synopsis "Strict impure trie tree")
    (description
     "This module implements strict impure trie tree data structure for
OCaml.")
    (license license:expat)))

(define-public ocaml-mew
  (package
    (name "ocaml-mew")
    (version "0.1.0")
    (home-page "https://github.com/kandu/mew")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0417xsghj92v3xa5q4dk4nzf2r4mylrx2fd18i7cg3nzja65nia2"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-result ocaml-trie))
    (native-inputs
     (list ocaml-ppx-expect))
    (synopsis "General modal editing engine generator")
    (description
     "This package provides the core modules of Modal Editing Witch, a general
modal editing engine generator.")
    (license license:expat)))

(define-public ocaml-mew-vi
  (package
    (name "ocaml-mew-vi")
    (version "0.5.0")
    (home-page "https://github.com/kandu/mew_vi")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
        (sha256
          (base32 "0lihbf822k5zasl60w5mhwmdkljlq49c9saayrws7g4qc1j353r8"))))
    (build-system dune-build-system)
    (propagated-inputs
      (list ocaml-mew ocaml-react))
    (native-inputs
     (list ocaml-ppx-expect))
    (properties `((upstream-name . "mew_vi")))
    (synopsis "Modal editing VI-like editing engine generator")
    (description "This module provides a vi-like modal editing engine
generator.")
    (license license:expat)))

(define-public ocaml-syntax-shims
  (package
    (name "ocaml-syntax-shims")
    (version "1.0.0")
    (home-page "https://github.com/ocaml-ppx/ocaml-syntax-shims")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l1i8z95qgb0lxlrv3yb5nkp391hqsiyi4r91p12k3xmggqixagf"))))
    (build-system dune-build-system)
    (properties
     `((upstream-name . "ocaml-syntax-shims")))
    (synopsis
     "Backport new syntax to older OCaml versions")
    (description
     "This package backports new language features such as @code{let+} to older
OCaml compilers.")
    (license license:expat)))

(define-public ocaml-angstrom
  (package
    (name "ocaml-angstrom")
    (version "0.15.0")
    (home-page "https://github.com/inhabitedtype/angstrom")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hmrkdcdlkwy7rxhngf3cv3sa61cznnd9p5lmqhx20664gx2ibrh"))))
    (build-system dune-build-system)
    (arguments
     ;; Only build the base angstrom package.
     '(#:package "angstrom"))
    (propagated-inputs
     (list ocaml-bigstringaf))
    (native-inputs
     (list ocaml-alcotest ocaml-ppx-let ocaml-syntax-shims))
    (synopsis "Parser combinators built for speed and memory-efficiency")
    (description
     "Angstrom is a parser-combinator library that makes it easy to write
efficient, expressive, and reusable parsers suitable for high-performance
applications.  It exposes monadic and applicative interfaces for composition,
and supports incremental input through buffered and unbuffered interfaces.
Both interfaces give the user total control over the blocking behavior of
their application, with the unbuffered interface enabling zero-copy IO.
Parsers are backtracking by default and support unbounded lookahead.")
    (license license:bsd-3)))

(define-public ocaml-graphics
  (package
    (name "ocaml-graphics")
    (version "5.1.2")
    (home-page "https://github.com/ocaml/graphics")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1q20f8y6ijxbvzik2ns4yl3w54q5z8kd0pby8i8c64a04hvly08m"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list libx11))
    (synopsis "The OCaml graphics library")
    (description
     "The graphics library provides a set of portable drawing primitives.
Drawing takes place in a separate window that is created when
Graphics.open_graph is called.  This library used to be distributed with OCaml
up to OCaml 4.08.")
    (license license:lgpl2.1+)))

(define-public ocaml-uri-sexp
  (package
    (inherit ocaml-uri)
    (name "ocaml-uri-sexp")
    (arguments
     '(#:package "uri-sexp"))
    (propagated-inputs
      (list ocaml-uri ocaml-ppx-sexp-conv ocaml-sexplib0))
    (native-inputs (list ocaml-ounit))
    (synopsis "RFC3986 URI/URL parsing library")
    (description "This package adds S-exp support to @code{ocaml-uri}.")))

(define-public ocaml-cohttp
  (package
    (name "ocaml-cohttp")
    (version "5.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/mirage/ocaml-cohttp")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "074xis3wmr76gadh1ffmfzjfx13mw4kr2s6rkwqwzcl6l85n9x2z"))))
    (build-system dune-build-system)
    (arguments '(#:package "cohttp"))
    (propagated-inputs
      (list ocaml-re
            ocaml-uri
            ocaml-uri-sexp
            ocaml-sexplib0
            ocaml-ppx-sexp-conv
            ocaml-stringext
            ocaml-base64))
    (native-inputs
     (list ocaml-fmt
           ocaml-jsonm
           ocaml-alcotest
           ocaml-crowbar))
    (home-page "https://github.com/mirage/ocaml-cohttp")
    (synopsis "OCaml library for HTTP clients and servers")
    (description
      "Cohttp is an OCaml library for creating HTTP daemons.  It has a portable
HTTP parser, and implementations using various asynchronous programming
libraries.")
    (license license:isc)))

(define-public js-of-ocaml
  (package
    (name "js-of-ocaml")
    (version "4.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocsigen/js_of_ocaml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14ig69iyc9yzniclfsc6cz9g9zqp96bs66y6dy4rzrm78s81w6i1"))))
    (build-system dune-build-system)
    (arguments
     ;;tests assume ocaml 4.13
     `(#:tests? #f))
    (propagated-inputs
     (list ocaml-ppxlib
           ocaml-uchar
           ocaml-menhir
           ocaml-reactivedata
           ocaml-cmdliner
           ocaml-lwt
           ocaml-tyxml
           ocaml-re
           ocaml-uutf
           ocaml-graphics
           ocaml-yojson))
    (native-inputs
     ;; for tests
     (list node-lts ocaml-ppx-expect ocaml-num))
    (properties `((upstream-name . "js_of_ocaml")))
    (home-page "https://ocsigen.org/js_of_ocaml/")
    (synopsis "Compiler from OCaml bytecode to Javascript")
    (description "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript.
It makes it possible to run pure OCaml programs in JavaScript environment like
browsers and Node.js.")
    (license license:lgpl2.1+)))

(define-public ocaml-afl-persistent
  (package
    (name "ocaml-afl-persistent")
    (version "1.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/stedolan/ocaml-afl-persistent")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
           "06yyds2vcwlfr2nd3gvyrazlijjcrd1abnvkfpkaadgwdw3qam1i"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "./build.sh")))
         ;; XXX: The tests are already run in the build.sh script.
         (delete 'check))))
    (native-inputs (list opam-installer))
    (home-page "https://github.com/stedolan/ocaml-afl-persistent")
    (synopsis "Use afl-fuzz in persistent mode")
    (description
      "afl-fuzz normally works by repeatedly forking the program being tested.
Using this package, you can run afl-fuzz in ``persistent mode'', which avoids
repeated forking and is much faster.")
    (license license:expat)))

(define-public ocaml-monolith
  (package
    (name "ocaml-monolith")
    (version "20210525")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.inria.fr/fpottier/monolith")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1b6jj4ivl9ni8kba7wls4xsqdy8nm7q9mnx9347jvb99dmmlj5mc"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-afl-persistent ocaml-pprint ocaml-seq))
    (home-page "https://gitlab.inria.fr/fpottier/monolith")
    (synopsis "Framework for testing an OCaml library using afl-fuzz")
    (description "Monolith offers facilities for testing an OCaml library (for
instance, a data structure implementation) by comparing it against a reference
implementation.  It can be used to perform either random testing or fuzz
testing by using the @code{afl-fuzz} tool.")
    (license license:lgpl3+)))

(define-public ocaml-pprint
  (package
    (name "ocaml-pprint")
    (version "20220103")
    (home-page "https://github.com/fpottier/pprint")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09y6nwnjldifm47406q1r9987njlk77g4ifqg6qs54dckhr64vax"))))
    (build-system dune-build-system)
    (synopsis "OCaml pretty-printing combinator library and rendering
engine")
    (description "This OCaml library offers a set of combinators for building
so-called documents as well as an efficient engine for converting documents to
a textual, fixed-width format.  The engine takes care of indentation and line
breaks, while respecting the constraints imposed by the structure of the
document and by the text width.")
    (license license:lgpl2.0)))

(define-public ocaml-crowbar
  (package
    (name "ocaml-crowbar")
    (version "0.2.1")
    (home-page "https://github.com/stedolan/crowbar")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11f3kiw58g8njx15akx16xcplzvzdw9y6c4jpyfxylkxws4g0f6j"))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-ocplib-endian
           ocaml-cmdliner
           ocaml-afl-persistent))
    (native-inputs
     (list ocaml-calendar
           ocaml-fpath
           ocaml-uucp
           ocaml-uunf
           ocaml-uutf
           ocaml-pprint))
    (properties `((ocaml5.0-variant . ,(delay ocaml5.0-crowbar))))
    (synopsis "Ocaml library for tests, let a fuzzer find failing cases")
    (description "Crowbar is a library for testing code, combining
QuickCheck-style property-based testing and the magical bug-finding powers of
@uref{http://lcamtuf.coredump.cx/afl/, afl-fuzz}.")
    (license license:expat)))

(define-public ocaml5.0-crowbar
  (package-with-ocaml5.0
   (package
     (inherit ocaml-crowbar)
     ;; Tests require ocaml-calendar which does not work with OCaml 5.0
     (arguments `(#:tests? #f))
     (properties '()))))

(define-public ocaml-eqaf
  (package
    (name "ocaml-eqaf")
    (version "0.9")
    (home-page "https://github.com/mirage/eqaf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16ics56wiqyng70dy2hqikicm8ag1mv5w1h7hkiwvydw1x2j2rsl"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-cstruct))
    (native-inputs (list ocaml-alcotest ocaml-crowbar))
    (synopsis "OCaml library for constant-time equal function on string")
    (description "This OCaml library provides an equal function on string in
constant-time to avoid timing-attack with crypto stuff.")
    (license license:expat)))

(define-public ocaml-digestif
  (package
    (name "ocaml-digestif")
    (version "1.1.3")
    (home-page "https://github.com/mirage/digestif")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0x5iskavqkclr5mk2q6jvh5h1v81krqi4v353rj4xsmdqb33s0f1"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-eqaf))
    (native-inputs
     (list pkg-config
           ocaml-fmt
           ocaml-alcotest
           ocaml-bos
           ocaml-astring
           ocaml-fpath
           ocaml-rresult
           ocaml-findlib))
    (synopsis "Simple hash algorithms in OCaml")
    (description
     "Digestif is an OCaml library that provides implementations of hash
algorithms.  Implemented hash algorithms include MD5, SHA1, SHA224, SHA256,
SHA384, SHA512, Blake2b, Blake2s and RIPEMD160.")
    (license license:expat)))

(define-public ocaml-bibtex2html
  (package
    (name "ocaml-bibtex2html")
    (version "1.99")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.lri.fr/~filliatr/ftp/bibtex2html/"
                           "bibtex2html-"  version ".tar.gz"))
       (sha256
        (base32
         "07gzrs4lfrkvbn48cgn2gn6c7cx3jsanakkrb2irj0gmjzfxl96j"))))
    (build-system ocaml-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-/bin/sh
            (lambda _
              (substitute* "configure" (("/bin/sh") (which "bash")))
              ;; mktexfmt needs writable home directory.
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list (texlive-local-tree
            (list texlive-infwarerr
                  texlive-kvoptions
                  texlive-pdftexcmds
                  texlive-preprint))
           which))
    (propagated-inputs
     (list hevea))
    (home-page "https://www.lri.fr/~filliatr/bibtex2html/")
    (synopsis "BibTeX to HTML translator")
    (description "This package allows you to produce, from a set of
bibliography files in BibTeX format, a bibliography in HTML format.")
    (license license:gpl2)))

(define-public ocaml-guile
  (package
    (name "ocaml-guile")
    (version "1.0")
    (home-page "https://github.com/gopiandcode/guile-ocaml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0yxdkrhrrbwvay5sn0p26rh3f11876k6kdharmpi4afxknml74ql"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (propagated-inputs
     (list ocaml-sexplib
           ocaml-ctypes
           ocaml-stdio
           ocaml-odoc))
    (inputs (list guile-3.0 libffi))
    (native-inputs
     (list ocaml-odoc
           pkg-config))
    (synopsis "Bindings to GNU Guile Scheme for OCaml")
    (description
     "The OCaml guile library provides high-level OCaml bindings to GNU Guile
3.0, supporting easy interop between OCaml and GNU Guile Scheme.")
    (license license:gpl3+)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
