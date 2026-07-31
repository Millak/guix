;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2019-2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Hendursaga <hendursaga@yahoo.com>
;;; Copyright © 2020 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2020 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages esolangs)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compiler-tools)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tcl)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public cfunge
  (package
    (name "cfunge")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/VorpalBlade/cfunge")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18ir0h10vxdb5jb57w5hjbgi8spjxg9x2148agadhhmbhsja02m7"))))
    (build-system cmake-build-system)
    (arguments
      ;; The tests are not designed to be run and evaluated automatically.
     '(#:tests? #f))
    (inputs
     (list ncurses))
    (home-page "https://github.com/VorpalBlade/cfunge")
    (synopsis "Fast conforming Befunge93/98/109 interpreter in C")
    (description "@command{cfunge} is a fast conforming Befunge93/98/109 interpreter
written in C.  It supports several @dfn{fingerprints} (opt-in language extensions
identified by unique ID codes).")
    (license license:gpl3)))

(define-public lolcode-lci
  ;; Use the latest commit as the last release is from 2014 with Python 2.
  (let ((commit "6762b724361a4fb471345961b4750657783aeb3b")
        (revision "0"))
    (package
      (name "lolcode-lci")
      (version (git-version "0.11.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/justinmeza/lci")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0phqnqp7qvkn0kxkk5qsc76b9gxs932w4dy3jm96pmknh1q7h6kk"))))
      (build-system cmake-build-system)
      ;; The test suite is currently failing with Python 3 (see:
      ;; https://github.com/justinmeza/lci/issues/75).
      (arguments (list #:tests? #f))
      (inputs
       (list readline))
      (native-inputs
       (list python-wrapper))           ; for the tests
      (synopsis "LOLCODE interpreter written in C")
      (description
       "@code{lci} is a LOLCODE interpreter written in C and is designed to be
correct, portable, fast, and precisely documented.
@enumerate
@item correct: Every effort has been made to test lci's conformance to the
LOLCODE language specification.  Unit tests come packaged with the lci source code.
@item portable: lci follows the widely ported ANSI C specification allowing it
to compile on a broad range of systems.
@item fast: Much effort has gone into producing simple and efficient code
whenever possible to the extent that the above points are not compromised.
@end enumerate")
      (home-page "http://lolcode.org/")
      (license license:gpl3+))))

(define-public folders
  (package
    (name "folders")
    (version "0.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SinaKhalili/Folders.py")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14fs8c7ilvsw6xbskr688s1dp3nd8vnwv7bg23ab1l6vj6fpzwmw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (invoke "Folders" "sample_programs/HelloWorld")
                  (format #t "test suite not run~%")))))))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/SinaKhalili/Folders.py")
    (synopsis "Structural programming language")
    (description
     "Folders is a programming language, in which programs are encoded as
(nested) directories.  Note that the switches you pass to @command{du} may
affect your score when code golfing.")
    (properties `((lint-hidden-cpe-vendors "premio" "jenkins")))
    (license license:expat)))

(define-public npiet
  (package
    (name "npiet")
    (version "1.3f")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.bertnase.de/npiet/npiet-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0nl59fhdqqr7nslxdirdn8nvlq5wws67c7jyx2ckbmxbc9h8bv9d"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-binaries
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (wrap-program (string-append out
                                                  "/bin/npietedit")
                       `("PATH" ":" prefix
                         (,(dirname
                            (search-input-file
                             inputs "bin/wish")))))))))))
    (inputs (list bash-minimal gd giflib libpng tk))
    (native-inputs (list groff))
    (synopsis "Piet interpreter")
    (description
     "Npiet is an interpreter for the Piet programming language.  Instead of
text, Piet programs are pictures.  Commands are determined based on changes in
color.

This package includes:
@enumerate
@item @command{npiet}, a Piet interpreter with debugging capabilities
@item @command{npiet-foogol}, a program that builds a Piet program from Foogol,
an Algol-like language
@item @command{npietedit}, an editor for Piet programs.
@end enumerate\n")
    (home-page "https://www.bertnase.de/npiet/")
    (license license:gpl2+)))

(define-public piet-toolchain
  (let ((commit "f002ff6a924a6bbace5eef94f3be06f425e7f590")
        (revision "0"))
    (package
      (name "piet-toolchain")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sl236/Piet")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xwbhwizfbn080fmrgavaz3b939brycmlar3m5px9avl2b68c816"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Remove a bundled fork of Marc Majcher's Piet interpreter.
             (delete-file-recursively "interpreter")))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (delete 'build)              ; nothing to build
           (delete 'check)              ; run our own tests below
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (doc (string-append out "/share/doc/"
                                          ,name "-" ,version)))
                 (for-each (lambda (script)
                             (install-file script bin)
                             (wrap-program (string-append bin "/" script)
                               `("PERL5LIB" ":" = (,(getenv "PERL5LIB")))))
                           (list "piet-assembler"
                                 "piet-compiler"))

                 ;; Fix an odd mode.
                 (chmod "compiler-samples/test-binary-ops.script" #o644)
                 (for-each (lambda (file)    ; INSTALL-FILE is not recursive
                             (copy-recursively file
                                               (string-append doc "/" file)))
                           (list "assembler-samples"
                                 "compiler-samples"
                                 "README.md"))))) ;includes the licence grant
           (add-after 'install 'check
             (lambda* (#:key outputs tests? #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (when tests?
                   (unsetenv "PERL5LIB") ; test the wrapping
                   ;; Compile all scripts assemble all Piets.
                   (for-each (lambda (file)
                               (system (string-append bin "/piet-compiler "
                                                      file ">"
                                                      file ".piet")))
                             (find-files "." "\\.script$"))
                   (for-each (lambda (file)
                               (system (string-append bin "/piet-assembler "
                                                      file "|pnmtopng>"
                                                      file ".png")))
                             (find-files "." "\\.piet$"))

                   ;; Don't run the interactive one.
                   (delete-file "assembler-samples/quest.piet.png")
                   (for-each (cut invoke "npiet" <>)
                             (find-files "." "\\.png$")))))))))
      (native-inputs (list netpbm npiet)) ;for tests
      (inputs (list bash-minimal perl perl-parse-recdescent))
      (home-page "https://www.toothycat.net/wiki/wiki.pl?MoonShadow/Piet")
      (synopsis "Piet compiler and assembler")
      (description
       "This package provides a compiler and assembler that target the Piet
graphical programming language.

@command{piet-assembler} converts Piet assembler instructions (e.g.,
@code{push}, @code{add}, @code{switch}, @code{outn}) and directives into an
executable @code{netpbm} image of the corresponding Piet program.

@command{piet-compiler} compiles a C-like high-level language into assembly
source understood by @command{piet-assembler}.  It supports common arithmetic
and boolean logic operators (though not bitwise manipulation), flow control
(@code{if}, @code{for}, @code{while}), recursive functions, in-line assembler,
and input/output intrinsics.  The only supported data type is the integer.

The language is documented only by the compiler's Perl source code and the
included samples.")
      (license license:cc-by-sa4.0))))

;;; esolangs.scm ends here
