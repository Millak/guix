;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
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

(define-module (gnu packages gawk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages multiprecision)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu))

(define-public gawk
  (package
   (name "gawk")
   (version "5.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gawk/gawk-" version
                                ".tar.xz"))
            (sha256
             (base32 "1gc2cccqy1x1bf6rhwlmd8q7dz7gnam6nwgl38bxapv6qm5flpyg"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (add-before 'configure 'set-shell-file-name
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Refer to the right shell.
                     (let ((bash (assoc-ref inputs "bash")))
                       (substitute* "io.c"
                         (("/bin/sh")
                          (string-append bash "/bin/sh")))

                       ;; When cross-compiling, remove dependencies on the
                       ;; `check-for-shared-lib-support' target, which tries
                       ;; to run the cross-built `gawk'.
                       ,@(if (%current-target-system)
                             '((substitute* "extension/Makefile.in"
                                 (("^.*: check-for-shared-lib-support" match)
                                  (string-append "### " match))))
                             '()))))

                 (add-before 'check 'adjust-test-infrastructure
                   (lambda _
                     ;; Remove dependency on 'more' (from util-linux), which
                     ;; would needlessly complicate bootstrapping.
                     (substitute* "test/Makefile"
                       (("\\| more") ""))

                     ;; Silence a warning from bash about not being able
                     ;; to change to an ISO-8859-1 locale.  The test itself
                     ;; works fine, but newer versions of bash give a
                     ;; locale warning which mangles the test output.
                     (substitute* "test/localenl.sh"
                       (("for LC_ALL in")
                        "for LC in")
                       (("export LC_ALL\n")
                        "export LC_ALL=$LC 2>/dev/null\n"))

                     ;; Adjust the shebang in that file since it is then diff'd
                     ;; against the actual test output.
                     (substitute* "test/watchpoint1.ok"
                       (("#! /usr/bin/gawk")
                        (string-append "#!" (which "gawk")))))))))

   (inputs (list libsigsegv
                 ;; Use the full-fledged Bash package, otherwise the test suite
                 ;; sometimes fail non-deterministically.
                 bash))

   (home-page "https://www.gnu.org/software/gawk/")
   (synopsis "Text scanning and processing language")
   (description
    "Gawk is the GNU implementation of Awk, a specialized programming
language for the easy manipulation of formatted text, such as tables of data.
Gawk features many extensions beyond the traditional implementation,
including network access, sorting, and large libraries.")
   (license license:gpl3+)))

;; Separate from gawk to facilitate bootstrapping.
(define-public gawk-mpfr
  (package/inherit gawk
    (name "gawk-mpfr")
    (inputs
     (modify-inputs (package-inputs gawk)
       (prepend mpfr)))))

;; Suffixed with -next because, similarly to Emacs, development versions are
;; numbered x.y.60+z, and also there are no tagged versions of egawk yet.
;; (However, though egawk's --version lists 5.1.60, it is actually forked from
;; a development version of gawk 5.1.1.)
(define-public egawk-next
  (let ((commit "f00e74ffc73f6ba6fe74fb7a26319770b8c3792c")
        (revision "0"))
    (package
      (inherit gawk-mpfr)
      (name "egawk-next")
      (version (git-version "5.1.60" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://www.kylheku.com/git/egawk")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bmfbw6k1aiyiardnk7ha5zlpkvavj013mm4n7wwj2vdcgrs6p1f"))))
      (home-page "https://www.kylheku.com/cgit/egawk/")
      (synopsis "Enhanced GNU Awk")
      (description
       "@command{egawk} is Enhanced GNU Awk.  It is a fork of GNU Awk with
some enhancements designed and implemented by Kaz Kylheku.  In particular,
Enhanced GNU Awk provides the @code{@@let} statement for declaring
block-scoped lexical variables."))))

(define-public mawk
  (package
    (name "mawk")
    (version "1.3.4-20200120")
    (home-page "https://invisible-island.net/mawk/mawk.html")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://invisible-mirror.net/archives/mawk"
                                  "/mawk-" version ".tgz"))
              (sha256
               (base32
                "0dw2icf8bnqd9y0clfd9pkcxz4b2phdihwci13z914mf3wgcvm3z"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Prevent tests from hard coding PATH to a bogus value.
                  (substitute* '("test/mawktest" "test/fpe_test")
                    (("^PATH=.*")
                     ""))))))
    (build-system gnu-build-system)
    (synopsis "Text scanning and processing language")
    (description
     "@command{mawk} is an interpreter for the Awk programming language.
This version aims to be smaller and faster than GNU Awk, at the expense
of fewer features and extensions.")
    (license license:gpl2))) ;version 2 only

(define-public cppawk
  (package
    (name "cppawk")
    (version "20220703")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://www.kylheku.com/git/cppawk")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b09757q81sz4gn62k3mv5bgllyb2v5m64346s8fc99mqqif70cx"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("bin/cppawk" "bin/cppawk")
                        ("share/cppawk/include" "share/cppawk/include")
                        ("./" "share/man/man1" #:include-regexp (".*\\.1$")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda _
             (substitute* "bin/cppawk"
               (("/bin/bash") (which "bash"))
               (("dirname") (which "dirname"))
               (("mktemp") (which "mktemp"))
               ;; Extra space to prevent matching Awk's printf.
               (("printf ") (string-append (which "printf") " "))
               (("rm -f") (string-append (which "rm") " -f"))
               (("prepro=cpp") (string-append "prepro=" (which "cpp")))
               (("sed -e") (string-append (which "sed") " -e")))))
         (add-after 'fix-paths 'fix-awk-paths
           (lambda _
             (substitute* "bin/cppawk"
               (("awk=gawk") (string-append "awk=" (which "gawk")))
               (("awk '") (string-append (which "gawk") " '")))))
         (add-after 'build 'check
           (lambda _
             (invoke "./runtests"))))))
    (native-inputs
     ;; For tests
     (list mawk))
    (inputs
     (list coreutils                    ; For dirname, mktemp, printf, rm
           gawk-mpfr                    ; Default variant, but supports others
           gcc                          ; For cpp
           sed))
    (home-page "https://www.kylheku.com/cgit/cppawk/")
    (synopsis "Wrapper script that adds C preprocessing to Awk")
    (description
     "@command{cppawk} is a shell script that invokes the C preprocessor
(@command{cpp}) on Awk code and calls Awk (by default GNU Awk) on the result.

@command{cppawk} understands the basic Awk options like @option{-F} and
@option{-v}, and also understands common @command{cpp} options like
@option{-I} and @option{-Dmacro=value}.

@command{cppawk} has no dependencies beyond Awk, @command{cpp}, @command{sed}
and some GNU core utilities (including @command{printf}).  Preprocessed
programs can be captured and transferred to systems that have Awk but not
@command{cpp} or @command{cppawk}.")
    (license license:bsd-2)))

(define-public cppawk-egawk
  (package/inherit cppawk
    (name "cppawk-egawk")
    (arguments
     (substitute-keyword-arguments (package-arguments cppawk)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'fix-awk-paths
             (lambda _
               (substitute* "bin/cppawk"
                 (("awk=gawk") (string-append "awk=" (which "egawk")))
                 (("awk '") (string-append (which "egawk") " '")))))))))
    (inputs
     (modify-inputs (package-inputs cppawk)
       (delete "gawk-mpfr")
       (prepend egawk-next)))
    (synopsis "cppawk that calls Enhanced GNU Awk by default")))
