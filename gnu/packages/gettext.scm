;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2019, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Miguel <rosen644835@gmail.com>
;;; Copyright © 2020, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 EuAndreh <eu@euandre.org>
;;; Copyright © 2022, 2024, 2025 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023 Maxim Cournoyer maxim.cournoyer@gmail.com>
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

(define-module (gnu packages gettext)
  #:use-module ((guix licenses) #:select (gpl2+ gpl3+ bsd-3))
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (guix utils))

(define-public gettext-minimal
  (package
    (name "gettext-minimal")
    (version "0.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gettext/gettext-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "19h717qnmg01499gi3494z9l2ss12cp60405ml473p82580dfpcl"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;9 MiB of HTML
    (inputs
     (list libunistring
           libxml2
           ;; TODO: ncurses is only needed for the 'libtextstyle' library.
           ;; The next version of gettext can use a separate libtextstyle,
           ;; but for now we include it here in 'gettext-minimal'.
           ncurses))
    (arguments
     (list #:configure-flags #~'("--with-included-libunistring=no"
                                 "--with-included-libxml=no")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'patch-source-shebangs 'patch-fixed-paths
                 (lambda _
                   (substitute* '("gettext-tools/config.h.in"
                                  "gettext-tools/gnulib-tests/init.sh"
                                  "gettext-tools/tests/init.sh"
                                  "gettext-tools/system-tests/run-test")
                     (("/bin/sh") "sh"))
                   (substitute* '("gettext-tools/src/project-id"
                                  "gettext-tools/projects/KDE/trigger"
                                  "gettext-tools/projects/GNOME/trigger")
                     (("/bin/pwd") "pwd"))))
               (add-before 'check 'patch-tests
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;;libgettextlib-0.23.so => not found
                   (substitute* "gettext-tools/gnulib-tests/test-execute.sh"
                     (("^#!.*" all)
                      (string-append all "exit 77;\n")))
                   (let* ((bash (which "sh")))
                     ;; Some of the files we're patching are
                     ;; ISO-8859-1-encoded, so choose it as the default
                     ;; encoding so the byte encoding is preserved.
                     (with-fluids ((%default-port-encoding #f))
                       (substitute*
                           (find-files "gettext-tools/tests"
                                       "^(lang-sh|msg(exec|filter)-[0-9])")
                         (("#![[:blank:]]/bin/sh")
                          (format #f "#!~a" bash)))

                       (substitute* (cons "gettext-tools/src/msginit.c"
                                          (find-files "gettext-tools/gnulib-tests"
                                                      "posix_spawn"))
                         (("/bin/sh")
                          bash))

                       (substitute* "gettext-tools/src/project-id"
                         (("/bin/pwd")
                          "pwd"))

                       ;; Work around Gnulib test failures on armhf-linux.
                       #$@(if (target-arm32?)
                              #~((with-directory-excursion "gettext-tools"
                                   (invoke "patch" "--force" "-p1" "-i"
                                           #$(local-file
                                              (search-patch
                                               "coreutils-gnulib-tests.patch")))))
                              '())

                       #$@(if (target-hurd?)
                              #~((substitute*
                                     "gettext-tools/gnulib-tests/Makefile.in"
                                   ;; See 'coreutils' for the rationale.
                                   ((" test-tls\\$\\(EXEEXT\\) ") " ")))
                              '())))))
          #$@(if (%current-target-system)
                 #~((add-after 'install 'patch-cross-shebangs
                      (lambda _
                        (let ((path (list (string-append #$bash-minimal "/bin"))))
                          (define (patch-cross-shebang file)
                            (patch-shebang file path))
                          (with-directory-excursion
                              (string-append #$output "/bin")
                            (for-each
                             patch-cross-shebang
                             '("autopoint" "gettext.sh" "gettextize")))))))
                 '()))

       ;; When tests fail, we want to know the details.
       #:make-flags #~'("VERBOSE=yes")))
    (home-page "https://www.gnu.org/software/gettext/")
    (synopsis
     "Tools and documentation for translation (used to build other packages)")
    (description
     "GNU Gettext is a package providing a framework for translating the
textual output of programs into multiple languages.  It provides translators
with the means to create message catalogs, and a runtime library to load
translated messages from the catalogs.  Nearly all GNU packages use Gettext.")
    (properties `((upstream-name . "gettext")
                  (cpe-name . "gettext")))
    (license gpl3+)))                             ;some files are under GPLv2+

;; Use that name to avoid clashes with Guile's 'gettext' procedure.
;;
;; We used to resort to #:renamer on the user side, but that prevented
;; circular dependencies involving (gnu packages gettext).  This is because
;; 'resolve-interface' (as of Guile 2.0.9) iterates eagerly over the used
;; module when there's a #:renamer, and that module may be empty at that point
;; in case or circular dependencies.
(define-public gnu-gettext
  (package/inherit gettext-minimal
    (name "gettext")
    (arguments
     (substitute-keyword-arguments (package-arguments gettext-minimal)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'add-emacs-autoloads
              (lambda* (#:key outputs #:allow-other-keys)
                ;; Make 'po-mode' and other things available by default.
                (with-directory-excursion
                    (string-append (assoc-ref outputs "out")
                                   "/share/emacs/site-lisp")
                  (symlink "start-po.el" "gettext-autoloads.el"))))))))
    (native-inputs (list emacs-minimal))          ;for Emacs tools
    (synopsis "Tools and documentation for translation")))

(define-public libtextstyle
  (package
    (name "libtextstyle")
    (version "0.21")
    (source (origin
              (inherit (package-source gnu-gettext))
              (uri (string-append "mirror://gnu/gettext/gettext-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04kbg1sx0ncfrsbr85ggjslqkzzb243fcw9nyh3rrv1a22ihszf7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'chdir
                    (lambda _
                      (chdir "libtextstyle")
                      #t)))))
    ;; libtextstyle bundles libxml2, glib (a small subset thereof), and
    ;; libcroco, but it purposefully prevents users from using an external
    ;; copy, to reduce the startup time of programs using libtextstyle.
    (home-page "https://www.gnu.org/software/gettext/")
    (synopsis "Text styling library")
    (description
     "GNU libtextstyle is a C library that provides an easy way to add styling
to programs that produce output to a console or terminal emulator window.  It
allows applications to emit text annotated with styling information, such as
color, font attributes (weight, posture), or underlining.")
    (license gpl3+)))

(define-public mdpo
  (package
    (name "mdpo")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mdpo" version))
       (sha256
        (base32 "0kgbm0af7jwpfspa2xxiy9nc2l1r2s1rhbhz4r229zcqv49ak6sq"))))
    (build-system python-build-system)
    (native-inputs
     (list python-bump2version
           python-flake8
           python-flake8-implicit-str-concat
           python-flake8-print
           python-isort
           python-pytest
           python-pytest-cov
           python-sphinx
           python-sphinx-argparse
           python-sphinx-rtd-theme
           python-twine
           python-yamllint))
    (propagated-inputs
     (list python-polib python-pymd4c))
    (home-page "https://github.com/mondeja/mdpo")
    (synopsis "Markdown file translation utilities using pofiles")
    (description
     "The mdpo utility creates pofiles, the format stabilished by GNU Gettext,
from Markdown files.")
    (license bsd-3)))

(define-public po4a
  (package
    (name "po4a")
    (version "0.74")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mquinson/po4a"
                                  "/releases/download/v"
                                  version "/po4a-" version ".tar.gz"))
              (sha256
               (base32
                "1hp7iy1rl8ci7rirh7r6d3jb0i16jm4vy3mgqd4bsyx35czk5z15"))))
    (build-system perl-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-programs
            (lambda _
              ;; Make sure all executables in "bin" find the Perl modules
              ;; required by this package at runtime.
              (for-each
               (lambda (file)
                 (wrap-program file
                   (list "PERL5LIB" 'suffix
                         (list (string-append #$output "/lib/perl5/site_perl")
                               (getenv "PERL5LIB")))))
               (find-files (string-append #$output "/bin/") "\\.*$"))))
          #$@(if (system-hurd?)
                 #~((add-after 'unpack 'skip-tests/hurd
                      (lambda _
                        (delete-file "t/cfg-multi.t")
                        (delete-file "t/cfg-single.t")
                        (delete-file "t/cfg-split.t"))))
                 #~()))))
    (native-inputs
     (list gettext-minimal
           perl-module-build
           docbook-xsl
           libxslt
           libxml2 ;xmlcatalog
           ;; For tests.
           docbook-sgml-4.1
           docbook-xml-4.5
           perl-test-pod
           (texlive-local-tree)))
    (inputs
     (list bash-minimal
           opensp
           perl-gettext
           perl-pod-parser
           perl-pod-simple
           perl-sgmls
           perl-syntax-keyword-try
           perl-xs-parse-keyword
           perl-term-readkey
           perl-text-wrapi18n
           perl-unicode-linebreak
           perl-yaml-tiny))
    (home-page "https://po4a.org/")
    (synopsis "Scripts to ease maintenance of translations")
    (description
     "The po4a (PO for anything) project goal is to ease translations (and
more interestingly, the maintenance of translations) using gettext tools on
areas where they were not expected like documentation.")
    (license gpl2+)))

(define-public po4a-minimal
  (package/inherit po4a
    (native-inputs
     ;; Remove test dependencies, primarily to reduce the size of the
     ;; dependency graph of the ‘guix’ package.
     (modify-inputs (package-native-inputs po4a)
       (delete "docbook-xml" "perl-test-pod" "texlive-local-tree")))
    (arguments
     (substitute-keyword-arguments (package-arguments po4a)
       ((#:tests? _ #t) #f)))
    (properties '((hidden? . #t)))))
