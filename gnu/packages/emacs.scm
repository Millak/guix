;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2017, 2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019, 2020, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2023, 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Jesse John Gildersleve <jessejohngildersleve@zohomail.eu>
;;; Copyright © 2019 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2019, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 Declan Tsien <declantsien@riseup.net>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)     ; for librsvg
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages lesstif)   ; motif
  #:use-module (gnu packages linux)     ; alsa-lib, gpm
  #:use-module (gnu packages mail)      ; for mailutils
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages web)       ; for jansson
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (emacs->emacs-next))

(define (%emacs-modules build-system)
  (let ((which (build-system-name build-system)))
    `((guix build ,(symbol-append which '-build-system))
      (guix build utils)
      (srfi srfi-1)
      (ice-9 ftw))))

(define* (emacs-ert-selector excluded-tests #:key run-nativecomp run-expensive run-unstable)
  "Create an ERT selector that excludes tests."
  (string-append
   "(not (or "
   (if run-nativecomp
       ""
       "(tag :nativecomp) ")
   (if run-expensive
       ""
       "(tag :expensive-test) ")
   (if run-unstable
       ""
       "(tag :unstable) ")
   (string-join
    (map
     (lambda (test)
       (string-append "\\\"" test "\\\""))
     excluded-tests))
   "))"))

(define %selector
  (emacs-ert-selector
   '("bytecomp--fun-value-as-head"
     "esh-util-test/path/get-remote"
     "esh-var-test/path-var/preserve-across-hosts"
     "ffap-tests--c-path"
     "find-func-tests--locate-macro-generated-symbols"
     "grep-tests--rgrep-abbreviate-properties-darwin"
     "grep-tests--rgrep-abbreviate-properties-gnu-linux"
     "grep-tests--rgrep-abbreviate-properties-windows-nt-dos-semantics"
     "grep-tests--rgrep-abbreviate-properties-windows-nt-sh-semantics"
     "info-xref-test-makeinfo"
     "man-tests-find-header-file"
     "tab-bar-tests-quit-restore-window"
     "tramp-test48-remote-load-path")))

(define-public emacs-minimal
  (package
    (name "emacs-minimal")
    (version "30.1")
    ;; Note: When using (replacement …), ensure that comp-native-version-dir
    ;; stays the same across grafts.
    ;; Run `make check-system TESTS=emacs-native-comp' to ensure that grafts
    ;; can meaningfully be applied.
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/emacs/emacs-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "13qkdx515qv7m8b2mpd37p16frs0xgl7bw8xvv397bz6fspc3jkc"))
              (patches (search-patches "emacs-disable-jit-compilation.patch"
                                       "emacs-exec-path.patch"
                                       "emacs-fix-scheme-indent-function.patch"
                                       "emacs-native-comp-driver-options.patch"
                                       "emacs-native-comp-fix-filenames.patch"
                                       "emacs-native-comp-pin-packages.patch"
                                       "emacs-pgtk-super-key-fix.patch"))
              (modules '((guix build utils)))
              (snippet
               '(with-directory-excursion "lisp"
                  ;; Delete the bundled byte-compiled elisp files and generated
                  ;; autoloads.
                  (for-each delete-file
                            (append (find-files "." "\\.elc$")
                                    (find-files "." "loaddefs\\.el$")
                                    (find-files "eshell" "^esh-groups\\.el$")))

                  ;; Make sure Tramp looks for binaries in the right places on
                  ;; remote Guix System machines, where 'getconf PATH' returns
                  ;; something bogus.
                  (substitute* "net/tramp.el"
                    ;; Patch the line after "(defcustom tramp-remote-path".
                    (("\\(tramp-default-remote-path")
                     (format
                      #f "(tramp-default-remote-path ~s ~s ~s ~s ~s ~s ~s "
                      "/run/privileged/bin"
                      "~/.guix-profile/bin" "~/.guix-profile/sbin"
                      "~/.guix-home/bin" "~/.guix-home/sbin"
                      "/run/current-system/profile/bin"
                      "/run/current-system/profile/sbin")))

                  ;; Make sure Man looks for C header files in the right
                  ;; places.
                  (substitute* "man.el"
                    (("\"/usr/local/include\"" line)
                     (string-join
                      (list line
                            "\"~/.guix-profile/include\""
                            "\"~/.guix-home/include\""
                            "\"/run/current-system/profile/include\"")
                      " ")))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules (%emacs-modules build-system)
      #:configure-flags #~(list
                           #$(string-append
                              "CFLAGS=-g -O2"
                              " -Wno-error=incompatible-pointer-types")
                           "--with-gnutls=no"
                           "--disable-build-details")
      #:make-flags #~(list (string-append "SELECTOR=" #$%selector))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enable-elogind
            (lambda _
              (substitute* "configure.ac"
                (("libsystemd") "libelogind"))
              (when (file-exists? "configure")
                (delete-file "configure"))))
          (add-after 'unpack 'patch-program-file-names
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Substitute "sh" command.
              (substitute* '("src/callproc.c"
                             "lisp/term.el"
                             "lisp/htmlfontify.el"
                             "lisp/mail/feedmail.el"
                             "lisp/obsolete/pgg-pgp.el"
                             "lisp/obsolete/pgg-pgp5.el"
                             "lisp/org/ob-eval.el"
                             "lisp/textmodes/artist.el"
                             "lisp/progmodes/sh-script.el"
                             "lisp/textmodes/artist.el"
                             "lisp/htmlfontify.el"
                             "lisp/term.el")
                (("\"/bin/sh\"")
                 (format #f "~s" (search-input-file inputs "bin/sh"))))
              (substitute* '("lisp/gnus/mm-uu.el"
                             "lisp/gnus/nnrss.el"
                             "lisp/mail/blessmail.el")
                (("\"#!/bin/sh\\\n\"")
                 (format #f "\"#!~a~%\"" (search-input-file inputs "bin/sh"))))
              (substitute* '("lisp/jka-compr.el"
                             "lisp/man.el")
                (("\"sh\"")
                 (format #f "~s" (search-input-file inputs "bin/sh"))))

              ;; Substitute "awk" command.
              (substitute* '("lisp/gnus/nnspool.el"
                             "lisp/org/ob-awk.el"
                             "lisp/man.el")
                (("\"awk\"")
                 (format #f "~s" (search-input-file inputs "bin/awk"))))

              ;; Substitute "find" command.
              (substitute* '("lisp/gnus/gnus-search.el"
                             "lisp/obsolete/nnir.el"
                             "lisp/progmodes/executable.el"
                             "lisp/progmodes/grep.el"
                             "lisp/filecache.el"
                             "lisp/ldefs-boot.el"
                             "lisp/mpc.el")
                (("\"find\"")
                 (format #f "~s" (search-input-file inputs "bin/find"))))

              ;; Substitute "sed" command.
              (substitute* "lisp/org/ob-sed.el"
                (("org-babel-sed-command \"sed\"")
                 (format #f "org-babel-sed-command ~s"
                         (search-input-file inputs "bin/sed"))))
              (substitute* "lisp/man.el"
                (("Man-sed-command \"sed\"")
                 (format #f "Man-sed-command ~s"
                         (search-input-file inputs "bin/sed"))))

              (substitute* "lisp/doc-view.el"
                (("\"(gs|dvipdf|ps2pdf|pdftotext)\"" all what)
                 (let ((replacement (false-if-exception
                                     (search-input-file
                                      inputs
                                      (string-append "/bin/" what)))))
                   (if replacement
                       (string-append "\"" replacement "\"")
                       all))))
              ;; match ".gvfs-fuse-daemon-real" and ".gvfsd-fuse-real"
              ;; respectively when looking for GVFS processes.
              (substitute* "lisp/net/tramp-gvfs.el"
                (("\\(tramp-compat-process-running-p \"(.*)\"\\)" all process)
                 (format #f "(or ~a (tramp-compat-process-running-p ~s))"
                         all (string-append "." process "-real"))))))
          (add-before 'configure 'fix-/bin/pwd
            (lambda _
              ;; Use `pwd', not `/bin/pwd'.
              (substitute* (find-files "." "^Makefile\\.in$")
                (("/bin/pwd")
                 "pwd"))))
          (add-after 'unpack 'fix-tests
            (lambda* (#:key tests? inputs #:allow-other-keys)
              (when tests?
                (substitute* "test/src/process-tests.el"
                  (("/bin//sh") (search-input-file inputs "bin/sh")))
                (substitute* "test/lisp/eshell/em-script-tests.el"
                  (("/usr/bin/env") (search-input-file inputs "bin/env"))))))
          (add-after 'install 'install-site-start
            ;; Use 'guix-emacs' in "site-start.el", which is used autoload the
            ;; Elisp packages found in EMACSLOADPATH.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out      (assoc-ref outputs "out"))
                     (lisp-dir (string-append out "/share/emacs/site-lisp"))
                     (emacs    (string-append out "/bin/emacs")))

                ;; This is duplicated from emacs-utils to prevent coupling.
                (define* (emacs-byte-compile-directory dir)
                  (let ((expr `(progn
                                (setq byte-compile-debug t)
                                (byte-recompile-directory
                                 (file-name-as-directory ,dir) 0 1))))
                    (invoke emacs "--quick" "--batch"
                            (format #f "--eval=~s" expr))))

                (copy-file #$(local-file
                              (search-auxiliary-file "emacs/guix-emacs.el"))
                           (string-append lisp-dir "/guix-emacs.el"))
                (with-output-to-file (string-append lisp-dir "/site-start.el")
                  (lambda ()
                    (display
                     (string-append
                      "(when (require 'guix-emacs nil t)\n"
                      "  (guix-emacs-autoload-packages 'no-reload)\n"
                      "  (advice-add 'package-load-all-descriptors"
                      " :after #'guix-emacs-load-package-descriptors))"))))
                ;; Remove the extraneous subdirs.el file, as it causes Emacs to
                ;; add recursively all the the sub-directories of a profile's
                ;; share/emacs/site-lisp union when added to EMACSLOADPATH,
                ;; which leads to conflicts.
                (delete-file (string-append lisp-dir "/subdirs.el"))
                ;; Byte compile the site-start files.
                (emacs-byte-compile-directory lisp-dir))))
          (add-after 'install 'wrap-emacs-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lisp-dirs (find-files (string-append out "/share/emacs")
                                            "^lisp$"
                                            #:directories? #t)))
                (for-each
                 (lambda (prog)
                   (wrap-program prog
                     ;; Some variants rely on uname being in PATH for Tramp.
                     ;; Tramp paths can't be hardcoded, because they need to
                     ;; be portable.
                     `("PATH" suffix
                       ,(map dirname
                             (list (search-input-file inputs "/bin/gzip")
                                   ;; for coreutils
                                   (search-input-file inputs "/bin/yes"))))
                     `("EMACSLOADPATH" suffix ,lisp-dirs)))
                 (find-files (string-append out "/bin")
                             ;; Matches versioned and unversioned emacs binaries.
                             ;; We don't patch emacsclient, because it takes its
                             ;; environment variables from emacs.
                             ;; Likewise, we don't need to patch helper binaries
                             ;; like etags, ctags or ebrowse.
                             "^emacs(-[0-9]+(\\.[0-9]+)*)?$")))))
          (add-after 'wrap-emacs-paths 'undo-double-wrap
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Directly copy emacs-X.Y to emacs, so that it is not wrapped
              ;; twice.  This also fixes a minor issue, where WMs would not be
              ;; able to track emacs back to emacs.desktop.
              (with-directory-excursion (assoc-ref outputs "out")
                (copy-file
                 (car (find-files "bin" "^emacs-([0-9]+\\.)+[0-9]+$"))
                 "bin/emacs")))))))
    (inputs (list bash-minimal coreutils findutils gawk gzip ncurses sed))
    (native-inputs (list autoconf pkg-config texinfo))
    (home-page "https://www.gnu.org/software/emacs/")
    (synopsis "The extensible text editor (minimal build for byte-compilation)")
    (description
     "GNU Emacs is an extensible and highly customizable text editor.  It is
based on an Emacs Lisp interpreter with extensions for text editing.  Emacs
has been extended in essentially all areas of computing, giving rise to a
vast array of packages supporting, e.g., email, IRC and XMPP messaging,
spreadsheets, remote server editing, and much more.  Emacs includes extensive
documentation on all aspects of the system, from basic editing to writing
large Lisp programs.  It has full Unicode support for nearly all human
languages.")
    (license license:gpl3+)
    (native-search-paths
     (list (search-path-specification
            (variable "EMACSLOADPATH")
            (files '("share/emacs/site-lisp")))
           (search-path-specification
            (variable "EMACSNATIVELOADPATH")
            (files '("lib/emacs/native-site-lisp")))
           (search-path-specification
            (variable "INFOPATH")
            (files '("share/info")))
           ;; Most variants support tree-sitter, so let's include it here.
           (search-path-specification
            (variable "TREE_SITTER_GRAMMAR_PATH")
            (files '("lib/tree-sitter")))))
    (properties `((upstream-name . "emacs")))))

(define-public emacs-no-x
  (package/inherit emacs-minimal
    (name "emacs-no-x")
    (synopsis "The extensible, customizable, self-documenting text
editor (console only)")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-minimal)
       ((#:configure-flags flags #~'())
        #~(cons* "--with-modules" "--with-native-compilation=aot"
                 (delete "--with-gnutls=no" #$flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'set-paths 'set-libgccjit-path
              (lambda* (#:key inputs #:allow-other-keys)
                (define (first-subdirectory/absolute directory)
                  (let ((files (scandir
                                directory
                                (lambda (file)
                                  (and (not (member file '("." "..")))
                                       (file-is-directory? (string-append
                                                            directory "/"
                                                            file)))))))
                    (and (not (null? files))
                         (string-append directory "/" (car files)))))
                (let* ((libgccjit-libdir
                        (first-subdirectory/absolute ;; version
                         (first-subdirectory/absolute ;; host type
                          (search-input-directory inputs "lib/gcc")))))
                  (setenv "LIBRARY_PATH"
                          (string-append (getenv "LIBRARY_PATH")
                                         ":" libgccjit-libdir)))))
            (add-after 'unpack 'patch-compilation-driver
              (lambda _
                (substitute* "lisp/emacs-lisp/comp.el"
                  (("\\(defcustom native-comp-driver-options nil")
                   (format
                    #f "(defcustom native-comp-driver-options '(~@{~s~^ ~})"
                    (string-append
                     "-B" #$(this-package-input "binutils") "/bin/")
                    (string-append
                     "-B" #$(this-package-input "glibc") "/lib/")
                    (string-append
                     "-B" #$(this-package-input "libgccjit") "/lib/")
                    (string-append
                     "-B" #$(this-package-input "libgccjit") "/lib/gcc/"))))))
            (add-after 'build 'build-trampolines
              (lambda* (#:key make-flags #:allow-other-keys)
                (apply invoke "make" "trampolines" make-flags)))
            (add-after 'validate-runpath 'validate-comp-integrity
              (lambda* (#:key outputs #:allow-other-keys)
                #$(cond
                   ((%current-target-system)
                    #~(display "Cannot validate native-comp on cross builds.\n"))
                   ((member (%current-system) '("armhf-linux" "i686-linux"))
                    #~(display "Integrity test is broken on armhf.\n"))
                   (else
                    #~(invoke
                       (string-append (assoc-ref outputs "out") "/bin/emacs")
                       "--batch"
                       "--load"
                       #$(local-file
                          (search-auxiliary-file "emacs/comp-integrity.el"))
                       "-f" "ert-run-tests-batch-and-exit")))))))))
    (inputs
     (modify-inputs (package-inputs emacs-minimal)
       (prepend gnutls
                ;; To "unshadow" ld-wrapper in native builds
                (make-ld-wrapper "ld-wrapper" #:binutils binutils)
                ;; For native compilation
                binutils
                (libc-for-target)
                libgccjit

                ;; Avoid Emacs's limited movemail substitute that retrieves POP3
                ;; email only via insecure channels.
                ;; This is not needed for (modern) IMAP.
                mailutils

                acl
                alsa-lib
                elogind
                ghostscript
                gpm
                jansson
                lcms
                libice
                libselinux
                libsm
                libxml2
                m17n-lib
                sqlite
                tree-sitter
                zlib)))))

(define-public emacs
  (package/inherit emacs-no-x
    (name "emacs")
    (synopsis "The extensible, customizable, self-documenting text editor")
    (build-system glib-or-gtk-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-no-x)
       ((#:modules _) (%emacs-modules build-system))
       ((#:configure-flags flags #~'())
        #~(cons* "--with-cairo" #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; Note: due to the changed #:modules, %standard-phases in #$phases
            ;; refers to glib-or-gtk:%standard-phases, so we don't need to add
            ;; them ourselves.
            (add-after 'glib-or-gtk-wrap 'restore-emacs-pdmp
              ;; Restore the dump file that Emacs installs somewhere in
              ;; libexec/ to its original state.
              (lambda* (#:key outputs target #:allow-other-keys)
                (let* ((libexec (string-append (assoc-ref outputs "out")
                                               "/libexec"))
                       ;; each of these ought to only match a single file,
                       ;; but even if not (find-files) sorts by string<,
                       ;; so the Nth element in one maps to the Nth element of
                       ;; the other
                       (pdmp (find-files libexec "\\.pdmp$"))
                       (pdmp-real (find-files libexec "\\.pdmp-real$")))
                  (for-each rename-file pdmp-real pdmp))))))))
    (inputs (modify-inputs (package-inputs emacs-no-x)
              (prepend
               cairo
               dbus
               gtk+
               giflib
               harfbuzz
               libjpeg-turbo
               libotf
               libpng
               (librsvg-for-system)
               libtiff
               libx11
               libxft
               libxpm
               libwebp
               pango
               poppler)))))

(define-public emacs-pgtk
  (package/inherit emacs
    (name "emacs-pgtk")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:configure-flags flags #~'())
        #~(cons* "--with-pgtk" #$flags))))
    (synopsis "Emacs text editor with @code{pgtk} frames")
    (description "This Emacs build implements graphical UI purely in terms
of GTK.")))

(define-public emacs-xwidgets
  (package/inherit emacs
    (name "emacs-xwidgets")
    (synopsis "The extensible, customizable, self-documenting text
editor (with xwidgets support)")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:configure-flags flags #~'())
        #~(cons "--with-xwidgets" #$flags))))
    (inputs
     (modify-inputs (package-inputs emacs)
       (prepend webkitgtk-with-libsoup2 libxcomposite)))))

(define-public emacs-pgtk-xwidgets
  (package
    (inherit emacs-pgtk)
    (name "emacs-pgtk-xwidgets")
    (synopsis "Emacs text editor with @code{xwidgets} and @code{pgtk} support")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-pgtk)
       ((#:configure-flags flags #~'())
        #~(cons "--with-xwidgets" #$flags))))
    (inputs
     (modify-inputs (package-inputs emacs-pgtk)
       (prepend gsettings-desktop-schemas webkitgtk-with-libsoup2)))))

(define-public emacs-lucid
  (package/inherit emacs-no-x
    (name "emacs-lucid")
    (synopsis
     "The extensible, customizable, self-documenting text editor (with Lucid toolkit)")
    (inputs (modify-inputs (package-inputs emacs)
              (delete "gtk+")
              (prepend libxaw)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments emacs-no-x)
       ((#:configure-flags flags #~'())
        #~(cons "--with-x-toolkit=lucid"
                #$flags))))))

(define-public emacs-motif
  (package/inherit emacs-no-x
    (name "emacs-motif")
    (synopsis
     "The extensible, customizable, self-documenting text editor (with Motif
toolkit)")
    ;; Using emacs' inputs as base, since it has all the graphical stuff
    (inputs (modify-inputs (package-inputs emacs)
              (delete "gtk+")
              (prepend inotify-tools motif)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments emacs-no-x)
       ((#:configure-flags flags #~'())
        #~(cons "--with-x-toolkit=motif"
                #$flags))))))

(define-public emacs-no-x-toolkit
  (package/inherit emacs-no-x
    (name "emacs-no-x-toolkit")
    (synopsis "The extensible, customizable, self-documenting text
editor (without X toolkit)" )
    ;; Using emacs' inputs as base, since it has all the graphical stuff
    (inputs (modify-inputs (package-inputs emacs)
              (delete "gtk+")
              (prepend inotify-tools)))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-no-x)
       ((#:configure-flags flags #~'())
        #~(cons "--with-x-toolkit=no" #$flags))))))

(define-public emacs-wide-int
  (package/inherit emacs
    (name "emacs-wide-int")
    (synopsis "The extensible, customizable, self-documenting text
editor (with wide ints)" )
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:configure-flags flags)
        #~(cons "--with-wide-int" #$flags))))))

(define-public emacs-next-minimal
  (let ((commit "9663c959c73d6cca0c56f833d80ff1d9e9708b70")
        (revision "1"))
  (package
    (inherit emacs-minimal)
    (name "emacs-next-minimal")
    (version (git-version "31.0.50" revision commit))
    (source
     (origin
       (inherit (package-source emacs-minimal))
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/emacs.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a03j9zdn1fl181xcqsw4vg3v8a3sbv1r3d49ld6ysldvfkwiz39"))
       (patches
        (search-patches "emacs-next-disable-jit-compilation.patch"
                        "emacs-next-exec-path.patch"
                        "emacs-fix-scheme-indent-function.patch"
                        "emacs-native-comp-driver-options.patch"
                        "emacs-next-native-comp-fix-filenames.patch"
                        "emacs-native-comp-pin-packages.patch"
                        "emacs-pgtk-super-key-fix.patch")))))))

(define* (emacs->emacs-next emacs #:optional name
                            #:key (version (package-version emacs-next-minimal))
                            (source (package-source emacs-next-minimal)))
  (package
    (inherit emacs)
    (name (or name
              (and (string-prefix? "emacs" (package-name emacs))
                   (string-append "emacs-next"
                                  (string-drop (package-name emacs)
                                               (string-length "emacs"))))))
    (version version)
    (source source)))

(define-public emacs-next (emacs->emacs-next emacs))
(define-public emacs-next-pgtk (emacs->emacs-next emacs-pgtk))
(define-public emacs-next-pgtk-xwidgets (emacs->emacs-next emacs-pgtk-xwidgets))
(define-public emacs-next-tree-sitter
  (deprecated-package "emacs-next-tree-sitter" emacs-next))

(define-public guile-emacs
  (let ((upstream-version "31.0.50")
        (commit "f2d0459d6a4cda8b7c38b6fb12a55f1330ba50fa")
        (revision "2"))
    (package
      (inherit emacs)
      (name "guile-emacs")
      (version (git-version upstream-version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://codeberg.org/lyrra/guilemacs")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ry0dn9xrndpz54hs92dnnsw4xa4qn4lz898ldlh2rs47ah1f4wl"))))
      (native-inputs
       (modify-inputs (package-native-inputs emacs)
         (prepend autoconf automake guile-for-guile-emacs)))
      (inputs
       (modify-inputs (package-inputs emacs)
         (prepend imagemagick)))
      (home-page "https://guile-emacs.org")
      (synopsis "Emacs text editor powered by Guile('s elisp implementation)")
      (description
       "Guile-Emacs brings Emacs and Guile together by providing a new Elisp
implementation based on Guile's compiler technology, serving as the basis for
a more expressive and extensible version of Elisp.

Started in 2014 as a GSOC project, Guile-Emacs was resurrected in 2024.")
      (arguments
       (substitute-keyword-arguments `(#:strip-binaries? #f
                                       ,@(package-arguments emacs))
         ((#:configure-flags flags ''())
          #~`("CFLAGS=-Og -ggdb3"
              "--with-native-compilation=no"
              "--without-modules"
              "--without-threads"
              "--with-jpeg=no"
              "--without-cairo"
              "--without-tree-sitter"
              "--with-imagemagick"
              ,@(fold delete #$flags '("--with-cairo"
                                       "--with-modules"
                                       "--with-native-compilation=aot"))))
         ((#:make-flags flags #~'())
          #~(list "V=1"))
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'autogen
                (lambda _
                  (invoke "sh" "autogen.sh")))
              (delete 'patch-compilation-driver)
              (delete 'set-libgccjit-path)
              (delete 'validate-comp-integrity)
              (delete 'restore-emacs-pdmp)
              (delete 'build-trampolines)
              (delete 'install-site-start)
              (replace 'wrap-emacs-paths
                ;; Restrict EMACSLOADPATH to builtin packages.
                (lambda _
                  (let ((lisp-dirs (list (string-append
                                          #$output "/share/emacs/"
                                          #$upstream-version "/lisp")))
                        (inputs '#$(map (match-lambda
                                          ((name directory)
                                           #~(#$name . #$directory)))
                                        (package-inputs this-package))))
                    (for-each
                     (lambda (prog)
                       (wrap-program prog
                         ;; Some variants rely on uname being in PATH for Tramp.
                         ;; Tramp paths can't be hardcoded, because they need to
                         ;; be portable.
                         `("PATH" suffix
                           ,(map dirname
                                 (list (search-input-file inputs "/bin/gzip")
                                       ;; for coreutils
                                       (search-input-file inputs "/bin/yes"))))
                         ;; We use "=" because loading non-builtin packages is
                         ;; currently not supported and prevents guile-emacs
                         ;; from running.
                         `("EMACSLOADPATH" = ,lisp-dirs)))
                     (find-files
                      (string-append #$output "/bin")
                      ;; Matches versioned and unversioned emacs binaries.
                      ;; We don't patch emacsclient, because it takes its
                      ;; environment variables from emacs.
                      ;; Likewise, we don't need to patch helper binaries
                      ;; like etags, ctags or ebrowse.
                      "^emacs(-[0-9]+(\\.[0-9]+)*)?$")))))
              (add-after 'configure 'touch-lisp/finder-inf.el
                (lambda _
                  (call-with-output-file "lisp/finder-inf.el"
                    (lambda (port) (display port))))))))))))

(define-public m17n-db
  (package
    (name "m17n-db")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/m17n/m17n-db-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0vfw7z9i2s9np6nmx1d4dlsywm044rkaqarn7akffmb6bf1j6zv5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-charmaps="
                            (assoc-ref %build-inputs "libc")
                            "/share/i18n/charmaps"))))
    ;; With `guix lint' the home-page URI returns a small page saying
    ;; that your browser does not handle frames. This triggers the "URI
    ;; returns suspiciously small file" warning.
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Multilingual text processing library (database)")
    (description "The m17n library realizes multilingualization of
many aspects of applications.  The m17n library represents
multilingual text as an object named M-text.  M-text is a string with
attributes called text properties, and designed to substitute for
string in C.  Text properties carry any information required to input,
display and edit the text.

This package contains the library database.")
    (license license:lgpl2.1+)))

(define-public m17n-lib
  (package
    (name "m17n-lib")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/m17n/m17n-lib-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0jp61y09xqj10mclpip48qlfhniw8gwy8b28cbzxy8hq8pkwmfkq"))
       (patches (search-patches "m17n-lib-1.8.0-use-pkg-config-for-freetype.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (if (%current-target-system)
         (list pkg-config
               libtool
               gettext-minimal
               autoconf automake)
         '()))
    (inputs
     (list fribidi
           gd
           libotf
           libxft
           libxml2
           m17n-db))
    (arguments
     `(#:parallel-build? #f
       ,@(if (%current-target-system)
             '(#:phases
               (modify-phases %standard-phases
                 ;; AC_FUNC_MALLOC and AC_FUNC_REALLOC usually unneeded
                 ;; see https://lists.gnu.org/archive/html/autoconf/2003-02/msg00017.html
                 (add-after 'unpack 'fix-rpl_malloc
                   (lambda _
                     (substitute* "configure.ac"
                       (("AC_FUNC_MALLOC") "")
                       (("AC_FUNC_REALLOC") ""))
                     ;; let bootstrap phase run.
                     (delete-file "./configure")))))
             '())))
    ;; With `guix lint' the home-page URI returns a small page saying
    ;; that your browser does not handle frames. This triggers the "URI
    ;; returns suspiciously small file" warning.
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Multilingual text processing library (runtime)")
    (description "The m17n library realizes multilingualization of
many aspects of applications.  The m17n library represents
multilingual text as an object named M-text.  M-text is a string with
attributes called text properties, and designed to substitute for
string in C.  Text properties carry any information required to input,
display and edit the text.

This package contains the library runtime.")
    (license license:lgpl2.1+)))
