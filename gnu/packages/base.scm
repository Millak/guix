;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2014, 2015 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016, 2017, 2019-2023, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2020, 2023, 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2018, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Vitaliy Shatrov <D0dyBo0D0dyBo0@protonmail.com>
;;; Copyright © 2020 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021, 2024 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2022 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2024, 2025 Zheng Junjie <z572@z572.online>
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

(define-module (gnu packages base)
  #:use-module ((guix licenses)
                #:select (gpl3+ lgpl2.0+ lgpl3+ public-domain))
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gettext)
  #:use-module (guix i18n)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix search-paths)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (glibc
            libc-for-target
            libc-locales-for-target
            libc-utf8-locales-for-target
            make-ld-wrapper
            libiconv-if-needed

            ;; Beware: the following should not be used the top level to avoid
            ;; introducing circular module dependencies.
            canonical-package
            %final-inputs))

;;; Commentary:
;;;
;;; Base packages of the Guix-based GNU user-land software distribution.
;;;
;;; Code:

(define-public hello
  (package
    (name "hello")
    (version "2.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hello/hello-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1aqq1379syjckf0wdn9vs6wfbapnj9zfikhiykf29k4jq9nrk6js"))))
    (build-system gnu-build-system)
    (synopsis "Example GNU package")
    (description
     "GNU Hello prints the message \"Hello, world!\" and then exits.  It
serves as an example of standard GNU coding practices.  As such, it supports
command-line arguments, multiple languages, and so on.")
    (home-page "https://www.gnu.org/software/hello/")
    (license gpl3+)))

(define-public grep
  (package
   (name "grep")
   (version "3.11")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/grep/grep-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1avf4x8skxbqrjp5j2qr9sp5vlf8jkw2i5bdn51fl3cxx3fsxchx"))
            (patches (search-patches "grep-timing-sensitive-test.patch"))))
   (build-system gnu-build-system)
   (native-inputs (list perl))                   ;some of the tests require it
   (inputs (list pcre2))
   (arguments
    (list #:configure-flags #~(list "--enable-perl-regexp")

          ;; XXX: On 32-bit Hurd platforms, 'time_t' is defined as a 32-bit
          ;; integer in 'hurd_types.defs', so this Gnulib test always fails.
          #:make-flags
          #~#$(if (and (not (%current-target-system))
                       (string=? (%current-system) "i586-gnu"))
                  #~'("XFAIL_TESTS=test-year2038")
                  #~'())

          #:phases
          #~(modify-phases %standard-phases
              (add-after 'install 'fix-egrep-and-fgrep
                ;; Patch 'egrep' and 'fgrep' to execute 'grep' via its
                ;; absolute file name instead of searching for it in $PATH.
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (bin (string-append out "/bin")))
                    (substitute* (list (string-append bin "/egrep")
                                       (string-append bin "/fgrep"))
                      (("^exec grep")
                       (string-append "exec " bin "/grep"))))))
              #$@(if (target-hurd64?)
                     #~((add-after 'unpack 'patch-sigsegv
                          (lambda _
                            ;; Stack overflow recovery does not compile
                            (substitute* "lib/sigsegv.in.h"
                              (("__GNU__") "__XGNU__")))))
                     #~())
              #$@(if (system-hurd?)
                     #~((add-before 'check 'skip-test
                          (lambda _
                            (substitute* ;; This test hangs
                                '("tests/hash-collision-perf"
                                  ;; This test fails
                                  "tests/file")
                              (("^#!.*" all)
                               (string-append all "exit 77;\n"))))))
                     #~())
              #$@(if (system-hurd64?)
                     #~((add-before 'check 'skip-test
                          (lambda _
                            (substitute* "tests/stack-overflow" ;This test hangs
                              (("^#!.*" all)
                               (string-append all "exit 77;\n"))))))
                     #~()))))
   (synopsis "Print lines matching a pattern")
   (description
     "grep is a tool for finding text inside files.  Text is found by
matching a pattern provided by the user in one or many files.  The pattern
may be provided as a basic or extended regular expression, or as fixed
strings.  By default, the matching text is simply printed to the screen,
however the output can be greatly customized to include, for example, line
numbers.  GNU grep offers many extensions over the standard utility,
including, for example, recursive directory searching.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/grep/")))

(define-public sed
  (package
    (name "sed")
    (version "4.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/sed/sed-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0bi808vfkg3szmpy9g5wc7jnn2yk6djiz412d30km9rky0c8liyi"))))
    (build-system gnu-build-system)
    (synopsis "Stream editor")
    (native-inputs (append (if (target-loongarch64?)
                               (list config)
                               '())
                           (list perl)))                    ;for tests
    (arguments (if (target-loongarch64?)
                   (list #:phases
                         #~(modify-phases %standard-phases
                             (add-after 'unpack 'update-config-scripts
                               (lambda* (#:key inputs native-inputs #:allow-other-keys)
                                 ;; Replace outdated config.guess and config.sub.
                                 (for-each (lambda (file)
                                             (install-file
                                              (search-input-file
                                               (or native-inputs inputs)
                                               (string-append "/bin/" file)) "./build-aux"))
                                           '("config.guess" "config.sub"))))))
                   '()))
    (description
     "Sed is a non-interactive, text stream editor.  It receives a text
input from a file or from standard input and it then applies a series of text
editing commands to the stream and prints its output to standard output.  It
is often used for substituting text patterns in a stream.  The GNU
implementation offers several extensions over the standard utility.")
    (license gpl3+)
    (home-page "https://www.gnu.org/software/sed/")))

(define-public tar
  (package
   (name "tar")
   (version "1.35")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/tar/tar-"
                                version ".tar.xz"))
            (sha256
             (base32
              "05nw7q7sazkana11hnf3f77lmybw1j9j6lsk93bsxirf6hvzyqjd"))
            (patches (search-patches "tar-skip-unreliable-tests.patch"
                                     "tar-remove-wholesparse-check.patch"))))
   (build-system gnu-build-system)
   ;; Note: test suite requires ~1GiB of disk space.
   (arguments
    `(,@(cond
          ((target-hurd?)
           '(#:make-flags
             (list (string-append
                     "TESTSUITEFLAGS= -k '"
                     "!sparse"
                     ",!renamed dirs in incrementals"
                     ",!--exclude-tag option in incremental pass"
                     ",!incremental dumps with -C"
                     ",!incremental dumps of nested directories"
                     ",!incremental restores with -C"
                     ",!concatenated incremental archives (renames)"
                     ",!renamed directory containing subdirectories"
                     ",!renamed subdirectories"
                     ",!chained renames"
                     ",!Directory"
                     "'"))))
          ;; https://lists.gnu.org/archive/html/bug-tar/2021-10/msg00007.html
          ;; tar-1.34 isn't aware of 64-bit time_t and upstream suggests
          ;; skipping the test for this release on 32-bit systems.
          ((not (target-64bit?))
           '(#:make-flags (list "TESTSUITEFLAGS= -k '!tricky time stamps'")))
          (else '()))
      ;; XXX: 32-bit Hurd platforms don't support 64bit time_t
      ,@(if (and (target-hurd?)
                 (not (target-64bit?)))
            (list #:configure-flags ''("--disable-year2038"))
            '())
      #:phases (modify-phases %standard-phases
                 (add-before 'build 'set-shell-file-name
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Do not use "/bin/sh" to run programs.
                     (let ((bash (assoc-ref inputs "bash")))
                       (substitute* "src/system.c"
                         (("/bin/sh")
                          (string-append bash "/bin/sh")))))))))

   ;; When cross-compiling, the 'set-shell-file-name' phase needs to be able
   ;; to refer to the target Bash.
   (inputs (if (%current-target-system)
               (list bash)
               '()))

   (synopsis "Managing tar archives")
   (description
    "Tar provides the ability to create tar archives, as well as the
ability to extract, update or list files in an existing archive.  It is
useful for combining many files into one larger file, while maintaining
directory structure and file information such as permissions and
creation/modification dates.  GNU tar offers many extensions over the
standard utility.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/tar/")))

;;; TODO: Replace/merge with 'patch' on core-updates.
(define-public patch/pinned
  (hidden-package
   (package
     (name "patch")
     (version "2.7.6")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/patch/patch-"
                                   version ".tar.xz"))
               (sha256
                (base32
                 "1zfqy4rdcy279vwn2z1kbv19dcfw25d2aqy9nzvdkq5bjzd0nqdc"))
               (patches (search-patches "patch-hurd-path-max.patch"))))
     (build-system gnu-build-system)
     (arguments
      ;; Work around a cross-compilation bug whereby libpatch.a would provide
      ;; '__mktime_internal', which conflicts with the one in libc.a.
      (if (%current-target-system)
          `(#:configure-flags '("gl_cv_func_working_mktime=yes"))
          '()))
     (native-inputs (list ed))
     (synopsis "Apply differences to originals, with optional backups")
     (description
      "Patch is a program that applies changes to files based on differences
laid out as by the program \"diff\".  The changes may be applied to one or more
files depending on the contents of the diff file.  It accepts several
different diff formats.  It may also be used to revert previously applied
differences.")
     (license gpl3+)
     (home-page "https://savannah.gnu.org/projects/patch/"))))

(define-public patch
  (package
    (inherit patch/pinned)
    (name "patch")
    (version "2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/patch/patch-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1qssgwgy3mfahkpgg99a35gl38vamlqb15m3c2zzrd62xrlywz7q"))))
    (arguments
     (substitute-keyword-arguments (package-arguments patch/pinned)
       ((#:configure-flags flags #~'())
        (if (and (target-hurd?) (not (target-64bit?)))
            #~(cons* "--disable-year2038"
                     #$flags)
            flags))))
    (properties '())))

(define-public diffutils
  (package
   (name "diffutils")
   (version "3.12")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/diffutils/diffutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1zbxf8vv7z18ypddwqgzj51n426k959fiv4wxbyl34b0r2gpz2vw"))))
   (build-system gnu-build-system)
   (arguments
    (list
     ;; XXX: On 32-bit Hurd platforms, 'time_t' is defined as a 32-bit
     ;; integer in 'hurd_types.defs', so this Gnulib test always fails.
     #:make-flags
     #~#$(cond ((and (not (%current-target-system))
                     (string=? (%current-system) "i586-gnu"))
                #~'("XFAIL_TESTS=test-year2038"))
               ;; TODO: Figure out why these gnulib tests are failing.
               ((and (not (%current-target-system))
                     (target-powerpc?))
                #~'("XFAIL_TESTS=test-float-h large-subopt"))
               (else
                #~'()))

     #:configure-flags
     #~#$(if (%current-target-system)
       ;; Fix for crosscompiling; on GNU system strcasecmp always works
       #~'("ac_cv_func_strcasecmp=yes" "gl_cv_func_strcasecmp_works=yes")
       #~'())

     #:phases (if (system-hurd?)
                  #~(modify-phases %standard-phases
                      (add-after 'unpack 'skip-tests
                        (lambda _
                          (substitute* "tests/large-subopt"
                            (("^#!.*" all)
                             (string-append all "exit 77;\n")))
                          #$@(if (system-hurd64?)
                                 #~((substitute*
                                        ;; These tests hang.
                                        '("gnulib-tests/test-c-stack.sh"
                                          "gnulib-tests/test-c-stack2.sh")
                                      (("^#!.*" all)
                                       (string-append all "exit 77;\n"))))
                                 #~()))))
                  #~%standard-phases)))
   (native-inputs (list perl))
   (synopsis "Comparing and merging files")
   (description
    "GNU Diffutils is a package containing tools for finding the
differences between files.  The \"diff\" command is used to show how two files
differ, while \"cmp\" shows the offsets and line numbers where they differ.
\"diff3\" allows you to compare three files.  Finally, \"sdiff\" offers an
interactive means to merge two files.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/diffutils/")))

(define-public findutils
  (package
   (name "findutils")
   (version "4.10.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/findutils/findutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1xd4y24qfsdfp3ndz7d5j49lkhbhpzgr13wrvsmx4izjgyvf11qk"))
            (patches (search-patches "findutils-localstatedir.patch"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags (list
                         ;; XXX: 32-bit Hurd platforms don't support 64bit time_t
                         ,@(if (and (target-hurd?)
                                    (not (target-64bit?)))
                               '("--disable-year2038")
                               '())
                         ;; Tell 'updatedb' to write to /var.
                         "--localstatedir=/var")
      #:phases (modify-phases %standard-phases
                 (add-before 'check 'adjust-test-shebangs
                   (lambda _
                     (substitute* '("tests/xargs/verbose-quote.sh"
                                    "tests/find/exec-plus-last-file.sh")
                       (("#!/bin/sh")
                        (string-append "#!" (which "sh"))))))
                 ,@(if (system-hurd?)
                       '((add-after 'unpack 'skip-tests
                           (lambda _
                             (substitute*
                                 ;; This test fails non-deterministically
                                 "gnulib-tests/test-strerror_r.c"
                               (("(^| )main *\\(.*" all)
                                (string-append all "{\n  exit (77);//"))))))
                       '()))))
   (synopsis "Operating on files matching given criteria")
   (description
    "Findutils supplies the basic file directory searching utilities of the
GNU system.  It consists of two primary searching utilities: \"find\"
recursively searches for files in a directory according to given criteria and
\"locate\" lists files in a database that match a query.  Two auxiliary tools
are included: \"updatedb\" updates the file name database and \"xargs\" may be
used to apply commands with arbitrarily long arguments.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/findutils/")))

(define-public coreutils
  (package
   (name "coreutils")
   (version "9.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/coreutils/coreutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "08q4b0w7mwfxbqjs712l6wrwl2ijs7k50kssgbryg9wbsw8g98b1"))))
   (build-system gnu-build-system)
   (inputs `(,acl                                 ;TODO: add SELinux
             ,attr                                ;for xattrs in ls, mv, etc
             ,gmp                                 ;bignums in 'expr', yay!

             ;; Do not use libcap when cross-compiling since it's not quite
             ;; cross-compilable; and use it only for supported systems.
             ,@(if (and (not (%current-target-system))
                        (member (%current-system)
                                (package-supported-systems libcap)))
                   `(,libcap)                ;capability support in 'ls', etc.
                   '())))
   (native-inputs
    ;; Perl is needed to run tests in native builds, and to run the bundled
    ;; copy of help2man.  However, don't pass it when cross-compiling since
    ;; that would lead it to try to run programs to get their '--help' output
    ;; for help2man.
    (if (%current-target-system)
        '()
        (list perl)))
   (outputs '("out" "debug"))
   (arguments
    `(#:parallel-build? #f            ; help2man may be called too early
      ,@(if (system-hurd?)
            '(#:make-flags            ; these tests fail deterministically
              (list (string-append "XFAIL_TESTS="
                                   ;; Gnulib tests.
                                   " test-fdutimensat"
                                   " test-futimens"
                                   " test-linkat"
                                   " test-renameat"
                                   " test-renameatu"
                                   " test-utimensat")))
            '())
      #:phases (modify-phases %standard-phases
                 (add-before 'build 'patch-shell-references
                   (lambda _
                     ;; 'split' uses either $SHELL or /bin/sh.  Set $SHELL so
                     ;; that tests pass, since /bin/sh isn't in the chroot.
                     (setenv "SHELL" (which "sh"))

                     (substitute* (find-files "gnulib-tests" "\\.c$")
                       (("/bin/sh") (which "sh")))
                     (substitute* (find-files "tests" "\\.sh$")
                       (("#!/bin/sh") (string-append "#!" (which "sh"))))))
                 (add-after 'unpack 'remove-tests
                   (lambda _
                     ,@(if (system-hurd?)
                           '((substitute*
                                 ;; These tests hang
                                 '("tests/cp/sparse-to-pipe.sh"
                                   "tests/split/fail.sh"
                                   ;; These tests error
                                   "tests/dd/nocache.sh"
                                   ;; These tests fail
                                   "tests/cp/sparse.sh"
                                   "tests/cp/special-f.sh"
                                   "tests/dd/bytes.sh"
                                   "tests/dd/stats.sh"
                                   "tests/ls/dangle.sh"
                                   "tests/ls/follow-slink.sh"
                                   "tests/ls/hyperlink.sh"
                                   "tests/ls/infloop.sh"
                                   "tests/ls/inode.sh"
                                   "tests/ls/selinux-segfault.sh"
                                   "tests/misc/env-S.pl"
                                   "tests/misc/factor-parallel.sh"
                                   "tests/misc/ls-misc.pl"
                                   "tests/misc/nice.sh"
                                   "tests/misc/pwd-long.sh"
                                   "tests/misc/shred-passes.sh"
                                   "tests/misc/stat-slash.sh"
                                   "tests/rm/fail-eperm.xpl"
                                   "tests/split/filter.sh")
                               (("^#!.*" all)
                                (string-append all "exit 77;\n")))
                             (substitute* "gnulib-tests/Makefile.in"
                               ;; This test sometimes fails and sometimes
                               ;; passes, but it does this consistently, so
                               ;; there might be some environmental factor
                               ;; here
                               ((" test-tls\\$\\(EXEEXT\\) ") " ")))
                           '())
                     ,@(if (system-hurd64?)
                           '((substitute*
                                 ;; These tests fail
                                 '("tests/misc/sort-NaN-infloop.sh"
                                   "tests/misc/wc-parallel.sh")
                               (("^#!.*" all)
                                (string-append all "exit 77;\n")))
                             (substitute* '("gnulib-tests/test-fdutimensat.c"
                                            "gnulib-tests/test-futimens.c"
                                            "gnulib-tests/test-linkat.c"
                                            "gnulib-tests/test-renameat.c"
                                            "gnulib-tests/test-renameatu.c"
                                            "gnulib-tests/test-utimensat.c")
                               (("(^| )main *\\(.*" all)
                                (string-append all "{\n  exit (77);//"))))
                           '())
                     ;; These tests can fail on btrfs.
                     (substitute* "tests/cp/reflink-auto.sh"
                       (("^#!.*" all)
                        (string-append all "exit 77;\n")))
                     (substitute* "Makefile.in"
                       ;; fails on filesystems where inotify cannot be used,
                       ;; more info in #47935
                       (("^ *tests/tail-2/inotify-dir-recreate.sh.*") "")))))))
   (synopsis "Core GNU utilities (file, text, shell)")
   (description
    "GNU Coreutils package includes all of the basic command-line tools that
are expected in a POSIX system, excluding shell.  This package is the union of
the GNU fileutils, sh-utils, and textutils packages.  Most of these tools
offer extended functionality beyond that which is outlined in the POSIX
standard.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/coreutils/")))

(define-public coreutils-minimal
  ;; Coreutils without its optional dependencies.
  (package
    (inherit coreutils)
    (name "coreutils-minimal")
    (outputs '("out"))
    (native-inputs '())
    (inputs '())))

(define-public coreutils-8.30
  ;; XXX: This version is kept just so we can run PRoot tests.
  (hidden-package
   (package
     (inherit coreutils-minimal)
     (version "8.30")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/coreutils/coreutils-"
                                   version ".tar.xz"))
               (sha256
                (base32
                 "0mxhw43d4wpqmvg0l4znk1vm10fy92biyh90lzdnqjcic2lb6cg8"))))
     (arguments
      (substitute-keyword-arguments (package-arguments coreutils-minimal)
        ((#:phases phases '%standard-phases)
         `(modify-phases ,phases
            (add-before 'check 'disable-broken-test
              (lambda _
                ;; This test hits the 127 character shebang limit in the build
                ;; environment due to the way "env -S" splits arguments into
                ;; shebangs.  Note that "env-S-script.sh" works around this
                ;; specific issue, but "env-S.pl" is not adjusted for build
                ;; environments with long prefixes (/tmp/guix-build-...).
                (substitute* "Makefile"
                  (("^.*tests/misc/env-S.pl.*$") "")))))))))))

(define-public gnu-make
  (package
   (name "make")
   (version "4.4.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/make/make-" version
                                ".tar.gz"))
            (sha256
             (base32
              "1cwgcmwdn7gqn5da2ia91gkyiqs9birr10sy5ykpkaxzcwfzn5nx"))
            (patches (search-patches "make-impure-dirs.patch"))))
   (build-system gnu-build-system)
   (native-inputs (list pkg-config))              ;to detect Guile
   (inputs (list guile-3.0))
   (outputs '("out" "debug"))
   (arguments
    `(,@(if (target-hurd?)
            '(#:configure-flags '("CFLAGS=-D__alloca=alloca"
                                  "ac_cv_func_posix_spawn=no"))
            '())
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'set-default-shell
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Change the default shell from /bin/sh.
            (let ((bash (assoc-ref inputs "bash")))
              (substitute* "src/job.c"
                (("default_shell =.*$")
                 (format #f "default_shell = \"~a/bin/sh\";\n"
                         bash)))))))))
   (synopsis "Remake files automatically")
   (description
    "Make is a program that is used to control the production of
executables or other files from their source files.  The process is
controlled from a Makefile, in which the developer specifies how each file is
generated from its source.  It has powerful dependency resolution and the
ability to determine when files have to be regenerated after their sources
change.  GNU make offers many powerful extensions over the standard utility.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/make/")))

(define-public gnu-make-4.2
  (package
    (inherit gnu-make)
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/make/make-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "12f5zzyq2w56g95nni65hc0g5p7154033y2f3qmjvd016szn5qnn"))))
    (arguments
     `(#:configure-flags '("CFLAGS=-D__alloca=alloca -D__stat=stat")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-default-shell
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Change the default shell from /bin/sh.
             (let ((bash (assoc-ref inputs "bash")))
               (substitute* "job.c"
                 (("default_shell =.*$")
                  (format #f "default_shell = \"~a/bin/sh\";\n"
                          bash)))))))))))

(define-public binutils
  (package
    (name "binutils")
    (version "2.44")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/binutils/binutils-"
                           version ".tar.bz2"))
       (sha256
        (base32 "0fnwaasfglbphqzvz5n25js9gl695p7pjbmb1z81g8gsc6k90qzn"))
       (patches (search-patches
                 "binutils-2.41-fix-cross.patch"
                 "binutils-loongson-workaround.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:out-of-source? #t          ;recommended in the README
           #:configure-flags
           #~'( ;; Add `-static-libgcc' to not retain a dependency
               ;; on GCC when bootstrapping.
               "LDFLAGS=-static-libgcc"

               ;; Turn on --enable-new-dtags by default to make the
               ;; linker set RUNPATH instead of RPATH on binaries.
               ;; This is important because RUNPATH can be overriden
               ;; using LD_LIBRARY_PATH at runtime.
               "--enable-new-dtags"

               ;; Don't search under /usr/lib & co.
               "--with-lib-path=/no-ld-lib-path"

               ;; Install BFD.  It ends up in a hidden directory,
               ;; but it's here.
               "--enable-install-libbfd"

               ;; Make sure 'ar' and 'ranlib' produce archives in a
               ;; deterministic fashion.
               "--enable-deterministic-archives"

               "--enable-64-bit-bfd"
               "--enable-compressed-debug-sections=all"
               "--enable-lto"
               "--enable-separate-code"
               "--enable-threads")

           ;; For some reason, the build machinery insists on rebuilding .info
           ;; files, even though they're already provided by the tarball.
           #:make-flags #~'("MAKEINFO=true")))
    (native-inputs (list bison))        ;needed to build 'gprofng'
    (synopsis "Binary utilities: bfd gas gprof ld")
    (description
     "GNU Binutils is a collection of tools for working with binary files.
Perhaps the most notable are \"ld\", a linker, and \"as\", an assembler.
Other tools include programs to display binary profiling information, list
the strings in a binary file, and utilities for working with archives.  The
\"bfd\" library for working with executable and object formats is also
included.")
    (license gpl3+)
    (home-page "https://www.gnu.org/software/binutils/")))

;; FIXME: ath9k-firmware-htc-binutils.patch do not apply on 2.34 because of a
;; big refactoring of xtensa-modules.c (commit 567607c11fbf7105 upstream).
;; Keep this version around until the patch is updated.
(define-public binutils-2.33
  (package
   (inherit binutils)
   (version "2.33.1")
   (source (origin
             (inherit (package-source binutils))
             (uri (string-append "mirror://gnu/binutils/binutils-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1cmd0riv37bqy9mwbg6n3523qgr8b3bbm5kwj19sjrasl4yq9d0c"))
             (patches '())))
   (arguments
    (substitute-keyword-arguments (package-arguments binutils)
      ((#:make-flags _ #~'()) #~'())))
   (native-inputs '())
   (properties '())))

(define-public binutils-gold
  (package
    (inherit binutils)
    (name "binutils-gold")
    (version "2.44")
    (source
     (origin
       (inherit (package-source binutils))
       (uri (string-append "mirror://gnu/binutils/binutils-with-gold-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1j64m2czn9ygd5g1cjjcw7q43b18xh6qkrdl1pkm03ncjnj3wwrl"))))
    (arguments
     (substitute-keyword-arguments (package-arguments binutils)
       ((#:configure-flags flags)
        #~(cons* "--enable-gold=default"
                 (delete "LDFLAGS=-static-libgcc" #$flags)))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
           (add-after 'patch-source-shebangs 'patch-more-shebangs
             (lambda _
               (substitute* "gold/Makefile.in"
                 (("/bin/sh") (which "sh")))))
           ;; Multiple failing tests on some architectures in the gold testsuite.
           #$@(if (or (target-arm?)
                      (target-ppc32?))
                  #~((add-after 'unpack 'skip-gold-testsuite
                       (lambda _
                         (substitute* "gold/Makefile.in"
                           ((" testsuite") " ")))))
                  #~())))))
    (native-inputs (modify-inputs (package-native-inputs binutils)
                     (append bc)))))

(define-public libbfd
  (package/inherit binutils
    (name "libbfd")
    (inputs
     (modify-inputs (package-native-inputs binutils)
       (append texinfo))) ; because makeinfo is needed when building bfd alone
    (arguments
     (substitute-keyword-arguments (package-arguments binutils)
       ;; Only build as a shared library
       ((#:configure-flags flags)
        #~(append #$flags '("--enable-shared" "--disable-static")))
       ;; Only build and install bfd
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda _
                (invoke "make" "-j" (number->string (parallel-job-count))
                        "all-bfd")))
            (replace 'check
              (lambda _
                (invoke "make" "check-bfd"
                        "-j" (number->string (parallel-job-count))
                        "MAKEINFO=true")))
            (replace 'install
              (lambda _ (invoke "make" "install-bfd")))))))
    (synopsis "GNU BFD library for operating on object files")
    (description "This package provides a standalone shared library version of
BFD, which is otherwise distributed and installed as part of the Binutils
package release.")))

(define* (make-ld-wrapper name #:key
                          (target (const #f))
                          binutils
                          (linker "ld")
                          (guile (canonical-package guile-3.0))
                          (bash (canonical-package bash))
                          (guile-for-build guile))
  "Return a package called NAME that contains a wrapper for the 'ld' program
of BINUTILS, which adds '-rpath' flags to the actual 'ld' command line.  The
wrapper uses GUILE and BASH.

TARGET must be a one-argument procedure that, given a system type, returns a
cross-compilation target triplet or #f.  When the result is not #f, make a
wrapper for the cross-linker for that target, called 'TARGET-ld'.  To use a
different linker than the default \"ld\", such as \"ld.gold\" the linker name
can be provided via the LINKER argument."
  ;; Note: #:system->target-triplet is a procedure so that the evaluation of
  ;; its result can be delayed until the 'arguments' field is evaluated, thus
  ;; in a context where '%current-system' is accurate.
  (package
    (name name)
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (inputs `(("binutils" ,binutils)
              ("guile"    ,guile)
              ("bash"     ,bash)
              ("wrapper"  ,(search-path %load-path
                                        "gnu/packages/ld-wrapper.in"))))
    (arguments
     (let ((target (target (%current-system))))
       `(#:guile ,guile-for-build
         #:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils)
                                  (system base compile))

                     (let* ((out (assoc-ref %outputs "out"))
                            (bin (string-append out "/bin"))
                            (ld  ,(if target
                                      `(string-append bin "/" ,target "-"
                                                      ,linker)
                                      `(string-append bin "/" ,linker)))
                            (go  (string-append ld ".go")))

                       (setvbuf (current-output-port)
                                (cond-expand (guile-2.0 _IOLBF)
                                             (else 'line)))
                       (format #t "building ~s/bin/ld wrapper in ~s~%"
                               (assoc-ref %build-inputs "binutils")
                               out)

                       (mkdir-p bin)
                       (copy-file (assoc-ref %build-inputs "wrapper") ld)
                       (substitute* ld
                         (("@SELF@")
                          ld)
                         (("@GUILE@")
                          (string-append (assoc-ref %build-inputs "guile")
                                         "/bin/guile"))
                         (("@BASH@")
                          (string-append (assoc-ref %build-inputs "bash")
                                         "/bin/bash"))
                         (("@LD@")
                          (string-append (assoc-ref %build-inputs "binutils")
                                         ,(if target
                                              (string-append "/bin/"
                                                             target "-" linker)
                                              (string-append "/bin/" linker)))))
                       (chmod ld #o555)
                       (compile-file ld #:output-file go))))))
    (synopsis "The linker wrapper")
    (description
     "The linker wrapper (or @code{ld-wrapper}) wraps the linker to add any
missing @code{-rpath} flags, and to detect any misuse of libraries outside of
the store.")
    (home-page "https://www.gnu.org/software/guix//")
    (license gpl3+)))

(define %glibc-patches
  (list "glibc-ldd-powerpc.patch"
        "glibc-2.41-ldd-x86_64.patch"
        "glibc-2.40-dl-cache.patch"
        "glibc-2.37-versioned-locpath.patch"
        ;; "glibc-allow-kernel-2.6.32.patch"
        "glibc-reinstate-prlimit64-fallback.patch"
        "glibc-supported-locales.patch"
        "glibc-2.37-hurd-clock_t_centiseconds.patch"
        "glibc-2.41-hurd-local-clock_gettime_MONOTONIC.patch"
        "glibc-hurd-mach-print.patch"
        "glibc-hurd-gettyent.patch"
        "glibc-hurd-getauxval.patch"
        "glibc-hurd-pthread_setcancelstate.patch"
        "glibc-hurd-2.41-pthread-once.patch"
        "glibc-hurd-2.41-pthread-sigmask.patch"
        "glibc-hurd-2.41-symlink.patch"
        "glibc-hurd64-intr-msg-clobber.patch"
        "glibc-hurd64-gcc-14.2-tls-bug.patch"))

(define-public glibc
  ;; This is the GNU C Library, used on GNU/Linux and GNU/Hurd.  Prior to
  ;; version 2.28, GNU/Hurd used a different glibc branch.
  (package
   (name "glibc")
   (version "2.41")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/glibc/glibc-" version ".tar.xz"))
            (sha256
             (base32
              "00g95047sshv0zxk9ja3mi7lzwi8wh8qx0nxngbvgmj5yli6p8m5"))
            (patches (map search-patch %glibc-patches))))
   (properties `((lint-hidden-cve . ("CVE-2024-2961"
                                     "CVE-2024-33601" "CVE-2024-33602"
                                     "CVE-2024-33600" "CVE-2024-33599"))))
   (build-system gnu-build-system)

   ;; Glibc's <limits.h> refers to <linux/limit.h>, for instance, so glibc
   ;; users should automatically pull Linux headers as well.  On GNU/Hurd,
   ;; libc provides <hurd.h>, which includes a bunch of Hurd and Mach headers,
   ;; so both should be propagated.
   (propagated-inputs
    (if (target-hurd?)
        `(("hurd-core-headers" ,hurd-core-headers))
        `(("kernel-headers" ,linux-libre-headers))))

   (outputs '("out" "debug"
              "static"))                          ;9 MiB of .a files

   (arguments
    `(#:out-of-source? #t

      ;; The libraries have an empty RUNPATH, but some, such as the versioned
      ;; libraries (libdl-2.24.so, etc.) have ld.so marked as NEEDED.  Since
      ;; these libraries are always going to be found anyway, just skip
      ;; RUNPATH checks.
      #:validate-runpath? #f

      #:modules ((ice-9 ftw)
                 (srfi srfi-1)
                 (srfi srfi-26)
                 (guix build utils)
                 (guix build gnu-build-system))

      ;; Strip binaries but preserve the symbol table needed by Valgrind:
      ;; <https://lists.gnu.org/archive/html/help-guix/2022-03/msg00036.html>.
      #:strip-flags '("--strip-debug")

      #:configure-flags
      (list "--sysconfdir=/etc"

            ;; Installing a locale archive with all the locales is to
            ;; expensive (~100 MiB), so we rely on users to install the
            ;; locales they really want.
            ;;
            ;; Set the default locale path.  In practice, $LOCPATH may be
            ;; defined to point whatever locales users want.  However, setuid
            ;; binaries don't honor $LOCPATH, so they'll instead look into
            ;; $libc_cv_complocaledir; we choose /run/current-system/locale/X.Y,
            ;; with the idea that it is going to be populated by the sysadmin.
            ;; The "X.Y" sub-directory is because locale data formats are
            ;; incompatible across libc versions; see
            ;; <https://lists.gnu.org/archive/html/guix-devel/2015-08/msg00737.html>.
            ;;
            ;; `--localedir' is not honored, so work around it.
            ;; See <http://sourceware.org/ml/libc-alpha/2013-03/msg00093.html>.
            (string-append "libc_cv_complocaledir=/run/current-system/locale/"
                           ,(version-major+minor version))

            (string-append "--with-headers="
                           (assoc-ref ,(if (%current-target-system)
                                           '%build-target-inputs
                                           '%build-inputs)
                                      "kernel-headers")
                           "/include")

            ;; This is the default for most architectures as of GNU libc 2.26,
            ;; but we specify it explicitly for clarity and consistency.  See
            ;; "kernel-features.h" in the GNU libc for details.
            "--enable-kernel=3.2.0"

            ;; Use our Bash instead of /bin/sh.
            (string-append "BASH_SHELL="
                           (assoc-ref %build-inputs "bash")
                           "/bin/bash")

            ;; On GNU/Hurd we get discarded-qualifiers warnings for
            ;; 'device_write_inband' among other things.  Ignore them.
            ,@(if (target-hurd?)
                  `("--disable-werror")
                  '()))

      #:tests? #f                                 ; XXX
      #:phases (modify-phases %standard-phases
                 (add-before
                  'configure 'pre-configure
                  (lambda* (#:key inputs native-inputs outputs
                                  #:allow-other-keys)
                    (let* ((out  (assoc-ref outputs "out"))
                           (bin  (string-append out "/bin"))
                           ;; FIXME: Normally we would look it up only in INPUTS
                           ;; but cross-base uses it as a native input.
                           (bash (or (assoc-ref inputs "static-bash")
                                     (assoc-ref native-inputs "static-bash"))))
                      ;; Install the rpc data base file under `$out/etc/rpc'.
                      (substitute* "inet/Makefile"
                        (("^\\$\\(inst_sysconfdir\\)/rpc(.*)$" _ suffix)
                         (string-append out "/etc/rpc" suffix "\n"))
                        (("^install-others =.*$")
                         (string-append "install-others = " out "/etc/rpc\n")))

                      (substitute* "Makeconfig"
                        ;; According to
                        ;; <http://www.linuxfromscratch.org/lfs/view/stable/chapter05/glibc.html>,
                        ;; linking against libgcc_s is not needed with GCC
                        ;; 4.7.1.
                        ((" -lgcc_s") ""))

                      ;; Tell the ld.so cache code where the store is.
                      (substitute* "elf/dl-cache.c"
                        (("@STORE_DIRECTORY@")
                         (string-append "\"" (%store-directory) "\"")))

                      ;; Have `system' use that Bash.
                      (substitute* "sysdeps/posix/system.c"
                        (("#define[[:blank:]]+SHELL_PATH.*$")
                         (format #f "#define SHELL_PATH \"~a/bin/bash\"\n"
                                 bash)))

                      ;; Same for `popen'.
                      (substitute* "libio/iopopen.c"
                        (("/bin/sh")
                         (string-append bash "/bin/sh")))

                      ;; Same for the shell used by the 'exec' functions for
                      ;; scripts that lack a shebang.
                      (substitute* (find-files "." "^paths\\.h$")
                        (("#define[[:blank:]]+_PATH_BSHELL[[:blank:]].*$")
                         (string-append "#define _PATH_BSHELL \""
                                        bash "/bin/sh\"\n")))

                      ;; Make sure we don't retain a reference to the
                      ;; bootstrap Perl.
                      (substitute* "malloc/mtrace.pl"
                        (("^#!.*")
                         ;; The shebang can be omitted, because there's the
                         ;; "bilingual" eval/exec magic at the top of the file.
                         "")
                        (("exec @PERL@")
                         "exec perl")))))

                 (add-after 'install 'move-static-libs
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Move static libraries to the "static" output.
                     ;; Note: As of GNU libc 2.34, the contents of some ".a"
                     ;; files have been moved into "libc.so", and *both* empty
                     ;; ".so" and ".a" files have been introduced to avoid
                     ;; breaking existing executables and existing builds
                     ;; respectively.  The intent of the seemingly redundant
                     ;; empty ".a" files is to avoid newly-compiled executables
                     ;; from having dependencies on the empty shared libraries,
                     ;; and as such, it is useful to have these ".a" files in
                     ;; OUT in addition to STATIC.

                     (define (empty-static-library? file)
                       ;; Return true if FILE is an 'ar' archive with nothing
                       ;; beyond the header.
                       (let ((file (string-append (assoc-ref outputs "out")
                                                  "/lib/" file)))
                         (and (ar-file? file)
                              (= (stat:size (stat file)) 8))))

                     (define (static-library? file)
                       ;; Return true if FILE is a static library.  The
                       ;; "_nonshared.a" files are referred to by libc.so,
                       ;; libpthread.so, etc., which are in fact linker
                       ;; scripts.
                       (and (string-suffix? ".a" file)
                            (not (string-contains file "_nonshared"))
                            (not (empty-static-library? file))))

                     (define (linker-script? file)
                       ;; Guess whether FILE, a ".a" file, is actually a
                       ;; linker script.
                       (and (not (ar-file? file))
                            (not (elf-file? file))))

                     (let* ((out    (assoc-ref outputs "out"))
                            (lib    (string-append out "/lib"))
                            (files  (scandir lib static-library?))
                            (empty  (scandir lib empty-static-library?))
                            (static (assoc-ref outputs "static"))
                            (slib   (string-append static "/lib")))
                       (mkdir-p slib)
                       (for-each (lambda (base)
                                   (rename-file (string-append lib "/" base)
                                                (string-append slib "/" base)))
                                 files)
                       (for-each (lambda (base)
                                   (copy-file (string-append lib "/" base)
                                              (string-append slib "/" base)))
                                 empty)

                       ;; Usually libm.a is a linker script so we need to
                       ;; change the file names in there to refer to STATIC
                       ;; instead of OUT.
                       (for-each (lambda (ld-script)
                                   (substitute* ld-script
                                     ((out) static)))
                                 (filter linker-script?
                                         (map (cut string-append slib "/" <>)
                                              files))))))

                 (add-after 'install 'install-utf8-c-locale
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Install the C.UTF-8 locale so there's always a UTF-8
                     ;; locale around.
                     (let* ((out (assoc-ref outputs "out"))
                            (bin (string-append out "/bin"))
                            (locale (string-append out "/lib/locale/"
                                                   ,(package-version
                                                     this-package))))
                       (mkdir-p locale)

                       ;; FIXME: When cross-compiling, attempt to use
                       ;; 'localedef' from the same libc version.
                       (invoke ,(if (%current-target-system)
                                    "true"
                                    '(string-append bin "/localedef"))
                               "--no-archive" "--prefix" locale
                               "-i" "C" "-f" "UTF-8"
                               (string-append locale "/C.UTF-8")))))

                 ,@(if (target-hurd?)
                       `((add-after 'install 'augment-libc.so
                           (lambda* (#:key outputs #:allow-other-keys)
                             (let ((out (assoc-ref outputs "out")))
                               (substitute* (string-append out "/lib/libc.so")
                                 (("/[^ ]+/lib/libc.so.0.3")
                                  (string-append out "/lib/libc.so.0.3"
                                                 " libmachuser.so libhurduser.so"))))))
                         (add-after 'install 'create-machine-symlink
                           (lambda* (#:key outputs #:allow-other-keys)
                             (let* ((out (assoc-ref outputs "out"))
                                    (cpu ,(match (or (%current-target-system)
                                                     (%current-system))
                                            ((? target-x86-32?)
                                             "i386")
                                            ((? target-x86-64?)
                                             "x86_64")))
                                    (machine (string-append
                                              out "/include/mach/machine")))
                               (unless (file-exists? machine)
                                 (symlink cpu machine))))))
                       '()))))

   (inputs `(("static-bash" ,static-bash)))

   ;; To build the manual, we need Texinfo and Perl.  Gettext is needed to
   ;; install the message catalogs, with 'msgfmt'.
   (native-inputs `(("texinfo" ,texinfo)
                    ("perl" ,perl)
                    ("bison" ,bison)
                    ("gettext" ,gettext-minimal)
                    ("python" ,python-minimal)

                    ,@(if (target-hurd?)
                          `(("mig" ,mig)
                            ("perl" ,perl))
                          '())))

   (native-search-paths
    ;; Search path for packages that provide locale data.  This is useful
    ;; primarily in build environments.  Use 'GUIX_LOCPATH' rather than
    ;; 'LOCPATH' to avoid interference with the host system's libc on foreign
    ;; distros.
    (list (search-path-specification
           (variable "GUIX_LOCPATH")
           (files '("lib/locale")))
          $TZDIR))

   (synopsis "The GNU C Library")
   (description
    "Any Unix-like operating system needs a C library: the library which
defines the \"system calls\" and other basic facilities such as open, malloc,
printf, exit...

The GNU C library is used as the C library in the GNU system and most systems
with the Linux kernel.")
   (license lgpl2.0+)
   (home-page "https://www.gnu.org/software/libc/")))

;; Define a variation of glibc which uses the default /etc/ld.so.cache, useful
;; in FHS containers.
(define-public glibc-for-fhs
  (hidden-package
   (package/inherit glibc
     (name "glibc-for-fhs")
     (source (origin (inherit (package-source glibc))
                     ;; Remove Guix's patch to read ld.so.cache from /gnu/store
                     ;; directories, re-enabling the default /etc/ld.so.cache
                     ;; behavior.
                     (patches
                      (delete (search-patch "glibc-2.40-dl-cache.patch")
                              (origin-patches (package-source glibc)))))))))

;; Below are old libc versions, which we use mostly to build locale data in
;; the old format (which the new libc cannot cope with.)

(define-public glibc-2.39
  (package
    (inherit glibc)
    (version "2.39")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/glibc/glibc-" version ".tar.xz"))
              (sha256
               (base32
                "09nrwb0ksbah9k35jchd28xxp2hidilqdgz7b8v5f30pz1yd8yzp"))
              (patches (search-patches "glibc-2.39-git-updates.patch"
                                       "glibc-ldd-powerpc.patch"
                                       "glibc-2.38-ldd-x86_64.patch"
                                       "glibc-dl-cache.patch"
                                       "glibc-2.37-versioned-locpath.patch"
                                       ;; "glibc-allow-kernel-2.6.32.patch"
                                       "glibc-reinstate-prlimit64-fallback.patch"
                                       "glibc-supported-locales.patch"
                                       "glibc-2.37-hurd-clock_t_centiseconds.patch"
                                       ;; "glibc-2.37-hurd-local-clock_gettime_MONOTONIC.patch"
                                       "glibc-hurd-mach-print.patch"
                                       "glibc-hurd-gettyent.patch"
                                       "glibc-hurd-getauxval.patch"))))))

(define-public glibc-2.35
  (package
    (inherit glibc)
    (version "2.35")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/glibc/glibc-" version ".tar.xz"))
              (sha256
               (base32
                "0bpm1kfi09dxl4c6aanc5c9951fmf6ckkzay60cx7k37dcpp68si"))
              (patches (search-patches "glibc-2.35-CVE-2023-4911.patch"
                                       "glibc-ldd-powerpc.patch"
                                       "glibc-ldd-x86_64.patch"
                                       "glibc-dl-cache.patch"
                                       "glibc-versioned-locpath.patch"
                                       "glibc-allow-kernel-2.6.32.patch"
                                       "glibc-reinstate-prlimit64-fallback.patch"
                                       "glibc-supported-locales.patch"
                                       "glibc-cross-objdump.patch"
                                       "glibc-cross-objcopy.patch" ;must come 2nd
                                       "glibc-hurd-clock_t_centiseconds.patch"
                                       "glibc-hurd-clock_gettime_monotonic.patch"
                                       "glibc-hurd-mach-print.patch"
                                       "glibc-hurd-gettyent.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments glibc)
       ((#:configure-flags flags #~'())
        #~(cons* "CFLAGS=-g -O2 -Wno-error=builtin-declaration-mismatch"
                 "--enable-crypt"
                 ;; We do not want to use the C++ compiler, because its
                 ;; libstdc++ is linked against a newer glibc, and so relies
                 ;; on those newer symbols.  Pretend it doesn't link (the test
                 ;; doesn't actually check that the compiler works with new
                 ;; libstdc++ and older glibc).
                 "libc_cv_cxx_link_ok=no"
                 #$flags))
       ((#:phases phases)
        ;; The C.UTF-8 fails to build in glibc 2.35:
        ;; <https://sourceware.org/bugzilla/show_bug.cgi?id=28861>.
        ;; It is missing altogether in versions earlier than 2.35.
        `(modify-phases ,phases
           (delete 'install-utf8-c-locale)))))))

(define-public glibc-2.33
  (package
    (inherit glibc-2.35)
    (name "glibc")
    (version "2.33")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/glibc/glibc-" version ".tar.xz"))
              (sha256
               (base32
                "1zvp0qdfbdyqrzydz18d9zg3n5ygy8ps7cmny1bvsp8h1q05c99f"))
              (patches
               (cons (search-patch "glibc-2.33-riscv64-miscompilation.patch")
                     ;; Remove patches that are irrelevant or do not apply to
                     ;; this version.
                     (remove
                      (lambda (patch)
                        (member (basename patch)
                                '("glibc-2.35-CVE-2023-4911.patch"
                                  "glibc-hurd-clock_gettime_monotonic.patch")))
                             (origin-patches (package-source glibc-2.35)))))))
    (arguments
     (substitute-keyword-arguments (package-arguments glibc)
       ((#:configure-flags flags #~'())
        #~(cons* #$(string-append
                    "CFLAGS=-g -O2"
                    " -Wno-error=builtin-declaration-mismatch"
                    " -Wno-error=format-overflow"
                    " -Wno-error=stringop-overflow"
                    " -Wno-error=use-after-free")
                 "--enable-crypt"
                 ;; We do not want to use the C++ compiler, because its
                 ;; libstdc++ is linked against a newer glibc, and so relies
                 ;; on those newer symbols.  Pretend it doesn't link (the test
                 ;; doesn't actually check that the compiler works with new
                 ;; libstdc++ and older glibc).
                 "libc_cv_cxx_link_ok=no"
                 #$flags))
       ((#:phases phases)
        `(modify-phases ,phases
           ;; This phase fails trying to create /etc/ld.so.cache
           (delete 'install-utf8-c-locale)))))))

(define-public (make-gcc-libc base-gcc libc)
  "Return a GCC that targets LIBC."
  (package (inherit base-gcc)
           (name (string-append (package-name base-gcc) "-"
                                (package-name libc) "-"
                                (package-version libc)))
           (arguments
            (ensure-keyword-arguments (package-arguments base-gcc)
                                      '(#:implicit-inputs? #f)))
           (native-inputs
            `(,@(package-native-inputs base-gcc)
              ,@(append (fold alist-delete (%final-inputs) '("libc" "libc:static")))
              ("libc" ,libc)
              ("libc:static" ,libc "static")))))

(define-public (make-glibc-locales glibc)
  (package
    (inherit glibc)
    (name "glibc-locales")
    (source (origin (inherit (package-source glibc))
                    (patches (cons (search-patch "glibc-locales.patch")
                                   (origin-patches (package-source glibc))))))
    (synopsis "All the locales supported by the GNU C Library")
    (description
     "This package provides all the locales supported by the GNU C Library,
more than 400 in total.  To use them set the @code{LOCPATH} environment variable
to the @code{share/locale} sub-directory of this package.")
    (outputs '("out"))                            ;110+ MiB
    (native-search-paths '())
    (arguments
     (let ((args `(#:tests? #f #:strip-binaries? #f
                   ,@(package-arguments glibc))))
       (substitute-keyword-arguments args
         ((#:modules modules '((guix build utils)
                               (guix build gnu-build-system)))
          `((srfi srfi-11)
            (gnu build locale)
            ,@modules))
         ((#:imported-modules modules '())
          `((gnu build locale)
            ,@%default-gnu-imported-modules))
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'build
                (lambda _
                  (invoke "make" "localedata/install-locales"
                          "-j" (number->string (parallel-job-count)))))
              (add-after 'build 'symlink-normalized-codesets
                (lambda* (#:key outputs #:allow-other-keys)
                  ;; The above phase does not install locales with names using
                  ;; the "normalized codeset."  Thus, create symlinks like:
                  ;;   en_US.utf8 -> en_US.UTF-8
                  (define (locale-directory? file stat)
                    (and (file-is-directory? file)
                         (string-index (basename file) #\_)
                         (string-rindex (basename file) #\.)))

                  (let* ((locales (find-files #$output locale-directory?
                                              #:directories? #t)))
                    (for-each (lambda (directory)
                                (let*-values (((base)
                                               (basename directory))
                                              ((name codeset)
                                               (locale->name+codeset base))
                                              ((normalized)
                                               (normalize-codeset codeset)))
                                  (unless (string=? codeset normalized)
                                    (symlink base
                                             (string-append (dirname directory)
                                                            "/" name "."
                                                            normalized)))))
                              locales))))
              (delete 'install)
              (delete 'install-utf8-c-locale)
              (delete 'move-static-libs)))
         ((#:configure-flags flags)
          #~(append #$flags
                    ;; Use $(libdir)/locale/X.Y as is the case by default.
                    (list (string-append "libc_cv_complocaledir="
                                         #$output
                                         "/lib/locale/"
                                         #$(version-major+minor
                                            (package-version glibc)))))))))
    (properties `((upstream-name . "glibc")))))

(define %default-utf8-locales
  ;; These are the locales commonly used for tests---e.g., in Guile's i18n
  ;; tests.
  '("C" "de_DE" "el_GR" "en_US" "fr_FR" "tr_TR"))

(define*-public (make-glibc-utf8-locales glibc #:key
                                         (locales %default-utf8-locales)
                                         (name "glibc-utf8-locales"))
  (define default-locales?
    (equal? locales %default-utf8-locales))

  (package
    (name name)
    (version (package-version glibc))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))

               (let* ((libc      (dirname
                                  (search-input-file %build-inputs
                                                     "/bin/localedef")))
                      (gzip      (dirname
                                  (search-input-file %build-inputs
                                                     "/bin/gzip")))
                      (out       #$output)
                      (localedir (string-append out "/lib/locale/"
                                                #$(version-major+minor
                                                   (package-version this-package)))))
                 ;; 'localedef' needs 'gzip'.
                 (setenv "PATH" (string-append libc ":" gzip ""))

                 (mkdir-p localedir)
                 (for-each (lambda (locale)
                             (define file
                               ;; Use the "normalized codeset" by
                               ;; default--e.g., "en_US.utf8".
                               (string-append localedir "/" locale ".utf8"))

                             (invoke "localedef" "--no-archive"
                                     "--prefix" localedir
                                     "-i" locale
                                     "-f" "UTF-8" file)

                             ;; For backward compatibility with Guix
                             ;; <= 0.8.3, add "xx_YY.UTF-8".
                             (symlink (string-append locale ".utf8")
                                      (string-append localedir "/"
                                                     locale ".UTF-8")))

                           ;; The C.UTF-8 locale was introduced in 2.35 but it
                           ;; fails to build there:
                           ;; <https://sourceware.org/bugzilla/show_bug.cgi?id=28861>.
                           '#$(if (version>? (package-version this-package)
                                             "2.35")
                                  locales
                                  (delete "C" locales)))))))
    (native-inputs (list glibc gzip))
    (synopsis (if default-locales?
                  (P_ "Small sample of UTF-8 locales")
                  (P_ "Customized sample of UTF-8 locales")))
    (description
     (if default-locales?
         (P_ "This package provides a small sample of UTF-8 locales mostly useful in
test environments.")
         (format #f (P_ "This package provides the following UTF-8 locales:
@itemize
~{@item ~a~%~}
@end itemize~%")
                 locales)))
    (home-page (package-home-page glibc))
    (license (package-license glibc))))

(define-public glibc-locales
  (make-glibc-locales glibc))
(define-public glibc-utf8-locales
  (hidden-package
   (make-glibc-utf8-locales glibc)))

;; Packages provided to ease use of binaries linked against the previous libc.
(define-public glibc-locales-2.35
  (package (inherit (make-glibc-locales glibc-2.35))
           (name "glibc-locales-2.35")))
(define-public glibc-locales-2.33
  (package (inherit (make-glibc-locales glibc-2.33))
           (name "glibc-locales-2.33")))

(define-public which
  (package
    (name "which")
    (version "2.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/which/which-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1bgafvy3ypbhhfznwjv1lxmd6mci3x1byilnnkc7gcr486wlb8pl"))))
    (build-system gnu-build-system)
    (home-page "https://gnu.org/software/which/")
    (synopsis "Find full path of shell commands")
    (description
     "The which program finds the location of executables in PATH, with a
variety of options.  It is an alternative to the shell \"type\" built-in
command.")
    (license gpl3+))) ; some files are under GPLv2+

(define-public glibc/hurd glibc)

(define-public glibc/hurd-headers
  (package/inherit glibc/hurd
    (name "glibc-hurd-headers")
    (outputs '("out"))
    (propagated-inputs (list gnumach-headers hurd-headers))
    (native-inputs
     (modify-inputs (package-native-inputs glibc/hurd)
       (prepend (if (%current-target-system)
                    (let* ((cross-base (resolve-interface '(gnu packages cross-base)))
                           (cross-mig (module-ref cross-base 'cross-mig)))
                      (cross-mig (%current-target-system)))
                    mig))))
    (arguments
     (substitute-keyword-arguments (package-arguments glibc/hurd)
       ;; We just pass the flags really needed to build the headers.
       ((#:configure-flags flags)
        `(list "--enable-add-ons"))
       ((#:phases _)
        '(modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "make" "install-headers")

               ;; Make an empty stubs.h to work around not being able to
               ;; produce a valid stubs.h and causing the build to fail. See
               ;; <http://lists.gnu.org/archive/html/guix-devel/2014-04/msg00233.html>.
               (let ((out (assoc-ref outputs "out")))
                 (close-port
                  (open-output-file
                   (string-append out "/include/gnu/stubs.h"))))))
           (delete 'build)))))                  ; nothing to build
    (supported-systems %hurd-systems)))

(define-public glibc-utf8-locales/hurd
  ;; Locales for the libc version used on GNU/Hurd.
  (hidden-package
   (make-glibc-utf8-locales glibc/hurd)))

(define* (libc-for-target #:optional
                          (target (or (%current-target-system)
                                      (%current-system))))
  (match target
    ((? target-hurd?)
     glibc/hurd)
    (_
     glibc)))

(define-public glibc-locales/hurd
  ;; Locales again; hide them because their 'supported-systems' field suggests
  ;; they're Hurd-only, making them non-installable on GNU/Linux.
  (hidden-package
   (make-glibc-locales glibc/hurd)))

(define* (libc-locales-for-target #:optional
                                  (target (or (%current-target-system)
                                              (%current-system))))
  (if (target-hurd? target)
      glibc-locales/hurd
      glibc-locales))

(define* (libc-utf8-locales-for-target #:optional
                                       (target (or (%current-target-system)
                                                   (%current-system))))
  "Return the glibc UTF-8 locale package for TARGET."
  ;; Note: To avoid circular dependencies (such as: texinfo ->
  ;; glibc-utf8-locales -> glibc -> texinfo), refer to
  ;; 'glibc-utf8-locales-final' via 'canonical-package'.
  (canonical-package
   (if (target-hurd? target)
       glibc-utf8-locales/hurd
       glibc-utf8-locales)))

(define-public tzdata
  (package
    (name "tzdata")
    ;; This package should be kept in sync with python-pytz and python-tzdata
    ;; in (gnu packages time).
    (version "2025a")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://data.iana.org/time-zones/releases/tzdata"
                   version ".tar.gz"))
             (sha256
              (base32
               "1l7hnlgc4wgy2gmaj5zmswpzbyq23h7vsndnw2zhwibw5k3wnpsd"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f

           ;; This consists purely of (architecture-independent) data, so
           ;; ‘cross-compilation’ is pointless here!  (The binaries zic,
           ;; dump, and tzselect are deleted in the post-install phase.)
           #:target #f

           #:make-flags
           #~(let ((out #$output)
                   (tmp (getenv "TMPDIR")))
               (list (string-append "TOPDIR=" out)
                     (string-append "TZDIR=" out "/share/zoneinfo")
                     (string-append "TZDEFAULT=" out
                                    "/share/zoneinfo/localtime")

                     ;; Likewise for the C library routines.
                     (string-append "LIBDIR=" tmp "/lib")
                     (string-append "MANDIR=" tmp "/man")

                     ;; XXX: tzdata 2020b changed the on-disk format
                     ;; of the time zone files from 'fat' to 'slim'.
                     ;; Many packages (particularly evolution-data-server)
                     ;; can not yet handle the latter, so we stick with
                     ;; 'fat' for now.
                     #$@(if (version>=? (package-version this-package)
                                        "2020b")
                            '("CPPFLAGS=-DZIC_BLOAT_DEFAULT='\"fat\"'")
                            '())

                     "AWK=awk"
                     "CC=gcc"))
           #:modules '((guix build utils)
                       (guix build gnu-build-system)
                       (srfi srfi-1))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source inputs #:allow-other-keys)
                   (invoke "tar" "xvf" source)
                   (invoke "tar" "xvf"
                           #$(match (package-inputs this-package)
                               (((_ tzcode)) tzcode)))))
               (add-after 'install 'post-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Move data in the right place.
                   (let ((out (assoc-ref outputs "out")))
                     ;; Discard zic, dump, and tzselect, already
                     ;; provided by glibc.
                     (delete-file-recursively (string-append out "/usr"))
                     (symlink (string-append out "/share/zoneinfo")
                              (string-append out "/share/zoneinfo/posix"))
                     (delete-file-recursively
                      (string-append out "/share/zoneinfo-posix"))
                     (copy-recursively (string-append out "/share/zoneinfo-leaps")
                                       (string-append out "/share/zoneinfo/right"))
                     (delete-file-recursively
                      (string-append out "/share/zoneinfo-leaps")))))
               (delete 'configure))))
    (inputs (list (origin
                    (method url-fetch)
                    (uri (string-append
                          "https://data.iana.org/time-zones/releases/tzcode"
                          version ".tar.gz"))
                    (sha256
                     (base32
                      "0qahpwp1zlyvi7qrlm4r74pmj3c7sx3jlg9xw2siwj3nkzapk5hi")))))
    (home-page "https://www.iana.org/time-zones")
    (synopsis "Database of current and historical time zones")
    (description "The Time Zone Database (often called tz or zoneinfo)
contains code and data that represent the history of local time for many
representative locations around the globe.  It is updated periodically to
reflect changes made by political bodies to time zone boundaries, UTC offsets,
and daylight-saving rules.")
    (license public-domain)))

;;; A "fixed" version of tzdata, which is used in the test suites of glib and R
;;; and a few other places. We can update this whenever we are able to rebuild
;;; thousands of packages (for example, in a core-updates rebuild). This package
;;; will typically be obsolete and should never be referred to by a built
;;; package.
;;;
;;; Please make this a hidden-package if it is different from the primary tzdata
;;; package.
(define-public tzdata-for-tests tzdata)

;;; TODO: Move the 'install-leap-seconds' phase into the main package's
;;; 'post-install' phase on the next rebuild cycle.
(define-public tzdata/leap-seconds
  (hidden-package
    (package/inherit tzdata
      (arguments
        (substitute-keyword-arguments (package-arguments tzdata)
          ((#:phases phases)
           #~(modify-phases #$phases
               (add-after 'post-install 'install-leap-seconds
                 (lambda _
                   (install-file "leap-seconds.list"
                     (string-append #$output "/share/zoneinfo")))))))))))

(define-public libiconv
  (package
    (name "libiconv")
    (version "1.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/libiconv/libiconv-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1s35d4kk7abfqrcicjzy5s9gpip6dzyhn157a7qq5sxlz7sga21v"))
              (modules '((guix build utils)))
              (snippet
               ;; Work around "declared gets" error on glibc systems (fixed by
               ;; Gnulib commit 66712c23388e93e5c518ebc8515140fa0c807348.)
               '(substitute* "srclib/stdio.in.h"
                  (("^#undef gets") "")
                  (("^_GL_WARN_ON_USE \\(gets.*") "")))))
    (build-system gnu-build-system)
    (synopsis "Character set conversion library")
    (description
     "libiconv provides an implementation of the iconv function for systems
that lack it.  iconv is used to convert between character encodings in a
program.  It supports a wide variety of different encodings.")
    (home-page "https://www.gnu.org/software/libiconv/")
    (license lgpl3+)))

(define* (libiconv-if-needed #:optional (target (%current-target-system)))
  "Return either a libiconv package specification to include in a dependency
list for platforms that have an incomplete libc, or the empty list.  If a
package needs iconv ,@(libiconv-if-needed) should be added."
  ;; POSIX C libraries provide iconv.  Platforms with an incomplete libc
  ;; without iconv, such as MinGW, must return the then clause.
  (if (target-mingw? target)
      (list libiconv)
      '()))

;;; Beware: canonical-package should not be used at the top level, to avoid
;;; eagerly resolving (gnu packages commencement), which would introduce
;;; circular module dependencies.
(define (canonical-package package)
  ;; Avoid circular dependency by lazily resolving 'commencement'.
  (let* ((iface (resolve-interface '(gnu packages commencement)))
         (proc  (module-ref iface 'canonical-package)))
    (proc package)))

;;; Beware: %final-inputs should not be used at the top level, to avoid
;;; eagerly resolving (gnu packages commencement), which would introduce
;;; circular module dependencies.
(define* (%final-inputs #:optional (system (%current-system)))
  "Return the list of \"final inputs\"."
  ;; Avoid circular dependency by lazily resolving 'commencement'.
  (let ((iface (resolve-interface '(gnu packages commencement))))
    ((module-ref iface '%final-inputs) system)))

;;; base.scm ends here
