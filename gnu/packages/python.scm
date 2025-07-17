;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013-2018, 2021, 2023, 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2017, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Omar Radwan <toxemicsquire4@gmail.com>
;;; Copyright © 2015 Pierre-Antoine Rault <par@rigelk.eu>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2015-2020, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015, 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016, 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Daniel Pimentel <d4n1@d4n1.org>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016, 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2016, 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016–2018, 2021, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017, 2018, 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017, 2018 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2017, 2018, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2018 Ethan R. Jones <ethanrjones97@gmail.com
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Vijayalakshmi Vedantham <vijimay12@gmail.com>
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2016, 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018, 2019, 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Luther Thompson <lutheroto@gmail.com>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019, 2024 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020, 2023, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
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

(define-module (gnu packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (customize-site
            guix-pythonpath-search-path))

(define* (customize-site version)
  "Generate a install-sitecustomize.py phase, using VERSION."
  `(lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
     (let* ((out (assoc-ref outputs "out"))
            (site-packages (string-append
                            out "/lib/python"
                            ,(version-major+minor version)
                            "/site-packages"))
            (sitecustomize.py (assoc-ref (or native-inputs inputs)
                                         "sitecustomize.py"))
            (dest (string-append site-packages "/sitecustomize.py")))
       (mkdir-p site-packages)
       (copy-file sitecustomize.py dest)
       ;; Set the correct permissions on the installed file, else the byte
       ;; compilation phase fails with a permission denied error.
       (chmod dest #o644))))

(define (guix-pythonpath-search-path version)
  "Generate a GUIX_PYTHONPATH search path specification, using VERSION."
  (search-path-specification (variable "GUIX_PYTHONPATH")
                             (files (list (string-append
                                           "lib/python"
                                           (version-major+minor version)
                                           "/site-packages")))))

(define-public python-2.7
  (package
    (name "python2")
    (version "2.7.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.python.org/ftp/python/"
                           version "/Python-" version ".tar.xz"))
       (sha256
        (base32
         "0hzgxl94hnflis0d6m4szjx0b52gah7wpmcg5g00q7am6xwhwb5n"))
       (patches (search-patches "python-2.7-search-paths.patch"
                                "python-2-deterministic-build-info.patch"
                                "python-2.7-site-prefixes.patch"
                                "python-2.7-source-date-epoch.patch"
                                "python-2.7-adjust-tests.patch"
                                "python-cross-compile.patch"
                                "python-2.7-CVE-2021-3177.patch"
                                "python-2.7-expat-compat.patch"
                                "python-2.7-no-static-lib.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Ensure the bundled copies of these libraries are not used.
           (for-each delete-file-recursively
                     '("Modules/_ctypes/libffi" "Modules/expat" "Modules/zlib"))

           (substitute* "Modules/Setup.dist"
             ;; Link Expat instead of embedding the bundled one.
             (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))

           ;; Suboptimal to delete failing tests here, but if we delete them in
           ;; the arguments then we need to make sure to strip out that phase
           ;; when it gets inherited by python and python-minimal.
           (for-each delete-file
                     '("Lib/test/test_compileall.py"
                       "Lib/test/test_ctypes.py" ; fails on mips64el
                       "Lib/test/test_distutils.py"
                       "Lib/test/test_import.py"
                       "Lib/test/test_shutil.py"
                       "Lib/test/test_socket.py"
                       "Lib/test/test_subprocess.py"))))))
    (outputs '("out"
               "tk"                     ;tkinter; adds 50 MiB to the closure
               "idle"))                 ;programming environment; weighs 5MB
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:configure-flags
      #~(list "--enable-shared"          ;allow embedding
              "--with-system-expat"      ;for XML support
              "--with-system-ffi"        ;build ctypes
              "--with-ensurepip=install" ;install pip and setuptools
              "--with-computed-gotos"    ;main interpreter loop optimization
              "--enable-unicode=ucs4"

              ;; FIXME: These flags makes Python significantly faster, but
              ;; leads to non-reproducible binaries.
              ;; "--with-lto"               ;increase size by 20MB, but 15% speedup
              ;; "--enable-optimizations"

              ;; Prevent the installed _sysconfigdata.py from retaining a reference
              ;; to coreutils.
              "INSTALL=install -c"
              "MKDIR_P=mkdir -p"

              ;; Disable runtime check failing if cross-compiling, see:
              ;; https://lists.yoctoproject.org/pipermail/poky/2013-June/008997.html
              #$@(if (%current-target-system)
                     #~("ac_cv_buggy_getaddrinfo=no"
                        "ac_cv_file__dev_ptmx=no"
                        "ac_cv_file__dev_ptc=no")
                     #~())
              ;; -fno-semantic-interposition reinstates some optimizations by gcc
              ;; leading to around 15% speedup. This is the default starting from
              ;; python 3.10.
              "CFLAGS=-fno-semantic-interposition"
              (string-append "LDFLAGS=-Wl,-rpath="
                             (assoc-ref %outputs "out") "/lib"
                             " -fno-semantic-interposition")
              ;; Add a reference to libxcrypt in LIBS so that the sysconfigdata
              ;; file records it and propagates it to programs linking against
              ;; Python.
              (let ((libxcrypt
                     (false-if-exception
                      (dirname
                       (search-input-file %build-inputs
                                          "lib/libcrypt.so.1")))))
                (string-append
                 "LIBS="
                 (if libxcrypt
                     (string-append "-L" libxcrypt)
                     ""))))
      ;; With no -j argument tests use all available cpus, so provide one.
      #:make-flags
      #~(list (string-append
               (format #f "TESTOPTS=-j~d" (parallel-job-count))
               ;; Exclude the following tests as they fail
               ;; non-deterministically with "error: [Errno 104] Connection
               ;; reset by peer."  Python 3 seems unaffected.  A potential fix,
               ;; yet to be backported to Python 2, is available at:
               ;; https://github.com/python/cpython/commit/529525fb5a8fd9b96ab4021311a598c77588b918.
               " --exclude test_urllib2_localnet test_httplib"))

      #:modules '((ice-9 ftw)
                  (ice-9 match)
                  (guix build utils)
                  (guix build gnu-build-system))
      #:phases
      #~(modify-phases %standard-phases
          (add-before
              'configure 'patch-lib-shells
            (lambda _
              ;; This variable is used in setup.py to enable cross compilation
              ;; specific switches. As it is not set properly by configure
              ;; script, set it manually.
              #$@(if (%current-target-system)
                     #~((setenv "_PYTHON_HOST_PLATFORM" ""))
                     #~())
              ;; Filter for existing files, since some may not exist in all
              ;; versions of python that are built with this recipe.
              (substitute* (filter file-exists?
                                   '("Lib/subprocess.py"
                                     "Lib/popen2.py"
                                     "Lib/distutils/tests/test_spawn.py"
                                     "Lib/test/support/__init__.py"
                                     "Lib/test/test_subprocess.py"))
                (("/bin/sh") (which "sh")))))
          #$@(if (system-hurd?)
                 #~((add-before 'build 'patch-regen-for-hurd
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((libc (assoc-ref inputs "libc")))
                          (substitute* "Lib/plat-generic/regen"
                            (("/usr/include/") (string-append libc "/include/")))))))
                 #~())
          (add-before 'configure 'do-not-record-configure-flags
            (lambda* (#:key configure-flags #:allow-other-keys)
              ;; Remove configure flags from the installed '_sysconfigdata.py'
              ;; and 'Makefile' so we don't end up keeping references to the
              ;; build tools.
              ;;
              ;; Preserve at least '--with-system-ffi' since otherwise the
              ;; thing tries to build libffi, fails, and we end up with a
              ;; Python that lacks ctypes.
              (substitute* "configure"
                (("^CONFIG_ARGS=.*$")
                 (format #f "CONFIG_ARGS='~a'\n"
                         (if (member "--with-system-ffi" configure-flags)
                             "--with-system-ffi"
                             ""))))))
          (add-before 'check 'pre-check
            (lambda _
              ;; 'Lib/test/test_site.py' needs a valid $HOME
              (setenv "HOME" (getcwd))))
          (add-after 'unpack 'set-source-file-times-to-1980
            ;; XXX One of the tests uses a ZIP library to pack up some of the
            ;; source tree, and fails with "ZIP does not support timestamps
            ;; before 1980".  Work around this by setting the file times in the
            ;; source tree to sometime in early 1980.
            (lambda _
              (let ((circa-1980 (* 10 366 24 60 60)))
                (ftw "." (lambda (file stat flag)
                           (utime file circa-1980 circa-1980)
                           #t)))))
          (add-after 'install 'remove-tests
            ;; Remove 25 MiB of unneeded unit tests.  Keep test_support.*
            ;; because these files are used by some libraries out there.
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (match (scandir (string-append out "/lib")
                                (lambda (name)
                                  (string-prefix? "python" name)))
                  ((pythonX.Y)
                   (let ((testdir (string-append out "/lib/" pythonX.Y
                                                 "/test")))
                     (with-directory-excursion testdir
                       (for-each delete-file-recursively
                                 (scandir testdir
                                          (match-lambda
                                            ((or "." "..") #f)
                                            ("support" #f)
                                            (file
                                             (not
                                              (string-prefix? "test_support."
                                                              file))))))
                       (call-with-output-file "__init__.py" (const #t))))
                   (let ((libdir (string-append out "/lib/" pythonX.Y)))
                     (for-each
                      (lambda (directory)
                        (let ((dir (string-append libdir "/" directory)))
                          (when (file-exists? dir)
                            (delete-file-recursively dir))))
                      '("email/test" "ctypes/test" "unittest/test" "tkinter/test"
                        "sqlite3/test" "bsddb/test" "lib-tk/test" "json/tests"
                        "distutils/tests"))))))))
          (add-after 'install 'add-libxcrypt-reference-pkgconfig
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (define out (assoc-ref outputs "out"))
              (define libxcrypt
                (false-if-exception
                 (dirname (search-input-file inputs "lib/libcrypt.so.1"))))
              (when libxcrypt
                (substitute*
                    (find-files (string-append out "/lib/pkgconfig")
                                ".*\\.pc")
                  (("-lcrypt")
                   (string-append "-L" libxcrypt " -lcrypt"))))))
          (add-after 'remove-tests 'move-tk-inter
            (lambda* (#:key outputs #:allow-other-keys)
              ;; When Tkinter support is built move it to a separate output so
              ;; that the main output doesn't contain a reference to Tcl/Tk.
              (let ((out (assoc-ref outputs "out"))
                    (tk  (assoc-ref outputs "tk")))
                (when tk
                  (match (find-files out "tkinter.*\\.so")
                    ((tkinter.so)
                     ;; The .so is in OUT/lib/pythonX.Y/lib-dynload, but we
                     ;; want it under TK/lib/pythonX.Y/site-packages.
                     (let* ((len    (string-length out))
                            (target (string-append
                                     tk "/"
                                     (string-drop
                                      (dirname (dirname tkinter.so))
                                      len)
                                     "/site-packages")))
                       (install-file tkinter.so target)
                       (delete-file tkinter.so))))))))
          (add-after 'move-tk-inter 'move-idle
            (lambda* (#:key outputs #:allow-other-keys)
              ;; when idle is built, move it to a separate output to save some
              ;; space (5MB)
              (let ((out (assoc-ref outputs "out"))
                    (idle (assoc-ref outputs "idle")))
                (when idle
                  (for-each
                   (lambda (file)
                     (let ((target (string-append idle "/bin/" (basename file))))
                       (install-file file (dirname target))
                       (delete-file file)))
                   (find-files (string-append out "/bin") "^idle"))
                  (match (find-files out "^idlelib$" #:directories? #t)
                    ((idlelib)
                     (let* ((len (string-length out))
                            (target (string-append idle "/"
                                                   (string-drop idlelib len)
                                                   "/site-packages")))
                       (mkdir-p (dirname target))
                       (rename-file idlelib target))))))))
          (add-after 'move-idle 'rebuild-bytecode
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                ;; Disable hash randomization to ensure the generated .pycs
                ;; are reproducible.
                (setenv "PYTHONHASHSEED" "0")
                (for-each
                 (lambda (output)
                   (for-each (lambda (opt)
                               (format #t "Compiling with optimization level: ~a\n"
                                       (if (null? opt) "none" (car opt)))
                               (apply invoke
                                      `(,#$(if (%current-target-system)
                                               "python2"
                                               #~(string-append out "/bin/python"))
                                        ,@opt
                                        "-m" "compileall"
                                        "-f" ; force rebuild
                                        ;; Don't build lib2to3, because it contains
                                        ;; Python 3 code.
                                        "-x" "lib2to3/.*"
                                        ,output)))
                             ;; Python 2 has a single file extension (.pyo) for the
                             ;; chosen level of optimization, so it doesn't make
                             ;; sense to byte compile with more than one level.
                             (list '() '("-OO"))))
                 (map cdr outputs)))))
          (add-after 'install 'install-sitecustomize.py
            #$(customize-site version)))))
    (inputs
     (list bzip2
           expat
           gdbm
           libffi        ; for ctypes
           libxcrypt     ; crypto module slated for removal in 3.13, re-enable
                                        ; python tests of libxcrypt when that happens
           sqlite                       ; for sqlite extension
           openssl-1.1
           readline
           zlib
           tcl
           tk))                         ; for tkinter
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("sitecustomize.py" ,(local-file (search-auxiliary-file
                                         "python/sitecustomize.py")))
       ;; When cross-compiling, a native version of Python itself is needed.
       ,@(if (%current-target-system)
             `(("python2" ,this-package)
               ("which" ,which))
             '())))
    (native-search-paths
     (list (guix-pythonpath-search-path version)))
    (home-page "https://www.python.org")
    (synopsis "High-level, dynamically-typed programming language")
    (description
     "Python is a remarkably powerful dynamic programming language that
is used in a wide variety of application domains.  Some of its key
distinguishing features include: clear, readable syntax; strong
introspection capabilities; intuitive object orientation; natural
expression of procedural code; full modularity, supporting hierarchical
packages; exception-based error handling; and very high level dynamic
data types.")
    (properties '((cpe-name . "python")))
    (license license:psfl)))

;; Current 2.x version.
(define-public python-2 python-2.7)

(define-public python-3.10
  (package
    (inherit python-2)
    (name "python")
    (version "3.10.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "python-3-arm-alignment.patch"
                        "python-3-deterministic-build-info.patch"
                        "python-3-fix-tests.patch"
                        "python-3-hurd-configure.patch"
                        "python-3-reproducible-build.patch"
                        "python-3-search-paths.patch"))
              (sha256
               (base32
                "0j6wvh2ad5jjq5n7sjmj1k66mh6lipabavchc3rb4vsinwaq9vbf"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete the bundled copy of libexpat.
                  (delete-file-recursively "Modules/expat")
                  (substitute* "Modules/Setup"
                    ;; Link Expat instead of embedding the bundled one.
                    (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))
                  ;; Delete windows binaries
                  (for-each delete-file
                            (find-files "Lib/distutils/command" "\\.exe$"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-2)
       ((#:configure-flags flags)
        #~(append #$flags
                  ;; XXX Use quote to avoid world rebuild at this time
                  '("--without-static-libpython")))
       ((#:make-flags _)
        #~(list (string-append
                 (format #f "TESTOPTS=-j~d" (parallel-job-count))
                 ;; test_mmap fails on low-memory systems
                 " --exclude test_mmap test_socket"
                 #$@(if (system-hurd?)
                        #~(" test_posix" ;multiple errors
                           " test_time"
                           " test_pty"
                           " test_shutil"
                           " test_tempfile" ;chflags: invalid argument:
                                        ;  tbv14c9t/dir0/dir0/dir0/test0.txt
                           " test_asyncio" ;runs over 10min
                           " test_os"      ;stty: 'standard input':
                                        ;  Inappropriate ioctl for device
                           " test_openpty" ;No such file or directory
                           " test_selectors" ;assertEqual(NUM_FDS // 2, len(fds))
                                        ;  32752 != 4
                           " test_compileall" ;multiple errors
                           " test_poll"       ;list index out of range
                           " test_subprocess" ;runs over 10min
                           " test_asyncore"   ;multiple errors
                           " test_threadsignals"
                           " test_eintr" ;Process return code is -14
                           " test_io"    ;multiple errors
                           " test_logging"
                           " test_signal"
                           " test_threading" ;runs over 10min
                           " test_flags"     ;ERROR
                           " test_bidirectional_pty"
                           " test_create_unix_connection"
                           " test_unix_sock_client_ops"
                           " test_open_unix_connection"
                           " test_open_unix_connection_error"
                           " test_read_pty_output"
                           " test_write_pty"
                           " test_concurrent_futures"        ;freeze
                           " test_venv"                      ;freeze
                           " test_multiprocessing_forkserver" ;runs over 10min
                           " test_multiprocessing_spawn"      ;runs over 10min
                           " test_builtin"
                           " test_capi"
                           " test_dbm_ndbm"
                           " test_exceptions"
                           " test_faulthandler"
                           " test_getopt"
                           " test_importlib"
                           " test_json"
                           " test_multiprocessing_fork"
                           " test_multiprocessing_main_handling"
                           " test_pdb "
                           " test_regrtest"
                           " test_sqlite")
                        #~()))))
       ((#:phases phases)
        #~(modify-phases #$phases
            #$@(if (system-hurd?)
                   #~((delete 'patch-regen-for-hurd) ;regen was removed after 3.5.9
                      (add-after 'unpack 'disable-multi-processing
                        (lambda _
                          (substitute* "Makefile.pre.in"
                            (("-j0") "-j1")))))
                   #~())
            (add-after 'unpack 'remove-vendored-wheel-content
              (lambda _
                ;; Delete .exe from embedded .whl (zip) files
                (for-each
                 (lambda (whl)
                   (let ((dir "whl-content")
                         (circa-1980 (* 10 366 24 60 60)))
                     (mkdir-p dir)
                     (with-directory-excursion dir
                       (let ((whl (string-append "../" whl)))
                         (invoke "unzip" whl)
                         (for-each delete-file
                                   (find-files "." "\\.exe$"))
                         (delete-file whl)

                         ;; Search for cacert.pem, delete it, and rewrite the
                         ;; file which directs python to look for it.
                         (let ((cacert (find-files "." "cacert\\.pem")))
                           (unless (null? cacert)
                             (let ((certifi (dirname (car cacert))))
                               (delete-file (string-append certifi "/cacert.pem"))
                               (delete-file (string-append certifi "/core.py"))
                               (with-output-to-file (string-append certifi "/core.py")
                                 (lambda _
                                   (display "\"\"\"
certifi.py
~~~~~~~~~~
This file is a Guix-specific version of core.py.

This module returns the installation location of SSL_CERT_FILE or
/etc/ssl/certs/ca-certificates.crt, or its contents.
\"\"\"
import os

_CA_CERTS = None

try:
    _CA_CERTS = os.environ [\"SSL_CERT_FILE\"]
except:
    _CA_CERTS = os.path.join(\"/etc\", \"ssl\", \"certs\", \"ca-certificates.crt\")

def where() -> str:
    return _CA_CERTS

def contents() -> str:
    with open(where(), \"r\", encoding=\"ascii\") as data:
        return data.read()"))))))

                         ;; Reset timestamps to prevent them from ending
                         ;; up in the Zip archive.
                         (ftw "." (lambda (file stat flag)
                                    (utime file circa-1980 circa-1980)
                                    #t))
                         (apply invoke "zip" "-X" whl
                                (find-files "." #:directories? #t))))
                     (delete-file-recursively dir)))
                 (find-files "Lib/ensurepip" "\\.whl$"))))
            (add-before 'check 'set-TZDIR
              (lambda* (#:key inputs native-inputs #:allow-other-keys)
                ;; test_email requires the Olson time zone database.
                (setenv "TZDIR"
                        (string-append (assoc-ref
                                        (or native-inputs inputs) "tzdata")
                                       "/share/zoneinfo"))))
            (replace 'rebuild-bytecode
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  ;; Disable hash randomization to ensure the generated .pycs
                  ;; are reproducible.
                  (setenv "PYTHONHASHSEED" "0")

                  (for-each (lambda (output)
                              ;; XXX: Delete existing pycs generated by the build
                              ;; system beforehand because the -f argument does
                              ;; not necessarily overwrite all files, leading to
                              ;; indeterministic results.
                              (for-each (lambda (pyc)
                                          (delete-file pyc))
                                        (find-files output "\\.pyc$"))

                              (apply invoke
                                     `(,#$(if (%current-target-system)
                                              "python3"
                                              #~(string-append out
                                                               "/bin/python3"))
                                       "-m" "compileall"
                                       "-o" "0" "-o" "1" "-o" "2"
                                       "-f" ; force rebuild
                                       "--invalidation-mode=unchecked-hash"
                                       ;; Don't build lib2to3, because it's
                                       ;; Python 2 code.
                                       "-x" "lib2to3/.*"
                                       ,output)))
                            (map cdr outputs)))))
            (replace 'install-sitecustomize.py
              #$(customize-site version))))))
    (inputs
     (modify-inputs (package-inputs python-2.7)
       (replace "openssl" openssl)))
    (native-inputs
     (let ((inputs (modify-inputs (package-native-inputs python-2)
                     (prepend tzdata-for-tests
                              unzip
                              (@ (gnu packages compression) zip)))))
       (if (%current-target-system)
           (modify-inputs inputs (prepend this-package))
           inputs)))
    (native-search-paths
     (list (guix-pythonpath-search-path version)
           ;; Used to locate tzdata by the zoneinfo module introduced in
           ;; Python 3.9.
           (search-path-specification
            (variable "PYTHONTZPATH")
            (files (list "share/zoneinfo")))))))

(define-public python-3.11
  (package
    (inherit python-3.10)
    (name "python")
    (version "3.11.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "python-3-deterministic-build-info.patch"
                        "python-3.11-fix-tests.patch"
                        "python-3-hurd-configure.patch"
                        "python-3-search-paths.patch"))
              (sha256
               (base32
                "1qrvsxg5g0b0pgz2iigxic2j3g6b2c59iva46vins8ydl33j169a"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete the bundled copy of libexpat.
                  (delete-file-recursively "Modules/expat")
                  (substitute* "Modules/Setup"
                    ;; Link Expat instead of embedding the bundled one.
                    (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))
                  ;; Delete windows binaries
                  (for-each delete-file
                            (find-files "Lib/distutils/command" "\\.exe$"))))))
    (arguments
     (list
      #:test-target "test"
      #:configure-flags
      #~(list "--enable-shared"          ;allow embedding
              "--with-system-expat"      ;for XML support
              "--with-system-ffi"        ;build ctypes
              "--with-ensurepip=install" ;install pip and setuptools
              "--with-computed-gotos"    ;main interpreter loop optimization
              "--enable-unicode=ucs4"
              "--without-static-libpython"
              "--enable-loadable-sqlite-extensions"

              ;; FIXME: These flags makes Python significantly faster,
              ;; but leads to non-reproducible binaries.
              ;; "--with-lto"   ;increase size by 20MB, but 15% speedup
              ;; "--enable-optimizations"

              ;; Prevent the installed _sysconfigdata.py from retaining
              ;; a reference to coreutils.
              "INSTALL=install -c"
              "MKDIR_P=mkdir -p"

              ;; Add a reference to libxcrypt in LIBS so that the
              ;; sysconfigdata file records it and propagates it to programs
              ;; linking against Python.
              (let ((libxcrypt
                     (false-if-exception
                      (dirname
                       (search-input-file %build-inputs
                                          "lib/libcrypt.so.1")))))
                (string-append
                 "LIBS="
                 (if libxcrypt
                     (string-append "-L" libxcrypt)
                     "")))

              ;; Disable runtime check failing if cross-compiling, see:
              ;; https://lists.yoctoproject.org/pipermail/poky/2013-June/008997.html
              #$@(if (%current-target-system)
                     #~("ac_cv_buggy_getaddrinfo=no"
                        "ac_cv_file__dev_ptmx=no"
                        "ac_cv_file__dev_ptc=no"
                        (string-append "--with-build-python="
                                       #+(this-package-native-input "python")
                                       "/bin/python3"))
                     '())
              ;; -fno-semantic-interposition reinstates some
              ;; optimizations by gcc leading to around 15% speedup.
              ;; This is the default starting from python 3.10.
              "CFLAGS=-fno-semantic-interposition"
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"
                             " -fno-semantic-interposition"))
      ;; With no -j argument tests use all available cpus, so provide one.
      #:make-flags
      `(list (string-append (format #f "TESTOPTS=-j~d"
                                    (parallel-job-count))
                            ;; those tests fail on low-memory systems
                            " --exclude"
                            " test_mmap"
                            " test_socket"
                            " test_threading"
                            " test_asyncio"
                            " test_shutdown"
                            ,@(if (system-hurd?)
                                  '(" test_posix" ;multiple errors
                                    " test_time"
                                    " test_pty"
                                    " test_shutil"
                                    " test_tempfile" ;chflags: invalid argument:
                                    ;; tbv14c9t/dir0/dir0/dir0/test0.txt
                                    " test_os" ;stty: 'standard input':
                                    ;; Inappropriate ioctl for device
                                    " test_openpty" ;No such file or directory
                                    " test_selectors" ;assertEqual(NUM_FDS // 2, len(fds))
                                    ;; 32752 != 4
                                    " test_compileall" ;multiple errors
                                    " test_poll" ;list index out of range
                                    " test_subprocess" ;runs over 10min
                                    " test_asyncore"   ;multiple errors
                                    " test_threadsignals"
                                    " test_eintr" ;Process return code is -14
                                    " test_io"    ;multiple errors
                                    " test_logging"
                                    " test_signal"
                                    " test_flags" ;ERROR
                                    " test_bidirectional_pty"
                                    " test_create_unix_connection"
                                    " test_unix_sock_client_ops"
                                    " test_open_unix_connection"
                                    " test_open_unix_connection_error"
                                    " test_read_pty_output"
                                    " test_write_pty"
                                    " test_concurrent_futures" ;freeze
                                    " test_venv"               ;freeze
                                    " test_multiprocessing_forkserver" ;runs over 10min
                                    " test_multiprocessing_spawn" ;runs over 10min
                                    " test_glob" ;did not finish in 10h
                                    " test_site" ;Invalid argument
                                    " test_termios" ;os.openpty() Operation not permitted
                                    " test_tty" ;os.openpty() Operation not permitted
                                    " test_sqlite3" ;; sqlite3.OperationalError: database is locked
                                    " test_builtin"
                                    " test_capi"
                                    " test_dbm_ndbm"
                                    " test_exceptions"
                                    " test_faulthandler"
                                    " test_getopt"
                                    " test_importlib"
                                    " test_json"
                                    " test_multiprocessing_fork"
                                    " test_multiprocessing_main_handling"
                                    " test_pdb "
                                    " test_regrtest"
                                    " test_sqlite")
                                  '())))

      #:modules
      '((ice-9 ftw)
        (ice-9 match)
        (guix build utils)
        (guix build gnu-build-system))

      #:phases
      #~(modify-phases %standard-phases
          #$@(if (system-hurd?)
                 `((add-after 'unpack
                       'disable-multi-processing
                     (lambda _
                       (substitute* "Makefile.pre.in"
                         (("-j0")
                          "-j1")))))
                 '())
          (add-before 'configure 'patch-lib-shells
            (lambda _
              ;; This variable is used in setup.py to enable cross compilation
              ;; specific switches. As it is not set properly by configure
              ;; script, set it manually.
              #$@(if (%current-target-system)
                     '((setenv "_PYTHON_HOST_PLATFORM" ""))
                     '())
              ;; Filter for existing files, since some may not exist in all
              ;; versions of python that are built with this recipe.
              (substitute* (filter file-exists?
                                   '("Lib/subprocess.py"
                                     "Lib/popen2.py"
                                     "Lib/distutils/tests/test_spawn.py"
                                     "Lib/test/support/__init__.py"
                                     "Lib/test/test_subprocess.py"))
                (("/bin/sh")
                 (which "sh")))))
          (add-before 'configure 'do-not-record-configure-flags
            (lambda* (#:key configure-flags #:allow-other-keys)
              ;; Remove configure flags from the installed '_sysconfigdata.py'
              ;; and 'Makefile' so we don't end up keeping references to the
              ;; build tools.
              ;;
              ;; Preserve at least '--with-system-ffi' since otherwise the
              ;; thing tries to build libffi, fails, and we end up with a
              ;; Python that lacks ctypes.
              (substitute* "configure"
                (("^CONFIG_ARGS=.*$")
                 (format #f "CONFIG_ARGS='~a'\n"
                         (if (member "--with-system-ffi"
                                     configure-flags)
                             "--with-system-ffi" ""))))))
          (add-before 'check 'pre-check
            (lambda _
              ;; 'Lib/test/test_site.py' needs a valid $HOME
              (setenv "HOME"
                      (getcwd))))
          (add-after 'unpack 'set-source-file-times-to-1980
            ;; XXX One of the tests uses a ZIP library to pack up some of the
            ;; source tree, and fails with "ZIP does not support timestamps
            ;; before 1980".  Work around this by setting the file times in the
            ;; source tree to sometime in early 1980.
            (lambda _
              (let ((circa-1980 (* 10 366 24 60 60)))
                (ftw "."
                     (lambda (file stat flag)
                       (utime file circa-1980 circa-1980) #t)))))
          (add-after 'unpack 'remove-windows-binaries
            (lambda _
              ;; Delete .exe from embedded .whl (zip) files
              (for-each (lambda (whl)
                          (let ((dir "whl-content")
                                (circa-1980 (* 10 366 24 60 60)))
                            (mkdir-p dir)
                            (with-directory-excursion dir
                              (let ((whl (string-append "../" whl)))
                                (invoke "unzip" whl)
                                (for-each delete-file
                                          (find-files "." "\\.exe$"))
                                (delete-file whl)
                                ;; Reset timestamps to prevent them from ending
                                ;; up in the Zip archive.
                                (ftw "."
                                     (lambda (file stat flag)
                                       (utime file circa-1980
                                              circa-1980) #t))
                                (apply invoke "zip" "-X" whl
                                       (find-files "."
                                                   #:directories? #t))))
                            (delete-file-recursively dir)))
                        (find-files "Lib/ensurepip" "\\.whl$"))))
          (add-after 'install 'remove-tests
            ;; Remove 25 MiB of unneeded unit tests.  Keep test_support.*
            ;; because these files are used by some libraries out there.
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (match (scandir (string-append out "/lib")
                                (lambda (name)
                                  (string-prefix? "python" name)))
                  ((pythonX.Y)
                   (let ((testdir (string-append out "/lib/" pythonX.Y
                                                 "/test")))
                     (with-directory-excursion testdir
                       (for-each delete-file-recursively
                                 (scandir testdir
                                          (match-lambda
                                            ((or "." "..")
                                             #f)
                                            ("support" #f)
                                            (file (not (string-prefix?
                                                        "test_support."
                                                        file))))))
                       (call-with-output-file "__init__.py"
                         (const #t))))
                   (let ((libdir (string-append out "/lib/" pythonX.Y)))
                     (for-each (lambda (directory)
                                 (let ((dir (string-append libdir "/"
                                                           directory)))
                                   (when (file-exists? dir)
                                     (delete-file-recursively dir))))
                               '("email/test" "ctypes/test"
                                 "unittest/test"
                                 "tkinter/test"
                                 "sqlite3/test"
                                 "bsddb/test"
                                 "lib-tk/test"
                                 "json/tests"
                                 "distutils/tests"))))))))
          (add-after 'remove-tests 'move-tk-inter
            (lambda* (#:key outputs inputs #:allow-other-keys)
              ;; When Tkinter support is built move it to a separate output so
              ;; that the main output doesn't contain a reference to Tcl/Tk.
              (let ((out (assoc-ref outputs "out"))
                    (tk (assoc-ref outputs "tk")))
                (when tk
                  (match (find-files out "tkinter.*\\.so")
                    ((tkinter.so)
                     ;; The .so is in OUT/lib/pythonX.Y/lib-dynload, but we
                     ;; want it under TK/lib/pythonX.Y/site-packages.
                     (let* ((len (string-length out))
                            (target (string-append tk "/"
                                                   (string-drop (dirname
                                                                 (dirname
                                                                  tkinter.so))
                                                                len)
                                                   "/site-packages")))
                       (install-file tkinter.so target)
                       (delete-file tkinter.so))))
                  ;; Remove explicit store path references.
                  (let ((tcl (assoc-ref inputs "tcl"))
                        (tk (assoc-ref inputs "tk")))
                    (substitute* (find-files (string-append out "/lib")
                                             "^(_sysconfigdata_.*\\.py|Makefile)$")
                      (((string-append "-L" tk "/lib"))
                       "")
                      (((string-append "-L" tcl "/lib"))
                       "")))))))
          (add-after 'move-tk-inter 'move-idle
            (lambda* (#:key outputs #:allow-other-keys)
              ;; when idle is built, move it to a separate output to save some
              ;; space (5MB)
              (let ((out (assoc-ref outputs "out"))
                    (idle (assoc-ref outputs "idle")))
                (when idle
                  (for-each (lambda (file)
                              (let ((target (string-append idle
                                                           "/bin/"
                                                           (basename
                                                            file))))
                                (install-file file
                                              (dirname target))
                                (delete-file file)))
                            (find-files (string-append out "/bin")
                                        "^idle"))
                  (match (find-files out "^idlelib$"
                                     #:directories? #t)
                    ((idlelib)
                     (let* ((len (string-length out))
                            (target (string-append idle "/"
                                                   (string-drop
                                                    idlelib len)
                                                   "/site-packages")))
                       (mkdir-p (dirname target))
                       (rename-file idlelib target))))))))
          (add-after 'move-idle 'rebuild-bytecode
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Disable hash randomization to ensure the generated .pycs
              ;; are reproducible.
              (setenv "PYTHONHASHSEED" "0")

              (for-each (lambda (output)
                          ;; XXX: Delete existing pycs generated by the build
                          ;; system beforehand because the -f argument does
                          ;; not necessarily overwrite all files, leading to
                          ;; indeterministic results.
                          (for-each (lambda (pyc)
                                      (delete-file pyc))
                                    (find-files output "\\.pyc$"))

                          (apply invoke
                                 #$(if (%current-target-system)
                                       "python3"
                                       #~(string-append #$output "/bin/python3"))
                                 `("-m" "compileall"
                                   "-o" "0"
                                   "-o" "1"
                                   "-o" "2"
                                   "-f" ;force rebuild
                                   "--invalidation-mode=unchecked-hash"
                                   ;; Don't build lib2to3, because it's Python
                                   ;; 2 code.
                                   "-x" "lib2to3/.*"
                                   ,output)))
                        (map cdr outputs))))
          (add-before 'check 'set-TZDIR
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              ;; test_email requires the Olson time zone database.
              (setenv "TZDIR"
                      (string-append (assoc-ref (or native-inputs
                                                    inputs) "tzdata")
                                     "/share/zoneinfo"))))
          (add-after 'install 'add-libxcrypt-reference-pkgconfig
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libxcrypt
                     (false-if-exception
                      (dirname
                       (search-input-file inputs "lib/libcrypt.so.1")))))
                (when libxcrypt
                  (substitute*
                      (find-files (string-append #$output "/lib/pkgconfig")
                                  ".*\\.pc")
                    (("Libs:")
                     (string-append "Libs: " "-L" libxcrypt " -lcrypt")))))))
          (add-after 'install 'install-sitecustomize.py
            #$(customize-site version)))))
    (inputs (list bzip2
                  expat
                  gdbm
                  libffi                ;for ctypes
                  libxcrypt             ;crypto module slated for removal in 3.13
                  sqlite                ;for sqlite extension
                  openssl
                  readline
                  zlib
                  tcl
                  tk))                  ;for tkinter
    (native-inputs `(("tzdata" ,tzdata-for-tests)
                     ("unzip" ,unzip)
                     ("zip" ,(@ (gnu packages compression) zip))
                     ("pkg-config" ,pkg-config)
                     ("sitecustomize.py" ,(local-file (search-auxiliary-file
                                                       "python/sitecustomize.py")))
                     ;; When cross-compiling, a native version of Python itself is needed.
                     ,@(if (%current-target-system)
                           `(("python" ,this-package)
                             ("which" ,which))
                           '())))
    (native-search-paths
     (list (guix-pythonpath-search-path version)
           ;; Used to locate tzdata by the zoneinfo module introduced in
           ;; Python 3.9.
           (search-path-specification
            (variable "PYTHONTZPATH")
            (files (list "share/zoneinfo")))))))

(define-public python-3.12
  (package
    (name "python-next")
    (version "3.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.python.org/ftp/python/" version
                           "/Python-" version ".tar.xz"))
       (sha256
        (base32 "0w6qyfhc912xxav9x9pifwca40b4l49vy52wai9j0gc1mhni2a5y"))
       (patches (search-patches "python-3-deterministic-build-info.patch"
                                "python-3.12-fix-tests.patch"
                                "python-3-hurd-configure.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Delete the bundled copy of libexpat.
                   (delete-file-recursively "Modules/expat")
                   (substitute* "Modules/Setup"
                     ;; Link Expat instead of embedding the bundled one.
                     (("^#pyexpat.*")
                      "pyexpat pyexpat.c -lexpat\n"))
                   ;; Delete windows binaries
                   (for-each delete-file
                             (find-files "Lib/distutils/command" "\\.exe$"))))))
    (outputs '("out" "tk"               ;tkinter; adds 50 MiB to the closure
               "idle"))                 ;programming environment; weighs 5MB
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:configure-flags
      #~(list "--enable-shared"          ;allow embedding
              "--with-system-expat"      ;for XML support
              "--with-system-ffi"        ;build ctypes
              "--with-ensurepip=install" ;install pip and setuptools
              "--with-computed-gotos"    ;main interpreter loop optimization
              "--enable-unicode=ucs4"
              "--without-static-libpython"
              "--enable-loadable-sqlite-extensions"

              ;; FIXME: These flags makes Python significantly faster,
              ;; but leads to non-reproducible binaries.
              ;; "--with-lto"   ;increase size by 20MB, but 15% speedup
              ;; "--enable-optimizations"

              ;; Prevent the installed _sysconfigdata.py from retaining
              ;; a reference to coreutils.
              "INSTALL=install -c"
              "MKDIR_P=mkdir -p"

              ;; Disable runtime check failing if cross-compiling, see:
              ;; https://lists.yoctoproject.org/pipermail/poky/2013-June/008997.html
              #$@(if (%current-target-system)
                     #~("ac_cv_buggy_getaddrinfo=no"
                        "ac_cv_file__dev_ptmx=no"
                        "ac_cv_file__dev_ptc=no")
                     #~())
              ;; -fno-semantic-interposition reinstates some
              ;; optimizations by gcc leading to around 15% speedup.
              ;; This is the default starting from python 3.10.
              "CFLAGS=-fno-semantic-interposition"
              (string-append "LDFLAGS=-Wl,-rpath="
                             (assoc-ref %outputs "out")
                             "/lib"
                             " -fno-semantic-interposition"))
      ;; With no -j argument tests use all available cpus, so provide one.
      #:make-flags
      #~(list (string-append (format #f "TESTOPTS=-j~d"
                                     (parallel-job-count))
                             ;; those tests fail on low-memory systems
                             " --exclude"
                             " test_mmap"
                             " test_socket"
                             " test_threading"
                             " test_asyncio"
                             " test_shutdown"
                             #$@(if (system-hurd?)
                                    #~(" test_posix" ;multiple errors
                                       " test_time"
                                       " test_pty"
                                       " test_shutil"
                                       " test_tempfile" ;chflags: invalid argument:
                                       ;; tbv14c9t/dir0/dir0/dir0/test0.txt
                                       " test_os" ;stty: 'standard input':
                                       ;; Inappropriate ioctl for device
                                       " test_openpty" ;No such file or directory
                                       " test_selectors" ;assertEqual(NUM_FDS // 2, len(fds))
                                       ;; 32752 != 4
                                       " test_compileall" ;multiple errors
                                       " test_poll" ;list index out of range
                                       " test_subprocess" ;runs over 10min
                                       " test_asyncore"   ;multiple errors
                                       " test_threadsignals"
                                       " test_eintr" ;Process return code is -14
                                       " test_io"    ;multiple errors
                                       " test_logging"
                                       " test_signal"
                                       " test_flags" ;ERROR
                                       " test_bidirectional_pty"
                                       " test_create_unix_connection"
                                       " test_unix_sock_client_ops"
                                       " test_open_unix_connection"
                                       " test_open_unix_connection_error"
                                       " test_read_pty_output"
                                       " test_write_pty"
                                       " test_concurrent_futures" ;freeze
                                       " test_venv"               ;freeze
                                       " test_multiprocessing_forkserver" ;runs over 10min
                                       " test_multiprocessing_spawn" ;runs over 10min
                                       " test_builtin"
                                       " test_capi"
                                       " test_dbm_ndbm"
                                       " test_exceptions"
                                       " test_faulthandler"
                                       " test_getopt"
                                       " test_importlib"
                                       " test_json"
                                       " test_multiprocessing_fork"
                                       " test_multiprocessing_main_handling"
                                       " test_pdb "
                                       " test_regrtest"
                                       " test_sqlite")
                                    #~())))

      #:modules '((ice-9 ftw)
                  (ice-9 match)
                  (guix build utils)
                  (guix build gnu-build-system))

      #:phases
      #~(modify-phases %standard-phases
          #$@(if (system-hurd?)
                 #~((add-after 'unpack
                        'disable-multi-processing
                      (lambda _
                        (substitute* "Makefile.pre.in"
                          (("-j0")
                           "-j1")))))
                 #~())
          (add-before 'configure 'patch-lib-shells
            (lambda _
              ;; This variable is used in setup.py to enable cross compilation
              ;; specific switches. As it is not set properly by configure
              ;; script, set it manually.
              #$@(if (%current-target-system)
                     #~((setenv "_PYTHON_HOST_PLATFORM" ""))
                     #~())
              ;; Filter for existing files, since some may not exist in all
              ;; versions of python that are built with this recipe.
              (substitute* (filter file-exists?
                                   '("Lib/subprocess.py"
                                     "Lib/popen2.py"
                                     "Lib/distutils/tests/test_spawn.py"
                                     "Lib/test/support/__init__.py"
                                     "Lib/test/test_subprocess.py"))
                (("/bin/sh")
                 (which "sh")))))
          (add-before 'configure 'do-not-record-configure-flags
            (lambda* (#:key configure-flags #:allow-other-keys)
              ;; Remove configure flags from the installed '_sysconfigdata.py'
              ;; and 'Makefile' so we don't end up keeping references to the
              ;; build tools.
              ;;
              ;; Preserve at least '--with-system-ffi' since otherwise the
              ;; thing tries to build libffi, fails, and we end up with a
              ;; Python that lacks ctypes.
              (substitute* "configure"
                (("^CONFIG_ARGS=.*$")
                 (format #f "CONFIG_ARGS='~a'\n"
                         (if (member "--with-system-ffi"
                                     configure-flags)
                             "--with-system-ffi" ""))))))
          (add-before 'check 'pre-check
            (lambda _
              ;; 'Lib/test/test_site.py' needs a valid $HOME
              (setenv "HOME"
                      (getcwd))))
          (add-after 'unpack 'set-source-file-times-to-1980
            ;; XXX One of the tests uses a ZIP library to pack up some of the
            ;; source tree, and fails with "ZIP does not support timestamps
            ;; before 1980".  Work around this by setting the file times in the
            ;; source tree to sometime in early 1980.
            (lambda _
              (let ((circa-1980 (* 10 366 24 60 60)))
                (ftw "."
                     (lambda (file stat flag)
                       (utime file circa-1980 circa-1980) #t)))))
          (add-after 'unpack 'remove-windows-binaries
            (lambda _
              ;; Delete .exe from embedded .whl (zip) files
              (for-each (lambda (whl)
                          (let ((dir "whl-content")
                                (circa-1980 (* 10 366 24 60 60)))
                            (mkdir-p dir)
                            (with-directory-excursion dir
                              (let ((whl (string-append "../" whl)))
                                (invoke "unzip" whl)
                                (for-each delete-file
                                          (find-files "." "\\.exe$"))
                                (delete-file whl)
                                ;; Reset timestamps to prevent them from ending
                                ;; up in the Zip archive.
                                (ftw "."
                                     (lambda (file stat flag)
                                       (utime file circa-1980
                                              circa-1980) #t))
                                (apply invoke "zip" "-X" whl
                                       (find-files "."
                                                   #:directories? #t))))
                            (delete-file-recursively dir)))
                        (find-files "Lib/ensurepip" "\\.whl$"))))
          (add-after 'install 'remove-tests
            ;; Remove 25 MiB of unneeded unit tests.  Keep test_support.*
            ;; because these files are used by some libraries out there.
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (match (scandir (string-append out "/lib")
                                (lambda (name)
                                  (string-prefix? "python" name)))
                  ((pythonX.Y)
                   (let ((testdir (string-append out "/lib/" pythonX.Y
                                                 "/test")))
                     (with-directory-excursion testdir
                       (for-each delete-file-recursively
                                 (scandir testdir
                                          (match-lambda
                                            ((or "." "..")
                                             #f)
                                            ("support" #f)
                                            (file (not (string-prefix?
                                                        "test_support."
                                                        file))))))
                       (call-with-output-file "__init__.py"
                         (const #t))))
                   (let ((libdir (string-append out "/lib/" pythonX.Y)))
                     (for-each (lambda (directory)
                                 (let ((dir (string-append libdir "/"
                                                           directory)))
                                   (when (file-exists? dir)
                                     (delete-file-recursively dir))))
                               '("email/test" "ctypes/test"
                                 "unittest/test"
                                 "tkinter/test"
                                 "sqlite3/test"
                                 "bsddb/test"
                                 "lib-tk/test"
                                 "json/tests"
                                 "distutils/tests"))))))))
          (add-after 'remove-tests 'move-tk-inter
            (lambda* (#:key outputs inputs #:allow-other-keys)
              ;; When Tkinter support is built move it to a separate output so
              ;; that the main output doesn't contain a reference to Tcl/Tk.
              (let ((out (assoc-ref outputs "out"))
                    (tk (assoc-ref outputs "tk")))
                (when tk
                  (match (find-files out "tkinter.*\\.so")
                    ((tkinter.so)
                     ;; The .so is in OUT/lib/pythonX.Y/lib-dynload, but we
                     ;; want it under TK/lib/pythonX.Y/site-packages.
                     (let* ((len (string-length out))
                            (target (string-append tk "/"
                                                   (string-drop (dirname
                                                                 (dirname
                                                                  tkinter.so))
                                                                len)
                                                   "/site-packages")))
                       (install-file tkinter.so target)
                       (delete-file tkinter.so))))
                  ;; Remove explicit store path references.
                  (let ((tcl (assoc-ref inputs "tcl"))
                        (tk (assoc-ref inputs "tk")))
                    (substitute* (find-files (string-append out "/lib")
                                             "^(_sysconfigdata_.*\\.py|Makefile)$")
                      (((string-append "-L" tk "/lib"))
                       "")
                      (((string-append "-L" tcl "/lib"))
                       "")))))))
          (add-after 'move-tk-inter 'move-idle
            (lambda* (#:key outputs #:allow-other-keys)
              ;; when idle is built, move it to a separate output to save some
              ;; space (5MB)
              (let ((out (assoc-ref outputs "out"))
                    (idle (assoc-ref outputs "idle")))
                (when idle
                  (for-each (lambda (file)
                              (let ((target (string-append idle
                                                           "/bin/"
                                                           (basename
                                                            file))))
                                (install-file file
                                              (dirname target))
                                (delete-file file)))
                            (find-files (string-append out "/bin")
                                        "^idle"))
                  (match (find-files out "^idlelib$"
                                     #:directories? #t)
                    ((idlelib)
                     (let* ((len (string-length out))
                            (target (string-append idle "/"
                                                   (string-drop
                                                    idlelib len)
                                                   "/site-packages")))
                       (mkdir-p (dirname target))
                       (rename-file idlelib target))))))))
          (add-after 'move-idle 'rebuild-bytecode
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                ;; Disable hash randomization to ensure the generated .pycs
                ;; are reproducible.
                (setenv "PYTHONHASHSEED" "0")

                (for-each (lambda (output)
                            ;; XXX: Delete existing pycs generated by the build
                            ;; system beforehand because the -f argument does
                            ;; not necessarily overwrite all files, leading to
                            ;; indeterministic results.
                            (for-each (lambda (pyc)
                                        (delete-file pyc))
                                      (find-files output "\\.pyc$"))

                            (apply invoke
                                   `(,#$(if (%current-target-system)
                                            "python3"
                                            #~(string-append
                                               out
                                               "/bin/python3")) "-m"
                                               "compileall"
                                               "-o"
                                               "0"
                                               "-o"
                                               "1"
                                               "-o"
                                               "2"
                                               "-f" ;force rebuild
                                               "--invalidation-mode=unchecked-hash"
                                               ;; Don't build lib2to3, because it's
                                               ;; Python 2 code.
                                               "-x"
                                               "lib2to3/.*"
                                               ,output)))
                          (map cdr outputs)))))
          (add-before 'check 'set-TZDIR
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              ;; test_email requires the Olson time zone database.
              (setenv "TZDIR"
                      (string-append (assoc-ref (or native-inputs
                                                    inputs) "tzdata")
                                     "/share/zoneinfo"))))
          (add-after 'install 'install-sitecustomize.py
            #$(customize-site version)))))
    (inputs (list bzip2
                  expat
                  gdbm
                  libffi                ;for ctypes
                  sqlite                ;for sqlite extension
                  openssl
                  readline
                  zlib
                  tcl
                  tk))                  ;for tkinter
    (native-inputs `(("tzdata" ,tzdata-for-tests)
                     ("unzip" ,unzip)
                     ("zip" ,(@ (gnu packages compression) zip))
                     ("pkg-config" ,pkg-config)
                     ("sitecustomize.py" ,(local-file (search-auxiliary-file
                                                       "python/sitecustomize.py")))
                     ;; When cross-compiling, a native version of Python itself is needed.
                     ,@(if (%current-target-system)
                           `(("python" ,this-package)
                             ("which" ,which))
                           '())))
    (native-search-paths
     (list (guix-pythonpath-search-path version)
           ;; Used to locate tzdata by the zoneinfo module introduced in
           ;; Python 3.9.
           (search-path-specification
            (variable "PYTHONTZPATH")
            (files (list "share/zoneinfo")))))
    (home-page "https://www.python.org")
    (synopsis "High-level, dynamically-typed programming language")
    (description
     "Python is a remarkably powerful dynamic programming language that
is used in a wide variety of application domains.  Some of its key
distinguishing features include: clear, readable syntax; strong
introspection capabilities; intuitive object orientation; natural
expression of procedural code; full modularity, supporting hierarchical
packages; exception-based error handling; and very high level dynamic
data types.")
    (properties '((cpe-name . "python")))
    (license license:psfl)))

;; Next 3.x version.
(define-public python-next python-3.12)

;; Current 3.x version.
(define-public python-3 python-3.11)

;; Current major version.
(define-public python python-3)

;; Minimal variants of Python, mostly used to break the cycle between Tk and
;; Python (Tk -> libxcb -> Python.)

(define-public python2-minimal
  (package/inherit python-2
    (name "python2-minimal")
    (outputs '("out"))

    ;; Keep zlib, which is used by 'pip' (via the 'zipimport' module), which
    ;; is invoked upon 'make install'.  'pip' also expects 'ctypes' and thus
    ;; libffi.  Expat is needed for XML support which is expected by a lot
    ;; of libraries out there.
    (inputs (list expat libffi zlib))))

(define-public python-minimal
  (package/inherit python
    (name "python-minimal")
    (outputs '("out"))

    ;; Build fails due to missing ctypes without libffi.
    ;; OpenSSL is a mandatory dependency of Python 3.x, for urllib;
    ;; zlib is required by 'zipimport', used by pip.  Expat is needed
    ;; for XML support, which is generally expected to be available.
    (inputs (list expat libffi openssl zlib))))

(define-public python-debug
  (package/inherit python
    (name "python-debug")
    (outputs '("out" "debug"))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments python)
       ((#:configure-flags configure-flags)
        #~(append #$configure-flags
                  (list "--with-pydebug")))))
    (synopsis "Python with the debug hooks enabled")
    (description
     "This variant of Python provides an interpreter built with
@code{--with-pydebug} to help develop and debug extensions.  See
@url{https://pythonextensionpatterns.readthedocs.io/en/latest/debugging/debug.html},
for more information.")))

(define* (wrap-python3 python
                       #:optional
                       (name (string-append (package-name python) "-wrapper")))
  (package/inherit python
    (name name)
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out"))
    (inputs `(("bash" ,bash)))
    (propagated-inputs `(("python" ,python)))
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let ((bin (string-append #$output "/bin"))
                     (python (string-append
                              ;; XXX: '%build-inputs' contains the native
                              ;; Python when cross-compiling.
                              #$(if (%current-target-system)
                                    (this-package-input "python")
                                    #~(assoc-ref %build-inputs "python"))
                              "/bin/")))
                 (mkdir-p bin)
                 (for-each
                  (lambda (old new)
                    (symlink (string-append python old)
                             (string-append bin "/" new)))
                  `("python3" ,"pydoc3" ,"pip3")
                  `("python"  ,"pydoc"  ,"pip"))
                 ;; python-config outputs search paths based upon its location,
                 ;; use a bash wrapper to avoid changing its outputs.
                 (let ((bash (string-append (assoc-ref %build-inputs "bash")
                                            "/bin/bash"))
                       (old  (string-append python "python3-config"))
                       (new  (string-append bin "/python-config")))
                   (with-output-to-file new
                     (lambda ()
                       (format #t "#!~a~%" bash)
                       (format #t "exec \"~a\" \"$@\"~%" old)
                       (chmod new #o755))))))))
    (synopsis "Wrapper for the Python 3 commands")
    (description
     "This package provides wrappers for the commands of Python@tie{}3.x such
that they can also be invoked under their usual names---e.g., @command{python}
instead of @command{python3} or @command{pip} instead of @command{pip3}.

To function properly, this package should not be installed together with the
@code{python} package: this package uses the @code{python} package as a
propagated input, so installing this package already makes both the versioned
and the unversioned commands available.")))

(define-public python-wrapper (wrap-python3 python))
(define-public python-minimal-wrapper (wrap-python3 python-minimal))

;; The Python used in pyproject-build-system.
(define-public python-sans-pip
  (hidden-package
   (package/inherit python
     (arguments
      (substitute-keyword-arguments (package-arguments python)
        ((#:configure-flags flags #~())
         #~(append '("--with-ensurepip=no")
                   (delete "--with-ensurepip=install" #$flags))))))))

(define-public python-sans-pip-wrapper
  (wrap-python3 python-sans-pip))

(define-public python-toolchain
  (let ((base (package/inherit python-sans-pip-wrapper)))
    (package
      (inherit base)
      (properties '())
      (name "python-toolchain")
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (append python-pip
                 python-pypa-build
                 python-setuptools
                 python-wheel)))
      (synopsis "Python toolchain")
      (description
       "Python toolchain including Python itself, setuptools and pip.
Use this package if you need a minimal Python toolchain instead of just
the interpreter."))))

(define-public micropython
  (package
    (name "micropython")
    (version "1.19")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/micropython/micropython/"
                            "releases/download/v" version
                            "/micropython-" version ".tar.xz"))
        (sha256
         (base32 "090bbls5vnicynjyjqqd5zqwx8d6zxp7wlnrxgh0b4s8cyf5i8zj"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (delete-file-recursively "ports/cc3200/FreeRTOS")
          (with-directory-excursion "lib"
            ;; TODO: Unbundle axtls and berkley-db-1.xx
            (for-each delete-file-recursively
                      '("libffi" "lwip" "stm32lib" "nrfx")))))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'build-mpy-cross
                 (lambda* (#:key make-flags #:allow-other-keys)
                   (with-directory-excursion "mpy-cross"
                     (apply invoke "make" make-flags))))
               (add-after 'build-mpy-cross 'prepare-build
                 (lambda _
                   (chdir "ports/unix")
                   ;; see: https://github.com/micropython/micropython/pull/4246
                   (substitute* "Makefile"
                     (("-Os") "-Os -ffp-contract=off"))))
               (add-before 'install-license-files 'chdir-back
                 ;; We don't build in the root directory so the file isn't found.
                 (lambda _
                   (chdir "../..")))
               (delete 'configure))       ; no configure
           #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   "V=1")
           #:test-target "test"))
    (native-inputs (list pkg-config python-wrapper))
    (inputs
     (list libffi))
    (home-page "https://micropython.org/")
    (synopsis "Python implementation for microcontrollers and constrained systems")
    (description "MicroPython is a lean and efficient implementation of the
Python 3 programming language that includes a small subset of the Python
standard library and is optimised to run on microcontrollers and in constrained
environments.  MicroPython is packed full of advanced features such as an
interactive prompt, arbitrary precision integers, closures, list comprehension,
generators, exception handling and more.  Still it is compact enough to fit and
run within just 256k of code space and 16k of RAM.  MicroPython aims to be as
compatible with normal Python as possible to allow you to transfer code with
ease from the desktop to a microcontroller or embedded system.")
    (license license:expat)))
