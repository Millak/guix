;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2013-2022, 2024-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2016, 2019, 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015-2025 Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2018, 2020, 2021, 2022 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015, 2017, 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2020 EuAndreh <eu@euandre.org>
;;; Copyright © 2017, 2018, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017, 2020, 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 John D. Boy <jboy@bius.moe>
;;; Copyright © 2020, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021, 2022, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 LibreMiami <packaging-guix@libremiami.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 François J. <francois-oss@avalenn.eu>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021, 2024 jgart <jgart@dismail.de>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Dhruvin Gandhi <contact@dhruvin.dev>
;;; Copyright © 2015, 2022 David Thompson <davet@gnu.org>
;;; Copyright © 2023, 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 Kjartan Oli Agustsson <kjartanoli@disroot.org>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2024 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Ryan Desfosses <rdesfo@sdf.org>
;;; Copyright © 2024 Suhail Singh <suhail@bayesians.ca>
;;; Copyright © 2024 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2024 Javier Olaechea <pirata@gmail.com>
;;; Copyright © 2024, 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Dariqq <dariqq@posteo.net>
;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>
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

(define-module (gnu packages version-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cook)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-vcs)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages less)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages web)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages php)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages sync)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (make-gitolite))

(define-public breezy
  (package
    (name "breezy")
    (version "3.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/brz/"
                           (version-major+minor version) "/" version
                           "/+download/breezy-" version ".tar.gz"))
       (modules '((guix build utils)))
       ;; Delete pre-generated Cython C files.
       (snippet '(for-each delete-file (find-files "." "\\pyx.c$")))
       (sha256
        (base32
         "1n6mqd1iy50537kb4lsr52289yyr1agmkxpchxlhb9682zr8nn62"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-inputs (list rust-lazy-static-1
                           rust-pyo3-0.22
                           rust-regex-1)
      #:install-source? #f
      #:modules
      '((guix build cargo-build-system)
        ((guix build python-build-system) #:prefix py:)
        (guix build utils))
      #:imported-modules
      `(,@%cargo-build-system-modules
        ,@%python-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'ensure-no-mtimes-pre-1980
            (assoc-ref py:%standard-phases 'ensure-no-mtimes-pre-1980))
          (add-after 'ensure-no-mtimes-pre-1980 'enable-bytecode-determinism
            (assoc-ref py:%standard-phases 'enable-bytecode-determinism))
          (add-after 'enable-bytecode-determinism 'ensure-no-cythonized-files
            (assoc-ref py:%standard-phases 'ensure-no-cythonized-files))
          (add-after 'unpack 'patch-test-shebangs
            (lambda _
              (substitute* (append (find-files "breezy/bzr/tests")
                                   (find-files "breezy/tests"))
                (("#!/bin/sh")
                 (format #f "#!~a" (which "sh"))))))
          (add-before 'build 'adjust-for-python-3.10
            (lambda _
              (substitute* '("breezy/doc_generate/__init__.py"
                             "breezy/tests/test_selftest.py")
                ;; AttributeError: module 'datetime' has no attribute 'UTC'
                ;; This only works for python >= 3.11
                (("datetime.UTC") "datetime.timezone.utc"))))
          (replace 'build
            (assoc-ref py:%standard-phases 'build))
          (delete 'check)             ;moved after the install phase
          (replace 'install
            (assoc-ref py:%standard-phases 'install))
          (add-after 'install 'add-install-to-pythonpath
            (assoc-ref py:%standard-phases 'add-install-to-pythonpath))
          (add-after 'add-install-to-pythonpath 'add-install-to-path
            (assoc-ref py:%standard-phases 'add-install-to-path))
          (add-after 'add-install-to-path 'install-completion
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (bash (string-append out "/share/bash-completion"
                                          "/completions")))
                (install-file "contrib/bash/brz" bash))))
          (add-after 'add-install-to-path 'wrap
            (assoc-ref py:%standard-phases 'wrap))
          (add-after 'wrap 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "BZR_EDITOR" "nano")
                (invoke "brz" "selftest" "--verbose" "--parallel=fork"
                        ;; This test hangs
                        "-x" "breezy.tests.blackbox.test_serve"
                        ;; No GnuPG key results for pattern: bazaar@example.com
                        "-x" "breezy.tests.test_gpg"
                        ;; compgen: command not found
                        "-x" "bash_completion"
                        ;; No such file or directory: '/etc/mtab'
                        "-x" "breezy.tests.blackbox.test_diff.TestExternalDiff.test_external_diff"
                        ;; Value "/etc/ssl/certs/ca-certificates.crt" is not valid for "ssl.ca_certs"
                        "-x" "breezy.tests.test_https_urllib.CaCertsConfigTests.test_default_exists"
                        ;; Unknown Failure
                        "-x" "breezy.tests.test_plugins.TestLoadPluginAt.test_compiled_loaded"
                        "-x" "breezy.tests.test_plugins.TestPlugins.test_plugin_get_path_pyc_only"
                        "-x" "breezy.tests.test_selftest.TestActuallyStartBzrSubprocess.test_start_and_stop_bzr_subprocess_send_signal"))))
          (add-before 'strip 'rename-pth-file
            (assoc-ref py:%standard-phases 'rename-pth-file)))))
    (native-inputs (list gettext-minimal
                         python-wrapper
                         python-cython
                         python-setuptools
                         python-setuptools-gettext
                         python-setuptools-rust
                         python-tomli
                         python-wheel
                         ;; tests
                         nano
                         python-testtools
                         python-packaging
                         python-subunit))
    (inputs (list python-configobj
                  python-dulwich
                  python-fastbencode
                  python-fastimport
                  python-launchpadlib
                  python-merge3
                  python-paramiko
                  python-gpg
                  python-patiencediff
                  python-pygithub
                  python-pyyaml
                  python-tzlocal
                  python-urllib3))
    (home-page "https://www.breezy-vcs.org/")
    (synopsis "Decentralized revision control system")
    (description
     "Breezy (@command{brz}) is a decentralized revision control system.  By
default, Breezy provides support for both the
@uref{https://bazaar.canonical.com/, Bazaar} and @uref{https://www.git-scm.com,
Git} file formats.  Breezy is backwabrds compatible with Bazaar's disk format
and protocols.  One of the key differences with Bazaar is that Breezy runs on
Python 3.3 and later, rather than on Python 2.")
    (license license:gpl2+)))

(define-public bazaar
  (deprecated-package "bazaar" breezy))

(define git-cross-configure-flags
  #~(list "ac_cv_fread_reads_directories=yes"
          "ac_cv_snprintf_returns_bogus=no"
          "ac_cv_iconv_omits_bom=no"))

;; The size of the closure of 'git-minimal' is two thirds that of 'git'.
;; Its test suite runs slightly faster and most importantly it doesn't
;; depend on packages that are expensive to build such as Subversion.
(define-public git-minimal
  (package
    (name "git-minimal")
    (version "2.49.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://kernel.org/software/scm/git/git-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0a2nm2szhn47dm0m1f1kmg1rikb7saqj67zr25n9yzhbb77r10b1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules `((srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 format)
                  (ice-9 textual-ports)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  ,@%default-gnu-modules)
      ;; Make sure the full bash does not end up in the final closure.
      #:disallowed-references (list bash perl)
      #:test-target "test"
      #:configure-flags
      (if (%current-target-system)
          git-cross-configure-flags
          #~(list))
      #:make-flags
      #~(list "V=1"                     ;more verbose compilation
              (string-append "SHELL_PATH="
                             #+(this-package-native-input "bash-minimal")
                             "/bin/sh")

              ;; Tests require a bash with completion support.
              (string-append "TEST_SHELL_PATH="
                             #+(this-package-native-input "bash")
                             "/bin/bash")

              ;; By default 'make install' creates hard links for
              ;; things in 'libexec/git-core', which leads to huge
              ;; nars; see <https://bugs.gnu.org/21949>.
              "NO_INSTALL_HARDLINKS=indeed"
              #$@(if (or (target-hurd64?) (%current-target-system))
                     #~("-Wno-implicit-function-declaration")
                     #~()))
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (%current-target-system)
                 ;; The git build system assumes build == host
                 #~((add-after 'unpack  'use-host-uname_S
                      (lambda _
                        (substitute* "config.mak.uname"
                          (("uname_S := .*" all)
                           (if #$(target-hurd?)
                               "uname_S := GNU\n"
                               all))))))
                 ;; We do not have a full bash when cross-compiling.
                 #~((add-after 'unpack 'modify-PATH
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((path (string-split (getenv "PATH") #\:))
                              (bash-full #$(this-package-native-input "bash")))
                          ;; Drop the test bash from PATH so that (which "sh")
                          ;; and similar does the right thing.
                          (setenv "PATH"
                                  (string-join
                                   (remove (cut string-prefix? bash-full <>)
                                           path)
                                   ":")))))))
          #$@(if (system-hurd?)
                 #~((add-after 'unpack 'delete-tests/hurd
                      (lambda _
                        (delete-file "t/t0052-simple-ipc.sh")
                        (delete-file "t/t5562-http-backend-content-length.sh")
                        (delete-file "t/t9902-completion.sh"))))
                 #~())
          ;; Add cross curl-config script to PATH when cross-compiling.
          #$@(if (%current-target-system)
                 #~((add-before 'configure 'add-cross-curl-config
                      (lambda* (#:key inputs #:allow-other-keys)
                        (setenv "PATH"
                                (string-append
                                 (dirname (search-input-file
                                           inputs "bin/curl-config"))
                                 ":" (getenv "PATH"))))))
                 #~())
          (add-after 'unpack 'patch-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (define (prepend-string-to-file text file)
                "Prepend TEXT to FILE."
                (let ((content (call-with-input-file file
                                 (cut get-string-all <>))))
                  (call-with-output-file file
                    (lambda (port)
                      (display text port)
                      (display content port)))))

              (define PATH-variable-definition
                (format #f "PATH=~{~a~^:~}${PATH:+:}$PATH~%~%"
                        (map (compose dirname (cut search-input-file inputs <>))
                             '("bin/basename"
                               "bin/sed"))))

              ;; Ensure that coreutils (for basename) and sed are on PATH
              ;; for any script that sources the 'git-sh-setup.sh' file.
              (prepend-string-to-file PATH-variable-definition
                                      "git-sh-setup.sh")

              ;; Avoid depending on util-linux; it's only used to detect
              ;; whether the system is MinGW, which we can detect at build
              ;; time.
              (substitute* "git-sh-setup.sh"
                (("\\$\\(uname -s)")
                 (if #$(target-mingw?)
                     "MINGW"
                     "GNU")))           ;matched against '*'

              ;; git-submodule sources 'git-sh-setup.sh', but not before
              ;; invoking the basename and sed commands... patch them to their
              ;; absolute location.
              (substitute* "git-submodule.sh"
                (("\\$\\(basename")
                 (string-append "$(" (search-input-file inputs "bin/basename")))
                (("sed -e")
                 (string-append (search-input-file inputs "bin/sed") " -e")))))
          (add-after 'configure 'patch-makefiles
            (lambda _
              (substitute* "Makefile"
                (("/usr/bin/perl") (which "perl")))))
          (add-after 'configure 'add-PM.stamp
            (lambda _
              ;; Add the "PM.stamp" to avoid "no rule to make target".
              (call-with-output-file "perl/PM.stamp" (const #t))))
          (add-before 'check 'patch-tests
            (lambda _
              (let ((store-directory (%store-directory)))
                ;; These files contain some funny bytes that Guile is unable
                ;; to decode for shebang patching. Just delete them.
                (for-each delete-file '("t/t4201-shortlog.sh"
                                        "t/t7813-grep-icase-iso.sh"))
                ;; Many tests contain inline shell scripts (hooks etc).
                (substitute* (find-files "t" "\\.sh$")
                  (("#!/bin/sh") (string-append "#!" (which "sh"))))
                ;; Un-do shebang patching here to prevent checksum mismatch.
                (substitute* '("t/t4034/perl/pre" "t/t4034/perl/post")
                  (("^#!.*/bin/perl") "#!/usr/bin/perl"))
                (substitute* "t/t5003-archive-zip.sh"
                  (("cp /bin/sh") (string-append "cp " (which "sh"))))
                (substitute* "t/t6030-bisect-porcelain.sh"
                  (("\"/bin/sh\"") (string-append "\"" (which "sh") "\"")))
                ;; FIXME: This test runs `git commit` with a bogus EDITOR
                ;; and empty commit message, but does not fail the way it's
                ;; expected to. The test passes when invoked interactively.
                (substitute* "t/t7508-status.sh"
                  (("\tcommit_template_commented") "\ttrue"))
                ;; More checksum mismatches due to odd shebangs.
                (substitute* "t/t9100-git-svn-basic.sh"
                  (((string-append "\"#!" store-directory ".*/bin/sh"))
                   "\"#!/bin/sh") )
                (substitute* "t/t9300-fast-import.sh"
                  (((string-append "\t#!" store-directory ".*/bin/sh"))
                   "\t#!/bin/sh")
                  (((string-append "'#!" store-directory ".*/bin/sh"))
                   "'#!/bin/sh"))
                ;; FIXME: Some hooks fail with "basename: command not found".
                ;; See 't/trash directory.t9164.../svn-hook.log'.
                (delete-file "t/t9164-git-svn-dcommit-concurrent.sh")

                ;; XXX: These tests fail intermittently for unknown reasons:
                ;; <https://bugs.gnu.org/29546>.
                (for-each delete-file
                          '("t/t9128-git-svn-cmd-branch.sh"
                            "t/t9167-git-svn-cmd-branch-subproject.sh"
                            "t/t9141-git-svn-multiple-branches.sh"))

                #$@(if (version>=? (package-version this-package)
                                   "2.48.0")
                       ;; Purge the purged tests in meson.build
                       #~((substitute
                           "t/meson.build"
                           (list (cons "^(.+')(t[^']+[.]sh)('.*)$"
                                       (lambda (line matches)
                                         (let* ((match-offset (vector-ref (car matches) 3))
                                                (test-file (string-append "t/"
                                                                          (substring line
                                                                                     (car match-offset)
                                                                                     (cdr match-offset)))))
                                           (if (file-exists? test-file)
                                               line
                                               "")))))))
                       #~()))))
          (add-after 'install 'install-shell-completion
            (lambda _
              (let ((bash (string-append #$output "/etc/bash_completion.d"))
                    (zsh  (string-append #$output "/share/zsh/site-functions")))
                ;; TODO: Install the tcsh completions in the right place.
                (for-each mkdir-p (list bash zsh))
                (copy-file "contrib/completion/git-completion.bash"
                           (string-append bash "/git"))
                (copy-file "contrib/completion/git-prompt.sh"
                           (string-append #$output "/bin/git-prompt"))
                (copy-file "contrib/completion/git-completion.zsh"
                           (string-append zsh "/_git")))))
          (add-after 'install 'remove-unusable-perl-commands
            (lambda _
              (let ((bin     (string-append #$output "/bin"))
                    (libexec (string-append #$output "/libexec")))
                (for-each (lambda (file)
                            (delete-file (string-append libexec
                                                        "/git-core/" file)))
                          '("git-svn" "git-cvsimport" "git-archimport"
                            "git-cvsserver" "git-request-pull"

                            ;; git-add--interactive was removed in Git 2.40 but
                            ;; this phase is inherited by older versions.
                            #$@(if (version>=? (package-version this-package)
                                               "2.40.1")
                                   #~()
                                   #~("git-add--interactive"))

                            "git-cvsexportcommit"
                            "git-instaweb" "git-send-email"))
                (delete-file (string-append bin "/git-cvsserver"))

                ;; These templates typically depend on Perl.  Remove them.
                (delete-file-recursively
                 (string-append #$output "/share/git-core/templates/hooks"))

                ;; Gitweb depends on Perl as well.
                (delete-file-recursively
                 (string-append #$output "/share/gitweb")))))
          (add-after 'install 'restore-sample-hooks-shebang
            (lambda _
              (let* ((dir (string-append #$output
                                         "/share/git-core/templates/hooks")))
                (for-each (lambda (file)
                            (format #t "restoring shebang on `~a'~%" file)
                            (substitute* file
                              (("^#!.*/bin/sh") "#!/bin/sh")))
                          (find-files dir ".*"))))))))
    (native-inputs
     ;; Add bash-minimal explicitly to ensure it comes before bash-for-tests,
     ;; see <https://bugs.gnu.org/39513>.
     (list bash-minimal
           bash
           gettext-minimal
           perl))
    (inputs
     (list coreutils-minimal
           curl                         ;for HTTP(S) access
           expat                        ;for 'git push' over HTTP(S)
           openssl
           perl
           sed
           zlib))
    (native-search-paths
     ;; For HTTPS access, Git needs a single-file certificate bundle, specified
     ;; with $GIT_SSL_CAINFO.
     (list (search-path-specification
            (variable "GIT_SSL_CAINFO")
            (file-type 'regular)
            (separator #f)              ;single entry
            (files '("etc/ssl/certs/ca-certificates.crt")))
           (search-path-specification
            (variable "GIT_EXEC_PATH")
            (separator #f)              ;single entry
            (files '("libexec/git-core")))))
    (synopsis "Distributed version control system")
    (description
     "Git is a free distributed version control system designed to handle
everything from small to very large projects with speed and efficiency.")
    ;; XXX: Ignore this CVE to work around a name clash with the unrelated
    ;; "cpe:2.3:a:jenkins:git" package.  The proper fix is for (guix cve) to
    ;; account for "vendor names".
    (properties '((lint-hidden-cve . ("CVE-2018-1000182"
                                      "CVE-2018-1000110"
                                      "CVE-2019-1003010"
                                      "CVE-2020-2136"
                                      "CVE-2021-21684"
                                      "CVE-2022-30947"
                                      "CVE-2022-30948"
                                      "CVE-2022-30949"
                                      "CVE-2022-36882"
                                      "CVE-2022-36883"
                                      "CVE-2022-36884"))
                  (upstream-name . "git")))
    (license license:gpl2)
    (home-page "https://git-scm.com/")))

(define-public git
  (package/inherit git-minimal
    (name "git")
    (outputs '("out"                    ;the core
               "send-email"             ;for git-send-email
               "svn"                    ;git-svn
               "credential-netrc"       ;git-credential-netrc
               "credential-libsecret"   ;git-credential-libsecret
               "subtree"                ;git-subtree
               "gui"))                  ;gitk, git gui
    (arguments
     (substitute-keyword-arguments (package-arguments git-minimal)
       ((#:disallowed-references disallowed-refs ''())
        (delete perl disallowed-refs))
       ((#:make-flags flags #~'())
        #~(cons "USE_LIBPCRE2=yes" #$flags))
       ((#:configure-flags flags #~'())
        ;; The explicit --with-tcltk forces the build system to hardcode the
        ;; absolute file name to 'wish'.
        #~(cons (string-append "--with-tcltk="
                               (search-input-file %build-inputs
                                                  "bin/wish8.6"))
                #$flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (delete 'remove-unusable-perl-commands)
            (replace 'patch-makefiles
              (lambda _
                (substitute* (find-files "." "Makefile")
                  (("/bin/sh") (which "sh"))
                  (("/usr/bin/perl") (which "perl"))
                  (("/usr/bin/python") (which "python3")))))
            (add-after 'build 'build-subtree
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (invoke "make" "-C" "Documentation" "asciidoc.conf")
                (with-directory-excursion "contrib/subtree"
                  (invoke "make")
                  (invoke "make" "install")
                  (invoke "make" "install-doc")
                  (substitute* "git-subtree"
                    (("/bin/sh") (which "sh"))))))
            (add-after 'install 'install-info-manual
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (define job-count (if parallel-build?
                                      (number->string (parallel-job-count))
                                      "1"))
                (invoke "make" "-C" "Documentation" "install-info"
                        "-j" job-count
                        ;; The Makefile refer to 'docbook2x-texi', but our
                        ;; binary is named 'docbook2texi'.
                        "DOCBOOK2X_TEXI=docbook2texi" "PERL_PATH=perl")))
            (add-after 'install 'install-credential-netrc
              (lambda _
                (install-file
                 "contrib/credential/netrc/git-credential-netrc.perl"
                 (string-append #$output:credential-netrc "/bin"))
                (rename-file (string-append #$output:credential-netrc
                                            "/bin/git-credential-netrc.perl")
                             (string-append #$output:credential-netrc
                                            "/bin/git-credential-netrc"))
                ;; Previously, Git.pm was automatically found by netrc.
                ;; Perl 5.26 changed how it locates modules so that @INC no
                ;; longer includes the current working directory (the Perl
                ;; community calls this "dotless @INC").
                (wrap-program (string-append #$output:credential-netrc
                                             "/bin/git-credential-netrc")
                  `("PERL5LIB" ":" prefix
                    (,(string-append #$output "/share/perl5"))))))
            (add-after 'install 'install-credential-libsecret
              (lambda _
                (with-directory-excursion "contrib/credential/libsecret"
                  ((assoc-ref gnu:%standard-phases 'build))
                  (install-file "git-credential-libsecret"
                                (string-append #$output:credential-libsecret
                                               "/bin")))))
            (add-after 'install 'install-subtree
              (lambda _
                (install-file "contrib/subtree/git-subtree"
                              (string-append #$output:subtree "/bin"))
                (install-file "contrib/subtree/git-subtree.1"
                              (string-append #$output:subtree
                                             "/share/man/man1"))))
            (add-after 'install 'split
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Split the binaries to the various outputs.
                (let* ((out      #$output)
                       (se       #$output:send-email)
                       (svn      #$output:svn)
                       (gui      #$output:gui)
                       (gitk     (string-append out "/bin/gitk"))
                       (gitk*    (string-append gui "/bin/gitk"))
                       (git-gui  (string-append
                                  out "/libexec/git-core/git-gui"))
                       (git-gui* (string-append
                                  gui "/libexec/git-core/git-gui"))
                       (git-cit  (string-append
                                  out "/libexec/git-core/git-citool"))
                       (git-cit* (string-append
                                  gui "/libexec/git-core/git-citool"))
                       (git-se   (string-append
                                  out "/libexec/git-core/git-send-email"))
                       (git-se*  (string-append
                                  se  "/libexec/git-core/git-send-email"))
                       (git-svn  (string-append
                                  out "/libexec/git-core/git-svn"))
                       (git-svn* (string-append
                                  svn "/libexec/git-core/git-svn"))
                       (git-sm   (string-append
                                  out "/libexec/git-core/git-submodule")))
                  (mkdir-p (string-append gui "/bin"))
                  (mkdir-p (string-append gui "/libexec/git-core"))
                  (mkdir-p (string-append se  "/libexec/git-core"))
                  (mkdir-p (string-append svn "/libexec/git-core"))

                  (for-each (lambda (old new)
                              (copy-file old new)
                              (delete-file old)
                              (chmod new #o555))
                            (list gitk git-gui git-cit git-se git-svn)
                            (list gitk* git-gui* git-cit* git-se* git-svn*))

                  ;; Tell 'git-svn' where Subversion and perl-term-readkey are.

                  ;; FIXME: Old school 'assoc-ref' is used to retrieve
                  ;; subversion here, as #$(this-package-input "subversion")
                  ;; causes a dependency cycle for unknown reasons.
                  (wrap-program git-svn*
                    `("PATH" ":" prefix
                      (,(dirname (search-input-file inputs "bin/perl"))))
                    `("PERL5LIB" ":" prefix
                      ,(search-path-as-list
                        '("lib/perl5/site_perl")
                        (list (assoc-ref inputs "subversion")
                              #$(this-package-input "perl-term-readkey"))))

                    ;; XXX: The .so for SVN/Core.pm lacks a RUNPATH, so
                    ;; help it find 'libsvn_client-1.so'.
                    `("LD_LIBRARY_PATH" ":" prefix
                      (,(string-append (assoc-ref inputs "subversion")
                                       "/lib"))))

                  ;; Tell 'git-send-email' where perl modules are.
                  (wrap-program git-se*
                    `("PERL5LIB" ":" prefix
                      ,(search-path-as-list
                        '("lib/perl5/site_perl")
                        '#$(delete-duplicates
                            (let ((perl-inputs
                                   (list (this-package-input "perl-authen-sasl")
                                         (this-package-input "perl-net-smtp-ssl")
                                         (this-package-input "perl-io-socket-ssl"))))
                              (append perl-inputs
                                      (map last
                                           (append-map
                                            package-transitive-propagated-inputs
                                            perl-inputs))))))))

                  ;; Tell 'gitweb.cgi' where perl modules are.
                  (wrap-program (string-append out "/share/gitweb/gitweb.cgi")
                    `("PERL5LIB" ":" prefix
                      ,(search-path-as-list
                        '("lib/perl5/site_perl")
                        '#$(delete-duplicates
                            (let ((perl-inputs (list (this-package-input
                                                      "perl-cgi"))))
                              (append perl-inputs
                                      (map last
                                           (append-map
                                            package-transitive-propagated-inputs
                                            perl-inputs))))))))

                  ;; Tell 'git-submodule' where Perl is.
                  (wrap-program git-sm
                    `("PATH" ":" prefix
                      (,(dirname (search-input-file inputs "bin/perl"))))))))
            (add-after 'split 'install-man-pages
              (lambda _
                (let ((man (string-append #$output "/share/man")))
                  (mkdir-p man)
                  (with-directory-excursion man
                    (invoke
                     "tar" "xvf"
                     #$(origin
                         (method url-fetch)
                         (uri (string-append
                               "mirror://kernel.org/software/scm/git/"
                               "git-manpages-" (package-version this-package)
                               ".tar.xz"))
                         (sha256
                          (base32
                           "1my4qax2wxlhxsyf3wjxllsc2jy9lm97w78alllrgfjgihb46irf"))))))))))))
    (native-inputs
     (modify-inputs (package-native-inputs git-minimal)
       ;; For subtree documentation.
       (append asciidoc
               docbook2x
               docbook-xml-4.5
               docbook-xsl
               libxslt
               pkg-config
               texinfo
               xmlto)))
    (inputs
     (modify-inputs (package-inputs git-minimal)
       (append bash-minimal             ;for wrap-program
               python                   ;for git-p4

               ;; For PCRE support in git grep (USE_LIBPCRE2).
               pcre2

               ;; For 'gitweb.cgi'.
               perl-cgi

               ;; For 'git-svn'.
               subversion
               perl-term-readkey

               ;; For 'git-send-email'.
               perl-authen-sasl
               perl-net-smtp-ssl
               perl-io-socket-ssl

               ;; For 'git gui', 'gitk', and 'git citool'.
               tcl
               tk

               ;; For 'git-credential-libsecret'
               glib
               libsecret)))))

;;; The symbol git-minimal/fixed should be used when git-minimal needs fixes
;;; (security or else) and this deprecation could be removed.
(define-deprecated/public-alias git-minimal/fixed git-minimal/pinned)

(define-public git-minimal/pinned
  ;; Version that rarely changes, depended on by Graphene/GTK+.
  (package/inherit git-minimal
    (version "2.41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/scm/git/git-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0h40arw08xbpi2cbf7pvc947v963rjxz3inb2ar81zjc8byvlj77"))))))

(define-public python-klaus
  (package
    (name "python-klaus")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "klaus" version))
              (sha256
               (base32
                "1w6sl15llnkcg7kmnpn64awdiis061q2gijnhdx0ng7z4p1glapl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; TODO: https://github.com/jonashaag/klaus/issues/322
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'configure-git
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each (lambda (file)
                          (substitute* file
                            (("\"git\"")
                             (string-append "\""
                                            (search-input-file inputs "/bin/git") "\""))))
                        (list "klaus/ctagsutils.py"
                              "klaus/repo.py"
                              "klaus/utils.py"
                              "tests/test_contrib.py"
                              "tests/test_make_app.py")))))))
    (inputs (list git-minimal))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-dulwich python-flask python-httpauth
           python-humanize python-pygments python-werkzeug))
    (home-page "https://github.com/jonashaag/klaus")
    (synopsis "Simple git web viewer")
    (description
"@code{klaus} is a simple, easy-to-set-up git web viewer.  It features
@itemize
@item Super easy to set up -- no configuration required
@item Syntax highlighting
@item Markdown + RestructuredText rendering support
@item Pull + push support (Git Smart HTTP)
@item Code navigation using Exuberant ctags
@end itemize")
    (license license:isc)))

(define-public git2cl
  (let ((commit "1d74d4c0d933fc69ed5cec838c73502584dead05"))
    (package
      (name "git2cl")
      (version (string-append "20120919." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.nongnu.org/git/git2cl.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wnnbm2sjvfj0qrksj89jlnl69miwl0vk3wrrvgvpclgys3na2g1"))))
      (build-system copy-build-system)
      (inputs
       (list perl))
      (arguments
       `(#:install-plan '(("git2cl" "bin/git2cl"))))
      (home-page "https://savannah.nongnu.org/projects/git2cl")
      (synopsis "Convert Git logs to GNU ChangeLog format")
      (description "@code{git2cl} is a command line tool for converting Git
logs to GNU ChangeLog format.")
      (license license:gpl2+))))

(define-public gitless
  (package
    (name "gitless")
    (version "0.8.8")
    (source
     (origin
       ;; The PyPI package lacks a test suite.  Build directly from git.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gitless-vcs/gitless")
             (commit (string-append "v" version))))
       (sha256
        (base32 "048kl27zjr68hgs70g3l98ci9765wxva6azzrhcdys7nsdd493n6"))
       (file-name (git-file-name name version))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'loosen-requirements
            (lambda _
              (substitute* "setup.py"
                ;; Using Guix's python-pygit2 1.1.0 appears to work fine…
                (("pygit2==") "pygit2>="))))
          (add-before 'check 'prepare-for-tests
            (lambda _
              ;; Find the 'gl' command.
              (rename-file "gl.py" "gl")
              (setenv "PATH" (string-append (getcwd) ":" (getenv "PATH")))

              ;; The tests try to run git as if it were already set up.
              (setenv "HOME" (getcwd))
              (invoke "git" "config" "--global" "user.email" "git@example.com")
              (invoke "git" "config" "--global" "user.name" "Guix")))
          (replace 'wrap
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((out #$output)
                    (git (search-input-file inputs "bin/git")))
                (wrap-program (string-append out "/bin/gl")
                  `("PATH" ":" prefix (,(dirname git)))
                  `("GUIX_PYTHONPATH" ":" =
                    (,(string-append out "/lib/python"
                                     #$(version-major+minor
                                        (package-version python))
                                     "/site-packages:")
                     ,(getenv "GUIX_PYTHONPATH"))))))))))
    (native-inputs (list git-minimal))
    (inputs
     (list bash-minimal
           git-minimal
           python-clint
           python-pygit2
           python-sh))
    (home-page "https://gitless.com")
    (synopsis "Simple version control system built on top of Git")
    (description
     "Gitless is a Git-compatible version control system that aims to be easy to
learn and use.  It simplifies the common workflow by committing changes to
tracked files by default and saving any uncommitted changes as part of a branch.

The friendly @command{gl} command-line interface gives feedback and helps you
figure out what to do next.

Gitless is implemented on top of Git and its commits and repositories are
indistinguishable from Git's.  You (or other contributors) can always fall back
on @command{git}, and use any regular Git hosting service.")
    (license license:expat)))

(define-public git-cal
  (package
    (name "git-cal")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/k4rthik/git-cal")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08s9sif3qlk5n2dqpzq5yjczggnqlnxldljspjdqgpfydb2dqg3z"))))
    (build-system perl-build-system)
    (home-page "https://github.com/k4rthik/git-cal/")
    (synopsis "GitHub like contributions calendar for terminal")
    (description "@code{git-cal} is a script to view commits calendar similar
to GitHub contributions calendar.")
    (license license:expat)))

(define-public git-tools
  (package
    (name "git-tools")
    (version "2022.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MestreLion/git-tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s8x74ggcr6nqzplr0jfzp3cavq0nmdm35hqywzs2bbq75i1mijd"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("git-branches-rename" "bin/git-branches-rename")
                        ("git-clone-subset" "bin/git-clone-subset")
                        ("git-find-uncommitted-repos"
                         "bin/git-find-uncommitted-repos")
                        ("git-rebase-theirs" "bin/git-rebase-theirs")
                        ("git-restore-mtime" "bin/git-restore-mtime")
                        ("git-strip-merge" "bin/git-strip-merge")
                        ("./man1/" "share/man/man1"
                         #:include-regexp (".*\\.1$")))))
    (inputs (list bash-minimal git-minimal python-minimal))
    (home-page "https://github.com/MestreLion/git-tools")
    (synopsis "Assorted git-related scripts and tools")
    (description
     "@code{git-tools} is a collection of bash and python scripts.
Specifically, it includes the following tools:

@itemize
@item @code{git-branches-rename}: Batch rename branches with a matching prefix
to another prefix
@item @code{git-clone-subset}: Clone a subset of a git repository
@item @code{git-find-uncommitted-repos}: Recursively list repositories in the
given directory(ies) that have uncommitted changes
@item @code{git-rebase-theirs}: Resolve rebase conflicts and failed
cherry-picks by favoring \"theirs\" version
@item @code{git-restore-mtime}: Restore modification time of files based on
the date of the most recent commit that modified them
@item @code{git-strip-merge}: A git-merge wrapper that deletes files on a
\"foreign\" branch before merging
@end itemize")
    (license license:gpl3+)))

(define-public git-spice
  (package
    (name "git-spice")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abhinav/git-spice")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yvnd5a3ql905jrxh0sq9sdcfmyq38fsbqx0zbhxbd4rgs8hv5s3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.abhg.dev/gs"
      #:install-source? #f
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; XXX: Tests failing with various reasons; requiring
               ;; networking config or write access, or outbound access, check
               ;; if some of them may be fixed.
               (list "TestDeviceFlowAuthenticator"
                     "TestScript/auth_detect_forge"
                     "TestScript/auth_explicit_forge"
                     "TestScript/auth_insecure_storage"
                     "TestScript/auth_prompt_forge"
                     "TestScript/branch_split_reassign_submitted"
                     "TestScript/branch_submit_ambiguous_branch"
                     "TestScript/branch_submit_by_name"
                     "TestScript/branch_submit_config_no_publish"
                     "TestScript/branch_submit_create_update"
                     "TestScript/branch_submit_detect_existing"
                     "TestScript/branch_submit_detect_existing_conflict"
                     "TestScript/branch_submit_detect_existing_upstream_name"
                     "TestScript/branch_submit_force_push"
                     "TestScript/branch_submit_long_body"
                     "TestScript/branch_submit_many_upstream_names_taken"
                     "TestScript/branch_submit_multiple_commits"
                     "TestScript/branch_submit_multiple_pr_templates"
                     "TestScript/branch_submit_navigation_.*_out_multiple"
                     "TestScript/branch_submit_needs_restack"
                     "TestScript/branch_submit_no_editor"
                     "TestScript/branch_submit_no_publish"
                     "TestScript/branch_submit_pr_template"
                     "TestScript/branch_submit_pr_template_cache_invalidation"
                     "TestScript/branch_submit_pr_template_no_body"
                     "TestScript/branch_submit_pr_template_prompt"
                     "TestScript/branch_submit_recover_prepared"
                     "TestScript/branch_submit_remote_prompt"
                     "TestScript/branch_submit_rename"
                     "TestScript/branch_submit_rename_base"
                     "TestScript/branch_submit_update_pr_is_closed"
                     "TestScript/branch_submit_update_pr_is_merged"
                     "TestScript/branch_submit_upstream_name"
                     "TestScript/branch_submit_upstream_name_wrong_remote"
                     "TestScript/branch_submit_use_git_editor"
                     "TestScript/branch_submit_web"
                     "TestScript/branch_submit_web_opt_out"
                     "TestScript/downstack_submit"
                     "TestScript/issue369_branch_.*_case_insensitive"
                     "TestScript/issue369_branch_.*_remote_update"
                     "TestScript/issue398_repo_sync_many_merged"
                     "TestScript/repo_sync_after_merging_renamed_branch"
                     "TestScript/repo_sync_detached_head"
                     "TestScript/repo_sync_detect_externally_created_prs"
                     "TestScript/repo_sync_external_pr_head_mismatch"
                     "TestScript/repo_sync_manual_pull_merged_pr"
                     "TestScript/repo_sync_merged_pr"
                     "TestScript/repo_sync_remote_already_deleted"
                     "TestScript/repo_sync_restack"
                     "TestScript/repo_sync_trunk_dirty_tree"
                     "TestScript/repo_sync_trunk_no_prs"
                     "TestScript/repo_sync_unpushed_commits"
                     "TestScript/stack_submit"
                     "TestScript/stack_submit_update_leave_draft"
                     "TestScript/stack_submit_web"
                     "TestScript/upstack_submit_main")
               "|"))))
    (native-inputs
     (list git-minimal ; for tests in testdata/scripts
           go-github-com-alecthomas-kong
           go-github-com-buildkite-shellwords
           go-github-com-charmbracelet-bubbles
           go-github-com-charmbracelet-bubbletea
           go-github-com-charmbracelet-lipgloss
           go-github-com-charmbracelet-log
           go-github-com-cli-browser
           go-github-com-creack-pty
           go-github-com-dustin-go-humanize
           go-github-com-mattn-go-isatty
           go-github-com-rogpeppe-go-internal
           go-github-com-sahilm-fuzzy
           go-github-com-shurcool-githubv4
           go-github-com-stretchr-testify
           go-github-com-tidwall-gjson
           go-github-com-vito-midterm
           go-github-com-xanzy-go-gitlab
           go-github-com-zalando-go-keyring
           go-go-abhg-dev-komplete
           go-go-abhg-dev-requiredfield
           go-go-abhg-dev-testing-stub
           go-go-uber-org-mock
           go-golang-org-x-oauth2
           go-gopkg-in-dnaeon-go-vcr-v4
           go-gopkg-in-yaml-v3
           go-pgregory-net-rapid))
    (home-page "https://go.abhg.dev/gs")
    (synopsis "Manage stacks of Git branches")
    (description
     "git-spice (@code{gs}) is a command line tool for stacking Git branches,
a collection of branches expecting the trunk has a base branch.  It manages
and navigates stacks of branches, conveniently modifies and rebases them also
provides an integration with GitHub and GitLab.")
    (license license:gpl3)))

(define-public got
  (package
    (name "got")
    (version "0.110")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://gameoftrees.org/releases/portable/got-portable-"
                version ".tar.gz"))
              (sha256
               (base32
                "0a8xcqqw09bddzgqqg1mrwqp36fqsf2zfvz7lqv55y770l9f8d9n"))))
    (inputs
     (list libevent
           `(,util-linux "lib")
           zlib
           libressl
           libmd
           libbsd
           ncurses))
    (native-inputs
     (list pkg-config perl))
    (arguments
     `(;; disable runpath validation, courtesy: libbsd's special
       ;; treatment of libmd, as it embeds path to libmd.so
       #:validate-runpath? #f
       ;; default values of GOT_*_PATH_* point to /usr/bin
       #:make-flags
       '("CFLAGS+=-DGOT_DIAL_PATH_SSH=\\\"ssh\\\""
         "CFLAGS+=-DGOT_TAG_PATH_SSH_KEYGEN=\\\"ssh-keygen\\\""
         "CFLAGS+=-DGOT_TAG_PATH_SIGNIFY=\\\"signify\\\"")
      #:phases ,#~(modify-phases %standard-phases
                    (add-after 'unpack 'patch-execv-to-execvp
                      (lambda _
                        ;; got sources has paths hardcoded to /usr/bin
                        (substitute* "lib/dial.c"
                          (("execv\\(GOT_DIAL_") "execvp(GOT_DIAL_")
                          (("execv %s\", GOT_DIAL") "execvp %s\", GOT_DIAL"))
                        (substitute* "lib/sigs.c"
                          (("execv\\(GOT_TAG") "execvp(GOT_TAG")
                          (("execv %s\", GOT_TAG") "execvp %s\", GOT_TAG")))))))
    (build-system gnu-build-system)
    (synopsis "Distributed version control system")
    (description
     "Game of Trees (Got) is a version control system which prioritizes ease of use
and simplicity over flexibility.")
    (license license:isc)
    (home-page "https://gameoftrees.org/")))

(define-public xdiff
  (let ((revision "0")
        (commit "a137bc7ee6c76618ed1737c257548eaa10ac0089"))
    (package
      (name "xdiff")
      ;; The base version is taken from the CMakeLists.txt file.
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/libgit2/xdiff")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1rxzpag2pih64qlgq40xg1z6mz0bzvps4baxw7bmykyhjhc2gx75"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:modules '((guix build cmake-build-system)
                    (guix build utils)
                    (srfi srfi-26))
        #:tests? #f                     ;no test suite
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'create-shared-library
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("add_library\\(xdiff STATIC")
                   "add_library(xdiff SHARED"))))
            (replace 'install           ;no install target
              (lambda _
                (with-directory-excursion "../source"
                  (for-each (cute install-file <>
                                  (string-append #$output "/include"))
                            (list "xdiff.h"
                                  "git-xdiff.h"))) ;included by xdiff.h
                (install-file "libxdiff.so"
                              (string-append #$output "/lib")))))))
      (home-page "https://github.com/libgit2/xdiff")
      (synopsis "File differential library used by git")
      (description "@code{xdiff} is the file differential library used by git,
which has been extracted into a standalone library for compatibility with
other git-like projects such as @code{libgit2}.")
      (license license:lgpl2.1+))))

(define-public libgit2-1.5
  (package
    (name "libgit2")
    (version "1.5.2")
    (source (origin
              ;; Since v1.1.1, release artifacts are no longer offered (see:
              ;; https://github.com/libgit2/libgit2/discussions/5932#discussioncomment-1682729).
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libgit2/libgit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0v9jdaxmqrzbs9v5vhh2xf5xv9h29q8qqn8vmns279ljx1zav5yd"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "deps")))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags
       (list "-DUSE_NTLMCLIENT=OFF" ;TODO: package this
             "-DREGEX_BACKEND=pcre2"
             "-DUSE_HTTP_PARSER=system"
             "-DUSE_SSH=ON" ; cmake fails to find libssh if this is missing
             ,@(if (%current-target-system)
                   `((string-append
                      "-DPKG_CONFIG_EXECUTABLE="
                      (search-input-file
                       %build-inputs
                       (string-append "/bin/" ,(%current-target-system)
                                      "-pkg-config"))))
                   '()))
       #:phases
       (modify-phases %standard-phases
         ,@(if (or (target-arm32?) (target-hurd?))
             ;; Some tests are flaky on armhf.
             ;; On GNU/Hurd, the 'diff/workdir' test in libgit2 1.7.1 fails
             ;; while comparing st.st_size to zero.
             '((add-before 'check 'pre-check
                 (lambda _
                   (setenv "GITTEST_FLAKY_STAT" "true"))))
             '())
         ;; Run checks more verbosely, unless we are cross-compiling.
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (if tests?
                 (invoke "./libgit2_tests" "-v" "-Q")
                 ;; Tests may be disabled if cross-compiling.
                 (format #t "Test suite not run.~%")))))))
    (inputs
     (list libssh2 http-parser))
    (native-inputs
     (list pkg-config python))
    (propagated-inputs
     ;; These libraries are in 'Requires.private' in libgit2.pc.
     (list openssl pcre2 zlib))
    (home-page "https://libgit2.org/")
    (synopsis "Library providing Git core methods")
    (description
     "Libgit2 is a portable, pure C implementation of the Git core methods
provided as a re-entrant linkable library with a solid API, allowing you to
write native speed custom Git applications in any language with bindings.")
    ;; GPLv2 with linking exception
    (license license:gpl2)))

(define-public libgit2
  ;; Default version of libgit2.
  libgit2-1.5)

(define-public libgit2-1.7
  (package
    (inherit libgit2)
    (version "1.7.2")
    (source (origin
              (inherit (package-source libgit2))
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libgit2/libgit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "libgit2" version))
              (sha256
               (base32
                "0i95jwrwx4svh5l4dpa5r4a99f813hlm7nzzkbqzmnw4pkyxhlvx"))
              ;; We need to use the bundled xdiff until an option is given
              ;; to use the one from git.
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                            '("deps/chromium-zlib"
                              "deps/http-parser"
                              "deps/ntlmclient"
                              "deps/pcre"
                              "deps/winhttp"
                              "deps/zlib"))))))))

(define-public libgit2-1.8
  (package
    (inherit libgit2-1.7)
    (version "1.8.4")
    (source (origin
              (inherit (package-source libgit2-1.7))
              (uri (git-reference
                    (url "https://github.com/libgit2/libgit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "libgit2" version))
              (sha256
               (base32
                "0jydckwn0bbrp2kbcr1ih1bz4sc6yhx7lrl22lqcgnf2v6ml6n01"))
              (patches
               (search-patches "libgit2-uninitialized-proxy-settings.patch"))
	      (snippet
               '(begin
                  (for-each delete-file-recursively
                            '("deps/chromium-zlib"
                              "deps/llhttp"
                              "deps/ntlmclient"
                              "deps/pcre"
                              "deps/winhttp"
                              "deps/zlib"))))))
    (arguments (substitute-keyword-arguments (package-arguments libgit2-1.7)
                 ((#:configure-flags flags #~(list))
                  #~(map (lambda (arg)
                           (if (string= "-DUSE_HTTP_PARSER=system" arg)
                               "-DUSE_HTTP_PARSER=http-parser"
                               arg))
                         #$flags))))))

(define-public libgit2-1.9
  (package
    (inherit libgit2-1.8)
    (version "1.9.0")
    (source (origin
              (inherit (package-source libgit2-1.8))
              (uri (git-reference
                    (url "https://github.com/libgit2/libgit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "libgit2" version))
              (sha256
               (base32
                "06ajn5i5l1209z7x7jxcpw68ph0a6g3q67bmx0jm381rr8cb4zdz"))))))

(define-public libgit2-1.6
  (package
    (inherit libgit2)
    (version "1.6.5")
    (source (origin
              (inherit (package-source libgit2))
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libgit2/libgit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "libgit2" version))
              (sha256
               (base32
                "1v8sndvknsknf0i967qidmz73q9jx928iq7fqqgx3rbwn2g1gn6s"))))))

(define-public libgit2-1.4
  (package
    (inherit libgit2)
    (version "1.4.6")
    (source (origin
              (inherit (package-source libgit2))
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libgit2/libgit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "libgit2" version))
              (sha256
               (base32
                "0iv7h2fdnlv5vj4dx09w71xbj004hidbpsbgv02gbvlpvsz3jpcf"))))))

(define-public libgit2-1.3
  (package
    (inherit libgit2-1.4)
    (version "1.3.2")
    (source (origin
              (inherit (package-source libgit2-1.4))
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libgit2/libgit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "libgit2" version))
              (sha256
               (base32
                "1dngga8jq419z6ps65wpmh2jihcf70k6r98pb1m1yiwj7qqh9792"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libgit2)
       ((#:phases _ '%standard-phases)
        `(modify-phases %standard-phases
           ;; Run checks more verbosely, unless we are cross-compiling.
           (replace 'check
             (lambda* (#:key (tests? #t) #:allow-other-keys)
               (if tests?
                   (invoke "./libgit2_clar" "-v" "-Q")
                   ;; Tests may be disabled if cross-compiling.
                   (format #t "Test suite not run.~%"))))))))))

(define-public git-issue
  (let ((commit "d056998566d30235072b97982756ff607e9ecce9")
        (revision "0"))
    (package
      (name "git-issue")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dspinellis/git-issue")
                      (commit commit)))
                (sha256
                 (base32
                  "0002bjzv6rgpxbbsjiswg73prl7iq217qvafbxhsjp2wjj00i0sm"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags #~(list (string-append "PREFIX=" #$output))
             #:test-target "test"
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure) ;no configure script
                 (add-before 'build 'generate-docs
                   (lambda _
                     (invoke "make" "sync-docs")))
                 (add-before 'check 'fix-tests
                   (lambda _
                     (substitute* "test.sh"
                       ;; Skip 3 failing tests.
                       (("fail \"Uncommitted files sync-docs.*")
                        "ok \"ignored\"\n")
                       (("try_grep '\\^Tags:\\.\\*cloned'")
                        "ok \"ignored\"")
                       (("try \"\\$gi\" tag \"\\$issue\" cloned")
                        "ok \"ignored\"")
                       ;; Fix a test.
                       (("#!/bin/sh") (string-append "#!" (which "sh")))))))))
      (native-inputs (list git-minimal util-linux))
      (inputs (list jq curl))
      (synopsis "Git-based decentralized issue management")
      (description
       "This is a minimalist decentralized issue management system based on
Git, offering (optional) bidirectional integration with GitHub and GitLab
issue management.")
      (home-page "https://github.com/dspinellis/git-issue")
      (license license:gpl3+))))

(define-public git-crypt
  (package
    (name "git-crypt")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AGWA/git-crypt")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ymk2z0jfyhycia8hg6wbj2g06m163yzqzanfk172cxb13fa8c26"))))
    (build-system gnu-build-system)
    (inputs
     (list git openssl))
    (native-inputs
     (list docbook-xml-4.2 docbook-xsl libxslt))
    (arguments
     (list
      #:tests? #f                       ; No tests.
      #:make-flags
      #~(list
         "ENABLE_MAN=yes"
         ;; Add flag to work around OpenSSL 3 incompatibility.
         ;; See <https://github.com/AGWA/git-crypt/issues/232>.
         "CXXFLAGS+=-DOPENSSL_API_COMPAT=0x30000000L"
         (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (home-page "https://www.agwa.name/projects/git-crypt/")
    (synopsis "Transparent encryption of files in a git repository")
    (description "git-crypt enables transparent encryption and decryption of
files in a git repository.  Files which you choose to protect are encrypted when
committed, and decrypted when checked out.  git-crypt lets you freely share a
repository containing a mix of public and private content.  git-crypt gracefully
degrades, so developers without the secret key can still clone and commit to a
repository with encrypted files.  This lets you store your secret material (such
as keys or passwords) in the same repository as your code, without requiring you
to lock down your entire repository.")
    (license license:gpl3+)))

(define-public git-remote-gcrypt
  (package
   (name "git-remote-gcrypt")
   (version "1.5")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://git.spwhitton.name/git-remote-gcrypt")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1m1wlbqpqyhh2z0ka3gjs5yabd32nnkzw5hak6czcqrhhkfsqbmv"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let* ((source (assoc-ref %build-inputs "source"))
                         (output (assoc-ref %outputs "out"))
                         (bindir (string-append output "/bin")))
                    (install-file (string-append source "/git-remote-gcrypt")
                                  bindir)))))
   (home-page "https://spwhitton.name/tech/code/git-remote-gcrypt/")
   (synopsis "Whole remote repository encryption")
   (description "git-remote-gcrypt is a Git remote helper to push and pull from
repositories encrypted with GnuPG.  It works with the standard Git transports,
including repository hosting services like GitLab.

Remote helper programs are invoked by Git to handle network transport.  This
helper handles @code{gcrypt:} URLs that access a remote repository encrypted
with GPG, using our custom format.

Supported locations are local, @code{rsync://} and @code{sftp://}, where the
repository is stored as a set of files, or instead any Git URL where gcrypt
will store the same representation in a Git repository, bridged over arbitrary
Git transport.

The aim is to provide confidential, authenticated Git storage and
collaboration using typical untrusted file hosts or services.")
   (license license:gpl3+)))

(define-public cgit
  ;; Use the latest commit, as the latest tagged release is 5 years old.
  (let ((commit "994d3fe1a8fa56d5a1dd36aae62c788169160c3a")
        (rev "9"))
    (package
      (name "cgit")
      ;; Update the ‘git-source’ input as well.
      (version (git-version "1.2.3" rev commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.zx2c4.com/cgit")
                      (commit commit)))
                (sha256
                 (base32
                  "1c38yh4y5xmc8lnf55q46v63vkwpa3vs9mj17a74i66lm8zhrhk7"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                    ; XXX: fail to build the in-source git.
        #:test-target "test"
        #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                             "SHELL_PATH=sh")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'unpack-git
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Unpack the source of git into the 'git' directory.
                (invoke "tar" "--strip-components=1" "-C" "git" "-xf"
                        #$(this-package-input "git-source.tar.xz"))))
            (add-after 'unpack 'patch-absolute-file-names
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (define (quoted-file-name input path)
                  (string-append "\"" input path "\""))
                (substitute* "ui-snapshot.c"
                  (("\"gzip\"")
                   (quoted-file-name (assoc-ref inputs "gzip") "/bin/gzip"))
                  (("\"bzip2\"")
                   (quoted-file-name (assoc-ref inputs "bzip2") "/bin/bzip2"))
                  (("\"xz\"")
                   (quoted-file-name (assoc-ref inputs "xz") "/bin/xz")))

                (substitute* "filters/about-formatting.sh"
                  (("\\$\\(dirname \\$0\\)") (string-append (assoc-ref outputs "out")
                                                        "/lib/cgit/filters"))
                  (("\\| tr") (string-append "| " (which "tr"))))

                (substitute* "filters/html-converters/txt2html"
                  (("sed") (which "sed")))

                (substitute* "filters/html-converters/man2html"
                  (("groff") (which "groff")))

                (substitute* "filters/html-converters/rst2html"
                  (("rst2html\\.py") (which "rst2html.py")))))
            (delete 'configure)         ; no configure script
            (add-after 'build 'build-man
              (lambda* (#:key make-flags #:allow-other-keys)
                (apply invoke "make" "doc-man" make-flags)))
            (replace 'install
              (lambda* (#:key make-flags outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (apply invoke
                         "make" "install" "install-man"
                         (string-append "prefix=" out)
                         (string-append "CGIT_SCRIPT_PATH=" out "/share/cgit")
                         make-flags)
                  ;; Move the platform-dependent 'cgit.cgi' into lib to get it
                  ;; stripped.
                  (rename-file (string-append out "/share/cgit/cgit.cgi")
                               (string-append out "/lib/cgit/cgit.cgi")))))
            (add-after 'install 'wrap-python-scripts
              (lambda* (#:key outputs #:allow-other-keys)
                (for-each
                 (lambda (file)
                   (wrap-program (string-append (assoc-ref outputs "out")
                                                "/lib/cgit/filters/" file)
                     `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))))
                 '("syntax-highlighting.py"
                   "html-converters/md2html")))))))
      (native-inputs
       ;; For building manpage.
       (list asciidoc))
      (inputs
       (list (origin
               (method url-fetch)
               ;; Building cgit requires a Git source tree.
               ;; cgit is tightly bound to git.  Use GIT_VER from the Makefile,
               ;; which may not match the current (package-version git).
               (uri "mirror://kernel.org/software/scm/git/git-2.49.0.tar.xz")
               (sha256
                (base32
                 "0a2nm2szhn47dm0m1f1kmg1rikb7saqj67zr25n9yzhbb77r10b1"))
               (file-name "git-source.tar.xz"))
             bash-minimal
             openssl
             python
             python-docutils
             python-markdown
             python-pygments
             zlib
             ;; bzip2, groff, gzip and xz are inputs (not native inputs)
             ;; since they are actually substituted into cgit source and
             ;; referenced by the built package output.
             bzip2
             groff
             gzip
             xz))
      (home-page "https://git.zx2c4.com/cgit/")
      (synopsis "Web frontend for git repositories")
      (description
       "CGit is an attempt to create a fast web interface for the Git SCM, using
a built-in cache to decrease server I/O pressure.")
      (license license:gpl2))))

(define-public cgit-pink
  (package
    (inherit cgit)
    (name "cgit-pink")
    (version "1.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.causal.agency/cgit-pink")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yp6rm60pz8pj8wrm1aglix51hhy00al86mm94ag2bifc92q23ar"))))
    (arguments
     (substitute-keyword-arguments (package-arguments cgit)
       ((#:tests? _ #f)
        (not (%current-target-system)))
       ((#:make-flags _ '())
        #~(list (string-append "CC=" #$(cc-for-target))
                (string-append "PERL_PATH="
                               (search-input-file %build-inputs "/bin/perl"))
                ;; It is important to set an absolute path in SHELL_PATH
                ;; because it is used as the shebang of generated scripts that
                ;; are invoked during the test phase.
                (string-append "SHELL_PATH="
                               (search-input-file %build-inputs "/bin/sh"))))))
    (inputs
     (modify-inputs (package-inputs cgit)
       (replace "git-source"
         ;; cgit-pink is tightly bound to git. Use GIT_VER from the Makefile,
         ;; which may not match the current (package-version git).
         (origin
           (method url-fetch)
           (uri "mirror://kernel.org/software/scm/git/git-2.36.1.tar.xz")
           (sha256
            (base32
             "0w43a35mhc2qf2gjkxjlnkf2lq8g0snf34iy5gqx2678yq7llpa0"))))))
    (native-inputs
     (modify-inputs (package-native-inputs cgit)
       (append gnu-gettext perl)))
    (home-page "https://git.causal.agency/cgit-pink/about/")
    (description "cgit-pink is a fast web interface for the Git SCM, using a
built-in cache to decrease server I/O pressure.  cgit-pink is a fork of
cgit.")))

(define-public python-git-multimail
  (package
    (name "python-git-multimail")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "git-multimail" version))
       (sha256
        (base32
         "0hwgf2p2dd4z397wj0y558s8xxbkzbsa6yb9n1iax624y7swjng1"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("'git-multimail', 'README.rst'")
                "'README.rst'"))
             (substitute* "git-multimail/git_multimail.py"
               (("GIT_EXECUTABLE = 'git'")
                (string-append "GIT_EXECUTABLE = '"
                               (assoc-ref inputs "git") "/bin/git"
                               "'"))
               (("/usr/sbin/sendmail")
                (search-input-file inputs
                                   "/sbin/sendmail"))))))))
    (inputs
     (list git sendmail))
    (home-page "https://github.com/git-multimail/git-multimail")
    (synopsis "Send notification emails for Git pushes")
    (description
     "This hook sends emails describing changes introduced by pushes to a Git
repository.  For each reference that was changed, it emits one ReferenceChange
email summarizing how the reference was changed, followed by one Revision
email for each new commit that was introduced by the reference change.

This script is designed to be used as a post-receive hook in a Git
repository")
    (license license:gpl2)))

(define-public python-ghp-import
  (package
    (name "python-ghp-import")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/davisp/ghp-import")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i4lxsgqri1y8sw4k44bkwbzmdmk4vpmdi882mw148j8gk4i7vvj"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'install-documentation
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (doc (string-append out "/share/doc"))
                             (licenses (string-append out "/share/licenses")))
                        (install-file "README.md" doc)
                        (install-file "LICENSE" licenses)))))))
    (propagated-inputs (list python-dateutil))
    (home-page "https://github.com/davisp/ghp-import")
    (synopsis "Copy directory to the gh-pages branch")
    (description "Script that copies a directory to the gh-pages branch (by
default) of the repository.")

    ;; See <https://bugs.gnu.org/27913>.
    (license (license:non-copyleft
              "https://raw.githubusercontent.com/davisp/ghp-import/master/LICENSE"
              "Tumbolia Public License"))))

(define-public python-gitdb
  (package
    (name "python-gitdb")
    (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gitdb" version))
              (sha256
               (base32
                "0l113fphn6msjl3cl3kyf332b6lal7daxdd0nfma0x9ipfb013jr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'create-test-repository
                    (lambda _
                      (mkdir "/tmp/testrepo")
                      ;; Some tests require a git repository, so create one.
                      (with-directory-excursion "/tmp/testrepo"
                        (do ((filecount 1 (1+ filecount)))
                            ((> filecount 1000))
                          (call-with-output-file (string-append
                                                  "file" (number->string filecount))
                            (lambda (port)
                              (format port "~a" filecount))))
                        (begin
                         (invoke "git" "init")
                         (invoke "git" "config" "user.name" "Total Git")
                         (invoke "git" "config" "user.email" "git@localhost")
                         (invoke "git" "add" "-A")
                         (invoke "git" "commit" "-q" "-m" "dummy commit")))

                      ;; The repository checkout must be a "bare" clone.
                      (invoke "git" "clone" "--bare" "/tmp/testrepo"
                              "/tmp/testrepo.git")))
                  (replace 'check
                    (lambda _
                      (setenv "GITDB_TEST_GIT_REPO_BASE" "/tmp/testrepo.git")
                      ;; Skip tests that must be run from the gitdb repository.
                      (setenv "TRAVIS" "1")
                      (invoke "nosetests" "-v"))))))
    (propagated-inputs
     (list python-smmap))
    (native-inputs
     (list git-minimal/pinned python-nose))
    (home-page "https://github.com/gitpython-developers/gitdb")
    (synopsis "Python implementation of the Git object database")
    (description
     "GitDB allows you to access @dfn{bare} Git repositories for reading and
writing.  It aims at allowing full access to loose objects as well as packs
with performance and scalability in mind.  It operates exclusively on streams,
allowing to handle large objects with a small memory footprint.")
    (license license:bsd-3)))

(define-public python-gitpython
  (package
    (name "python-gitpython")
    (version "3.1.24")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "GitPython" version))
              (sha256
               (base32
                "1rarp97cpjnhi106k2yhb7kygdyflmlgq0icxv3ggzl4wvszv0yz"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f ;XXX: tests can only be run within the GitPython repository
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'embed-git-reference
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "git/cmd.py"
                     (("git_exec_name = \"git\"")
                      (string-append "git_exec_name = \""
                                     (search-input-file inputs "/bin/git")
                                     "\""))))))))
    (inputs
     (list git-minimal/pinned))
    (propagated-inputs
     (list python-gitdb python-typing-extensions))
    (native-inputs
     (list python-ddt python-nose))
    (home-page "https://github.com/gitpython-developers/GitPython")
    (synopsis "Python library for interacting with Git repositories")
    (description
     "GitPython is a python library used to interact with Git repositories,
high-level like git-porcelain, or low-level like git-plumbing.

It provides abstractions of Git objects for easy access of repository data,
and additionally allows you to access the Git repository more directly using
either a pure Python implementation, or the faster, but more resource intensive
 @command{git} command implementation.")
     (license license:bsd-3)))

(define-public savane
  (package
    (name "savane")
    (version "3.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/administration/savane")
                    (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10jg264wqmkc87nz0d8d2pq4hvradwqrvrpvgpz3h409y6c6v78z"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           imagemagick))
    (inputs
     (list exim
           gnupg
           httpd
           mariadb
           php))
    (propagated-inputs
     (list perl
           perl-dbd-mysql
           perl-dbi
           perl-date-calc
           perl-digest-md5
           perl-mailtools
           perl-file-find-rule
           perl-xml-writer))
    (synopsis "Web-based software hosting system")
    (description
     "Savane is a Web-based software hosting system.  It includes issue
tracking (bugs, tasks, support, news and documentation), project member
management by roles and individual account maintenance.")
    (home-page "https://savannah.nongnu.org/p/administration")
    (license license:agpl3+)))

 (define-public shflags
   (package
     (name "shflags")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kward/shflags")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jj0zkly8yg42b8jvih2cmmafv95vm8mv80n3dyalvr5i14lzqd8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; nothing to configure
         (delete 'build)                ; nothing to build
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (src (string-append out "/src")))
               (install-file "shflags" src)
               #t))))))
    (home-page "https://github.com/kward/shflags")
    (synopsis "Command-line flags library for shell scripts")
    (description
     "Shell Flags (shFlags) is a library written to greatly simplify the
handling of command-line flags in Bourne based Unix shell scripts (bash, dash,
ksh, sh, zsh).  Most shell scripts use getopt for flags processing, but the
different versions of getopt on various OSes make writing portable shell
scripts difficult.  shFlags instead provides an API that doesn't change across
shell and OS versions so the script writer can be confident that the script
will work.")
    (license license:lgpl2.1)))

(define-public trac
  (package
    (name "trac")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Trac" version))
       (sha256
        (base32 "013kqa93kd1giswir9qsasm5080x5x5x4ab86ky8zmkhyrhkrmv1"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list "-k"
                                ;; XXX: these two tests fail, check why.
                                (string-append
                                 "not test_remove_composite_keys"
                                 " and not test_remove_simple_keys"))))
    (native-inputs (list python-psycopg2 python-pymysql python-pytest))
    (propagated-inputs (list python-jinja2 python-multipart))
    (home-page "https://trac.edgewall.org")
    (synopsis "Integrated SCM, wiki, issue tracker and project environment")
    (description "Trac is a minimalistic web-based software project management
and bug/issue tracking system.  It provides an interface to the Git and
Subversion revision control systems, an integrated wiki, flexible issue
tracking and convenient report facilities.")
    (license license:bsd-3)))

(define-public git-flow
  (package
    (name "git-flow")
    ;; This version has not be officially released yet, so we build it
    ;; directly from the git repository.
    (version "1.12.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/petervanderdoes/gitflow-avh/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13q4mnrxr03wz2dkhzy73j384g299m4d545cnhxcaznvdwfany4h"))))
    (build-system gnu-build-system)
    (inputs (list shflags))
    (arguments
     '(#:tests? #f                    ; no tests
       #:make-flags (list (string-append "prefix="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'reset-shFlags-link
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The link points to a file in the shFlags submodule.
             ;; Redirect it to point to our system shFlags.
             (let ((shflags (assoc-ref inputs "shflags")))
               (begin
                 (delete-file "gitflow-shFlags")
                 (symlink (string-append shflags "/src/shflags")
                          "gitflow-shFlags")))))
         (delete 'configure)
         (delete 'build))))
    (home-page "https://nvie.com/posts/a-successful-git-branching-model/")
    (synopsis "Git extensions for Vincent Driessen's branching model")
    (description
     "Vincent Driessen's branching model is a git branching and release
management strategy that helps developers keep track of features, hotfixes,
and releases in bigger software projects.  The git-flow library of git
subcommands helps automate some parts of the flow to make working with it a
lot easier.")
    (license license:bsd-2)))

(define-public stgit-2
  (package
    (name "stgit")
    (version "2.4.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stacked-git/stgit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kp3gwmxcjvphg1s0san0vyis8dsdaf02xsflc2b7kkg8m0r0mi3"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* (find-files "." "^Cargo\\.toml$")
                  (("\"~([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstyle" ,rust-anstyle-1)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-bzip2-rs" ,rust-bzip2-rs-0.1)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-ctrlc" ,rust-ctrlc-3)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-encoding_rs" ,rust-encoding-rs-0.8)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix" ,rust-gix-0.66)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-is-terminal" ,rust-is-terminal-0.4)
                       ("rust-jiff" ,rust-jiff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-strsim" ,rust-strsim-0.10)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.6))
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-extras
           (lambda _
             (substitute* "Documentation/Makefile"
               (("docbook2x-texi") "docbook2texi"))
             (setenv "PERL_PATH" "perl")
             (invoke "make" "-C" "Documentation" "info")
             (invoke "make" "-C" "completion" "stgit.bash")
             (invoke "make" "-C" "completion" "stg.fish")))
         (add-after 'install 'install-extras
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make" "-C" "Documentation" "install-info"
                       (string-append "prefix=" out))
               (invoke "make" "-C" "completion" "install"
                       (string-append "prefix=" out)
                       (string-append "bashdir=" out "/etc/bash_completion.d/"))))))))
    (native-inputs
     (list pkg-config
           ;; For the documentation
           asciidoc
           docbook2x
           libxslt
           perl
           texinfo
           xmlto))
    (inputs (list openssl zlib curl))
    (home-page "https://stacked-git.github.io/")
    (synopsis "Stacked Git (StGit) manages Git commits as a stack of patches")
    (description "StGit uses a patch stack workflow.  Each individual patch
focuses on a single concern, while a stack of patches forms a series of commits.
Patches are stored as normal git commits, allowing easy merging of StGit
patches into other repositories using standard Git.

Features include:
@itemize
@item Import and export patches from Git with @command{stg commit} and
@command{stg uncommit}
@item Create new patches and add them to the stack with @command{stg new}
@item Update a patch from the working tree with @command{stg refresh} and
@command{stg edit}
@item See information about the stack or patch with @command{stg series} and
@command{stg show}
@item Export and send a series of patches by email using @command{stg email}
@end itemize")
    (license license:gpl2)))

(define-public emacs-stgit
  (package
    (inherit stgit-2)
    (name "emacs-stgit")
    (build-system emacs-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'enter-lisp-directory
            (lambda _
              (chdir "contrib")))
          (add-before 'install-license-files 'leave-lisp-directory
            (lambda _
              (chdir ".."))))))
    (synopsis "Emacs major mode for StGit interaction")
    (description "This package a interactive tool to interact with git
branches using StGit.")
    (license license:gpl3+)))

(define-public stgit
  (package
    (name "stgit")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ctmarinas/stgit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1igljjpdgl4na1a5hi0nmg36ph0hw6hw8hhq5436fgcl8yjimyz3"))))
    (build-system python-build-system)
    (native-inputs
     (list perl))
    (inputs
     (list git))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'hard-code-version
           (lambda _
             ;; setup.py tries to cleverly extract the version number from the
             ;; git history, which the source checkout lacks.  Hard-code one.
             (substitute* "setup.py"
               (("get_ver\\(\\)")
                (format #f "'~a'" ,version)))
             #t))
         (add-before 'check 'patch-tests
           (lambda _
             (substitute* (list "t/t1900-mail.sh"
                                "t/t7504-commit-msg-hook.sh")
               (("/bin/sh")
                (which "bash")))
             #t))
         (replace 'check
           (lambda _
             (invoke "make"
                     "PERL_PATH=perl"
                     (string-append "SHELL_PATH=" (which "bash"))
                     "test"))))))
    (home-page "https://stacked-git.github.io/")
    (synopsis "Stacked Git")
    (description
     "StGit is a command-line application that provides functionality similar
to Quilt (i.e., pushing/popping patches to/from a stack), but using Git
instead of @command{diff} and @command{patch}.  StGit stores its patches in a
Git repository as normal Git commits, and provides a number of commands to
manipulate them in various ways.")
    (license license:gpl2)))

(define-public vcsh
  (package
    (name "vcsh")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RichiH/vcsh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15lb09c2q261p1pp5r7j9k8389ybrd2q19xhnp1nnha6gs78i4wq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           ;; for man page
           ronn-ng
           ;; for tests
           perl
           perl-test-harness
           perl-shell-command
           perl-test-most
           ;; for bash-completion
           pkg-config))
    (inputs
     (list git))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'fix-version-gen
           (lambda _
             (call-with-output-file ".tarball-version"
               (lambda (port)
                 (display version port))))))
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:test-target "test"))
    (home-page "https://github.com/RichiH/vcsh")
    (synopsis "Version control system for @code{$HOME}")
    (description
     "vcsh version-controls configuration files in several Git repositories,
all in one single directory.  They all maintain their working trees without
clobbering each other or interfering otherwise.  By default, all Git
repositories maintained via vcsh store the actual files in @code{$HOME},
though this can be overridden.")
    (license license:gpl2+)))

(define-public git-test-sequence
  (let ((commit "48e5a2f5a13a5f30452647237e23362b459b9c76"))
    (package
      (name "git-test-sequence")
      (version (string-append "20140312." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; There are many other scripts in this directory; we
                      ;; are interested in just one for this package.
                      (url "https://github.com/dustin/bindir")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dcq0y16yznbv4k9h8gg90kv1gkn8r8dbvl4m2rpfd7q5nqhn617"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils))
                     (let* ((source (assoc-ref %build-inputs "source"))
                            (output (assoc-ref %outputs "out"))
                            (bindir (string-append output "/bin"))
                            (script "git-test-sequence"))
                       (install-file (string-append source "/" script)
                                     bindir)
                       #t))))
      (home-page "https://dustin.sallings.org/2010/03/28/git-test-sequence.html")
      (synopsis "Run a command over a sequence of commits")
      (description
       "git-test-sequence is similar to an automated git bisect except it’s
linear.  It will test every change between two points in the DAG.  It will
also walk each side of a merge and test those changes individually.")
      (license (license:x11-style "file://LICENSE")))))

(define* (make-gitolite #:optional (extra-inputs '()))
  "Make a gitolite package object with EXTRA-INPUTS added to the binary
wrappers, to be used for optional gitolite extensions."
  (package
    (name "gitolite")
    (version "3.6.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sitaramc/gitolite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lp4hi8pfg7k0fk0l8wzs8hxp1aspzv78nkafdbbq8m9lzwnwl7x"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (delete 'build)
               (add-before 'install 'patch-scripts
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; This seems to take care of every shell script that
                   ;; invokes Perl.
                   (substitute* (find-files ".")
                     ((" perl -")
                      (string-append
                       " " (search-input-file inputs "bin/perl") " -")))

                   (substitute* (find-files "src/triggers" ".*")
                     ((" sed ")
                      (string-append
                       " " (search-input-file inputs "bin/sed") " ")))

                   (substitute*
                       '("src/triggers/post-compile/update-gitweb-access-list"
                         "src/triggers/post-compile/ssh-authkeys-split"
                         "src/triggers/upstream")
                     ((" grep ")
                      (string-append
                       " " (search-input-file inputs "bin/grep") " ")))

                   (substitute* "src/triggers/post-compile/ssh-authkeys"
                     (("\\$glshell \\$user")
                      (string-append
                       #$output "/bin/gitolite-shell $user")))))
               (add-before 'install 'patch-source
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Gitolite uses cat to test the readability of the
                   ;; pubkey
                   (substitute* "src/lib/Gitolite/Setup.pm"
                     (("\"cat ")
                      (string-append
                       "\"" (search-input-file inputs "bin/cat") " "))
                     (("\"ssh-keygen")
                      (string-append
                       "\"" (search-input-file inputs "bin/ssh-keygen"))))

                   (substitute* '("src/lib/Gitolite/Hooks/PostUpdate.pm"
                                  "src/lib/Gitolite/Hooks/Update.pm")
                     (("/usr/bin/perl")
                      (search-input-file inputs "bin/perl")))

                   (substitute* "src/lib/Gitolite/Common.pm"
                     (("\"ssh-keygen")
                      (string-append
                       "\"" (search-input-file inputs "bin/ssh-keygen")))
                     (("\"logger\"")
                      (string-append
                       "\"" (search-input-file inputs "bin/logger") "\"")))

                   (substitute* "src/lib/Gitolite/Cache.pm"
                     (("/usr/sbin/redis-server") "redis-server"))

                   (substitute* "src/commands/svnserve"
                     (("/usr/bin/svnserve") "svnserve"))))
               (replace 'install
                 (lambda* _
                   (let* ((sharedir (string-append #$output "/share/gitolite"))
                          (bindir (string-append #$output "/bin")))
                     (mkdir-p sharedir)
                     (mkdir-p bindir)
                     (invoke "./install" "-to" sharedir)
                     ;; Create symlinks for executable scripts in /bin.
                     (for-each (lambda (script)
                                 (symlink (string-append sharedir "/" script)
                                          (string-append bindir "/" script)))
                               '("gitolite" "gitolite-shell")))))
               (add-after 'install 'wrap-scripts
                 (lambda* (#:key inputs #:allow-other-keys)
                   (for-each (lambda (file-name)
                               (wrap-program (string-append #$output file-name)
                                 `("PATH" ":" prefix
                                   ,(append
                                     (map (lambda (command)
                                            (dirname
                                             (search-input-file
                                              inputs
                                              (string-append "bin/" command))))
                                          '("chmod" ;coreutils
                                            "find"
                                            "git"))
                                     (map (lambda (dir)
                                           (string-append dir "/bin"))
                                         (list #$output
                                               #$@extra-inputs))))))
                             '("/bin/gitolite" "/bin/gitolite-shell")))))))
    (inputs
     (append (list bash-minimal coreutils findutils git inetutils openssh perl)
             extra-inputs))
    (home-page "https://gitolite.com")
    (synopsis "Git access control layer")
    (description
     "Gitolite is an access control layer on top of Git, providing fine access
control to Git repositories.")
    (license license:gpl2)))

(define-public gitolite (make-gitolite))

(define-public gitile
  (package
    (name "gitile")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.lepiller.eu/git/gitile")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wb1rajcrzdqjncv40s7hjsnvlh1gq4z9pn9gf210g1iy35vimmz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:make-flags (list "GUILE_AUTO_COMPILE=0")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             ;; The 'bootstrap' script lacks a shebang, leading to "Exec
             ;; format error" with glibc 2.35.
             (invoke "autoreconf" "-vfi")))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (use-modules (guix build guile-build-system))
             ;; Wrap the 'gitile' command to refer to the right modules.
             (let* ((out    (assoc-ref outputs "out"))
                    (commonmark (assoc-ref inputs "guile-commonmark"))
                    (git    (assoc-ref inputs "guile-git"))
                    (bytes  (assoc-ref inputs "guile-bytestructures"))
                    (fibers (assoc-ref inputs "guile-fibers"))
                    (gcrypt (assoc-ref inputs "guile-gcrypt"))
                    (syntax-highlight (assoc-ref inputs "guile-syntax-highlight"))
                    (deps   (list out commonmark git bytes fibers gcrypt
                                  syntax-highlight))
                    (guile  (assoc-ref inputs "guile"))
                    (effective (target-guile-effective-version))
                    (mods   (string-drop-right  ;drop trailing colon
                             (string-join deps
                                          (string-append "/share/guile/site/"
                                                         effective ":")
                                          'suffix)
                             1))
                    (objs   (string-drop-right
                             (string-join deps
                                          (string-append "/lib/guile/" effective
                                                         "/site-ccache:")
                                          'suffix)
                             1)))
               (wrap-program (string-append out "/bin/gitile")
                 `("GUILE_LOAD_PATH" ":" prefix (,mods))
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,objs)))))))))
    (native-inputs
     (list autoconf automake guile-3.0 pkg-config))
    (inputs
     (list bash-minimal                 ;for wrap-program
           guile-3.0
           guile-commonmark
           guile-fibers
           guile-gcrypt
           guile-git
           guile-syntax-highlight
           guile-gnutls))
    (home-page "https://git.lepiller.eu/gitile")
    (synopsis "Simple Git forge written in Guile")
    (description "Gitile is a Git forge written in Guile that lets you
visualize your public Git repositories on a web interface.")
    (license license:agpl3+)))

(define-public pre-commit
  (package
    (name "pre-commit") ;formerly known as python-pre-commit
    (version "3.7.1")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/pre-commit/pre-commit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m2cs21xq2j1x80s7bh47fm2nsbnfxgscxfijaqwdsi2rrf4vlzv"))
       (modules '((guix build utils)))
       (snippet '(substitute* "setup.cfg"
                   (("virtualenv>=20.10.0") ;our virtualenv (20.3.1) is fine
                    "virtualenv>=20.0.8")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Skip language-specific tests because they depennd on language tools.
      #:test-flags
      #~(list "--ignore" "tests/languages"
              ;; These fail with AssertionError.
              "-k" (string-append
                    "not test_additional_dependencies_roll_forward"
                    " and not test_control_c_control_c_on_install"
                    " and not test_invalidated_virtualenv"
                    " and not test_local_python_repo"
                    " and not test_install_existing_hooks_no_overwrite"
                    " and not test_uninstall_restores_legacy_hooks"
                    " and not test_installed_from_venv"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'prepare-check-env
            (lambda _
              ;; Change from /homeless-shelter to /tmp for write permission.
              (setenv "HOME" "/tmp")
              ;; Environment variables used in the tests.
              (setenv "GIT_AUTHOR_NAME" "Your Name")
              (setenv "GIT_COMMITTER_NAME" "Your Name")
              (setenv "GIT_AUTHOR_EMAIL" "you@example.com")
              (setenv "GIT_COMMITTER_EMAIL" "you@example.com")
              ;; Some tests still fail with PermissionError.  Make the source
              ;; tree writable.
              ;; (for-each make-file-writable (find-files "."))
              ;; Some tests will need a working git repository.
              (invoke "git" "init")
              (invoke "git" "config" "--global" "user.name" "Your Name")
              (invoke "git" "config" "--global" "user.email" "you@example.com"))))))
    (native-inputs
     (list git-minimal/pinned
           python-covdefaults
           python-coverage
           python-distlib
           python-pytest
           python-pytest-env
           python-re-assert
           python-setuptools
           python-wheel
           which))
    ;; Propagate because pre-commit is also used as a module.
    (propagated-inputs
     (list python-cfgv
           python-identify
           python-nodeenv
           python-pyyaml
           python-virtualenv))
    (home-page "https://pre-commit.com/")
    (synopsis "Framework for managing and maintaining pre-commit hooks")
    (description
     "Pre-commit is a multi-language package manager for pre-commit hooks.  You
specify a list of hooks you want and pre-commit manages the installation and
execution of any hook written in any language before every commit.")
    (license license:expat)))

(define-public python-pre-commit
  (deprecated-package "python-pre-commit" pre-commit))

(define-public mercurial
  (package
    (name "mercurial")
    (version "6.7.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.mercurial-scm.org/"
                                 "release/mercurial-" version ".tar.gz"))
             (patches (search-patches "mercurial-hg-extension-path.patch"))
             (sha256
              (base32
               "01nqvp3cvidlz9z5vm05vpq81r6x10jwwfcaz0gw9anz0l60f8hw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* (find-files "tests" "\\.(t|py)$")
               (("/bin/sh")
                (which "sh"))
               (("/usr/bin/env")
                (which "env")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (with-directory-excursion "tests"
               ;; The following tests are known to fail.
               (for-each delete-file
                         '(;; XXX: This test calls 'run-tests.py --with-hg=
                           ;; `which hg`' and fails because there is no hg on
                           ;; PATH from before (that's why we are building it!)?
                           "test-hghave.t"

                           ;; This test creates a shebang spanning multiple
                           ;; lines which is difficult to substitute.  It
                           ;; only tests the test runner itself, which gets
                           ;; thoroughly tested during the check phase anyway.
                           "test-run-tests.t"

                           ;; These tests fail because the program is not
                           ;; connected to a TTY in the build container.
                           "test-nointerrupt.t"
                           "test-transaction-rollback-on-sigpipe.t"

                           ;; FIXME: This gets killed but does not receive an interrupt.
                           "test-commandserver.t"

                           ;; These tests get unexpected warnings about using
                           ;; deprecated functionality in Python, but otherwise
                           ;; succeed; try enabling for later Mercurial versions.
                           "test-demandimport.py"
                           "test-patchbomb-tls.t"
                           ;; Similarly, this gets a more informative error
                           ;; message from Python 3.10 than it expects.
                           "test-http-bad-server.t"

                           ;; Only works when run in a hg-repo, not in an
                           ;; extracted tarball
                           "test-doctest.py"

                           ;; TODO: the fqaddr() call fails in the build
                           ;; container, causing these server tests to fail.
                           "test-hgwebdir.t"
                           "test-http-branchmap.t"
                           "test-pull-bundle.t"
                           "test-push-http.t"
                           "test-serve.t"
                           "test-subrepo-deep-nested-change.t"
                           "test-subrepo-recursion.t"))
               (when tests?
                 (invoke "./run-tests.py"
                         ;; ‘make check’ does not respect ‘-j’.
                         (string-append "-j" (number->string
                                              (parallel-job-count)))
                         ;; The default time-outs are too low for many systems.
                         ;; Raise them generously: Guix enforces its own.
                         "--timeout" "86400"
                         "--slowtimeout" "86400"
                         ;; The test suite takes a long time and produces little
                         ;; output by default.  Prevent timeouts due to silence.
                         "-v"))))))))
    (native-inputs
     (list python-docutils
           ;; The following inputs are only needed to run the tests.
           python-nose unzip which))
    (inputs
     (list python-wrapper))
    ;; Find third-party extensions.
    (native-search-paths
     (list (search-path-specification
            (variable "HGEXTENSIONPATH")
            (files '("lib/python3.11/site-packages/hgext3rd")))))
    (home-page "https://www.mercurial-scm.org/")
    (synopsis "Decentralized version control system")
    (description
     "Mercurial is a free, distributed source control management tool.  It
efficiently handles projects of any size and offers an easy and intuitive
interface.")
    (license license:gpl2+)))

(define-public python-hg-evolve
  (package
    (name "python-hg-evolve")
    (version "11.1.3")
    (source
      (origin
        (method hg-fetch)
        (uri (hg-reference
               (url "https://www.mercurial-scm.org/repo/evolve")
               (changeset version)))
        (file-name (hg-file-name name version))
        (sha256
          (base32
            "09rq3hgbb6qjws0kymnh8lbglsc5yjby3b2bc0savs2agf88j83z"))))
    (build-system python-build-system)
    (arguments
     ;; Tests need mercurial source code.
     '(#:tests? #f))
    (propagated-inputs
      (list mercurial))
    (home-page "https://www.mercurial-scm.org/doc/evolution/")
    (synopsis "Flexible evolution of Mercurial history")
    (description "Evolve is a Mercurial extension for faster and safer mutable
history.  It implements the changeset evolution concept for Mercurial.")
    (license license:gpl2)))

(define-public hg-commitsigs
  ;; Latest tag is 11 years old.
  (let ((changeset "b53eb6862bff")
        (revision "0"))
    (package
      (name "hg-commitsigs")
      (version (git-version "0.1.0" revision changeset))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://foss.heptapod.net/mercurial/commitsigs")
                      (changeset changeset)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "059gm66q06m6ayl4brsc517zkw3ahmz249b6xm1m32ac5y24wb9x"))))
      (build-system copy-build-system)
      (arguments
       `(#:imported-modules ((guix build python-build-system)
                             ,@%copy-build-system-modules)
         #:modules ((srfi srfi-1)
                    (guix build python-build-system)
                    ;; Don't use `%copy-build-system-modules' because
                    ;; `standard-phases' from (guix build gnu-build-system)
                    ;; shadows the one from (guix build copy-build-system),
                    ;; which is the one we actually want.
                    (guix build copy-build-system)
                    ((guix build gnu-build-system) #:prefix gnu)
                    (guix build utils)
                    (guix build gremlin)
                    (ice-9 ftw)
                    (guix elf))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gpg (search-input-file inputs "/bin/gpg"))
                   (openssl (search-input-file inputs "/bin/openssl")))
               (substitute* "commitsigs.py"
                 (("b'gpg',") (string-append "b'" gpg "',"))
                 (("b'openssl',") (string-append "b'" openssl "',")))))))
         #:install-plan
         `(("commitsigs.py" ,(string-append "lib/python"
                                            (python-version
                                             (assoc-ref %build-inputs "python"))
                                            "/site-packages/hgext3rd/commitsigs.py")))))
      (native-inputs
       (list python))
      (inputs
       (list gnupg openssl))
      (home-page "https://foss.heptapod.net/mercurial/commitsigs")
      (synopsis "Automatic signing of changeset hashes")
      (description "This package provides a Mercurial extension for signing
the changeset hash of commits.  The signure is embedded directly in the
changeset itself; there won't be any extra commits.  Either GnuPG or OpenSSL
can be used for signing.")
      (license license:gpl2))))                   ;per commitsigs.py

(define-public heatwave
  (package
    (name "heatwave")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "heatwave" version))
       (sha256
        (base32 "1zzwmb9hvbyswzjgap02rrq8p44hb6xlzk1wd8w01mh2vva0xlx7"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-click
           python-gitpython
           python-monthdelta))
    (home-page "https://github.com/james-stoup/heatwave")
    (synopsis "Heat map visualization of a git repository")
    (description
     "This package provides a way of visualizing a heat map of a git repo.")
    (license license:gpl3)))

(define-public neon
  (package
    (name "neon")
    (version "0.32.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://notroj.github.io/neon/neon-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "11mj5zpp317dmds874wfwcpgij9i3scaahdi1xfzr5b2ii36crcq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list libxml2 openssl zlib))
    (arguments
     `(;; FIXME: Add tests once reverse address lookup is fixed in glibc, see
       ;; https://sourceware.org/bugzilla/show_bug.cgi?id=16475
       #:tests? #f
       #:configure-flags '("--enable-shared"
                           "--disable-static"
                           ;; requires libgnutils-config, deprecated
                           ;; in gnutls 2.8.
                           ; "--with-ssl=gnutls")))
                           "--with-ssl=openssl")))
    (home-page "https://notroj.github.io/neon/")
    (synopsis "HTTP and WebDAV client library")
    (description
     "Neon is an HTTP and WebDAV client library, with a C interface and the
following features:
@enumerate
@item High-level wrappers for common HTTP and WebDAV operations (GET, MOVE,
  DELETE, etc.);
@item low-level interface to the HTTP request/response engine, allowing the use
  of arbitrary HTTP methods, headers, etc.;
@item authentication support including Basic and Digest support, along with
  GSSAPI-based Negotiate on Unix, and SSPI-based Negotiate/NTLM on Win32;
@item SSL/TLS support using OpenSSL or GnuTLS, exposing an abstraction layer for
  verifying server certificates, handling client certificates, and examining
  certificate properties, smartcard-based client certificates are also
  supported via a PKCS#11 wrapper interface;
@item abstract interface to parsing XML using libxml2 or expat, and wrappers for
  simplifying handling XML HTTP response bodies;
@item WebDAV metadata support, wrappers for PROPFIND and PROPPATCH to simplify
  property manipulation.
@end enumerate\n")
    (license license:gpl2+))) ; for documentation and tests; source under lgpl2.0+

(define-public subversion
  (package
    (name "subversion")
    (version "1.14.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/subversion/"
                                  "subversion-" version ".tar.bz2"))
              (sha256
               (base32
                "0h54l4p2dlk1rm4zm428hi6ij6xpqxqlqmvkhmz5yhq9392zv7ll"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Running the tests in parallel causes test failures on i686-linux.
      ;; The issue was reported to users@subversion.apache.org, as suggested
      ;; at https://subversion.apache.org/reporting-issues.
      #:parallel-tests? #f
      #:configure-flags #~(list "--enable-static=no")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'patch-libtool-wrapper-ls
            (lambda* (#:key inputs #:allow-other-keys)
              ;; This substitution allows tests svnauthz_tests and svnlook_tests
              ;; to pass.  These tests execute svnauthz and svnlook through
              ;; their libtool wrapper scripts from svn hooks, whose empty
              ;; environments cause "ls: command not found" errors.  It would be
              ;; nice if this fix ultimately made its way into libtool.
              (substitute* "libtool"
                (("\\\\`ls")
                 (string-append "\\`" (search-input-file inputs "bin/ls"))))))
          (add-before 'build 'patch-test-sh
            (lambda _
              (substitute* "subversion/tests/libsvn_repos/repos-test.c"
                (("#!/bin/sh") (string-append "#!" (which "sh"))))))
          (add-before 'check 'set-PARALLEL
            (lambda* (#:key parallel-tests? #:allow-other-keys)
              (if parallel-tests?
                  (setenv "PARALLEL" (number->string (parallel-job-count)))
                  (simple-format #t "parallel-tests? are disabled\n"))))
          (add-after 'install 'install-perl-bindings
            (lambda _
              ;; Follow the instructions from 'subversion/bindings/swig/INSTALL'.
              (invoke "make" "swig-pl-lib")
              ;; FIXME: Test failures.
              ;; (invoke "make" "check-swig-pl")
              (invoke "make" "install-swig-pl-lib")

              ;; Set the right installation prefix.
              (with-directory-excursion "subversion/bindings/swig/perl/native"
                (invoke "perl" "Makefile.PL" "NO_PERLLOCAL=1"
                        (string-append "PREFIX=" #$output))
                (invoke "make" "install"
                        (string-append "OTHERLDFLAGS=-Wl,-rpath="
                                       #$output "/lib"))))))))
    (native-inputs
     (list pkg-config
           ;; For the Perl bindings.
           swig))
    (inputs
     (list apr
           apr-util
           lz4
           perl
           python-wrapper
           serf
           sqlite
           utf8proc
           zlib))
    (home-page "https://subversion.apache.org/")
    (synopsis "Revision control system")
    (description
     "@dfn{Subversion} (svn) exists to be recognized and adopted as a
centralized version control system characterized by its
reliability as a safe haven for valuable data; the simplicity of its model and
usage; and its ability to support the needs of a wide variety of users and
projects, from individuals to large-scale enterprise operations.")
    (license license:asl2.0)))

(define-public rcs
  (package
    (name "rcs")
    (version "5.10.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/rcs/rcs-"
                                 version ".tar.lz"))
             (sha256
              (base32
               "1iac4d1dhsfy5zb0n3p605pihdq702v06r4g8vi8b2saf88gxpa3"))))
    (build-system gnu-build-system)
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (add-after 'install 'install-rcsfreeze
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (bin (string-append out "/bin"))
                              (man1 (string-append out "/share/man/man1")))
                         (chmod "src/rcsfreeze" #o755)
                         (install-file "src/rcsfreeze" bin)
                         (install-file "man/rcsfreeze.1" man1)))))))
    (native-inputs (list ed lzip))
    (home-page "https://www.gnu.org/software/rcs/")
    (synopsis "Per-file local revision control system")
    (description
     "RCS is the original Revision Control System.  It works on a
file-by-file basis, in contrast to subsequent version control systems such as
CVS, Subversion, and Git.  This can make it suitable for system
administration files, for example, which are often inherently local to one
machine.")
    (license license:gpl3+)))

(define-public rcs-blame
  (package
    (name "rcs-blame")
    (version "1.3.1-20210207")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://invisible-mirror.net/archives/rcs-blame/blame-"
                   version ".tgz"))
             (sha256
              (base32
               "1j0brsvdx3hlbwchddafh8r2xmxv5vg4ahpd68v4bb9xhcq6pcih"))))
    (build-system gnu-build-system)
    (home-page "https://invisible-island.net/rcs-blame/rcs-blame.html")
    (synopsis "Display the last modification for each line in an RCS file")
    (description
     "@code{blame} outputs an annotated revision from each RCS file.  An
annotated RCS file describes the revision and date in which each line was
added to the file, and the author of each line.")
    (license license:gpl2+)))

(define-public rcshist
  (package
    (name "rcshist")
    (version "1.04-20190106")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://invisible-mirror.net/archives/rcshist/rcshist-"
                   version ".tgz"))
             (sha256
              (base32
               "01ab3xwgm934lxr8bm758am3vxwx4hxx7cc9prbgqj5nh30vdg1n"))))
    (build-system gnu-build-system)
    (home-page "https://invisible-island.net/rcshist/rcshist.html")
    (synopsis "Display RCS change history")
    (description
     "The @code{rcshist} utility displays the complete revision history of a
set of RCS files including log messages and patches.  It can also display the
patch associated with a particular revision of an RCS file.")
    (license (list license:bsd-2
                   license:bsd-3))))  ; bsd_queue.h

(define-public cvs
  (package
    (name "cvs")
    (version "1.12.13")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.gnu.org/non-gnu/cvs/source/feature/"
                   version "/cvs-" version ".tar.bz2"))
             (patches (search-patches "cvs-CVE-2017-12836.patch"))
             (sha256
              (base32
               "0pjir8cwn0087mxszzbsi1gyfc6373vif96cw4q3m1x6p49kd1bq"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The test suite looks flawed, and the package is obsolete anyway.
     '(#:tests? #f
       #:configure-flags (list "--with-external-zlib")))
    (inputs (list zlib nano))                    ; the default editor
    (home-page "https://cvs.nongnu.org")
    (synopsis "Historical centralized version control system")
    (description
     "CVS is a version control system, an important component of Source
Configuration Management (SCM).  Using it, you can record the history of
sources files, and documents.  It fills a similar role to the free software
RCS, PRCS, and Aegis packages.")
    (license license:gpl1+)))

(define-public cvs-fast-export
  (package
    (name "cvs-fast-export")
    (version "1.56")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.catb.org/~esr/cvs-fast-export/"
                                  "cvs-fast-export-" version ".tar.gz"))
              (sha256
               (base32
                "058bzp3npng48ascls943m16kgvrl0h197a10brf7mvx8zpfc7sc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure))       ; no configure script
       #:parallel-build? #f         ; parallel a2x commands fail spectacularly
       #:make-flags
       (list "CC=gcc" (string-append "prefix?=" (assoc-ref %outputs "out")))))
    (inputs
     `(("git" ,git)
       ("python" ,python-wrapper)))
    (native-inputs
     (list asciidoc
           ;; These are needed for the tests.
           cvs rcs))
    (home-page "http://www.catb.org/esr/cvs-fast-export/")
    (synopsis "Export an RCS or CVS history as a fast-import stream")
    (description "This program analyzes a collection of RCS files in a CVS
repository (or outside of one) and, when possible, emits an equivalent history
in the form of a fast-import stream.  Not all possible histories can be
rendered this way; the program tries to emit useful warnings when it can't.

The program can also produce a visualization of the resulting commit directed
acyclic graph (DAG) in the input format of @uref{http://www.graphviz.org,
Graphviz}.  The package also includes @command{cvssync}, a tool for mirroring
masters from remote CVS hosts.")
    (license license:gpl2+)))

(define-public vc-dwim
  (package
    (name "vc-dwim")
    (version "1.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/vc-dwim/vc-dwim-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0am6axxdvkm2vwgg0gjrd930yv4dlsdbf0rdv0zh5bhy1ir64rph"))))
    (build-system gnu-build-system)
    (inputs (list perl))
    (native-inputs
     (list emacs-minimal ; for `ctags'
           inetutils))   ; for `hostname', used in the tests
    (home-page "https://www.gnu.org/software/vc-dwim/")
    (synopsis "Version-control-agnostic ChangeLog diff and commit tool")
    (description
     "The vc-dwim package contains two tools, \"vc-dwim\" and \"vc-chlog\".
vc-dwim is a tool that simplifies the task of maintaining a ChangeLog and
using version control at the same time, for example by printing a reminder
when a file change has been described in the ChangeLog but the file has not
been added to the VC.  vc-chlog scans changed files and generates
standards-compliant ChangeLog entries based on the changes that it detects.")
    (license license:gpl3+)))

(define-public diffstat
  (package
    (name "diffstat")
    (version "1.65")
    (source (origin
              (method url-fetch)
              (uri
               (list
                 (string-append "ftp://invisible-island.net/diffstat/"
                                "diffstat-" version ".tgz")
                 (string-append "http://invisible-mirror.net/archives/diffstat/"
                                "diffstat-" version ".tgz")))
              (sha256
               (base32
                "12m2aysq6syw83bn4gqhpm284a2ran8w6m8pja2wvsvdj8j79wlc"))))
    (build-system gnu-build-system)
    (home-page "https://invisible-island.net/diffstat/")
    (synopsis "Make histograms from the output of @command{diff}")
    (description
     "Diffstat reads the output of @command{diff} and displays a histogram of
the insertions, deletions, and modifications per file.  It is useful for
reviewing large, complex patch files.")
    (license (license:x11-style "file://COPYING"))))

(define-public cssc
  (package
    (name "cssc")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/" name "/CSSC-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vsisqq573xjr2qpn19iwmpqgl3mq03m790akpa4rvj60b4d1gni"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'precheck
           (lambda _
             (begin
               (substitute* "tests/common/test-common"
                 (("/bin/pwd") (which "pwd")))

               (substitute* "tests/prt/all-512.sh"
                 (("/bin/sh") (which "sh")))

               (for-each
                (lambda (file)
                  (substitute* file (("egrep") "grep -E")))
                '("tests/common/test-common"
                  "tests/admin/comment.sh"
                  "tests/cdc/2comment.sh"
                  "tests/cdc/4order.sh"
                  "tests/get/subst.sh"))

               ;; XXX: This test has no hope of passing until there is a "nogroup"
               ;; entry (or at least some group to which the guix builder does
               ;; not belong) in the /etc/group file of the build environment.
               ;; Currently we do not have such a group.  Disable this test for now.
               (substitute* "tests/Makefile"
                 (("test-delta ") ""))))))))
    ;; These are needed for the tests
    (native-inputs (list git cvs))
    (home-page "https://www.gnu.org/software/cssc/")
    (synopsis "File-based version control like SCCS")
    (description  "GNU CSSC provides a replacement for the legacy Unix source
code control system SCCS.  This allows old code still under that system to be
accessed and migrated on modern systems.")
    (license license:gpl3+)))

;; This package can unfortunately work only in -TEST mode, since Aegis
;; requires that it is installed setuid root.
(define-public aegis
  (package
    (name "aegis")
    (version "4.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/aegis/aegis/" version
                                  "/aegis-" version ".tar.gz"))
              (sha256
               (base32
                "18s86ssarfmc4l17gbpzybca29m5wa37cbaimdji8czlcry3mcjl"))
            (patches (search-patches "aegis-perl-tempdir1.patch"
                                     "aegis-perl-tempdir2.patch"
                                     "aegis-test-fixup-1.patch"
                                     "aegis-test-fixup-2.patch"
                                     "aegis-constness-error.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("e2fsprogs" ,e2fsprogs)
       ("curl" ,curl)
       ("file" ,file)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)
       ("gettext" ,gettext-minimal)))
    (native-inputs
     (list bison
           groff
           perl
           ;; Various tests require the following:
           cvs
           flex
           cook
           subversion
           rcs
           ed))
    (arguments
     `(#:configure-flags (list "--with-no-aegis-configured"
                               "--sharedstatedir=/var/com/aegis"
                               ;; Uses the old 'throw()' specifier with 'new'
                               ;; which changed in C++11.
                               "CXXFLAGS=-std=c++03")
       #:parallel-build? #f ; There are some nasty racy rules in the Makefile.
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-conf
           (lambda _
              (substitute* (append '("configure"
                                     "etc/check-tar-gz.sh"
                                     "etc/patches.sh"
                                     "etc/test.sh"
                                     "script/aexver.in"
                                     "script/aebisect.in"
                                     "script/aeintegratq.in"
                                     "script/tkaegis.in"
                                     "script/test_funcs.in"
                                     "web/eg_oss_templ.sh"
                                     "web/webiface.html"
                                     "libaegis/getpw_cache.cc")
                                   (find-files "test" "\\.sh"))
                           (("/bin/sh") (which "sh")))
              (setenv "SH" (which "sh"))
              #t))
         (replace 'check
           (lambda _
             (let ((home (string-append (getcwd) "/my-new-home")))
               ;; Some tests need to write to $HOME.
               (mkdir home)
               (setenv "HOME" home)

               ;; This test assumes that flex has been symlinked to "lex".
               (substitute* "test/00/t0011a.sh"
                 (("type lex")  "type flex"))

               ;; XXX Disable tests that fail, for unknown reasons, ‘for now’.
               (for-each
                (lambda (test) (substitute* "Makefile"
                                 (((string-append "test/" test "\\.ES ")) "")))
                (list "00/t0011a"
                      "00/t0049a"
                      "01/t0196a"))

               ;; The author decided to call the check rule "sure".
               (invoke "make" "sure")))))))
    (home-page "https://sourceforge.net/projects/aegis/")
    (synopsis "Project change supervisor")
    (description "Aegis is a project change supervisor, and performs some of
the Software Configuration Management needed in a CASE environment.  Aegis
provides a framework within which a team of developers may work on many
changes to a program independently, and Aegis coordinates integrating these
changes back into the master source of the program, with as little disruption
as possible.  Resolution of contention for source files, a major headache for
any project with more than one developer, is one of Aegis's major functions.")
    (license license:gpl3+)))

(define-public tig
  (package
    (name "tig")
    (version "2.5.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonas/tig")
             (commit (string-append "tig-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ncrxvn7vbcyfvcczra6jx4mr5hv6p5xfa1wdvdfzwgfkj16hhys"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-doc
            (lambda _
              (invoke "make" "install-doc")))
          (add-after 'install 'install-completions
            (lambda _
              (let ((share (string-append #$output "/share")))
                (mkdir-p (string-append #$output "/etc/bash_completion.d"))
                (mkdir-p (string-append share "/zsh/site-functions"))
                (copy-file "contrib/tig-completion.bash"
                           (string-append #$output "/etc/bash_completion.d/tig"))
                (copy-file "contrib/tig-completion.zsh"
                           (string-append share "/zsh/site-functions/_tig"))))))
      #:test-target "test"
      #:tests? #f))                    ; tests require access to /dev/tty
    (native-inputs
     (list asciidoc autoconf automake docbook-xsl libxml2 pkg-config xmlto))
    (inputs
     (list ncurses readline))
    (home-page "https://jonas.github.io/tig/")
    (synopsis "Ncurses-based text user interface for Git")
    (description
     "Tig is an ncurses text user interface for Git, primarily intended as
a history browser.  It can also stage hunks for commit, or colorize the
output of the @code{git} command.")
    (license license:gpl2+)))

(define-public findnewest
  (package
    (name "findnewest")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/0-wiz-0/findnewest")
             (commit (string-append "findnewest-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x1cbn2b27h5r0ah5xc06fkalfdci2ngrgd4wibxjw0h88h0nvgq"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake))
    (home-page "https://github.com/0-wiz-0/findnewest/releases")
    (synopsis "Print the modification time of the latest file")
    (description
     "Recursively find the newest file in a file tree and print its
modification time.")
    (license license:bsd-2)))

(define-public fnc
  (package
    (name "fnc")
    (version "0.18")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://fnc.bsdbox.org/uv/dl/fnc-"
                              version ".tar.gz"))
              (sha256
               (base32
                "1067rr4nqngld1nqa8c7imp9n3w5fp7rpc7khh6l84q2w1klrya9"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   ;; fix cross-compiling.
                   (add-after 'unpack 'don-t-use-install-s
                     (lambda _
                       (substitute* "fnc.bld.mk"
                         (("install -s")
                          "install")))))
      #:tests? #f                       ; no tests
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))))
    (inputs (list ncurses zlib sqlite-next))
    (home-page "https://fnc.bsdbox.org")
    (synopsis "Interactive text-based user interface for Fossil")
    (description "fnc uses ncurses and libfossil to create a fossil user
interface in the terminal.  It can view local changes at the hunk level to
prepare atomic commits.")
    (license license:isc)))

(define-public myrepos
  (package
    (name "myrepos")
    (version "1.20180726")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://myrepos.branchable.com/myrepos")
             (commit version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "0jphw61plm8cgklja6hs639xhdvxgvjwbr6jpvjwpp7hc5gmhms5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'wrap-webcheckout
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/webcheckout")
                 `("PERL5LIB" ":" prefix
                   ,(map (lambda (i) (string-append (assoc-ref inputs i)
                                                    "/lib/perl5/site_perl"))
                         '("perl-encode-locale" "perl-http-date"
                           "perl-http-message" "perl-html-parser" "perl-libwww"
                           "perl-uri" "perl-try-tiny"))))))))))
    (inputs
     (list bash-minimal                 ;for wrap-program
           perl
           perl-encode-locale
           perl-html-parser
           perl-http-date
           perl-http-message
           perl-libwww
           perl-try-tiny
           perl-uri))
    (home-page "https://myrepos.branchable.com/")
    (synopsis "Multiple repository management tool")
    (description
     "Myrepos provides the @code{mr} command, which maps an operation (e.g.,
fetching updates) over a collection of version control repositories.  It
supports a large number of version control systems: Git, Subversion,
Mercurial, Bazaar, Darcs, CVS, Fossil, and Veracity.")
    (license license:gpl2+)))

(define-public grokmirror
  (package
    (name "grokmirror")
    (version "2.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://git.kernel.org/pub/scm/"
                                 "utils/grokmirror/grokmirror.git"))
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "0c6nnfzzyl247r1dcjnsyx16d34nyra9ikjjhi0xzlrbiwnb0w32"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-manpages
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((man (string-append (assoc-ref outputs "out")
                                        "/man/man1/")))
               (mkdir-p man)
               (for-each (lambda (file) (install-file file man))
                         (find-files "." "\\.1$"))))))))
    (propagated-inputs
     (list python-packaging python-requests))
    (home-page
     "https://git.kernel.org/pub/scm/utils/grokmirror/grokmirror.git")
    (synopsis "Framework to smartly mirror git repositories")
    (description "Grokmirror enables replicating large git repository
collections efficiently.  Mirrors decide to clone and update repositories
based on a manifest file published by servers.")
    (license license:gpl3+)))

(define-public patatt
  (package
    (name "patatt")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "patatt" version))
       (sha256
        (base32 "0a0a5ndlnv7dk2smn8algss6q17gbd6mc7yacz17c9cxabv2c24q"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ; No tests.
    (propagated-inputs
     (list python-pynacl))
    (home-page "https://git.kernel.org/pub/scm/utils/patatt/patatt.git")
    (synopsis "Tool for cryptographic patch attestation")
    (description "This utility provides end-to-end cryptographic attestation
of patches sent via mail.  It does so by adapting the DKIM email signature
standard to include cryptographic signatures via the X-Developer-Signature
email header.")
    (license license:expat-0)))

(define-public b4
  (package
    (name "b4")
    (version "0.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/utils/b4/b4.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18pacf7brvkmvxwkrsjigq9ymrr289a82wg4f1f1n4xr3k7vyr9i"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? (not (%current-target-system)) ;git path hardcoded.
           #:phases
           #~(modify-phases %standard-phases
               ;; XXX: dnspython attempts to read /etc/resolv.conf when loading
               ;; resolver.py, which breaks the sanity check in dependent
               ;; packages.  This should rather be fixed in dnspython.
               (delete 'sanity-check)
               ;; This ensures git is present when called.
               (add-after 'unpack 'hardcode-git-bin
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "src/b4" "\\.py$")
                     (("\\['git'")
                      (string-append
                       "['" (search-input-file inputs "bin/git") "'"))))))))
    (inputs
     (list git-filter-repo
           git-minimal
           patatt
           python-dkimpy
           python-dnspython
           python-requests))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://git.kernel.org/pub/scm/utils/b4/b4.git")
    (synopsis "Tool for working with patches in public-inbox archives")
    (description
     "The @code{b4} command is designed to make it easier to participate in
patch-based workflows for projects that have public-inbox archives.

Features include:
@itemize
@item downloading a thread's mbox given a message ID
@item processing an mbox so that is ready to be fed to @code{git-am}
@item creating templated replies for processed patches and pull requests
@item submitting cryptographic attestation for patches.
@end itemize")
    (license license:gpl2+)))

(define-public git-annex-remote-rclone
  (package
    (name "git-annex-remote-rclone")
    (version "0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DanielDent/git-annex-remote-rclone")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03m95620fp891ki6rsqw5nkydwx84nag5nhyvzfi3q64fpnpmb07"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((bash (search-input-file %build-inputs "/bin/bash"))
               (rclone (search-input-file %build-inputs "/bin/rclone")))
           (copy-file (string-append (assoc-ref %build-inputs "source")
                                     "/git-annex-remote-rclone")
                      "git-annex-remote-rclone")
           (substitute* "git-annex-remote-rclone"
             (("/bin/bash") bash)
             (("runcmd rclone") (string-append "runcmd " rclone)))
           (install-file "git-annex-remote-rclone"
                         (string-append %output "/bin"))
           #t))))
    (inputs
     (list bash rclone))
    (home-page "https://github.com/DanielDent/git-annex-remote-rclone")
    (synopsis "Use rclone-supported cloud storage providers with git-annex")
    (description "This wrapper around rclone makes any destination supported
by rclone usable with git-annex.")
    (license license:gpl3+)))

(define-public fossil
  (package
    (name "fossil")
    (version "2.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://fossil-scm.org/home/tarball/version-" version
	     "/fossil-" version ".tar.gz"))
       (sha256
        (base32 "18gws90by2q6a6rk7h3mx46pn79lz4zi3saxlyrdz5982mw9rvp4"))))
    (build-system gnu-build-system)
    (native-inputs
     (list tcl                          ;for configuration only
           which                        ;for tests only
           ed))                         ;ditto
    (inputs
     ;; Need sqlite >= 3.43.0.
     (list openssl zlib sqlite-next))
    (arguments
     `(#:configure-flags (list "--with-openssl=auto"
                               "--enable-json"
                               "--disable-internal-sqlite")
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-source-shebangs 'patch-sh
                    (lambda _
                      (substitute* '("auto.def")
                        (("/bin/sh") (which "sh")))))
                  (replace 'configure
                    (lambda* (#:key outputs (configure-flags '())
                              #:allow-other-keys)
                      ;; The 'configure' script is not an autoconf script and
                      ;; chokes on unrecognized options.
                      (apply invoke
                             "./configure"
                             (string-append "--prefix="
                                            (assoc-ref outputs "out"))
                             configure-flags)))
                  (add-before 'check 'test-setup
                    (lambda _
                      (setenv "USER" "guix")
                      (setenv "TZ" "UTC"))))))
    (home-page "https://fossil-scm.org")
    (synopsis "Software configuration management system")
    (description
     "Fossil is a distributed source control management system which supports
access and administration over HTTP CGI or via a built-in HTTP server.  It has
a built-in wiki, built-in file browsing, built-in tickets system, etc.")
    (properties
     '((release-monitoring-url
        . "https://fossil-scm.org/home/uv/latest-release.md")))
    (license (list license:public-domain        ;src/miniz.c, src/shell.c
                   license:bsd-2))))

(define-public pijul
  (package
    (name "pijul")
    (version "1.0.0-beta.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pijul" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lk261rrk4xy60d4akfn8mrrqxls28kf9mzrjcrxdzbdysml66n5"))
        (snippet
         #~(begin (use-modules (guix build utils))
                  (substitute* "Cargo.toml"
                    (("\"= ?([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                     (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:install-source? #f
       #:cargo-inputs
       (list rust-anyhow-1
             rust-async-trait-0.1
             rust-atty-0.2
             rust-byteorder-1
             rust-bytes-1
             rust-canonical-path-2
             rust-chrono-0.4
             rust-clap-4
             rust-clap-complete-4
             rust-ctrlc-3
             rust-data-encoding-2
             rust-dateparser-0.1
             rust-dirs-next-2
             rust-edit-0.1
             rust-env-logger-0.8
             rust-futures-0.3
             rust-futures-util-0.3
             rust-git2-0.13
             rust-human-panic-1
             rust-hyper-0.14
             rust-ignore-0.4
             rust-keyring-2
             rust-lazy-static-1
             rust-libpijul-1
             rust-log-0.4
             rust-open-3
             rust-pager-0.16
             rust-path-slash-0.1
             rust-pijul-config-0.0.1
             rust-pijul-identity-0.0.1
             rust-pijul-interaction-0.0.1
             rust-pijul-remote-1
             rust-pijul-repository-0.0.1
             rust-ptree-0.4
             rust-rand-0.8
             rust-regex-1
             rust-reqwest-0.11
             rust-sanakirja-1
             rust-serde-1
             rust-serde-derive-1
             rust-serde-json-1
             rust-tempfile-3
             rust-termcolor-1
             rust-thiserror-1
             rust-thrussh-0.33
             rust-thrussh-config-0.5
             rust-thrussh-keys-0.21
             rust-tokio-1
             rust-toml-0.5
             rust-url-2
             rust-validator-0.15
             rust-whoami-1)
       #:cargo-development-inputs
       (list rust-exitcode-1
             rust-expectrl-0.7)
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'install-extras
             (lambda* (#:key native-inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (share (string-append out "/share"))
                      (bash-completions-dir
                       (string-append out "/etc/bash_completion.d/"))
                      (zsh-completions-dir
                       (string-append share "/zsh/site-functions"))
                      (fish-completions-dir
                       (string-append share "/fish/vendor_completions.d"))
                      (elvish-completions-dir
                       (string-append share "/elvish/lib"))
                      (pijul (if #$(%current-target-system)
                             (search-input-file native-inputs "/bin/pijul")
                             (string-append out "/bin/pijul"))))
                 (mkdir-p bash-completions-dir)
                 (with-output-to-file
                   (string-append bash-completions-dir "/pijul")
                   (lambda _ (invoke pijul "completion" "bash")))
                 (mkdir-p zsh-completions-dir)
                 (with-output-to-file
                   (string-append zsh-completions-dir "/_pijul")
                   (lambda _ (invoke pijul "completion" "zsh")))
                 (mkdir-p fish-completions-dir)
                 (with-output-to-file
                   (string-append fish-completions-dir "/pijul.fish")
                   (lambda _ (invoke pijul "completion" "fish")))
                 (mkdir-p elvish-completions-dir)
                 (with-output-to-file
                   (string-append elvish-completions-dir "/pijul")
                   (lambda _ (invoke pijul "completion" "elvish")))))))))
    (native-inputs
     (append (if (%current-target-system)
                 (list this-package)
                 '())
             (list pkg-config)))
    (inputs (list libsodium openssl))
    (home-page "https://nest.pijul.com/pijul/pijul")
    (synopsis "Distributed version control system")
    (description "This package provides pijul, a sound and fast distributed
version control system based on a mathematical theory of asynchronous work.")
    (license license:gpl2+)))

(define-public stagit
  (package
    (name "stagit")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    ;; NOTE: It can only be cloned using the git protocol
                    (url "git://git.codemadness.org/stagit")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17yggk3fbm731z98warvix332487s0k6knhxnf9zc6f667qi2mlr"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; No tests
           #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)))) ; No configure script
    (inputs
     (list libgit2))
    (home-page "https://git.codemadness.org/stagit/")
    (synopsis "Static git page generator")
    (description "Stagit creates static pages for git repositories, the results can
be served with a HTTP file server of your choice.")
    (license license:expat)))

(define-public gource
  (package
    (name "gource")
    (version "0.55")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/acaudwell/Gource/releases/download"
                    "/gource-" version "/gource-" version ".tar.gz"))
              (sha256
               (base32
                "0hh17h0pf4b7yq23xsr5zhl1cs02d2bijxj7ks6m01wbs89948y8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost-libdir="
                            (assoc-ref %build-inputs "boost")
                            "/lib")
             "--with-tinyxml")
       #:disallowed-references (,tzdata-for-tests)
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'unbundle
                    (lambda _
                      (delete-file-recursively "src/tinyxml")))
                  (add-before 'check 'check-setup
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "TZDIR"   ; for src/test/datetime_tests.cpp
                              (search-input-directory inputs
                                                      "share/zoneinfo")))))))
    (native-inputs
     (list pkg-config tzdata-for-tests))
    (inputs
     (list boost
           ftgl
           glew
           glm
           glu
           libpng
           mesa
           pcre2
           (sdl-union (list sdl2 sdl2-image))
           tinyxml))
    (home-page "https://gource.io/")
    (synopsis "3D visualisation tool for source control repositories")
    (description "@code{gource} provides a software version control
visualization.  The repository is displayed as a tree where the root of the
repository is the centre, directories are branches and files are leaves.
Contributors to the source code appear and disappear as they contribute to
specific files and directories.")
    (license license:gpl3+)))

(define-public src
  (package
    (name "src")
    (version "1.32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/esr/src.git/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kxbmpjr98kfacjidizxcghl541fwnf8yzfvwfq5f9zbv42p8l41"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ; no 'configure' script
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((prog (string-append #$output "/bin/src"))
                     (rcs  (search-input-file inputs "bin/rcs")))
                (wrap-program prog
                  `("PATH" ":" prefix (,(dirname rcs)))))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" (getenv "TMPDIR"))
                (invoke "git" "config" "--global" "user.name" "guix")
                (invoke "git" "config" "--global" "user.email" "guix")
                (invoke "./srctest")))))))
    (native-inputs
     (list asciidoc
           ;; For testing.
           git
           perl))
    (inputs
     (list bash-minimal
           cssc
           python-wrapper
           rcs))
    (synopsis "Simple revision control")
    (home-page "http://www.catb.org/~esr/src/")
    (description
     "SRC (or src) is simple revision control, a version-control system for
single-file projects by solo developers and authors.  It modernizes the
venerable RCS, hence the anagrammatic acronym.  The design is tuned for use
cases like all those little scripts in your @file{~/bin} directory, or a
directory full of HOWTOs.")
    (license license:bsd-2)))

(define-public git-when-merged
  ;; Use an unreleased version to get a PY3 compatibility fix.
  (let ((commit "ab6af7865a0ba55ba364a6c507e0be6f84f31c6d"))
    (package
      (name "git-when-merged")
      (version (string-append "1.2.0-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mhagger/git-when-merged/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0iyk2psf97bc9h43m89p3xjmm79fsx99i7px29g4lcnmdy5kmz0p"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; there are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "bin/git-when-merged"
                             (string-append (assoc-ref outputs "out")
                                            "/bin"))))
           (add-before 'install 'patch-git
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((git (search-input-file inputs "/bin/git")))
                 (substitute* "bin/git-when-merged"
                   (("'git'") (string-append "'" git "'"))))))
           (add-after 'install 'wrap-script
             (lambda* (#:key outputs #:allow-other-keys)
               (wrap-program (string-append (assoc-ref outputs "out")
                                            "/bin/git-when-merged")
                 `("GUIX_PYTHONPATH" ":" prefix
                   (,(getenv "GUIX_PYTHONPATH")))))))))
      (inputs (list bash-minimal git python-wrapper))
      (home-page "https://github.com/mhagger/git-when-merged")
      (synopsis "Determine when a commit was merged into a Git branch")
      (description "This Git extension defines a subcommand,
@code{when-merged}, whose core operation is to find the merge that brought a
given commit into the specified ref(s).  It has various options that control
how information about the merge is displayed.")
      (license license:gpl2+))))

(define-public git-imerge
  (package
    (name "git-imerge")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mhagger/git-imerge")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vi1w3f0yk4gqhxj2hzqafqq28rihyhyfnp8x7xzib96j2si14a4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; only manual test scripts
       #:make-flags (list (string-append "DESTDIR=" %output)
                          "PREFIX=")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'patch-git
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((git (search-input-file inputs "/bin/git")))
               (substitute* "git-imerge"
                 (("'git'") (string-append "'" git "'"))))))
         (add-after 'install 'wrap-script
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/git-imerge")
               `("GUIX_PYTHONPATH" ":" prefix
                 (,(getenv "GUIX_PYTHONPATH")))))))))
    (inputs (list bash-minimal git python-wrapper))
    (home-page "https://github.com/mhagger/git-imerge")
    (synopsis "Incremental merge for Git")
    (description "This Git extension defines a subcommand, @code{imerge},
which performs an incremental merge between two branches.  Its two primary
design goals are to reduce the pain of resolving merge conflicts by finding
the smallest possible conflicts and to allow a merge to be saved, tested,
interrupted, published, and collaborated on while in progress.")
    (license license:gpl2+)))

(define-public go-github-com-git-lfs-pktline
  (let ((commit "06e9096e28253ba5c7825cbba43f469e4efd10f0")
        (revision "0"))
    (package
      (name "go-github-com-git-lfs-pktline")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/git-lfs/pktline")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02sn3v8vrl7qjnagbnrbrjnyjvzq8cwkxmc922zyc9b2hg187kpz"))))
      (build-system go-build-system)
      (arguments `(#:import-path "github.com/git-lfs/pktline"))
      (propagated-inputs (list go-github-com-stretchr-testify
                               go-github-com-pmezard-go-difflib
                               go-github-com-davecgh-go-spew))
      (home-page "https://github.com/git-lfs/pktline")
      (synopsis "Git pkt-line Go toolkit")
      (description "This package is a Go language toolkit for reading and
writing files using the Git pkt-line format used in various Git operations.")
(license license:expat))))

(define-public go-github-com-git-lfs-wildmatch-v2
  (package
    (name "go-github-com-git-lfs-wildmatch-v2")
    (version "2.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/git-lfs/wildmatch")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yg6d77d5l6v7cd8vr00y68z9aqb8qs4lidv0hkqh4fvz0ggvpln"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/git-lfs/wildmatch/v2"))
    (home-page "https://github.com/git-lfs/wildmatch")
    (synopsis "Go implementation of Git's wildmatch")
    (description
     "This package is an implementation of Git's wildmatch.c-style pattern
matching.")
    (license license:expat)))

(define-public go-github-com-git-lfs-gitobj-v2
  (package
    (name "go-github-com-git-lfs-gitobj-v2")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/git-lfs/gitobj")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sd7y4xbx00js1g2az4nq8g5lvsm4d7nqr3v4kxy8fxrfzdm63j9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/git-lfs/gitobj/v2"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-pmezard-go-difflib
                             go-github-com-davecgh-go-spew))
    (home-page "https://github.com/git-lfs/gitobj")
    (synopsis "Read and write git objects")
    (description
     "This package reads and writes loose and packed (objects found in git
packfiles) Git objects.  It uses the pack package to search pack index files
and locate the corresponding delta-base chain in the appropriate pack file.
If gitobj can't find a loose object with the appropriate SHA-1, it will search
the repository's packfile(s) instead.  If it finds an object in a packfile, it
will reconstruct the object along its delta-base chain and return it.")
    (license license:expat)))

(define-public git-lfs
  (package
    (name "git-lfs")
    (version "3.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/git-lfs/git-lfs")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09ry2nq5bpdxk446dyhc0d6d85wy5x2i5ckwwg9r00a3zdp5v4ry"))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/git-lfs/git-lfs"
      #:install-source? #f
      #:test-flags #~(list "-skip" "TestHistoryRewriterUpdatesRefs")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-/bin/sh
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/github.com/git-lfs/git-lfs/lfs/hook.go"
                (("/bin/sh")
                 (search-input-file inputs "bin/sh")))))
          ;; Only build the man pages if ruby-asciidoctor is available.
          #$@(if (this-package-native-input "ruby-asciidoctor")
               #~((add-before 'build 'man-gen
                    ;; Without this, the binary generated in 'build
                    ;; phase won't have any embedded usage-text.
                    (lambda _
                      (with-directory-excursion "src/github.com/git-lfs/git-lfs"
                        (invoke "make" "mangen"))))
                  (add-after 'build 'build-man-pages
                    (lambda _
                      (with-directory-excursion "src/github.com/git-lfs/git-lfs"
                        (invoke "make" "man"))))
                  (add-after 'install 'install-man-pages
                    (lambda* (#:key outputs #:allow-other-keys)
                      (with-directory-excursion "src/github.com/git-lfs/git-lfs/man"
                        (for-each
                         (lambda (manpage)
                           (install-file manpage
                                         (string-append #$output "/share/man/man1")))
                         (find-files "." "^git-lfs.*\\.1$"))))))
               #~()))))
    ;; make `ronn` available during build for man page generation
    (native-inputs
     (append (list git-minimal)
             (if (supported-package? ruby-asciidoctor)
               (list ronn-ng ruby-asciidoctor)
               '())))
    (propagated-inputs
     (list go-github-com-avast-retry-go
           go-github-com-dpotapov-go-spnego
           go-github-com-git-lfs-gitobj-v2
           go-github-com-git-lfs-go-netrc
           go-github-com-git-lfs-pktline
           go-github-com-git-lfs-wildmatch-v2
           go-github-com-jmhodges-clock
           go-github-com-leonelquinteros-gotext
           go-github-com-mattn-go-isatty
           go-github-com-olekukonko-ts
           go-github-com-pkg-errors
           go-github-com-rubyist-tracerx
           go-github-com-spf13-cobra
           go-github-com-ssgelm-cookiejarparser
           go-github-com-stretchr-testify
           go-github-com-xeipuuv-gojsonschema
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys))
    (home-page "https://git-lfs.github.com/")
    (synopsis "Git extension for versioning large files")
    (description
     "Git Large File Storage (LFS) replaces large files such as audio samples,
videos, datasets, and graphics with text pointers inside Git, while storing the
file contents on a remote server.")
    (license license:expat)))

(define-public lfs-s3
  (package
    (name "lfs-s3")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~ngraves/lfs-s3")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yilbxpia2lh36s872hiji77hazy83h2zc0iyqldrf3r18szqniw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~ngraves/lfs-s3"))
    (inputs (list git-lfs))
    (propagated-inputs
     (list go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
           go-github-com-aws-aws-sdk-go-v2-service-s3))
    (home-page "https://git.sr.ht/~ngraves/lfs-s3/")
    (synopsis "Git extension for versioning large files in S3")
    (description
     "This package provides a custom transfer agent for Git LFS, allowing
plain S3 bucket usage as remote storage for media files. This package uses a
standalone agent instead of a server.")
    (license license:expat)))

(define-public git-open
  (package
    (name "git-open")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/paulirish/git-open")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11n46bngvca5wbdbfcxzjhjbfdbad7sgf7h9gf956cb1q8swsdm0"))))
    (build-system copy-build-system)
    (inputs
     (list bash-minimal xdg-utils))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (xdg-utils (assoc-ref inputs "xdg-utils")))
               (wrap-program (string-append out "/bin/git-open")
                 `("PATH" ":" prefix (,(string-append xdg-utils "/bin"))))))))
       #:install-plan
       '(("git-open" "bin/git-open"))))
    (home-page "https://github.com/paulirish/git-open")
    (synopsis "Open a Git repository's homepage from the command-line")
    (description
     "@code{git open} opens the repository's website from the command-line,
guessing the URL pattern from the @code{origin} remote.")
    (license license:expat)))

(define-public tla
  (package
    (name "gnu-arch")
    (version "1.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.gnu.org/old-gnu/gnu-arch/"
                                  "tla-" version ".tar.gz"))
              (sha256
               (base32
                "01mfzj1i6p4s8191cgd5850hds1zls88hkf9rb6qx1vqjv585aj0"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; In tar 1.32, '--preserve' is ambiguous and leads to an
                  ;; error, so address that.
                  (substitute* "src/tla/libarch/archive.c"
                    (("\"--preserve\"")
                     "\"--preserve-permissions\""))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (chdir "src")

                        (mkdir "=build")
                        (chdir "=build")

                        ;; For libneon's 'configure' script.
                        ;; XXX: There's a bundled copy of neon.
                        (setenv "CONFIG_SHELL" (which "sh"))

                        (invoke "../configure" "--prefix" out
                                "--config-shell" (which "sh")
                                "--with-posix-shell" (which "sh")
                                "--with-cc" "gcc")))))


       ;; There are build failures when building in parallel.
       #:parallel-build? #f
       #:parallel-tests? #f

       #:test-target "test"))
    (native-inputs
     (list which))
    (synopsis "Historical distributed version-control system")
    (description
     "GNU Arch, aka. @code{tla}, was one of the first free distributed
version-control systems (DVCS).  It saw its last release in 2006.  This
package is provided for users who need to recover @code{tla} repositories and
for historians.")
    (home-page "https://www.gnu.org/software/gnu-arch/")
    (license license:gpl2)))                      ;version 2 only

(define-public diff-so-fancy
  (package
    (name "diff-so-fancy")
    (version "1.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/so-fancy/diff-so-fancy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11vkq5njjlvjipic7db44ga875n61drszw1qrdzwxmmfmnz425zz"))))
    (inputs
     (list bash-minimal perl ncurses))
    (build-system copy-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-lib-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               (substitute* "diff-so-fancy"
                 (("use lib.*$")
                  (string-append "use lib '" lib "';\n"))))))
         (add-after 'install 'symlink-executable
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (ncurses (assoc-ref inputs "ncurses"))
                   (perl (assoc-ref inputs "perl")))
               (wrap-program (string-append out "/bin/diff-so-fancy")
                 `("PATH" ":" prefix (,(string-append ncurses "/bin")
                                      ,(string-append perl "/bin"))))))))
       #:install-plan
       '(("lib" "lib")
         ("diff-so-fancy" "bin/"))))
    (home-page "https://github.com/so-fancy/diff-so-fancy")
    (synopsis "Makes diffs more human friendly and readable")
    (description
     "@code{diff-so-fancy} strives to make your diffs human readable instead
of machine readable.  This helps improve code quality and helps you spot
defects faster.")
    (license license:expat)))

(define-public gita
  (let ((commit "e41b504dca90a25e9be27f296da7ce22e5782893")
        (revision "1"))
    (package
      (name "gita")
      (version (git-version "0.12.9" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nosarthur/gita")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1k03zgcbhl91cgyh4k7ywyjp00y63q4bqbimncqh5b3lni8l8j5l"))))
      (build-system python-build-system)
      (native-inputs
       (list git ;for tests
             python-pytest))
      (propagated-inputs
       (list python-pyyaml))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "tests/test_main.py"
                 (("'gita\\\\n'") "'source\\n'")
                 (("'gita'") "'source'"))
               (invoke (search-input-file inputs "/bin/git")
                       "init")
               (add-installed-pythonpath inputs outputs)
               (invoke (search-input-file inputs "/bin/pytest")
                       "-vv" "tests")))
           (add-after 'install 'install-shell-completions
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bash-completion (string-append out "/etc/bash_completion.d"))
                      (zsh-completion (string-append out "/etc/zsh/site-functions")))
                 (mkdir-p bash-completion)
                 (copy-file ".gita-completion.bash"
                            (string-append bash-completion "/gita"))
                 (mkdir-p zsh-completion)
                 (copy-file ".gita-completion.zsh"
                            (string-append zsh-completion "/_gita"))))))))
      (home-page "https://github.com/nosarthur/gita")
      (synopsis "Command-line tool to manage multiple Git repos")
      (description "This package provides a command-line tool to manage
multiple Git repos.

This tool does two things:
@itemize
@item display the status of multiple Git repos such as branch, modification,
commit message side by side
@item (batch) delegate Git commands/aliases from any working directory
@end itemize

If several repos are related, it helps to see their status together.")
      (license license:expat))))

(define-public ghq
  (package
    (name "ghq")
    (version "1.7.1")
    (home-page "https://github.com/x-motemen/ghq")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ai3klp3fm5r0idnml5pm55wcvkav3w0s11snlmr0ab1ki8m9sg5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/x-motemen/ghq"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-completions
            (lambda* (#:key outputs import-path #:allow-other-keys)
              (let* ((out #$output)
                     (bash-completion (string-append out "/etc/bash_completion.d"))
                     (zsh-completion (string-append out "/share/zsh/site-functions")))
                (with-directory-excursion (string-append "src/" import-path)
                  (mkdir-p bash-completion)
                  (copy-file "misc/bash/_ghq"
                             (string-append bash-completion "/ghq"))
                  (mkdir-p zsh-completion)
                  (copy-file "misc/zsh/_ghq"
                             (string-append zsh-completion "/_ghq")))))))))
    (native-inputs
     (list git-minimal))
    (inputs
     (list go-github-com-mattn-go-isatty
           go-github-com-motemen-go-colorine
           go-github-com-saracen-walker
           go-github-com-songmu-gitconfig
           go-github-com-urfave-cli-v2
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-text))
    (synopsis "Manage remote repository clones")
    (description
     "@code{ghq} provides a way to organize remote repository clones, like
@code{go get} does.  When you clone a remote repository by @code{ghq get}, ghq
makes a directory under a specific root directory (by default @file{~/ghq})
using the remote repository URL's host and path.")
    (license license:expat)))

(define-public tkrev
  (package
    (name "tkrev")
    (version "9.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/tkcvs/tkrev_" version ".tar.gz"))
       (sha256
        (base32 "0bpfbhkngzmwy476mfc69mkd94l0m2wxznrn0qzd81s450yxjw2q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (invoke "wish" "doinstall.tcl" "-nox" out)
               (install-file "contrib/tkdirdiff" bin))))
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
               (lambda (file)
                 (wrap-program (string-append (assoc-ref outputs "out")
                                              "/bin/" file)
                   `("PATH" ":" prefix (,(dirname (which "wish"))))))
               '("tkdiff"
                 "tkdirdiff"
                 "tkrev")))))
       #:tests? #f))
    (inputs
     (list bash-minimal tk))
    (home-page "https://tkcvs.sourceforge.io")
    (synopsis "Graphical interface to CVS, Subversion, Git, and RCS")
    (description
     "TkRev (formerly TkCVS) is a Tcl/Tk-based graphical interface to the CVS,
Subversion and Git configuration management systems.  It will also help with
RCS.  It shows the status of the files in the current working directory, and
has tools for tagging, merging, checking in/out, and other user operations.
TkDiff is included for browsing and merging your changes.")
    (license license:gpl2+)))

(define-public qgit
  (package
    (name "qgit")
    (version "2.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/tibirna/qgit")
                     (commit (string-append "qgit-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10j5xll7ai1rb2ybyblbgqm762bqspffpf33fdr61qdchnp2gkf4"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f)) ;no tests
    (propagated-inputs
     (list git))
    (home-page "https://github.com/tibirna/qgit")
    (synopsis "Graphical front-end for git")
    (description
     "Qgit is a graphical front-end for git, with features to:
@itemize
@item view revisions, diffs, files history, files annotation and archive tree,
@item commit changes visually cherry picking modified files,
@item apply or save patch series from selected commits, drag and drop commits,
@item associate commands sequences, scripts and anything else executable to a
 custom action,
@item push/pop commits,
@item apply/save/create patches
@item and cherry pick single modified files.
@end itemize")
    (license license:gpl3+)))

(define-public git-filter-repo
  (package
    (name "git-filter-repo")
    (version "2.45.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/newren/git-filter-repo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03sjxscj7pkldvwcvlqi6k79rcxkd2fyy1rjvpwyp4jgni5kddkx"))
       ;; TODO: Remove when updating to 2.47.0.
       (modules '((guix build utils)))
       (snippet
        #~(substitute* "t/t9390-filter-repo.sh"
            (("(test_expect_success) ('--version')" _ prefix suffix)
             (string-append prefix " IN_FILTER_REPO_CLONE " suffix))))
       ;; Modified from <https://github.com/newren/git-filter-repo/pull/477>.
       ;; Used with 'unpack-git-source phase.
       (patches (search-patches "git-filter-repo-generate-doc.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%default-gnu-imported-modules
        (guix build python-build-system))
      #:modules
      '((guix build gnu-build-system)
        ((guix build python-build-system) #:select (site-packages))
        (guix build utils)
        (srfi srfi-26))
      #:make-flags
      #~(list (string-append "prefix=" #$output)
              (string-append "VERSION=" #$(package-version this-package)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'unpack-git-source
            (lambda _
              (let* ((old-path (getcwd))
                     (doc-source (string-append old-path "/Documentation")))
                (mkdir-p "git-source")
                (chdir "git-source")
                ((assoc-ref %standard-phases 'unpack)
                 #:source
                 #+(package-source (this-package-native-input "git-minimal")))
                (for-each
                 (cut install-file <> doc-source)
                 (find-files "." "asciidoc\\.conf\\.in$|manpage.*\\.xsl$"))
                ;; These attributes are probably not needed.
                (with-directory-excursion doc-source
                  (substitute* "asciidoc.conf.in"
                    (("@GIT_(VERSION|DATE)@") ""))
                  (rename-file "asciidoc.conf.in" "asciidoc.conf"))
                (chdir old-path)
                (delete-file-recursively "git-source"))))
          (add-before 'build 'set-pythondir
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "Makefile"
                (("(pythondir = ).*" _ pre)
                 (string-append pre (site-packages inputs outputs))))))
          (replace 'build
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "doc" make-flags)))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (substitute* "t/t9391-filter-repo-lib-usage.sh"
                (("/bin/bash") (which "bash")))
              (when tests?
                (invoke "t/run_tests")))))))
    (native-inputs
     (list asciidoc
           docbook-xsl
           git-minimal
           libxml2                        ;for XML_CATALOG_FILES
           xmlto
           perl
           rsync))
    (inputs (list python))                ;for the shebang
    (home-page "https://github.com/newren/git-filter-repo")
    (synopsis "Quickly rewrite Git repository history")
    (description
     "@command{git filter-repo} is a versatile tool for rewriting history,
which roughly falls into the same space of tool like git filter-branch but
with more capabilities.  @command{git filter-repo} is now recommended by the
Git project instead of @command{git filter-branch}.")
    (license (list license:expat ;; Main license.
                   license:gpl2)))) ;; For test harness.

(define-public gitlint
  (package
    (name "gitlint")
    (version "0.17.0")
    (source (origin
              (method url-fetch)
              ;; the gitlint-core pypi package contains the actual gitlint
              ;; code; the gitlint package only pulls in gitlint-core with
              ;; stricter dependency versioning
              (uri (pypi-uri "gitlint-core" version))
              (sha256
               (base32
                "14cn89biys8r7mwcdgllv371k34km9k1941ylxf53a7sxwrzsbbp"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'loosen-requirements
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "gitlint/shell.py"
                (("'git'") (string-append
                            "'"
                            (search-input-file inputs "bin/git")
                            "'"))
                ;; force using subprocess instead of sh so git does not need
                ;; to be a propagated input
                (("if USE_SH_LIB") "if False")))))))
    (inputs
     (list git python-arrow python-click python-sh))
    (home-page "https://jorisroovers.com/gitlint/")
    (synopsis "Linting Git commit messages")
    (description
     "Gitlint is a Git commit message linter written in Python: it checks your
commit messages for style.")
    (license license:expat)))

(define-public git-extras
  (package
    (name "git-extras")
    (version "7.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tj/git-extras")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lig1sbk83qqvbvpmpcjaf23nk0r7snny5lix75ym1z320970xni"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; XXX: Tests require additional setup with Pytest, see
      ;; <.github/workflows/ci.yml>.
      #:tests? #f
      #:make-flags
      #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          ;; No configure script, build process, or tests.
          (delete 'bootstrap)
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          (add-after 'unpack 'hardcode-dependency-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Write to and copy from temporary file to prevent
              ;; "make: bash: Argument list too long" errors.
              (let* ((temp-file (mkstemp! "temp-file.XXXXXX"))
                     (temp-filename (port-filename temp-file)))
                (map (lambda (name)
                       (format temp-file "export PATH=$PATH:~a/bin~%"
                               (assoc-ref inputs name)))
                     (list "coreutils-minimal"
                           "curl"
                           "findutils"
                           "gawk"
                           "less"
                           "ncurses"
                           "procps"
                           "rsync"
                           "sed"
                           "util-linux"))
                ;; The Makefile injects helper scripts and functions into each
                ;; script. This substitution injects a PATH appending the bin
                ;; directory of each non-propagated input in order to minimize
                ;; the number of packages propagated to the profile.
                (substitute* "Makefile"
                  (("head -1 bin/\\$\\(COMMAND\\) > \\$\\(TEMPFILE\\); \\\\" line)
                   (string-append
                    line "\n"
                    "cat " temp-filename " >> $(TEMPFILE); \\")))))))))
    (propagated-inputs (list git))
    (inputs
     (list coreutils-minimal
           curl
           findutils
           gawk
           less
           ncurses
           procps
           rsync
           sed
           util-linux))
    (home-page "https://github.com/tj/git-extras")
    (synopsis "Additional Git utilities")
    (description "The git-extras package provides a collection of additional
git commands for repository metrics and summarization, commit and log editing,
developer workflow, and project and release management.")
    (license license:expat)))

(define-public hut
  (package
    (name "hut")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~xenrox/hut")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14cia976i2jdzyzw4wk9fhkh6zqgmb09ryf31ys24smmfcdfxyf1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~xenrox/hut"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                ;; The flags are copied from (guix build go-build-system).
                (setenv "CGO_LDFLAGS" "-s -w")
                (invoke "make" "all" "GOFLAGS=-v -x"))))
          (replace 'install
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "make" "install"
                        (string-append "PREFIX=" #$output))))))))
    (native-inputs
     (list scdoc))
    (inputs
     (list go-git-sr-ht-emersion-go-scfg
           go-git-sr-ht-emersion-gqlclient
           go-github-com-dustin-go-humanize
           go-github-com-google-shlex
           go-github-com-juju-ansiterm
           go-github-com-spf13-cobra
           go-golang-org-x-term))
    (home-page "https://git.sr.ht/~xenrox/hut")
    (synopsis "CLI tool for sr.ht")
    (description "@command{hut} is a CLI tool for
@uref{https://sr.ht/~sircmpwn/sourcehut/, sr.ht}.  It helps you interact with
sr.ht's public services:
@table @asis
@item builds
submit and manage build jobs
@item git
create, and manage git repositories and artifacts
@item hg
list Mercurial repositories
@item lists
manage mailing lists and patches
@item meta
manage PGP, and SSH keys
@item pages
publish and manage hosted websites
@item paste
create and manage pastes
@item todo
create and manage trackers, tickets
@item graphql
interact with GraphQL APIs directly
@end table")
    (license license:agpl3)))

(define-public commit-patch
  (package
    (name "commit-patch")
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/caldwell/commit-patch/releases/download/"
                    version "/commit-patch-" version ".tar.gz"))
              (sha256
               (base32
                "0v11vjyisk243zi0ym90bnqb229j7iaqx1lwqdkszxzn1yxwq4ck"))))
    (build-system copy-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-paths
                          (lambda* (#:key inputs #:allow-other-keys)
                            (patch-shebang "commit-patch"))))
           #:install-plan ''(("commit-patch" "bin/")
                             ("commit-patch-buffer.el"
                              "share/emacs/site-lisp/"))))
    (inputs (list perl))
    (propagated-inputs (list patchutils))
    (synopsis "Commit parts of changes to VCS repositories")
    (description
     "commit-patch is a utility that lets you check in select portions of a
file into Darcs, Git, Mercurial, Bazaar, Subversion, or CVS repositories.  It
comes as a command line app and also an Emacs interface.")
    (home-page "https://porkrind.org/commit-patch/")
    (license license:gpl2+)))

(define-public git-sizer
  (package
    (name "git-sizer")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/github/git-sizer")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b4sl4djnfaxwph41y4bh9yal4bpd1nz4403ryp7nzna7h2x0zis"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/github/git-sizer"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* '("src/github.com/github/git-sizer/git/git.go")
               (("gitBin, err := findGitBin\\(\\)")
                (string-append "gitBin := \""
                               (search-input-file inputs "bin/git")
                               "\"\n\tvar err error")))
             (substitute* '("src/github.com/github/git-sizer/git_sizer_test.go")
               (("bin/git-sizer")
                (string-append (assoc-ref outputs "out")
                               "/bin/git-sizer")))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (when tests?
               (for-each (lambda (test)
                           (invoke "go" "test" "-v" "-run" test import-path))
                         ;; TestExec and TestSubmodule require a copy of the
                         ;; Git repository.
                         '("TestBomb" "TestFromSubdir" "TestRefgroups"
                           "TestRefSelections" "TestTaggedTags"))))))))
    (inputs (list git-minimal/pinned))
    (propagated-inputs
     (list go-github-com-cli-safeexec
           go-github-com-davecgh-go-spew
           go-github-com-pmezard-go-difflib
           go-github-com-spf13-pflag
           go-github-com-stretchr-testify
           go-go-uber-org-goleak
           go-golang-org-x-sync
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/github/git-sizer")
    (synopsis "Analyze size of a Git repo")
    (description "Compute various size metrics for a Git repository, flagging
those that might cause problems or inconvenience.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
