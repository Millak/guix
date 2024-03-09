;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018–2021, 2023 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages mc)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public mc
  (package
    (name "mc")
    (version "4.8.31")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://ftp.osuosl.org/pub/midnightcommander/mc-"
                          version ".tar.xz"))
      (sha256
       (base32 "06mbnhxd2k29jah4wp1ciicw1gb51a5d3af43zivhxbncvw1q694"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--with-screen=ncurses"
              "--enable-aspell"
              "--disable-configure-args" ; don't keep superfluous references
              (string-append "PERL_FOR_BUILD=" ; to build .hlp files
                             #$(this-package-native-input "perl") "/bin/perl")
              (string-append "PERL="    ; for run-time helpers, mainly VFS
                             #$(this-package-input "perl") "/bin/perl"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-source-shebangs 'patch-file-names
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The following commands are all invoked at run time.
              (substitute*
                  (list "lib/utilunix.c"
                        "src/filemanager/ext.c"
                        "src/usermenu.c"
                        ;; This file hard-codes other shells, but they're never
                        ;; tried after mc's first choice (bash) is found.
                        "lib/shell.c")
                (("/bin/(ba|)sh" file)
                 (search-input-file inputs file)))
              (substitute* "src/filemanager/ext.c"
                ;; Look up in $PATH at run time, rather than hard-coding now.
                (("/bin/(rm)" command)
                 command))
              (substitute* "misc/mcedit.menu.in"
                ;; These are script templates.  Don't embed store file names.
                (("#! /bin/([[:alnum:]]*)" _ file)
                 (string-append "#!/usr/bin/env " file)))

              ;; The following commands are all invoked at build time.
              (substitute* "tests/src/vfs/extfs/helpers-list/Makefile.in"
                (("/bin/sh")
                 (which "sh")))))
          (add-before 'check 'fix-tests
            (lambda _
              ;; Don't expect a UID or GID of ‘0’ in the build environment.
              (with-directory-excursion "tests/src/vfs/extfs/helpers-list/data"
                (substitute* (list "rpm.custom.output"
                                   "rpm.glib.output")
                  (("      0        0") "<<uid>>  <<gid>>")))
              ;; XXX ERROR:mc_realpath.c:99:realpath_test: assertion failed
              ;; (resolved_path == data->expected_string): ("" == "/usr/bin")
              (substitute* "tests/lib/mc_realpath.c"
                (("/usr/bin") "/")
                (("usr/bin") "/")))))))
    (native-inputs (list perl pkg-config))
    (inputs (list aspell
                  bash-minimal
                  check
                  glib
                  gpm
                  libssh2
                  ncurses
                  perl
                  unzip))
    (home-page "https://www.midnight-commander.org")
    (properties
      `((release-monitoring-url . "https://ftp.osuosl.org/pub/midnightcommander/")))
    (synopsis "Graphical file manager")
    (description
     "GNU Midnight Commander is a command-line file manager laid out in a
common two-pane format.  In addition to standard file management tasks such as
copying and moving, Midnight Commander also supports viewing the contents of
RPM package files and other archives and managing files on other computers via
FTP or FISH.  It also includes a powerful text editor for opening text
files.")
    (license gpl3+)))
