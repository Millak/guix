;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Diego Nicola Barbato <dnbarbato@posteo.de>
;;; Copyright © 2024 Homo <gay@disroot.org>
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

(define-module (gnu packages inferno)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages plan9)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public inferno
  ;; There are no tags, also a lot of improvements and bugfixes happenned since 2015.
  (let ((commit "67e70befb2ad0058fd7894be34c492ddb6d09988")
        (revision "0"))
    (package
      (name "inferno")
      (version (git-version "4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/inferno-os/inferno-os")
               (commit commit)
               ;; Inferno uses customized Freetype library with #include "lib9.h"
               ;; TODO: use packaged Freetype library.
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0g3fzb991hbwa7r3vsnybw7m1v13nins5ajmygnvz4vmx5rzl405"))
         (patches (search-patches "inferno-fix-crash.patch"))
         (modules '((guix build utils)))
         ;; Remove bundled non-free and potentially non-free fonts.
         (snippet '(delete-file-recursively "fonts"))))
      (build-system gnu-build-system)
      (arguments
       (list
        ;; Force a 32-bit build targeting a similar architecture, i.e.:
        ;; armhf for armhf/aarch64, i686 for i686/x86_64.
        ;; TODO: mips and powerpc.
        #:system (match (%current-system)
                   ((or "armhf-linux" "aarch64-linux")
                    "armhf-linux")
                   (_ "i686-linux"))
        ;; No tests.
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (let* ((root (string-append #$output "/inferno")))
                  ;; Copy directly to 'root' since the source
                  ;; tree doubles as Inferno's root directory
                  ;; and its path is baked into the emu binary.
                  (mkdir-p root)
                  (copy-recursively source root)
                  (chdir root))))
            ;; Scripts running in virtual machine need /dis/sh.
            (delete 'patch-generated-file-shebangs)
            (delete 'patch-shebangs)
            (replace 'patch-source-shebangs
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Substitute ocurrences of /bin/sh.
                (for-each (lambda (file)
                            (substitute* file
                              (("/bin/sh")
                               (search-input-file inputs "/bin/bash"))))
                          '("makemk.sh"
                            "mkfiles/mkhost-Linux"
                            "emu/Linux/mk-wrt"
                            "utils/mk/Posix.c"))))
            (add-after 'patch-source-shebangs 'patch-src-files
              (lambda _
                ;; Do not pass '-m32' unconditionally
                ;; when building mk.
                (substitute* "makemk.sh" (("-m32") ""))
                ;; Use the correct name for gcc.
                (substitute* "mkfiles/mkfile-Linux-386"
                  (("cc -")
                   ;; Don't match yacc.
                   "gcc -"))
                (substitute* "mkfiles/mkfile-Linux-arm"
                  (("arm-gcc")
                   "gcc"))
                ;; Conflicting occurence of fsub.
                (for-each (lambda (file)
                            (substitute* file
                              (("fsub")
                               "_fsub")))
                          '("utils/libmach/vcodas.c"
                            "utils/libmach/vdb.c"))
                ;; Fix build.
                (for-each (lambda (file)
                            (substitute* file
                              (("^CFLAGS=")
                               "CFLAGS=-D_GNU_SOURCE -fcommon")))
                          '("mkfiles/mkfile-Linux-386"
                            "mkfiles/mkfile-Linux-arm"
                            "mkfiles/mkfile-Linux-power"
                            "mkfiles/mkfile-Linux-spim"))
                ;; Make build reproducible by
                ;; ensuring timestamps embedded into
                ;; binaries are set to 0.
                (substitute* "emu/Linux/mkfile"
                  (("^KERNDATE=.*$")
                   "KERNDATE=0\n"))))
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((objtype #$@(match (%current-system)
                                     ((or "armhf-linux" "aarch64-linux")
                                      `("arm"))
                                     (_ `("386"))))
                       (root (string-append #$output "/inferno"))
                       (root/bindir (string-append root "/Linux/" objtype "/bin")))
                  (substitute* "mkconfig"
                    (("ROOT=/usr/inferno")
                     (string-append "ROOT=" root))
                    (("SYSHOST=Plan9")
                     "SYSHOST=Linux")
                    (("OBJTYPE=\\$objtype")
                     (string-append "OBJTYPE=" objtype)))
                  ;; Delete pre-built binaries.
                  (delete-file-recursively root/bindir)
                  (mkdir-p root/bindir)
                  ;; Avoid duplicating objtype variable later.
                  (symlink root/bindir (string-append #$output "/inferno/bin"))
                  (setenv "PATH"
                    (string-append
                      ;; These utilities will be used later in build.
                      root/bindir ":"
                      (getenv "PATH") ":"
                      ;; Bootstrap mk binary.
                      (search-input-directory inputs "/plan9/bin"))))))
            (replace 'build
              (lambda _
                ;; Build emu, utilities and Dis binaries using mk.
                (invoke "mk" "-s" "nuke" "mkdirs" "install")))
            (replace 'install
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((bindir (string-append #$output "/bin"))
                       (emu-script (string-append bindir "/emu"))
                       (infwm-script (string-append bindir "/infwm"))
                       (root/bindir (string-append #$output "/inferno/bin"))
                       (simulate-fonts
                         (lambda (size targets)
                           (string-concatenate
                             (map (lambda (file)
                                    (string-append
                                      "ramfile /fonts/" file ".font; "
                                      "bind /fonts/fixed/unicode." size ".font"
                                      " /fonts/" file ".font; "))
                                  targets))))
                       (write-script
                         (lambda (file cmds)
                           (with-output-to-file file
                             (lambda ()
                               (display
                                 (string-concatenate
                                   (cons* "#!"
                                          (search-input-file inputs "/bin/bash")
                                          "\n"
                                          cmds)))))
                           (chmod file #o755))))
                  (mkdir-p bindir)
                  (mkdir-p "fonts")
                  ;; Install emu script.
                  (write-script emu-script (list
                    "exec " root/bindir "/emu sh -c \""
                    ;; Use plan9port's fonts.
                    "bind '#U*"
                    (search-input-directory inputs "/plan9/font")
                    "' /fonts; "
                    ;; Pretend fonts were not removed.
                    ;; TODO: de-hardcode fonts from apps.
                    ;; TODO: use fonts available in the host system,
                    ;;       plan9port's fontsrv is a good way to start.
                    ;; TODO: find fix for minitel fonts.
                    "memfs -b /fonts; "
                    (string-concatenate
                      (map (lambda (dir)
                             (string-append
                              "mkdir /fonts/" dir "; "
                              "bind /fonts/fixed /fonts/" dir "; "))
                           '("charon" "lucida" "lucidasans" "lucm" "pelm")))
                    (simulate-fonts "6x9"
                                    '("charon/cw.tiny"
                                      "charon/plain.tiny"
                                      "lucida/unicode.6"
                                      "lucidasans/latin1.6"
                                      "lucidasans/typelatin1.6"
                                      "lucidasans/unicode.6"))
                    (simulate-fonts "7x13"
                                    '("charon/cw.small"
                                      "charon/plain.small"
                                      "lucida/unicode.7"
                                      "lucidasans/latin1.7"
                                      "lucidasans/typelatin1.7"
                                      "lucidasans/unicode.7"))
                    (simulate-fonts "8x13"
                                    '("charon/cw.normal"
                                      "charon/plain.normal"
                                      "lucida/unicode.8"
                                      "lucidasans/euro.8"
                                      "lucidasans/latin1.8"
                                      "lucidasans/typelatin1.8"
                                      "lucidasans/unicode.8"
                                      "pelm/unicode.8"))
                    (simulate-fonts "9x15"
                                    '("charon/cw.large"
                                      "charon/plain.large"
                                      "lucida/unicode.10"
                                      "lucidasans/latin1.10"
                                      "lucidasans/typelatin1.10"
                                      "lucidasans/unicode.10"
                                      "lucm/unicode.9"
                                      "pelm/ascii.12"
                                      "pelm/latin1.9"
                                      "pelm/unicode.9"))
                    (simulate-fonts "10x20"
                                    '("charon/cw.vlarge"
                                      "charon/plain.vlarge"
                                      "lucida/moo.16"
                                      "lucida/unicode.13"
                                      "lucidasans/latin1.13"
                                      "lucidasans/typelatin1.13"
                                      "lucidasans/unicode.13"
                                      "pelm/ascii.16"))
                    (simulate-fonts "6x13O"
                                    '("charon/italic.tiny"
                                      "charon/italic.small"
                                      "lucidasans/italiclatin1.6"
                                      "lucidasans/italiclatin1.7"))
                    (simulate-fonts "7x13O"
                                    '("charon/italic.normal"
                                      "lucidasans/italiclatin1.8"))
                    (simulate-fonts "8x13O"
                                    '("charon/italic.large"
                                      "charon/italic.vlarge"
                                      "lucidasans/italiclatin1.10"
                                      "lucidasans/italiclatin1.13"))
                    (simulate-fonts "6x13B"
                                    '("charon/bold.tiny"
                                      "lucidasans/boldlatin1.6"))
                    (simulate-fonts "7x13B"
                                    '("charon/bold.small"
                                      "lucidasans/boldlatin1.7"))
                    (simulate-fonts "8x13B"
                                    '("charon/bold.normal"
                                      "lucidasans/boldlatin1.8"))
                    (simulate-fonts "9x15B"
                                    '("charon/bold.large"
                                      "lucidasans/boldlatin1.10"))
                    (simulate-fonts "9x18B"
                                    '("charon/bold.vlarge"
                                      "lucidasans/boldlatin1.13"))
                    ;; Bind the host's /tmp to Inferno's
                    ;; /tmp to make it writable.
                    "bind -bc '#U*/tmp' /tmp; "
                    ;; Bind the host's /home to Inferno's
                    ;; /usr.
                    "bind '#U*/home' /usr; "
                    "$*\"\n"))
                  ;; Install infwm script.
                  (write-script infwm-script (list
                    "exec " bindir "/emu $* wm/wm wm/logon -u $USER\n"))
                  ;; Install a symlink to the Limbo compiler.
                  (symlink (string-append root/bindir "/limbo")
                           (string-append bindir "/limbo")))))
            ;; Inferno doesn't compress man pages.
            (delete 'compress-documentation))))
      (inputs (list libx11 libxext plan9port xorgproto))
      (home-page "https://www.inferno-os.org")
      (synopsis
       "Compact operating system for building cross-platform distributed systems")
      (description
       ;; Except for the last sentence this is taken verbatim from the
       ;; intro(1) man page (man/1/0intro in the source tree).
       "Inferno is a virtualised operating system that can run natively across
a wide range of processor architectures or hosted on a wide range of
operating systems.  The principal components of the system are:
@itemize
@item The Inferno kernel which can run both native and hosted on a
range of platforms and which presents the same interface to programs
in both cases.
@item The Dis virtual machine.
@item Styx - the tiny broad-spectrum file service protocol.
@item Limbo - a new simple, modular, concurrent programming language.
@item Tk and Prefab - graphical user interface (GUI) primitives
without a lot of goo.
@item The portable cross-development suites that allow any native
Inferno platform to be cross-compiled on any hosted system.
@end itemize
This package provides hosted Inferno.
")
      (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"
                           "aarch64-linux"))
      (license (list license:expat ;MIT license
                     license:freetype
                     ;; According to NOTICE the combined work is effectively
                     ;; GPLv2+.
                     license:gpl2+
                     license:lgpl2.0+)))))
