;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2021, 2021-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015-2018, 2020, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Raymond Nicholson <rain1@openmailbox.org>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016, 2018-2023, 2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016, 2018, 2019, 2020, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016-2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2016 Carlos Sánchez de La Lama <csanchezdll@gmail.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020, 2021, 2022 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2017, 2019, 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018, 2020, 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2018 Manuel Graf <graf@init.at>
;;; Copyright © 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2019 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 mikadoZero <mikadozero@yandex.com>
;;; Copyright © 2019-2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Stefan Stefanović <stefanx2ovic@gmail.com>
;;; Copyright © 2019-2022 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Anders Thuné <asse.97@gmail.com>
;;; Copyright © 2020, 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2020 David Dashyan <mail@davie.li>
;;; Copyright © 2020 pukkamustard <pukkamustard@posteo.net>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021, 2023 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Josselin Poiret <josselin.poiret@protonmail.ch>
;;; Copyright © 2021 Olivier Dion <olivier.dion@polymtl.ca>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021, 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Ryan Sundberg <ryan@arctype.co>
;;; Copyright © 2022-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2022 Rene Saavedra <nanuui@protonmail.com>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022, 2023 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2022 Hunter Jozwiak <hunter.t.joz@gmail.com>
;;; Copyright © 2022 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2022 Stefan <stefan-guix@vodafonemail.de>
;;; Copyright © 2022, 2023 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 dan <i@dan.games>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
;;; Copyright © 2023, 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2024 Gabriel Wicki <gabriel@erlikon.ch>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages apparmor)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages rrdtool)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (guix platform)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system linux-module)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix deprecation)    ;for libcap/next
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (customize-linux
            make-defconfig))



;;;
;;; Linux kernel customization functions.
;;;

(define* (customize-linux #:key name
                          (linux linux-libre)
                          source
                          defconfig
                          (configs "")
                          extra-version)
  "Make a customized Linux package NAME derived from the LINUX package.

If NAME is not given, then it defaults to the same name as the LINUX package.

Unless SOURCE is given the source of LINUX is used.

A DEFCONFIG file to be used can be given as an origin, as a file-like object
(file-append, local-file etc.), or as a string with the name of a defconfig file
available in the Linux sources.  If DEFCONFIG is not given, then a defconfig
file will be saved from the LINUX package configuration.

Additional CONFIGS will be used to modify the given or saved defconfig, which
will finally be used to build Linux.

CONFIGS can be a list of strings, with one configuration per line.  The usual
defconfig syntax has to be used, but there is a special extension to ease the
removal of configurations.  Comment lines are supported as well.

Here is an example:

  '(;; This string defines the version tail in 'uname -r'.
    \"CONFIG_LOCALVERSION=\\\"-handcrafted\\\"
    ;; This '# CONFIG_... is not set' syntax has to match exactly!
    \"# CONFIG_BOOT_CONFIG is not set\"
    \"CONFIG_NFS_SWAP=y\"
    ;; This is a multiline configuration:
    \"CONFIG_E1000=y
# This is a comment, below follows an extension to unset a configuration:
CONFIG_CMDLINE_EXTEND\")

A string of configurations instead of a list of configuration strings is also
possible.

EXTRA-VERSION can be a string overwriting the EXTRAVERSION setting of the LINUX
package, after being prepended by a hyphen.  It will be visible in the output
of 'uname -r' behind the Linux version numbers."
  (package
    (inherit linux)
    (name (or name (package-name linux)))
    (source (or source (package-source linux)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments linux)
       ((#:imported-modules imported-modules %default-gnu-imported-modules)
        `((guix build kconfig) ,@imported-modules))
       ((#:modules modules)
        `((guix build kconfig) ,@modules))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys #:rest arguments)
                (setenv "EXTRAVERSION"
                        #$(and extra-version
                               (not (string-null? extra-version))
                               (string-append "-" extra-version)))
                (let* ((configs (string-append "arch/" #$(linux-srcarch)
                                               "/configs/"))
                       (guix_defconfig (string-append configs
                                                      "guix_defconfig")))
                  #$(cond
                     ((not defconfig)
                      #~(begin
                          ;; Call the original 'configure phase.
                          (apply (assoc-ref #$phases 'configure) arguments)
                          ;; Save a defconfig file.
                          (invoke "make" "savedefconfig")
                          ;; Move the saved defconfig to the proper location.
                          (rename-file "defconfig"
                                       guix_defconfig)))
                     ((string? defconfig)
                      ;; Use another existing defconfig from the Linux sources.
                      #~(rename-file (string-append configs #$defconfig)
                                     guix_defconfig))
                     (else
                      ;; Copy the defconfig input to the proper location.
                      #~(copy-file #$defconfig guix_defconfig)))
                  (chmod guix_defconfig #o644)
                  (modify-defconfig guix_defconfig '#$configs)
                  (invoke "make" "guix_defconfig")
                  (verify-config ".config" guix_defconfig))))))))))

(define (make-defconfig uri sha256-as-base32)
  (origin (method url-fetch)
          (uri uri)
          (sha256 (base32 sha256-as-base32))))

(define (linux-srcarch)
  "Return the linux SRCARCH name, which is set in the toplevel Makefile of
Linux and denotes the architecture-specific directory name below arch/ in its
source code.  Some few architectures share a common folder.  It resembles the
definition of SRCARCH based on ARCH in the Makefile and may be used to place a
defconfig file in the proper path."
  (let ((linux-arch (platform-linux-architecture
                     (lookup-platform-by-target-or-system
                      (or (%current-target-system)
                          (%current-system))))))
    (match linux-arch
      ("i386"    "x86")
      ("x86_64"  "x86")
      ("sparc32" "sparc")
      ("sparc64" "sparc")
      ("sh64"    "sh")
      (_         linux-arch))))

(define-public (system->defconfig system)
  "Some systems (notably powerpc-linux) require a special target for kernel
defconfig.  Return the appropriate Make target if applicable, otherwise return
\"defconfig\"."
  (cond ((string-prefix? "powerpc-" system) "pmac32_defconfig")
        ((string-prefix? "powerpc64-" system) "ppc64_defconfig")
        ((string-prefix? "powerpc64le-" system) "ppc64_defconfig")
        (else "defconfig")))


;;;
;;; Kernel source code deblobbing.
;;;

(define (linux-libre-deblob-scripts version gnu-revision
                                    deblob-hash
                                    deblob-check-hash)
  (list (version-major+minor version)
        (origin
          (method url-fetch)
          (uri (string-append "https://linux-libre.fsfla.org"
                              "/pub/linux-libre/releases/" version "-" gnu-revision "/"
                              "deblob-" (version-major+minor version)))
          (file-name (string-append "linux-libre-deblob-"
                                    version "-" gnu-revision))
          (sha256 deblob-hash))
        (origin
          (method url-fetch)
          (uri (string-append "https://linux-libre.fsfla.org"
                              "/pub/linux-libre/releases/" version "-" gnu-revision "/"
                              "deblob-check"))
          (file-name (string-append "linux-libre-deblob-check-" version "-" gnu-revision))
          (sha256 deblob-check-hash))))

;; XXXX: Workaround 'snippet' limitations
(define computed-origin-method (@@ (guix packages) computed-origin-method))

(define (make-linux-libre-source version
                                 upstream-source
                                 deblob-scripts)
  "Return a 'computed' origin that generates a Linux-libre tarball from the
corresponding UPSTREAM-SOURCE (an origin), using the given DEBLOB-SCRIPTS."
  (match deblob-scripts
    ((deblob-version (? origin? deblob) (? origin? deblob-check))
     (unless (string=? deblob-version (version-major+minor version))
       ;; The deblob script cannot be expected to work properly on a
       ;; different version (major+minor) of Linux, even if no errors
       ;; are signaled during execution.
       (error "deblob major+minor version mismatch"))
     (origin
       (method computed-origin-method)
       (file-name (string-append "linux-libre-" version "-guix.tar.xz"))
       (sha256 #f)
       (uri
        (delay
          (with-imported-modules '((guix build utils))
            #~(begin
                (use-modules (guix build utils)
                             (srfi srfi-1)
                             (ice-9 match)
                             (ice-9 ftw))

                (setvbuf (current-output-port) 'line)

                (let ((dir (string-append "linux-" #$version)))

                  (mkdir "/tmp/bin")
                  (set-path-environment-variable
                   "PATH" '("bin")
                   (list "/tmp"
                         #+(canonical-package bash)
                         #+(canonical-package coreutils)
                         #+(canonical-package diffutils)
                         #+(canonical-package findutils)
                         #+(canonical-package patch)
                         #+(canonical-package xz)
                         #+(canonical-package sed)
                         #+(canonical-package grep)
                         #+(canonical-package bzip2)
                         #+(canonical-package gzip)
                         #+(canonical-package tar)
                         #+(canonical-package gawk)
                         #+python-wrapper))

                  (with-directory-excursion "/tmp/bin"

                    (copy-file #+deblob "deblob")
                    (chmod "deblob" #o755)
                    (substitute* "deblob"
                      (("/bin/sh") (which "sh")))

                    (copy-file #+deblob-check "deblob-check")
                    (chmod "deblob-check" #o755)
                    (substitute* "deblob-check"
                      (("/bin/sh") (which "sh"))
                      (("/bin/sed") (which "sed"))
                      (("/usr/bin/python") (which "python"))))

                  (if (file-is-directory? #+upstream-source)
                      (begin
                        (format #t "Copying upstream linux source...~%")
                        (invoke "cp" "--archive" #+upstream-source dir)
                        (invoke "chmod" "--recursive" "u+w" dir))
                      (begin
                        (format #t "Unpacking upstream linux tarball...~%")
                        (invoke "tar" "xf" #$upstream-source)
                        (match (scandir "."
                                        (lambda (name)
                                          (and (not (member name '("." "..")))
                                               (file-is-directory? name))))
                          ((unpacked-dir)
                           (unless (string=? dir unpacked-dir)
                             (rename-file unpacked-dir dir)))
                          (dirs
                           (error "multiple directories found" dirs)))))

                  (with-directory-excursion dir
                    (format #t "Running deblob script...~%")
                    (invoke "/tmp/bin/deblob"))

                  (format #t "~%Packing new Linux-libre tarball...~%")
                  (invoke "tar" "cvfa" #$output
                          ;; Avoid non-determinism in the archive.
                          "--mtime=@0"
                          "--owner=root:0"
                          "--group=root:0"
                          "--sort=name"
                          "--hard-dereference"
                          dir)

                  (format #t "~%Scanning the generated tarball for blobs...~%")
                  (invoke "/tmp/bin/deblob-check" "--use-awk" "--list-blobs"
                          #$output))))))))))


;;;
;;; Kernel sources.
;;;

(define (linux-libre-urls version gnu-revision)
  "Return a list of URLs for Linux-Libre VERSION."
  (list (string-append
         "https://linux-libre.fsfla.org/pub/linux-libre/releases/"
         version "-" gnu-revision "/linux-libre-" version "-" gnu-revision ".tar.xz")

        ;; XXX: Work around <http://bugs.gnu.org/14851>.
        (string-append
         "ftp://alpha.gnu.org/gnu/guix/mirror/linux-libre-"
         version "-" gnu-revision ".tar.xz")

        ;; Maybe this URL will become valid eventually.
        (string-append
         "mirror://gnu/linux-libre/" version "-" gnu-revision "/linux-libre-"
         version "-" gnu-revision ".tar.xz")))

(define (%upstream-linux-source version hash)
  (origin
    (method url-fetch)
    (uri (string-append "mirror://kernel.org"
                        "/linux/kernel/v" (version-major version) ".x/"
                        "linux-" version ".tar.xz"))
    (sha256 hash)))

;; The current "stable" kernels. That is, the most recently released major
;; versions that are still supported upstream.

(define-public linux-libre-6.14-version "6.14.8")
(define-public linux-libre-6.14-gnu-revision "gnu")
(define deblob-scripts-6.14
  (linux-libre-deblob-scripts
   linux-libre-6.14-version
   linux-libre-6.14-gnu-revision
   (base32 "00b6axy07ykdxk4qy3dnx5mvhz2dvbf78qxax3zq81bg11wbfvay")
   (base32 "076x15yp1qjhvv81si3aj3n6ny4l6yl1rcj7f7l12rlbl9p64vl2")))
(define-public linux-libre-6.14-pristine-source
  (let ((version linux-libre-6.14-version)
        (hash (base32 "0199maj3mk577wwaszbi0cl5a0afx1ynad896bmmg8vm636jxcb2")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-6.14)))

;; The "longterm" kernels — the older releases with long-term upstream support.
;; Here are the support timelines:
;; <https://www.kernel.org/category/releases.html>

(define-public linux-libre-6.12-version "6.12.29")
(define-public linux-libre-6.12-gnu-revision "gnu")
(define deblob-scripts-6.12
  (linux-libre-deblob-scripts
   linux-libre-6.12-version
   linux-libre-6.12-gnu-revision
   (base32 "0zqw2hw9f9vssqvizc0nlnp9m54a2inxg23gsf1wixhgc79wdhsi")
   (base32 "1sqgg46xnjvk1dcz7j2f5fys43d7b5m3s4fj2hx3wrvd982n14k6")))
(define-public linux-libre-6.12-pristine-source
  (let ((version linux-libre-6.12-version)
        (hash (base32 "0k86nmmpg0jsx11w34vlj20cxpxavip4y5g7dp4bkk1q4dzfrcp8")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-6.12)))

(define-public linux-libre-6.6-version "6.6.91")
(define-public linux-libre-6.6-gnu-revision "gnu")
(define deblob-scripts-6.6
  (linux-libre-deblob-scripts
   linux-libre-6.6-version
   linux-libre-6.6-gnu-revision
   (base32 "1fph0cf5vwkxcyz2cay4d70sqw1y02dx3shfclhp9y4nmkk169pq")
   (base32 "0qi8hhgl9sk0q56qxaiabwpsw5m3dnxf4sga0yj2lhbnapl60pjj")))
(define-public linux-libre-6.6-pristine-source
  (let ((version linux-libre-6.6-version)
        (hash (base32 "1x2lwaaqzlgszk41cy6k5j9dcbxxkca5xjaznb82r987ahbkv3fh")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-6.6)))

(define-public linux-libre-6.1-version "6.1.139")
(define-public linux-libre-6.1-gnu-revision "gnu")
(define deblob-scripts-6.1
  (linux-libre-deblob-scripts
   linux-libre-6.1-version
   linux-libre-6.1-gnu-revision
   (base32 "00yh14z7sr9pkakkkpdyp8fj41d56a306xsf5yb2lzm0sgl6lvza")
   (base32 "0kz3z4jjag5xjn5scp72is0f6ql550z6xixydc370qmvp8aswxix")))
(define-public linux-libre-6.1-pristine-source
  (let ((version linux-libre-6.1-version)
        (hash (base32 "064zzk7kwkq7i5160s0alzkz16pp89wcq07s9jhzhv4bxvgzyspn")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-6.1)))

(define-public linux-libre-5.15-version "5.15.183")
(define-public linux-libre-5.15-gnu-revision "gnu")
(define deblob-scripts-5.15
  (linux-libre-deblob-scripts
   linux-libre-5.15-version
   linux-libre-5.15-gnu-revision
   (base32 "1diav2baanmm26z1gh8qyfgwmw0pvws923rvnp637wal35wq9hkd")
   (base32 "19cwqpvf6x02ivxwx36jkbccg3dcii1ynis34mjz3rn1wpg61s9a")))
(define-public linux-libre-5.15-pristine-source
  (let ((version linux-libre-5.15-version)
        (hash (base32 "1s4fqm83api3xk0b443b4bhgrx7bx6n8bchdpmzahqadk9i7yvyh")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-5.15)))

(define-public linux-libre-5.10-version "5.10.237")
(define-public linux-libre-5.10-gnu-revision "gnu1")
(define deblob-scripts-5.10
  (linux-libre-deblob-scripts
   linux-libre-5.10-version
   linux-libre-5.10-gnu-revision
   (base32 "1lg0sxfjn0hpcdfyqy2r5kchnjfyyx2xraxhmjhwwkpiwl9ph501")
   (base32 "11liynz5vayms646gj0rjj8fmrqhv203mwsqs3sx9p8y84v50d4p")))
(define-public linux-libre-5.10-pristine-source
  (let ((version linux-libre-5.10-version)
        (hash (base32 "098gvqfaahabqqz64m5fwri57drwiz3006pr805sxw74w0vjgj0z")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-5.10)))

(define-public linux-libre-5.4-version "5.4.293")
(define-public linux-libre-5.4-gnu-revision "gnu1")
(define deblob-scripts-5.4
  (linux-libre-deblob-scripts
   linux-libre-5.4-version
   linux-libre-5.4-gnu-revision
   (base32 "168ysrpcbfhm6s71l9w8ibjq2frjmiaixc2wbsq86gx2zw6zyb5p")
   (base32 "0l7pwhgw9laxfypcpqlz411x3hybcw2269abh3lpcw96bgv5m1k2")))
(define-public linux-libre-5.4-pristine-source
  (let ((version linux-libre-5.4-version)
        (hash (base32 "0b9p8l6ndm75751f7s03rnxg7yg9c4pj9rb537lhsv6pqx096n1l")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-5.4)))

(define %boot-logo-patch
  ;; Linux-Libre boot logo featuring Freedo and a gnu.
  (origin
    (method url-fetch)
    (uri (string-append "http://www.fsfla.org/svn/fsfla/software/linux-libre/"
                        "lemote/gnewsense/branches/3.16/100gnu+freedo.patch"))
    (sha256
     (base32
      "1hk9swxxc80bmn2zd2qr5ccrjrk28xkypwhl4z0qx4hbivj7qm06"))))

(define %linux-libre-arm-export-__sync_icache_dcache-patch
  (origin
    (method url-fetch)
    (uri (string-append
          "https://salsa.debian.org/kernel-team/linux"
          "/raw/34a7d9011fcfcfa38b68282fd2b1a8797e6834f0"
          "/debian/patches/bugfix/arm/"
          "arm-mm-export-__sync_icache_dcache-for-xen-privcmd.patch"))
    (file-name "linux-libre-arm-export-__sync_icache_dcache.patch")
    (sha256
     (base32 "1ifnfhpakzffn4b8n7x7w5cps9mzjxlkcfz9zqak2vaw8nzvl39f"))))

(define (source-with-patches source patches)
  (origin
    (inherit source)
    (patches (append (origin-patches source)
                     patches))))

(define-public linux-libre-6.14-source
  (source-with-patches linux-libre-6.14-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch)))

(define-public linux-libre-6.12-source
  (source-with-patches linux-libre-6.12-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch)))

(define-public linux-libre-6.6-source
  (source-with-patches linux-libre-6.6-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch)))

(define-public linux-libre-6.1-source
  (source-with-patches linux-libre-6.1-pristine-source
                       (append
                        (list %boot-logo-patch
                              %linux-libre-arm-export-__sync_icache_dcache-patch)
                        (search-patches
                         "linux-libre-infodocs-target.patch"))))

(define-public linux-libre-5.15-source
  (source-with-patches linux-libre-5.15-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch)))

(define-public linux-libre-5.10-source
  (source-with-patches linux-libre-5.10-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch)))

(define-public linux-libre-5.4-source
  (source-with-patches linux-libre-5.4-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch
                             ;; Pinebook Pro patch from linux-next,
                             ;; can be dropped for linux-libre 5.7
                             (search-patch
                              "linux-libre-support-for-Pinebook-Pro.patch"))))


;;;
;;; Kernel headers.
;;;

(define %linux-libre-timeout-properties
  ;; Package properties for 'linux-libre' and 'linux-libre-headers' packages.
  `((timeout . ,(* 8 3600))                       ;deblob takes >5h on AArch64
    (max-silent-time . ,(* 3 3600))))             ;don't time out on blob scan

(define (make-linux-libre-headers version gnu-revision hash-string)
  (make-linux-libre-headers* version gnu-revision
                             (origin
                               (method url-fetch)
                               (uri (linux-libre-urls version gnu-revision))
                               (sha256 (base32 hash-string)))))

(define (make-linux-libre-headers* version gnu-revision source)
  (package
    (name "linux-libre-headers")
    (version version)
    (source source)
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)
                     ,@(if (version>=? version "4.16")
                           `(("flex" ,flex)
                             ("bison" ,bison))
                           '())))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (let ((arch ,(platform-linux-architecture
                           (lookup-platform-by-target-or-system
                            (or (%current-target-system)
                                (%current-system)))))
                   (defconfig ,(system->defconfig
                                (or (%current-target-system)
                                    (%current-system))))
                   (make-target ,(if (version>=? version "5.3")
                                     "headers"
                                     "headers_check")))
               (setenv "ARCH" arch)
               (format #t "`ARCH' set to `~a'~%" (getenv "ARCH"))
               (invoke "make" defconfig)
               (invoke "make" "mrproper" make-target))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))

               ;; Mimic the quiet_cmd_headers_install target to avoid a
               ;; dependency on rsync.
               (for-each (lambda (file)
                           (let ((destination (string-append
                                               out "/include/"
                                               ;; Drop the 'usr/include' prefix.
                                               (match (string-split file #\/)
                                                 ((_ _ path ...)
                                                  (string-join path "/"))))))
                             (format #t "`~a' -> `~a'~%" file destination)
                             (install-file file (dirname destination))))
                         (find-files "usr/include" "\\.h$"))

               (mkdir (string-append out "/include/config"))
               (call-with-output-file
                   (string-append out
                                  "/include/config/kernel.release")
                 (lambda (p)
                   (format p "~a-default~%" ,version)))))))
       #:allowed-references ()
       #:tests? #f))
    (supported-systems (remove target-hurd? %supported-systems))
    (properties %linux-libre-timeout-properties)
    (home-page "https://www.gnu.org/software/linux-libre/")
    (synopsis "GNU Linux-Libre kernel headers")
    (description "Headers of the Linux-Libre kernel.")
    (license license:gpl2)))

(define-public linux-libre-headers-6.14
  (make-linux-libre-headers* linux-libre-6.14-version
                             linux-libre-6.14-gnu-revision
                             linux-libre-6.14-source))

(define-public linux-libre-headers-6.12
  (make-linux-libre-headers* linux-libre-6.12-version
                             linux-libre-6.12-gnu-revision
                             linux-libre-6.12-source))

(define-public linux-libre-headers-6.6
  (make-linux-libre-headers* linux-libre-6.6-version
                             linux-libre-6.6-gnu-revision
                             linux-libre-6.6-source))

(define-public linux-libre-headers-6.1
  (make-linux-libre-headers* linux-libre-6.1-version
                             linux-libre-6.1-gnu-revision
                             linux-libre-6.1-source))

(define-public linux-libre-headers-5.15
  (make-linux-libre-headers* linux-libre-5.15-version
                             linux-libre-5.15-gnu-revision
                             linux-libre-5.15-source))

(define-public linux-libre-headers-5.10
  (make-linux-libre-headers* linux-libre-5.10-version
                             linux-libre-5.10-gnu-revision
                             linux-libre-5.10-source))

(define-public linux-libre-headers-5.4
  (make-linux-libre-headers* linux-libre-5.4-version
                             linux-libre-5.4-gnu-revision
                             linux-libre-5.4-source))

;; The following package is used in the early bootstrap, and thus must be kept
;; stable and with minimal build requirements.
(define-public linux-libre-headers-5.15.49
  (make-linux-libre-headers "5.15.49" "gnu"
                            "13zqdcm4664vh7g57sxbfrlpsxm7zrma72mxdfdz7d9yndy2gfv8"))

;; linux 5.19 include loongarch support.
(define-public linux-libre-headers-5.19.17
  (make-linux-libre-headers "5.19.17" "gnu"
                            "0m1yabfvaanbzv0ip04r4kvs16aq0pp2dk93xzi5cq18i3vw351m"))

(define-public linux-libre-headers linux-libre-headers-5.15.49)
;; linux-libre-headers-latest points to the latest headers package
;; and should be used as a dependency for packages that depend on
;; the headers.
(define-public linux-libre-headers-latest linux-libre-headers-6.14)


;;;
;;; Kernel configurations.
;;;

(define* (kernel-config arch #:key variant)
  "Return a file-like object of the Linux-Libre build configuration file for
ARCH and optionally VARIANT, or #f if there is no such configuration."
  (let* ((name (string-append (if variant (string-append variant "-") "")
                              (if (string=? "i386" arch) "i686" arch) ".conf"))
         (file (string-append "linux-libre/" name))
         (config (search-auxiliary-file file)))
    (and config (local-file config))))

(define (default-extra-linux-options version)
  `(;; Make the kernel config available at /proc/config.gz
    ("CONFIG_IKCONFIG" . #t)
    ("CONFIG_IKCONFIG_PROC" . #t)
    ;; Debugging options.
    ("CONFIG_DEBUG_INFO" . #t)          ;required by BTF
    ,@(if (version>=? version "5.1")
          '(("CONFIG_DEBUG_INFO_BTF" . #t))
          '())
    ,@(if (version>=? version "5.12")
          '(("CONFIG_DEBUG_INFO_DWARF_TOOLCHAIN_DEFAULT" . #t))
          '())
    ("CONFIG_DEBUG_INFO_REDUCED" . #f)  ;incompatible with BTF
    ;; Tracing and related options.
    ,@(if (version>=? version "5.1")
          '(("CONFIG_BPF_JIT" . #t)
            ("CONFIG_BPF_JIT_ALWAYS_ON" . #t)
            ("CONFIG_BPF_SYSCALL" . #t))
          '())
    ,@(if (version>=? version "5.13")
          '(("CONFIG_BPF_UNPRIV_DEFAULT_OFF" . #t))
          '())
    ("CONFIG_NET_CLS_BPF" . m)         ;classify packets based on BPF filters
    ("CONFIG_NET_ACT_BPF" . m)         ;to execute BPF code on packets
    ;; Compress kernel modules via Zstd.
    ,(if (version>=? version "5.13")
         '("CONFIG_MODULE_COMPRESS_ZSTD" . #t)
         '("CONFIG_MODULE_COMPRESS_GZIP" . #t))
    ;; Some very mild hardening.
    ("CONFIG_SECURITY_DMESG_RESTRICT" . #t)
    ;; All kernels should have NAMESPACES options enabled
    ("CONFIG_NAMESPACES" . #t)
    ("CONFIG_UTS_NS" . #t)
    ("CONFIG_IPC_NS" . #t)
    ("CONFIG_USER_NS" . #t)
    ("CONFIG_PID_NS" . #t)
    ("CONFIG_NET_NS" . #t)
    ;; Various options needed for elogind service:
    ;; https://issues.guix.gnu.org/43078
    ("CONFIG_CGROUP_FREEZER" . #t)
    ("CONFIG_BLK_CGROUP" . #t)
    ("CONFIG_CGROUP_WRITEBACK" . #t)
    ("CONFIG_CGROUP_SCHED" . #t)
    ("CONFIG_CGROUP_PIDS" . #t)
    ("CONFIG_CGROUP_FREEZER" . #t)
    ("CONFIG_CGROUP_DEVICE" . #t)
    ("CONFIG_CGROUP_CPUACCT" . #t)
    ("CONFIG_CGROUP_PERF" . #t)
    ("CONFIG_SOCK_CGROUP_DATA" . #t)
    ("CONFIG_BLK_CGROUP_IOCOST" . #t)
    ("CONFIG_CGROUP_NET_PRIO" . #t)
    ("CONFIG_CGROUP_NET_CLASSID" . #t)
    ("CONFIG_MEMCG" . #t)
    ("CONFIG_MEMCG_SWAP" . #t)
    ("CONFIG_MEMCG_KMEM" . #t)
    ("CONFIG_CPUSETS" . #t)
    ("CONFIG_PROC_PID_CPUSET" . #t)
    ;; Allow disk encryption by default
    ("CONFIG_DM_CRYPT" . m)
    ;; Allow fscrypt filesystem encryption by default
    ("CONFIG_FS_ENCRYPTION" . #t)
    ;; Support zram on all kernel configs
    ("CONFIG_ZSWAP" . #t)
    ("CONFIG_ZSMALLOC" . #t)
    ("CONFIG_ZRAM" . m)
    ;; Accessibility support.
    ("CONFIG_ACCESSIBILITY" . #t)
    ("CONFIG_A11Y_BRAILLE_CONSOLE" . #t)
    ("CONFIG_SPEAKUP" . m)
    ("CONFIG_SPEAKUP_SYNTH_SOFT" . m)
    ;; Modules required for initrd:
    ("CONFIG_NET_9P" . m)
    ("CONFIG_NET_9P_VIRTIO" . m)
    ("CONFIG_VIRTIO_BLK" . m)
    ("CONFIG_VIRTIO_NET" . m)
    ("CONFIG_VIRTIO_PCI" . m)
    ("CONFIG_VIRTIO_BALLOON" . m)
    ("CONFIG_VIRTIO_MMIO" . m)
    ("CONFIG_FUSE_FS" . m)
    ("CONFIG_CIFS" . m)
    ("CONFIG_9P_FS" . m)
    ;; Disable the EFI pstore storage backend to avoid causing
    ;; unrecoverable failures on some EFI systems:
    ;; https://lists.gnu.org/archive/html/help-guix/2025-01/msg00173.html
    ("CONFIG_EFI_VARS_PSTORE_DEFAULT_DISABLE" . #t)))

(define (config->string options)
  (string-join (map (match-lambda
                      ((option . 'm)
                       (string-append option "=m"))
                      ((option . #t)
                       (string-append option "=y"))
                      ((option . #f)
                       (string-append option "=n"))
                      ((option . string)
                       (string-append option "=\"" string "\"")))
                    options)
               "\n"))


;;;
;;; Kernel package utilities.
;;;

(define (apply-infodoc-patch? version)
  ;; Versions older than 5.10 have different enough build scripts that the
  ;; infodocs patch doesn't apply.
  (and (version>=? version "5.10")
       (not (version>=? version "6.2")))) ;patch applied upstream


(define* (make-linux-libre version gnu-revision hash-string supported-systems
                           #:key
                           (extra-version #f)
                           ;; A function that takes an arch and a variant, and
                           ;; return a file-like object.  See kernel-config
                           ;; for an example.
                           (configuration-file #f)
                           (defconfig "defconfig")
                           (extra-options (default-extra-linux-options version))
                           (patches
                            `(,%boot-logo-patch
                              ,@(if (apply-infodoc-patch? version)
                                    (list (search-patch
                                           "linux-libre-infodocs-target.patch"))
                                    '()))))
  (make-linux-libre* version gnu-revision
                     (origin
                       (method url-fetch)
                       (uri (linux-libre-urls version gnu-revision))
                       (sha256 (base32 hash-string))
                       (patches patches))
                     supported-systems
                     #:extra-version extra-version
                     #:configuration-file configuration-file
                     #:defconfig defconfig
                     #:extra-options extra-options))

(define* (make-linux-libre* version gnu-revision source supported-systems
                            #:key
                            (extra-version #f)
                            ;; A function that takes an arch and a variant.
                            ;; See kernel-config for an example.
                            (configuration-file #f)
                            (defconfig "defconfig")
                            (extra-options (default-extra-linux-options version)))
  (package
    (name (if extra-version
              (string-append "linux-libre-" extra-version)
              "linux-libre"))
    (version version)
    (source source)
    (supported-systems supported-systems)
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 ftw)
                  (ice-9 match))
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-/bin/pwd
            (lambda _
              (substitute* (find-files
                            "." "^Makefile(\\.include)?$")
                (("/bin/pwd") "pwd"))))
          (add-before 'configure 'set-environment
            (lambda* (#:key target #:allow-other-keys)
              ;; Avoid introducing timestamps.
              (setenv "KCONFIG_NOTIMESTAMP" "1")
              (setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))

              ;; Other variables useful for reproducibility.
              (setenv "KBUILD_BUILD_VERSION" "1")
              (setenv "KBUILD_BUILD_USER" "guix")
              (setenv "KBUILD_BUILD_HOST" "guix")

              ;; Set ARCH and CROSS_COMPILE.
              (let ((arch #$(platform-linux-architecture
                             (lookup-platform-by-target-or-system
                              (or (%current-target-system)
                                  (%current-system))))))
                (setenv "ARCH" arch)
                (format #t "`ARCH' set to `~a'~%" (getenv "ARCH"))
                (when target
                  (setenv "CROSS_COMPILE" (string-append target "-"))
                  (format #t "`CROSS_COMPILE' set to `~a'~%"
                          (getenv "CROSS_COMPILE"))))

              ;; Allow EXTRAVERSION to be set via the environment.
              (substitute* "Makefile"
                (("^ *EXTRAVERSION[[:blank:]]*=")
                 "EXTRAVERSION ?="))
              (setenv "EXTRAVERSION"
                      #$(and extra-version
                             (string-append "-" extra-version)))
              ;; Use the maximum compression available for Zstd-compressed
              ;; modules.
              (setenv "ZSTD_CLEVEL" "19")))
          (replace 'configure
            (lambda _
              (let ((config
                     #$(match (let ((arch (platform-linux-architecture
                                           (lookup-platform-by-target-or-system
                                            (or (%current-target-system)
                                                (%current-system))))))
                                (and configuration-file arch
                                     (configuration-file
                                      arch
                                      #:variant (version-major+minor version))))
                         (#f            ;no config for this platform
                          #f)
                         ((? file-like? config)
                          config))))
                ;; Use a custom kernel configuration file or a default
                ;; configuration file.
                (if config
                    (begin
                      (copy-file config ".config")
                      (chmod ".config" #o666))
                    (invoke "make" #$defconfig))
                ;; Appending works even when the option wasn't in the file.
                ;; The last one prevails if duplicated.
                (let ((port (open-file ".config" "a"))
                      (extra-configuration #$(config->string extra-options)))
                  (display extra-configuration port)
                  (close-port port))
                (invoke "make" "oldconfig"))))
          (replace 'install
            (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
              (let ((moddir (string-append #$output "/lib/modules"))
                    (dtbdir (string-append #$output "/lib/dtbs"))
                    (make-flags
                     (append make-flags
                             (list "-j"
                                   (if parallel-build?
                                       (number->string (parallel-job-count))
                                       "1")))))
                ;; Install kernel image, kernel configuration and link map.
                (for-each (lambda (file) (install-file file #$output))
                          (find-files "." "^(\\.config|bzImage|zImage|Image\
|vmlinuz|System\\.map|Module\\.symvers)$"))
                ;; Install device tree files
                (unless (null? (find-files "." "\\.dtb$"))
                  (mkdir-p dtbdir)
                  (apply invoke "make"
                         (string-append "INSTALL_DTBS_PATH=" dtbdir)
                         "dtbs_install" make-flags))
                ;; Install kernel modules
                (mkdir-p moddir)
                (apply invoke "make"
                       ;; Disable depmod because the Guix system's module
                       ;; directory is an union of potentially multiple
                       ;; packages.  It is not possible to use depmod to
                       ;; usefully calculate a dependency graph while building
                       ;; only one of them.
                       "DEPMOD=true"
                       (string-append "MODULE_DIR=" moddir)
                       (string-append "INSTALL_PATH=" #$output)
                       (string-append "INSTALL_MOD_PATH=" #$output)
                       "INSTALL_MOD_STRIP=1"
                       "modules_install" make-flags)
                (let* ((versions (filter (lambda (name)
                                           (not (string-prefix? "." name)))
                                         (scandir moddir)))
                       (version (match versions
                                  ((x) x))))
                  ;; There are symlinks to the build and source directory.
                  ;; Both will point to target /tmp/guix-build* and thus not
                  ;; be useful in a profile.  Delete the symlinks.
                  (false-if-file-not-found
                   (delete-file
                    (string-append moddir "/" version "/build")))
                  (false-if-file-not-found
                   (delete-file
                    (string-append moddir "/" version "/source"))))))))))
    (native-inputs
     (list perl
           bc
           openssl
           elfutils                  ;needed to enable CONFIG_STACK_VALIDATION
           flex
           bison
           util-linux          ;needed for hexdump
           ;; These are needed to compile the GCC plugins.
           gmp
           mpfr
           mpc
           ;; These are needed when building with the CONFIG_DEBUG_INFO_BTF
           ;; support.
           dwarves                      ;for pahole
           python-wrapper
           zlib
           ;; For Zstd compression of kernel modules.
           zstd))
    (home-page "https://www.gnu.org/software/linux-libre/")
    (synopsis "100% free redistribution of a cleaned Linux kernel")
    (description "GNU Linux-Libre is a free (as in freedom) variant of the
Linux kernel.  It has been modified to remove all non-free binary blobs.")
    (license license:gpl2)
    (properties %linux-libre-timeout-properties)))


;;;
;;; Generic kernel packages.
;;;

(define-public linux-libre-6.14
  (make-linux-libre* linux-libre-6.14-version
                     linux-libre-6.14-gnu-revision
                     linux-libre-6.14-source
                     '("x86_64-linux" "i686-linux" "armhf-linux"
                       "aarch64-linux" "powerpc64le-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-version         linux-libre-6.14-version)
(define-public linux-libre-gnu-revision    linux-libre-6.14-gnu-revision)
(define-public linux-libre-pristine-source linux-libre-6.14-pristine-source)
(define-public linux-libre-source          linux-libre-6.14-source)
(define-public linux-libre                 linux-libre-6.14)

(define-public linux-libre-6.12
  (make-linux-libre* linux-libre-6.12-version
                     linux-libre-6.12-gnu-revision
                     linux-libre-6.12-source
                     '("x86_64-linux" "i686-linux" "armhf-linux"
                       "aarch64-linux" "powerpc64le-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-6.6
  (make-linux-libre* linux-libre-6.6-version
                     linux-libre-6.6-gnu-revision
                     linux-libre-6.6-source
                     '("x86_64-linux" "i686-linux" "armhf-linux"
                       "aarch64-linux" "powerpc64le-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-6.1
  (make-linux-libre* linux-libre-6.1-version
                     linux-libre-6.1-gnu-revision
                     linux-libre-6.1-source
                     '("x86_64-linux" "i686-linux" "armhf-linux"
                       "aarch64-linux" "powerpc64le-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-5.15
  (make-linux-libre* linux-libre-5.15-version
                     linux-libre-5.15-gnu-revision
                     linux-libre-5.15-source
                     '("x86_64-linux" "i686-linux" "armhf-linux"
                       "aarch64-linux" "powerpc64le-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-5.10
  (make-linux-libre* linux-libre-5.10-version
                     linux-libre-5.10-gnu-revision
                     linux-libre-5.10-source
                     '("x86_64-linux" "i686-linux" "armhf-linux"
                       "aarch64-linux" "powerpc64le-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

(define-public linux-libre-5.4
  (make-linux-libre* linux-libre-5.4-version
                     linux-libre-5.4-gnu-revision
                     linux-libre-5.4-source
                     '("x86_64-linux" "i686-linux" "armhf-linux"
                       "aarch64-linux" "powerpc64le-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

;; Linux-Libre-LTS points to the *newest* released long-term support version of
;; Linux-Libre.
;; Reference: <https://www.kernel.org/category/releases.html>

(define-public linux-libre-lts-version         linux-libre-6.12-version)
(define-public linux-libre-lts-gnu-revision    linux-libre-6.12-gnu-revision)
(define-public linux-libre-lts-pristine-source linux-libre-6.12-pristine-source)
(define-public linux-libre-lts-source          linux-libre-6.12-source)
(define-public linux-libre-lts                 linux-libre-6.12)


;;;
;;; Specialized kernel variants.
;;;

(define-public linux-libre-arm-generic
  (make-linux-libre* linux-libre-version
                     linux-libre-gnu-revision
                     linux-libre-source
                     '("armhf-linux")
                     #:defconfig "multi_v7_defconfig"
                     #:extra-version "arm-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t))
                      (default-extra-linux-options linux-libre-version))))

(define-public linux-libre-arm-generic-5.10
  (make-linux-libre* linux-libre-5.10-version
                     linux-libre-5.10-gnu-revision
                     linux-libre-5.10-source
                     '("armhf-linux")
                     #:defconfig "multi_v7_defconfig"
                     #:extra-version "arm-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t))
                      (default-extra-linux-options linux-libre-5.10-version))))

(define-public linux-libre-arm-generic-5.4
  (make-linux-libre* linux-libre-5.4-version
                     linux-libre-5.4-gnu-revision
                     linux-libre-5.4-source
                     '("armhf-linux")
                     #:defconfig "multi_v7_defconfig"
                     #:extra-version "arm-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t))
                      (default-extra-linux-options linux-libre-5.4-version))))

(define-public linux-libre-arm-omap2plus
  (make-linux-libre* linux-libre-version
                     linux-libre-gnu-revision
                     linux-libre-source
                     '("armhf-linux")
                     #:defconfig "omap2plus_defconfig"
                     #:extra-version "arm-omap2plus"))

(define-public linux-libre-arm64-generic
  (make-linux-libre* linux-libre-version
                     linux-libre-gnu-revision
                     linux-libre-source
                     '("aarch64-linux")
                     #:defconfig "defconfig"
                     #:extra-version "arm64-generic"
                     #:extra-options
                     (append
                      `(;; Provide support for ath9k wireless
                        ("CONFIG_ATH9K_HTC" . m)
                        ;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t)
                        ;; Pinebook display, battery, charger and usb
                        ("CONFIG_DRM_ANALOGIX_ANX6345" . m)
                        ("CONFIG_CHARGER_AXP20X" . m)
                        ("CONFIG_INPUT_AXP20X_PEK" . m)
                        ("CONFIG_CHARGER_AXP20X" . m)
                        ("CONFIG_BATTERY_AXP20X" . m)
                        ("CONFIG_PINCTRL_AXP209" . m)
                        ("CONFIG_AXP20X_POWER" . m)
                        ("CONFIG_AXP20X_ADC" . m)
                        ;; Pinebook PRO battery and sound support
                        ("CONFIG_BATTERY_CW2015" . m)
                        ("CONFIG_CHARGER_GPIO" . m)
                        ("CONFIG_SND_SOC_ES8316" . m))
                      (default-extra-linux-options linux-libre-version))))

(define-public linux-libre-lts-arm64-generic
  (if (string=? linux-libre-lts-version linux-libre-version)

      ;; Make it a strict alias so that the user interface does not see two
      ;; packages with the exact same name and version.
      linux-libre-arm64-generic

      (make-linux-libre* linux-libre-lts-version
                         linux-libre-lts-gnu-revision
                         linux-libre-lts-source
                         '("aarch64-linux")
                         #:defconfig "defconfig"
                         #:extra-version "arm64-generic"
                         #:extra-options
                         (append
                          `( ;; Provide support for ath9k wireless
                            ("CONFIG_ATH9K_HTC" . m)
                            ;; needed to fix the RTC on rockchip platforms
                            ("CONFIG_RTC_DRV_RK808" . #t)
                            ;; Pinebook display, battery, charger and usb
                            ("CONFIG_DRM_ANALOGIX_ANX6345" . m)
                            ("CONFIG_CHARGER_AXP20X" . m)
                            ("CONFIG_INPUT_AXP20X_PEK" . m)
                            ("CONFIG_CHARGER_AXP20X" . m)
                            ("CONFIG_BATTERY_AXP20X" . m)
                            ("CONFIG_PINCTRL_AXP209" . m)
                            ("CONFIG_AXP20X_POWER" . m)
                            ("CONFIG_AXP20X_ADC" . m)
                            ;; Pinebook PRO battery and sound support
                            ("CONFIG_BATTERY_CW2015" . m)
                            ("CONFIG_CHARGER_GPIO" . m)
                            ("CONFIG_SND_SOC_ES8316" . m))
                          (default-extra-linux-options linux-libre-lts-version)))))

(define-public linux-libre-arm64-generic-5.10
  (make-linux-libre* linux-libre-5.10-version
                     linux-libre-5.10-gnu-revision
                     linux-libre-5.10-source
                     '("aarch64-linux")
                     #:defconfig "defconfig"
                     #:extra-version "arm64-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t)
                        ;; Pinebook display, battery, charger and usb
                        ("CONFIG_DRM_ANALOGIX_ANX6345" . m)
                        ("CONFIG_CHARGER_AXP20X" . m)
                        ("CONFIG_INPUT_AXP20X_PEK" . m)
                        ("CONFIG_CHARGER_AXP20X" . m)
                        ("CONFIG_BATTERY_AXP20X" . m)
                        ("CONFIG_PINCTRL_AXP209" . m)
                        ("CONFIG_AXP20X_POWER" . m)
                        ("CONFIG_AXP20X_ADC" . m)
                        ;; Pinebook PRO battery and sound support
                        ("CONFIG_BATTERY_CW2015" . m)
                        ("CONFIG_CHARGER_GPIO" . m)
                        ("CONFIG_SND_SOC_ES8316" . m))
                      (default-extra-linux-options linux-libre-5.10-version))))

(define-public linux-libre-arm64-generic-5.4
  (make-linux-libre* linux-libre-5.4-version
                     linux-libre-5.4-gnu-revision
                     linux-libre-5.4-source
                     '("aarch64-linux")
                     #:defconfig "defconfig"
                     #:extra-version "arm64-generic"
                     #:extra-options
                     (append
                      `(;; needed to fix the RTC on rockchip platforms
                        ("CONFIG_RTC_DRV_RK808" . #t))
                      (default-extra-linux-options linux-libre-5.4-version))))

(define-public linux-libre-arm64-honeycomb
  ;; Kernel for use on the HoneyComb LX2 boards:
  ;; <https://shop.solid-run.com/product/SRCFTXE000IV13/>.
  (make-linux-libre* linux-libre-lts-version
                     linux-libre-lts-gnu-revision
                     linux-libre-lts-source
                     '("aarch64-linux")
                     #:extra-version "arm64-honeycomb"
                     #:extra-options
                     ;; See
                     ;; https://github.com/SolidRun/lx2160a_build/blob/master/configs/linux/lx2k_additions.config
                     (append
                      `(("CONFIG_GPIO_SYSFS" . #true)
                        ("CONFIG_GPIO_MPC8XXX" . #true)
                        ("CONFIG_NET_PKTGEN" . #true)
                        ("CONFIG_USB_SERIAL" . #true)
                        ("CONFIG_USB_SERIAL_CONSOLE" . #true)
                        ("CONFIG_USB_SERIAL_GENERIC" . #true)
                        ("CONFIG_USB_SERIAL_SIMPLE" . #true)
                        ("CONFIG_USB_SERIAL_FTDI_SIO" . #true)
                        ("CONFIG_USB_ACM" . #true)
                        ("CONFIG_USB_NET_DRIVERS" . #true)
                        ("CONFIG_USB_USBNET" . #true)
                        ("CONFIG_USB_NET_CDCETHER" . #true)
                        ("CONFIG_USB_NET_CDC_NCM" . #true)
                        ("CONFIG_USB_NET_NET1080" . #true)
                        ("CONFIG_USB_NET_CDC_SUBSET_ENABLE" . #true)
                        ("CONFIG_USB_NET_CDC_SUBSET" . #true)
                        ("CONFIG_USB_ARMLINUX" . #true)
                        ("CONFIG_BLK_DEV_NVME" . #true)
                        ("CONFIG_NVMEM_BCM_OCOTP" . #true)
                        ("CONFIG_DRM_AMDGPU" . #true)
                        ("CONFIG_DRM_AMDGPU_SI" . #true)
                        ("CONFIG_DRM_AMDGPU_CIK" . #true)
                        ("CONFIG_DRM_AMDGPU_USERPTR" . #true)
                        ("CONFIG_DRM_AMD_DC" . #true)
                        ("CONFIG_CHASH" . #true)
                        ("CONFIG_PMBUS" . #true)
                        ("CONFIG_SENSORS_PMBUS" . #true)
                        ("CONFIG_REGULATOR" . #true)
                        ("CONFIG_REGULATOR_FIXED_VOLTAGE" . #true)
                        ("CONFIG_REGULATOR_PWM" . #true)
                        ("CONFIG_SENSORS_AMC6821" . #true)
                        ("CONFIG_SENSORS_LM90" . #true)
                        ("CONFIG_SENSORS_LTC2978" . #true)
                        ("CONFIG_SENSORS_LTC2978_REGULATOR" . #true)
                        ("CONFIG_TMPFS" . #true)
                        ("CONFIG_TMPFS_POSIX_ACL" . #true)
                        ("CONFIG_TMPFS_XATTR" . #true)
                        ;;("CONFIG_BLK_DEV_RAM_SIZE" . 524288)
                        ("CONFIG_POWER_RESET_GPIO" . #true)
                        ("CONFIG_CRYPTO_USER_API_HASH" . #true)
                        ("CONFIG_CRYPTO_USER_API_SKCIPHER" . #true)
                        ("CONFIG_CRYPTO_USER_API_RNG" . #true)
                        ("CONFIG_CRYPTO_USER_API_AEAD" . #true)

                        ;; For connecting to ci.guix.gnu.org over VPN.
                        ("CONFIG_WIREGUARD" . m)

                        ;; restool support
                        ("CONFIG_FSL_MC_UAPI_SUPPORT" . #t)
                        ("CONFIG_FSL_MC_BUS" . #t)
                        ("CONFIG_VFIO_FSL_MC" . #t)
                        ("CONFIG_FSL_MC_DPIO" . #t)
                        ("CONFIG_ARM_GIC_V3_ITS_FSL_MC" . #t)

                        ;; vsockets support
                        ;; TODO This seems to be enabled by default on other
                        ;; architectures?
                        ("CONFIG_VSOCKETS" . m)
                        ("CONFIG_VSOCKETS_DIAG" . m)
                        ("CONFIG_VSOCKETS_LOOPBACK" . m)
                        ("CONFIG_VMWARE_VMCI_VSOCKETS" . m)
                        ("CONFIG_VIRTIO_VSOCKETS" . m)
                        ("CONFIG_VIRTIO_VSOCKETS_COMMON" . m)
                        ("CONFIG_HYPERV_VSOCKETS" . m)
                        ("CONFIG_VHOST_IOTLB" . m)
                        ("CONFIG_VHOST_RING" . m)
                        ("CONFIG_VHOST_TASK" . #true)
                        ("CONFIG_VHOST" . m)
                        ("CONFIG_VHOST_MENU" . #true)
                        ("CONFIG_VHOST_NET" . m)
                        ("CONFIG_VHOST_SCSI" . m)
                        ("CONFIG_VHOST_VSOCK" . m)
                        ("CONFIG_VHOST_VDPA" . m))
                      (default-extra-linux-options linux-libre-lts-version))))

(define-public reform-debian-packages
  (package
    (name "reform-debian-packages")
    (version "2023-07-10-320-gaf0a461") ;from git describe
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://source.mnt.re/reform/reform-debian-packages.git")
             (commit "af0a461d38e13481323f061d9ff6827d1d13873b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cylprld2ri3mprj5cgwla1l8qv9iqqvcra7h64yarzs49zaa16m"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("linux/patches6.12/" "/patches")
          ("linux/" "/dts/amlogic"
           #:include-regexp ("meson.*\\.dts$"))
          ("linux/" "/dts/freescale"
           #:include-regexp ("imx8.*\\.dts$"))
          ("linux/" "/dts/freescale"
           #:include-regexp ("fsl.*\\.dts$"))
          ("linux/" "/dts/rockchip"
           #:include-regexp ("rk3588.*\\.dts$"))
          ("linux/config" "config"))))
    (home-page "https://source.mnt.re/reform/reform-debian-packages")
    (synopsis
     "Linux kernel patches and device-trees used for MNT Reform systems")
    (description
     "Linux kernel patches and device-trees used for the MNT Reform systems")
    (license (list
              (license:fsf-free "file://filter-output"
                                "https://www.gnu.org/prep/maintain/html_node/License-Notices-for-Other-Files.html")
              license:bsd-2
              license:expat
              license:gpl2
              license:gpl2+
              license:gpl3
              license:x11))))

(define-public linux-libre-arm64-mnt-reform
  ;; Kernel for use on the MNT/Reform systems
  ;; https://mntre.com/reform.html
  (let ((base (make-linux-libre* linux-libre-6.12-version
                                 linux-libre-6.12-gnu-revision
                                 linux-libre-6.12-source
                                 '("aarch64-linux")
                                 #:extra-version "arm64-mnt-reform"
                                 #:extra-options
                                 ;; https://source.mnt.re/reform/reform-debian-packages/-/blob/main/linux/config
                                 (append `(("CONFIG_DRM_LVDS_CODEC" . m)
                                           ("CONFIG_DRM_CDNS_MHDP8546" . m)
                                           ("CONFIG_DRM_CDNS_HDMI_CEC" . m)
                                           ("CONFIG_DRM_IMX_CDNS_MHDP" . m)
                                           ("CONFIG_DRM_IMX_DCSS" . m)
                                           ("CONFIG_PHY_FSL_IMX8MQ_HDPTX" . m)
                                           ("CONFIG_DRM_PANEL_LVDS" . m)
                                           ("CONFIG_I2C_IMX_LPI2C" . m)
                                           ("CONFIG_I2C_MUX_REG" . m)
                                           ("CONFIG_INTERCONNECT_IMX" . #true)
                                           ("CONFIG_INTERCONNECT_IMX8MQ" . #true)
                                           ("CONFIG_MFD_WM8994" . m)
                                           ("CONFIG_MUX_GPIO" . m)
                                           ("CONFIG_MUX_MMIO" . m)
                                           ("CONFIG_RTC_DRV_PCF8523" . m)
                                           ("CONFIG_USB_EHCI_FSL" . m)
                                           ("CONFIG_NO_HZ_IDLE" . #true)
                                           ("CONFIG_SND_SOC_FSL_MICFIL" . m)
                                           ("CONFIG_SND_IMX_SOC" . m)
                                           ("CONFIG_SND_SOC_FSL_ASOC_CARD" . m)
                                           ("CONFIG_SND_SOC_IMX_AUDMIX" . m)
                                           ("CONFIG_SND_SOC_IMX_HDMI" . m)
                                           ("CONFIG_INPUT_JOYSTICK" . #true)
                                           ("CONFIG_JOYSTICK_XPAD" . m)
                                           ("CONFIG_JOYSTICK_XPAD_FF" . #true)
                                           ("CONFIG_JOYSTICK_XPAD_LEDS" . #true)
                                           ("CONFIG_INTERCONNECT_IMX8MP" . #true)
                                           ("CONFIG_SND_SOC_FSL_ASRC" . #true)
                                           ("CONFIG_DRM_IMX_LCDIF" . #true)
                                           ("CONFIG_DRM_IMX8MP_DW_HDMI_BRIDGE" . #true)
                                           ("CONFIG_DRM_IMX8MP_HDMI_PVI" . #true)
                                           ("CONFIG_IMX8MM_THERMAL" . #true)
                                           ("CONFIG_IMX2_WDT" . #true)
                                           ("CONFIG_DRM_SAMSUNG_DSIM" . #true)
                                           ("CONFIG_PHY_FSL_SAMSUNG_HDMI_PHY" . #true)
                                           ("CONFIG_DRM" . #true)
                                           ("CONFIG_DRM_PANEL_MNT_POCKET_REFORM" . #true)
                                           ("CONFIG_IMX8M_BLK_CTRL" . #true)
                                           ("CONFIG_IMX_GPCV2_PM_DOMAINS" . #true)
                                           ("CONFIG_DRM_DISPLAY_CONNECTOR" . #true)
                                           ("CONFIG_DRM_FSL_LDB" . #true)
                                           ("CONFIG_BACKLIGHT_CLASS_DEVICE" . #true)
                                           ("CONFIG_BACKLIGHT_PWM" . #true)
                                           ("CONFIG_BACKLIGHT_GPIO" . #true)
                                           ("CONFIG_BACKLIGHT_LED" . #true)
                                           ("CONFIG_CPU_FREQ_GOV_PERFORMANCE" . #true)
                                           ("CONFIG_CPU_FREQ_GOV_POWERSAVE" . #true)
                                           ("CONFIG_CPU_FREQ_GOV_USERSPACE" . #true)
                                           ("CONFIG_CPU_FREQ_GOV_ONDEMAND" . #true)
                                           ("CONFIG_CPU_FREQ_GOV_CONSERVATIVE" . #true)
                                           ("CONFIG_CPU_FREQ_GOV_SCHEDUTIL" . #true)
                                           ("CONFIG_ARM_IMX_CPUFREQ_DT" . #true)
                                           ("CONFIG_ARM_IMX_BUS_DEVFREQ" . #true)
                                           ("CONFIG_IMX_IRQSTEER" . #true)
                                           ("CONFIG_PCI_MESON" . #true)
                                           ("CONFIG_DWMAC_MESON" . #true)
                                           ("CONFIG_MDIO_BUS_MUX_MESON_G12A" . #true)
                                           ("CONFIG_I2C_MESON" . #true)
                                           ("CONFIG_PWM_MESON" . #true)
                                           ("CONFIG_USB_DWC3_MESON_G12A" . #true)
                                           ("CONFIG_MMC_MESON_GX" . #true)
                                           ("CONFIG_MMC_MESON_MX_SDIO" . #true)
                                           ("CONFIG_MESON_DDR_PMU" . #true)
                                           ("CONFIG_RTW88_8822CS" . m)
                                           ("CONFIG_PWM_FSL_FTM" . #true)
                                           ("CONFIG_FSL_RCPM" . #true)
                                           ("CONFIG_ARM_ROCKCHIP_CPUFREQ" . m)
                                           ("CONFIG_DRM_PANTHOR" . m)
                                           ("CONFIG_NVMEM_ROCKCHIP_OTP" . #true)
                                           ("CONFIG_PHY_ROCKCHIP_SAMSUNG_HDPTX" . m)
                                           ("CONFIG_PHY_ROCKCHIP_USBDP" . m)
                                           ("CONFIG_ROCKCHIP_INNO_HDMI" . #true)
                                           ("CONFIG_ROCKCHIP_LVDS" . #true)
                                           ("CONFIG_ROCKCHIP_REGULATOR_COUPLER" . #true)
                                           ("CONFIG_SPI_ROCKCHIP" . #true)
                                           ("CONFIG_SPI_ROCKCHIP_SFC" . m)
                                           ("CONFIG_ARM_SCMI_CPUFREQ" . m)
                                           ("CONFIG_PHY_ROCKCHIP_SAMSUNG_DCPHY" . m)
                                           ("CONFIG_VIDEO_ROCKCHIP_VDEC2" . m)
                                           ("CONFIG_ROCKCHIP_DW_HDMI_QP" . #true)
                                           ("CONFIG_ROCKCHIP_DW_MIPI_DSI" . #true)
                                           ;; Provide support for ath9k wireless
                                           ("CONFIG_ATH9K" . m)
                                           ("CONFIG_ATH9K_HTC" . m))
                                         (default-extra-linux-options
                                          linux-libre-6.12-version)))))
    (package
      (inherit base)
      (inputs (list reform-debian-packages))
      (synopsis (string-append (package-synopsis base)
                               " for MNT/Reform systems"))
      (description (string-append (package-description base)
                    "  Configuration options and patches have been applied for use with MNT/Reform systems."))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'apply-reform-patches
                (lambda* (#:key inputs #:allow-other-keys)
                  (for-each (lambda (patch)
                              (invoke "patch" "-p1" "-i"
                                      (search-input-file inputs
                                                         (string-append
                                                          "patches/" patch))))
                            (list
                             "imx8mp-mnt-pocket-reform/2ghz/0001-imx8mp-2ghz-clk.patch"
                             "imx8mp-mnt-pocket-reform/2ghz/0002-imx8mp-2ghz-opp.patch"
                             "imx8mp-mnt-pocket-reform/audio/0000-revert-crashy-audiomix-pm-support.patch"
                             "imx8mp-mnt-pocket-reform/mmc-sdio/0001-sdhci-add-no-sd-uhs-sdr104-devicetree-property.patch"
                             "imx8mp-mnt-pocket-reform/mmc-sdio/0002-During-the-card-init-the-host-side-sometimes-may-nee.patch"
                             "imx8mp-mnt-pocket-reform/mmc-sdio/0003-USDHC-IP-has-one-limitation-the-tuning-circuit-can-t.patch"
                             "imx8mp-mnt-pocket-reform/pocket-panel/0001-v5-add-multi-display-panel-driver.patch"
                             "imx8mp-mnt-reform2/0001-sn65dsi86-use-hs-clock-of-samsung-dsim-host-directly.patch"
                             "imx8mp-mnt-reform2/0002-lcdif-dont-exceed-desired-pixel-clock.patch"
                             "imx8mq-mnt-reform2/0001-nwl-dsi-fixup-mode-only-for-LCDIF-input-not-DCSS.patch"
                             "imx8mq-mnt-reform2/0002-pci-imx6-add-support-for-internal-refclk-imx8mq.patch"
                             "imx8mq-mnt-reform2/0003-lcdif-fix-pcie-interference.patch"
                             "imx8mq-mnt-reform2/0004-mnt4002-imx-gpcv2-wake-smccc.patch.patch"
                             ;; Does not apply, needs further investigation
                             ;; "imx8mq-mnt-reform2/v19_20241126_sandor_yu_initial_support_cadence_mhdp8501_hdmi_dp_for_i_mx8mq.mbx"
                             "ls1028a-mnt-reform2/0000-dtsi-add-hdptx.patch"
                             "meson-g12b-bananapi-cm4-mnt-pocket-reform/0001-a311d-viu-fifo-lines-config.patch"
                             "meson-g12b-bananapi-cm4-mnt-pocket-reform/0002-a311d-viu-fifo-lines-config-header.patch"
                             "meson-g12b-bananapi-cm4-mnt-pocket-reform/0003-tlv320aic31xx-add-1228800hz-support.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0001-Revert-drm-bridge-synopsys-dw-mipi-dsi-enable-EoTp-b.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0011-dw-mipi-dsi-phy-stop-wait-time.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0012-innolux-n125hce-gn1-timing-tweaks.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0013-meson-viu-hold-fifo-lines.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0014-meson-venc-sync.patch.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0015-meson-dw-mipi-dsi-sync-invert.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0016-sn65dsi86-burst-mode-support.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0018-sn65dsi86-never-turn-off.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0020-LOCAL-ALSA-Assign-internal-PCM-chmap-ELD-IEC958-kctl.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0021-HACK-of-partial-revert-of-fdt.c-changes.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0022-add-bt-and-eth-resets.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0023-sdio-pullups.patch"
                             "meson-g12b-bananapi-cm4-mnt-reform2/0024-sdio-improve-wifi-speed.patch"
                             "rk3588-mnt-reform2/0001-regulator-Add-of_regulator_get_optional-for-pure-DT-.patch"
                             "rk3588-mnt-reform2/0001-scripts-package-builddeb-allow-hooks-also-in-usr-sha.patch"
                             "rk3588-mnt-reform2/0002-regulator-Add-devres-version-of-of_regulator_get_opt.patch"
                             "rk3588-mnt-reform2/0004-arm64-dts-rockchip-rk3588-rock5b-add-USB-C-support.patch"
                             "rk3588-mnt-reform2/0005-math.h-add-DIV_ROUND_UP_NO_OVERFLOW.patch"
                             "rk3588-mnt-reform2/0006-clk-divider-Fix-divisor-masking-on-64-bit-platforms.patch"
                             "rk3588-mnt-reform2/0007-clk-composite-replace-open-coded-abs_diff.patch"
                             "rk3588-mnt-reform2/0008-clk-rockchip-support-clocks-registered-late.patch"
                             "rk3588-mnt-reform2/0009-clk-rockchip-rk3588-register-GATE_LINK-later.patch"
                             "rk3588-mnt-reform2/0010-clk-rockchip-expose-rockchip_clk_set_lookup.patch"
                             "rk3588-mnt-reform2/0011-clk-rockchip-implement-linked-gate-clock-support.patch"
                             "rk3588-mnt-reform2/0012-clk-rockchip-rk3588-drop-RK3588_LINKED_CLK.patch"
                             "rk3588-mnt-reform2/0013-arm64-dts-rockchip-rk3588-evb1-add-bluetooth-rfkill.patch"
                             "rk3588-mnt-reform2/0014-arm64-dts-rockchip-rk3588-evb1-improve-PCIe-ethernet.patch"
                             "rk3588-mnt-reform2/0015-arm64-dts-rockchip-Slow-down-EMMC-a-bit-to-keep-IO-s.patch"
                             "rk3588-mnt-reform2/0016-vop2-Add-clock-resets-support.patch"
                             "rk3588-mnt-reform2/0017-arm64-dts-rockchip-Add-VOP-clock-resets-for-rk3588s.patch"
                             "rk3588-mnt-reform2/0018-dt-bindings-display-vop2-Add-VP-clock-resets.patch"
                             "rk3588-mnt-reform2/0019-media-v4l2-ctrls-core-Set-frame_mbs_only_flag-by-def.patch"
                             "rk3588-mnt-reform2/0020-media-rockchip-Move-H264-CABAC-table-to-header-file.patch"
                             "rk3588-mnt-reform2/0021-media-rockchip-Introduce-the-rkvdec2-driver.patch"
                             "rk3588-mnt-reform2/0022-media-dt-bindings-rockchip-Document-RK3588-Video-Dec.patch"
                             "rk3588-mnt-reform2/0023-arm64-dts-rockchip-Add-rkvdec2-Video-Decoder-on-rk35.patch"
                             "rk3588-mnt-reform2/0024-arm64-defconfig-enable-Rockchip-RK3588-video-decoder.patch"
                             "rk3588-mnt-reform2/0025-mfd-rk8xx-Fix-shutdown-handler.patch"
                             "rk3588-mnt-reform2/0026-WIP-phy-phy-rockchip-samsung-hdptx-Add-FRL-EARC-supp.patch"
                             "rk3588-mnt-reform2/0027-TESTING-phy-phy-rockchip-samsung-hdptx-Add-verbose-l.patch"
                             "rk3588-mnt-reform2/0028-WIP-dt-bindings-display-rockchip-drm-Add-optional-cl.patch"
                             "rk3588-mnt-reform2/0029-WIP-drm-rockchip-vop2-Improve-display-modes-handling.patch"
                             "rk3588-mnt-reform2/0030-arm64-dts-rockchip-Add-HDMI0-bridge-to-rk3588.patch"
                             "rk3588-mnt-reform2/0031-arm64-dts-rockchip-Enable-HDMI0-on-rock-5b.patch"
                             "rk3588-mnt-reform2/0032-arm64-dts-rockchip-Enable-HDMI0-on-rk3588-evb1.patch"
                             "rk3588-mnt-reform2/0033-WIP-arm64-dts-rockchip-Enable-HDMI0-PHY-clk-provider.patch"
                             "rk3588-mnt-reform2/0034-WIP-arm64-dts-rockchip-Make-use-of-HDMI0-PHY-PLL-on-.patch"
                             "rk3588-mnt-reform2/0035-WIP-arm64-dts-rockchip-Make-use-of-HDMI0-PHY-PLL-on-.patch"
                             "rk3588-mnt-reform2/0036-dt-bindings-display-bridge-Add-schema-for-Synopsys-D.patch"
                             "rk3588-mnt-reform2/0037-dt-bindings-display-rockchip-Add-schema-for-RK3588-H.patch"
                             "rk3588-mnt-reform2/0038-drm-bridge-synopsys-Add-DW-HDMI-QP-TX-controller-dri.patch"
                             "rk3588-mnt-reform2/0039-drm-rockchip-Add-basic-RK3588-HDMI-output-support.patch"
                             "rk3588-mnt-reform2/0040-arm64-defconfig-Enable-Rockchip-extensions-for-Synop.patch"
                             "rk3588-mnt-reform2/0041-MAINTAINERS-Add-entry-for-Synopsys-DesignWare-HDMI-R.patch"
                             "rk3588-mnt-reform2/0042-dt-bindings-media-Document-bindings-for-HDMI-RX-Cont.patch"
                             "rk3588-mnt-reform2/0043-arm64-dts-rockchip-Add-device-tree-support-for-HDMI-.patch"
                             "rk3588-mnt-reform2/0044-media-platform-synopsys-Add-support-for-HDMI-input-d.patch"
                             "rk3588-mnt-reform2/0045-arm64-defconfig-Enable-Synopsys-HDMI-receiver.patch"
                             "rk3588-mnt-reform2/0046-arm64-dts-rockchip-Enable-HDMI-receiver-on-rock-5b.patch"
                             "rk3588-mnt-reform2/0047-arm64-dts-rockchip-Enable-HDMI-receiver-on-RK3588-EV.patch"
                             "rk3588-mnt-reform2/0048-arm64-defconfig-Enable-default-EDID-for-Synopsys-HDM.patch"
                             "rk3588-mnt-reform2/0049-regulator-Add-devm_-of_regulator_get.patch"
                             "rk3588-mnt-reform2/0050-pmdomain-rockchip-cleanup-mutex-handling-in-rockchip.patch"
                             "rk3588-mnt-reform2/0051-pmdomain-rockchip-forward-rockchip_do_pmu_set_power_.patch"
                             "rk3588-mnt-reform2/0052-pmdomain-rockchip-reduce-indentation-in-rockchip_pd_.patch"
                             "rk3588-mnt-reform2/0053-dt-bindings-power-rockchip-add-regulator-support.patch"
                             "rk3588-mnt-reform2/0054-pmdomain-rockchip-add-regulator-support.patch"
                             "rk3588-mnt-reform2/0055-arm64-dts-rockchip-Add-GPU-power-domain-regulator-de.patch"
                             "rk3588-mnt-reform2/0056-dt-bindings-net-wireless-brcm4329-fmac-add-pci14e4-4.patch"
                             "rk3588-mnt-reform2/0057-dt-bindings-net-wireless-brcm4329-fmac-add-clock-des.patch"
                             "rk3588-mnt-reform2/0058-wifi-brcmfmac-Add-optional-lpo-clock-enable-support.patch"
                             ;; does not apply cleanly due to DEBLOBBING
                             ;; "rk3588-mnt-reform2/0059-wifi-brcmfmac-add-flag-for-random-seed-during-firmwa.patch"
                             "rk3588-mnt-reform2/0060-arm64-dts-rockchip-rk3588-evb1-add-WLAN-controller.patch"
                             "rk3588-mnt-reform2/0061-arm64-dts-rockchip-add-and-enable-gpu-node-for-Radxa.patch"
                             "rk3588-mnt-reform2/0062-arm64-dts-rockchip-Enable-HDMI0-on-rock-5a.patch"
                             "rk3588-mnt-reform2/0100-rk3588-dtsi-add-hdmi1-controller.patch"
                             "rk3588-mnt-reform2/0102-drm-panthor-Actually-suspend-IRQs-in-the-unplug-path.patch"
                             "rk3588-mnt-reform2/3001-display-rockchip-add-schema-for-rk3588-hdmi-tx.patch"
                             "rk3588-mnt-reform2/4000-mnt-rk3588-dual-hdmi-qp-rockchip.patch"
                             "rk3588-mnt-reform2/4001-mnt-rk3588-dual-hdmi-vop2-pll.patch"
                             "rk3588-mnt-reform2/5001-rk3588-dsi2-driver.patch"
                             "rk3588-mnt-reform2/5002-rk3588-dsi-dts-nodes.patch"
                             "rk3588-mnt-reform2/5100-modernize-hdmi1-in-dtsi.patch"
                             "rk3588-mnt-reform2/5110-hdptx-crash-workaround.patch"
                             "rk3588-mnt-reform2/5200-drm-rockchip-Set-dma-mask-to-64-bit.patch"))))
              (add-after 'apply-reform-patches 'copy-reform-dts-files
                (lambda* (#:key inputs #:allow-other-keys)
                  (for-each (lambda (dts)
                              (copy-file (search-input-file inputs
                                                            (string-append
                                                             "/dts/" dts))
                                         (string-append "arch/arm64/boot/dts/"
                                          dts)))
                            (list
                             "amlogic/meson-g12b-bananapi-cm4-mnt-pocket-reform.dts"
                             "amlogic/meson-g12b-bananapi-cm4-mnt-reform2.dts"
                             "freescale/fsl-ls1028a-mnt-reform2.dts"
                             "freescale/imx8mp-mnt-pocket-reform.dts"
                             "freescale/imx8mp-mnt-reform2.dts"
                             "freescale/imx8mq-mnt-reform2-hdmi.dts"
                             "freescale/imx8mq-mnt-reform2.dts"
                             "rockchip/rk3588-mnt-pocket-reform.dts"
                             "rockchip/rk3588-mnt-reform-next.dts"
                             "rockchip/rk3588-mnt-reform2.dts"))))
              (add-after 'apply-reform-patches 'adjust-makefiles-with-new-dtb
                (lambda _
                  (substitute* "arch/arm64/boot/dts/amlogic/Makefile"
                    (("meson-g12b-bananapi-cm4-mnt-reform2.dtb")
                     "meson-g12b-bananapi-cm4-mnt-reform2.dtb
dtb-$(CONFIG_ARCH_MESON) += meson-g12b-bananapi-cm4-mnt-pocket-reform.dtb"))
                  (substitute* "arch/arm64/boot/dts/freescale/Makefile"
                    (("fsl-ls1028a-rdb.dtb")
                     "fsl-ls1028a-rdb.dtb
dtb-$(CONFIG_ARCH_LAYERSCAPE) += fsl-ls1028a-mnt-reform2.dtb"))
                  (substitute* "arch/arm64/boot/dts/freescale/Makefile"
                    (("imx8mq-mnt-reform2.dtb")
                     "imx8mq-mnt-reform2.dtb
dtb-$(CONFIG_ARCH_MXC) += imx8mp-mnt-reform2.dtb
dtb-$(CONFIG_ARCH_MXC) += imx8mp-mnt-pocket-reform.dtb
dtb-$(CONFIG_ARCH_MXC) += imx8mq-mnt-reform2-hdmi.dtb"))
                  (substitute* "arch/arm64/boot/dts/rockchip/Makefile"
                    (("rk3588-rock-5b.dtb")
                     "rk3588-rock-5b.dtb
dtb-$(CONFIG_ARCH_ROCKCHIP) += rk3588-mnt-reform2.dtb
dtb-$(CONFIG_ARCH_ROCKCHIP) += rk3588-mnt-reform-next.dtb
dtb-$(CONFIG_ARCH_ROCKCHIP) += rk3588-mnt-pocket-reform.dtb")))))))))))

(define-public linux-libre-riscv64-generic
  (make-linux-libre* linux-libre-version
                     linux-libre-gnu-revision
                     linux-libre-source
                     '("riscv64-linux")
                     #:extra-version "riscv64-generic"
                     #:extra-options
                     (append
                      ;; required `guix system vm'
                      `(("CONFIG_USB_HID" . m)
                        ("CONFIG_HID_GEMBIRD" . m)
                        ("CONFIG_AHCI_DWC" . m)
                        ("CONFIG_SATA_AHCI" . m)
                        ("CONFIG_CRYPTO_SERPENT" . m)
                        ("CONFIG_CRYPTO_WP512" . m)
                        ("CONFIG_USB_UAS" . m)
                        ("CONFIG_USB_STORAGE" . m)
                        ("CONFIG_HID_GENERIC" . m)
                        ("CONFIG_DRM_CIRRUS_QEMU" . m)
                        ("CONFIG_HW_RANDOM_VIRTIO" . m)
                        ("CONFIG_VIRTIO_CONSOLE" . m)
                        ("CONFIG_CRYPTO_XTS" . m))
                      (default-extra-linux-options linux-libre-version))))

(define-public linux-libre-mips64el-fuloong2e
  (make-linux-libre* linux-libre-version
                     linux-libre-gnu-revision
                     linux-libre-source
                     '("mips64el-linux")
                     #:defconfig "fuloong2e_defconfig"
                     #:extra-version "mips64el-fuloong2e"
                     #:extra-options
                     (append
                      `(("CONFIG_OVERLAY_FS" . m))
                      (default-extra-linux-options linux-libre-version))))

(define-public linux-libre-with-bpf
  (deprecated-package "linux-libre-with-bpf" linux-libre))


;;;
;;; Linux kernel modules.
;;;

(define-public acpi-call-linux-module
  (package
    (name "acpi-call-linux-module")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nix-community/acpi_call")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09ljx0jl987753r2kjqj5kxms95bijw0xn14kf82ryn38ck5c8cf"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:tests? #f                  ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'install 'patch-shebangs-harder
                 ;; The (only) shebangs in examples/ don't justify a reference.
                 ;; However, do substitute a slightly more portable one.
                 (lambda _
                   (substitute* (find-files "examples" ".")
                     (("^(#! *)/[^ ]*/" _ shebang)
                      (string-append shebang "/usr/bin/env ")))))
               (add-after 'install 'install-documentation
                 (lambda _
                   (let* ((doc (string-append #$output "/share/doc/"
                                              #$name "-" #$version)))
                     (for-each (lambda (file)
                                 (let ((target (string-append doc "/" file)))
                                   (mkdir-p (dirname target))
                                   (copy-recursively file target)))
                               (list "README.md" "examples"))))))))
    (home-page "https://github.com/nix-community/acpi_call")
    (synopsis "Linux kernel module to perform ACPI method calls")
    (description
     "This simple Linux kernel module allows calls from user space to any
@acronym{ACPI, Advanced Configuration and Power Interface} method provided by
your computer's firmware, by writing to @file{/proc/acpi/call}.  You can pass
any number of parameters of types @code{ACPI_INTEGER}, @code{ACPI_STRING},
and @code{ACPI_BUFFER}.

It grants direct and undocumented access to your hardware that may cause damage
and should be used with caution, especially on untested models.")
    (license license:gpl3+)))           ; see README.md (no licence headers)

(define-public corefreq
  (package
    (name "corefreq")
    (version "1.98.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cyring/CoreFreq")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mxp5h23y09674syaj5gpdifr53zvgjv7g7hargwg6897883qfln"))))
    ;; The upstream Makefile is now such a proprietary mess that standard builds
    ;; as performed by our linux-module-build-system are more or less impossible
    ;; without heavy patching.  However, we still keep it as the main build
    ;; system because lifting out LINUX-MODULE-BUILDER would be even messier.
    (build-system linux-module-build-system)
    (outputs (list "out" "linux-module"))
    (arguments
     (list #:imported-modules `((guix build gnu-build-system)
                                ,@%linux-module-build-system-modules)
           #:modules `((guix build linux-module-build-system)
                       ((guix build gnu-build-system) #:prefix gnu:)
                       (guix build utils))
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   "OPTIM_LVL=3"
                   (string-append "PREFIX=" #$output))
           #:tests? #f                   ;no test suite
           #:source-directory "../build" ;must contain modules.order
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-Makefile
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((dir (search-input-directory
                               inputs "lib/modules/build")))
                     (substitute* "Makefile"
                       (("^(KERNELREL \\?= ).*" _ =)
                        (string-append = (dirname dir) "\n"))
                       (("^(KERNELDIR \\?= ).*" _ =)
                        (string-append = dir "\n"))
                       (("^([[:space:]]+)(.* modules_install.*)"
                         _ indent command)
                        (string-append indent "@echo skipping " command))))))
               (add-before 'build 'prepare
                 (lambda* (#:key source-directory make-flags #:allow-other-keys)
                   ;; Lazily fix another mismatch between what the kernel module
                   ;; build system expects and what this package provides.
                   (symlink "source/build" source-directory) ;$(BUILD)
                   ;; Set up symbolic links inside $(BUILD), for some reason.
                   (apply invoke "make" "prepare" make-flags)))
               (replace 'build (assoc-ref gnu:%standard-phases 'build))
               (add-before 'install 'resolve-symlink
                 ;; The build system silently fails to install from a symlink.
                 (lambda _
                   (delete-file "../build")
                   (copy-recursively "build" "../build")))
               (add-before 'install 'create-output
                 (lambda _
                   ;; Avoid installing the corefreq binary as …/bin.
                   (mkdir-p (string-append #$output "/bin"))))
               (add-after 'install 'gnu:install
                 (assoc-ref gnu:%standard-phases 'install))
               (add-after 'install 'separate-module
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Adding INSTALL_MOD_PATH= to #:make-flags would still create an
                   ;; empty <out>/lib/modules directory, so just do it all by hand.
                   (let* ((output (assoc-ref outputs "out"))
                          (module (assoc-ref outputs "linux-module")))
                     (mkdir-p module)
                     (rename-file (string-append output "/lib")
                                  (string-append module "/lib")))))
               (add-after 'install 'install-README
                 ;; There is no proper documentation.  Provide something.
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (doc (string-append out "/share/doc/"
                                              #$name "-" #$version)))
                     (install-file "README.md" doc)))))))
    (home-page "https://github.com/cyring/CoreFreq")
    (synopsis
     "Measure performance data & tweak low-level settings on x86-64 CPUs")
    (description
     "CoreFreq is a CPU monitor that reports low-level processor settings and
performance data with notably high precision by using a loadable Linux kernel
module.  Unlike most similar tools, it can be used to modify some settings if
supported by the hardware and at your own risk.  It's designed for 64-bit x86
Intel processors (Atom, Core2, Nehalem, SandyBridge, and newer) and compatible
architectures like AMD@tie{}Zen and Hygon@tie{}Dhyana.

Supported processor features include:
@enumerate
@item time spent in C-states, including C1/C3 Auto- and UnDemotion;
@item core temperatures, voltage, and tweaking thermal limits;
@item core frequencies, ratios, and base clock rate;
@item enabling, disabling, and testing SpeedStep (EIST), Turbo Boost, and
Hyper-Threading or SMT;
@item enabling or disabling data cache prefetching;
@item kernel assembly code to keep as near as possible readings of performance
counters such as the @acronym{TSC, Time Stamp Counter}, @acronym{UCC, Unhalted
Core Cycles}, and @acronym{URC, Unhalted Reference Cycles};
@item the number of instructions per cycle or second (IPS, IPC, and CPI);
@item memory controller geometry and RAM timings;
@item running processes' CPU affinity.
@end enumerate

This package provides the @command{corefreqd} data collection daemon, the
@command{corefreq-cli} client to visualise and control it in real time, and the
@code{corefreqk} kernel module in its own separate output.  Read the included
@file{README.md} before loading it.")
    (supported-systems (list "x86_64-linux"))
    (license license:gpl2)))

(define-public librem-ec-acpi-linux-module
  (package
    (name "librem-ec-acpi-linux-module")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://source.puri.sm/nicole.faerber/librem-ec-acpi-dkms")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m8hamf0550083jnk5q4hv20l8lfiardnkxbib4hhvqghpzzbxl0"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:tests? #f))                ; no test suite
    (home-page "https://source.puri.sm/nicole.faerber/librem-ec-acpi-dkms")
    (synopsis "Linux kernel module to control the Librem Embedded Controller")
    (description
     "This is the Linux kernel @acronym{ACPI, Advanced Configuration and Power
Interface} platform driver for the @acronym{EC, Embedded Controller} firmware
on Purism Librem laptop computers.  It allows user-space control over the
battery charging thresholds, keyboard backlight, fans and thermal monitors,
and the notification, WiFi, and Bluetooth LED.")
    (license license:gpl2)))

(define-public tuxedo-keyboard
  (package
    (name "tuxedo-keyboard")
    (version "4.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/tuxedocomputers/development/packages/tuxedo-drivers.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h5hjwlphld1ijin08qi1hrpsczpw8dqyw49yjprl959j74nwnmm"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:tests? #f))                ; no test suite
    (home-page "https://gitlab.com/tuxedocomputers/development/packages/tuxedo-drivers")
    (synopsis "Linux kernel modules to control keyboard on most Tuxedo computers")
    (description
     "This package provides the @code{tuxedo_keyboard}, @code{tuxedo_io},
@code{clevo_wmi} @acronym{WMI, Windows Management Engine} and the
@code{clevo_acpi} @acronym{ACPI, Advanced Configuration and Power Interface}
kernel modules to control the keyboard on most Tuxedo computers.  Only white
backlight only models are currently not supported.  The @code{tuxedo_io}
module is also needed for the @code{tuxedo-control-center} (short tcc)
package.")
    (license license:gpl3+)))

(define-public evdi
  (package
    (name "evdi")
    (version "1.14.8")                  ;inherited by libevdi
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DisplayLink/evdi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18n8kk4gbbj252a2dqb7mbpscfb437x42flh1fsl4g5c8brczc77"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "module"))))))
    (home-page "https://github.com/DisplayLink/evdi")
    (synopsis
     "@acronym{EVDI, Extensible Virtual Display Interface} Linux kernel module")
    (description
     "The @acronym{EVDI, Extensible Virtual Display Interface} is a Linux kernel
module that enables management of multiple screens, allowing user-space programs
to take control over what happens with the image.  It is essentially a virtual
display for which applications using the @code{libevdi} library can add, remove,
and receive screen updates.

The EVDI driver uses the standard Linux @acronym{DRM, Direct Rendering Manager}.
Its displays can be controlled by standard tools such as @command{xrandr} and
display settings applets in graphical environments")
    (license license:gpl2)))

(define-public libevdi
  (package
    (inherit evdi)
    (name "libevdi")
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "library")))
                        (replace 'install
                          (lambda* _
                            (let* ((lib (string-append #$output "/lib")))
                              (mkdir-p lib)
                              (install-file "libevdi.so" lib)))))))
    (inputs (list libdrm))
    (native-inputs (list pkg-config))
    (synopsis
     "@acronym{EVDI, Extensible Virtual Display Interface} user-space library")
    (description
     "Libevdi is a library that gives applications easy access to
@acronym{EVDI, Extensible Virtual Display Interface} devices provided by the
@code{evdi} driver package.")
    (license license:lgpl2.1)))

(define-public ec
  (package
    (name "ec")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "sbindir="
                                               #$output "/sbin")
                                "INSTALL=install" "STRIPCMD=true")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'enter-subdirectory
                          (lambda _
                            (chdir "tools/power/acpi/tools/ec")))
                        (delete 'configure)) ;no configure script
           #:tests? #f)) ;no tests
    (home-page (package-home-page linux-libre))
    (synopsis
     "Utility for reading or writing @acronym{EC, Embedded Controller} registers")
    (description
     "This utility can read or write specific registers or all the available
registers of the @acronym{EC, Embedded Controller} supported by the
@code{ec_sys} Linux driver.")
    (license license:gpl2)))

(define-public lkrg
  (package
    (name "lkrg")
    (version "0.9.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lkrg-org/lkrg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0k0z9caj48nqjwk3bapgfcdzi1lkizxcjj4r1dvkvwsk38mbk1c4"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:linux linux-libre
           #:tests? #f
           #:make-flags #~(list (string-append "CC="
                                               #$(cc-for-target))
                                (string-append "SYSSRC="
                                               (assoc-ref %build-inputs
                                                "linux-module-builder")
                                               "/lib/modules/build"))))
    (inputs (list bash-minimal))
    (home-page "https://lkrg.org/")
    (synopsis "Linux Kernel Runtime Guard")
    (description
     "This package performs runtime integrity checking of the Linux kernel and
detection of security vulnerability exploits against the kernel.")
    (license license:gpl2)))

(define-public vhba-module
  (package
    (name "vhba-module")
    (version "20211218")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://downloads.sourceforge.net/cdemu/vhba-module/vhba-module-"
             version ".tar.xz"))
       (sha256
        (base32 "1dkprnnya0i8121p7ip4g8cww99drk7fzbwcxx65x02jqk0siibj"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:tests? #f))                ; no test suite
    (home-page "https://cdemu.sourceforge.io/")
    (synopsis "Linux kernel module that emulates SCSI devices")
    (description
     "The @acronym{VHBA, Virtual SCSI Host Bus Adapter} module is the link
between the CDemu user-space daemon and the kernel Linux.  It acts as a
low-level SCSI driver that emulates a virtual SCSI adapter which can have
multiple virtual devices attached to it.  Its typical use with CDEmu is to
emulate optical devices such as DVD and CD-ROM drives.")
    (license license:gpl2+)))

(define-public bbswitch-module
  ;; Use "develop" branch since stable release does not build on Linux >= 5.6.
  ;; See https://github.com/Bumblebee-Project/bbswitch/issues/205.
  (let ((commit "19f60204596a6463b162fc7ca11f4946f5c20cea"))
    (package
      (name "bbswitch-module")
      (version (git-version "0.8" "2" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; Use fork until
                      ;; https://github.com/Bumblebee-Project/bbswitch/pull/219
                      ;; is merged.
                      (url "https://github.com/madchic/bbswitch")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pv41y02c9xg9k1bg4i4ll3w7hxxzdr651i08f092b9q5hr57mqn"))))
      (build-system linux-module-build-system)
      (arguments
       (list #:tests? #f))              ; no test suite
      (home-page "https://github.com/Bumblebee-Project/bbswitch")
      (synopsis "Kernel module that disables discrete Nvidia graphics cards")
      (description "The bbswitch module provides a way to toggle the Nvidia
graphics card on Optimus laptops.")
      (license license:gpl2))))

(define-public bin-graph
  ;; XXX: The upstream does not have tags yet.
  (let ((commit "1dd42e3e8e123e993d6c287967502c8d4b36f9ba")
        (revision "0"))
    (package
      (name "bin-graph")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/8dcc/bin-graph")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wjkl789r7iys3nnyk813gsdxwwy2ryxgxirx5xw02lzk790dywl"))))
      (arguments
       (list #:tests? #f                ; no tests
             #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target))
                     (string-append "PREFIX=" #$output)
                     (string-append "INSTALL_DIR=" #$output "/bin"))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure))))    ; no configure script
      (build-system gnu-build-system)
      (inputs (list libpng))
      (home-page "https://github.com/8dcc/bin-graph")
      (synopsis "Visualize binary files")
      (description
       "@code{bin-graph} provides a simple way of visualizing the different regions
of a binary file.")
      (license license:gpl3))))

(define-public ddcci-driver-linux
  (let ((revision "0")
        (commit "7f8f8e6c221a286d57a643c2909109a54e084eed"))
    (package
      (name "ddcci-driver-linux")
      (version (git-version "0.4.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://gitlab.com/ddcci-driver-linux/ddcci-driver-linux.git")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0sgldghd8n4qjm5nv9dgxjibj1hg4kkk1811wndf3vx3dacsmkl9"))))
      (build-system linux-module-build-system)
      (arguments
       (list #:tests? #f                  ; no tests
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'build
                   (lambda args
                     (for-each
                      (lambda (module)
                        (with-directory-excursion module
                          (apply (assoc-ref %standard-phases 'build) args)))
                      '("ddcci" "ddcci-backlight"))))
                 (replace 'install
                   (lambda args
                     (for-each
                      (lambda (module)
                        (with-directory-excursion module
                          (apply (assoc-ref %standard-phases 'install) args)))
                      '("ddcci" "ddcci-backlight")))))))
      (home-page "https://gitlab.com/ddcci-driver-linux/ddcci-driver-linux")
      (synopsis "Pair of Linux kernel drivers for DDC/CI monitors")
      (description "This package provides two Linux kernel drivers, ddcci and
ddcci-backlight, that allows the control of DDC/CI monitors through the sysfs
interface.  The ddcci module creates a character device for each DDC/CI
monitors in @file{/dev/bus/ddcci/[I²C busnumber]}.  While the ddcci-backlight
module allows the control of the backlight level or luminance property when
supported under @file{/sys/class/backlight/}.")
      (license license:gpl2+))))

(define-public v4l2loopback-linux-module
  (package
    (name "v4l2loopback-linux-module")
    (version "0.14.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/umlaeute/v4l2loopback")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vrf0mndzcpypz5a48kyjqwvvfh17wgganzml4vwiqhl4whq2a8v"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:tests? #f))                ; no test suite
    (home-page "https://github.com/umlaeute/v4l2loopback")
    (synopsis "Linux kernel module to create virtual V4L2 video devices")
    (description
     "This Linux module creates virtual video devices.  @acronym{V4L2, Video
for Linux 2} applications will treat these as ordinary video devices but read
video data generated by another application, instead of a hardware device such
as a capture card.

This lets you apply nifty effects to your Jitsi video, for example, but also
allows some more serious things like adding streaming capabilities to an
application by hooking GStreamer into the loopback device.")
    (license license:gpl2+)))

(define-public xpadneo
  (package
    (name "xpadneo")
    (version "0.9.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/atar-axis/xpadneo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v5akvz14izxk3fav6x4l50gi8xzsjwxjddczc2yy78gni1a7ybs"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:tests? #f                  ; no `check' target
           #:source-directory "hid-xpadneo/src"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'post-install
                 (lambda _
                   (copy-recursively "hid-xpadneo/etc-modprobe.d"
                                     (string-append #$output "/etc/modprobe.d"))
                   ;; udev-service-type takes its rules from /lib rather than
                   ;; /etc, so copy it there instead
                   (copy-recursively "hid-xpadneo/etc-udev-rules.d"
                                     (string-append #$output
                                                    "/lib/udev/rules.d")))))))
    (home-page "https://atar-axis.github.io/xpadneo/")
    (synopsis "Xbox One Wireless Controller driver for the kernel Linux")
    (description
     "This package provides a driver for the XBox One S Wireless controller
and some newer models when connected via Bluetooth.  In addition to the included
Linux kernel module, it also contains a modprobe configuration and udev rules,
which need to be installed separately.")
    (license license:gpl3+)))

(define-public vendor-reset-linux-module
  (let ((commit "4b466e92a2d9f76ce1082cde982c7be0be91e248")
        (revision "0"))
    (package
      (name "vendor-reset-linux-module")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gnif/vendor-reset")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1gaf4j20grng689c9fylcqri3j9ycnhr2bsva2z6qcdqvzl6yxbi"))))
      (build-system linux-module-build-system)
      (arguments
       (list #:tests? #f)) ;no test suite
      (home-page "https://github.com/gnif/vendor-reset")
      (synopsis
       "Kernel module that resets GPUs that are affected by the reset bug")
      (description
       "This package provides a kernel module that is capable of
resetting hardware devices into a state where they can be
re-initialized or passed through into a virtual machine (VFIO).
While it would be great to have these in the kernel as PCI quirks,
some of the reset procedures are very complex and would never be
accepted as a quirk (ie AMD Vega 10).")
      (license license:gpl2))))


;;;
;;; Pluggable authentication modules (PAM).
;;;

(define-public linux-pam
  (package
    (name "linux-pam")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/linux-pam/linux-pam/releases/download/v"
             version "/Linux-PAM-" version ".tar.xz"))
       (sha256
        (base32
         "0kgrsj2scv5mx6w925h9hxf11jnqqs9z8s22aw94b90xm4qp3v74"))
       (patches (search-patches "linux-pam-unix_chkpwd.patch"
                                "linux-pam-no-setfsuid.patch"))))

    (build-system gnu-build-system)
    (inputs (list libxcrypt))
    (native-inputs
     (list flex
           ;; TODO: optional dependencies
           ;; ("cracklib" ,cracklib)
           ))
    (arguments
     (list
      ;; Most users, such as `shadow', expect the headers to be under
      ;; `security'.
      #:configure-flags #~(list (string-append "--includedir="
                                               (assoc-ref %outputs "out")
                                               "/include/security")
                                ;; explicit libdir for pkgconfig files
                                ;; drop with 1.5.3, which fixes
                                ;; https://github.com/linux-pam/linux-pam/issues/466
                                (string-append "--libdir="
                                               (assoc-ref %outputs "out")
                                               "/lib")

                                ;; XXX: <rpc/rpc.h> is missing from glibc when
                                ;; cross-compiling, so we have to disable NIS
                                ;; support altogether.
                                #$@(if (%current-target-system)
                                       #~("--disable-nis")
                                       #~()))

      #:phases (if (target-hurd?)
                   #~(modify-phases %standard-phases
                       (add-after 'unpack 'skip-pam-limits
                         (lambda _
                           ;; 'pam_limits.c' uses <sys/prctl.h>, which is
                           ;; Linux-specific.  Skip it on GNU/Hurd.
                           (substitute* "modules/Makefile.in"
                             (("pam_limits") "")))))
                   #~%standard-phases)

      ;; XXX: Tests won't run in chroot, presumably because /etc/pam.d
      ;; isn't available.
      #:tests? #f))
    (home-page "http://www.linux-pam.org/")
    (synopsis "Pluggable authentication modules for Linux")
    (description
     "A *Free* project to implement OSF's RFC 86.0.
Pluggable authentication modules are small shared object files that can
be used through the PAM API to perform tasks, like authenticating a user
at login.  Local and dynamic reconfiguration are its key features.")
    (license license:bsd-3)))

(define-public python-pamela
  (package
    (name "python-pamela")
    (version "1.0.0")
    (source
      (origin
        ;; Tests not distributed in pypi release.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/minrk/pamela")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0cg3w6np1fbjpvzhv54xg567hpf38szwp2d4gvzb9r736nxbv0vr"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; Test suite isn't designed to be run inside a container.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'hardcode-pam.so
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pam (assoc-ref inputs "linux-pam")))
               (substitute* "pamela.py"
                 (("find_library\\(\"pam\")")
                  (string-append "'" pam "/lib/libpam.so'")))
               #t)))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
               (if (file-exists? "test_pamela.py")
                 (invoke "py.test" "--assert=plain" "test_pamela.py")
                 (invoke "python" "-m" "pamela" "-a" "`whoami`"))
               #t))))))
    (inputs
     (list linux-pam))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/minrk/pamela")
    (synopsis "PAM interface using ctypes")
    (description "This package provides a PAM interface using @code{ctypes}.")
    (license license:expat)))

(define-public pam-gnupg
  (package
    (name "pam-gnupg")
    (version "0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cruegge/pam-gnupg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bf91gi6zmfzzmczxm7pajxdlgnikasvg5xsd3j0a368rcr7lf9l"))))
    (build-system gnu-build-system)
    (inputs
     (list gnupg linux-pam))
    (native-inputs
     (list autoconf automake libtool))
    (arguments
     `(#:tests? #f ;no tests suite
       #:configure-flags
       (list (string-append "--with-moduledir="
                            (assoc-ref %outputs "out") "/lib/security"))))

    (home-page "https://github.com/cruegge/pam-gnupg")
    (synopsis "Unlock GnuPG keys on login")
    (description "This package provides a PAM module that hands over your
login password to @code{gpg-agent}.  This can be useful if you are using a
GnuPG-based password manager like @code{pass}.")
    (license license:gpl3+)))


;;;
;;; Kernel documentation
;;;

(define-public linux-libre-documentation
  (package
    (inherit linux-libre)
    (name "linux-libre-documentation")
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (replace 'build
                     (lambda* (#:key parallel-build? #:allow-other-keys)
                       (substitute* "Documentation/Makefile"
                         ;; Remove problematic environment check script.
                         ((".*scripts/sphinx-pre-install.*") ""))
                       (invoke "make" "infodocs"
                               "-j" (if parallel-build?
                                        (number->string (parallel-job-count))
                                        "1"))))
                   (replace 'install
                     (lambda _
                       (let* ((info-dir (string-append #$output "/share/info"))
                              (info (string-append info-dir
                                                   "/TheLinuxKernel.info.gz")))
                         (with-directory-excursion "Documentation/output"
                           (invoke "make" "-C" "texinfo" "install-info"
                                   (string-append "infodir=" info-dir)))
                         ;; Create a symlink, for convenience.
                         (symlink info (string-append info-dir
                                                      "/linux.info.gz"))))))))
    (native-inputs
     (list graphviz
           perl
           python
           python-sphinx
           python-pyyaml
           texinfo
           which))
    (synopsis "Documentation for the kernel Linux-Libre")
    (description "This package provides the documentation for the kernel
Linux-Libre, as an Info manual.  To consult it, run @samp{info linux}.")))

;;;
;;; Miscellaneous.
;;;

(define-public powercap
  (package
    (name "powercap")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/powercap/powercap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vs84fmhdc3w1541vp0f5ydvdsbg0amjdv2g2f8xdvaw01nakxsn"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DBUILD_SHARED_LIBS=ON")))
    (home-page "https://github.com/powercap/powercap")
    (synopsis "Utilities for accessing the powercap Linux kernel feature")
    (description "This package contains utilities for accessing the powercap
Linux kernel feature through sysfs.  It includes an implementation for working
with Intel @acronym{RAPL, Running Average Power Limit}.
It provides the commands @code{powercap-info} and @code{powercap-set}.")
    (license license:bsd-3)))

(define-public powerstat
  (package
    (name "powerstat")
    (version "0.04.03")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ColinIanKing/powerstat")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a1fnw4rb8fx4wrcl0yc6dbjj49k6vagnq7diyw4fs4i5nin7mv3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (home-page "https://kernel.ubuntu.com/~cking/powerstat/")
    (synopsis "Measure system power consumption")
    (description
     "Powerstat measures and reports your computer's power consumption in real
time.  On mobile PCs, it uses ACPI battery information to measure the power
drain of the entire system.

Powerstat can also report @acronym{RAPL, Running Average Power Limit} power
domain measurements.  These are available only on some hardware such as Intel
Sandybridge and newer, and cover only part of the machine's components such as
CPU, DRAM, and graphics.  However, they provide accurate and immediate readings
and don't require a battery at all.

The output is like @command{vmstat} but also shows power consumption statistics:
at the end of a run, @command{powerstat} will calculate the average, standard
deviation, and minimum and maximum values.  It can show a nice histogram too.")
    (license license:gpl2)))

(define-public psmisc
  (package
    (name "psmisc")
    (version "23.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/psmisc/psmisc/psmisc-"
                          version ".tar.xz"))
      (sha256
       (base32 "09y30ywh1d4s4f0wgvki115kgpliq48rarmfmmjl0iq22jf5viaq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      (if (%current-target-system)
          #~(list "ac_cv_func_malloc_0_nonnull=yes"
                  "ac_cv_func_realloc_0_nonnull=yes")
          #~'())))
    (inputs (list ncurses))
    (home-page "https://gitlab.com/psmisc/psmisc")
    (synopsis "Small utilities that use the proc file system")
    (description
     "psmisc is a set of small utilities that use the proc file system.
@itemize @bullet
@item @command{fuser} identifies processes using files or sockets;
@item @command{killall} kills processes by name;
@item @command{prtstat} prints statistics of a process;
@item @command{pslog} prints the log file(s) of a process;
@item @command{pstree} shows the currently running processes as a tree;
@item @command{peekfd} shows the data travelling over a file descriptor.
@end itemize")
    (license license:gpl2+)))

(define-public util-linux
  (package
    (name "util-linux")
    (version "2.37.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/utils/"
                                  "util-linux/v" (version-major+minor version) "/"
                                  "util-linux-" version ".tar.xz"))
              (sha256
               (base32
                "10svcnsqmrsd660bzcm7k6dm8sa7hkknhr3bag1nccwimlb6jkk3"))
              (patches (search-patches "util-linux-tests.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; We take 'nologin' from Shadow, the 'logger' program from
               ;; GNU Inetutils and 'kill' from GNU Coreutils.
               '(begin
                  (substitute* "configure"
                    (("build_nologin=yes") "build_nologin=no")
                    (("build_logger=yes") "build_logger=no")
                    (("build_kill=yes") "build_kill=no"))
                  #t))))
    (build-system gnu-build-system)
    (outputs '("out"            ;6.4 MiB executables and documentation
               "lib"            ;8.8 MiB shared libraries, headers and locales
               "static"))       ;2.9 MiB static .a libraries
    (arguments
     (list #:configure-flags
           #~(list "--disable-use-tty-group"
                   (string-append
                    "--enable-fs-paths-default="
                    "/run/setuid-programs"
                    ":/run/current-system/profile/sbin")
                   ;; Don't try to chown root:root mount and umount
                   "--disable-makeinstall-chown"
                   "--localstatedir=/var"
                   (string-append "--localedir=" #$output:lib
                                  "/share/locale")
                   ;; Install completions where our bash-completion package
                   ;; expects them.
                   (string-append "--with-bashcompletiondir=" #$output
                                  "/etc/bash_completion.d"))

           ;; FIXME: For now we cannot reliably run tests on GNU/Hurd:
           ;; <https://bugs.gnu.org/47791>.
           #:tests? (and (not (%current-target-system))
                         (not (string-suffix? "-gnu" (%current-system))))

           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'patch-build-scripts
                 (lambda _
                   (substitute* "configure"
                     ;; The build system assumes that we want to install
                     ;; libraries below $exec_prefix when $libdir does not
                     ;; match any of the "usual" locations.  Fix that.
                     (("usrlib_execdir='\\$\\{exec_prefix\\}'\\$libdir")
                      "usrlib_execdir=$libdir"))))
               (add-before 'build 'set-umount-file-name
                 (lambda _
                   ;; Tell 'eject' the right file name of 'umount'.
                   (substitute* "sys-utils/eject.c"
                     (("\"/bin/umount\"")
                      (string-append "\"" #$output "/bin/umount\"")))))
               (add-before 'check 'pre-check
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   (let ((services (search-input-file (or native-inputs inputs)
                                                      "etc/services")))
                     ;; Change the test to refer to the right file.
                     (substitute* "tests/ts/misc/mcookie"
                       (("/etc/services")
                        services)))))
               (add-before 'check 'disable-setarch-test
                 (lambda _
                   ;; The setarch tests are unreliable in QEMU's user-mode
                   ;; emulation, which is our primary method of building
                   ;; ARMv7 packages.  See
                   ;; <https://github.com/karelzak/util-linux/issues/601>.
                   (substitute* "tests/ts/misc/setarch"
                     (("ts_init_subtest.*" all)
                      (string-append
                       all "\n"
                       "ts_skip \"setarch tests are unreliable under QEMU\"")))))
               (add-before 'check 'disable-lsns-test
                 (lambda _
                   ;; The lsns tests can fail due to ioctl(_, NS_GET_USERNS)
                   ;; returning ENOTTY, indicating this kernel does not
                   ;; support user namespaces.  Curiously, this test can fail
                   ;; on i686 even if the same test passes on x86_64 on the
                   ;; same machine.  See <https://issues.guix.gnu.org/49933>.
                   (delete-file "tests/ts/lsns/ioctl_ns")))
               (add-after 'install 'move-static-libraries
                 (lambda _
                   (let ((lib    #$output:lib)
                         (static #$output:static))

                     ;; Move static libraries to the "static" output.
                     (mkdir-p (string-append static "/lib"))
                     (with-directory-excursion lib
                       (for-each (lambda (file)
                                   (rename-file file
                                                (string-append static "/"
                                                               file)))
                                 (find-files "lib" "\\.a$"))

                       ;; Remove references to the static library from the '.la'
                       ;; files so that Libtool does the right thing when both
                       ;; the shared and static library is available.
                       (substitute* (find-files "lib" "\\.la$")
                         (("old_library=.*") "old_library=''\n"))))))
               (add-after 'install 'adjust-pkg-config-files
                 (lambda _
                   ;; Drop the unused "prefix=" and "exec_prefix=" variables from
                   ;; the pkg-config files to avoid a cyclic reference on "out".
                   (substitute* (find-files (string-append #$output:lib
                                                           "/lib/pkgconfig")
                                            "\\.pc$")
                     (("^(exec_)?prefix=.*") "")))))))
    (inputs
     (list file                         ;for libmagic
           ncurses
           zlib))
    (native-inputs
     (list net-base                     ;for tests
           perl))
    (home-page "https://www.kernel.org/pub/linux/utils/util-linux/")
    (synopsis "Collection of utilities for the Linux kernel")
    (description "Util-linux is a diverse collection of Linux kernel
utilities.  It provides dmesg and includes tools for working with file systems,
block devices, UUIDs, TTYs, and many other tools.")

    (properties '((upstream-name . "util-linux")))
    ;; Note that util-linux doesn't use the same license for all the
    ;; code.  GPLv2+ is the default license for a code without an
    ;; explicitly defined license.
    (license (list license:gpl3+ license:gpl2+ license:gpl2 license:lgpl2.0+
                   license:bsd-4 license:public-domain))))

;; util-linux optionally supports udev, which allows lsblk to read file system
;; metadata without special privileges.  Add it as a separate package to avoid
;; a circular dependency, and to keep the size small.
(define-public util-linux+udev
  (package/inherit
   util-linux
   (name "util-linux-with-udev")
   (inputs
    (modify-inputs (package-inputs util-linux)
      (prepend eudev)))))

(define-public ddate
  (package
    (name "ddate")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/bo0ts/ddate")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1qchxnxvghbma6gp1g78wnjxsri0b72ha9axyk31cplssl7yn73f"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/bo0ts/ddate")
    (synopsis "PERPETUAL DATE CONVERTER FROM GREGORIAN TO POEE CALENDAR")
    (description
     "ddate displays the Discordian date and holidays of a given date.
The Discordian calendar was made popular by the \"Illuminatus!\" trilogy
by Robert Shea and Robert Anton Wilson.")
    (license license:public-domain)))

(define-public dislocker
  (package
    (name "dislocker")
    (version "0.7.3")
    (home-page "https://github.com/Aorimn/dislocker")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ak68s1v5dwh8y2dy5zjybmrh0pnqralmyqzis67y21m87g47h2k"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ;no test suite
    (inputs (list fuse-2 mbedtls-lts))
    (synopsis "FUSE driver to read/write Windows BitLocker drives")
    (description
     "This package provides means to to read BitLocker encrypted
partitions.  Write functionality is also provided but check the README.")
    (license license:gpl2+)))

(define-public dwarves
  (package
    (name "dwarves")
    (version "1.29")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/acmel/dwarves")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ia3r77zyw7r8mydy1zrrdnqzm9ginsvf9pqviq48rglihhddvli"))
              (patches
               (search-patches "dwarves-threading-reproducibility.patch"))))
    (build-system cmake-build-system)
    (arguments (list #:configure-flags #~(list "-D__LIB=lib"
                                               "-DLIBBPF_EMBEDDED=OFF")
                     #:tests? #f))      ;no test suite
    (native-inputs (list pkg-config))
    (inputs (list libbpf))
    (home-page "https://github.com/acmel/dwarves")
    (synopsis "Debugging information processing library and utilities")
    (description "Dwarves is a set of tools that use the debugging information
inserted in ELF binaries by compilers such as GCC, used by well known
debuggers such as GDB.

Utilities in the Dwarves suite include @command{pahole}, that can be used to
find alignment holes in structures and classes in languages such as C, C++,
but not limited to these.  These tools can also be used to encode and read the
BTF type information format used with the kernel Linux @code{bpf} syscall.

The @command{codiff} command can be used to compare the effects changes in
source code generate on the resulting binaries.

The @command{pfunct} command can be used to find all sorts of information
about functions, inlines, decisions made by the compiler about inlining, etc.

The @command{pahole} command can be used to use all this type information to
pretty print raw data according to command line directions.

Headers can have its data format described from debugging info and offsets from
it can be used to further format a number of records.

Finally, the @command{btfdiff} command can be used to compare the output of
pahole from BTF and DWARF, to make sure they produce the same results.")
    (license license:gpl2+)))

(define-public fbset
  (package
    (name "fbset")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://users.telenet.be/geertu/Linux/fbdev/fbset-"
                           version ".tar.gz"))
       (sha256
        (base32 "080wnisi0jq7dp0jcwdp83rq8q8s3kw41vc712516xbv4jq4mzs0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'pre-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("mknod ") "true ")
                 ;; The Makefile doesn't honour PREFIX or similar.
                 (("/usr") out))
               (mkdir out)
               (with-directory-excursion out
                 (for-each mkdir-p (list "sbin"
                                         "man/man5"
                                         "man/man8")))
               #t)))
         (add-after 'install 'install-fb.modes
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (etc (string-append out "/etc")))
               (for-each (cut install-file <> etc)
                         (find-files "etc" "^fb\\.modes"))
               (symlink "fb.modes.ATI"
                        (string-append etc "/fb.modes"))
               #t))))
       ;; Parallel building races to create modes.tab.c.
       #:parallel-build? #f
       #:tests? #f))                    ; no test suite
    (native-inputs
     (list bison flex))
    (home-page "http://users.telenet.be/geertu/Linux/fbdev/")
    (synopsis "Show and modify Linux frame buffer settings")
    (description
     "The kernel Linux's @dfn{frame buffers} provide a simple interface to
different kinds of graphic displays.  The @command{fbset} utility can query and
change various device settings such as depth, virtual resolution, and timing
parameters.")
    (license license:gpl2)))

(define-public procps
  (package
    (name "procps")
    (version "4.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/procps-ng/Production/"
                                  "procps-ng-" version ".tar.xz"))
              (sha256
               (base32
                "14ms9mkqr1mgl3h5yl6w8m57cf39k382nv7qms78vqbaz728wg1h"))
              (patches (search-patches "procps-strtod-test.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:modules '((guix build utils)
                       (guix build gnu-build-system)
                       (srfi srfi-1)
                       (srfi srfi-26))
           #:configure-flags
           (if (%current-target-system)
               #~'("ac_cv_func_malloc_0_nonnull=yes"
                   "ac_cv_func_realloc_0_nonnull=yes")
               #~'())
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'post-install
                 ;; Remove commands and man pages redudant with
                 ;; Coreutils.
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out #$output)
                          (dup (append-map (cut find-files out <>)
                                           '("^kill" "^uptime"))))
                     (for-each delete-file dup))))
               #$@(if (system-hurd?)
                      #~((add-after 'unpack 'skip-tests
                           (lambda _
                             (substitute* "library/tests/test_version.c"
                               (("^int main\\(.*" all)
                                (string-append all "{\n  exit (77);//"))))))
                      #~()))))
    (inputs (list ncurses))
    (native-inputs (list pkg-config))
    (home-page "https://gitlab.com/procps-ng/procps/")
    (synopsis "Utilities that give information about processes")
    (description
     "Procps is the package that has a bunch of small useful utilities
that give information about processes using the Linux /proc file system.
The package includes the programs free, pgrep, pidof, pkill, pmap, ps, pwdx,
slabtop, tload, top, vmstat, w, watch and sysctl.")
    (properties '((upstream-name . "procps-ng")))
    (license license:gpl2)))

(define-public usbutils
  (package
    (name "usbutils")
    (version "017")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kernel.org/linux/utils/usb/usbutils/"
                          "usbutils-" version ".tar.xz"))
      (sha256
       (base32 "0nz008kshcajc9asxr4j5dh4wgq72z52lws4ga6y60wirzymz8m6"))))
    (build-system gnu-build-system)
    (outputs (list "out" "python"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'bootstrap 'patch-bootstrap-scripts
            (lambda _
              (substitute* "usbhid-dump/bootstrap"
                (("/bin/sh") (which "sh")))))
          (add-after 'install 'separate-python-output
            ;; Separating one Python script shaves more than 106 MiB from :out.
            (lambda _
              (for-each (lambda (file)
                          (let ((old (string-append #$output "/" file))
                                (new (string-append #$output:python "/" file)))
                            (mkdir-p (dirname new))
                            (rename-file old new)))
                        (list "bin/lsusb.py")))))))
    (inputs
     (list eudev libusb python))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "http://www.linux-usb.org/")
    (synopsis
     "Tools for working with USB devices")
    (description
     "Collection of tools to query what type of USB devices are connected to the
system, including @command{lsusb}.")
    (license license:gpl2+)))

(define-public usbip-utils
  (package
    (name "usbip-utils")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'enter-subdirectory
                    (lambda _
                      (chdir "tools/usb/usbip")
                      #t)))))
    (native-inputs
     `(("automake" ,automake)
       ("autoreconf" ,autoconf)
       ("libtool" ,libtool)))
    (inputs (list eudev))
    (home-page (package-home-page linux-libre))
    (synopsis "Utilities for sharing USB devices over IP networks")
    (description
     "The USB/IP protocol enables to pass USB device from a server to
a client over the network.  The server is a machine which shares an
USB device and the client is a machine which uses USB device provided by
a server over the network.  The USB device may be either physical device
connected to a server or software entity created on a server using USB
gadget subsystem.  The usbip-utils are userspace tools to used to handle
connection and management on both side.  The client needs the @file{vhci-hcd}
Linux kernel module and the server needs the @file{usbip_host} Linux kernel
module.")
    (license license:gpl2)))

(define-public e2fsprogs
  (package
    (name "e2fsprogs")
    (version "1.47.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/kernel/people/tytso/"
                   "e2fsprogs/v" version "/"
                   "e2fsprogs-" version ".tar.xz"))
             (sha256
              (base32
               "0g76fhnyzr2awwybd6c16439l80r5dv4kbfaq7cr908fr9j2w908"))))
    (build-system gnu-build-system)
    (inputs (list `(,util-linux "lib")))
    (native-inputs (list pkg-config
                         texinfo ;for the libext2fs Info manual
                         ;; For tests.
                         perl
                         procps))
    (arguments
     `(;; util-linux is the preferred source for some of the libraries and
       ;; commands, so disable them (see, e.g.,
       ;; <http://git.buildroot.net/buildroot/commit/?id=e1ffc2f791b33633>.)
       #:configure-flags (list "--disable-libblkid"
                               "--disable-libuuid" "--disable-uuidd"
                               "--disable-fsck"

                               ;; Use symlinks instead of hard links for
                               ;; 'fsck.extN' etc.  This makes the resulting nar
                               ;; smaller and is preserved across copies.
                               "--enable-symlink-install"

                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib")

                               ;; Install libext2fs et al.
                               "--enable-elf-shlibs")

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-shells
           (lambda _
             (substitute* "configure"
               (("/bin/sh (.*)parse-types.sh" _ dir)
                (string-append (which "sh") " " dir
                               "parse-types.sh")))
             (substitute* "MCONFIG.in"
               (("INSTALL_SYMLINK = /bin/sh")
                "INSTALL_SYMLINK = sh"))
             (substitute* (find-files "." "^Makefile.in$")
               (("#!/bin/sh")
                (string-append "#!" (which "sh"))))))
           (add-after 'install 'install-libs
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib")))
                 (invoke "make" "install-libs")

                 ;; Make the .a writable so that 'strip' works.
                 ;; Failing to do that, due to debug symbols, we
                 ;; retain a reference to the final
                 ;; linux-libre-headers, which refer to the
                 ;; bootstrap binaries.
                 (let ((archives (find-files lib "\\.a$")))
                   (for-each (lambda (file)
                               (chmod file #o666))
                             archives)))))
           ,@(if (system-hurd?)
                 '((add-after 'unpack 'skip-tests
                     (lambda _
                       (with-directory-excursion "tests"
                         (for-each
                          (lambda (directory)
                            (delete-file-recursively directory))
                          '("d_bad_ostype"
                            "f_detect_junk"
                            "f_extent_oobounds"
                            "j_ext_long_revoke_trans"
                            "j_ext_long_trans"
                            "j_long_revoke_trans"
                            "j_long_revoke_trans_mcsum_32bit"
                            "j_long_revoke_trans_mcsum_64bit"
                            "j_long_trans"
                            "j_long_trans_mcsum_32bit"
                            "j_long_trans_mcsum_64bit"
                            "j_short_revoke_trans"
                            "j_short_revoke_trans_mcsum_64bit"
                            "j_short_trans_64bit"
                            "j_short_trans"
                            "j_short_trans_mcsum_64bit"
                            "j_short_trans_old_csum"
                            "j_short_trans_open_recover"
                            "j_short_trans_recover"
                            "j_short_trans_recover_mcsum_64bit"
                            "j_short_uncommitted_trans"
                            "j_short_uncommitted_trans_mcsum_64bit"
                            "m_error_behavior"
                            "m_minrootdir"
                            "m_rootdir"
                            "r_32to64bit_expand_full"
                            "r_expand_full"
                            "r_fixup_lastbg_big"
                            "t_change_uuid"
                            "t_change_uuid_mcsum"
                            "t_change_uuid_mcsum_mounted"
                            "t_change_uuid_mcsum_seed_mounted"
                            "t_change_uuid_mounted"
                            "t_disable_changed_csum_seed"
                            "t_disable_changed_csum_seed_mounted"
                            "t_disable_csum_seed"
                            "t_disable_meta_csum_and_seed"
                            "t_enable_csum_seed"
                            "t_format_csum_seed"
                            "t_replay_and_set"
                            "u_compound_rollback"
                            "u_corrupt_blk_csum"
                            "u_corrupt_blk_csum_force"
                            "u_corrupt_key_csum"
                            "u_debugfs_opt"
                            "u_dryrun"
                            "u_e2fsck_opt"
                            "u_errorout"
                            "u_force"
                            "u_force_dryrun"
                            "u_incomplete"
                            "u_mke2fs_opt"
                            "u_mke2fs_opt_oddsize"
                            "u_offset"
                            "u_onefile_bad"
                            "u_resize2fs_opt"
                            "u_revert_64bitmcsum_onefile"
                            "u_revert_all_onefile"
                            "u_revert_upgrade_to_64bitmcsum"
                            "u_tune2fs_opt"
                            "u_undo_undo"
                            "u_wrong_fs"))))))
                 '()))))
    (home-page "https://e2fsprogs.sourceforge.net/")
    (synopsis "Creating and checking ext2/ext3/ext4 file systems")
    (description
     "This package provides tools for manipulating ext2/ext3/ext4 file systems.")
    (license (list license:gpl2                   ;programs
                   license:lgpl2.0                ;libext2fs
                   license:x11))))                ;libuuid

(define e2fsprogs/static
  (static-package
   (package (inherit e2fsprogs)
            (arguments
             ;; Do not build shared libraries.
             (substitute-keyword-arguments (package-arguments e2fsprogs)
               ((#:configure-flags _)
                '(list "--disable-blkid"))
               ((#:make-flags _)
                '(list)))))))

(define-public e2fsck/static
  (package
    (name "e2fsck-static")
    (version (package-version e2fsprogs))
    (build-system trivial-build-system)
    (source #f)
    (inputs
     (list e2fsprogs/static))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))

         (let ((e2fsck (search-input-file %build-inputs "/sbin/e2fsck"))
               (bin    (string-append (assoc-ref %outputs "out") "/sbin")))
           (mkdir-p bin)
           (with-directory-excursion bin
             (copy-file e2fsck "e2fsck")
             (remove-store-references "e2fsck")
             (chmod "e2fsck" #o555))
           #t))))
    (home-page (package-home-page e2fsprogs))
    (synopsis "Statically-linked e2fsck command from e2fsprogs")
    (description "This package provides statically-linked e2fsck command taken
from the e2fsprogs package.  It is meant to be used in initrds.")
    (license (package-license e2fsprogs))))

(define-public extundelete
  (package
    (name "extundelete")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/extundelete/"
                           "extundelete/" version "/extundelete-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "1x0r7ylxlp9lbj3d7sqf6j2a222dwy2nfpff05jd6mkh4ihxvyd1"))
       (patches (search-patches "extundelete-e2fsprogs-1.44.patch"))))
    (build-system gnu-build-system)
    (inputs (list e2fsprogs))
    (home-page "https://extundelete.sourceforge.net/")
    (synopsis "Recover deleted files from ext2/3/4 partitions")
    (description
     "Extundelete is a set of tools that can recover deleted files from an
ext3 or ext4 partition.")
    (license license:gpl2)))

(define-public zerofree
  (package
    (name "zerofree")
    (version "1.1.1")
    (home-page "https://frippery.org/uml/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page name "-" version
                                  ".tgz"))
              (sha256
               (base32
                "0rrqfa5z103ws89vi8kfvbks1cfs74ix6n1wb6vs582vnmhwhswm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (chmod "zerofree" #o555)
               (install-file "zerofree" bin)
               #t))))
       #:tests? #f))                    ; no tests
    (inputs `(("libext2fs" ,e2fsprogs)))
    (synopsis "Zero non-allocated regions in ext2/ext3/ext4 file systems")
    (description
     "Zerofree finds the unallocated blocks with non-zero value content in an
ext2, ext3, or ext4 file system and fills them with zeroes (or another value).
This is a simple way to make disk images more compressible.
Zerofree requires the file system to be unmounted or mounted read-only.")
    (license license:gpl2)))

(define-public strace
  (package
    (name "strace")
    (version "6.4")
    (home-page "https://strace.io")
    (source (origin
             (method url-fetch)
             (uri (string-append home-page "/files/" version
                                 "/strace-" version ".tar.xz"))
             (sha256
              (base32
               "0f4jxgsdr76mf51kv2kwhv39ap7kilrchkfvqrhd5pvzqnx7v617"))
             (patches (search-patches "strace-readlink-tests.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* "src/strace.c"
               (("/bin/sh") (which "sh")))))
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "tests/Makefile.in"
               ;; XXX: These hang forever even if the test time-out is
               ;; extended.
               (("^\tstrace-DD?D?\\.test \\\\.*") "")
               (("^\tpidns-cache.test \\\\.*") "")
               (("^\t.*--pidns-translation.test \\\\.*") "")
               ;; This one fails with an encoding error.
               (("^\t.*net-yy-unix.test \\\\.*") "")))))
       ;; Don't fail if the architecture doesn't support different
       ;; personalities.
       #:configure-flags '("--enable-mpers=check")
       ;; See <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32459>.
       #:parallel-tests? #f))           ; undeterministic failures
    (native-inputs (list perl))
    (synopsis "System call tracer for Linux")
    (description
     "strace is a system call tracer, i.e. a debugging tool which prints out a
trace of all the system calls made by a another process/program.")
    (properties
     '((release-monitoring-url . "https://github.com/strace/strace/releases")))
    (license license:lgpl2.1+)))

(define-public ltrace
  (package
    (name "ltrace")
    (version "0.7.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ltrace.org/ltrace_" version
                                 ".orig.tar.bz2"))
             (sha256
              (base32
               "00wmbdghqbz6x95m1mcdd3wd46l6hgcr4wggdp049dbifh3qqvqf"))))
    (build-system gnu-build-system)
    (inputs `(("libelf" ,elfutils)))
    (arguments
     ;; Compilation uses -Werror by default, but it fails.
     '(#:configure-flags '("--disable-werror")))
    (home-page "https://www.ltrace.org/")
    (synopsis "Library call tracer for Linux")
    (description
     "ltrace intercepts and records dynamic library calls which are called by
an executed process and the signals received by that process.  It can also
intercept and print the system calls executed by the program.")
    (license license:gpl2+)))

(define-public alsa-ucm-conf
  (package
    (name "alsa-ucm-conf")
    (version "1.2.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.alsa-project.org/pub/lib/" name "-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "10dfzvrmpp9swflw47nxf35an6gj3ilb4wlggdnng8g2637h2z1q"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("ucm" "share/alsa/ucm")
         ("ucm2" "share/alsa/ucm2"))))
    (home-page "https://www.alsa-project.org/wiki/Main_Page")
    (synopsis "The Advanced Linux Sound Architecture Use Case Manager")
    (description
     "This package contains Advanced Linux Sound Architecture Use Case Manager
configuration of audio input/output names and routing for specific audio
hardware.")
    (license license:bsd-3)))

(define-public alsa-topology-conf
  (package
    (name "alsa-topology-conf")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.alsa-project.org/pub/lib/" name "-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "01zdg6q4s6d01k39z96wi4vbhrfw1i2g4yi5dijwfk6a5vjfdq2m"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("topology" "share/alsa/topology"))))
    (home-page "https://www.alsa-project.org/wiki/Main_Page")
    (synopsis "The Advanced Linux Sound Architecture libraries")
    (description
     "This package contains Advanced Linux Sound Architecture topology
configuration files that can be used for specific audio hardware.")
    (license license:bsd-3)))

(define-public alsa-lib
  (package
    (name "alsa-lib")
    (version "1.2.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.alsa-project.org/files/pub/lib/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0kdvjlknc50fwfdkxj0z12xbz21skb3gnwlh6lvsvycmp5ljygwz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'pre-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((ucm
                     (string-append (assoc-ref inputs "alsa-ucm-conf")))
                    (topology
                     (string-append (assoc-ref inputs "alsa-topology-conf")))
                    (alsa
                     (string-append (assoc-ref outputs "out") "/share/alsa"))
                    (ucm-share
                     (string-append ucm "/share/alsa/ucm"))
                    (ucm2-share
                     (string-append ucm "/share/alsa/ucm2"))
                    (topology-share
                     (string-append topology "/share/alsa/topology")))
               (mkdir-p alsa)
               (symlink ucm-share (string-append alsa "/ucm"))
               (symlink ucm2-share (string-append alsa "/ucm2"))
               (symlink topology-share (string-append alsa "/topology")))
             #t)))))
    (inputs
     (list alsa-ucm-conf alsa-topology-conf))
    (home-page "https://www.alsa-project.org/wiki/Main_Page")
    (synopsis "The Advanced Linux Sound Architecture libraries")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.")
    (license license:lgpl2.1+)))

(define-public alsa-utils
  (package
    (name "alsa-utils")
    (version "1.2.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.alsa-project.org/files/pub/utils/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "19r8qi6b7sd2p1mhxfqrp18wrgjw5s6rp5ygimb1w59zi0xcmils"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; The udev rule is responsible for restoring the volume.
         (string-append "--with-udev-rules-dir=" #$output "/lib/udev/rules.d"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'pre-install
            (lambda _
              ;; Don't try to mkdir /var/lib/alsa.
              (substitute* "Makefile"
                (("\\$\\(MKDIR_P\\) .*ASOUND_STATE_DIR.*")
                 "true\n")))))))
    (native-inputs
     (list docbook-xml-4.2 docbook-xsl xmlto
           gettext-minimal))
    (inputs
     (list libsamplerate ncurses alsa-lib))
    (home-page "http://www.alsa-project.org/")
    (synopsis "Utilities for the Advanced Linux Sound Architecture (ALSA)")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.")

    ;; This is mostly GPLv2+ but a few files such as 'alsactl.c' are
    ;; GPLv2-only.
    (license license:gpl2)))

(define-public alsa-plugins
  (package
    (name "alsa-plugins")
    (version "1.2.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp.alsa-project.org/pub/plugins/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "0z9k3ssbfk2ky2w13avgyf202j1drsz9sv3834bp33cj1i2hc3qw"))))
    (build-system gnu-build-system)
    ;; TODO: Split libavcodec and speex if possible. It looks like they can not
    ;; be split, there are references to both in files.
    ;; TODO: Remove OSS related plugins, they add support to run native
    ;; ALSA applications on OSS however we do not offer OSS and OSS is
    ;; obsolete.
    (outputs '("out" "pulseaudio" "jack"))
    (native-search-paths
      (list (search-path-specification
              (variable "ALSA_PLUGIN_DIR")
              (files '("lib/alsa-lib"))
              (separator #f))))
    (arguments
     `(#:configure-flags '(;; Do not install a "local" configuration targeted
                           ;; for /etc/alsa.  On Guix System plugins are loaded from
                           ;; the ALSA service, and other distributions likely
                           ;; won't use these files.
                           "--with-alsalconfdir=/tmp/noop")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'split
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Distribute the binaries to the various outputs.
             (let* ((out (assoc-ref outputs "out"))
                    (jack (assoc-ref outputs "jack"))
                    (jacklib (string-append jack "/lib/alsa-lib"))
                    (pua (assoc-ref outputs "pulseaudio"))
                    (pualib (string-append pua "/lib/alsa-lib")))
               ;; For jack.
               (mkdir-p jacklib)
               (for-each (lambda (file)
                           (rename-file file (string-append jacklib "/" (basename file))))
                         (find-files out ".*jack\\.(la|so)"))
               ;; For pulseaudio.
               (mkdir-p pualib)
               (for-each (lambda (file)
                           (rename-file file (string-append pualib "/" (basename file))))
                         (find-files out ".*pulse\\.(la|so)"))
               #t))))))
    (inputs
     (list alsa-lib
           jack-1
           speex ; libspeexdsp resampling plugin
           libsamplerate ; libsamplerate resampling plugin
           ffmpeg ; libavcodec resampling plugin, a52 plugin
           pulseaudio)) ; PulseAudio plugin
    (native-inputs
     (list pkg-config))
    (home-page "http://www.alsa-project.org/")
    (synopsis "Plugins for the Advanced Linux Sound Architecture (ALSA)")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.  This package enhances ALSA
by providing additional plugins which include: upmixing, downmixing, jackd and
pulseaudio support for native alsa applications, format conversion (s16 to a52), and
external rate conversion.")
    (license (list license:gpl2+
                   ;; `rate/rate_samplerate.c': LGPL v2.1 or later.
                   license:lgpl2.1+))))

(define-public iptables
  (package
    (name "iptables")
    (version "1.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://netfilter.org/iptables/iptables-"
                                 version ".tar.bz2")))
       (sha256
        (base32 "17w5a4znq8rdj5djcldmy6mbnxq1v88ibssk2mipc1kivj4miivi"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config flex bison))
    (inputs
     (list libmnl libnftnl/pinned))
    (arguments
     (list #:tests? #f             ; no test suite
           #:configure-flags       ; add $libdir to the RUNPATH of executables
           #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))))
    (home-page "https://www.netfilter.org/projects/iptables/index.html")
    (synopsis "Programs to configure Linux IP packet filtering rules")
    (description
     "@command{iptables} is the user-space command line program used to
configure the Linux 2.4.x and later IPv4 packet filtering ruleset
(@dfn{firewall}), including @dfn{NAT} (Network Address Translation).

This package also includes @command{ip6tables}, which is used to configure the
IPv6 packet filter.

Both commands are targeted at system administrators.")
    (properties
     '((release-monitoring-url . "https://www.netfilter.org/pub/iptables/")))
    (license license:gpl2+)))

(define-public iptables-nft
  (package
    (inherit iptables)
    (name "iptables-nft")
    (source #f)
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'unpack)
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each
               (lambda (command-path)
                 (let ((link-path (string-append #$output command-path)))
                   (mkdir-p (dirname link-path))
                   (symlink (search-input-file inputs "sbin/xtables-nft-multi")
                            link-path)))
               (apply append
                      '("/bin/iptables-xml")
                      (map (lambda (xtables)
                             (list (string-append "/sbin/" xtables)
                                   (string-append "/sbin/" xtables "-restore")
                                   (string-append "/sbin/" xtables "-save")))
                           '("arptables"
                             "ebtables"
                             "iptables"
                             "ip6tables")))))))))
    (inputs (list iptables))
    (native-inputs '())
    (synopsis
     "Programs to configure Linux IP packet filtering rules (nftables API)")))

(define-public bolt
  (package
    (name "bolt")
    (version "0.9.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/bolt/bolt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b9z0sfrz6bj0mddng9s0dx59g9239zmrl03hxx2x88mb7r0wmcg"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags '(list "--localstatedir=/var")
           #:glib-or-gtk? #t ;To wrap binaries and/or compile schemas
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'replace-directories
                          (lambda* (#:key outputs #:allow-other-keys)
                            (substitute* "meson.build"
                              (("udev.get_pkgconfig_variable..udevdir..")
                               (string-append "'"
                                              #$output "/lib/udev'")))
                            (substitute* "scripts/meson-install.sh"
                              (("mkdir.*")
                               ""))))
                        (add-before 'install 'no-polkit-magic
                          (lambda* (#:key outputs #:allow-other-keys)
                            (setenv "PKEXEC_UID" "something"))))))
    (native-inputs (list pkg-config
                         `(,glib "bin") python asciidoc umockdev))
    (inputs (list eudev dbus polkit))
    (synopsis "Thunderbolt 3 device manager")
    (description
     "This package provides @command{boltd}, a userspace daemon
for Thunderbolt devices, and @command{boltctl}, a command-line utility for
managing those devices.

The daemon @command{boltd} exposes devices via D-Bus to clients.  It also
stores a database of previously authorized devices and will, depending on the
policy set for the individual devices, automatically authorize newly connected
devices without user interaction.

The command-line utility @command{boltctl} manages Thunderbolt devices via
@command{boltd}.  It can list devices, monitor changes, and initiate
authorization of devices.")
    (home-page "https://gitlab.freedesktop.org/bolt/bolt")
    (license license:gpl2+)))

(define-public jitterentropy-rngd
  (package
    (name "jitterentropy-rngd")
    (version "1.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smuellerDD/jitterentropy-rngd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13br8s6gqnfc844ps38ya5nny3pndsmskszv3dsp1cxcgvmscg1c"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no test suite
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output)
              "UNITDIR=$(PREFIX)/lib/systemd/system")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))         ; no ./configure script
    (home-page "https://www.chronox.de/jent.html")
    (synopsis "CPU jitter random number generator daemon")
    (description
     "This simple daemon feeds entropy from the CPU Jitter @acronym{RNG, random
number generator} core to the kernel Linux's entropy estimator.  This prevents
the @file{/dev/random} device from blocking and should benefit users of the
preferred @file{/dev/urandom} and @code{getrandom()} interfaces too.

The CPU Jitter RNG itself is part of the kernel and claims to provide good
entropy by collecting and magnifying differences in CPU execution time as
measured by the high-resolution timer built into modern CPUs.  It requires no
additional hardware or external entropy source.

The random bit stream generated by @command{jitterentropy-rngd} is not processed
by a cryptographically secure whitening function.  Nonetheless, its authors
believe it to be a suitable source of cryptographically secure key material or
other cryptographically sensitive data.

If you agree with them, start this daemon as early as possible to provide
properly seeded random numbers to services like SSH or those using TLS during
early boot when entropy may be low, especially in virtualised environments.")
    (license (list license:bsd-3        ; or
                   license:gpl2+))))

(define-public ebtables
  (package
    (name "ebtables")
    (version "2.0.11")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://netfilter.org/ebtables/ebtables-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0apxgmkhsk3vxn9q3libxn3dgrdljrxyy4mli2gk49m7hi3na7xp"))))
    (build-system gnu-build-system)
    (inputs
     (list perl iptables))
    (synopsis "Ethernet bridge frame table administration")
    (home-page "https://ebtables.netfilter.org/")
    (description
     "ebtables is an application program used to set up and maintain the
tables of rules (inside the Linux kernel) that inspect Ethernet frames.  It is
analogous to the iptables application, but less complicated, due to the fact
that the Ethernet protocol is much simpler than the IP protocol.")
    (license license:gpl2+)))

(define-public iproute
  (package
    (name "iproute2")
    (version "6.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/net/iproute2/iproute2-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0wm2g70vfhnf8wb6py3zmzwxp4zv1icny1pvkwaxmr67rggbhlac"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; There is a test suite, but it wants network namespaces and sudo.
      #:tests? #f
      #:make-flags
      #~(let ((out #$output))
          (list (string-append "CC=" #$(cc-for-target))
                "HOSTCC=gcc"
                (string-append "BASH_COMPDIR=" out
                               "/etc/bash_completion.d")
                (string-append "LIBDIR=" out "/lib")
                (string-append "HDRDIR=" out "/include")
                (string-append "SBINDIR=" out "/sbin")
                (string-append "CONFDIR=" out "/etc")
                (string-append "MANDIR=" out "/share/man")))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key (configure-flags #~'()) #:allow-other-keys)
              ;; The configure script does not understand some of the
              ;; default options of gnu-build-system.
              (setenv "PKG_CONFIG" #$(pkg-config-for-target))
              (apply invoke "./configure"
                     "--prefix" #$output
                     configure-flags)))
          (add-before 'install 'pre-install
            (lambda _
              ;; Don't attempt to create /var/lib/arpd.
              (substitute* "Makefile"
                (("^.*ARPDDIR.*$") "")))))))
    (inputs
     (list bdb iptables libmnl))
    (native-inputs
     (list bison flex pkg-config))
    ;; For tests.
    ;; ("libmnl" ,libmnl)
    ;; ("util-linux" ,util-linux)
    (home-page
     "https://wiki.linuxfoundation.org/networking/iproute2")
    (synopsis
     "Utilities for controlling TCP/IP networking and traffic in Linux")
    (description
     "Iproute2 is a collection of utilities for controlling TCP/IP networking
and traffic with the Linux kernel.  The most important of these are
@command{ip}, which configures IPv4 and IPv6, and @command{tc} for traffic
control.

Most network configuration manuals still refer to ifconfig and route as the
primary network configuration tools, but ifconfig is known to behave
inadequately in modern network environments, and both should be deprecated.")
    (license license:gpl2+)))

(define-public net-tools
  ;; XXX: This package is basically unmaintained, but it provides a few
  ;; commands not yet provided by Inetutils, such as 'route', so we have to
  ;; live with it.
  (let ((commit "479bb4a7e11a4084e2935c0a576388f92469225b")
        (revision "0"))
    (package
      (name "net-tools")
      (version (string-append "1.60-" revision "." (string-take commit 7)))
      (source (origin
               (method url-fetch)
               (uri (string-append "https://sourceforge.net/code-snapshots/git/"
                                   "n/ne/net-tools/code.git/net-tools-code-"
                                   commit ".zip"))
               (file-name (string-append name "-" version ".zip"))
               (sha256
                (base32
                 "0hz9fda9d78spp774b6rr5xaxav7cm4h0qcpxf70rvdbrf6qx7vy"))))
      (home-page "https://net-tools.sourceforge.net/")
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (mkdir-p (string-append out "/bin"))
                 (mkdir-p (string-append out "/sbin"))

                 ;; Pretend we have everything...
                 (system "yes | make config")

                 ;; ... except for the things we don't have.
                 ;; HAVE_AFDECnet requires libdnet, which we don't have.
                 ;; HAVE_HWSTRIP and HAVE_HWTR require kernel headers
                 ;; that have been removed.
                 ;; XXX SELINUX and AFBLUETOOTH are removed for now, but we should
                 ;; think about adding them later.
                 (substitute* '("config.make" "config.h")
                   (("^.*HAVE_(AFDECnet|HWSTRIP|HWTR|SELINUX|AFBLUETOOTH)[ =]1.*$")
                    ""))
                 #t)))
           (add-after 'install 'remove-redundant-commands
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Remove commands and man pages redundant with Inetutils.
               (let* ((out (assoc-ref outputs "out"))
                      (dup (append-map (cut find-files out <>)
                                       '("^hostname"
                                         "^(yp|nis|dns)?domainname"))))
                 (for-each delete-file dup)
                 #t))))
         ;; Binaries that depend on libnet-tools.a don't declare that
         ;; dependency, making it parallel-unsafe.
         #:parallel-build? #f

         #:tests? #f                                ; no test suite
         #:make-flags (let ((out (assoc-ref %outputs "out")))
                        (list ,(string-append "CC=" (cc-for-target))
                              (string-append "BASEDIR=" out)
                              (string-append "INSTALLNLSDIR=" out "/share/locale")
                              (string-append "mandir=/share/man")))))
      (native-inputs `(("gettext" ,gettext-minimal)
                       ("unzip" ,unzip)))
      (supported-systems (remove target-hurd? %supported-systems))
      (synopsis "Tools for controlling the network subsystem in Linux")
      (description
       "This package includes the important tools for controlling the network
subsystem of the Linux kernel.  This includes arp, ifconfig, netstat, rarp and
route.  Additionally, this package contains utilities relating to particular
network hardware types (plipconfig, slattach) and advanced aspects of IP
configuration (iptunnel, ipmaddr).")
      (license license:gpl2+))))

(define-public libcap
  (package
    (name "libcap")
    (version "2.64")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/libs/security/linux-privs/"
                    "libcap2/libcap-" version ".tar.xz"))
              (sha256
               (base32
                "04qy0z6yhlljb29xxcb2srbdnymcrhsi28wrc705z3861cgmwin8"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))
           #:test-target "test"
           #:make-flags
           #~(list "lib=lib"
                   (string-append "prefix=" #$output)
                   (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
                   "RAISE_SETFCAP=no"
                   ;; Tell the makefile to use TARGET-gcc and friends
                   ;; when cross-compiling.
                   #$@(if (%current-target-system)
                          `((string-append "CROSS_COMPILE="
                                           ,(%current-target-system) "-")
                            "BUILD_CC=gcc")
                          '()))))
    (native-inputs (list perl))
    (supported-systems (remove target-hurd? %supported-systems))
    (home-page "https://sites.google.com/site/fullycapable/")
    (synopsis "Library for working with POSIX capabilities")
    (description
     "Libcap2 provides a programming interface to POSIX capabilities on
Linux-based operating systems.")

    ;; License is BSD-3 or GPLv2, at the user's choice.
    (license (list license:bsd-3 license:gpl2))))

(define-public bridge-utils
  (package
    (name "bridge-utils")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kernel.org/linux/utils/net/"
                           "bridge-utils/bridge-utils-" version ".tar.xz"))
       (sha256
        (base32 "03p6cmhm6cqhnfsfa1wv7frhb361y12daf7gr1h5qh51y7j8n7d6"))))
    (build-system gnu-build-system)

    ;; The tarball lacks all the generated files.
    (native-inputs (list autoconf automake))
    (arguments
     '(#:tests? #f))                    ; no 'check' target

    (home-page "https://wiki.linuxfoundation.org/networking/bridge")
    (synopsis "Manipulate Ethernet bridges")
    (description
     "Utilities for Linux's Ethernet bridging facilities.  A bridge is a way
to connect two Ethernet segments together in a protocol independent way.
Packets are forwarded based on Ethernet address, rather than IP address (like
a router).  Since forwarding is done at Layer 2, all protocols can go
transparently through a bridge.")
    (license license:gpl2+)))

(define-public libnl
  (package
    (name "libnl")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/thom311/libnl/releases/download/"
                    "libnl" (string-join (string-split version #\.) "_")
                    "/libnl-" version ".tar.gz"))
              (sha256
               (base32
                "1yh5bqmkivd78x378x34gzb28lvykn6b9k3hgvvpdnj5jpn3689m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)
       ("libnl3-doc"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/thom311/libnl/releases/download/libnl"
                 (string-join (string-split version #\.) "_")
                 "/libnl-doc-" version ".tar.gz"))
           (sha256
            (base32 "19p5y8q3cm5wqvamqc4s5syxnnkvzxy3gw8ivxk6fv9ybn8jm35h"))))))
    (outputs `("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             (let ((dest (string-append (assoc-ref outputs "doc")
                                        "/share/doc/libnl")))
               (mkdir-p dest)
               (invoke "tar" "xf" (assoc-ref
                                   (or native-inputs inputs)
                                   "libnl3-doc")
                       "--strip-components=1" "-C" dest)))))))
    (home-page "https://www.infradead.org/~tgr/libnl/")
    (synopsis "NetLink protocol library suite")
    (description
     "The libnl suite is a collection of libraries providing APIs to netlink
protocol based Linux kernel interfaces.  Netlink is an IPC mechanism primarily
between the kernel and user space processes.  It was designed to be a more
flexible successor to ioctl to provide mainly networking related kernel
configuration and monitoring interfaces.")

    ;; Most files are LGPLv2.1-only, but some are GPLv2-only (like
    ;; 'nl-addr-add.c'), so the result is GPLv2-only.
    (license license:gpl2)))

;; libnl python extensions used to be outputs of libnl. However, as
;; cross-compiling python extensions is currently broken, create separate
;; packages for libnl python extensions.
(define (libnl-python-package python)
  (let ((name (string-append "libnl-" python)))
    (package
      (inherit libnl)
      (name name)
      (inputs `(,@(cond
                   ((string=? python "python2")
                    `(("python-2" ,python-2)))
                   ((string=? python "python3")
                    `(("python-3" ,python-3))))
                ("libxcrypt" ,libxcrypt))) ;required by Python.h
      (propagated-inputs (list libnl))
      (outputs '("out"))
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1))
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (define (python-inst python)
                 (invoke python "setup.py" "build")
                 (invoke python "setup.py" "install"
                         (string-append "--prefix="
                                        (assoc-ref %outputs "out")))
                 (invoke python "setup.py" "clean"))
               (setenv "LDFLAGS" (format #f "-Wl,-rpath=~a/lib"
                                         (assoc-ref inputs "libnl")))
               (with-directory-excursion "./python" (python-inst ,python))
               #t))))))))

(define-public libnl-python2 (libnl-python-package "python2"))
(define-public libnl-python3 (libnl-python-package "python3"))

(define-public iw
  (package
    (name "iw")
    (version "5.19")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/network/iw/iw-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0kjdrihc7ibnjdpjqkq8sv0kmmvavgsww78cpjgbnlyx8zlvnrzi"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libnl))
    (arguments
     (list #:make-flags
           #~(list
            (string-append "CC=" #$(cc-for-target))
            (string-append "PKG_CONFIG=" #$(pkg-config-for-target))
            (string-append "PREFIX=" (assoc-ref %outputs "out")))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))   ; no configure script
    (home-page "https://wireless.wiki.kernel.org/")
    (synopsis "Tool for configuring wireless devices")
    (description
     "iw is a new nl80211 based CLI configuration utility for wireless
devices.  It replaces @code{iwconfig}, which is deprecated.")
    (license license:isc)))

(define-public powertop
  (package
    (name "powertop")
    (version "2.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fenrus75/powertop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10vbk4vplmzp3p1mhwnhj81g6i5xvam9pdvmiy6cmd0xvnmdyy77"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "LDFLAGS=-pthread")
       #:phases
       (modify-phases %standard-phases
         ;; TODO: Patch some hardcoded "wlan0" in calibrate/calibrate.cpp to
         ;; allow calibrating the network interface in Guix System.
         (add-after 'unpack 'patch-absolute-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((kmod (assoc-ref inputs "kmod")))
               (substitute* (find-files "src" "\\.cpp$")
                 ;; Give the right 'modprobe' file name so that essential
                 ;; modules such as msr.ko can be loaded.
                 (("/sbin/modprobe") (string-append kmod "/bin/modprobe"))
                 ;; These programs are only needed to calibrate, so using
                 ;; relative file names avoids adding extra inputs.  When they
                 ;; are missing powertop gracefully handles it.
                 (("/usr/s?bin/(hciconfig|hcitool|xset)" _ command)
                  command))))))))
    (native-inputs
     (list autoconf
           autoconf-archive
           automake
           gettext-minimal
           libtool
           pkg-config))
    (inputs
     (list kmod libnl ncurses pciutils zlib))
    (home-page "https://01.org/powertop/")
    (synopsis "Analyze power consumption on x86-based laptops")
    (description
     "PowerTOP is a Linux tool to diagnose issues with power consumption and
power management.  In addition to being a diagnostic tool, PowerTOP also has
an interactive mode where the user can experiment various power management
settings for cases where the operating system has not enabled these
settings.")
    (license license:gpl2)))

(define-public aumix
  (package
    (name "aumix")
    (version "2.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://web.archive.org/web/20211201184543/"
                    "http://www.jpj.net/~trevor/aumix/releases/aumix-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0a8fwyxnc5qdxff8sl2sfsbnvgh6pkij4yafiln0fxgg6bal7knj"))))
    (build-system gnu-build-system)
    (arguments
     ;; Allow compilation with GCC 10.
     '(#:configure-flags '("CFLAGS=-O2 -g -fcommon")))
    (inputs (list ncurses))
    (home-page (string-append "https://web.archive.org/web/20211201184543/"
                              "http://jpj.net/~trevor/aumix.html"))
    (synopsis "Audio mixer for X and the console")
    (description
     "Aumix adjusts an audio mixer from X, the console, a terminal,
the command line or a script.")
    (license license:gpl2+)))

(define-public iotop-python
  ;; The last release is from 2013 and gives a misleading CONFIG_TASK_DELAY_ACCT
  ;; error on ‘newer’ kernels.
  (let ((revision "0")
        (commit "a14256a3ff74eeee59493ac088561f1bafab85a7"))
    (package
      (name "iotop-python")
      (version (git-version "0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://repo.or.cz/iotop.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00h5p8fk1zi237q8mqds8apqbis9iw0yih1hl0pr63dsnyzmmrpw"))))
      (build-system python-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-build-with-python3
             (lambda _
               (substitute* "setup.py"
                 (("itervalues") "values")))))
         ;; There are currently no checks in the package.
         #:tests? #f))
      (native-inputs (list python))
      (home-page "http://guichaz.free.fr/iotop/")
      (synopsis
       "Displays the IO activity of running processes")
      (description
       "Iotop is a Python program with a top like user interface to show the
processes currently causing I/O.")
      (license license:gpl2+))))

(define-public iotop
  (package
    (name "iotop")
    (version "1.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Tomas-M/iotop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0raac1sp46mls6p0a4yzzc8iqxkw0da4zq54cwjdg4wcy8g43glv"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:tests? #f                  ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)))) ; no configure script
    (native-inputs (list pkg-config))
    (inputs (list ncurses))
    (home-page "https://github.com/Tomas-M/iotop")
    (synopsis "Interactive @command{top}-like input/output activity monitor")
    (description
     "iotop identifies which processes and threads are most responsible for
@acronym{I/O, input/output} activity such as disc reads and writes.  It sorts
them in a live, interactive table overview similar to that of the well-known
@command{top}.

This information makes it much easier for an administrator to see which tasks
are blocking others and adjust their priority (using @command{ionice}) or stop
or kill them altogether.")
    (license license:gpl2+)))

(define-public fuse
  (package
    (name "fuse")
    (version "3.10.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/libfuse/libfuse/releases/"
                              "download/fuse-" version
                              "/fuse-" version ".tar.xz"))
              (sha256
               (base32
                "0rlnnsiw614qcmgy8xz67044gqc1pbvvf2yxjv44lh27bm487qmj"))))
    (build-system meson-build-system)
    (inputs
     (list bash-minimal util-linux))
    (arguments
     `(#:configure-flags
       ,#~(list
           (string-append "-Dudevrulesdir=" #$output "/udev/rules.d")
           "-Duseroot=false")
       #:tests? #f
       #:phases
       ,#~(modify-phases %standard-phases
            (add-after 'unpack 'set-file-names
              (lambda* (#:key inputs #:allow-other-keys)
                ;; libfuse calls out to mount(8) and umount(8).  Make sure
                ;; it refers to the right ones.
                (substitute* '("lib/mount_util.c")
                  (("/bin/(u?)mount" _ maybe-u)
                   (search-input-file inputs
                                      (string-append "bin/"
                                                     maybe-u "mount"))))
                (substitute* '("util/mount.fuse.c")
                  (("/bin/sh")
                   (search-input-file inputs "/bin/sh")))

                ;; This hack leads libfuse to search for 'fusermount' in
                ;; $PATH, where it may find a setuid-root binary, instead of
                ;; trying solely $out/sbin/fusermount and failing because
                ;; it's not setuid.
                (substitute* "lib/meson.build"
                  (("-DFUSERMOUNT_DIR=[[:graph:]]+")
                   "-DFUSERMOUNT_DIR=\"/var/empty\"'"))))
            (add-after 'unpack 'fix-install
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* '("util/meson.build")
                  (("install_helper.sh") "true"))
                (substitute* '("util/meson.build")
                  (("fuseconf_path = .*")
                   "fuseconf_path = '/etc/fuse.conf'"))))
            (add-before 'configure 'set-paths
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((dummy-init.d
                       (string-append (getcwd) "/etc/init.d")))
                  (setenv "MOUNT_FUSE_PATH"
                          (string-append #$output "/sbin"))
                  (setenv "UDEV_RULES_PATH"
                          (string-append #$output
                                         "/lib/udev/rules.d"))))))))
    (supported-systems (remove target-hurd? %supported-systems))
    (home-page "https://github.com/libfuse/libfuse")
    (synopsis "Support file systems implemented in user space")
    (description
     "As a consequence of its monolithic design, file system code for Linux
normally goes into the kernel itself---which is not only a robustness issue,
but also an impediment to system extensibility.  FUSE, for \"file systems in
user space\", is a kernel module and user-space library that tries to address
part of this problem by allowing users to run file system implementations as
user-space processes.")
    (license (list license:lgpl2.1      ; library
                   license:gpl2+))))    ; command-line utilities

(define-public fuse-2
  (package
    (inherit fuse)
    (name "fuse")
    (version "2.9.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libfuse/libfuse/releases/"
                           "download/fuse-" version
                           "/fuse-" version ".tar.gz"))
       (sha256
        (base32 "1ddlq6kzxilccgbvxjfx80jx6kamgw4sv49phks2zhlcc1frvrnh"))
       (patches (search-patches "fuse-overlapping-headers.patch"
                                "fuse-glibc-2.34.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake gettext-minimal libtool))
    (arguments
     '(#:configure-flags (list (string-append "MOUNT_FUSE_PATH="
                                              (assoc-ref %outputs "out")
                                              "/sbin")
                               (string-append "INIT_D_PATH="
                                              (assoc-ref %outputs "out")
                                              "/etc/init.d")

                               ;; The rule makes /dev/fuse 666.
                               (string-append "UDEV_RULES_PATH="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'force-bootstrap
           (lambda _
             ;; Force a bootstrap to make the patch changes effective.
             (delete-file "configure")))
         (add-before 'build 'set-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             ;; libfuse calls out to mount(8) and umount(8).  Make sure
             ;; it refers to the right ones.
             (substitute* '("lib/mount_util.c" "util/mount_util.c")
               (("/bin/(u?)mount" _ maybe-u)
                (search-input-file inputs
                                   (string-append "bin/"
                                                  maybe-u "mount"))))
             (substitute* '("util/mount.fuse.c")
               (("/bin/sh")
                (search-input-file inputs "/bin/sh")))

             ;; This hack leads libfuse to search for 'fusermount' in
             ;; $PATH, where it may find a setuid-root binary, instead of
             ;; trying solely $out/sbin/fusermount and failing because
             ;; it's not setuid.
             (substitute* "lib/Makefile"
               (("-DFUSERMOUNT_DIR=[[:graph:]]+")
                "-DFUSERMOUNT_DIR=\\\"/var/empty\\\"")))))))))

(define-public fuse-for-appimage
  (package
    (inherit fuse)
    (name "fuse")
    (version "3.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libfuse/libfuse/releases/"
                           "download/fuse-" version "/fuse-" version ".tar.gz"))
       (sha256
        (base32 "11yfl2w2a445hllyzlakq97n32g06972vxpmh7lpbclnj9fhb5zp"))))
    (arguments
     (substitute-keyword-arguments (package-arguments fuse)
       ((#:configure-flags original-flags #~(list))
        #~(append #$original-flags '("--default-library=static")))))))

(define-public unionfs-fuse
  (package
    (name "unionfs-fuse")
    (version "3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/rpodgorny/unionfs-fuse")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wl5m5qnwf3s1792xphr35pb80sx8ybaqi3n3ddi5vvk3qjc4iws"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            ;; The epitome of ‘I tried’: run the 2 trivial tests that don't rely
            ;; on the fuse kernel module being loaded.  All others would fail.
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "../source/test_all.py" "-k" "test_help")))))))
    (native-inputs
     (list pkg-config

           ;; Only for the test ‘suite’.
           python
           python-pytest))
    (inputs
     (list fuse))
    (home-page "https://github.com/rpodgorny/unionfs-fuse")
    (synopsis "User-space union file system")
    (description
     "UnionFS-FUSE is a flexible union file system implementation in user
space, using the FUSE library.  Mounting a union file system allows you to
\"aggregate\" the contents of several directories into a single mount point.
UnionFS-FUSE additionally supports copy-on-write.")
    (license license:bsd-3)))

(define fuse-static
  (package (inherit fuse)
    (name "fuse-static")
    (source
     (origin
       (inherit (package-source fuse))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Normally libfuse invokes mount(8) so that /etc/mtab is updated.
            ;; Change calls to 'mtab_needs_update' to 0 so that it doesn't do
            ;; that, allowing us to remove the dependency on util-linux
            ;; (something that is useful in initrds.)
            (substitute* "lib/mount_util.c"
              (("mtab_needs_update[[:blank:]]*\\([a-z_]+\\)") "0")
              (("/bin/") ""))))))
    (arguments
     (substitute-keyword-arguments (package-arguments fuse)
       ((#:configure-flags flags '())
        #~(cons "-Ddefault_library=static"
                #$flags))))))

(define-public unionfs-fuse/static
  (package (inherit unionfs-fuse)
    (synopsis "User-space union file system (statically linked)")
    (name (string-append (package-name unionfs-fuse) "-static"))
    (source
     (origin
       (inherit (package-source unionfs-fuse))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Add -ldl to the libraries, because libfuse.a needs that.
            (substitute* "src/CMakeLists.txt"
              (("target_link_libraries(.*)\\)" _ libs)
               (string-append "target_link_libraries"
                              libs " dl)")))))))
    (arguments
     (substitute-keyword-arguments (package-arguments unionfs-fuse)
       ((#:configure-flags flags #~'())
        #~(cons "-DCMAKE_EXE_LINKER_FLAGS=-static" #$flags))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'post-install
              (lambda _
                ;; By default, 'unionfs' keeps references to
                ;; $glibc/share/locale and similar stuff.  Remove them.
                (remove-store-references (string-append #$output "/bin/unionfs"))
                ;; 'unionfsctl' has references to glibc as well.  Since
                ;; we don't need it, remove it.
                (delete-file (string-append #$output "/bin/unionfsctl"))))))))
    (inputs (list fuse-static))))

(define-public sshfs
  (package
    (name "sshfs")
    (version "3.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libfuse/sshfs/releases/"
                                  "download/sshfs-" version "/sshfs-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "169fkilv060afgp9h7hvsbkg02bd1y77kmx06x0s7q1cvmxww62j"))))
    (build-system meson-build-system)
    (arguments
     ;; XXX: tests are skipped: FUSE kernel module does not seem to be loaded
     '(#:tests? #f))
    (inputs
     (list fuse glib))
    (native-inputs
     (list pkg-config
           ;; man page
           python-docutils))
    (home-page "https://github.com/libfuse/sshfs")
    (synopsis "Mount remote file systems over SSH")
    (description
     "This is a file system client based on the SSH File Transfer Protocol.
Since most SSH servers already support this protocol it is very easy to set
up: on the server side there's nothing to do; on the client side mounting the
file system is as easy as logging into the server with an SSH client.")
    (license license:gpl2+)))

(define-public sshfs-fuse
  (package (inherit sshfs)
    (name "sshfs-fuse")
    (properties `((superseded . ,sshfs)))))

(define-public archivemount
  (package
    (name "archivemount")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.cybernoia.de/software/archivemount/"
                           "archivemount-" version ".tar.gz"))
       (sha256
        (base32 "1cy5b6qril9c3ry6fv7ir87s8iyy5vxxmbyx90dm86fbra0vjaf5"))))
    (build-system gnu-build-system)
    (inputs (list fuse-2 libarchive))
    (native-inputs (list pkg-config))
    (home-page "https://www.cybernoia.de/software/archivemount.html")
    (synopsis "Tool for mounting archive files with FUSE")
    (description "archivemount is a FUSE-based file system for Unix variants,
including Linux.  Its purpose is to mount archives (i.e. tar, tar.gz, etc.) to a
mount point where it can be read from or written to as with any other file
system.  This makes accessing the contents of the archive, which may be
compressed, transparent to other programs, without decompressing them.")
    (license license:lgpl2.0+)))

(define-public numactl
  (package
    (name "numactl")
    (version "2.0.16")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/numactl/numactl/releases/download/v"
                    version "/numactl-" version ".tar.gz"))
              (sha256
               (base32
                "1j67wx3383fwqbvhg4nwqf72vpdgimmrvkpn3b9s2xzr7a4jy90v"))))
    (build-system gnu-build-system)
    (arguments
     `(,@(if (target-riscv64?)
           `(#:make-flags (list "LDFLAGS=-latomic"))
           '())
       ;; There's a 'test' target, but it requires NUMA support in the kernel
       ;; to run, which we can't assume to have.
       #:tests? #f))
    (home-page "https://github.com/numactl/numactl")
    (synopsis "Tools for non-uniform memory access (NUMA) machines")
    (description
     "NUMA stands for Non-Uniform Memory Access, in other words a system whose
memory is not all in one place.  The @command{numactl} program allows you to
run your application program on specific CPUs and memory nodes.  It does this
by supplying a NUMA memory policy to the operating system before running your
program.

The package contains other commands, such as @command{numastat},
@command{memhog}, and @command{numademo} which provides a quick overview of
NUMA performance on your system.")
    (license (list license:gpl2                   ;programs
                   license:lgpl2.1))))            ;library

(define-public kbd-neo
  (package
    (name "kbd-neo")
    (version "2486")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://svn.neo-layout.org/!svn/bc/"
                           version "/linux/console/neo.map"))
       (file-name (string-append name "-" version ".map"))
       (sha256
        (base32
         "19mfrd31vzpsjiwc7pshxm0b0sz5dd17xrz6k079cy4im1vf0r4g"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((out (string-append %output "/share/keymaps"))
                         (source (assoc-ref %build-inputs "source")))
                     (mkdir-p out)
                     (copy-file source (string-append out "/neo.map"))
                     #t))))
    (home-page "https://neo-layout.org")
    (synopsis "Neo2 console layout")
    (description
     "Kbd-neo provides the Neo2 keyboard layout for use with
@command{loadkeys(1)} from @code{kbd(4)}.")
    ;; The file is located in an svn directory, the entire content of
    ;; the directory is licensed as GPL3.
    (license license:gpl3+)))

(define-public kbd
  (package
    (name "kbd")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/utils/kbd/kbd-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "10s608i4blprgy9nynlid0hglfdrrgln6wwjs9rhjf56hwilbpyc"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* '("src/unicode_start" "src/unicode_stop")
                    ;; Assume the Coreutils are in $PATH.
                    (("/usr/bin/tty")
                     "tty"))))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bzip2 (assoc-ref inputs "bzip2"))
                   (gzip  (assoc-ref inputs "gzip"))
                   (xz    (assoc-ref inputs "xz"))
                   (zstd  (assoc-ref inputs "zstd")))
               (substitute* "src/libkbdfile/kbdfile.c"
                 (("bzip2") (string-append bzip2 "/bin/bzip2"))
                 (("gzip") (string-append gzip "/bin/gzip"))
                 (("xz -d") (string-append xz "/bin/xz -d"))
                 (("zstd") (string-append zstd "/bin/zstd"))))))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure these programs find their comrades.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (prog)
                           (wrap-program (string-append bin "/" prog)
                             `("PATH" ":" prefix (,bin))))
                         '("unicode_start" "unicode_stop"))))))))
    (native-inputs
     (list autoconf pkg-config))
    (inputs
     `(("bash" ,bash-minimal) ; for wrap-program
       ("bzip2" ,bzip2)
       ("gzip" ,gzip)
       ("pam" ,linux-pam)
       ("xz" ,xz)
       ("zstd" ,zstd)))
    (native-search-paths
     (list (search-path-specification
            (variable "LOADKEYS_KEYMAP_PATH")
            ;; Append ‘/**’ to recursively search all directories.  One can then
            ;; run (for example) ‘loadkeys en-latin9’ instead of having to find
            ;; and type ‘i386/colemak/en-latin9’ on a mislabelled keyboard.
            (files (list "share/keymaps/**")))))
    (home-page "https://kbd-project.org/")
    (synopsis "Linux keyboard utilities and keyboard maps")
    (description
     "This package contains keytable files and keyboard utilities compatible
for systems using the Linux kernel.  This includes commands such as
@code{loadkeys}, @code{setfont}, @code{kbdinfo}, and @code{chvt}.")
    (license license:gpl2+)))

(define-public loadkeys-static
  (package
    (inherit kbd)
    (name "loadkeys-static")
    (arguments
     (substitute-keyword-arguments (package-arguments kbd)
       ((#:configure-flags flags ''())
        `(append '("LDFLAGS=-static" "--disable-shared" "--disable-nls"
                   "--disable-vlock"              ;so we don't need libpam
                   "--disable-libkeymap")
                 ,flags))
       ((#:make-flags flags ''())
        `(cons "LDFLAGS=-all-static -lrt -lpthread" ,flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; The binary keeps references to gzip, among other things,
                 ;; which we don't need in the initrd, so strip references.
                 (remove-store-references "src/loadkeys")

                 (install-file "src/loadkeys"
                               (string-append out "/bin")))))
           (delete 'post-install)))
       ((#:strip-flags _ '())
        ''("--strip-all"))
       ((#:allowed-references _ '())
        '())))

    (synopsis "Statically-linked @command{loadkeys} program")

    ;; This package is meant to be used internally in the initrd so don't
    ;; expose it.
    (properties '((hidden? . #t)))))

(define-public inotify-tools
  (package
    (name "inotify-tools")
    (version "3.22.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rvoicilas/inotify-tools")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j9j8k5zjp8m4cl53zjncnll9z4dnvzr4ygmfcjk0ci81i59b18i"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (home-page "https://github.com/rvoicilas/inotify-tools/wiki")
    (synopsis "Monitor file accesses")
    (description
     "The inotify-tools packages provides a C library and command-line tools
to use Linux' inotify mechanism, which allows file accesses to be monitored.")
    (license license:gpl2+)))

(define-public kmod
  (package
    (name "kmod")
    (version "29")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kernel.org/linux/utils/kernel/kmod/"
                              "kmod-" version ".tar.xz"))
              (sha256
               (base32
                "0am54mi5rk72g5q7k6l6f36gw3r9vwgjmyna43ywcjhqmakyx00b"))
              (patches (search-patches "kmod-module-directory.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~(list "--with-xz" "--with-zlib" "--with-zstd"
                                     "--disable-test-modules")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-tests
                 (lambda _
                   ;; XXX: These tests need '--sysconfdir=/etc' to pass.
                   (substitute* "Makefile.in"
                     (("testsuite/test-modprobe") "")
                     (("testsuite/test-depmod") "")
                     (("testsuite/test-blacklist") ""))))
               (add-after 'install 'install-modprobe&co
                 (lambda _
                   (for-each (lambda (tool)
                               (symlink "kmod"
                                        (string-append #$output "/bin/" tool)))
                             '("insmod" "rmmod" "lsmod" "modprobe"
                               "modinfo" "depmod")))))))
    (native-inputs (list pkg-config zstd)) ;zstd needed for tests
    (inputs (list xz zlib `(,zstd "lib")))
    (supported-systems (remove target-hurd? %supported-systems))
    (home-page "https://www.kernel.org/")
    (synopsis "Kernel module tools")
    (description "Kmod is a set of tools to handle common tasks with Linux
kernel modules like insert, remove, list, check properties, resolve
dependencies and aliases.

These tools are designed on top of libkmod, a library that is shipped with
kmod.  The aim is to be compatible with tools, configurations and indices
from the module-init-tools project.")
    (license license:gpl2+))) ; library under lgpl2.1+

(define-public earlyoom
  (package
    (name "earlyoom")
    (version "1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rfjakob/earlyoom")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xqrs6wz59ks76hdgfd4vaj010kbvllilgam2xxyn0g56kai71zi"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)            ; no configure script
           (add-before 'check 'set-go-HOME
             (lambda _
               (setenv "HOME" (getcwd))))
           (add-before 'check 'disable-failing-test
             (lambda _
               ;; This test relies on writing to /proc/%d/oom_score_adj.
               (substitute* "testsuite_cli_test.go"
                 (("TestI" match)
                  (string-append "skipped" match))))))
       #:make-flags
       #~(list (string-append "CC=" #$(cc-for-target))
               (string-append "VERSION=v" #$version)
               (string-append "PREFIX=" #$output)
               (string-append "SYSCONFDIR=" #$output "/etc")
               "GO111MODULE=off")
       #:test-target "test"))
    (native-inputs
      (append
        ;; To generate the manpage.
        (if (or (target-x86-64?) (target-x86-32?))
          (list pandoc)
          '())
        (list
          ;; For the test suite.
          cppcheck
          go)))
    (home-page "https://github.com/rfjakob/earlyoom")
    (synopsis "Simple out of memory (OOM) daemon for the Linux kernel")
    (description "Early OOM is a minimalist out of memory (OOM) daemon that
runs in user space and provides a more responsive and configurable alternative
to the in-kernel OOM killer.")
    (license license:expat)))

(define-public eudev
  (package
    (name "eudev")
    (version "3.2.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/eudev-project/eudev")
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f6lz57igi7iw2ls3fpzgw42bfznam4nf9368h7x8yf1mb737yxz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; The binary should be built to look for its rules under
      ;; /etc/udev/rules.d, which is where the udev-shepherd-service keeps
      ;; them.
      #:make-flags #~(list "udevrulesdir=/etc/udev/rules.d")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'bootstrap 'patch-file-names
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (substitute* "man/make.sh"
                (("/usr/bin/xsltproc")
                 (search-input-file (or native-inputs inputs) "/bin/xsltproc")))))
          (add-before 'bootstrap 'install-in-lib
            (lambda _
              ;; When the udev-service-type instantiates /etc, it collects
              ;; hardware files from the <package>/lib/udev/hwdb.d directories
              ;; of different packages.  Since we set sysconfdir to /etc, the
              ;; only package-dependent location we can install hwdb files is
              ;; in <package>/lib/udev/hwdb.d.  Eudev is configured to install
              ;; these files in sysconfdir, but they should be placed into
              ;; udevlibexecdir.
              (copy-file "hwdb/Makefile.am" "hwdb/files.am")
              (call-with-output-file "hwdb/Makefile.am"
                (lambda (port)
                  (format port "hardwarelibdir = $(udevlibexecdir)/hwdb.d\n")
                  (format port "include ./files.am")))
              (substitute* "hwdb/files.am"
                (("dist_udevhwdb_DATA =")
                 "dist_hardwarelib_DATA ="))
              ;; Do not install the empty udev.conf template.
              (substitute* "src/udev/Makefile.am"
                (("dist_udevconf_DATA =")
                 "dist_noinst_DATA ="))
              ;; Do not ensure that /etc/udev/rules.d exists.
              (substitute* "rules/Makefile.am"
                (("\\$\\(MKDIR_P\\) \\$\\(DESTDIR\\)\\$\\(udevconfdir\\)/rules\\.d")
                 "true"))))
          (add-after 'install 'move-static-library
            (lambda _
              (let ((source (string-append #$output "/lib/libudev.a"))
                    (target (string-append #$output:static "/lib/libudev.a")))
                (mkdir-p (dirname target))
                (link source target)
                (delete-file source)
                ;; Remove reference to the static library from the .la file
                ;; such that Libtool looks for it in the usual places.
                (substitute* (string-append #$output "/lib/libudev.la")
                  (("old_library=.*")
                   "old_library=''\n")))))
          (replace 'install
            (lambda* (#:key make-flags #:allow-other-keys #:rest args)
              ;; Although the runtime udevrulesdir is set to
              ;; /etc/udev/rules.d, the package should provide its default
              ;; rules under $libdir/udev/rules.d.
              (let* ((default-udev-rules.d (string-append #$output
                                                          "/lib/udev/rules.d"))
                     (make-flags (cons (string-append "udevrulesdir="
                                                      default-udev-rules.d)
                                       (delete "udevrulesdir=/etc/udev/rules.d"
                                               make-flags))))
                (apply (assoc-ref %standard-phases 'install)
                       `(,@args #:make-flags ,make-flags))))))
      #:configure-flags
      #~(list "--enable-manpages"
              ;; By default, autoconf uses $prefix/etc. The udev-service-type
              ;; makes sure /etc is set up with rules and hardware file
              ;; descriptions.
              "--sysconfdir=/etc")))
    (native-search-paths
     (list (search-path-specification
            (variable "UDEV_HWDB_PATH")
            (files '("lib/udev/hwdb.d")))))
    (native-inputs
     (list autoconf
           automake
           gperf
           libtool
           pkg-config
           ;; For tests.
           perl
           python-wrapper
           ;; For documentation.
           docbook-xml-4.2
           docbook-xsl
           libxslt))
    (inputs
     ;; When linked against libblkid, eudev can populate /dev/disk/by-label
     ;; and similar; it also installs the '60-persistent-storage.rules' file,
     ;; which contains the rules to do that.
     (list `(,util-linux "lib")         ;for blkid
           kmod))
    (outputs '("out" "static"))
    (home-page "https://github.com/eudev-project/eudev")
    (synopsis "Userspace device management")
    (description "Udev is a daemon which dynamically creates and removes
device nodes from /dev/, handles hotplug events and loads drivers at boot
time.")
    (license license:gpl2+)))

(define-public python-evdev
  (package
    (name "python-evdev")
    (version "1.9.1")
    (source
     (origin
       (method git-fetch)   ; no tests data in PyPi package
       (uri (git-reference
             (url "https://github.com/gvalkov/python-evdev")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09p7pj0xhv23hqlpx865klrxrf7x6sw1bdsgs6pq900jvz92pl16"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Silent tests requiring access to /dev/uinput.
      #~(list "-k" (string-append  "not test_open"
                                   " and not test_open_context"
                                   " and not test_enable_events"
                                   " and not test_abs_values"
                                   " and not test_write"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-hard-coded-directory
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "setup.py"
                (("/usr/include/linux")
                 (string-append
                  (assoc-ref inputs "kernel-headers") "/include/linux"))))))))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/gvalkov/python-evdev")
    (synopsis "Bindings to the Linux input handling subsystem")
    (description
     "Python-evdev provides bindings to the generic input event interface in
Linux.  The @code{evdev} interface serves the purpose of passing events
generated in the kernel directly to userspace through character devices that
are typically located in @file{/dev/input/}.

This package also comes with bindings to @code{uinput}, the userspace input
subsystem.  @code{uinput} allows userspace programs to create and handle input
devices that can inject events directly into the input subsystem.")
    (license license:bsd-3)))

(define-public interception-tools
  (package
    (name "interception-tools")
    (version "0.6.8")
    (home-page "https://gitlab.com/interception/linux/tools")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sbidym6ld4c8cx2ww5i54zsv8v0kygv15zq1yimz44v4my605wf"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list boost libevdev eudev yaml-cpp))
    (arguments
     `(#:tests? #f))                    ; no test suite
    (synopsis "Utilities for operating on input events of evdev devices")
    (description
     "Interception Tools provides a composable infrastructure on top of
@code{libudev} and @code{libevdev}.  The following utilities are provided:

@itemize
@item @command{udevmon} --- monitor input devices for launching tasks
@item @command{intercept} --- redirect device input events to stdout
@item @command{uinput} --- redirect device input events from stding to virtual device
@item @command{mux} --- mux streams of input events
@end itemize")
    ;; Dual-licensed under GPLv3+ or "something else" on request, per
    ;; 'README.md'.
    (license license:gpl3+)))

(define-public interception-dual-function-keys
  (package
    (name "interception-dual-function-keys")
    (version "1.4.0")
    (home-page "https://gitlab.com/interception/linux/plugins/dual-function-keys")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s49vbg3j4rwh78i8rx8qr7myql09p7b3lhrjl0p7dd98xp6ann6"))))
    (build-system gnu-build-system)
    (inputs
     (list libevdev yaml-cpp))
    (arguments
     `(#:make-flags (list ,(string-append "CC=" (cc-for-target))
                          ,(string-append "CXX=" (cxx-for-target))
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-libevdev-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libevdev (assoc-ref inputs "libevdev")))
               (substitute* "config.mk"
                 (("/usr/include/libevdev-1.0")
                  (string-append libevdev "/include/libevdev-1.0")))
               #t)))
         ;; No configure script
         (delete 'configure))
       ;; No tests are included.
       #:tests? #f))
    (synopsis "Tap for one key, hold for another")
    (description
     "Dual Function Keys is a plugin for @code{interception-tools} that allows
one to send arbitrary keycodes when a given key is tapped or held.")
    (license license:expat)))

(define-public lvm2
  (package
    (name "lvm2")
    (version "2.03.22")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://sourceware.org/ftp/lvm2/LVM2."
                                        version ".tgz")
                         (string-append "ftp://sources.redhat.com/pub/lvm2/releases/LVM2."
                                        version ".tgz")))
              (sha256
               (base32
                "0z6w6bknhwh1n3qfkb5ij6x57q3wjf28lq3l8kh7rkhsplinjnjc"))
              (patches (search-patches "lvm2-no-systemd.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (use-modules (guix build utils))

                  ;; Honor sysconfdir.
                  (substitute* "make.tmpl.in"
                    (("^confdir = .*$")
                     "confdir = @sysconfdir@\n")
                    (("DEFAULT_SYS_DIR = @DEFAULT_SYS_DIR@")
                     "DEFAULT_SYS_DIR = @sysconfdir@"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list config
           pkg-config procps))                       ;tests use 'pgrep'
    (inputs
     `(("libaio" ,libaio)
       ("udev" ,eudev)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'update-config
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (install-file (search-input-file
                             (or native-inputs inputs) "/bin/config.sub")
                           "autoconf")
             (install-file (search-input-file
                             (or native-inputs inputs) "/bin/config.guess")
                           "autoconf")))
         (add-after 'configure 'set-makefile-shell
           (lambda _
             ;; Use 'sh', not 'bash', so that '. lib/utils.sh' works as
             ;; expected.
             (setenv "SHELL" (which "sh"))

             ;; Replace /bin/sh with the right file name.
             (patch-makefile-SHELL "make.tmpl"))))

       #:configure-flags (list (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc/lvm")
                               "--enable-udev_sync"
                               "--enable-udev_rules"
                               "--enable-pkgconfig"
                               "--enable-cmdlib"
                               "--enable-dmeventd" ; Requires '--enable-cmdlib'.

                               ;; Make sure programs such as 'dmsetup' can
                               ;; find libdevmapper.so.
                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib/device-mapper")
                               ;; This is needed when cross-compiling.
                               ,@(if (%current-target-system)
                                     '("ac_cv_func_malloc_0_nonnull=yes"
                                       "ac_cv_func_realloc_0_nonnull=yes")
                                     '()))

       ;; The tests use 'mknod', which requires root access.
       #:tests? #f))
    (supported-systems (remove target-hurd? %supported-systems))
    (home-page "https://sourceware.org/lvm2/")
    (synopsis "Logical volume management for Linux")
    (description
     "LVM2 is the logical volume management tool set for Linux-based systems.
This package includes the user-space libraries and tools, including the device
mapper.  Kernel components are part of Linux-libre.")

    ;; Libraries (liblvm2, libdevmapper) are LGPLv2.1.
    ;; Command-line tools are GPLv2.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public (libdevmapper-propagated-inputs)
  (list eudev))

(define-public lvm2-static
  (package
    (inherit lvm2)
    (name "lvm2-static")

    (inputs `(,@(package-inputs lvm2)
              ("udev:static" ,eudev "static")))

    (arguments
     (substitute-keyword-arguments (package-arguments lvm2)
       ((#:configure-flags flags '())
        ;; LVM2 doesn't use Libtool, hence the custom option.
        `(append '("--enable-static_link")
                 ;; Building dmeventd statically is complicated due to a
                 ;; requirement on libdevmapper.a, which is being phased out
                 ;; in favor of libdevice-mapper.a, which in turn is is not
                 ;; easily made available at dmeventd build time.  Just ignore
                 ;; it until the situation improves.
                 (delete "--enable-dmeventd" ,flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'configure 'adjust-Makefile
              (lambda _
                ;; These fixes are related to the upstream libdm->device_mapper
                ;; migration and will hopefully be fixed upstream in due time.
                (substitute* "tools/Makefile.in"
                  ;; This variable is empty in a static configuration and causes
                  ;; an erroneous GCC command line.
                  (("-L\\$\\(interfacebuilddir\\)") "")
                  ;; Remove obsolete reference to libdevmapper.a.
                  (("-ldevmapper") ""))
                #t))
            (add-after 'install 'adjust-pkgconfig
              ;; The static eudev is missing its pkg config file, and I am not
              ;; rebuilding it at this point.
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* (string-append #$output "/lib/pkgconfig/devmapper.pc")
                  (("Requires.private: .*") "")
                  (("Libs.private:")
                   (format #f "Libs.private: -L~a -ludev"
                           (dirname (search-input-file inputs "lib/libudev.a")))))))))))
  (synopsis "Logical volume management for Linux (statically linked)")))

(define-public thin-provisioning-tools
  (package
    (name "thin-provisioning-tools")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jthornber/thin-provisioning-tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01wl8c0cjbx1smbhj8dx6av5bnw5775m58gasc3vqwvsj0s9hq19"))))
    (build-system gnu-build-system)
    (arguments
     ;; Doesn't build with --enable-testing due to a function name collision
     ;; with glibc. Fixed upstream. TODO: Enable tests when 0.9.0 is released.
     `(#:tests? #f))
    (native-inputs
     `(("automake" ,automake)
       ("autoreconf" ,autoconf)))
    (inputs
     (list boost expat libaio))
    (synopsis "Tools for manipulating the metadata of device-mapper targets")
    (description "A suite of tools for manipulating the metadata of the
dm-thin, dm-cache and dm-era device-mapper targets.")
    (home-page "https://github.com/jthornber/thin-provisioning-tools")
    (license license:gpl3+)))

(define-public watchdogd
  (package
    (name "watchdogd")
    (version "3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/troglobit/watchdogd")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05f7igavzimfgbh39fflbkmpya12r854n03dkyimwashcqwchx8f"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool pkg-config))
    (inputs (list libconfuse libite libuev))
    (synopsis "Advanced system & process supervisor for Linux")
    (description "This package provides an advanced monitor of critical system
resources, supervises the heartbeat of processes, records deadline
transgressions, and initiates a controlled reset if needed.")
    (home-page "https://troglobit.com/projects/watchdogd/")
    (license license:isc)))

(define-public wireless-tools
  (package
    (name "wireless-tools")
    (version "30.pre9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.hpl.hp.com/personal/Jean_Tourrilhes/"
                           "Linux/wireless_tools." version ".tar.gz"))
       (sha256
        (base32 "0qscyd44jmhs4k32ggp107hlym1pcyjzihiai48xs7xzib4wbndb"))
       (snippet
        #~(begin
            ;; Remove the older header files that are not free software.
            (for-each (lambda (n)
                        (delete-file (format #f "wireless.~a.h" n)))
                      '(10 11 12 13 14 15 16 17 18 19 20))))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "INSTALL_MAN=" #$output "/share/man")
                   (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
                   "BUILD_STATIC=")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda* (#:key target #:allow-other-keys)
                   (when target
                     ;; Cross-compilation: use the cross tools.
                     (substitute* (find-files "." "Makefile")
                       (("CC = .*$")
                        (string-append "CC = " target "-gcc\n"))
                       (("AR = .*$")
                        (string-append "AR = " target "-ar\n"))
                       (("RANLIB = .*$")
                        (string-append "RANLIB = " target "-ranlib\n")))))))
           #:tests? #f))
    (synopsis "Tools for manipulating Linux Wireless Extensions")
    (description "Wireless Tools are used to manipulate the now-deprecated
Linux Wireless Extensions; consider using @code{iw} instead.  The Wireless
Extension was an interface allowing you to set Wireless LAN specific
parameters and get the specific stats.  It is deprecated in favor the nl80211
interface.")
    (home-page "https://www.hpl.hp.com/personal/Jean_Tourrilhes/Linux/Tools.html")
    ;; wireless.21.h and wireless.22.h are distributed under lgpl2.1+, the
    ;; other files are distributed under gpl2.
    (license (list license:gpl2 license:lgpl2.1+))))

(define-public crda
  (package
    (name "crda")
    (version "4.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/linux/kernel/git/mcgrof/crda.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ajh8zx84p15y9wawh764zawniwn059iw9m32v56ajvkz9xbnkp2"))
       (patches (search-patches "crda-optional-gcrypt.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'gzip-deterministically
                 (lambda _
                   (substitute* "Makefile"
                     (("gzip" command)
                      (string-append command " --no-name")))))
               #$@(if (%current-target-system)
                     #~((add-after 'unpack 'fix-pkg-config
                         (lambda* (#:key target #:allow-other-keys)
                           (substitute* "Makefile"
                             (("pkg-config" command)
                              (string-append target "-" command))))))
                     #~())
               (add-before 'build 'patch-Makefile
                 (lambda _
                   (substitute* "Makefile"
                     (("ldconfig") "true"))))
               (add-before 'build 'set-regulatory-db-file-name
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   ;; Tell CRDA where to find our database.
                   (let ((regdb (assoc-ref (or native-inputs inputs)
                                           "wireless-regdb")))
                     (substitute* "crda.c"
                       (("\"/lib/crda/regulatory.bin\"")
                        (string-append "\"" regdb
                                       "/lib/crda/regulatory.bin\"")))))))
       #:test-target "verify"
       #:make-flags
       #~(list
          (string-append "CC=" #$(cc-for-target))
          "V=1"

          ;; Disable signature-checking on 'regulatory.bin'.
          ;; The reason is that this simplifies maintenance
          ;; on our side (no need to manage a distro key
          ;; pair), and we can guarantee integrity of
          ;; 'regulatory.bin' by other means anyway, such as
          ;; 'guix gc --verify'.  See
          ;; <https://wireless.wiki.kernel.org/en/developers/regulatory/wireless-regdb>
          ;; for a discssion.
          "USE_OPENSSL=0"

          (string-append "PREFIX=" #$output)
          (string-append "SBINDIR=" #$output "/sbin/")
          (string-append "UDEV_RULE_DIR="
                         #$output "/lib/udev/rules.d")
          (string-append "LDFLAGS=-Wl,-rpath="
                         #$output "/lib -L.")
          (string-append "REG_BIN="
                         #$(this-package-native-input "wireless-regdb")
                         "/lib/crda/regulatory.bin")
          "all_noverify")))
    (native-inputs (list pkg-config wireless-regdb))
    (inputs (list libnl))
    (home-page
     "https://wireless.wiki.kernel.org/en/developers/Regulatory/CRDA")
    (synopsis "@acronym{CRDA, Central Regulatory Domain Agent} for WiFi")
    (description
     "The @acronym{CRDA, Central Regulatory Domain Agent} acts as the udev
helper for communication between the kernel Linux and user space for regulatory
compliance.")
    (license license:copyleft-next)))

(define-public wireless-regdb
  (package
    (name "wireless-regdb")
    (version "2023.05.03")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/network/wireless-regdb/"
                    "wireless-regdb-" version ".tar.xz"))
              (sha256
               (base32 "04lc9jp8zxhyqxvkhrm637sswi2xm48jw8jnp3iflnknnf5d0m7j"))

              ;; We're building 'regulatory.bin' by ourselves.
              (snippet '(begin
                          (delete-file "regulatory.bin")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'gzip-determinism
            (lambda _
              (substitute* "Makefile"
                (("gzip") "gzip --no-name"))))
          (add-after 'unpack 'omit-signature
            (lambda _
              (substitute* "Makefile"
                ;; Signing requires a REGDB_PUBCERT and REGDB_PRIVKEY which we
                ;; don't provide (see below).  Disable it.
                ((" regulatory\\.db\\.p7s") "")
                ;; regulatory.db is built as a dependency of regulatory.db.p7s,
                ;; but ‘make install’ depends only on the latter while
                ;; installing both (and failing).  Depend on it explicitly.
                (("^install: " all) (string-append all "regulatory.db ")))))
          (delete 'configure))  ; no configure script

      ;; The 'all' target of the makefile depends on $(REGDB_CHANGED), which
      ;; is computed and can be equal to 'maintainer-clean'; when that
      ;; happens, we can end up deleting the 'regulatory.bin' file that we
      ;; just built.  Thus, build things sequentially.
      #:parallel-build? #f

      #:tests? #f                      ; no tests
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              (string-append "FIRMWARE_PATH=$(PREFIX)/lib/firmware")

              ;; Leave this empty so that db2bin.py doesn't try to sign
              ;; ‘regulatory.bin’.  This allows us to avoid managing a key
              ;; pair for the whole distribution.
              (string-append "REGDB_PRIVKEY=")
              ;; Don't generate a public key for the same reason.  These are
              ;; used as Makefile targets and can't be the empty string.
              (string-append "REGDB_PUBCERT=/dev/null")
              (string-append "REGDB_PUBKEY=/dev/null"))))
    (native-inputs
     (list python-wrapper))
    (home-page
     "https://wireless.wiki.kernel.org/en/developers/regulatory/wireless-regdb")
    (synopsis "Wireless regulatory database")
    (description
     "This package contains the wireless regulatory database for the
@acronym{CRDA, Central Regulatory Database Agent}.  The database contains
information on country-specific regulations for the wireless spectrum.")
    (license license:isc)))

(define-public lm-sensors
  (package
    (name "lm-sensors")
    (version "3.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/groeck/lm-sensors")
             (commit (string-append "V" (string-join
                                         (string-split version #\.) "-")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ipf6wjx037sqyhy0r5jh4983h216anq9l68ckn2x5c3qc4wfmzn"))
       (patches (search-patches "lm-sensors-hwmon-attrs.patch"))))
    (build-system gnu-build-system)
    (inputs (list rrdtool perl kmod gnuplot))
    (native-inputs (list pkg-config flex bison which))
    (outputs '("lib"                    ; avoid perl in closure
               "out"))
    (arguments
     `(#:tests? #f                      ; no 'check' target
       #:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "ETCDIR=" (assoc-ref %outputs "lib") "/etc")
                          (string-append "INCLUDEDIR="
                                         (assoc-ref %outputs "lib") "/include")
                          (string-append "MANDIR=" %output "/share/man")
                          (string-append "LIBDIR=" (assoc-ref %outputs "lib") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-exec-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "prog/detect/sensors-detect"
               (("`uname")
                (string-append "`" (assoc-ref inputs "coreutils")
                               "/bin/uname"))
               (("(`|\")modprobe" all open-quote)
                (string-append open-quote
                               (assoc-ref inputs "kmod")
                               "/bin/modprobe")))
             (substitute* '("prog/pwm/pwmconfig"
                            "prog/pwm/fancontrol")
               (("gnuplot")
                (search-input-file inputs "/bin/gnuplot"))
               (("cat ")
                (string-append (search-input-file inputs "/bin/cat")
                               " "))
               (("e?grep " match)
                (string-append (search-input-file inputs
                                                  (string-append
                                                   "/bin/"
                                                   (string-trim-right match)))
                               " "))
               (("sed -e")
                (string-append (search-input-file inputs "/bin/sed")
                               " -e"))
               (("cut -d")
                (string-append (search-input-file inputs "/bin/cut")
                               " -d"))
               (("sleep ")
                (string-append (search-input-file inputs "/bin/sleep")
                               " "))
               (("readlink -f")
                (string-append (search-input-file inputs "/bin/readlink")
                               " -f"))))))))
    (home-page "https://hwmon.wiki.kernel.org/lm_sensors")
    (synopsis "Utilities to read temperature/voltage/fan sensors")
    (description
     "Lm-sensors is a hardware health monitoring package for Linux.  It allows
you to access information from temperature, voltage, and fan speed sensors.
It works with most newer systems.")
    (license license:gpl2+)))

(define-public iucode-tool
  (package
    (name "iucode-tool")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.com/iucode-tool/releases"
                                  "/raw/latest/iucode-tool_" version ".tar.xz"))
              (sha256
               (base32
                "159gvf6ljgg3g4vlhyy6pyr0wz11rcyhp985vc4az58d9px8xf0j"))))
    (build-system gnu-build-system)
    (home-page "https://gitlab.com/iucode-tool/iucode-tool/wikis/home")
    (synopsis "Manipulate Intel microcode bundles")
    (description
     "@command{iucode_tool} is a utility to work with microcode packages for
Intel processors.  It can convert between formats, extract specific versions,
create a firmware image suitable for the Linux kernel, and more.")
    ;; cpuid.h is available for i686, x86_64, and ia64.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))

(define-public i2c-tools
  (package
    (name "i2c-tools")
    (version "4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://jdelvare.nerim.net/mirror/i2c-tools/i2c-tools-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1y0fphjd5ah2j886x8i175r7viq0hmx666hyca0wi4dzrm290qxk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no 'check' target
       #:make-flags
       ,#~(list (string-append "PREFIX=" #$output)
                (string-append "LDFLAGS+=-Wl,-rpath=" #$output "/lib")
                (string-append "CC=" #$(cc-for-target)))
       ;; No configure script.
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (inputs
     (list perl))
    (home-page "http://jdelvare.nerim.net/devel.html#i2ctools")
    (synopsis "I2C tools for Linux")
    (description
     "The i2c-tools package contains a heterogeneous set of I2C tools for
Linux: a bus probing tool, a chip dumper, register-level SMBus access helpers,
EEPROM decoding scripts, EEPROM programming tools, and a python module for
SMBus access.")
    (license license:gpl2+)))

(define-public i2c-tools-3
  (package
    (inherit i2c-tools)
    (name "i2c-tools")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://jdelvare.nerim.net/mirror/i2c-tools/i2c-tools-"
                    version ".tar.bz2"))
              (sha256
               (base32 "0hd4c1w8lnwc3j95h3vpd125170l1d4myspyrlpamqx6wbr6jpnv"))))
    (arguments
     (substitute-keyword-arguments (package-arguments i2c-tools)
       ((#:make-flags _)
        #~(list (string-append "prefix=" #$output)
                (string-append "CC=" #$(cc-for-target))))))))

(define-public python-smbus
  (package
    (inherit i2c-tools)
    (name "python-smbus")
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f ;; No test suite.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'change-directory
                 (lambda _ (chdir "py-smbus")))
               (add-after 'change-directory 'set-library-path
                 (lambda _
                   (substitute* "setup.py"
                     (("-L\\.\\./lib")
                      (string-append "-L" #$(this-package-input "i2c-tools")
                                     "/lib"))))))))
    (inputs (list i2c-tools))
    (native-inputs (list python-setuptools python-wheel))
    (synopsis "I2C/SMBus access for Python")
    (description "This package provides a Python library to access
@acronym{I2C, Inter-Integrated Circuit} and @acronym{SMBus, System
Management Bus} devices on Linux.")))

(define-public xsensors
  (package
    (name "xsensors")
    (version "0.70")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.linuxhardware.org/xsensors/xsensors-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1siplsfgvcxamyqf44h71jx6jdfmvhfm7mh0y1q8ps4zs6pj2zwh"))))
    (build-system gnu-build-system)
    (inputs `(("lm-sensors" ,lm-sensors "lib")
              ("gtk" ,gtk+-2)))
    (native-inputs (list pkg-config))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enable-deprecated
           (lambda _
             (substitute* "src/Makefile.in"
               (("-DGDK_DISABLE_DEPRECATED") "")
               (("-DGTK_DISABLE_DEPRECATED") ""))
             #t))
         (add-before 'configure 'remove-Werror
           (lambda _
             (substitute* '("configure" "src/Makefile.in")
               (("-Werror") ""))
             #t)))))
    (home-page "http://www.linuxhardware.org/xsensors/")
    (synopsis "Hardware health information viewer")
    (description
     "Xsensors reads data from the libsensors library regarding hardware
health such as temperature, voltage and fan speed and displays the information
in a digital read-out.")
    (license license:gpl2+)))

(define-public perf
  (package
    (name "perf")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "SHELL_PATH" (which "bash"))
             (chdir "tools/perf")

             ;; In Linux 6.10.6, 'Makefile.config' overrides PKG_CONFIG_PATH
             ;; when looking for libtraceevent and thus fails to find it.
             ;; Remove that.
             (substitute* "Makefile.config"
               (("PKG_CONFIG_PATH=[[:graph:]]+")
                ""))

             ;; This file hard-codes file system layouts for specific distros
             ;; but not for ours; address that.  With this change, one can run
             ;; "perf report --symfs=$HOME/.guix-profile" (without
             ;; "/lib/debug") and 'perf' should be able to find separate debug
             ;; info files.
             (substitute* "util/dso.c"
               (("/usr/lib/debug")
                "/lib/debug")))))
       #:make-flags (list (string-append "prefix="
                                         (assoc-ref %outputs "out"))
                          "CC=gcc"
                          "WERROR=0"

                          ;; By default, 'config/Makefile' uses lib64 on
                          ;; x86_64.  Work around that.
                          "lib=lib")
       #:tests? #f))                              ;no tests
    (native-inputs
     (list pkg-config
           bison
           flex
           ;; There are build scripts written in these languages.
           perl
           python-2
           python-3
           ;; Documentation
           docbook-xsl
           xmlto
           asciidoc))
    (inputs
     (list slang ;for the interactive TUI
           zlib
           ;; newt
           python-2                            ;'perf' links against libpython
           elfutils
           libiberty                 ;used alongside BDF for symbol demangling
           libunwind                 ;better stack walking
           libtraceevent
           numactl))                 ;for 'perf bench numa mem'
    (home-page "https://perf.wiki.kernel.org/")
    (synopsis "Linux profiling with performance counters")
    (description
     "perf is a tool suite for profiling using hardware performance counters,
with support in the Linux kernel.  perf can instrument CPU performance
counters, tracepoints, kprobes, and uprobes (dynamic tracing).  It is capable
of lightweight profiling.  This package contains the user-land tools and in
particular the @code{perf} command.")
    (license (package-license linux-libre))))

(define-public pflask
  (package
    (name "pflask")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ghedo/pflask")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jikjbhlxlqracnai3v9krzcgd2xwp0p4adw5n07yxc7b857damz"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (home-page "https://ghedo.github.io/pflask/")
    (synopsis "Simple tool for creating Linux namespace containers")
    (description "pflask is a simple tool for creating Linux namespace
containers.  It can be used for running a command or even booting an OS inside
an isolated container, created with the help of Linux namespaces.  It is
similar in functionality to chroot, although pflask provides better isolation
thanks to the use of namespaces.")
    (license license:bsd-2)))

(define-public singularity
  (package
    (name "singularity")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/singularityware/singularity/"
                                  "releases/download/" version
                                  "/singularity-" version ".tar.gz"))
              (sha256
               (base32
                "1whx0hqqi1326scgdxxxa1d94vn95mnq0drid6s8wdp84ni4d3gk"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Do not create directories in /var.
                  (substitute* "Makefile.in"
                    (("\\$\\(MAKE\\) .*install-data-hook") ""))

                  ;; The original source overrides PATH so that it points to
                  ;; /bin, /usr/local/bin, etc., which obviously doesn't work
                  ;; on Guix System.  Leave PATH unchanged so we refer to the
                  ;; installed Coreutils, grep, etc.
                  (substitute* "bin/singularity.in"
                    (("^PATH=.*" all)
                     (string-append "#" all "\n")))

                  (substitute* (find-files "libexec/cli" "\\.exec$")
                    (("\\$SINGULARITY_libexecdir/singularity/bin/([a-z]+)-suid"
                      _ program)
                     (string-append "/run/privileged/bin/singularity-"
                                    program "-helper")))

                  ;; These squashfs mount options are apparently no longer
                  ;; supported since Linux-libre 5.4.5.
                  (substitute* "src/lib/image/squashfs/mount.c"
                    (("\"errors=remount-ro\"")
                     "NULL"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--localstatedir=/var")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-references
            (lambda _
              (substitute* "libexec/cli/build.exec.in"
                (("-mksquashfs") (string-append "-" (which "mksquashfs"))))
              (substitute* (append
                            (find-files "libexec" "functions")
                            (find-files "libexec/bootstrap-scripts" ".*sh$")
                            (find-files "libexec/cli" ".*exec$"))
                (("\\| grep ")
                 (string-append "| " (which "grep") " "))
                (("egrep ")
                 (string-append (which "egrep") " "))
                ((" sed ")
                 (string-append " " (which "sed") " ")))))
          (add-after 'install 'set-PATH
            (lambda _
              ;; Have the 'singularity' and 'run-singularity' self-sufficient.
              ;; But don't override PATH, so that other tools like zcat and
              ;; tar can still be found if they are available.
              (let ((coreutils #$(this-package-input "coreutils")))
                (wrap-program (string-append #$output "/bin/singularity")
                  `("PATH" prefix (,(string-append coreutils "/bin"))))
                (substitute* (string-append #$output "/bin/run-singularity")
                  (("/usr/bin/env singularity")
                   (string-append (which "env") " "
                                  #$output "/bin/singularity")))))))))
    (inputs
     (list bash-minimal
           coreutils
           libarchive
           python-wrapper
           squashfs-tools
           zlib))
    (home-page "https://singularity.lbl.gov/")
    (synopsis "Container platform")
    (description "Singularity is a container platform supporting a number of
container image formats.  It can build SquashFS container images or import
existing Docker images.  Singularity requires kernel support for container
isolation or root privileges.")
    (license license:bsd-3)))

(define-public python-spython
  (package
    (name "python-spython")
    (version "0.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "spython" version))
       (sha256
        (base32 "0kly851k6mj7xzcybciav5d0pq5q04pzg7c5a1g712bqbxkha4ck"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Configure absolute path to singularity.
          (add-after 'unpack 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((singularity (search-input-file inputs "bin/singularity")))
                (substitute* "spython/utils/terminal.py"
                  (("software=\"singularity\"")
                   (string-append "software=\"" singularity "\"")))
                (substitute* (list "spython/utils/terminal.py"
                                   "spython/main/help.py"
                                   "spython/main/base/command.py")
                  (("\\[\"singularity\"")
                   (string-append "[\"" singularity "\"")))
                (substitute* "spython/main/execute.py"
                  (("shutil.which\\(\"singularity\"\\)")
                   (string-append "shutil.which(\"" singularity "\")"))))))
          ;; Skip tests that require network access.
          (add-before 'check 'skip-tests
            (lambda _
              (delete-file "spython/tests/test_client.py"))))))
    (inputs
     (list singularity))
    (native-inputs
     (list python-pytest
           python-pytest-runner
           python-setuptools
           python-wheel))
    (home-page "https://github.com/singularityhub/singularity-cli")
    (synopsis "Singularity Python client")
    (description "@code{python-spython} is a Python library to interact with
Singularity containers.")
    (license license:mpl2.0)))

(define-public libnvme
  (package
    (name "libnvme")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linux-nvme/libnvme.git")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "1wq8bw60l090z2kb717wyzk5wz1jrcn31ykdaa7k9pz9w79v0v67"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (outputs (list "out" "doc"))        ; docs are 80% of all output
    (arguments
     (list
      #:configure-flags
      #~(let ((doc (string-append #$output:doc "/share/doc/" #$name)))
          (list (string-append "-Dhtmldir=" doc "/html")
                (string-append "-Drstdir=" doc "/rst")
                "-Ddocs-build=true" "-Ddocs=all"))))
    (native-inputs (list pkg-config perl python python-sphinx))
    ;; libnvme.pc, libnvme-mi.pc lists these in Requires.private.
    (propagated-inputs (list dbus json-c openssl))
    (home-page "https://github.com/linux-nvme/libnvme")
    (synopsis "C Library for NVM Express on Linux")
    (description "libnvme provides type definitions for NVMe specification
structures, enumerations, and bit fields, helper functions to construct,
dispatch, and decode commands and payloads, and utilities to connect, scan,
and manage nvme devices on a Linux system.")
    (license license:lgpl2.1+)))

(define-public nvme-cli
  (package
    (name "nvme-cli")
    (version "2.5")
    (home-page "https://github.com/linux-nvme/nvme-cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32 "1jv1xir6gm86yyk5846qqkcjhc1bq103zyxf794fznyinh4nhlbg"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (outputs (list "out" "doc"))        ; docs make up ~70% of total size
    (arguments
     (list
      #:configure-flags #~(list (format #f "-Dhtmldir=~a/share/doc/~a/html"
                                        #$output:doc #$name)
                                "-Ddocs=all")))
    (native-inputs (list pkg-config))
    (inputs (list libnvme json-c zlib))
    (synopsis "NVM-Express user space tooling for Linux")
    (description "Nvme-cli is a utility to provide standards compliant tooling
for NVM-Express drives.  It was made specifically for Linux as it relies on the
IOCTLs defined by the mainline kernel driver.")
    (license license:gpl2+)))

(define-public rfkill
  (package
    (name "rfkill")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/network/"
                                  name "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0snqj5h0y991lszbigbyyqb8swj0hxajc1vfqg2scfay44231bp0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:tests? #f))
    (home-page "https://wireless.wiki.kernel.org/en/users/Documentation/rfkill")
    (synopsis "Tool for enabling and disabling wireless devices")
    (description
     "rfkill is a simple tool for accessing the rfkill device interface,
which is used to enable and disable wireless networking devices, typically
WLAN, Bluetooth and mobile broadband.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))
    ;; rfkill is part of util-linux as of 2.31.
    (properties `((superseded . ,util-linux)))))

(define-public acpi
  (package
    (name "acpi")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/acpiclient/acpiclient/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "01ahldvf0gc29dmbd5zi4rrnrw2i1ajnf30sx2vyaski3jv099fp"))))
    (build-system gnu-build-system)
    (home-page "http://acpiclient.sourceforge.net")
    (synopsis "Display information on ACPI devices")
    (description "@code{acpi} attempts to replicate the functionality of the
\"old\" @code{apm} command on ACPI systems, including battery and thermal
information.  It does not support ACPI suspending, only displays information
about ACPI devices.")
    (license license:gpl2+)))

(define-public acpid
  (package
    (name "acpid")
    (version "2.0.34")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/acpid2/acpid-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0cxkdbd087kj9ikprvvjpk0ixzqbipf2rmj6qyp7r15wzj65q29d"))))
    (build-system gnu-build-system)
    (home-page "https://sourceforge.net/projects/acpid2/")
    (synopsis "Daemon for delivering ACPI events to user-space programs")
    (description
     "acpid is designed to notify user-space programs of Advanced
Configuration and Power Interface (ACPI) events.  acpid should be started
during the system boot, and will run as a background process.  When an ACPI
event is received from the kernel, acpid will examine the list of rules
specified in /etc/acpi/events and execute the rules that match the event.")
    (license license:gpl2+)))

(define-public sysfsutils
  (package
    (name "sysfsutils")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://sourceforge/linux-diag/sysfsutils/" version "/sysfsutils-"
         version ".tar.gz"))
       (sha256
        (base32 "12i0ip11xbfcjzxz4r10cvz7mbzgq1hfcdn97w6zz7sm3wndwrg8"))))
    (build-system gnu-build-system)
    (home-page "https://linux-diag.sourceforge.net/Sysfsutils.html")
    (synopsis "System utilities based on Linux sysfs")
    (description
     "These are a set of utilities built upon sysfs, a virtual file system in
Linux kernel versions 2.5+ that exposes a system's device tree.  The package
also contains the libsysfs library.")
    ;; The library is under lgpl2.1+ (all files say "or any later version").
    ;; The rest is mostly gpl2, with a few files indicating gpl2+.
    (license (list license:gpl2 license:gpl2+ license:lgpl2.1+))))

(define-public cpufrequtils
  (let ((commit "a2f0c39d5f21596bb9f5223e895c0ff210b265d0")
        (revision "1"))
    (package
      (name "cpufrequtils")
      (version (git-version "008" revision commit ))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://git.kernel.org/pub/scm/linux/kernel/git/brodo/cpufrequtils.git")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "01n2hp6v89cilqqvqvlcprphyhnljsjclh4h1zf3b1l7ypz29lbp"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                      ; no test suite
         #:make-flags
         (let ((out (assoc-ref %outputs "out")))
           (list "PROC=false"             ; obsoleted by sysfs in Linux 2.6(!)
                 (string-append "CC=" ,(cc-for-target))
                 (string-append "LDFLAGS=-Wl,-rpath=" out "/lib")
                 "INSTALL=install"
                 (string-append "bindir=" out "/bin")
                 (string-append "sbindir=" out "/sbin")
                 (string-append "mandir=" out "/share/man")
                 (string-append "includedir=" out "/include")
                 (string-append "libdir=" out "/lib")
                 (string-append "localedir=" out "/share/locale")
                 (string-append "docdir=" out "/share/doc/" ,name)))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))         ; no configure script
      (native-inputs
       `(("gettext" ,gettext-minimal)))
      (home-page
       "http://ftp.be.debian.org/pub/linux/utils/kernel/cpufreq/cpufrequtils.html")
      (synopsis "Utilities to get and set CPU frequency on Linux")
      (description
       "The cpufrequtils suite contains utilities to retrieve CPU frequency
information, and set the CPU frequency if supported, using the cpufreq
capabilities of the Linux kernel.")
      (license license:gpl2))))

(define-public libite
  (package
    (name "libite")
    (version "2.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/troglobit/libite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i27ppb557kdc1hm7nf0f5r7sxvqma31pr57h79whl6qcp28gy4a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda* (#:key inputs #:allow-other-keys)
             ;; replace paths to the executables
             (substitute* "test/which.c"
               (("/usr/bin/which")
                (search-input-file inputs "/bin/which"))
               (("ls\"")
                (string-append
                 (search-input-file inputs "/bin/ls") "\"")))
             ;; create pidfile in /tmp instead of /var
             (substitute* "test/pidfile.c" (("/var/tmp") "/tmp")))))))
    (native-inputs (list autoconf automake libtool which))
    (synopsis "Library providing missing pieces in GNU libc")
    (description "This package provides many of the missing pieces in GNU
libc.  Most notably the string functions: strlcpy(3), strlcat(3) and the *BSD
sys/queue.h and sys/tree.h API's.")
    (home-page "https://troglobit.com/projects/libite/")
    (license license:expat)))

(define-public libuev
  (package
    (name "libuev")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/troglobit/libuev")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ih82lxfdcq179kknzdl5c8vi1l0n5j7yh68y8f6kwsrm457paf7"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool))
    (synopsis "Lightweight event loop library for epoll family APIs")
    (description "This package provides small event loop that wraps the
epoll family of APIs.")
    (home-page "https://troglobit.com/projects/libuev/")
    (license license:expat)))

(define-public libraw1394
  (package
    (name "libraw1394")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/libs/ieee1394/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0pm5b415j1qdzyw38wdv8h7ff4yx20831z1727mpsb6jc6bwdk03"))))
    (build-system gnu-build-system)
    (home-page "https://ieee1394.wiki.kernel.org/index.php/Main_Page")
    (synopsis "Interface library for the Linux IEEE1394 drivers")
    (description
     "Libraw1394 is the only supported interface to the kernel side raw1394 of
the Linux IEEE-1394 subsystem, which provides direct access to the connected
1394 buses to user space.  Through libraw1394/raw1394, applications can directly
send to and receive from other nodes without requiring a kernel driver for the
protocol in question.")
    (license license:lgpl2.1+)))

(define-public libavc1394
  (package
    (name "libavc1394")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libavc1394/libavc1394/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lsv46jdqvdx5hx92v0z2cz3yh6212pz9gk0k3513sbaa04zzcbw"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list libraw1394)) ; required by libavc1394.pc
    (home-page "https://sourceforge.net/projects/libavc1394/")
    (synopsis "AV/C protocol library for IEEE 1394")
    (description
     "Libavc1394 is a programming interface to the AV/C specification from
the 1394 Trade Association.  AV/C stands for Audio/Video Control.")
    (license license:lgpl2.1+)))

(define-public libiec61883
  (package
    (name "libiec61883")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/libs/ieee1394/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "17ph458zya2l8dr2xwqnzy195qd9swrir31g78qkgb3g4xz2rq6i"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list libraw1394)) ; required by libiec61883.pc
    (home-page "https://ieee1394.wiki.kernel.org/index.php/Main_Page")
    (synopsis "Isochronous streaming media library for IEEE 1394")
    (description
     "The libiec61883 library provides a higher level API for streaming DV,
MPEG-2 and audio over Linux IEEE 1394.")
    (license license:lgpl2.1+)))

(define-public mdadm
  (package
    (name "mdadm")
    (version "4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/raid/mdadm/mdadm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "07ghmibmbnkdy91ng87zdllzicm299l20dhs9m5bfjw6f1b22726"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   "INSTALL=install"
                   "CHECK_RUN_DIR=0"
                   ;; TODO: tell it where to find 'sendmail'
                   ;; (string-append "MAILCMD=" <???> "/sbin/sendmail")
                   (string-append "BINDIR="  #$output "/sbin")
                   (string-append "MANDIR="  #$output "/share/man")
                   (string-append "UDEVDIR=" #$output "/lib/udev"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'patch-program-paths
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   (let ((coreutils (assoc-ref (or native-inputs inputs)
                                               "coreutils")))
                     (substitute* "udev-md-raid-arrays.rules"
                       (("/usr/bin/(readlink|basename)" all program)
                        (string-append coreutils "/bin/" program))))))
               (add-before 'build 'remove-W-error
                 (lambda _
                   ;; We cannot build with -Werror on i686 due to a
                   ;; 'sign-compare' warning in util.c.
                   (substitute* "Makefile"
                     (("-Werror") ""))))
               (delete 'configure))
           ;; Tests must be run as root.
           #:tests? #f))
    (inputs
     (list eudev))
    (supported-systems (remove target-hurd? %supported-systems))
    (home-page "http://neil.brown.name/blog/mdadm")
    (synopsis "Tool for managing Linux Software RAID arrays")
    (description
     "mdadm is a tool for managing Linux Software RAID arrays.  It can create,
assemble, report on, and monitor arrays.  It can also move spares between raid
arrays when needed.")
    (license license:gpl2+)))

(define-public mdadm-static
  (package
    (inherit mdadm)
    (name "mdadm-static")
    (arguments
     (substitute-keyword-arguments (package-arguments mdadm)
       ((#:make-flags flags)
        #~(cons* "LDFLAGS = -static"
                 "CXFLAGS = -DNO_LIBUDEV"
                 #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-cruft
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out         (assoc-ref outputs "out"))
                       (precious?   (lambda (file)
                                      (member file '("." ".." "sbin"))))
                       (directories (scandir out (negate precious?))))
                  (with-directory-excursion out
                    (for-each delete-file-recursively directories)
                    (remove-store-references "sbin/mdadm")
                    (delete-file "sbin/mdmon")))))))
       ((#:modules modules %default-gnu-modules)
        `((ice-9 ftw) ,@modules))
       ((#:strip-flags _ '())
        ''("--strip-all"))                        ;strip a few extra KiB
       ((#:allowed-references _ '("out"))
        '("out"))))                               ;refer only self
    (synopsis "Statically-linked 'mdadm' command for use in an initrd")))

(define-public multipath-tools
  (package
    (name "multipath-tools")
    (version "0.9.6")
    (home-page "https://github.com/opensvc/multipath-tools")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1933iqh9r54pdl95yck0n4bw7jiiblymc964vlc1787qd4q012sz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Drop bundled valgrind headers.
                  (delete-file-recursively "third-party")
                  (substitute* '("multipathd/main.c"
                                 "libmpathutil/debug.c")
                    (("#include \"../third-party/")
                     "#include \""))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "prefix=" #$output)
                           ;; Install Udev rules below this directory, relative
                           ;; to the prefix.
                           (string-append "systemd_prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libdevmapper.h
                     (search-input-file inputs "include/libdevmapper.h"))
                    (libudev.h
                     (search-input-file inputs "include/libudev.h")))
                (substitute* "Makefile.inc"
                  (("/bin/echo") "echo")
                  (("\\$\\(prefix\\)/usr") "$(prefix)")
                  (("configdir.*:= \\$\\(prefix\\)/etc/multipath/conf.d")
                   "configdir := /etc/multipath/conf.d")
                  ;; Do not save timestamp to avoid gzip "timestamp
                  ;; out-of-range" warnings.
                  (("gzip -9") "gzip -9n"))
                (substitute* '("kpartx/Makefile" "libmultipath/Makefile")
                  (("/usr/include/libdevmapper.h") libdevmapper.h)
                  (("/usr/include/libudev.h") libudev.h)))))
          (add-after 'unpack 'skip-failing-tests
            (lambda _
              ;; This test and the module's setup() test an arbitrary block
              ;; device node name, but the build environment has none.
              (substitute* "tests/devt.c"
                (("return get_one_devt.*") "return 0;\n")
                (("cmocka_unit_test\\(test_devt2devname_devt_good\\),") ""))
              ;; The above triggers -Werror=unused-function.  Ignore it.
              (substitute* "tests/Makefile"
                (("CFLAGS \\+= " match)
                 (string-append match "-Wno-error=unused-function ")))))
          (delete 'configure)           ;no configure script
          (add-before 'build 'no-fortify-3
            (lambda _
              ;; NOTE: The check made seems to wrongly assume the
              ;; FORTIFY_SOURCE=3 is valid.  However, when compiling, warnings
              ;; are emitted from glibc, resulting in failed build.  Fix this
              ;; by forcing the usage of FORTIFY_SOURCE=2.
              (substitute* "create-config.mk"
                (("FORTIFY_SOURCE=3")
                 "FORTIFY_SOURCE=2"))
              ))
          (add-before 'build 'set-LDFLAGS
            (lambda _
              ;; Note: this cannot be passed as a make flag because that will
              ;; override the build system LDFLAGS.
              (setenv "LDFLAGS"
                      (string-append "-Wl,-rpath=" #$output "/lib")))))))

    (native-inputs
     (list perl pkg-config valgrind/pinned
           ;; For tests.
           cmocka))
    (inputs
     (list json-c
           libaio
           liburcu
           lvm2
           readline
           eudev
           ;; For libmount.
           `(,util-linux "lib")))
    (synopsis "Access block devices through multiple paths")
    (description
     "This package provides the following binaries to drive the
Linux Device Mapper multipathing driver:
@enumerate
@item @command{multipath} - Device mapper target autoconfig.
@item @command{multipathd} - Multipath daemon.
@item @command{mpathpersist} - Manages SCSI persistent reservations on
@code{dm} multipath devices.
@item @command{kpartx} - Create device maps from partition tables.
@end enumerate")
    (license (list license:gpl2+        ;main distribution
                   license:lgpl2.0+)))) ;libmpathcmd/mpath_cmd.h

(define-public libaio
  (package
    (name "libaio")
    (version "0.3.113")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://releases.pagure.org/libaio/"
                                   name "-" version ".tar.gz")))
              (sha256
               (base32
                "02r0g2vfv6kfljplm3ga93w4xw13q2wixbn9hwi7ahqdzp2x2i1c"))
              (patches (search-patches "libaio-32bit-test.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "prefix=" #$output)
                   (string-append "CC=" #$(cc-for-target)))
           #:test-target "partcheck"    ; need root for a full 'check'
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-problematic-tests
                 (lambda _
                   (with-directory-excursion "harness/cases"
                     ;; The 21.t test fails with "Expected 4096, got
                     ;; 18446744073709551605" (see:
                     ;; https://pagure.io/libaio/issue/26).
                     (rename-file "21.t" "21.t.disabled"))))
               (delete 'configure)      ; no configure script
               #$@(if (target-riscv64?)
                    #~((add-after 'unpack 'patch-test
                         (lambda* (#:key build-inputs #:allow-other-keys)
                           (invoke "patch" "-p1" "-i"
                                   #$(local-file
                                       (search-patch
                                         "libaio-riscv-test5.patch"))))))
                    #~()))))
    (native-inputs
     (if (target-riscv64?)
       (list (search-patch "libaio-riscv-test5.patch")
             patch)
       '()))
    (home-page "https://pagure.io/libaio")
    (synopsis "Linux-native asynchronous I/O access library")
    (description
     "This library enables userspace to use Linux kernel asynchronous I/O
system calls, important for the performance of databases and other advanced
applications.")
    (license license:lgpl2.1+)))

(define-public blktrace
  ;; Take a newer commit to get the latest patches.
  (let ((commit "b9ea6e507e8849f01d06aa48c0c59c5cee4820be")
        (revision "1"))
    (package
      (name "blktrace")
      (version (git-version "1.3.0" revision commit))
      (home-page
        "https://git.kernel.org/pub/scm/linux/kernel/git/axboe/blktrace.git")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32 "0a4830mlqckbhchar1xcn2w4f24bzb75bigdig5wpm2axl0zc8cq"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target))
                     (string-append "prefix=" #$output))
             #:tests? #f                    ; no tests
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)          ; no configure script
                 (add-after 'unpack 'fix-gnuplot-path
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((gnuplot (assoc-ref inputs "gnuplot")))
                       (substitute* "btt/bno_plot.py"
                         (("gnuplot %s")
                          (string-append gnuplot "/bin/gnuplot %s")))))))))
      (inputs
       `(("libaio" ,libaio)
         ("gnuplot" ,gnuplot)
         ("python" ,python-wrapper)))             ;for 'bno_plot.py'
      (synopsis "Block layer IO tracing mechanism")
      (description "Blktrace is a block layer IO tracing mechanism which provides
detailed information about request queue operations to user space.  It extracts
event traces from the kernel (via the relaying through the debug file system).")
      (license license:gpl2))))

(define-public sbc
  (package
    (name "sbc")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/bluetooth/sbc-"
                                  version ".tar.xz"))
              (patches (search-patches "sbc-fix-build-non-x86.patch"))
              (sha256
               (base32
                "1liig5856crb331dps18mp0s13zbkv7yh007zqhq97m94fcddfhc"))))
    (build-system gnu-build-system)
    (inputs
     (list libsndfile))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.kernel.org/pub/linux/bluetooth/")
    (synopsis "Bluetooth subband audio codec")
    (description
     "The SBC is a digital audio encoder and decoder used to transfer data to
Bluetooth audio output devices like headphones or loudspeakers.")
    (license license:gpl2+)))

(define-public bluez
  (package
    (name "bluez")
    (version "5.79")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/bluetooth/bluez-"
                    version ".tar.xz"))
              (sha256
               (base32
                "12pal1m4xlr8k7kxb6isv5lbaca2wc5zcgy0907wfwcz78qaar21"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--sysconfdir=/etc"
              "--localstatedir=/var"
              "--enable-library"
              "--enable-wiimote"
              "--disable-systemd"
              ;; TODO: is this needed?  Not installed by default since 5.55.
              "--enable-hid2hci"
              ;; Install dbus/udev files to the correct location.
              (string-append "--with-dbusconfdir=" #$output "/etc")
              (string-append "--with-udevdir=" #$output "/lib/udev"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Test unit/test-gatt fails unpredictably. Seems to be a timing
          ;; issue (discussion on upstream mailing list:
          ;; https://marc.info/?t=149578476300002&r=1&w=2)
          (add-before 'check 'skip-wonky-test
            (lambda _
              (substitute* "unit/test-gatt.c"
                (("tester_init\\(&argc, &argv\\);") "return 77;"))))
          (replace 'install
            (lambda* (#:key make-flags #:allow-other-keys #:rest args)
              ;; Override the sysconfdir and localstatedir locations only for
              ;; the installation phase.  Otherwise, the installation fails when
              ;; it tries to write to /etc/bluetooth and /var.
              (define make-flags*
                (append make-flags (list (string-append "sysconfdir="
                                                        #$output "/etc")
                                         (string-append "localstatedir="
                                                        #$output "/var"))))
              (apply (assoc-ref %standard-phases 'install)
                     (append args (list #:make-flags make-flags*)))))
          (add-after 'install 'post-install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((servicedir (string-append #$output
                                                "/share/dbus-1/services"))
                     (service    "obexd/src/org.bluez.obex.service")
                     (rule       (string-append
                                  #$output "/lib/udev/rules.d/97-hid2hci.rules")))
                ;; Install the obex dbus service file.
                (substitute* service
                  (("/bin/false")
                   (string-append #$output "/libexec/bluetooth/obexd")))
                (install-file service servicedir)
                ;; Fix paths in the udev rule.
                (substitute* rule
                  (("hid2hci --method")
                   (string-append #$output "/lib/udev/hid2hci --method"))
                  (("/sbin/udevadm")
                   (search-input-file inputs "/bin/udevadm")))))))))
    (native-inputs
     (list gettext-minimal
           pkg-config
           python
           python-docutils
           python-pygments))
    (inputs
     (list glib dbus eudev libical readline))
    (home-page "https://www.bluez.org/")
    (synopsis "Linux Bluetooth protocol stack")
    (description
     "BlueZ provides support for the core Bluetooth layers and protocols.  It
is flexible, efficient and uses a modular implementation.")
    (license license:gpl2+)))

(define-public fuse-exfat
  (package
    (name "fuse-exfat")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/relan/exfat/releases/download/v"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lz00q8g4590mrdqmf13ba1s9zrqq645ymgm5p9y99ad0qv22r87"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list fuse-2))
    (home-page "https://github.com/relan/exfat")
    (synopsis "Mount exFAT file systems")
    (description
     "This package provides a FUSE-based file system that provides read and
write access to exFAT devices.")
    (license license:gpl2+)))

(define-public fuseiso
  (package
    (name "fuseiso")
    (version "20070708")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/fuseiso/fuseiso/"
                                  version "/fuseiso-" version ".tar.bz2"))
              (sha256
               (base32
                "127xql52dcdhmh7s5m9xc6q39jdlj3zhbjar1j821kb6gl3jw94b"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list fuse-2 glib zlib))
    (home-page "https://sourceforge.net/projects/fuseiso/")
    (synopsis "Mount ISO file system images")
    (description
     "FuseISO is a FUSE module to mount ISO file system images (.iso, .nrg,
.bin, .mdf and .img files).  It supports plain ISO9660 Level 1 and 2, Rock
Ridge, Joliet, and zisofs.")
    (license license:gpl2)))

(define-public gpm
  (package
    (name "gpm")
    (version "1.20.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.nico.schottelius.org/software/gpm/archives/"
                    "gpm-" version ".tar.bz2"))
              (patches (search-patches "gpm-glibc-2.26.patch"))
              (sha256
               (base32
                "13d426a8h403ckpc8zyf7s2p5rql0lqbg2bv0454x0pvgbfbf4gh"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Take a patch from upstream to fix building with -fno-common,
                  ;; which is default in GCC 10:
                  ;;  https://github.com/telmich/gpm/pull/37
                  (substitute* "src/headers/daemon.h"
                    (("^time_t[[:blank:]]+last_selection_time;")
                      "extern time_t           last_selection_time;"))))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'bootstrap
                    (lambda _
                      ;; The tarball was not generated with 'make dist' so we
                      ;; need to bootstrap things ourselves.
                      (substitute* "autogen.sh"
                        (("/bin/sh") (which "sh")))
                      (invoke "./autogen.sh")
                      (patch-makefile-SHELL "Makefile.include.in")
                      #t)))

       ;; Make sure programs find libgpm.so.
       #:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (native-inputs
     (list texinfo
           bison
           flex
           autoconf
           automake
           libtool))
    (home-page "https://www.nico.schottelius.org/software/gpm/")
    (synopsis "Mouse support for the Linux console")
    (description
     "The GPM (general-purpose mouse) daemon is a mouse server for
applications running on the Linux console.  It allows users to select items
and copy/paste text in the console and in xterm.")
    (license license:gpl2+)))

(define-public btrfs-progs
  (package
    (name "btrfs-progs")
    (version "6.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/kernel/"
                                  "people/kdave/btrfs-progs/" "btrfs-progs-v"
                                  version ".tar.xz"))
              (sha256
               (base32
                "189xgsgdzqqcwm7k1ga33cj107md07w630xd88f2jvmkzh2bhnnz"))))
    (build-system gnu-build-system)
    (outputs '("out" "static")) ;static versions of the binaries in "out"
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; Without --disable-documentation, it complains about missing
         ;; python-sphinx on systems where this package isn't available
         ;; (because it requires Rust).
         #$@(if (member (%current-system)
                        (package-transitive-supported-systems
                         python-sphinx))
                '()
                '("--disable-documentation"))
         ;; The ‘Python support’ was never actually installed by previous
         ;; versions of this package, but did prevent cross-compilation.
         "--disable-python")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-makefile
                     (lambda* (#:key outputs #:allow-other-keys)
                       (substitute* "Makefile"
                         (("\\$\\(DESTDIR\\)\\$\\(udevruledir\\)")
                          (string-append (assoc-ref outputs "out")
                                         "/lib/udev/rules.d")))))
                   (add-after 'build 'build-static
                     (lambda _
                       (invoke "make" "static")))
                   (add-after 'install 'install-bash-completion
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (bashcomp (string-append out
                                                       "/etc/bash_completion.d")))
                         (mkdir-p bashcomp)
                         (copy-file "btrfs-completion"
                                    (string-append bashcomp "/btrfs")))))
                   (add-after 'install 'install-static
                     (let ((staticbin (string-append (assoc-ref %outputs
                                                                "static")
                                                     "/bin")))
                       (lambda _
                         (invoke "make"
                                 (string-append "bindir=" staticbin)
                                 "install-static")))))
       #:tests? #f ;XXX: require the 'btrfs' kernel module.
       #:test-target "test"
       #:parallel-tests? #f)) ;tests fail when run in parallel
    (inputs (list e2fsprogs   ;for btrfs-convert
                  eudev
                  lzo
                  `(,util-linux "lib")            ;for libblkid and libuuid
                  `(,util-linux "static")         ;ditto
                  zlib
                  `(,zlib "static")
                  `(,zstd "lib")
                  `(,zstd "static")))
    (native-inputs
     (append
      ;; For building documentation.  Since python-sphinx requires Rust, add
      ;; it conditionally depending on such support.
      (if (supported-package? python-sphinx)
          (list python-wrapper python-sphinx python-sphinx-rtd-theme)
          '())
      (list pkg-config
            acl                                   ;for tests
            lvm2                                  ;for dmsetup
            grep                                  ;need Perl regexp support
            libaio
            liburing
            util-linux                            ;for fallocate
            which)))
    (home-page "https://btrfs.wiki.kernel.org/index.php/Main_Page")
    (synopsis "Create and manage btrfs copy-on-write file systems")
    (description
     "Btrfs is a @acronym{CoW, copy-on-write} file system for Linux
aimed at implementing advanced features while focusing on fault tolerance,
repair and easy administration.")
    ;; GPL2+: crc32.c, radix-tree.c, raid6.c, rbtree.c.
    ;; GPL2: Everything else.
    (license (list license:gpl2 license:gpl2+))))

(define-public btrfs-progs/static
  (package
    (name "btrfs-progs-static")
    (version (package-version btrfs-progs))
    (source #f)
    (build-system trivial-build-system)
    (inputs
     `(("btrfs-progs:static" ,btrfs-progs "static")))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))

         (let* ((btrfs  (assoc-ref %build-inputs "btrfs-progs:static"))
                (out    (assoc-ref %outputs "out"))
                (source (string-append btrfs "/bin/btrfs.static"))
                (target (string-append out "/bin/btrfs")))
           (mkdir-p (dirname target))
           (copy-file source target)
           (remove-store-references target)
           (chmod target #o555)
           #t))))
    (home-page (package-home-page btrfs-progs))
    (synopsis "Statically-linked btrfs command from btrfs-progs")
    (description "This package provides the statically-linked @command{btrfs}
from the btrfs-progs package.  It is meant to be used in initrds.")
    (license (package-license btrfs-progs))))

(define-public cramfs-tools
  (package
    (name "cramfs-tools")
    (home-page "https://github.com/npitre/cramfs-tools")
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32 "183rfqqyzx52q0vxicdgf0p984idh3rqkvzfb93gjvyzfhc15c0p"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; No tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "mkcramfs" (string-append out "/sbin"))
               (install-file "cramfsck" (string-append out "/sbin")))
             #t)))))
    (inputs
     (list zlib))
    (synopsis "Tools to manage Cramfs file systems")
    (description "Cramfs is a Linux file system designed to be simple, small,
and to compress things well.  It is used on a number of embedded systems and
small devices.  This version has additional features such as uncompressed
blocks and random block placement.")
    (license license:gpl2+)))

(define-public compsize
  (let ((commit "d79eacf77abe3b799387bb8a4e07a18f1f1031e8")
        (revision "0"))
    (package
      (name "compsize")
      (version (git-version "1.5" revision commit))
      (home-page "https://github.com/kilobyte/compsize")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32 "02fvgy12m50rg1snp555a1kc3cm01g2imb81cih7ikhkbjbca0d7"))
                (file-name (git-file-name name version))
                (patches
                 (search-patches "compsize-fix-btrfs-progs-compatibility.patch"))))
      (build-system gnu-build-system)
      (inputs
       (list btrfs-progs))
      (arguments
       `(#:tests? #f                      ; No tests.
         #:make-flags
         (list (string-append "CC=" ,(cc-for-target)))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "compsize" (string-append out "/bin"))
                 (install-file "compsize.8" (string-append out "/share/man/man8"))))))))
      (synopsis "Find compression type/ratio on Btrfs files")
      (description "@command{compsize} takes a list of files (given as
arguments) on a Btrfs file system and measures used compression types and
effective compression ratio, producing a report.

A directory has no extents but has a (recursive) list of files.  A non-regular
file is silently ignored.

As it makes no sense to talk about compression ratio of a partial extent,
every referenced extent is counted whole, exactly once -- no matter if you use
only a few bytes of a 1GB extent or reflink it a thousand times.  Thus, the
uncompressed size will not match the number given by @command{tar} or
@command{du}.  On the other hand, the space used should be accurate (although
obviously it can be shared with files outside our set).")
      (license license:gpl2+))))

(define-public f2fs-tools
  (package
    (name "f2fs-tools")
    (version "1.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.kernel.org/pub/scm/linux/kernel\
/git/jaegeuk/f2fs-tools.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ffws8pbpzp9730v0wy5xjas698lnbd2p7wpr2gl4mx45rsay9a5"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list `(,util-linux "lib")))       ;for libuuid
    (home-page "https://f2fs.wiki.kernel.org/")
    (synopsis "Userland tools for f2fs")
    (description
     "F2FS, the Flash-Friendly File System, is a modern file system
designed to be fast and durable on flash devices such as solid-state
disks and SD cards.  This package provides the userland utilities.")
    ;; The formatting utility, libf2fs and include/f2fs_fs.h is dual
    ;; GPL2/LGPL2.1, everything else is GPL2 only. See 'COPYING'.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public f2fs-tools-1.7
  (package
    (inherit f2fs-tools)
    (name "f2fs-tools")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (git-reference-url
                          (origin-uri (package-source f2fs-tools))))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wpzklw8smnngng0dm25jdmi7v5zfhpz02dksyxpz0a7kzzvnqqm"))))
    (inputs
     (list `(,util-linux "lib") libselinux))
    (arguments
     '(#:configure-flags '("CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-include (string-append out "/include")))
               (install-file "include/f2fs_fs.h" out-include)
               (install-file "mkfs/f2fs_format_utils.h" out-include)))))))))

(define-public f2fs-tools/static
  (static-package
   (package
     (inherit f2fs-tools)
     (name "f2fs-tools-static")
     (arguments
     `(#:configure-flags
       (let ((libuuid-static (assoc-ref %build-inputs "libuuid:static"))
             (libuuid (assoc-ref %build-inputs "libuuid")))
         (list
          (string-append "libuuid_CFLAGS=-I" libuuid "/include/uuid")
          (string-append "libuuid_LIBS=-L" libuuid-static "/lib -luuid")
          (string-append "libblkid_CFLAGS=-I" libuuid "/include/uuid "
                         "-I" libuuid "/include/blkid")
          (string-append "libblkid_LIBS=-L" libuuid-static "/lib -lblkid")))
       #:disallowed-references (,util-linux)
       #:make-flags '("LDFLAGS=-all-static")
       #:phases
       (modify-phases %standard-phases ; TODO: f2fs phases.
          (add-after 'install 'remove-store-references
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Work around bug in our util-linux.
              ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41019>.
              (remove-store-references (string-append (assoc-ref outputs "out")
                                                      "/sbin/mkfs.f2fs"))
              #t)))))
     (inputs
      `(("libuuid:static" ,util-linux "static")
        ("libuuid" ,util-linux "lib")))))) ; for include files

(define-public f2fs-fsck/static
  (package
    (name "f2fs-fsck-static")
    (version (package-version f2fs-tools/static))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))
         (let* ((f2fs-tools (assoc-ref %build-inputs "f2fs-tools-static"))
                (fsck "fsck.f2fs")
                (out (assoc-ref %outputs "out"))
                (sbin (string-append out "/sbin")))
           (mkdir-p sbin)
           (with-directory-excursion sbin
             (install-file (string-append f2fs-tools "/sbin/" fsck)
                           ".")
             (remove-store-references fsck)
             (chmod fsck #o555))
           #t))))
    (inputs
     (list f2fs-tools/static))
    (home-page (package-home-page f2fs-tools/static))
    (synopsis "Statically-linked fsck.f2fs command from f2fs-tools")
    (description "This package provides statically-linked fsck.f2fs command taken
from the f2fs-tools package.  It is meant to be used in initrds.")
    (license (package-license f2fs-tools/static))))

(define-public freefall
  (package
    (name "freefall")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'enter-subdirectory
                    (lambda _
                      (chdir "tools/laptop/freefall")
                      #t))
                  (delete 'configure)
                  (add-before 'build 'increase-timeout
                    (lambda _
                      ;; The default of 2 seconds is too low: it assumes an
                      ;; open lid and AC power without actually checking.
                      (substitute* "freefall.c"
                        (("alarm\\(2\\)") "alarm(5)"))
                      #t)))
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:tests? #f)) ;no tests
    (home-page (package-home-page linux-libre))
    (synopsis "Free-fall protection for spinning laptop hard drives")
    (description
     "Prevents shock damage to the internal spinning hard drive(s) of some
HP and Dell laptops.  When sudden movement is detected, all input/output
operations on the drive are suspended and its heads are parked on the ramp,
where they are less likely to cause damage to the spinning disc.  Requires a
drive that supports the ATA/ATAPI-7 IDLE IMMEDIATE command with unload
feature, and a laptop with an accelerometer.  It has no effect on SSDs.")
    (license license:gpl2)))


(define-public nbfc-linux
  (let ((version "0.1.7")
        (commit "4c2b75e4a875459e86a9892319889ff945e9cadf")
        (revision "0"))
    (package
      (name "nbfc-linux")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nbfc-linux/nbfc-linux")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mmyfaigrh3fd5v11a8p38km4m02qzsfx8yh72g0z405bzhqn5jk"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "CC="
                                           ,(cc-for-target))
                            (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))
         #:tests? #f
         #:phases (modify-phases %standard-phases
                    (delete 'configure))))
      (native-inputs (list pkg-config))
      (propagated-inputs (list python dmidecode))
      (synopsis "NoteBook FanControl ported to Linux")
      (description
       "This package provides a C port of NoteBook FanControl (NBFC), a fan
control service for notebooks.  It provides the same utilities with the same
interfaces as the original NBFC, although the implementation differs.")
      (home-page "https://github.com/nbfc-linux/nbfc-linux")
      (license license:gpl3+))))

(define-public thinkfan
  (package
    (name "thinkfan")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmatare/thinkfan")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07l7cxbsyvy7awa1zk0zxng60749idvsx3535iginhkqxfzij4b9"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:tests? #f                       ; no test target
      #:configure-flags
      ;; Enable reading temperatures from hard disks via S.M.A.R.T.
      ;; Upstream ‘defaults to OFF because libatasmart seems to be horribly
      ;; inefficient’.
      #~(list "-DUSE_ATASMART:BOOL=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'create-init-scripts
            ;; CMakeLists.txt relies on build-time symptoms of OpenRC and
            ;; systemd to patch and install their service files.  Fake their
            ;; presence rather than duplicating the build system below.  Leave
            ;; things like ‘/bin/kill’ because they're not worth a dependency.
            ;; The sysvinit needs manual patching, but since upstream doesn't
            ;; even provide the option to install it: don't.
            (lambda _
              (substitute* "CMakeLists.txt"
                (("pkg_check_modules\\((OPENRC|SYSTEMD) .*" _ package)
                 (format #f "option(~a_FOUND \"Faked\" ON)\n" package))
                ;; That was easy!  Now we just need to fix the destinations.
                (("/etc" directory)
                 (string-append #$output directory))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libatasmart yaml-cpp))
    (home-page "https://github.com/vmatare/thinkfan")
    (synopsis "Simple fan control program")
    (description
     "Thinkfan is a simple fan control program.  It reads temperatures,
checks them against configured limits and switches to appropriate (also
pre-configured) fan level.  It requires a working @code{thinkpad_acpi} or any
other @code{hwmon} driver that enables temperature reading and fan control
from userspace.")
    (license license:gpl3+)))

(define-public tpacpi-bat
  (package
    (name "tpacpi-bat")
    (version "3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/teleshoes/tpacpi-bat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nd8s2bqbhl0sjpflphb4l9ix71p7kvnbvkq4dg9a1v0sxafyygm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'refer-to-inputs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "tpacpi-bat"
               (("cat ")
                (string-append (search-input-file inputs "bin/cat") " "))
               ;; tpacpi-bat modprobes the acpi_call kernel module if it's not
               ;; loaded.  That's the administrator's prerogative; disable it.
               (("system \"(modprobe .*)\"" _ match)
                (format #f "die \"Please run ‘~a’ first.\\n\"" match)))))
         (delete 'configure)            ; nothing to configure
         (delete 'build)                ; nothing to build
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "tpacpi-bat" bin)

               ;; There's no man page.  Install other forms of documentation.
               (for-each (lambda (file)
                           (let ((target (string-append doc "/" file)))
                             (mkdir-p (dirname target))
                             (copy-recursively file target)))
                         (list "battery_asl" "examples" "README.md"))))))))
    (inputs
     (list coreutils-minimal
           perl))
    (home-page "https://github.com/teleshoes/tpacpi-bat")
    (synopsis "ThinkPad battery charge controller")
    (description
     "Tpacpi-bat is a command-line interface to control battery charging on
@uref{https://github.com/teleshoes/tpacpi-bat/wiki/Supported-Hardware, Lenovo
ThinkPad models released after 2011}, starting with the xx20 series.  It can
query and set the thresholds at which one or both batteries will start and stop
charging, inhibit charging batteries for a set period of time, or force them to
discharge when they otherwise would not.

This tool merely exposes ACPI calls provided by the @code{acpi_call} Linux
kernel module provided by the @code{acpi-call-linux-module} package, which must
be installed and loaded separately.  Only the original vendor firmware is
supported.")
    (license license:gpl3+)))

(define-public tmon
  (package
    (name "tmon")
    ;; Tmon's ‘VERSION = 1.0’ hasn't been touched since 2013; the code has.
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no test suite
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "INSTALL_ROOT=" #$output)
              "BINDIR=bin")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-subdirectory
            (lambda _
              (chdir "tools/thermal/tmon")))
          (add-after 'install 'install-man-page
            (lambda _
              (let ((man8 (string-append #$output "/share/man/man8")))
                (install-file "tmon.8" man8))))
          (delete 'configure))))        ; no configure script
    (inputs
     (list ncurses))
    (home-page (package-home-page linux-libre))
    (synopsis "Monitor and test the Linux thermal subsystem in real time")
    (description
     "Tmon is a tool to interact with the complex thermal subsystem of the
kernel Linux.  It helps visualize thermal relationships and real-time thermal
data, tune and test cooling devices and sensors, and collect thermal data for
further analysis.

As computers become smaller and more thermally constrained, more sensors are
added and new cooling capabilities introduced.  Thermal relationships can change
dynamically.  Their complexity grows exponentially among cooling devices, zones,
sensors, and trip points.

Linux exposes this relationship through @file{/sys/class/thermal} with a matrix
of symbolic links, trip point bindings, and device instances.  To traverse it
by hand is no trivial task: @command{tmon} aims to make it understandable.")
    (license (list license:gpl2         ; the man page
                   license:gpl2+))))    ; the actual rest

(define-public turbostat
  (package
    (name "turbostat")
    ;; XXX turbostat reports a version like ‘20.09.30’ but using it here would
    ;; make it harder to benefit from ‘free’ linux-libre package updates.
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-subdirectory
            (lambda _
              (chdir "tools/power/x86/turbostat")))
          (delete 'configure))))         ;no configure script
    (inputs
     (list libcap))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page (package-home-page linux-libre))
    (synopsis "Report x86 processor frequency and idle statistics")
    (description
     "Turbostat reports x86 processor topology, frequency, idle power state
statistics, temperature, and power consumption.  Some information is unavailable
on older processors.

It can be used to identify machines that are inefficient in terms of power usage
or idle time, report the rate of @acronym{SMI, system management interrupt}s
occurring on the system, or verify the effects of power management tuning.

@command{turbostat} reads hardware counters but doesn't write to them, so it
won't interfere with the OS or other running processes---including multiple
invocations of itself.")
    (license license:gpl2)))

(define-public ntfs-3g
  (package
    (name "ntfs-3g")
    (version "2022.10.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tuxera.com/opensource/"
                                  "ntfs-3g_ntfsprogs-" version ".tgz"))
              (sha256
               (base32
                "030pakw3h1z6p8phdbyb0hw0bb868znvrri96rg88jq7d3p3c3pj"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Install under $prefix.
                          (substitute* '("src/Makefile.in" "ntfsprogs/Makefile.in")
                            (("/sbin")
                             "@sbindir@"))))))
    (build-system gnu-build-system)
    (inputs (list util-linux ; libuuid
                  fuse-2))
    (native-inputs (list pkg-config))
    (arguments
     '(#:configure-flags (list "--disable-static"
                               "--disable-ldconfig" ;not necessary
                               "--exec-prefix=${prefix}"
                               "--with-fuse=external" ;use our own FUSE
                               "--enable-mount-helper"
                               "--enable-posix-acls"
                               "--enable-xattr-mappings")
       #:phases
       (modify-phases %standard-phases
         ;; If users install ntfs-3g, they probably want to make it the
         ;; default driver as well, so we opt for sensible defaults and link
         ;; mount.ntfs to mount.ntfs-3g.  (libmount tries to run mount.ntfs to
         ;; mount NTFS file systems.)
         (add-after 'install 'install-link
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin")))
               (symlink "mount.ntfs-3g"
                        (string-append sbin "/mount.ntfs"))))))))
    (home-page "https://www.tuxera.com/community/open-source-ntfs-3g/")
    (synopsis "Read-write access to NTFS file systems")
    (description
     "NTFS-3G provides read-write access to NTFS file systems, which are
commonly found on Microsoft Windows.  It is implemented as a FUSE file system.
The package provides additional NTFS tools.")
    (properties
     '((release-monitoring-url . "https://github.com/tuxera/ntfs-3g/releases")
       (upstream-name . "ntfs-3g_ntfsprogs")))
    (license license:gpl2+)))

(define-public ntfs-3g/static
  (static-package
   (package
     (inherit ntfs-3g)
     (name "ntfs-3g-static")
     (arguments
      (substitute-keyword-arguments (package-arguments ntfs-3g)
        ((#:configure-flags flags)
         `(append ,flags
                  (list "--enable-really-static"
                        ;; The FUSE driver isn't currently used by our initrd.
                        "--disable-ntfs-3g")))
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'unpack 'make-really-static-really-static
              (lambda _
                (substitute* "ntfsprogs/Makefile.in"
                  ((" -static") " -all-static"))))
            (delete 'install-link))))))))

(define-public ntfsfix/static
  (package
    (name "ntfsfix-static")
    (version (package-version ntfs-3g/static))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((ntfs-3g (assoc-ref %build-inputs "ntfs-3g"))
                (out     (assoc-ref %outputs "out"))
                (bin     (string-append out "/bin")))
           (install-file (string-append ntfs-3g "/bin/ntfsfix") bin)
           (with-directory-excursion bin
             (remove-store-references "ntfsfix"))))))
    (inputs
     `(("ntfs-3g" ,ntfs-3g/static)))
    (home-page (package-home-page ntfs-3g/static))
    (synopsis "Statically linked @command{ntfsfix} from ntfs-3g")
    (description
     "This package provides a statically linked @command{ntfsfix} taken
from the ntfs-3g package.  It is meant to be used in initrds.")
    (license (package-license ntfs-3g/static))))

(define-public rdma-core
  (package
    (name "rdma-core")
    (version "54.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/linux-rdma/rdma-core"
                                  "/releases/download/v" version "/rdma-core-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0q1gd4wrn7sb1l6qj6mqqlf8k4pk865b96cfnbgfxbgfs9q4jjm5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no tests

       ;; Upstream uses the "ninja" build system and encourage distros
       ;; to do the same for consistency.
       #:configure-flags (list "-GNinja"

                               ,@(if (%current-target-system)
                                     `((string-append
                                        "-DPKG_CONFIG_EXECUTABLE="
                                        (search-input-file
                                         %build-inputs
                                         (string-append "/bin/"
                                                        ,(pkg-config-for-target)))))
                                     '())
                               (string-append "-DRST2MAN_EXECUTABLE="
                                              (search-input-file
                                               %build-inputs "/bin/rst2man.py")))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "ninja"
                     "-j" (number->string (parallel-job-count)))))
         (replace 'install
           (lambda _
             (invoke "ninja" "install"))))))
    (native-inputs
     (list ninja pkg-config python-wrapper python-docutils)) ;for 'rst2man'
    (inputs
     (list libnl eudev))
    (home-page "https://github.com/linux-rdma/rdma-core")
    (synopsis "Utilities and libraries for working with RDMA devices")
    (description
     "This package provides userspace components for the InfiniBand
subsystem of the Linux kernel.  Specifically it contains userspace
libraries for the following device nodes:

@enumerate
@item @file{/dev/infiniband/uverbsX} (@code{libibverbs})
@item @file{/dev/infiniband/rdma_cm} (@code{librdmacm})
@item @file{/dev/infiniband/umadX} (@code{libibumad})
@end enumerate

The following service daemons are also provided:
@enumerate
@item @code{srp_daemon} (for the @code{ib_srp} kernel module)
@item @code{iwpmd} (for iWARP kernel providers)
@item @code{ibacm} (for InfiniBand communication management assistant)
@end enumerate")
    ;; All library code is dual licensed under GPL2 and a custom MIT
    ;; variant. The package also includes some components covered by
    ;; other licenses. Consult COPYING.md for full details.
    (license
     (list license:gpl2
           (license:x11-style "See COPYING.BSD_MIT in the distribution")
           license:bsd-2             ; Files referring to COPYING.BSD_FB
           license:cc0               ; most files in ccan/
           license:bsd-3))))         ; providers/hfi1verbs are dual GPL2/BSD-3

(define-public perftest
  (package
    (name "perftest")
    (version "4.5-0.20")
    (home-page "https://github.com/linux-rdma/perftest")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hrpzmkz1kq4jwpy6b5fl8073iy7dllcq2hfzdw6waaf5920vd64"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (inputs (list pciutils rdma-core))
    (synopsis "Open Fabrics Enterprise Distribution (OFED) Performance Tests")
    (description "This is a collection of tests written over uverbs intended
for use as a performance micro-benchmark.  The tests may be used for hardware
or software tuning as well as for functional testing.

The collection contains a set of bandwidth and latency benchmark such as:
@enumerate
@item Send        - @code{ib_send_bw} and @code{ib_send_lat}
@item RDMA Read   - @code{ib_read_bw} and @code{ib_read_lat}
@item RDMA Write  - @code{ib_write_bw} and @code{ib_wriet_lat}
@item RDMA Atomic - @code{ib_atomic_bw} and @code{ib_atomic_lat}
@item Native Ethernet (when working with MOFED2) - @code{raw_ethernet_bw}, @code{raw_ethernet_lat}
@end enumerate")
    (license license:gpl2)))

(define-public rng-tools
  (package
    (name "rng-tools")
    (home-page "https://github.com/nhorman/rng-tools")
    (version "6.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rsk8nxs0j32b9hr88qk8hv029fb1q0hcrw0mfdywvm7dn2d15gn"))
              (patches
               (search-patches "rng-tools-revert-build-randstat.patch"))
              (patch-flags '("-p0"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Disable support for various hardware entropy sources as they need
      ;; dependencies that are not yet in Guix, and would significantly
      ;; increase closure size.
      #:configure-flags #~(list "--without-nistbeacon"
                                "--without-pkcs11"
                                "--without-rtlsdr"
                                "--without-qrypt")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'disable-failing-test
            (lambda _
              (substitute* "tests/Makefile"
                ;; This test requires a hwrng, rdrand, or tpm device.
                ;; Worse, it appears to fail if that isn't sufficiently random.
                (("\\brngtestjitter\\.sh\\b") " ")))))))
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list libcap sysfsutils openssl))
    (synopsis "Random number generator daemon")
    (description
     "Monitor a hardware random number generator, and supply entropy
from that to the system kernel's @file{/dev/random} machinery.")
    ;; The source package is offered under the GPL2+, but the files
    ;; 'rngd_rdrand.c' and 'rdrand_asm.S' are only available under the GPL2.
    (license (list license:gpl2 license:gpl2+))))

(define-public cpupower
  (package
    (name "cpupower")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list "CC=gcc"
                   (string-append "DESTDIR=" #$output)
                   (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
                   "docdir=/share/doc/cpupower" ;drop default ‘packages/’
                   "confdir=$(docdir)/examples"
                   ;; The Makefile recommends the following changes.
                   "DEBUG=false"
                   "PACKAGE_BUGREPORT=bug-guix@gnu.org")
           #:tests? #f                  ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subdirectory
                 (lambda _
                   (chdir "tools/power/cpupower")))
               (delete 'configure)      ; no configure script
               (add-before 'build 'fix-makefiles
                 (lambda _
                   (substitute* "Makefile"
                     (("/usr/") "/")
                     (("/bin/(install)" _ command) command))
                   (substitute* "bench/Makefile"
                     (("\\$\\(CC\\) -o") "$(CC) $(LDFLAGS) -o")))))))
    (native-inputs (list gettext-minimal
		         which))        ;to find gettext
    (inputs (list pciutils))
    (home-page (package-home-page linux-libre))
    (synopsis "CPU frequency and voltage scaling tools for Linux")
    (description
     "cpupower is a set of user-space tools that use the cpufreq feature of the
Linux kernel to retrieve and control processor features related to power saving,
such as frequency and voltage scaling.")
    (license license:gpl2)))

(define-public x86-energy-perf-policy
  (package
    (name "x86-energy-perf-policy")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-subdirectory
           (lambda _
             (chdir "tools/power/x86/x86_energy_perf_policy")
             #t))
         (delete 'configure)
         (add-before 'build 'fix-makefile
           (lambda _
             (substitute* "Makefile" (("/usr") ""))
             #t)))
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "DESTDIR=" out)
               (string-append "LDFLAGS=-Wl,-rpath=" out "/lib")))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page (package-home-page linux-libre))
    (synopsis "Display and update Intel-CPU energy-performance policy")
    (description
     "@command{x86_energy_perf_policy} displays and updates energy-performance
policy settings specific to Intel Architecture Processors.  Settings are
accessed via Model Specific Register (MSR) updates, no matter if the Linux
cpufreq sub-system is enabled or not.")
    (license license:gpl2)))

(define-public haveged
  (package
    (name "haveged")
    (version "1.9.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jirka-h/haveged")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y1p3qhjzhpgd20xz5ffms3w1bdvpk883y38ydwsmp9mc0kzy8kz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (home-page "https://www.issihosts.com/haveged")
    (synopsis "Entropy source for the Linux random number generator")
    (description
     "haveged generates an unpredictable stream of random numbers for use by
Linux's @file{/dev/random} and @file{/dev/urandom} devices.  The kernel's
standard mechanisms for filling the entropy pool may not be sufficient for
systems with high needs or limited user interaction, such as headless servers.

@command{haveged} runs as a privileged daemon, harvesting randomness from the
indirect effects of hardware events on hidden processor state using the
@acronym{HAVEGE, HArdware Volatile Entropy Gathering and Expansion} algorithm.
It tunes itself to its environment and provides the same built-in test suite
for the output stream as used on certified hardware security devices.

The quality of the randomness produced by this algorithm has not been proven.
It is recommended to run it together with another entropy source like rngd, and
not as a replacement for it.")
    (license (list (license:non-copyleft "file://nist/mconf.h")
                   (license:non-copyleft "file://nist/packtest.c")
                   license:public-domain        ; nist/dfft.c
                   license:gpl3+))))            ; everything else

(define-public hotspot
  (package
    (name "hotspot")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KDAB/hotspot")
                    (commit (string-append "v" version))
                    ;; Include the bundled perfparser and PrefixTickLabels
                    ;; libraries, which are to be used in source form.
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04k2rvf2lgi8hp7dzqzn65fcd2lsiylwr04d44q75j0wvgbjjv1v"))))
    (build-system qt-build-system)
    (arguments
     (list
      ;; As mentioned in the option help text, the KAuth helper cannot be
      ;; installed to a custom prefix and the build fails with "file cannot
      ;; create directory: /polkit-1/actions.  Maybe need administrative"
      ;; (see: https://bugs.kde.org/show_bug.cgi?id=363678).
      #:configure-flags #~(list "-DINSTALL_KAUTH_HELPER=OFF"
                                "-DQT6_BUILD=ON")
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-perfparser
            ;; XXX: This phase is copied from qt-creator: keep them in sync!
            (lambda* (#:key inputs #:allow-other-keys)
              ;; perfparser attempts to dynamically load the demangle
              ;; libraries; use their absolute file name to avoid having to
              ;; set LD_LIBRARY_PATH.
              (let ((librustc_demangle.so
                     (with-exception-handler (lambda (ex)
                                               (if (search-error? ex)
                                                   #f
                                                   (raise-exception ex)))
                       (lambda ()
                         (search-input-file inputs "lib/librustc_demangle.so"))
                       #:unwind? #t)))
                (substitute* "3rdparty/perfparser/app/demangler.cpp"
                  (("loadDemangleLib\\(QStringLiteral\\(\"rustc_demangle\")"
                    all)
                   (if librustc_demangle.so
                       (format #f "loadDemangleLib(QStringLiteral(~s)"
                               librustc_demangle.so)
                       all))            ;no rustc_demangle; leave unchanged
                  (("loadDemangleLib\\(QStringLiteral\\(\"d_demangle\")")
                   (format #f "loadDemangleLib(QStringLiteral(~s)"
                           (search-input-file inputs
                                              "lib/libd_demangle.so")))))))
          (add-after 'unpack 'path-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/perfoutputwidgetkonsole.cpp"
                (("\"tail\"")
                 (format #f "~s" (search-input-file inputs "bin/tail"))))
              (substitute* "src/perfrecord.cpp"
                (("\"perf( )?\"" _ space)
                 (string-append "\"" (search-input-file inputs "bin/perf")
                                (or space "") "\"")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; The 'tst_models' and 'tst_callgraphgenerator' fail, with
                ;; the later seemingly requiring sudo or access to the kernel
                ;; trace points.
                (invoke "ctest" "-E"
                        (string-append
                         "("
                         (string-join
                          ;; The 'tst_models' expected output doesn't exactly
                          ;; match.
                          '("tst_models"
                            ;; The 'tst_callgraphgenerator' perf invocation
                            ;; fails when run in the build container.
                            "tst_callgraphgenerator"
                            ;; The 'tst_perfparser' test requires sudo/access
                            ;; to the kernel scheduler trace points.
                            "tst_perfparser")
                          "|")
                         ")"))))))))
    (native-inputs
     (list extra-cmake-modules
           vulkan-headers))
    (inputs
     (append
      (list coreutils-minimal
            d-demangler
            elfutils
            karchive
            kconfig
            kcoreaddons
            kddockwidgets
            kgraphviewer
            ki18n
            kio
            kiconthemes
            kitemmodels
            kitemviews
            knotifications
            kparts
            ksyntaxhighlighting
            kwindowsystem
            libxkbcommon
            perf
            qtdeclarative
            qtsvg
            solid
            threadweaver
            `(,zstd "lib"))
      (if (supported-package? rust-rustc-demangle-capi-0.1)
          (list rust-rustc-demangle-capi-0.1)
          '())))
    (home-page "https://github.com/KDAB/hotspot")
    (synopsis "Performance analysis GUI for Linux perf")
    (description "Hotspot is a standalone GUI for performance data analysis.
It aims to be similar to KCachegrind, but for data collected with
@command{perf}, a profiler for use with the kernel Linux.  Its main feature is
graphically visualizing a @file{perf.data} file.")
    (license (list license:gpl2+ license:gpl3+)))) ;dual licensed

(define-public ecryptfs-utils
  (package
    (name "ecryptfs-utils")
    (version "111")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/ecryptfs/trunk/"
                           version "/+download/ecryptfs-utils_"
                           version ".orig.tar.gz"))
       (sha256
        (base32
         "0zwq19siiwf09h7lwa7n7mgmrr8cxifp45lmwgcfr8c1gviv6b0i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-pywrap")
       #:modules (,@%default-gnu-modules
                  (ice-9 binary-ports)
                  (rnrs bytevectors)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-hardcoded-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (util-linux (assoc-ref inputs "util-linux"))
                   (cryptsetup (assoc-ref inputs "cryptsetup"))
                   (linux-pam (assoc-ref inputs "linux-pam"))
                   (lvm2 (assoc-ref inputs "lvm2")))
               (substitute* '("src/utils/ecryptfs-mount-private"
                              "src/utils/ecryptfs-umount-private"
                              "src/utils/ecryptfs-setup-private"
                              "src/utils/ecryptfs-setup-swap"
                              "src/utils/mount.ecryptfs.c"
                              "src/utils/umount.ecryptfs.c"
                              "src/pam_ecryptfs/pam_ecryptfs.c"
                              "src/desktop/ecryptfs-mount-private.desktop.in"
                              "src/desktop/ecryptfs-setup-private.desktop.in")
                 (("/bin/mount")
                  (string-append util-linux "/bin/mount"))
                 (("/bin/umount")
                  (string-append util-linux "/bin/umount"))
                 (("/sbin/mount.ecryptfs_private")
                  (string-append out "/sbin/mount.ecryptfs_private"))
                 (("/sbin/umount.ecryptfs_private")
                  (string-append out "/sbin/umount.ecryptfs_private"))
                 (("/usr/bin/ecryptfs-mount-private")
                  (string-append out "/bin/ecryptfs-mount-private"))
                 (("/usr/bin/ecryptfs-rewrite-file")
                  (string-append out "/bin/ecryptfs-rewrite-file"))
                 (("/usr/bin/ecryptfs-setup-private")
                  (string-append out "/bin/ecryptfs-setup-private"))
                 (("/sbin/cryptsetup")
                  (string-append cryptsetup "/sbin/cryptsetup"))
                 (("/sbin/unix_chkpwd")
                  (string-append linux-pam "/sbin/unix_chkpwd"))
                 (("/sbin/dmsetup")
                  (string-append lvm2 "/sbin/dmsetup"))))))
         (add-after 'install 'wrap-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (add (map (lambda (bin)
                                (dirname (search-input-file
                                          inputs (string-append "bin/" bin))))
                              ;; For simplicity, we wrap all scripts the same.
                              (list "awk" "find" "gettext" "grep" "keyctl" "ls"
                                    "lsof" "mount" "rsync" "sed" "which")))
                                        (script? (lambda (file)
                               (call-with-input-file file
                                 (lambda (port)
                                   (bytevector=? (string->utf8 "#!")
                                                 (get-bytevector-n port 2)))))))
               (for-each (lambda (file)
                           (when (script? file)
                             (wrap-program file
                               ;; '= would be better than 'suffix but break
                               ;; setuid binaries.
                               `("PATH" ":" suffix (,@add
                                                    ,(string-append bin))))))
                         (find-files bin "."))))))))
    (native-inputs
     (list intltool perl ; for pod2man
           pkg-config))
    (inputs
     (list coreutils
           cryptsetup
           findutils
           gawk
           grep
           keyutils
           linux-pam
           lsof
           lvm2
           nss
           rsync
           sed
           util-linux
           which))
    (home-page "https://ecryptfs.org/")
    (synopsis "Cryptographic file system utilities")
    (description
     "eCryptfs is a POSIX-compliant stacked cryptographic file system for Linux.
Each file's cryptographic meta-data is stored inside the file itself, along
with the encrypted contents.  This allows individual encrypted files to be
copied between hosts and still be decrypted with the proper key.  eCryptfs is a
native Linux file system, and has been part of the Linux kernel since version
2.6.19.  This package contains the userland utilities to manage it.")
    ;; The files src/key_mod/ecryptfs_key_mod_{openssl,pkcs11_helper,tspi}.c
    ;; grant additional permission to link with OpenSSL.
    (license license:gpl2+)))

(define-public libnfsidmap
  (package
    (name "libnfsidmap")
    (version "0.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://fedorapeople.org/~steved/"
                           name "/" version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "0bg2bcii424mf1bnp3fssr8jszbvhdxl7wvifm1yf6g596v8b8i5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list
                          (string-append "--with-pluginpath="
                                         (assoc-ref %outputs "out")
                                         "/lib/libnfsidmap"))))
    (native-inputs
     (list autoconf))         ; 0.27 still needs autoheader
    (home-page
     "http://www.citi.umich.edu/projects/nfsv4/crossrealm/libnfsidmap_config.html")
    (synopsis "NFSv4 support library for name/ID mapping")
    (description "Libnfsidmap is a library holding multiple methods of
mapping names to ids and visa versa, mainly for NFSv4.  It provides an
extensible array of mapping functions, currently consisting of two choices:
the default @code{nsswitch} and the experimental @code{umich_ldap}.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

(define-public module-init-tools
  (package
    (name "module-init-tools")
    (version "3.16")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/utils/kernel/module-init-tools/"
                   "module-init-tools-" version ".tar.bz2"))
             (sha256
              (base32
               "0jxnz9ahfic79rp93l5wxcbgh4pkv85mwnjlbv1gz3jawv5cvwp1"))
             (patches (search-patches "module-init-tools-moduledir.patch"))))
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: The upstream tarball lacks man pages, and building them would
     ;; require DocBook & co.  We used to use Gentoo's pre-built man pages,
     ;; but they vanished.  In the meantime, fake it.
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fake-docbook
           (lambda _
             (substitute* "Makefile.in"
               (("^DOCBOOKTOMAN.*$")
                "DOCBOOKTOMAN = true\n"))
             #t)))))
    (home-page "https://www.kernel.org/pub/linux/utils/kernel/module-init-tools/")
    (synopsis "Tools for loading and managing Linux kernel modules")
    (description
     "Tools for loading and managing Linux kernel modules, such as
@code{modprobe}, @code{insmod}, @code{lsmod}, and more.")
    (license license:gpl2+)))

(define-public mce-inject
  (let ((revision "0")                  ; no git tags :-/
        (commit "4cbe46321b4a81365ff3aafafe63967264dbfec5"))
    (package
      (name "mce-inject")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.kernel.org/pub/scm/utils/cpu/mce/mce-inject.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0gjapg2hrlxp8ssrnhvc19i3r1xpcnql7xv0zjgbv09zyha08g6z"))))
      (build-system gnu-build-system)
      (arguments
       ;; There is no test suite.  There's a test/ directory, but it just holds
       ;; example text files you could feed to the programme to crash something.
       (list #:tests? #f
             #:make-flags
             #~(list (string-append "prefix=" #$output)
                     (string-append "CC=" #$(cc-for-target)))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)    ; no configure script
                 (add-after 'install 'install-examples
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (doc (string-append out "/share/doc/" #$name))
                            (dir (string-append doc "/examples")))
                       (copy-recursively "test" dir )))))))
      (native-inputs
       (list bison flex))
      (supported-systems (list "i686-linux" "x86_64-linux"))
      (home-page
       "https://git.kernel.org/pub/scm/utils/cpu/mce/mce-inject.git/about/")
      (synopsis
       "Inject x86 @acronym{MCEs, machine-check exceptions} into Linux")
      (description
       "This simple tool injects fake @acronym{MCEs, machine-check exceptions}
into a running Linux kernel, to debug or test the kernel's @acronym{EDAC, error
detection and correction}-handling code specific to x86 and x86_64 platforms.

Real MCEs are internal CPU errors.  Handling them correctly can be important to
system stability and even prevent physical damage.  In contrast, simulated MCEs
produced by @command{mce-inject} are purely synthetic: injection happens only at
the software level, inside the kernel, and is not visible to the platform
hardware or firmware.

A convenient feature of @command{mce-inject} is that the input language used to
describe MCEs is similar to the format used in Linux panic messages, with a few
extensions.  In general, you should be able to pipe in any logged MCE panic to
simulate that same MCE.

The target kernel must have the @code{CONFIG_X86_MCE_INJECT} option enabled and
the @code{mce-inject} module loaded if it exists.")
      (license license:gpl2))))

(define-public mcelog
  (package
    (name "mcelog")
    (version "195")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/utils/cpu/mce/mcelog.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bg2bj8flybd8kzmmaaslisc6lc1fs9nbv09im6r32dq48skx5aj"))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; The checkout lacks a .git directory, breaking ‘git describe’.
           (substitute* "Makefile"
             (("\"unknown\"") (string-append "\"v" ,version "\"")))))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; The tests will only run as root on certain supported CPU models.
      #:tests? #f
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "prefix=" #$output)
              (string-append "DOCDIR=" #$output "/share/doc/"
                             #$name "-" #$version)
              "etcprefix=$(DOCDIR)/examples")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))        ; no configure script
    (native-inputs
     (list python-wrapper))             ; to generate example mcelog.conf
    (supported-systems (list "i686-linux" "x86_64-linux"))
    (home-page "https://mcelog.org/")
    (synopsis "Machine check monitor for x86 Linux systems")
    (description
     "The mcelog daemon logs memory, I/O, CPU, and other hardware errors on x86
systems running the kernel Linux.  It can also perform user-defined tasks, such
as bringing bad pages off-line, when configurable error thresholds are
exceeded.")
    (license license:gpl2)))

(define-public mtd-utils
  (package
    (name "mtd-utils")
    (version "2.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.infradead.org/pub/mtd-utils/"
                    "mtd-utils-" version ".tar.bz2"))
              (sha256
               (base32
                "0ilz8hkcyvfcnqpy25kbr8fb71x9vl28wbmw56vvd68n2byjfviq"))))
    (arguments
     (list #:configure-flags
           (if (%current-target-system) ; When cross-compiling.
             #~(list)
             #~(list "--enable-unit-tests"))

           #:parallel-tests? #f))     ;tests may fail when running in parallel
    (native-inputs
     (list cmocka pkg-config))
    (inputs
     (list acl                          ; extended attributes (xattr)
           lzo
           openssl                      ; optional crypto support
           `(,util-linux "lib")
           zlib
           `(,zstd "lib")))
    (build-system gnu-build-system)
    (synopsis "MTD Flash Storage Utilities")
    (description "This package provides utilities for testing, partitioning, etc
of flash storage.")
    (home-page "http://www.linux-mtd.infradead.org/")
    (license
      (list license:gpl2 ; Almost everything is gpl2 or gpl2+
            license:mpl1.1 ; All ftl* files
            license:expat)))) ; libiniparser

(define-public libseccomp
  (package
    (name "libseccomp")
    (version "2.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/seccomp/libseccomp/"
                                  "releases/download/v" version
                                  "/libseccomp-" version ".tar.gz"))
              (sha256
               (base32
                "1nyb3lspc5bsirpsx89vah3n54pmwlgxrwsfaxl01kq50i004afq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'skip-load-test
                    (lambda _
                      ;; This test does a native system call and fails when
                      ;; run under QEMU user-mode emulation.  Just skip it.
                      (delete-file "tests/52-basic-load.tests"))))))
    (native-inputs
     (list gperf which))
    (synopsis "Interface to Linux's seccomp syscall filtering mechanism")
    (description "The libseccomp library provides an easy to use, platform
independent, interface to the Linux Kernel's syscall filtering mechanism.  The
libseccomp API is designed to abstract away the underlying BPF based syscall
filter language and present a more conventional function-call based filtering
interface that should be familiar to, and easily adopted by, application
developers.")
    (home-page "https://github.com/seccomp/libseccomp")
    (license license:lgpl2.1)))

(define-public radeontop
  (package
    (name "radeontop")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clbr/radeontop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kwqddidr45s1blp0h8r8h1dd1p50l516yb6mb4s6zsc827xzgg3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  ;; getver.sh uses ‘git --describe’, isn't worth an extra git
                  ;; dependency, and doesn't even work on release(!) tarballs.
                  (add-after 'unpack 'report-correct-version
                    (lambda _
                      (substitute* "getver.sh"
                        (("ver=unknown")
                         (string-append "ver=" ,version)))))
                  (delete 'configure))  ; no configure script
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output))
       #:tests? #f))                    ; no tests
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list libdrm libpciaccess libxcb ncurses))
    (home-page "https://github.com/clbr/radeontop/")
    (synopsis "Usage monitor for AMD Radeon graphics")
    (description "RadeonTop monitors resource consumption on supported AMD
Radeon Graphics Processing Units (GPUs), either in real time as bar graphs on
a terminal or saved to a file for further processing.  It measures both the
activity of the GPU as a whole, which is also accurate during OpenCL
computations, as well as separate component statistics that are only meaningful
under OpenGL graphics workloads.")
    (license license:gpl3)))

(define-public efivar
  ;; XXX: 15622b7e5761f3dde3f0e42081380b2b41639a48 fixes compilation on i686.
  ;; ca48d3964d26f5e3b38d73655f19b1836b16bd2d fixes cross-compilation.
  (let ((commit "ca48d3964d26f5e3b38d73655f19b1836b16bd2d")
        (revision "0"))
    (package
      (name "efivar")
      (version (git-version "38" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rhboot/efivar")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0zsab3hcv1v53cxwkvsk09ifnwhs48a6xa3kxlwvs87yxswspvi8"))))
      (build-system gnu-build-system)
      (arguments
       (list
        ;; Tests require a UEFI system and is not detected in the chroot.
        #:tests? #f
        #:make-flags #~(list (string-append "prefix="
                                            #$output)
                             (string-append "libdir="
                                            #$output "/lib")
                             (string-append "CC="
                                            #$(cc-for-target)) "HOSTCC=gcc"
                             (string-append "LDFLAGS=-Wl,-rpath="
                                            #$output "/lib"))
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'build-deterministically
                       (lambda _
                         (substitute* "src/include/defaults.mk"
                           ;; Don't use -march=native.
                           (("-march=native")
                            ""))))
                     (delete 'configure))))
      (native-inputs (list mandoc pkg-config))
      (inputs (list popt))
      (home-page "https://github.com/rhboot/efivar")
      (synopsis "Tool and library to manipulate EFI variables")
      (description "This package provides a library and a command line
interface to the variable facility of UEFI boot firmware.")
      (license license:lgpl2.1+))))

(define-public efibootmgr
  (package
    (name "efibootmgr")
    (version "18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rhboot/efibootmgr")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j19m3dngcb7jl88ib32phbh5r7c02dhhakq8jk6z7y408c111hd"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:make-flags
      #~(append (list (string-append "prefix=" #$output)
                      (string-append "libdir=" #$output "/lib")
                      ;; EFIDIR denotes a subdirectory relative to the
                      ;; EFI System Partition where the loader will be
                      ;; installed (known as OS_VENDOR in the code).
                      ;; GRUB overrides this, as such it's only used if
                      ;; nothing else is specified on the command line.
                      "EFIDIR=gnu")
                #$(if (%current-target-system)
                      #~(list
                         (string-append "CC=" #$(cc-for-target))
                         (string-append "PKG_CONFIG=" #$(pkg-config-for-target))
                         (string-append "EXTRAINCDIRS="
                                        #$(this-package-input "efivar") "/include"))
                      #~'()))
      #:phases #~(modify-phases %standard-phases (delete 'configure))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list efivar popt))
    (home-page "https://github.com/rhboot/efibootmgr")
    (synopsis "Modify the Extensible Firmware Interface (EFI) boot manager")
    (description
     "@code{efibootmgr} is a user-space application to modify the Intel
Extensible Firmware Interface (EFI) Boot Manager.  This application can
create and destroy boot entries, change the boot order, change the next
running boot option, and more.")
    (license license:gpl2+)))

(define-public sysstat
  (package
    (name "sysstat")
    (version "12.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sysstat.github.io/sysstat-packages/"
                           "sysstat-" version ".tar.xz"))
       (sha256
        (base32 "0ssdrm3k0fclq5b5i0aznfwdkbac9l9cf88idaq3pls9lm040rjp"))))
    (build-system gnu-build-system)
    (native-inputs (list gettext-minimal))
    (arguments
     `(#:tests? #f                      ; no test suite.
       ;; Without this flag, it tries to install the man pages with group 'root'
       ;; and fails because /etc/passwd lacks an entry for the root user.
       #:configure-flags
       (list "--disable-file-attr"
             (string-append "conf_dir=" (assoc-ref %outputs "out") "/etc"))
       #:phases
       (modify-phases %standard-phases
         ;; The build process tries to create '/var/lib/sa', so we skip that
         ;; instruction.
         (add-after 'build 'skip-touching-var
           (lambda _
             (substitute* "Makefile"
               (("mkdir -p \\$\\(DESTDIR\\)\\$\\(SA_DIR\\)")
                "")))))))
    (home-page "https://sysstat.github.io")
    (synopsis "Performance monitoring tools for Linux")
    (description "The sysstat utilities are a collection of performance
monitoring tools for Linux.  These include @code{mpstat}, @code{iostat},
@code{tapestat}, @code{cifsiostat}, @code{pidstat}, @code{sar}, @code{sadc},
@code{sadf} and @code{sa}.")
    (license license:gpl2+)))

(define-public acpilight
  (package
    (name "acpilight")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/wavexx/acpilight.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r0r3nx6x6vkpal6vci0zaa1n9dfacypldf6k8fxg7919vzxdn1w"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no tests
           #:make-flags
           #~(list (string-append "prefix=" #$output)
                   (string-append "sysconfdir=" #$output "/lib"))  ;udev rule
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'patch
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Prevent reloading udev at build time
                   (substitute* "Makefile"
                     (("udevadm") "true"))
                   (substitute* "90-backlight.rules"
                     (("/bin")
                      (string-append #$(this-package-input "coreutils")
                                     "/bin"))))))))
    (inputs (list coreutils python))
    (home-page "https://gitlab.com/wavexx/acpilight")
    (synopsis "Backward-compatibile xbacklight replacement")
    (description "acpilight is a backward-compatibile replacement for
xbacklight that uses the ACPI interface to set the display brightness.  On
modern laptops acpilight can control both display and keyboard backlight
uniformly on either X11, the console or Wayland.")
    (license license:gpl3+)))

(define-public light
  (package
    (name "light")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/haikarainen/light")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1a70zcf88ifsnwll486aicjnh48zisdf8f7vi34ihw61kdadsq9s"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-udev-rules-absolute-path-bins
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "90-backlight.rules"
               (("/bin/chgrp") (which "chgrp"))
               (("/bin/chmod") (which "chmod")))
             #t))
         (add-after 'install 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file
                "90-backlight.rules" (string-append out "/lib/udev/rules.d"))
               #t))))))
    (native-inputs
     (list autoconf automake))
    (home-page "https://haikarainen.github.io/light/")
    (synopsis "GNU/Linux application to control backlights")
    (description
     "Light is a program to send commands to screen backlight controllers
under GNU/Linux.  Features include:

@itemize
@item It does not rely on X.
@item Light can automatically figure out the best controller to use, making
full use of underlying hardware.
@item It is possible to set a minimum brightness value, as some controllers
set the screen to be pitch black at a value of 0 (or higher).
@end itemize\n")
    (license license:gpl3+)))

(define-public brightnessctl
  (package
    (name "brightnessctl")
    (version "0.5.1")
    (home-page "https://github.com/Hummer12007/brightnessctl")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0immxc7almmpg80n3bdn834p3nrrz7bspl2syhb04s3lawa5y2lq"))
              (patches (search-patches "brightnessctl-elogind-support.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output)
                          (string-append "UDEVDIR=" %output "/lib/udev/rules.d/")
                          "ENABLE_SYSTEMD=1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'adjust-udev-rules
           (lambda _
             (substitute* "Makefile"
               (("INSTALL_UDEV_RULES=0") "INSTALL_UDEV_RULES=1"))
             (substitute* "90-brightnessctl.rules"
               (("/bin/") "/run/current-system/profile/bin/"))
             #t)))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list elogind))
    (synopsis "Backlight and LED brightness control")
    (description
     "This program allows you read and control device brightness.  Devices
include backlight and LEDs.  It can also preserve current brightness before
applying the operation, such as on lid close.

The appropriate permissions must be set on the backlight or LED control
interface in sysfs, which can be accomplished with the included udev rules.")
    (license license:expat)))

(define-public tlp
  (package
    (name "tlp")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linrunner/TLP")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r8r95wjb1sl5v8mli7kba2py9hc0ps915fx4lap5pg108ik9a06"))))
    (native-inputs
     (list shellcheck))
    (inputs
     `(("bash" ,bash)
       ("dbus" ,dbus)
       ("ethtool" ,ethtool)
       ("eudev" ,eudev)
       ("grep" ,grep)
       ("hdparm" ,hdparm)
       ("inetutils" ,inetutils)
       ("iw" ,iw)
       ("kmod" ,kmod)
       ("pciutils" ,pciutils)
       ("perl" ,perl)
       ("rfkill" ,rfkill)
       ("sed" ,sed)
       ("usbutils" ,usbutils)
       ("util-linux" ,util-linux)
       ("wireless-tools" ,wireless-tools)
       ,@(if (let ((system (or (%current-target-system)
                               (%current-system))))
               (or (string-prefix? "i686-" system)
                   (string-prefix? "x86_64-" system)))
             `(("x86-energy-perf-policy" ,x86-energy-perf-policy))
             '())))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The full test suite is run with "checkall" but it requires
     ;; "checkbashisms" and "perlcritic", not yet packaged in Guix.
     (list
      #:make-flags
      #~(list "TLP_CONFUSR=/etc/tlp.conf" "TLP_CONFDIR=/etc/tlp.d")
      #:test-target "shellcheck"
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ; no configure script
          (add-before 'build 'setenv
            (lambda _
              (setenv "TLP_WITH_SYSTEMD" "0")
              (setenv "TLP_NO_INIT" "1")))
          (add-before 'build 'fix-installation
            (lambda _
              ;; Stop the Makefile from trying to create system directories.
              (substitute* "Makefile"
                (("= /usr") (string-append "= " #$output))
                (("= /etc") (string-append "= " #$output "/etc"))
                (("install -d -m 755 \\$\\(_VAR\\)") "#"))))
          (add-before 'build 'fix-perl-calls
            ;; Shell scripts in "bat.d" and "func.d" call "perl".  Fix
            ;; invocation without wrapping every one of them.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (append (find-files "bat.d" ".")
                                   (find-files "func.d" "."))
                (("(^|[^/])perl" _ prefix)
                 (string-append prefix
                                (search-input-file inputs "bin/perl"))))))
          (replace 'install
            (lambda _ (invoke "make" "install-tlp" "install-man-tlp")))
          (add-after 'install 'wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                     (sbin (string-append (assoc-ref outputs "out") "/sbin"))
                     (bin-files (find-files bin ".*"))
                     (sbin-files (find-files sbin ".*")))
                (define (bin-directory input-name)
                  (let ((p (assoc-ref inputs input-name)))
                    (and p (string-append p "/bin"))))
                (define (sbin-directory input-name)
                  (string-append (assoc-ref inputs input-name) "/sbin"))
                (for-each (lambda (program)
                            (wrap-program program
                              `("PATH" ":" prefix
                                ,(append
                                  (filter-map bin-directory
                                              '("bash"
                                                "coreutils"
                                                "dbus"
                                                "eudev"
                                                "grep"
                                                "inetutils"
                                                "kmod"
                                                "perl"
                                                "sed"
                                                "usbutils"
                                                "util-linux"
                                                "x86-energy-perf-policy"))
                                  (filter-map sbin-directory
                                              '("ethtool"
                                                "hdparm"
                                                "iw"
                                                "pciutils"
                                                "rfkill"
                                                "wireless-tools"))))))
                          (append bin-files sbin-files))))))))
    (home-page "https://linrunner.de/en/tlp/tlp.html")
    (synopsis "Power management tool for Linux")
    (description "TLP is a power management tool for Linux.  It comes with
a default configuration already optimized for battery life.  Nevertheless,
TLP is customizable to fulfil system requirements.  TLP settings are applied
every time the power supply source is changed.")
    ;; 'COPYING' is a custom version that says that one file is GPLv3+ and the
    ;; rest is GPLv2+.
    (license (list license:gpl2+ license:gpl3+))))

(define-public tlpui
  (package
    (name "tlpui")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/d4nj1/TLPUI")
             (commit (string-append "tlpui-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q33p4gf5ii53wlgs61gghqxp3js5c45pn5nlibg2ygw069225r5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-absolute-locations
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((defaults.conf
                      (search-input-file inputs "/share/tlp/defaults.conf"))
                    (lspci (search-input-file inputs "/bin/lspci"))
                    (lsusb (search-input-file inputs "/bin/lsusb"))
                    (tlp-stat (search-input-file inputs "/bin/tlp-stat")))
                (with-directory-excursion "tlpui"
                  (substitute* '("file.py" "settingshelper.py" "statui.py")
                    (("\"tlp-stat\"")
                     (string-append "'" tlp-stat "'"))
                    (("/usr/share/tlp/defaults.conf")
                     defaults.conf))
                  (substitute* "ui_config_objects/gtkusblist.py"
                    (("\"lsusb\"")
                     (string-append "'" lsusb "'")))
                  (substitute* "ui_config_objects/gtkpcilist.py"
                    (("\"lspci\"")
                     (string-append "'" lspci "'")))
                  ;; Settings check if various tlp executables, lspci and
                  ;; usbutils are available.  Skip this phase since we know
                  ;; for sure they are (and it avoids patching each location).
                  (substitute* "settingshelper.py"
                    (("(command_exists = ).*" _ lead)
                     (string-append lead "True\n")))))))
          (add-after 'install 'install-desktop-file
            (lambda _
              (let ((dir (string-append #$output "/share/applications")))
                (install-file "tlpui.desktop" dir))))
          (add-after 'wrap 'wrap-gi-python
            (lambda _
              (let ((gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                (wrap-program (string-append #$output "/bin/tlpui")
                  `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           python-poetry-core
           python-pytest
           python-pyyaml))
    (inputs
     (list gtk+
           pciutils
           python-pygobject
           tlp
           usbutils))
    (propagated-inputs (list (librsvg-for-system)))
    (home-page "https://github.com/d4nj1/TLPUI")
    (synopsis "User interface for TLP written in Python")
    (description
     "The Python scripts in this project generate a GTK-UI to change
TLP configuration files easily.  It aims to protect users from setting
bad configuration and to deliver a basic overview of all the valid
configuration values.")
    (license license:gpl2+)))

(define-public lshw
  (package
    (name "lshw")
    (version "B.02.19.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.ezix.org/software/"
                                  "files/lshw-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "100gm1c6gb2hkfws22h0xhvv7nz38p49lxd1csikj8qlhyn4gcwv"))))
    (build-system gnu-build-system)
    (arguments
      `(#:phases (modify-phases %standard-phases (delete 'configure))
        #:tests? #f ; no tests
        #:make-flags
          (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (synopsis "List hardware information")
    (description
     "@command{lshw} (Hardware Lister) is a small tool to provide
detailed information on the hardware configuration of the machine.
It can report exact memory configuration, firmware version, mainboard
configuration, CPU version and speed, cache configuration, bus speed,
and more on DMI-capable x86 or EFI (IA-64) systems and on some PowerPC
machines (PowerMac G4 is known to work).")
    (home-page "https://www.ezix.org/project/wiki/HardwareLiSter")
    (license license:gpl2+)))

(define-public libmnl
  (package
    (name "libmnl")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://netfilter.org/libmnl/"
                            "libmnl-" version ".tar.bz2"))
        (sha256
         (base32
          "09851ns07399rbz0y8slrlmnw3fn1nakr8d37pxjn5gkks8rnjr7"))))
    (build-system gnu-build-system)
    (home-page "https://www.netfilter.org/projects/libmnl/")
    (synopsis "Netlink utility library")
    (description "Libmnl is a minimalistic user-space library oriented to
Netlink developers.  There are a lot of common tasks in parsing, validating,
constructing of both the Netlink header and TLVs that are repetitive and easy to
get wrong.  This library aims to provide simple helpers that allows you to
re-use code and to avoid re-inventing the wheel.")
    (properties
     '((release-monitoring-url . "https://www.netfilter.org/pub/libmnl/")))
    (license license:lgpl2.1+)))

(define-public libnftnl
  (package
    (name "libnftnl")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://netfilter.org/libnftnl/"
                           "libnftnl-" version ".tar.xz"))
       (sha256
        (base32 "1x3pqxclpxcw8x5qx0vyi7znf9xwlkqsfd9sy4cxlir1v4nfmsnf"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libmnl))
    (home-page "https://www.netfilter.org/projects/libnftnl/index.html")
    (synopsis "Netlink programming interface to the Linux nf_tables subsystem")
    (description "Libnftnl is a userspace library providing a low-level netlink
programming interface to the in-kernel nf_tables subsystem.  The library
libnftnl has been previously known as libnftables.  This library is currently
used by nftables.")
    (license license:gpl2+)))

;;; The symbol libnftl/fixed should be used when libnftnl needs fixes
;;; (security or else) and this deprecation could be removed.
(define-deprecated/public-alias libnftnl/fixed libnftnl/pinned)

;; This is used in iptables, which contributes to rust.  We're pinning this
;; variant to avoid accidental rebuilds of rust.
(define-public libnftnl/pinned
  (package (inherit libnftnl)
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://netfilter.org/libnftnl/"
                           "libnftnl-" version ".tar.bz2"))
       (sha256
        (base32 "0m82bmh8i24hwxmz7rxwxjll4904ghd2b1x1p5h8algrg6dyl5p9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libmnl))))

(define-public nftables
  (package
    (name "nftables")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://netfilter.org/nftables/nftables-"
                                 version ".tar.xz")
                  (string-append "https://www.nftables.org/projects/nftables"
                                 "/files/nftables-" version ".tar.xz")))
       (sha256
        (base32 "0l1xpwr9qfbl3bxa97v8s2lbibiz0xma9q0qi34xp0hswh6p8wwk"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags
                 '("--disable-static"
                   "--with-cli=readline"
                   "--with-json")
                 #:phases
                  (modify-phases %standard-phases
                    (add-before 'configure 'autoreconf
                      (lambda _
                        (invoke "autoreconf" "-fi"))))))
    (inputs (list gmp libmnl libnftnl readline jansson))
    (native-inputs (list pkg-config
                         bison
                         flex
                         docbook2x
                         autoconf
                         automake
                         libtool))
    (home-page "https://www.nftables.org")
    (synopsis "Userspace utility for Linux packet filtering")
    (description "nftables is the project that aims to replace the existing
{ip,ip6,arp,eb}tables framework.  Basically, this project provides a new packet
filtering framework, a new userspace utility and also a compatibility layer for
{ip,ip6}tables.  nftables is built upon the building blocks of the Netfilter
infrastructure such as the existing hooks, the connection tracking system, the
userspace queueing component and the logging subsystem.")
    (license license:gpl2)))

(define-public libnetfilter-conntrack
  (package
    (name "libnetfilter-conntrack")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://netfilter.org/projects/"
                           "libnetfilter_conntrack/files/"
                           "libnetfilter_conntrack-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1a3rnpsba64dzy97wwjrxal89wr0nf9znvag2j18nkp3kzs9vgb7"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libnfnetlink libmnl))
    (synopsis "Library for kernel connection tracking state table")
    (description "libnetfilter_conntrack is a userspace library providing a
programming interface (API) to the in-kernel connection tracking state table.
The library libnetfilter_conntrack has been previously known as
libnfnetlink_conntrack and libctnetlink.  This library is currently used by
conntrack-tools among many other applications.")
    (home-page "https://netfilter.org/projects/libnetfilter_conntrack/index.html")
    (supported-systems (filter target-linux? %supported-systems))
    (properties '((upstream-name . "libnetfilter_conntrack")))
    (license license:gpl2+)))

(define-public libnetfilter-cttimeout
  (package
    (name "libnetfilter-cttimeout")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://netfilter.org/projects/"
                           "libnetfilter_cttimeout/files/"
                           "libnetfilter_cttimeout-" version ".tar.bz2"))
       (sha256
        (base32 "0983cpyvxyycbnzqlrzml80pph2z51r6s7sxp06ciq8468pxln8b"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libmnl))
    (synopsis "Library for kernel connection tracking timeout infrastructure")
    (description "libnetfilter_cttimeout is the userspace library that
provides the programming interface to the fine-grain connection tracking
timeout infrastructure.  With this library, you can create, update and delete
timeout policies that can be attached to traffic flows.  This library is used
by conntrack-tools.")
    (home-page "https://netfilter.org/projects/libnetfilter_cttimeout/index.html")
    (supported-systems (filter target-linux? %supported-systems))
    (properties '((upstream-name . "libnetfilter_cttimeout")))
    (license license:gpl2)))

(define-public libnetfilter-cthelper
  (package
    (name "libnetfilter-cthelper")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://netfilter.org/projects/"
                           "libnetfilter_cthelper/files/"
                           "libnetfilter_cthelper-" version ".tar.bz2"))
       (sha256
        (base32 "04n95ngil5l8m8v64dfjm1dwq0wd3kf4vw1zblsrff13hxa3s1ql"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libmnl))
    (synopsis "Library for user-space connection tracking helpers")
    (description "libnetfilter_cthelper is a userspace library that provides a
programming interface to user-space connection tracking helpers.
@enumerate
@item
register new user-space connection tracking helpers
@item
unregister user-space connection tracking helpers
@item
list existing registered user-space connection tracking helpers
@end enumerate")
    (home-page "https://netfilter.org/projects/libnetfilter_cthelper/index.html")
    (supported-systems (filter target-linux? %supported-systems))
    (properties '((upstream-name . "libnetfilter_cthelper")))
    (license license:gpl2+)))

(define-public libnetfilter-queue
  (package
    (name "libnetfilter-queue")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://netfilter.org/projects/"
                           "libnetfilter_queue/files/"
                           "libnetfilter_queue-" version ".tar.bz2"))
       (sha256
        (base32
         "1xdra6i4p8jkv943ygjw646qx8df27f7p5852kc06vjx608krzzr"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libmnl libnfnetlink))
    (synopsis "Library for kernel netfilter infrastructure and state")
    (description "libnetfilter_queue is a userspace library providing an API
to packets that have been queued by the kernel packet filter.  It is is part
of a system that deprecates the old ip_queue/libipq mechanism.")
    (home-page "https://netfilter.org/projects/libnetfilter_queue/index.html")
    (supported-systems (filter target-linux? %supported-systems))
    (properties '((upstream-name . "libnetfilter_queue")))
    (license license:gpl2+)))

(define-public conntrack-tools
  (package
    (name "conntrack-tools")
    (version "1.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://netfilter.org/projects/"
                           "conntrack-tools/files/"
                           "conntrack-tools-" version ".tar.bz2"))
       (sha256
        (base32 "0pabq97rpci3z1bdc54cyhc3b9w86m4nnlbzsp7905p8az7yp789"))))
    (build-system gnu-build-system)
    (native-inputs (list bison flex pkg-config))
    (inputs (list libtirpc
                  libnetfilter-conntrack
                  libnetfilter-cttimeout
                  libnetfilter-cthelper
                  libnetfilter-queue
                  libnfnetlink
                  libmnl))
    (synopsis "Set of tools targeting the conntrack kernel subsystem")
    (description "The tool conntrack provides a full featured interface that
is intended to replace the old @file{/proc/net/ip_conntrack} interface.  Using
conntrack, you can view and manage the in-kernel connection tracking state
table from userspace.  On the other hand, conntrackd covers the specific
aspects of stateful firewalls to enable highly available scenarios, and can be
used as statistics collector as well.

Since 1.2.0, the conntrack-tools includes the @command{nfct} command line
utility.  This utility only supports the nfnetlink_cttimeout by now.  In the
long run, we expect that it will replace conntrack by providing a syntax
similar to nftables.")
    (home-page "https://netfilter.org/projects/conntrack-tools/index.html")
    (supported-systems (filter target-linux? %supported-systems))
    (license license:gpl2+)))

(define-public libnetfilter-acct
  (package
    (name "libnetfilter-acct")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://netfilter.org/projects/"
                           "libnetfilter_acct/files/"
                           "libnetfilter_acct-" version ".tar.bz2"))
       (sha256
        (base32
         "06lsjndgfjsgfjr43px2n2wk3nr7whz6r405mks3887y7vpwwl22"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libmnl))
    (synopsis "Library providing interface to extended accounting infrastructure")
    (description "libnetfilter_acct is the userspace library providing interface
to extended accounting infrastructure.

@enumerate
@item
creating accounting objects
@item
retrieving accounting objects (and atomically set to zero)
@item
deleting accounting objects
@end enumerate
For the nfnetlink_acct subsystem.")
    (home-page "https://netfilter.org/projects/libnetfilter_acct/index.html")
    (supported-systems (filter target-linux? %supported-systems))
    (properties '((upstream-name . "libnetfilter_acct")))
    (license license:lgpl2.1+)))

(define-public nfacct
  (package
    (name "nfacct")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://netfilter.org/projects/" name
                           "/files/nfacct-" version ".tar.bz2"))
       (sha256
        (base32
         "0sdxbxjyapbqcp2ami5jd10vz4xbbdvx39f3wfy1iqsbflc25zzc"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libmnl libnetfilter-acct))
    (synopsis "Command line tool to create/retrieve/delete accounting objects")
    (description "nfacct is the command line tool to create/retrieve/delete
accounting objects
@enumerate
@item
listing the objects of the nfacct table in plain text/XML
@item
atomically get and reset objects of the nfacct table
@item
adding new objects to the nfacct table
@item
deleting objects from the nfacct table
@end enumerate")
    (home-page "https://netfilter.org/projects/nfacct/index.html")
    (supported-systems (filter target-linux? %supported-systems))
    (license license:gpl2+)))

(define-public libnetfilter-log
  (package
    (name "libnetfilter-log")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://netfilter.org/projects/"
                           "libnetfilter_log/files/"
                           "libnetfilter_log-" version ".tar.bz2"))
       (sha256
        (base32
         "1spy9xs41v76kid5ana8n126f3mvgq6fjibbfbj4kn0larbhix73"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libnfnetlink libmnl))
    (synopsis "Library providing interface to packets logged by netfilter")
    (description "libnetfilter_log is a userspace library providing interface to
packets that have been logged by the kernel packet filter.  It is is part of a
system that deprecates the old syslog/dmesg based packet logging.  This library
has been previously known as libnfnetlink_log.

@enumerate
@item
receiving to-be-logged packets from the kernel nfnetlink_log subsystem
@end enumerate")
    (home-page "https://netfilter.org/projects/libnetfilter_log/index.html")
    (supported-systems (filter target-linux? %supported-systems))
    (properties '((upstream-name . "libnetfilter_log")))
    (license license:gpl2+)))

(define-public ulogd
  (package
    (name "ulogd")
    (version "2.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://netfilter.org/projects/" name
                           "/files/ulogd-" version ".tar.bz2"))
       (sha256
        (base32
         "0ax9959c4bapq78n13bbaibcf1gwjir3ngx8l2dh45lw9m4ha2lr"))))
    (build-system gnu-build-system)
    (outputs '("out"
               ;; additional non-default output plugins
               "json" "pcap" "sqlite3" "pgsql" "mysql"))
    (native-inputs (list pkg-config))
    (inputs (list libnfnetlink
                  libmnl
                  libnetfilter-log
                  libnetfilter-conntrack
                  libnetfilter-acct
                  sqlite
                  libpcap
                  jansson
                  postgresql
                  (list mariadb "dev")
                  zlib
                  openssl))
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-pgsql="
                                  (assoc-ref %build-inputs "postgresql"))
                   (string-append "--with-mysql="
                                  (assoc-ref %build-inputs "mariadb")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-doc
                 (lambda _
                   (let ((out-etc (string-append #$output "/etc"))
                         (ulogd.conf "ulogd.conf"))
                     (mkdir-p out-etc)
                     (copy-file ulogd.conf (string-append out-etc "/"
                                                          ulogd.conf)))))
               (add-after 'install 'setup-plugin-outputs
                 (lambda* (#:key outputs #:allow-other-keys)
                   (with-directory-excursion
                       (string-append #$output "/lib/ulogd/")
                     (for-each
                      (lambda (output-name)
                        (let ((output-dir (string-append
                                           (assoc-ref outputs output-name)
                                           "/lib/ulogd/")))
                          (mkdir-p output-dir)
                          (for-each
                           (lambda (plugin)
                             (copy-file plugin (string-append output-dir plugin))
                             (delete-file plugin))
                           (find-files "."
                                       (string-append "ulogd_output_"
                                                      (string-upcase output-name)
                                                      ".*$")))))
                      (list "json" "pcap" "sqlite3" "pgsql" "mysql"))))))))
    (synopsis "Logging daemon for netfilter and iptables")
    (description "ulogd is a userspace logging daemon for netfilter/iptables
related logging.  This includes per-packet logging of security violations,
per-packet logging for accounting, per-flow logging and flexible user-defined
accounting.

@enumerate
@item
Packet and flow-based traffic accounting
@item
Flexible user-defined traffic accounting via nfacct infrastructure
@item
SQL database back-end support: SQLite3, PostgreSQL, MySQL
@item
Text-based output formats: CSV, XML, Netfilter's LOG, Netfilter's conntrack
@end enumerate")
    (home-page "https://netfilter.org/projects/nfacct/index.html")
    (supported-systems (filter target-linux? %supported-systems))
    (license license:gpl2)))

(define-public proot
  (package
    (name "proot")
    (version "5.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/proot-me/PRoot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "186qsg4yvisqjgf8w5jxhnlig7x341vpqwcgp8as3r59qmqkpmk7"))
       ;; Waiting for upstream inclusion at
       ;; https://github.com/proot-me/proot/pull/355
       (patches (search-patches "proot-add-clone3.patch"))))
    (build-system gnu-build-system)
    ;; Many architectures are not supported (see:
    ;; https://github.com/proot-me/proot/blob/master/src/arch.h#L51).
    (supported-systems '("x86_64-linux" "i686-linux"
                         "armhf-linux" "aarch64-linux" "i586-gnu" "x86_64-gnu"))
    (arguments
     ;; Disable the test suite on armhf-linux, as there are too many
     ;; failures to keep track of (see for example:
     ;; https://github.com/proot-me/proot/issues/286).
     `(#:tests? ,(not (or (%current-target-system)
                          (string-prefix? "armhf"
                                          (or (%current-system)))))
       #:make-flags '("-C" "src")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-sources
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* (find-files "src" "\\.[ch]$")
                        (("\"/bin/sh\"")
                         (string-append "\"" (assoc-ref inputs "bash")
                                        "/bin/sh\"")))

                      (substitute* "src/GNUmakefile"
                        (("/bin/echo") (which "echo"))
                        (("^VERSION = .*")
                         (string-append "VERSION := " ,version "\n")))

                      (substitute* (find-files "test" "\\.sh$")
                        ;; Some of the tests try to "bind-mount" /bin/true.
                        (("-b /bin/true:")
                         (string-append "-b " (which "true") ":"))
                        ;; Likewise for /bin.
                        (("-b /bin:") "-b /gnu:")
                        ;; Others try to run /bin/sh.
                        (("/bin/sh") (which "sh"))
                        ;; Others assume /etc/fstab exists.
                        (("/etc/fstab") "/etc/passwd"))
                      (substitute* "test/GNUmakefile"
                        (("-b /bin:") "-b /gnu:"))
                      (substitute* "test/test-c6b77b77.mk"
                        (("/bin/bash") (which "bash"))
                        (("/usr/bin/test") (which "test")))
                      (substitute* "test/test-16573e73.c"
                        (("/bin/([a-z-]+)" _ program)
                         (which program)))
                      (substitute* "test/test-5467b986.sh"
                        (("-w /usr") "-w /gnu")
                        (("-w usr") "-w gnu")
                        (("/usr/share") "/gnu/store")
                        (("share") "store"))
                      (substitute* "test/test-092c5e26.sh"
                        (("-q echo ")
                         "-q $(which echo) "))

                      ;; The socket tests requires networking.
                      (for-each delete-file
                                (find-files "test" "test-socket.*\\.sh$"))))
                  (delete 'configure)
                  (add-after 'build 'build-manpage
                    (lambda _
                      (with-directory-excursion "doc"
                        (invoke "make" "proot/man.1" "SUFFIX=.py"))))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (let ((n (parallel-job-count)))
                          ;; Most of the tests expect "/bin" to be in $PATH so
                          ;; they can run things that live in $ROOTFS/bin.
                          (setenv "PATH"
                                  (string-append (getenv "PATH") ":/bin"))
                          (invoke "make" "check" "-C" "test"
                                  ;;"V=1"
                                  "-j" (number->string n))))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; The 'install' rule does nearly nothing.
                      (let* ((out (assoc-ref outputs "out"))
                             (man1 (string-append out "/share/man/man1")))
                        ;; TODO: 'make install-care' (does not even
                        ;; build currently.)
                        (invoke "make" "-C" "src" "install"
                                (string-append "PREFIX=" out))
                        (mkdir-p man1)
                        (copy-file "doc/proot/man.1"
                                   (string-append man1 "/proot.1"))))))))
    (native-inputs (list which
                         ;; For 'mcookie', used by some of the tests.
                         util-linux
                         coreutils
                         pkg-config
                         ;; For rst2man, used to generate the manual page.
                         python-docutils))
    (inputs (list libarchive talloc))
    (home-page "https://github.com/proot-me/PRoot")
    (synopsis "Unprivileged chroot, bind mount, and binfmt_misc")
    (description
     "PRoot is a user-space implementation of @code{chroot}, @code{mount --bind},
and @code{binfmt_misc}.  This means that users don't need any privileges or
setup to do things like using an arbitrary directory as the new root
file system, making files accessible somewhere else in the file system
hierarchy, or executing programs built for another CPU architecture
transparently through QEMU user-mode.  Also, developers can use PRoot as a
generic process instrumentation engine thanks to its extension mechanism.
Technically PRoot relies on @code{ptrace}, an unprivileged system-call
available in the kernel Linux.")
    (license license:gpl2+)))

(define-public proot-static
  (package
    (inherit proot)
    (name "proot-static")
    (synopsis
     "Unprivileged chroot, bind mount, and binfmt_misc (statically linked)")
    (inputs `(("talloc" ,talloc/static)))
    (arguments
     (substitute-keyword-arguments (package-arguments proot)
       ((#:make-flags flags)
        `(cons "LDFLAGS = -ltalloc -static -static-libgcc" ,flags))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'strip 'remove-store-references
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (with-directory-excursion out
                   (remove-store-references "bin/proot")
                   #t))))))
       ((#:allowed-references _ '("out"))
        '("out"))))))

(define-public cpuid
  (package
    (name "cpuid")
    (version "20230614")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.etallen.com/cpuid/cpuid-"
                                  version ".src.tar.gz"))
              (sha256
               (base32
                "1s54qc1j10d765r05kw9pzwzaxq2b0nndq2ifwq7cq62xx2k1j5i"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))
           #:tests? #f                  ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ; no configure script
               (add-before 'install 'fix-makefile
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute* "Makefile"
                     (("\\$\\(BUILDROOT\\)/usr")
                      (assoc-ref outputs "out"))))))))
    (inputs (list perl))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://www.etallen.com/cpuid.html")
    (synopsis "Dump x86 CPUID processor information")
    (description "cpuid dumps detailed information about the CPU(s) gathered
from the CPUID instruction, and also determines the exact model of CPU(s).  It
supports Intel, AMD, and VIA CPUs, as well as older Transmeta, Cyrix, UMC,
NexGen, Rise, and SiS CPUs.")
    (license license:gpl2+)))

(define-public jmtpfs
  (package
    (name "jmtpfs")
    (version "0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JasonFerrara/jmtpfs")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1pm68agkhrwgrplrfrnbwdcvx5lrivdmqw8pb5gdmm3xppnryji1"))))
    (build-system gnu-build-system)
    (inputs
     (list file fuse-2 libmtp))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/JasonFerrara/jmtpfs")
    (synopsis "Use a FUSE file system to access data over MTP")
    (description "jmtpfs uses @acronym{FUSE, File system in USEr space} to
provide access to data over @acronym{MTP, the Media Transfer Protocol}.
Unprivileged users can mount the MTP device as a file system.")
    (license license:gpl3)))

(define-public procenv
  (package
   (name "procenv")
   (version "0.60")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
            (url "https://github.com/jamesodhunt/procenv")
            (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "00d7q0h4qjc8lg435lq77lp2fx6ikm5piq90m81mr1dqqna1g6pz"))))
   (build-system gnu-build-system)
   (native-inputs
    (list pkg-config
          ;; For tests.
          check groff))
   (inputs
    (list expat libcap libselinux))
   (synopsis "Utility to show process environment")
   (description
    "Procenv is a command-line tool that displays as much detail about
itself and its environment as possible.  It can be used as a test
tool, to understand the type of environment a process runs in, and for
comparing system environments.")
   (home-page "https://github.com/jamesodhunt/procenv/")
   (license license:gpl3+)))

(define-public cassini-headers
  (let ((commit "9a8a738a879f007849fbc69be8e3487a4abf0952")
        (revision "0"))
    (package
      (name "cassini-headers")
      (version (git-version "2.0.0"               ;per .spec file
                            revision commit))
      (home-page "https://github.com/HewlettPackard/shs-cassini-headers")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0a54vwfr29n0i392wdap7rzmq0lb8mxa17d8yljdbm0kzrq48csz"))))
      (build-system copy-build-system)
      (arguments
       (list #:install-plan
             #~'(("include" "include")
                 ("share/cassini-headers" "share/cassini-headers"))))
      (synopsis "Cassini network hardware definitions and headers")
      (description
       "This package provides hardware definitions and C headers for use by
the Linux driver and by user-space applications for the Cassini/Slingshot
high-speed network interconnect made by HPE (formerly Cray).  User-land
software uses @file{cxi_prov_hw.h} from this package.")
      ;; As per include/cxi_prov_hw.h it is __aarch64__ or __x86_64__ only.
      (supported-systems '("x86_64-linux" "aarch64-linux"))
      (license (list license:gpl2 license:bsd-2))))) ;dual-licensed

(define-public cxi-driver
  (let ((commit "5f0ec0ead6ef3f98542a2ef5e76b89d14dd22150")
        (revision "0"))
    (package
      (name "cxi-driver")
      (version (git-version "1.0.0"               ;per .spec file
                            revision commit))
      (home-page "https://github.com/HewlettPackard/shs-cxi-driver")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page) (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "19cly014ihgdidrc1aki2xsbfhpc0g73v0vxcky8r27xza7rz5bg"))))
      ;; TODO: Actually build the Linux driver.
      (build-system copy-build-system)
      (arguments
       (list #:install-plan #~'(("include" "include"))))
      (propagated-inputs (list cassini-headers))
      (synopsis "Linux driver for the Cassini/Slingshot interconnect")
      (description
       "This is the Linux driver for the Cray/HPE Cassini 1 and 2 high-speed
network interconnect (aka. Slingshot), and its Ethernet driver.  It includes
the @file{uapi/misc/cxi.h} C header file for use by user-land software.

Currently the Linux driver itself is missing from this package.")
      (license license:gpl2+))))

(define-public libcxi
  (let ((commit "5b6f8b5d57017c7963debb379d5693c59aca63ed")
        (revision "0"))
    (package
      (name "libcxi")
      (version (git-version "1.0.1" revision commit))
      (home-page "https://github.com/HewlettPackard/shs-libcxi")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page) (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1h3dhird8p11q4ziaxzg1hr5gxcgwx1limzdcyildyaw50dy549g"))))
      (build-system gnu-build-system)
      (arguments
       (list #:configure-flags
             #~(list "--disable-static"
                     (string-append "--with-udevrulesdir="
                                    #$output "/lib/udev/rules.d"))

             #:phases
             #~(modify-phases %standard-phases
                 (add-before 'configure 'set-cassini-file-names
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "utils/cxi_dump_csrs.py"
                       (("/usr/share/cassini-headers/csr_defs.json")
                        (search-input-file
                         inputs
                         "/share/cassini-headers/csr_defs.json"))))))))
      (native-inputs (list autoconf
                           automake
                           libtool
                           pkg-config
                           python-wrapper))
      (inputs (list libconfig
                    libuv
                    fuse-2
                    libyaml
                    libnl
                    numactl
                    eudev
                    (list lm-sensors "lib")))
      (propagated-inputs (list cassini-headers cxi-driver))
      (synopsis "Interface to the Cassini/Slingshot high-speed interconnect")
      (description
       "Libcxi provides applications with a low-level interface to the
Cray/HPE Cassini high-speed @acronym{NIC, network interface controller}, also
known as Slingshot.")

      ;; License is spelled out in 'cray-libcxi.spec' and in source file
      ;; headers.
      (license (list license:lgpl2.1+ license:bsd-3))))) ;dual-licensed

(define-public libfabric
  (package
    (name "libfabric")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ofiwg/libfabric/releases/download/v"
                       version "/libfabric-" version ".tar.bz2"))
       (sha256
        (base32 "1znmw83rmippv0fwz0x7lgylfk17dr9ckll8lrm4z7kclspnqpj8"))))
    (build-system gnu-build-system)
    (inputs
     (let ((if-supported                          ;XXX: modified from openmpi
            (lambda (package . extra)
              (if (and (not (%current-target-system))
                       (member (%current-system)
                               (package-transitive-supported-systems package)))
                  (cons package extra)
                  '()))))
       (append (list rdma-core libnl)
               (if-supported psm)
               (if-supported psm2)
               (if-supported libcxi curl json-c))))
    (arguments
     (list #:configure-flags
           #~(append (if #$(target-64bit?)
                           (list "--enable-efa")
                           '())
                     (list #$@(if (this-package-input "libcxi")
                                  #~("--enable-cxi")
                                  #~())
                           "--enable-verbs"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'remove-libtool-archive
                 (lambda _
                   ;; 'libfabric.la' has '-ljson-c' without a corresponding
                   ;; '-L' in 'dependency_libs', which in turn causes users
                   ;; such as Open MPI to fail at link time due to '-ljson-c'
                   ;; not being found, even when building a shared library.
                   ;; So, remove the .la file.
                   (delete-file
                    (string-append #$output
                                   "/lib/libfabric.la")))))))
    (home-page "https://ofiwg.github.io/libfabric/")
    (synopsis "Open Fabric Interfaces")
    (description
     "OpenFabrics Interfaces (OFI) is a framework focused on exporting fabric
communication services to applications.  OFI is best described as a collection
of libraries and applications used to export fabric services.  The key
components of OFI are: application interfaces, provider libraries, kernel
services, daemons, and test applications.

Libfabric is a core component of OFI.  It is the library that defines and
exports the user-space API of OFI, and is typically the only software that
applications deal with directly.  It works in conjunction with provider
libraries, which are often integrated directly into libfabric.")
    (license (list license:bsd-2 license:gpl2)))) ;dual

(define-public psm
  (package
    (name "psm")
    (version "3.3.20170428")
    (home-page "https://github.com/intel/psm")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit "604758e76dc31e68d1de736ccf5ddf16cb22355b")))
       (file-name (string-append "psm-" version ".tar.gz"))
       (sha256
        (base32 "0nsb325dmhn5ia3d2cnksqr0gdvrrx2hmvlylfgvmaqdpq76zm85"))
       (patches (search-patches
                 "psm-arch.patch"     ; uname -p returns "unknown" on Debian 9
                 "psm-ldflags.patch"  ; build shared lib with LDFLAGS
                 "psm-repro.patch"    ; reproducibility
                 "psm-disable-memory-stats.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; That file declares its own 'strlcat' as static.  To avoid a
        ;; conflict with the function now in glibc 2.39, give it a
        ;; different name.
        #~(substitute* "ptl_ips/ips_proto_dump.c"
            (("strlcat")
             "psm_custom_strlcat")))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (inputs `(("libuuid" ,util-linux "lib")))
    (arguments
     '(#:make-flags `("PSM_USE_SYS_UUID=1" "CC=gcc" "WERROR="
                      ,(string-append "INSTALL_PREFIX=" %output)
                      ,(string-append "CFLAGS=-Wall -fpic -fPIC -D_GNU_SOURCE"
                                      " -funwind-tables -O3 -g3"
                                      " -DPSM_USE_SYS_UUID"
                                      " -Wno-strict-aliasing -DNVALGRIND"
                                      " -fcommon")
                      ,(string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-/usr/include
                    (lambda _
                      (substitute* "Makefile"
                        (("\\$\\{DESTDIR}/usr/include")
                         (string-append %output "/include")))
                      (substitute* "Makefile"
                        (("/lib64") "/lib"))
                      #t))
                  (add-after 'unpack 'patch-sysmacros
                    (lambda _
                      (substitute* "ipath/ipath_proto.c"
                        (("#include <sys/poll.h>" m)
                         (string-append m "\n"
                                        "#include <sys/sysmacros.h>")))
                      #t)))))
    (synopsis "Intel Performance Scaled Messaging (PSM) Libraries")
    (description
     "The PSM Messaging API, or PSM API, is Intel's low-level user-level
communications interface for the True Scale family of products.  PSM users are
enabled with mechanisms necessary to implement higher level communications
interfaces in parallel environments.")
    ;; Only Intel-compatible processors are supported.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license (list license:bsd-2 license:gpl2)))) ;dual

(define-public snapscreenshot
  (package
    (name "snapscreenshot")
    (version "1.0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://bisqwit.iki.fi/src/arch/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32 "0gzvqsbf6a2sbd1mqvj1lbm57i2bm5k0cr6ncr821d1f32gw03mk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "BINDIR=" out "/bin")
               (string-append "MANDIR=" out "/share/man")))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; ./configure is a snarky no-op
         (add-before 'install 'fix-ownership
           ;; Install binaries owned by ‘root’ instead of the nonexistent ‘bin’.
           (lambda _
             (substitute* "depfun.mak"
               ((" -o bin -g bin ") " "))
             #t))
         (add-before 'install 'create-output-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/man/man1"))
               #t))))))
    (home-page "https://bisqwit.iki.fi/source/snapscreenshot.html")
    (synopsis "Take screenshots of one or more Linux text consoles")
    (description
     "snapscreenshot saves a screenshot of one or more Linux text consoles as a
Targa (@dfn{.tga}) image.  It can be used by anyone with read access to the
relevant @file{/dev/vcs*} file(s).")
    (license license:gpl2)))

(define-public fbcat
  (package
    (name "fbcat")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jwilk/fbcat/releases/download/"
                           version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "07q6f0xj7b4gjvn69qfn0g04yd0ch8ndzyigcz8nnrhli0cvsbh6"))))
    (build-system gnu-build-system)
    (inputs
     ;; The ‘fbgrab’ wrapper can use one of several PPM-to-PNG converters.  We
     ;; choose netpbm simply because it's the smallest.  It still adds ~94 MiB
     ;; to an otherwise tiny package, so we put ‘fbgrab’ in its own output.
     `(("pnmtopng" ,netpbm)))
    (outputs (list "out" "fbgrab"))
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'build 'qualify-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((pnmtopng (assoc-ref inputs "pnmtopng"))
                    (out (assoc-ref outputs "out")))
               (substitute* "fbgrab"
                 (("fbcat" all)
                  (string-append out "/bin/" all))
                 (("pnmtopng" all)
                  (string-append pnmtopng "/bin/" all))))))
         (add-after 'install 'split-fbgrab-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out:fbgrab (assoc-ref outputs "fbgrab")))
               (for-each (lambda (file)
                           (let ((old (string-append out "/" file))
                                 (new (string-append out:fbgrab "/" file)))
                             (mkdir-p (dirname new))
                             (rename-file old new)))
                         (list "bin/fbgrab"
                               "share/man/man1/fbgrab.1"))))))))
    (home-page "https://jwilk.net/software/fbcat")
    (synopsis "Take a screenshot of the contents of the Linux framebuffer")
    (description
     "fbcat saves the contents of the Linux framebuffer (@file{/dev/fb*}), or
a dump thereof.  It supports a wide range of drivers and pixel formats.

@command{fbcat} can take screenshots of virtually any application that can be
made to write its output to the framebuffer, including (but not limited to)
text-mode or graphical applications that don't use a display server.

Also included is @command{fbgrab}, a wrapper around @command{fbcat} that
emulates the behaviour of Gunnar Monell's older fbgrab utility.")
    (license license:gpl2)))

(define-public fbgrab
  (package
    (name "fbgrab")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/GunnarMonell/fbgrab")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1npn7l8jg0nhjraybjl38v8635zawzmn06ql3hs3vhci1vi1r90r"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                (string-append "DESTDIR=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'strip-/usr-prefix
                          (lambda _
                            (substitute* "Makefile"
                              (("/usr") ""))))
                        (delete 'configure))))
    (inputs (list libpng zlib))
    (home-page "https://github.com/GunnarMonell/fbgrab")
    (synopsis "Linux framebuffer screenshot/conversion utility")
    (description "FBGrab is a framebuffer screenshot program that captures the
Linux framebuffer and converts it to a PNG image.  It can also convert a
framebuffer dump file (copy) to a PNG image.")
    (license license:gpl2)))   ;GPL 2.0 only (per SPDX identifier in fbgrab.c)

(define-public libcgroup
  (package
    (name "libcgroup")
    (version "3.1.0")
    (home-page "https://github.com/libcgroup/libcgroup")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/libcgroup/libcgroup/releases/download/v"
             version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0n0jkvmagw14vgwx3j5b6vv5h25lasrg2a7xihq9h11ww2qw8vlp"))))
    (build-system gnu-build-system)
    (arguments
     ;; Tests are virtualized with lxc, it is not very feasible
     ;; to make them executable under guix build. Also, note that
     ;; origin is using source tarball release which is prepared
     ;; after testing.
     (list #:tests? #f
           #:configure-flags
           #~'("--disable-systemd")))
    (native-inputs
     (list bison flex))
    (inputs
     (list linux-pam))
    (synopsis "Control groups management tools")
    (description "Control groups is Linux kernel method for process resource
restriction, permission handling and more.  This package provides userspace
interface to this kernel feature.")
    (license license:lgpl2.1)))

(define-public mbpfan
  (package
    (name "mbpfan")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgraziotin/mbpfan")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gc9ypxi55vxs77nx8ihhh9zk7fr9v0m0zfm76q7x0bi6jz11mbr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; tests ask to be run as root
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "DESTDIR=" out)
                            ,(string-append "CC=" (cc-for-target))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "Makefile"
               (("/usr") ""))
             #t))
         (delete 'configure))))         ; there's no configure phase
    (home-page "https://github.com/dgraziotin/mbpfan")
    (synopsis "Control fan speed on Macbooks")
    (description
     "mbpfan is a fan control daemon for Apple Macbooks.  It uses input from
the @code{coretemp} module and sets the fan speed using the @code{applesmc}
module.  It can be executed as a daemon or in the foreground with root
privileges.")
    (license license:gpl3+)))

(define-public psm2
  (package
    (name "psm2")
    (version "12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/opa-psm2")
                    ;; tag 'psm-v12.0' = commit ad5dd1b
                    (commit (string-append "psm-v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04nar65ac11qqx41vkfs7iip8kfiah0zms7l4rmsxncpiz2iqfik"))
              (modules '((guix build utils)))
              (snippet
               ;; That file declares its own 'strlcat' as static.  To avoid a
               ;; conflict with the function now in glibc 2.39, give it a
               ;; different name.
               #~(substitute* "ptl_ips/ips_proto_dump.c"
                   (("strlcat")
                    "psm2_custom_strlcat")))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
           #:tests? #f
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (add-after 'unpack 'patch-Makefiles
                          (lambda _
                            (substitute* "Makefile"
                              (("/lib64") "/lib")
                              (("/usr") ""))
                            (substitute* "compat/Makefile"
                              (("/lib64") "/lib")
                              (("/usr") ""))))
                        (replace 'install
                          (lambda _
                            (setenv "DESTDIR" #$output)
                            (invoke "make" "install"))))))
    (inputs
     (list rdma-core numactl))
    (synopsis "Intel Performance Scaled Messaging 2 (PSM2) library")
    (description
     "This package is low-level user-level Intel's communications interface.
The PSM2 API is a high-performance vendor-specific protocol that provides a
low-level communications interface for the Intel Omni-Path family of
high-speed networking devices.")
    (home-page "https://github.com/intel/opa-psm2")
    ;; Only the x86_64 architecture is supported.
    (supported-systems '("x86_64-linux"))
    (license (list license:bsd-3 license:gpl2)))) ; dual

(define-public libpfm4
  (package
    (name "libpfm4")
    (version "4.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/perfmon2/"
                                  name "/libpfm-" version ".tar.gz"))
              (sha256
               (base32
                "1qp4g4n6dw42p2w5rkwzdb7ynk8h7g5vg01ybpmvxncgwa7bw3yv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules ((guix build utils)
                  (guix build gnu-build-system))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'check)
                  (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (setenv "CC" "gcc")
                        (setenv "CFLAGS" "-Wno-format-truncation")
                        (invoke "make")
                        #t)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (invoke "make"
                                (string-append "PREFIX=" out)
                                "install")
                        #t))))))
    (synopsis "Performance event monitoring library")
    (description
     "This package provides a library called libpfm4, which is used to develop
monitoring tools exploiting the performance monitoring events such as those
provided by the Performance Monitoring Unit (PMU) of modern processors.

Libpfm4 helps convert from an event name, expressed as a string, to the event
encoding that is either the raw event as documented by the hardware vendor or
the OS-specific encoding.  In the latter case, the library is able to prepare
the OS-specific data structures needed by the kernel to setup the event.

libpfm4 provides support for the @code{perf_events} interface, which was
introduced in Linux 2.6.31.")
    (home-page "https://perfmon2.sourceforge.net/")
    (license license:expat)))

(define-public libnfnetlink
  (package
    (name "libnfnetlink")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.netfilter.org/projects/libnfnetlink/files/"
                    "libnfnetlink-" version ".tar.bz2"))
              (sha256
               (base32
                "0xn3rcrzxr6g82kfxzs9bqn2zvl2kf2yda30drwb9vr6sk1wfr5h"))))
    (build-system gnu-build-system)
    (home-page "https://www.netfilter.org/projects/libnfnetlink/")
    (synopsis "Low-level netfilter netlink communication library")
    (description
     "@code{libnfnetlink} is the low-level library for netfilter related
kernel/userspace communication.  It provides a generic messaging
infrastructure for in-kernel netfilter subsystems (such as nfnetlink_log,
nfnetlink_queue, nfnetlink_conntrack) and their respective users and/or
management tools in userspace.")
    (license license:gpl2)))

(define-public libinih
  (package
    (name "libinih")
    (version "57")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/benhoyt/inih")
                    (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03hpyahdkipm5wfalj2xkz6y9ncd9zrlhnf7vap6lr56jj2fz2bb"))))
    (build-system meson-build-system)
    ;; Install static libraries for use by the initrd's xfsprogs/static.
    (outputs (list "out" "static"))
    (arguments
     `(#:configure-flags
       (list "-Ddistro_install=true"
             "-Ddefault_library=both")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'separate-static
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out    (assoc-ref outputs "out"))
                   (static (assoc-ref outputs "static")))
               (with-directory-excursion out
                 (for-each (lambda (source)
                             (let ((target (string-append static "/" source)))
                               (mkdir-p (dirname target))
                               (rename-file source target)))
                           (find-files "lib" "\\.a$")))))))))
    (home-page "https://github.com/benhoyt/inih")
    (synopsis "Simple .INI parser library for C")
    (description "The inih (INI Not Invented Here) library is a simple .INI file
parser written in C.  It's only a couple of pages of code, and it was designed to
be small and simple, so it's good for embedded systems.  It's also more or less
compatible with Python's ConfigParser style of .INI files, including RFC
822-style multi-line syntax and name: value entries.")
    (license license:bsd-3)))

(define-public simpleini
  (let ((commit "6048871ea9ee0ec24be5bd099d161a10567d7dc2")
        (revision "1"))
    (package
      (name "simpleini")
      (version (git-version "4.22" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brofield/simpleini")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1szaflnrzw1zx9v5g6mbbiaf9wfglp4n4jjq2793k9ryz3qxil9j"))))
      (build-system cmake-build-system)
      (arguments
       (list #:configure-flags #~(list "-DSIMPLEINI_USE_SYSTEM_GTEST=ON")))
      (native-inputs (list googletest))
      (home-page "https://github.com/brofield/simpleini")
      (synopsis "Simple API to read and write INI-style files")
      (description
       "SimpleIni provides a simple API to read and write INI-style
configuration files.  It supports data files in ASCII, MBCS and Unicode.")
      (license license:expat))))

(define-public xfsprogs
  (package
    (name "xfsprogs")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/fs/xfs/xfsprogs/"
                    "xfsprogs-" version ".tar.gz"))
              (sha256
               (base32
                "14hc61nfc73nqwhyasc4haj5g7046im1dwz61bx338f86mjj5n5y"))))
    (build-system gnu-build-system)
    (outputs (list "out" "python"))
    (arguments
     `(#:tests? #f   ; kernel/user integration tests are in package "xfstests"
       #:configure-flags
       (list "--disable-static")
       #:make-flags
       (list "V=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'separate-python-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out    (assoc-ref outputs "out"))
                   (python (assoc-ref outputs "python")))
               (for-each
                (lambda (script)
                  (mkdir-p (string-append python (dirname script)))
                  (rename-file (string-append out script)
                               (string-append python script)))
                (list "/sbin/xfs_scrub_all")))))
         (add-after 'install 'install-headers
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "install-dev" make-flags))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("libinih" ,libinih)
       ("liburcu" ,liburcu)
       ("libuuid" ,util-linux "lib")
       ("python" ,python-wrapper)))
    (home-page "https://xfs.wiki.kernel.org/")
    (synopsis "XFS file system tools")
    (description "This package provides commands to create and check XFS
file systems.")
    ;; The library "libhandle" and the headers in "xfslibs-dev" are
    ;; licensed under lgpl2.1. the other stuff is licensed under gpl2.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public xfsprogs-5.9
  (package
    (inherit xfsprogs)
    (name "xfsprogs")
    (version "5.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/fs/xfs/xfsprogs/"
                    "xfsprogs-" version ".tar.gz"))
              (sha256
               (base32
                "13xkn9jpmwp4fm9r68vhgznkmxhnv83n2b39mhy2qdaph90w2a1l"))))))

(define-public xfsprogs/static
  (package
    (inherit xfsprogs)
    (name "xfsprogs-static")
    (outputs (list "out"))
    (arguments
     (substitute-keyword-arguments (package-arguments xfsprogs)
       ((#:configure-flags configure-flags '())
        `(append ,configure-flags
                 (list "--enable-static")))
       ((#:make-flags make-flags ''())
        `(cons* "LLDFLAGS=-all-static" ,make-flags))
       ((#:phases _ ''())
        `(modify-phases %standard-phases
           (add-after 'install 'delete-useless-files
             (lambda* (#:key outputs #:allow-other-keys)
               (with-directory-excursion (assoc-ref outputs "out")
                 (for-each delete-file-recursively
                           (list "include" "lib")))))))))
    (inputs
     `(("libinih:static" ,libinih "static")
       ("util-linux:static" ,util-linux "static")
       ,@(remove (match-lambda
                   ((label . _)
                    (member label '("python"))))
                 (package-inputs xfsprogs))))
    (synopsis "Statically linked XFS file system tools")))

(define-public xfs_repair/static
  (package
    (name "xfs_repair-static")
    (version (package-version xfsprogs/static))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((xfsprogs (assoc-ref %build-inputs "xfsprogs"))
                (out      (assoc-ref %outputs "out"))
                (sbin     (string-append out "/sbin")))
           (install-file (string-append xfsprogs "/sbin/xfs_repair") sbin)
           (with-directory-excursion sbin
             (remove-store-references "xfs_repair"))))))
    (inputs
     `(("xfsprogs" ,xfsprogs/static)))
    (home-page (package-home-page xfsprogs/static))
    (synopsis "Statically linked @command{xfs_repair} from xfsprogs")
    (description
     "This package provides a statically linked @command{xfs_repair} taken
from the xfsprogs package.  It is meant to be used in initrds.")
    (license (package-license xfsprogs/static))))

(define-public genext2fs
  (package
    (name "genext2fs")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bestouff/genext2fs")
                    (commit "474111097321f03de9e009aa9f7d4a8948e310b2")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "14lgvml5zz99845ja47jpf4iirjzfqv36ffzachh3hw2ggji9c7l"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (home-page "https://github.com/bestouff/genext2fs")
    (synopsis "Generate ext2 file system as a normal user")
    (description "This package provides a program to generate an ext2
file system as a normal (non-root) user.  It does not require you to mount
the image file to copy files on it, nor does it require that you become
the superuser to make device nodes.")
    (license license:gpl2)))

(define-public fakeroot
  (package
    (name "fakeroot")
    (version "1.35.1")
    (source
     (origin
       ;; There are no tags in the repository, so take this snapshot.
       (method url-fetch)
       (uri (string-append "https://deb.debian.org/debian/pool/main/f/"
                           "fakeroot/fakeroot_" version ".orig.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p2zcng64sigixppmh42gd3ava771pmq9a6lwva7flp05lxya3ba"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet
        `(begin
           ;; Delete pregenerated man page translations, but not the originals.
           (with-directory-excursion "doc"
             (for-each (lambda (language)
                         (for-each delete-file
                                   (find-files language "\\.[0-9]$")))
                       (scandir "."
                                (lambda (file)
                                  (and (not (string-prefix? "." file))
                                       (eq? 'directory
                                            (stat:type (lstat file))))))))))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile.am
           (lambda _
             (substitute* "Makefile.am"
               (("/bin/sh") (which "sh")))))
         (add-after 'unpack 'patch-script
           (lambda*  (#:key inputs #:allow-other-keys)
             (substitute* "scripts/fakeroot.in"
               (("getopt")
                (string-append (assoc-ref inputs "util-linux")
                               "/bin/getopt"))
               (("sed")
                (string-append (assoc-ref inputs "sed")
                               "/bin/sed"))
               (("cat|cut" command)
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/" command)) )))
         (replace 'bootstrap
           (lambda _
             ;; The "preroll" script takes care of Autoconf and also
             ;; prepares the translated manuals.
             (invoke "sh" "./preroll")))
         (add-before 'configure 'setenv
           (lambda _
             (setenv "LIBS" "-lacl")))
         (add-before 'check 'prepare-check
           (lambda _
             (setenv "SHELL" (which "bash"))
             (setenv "VERBOSE" "1")
             (substitute* "test/t.touchinstall"
               ;; We don't have the name of the root user, so use ID=0.
               (("grep root") "grep \"\\<0\\>\""))
             (substitute* "test/tartest"
               ;; We don't have the name of the root group, so use ID=0.
               (("ROOTGROUP=root") "ROOTGROUP=0")
               ;; We don't have the name of the daemon user, so use IDs.
               (("daemon:sys") "1:3")
               (("daemon:") "1:"))
             ;; We don't have an /etc/passwd entry for "root" - use numeric IDs.
             (substitute* "test/compare-tar"
               (("tar -tvf") "tar --numeric-owner -tvf")))))))
    (native-inputs
     (list autoconf-2.71 automake gettext-minimal libtool po4a
           sharutils xz))               ; for tests
    (inputs
     (list acl libcap util-linux sed coreutils))
    (synopsis "Run commands in an environment with fake root privileges")
    (description
     "@command{fakeroot} runs a command in an environment where it appears to
have root privileges for file manipulation.  This is useful for allowing users
to create archives (@file{tar}, @file{ar}, @file{deb}, etc.)  with files in
them with root permissions and/or ownership.

Without fakeroot, one would have to have root privileges to create the
constituent files of the archives with the correct permissions and ownership,
and then pack them up, or one would have to construct the archives directly,
without using the archiver.")
    (home-page "http://freshmeat.sourceforge.net/projects/fakeroot")
    (license license:gpl3+)))

(define-public fakechroot
  ;; XXX: Build from the change submitted at
  ;; <https://github.com/dex4er/fakechroot/pull/85> to allow compilation
  ;; against glibc 2.33.  Switch back to the official repository on the next
  ;; release.
  (let ((commit "e7c1f3a446e594a4d0cce5f5d499c9439ce1d5c5")
        (revision "0"))
    (package
      (name "fakechroot")
      (version (git-version "2.20.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lipnitsk/fakechroot")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0gac6a6djx3nf343vd33sr5qqngz8ss3aij54zl8x9wb47pc11kb"))))
      (build-system gnu-build-system)
      (arguments
       ;; XXX: The tests heavily assume they run on an FHS system so for now
       ;; skip them.
       '(#:tests? #f
         #:configure-flags '("--disable-static")))
      (native-inputs (list autoconf automake libtool perl))
      (synopsis "Emulate @code{chroot} by overriding file system calls")
      (description
       "@command{fakechroot} runs a command in an environment were is additional
possibility to use @code{chroot} command without root privileges.  This is
useful for allowing users to create own chrooted environment with possibility
to install another packages without need for root privileges.

It works by providing @file{libfakechroot.so}, a shared library meant to be
set as @code{LD_PRELOAD} to override the C library file system functions.")
      (home-page "https://github.com/dex4er/fakechroot/")
      (license license:lgpl2.1+))))

(define-public falcosecurity-libs
  (package
    (name "falcosecurity-libs")
    (version "0.20.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/falcosecurity/libs/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "041ir9wk44v7isidwl7fzxrjvs85j637wcr7xirasd8ysxa0r4qv"))
              (patches
               (search-patches
                "falcosecurity-libs-shared-build.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DUSE_BUNDLED_DEPS=OFF"
              "-DBUILD_DRIVER=OFF"
              "-DENABLE_DKMS=OFF"
              "-DBUILD_LIBSCAP_MODERN_BPF=ON"
              "-DSCAP_FILES_SUITE_ENABLE=OFF" ;attempts to download scap files
              "-DBUILD_SHARED_LIBS=ON"
              #$(string-append "-DFALCOSECURITY_LIBS_VERSION=" version))
      ;; Only the libsinsp test suite is run, as the one for libscap requires
      ;; elevated privileges.
      #:test-target "run-unit-test-libsinsp"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              (substitute* "userspace/libsinsp/test/user.ut.cpp"
                ;; The 'system_lookup' test assumes a root user
                ;; exists in the build environment.
                (("TEST_F\\(usergroup_manager_test, system_lookup)")
                 "TEST_F(usergroup_manager_test, DISABLED_system_lookup)"))))
          (add-after 'install 'delete-src
            (lambda _
              (delete-file-recursively
               (string-append #$output "/src")))))))
    (native-inputs
     (list bpftool
           clang-18                    ;avoid stack limit exceeded build error
           googletest
           pkg-config
           valijson))                   ;header-only library
    (inputs
     (list elfutils))
    (propagated-inputs
     ;; The following inputs are in the 'Requires' field of libscap.pc,
     ;; libsinp.pc or libpman.pc.
     (list c-ares
           grpc
           jsoncpp
           libbpf
           libelf
           openssl
           protobuf
           uthash                       ;included in libscap headers
           zlib
           ;; These are in the 'Requires.private' field of libscap.pc and
           ;; libsinp.pc.  They are required because the headers are installed
           ;; to a non-standard directory, and thus need to be found via the
           ;; 'Cflags' field, which in turn mandates that both the pkg-config
           ;; modules listed in the 'Requires' and 'Requires.private' be
           ;; available.
           curl
           re2
           tbb))
    (home-page "https://github.com/falcosecurity/libs/")
    (synopsis "Falco security libraries")
    (description "The Falco security libraries include @code{libsinsp} and
@code{libscap}.  @code{libscap} manages the data capture process, while
@code{libsinsp} is a system inspection library that enriches events from
@code{libscap} with machine state.  @code{libsinsp} also performs events
filtering with rule evaluation through its internal rule engine.  These
libraries are used by the @command{sysdig} command-line utility.")
    (license license:asl2.0)))

(define-public inputattach
  (package
    (name "inputattach")
    (version "0.42.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linuxwacom/input-wacom")
                    (commit (string-append "input-wacom-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "04lnn7v0rm4ppbya140im5d4igcl6c1nrqpgbsr0i8wkral0nv7j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inputattach"
               (invoke "gcc" "-O2" "-o" "inputattach"
                       "inputattach.c"))))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target-dir (string-append
                                (assoc-ref outputs "out")
                                "/bin/")))
               (mkdir-p target-dir)
               (copy-file "inputattach/inputattach"
                          (string-append target-dir
                                         "inputattach"))
               #t))))))
    (home-page "https://linuxwacom.github.io/")
    (synopsis "Dispatch input peripherals events to a device file")
    (description "inputattach dispatches input events from several device
types and interfaces and translates so that the X server can use them.")
    (license license:gpl2+)))

(define-public keyd
  (package
    (name "keyd")
    (version "2.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rvaiya/keyd")
               (commit (string-append "v" version))))
        (sha256
          (base32
            "0nkra6lwdjhjcwj6486cgy562n4bcp98fjgl52rj8pp76i15yad7"))))
    (arguments
      (list #:tests? #f ; tests require root
            #:make-flags
            #~(list (string-append "CC=" #$(cc-for-target))
                    "PREFIX="
                    (string-append "DESTDIR=" #$output))
            #:phases
            '(modify-phases
               %standard-phases
               (delete 'configure)))) ; no autoconf
    (build-system gnu-build-system)
    (inputs (list linux-libre-headers))
    (synopsis "Key remapping daemon for Linux")
    (description
      "Keyd is a keyboard remapping utility with intuitive ini configuration
file format.  Keyd has several features, many of which are traditionally only
found in custom keyboard firmware like QMK.")
    (home-page "https://github.com/rvaiya/keyd")
    (license license:expat)))

(define-public pipewire
  (package
    (name "pipewire")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/pipewire/pipewire")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00c9irz566lmg51wxq5ywpvql6svvjxk9zjqx61xkvsnhz1526f6"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dudevrulesdir=" #$output "/lib/udev/rules.d")
              "-Dman=enabled"
              "-Drlimits-install=false"
              "-Dsession-managers=[]"
              "-Dsysconfdir=/etc"
              "-Dsystemd=disabled")))
    (native-inputs
     (list `(,glib "bin")
           pkg-config
           doxygen
           python
           python-docutils))
    (inputs (list alsa-lib
                  avahi
                  bluez
                  dbus
                  eudev
                  ffmpeg
                  gst-plugins-base
                  gstreamer
                  jack-2
                  ldacbt
                  libcamera
                  libdrm
                  libfdk
                  libfreeaptx
                  libsndfile
                  libusb
                  openssl ; raop sink
                  libva
                  pulseaudio
                  readline ; for pw-cli
                  sbc
                  vulkan-headers
                  vulkan-loader
                  webrtc-audio-processing))
    (home-page "https://pipewire.org/")
    (synopsis "Server and user space API to deal with multimedia pipelines")
    (description
     "PipeWire is a project that aims to greatly improve handling of audio and
video under Linux.  It aims to support the usecases currently handled by both
PulseAudio and Jack and at the same time provide same level of powerful handling
of Video input and output.  It also introduces a security model that makes
interacting with audio and video devices from containerized applications easy,
with supporting Flatpak applications being the primary goal.  Alongside Wayland
and Flatpak we expect PipeWire to provide a core building block for the future
of Linux application development.")
    (license license:lgpl2.0+)))

(define-public wireplumber
  (package
    (name "wireplumber")
    (version "0.5.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.freedesktop.org/pipewire/wireplumber.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g6gv7apwyc74z4rfhcdgdgwidda7cy4znwjjq39q4jh24dg70j4"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dsystemd=disabled"
                           "-Dsystem-lua=true")))
    (native-inputs
     (list `(,glib "bin")
           pkg-config))
    (inputs (list dbus elogind glib lua pipewire))
    (home-page "https://gitlab.freedesktop.org/pipewire/wireplumber")
    (synopsis "Session / policy manager implementation for PipeWire")
    (description "WirePlumber is a modular session / policy manager for
PipeWire and a GObject-based high-level library that wraps PipeWire's API,
providing convenience for writing the daemon's modules as well as external
tools for managing PipeWire.")
    (license license:expat)))

(define-public wireplumber-minimal
  (let ((base wireplumber))
    (package
      (inherit base)
      (name "wireplumber-minimal")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags ''())
          #~(cons "-Delogind=disabled" #$flags))))
      (inputs
       (modify-inputs (package-inputs base)
         (delete "elogind"))))))

(define-public ell
  (let ((commit "4f77dca1f1cd19041a5a882ba02ad5a39cde3661")
        (revision "1"))
    (package
      (name "ell")
      (version (git-version "0.77" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.kernel.org/pub/scm/libs/ell/ell.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ccxn9vdhn8miqhyxpj89hd9y8blz10q31qwvd6hbk2x214vgnk2"))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
                     (substitute* "Makefile.am"
                       ;; requires hwdb.bin
                       (("unit/test-hwdb.*") ""))))))
      (build-system gnu-build-system)
      (arguments
       ;; Tests launch dbus-daemon instances that all try to bind to
       ;; "/tmp/ell-test-bus".  Thus, we need to run them sequentially.
       '(#:parallel-tests? #f))
      (inputs
       (list dbus))
      (native-inputs
       (list autoconf automake libtool pkg-config procps))
      (home-page "https://01.org/ell")
      (synopsis "Embedded Linux Library")
      (description "The Embedded Linux* Library (ELL) provides core, low-level
functionality for system daemons.  It typically has no dependencies other than
the Linux kernel, C standard library, and libdl (for dynamic linking).  While
ELL is designed to be efficient and compact enough for use on embedded Linux
platforms, it is not limited to resource-constrained systems.")
      (license license:lgpl2.1+))))

(define-public kexec-tools
  (package
    (name "kexec-tools")
    (version "2.0.30")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/utils/kernel"
                                  "/kexec/kexec-tools-" version ".tar.xz"))
              (sha256
               (base32
                "0khjha6qjgbg7v470mwv333k1i9aqggvs1z93nfhba17mykmz2kl"))))
    (build-system gnu-build-system)
    (arguments
     ;; There are no automated tests.
     '(#:tests? #f))
    ;; This variant of binutils is used for the 64 bit support needed to
    ;; assemble the `purgatory/arch/i386/compat_x86_64.S' program on i686-linux.
    (native-inputs (list (make-ld-wrapper "ld-wrapper"
                                          #:binutils binutils)
                         binutils))
    (home-page "https://projects.horms.net/projects/kexec/")
    (synopsis "Tools for booting directly into different kernels")
    (description "This package provides the @code{kexec} program and ancillary
utilities.  Using @code{kexec}, it is possible to boot directly into a new
kernel from the context of an already-running kernel, bypassing the normal
system boot process.")
    (supported-systems (delete "riscv64-linux" %supported-systems))
    (license license:gpl2)))

(define-public cachefilesd
  (package
    (name "cachefilesd")
    (version "0.10.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://git.kernel.org/pub/scm/linux/kernel/git/dhowells"
                    "/cachefilesd.git/snapshot/cachefilesd-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0g40ljjnn3wzh9gp6il21c95f977298qrrkrxfnwfl3k3asfmnbi"))))
    (build-system gnu-build-system)
    (outputs '("out"))
    (arguments
     `(#:tests? #f ; there are no tests
       #:make-flags
       (let ((prefix-dir (lambda (var dir)
                           (string-append var "=" %output "/" dir))))
         (list (string-append "CC=" ,(cc-for-target))
               (prefix-dir "SBINDIR" "sbin/")
               (prefix-dir "ETCDIR" "etc/")
               (prefix-dir "MANDIR" "share/man/")))
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://people.redhat.com/~dhowells/cachefs/")
    (synopsis "CacheFiles userspace management daemon")
    (description "@code{cachefilesd} is a userspace daemon that manages the
cache data store that is used by network file systems such as @code{AFS} and
@code{NFS} to cache data locally on disk.  The content of the cache is
persistent over reboots.")
    (license license:gpl2+)))

(define-public libbpf
  (package
    (name "libbpf")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libbpf/libbpf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j7rm76g2ss09vksm4nm2g5yq7df2z5qpjr4h6kd3959sc1rvxnh"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;self-tests run in QEMU
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              (string-append "LIBDIR=$(PREFIX)/lib")
              (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'pre-build
            (lambda _
              (chdir "src")))
          (replace 'install
            (lambda* (#:key make-flags #:allow-other-keys #:rest args)
              (apply (assoc-ref %standard-phases 'install)
                     (append args
                             (list #:make-flags
                                   (append (list "install"
                                                 ;; Also install the kernel
                                                 ;; user API (uAPI) headers.
                                                 "install_uapi_headers")
                                           make-flags)))))))))
    (native-inputs (list linux-libre-headers-latest pkg-config))
    (propagated-inputs (list elfutils zlib)) ;in Requires.private of libbpf.pc
    (home-page "https://github.com/libbpf/libbpf")
    (synopsis "BPF CO-RE (Compile Once – Run Everywhere)")
    (description
     "Libbpf supports building BPF CO-RE-enabled applications, which, in
contrast to BCC, do not require the Clang/LLVM runtime or linux kernel
headers.")
    (license (list license:lgpl2.1 license:bsd-2))))

(define-public libbpf-0.8
  (package
    (inherit libbpf)
    (name "libbpf")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libbpf/libbpf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zzpkk4x3f20483dzw43b3ml03d63vvkmqf4j8y3b61b67wm59bm"))))
    (arguments
     (list
      #:tests? #f                       ;self-tests run in QEMU
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              (string-append "LIBDIR=$(PREFIX)/lib")
              (string-append "CC=" #$(cc-for-target)))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (add-before 'build 'pre-build
                     (lambda _
                       (chdir "src"))))))
    (native-inputs (list pkg-config))
    (propagated-inputs (list elfutils zlib)))) ;in Requires.private of libbpf.pc

(define-public bcc
  (package
    (name "bcc")
    (version "0.30.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iovisor/bcc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0b5la0yn6x6ll73drnrm5v5yibbrzkvl86hqivkrmnpgy8cqn0cy"))))
    (build-system cmake-build-system)
    (native-inputs
     (list bison
           flex
           (@ (gnu packages compression) zip)))
    (inputs
     (list bash-minimal                 ;for wrap-program
           clang-15
           elfutils
           luajit
           libbpf
           python-wrapper))
    (arguments
     (list
      ;; Tests all require root permissions and a "standard" file hierarchy.
      #:tests? #f
      #:configure-flags #~(list (string-append "-DREVISION=" #$version)
                                "-DCMAKE_USE_LIBBPF_PACKAGE=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'substitute-libbc
            (lambda _
              (substitute* "src/python/bcc/libbcc.py"
                (("(libbcc\\.so.*)\\b" _ libbcc)
                 (string-append #$output "/lib/" libbcc)))))
          (add-after 'install 'wrap-tools
            (lambda _
              (use-modules (ice-9 textual-ports))
              (let* ((out #$output)
                     (lib (string-append out "/lib"))
                     (tools (string-append out "/share/bcc/tools"))
                     (python-executable?
                      (lambda (filename _)
                        (call-with-input-file filename
                          (lambda (port)
                            (string-contains (get-line port)
                                             "/bin/python"))))))
                (for-each (lambda (python-executable)
                            (format #t "Wrapping: ~A.~%" python-executable)
                            (wrap-program python-executable
                              `("GUIX_PYTHONPATH" ":" prefix
                                (,(string-append lib
                                                 "/python"
                                                 #$(version-major+minor
                                                    (package-version python))
                                                 "/site-packages")))))
                          (find-files tools python-executable?))))))))
    (home-page "https://github.com/iovisor/bcc")
    (synopsis "Tools for BPF on Linux")
    (description
     "BCC is a toolkit for creating efficient kernel tracing and manipulation
programs, and includes several useful tools and examples.  It makes use of
extended BPF (Berkeley Packet Filters), formally known as eBPF, a new feature
that was first added to Linux 3.15.  Much of what BCC uses requires Linux 4.1
and above.")
    (license license:asl2.0)))

(define-public bpftool
  (package
    (name "bpftool")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;This package has no tests.
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key inputs #:allow-other-keys)
                      (invoke "make" "-C" "tools/bpf/bpftool"
                              ,(string-append "CC=" (cc-for-target)))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/sbin"))
                        (mkdir-p (string-append out
                                  "/share/bash-completion/completions"))
                        (invoke "make"
                                (string-append "prefix=" out)
                                (string-append
                                 "bash_compdir=" out
                                 "/share/bash-completion/completions")
                                "-C" "tools/bpf/bpftool"
                                "install")))))))
    (inputs (list elfutils                        ;provides libelf
                  readline libcap zlib))
    (native-inputs (list bison python-3))
    ;; This tool does not have a proper web page.
    (home-page
     "https://git.kernel.org/pub/scm/linux/kernel/git/bpf/bpf-next.git/tree/tools/bpf/bpftool")
    (synopsis "Tool for inspection and manipulation of eBPF programs and maps")
    (description "@command{bpftool} allows for inspection and simple
modification of BPF objects on the system.")
    (license (package-license linux-libre))))

(define-public bpftrace
  (package
    (name "bpftrace")
    (version "0.21.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bpftrace/bpftrace")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0icbhf1wk523a7lcmwqa67zc6hl6h02p5mfg26cizva447kbwsgz"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DBUILD_TESTING=ON")
      ;; Only run the unit tests suite, as the other ones
      ;; (runtime_tests, tools-parsing-test) require to run as
      ;; 'root'.
      #:test-target "bpftrace_test"
      #:phases
      #~(modify-phases %standard-phases
          ;; This patch also fixes broken compilation due to improper detection
          ;; of bfd features. This is taken from following upstream PR:
          ;; https://github.com/bpftrace/bpftrace/pull/3332
          (add-after 'unpack 'patch-paths-and-bfd-defs
            (lambda _
              (substitute* '("cmake/FindLibBfd.cmake"
                             "src/CMakeLists.txt")
                (("LIBSFRAME_FOUND")
                 "LIBSFRAME_LIBRARIES")
                (("LIBZSTD_FOUND")
                 "LIBZSTD_LIBRARIES"))
              (with-directory-excursion "tests"
                (substitute* (find-files ".")
                  (("/bin/sh")
                   (which "sh")))
                (substitute* '("child.cpp"
                               "runtime/call"
                               "procmon.cpp")
                  (("/bin/ls")
                   (which "ls")))))))))
    (native-inputs (list bison dwarves flex googletest xxd))
    (inputs (list bcc clang-15 elfutils libbpf libiberty cereal))
    (home-page "https://github.com/bpftrace/bpftrace")
    (synopsis "High-level tracing language for Linux eBPF")
    (description
     "bpftrace is a high-level tracing language for Linux enhanced Berkeley
Packet Filter (eBPF) available in recent Linux kernels (4.x).  bpftrace uses
LLVM as a backend to compile scripts to BPF-bytecode and makes use of BCC for
interacting with the Linux BPF system, as well as existing Linux tracing
capabilities: kernel dynamic tracing (kprobes), user-level dynamic
tracing (uprobes), and tracepoints.  The bpftrace language is inspired by awk
and C, and predecessor tracers such as DTrace and SystemTap.  bpftrace was
created by Alastair Robertson.")
    (license license:asl2.0)))

(define-public ttyebus-linux-module
  (let ((revision "0")
        (commit "fe4332a2281cf79804ef4d8516aa848ca1c58d1f"))
    (package
      (name "ttyebus-linux-module")
      (version (git-version "1.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eBUS/ttyebus")
               (commit "fe4332a2281cf79804ef4d8516aa848ca1c58d1f")))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1i66xjs9vln5cca6wx7aiiw7jihrlsk9hjdqyczp36fvm1b1bani"))))
      (supported-systems '("armhf-linux" "aarch64-linux"))
      (build-system linux-module-build-system)
      (arguments
       `(#:tests? #f))
      (home-page "https://github.com/eBUS/ttyebus")
      (synopsis "Low-latency Raspberry Pi UART driver")
      (description "This package provides a Linux kernel module that will
provide a serial device @code{/dev/ttyebus} with almost no latency upon
receiving.  It is dedicated to the PL011 UART of the Raspberry Pi.")
      (license license:gpl3+))))

(define-public ipset
  (package
    (name "ipset")
    (version "7.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ipset.netfilter.org/"
                           "ipset-" version ".tar.bz2"))
       (sha256
        (base32 "1n34mkrdha9rggd0fizjx6baqkiqqd1yfhb5ml663mlx93zwjjdy"))))
    (build-system gnu-build-system)
    (inputs
     (list libmnl))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--with-kmod=no")))
    (home-page "https://ipset.netfilter.org/")
    (synopsis "Administration tool for IP sets")
    (description "IP sets are a framework inside the Linux 2.4.x and 2.6.x kernel which
can be administered by the ipset utility.  Depending on the type,
currently an IP set may store IP addresses, (TCP/UDP) port numbers or
IP addresses with MAC addresses in a way which ensures lightning speed
when matching an entry against a set.

If you want to
@itemize @bullet
@item store multiple IP addresses or port numbers and match against the entire
collection using a single iptables rule.
@item dynamically update iptables rules against IP addresses or ports without
performance penalty.
@item express complex IP address and ports based rulesets with a single
iptables rule and benefit from the speed of IP sets.
@end itemize\n
then IP sets may be the proper tool for you.")
    (license license:gpl2+)))

(define-public liburing
  (package
    (name "liburing")
    (version "2.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.kernel.dk/liburing")
                    (commit (string-append "liburing-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1skzzdb769jm8p098k292maqycfchpz16mqm6ml8sfzkq2hfck6p"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Tests are dependent on kernel version and features
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; configure fails if it is followed by SHELL, CONFIG_SHELL,
           ;; --enable-fast-install, and --build
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))
               (invoke "./configure" (string-append "--prefix=" out))))))))
    (home-page "https://github.com/axboe/liburing")
    (synopsis "Interface to the Linux kernel @code{io_uring} interface")
    (description "This is the io_uring library, liburing. liburing provides
helpers to setup and teardown io_uring instances, and also a simplified
interface for applications that don't need (or want) to deal with the full
kernel side implementation.")
    (license license:expat)))

(define-public erofs-utils
  (package
    (name "erofs-utils")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/linux/kernel/git/xiang/erofs-utils.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a57a8r58wp90a9r2fmkfxsq2agq78rm2qif3js0rsraz4hhrfn2"))))
    (build-system gnu-build-system)
    (inputs
     (list lz4
           `(,util-linux "lib")
           zlib))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://git.kernel.org/pub/scm/linux/kernel/git/xiang/erofs-utils.git/")
    (synopsis "User-space tools for the EROFS file system")
    (description
     "@acronym{EROFS, The Enhanced Read-Only File System} is a compressed,
read-only file system optimized for resource-scarce devices.  This package
provides user-space tools for creating EROFS file systems.")
    (license license:gpl2+)))

(define-public rasdaemon
  (package
    (name "rasdaemon")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mchehab/rasdaemon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m3j1hz9rqcvwmrimpakd239s0ppzaplkykhf9wyh55xmmry8z85"))))
    (native-inputs (list autoconf automake libtool pkg-config))
    (inputs (list libtraceevent perl perl-dbd-sqlite sqlite dmidecode kmod))
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-all"
              ;; Don't install unused /etc/sysconfig/rasdaemon environment file.
              "--with-sysconfdefdir=."
              "--localstatedir=/var")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'rename-README
            (lambda _
              ;; Required by autoreconf
              (rename-file "README.md" "README")))
          (add-before 'configure 'munge-autotools
            (lambda _
              ;; For some reason upstream forces sysconfdir=/etc.  This results
              ;; in EPERM during the install phase.  Removing the offending
              ;; line lets sysconfdir correctly pick up DESTDIR.
              (substitute* "configure.ac"
                (("^test .* sysconfdir=/etc\n$") ""))))
          (add-after 'install 'wrap-rasdaemon
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((path (map dirname
                               (list (search-input-file inputs "/sbin/dmidecode")
                                     (search-input-file inputs "/bin/modprobe")))))
                (wrap-program (string-append #$output "/sbin/ras-mc-ctl")
                  `("PATH" ":" prefix ,path)
                  `("PERL5LIB" ":" prefix ,(string-split (getenv "PERL5LIB") #\:)))))))))
    (build-system gnu-build-system)
    (home-page "https://github.com/mchehab/rasdaemon")
    (synopsis "Platform Reliability, Availability, and Serviceability tools")
    (description
     "The @code{rasdaemon} daemon monitors platform @acronym{RAS, Reliability
Availability and Serviceability} reports from Linux kernel trace events.
These trace events are logged in @file{/sys/kernel/debug/tracing} and reported
through standard log mechanisms like syslog.")
    (license license:gpl2)))

(define-public renameat2
  ;; This is a Gist, with no release or tags.
  (let ((revision "0")
        (commit "5c5193f20142511a5fc7069a539f4e5aba0ea470"))
    (package
      (name "renameat2")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://gist.githubusercontent.com/"
                                    "eatnumber1/f97ac7dad7b1f5a9721f/raw/"
                                    commit "/renameat2.c"))
                (sha256
                 (base32
                  "07b4hsxqjm610sdkm4nxhp0gnl2s7gzlh4zdnja5ay40v4x24bb9"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:phases #~(modify-phases %standard-phases
                     (delete 'configure)
                     (replace 'build
                       (lambda _
                         (invoke #$(cc-for-target) "renameat2.c"
                                 "-o" "renameat2")))
                     (replace 'install
                       (lambda _
                         (install-file "renameat2"
                                       (string-append #$output "/bin")))))))
      (home-page "https://gist.github.com/eatnumber1/f97ac7dad7b1f5a9721f")
      (synopsis "Command to call the renameat2 Linux system call")
      (description "This package provides a @command{renameat2} command that
calls the Linux-specific @code{renameat2} system call.")
      (license license:expat))))

(define-public libgpiod
  (package
    (name "libgpiod")
    (version "1.6.3")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://git.kernel.org/pub/scm/libs/libgpiod/libgpiod.git")
            (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "0rv8a11hx3pc6sdw6nfc6k35hkp2clb3v53n1381cvip8fzhbsad"))))
    (build-system gnu-build-system)
    (arguments
      `(#:configure-flags
        '("--enable-tools=yes"
          "--enable-bindings-cxx"
          "--enable-bindings-python")))
    (native-inputs
      (list automake
            autoconf
            libtool
            autoconf-archive
            pkg-config
            python-3))
    (synopsis "Interact with the Linux GPIO character device")
    (description
     "This package provides a C library with C++/Python bindings and
command-line tools for interacting with GPIO devices that avoids the usage of
older system-wide @file{/sys} interface.")
    (home-page "https://git.kernel.org/pub/scm/libs/libgpiod/libgpiod.git/")
    (license (list license:lgpl2.1+   ;; libgpiod
                   license:gpl2+      ;; gpio-tools
                   license:lgpl3+)))) ;; C++ bindings

(define-public libtraceevent
  (package
    (name "libtraceevent")
    (version "1.7.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/libs/libtrace/libtraceevent.git")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06mw2f0xnk6dy9w2z0n4dz7lnm02qfsmnmj2h24453qxlw57x0d6"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (substitute* (list "Makefile" "scripts/utils.mk")
              (("/bin/(pwd)" _ command) command))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no test suite
      #:make-flags
      #~(list
         (string-append "pkgconfig_dir=" #$output "/lib/pkgconfig")
         (string-append "prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))        ; no configure script
    (home-page "https://git.kernel.org/pub/scm/libs/libtrace/libtraceevent.git/")
    (synopsis "Linux kernel trace event library")
    (description
     "This library parses raw Linux kernel trace event formats.")
    (license (list license:gpl2
                   license:lgpl2.1))))

(define-public libtracefs
  (package
    (name "libtracefs")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/libs/libtrace/libtracefs.git")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v896n3q0df0nxx5drbwyaqhrqiyxl06rvrdw3gp2r37awa9g1zb"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (substitute* (list "Makefile" "scripts/utils.mk")
              (("/bin/(pwd)" _ command) command))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no test suite
      #:make-flags
      #~(list
         (string-append "CC=" #$(cc-for-target))
         (string-append "pkgconfig_dir=" #$output "/lib/pkgconfig")
         (string-append "prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))        ; no configure script
    (native-inputs (list pkg-config))
    (inputs (list libtraceevent))
    (home-page "https://git.kernel.org/pub/scm/libs/libtrace/libtracefs.git/")
    (synopsis "Linux kernel trace file system library")
    (description
     "This library provides APIs to access the Linux kernel's trace file
system.")
    (license (list license:gpl2
                   license:lgpl2.1))))

(define-public libtree
  (package
    (name "libtree")
    (version "3.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/haampie/libtree")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "libtree-fix-check-non-x86.patch"))
       (sha256
        (base32 "1jbc60cslzcfxk9jwva51ncr1zl1nz0fkgfjdckgv81is11nswmb"))))
    (arguments
     (list #:make-flags
           ;; NOTE: Official documentation recommends to build libtree with
           ;; "-static" flag.
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-tests
                 ;; XXX: '08_nodeflib' fails as 'libtree' returns a non-zero
                 ;; code in one of the tests.
                 ;;
                 ;; See <https://github.com/haampie/libtree/issues/80>).
                 (lambda _
                   (substitute* "tests/08_nodeflib/Makefile"
                     (("! ../../libtree -vvv exe_b")
                      "../../libtree -vvv exe_b"))))
               (delete 'configure))))
    (build-system gnu-build-system)
    (home-page "https://github.com/haampie/libtree")
    (synopsis "Show output of @command{ldd} as a tree")
    (description
     "This tool turns @command{ldd} into a tree and explains how shared
libraries are found or why they cannot be located.")
    (license license:expat)))

(define-public touchegg
  (package
    (name "touchegg")
    (version "2.0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JoseExposito/touchegg")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0shvslz0c8nqx5f988z55qjc9xw0in9rb7b19r6vr1f7cdkqb6yr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; No tests exist
       #:configure-flags
       (list "-DUSE_SYSTEMD=OFF"))) ; No systemd
    (native-inputs
     (list
      pkg-config))
    (inputs
     (list
      cairo
      gtk+
      libgudev
      libinput
      libxrandr
      libxtst
      pugixml))
    (home-page "https://github.com/JoseExposito/touchegg")
    (synopsis "Multitouch gesture recognizer")
    (description
     "Touchégg is an application that runs in the background and transform the
gestures you make on your touchpad or touchscreen into visible actions in your
desktop.")
    (license license:gpl3+)))

(define-public evtest
  (package
    (name "evtest")
    (version "1.35")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/libevdev/evtest")
                    (commit (string-append "evtest-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "164prnw35kda6jfykl7h52lfzy99ma2lk029zscyqk766k19spf4"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;No tests exist
           #:make-flags #~(list (string-append "CC="
                                               #$(cc-for-target))
                                (string-append "PREFIX="
                                               #$output))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'generate-doc
                          (lambda _
                            (invoke "asciidoc" "-d" "manpage"
                                    "-b" "docbook"
                                    "-o" "evtest.1.xml"
                                    "evtest.txt")
                            (invoke "xsltproc" "--nonet"
                                    (string-append
                                     #$(this-package-native-input "docbook-xsl")
                                     "/xml/xsl/docbook-xsl-"
                                     #$(package-version
                                        (this-package-native-input "docbook-xsl"))
                                     "/manpages/docbook.xsl")
                                    "evtest.1.xml")))
                        (replace 'bootstrap
                          (lambda _
                            (setenv "CONFIG_SHELL" (which "sh"))
                            (invoke "autoreconf" "-fi"))))))
    (native-inputs (list autoconf
                         automake
                         bash-minimal
                         xmlto
                         docbook-xsl
                         libxslt
                         asciidoc))
    (home-page "https://gitlab.freedesktop.org/libevdev/evtest")
    (synopsis "Kernel evdev device information and monitor")
    (description
     "@code{evtest} is a tool to print @code{evdev} kernel events.  It reads
directly from the kernel device and prints a device description and the events
with the value and the symbolic name.")
    (license license:gpl2+)))

(define-public tp-smapi-module
  (package
    (name "tp-smapi-module")
    (version "0.44")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linux-thinkpad/tp_smapi")
                    (commit (string-append "tp-smapi/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kzbks07bh5jbi1n311zp9cbp1xxyzi7nys8wq1k0k5ig81h9w6k"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:tests? #f))                    ;there are none.
    (home-page "https://github.com/linux-thinkpad/tp_smapi")
    (synopsis
     "Linux Kernel module exposing features of ThinkPad hardware")
    (description
     "This package provides a Linux Kernel module that controls
battery charging of specific ThinkPad laptops.  It also includes an improved
version of the HDAPS driver.  The underlying hardware interfaces are
@acronym{SMAPI, System Management Application Program Interface} and direct
access to the embedded controller.")
    (license license:gpl2+)))

(define-public modprobed-db
  (package
    (name "modprobed-db")
    (version "2.48")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/graysky2/modprobed-db")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jscinga32jjvy3vwl4s1pyd5fjgqhmmk010k665nhil302hzjdc"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no tests
           #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   "INITDIR_SYSTEMD=no-thanks")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'install 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (string-append #$output "/bin/modprobed-db")
                     (("/usr") #$output)
                     (((string-append "(" (string-join (list "awk"
                                                             "cp"
                                                             "cut"
                                                             "getent"
                                                             "grep"
                                                             "logname"
                                                             "md5sum"
                                                             "mkdir"
                                                             "mv"
                                                             "sed"
                                                             "sort"
                                                             "uniq"
                                                             "wc")
                                                       "|") ")") m)
                      (search-input-file inputs (string-append "/bin/" m)))
                     (("modprobe ")
                      (string-append
                       (search-input-file inputs "/bin/modprobe") " "))))))))
    (inputs (list coreutils kmod gawk glibc grep sed))
    (home-page "https://wiki.archlinux.org/title/Modprobed-db")
    (synopsis "Keep track of Linux modules that have been probed")
    (description
     "Modprobed-db is a useful utility for users wishing to build a minimal
kernel via a @code{make localmodconfig}.  In a nutshell, this @command{make}
target creates a config based on the current config and a list of modules you
define (that @command{modprobed-db} keeps for you).  It then disables any
module option that is not needed thus not building extraneous modules.  This
results in a system-specific, streamlined kernel package and footprint as well
as reduced compilation times.

Modprobed-db simply logs every module ever probed on the target system to a
text-based database (@file{$XDG_CONFIG_HOME/modprobed-db}), which can be read
directly by @code{make localmodconfig} as described above.")
    (license license:expat)))

(define-public kernel-hardening-checker
  (package
    (name "kernel-hardening-checker")
    (version "0.6.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/a13xp0p0v/kernel-hardening-checker")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01p5wcyj4w5c4264abymhvpai7bvfy8ivspgqnrmg33qplwxl443"))))
    (build-system python-build-system)
    (home-page "https://github.com/a13xp0p0v/kernel-hardening-checker")
    (synopsis
     "Tool for checking the security hardening options of the Linux kernel")
    (description
     "@code{kernel-hardening-checker} is a tool for checking the security
hardening options of the Linux kernel.  Provided preferences are based on
suggestions from various sources, including:

@itemize
@item KSPP recommended settings
@item CLIP OS kernel configuration
@item Last public grsecurity patch (options which they disable)
@item SECURITY_LOCKDOWN_LSM patchset
@item Direct feedback from the Linux kernel maintainers
@end itemize\n
This tool supports checking Kconfig options and kernel cmdline parameters.")
    (license license:gpl3)))

(define-public kconfig-hardened-check
  (deprecated-package "kconfig-hardened-check" kernel-hardening-checker))

(define-public firejail
  (package
    (name "firejail")
    (version "0.9.74")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/netblue30/firejail/releases/download/" version
                    "/firejail-" version ".tar.xz" ))
              (sha256
               (base32
                "04w11jjh62ghs66ippw5xlg3l7l9mi3b5cpch1cg3fw7gxnbbn3h"))))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (with-directory-excursion "test"
                       (invoke "make"))))))))
    (build-system gnu-build-system)
    (inputs
     (list apparmor xdg-dbus-proxy))
    (synopsis "Linux namespaces sandbox program")
    (description
     "Firejail is a SUID sandbox program that reduces the risk of security
breaches by restricting the running environment of untrusted applications
using Linux namespaces, seccomp-bpf and Linux capabilities.  The software
includes sandbox profiles for a number of common Linux programs.  Firejail
should be added to the list of setuid programs in the system configuration to
work properly.")
    (home-page "https://github.com/netblue30/firejail")
    (supported-systems
     (filter (cut string-suffix? "-linux" <>) %supported-systems))
    (license license:gpl2+)))

(define-public edac-utils
  (package
    (name "edac-utils")
    (version "0.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/grondo/edac-utils.git")
                     (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "04j686m58wsnyy5di7rz0sw9jahlm4ziwxjmgs31pjb87vzw3xgp"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl))
    (inputs
     (list sysfsutils))
    (synopsis "Memory error detection and correction userspace helpers")
    (description "This package provides userspace helpers for memory
error detection and correction (EDAC).")
    (home-page "https://github.com/grondo/edac-utils")
    (license license:gpl2+)))

(define-public spectre-meltdown-checker
  (package
    (name "spectre-meltdown-checker")
    (version "0.46")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/speed47/spectre-meltdown-checker")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches
               (search-patches
                "spectre-meltdown-checker-externalize-fwdb.patch"))
              ;; Remove builtin firmware database.
              (modules '((guix build utils)))
              (snippet '(substitute* "spectre-meltdown-checker.sh"
                          (("^# [AI],.*") "")))
              (sha256
               (base32
                "0j42p6dayb7k87kf8sqimxlaswis3qh0569a15zccyknv9vf129k"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("spectre-meltdown-checker.sh" "bin/spectre-meltdown-checker"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fixpath
            (lambda* (#:key inputs #:allow-other-keys)
              (define* (find-command inputs cmd #:optional (bin "bin"))
                (search-input-file inputs (string-append bin "/" cmd)))
              (substitute* "spectre-meltdown-checker.sh"
                ;; ${opt_arch_prefix}CMD
                (("\\$\\{opt_arch_prefix\\}\\<(nm|objdump|readelf|strings)\\>"
                  all cmd)
                 (find-command inputs cmd))

                ;; Commands safe to substitute directly.
                (("\\<(awk|(base|dir)name|bunzip2|g(un)?zip|lz4)\\>" all cmd)
                 (find-command inputs cmd))
                (("\\<(lzop|mktemp|modprobe|pgrep|rmmod|umount)\\>" all cmd)
                 (find-command inputs cmd))
                (("\\<(unlzma|unxz|unzstd|uuencode)\\>" all cmd)
                 (find-command inputs cmd))

                ;; Commands which should only be substituted based on their
                ;; surroundings: First up, dd.
                (("\\<(dd)\\> if=" all cmd)
                 (string-append
                  (find-command inputs cmd)
                  " if="))

                ;; Another special case: sysctl is in sbin.
                (("(if |\\$\\( *)\\<(sysctl)\\>" all pre cmd)
                 (string-append pre (find-command inputs cmd "sbin")))

                ;; Meow
                (("cat (<<EOF|\"\\$)" all what)
                 (string-append (find-command inputs "cat") " " what))
                (("'cat'")
                 (string-append "'" (find-command inputs "cat") "'"))
                (("\"cat\"")
                 (string-append "\"" (find-command inputs "cat") "\""))

                ;; ${COMMAND} -
                ;; ${COMMAND} ^
                (("\\<(base64|cut|grep|head|id|mount)\\> ([-^])" all cmd suffix)
                 (string-append (find-command inputs cmd) " " suffix))
                (("\\<(od|perl|rm|uname|xargs)\\> ([-^])" all cmd suffix)
                 (string-append (find-command inputs cmd) " " suffix))

                ;; ${COMMAND} |
                (("\\<(dmesg)\\> \\|" all cmd)
                 (string-append (find-command inputs cmd) " |"))
                ;; | ${COMMAND}
                (("\\| \\<(grep|sed|sort|stat|tr)\\>" all cmd)
                 (string-append "| " (find-command inputs cmd)))

                ;; Command in sub-shell, i.e. $($COMMAND ...)
                (("\\$\\( *(\\<cat|find|grep|mount|nproc|stat|tr\\>)"
                  all cmd)
                 (string-append "$(" (find-command inputs cmd)))

                ;; command -v
                (("command -v \"*\\<(base64|nproc|perl|printf)\\>\"*" all cmd)
                 (string-append "command -v " (find-command inputs cmd)))))))))
    (inputs (list kmod lz4 lzop perl procps sharutils util-linux zstd))
    (home-page "https://github.com/speed47/spectre-meltdown-checker")
    (synopsis "CPU vulnerability / mitigation checker")
    (description
     "This package provides a shell script to assess your system's resilience
against the several transient execution CVEs that were published since early
2018, and gives guidance as to how to mitigate them.")
    (license license:gpl3)))

(define-public csmith
  (package
    (name "csmith")
    (version "2.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/csmith-project/csmith")
                    (commit (string-append "csmith-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nhrsnv6cny14xz68qb1h30fbwc05adkisk51p3x63mydm60ddl3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool m4 perl))
    (arguments
     (list
      #:tests? #f                       ;no test suite
      ;; Do not install headers under 'include/csmith-VERSION' but in
      ;; 'include/csmith'.
      #:phases
      `(modify-phases %standard-phases
         (add-after 'unpack 'patch-includedir
           (lambda _
             (substitute* "runtime/Makefile.am"
               (("\\$\\(includedir\\)/\\$\\(PACKAGE\\)-\\$\\(VERSION\\)")
                "$(includedir)/$(PACKAGE)"))))
         (add-before 'bootstrap 'force-bootstrap
           (lambda _
             (delete-file "configure"))))))
    (home-page "https://github.com/csmith-project/csmith")
    (synopsis "Random generator of C programs")
    (description "The primary purpose of Csmith is to find compiler bugs with
random programs using differential testing.")
    (license license:bsd-4)))

(define-public ipvsadm
  (package
    (name "ipvsadm")
    (version "1.31")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://kernel.org/pub/linux/utils/kernel"
                           "/ipvsadm/ipvsadm-" version ".tar.xz"))
       (sha256
        (base32 "1nyzpv1hx75k9lh0vfxfhc0p2fpqaqb38xpvs8sn88m1nljmw2hs"))))
    (build-system gnu-build-system)
    (native-inputs (list linux-libre-headers pkg-config which))
    (inputs (list libnl popt))
    (arguments
     (list #:tests? #f
           #:parallel-build? #f               ;build fails randomly with '-j'
           #:make-flags
           #~(list (string-join
                    (list "CFLAGS="
                          (string-append "-I"
                                         #$(this-package-input "libnl")
                                         "/include/libnl3")
                          (string-append "-L" #$(this-package-input "libnl")
                                         "/lib")
                          "-fPIC")
                    " ")
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "SBIN=" #$output "/sbin")
                   (string-append "INIT=" #$output "/etc/init.d")
                   (string-append "MANDIR=" #$output "/share/man"))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))))
    (home-page "http://www.linuxvirtualserver.org/software/ipvs.html")
    (synopsis "IP virtual server administration utility")
    (description "@code{ipvsadm(8)} is used to set up, maintain or inspect the
virtual server table in the Linux kernel.  The Linux Virtual Server can be used
to build scalable network services based on a cluster of two or more nodes.")
    (license license:gpl2+)))

(define-public ryzen-smu
  (package
    (name "ryzen-smu")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.com/leogx9r/ryzen_smu.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "010la2a8zp7rljlg5ssc9ragzva4ca05gvzlicjagl508659d2wz"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (synopsis "System Management Unit driver for AMD Ryzen processors")
    (description "This package provides a way to access the
System Management Unit for certain AMD Ryzen processors.
This includes access to the System Management Network.")
    (home-page "https://gitlab.com/leogx9r/ryzen_smu")
    (license license:gpl2)))

(define-public hid-wiimote-plus
  (package
    (name "hid-wiimote-plus")
    (version "0.9.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dkosmari/hid-wiimote-plus.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rkp311x60jm73xlqypxsp375rx3fa55jyrspz1aqjga0q6cvswi"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:tests? #f)) ; No tests.
    (synopsis "WiiMote HID kernel module")
    (description "This package provides a WiiMote driver that follows the
Linux input convention.  Specifically, that allows you to use the D-pad
as a gamepad.")
    (home-page "https://github.com/dkosmari/hid-wiimote-plus")
    (license license:gpl2+)))

(define-public python-evemu
  (package
    (name "python-evemu")
    (version "2.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/libevdev/evemu")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sxf6v2wnajj9srlkfjfymjmmp4dlq73gzjcjmaw015a3c0xl029"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; would open device files
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "python/evemu/base.py"
                     (("\"libevdev.so\"")
                      (string-append "\"" (assoc-ref inputs "libevdev")
                                     "/lib/libevdev.so\""))))))))
    (native-inputs
     (list asciidoc autoconf automake libtool docbook-xsl pkg-config
           python-wrapper xmlto))
    (inputs
     (list libevdev))
    (synopsis "Record and replay input device events")
    (description "This package provides a way to record and replay device
descriptions and events, making it possible to emulate input devices
through the Linux kernel's input system.  Emulated devices are for most
practical purposes indistinguishable from real devices.

It provides a command line program and also a Python library.")
    (home-page "https://www.freedesktop.org/wiki/Evemu/")
    (license license:gpl3)))

(define-public dualsensectl
  (package
    (name "dualsensectl")
    (version "0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/nowrep/dualsensectl.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05snsp5r3hc2yc1nf0w4aam1my4h0lkxnmy7k4glxvasd9jwahzw"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list eudev dbus hidapi))
    (synopsis "Linux tool for controlling PS5 DualSense controller")
    (description "This package provides a Linux tool for controlling a PS5
DualSense controller.  It has to be already connected via USB or connected
via Bluetooth.")
    (home-page "https://github.com/nowrep/dualsensectl")
    (license license:gpl2)))
